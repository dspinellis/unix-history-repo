/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Lawrence Berkeley Laboratory.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)pmap.c	8.4 (Berkeley) 2/5/94
 *
 * from: $Header: pmap.c,v 1.43 93/10/31 05:34:56 torek Exp $
 */

/*
 * SPARC physical map management code.
 * Does not function on multiprocessors (yet).
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/device.h>
#include <sys/proc.h>
#include <sys/malloc.h>

#include <vm/vm.h>
#include <vm/vm_kern.h>
#include <vm/vm_prot.h>
#include <vm/vm_page.h>

#include <machine/autoconf.h>
#include <machine/bsd_openprom.h>
#include <machine/cpu.h>
#include <machine/ctlreg.h>

#include <sparc/sparc/asm.h>
#include <sparc/sparc/cache.h>
#include <sparc/sparc/vaddrs.h>

#ifdef DEBUG
#define PTE_BITS "\20\40V\37W\36S\35NC\33IO\32U\31M"
#endif

extern struct promvec *promvec;

/*
 * The SPARCstation offers us the following challenges:
 *
 *   1. A virtual address cache.  This is, strictly speaking, not
 *	part of the architecture, but the code below assumes one.
 *	This is a write-through cache on the 4c and a write-back cache
 *	on others.
 *
 *   2. An MMU that acts like a cache.  There is not enough space
 *	in the MMU to map everything all the time.  Instead, we need
 *	to load MMU with the `working set' of translations for each
 *	process.
 *
 *   3.	Segmented virtual and physical spaces.  The upper 12 bits of
 *	a virtual address (the virtual segment) index a segment table,
 *	giving a physical segment.  The physical segment selects a
 *	`Page Map Entry Group' (PMEG) and the virtual page number---the
 *	next 5 or 6 bits of the virtual address---select the particular
 *	`Page Map Entry' for the page.  We call the latter a PTE and
 *	call each Page Map Entry Group a pmeg (for want of a better name).
 *
 *	Since there are no valid bits in the segment table, the only way
 *	to have an invalid segment is to make one full pmeg of invalid PTEs.
 *	We use the last one (since the ROM does as well).
 *
 *   4. Discontiguous physical pages.  The Mach VM expects physical pages
 *	to be in one sequential lump.
 *
 *   5. The MMU is always on: it is not possible to disable it.  This is
 *	mainly a startup hassle.
 */

struct pmap_stats {
	int	ps_unlink_pvfirst;	/* # of pv_unlinks on head */
	int	ps_unlink_pvsearch;	/* # of pv_unlink searches */
	int	ps_changeprots;		/* # of calls to changeprot */
	int	ps_useless_changeprots;	/* # of changeprots for wiring */
	int	ps_enter_firstpv;	/* pv heads entered */
	int	ps_enter_secondpv;	/* pv nonheads entered */
	int	ps_useless_changewire;	/* useless wiring changes */
	int	ps_npg_prot_all;	/* # of active pages protected */
	int	ps_npg_prot_actual;	/* # pages actually affected */
} pmap_stats;

#ifdef DEBUG
#define	PDB_CREATE	0x0001
#define	PDB_DESTROY	0x0002
#define	PDB_REMOVE	0x0004
#define	PDB_CHANGEPROT	0x0008
#define	PDB_ENTER	0x0010

#define	PDB_MMU_ALLOC	0x0100
#define	PDB_MMU_STEAL	0x0200
#define	PDB_CTX_ALLOC	0x0400
#define	PDB_CTX_STEAL	0x0800
int	pmapdebug = 0x0;
#endif

#define	splpmap() splimp()

/*
 * First and last managed physical addresses.
 */
#if 0
vm_offset_t	vm_first_phys, vm_last_phys;
#define	managed(pa)	((pa) >= vm_first_phys && (pa) < vm_last_phys)
#else
vm_offset_t	vm_first_phys, vm_num_phys;
#define	managed(pa)	((unsigned)((pa) - vm_first_phys) < vm_num_phys)
#endif

/*
 * For each managed physical page, there is a list of all currently
 * valid virtual mappings of that page.  Since there is usually one
 * (or zero) mapping per page, the table begins with an initial entry,
 * rather than a pointer; this head entry is empty iff its pv_pmap
 * field is NULL.
 *
 * Note that these are per machine independent page (so there may be
 * only one for every two hardware pages, e.g.).  Since the virtual
 * address is aligned on a page boundary, the low order bits are free
 * for storing flags.  Only the head of each list has flags.
 *
 * THIS SHOULD BE PART OF THE CORE MAP
 */
struct pvlist {
	struct	pvlist *pv_next;	/* next pvlist, if any */
	struct	pmap *pv_pmap;		/* pmap of this va */
	int	pv_va;			/* virtual address */
	int	pv_flags;		/* flags (below) */
};

/*
 * Flags in pv_flags.  Note that PV_MOD must be 1 and PV_REF must be 2
 * since they must line up with the bits in the hardware PTEs (see pte.h).
 */
#define PV_MOD	1		/* page modified */
#define PV_REF	2		/* page referenced */
#define PV_NC	4		/* page cannot be cached */
/*efine	PV_ALLF	7		** all of the above */

struct pvlist *pv_table;	/* array of entries, one per physical page */

#define pvhead(pa)	(&pv_table[atop((pa) - vm_first_phys)])

/*
 * Each virtual segment within each pmap is either valid or invalid.
 * It is valid if pm_npte[VA_VSEG(va)] is not 0.  This does not mean
 * it is in the MMU, however; that is true iff pm_segmap[VA_VSEG(va)]
 * does not point to the invalid PMEG.
 *
 * If a virtual segment is valid and loaded, the correct PTEs appear
 * in the MMU only.  If it is valid and unloaded, the correct PTEs appear
 * in the pm_pte[VA_VSEG(va)] only.  However, some effort is made to keep
 * the software copies consistent enough with the MMU so that libkvm can
 * do user address translations.  In particular, pv_changepte() and
 * pmap_enu() maintain consistency, while less critical changes are
 * not maintained.  pm_pte[VA_VSEG(va)] always points to space for those
 * PTEs, unless this is the kernel pmap, in which case pm_pte[x] is not
 * used (sigh).
 *
 * Each PMEG in the MMU is either free or contains PTEs corresponding to
 * some pmap and virtual segment.  If it contains some PTEs, it also contains
 * reference and modify bits that belong in the pv_table.  If we need
 * to steal a PMEG from some process (if we need one and none are free)
 * we must copy the ref and mod bits, and update pm_segmap in the other
 * pmap to show that its virtual segment is no longer in the MMU.
 *
 * There are 128 PMEGs in a small Sun-4, of which only a few dozen are
 * tied down permanently, leaving `about' 100 to be spread among
 * running processes.  These are managed as an LRU cache.  Before
 * calling the VM paging code for a user page fault, the fault handler
 * calls mmu_load(pmap, va) to try to get a set of PTEs put into the
 * MMU.  mmu_load will check the validity of the segment and tell whether
 * it did something.
 *
 * Since I hate the name PMEG I call this data structure an `mmu entry'.
 * Each mmuentry is on exactly one of three `usage' lists: free, LRU,
 * or locked.  The LRU list is for user processes; the locked list is
 * for kernel entries; both are doubly linked queues headed by `mmuhd's.
 * The free list is a simple list, headed by a free list pointer.
 */
struct mmuhd {
	struct	mmuentry *mh_next;
	struct	mmuentry *mh_prev;
};
struct mmuentry {
	struct	mmuentry *me_next;	/* queue (MUST BE FIRST) or next free */
	struct	mmuentry *me_prev;	/* queue (MUST BE FIRST) */
	struct	pmap *me_pmap;		/* pmap, if in use */
	struct	mmuentry *me_pmforw;	/* pmap pmeg chain */
	struct	mmuentry **me_pmback;	/* pmap pmeg chain */
	u_short	me_vseg;		/* virtual segment number in pmap */
	pmeg_t	me_pmeg;		/* hardware PMEG number */
};
struct mmuentry *mmuentry;	/* allocated in pmap_bootstrap */

struct mmuentry *me_freelist;	/* free list (not a queue) */
struct mmuhd me_lru = {		/* LRU (user) entries */
	(struct mmuentry *)&me_lru, (struct mmuentry *)&me_lru
};
struct mmuhd me_locked = {	/* locked (kernel) entries */
	(struct mmuentry *)&me_locked, (struct mmuentry *)&me_locked
};

int	seginval;		/* the invalid segment number */

/*
 * A context is simply a small number that dictates which set of 4096
 * segment map entries the MMU uses.  The Sun 4c has eight such sets.
 * These are alloted in an `almost MRU' fashion.
 *
 * Each context is either free or attached to a pmap.
 *
 * Since the virtual address cache is tagged by context, when we steal
 * a context we have to flush (that part of) the cache.
 */
union ctxinfo {
	union	ctxinfo *c_nextfree;	/* free list (if free) */
	struct	pmap *c_pmap;		/* pmap (if busy) */
};
union ctxinfo *ctxinfo;		/* allocated at in pmap_bootstrap */
int	ncontext;

union	ctxinfo *ctx_freelist;	/* context free list */
int	ctx_kick;		/* allocation rover when none free */
int	ctx_kickdir;		/* ctx_kick roves both directions */

/* XXX need per-cpu vpage[]s (and vmempage, unless we lock in /dev/mem) */
caddr_t	vpage[2];		/* two reserved MD virtual pages */
caddr_t	vmempage;		/* one reserved MI vpage for /dev/mem */
caddr_t vdumppages;		/* 32KB worth of reserved dump pages */

struct kpmap kernel_pmap_store;	/* the kernel's pmap */

/*
 * We need to know real physical memory ranges (for /dev/mem).
 */
#define	MA_SIZE	32		/* size of memory descriptor arrays */
struct	memarr pmemarr[MA_SIZE];/* physical memory regions */
int	npmemarr;		/* number of entries in pmemarr */

/*
 * The following four global variables are set in pmap_bootstrap
 * for the vm code to find.  This is Wrong.
 */
vm_offset_t	avail_start;	/* first free physical page number */
vm_offset_t	avail_end;	/* last free physical page number */
vm_offset_t	virtual_avail;	/* first free virtual page number */
vm_offset_t	virtual_end;	/* last free virtual page number */

/*
 * pseudo-functions for mnemonic value
#ifdef notyet
 * NB: setsegmap should be stba for 4c, but stha works and makes the
 * code right for the Sun-4 as well.
#endif
 */
#define	getcontext()		lduba(AC_CONTEXT, ASI_CONTROL)
#define	setcontext(c)		stba(AC_CONTEXT, ASI_CONTROL, c)
#ifdef notyet
#define	getsegmap(va)		lduha(va, ASI_SEGMAP)
#define	setsegmap(va, pmeg)	stha(va, ASI_SEGMAP, pmeg)
#else
#define	getsegmap(va)		lduba(va, ASI_SEGMAP)
#define	setsegmap(va, pmeg)	stba(va, ASI_SEGMAP, pmeg)
#endif

#define	getpte(va)		lda(va, ASI_PTE)
#define	setpte(va, pte)		sta(va, ASI_PTE, pte)

/*----------------------------------------------------------------*/

#ifdef	sun4c
/*
 * Translations from dense (contiguous) pseudo physical addresses
 * (fed to the VM code, to keep it happy) to sparse (real, hardware)
 * physical addresses.  We call the former `software' page frame
 * numbers and the latter `hardware' page frame numbers.  The
 * translation is done on a `per bank' basis.
 *
 * The HWTOSW and SWTOHW macros handle the actual translation.
 * They are defined as no-ops on Sun-4s.
 *
 * SHOULD DO atop AND ptoa DIRECTLY IN THESE MACROS SINCE ALL CALLERS
 * ALWAYS NEED THAT ANYWAY ... CAN JUST PRECOOK THE TABLES	(TODO)
 *
 * Since we cannot use the memory allocated to the ROM monitor, and
 * this happens to be just under 64K, I have chosen a bank size of
 * 64K.  This is necessary since all banks must be completely full.
 * I have also chosen a physical memory limit of 128 MB.  The 4c is
 * architecturally limited to 256 MB, but 128 MB is more than will
 * fit on present hardware.
 *
 * XXX	FIX THIS: just make all of each bank available and then
 *	take out the pages reserved to the monitor!!
 */
#define MAXMEM 	(128 * 1024 * 1024)	/* no more than 128 MB phys mem */
#define NPGBANK	16			/* 2^4 pages per bank (64K / bank) */
#define	BSHIFT	4			/* log2(NPGBANK) */
#define BOFFSET	(NPGBANK - 1)
#define BTSIZE 	(MAXMEM / NBPG / NPGBANK)

int	pmap_dtos[BTSIZE];		/* dense to sparse */
int	pmap_stod[BTSIZE];		/* sparse to dense */

#define	HWTOSW(pg) (pmap_stod[(pg) >> BSHIFT] | ((pg) & BOFFSET))
#define	SWTOHW(pg) (pmap_dtos[(pg) >> BSHIFT] | ((pg) & BOFFSET))

/*
 * Sort a memory array by address.
 */
static void
sortm(mp, n)
	register struct memarr *mp;
	register int n;
{
	register struct memarr *mpj;
	register int i, j;
	register u_int addr, len;

	/* Insertion sort.  This is O(n^2), but so what? */
	for (i = 1; i < n; i++) {
		/* save i'th entry */
		addr = mp[i].addr;
		len = mp[i].len;
		/* find j such that i'th entry goes before j'th */
		for (j = 0, mpj = mp; j < i; j++, mpj++)
			if (addr < mpj->addr)
				break;
		/* slide up any additional entries */
		ovbcopy(mpj, mpj + 1, (i - j) * sizeof(*mp));
		mpj->addr = addr;
		mpj->len = len;
	}
}

#ifdef DEBUG
struct	memarr pmap_ama[MA_SIZE];
int	pmap_nama;
#define ama pmap_ama
#endif

/*
 * init_translations sets up pmap_dtos[] and pmap_stod[], and
 * returns the number of usable physical pages.
 */
int
init_translations()
{
	register struct memarr *mp;
	register int n, nmem;
	register u_int vbank = 0, pbank, v, a;
	register u_int pages = 0, lost = 0;
#ifndef DEBUG
	struct memarr ama[MA_SIZE];	/* available memory array */
#endif

	nmem = makememarr(ama, MA_SIZE, MEMARR_AVAILPHYS);

	/*
	 * Open Boot supposedly guarantees at least 3 MB free mem at 0;
	 * this is where the kernel has been loaded (we certainly hope the
	 * kernel is <= 3 MB).  We need the memory array to be sorted, and
	 * to start at 0, so that `software page 0' and `hardware page 0'
	 * are the same (otherwise the VM reserves the wrong pages for the
	 * kernel).
	 */
	sortm(ama, nmem);
	if (ama[0].addr != 0) {
		/* cannot panic here; there's no real kernel yet. */
		printf("init_translations: no kernel memory?!\n");
		callrom();
	}
#ifdef DEBUG
	pmap_nama = nmem;
#endif
	for (mp = ama; --nmem >= 0; mp++) {
		a = mp->addr >> PGSHIFT;
		v = mp->len >> PGSHIFT;
		if ((n = a & BOFFSET) != 0) {
			/* round up to next bank */
			n = NPGBANK - n;
			if (v < n) {	/* not a whole bank: skip it */
				lost += v;
				continue;
			}
			lost += n;	/* lose n pages from front */
			a += n;
			v -= n;
		}
		n = v >> BSHIFT;	/* calculate number of banks */
		pbank = a >> BSHIFT;	/* and the bank itself */
		if (pbank + n >= BTSIZE)
			n = BTSIZE - pbank;
		pages += n;		/* off by a factor of 2^BSHIFT */
		lost += v - (n << BSHIFT);
		while (--n >= 0) {
			pmap_dtos[vbank] = pbank << BSHIFT;
			pmap_stod[pbank] = vbank << BSHIFT;
			pbank++;
			vbank++;
		}
	}
	/* adjust page count */
	pages <<= BSHIFT;
#ifdef DEBUG
	printf("note: lost %d pages in translation\n", lost);
#endif
	return (pages);
}

#else /* sun4c */

/*
 * Pages are physically contiguous, and hardware PFN == software PFN.
 *
 * XXX assumes PAGE_SIZE == NBPG (???)
 */
#define	HWTOSW(pg)	(pg)
#define	SWTOHW(pg)	(pg)

#endif /* sun4c */

/* update pv_flags given a valid pte */
#define	MR(pte) (((pte) >> PG_M_SHIFT) & (PV_MOD | PV_REF))

/*----------------------------------------------------------------*/

/*
 * Agree with the monitor ROM as to how many MMU entries are
 * to be reserved, and map all of its segments into all contexts.
 *
 * Unfortunately, while the Version 0 PROM had a nice linked list of
 * taken virtual memory, the Version 2 PROM provides instead a convoluted
 * description of *free* virtual memory.  Rather than invert this, we
 * resort to two magic constants from the PROM vector description file.
 */
int
mmu_reservemon(nmmu)
	register int nmmu;
{
	register u_int va, eva;
	register int mmuseg, i;

	va = OPENPROM_STARTVADDR;
	eva = OPENPROM_ENDVADDR;
	while (va < eva) {
		mmuseg = getsegmap(va);
		if (mmuseg < nmmu)
			nmmu = mmuseg;
		for (i = ncontext; --i > 0;)
			(*promvec->pv_setctxt)(i, (caddr_t)va, mmuseg);
		if (mmuseg == seginval) {
			va += NBPSG;
			continue;
		}
		/* PROM maps its memory user-accessible: fix it. */
		for (i = NPTESG; --i >= 0; va += NBPG)
			setpte(va, getpte(va) | PG_S);
	}
	return (nmmu);
}

/*
 * TODO: agree with the ROM on physical pages by taking them away
 * from the page list, rather than having a dinky BTSIZE above.
 */

/*----------------------------------------------------------------*/

/*
 * MMU management.
 */

/*
 * Change contexts.  We need the old context number as well as the new
 * one.  If the context is changing, we must write all user windows
 * first, lest an interrupt cause them to be written to the (other)
 * user whose context we set here.
 */
#define	CHANGE_CONTEXTS(old, new) \
	if ((old) != (new)) { \
		write_user_windows(); \
		setcontext(new); \
	}

/*
 * Allocate an MMU entry (i.e., a PMEG).
 * If necessary, steal one from someone else.
 * Put it on the tail of the given queue
 * (which is either the LRU list or the locked list).
 * The locked list is not actually ordered, but this is easiest.
 * Also put it on the given (new) pmap's chain,
 * enter its pmeg number into that pmap's segmap,
 * and store the pmeg's new virtual segment number (me->me_vseg).
 *
 * This routine is large and complicated, but it must be fast
 * since it implements the dynamic allocation of MMU entries.
 */
struct mmuentry *
me_alloc(mh, newpm, newvseg)
	register struct mmuhd *mh;
	register struct pmap *newpm;
	register int newvseg;
{
	register struct mmuentry *me;
	register struct pmap *pm;
	register int i, va, pa, *pte, tpte;
	int ctx;

	/* try free list first */
	if ((me = me_freelist) != NULL) {
		me_freelist = me->me_next;
#ifdef DEBUG
		if (me->me_pmap != NULL)
			panic("me_alloc: freelist entry has pmap");
		if (pmapdebug & PDB_MMU_ALLOC)
			printf("me_alloc: got pmeg %x\n", me->me_pmeg);
#endif
		insque(me, mh->mh_prev);	/* onto end of queue */

		/* onto on pmap chain; pmap is already locked, if needed */
		me->me_pmforw = NULL;
		me->me_pmback = newpm->pm_mmuback;
		*newpm->pm_mmuback = me;
		newpm->pm_mmuback = &me->me_pmforw;

		/* into pmap segment table, with backpointers */
		newpm->pm_segmap[newvseg] = me->me_pmeg;
		me->me_pmap = newpm;
		me->me_vseg = newvseg;

		return (me);
	}

	/* no luck, take head of LRU list */
	if ((me = me_lru.mh_next) == (struct mmuentry *)&me_lru)
		panic("me_alloc: all pmegs gone");
	pm = me->me_pmap;
#ifdef DEBUG
	if (pm == NULL)
		panic("me_alloc: LRU entry has no pmap");
	if (pm == kernel_pmap)
		panic("me_alloc: stealing from kernel");
	pte = pm->pm_pte[me->me_vseg];
	if (pte == NULL)
		panic("me_alloc: LRU entry's pmap has no ptes");
	if (pmapdebug & (PDB_MMU_ALLOC | PDB_MMU_STEAL))
		printf("me_alloc: stealing pmeg %x from pmap %x\n",
		    me->me_pmeg, pm);
#endif
	/*
	 * Remove from LRU list, and insert at end of new list
	 * (probably the LRU list again, but so what?).
	 */
	remque(me);
	insque(me, mh->mh_prev);

	/*
	 * The PMEG must be mapped into some context so that we can
	 * read its PTEs.  Use its current context if it has one;
	 * if not, and since context 0 is reserved for the kernel,
	 * the simplest method is to switch to 0 and map the PMEG
	 * to virtual address 0---which, being a user space address,
	 * is by definition not in use.
	 *
	 * XXX for ncpus>1 must use per-cpu VA?
	 * XXX do not have to flush cache immediately
	 */
	ctx = getcontext();
	if (pm->pm_ctx) {
		CHANGE_CONTEXTS(ctx, pm->pm_ctxnum);
#ifdef notdef
		if (vactype != VAC_NONE)
#endif
			cache_flush_segment(me->me_vseg);
		va = VSTOVA(me->me_vseg);
	} else {
		CHANGE_CONTEXTS(ctx, 0);
		setsegmap(0, me->me_pmeg);
		/*
		 * No cache flush needed: it happened earlier when
		 * the old context was taken.
		 */
		va = 0;
	}

	/*
	 * Record reference and modify bits for each page,
	 * and copy PTEs into kernel memory so that they can
	 * be reloaded later.
	 */
	i = NPTESG;
	do {
		tpte = getpte(va);
		if (tpte & PG_V) {
			pa = ptoa(HWTOSW(tpte & PG_PFNUM));
			if (managed(pa))
				pvhead(pa)->pv_flags |= MR(tpte);
		}
		*pte++ = tpte & ~(PG_U|PG_M);
		va += NBPG;
	} while (--i > 0);

	/* update segment tables */
	simple_lock(&pm->pm_lock); /* what if other cpu takes mmuentry ?? */
	if (pm->pm_ctx)
		setsegmap(VSTOVA(me->me_vseg), seginval);
	pm->pm_segmap[me->me_vseg] = seginval;

	/* off old pmap chain */
	if ((*me->me_pmback = me->me_pmforw) != NULL) {
		me->me_pmforw->me_pmback = me->me_pmback;
		me->me_pmforw = NULL;
	} else
		pm->pm_mmuback = me->me_pmback;
	simple_unlock(&pm->pm_lock);
	setcontext(ctx);	/* done with old context */

	/* onto new pmap chain; new pmap is already locked, if needed */
	/* me->me_pmforw = NULL; */	/* done earlier */
	me->me_pmback = newpm->pm_mmuback;
	*newpm->pm_mmuback = me;
	newpm->pm_mmuback = &me->me_pmforw;

	/* into new segment table, with backpointers */
	newpm->pm_segmap[newvseg] = me->me_pmeg;
	me->me_pmap = newpm;
	me->me_vseg = newvseg;

	return (me);
}

/*
 * Free an MMU entry.
 *
 * Assumes the corresponding pmap is already locked.
 * Does NOT flush cache, but does record ref and mod bits.
 * The rest of each PTE is discarded.
 * CALLER MUST SET CONTEXT to pm->pm_ctxnum (if pmap has
 * a context) or to 0 (if not).  Caller must also update
 * pm->pm_segmap and (possibly) the hardware.
 */
void
me_free(pm, pmeg)
	register struct pmap *pm;
	register u_int pmeg;
{
	register struct mmuentry *me = &mmuentry[pmeg];
	register int i, va, pa, tpte;

#ifdef DEBUG
	if (pmapdebug & PDB_MMU_ALLOC)
		printf("me_free: freeing pmeg %x from pmap %x\n",
		    me->me_pmeg, pm);
	if (me->me_pmeg != pmeg)
		panic("me_free: wrong mmuentry");
	if (pm != me->me_pmap)
		panic("me_free: pm != me_pmap");
#endif

	/* just like me_alloc, but no cache flush, and context already set */
	if (pm->pm_ctx)
		va = VSTOVA(me->me_vseg);
	else {
		setsegmap(0, me->me_pmeg);
		va = 0;
	}
	i = NPTESG;
	do {
		tpte = getpte(va);
		if (tpte & PG_V) {
			pa = ptoa(HWTOSW(tpte & PG_PFNUM));
			if (managed(pa))
				pvhead(pa)->pv_flags |= MR(tpte);
		}
		va += NBPG;
	} while (--i > 0);

	/* take mmu entry off pmap chain */
	*me->me_pmback = me->me_pmforw;
	if ((*me->me_pmback = me->me_pmforw) != NULL)
		me->me_pmforw->me_pmback = me->me_pmback;
	else
		pm->pm_mmuback = me->me_pmback;
	/* ... and remove from segment map */
	pm->pm_segmap[me->me_vseg] = seginval;

	/* off LRU or lock chain */
	remque(me);

	/* no associated pmap; on free list */
	me->me_pmap = NULL;
	me->me_next = me_freelist;
	me_freelist = me;
}

/*
 * `Page in' (load or inspect) an MMU entry; called on page faults.
 * Returns 1 if we reloaded the segment, -1 if the segment was
 * already loaded and the page was marked valid (in which case the
 * fault must be a bus error or something), or 0 (segment loaded but
 * PTE not valid, or segment not loaded at all).
 */
int
mmu_pagein(pm, va, bits)
	register struct pmap *pm;
	register int va, bits;
{
	register int *pte;
	register struct mmuentry *me;
	register int vseg = VA_VSEG(va), pmeg, i, s;

	/* return 0 if we have no PTEs to load */
	if ((pte = pm->pm_pte[vseg]) == NULL)
		return (0);
	/* return -1 if the fault is `hard', 0 if not */
	if (pm->pm_segmap[vseg] != seginval)
		return (bits && (getpte(va) & bits) == bits ? -1 : 0);

	/* reload segment: write PTEs into a new LRU entry */
	va = VA_ROUNDDOWNTOSEG(va);
	s = splpmap();		/* paranoid */
	pmeg = me_alloc(&me_lru, pm, vseg)->me_pmeg;
	setsegmap(va, pmeg);
	i = NPTESG;
	do {
		setpte(va, *pte++);
		va += NBPG;
	} while (--i > 0);
	splx(s);
	return (1);
}

/*
 * Allocate a context.  If necessary, steal one from someone else.
 * Changes hardware context number and loads segment map.
 *
 * This routine is only ever called from locore.s just after it has
 * saved away the previous process, so there are no active user windows.
 */
void
ctx_alloc(pm)
	register struct pmap *pm;
{
	register union ctxinfo *c;
	register int cnum, i, va;
	register pmeg_t *segp;

#ifdef DEBUG
	if (pm->pm_ctx)
		panic("ctx_alloc pm_ctx");
	if (pmapdebug & PDB_CTX_ALLOC)
		printf("ctx_alloc(%x)\n", pm);
#endif
	if ((c = ctx_freelist) != NULL) {
		ctx_freelist = c->c_nextfree;
		cnum = c - ctxinfo;
		setcontext(cnum);
	} else {
		if ((ctx_kick += ctx_kickdir) >= ncontext) {
			ctx_kick = ncontext - 1;
			ctx_kickdir = -1;
		} else if (ctx_kick < 1) {
			ctx_kick = 1;
			ctx_kickdir = 1;
		}
		c = &ctxinfo[cnum = ctx_kick];
#ifdef DEBUG
		if (c->c_pmap == NULL)
			panic("ctx_alloc cu_pmap");
		if (pmapdebug & (PDB_CTX_ALLOC | PDB_CTX_STEAL))
			printf("ctx_alloc: steal context %x from %x\n",
			    cnum, c->c_pmap);
#endif
		c->c_pmap->pm_ctx = NULL;
		setcontext(cnum);
#ifdef notdef
		if (vactype != VAC_NONE)
#endif
			cache_flush_context();
	}
	c->c_pmap = pm;
	pm->pm_ctx = c;
	pm->pm_ctxnum = cnum;

	/*
	 * XXX	loop below makes 3584 iterations ... could reduce
	 *	by remembering valid ranges per context: two ranges
	 *	should suffice (for text/data/bss and for stack).
	 */
	segp = pm->pm_rsegmap;
	for (va = 0, i = NUSEG; --i >= 0; va += NBPSG)
		setsegmap(va, *segp++);
}

/*
 * Give away a context.  Flushes cache and sets current context to 0.
 */
void
ctx_free(pm)
	struct pmap *pm;
{
	register union ctxinfo *c;
	register int newc, oldc;

	if ((c = pm->pm_ctx) == NULL)
		panic("ctx_free");
	pm->pm_ctx = NULL;
	oldc = getcontext();
	if (vactype != VAC_NONE) {
		newc = pm->pm_ctxnum;
		CHANGE_CONTEXTS(oldc, newc);
		cache_flush_context();
		setcontext(0);
	} else {
		CHANGE_CONTEXTS(oldc, 0);
	}
	c->c_nextfree = ctx_freelist;
	ctx_freelist = c;
}


/*----------------------------------------------------------------*/

/*
 * pvlist functions.
 */

/*
 * Walk the given pv list, and for each PTE, set or clear some bits
 * (e.g., PG_W or PG_NC).
 *
 * As a special case, this never clears PG_W on `pager' pages.
 * These, being kernel addresses, are always in hardware and have
 * a context.
 *
 * This routine flushes the cache for any page whose PTE changes,
 * as long as the process has a context; this is overly conservative.
 * It also copies ref and mod bits to the pvlist, on the theory that
 * this might save work later.  (XXX should test this theory)
 */
void
pv_changepte(pv0, bis, bic)
	register struct pvlist *pv0;
	register int bis, bic;
{
	register int *pte;
	register struct pvlist *pv;
	register struct pmap *pm;
	register int va, vseg, pmeg, i, flags;
	int ctx, s;

	write_user_windows();		/* paranoid? */

	s = splpmap();			/* paranoid? */
	if (pv0->pv_pmap == NULL) {
		splx(s);
		return;
	}
	ctx = getcontext();
	flags = pv0->pv_flags;
	for (pv = pv0; pv != NULL; pv = pv->pv_next) {
		pm = pv->pv_pmap;
if(pm==NULL)panic("pv_changepte 1");
		va = pv->pv_va;
		vseg = VA_VSEG(va);
		pte = pm->pm_pte[vseg];
		if ((pmeg = pm->pm_segmap[vseg]) != seginval) {
			register int tpte;

			/* in hardware: fix hardware copy */
			if (pm->pm_ctx) {
				extern vm_offset_t pager_sva, pager_eva;

				/*
				 * Bizarreness:  we never clear PG_W on
				 * pager pages, nor PG_NC on DVMA pages.
				 */
				if (bic == PG_W &&
				    va >= pager_sva && va < pager_eva)
					continue;
				if (bic == PG_NC &&
				    va >= DVMA_BASE && va < DVMA_END)
					continue;
				setcontext(pm->pm_ctxnum);
				/* XXX should flush only when necessary */
#ifdef notdef
				if (vactype != VAC_NONE)
#endif
					cache_flush_page(va);
			} else {
				/* XXX per-cpu va? */
				setcontext(0);
				setsegmap(0, pmeg);
				va = VA_VPG(va) * NBPG;
			}
			tpte = getpte(va);
			if (tpte & PG_V)
				flags |= (tpte >> PG_M_SHIFT) &
				    (PV_MOD|PV_REF);
			tpte = (tpte | bis) & ~bic;
			setpte(va, tpte);
			if (pte != NULL)	/* update software copy */
				pte[VA_VPG(va)] = tpte;
		} else {
			/* not in hardware: just fix software copy */
			if (pte == NULL)
				panic("pv_changepte 2");
			pte += VA_VPG(va);
			*pte = (*pte | bis) & ~bic;
		}
	}
	pv0->pv_flags = flags;
	setcontext(ctx);
	splx(s);
}

/*
 * Sync ref and mod bits in pvlist (turns off same in hardware PTEs).
 * Returns the new flags.
 *
 * This is just like pv_changepte, but we never add or remove bits,
 * hence never need to adjust software copies.
 */
int
pv_syncflags(pv0)
	register struct pvlist *pv0;
{
	register struct pvlist *pv;
	register struct pmap *pm;
	register int tpte, va, vseg, pmeg, i, flags;
	int ctx, s;

	write_user_windows();		/* paranoid? */

	s = splpmap();			/* paranoid? */
	if (pv0->pv_pmap == NULL) {	/* paranoid */
		splx(s);
		return (0);
	}
	ctx = getcontext();
	flags = pv0->pv_flags;
	for (pv = pv0; pv != NULL; pv = pv->pv_next) {
		pm = pv->pv_pmap;
		va = pv->pv_va;
		vseg = VA_VSEG(va);
		if ((pmeg = pm->pm_segmap[vseg]) == seginval)
			continue;
		if (pm->pm_ctx) {
			setcontext(pm->pm_ctxnum);
			/* XXX should flush only when necessary */
#ifdef notdef
			if (vactype != VAC_NONE)
#endif
				cache_flush_page(va);
		} else {
			/* XXX per-cpu va? */
			setcontext(0);
			setsegmap(0, pmeg);
			va = VA_VPG(va) * NBPG;
		}
		tpte = getpte(va);
		if (tpte & (PG_M|PG_U) && tpte & PG_V) {
			flags |= (tpte >> PG_M_SHIFT) &
			    (PV_MOD|PV_REF);
			tpte &= ~(PG_M|PG_U);
			setpte(va, tpte);
		}
	}
	pv0->pv_flags = flags;
	setcontext(ctx);
	splx(s);
	return (flags);
}

/*
 * pv_unlink is a helper function for pmap_remove.
 * It takes a pointer to the pv_table head for some physical address
 * and removes the appropriate (pmap, va) entry.
 *
 * Once the entry is removed, if the pv_table head has the cache
 * inhibit bit set, see if we can turn that off; if so, walk the
 * pvlist and turn off PG_NC in each PTE.  (The pvlist is by
 * definition nonempty, since it must have at least two elements
 * in it to have PV_NC set, and we only remove one here.)
 */
static void
pv_unlink(pv, pm, va)
	register struct pvlist *pv;
	register struct pmap *pm;
	register vm_offset_t va;
{
	register struct pvlist *npv;

	/*
	 * First entry is special (sigh).
	 */
	npv = pv->pv_next;
	if (pv->pv_pmap == pm && pv->pv_va == va) {
		pmap_stats.ps_unlink_pvfirst++;
		if (npv != NULL) {
			pv->pv_next = npv->pv_next;
			pv->pv_pmap = npv->pv_pmap;
			pv->pv_va = npv->pv_va;
			free((caddr_t)npv, M_VMPVENT);
		} else
			pv->pv_pmap = NULL;
	} else {
		register struct pvlist *prev;

		for (prev = pv;; prev = npv, npv = npv->pv_next) {
			pmap_stats.ps_unlink_pvsearch++;
			if (npv == NULL)
				panic("pv_unlink");
			if (npv->pv_pmap == pm && npv->pv_va == va)
				break;
		}
		prev->pv_next = npv->pv_next;
		free((caddr_t)npv, M_VMPVENT);
	}
	if (pv->pv_flags & PV_NC) {
		/*
		 * Not cached: check to see if we can fix that now.
		 */
		va = pv->pv_va;
		for (npv = pv->pv_next; npv != NULL; npv = npv->pv_next)
			if (BADALIAS(va, npv->pv_va))
				return;
		pv->pv_flags &= ~PV_NC;
		pv_changepte(pv, 0, PG_NC);
	}
}

/*
 * pv_link is the inverse of pv_unlink, and is used in pmap_enter.
 * It returns PG_NC if the (new) pvlist says that the address cannot
 * be cached.
 */
static int
pv_link(pv, pm, va)
	register struct pvlist *pv;
	register struct pmap *pm;
	register vm_offset_t va;
{
	register struct pvlist *npv;
	register int ret;

	if (pv->pv_pmap == NULL) {
		/* no pvlist entries yet */
		pmap_stats.ps_enter_firstpv++;
		pv->pv_next = NULL;
		pv->pv_pmap = pm;
		pv->pv_va = va;
		return (0);
	}
	/*
	 * Before entering the new mapping, see if
	 * it will cause old mappings to become aliased
	 * and thus need to be `discached'.
	 */
	ret = 0;
	pmap_stats.ps_enter_secondpv++;
	if (pv->pv_flags & PV_NC) {
		/* already uncached, just stay that way */
		ret = PG_NC;
	} else {
		/* MAY NEED TO DISCACHE ANYWAY IF va IS IN DVMA SPACE? */
		for (npv = pv; npv != NULL; npv = npv->pv_next) {
			if (BADALIAS(va, npv->pv_va)) {
				pv->pv_flags |= PV_NC;
				pv_changepte(pv, ret = PG_NC, 0);
				break;
			}
		}
	}
	npv = (struct pvlist *)malloc(sizeof *npv, M_VMPVENT, M_WAITOK);
	npv->pv_next = pv->pv_next;
	npv->pv_pmap = pm;
	npv->pv_va = va;
	pv->pv_next = npv;
	return (ret);
}

/*
 * Walk the given list and flush the cache for each (MI) page that is
 * potentially in the cache.
 */
pv_flushcache(pv)
	register struct pvlist *pv;
{
	register struct pmap *pm;
	register int i, s, ctx;

	write_user_windows();	/* paranoia? */

	s = splpmap();		/* XXX extreme paranoia */
	if ((pm = pv->pv_pmap) != NULL) {
		ctx = getcontext();
		for (;;) {
			if (pm->pm_ctx) {
				setcontext(pm->pm_ctxnum);
				cache_flush_page(pv->pv_va);
			}
			pv = pv->pv_next;
			if (pv == NULL)
				break;
			pm = pv->pv_pmap;
		}
		setcontext(ctx);
	}
	splx(s);
}

/*----------------------------------------------------------------*/

/*
 * At last, pmap code.
 */

/*
 * Bootstrap the system enough to run with VM enabled.
 *
 * nmmu is the number of mmu entries (``PMEGs'');
 * nctx is the number of contexts.
 */
void
pmap_bootstrap(nmmu, nctx)
	int nmmu, nctx;
{
	register union ctxinfo *ci;
	register struct mmuentry *me;
	register int i, j, n, z, vs;
	register caddr_t p;
	register void (*rom_setmap)(int ctx, caddr_t va, int pmeg);
	int lastpage;
	extern char end[];
	extern caddr_t reserve_dumppages(caddr_t);

	ncontext = nctx;

	/*
	 * Last segment is the `invalid' one (one PMEG of pte's with !pg_v).
	 * It will never be used for anything else.
	 */
	seginval = --nmmu;

	/*
	 * Preserve the monitor ROM's reserved VM region, so that
	 * we can use L1-A or the monitor's debugger.  As a side
	 * effect we map the ROM's reserved VM into all contexts
	 * (otherwise L1-A crashes the machine!).
	 */
	nmmu = mmu_reservemon(nmmu);

	/*
	 * Allocate and clear mmu entry and context structures.
	 */
	p = end;
	mmuentry = me = (struct mmuentry *)p;
	p += nmmu * sizeof *me;
	ctxinfo = ci = (union ctxinfo *)p;
	p += nctx * sizeof *ci;
	bzero(end, p - end);

	/*
	 * Set up the `constants' for the call to vm_init()
	 * in main().  All pages beginning at p (rounded up to
	 * the next whole page) and continuing through the number
	 * of available pages are free, but they start at a higher
	 * virtual address.  This gives us two mappable MD pages
	 * for pmap_zero_page and pmap_copy_page, and one MI page
	 * for /dev/mem, all with no associated physical memory.
	 */
	p = (caddr_t)(((u_int)p + NBPG - 1) & ~PGOFSET);
	avail_start = (int)p - KERNBASE;
	avail_end = init_translations() << PGSHIFT;
	i = (int)p;
	vpage[0] = p, p += NBPG;
	vpage[1] = p, p += NBPG;
	vmempage = p, p += NBPG;
	p = reserve_dumppages(p);
	virtual_avail = (vm_offset_t)p;
	virtual_end = VM_MAX_KERNEL_ADDRESS;

	p = (caddr_t)i;			/* retract to first free phys */

	/*
	 * Intialize the kernel pmap.
	 */
	{
		register struct kpmap *k = &kernel_pmap_store;

/*		kernel_pmap = (struct pmap *)k; */
		k->pm_ctx = ctxinfo;
		/* k->pm_ctxnum = 0; */
		simple_lock_init(&k->pm_lock);
		k->pm_refcount = 1;
		/* k->pm_mmuforw = 0; */
		k->pm_mmuback = &k->pm_mmuforw;
		k->pm_segmap = &k->pm_rsegmap[-NUSEG];
		k->pm_pte = &k->pm_rpte[-NUSEG];
		k->pm_npte = &k->pm_rnpte[-NUSEG];
		for (i = NKSEG; --i >= 0;)
			k->pm_rsegmap[i] = seginval;
	}

	/*
	 * All contexts are free except the kernel's.
	 *
	 * XXX sun4c could use context 0 for users?
	 */
	ci->c_pmap = kernel_pmap;
	ctx_freelist = ci + 1;
	for (i = 1; i < ncontext; i++) {
		ci++;
		ci->c_nextfree = ci + 1;
	}
	ci->c_nextfree = NULL;
	ctx_kick = 0;
	ctx_kickdir = -1;

	/* me_freelist = NULL; */	/* already NULL */

	/*
	 * Init mmu entries that map the kernel physical addresses.
	 * If the page bits in p are 0, we filled the last segment
	 * exactly (now how did that happen?); if not, it is
	 * the last page filled in the last segment.
	 *
	 * All the other MMU entries are free.
	 *
	 * THIS ASSUMES SEGMENT i IS MAPPED BY MMU ENTRY i DURING THE
	 * BOOT PROCESS
	 */
	z = ((((u_int)p + NBPSG - 1) & ~SGOFSET) - KERNBASE) >> SGSHIFT;
	lastpage = VA_VPG(p);
	if (lastpage == 0)
		lastpage = NPTESG;
	p = (caddr_t)KERNBASE;		/* first va */
	vs = VA_VSEG(KERNBASE);		/* first virtual segment */
	rom_setmap = promvec->pv_setctxt;
	for (i = 0;;) {
		/*
		 * Distribute each kernel segment into all contexts.
		 * This is done through the monitor ROM, rather than
		 * directly here: if we do a setcontext we will fault,
		 * as we are not (yet) mapped in any other context.
		 */
		for (j = 1; j < nctx; j++)
			rom_setmap(j, p, i);

		/* set up the mmu entry */
		me->me_pmeg = i;
		insque(me, me_locked.mh_prev);
		/* me->me_pmforw = NULL; */
		me->me_pmback = kernel_pmap->pm_mmuback;
		*kernel_pmap->pm_mmuback = me;
		kernel_pmap->pm_mmuback = &me->me_pmforw;
		me->me_pmap = kernel_pmap;
		me->me_vseg = vs;
		kernel_pmap->pm_segmap[vs] = i;
		n = ++i < z ? NPTESG : lastpage;
		kernel_pmap->pm_npte[vs] = n;
		me++;
		vs++;
		if (i < z) {
			p += NBPSG;
			continue;
		}
		/*
		 * Unmap the pages, if any, that are not part of
		 * the final segment.
		 */
		for (p += n * NBPG; j < NPTESG; j++, p += NBPG)
			setpte(p, 0);
		break;
	}
	for (; i < nmmu; i++, me++) {
		me->me_pmeg = i;
		me->me_next = me_freelist;
		/* me->me_pmap = NULL; */
		me_freelist = me;
	}

	/*
	 * write protect & encache kernel text;
	 * set red zone at kernel base; enable cache on message buffer.
	 */
	{
		extern char etext[], trapbase[];
#ifdef KGDB
		register int mask = ~PG_NC;	/* XXX chgkprot is busted */
#else
		register int mask = ~(PG_W | PG_NC);
#endif
		for (p = trapbase; p < etext; p += NBPG)
			setpte(p, getpte(p) & mask);
		p = (caddr_t)KERNBASE;
		setpte(p, 0);
		p += NBPG;
		setpte(p, getpte(p) & ~PG_NC);
	}

	/*
	 * Grab physical memory list (for /dev/mem).
	 */
	npmemarr = makememarr(pmemarr, MA_SIZE, MEMARR_TOTALPHYS);
}

/*
 * Bootstrap memory allocator. This function allows for early dynamic
 * memory allocation until the virtual memory system has been bootstrapped.
 * After that point, either kmem_alloc or malloc should be used. This
 * function works by stealing pages from the (to be) managed page pool,
 * stealing virtual address space, then mapping the pages and zeroing them.
 *
 * It should be used from pmap_bootstrap till vm_page_startup, afterwards
 * it cannot be used, and will generate a panic if tried. Note that this
 * memory will never be freed, and in essence it is wired down.
 */
void *
pmap_bootstrap_alloc(size)
	int size;
{
	register void *mem;
	extern int vm_page_startup_initialized;

	if (vm_page_startup_initialized)
		panic("pmap_bootstrap_alloc: called after startup initialized");
	size = round_page(size);
	mem = (void *)virtual_avail;
	virtual_avail = pmap_map(virtual_avail, avail_start,
	    avail_start + size, VM_PROT_READ|VM_PROT_WRITE);
	avail_start += size;
	bzero((void *)mem, size);
	return (mem);
}

/*
 * Initialize the pmap module.
 */
void
pmap_init(phys_start, phys_end)
	register vm_offset_t phys_start, phys_end;
{
	register vm_size_t s;

	if (PAGE_SIZE != NBPG)
		panic("pmap_init: CLSIZE!=1");
	/*
	 * Allocate and clear memory for the pv_table.
	 */
	s = sizeof(struct pvlist) * atop(phys_end - phys_start);
	s = round_page(s);
	pv_table = (struct pvlist *)kmem_alloc(kernel_map, s);
	bzero((caddr_t)pv_table, s);
	vm_first_phys = phys_start;
	vm_num_phys = phys_end - phys_start;
}

/*
 * Map physical addresses into kernel VM.
 */
vm_offset_t
pmap_map(va, pa, endpa, prot)
	register vm_offset_t va, pa, endpa;
	register int prot;
{
	register int pgsize = PAGE_SIZE;

	while (pa < endpa) {
		pmap_enter(kernel_pmap, va, pa, prot, 1);
		va += pgsize;
		pa += pgsize;
	}
	return (va);
}

/*
 * Create and return a physical map.
 *
 * If size is nonzero, the map is useless. (ick)
 */
struct pmap *
pmap_create(size)
	vm_size_t size;
{
	register struct pmap *pm;

	if (size)
		return (NULL);
	pm = (struct pmap *)malloc(sizeof *pm, M_VMPMAP, M_WAITOK);
#ifdef DEBUG
	if (pmapdebug & PDB_CREATE)
		printf("pmap_create: created %x\n", pm);
#endif
	bzero((caddr_t)pm, sizeof *pm);
	pmap_pinit(pm);
	return (pm);
}

/*
 * Initialize a preallocated and zeroed pmap structure,
 * such as one in a vmspace structure.
 */
void
pmap_pinit(pm)
	register struct pmap *pm;
{
	register int i;

#ifdef DEBUG
	if (pmapdebug & PDB_CREATE)
		printf("pmap_pinit(%x)\n", pm);
#endif
	/* pm->pm_ctx = NULL; */
	simple_lock_init(&pm->pm_lock);
	pm->pm_refcount = 1;
	/* pm->pm_mmuforw = NULL; */
	pm->pm_mmuback = &pm->pm_mmuforw;
	pm->pm_segmap = pm->pm_rsegmap;
	pm->pm_pte = pm->pm_rpte;
	pm->pm_npte = pm->pm_rnpte;
	for (i = NUSEG; --i >= 0;)
		pm->pm_rsegmap[i] = seginval;
	/* bzero((caddr_t)pm->pm_rpte, sizeof pm->pm_rpte); */
	/* bzero((caddr_t)pm->pm_rnpte, sizeof pm->pm_rnpte); */
}

/*
 * Retire the given pmap from service.
 * Should only be called if the map contains no valid mappings.
 */
void
pmap_destroy(pm)
	register struct pmap *pm;
{
	int count;

	if (pm == NULL)
		return;
#ifdef DEBUG
	if (pmapdebug & PDB_DESTROY)
		printf("pmap_destroy(%x)\n", pm);
#endif
	simple_lock(&pm->pm_lock);
	count = --pm->pm_refcount;
	simple_unlock(&pm->pm_lock);
	if (count == 0) {
		pmap_release(pm);
		free((caddr_t)pm, M_VMPMAP);
	}
}

/*
 * Release any resources held by the given physical map.
 * Called when a pmap initialized by pmap_pinit is being released.
 */
void
pmap_release(pm)
	register struct pmap *pm;
{
	register union ctxinfo *c;
	register int s = splpmap();	/* paranoia */

#ifdef DEBUG
	if (pmapdebug & PDB_DESTROY)
		printf("pmap_release(%x)\n", pm);
#endif
	if (pm->pm_mmuforw)
		panic("pmap_release mmuforw");
	if ((c = pm->pm_ctx) != NULL) {
		if (pm->pm_ctxnum == 0)
			panic("pmap_release: releasing kernel");
		ctx_free(pm);
	}
	splx(s);
}

/*
 * Add a reference to the given pmap.
 */
void
pmap_reference(pm)
	struct pmap *pm;
{

	if (pm != NULL) {
		simple_lock(&pm->pm_lock);
		pm->pm_refcount++;
		simple_unlock(&pm->pm_lock);
	}
}

static int pmap_rmk(struct pmap *, vm_offset_t, vm_offset_t, int, int, int);
static int pmap_rmu(struct pmap *, vm_offset_t, vm_offset_t, int, int, int);

/*
 * Remove the given range of mapping entries.
 * The starting and ending addresses are already rounded to pages.
 * Sheer lunacy: pmap_remove is often asked to remove nonexistent
 * mappings.
 */
void
pmap_remove(pm, va, endva)
	register struct pmap *pm;
	register vm_offset_t va, endva;
{
	register vm_offset_t nva;
	register int vseg, nleft, s, ctx;
	register int (*rm)(struct pmap *, vm_offset_t, vm_offset_t,
			    int, int, int);

	if (pm == NULL)
		return;
#ifdef DEBUG
	if (pmapdebug & PDB_REMOVE)
		printf("pmap_remove(%x, %x, %x)\n", pm, va, endva);
#endif

	if (pm == kernel_pmap) {
		/*
		 * Removing from kernel address space.
		 */
		rm = pmap_rmk;
	} else {
		/*
		 * Removing from user address space.
		 */
		write_user_windows();
		rm = pmap_rmu;
	}

	ctx = getcontext();
	s = splpmap();		/* XXX conservative */
	simple_lock(&pm->pm_lock);
	for (; va < endva; va = nva) {
		/* do one virtual segment at a time */
		vseg = VA_VSEG(va);
		nva = VSTOVA(vseg + 1);
		if (nva == 0 || nva > endva)
			nva = endva;
		if ((nleft = pm->pm_npte[vseg]) != 0)
			pm->pm_npte[vseg] = (*rm)(pm, va, nva,
			    vseg, nleft, pm->pm_segmap[vseg]);
	}
	simple_unlock(&pm->pm_lock);
	splx(s);
	setcontext(ctx);
}

#define perftest
#ifdef perftest
/* counters, one per possible length */
int	rmk_vlen[NPTESG+1];	/* virtual length per rmk() call */
int	rmk_npg[NPTESG+1];	/* n valid pages per rmk() call */
int	rmk_vlendiff;		/* # times npg != vlen */
#endif

/*
 * The following magic number was chosen because:
 *	1. It is the same amount of work to cache_flush_page 4 pages
 *	   as to cache_flush_segment 1 segment (so at 4 the cost of
 *	   flush is the same).
 *	2. Flushing extra pages is bad (causes cache not to work).
 *	3. The current code, which malloc()s 5 pages for each process
 *	   for a user vmspace/pmap, almost never touches all 5 of those
 *	   pages.
 */
#define	PMAP_RMK_MAGIC	5	/* if > magic, use cache_flush_segment */

/*
 * Remove a range contained within a single segment.
 * These are egregiously complicated routines.
 */

/* remove from kernel, return new nleft */
static int
pmap_rmk(pm, va, endva, vseg, nleft, pmeg)
	register struct pmap *pm;
	register vm_offset_t va, endva;
	register int vseg, nleft, pmeg;
{
	register int i, tpte, perpage, npg;
	register struct pvlist *pv;
#ifdef perftest
	register int nvalid;
#endif

#ifdef DEBUG
	if (pmeg == seginval)
		panic("pmap_rmk: not loaded");
	if (pm->pm_ctx == NULL)
		panic("pmap_rmk: lost context");
#endif

	setcontext(0);
	/* decide how to flush cache */
	npg = (endva - va) >> PGSHIFT;
	if (npg > PMAP_RMK_MAGIC) {
		/* flush the whole segment */
		perpage = 0;
#ifdef notdef
		if (vactype != VAC_NONE)
#endif
			cache_flush_segment(vseg);
	} else {
		/* flush each page individually; some never need flushing */
		perpage = 1;
	}
#ifdef perftest
	nvalid = 0;
#endif
	while (va < endva) {
		tpte = getpte(va);
		if ((tpte & PG_V) == 0) {
			va += PAGE_SIZE;
			continue;
		}
		pv = NULL;
		/* if cacheable, flush page as needed */
		if ((tpte & PG_NC) == 0) {
#ifdef perftest
			nvalid++;
#endif
			if (perpage)
				cache_flush_page(va);
		}
		if ((tpte & PG_TYPE) == PG_OBMEM) {
			i = ptoa(HWTOSW(tpte & PG_PFNUM));
			if (managed(i)) {
				pv = pvhead(i);
				pv->pv_flags |= MR(tpte);
				pv_unlink(pv, pm, va);
			}
		}
		nleft--;
		setpte(va, 0);
		va += NBPG;
	}
#ifdef perftest
	rmk_vlen[npg]++;
	rmk_npg[nvalid]++;
	if (npg != nvalid)
		rmk_vlendiff++;
#endif

	/*
	 * If the segment is all gone, remove it from everyone and
	 * free the MMU entry.
	 */
	if (nleft == 0) {
		va = VSTOVA(vseg);		/* retract */
		setsegmap(va, seginval);
		for (i = ncontext; --i > 0;) {
			setcontext(i);
			setsegmap(va, seginval);
		}
		me_free(pm, pmeg);
	}
	return (nleft);
}

#ifdef perftest
/* as before but for pmap_rmu */
int	rmu_vlen[NPTESG+1];	/* virtual length per rmu() call */
int	rmu_npg[NPTESG+1];	/* n valid pages per rmu() call */
int	rmu_vlendiff;		/* # times npg != vlen */
int	rmu_noflush;		/* # times rmu does not need to flush at all */
#endif

/*
 * Just like pmap_rmk_magic, but we have a different threshold.
 * Note that this may well deserve further tuning work.
 */
#define	PMAP_RMU_MAGIC	4	/* if > magic, use cache_flush_segment */

/* remove from user */
static int
pmap_rmu(pm, va, endva, vseg, nleft, pmeg)
	register struct pmap *pm;
	register vm_offset_t va, endva;
	register int vseg, nleft, pmeg;
{
	register int *pte0, i, pteva, tpte, perpage, npg;
	register struct pvlist *pv;
#ifdef perftest
	register int doflush, nvalid;
#endif

	pte0 = pm->pm_pte[vseg];
	if (pmeg == seginval) {
		register int *pte = pte0 + VA_VPG(va);

		/*
		 * PTEs are not in MMU.  Just invalidate software copies.
		 */
		for (; va < endva; pte++, va += PAGE_SIZE) {
			tpte = *pte;
			if ((tpte & PG_V) == 0) {
				/* nothing to remove (braindead VM layer) */
				continue;
			}
			if ((tpte & PG_TYPE) == PG_OBMEM) {
				i = ptoa(HWTOSW(tpte & PG_PFNUM));
				if (managed(i))
					pv_unlink(pvhead(i), pm, va);
			}
			nleft--;
			*pte = 0;
		}
		if (nleft == 0) {
			free((caddr_t)pte0, M_VMPMAP);
			pm->pm_pte[vseg] = NULL;
		}
		return (nleft);
	}

	/*
	 * PTEs are in MMU.  Invalidate in hardware, update ref &
	 * mod bits, and flush cache if required.
	 */
	if (pm->pm_ctx) {
		/* process has a context, must flush cache */
		npg = (endva - va) >> PGSHIFT;
#ifdef perftest
		doflush = 1;
		nvalid = 0;
#endif
		setcontext(pm->pm_ctxnum);
		if (npg > PMAP_RMU_MAGIC) {
			perpage = 0; /* flush the whole segment */
#ifdef notdef
			if (vactype != VAC_NONE)
#endif
				cache_flush_segment(vseg);
		} else
			perpage = 1;
		pteva = va;
	} else {
		/* no context, use context 0; cache flush unnecessary */
		setcontext(0);
		/* XXX use per-cpu pteva? */
		setsegmap(0, pmeg);
		pteva = VA_VPG(va) * NBPG;
		perpage = 0;
#ifdef perftest
		npg = 0;
		doflush = 0;
		nvalid = 0;
		rmu_noflush++;
#endif
	}
	for (; va < endva; pteva += PAGE_SIZE, va += PAGE_SIZE) {
		tpte = getpte(pteva);
		if ((tpte & PG_V) == 0)
			continue;
		pv = NULL;
		/* if cacheable, flush page as needed */
		if (doflush && (tpte & PG_NC) == 0) {
#ifdef perftest
			nvalid++;
#endif
			if (perpage)
				cache_flush_page(va);
		}
		if ((tpte & PG_TYPE) == PG_OBMEM) {
			i = ptoa(HWTOSW(tpte & PG_PFNUM));
			if (managed(i)) {
				pv = pvhead(i);
				pv->pv_flags |= MR(tpte);
				pv_unlink(pv, pm, va);
			}
		}
		nleft--;
		setpte(pteva, 0);
	}
#ifdef perftest
	if (doflush) {
		rmu_vlen[npg]++;
		rmu_npg[nvalid]++;
		if (npg != nvalid)
			rmu_vlendiff++;
	}
#endif

	/*
	 * If the segment is all gone, and the context is loaded, give
	 * the segment back.
	 */
	if (nleft == 0 && pm->pm_ctx != NULL) {
		va = VSTOVA(vseg);		/* retract */
		setsegmap(va, seginval);
		free((caddr_t)pte0, M_VMPMAP);
		pm->pm_pte[vseg] = NULL;
		me_free(pm, pmeg);
	}
	return (nleft);
}

/*
 * Lower (make more strict) the protection on the specified
 * physical page.
 *
 * There are only two cases: either the protection is going to 0
 * (in which case we do the dirty work here), or it is going from
 * to read-only (in which case pv_changepte does the trick).
 */
void
pmap_page_protect(pa, prot)
	vm_offset_t pa;
	vm_prot_t prot;
{
	register struct pvlist *pv, *pv0, *npv;
	register struct pmap *pm;
	register int *pte;
	register int va, vseg, pteva, tpte;
	register int flags, nleft, i, pmeg, s, ctx, doflush;

#ifdef DEBUG
	if ((pmapdebug & PDB_CHANGEPROT) ||
	    (pmapdebug & PDB_REMOVE && prot == VM_PROT_NONE))
		printf("pmap_page_protect(%x, %x)\n", pa, prot);
#endif
	/*
	 * Skip unmanaged pages, or operations that do not take
	 * away write permission.
	 */
	if (!managed(pa) || prot & VM_PROT_WRITE)
		return;
	write_user_windows();	/* paranoia */
	if (prot & VM_PROT_READ) {
		pv_changepte(pvhead(pa), 0, PG_W);
		return;
	}

	/*
	 * Remove all access to all people talking to this page.
	 * Walk down PV list, removing all mappings.
	 * The logic is much like that for pmap_remove,
	 * but we know we are removing exactly one page.
	 */
	pv = pvhead(pa);
	s = splpmap();
	if ((pm = pv->pv_pmap) == NULL) {
		splx(s);
		return;
	}
	ctx = getcontext();
	pv0 = pv;
	flags = pv->pv_flags & ~PV_NC;
	for (;; pm = pv->pv_pmap) {
		va = pv->pv_va;
		vseg = VA_VSEG(va);
		if ((nleft = pm->pm_npte[vseg]) == 0)
			panic("pmap_remove_all: empty vseg");
		nleft--;
		pm->pm_npte[vseg] = nleft;
		pmeg = pm->pm_segmap[vseg];
		pte = pm->pm_pte[vseg];
		if (pmeg == seginval) {
			if (nleft) {
				pte += VA_VPG(va);
				*pte = 0;
			} else {
				free((caddr_t)pte, M_VMPMAP);
				pm->pm_pte[vseg] = NULL;
			}
			goto nextpv;
		}
		if (pm->pm_ctx) {
			setcontext(pm->pm_ctxnum);
			pteva = va;
#ifdef notdef
			doflush = vactype != VAC_NONE;
#else
			doflush = 1;
#endif
		} else {
			setcontext(0);
			/* XXX use per-cpu pteva? */
			setsegmap(0, pmeg);
			pteva = VA_VPG(va) * NBPG;
			doflush = 0;
		}
		if (nleft) {
			if (doflush)
				cache_flush_page(va);
			tpte = getpte(pteva);
			if ((tpte & PG_V) == 0)
				panic("pmap_page_protect !PG_V 1");
			flags |= MR(tpte);
			setpte(pteva, 0);
		} else {
			if (doflush)
				cache_flush_page(va);
			tpte = getpte(pteva);
			if ((tpte & PG_V) == 0)
				panic("pmap_page_protect !PG_V 2");
			flags |= MR(tpte);
			if (pm->pm_ctx) {
				setsegmap(va, seginval);
				if (pm == kernel_pmap) {
					for (i = ncontext; --i > 0;) {
						setcontext(i);
						setsegmap(va, seginval);
					}
					goto skipptefree;
				}
			}
			free((caddr_t)pte, M_VMPMAP);
			pm->pm_pte[vseg] = NULL;
		skipptefree:
			me_free(pm, pmeg);
		}
	nextpv:
		npv = pv->pv_next;
		if (pv != pv0)
			free((caddr_t)pv, M_VMPVENT);
		if ((pv = npv) == NULL)
			break;
	}
	pv0->pv_pmap = NULL;
	pv0->pv_flags = flags;
	setcontext(ctx);
	splx(s);
}

/*
 * Lower (make more strict) the protection on the specified
 * range of this pmap.
 *
 * There are only two cases: either the protection is going to 0
 * (in which case we call pmap_remove to do the dirty work), or
 * it is going from read/write to read-only.  The latter is
 * fairly easy.
 */
void
pmap_protect(pm, sva, eva, prot)
	register struct pmap *pm;
	vm_offset_t sva, eva;
	vm_prot_t prot;
{
	register int va, nva, vseg, pteva, pmeg;
	register int s, ctx;

	if (pm == NULL || prot & VM_PROT_WRITE)
		return;
	if ((prot & VM_PROT_READ) == 0) {
		pmap_remove(pm, sva, eva);
		return;
	}

	write_user_windows();
	ctx = getcontext();
	s = splpmap();
	simple_lock(&pm->pm_lock);

	for (va = sva; va < eva;) {
		vseg = VA_VSEG(va);
		nva = VSTOVA(vseg + 1);
if (nva == 0) panic("pmap_protect: last segment");	/* cannot happen */
		if (nva > eva)
			nva = eva;
		if (pm->pm_npte[vseg] == 0) {
			va = nva;
			continue;
		}
		pmeg = pm->pm_segmap[vseg];
		if (pmeg == seginval) {
			register int *pte = &pm->pm_pte[vseg][VA_VPG(va)];

			/* not in MMU; just clear PG_W from core copies */
			for (; va < nva; va += NBPG)
				*pte++ &= ~PG_W;
		} else {
			/* in MMU: take away write bits from MMU PTEs */
			if (
#ifdef notdef
			    vactype != VAC_NONE &&
#endif
			    pm->pm_ctx) {
				register int tpte;

				/*
				 * Flush cache so that any existing cache
				 * tags are updated.  This is really only
				 * needed for PTEs that lose PG_W.
				 */
				setcontext(pm->pm_ctxnum);
				for (; va < nva; va += NBPG) {
					tpte = getpte(va);
					pmap_stats.ps_npg_prot_all++;
					if (tpte & PG_W) {
						pmap_stats.ps_npg_prot_actual++;
						cache_flush_page(va);
						setpte(va, tpte & ~PG_W);
					}
				}
			} else {
				register int pteva;

				/*
				 * No context, hence not cached;
				 * just update PTEs.
				 */
				setcontext(0);
				/* XXX use per-cpu pteva? */
				setsegmap(0, pmeg);
				pteva = VA_VPG(va) * NBPG;
				for (; va < nva; pteva += NBPG, va += NBPG)
					setpte(pteva, getpte(pteva) & ~PG_W);
			}
		}
	}
	simple_unlock(&pm->pm_lock);
	splx(s);
}

/*
 * Change the protection and/or wired status of the given (MI) virtual page.
 * XXX: should have separate function (or flag) telling whether only wiring
 * is changing.
 */
void
pmap_changeprot(pm, va, prot, wired)
	register struct pmap *pm;
	register vm_offset_t va;
	vm_prot_t prot;
	int wired;
{
	register int vseg, tpte, newprot, pmeg, ctx, i, s;

#ifdef DEBUG
	if (pmapdebug & PDB_CHANGEPROT)
		printf("pmap_changeprot(%x, %x, %x, %x)\n",
		    pm, va, prot, wired);
#endif

	write_user_windows();	/* paranoia */

	if (pm == kernel_pmap)
		newprot = prot & VM_PROT_WRITE ? PG_S|PG_W : PG_S;
	else
		newprot = prot & VM_PROT_WRITE ? PG_W : 0;
	vseg = VA_VSEG(va);
	s = splpmap();		/* conservative */
	pmap_stats.ps_changeprots++;

	/* update PTEs in software or hardware */
	if ((pmeg = pm->pm_segmap[vseg]) == seginval) {
		register int *pte = &pm->pm_pte[vseg][VA_VPG(va)];

		/* update in software */
		if ((*pte & PG_PROT) == newprot)
			goto useless;
		*pte = (*pte & ~PG_PROT) | newprot;
	} else {
		/* update in hardware */
		ctx = getcontext();
		if (pm->pm_ctx) {
			/* use current context; flush writeback cache */
			setcontext(pm->pm_ctxnum);
			tpte = getpte(va);
			if ((tpte & PG_PROT) == newprot)
				goto useless;
			if (vactype == VAC_WRITEBACK &&
			    (newprot & PG_W) == 0 &&
			    (tpte & (PG_W | PG_NC)) == PG_W)
				cache_flush_page((int)va);
		} else {
			setcontext(0);
			/* XXX use per-cpu va? */
			setsegmap(0, pmeg);
			va = VA_VPG(va) * NBPG;
			tpte = getpte(va);
			if ((tpte & PG_PROT) == newprot)
				goto useless;
		}
		tpte = (tpte & ~PG_PROT) | newprot;
		setpte(va, tpte);
		setcontext(ctx);
	}
	splx(s);
	return;

useless:
	/* only wiring changed, and we ignore wiring */
	pmap_stats.ps_useless_changeprots++;
	splx(s);
}

/*
 * Insert (MI) physical page pa at virtual address va in the given pmap.
 * NB: the pa parameter includes type bits PMAP_OBIO, PMAP_NC as necessary.
 *
 * If pa is not in the `managed' range it will not be `bank mapped'.
 * This works during bootstrap only because the first 4MB happens to
 * map one-to-one.
 *
 * There may already be something else there, or we might just be
 * changing protections and/or wiring on an existing mapping.
 *	XXX	should have different entry points for changing!
 */
void
pmap_enter(pm, va, pa, prot, wired)
	register struct pmap *pm;
	vm_offset_t va, pa;
	vm_prot_t prot;
	int wired;
{
	register struct pvlist *pv;
	register int pteproto, ctx;

	if (pm == NULL)
		return;
#ifdef DEBUG
	if (pmapdebug & PDB_ENTER)
		printf("pmap_enter(%x, %x, %x, %x, %x)\n",
		    pm, va, pa, prot, wired);
#endif

	pteproto = PG_V | ((pa & PMAP_TNC) << PG_TNC_SHIFT);
	pa &= ~PMAP_TNC;
	/*
	 * Set up prototype for new PTE.  Cannot set PG_NC from PV_NC yet
	 * since the pvlist no-cache bit might change as a result of the
	 * new mapping.
	 */
	if (managed(pa)) {
		pteproto |= SWTOHW(atop(pa));
		pv = pvhead(pa);
	} else {
		pteproto |= atop(pa) & PG_PFNUM;
		pv = NULL;
	}
	if (prot & VM_PROT_WRITE)
		pteproto |= PG_W;

	ctx = getcontext();
	if (pm == kernel_pmap)
		pmap_enk(pm, va, prot, wired, pv, pteproto | PG_S);
	else
		pmap_enu(pm, va, prot, wired, pv, pteproto);
	setcontext(ctx);
}

/* enter new (or change existing) kernel mapping */
pmap_enk(pm, va, prot, wired, pv, pteproto)
	register struct pmap *pm;
	vm_offset_t va;
	vm_prot_t prot;
	int wired;
	register struct pvlist *pv;
	register int pteproto;
{
	register int vseg, tpte, pmeg, i, s;

	vseg = VA_VSEG(va);
	s = splpmap();		/* XXX way too conservative */
	if (pm->pm_segmap[vseg] != seginval &&
	    (tpte = getpte(va)) & PG_V) {
		register int addr = tpte & PG_PFNUM;

		/* old mapping exists */
		if (addr == (pteproto & PG_PFNUM)) {
			/* just changing protection and/or wiring */
			splx(s);
			pmap_changeprot(pm, va, prot, wired);
			return;
		}

/*printf("pmap_enk: changing existing va=>pa entry\n");*/
		/*
		 * Switcheroo: changing pa for this va.
		 * If old pa was managed, remove from pvlist.
		 * If old page was cached, flush cache.
		 */
		addr = ptoa(HWTOSW(addr));
		if (managed(addr))
			pv_unlink(pvhead(addr), pm, va);
		if (
#ifdef notdef
		    vactype != VAC_NONE &&
#endif
		    (tpte & PG_NC) == 0) {
			setcontext(0);	/* ??? */
			cache_flush_page((int)va);
		}
	} else {
		/* adding new entry */
		pm->pm_npte[vseg]++;
	}

	/*
	 * If the new mapping is for a managed PA, enter into pvlist.
	 * Note that the mapping for a malloc page will always be
	 * unique (hence will never cause a second call to malloc).
	 */
	if (pv != NULL)
		pteproto |= pv_link(pv, pm, va);

	pmeg = pm->pm_segmap[vseg];
	if (pmeg == seginval) {
		register int tva;

		/*
		 * Allocate an MMU entry now (on locked list),
		 * and map it into every context.  Set all its
		 * PTEs invalid (we will then overwrite one, but
		 * this is more efficient than looping twice).
		 */
#ifdef DEBUG
		if (pm->pm_ctx == NULL || pm->pm_ctxnum != 0)
			panic("pmap_enk: kern seg but no kern ctx");
#endif
		pmeg = me_alloc(&me_locked, pm, vseg)->me_pmeg;
		pm->pm_segmap[vseg] = pmeg;
		i = ncontext - 1;
		do {
			setcontext(i);
			setsegmap(va, pmeg);
		} while (--i >= 0);

		/* set all PTEs to invalid, then overwrite one PTE below */
		tva = VA_ROUNDDOWNTOSEG(va);
		i = NPTESG;
		do {
			setpte(tva, 0);
			tva += NBPG;
		} while (--i > 0);
	}

	/* ptes kept in hardware only */
	setpte(va, pteproto);
	splx(s);
}

/* enter new (or change existing) user mapping */
pmap_enu(pm, va, prot, wired, pv, pteproto)
	register struct pmap *pm;
	vm_offset_t va;
	vm_prot_t prot;
	int wired;
	register struct pvlist *pv;
	register int pteproto;
{
	register int vseg, *pte, tpte, pmeg, i, s, doflush;

	write_user_windows();		/* XXX conservative */
	vseg = VA_VSEG(va);
	s = splpmap();			/* XXX conservative */

	/*
	 * If there is no space in which the PTEs can be written
	 * while they are not in the hardware, this must be a new
	 * virtual segment.  Get PTE space and count the segment.
	 *
	 * TO SPEED UP CTX ALLOC, PUT SEGMENT BOUNDS STUFF HERE
	 * AND IN pmap_rmu()
	 */
retry:
	pte = pm->pm_pte[vseg];
	if (pte == NULL) {
		/* definitely a new mapping */
		register int size = NPTESG * sizeof *pte;

		pte = (int *)malloc((u_long)size, M_VMPMAP, M_WAITOK);
		if (pm->pm_pte[vseg] != NULL) {
printf("pmap_enter: pte filled during sleep\n");	/* can this happen? */
			free((caddr_t)pte, M_VMPMAP);
			goto retry;
		}
#ifdef DEBUG
		if (pm->pm_segmap[vseg] != seginval)
			panic("pmap_enter: new ptes, but not seginval");
#endif
		bzero((caddr_t)pte, size);
		pm->pm_pte[vseg] = pte;
		pm->pm_npte[vseg] = 1;
	} else {
		/* might be a change: fetch old pte */
		doflush = 0;
		if ((pmeg = pm->pm_segmap[vseg]) == seginval)
			tpte = pte[VA_VPG(va)];	/* software pte */
		else {
			if (pm->pm_ctx) {	/* hardware pte */
				setcontext(pm->pm_ctxnum);
				tpte = getpte(va);
				doflush = 1;
			} else {
				setcontext(0);
				/* XXX use per-cpu pteva? */
				setsegmap(0, pmeg);
				tpte = getpte(VA_VPG(va) * NBPG);
			}
		}
		if (tpte & PG_V) {
			register int addr = tpte & PG_PFNUM;

			/* old mapping exists */
			if (addr == (pteproto & PG_PFNUM)) {
				/* just changing prot and/or wiring */
				splx(s);
				/* caller should call this directly: */
				pmap_changeprot(pm, va, prot, wired);
				return;
			}
			/*
			 * Switcheroo: changing pa for this va.
			 * If old pa was managed, remove from pvlist.
			 * If old page was cached, flush cache.
			 */
/*printf("%s[%d]: pmap_enu: changing existing va(%x)=>pa entry\n",
curproc->p_comm, curproc->p_pid, va);*/
			addr = ptoa(HWTOSW(addr));
			if (managed(addr))
				pv_unlink(pvhead(addr), pm, va);
			if (
#ifdef notdef
			    vactype != VAC_NONE &&
#endif
			    doflush && (tpte & PG_NC) == 0)
				cache_flush_page((int)va);
		} else {
			/* adding new entry */
			pm->pm_npte[vseg]++;
		}
	}

	if (pv != NULL)
		pteproto |= pv_link(pv, pm, va);

	/*
	 * Update hardware or software PTEs (whichever are active).
	 */
	if ((pmeg = pm->pm_segmap[vseg]) != seginval) {
		/* ptes are in hardare */
		if (pm->pm_ctx)
			setcontext(pm->pm_ctxnum);
		else {
			setcontext(0);
			/* XXX use per-cpu pteva? */
			setsegmap(0, pmeg);
			va = VA_VPG(va) * NBPG;
		}
		setpte(va, pteproto);
	}
	/* update software copy */
	pte += VA_VPG(va);
	*pte = pteproto;

	splx(s);
}

/*
 * Change the wiring attribute for a map/virtual-address pair.
 */
/* ARGSUSED */
void
pmap_change_wiring(pm, va, wired)
	struct pmap *pm;
	vm_offset_t va;
	int wired;
{

	pmap_stats.ps_useless_changewire++;
}

/*
 * Extract the physical page address associated
 * with the given map/virtual_address pair.
 * GRR, the vm code knows; we should not have to do this!
 */
vm_offset_t
pmap_extract(pm, va)
	register struct pmap *pm;
	vm_offset_t va;
{
	register int tpte;
	register int vseg;

	if (pm == NULL) {
		printf("pmap_extract: null pmap\n");
		return (0);
	}
	vseg = VA_VSEG(va);
	if (pm->pm_segmap[vseg] != seginval) {
		register int ctx = getcontext();

		if (pm->pm_ctx) {
			setcontext(pm->pm_ctxnum);
			tpte = getpte(va);
		} else {
			setcontext(0);
			tpte = getpte(VA_VPG(va) * NBPG);
		}
		setcontext(ctx);
	} else {
		register int *pte = pm->pm_pte[vseg];

		if (pte == NULL) {
			printf("pmap_extract: invalid vseg\n");
			return (0);
		}
		tpte = pte[VA_VPG(va)];
	}
	if ((tpte & PG_V) == 0) {
		printf("pmap_extract: invalid pte\n");
		return (0);
	}
	tpte &= PG_PFNUM;
	tpte = HWTOSW(tpte);
	return ((tpte << PGSHIFT) | (va & PGOFSET));
}

/*
 * Copy the range specified by src_addr/len
 * from the source map to the range dst_addr/len
 * in the destination map.
 *
 * This routine is only advisory and need not do anything.
 */
/* ARGSUSED */
void
pmap_copy(dst_pmap, src_pmap, dst_addr, len, src_addr)
	struct pmap *dst_pmap, *src_pmap;
	vm_offset_t dst_addr;
	vm_size_t len;
	vm_offset_t src_addr;
{
}

/*
 * Require that all active physical maps contain no
 * incorrect entries NOW.  [This update includes
 * forcing updates of any address map caching.]
 */
void
pmap_update()
{
}

/*
 * Garbage collects the physical map system for
 * pages which are no longer used.
 * Success need not be guaranteed -- that is, there
 * may well be pages which are not referenced, but
 * others may be collected.
 * Called by the pageout daemon when pages are scarce.
 */
/* ARGSUSED */
void
pmap_collect(pm)
	struct pmap *pm;
{
}

/*
 * Clear the modify bit for the given physical page.
 */
void
pmap_clear_modify(pa)
	register vm_offset_t pa;
{
	register struct pvlist *pv;

	if (managed(pa)) {
		pv = pvhead(pa);
		(void) pv_syncflags(pv);
		pv->pv_flags &= ~PV_MOD;
	}
}

/*
 * Tell whether the given physical page has been modified.
 */
int
pmap_is_modified(pa)
	register vm_offset_t pa;
{
	register struct pvlist *pv;

	if (managed(pa)) {
		pv = pvhead(pa);
		if (pv->pv_flags & PV_MOD || pv_syncflags(pv) & PV_MOD)
			return (1);
	}
	return (0);
}

/*
 * Clear the reference bit for the given physical page.
 */
void
pmap_clear_reference(pa)
	vm_offset_t pa;
{
	register struct pvlist *pv;

	if (managed(pa)) {
		pv = pvhead(pa);
		(void) pv_syncflags(pv);
		pv->pv_flags &= ~PV_REF;
	}
}

/*
 * Tell whether the given physical page has been referenced.
 */
int
pmap_is_referenced(pa)
	vm_offset_t pa;
{
	register struct pvlist *pv;

	if (managed(pa)) {
		pv = pvhead(pa);
		if (pv->pv_flags & PV_REF || pv_syncflags(pv) & PV_REF)
			return (1);
	}
	return (0);
}

/*
 * Make the specified pages (by pmap, offset) pageable (or not) as requested.
 *
 * A page which is not pageable may not take a fault; therefore, its page
 * table entry must remain valid for the duration (or at least, the trap
 * handler must not call vm_fault).
 *
 * This routine is merely advisory; pmap_enter will specify that these pages
 * are to be wired down (or not) as appropriate.
 */
/* ARGSUSED */
void
pmap_pageable(pm, start, end, pageable)
	struct pmap *pm;
	vm_offset_t start, end;
	int pageable;
{
}

/*
 * Fill the given MI physical page with zero bytes.
 *
 * We avoid stomping on the cache.
 * XXX	might be faster to use destination's context and allow cache to fill?
 */
void
pmap_zero_page(pa)
	register vm_offset_t pa;
{
	register caddr_t va;
	register int pte;

	if (managed(pa)) {
		/*
		 * The following might not be necessary since the page
		 * is being cleared because it is about to be allocated,
		 * i.e., is in use by no one.
		 */
#if 1
#ifdef notdef
		if (vactype != VAC_NONE)
#endif
			pv_flushcache(pvhead(pa));
#endif
		pte = PG_V | PG_S | PG_W | PG_NC | SWTOHW(atop(pa));
	} else
		pte = PG_V | PG_S | PG_W | PG_NC | (atop(pa) & PG_PFNUM);

	va = vpage[0];
	setpte(va, pte);
	qzero(va, NBPG);
	setpte(va, 0);
}

/*
 * Copy the given MI physical source page to its destination.
 *
 * We avoid stomping on the cache as above (with same `XXX' note).
 * We must first flush any write-back cache for the source page.
 * We go ahead and stomp on the kernel's virtual cache for the
 * source page, since the cache can read memory MUCH faster than
 * the processor.
 */
void
pmap_copy_page(src, dst)
	vm_offset_t src, dst;
{
	register caddr_t sva, dva;
	register int spte, dpte;

	if (managed(src)) {
		if (vactype == VAC_WRITEBACK)
			pv_flushcache(pvhead(src));
		spte = PG_V | PG_S | SWTOHW(atop(src));
	} else
		spte = PG_V | PG_S | (atop(src) & PG_PFNUM);

	if (managed(dst)) {
		/* similar `might not be necessary' comment applies */
#if 1
#ifdef notdef
		if (vactype != VAC_NONE)
#endif
			pv_flushcache(pvhead(dst));
#endif
		dpte = PG_V | PG_S | PG_W | PG_NC | SWTOHW(atop(dst));
	} else
		dpte = PG_V | PG_S | PG_W | PG_NC | (atop(dst) & PG_PFNUM);

	sva = vpage[0];
	dva = vpage[1];
	setpte(sva, spte);
	setpte(dva, dpte);
	qcopy(sva, dva, NBPG);	/* loads cache, so we must ... */
	cache_flush_page((int)sva);
	setpte(sva, 0);
	setpte(dva, 0);
}

/*
 * Turn a cdevsw d_mmap value into a byte address for pmap_enter.
 * XXX	this should almost certainly be done differently, and
 *	elsewhere, or even not at all
 */
vm_offset_t
pmap_phys_address(x)
	int x;
{

	return (x);
}

/*
 * Turn off cache for a given (va, number of pages).
 *
 * We just assert PG_NC for each PTE; the addresses must reside
 * in locked kernel space.  A cache flush is also done.
 */
kvm_uncache(va, npages)
	register caddr_t va;
	register int npages;
{
	register int pte;

	for (; --npages >= 0; va += NBPG) {
		pte = getpte(va);
		if ((pte & PG_V) == 0)
			panic("kvm_uncache !pg_v");
		pte |= PG_NC;
		setpte(va, pte);
		cache_flush_page((int)va);
	}
}

/*
 * For /dev/mem.
 */
int
pmap_enter_hw(pm, va, pa, prot, wired)
	register struct pmap *pm;
	vm_offset_t va, pa;
	vm_prot_t prot;
	int wired;
{
	register struct memarr *ma;
	register int n;
	register u_int t;

	if (pa >= MAXMEM)				/* ??? */
		return (EFAULT);
	for (ma = pmemarr, n = npmemarr; --n >= 0; ma++) {
		t = (u_int)pa - ma->addr;
		if (t < ma->len)
			goto ok;
	}
	return (EFAULT);
ok:
	pa = (HWTOSW(atop(pa)) << PGSHIFT) | (pa & PGOFSET);
	if (pa >= vm_first_phys + vm_num_phys)		/* ??? */
		return (EFAULT);

	pmap_enter(pm, va, pa, prot, wired);
	return (0);
}
