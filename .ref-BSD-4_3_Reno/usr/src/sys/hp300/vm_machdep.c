/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: vm_machdep.c 1.18 89/08/23$
 *
 *	@(#)vm_machdep.c	7.5 (Berkeley) 6/21/90
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "cmap.h"
#include "vm.h"
#include "text.h"
#include "malloc.h"
#include "buf.h"

#include "cpu.h"
#include "pte.h"

/*
 * Set a red zone in the kernel stack after the u. area.
 * We don't support a redzone right now.  It really isn't clear
 * that it is a good idea since, if the kernel stack were to roll
 * into a write protected page, the processor would lock up (since
 * it cannot create an exception frame) and we would get no useful
 * post-mortem info.  Currently, under the DEBUG option, we just
 * check at every clock interrupt to see if the current k-stack has
 * gone too far (i.e. into the "redzone" page) and if so, panic.
 * Look at _lev6intr in locore.s for more details.
 */
/*ARGSUSED*/
setredzone(pte, vaddr)
	struct pte *pte;
	caddr_t vaddr;
{
}

/*
 * Check for valid program size
 * NB - Check data and data growth separately as they may overflow 
 * when summed together.
 */
chksize(ts, ids, uds, ss)
	unsigned ts, ids, uds, ss;
{
	extern unsigned maxtsize;

	if (ctob(ts) > maxtsize ||
	    ctob(ids) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(uds) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(ids + uds) > u.u_rlimit[RLIMIT_DATA].rlim_cur ||
	    ctob(ss) > u.u_rlimit[RLIMIT_STACK].rlim_cur) {
		return (ENOMEM);
	}
	return (0);
}

/*ARGSUSED*/
newptes(pte, v, size)
	struct pte *pte;
	u_int v;
	register int size;
{
	register caddr_t a;

#ifdef lint
	pte = pte;
#endif
	if (size >= 8)
		TBIAU();
	else {
		a = ptob(v);
		while (size > 0) {
			TBIS(a);
			a += NBPG;
			size--;
		}
	}
	DCIU();
}

/*
 * Change protection codes of text segment.
 * Have to flush translation buffer since this
 * affect virtual memory mapping of current process.
 */
chgprot(addr, tprot)
	caddr_t addr;
	long tprot;
{
	unsigned v;
	int tp;
	register struct pte *pte;
	register struct cmap *c;

	v = clbase(btop(addr));
	if (!isatsv(u.u_procp, v))
		return (EFAULT);
	tp = vtotp(u.u_procp, v);
	pte = tptopte(u.u_procp, tp);
	if (pte->pg_fod == 0 && pte->pg_pfnum) {
		c = &cmap[pgtocm(pte->pg_pfnum)];
		if (c->c_blkno)
			munhash(c->c_vp, (daddr_t)(u_long)c->c_blkno);
	}
	*(u_int *)pte &= ~PG_PROT;
	*(u_int *)pte |= tprot;
	TBIS(addr);
	return (0);
}

settprot(tprot)
	long tprot;
{
	register u_int *pte, i;

	pte = (u_int *)u.u_procp->p_p0br;
	for (i = 0; i < u.u_tsize; i++, pte++) {
		*pte &= ~PG_PROT;
		*pte |= tprot;
	}
	TBIAU();
}

/*
 * Simulate effect of VAX region length registers.
 * The one case where we must do anything is if a region has shrunk.
 * In that case we must invalidate all the PTEs for the no longer valid VAs.
 */
setptlr(region, nlen)
	int nlen;
{
	register struct pte *pte;
	register int change;
	int olen;

	if (region == 0) {
		olen = u.u_pcb.pcb_p0lr;
		u.u_pcb.pcb_p0lr = nlen;
	} else {
		olen = P1PAGES - u.u_pcb.pcb_p1lr;
		u.u_pcb.pcb_p1lr = nlen;
		nlen = P1PAGES - nlen;
	}
	if ((change = olen - nlen) <= 0)
		return;
	if (region == 0)
		pte = u.u_pcb.pcb_p0br + u.u_pcb.pcb_p0lr;
	else
		pte = u.u_pcb.pcb_p1br + u.u_pcb.pcb_p1lr - change;
	do {
		*(u_int *)pte++ = PG_NV;
	} while (--change);
	/* short cut newptes */
	TBIAU();
	DCIU();
}

/*
 * Map `size' bytes of physical memory starting at `paddr' into
 * kernel VA space using PTEs starting at `pte'.  Read/write and
 * cache-inhibit status are specified by `prot'.
 */ 
physaccess(pte, paddr, size, prot)
	register struct pte *pte;
	caddr_t paddr;
	register int size;
{
	register u_int page;

	page = (u_int)paddr & PG_FRAME;
	for (size = btoc(size); size; size--) {
		*(int *)pte = PG_V | prot | page;
		page += NBPG;
		pte++;
	}
	TBIAS();
}

/*
 * Move pages from one kernel virtual address to another.
 * Both addresses are assumed to reside in the Sysmap,
 * and size must be a multiple of CLSIZE.
 */
pagemove(from, to, size)
	register caddr_t from, to;
	int size;
{
	register struct pte *fpte, *tpte;

	if (size % CLBYTES)
		panic("pagemove");
	fpte = kvtopte(from);
	tpte = kvtopte(to);
	while (size > 0) {
		*tpte++ = *fpte;
		*(int *)fpte++ = PG_NV;
		TBIS(from);
		TBIS(to);
		from += NBPG;
		to += NBPG;
		size -= NBPG;
	}
}

#ifdef KGDB
/*
 * Change protections on kernel pages from addr to addr+size
 * (presumably so debugger can plant a breakpoint).
 * All addresses are assumed to reside in the Sysmap,
 */
chgkprot(addr, size, rw)
	register caddr_t addr;
	int size, rw;
{
	register struct pte *pte;

	pte = &Sysmap[btop(addr)];
	while (size > 0) {
		pte->pg_prot = rw == B_WRITE? 0 : 1;
		TBIS(addr);
		addr += NBPG;
		size -= NBPG;
		pte++;
	}
}
#endif

/*
 * The probe[rw] routines should probably be redone in assembler
 * for efficiency.
 */
prober(addr)
	register u_int addr;
{
	register int page;
	register struct proc *p;

	if (addr >= USRSTACK)
		return(0);
#ifdef HPUXCOMPAT
	if (ISHPMMADDR(addr))
		addr = HPMMBASEADDR(addr);
#endif
	page = btop(addr);
	p = u.u_procp;
	if (page < dptov(p, p->p_dsize) || page > sptov(p, p->p_ssize))
		return(1);
#ifdef MAPMEM
	if (page < dptov(p, p->p_dsize+p->p_mmsize) &&
	    (*(int *)vtopte(p, page) & (PG_FOD|PG_V)) == (PG_FOD|PG_V))
		return(1);
#endif
	return(0);
}

probew(addr)
	register u_int addr;
{
	register int page;
	register struct proc *p;

	if (addr >= USRSTACK)
		return(0);
#ifdef HPUXCOMPAT
	if (ISHPMMADDR(addr))
		addr = HPMMBASEADDR(addr);
#endif
	page = btop(addr);
	p = u.u_procp;
	if (page < dptov(p, p->p_dsize) || page > sptov(p, p->p_ssize))
		return((*(int *)vtopte(p, page) & PG_PROT) == PG_RW);
#ifdef MAPMEM
	if (page < dptov(p, p->p_dsize+p->p_mmsize))
		return((*(int *)vtopte(p, page) & (PG_FOD|PG_V|PG_PROT))
		       == (PG_FOD|PG_V|PG_RW));
#endif
	return(0);
}

/*
 * NB: assumes a physically contiguous kernel page table
 *     (makes life a LOT simpler).
 */
kernacc(addr, count, rw)
	register caddr_t addr;
	int count, rw;
{
	register struct ste *ste;
	register struct pte *pte;
	register u_int ix, cnt;
	extern long Syssize;

	if (count <= 0)
		return(0);
	ix = ((int)addr & SG_IMASK) >> SG_ISHIFT;
	cnt = (((int)addr + count + (1<<SG_ISHIFT)-1) & SG_IMASK) >> SG_ISHIFT;
	cnt -= ix;
	for (ste = &Sysseg[ix]; cnt; cnt--, ste++)
		/* should check SG_PROT, but we have no RO segments now */
		if (ste->sg_v == 0)
			return(0);
	ix = btop(addr);
	cnt = btop(addr+count+NBPG-1);
	if (cnt > (u_int)&Syssize)
		return(0);
	cnt -= ix;
	for (pte = &Sysmap[ix]; cnt; cnt--, pte++)
		if (pte->pg_v == 0 || (rw == B_WRITE && pte->pg_prot == 1))
			return(0);
	return(1);
}

useracc(addr, count, rw)
	register caddr_t addr;
	unsigned count;
{
	register int (*func)();
	register u_int addr2;
	extern int prober(), probew();

	if (count <= 0)
		return(0);
	addr2 = (u_int) addr;
	addr += count;
	func = (rw == B_READ) ? prober : probew;
	do {
		if ((*func)(addr2) == 0)
			return(0);
		addr2 = (addr2 + NBPG) & ~PGOFSET;
	} while (addr2 < (u_int)addr);
	return(1);
}

/*
 * Convert kernel VA to physical address
 */
kvtop(addr)
	register caddr_t addr;
{
	register int pf;

	pf = Sysmap[btop(addr)].pg_pfnum;
	if (pf == 0)
		panic("kvtop: zero page frame");
	return((u_int)ptob(pf) + ((int)addr & PGOFSET));
}

struct ste *
vtoste(p, va)
	register struct proc *p;
	register u_int va;
{
	register struct ste *ste;

	ste = (struct ste *)((u_int)p->p_p0br + p->p_szpt * NBPG);
	return(ste + ((va & SG_IMASK) >> SG_ISHIFT));
}

initustp(p)
	register struct proc *p;
{
	return((int)Usrptmap[btokmx(p->p_p0br) + p->p_szpt].pg_pfnum);
}

/*
 * Initialize segment table to reflect PTEs in Usrptmap.
 * Segment table address is given by Usrptmap index of p_szpt.
 */
initsegt(p)
	register struct proc *p;
{
	register int i, k, sz;
	register struct ste *ste;
	extern struct ste *vtoste();

	k = btokmx(p->p_p0br);
	ste = vtoste(p, 0);
	/* text and data */
	sz = ctopt(p->p_tsize + p->p_dsize + p->p_mmsize);
	for (i = 0; i < sz; i++, ste++) {
		*(int *)ste = SG_RW | SG_V;
		ste->sg_pfnum = Usrptmap[k++].pg_pfnum;
	}
	/*
	 * Bogus!  The kernelmap may map unused PT pages
	 * (since we don't shrink PTs) so we need to skip over
	 * those STEs.  We should really free the unused PT
	 * pages in expand().
	 */
	sz += ctopt(p->p_ssize + HIGHPAGES);
	if (sz < p->p_szpt)
		k += p->p_szpt - sz;
	/* hole */
	sz = NPTEPG - ctopt(p->p_ssize + HIGHPAGES);
	for ( ; i < sz; i++, ste++)
		*(int *)ste = SG_NV;
	/* stack and u-area */
	sz = NPTEPG;
	for ( ; i < sz; i++, ste++) {
		*(int *)ste = SG_RW | SG_V;
		ste->sg_pfnum = Usrptmap[k++].pg_pfnum;
	}
}

/*
 * Allocate/free cache-inhibited physical memory.
 * Assumes that malloc returns page aligned memory for requests which are
 * a multiple of the page size.  Hence, size must be such a multiple.
 */
caddr_t
cialloc(sz)
	int sz;
{
	caddr_t kva;
	register int npg, *pte;

	if (sz & CLOFSET)
		return(NULL);
	kva = (caddr_t)malloc(sz, M_DEVBUF, M_NOWAIT);
	if (kva) {
		if (!claligned(kva))
			panic("cialloc");
		pte = (int *)kvtopte(kva);
		npg = btoc(sz);
		while (--npg >= 0)
			*pte++ |= PG_CI;
		TBIAS();
	}
	return(kva);
}

cifree(kva, sz)
	caddr_t kva;
	int sz;
{
	register int npg, *pte;

	if (sz & CLOFSET)
		panic("cifree");
	pte = (int *)kvtopte(kva);
	npg = btoc(sz);
	while (--npg >= 0)
		*pte++ &= ~PG_CI;
	TBIAS();
	free(kva, M_DEVBUF);
}

extern char usrio[];
extern struct pte Usriomap[];
struct map *useriomap;
int usriowanted;

/*
 * Map an IO request into kernel virtual address space.  Requests fall into
 * one of five catagories:
 *
 *	B_PHYS|B_UAREA:	User u-area swap.
 *			Address is relative to start of u-area (p_addr).
 *	B_PHYS|B_PAGET:	User page table swap.
 *			Address is a kernel VA in usrpt (Usrptmap).
 *	B_PHYS|B_DIRTY:	Dirty page push.
 *			Address is a VA in proc2's address space.
 *	B_PHYS|B_PGIN:	Kernel pagein of user pages.
 *			Address is VA in user's address space.
 *	B_PHYS:		User "raw" IO request.
 *			Address is VA in user's address space.
 *
 * All requests are (re)mapped into kernel VA space via the useriomap
 * (a name with only slightly more meaning than "kernelmap")
 */
vmapbuf(bp)
	register struct buf *bp;
{
	register int npf, a;
	register caddr_t addr;
	register struct pte *pte, *iopte;
	register long flags = bp->b_flags;
	struct proc *p;
	int off, s;

	if ((flags & B_PHYS) == 0)
		panic("vmapbuf");
	/*
	 * Find PTEs for the area to be mapped
	 */
	p = flags&B_DIRTY ? &proc[2] : bp->b_proc;
	addr = bp->b_un.b_addr;
	if (flags & B_UAREA)
		pte = &p->p_addr[btop(addr)];
	else if (flags & B_PAGET)
		pte = &Usrptmap[btokmx((struct pte *)addr)];
	else
		pte = vtopte(p, btop(addr));
	/*
	 * Allocate some kernel PTEs and load them
	 */
	off = (int)addr & PGOFSET;
	npf = btoc(bp->b_bcount + off);
	s = splbio();
	while ((a = rmalloc(useriomap, npf)) == 0) {
		usriowanted = 1;
		sleep((caddr_t)useriomap, PSWP);
	}
	splx(s);
	iopte = &Usriomap[a];
	bp->b_saveaddr = bp->b_un.b_addr;
	addr = bp->b_un.b_addr = (caddr_t)(usrio + (a << PGSHIFT)) + off;
	while (npf--) {
		mapin(iopte, (u_int)addr, pte->pg_pfnum, PG_CI|PG_RW|PG_V);
		iopte++, pte++;
		addr += NBPG;
	}
}

/*
 * Free the io map PTEs associated with this IO operation.
 * We also invalidate the TLB entries and restore the original b_addr.
 */
vunmapbuf(bp)
	register struct buf *bp;
{
	register int a, npf;
	register caddr_t addr = bp->b_un.b_addr;
	register struct pte *pte;
	int s;

	if ((bp->b_flags & B_PHYS) == 0)
		panic("vunmapbuf");
	a = (int)(addr - usrio) >> PGSHIFT;
	npf = btoc(bp->b_bcount + ((int)addr & PGOFSET));
	s = splbio();
	rmfree(useriomap, npf, a);
	if (usriowanted) {
		usriowanted = 0;
		wakeup((caddr_t)useriomap); 
	}
	splx(s);
	pte = &Usriomap[a];
	while (npf--) {
		*(int *)pte = PG_NV;
		TBIS((caddr_t)addr);
		addr += NBPG;
		pte++;
	}
	bp->b_un.b_addr = bp->b_saveaddr;
	bp->b_saveaddr = NULL;
}
