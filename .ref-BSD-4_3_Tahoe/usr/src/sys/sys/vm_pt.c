/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vm_pt.c	7.6 (Berkeley) 5/26/88
 */
#include "../machine/pte.h"
#include "../machine/mtpr.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "map.h"
#include "cmap.h"
#include "vm.h"
#include "buf.h"
#include "text.h"
#include "mount.h"
#include "inode.h"
#include "kernel.h"

/*
 * Get page tables for process p.  Allocator
 * for memory is argument; process must be locked
 * from swapping if vmemall is used; if memall is
 * used, call will return w/o waiting for memory.
 * In any case an error return results if no user
 * page table space is available.
 */
vgetpt(p, pmemall)
	register struct proc *p;
	int (*pmemall)();
{
	register long a;

	if (p->p_szpt == 0)
		panic("vgetpt");
	/*
	 * Allocate space in the kernel map for this process.
	 * Then allocate page table pages, and initialize the
	 * process' p0br and addr pointer to be the kernel
	 * virtual addresses of the base of the page tables and
	 * the pte for the process pcb (at the base of the u.).
	 */
	a = rmalloc(kernelmap, (long)p->p_szpt);
	if (a == 0)
		return (0);
	if ((*pmemall)(&Usrptmap[a], p->p_szpt, p, CSYS) == 0) {
		rmfree(kernelmap, (long)p->p_szpt, a);
		return (0);
	}
	p->p_p0br = kmxtob(a);
	p->p_addr = uaddr(p);
	/*
	 * Now validate the system page table entries for the
	 * user page table pages, flushing old translations
	 * for these kernel virtual addresses.  Clear the new
	 * page table pages for clean post-mortems.
	 */
	vmaccess(&Usrptmap[a], (caddr_t)p->p_p0br, p->p_szpt);
	bzero((caddr_t) p->p_p0br, (unsigned) (p->p_szpt * NBPG));
	return (1);
}

/*
 * Initialize text portion of page table.
 */
vinitpt(p)
	struct proc *p;
{
	register struct text *xp;
	register struct proc *q;
	register struct pte *pte;
	register int i;
	struct pte proto;

	xp = p->p_textp;
	if (xp == 0)
		return;
	pte = tptopte(p, 0);
	/*
	 * If there is another instance of same text in core
	 * then just copy page tables from other process.
	 */
	if (q = xp->x_caddr) {
		bcopy((caddr_t)tptopte(q, 0), (caddr_t)pte,
		    (unsigned) (sizeof(struct pte) * xp->x_size));
		goto done;
	}
	if (xp->x_flag & XLOAD || (xp->x_flag & XPAGI) == 0) {
		/*
		 * Initialize text page tables, zfod if we are loading
		 * the text now; unless the process is demand loaded,
		 * this will suffice as the text will henceforth either be
		 * read from a file or demand paged in.
		 */
		*(int *)&proto = PG_URKR;
		if (xp->x_flag & XLOAD) {
			proto.pg_fod = 1;
			((struct fpte *)&proto)->pg_fileno = PG_FZERO;
		}
		for (i = 0; i < xp->x_size; i++)
			*pte++ = proto;
		if ((xp->x_flag & XPAGI) == 0)
			goto done;
	}
	/*
	 * Text is demand loaded.  If process is not loaded (i.e. being
	 * swapped in) then retrieve page tables from swap area.  Otherwise
	 * this is the first time and we must initialize the page tables
	 * from the blocks in the file system.
	 */
	if (xp->x_flag & XLOAD)
		vinifod((struct fpte *)tptopte(p, 0), PG_FTEXT, xp->x_iptr,
		    (daddr_t)1, xp->x_size);
	else
		(void) swap(p, xp->x_ptdaddr, (caddr_t)tptopte(p, 0),
		    (int)(xp->x_size * sizeof (struct pte)), B_READ,
		    B_PAGET, swapdev, 0);
done:
	/*
	 * In the case where we are overlaying ourself with new page
	 * table entries, old user-space translations should be flushed.
	 */
	if (p == u.u_procp)
		newptes(tptopte(p, 0), tptov(p, 0), (int)xp->x_size);
	else
		p->p_flag |= SPTECHG;
}

/*
 * Update the page tables of all processes linked
 * to a particular text segment, by distributing
 * dpte to the the text page at virtual frame v.
 *
 * Note that invalidation in the translation buffer for
 * the current process is the responsibility of the caller.
 */
distpte(xp, tp, dpte)
	struct text *xp;
	register unsigned tp;
	register struct pte *dpte;
{
	register struct proc *p;
	register struct pte *pte;
	register int i;

	for (p = xp->x_caddr; p; p = p->p_xlink) {
		pte = tptopte(p, tp);
		p->p_flag |= SPTECHG;
		if (pte != dpte)
			for (i = 0; i < CLSIZE; i++)
				pte[i] = dpte[i];
	}
}

/*
 * Release page tables of process p.
 */
vrelpt(p)
	register struct proc *p;
{
	register int a;

	if (p->p_szpt == 0)
		return;
	a = btokmx(p->p_p0br);
	(void) vmemfree(&Usrptmap[a], p->p_szpt);
	rmfree(kernelmap, (long)p->p_szpt, (long)a);
}

#define	Xu(a)	t = up->u_pcb.a; up->u_pcb.a = uq ->u_pcb.a; uq->u_pcb.a = t;
#define	Xup(a)	tp = up->u_pcb.a; up->u_pcb.a = uq ->u_pcb.a; uq->u_pcb.a = tp;
#define	Xp(a)	t = p->a; p->a = q->a; q->a = t;
#define	Xpp(a)	tp = p->a; p->a = q->a; q->a = tp;

/*
 * Pass the page tables of process p to process q.
 * Used during vfork().  P and q are not symmetric;
 * p is the giver and q the receiver; after calling vpasspt
 * p will be ``cleaned out''.  Thus before vfork() we call vpasspt
 * with the child as q and give it our resources; after vfork() we
 * call vpasspt with the child as p to steal our resources back.
 * We are cognizant of whether we are p or q because we have to
 * be careful to keep our u. area and restore the other u. area from
 * umap after we temporarily put our u. area in both p and q's page tables.
 */
vpasspt(p, q, up, uq, umap)
	register struct proc *p, *q;
	register struct user *up, *uq;
	struct pte *umap;
{
	int t;
	int s;
	struct pte *tp;
	register int i;

	s = splhigh();	/* conservative, and slightly paranoid */
	Xu(pcb_szpt); Xu(pcb_p0lr); Xu(pcb_p1lr);
#if defined(tahoe)
	Xu(pcb_p2lr);
#endif
	Xup(pcb_p0br); Xup(pcb_p1br);
#if defined(tahoe)
	Xup(pcb_p2br);
#endif

	/*
	 * The u. area is contained in the process' p1 region.
	 * Thus we map the current u. area into the process virtual space
	 * of both sets of page tables we will deal with so that it
	 * will stay with us as we rearrange memory management.
	 */
	for (i = 0; i < UPAGES; i++)
		if (up == &u)
			q->p_addr[i] = p->p_addr[i];
		else
			p->p_addr[i] = q->p_addr[i];
#if defined(vax) || defined(tahoe)
	mtpr(TBIA, 0);
#endif
	/*
	 * Now have u. double mapped, and have flushed
	 * any stale translations to new u. area.
	 * Switch the page tables.
	 */
	Xpp(p_p0br); Xp(p_szpt); Xpp(p_addr);
#if defined(vax) || defined(tahoe)
	mtpr(P0BR, u.u_pcb.pcb_p0br);
#if defined(vax)
	mtpr(P0LR, u.u_pcb.pcb_p0lr &~ AST_CLR);
#else
	mtpr(P0LR, u.u_pcb.pcb_p0lr);
#endif
	mtpr(P1BR, u.u_pcb.pcb_p1br);
	mtpr(P1LR, u.u_pcb.pcb_p1lr);
#if defined(tahoe)
	mtpr(P2BR, u.u_pcb.pcb_p2br);
	mtpr(P2LR, u.u_pcb.pcb_p2lr);
#endif
#endif
	/*
	 * Now running on the ``other'' set of page tables.
	 * Flush translation to insure that we get correct u.
	 * Resurrect the u. for the other process in the other
	 * (our old) set of page tables.  Thus the other u. has moved
	 * from its old (our current) set of page tables to our old
	 * (its current) set of page tables, while we have kept our
	 * u. by mapping it into the other page table and then keeping
	 * the other page table.
	 */
#if defined(vax) || defined(tahoe)
	mtpr(TBIA, 0);
#endif
	for (i = 0; i < UPAGES; i++) {
		int pf;
		struct pte *pte;
		if (up == &u) {
			pf = umap[i].pg_pfnum;
			pte = &q->p_addr[i];
			pte->pg_pfnum = pf;
		} else {
			pf = umap[i].pg_pfnum;
			pte = &p->p_addr[i];
			pte->pg_pfnum = pf;
		}
	}
#if defined(vax) || defined(tahoe)
	mtpr(TBIA, 0);
#endif
	splx(s);
}

/*
 * Compute number of pages to be allocated to the u. area
 * and data and stack area page tables, which are stored on the
 * disk immediately after the u. area.
 */
/*ARGSUSED*/
vusize(p, utl)
	register struct proc *p;
	struct user *utl;
{
	register int tsz = p->p_tsize / NPTEPG;

	/*
	 * We do not need page table space on the disk for page
	 * table pages wholly containing text.  This is well
	 * understood in the code in vmswap.c.
	 */
	return (clrnd(UPAGES +
	    clrnd(ctopt(p->p_tsize+p->p_dsize+p->p_ssize+UPAGES)) - tsz));
}

/*
 * Get u area for process p.  If a old u area is given,
 * then copy the new area from the old, else
 * swap in as specified in the proc structure.
 *
 * Since argument map/newu is potentially shared
 * when an old u. is provided we have to be careful not
 * to block after beginning to use them in this case.
 * (This is not true when called from swapin() with no old u.)
 */
vgetu(p, palloc, map, newu, oldu)
	register struct proc *p;
	int (*palloc)();
	register struct pte *map;
	register struct user *newu;
	struct user *oldu;
{
	register int i;

	if ((*palloc)(p->p_addr, clrnd(UPAGES), p, CSYS) == 0)
		return (0);
	/*
	 * New u. pages are to be accessible in map/newu as well
	 * as in process p's virtual memory.
	 */
	for (i = 0; i < UPAGES; i++) {
		map[i] = p->p_addr[i];
		*(int *)(p->p_addr + i) |= PG_URKW | PG_V;
	}
	setredzone(p->p_addr, (caddr_t)0);
	vmaccess(map, (caddr_t)newu, UPAGES);
	/*
	 * New u.'s come from forking or inswap.
	 */
	if (oldu) {
		bcopy((caddr_t)oldu, (caddr_t)newu, (unsigned) UPAGES * NBPG);
		newu->u_procp = p;
	} else {
#if defined(vax)
		(void) swap(p, p->p_swaddr, (caddr_t)0, ctob(UPAGES),
#else
		(void) swap(p, p->p_swaddr, (caddr_t)&u, ctob(UPAGES),
#endif
		   B_READ, B_UAREA, swapdev, 0);
		if (
#if defined(vax)
		    newu->u_pcb.pcb_ssp != -1 || newu->u_pcb.pcb_esp != -1 ||
#endif
		    newu->u_tsize != p->p_tsize || newu->u_dsize != p->p_dsize ||
		    newu->u_ssize != p->p_ssize || newu->u_procp != p)
			panic("vgetu");
	}
	/*
	 * Initialize the pcb copies of the p0 and p1 region bases and
	 * software page table size from the information in the proc structure.
	 */
	newu->u_pcb.pcb_p0br = p->p_p0br;
#if defined(vax)
	newu->u_pcb.pcb_p1br = initp1br(p->p_p0br + p->p_szpt * NPTEPG);
#endif
#if defined(tahoe)
	newu->u_pcb.pcb_p2br = initp2br(p->p_p0br + p->p_szpt * NPTEPG);
#endif
	newu->u_pcb.pcb_szpt = p->p_szpt;
	return (1);
}

/*
 * Release swap space for a u. area.
 */
vrelswu(p, utl)
	struct proc *p;
	struct user *utl;
{

	rmfree(swapmap, (long)ctod(vusize(p, utl)), p->p_swaddr);
	/* p->p_swaddr = 0; */	/* leave for post-mortems */
}

/*
 * Get swap space for a u. area.
 */
vgetswu(p, utl)
	struct proc *p;
	struct user *utl;
{

	p->p_swaddr = rmalloc(swapmap, (long)ctod(vusize(p, utl)));
	return (p->p_swaddr);
}

/*
 * Release u. area, swapping it out if desired.
 */
vrelu(p, swapu)
	register struct proc *p;
{
	register int i;
	struct pte uu[UPAGES];

	if (swapu)
#if defined(vax)
		(void) swap(p, p->p_swaddr, (caddr_t)0, ctob(UPAGES),
#else
		(void) swap(p, p->p_swaddr, (caddr_t)&u, ctob(UPAGES),
#endif
		    B_WRITE, B_UAREA, swapdev, 0);
	for (i = 0; i < UPAGES; i++)
		uu[i] = p->p_addr[i];
	/*
	 * If freeing the user structure and kernel stack
	 * for the current process, have to run a bit longer
	 * using the pages which have already been freed...
	 * block memory allocation from the network by raising ipl.
	 */
	if (p == u.u_procp)
		(void) splimp();		/* XXX */
	(void) vmemfree(uu, clrnd(UPAGES));
}

#ifdef unneeded
int	ptforceswap;
#endif
/*
 * Expand a page table, assigning new kernel virtual
 * space and copying the page table entries over both
 * in the system map and as necessary in the user page table space.
 */
ptexpand(change, ods, oss)
	register int change;
	size_t ods, oss;
{
	register struct pte *p1, *p2;
	register int i;
#if defined(vax)
	register int spages, ss = P1PAGES - u.u_pcb.pcb_p1lr;
#endif
#if defined(tahoe)
	register int spages, ss = P2PAGES - u.u_pcb.pcb_p2lr;
#endif
	register int kold = btokmx(u.u_pcb.pcb_p0br);
	int knew, tdpages;
	int szpt = u.u_pcb.pcb_szpt;
	caddr_t a;
	int s;

	if (change <= 0 || change % CLSIZE)
		panic("ptexpand");
	/*
	 * Change is the number of new page table pages needed.
	 * Kold is the old index in the kernelmap of the page tables.
	 * Allocate a new kernel map segment of size szpt+change for
	 * the page tables, and the new page table pages in the
	 * middle of this new region.
	 */
top:
#ifdef unneeded
	if (ptforceswap)
		goto bad;
#endif
	if ((knew=rmalloc(kernelmap, (long)(szpt+change))) == 0)
		goto bad;
	spages = ss/NPTEPG;
	tdpages = szpt - spages;
	if (memall(&Usrptmap[knew+tdpages], change, u.u_procp, CSYS) == 0) {
		rmfree(kernelmap, (long)(szpt+change), (long)knew);
		goto bad;
	}

	/*
	 * Spages pages of u.+stack page tables go over unchanged.
	 * Tdpages of text+data page table may contain a few stack
	 * pages which need to go in one of the newly allocated pages;
	 * this is a rough cut.
	 */
	kmcopy(knew, kold, tdpages);
	kmcopy(knew+tdpages+change, kold+tdpages, spages);

	/*
	 * Validate and clear the newly allocated page table pages in the
	 * center of the new region of the kernelmap.
	 */
	i = knew + tdpages;
	p1 = &Usrptmap[i];
	a = (caddr_t) kmxtob(i);
	p2 = p1 + change;
	while (p1 < p2) {
		mapin(p1, (u_int) a, p1->pg_pfnum, (int)(PG_V|PG_KW));
		bzero(a, NBPG);
		a += NBPG;
		p1++;
	}

	/*
	 * Move the stack and u. pte's which are before the newly
	 * allocated pages into the last of the newly allocated pages.
	 * They are taken from the end of the current p1 region,
	 * and moved to the end of the new p1 region.
	 */
#if defined(vax)
	p1 = u.u_pcb.pcb_p1br + u.u_pcb.pcb_p1lr;
	p2 = initp1br(kmxtob(knew+szpt+change)) + u.u_pcb.pcb_p1lr;
#endif
#if defined(tahoe)
	p1 = u.u_pcb.pcb_p2br + u.u_pcb.pcb_p2lr;
	p2 = initp2br(kmxtob(knew+szpt+change)) + u.u_pcb.pcb_p2lr;
#endif
	bcopy((caddr_t) p1, (caddr_t) p2,
	    (unsigned)((caddr_t)kmxtob(kold + tdpages) - (caddr_t)p1));

	/*
	 * Now switch to the new page tables.
	 */
	s = splhigh();	/* conservative */
	u.u_procp->p_p0br = kmxtob(knew);
	setp0br(u.u_procp->p_p0br);
#if defined(vax)
	u.u_pcb.pcb_p1br = initp1br(kmxtob(knew+szpt+change));
	setp1br(u.u_pcb.pcb_p1br);
#endif
#if defined(tahoe)
	u.u_pcb.pcb_p1br = kmxtob(knew);
	setp1br(u.u_pcb.pcb_p1br);
	u.u_pcb.pcb_p2br = initp2br(kmxtob(knew+szpt+change));
	setp2br(u.u_pcb.pcb_p2br);
#endif
	u.u_pcb.pcb_szpt += change;
	u.u_procp->p_szpt += change;
	u.u_procp->p_addr = uaddr(u.u_procp);
#if defined(vax) || defined(tahoe)
	mtpr(TBIA, 0);
#endif
	splx(s);

	/*
	 * Finally, free old kernelmap.
	 */
	if (szpt)
		rmfree(kernelmap, (long)szpt, (long)kold);
	return;

bad:
	/*
	 * Swap out the process so that the unavailable 
	 * resource will be allocated upon swapin.
	 *
	 * When resume is executed for the process, 
	 * here is where it will resume.
	 */
	resume(pcbb(u.u_procp));
	if (savectx(&u.u_ssave))
		return;
	if (swapout(u.u_procp, ods, oss) == 0) {
		/*
		 * No space to swap... it is inconvenient to try
		 * to exit, so just wait a bit and hope something
		 * turns up.  Could deadlock here.
		 *
		 * SOMEDAY REFLECT ERROR BACK THROUGH expand TO CALLERS
		 * (grow, sbreak) SO CAN'T DEADLOCK HERE.
		 */
		sleep((caddr_t)&lbolt, PRIBIO);
		goto top;
	}
	/*
	 * Set SSWAP bit, so that when process is swapped back in
	 * swapin will set u.u_pcb.pcb_sswap to u_sswap and force a
	 * return from the savectx() above.
	 */
	u.u_procp->p_flag |= SSWAP;
	swtch();
	/* NOTREACHED */
}

kmcopy(to, from, count)
	int to;
	int from;
	register int count;
{
	register struct pte *tp = &Usrptmap[to];
	register struct pte *fp = &Usrptmap[from];
	u_int a;

	a = (u_int) kmxtob(to);
	while (count != 0) {
		mapin(tp, a, fp->pg_pfnum,
		    (int)(*((int *)fp) & (PG_V|PG_PROT)));
#if defined(tahoe)
		mtpr(P1DC, a);
#endif
		tp++;
		fp++;
		a += NBPG;
		count--;
	}
}
