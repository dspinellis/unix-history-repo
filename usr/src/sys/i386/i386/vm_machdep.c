/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the University of Utah, and William Jolitz.
 *
 * %sccs.include.386.c%
 *
 *	@(#)vm_machdep.c	5.5 (Berkeley) %G%
 */

/*
 * Copyright (c) 1989, 1990 William F. Jolitz
 */

/*
 * Copyright (c) 1988 University of Utah.
 * All rights reserved.  The Utah Software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	Utah $Hdr: vm_machdep.c 1.16.1.1 89/06/23$
 */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vm_machdep.c	7.1 (Berkeley) 6/5/86
 */

#include "pte.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "cmap.h"
#include "mount.h"
#include "vm.h"
#include "text.h"

#include "buf.h"

/*
 * Set a red zone in the kernel stack after the u. area.
 */
setredzone(pte, vaddr)
	u_short *pte;
	caddr_t vaddr;
{
/* eventually do this by setting up an expand-down stack segment
   for ss0: selector, allowing stack access down to top of u.
   this means though that protection violations need to be handled
   thru a double fault exception that must do an integral task
   switch to a known good context, within which a dump can be
   taken. a sensible scheme might be to save the initial context
   used by sched (that has physical memory mapped 1:1 at bottom)
   and take the dump while still in mapped mode */
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
		u.u_error = ENOMEM;
		return (1);
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
	load_cr3(u.u_pcb.pcb_ptd);
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
	if (!isatsv(u.u_procp, v)) {
		u.u_error = EFAULT;
		return (0);
	}
	tp = vtotp(u.u_procp, v);
	pte = tptopte(u.u_procp, tp);
	if (pte->pg_fod == 0 && pte->pg_pfnum) {
		c = &cmap[pgtocm(pte->pg_pfnum)];
		if (c->c_blkno && c->c_mdev != MSWAPX)
			munhash(mount[c->c_mdev].m_dev,
			    (daddr_t)(u_long)c->c_blkno);
	}
	*(u_int *)pte &= ~PG_PROT;
	*(u_int *)pte |= tprot;
	load_cr3(u.u_pcb.pcb_ptd);
	return (1);
}

settprot(tprot)
	long tprot;
{
	register u_int *ptaddr, i;

	ptaddr = (u_int *)u.u_procp->p_p0br;
	for (i = 0; i < u.u_tsize; i++) {
		ptaddr[i] &= ~PG_PROT;
		ptaddr[i] |= tprot;
	}
	load_cr3(u.u_pcb.pcb_ptd);
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
		*(u_int *)pte++ = 0;
	} while (--change);
	/* short cut newptes */
	load_cr3(u.u_pcb.pcb_ptd);
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
	load_cr3(u.u_pcb.pcb_ptd);
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
	fpte = &Sysmap[btop(from-0xfe000000)];
	tpte = &Sysmap[btop(to-0xfe000000)];
	while (size > 0) {
		*tpte++ = *fpte;
		*(int *)fpte++ = 0;
		from += NBPG;
		to += NBPG;
		size -= NBPG;
	}
	load_cr3(u.u_pcb.pcb_ptd);
}

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
	p = u.u_procp;
	page = btop(addr);
	if (page < dptov(p, p->p_dsize) || page > sptov(p, p->p_ssize))
		return(1);
	return(0);
}

probew(addr)
	register u_int addr;
{
	register int page;
	register struct proc *p;

	if (addr >= USRSTACK)
		return(0);
	p = u.u_procp;
	page = btop(addr);
	if (page < dptov(p, p->p_dsize) || page > sptov(p, p->p_ssize))
		return((*(int *)vtopte(p, page) & PG_PROT) == PG_UW);
	return(0);
}

/*
 * NB: assumes a physically contiguous kernel page table
 *     (makes life a LOT simpler).
 */
kernacc(addr, count, rw)
	register u_int addr;
	int count, rw;
{
	register struct pde *pde;
	register struct pte *pte;
	register int ix, cnt;
	extern long Syssize;

	if (count <= 0)
		return(0);
	pde = (struct pde *)((u_int)u.u_procp->p_p0br + u.u_procp->p_szpt * NBPG);
	ix = (addr & PD_MASK) >> PD_SHIFT;
	cnt = ((addr + count + (1 << PD_SHIFT) - 1) & PD_MASK) >> PD_SHIFT;
	cnt -= ix;
	for (pde += ix; cnt; cnt--, pde++)
		if (pde->pd_v == 0)
			return(0);
	ix = btop(addr-0xfe000000);
	cnt = btop(addr-0xfe000000+count+NBPG-1);
	if (cnt > (int)&Syssize)
		return(0);
	cnt -= ix;
	for (pte = &Sysmap[ix]; cnt; cnt--, pte++)
		if (pte->pg_v == 0 /*|| (rw == B_WRITE && pte->pg_prot == 1)*/) 
			return(0);
	return(1);
}

useracc(addr, count, rw)
	register u_int addr;
	int count, rw;
{
	register int (*func)();
	register u_int addr2;
	extern int prober(), probew();

	if (count <= 0)
		return(0);
	addr2 = addr;
	addr += count;
	func = (rw == B_READ) ? prober : probew;
	do {
		if ((*func)(addr2) == 0)
			return(0);
		addr2 = (addr2 + NBPG) & ~PGOFSET;
	} while (addr2 < addr);
	return(1);
}

/*
 * Convert kernel VA to physical address
 */
kvtop(addr)
	register u_int addr;
{
	register int pf;

	pf = Sysmap[btop(addr-0xfe000000)].pg_pfnum;
	if (pf == 0)
		panic("kvtop: zero page frame");
	return((u_int)ptob(pf) + (addr & PGOFSET));
}

struct pde *
vtopde(p, va)
	register struct proc *p;
	register u_int va;
{
	register struct pde *pde;

	pde = (struct pde *)((u_int)p->p_p0br + p->p_szpt * NBPG);
	return(pde + ((va & PD_MASK) >> PD_SHIFT));
}


initcr3(p)
	register struct proc *p;
{
	return(ctob(Usrptmap[btokmx(p->p_p0br+p->p_szpt*NPTEPG)].pg_pfnum));
	/*return((int)Usrptmap[btokmx(p->p_p0br) + p->p_szpt].pg_pfnum);*/
}

/*
 * Initialize page directory table to reflect PTEs in Usrptmap.
 * Page directory table address is given by Usrptmap index of p_szpt.
 * [used by vgetpt for kernal mode entries, and ptexpand for user mode entries]
 */
initpdt(p)
	register struct proc *p;
{
	register int i, k, sz;
	register struct pde *pde, *toppde;
	extern struct pde *vtopde();
	extern Sysbase;

	/* clear entire map */
	pde = vtopde(p, 0);
	/*bzero(pde, NBPG); */
	/* map kernel */
	pde = vtopde(p, &Sysbase);
	for (i = 0; i < 5; i++, pde++) {
		*(int *)pde = PG_UW | PG_V;
		pde->pd_pfnum = btoc((unsigned) Sysmap & ~0xfe000000)+i;
	}
	/* map u dot */
	pde = vtopde(p, &u);
	*(int *)pde = PG_UW | PG_V;
	pde->pd_pfnum = Usrptmap[btokmx(p->p_addr)].pg_pfnum;

	/* otherwise, fill in user map */
	k = btokmx(p->p_p0br);
	pde = vtopde(p, 0);
	toppde = vtopde(p, &u);

	/* text and data */
	sz = ctopt(p->p_tsize + p->p_dsize);
	for (i = 0; i < sz; i++, pde++) {
		*(int *)pde = PG_UW | PG_V;
		pde->pd_pfnum = Usrptmap[k++].pg_pfnum;
	}
	/*
	 * Bogus!  The kernelmap may map unused PT pages
	 * (since we don't shrink PTs) so we need to skip over
	 * those PDEs.  We should really free the unused PT
	 * pages in expand().
	 */
	sz += ctopt(p->p_ssize+UPAGES);
	if (sz < p->p_szpt)
		k += p->p_szpt - sz;
	/* hole */
	sz = NPTEPG - ctopt(p->p_ssize + UPAGES + btoc(&Sysbase));
	for ( ; i < sz; i++, pde++)
		*(int *)pde = 0;
	/* stack and u-area */
	sz = NPTEPG - ctopt(UPAGES + btoc(&Sysbase));
	for ( ; i < sz; i++, pde++) {
		*(int *)pde = PG_UW | PG_V;
		pde->pd_pfnum = Usrptmap[k++].pg_pfnum;
	}
	return(initcr3(p));
}

#ifdef notdef
/*
 * Allocate wired-down, non-paged, cache-inhibited pages in kernel
 * virtual memory and clear them
 */
caddr_t
cimemall(n)
	int n;
{
	register int npg, a;
	register struct pte *pte;
	extern struct map *kernelmap;

	npg = clrnd(btoc(n));
	a = rmalloc(kernelmap, (long)npg);
	if (a == 0)
		return ((caddr_t)0);
	pte = &Usrptmap[a];
	(void) vmemall(pte, npg, &proc[0], CSYS);
	while (--npg >= 0) {
		*(int *)pte |= (PG_V|PG_KW|PG_CI);
		clearseg((unsigned)pte->pg_pfnum);
		pte++;
	}
	TBIAS();
	return ((caddr_t)kmxtob(a));
}
#endif

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
	addr = bp->b_un.b_addr = (caddr_t)(usrio + (a << PGSHIFT)) + off;
	a = btop(addr);
	while (npf--) {
		mapin(iopte, a, pte->pg_pfnum, PG_V);
		iopte++, pte++;
		a++;
	}
	load_cr3(u.u_pcb.pcb_ptd);
}

/*
 * Free the io map PTEs associated with this IO operation.
 * We also invalidate the TLB entries.
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
		*(int *)pte = 0;
		addr += NBPG;
		pte++;
	}
	/*
	 * If we just completed a dirty page push, we must reconstruct
	 * the original b_addr since cleanup() needs it.
	 */
	if (bp->b_flags & B_DIRTY) {
		a = ((bp - swbuf) * CLSIZE) * KLMAX;
		bp->b_un.b_addr = (caddr_t)ctob(dptov(&proc[2], a));
	}
	load_cr3(u.u_pcb.pcb_ptd);
}
