/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vm_swap.c	7.7 (Berkeley) 6/30/90
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "text.h"
#include "map.h"
#include "buf.h"
#include "cmap.h"
#include "vm.h"

#include "machine/cpu.h"
#include "machine/pte.h"
#include "machine/mtpr.h"

/*
 * Swap a process in.
 */
swapin(p)
	register struct proc *p;
{
	register struct text *xp;
	register int i, s;

	if (xp = p->p_textp) 
		xlock(xp);
	p->p_szpt = ptsize(p);
	if (vgetpt(p, memall) == 0)
		goto nomem;
	if (vgetu(p, memall, Swapmap, &swaputl, (struct user *)0) == 0) {
		vrelpt(p);
		goto nomem;
	}

#if defined(tahoe)
	for (i = 0; i < UPAGES; i++)
		mtpr(P1DC, (caddr_t)&u+i*NBPG);
#endif
	swdspt(p, &swaputl, B_READ);
	/*
	 * Make sure swdspt didn't smash u. pte's
	 */
	for (i = 0; i < UPAGES; i++) {
		if (Swapmap[i].pg_pfnum != p->p_addr[i].pg_pfnum)
			panic("swapin");
	}
	vrelswu(p, &swaputl);
	if (xp) {
		xlink(p);
		xunlock(xp);
	}

	p->p_rssize = 0;
	s = splclock();
	if (p->p_stat == SRUN)
		setrq(p);
	p->p_flag |= SLOAD;
	if (p->p_flag & SSWAP) {
		swaputl.u_pcb.pcb_sswap = (int *)&u.u_ssave;
		p->p_flag &= ~SSWAP;
	}
	splx(s);
	p->p_time = 0;
	multprog++;
	cnt.v_swpin++;
	return (1);

nomem:
	if (xp)
		xunlock(xp);
	return (0);
}

int	xswapwant, xswaplock;
/*
 * Swap out process p.
 * ds and ss are the old data size and the stack size
 * of the process, and are supplied during page table
 * expansion swaps.
 */
swapout(p, ds, mms, ss)
	register struct proc *p;
	segsz_t ds, mms, ss;
{
	register struct pte *map;
	register struct user *utl;
	int s;
	int rc = 1;

	s = 1;
	map = Xswapmap;
	utl = &xswaputl;
	if (xswaplock & s)
		if ((xswaplock & 2) == 0) {
			s = 2;
			map = Xswap2map;
			utl = &xswap2utl;
		}
	while (xswaplock & s) {
		xswapwant |= s;
		sleep((caddr_t)map, PSWP);
	}
	xswaplock |= s;
	uaccess(p, map, utl);
	if (vgetswu(p, utl) == 0) {
		p->p_flag |= SLOAD;
		rc = 0;
		goto out;
	}
#if defined(tahoe)
	{ int i;
	  for (i = 0; i < UPAGES; i++)
		mtpr(P1DC, (caddr_t)&u+i*NBPG);
	}
#endif
	utl->u_ru.ru_nswap++;
	utl->u_odsize = ds + mms;
	utl->u_ossize = ss;
	p->p_flag |= SLOCK;
	if (p->p_textp) {
		if (p->p_textp->x_ccount == 1)
			p->p_textp->x_swrss = p->p_textp->x_rssize;
		xdetach(p->p_textp, p);
	}
	p->p_swrss = p->p_rssize;
	vsswap(p, dptopte(p, 0), CDATA, 0, (int)ds, &utl->u_dmap);
	vsswap(p, sptopte(p, CLSIZE-1), CSTACK, 0, (int)ss, &utl->u_smap);
	if (p->p_rssize != 0)
		panic("swapout rssize");

	swdspt(p, utl, B_WRITE);
	/*
	 * If freeing the user structure and kernel stack
	 * for the current process, have to run a bit longer
	 * using the pages which are about to be freed...
	 * vrelu will then block memory allocation by raising ipl.
	 */
	vrelu(p, 1);
	if ((p->p_flag & SLOAD) && (p->p_stat != SRUN || p != u.u_procp))
		panic("swapout");
	p->p_flag &= ~SLOAD;
	vrelpt(p);
	p->p_flag &= ~SLOCK;
	p->p_time = 0;

	multprog--;
	cnt.v_swpout++;

	if (runout) {
		runout = 0;
		wakeup((caddr_t)&runout);
	}
out:
	xswaplock &= ~s;
	if (xswapwant & s) {
		xswapwant &= ~s;
		wakeup((caddr_t)map);
	}
	return (rc);
}

/*
 * Swap the data and stack page tables in or out.
 * Only hard thing is swapping out when new pt size is different than old.
 * If we are growing new pt pages, then we must spread pages with 2 swaps.
 * If we are shrinking pt pages, then we must merge stack pte's into last
 * data page so as not to lose them (and also do two swaps).
 */
swdspt(p, utl, rdwri)
	register struct proc *p;
	register struct user *utl;
{
	register int szpt, tsz, ssz;
	register struct pte *pte;
	register int i;

	szpt = ptsize(p);
	tsz = p->p_tsize / NPTEPG;
	if (szpt == p->p_szpt) {
		swptstat.pteasy++;
		swpt(rdwri, p, 0, tsz,
		    (p->p_szpt - tsz) * NBPG - HIGHPAGES * sizeof (struct pte));
		goto check;
	}
	if (szpt < p->p_szpt)
		swptstat.ptshrink++;
	else
		swptstat.ptexpand++;
	ssz = clrnd(ctopt(utl->u_ossize+HIGHPAGES));
#if !defined(hp300) && !defined(i386)
	if (szpt < p->p_szpt && utl->u_odsize && (utl->u_ossize+HIGHPAGES)) {
		int tdlast, slast, tdsz;

		/*
		 * Page tables shrinking... see if last text+data and
		 * last stack page must be merged... if so, copy
		 * stack pte's from last stack page to end of last
		 * data page, and decrease size of stack pt to be swapped.
		 */
		tdlast = (p->p_tsize + utl->u_odsize) % (NPTEPG * CLSIZE);
		slast = (utl->u_ossize + HIGHPAGES) % (NPTEPG * CLSIZE);
		if (tdlast && slast && tdlast + slast <= (NPTEPG * CLSIZE)) {
			swptstat.ptpack++;
			tdsz = clrnd(ctopt(p->p_tsize + utl->u_odsize));
			bcopy((caddr_t)sptopte(p, utl->u_ossize - 1),
			    (caddr_t)&p->p_p0br[tdsz * NPTEPG - slast],
			    (unsigned)(slast * sizeof (struct pte)));
			ssz -= CLSIZE;
		}
	}
#endif
	if (ssz)
		swpt(rdwri, p, szpt - ssz - tsz, p->p_szpt - ssz, ssz * NBPG);
	if (utl->u_odsize)
		swpt(rdwri, p, 0, tsz,
		  (int)(clrnd(ctopt(p->p_tsize + utl->u_odsize)) - tsz) * NBPG);
check:
	for (i = 0; i < utl->u_odsize; i++) {
		pte = dptopte(p, i);
#if defined(tahoe)
		uncache(pte);
#endif
#ifdef MAPMEM
		if (pte->pg_v && pte->pg_fod)		/* mapped page */
			continue;
#endif
		if (pte->pg_v || pte->pg_fod == 0 && (pte->pg_pfnum||pte->pg_m))
			panic("swdspt");
	}
	for (i = 0; i < utl->u_ossize; i++) {
		pte = sptopte(p, i);
#if defined(tahoe)
		uncache(pte);
#endif
#ifdef MAPMEM
		if (pte->pg_v && pte->pg_fod)		/* mapped page */
			continue;
#endif
		if (pte->pg_v || pte->pg_fod == 0 && (pte->pg_pfnum||pte->pg_m))
			panic("swdspt");
	}
}

/*
 * Swap a section of the page tables.
 * Errors are handled at a lower level (by doing a panic).
 */
swpt(rdwri, p, doff, a, n)
	int rdwri;
	struct proc *p;
	int doff, a, n;
{

	if (n <= 0)
		return;
	(void) swap(p, p->p_swaddr + ctod(UPAGES) + ctod(doff),
	    (caddr_t)&p->p_p0br[a * NPTEPG], n, rdwri, B_PAGET, swapdev_vp, 0);
}
