/*	vmproc.c	2.2	1/21/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/mtpr.h"
#include "../h/pte.h"
#include "../h/map.h"
#include "../h/cmap.h"
#include "../h/text.h"
#include "../h/vm.h"

/*
 * Begin to get virtual memory resources for a new process.
 * Called by exec with the new text data and stack sizes.
 *
 * NOTE: should free extra page table pages here or somewhere.
 */
vgetvm(ts, ds, ss)
	int ts, ds, ss;
{
#ifdef UNTESTED
	register int szpt = clrnd(ctopt(ts+ds+ss));
	register int freeptp = u.u_procp.p_szpt - szpt;
	register int i;
	int a;

	a = btokmx(u.u_pcb.pcb_p0br);
	if (freeptp > 0) {
		memfree(&Usrptmap[a + szpt], freeptp, 1);
		mfree(kernelmap, freeptp, a + szpt);
		u.u_pcb.pcb_szpt = szpt;
		u.u_procp->p_szpt = szpt;
		u.u_pcb.pcb_p1br = kmxtob(a + szpt);
		mtpr(P1BR, u.u_pcb.pcb_p1br);
		mtpr(TBIA, 1);
	}
	for (i = 0; i < u.u_pcb.pcb_szpt; i++)
		clearseg(Usrptmap[a + i].pg_pfnum);
#endif
	mtpr(P0LR, ts);
	u.u_pcb.pcb_p0lr = ts | AST;
	mtpr(P1LR, P1TOP);
	u.u_pcb.pcb_p1lr = P1TOP;
	u.u_procp->p_tsize = ts;
	u.u_tsize = ts;
	expand((int)ss, P1BR);
	expand((int)ds, P0BR);
}

/*
 * Release the virtual memory resources (memory
 * pages, and swap area) associated with the current process.
 * Used at exit or execl.
 */
vrelvm()
{
	register struct proc *p = u.u_procp;
	register int n;
	
	xfree();
	p->p_rssize -= vmemfree(dptopte(p, 0), p->p_dsize);
	p->p_rssize -= vmemfree(sptopte(p, p->p_ssize - 1), p->p_ssize);
	if (p->p_rssize != 0)
		panic("vrelvm rss");
	p->p_swrss = 0;
	while (p->p_poip)
		sleep((caddr_t)&p->p_poip, PSWP+1);
	VOID vsexpand(0, &u.u_dmap);
	VOID vsexpand(0, &u.u_smap);
	p->p_tsize = 0;
	p->p_dsize = 0;
	p->p_ssize = 0;
	u.u_tsize = 0;
	u.u_dsize = 0;
	u.u_ssize = 0;
	for (n = 0; n < NOFILE; n++)
		if (u.u_vrpages[n])
			panic("vrelvm vrpages");
}

/*
 * Pass virtual memory resources from p to q.
 * P's u. area is up, q's is uq.  Used internally
 * when starting/ending a vfork().
 */
vpassvm(p, q, up, uq)
	register struct proc *p, *q;
	register struct user *up, *uq;
{

	vpasspt(p, q, up, uq);

	uq->u_tsize = q->p_tsize = p->p_tsize; up->u_tsize = p->p_tsize = 0;
	uq->u_dsize = q->p_dsize = p->p_dsize; up->u_dsize = p->p_dsize = 0;
	uq->u_ssize = q->p_ssize = p->p_ssize; up->u_ssize = p->p_ssize = 0;

	q->p_swrss = p->p_swrss; p->p_swrss = 0;
	q->p_rssize = p->p_rssize; p->p_rssize = 0;
	q->p_poip = p->p_poip; p->p_poip = 0;

	q->p_textp = p->p_textp;
	xrepl(p, q);
	p->p_textp = 0;

	uq->u_dmap = up->u_dmap; up->u_dmap = zdmap;
	uq->u_smap = up->u_smap; up->u_smap = zdmap;
	uq->u_outime = up->u_outime; up->u_outime = 0;
	uq->u_majorflt = up->u_majorflt; up->u_majorflt = 0;
	uq->u_minorflt = up->u_minorflt; up->u_minorflt = 0;
}

/*
 * Change the size of the data+stack regions of the process.
 * If the size is shrinking, it's easy-- just release virtual memory.
 * If it's growing, initalize new page table entries as 
 * 'zero fill on demand' pages.
 */
expand(change, region)
{
	register struct proc *p;
	register struct pte *base, *p0, *p1;
	struct pte proto;
	int a0, a1;

	p = u.u_procp;
	if (change == 0)
		return;
	if (change % CLSIZE)
		panic("expand");


#ifdef ERNIE
	vmsizmon();
#endif

	/*
	 * Update the sizes to reflect the change.  Note that we may
	 * swap as a result of a ptexpand, but this will work, because
	 * the routines which swap out will get the current text and data
	 * sizes from the arguments they are passed, and when the process
	 * resumes the lengths in the proc structure are used to 
	 * build the new page tables.
	 */
	if (region == P0BR) {
		p->p_dsize += change;
		u.u_dsize += change;
	} else {
		p->p_ssize += change;
		u.u_ssize += change;
	}

	/*
	 * Compute the end of the text+data regions and the beginning
	 * of the stack region in the page tables,
	 * and expand the page tables if necessary.
	 */
	p0 = (struct pte *)mfpr(P0BR) + mfpr(P0LR);
	p1 = (struct pte *)mfpr(P1BR) + mfpr(P1LR);
	if (change > p1 - p0)
		ptexpand(clrnd(ctopt(change - (p1 - p0))));
	/* PTEXPAND SHOULD GIVE BACK EXCESS PAGE TABLE PAGES */

	/*
	 * Compute the base of the allocated/freed region.
	 */
	if (region == P0BR) {
		base = (struct pte *)mfpr(P0BR);
		base += (a0 = mfpr(P0LR)) + (change > 0 ? 0 : change);
	} else {
		base = (struct pte *)mfpr(P1BR);
		base += (a1 = mfpr(P1LR)) - (change > 0 ? change : 0);
	}

	/*
	 * If we shrunk, give back the virtual memory.
	 * This code should happen earlier in this routine.
	 */
	if (change < 0)
		p->p_rssize -= vmemfree(base, -change);

	/*
	 * Update the processor length registers and words in the pcb.
	 */
	if (region == P0BR)  {
		mtpr(P0LR,a0 + change);
		u.u_pcb.pcb_p0lr = a0 + change | AST;
	} else {
		mtpr(P1LR,a1 - change);
		u.u_pcb.pcb_p1lr = a1 - change;
	}

	/*
	 * If shrinking, clear pte's, otherwise
	 * initialize zero fill on demand pte's.
	 */
	*(int *)&proto = PG_UW;
	if (change < 0)
		change = -change;
	else {
		proto.pg_fod = 1;
		((struct fpte *)&proto)->pg_fileno = PG_FZERO;
		cnt.v_nzfod += change;
	}
	while (--change >= 0)
		*base++ = proto;

	mtpr(TBIA,0);			/* conservative */
}

/*
 * Create a duplicate copy of the current process
 * in process slot p, which has been partially initialized
 * by newproc().
 */
procdup(p, isvfork)
	register struct proc *p;
{
	register int n;

	/*
	 * There is an extremely unlikely possible deadlock here.
	 * It can only happen if two very large processes grab
	 * their page tables and then each gets part of his UPAGES
	 * and then they have consumed all the available memory.
	 * This can only happen when
	 *	USRPTSIZE + UPAGES * NPROC > maxmem
	 * and this is almost never true, except on systems
	 * with incredibly tiny real memories.  In practice
	 * such large processes will not fork() but vfork() where
	 * there is no problem.
	 */
	if (isvfork == 0)
		while (vgetpt(p, vmemall) == 0) {
			kmapwnt++;
			sleep((caddr_t)kernelmap, PSWP+4);
		}
	VOID vgetu(p, vmemall, Forkmap, &forkutl, &u);
	if (isvfork) {
		vpassvm(u.u_procp, p, &u, &forkutl);
		for (n = 0; n < NOFILE; n++)
			if (forkutl.u_vrpages[n] != 0)
				forkutl.u_vrpages[n]++;
		return;
	}
	forkutl.u_majorflt = 0;
	forkutl.u_minorflt = 0;
	forkutl.u_dmap = u.u_cdmap;
	forkutl.u_smap = u.u_csmap;
	forkutl.u_outime = 0;
	if (p->p_textp) {
		p->p_textp->x_count++;
		xlink(p);
	}

	vmdup(p, dptopte(p, 0), dptov(p, 0), p->p_dsize, MDATA);
	vmdup(p, sptopte(p, p->p_ssize - 1), sptov(p, p->p_ssize - 1), p->p_ssize, MSTACK);
}

vmdup(p, pte, v, count, type)
	struct proc *p;
	register struct pte *pte;
	register unsigned v;
	register int count;
	int type;
{
	register struct pte *opte = vtopte(u.u_procp, v);
	register int i;

	while (count != 0) {
		count -= CLSIZE;
		if (opte->pg_fod) {
			v += CLSIZE;
			for (i = 0; i < CLSIZE; i++)
				*(int *)pte++ = *(int *)opte++;
			continue;
		}
		opte += CLSIZE;
		VOID vmemall(pte, CLSIZE, p, type);
		p->p_rssize += CLSIZE;
		for (i = 0; i < CLSIZE; i++) {
			copyseg((caddr_t)ctob(v+i), (pte+i)->pg_pfnum);
			*(int *)(pte+i) |= (PG_V|PG_M) + PG_UW;
		}
		v += CLSIZE;
		munlock(pte->pg_pfnum);
		pte += CLSIZE;
	}
}

/*
 * Check that a process will not be too large.
 */
chksize(ts, ds, ss)
	size_t ts, ds, ss;
{

	if (ts>MAXTSIZ || ds>MAXDSIZ || ss>MAXSSIZ) {
		u.u_error = ENOMEM;
		return(1);
	}
	return(0);
}
