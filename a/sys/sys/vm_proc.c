/*	vm_proc.c	4.13	82/12/17	*/

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/map.h"
#include "../h/cmap.h"
#include "../h/text.h"
#include "../h/vm.h"

#ifdef vax
#include "../vax/mtpr.h"
#endif

/*
 * Get virtual memory resources for a new process.
 * Called after page tables are allocated, but before they
 * are initialized, we initialize the memory management registers,
 * and then expand the page tables for the data and stack segments
 * creating zero fill pte's there.  Text pte's are set up elsewhere.
 *
 * SHOULD FREE EXTRA PAGE TABLE PAGES HERE OR SOMEWHERE.
 */
vgetvm(ts, ds, ss)
	size_t ts, ds, ss;
{

	u.u_pcb.pcb_p0lr = AST_NONE;
	setp0lr(ts);
	setp1lr(P1PAGES - HIGHPAGES);
	u.u_procp->p_tsize = ts;
	u.u_tsize = ts;
	expand((int)ss, 1);
	expand((int)ds, 0);
}

/*
 * Release the virtual memory resources (memory
 * pages, and swap area) associated with the current process.
 * Caller must not be swappable.  Used at exit or execl.
 */
vrelvm()
{
	register struct proc *p = u.u_procp;
	
	/*
	 * Release memory; text first, then data and stack pages.
	 */
	xfree();
	p->p_rssize -= vmemfree(dptopte(p, 0), p->p_dsize);
	p->p_rssize -= vmemfree(sptopte(p, p->p_ssize - 1), p->p_ssize);
	if (p->p_rssize != 0)
		panic("vrelvm rss");
	/*
	 * Wait for all page outs to complete, then
	 * release swap space.
	 */
	p->p_swrss = 0;
	while (p->p_poip)
		sleep((caddr_t)&p->p_poip, PSWP+1);
	(void) vsexpand((size_t)0, &u.u_dmap, 1);
	(void) vsexpand((size_t)0, &u.u_smap, 1);
	p->p_tsize = 0;
	p->p_dsize = 0;
	p->p_ssize = 0;
	u.u_tsize = 0;
	u.u_dsize = 0;
	u.u_ssize = 0;
}

/*
 * Pass virtual memory resources from p to q.
 * P's u. area is up, q's is uq.  Used internally
 * when starting/ending a vfork().
 */
vpassvm(p, q, up, uq, umap)
	register struct proc *p, *q;
	register struct user *up, *uq;
	struct pte *umap;
{

	/*
	 * Pass fields related to vm sizes.
	 */
	uq->u_tsize = q->p_tsize = p->p_tsize; up->u_tsize = p->p_tsize = 0;
	uq->u_dsize = q->p_dsize = p->p_dsize; up->u_dsize = p->p_dsize = 0;
	uq->u_ssize = q->p_ssize = p->p_ssize; up->u_ssize = p->p_ssize = 0;

	/*
	 * Pass proc table paging statistics.
	 */
	q->p_swrss = p->p_swrss; p->p_swrss = 0;
	q->p_rssize = p->p_rssize; p->p_rssize = 0;
	q->p_poip = p->p_poip; p->p_poip = 0;

	/*
	 * Relink text segment.
	 */
	q->p_textp = p->p_textp;
	xrepl(p, q);
	p->p_textp = 0;

	/*
	 * Pass swap space maps.
	 */
	uq->u_dmap = up->u_dmap; up->u_dmap = zdmap;
	uq->u_smap = up->u_smap; up->u_smap = zdmap;

	/*
	 * Pass u. paging statistics.
	 */
	uq->u_outime = up->u_outime; up->u_outime = 0;
	uq->u_ru = up->u_ru;
	bzero((caddr_t)&up->u_ru, sizeof (struct rusage));
	uq->u_cru = up->u_cru;
	bzero((caddr_t)&up->u_cru, sizeof (struct rusage));

	/*
	 * And finally, pass the page tables themselves.
	 * On return we are running on the other set of
	 * page tables, but still with the same u. area.
	 */
	vpasspt(p, q, up, uq, umap);
}

/*
 * Change the size of the data+stack regions of the process.
 * If the size is shrinking, it's easy-- just release virtual memory.
 * If it's growing, initalize new page table entries as 
 * 'zero fill on demand' pages.
 */
expand(change, region)
	int change, region;
{
	register struct proc *p;
	register struct pte *base, *p0, *p1;
	struct pte proto;
	int p0lr, p1lr;
	struct pte *base0;
	int size;
	u_int v;

	p = u.u_procp;
	if (change == 0)
		return;
	if (change % CLSIZE)
		panic("expand");

#ifdef PGINPROF
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
	if (region == 0) {
		v = dptov(p, p->p_dsize);
		p->p_dsize += change;
		u.u_dsize += change;
	} else {
		p->p_ssize += change;
		v = sptov(p, p->p_ssize-1);
		u.u_ssize += change;
	}

	/*
	 * Compute the end of the text+data regions and the beginning
	 * of the stack region in the page tables,
	 * and expand the page tables if necessary.
	 */
	p0 = u.u_pcb.pcb_p0br + (u.u_pcb.pcb_p0lr&~AST_CLR);
	p1 = u.u_pcb.pcb_p1br + (u.u_pcb.pcb_p1lr&~PME_CLR);
	if (change > p1 - p0)
		ptexpand(clrnd(ctopt(change - (p1 - p0))));
	/* PTEXPAND SHOULD GIVE BACK EXCESS PAGE TABLE PAGES */

	/*
	 * Compute the base of the allocated/freed region.
	 */
	p0lr = u.u_pcb.pcb_p0lr&~AST_CLR;
	p1lr = u.u_pcb.pcb_p1lr&~PME_CLR;
	if (region == 0)
		base = u.u_pcb.pcb_p0br + p0lr + (change > 0 ? 0 : change);
	else
		base = u.u_pcb.pcb_p1br + p1lr - (change > 0 ? change : 0);

	/*
	 * If we shrunk, give back the virtual memory.
	 */
	if (change < 0)
		p->p_rssize -= vmemfree(base, -change);

	/*
	 * Update the processor length registers and copies in the pcb.
	 */
	if (region == 0)
		setp0lr(p0lr + change);
	else
		setp1lr(p1lr - change);

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
	base0 = base;
	size = change;
	while (--change >= 0)
		*base++ = proto;

	/*
	 * We changed mapping for the current process,
	 * so must update the hardware translation
	 */
	newptes(base0, v, size);
}

/*
 * Create a duplicate copy of the current process
 * in process slot p, which has been partially initialized
 * by newproc().
 *
 * Could deadlock here if two large proc's get page tables
 * and then each gets part of his UPAGES if they then have
 * consumed all the available memory.  This can only happen when
 *	USRPTSIZE + UPAGES * NPROC > maxmem
 * which is impossible except on systems with tiny real memories,
 * when large procs stupidly fork() instead of vfork().
 */
procdup(p, isvfork)
	register struct proc *p;
{

	/*
	 * Allocate page tables for new process, waiting
	 * for memory to be free.
	 */
	while (vgetpt(p, vmemall) == 0) {
		kmapwnt++;
		sleep((caddr_t)kernelmap, PSWP+4);
	}
	/*
	 * Snapshot the current u. area pcb and get a u.
	 * for the new process, a copy of our u.
	 */
	resume(pcbb(u.u_procp));
	(void) vgetu(p, vmemall, Forkmap, &forkutl, &u);

	/*
	 * Arrange for a non-local goto when the new process
	 * is started, to resume here, returning nonzero from setjmp.
	 */
	forkutl.u_pcb.pcb_sswap = (int *)&u.u_ssave;
	if (setjmp(&forkutl.u_ssave))
		/*
		 * Return 1 in child.
		 */
		return (1);

	/*
	 * If the new process is being created in vfork(), then
	 * exchange vm resources with it.  We want to end up with
	 * just a u. area and an empty p0 region, so initialize the
	 * prototypes in the other area before the exchange.
	 */
	if (isvfork) {
		forkutl.u_pcb.pcb_p0lr = u.u_pcb.pcb_p0lr & AST_CLR;
		forkutl.u_pcb.pcb_p1lr = P1PAGES - HIGHPAGES;
#ifdef sun
		forkutl.u_pcb.pcb_ctx = 0;
#endif
		vpassvm(u.u_procp, p, &u, &forkutl, Forkmap);
		/*
		 * Return 0 in parent.
		 */
		return (0);
	}
	/*
	 * A real fork; clear vm statistics of new process
	 * and link into the new text segment.
	 * Equivalent things happen during vfork() in vpassvm().
	 */
	bzero((caddr_t)&forkutl.u_ru, sizeof (struct rusage));
	bzero((caddr_t)&forkutl.u_cru, sizeof (struct rusage));
	forkutl.u_dmap = u.u_cdmap;
	forkutl.u_smap = u.u_csmap;
	forkutl.u_outime = 0;
#ifdef sun
	forkutl.u_pcb.pcb_ctx = 0;
#endif

	/*
	 * Attach to the text segment.
	 */
	if (p->p_textp) {
		p->p_textp->x_count++;
		xlink(p);
	}

	/*
	 * Duplicate data and stack space of current process
	 * in the new process.
	 */
	vmdup(p, dptopte(p, 0), dptov(p, 0), p->p_dsize, CDATA);
	vmdup(p, sptopte(p, p->p_ssize - 1), sptov(p, p->p_ssize - 1), p->p_ssize, CSTACK);

	/*
	 * Return 0 in parent.
	 */
	return (0);
}

vmdup(p, pte, v, count, type)
	struct proc *p;
	register struct pte *pte;
	register unsigned v;
	register size_t count;
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
		(void) vmemall(pte, CLSIZE, p, type);
		p->p_rssize += CLSIZE;
		for (i = 0; i < CLSIZE; i++) {
			copyseg((caddr_t)ctob(v+i), (pte+i)->pg_pfnum);
			*(int *)(pte+i) |= (PG_V|PG_M) + PG_UW;
		}
		v += CLSIZE;
		munlock(pte->pg_pfnum);
		pte += CLSIZE;
	}
	p->p_flag |= SPTECHG;
}
