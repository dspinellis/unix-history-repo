/*	vmmem.c	2.2	1/25/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/pte.h"
#include "../h/cmap.h"
#include "../h/proc.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/mtpr.h"
#include "../h/text.h"
#include "../h/vm.h"
#include "../h/file.h"
#include "../h/inode.h"
#include "../h/buf.h"

/*
 * Allocate memory, and always succeed
 * by jolting page-out daemon
 * so as to obtain page frames.
 * To be used in conjunction with vmemfree().
 */
vmemall(pte, size, p, type)
	register struct pte *pte;
	int size;
	struct proc *p;
{
	register int m;

	if (size <= 0 || size > maxmem)
		panic("vmemall size");
	while (size > 0) {
		if (freemem < desfree)
			wakeup((caddr_t)&proc[2]);	/* jolt daemon */
		while (freemem == 0)
			sleep((caddr_t)&freemem, PSWP+2);
		m = imin(size, freemem);
		VOID memall(pte, m, p, type);
		size -= m;
		pte += m;
	}
	if (freemem < desfree)
		wakeup((caddr_t)&proc[2]);		/* jolt daemon */
	/*
	 * Always succeeds, but return success for
	 * vgetu and vgetpt (e.g.) which call either
	 * memall or vmemall depending on context.
	 */
	return (1);
}

/*
 * Free valid and reclaimable page frames belonging to the
 * count pages starting at pte.  If a page is valid
 * or reclaimable and locked (but not a system page), then
 * we simply mark the page as MGONE and let the pageout
 * daemon free the page when it is through with it.
 * If a page is reclaimable, and already in the free list, then
 * we mark the page as MGONE, and (of course) don't free it.
 *
 * Determines the largest contiguous cluster of
 * valid pages and frees them in one call to memfree.
 */
vmemfree(pte, count)
	register struct pte *pte;
	register int count;
{
	register struct cmap *c;
	register struct pte *spte;
/*
*/
	register int j;
	int size, pcnt, fileno;

	if (count % CLSIZE)
		panic("vmemfree");
	for (size = 0, pcnt = 0; count > 0; pte += CLSIZE, count -= CLSIZE) {
		if (pte->pg_fod == 0 && pte->pg_pfnum) {
			c = &cmap[pgtocm(pte->pg_pfnum)];
			pcnt += CLSIZE;
			if ((c->c_flag&MLOCK) && !(c->c_flag&MSYS)) {
				for (j = 0; j < CLSIZE; j++)
					*(int *)(pte+j) &= (PG_PROT|PG_VREADM);
				c->c_flag |= MGONE;
				goto free;
			}
			if (c->c_flag & MFREE) {
				pcnt -= CLSIZE;
				for (j = 0; j < CLSIZE; j++)
					*(int *)(pte+j) &= (PG_PROT|PG_VREADM);
				if (c->c_flag & MTEXT)
					distpte(&text[c->c_ndx], (int)c->c_page, pte);
				c->c_flag |= MGONE;
				goto free;
			}
			if (size == 0)
				spte = pte;
			size += CLSIZE;
			continue;
		}
		if (pte->pg_fod) {
			fileno = ((struct fpte *)pte)->pg_fileno;
			if (fileno < NOFILE)
				if ((u.u_vrpages[fileno] -= CLSIZE) <= 0) {
					if (u.u_vrpages[fileno] < 0)
						panic("vmemfree vrpages");
					if (--u.u_ofile[fileno]->f_inode->i_vfdcnt < 0)
						panic("vmemfree vfdcnt");
				}
			for (j = 0; j < CLSIZE; j++)
				*(int *)(pte+j) &= (PG_PROT|PG_VREADM);
		}
free:
		if (size) {
			memfree(spte, size, 1);
			size = 0;
		}
	}
	if (size)
		memfree(spte, size, 1);
	return (pcnt);
}

/*
 * Unlink a page frame from the free list -
 *
 * Performed if the page being reclaimed
 * is in the free list.
 */
munlink(pf)
	unsigned pf;
{
	register int next, prev;

	next = cmap[pgtocm(pf)].c_next;
	prev = cmap[pgtocm(pf)].c_prev;
	cmap[prev].c_next = next;
	cmap[next].c_prev = prev;
	cmap[pgtocm(pf)].c_flag &= ~MFREE;
	if (freemem < minfree)
		wakeup((caddr_t)&proc[2]);	/* jolt paging daemon */
	freemem -= CLSIZE;
}

/*
 * Allocate memory -
 *
 * The free list appears as a doubly linked list
 * in the core map with cmap[0] serving as a header.
 */
memall(pte, size, p, type)
	register struct pte *pte;
	int size;
	struct proc *p;
{
	register struct cmap *c;
	register struct pte *rpte;
	register struct proc *rp;
	int i, j, next, curpos;
	unsigned pf;

	if (size % CLSIZE)
		panic("memall");
	if (size > freemem)
		return (0);
	for (i = size; i > 0; i -= CLSIZE) {
		curpos = cmap[CMHEAD].c_next;
		c = &cmap[curpos];
		if ((c->c_flag & MFREE) == 0)
			panic("dup mem alloc");
		if (cmtopg(curpos) > maxfree)
			panic("bad mem alloc");
		if ((c->c_flag & (MGONE|MSYS)) == 0) {
			if (c->c_flag & MTEXT)
				rp = text[c->c_ndx].x_caddr;
			else
				rp = &proc[c->c_ndx];
			while (rp->p_flag & SNOVM)
				rp = rp->p_xlink;
			if (c->c_flag & MTEXT)
				rpte = tptopte(rp, c->c_page);
			else if (c->c_flag & MDATA)
				rpte = dptopte(rp, c->c_page);
			else
				rpte = sptopte(rp, c->c_page);
			zapcl(rpte, pg_pfnum) = 0;
			if (c->c_flag & MTEXT)
				distpte(&text[c->c_ndx], (int)c->c_page, rpte);
		}
		if (type != MSYS)
			if (type == MTEXT) {
				c->c_page = vtotp(p, ptetov(p, pte));
				c->c_ndx = p->p_textp - &text[0];
			} else {
				if (type == MDATA)
					c->c_page = vtodp(p, ptetov(p, pte));
				else
					c->c_page = vtosp(p, ptetov(p, pte));
				c->c_ndx = p->p_ndx;
			}
		pf = cmtopg(curpos);
		for (j = 0; j < CLSIZE; j++)
			*(int *)pte++ = pf++;
		c->c_flag = MLOCK | type;
		freemem -= CLSIZE;
		next = c->c_next;
		cmap[CMHEAD].c_next = next;
		cmap[next].c_prev = CMHEAD;
	}
	return (size);
}

/*
 * Free memory -
 *
 * The page frames being returned are inserted
 * to the head/tail of the free list depending
 * on whether there is any possible future use of them.
 *
 * If the freemem count had been zero,
 * the processes sleeping for memory
 * are awakened.
 */
memfree(pte, size, useless)
	register struct pte *pte;
	register int size;
{
	register int i, j, prev, next;
	
	if (size % CLSIZE)
		panic("memfree");
	while (size > 0) {
		size -= CLSIZE;
		i = pte->pg_pfnum;
		if (i < firstfree || i > maxfree)
			panic("bad mem free");
		i = pgtocm(i);
		if (cmap[i].c_flag & MFREE)
			panic("dup mem free");
		if (useless) {
			for (j = 0; j < CLSIZE; j++)
				*(int *)(pte+j) &= (PG_PROT|PG_VREADM);
			cmap[i].c_flag |= MGONE;
			next = cmap[CMHEAD].c_next;
			cmap[next].c_prev = i;
			cmap[i].c_prev = CMHEAD;
			cmap[i].c_next = next;
			cmap[CMHEAD].c_next = i;
		} else {
			prev = cmap[CMHEAD].c_prev;
			cmap[prev].c_next = i;
			cmap[i].c_next = CMHEAD;
			cmap[i].c_prev = prev;
			cmap[CMHEAD].c_prev = i;
		}
		cmap[i].c_flag |= MFREE;
		if (freemem == 0)
			wakeup((caddr_t)&freemem);
		freemem += CLSIZE;
		pte += CLSIZE;
	}
}

/*
 * Initialize core map
 */
meminit(first, last)
	int first, last;
{
	register int i;

	firstfree = clrnd(first);
	maxfree = clrnd(last - (CLSIZE - 1));
	freemem = maxfree - firstfree;
	if ((ecmap-cmap) < freemem / CLSIZE)
		freemem = (ecmap - cmap) * CLSIZE;
	for (i = 1; i <= freemem / CLSIZE; i++) {
		cmap[i-1].c_next = i;
		cmap[i].c_prev = i-1;
		cmap[i].c_flag = MFREE|MGONE;
	}
	cmap[freemem / CLSIZE].c_next = CMHEAD;
	cmap[CMHEAD].c_prev = freemem / CLSIZE;
	cmap[CMHEAD].c_flag = MSYS;
	avefree = freemem;
	hand = 0;
}

/*
 * Wait for frame pf to become unlocked
 * if it is currently locked.
 */
mwait(pf)
	unsigned pf;
{

	mlock(pf);
	munlock(pf);
}

/*
 * Lock a page frame.
 */
mlock(pf)
	unsigned pf;
{
	register struct cmap *c = &cmap[pgtocm(pf)];

	while (c->c_flag & MLOCK) {
		c->c_flag |= MWANT;
		sleep((caddr_t)c, PSWP+1);
	}
	c->c_flag |= MLOCK;
}

/*
 * Unlock a page frame.
 */
munlock(pf)
	unsigned pf;
{
	register struct cmap *c = &cmap[pgtocm(pf)];

	if ((c->c_flag & MLOCK) == 0)
		panic("dup page unlock");
	if (c->c_flag & MWANT)
		wakeup((caddr_t)c);
	c->c_flag &= ~(MLOCK|MWANT);
}

/* 
 * Lock a virtual segment.
 *
 * For each cluster of pages, if the cluster is not valid,
 * touch it to fault it in, otherwise just lock page frame.
 * Called from physio to ensure that the pages 
 * participating in raw i/o are valid and locked.
 * We use SDLYU to keep pagein from unlocking pages,
 * so they make it safely back here locked.
 */
vslock(base, count)
	caddr_t base;
{
	register unsigned v;
	register int npf;
	register struct pte *pte;

	u.u_procp->p_flag |= SDLYU;
	v = btop(base);
	pte = vtopte(u.u_procp, v);
	npf = btoc(count + ((int)base & CLOFSET));
	while (npf > 0) {
		if (pte->pg_v) 
			mlock(pte->pg_pfnum);
		else
			VOID fubyte((caddr_t)ctob(v));	/* fault in cluster */
		pte += CLSIZE;
		v += CLSIZE;
		npf -= CLSIZE;
	}
	u.u_procp->p_flag &= ~SDLYU;
}

/* 
 * Unlock a virtual segment.
 */
vsunlock(base, count, rw)
	caddr_t base;
{
	register struct pte *pte;
	register int npf;

	pte = vtopte(u.u_procp, btop(base));
	npf = btoc(count + ((int)base & CLOFSET));
	while (npf > 0) {
		munlock(pte->pg_pfnum);
		if (rw == B_READ)	/* Reading from device writes memory */
			pte->pg_m = 1;
		pte += CLSIZE;
		npf -= CLSIZE;
	}
}
