/*
 * Copyright (c) 1982, 1986, 1989 Regents of the University of California.
 * All rights reserved.
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
 *	@(#)vm_mem.c	7.15 (Berkeley) 6/28/90
 */

#include "param.h"
#include "systm.h"
#include "cmap.h"
#include "user.h"
#include "proc.h"
#include "text.h"
#include "vm.h"
#include "buf.h"
#include "vnode.h"
#include "mount.h"
#include "trace.h"

#include "machine/pte.h"

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
			outofmem();
		while (freemem == 0)
			sleep((caddr_t)&freemem, PSWP+2);
		m = imin(size, freemem);
		(void) memall(pte, m, p, type);
		size -= m;
		pte += m;
	}
	if (freemem < desfree)
		outofmem();
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
 * we simply mark the page as c_gone and let the pageout
 * daemon free the page when it is through with it.
 * If a page is reclaimable, and already in the free list, then
 * we mark the page as c_gone, and (of course) don't free it.
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
	register int j;
	int size, pcnt;
#ifdef notdef
	int fileno;
#endif

	if (count % CLSIZE)
		panic("vmemfree");
	for (size = 0, pcnt = 0; count > 0; pte += CLSIZE, count -= CLSIZE) {
		if (pte->pg_fod == 0 && pte->pg_pfnum) {
			c = &cmap[pgtocm(pte->pg_pfnum)];
			pcnt += CLSIZE;
			if (c->c_lock && c->c_type != CSYS) {
				for (j = 0; j < CLSIZE; j++)
					*(int *)(pte+j) &= PG_PROT;
				c->c_gone = 1;
				goto free;
			}
			if (c->c_free) {
				pcnt -= CLSIZE;
				for (j = 0; j < CLSIZE; j++)
					*(int *)(pte+j) &= PG_PROT;
				if (c->c_type == CTEXT)
					distpte(&text[c->c_ndx], c->c_page,
					    pte);
				c->c_gone = 1;
				goto free;
			}
			if (size == 0)
				spte = pte;
			size += CLSIZE;
			continue;
		}
#ifdef notdef
		/* Don't do anything with mapped ptes */
		if (pte->pg_fod && pte->pg_v)
			goto free;
#endif
		if (pte->pg_fod) {
#ifdef notdef
			fileno = ((struct fpte *)pte)->pg_fileno;
			if (fileno < NOFILE)
				panic("vmemfree vread");
#endif
			for (j = 0; j < CLSIZE; j++)
				*(int *)(pte+j) &= PG_PROT;
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
munlink(c)
	register struct cmap *c;
{
	register int next, prev;

	next = c->c_next;
	prev = c->c_prev;
	cmap[prev].c_next = next;
	cmap[next].c_prev = prev;
	c->c_free = 0;
	if (freemem < minfree)
		outofmem();
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
	struct cmap *c1, *c2;
	int s;

	if (size % CLSIZE)
		panic("memall");
	s = splimp();
	if (size > freemem) {
		splx(s);
		return (0);
	}
	trace(TR_MALL, size, u.u_procp->p_pid);
	for (i = size; i > 0; i -= CLSIZE) {
		curpos = cmap[CMHEAD].c_next;
		c = &cmap[curpos];
		freemem -= CLSIZE;
		next = c->c_next;
		cmap[CMHEAD].c_next = next;
		cmap[next].c_prev = CMHEAD;
		if (c->c_free == 0)
			panic("dup mem alloc");
		if (cmtopg(curpos) > maxfree)
			panic("bad mem alloc");
		if (c->c_gone == 0 && c->c_type != CSYS) {
			if (c->c_type == CTEXT)
				rp = text[c->c_ndx].x_caddr;
			else
				rp = &proc[c->c_ndx];
			while (rp->p_flag & SNOVM)
				rp = rp->p_xlink;
			switch (c->c_type) {

			case CTEXT:
				rpte = tptopte(rp, c->c_page);
				break;

			case CDATA:
				rpte = dptopte(rp, c->c_page);
				break;

			case CSTACK:
				rpte = sptopte(rp, c->c_page);
				break;
			}
			zapcl(rpte, pg_pfnum) = 0;
			if (c->c_type == CTEXT)
				distpte(&text[c->c_ndx], c->c_page, rpte);
		}
		switch (type) {

		case CSYS:
			c->c_ndx = p->p_ndx;
			break;

		case CTEXT:
			c->c_page = vtotp(p, ptetov(p, pte));
			c->c_ndx = p->p_textp - &text[0];
			break;

		case CDATA:
			c->c_page = vtodp(p, ptetov(p, pte));
			c->c_ndx = p->p_ndx;
			break;

		case CSTACK:
			c->c_page = vtosp(p, ptetov(p, pte));
			c->c_ndx = p->p_ndx;
			break;
		}
		if (c->c_blkno) {
			/*
			 * This is very like munhash(), except
			 * that we really don't want to bother
			 * to calculate a vp to pass to it.
			 */
			j = CMHASH(c->c_blkno);
			c1 = &cmap[cmhash[j]];
			if (c1 == c)
				cmhash[j] = c1->c_hlink;
			else {
				for (;;) {
					if (c1 == ecmap)
						panic("memall ecmap");
					c2 = c1;
					c1 = &cmap[c2->c_hlink];
					if (c1 == c)
						break;
				}
				c2->c_hlink = c1->c_hlink;
			}
			if (mfind(c->c_vp, (daddr_t)(u_long)c->c_blkno))
				panic("memall mfind");
			HOLDRELE(c1->c_vp);
			c1->c_vp = NULLVP;
			c1->c_blkno = 0;
			c1->c_hlink = 0;
		}
		pf = cmtopg(curpos);
		for (j = 0; j < CLSIZE; j++) {
			*(int *)pte = 0;
			pte++->pg_pfnum = pf++;
		}
		c->c_free = 0;
		c->c_gone = 0;
		if (c->c_intrans || c->c_want)
			panic("memall intrans|want");
		c->c_lock = 1;
		c->c_type = type;
	}
	splx(s);
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
memfree(pte, size, detach)
	register struct pte *pte;
	register int size;
{
	register int i, j, prev, next;
	register struct cmap *c;
	int s;
	
	if (size % CLSIZE)
		panic("memfree");
	if (freemem < CLSIZE * KLMAX)
		wakeup((caddr_t)&freemem);
	while (size > 0) {
		size -= CLSIZE;
		i = pte->pg_pfnum;
		if (i < firstfree || i > maxfree)
			panic("bad mem free");
		i = pgtocm(i);
		c = &cmap[i];
		if (c->c_free)
			panic("dup mem free");
		if (detach && c->c_type != CSYS) {
			for (j = 0; j < CLSIZE; j++)
				*(int *)(pte+j) &= PG_PROT;
			c->c_gone = 1;
		}
		s = splimp();
		if (detach && c->c_blkno == 0) {
			next = cmap[CMHEAD].c_next;
			cmap[next].c_prev = i;
			c->c_prev = CMHEAD;
			c->c_next = next;
			cmap[CMHEAD].c_next = i;
		} else {
			prev = cmap[CMHEAD].c_prev;
			cmap[prev].c_next = i;
			c->c_next = CMHEAD;
			c->c_prev = prev;
			cmap[CMHEAD].c_prev = i;
		}
		c->c_free = 1;
		freemem += CLSIZE;
		splx(s);
		pte += CLSIZE;
	}
}

/*
 * Enter clist block c on the hash chains.
 * It contains file system block bn from vnode vp.
 */
mhash(c, vp, bn)
	register struct cmap *c;
	struct vnode *vp;
	daddr_t bn;
{
	register int i = CMHASH(bn);

	c->c_hlink = cmhash[i];
	cmhash[i] = c - cmap;
	c->c_blkno = bn;
	c->c_vp = vp;
	VHOLD(vp);
}

/*
 * Pull the clist entry of <vp,bn> off the hash chains
 * if present.
 */
munhash(vp, bn)
	struct vnode *vp;
	daddr_t bn;
{
	int i = CMHASH(bn);
	register struct cmap *c1, *c2;
	int s = splimp();

	c1 = &cmap[cmhash[i]];
	if (c1 == ecmap)
		goto out;
	if (c1->c_blkno == bn && c1->c_vp == vp)
		cmhash[i] = c1->c_hlink;
	else {
		for (;;) {
			c2 = c1;
			c1 = &cmap[c2->c_hlink];
			if (c1 == ecmap)
				goto out;
			if (c1->c_blkno == bn && c1->c_vp == vp)
				break;
		}
		c2->c_hlink = c1->c_hlink;
	}
	HOLDRELE(vp);
	c1->c_vp = NULLVP;
	c1->c_blkno = 0;
	c1->c_hlink = 0;
out:
	splx(s);
}

/*
 * Look for block bn of vnode vp in the free pool.
 * Currently it should not be possible to find it unless it is
 * c_free and c_gone, although this may later not be true.
 * (This is because active texts are locked against file system
 * writes by the system.)
 */
struct cmap *
mfind(vp, bn)
	struct vnode *vp;
	daddr_t bn;
{
	register struct cmap *c1 = &cmap[cmhash[CMHASH(bn)]];
	int si = splimp();

	while (c1 != ecmap) {
		if (c1->c_blkno == bn && c1->c_vp == vp) {
			splx(si);
			return (c1);
		}
		c1 = &cmap[c1->c_hlink];
	}
	splx(si);
	return ((struct cmap *)0);
}

/*
 * Purge blocks from vnode vp from incore cache
 * before umount().
 */
mpurge(vp)
	struct vnode *vp;
{
	register struct cmap *c1, *c2;
	register int i;
	int relcnt = 0, si = splimp();

	for (i = 0; i < CMHSIZ; i++) {
more:
		c1 = &cmap[cmhash[i]];
		if (c1 == ecmap)
			continue;
		if (c1->c_vp == vp)
			cmhash[i] = c1->c_hlink;
		else {
			for (;;) {
				c2 = c1;
				c1 = &cmap[c1->c_hlink];
				if (c1 == ecmap)
					goto cont;
				if (c1->c_vp == vp)
					break;
			}
			c2->c_hlink = c1->c_hlink;
		}
		relcnt++;
		c1->c_vp = NULLVP;
		c1->c_blkno = 0;
		c1->c_hlink = 0;
		goto more;
cont:
		;
	}
	while (relcnt--)
		HOLDRELE(vp);
	splx(si);
}

/*
 * Purge blocks for filesystem mp from incore cache
 * before umount().
 */
mpurgemp(mp)
	struct mount *mp;
{
	register struct cmap *c1, *c2;
	register int i;
	int s = splimp();

more:
	for (i = 0; i < CMHSIZ; i++) {
		c1 = &cmap[cmhash[i]];
		if (c1 == ecmap)
			continue;
		if (mp == NULL || c1->c_vp->v_mount == mp)
			cmhash[i] = c1->c_hlink;
		else {
			for (;;) {
				c2 = c1;
				c1 = &cmap[c1->c_hlink];
				if (c1 == ecmap)
					goto cont;
				if (c1->c_vp->v_mount == mp)
					break;
			}
			c2->c_hlink = c1->c_hlink;
		}
		(void) splx(s);
		HOLDRELE(c1->c_vp);
		s = splimp();
		c1->c_vp = NULLVP;
		c1->c_blkno = 0;
		c1->c_hlink = 0;
		goto more;
cont:
		;
	}
	(void) splx(s);
}

/*
 * Initialize core map
 */
meminit(first, last)
	int first, last;
{
	register int i;
	register struct cmap *c;

	firstfree = clrnd(first);
	maxfree = clrnd(last - (CLSIZE - 1));
	freemem = maxfree - firstfree;
	ecmx = ecmap - cmap;
	if (ecmx < freemem / CLSIZE)
		freemem = ecmx * CLSIZE;
	for (i = 1; i <= freemem / CLSIZE; i++) {
		cmap[i-1].c_next = i;
		c = &cmap[i];
		c->c_prev = i-1;
		c->c_free = 1;
		c->c_gone = 1;
		c->c_type = CSYS;
		c->c_blkno = 0;
	}
	cmap[freemem / CLSIZE].c_next = CMHEAD;
	for (i = 0; i < CMHSIZ; i++)
		cmhash[i] = ecmx;
	cmap[CMHEAD].c_prev = freemem / CLSIZE;
	cmap[CMHEAD].c_type = CSYS;
	avefree = freemem;
}

#ifdef notdef
/*
 * Wait for frame pf to become unlocked
 * if it is currently locked.
 */
mwait(c)
	struct cmap *c;
{

	mlock(c);
	munlock(c);
}

/*
 * Lock a page frame.
 */
mlock(c)
	register struct cmap *c;
{

	while (c->c_lock) {
		c->c_want = 1;
		sleep((caddr_t)c, PSWP+1);
	}
	c->c_lock = 1;
}

/*
 * Unlock a page frame.
 */
munlock(c)
	register struct cmap *c;
{

	if (c->c_lock == 0)
		panic("dup page unlock");
	if (c->c_want) {
		wakeup((caddr_t)c);
		c->c_want = 0;
	}
	c->c_lock = 0;
}
#endif

/* 
 * Lock a virtual segment.
 *
 * For each cluster of pages, if the cluster is not valid,
 * touch it to fault it in, otherwise just lock page frame.
 * Called from physio to ensure that the pages 
 * participating in raw i/o are valid and locked.
 */
vslock(base, count)
	caddr_t base;
{
	register unsigned v;
	register int npf;
	register struct pte *pte;
	register struct cmap *c;

#if defined(tahoe)
	/*
	 * TAHOE I/O drivers may arrive here on raw I/O,
	 * base will be a system address in this case
	 */
	if (((int)base & KERNBASE) == KERNBASE) 	/* system addresses */
		return;
#endif
	v = clbase(btop(base));
	pte = vtopte(u.u_procp, v);
	npf = btoc(count + ((int)base & CLOFSET));
	for (; npf > 0; pte += CLSIZE, v += CLSIZE, npf -= CLSIZE) {
retry:
		if (pte->pg_v) {
#ifdef MAPMEM
			if (pte->pg_fod)	/* mapped page */
				continue;
#endif
			c = &cmap[pgtocm(pte->pg_pfnum)];
			if (c->c_lock) {
				MLOCK(c);
				MUNLOCK(c);
				goto retry;
			}
			MLOCK(c);
		} else
			pagein(ctob(v), 1);	/* return it locked */
	}
}

/* 
 * Unlock a virtual segment.
 */
vsunlock(base, count, rw)
	caddr_t base;
{
	register struct pte *pte;
	register struct cmap *c;
	unsigned v;
	int npf;

#if defined(tahoe)
	/*
	 * TAHOE I/O drivers may arrive here on raw I/O,
	 * base will be a system address in this case
	 */
	if (((int)base & KERNBASE) == KERNBASE) 	/* system addresses */
		return;
#endif
	v = clbase(btop(base));
	pte = vtopte(u.u_procp, v);
	npf = btoc(count + ((int)base & CLOFSET));
	for (; npf > 0; pte += CLSIZE, npf -= CLSIZE) {
#ifdef MAPMEM
		if (pte->pg_fod && pte->pg_v)	/* mapped page */
			continue;
#endif
		c = &cmap[pgtocm(pte->pg_pfnum)];
		MUNLOCK(c);
		if (rw == B_READ)	/* Reading from device writes memory */
			pte->pg_m = 1;
	}
}
