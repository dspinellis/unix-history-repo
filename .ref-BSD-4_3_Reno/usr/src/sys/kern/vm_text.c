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
 *	@(#)vm_text.c	7.9 (Berkeley) 6/28/90
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "proc.h"
#include "text.h"
#include "vnode.h"
#include "buf.h"
#include "seg.h"
#include "cmap.h"
#include "uio.h"
#include "exec.h"
#include "vm.h"

#include "machine/pte.h"
#include "machine/cpu.h"

#define X_LOCK(xp) { \
	while ((xp)->x_flag & XLOCK) { \
		(xp)->x_flag |= XWANT; \
		sleep((caddr_t)(xp), PSWP); \
	} \
	(xp)->x_flag |= XLOCK; \
}
#define	XUNLOCK(xp) { \
	if ((xp)->x_flag & XWANT) \
		wakeup((caddr_t)(xp)); \
	(xp)->x_flag &= ~(XLOCK|XWANT); \
}
#define FREE_AT_HEAD(xp) { \
	(xp)->x_forw = xhead; \
	xhead = (xp); \
	(xp)->x_back = &xhead; \
	if (xtail == &xhead) \
		xtail = &(xp)->x_forw; \
	else \
		(xp)->x_forw->x_back = &(xp)->x_forw; \
}
#define FREE_AT_TAIL(xp) { \
	(xp)->x_back = xtail; \
	*xtail = (xp); \
	xtail = &(xp)->x_forw; \
	/* x_forw is NULL */ \
}
#define	ALLOC(xp) { \
	*((xp)->x_back) = (xp)->x_forw; \
	if ((xp)->x_forw) \
		(xp)->x_forw->x_back = (xp)->x_back; \
	else \
		xtail = (xp)->x_back; \
	(xp)->x_forw = NULL; \
	(xp)->x_back = NULL; \
}

/*
 * The text-cache:
 *
 * We place up to ``maxtextcache'' free text table entries on a free list
 * to form an LRU cache.  This causes the swap (but not RAM) resources to
 * be saved.  These text images are treated as "sticky", and are placed on
 * the free list when unused.  They may be reclaimed from the free list
 * until reused.  The cache changes to MRU once the maximum limit is
 * reached since we just cease caching new texts rather than replacing
 * the LRU one (should be fixed).  All cached text resources may be
 * reclaimed by calling xpurge().  Currently, swpexpand() and xalloc() do
 * this if an attempted swap allocation fails.
 *
 * Note that although true "sticky" texts are handling in the same way,
 * they are not considered part of the cache; i.e. they are not subject
 * to the maximum limit nor are they purged with xpurge().  They are in
 * a sense "locked down" cache entries.
 */
struct	text *xhead, **xtail;		/* text table free list */
int	xcache;				/* number of "sticky" texts retained */
int	maxtextcache = -1;		/* maximum number of "sticky" texts */
struct	xstats xstats;			/* cache statistics */

/*
 * initialize text table
 */
xinit()
{
	register struct text *xp;

	xtail = &xhead;
	for (xp = text; xp < textNTEXT; xp++)
		FREE_AT_TAIL(xp);
	if (maxtextcache == -1)
		maxtextcache = ntext;
}

/*
 * relinquish use of the shared text segment
 * of a process.
 */
xfree()
{
	register struct text *xp;
	register struct vnode *vp;
	struct vattr vattr;

	if ((xp = u.u_procp->p_textp) == NULL)
		return;
	xstats.free++;
	X_LOCK(xp);
	vp = xp->x_vptr;
	if (--xp->x_count == 0 &&
	    (VOP_GETATTR(vp, &vattr, u.u_cred) != 0 ||
	    (vattr.va_mode & VSVTX) == 0)) {
		if (xcache >= maxtextcache || xp->x_flag & XTRC ||
		    vattr.va_nlink == 0) {			/* XXX */
			xp->x_rssize -= vmemfree(tptopte(u.u_procp, 0),
				(int)u.u_tsize);
			if (xp->x_rssize != 0)
				panic("xfree rssize");
			while (xp->x_poip)
				sleep((caddr_t)&xp->x_poip, PSWP+1);
			xp->x_flag &= ~XLOCK;
			xuntext(xp);
			FREE_AT_HEAD(xp);
		} else {
			if (xp->x_flag & XWRIT) {
				xstats.free_cacheswap++;
				xp->x_flag |= XUNUSED;
			}
			xcache++;
			xstats.free_cache++;
			xp->x_flag |= XCACHED;
			xccdec(xp, u.u_procp);
#if defined(tahoe)	
			xp->x_ckey = 0;
#endif
			FREE_AT_TAIL(xp);
		}
	} else {
#if defined(tahoe)
		if (xp->x_count == 0)
			xp->x_ckey = 0;
#endif
		xccdec(xp, u.u_procp);
		xstats.free_inuse++;
	}
	xunlink(u.u_procp);
	XUNLOCK(xp);
	u.u_procp->p_textp = NULL;
}

/*
 * Attach to a shared text segment.
 * If there is no shared text, just return.
 * If there is, hook up to it:
 * if it is not currently being used, it has to be read
 * in from the vnode (vp); the written bit is set to force it
 * to be written out as appropriate.
 * If it is being used, but is not currently in core,
 * a swap has to be done to get it back.
 */
xalloc(vp, ep, toff, cred)
	register struct vnode *vp;
	struct exec *ep;
	off_t toff;
	struct ucred *cred;
{
	register struct text *xp;
	register struct proc *p;

	if (ep->a_text == 0)
		return;
	xstats.alloc++;
	p = u.u_procp;
	while ((xp = vp->v_text) != NULL) {
		if (xp->x_flag&XLOCK) {
			/*
			 * Wait for text to be unlocked,
			 * then start over (may have changed state).
			 */
			xwait(xp);
			continue;
		}
		X_LOCK(xp);
		if (xp->x_flag & XCACHED) {
			xstats.alloc_cachehit++;
			ALLOC(xp);
			xp->x_flag &= ~(XCACHED|XUNUSED);
			xcache--;
		} else
			xstats.alloc_inuse++;
		xp->x_count++;
		p->p_textp = xp;
		xlink(p);
		XUNLOCK(xp);
#if defined(tahoe)
		ckeyrelease(p->p_ckey);
		if (ckey_cnt[xp->x_ckey])
			ckey_cnt[xp->x_ckey]++;
		else		/* dead key */
			xp->x_ckey = getcodekey();
		p->p_ckey = xp->x_ckey;
#endif
		return;
	}
	xp = xhead;
	if (xp == NULL) {
		tablefull("text");
		psignal(p, SIGKILL);
		return;
	}
	ALLOC(xp);
	if (xp->x_vptr)
		xuntext(xp);
	xp->x_flag = XLOAD|XLOCK;
	if (p->p_flag & SPAGV)
		xp->x_flag |= XPAGV;
	xp->x_size = clrnd(btoc(ep->a_text));
	if (vsxalloc(xp) == NULL) {
		/* flush text cache and try again */
		if (xpurge() == 0 || vsxalloc(xp) == NULL) {
			swkill(p, "xalloc: no swap space");
			return;
		}
	}
	xp->x_count = 1;
	xp->x_ccount = 0;
	xp->x_rssize = 0;
	xp->x_mtime = 0;
	xp->x_vptr = vp;
	vp->v_flag |= VTEXT;
	vp->v_text = xp;
	VREF(vp);
	p->p_textp = xp;
	xlink(p);
	if ((p->p_flag & SPAGV) == 0) {
		settprot(RW);
		p->p_flag |= SKEEP;
		(void) vn_rdwr(UIO_READ, vp,
			(caddr_t)ctob(tptov(p, 0)),
			(int)ep->a_text, toff,
			UIO_USERSPACE, (IO_UNIT|IO_NODELOCKED), cred, (int *)0);
		p->p_flag &= ~SKEEP;
	}
	settprot(RO);
#if defined(tahoe)
	ckeyrelease(p->p_ckey);
	xp->x_ckey = getcodekey();
	p->p_ckey = xp->x_ckey;
#endif
	xp->x_flag |= XWRIT;
	xp->x_flag &= ~XLOAD;
	XUNLOCK(xp);
}

/*
 * Lock and unlock a text segment from swapping
 */
xlock(xp)
	register struct text *xp;
{

	X_LOCK(xp);
}

/*
 * Wait for xp to be unlocked if it is currently locked.
 */
xwait(xp)
	register struct text *xp;
{

	X_LOCK(xp);
	XUNLOCK(xp);
}

xunlock(xp)
	register struct text *xp;
{

	XUNLOCK(xp);
}

/*
 * Decrement the in-core usage count of a shared text segment,
 * which must be locked.  When the count drops to zero,
 * free the core space.
 */
xccdec(xp, p)
	register struct text *xp;
	register struct proc *p;
{

	if (--xp->x_ccount == 0) {
		if (xp->x_flag & XWRIT) {
			vsswap(p, tptopte(p, 0), CTEXT, 0, (int)xp->x_size,
			    (struct dmap *)0);
			if (xp->x_flag & XPAGV)
				(void) swap(p, xp->x_ptdaddr,
				    (caddr_t)tptopte(p, 0),
				    (int)xp->x_size * sizeof (struct pte),
				    B_WRITE, B_PAGET, swapdev_vp, 0);
			xp->x_flag &= ~XWRIT;
		} else
			xp->x_rssize -= vmemfree(tptopte(p, 0),
			    (int)xp->x_size);
		if (xp->x_rssize != 0)
			panic("text rssize");
	}
}

/*
 * Detach a process from the in-core text.
 * External interface to xccdec, used when swapping out a process.
 */
xdetach(xp, p)
	register struct text *xp;
	struct proc *p;
{

	if (xp && xp->x_ccount != 0) {
		X_LOCK(xp);
		xccdec(xp, p);
		xunlink(p);
		XUNLOCK(xp);
	}
}

/*
 * Free the swap image of all unused saved-text text segments
 * which are from file system mp (used by umount system call).
 */
xumount(mp)
	struct mount *mp;
{
	register struct text *xp;

	for (xp = text; xp < textNTEXT; xp++) 
		if (xp->x_vptr != NULL &&
		    (mp == NULL || (xp->x_vptr->v_mount == mp)) &&
		    (xp->x_flag & XLOCK) == 0)
			xuntext(xp);
	mpurgemp(mp);
}

/*
 * Flush all cached text segments to reclaim swap space.
 * Used during swap allocation when out of swap space.
 */
xpurge()
{
	register struct text *xp;
	int found = 0;

	xstats.purge++;
	for (xp = text; xp < textNTEXT; xp++)
		if (xp->x_vptr && (xp->x_flag & (XLOCK|XCACHED)) == XCACHED) {
			xuntext(xp);
			/* really gone? */
			if (xp->x_vptr == NULL)
				found++;
		}
	return(found);
}

/*
 * remove a shared text segment from the text table, if possible.
 */
xrele(vp)
	register struct vnode *vp;
{

	if (vp->v_flag & VTEXT)
		xuntext(vp->v_text);
}

/*
 * remove text image from the text table.
 * the use count must be zero.
 */
xuntext(xp)
	register struct text *xp;
{
	register struct vnode *vp;

	X_LOCK(xp);
	if (xp->x_count == 0) {
		vp = xp->x_vptr;
		xp->x_vptr = NULL;
		vsxfree(xp, (long)xp->x_size);
		vp->v_flag &= ~VTEXT;
		vp->v_text = NULL;
		mpurge(vp);
		vrele(vp);
		/*
		 * Take care of text cache statistics
		 */
		if (xp->x_flag & XCACHED) {
			if (xp->x_flag & XUNUSED)
				xstats.alloc_unused++;
			xp->x_flag &= ~(XCACHED|XUNUSED);
			xstats.alloc_cacheflush++;
			xcache--;
		}
	}
	XUNLOCK(xp);
}

/*
 * Add a process to those sharing a text segment by
 * getting the page tables and then linking to x_caddr.
 */
xlink(p)
	register struct proc *p;
{
	register struct text *xp = p->p_textp;

	if (xp == 0)
		return;
	vinitpt(p);
	p->p_xlink = xp->x_caddr;
	xp->x_caddr = p;
	xp->x_ccount++;
}

xunlink(p)
	register struct proc *p;
{
	register struct text *xp = p->p_textp;
	register struct proc *q;

	if (xp == 0)
		return;
	if (xp->x_caddr == p) {
		xp->x_caddr = p->p_xlink;
		p->p_xlink = 0;
		return;
	}
	for (q = xp->x_caddr; q->p_xlink; q = q->p_xlink)
		if (q->p_xlink == p) {
			q->p_xlink = p->p_xlink;
			p->p_xlink = 0;
			return;
		}
	panic("lost text");
}

/*
 * Replace p by q in a text incore linked list.
 * Used by vfork(), internally.
 */
xrepl(p, q)
	struct proc *p, *q;
{
	register struct text *xp = q->p_textp;

	if (xp == 0)
		return;
	xunlink(p);
	q->p_xlink = xp->x_caddr;
	xp->x_caddr = q;
}

int xkillcnt = 0;

/*
 * Invalidate the text associated with vp.
 * Purge in core cache of pages associated with vp and kill all active
 * processes.
 */
xinval(vp)
	struct vnode *vp;
{
	register struct text *xp;
	register struct proc *p;
	int found = 0;

	mpurge(vp);
	xp = vp->v_text;
	if (xp->x_flag & XPAGV) {
		for (p = xp->x_caddr; p; p = p->p_xlink) {
			/*
			 * swkill without uprintf
			 */
			printf("pid %d killed due to text modification\n",
				p->p_pid);
			psignal(p, SIGKILL);
			p->p_flag |= SULOCK;
			xkillcnt++;
			found++;
		}
		/*
		 * Take care of the text cache.
		 * If there was a process still using the text just mark
		 * the text as XTRC so it won't be cached.  If no one was
		 * using it then it is in the cache and we need to flush
		 * it with xuntext.
		 */
		if (found)
			xp->x_flag |= XTRC;
		else
			xuntext(xp);
	}
}
