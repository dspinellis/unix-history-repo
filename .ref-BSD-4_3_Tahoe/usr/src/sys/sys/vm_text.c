/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vm_text.c	7.3 (Berkeley) 3/25/87
 */

#include "param.h"
#include "systm.h"
#include "map.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "text.h"
#include "inode.h"
#include "buf.h"
#include "seg.h"
#include "vm.h"
#include "cmap.h"
#include "uio.h"
#include "exec.h"

#include "../machine/pte.h"
#include "../machine/cpu.h"

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
 * We place free text table entries on a free list.
 * All text images are treated as "sticky,"
 * and are placed on the free list (as an LRU cache) when unused.
 * They may be reclaimed from the free list until reused.
 * Files marked sticky are locked into the table, and are never freed.
 * For machines with limited swap space, this may result
 * in filling up swap, and thus we allow a limit
 * to be placed on the number of text images to cache.
 * (In that case, really should change the algorithm
 * for freeing a text when the cache is full;
 * should free least-recently-used text rather than current one.)
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

	if ((xp = u.u_procp->p_textp) == NULL)
		return;
	xstats.free++;
	X_LOCK(xp);
	if (--xp->x_count == 0 && (xp->x_iptr->i_mode & ISVTX) == 0) {
		if (xcache >= maxtextcache || xp->x_flag & XTRC ||
		    xp->x_iptr->i_nlink == 0) {			/* XXX */
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
 * in from the inode (ip); the written bit is set to force it
 * to be written out as appropriate.
 * If it is being used, but is not currently in core,
 * a swap has to be done to get it back.
 */
xalloc(ip, ep, pagi)
	struct exec *ep;
	register struct inode *ip;
{
	register struct text *xp;
	register size_t ts;

	if (ep->a_text == 0)
		return;
	xstats.alloc++;
	while ((xp = ip->i_text) != NULL) {
		if (xp->x_flag&XLOCK) {
			/*
			 * Wait for text to be unlocked,
			 * then start over (may have changed state).
			 */
			xwait(xp);
			continue;
		}
		X_LOCK(xp);
		if (xp->x_back) {
			xstats.alloc_cachehit++;
			ALLOC(xp);
			xp->x_flag &= ~XUNUSED;
			xcache--;
		} else
			xstats.alloc_inuse++;
		xp->x_count++;
		u.u_procp->p_textp = xp;
		xlink(u.u_procp);
		XUNLOCK(xp);
#if defined(tahoe)
		ckeyrelease(u.u_procp->p_ckey);
		if (ckey_cnt[xp->x_ckey])
			ckey_cnt[xp->x_ckey]++;
		else		/* dead key */
			xp->x_ckey = getcodekey();
		u.u_procp->p_ckey = xp->x_ckey;
#endif
		return;
	}
	xp = xhead;
	if (xp == NULL) {
		tablefull("text");
		psignal(u.u_procp, SIGKILL);
		return;
	}
	ALLOC(xp);
	if (xp->x_iptr) {
		xstats.alloc_cacheflush++;
		if (xp->x_flag & XUNUSED)
			xstats.alloc_unused++;
		xuntext(xp);
		xcache--;
	}
	xp->x_flag = XLOAD|XLOCK;
	if (pagi)
		xp->x_flag |= XPAGI;
	ts = clrnd(btoc(ep->a_text));
	xp->x_size = ts;
	if (vsxalloc(xp) == NULL) {
		swkill(u.u_procp, "xalloc: no swap space");
		return;
	}
	xp->x_count = 1;
	xp->x_ccount = 0;
	xp->x_rssize = 0;
	xp->x_iptr = ip;
	ip->i_flag |= ITEXT;
	ip->i_text = xp;
	ip->i_count++;
	u.u_procp->p_textp = xp;
	xlink(u.u_procp);
	if (pagi == 0) {
		settprot(RW);
		u.u_procp->p_flag |= SKEEP;
		(void) rdwri(UIO_READ, ip,
			(caddr_t)ctob(tptov(u.u_procp, 0)),
			(int)ep->a_text, (off_t)sizeof (struct exec),
			2, (int *)0);
		u.u_procp->p_flag &= ~SKEEP;
	}
	settprot(RO);
#if defined(tahoe)
	ckeyrelease(u.u_procp->p_ckey);
	xp->x_ckey = getcodekey();
	u.u_procp->p_ckey = xp->x_ckey;
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
			if (xp->x_flag & XPAGI)
				(void) swap(p, xp->x_ptdaddr,
				    (caddr_t)tptopte(p, 0),
				    (int)xp->x_size * sizeof (struct pte),
				    B_WRITE, B_PAGET, swapdev, 0);
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
 * which are from device dev (used by umount system call).
 * If dev is NODEV, do all devices (used when rebooting).
 */
xumount(dev)
	register dev_t dev;
{
	register struct text *xp;

	for (xp = text; xp < textNTEXT; xp++) 
		if (xp->x_iptr != NULL &&
		    (dev == xp->x_iptr->i_dev || dev == NODEV) &&
		    (xp->x_flag & XLOCK) == 0)
			xuntext(xp);
}

/*
 * remove a shared text segment from the text table, if possible.
 */
xrele(ip)
	register struct inode *ip;
{

	if (ip->i_flag & ITEXT)
		xuntext(ip->i_text);
}

/*
 * remove text image from the text table.
 * the use count must be zero.
 */
xuntext(xp)
	register struct text *xp;
{
	register struct inode *ip;

	X_LOCK(xp);
	if (xp->x_count == 0) {
		ip = xp->x_iptr;
		xp->x_iptr = NULL;
		vsxfree(xp, (long)xp->x_size);
		ip->i_flag &= ~ITEXT;
		ip->i_text = NULL;
		irele(ip);
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
