/*	text.c	2.2	2/10/80	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/proc.h"
#include "../h/text.h"
#include "../h/inode.h"
#include "../h/buf.h"
#include "../h/seg.h"
#include "../h/pte.h"
#include "../h/mtpr.h"
#include "../h/vm.h"
#include "../h/cmap.h"

/*
 * relinquish use of the shared text segment
 * of a process.
 */
xfree()
{
	register struct text *xp;
	register struct inode *ip;

	if((xp=u.u_procp->p_textp) == NULL)
		return;
	xlock(xp);
	ip = xp->x_iptr;
	if(--xp->x_count==0 && (ip->i_mode&ISVTX)==0) {
		xunlink(u.u_procp);
		xp->x_rssize -= vmemfree(tptopte(u.u_procp, 0), u.u_tsize);
		if (xp->x_rssize != 0)
			panic("xfree rssize");
		ip->i_flag &= ~ITEXT;
		if (ip->i_flag&ILOCK)
			ip->i_count--;
		else
			iput(ip);
		while (xp->x_poip)
			sleep((caddr_t)&xp->x_poip, PSWP+1);
		mfree(swapmap, xdsize(xp), xp->x_daddr);
		xp->x_flag &= ~XLOCK;
		xp->x_iptr = NULL;
	} else {
		xp->x_flag &= ~XLOCK;
		xccdec(xp, u.u_procp);
	}
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
xalloc(ip, pagi)
register struct inode *ip;
{
	register struct text *xp;
	register size_t ts;
	register struct text *xp1;

	if(u.u_exdata.ux_tsize == 0)
		return;
	xp1 = NULL;
	for (xp = &text[0]; xp < &text[NTEXT]; xp++) {
		if(xp->x_iptr == NULL) {
			if(xp1 == NULL)
				xp1 = xp;
			continue;
		}
		if ((xp->x_count > 0 || (xp->x_iptr->i_mode&ISVTX)) && xp->x_iptr == ip) {
			xlock(xp);
			xp->x_count++;
			u.u_procp->p_textp = xp;
			xlink(u.u_procp);
			xunlock(xp);
			return;
		}
	}
	if((xp=xp1) == NULL) {
		printf("out of text");
		psignal(u.u_procp, SIGKIL);
		return;
	}
	xp->x_flag = XLOAD|XLOCK;
	if (pagi)
		xp->x_flag |= XPAGI;
	ts = clrnd(btoc(u.u_exdata.ux_tsize));
	xp->x_size = ts;
	if((xp->x_daddr = malloc(swapmap, xdsize(xp))) == NULL) {
		swkill(u.u_procp);
		return;
	}
	xp->x_count = 1;
	xp->x_ccount = 0;
	xp->x_rssize = 0;
	xp->x_iptr = ip;
	ip->i_flag |= ITEXT;
	ip->i_count++;
	u.u_procp->p_textp = xp;
	xlink(u.u_procp);
	if (pagi == 0) {
		u.u_count = u.u_exdata.ux_tsize;
		u.u_offset = sizeof(u.u_exdata);
		u.u_base = 0;
		u.u_segflg = 2;
		chgprot(RW);
		u.u_procp->p_flag |= SKEEP;
		readi(ip);
		u.u_procp->p_flag &= ~SKEEP;
	}
	chgprot(RO);
	u.u_segflg = 0;
	xp->x_flag |= XWRIT;
	xp->x_flag &= ~XLOAD;
	xunlock(xp);
}

xdsize(xp)
	struct text *xp;
{

	if ((xp->x_flag & XPAGI) == 0)
		return (xp->x_size);
	return (clrnd(xp->x_size + ctopt(xp->x_size)));
}

/*
 * Lock and unlock a text segment from swapping
 */
xlock(xp)
register struct text *xp;
{

	while(xp->x_flag&XLOCK) {
		xp->x_flag |= XWANT;
		sleep((caddr_t)xp, PSWP);
	}
	xp->x_flag |= XLOCK;
}

xunlock(xp)
register struct text *xp;
{

	if (xp->x_flag&XWANT)
		wakeup((caddr_t)xp);
	xp->x_flag &= ~(XLOCK|XWANT);
}

/*
 * Decrement the in-core usage count of a shared text segment.
 * When it drops to zero, free the core space.
 */
xccdec(xp, p)
register struct text *xp;
register struct proc *p;
{

	if (xp==NULL || xp->x_ccount==0)
		return;
	xlock(xp);
	if (--xp->x_ccount == 0) {
		if (xp->x_flag & XWRIT) {
			vsswap(p, tptopte(p, 0), MTEXT, 0, xp->x_size, (struct dmap *)0);
			if (xp->x_flag & XPAGI)
				swap(p, xp->x_daddr + ctod(xp->x_size),
				    (caddr_t)tptopte(p, 0),
				    xp->x_size * sizeof (struct pte),
				    B_WRITE, B_PAGET, swapdev);
			xp->x_flag &= ~XWRIT;
		} else
			xp->x_rssize -= vmemfree(tptopte(p, 0), xp->x_size);
		if (xp->x_rssize != 0)
			panic("text rssize");
	}
	xunlink(p);
	xunlock(xp);
}

/*
 * free the swap image of all unused saved-text text segments
 * which are from device dev (used by umount system call).
 */
xumount(dev)
register dev;
{
	register struct text *xp;

	for (xp = &text[0]; xp < &text[NTEXT]; xp++) 
		if (xp->x_iptr!=NULL && dev==xp->x_iptr->i_dev)
			xuntext(xp);
}

/*
 * remove a shared text segment from the text table, if possible.
 */
xrele(ip)
register struct inode *ip;
{
	register struct text *xp;

	if ((ip->i_flag&ITEXT)==0)
		return;
	for (xp = &text[0]; xp < &text[NTEXT]; xp++)
		if (ip==xp->x_iptr)
			xuntext(xp);
}

/*
 * remove text image from the text table.
 * the use count must be zero.
 */
xuntext(xp)
register struct text *xp;
{
	register struct inode *ip;

	xlock(xp);
	if (xp->x_count) {
		xunlock(xp);
		return;
	}
	ip = xp->x_iptr;
	xp->x_flag &= ~XLOCK;
	xp->x_iptr = NULL;
	mfree(swapmap, xdsize(xp), xp->x_daddr);
	ip->i_flag &= ~ITEXT;
	if (ip->i_flag&ILOCK)
		ip->i_count--;
	else
		iput(ip);
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
