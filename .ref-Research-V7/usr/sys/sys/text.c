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

/*
 * Swap out process p.
 * The ff flag causes its core to be freed--
 * it may be off when called to create an image for a
 * child process in newproc.
 * Os is the old size of the data area of the process,
 * and is supplied during core expansion swaps.
 *
 * panic: out of swap space
 */
xswap(p, ff, os)
register struct proc *p;
{
	register a;

	if(os == 0)
		os = p->p_size;
	a = malloc(swapmap, ctod(p->p_size));
	if(a == NULL)
		panic("out of swap space");
	p->p_flag |= SLOCK;
	xccdec(p->p_textp);
	swap(a, p->p_addr, os, B_WRITE);
	if(ff)
		mfree(coremap, os, p->p_addr);
	p->p_addr = a;
	p->p_flag &= ~(SLOAD|SLOCK);
	p->p_time = 0;
	if(runout) {
		runout = 0;
		wakeup((caddr_t)&runout);
	}
}

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
	xp->x_flag &= ~XLOCK;
	u.u_procp->p_textp = NULL;
	ip = xp->x_iptr;
	if(--xp->x_count==0 && (ip->i_mode&ISVTX)==0) {
		xp->x_iptr = NULL;
		mfree(swapmap, ctod(xp->x_size), xp->x_daddr);
		mfree(coremap, xp->x_size, xp->x_caddr);
		ip->i_flag &= ~ITEXT;
		if (ip->i_flag&ILOCK)
			ip->i_count--;
		else
			iput(ip);
	} else
		xccdec(xp);
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
xalloc(ip)
register struct inode *ip;
{
	register struct text *xp;
	register unsigned ts;
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
		if(xp->x_iptr == ip) {
			xlock(xp);
			xp->x_count++;
			u.u_procp->p_textp = xp;
			if (xp->x_ccount == 0)
				xexpand(xp);
			else
				xp->x_ccount++;
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
	xp->x_count = 1;
	xp->x_ccount = 0;
	xp->x_iptr = ip;
	ip->i_flag |= ITEXT;
	ip->i_count++;
	ts = btoc(u.u_exdata.ux_tsize);
	xp->x_size = ts;
	if((xp->x_daddr = malloc(swapmap, (int)ctod(ts))) == NULL)
		panic("out of swap space");
	u.u_procp->p_textp = xp;
	xexpand(xp);
	estabur(ts, (unsigned)0, (unsigned)0, 0, RW);
	u.u_count = u.u_exdata.ux_tsize;
	u.u_offset = sizeof(u.u_exdata);
	u.u_base = 0;
	u.u_segflg = 2;
	u.u_procp->p_flag |= SLOCK;
	readi(ip);
	u.u_procp->p_flag &= ~SLOCK;
	u.u_segflg = 0;
	xp->x_flag = XWRIT;
}

/*
 * Assure core for text segment
 * Text must be locked to keep someone else from
 * freeing it in the meantime.
 * x_ccount must be 0.
 */
xexpand(xp)
register struct text *xp;
{
	if ((xp->x_caddr = malloc(coremap, xp->x_size)) != NULL) {
		if ((xp->x_flag&XLOAD)==0)
			swap(xp->x_daddr, xp->x_caddr, xp->x_size, B_READ);
		xp->x_ccount++;
		xunlock(xp);
		return;
	}
	if (save(u.u_ssav)) {
		sureg();
		return;
	}
	xswap(u.u_procp, 1, 0);
	xunlock(xp);
	u.u_procp->p_flag |= SSWAP;
	qswtch();
	/* no return */
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
xccdec(xp)
register struct text *xp;
{

	if (xp==NULL || xp->x_ccount==0)
		return;
	xlock(xp);
	if (--xp->x_ccount==0) {
		if (xp->x_flag&XWRIT) {
			xp->x_flag &= ~XWRIT;
			swap(xp->x_daddr,xp->x_caddr,xp->x_size,B_WRITE);
		}
		mfree(coremap, xp->x_size, xp->x_caddr);
	}
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

	if (ip->i_flag&ITEXT==0)
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
	mfree(swapmap, ctod(xp->x_size), xp->x_daddr);
	ip->i_flag &= ~ITEXT;
	if (ip->i_flag&ILOCK)
		ip->i_count--;
	else
		iput(ip);
}
