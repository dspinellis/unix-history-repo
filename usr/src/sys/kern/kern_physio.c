/*-
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)kern_physio.c	7.26 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/conf.h>
#include <sys/proc.h>
#include <sys/vnode.h>

#ifdef HPUXCOMPAT
#include <sys/user.h>
#endif

static void freeswbuf __P((struct buf *));
static struct buf *getswbuf __P((int));

/*
 * This routine does device I/O for a user process.
 *
 * If the user has the proper access privileges, the process is
 * marked 'delayed unlock' and the pages involved in the I/O are
 * faulted and locked. After the completion of the I/O, the pages
 * are unlocked.
 */
physio(strat, bp, dev, rw, mincnt, uio)
	int (*strat)(); 
	register struct buf *bp;
	dev_t dev;
	int rw;
	u_int (*mincnt)();
	struct uio *uio;
{
	register struct iovec *iov;
	register int requested = 0, done = 0;
	register struct proc *p = curproc;
	char *a;
	int s, allocbuf = 0, error = 0;
#ifdef SECSIZE
	int bsize;
	struct partinfo dpart;
#endif SECSIZE

#ifdef SECSIZE
	if ((unsigned)major(dev) < nchrdev &&
	    (*cdevsw[major(dev)].d_ioctl)(dev, DIOCGPART, (caddr_t)&dpart,
	    FREAD) == 0)
		bsize = dpart.disklab->d_secsize;
	else
		bsize = DEV_BSIZE;
#endif SECSIZE
	for (;;) {
		if (uio->uio_iovcnt == 0)
			return (0);
		iov = uio->uio_iov;
		if (useracc(iov->iov_base, (u_int)iov->iov_len,
		    rw==B_READ? B_WRITE : B_READ) == NULL)
			return (EFAULT);
		s = splbio();
		while (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO+1);
		}
		if (!allocbuf) {	/* only if sharing caller's buffer */
			s = splbio();
			while (bp->b_flags&B_BUSY) {
				bp->b_flags |= B_WANTED;
				sleep((caddr_t)bp, PRIBIO+1);
			}
			splx(s);
		}
		bp->b_error = 0;
		bp->b_proc = u.u_procp;
#ifdef SECSIZE
		bp->b_blksize = bsize;
#endif SECSIZE
		bp->b_un.b_addr = iov->iov_base;
		while (iov->iov_len > 0) {
			bp->b_flags = B_BUSY | B_PHYS | rw;
			bp->b_dev = dev;
#ifdef SECSIZE
			bp->b_blkno = uio->uio_offset / bsize;
#else SECSIZE
			bp->b_blkno = btodb(uio->uio_offset);
#endif SECSIZE
			bp->b_bcount = iov->iov_len;
			(*mincnt)(bp);
			c = bp->b_bcount;
			u.u_procp->p_flag |= SPHYSIO;
			vslock(a = bp->b_un.b_addr, c);
			physstrat(bp, strat, PRIBIO);
			(void) splbio();
			vsunlock(a, c, rw);
			u.u_procp->p_flag &= ~SPHYSIO;
			if (bp->b_flags&B_WANTED)
				wakeup((caddr_t)bp);
			splx(s);
			c -= bp->b_resid;
			bp->b_un.b_addr += c;
			iov->iov_len -= c;
			uio->uio_resid -= c;
			uio->uio_offset += c;
			/* temp kludge for tape drives */
			if (bp->b_resid || (bp->b_flags&B_ERROR))
				break;
		}
		bp->b_flags &= ~(B_BUSY|B_WANTED|B_PHYS);
		error = geterror(bp);
		if (bp->b_resid || error)
			return (error);
		uio->uio_iov++;
		uio->uio_iovcnt--;
	}
#if defined(hp300)
	DCIU();
#endif
	if (allocbuf)
		freeswbuf(bp);
	return (error);
}

/*
 * Calculate the maximum size of I/O request that can be requested
 * in a single operation. This limit is necessary to prevent a single
 * process from being able to lock more than a fixed amount of memory
 * in the kernel.
 */
u_int
minphys(bp)
	struct buf *bp;
{
	if (bp->b_bcount > MAXPHYS)
		bp->b_bcount = MAXPHYS;
}

static struct buf *
getswbuf(prio)
	int prio;
{
	int s;
	struct buf *bp;

	s = splbio();
	while (bswlist.av_forw == NULL) {
		bswlist.b_flags |= B_WANTED;
		sleep((caddr_t)&bswlist, prio);
	}
	bp = bswlist.av_forw;
	bswlist.av_forw = bp->av_forw;
	splx(s);
	return (bp);
}

static void
freeswbuf(bp)
	struct buf *bp;
{
	int s;

	s = splbio();
	bp->av_forw = bswlist.av_forw;
	bswlist.av_forw = bp;
	if (bp->b_vp)
		brelvp(bp);
	if (bswlist.b_flags & B_WANTED) {
		bswlist.b_flags &= ~B_WANTED;
		wakeup((caddr_t)&bswlist);
		wakeup((caddr_t)pageproc);
	}
	splx(s);
}

/*
 * Do a read on a device for a user process.
 */
rawread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return (physio(cdevsw[major(dev)].d_strategy, (struct buf *)NULL,
	    dev, B_READ, minphys, uio));
}

/*
 * Do a write on a device for a user process.
 */
rawwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return (physio(cdevsw[major(dev)].d_strategy, (struct buf *)NULL,
	    dev, B_WRITE, minphys, uio));
}
