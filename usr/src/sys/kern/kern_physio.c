/*
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kern_physio.c	7.18 (Berkeley) %G%
 */

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "proc.h"
#include "seg.h"
#include "trace.h"
#include "map.h"
#include "vnode.h"
#include "specdev.h"

#ifdef HPUXCOMPAT
#include "user.h"
#endif

static	struct buf *getswbuf();
static	freeswbuf();

/*
 * Raw I/O. The arguments are
 *	The strategy routine for the device
 *	A buffer, which will either be a special buffer header owned
 *	    exclusively by the device for this purpose, or NULL,
 *	    indicating that we should use a swap buffer
 *	The device number
 *	Read/write flag
 * Essentially all the work is computing physical addresses and
 * validating them.
 * If the user has the proper access privilidges, the process is
 * marked 'delayed unlock' and the pages involved in the I/O are
 * faulted and locked. After the completion of the I/O, the above pages
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
	register int requested, done;
	register struct proc *p = curproc;
	char *a;
	int s, allocbuf = 0, error = 0;

	if (bp == NULL) {
		allocbuf = 1;
		bp = getswbuf(PRIBIO+1);
	}
	for (; uio->uio_iovcnt; uio->uio_iov++, uio->uio_iovcnt--) {
		iov = uio->uio_iov;
		if (!useracc(iov->iov_base, (u_int)iov->iov_len,
		    rw == B_READ ? B_WRITE : B_READ)) {
			error = EFAULT;
			break;
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
		bp->b_proc = p;
#ifdef HPUXCOMPAT
		if (ISHPMMADDR(iov->iov_base))
			bp->b_un.b_addr = (caddr_t)HPMMBASEADDR(iov->iov_base);
		else
#endif
		bp->b_un.b_addr = iov->iov_base;
		while (iov->iov_len > 0) {
			bp->b_flags = B_BUSY | B_PHYS | B_RAW | rw;
			bp->b_dev = dev;
			bp->b_blkno = btodb(uio->uio_offset);
			bp->b_bcount = iov->iov_len;
			(*mincnt)(bp);
			requested = bp->b_bcount;
			p->p_flag |= SPHYSIO;
			vslock(a = bp->b_un.b_addr, requested);
#if defined(hp300) || defined(i386)
			vmapbuf(bp);
#endif
			(*strat)(bp);
			s = splbio();
			while ((bp->b_flags & B_DONE) == 0)
				sleep((caddr_t)bp, PRIBIO);
#if defined(hp300) || defined(i386)
			vunmapbuf(bp);
#endif
			vsunlock(a, requested, rw);
			p->p_flag &= ~SPHYSIO;
			if (bp->b_flags&B_WANTED)	/* rare */
				wakeup((caddr_t)bp);
			splx(s);
			done = bp->b_bcount - bp->b_resid;
			bp->b_un.b_addr += done;
			iov->iov_len -= done;
			uio->uio_resid -= done;
			uio->uio_offset += done;
			/* temp kludge for disk drives */
			if (done < requested || bp->b_flags & B_ERROR)
				break;
		}
		bp->b_flags &= ~(B_BUSY | B_WANTED | B_PHYS | B_RAW);
		error = biowait(bp);
		/* temp kludge for disk drives */
		if (done < requested || bp->b_flags & B_ERROR)
			break;
	}
#if defined(hp300)
	DCIU();
#endif
	if (allocbuf)
		freeswbuf(bp);
	return (error);
}

u_int
minphys(bp)
	struct buf *bp;
{
	if (bp->b_bcount > MAXPHYS)
		bp->b_bcount = MAXPHYS;
}

static
struct buf *
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

static
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

rawread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return (physio(cdevsw[major(dev)].d_strategy, (struct buf *)NULL,
	    dev, B_READ, minphys, uio));
}

rawwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	return (physio(cdevsw[major(dev)].d_strategy, (struct buf *)NULL,
	    dev, B_WRITE, minphys, uio));
}
