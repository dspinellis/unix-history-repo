/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vfs_bio.c	7.3 (Berkeley) %G%
 */

#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "buf.h"
#include "conf.h"
#include "proc.h"
#include "seg.h"
#include "vm.h"
#include "trace.h"

/*
 * Read in (if necessary) the block and return a buffer pointer.
 */
struct buf *
#ifdef SECSIZE
bread(dev, blkno, size, secsize)
#else SECSIZE
bread(dev, blkno, size)
#endif SECSIZE
	dev_t dev;
	daddr_t blkno;
	int size;
#ifdef SECSIZE
	long secsize;
#endif SECSIZE
{
	register struct buf *bp;

	if (size == 0)
		panic("bread: size 0");
#ifdef SECSIZE
	bp = getblk(dev, blkno, size, secsize);
#else SECSIZE
	bp = getblk(dev, blkno, size);
#endif SECSIZE
	if (bp->b_flags&(B_DONE|B_DELWRI)) {
		trace(TR_BREADHIT, pack(dev, size), blkno);
		return (bp);
	}
	bp->b_flags |= B_READ;
	if (bp->b_bcount > bp->b_bufsize)
		panic("bread");
	(*bdevsw[major(dev)].d_strategy)(bp);
	trace(TR_BREADMISS, pack(dev, size), blkno);
	u.u_ru.ru_inblock++;		/* pay for read */
	biowait(bp);
	return (bp);
}

/*
 * Read in the block, like bread, but also start I/O on the
 * read-ahead block (which is not allocated to the caller)
 */
struct buf *
#ifdef SECSIZE
breada(dev, blkno, size, secsize, rablkno, rabsize)
#else SECSIZE
breada(dev, blkno, size, rablkno, rabsize)
#endif SECSIZE
	dev_t dev;
	daddr_t blkno; int size;
#ifdef SECSIZE
	long secsize;
#endif SECSIZE
	daddr_t rablkno; int rabsize;
{
	register struct buf *bp, *rabp;

	bp = NULL;
	/*
	 * If the block isn't in core, then allocate
	 * a buffer and initiate i/o (getblk checks
	 * for a cache hit).
	 */
	if (!incore(dev, blkno)) {
#ifdef SECSIZE
		bp = getblk(dev, blkno, size, secsize);
#else SECSIZE
		bp = getblk(dev, blkno, size);
#endif SECSIZE
		if ((bp->b_flags&(B_DONE|B_DELWRI)) == 0) {
			bp->b_flags |= B_READ;
			if (bp->b_bcount > bp->b_bufsize)
				panic("breada");
			(*bdevsw[major(dev)].d_strategy)(bp);
			trace(TR_BREADMISS, pack(dev, size), blkno);
			u.u_ru.ru_inblock++;		/* pay for read */
		} else
			trace(TR_BREADHIT, pack(dev, size), blkno);
	}

	/*
	 * If there's a read-ahead block, start i/o
	 * on it also (as above).
	 */
	if (rablkno && !incore(dev, rablkno)) {
#ifdef SECSIZE
		rabp = getblk(dev, rablkno, rabsize, secsize);
#else SECSIZE
		rabp = getblk(dev, rablkno, rabsize);
#endif SECSIZE
		if (rabp->b_flags & (B_DONE|B_DELWRI)) {
			brelse(rabp);
			trace(TR_BREADHITRA, pack(dev, rabsize), blkno);
		} else {
			rabp->b_flags |= B_READ|B_ASYNC;
			if (rabp->b_bcount > rabp->b_bufsize)
				panic("breadrabp");
			(*bdevsw[major(dev)].d_strategy)(rabp);
			trace(TR_BREADMISSRA, pack(dev, rabsize), rablock);
			u.u_ru.ru_inblock++;		/* pay in advance */
		}
	}

	/*
	 * If block was in core, let bread get it.
	 * If block wasn't in core, then the read was started
	 * above, and just wait for it.
	 */
	if (bp == NULL)
#ifdef SECSIZE
		return (bread(dev, blkno, size, secsize));
#else SECSIZE
		return (bread(dev, blkno, size));
#endif SECSIZE
	biowait(bp);
	return (bp);
}

/*
 * Write the buffer, waiting for completion.
 * Then release the buffer.
 */
bwrite(bp)
	register struct buf *bp;
{
	register flag;

	flag = bp->b_flags;
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI);
	if ((flag&B_DELWRI) == 0)
		u.u_ru.ru_oublock++;		/* noone paid yet */
	trace(TR_BWRITE, pack(bp->b_dev, bp->b_bcount), bp->b_blkno);
	if (bp->b_bcount > bp->b_bufsize)
		panic("bwrite");
	(*bdevsw[major(bp->b_dev)].d_strategy)(bp);

	/*
	 * If the write was synchronous, then await i/o completion.
	 * If the write was "delayed", then we put the buffer on
	 * the q of blocks awaiting i/o completion status.
	 */
	if ((flag&B_ASYNC) == 0) {
		biowait(bp);
		brelse(bp);
	} else if (flag & B_DELWRI)
		bp->b_flags |= B_AGE;
}

/*
 * Release the buffer, marking it so that if it is grabbed
 * for another purpose it will be written out before being
 * given up (e.g. when writing a partial block where it is
 * assumed that another write for the same block will soon follow).
 * This can't be done for magtape, since writes must be done
 * in the same order as requested.
 */
bdwrite(bp)
	register struct buf *bp;
{

	if ((bp->b_flags&B_DELWRI) == 0)
		u.u_ru.ru_oublock++;		/* noone paid yet */
	if (bdevsw[major(bp->b_dev)].d_flags & B_TAPE)
		bawrite(bp);
	else {
		bp->b_flags |= B_DELWRI | B_DONE;
		brelse(bp);
	}
}

/*
 * Release the buffer, start I/O on it, but don't wait for completion.
 */
bawrite(bp)
	register struct buf *bp;
{

	bp->b_flags |= B_ASYNC;
	bwrite(bp);
}

/*
 * Release the buffer, with no I/O implied.
 */
brelse(bp)
	register struct buf *bp;
{
	register struct buf *flist;
	register s;

	trace(TR_BRELSE, pack(bp->b_dev, bp->b_bufsize), bp->b_blkno);
	/*
	 * If someone's waiting for the buffer, or
	 * is waiting for a buffer wake 'em up.
	 */
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	if (bfreelist[0].b_flags&B_WANTED) {
		bfreelist[0].b_flags &= ~B_WANTED;
		wakeup((caddr_t)bfreelist);
	}
	if (bp->b_flags&B_ERROR)
		if (bp->b_flags & B_LOCKED)
			bp->b_flags &= ~B_ERROR;	/* try again later */
		else
			bp->b_dev = NODEV;  		/* no assoc */

	/*
	 * Stick the buffer back on a free list.
	 */
	s = splbio();
	if (bp->b_bufsize <= 0) {
		/* block has no buffer ... put at front of unused buffer list */
		flist = &bfreelist[BQ_EMPTY];
		binsheadfree(bp, flist);
	} else if (bp->b_flags & (B_ERROR|B_INVAL)) {
		/* block has no info ... put at front of most free list */
		flist = &bfreelist[BQ_AGE];
		binsheadfree(bp, flist);
	} else {
		if (bp->b_flags & B_LOCKED)
			flist = &bfreelist[BQ_LOCKED];
		else if (bp->b_flags & B_AGE)
			flist = &bfreelist[BQ_AGE];
		else
			flist = &bfreelist[BQ_LRU];
		binstailfree(bp, flist);
	}
	bp->b_flags &= ~(B_WANTED|B_BUSY|B_ASYNC|B_AGE);
	splx(s);
}

/*
 * See if the block is associated with some buffer
 * (mainly to avoid getting hung up on a wait in breada)
 */
incore(dev, blkno)
	dev_t dev;
	daddr_t blkno;
{
	register struct buf *bp;
	register struct buf *dp;

	dp = BUFHASH(dev, blkno);
	for (bp = dp->b_forw; bp != dp; bp = bp->b_forw)
		if (bp->b_blkno == blkno && bp->b_dev == dev &&
		    (bp->b_flags & B_INVAL) == 0)
			return (1);
	return (0);
}

struct buf *
#ifdef SECSIZE
baddr(dev, blkno, size, secsize)
#else SECSIZE
baddr(dev, blkno, size)
#endif SECSIZE
	dev_t dev;
	daddr_t blkno;
	int size;
#ifdef SECSIZE
	long secsize;
#endif SECSIZE
{

	if (incore(dev, blkno))
#ifdef SECSIZE
		return (bread(dev, blkno, size, secsize));
#else SECSIZE
		return (bread(dev, blkno, size));
#endif SECSIZE
	return (0);
}

/*
 * Assign a buffer for the given block.  If the appropriate
 * block is already associated, return it; otherwise search
 * for the oldest non-busy buffer and reassign it.
 *
 * If we find the buffer, but it is dirty (marked DELWRI) and
 * its size is changing, we must write it out first. When the
 * buffer is shrinking, the write is done by brealloc to avoid
 * losing the unwritten data. When the buffer is growing, the
 * write is done by getblk, so that bread will not read stale
 * disk data over the modified data in the buffer.
 *
 * We use splx here because this routine may be called
 * on the interrupt stack during a dump, and we don't
 * want to lower the ipl back to 0.
 */
struct buf *
#ifdef SECSIZE
getblk(dev, blkno, size, secsize)
#else SECSIZE
getblk(dev, blkno, size)
#endif SECSIZE
	dev_t dev;
	daddr_t blkno;
	int size;
#ifdef SECSIZE
	long secsize;
#endif SECSIZE
{
	register struct buf *bp, *dp;
	int s;

	if (size > MAXBSIZE)
		panic("getblk: size too big");
	/*
	 * To prevent overflow of 32-bit ints when converting block
	 * numbers to byte offsets, blknos > 2^32 / DEV_BSIZE are set
	 * to the maximum number that can be converted to a byte offset
	 * without overflow. This is historic code; what bug it fixed,
	 * or whether it is still a reasonable thing to do is open to
	 * dispute. mkm 9/85
	 */
	if ((unsigned)blkno >= 1 << (sizeof(int)*NBBY-DEV_BSHIFT))
		blkno = 1 << ((sizeof(int)*NBBY-DEV_BSHIFT) + 1);
	/*
	 * Search the cache for the block.  If we hit, but
	 * the buffer is in use for i/o, then we wait until
	 * the i/o has completed.
	 */
	dp = BUFHASH(dev, blkno);
loop:
	for (bp = dp->b_forw; bp != dp; bp = bp->b_forw) {
		if (bp->b_blkno != blkno || bp->b_dev != dev ||
		    bp->b_flags&B_INVAL)
			continue;
		s = splbio();
		if (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO+1);
			splx(s);
			goto loop;
		}
		splx(s);
		notavail(bp);
		if (bp->b_bcount != size) {
			if (bp->b_bcount < size && (bp->b_flags&B_DELWRI)) {
				bp->b_flags &= ~B_ASYNC;
				bwrite(bp);
				goto loop;
			}
			if (brealloc(bp, size) == 0)
				goto loop;
		}
		if (bp->b_bcount != size && brealloc(bp, size) == 0)
			goto loop;
		bp->b_flags |= B_CACHE;
		return (bp);
	}
	if (major(dev) >= nblkdev)
		panic("blkdev");
	bp = getnewbuf();
	bfree(bp);
	bremhash(bp);
	binshash(bp, dp);
	bp->b_dev = dev;
#ifdef SECSIZE
	bp->b_blksize = secsize;
#endif SECSIZE
	bp->b_blkno = blkno;
	bp->b_error = 0;
	if (brealloc(bp, size) == 0)
		goto loop;
	return (bp);
}

/*
 * get an empty block,
 * not assigned to any particular device
 */
struct buf *
geteblk(size)
	int size;
{
	register struct buf *bp, *flist;

	if (size > MAXBSIZE)
		panic("geteblk: size too big");
loop:
	bp = getnewbuf();
	bp->b_flags |= B_INVAL;
	bfree(bp);
	bremhash(bp);
	flist = &bfreelist[BQ_AGE];
	binshash(bp, flist);
	bp->b_dev = (dev_t)NODEV;
#ifdef SECSIZE
	bp->b_blksize = DEV_BSIZE;
#endif SECSIZE
	bp->b_error = 0;
	if (brealloc(bp, size) == 0)
		goto loop;
	return (bp);
}

/*
 * Allocate space associated with a buffer.
 * If can't get space, buffer is released
 */
brealloc(bp, size)
	register struct buf *bp;
	int size;
{
	daddr_t start, last;
	register struct buf *ep;
	struct buf *dp;
	int s;

	/*
	 * First need to make sure that all overlapping previous I/O
	 * is dispatched with.
	 */
	if (size == bp->b_bcount)
		return (1);
	if (size < bp->b_bcount) { 
		if (bp->b_flags & B_DELWRI) {
			bwrite(bp);
			return (0);
		}
		if (bp->b_flags & B_LOCKED)
			panic("brealloc");
		return (allocbuf(bp, size));
	}
	bp->b_flags &= ~B_DONE;
	if (bp->b_dev == NODEV)
		return (allocbuf(bp, size));

	trace(TR_BREALLOC, pack(bp->b_dev, size), bp->b_blkno);
	/*
	 * Search cache for any buffers that overlap the one that we
	 * are trying to allocate. Overlapping buffers must be marked
	 * invalid, after being written out if they are dirty. (indicated
	 * by B_DELWRI) A disk block must be mapped by at most one buffer
	 * at any point in time. Care must be taken to avoid deadlocking
	 * when two buffer are trying to get the same set of disk blocks.
	 */
	start = bp->b_blkno;
#ifdef SECSIZE
	last = start + size/bp->b_blksize - 1;
#else SECSIZE
	last = start + btodb(size) - 1;
#endif SECSIZE
	dp = BUFHASH(bp->b_dev, bp->b_blkno);
loop:
	for (ep = dp->b_forw; ep != dp; ep = ep->b_forw) {
		if (ep == bp || ep->b_dev != bp->b_dev || (ep->b_flags&B_INVAL))
			continue;
		/* look for overlap */
		if (ep->b_bcount == 0 || ep->b_blkno > last ||
#ifdef SECSIZE
		    ep->b_blkno + ep->b_bcount/ep->b_blksize <= start)
#else SECSIZE
		    ep->b_blkno + btodb(ep->b_bcount) <= start)
#endif SECSIZE
			continue;
		s = splbio();
		if (ep->b_flags&B_BUSY) {
			ep->b_flags |= B_WANTED;
			sleep((caddr_t)ep, PRIBIO+1);
			splx(s);
			goto loop;
		}
		splx(s);
		notavail(ep);
		if (ep->b_flags & B_DELWRI) {
			bwrite(ep);
			goto loop;
		}
		ep->b_flags |= B_INVAL;
		brelse(ep);
	}
	return (allocbuf(bp, size));
}

/*
 * Find a buffer which is available for use.
 * Select something from a free list.
 * Preference is to AGE list, then LRU list.
 */
struct buf *
getnewbuf()
{
	register struct buf *bp, *dp;
	int s;

loop:
	s = splbio();
	for (dp = &bfreelist[BQ_AGE]; dp > bfreelist; dp--)
		if (dp->av_forw != dp)
			break;
	if (dp == bfreelist) {		/* no free blocks */
		dp->b_flags |= B_WANTED;
		sleep((caddr_t)dp, PRIBIO+1);
		splx(s);
		goto loop;
	}
	splx(s);
	bp = dp->av_forw;
	notavail(bp);
	if (bp->b_flags & B_DELWRI) {
		bp->b_flags |= B_ASYNC;
		bwrite(bp);
		goto loop;
	}
	trace(TR_BRELSE, pack(bp->b_dev, bp->b_bufsize), bp->b_blkno);
	bp->b_flags = B_BUSY;
	return (bp);
}

/*
 * Wait for I/O completion on the buffer; return errors
 * to the user.
 */
biowait(bp)
	register struct buf *bp;
{
	int s;

	s = splbio();
	while ((bp->b_flags&B_DONE)==0)
		sleep((caddr_t)bp, PRIBIO);
	splx(s);
	if (u.u_error == 0)			/* XXX */
		u.u_error = geterror(bp);
}

/*
 * Mark I/O complete on a buffer.
 * If someone should be called, e.g. the pageout
 * daemon, do so.  Otherwise, wake up anyone
 * waiting for it.
 */
biodone(bp)
	register struct buf *bp;
{

	if (bp->b_flags & B_DONE)
		panic("dup biodone");
	bp->b_flags |= B_DONE;
	if (bp->b_flags & B_CALL) {
		bp->b_flags &= ~B_CALL;
		(*bp->b_iodone)(bp);
		return;
	}
	if (bp->b_flags&B_ASYNC)
		brelse(bp);
	else {
		bp->b_flags &= ~B_WANTED;
		wakeup((caddr_t)bp);
	}
}

/*
 * Insure that no part of a specified block is in an incore buffer.
#ifdef SECSIZE
 * "size" is given in device blocks (the units of b_blkno).
#endif SECSIZE
#ifdef SECSIZE
 * "size" is given in device blocks (the units of b_blkno).
#endif SECSIZE
 */
blkflush(dev, blkno, size)
	dev_t dev;
	daddr_t blkno;
#ifdef SECSIZE
	int size;
#else SECSIZE
	long size;
#endif SECSIZE
{
	register struct buf *ep;
	struct buf *dp;
	daddr_t start, last;
	int s;

	start = blkno;
#ifdef SECSIZE
	last = start + size - 1;
#else SECSIZE
	last = start + btodb(size) - 1;
#endif SECSIZE
	dp = BUFHASH(dev, blkno);
loop:
	for (ep = dp->b_forw; ep != dp; ep = ep->b_forw) {
		if (ep->b_dev != dev || (ep->b_flags&B_INVAL))
			continue;
		/* look for overlap */
		if (ep->b_bcount == 0 || ep->b_blkno > last ||
#ifdef SECSIZE
		    ep->b_blkno + ep->b_bcount / ep->b_blksize <= start)
#else SECSIZE
		    ep->b_blkno + btodb(ep->b_bcount) <= start)
#endif SECSIZE
			continue;
		s = splbio();
		if (ep->b_flags&B_BUSY) {
			ep->b_flags |= B_WANTED;
			sleep((caddr_t)ep, PRIBIO+1);
			splx(s);
			goto loop;
		}
		if (ep->b_flags & B_DELWRI) {
			splx(s);
			notavail(ep);
			bwrite(ep);
			goto loop;
		}
		splx(s);
	}
}

/*
 * Make sure all write-behind blocks
 * on dev (or NODEV for all)
 * are flushed out.
 * (from umount and update)
 */
bflush(dev)
	dev_t dev;
{
	register struct buf *bp;
	register struct buf *flist;
	int s;

loop:
	s = splbio();
	for (flist = bfreelist; flist < &bfreelist[BQ_EMPTY]; flist++)
	for (bp = flist->av_forw; bp != flist; bp = bp->av_forw) {
		if ((bp->b_flags & B_DELWRI) == 0)
			continue;
		if (dev == NODEV || dev == bp->b_dev) {
			bp->b_flags |= B_ASYNC;
			notavail(bp);
			bwrite(bp);
			splx(s);
			goto loop;
		}
	}
	splx(s);
}

/*
 * Pick up the device's error number and pass it to the user;
 * if there is an error but the number is 0 set a generalized code.
 */
geterror(bp)
	register struct buf *bp;
{
	int error = 0;

	if (bp->b_flags&B_ERROR)
		if ((error = bp->b_error)==0)
			return (EIO);
	return (error);
}

/*
 * Invalidate in core blocks belonging to closed or umounted filesystem
 *
 * This is not nicely done at all - the buffer ought to be removed from the
 * hash chains & have its dev/blkno fields clobbered, but unfortunately we
 * can't do that here, as it is quite possible that the block is still
 * being used for i/o. Eventually, all disc drivers should be forced to
 * have a close routine, which ought ensure that the queue is empty, then
 * properly flush the queues. Until that happy day, this suffices for
 * correctness.						... kre
 */
binval(dev)
	dev_t dev;
{
	register struct buf *bp;
	register struct bufhd *hp;
#define dp ((struct buf *)hp)

	for (hp = bufhash; hp < &bufhash[BUFHSZ]; hp++)
		for (bp = dp->b_forw; bp != dp; bp = bp->b_forw)
			if (bp->b_dev == dev)
				bp->b_flags |= B_INVAL;
}
