/*	vfs_cluster.c	4.31	82/05/31	*/

#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/proc.h"
#include "../h/seg.h"
#include "../h/pte.h"
#include "../h/vm.h"
#include "../h/trace.h"

/*
 * Read in (if necessary) the block and return a buffer pointer.
 */
struct buf *
bread(dev, blkno, size)
	dev_t dev;
	daddr_t blkno;
	int size;
{
	register struct buf *bp;

	bp = getblk(dev, blkno, size);
	if (bp->b_flags&B_DONE) {
		trace(TR_BREADHIT, dev, blkno);
		return(bp);
	}
	bp->b_flags |= B_READ;
	(*bdevsw[major(dev)].d_strategy)(bp);
	trace(TR_BREADMISS, dev, blkno);
	u.u_vm.vm_inblk++;		/* pay for read */
	biowait(bp);
	return(bp);
}

/*
 * Read in the block, like bread, but also start I/O on the
 * read-ahead block (which is not allocated to the caller)
 */
struct buf *
breada(dev, blkno, rablkno, size)
	dev_t dev;
	daddr_t blkno, rablkno;
	int size;
{
	register struct buf *bp, *rabp;

	bp = NULL;
	/*
	 * If the block isn't in core, then allocate
	 * a buffer and initiate i/o (getblk checks
	 * for a cache hit).
	 */
	if (!incore(dev, blkno)) {
		bp = getblk(dev, blkno, size);
		if ((bp->b_flags&B_DONE) == 0) {
			bp->b_flags |= B_READ;
			(*bdevsw[major(dev)].d_strategy)(bp);
			trace(TR_BREADMISS, dev, blkno);
			u.u_vm.vm_inblk++;		/* pay for read */
		} else
			trace(TR_BREADHIT, dev, blkno);
	}

	/*
	 * If there's a read-ahead block, start i/o
	 * on it also (as above).
	 */
	if (rablkno && !incore(dev, rablkno)) {
		rabp = getblk(dev, rablkno, size);
		if (rabp->b_flags & B_DONE) {
			brelse(rabp);
			trace(TR_BREADHITRA, dev, blkno);
		} else {
			rabp->b_flags |= B_READ|B_ASYNC;
			(*bdevsw[major(dev)].d_strategy)(rabp);
			trace(TR_BREADMISSRA, dev, rablock);
			u.u_vm.vm_inblk++;		/* pay in advance */
		}
	}

	/*
	 * If we get here with bp NULL, then the block
	 * must've been in core and bread will find it for us.
	 */
	if(bp == NULL)
		return(bread(dev, blkno, size));
	biowait(bp);
	return(bp);
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
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI | B_AGE);
	if ((flag&B_DELWRI) == 0)
		u.u_vm.vm_oublk++;		/* noone paid yet */
	trace(TR_BWRITE, bp->b_dev, bp->b_blkno);
	(*bdevsw[major(bp->b_dev)].d_strategy)(bp);

	/*
	 * If the write was synchronous, then await i/o completion.
	 * If the write was "delayed", then we put the buffer on
	 * the q of blocks awaiting i/o completion status.
	 * Otherwise, the i/o must be finished and we check for
	 * an error.
	 */
	if ((flag&B_ASYNC) == 0) {
		biowait(bp);
		brelse(bp);
	} else if (flag & B_DELWRI)
		bp->b_flags |= B_AGE;
	else
		geterror(bp);
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
	register int flags;

	if ((bp->b_flags&B_DELWRI) == 0)
		u.u_vm.vm_oublk++;		/* noone paid yet */
	flags = bdevsw[major(bp->b_dev)].d_flags;
	if(flags & B_TAPE)
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
	s = spl6();
	if (bp->b_flags & (B_ERROR|B_INVAL)) {
		/* block has no info ... put at front of most free list */
		flist = &bfreelist[BQUEUES-1];
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
baddr(dev, blkno, size)
	dev_t dev;
	daddr_t blkno;
	int size;
{

	if (incore(dev, blkno))
		return (bread(dev, blkno, size));
	return (0);
}

/*
 * Assign a buffer for the given block.  If the appropriate
 * block is already associated, return it; otherwise search
 * for the oldest non-busy buffer and reassign it.
 *
 * We use splx here because this routine may be called
 * on the interrupt stack during a dump, and we don't
 * want to lower the ipl back to 0.
 */
struct buf *
getblk(dev, blkno, size)
	dev_t dev;
	daddr_t blkno;
	int size;
{
	register struct buf *bp, *dp, *ep;
	int s;

	if ((unsigned)blkno >= 1 << (sizeof(int)*NBBY-PGSHIFT))
		blkno = 1 << ((sizeof(int)*NBBY-PGSHIFT) + 1);
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
		s = spl6();
		if (bp->b_flags&B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO+1);
			splx(s);
			goto loop;
		}
		splx(s);
		notavail(bp);
		brealloc(bp, size);
		bp->b_flags |= B_CACHE;
		return(bp);
	}
	if (major(dev) >= nblkdev)
		panic("blkdev");
	/*
	 * Not found in the cache, select something from
	 * a free list.  Preference is to LRU list, then AGE list.
	 */
	s = spl6();
	for (ep = &bfreelist[BQUEUES-1]; ep > bfreelist; ep--)
		if (ep->av_forw != ep)
			break;
	if (ep == bfreelist) {		/* no free blocks at all */
		ep->b_flags |= B_WANTED;
		sleep((caddr_t)ep, PRIBIO+1);
		splx(s);
		goto loop;
	}
	splx(s);
	bp = ep->av_forw;
	notavail(bp);
	if (bp->b_flags & B_DELWRI) {
		bp->b_flags |= B_ASYNC;
		bwrite(bp);
		goto loop;
	}
	trace(TR_BRELSE, bp->b_dev, bp->b_blkno);
	bp->b_flags = B_BUSY;
	bfree(bp);
	bremhash(bp);
	binshash(bp, dp);
	bp->b_dev = dev;
	bp->b_blkno = blkno;
	brealloc(bp, size);
	return(bp);
}

/*
 * get an empty block,
 * not assigned to any particular device
 */
struct buf *
geteblk(size)
	int size;
{
	register struct buf *bp, *dp;
	int s;

loop:
	s = spl6();
	for (dp = &bfreelist[BQUEUES-1]; dp > bfreelist; dp--)
		if (dp->av_forw != dp)
			break;
	if (dp == bfreelist) {		/* no free blocks */
		dp->b_flags |= B_WANTED;
		sleep((caddr_t)dp, PRIBIO+1);
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
	trace(TR_BRELSE, bp->b_dev, bp->b_blkno);
	bp->b_flags = B_BUSY|B_INVAL;
	bfree(bp);
	bremhash(bp);
	binshash(bp, dp);
	bp->b_dev = (dev_t)NODEV;
	bp->b_bcount = size;
	return(bp);
}

/*
 * Allocate space associated with a buffer.
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
	 * First need to make sure that all overlaping previous I/O
	 * is dispatched with.
	 */
	if (size == bp->b_bcount)
		return;
	if (size < bp->b_bcount) {
		bp->b_bcount = size;
		return;
	}
	start = bp->b_blkno + (bp->b_bcount / DEV_BSIZE);
	last = bp->b_blkno + (size / DEV_BSIZE) - 1;
	if (bp->b_bcount == 0) {
		start++;
		if (start == last)
			goto allocit;
	}
	dp = BUFHASH(bp->b_dev, bp->b_blkno);
loop:
	for (ep = dp->b_forw; ep != dp; ep = ep->b_forw) {
		if (ep->b_blkno < start || ep->b_blkno > last ||
		    ep->b_dev != bp->b_dev || ep->b_flags&B_INVAL)
			continue;
		s = spl6();
		if (ep->b_flags&B_BUSY) {
			ep->b_flags |= B_WANTED;
			sleep((caddr_t)ep, PRIBIO+1);
			(void) splx(s);
			goto loop;
		}
		(void) splx(s);
		/*
		 * What we would really like to do is kill this
		 * I/O since it is now useless. We cannot do that
		 * so we force it to complete, so that it cannot
		 * over-write our useful data later.
		 */
		if (ep->b_flags & B_DELWRI) {
			notavail(ep);
			ep->b_flags |= B_ASYNC;
			bwrite(ep);
			goto loop;
		}
	}
allocit:
	/*
	 * Here the buffer is already available, so all we
	 * need to do is set the size. Someday a better memory
	 * management scheme will be implemented.
	 */
	bp->b_bcount = size;
}

/*
 * Release space associated with a buffer.
 */
bfree(bp)
	struct buf *bp;
{
	/*
	 * Here the buffer does not change, so all we
	 * need to do is set the size. Someday a better memory
	 * management scheme will be implemented.
	 */
	bp->b_bcount = 0;
}

/*
 * Wait for I/O completion on the buffer; return errors
 * to the user.
 */
biowait(bp)
	register struct buf *bp;
{
	int s;

	s = spl6();
	while ((bp->b_flags&B_DONE)==0)
		sleep((caddr_t)bp, PRIBIO);
	splx(s);
	geterror(bp);
}

/*
 * Mark I/O complete on a buffer. If the header
 * indicates a dirty page push completion, the
 * header is inserted into the ``cleaned'' list
 * to be processed by the pageout daemon. Otherwise
 * release it if I/O is asynchronous, and wake 
 * up anyone waiting for it.
 */
biodone(bp)
	register struct buf *bp;
{
	register int s;

	if (bp->b_flags & B_DONE)
		panic("dup biodone");
	bp->b_flags |= B_DONE;
	if (bp->b_flags & B_DIRTY) {
		if (bp->b_flags & B_ERROR)
			panic("IO err in push");
		s = spl6();
		bp->av_forw = bclnlist;
		bp->b_bcount = swsize[bp - swbuf];
		bp->b_pfcent = swpf[bp - swbuf];
		cnt.v_pgout++;
		cnt.v_pgpgout += bp->b_bcount / NBPG;
		bclnlist = bp;
		if (bswlist.b_flags & B_WANTED)
			wakeup((caddr_t)&proc[2]);
		splx(s);
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
 * make sure all write-behind blocks
 * on dev (or NODEV for all)
 * are flushed out.
 * (from umount and update)
 * (and temporarily pagein)
 */
bflush(dev)
	dev_t dev;
{
	register struct buf *bp;
	register struct buf *flist;
	int s;

loop:
	s = spl6();
	for (flist = bfreelist; flist < &bfreelist[BQUEUES]; flist++)
	for (bp = flist->av_forw; bp != flist; bp = bp->av_forw) {
		if ((bp->b_flags & B_DELWRI) == 0)
			continue;
		if (dev == NODEV || dev == bp->b_dev) {
			bp->b_flags |= B_ASYNC;
			notavail(bp);
			bwrite(bp);
			goto loop;
		}
	}
	splx(s);
}

/*
 * Pick up the device's error number and pass it to the user;
 * if there is an error but the number is 0 set a generalized
 * code.  Actually the latter is always true because devices
 * don't yet return specific errors.
 */
geterror(bp)
	register struct buf *bp;
{

	if (bp->b_flags&B_ERROR)
		if ((u.u_error = bp->b_error)==0)
			u.u_error = EIO;
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
