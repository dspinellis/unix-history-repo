/*-
 * Copyright (c) 1982, 1986, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)vfs_cluster.c	7.53 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/trace.h>
#include <sys/resourcevar.h>

/*
 * Initialize buffers and hash links for buffers.
 */
void
bufinit()
{
	register int i;
	register struct buf *bp, *dp;
	register struct bufhd *hp;
	int base, residual;

	for (hp = bufhash, i = 0; i < BUFHSZ; i++, hp++)
		hp->b_forw = hp->b_back = (struct buf *)hp;

	for (dp = bfreelist; dp < &bfreelist[BQUEUES]; dp++) {
		dp->b_forw = dp->b_back = dp->av_forw = dp->av_back = dp;
		dp->b_flags = B_HEAD;
	}
	base = bufpages / nbuf;
	residual = bufpages % nbuf;
	for (i = 0; i < nbuf; i++) {
		bp = &buf[i];
		bp->b_dev = NODEV;
		bp->b_bcount = 0;
		bp->b_rcred = NOCRED;
		bp->b_wcred = NOCRED;
		bp->b_dirtyoff = 0;
		bp->b_dirtyend = 0;
		bp->b_validoff = 0;
		bp->b_validend = 0;
		bp->b_un.b_addr = buffers + i * MAXBSIZE;
		if (i < residual)
			bp->b_bufsize = (base + 1) * CLBYTES;
		else
			bp->b_bufsize = base * CLBYTES;
		binshash(bp, &bfreelist[BQ_AGE]);
		bp->b_flags = B_INVAL;
		dp = bp->b_bufsize ? &bfreelist[BQ_AGE] : &bfreelist[BQ_EMPTY];
		binsheadfree(bp, dp);
	}
}

/*
 * Find the block in the buffer pool.
 * If the buffer is not present, allocate a new buffer and load
 * its contents according to the filesystem fill routine.
 */
bread(vp, blkno, size, cred, bpp)
	struct vnode *vp;
	daddr_t blkno;
	int size;
	struct ucred *cred;
	struct buf **bpp;
#ifdef SECSIZE
	long secsize;
#endif SECSIZE
{
	struct proc *p = curproc;		/* XXX */
	register struct buf *bp;

	if (size == 0)
		panic("bread: size 0");
#ifdef SECSIZE
	bp = getblk(dev, blkno, size, secsize);
#else SECSIZE
	*bpp = bp = getblk(vp, blkno, size);
#endif SECSIZE
	if (bp->b_flags & (B_DONE | B_DELWRI)) {
		trace(TR_BREADHIT, pack(vp, size), blkno);
		return (0);
	}
	bp->b_flags |= B_READ;
	if (bp->b_bcount > bp->b_bufsize)
		panic("bread");
	if (bp->b_rcred == NOCRED && cred != NOCRED) {
		crhold(cred);
		bp->b_rcred = cred;
	}
	VOP_STRATEGY(bp);
	trace(TR_BREADMISS, pack(vp, size), blkno);
	p->p_stats->p_ru.ru_inblock++;		/* pay for read */
	return (biowait(bp));
}

/*
 * Operates like bread, but also starts I/O on the N specified
 * read-ahead blocks.
 */
breadn(vp, blkno, size, rablkno, rabsize, num, cred, bpp)
	struct vnode *vp;
	daddr_t blkno; int size;
#ifdef SECSIZE
	long secsize;
#endif SECSIZE
	daddr_t rablkno[]; int rabsize[];
	int num;
	struct ucred *cred;
	struct buf **bpp;
{
	struct proc *p = curproc;		/* XXX */
	register struct buf *bp, *rabp;
	register int i;

	bp = NULL;
	/*
	 * If the block is not memory resident,
	 * allocate a buffer and start I/O.
	 */
	if (!incore(vp, blkno)) {
		*bpp = bp = getblk(vp, blkno, size);
#endif SECSIZE
		if ((bp->b_flags & (B_DONE | B_DELWRI)) == 0) {
			bp->b_flags |= B_READ;
			if (bp->b_bcount > bp->b_bufsize)
				panic("breadn");
			if (bp->b_rcred == NOCRED && cred != NOCRED) {
				crhold(cred);
				bp->b_rcred = cred;
			}
			VOP_STRATEGY(bp);
			trace(TR_BREADMISS, pack(vp, size), blkno);
			p->p_stats->p_ru.ru_inblock++;	/* pay for read */
		} else {
			trace(TR_BREADHIT, pack(vp, size), blkno);
		}
	}

	/*
	 * If there's read-ahead block(s), start I/O
	 * on them also (as above).
	 */
	for (i = 0; i < num; i++) {
		if (incore(vp, rablkno[i]))
			continue;
		rabp = getblk(vp, rablkno[i], rabsize[i]);
#endif SECSIZE
		if (rabp->b_flags & (B_DONE | B_DELWRI)) {
			brelse(rabp);
			trace(TR_BREADHITRA, pack(vp, rabsize[i]), rablkno[i]);
		} else {
			rabp->b_flags |= B_ASYNC | B_READ;
			if (rabp->b_bcount > rabp->b_bufsize)
				panic("breadrabp");
			if (rabp->b_rcred == NOCRED && cred != NOCRED) {
				crhold(cred);
				rabp->b_rcred = cred;
			}
			VOP_STRATEGY(rabp);
			trace(TR_BREADMISSRA, pack(vp, rabsize[i]), rablkno[i]);
			p->p_stats->p_ru.ru_inblock++;	/* pay in advance */
		}
	}

	/*
	 * If block was memory resident, let bread get it.
	 * If block was not memory resident, the read was
	 * started above, so just wait for the read to complete.
	 */
	if (bp == NULL)
#ifdef SECSIZE
		return (bread(dev, blkno, size, secsize));
#else SECSIZE
		return (bread(vp, blkno, size, cred, bpp));
	return (biowait(bp));
}

/*
 * Synchronous write.
 * Release buffer on completion.
 */
bwrite(bp)
	register struct buf *bp;
{
	struct proc *p = curproc;		/* XXX */
	register int flag;
	int s, error = 0;

	flag = bp->b_flags;
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI);
	if (flag & B_ASYNC) {
		if ((flag & B_DELWRI) == 0)
			p->p_stats->p_ru.ru_oublock++;	/* no one paid yet */
		else
			reassignbuf(bp, bp->b_vp);
	}
	trace(TR_BWRITE, pack(bp->b_vp, bp->b_bcount), bp->b_lblkno);
	if (bp->b_bcount > bp->b_bufsize)
		panic("bwrite");
	s = splbio();
	bp->b_vp->v_numoutput++;
	splx(s);
	VOP_STRATEGY(bp);

	/*
	 * If the write was synchronous, then await I/O completion.
	 * If the write was "delayed", then we put the buffer on
	 * the queue of blocks awaiting I/O completion status.
	 */
	if ((flag & B_ASYNC) == 0) {
		error = biowait(bp);
		if ((flag&B_DELWRI) == 0)
			p->p_stats->p_ru.ru_oublock++;	/* no one paid yet */
		else
			reassignbuf(bp, bp->b_vp);
		brelse(bp);
	} else if (flag & B_DELWRI) {
		s = splbio();
		bp->b_flags |= B_AGE;
		splx(s);
	}
	return (error);
}

int
vn_bwrite(ap)
	struct vop_bwrite_args *ap;
{
	return bwrite (ap->a_bp);
}


/*
 * Delayed write.
 *
 * The buffer is marked dirty, but is not queued for I/O.
 * This routine should be used when the buffer is expected
 * to be modified again soon, typically a small write that
 * partially fills a buffer.
 *
 * NB: magnetic tapes cannot be delayed; they must be
 * written in the order that the writes are requested.
 */
bdwrite(bp)
	register struct buf *bp;
{
	struct proc *p = curproc;		/* XXX */

	if ((bp->b_flags & B_DELWRI) == 0) {
		bp->b_flags |= B_DELWRI;
		reassignbuf(bp, bp->b_vp);
		p->p_stats->p_ru.ru_oublock++;		/* no one paid yet */
	}
	/*
	 * If this is a tape drive, the write must be initiated.
	 */
	if (bdevsw[major(bp->b_dev)].d_flags & B_TAPE)
		bawrite(bp);
	} else {
		bp->b_flags |= (B_DONE | B_DELWRI);
		brelse(bp);
	}
}

/*
 * Asynchronous write.
 * Start I/O on a buffer, but do not wait for it to complete.
 * The buffer is released when the I/O completes.
 */
bawrite(bp)
	register struct buf *bp;
{

	/*
	 * Setting the ASYNC flag causes bwrite to return
	 * after starting the I/O.
	 */
	bp->b_flags |= B_ASYNC;
	(void) bwrite(bp);
}

/*
 * Release a buffer.
 * Even if the buffer is dirty, no I/O is started.
 */
brelse(bp)
	register struct buf *bp;
{
	register struct buf *flist;
	int s;

	trace(TR_BRELSE, pack(bp->b_vp, bp->b_bufsize), bp->b_lblkno);
	/*
	 * If a process is waiting for the buffer, or
	 * is waiting for a free buffer, awaken it.
	 */
	if (bp->b_flags & B_WANTED)
		wakeup((caddr_t)bp);
	if (bfreelist[0].b_flags & B_WANTED) {
		bfreelist[0].b_flags &= ~B_WANTED;
		wakeup((caddr_t)bfreelist);
	}
	/*
	 * Retry I/O for locked buffers rather than invalidating them.
	 */
	s = splbio();
	if ((bp->b_flags & B_ERROR) && (bp->b_flags & B_LOCKED))
		bp->b_flags &= ~B_ERROR;
	/*
	 * Disassociate buffers that are no longer valid.
	 */
	if (bp->b_flags & (B_NOCACHE | B_ERROR))
		bp->b_flags |= B_INVAL;
	if ((bp->b_bufsize <= 0) || (bp->b_flags & (B_ERROR | B_INVAL))) {
		if (bp->b_vp)
			brelvp(bp);
		bp->b_flags &= ~B_DELWRI;
	}
	/*
	 * Stick the buffer back on a free list.
	 */
	if (bp->b_bufsize <= 0) {
		/* block has no buffer ... put at front of unused buffer list */
		flist = &bfreelist[BQ_EMPTY];
		binsheadfree(bp, flist);
	} else if (bp->b_flags & (B_ERROR | B_INVAL)) {
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
	bp->b_flags &= ~(B_WANTED | B_BUSY | B_ASYNC | B_AGE | B_NOCACHE);
	splx(s);
}

/*
 * Check to see if a block is currently memory resident.
 */
incore(vp, blkno)
	struct vnode *vp;
	daddr_t blkno;
{
	register struct buf *bp;
	register struct buf *dp;

	dp = BUFHASH(vp, blkno);
	for (bp = dp->b_forw; bp != dp; bp = bp->b_forw)
		if (bp->b_lblkno == blkno && bp->b_vp == vp &&
		    (bp->b_flags & B_INVAL) == 0)
			return (1);
	return (0);
}

/*
 * Check to see if a block is currently memory resident.
 * If it is resident, return it. If it is not resident,
 * allocate a new buffer and assign it to the block.
 */
struct buf *
#ifdef SECSIZE
getblk(dev, blkno, size, secsize)
#else SECSIZE
getblk(vp, blkno, size)
	register struct vnode *vp;
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
	 * Search the cache for the block. If the buffer is found,
	 * but it is currently locked, the we must wait for it to
	 * become available.
	 */
	dp = BUFHASH(vp, blkno);
loop:
	for (bp = dp->b_forw; bp != dp; bp = bp->b_forw) {
		if (bp->b_lblkno != blkno || bp->b_vp != vp ||
		    (bp->b_flags & B_INVAL))
			continue;
		s = splbio();
		if (bp->b_flags & B_BUSY) {
			bp->b_flags |= B_WANTED;
			sleep((caddr_t)bp, PRIBIO + 1);
			splx(s);
			goto loop;
		}
		bremfree(bp);
		bp->b_flags |= B_BUSY;
		splx(s);
		if (bp->b_bcount != size) {
			printf("getblk: stray size");
			bp->b_flags |= B_INVAL;
			bwrite(bp);
			goto loop;
		}
		bp->b_flags |= B_CACHE;
		return (bp);
	}
	bp = getnewbuf();
	bremhash(bp);
	bgetvp(vp, bp);
	bp->b_bcount = 0;
	bp->b_lblkno = blkno;
#ifdef SECSIZE
	bp->b_blksize = secsize;
#endif SECSIZE
	bp->b_blkno = blkno;
	bp->b_error = 0;
	bp->b_resid = 0;
	binshash(bp, dp);
	allocbuf(bp, size);
	return (bp);
}

/*
 * Allocate a buffer.
 * The caller will assign it to a block.
 */
struct buf *
geteblk(size)
	int size;
{
	register struct buf *bp, *flist;

	if (size > MAXBSIZE)
		panic("geteblk: size too big");
	bp = getnewbuf();
	bp->b_flags |= B_INVAL;
	bremhash(bp);
	flist = &bfreelist[BQ_AGE];
	bp->b_bcount = 0;
#ifdef SECSIZE
	bp->b_blksize = DEV_BSIZE;
#endif SECSIZE
	bp->b_error = 0;
	bp->b_resid = 0;
	binshash(bp, flist);
	allocbuf(bp, size);
	return (bp);
}

/*
 * Expand or contract the actual memory allocated to a buffer.
 * If no memory is available, release buffer and take error exit.
 */
allocbuf(tp, size)
	register struct buf *tp;
	int size;
{
	register struct buf *bp, *ep;
	int sizealloc, take, s;

	sizealloc = roundup(size, CLBYTES);
	/*
	 * Buffer size does not change
	 */
	if (sizealloc == tp->b_bufsize)
		goto out;
	/*
	 * Buffer size is shrinking.
	 * Place excess space in a buffer header taken from the
	 * BQ_EMPTY buffer list and placed on the "most free" list.
	 * If no extra buffer headers are available, leave the
	 * extra space in the present buffer.
	 */
	if (sizealloc < tp->b_bufsize) {
		ep = bfreelist[BQ_EMPTY].av_forw;
		if (ep == &bfreelist[BQ_EMPTY])
			goto out;
		s = splbio();
		bremfree(ep);
		ep->b_flags |= B_BUSY;
		splx(s);
		pagemove(tp->b_un.b_addr + sizealloc, ep->b_un.b_addr,
		    (int)tp->b_bufsize - sizealloc);
		ep->b_bufsize = tp->b_bufsize - sizealloc;
		tp->b_bufsize = sizealloc;
		ep->b_flags |= B_INVAL;
		ep->b_bcount = 0;
		brelse(ep);
		goto out;
	}
	/*
	 * More buffer space is needed. Get it out of buffers on
	 * the "most free" list, placing the empty headers on the
	 * BQ_EMPTY buffer header list.
	 */
	while (tp->b_bufsize < sizealloc) {
		take = sizealloc - tp->b_bufsize;
		bp = getnewbuf();
		if (take >= bp->b_bufsize)
			take = bp->b_bufsize;
		pagemove(&bp->b_un.b_addr[bp->b_bufsize - take],
		    &tp->b_un.b_addr[tp->b_bufsize], take);
		tp->b_bufsize += take;
		bp->b_bufsize = bp->b_bufsize - take;
		if (bp->b_bcount > bp->b_bufsize)
			bp->b_bcount = bp->b_bufsize;
		if (bp->b_bufsize <= 0) {
			bremhash(bp);
			binshash(bp, &bfreelist[BQ_EMPTY]);
			bp->b_dev = NODEV;
			bp->b_error = 0;
			bp->b_flags |= B_INVAL;
		}
		brelse(bp);
	}
out:
	tp->b_bcount = size;
	return (1);
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
	register struct ucred *cred;
	int s;

loop:
	s = splbio();
	for (dp = &bfreelist[BQ_AGE]; dp > bfreelist; dp--)
		if (dp->av_forw != dp)
			break;
	if (dp == bfreelist) {		/* no free blocks */
		dp->b_flags |= B_WANTED;
		sleep((caddr_t)dp, PRIBIO + 1);
		splx(s);
		goto loop;
	}
	bp = dp->av_forw;
	bremfree(bp);
	bp->b_flags |= B_BUSY;
	splx(s);
	if (bp->b_flags & B_DELWRI) {
		(void) bawrite(bp);
		goto loop;
	}
	trace(TR_BRELSE, pack(bp->b_vp, bp->b_bufsize), bp->b_lblkno);
	if (bp->b_vp)
		brelvp(bp);
	if (bp->b_rcred != NOCRED) {
		cred = bp->b_rcred;
		bp->b_rcred = NOCRED;
		crfree(cred);
	}
	if (bp->b_wcred != NOCRED) {
		cred = bp->b_wcred;
		bp->b_wcred = NOCRED;
		crfree(cred);
	}
	bp->b_flags = B_BUSY;
	bp->b_dirtyoff = bp->b_dirtyend = 0;
	bp->b_validoff = bp->b_validend = 0;
	return (bp);
}

/*
 * Wait for I/O to complete.
 *
 * Extract and return any errors associated with the I/O.
 * If the error flag is set, but no specific error is
 * given, return EIO.
 */
biowait(bp)
	register struct buf *bp;
{
	int s;

	s = splbio();
	while ((bp->b_flags & B_DONE) == 0)
		sleep((caddr_t)bp, PRIBIO);
	splx(s);
	if ((bp->b_flags & B_ERROR) == 0)
		return (0);
	if (bp->b_error)
		return (bp->b_error);
	return (EIO);
}

/*
 * Mark I/O complete on a buffer.
 *
 * If a callback has been requested, e.g. the pageout
 * daemon, do so. Otherwise, awaken waiting processes.
 */
void
biodone(bp)
	register struct buf *bp;
{

	if (bp->b_flags & B_DONE)
		panic("dup biodone");
	bp->b_flags |= B_DONE;
	if ((bp->b_flags & B_READ) == 0)
		vwakeup(bp);
	if (bp->b_flags & B_CALL) {
		bp->b_flags &= ~B_CALL;
		(*bp->b_iodone)(bp);
		return;
	}
	if (bp->b_flags & B_ASYNC)
		brelse(bp);
	else {
		bp->b_flags &= ~B_WANTED;
		wakeup((caddr_t)bp);
	}
}
