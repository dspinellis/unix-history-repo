/*-
 * Copyright (c) 1986, 1989, 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Berkeley Software Design Inc.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_bio.c	8.5 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/trace.h>
#include <sys/malloc.h>
#include <sys/resourcevar.h>
#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>

/*
 * Definitions for the buffer hash lists.
 */
#define	BUFHASH(dvp, lbn)	\
	(&bufhashtbl[((int)(dvp) / sizeof(*(dvp)) + (int)(lbn)) & bufhash])
LIST_HEAD(bufhashhdr, buf) *bufhashtbl, invalhash;
u_long	bufhash;

/*
 * Insq/Remq for the buffer hash lists.
 */
#define	binshash(bp, dp)	LIST_INSERT_HEAD(dp, bp, b_hash)
#define	bremhash(bp)		LIST_REMOVE(bp, b_hash)

/*
 * Definitions for the buffer free lists.
 */
#define	BQUEUES		4		/* number of free buffer queues */

#define	BQ_LOCKED	0		/* super-blocks &c */
#define	BQ_LRU		1		/* lru, useful buffers */
#define	BQ_AGE		2		/* rubbish */
#define	BQ_EMPTY	3		/* buffer headers with no memory */

TAILQ_HEAD(bqueues, buf) bufqueues[BQUEUES];
int needbuffer;

/*
 * Insq/Remq for the buffer free lists.
 */
#define	binsheadfree(bp, dp)	TAILQ_INSERT_HEAD(dp, bp, b_freelist)
#define	binstailfree(bp, dp)	TAILQ_INSERT_TAIL(dp, bp, b_freelist)

void
bremfree(bp)
	struct buf *bp;
{
	struct bqueues *dp = NULL;

	/*
	 * We only calculate the head of the freelist when removing
	 * the last element of the list as that is the only time that
	 * it is needed (e.g. to reset the tail pointer).
	 *
	 * NB: This makes an assumption about how tailq's are implemented.
	 */
	if (bp->b_freelist.tqe_next == NULL) {
		for (dp = bufqueues; dp < &bufqueues[BQUEUES]; dp++)
			if (dp->tqh_last == &bp->b_freelist.tqe_next)
				break;
		if (dp == &bufqueues[BQUEUES])
			panic("bremfree: lost tail");
	}
	TAILQ_REMOVE(dp, bp, b_freelist);
}

/*
 * Initialize buffers and hash links for buffers.
 */
void
bufinit()
{
	register struct buf *bp;
	struct bqueues *dp;
	register int i;
	int base, residual;

	for (dp = bufqueues; dp < &bufqueues[BQUEUES]; dp++)
		TAILQ_INIT(dp);
	bufhashtbl = hashinit(nbuf, M_CACHE, &bufhash);
	base = bufpages / nbuf;
	residual = bufpages % nbuf;
	for (i = 0; i < nbuf; i++) {
		bp = &buf[i];
		bzero((char *)bp, sizeof *bp);
		bp->b_dev = NODEV;
		bp->b_rcred = NOCRED;
		bp->b_wcred = NOCRED;
		bp->b_vnbufs.le_next = NOLIST;
		bp->b_data = buffers + i * MAXBSIZE;
		if (i < residual)
			bp->b_bufsize = (base + 1) * CLBYTES;
		else
			bp->b_bufsize = base * CLBYTES;
		bp->b_flags = B_INVAL;
		dp = bp->b_bufsize ? &bufqueues[BQ_AGE] : &bufqueues[BQ_EMPTY];
		binsheadfree(bp, dp);
		binshash(bp, &invalhash);
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
	*bpp = bp = getblk(vp, blkno, size, 0, 0);
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
		*bpp = bp = getblk(vp, blkno, size, 0, 0);
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
		rabp = getblk(vp, rablkno[i], rabsize[i], 0, 0);
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
	bp->b_flags |= B_WRITEINPROG;
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
		if (bp->b_flags & B_EINTR) {
			bp->b_flags &= ~B_EINTR;
			error = EINTR;
		}
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
	return (bwrite(ap->a_bp));
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
	(void) VOP_BWRITE(bp);
}

/*
 * Release a buffer.
 * Even if the buffer is dirty, no I/O is started.
 */
brelse(bp)
	register struct buf *bp;
{
	register struct bqueues *flist;
	int s;

	trace(TR_BRELSE, pack(bp->b_vp, bp->b_bufsize), bp->b_lblkno);
	/*
	 * If a process is waiting for the buffer, or
	 * is waiting for a free buffer, awaken it.
	 */
	if (bp->b_flags & B_WANTED)
		wakeup((caddr_t)bp);
	if (needbuffer) {
		needbuffer = 0;
		wakeup((caddr_t)&needbuffer);
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
		flist = &bufqueues[BQ_EMPTY];
		binsheadfree(bp, flist);
	} else if (bp->b_flags & (B_ERROR | B_INVAL)) {
		/* block has no info ... put at front of most free list */
		flist = &bufqueues[BQ_AGE];
		binsheadfree(bp, flist);
	} else {
		if (bp->b_flags & B_LOCKED)
			flist = &bufqueues[BQ_LOCKED];
		else if (bp->b_flags & B_AGE)
			flist = &bufqueues[BQ_AGE];
		else
			flist = &bufqueues[BQ_LRU];
		binstailfree(bp, flist);
	}
	bp->b_flags &= ~(B_WANTED | B_BUSY | B_ASYNC | B_AGE | B_NOCACHE);
	splx(s);
}

/*
 * Check to see if a block is currently memory resident.
 */
struct buf *
incore(vp, blkno)
	struct vnode *vp;
	daddr_t blkno;
{
	register struct buf *bp;

	for (bp = BUFHASH(vp, blkno)->lh_first; bp; bp = bp->b_hash.le_next)
		if (bp->b_lblkno == blkno && bp->b_vp == vp &&
		    (bp->b_flags & B_INVAL) == 0)
			return (bp);
	return (NULL);
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
getblk(vp, blkno, size, slpflag, slptimeo)
	register struct vnode *vp;
	daddr_t blkno;
	int size, slpflag, slptimeo;
#ifdef SECSIZE
	long secsize;
#endif SECSIZE
{
	register struct buf *bp;
	struct bufhashhdr *dp;
	int s, error;

	if (size > MAXBSIZE)
		panic("getblk: size too big");
	/*
	 * Search the cache for the block. If the buffer is found,
	 * but it is currently locked, the we must wait for it to
	 * become available.
	 */
	dp = BUFHASH(vp, blkno);
loop:
	for (bp = dp->lh_first; bp; bp = bp->b_hash.le_next) {
		if (bp->b_lblkno != blkno || bp->b_vp != vp)
			continue;
		s = splbio();
		if (bp->b_flags & B_BUSY) {
			bp->b_flags |= B_WANTED;
			error = tsleep((caddr_t)bp, slpflag | (PRIBIO + 1),
				"getblk", slptimeo);
			splx(s);
			if (error)
				return (NULL);
			goto loop;
		}
		/*
		 * The test for B_INVAL is moved down here, since there
		 * are cases where B_INVAL is set before VOP_BWRITE() is
		 * called and for NFS, the process cannot be allowed to
		 * allocate a new buffer for the same block until the write
		 * back to the server has been completed. (ie. B_BUSY clears)
		 */
		if (bp->b_flags & B_INVAL) {
			splx(s);
			continue;
		}
		bremfree(bp);
		bp->b_flags |= B_BUSY;
		splx(s);
		if (bp->b_bcount != size) {
			printf("getblk: stray size");
			bp->b_flags |= B_INVAL;
			VOP_BWRITE(bp);
			goto loop;
		}
		bp->b_flags |= B_CACHE;
		return (bp);
	}
	/*
	 * The loop back to the top when getnewbuf() fails is because
	 * stateless filesystems like NFS have no node locks. Thus,
	 * there is a slight chance that more than one process will
	 * try and getnewbuf() for the same block concurrently when
	 * the first sleeps in getnewbuf(). So after a sleep, go back
	 * up to the top to check the hash lists again.
	 */
	if ((bp = getnewbuf(slpflag, slptimeo)) == 0)
		goto loop;
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
	register struct buf *bp;

	if (size > MAXBSIZE)
		panic("geteblk: size too big");
	while ((bp = getnewbuf(0, 0)) == NULL)
		/* void */;
	bp->b_flags |= B_INVAL;
	bremhash(bp);
	binshash(bp, &invalhash);
	bp->b_bcount = 0;
#ifdef SECSIZE
	bp->b_blksize = DEV_BSIZE;
#endif SECSIZE
	bp->b_error = 0;
	bp->b_resid = 0;
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
		if ((ep = bufqueues[BQ_EMPTY].tqh_first) == NULL)
			goto out;
		s = splbio();
		bremfree(ep);
		ep->b_flags |= B_BUSY;
		splx(s);
		pagemove((char *)tp->b_data + sizealloc, ep->b_data,
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
		while ((bp = getnewbuf(0, 0)) == NULL)
			/* void */;
		if (take >= bp->b_bufsize)
			take = bp->b_bufsize;
		pagemove(&((char *)bp->b_data)[bp->b_bufsize - take],
		    &((char *)tp->b_data)[tp->b_bufsize], take);
		tp->b_bufsize += take;
		bp->b_bufsize = bp->b_bufsize - take;
		if (bp->b_bcount > bp->b_bufsize)
			bp->b_bcount = bp->b_bufsize;
		if (bp->b_bufsize <= 0) {
			bremhash(bp);
			binshash(bp, &invalhash);
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
getnewbuf(slpflag, slptimeo)
	int slpflag, slptimeo;
{
	register struct buf *bp;
	register struct bqueues *dp;
	register struct ucred *cred;
	int s;
	struct buf *abp;
	static int losecnt = 0;

loop:
	s = splbio();
	abp = NULL;
	for (dp = &bufqueues[BQ_AGE]; dp > bufqueues; dp--) {
		for (bp = dp->qe_next; bp; bp = bp->b_freelist.qe_next) {
			if (abp == NULL)
				abp = bp;
			if ((bp->b_flags & B_DELWRI) &&
			    bp->b_vp && VOP_ISLOCKED(bp->b_vp))
				continue;
			goto found;
		}
	}
	if (dp == bufqueues) {		/* no free blocks */
		if (abp) {
			bp = abp;
			bp->b_flags |= B_XXX;
			if (losecnt++ < 20) {
				vprint("skipping blkno check", bp->b_vp);
				printf("\tlblkno %d, blkno %d\n",
				   bp->b_lblkno, bp->b_blkno);
			}
			goto found;
		}
		needbuffer = 1;
		(void) tsleep((caddr_t)&needbuffer, slpflag | (PRIBIO + 1),
			"getnewbuf", slptimeo);
		splx(s);
		return (NULL);
	}
found:
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

int
count_lock_queue()
{
	register struct buf *bp;
	register int ret;

	for (ret = 0, bp = (struct buf *)bufqueues[BQ_LOCKED].tqh_first;
	    bp; bp = (struct buf *)bp->b_freelist.tqe_next)
		++ret;
	return(ret);
}

#ifdef DIAGNOSTIC
/*
 * Print out statistics on the current allocation of the buffer pool.
 * Can be enabled to print out on every ``sync'' by setting "syncprt"
 * in vfs_syscalls.c using sysctl.
 */
void
vfs_bufstats()
{
	int s, i, j, count;
	register struct buf *bp;
	register struct bqueues *dp;
	int counts[MAXBSIZE/CLBYTES+1];
	static char *bname[BQUEUES] = { "LOCKED", "LRU", "AGE", "EMPTY" };

	for (dp = bufqueues, i = 0; dp < &bufqueues[BQUEUES]; dp++, i++) {
		count = 0;
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			counts[j] = 0;
		s = splbio();
		for (bp = dp->tqh_first; bp; bp = bp->b_freelist.tqe_next) {
			counts[bp->b_bufsize/CLBYTES]++;
			count++;
		}
		splx(s);
		printf("%s: total-%d", bname[i], count);
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			if (counts[j] != 0)
				printf(", %d-%d", j * CLBYTES, counts[j]);
		printf("\n");
	}
}
#endif /* DIAGNOSTIC */
