/*-
 * Copyright (c) 1982, 1986, 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 *
 *	@(#)vfs_bio.c	7.57 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/trace.h>
#include <sys/resourcevar.h>
#include <sys/malloc.h>
#include <libkern/libkern.h>

/*
 * Definitions for the buffer hash lists.
 */
#define	BUFHASH(dvp, lbn)	\
	(&bufhashtbl[((int)(dvp) / sizeof(*(dvp)) + (int)(lbn)) & bufhash])
struct	list_entry *bufhashtbl, invalhash;
u_long	bufhash;

/*
 * Insq/Remq for the buffer hash lists.
 */
#define	binshash(bp, dp)	list_enter_head(dp, bp, struct buf *, b_hash)
#define	bremhash(bp)		list_remove(bp, struct buf *, b_hash)

/*
 * Definitions for the buffer free lists.
 */
#define	BQUEUES		4		/* number of free buffer queues */

#define	BQ_LOCKED	0		/* super-blocks &c */
#define	BQ_LRU		1		/* lru, useful buffers */
#define	BQ_AGE		2		/* rubbish */
#define	BQ_EMPTY	3		/* buffer headers with no memory */

struct queue_entry bufqueues[BQUEUES];
int needbuffer;

/*
 * Insq/Remq for the buffer free lists.
 */
#define	binsheadfree(bp, dp) \
	queue_enter_head(dp, bp, struct buf *, b_freelist)
#define	binstailfree(bp, dp) \
	queue_enter_tail(dp, bp, struct buf *, b_freelist)

/*
 * Local declarations
 */
struct buf *cluster_newbuf __P((struct vnode *, struct buf *, long, daddr_t,
	    daddr_t, long, int));
struct buf *cluster_rbuild __P((struct vnode *, u_quad_t, struct buf *,
	    daddr_t, daddr_t, long, int, long));
void	    cluster_wbuild __P((struct vnode *, struct buf *, long size,
	    daddr_t start_lbn, int len, daddr_t lbn));

void
bremfree(bp)
	struct buf *bp;
{
	struct queue_entry *dp;

	/*
	 * We only calculate the head of the freelist when removing
	 * the last element of the list as that is the only time that
	 * it is needed (e.g. to reset the tail pointer).
	 */
	if (bp->b_freelist.qe_next == NULL) {
		for (dp = bufqueues; dp < &bufqueues[BQUEUES]; dp++)
			if (dp->qe_prev == &bp->b_freelist.qe_next)
				break;
		if (dp == &bufqueues[BQUEUES])
			panic("bremfree: lost tail");
	}
	queue_remove(dp, bp, struct buf *, b_freelist);
}

/*
 * Initialize buffers and hash links for buffers.
 */
void
bufinit()
{
	register struct buf *bp;
	struct queue_entry *dp;
	register int i;
	int base, residual;

	for (dp = bufqueues; dp < &bufqueues[BQUEUES]; dp++)
		queue_init(dp);
	bufhashtbl = (struct list_entry *)hashinit(nbuf, M_CACHE, &bufhash);
	base = bufpages / nbuf;
	residual = bufpages % nbuf;
	for (i = 0; i < nbuf; i++) {
		bp = &buf[i];
		bzero((char *)bp, sizeof *bp);
		bp->b_dev = NODEV;
		bp->b_rcred = NOCRED;
		bp->b_wcred = NOCRED;
		bp->b_un.b_addr = buffers + i * MAXBSIZE;
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
 * We could optimize this by keeping track of where the last read-ahead
 * was, but it would involve adding fields to the vnode.  For now, let's
 * just get it working.
 *
 * This replaces bread.  If this is a bread at the beginning of a file and
 * lastr is 0, we assume this is the first read and we'll read up to two
 * blocks if they are sequential.  After that, we'll do regular read ahead
 * in clustered chunks.
 *
 * There are 4 or 5 cases depending on how you count:
 *	Desired block is in the cache:
 *	    1 Not sequential access (0 I/Os).
 *	    2 Access is sequential, do read-ahead (1 ASYNC).
 *	Desired block is not in cache:
 *	    3 Not sequential access (1 SYNC).
 *	    4 Sequential access, next block is contiguous (1 SYNC).
 *	    5 Sequential access, next block is not contiguous (1 SYNC, 1 ASYNC)
 *
 * There are potentially two buffers that require I/O.
 * 	bp is the block requested.
 *	rbp is the read-ahead block.
 *	If either is NULL, then you don't have to do the I/O.
 */
cluster_read(vp, filesize, lblkno, size, cred, bpp)
	struct vnode *vp;
	u_quad_t filesize;
	daddr_t lblkno;
	long size;
	struct ucred *cred;
	struct buf **bpp;
{
	struct buf *bp, *rbp;
	daddr_t blkno, ioblkno;
	long flags;
	int error, num_ra, alreadyincore;

#ifdef DIAGNOSTIC
	if (size == 0)
		panic("cluster_read: size = 0");
#endif

	error = 0;
	flags = B_READ;
	*bpp = bp = getblk(vp, lblkno, size);
	if (bp->b_flags & (B_CACHE | B_DONE | B_DELWRI)) {
		/*
		 * Desired block is in cache; do any readahead ASYNC.
		 * Case 1, 2.
		 */
		trace(TR_BREADHIT, pack(vp, size), lblkno);
		flags |= B_ASYNC;
		ioblkno = lblkno +
		    (lblkno < vp->v_ralen ? vp->v_ralen >> 1 : vp->v_ralen);
		alreadyincore = incore(vp, ioblkno);
		bp = NULL;
	} else {
		/* Block wasn't in cache, case 3, 4, 5. */
		trace(TR_BREADMISS, pack(vp, size), lblkno);
		ioblkno = lblkno;
		bp->b_flags |= flags;
		alreadyincore = 0;
		curproc->p_stats->p_ru.ru_inblock++;		/* XXX */
	}
	/*
	 * XXX
	 * Replace 1 with a window size based on some permutation of
	 * maxcontig and rot_delay.  This will let you figure out how
	 * many blocks you should read-ahead (case 2, 4, 5).
	 *
	 * If the access isn't sequential, cut the window size in half.
	 */
	rbp = NULL;
	if (lblkno != vp->v_lastr + 1 && lblkno != 0)
		vp->v_ralen = max(vp->v_ralen >> 1, 1);
	else if ((ioblkno + 1) * size < filesize && !alreadyincore &&
	    !(error = VOP_BMAP(vp, ioblkno, NULL, &blkno, &num_ra))) {
		/*
		 * Reading sequentially, and the next block is not in the
		 * cache.  We are going to try reading ahead. If this is
		 * the first read of a file, then limit read-ahead to a
		 * single block, else read as much as we're allowed.
		 */
		if (num_ra > vp->v_ralen) {
			num_ra = vp->v_ralen;
			vp->v_ralen = min(MAXPHYS / size, vp->v_ralen << 1);
		} else 
			vp->v_ralen = num_ra + 1;


		if (num_ra)				/* case 2, 4 */
			rbp = cluster_rbuild(vp, filesize,
			    bp, ioblkno, blkno, size, num_ra, flags);
		else if (lblkno != 0 && ioblkno == lblkno) {
			/* Case 5: check how many blocks to read ahead */
			++ioblkno;
			if ((ioblkno + 1) * size > filesize ||
			    (error = VOP_BMAP(vp,
			    ioblkno, NULL, &blkno, &num_ra)))
				goto skip_readahead;
			flags |= B_ASYNC;
			if (num_ra)
				rbp = cluster_rbuild(vp, filesize,
				    NULL, ioblkno, blkno, size, num_ra, flags);
			else {
				rbp = getblk(vp, ioblkno, size);
				rbp->b_flags |= flags;
				rbp->b_blkno = blkno;
			}
		} else if (lblkno != 0) {
			/* case 2; read ahead single block */
			rbp = getblk(vp, ioblkno, size);
			rbp->b_flags |= flags;
			rbp->b_blkno = blkno;
		} else if (bp)				/* case 1, 3, block 0 */
			bp->b_blkno = blkno;
		/* Case 1 on block 0; not really doing sequential I/O */

		if (rbp == bp)		/* case 4 */
			rbp = NULL;
		else if (rbp) {			/* case 2, 5 */
			trace(TR_BREADMISSRA,
			    pack(vp, (num_ra + 1) * size), ioblkno);
			curproc->p_stats->p_ru.ru_inblock++;	/* XXX */
		}
	}

	/* XXX Kirk, do we need to make sure the bp has creds? */
skip_readahead:
	if (bp)
		if (bp->b_flags & (B_DONE | B_DELWRI))
			panic("cluster_read: DONE bp");
		else 
			error = VOP_STRATEGY(bp);

	if (rbp)
		if (error || rbp->b_flags & (B_DONE | B_DELWRI)) {
			rbp->b_flags &= ~(B_ASYNC | B_READ);
			brelse(rbp);
		} else
			(void) VOP_STRATEGY(rbp);

	if (bp)
		return(biowait(bp));
	return(error);
}

/*
 * If blocks are contiguous on disk, use this to provide clustered
 * read ahead.  We will read as many blocks as possible sequentially
 * and then parcel them up into logical blocks in the buffer hash table.
 */
struct buf *
cluster_rbuild(vp, filesize, bp, lbn, blkno, size, run, flags)
	struct vnode *vp;
	u_quad_t filesize;
	struct buf *bp;
	daddr_t lbn;
	daddr_t blkno;
	long size;
	int run;
	long flags;
{
	struct cluster_save *b_save;
	struct buf *tbp;
	daddr_t bn;
	int i, inc;

	if (size * (lbn + run + 1) > filesize)
		--run;
	if (run == 0) {
		if (!bp) {
			bp = getblk(vp, lbn, size);
			bp->b_blkno = blkno;
			bp->b_flags |= flags;
		}
		return(bp);
	}

	bp = cluster_newbuf(vp, bp, flags, blkno, lbn, size, run + 1);
	if (bp->b_flags & (B_DONE | B_DELWRI))
		return (bp);

	b_save = malloc(sizeof(struct buf *) * run + sizeof(struct cluster_save),
	    M_SEGMENT, M_WAITOK);
	b_save->bs_bufsize = b_save->bs_bcount = size;
	b_save->bs_nchildren = 0;
	b_save->bs_children = (struct buf **)(b_save + 1);
	b_save->bs_saveaddr = bp->b_saveaddr;
	bp->b_saveaddr = (caddr_t) b_save;

	inc = size / DEV_BSIZE;
	for (bn = blkno + inc, i = 1; i <= run; ++i, bn += inc) {
		if (incore(vp, lbn + i)) {
			if (i == 1) {
				bp->b_saveaddr = b_save->bs_saveaddr;
				bp->b_flags &= ~B_CALL;
				bp->b_iodone = NULL;
				allocbuf(bp, size);
				free(b_save, M_SEGMENT);
			} else
				allocbuf(bp, size * i);
			break;
		}
		tbp = getblk(vp, lbn + i, 0);
		tbp->b_bcount = tbp->b_bufsize = size;
		tbp->b_blkno = bn;
		tbp->b_flags |= flags | B_READ | B_ASYNC;
		++b_save->bs_nchildren;
		b_save->bs_children[i - 1] = tbp;
	}
	if (!(bp->b_flags & B_ASYNC))
		vp->v_ralen = max(vp->v_ralen - 1, 1);
	return(bp);
}

/*
 * Either get a new buffer or grow the existing one.
 */
struct buf *
cluster_newbuf(vp, bp, flags, blkno, lblkno, size, run)
	struct vnode *vp;
	struct buf *bp;
	long flags;
	daddr_t blkno;
	daddr_t lblkno;
	long size;
	int run;
{
	if (!bp) {
		bp = getblk(vp, lblkno, size);
		if (bp->b_flags & (B_DONE | B_DELWRI)) {
			bp->b_blkno = blkno;
			return(bp);
		}
	}
	allocbuf(bp, run * size);
	bp->b_blkno = blkno;
	bp->b_iodone = cluster_callback;
	bp->b_flags |= flags | B_CALL;
	return(bp);
}

/*
 * Cleanup after a clustered read or write.
 */
void
cluster_callback(bp)
	struct buf *bp;
{
	struct cluster_save *b_save;
	struct buf **tbp;
	long bsize;
	caddr_t cp;

	b_save = (struct cluster_save *)(bp->b_saveaddr);
	bp->b_saveaddr = b_save->bs_saveaddr;

	cp = bp->b_un.b_addr + b_save->bs_bufsize;
	for (tbp = b_save->bs_children; b_save->bs_nchildren--; ++tbp) {
		pagemove(cp, (*tbp)->b_un.b_addr, (*tbp)->b_bufsize);
		cp += (*tbp)->b_bufsize;
		bp->b_bufsize -= (*tbp)->b_bufsize;
		biodone(*tbp);
	}
#ifdef DIAGNOSTIC
	if (bp->b_bufsize != b_save->bs_bufsize)
		panic ("cluster_callback: more space to reclaim");
#endif
	bp->b_bcount = bp->b_bufsize;
	bp->b_iodone = NULL;
	free(b_save, M_SEGMENT);
	if (bp->b_flags & B_ASYNC)
		brelse(bp);
	else
		wakeup((caddr_t)bp);
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
	(void) bwrite(bp);
}

/*
 * Do clustered write for FFS.
 *
 * Three cases:
 *	1. Write is not sequential (write asynchronously)
 *	Write is sequential:
 *	2.	beginning of cluster - begin cluster
 *	3.	middle of a cluster - add to cluster
 *	4.	end of a cluster - asynchronously write cluster
 */
void
cluster_write(bp, filesize)
        struct buf *bp;
	u_quad_t filesize;
{
        struct vnode *vp;
        daddr_t lbn;
        int clen, error, maxrun;

        vp = bp->b_vp;
        lbn = bp->b_lblkno;
	clen = 0;

	/*
	 * Handle end of file first.  If we are appending, we need to check
	 * if the current block was allocated contiguously.  If it wasn't,
	 * then we need to fire off a previous cluster if it existed.
	 * Additionally, when we're appending, we need to figure out how
	 * to initialize vp->v_clen.
	 */
	if ((lbn + 1) * bp->b_bcount == filesize) {
		if (bp->b_blkno != vp->v_lasta + bp->b_bcount / DEV_BSIZE) {
			/* This block was not allocated contiguously */
			if (vp->v_clen)
			    cluster_wbuild(vp, NULL, bp->b_bcount, vp->v_cstart,
				vp->v_lastw - vp->v_cstart + 1, lbn);
			vp->v_cstart = lbn;
			clen = vp->v_clen =
			    MAXBSIZE / vp->v_mount->mnt_stat.f_iosize - 1;
			/*
			 * Next cluster started. Write this buffer and return.
			 */
			vp->v_lastw = lbn;
			vp->v_lasta = bp->b_blkno;
			bdwrite(bp);
			return;
		}
		vp->v_lasta = bp->b_blkno;
	} else if (lbn == 0) {
		vp->v_clen = vp->v_cstart = vp->v_lastw = 0;
	}
        if (vp->v_clen == 0 || lbn != vp->v_lastw + 1) {
		if (vp->v_clen != 0)
			/*
			 * Write is not sequential.
			 */
			cluster_wbuild(vp, NULL, bp->b_bcount, vp->v_cstart,
			    vp->v_lastw - vp->v_cstart + 1, lbn);
		/*
		 * Consider beginning a cluster.
		 */
		if (error = VOP_BMAP(vp, lbn, NULL, &bp->b_blkno, &clen)) {
			bawrite(bp);
			vp->v_cstart = lbn + 1;
			vp->v_lastw = lbn;
			return;
		}
                vp->v_clen = clen;
                if (clen == 0) {		/* I/O not contiguous */
			vp->v_cstart = lbn + 1;
                        bawrite(bp);
                } else {			/* Wait for rest of cluster */
			vp->v_cstart = lbn;
                        bdwrite(bp);
		}
        } else if (lbn == vp->v_cstart + vp->v_clen) {
		/*
		 * At end of cluster, write it out.
		 */
		cluster_wbuild(vp, bp, bp->b_bcount, vp->v_cstart,
		    vp->v_clen + 1, lbn);
		vp->v_clen = 0;
		vp->v_cstart = lbn + 1;
        } else
		/*
		 * In the middle of a cluster, so just delay the
		 * I/O for now.
		 */
                bdwrite(bp);
        vp->v_lastw = lbn;
}


/*
 * This is an awful lot like cluster_rbuild...wish they could be combined.
 * The last lbn argument is the current block on which I/O is being
 * performed.  Check to see that it doesn't fall in the middle of
 * the current block.
 */
void
cluster_wbuild(vp, last_bp, size, start_lbn, len, lbn)
	struct vnode *vp;
	struct buf *last_bp;
	long size;
	daddr_t start_lbn;
	int len;
	daddr_t	lbn;
{
	struct cluster_save *b_save;
	struct buf *bp, *tbp;
	caddr_t	cp;
	int i, s;

redo:
	while ((!incore(vp, start_lbn) || start_lbn == lbn) && len) {
		++start_lbn;
		--len;
	}

	/* Get more memory for current buffer */
	if (len <= 1) {
		if (last_bp)
			bawrite(last_bp);
		return;
	}

	bp = getblk(vp, start_lbn, size);
	if (!(bp->b_flags & B_DELWRI)) {
		++start_lbn;
		--len;
		brelse(bp);
		goto redo;
	}

	--len;
	b_save = malloc(sizeof(struct buf *) * len + sizeof(struct cluster_save),
	    M_SEGMENT, M_WAITOK);
	b_save->bs_bcount = bp->b_bcount;
	b_save->bs_bufsize = bp->b_bufsize;
	b_save->bs_nchildren = 0;
	b_save->bs_children = (struct buf **)(b_save + 1);
	b_save->bs_saveaddr = bp->b_saveaddr;
	bp->b_saveaddr = (caddr_t) b_save;


	bp->b_flags |= B_CALL;
	bp->b_iodone = cluster_callback;
	cp = bp->b_un.b_addr + bp->b_bufsize;
	for (++start_lbn, i = 0; i < len; ++i, ++start_lbn) {
		if (!incore(vp, start_lbn) || start_lbn == lbn)
			break;

		if (last_bp == NULL || start_lbn != last_bp->b_lblkno) {
			tbp = getblk(vp, start_lbn, size);
#ifdef DIAGNOSTIC
			if (tbp->b_bcount != tbp->b_bufsize)
				panic("cluster_wbuild: Buffer too big");
#endif
			if (!(tbp->b_flags & B_DELWRI)) {
				brelse(tbp);
				break;
			}
		} else
			tbp = last_bp;

		++b_save->bs_nchildren;

		/* Move memory from children to parent */
		pagemove(tbp->b_un.b_daddr, cp, size);
		bp->b_bcount += size;
		bp->b_bufsize += size;

		tbp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI);
		tbp->b_flags |= B_ASYNC;
		s = splbio();
		reassignbuf(tbp, tbp->b_vp);		/* put on clean list */
		++tbp->b_vp->v_numoutput;
		splx(s);
		b_save->bs_children[i] = tbp;

		cp += tbp->b_bufsize;
	}

	if (i == 0) {
		/* None to cluster */
		bp->b_saveaddr = b_save->bs_saveaddr;
		bp->b_flags &= ~B_CALL;
		bp->b_iodone = NULL;
		free(b_save, M_SEGMENT);
	}
	bawrite(bp);
	if (i < len) {
		len -= i + 1;
		start_lbn += 1;
		goto redo;
	}
}

/*
 * Release a buffer.
 * Even if the buffer is dirty, no I/O is started.
 */
brelse(bp)
	register struct buf *bp;
{
	register struct queue_entry *flist;
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
incore(vp, blkno)
	struct vnode *vp;
	daddr_t blkno;
{
	register struct buf *bp;

	for (bp = BUFHASH(vp, blkno)->le_next; bp; bp = bp->b_hash.qe_next)
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
	register struct buf *bp;
	struct list_entry *dp;
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
	for (bp = dp->le_next; bp; bp = bp->b_hash.qe_next) {
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
	register struct buf *bp;

	if (size > MAXBSIZE)
		panic("geteblk: size too big");
	bp = getnewbuf();
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
		if ((ep = bufqueues[BQ_EMPTY].qe_next) == NULL)
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
getnewbuf()
{
	register struct buf *bp;
	register struct queue_entry *dp;
	register struct ucred *cred;
	int s;

loop:
	s = splbio();
	for (dp = &bufqueues[BQ_AGE]; dp > bufqueues; dp--)
		if (dp->qe_next)
			break;
	if (dp == bufqueues) {		/* no free blocks */
		needbuffer = 1;
		sleep((caddr_t)&needbuffer, PRIBIO + 1);
		splx(s);
		goto loop;
	}
	bp = dp->qe_next;
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

	for (ret = 0, bp = (struct buf *)bufqueues[BQ_LOCKED].qe_next;
	    bp; bp = (struct buf *)bp->b_freelist.qe_next)
		++ret;
	return(ret);
}

#ifdef DIAGNOSTIC
/*
 * Print out statistics on the current allocation of the buffer pool.
 * Can be enabled to print out on every ``sync'' by setting "syncprt"
 * above.
 */
void
vfs_bufstats()
{
	int s, i, j, count;
	register struct buf *bp;
	register struct queue_entry *dp;
	int counts[MAXBSIZE/CLBYTES+1];
	static char *bname[BQUEUES] = { "LOCKED", "LRU", "AGE", "EMPTY" };

	for (dp = bufqueues, i = 0; dp < &bufqueues[BQUEUES]; dp++, i++) {
		count = 0;
		for (j = 0; j <= MAXBSIZE/CLBYTES; j++)
			counts[j] = 0;
		s = splbio();
		for (bp = dp->qe_next; bp; bp = bp->b_freelist.qe_next) {
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
