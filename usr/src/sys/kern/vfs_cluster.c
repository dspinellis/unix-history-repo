/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)vfs_cluster.c	8.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/proc.h>
#include <sys/buf.h>
#include <sys/vnode.h>
#include <sys/mount.h>
#include <sys/trace.h>
#include <sys/malloc.h>
#include <sys/resourcevar.h>
#include <libkern/libkern.h>
#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>

/*
 * Local declarations
 */
struct buf *cluster_newbuf __P((struct vnode *, struct buf *, long, daddr_t,
	    daddr_t, long, int));
struct buf *cluster_rbuild __P((struct vnode *, u_quad_t, struct buf *,
	    daddr_t, daddr_t, long, int, long));
void	    cluster_wbuild __P((struct vnode *, struct buf *, long,
	    daddr_t, int, daddr_t));

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
	*bpp = bp = getblk(vp, lblkno, size, 0, 0);
	if (bp->b_flags & (B_CACHE | B_DONE | B_DELWRI)) {
		/*
		 * Desired block is in cache; do any readahead ASYNC.
		 * Case 1, 2.
		 */
		trace(TR_BREADHIT, pack(vp, size), lblkno);
		flags |= B_ASYNC;
		ioblkno = lblkno +
		    (lblkno < vp->v_ralen ? vp->v_ralen >> 1 : vp->v_ralen);
		alreadyincore = (int)incore(vp, ioblkno);
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
	    !(error = VOP_BMAP(vp, ioblkno, NULL, &blkno, &num_ra)) &&
	    blkno != -1) {
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
			    (error = VOP_BMAP(vp, ioblkno, NULL, &blkno,
			    &num_ra)) || blkno == -1)
				goto skip_readahead;
			flags |= B_ASYNC;
			if (num_ra)
				rbp = cluster_rbuild(vp, filesize,
				    NULL, ioblkno, blkno, size, num_ra, flags);
			else {
				rbp = getblk(vp, ioblkno, size, 0, 0);
				rbp->b_flags |= flags;
				rbp->b_blkno = blkno;
			}
		} else if (lblkno != 0) {
			/* case 2; read ahead single block */
			rbp = getblk(vp, ioblkno, size, 0, 0);
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

#ifdef DIAGNOSTIC
	if (size != vp->v_mount->mnt_stat.f_iosize)
		panic("cluster_rbuild: size %d != filesize %d\n",
			size, vp->v_mount->mnt_stat.f_iosize);
#endif
	if (size * (lbn + run + 1) > filesize)
		--run;
	if (run == 0) {
		if (!bp) {
			bp = getblk(vp, lbn, size, 0, 0);
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
		tbp = getblk(vp, lbn + i, 0, 0, 0);
		tbp->b_bcount = tbp->b_bufsize = size;
		tbp->b_blkno = bn;
		{
			daddr_t temp;
			VOP_BMAP(tbp->b_vp, tbp->b_lblkno, NULL, &temp, NULL);
			if (temp != bn) {
				printf("Block: %d Assigned address: %x Bmap address: %x\n",
					    tbp->b_lblkno, tbp->b_blkno, temp);
				panic("cluster_rbuild: wrong disk address");
			}
		}
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
		bp = getblk(vp, lblkno, size, 0, 0);
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
	caddr_t cp;

	daddr_t	daddr;
	b_save = (struct cluster_save *)(bp->b_saveaddr);
	bp->b_saveaddr = b_save->bs_saveaddr;

	cp = (char *)bp->b_data + b_save->bs_bufsize;
	daddr = bp->b_blkno + b_save->bs_bufsize / DEV_BSIZE;
	for (tbp = b_save->bs_children; b_save->bs_nchildren--; ++tbp) {
		pagemove(cp, (*tbp)->b_data, (*tbp)->b_bufsize);
		cp += (*tbp)->b_bufsize;
		bp->b_bufsize -= (*tbp)->b_bufsize;
		if ((*tbp)->b_blkno != daddr) {
			struct inode *ip;
			printf("cluster_callback: bad disk address:\n");
			printf("Clustered Block: %d DiskAddr: %x bytes left: %d\n",
			    bp->b_lblkno, bp->b_blkno, bp->b_bufsize);
			printf("\toriginal size: %d flags: %x\n", bp->b_bcount,
			    bp->b_flags);
			printf("Child Block: %d DiskAddr: %x bytes: %d\n",
			    (*tbp)->b_lblkno, (*tbp)->b_blkno,
			    (*tbp)->b_bufsize);
			ip = VTOI((*tbp)->b_vp);
			printf("daddr: %x i_size %qd\n", daddr, ip->i_size);
			if ((*tbp)->b_lblkno < NDADDR)
				printf("Child block pointer from inode: %x\n",
				    ip->i_din.di_db[(*tbp)->b_lblkno]);
			spl0();
			panic ("cluster_callback: bad disk address");
		}
		daddr += (*tbp)->b_bufsize / DEV_BSIZE;
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
        int clen;

        vp = bp->b_vp;
        lbn = bp->b_lblkno;

	/* Initialize vnode to beginning of file. */
	if (lbn == 0)
		vp->v_lasta = vp->v_clen = vp->v_cstart = vp->v_lastw = 0;

        if (vp->v_clen == 0 || lbn != vp->v_lastw + 1 ||
	    (bp->b_blkno != vp->v_lasta + bp->b_bcount / DEV_BSIZE)) {
		if (vp->v_clen != 0)
			/*
			 * Write is not sequential.
			 */
			cluster_wbuild(vp, NULL, bp->b_bcount, vp->v_cstart,
			    vp->v_lastw - vp->v_cstart + 1, lbn);
		/*
		 * Consider beginning a cluster.
		 */
		if ((lbn + 1) * bp->b_bcount == filesize)
			/* End of file, make cluster as large as possible */
			clen = MAXBSIZE / vp->v_mount->mnt_stat.f_iosize - 1;
		else if (VOP_BMAP(vp, lbn, NULL, &bp->b_blkno, &clen) ||
			    bp->b_blkno == -1) {
			bawrite(bp);
			vp->v_clen = 0;
			vp->v_lasta = bp->b_blkno;
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
	vp->v_lasta = bp->b_blkno;
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

#ifdef DIAGNOSTIC
	if (size != vp->v_mount->mnt_stat.f_iosize)
		panic("cluster_wbuild: size %d != filesize %d\n",
			size, vp->v_mount->mnt_stat.f_iosize);
#endif
redo:
	while ((!incore(vp, start_lbn) || start_lbn == lbn) && len) {
		++start_lbn;
		--len;
	}

	/* Get more memory for current buffer */
	if (len <= 1) {
		if (last_bp) {
			bawrite(last_bp);
		} else if (len) {
			bp = getblk(vp, start_lbn, size, 0, 0);
			bawrite(bp);
		}
		return;
	}

	bp = getblk(vp, start_lbn, size, 0, 0);
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
	cp = (char *)bp->b_data + bp->b_bufsize;
	for (++start_lbn, i = 0; i < len; ++i, ++start_lbn) {
		if (!incore(vp, start_lbn) || start_lbn == lbn)
			break;

		if (last_bp == NULL || start_lbn != last_bp->b_lblkno) {
			tbp = getblk(vp, start_lbn, size, 0, 0);
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
		if (tbp->b_blkno != (bp->b_blkno + bp->b_bufsize / DEV_BSIZE)) {
			printf("Clustered Block: %d addr %x bufsize: %d\n",
			    bp->b_lblkno, bp->b_blkno, bp->b_bufsize);
			printf("Child Block: %d addr: %x\n", tbp->b_lblkno,
			    tbp->b_blkno);
			panic("Clustered write to wrong blocks");
		}

		pagemove(tbp->b_data, cp, size);
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
