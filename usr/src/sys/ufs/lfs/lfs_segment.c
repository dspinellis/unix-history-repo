/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_segment.c	7.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/resourcevar.h>
#include <sys/kernel.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/conf.h>
#include <sys/vnode.h>
#include <sys/specdev.h>
#include <sys/fifo.h>
#include <sys/malloc.h>
#include <sys/mount.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/dir.h>
#include <ufs/ufs/ufsmount.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/*
 * Add a check so that if the segment is empty, you don't write it.
 *
 * Change lfs_ialloc to allocate a new page of inodes if you have to.
 *
 * Need to keep vnode v_numoutput up to date for pending writes?  Could
 * actually fire off the datablock writes before you finish.  This would give
 * them a chance to get started earlier.
 */

static int	 lfs_biocallback __P((BUF *));
static void	 lfs_endsum __P((struct lfs *, SEGMENT *, int));
static SEGMENT	*lfs_gather __P((struct lfs *,
		     SEGMENT *, VNODE *, int (*) __P((BUF *))));
static BUF	*lfs_newbuf __P((struct lfs *, daddr_t, size_t));
static SEGMENT	*lfs_newseg __P((struct lfs *));
static SEGMENT	*lfs_newsum __P((struct lfs *, SEGMENT *));
static daddr_t	 lfs_nextseg __P((struct lfs *));
static void	 lfs_updatemeta __P((struct lfs *,
		     SEGMENT *, INODE *, daddr_t *, BUF **, int));
static SEGMENT	*lfs_writeckp __P((struct lfs *, SEGMENT *));
static SEGMENT	*lfs_writefile __P((struct lfs *, SEGMENT *, VNODE *, int));
static SEGMENT	*lfs_writeinode __P((struct lfs *, SEGMENT *, INODE *));
static void	 lfs_writeseg __P((struct lfs *, SEGMENT *));
static void	 lfs_writesum __P((struct lfs *));
static void	 lfs_writesuper __P((struct lfs *));
static int	 match_data __P((BUF *));
static int	 match_dindir __P((BUF *));
static int	 match_indir __P((BUF *));
static daddr_t	 next __P((struct lfs *, SEGMENT *, int *));
static void	 shellsort __P((BUF **, daddr_t *, register int));

/*
 * XXX -- when we add fragments in here, we will need to allocate a larger
 * buffer pointer array (sp->bpp).
 */
int
lfs_segwrite(mp, do_ckp)
	MOUNT *mp;
	int do_ckp;			/* do a checkpoint too */
{
	INODE *ip;
	struct lfs *fs;
	VNODE *vp;
	SEGMENT *sp;
	int s;

	fs = VFSTOUFS(mp)->um_lfs;

#ifdef DIAGNOSTIC
	if (fs->lfs_seglist != NULL)
		panic("lfs_segwrite: seglist not NULL");
#endif

	/*
	 * LFS requires that the summary blocks be written after the rest of
	 * the segment, and that the super blocks (on checkpoint) be written
	 * last of all.  We keep a cumulative count of the outstanding blocks
	 * from all of the segments, and write these blocks when this count
	 * goes to zero.  If the disk drive catches up with us it could go
	 * to zero before we finish, so we artificially increment it by one
	 * until we've scheduled all of the writes we intend to do.  At the
	 * moment, the user's process hangs around so we can sleep; this should
	 * probably be redone using a kernel thread.
	 */
	s = splbio();
	fs->lfs_iocount = 1;
	splx(s);

	sp = lfs_newseg(fs);
loop:
	for (vp = mp->mnt_mounth; vp; vp = vp->v_mountf) {
		/*
		 * If the vnode that we are about to sync is no longer
		 * associated with this mount point, start over.
		 */
		if (vp->v_mount != mp)
			goto loop;
		if (VOP_ISLOCKED(vp))
			continue;
		ip = VTOI(vp);
		if (ip->i_number == LFS_IFILE_INUM)
			continue;
		if ((ip->i_flag & (IMOD | IACC | IUPD | ICHG)) == 0 &&
		    vp->v_dirtyblkhd == NULL)
			continue;
		if (vget(vp))
			goto loop;
		sp = lfs_writefile(fs, sp, vp, do_ckp);
		vput(vp);
	}
	if (do_ckp)
		sp = lfs_writeckp(fs, sp);
	lfs_writeseg(fs, sp);

	s = splbio();
	if (--fs->lfs_iocount)
		sleep(&fs->lfs_iocount, PRIBIO + 1);
	splx(s);
	lfs_writesum(fs);
	if (do_ckp)
		lfs_writesuper(fs);
	return (0);
}

static int					/* XXX should be void */
lfs_biocallback(bp)
	BUF *bp;
{
	struct lfs *fs;

	/*
	 * XXX
	 * Reset the flags (probably wrong).  If the contents of the buffer
	 * are valid, move back onto the clean list.
	 */
	bp->b_flags &= ~(B_READ | B_DONE | B_ERROR | B_DELWRI);
	fs = VFSTOUFS(bp->b_vp->v_mount)->um_lfs;
	if (bp->b_flags & B_NOCACHE)
		bp->b_vp = NULL;
	else
		reassignbuf(bp, bp->b_vp);
	brelse(bp);

#ifdef SEGWRITE
printf("callback: buffer: %x iocount %d\n", bp, fs->lfs_iocount);
#endif
	if (fs->lfs_iocount == 0)
		panic("lfs_biocallback: zero iocount\n");

	if (--fs->lfs_iocount == 0)
		wakeup(&fs->lfs_iocount);
}

/* Finish up a summary block. */
static void
lfs_endsum(fs, sp, calc_next)
	struct lfs *fs;
	SEGMENT *sp;
	int calc_next;
{
	SEGSUM *ssp;
	int nsums_per_blk;

	if (sp->sbp == NULL)
		return;

	ssp = sp->segsum;

	/*
	 * Compute the address of the next summary block if calc_next is set,
	 * otherwise end the chain.  If the summary block is full, close it
	 * by setting sp->sbp to NULL, so lfs_newsum will allocate a new one.
	 * Calculate the checksum last.
	 */
	nsums_per_blk = fs->lfs_bsize / LFS_SUMMARY_SIZE;
	if (sp->nsums % nsums_per_blk == 0) {
		ssp->ss_nextsum =
		    calc_next ? next(fs, sp, NULL) +
		    (nsums_per_blk - 1) * LFS_SUMMARY_SIZE / DEV_BSIZE :
		    (daddr_t)-1;
		sp->sbp = NULL;
	} else
		ssp->ss_nextsum = calc_next ?
		    sp->sum_addr - LFS_SUMMARY_SIZE / DEV_BSIZE : (daddr_t)-1;

	ssp->ss_cksum =
	    cksum(&ssp->ss_cksum, LFS_SUMMARY_SIZE - sizeof(ssp->ss_cksum));
}

static SEGMENT *
lfs_gather(fs, sp, vp, match)
	struct lfs *fs;
	SEGMENT *sp;
	VNODE *vp;
	int (*match) __P((BUF *));
{
	BUF **bpp, *bp, *nbp;
	FINFO *fip;
	INODE *ip;
	daddr_t *lbp, *start_lbp;
	u_long version;
	int s;

	ip = VTOI(vp);
	bpp = sp->cbpp;
	fip = sp->fip;
	start_lbp = lbp = &fip->fi_blocks[fip->fi_nblocks];

	s = splbio();
	for (bp = vp->v_dirtyblkhd; bp; bp = nbp) {
		nbp = bp->b_blockf;
		if (bp->b_flags & B_BUSY)
			continue;
#ifdef DIAGNOSTIC
		if ((bp->b_flags & B_DELWRI) == 0)
			panic("lfs_gather: not dirty");
#endif
		if (!match(bp))
			continue;

		/* Remove the buffer from the free lists, prepare it for I/O. */
		bremfree(bp);
		bp->b_flags |= B_BUSY | B_CALL;
		bp->b_iodone = lfs_biocallback;
		bp->b_dev = VTOI(fs->lfs_ivnode)->i_dev;

		/* Insert into the buffer list, update the FINFO block. */
		*sp->cbpp++ = bp;
		++fip->fi_nblocks;
		*lbp++ = bp->b_lblkno;

		sp->sum_bytes_left -= sizeof(daddr_t);
		sp->seg_bytes_left -= bp->b_bufsize;

		/*
		 * Allocate a new summary block (and, possibly, a new segment)
		 * if necessary.  In this case we sort the blocks we've done
		 * so far and assign disk addresses so we can start the new
		 * block correctly.  We may be doing I/O, so we need to release
		 * the splbio() before anything else.
		 */
		if (sp->sum_bytes_left < sizeof(daddr_t) ||
		    sp->seg_bytes_left < fs->lfs_bsize) {
			splx(s);
			lfs_updatemeta(fs,
			    sp, ip, start_lbp, bpp, lbp - start_lbp);

			/* Add the current file to the segment summary. */
			++((SEGSUM *)(sp->segsum))->ss_nfinfo;

			version = fip->fi_version;
			if (sp->seg_bytes_left < fs->lfs_bsize) {
				lfs_writeseg(fs, sp);
				sp = lfs_newseg(fs);
			} else if (sp->sum_bytes_left < sizeof(daddr_t))
				sp = lfs_newsum(fs, sp);

			/* A new FINFO either way. */
			fip = sp->fip;
			fip->fi_version = version;
			fip->fi_ino = ip->i_number;
			start_lbp = lbp = fip->fi_blocks;

			bpp = sp->cbpp;
			s = splbio();
		}
	}
	splx(s);
	lfs_updatemeta(fs, sp, ip, start_lbp, bpp, lbp - start_lbp);
	return (sp);
}

/*
 * Allocate a new buffer header.
 */
static BUF *
lfs_newbuf(fs, daddr, size)
	struct lfs *fs;
	daddr_t daddr;
	size_t size;
{
	BUF *bp;

	bp = getnewbuf();
	bremhash(bp);
	bp->b_vp = fs->lfs_ivnode;
	bp->b_bcount = 0;
	bp->b_blkno = bp->b_lblkno = daddr;
	bp->b_error = 0;
	bp->b_resid = 0;
	bp->b_flags |= B_DELWRI | B_NOCACHE;
	bp->b_iodone = lfs_biocallback;
	bp->b_dev = VTOI(fs->lfs_ivnode)->i_dev;
	allocbuf(bp, size);
	return (bp);
}

/*
 * Start a new segment.
 */
static SEGMENT *
lfs_newseg(fs)
	struct lfs *fs;
{
	FINFO *fip;
	SEGMENT *sp;
	SEGUSE *sup;
	SEGSUM *ssp;
	daddr_t lbn, *lbnp;

	sp = malloc(sizeof(SEGMENT), M_SEGMENT, M_WAITOK);
	sp->nextp = NULL;
	sp->cbpp = sp->bpp =
	    malloc(fs->lfs_ssize * sizeof(BUF *), M_SEGMENT, M_WAITOK);
	sp->ibp = sp->sbp = NULL;
	sp->seg_bytes_left = (fs->lfs_segmask + 1);
	sp->saddr = fs->lfs_nextseg;
	sp->sum_addr = sp->saddr + sp->seg_bytes_left / DEV_BSIZE;
	sp->ninodes = 0;
	sp->nsums = 0;
	sp->seg_number =
	    (sp->saddr - fs->lfs_sboffs[0]) / fsbtodb(fs, fs->lfs_ssize);

	/* Advance to the next segment. */
	fs->lfs_nextseg = lfs_nextseg(fs);

	/* Initialize the summary block. */
	sp = lfs_newsum(fs, sp);

	/*
	 * If su_nbytes non-zero after the segment was cleaned, the segment
	 * contains a super-block.  Add segment summary information to not
	 * allocate over it.
	 */
	sup = fs->lfs_segtab + sp->seg_number;
	if (sup->su_nbytes != 0) {
		ssp = (SEGSUM *)sp->segsum;
		++ssp->ss_nfinfo;
		fip = sp->fip;
		fip->fi_nblocks = LFS_SBPAD >> fs->lfs_bshift;
		fip->fi_version = 1;
		fip->fi_ino = LFS_UNUSED_INUM;
		lbnp = fip->fi_blocks;
		for (lbn = 0; lbn < fip->fi_nblocks; ++lbn)
			*lbnp++ = lbn;
		sp->saddr += fsbtodb(fs, fip->fi_nblocks);
		sp->seg_bytes_left -= sup->su_nbytes;
		sp->sum_bytes_left -=
		    sizeof(FINFO) + (fip->fi_nblocks - 1) * sizeof(daddr_t);
		sp->fip = (FINFO *)lbnp;
	}
	return (sp);
}

static SEGMENT *
lfs_newsum(fs, sp)
	struct lfs *fs;
	SEGMENT *sp;
{
	SEGSUM *ssp;
	int nblocks;

	lfs_endsum(fs, sp, 1);

	/* Allocate a new buffer if necessary. */
	if (sp->sbp == NULL) {
		/* Allocate a new segment if necessary. */
		if (sp->seg_bytes_left < fs->lfs_bsize) {
			lfs_writeseg(fs, sp);
			sp = lfs_newseg(fs);
		}

		/* Get the next summary block. */
		sp->sum_addr = next(fs, sp, &nblocks);

		/*
		 * Get a new buffer and enter into the buffer list from
		 * the top of the list.
		 */
		sp->sbp = sp->bpp[fs->lfs_ssize - (nblocks + 1)] =
		    lfs_newbuf(fs, sp->sum_addr, fs->lfs_bsize);

		sp->seg_bytes_left -= fs->lfs_bsize;

		/*
		 * Do a callback for all but the very last summary block in
		 * the segment, for which we wait.
		 */
		if (sp->nsums != 0)
			sp->sbp->b_flags |= B_CALL;
		/*
		 * Fill in the block from the end.  The summary block is filled
		 * in from the end to the beginning so that the last summary
		 * is the last thing written, verifying the entire block.  This
		 * should go away when fragments are available.
		 */
		sp->segsum =
		    sp->sbp->b_un.b_addr + fs->lfs_bsize - LFS_SUMMARY_SIZE;
		sp->sum_addr += (fs->lfs_bsize - LFS_SUMMARY_SIZE) / DEV_BSIZE;

#ifdef SEGWRITE
		printf("alloc summary: bp %x, lblkno %x, bp index %d\n",
		    sp->sbp, sp->sbp->b_lblkno, fs->lfs_ssize - nblocks);
#endif
	} else {
		sp->segsum -= LFS_SUMMARY_SIZE;
		sp->sum_addr -= LFS_SUMMARY_SIZE / DEV_BSIZE;
	}
	++sp->nsums;

	/* Set point to SEGSUM, initialize it. */
	ssp = sp->segsum;
	ssp->ss_next = fs->lfs_nextseg;
	ssp->ss_prev = fs->lfs_lastseg;
	ssp->ss_nextsum = (daddr_t)-1;
	ssp->ss_create = time.tv_sec;
	ssp->ss_nfinfo = ssp->ss_ninos = 0;

	/* Set pointer to first FINFO, initialize it. */
	sp->fip = (FINFO *)(sp->segsum + sizeof(SEGSUM));

	sp->sum_bytes_left = LFS_SUMMARY_SIZE - sizeof(SEGSUM);
	return (sp);
}

#define seginc(fs, sn)		/* increment segment number */ \
	(((sn) + 1) % (fs)->lfs_nseg)
/*
 * Return the next segment to write.
 */
static daddr_t
lfs_nextseg(fs)
	struct lfs *fs;
{
	int segnum, sn;

	segnum = sn = datosn(fs, fs->lfs_nextseg);
	while ((sn = seginc(fs, sn)) != segnum &&
	    fs->lfs_segtab[sn].su_flags & SEGUSE_DIRTY);

	if (sn == segnum)
		panic("lfs_nextseg: file system full");		/* XXX */
	return (sntoda(fs, sn));
}

/*
 * Update the metadata that points to the blocks listed in the FINFO
 * array.
 */
static void
lfs_updatemeta(fs, sp, ip, lbp, bpp, nblocks)
	struct lfs *fs;
	SEGMENT *sp;
	INODE *ip;
	daddr_t *lbp;
	BUF **bpp;
	int nblocks;
{
	SEGUSE *segup;
	BUF **lbpp, *bp;
	daddr_t daddr, iblkno;
	int db_per_fsb, error, i;
	long lbn;

	if (nblocks == 0)
		return;

	/* Sort the blocks and add disk addresses */
	shellsort(bpp, lbp, nblocks);

	db_per_fsb = 1 << fs->lfs_fsbtodb;
	for (lbpp = bpp, i = 0; i < nblocks; ++i, ++lbpp) {
		(*lbpp)->b_blkno = sp->saddr;
		sp->saddr += db_per_fsb;
	}

	for (lbpp = bpp, i = 0; i < nblocks; ++i, ++lbpp) {
		lbn = lbp[i];
		if (error = lfs_bmap(ip, lbn, &daddr))
			panic("lfs_updatemeta: lfs_bmap");

		/* Update in-core copy of old segment usage information. */
		if (daddr != UNASSIGNED) {
			segup = fs->lfs_segtab + datosn(fs, daddr);
			segup->su_lastmod = time.tv_sec;
#ifdef DIAGNOSTIC
			if (segup->su_nbytes < fs->lfs_bsize)
				panic("lfs: negative bytes (segment %d)\n",
				    segup - fs->lfs_segtab);
#endif
			segup->su_nbytes -= fs->lfs_bsize;
		}

		/*
		 * Now change whomever points to lbn.  We could start with the
		 * smallest (most negative) block number in these if clauses,
		 * but we assume that indirect blocks are least common, and
		 * handle them separately.  The test for < 0 is correct and
		 * minimizes the path in the common case.
		 */
#define	BREAD(bno) \
	if (error = bread(ITOV(ip), (bno), fs->lfs_bsize, NOCRED, &bp)) \
		panic("lfs_updatemeta: bread");

		if (lbn < 0)
			if (lbn < -NIADDR) {
#ifdef META
				printf("meta: update indirect block %d\n",
				    D_INDIR);
#endif
				BREAD(D_INDIR);
				bp->b_un.b_daddr[-lbn % NINDIR(fs)] =
				    (*lbpp)->b_blkno;
				lfs_bwrite(bp);
			} else {
				ip->i_ib[-lbn-1] = (*lbpp)->b_blkno;
		} else if (lbn < NDADDR) {
			ip->i_db[lbn] = (*lbpp)->b_blkno;
		} else if ((lbn -= NDADDR) < NINDIR(fs)) {
#ifdef META
			printf("meta: update indirect block %d\n", S_INDIR);
#endif
			BREAD(S_INDIR);
			bp->b_un.b_daddr[lbn] = (*lbpp)->b_blkno;
			lfs_bwrite(bp);
		} else if ((lbn =
		    (lbn - NINDIR(fs)) / NINDIR(fs)) < NINDIR(fs)) {
			iblkno = -(lbn + NIADDR + 1);
#ifdef META
			printf("meta: update indirect block %d\n", iblkno);
#endif
			BREAD(iblkno);
			bp->b_un.b_daddr[lbn % NINDIR(fs)] = (*lbpp)->b_blkno;
			lfs_bwrite(bp);
		} else
			panic("lfs_updatemeta: logical block number too large");
	}
}

static SEGMENT *
lfs_writeckp(fs, sp)
	struct lfs *fs;
	SEGMENT *sp;
{
	BUF *bp;
	FINFO *fip;
	INODE *ip;
	SEGUSE *sup;
	void *xp;
	daddr_t *lbp;
	int bytes_needed, i;

	/*
	 * This will write the dirty ifile blocks, but not the segusage
	 * table nor the ifile inode.
	 */
	sp = lfs_writefile(fs, sp, fs->lfs_ivnode, 1);

	/*
	 * If the segment usage table and the ifile inode won't fit in this
	 * segment, put them in the next one.
	 */
	bytes_needed = fs->lfs_segtabsz << fs->lfs_bshift;
	if (sp->ninodes % INOPB(fs) == 0)
		bytes_needed += fs->lfs_bsize;

	if (sp->seg_bytes_left < bytes_needed) {
		lfs_writeseg(fs, sp);
		sp = lfs_newseg(fs);
		++((SEGSUM *)(sp->segsum))->ss_nfinfo;
	} else if (sp->sum_bytes_left < fs->lfs_segtabsz * sizeof(daddr_t)) {
		sp = lfs_newsum(fs, sp);
		++((SEGSUM *)(sp->segsum))->ss_nfinfo;
	}

#ifdef DEBUG
	if (sp->seg_bytes_left < bytes_needed)
		panic("lfs_writeckp: unable to write checkpoint");
#endif
	/*
	 * Update the segment usage information and the ifile inode
	 * and write it out.
	 */
	sup = fs->lfs_segtab + sp->seg_number;
	sup->su_nbytes =
	    (fs->lfs_segmask + 1) - sp->seg_bytes_left + bytes_needed;
	sup->su_lastmod = time.tv_sec;
	sup->su_flags = SEGUSE_DIRTY;

	/*
	 * Get buffers for the segusage table and write it out.  Don't
	 * bother updating the FINFO pointer, it's not used after this.
	 */
	ip = VTOI(fs->lfs_ivnode);
	fip = sp->fip;
	lbp = &fip->fi_blocks[fip->fi_nblocks];
	for (xp = fs->lfs_segtab, i = 0; i < fs->lfs_segtabsz;
	    xp += fs->lfs_bsize, ++i, ++lbp) {
		*sp->cbpp++ = bp = lfs_newbuf(fs, sp->saddr, fs->lfs_bsize);
		bp->b_flags |= B_CALL;
		bcopy(xp, bp->b_un.b_addr, fs->lfs_bsize);
		ip->i_db[i] = sp->saddr;
		sp->saddr += (1 << fs->lfs_fsbtodb);
		*lbp = i;
		++fip->fi_nblocks;
	}
	return (lfs_writeinode(fs, sp, VTOI(fs->lfs_ivnode)));
}

/*
 * Write the dirty blocks associated with a vnode.
 */
static SEGMENT *
lfs_writefile(fs, sp, vp, do_ckp)
	struct lfs *fs;
	SEGMENT *sp;
	VNODE *vp;
	int do_ckp;
{
	FINFO *fip;
	ino_t inum;

	inum = VTOI(vp)->i_number;

	if (vp->v_dirtyblkhd != NULL) {
		if (sp->seg_bytes_left < fs->lfs_bsize) {
			lfs_writeseg(fs, sp);
			sp = lfs_newseg(fs);
		} else if (sp->sum_bytes_left < sizeof(FINFO))
			sp = lfs_newsum(fs, sp);
		sp->sum_bytes_left -= sizeof(FINFO) - sizeof(daddr_t);

		fip = sp->fip;
		fip->fi_nblocks = 0;
		fip->fi_version =
		    inum == LFS_IFILE_INUM ? 1 : lfs_getversion(fs, inum);
		fip->fi_ino = inum;

		sp = lfs_gather(fs, sp, vp, match_data);
		if (do_ckp) {
			sp = lfs_gather(fs, sp, vp, match_indir);
			sp = lfs_gather(fs, sp, vp, match_dindir);
		}

		fip = sp->fip;

#ifdef META
		printf("lfs_writefile: adding %d blocks\n", fip->fi_nblocks);
#endif
		/*
		 * If this is the ifile, always update the file count as we'll
		 * be adding the segment usage information even if we didn't
		 * write any blocks.  Also, don't update the FINFO pointer for
		 * the ifile as the segment usage information hasn't yet been
		 * added.
		 */
		if (inum == LFS_IFILE_INUM)
			++((SEGSUM *)(sp->segsum))->ss_nfinfo;
		else if (fip->fi_nblocks != 0) {
			++((SEGSUM *)(sp->segsum))->ss_nfinfo;
			sp->fip = (FINFO *)((caddr_t)fip + sizeof(FINFO) +
			    sizeof(daddr_t) * (fip->fi_nblocks - 1));
		}
	}

	/* If this isn't the ifile, update the inode. */
	if (inum != LFS_IFILE_INUM)
		sp = lfs_writeinode(fs, sp, VTOI(vp));
	return (sp);
}

static SEGMENT *
lfs_writeinode(fs, sp, ip)
	struct lfs *fs;
	SEGMENT *sp;
	INODE *ip;
{
	BUF *bp;
	daddr_t next_addr;
	int nblocks;

	/* Allocate a new inode block if necessary. */
	if (sp->ibp == NULL) {
		/* Allocate a new segment if necessary. */
		if (sp->seg_bytes_left < fs->lfs_bsize) {
			lfs_writeseg(fs, sp);
			sp = lfs_newseg(fs);
		}

		/* Get next inode block. */
		next_addr = next(fs, sp, &nblocks);

		/*
		 * Get a new buffer and enter into the buffer list from
		 * the top of the list.
		 */
		sp->ibp = sp->bpp[fs->lfs_ssize - (nblocks + 1)] =
		    lfs_newbuf(fs, next_addr, fs->lfs_bsize);
		sp->ibp->b_flags |= B_CALL;

		/* Set remaining space counter. */
		sp->seg_bytes_left -= fs->lfs_bsize;
	}

	/* Copy the new inode onto the inode page. */
	bp = sp->ibp;
	bcopy(&ip->i_din,
	    bp->b_un.b_dino + (sp->ninodes % INOPB(fs)), sizeof(DINODE));

	/* Increment inode count in segment summary block. */
	++((SEGSUM *)(sp->segsum))->ss_ninos;

	/* If this page is full, set flag to allocate a new page. */
	if (++sp->ninodes % INOPB(fs) == 0)
		sp->ibp = NULL;

	/*
	 * If updating the ifile, update the super-block; otherwise, update
	 * the ifile itself.  In either case, turn of inode update flags.
	 */
	if (ip->i_number == LFS_IFILE_INUM)
		fs->lfs_idaddr = bp->b_blkno;
	else
		lfs_iset(ip, bp->b_blkno, ip->i_atime);
	ip->i_flags &= ~(IMOD | IACC | IUPD | ICHG);
	return (sp);
}

static void
lfs_writeseg(fs, sp)
	struct lfs *fs;
	SEGMENT *sp;
{
	BUF **bpp;
	SEGUSE *sup;
	int i, nblocks, s, (*strategy) __P((BUF *));
	void *pmeta;

	/* Update superblock segment address. */
	fs->lfs_lastseg = sntoda(fs, sp->seg_number);

	/* Finish up any summary block. */
	lfs_endsum(fs, sp, 0);

	/*
	 * Copy inode and summary block buffer pointers down so they are
	 * contiguous with the page buffer pointers.
	 */
	(void)next(fs, sp, &nblocks);
	pmeta = (sp->bpp + fs->lfs_ssize) - nblocks;
	if (pmeta != sp->cbpp)
		bcopy(pmeta, sp->cbpp, sizeof(BUF *) * nblocks);
	sp->cbpp += nblocks;
	nblocks = sp->cbpp - sp->bpp;

	sup = fs->lfs_segtab + sp->seg_number;
	sup->su_nbytes = nblocks << fs->lfs_bshift;
	sup->su_lastmod = time.tv_sec;
	sup->su_flags = SEGUSE_DIRTY;

	/*
	 * Since we need to guarantee that the summary block gets written last,
	 * we issue the writes in two sets.  The first n-1 buffers first, and
	 * then, after they've completed, the summary buffer.  Only when that
	 * final write completes is the segment valid.
	 */
	--nblocks;			/* Don't count last summary block. */

	sp->nextp = fs->lfs_seglist;
	fs->lfs_seglist = sp;

	s = splbio();
	fs->lfs_iocount += nblocks;
	splx(s);

	strategy =
	    VFSTOUFS(fs->lfs_ivnode->v_mount)->um_devvp->v_op->vop_strategy;
	for (bpp = sp->bpp, i = 0; i < nblocks; ++i, ++bpp)
		(strategy)(*bpp);
}

static void
lfs_writesum(fs)
	struct lfs *fs;
{
	BUF *bp;
	SEGMENT *next_sp, *sp;
	int (*strategy) __P((BUF *));

	strategy =
	    VFSTOUFS(fs->lfs_ivnode->v_mount)->um_devvp->v_op->vop_strategy;
	for (sp = fs->lfs_seglist; sp; sp = next_sp) {
		bp = *(sp->cbpp - 1);
		(strategy)(bp);
		biowait(bp);
		bp->b_vp = NULL;		/* No associated vnode. */
		brelse(bp);

		next_sp = sp->nextp;
		free(sp->bpp, M_SEGMENT);
		free(sp, M_SEGMENT);
	}
	/* Segment list is done. */
	fs->lfs_seglist = NULL;
}

static void
lfs_writesuper(fs)
	struct lfs *fs;
{
	BUF *bp;
	int (*strategy) __P((BUF *));

	strategy =
	    VFSTOUFS(fs->lfs_ivnode->v_mount)->um_devvp->v_op->vop_strategy;

	/* Checksum the superblock and copy it into a buffer. */
	fs->lfs_cksum = cksum(fs, sizeof(struct lfs) - sizeof(fs->lfs_cksum));
	bp = lfs_newbuf(fs, fs->lfs_sboffs[0], LFS_SBPAD);
	bcopy(fs, bp->b_un.b_lfs, sizeof(struct lfs));

	/* Write the first superblock (wait). */
	(strategy)(bp);
	biowait(bp);

	/* Write the second superblock (don't wait). */
	bp->b_flags &= ~B_DONE;
	bp->b_flags |= B_ASYNC;
	bp->b_vp = NULL;			/* No associated vnode. */
	bp->b_blkno = bp->b_lblkno = fs->lfs_sboffs[1];
	(strategy)(bp);
}

/*
 * Logical block number match routines used when traversing the dirty block
 * chain.
 */
static int
match_data(bp)
	BUF *bp;
{
	return (bp->b_lblkno >= 0);
}

static int
match_dindir(bp)
	BUF *bp;
{
	return (bp->b_lblkno == D_INDIR);
}

/*
 * These are single indirect blocks.  There are three types:
 *
 * the one in the inode (lblkno == S_INDIR, or -1).
 * the ones that hang off of the double indirect in the inode (D_INDIR);
 *    these all have addresses in the range -2NINDIR to -(3NINDIR-1).
 * the ones that hang off of the double indirect that hangs off of the
 *    triple indirect.  These all have addresses < -(NINDIR^2).
 *
 * Since we currently don't support triple indirect blocks, this gets
 * simpler, and we just look for block numbers less than -NIADDR.
 */
static int
match_indir(bp)
	BUF *bp;
{
	return (bp->b_lblkno == S_INDIR || bp->b_lblkno < -NIADDR);
}

/* Get the next inode/summary block. */
static daddr_t
next(fs, sp, nbp)
	struct lfs *fs;
	SEGMENT *sp;
	int *nbp;
{
	int nblocks, nino_blocks, nseg_blocks, sums_per_block;

	/* Fs blocks allocated to summary blocks. */
	sums_per_block = fs->lfs_bsize / LFS_SUMMARY_SIZE;
	nseg_blocks = (sp->nsums + sums_per_block - 1) / sums_per_block;

	/* Fs blocks allocated to inodes. */
	nino_blocks = (sp->ninodes + INOPB(fs) - 1) / INOPB(fs);

	/* Total number of fs blocks allocated. */
	nblocks = nseg_blocks + nino_blocks;

	if (nbp)
		*nbp = nblocks;

	/*
	 * The disk address of the new inode/summary block is the address of
	 * the start of the segment after this one minus the number of blocks
	 * that we've already used.
	 */
	return (sntoda(fs, sp->seg_number + 1) - fsbtodb(fs, nblocks + 1));
}

/*
 * Shellsort (diminishing increment sort) from Data Structures and
 * Algorithms, Aho, Hopcraft and Ullman, 1983 Edition, page 290;
 * see also Knuth Vol. 3, page 84.  The increments are selected from
 * formula (8), page 95.  Roughly O(N^3/2).
 */
/*
 * This is our own private copy of shellsort because we want to sort
 * two parallel arrays (the array of buffer pointers and the array of
 * logical block numbers) simultaneously.  Note that we cast the array
 * of logical block numbers to a unsigned in this routine so that the
 * negative block numbers (meta data blocks) sort AFTER the data blocks.
 */
static void
shellsort(bp_array, lb_array, nmemb)
	BUF **bp_array;
	daddr_t *lb_array;
	register int nmemb;
{
	static int __rsshell_increments[] = { 4, 1, 0 };
	register int incr, *incrp, t1, t2;
	BUF *bp_temp;
	u_long lb_temp;

	for (incrp = __rsshell_increments; incr = *incrp++;)
		for (t1 = incr; t1 < nmemb; ++t1)
			for (t2 = t1 - incr; t2 >= 0;)
				if (lb_array[t2] > lb_array[t2 + incr]) {
					lb_temp = lb_array[t2];
					lb_array[t2] = lb_array[t2 + incr];
					lb_array[t2 + incr] = lb_temp;
					bp_temp = bp_array[t2];
					bp_array[t2] = bp_array[t2 + incr];
					bp_array[t2 + incr] = bp_temp;
					t2 -= incr;
				} else
					break;
}
