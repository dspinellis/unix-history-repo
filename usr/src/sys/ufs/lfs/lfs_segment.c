/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_segment.c	7.28 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/namei.h>
#include <sys/kernel.h>
#include <sys/resourcevar.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/buf.h>
#include <sys/proc.h>
#include <sys/conf.h>
#include <sys/vnode.h>
#include <sys/malloc.h>
#include <sys/mount.h>

#include <miscfs/specfs/specdev.h>
#include <miscfs/fifofs/fifo.h>

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/dir.h>
#include <ufs/ufs/ufsmount.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/* In-memory description of a segment about to be written. */
struct segment {
	struct buf	**bpp;		/* pointer to buffer array */
	struct buf	**cbpp;		/* pointer to next available bp */
	struct buf	*ibp;		/* buffer pointer to inode page */
	struct finfo	*fip;		/* current fileinfo pointer */
	void	*segsum;		/* segment summary info */
	u_long	ninodes;		/* number of inodes in this segment */
	u_long	seg_bytes_left;		/* bytes left in segment */
	u_long	sum_bytes_left;		/* bytes left in summary block */
	u_long	seg_number;		/* number of this segment */
#define	SEGM_CKP	0x01		/* doing a checkpoint */
	u_long	seg_flags;		/* run-time flags for this segment */
};

/*
 * Determine if it's OK to start a partial in this segment, or if we need
 * to go on to a new segment.
 */
#define	LFS_PARTIAL_FITS(fs) \
	((fs)->lfs_dbpseg - ((fs)->lfs_offset - (fs)->lfs_curseg) > \
	1 << (fs)->lfs_fsbtodb)

void	 lfs_callback __P((struct buf *));
void	 lfs_gather __P((struct lfs *, struct segment *,
	     struct vnode *, int (*) __P((struct lfs *, struct buf *))));
void	 lfs_initseg __P((struct lfs *, struct segment *));
void	 lfs_iset __P((struct inode *, daddr_t, time_t));
int	 lfs_match_data __P((struct lfs *, struct buf *));
int	 lfs_match_dindir __P((struct lfs *, struct buf *));
int	 lfs_match_indir __P((struct lfs *, struct buf *));
int	 lfs_match_tindir __P((struct lfs *, struct buf *));
struct buf *
	 lfs_newbuf __P((struct lfs *, daddr_t, size_t));
void	 lfs_newseg __P((struct lfs *));
void	 lfs_shellsort __P((struct buf **, daddr_t *, register int));
void	 lfs_updatemeta __P((struct lfs *,
	    struct segment *, struct vnode *, daddr_t *, struct buf **, int));
void	 lfs_writefile __P((struct lfs *, struct segment *, struct vnode *));
int	 lfs_writeinode __P((struct lfs *, struct segment *, struct inode *));
int	 lfs_writeseg __P((struct lfs *, struct segment *));
void	 lfs_writesuper __P((struct lfs *, struct segment *));
void	 lfs_writevnodes __P((struct lfs *fs, struct mount *mp,
	    struct segment *sp, int dirops));

int	lfs_allclean_wakeup;		/* Cleaner wakeup address. */

/*
 * Ifile and meta data blocks are not marked busy, so segment writes MUST be
 * single threaded.  Currently, there are two paths into lfs_segwrite, sync()
 * and getnewbuf().  They both mark the file system busy.  Lfs_vflush()
 * explicitly marks the file system busy.  So lfs_segwrite is safe.  I think.
 */

int
lfs_vflush(vp)
	struct vnode *vp;
{
	struct inode *ip;
	struct lfs *fs;
	struct segment *sp;
	int error, s;

	fs = VFSTOUFS(vp->v_mount)->um_lfs;
	lfs_seglock(fs);

	/*
	 * Allocate a segment structure and enough space to hold pointers to
	 * the maximum possible number of buffers which can be described in a
	 * single summary block.
	 */
	sp = malloc(sizeof(struct segment), M_SEGMENT, M_WAITOK);
	sp->bpp = malloc(((LFS_SUMMARY_SIZE - sizeof(SEGSUM)) /
	    sizeof(daddr_t) + 1) * sizeof(struct buf *), M_SEGMENT, M_WAITOK);
	sp->seg_flags = SEGM_CKP;

	/*
	 * Keep a cumulative count of the outstanding I/O operations.  If the
	 * disk drive catches up with us it could go to zero before we finish,
	 * so we artificially increment it by one until we've scheduled all of
	 * the writes we intend to do.
	 */
	s = splbio();
	++fs->lfs_iocount;
	splx(s);

	ip = VTOI(vp);
	do {
		lfs_initseg(fs, sp);
		do {
			if (vp->v_dirtyblkhd != NULL)
				lfs_writefile(fs, sp, vp);
		} while (lfs_writeinode(fs, sp, ip));
		ip->i_flags &= ~(IMOD | IACC | IUPD | ICHG);

	} while (lfs_writeseg(fs, sp) && ip->i_number == LFS_IFILE_INUM);

	/*
	 * If the I/O count is non-zero, sleep until it reaches zero.  At the
	 * moment, the user's process hangs around so we can sleep.
	 */
	s = splbio();
	if (--fs->lfs_iocount && (error =
	    tsleep(&fs->lfs_iocount, PRIBIO + 1, "lfs vflush", 0))) {
		free(sp->bpp, M_SEGMENT);
		free(sp, M_SEGMENT);
		return (error);
	}
	splx(s);
	lfs_segunlock(fs);

	/*
	 * XXX
	 * Should be writing a checkpoint?
	 */
	free(sp->bpp, M_SEGMENT);
	free(sp, M_SEGMENT);

	return (0);
}

void
lfs_writevnodes(fs, mp, sp, dirops)
	struct lfs *fs;
	struct mount *mp;
	struct segment *sp;
	int dirops;
{
	struct inode *ip;
	struct vnode *vp;
	int error, s;

loop:	for (vp = mp->mnt_mounth; vp; vp = vp->v_mountf) {
		/*
		 * If the vnode that we are about to sync is no longer
		 * associated with this mount point, start over.
		 */
		if (vp->v_mount != mp)
			goto loop;

		if (dirops && !(vp->v_flag & VDIROP) ||
		    !dirops && (vp->v_flag & VDIROP))
			continue;
		/*
		 * XXX
		 * Up the ref count so we don't get tossed out of
		 * memory.
		 */
		VREF(vp);

		/*
		 * Write the inode/file if dirty and it's not the
		 * the IFILE.
		 */
		ip = VTOI(vp);
		if ((ip->i_flag & (IMOD | IACC | IUPD | ICHG) ||
		    vp->v_dirtyblkhd != NULL) &&
		    ip->i_number != LFS_IFILE_INUM) {
			if (vp->v_dirtyblkhd != NULL)
				lfs_writefile(fs, sp, vp);
			(void) lfs_writeinode(fs, sp, ip);
			ip->i_flags &= ~(IMOD | IACC | IUPD | ICHG);
		}
		vp->v_flag &= ~VDIROP;
		vrele(vp);
	}
}

int
lfs_segwrite(mp, do_ckp)
	struct mount *mp;
	int do_ckp;			/* Do a checkpoint. */
{
	struct buf *bp;
	struct inode *ip;
	struct lfs *fs;
	struct segment *sp;
	struct vnode *vp;
	SEGUSE *segusep;
	daddr_t ibno;
	int error, i, s;

	fs = VFSTOUFS(mp)->um_lfs;
	lfs_seglock(fs);

	/*
	 * Allocate a segment structure and enough space to hold pointers to
	 * the maximum possible number of buffers which can be described in a
	 * single summary block.
	 */
	sp = malloc(sizeof(struct segment), M_SEGMENT, M_WAITOK);
	sp->bpp = malloc(((LFS_SUMMARY_SIZE - sizeof(SEGSUM)) /
	    sizeof(daddr_t) + 1) * sizeof(struct buf *), M_SEGMENT, M_WAITOK);
	sp->seg_flags = do_ckp ? SEGM_CKP : 0;
	lfs_initseg(fs, sp);

	/*
	 * Keep a cumulative count of the outstanding I/O operations.  If the
	 * disk drive catches up with us it could go to zero before we finish,
	 * so we artificially increment it by one until we've scheduled all of
	 * the writes we intend to do.  If not a checkpoint, we never do the
	 * final decrement, avoiding the wakeup in the callback routine.
	 */
	s = splbio();
	++fs->lfs_iocount;
	splx(s);

	lfs_writevnodes(fs, mp, sp, 0);
	fs->lfs_writer = 1;
	if (fs->lfs_dirops && (error =
	    tsleep(&fs->lfs_writer, PRIBIO + 1, "lfs writer", 0))) {
		free(sp->bpp, M_SEGMENT);
		free(sp, M_SEGMENT); 
		fs->lfs_writer = 0;
		return (error);
	}

	lfs_writevnodes(fs, mp, sp, 1);

	/*
	 * If we are doing a checkpoint, mark everything since the
	 * last checkpoint as no longer ACTIVE.
	 */
	if (do_ckp)
		for (ibno = fs->lfs_cleansz + fs->lfs_segtabsz;
		     --ibno >= fs->lfs_cleansz; ) {
			if (bread(fs->lfs_ivnode, ibno, fs->lfs_bsize,
			    NOCRED, &bp))

				panic("lfs: ifile read");
			segusep = (SEGUSE *)bp->b_un.b_addr;
			for (i = fs->lfs_sepb; i--; segusep++)
				segusep->su_flags &= ~SEGUSE_ACTIVE;
				
			LFS_UBWRITE(bp);
		}

	if (do_ckp || fs->lfs_doifile) {
		vp = fs->lfs_ivnode;
		while (vget(vp));
		ip = VTOI(vp);
		if (vp->v_dirtyblkhd != NULL)
			lfs_writefile(fs, sp, vp);
		(void)lfs_writeinode(fs, sp, ip);
		ip->i_flags &= ~(IMOD | IACC | IUPD | ICHG);
		vput(vp);
		/*
		 * This should never happen because we just guaranteed
		 * that all the segment usage table blocks are dirty, so
		 * no new ones should get written.
		 */
		if (lfs_writeseg(fs, sp) && do_ckp)
			panic("lfs_segwrite: created dirty blocks on ckp");
	} else
		(void) lfs_writeseg(fs, sp);

	/*
	 * If the I/O count is non-zero, sleep until it reaches zero.  At the
	 * moment, the user's process hangs around so we can sleep.
	 */
	fs->lfs_writer = 0;
	fs->lfs_doifile = 0;
	wakeup(&fs->lfs_dirops);

	s = splbio();
	--fs->lfs_iocount;
	if (do_ckp) {
		if (fs->lfs_iocount && (error =
		    tsleep(&fs->lfs_iocount, PRIBIO + 1, "lfs sync", 0))) {
			free(sp->bpp, M_SEGMENT);
			free(sp, M_SEGMENT);
			return (error);
		}
		splx(s);
		lfs_writesuper(fs, sp);
	} else 
		splx(s);

	lfs_segunlock(fs);

	free(sp->bpp, M_SEGMENT);
	free(sp, M_SEGMENT);

	return (0);
}

/*
 * Write the dirty blocks associated with a vnode.
 */
void
lfs_writefile(fs, sp, vp)
	struct lfs *fs;
	struct segment *sp;
	struct vnode *vp;
{
	struct buf *bp;
	struct finfo *fip;
	IFILE *ifp;

	if (sp->seg_bytes_left < fs->lfs_bsize ||
	    sp->sum_bytes_left < sizeof(struct finfo)) {
		(void) lfs_writeseg(fs, sp);
		lfs_initseg(fs, sp);
	}
	sp->sum_bytes_left -= sizeof(struct finfo) - sizeof(daddr_t);

	fip = sp->fip;
	fip->fi_nblocks = 0;
	fip->fi_ino = VTOI(vp)->i_number;
	LFS_IENTRY(ifp, fs, fip->fi_ino, bp);
	fip->fi_version = ifp->if_version;
	brelse(bp);

	/*
	 * It may not be necessary to write the meta-data blocks at this point,
	 * as the roll-forward recovery code should be able to reconstruct the
	 * list.
	 */
	lfs_gather(fs, sp, vp, lfs_match_data);
	lfs_gather(fs, sp, vp, lfs_match_indir);
	lfs_gather(fs, sp, vp, lfs_match_dindir);
#ifdef TRIPLE
	lfs_gather(fs, sp, vp, lfs_match_tindir);
#endif

	fip = sp->fip;
#ifdef META
	printf("lfs_writefile: adding %d blocks\n", fip->fi_nblocks);
#endif
	if (fip->fi_nblocks != 0) {
		++((SEGSUM *)(sp->segsum))->ss_nfinfo;
		sp->fip =
		    (struct finfo *)((caddr_t)fip + sizeof(struct finfo) +
		    sizeof(daddr_t) * (fip->fi_nblocks - 1));
	} else
		sp->sum_bytes_left += sizeof(struct finfo) - sizeof(daddr_t);
}

int
lfs_writeinode(fs, sp, ip)
	struct lfs *fs;
	struct segment *sp;
	struct inode *ip;
{
	struct buf *bp, *ibp;
	IFILE *ifp;
	SEGUSE *sup;
	daddr_t daddr;
	ino_t ino;
	int ndx;
	int redo_ifile = 0;

	/* Allocate a new inode block if necessary. */
	if (sp->ibp == NULL) {
		/* Allocate a new segment if necessary. */
		if (sp->seg_bytes_left < fs->lfs_bsize ||
		    sp->sum_bytes_left < sizeof(daddr_t)) {
			(void) lfs_writeseg(fs, sp);
			lfs_initseg(fs, sp);
		}

		/* Get next inode block. */
		daddr = fs->lfs_offset;
		fs->lfs_offset += fsbtodb(fs, 1);
		sp->ibp = *sp->cbpp++ =
		    lfs_newbuf(fs, daddr, fs->lfs_bsize);

		/* Set remaining space counters. */
		sp->seg_bytes_left -= fs->lfs_bsize;
		sp->sum_bytes_left -= sizeof(daddr_t);
		ndx = LFS_SUMMARY_SIZE / sizeof(daddr_t) -
		    sp->ninodes / INOPB(fs) - 1;
		((daddr_t *)(sp->segsum))[ndx] = daddr;
	}

	/* Update the inode times and copy the inode onto the inode page. */
	ITIMES(ip, &time, &time);
	bp = sp->ibp;
	bp->b_un.b_dino[sp->ninodes % INOPB(fs)] = ip->i_din;

	/* Increment inode count in segment summary block. */
	++((SEGSUM *)(sp->segsum))->ss_ninos;

	/* If this page is full, set flag to allocate a new page. */
	if (++sp->ninodes % INOPB(fs) == 0)
		sp->ibp = NULL;

	/*
	 * If updating the ifile, update the super-block.  Update the disk
	 * address and access times for this inode in the ifile.
	 */
	ino = ip->i_number;
	if (ino == LFS_IFILE_INUM) {
		daddr = fs->lfs_idaddr;
		fs->lfs_idaddr = bp->b_blkno;
	} else {
		LFS_IENTRY(ifp, fs, ino, ibp);
		daddr = ifp->if_daddr;
		ifp->if_daddr = bp->b_blkno;
		LFS_UBWRITE(ibp);
	}

	/*
	 * No need to update segment usage if there was no former inode address
	 * or if the last inode address is in the current partial segment.
	 */
	if (daddr != LFS_UNUSED_DADDR && 
	    !(daddr >= fs->lfs_lastpseg && daddr <= bp->b_blkno)) {
		LFS_SEGENTRY(sup, fs, datosn(fs, daddr), bp);
#ifdef DIAGNOSTIC
		if (sup->su_nbytes < sizeof(struct dinode)) {
			/* XXX -- Change to a panic. */
			printf("lfs: negative bytes (segment %d)\n",
			    datosn(fs, daddr));
			panic("negative bytes");
		}
#endif
		sup->su_nbytes -= sizeof(struct dinode);
		LFS_UBWRITE(bp);
		redo_ifile = (ino == LFS_IFILE_INUM && !(bp->b_flags & B_GATHERED));
	}
	return (redo_ifile);
}

void
lfs_gather(fs, sp, vp, match)
	struct lfs *fs;
	struct segment *sp;
	struct vnode *vp;
	int (*match) __P((struct lfs *, struct buf *));
{
	struct buf **bpp, *bp;
struct buf *lastbp;
	struct finfo *fip;
	struct inode *ip;
	daddr_t *lbp, *start_lbp;
	u_long version;
	int s;

	ip = VTOI(vp);
	bpp = sp->cbpp;
	fip = sp->fip;
	start_lbp = lbp = &fip->fi_blocks[fip->fi_nblocks];

loop:	s = splbio();
	lastbp = NULL;
	for (bp = vp->v_dirtyblkhd; bp; lastbp = bp, bp = bp->b_blockf) {
		if (bp->b_flags & B_BUSY || !match(fs, bp) ||
		    bp->b_flags & B_GATHERED)
			continue;
#ifdef DIAGNOSTIC
		if (!(bp->b_flags & B_DELWRI))
			panic("lfs_gather: bp not B_DELWRI");
		if (!(bp->b_flags & B_LOCKED))
			panic("lfs_gather: bp not B_LOCKED");
#endif
		/*
		 * If full, finish this segment.  We may be doing I/O, so
		 * release and reacquire the splbio().
		 */
		if (sp->sum_bytes_left < sizeof(daddr_t) ||
		    sp->seg_bytes_left < fs->lfs_bsize) {
			splx(s);
			lfs_updatemeta(fs,
			    sp, vp, start_lbp, bpp, lbp - start_lbp);

			/* Add the current file to the segment summary. */
			++((SEGSUM *)(sp->segsum))->ss_nfinfo;

			version = fip->fi_version;
			(void) lfs_writeseg(fs, sp);
			lfs_initseg(fs, sp);

			fip = sp->fip;
			fip->fi_version = version;
			fip->fi_ino = ip->i_number;
			start_lbp = lbp = fip->fi_blocks;

			sp->sum_bytes_left -= 
			    sizeof(struct finfo) - sizeof(daddr_t);

			bpp = sp->cbpp;
			goto loop;
		}

		/* Insert into the buffer list, update the FINFO block. */
		bp->b_flags |= B_GATHERED;
		*sp->cbpp++ = bp;
		++fip->fi_nblocks;
		*lbp++ = bp->b_lblkno;

		sp->sum_bytes_left -= sizeof(daddr_t);
		sp->seg_bytes_left -= bp->b_bufsize;
	}
	splx(s);
	lfs_updatemeta(fs, sp, vp, start_lbp, bpp, lbp - start_lbp);
}

/*
 * Update the metadata that points to the blocks listed in the FINFO
 * array.
 */
void
lfs_updatemeta(fs, sp, vp, lbp, bpp, nblocks)
	struct lfs *fs;
	struct segment *sp;
	struct vnode *vp;
	daddr_t *lbp;
	struct buf **bpp;
	int nblocks;
{
	SEGUSE *sup;
	struct buf *bp;
	INDIR a[NIADDR], *ap;
	struct inode *ip;
	daddr_t daddr, lbn, off;
	int db_per_fsb, error, i, num;

	if (nblocks == 0)
		return;

	/* Sort the blocks. */
	lfs_shellsort(bpp, lbp, nblocks);

	/*
	 * Assign disk addresses, and update references to the logical
	 * block and the segment usage information.
	 */
	db_per_fsb = fsbtodb(fs, 1);
	for (i = nblocks; i--; ++bpp) {
		lbn = *lbp++;
		(*bpp)->b_blkno = off = fs->lfs_offset;
		fs->lfs_offset += db_per_fsb;

		if (error = lfs_bmaparray(vp, lbn, &daddr, a, &num))
			panic("lfs_updatemeta: lfs_bmaparray %d", error);
		ip = VTOI(vp);
		switch (num) {
		case 0:
			ip->i_db[lbn] = off;
			break;
		case 1:
			ip->i_ib[a[0].in_off] = off;
			break;
		default:
			ap = &a[num - 1];
			if (bread(vp, ap->in_lbn, fs->lfs_bsize, NOCRED, &bp))
				panic("lfs_updatemeta: bread bno %d",
				    ap->in_lbn);
			/*
			 * Bread may create a new indirect block which needs
			 * to get counted for the inode.
			 */
			if (bp->b_blkno == -1 && !(bp->b_flags & B_CACHE)) {
				ip->i_blocks += btodb(fs->lfs_bsize);
				fs->lfs_bfree -= btodb(fs->lfs_bsize);
			}
			bp->b_un.b_daddr[ap->in_off] = off;
			VOP_BWRITE(bp);
		}

		/* Update segment usage information. */
		if (daddr != UNASSIGNED) {
			LFS_SEGENTRY(sup, fs, datosn(fs, daddr), bp);
#ifdef DIAGNOSTIC
			if (sup->su_nbytes < fs->lfs_bsize) {
				/* XXX -- Change to a panic. */
				printf("lfs: negative bytes (segment %d)\n",
				    datosn(fs, daddr));
				panic ("Negative Bytes");
			}
#endif
			sup->su_nbytes -= fs->lfs_bsize;
			LFS_UBWRITE(bp);
		}
	}
}

/*
 * Start a new segment.
 */
void
lfs_initseg(fs, sp)
	struct lfs *fs;
	struct segment *sp;
{
	SEGUSE *sup;
	SEGSUM *ssp;
	struct buf *bp;
	daddr_t lbn, *lbnp;

	/* Advance to the next segment. */
	if (!LFS_PARTIAL_FITS(fs)) {
		/* Wake up any cleaning procs waiting on this file system. */
		wakeup(&fs->lfs_nextseg);
		wakeup(&lfs_allclean_wakeup);

		lfs_newseg(fs);
		fs->lfs_offset = fs->lfs_curseg;
		sp->seg_number = datosn(fs, fs->lfs_curseg);
		sp->seg_bytes_left = fs->lfs_dbpseg * DEV_BSIZE;

		/*
		 * If the segment contains a superblock, update the offset
		 * and summary address to skip over it.
		 */
		LFS_SEGENTRY(sup, fs, sp->seg_number, bp);
		if (sup->su_flags & SEGUSE_SUPERBLOCK) {
			fs->lfs_offset += LFS_SBPAD / DEV_BSIZE;
			sp->seg_bytes_left -= LFS_SBPAD;
		}
		brelse(bp);
	} else {
		sp->seg_number = datosn(fs, fs->lfs_curseg);
		sp->seg_bytes_left = (fs->lfs_dbpseg -
		    (fs->lfs_offset - fs->lfs_curseg)) * DEV_BSIZE;
	}
	fs->lfs_lastpseg = fs->lfs_offset;

	sp->ibp = NULL;
	sp->ninodes = 0;

	/* Get a new buffer for SEGSUM and enter it into the buffer list. */
	sp->cbpp = sp->bpp;
	*sp->cbpp = lfs_newbuf(fs, fs->lfs_offset, LFS_SUMMARY_SIZE);
	sp->segsum = (*sp->cbpp)->b_un.b_addr;
	++sp->cbpp;
	fs->lfs_offset += LFS_SUMMARY_SIZE / DEV_BSIZE;

	/* Set point to SEGSUM, initialize it. */
	ssp = sp->segsum;
	ssp->ss_next = fs->lfs_nextseg;
	ssp->ss_nfinfo = ssp->ss_ninos = 0;

	/* Set pointer to first FINFO, initialize it. */
	sp->fip = (struct finfo *)(sp->segsum + sizeof(SEGSUM));
	sp->fip->fi_nblocks = 0;

	sp->seg_bytes_left -= LFS_SUMMARY_SIZE;
	sp->sum_bytes_left = LFS_SUMMARY_SIZE - sizeof(SEGSUM);
}

/*
 * Return the next segment to write.
 */
void
lfs_newseg(fs)
	struct lfs *fs;
{
	CLEANERINFO *cip;
	SEGUSE *sup;
	struct buf *bp;
	int curseg, isdirty, sn;

        LFS_SEGENTRY(sup, fs, datosn(fs, fs->lfs_nextseg), bp);
        sup->su_flags |= SEGUSE_DIRTY;
        LFS_UBWRITE(bp);

	LFS_CLEANERINFO(cip, fs, bp);
	--cip->clean;
	++cip->dirty;
	LFS_UBWRITE(bp);

	fs->lfs_lastseg = fs->lfs_curseg;
	fs->lfs_curseg = fs->lfs_nextseg;
	for (sn = curseg = datosn(fs, fs->lfs_curseg);;) {
		sn = (sn + 1) % fs->lfs_nseg;
		if (sn == curseg)
			panic("lfs_nextseg: no clean segments");
		LFS_SEGENTRY(sup, fs, sn, bp);
		isdirty = sup->su_flags & SEGUSE_DIRTY;
		brelse(bp);
		if (!isdirty)
			break;
	}

	fs->lfs_nextseg = sntoda(fs, sn);
}

int
lfs_writeseg(fs, sp)
	struct lfs *fs;
	struct segment *sp;
{
	struct buf **bpp, *bp, *cbp;
	SEGUSE *sup;
	SEGSUM *ssp;
	dev_t i_dev;
	size_t size;
	u_long *datap, *dp;
	int ch_per_blk, do_again, i, nblocks, num, s;
	int (*strategy)__P((struct vop_strategy_args *));
	struct vop_strategy_args vop_strategy_a;
	u_short ninos;
	char *p;

	/* Checkpoint always writes superblock, even if no data blocks. */
	if ((nblocks = sp->cbpp - sp->bpp) == 0 && !(sp->seg_flags & SEGM_CKP))
		return (0);

	/*
	 * Compute checksum across data and then across summary; the first
	 * block (the summary block) is skipped.  Set the create time here
	 * so that it's guaranteed to be later than the inode mod times.
	 *
	 * XXX
	 * Fix this to do it inline, instead of malloc/copy.
	 */
	datap = dp = malloc(nblocks * sizeof(u_long), M_SEGMENT, M_WAITOK);
	for (bpp = sp->bpp, i = nblocks - 1; i--;)
		*dp++ = (*++bpp)->b_un.b_words[0];
	ssp = (SEGSUM *)sp->segsum;
	ssp->ss_create = time.tv_sec;
	ssp->ss_datasum = cksum(datap, (nblocks - 1) * sizeof(u_long));
	ssp->ss_sumsum =
	    cksum(&ssp->ss_datasum, LFS_SUMMARY_SIZE - sizeof(ssp->ss_sumsum));
	free(datap, M_SEGMENT);

	/* Update the segment usage information. */
	LFS_SEGENTRY(sup, fs, sp->seg_number, bp);
	ninos = (ssp->ss_ninos + INOPB(fs) - 1) / INOPB(fs);
	sup->su_nbytes += nblocks - 1 - ninos << fs->lfs_bshift;
	sup->su_nbytes += ssp->ss_ninos * sizeof(struct dinode);
	sup->su_nbytes += LFS_SUMMARY_SIZE;
	sup->su_lastmod = time.tv_sec;
	sup->su_flags |= SEGUSE_ACTIVE;
	sup->su_ninos += ninos;
	++sup->su_nsums;
	LFS_UBWRITE(bp);
	fs->lfs_bfree -= (fsbtodb(fs, ninos) + LFS_SUMMARY_SIZE / DEV_BSIZE);
	do_again = !(bp->b_flags & B_GATHERED);

	i_dev = VTOI(fs->lfs_ivnode)->i_dev;
	strategy = VTOI(fs->lfs_ivnode)->i_devvp->v_op[VOFFSET(vop_strategy)];

	/*
	 * When we simply write the blocks we lose a rotation for every block
	 * written.  To avoid this problem, we allocate memory in chunks, copy
	 * the buffers into the chunk and write the chunk.  56K was chosen as
	 * some driver/controllers can't handle unsigned 16 bit transfers.
	 * When the data is copied to the chunk, turn off the the B_LOCKED bit
	 * and brelse the buffer (which will move them to the LRU list).  Add
	 * the B_CALL flag to the buffer header so we can count I/O's for the
	 * checkpoints and so we can release the allocated memory.
	 *
	 * XXX
	 * This should be removed if the new virtual memory system allows us to
	 * easily make the buffers contiguous in kernel memory and if that's
	 * fast enough.
	 */
#define	LFS_CHUNKSIZE	(56 * 1024)
	ch_per_blk = LFS_CHUNKSIZE / fs->lfs_bsize;
	for (bpp = sp->bpp, i = nblocks; i;) {
		num = ch_per_blk;
		if (num > i)
			num = i;
		i -= num;
		size = num * fs->lfs_bsize;

		cbp = lfs_newbuf(fs, (*bpp)->b_blkno, 0);
		cbp->b_dev = i_dev;
		cbp->b_flags = B_ASYNC | B_BUSY | B_CALL;
		cbp->b_iodone = lfs_callback;
		cbp->b_saveaddr = cbp->b_un.b_addr;
		cbp->b_un.b_addr = malloc(size, M_SEGMENT, M_WAITOK);

		s = splbio();
		++fs->lfs_iocount;
		for (p = cbp->b_un.b_addr; num--;) {
			bp = *bpp++;
			bcopy(bp->b_un.b_addr, p, bp->b_bcount);
			p += bp->b_bcount;
			bp->b_flags &= ~(B_DONE | B_ERROR | B_READ | B_DELWRI |
			     B_LOCKED | B_GATHERED);
			if (!(bp->b_flags & (B_NOCACHE | B_INVAL))) {
				bremfree(bp);
				reassignbuf(bp, bp->b_vp);
			}
			brelse(bp);
		}
		splx(s);
		cbp->b_bcount = p - cbp->b_un.b_addr;
		vop_strategy_a.a_desc = VDESC(vop_strategy);
		vop_strategy_a.a_bp = cbp;
		(strategy)(&vop_strategy_a);
	}
	return (do_again);
}

void
lfs_writesuper(fs, sp)
	struct lfs *fs;
	struct segment *sp;
{
	struct buf *bp;
	dev_t i_dev;
	int (*strategy) __P((struct vop_strategy_args *));
	struct vop_strategy_args vop_strategy_a;

	i_dev = VTOI(fs->lfs_ivnode)->i_dev;
	strategy = VTOI(fs->lfs_ivnode)->i_devvp->v_op[VOFFSET(vop_strategy)];

	/* Checksum the superblock and copy it into a buffer. */
	fs->lfs_cksum = cksum(fs, sizeof(struct lfs) - sizeof(fs->lfs_cksum));
	bp = lfs_newbuf(fs, fs->lfs_sboffs[0], LFS_SBPAD);
	*bp->b_un.b_lfs = *fs;

	/* Write the first superblock (wait). */
	bp->b_dev = i_dev;
	bp->b_flags |= B_BUSY;
	bp->b_flags &= ~(B_DONE | B_ERROR | B_READ | B_DELWRI);
	vop_strategy_a.a_desc = VDESC(vop_strategy);
	vop_strategy_a.a_bp = bp;
	(strategy)(&vop_strategy_a);
	biowait(bp);

	/* Write the second superblock (don't wait). */
	bp->b_blkno = bp->b_lblkno = fs->lfs_sboffs[1];
	bp->b_flags |= B_ASYNC | B_BUSY;
	bp->b_flags &= ~(B_DONE | B_ERROR | B_READ | B_DELWRI);
	(strategy)(&vop_strategy_a);
}

/*
 * Logical block number match routines used when traversing the dirty block
 * chain.
 */
int
lfs_match_data(fs, bp)
	struct lfs *fs;
	struct buf *bp;
{
	return (bp->b_lblkno >= 0);
}

int
lfs_match_indir(fs, bp)
	struct lfs *fs;
	struct buf *bp;
{
	int lbn;

	lbn = bp->b_lblkno;
	return (lbn < 0 && (-lbn - NDADDR) % NINDIR(fs) == 0);
}

int
lfs_match_dindir(fs, bp)
	struct lfs *fs;
	struct buf *bp;
{
	int lbn;

	lbn = bp->b_lblkno;
	return (lbn < 0 && (-lbn - NDADDR) % NINDIR(fs) == 1);
}

int
lfs_match_tindir(fs, bp)
	struct lfs *fs;
	struct buf *bp;
{
	int lbn;

	lbn = bp->b_lblkno;
	return (lbn < 0 && (-lbn - NDADDR) % NINDIR(fs) == 2);
}

/*
 * Allocate a new buffer header.
 */
struct buf *
lfs_newbuf(fs, daddr, size)
	struct lfs *fs;
	daddr_t daddr;
	size_t size;
{
	struct buf *bp;

	bp = getnewbuf();
	bremhash(bp);
	bgetvp(fs->lfs_ivnode, bp);
	bp->b_bcount = 0;
	bp->b_lblkno = daddr;
	bp->b_blkno = daddr;
	bp->b_error = 0;
	bp->b_resid = 0;
	if (size)
		allocbuf(bp, size);
	bp->b_flags |= B_NOCACHE;
	bp->b_saveaddr = NULL;
	binshash(bp, &bfreelist[BQ_AGE]);
	return (bp);
}

void
lfs_callback(bp)
	struct buf *bp;
{
	struct lfs *fs;

	fs = VFSTOUFS(bp->b_vp->v_mount)->um_lfs;
#ifdef DIAGNOSTIC
	if (fs->lfs_iocount == 0)
		panic("lfs_callback: zero iocount\n");
#endif
	if (--fs->lfs_iocount == 0)
		wakeup(&fs->lfs_iocount);

	if (bp->b_saveaddr) {
		free(bp->b_un.b_addr, M_SEGMENT);
		bp->b_un.b_addr = bp->b_saveaddr;
		bp->b_saveaddr = NULL;
	}
	brelse(bp);
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
void
lfs_shellsort(bp_array, lb_array, nmemb)
	struct buf **bp_array;
	daddr_t *lb_array;
	register int nmemb;
{
	static int __rsshell_increments[] = { 4, 1, 0 };
	register int incr, *incrp, t1, t2;
	struct buf *bp_temp;
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
