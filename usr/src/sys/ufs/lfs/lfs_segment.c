/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_segment.c	7.5 (Berkeley) %G%
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
#include <sys/kernel.h>			/* XXX  delete when time goes away */

#include <ufs/ufs/quota.h>
#include <ufs/ufs/inode.h>
#include <ufs/ufs/dir.h>
#include <ufs/ufs/ufsmount.h>

#include <ufs/lfs/lfs.h>
#include <ufs/lfs/lfs_extern.h>

/* In-memory description of a segment about to be written. */
typedef struct segment SEGMENT;
struct segment {
	BUF	**bpp;			/* pointer to buffer array */
	BUF	**cbpp;			/* pointer to next available bp */
	BUF	*ibp;			/* buffer pointer to inode page */
	void	*segsum;		/* segment summary info */
	u_long	ninodes;		/* number of inodes in this segment */
	u_long	seg_bytes_left;		/* bytes left in segment */
	u_long	sum_bytes_left;		/* bytes left in summary block */
	u_long	seg_number;		/* number of this segment */
#define	SEGM_CKP	0x01		/* doing a checkpoint */
	u_long	seg_flags;		/* run-time flags for this segment */
	FINFO	*fip;			/* current fileinfo pointer */
};

/*
 * Determine if it's OK to start a partial in this segment, or if we need
 * to go on to a new segment.
 */
#define	LFS_PARTIAL_FITS(fs) \
	((fs)->lfs_dbpseg - ((fs)->lfs_offset - (fs)->lfs_curseg) > \
	1 << (fs)->lfs_fsbtodb)

#define	datosn(fs, daddr)	/* disk address to segment number */ \
	(((daddr) - (fs)->lfs_sboffs[0]) / fsbtodb((fs), (fs)->lfs_ssize))

#define sntoda(fs, sn) 		/* segment number to disk address */ \
	((daddr_t)((sn) * ((fs)->lfs_ssize << (fs)->lfs_fsbtodb) + \
	    (fs)->lfs_sboffs[0]))

static int	 lfs_callback __P((BUF *));
static void	 lfs_gather __P((struct lfs *,
		     SEGMENT *, VNODE *, int (*) __P((struct lfs *, BUF *))));
static void	 lfs_initseg __P((struct lfs *, SEGMENT *));
static BUF	*lfs_newbuf __P((struct lfs *, SEGMENT *, daddr_t, size_t));
static daddr_t	 lfs_newseg __P((struct lfs *));
static void	 lfs_updatemeta __P((struct lfs *,
		     SEGMENT *, VNODE *, daddr_t *, BUF **, int));
static void	 lfs_writefile __P((struct lfs *, SEGMENT *, VNODE *));
static void	 lfs_writeinode __P((struct lfs *, SEGMENT *, INODE *));
static void	 lfs_writeseg __P((struct lfs *, SEGMENT *));
static void	 lfs_writesuper __P((struct lfs *, SEGMENT *));
static int	 match_data __P((struct lfs *, BUF *));
static int	 match_dindir __P((struct lfs *, BUF *));
static int	 match_indir __P((struct lfs *, BUF *));
static int	 match_tindir __P((struct lfs *, BUF *));
static void	 shellsort __P((BUF **, daddr_t *, register int));

int	lfs_allclean_wakeup;		/* Cleaner wakeup address. */

int
lfs_segwrite(mp, do_ckp)
	MOUNT *mp;
	int do_ckp;			/* Do a checkpoint. */
{
	INODE *ip;
	struct lfs *fs;
	VNODE *vp;
	SEGMENT *sp;
	int s, error;

#ifdef VERBOSE
	printf("lfs_segwrite\n");
#endif
	/*
	 * If doing a checkpoint, we keep a cumulative count of the outstanding
	 * I/O operations.  If the disk drive catches up with us it could go to
	 * zero before we finish, so we artificially increment it by one until
	 * we've scheduled all of the writes we intend to do.
	 */
	fs = VFSTOUFS(mp)->um_lfs;
	if (do_ckp) {
		s = splbio();
		fs->lfs_iocount = 1;
		splx(s);
	}

	/*
	 * Allocate a segment structure and enough space to hold pointers to
	 * the maximum possible number of buffers which can be described in a
	 * single summary block.
	 */
	sp = malloc(sizeof(SEGMENT), M_SEGMENT, M_WAITOK);
	sp->bpp = malloc(((LFS_SUMMARY_SIZE - sizeof(SEGSUM)) /
	    sizeof(daddr_t) + 1) * sizeof(BUF *), M_SEGMENT, M_WAITOK);
	sp->seg_flags = do_ckp ? SEGM_CKP : 0;
	lfs_initseg(fs, sp);
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

		/*
		 * Write the inode/file if dirty and it's not the
		 * the IFILE.
		 */
		ip = VTOI(vp);
		if (ip->i_flag & (IMOD | IACC | IUPD | ICHG) == 0 &&
		    vp->v_dirtyblkhd == NULL ||
		    ip->i_number == LFS_IFILE_INUM)
			continue;

		if (vget(vp))
			goto loop;
		lfs_writefile(fs, sp, vp);
		lfs_writeinode(fs, sp, ip);
		vput(vp);
	}
	if (do_ckp) {
		lfs_writefile(fs, sp, fs->lfs_ivnode);
		lfs_writeinode(fs, sp, VTOI(fs->lfs_ivnode));
	}
	lfs_writeseg(fs, sp);

	/*
	 * If the I/O count is non-zero, sleep until it reaches zero.  At the
	 * moment, the user's process hangs around so we can sleep.
	 */
	if (do_ckp) {
		s = splbio();
		if (--fs->lfs_iocount &&
		    (error = tsleep(&fs->lfs_iocount, PRIBIO + 1, "sync", 0)))
			return (error);
		splx(s);
		lfs_writesuper(fs, sp);
	}

	(void)free(sp->bpp, M_SEGMENT);
	(void)free(sp, M_SEGMENT);

	/* Wake up any cleaning processes waiting on this file system. */
	wakeup(&fs->lfs_nextseg);
	wakeup(&lfs_allclean_wakeup);
printf("sync returned\n");
	return (0);
}

/*
 * Write the dirty blocks associated with a vnode.
 */
static void
lfs_writefile(fs, sp, vp)
	struct lfs *fs;
	SEGMENT *sp;
	VNODE *vp;
{
	struct buf *bp;
	FINFO *fip;
	IFILE *ifp;
	ino_t inum;

#ifdef VERBOSE
	printf("lfs_writefile\n");
#endif
	inum = VTOI(vp)->i_number;
	if (vp->v_dirtyblkhd != NULL) {
		if (sp->seg_bytes_left < fs->lfs_bsize ||
		    sp->sum_bytes_left < sizeof(FINFO)) {
			lfs_writeseg(fs, sp);
			lfs_initseg(fs, sp);
		}
		sp->sum_bytes_left -= sizeof(FINFO) - sizeof(daddr_t);

		fip = sp->fip;
		fip->fi_nblocks = 0;
		if (inum == LFS_IFILE_INUM)
			fip->fi_version = 1;
		else {
			LFS_IENTRY(ifp, fs, inum, bp);
			fip->fi_version = ifp->if_version;
			brelse(bp);
		}
		fip->fi_ino = inum;

		/*
		 * It may not be necessary to write the meta-data blocks
		 * at this point, as the roll-forward recovery code should
		 * be able to reconstruct the list.
		 */
		lfs_gather(fs, sp, vp, match_data);
		lfs_gather(fs, sp, vp, match_indir);
		lfs_gather(fs, sp, vp, match_dindir);
#ifdef TRIPLE
		lfs_gather(fs, sp, vp, match_tindir);
#endif

		fip = sp->fip;
#ifdef META
		printf("lfs_writefile: adding %d blocks\n", fip->fi_nblocks);
#endif
		if (fip->fi_nblocks != 0) {
			++((SEGSUM *)(sp->segsum))->ss_nfinfo;
			sp->fip = (FINFO *)((caddr_t)fip + sizeof(FINFO) +
			    sizeof(daddr_t) * (fip->fi_nblocks - 1));
		}
	}
}

static void
lfs_writeinode(fs, sp, ip)
	struct lfs *fs;
	SEGMENT *sp;
	INODE *ip;
{
	BUF *bp;
	daddr_t next_addr;
	int ndx;

#ifdef VERBOSE
	printf("lfs_writeinode\n");
#endif
	/* Allocate a new inode block if necessary. */
	if (sp->ibp == NULL) {
		/* Allocate a new segment if necessary. */
		if (sp->seg_bytes_left < fs->lfs_bsize ||
		    sp->sum_bytes_left < sizeof(daddr_t)) {
			lfs_writeseg(fs, sp);
			lfs_initseg(fs, sp);
		}

		/* Get next inode block. */
		next_addr = fs->lfs_offset;
		fs->lfs_offset += fsbtodb(fs, 1);
		sp->ibp = *sp->cbpp++ =
		    lfs_newbuf(fs, sp, next_addr, fs->lfs_bsize);

		/* Set remaining space counter. */
		sp->seg_bytes_left -= fs->lfs_bsize;
		sp->sum_bytes_left -= sizeof(daddr_t);
		ndx = LFS_SUMMARY_SIZE / sizeof(daddr_t) - 
		    sp->ninodes / INOPB(fs) - 1;
		((daddr_t *)(sp->segsum))[ndx] = next_addr;
	}

	/* Copy the new inode onto the inode page.
	 * XXX
	 * Do struct assignment.
	 */
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
	 * the ifile itself.  In either case, turn off inode update flags.
	 */
	if (ip->i_number == LFS_IFILE_INUM)
		fs->lfs_idaddr = bp->b_blkno;
	else
		lfs_iset(ip, bp->b_blkno, ip->i_atime);
	ip->i_flags &= ~(IMOD | IACC | IUPD | ICHG);
}

static void
lfs_gather(fs, sp, vp, match)
	struct lfs *fs;
	SEGMENT *sp;
	VNODE *vp;
	int (*match) __P((struct lfs *, BUF *));
{
	BUF **bpp, *bp, *nbp;
	FINFO *fip;
	INODE *ip;
	daddr_t *lbp, *start_lbp;
	u_long version;
	int s;

#ifdef VERBOSE
	printf("lfs_gather\n");
#endif
	ip = VTOI(vp);
	bpp = sp->cbpp;
	fip = sp->fip;
	start_lbp = lbp = &fip->fi_blocks[fip->fi_nblocks];

	s = splbio();
	for (bp = vp->v_dirtyblkhd; bp; bp = nbp) {
		nbp = bp->b_blockf;
		/*
		 * XXX
		 * Should probably sleep on any BUSY buffer if
		 * doing an fsync?
		 */
		if (bp->b_flags & B_BUSY)
			continue;
#ifdef DIAGNOSTIC
		if (!(bp->b_flags & B_DELWRI))
			panic("lfs_gather: bp not B_DELWRI");
		if (!(bp->b_flags & B_LOCKED))
			panic("lfs_gather: bp not B_LOCKED");
#endif
		if (!match(fs, bp))
			continue;

		/* Insert into the buffer list, update the FINFO block. */
		*sp->cbpp++ = bp;
		++fip->fi_nblocks;
		*lbp++ = bp->b_lblkno;

		/*
		 * If full, finish this segment.  We may be doing I/O, so
		 * release and reacquire the splbio().
		 */
		sp->sum_bytes_left -= sizeof(daddr_t);
		sp->seg_bytes_left -= bp->b_bufsize;
		if (sp->sum_bytes_left < sizeof(daddr_t) ||
		    sp->seg_bytes_left < fs->lfs_bsize) {
			splx(s);
			lfs_updatemeta(fs,
			    sp, vp, start_lbp, bpp, lbp - start_lbp);

			/* Add the current file to the segment summary. */
			++((SEGSUM *)(sp->segsum))->ss_nfinfo;

			version = fip->fi_version;
			lfs_writeseg(fs, sp);
			lfs_initseg(fs, sp);

			fip = sp->fip;
			fip->fi_version = version;
			fip->fi_ino = ip->i_number;
			start_lbp = lbp = fip->fi_blocks;

			bpp = sp->cbpp;
			s = splbio();
		}
	}
	splx(s);
	lfs_updatemeta(fs, sp, vp, start_lbp, bpp, lbp - start_lbp);
}

/*
 * Update the metadata that points to the blocks listed in the FINFO
 * array.
 */
static void
lfs_updatemeta(fs, sp, vp, lbp, bpp, nblocks)
	struct lfs *fs;
	SEGMENT *sp;
	VNODE *vp;
	daddr_t *lbp;
	BUF **bpp;
	int nblocks;
{
	SEGUSE *sup;
	BUF *bp;
	INDIR a[NIADDR], *ap;
	INODE *ip;
	daddr_t daddr, lbn, off;
	int db_per_fsb, error, i, num;

#ifdef VERBOSE
	printf("lfs_updatemeta\n");
#endif
	if (nblocks == 0)
		return;

	/* Sort the blocks. */
	shellsort(bpp, lbp, nblocks);

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
			panic("lfs_updatemeta: lfs_bmaparray returned %d",
			    error);
#ifdef META
		printf("daddr: %d num: %d\n", daddr, num);
		if (num != 0) {
			int x;
			printf("array from bmaparray:\n");
			for (x = 0; x < num; x++)
				printf("\tlbn %d off %d\n", a[x].in_lbn, a[x].in_off);
		}
#endif
		ip = VTOI(vp);
		switch (num) {
		case 0:
#ifdef META
			printf("update inode for direct block %d\n", lbn);
#endif
			ip->i_db[lbn] = off;
			break;
		case 1:
			ip->i_ib[a[0].in_off] = off;
			break;
		default:
			ap = &a[num - 1];
#ifdef META
			printf("update indirect block %d offset %d\n",
			    ap->in_lbn, ap->in_off); 
#endif
			if (bread(vp, ap->in_lbn, fs->lfs_bsize, NOCRED, &bp))
				panic("lfs_updatemeta: bread bno %d",
				    ap->in_lbn);
			bp->b_un.b_daddr[ap->in_off] = off;
			lfs_bwrite(bp);
		}

		/* Update segment usage information. */
		if (daddr != UNASSIGNED) {
			LFS_SEGENTRY(sup, fs, datosn(fs, daddr), bp);
			sup->su_lastmod = time.tv_sec;
#ifdef DIAGNOSTIC
			if (sup->su_nbytes < fs->lfs_bsize)
				panic("lfs: negative bytes (segment %d)\n",
				    datosn(fs, daddr));
#endif
			sup->su_nbytes -= fs->lfs_bsize;
			lfs_bwrite(bp);
		}
	}
}

/*
 * Start a new segment.
 */
static void
lfs_initseg(fs, sp)
	struct lfs *fs;
	SEGMENT *sp;
{
	SEGUSE *sup;
	SEGSUM *ssp;
	struct buf *bp;
	daddr_t lbn, *lbnp;

#ifdef VERBOSE
	printf("lfs_initseg\n");
#endif
	/* Advance to the next segment. */
	if (1 || !LFS_PARTIAL_FITS(fs)) {
		LFS_SEGENTRY(sup, fs, datosn(fs, fs->lfs_curseg), bp);
		sup->su_flags &= ~SEGUSE_ACTIVE;
		lfs_bwrite(bp);
		fs->lfs_curseg = fs->lfs_offset = fs->lfs_nextseg;
		fs->lfs_nextseg = lfs_newseg(fs);
		sp->seg_number = datosn(fs, fs->lfs_curseg);
		sp->seg_bytes_left = fs->lfs_dbpseg * DEV_BSIZE;

		/*
		 * If su_nbytes is non-zero after the segment was cleaned,
		 * the segment contains a super-block.  Update offset and
		 * summary address to skip over the superblock.
		 */
		LFS_SEGENTRY(sup, fs, sp->seg_number, bp); 
		if (sup->su_nbytes != 0) {
			fs->lfs_offset += LFS_SBPAD / DEV_BSIZE;
			sp->seg_bytes_left -= LFS_SBPAD;
		}
		brelse(bp);
	} else {
		sp->seg_number = datosn(fs, fs->lfs_curseg);
		sp->seg_bytes_left = (fs->lfs_dbpseg -
		    (fs->lfs_offset - fs->lfs_curseg)) * DEV_BSIZE;
	}

	sp->ibp = NULL;
	sp->ninodes = 0;

	/* Get a new buffer for SEGSUM and enter it into the buffer list. */
	sp->cbpp = sp->bpp;
	*sp->cbpp = lfs_newbuf(fs, sp, fs->lfs_offset, LFS_SUMMARY_SIZE);
	sp->segsum = (*sp->cbpp)->b_un.b_addr;
	++sp->cbpp;
	fs->lfs_offset += LFS_SUMMARY_SIZE / DEV_BSIZE;

	/* Set point to SEGSUM, initialize it. */
	ssp = sp->segsum;
	ssp->ss_next = fs->lfs_nextseg;
	ssp->ss_create = time.tv_sec;
	ssp->ss_nfinfo = ssp->ss_ninos = 0;

	/* Set pointer to first FINFO, initialize it. */
	sp->fip = (FINFO *)(sp->segsum + sizeof(SEGSUM));
	sp->fip->fi_nblocks = 0;

	sp->seg_bytes_left -= LFS_SUMMARY_SIZE;
	sp->sum_bytes_left = LFS_SUMMARY_SIZE - sizeof(SEGSUM);
}

/*
 * Return the next segment to write.
 */
static daddr_t
lfs_newseg(fs)
	struct lfs *fs;
{
	SEGUSE *sup;
	struct buf *bp;
	int isdirty, segnum, sn;

#ifdef VERBOSE
	printf("lfs_newseg\n");
#endif
	segnum = datosn(fs, fs->lfs_nextseg);
	LFS_SEGENTRY(sup, fs, segnum, bp);
	sup->su_flags |= SEGUSE_ACTIVE;
	lfs_bwrite(bp);
	for (sn = segnum;;) {
		sn = (sn + 1) % fs->lfs_nseg;
		if (sn == segnum)
			panic("lfs_nextseg: no clean segments");
		LFS_SEGENTRY(sup, fs, sn, bp);
		isdirty = sup->su_flags & SEGUSE_DIRTY;
		brelse(bp);
		if (!isdirty)
			break;
	}
	return (sntoda(fs, sn));
}

static void
lfs_writeseg(fs, sp)
	struct lfs *fs;
	SEGMENT *sp;
{
	BUF **bpp, *bp;
	SEGUSE *sup;
	SEGSUM *segp;
	dev_t i_dev;
	u_long *datap, *dp;
	void *pmeta;
	int flags, i, nblocks, s, (*strategy) __P((BUF *));

#ifdef VERBOSE
	printf("lfs_writeseg\n");
#endif
	/* Update superblock segment address. */
	fs->lfs_lastseg = sntoda(fs, sp->seg_number);
	nblocks = sp->cbpp - sp->bpp;
	
	LFS_SEGENTRY(sup, fs, sp->seg_number, bp);
	sup->su_nbytes += LFS_SUMMARY_SIZE + (nblocks - 1 << fs->lfs_bshift);
	sup->su_lastmod = time.tv_sec;
	sup->su_flags = SEGUSE_DIRTY;
	lfs_bwrite(bp);

	/*
	 * Compute checksum across data and then across summary;
	 * the first block (the summary block) is skipped.
	 *
	 * XXX
	 * Fix this to do it inline, instead of malloc/copy.
	 */
	datap = dp = malloc(nblocks * sizeof(u_long), M_SEGMENT, M_WAITOK);
	for (bpp = sp->bpp, i = nblocks - 1; i--;)
		*dp++ = (*++bpp)->b_un.b_words[0];
		
	segp = (SEGSUM *)sp->segsum;
	segp->ss_datasum = cksum(datap, nblocks * sizeof(u_long));
	segp->ss_sumsum = cksum(&segp->ss_datasum, 
	    LFS_SUMMARY_SIZE - sizeof(segp->ss_sumsum));
	(void)free(datap, M_SEGMENT);

	/*
	 * When we gathered the blocks for I/O we did not mark them busy or
	 * remove them from the freelist.  As we do this, turn off the B_LOCKED
	 * bit so the future brelse will put them on the LRU list, and add the
	 * B_CALL flags if we're doing a checkpoint so we can count I/O's.  LFS
	 * requires that the super blocks (on checkpoint) be written after all
	 * the segment data.
	 */
	i_dev = VTOI(fs->lfs_ivnode)->i_dev;
	strategy = VTOI(fs->lfs_ivnode)->i_devvp->v_op->vop_strategy;

	s = splbio();
	if (sp->seg_flags & SEGM_CKP) {
		fs->lfs_iocount += nblocks;
 		flags = B_ASYNC | B_BUSY | B_CALL;
	} else
		flags = B_ASYNC | B_BUSY;
	for (bpp = sp->bpp, i = nblocks; i--;) {
		bp = *bpp++;
		bp->b_flags |= flags;
		bp->b_flags &=
		    ~(B_DONE | B_ERROR | B_READ | B_DELWRI | B_LOCKED);
		bp->b_dev = i_dev;
		bp->b_iodone = lfs_callback;
		if (!(bp->b_flags & B_NOCACHE)) {
			bremfree(bp);
			reassignbuf(bp, bp->b_vp);
		}
	}
	splx(s);
	
	for (bpp = sp->bpp, i = nblocks; i--;)
		(strategy)(*bpp++);
}

static void
lfs_writesuper(fs, sp)
	struct lfs *fs;
	SEGMENT *sp;
{
	BUF *bp;
	dev_t i_dev;
	int (*strategy) __P((BUF *));

#ifdef VERBOSE
	printf("lfs_writesuper\n");
#endif
	i_dev = VTOI(fs->lfs_ivnode)->i_dev;
	strategy = VTOI(fs->lfs_ivnode)->i_devvp->v_op->vop_strategy;

	/* Checksum the superblock and copy it into a buffer. */
	fs->lfs_cksum = cksum(fs, sizeof(struct lfs) - sizeof(fs->lfs_cksum));
	bp = lfs_newbuf(fs, sp, fs->lfs_sboffs[0], LFS_SBPAD);
	*bp->b_un.b_lfs = *fs;

	/* Write the first superblock (wait). */
	bp->b_dev = i_dev;
	bp->b_flags |= B_BUSY;
	bp->b_flags &= ~(B_DONE | B_ERROR | B_READ | B_DELWRI);
	(strategy)(bp);
	biowait(bp);

	/* Write the second superblock (don't wait). */
	bp->b_blkno = bp->b_lblkno = fs->lfs_sboffs[1];
	bp->b_flags |= B_ASYNC | B_BUSY;
	bp->b_flags &= ~(B_DONE | B_ERROR | B_READ | B_DELWRI);
	(strategy)(bp);
}

/*
 * Logical block number match routines used when traversing the dirty block
 * chain.
 */
static int
match_data(fs, bp)
	struct lfs *fs;
	BUF *bp;
{
	return (bp->b_lblkno >= 0);
}

static int
match_indir(fs, bp)
	struct lfs *fs;
	BUF *bp;
{
	int lbn;

	lbn = bp->b_lblkno;
	return (lbn < 0 && (-lbn - NDADDR) % NINDIR(fs) == 0);
}

static int
match_dindir(fs, bp)
	struct lfs *fs;
	BUF *bp;
{
	int lbn;

	lbn = bp->b_lblkno;
	return (lbn < 0 && (-lbn - NDADDR) % NINDIR(fs) == 1);
}

static int
match_tindir(fs, bp)
	struct lfs *fs;
	BUF *bp;
{
	int lbn;

	lbn = bp->b_lblkno;
	return (lbn < 0 && (-lbn - NDADDR) % NINDIR(fs) == 2);
}

/*
 * Allocate a new buffer header.
 */
static BUF *
lfs_newbuf(fs, sp, daddr, size)
	struct lfs *fs;
	SEGMENT *sp;
	daddr_t daddr;
	size_t size;
{
	BUF *bp;

#ifdef VERBOSE
	printf("lfs_newbuf\n");
#endif
	bp = getnewbuf();
	bremhash(bp);
	bgetvp(fs->lfs_ivnode, bp);
	bp->b_bcount = 0;
	bp->b_lblkno = daddr;
	bp->b_blkno = daddr;
	bp->b_error = 0;
	bp->b_resid = 0;
	allocbuf(bp, size);
	bp->b_flags |= B_NOCACHE;
	binshash(bp, &bfreelist[BQ_AGE]);
	return (bp);
}

/*
 * The buffer cache callback routine.  
 */
static int					/* XXX should be void */
lfs_callback(bp)
	BUF *bp;
{
	struct lfs *fs;

	fs = VFSTOUFS(bp->b_vp->v_mount)->um_lfs;
#ifdef DIAGNOSTIC
	if (fs->lfs_iocount == 0)
		panic("lfs_callback: zero iocount\n");
#endif
	if (--fs->lfs_iocount == 0)
		wakeup(&fs->lfs_iocount);

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
