/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_segment.c	5.3 (Berkeley) %G%
 */

#ifdef LOGFS
#include "param.h"
#include "systm.h"
#include "namei.h"
#include "resourcevar.h"
#include "kernel.h"
#include "file.h"
#include "stat.h"
#include "buf.h"
#include "proc.h"
#include "conf.h"
#include "vnode.h"
#include "specdev.h"
#include "fifo.h"
#include "malloc.h"
#include "mount.h"
#include "../ufs/lockf.h"
#include "../ufs/quota.h"
#include "../ufs/inode.h"
#include "../ufs/dir.h"
#include "../ufs/ufsmount.h"
#include "lfs.h"
#include "lfs_extern.h"

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
static void	 lfs_endsum __P((LFS *, SEGMENT *, int));
static SEGMENT	*lfs_gather
		    __P((LFS *, SEGMENT *, VNODE *, int (*) __P((BUF *))));
static BUF	*lfs_newbuf __P((LFS *, daddr_t, size_t));
static SEGMENT	*lfs_newseg __P((LFS *));
static void	 lfs_newsum __P((LFS *, SEGMENT *));
static daddr_t	 lfs_nextseg __P((LFS *));
static void	 lfs_updatemeta __P((LFS *, SEGMENT *, INODE *, daddr_t *, 
		     BUF **, int));
static SEGMENT	*lfs_writeckp __P((LFS *, SEGMENT *));
static SEGMENT	*lfs_writefile __P((SEGMENT *, LFS *, VNODE *, int));
static SEGMENT	*lfs_writeinode __P((LFS *, SEGMENT *, VNODE *));
static void	 lfs_writeseg __P((LFS *, SEGMENT *));
static void	 lfs_writesum __P((LFS *));
static void	 lfs_writesuper __P((LFS *, SEGMENT *));
static int	 match_data __P((BUF *));
static int	 match_dindir __P((BUF *));
static int	 match_indir __P((BUF *));
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
	FINFO *fip;			/* current file info structure */
	INODE *ip;
	LFS *fs;
	VNODE *vp;
	SEGMENT *sp;
	int s;

	fs = VFSTOUFS(mp)->um_lfs;
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
		if ((ip->i_flag & (IMOD|IACC|IUPD|ICHG)) == 0 &&
		    vp->v_dirtyblkhd == NULL)
			continue;
		if (vget(vp))
			goto loop;
		sp = lfs_writefile(sp, fs, vp, do_ckp);
		vput(vp);
	}

	if (do_ckp)
		sp = lfs_writeckp(fs, sp);
	else
		lfs_writeseg(fs, sp);
	s = splbio();
	if (--fs->lfs_iocount)
		sleep(&fs->lfs_iocount, PRIBIO + 1);
	splx(s);
	lfs_writesum(fs);
	if (do_ckp)
		lfs_writesuper(fs, sp);
	return (0);
}

static int
lfs_biocallback(bp)
	BUF *bp;
{
	LFS *fs;
	SEGMENT *sp, *next_sp;
	VNODE *devvp;

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

printf("callback: buffer: %x iocount %d\n", bp, fs->lfs_iocount);
	if (fs->lfs_iocount == 0)
		panic("lfs_biocallback: zero iocount\n");

	if (--fs->lfs_iocount == 0)
		wakeup(&fs->lfs_iocount);
}

static void
lfs_endsum(fs, sp, calc_next)
	LFS *fs;
	SEGMENT *sp;
	int calc_next;		/* If 1 calculate next, else set to -1. */
{
	BUF *bp;
	SEGSUM *ssp;
	daddr_t next_addr;
	int npages, nseg_pages, nsums_per_blk;

	if (sp->sbp == NULL)
		return;

	ssp = sp->segsum;
	ssp->ss_nextsum = calc_next ?
	    sp->sum_addr - LFS_SUMMARY_SIZE / DEV_BSIZE : (daddr_t)-1;

	/*
	 * XXX
	 * I don't understand how sum_num works; the 0 base seems wrong.
	 */
	nsums_per_blk = fs->lfs_bsize / LFS_SUMMARY_SIZE;
	if ((sp->sum_num % (fs->lfs_bsize / LFS_SUMMARY_SIZE)) ==
	    (nsums_per_blk - 1)) {
		/*
		 * This buffer is now full.  Compute the next address if
		 * appropriate and the checksum, and close the buffer by
		 * setting sp->sbp NULL.
		 */
		if (calc_next) {
			nsums_per_blk = fs->lfs_bsize / LFS_SUMMARY_SIZE;
			nseg_pages = 1 + sp->sum_num / nsums_per_blk;
			npages = nseg_pages +
			    (sp->ninodes + INOPB(fs) - 1) / INOPB(fs);
			next_addr = fs->lfs_sboffs[0] + (sp->seg_number + 1) *
			    fsbtodb(fs, fs->lfs_ssize) -
			    fsbtodb(fs, (npages - 1)) -
			    LFS_SUMMARY_SIZE / DEV_BSIZE;
			ssp->ss_nextsum = next_addr;
		}
		ssp->ss_cksum = cksum(&ssp->ss_cksum,
		    LFS_SUMMARY_SIZE - sizeof(ssp->ss_cksum));
		sp->sbp = NULL;
	} else
		/* Calculate cksum on previous segment summary */
		ssp->ss_cksum = cksum(&ssp->ss_cksum, 
		    LFS_SUMMARY_SIZE - sizeof(ssp->ss_cksum));
}

static SEGMENT *
lfs_gather(fs, sp, vp, match)
	LFS *fs;
	SEGMENT *sp;
	VNODE *vp;
	int (*match) __P((BUF *));
{
	BUF **bpp, *bp, *nbp;
	FINFO *fip;
	INODE *ip;
	int count, s, version;
	daddr_t *lbp, *start_lbp;

	ip = VTOI(vp);
	bpp = sp->cbpp;
	fip = sp->fip;
	version = fip->fi_version;
	start_lbp = lbp = &fip->fi_blocks[fip->fi_nblocks];
	count = 0;

	s = splbio();
	for (bp = vp->v_dirtyblkhd; bp; bp = nbp) {
		nbp = bp->b_blockf;
		if ((bp->b_flags & B_BUSY))
			continue;
		if ((bp->b_flags & B_DELWRI) == 0)
			panic("lfs_write: not dirty");
		if (!match(bp))
			continue;
		bremfree(bp);
		bp->b_flags |= B_BUSY | B_CALL;
		bp->b_iodone = lfs_biocallback;
		bp->b_dev = VTOI(fs->lfs_ivnode)->i_dev;

		*lbp++ = bp->b_lblkno;
		*sp->cbpp++ = bp;
		fip->fi_nblocks++;
		sp->sum_bytes_left -= sizeof(daddr_t);
		sp->seg_bytes_left -= bp->b_bufsize;
		if (sp->sum_bytes_left < sizeof(daddr_t) || 
		    sp->seg_bytes_left < fs->lfs_bsize) {
			/*
			 * We are about to allocate a new summary block
			 * and possibly a new segment.  So, we need to
			 * sort the blocks we've done so far, and assign
			 * the disk addresses, so we can start a new block
			 * correctly.  We may be doing I/O so we need to
			 * release the s lock before doing anything.
			 */
			splx(s);
			lfs_updatemeta(fs, sp, ip, start_lbp, bpp,
			    lbp - start_lbp);

			/* Put this file in the segment summary */
			((SEGSUM *)(sp->segsum))->ss_nfinfo++;

			if (sp->seg_bytes_left < fs->lfs_bsize) {
				lfs_writeseg(fs, sp);
				sp = lfs_newseg(fs);
			} else if (sp->sum_bytes_left < sizeof(daddr_t))
				lfs_newsum(fs, sp);
			fip = sp->fip;
			fip->fi_ino = ip->i_number;
			fip->fi_version = version;
			bpp = sp->cbpp;
			/* You know that you have a new FINFO either way. */
			start_lbp = lbp = fip->fi_blocks;
			s = splbio();
		}
	}
	splx(s);
	lfs_updatemeta(fs, sp, ip, start_lbp, bpp, lbp - start_lbp);
	return(sp);
}

static BUF *
lfs_newbuf(fs, daddr, size)
	LFS *fs;
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

/* Start a new segment. */
static SEGMENT *
lfs_newseg(fs)
	LFS *fs;
{
	SEGMENT *sp;
	SEGUSE *sup;

printf("lfs_newseg: new segment %x\n", fs->lfs_nextseg);

	sp = malloc(sizeof(SEGMENT), M_SEGMENT, M_WAITOK);
	sp->nextp = NULL;
	sp->cbpp = sp->bpp =
	    malloc(fs->lfs_ssize * sizeof(BUF *), M_SEGMENT, M_WAITOK);
	sp->ibp = sp->sbp = NULL;
	sp->sum_bytes_left = LFS_SUMMARY_SIZE;
	sp->seg_bytes_left = (fs->lfs_segmask + 1) - LFS_SUMMARY_SIZE;
	sp->saddr = fs->lfs_nextseg;
	sp->sum_addr = sp->saddr + sp->seg_bytes_left / DEV_BSIZE;
	sp->ninodes = 0;
	sp->sum_num = -1;
	sp->seg_number =
	    (sp->saddr - fs->lfs_sboffs[0]) / fsbtodb(fs, fs->lfs_ssize);

	/* Bump the segment count. */
	fs->lfs_nextseg = lfs_nextseg(fs);
	lfs_newsum(fs, sp);			/* Init sp->segsum. */

	sup = fs->lfs_segtab + sp->seg_number;

	if (sup->su_nbytes != 0) {
		/* This is a segment containing a super block. */
		FINFO *fip;
		daddr_t lbn, *lbnp;
		SEGSUM *ssp;

		ssp = (SEGSUM *)sp->segsum;
		ssp->ss_nfinfo++;
		fip = sp->fip;
		fip->fi_nblocks = LFS_SBPAD >> fs->lfs_bshift;
		fip->fi_version = 1;
		fip->fi_ino = LFS_UNUSED_INUM;
		sp->saddr += fsbtodb(fs, fip->fi_nblocks);
		lbnp = fip->fi_blocks;
		for (lbn = 0; lbn < fip->fi_nblocks; lbn++)
			*lbnp++ = lbn;
		sp->seg_bytes_left -= sup->su_nbytes;
		sp->sum_bytes_left -= 
		    sizeof(FINFO) + (fip->fi_nblocks - 1) * sizeof(daddr_t);
		sp->fip = (FINFO *)lbnp;
	}
	return(sp);
}

static void
lfs_newsum(fs, sp)
	LFS *fs;
	SEGMENT *sp;
{
	SEGSUM *ssp;
	int npages, nseg_pages, sums_per_blk;

printf("lfs_newsum\n");
	lfs_endsum(fs, sp, 1);
	++sp->sum_num;
	if (sp->sbp == NULL) {
		/* Allocate a new buffer. */
		if (sp->seg_bytes_left < fs->lfs_bsize) {
			lfs_writeseg(fs, sp);
			sp = lfs_newseg(fs);
		}
		sums_per_blk = fs->lfs_bsize / LFS_SUMMARY_SIZE;
		nseg_pages = 1 + sp->sum_num / sums_per_blk;
		npages = nseg_pages + (sp->ninodes + INOPB(fs) - 1) / INOPB(fs);
		sp->sum_addr = fs->lfs_sboffs[0] + 
		    (sp->seg_number + 1) * fsbtodb(fs, fs->lfs_ssize)
		    - fsbtodb(fs, npages);
		sp->sbp = lfs_newbuf(fs, sp->sum_addr, fs->lfs_bsize);
		if (sp->sum_num != 0)
			sp->sbp->b_flags |= B_CALL;
		sp->bpp[fs->lfs_ssize - npages] = sp->sbp;
printf("Inserting summary block, address %x at index %d\n",
sp->sbp->b_lblkno, fs->lfs_ssize - npages);
		sp->seg_bytes_left -= fs->lfs_bsize;
		sp->segsum =
		    sp->sbp->b_un.b_addr + fs->lfs_bsize - LFS_SUMMARY_SIZE;
		sp->sum_addr += (fs->lfs_bsize - LFS_SUMMARY_SIZE) / DEV_BSIZE;
	} else {
		sp->segsum -= LFS_SUMMARY_SIZE;
		sp->sum_addr -= LFS_SUMMARY_SIZE / DEV_BSIZE;
	}

	ssp = sp->segsum;
	ssp->ss_next = fs->lfs_nextseg;
	ssp->ss_prev = fs->lfs_lastseg;

	/* Initialize segment summary info. */
	sp->fip = (FINFO *)(sp->segsum + sizeof(SEGSUM));
	sp->fip->fi_nblocks = 0;
	ssp->ss_nextsum = (daddr_t)-1;
	ssp->ss_create = time.tv_sec;

	ssp->ss_nfinfo = 0;
	ssp->ss_ninos = 0;
	sp->sum_bytes_left -= LFS_SUMMARY_SIZE;	
	sp->seg_bytes_left -= LFS_SUMMARY_SIZE;	
}

#define seginc(fs, sn)	((sn + 1) % fs->lfs_nseg)
static daddr_t
lfs_nextseg(fs)
	LFS *fs;
{
	int segnum, sn;
	SEGUSE *sup;

	segnum = satosn(fs, fs->lfs_nextseg);
	for (sn = seginc(fs, segnum); sn != segnum; sn = seginc(fs, sn))
		if (!(fs->lfs_segtab[sn].su_flags & SEGUSE_DIRTY))
			break;

	if (sn == segnum)
		panic("lfs_nextseg: file system full");		/* XXX */
	return(sntosa(fs, sn));
}

/*
 * Update the metadata that points to the blocks listed in the FIP
 * array.
 */
static void
lfs_updatemeta(fs, sp, ip, lbp, bpp, nblocks)
	LFS *fs;
	SEGMENT *sp;
	INODE *ip;
	daddr_t *lbp;
	BUF **bpp;
	int nblocks;
{
	SEGUSE *segup;
	BUF **lbpp, *bp, *mbp;
	daddr_t da, iblkno;
	int db_per_fsb, error, i, oldsegnum;
	long lbn;

printf("lfs_updatemeta of %d blocks\n", nblocks);	
	if (nblocks == 0 && (ip->i_flag & (IMOD | IACC | IUPD | ICHG)) == 0)
		return;

	/* First sort the blocks and add disk addresses */
	shellsort(bpp, lbp, nblocks);

	db_per_fsb = 1 << fs->lfs_fsbtodb;
	for (lbpp = bpp, i = 0; i < nblocks; i++, lbpp++) {
		(*lbpp)->b_blkno = sp->saddr;
		sp->saddr += db_per_fsb;
	}

	for (lbpp = bpp, i = 0; i < nblocks; i++, lbpp++) {
		lbn = lbp[i];
printf("lfs_updatemeta: block %d\n", lbn);
		if (error = lfs_bmap(ip, lbn, &da))
		    panic("lfs_updatemeta: lfs_bmap returned error");

		if (da) {
			/* Update segment usage information */
			oldsegnum = (da - fs->lfs_sboffs[0]) /
			    fsbtodb(fs, fs->lfs_ssize);
			segup = fs->lfs_segtab+oldsegnum;
			segup->su_lastmod = time.tv_sec;
			if ((segup->su_nbytes -= fs->lfs_bsize) < 0)
				printf("lfs_updatemeta: negative bytes %s %d\n",
					"in segment", oldsegnum);
		}

		/*
		 * Now change whoever points to lbn.  We could start with the
		 * smallest (most negative) block number in these if clauses,
		 * but we assume that indirect blocks are least common, and
		 * handle them separately.
		 */
		bp = NULL;
		if (lbn < 0) {
			if (lbn < -NIADDR) {
printf("lfs_updatemeta: changing indirect block %d\n", D_INDIR);
				if (error = bread(ITOV(ip), D_INDIR, 
				    fs->lfs_bsize, NOCRED, &bp))
				    panic("lfs_updatemeta: error on bread");

				bp->b_un.b_daddr[-lbn % NINDIR(fs)] = 
				    (*lbpp)->b_blkno;
			} else
				ip->i_din.di_ib[-lbn-1] = (*lbpp)->b_blkno;
			
		} else if (lbn < NDADDR) 
			ip->i_din.di_db[lbn] = (*lbpp)->b_blkno;
		else if ((lbn -= NDADDR) < NINDIR(fs)) {
printf("lfs_updatemeta: changing indirect block %d\n", S_INDIR);
			if (error = bread(ITOV(ip), S_INDIR, fs->lfs_bsize, 
			    NOCRED, &bp))
			    panic("lfs_updatemeta: bread returned error");

			bp->b_un.b_daddr[lbn] = (*lbpp)->b_blkno;
		} else if ( (lbn = (lbn - NINDIR(fs)) / NINDIR(fs)) < 
			    NINDIR(fs)) {

			iblkno = - (lbn + NIADDR + 1);
printf("lfs_updatemeta: changing indirect block %d\n", iblkno);
			if (error = bread(ITOV(ip), iblkno, fs->lfs_bsize, 
			    NOCRED, &bp))
			    panic("lfs_updatemeta: bread returned error");

			bp->b_un.b_daddr[lbn % NINDIR(fs)] = (*lbpp)->b_blkno;
		}
		else
			panic("lfs_updatemeta: logical block number too large");
		if (bp)
			lfs_bwrite(bp);
	}
}

static SEGMENT *
lfs_writeckp(fs, sp)
	LFS *fs;
	SEGMENT *sp;
{
	BUF *bp;
	FINFO *fip;
	INODE *ip;
	SEGUSE *sup;
	void *xp;
	daddr_t *lbp;
	int bytes_needed, i;

printf("lfs_writeckp\n");
	/*
	 * This will write the dirty ifile blocks, but not the segusage
	 * table nor the ifile inode.
	 */
	sp = lfs_writefile(sp, fs, fs->lfs_ivnode, 1);

	/*
	 * Make sure that the segment usage table and the ifile inode will
	 * fit in this segment.  If they won't, put them in the next segment
	 */
	bytes_needed = fs->lfs_segtabsz << fs->lfs_bshift;
	if (sp->ninodes % INOPB(fs) == 0)
		bytes_needed += fs->lfs_bsize;

	if (sp->seg_bytes_left < bytes_needed) {
		lfs_writeseg(fs, sp);
		sp = lfs_newseg(fs);
	} else if (sp->sum_bytes_left < (fs->lfs_segtabsz * sizeof(daddr_t)))
		lfs_newsum(fs, sp);

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

	/* Get buffers for the segusage table and write it out. */
	ip = VTOI(fs->lfs_ivnode);
	fip = sp->fip;
	lbp = &fip->fi_blocks[fip->fi_nblocks];
	for (xp = fs->lfs_segtab, i = 0; i < fs->lfs_segtabsz; 
	    i++, xp += fs->lfs_bsize, lbp++) {
		*sp->cbpp++ = bp = lfs_newbuf(fs, sp->saddr, fs->lfs_bsize);
		bp->b_flags |= B_CALL;
		bcopy(xp, bp->b_un.b_words, fs->lfs_bsize);
		ip->i_din.di_db[i] = sp->saddr;
		sp->saddr += (1 << fs->lfs_fsbtodb);
		*lbp = i;
		fip->fi_nblocks++;
	}
	sp = lfs_writeinode(fs, sp, fs->lfs_ivnode);
	lfs_writeseg(fs, sp);
	return (sp);
}

/*
 * XXX -- I think we need to figure out what to do if we write
 * the segment and find more dirty blocks when we're done.
 */
static SEGMENT *
lfs_writefile(sp, fs, vp, do_ckp)
	SEGMENT *sp;
	LFS *fs;
	VNODE *vp;
	int do_ckp;
{
	FINFO *fip;
	ino_t inum;

	/* Initialize the FINFO structure. */
	inum = VTOI(vp)->i_number;
printf("lfs_writefile: node %d\n", inum);
	sp->fip->fi_ino = inum;
	sp->fip->fi_nblocks = 0;
	sp->fip->fi_version =
	    inum == LFS_IFILE_INUM ? 1 : lfs_getversion(fs, inum);

	sp = lfs_gather(fs, sp, vp, match_data);
	if (do_ckp) {
		sp = lfs_gather(fs, sp, vp, match_indir);
		sp = lfs_gather(fs, sp, vp, match_dindir);
	}

(void)printf("lfs_writefile: adding %d blocks to segment\n",
sp->fip->fi_nblocks);
	/* 
	 * Update the inode for this file and reflect new inode address in
	 * the ifile.  If this is the ifile, don't update the inode, because
	 * we're checkpointing and will update the inode with the segment
	 * usage information (so we musn't bump the finfo pointer either).
	 */
	if (inum != LFS_IFILE_INUM) {
		sp = lfs_writeinode(fs, sp, vp);
		fip = sp->fip;
		if (fip->fi_nblocks) {
			((SEGSUM *)(sp->segsum))->ss_nfinfo++;
			sp->fip = (FINFO *)((u_long)fip + sizeof(FINFO) + 
			    sizeof(u_long) * fip->fi_nblocks - 1);
		}
	}
	return(sp);
}

static SEGMENT *
lfs_writeinode(fs, sp, vp)
	LFS *fs;
	SEGMENT *sp;
	VNODE *vp;
{
	BUF *bp;
	INODE *ip;
	SEGSUM *ssp;
	daddr_t iaddr, next_addr;
	int npages, nseg_pages, sums_per_blk;
	struct dinode *dip;

printf("lfs_writeinode\n");
	sums_per_blk = fs->lfs_bsize / LFS_SUMMARY_SIZE;
	if (sp->ibp == NULL) {
		/* Allocate a new buffer. */
		if (sp->seg_bytes_left < fs->lfs_bsize) {
			lfs_writeseg(fs, sp);
			sp = lfs_newseg(fs);
		}
		nseg_pages = (sp->sum_num + sums_per_blk) / sums_per_blk;
		npages = nseg_pages + (sp->ninodes + INOPB(fs)) / INOPB(fs);
		next_addr = fs->lfs_sboffs[0] + 
		    (sp->seg_number + 1) * fsbtodb(fs, fs->lfs_ssize)
		    - fsbtodb(fs, npages);
		sp->ibp = lfs_newbuf(fs, next_addr, fs->lfs_bsize);
		sp->ibp->b_flags |= B_CALL;
		sp->bpp[fs->lfs_ssize - npages] = sp->ibp;
		sp->seg_bytes_left -= fs->lfs_bsize;
printf("alloc inode block @ daddr %x, bp = %x inserted at %d\n", 
next_addr, sp->ibp, fs->lfs_ssize - npages);
	}
	ip = VTOI(vp);
	bp = sp->ibp;
	dip = bp->b_un.b_dino + (sp->ninodes % INOPB(fs));
	bcopy(&ip->i_din, dip, sizeof(struct dinode));
	iaddr = bp->b_blkno;
	++sp->ninodes;
	ssp = sp->segsum;
	++ssp->ss_ninos;
	if (sp->ninodes % INOPB(fs) == 0)
		sp->ibp = NULL;
	if (ip->i_number == LFS_IFILE_INUM)
		fs->lfs_idaddr = iaddr;
	else
		lfs_iset(ip, iaddr, ip->i_atime);	/* Update ifile */
	ip->i_flags &= ~(IMOD|IACC|IUPD|ICHG);		/* make inode clean */
	return(sp);
}

static void
lfs_writeseg(fs, sp)
	LFS *fs;
	SEGMENT *sp;
{
	BUF **bpp;
	SEGSUM *ssp;
	SEGUSE *sup;
	VNODE *devvp;
	int nblocks, nbuffers, ninode_blocks, nsegsums, nsum_pb;
	int i, metaoff, nmeta, s;

printf("lfs_writeseg\n");
	fs->lfs_lastseg = sntosa(fs, sp->seg_number);
	lfs_endsum(fs, sp, 0);

	/*
	 * Copy inode and summary block buffer pointers down so they are
	 * contiguous with the page buffer pointers.
	 */
	ssp = sp->segsum;
	nsum_pb = fs->lfs_bsize / LFS_SUMMARY_SIZE;
	nbuffers = sp->cbpp - sp->bpp;
	/*
	 * XXX
	 * Why isn't this (sp->sum_num + nsum_pb - 1) / nsum_pb;
	 */
	nsegsums = 1 + sp->sum_num / nsum_pb;
	ninode_blocks = (sp->ninodes + INOPB(fs) - 1) / INOPB(fs);
	nmeta = ninode_blocks + nsegsums;
	metaoff = fs->lfs_ssize - nmeta;
	nblocks = nbuffers + nmeta;
	if (sp->bpp + metaoff != sp->cbpp)
		bcopy(sp->bpp + metaoff, sp->cbpp, sizeof(BUF *) * nmeta);
	sp->cbpp += nmeta;

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
	devvp = VFSTOUFS(fs->lfs_ivnode->v_mount)->um_devvp;
	--nblocks;				/* Don't count summary. */

	sp->nextp = fs->lfs_seglist;
	fs->lfs_seglist = sp;

	s = splbio();
	fs->lfs_iocount += nblocks;
	splx(s);
	for (bpp = sp->bpp, i = 0; i < nblocks; ++i, ++bpp)
		(devvp->v_op->vop_strategy)(*bpp);
}

static void
lfs_writesum(fs)
	LFS *fs;
{
	BUF *bp;
	SEGMENT *next_sp, *sp;
	VNODE *devvp;

printf("lfs_writesum\n");
	devvp = NULL;
	for (sp = fs->lfs_seglist; sp; sp = next_sp) {
		bp = *(sp->cbpp - 1);
		if (devvp == NULL)
			devvp = VFSTOUFS(bp->b_vp->v_mount)->um_devvp;
		(devvp->v_op->vop_strategy)(bp);
		biowait(bp);
		next_sp = sp->nextp;
		free(sp->bpp, M_SEGMENT);
		free(sp, M_SEGMENT);
	}
}

static void
lfs_writesuper(fs, sp)
	LFS *fs;
	SEGMENT *sp;
{
	BUF *bp;
	VNODE *devvp;

printf("lfs_writesuper\n");
	/* Get a buffer for the super block */
	fs->lfs_cksum = cksum(fs, sizeof(LFS) - sizeof(fs->lfs_cksum));
	bp = lfs_newbuf(fs, fs->lfs_sboffs[0], LFS_SBPAD);
	bcopy(fs, bp->b_un.b_lfs, sizeof(LFS));

	/* Write the first superblock; wait. */
	devvp = VFSTOUFS(fs->lfs_ivnode->v_mount)->um_devvp;
	(devvp->v_op->vop_strategy)(bp);
	biowait(bp);
	
	/* Now, write the second one for which we don't have to wait */
	bp->b_flags &= ~B_DONE;
	bp->b_blkno = bp->b_lblkno = fs->lfs_sboffs[1];
	(devvp->v_op->vop_strategy)(bp);

	bp->b_vp = NULL;			/* No associated vnode. */
	brelse(bp);
}

/* Block match routines used when traversing the dirty block chain. */
match_data(bp)
	BUF *bp;
{
	return(bp->b_lblkno >= 0);
}

match_dindir(bp)
	BUF *bp;
{
	return(bp->b_lblkno == D_INDIR);
}

/*
 * These are single indirect blocks.  There are three types:
 * 	the one in the inode (address S_INDIR = -1).
 * 	the ones that hang off of D_INDIR the double indirect in the inode.
 * 		these all have addresses in the range -2NINDIR to -(3NINDIR-1)
 *	the ones that hang off of double indirect that hang off of the
 *		triple indirect.  These all have addresses < -(NINDIR^2).
 * Since we currently don't support, triple indirect blocks, this gets simpler.
 * We just need to look for block numbers less than -NIADDR.
 */
match_indir(bp)
	BUF *bp;
{
	return(bp->b_lblkno == S_INDIR || bp->b_lblkno < -NIADDR);
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
#endif /* LOGFS */
