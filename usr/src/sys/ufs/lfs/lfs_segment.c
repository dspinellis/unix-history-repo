/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)lfs_segment.c	5.1 (Berkeley) %G%
 */

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
Need to write the inodes out.
The indirect buffers need to be marked dirty
What about sync?  How do you wait on the last I/O?
Need to keep vnode v_numoutput up to date for pending writes.
*/

static int	 lfs_biocallback __P((BUF *));
static void	 lfs_endsum __P((LFS *, SEGMENT *, int));
static BUF	*lfs_newbuf __P((LFS *, daddr_t, size_t));
static SEGMENT	*lfs_newseg __P((LFS *));
static void	 lfs_newsum __P((LFS *, SEGMENT *));
static daddr_t	 lfs_nextseg __P((LFS *));
static int	 lfs_updatemeta __P((LFS *, INODE *, FINFO *, BUF **));
static SEGMENT	*lfs_writefile __P((SEGMENT *, LFS *, VNODE *));
static void	 lfs_writemeta __P((void));
static void	 lfs_writeseg __P((LFS *, SEGMENT *));
static void	 shellsort __P((BUF **, u_long *, register int));

/*
 * XXX -- when we add fragments in here, we will need to allocate a larger
 * buffer pointer array (sp->bpp).
 */
int
lfs_segwrite(mp)
	MOUNT *mp;
{
	FINFO *fip;			/* current file info structure */
	INODE *ip;
	LFS *fs;
	VNODE *vp;
	SEGMENT *sp;

printf("lfs_segwrite: %s %s\n", mp->mnt_stat.f_mntonname, mp->mnt_stat.f_mntfromname);
	fs = VFSTOUFS(mp)->um_lfs;

	sp = lfs_newseg(fs);
loop:
	for (vp = mp->mnt_mounth; vp; vp = vp->v_mountf) {
		/*
		 * If the vnode that we are about to sync is no longer
		 * associated with this mount point, start over.
		 */
printf("lfs_segwrite: processing inum %d\n", VTOI(vp)->i_number);
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
		sp = lfs_writefile(sp, fs, vp);

		/* Need to take care of inode now */
printf("lfs_segwrite: need to add dinode %d to seg\n", ip->i_din.di_inum);
		vput(vp);
	}
	/*
	 * Force stale file system control information to be flushed.
	 */
	lfs_writeseg(fs, sp);
/*	vflushbuf(ump->um_devvp, waitfor == MNT_WAIT ? B_SYNC : 0); */
printf("lfs_segwrite: returning from segwrite\n");
	return (0);
}

static int
lfs_biocallback(bp)
	BUF *bp;
{
	LFS *fs;
	SEGMENT *sp, *next_sp;
	UFSMOUNT *ump;
	VNODE *devvp;

	ump = VFSTOUFS(bp->b_vp->v_mount);
	fs = ump->um_lfs;
	devvp = ump->um_devvp;
							/* XXX splbio(); */
printf("lfs_biocallback: iocount: %d\n", fs->lfs_iocount);
	if (--fs->lfs_iocount) {
		/* Fire off summary writes */
		for (sp = fs->lfs_seglist; sp; sp = next_sp) {
			next_sp = sp->nextp;
			(*(devvp->v_op->vop_strategy))(*(sp->cbpp - 1));
printf("free: segsum %x bpp %x sp %x\n", sp->segsum, sp->bpp, sp);
			free(sp->segsum, M_SEGMENT);
			free(sp->bpp, M_SEGMENT);
			free(sp, M_SEGMENT);
		}
	}
}


static void
lfs_endsum(fs, sp, calc_next)
	LFS *fs;
	SEGMENT *sp;
	int calc_next;		/* if 1, calculate next, else -1 */
{
	BUF *bp;
	SEGSUM *ssp;
	daddr_t next_addr;
	int npages, nseg_pages;

printf("lfs_endsum\n");
	ssp = sp->segsum;
	if (!calc_next)
		ssp->ss_nextsum = (daddr_t) -1;

	nseg_pages = sp->sum_num / (fs->lfs_bsize / LFS_SUMMARY_SIZE);
	if ((sp->sum_num % (fs->lfs_bsize / LFS_SUMMARY_SIZE)) == 0) {
		/*
		 * May need to change the nextsum field on the previous
		 * summary header in which case we need to recompute the
		 * checksum as well.
		 */
		npages = nseg_pages + (sp->ninodes + INOPB(fs) - 1) / INOPB(fs);
		next_addr = fs->lfs_sboffs[0] + 
		    (sp->seg_number + 1) * fsbtodb(fs, fs->lfs_ssize)
		    - fsbtodb(fs, npages) - LFS_SUMMARY_SIZE / DEV_BSIZE;
		if (calc_next)
			ssp->ss_nextsum = next_addr;
		ssp->ss_cksum = cksum(&ssp->ss_cksum, 
		    LFS_SUMMARY_SIZE - sizeof(ssp->ss_cksum));
		bp = lfs_newbuf(fs, sp->sum_addr, fs->lfs_bsize);
		bcopy(sp->segsum, bp->b_un.b_words, fs->lfs_bsize);
		bp->b_flags |= B_BUSY;
		if (nseg_pages != 1) {
			bp->b_flags |= B_CALL;
			bp->b_iodone = lfs_biocallback;
		}
		brelse(bp);
		sp->bpp[fs->lfs_ssize - npages] = bp;
		sp->segsum = (SEGSUM *)(sp->segsum + fs->lfs_bsize - 
		    LFS_SUMMARY_SIZE);
		sp->sum_addr = next_addr;
	} else {
		sp->sum_addr -= LFS_SUMMARY_SIZE / DEV_BSIZE;
		ssp->ss_nextsum = sp->sum_addr;
		/* Calculate cksum on previous segment summary */
		ssp->ss_cksum = cksum(&ssp->ss_cksum, 
		    LFS_SUMMARY_SIZE - sizeof(ssp->ss_cksum));
		sp->segsum -= LFS_SUMMARY_SIZE;
	}
}

static BUF *
lfs_newbuf(fs, daddr, size)
	LFS *fs;
	daddr_t daddr;
	size_t size;
{
	BUF *bp;
	VNODE *devvp;

printf("lfs_newbuf\n");
	bp = getnewbuf();
	bremhash(bp);

	/*
	 * XXX
	 * Need a devvp, but this isn't a particularly clean way to get one.
	 */
	devvp = VTOI(fs->lfs_ivnode)->i_devvp;
	bgetvp(devvp, bp);
	bp->b_bcount = 0;
	bp->b_lblkno = daddr;
	bp->b_blkno = daddr;
	bp->b_error = 0;
	bp->b_resid = 0;
	binshash(bp, BUFHASH(devvp, daddr));
	allocbuf(bp, size);
	return (bp);
}


/*
 * Start a new segment
 */
static SEGMENT *
lfs_newseg(fs)
	LFS *fs;
{
	SEGMENT *sp;
	SEGUSE *sup;

printf("lfs_newseg\n");
	/* Get buffer space to write out a segment */
	sp = malloc(sizeof(SEGMENT), M_SEGMENT, M_WAITOK);
	sp->cbpp = sp->bpp =
	    malloc(fs->lfs_ssize * sizeof(BUF *), M_SEGMENT, M_WAITOK);
	sp->nextp = NULL;
	sp->sum_bytes_left = LFS_SUMMARY_SIZE;
	sp->seg_bytes_left = (fs->lfs_segmask + 1) - LFS_SUMMARY_SIZE;
	sp->saddr = fs->lfs_nextseg;
	sp->sum_addr = sp->saddr + sp->seg_bytes_left / DEV_BSIZE;
	sp->ninodes = 0;
	sp->sum_num = -1;
	sp->seg_number = (sp->saddr - fs->lfs_sboffs[0]) /
	    fsbtodb(fs, fs->lfs_ssize);

	/* initialize segment summary info */
	lfs_newsum(fs, sp);
	sup = fs->lfs_segtab + sp->seg_number;

	if (sup->su_nbytes != 0) {
		/* This is a segment containing a super block */
		FINFO *fip;
		daddr_t lbn, *lbnp;

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
	void *sum;

printf("lfs_newsum\n");
	sp->sum_num++;
	if (sp->sum_num == 0) {
		sum = malloc(fs->lfs_bsize, M_SEGMENT, M_WAITOK);
		sp->segsum = sum + fs->lfs_bsize - LFS_SUMMARY_SIZE;
		ssp = sp->segsum;
		ssp->ss_next = fs->lfs_nextseg = lfs_nextseg(fs);
		ssp->ss_prev = fs->lfs_lastseg;
	} else {
		lfs_endsum(fs, sp, 1);
		ssp = sp->segsum;
		ssp->ss_next = ssp->ss_next;
		ssp->ss_prev = ssp->ss_prev;
	}

	/* Initialize segment summary info. */
	sp->fip = (FINFO *)(sp->segsum + sizeof(SEGSUM));
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

printf("lfs_nextseg\n");
	segnum = satosn(fs, fs->lfs_nextseg);
	for (sn = seginc(fs, sn); sn != segnum; sn = seginc(fs, sn)) {
		sup = &fs->lfs_segtab[sn];
		if (!(sup->su_flags & SEGUSE_DIRTY))
			break;
	}
	if (sn == segnum)
		panic("lfs_nextseg: file system full");		/* XXX */
	return(sntosa(fs, sn));
}

/*
 * Update the metadata that points to the blocks listed in the FIP
 * array.
 */
static
lfs_updatemeta(fs, ip, fip, bpp)
	LFS *fs;
	INODE *ip;
	FINFO *fip;
	BUF **bpp;
{
	SEGUSE *segup;
	BUF **lbpp, *bp;
	daddr_t da, iblkno;
	int error, i, oldsegnum;
	long lbn, *lbp;

printf("lfs_updatemeta\n");	
	for (lbpp= bpp, lbp = fip->fi_blocks, i = 0; 
	    i < fip->fi_nblocks; i++, lbp++, bp++) {
		lbn = *lbp;
		if (error = lfs_bmap(ip, lbn, &da))
			return(error);

		if (da) {
			oldsegnum = (da - fs->lfs_sboffs[0]) /
			    fsbtodb(fs, fs->lfs_ssize);
			segup = fs->lfs_segtab+oldsegnum;
			segup->su_lastmod = time.tv_sec;
			if ((segup->su_nbytes -= fs->lfs_bsize) < 0)
				printf("lfs_updatemeta: negative bytes %s %d\n",
					"in segment", oldsegnum);
		}

		/* Now change whoever points to lbn */
		if (lbn < NDADDR)
			ip->i_din.di_db[lbn] = (*lbpp)->b_blkno;
		else if ((lbn -= NDADDR) < NINDIR(fs)) {
printf("lfs_updatemeta: changing indirect block %d\n", S_INDIR);
			error = bread(ITOV(ip), S_INDIR, fs->lfs_bsize, 
			    NOCRED, &bp);
			if (error)
				return(error);
			bp->b_un.b_daddr[lbn] = (*lbpp)->b_blkno;
			brelse(bp);
		} else if ( (lbn = (lbn - NINDIR(fs)) / NINDIR(fs)) < 
			    NINDIR(fs)) {

			iblkno = - (lbn + NIADDR + 1);
printf("lfs_updatemeta: changing indirect block %d\n", iblkno);
			error = bread(ITOV(ip), iblkno, fs->lfs_bsize, NOCRED, 
			    &bp);
			if (error)
				return(error);
			bp->b_un.b_daddr[lbn % NINDIR(fs)] = (*lbpp)->b_blkno;
		}
		else
			return(EFBIG);
	}
	return(0);
}

/*
 * Returns 0 if the entire file fit into the current segment and
 * summary region, 1 if not.
 * XXX -- I think we need to figure out what to do if we write
 * the segment and find more dirty blocks when we're done.
 */
static SEGMENT *
lfs_writefile(sp, fs, vp)
	SEGMENT *sp;
	LFS *fs;
	VNODE *vp;
{
	register BUF *bp;
	BUF **bpp, *nbp;
	FINFO *fip;
	INODE *ip;
	int db_per_fsb, error, i;
	int ret_val, s;
	long *lbp;

	/* initialize the FINFO structure */
	ip = VTOI(vp);
printf("lfs_writefile: node %d\n", ip->i_number);
loop:
	fip = sp->fip;
	fip->fi_nblocks = 0;
	fip->fi_ino = ip->i_number;
	fip->fi_version = lfs_getversion(fs, ip->i_number);
	lbp = fip->fi_blocks;
	
	bpp = sp->cbpp;
	s = splbio();
	for (bp = vp->v_dirtyblkhd; bp; bp = nbp) {
		nbp = bp->b_blockf;
printf("lfs_writefile: disk block num %d flags %x\n", bp->b_blkno, bp->b_flags);
		if ((bp->b_flags & B_BUSY))
			continue;
		if ((bp->b_flags & B_DELWRI) == 0)
			panic("lfs_write: not dirty");
		bremfree(bp);
		bp->b_flags |= (B_BUSY | B_CALL);
		bp->b_iodone = lfs_biocallback;

		/* UFS does the bawrites and bwrites here; we don't */
		*lbp++ = bp->b_lblkno;		/* UPDATE META HERE */
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
			if (error = lfs_updatemeta(fs, ip, fip, bpp))
				panic("lfs_writefile: error from lfs_updatemeta\n");

			/* Put this file in the segment summary */
			((SEGSUM *)(sp->segsum))->ss_nfinfo++;

			if (sp->seg_bytes_left < fs->lfs_bsize) {
				lfs_writeseg(fs, sp);
				sp = lfs_newseg(fs);
			} else if (sp->sum_bytes_left < sizeof(daddr_t))
				lfs_newsum(fs, sp);
			fip = sp->fip;
			s = splbio();
		}

	}
	splx(s);
	db_per_fsb = 1 << fs->lfs_fsbtodb;
	shellsort(bpp, (u_long *)fip->fi_blocks, fip->fi_nblocks);
	for (bp = *bpp, i = 0; i < fip->fi_nblocks; i++, bp++) {
		bp->b_blkno = sp->saddr;
		sp->saddr += db_per_fsb;
		/* 
		 * Update the meta data now for this file.  If we've filled
		 * a segment, then we'll have to wait until the next segment
		 * to write out the updated metadata.
		 */
		lfs_writemeta();
	}
(void)printf("lfs_writefile: adding %d blocks to segment\n", fip->fi_nblocks);
	if (fip->fi_nblocks) {
		((SEGSUM *)(sp->segsum))->ss_nfinfo++;
		sp->fip = (FINFO *)((u_long)fip + sizeof(FINFO) + 
		    sizeof(u_long) * (fip->fi_nblocks - 1));
	}
	return(sp);
}

static void
lfs_writemeta()
{
	printf("lfs_writemeta (STUB)\n");
}

static void
lfs_writeseg(fs, sp)
	LFS *fs;
	SEGMENT *sp;
{
	BUF **bpp, *bp;
	SEGSUM *ssp;
	SEGUSE *sup;
	VNODE *devvp;
	int nblocks, nbuffers, ninode_blocks, nsegsums, nsum_pb;
	int i, metaoff, nmeta;

printf("lfs_writeseg\n");
	ssp = sp->segsum;
	nsum_pb = fs->lfs_bsize / LFS_SUMMARY_SIZE;
	/*
	 * This is a hack because we're currently allocating summary segments
	 * in full blocks.  It will go away when we do fragments, when we'll
	 * allocate fragment sized summary blocks.
	 */
	do {
		sp->sum_num++;
		lfs_endsum(fs, sp, 0);
	} while (sp->sum_num % nsum_pb);
	nbuffers = sp->cbpp - sp->bpp;
	nsegsums = (sp->sum_num + nsum_pb - 1) / nsum_pb;
	ninode_blocks = (sp->ninodes + INOPB(fs) - 1)/INOPB(fs);

	/* Do checksum for last segment summary */
	ssp->ss_cksum = cksum(&ssp->ss_cksum, 
		    LFS_SUMMARY_SIZE - sizeof(ssp->ss_cksum));

	/* Finish off any inodes */

	/*
	 * Copy inode and summary block buffer pointers down so they are
	 * contiguous with the page buffer pointers
	 */
	nmeta = 1 + ninode_blocks + nsegsums;
	metaoff = fs->lfs_ssize - nmeta;
	if (sp->bpp + metaoff != sp->cbpp)
		bcopy(sp->bpp+metaoff, sp->cbpp, sizeof(BUF *)  * nmeta);

	nblocks = nbuffers + ninode_blocks + nsegsums;
	
	sup = fs->lfs_segtab + sp->seg_number;
	sup->su_nbytes = nblocks << fs->lfs_bshift;
	sup->su_lastmod = time.tv_sec;
	sup->su_flags = SEGUSE_DIRTY;

	/*
	 * Since we need to guarantee that our last buffer gets written last,
	 * we issue the writes in two sets.  The first n-1 buffers first, and
	 * then, after they've completed, the last 1 buffer.  Only when that
	 * final write completes is the segment actually written.
	 */
	devvp = VFSTOUFS(fs->lfs_ivnode->v_mount)->um_devvp;
/* MIS -- THIS COULD BE BAD IF WE GOT INTERRUPTED IN THE MIDDLE OF THIS */
	fs->lfs_iocount += nblocks - 1;
	sp->nextp = fs->lfs_seglist;
	fs->lfs_seglist = sp;
	for (bpp = sp->bpp, i = 0; i < (nblocks - 1); i++) {
		bp = *bpp;
printf("lfs_writeseg: buffer: ino %d lbn %d flags %lx\n", VTOI(bp->b_vp)->i_number, bp->b_lblkno, bp->b_flags);
		(*(devvp->v_op->vop_strategy))(*bpp++);
	}
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
	u_long *lb_array;
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
