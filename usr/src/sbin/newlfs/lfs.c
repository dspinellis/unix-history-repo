/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lfs.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/disklabel.h>
#include <ufs/dinode.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "lfs.h"				/* XXX move to <sys/lfs.h> */
#include "config.h"
#include "extern.h"

static LFS_SUPER lfs_default =  {
	/* lfs_magic */		LFS_MAGIC,
	/* lfs_version */	LFS_VERSION,
	/* lfs_size */		0,
	/* lfs_ssize */		0,
	/* lfs_dsize */		0,
	/* lfs_bsize */		DFL_LFSBLOCK,
	/* lfs_fsize */		0,
	/* lfs_frag */		0,
	/* lfs_free */		LFS_FIRST_INUM,
	/* lfs_idaddr */	0,
	/* lfs_ifile */		LFS_IFILE_INUM,
	/* lfs_lastseg */	0,
	/* lfs_tstamp */	0,
	/* lfs_minfree */	MINFREE,
	/* lfs_inopb */		DFL_LFSBLOCK/sizeof(struct dinode),
	/* lfs_ifpb */		DFL_LFSBLOCK/sizeof(IFILE),
	/* lfs_nindir */	DFL_LFSBLOCK/sizeof(daddr_t),
	/* lfs_nseg */		0,
	/* lfs_nspf */		0,
	/* lfs_segtabsz */	0,
	/* lfs_segmask */	DFL_LFSSEG_MASK,
	/* lfs_segshift */	DFL_LFSSEG_SHIFT,
	/* lfs_bmask */		DFL_LFSBLOCK_MASK,
	/* lfs_bshift */	DFL_LFSBLOCK_SHIFT,
	/* lfs_ffmask */	0,
	/* lfs_ffshift */	0,
	/* lfs_fbmask */	0,
	/* lfs_fbshift */	0,
	/* lfs_fsbtodb */	0,
	/* lfs_sboffs */	{ 0 },
	/* lfs_fmod */		0,
	/* lfs_clean */		0,
	/* lfs_ronly */		0,
	/* lfs_flags */		0,
	/* lfs_fsmnt */		{ 0 },
	/* lfs_cksum */		0
};

static void put __P((int, off_t, void *, size_t));

int
make_lfs(fd, lp, partp, minfree, block_size, seg_size)
	int fd;
	struct disklabel *lp;
	struct partition *partp;
	int minfree;
	int block_size;
	int seg_size;
{
	struct dinode *dip;	/* Pointer to a disk inode */
	FINFO file_info;	/* File info structure in summary blocks */
	IFILE *ifile;		/* Pointer to array of ifile structures */
	IFILE *ip;		/* Pointer to array of ifile structures */
	LFS_SUPER *lfsp;	/* Superblock */
	SEGUSAGE *segp;		/* Segment usage table */
	SEGUSAGE *segtable;	/* Segment usage table */
	SEGSUM summary;		/* Segment summary structure */
	SEGSUM *sp;		/* Segment summary pointer */
	daddr_t	last_sb_addr;	/* Address of superblocks */
	daddr_t	sb_addr;	/* Address of superblocks */
	void *pagep;		/* Pointer to the page we use to write stuff */
	void *sump;		/* Used to copy stuff into segment buffer */
	u_long *block_array;	/* Array of logical block nos to put in sum */
	int block_array_size;	/* How many entries in block array */
	int bsize;		/* Block size */
	int db_per_fb;		/* Disk blocks per file block */
	int i;
	int sb_to_sum;		/* offset between superblock and summary */
	int sb_interval;	/* number of segs between super blocks */
	int seg_seek;		/* Seek offset for a segment */
	int ssize;		/* Segment size */
	int sum_size;		/* Size of the summary block */
	int wbytes;		/* Number of bytes returned by write */

	lfsp = &lfs_default;

	if (!(bsize = block_size))
		bsize = DFL_LFSBLOCK;
	if (!(ssize = seg_size))
		ssize = DFL_LFSSEG;

	/* Modify parts of superblock overridden by command line arguments */
	if (bsize != DFL_LFSBLOCK) {
		lfsp->lfs_bshift = log2(bsize);
		if (1 << lfsp->lfs_bshift != bsize)
			fatal("%d: block size not a power of 2", bsize);
		lfsp->lfs_bsize = bsize;
		lfsp->lfs_bmask = bsize - 1;
		lfsp->lfs_inopb = bsize / sizeof(struct dinode);
/* MIS -- should I round to power of 2 */
		lfsp->lfs_ifpb = bsize / sizeof(IFILE);
		lfsp->lfs_nindir = bsize / sizeof(daddr_t);
	}

	if (ssize != DFL_LFSSEG) {
		lfsp->lfs_segshift = log2(ssize);
		if (1 << lfsp->lfs_segshift != ssize)
			fatal("%d: segment size not power of 2", ssize);
		lfsp->lfs_ssize = ssize;
		lfsp->lfs_segmask = ssize - 1;
	}
	lfsp->lfs_ssize = ssize >> lfsp->lfs_bshift;

	if (minfree)
		lfsp->lfs_minfree = minfree;

	/*
	 * Fill in parts of superblock that can be computed from file system
	 * size, disk geometry and current time.
	 */
	db_per_fb = bsize/lp->d_secsize;
	lfsp->lfs_fsbtodb = log2(db_per_fb);
	lfsp->lfs_size = partp->p_size >> lfsp->lfs_fsbtodb;
	lfsp->lfs_dsize = lfsp->lfs_size - (LFS_LABELPAD >> lfsp->lfs_bshift);
	lfsp->lfs_nseg = lfsp->lfs_dsize / lfsp->lfs_ssize;
	lfsp->lfs_segtabsz = SEGTABSIZE_SU(lfsp);
	if ((lfsp->lfs_tstamp = time(NULL)) == -1)
		fatal("time: %s", strerror(errno));
	if ((sb_interval = lfsp->lfs_nseg / LFS_MAXNUMSB) < LFS_MIN_SBINTERVAL)
		sb_interval = LFS_MIN_SBINTERVAL;

	/*
	 * Now, lay out the file system.  We need to figure out where
	 * the superblocks go, initialize the checkpoint information
	 * for the first two superblocks, initialize the segment usage
	 * information, put the segusage information in the ifile, create
	 * the first block of IFILE structures, and link all the IFILE
	 * structures into a free list.
	 */

	/* Figure out where the superblocks are going to live */
	lfsp->lfs_sboffs[0] = LFS_LABELPAD/lp->d_secsize;
	for (i = 1; i < LFS_MAXNUMSB; i++) {
		sb_addr = ((i * sb_interval) << 
		    (lfsp->lfs_segshift - lfsp->lfs_bshift + lfsp->lfs_fsbtodb))
		    + lfsp->lfs_sboffs[0];
		if (sb_addr > partp->p_size)
			break;
		lfsp->lfs_sboffs[i] = sb_addr;
	}
	last_sb_addr = lfsp->lfs_sboffs[i - 1];
	lfsp->lfs_lastseg = lfsp->lfs_sboffs[0];

	/*
	 * Initialize the segment usage table.  The first segment will
	 * contain the superblock, the segusage table (segtabsz), 1
	 * block's worth of IFILE entries, and one block's worth of inodes
	 * (containing the ifile inode).
	 */
	if (!(segtable = malloc(lfsp->lfs_segtabsz << lfsp->lfs_bshift)))
		fatal("%s", strerror(errno));
	segp = segtable;
	segp->su_nbytes =
	    LFS_SBPAD + (lfsp->lfs_segtabsz + 2 << lfsp->lfs_bshift);
	segp->su_lastmod = lfsp->lfs_tstamp;
	segp->su_flags = SEGUSAGE_DIRTY;

	/* Now use su_nbytes to figure out the daddr of the ifile inode */
	lfsp->lfs_idaddr = (((segp->su_nbytes >> lfsp->lfs_bshift) - 1) <<
	    lfsp->lfs_fsbtodb) + lfsp->lfs_sboffs[0];

	for (segp = segtable + 1, i = 1; i < lfsp->lfs_nseg; i++, segp++) {
		if ((i % sb_interval) == (sb_interval - 1))
			segp->su_nbytes = LFS_SBPAD;
		else
			segp->su_nbytes = 0;
		segp->su_lastmod = 0;
		segp->su_flags = 0;
	}

	/*
	 * Ready to start writing segments.  The first segment is different
	 * because it contains the segment usage table and the ifile inode
	 * as well as a superblock.  We don't have to write any segments which
	 * don't contain superblocks since they are marked as clean and do not
	 * containing any live bytes, so the only other segments we need to
	 * write are those containing superblock info.  For all these segments,
	 * set the time stamp to be 0 so that the first superblock looks like
	 * the most recent.
	 */
	lfsp->lfs_cksum = 
	    cksum(lfsp, sizeof(LFS_SUPER) - sizeof(lfsp->lfs_cksum));
	put(fd, LFS_LABELPAD, lfsp, sizeof(LFS_SUPER));
	put(fd, LFS_LABELPAD + LFS_SBPAD, segtable,
	    lfsp->lfs_segtabsz << lfsp->lfs_bshift);
	(void)free(segtable);

	/* Create the first block of the ifile. */
	if (!(pagep = malloc(lfsp->lfs_bsize)))
		fatal("%s", strerror(errno));
	ifile = (IFILE *)pagep;
	for (ip = &ifile[2], i = 2; i < lfsp->lfs_ifpb; ++ip) {
		ip->if_version = 1;
		ip->if_daddr = LFS_UNUSED_DADDR;
		ip->if_nextfree = ++i;
	}
	ifile[lfsp->lfs_ifpb - 1].if_nextfree = LFS_UNUSED_INUM;
	ip = &ifile[LFS_IFILE_INUM];
	ip->if_version = 1;
	ip->if_daddr = lfsp->lfs_idaddr;
	ip->if_st_atime = lfsp->lfs_tstamp;

	if ((wbytes = write(fd, ifile, lfsp->lfs_bsize)) < 0)
		fatal("%s: %s", special, strerror(errno));
	if (wbytes != lfsp->lfs_bsize)
		fatal("%s: short write (%d, not %d)",
		    special, wbytes, lfsp->lfs_bsize);

	/* Now create a block of disk inodes */
	dip = (struct dinode *)pagep;
	bzero(dip, sizeof(struct dinode));
	dip->di_mode = IFREG;
	dip->di_nlink = 1;
	dip->di_blocks = lfsp->lfs_segtabsz + 1;
	/* If we ever need something longer than 32 bits, this changes */
	dip->di_size = (dip->di_blocks << lfsp->lfs_bshift);
	dip->di_atime = dip->di_mtime = dip->di_ctime = lfsp->lfs_tstamp;
	dip->di_inum = LFS_IFILE_INUM;
#define	SEGERR \
"Segusage table requires too many blocks; increase block or segment size."
	if (NDADDR < lfsp->lfs_segtabsz)
		fatal("%s", SEGERR);
	sb_addr = (LFS_LABELPAD + LFS_SBPAD) / lp->d_secsize;

	/* Assign the block addresses for the ifile */
	for (i = 0; i < dip->di_blocks; i++, sb_addr += db_per_fb)
		dip->di_db[i] = sb_addr;

	/* Make all the other dinodes invalid */
	for (i = 1, dip++; i < lfsp->lfs_inopb; i++, dip++)
		dip->di_inum = LFS_UNUSED_INUM;

	/* Finally, write out the inode block */
	if ((wbytes = write(fd, pagep, lfsp->lfs_bsize)) < 0)
		fatal("%s: %s", special, strerror(errno));
	if (wbytes != lfsp->lfs_bsize)
		fatal("%s: short write (%d, not %d)",
		    special, wbytes, lfsp->lfs_bsize);

	/* MIS -- probably want to replace with "write block code" */
	/* Now it's time to write the summary for the first segment. */
	summary.ss_next =
	    lfsp->lfs_sboffs[1] ? lfsp->lfs_sboffs[1] : lfsp->lfs_sboffs[0];
	summary.ss_prev = last_sb_addr;
	summary.ss_nextsum = -1;
	summary.ss_create = lfsp->lfs_tstamp;
	summary.ss_nfinfo = 2;
	summary.ss_ninos = 1;

	/* Superblock and disk label */
	file_info.fi_nblocks = LFS_SBPAD >> lfsp->lfs_bshift;
	file_info.fi_version = 1;
	file_info.fi_ino = LFS_UNUSED_INUM;

	/* Make sure that we don't overflow a summary block. */
	sum_size = 2*sizeof(FINFO) + sizeof(SEGSUM) +
	    file_info.fi_nblocks * sizeof(u_long) +
	    (lfsp->lfs_segtabsz + 1) * sizeof(u_long);
#define	SUMERR \
"Multiple summary blocks in segment 1 not yet implemented\nsummary is %d bytes."
	if (sum_size > LFS_SUMMARY_SIZE)
		fatal(SUMERR, sum_size);

	block_array_size = file_info.fi_nblocks;
	if ((lfsp->lfs_segtabsz + 1) > block_array_size)
		block_array_size = lfsp->lfs_segtabsz + 1;

	if (!(block_array = malloc(block_array_size *sizeof(int))))
		fatal("%s: %s", special, strerror(errno));

	/* fill in the array */
	for (i = 0; i < file_info.fi_nblocks; i++)
		block_array[i] = i;

	/* copy into segment */
	sump = pagep;
	bcopy(&summary, sump, sizeof(SEGSUM));
	sump += sizeof(SEGSUM);
	bcopy(&file_info, sump, sizeof(FINFO) - sizeof(u_long));
	sump += sizeof(FINFO) - sizeof(u_long);
	bcopy(block_array, sump, sizeof(u_long) * file_info.fi_nblocks);
	sump += sizeof(u_long) * file_info.fi_nblocks;

	/* Now, add the ifile */
	file_info.fi_nblocks = lfsp->lfs_segtabsz + 1;
	file_info.fi_version = 1;
	file_info.fi_ino = LFS_IFILE_INUM;

	for (i = 0; i < file_info.fi_nblocks; i++)
		block_array[i] = i;

	bcopy(&file_info, sump, sizeof(FINFO) - sizeof(u_long));
	sump += sizeof(FINFO) - sizeof(u_long);
	bcopy(block_array, sump, sizeof(u_long) * file_info.fi_nblocks);

	sb_to_sum = (lfsp->lfs_ssize << lfsp->lfs_bshift) - LFS_SUMMARY_SIZE;
	((SEGSUM *)pagep)->ss_cksum = cksum(pagep+sizeof(summary.ss_cksum), 
	    LFS_SUMMARY_SIZE - sizeof(summary.ss_cksum));
	put(fd, LFS_LABELPAD + sb_to_sum, pagep, LFS_SUMMARY_SIZE);

	summary.ss_nextsum = -1;
	summary.ss_create = 0;
	summary.ss_nfinfo = 1;
	summary.ss_ninos = 0;

	/* Superblock */
	file_info.fi_nblocks = LFS_SBPAD >> lfsp->lfs_bshift;
	file_info.fi_version = 1;
	file_info.fi_ino = LFS_UNUSED_INUM;

	for (i = 0; i < file_info.fi_nblocks; i++)
		block_array[i] = i;

	sump = pagep;
	bcopy (&summary, sump, sizeof(SEGSUM));
	sump += sizeof(SEGSUM);
	bcopy(&file_info, sump, sizeof(FINFO) - sizeof(u_long));
	sump += sizeof(FINFO) - sizeof(u_long);
	bcopy(block_array, sump, sizeof(u_long) * file_info.fi_nblocks);

	/* Now, write rest of segments containing superblocks */
	lfsp->lfs_tstamp = 0;
	for (sp = (SEGSUM *)pagep, i = 1; i < LFS_MAXNUMSB; i++) {
		if (!lfsp->lfs_sboffs[i])
			break;
		sp->ss_prev = lfsp->lfs_sboffs[i - 1];
		if (i < (LFS_MAXNUMSB - 1))
			sp->ss_next = lfsp->lfs_sboffs[i + 1];
		else
			sp->ss_next = lfsp->lfs_sboffs[0];

		/* Superblock */
		seg_seek = lfsp->lfs_sboffs[i] * lp->d_secsize;
		lfsp->lfs_cksum = 
		    cksum(lfsp, sizeof(LFS_SUPER) - sizeof(lfsp->lfs_cksum));
		put(fd, seg_seek, lfsp, sizeof(LFS_SUPER));

		/* Summary */
		sp->ss_cksum = cksum(&sp->ss_cksum, 
		    LFS_SUMMARY_SIZE - sizeof(sp->ss_cksum));
		put(fd, sb_to_sum + seg_seek, pagep, LFS_SUMMARY_SIZE);
	}
	(void)free(pagep);
	(void)close(fd);
	return (0);
}

static void
put(fd, off, p, len)
	int fd;
	off_t off;
	void *p;
	size_t len;
{
	int wbytes;

	if (lseek(fd, off, SEEK_SET) < 0)
		fatal("%s: %s", special, strerror(errno));
	if ((wbytes = write(fd, p, len)) < 0)
		fatal("%s: %s", special, strerror(errno));
	if (wbytes != len)
		fatal("%s: short write (%d, not %d)", special, wbytes, len);
}
