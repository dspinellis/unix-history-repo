/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: cdvar.h 1.1 90/07/09$
 *
 *	@(#)cdvar.h	7.2 (Berkeley) %G%
 */

#define	NCDISKS	8			/* max # of component disks */

/*
 * A concatenated disk is described at config time by this structure.
 */
struct cddevice {
	int	cd_unit;		/* logical unit of this cd */
	int	cd_interleave;		/* interleave (DEV_BSIZE blocks) */
	int	cd_flags;		/* misc. information */
	int	cd_dk;			/* disk number */
	dev_t	cd_dev[NCDISKS];	/* component devices */
};

/* cd_flags */
#define	CDF_SWAP	0x01	/* interleave should be dmmax */
#define CDF_UNIFORM	0x02	/* use LCD of sizes for uniform interleave */

/*
 * Component info table.
 * Describes a single component of a concatenated disk.
 */
struct cdcinfo {
	dev_t		ci_dev;	 /* devno */
	size_t		ci_size; /* size */
};

/*
 * Interleave description table.
 * Computed at boot time to speed irregular-interleave lookups.
 * The idea is that we interleave in "groups".  First we interleave
 * evenly over all component disks up to the size of the smallest
 * component (the first group), then we interleave evenly over all
 * remaining disks up to the size of the next-smallest (second group),
 * and so on.
 *
 * Each table entry describes the interleave characteristics of one
 * of these groups.  For example if a concatenated disk consisted of
 * three components of 5, 3, and 7 DEV_BSIZE blocks interleaved at
 * DEV_BSIZE (1), the table would have three entries:
 *
 *	ndisk	startblk	startoff	dev
 *	3	0		0		0, 1, 2
 *	2	9		3		0, 2
 *	1	13		5		2
 *	0	-		-		-
 *
 * which says that the first nine blocks (0-8) are interleaved over
 * 3 disks (0, 1, 2) starting at block offset 0 on any component disk,
 * the next 4 blocks (9-12) are interleaved over 2 disks (0, 2) starting
 * at component block 3, and the remaining blocks (13-14) are on disk
 * 2 starting at offset 5.
 */
struct cdiinfo {
	int	ii_ndisk;	/* # of disks range is interleaved over */
	daddr_t	ii_startblk;	/* starting scaled block # for range */
	daddr_t	ii_startoff;	/* starting component offset (block #) */
	char	ii_index[NCDISKS];/* ordered list of components in range */
};

#ifdef KERNEL
extern	struct cddevice cddevice[];
#endif
