/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)omron_disklabel.h	7.1 (Berkeley) %G%
 */

/* omron_dislabel.h from UniOS-B */
/*    by A.Fujita, JAN-30-1992   */


#define DKL_MAGIC	0xDABE 			/* Disk label Magic number */
#define NLPART		8			/* # of logical partition */ 

struct scd_dk_label {
	char	dkl_asciilabel[128];		/* for compatibility */
	char	dkl_pad[512-(128+8*8+11*2+4)];
	unsigned short	dkl_badchk;		/* checksum of bad track */
	unsigned long	dkl_maxblk;		/* # of total logical block */
	unsigned short	dkl_dtype;		/* disk drive type */
	unsigned short	dkl_ndisk;		/* # of disk drives */
	unsigned short	dkl_ncyl;		/* # of data cylinders */
	unsigned short	dkl_acyl;		/* # of alternate cylinders */
	unsigned short	dkl_nhead;		/* # of heads in this partition */
	unsigned short	dkl_nsect;		/* # of 512 byte sectors per track */
	unsigned short	dkl_bhead;		/* identifies proper label locations */
	unsigned short	dkl_ppart;		/* physical partition # */
	struct dk_map {				/* logical partitions */
		daddr_t	dkl_blkno;		/* starting block */
		daddr_t dkl_nblk;		/* number of blocks */
	} dkl_map[NLPART];
	unsigned short	dkl_magic;		/* identifies this label format */
	unsigned short	dkl_cksum;		/* xor checksum of sector */
};
