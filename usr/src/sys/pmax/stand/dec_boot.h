/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dec_boot.h	7.2 (Berkeley) %G%
 *
 * devDiskLabel.h --
 *
 *      This defines the disk label that Sun writes on the 0'th sector of
 *      the 0'th cylinder of its SMD disks.  The disk label contains some
 *      geometry information and also the division of the disk into a
 *      number of partitions.  Each partition is identified to the drive
 *      by a different unit number.
 *
 * from: $Header: /sprite/src/kernel/dev/RCS/devDiskLabel.h,
 *	v 9.4 90/03/01 12:22:36 jhh Exp $ SPRITE (Berkeley)
 */

/*
 * Boot block information on the 0th sector.
 * The boot program is stored in sequences of contiguous blocks.
 * If mode is 0, there is just one sequence of blocks and one Dec_BootMap
 * is used.  If mode is 1, there are multiple sequences of blocks
 * and multiple Dec_BootMaps are used, the last with numBlocks = 0.
 *
 * NOTE: The standard disk label offset is 64 which is
 * after the boot information expected by the PROM boot loader.
 */

struct Dec_BootMap {
	int	numBlocks;		/* Number of blocks to read. */
	int	startBlock;		/* Starting block on disk. */
};

struct Dec_DiskBoot {
	char	pad[8];
	int	magic;			/* DEC_BOOT_MAGIC */
	int	mode;			/* Mode for boot info. */
	int	loadAddr;		/* Address to start loading. */
	int	execAddr;		/* Address to start execing. */
	struct	Dec_BootMap map[61];	/* boot program sections. */
};
 
#define DEC_BOOT_MAGIC	0x02757a
#define DEC_BOOT_SECTOR	0
