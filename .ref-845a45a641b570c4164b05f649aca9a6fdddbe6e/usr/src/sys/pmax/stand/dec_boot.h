/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dec_boot.h	7.3 (Berkeley) %G%
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
 
#define DEC_BOOT_MAGIC		0x0002757a
#define DEC_BOOT_SECTOR		0

/*
 * DEC_NUM_DISK_PARTS is the number of partitions that are recorded in
 * the label information.  The size of the padding in the Dec_DiskLabel
 * type is dependent on this number...
 */
#define DEC_NUM_DISK_PARTS	8

/*
 * A disk is divided into partitions and this type specifies where a
 * partition starts and how many bytes it contains.
 */
typedef struct Dec_DiskMap {
	int	numBlocks;	/* Number of 512 byte blocks in partition. */
	int	startBlock;	/* Start of partition in blocks. */
} Dec_DiskMap;

/*
 * Label information on the 31st (DEC_LABEL_SECTOR) sector.
 */
typedef struct Dec_DiskLabel {
    char	pad0[440];		/* DIFFERENT from sprite!!! */
    int		magic;			/* DEC_LABEL_MAGIC */
    int		isPartitioned;		/* 1 if disk is partitioned. */
    Dec_DiskMap map[DEC_NUM_DISK_PARTS]; /* Indicates disk partitions. */
} Dec_DiskLabel;

#define DEC_LABEL_MAGIC		0x00032957
#define DEC_LABEL_SECTOR	31
