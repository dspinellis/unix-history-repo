/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dec_boot.h	7.1 (Berkeley) %G%
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

#ifndef _DEVDISKLABEL
#define _DEVDISKLABEL

/*
 * SUN_NUM_DISK_PARTS is the number of partitions that are recorded in
 * the label information.  The size of the padding in the Sun_DiskLabel
 * type is dependend on this number...
 */
#define SUN_NUM_DISK_PARTS  8

/*
 * A disk is divided into partitions and this type specifies where a
 * partition starts and how many blocks (sectors) it contains.
 */
typedef struct Sun_DiskMap {
    int cylinder;
    int numBlocks;
} Sun_DiskMap;

/*
 * Label information on the zero'th sector.  The partition map and the
 * number of heads, sectors, and cylinders are used by the disk driver to
 * map from a block number in a disk partion to a block number on the raw
 * disk.  The gap and interleaving information are used by the block
 * allocation routines.
 */
typedef struct Sun_DiskLabel {
    char    asciiLabel[128];    	/* For compatibility with other systems
					 * that write an ascii label on a disk*/
    char    pad[512-(128+8*8+11*2)];	/* The sector size is 512 bytes, and
					 * padding puts our label info at the
					 * end of the sector. */
    unsigned short  gap1;		/* size of gap 1 */
    unsigned short  gap2;		/* size of gap 2 */
    unsigned short  interleave;		/* interleave factor */
    unsigned short  numCylinders;	/* # of data cylinders */
    unsigned short  numAltCylinders;	/* # of alternate cylinders */
    unsigned short  numHeads;		/* # of heads in this partition */
    unsigned short  numSectors;		/* # of 512 byte sectors per track */
    unsigned short  bhead;		/* ?? Head number under which this
					 * label is found, consistency chk */
    unsigned short  partitionID;	/* physical partition # */
    Sun_DiskMap map[SUN_NUM_DISK_PARTS]; /* Indicates the size and starting
					 * cylinder of the disk partitions */
    unsigned short  magic;		/* Identifies this label format */
    unsigned short  checkSum;		/* XOR checksum of sector */
} Sun_DiskLabel;
 
#define SUN_DISK_MAGIC       0xDABE

/*
 * The sun label does not describe the location of the filesystem header
 * information that comes after the zero'th label sector.  (The sprite label
 * will, but is never used.)  Instead, the following constants are used.
 * SUN_SUMMARY_SECTOR  one sector of summary info
 * SUN_DOMAIN_SECTOR  the first sector of the static domain header
 */
#define SUN_SUMMARY_SECTOR	17
#define SUN_DOMAIN_SECTOR	18

/* 
 * SUN_LABEL_SECTOR -- sector holding the disk label
 * SUN_BOOT_SECTOR -- first sector of the boot program
 */
#define SUN_LABEL_SECTOR 0
#define SUN_BOOT_SECTOR 1

/*
 * DEC_NUM_DISK_PARTS is the number of partitions that are recorded in
 * the label information.  The size of the padding in the Dec_DiskLabel
 * type is dependend on this number...
 */
#define DEC_NUM_DISK_PARTS  8

/*
 * A disk is divided into partitions and this type specifies where a
 * partition starts and how many bytes it contains.
 */
typedef struct Dec_DiskMap {
    int numBlocks;	/* Number of 512 byte blocks in partition. */
    int startBlock;	/* Start of partition in blocks. */
} Dec_DiskMap;

typedef struct Dec_BootMap {
    int numBlocks;	/* Number of blocks to read. */
    int startBlock;	/* Starting block on disk. */
} Dec_BootMap;

/*
 * Label information on the 31st (DEC_LABEL_SECTOR)  sector.
 */
typedef struct Dec_DiskLabel {
    char	pad0[440];		/* DIFFERENT from sprite!!! */
    int		magic;			/* DEC_LABEL_MAGIC */
    int		isPartitioned;		/* 1 if disk is partitioned. */
    Dec_DiskMap map[DEC_NUM_DISK_PARTS]; /* Indicates disk partitions. */
} Dec_DiskLabel;

/*
 * Boot block information on the 0th sector.
 * The boot program is stored in sequences of contiguous blocks.
 * If mode is 0, there is just one sequence of blocks and one Dec_BootMap
 * is used.  If mode is 1, there are multiple sequences of blocks
 * and multiple Dec_BootMaps are used, the last with numBlocks = 0.
 */
typedef struct Dec_DiskBoot {
    char	pad[8];
    int		magic;			/* DEC_BOOT_MAGIC */
    int		mode;			/* Mode for boot info. */
    int		loadAddr;		/* Address to start loading. */
    int		execAddr;		/* Address to start execing. */
    Dec_BootMap	map[61];		/* Position of boot program. */
} Dec_DiskBoot;
 
#define DEC_BOOT_MAGIC	0x02757a
#define DEC_LABEL_MAGIC	0x032957

/*
 * The following default values are used:
 * DEC_BOOT_SECTOR	The sector holding the boot information.
 * DEC_SUMMARY_SECTOR  one sector of summary info
 * DEC_DOMAIN_SECTOR  the first sector of the static domain header
 * DEC_LABEL_SECTOR	The sector holding the disk label.
 */
#define DEC_BOOT_SECTOR		0
#define DEC_SUMMARY_SECTOR	17
#define DEC_DOMAIN_SECTOR	18
#define DEC_LABEL_SECTOR	31

/*
 * There are generally 8 disk partitions defined for a disk.
 */
#define DEV_NUM_DISK_PARTS	8

#endif /* _DEVDISKLABEL */
