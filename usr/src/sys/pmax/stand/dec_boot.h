/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)dec_boot.h	8.1 (Berkeley) 6/10/93
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
