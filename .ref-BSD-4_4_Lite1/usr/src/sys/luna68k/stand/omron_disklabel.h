/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
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
 *	@(#)omron_disklabel.h	8.1 (Berkeley) 6/10/93
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
