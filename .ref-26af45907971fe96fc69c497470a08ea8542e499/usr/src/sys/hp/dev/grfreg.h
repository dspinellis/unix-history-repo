/*
 * Copyright (c) 1991 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: grfreg.h 1.6 92/01/31$
 *
 *	@(#)grfreg.h	7.1 (Berkeley) %G%
 */

/* 300 bitmapped display hardware primary id */
#define GRFHWID		0x39

/* 300 internal bitmapped display address */
#define GRFIADDR	0x560000

/* 300 hardware secondary ids */
#define GID_GATORBOX	1
#define	GID_TOPCAT	2
#define GID_RENAISSANCE	4
#define GID_LRCATSEYE	5
#define GID_HRCCATSEYE	6
#define GID_HRMCATSEYE	7
#define GID_DAVINCI	8
#define GID_XXXCATSEYE	9
#define GID_XGENESIS   11
#define GID_TIGER      12
#define GID_YGENESIS   13
#define GID_HYPERION   14

typedef unsigned char	grftype;

struct	grfreg {
	grftype	gr_pad0,
		gr_id,		/* +0x01 */
		gr_pad1[0x3],
		gr_fbwidth_h,	/* +0x05 */
		gr_pad2,
		gr_fbwidth_l,	/* +0x07 */
		gr_pad3,
		gr_fbheight_h,	/* +0x09 */
		gr_pad4,
		gr_fbheight_l,	/* +0x0B */
		gr_pad5,
		gr_dwidth_h,	/* +0x0D */
		gr_pad6,
		gr_dwidth_l,	/* +0x0F */
		gr_pad7,
		gr_dheight_h,	/* +0x11 */
		gr_pad8,
		gr_dheight_l,	/* +0x13 */
		gr_pad9,
		gr_id2,		/* +0x15 */
		gr_pad10[0x47],
		gr_fbomsb,	/* +0x5d */
		gr_pad11,
		gr_fbolsb;	/* +0x5f */
};
