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
 * from: Utah $Hdr: itereg.h 1.3 92/01/21$
 *
 *	@(#)itereg.h	7.3 (Berkeley) %G%
 */

/*
 * Offsets into the display ROM that is part of the first 4K of each
 * display device.
 */
#define FONTROM		0x3B	/* Offset of font information structure. */
#define FONTADDR	0x4	/* Offset from FONTROM to font address. */
#define FONTHEIGHT	0x0	/* Offset from font address to font height. */
#define FONTWIDTH	0x2	/* Offset from font address to font width. */
#define FONTDATA	0xA	/* Offset from font address to font glyphs. */

#ifdef hp300
#define FBBASE		((volatile u_char *)ip->fbbase)
#else
#define FBBASE		((volatile u_long *)ip->fbbase)
#endif
