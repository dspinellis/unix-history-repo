/* Copyright (C) 1989 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* gxbitmap.h */
/* Definitions for stored bitmaps for Ghostscript */

#ifndef gxbitmap_INCLUDED
#  define gxbitmap_INCLUDED

/*
 * Drivers such as the X driver and the command list (band list) driver
 * benefit greatly by being able to cache bitmaps (tiles and characters)
 * and refer to them later.  To support this, we define a bitmap ID type
 * which the kernel passes to the driver on each copy_ or tile_ operation.
 */
typedef unsigned long gx_bitmap_id;
#define gx_no_bitmap_id 0L

/*
 * Structure for describing stored bitmaps.
 * Bitmaps are stored bit-big-endian (i.e., the 2^7 bit of the first
 * byte corresponds to x=0), as a sequence of bytes (i.e., you can't
 * do word-oriented operations on them if you're on a little-endian
 * platform like the Intel 80x86 or VAX).  Each scan line must start on
 * a (32-bit) word boundary, and hence must be is padded to a word boundary,
 * although this should rarely be of concern, since the raster and width
 * are specified individually.  The first scan line corresponds to y=0
 * in whatever coordinate system is relevant.
 *
 * For bitmaps used as halftone tiles, we may replicate the tile in
 * X and/or Y, but it is still valuable to know the true tile dimensions.
 */
typedef struct gx_bitmap_s {
	byte *data;
	int raster;			/* bytes per scan line */
	gs_int_point size;		/* width, height */
	gx_bitmap_id id;
	ushort rep_width, rep_height;	/* true size of tile */
} gx_bitmap;

#endif					/* gxbitmap_INCLUDED */
