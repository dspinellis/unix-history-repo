/* Copyright (C) 1989, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gzht.h */
/* Private halftone representation for GhostScript */

/* Halftone parameter structure */
typedef struct halftone_params_s {
	float frequency;
	float angle;
	/* Computed values */
	int width;
	int height;
	struct ht_bit_s *order;		/* whitening order */
	int order_size;
} halftone_params;

/*
 * The whitening order is represented by an array stored in row order.
 * The pixel represented by order[0] is whitened first.
 * During sampling, order[i].mask is a normalized sample value.
 * After sampling and sorting,
 * order[i].offset is the byte index of the pixel in the rendering cache;
 * order[i].mask is the mask to be or'ed into this byte and the
 * following one.  (This is arranged so it will work properly on
 * either big- or little-endian machines.)
 */
typedef struct ht_bit_s {
	ushort offset;
	ushort mask;
} ht_bit;
