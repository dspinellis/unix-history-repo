/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
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

/* gdevpcl.h */
/* Interface to PCL utilities for Ghostscript printer drivers */
/* Requires gdevprn.h */

/* Color mapping procedures for 3-bit-per-pixel RGB printers */
extern dev_proc_map_rgb_color(gdev_pcl_3bit_map_rgb_color);
extern dev_proc_map_color_rgb(gdev_pcl_3bit_map_color_rgb);

/* Row compression routines */
typedef ulong word;
extern int gdev_pcl_mode2compress(P3(const word *row, const word *end_row, char *compressed));
extern int gdev_pcl_mode3compress(P4(int bytecount, const char *current, char *previous, char *compressed));
