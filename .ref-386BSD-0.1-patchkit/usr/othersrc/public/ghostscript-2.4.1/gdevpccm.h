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

/* gdevpccm.h */
/* Interface to PC color mapping utilities for Ghostscript */
/* Requires gxdevice.h */

/* Color mapping routines for EGA/VGA-style color. */
extern dev_proc_map_rgb_color(pc_4bit_map_rgb_color);
extern dev_proc_map_color_rgb(pc_4bit_map_color_rgb);

/* Color mapping routines for 8-bit color (with a fixed palette). */
extern dev_proc_map_rgb_color(pc_8bit_map_rgb_color);
extern dev_proc_map_color_rgb(pc_8bit_map_color_rgb);

/* Write the palette on a file. */
extern int pc_write_palette(P3(gx_device *, uint, FILE *));
