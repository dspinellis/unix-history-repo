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

/* gdevpccm.c */
/* Support routines for PC color mapping */
#include "gs.h"
#include "gsmatrix.h"			/* for gxdevice.h */
#include "gxdevice.h"
#include "gdevpccm.h"			/* interface */

/* Color mapping routines for EGA/VGA-style color. */
/* Colors are 4 bits: 8=intensity, 4=R, 2=G, 1=B. */

#define black 0
#define blue 1
#define green 2
#define cyan 3
#define red 4
#define magenta 5
#define brown 6
#define white 7
#define dgray 8				/* dark gray is not very usable */
#define lblue 9
#define lgreen 10
#define lcyan 11
#define lred 12
#define lmagenta 13
#define yellow 14
#define bwhite 15
gx_color_index
pc_4bit_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{
#define c13 (gx_max_color_value / 3)
#define c23 (gx_max_color_value - c13)
	static byte g0[3][3] =
	 {{black,blue,lblue},{red,magenta,lmagenta},{lred,lmagenta,lmagenta}};
	static byte g1[3][3] =
	 {{green,cyan,lcyan},{brown,white,lcyan},{yellow,yellow,lmagenta}};
	static byte g2[3][3] =
	 {{lgreen,lgreen,lcyan},{lgreen,lgreen,lcyan},{yellow,yellow,bwhite}};
	return (gx_color_index)
		((g >= c23 ? g2 : g >= c13 ? g1 : g0)
		 [r >= c23 ? 2 : r >= c13 ? 1 : 0]
		 [b >= c23 ? 2 : b >= c13 ? 1 : 0]);
#undef c13
#undef c23
}
int
pc_4bit_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{
#define icolor (int)color
	gx_color_value one =
		(icolor & 8 ? gx_max_color_value : gx_max_color_value / 3);
	prgb[0] = (icolor & 4 ? one : 0);
	prgb[1] = (icolor & 2 ? one : 0);
	prgb[2] = (icolor & 1 ? one : 0);
	return 0;
#undef icolor
}

/* Color mapping routines for 8-bit color with a fixed palette */
/* (3 bits of R, 3 bits of G, 2 bits of B). */

gx_color_index
pc_8bit_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	return (gx_color_index)
	  (((r >> (gx_color_value_bits - 3)) << 5) +
	   ((g >> (gx_color_value_bits - 3)) << 2) +
	   ((b >> (gx_color_value_bits - 2))));
}
int
pc_8bit_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{
#define icolor (uint)color
	prgb[0] = ((icolor >> 5) & 7) * (gx_max_color_value / 7);
	prgb[1] = ((icolor >> 2) & 7) * (gx_max_color_value / 7);
	prgb[2] = (icolor & 3) * (gx_max_color_value / 3);
#undef icolor
	return 0;
}

/* Write a palette on a file. */
int
pc_write_palette(gx_device *dev, uint max_index, FILE *file)
{	uint i, c;
	gx_color_value rgb[3];
	for ( i = 0; i < max_index; i++ )
	{	(*dev->procs->map_color_rgb)(dev, (gx_color_index)i, rgb);
		for ( c = 0; c < 3; c++ )
		{	byte b = rgb[c] >> (gx_color_value_bits - 8);
			fputc(b, file);
		}
	}
	return 0;
}