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

/* gzcolor.h */
/* Private definition of color representation for Ghostscript */
#include "gscolor.h"			/* client interface */
#include "gxlum.h"

/* Colors as seen by clients. */
/* The color parameters are stored internally as color_params. */
typedef unsigned short color_param;
/*
 * The following line should read:
 *	#define max_color_param max_ushort
 * but this seems to trigger brain-damage in a number of compilers.
 * The problems all seem to come up when the compiler has to cast this value
 * to some other type, so we define all the relevant casted values explicitly:
 */
#define max_color_param ((ushort)0xffff)
#define max_color_param_long 0xffffL
#define max_color_param_float ((float)0xffffL)
/* Color_param_bits must be between 8 and 16. */
#define color_param_bits 16
/* Convert a byte to or from a color_param efficiently. */
#define color_param_from_byte(b)\
  (((ushort)(b) << (color_param_bits - 8)) + ((b) >> (16 - color_param_bits)))
#define color_param_to_byte(p)\
  ((p) >> (color_param_bits - 8))

/*
 * The following members are used in the structure, depending on the space:
 *	DeviceGray	red, green, blue (all equal)
 *	DeviceRGB	red, green, blue
 *	DeviceCMYK	red, green, blue, not_black
 * luminance is also used for all of the above.
 */
/*typedef struct gs_color_s gs_color;*/	/* in gscolor.h */
struct gs_color_s {
	color_param red, green, blue;	/* RGB/~CMY */
	color_param not_black;		/* ~K */
	color_param luminance;		/* computed luminance */
	byte /*gs_color_space*/ space;	/* see above */
	unsigned is_gray : 1;		/* quick test for gray */
					/* (red==green==blue) */
	unsigned unused : 6;
	unsigned luminance_set : 1;	/* true if luminance is set */
};
extern color_param gx_color_luminance(P1(struct gs_color_s *));
#define color_luminance(pcolor)\
	((pcolor)->luminance_set ? (pcolor)->luminance :\
	 gx_color_luminance(pcolor))

/*
 * The following parameters are computed from the above,
 * and kept current through changes in transfer function or device.
 * If the halftone is a colored tile, color1 == color2 == gx_no_color_index,
 * and halftone_level == -1.  (Colored tiles are not currently used.)
 */
typedef struct gx_device_color_s gx_device_color;
struct gx_device_color_s {
	gx_color_index color1;		/* device color, or */
					/* darker color for halftoning */
	gx_color_index color2;		/* lighter color for halftoning */
	int halftone_level;		/* number of spots to whiten */
					/* when halftoning, 0 if */
					/* halftoning not needed, */
					/* <0 if color halftone */
	struct gx_bitmap_s *tile;	/* pointer to cached halftone */
};
#define color_is_pure(pdevc)\
  ((pdevc)->halftone_level == 0)
#define color_is_color_halftone(pdevc)\
  ((pdevc)->halftone_level < 0)

/* A fast version of gz_fill_rectangle. */
/* Note that it takes additional arguments. */
#define gz_fill_rectangle_open(dev, xi, yi, w, h, fill_proc, tile_proc, pdevc, pgs)\
  (color_is_pure(pdevc) ?\
    (*fill_proc)(dev, xi, yi, w, h, pdevc->color1) :\
    (*tile_proc)(dev, pdevc->tile, xi, yi, w, h,\
	pdevc->color1, pdevc->color2,\
	pgs->phase_mod.x, pgs->phase_mod.y) )

/* Procedures in gxcolor.c used elsewhere. */
void	gx_set_gray_only(P2(gs_color *, color_param)),
	gx_set_rgb_only(P4(gs_color *, color_param, color_param, color_param));
color_param	gx_color_unit_param(P1(floatp));

/* A color transfer function and cache. */
/* log2... must not be greater than color_param_bits. */
#define log2_transfer_map_size 8
#define transfer_map_size (1 << log2_transfer_map_size)
typedef struct gx_transfer_map_s {
	gs_transfer_proc proc;
	color_param values[transfer_map_size];
} gx_transfer_map;
typedef struct gx_transfer_s {
	int ref_count;			/* # of references from gstates */
	gx_transfer_map red, green, blue, gray;
} gx_transfer;

/* Map a color_param or byte through a transfer map. */
extern color_param gx_color_param_map(P2(color_param, color_param *));
#define gx_map_color_param(pgs,cp,m)\
  gx_color_param_map(cp, &pgs->transfer->m.values[0])
#if log2_transfer_map_size <= 8
#  define byte_to_tmx(b) ((b) >> (8 - log2_transfer_map_size))
#else
#  define byte_to_tmx(b)\
	(((b) << (log2_transfer_map_size - 8)) +\
	 ((b) >> (16 - log2_transfer_map_size)))
#endif
#define gx_map_color_param_byte(pgs,b,m)\
 (pgs->transfer->m.values[byte_to_tmx(b)])
