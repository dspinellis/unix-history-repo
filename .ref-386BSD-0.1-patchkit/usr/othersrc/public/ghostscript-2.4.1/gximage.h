/* Copyright (C) 1989, 1990, 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gximage.h */
/* Internal definitions for image rendering */
/* Requires gxcpath.h, gxdevmem.h, gzcolor.h, gzpath.h */

/* Imaging state structure */
/* The Unix C compiler can't handle typedefs for procedure */
/* (as opposed to pointer-to-procedure) types, */
/* so we have to do it with a macro instead: */
#define iunpack_proc_args\
	P5(gs_image_enum *, byte *, byte *, uint, uint)
#define irender_proc_args\
	P4(gs_image_enum *, byte *, uint, int)
/* Main state structure */
struct gs_image_enum_s {
	/* Following are set at structure initialization */
	int width;
	int height;
	int bps;			/* bits per sample: 1, 2, 4, 8, 12 */
	int spp;			/* samples per pixel: 1, 3, or 4 */
	int spread;			/* spp if colors are separated, */
					/* 1 otherwise */
	int masked;			/* 0 = [color]image, 1 = imagemask */
	fixed fxx, fxy, fyx, fyy;	/* fixed version of matrix */
	void (*unpack)(iunpack_proc_args);
	int (*render)(irender_proc_args);
	gs_state *pgs;
	gs_fixed_rect clip_box;		/* pgs->clip_path.path->bbox, */
					/* possibly translated */
	gs_int_rect ibox;		/* integer version of clip_box, */
					/* only used for direct/buffered */
	byte *buffer;			/* for expanding to 8 bits/sample */
	uint buffer_size;
	uint bytes_per_row;		/* # of input bytes per row */
					/* (per plane, if spp == 1 and */
					/* spread > 1) */
	int never_clip;			/* true if entire image fits */
	int skewed;			/* true if image is skewed */
					/* or rotated */
	int slow_loop;			/* true if !(skewed | non-rect clip */
					/* | imagemask with a halftone) */
	fixed adjust;			/* adjustment when rendering */
					/* characters */
	ulong _ds *map4to32;		/* expansion map for monobit images */
	gx_device_clip clip_dev;	/* clipping device (if needed) */
	/* Following are updated dynamically */
	byte *planes[4];		/* separated color data */
	int plane_index;		/* current plane index, [0..spp) */
	uint plane_size;		/* size of data in each plane */
	uint byte_in_row;		/* current input byte position in row */
	fixed xcur, ycur;		/* device x, y of current row */
	int yci, hci;			/* integer y & height of row */
					/* (if no skew) */
	int y;
	/* Entries 0 and 255 of the following are set at initialization */
	/* for monochrome images; other entries are updated dynamically. */
	gx_device_color dev_colors[256];
#define icolor0 dev_colors[0]
#define icolor1 dev_colors[255]
};
