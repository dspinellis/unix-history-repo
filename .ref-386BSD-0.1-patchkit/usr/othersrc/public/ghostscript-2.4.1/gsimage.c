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

/* gsimage.c */
/* Image procedures for Ghostscript library */
#include "gx.h"
#include "memory_.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gspaint.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate.h */
#include "gzcolor.h"			/* requires gxdevice.h */
#include "gzpath.h"
#include "gxcpath.h"
#include "gxdevmem.h"
#include "gximage.h"

/* Exported size of enumerator */
const uint gs_image_enum_sizeof = sizeof(gs_image_enum);

/* Forward declarations */
private int image_init(P9(gs_image_enum *, int, int, int, int, int,
  gs_matrix *, gs_state *, fixed));
/* Procedures for unpacking the input data into 8 bits/sample. */
private void image_unpack_1(iunpack_proc_args);
private void image_unpack_1_spread(iunpack_proc_args);
private void image_unpack_2(iunpack_proc_args);
private void image_unpack_2_spread(iunpack_proc_args);
private void image_unpack_4(iunpack_proc_args);
private void image_unpack_8(iunpack_proc_args);
private void image_unpack_8_spread(iunpack_proc_args);
private void image_unpack_12(iunpack_proc_args);
/* The image_render procedures work on fully expanded, complete rows. */
/* These take a height argument, which is an integer > 0; */
/* they return a negative code, or the number of */
/* rows actually processed (which may be less than the height). */
private int image_render_skip(irender_proc_args);
private int image_render_direct(irender_proc_args);
private int image_render_mono(irender_proc_args);
private int image_render_color(irender_proc_args);

/* Set up a gs_color with a transfer-mapped gray sample. */
#define image_set_rgb(rcolor,sample_value)\
  rcolor.luminance = rcolor.red = rcolor.green = rcolor.blue =\
    gx_map_color_param_byte(pgs, sample_value, gray)

/* Mask tables for spreading input data. */
/* Note that the mask tables depend on the end-orientation of the CPU. */
/* We can't simply define them as byte arrays, because */
/* they might not wind up properly long- or short-aligned. */
#define map4tox(z,a,b,c,d)\
	z, z^a, z^b, z^(a+b),\
	z^c, z^(a+c), z^(b+c), z^(a+b+c),\
	z^d, z^(a+d), z^(b+d), z^(a+b+d),\
	z^(c+d), z^(a+c+d), z^(b+c+d), z^(a+b+c+d)
#if arch_is_big_endian
private unsigned long map_4_to_32[16] =
   {	map4tox(0L, 0xffL, 0xff00L, 0xff0000L, 0xff000000L)	};
private unsigned long map_4_to_32_invert[16] =
   {	map4tox(0xffffffffL, 0xffL, 0xff00L, 0xff0000L, 0xff000000L)	};
private unsigned short map_4_to_16[16] =
   {	map4tox(0, 0x55, 0xaa, 0x5500, 0xaa00)	};
#else					/* !arch_is_big_endian */
private unsigned long map_4_to_32[16] =
   {	map4tox(0L, 0xff000000L, 0xff0000L, 0xff00L, 0xffL)	};
private unsigned long map_4_to_32_invert[16] =
   {	map4tox(0xffffffffL, 0xff000000L, 0xff0000L, 0xff00L, 0xffL)	};
private unsigned short map_4_to_16[16] =
   {	map4tox(0, 0x5500, 0xaa00, 0x55, 0xaa)	};
#endif

/* Start processing an image */
int
gs_image_init(gs_image_enum *penum, gs_state *pgs,
  int width, int height, int bps, int spp, gs_matrix *pmat)
{	int spread;
	if ( pgs->in_cachedevice ) return_error(gs_error_undefined);
	switch ( spp )
	   {
	case 1: case 3: case 4:
		spread = 1; break;
	case -3: case -4:
		spp = -spp; spread = spp; break;
	default:
		return_error(gs_error_rangecheck);
	   }
	if ( spp == 1 )
	   {	/* Initialize the color table */
#define chtl(i)\
  penum->dev_colors[i].halftone_level
		switch ( bps )
		   {
		default:
		   {	/* Yes, clearing the entire table is slow, */
			/* but for 8 bit-per-sample images, it's worth it. */
			register gx_device_color *pcht = &penum->dev_colors[0];
			register int n = 64;
			do
			   {	pcht[0].halftone_level =
				  pcht[1].halftone_level =
				  pcht[2].halftone_level =
				  pcht[3].halftone_level = -1;
				pcht += 4;
			   }
			while ( --n > 0 );
			break;
		   }
		case 4:
			chtl(17) = chtl(2*17) = chtl(3*17) =
			  chtl(4*17) = chtl(6*17) = chtl(7*17) =
			  chtl(8*17) = chtl(9*17) = chtl(11*17) =
			  chtl(12*17) = chtl(13*17) = chtl(14*17) = -1;
			/* falls through */
		case 2:
			chtl(5*17) = chtl(10*17) = -1;
		case 1:
			;
		   }
		/* Pre-map entries 0 and 255. */
		   {	gs_color rcolor;
			image_set_rgb(rcolor, 0);
			gx_color_render(&rcolor, &penum->icolor0, pgs);
			image_set_rgb(rcolor, 255);
			gx_color_render(&rcolor, &penum->icolor1, pgs);
		   }
#undef chtl
	   }
	penum->masked = 0;
	penum->map4to32 = map_4_to_32;
	return image_init(penum, width, height, bps, spp, spread,
			  pmat, pgs, (fixed)0);
}

/* Start processing a masked image */
int
gs_imagemask_init(gs_image_enum *penum, gs_state *pgs,
  int width, int height, int invert, gs_matrix *pmat, int adjust)
{	/* Initialize color entries 0 and 255. */
	penum->icolor0.halftone_level = 0;
	penum->icolor0.color1 = penum->icolor0.color2 = gx_no_color_index;
	penum->icolor1 = *pgs->dev_color;
	penum->masked = 1;
	penum->map4to32 = (invert ? map_4_to_32_invert : map_4_to_32);
	return image_init(penum, width, height, 1, 1, 1, pmat, pgs,
			  (adjust && pgs->in_cachedevice ?
			   float2fixed(0.25) : (fixed)0));
}

/* Common setup for image and imagemask. */
/* Caller has set penum->masked, map4to32, dev_colors[]. */
private int
image_init(register gs_image_enum *penum, int width, int height,
  int bps, int spp, int spread, gs_matrix *pmat, gs_state *pgs,
  fixed adjust)
{	int code;
	int index_bps;
	gs_matrix mat;
	gs_fixed_rect clip_box;
	uint bsize = (width + 8) * spp;	/* round up, +1 for end-of-run byte */
	byte *buffer;
	fixed mtx, mty;
	if ( width <= 0 || height < 0 )
		return_error(gs_error_rangecheck);
	switch ( bps )
	   {
	case 1: index_bps = 0; break;
	case 2: index_bps = 1; break;
	case 4: index_bps = 2; break;
	case 8: index_bps = 3; break;
	case 12: index_bps = 4; break;
	default: return_error(gs_error_rangecheck);
	   }
	if ( height == 0 ) return 0;	/* empty image */
	if (	(code = gs_matrix_invert(pmat, &mat)) < 0 ||
		(code = gs_matrix_multiply(&mat, &ctm_only(pgs), &mat)) < 0
	   )	return code;
	buffer = (byte *)gs_malloc(1, bsize, "image buffer");
	if ( buffer == 0 ) return_error(gs_error_VMerror);
	penum->width = width;
	penum->height = height;
	penum->bps = bps;
	penum->spp = spp;
	penum->spread = spread;
	penum->fxx = float2fixed(mat.xx);
	penum->fyy = float2fixed(mat.yy);
	if ( (penum->skewed = is_skewed(&mat)) )
	   {	penum->fxy = float2fixed(mat.xy);
		penum->fyx = float2fixed(mat.yx);
	   }
	else
	   {	penum->fxy = 0;
		penum->fyx = 0;
	   }
	penum->xcur = mtx = float2fixed(mat.tx);
	penum->ycur = mty = float2fixed(mat.ty);
	penum->pgs = pgs;
	clip_box = pgs->clip_path->path.bbox;	/* box is known to be up to date */
	penum->clip_box = clip_box;
	penum->buffer = buffer;
	penum->buffer_size = bsize;
	penum->bytes_per_row =
		(uint)(((ulong)width * (bps * spp) / spread + 7) >> 3);
	penum->slow_loop = penum->skewed;
	/* If all four extrema of the image fall within the clipping */
	/* rectangle, clipping is never required. */
	   {	gs_fixed_rect cbox;
		fixed edx = float2fixed(mat.xx * width);
		fixed edy = float2fixed(mat.yy * height);
		fixed epx, epy, eqx, eqy;
		if ( edx < 0 ) epx = edx, eqx = 0;
		else epx = 0, eqx = edx;
		if ( edy < 0 ) epy = edy, eqy = 0;
		else epy = 0, eqy = edy;
		if ( penum->skewed )
		   {	edx = float2fixed(mat.yx * height);
			edy = float2fixed(mat.xy * width);
			if ( edx < 0 ) epx += edx; else eqx += edx;
			if ( edy < 0 ) epy += edy; else eqy += edy;
		   }
		gx_cpath_box_for_check(pgs->clip_path, &cbox);
		penum->never_clip =
			mtx + epx >= cbox.p.x && mtx + eqx <= cbox.q.x &&
			mty + epy >= cbox.p.y && mty + eqy <= cbox.q.y;
#ifdef DEBUG
if ( gs_debug['b'] | gs_debug['B'] )
	dprintf4("[b]Image: cbox=(%g,%g),(%g,%g)\n",
		 fixed2float(cbox.p.x), fixed2float(cbox.p.y),
		 fixed2float(cbox.q.x), fixed2float(cbox.q.y)),
	dprintf3("     mt=(%g,%g) never_clip=%d\n",
		 fixed2float(mtx), fixed2float(mty), penum->never_clip);
#endif
	   }
	   {	static void (*procs[5])(iunpack_proc_args) = {
			image_unpack_1, image_unpack_2,
			image_unpack_4, image_unpack_8, image_unpack_12
		   };
		static void (*spread_procs[5])(iunpack_proc_args) = {
			image_unpack_1_spread, image_unpack_2_spread,
			image_unpack_4, image_unpack_8_spread,
			image_unpack_12
		   };
		penum->slow_loop |=
			/* Use slow loop for imagemask with a halftone */
			(penum->masked &&
			 !color_is_pure(pgs->dev_color));
		if ( pgs->in_charpath )
			penum->render = image_render_skip;
		else if ( spp == 1 && bps == 1 && !penum->slow_loop &&
			 (fixed2long_rounded(mtx + width * penum->fxx) -
			  fixed2long_rounded(mtx) == width) &&
			  (penum->masked || color_is_pure(&penum->icolor0))
		   )
			penum->render = image_render_direct;
		else
			penum->render =
			  (spp == 1 ? image_render_mono : image_render_color);
		/* The following should just be an assignment of */
		/* a conditional expression, but the Ultrix C compiler */
		/* can't handle it. */
		if ( penum->render == image_render_direct )
		  { penum->unpack = image_unpack_8;
		    /* If the image is 1-for-1 with the device, */
		    /* we don't want to spread the samples, */
		    /* but we have to reset bps to prevent the buffer */
		    /* pointer from being incremented by 8 bytes */
		    /* per input byte. */
		    penum->bps = 8;
		  }
		else if ( spread != 1 )
		  penum->unpack = spread_procs[index_bps];
		else
		  penum->unpack = procs[index_bps];
	   }
	if ( !penum->never_clip )
	   {	/* Set up the clipping device. */
		gx_device *dev = (gx_device *)&penum->clip_dev;
		penum->clip_dev = gs_clip_device;
		penum->clip_dev.target = gs_currentdevice(pgs);
		penum->clip_dev.list = pgs->clip_path->list;
		(*dev->procs->open_device)(dev);
	   }
	penum->adjust = adjust;
	penum->plane_index = 0;
	penum->byte_in_row = 0;
	penum->y = 0;
#ifdef DEBUG
if ( gs_debug['b'] | gs_debug['B'] )
	dprintf3("[b]Image: w=%d h=%d %s\n",
		 width, height, (penum->never_clip ? "no clip" : "must clip")),
	dprintf6("   [%f %f %f %f %f %f]\n",
		 mat.xx, mat.xy, mat.yx, mat.yy, mat.tx, mat.ty);
#endif
	return 0;
}

/* Process the next piece of an image */
int
gs_image_next(register gs_image_enum *penum, byte *dbytes, uint dsize)
{	uint rsize = penum->bytes_per_row;
	uint pos = penum->byte_in_row;
	int width = penum->width;
	uint dleft = dsize;
	uint dpos = 0;
	gs_state *pgs = penum->pgs;
	gx_device *save_dev = 0;
	int code;
	/* Accumulate separated colors, if needed */
	if ( penum->plane_index == 0 )
		penum->plane_size = dsize;
	else if ( dsize != penum->plane_size )
		return_error(gs_error_undefinedresult);
	penum->planes[penum->plane_index] = dbytes;
	if ( ++(penum->plane_index) != penum->spread )
		return 0;
	penum->plane_index = 0;
	/* We've accumulated an entire set of planes. */
	if ( !penum->never_clip )
	   {	/* Install the clipping device if needed. */
		gx_device *dev = (gx_device *)&penum->clip_dev;
		save_dev = gs_currentdevice(pgs);
		penum->clip_dev.target = save_dev;
		gx_set_device_only(pgs, dev);
	   }
	while ( dleft )
	   {	/* Fill up a row, then display it. */
		uint bcount = min(dleft, rsize - pos);
		byte *bptr =
		  penum->buffer + (pos << 3) / penum->bps * penum->spread;
		int px;
		for ( px = 0; px < penum->spread; px++ )
		  (*penum->unpack)(penum, bptr + px, penum->planes[px] + dpos, bcount, pos);
		pos += bcount;
		dpos += bcount;
		dleft -= bcount;
		if ( pos == rsize )	/* filled an entire row */
		   {
#ifdef DEBUG
if ( gs_debug['B'] )
   {			int i, n = width * penum->spp;
			dputs("[B]row:");
			for ( i = 0; i < n; i++ )
				dprintf1(" %02x", penum->buffer[i]);
			dputs("\n");
   }
#endif
			if ( !penum->skewed )
			  { /* Precompute integer y and height, */
			    /* and check for clipping. */
			    fixed yc = penum->ycur, yn;
			    fixed dyy = penum->fyy;
			    fixed adjust = penum->adjust;
			    if ( dyy > 0 )
			      dyy += adjust << 1,
			      yc -= adjust;
			    else
			      dyy = (adjust << 1) - dyy,
			      yc -= dyy - adjust;
			    if ( yc >= penum->clip_box.q.y ) goto mt;
			    yn = yc + dyy;
			    if ( yn <= penum->clip_box.p.y ) goto mt;
			    penum->yci = fixed2int_var_rounded(yc);
			    penum->hci =
			      fixed2int_var_rounded(yn) - penum->yci;
			    if ( penum->hci == 0 ) goto mt;
			  }
			code = (*penum->render)(penum, penum->buffer, width * penum->spp, 1);
			if ( code < 0 ) goto err;
mt:			if ( ++(penum->y) == penum->height ) goto end;
			pos = 0;
			penum->xcur += penum->fyx;
			penum->ycur += penum->fyy;
		   }
	   }
	penum->byte_in_row = pos;
	code = 0;
	goto out;
end:	/* End of data */
	code = 1;
	/* falls through */
err:	/* Error, abort */
	gs_free((char *)penum->buffer, penum->buffer_size, 1, "image buffer");
out:	if ( save_dev != 0 ) gx_set_device_only(pgs, save_dev);
	return code;
}

/* ------ Unpacking procedures ------ */

private void
image_unpack_1(gs_image_enum *penum, byte *bptr,
  register byte *data, uint dsize, uint inpos)
{	register unsigned long *bufp = (unsigned long *)bptr;
	int left = dsize;
	register unsigned long _ds *map = penum->map4to32; /* may invert */
	while ( left-- )
	   {	register unsigned b = *data++;
		*bufp++ = map[b >> 4];
		*bufp++ = map[b & 0xf];
	   }
}

private void
image_unpack_1_spread(gs_image_enum *penum, register byte *bufp,
  register byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	int left = dsize;
	while ( left-- )
	   {	register unsigned b = *data++;
		*bufp = -(b >> 7); bufp += spread;
		*bufp = -((b >> 6) & 1); bufp += spread;
		*bufp = -((b >> 5) & 1); bufp += spread;
		*bufp = -((b >> 4) & 1); bufp += spread;
		*bufp = -((b >> 3) & 1); bufp += spread;
		*bufp = -((b >> 2) & 1); bufp += spread;
		*bufp = -((b >> 1) & 1); bufp += spread;
		*bufp = -(b & 1); bufp += spread;
	   }
}

private void
image_unpack_2(gs_image_enum *penum, byte *bptr,
  register byte *data, uint dsize, uint inpos)
{	register unsigned short *bufp = (unsigned short *)bptr;
	int left = dsize;
	register unsigned short _ds *map = map_4_to_16;
	while ( left-- )
	   {	register unsigned b = *data++;
		*bufp++ = map[b >> 4];
		*bufp++ = map[b & 0xf];
	   }
}

private void
image_unpack_2_spread(gs_image_enum *penum, register byte *bufp,
  register byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	int left = dsize;
	static byte map_2_to_8[4] = { 0, 0x55, 0xaa, 0xff };
	register byte _ds *map = map_2_to_8;
	while ( left-- )
	   {	register unsigned b = *data++;
		*bufp = map[b >> 6]; bufp += spread;
		*bufp = map[(b >> 4) & 3]; bufp += spread;
		*bufp = map[(b >> 2) & 3]; bufp += spread;
		*bufp = map[b & 3]; bufp += spread;
	   }
}

private void
image_unpack_4(gs_image_enum *penum, register byte *bufp,
  register byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	int left = dsize;
	while ( left-- )
	   {	register unsigned b = *data++;
		*bufp = (b & 0xf0) + (b >> 4); bufp += spread;
		b &= 0xf;
		*bufp = (b << 4) + b; bufp += spread;
	   }
}

private void
image_unpack_8(gs_image_enum *penum, byte *bufp,
  byte *data, uint dsize, uint inpos)
{	if ( data != bufp ) memcpy(bufp, data, dsize);
}

private void
image_unpack_8_spread(gs_image_enum *penum, register byte *bufp,
  register byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	register int left = dsize;
	while ( left-- )
	   {	*bufp = *data++; bufp += spread;
	   }
}

private void
image_unpack_12(gs_image_enum *penum, register byte *bufp,
  register byte *data, uint dsize, uint inpos)
{	register int spread = penum->spread;
	register int left = dsize;
	/* We have to deal with the 3 cases of inpos % 3 separately. */
	/* (In fact, this is the only reason inpos is passed to */
	/* the unpacking procedures at all.) */
	/* Let N = inpos / 3. */
	switch ( inpos % 3 )
	   {
	case 1:
		/* bufp points to byte 2N, which was already filled */
		/* with the leftover byte from the previous call. */
		bufp += spread;
		*bufp = *data++ << 4;
		if ( !--left ) return;
	case 2:
		/* bufp points to byte 2N+1, which was half-filled */
		/* with the second leftover byte from the previous call. */
		*bufp += *data++ >> 4;
		--left;
	case 0:
		/* Nothing special to do. */
		;
	   }
	/* Just drop the low 4 bits of each 12. */
	while ( left >= 3 )
	   {	*bufp = *data;
		bufp += spread;
		*bufp = (data[1] << 4) + (data[2] >> 4);
		bufp += spread;
		data += 3;
	   }
	switch ( left )
	   {
	case 2:				/* dddddddd xxxxdddd */
		bufp[1] = data[1] << 4;
	case 1:				/* dddddddd */
		*bufp = *data;
	case 0:				/* Nothing more to do. */
		;
	   }
}

/* ------ Rendering procedures ------ */

/* Rendering procedure for ignoring an image.  We still need to iterate */
/* over the samples, because the procedure might have side effects. */
private int
image_render_skip(gs_image_enum *penum, byte *data, uint w, int h)
{	return h;
}

/* Rendering procedure for a 1-bit-per-pixel sampled image */
/* with no skewing/rotation or X scaling. */
/* In this case a direct BitBlt is possible. */
private int
image_render_direct(register gs_image_enum *penum, byte *data, uint w, int h)
{	int ix = fixed2int_var_rounded(penum->xcur), iw = w;
	int iy = penum->yci, ih = penum->hci;
	uint raster = (w + 7) >> 3;
	gx_device *dev = penum->pgs->device->info;
	dev_proc_copy_mono((*copy_mono)) = dev->procs->copy_mono;
	gx_color_index
		zero = penum->icolor0.color1,
		one = penum->icolor1.color1;
#ifdef DEBUG
if ( gs_debug['b'] | gs_debug['B'] )
	dprintf4("[b]direct (%d,%d),(%d,%d)\n", ix, iy, iw, ih);
#endif
	/* Check for inverted imagemask */
	if ( penum->map4to32 == map_4_to_32_invert )
		zero = penum->icolor1.color1,
		one = penum->icolor0.color1;
	if ( ih == 1 && h == 1 && penum->fyy >= 0 )
	   {	/* We can do the whole thing at once. */
		(*copy_mono)(dev, data, 0, raster, gx_no_bitmap_id,
			ix, iy, iw, ih, zero, one);
		return h;
	   }
	else
	   {	/* Do just one row, clipping if necessary. */
		int dy;
		for ( dy = 0; dy < ih; dy++ )
			(*copy_mono)(dev, data, 0, raster, gx_no_bitmap_id,
				ix, iy + dy, iw, 1, zero, one);
		return 1;
	   }
}

/* Rendering procedure for the general case of displaying a */
/* monochrome image, dealing with multiple bit-per-sample images, */
/* bits not 1-for-1 with the device, and general transformations. */
/* This procedure handles a single scan line. */
private int
image_render_mono(gs_image_enum *penum, byte *buffer, uint w, int h)
{	gs_state *pgs = penum->pgs;
	const int masked = penum->masked;
	const fixed dxx = penum->fxx;
	fixed xt = penum->xcur;
	gs_color rcolor;
	gx_device_color *pdevc = pgs->dev_color;
	/* Make sure the cache setup matches the graphics state. */
	/* Also determine whether all tiles fit in the cache. */
	int tiles_fit = gx_check_tile_cache(pgs);
#define image_set_gray(sample_value)\
   { pdevc = &penum->dev_colors[sample_value];\
     switch ( pdevc->halftone_level )\
      { default:		/* halftone */\
	  if ( !tiles_fit ) gx_color_load(pdevc, pgs); break;\
        case -1:		/* not computed yet */\
	  image_set_rgb(rcolor, sample_value);\
	  gx_color_render(&rcolor, pdevc, pgs);\
	case 0: ;		/* pure color */\
      }\
   }
	fixed xl = xt;
	byte *psrc = buffer;
	byte *endp = buffer + w;
	fixed xrun = xt;		/* x at start of run */
	int run = *psrc;		/* run value */
	int htrun =			/* halftone run value */
	  (masked ? 255 : -2);
	*endp = ~endp[-1];	/* force end of run */
	gx_set_gray_only(&rcolor, (color_param)0);
	if ( penum->slow_loop )
	  { /* Skewed, or imagemask with a halftone. */
	    const fixed
	      dxy = penum->fxy, dyx = penum->fyx,
	      dyy = penum->fyy;
	    fixed ytf = penum->ycur;
	    fixed yrun = ytf;
	    for ( ; ; )
	      { if ( *psrc++ != run )
		  { /* Fill the region between xrun and xl. */
		    if ( run != htrun )
		      { if ( run == 0 )
			  { if ( masked ) goto trans;
			  }
			htrun = run;
			image_set_gray(run);
		      }
		    gz_fill_pgram_fixed(xrun, yrun, xl - xrun,
					ytf - yrun, dyx, dyy,
					pdevc, pgs);
trans:		    if ( psrc > endp ) break;
		    yrun = ytf;
		    xrun = xl;
		    run = psrc[-1];
		  }
		xl += dxx;
		ytf += dxy;		/* harmless if no skew */
	      }
	  }
	else			/* fast loop */
	  { /* No skew, and not imagemask with a halftone. */
	    const fixed adjust = penum->adjust;
	    fixed xa = (dxx >= 0 ? adjust : -adjust);
	    const int yt = penum->yci, iht = penum->hci;
	    gx_device *dev = pgs->device->info;
	    dev_proc_fill_rectangle((*fill_proc)) = dev->procs->fill_rectangle;
	    dev_proc_tile_rectangle((*tile_proc)) = dev->procs->tile_rectangle;
	    dev_proc_copy_mono((*copy_mono_proc)) = dev->procs->copy_mono;
	    dev_proc_copy_color((*copy_color_proc)) = dev->procs->copy_color;
	    /* If each pixel is likely to fit in a single halftone tile, */
	    /* determine that now (tile_offset = offset of row within tile). */
	    int tile_offset =
	      gx_check_tile_size(pgs,
				 fixed2int_rounded(any_abs(dxx) + (xa << 1)),
				 yt, iht);
	    /* Fold the adjustment into xrun and xl, */
	    /* including the +0.5 for rounding. */
	    xrun = xrun - xa + float2fixed(0.5);
	    xl = xl + xa + float2fixed(0.5);
	    xa <<= 1;
	    for ( ; ; )
	      { if ( *psrc++ != run )
		  { /* Fill the region between xrun and xl. */
		    int xi = fixed2int_var(xrun);
		    int wi = fixed2int_var(xl) - xi;
		    int tsx, code;
		    gx_bitmap *tile;
		    if ( wi <= 0 )
		      { if ( wi == 0 ) goto mt;
			xi += wi, wi = -wi;
		      }
		    switch ( run )
		      {
		      case 0:
			if ( masked ) goto mt;
			if ( !color_is_pure(&penum->icolor0) ) goto ht;
			code = (*fill_proc)(dev, xi, yt, wi, iht, penum->icolor0.color1);
			break;
		      case 255:		/* just for speed */
			if ( !color_is_pure(&penum->icolor1) ) goto ht;
			code = (*fill_proc)(dev, xi, yt, wi, iht, penum->icolor1.color1);
			break;
		      default:
ht:			/* Use halftone if needed */
			if ( run != htrun )
			  { image_set_gray(run);
			    htrun = run;
			  }
			/* We open-code gz_fill_rectangle_open, */
			/* because we've done some of the work for */
			/* halftone tiles in advance. */
			if ( color_is_pure(pdevc) )
			  { code = (*fill_proc)(dev, xi, yt, wi, iht, pdevc->color1);
			  }
			else if ( tile_offset >= 0 &&
				  (tile = pdevc->tile,
				   (tsx = (xi + pgs->phase_mod.x) % tile->rep_width) + wi <= tile->size.x)
				)
			  { /* The pixel(s) fit(s) in a single tile. */
			    byte *row = tile->data + tile_offset;
			    code = (color_is_color_halftone(pdevc) ?
				    (*copy_color_proc)
				      (dev, row, tsx, tile->raster, gx_no_bitmap_id,
				       xi, yt, wi, iht) :
				    (*copy_mono_proc)
				      (dev, row, tsx, tile->raster, gx_no_bitmap_id,
				       xi, yt, wi, iht,
				       pdevc->color1, pdevc->color2)
				    );
			  }
			else
			  { code = (*tile_proc)(dev, pdevc->tile, xi, yt, wi, iht,
					     pdevc->color1, pdevc->color2,
					     pgs->phase_mod.x, pgs->phase_mod.y);
			  }
		      }
		    if ( code < 0 ) return code;
mt:		    if ( psrc > endp ) break;
		    xrun = xl - xa;	/* original xa << 1 */
		    run = psrc[-1];
		  }
		xl += dxx;
	      }
	  }
	return 1;
}

/* Rendering procedure for handling color images. */
typedef union { struct { byte r, g, b, skip; } v; ulong all; } color_sample;
private int
image_render_color(gs_image_enum *penum, byte *buffer, uint w, int h)
{	gs_state *pgs = penum->pgs;
	fixed	dxx = penum->fxx, dxy = penum->fxy,
		dyx = penum->fyx, dyy = penum->fyy;
	int skew = penum->skewed;
	fixed xt = penum->xcur;
	fixed ytf = penum->ycur;
	int yt = penum->yci, iht = penum->hci;
	gs_color rcolor;
	gx_device_color devc1, devc2;
	gx_device_color _ss *spdevc = &devc1;
	gx_device_color _ss *spdevc_next = &devc2;
#define pdevc (gx_device_color *)spdevc
#define pdevc_next (gx_device_color *)spdevc_next
	int spp = penum->spp;
	fixed xl = xt;
	byte *psrc = buffer;
	fixed xrun = xt;		/* x at start of run */
	int irun = fixed2int_var_rounded(xrun);	/* int xrun */
	fixed yrun = ytf;		/* y ditto */
	color_sample run;		/* run value */
	color_sample next;		/* next sample value */
	byte *bufend = buffer + w;
	bufend[0] = ~bufend[-spp];	/* force end of run */
#ifdef DEBUG
if ( gs_debug['b'] | gs_debug['B'] )
	dprintf5("[b]y=%d w=%d xt=%f yt=%f yb=%f\n",
		 penum->y, w,
		 fixed2float(xt), fixed2float(ytf), fixed2float(ytf + dyy));
#endif
	run.all = 0;
	next.all = 0;
	rcolor.red = rcolor.green = rcolor.blue = 0;
	gx_color_from_rgb(&rcolor);
	gx_color_render(&rcolor, pdevc, pgs);
	while ( psrc <= bufend )	/* 1 extra iteration */
				/* to handle final run */
	   {	if ( spp == 4 )		/* cmyk */
		   {	switch ( psrc[3] )
			   {
			case 0:		/* no black */
				next.v.r = ~psrc[0];
				next.v.g = ~psrc[1];
				next.v.b = ~psrc[2];
				break;
			case 0xff:	/* all black */
				next.v.r = next.v.g = next.v.b = 0;
				break;
			default:
			   {	uint black = 0xff - psrc[3];
				/* The following is equivalent to */
				/* v * black / 0xff, without the divide. */
				register uint temp;
#define deduct_black(v)\
  (temp = (v) * black, (temp + (temp >> 8) + 1) >> 8)
				next.v.r = deduct_black(0xff - psrc[0]);
				next.v.g = deduct_black(0xff - psrc[1]);
				next.v.b = deduct_black(0xff - psrc[2]);
#undef deduct_black
			   }
			   }
			psrc += 4;
		   }
		else			/* rgb */
		   {	next.v.r = psrc[0];
			next.v.g = psrc[1];
			next.v.b = psrc[2];
			psrc += 3;
		   }
		if ( next.all != run.all )
		   {	rcolor.red = gx_map_color_param_byte(pgs, next.v.r, red);
			rcolor.green = gx_map_color_param_byte(pgs, next.v.g, green);
			rcolor.blue = gx_map_color_param_byte(pgs, next.v.b, blue);
			gx_color_from_rgb(&rcolor);
			gx_color_render(&rcolor, pdevc_next, pgs);
			/* Even though the supplied colors don't match, */
			/* the device colors might. */
			if ( devc1.color1 != devc2.color1 ||
			     devc1.halftone_level != devc2.halftone_level ||
			     (devc1.halftone_level &&
			      devc1.color2 != devc2.color2) ||
			     psrc > bufend	/* force end of last run */
			   )
			   {	/* Fill the region between */
				/* xrun/irun and xl */
				gx_device_color _ss *sptemp;
				int code;
				if ( skew )
			   {	/* Parallelogram */
				code = gz_fill_pgram_fixed(xrun, yrun,
					xl - xrun, ytf - yrun, dyx, dyy,
					pdevc, pgs);
				xrun = xl;
				yrun = ytf;
			   }
				else
			   {	/* Rectangle */
				int xi = irun;
				int wi = (irun = fixed2int_var_rounded(xl)) - xi;
				if ( wi < 0 ) xi += wi, wi = -wi;
				code = gz_fill_rectangle(xi, yt, wi, iht, pdevc, pgs);
			   }
				if ( code < 0 ) return code;
				sptemp = spdevc;
				spdevc = spdevc_next;
				spdevc_next = sptemp;
			   }
			run.all = next.all;
		   }
		xl += dxx;
		ytf += dxy;		/* harmless if no skew */
	   }
	return 1;
}
