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

/* gxdraw.c */
/* Primitive drawing routines for Ghostscript imaging library */
#include "gx.h"
#include "math_.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"			/* requires gsstate.h */
#include "gzcolor.h"			/* requires gxdevice.h */

/* Fill a rectangle. */
int
gz_fill_rectangle(int x, int y, int w, int h, gx_device_color *pdevc,
  gs_state *pgs)
{	gx_device *dev = pgs->device->info;
#ifdef DEBUG
if ( gs_debug['v'] )
	dprintf7("[v]x=%d y=%d w=%d h=%d  c1=%ld c2=%ld htl=%d\n",
		 x, y, w, h, (long)pdevc->color1, (long)pdevc->color2,
		 (long)pdevc->halftone_level);
#endif
	return gz_fill_rectangle_open(dev, x, y, w, h, dev->procs->fill_rectangle, dev->procs->tile_rectangle, pdevc, pgs);
}

/*
 * Auxiliary procedures for computing a * b / c and a * b % c
 * when a, b, and c are all non-negative,
 * b < c, and a * b exceeds (or might exceed) the capacity of a long.
 * It's really annoying that C doesn't provide any way to get at
 * the double-length multiply/divide instructions that
 * the machine undoubtedly provides....
 *
 * Note that these routines are exported for the benefit of gxfill.c.
 */

fixed
fixed_mult_quo(fixed a, fixed b, fixed c)
{	return (fixed)floor((double)a * b / c);
}
fixed
fixed_mult_rem(fixed a, fixed b, fixed c)
{	double prod = (double)a * b;
	return (fixed)(prod - floor(prod / c) * c);
}

/* Fill a trapezoid.  Requires: wt >= 0, wb >= 0, h >= 0. */
/* Note that the arguments are fixeds, not ints! */
/* This is derived from Paul Haeberli's floating point algorithm. */
typedef struct trap_line_s {
	int di; fixed df;		/* dx/dy ratio */
	fixed ldi, ldf;			/* increment per scan line */
	fixed x, xf;			/* current value */
} trap_line;
int
gz_fill_trapezoid_fixed(fixed fx0, fixed fw0, fixed fy0,
  fixed fx1, fixed fw1, fixed fh, int swap_axes,
  gx_device_color *pdevc, gs_state *pgs)
{	const fixed ymin = fixed_rounded(fy0) + float2fixed(0.5);
	const fixed ymax = fixed_rounded(fy0 + fh);
	int iy = fixed2int_var(ymin);
	const int iy1 = fixed2int_var(ymax);
	if ( iy >= iy1 ) return 0;	/* no scan lines to sample */
   {	trap_line l, r;
	int rxl, rxr, ry;
	const fixed dxl = fx1 - fx0;
	const fixed dxr = dxl + fw1 - fw0;
	const fixed yline = ymin - fy0;	/* partial pixel offset to */
					/* first line to sample */
	int fill_direct = color_is_pure(pdevc);
	gx_color_index cindex;
	gx_device *dev;
	dev_proc_fill_rectangle((*fill_rect));
	int code;

	r.x = (l.x = fx0 + float2fixed(0.5)) + fw0;
	if ( fill_direct )
	  cindex = pdevc->color1,
	  dev = pgs->device->info,
	  fill_rect = dev->procs->fill_rectangle;
#define fill_trap_rect(x,y,w,h)\
  (fill_direct ?\
    (swap_axes ? (*fill_rect)(dev, y, x, h, w, cindex) :\
     (*fill_rect)(dev, x, y, w, h, cindex)) :\
   swap_axes ? gz_fill_rectangle(y, x, h, w, pdevc, pgs) :\
   gz_fill_rectangle(x, y, w, h, pdevc, pgs))

	/* Compute the dx/dy ratios. */
	/* dx# = dx#i + (dx#f / fh). */
#define compute_dx(tl, d)\
  if ( d >= 0 )\
   { if ( d < fh ) tl.di = 0, tl.df = d;\
     else tl.di = (int)(d / fh), tl.df = d - tl.di * fh, tl.x += yline * tl.di;\
   }\
  else\
   { if ( (tl.df = d + fh) >= 0 /* d >= -fh */ ) tl.di = -1, tl.x -= yline;\
     else tl.di = (int)-((fh - 1 - d) / fh), tl.df = d - tl.di * fh, tl.x += yline * tl.di;\
   }

	/* Compute the x offsets at the first scan line to sample. */
	/* We need to be careful in computing yline * dx#f {/,%} fh */
	/* because the multiplication may overflow.  We know that */
	/* all the quantities involved are non-negative, and that */
	/* yline is less than 1 (as a fixed, of course); this gives us */
	/* a cheap conservative check for overflow in the multiplication. */
#define ymult_limit (max_fixed / fixed_1)
#define ymult_quo(yl, dxxf)\
  (dxxf < ymult_limit ? yl * dxxf / fh : fixed_mult_quo(yl, dxxf, fh))
#define ymult_rem(yl, dxxf)\
  (dxxf < ymult_limit ? yl * dxxf % fh : fixed_mult_rem(yl, dxxf, fh))

	/* It's worth checking for dxl == dxr, since this is the case */
	/* for parallelograms (including stroked lines). */
	compute_dx(l, dxl);
	if ( dxr == dxl )
	   {	fixed fx = ymult_quo(yline, l.df);
		l.x += fx;
		if ( l.di == 0 )
			r.di = 0, r.df = l.df;
		else			/* too hard to do adjustments right */
			compute_dx(r, dxr);
		r.x += fx;
	   }
	else
	   {	l.x += ymult_quo(yline, l.df);
		compute_dx(r, dxr);
		r.x += ymult_quo(yline, r.df);
	   }
	rxl = fixed2int_var(l.x);
	rxr = fixed2int_var(r.x);
	ry = iy;

	/* Compute one line's worth of dx/dy. */
	/* dx# * fixed_1 = ld#i + (ld#f / fh). */
	/* We don't have to bother with this if */
	/* we're only sampling a single scan line. */
	if ( iy1 - iy == 1 )
	   {	iy++;
		goto last;
	   }
#define compute_ldx(tl)\
  if ( tl.df < ymult_limit )\
    tl.ldi = int2fixed(tl.di) + int2fixed(tl.df) / fh,\
    tl.ldf = int2fixed(tl.df) % fh,\
    tl.xf = yline * tl.df % fh - fh;\
  else\
    tl.ldi = int2fixed(tl.di) + fixed_mult_quo(fixed_1, tl.df, fh),\
    tl.ldf = fixed_mult_rem(fixed_1, tl.df, fh),\
    tl.xf = fixed_mult_rem(yline, tl.df, fh) - fh
	compute_ldx(l);
	if ( dxr == dxl )
		r.ldi = l.ldi, r.ldf = l.ldf, r.xf = l.xf;
	else
	   {	compute_ldx(r);
	   }
#undef compute_ldx

	while ( ++iy != iy1 )
	   {	int ixl, ixr;
#define step_line(tl)\
  tl.x += tl.ldi;\
  if ( (tl.xf += tl.ldf) >= 0 ) tl.xf -= fh, tl.x++;
		step_line(l);
		step_line(r);
#undef step_line
		ixl = fixed2int_var(l.x);
		ixr = fixed2int_var(r.x);
		if ( ixl != rxl || ixr != rxr )
		   {	code = fill_trap_rect(rxl, ry, rxr - rxl, iy - ry);
			if ( code < 0 ) goto xit;
			rxl = ixl, rxr = ixr, ry = iy;
		   }	
	   }
last:	code = fill_trap_rect(rxl, ry, rxr - rxl, iy - ry);
xit:	if ( code < 0 && fill_direct ) return_error(code);
	return code;
   }
}

/* Fill a parallelogram whose points are p, p+a, p+b, and p+a+b. */
/* We should swap axes to get best accuracy, but we don't. */
int
gz_fill_pgram_fixed(fixed px, fixed py, fixed ax, fixed ay,
  fixed bx, fixed by, gx_device_color *pdevc, gs_state *pgs)
{	fixed t;
	fixed qx, dx, wx, pax, qax;
	int code;
#define swap(r, s) (t = r, r = s, s = t)
	/* Reorder the points so that 0 <= ay <= by. */
	if ( ay < 0 ) px += ax, py += ay, ax = -ax, ay = -ay;
	if ( by < 0 ) px += bx, py += by, bx = -bx, by = -by;
	if ( ay > by ) swap(ax, bx), swap(ay, by);
	if ( by == 0 ) return 0;	/* degenerate (line) */
	qx = px + ax + bx;
	/* Compute the distance from p to the point on the line (p, p+b) */
	/* whose Y coordinate is equal to ay. */
	dx = (fixed)((double)bx * ay / by);
	if ( dx < ax ) pax = px + dx, qax = qx - ax, wx = ax - dx;
	else pax = px + ax, qax = qx - dx, wx = dx - ax;
	if ( ay >= fixed_1 ||
	    fixed_rounded(py) != fixed_rounded(py + ay)
	   )
	   {	code = gz_fill_trapezoid_fixed(px, fixed_0, py, pax, wx, ay,
					       0, pdevc, pgs);
		if ( code < 0 ) return code;
	   }
	code = gz_fill_trapezoid_fixed(pax, wx, py + ay, qax, wx, by - ay,
				       0, pdevc, pgs);
	if ( code < 0 ) return code;
	py += by;
	if ( ay >= fixed_1 ||
	    fixed_rounded(py) != fixed_rounded(py + ay)
	   )
		return gz_fill_trapezoid_fixed(qax, wx, py, qx, fixed_0, ay,
					       0, pdevc, pgs);
#undef swap
	return 0;
}

/* Default implementation of tile_rectangle */
int
gx_default_tile_rectangle(gx_device *dev, register gx_bitmap *tile,
  int x, int y, int w, int h, gx_color_index color0, gx_color_index color1,
  int px, int py)
{	/* Fill the rectangle in chunks */
	int width = tile->size.x;
	int height = tile->size.y;
	int raster = tile->raster;
	int rwidth = tile->rep_width;
	int irx = ((rwidth & (rwidth - 1)) == 0 ?	/* power of 2 */
		(x + px) & (rwidth - 1) :
		(x + px) % rwidth);
	int ry = (y + py) % tile->rep_height;
	int icw = width - irx;
	int ch = height - ry;
	byte *row = tile->data + ry * raster;
#define d_proc_mono (dev->procs->copy_mono)
	dev_proc_copy_mono((*proc_mono));
#define d_proc_color (dev->procs->copy_color)
	dev_proc_copy_color((*proc_color));
#define d_color_halftone\
		(color0 == gx_no_color_index && color1 == gx_no_color_index)
	int color_halftone;
#define get_color_info()\
  if ( (color_halftone = d_color_halftone) ) proc_color = d_proc_color;\
  else proc_mono = d_proc_mono
	int code;
#ifdef DEBUG
if ( gs_debug['t'] )
   {	int ptx, pty;
	byte *ptp = tile->data;
	dprintf3("[t]tile %dx%d raster=%d;",
		tile->size.x, tile->size.y, tile->raster);
	dprintf6(" x,y=%d,%d w,h=%d,%d p=%d,%d\n",
		x, y, w, h, px, py);
	for ( pty = 0; pty < tile->size.y; pty++ )
	   {	dprintf("   ");
		for ( ptx = 0; ptx < tile->raster; ptx++ )
			dprintf1("%3x", *ptp++);
	   }
	dputc('\n');
   }
#endif
#define real_copy_tile(srcx, tx, ty, tw, th)\
  code =\
    (color_halftone ?\
     (*proc_color)(dev, row, srcx, raster, gx_no_bitmap_id, tx, ty, tw, th) :\
     (*proc_mono)(dev, row, srcx, raster, gx_no_bitmap_id, tx, ty, tw, th, color0, color1));\
  if ( code < 0 ) return_error(code)
#ifdef DEBUG
#define copy_tile(sx, tx, ty, tw, th)\
  if ( gs_debug['t'] )\
	dprintf5("   copy sx=%d x=%d y=%d w=%d h=%d\n",\
		 sx, tx, ty, tw, th);\
  real_copy_tile(sx, tx, ty, tw, th)
#else
#define copy_tile(sx, tx, ty, tw, th)\
  real_copy_tile(sx, tx, ty, tw, th)
#endif
	if ( icw >= w )
	   {	/* Narrow operation */
		int ey, fey, cy;
		if ( ch >= h )
		   {	/* Just one (partial) tile to transfer. */
#define color_halftone d_color_halftone
#define proc_color d_proc_color
#define proc_mono d_proc_mono
			copy_tile(irx, x, y, w, h);
#undef proc_mono
#undef proc_color
#undef color_halftone
			return 0;
		   }
		get_color_info();
		ey = y + h;
		fey = ey - height;
		copy_tile(irx, x, y, w, ch);
		cy = y + ch;
		row = tile->data;
		do
		   {	ch = (cy > fey ? ey - cy : height);
			copy_tile(irx, x, cy, w, ch);
		   }
		while ( (cy += ch) < ey );
		return 0;
	   }
	get_color_info();
	if ( ch >= h )
	   {	/* Shallow operation */
		int ex = x + w;
		int fex = ex - width;
		int cx = x + icw;
		copy_tile(irx, x, y, icw, h);
		while ( cx <= fex )
		   {	copy_tile(0, cx, y, width, h);
			cx += width;
		   }
		if ( cx < ex )
		   {	copy_tile(0, cx, y, ex - cx, h);
		   }
	   }
	else
	   {	/* Full operation */
		int ex = x + w, ey = y + h;
		int fex = ex - width, fey = ey - height;
		int cx, cy;
		for ( cy = y; ; )
		   {	copy_tile(irx, x, cy, icw, ch);
			cx = x + icw;
			while ( cx <= fex )
			   {	copy_tile(0, cx, cy, width, ch);
				cx += width;
			   }
			if ( cx < ex )
			   {	copy_tile(0, cx, cy, ex - cx, ch);
			   }
			if ( (cy += ch) >= ey ) break;
			ch = (cy > fey ? ey - cy : height);
			row = tile->data;
		   }
	   }
#undef copy_tile
#undef real_copy_tile
	return 0;
}

/* Draw a one-pixel-wide line. */
int
gz_draw_line_fixed(fixed ixf, fixed iyf, fixed itoxf, fixed itoyf,
  gx_device_color *pdevc, gs_state *pgs)
{	int ix = fixed2int_var(ixf);
	int iy = fixed2int_var(iyf);
	int itox = fixed2int_var(itoxf);
	int itoy = fixed2int_var(itoyf);
	gx_device *dev;
	if ( itoy == iy )		/* horizontal line */
	  { return (ix <= itox ?
		    gz_fill_rectangle(ix, iy, itox - ix + 1, 1, pdevc, pgs) :
		    gz_fill_rectangle(itox, iy, ix - itox + 1, 1, pdevc, pgs)
		    );
	  }
	if ( itox == ix )		/* vertical line */
	  { return (iy <= itoy ?
		    gz_fill_rectangle(ix, iy, 1, itoy - iy + 1, pdevc, pgs) :
		    gz_fill_rectangle(ix, itoy, 1, iy - itoy + 1, pdevc, pgs)
		    );
	  }
	if ( color_is_pure(pdevc) &&
	    (dev = pgs->device->info,
	     (*dev->procs->draw_line)(dev, ix, iy, itox, itoy,
				      pdevc->color1)) >= 0 )
	  return 0;
	{ fixed h = itoyf - iyf;
	  fixed w = itoxf - ixf;
	  fixed tf;
#define fswap(a, b) tf = a, a = b, b = tf
#define half float2fixed(0.5)
	  if ( (w < 0 ? -w : w) <= (h < 0 ? -h : h) )
	    { if ( h < 0 )
		fswap(ixf, itoxf), fswap(iyf, itoyf),
		h = -h;
	      return gz_fill_trapezoid_fixed(ixf - half, fixed_1, iyf,
					     itoxf - half, fixed_1, h,
					     0, pdevc, pgs);
	    }
	  else
	    { if ( w < 0 )
		fswap(ixf, itoxf), fswap(iyf, itoyf),
		w = -w;
	      return gz_fill_trapezoid_fixed(iyf - half, fixed_1, ixf,
					     itoyf - half, fixed_1, w,
					     1, pdevc, pgs);
	    }
#undef half
#undef fswap
	}
}

/* A stub to force use of the standard procedure. */
int
gx_default_draw_line(gx_device *dev,
  int x0, int y0, int x1, int y1, gx_color_index color)
{	return -1;
}
