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

/* gxpath2.c */
/* Path tracing procedures for Ghostscript library */
#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gzpath.h"

/* Forward declarations */
private int copy_path(P3(gx_path *, gx_path *, fixed));
private int flatten_recur(P8(gx_path *,
  fixed, fixed, fixed, fixed, fixed, fixed, fixed));

/* Read the current point of a path. */
int
gx_path_current_point(gx_path *ppath, gs_fixed_point *ppt)
{	if ( !ppath->position_valid )
	  return_error(gs_error_nocurrentpoint);
	/* Copying the coordinates individually */
	/* is much faster on a PC, and almost as fast on other machines.... */
	ppt->x = ppath->position.x, ppt->y = ppath->position.y;
	return 0;
}

/* Read the bounding box of a path. */
int
gx_path_bbox(gx_path *ppath, gs_fixed_rect *pbox)
{	if ( ppath->first_subpath == 0 )
	   {	/* The path is empty, use the current point if any. */
		gx_path_current_point(ppath, &pbox->p);
		return gx_path_current_point(ppath, &pbox->q);
	   }
	/* The stored bounding box may not be up to date. */
	/* Correct it now if necessary. */
	if ( ppath->box_last == ppath->current_subpath->last )
	   {	/* Box is up to date */
		*pbox = ppath->bbox;
	   }
	else
	   {	gs_fixed_rect box;
		register segment *pseg = ppath->box_last;
		if ( pseg == 0 )	/* box is uninitialized */
		   {	pseg = (segment *)ppath->first_subpath;
			box.p.x = box.q.x = pseg->pt.x;
			box.p.y = box.q.y = pseg->pt.y;
		   }
		else
		   {	box = ppath->bbox;
			pseg = pseg->next;
		   }
/* Macro for adjusting the bounding box when adding a point */
#define adjust_bbox(pt)\
  if ( (pt).x < box.p.x ) box.p.x = (pt).x;\
  else if ( (pt).x > box.q.x ) box.q.x = (pt).x;\
  if ( (pt).y < box.p.y ) box.p.y = (pt).y;\
  else if ( (pt).y > box.q.y ) box.q.y = (pt).y
		while ( pseg )
		   {	switch ( pseg->type )
			   {
			case s_curve:
#define pcurve ((curve_segment *)pseg)
				adjust_bbox(pcurve->p1);
				adjust_bbox(pcurve->p2);
#undef pcurve
				/* falls through */
			default:
				adjust_bbox(pseg->pt);
			   }
			pseg = pseg->next;
		   }
#undef adjust_bbox
		ppath->bbox = box;
		ppath->box_last = ppath->current_subpath->last;
		*pbox = box;
	   }
	return 0;
}

/* Test if a path has any curves. */
int
gx_path_has_curves(gx_path *ppath)
{	return ppath->curve_count != 0;
}

/* Test if a path has any segments. */
int
gx_path_is_void(gx_path *ppath)
{	return ppath->first_subpath == 0;
}

/* Test if a path is a rectangle. */
/* If so, return its bounding box. */
/* Note that this must recognize open as well as closed rectangles. */
int
gx_path_is_rectangle(gx_path *ppath, gs_fixed_rect *pbox)
{	subpath *pseg0;
	segment *pseg1, *pseg2, *pseg3, *pseg4;
	if (	ppath->subpath_count == 1 &&
		(pseg1 = (pseg0 = ppath->first_subpath)->next) != 0 &&
		(pseg2 = pseg1->next) != 0 &&
		(pseg3 = pseg2->next) != 0 &&
		((pseg4 = pseg3->next) == 0 || pseg4->type == s_line_close) &&
		ppath->curve_count == 0
	   )
	   {	fixed x0 = pseg0->pt.x, y0 = pseg0->pt.y;
		fixed x2 = pseg2->pt.x, y2 = pseg2->pt.y;
		if (	(x0 == pseg1->pt.x && pseg1->pt.y == y2 &&
			 x2 == pseg3->pt.x && pseg3->pt.y == y0) ||
			(x0 == pseg3->pt.x && pseg3->pt.y == y2 &&
			 x2 == pseg1->pt.x && pseg1->pt.y == y0)
		   )
		   {	/* Path is a rectangle.  Return bounding box. */
			if ( x0 < x2 )
				pbox->p.x = x0, pbox->q.x = x2;
			else
				pbox->p.x = x2, pbox->q.x = x0;
			if ( y0 < y2 )
				pbox->p.y = y0, pbox->q.y = y2;
			else
				pbox->p.y = y2, pbox->q.y = y0;
			return 1;
		   }
	   }
	return 0;
}

/* Copy a path */
int
gx_path_copy(gx_path *ppath_old, gx_path *ppath)
{	return copy_path(ppath_old, ppath, (fixed)0);
}

/* Translate an already-constructed path (in device space). */
/* Don't bother to translate the cbox. */
int
gx_path_translate(gx_path *ppath, fixed dx, fixed dy)
{	segment *pseg;
#define translate_xy(pt)\
  pt.x += dx, pt.y += dy
	translate_xy(ppath->bbox.p);
	translate_xy(ppath->bbox.q);
	translate_xy(ppath->position);
	pseg = (segment *)(ppath->first_subpath);
	while ( pseg )
	   {	switch ( pseg->type )
		   {
		case s_curve:
		   {	curve_segment *pc = (curve_segment *)pseg;
			translate_xy(pc->p1);
			translate_xy(pc->p2);
		   }
		default:
			translate_xy(pseg->pt);
		   }
		pseg = pseg->next;
	   }
	return 0;
}

/* Flatten a path */
private fixed
scale_flatness(floatp flatness)
{	/* See the flattening algorithm below for an explanation of */
	/* the following computation. */
	fixed scaled_flat = float2fixed(flatness);
	return (scaled_flat > int2fixed(100) ? int2fixed(100) :
		scaled_flat <= float2fixed(0.2) ? float2fixed(0.2) :
		scaled_flat);
}
int
gx_path_flatten(gx_path *ppath_old, gx_path *ppath, floatp flatness)
{	return copy_path(ppath_old, ppath, scale_flatness(flatness));
}

/* Add a flattened curve to a path. */
int
gx_path_add_flattened_curve(gx_path *ppath,
  fixed x1, fixed y1, fixed x2, fixed y2, fixed x3, fixed y3,
  floatp flatness)
{	return flatten_recur(ppath, x1, y1, x2, y2, x3, y3,
			     scale_flatness(flatness));
}

/* Copy a path, optionally flattening it. */
/* If the copy fails, free the new path. */
private int
copy_path(gx_path *ppath_old, gx_path *ppath, fixed scaled_flat)
{	gx_path old;
	segment *pseg;
	int code;
#ifdef DEBUG
if ( gs_debug['p'] )
	gx_dump_path(ppath_old, "before copy_path");
#endif
	old = *ppath_old;
	gx_path_init(ppath, &ppath_old->memory_procs);
	pseg = (segment *)(old.first_subpath);
	while ( pseg )
	   {	switch ( pseg->type )
		   {
		case s_start:
			code = gx_path_add_point(ppath, pseg->pt.x, pseg->pt.y);
			break;
		case s_curve:
		   {	curve_segment *pc = (curve_segment *)pseg;
			if ( scaled_flat == 0 )	/* don't flatten */
				code = gx_path_add_curve(ppath,
					pc->p1.x, pc->p1.y,
					pc->p2.x, pc->p2.y,
					pc->pt.x, pc->pt.y);
			else
				code = flatten_recur(ppath,
					pc->p1.x, pc->p1.y,
					pc->p2.x, pc->p2.y,
					pc->pt.x, pc->pt.y,
					scaled_flat);
			break;
		   }
		case s_line:
			code = gx_path_add_line(ppath, pseg->pt.x, pseg->pt.y);
			break;
		case s_line_close:
			code = gx_path_close_subpath(ppath);
			break;
		   }
		if ( code )
		   {	gx_path_release(ppath);
			if ( ppath == ppath_old ) *ppath_old = old;
			return code;
		   }
		pseg = pseg->next;
	}
	ppath->position = old.position;		/* restore current point */
#ifdef DEBUG
if ( gs_debug['p'] )
	gx_dump_path(ppath, "after copy_path");
#endif
	return 0;
}
/* Internal routine to flatten a curve. */
/* This calls itself recursively, using binary subdivision, */
/* until the approximation is good enough to satisfy the */
/* flatness requirement.  The starting point is ppath->position, */
/* which gets updated as line segments are added. */

/* Table of f(i) = 256 * sqrt(1 + (i/64)^2). */
/* This is good to within 1% or better. */
#define sqrt_index_shift 6		/* scaling of index */
#define sqrt_value_shift 8		/* scaling of value */
private int scaled_sqrt_tab[65] =
   {	256, 256, 256, 256, 256, 256, 257, 257,
	257, 258, 259, 259, 260, 261, 262, 262,
	263, 264, 265, 267, 268, 269, 270, 272,
	273, 274, 276, 277, 279, 281, 282, 284,
	286, 288, 289, 291, 293, 295, 297, 299,
	301, 304, 306, 308, 310, 312, 315, 317,
	320, 322, 324, 327, 329, 332, 334, 337,
	340, 342, 345, 348, 350, 353, 356, 359,
	362
   };
private int flatten_sample(P8(gx_path *, int,
  fixed, fixed, fixed, fixed, fixed, fixed));

private int
flatten_recur(gx_path *ppath,
  fixed x1, fixed y1, fixed x2, fixed y2, fixed x3, fixed y3,
  fixed scaled_flat)
{	fixed
	  x0 = ppath->position.x,
	  y0 = ppath->position.y;
top:
#ifdef DEBUG
if ( gs_debug['2'] )
	dprintf4("[2]x0=%f y0=%f x1=%f y1=%f\n",
		 fixed2float(x0), fixed2float(y0),
		 fixed2float(x1), fixed2float(y1)),
	dprintf4("   x2=%f y2=%f x3=%f y3=%f\n",
		 fixed2float(x2), fixed2float(y2),
		 fixed2float(x3), fixed2float(y3));
#endif
	/*
	 * Compute the maximum distance of the curve from
	 * the line (x0,y0)->(x3,y3).  We do this conservatively
	 * by observing that the curve is enclosed by the
	 * quadrilateral of its control points, so we simply
	 * compute the distances of (x1,y1) and (x2,y2)
	 * from the line.  Letting dx = x3-x0 and dy = y3-y0,
	 * the distance of (xp,yp) from the line is
	 * abs(N)/sqrt(D), where N = dy*(xp-x0)-dx*(yp-y0) and
	 * D = dx*dx+dy*dy; hence we want to test abs(N) <= sqrt(D)*F,
	 * where F is the flatness parameter from the graphics state.
	 * We can do this more efficiently by letting t=dy/dx, and
	 * testing abs(N1) <= sqrt(D1)*f, where N1=t*(xp-x0)-(yp-y0) and
	 * D1 = 1+t*t.  If dx < dy, we swap x and y for this
	 * computation.  This guarantees abs(t) <= 1, which allows us to
	 * compute sqrt(1+t*t) by table lookup on the high bits of abs(t).
	 */
	 { fixed dx3 = x3 - x0;
	   fixed adx3 = any_abs(dx3);
	   fixed dy3 = y3 - y0;
	   fixed ady3 = any_abs(dy3);
	   /* We have to be quite careful to ensure that */
	   /* none of the multiplications will overflow. */
#define short_max 0x7ff0L
#define reduce_3(ad3, maxv)\
  while ( ad3 > maxv )\
    adx3 >>= 1, ady3 >>= 1,\
    dx3 = arith_rshift_1(dx3), dy3 = arith_rshift_1(dy3)
#define reduce_d(d)\
  for ( shift = 0; (d < 0 ? d < -short_max : d > short_max); shift++ )\
    d = arith_rshift_1(d)
	   if ( adx3 > ady3 )
	    {	fixed d, dx, dy, dist;
		int shift;
		reduce_3(ady3, short_max);
		d = (scaled_sqrt_tab[(ady3 << sqrt_index_shift) / adx3] * scaled_flat) >> sqrt_value_shift;
		dx = x1 - x0, dy = y1 - y0;
		reduce_d(dx);
		if ( ((dist = ((dx * dy3 / dx3) << shift) - dy) < 0 ?
		      -dist : dist) > d )
		  goto sub;	/* not flat enough */
		dx = x2 - x0, dy = y2 - y0;
		reduce_d(dx);
		if ( ((dist = ((dx * dy3 / dx3) << shift) - dy) < 0 ?
		      -dist : dist) > d )
		  goto sub;	/* not flat enough */
	    }
	   else if ( ady3 != 0 )
	    {	fixed d, dy, dx, dist;
		int shift;
		reduce_3(adx3, short_max);
		d = (scaled_sqrt_tab[(adx3 << sqrt_index_shift) / ady3] * scaled_flat) >> sqrt_value_shift;
		dy = y1 - y0, dx = x1 - x0;
		reduce_d(dy);
		if ( ((dist = ((dy * dx3 / dy3) << shift) - dx) < 0 ?
		      -dist : dist) > d )
		  goto sub;	/* not flat enough */
		dy = y2 - y0, dx = x2 - x0;
		reduce_d(dy);
		if ( ((dist = ((dy * dx3 / dy3) << shift) - dx) < 0 ?
		      -dist : dist) > d )
		  goto sub;	/* not flat enough */
	    }
	   else				/* adx3 = ady3 = 0 */
	    {	/* (x0,y0) is the same point as (x3,y3). */
		/* This is an anomalous case.  If the entire curve */
		/* is a single point, stop now, otherwise subdivide. */
		if ( x1 != x0 || y1 != y0 || x2 != x0 || y2 != y0 )
		  goto sub;
	    }
	 }
	/* Curve is flat enough.  Add a line and exit. */
#ifdef DEBUG
if ( gs_debug['2'] )
	dprintf2("[2]\t*** x=%f, y=%f ***\n",
		 fixed2float(x3), fixed2float(y3));
#endif
	return gx_path_add_line(ppath, x3, y3);

	/* Curve isn't flat enough.  Break into two pieces and recur. */
	/* Algorithm is from "The Beta2-split: A special case of the */
	/* Beta-spline Curve and Surface Representation," B. A. Barsky */
	/* and A. D. DeRose, IEEE, 1985, courtesy of Crispin Goswell. */
sub:
	/* We have to define midpoint carefully to avoid overflow. */
	/* (If it overflows, something really pathological is going on, */
	/* but we could get infinite recursion that way.... */
#define midpoint(a,b)\
  (arith_rshift_1(a) + arith_rshift_1(b) + ((a) & (b) & 1))
   {	fixed x01 = midpoint(x0, x1), y01 = midpoint(y0, y1);
	fixed x12 = midpoint(x1, x2), y12 = midpoint(y1, y2);
	fixed x02 = midpoint(x01, x12), y02 = midpoint(y01, y12);
	int code;
	/* Update x/y1, x/y2, and x/y0 now for the second half. */
	x2 = midpoint(x2, x3), y2 = midpoint(y2, y3);
	x1 = midpoint(x12, x2), y1 = midpoint(y12, y2);
	code = flatten_recur(ppath, x01, y01, x02, y02,
		(x0 = midpoint(x02, x1)), (y0 = midpoint(y02, y1)),
		scaled_flat);
	if ( code < 0 ) return code;
   }	goto top;
}

/* Flatten a segment of the path by repeated sampling. */
/* For some reason, this produces better results, */
/* even though flatten_recur is careful to check the flatness.... */
/* n is the number of points to sample, including the endpoints; */
/* we require n >= 3. */
private int
flatten_sample(gx_path *ppath, int n,
  fixed x1, fixed y1, fixed x2, fixed y2, fixed x3, fixed y3)
{	const fixed
		x0 = ppath->position.x,
		y0 = ppath->position.y;
	const fixed
		cx = 3 * (x1 - x0),
		bx = 3 * (x2 - x1) - cx,
		ax = x3 - bx - cx - x0;
	const fixed
		cy = 3 * (y1 - y0),
		by = 3 * (y2 - y1) - cy,
		ay = y3 - by - cy - y0;
	const float
		dt = 1.0 / (float)(n - 1);
	int i;

	for ( i = 1; i < n-1; i++ )
	   {	const float t = dt * (float)(i);
		const fixed x = x0 + (fixed)(t*(cx + t*(bx + t*ax)));
		const fixed y = y0 + (fixed)(t*(cy + t*(by + t*ay)));
		int code;
		if ( 0 != (code = gx_path_add_line(ppath, x, y)) )
			return code;
	   }
	return gx_path_add_line(ppath, x3, y3);
}

/* Reverse a path. */
/* We know ppath != ppath_old. */
int
gx_path_reverse(gx_path *ppath_old, gx_path *ppath)
{	subpath *psub = ppath_old->first_subpath;
#ifdef DEBUG
if ( gs_debug['p'] )
	gx_dump_path(ppath_old, "before reversepath");
#endif
	gx_path_init(ppath, &ppath_old->memory_procs);
nsp:	while ( psub )
	   {	segment *pseg = psub->last;
		segment *prev;
		int code = gx_path_add_point(ppath, pseg->pt.x, pseg->pt.y);
		if ( code < 0 )
		   {	gx_path_release(ppath);
			return code;
		   }
		for ( ; ; pseg = prev )
		   {	prev = pseg->prev;
			switch ( pseg->type )
			   {
			case s_start:
				/* Finished subpath */
				if ( psub->closed )
					code = gx_path_close_subpath(ppath);
				psub = (subpath *)psub->last->next;
				goto nsp;
			case s_curve:
			   {	curve_segment *pc = (curve_segment *)pseg;
				code = gx_path_add_curve(ppath,
					pc->p2.x, pc->p2.y,
					pc->p1.x, pc->p1.y,
					prev->pt.x, prev->pt.y);
				break;
			   }
			case s_line:
			case s_line_close:
				code = gx_path_add_line(ppath, prev->pt.x, prev->pt.y);
				break;
			   }
			if ( code )
			   {	gx_path_release(ppath);
				return code;
			   }
		   }
	}
	ppath->position = ppath_old->position;		/* restore current point */
#ifdef DEBUG
if ( gs_debug['p'] )
	gx_dump_path(ppath, "after reversepath");
#endif
	return 0;
}
