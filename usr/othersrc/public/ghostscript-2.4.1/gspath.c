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

/* gspath.c */
/* Path construction routines for Ghostscript library */
#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gxpath.h"
#include "gzstate.h"

/* Conversion parameters */
#define degrees_to_radians (M_PI / 180.0)

/* ------ Miscellaneous ------ */

int
gs_newpath(gs_state *pgs)
{	gx_path_release(pgs->path);
	gx_path_init(pgs->path, &pgs->memory_procs);
	return 0;
}

int
gs_closepath(gs_state *pgs)
{	return gx_path_close_subpath(pgs->path);
}

int
gs_upmergepath(gs_state *pgs)
{	return gx_path_add_path(pgs->saved->path, pgs->path);
}

/* ------ Points and lines ------ */

int
gs_currentpoint(const gs_state *pgs, gs_point *ppt)
{	gs_fixed_point pt;
	int code = gx_path_current_point(pgs->path, &pt);
	if ( code < 0 ) return code;
	return gs_itransform(pgs, fixed2float(pt.x), fixed2float(pt.y), ppt);
}

int
gs_moveto(gs_state *pgs, floatp x, floatp y)
{	int code;
	gs_fixed_point pt;
	if ( (code = gs_point_transform2fixed(&pgs->ctm, x, y, &pt)) >= 0 )
		code = gx_path_add_point(pgs->path, pt.x, pt.y);
	return code;
}

int
gs_rmoveto(gs_state *pgs, floatp x, floatp y)
{	int code;
	gs_fixed_point dpt;
	if ( (code = gs_distance_transform2fixed(&pgs->ctm, x, y, &dpt)) >= 0 )
		code = gx_path_add_relative_point(pgs->path, dpt.x, dpt.y);
	return code;
}

int
gs_lineto(gs_state *pgs, floatp x, floatp y)
{	int code;
	gs_fixed_point pt;
	if ( (code = gs_point_transform2fixed(&pgs->ctm, x, y, &pt)) >= 0 )
		code = gx_path_add_line(pgs->path, pt.x, pt.y);
	return code;
}

int
gs_rlineto(gs_state *pgs, floatp x, floatp y)
{	gs_fixed_point cpt, dpt;
	int code = gx_path_current_point(pgs->path, &cpt);
	if ( code < 0 ) return code;
	if ( (code = gs_distance_transform2fixed(&pgs->ctm, x, y, &dpt)) >= 0 )
		code = gx_path_add_line(pgs->path, cpt.x + dpt.x, cpt.y + dpt.y);
	return code;
}

/* ------ Arcs ------ */

/* Forward declarations */
private int arc_either(P7(gs_state *,
  floatp, floatp, floatp, floatp, floatp, int));
private int arc_add(P9(gs_state *,
  floatp, floatp, floatp, floatp, floatp, floatp, floatp, int));

int
gs_arc(gs_state *pgs,
  floatp xc, floatp yc, floatp r, floatp ang1, floatp ang2)
{	return arc_either(pgs, xc, yc, r, ang1, ang2, 0);
}

int
gs_arcn(gs_state *pgs,
  floatp xc, floatp yc, floatp r, floatp ang1, floatp ang2)
{	return arc_either(pgs, xc, yc, r, ang1, ang2, 1);
}

private int
arc_either(gs_state *pgs,
  floatp axc, floatp ayc, floatp arad, floatp aang1, floatp aang2,
  int clockwise)
{	float ar = arad;
	fixed ang1 = float2fixed(aang1), ang2 = float2fixed(aang2), adiff;
	float ang1r;
	float x0, y0, sin0, cos0;
	float x3r, y3r;
	int first = 1;
	int code;
	if ( ar < 0 )
	   {	ang1 += int2fixed(180);
		ang2 += int2fixed(180);
		ar = - ar;
	   }
#define fixed90 int2fixed(90)
#define fixed360 int2fixed(360)
	/* Don't reduce the arc by multiples of 360 degrees: */
	/* this could lead to incorrect winding numbers. */
	if ( ang1 != ang2 )
	   {	if ( clockwise )
		   {	while ( ang2 >= ang1 ) ang1 += fixed360;
		   }
		else
		   {	while ( ang2 <= ang1 ) ang2 += fixed360;
		   }
	   }
	ang1r = fixed2float(ang1 % fixed360) * degrees_to_radians;
	sin0 = ar * sin(ang1r), cos0 = ar * cos(ang1r);
	x0 = axc + cos0, y0 = ayc + sin0;
	if ( clockwise )
	   {	/* Quadrant reduction */
		while ( (adiff = ang2 - ang1) < -fixed90 )
		   {	float w = sin0; sin0 = -cos0; cos0 = w;
			x3r = axc + cos0, y3r = ayc + sin0;
			code = arc_add(pgs, ar, x0, y0, x3r, y3r,
				(x0 + cos0),
				(y0 + sin0),
				first);
			if ( code < 0 ) return code;
			x0 = x3r, y0 = y3r;
			ang1 -= fixed90;
			first = 0;
		   }
	   }
	else
	   {	/* Quadrant reduction */
		while ( (adiff = ang2 - ang1) > fixed90 )
		   {	float w = cos0; cos0 = -sin0; sin0 = w;
			x3r = axc + cos0, y3r = ayc + sin0;
			code = arc_add(pgs, ar, x0, y0, x3r, y3r,
				(x0 + cos0),
				(y0 + sin0),
				first);
			if ( code < 0 ) return code;
			x0 = x3r, y0 = y3r;
			ang1 += fixed90;
			first = 0;
		   }
	   }
	/* Compute the intersection of the tangents. */
	   {	double trad = tan(fixed2float(adiff) * (degrees_to_radians / 2));
		float ang2r = fixed2float(ang2) * degrees_to_radians;
		code = arc_add(pgs, ar, x0, y0,
			(axc + ar * cos(ang2r)),
			(ayc + ar * sin(ang2r)),
			(x0 - trad * sin0),
			(y0 + trad * cos0),
			first);
	   }
	return code;
}

int
gs_arcto(gs_state *pgs,
  floatp ax1, floatp ay1, floatp ax2, floatp ay2, floatp arad,
  float *retxy)			/* float retxy[4] */
{	float xt0, yt0, xt2, yt2;
	gs_point up0;
#define ax0 up0.x
#define ay0 up0.y
	int code;
	if ( arad < 0 )
		return_error(gs_error_undefinedresult);
	/* Transform the current point back into user coordinates */
	if ( (code = gs_currentpoint(pgs, &up0)) < 0 ) return code;
	   {	/* Now we have to compute the tangent points. */
		/* Basically, the idea is to compute the tangent */
		/* of the bisector by using tan(x+y) and tan(z/2) */
		/* formulas, without ever using any trig. */
		float dx0 = ax0 - ax1, dy0 = ay0 - ay1;
		float dx2 = ax2 - ax1, dy2 = ay2 - ay1;
		/* Compute the squared lengths from p1 to p0 and p2. */
		double sql0 = dx0 * dx0 + dy0 * dy0;
		double sql2 = dx2 * dx2 + dy2 * dy2;
		/* Compute the distance from p1 to the tangent points. */
		/* This is the only hairy part. */
		double num = dy0 * dx2 - dy2 * dx0;
		double denom = sqrt(sql0 * sql2) - (dx0 * dx2 + dy0 * dy2);
		/* Check for collinear points. */
		if ( fabs(num) < 1.0e-6 || fabs(denom) < 1.0e-6 )
		   {	gs_fixed_point pt;
			code = gs_point_transform2fixed(&pgs->ctm, ax1, ay1, &pt);
			if ( code >= 0 ) code = gx_path_add_line(pgs->path, pt.x, pt.y);
			xt0 = xt2 = ax1;
			yt0 = yt2 = ay1;
		   }
		else		/* not collinear */
		   {	double dist = fabs(arad * num / denom);
			double l0 = dist / sqrt(sql0), l2 = dist / sqrt(sql2);
			xt0 = ax1 + dx0 * l0;
			yt0 = ay1 + dy0 * l0;
			xt2 = ax1 + dx2 * l2;
			yt2 = ay1 + dy2 * l2;
			code = arc_add(pgs, arad, xt0, yt0, xt2, yt2, ax1, ay1, 1);
		   }
	   }
	if ( retxy != 0 )
	   {	retxy[0] = xt0;
		retxy[1] = yt0;
		retxy[2] = xt2;
		retxy[3] = yt2;
	   }
	return code;
}

/* Internal routine for adding an arc to the path. */
private int
arc_add(gs_state *pgs,
  floatp r, floatp x0, floatp y0, floatp x3, floatp y3, floatp xt, floatp yt,
  int first)
{	gx_path *path = pgs->path;
	floatp dx = xt - x0, dy = yt - y0;
	floatp fraction;
	gs_fixed_point p0, p3, pt, cpt;
	int code;
	/* Compute the fraction coefficient for the curve. */
	/* See gx_path_add_arc for details. */
	if ( fabs(r) < 1.0e-4 )		/* almost zero radius */
	   {	fraction = 0.0;
	   }
	else
	   {	double ratio2 = (dx * dx + dy * dy) / (r * r);
		fraction = (4.0/3.0) / (1 + sqrt(1 + ratio2));
	   }
#ifdef DEBUG
if ( gs_debug['r'] )
	dprintf7("[r]Arc f=%f p0=(%f,%f) pt=(%f,%f) p3=(%f,%f) first=%d\n",
		 x0, y0, xt, yt, x3, y3, first);
#endif
	if (	(code = gs_point_transform2fixed(&pgs->ctm, x0, y0, &p0)) < 0 ||
		(code = gs_point_transform2fixed(&pgs->ctm, x3, y3, &p3)) < 0 ||
		(code = gs_point_transform2fixed(&pgs->ctm, xt, yt, &pt)) < 0 ||
		(first && (code = (gx_path_current_point(path, &cpt) >= 0 ?
			 gx_path_add_line(path, p0.x, p0.y) :
			 gx_path_add_point(path, p0.x, p0.y))) < 0)
	   )
		return code;
	return gx_path_add_arc(path, p0.x, p0.y, p3.x, p3.y, pt.x, pt.y, fraction);
}

/* ------ Curves ------ */

int
gs_curveto(gs_state *pgs,
  floatp x1, floatp y1, floatp x2, floatp y2, floatp x3, floatp y3)
{	gs_fixed_point p1, p2, p3;
	int code;
	if (	(code = gs_point_transform2fixed(&pgs->ctm, x1, y1, &p1)) < 0 ||
		(code = gs_point_transform2fixed(&pgs->ctm, x2, y2, &p2)) < 0 ||
		(code = gs_point_transform2fixed(&pgs->ctm, x3, y3, &p3)) < 0
	   ) return code;
	return gx_path_add_curve(pgs->path,
		p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);
}

int
gs_rcurveto(gs_state *pgs,
  floatp dx1, floatp dy1, floatp dx2, floatp dy2, floatp dx3, floatp dy3)
{	gs_fixed_point pt, p1, p2, p3;
	int code = gx_path_current_point(pgs->path, &pt);
	if ( code < 0 ) return code;
	if (	(code = gs_distance_transform2fixed(&pgs->ctm, dx1, dy1, &p1)) < 0 ||
		(code = gs_distance_transform2fixed(&pgs->ctm, dx2, dy2, &p2)) < 0 ||
		(code = gs_distance_transform2fixed(&pgs->ctm, dx3, dy3, &p3)) < 0
	   ) return code;
	return gx_path_add_curve(pgs->path,
		pt.x + p1.x, pt.y + p1.y,
		pt.x + p2.x, pt.y + p2.y,
		pt.x + p3.x, pt.y + p3.y);
}
