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

/* gxstroke.c */
/* Path stroking procedures for Ghostscript library */
#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzdevice.h"
#include "gzcolor.h"			/* requires gxdevice.h */
#include "gzline.h"
#include "gzpath.h"

/* stroke_add uses the following global for its path: */
private gx_path stroke_path_body;
private gx_path *stroke_path;

/*
 * Structure for a partial line (passed to the drawing routine).
 * Two of these are required to do joins right.
 * Each endpoint includes the two ends of the cap as well,
 * and the deltas for square and round cap computation.
 *
 * The deltas (co, cdelta, ce) are in clockwise order in device space
 * around the endpoint p: they are one-half the line width (suitably
 * transformed) at 90 degrees counter-clockwise, straight ahead,
 * and 90 degrees clockwise from the oriented line o->e,
 * where "90 degrees" is measured in *user* coordinates.
 * Note that the values at o are the negatives of the values at e.
 *
 * Initially, only o.p, e.p, o.cdelta, width, and thin are set.
 * compute_caps fills in the rest when needed.
 */
typedef gs_fixed_point _ss *p_ptr;
typedef struct endpoint_s {
	gs_fixed_point p;		/* the end of the line */
	gs_fixed_point co, ce;		/* ends of the cap, p +/- width */
	gs_fixed_point cdelta;		/* +/- cap length */
} endpoint;
typedef endpoint _ss *ep_ptr;
typedef struct partial_line_s {
	endpoint o;			/* starting coordinate */
	endpoint e;			/* ending coordinate */
	gs_fixed_point width;		/* one-half line width, see above */
	int thin;			/* true if minimum-width line */
} partial_line;
typedef partial_line _ss *pl_ptr;

/* Procedures that stroke a partial_line (the first argument). */
/* If both partial_lines are non-null, the procedure creates */
/* an appropriate join; otherwise, the procedure creates an */
/* end cap.  If the first int is 0, the procedure also starts with */
/* an appropriate cap. */
private int stroke_add(P4(int, pl_ptr, pl_ptr, gs_state *));
private int stroke_fill(P4(int, pl_ptr, pl_ptr, gs_state *));

/* Other forward declarations */
private int stroke(P3(gx_path *,
  int (*)(P4(int, pl_ptr, pl_ptr, gs_state *)),
  gs_state *));
private int expand_dashes(P3(subpath *, gx_path *, gs_state *));
private void compute_caps(P1(pl_ptr));
private int add_capped(P4(gx_path *, gs_line_cap,
  int (*)(P3(gx_path *, fixed, fixed)),
  ep_ptr));

/* Stroke a path for drawing or saving */
int
gx_stroke_fill(gx_path *ppath, gs_state *pgs)
{	int code;
	stroke_path = 0;
	code = stroke(ppath, stroke_fill, pgs);
	if ( stroke_path )		/* set if filling needed */
	  { if ( code >= 0 )
	      code = gx_fill_path(stroke_path, pgs->dev_color, pgs,
				  gx_rule_winding_number, (fixed)0);
	    gx_path_release(stroke_path);
	  }
	return code;
}
int
gx_stroke_add(gx_path *ppath, gx_path *topath, gs_state *pgs)
{	stroke_path = topath;
	return stroke(ppath, stroke_add, pgs);
}

/* Stroke a path.  Call line_proc (stroke_add or stroke_fill) */
/* for each line segment. */
private int
stroke(gx_path *ppath,
  int (*line_proc)(P4(int, pl_ptr, pl_ptr, gs_state *)),
  gs_state *pgs)
{	subpath *psub;
	subpath *save_psub = 0;
	int code = 0;
	line_params *lp = pgs->line_params;
	int dash_count = lp->dash.pattern_size;
	gx_path fpath, dpath;
	float xx = pgs->ctm.xx, xy = pgs->ctm.xy;
	float yx = pgs->ctm.yx, yy = pgs->ctm.yy;
	int skewed = !is_fzero2(xy, yx);
	int uniform = (skewed ? 0 : xx == yy ? 1 : xx == -yy ? -1 : 0);
	/*
	 * We are dealing with a reflected coordinate system
	 * if (1,0) is counter-clockwise from (0,1).
	 * See the note in stroke_add for the algorithm.
	 */	
	int reflected =
	  (uniform ? uniform > 0 :
	   skewed ? xy * yx < xx * yy :
	   (xx < 0) == (yy < 0));
	float line_width = lp->width;	/* this is *half* the line width! */
	int always_thin;
	float line_width_and_scale;
#ifdef DEBUG
if ( gs_debug['o'] )
   {	int count = lp->dash.pattern_size;
	int i;
	dprintf3("[o]half_width=%f, cap=%d, join=%d,\n",
		 lp->width, (int)lp->cap, (int)lp->join);
	dprintf2("   miter_limit=%f, miter_check=%f,\n",
		 lp->miter_limit, lp->miter_check);
	dprintf1("   dash pattern=%d", count);
	for ( i = 0; i < count; i++ )
		dprintf1(",%f", lp->dash.pattern[i]);
	dprintf4(",\n   offset=%f, init(ink_on=%d, index=%d, dist_left=%f)\n",
		 lp->dash.offset, lp->dash.init_ink_on, lp->dash.init_index,
		 lp->dash.init_dist_left);
   }
#endif
	if ( line_width < 0 ) line_width = -line_width;
	if ( is_fzero(line_width) )
		always_thin = 1;
	else if ( !skewed )
	   {	float xxa = xx, yya = yy;
		if ( xxa < 0 ) xxa = -xxa;
		if ( yya < 0 ) yya = -yya;
		always_thin = (max(xxa, yya) * line_width < 0.5);
	   }
	else
	   {	/* The check is more complicated, but it's worth it. */
		float xsq = xx * xx + xy * xy;
		float ysq = yx * yx + yy * yy;
		float cross = xx * yx + xy * yy;
		if ( cross < 0 ) cross = 0;
		always_thin =
		  ((max(xsq, ysq) + cross) * line_width * line_width < 0.5);
	   }
	line_width_and_scale = line_width * (float)int2fixed(1);
#ifdef DEBUG
if ( gs_debug['o'] )
	dprintf5("[o]ctm=(%g,%g,%g,%g) thin=%d\n",
		 xx, xy, yx, yy, always_thin);
#endif
	/* Start by flattening the path.  We should do this on-the-fly.... */
	if ( !ppath->curve_count )	/* don't need to flatten */
	   {	psub = ppath->first_subpath;
		if ( !psub ) return 0;
	   }
	else
	   {	if ( (code = gx_path_flatten(ppath, &fpath, pgs->flatness)) < 0 ) return code;
		psub = fpath.first_subpath;
	   }
	if ( dash_count )
		gx_path_init(&dpath, &ppath->memory_procs);
	for ( ; ; )
	 { line_segment *pline;
	   fixed x, y;
	   partial_line pl, pl_prev, pl_first;
	   int first = 0;
	   int index = 0;
	   if ( !psub )
	    {	/* Might just be the end of a dash expansion. */
		if ( save_psub )
		   {	gx_path_release(&dpath);
			psub = (subpath *)save_psub->last->next;
			if ( !psub ) break;
			gx_path_init(&dpath, &ppath->memory_procs);
			save_psub = 0;
		   }
		else		/* all done */
			break;
	    }
	   if ( dash_count && !save_psub )
	    {	code = expand_dashes(psub, &dpath, pgs);
		if ( code < 0 ) goto exit;
		save_psub = (subpath *)psub;
		psub = dpath.first_subpath;
		continue;		/* psub might be null */
	    }
	   pline = (line_segment *)(psub->next);
	   x = psub->pt.x;
	   y = psub->pt.y;
	   while ( pline != 0 && pline->type != s_start )
	    {	fixed sx = pline->pt.x;
		fixed sy = pline->pt.y;
		/* Compute the width parameters in device space. */
		/* We work with unscaled values, for speed. */
		pl.o.p.x = x, pl.o.p.y = y;
		pl.e.p.x = sx, pl.e.p.y = sy;
		if ( !always_thin )
		   {	fixed udx = sx - x, udy = sy - y;
			if ( !(udx | udy) )	/* degenerate */
			 { /* Only consider a degenerate segment */
			   /* if the entire subpath is degenerate and */
			   /* we are using round caps or joins. */
			   if ( index != 0 || (pline->next != 0 &&
				 pline->next->type != s_start) ||
				(lp->cap != gs_cap_round &&
				 lp->join != gs_join_round)
			      )
			     goto nd;
			   /* Pick an arbitrary orientation. */
			   udx = int2fixed(1);
			 }
			if ( uniform != 0 )
			   {	/* We can save a lot of work in this case. */
				float dpx = udx, dpy = udy;
 				float wl = line_width_and_scale * xx /
					hypot(dpx, dpy);
				if ( wl < 0 ) wl = -wl;
				pl.e.cdelta.x = (fixed)(dpx * wl);
				pl.e.cdelta.y = (fixed)(dpy * wl);
				pl.width.x = -pl.e.cdelta.y;
				pl.width.y = pl.e.cdelta.x;
				pl.thin = 0;	/* if not always_thin, */
						/* then never thin. */
			   }
			else
			   {	gs_point dpt;	/* unscaled */
				float wl;
				if ( skewed )
					gs_idtransform(pgs,
				          (float)udx, (float)udy, &dpt);
				else	/* shortcut */
					dpt.x = udx / xx,
					dpt.y = udy / yy;
				wl = line_width_and_scale /
					hypot(dpt.x, dpt.y);
				/* Construct the width vector in */
				/* user space, still unscaled. */
				dpt.x *= wl;
				dpt.y *= wl;
				/*
				 * We now compute both perpendicular
				 * and (optionally) parallel half-widths,
				 * as deltas in device space.  We use
				 * a fixed-point, unscaled version of
				 * gs_dtransform.  The second computation
				 * folds in a 90-degree rotation (in user
				 * space, before transforming) in the
				 * direction that corresponds to clockwise
				 * in device space.
				 */
				pl.e.cdelta.x = (fixed)(dpt.x * xx);
				pl.e.cdelta.y = (fixed)(dpt.y * yy);
				if ( skewed )
				  pl.e.cdelta.x += (fixed)(dpt.y * yx),
				  pl.e.cdelta.y += (fixed)(dpt.x * xy);
				if ( reflected )
				  dpt.x = -dpt.x, dpt.y = -dpt.y;
				pl.width.x = (fixed)(dpt.y * xx),
				pl.width.y = -(fixed)(dpt.x * yy);
				if ( skewed )
				  pl.width.x -= (fixed)(dpt.x * yx),
				  pl.width.y += (fixed)(dpt.y * xy);
				pl.thin =
				  any_abs(pl.width.x) + any_abs(pl.width.y) <
				    float2fixed(0.75);
			   }
			if ( !pl.thin ) compute_caps(&pl);
		   }
		else
			pl.e.cdelta.x = pl.e.cdelta.y = 0,
			pl.width.x = pl.width.y = 0,
			pl.thin = 1;
		if ( first++ == 0 ) pl_first = pl;
		if ( index++ ) (*line_proc)(index - 2, &pl_prev, &pl, pgs);
		pl_prev = pl;
		x = sx, y = sy;
nd:		pline = (line_segment *)(pline->next);
	    }
	   if ( index )
	    {	/* If closed, join back to start, else cap */
		(*line_proc)(index - 1, &pl_prev,
			     (psub->closed ? &pl_first : (pl_ptr)0), pgs);
	    }
	   psub = (subpath *)pline;
	   if ( stroke_path == &stroke_path_body )
	    {	/* Fill and release the accumulated path */
		gx_fill_path(stroke_path, pgs->dev_color, pgs,
			     gx_rule_winding_number, (fixed)0);
		gx_path_release(stroke_path);
		stroke_path = 0;
	    }
	 }
exit:	if ( dash_count ) gx_path_release(&dpath);
	if ( ppath->curve_count ) gx_path_release(&fpath);
	return code;
}

/* ------ Internal routines ------ */

/* Expand a dashed subpath into explicit segments. */
/* The subpath contains no curves. */
private int
expand_dashes(subpath *psub, gx_path *ppath, gs_state *pgs)
{	int skewed = is_skewed(&pgs->ctm);
	dash_params *dash = &pgs->line_params->dash;
	float *pattern = dash->pattern;
	int count, ink_on, index;
	float dist_left;
	fixed x0 = psub->pt.x, y0 = psub->pt.y;
	fixed x, y;
	segment *pseg;
	int wrap = (dash->init_ink_on && psub->closed ? -1 : 0);
	int drawing = wrap;
	int code;
	if ( (code = gx_path_add_point(ppath, x0, y0)) < 0 )
		return code;
	/* To do the right thing at the beginning of a closed path, */
	/* we have to skip any initial line, and then redo it at */
	/* the end of the path.  Drawing = -1 while skipping, */
	/* 0 while drawing normally, and 1 on the second round. */
top:	count = dash->pattern_size;
	ink_on = dash->init_ink_on;
	index = dash->init_index;
	dist_left = dash->init_dist_left;
	x = x0, y = y0;
	pseg = (segment *)psub;
	while ( (pseg = pseg->next) != 0 && pseg->type != s_start )
	   {	fixed sx = pseg->pt.x, sy = pseg->pt.y;
		fixed udx = sx - x, udy = sy - y;
		float length, dx, dy;
		float dist;
		if ( !(udx | udy) )	/* degenerate */
			dx = 0, dy = 0, length = 0;
		else
		   {	gs_point d;
			dx = udx, dy = udy;	/* scaled as fixed */
			if ( skewed )
				gs_idtransform(pgs, dx, dy, &d);
			else	/* shortcut */
				d.x = dx / pgs->ctm.xx,
				d.y = dy / pgs->ctm.yy;
			length = sqrt(d.x * d.x + d.y * d.y) *
				   (1 / (float)int2fixed(1));
		   }
		dist = length;
		while ( dist > dist_left )
		   {	/* We are using up the dash element */
			float fraction = dist_left / length;
			fixed nx = x + (fixed)(dx * fraction);
			fixed ny = y + (fixed)(dy * fraction);
			if ( ink_on )
			   {	if ( drawing >= 0 )
				  code = gx_path_add_line(ppath, nx, ny);
			   }
			else
			   {	if ( drawing > 0 ) return 0;	/* done */
				code = gx_path_add_point(ppath, nx, ny);
				drawing = 0;
			   }
			if ( code < 0 ) return code;
			dist -= dist_left;
			ink_on = !ink_on;
			if ( ++index == count ) index = 0;
			dist_left = pattern[index];
			x = nx, y = ny;
		   }
		dist_left -= dist;
		/* Handle the last dash of a segment. */
		if ( ink_on )
		   {	if ( drawing >= 0 )
			  code =
			    (pseg->type == s_line_close && drawing > 0 ?
			     gx_path_close_subpath(ppath) :
			     gx_path_add_line(ppath, sx, sy));
		   }
		else
		   {	if ( drawing > 0 ) return 0;	/* done */
			code = gx_path_add_point(ppath, sx, sy);
			drawing = 0;
		   }
		if ( code < 0 ) return code;
		x = sx, y = sy;
	   }
	/* Check for wraparound. */
	if ( wrap && drawing <= 0 )
	   {	/* We skipped some initial lines. */
		/* Go back and do them now. */
		drawing = 1;
		goto top;
	   }
	return 0;
}

/* Compute the intersection of two lines.  This is a messy algorithm */
/* that somehow ought to be useful in more places than just here.... */
private void
line_intersect(
    p_ptr pp1,				/* point on 1st line */
    p_ptr pd1,				/* slope of 1st line (dx,dy) */
    p_ptr pp2,				/* point on 2nd line */
    p_ptr pd2,				/* slope of 2nd line */
    p_ptr pi)				/* return intersection here */
{	/* We don't have to do any scaling, the factors all work out right. */
	float u1 = pd1->x, v1 = pd1->y;
	float u2 = pd2->x, v2 = pd2->y;
	double denom = u1 * v2 - u2 * v1;
	double num1 = v1 * pp1->x - u1 * pp1->y;
	double num2 = v2 * pp2->x - u2 * pp2->y;
	double xnum = u1 * num2 - u2 * num1;
	double ynum = v1 * num2 - v2 * num1;
	double max_result = any_abs(denom) * (double)max_fixed;
#ifdef DEBUG
if ( gs_debug['o'] )
   {	dprintf4("[o]Intersect %f,%f(%f/%f)",
		 fixed2float(pp1->x), fixed2float(pp1->y),
		 fixed2float(pd1->x), fixed2float(pd1->y));
	dprintf4(" & %f,%f(%f/%f),\n",
		 fixed2float(pp2->x), fixed2float(pp2->y),
		 fixed2float(pd2->x), fixed2float(pd2->y));
	dprintf4("\txnum=%f ynum=%f denom=%f max_result=%f ->\n",
		 xnum, ynum, denom, max_result);
   }
#endif
	/* Check for degenerate result. */
	if ( denom == 0 || any_abs(xnum) > max_result || any_abs(ynum) > max_result )
	   {	/* The lines are nearly parallel, */
		/* or one of them has zero length.  Punt. */
		*pi = *pp1;
#ifdef DEBUG
if ( gs_debug['o'] )
		dprintf("\tdegenerate!\n");
#endif
	   }
	else
	   {	pi->x = (fixed)(xnum / denom);
		pi->y = (fixed)(ynum / denom);
#ifdef DEBUG
if ( gs_debug['o'] )
	dprintf2("\t%f,%f\n", fixed2float(pi->x), fixed2float(pi->y));
#endif
	   }
}

#define lix plp->o.p.x
#define liy plp->o.p.y
#define litox plp->e.p.x
#define litoy plp->e.p.y

/* Draw a line on the device. */
private int
stroke_fill(int first, register pl_ptr plp, pl_ptr nplp, gs_state *pgs)
{	if ( plp->thin )
	   {	/* Minimum-width line, don't have to be careful. */
		/* We do have to check for the entire line being */
		/* within the clipping rectangle, allowing for some */
		/* slop at the ends. */
		fixed dx = litox - lix, dy = litoy - liy;
#define trsign(v, c) (v >= 0 ? c : -c)
#define slop int2fixed(2)
		fixed xslop = trsign(dx, slop);
		fixed yslop = trsign(dy, slop);
		if ( gx_cpath_includes_rectangle(pgs->clip_path,
				lix - xslop, liy - yslop,
				litox + xslop, litoy + yslop) )
			return gz_draw_line_fixed(lix, liy, litox, litoy,
				pgs->dev_color, pgs);
#undef slop
		/* We didn't set up the endpoint parameters before, */
		/* because the line was thin.  Do it now. */
		/* We only approximate the width and height. */
		if ( any_abs(dx) > any_abs(dy) )
		   {	plp->width.x = plp->e.cdelta.y = 0;
			plp->width.y = -(plp->e.cdelta.x =
				trsign(dx, float2fixed(-0.5)));
		   }
		else
		   {	plp->width.y = plp->e.cdelta.x = 0;
			plp->width.x = -(plp->e.cdelta.y =
				trsign(dy, float2fixed(-0.5)));
		   }
#undef trsign
		compute_caps(plp);
	   }
	   {	/* General case. */
		/* Construct a path and hand it to the fill algorithm. */
		if ( stroke_path == 0 )
		   {	/* We are rendering, and haven't run into the */
			/* general case yet.  Initialize the path. */
			stroke_path = &stroke_path_body;	/* set global for stroke_add */
			gx_path_init(stroke_path, &pgs->memory_procs);
		   }
		stroke_add(first, plp, nplp, pgs);
		   {	/****** PATCH ******/
			if ( stroke_path == &stroke_path_body )
			   {	gx_fill_path(stroke_path, pgs->dev_color, pgs,
					     gx_rule_winding_number, (fixed)0);
				gx_path_release(stroke_path);
				stroke_path = 0;
			   }
		   }
	   }
	return 0;
}

#undef lix
#undef liy
#undef litox
#undef litoy

/* Add a segment to the path.  This handles all the complex cases. */
private int add_capped(P4(gx_path *, gs_line_cap, int (*)(P3(gx_path *, fixed, fixed)), ep_ptr));
private int
stroke_add(int first, register pl_ptr plp, pl_ptr nplp, gs_state *pgs)
{	gx_path *ppath = stroke_path;
	int code;
	if ( ppath == 0 ) return 0;	/****** strokepath is NYI ******/
	if ( plp->thin )
	   {	/* We didn't set up the endpoint parameters before, */
		/* because the line was thin.  Do it now. */
		compute_caps(plp);
	   }
	if ( (code = add_capped(ppath, (first == 0 ? pgs->line_params->cap : gs_cap_butt), gx_path_add_point, &plp->o)) < 0 )
		return code;
	if ( nplp == 0 )
	   {	code = add_capped(ppath, pgs->line_params->cap, gx_path_add_line, &plp->e);
	   }
	else if ( pgs->line_params->join == gs_join_round )
	   {	code = add_capped(ppath, gs_cap_round, gx_path_add_line, &plp->e);
	   }
	else if ( nplp->thin )		/* no join */
	  {	code = add_capped(ppath, gs_cap_butt, gx_path_add_line, &plp->e);
	  }
	else				/* join_bevel or join_miter */
	   {	gs_fixed_point jp1, jp2;
		/*
		 * Set np to whichever of nplp->o.co or .ce
		 * is outside the current line.
		 * We use the interesting observation that
		 * point (x2,y2) is counter-clockwise from (x1,y1)
		 * relative to the origin iff x1*y2 < x2*y1.
		 * In this case x1,y1 are plp->width,
		 * x2,y2 are nplp->width, and the origin is
		 * their common point (plp->e.p, nplp->o.p).
		 */
		float wx1 = plp->width.x, wy1 = plp->width.y;
		float wx2 = nplp->width.x, wy2 = nplp->width.y;
		int ccw = wx1 * wy2 < wx2 * wy1;
		p_ptr outp, np, np1, np2;
		/* Initialize for a bevel join. */
		jp1.x = plp->e.co.x, jp1.y = plp->e.co.y;
		jp2.x = plp->e.ce.x, jp2.y = plp->e.ce.y;
		if ( ccw )
			outp = &jp2, np2 = np = &nplp->o.co, np1 = &plp->e.p;
		else
			outp = &jp1, np1 = np = &nplp->o.ce, np2 = &plp->e.p;
#ifdef DEBUG
if ( gs_debug['o'] )
		dprintf1("[o]use %s\n", (ccw ? "co (ccw)" : "ce (cw)"));
#endif
		/* Don't bother with the miter check if the two */
		/* points to be joined are very close together, */
		/* namely, in the same square half-pixel. */
		if ( pgs->line_params->join == gs_join_miter &&
		     !(fixed2long(outp->x << 1) == fixed2long(np->x << 1) &&
		       fixed2long(outp->y << 1) == fixed2long(np->y << 1))
		   )
		  { /*
		     * Check whether a miter join is appropriate.
		     * Let a, b be the angles of the two lines.
		     * We check tan(a-b) against the miter_check
		     * by using the following formula:
		     * If tan(a)=u1/v1 and tan(b)=u2/v2, then
		     * tan(a-b) = (u1*v2 - u2*v1) / (u1*u2 + v1*v2).
		     * We can do all the computations unscaled,
		     * because we're only concerned with ratios.
		     */
		    float u1 = plp->e.cdelta.x, v1 = plp->e.cdelta.y;
		    float u2 = nplp->o.cdelta.x, v2 = nplp->o.cdelta.y;
		    float num = u1 * v2 - u2 * v1;
		    float denom = u1 * u2 + v1 * v2;
		    float check = pgs->line_params->miter_check;
		    /*
		     * We will want either tan(a-b) or tan(b-a)
		     * depending on the orientations of the lines.
		     * Fortunately we know the relative orientations already.
		     */
		    if ( !ccw )		/* have plp - nplp, want vice versa */
			num = -num;
#ifdef DEBUG
if ( gs_debug['o'] )
                   {    dprintf4("[o]Miter check: u1/v1=%f/%f, u2/v2=%f/%f,\n",
				 u1, v1, u2, v2);
                        dprintf3("        num=%f, denom=%f, check=%f\n",
				 num, denom, check);
                   }
#endif
		    /* Use a miter if num / denom >= check. */
		    /* If check > 0, num < 0 always passes; */
		    /* if check < 0, num >= 0 always fails. */
		    if ( denom < 0 ) num = -num, denom = -denom;
		    if ( check > 0 ?
			(num < 0 || num >= denom * check) :
			(num < 0 && num >= denom * check)
		       )
			   {	/* OK to use a miter join. */
#ifdef DEBUG
if ( gs_debug['o'] )
				dputs("        ... passes.\n");
#endif
				/* Compute the intersection of */
				/* the extended edge lines. */
				line_intersect(outp, &plp->e.cdelta, np,
					       &nplp->o.cdelta, outp);
			   }
		   }
		if ( (code = gx_path_add_line(ppath, jp1.x, jp1.y)) < 0 ||
		     (code = gx_path_add_line(ppath, np1->x, np1->y)) < 0 ||
		     (code = gx_path_add_line(ppath, np2->x, np2->y)) < 0 ||
		     (code = gx_path_add_line(ppath, jp2.x, jp2.y)) < 0
		   )
			return code;
	   }
	if ( code < 0 || (code = gx_path_close_subpath(ppath)) < 0 )
		return code;
	return 0;
}

/* Routines for cap computations */

/* Compute the endpoints of the two caps of a segment. */
private void
compute_caps(register pl_ptr plp)
{	fixed wx2 = plp->width.x;
	fixed wy2 = plp->width.y;
	plp->o.co.x = plp->o.p.x + wx2, plp->o.co.y = plp->o.p.y + wy2;
	plp->o.cdelta.x = -plp->e.cdelta.x,
	  plp->o.cdelta.y = -plp->e.cdelta.y;
	plp->o.ce.x = plp->o.p.x - wx2, plp->o.ce.y = plp->o.p.y - wy2;
	plp->e.co.x = plp->e.p.x - wx2, plp->e.co.y = plp->e.p.y - wy2;
	plp->e.ce.x = plp->e.p.x + wx2, plp->e.ce.y = plp->e.p.y + wy2;
#ifdef DEBUG
if ( gs_debug['o'] )
	dprintf4("[o]Stroke o=(%f,%f) e=(%f,%f)\n",
		 fixed2float(plp->o.p.x), fixed2float(plp->o.p.y),
		 fixed2float(plp->e.p.x), fixed2float(plp->e.p.y)),
	dprintf4("\twxy=(%f,%f) lxy=(%f,%f)\n",
		 fixed2float(wx2), fixed2float(wy2),
		 fixed2float(plp->e.cdelta.x), fixed2float(plp->e.cdelta.y));
#endif
}

/* Add a properly capped line endpoint to the path. */
/* The first point may require either moveto or lineto. */
private int
add_capped(gx_path *ppath, gs_line_cap type,
  int (*add_proc)(P3(gx_path *, fixed, fixed)), /* gx_path_add_point/line */
  register ep_ptr endp)
{	int code;
#define px endp->p.x
#define py endp->p.y
#define xo endp->co.x
#define yo endp->co.y
#define xe endp->ce.x
#define ye endp->ce.y
#define cdx endp->cdelta.x
#define cdy endp->cdelta.y
#ifdef DEBUG
if ( gs_debug['o'] )
	dprintf4("[o]cap: p=(%g,%g), co=(%g,%g),\n",
		 fixed2float(px), fixed2float(py),
		 fixed2float(xo), fixed2float(yo)),
	dprintf4("[o]\tce=(%g,%g), cd=(%g,%g)\n",
		 fixed2float(xe), fixed2float(ye),
		 fixed2float(cdx), fixed2float(cdy));
#endif
	switch ( type )
	   {
	case gs_cap_round:
	   {	fixed xm = px + cdx;
		fixed ym = py + cdy;
		if (	(code = (*add_proc)(ppath, xo, yo)) < 0 ||
			(code = gx_path_add_arc(ppath, xo, yo, xm, ym,
				xo + cdx, yo + cdy, quarter_arc_fraction)) < 0 ||
			(code = gx_path_add_arc(ppath, xm, ym, xe, ye,
				xe + cdx, ye + cdy, quarter_arc_fraction)) < 0
		   ) return code;
	   }
		break;
	case gs_cap_square:
		if (	(code = (*add_proc)(ppath, xo + cdx, yo + cdy)) < 0 ||
			(code = gx_path_add_line(ppath, xe + cdx, ye + cdy)) < 0
		   ) return code;
		break;
	case gs_cap_butt:
		if (	(code = (*add_proc)(ppath, xo, yo)) < 0 ||
			(code = gx_path_add_line(ppath, xe, ye)) < 0
		   ) return code;
	   }
	return code;
}
