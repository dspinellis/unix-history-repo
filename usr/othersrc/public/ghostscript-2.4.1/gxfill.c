/* Copyright (C) 1989, 1990, 1991 Aladdin Enterprises.  All rights reserved.
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

/* gxfill.c */
/* Lower-level path filling procedures for GhostScript library */
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gxdevice.h"			/* for gx_color_index */
#include "gzcolor.h"
#include "gzpath.h"
#include "gzstate.h"
#include "gxcpath.h"

/* Import the fixed * fixed / fixed routine from gxdraw.c. */
/* The second argument must be less than or equal to the third; */
/* all must be non-negative, and the last must be non-zero. */
extern fixed fixed_mult_quo(P3(fixed, fixed, fixed));

/* Define the structure for keeping track of active lines. */
typedef struct active_line_s active_line;
struct active_line_s {
	gs_fixed_point start;		/* x,y where line starts */
	gs_fixed_point end;		/* x,y where line ends */
#define al_dx(alp) ((alp)->end.x - (alp)->start.x)
#define al_dy(alp) ((alp)->end.y - (alp)->start.y)
	fixed y_fast_max;		/* can do x_at_y in fixed point */
					/* if y <= y_fast_max */
#define set_al_points(alp, startp, endp)\
  (alp)->y_fast_max = max_fixed / (((endp).x > (startp).x ?\
    (endp).x - (startp).x : (startp).x - (endp).x) | 1) + (startp).y,\
  (alp)->start = startp, (alp)->end = endp
#define al_x_at_y(alp, yv)\
  ((yv) == (alp)->end.y ? (alp)->end.x :\
   ((yv) <= (alp)->y_fast_max ?\
    ((yv) - (alp)->start.y) * al_dx(alp) / al_dy(alp) :\
    (stat(n_slow_x),\
     fixed_mult_quo(al_dx(alp), (yv) - (alp)->start.y, al_dy(alp)))) +\
   (alp)->start.x)
	fixed x_current;		/* current x position */
	fixed x_next;			/* x position at end of band */
	segment *pseg;			/* endpoint of this line */
	int direction;			/* direction of line segment */
#define dir_up 1
#define dir_down (-1)
/* "Pending" lines (not reached in the Y ordering yet) use next and prev */
/* to order lines by increasing starting Y.  "Active" lines (being scanned) */
/* use next and prev to order lines by increasing current X, or if the */
/* current Xs are equal, by increasing final X. */
	active_line *prev, *next;
/* Link together active_lines allocated individually */
	active_line *alloc_next;
};

/* Define the ordering criterion for active lines. */
/* The xc argument is a copy of lp2->x_current. */
#define x_precedes(lp1, lp2, xc)\
  (lp1->x_current < xc || lp1->x_current == xc &&\
   (lp1->start.x > lp2->start.x || lp1->end.x < lp2->end.x))

#ifdef DEBUG
/* Internal procedures for printing active lines */
private void
print_active_line(char *label, active_line *alp)
{	dprintf5("[f]%s %lx(%d): x_current=%f x_next=%f\n",
	         label, (ulong)alp, alp->direction,
	         fixed2float(alp->x_current), fixed2float(alp->x_next));
	dprintf5("    start=(%f,%f) pt_end=%lx(%f,%f)\n",
	         fixed2float(alp->start.x), fixed2float(alp->start.y),
	         (ulong)alp->pseg,
	         fixed2float(alp->end.x), fixed2float(alp->end.y));
	dprintf2("    prev=%lx next=%lx\n",
		 (ulong)alp->prev, (ulong)alp->next);
}
private void
print_line_list(active_line *flp)
{	active_line *lp;
	for ( lp = flp; lp != 0; lp = lp->next )
	   {	fixed xc = lp->x_current, xn = lp->x_next;
		dprintf3("[f]%lx(%d): x_current/next=%g",
		         (ulong)lp, lp->direction,
		         fixed2float(xc));
		if ( xn != xc ) dprintf1("/%g", fixed2float(xn));
		dputc('\n');
	   }
}
#define print_al(label,alp)\
  if ( gs_debug['F'] ) print_active_line(label, alp)
#else
#define print_al(label,alp) 0
#endif

/* Line list structure */
struct line_list_s {
	active_line *active_area;	/* allocated active_line list */
	line_close_segment *close_area;	/* allocated closing line area */
	uint close_count;
	gs_fixed_rect box;		/* intersection of bounding boxes, */
					/* disregard lines outside this */
	active_line *next_active;	/* next allocation slot */
	active_line *limit;		/* limit of local allocation */
	line_close_segment *next_line;	/* next line allocation slot */
	active_line *y_list;		/* Y-sorted list of pending lines */
	active_line *y_line;		/* most recently inserted line */
	active_line x_head;		/* X-sorted list of active lines */
#define x_list x_head.next
		/* Put the arrays last so the scalars will have */
		/* small displacements. */
		/* Allocate a few active_lines and line_close_segments */
		/* locally to avoid round trips through the allocator. */
#define max_local_active 20
	active_line local_active[max_local_active];
#define max_local_close 5
	line_close_segment local_close[max_local_close];
};
typedef struct line_list_s line_list;
typedef line_list _ss *ll_ptr;

/* Forward declarations */
private int alloc_line_list(P2(ll_ptr, uint));
private void free_line_list(P1(ll_ptr));
private int add_y_list(P2(gx_path *, ll_ptr));
private int add_y_line(P4(segment *, segment *, int, ll_ptr));
private int fill_loop(P5(gx_device_color *, int, ll_ptr,
  gs_state *, fixed));
private void insert_x_new(P2(active_line *, ll_ptr));
private void update_x_list(P2(active_line *, fixed));

/* Statistics */
#ifdef DEBUG
#define stat(x) (x++)
#define statn(x,n) (x += (n))
private long n_fill;
private long n_fill_alloc;
private long n_y_up;
private long n_y_down;
private long n_x_step;
private long n_slow_x;
private long n_iter;
private long n_find_y;
private long n_band;
private long n_band_step;
private long n_band_fill;
#else
#define stat(x) 0
#define statn(x,n) 0
#endif

/* General area filling algorithm. */
/* The adjust parameter is a hack for keeping small characters */
/* from coming out too faint: it specifies an amount by which to expand */
/* all sides of every filled region. */
int
gx_fill_path(gx_path *ppath, gx_device_color *pdevc, gs_state *pgs,
  int rule, fixed adjust)
{	gx_clip_path *pcpath = pgs->clip_path;
	gs_fixed_rect cbox;
	gx_path *pfpath;
	gx_path ffpath;
	int code;
	line_list lst;
	uint sub_count;
	gx_device_clip cdev;
	int do_clip;
	/* Fatten everything a little to make it look better. */
	/****** This is something of a hack. ******/
	if ( adjust == 0 ) adjust = float2fixed(0.25);
	/* Start by flattening the path.  We should do this on-the-fly.... */
	if ( !ppath->curve_count )	/* don't need to flatten */
		pfpath = ppath;
	else
	   {	if ( (code = gx_path_flatten(ppath, &ffpath, pgs->flatness)) < 0 ) return code;
		pfpath = &ffpath;
	   }
	/* Check the bounding boxes. */
#define ibox lst.box
	gx_path_bbox(pfpath, &ibox);
	gx_cpath_box_for_check(pcpath, &cbox);
	if ( ibox.q.y <= cbox.q.y && ibox.q.x <= cbox.q.x &&
	     ibox.p.y >= cbox.p.y && ibox.p.x >= cbox.p.x
	   )
	   {	/* Path lies entirely within clipping rectangle */
		do_clip = 0;
	   }
	else
	   {	/* Intersect the path box and the clip bounding box. */
		/* If the intersection is empty, this fill is a no-op. */
		gs_fixed_rect bbox;
		bbox = pcpath->path.bbox;
		if ( ibox.p.x >= bbox.q.x || ibox.p.y >= bbox.q.y ||
		    ibox.q.x <= bbox.p.x || ibox.q.y <= bbox.p.y
		   )
		   {	/* Intersection of boxes is empty! */
			code = 0;
			goto skip;
		   }
		do_clip = 1;
	   }
#undef ibox
	sub_count = pfpath->subpath_count;
	if ( !(code = alloc_line_list(&lst, sub_count)) )
	   {	gx_device *save_dev;
		if ( (code = add_y_list(pfpath, &lst)) < 0 )
			goto nope;
		if ( do_clip )
		   {	/* Set up a clipping device. */
			gx_device *dev = (gx_device *)&cdev;
			save_dev = gs_currentdevice(pgs);
			cdev = gs_clip_device;
			cdev.target = save_dev;
			cdev.list = pcpath->list;
			gx_set_device_only(pgs, dev);
			(*dev->procs->open_device)(dev);
		   }
		code = fill_loop(pdevc, rule, &lst, pgs, adjust);
		if ( do_clip )
			gx_set_device_only(pgs, save_dev);
nope:		free_line_list(&lst);
	   }
skip:	if ( pfpath != ppath )	/* had to flatten */
		gx_path_release(pfpath);
#ifdef DEBUG
if ( gs_debug['f'] || gs_debug['F'] )
	   {	dputs("[f]calls alloc   up   down  step slowx  iter  find  band bstep bfill\n");
		dprintf4("   %5ld %5ld %5ld %5ld",
			n_fill, n_fill_alloc, n_y_up, n_y_down);
		dprintf4(" %5ld %5ld %5ld %5ld",
			n_x_step, n_slow_x, n_iter, n_find_y);
		dprintf3(" %5ld %5ld %5ld\n",
			n_band, n_band_step, n_band_fill);
	   }
#endif
	return code;
}

/* Create a line list for a (flattened) path. */
/* We pass in the list size, so that we can use this to iterate */
/* over more than one path simultaneously (needed for clipping). */
private int
alloc_line_list(ll_ptr ll, uint sub_count)
{	ll->active_area = 0;
	ll->close_count = sub_count;
	ll->close_area =
	  (sub_count <= max_local_close ?
	   ll->local_close :
	   (line_close_segment *)gs_malloc(sub_count, sizeof(line_close_segment),
					   "closing lines"));
	ll->next_line = ll->close_area;
	if ( ll->close_area == 0 )
		return_error(gs_error_VMerror);
	ll->next_active = ll->local_active;
	ll->limit = ll->next_active + max_local_active;
	ll->y_list = 0;
	ll->y_line = 0;
	stat(n_fill);
	return 0;
}

/* Free the line list */
private void
free_line_list(ll_ptr ll)
{	line_close_segment *lp;
	active_line *alp;
	/* Splice out any inserted closing lines */
	for ( lp = ll->close_area; lp != ll->next_line; lp++ )
	   {	segment *prev = lp->prev, *next = lp->next;
		prev->next = next;
		if ( next ) next->prev = prev;
		lp->sub->last = prev;
	   }
	/* Free any individually allocated active_lines. */
	while ( (alp = ll->active_area) != 0 )
	   {	active_line *next = alp->alloc_next;
		gs_free((char *)alp, 1, sizeof(active_line),
			"active line");
		ll->active_area = next;
	   }
	/* Free any separately allocated closing lines. */
	if ( ll->close_area != ll->local_close && ll->close_area != 0 )
	   {	gs_free((char *)ll->close_area, ll->close_count,
			sizeof(line_close_segment), "closing lines");
	   }
}

/* Construct a Y-sorted list of lines for a (flattened) path. */
/* We assume the path is non-empty.  Only include non-horizontal */
/* lines where one endpoint is locally Y-minimal. */
private int
add_y_list(gx_path *ppath, ll_ptr ll)
{	register segment *pseg = (segment *)ppath->first_subpath;
	subpath *psub;
	segment *plast;
	int first_dir, prev_dir, dir;
	segment *prev;
	/* fixed xmin = ll->box.p.x; */	/* not currently used */
	fixed ymin = ll->box.p.y;
	fixed xmax = ll->box.q.x;
	fixed ymax = ll->box.q.y;
	int code;

	while ( pseg )
	   {	switch ( pseg->type )
		   {	/* No curves left */
		case s_start:
			psub = (subpath *)pseg;
			plast = psub->last;
			dir = 2;	/* hack to skip first line */
			if ( plast->type != s_line_close )
			   {	/* Create a fake s_line_close */
				line_close_segment *lp = ll->next_line++;
				segment *next = plast->next;
				lp->next = next;
				lp->prev = plast;
				plast->next = (segment *)lp;
				if ( next ) next->prev = (segment *)lp;
				lp->type = s_line_close;
				lp->pt = psub->pt;
				lp->sub = psub;
				plast = (segment *)lp;
				psub->last = plast;
			   }
			break;
		default:		/* s_line, _close */
		   {	fixed iy = pseg->pt.y;
			fixed py = prev->pt.y;
			/* Lines falling entirely outside the ibox */
			/* are treated as though they were horizontal, */
			/* i.e., they are never put on the list. */
#define compute_dir(xo, xe, yo, ye)\
  (xo > xmax && xe > xmax ? 0 :\
   ye > yo ? (ye <= ymin || yo >= ymax ? 0 : dir_up) :\
   ye < yo ? (yo <= ymin || ye >= ymax ? 0 : dir_down) :\
   0)
#define add_dir_lines(prev2, prev, this, pdir, dir)\
  if ( pdir )\
   { if ( (code = add_y_line(prev2, prev, pdir, ll)) < 0 ) return code; }\
  if ( dir )\
   { if ( (code = add_y_line(prev, this, dir, ll)) < 0 ) return code; }
			dir = compute_dir(prev->pt.x, pseg->pt.x, py, iy);
			if ( dir > prev_dir )
			   {	add_dir_lines(prev->prev, prev, pseg, prev_dir, dir);
			   }
			else if ( prev_dir == 2 )	/* first line */
				first_dir = dir;
			if ( pseg == plast )
			   {	/* We skipped the first segment of the */
				/* subpath, so the last segment must */
				/* receive special consideration. */
				/* Note that we have `closed' all subpaths. */
				if ( first_dir > dir )
				   {	add_dir_lines(prev, pseg, psub->next, dir, first_dir);
				   }
			   }
		   }
#undef compute_dir
#undef add_dir_lines
		   }
		prev = pseg;
		prev_dir = dir;
		pseg = pseg->next;
	   }
	return 0;
}
/* Internal routine to test a line segment and add it to the */
/* pending list if appropriate. */
private int
add_y_line(segment *prev_lp, segment *lp, int dir, ll_ptr ll)
{	gs_fixed_point this, prev;
	register active_line *alp = ll->next_active;
	fixed y_start;
	if ( alp == ll->limit )
	   {	/* Allocate separately */
		alp = (active_line *)gs_malloc(1, sizeof(active_line), "active line");
		if ( alp == 0 ) return_error(gs_error_VMerror);
		alp->alloc_next = ll->active_area;
		ll->active_area = alp;
		stat(n_fill_alloc);
	   }
	else
		ll->next_active++;
	this.x = lp->pt.x;
	this.y = lp->pt.y;
	prev.x = prev_lp->pt.x;
	prev.y = prev_lp->pt.y;
	if ( (alp->direction = dir) > 0 )
	   {	/* Upward line */
		y_start = prev.y;
		set_al_points(alp, prev, this);
		alp->pseg = lp;
	   }
	else
	   {	/* Downward line */
		y_start = this.y;
		set_al_points(alp, this, prev);
		alp->pseg = prev_lp;
	   }
	/* Insert the new line in the Y ordering */
	   {	register active_line *yp = ll->y_line;
		register active_line *nyp;
		if ( yp == 0 )
		   {	alp->next = alp->prev = 0;
			ll->y_list = alp;
		   }
		else if ( y_start >= yp->start.y )
		   {	/* Insert the new line after y_line */
			while ( stat(n_y_up), (nyp = yp->next) != NULL && y_start > nyp->start.y )
				yp = nyp;
			alp->next = nyp;
			alp->prev = yp;
			yp->next = alp;
			if ( nyp ) nyp->prev = alp;
		   }
		else
		   {	/* Insert the new line before y_line */
			while ( stat(n_y_down), (nyp = yp->prev) != NULL && y_start < nyp->start.y )
				yp = nyp;
			alp->prev = nyp;
			alp->next = yp;
			yp->prev = alp;
			if ( nyp ) nyp->next = alp;
			else ll->y_list = alp;
		   }
	   }
	ll->y_line = alp;
	print_al("add ", alp);
	return 0;
}

/* Main filling loop.  Takes lines off of y_list and adds them to */
/* x_list as needed. */
private int
fill_loop(gx_device_color *pdevc, int rule, ll_ptr ll,
  gs_state *pgs, fixed adjust)
{	fixed adj2 = adjust << 1;
	active_line *yll = ll->y_list;
	gs_fixed_point pmax;
	fixed y;
	if ( yll == 0 ) return 0;		/* empty list */
	pmax = ll->box.q;
	y = yll->start.y;			/* first Y value */
	ll->x_list = 0;
	ll->x_head.x_current = min_fixed;	/* stop backward scan */
	while ( 1 )
	   {	fixed y1, y0;
		active_line *endp, *alp, *stopx;
		fixed x;
		int draw;
		stat(n_iter);
		/* Check whether we've reached the maximum y. */
		if ( y >= pmax.y ) break;
		/* Move newly active lines from y to x list. */
		while ( yll != 0 && yll->start.y == y )
		   {	active_line *ynext = yll->next;	/* insert smashes next/prev links */
			insert_x_new(yll, ll);
			yll = ynext;
		   }
		if ( ll->x_list == 0 )
		   {	/* No active lines, skip to next start */
			if ( yll == 0 ) break;	/* no lines left */
			y = yll->start.y;
			continue;
		   }
		/* Find the next evaluation point. */
		/* Start by finding the smallest y value */
		/* at which any currently active line ends */
		/* (or the next to-be-active line begins). */
		y1 = (yll != 0 ? yll->start.y : max_fixed);
		for ( alp = ll->x_list; alp != 0; alp = alp->next )
		  if ( alp->end.y < y1 ) y1 = alp->end.y;
#ifdef DEBUG
if ( gs_debug['F'] )
   {		dprintf2("[f]before loop: y=%f y1=%f:\n",
		         fixed2float(y), fixed2float(y1));
		print_line_list(ll->x_list);
   }
#endif
		/* Now look for line intersections before y1. */
		x = min_fixed;
		y0 = y - adjust;
#define have_pixels() (fixed_rounded(y0) < fixed_rounded(y1 + adjust))
		draw = (have_pixels() ? 1 : -1);
		/*
		 * Loop invariants:
		 *	alp = endp->next;
		 *	for all lines lp from stopx up to alp,
		 *	  lp->x_next = al_x_at_y(lp, y1).
		 */
		for ( alp = stopx = ll->x_list; stat(n_find_y), alp != 0;
		     endp = alp, alp = alp->next
		    )
		   {	fixed nx = al_x_at_y(alp, y1);
			fixed dx_old, dx_den;
			/* Check for intersecting lines. */
			if ( nx >= x )
				x = nx;
			else if
			   ( draw >= 0 && /* don't bother if no pixels */
			     (dx_old = alp->x_current - endp->x_current) >= 0 &&
			     (dx_den = dx_old + endp->x_next - nx) > dx_old
			   )
			   {	/* Make a good guess at the intersection */
				/* Y value using only local information. */
				fixed dy = y1 - y, y_new;
#ifdef DEBUG
if ( gs_debug['f'] || gs_debug['F'] )
				dprintf3("[f]cross: dy=%g, dx_old=%g, dx_new=%g\n",
				  fixed2float(dy), fixed2float(dx_old),
				  fixed2float(dx_den - dx_old));
#endif
				/* Do the computation in single precision */
				/* if the values are small enough. */
				y_new =
				  ((dy | dx_old) < 1L << (sizeof(fixed)*4-1) ?
				   dy * dx_old / dx_den :
				   fixed_mult_quo(dy, dx_old, dx_den))
				  + y;
				/* The crossing value doesn't have to be */
				/* very accurate, but it does have to be */
				/* greater than y and less than y1. */
#ifdef DEBUG
if ( gs_debug['f'] || gs_debug['F'] )
				dprintf3("[f]cross y=%g, y_new=%g, y1=%g\n",
				  fixed2float(y), fixed2float(y_new),
				  fixed2float(y1));
#endif
				stopx = alp;
				if ( y_new <= y ) y_new = y + 1;
				if ( y_new < y1 )
				   {	y1 = y_new;
					nx = al_x_at_y(alp, y1);
					draw = 0;
				   }
				if ( nx > x ) x = nx;
			   }
			alp->x_next = nx;
		   }
		/* Recompute next_x for lines before the intersection. */
		for ( alp = ll->x_list; alp != stopx; alp = alp->next )
			alp->x_next = al_x_at_y(alp, y1);
#ifdef DEBUG
if ( gs_debug['F'] )
   {		dprintf1("[f]after loop: y1=%f\n", fixed2float(y1));
		print_line_list(ll->x_list);
   }
#endif
		/* Fill a multi-trapezoid band for the active lines. */
		/* Don't bother if no pixel centers lie within the band. */
		if ( draw > 0 || draw == 0 && have_pixels() )
		   {	active_line *alp = ll->x_list;
			fixed height = y1 - y + adj2;
			fixed xlbot, xltop;	/* as of last "outside" line */
			int inside = 0;
			stat(n_band);
			x = min_fixed;
			/* rule = -1 for winding number rule, i.e. */
			/* we are inside if the winding number is non-zero; */
			/* rule = 1 for even-odd rule, i.e. */
			/* we are inside if the winding number is odd. */
			/* Clever, eh? */
#define inside_path_p() (inside & rule)
			while ( alp != 0 )
			   {	fixed xbot = alp->x_current;
				fixed xtop = alp->x_next;
				print_al("step", alp);
				stat(n_band_step);
				if ( inside_path_p() )
				 { inside += alp->direction;
				   if ( !inside_path_p() )	/* about to go out */
				    {	fixed wbot = xbot - xlbot + adj2;
					fixed wtop = xtop - xltop + adj2;
					int code;
					stat(n_band_fill);
					/* If lines are temporarily out of */
					/* order, wtop might be negative. */
					/* Patch this up now. */
					if ( wtop < 0 )
					   {	xltop += wtop >> 1;
						wtop = 0;
					   }
					code = gz_fill_trapezoid_fixed(
					     xlbot - adjust,
					     wbot, y0,
					     xltop - adjust, wtop,
					     height, 0, pdevc, pgs);
					if ( code < 0 ) return code;
				    }
				 }
				else			/* outside */
				   {	inside += alp->direction;
					if ( inside_path_p() )	/* about to go in */
						xlbot = xbot, xltop = xtop;
				   }
				alp = alp->next;
			   }
		   }
		update_x_list(ll->x_list, y1);
		y = y1;
	   }
	return 0;
}

/* Insert a newly active line in the X ordering. */
private void
insert_x_new(active_line *alp, ll_ptr ll)
{	register active_line *next;
	register active_line *prev = &ll->x_head;
	register fixed x = alp->start.x;
	alp->x_current = x;
	while ( stat(n_x_step),
		(next = prev->next) != 0 && x_precedes(next, alp, x)
	       )
		prev = next;
	alp->next = next;
	alp->prev = prev;
	if ( next != 0 ) next->prev = alp;
	prev->next = alp;
}

/* Clean up after a pass through the main loop. */
/* If any lines are out of order, re-sort them now. */
/* Also drop any ended lines. */
private void
update_x_list(active_line *x_first, fixed y1)
{	fixed x;
	register active_line *alp;
	active_line *nlp;
	for ( x = min_fixed, alp = x_first; alp != 0; alp = nlp )
	   {	fixed nx = alp->x_current = alp->x_next;
		nlp = alp->next;
#ifdef DEBUG
if ( gs_debug['f'] || gs_debug['F'] )
		dprintf4("[f]check %lx,x=%g %lx,x=%g\n",
		  (ulong)alp->prev, fixed2float(x),
		  (ulong)alp, fixed2float(nx));
#endif
		if ( alp->end.y == y1 )
		   {	/* Handle a line segment that just ended. */
			segment *pseg = alp->pseg;
			segment *next;
			gs_fixed_point npt;
			/*
			 * The computation of next relies on the fact that
			 * all subpaths have been closed.  When we cycle
			 * around to the other end of a subpath, we must be
			 * sure not to process the start/end point twice.
			 */
			next =
			  (alp->direction == dir_up ?
			   (/* Upward line, go forward along path. */
			    pseg->type == s_line_close ? /* end of subpath */
			     ((line_close_segment *)pseg)->sub->next :
			     pseg->next) :
			   (/* Downward line, go backward along path. */
			    pseg->type == s_start ? /* start of subpath */
			     ((subpath *)pseg)->last->prev :
			     pseg->prev)
			  );
			npt.y = next->pt.y;
#ifdef DEBUG
if ( gs_debug['F'] )
			dprintf5("[f]ended %lx: pseg=%lx y=%f next=%lx npt.y=%f\n",
				 (ulong)alp, (ulong)pseg, fixed2float(pseg->pt.y),
				 (ulong)next, fixed2float(npt.y));
#endif
			if ( npt.y <= pseg->pt.y )
			   {	/* End of a line sequence */
				alp->prev->next = nlp;
				if ( nlp ) nlp->prev = alp->prev;
#ifdef DEBUG
if ( gs_debug['F'] )
				dprintf1("[f]drop %lx\n", (ulong)alp);
#endif
				continue;
			   }
			else
			   {	alp->pseg = next;
				npt.x = next->pt.x;
				set_al_points(alp, alp->end, npt);
				print_al("repl", alp);
			   }
		   }
		if ( nx <= x )
		   {	/* Move alp backward in the list. */
			active_line *prev = alp->prev;
			active_line *next = nlp;
			prev->next = next;
			if ( next ) next->prev = prev;
			while ( !x_precedes(prev, alp, nx) )
			   {
#ifdef DEBUG
if ( gs_debug['f'] || gs_debug['F'] )
				dprintf2("[f]swap %lx,%lx\n",
				         (ulong)alp, (ulong)prev);
#endif
				next = prev, prev = prev->prev;
			   }
			alp->next = next;
			alp->prev = prev;
			/* next might be null, if alp was in */
			/* the correct spot already. */
			if ( next ) next->prev = alp;
			prev->next = alp;
		   }
		else
			x = nx;
	   }
#ifdef DEBUG
if ( gs_debug['f'] || gs_debug['F'] )
	for ( alp = x_first; alp != 0; alp = alp->next )
	 if ( alp->next != 0 && alp->next->x_current < alp->x_current )
	   {	lprintf("[f]Lines out of order!\n");
		print_active_line("   1:", alp);
		print_active_line("   2:", alp->next);
		gs_exit(1);
	   }
#endif
}
