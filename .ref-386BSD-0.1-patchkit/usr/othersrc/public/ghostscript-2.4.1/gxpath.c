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

/* gxpath.c */
/* Private path routines for Ghostscript library */
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gzpath.h"

/* These routines all assume that all points are */
/* already in device coordinates, and in fixed representation. */
/* As usual, they return either 0 or a (negative) error code. */

/* Forward references */
private subpath *path_alloc_copy(P1(gx_path *));
private int gx_path_new_subpath(P1(gx_path *));
#ifdef DEBUG
void gx_print_segment(P1(segment *));
#endif

/* ------ Initialize/free paths ------ */

/* Initialize a path */
void
gx_path_init(register gx_path *ppath, gs_memory_procs *pprocs)
{	ppath->memory_procs = *pprocs;
	ppath->box_last = 0;
	ppath->position_valid = 0;
	ppath->first_subpath = ppath->current_subpath = 0;
	ppath->subpath_count = 0;
	ppath->curve_count = 0;
	ppath->subpath_open = 0;
	ppath->shares_segments = 0;
}

/* Release the contents of a path.  We do this in reverse order */
/* so as to maximize LIFO allocator behavior. */
void
gx_path_release(gx_path *ppath)
{	segment *pseg;
	if ( ppath->first_subpath == 0 ) return;	/* empty path */
	if ( ppath->shares_segments ) return;	/* segments are shared */
	pseg = (segment *)ppath->current_subpath->last;
	while ( pseg )
	   {	segment *prev = pseg->prev;
		static uint sizes[] = { segment_type_sizes };
#ifdef DEBUG
if ( gs_debug['p'] )
		dprintf("[p]release"), gx_print_segment(pseg);
#endif
		(*ppath->memory_procs.free)((char *)pseg, 1, sizes[(int)pseg->type], "gx_path_release");
		pseg = prev;
	   }
	ppath->first_subpath = 0;	/* prevent re-release */
}

/* Mark a path as shared */
void
gx_path_share(gx_path *ppath)
{	if ( ppath->first_subpath ) ppath->shares_segments = 1;
}

/* ------ Incremental path building ------ */

/* Macro for opening the current subpath. */
/* ppath points to the path; psub has been set to ppath->current_subpath. */
#define path_open()\
	if ( !ppath->subpath_open )\
	   {	int code;\
		if ( !ppath->position_valid )\
		  return_error(gs_error_nocurrentpoint);\
		code = gx_path_new_subpath(ppath);\
		if ( code < 0 ) return code;\
		psub = ppath->current_subpath;\
	   }

/* Macros for allocating path segments. */
/* Note that they assume that ppath points to the path, */
/* and that psub points to the current subpath. */
/* We have to split the macro into two because of limitations */
/* on the size of a single statement (sigh). */
#ifdef DEBUG
#define p_alloc(pseg,size)\
  if ( gs_debug['A'] ) dprintf2("[p]%lx<%u>\n", (ulong)pseg, size)
#else
#define p_alloc(pseg,size) 0
#endif
#define path_unshare()\
  if(ppath->shares_segments)\
    if(!(psub = path_alloc_copy(ppath)))return_error(gs_error_limitcheck)
#define path_alloc_segment(pseg,ctype,stype,cname)\
  path_unshare();\
  if( !(pseg = (ctype *)(*ppath->memory_procs.alloc)(1, sizeof(ctype), cname)) )\
    return_error(gs_error_limitcheck);\
  p_alloc((char *)pseg, sizeof(ctype));\
  pseg->type = stype, pseg->next = 0
#define path_alloc_link(pseg)\
  { segment *prev = psub->last;\
    prev->next = (segment *)pseg;\
    pseg->prev = prev;\
    psub->last = (segment *)pseg;\
  }

/* Open a new subpath */
private int
gx_path_new_subpath(gx_path *ppath)
{	subpath *psub = ppath->current_subpath;
	register subpath *spp;
	path_alloc_segment(spp, subpath, s_start, "gx_path_new_subpath");
	spp->last = (segment *)spp;
	spp->curve_count = 0;
	spp->closed = 0;
	spp->pt = ppath->position;
	ppath->subpath_open = 1;
	if ( !psub )			/* first subpath */
	   {	ppath->first_subpath = spp;
		spp->prev = 0;
	   }
	else
	   {	segment *prev = psub->last;
		prev->next = (segment *)spp;
		spp->prev = prev;
	   }
	ppath->current_subpath = spp;
	ppath->subpath_count++;
#ifdef DEBUG
if ( gs_debug['p'] )
	dprintf("[p]"), gx_print_segment((segment *)spp);
#endif
	return 0;
}

/* Add a point to the current path (moveto). */
int
gx_path_add_point(register gx_path *ppath, fixed x, fixed y)
{	ppath->subpath_open = 0;
	ppath->position_valid = 1;
	ppath->position.x = x;
	ppath->position.y = y;
	return 0;
}

/* Add a relative point to the current path (rmoveto). */
int
gx_path_add_relative_point(register gx_path *ppath, fixed dx, fixed dy)
{	if ( !ppath->position_valid )
	  return_error(gs_error_nocurrentpoint);
	ppath->subpath_open = 0;
	ppath->position.x += dx;
	ppath->position.y += dy;
	return 0;
}

/* Set the segment point and the current point in the path. */
/* Assumes ppath points to the path. */
#define path_set_point(pseg, fx, fy)\
	(pseg)->pt.x = ppath->position.x = (fx),\
	(pseg)->pt.y = ppath->position.y = (fy)

/* Add a line to the current path (lineto). */
int
gx_path_add_line(gx_path *ppath, fixed x, fixed y)
{	subpath *psub = ppath->current_subpath;
	register line_segment *lp;
	path_open();
	path_alloc_segment(lp, line_segment, s_line, "gx_path_add_line");
	path_alloc_link(lp);
	path_set_point(lp, x, y);
#ifdef DEBUG
if ( gs_debug['p'] )
	dprintf("[p]"), gx_print_segment((segment *)lp);
#endif
	return 0;
}

/* Add a rectangle to the current path. */
/* This is a special case of adding a parallelogram. */
int
gx_path_add_rectangle(gx_path *ppath, fixed x0, fixed y0, fixed x1, fixed y1)
{	return gx_path_add_pgram(ppath, x0, y0, x0, y1, x1, y1);
}

/* Add a parallelogram to the current path. */
/* This is equivalent to an add_point, three add_lines, */
/* and a close_subpath. */
int
gx_path_add_pgram(gx_path *ppath,
  fixed x0, fixed y0, fixed x1, fixed y1, fixed x2, fixed y2)
{	int code;
 	if (	(code = gx_path_add_point(ppath, x0, y0)) < 0 ||
		(code = gx_path_add_line(ppath, x1, y1)) < 0 ||
		(code = gx_path_add_line(ppath, x2, y2)) < 0 ||
		(code = gx_path_add_line(ppath, x0 + x2 - x1, y0 + y2 - y1)) < 0 ||
		(code = gx_path_close_subpath(ppath)) < 0
	   ) return code;
	return 0;
}

/* Add a curve to the current path (curveto). */
int
gx_path_add_curve(gx_path *ppath,
  fixed x1, fixed y1, fixed x2, fixed y2, fixed x3, fixed y3)
{	subpath *psub = ppath->current_subpath;
	register curve_segment *lp;
	path_open();
	path_alloc_segment(lp, curve_segment, s_curve, "gx_path_add_curve");
	path_alloc_link(lp);
	lp->p1.x = x1;
	lp->p1.y = y1;
	lp->p2.x = x2;
	lp->p2.y = y2;
	path_set_point(lp, x3, y3);
	psub->curve_count++;
	ppath->curve_count++;
#ifdef DEBUG
if ( gs_debug['p'] )
	dprintf("[p]"), gx_print_segment((segment *)lp);
#endif
	return 0;
}

/*
 * Add an approximation of an arc to the current path.
 * Parameters are the initial and final points of the arc,
 * and the point at which the extended tangents meet.
 * We assume that the arc is less than a semicircle.
 * The arc may go either clockwise or counterclockwise.
 * The approximation is a very simple one: a single curve
 * whose other two control points are a fraction F of the way
 * to the intersection of the tangents, where
 *	F = (4/3)(1 / (1 + sqrt(1+(d/r)^2)))
 * where r is the radius and d is the distance from either tangent
 * point to the intersection of the tangents.  This produces
 * a curve whose center point, as well as its ends, lies on
 * the desired arc.
 *
 * Because F has to be computed in user space, we let the client
 * compute it and pass it in as an argument.
 */
int
gx_path_add_arc(gx_path *ppath,
  fixed x0, fixed y0, fixed x3, fixed y3, fixed xt, fixed yt, floatp fraction)
{	return gx_path_add_curve(ppath,
			x0 + (fixed)((xt - x0) * fraction),
			y0 + (fixed)((yt - y0) * fraction),
			x3 + (fixed)((xt - x3) * fraction),
			y3 + (fixed)((yt - y3) * fraction),
			x3, y3);
}

/* Append a path to another path, and reset the first path. */
/* Currently this is only used to append a path to its parent */
/* (the path in the previous graphics context). */
int
gx_path_add_path(gx_path *ppath, gx_path *ppfrom)
{	subpath *psub = ppath->current_subpath;
	path_unshare();
	if ( ppfrom->first_subpath )	/* i.e. ppfrom not empty */
	   {	if ( ppath->first_subpath )	/* i.e. ppath not empty */
		   {	segment *pseg = psub->last;
			segment *pfseg = (segment *)ppfrom->first_subpath;
			pseg->next = pfseg;
			pfseg->prev = pseg;
		   }
		else
			ppath->first_subpath = ppfrom->first_subpath;
		ppath->current_subpath = ppfrom->current_subpath;
	   }
	/* Transfer the remaining state. */
	ppath->subpath_count += ppfrom->subpath_count;
	ppath->curve_count += ppfrom->curve_count;
	ppath->position = ppfrom->position;
	ppath->position_valid = ppfrom->position_valid;
	ppath->subpath_open = ppfrom->subpath_open;
	/* Reset the source path. */
	ppfrom->first_subpath = 0;
	return 0;
}

/* Close the current subpath. */
int
gx_path_close_subpath(gx_path *ppath)
{	subpath *psub = ppath->current_subpath;
	register line_close_segment *lp;
	if ( !ppath->subpath_open ) return 0;
	path_alloc_segment(lp, line_close_segment, s_line_close,
			   "gx_path_close_subpath");
	path_alloc_link(lp);
	path_set_point(lp, psub->pt.x, psub->pt.y);
	lp->sub = psub;
	psub->closed = 1;
	ppath->subpath_open = 0;
#ifdef DEBUG
if ( gs_debug['p'] )
	if ( lp != 0 )
	  dprintf("[p]"), gx_print_segment((segment *)lp);
#endif
	return 0;
}

/* ------ Internal routines ------ */

/* Copy the current path, because it was shared. */
/* Return a pointer to the current subpath, or 0. */
private subpath *
path_alloc_copy(gx_path *ppath)
{	gx_path path_new;
	int code;
	code = gx_path_copy(ppath, &path_new);
	if ( code < 0 ) return 0;
	*ppath = path_new;
	ppath->shares_segments = 0;
	return ppath->current_subpath;
}

/* ------ Debugging printout ------ */

#ifdef DEBUG

/* Print out a path with a label */
void
gx_dump_path(gx_path *ppath, char *tag)
{	dprintf2("[p]Path %lx %s:\n", (ulong)ppath, tag);
	gx_path_print(ppath);
}

/* Print a path */
void
gx_path_print(gx_path *ppath)
{	segment *pseg = (segment *)ppath->first_subpath;
	dprintf4("   subpaths=%d, curves=%d, point=(%f,%f)\n",
		 ppath->subpath_count, ppath->curve_count,
		 fixed2float(ppath->position.x),
		 fixed2float(ppath->position.y));
	dprintf5("   box=(%f,%f),(%f,%f) last=%lx\n",
		 fixed2float(ppath->bbox.p.x), fixed2float(ppath->bbox.p.y),
		 fixed2float(ppath->bbox.q.x), fixed2float(ppath->bbox.q.y),
		 (ulong)ppath->box_last);
	while ( pseg )
	   {	gx_print_segment(pseg);
		pseg = pseg->next;
	   }
}
void
gx_print_segment(segment *pseg)
{	char out[80];
	sprintf(out, "   %lx<%lx,%lx>: %%s (%6g,%6g) ",
		(ulong)pseg, (ulong)pseg->prev, (ulong)pseg->next,
		fixed2float(pseg->pt.x), fixed2float(pseg->pt.y));
	switch ( pseg->type )
	   {
	case s_start:
#define psub ((subpath *)pseg)
		dprintf1(out, "start");
		dprintf2("#curves=%d last=%lx",
			 psub->curve_count, (ulong)psub->last);
#undef psub
		break;
	case s_curve:
		dprintf1(out, "curve");
#define pcur ((curve_segment *)pseg)
		dprintf4("\n\tp1=(%f,%f) p2=(%f,%f)",
			 fixed2float(pcur->p1.x), fixed2float(pcur->p1.y),
			 fixed2float(pcur->p2.x), fixed2float(pcur->p2.y));
#undef pcur
		break;
	case s_line:
		dprintf1(out, "line");
		break;
	case s_line_close:
#define plc ((line_close_segment *)pseg)
		dprintf1(out, "close");
		dprintf1(" %lx", (ulong)(plc->sub));
#undef plc
		break;
	default:
	   {	char t[20];
		sprintf(t, "type 0x%x", pseg->type);
		dprintf1(out, t);
	   }
	   }
	dputc('\n');
}

/* Print a clipping path */
void
gx_cpath_print(gx_clip_path *pcpath)
{	if ( pcpath->segments_valid )
		gx_path_print(&pcpath->path);
	else
		dputs("   (segments not valid)\n");
	dprintf5("   cbox=(%f,%f),(%f,%f) count=%d\n",
		 fixed2float(pcpath->cbox.p.x), fixed2float(pcpath->cbox.p.y),
		 fixed2float(pcpath->cbox.q.x), fixed2float(pcpath->cbox.q.y),
		 pcpath->list.count);
	/****** SHOULD PRINT CLIP LIST ******/
}

#endif					/* DEBUG */
