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

/* gspath2.c */
/* Non-constructor path routines for GhostScript library */
#include "gx.h"
#include "gserrors.h"
#include "gspath.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gzpath.h"
#include "gzdevice.h"

/* Forward references */
private int common_clip(P2(gs_state *, int));
private int set_clip_path(P3(gs_state *, gx_clip_path *, int));

/* Path enumeration structure */
struct gs_path_enum_s {
	segment *pseg;
	const gs_state *pgs;
};

/* Size of path enumeration structure, so clients can allocate */
const uint gs_path_enum_sizeof = sizeof(gs_path_enum);

/* ------ Path transformers ------ */

int
gs_flattenpath(gs_state *pgs)
{	gx_path fpath;
	int code;
	if ( !pgs->path->curve_count ) return 0;	/* no curves */
	code = gx_path_flatten(pgs->path, &fpath, pgs->flatness);
	if ( code < 0 ) return code;
	gx_path_release(pgs->path);
	*pgs->path = fpath;
	return 0;
}

int
gs_reversepath(gs_state *pgs)
{	gx_path rpath;
	int code = gx_path_reverse(pgs->path, &rpath);
	if ( code < 0 ) return code;
	gx_path_release(pgs->path);
	*pgs->path = rpath;
	return 0;
}

/* ------ Accessors ------ */

int
gs_pathbbox(gs_state *pgs, gs_rect *pbox)
{	gs_fixed_rect fbox;		/* box in device coordinates */
	gs_rect dbox;
	int code = gx_path_bbox(pgs->path, &fbox);
	if ( code < 0 ) return code;
	/* Transform the result back to user coordinates. */
	dbox.p.x = fixed2float(fbox.p.x);
	dbox.p.y = fixed2float(fbox.p.y);
	dbox.q.x = fixed2float(fbox.q.x);
	dbox.q.y = fixed2float(fbox.q.y);
	return gs_bbox_transform_inverse(&dbox, &ctm_only(pgs), pbox);
}

/* ------ Enumerators ------ */

/* Start enumerating a path */
void
gs_path_enum_init(gs_path_enum *penum, const gs_state *pgs)
{	penum->pseg = (segment *)pgs->path->first_subpath;
	penum->pgs = pgs;
}

/* Enumerate the next element of a path. */
/* If the path is finished, return 0; */
/* otherwise, return the element type. */
int
gs_path_enum_next(gs_path_enum *penum, gs_point ppts[3])
{	segment *pseg = penum->pseg;
	const gs_state *pgs = penum->pgs;
	gs_point pt;
	int code;
	if ( pseg == 0 ) return 0;	/* finished */
	penum->pseg = pseg->next;
	if ( pseg->type == s_line_close )
	  return gs_pe_closepath;
	if ( (code = gs_itransform(pgs, fixed2float(pseg->pt.x),
				   fixed2float(pseg->pt.y), &pt)) < 0 )
	  return code;
	switch ( pseg->type )
	   {
	case s_start:
	     ppts[0] = pt;
	     return gs_pe_moveto;
	case s_line:
	     ppts[0] = pt;
	     return gs_pe_lineto;
	case s_curve:
#define pcurve ((curve_segment *)pseg)
	     if ( (code =
		   gs_itransform(pgs, fixed2float(pcurve->p1.x),
				 fixed2float(pcurve->p1.y), &ppts[0])) < 0 ||
		  (code =
		   gs_itransform(pgs, fixed2float(pcurve->p2.x),
				 fixed2float(pcurve->p2.y), &ppts[1])) < 0 )
	       return 0;
	     ppts[2] = pt;
	     return gs_pe_curveto;
#undef pcurve
	default:
	     lprintf1("bad type %x in gs_path_enum_next!\n", pseg->type);
	     gs_exit(1);
	   }
}

/* ------ Clipping ------ */

int
gs_clippath(gs_state *pgs)
{	gx_path path;
	int code = gx_cpath_path(pgs->clip_path, &path);
	if ( code < 0 ) return code;
	return gx_path_copy(&path, pgs->path);
}

int
gs_initclip(gs_state *pgs)
{	register gx_device *dev = pgs->device->info;
	gs_fixed_rect box;
	if ( is_fzero2(dev->l_margin, dev->r_margin) &&
	     is_fzero2(dev->b_margin, dev->t_margin)
	   )
	   {	/* Shortcut, don't need to worry about density. */
		box.p.x = box.p.y = 0;
		box.q.x = int2fixed(dev->width);
		box.q.y = int2fixed(dev->height);
	   }
	else
	   {	/* Indent from bounding rectangle. */
		gs_matrix_fixed imat;
		(*dev->procs->get_initial_matrix)(dev, (gs_matrix *)&imat);
		gs_update_matrix_fixed(&imat);
		gs_point_transform2fixed(&imat,
					 dev->l_margin * 72,
					 dev->b_margin * 72,
					 &box.p);
		gs_point_transform2fixed(&imat,
					 (dev->width / dev->x_pixels_per_inch - dev->r_margin) * 72,
					 (dev->height / dev->y_pixels_per_inch - dev->t_margin) * 72,
					 &box.q);
	   }
	return gx_clip_to_rectangle(pgs, &box);
}

int
gs_clip(gs_state *pgs)
{	return common_clip(pgs, gx_rule_winding_number);
}

int
gs_eoclip(gs_state *pgs)
{	return common_clip(pgs, gx_rule_even_odd);
}

private int
common_clip(gs_state *pgs, int rule)
{	gx_path fpath;
	int code = gx_path_flatten(pgs->path, &fpath, pgs->flatness);
	if ( code < 0 ) return code;
	code = gx_cpath_intersect(pgs, pgs->clip_path, &fpath, rule);
	if ( code < 0 ) return code;
	return set_clip_path(pgs, pgs->clip_path, rule);
}

/* Establish a rectangle as the clipping path. */
/* Used by initclip and by the character cache logic. */
int
gx_clip_to_rectangle(gs_state *pgs, gs_fixed_rect *pbox)
{	gx_clip_path cpath;
	int code = gx_cpath_from_rectangle(&cpath, pbox, &pgs->memory_procs);
	if ( code < 0 ) return code;
	gx_cpath_release(pgs->clip_path);
	return set_clip_path(pgs, &cpath, gx_rule_winding_number);
}

/* Set the clipping path to the current path, without intersecting. */
/* Currently only used by the insideness testing operators, */
/* but might be used by viewclip eventually. */
/* The algorithm is very inefficient; we'll improve it later if needed. */
int
gx_clip_to_path(gs_state *pgs)
{	gs_fixed_rect bbox;
	int code;
	(code = gx_path_bbox(pgs->path, &bbox)) < 0 ||
	(code = gx_clip_to_rectangle(pgs, &bbox)) < 0 ||
	(code = gs_clip(pgs));
	return code;
}

/* Set the clipping path (internal). */
private int
set_clip_path(gs_state *pgs, gx_clip_path *pcpath, int rule)
{	*pgs->clip_path = *pcpath;
	pgs->clip_rule = rule;
#ifdef DEBUG
if ( gs_debug['p'] )
	dprintf("[p]Clipping path:\n"),
	gx_cpath_print(pcpath);
#endif
	return 0;
}
