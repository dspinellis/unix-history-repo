/* Copyright (C) 1991, 1992 Aladdin Enterprises.  All rights reserved.
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

/* gxcpath.c */
/* Implementation of clipping paths */
#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gxfixed.h"
#include "gzcolor.h"
#include "gzpath.h"
#include "gxcpath.h"

#define min_int (-1 << (sizeof(int) * 8 - 1))
#define max_int (~min_int)

const uint gs_clip_path_sizeof = sizeof(gx_clip_path);

/* Imported procedures */
gx_device *gs_currentdevice(P1(gs_state *));
void gx_set_device_only(P2(gs_state *, gx_device *));

/* Forward references */
private void clip_prepare(P1(gx_clip_list *));

private const gx_clip_list clip_list_empty =
{  { 0, 0, min_int, min_int, min_int, min_int },
   { 0, 0, min_int, max_int, 0, 0 },
   { 0, 0, max_int, max_int, max_int, max_int },
   0
};

/* Debugging */

#ifdef DEBUG
#  define clip_rect_print(str, ar)\
    if ( gs_debug['q'] )\
	dprintf6("[q]%s %lx: (%d,%d),(%d,%d)\n", str, (ulong)ar,\
		 (ar)->xmin, (ar)->ymin, (ar)->xmax, (ar)->ymax)
#else
#  define clip_rect_print(s, ar) 0
#endif

#ifdef DEBUG
/* Validate a clipping path that has gone through clip_prepare. */
private void
clip_list_validate(gx_clip_list *clp)
{	gx_clip_rect *prev = &clp->first;
	gx_clip_rect *ptr = prev;
	int wrong = 0;
	while ( ptr != 0 )
	  { if ( ptr->ymin > ptr->ymax || ptr->xmin > ptr->xmax ||
		!(ptr->ymin >= prev->ymax ||
		  ptr->ymin == prev->ymin && ptr->ymax == prev->ymax &&
		  ptr->xmin >= prev->xmax)
		)
	      { clip_rect_print("WRONG:", ptr);
		wrong = 1;
	      }
	    prev = ptr, ptr = ptr->next;
	  }
}
#endif

/* ------ Clipping path accessing ------ */

/* Return the path of a clipping path. */
int
gx_cpath_path(gx_clip_path *pcpath, gx_path *ppath)
{	if ( !pcpath->segments_valid )
	   {	int code = gx_clip_list_add_to_path(&pcpath->list, &pcpath->path);
		if ( code < 0 ) return code;
		pcpath->segments_valid = 1;
	   }
	*ppath = pcpath->path;
	return 0;
}

/* Return the quick-check rectangle for a clipping path. */
int
gx_cpath_box_for_check(gx_clip_path *pcpath, gs_fixed_rect *pbox)
{	*pbox = pcpath->cbox;
	return 0;
}

/* Test if a clipping path includes a rectangle. */
/* The rectangle need not be oriented correctly, i.e. x0 > x1 is OK. */
int
gx_cpath_includes_rectangle(register gx_clip_path *pcpath,
  fixed x0, fixed y0, fixed x1, fixed y1)
{	return
		(x0 <= x1 ?
			(pcpath->cbox.p.x <= x0 && x1 <= pcpath->cbox.q.x) :
			(pcpath->cbox.p.x <= x1 && x0 <= pcpath->cbox.q.x)) &&
		(y0 <= y1 ?
			(pcpath->cbox.p.y <= y0 && y1 <= pcpath->cbox.q.y) :
			(pcpath->cbox.p.y <= y1 && y0 <= pcpath->cbox.q.y));
}

/* Release a clipping path. */
void
gx_cpath_release(gx_clip_path *pcpath)
{	if ( !pcpath->shares_list )
		gx_clip_list_free(&pcpath->list, &pcpath->path.memory_procs);
	gx_path_release(&pcpath->path);
}

/* Share a clipping path. */
void
gx_cpath_share(gx_clip_path *pcpath)
{	gx_path_share(&pcpath->path);
	pcpath->shares_list = 1;
}

/* Create a rectangular clipping path. */
/* The supplied rectangle may not be oriented correctly, */
/* but it will be oriented correctly upon return. */
int
gx_cpath_from_rectangle(gx_clip_path *pcpath, gs_fixed_rect *pbox, gs_memory_procs *mp)
{	gx_clip_list_from_rectangle(&pcpath->list, pbox);
	pcpath->cbox = *pbox;
	pcpath->segments_valid = 0;
	pcpath->shares_list = 0;
	gx_path_init(&pcpath->path, mp);
	pcpath->path.bbox = *pbox;
	return 0;
}

/* Intersect a new clipping path with an old one. */
/* Note that it may overwrite its path argument. */
int
gx_cpath_intersect(gs_state *pgs, gx_clip_path *pcpath, gx_path *ppath, int rule)
{	gs_fixed_rect old_box, new_box;
	if ( gx_cpath_is_rectangle(pcpath, &old_box) &&
	    gx_path_is_rectangle(ppath, &new_box)
	   )
	   {	int changed = 0;
		/* Intersect the two rectangles if necessary. */
		if ( old_box.p.x > new_box.p.x )
			new_box.p.x = old_box.p.x, changed = 1;
		if ( old_box.p.y > new_box.p.y )
			new_box.p.y = old_box.p.y, changed = 1;
		if ( old_box.q.x < new_box.q.x )
			new_box.q.x = old_box.q.x, changed = 1;
		if ( old_box.q.y < new_box.q.y )
			new_box.q.y = old_box.q.y, changed = 1;
		if ( changed )
		   {	/* Store the new rectangle back into the new path. */
			register segment *pseg =
				(segment *)ppath->first_subpath;
#define set_pt(pqx,pqy)\
  pseg->pt.x = new_box.pqx.x, pseg->pt.y = new_box.pqy.y
			set_pt(p, p); pseg = pseg->next;
			set_pt(q, p); pseg = pseg->next;
			set_pt(q, q); pseg = pseg->next;
			set_pt(p, q); pseg = pseg->next;
			if ( pseg != 0 ) /* might be an open rectangle */
			  set_pt(p, p);
#undef set_pt
		   }
		ppath->bbox = new_box;
		gx_clip_list_from_rectangle(&pcpath->list, &new_box);
		pcpath->cbox = new_box;
		pcpath->path = *ppath;
		pcpath->segments_valid = 1;
	   }
	else
	   {	/* Not a rectangle.  Intersect the slow way. */
		gx_device_accum adev;
		gx_device_color devc;
		gx_device *save_dev = gs_currentdevice(pgs);
		int code;
		adev = gs_accum_device;
		adev.memory_procs = pcpath->path.memory_procs;
		(*adev.procs->open_device)((gx_device *)&adev);
		devc.color1 = devc.color2 = 0;	/* arbitrary, but not */
					/* transparent */
		devc.halftone_level = 0;
		gx_set_device_only(pgs, (gx_device *)&adev);
		code = gx_fill_path(ppath, &devc, pgs, rule, (fixed)0);
		gx_set_device_only(pgs, save_dev);
		if ( code < 0 ) return code;
		code = (*adev.procs->close_device)((gx_device *)&adev);
		if ( code < 0 ) return code;
		pcpath->list = adev.list;
		gx_path_init(&pcpath->path, &pcpath->path.memory_procs);
		pcpath->path.bbox.p.x = int2fixed(adev.bbox.p.x);
		pcpath->path.bbox.p.y = int2fixed(adev.bbox.p.y);
		pcpath->path.bbox.q.x = int2fixed(adev.bbox.q.x);
		pcpath->path.bbox.q.y = int2fixed(adev.bbox.q.y);
		/* Note that the result of the intersection might be */
		/* a single rectangle.  This will cause clip_path_is_rect.. */
		/* to return true.  This, in turn, requires that */
		/* we set pcpath->cbox correctly. */
		if ( clip_list_is_rectangle(&adev.list) )
			pcpath->cbox = pcpath->path.bbox;
		else
		   {	/* The quick check must fail. */
			pcpath->cbox.p.x = pcpath->cbox.p.y = 0;
			pcpath->cbox.q.x = pcpath->cbox.q.y = 0;
		   }
		pcpath->segments_valid = 0;
		pcpath->shares_list = 0;
	   }
	return 0;
}

/* ------ Clipping list routines ------ */

/* Initialize a clip list. */
void
gx_clip_list_init(gx_clip_list *clp)
{	*clp = clip_list_empty;
}

/* Initialize a clip list to a rectangle. */
/* The supplied rectangle may not be oriented correctly, */
/* but it will be oriented correctly upon return. */
void
gx_clip_list_from_rectangle(gx_clip_list *clp, register gs_fixed_rect *rp)
{	gx_clip_list_init(clp);
	if ( rp->p.x > rp->q.x )
	  { fixed t = rp->p.x; rp->p.x = rp->q.x; rp->q.x = t; }
	if ( rp->p.y > rp->q.y )
	  { fixed t = rp->p.y; rp->p.y = rp->q.y; rp->q.y = t; }
	clp->sole.xmin = fixed2int_var_rounded(rp->p.x);
	clp->sole.ymin = fixed2int_var_rounded(rp->p.y);
	clp->sole.xmax = fixed2int_var_rounded(rp->q.x);
	clp->sole.ymax = fixed2int_var_rounded(rp->q.y);
	clp->count = 1;
}

/* Add a clip list to a path. */
/* The current implementation is very inefficient. */
int
gx_clip_list_add_to_path(gx_clip_list *clp, gx_path *ppath)
{	gx_clip_rect *rp;
	int code;
	clip_prepare(clp);
	for ( rp = &clp->first; rp != 0; rp = rp->next )
	   {	if ( rp->xmin < rp->xmax && rp->ymin < rp->ymax )
		   {	code = gx_path_add_rectangle(ppath,
					int2fixed(rp->xmin),
					int2fixed(rp->ymin),
					int2fixed(rp->xmax),
					int2fixed(rp->ymax));
			if ( code < 0 ) return code;
		   }
	   }
	return 0;
}

/* Free a clip list. */
void
gx_clip_list_free(gx_clip_list *clp, gs_memory_procs *mp)
{	gx_clip_rect *rp = clp->last.prev;
	if ( clp->count <= 1 ) return;
	clip_prepare(clp);
	while ( rp != &clp->first )
	   {	gx_clip_rect *prev = rp->prev;
		(*mp->free)((char *)rp, 1, sizeof(gx_clip_rect), "gx_clip_list_free");
		rp = prev;
	   }
	gx_clip_list_init(clp);
}

/* Prepare a clip list for enumeration, */
/* by splicing pointers to account for possible relocation. */
private void
clip_prepare(register gx_clip_list *clp)
{	if ( clp->count <= 1 )
	   {	clp->first.next = clp->last.prev = &clp->sole;
		clp->sole.prev = &clp->first;
		clp->sole.next = &clp->last;
	   }
	else
	   {	clp->first.next->prev = &clp->first;
		clp->last.prev->next = &clp->last;
	   }
}

/* ------ Rectangle list accumulator ------ */

/* Device for accumulating a clipping region. */
private dev_proc_open_device(accum_open);
private dev_proc_close_device(accum_close);
private dev_proc_fill_rectangle(accum_fill_rectangle);

/* The device descriptor */
/* Many of these procedures won't be called; they are set to NULL. */
private gx_device_procs accum_procs = {
	accum_open,
	NULL,				/* get_initial_matrix */
	NULL,				/* sync_output */
	NULL,				/* output_page */
	accum_close,
	NULL,				/* map_rgb_color */
	NULL,				/* map_color_rgb */
	accum_fill_rectangle,
	NULL,				/* tile_rectangle */
	NULL,				/* copy_mono */
	NULL,				/* copy_color */
	NULL,				/* draw_line */
	NULL,				/* get_bits */
	NULL,				/* get_props */
	NULL				/* put_props */
};
gx_device_accum gs_accum_device =
{	sizeof(gx_device_accum),
	&accum_procs,
	"clip list accumulator",
	0, 0, 1, 1, no_margins, dci_black_and_white, 0	/* generic */
};
#define adev ((gx_device_accum *)dev)

/* Initialize the accumulation device. */
private int
accum_open(register gx_device *dev)
{	gx_clip_list_init(&adev->list);
	adev->last = &adev->list.first;
	adev->bbox.p.x = adev->bbox.p.y = max_int;
	adev->bbox.q.x = adev->bbox.q.y = min_int;
	return 0;
}

/* Close the accumulation device. */
private int
accum_close(gx_device *dev)
{	if ( adev->list.count >= 2 )
	   {	/* 'sole' isn't good for much of anything, */
		/* and it complicates the bookkeeping.... */
		gx_clip_rect *last = adev->last;
		gx_clip_rect *ar =
		  (gx_clip_rect *)(*adev->memory_procs.alloc)(1, sizeof(gx_clip_rect), "accum_close");
		if ( ar == 0 ) return_error(gs_error_VMerror);
		*ar = adev->list.sole;
		adev->list.sole.prev->next = ar;
		if ( last == &adev->list.sole )
			last = ar;
		else
			adev->list.sole.next->prev = ar;
		adev->list.last.prev = last;
	   }
#ifdef DEBUG
if ( gs_debug['q'] )
   {	gx_clip_rect *rp = &adev->list.first;
	adev->last->next = 0;
	while ( rp != 0 )
	   {	clip_rect_print("   ", rp);
		rp = rp->next;
	   }
   }
	clip_prepare(&adev->list); /* just for clip_list_validate */
	clip_list_validate(&adev->list);
#endif
	return 0;
}

/* Accumulate one rectangle. */
#define accum_alloc(s, ar, px, py, qx, qy)\
   {	ar = (adev->list.count == 0 ? &adev->list.sole :\
	   (gx_clip_rect *)(*adev->memory_procs.alloc)(1, sizeof(gx_clip_rect), "accum_rect"));\
	if ( ar == 0 ) return_error(gs_error_VMerror);\
	ar->xmin = px, ar->ymin = py, ar->xmax = qx, ar->ymax = qy;\
	adev->list.count++;\
	clip_rect_print(s, ar);\
   }
#define accum_add_last(ar)\
	adev->last->next = ar, ar->prev = adev->last, adev->last = ar
#define accum_add_after(ar, rprev)\
	ar->prev = rprev, ar->next = rprev->next;\
	if ( rprev != adev->last ) rprev->next->prev = ar;\
	else adev->last = ar;\
	rprev->next = ar
#define accum_add_before(ar, rnext)\
	ar->prev = rnext->prev, ar->next = rnext,\
	  rnext->prev->next = ar, rnext->prev = ar
/* Add a rectangle to the list.  It would be wonderful if rectangles */
/* were always presented in the correct order, but they aren't, */
/* because the fill loop works by trapezoids, not by scan lines. */
/* All we can count on is that they are disjoint and *approximately* */
/* in order. */
#undef adev
private int
accum_add_rect(gx_device_accum *adev, int x, int y, int xe, int ye)
{	gx_clip_rect *nr, *ar, *rptr;
	int ymin, ymax;
top:	rptr = adev->last;
	accum_alloc("accum", nr, x, y, xe, ye);
	if ( y >= rptr->ymax ||
	    y == rptr->ymin && ye == rptr->ymax && x >= rptr->xmax
	   )
	   {	accum_add_last(nr);
		return 0;
	   }
	/* Work backwards till we find the insertion point. */
	while ( ye <= rptr->ymin ) rptr = rptr->prev;
	ymin = rptr->ymin;
	ymax = rptr->ymax;
	if ( ye > ymax )
	   {	if ( y >= ymax )
		   {	/* Insert between two bands. */
			accum_add_after(nr, rptr);
			return 0;
		   }
		/* Split off the top part of the new rectangle. */
		accum_alloc("a.top", ar, x, ymax, xe, ye);
		accum_add_after(ar, rptr);
		ye = nr->ymax = ymax;
		clip_rect_print(" ymax", nr);
	   }
	/* Here we know ymin < ye <= ymax; */
	/* rptr points to the last node with this value of ymin/ymax. */
	/* Split the existing band if necessary. */
	if ( ye < ymax )
	   {	gx_clip_rect *rsplit = rptr;
		while ( rsplit->ymax == ymax )
		   {	accum_alloc("s.top", ar, rsplit->xmin, ye, rsplit->xmax, ymax);
			accum_add_after(ar, rptr);
			rsplit->ymax = ye;
			rsplit = rsplit->prev;
		   }
		ymax = ye;
	   }
	if ( y > ymin )
	   {	gx_clip_rect *rbot = rptr, *rsplit;
		while ( rbot->prev->ymin == ymin )
			rbot = rbot->prev;
		for ( rsplit = rbot; ; )
		   {	accum_alloc("s.bot", ar, rsplit->xmin, ymin, rsplit->xmax, y);
			accum_add_before(ar, rbot);
			rsplit->ymin = y;
			if ( rsplit == rptr ) break;
			rsplit = rsplit->next;
		   }
		ymin = y;
	   }
	/* Search for the X insertion point. */
	/* The new rectangle is guaranteed disjoint from all the old ones. */
	while ( rptr->ymin == ymin && x < rptr->xmax )
	   {	rptr = rptr->prev;
	   }
	if ( y < ymin )
	   {	/* Continue with the bottom part of the new rectangle. */
		nr->ymin = ymin;
		clip_rect_print(" ymin", nr);
		accum_add_after(nr, rptr);
		ye = ymin;
		goto top;
	   }
	accum_add_after(nr, rptr);
	return 0;
}
#define adev ((gx_device_accum *)dev)
private int
accum_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	int xe, ye;
	if ( w <= 0 || h <= 0 ) return 0;
	xe = x + w, ye = y + h;
	/* Update the bounding box. */
	if ( x < adev->bbox.p.x ) adev->bbox.p.x = x;
	if ( y < adev->bbox.p.y ) adev->bbox.p.y = y;
	if ( xe > adev->bbox.q.x ) adev->bbox.q.x = xe;
	if ( ye > adev->bbox.q.y ) adev->bbox.q.y = ye;
	return accum_add_rect(adev, x, y, xe, ye);
}

/* ------ Rectangle list clipper ------ */

/* Device for clipping with a region. */
private dev_proc_open_device(clip_open);
private dev_proc_map_rgb_color(clip_map_rgb_color);
private dev_proc_map_color_rgb(clip_map_color_rgb);
private dev_proc_fill_rectangle(clip_fill_rectangle);
private dev_proc_tile_rectangle(clip_tile_rectangle);
private dev_proc_copy_mono(clip_copy_mono);
private dev_proc_copy_color(clip_copy_color);
private dev_proc_get_bits(clip_get_bits);
private dev_proc_get_props(clip_get_props);
private dev_proc_put_props(clip_put_props);

/* The device descriptor. */
private gx_device_procs clip_procs = {
	clip_open,
	gx_default_get_initial_matrix,
	gx_default_sync_output,
	gx_default_output_page,
	gx_default_close_device,
	clip_map_rgb_color,
	clip_map_color_rgb,
	clip_fill_rectangle,
	clip_tile_rectangle,
	clip_copy_mono,
	clip_copy_color,
	gx_default_draw_line,
	clip_get_bits,
	clip_get_props,
	clip_put_props
};
gx_device_clip gs_clip_device =
{	sizeof(gx_device_clip),
	&clip_procs,
	"clipper",
	0, 0, 1, 1, no_margins, dci_black_and_white, 0	/* generic */
};
#define rdev ((gx_device_clip *)dev)

/* Declare and initialize the cursor variables. */
#ifdef DEBUG
private ulong clip_in, clip_down, clip_down2, clip_up, clip_x, clip_no_x;
#  define inc(v) v++
#else
#  define inc(v) 0
#endif
#define DECLARE_CLIP\
  register gx_clip_rect *rptr = rdev->current.rptr;\
  gx_device *tdev = rdev->target;
/* Check whether the rectangle x,y,w,h falls within the current entry. */
#define xywh_in_ryptr()\
  ((y >= rptr->ymin && y + h <= rptr->ymax &&\
    x >= rptr->xmin && x + w <= rptr->xmax) ? (inc(clip_in), 1) : 0)
/*
 * Warp the cursor forward or backward to the first rectangle row that
 * could include a given y value.  Assumes rptr is set, and updates it.
 * Specifically, after warp_cursor, y < rptr->ymax and y >= rptr->prev->ymax.
 * Note that ye <= rptr->ymin is possible.
 */
#define warp_cursor(y)\
  while ( (y) >= rptr->ymax ) { inc(clip_up); rptr = rptr->next; };\
  while ( rptr->prev != 0 && (y) < rptr->prev->ymax )\
   { inc(clip_down); rptr = rptr->prev; }
/*
 * Enumerate the rectangles of the x,w,y,h argument that fall within
 * the clipping region.  Usage:
 *	BEGIN_CLIP
 *		... xc, yc, xec, yec ... [must be an expression statement]
 *	NEXT_CLIP
 *		(about to set yc to yec)
 *	END_CLIP
 */
#ifdef DEBUG
#  define clip_2_print(str, v1, v2)\
    if ( gs_debug['q'] ) dprintf2(str, v1, v2)
#else
#  define clip_2_print(str, v1, v2) 0
#endif
#define BEGIN_CLIP\
	if ( w <= 0 || h <= 0 ) return 0;\
   {	int yc;\
	const int xe = x + w, ye = y + h;\
	warp_cursor(y);\
	rdev->current.rptr = rptr;\
	yc = rptr->ymin;\
	if ( yc < y ) yc = y;\
	else if ( yc >= ye ) return 0;\
	for ( ; ; )\
	   {	const int ymax = rptr->ymax;\
		int yec = ymax;\
		if ( yec > ye ) yec = ye;\
		clip_2_print("[q]yc=%d yec=%d\n", yc, yec);\
		do \
		   {	int xc = rptr->xmin;\
			int xec = rptr->xmax;\
			if ( xc < x ) xc = x;\
			if ( xec > xe ) xec = xe;\
			if ( xec > xc )\
			   {	int code;\
				clip_rect_print("match", rptr);\
				clip_2_print("[q]xc=%d xec=%d\n", xc, xec);\
				inc(clip_x);\
				code =
#define NEXT_CLIP\
				if ( code < 0 ) return code;\
			   }\
			else inc(clip_no_x);\
		   }\
		while ( (rptr = rptr->next) != 0 && rptr->ymax == ymax );\
		if ( rptr == 0 || (yec = rptr->ymin) >= ye ) break;
#define END_CLIP\
		yc = yec;\
	   }\
   }

/* Open a clipping device */
private int
clip_open(register gx_device *dev)
{	gx_device *tdev = rdev->target;
	/* Fix up possible dangling pointers. */
	clip_prepare(&rdev->list);
	/* Initialize the cursor. */
	rdev->current.rptr = &rdev->list.first;
	rdev->color_info = tdev->color_info;
	return 0;
}

/* Forward non-displaying operations to the target device. */
private gx_color_index
clip_map_rgb_color(gx_device *dev, gx_color_value r, gx_color_value g,
  gx_color_value b)
{	gx_device *tdev = rdev->target;
	return (*tdev->procs->map_rgb_color)(tdev, r, g, b);
}
private int
clip_map_color_rgb(gx_device *dev, gx_color_index color,
  gx_color_value prgb[3])
{	gx_device *tdev = rdev->target;
	return (*tdev->procs->map_color_rgb)(tdev, color, prgb);
}
private int
clip_get_props(gx_device *dev, gs_prop_item *plist)
{	gx_device *tdev = rdev->target;
	return (*tdev->procs->get_props)(tdev, plist);
}
private int
clip_put_props(gx_device *dev, gs_prop_item *plist, int count)
{	gx_device *tdev = rdev->target;
	return (*tdev->procs->put_props)(tdev, plist, count);
}

/* Fill a rectangle */
private int
clip_fill_rectangle(gx_device *dev, int x, int y, int w, int h,
  gx_color_index color)
{	DECLARE_CLIP
	dev_proc_fill_rectangle((*fill)) = tdev->procs->fill_rectangle;
	if ( xywh_in_ryptr() )
		return (*fill)(tdev, x, y, w, h, color);
	BEGIN_CLIP
		(*fill)(tdev, xc, yc, xec - xc, yec - yc, color);
	NEXT_CLIP
	END_CLIP
	return 0;
}

/* Tile a rectangle */
private int
clip_tile_rectangle(gx_device *dev, gx_bitmap *tile,
  int x, int y, int w, int h,
  gx_color_index color0, gx_color_index color1, int phase_x, int phase_y)
{	DECLARE_CLIP
	dev_proc_tile_rectangle((*fill)) = tdev->procs->tile_rectangle;
	if ( xywh_in_ryptr() )
		return (*fill)(tdev, tile, x, y, w, h, color0, color1, phase_x, phase_y);
	BEGIN_CLIP
		(*fill)(tdev, tile, xc, yc, xec - xc, yec - yc, color0, color1, phase_x, phase_y);
	NEXT_CLIP
	END_CLIP
	return 0;
}

/* Copy a monochrome rectangle */
private int
clip_copy_mono(gx_device *dev,
  byte *data, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h,
  gx_color_index color0, gx_color_index color1)
{	DECLARE_CLIP
	dev_proc_copy_mono((*copy)) = tdev->procs->copy_mono;
	if ( xywh_in_ryptr() )
		return (*copy)(tdev, data, sourcex, raster, id, x, y, w, h, color0, color1);
	BEGIN_CLIP
		(*copy)(tdev, data, sourcex + xc - x, raster, gx_no_bitmap_id, xc, yc, xec - xc, yec - yc, color0, color1);
	NEXT_CLIP
		data += (yec - yc) * raster;
	END_CLIP
	return 0;
}

/* Copy a color rectangle */
private int
clip_copy_color(gx_device *dev,
  byte *data, int sourcex, int raster, gx_bitmap_id id,
  int x, int y, int w, int h)
{	DECLARE_CLIP
	dev_proc_copy_color((*copy)) = tdev->procs->copy_color;
	if ( xywh_in_ryptr() )
		return (*copy)(tdev, data, sourcex, raster, id, x, y, w, h);
	BEGIN_CLIP
		(*copy)(tdev, data, sourcex + xc - x, raster, gx_no_bitmap_id, xc, yc, xec - xc, yec - yc);
	NEXT_CLIP
		data += (yec - yc) * raster;
	END_CLIP
	return 0;
}

/* Get bits back from the device. */
private int
clip_get_bits(gx_device *dev, int y, byte *data, uint size, int pad)
{	gx_device *tdev = rdev->target;
	return (*tdev->procs->get_bits)(tdev, y, data, size, pad);
}
