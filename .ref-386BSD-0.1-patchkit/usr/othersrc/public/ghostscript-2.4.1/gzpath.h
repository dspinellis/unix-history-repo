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

/* gzpath.h */
/* Private representation of paths for Ghostscript library */
/* Requires gxfixed.h */
#include "gxpath.h"

/* Paths are represented as a linked list of line or curve segments, */
/* similar to what pathforall reports. */

/* Definition of a path segment: a segment start, a line, */
/* or a Bezier curve. */
typedef enum {
	s_start,
	s_line,
	s_line_close,
	s_curve
} segment_type;
#define segment_type_sizes\
  sizeof(subpath), sizeof(line_segment), sizeof(line_close_segment),\
  sizeof(curve_segment)
#define segment_common\
	struct segment_s *prev;\
	struct segment_s *next;\
	segment_type type;\
	gs_fixed_point pt;		/* initial point for starts, */\
					/* final point for others */
/* A generic segment */
typedef struct segment_s {
	segment_common
} segment;
/* A start segment.  This serves as the head of a subpath. */
typedef struct {
	segment_common
	segment *last;			/* last segment of subpath, */
					/* points back to here if empty */
	int curve_count;		/* # of curves */
	char closed;			/* true if subpath is closed */
} subpath;
/* Line segments have no special data. */
typedef struct {
	segment_common
} line_segment;
/* Line_close segments are for the lines appended by closepath. */
/* They point back to the subpath being closed. */
typedef struct {
	segment_common
	subpath *sub;
} line_close_segment;
/* Curve segments store the control points, not the coefficients. */
/* We may want to change this someday. */
typedef struct {
	segment_common
	gs_fixed_point p1, p2;
} curve_segment;

/* Here is the actual structure of a path. */
struct gx_path_s {
	gs_memory_procs memory_procs;
	gs_fixed_rect bbox;		/* bounding box (in device space) */
	segment *box_last;		/* bbox incorporates segments */
					/* up to & including this one */
	subpath *first_subpath;
	subpath *current_subpath;
	int subpath_count;
	int curve_count;
	gs_fixed_point position;	/* current position */
	char position_valid;
	char subpath_open;
	char shares_segments;		/* if true, this path shares its */
					/* segment storage with the one in */
					/* the previous saved graphics state */
};

/* Macros equivalent to a few heavily used procedures. */
/* Be aware that these macros may evaluate arguments more than once. */
#define gx_path_current_point_inline(ppath,ppt)\
 ( !ppath->position_valid ? gs_note_error(gs_error_nocurrentpoint) :\
   ((ppt)->x = ppath->position.x, (ppt)->y = ppath->position.y, 0) )
/* ...rel_point rather than ...relative_point is because */
/* some compilers dislike identifiers of >31 characters. */
#define gx_path_add_rel_point_inline(ppath,dx,dy)\
 ( !ppath->position_valid ? gs_note_error(gs_error_nocurrentpoint) :\
   (ppath->position.x += dx, ppath->position.y += dy,\
    ppath->subpath_open = 0) )

/* ------ Clipping paths ------ */

/*
 * For clipping, a path is represented as a list of rectangles.
 * Normally, a path is created as a list of segments;
 * installing it as a clipping path creates the rectangle list.
 * However, when the clipping path originates in some other way
 * (e.g., from initclip, or for clipping a cached character),
 * or if it is a non-trivial intersection of two paths,
 * the resulting clipping path exists only as a rectangle list;
 * clippath constructs the segment representation if needed.
 * Note that even if the path only exists as a rectangle list,
 * its bounding box (path.bbox) is still correct.
 */

/*
 * Rectangle list structure.
 * Consecutive gx_clip_rect entries either have the same Y values,
 * or ymin of this entry >= ymax of the previous entry.
 * Note that the contents are like a gs_int_rect, not a gs_fixed_rect.
 */
typedef struct gx_clip_rect_s gx_clip_rect;
struct gx_clip_rect_s {
	gx_clip_rect *next, *prev;
	int ymin, ymax;			/* ymax > ymin */
	int xmin, xmax;			/* xmax > xmin */
};
/* There is a dummy first entry with xmin = xmax */
/* to cover Y values starting at min_int, and a dummy last entry */
/* to cover Y values ending at max_int. */
/* This eliminates the need for end tests. */
/* We also preallocate just one internal entry, so that */
/* plain rectangular clipping regions don't need to allocate anything. */
struct gx_clip_list_s {
	gx_clip_rect first, sole, last;
	int count;			/* # of rectangles not counting */
					/* first or last */
};
#define clip_list_is_rectangle(clp) ((clp)->count <= 1)

/* gx_clip_path is a 'subclass' of gx_path. */
struct gx_clip_path_s {
	gx_path path;
	gs_fixed_rect cbox;		/* an inner clipping rectangle */
					/* for a quick check */
	gx_clip_list list;
	char segments_valid;		/* segment representation is valid */
	char shares_list;		/* if true, this path shares its*/
					/* clip list storage with the one in */
					/* the previous saved graphics state */
};
#define gx_cpath_is_rectangle(pcpath, pbox)\
  (clip_list_is_rectangle(&(pcpath)->list) ?\
   (*(pbox) = (pcpath)->cbox, 1) : 0)
