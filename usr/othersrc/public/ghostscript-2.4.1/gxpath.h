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

/* gxpath.h */
/* Lower-level path routines for Ghostscript library */
/* Requires gxfixed.h */

/* The routines and types in this interface use */
/* device, rather than user, coordinates, and fixed-point, */
/* rather than floating, representation. */

/* Opaque type for a path */
typedef struct gx_path_s gx_path;
extern const uint gs_path_sizeof;

/* Opaque type for a path enumerator */
typedef struct gx_path_enum_s gx_path_enum;
extern const uint gx_path_enum_sizeof;

/* Define the two insideness rules */
#define gx_rule_winding_number (-1)
#define gx_rule_even_odd 1

/* Debugging routines */
#ifdef DEBUG
extern	void	gx_path_print(P1(gx_path *));
#endif

/* Path constructors. */
extern	void	gx_path_init(P2(gx_path *, gs_memory_procs *)),
		gx_path_release(P1(gx_path *)),
		gx_path_share(P1(gx_path *));
extern	int	gx_path_add_point(P3(gx_path *, fixed, fixed)),
		gx_path_add_relative_point(P3(gx_path *, fixed, fixed)),
		gx_path_add_line(P3(gx_path *, fixed, fixed)),
		gx_path_add_rectangle(P5(gx_path *, fixed, fixed, fixed, fixed)),
		gx_path_add_pgram(P7(gx_path *, fixed, fixed, fixed, fixed, fixed, fixed)),
		gx_path_add_curve(P7(gx_path *, fixed, fixed, fixed, fixed, fixed, fixed)),
		gx_path_add_flattened_curve(P8(gx_path *, fixed, fixed, fixed, fixed, fixed, fixed, floatp)),
		gx_path_add_arc(P8(gx_path *, fixed, fixed, fixed, fixed, fixed, fixed, floatp)),
		gx_path_add_path(P2(gx_path *, gx_path *)),
		gx_path_close_subpath(P1(gx_path *));
/* The last argument to gx_path_add_arc is a fraction for computing */
/* the curve parameters.  Here is the correct value for quarter-circles. */
/* (stroke uses this to draw round caps and joins.) */
#define quarter_arc_fraction 0.552285

/* Path accessors */

extern	int	gx_path_current_point(P2(gx_path *, gs_fixed_point *)),
		gx_path_bbox(P2(gx_path *, gs_fixed_rect *)),
		gx_path_has_curves(P1(gx_path *)),
		gx_path_is_void(P1(gx_path *)),
		gx_path_is_rectangle(P2(gx_path *, gs_fixed_rect *));

/* Path transformers */

extern	int	gx_path_copy(P2(gx_path * /*old*/, gx_path * /*new*/)),
		gx_path_flatten(P3(gx_path * /*old*/, gx_path * /*new*/, floatp)),
		gx_path_reverse(P2(gx_path * /*old*/, gx_path * /*new*/)),
		gx_path_translate(P3(gx_path *, fixed, fixed));

/* ------ Clipping paths ------ */

/* Opaque type for a path */
typedef struct gx_clip_path_s gx_clip_path;
extern const uint gs_clip_path_sizeof;

extern	int	gx_clip_to_rectangle(P2(gs_state *, gs_fixed_rect *)),
		gx_clip_to_path(P1(gs_state *)),
		gx_cpath_from_rectangle(P3(gx_clip_path *, gs_fixed_rect *, gs_memory_procs *)),
		gx_cpath_intersect(P4(gs_state *, gx_clip_path *, gx_path *, int));
extern	void	gx_cpath_release(P1(gx_clip_path *)),
		gx_cpath_share(P1(gx_clip_path *));
extern	int	gx_cpath_path(P2(gx_clip_path *, gx_path *)),
		gx_cpath_box_for_check(P2(gx_clip_path *, gs_fixed_rect *)),
		gx_cpath_includes_rectangle(P5(gx_clip_path *, fixed, fixed, fixed, fixed));

/* Opaque type for a clip list. */
typedef struct gx_clip_list_s gx_clip_list;

/* Initialize a clip list. */
extern	void	gx_clip_list_init(P1(gx_clip_list *));
/* Initialize a clip list to a rectangle. */
extern	void	gx_clip_list_from_rectangle(P2(gx_clip_list *, gs_fixed_rect *));
/* Add a clip list to a path. */
extern	int	gx_clip_list_add_to_path(P2(gx_clip_list *, gx_path *));
/* Free a clip list. */
extern	void	gx_clip_list_free(P2(gx_clip_list *, gs_memory_procs *));
