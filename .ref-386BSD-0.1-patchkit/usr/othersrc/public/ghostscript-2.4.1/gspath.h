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

/* gspath.h */
/* Path manipulation interface for GhostScript library */
/* Requires gsstate.h */

/* Path constructors */
extern	int	gs_newpath(P1(gs_state *)),
		gs_moveto(P3(gs_state *, floatp, floatp)),
		gs_rmoveto(P3(gs_state *, floatp, floatp)),
		gs_lineto(P3(gs_state *, floatp, floatp)),
		gs_rlineto(P3(gs_state *, floatp, floatp)),
		gs_arc(P6(gs_state *, floatp, floatp, floatp, floatp, floatp)),
		gs_arcn(P6(gs_state *, floatp, floatp, floatp, floatp, floatp)),
		gs_arcto(P7(gs_state *, floatp, floatp, floatp, floatp, floatp, float [4])),
		gs_curveto(P7(gs_state *, floatp, floatp, floatp, floatp, floatp, floatp)),
		gs_rcurveto(P7(gs_state *, floatp, floatp, floatp, floatp, floatp, floatp)),
		gs_closepath(P1(gs_state *));

/* Add the current path to the path in the previous graphics state. */
extern	int	gs_upmergepath(P1(gs_state *));

/* Path accessors and transformers */
extern	int	gs_currentpoint(P2(const gs_state *, gs_point *)),
		gs_pathbbox(P2(gs_state *, gs_rect *)),
		gs_flattenpath(P1(gs_state *)),
		gs_reversepath(P1(gs_state *)),
		gs_strokepath(P1(gs_state *));

/* Path enumeration */
#define gs_pe_moveto 1
#define gs_pe_lineto 2
#define gs_pe_curveto 3
#define gs_pe_closepath 4
typedef struct gs_path_enum_s gs_path_enum;
extern const uint gs_path_enum_sizeof;
void	gs_path_enum_init(P2(gs_path_enum *, const gs_state *));
int	gs_path_enum_next(P2(gs_path_enum *, gs_point [3])); /* 0 when done */

/* Clipping */
extern	int	gs_clippath(P1(gs_state *)),
		gs_initclip(P1(gs_state *)),
		gs_clip(P1(gs_state *)),
		gs_eoclip(P1(gs_state *));
