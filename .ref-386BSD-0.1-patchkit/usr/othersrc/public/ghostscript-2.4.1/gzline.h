/* Copyright (C) 1989 Aladdin Enterprises.  All rights reserved.
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

/* gxline.h */
/* Private line parameters for GhostScript */
/* Requires gsstate.h */

/* Line parameter structures */
typedef struct dash_params_s {
	float *pattern;
	uint pattern_size;
	float offset;
	/* The rest of the parameters are computed from the above */
	int init_ink_on;		/* true if ink is initially on */
	int init_index;			/* initial index in pattern */
	float init_dist_left;
} dash_params;
typedef struct line_params_s {
	float width;			/* one-half line width */
	gs_line_cap cap;
	gs_line_join join;
	float miter_limit;
	float miter_check;		/* computed from miter limit, */
					/* see gs_setmiterlimit and */
					/* gs_stroke */
	dash_params dash;
} line_params;
