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

/* gzstate.h */
/* Private graphics state definition for Ghostscript library */
/* Requires gxmatrix.h and gxfixed.h */
#include "gsstate.h"

/* Graphics state structure.  See pp. 59-60 of the PostScript manual. */
struct gs_state_s {
	gs_state *saved;		/* previous state from gsave */
	gs_memory_procs memory_procs;
/* Transformation: */
	gs_matrix_fixed ctm;
#define ctm_only(pgs) *(gs_matrix *)&(pgs)->ctm
	gs_matrix ctm_inverse;
	int inverse_valid;		/* true if ctm_inverse = ctm^-1 */
/* Paths: */
	struct gx_path_s *path;
	struct gx_clip_path_s *clip_path;
	int clip_rule;
/* Lines: */
	struct line_params_s *line_params;
/* Halftone screen: */
	struct halftone_params_s *halftone;
	float (*ht_proc)(P2(floatp, floatp));
	gs_int_point ht_phase;
	gs_int_point phase_mod;		/* negated phase mod tile size */
/* Colors: */
	struct gs_color_s *color;
	struct gx_device_color_s *dev_color;
	struct gx_transfer_s *transfer;
/* Font: */
	struct gs_font_s *font;
	gs_matrix char_tm;		/* font matrix * ctm */
	int char_tm_valid;		/* true if char_tm is valid */
	byte in_cachedevice;		/* true after a setcachedevice */
	byte in_charpath;		/* 0 if not in charpath, */
					/* 1 if false charpath, */
					/* 2 if true charpath */
					/* (see charpath_flag in */
					/* gs_show_enum_s) */
/* Other stuff: */
	int level;			/* incremented by 1 per gsave */
	float flatness;
	int stroke_adjust;
	struct device_s *device;
	int device_is_shared;		/* true if device is shared, */
					/* so don't deallocate at grestore */
};
