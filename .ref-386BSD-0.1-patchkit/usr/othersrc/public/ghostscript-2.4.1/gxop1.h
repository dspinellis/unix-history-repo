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

/* gxop1.h */
/* Type 1 state shared between interpreter and compiled fonts. */

/* Define the shared Type 1 interpreter state. */
#define max_coeff_bits 11		/* max coefficient in char space */
typedef struct gs_op1_state_s {
	struct gx_path_s *ppath;
	struct gs_type1_state_s *pis;
	fixed_coeff fc;
	fixed ctx, cty;			/* character origin (device space) */
	fixed px, py;			/* current point (char space) */
} gs_op1_state;
typedef gs_op1_state _ss *is_ptr;
