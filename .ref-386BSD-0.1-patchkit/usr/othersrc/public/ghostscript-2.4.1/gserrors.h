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

/* gserrors.h */
/* Error codes for GhostScript library */

/* A procedure that may return an error always returns */
/* a non-negative value (zero, unless otherwise noted) for success, */
/* or negative for failure. */
/* We use ints rather than an enum to avoid a lot of casting. */

#define gs_error_invalidfont (-10)
#define gs_error_ioerror (-12)
#define gs_error_limitcheck (-13)
#define gs_error_nocurrentpoint (-14)
#define gs_error_rangecheck (-15)
#define gs_error_undefined (-21)
#define gs_error_undefinedresult (-23)
#define gs_error_VMerror (-25)
