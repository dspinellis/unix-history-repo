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

/* errors.h */
/* Define error codes for GhostScript */

/* A procedure that may return an error always returns */
/* a non-negative value (zero, unless otherwise noted) for success, */
/* or negative for failure. */
/* We use ints rather than an enum to avoid a lot of casting. */

/* The following peculiar structure allows us to include this file */
/* wherever error code definitions are needed, and use the same file */
/* to generate the table of error names by setting INCLUDE_ERROR_NAMES. */

#		ifdef INCLUDE_ERROR_NAMES

/* Define the error name table */
const char _ds *gs_error_names[] = {
#define _e_(code,name) name,

#		else			/* !INCLUDE_ERROR_NAMES */

extern const char _ds *gs_error_names[];
#  define _e_(code,name)

#endif					/* (!)INCLUDE_ERROR_NAMES */

		/* ------ PostScript Level 1 errors ------ */

#define e_unknownerror (-1)		/* unknown error */
  _e_(e_unknown, "unknownerror")
#define e_dictfull (-2)
  _e_(e_dictfull, "dictfull")
#define e_dictstackoverflow (-3)
  _e_(e_dictstackoverflow, "dictstackoverflow")
#define e_dictstackunderflow (-4)
  _e_(e_dictstackunderflow, "dictstackunderflow")
#define e_execstackoverflow (-5)
  _e_(e_execstackoverflow, "execstackoverflow")
#define e_interrupt (-6)
  _e_(e_interrupt, "interrupt")
#define e_invalidaccess (-7)
  _e_(e_invalidaccess, "invalidaccess")
#define e_invalidexit (-8)
  _e_(e_invalidexit, "invalidexit")
#define e_invalidfileaccess (-9)
  _e_(e_invalidfileaccess, "invalidfileaccess")
#define e_invalidfont (-10)
  _e_(e_invalidfont, "invalidfont")
#define e_invalidrestore (-11)
  _e_(e_invalidrestore, "invalidrestore")
#define e_ioerror (-12)
  _e_(e_ioerror, "ioerror")
#define e_limitcheck (-13)
  _e_(e_limitcheck, "limitcheck")
#define e_nocurrentpoint (-14)
  _e_(e_nocurrentpoint, "nocurrentpoint")
#define e_rangecheck (-15)
  _e_(e_rangecheck, "rangecheck")
#define e_stackoverflow (-16)
  _e_(e_stackoverflow, "stackoverflow")
#define e_stackunderflow (-17)
  _e_(e_stackunderflow, "stackunderflow")
#define e_syntaxerror (-18)
  _e_(e_syntaxerror, "syntaxerror")
#define e_timeout (-19)
  _e_(e_timeout, "timeout")
#define e_typecheck (-20)
  _e_(e_typecheck, "typecheck")
#define e_undefined (-21)
  _e_(e_undefined, "undefined")
#define e_undefinedfilename (-22)
  _e_(e_undefinedfilename, "undefinedfilename")
#define e_undefinedresult (-23)
  _e_(e_undefinedresult, "undefinedresult")
#define e_unmatchedmark (-24)
  _e_(e_unmatchedmark, "unmatchedmark")
#define e_VMerror (-25)
  _e_(e_VMerror, "VMerror")

		/* ------ Additional Level 2 and DPS errors ------ */

#define e_invalidcontext (-26)
  _e_(e_invalidcontext, "invalidcontext")
#define e_undefinedresource (-27)	/* not used yet */
  _e_(e_undefinedresource, "undefinedresource")

#		ifdef INCLUDE_ERROR_NAMES

/* End of error name table */
  0
};

#		endif			/* INCLUDE_ERROR_NAMES */
