/* Copyright (C) 1989, 1991 Aladdin Enterprises.  All rights reserved.
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

/* state.h */
/* Ghostscript interpreter graphics state definition */

/* Note that from the interpreter's point of view, */
/* the graphics state is opaque, i.e. the interpreter is */
/* just another client of the library. */

/* The interpreter requires additional items in the graphics state: */
typedef struct int_gstate_s int_gstate;
struct int_gstate_s {
		/* Any refs must come first, for alignment. */
	ref screen_proc;		/* halftone screen procedure */
	struct {
		ref red, green, blue, gray;	/* transfer procedures */
	} transfer_procs;
	ref font;			/* font object (dictionary) */
	int_gstate *saved;		/* previous int_gstate from gsave */
};
/* Enumerate the refs in an int_gstate. */
#define int_gstate_map_refs(p,m)\
 (m(&(p)->screen_proc), m(&(p)->transfer_procs.red),\
  m(&(p)->transfer_procs.green), m(&(p)->transfer_procs.blue),\
  m(&(p)->font))

/* The current instances. */
extern int_gstate istate;
extern gs_state *igs;
