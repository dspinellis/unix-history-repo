/* Copyright (C) 1992 Aladdin Enterprises.  All rights reserved.
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

/* dstack.h */
/* Direct access to the Ghostscript dictionary stack */

/********************************
 * NOTE: on MS-DOS systems, the dict stack is stored in the data segment.
 * This leads to large performance gains, at the expense of having to swap
 * the stack explicitly when switching contexts or handling segment under-
 * or overflow (none of which are implemented yet!).
 ********************************/

/* Define the dictionary stack and the standard dictionaries. */
extern ref dstack[];
#define systemdict (dstack[0])
#define userdict (dstack[1])

/* Define the dictionary stack pointers. */
typedef ref _ds *ds_ptr;
extern ds_ptr dsp, dstop;

/* Define a special fast entry for name lookup in the interpreter. */
/* The key is known to be a name; search the entire dict stack. */
/* Return the pointer to the value slot. */
/* If the name isn't found, just return 0. */
extern ref *dict_find_name(P1(ref *pname));
