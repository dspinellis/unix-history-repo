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

/* estack.h */
/* Definitions for Ghostscript execution stack */

/********************************
 * NOTE: on MS-DOS systems, the execution stack is stored in the data segment.
 * This leads to large performance gains, at the expense of having to swap
 * the stack explicitly when switching contexts or handling segment under-
 * or overflow (none of which are implemented yet!).
 ********************************/
typedef ref _ds *es_ptr;
extern es_ptr esbot, esp, estop;
/*
 * In the near future, we will cache the currentfile pointer
 * (i.e., `shallow-bind' it in Lisp terminology).  The invariant is as
 * follows: either esfile points to the currentfile slot on the estack
 * (i.e., the topmost slot with an executable file), or it is 0.
 * Note that the following algorithm suffices to maintain the invariant:
 * whenever a routine pushes or pops anything on the estack, if the object
 * *might* be an executable file, set esfile to 0.
 ****** NOT IMPLEMENTED YET ******
 */
extern es_ptr esfile;


/*
 * The execution stack is used for three purposes:
 *	- Procedures being executed are held here.  They always have
 * type = t_array, t_mixedarray, or t_shortarray, with a_executable set.
 *	- if, ifelse, etc. push arguments to be executed here.
 * They may be any kind of object whatever.
 *	- for, repeat, loop, forall, pathforall, run, and stopped
 * mark the stack by pushing an object with type = t_null,
 * attrs = a_executable, and value.index = 0 for loops, 1 for run/stopped.
 * (Executable null objects can't ever appear on the e-stack otherwise:
 * if a control operator pushes one, it gets popped immediately.)
 * The loop operators also push whatever state they need,
 * followed by an operator object that handles continuing the loop.
 */

/* Macro for marking the execution stack */
#define make_mark_estack(ep, idx)\
  make_tav(ep, t_null, a_executable, index, idx)
#define mark_estack(idx)\
  (++esp, make_mark_estack(esp, idx))

/* Macro for pushing an operator on the execution stack */
/* to represent a continuation procedure */
#define make_op_estack(ep, proc, idx)\
  make_oper(ep, idx, (dummy_op_proc_p)(proc))
#define push_op_estack(proc, idx)\
  (++esp, make_op_estack(esp, proc, idx))

/* Macro to ensure enough room on the execution stack */
#define check_estack(n)\
  if ( esp + (n) > estop ) return e_execstackoverflow

/* Define the various kinds of execution stack marks. */
#define es_other 0			/* internal use */
#define es_show 1			/* show operators */
#define es_for 2			/* iteration operators */
#define es_stopped 3			/* stopped operator */
