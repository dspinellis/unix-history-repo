/* Copyright (C) 1991 Aladdin Enterprises.  All rights reserved.
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

/* ostack.h */
/* Definitions for Ghostscript operand stack */

/********************************
 * NOTE: on MS-DOS systems, the operand stack is stored in the data segment.
 * This leads to large performance gains, at the expense of having to swap
 * the stack explicitly when switching contexts or handling segment under-
 * or overflow (none of which are implemented yet!).
 ********************************/
typedef ref _ds *os_ptr;
extern os_ptr osbot, osp, ostop;

/* Macro to ensure enough room on the operand stack */
#define check_ostack(n)\
  if ( ostop - osp < (n) ) return e_stackoverflow
