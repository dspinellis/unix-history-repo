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

/* oper.h */
/* Definitions for Ghostscript operators */
#include "ostack.h"
#include "opdef.h"
#include "gsutil.h"
#include "iutil.h"

/* Structure for initializing variables that hold name constants. */
typedef struct {
	const char _ds *vname;
	ref _ds *pvref;
} names_def;
#define names_def_end {(char _ds *)0, (ref _ds *)0}
extern void init_names(P1(const names_def _ds *));

/* Operand stack manipulation */
/* The most efficient code is different depending on the compiler.... */
#ifdef __MSDOS__			/* stupid compilers */
#define push(n)\
  if ( (op += (n)) > ostop ) return (e_stackoverflow); else osp += (n)
#else					/* reasonable compiler */
#define push(n)\
  if ( (osp = op += (n)) > ostop ) return (osp -= (n), e_stackoverflow)
#endif
/*
 * Note that the pop macro only decrements osp, not op.  For this reason,
 *
 *	>>>	pop should only be used just before returning,	<<<
 *	>>>	or else op must be decremented explicitly.	<<<
 */
#define pop(n) (osp -= (n))
/*
 * Note that the interpreter does not check for operand stack underflow
 * before calling the operator procedure.  There are "guard" entries
 * with invalid types and attributes just below the bottom of the
 * operand stack: if the operator returns with a typecheck error,
 * the interpreter checks for underflow at that time.
 * Operators that don't typecheck their arguments must check for
 * operand stack underflow explicitly.
 */
#define os_max_nargs 6
extern os_ptr osp_nargs[os_max_nargs];
#define op_nargs_check(nargs) osp_nargs[(nargs) - 1]
#define check_op(nargs)\
  if ( op < op_nargs_check(nargs) ) return e_stackunderflow

/* Check type */
#define check_type(rf,typ)\
  if ( !r_has_type(&rf,typ) ) return e_typecheck
/* Check for array */
#define check_array_else(rf,err)\
  if ( !r_has_type(&rf, t_array) ) return err
#define check_array(rf) check_array_else(rf, e_typecheck)
/* Check for procedure */
#define check_proc(rf)\
  switch ( r_type_xe(&rf) )\
   { default:\
	return(r_has_attrs(&rf, a_execute + a_executable) ? e_typecheck :\
	       e_invalidaccess);\
     case type_xe_value(t_array, a_execute + a_executable):\
     case type_xe_value(t_mixedarray, a_execute + a_executable):\
     case type_xe_value(t_shortarray, a_execute + a_executable): ;\
   }

/* Check for read, write, or execute access. */
#define check_access(rf,acc1)\
  if ( !r_has_attr(&rf,acc1) ) return e_invalidaccess
#define check_read(rf) check_access(rf,a_read)
#define check_type_access(rf,typ,acc1)\
  if ( !r_has_type_attrs(&rf,typ,acc1) )\
    return (r_has_attr(&rf,acc1) ? e_typecheck : e_invalidaccess)
#define check_read_type(rf,typ) check_type_access(rf,typ,a_read)
#define check_write(rf) check_access(rf,a_write)
#define check_write_type(rf,typ) check_type_access(rf,typ,a_write)
#define check_execute(rf) check_access(rf,a_execute)

/* Macro for as yet unimplemented operators. */
/* The if ( 1 ) is to prevent the compiler from complaining about */
/* unreachable code. */
#define NYI(msg) if ( 1 ) return e_undefined

/*
 * If an operator has popped or pushed something on the control stack,
 * it must return o_pop_estack or o_push_estack respectively,
 * rather than 0, to indicate success.
 * It is OK to return o_pop_estack if nothing has been popped,
 * but it is not OK to return o_push_estack if nothing has been pushed.
 *
 * If an operator has suspended the current context and wants the
 * interpreter to call the scheduler, it must return o_reschedule.
 * It may also have pushed or popped elements on the control stack.
 * (This is only used when the Display PostScript option is included.)
 *
 * These values must be positive, and far enough apart from zero and
 * from each other not to tempt a compiler into implementing a 'switch'
 * on them using indexing rather than testing.
 */
#define o_push_estack 3
#define o_pop_estack 8
#define o_reschedule 14
