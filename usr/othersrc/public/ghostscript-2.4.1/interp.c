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

/* interp.c */
/* Ghostscript language interpreter */
#include "memory_.h"
#include "ghost.h"
#include "errors.h"
#include "estack.h"
#include "name.h"
#include "dict.h"
#include "dstack.h"
#include "oper.h"
#include "packed.h"
#include "save.h"
#include "stream.h"
#include "store.h"

/* Imported operator procedures */
extern int obj_le(P2(os_ptr, os_ptr));
extern int zop_add(P1(os_ptr));
extern int zop_def(P1(os_ptr));
extern int zop_sub(P1(os_ptr));

/* The procedure to call if an operator requests rescheduling. */
/* This causes an error unless the context machinery has been installed. */
private int no_reschedule() { return e_invalidcontext; }
int (*gs_interp_reschedule_proc)(P0()) = no_reschedule;
/* The procedure to call for time-slicing. */
/* This is a no-op unless the context machinery has been installed. */
private int no_time_slice() { return 0; }
int (*gs_interp_time_slice_proc)(P0()) = no_time_slice;

/* Forward references */
private int interp(P1(ref *pref));
private int interp_exit(P1(os_ptr));
private int i_interp_exit;
private int copy_stack(P3(ref *, uint, ref *));

/* Configuration parameters */
#define max_ostack 800
#define max_estack 150
#define max_dstack 20
#define num_clear_dstack 2

/* See estack.h for a description of the execution stack. */

/* The logic for managing icount and iref below assumes that */
/* there are no control operators which pop and then push */
/* information on the execution stack. */

/* Stacks */
#define os_guard_under 10
#define os_guard_over 10
private ref ostack[os_guard_under+max_ostack+os_guard_over];
private ref estack[max_estack];
ref dstack[max_dstack];
os_ptr osp_nargs[os_max_nargs];		/* for checking osp */

/* Stack pointers */
os_ptr osbot, osp, ostop;
es_ptr esbot, esp, estop;
es_ptr esfile;				/* cache pointer to currentfile */
ds_ptr dsp, dstop;

/* The object that caused an error */
ref error_object;

/* Object related to error handling */
extern ref name_errordict;
extern ref name_ErrorNames;

/* Extended types.  The interpreter may replace the type of operators */
/* in procedures with these, to speed up the interpretation loop. */
#define tx_op t_next_index
extern int zadd(P1(os_ptr));
extern int zdef(P1(os_ptr));
extern int zdup(P1(os_ptr));
extern int zexch(P1(os_ptr));
extern int zif(P1(os_ptr));
extern int zifelse(P1(os_ptr));
extern int zle(P1(os_ptr));
extern int zpop(P1(os_ptr));
extern int zroll(P1(os_ptr));
extern int zsub(P1(os_ptr));
private op_proc_p special_ops[] = {
	zadd, zdef, zdup, zexch, zif, zifelse, zle, zpop, zroll, zsub
};
typedef enum {
	tx_op_add = tx_op,
	tx_op_def,
	tx_op_dup,
	tx_op_exch,
	tx_op_if,
	tx_op_ifelse,
	tx_op_le,
	tx_op_pop,
	tx_op_roll,
	tx_op_sub,
	tx_next_op
} special_op_types;
#define num_special_ops ((int)tx_next_op - tx_op)
#define t_invalid tx_next_op		/* first invalid type */

/* The following is a special "error" code that is used internally */
/* to cause the interpreter to exit. */
#define e_InterpreterExit (-100)

/* Initialize the interpreter */
void
interp_init(int ndict)
{	/* Initialize the guard entries on the operand stack */
	/* with objects that have invalid type and attributes. */
	osbot = ostack + os_guard_under;
	osp = osbot - 1, ostop = osbot + (max_ostack-1);
	   {	register os_ptr op;
		for ( op = ostack; op < osbot; op++ )
			make_tav(op, t_invalid, 0, index, 0);
	   }
	   {	register int i;
		for ( i = 1; i <= os_max_nargs; i++ )
			op_nargs_check(i) = osbot + i - 1;
	   }
	esbot = estack, esp = estack - 1, estop = estack + (max_estack-1);
	esfile = 0;
	/* Initialize the dictionary stack to the first ndict */
	/* dictionaries.  ndict is a parameter because during */
	/* initialization, only systemdict exists. */
	dsp = dstack + ndict - 1, dstop = dstack + (max_dstack-1);
}

/* Look up an operator during initialization, */
/* changing its type if appropriate. */
void
interp_fix_op(ref *opref)
{	register int i = num_special_ops;
	op_proc_p proc = real_opproc(opref);
	while ( --i >= 0 && proc != special_ops[i] ) ;
	if ( i >= 0 )
	  make_tav(opref, tx_op + i, a_executable, opproc,
		   (dummy_op_proc_p)proc);
}

/* Invoke the interpreter.  If execution completes normally, return 0. */
/* if an error occurs, then if user_errors is true and the error is a */
/* recoverable one (not an overflow condition), let the user handle it; */
/* otherwise, return the error code. */
int
gs_interpret(ref *pref, int user_errors)
{	ref *epref = pref;
	ref erref;
	ref *perrordict, *pErrorNames;
	int code, ccode;
	ref saref;
	/* Push a special exit procedure on the execution stack */
	es_ptr esp0 = ++esp;
	make_oper(esp0, i_interp_exit, (dummy_op_proc_p)interp_exit);
retry:	code = interp(epref);
	if ( code == e_InterpreterExit ) return 0;
	/* Adjust osp in case of operand stack underflow */
	if ( osp < osbot - 1 )
		osp = osbot - 1;
	if ( !user_errors ) return code;
	if ( dict_find(&systemdict, &name_errordict, &perrordict) <= 0 ||
	     dict_find(&systemdict, &name_ErrorNames, &pErrorNames) <= 0
	   )
		return code;	/* errordict or ErrorNames not found?? */
	switch ( code )
	   {
	case e_dictstackoverflow:
		if ( osp + 1 >= ostop ) return e_stackoverflow;
		ccode = copy_stack(dstack, dsp - dstack + 1, &saref);
		if ( ccode < 0 ) return ccode;
		dsp = &dstack[num_clear_dstack - 1];
		*++osp = saref;
		break;
	case e_execstackoverflow:
		if ( osp + 1 >= ostop ) return e_stackoverflow;
		ccode = copy_stack(estack, esp - estack + 1, &saref);
		if ( ccode < 0 ) return ccode;
		esp = esp0;
		*++osp = saref;
		break;
	case e_stackoverflow:
		ccode = copy_stack(ostack, osp - osbot + 1, &saref);
		if ( ccode < 0 ) return ccode;
		osp = osbot;
		*osbot = saref;
		break;
	   }
	if ( -code > r_size(pErrorNames) )
		return code;		/* unknown error??? */
	if ( dict_find(perrordict, &pErrorNames->value.refs[-code - 1], &epref) <= 0 )
		return code;		/* error name not in errordict??? */
	erref = *epref;
	epref = &erref;
	/* Push the error object on the operand stack */
	*++osp = error_object;
	goto retry;
}	
private int
interp_exit(os_ptr op)
{	return e_InterpreterExit;
}

/* Copy the contents of an overflowed stack into an array. */
private int
copy_stack(ref *stk, uint size, ref *arr)
{	ref *abody = alloc_refs(size, "overflowed stack");
	if ( abody == 0 ) return e_VMerror;
	refcpy_to_new(abody, stk, size);
	make_tasv(arr, t_array, a_all, size, refs, abody);
	return 0;
}

/* Main interpreter. */
/* If execution terminates normally, return e_InterpreterExit. */
/* If an error occurs, leave the current object in error_object */
/* and return a (negative) error code. */
#define return_with_error(code, objp)\
  { esp = iesp; osp = iosp; error_object = *(objp); return_error(code); }
private int
interp(ref *pref /* object to interpret */)
{	register ref *iref = pref;
	register int icount = 0;	/* # of consecutive tokens at iref */
	register os_ptr iosp = osp;	/* private copy of osp */
	register es_ptr iesp = esp;	/* private copy of esp */
	int code;
	ref token;		/* token read from file or string, */
				/* must be declared in this scope */
	register ref *pvalue;
	os_ptr whichp;
	esfile = 0;		/* clear cache */
	/* We want to recognize executable arrays here, */
	/* so we push the argument on the estack and enter */
	/* the loop at the bottom. */
	if ( iesp >= estop ) return_with_error (e_execstackoverflow, pref);
	*++iesp = *pref;
	goto bot;
	/* At this point, if icount > 0, iref and icount correspond */
	/* to the top entry on the execution stack: icount is the */
	/* count of sequential entries remaining AFTER the current one. */
#define add1_short(pref) (ref *)((ushort *)(pref) + 1)
#define store_state(ep)\
  ( icount > 0 ? (ep->value.refs = iref + 1, r_set_size(ep, icount)) : 0 )
#define store_state_short(ep)\
  ( icount > 0 ? (ep->value.refs = add1_short(iref), r_set_size(ep, icount)) : 0 )
#define next()\
  if ( --icount > 0 ) { iref++; goto top; } else goto out
#define next_short()\
  if ( --icount <= 0 ) { if ( icount < 0 ) goto up; iesp--; }\
  iref = add1_short(iref); goto top;
top:	/*
	 * This is the top of the interpreter loop.
	 * iref points to the ref being interpreted.
	 * Note that this might be an element of a packed array,
	 * not a real ref: we carefully arranged the first 16 bits of
	 * a ref and of a packed array element so they could be distinguished
	 * from each other.  (See ghost.h and packed.h for more detail.)
	 */
#ifdef DEBUG
if ( gs_debug['I'] || gs_debug['i'] &&
     (*(ushort *)iref <= packed_max_full_ref ? r_type(iref) == t_name :
      *(short *)iref < 0)
   )
   {	void debug_print_ref(P1(ref *));
	int edepth = iesp - esbot;
	char depth[10];
	sprintf(depth, "%2d", edepth);
	dputs(depth);
	edepth -= strlen(depth);
	do { dputc('.'); } while ( --edepth > 0 );	/* indent */
	dprintf3("%lx(%2d)<%2d>: ",
		 (ulong)iref, icount, (uint)(iosp - osbot + 1));
	debug_print_ref(iref);
	if ( iosp >= osbot )
	   {	dputs(" // ");
		debug_print_ref(iosp);
	   }
	dputc('\n');
	fflush(dstderr);
   }
#endif
/* Object that have attributes (arrays, dictionaries, files, and strings) */
/* use lit and exec; other objects use plain and plain_exec. */
#define lit(t) type_xe_value(t, a_execute)
#define exec(t) type_xe_value(t, a_execute + a_executable)
#define nox(t) type_xe_value(t, 0)
#define nox_exec(t) type_xe_value(t, a_executable)
#define plain(t) type_xe_value(t, 0)
#define plain_exec(t) type_xe_value(t, a_executable)
	/*
	 * We have to populate enough cases of the switch statement to force
	 * some compilers to use a dispatch rather than a testing loop.
	 * What a nuisance!
	 */
	switch ( r_type_xe(iref) )
	   {
	/* Access errors. */
#define cases_nox()\
  case nox_exec(t_array): case nox_exec(t_dictionary):\
  case nox_exec(t_file): case nox_exec(t_string):\
  case nox_exec(t_mixedarray): case nox_exec(t_shortarray)
	cases_nox():
		return_with_error (e_invalidaccess, iref);
	/*
	 * Literal objects.  We have to enumerate all the types.
	 * In fact, we have to include some extra plain_exec entries
	 * just to populate the switch.  We break them up into groups
	 * to avoid overflowing some preprocessors.
	 */
#define cases_lit_1()\
  case lit(t_array): case nox(t_array):\
  case plain(t_boolean): case plain_exec(t_boolean):\
  case plain(t_condition): case plain_exec(t_condition):\
  case lit(t_dictionary): case nox(t_dictionary)
#define cases_lit_2()\
  case lit(t_file): case nox(t_file):\
  case plain(t_fontID): case plain_exec(t_fontID):\
  case plain(t_gstate): case plain_exec(t_gstate):\
  case plain(t_integer): case plain_exec(t_integer)
#define cases_lit_3()\
  case plain(t_lock): case plain_exec(t_lock):\
  case plain(t_mark): case plain_exec(t_mark):\
  case plain(t_name):\
  case plain(t_null):\
  case plain(t_oparray):\
  case plain(t_operator)
#define cases_lit_4()\
  case plain(t_real): case plain_exec(t_real):\
  case plain(t_save): case plain_exec(t_save):\
  case lit(t_string): case nox(t_string)
#define cases_lit_5()\
  case lit(t_mixedarray): case nox(t_mixedarray):\
  case lit(t_shortarray): case nox(t_shortarray):\
  case plain(t_color): case plain_exec(t_color):\
  case plain(t_device): case plain_exec(t_device)
	cases_lit_1():
	cases_lit_2():
	cases_lit_3():
	cases_lit_4():
	cases_lit_5():
		break;
	/* Special operators. */
	case plain_exec(tx_op_add):
x_add:		if ( (code = zop_add(iosp)) < 0 )
			return_with_error (code, iref);
		iosp--;
		next();
	case plain_exec(tx_op_def):
x_def:		if ( (code = zop_def(iosp)) < 0 )
			return_with_error (code, iref);
		iosp -= 2;
		next();
	case plain_exec(tx_op_dup):
x_dup:		if ( iosp < op_nargs_check(1) )
			return_with_error (e_stackunderflow, iref);
		iosp++;
		ref_assign(iosp, iosp - 1);
		next();
	case plain_exec(tx_op_exch):
x_exch:		if ( iosp < op_nargs_check(2) )
			return_with_error (e_stackunderflow, iref);
		ref_assign(&token, iosp);
		ref_assign(iosp, iosp - 1);
		ref_assign(iosp - 1, &token);
		next();
	case plain_exec(tx_op_if):
x_if:		if ( !r_has_type(iosp - 1, t_boolean) )
		  return_with_error (e_typecheck, iref);
		if ( !iosp[-1].value.index )
		  { iosp -= 2;
		    next();
		  }
		if ( iesp >= estop )
		  return_with_error (e_execstackoverflow, iref);
		store_state(iesp);
		whichp = iosp;
		iosp -= 2;
		goto ifup;
	case plain_exec(tx_op_ifelse):
x_ifelse:	if ( !r_has_type(iosp - 2, t_boolean) )
			return_with_error (e_typecheck, iref);
		if ( iesp >= estop )
			return_with_error (e_execstackoverflow, iref);
		store_state(iesp);
		whichp = (iosp[-2].value.index ? iosp - 1 : iosp);
		iosp -= 3;
		/* Open code "up" for the array case(s) */
ifup:		switch( r_type_xe(whichp) )
		   {
		case exec(t_file):
			esfile = 0;		/* clear currentfile cache */
		default:
			ref_assign(iesp + 1, whichp);
			iref = iesp + 1;
			icount = 0;
			goto top;
		case exec(t_array):
		case exec(t_mixedarray):
		case exec(t_shortarray): ;
		   }
		if ( (icount = r_size(whichp) - 1) <= 0 )
		   {	if ( icount < 0 ) goto up;	/* 0-element proc */
			iref = whichp->value.refs;	/* 1-element proc */
			goto top;
		   }
		++iesp;
		/* Do a ref_assign, but also set iref. */
		iesp->tas = whichp->tas;
		iref = iesp->value.refs = whichp->value.refs;
		goto top;
	case plain_exec(tx_op_le):
x_le:		code = obj_le(iosp - 1, iosp);
		if ( code < 0 )
			return_with_error (code, iref);
		iosp--;
		make_bool(iosp, code);
		next();
	case plain_exec(tx_op_pop):
x_pop:		if ( iosp < op_nargs_check(1) )
			return_with_error (e_stackunderflow, iref);
		iosp--;
		next();
	case plain_exec(tx_op_roll):
x_roll:		if ( (code = zroll(iosp)) < 0 )
			return_with_error (code, iref);
		iosp -= 2;
		next();
	case plain_exec(tx_op_sub):
x_sub:		if ( (code = zop_sub(iosp)) < 0 )
			return_with_error (code, iref);
		iosp--;
		next();
	/* Executable types. */
	case plain_exec(t_null):
		goto bot;
	case plain_exec(t_oparray):
		/* Replace with the definition and go again. */
		pvalue =
		  &op_array_table.value.refs[op_index(iref) - op_def_count];
prst:		/* Prepare to call the procedure (array) in *pvalue. */
		store_state(iesp);
pr:		/* Call the array in *pvalue.  State has been stored. */
		if ( (icount = r_size(pvalue) - 1) <= 0 )
		   {	if ( icount < 0 ) goto up;	/* 0-element proc */
			iref = pvalue->value.refs;	/* 1-element proc */
			goto top;
		   }
		if ( iesp >= estop )
			return_with_error (e_execstackoverflow, pvalue);
		++iesp;
		/* Do a ref_assign, but also set iref. */
		iesp->tas = pvalue->tas;
		iref = iesp->value.refs = pvalue->value.refs;
		goto top;
	case plain_exec(t_operator):
	   {	esp = iesp;		/* save for operator */
		osp = iosp;		/* ditto */
		/* Operator routines take osp as an argument. */
		/* This is just a convenience, since they adjust */
		/* osp themselves to reflect the results. */
		/* Operators that (net) push information on the */
		/* operand stack must check for overflow: */
		/* this normally happens automatically through */
		/* the push macro (in oper.h). */
		/* Operators that do not typecheck their operands */
		/* must check explicitly for stack underflow. */
		/* Note that each case must set iosp = osp: */
		/* this is so we can switch on code without having to */
		/* store it and reload it (for dumb compilers). */
		switch ( code = (*real_opproc(iref))(iosp) )
		   {
		case 0:			/* normal case */
			iosp = osp;
			next();
		case o_push_estack:	/* store the state and go to up */
			iosp = osp;
			store_state(iesp);
			iesp = esp;
			goto up;
		case o_pop_estack:	/* just go to up */
			iosp = osp;
			if ( esp == iesp ) goto bot;
			iesp = esp;
			goto up;
		case o_reschedule:
			store_state(iesp);
			goto res;
		case e_typecheck:
			/* This might be an operand stack */
			/* underflow: check the required # of */
			/* operands now. */
			if ( osp < osbot - 1 + op_num_args(iref) )
				code = e_stackunderflow;
			/* (falls through) */
		   }
		iosp = osp;
		return_with_error (code, iref);
	   }
	case plain_exec(t_name):
		pvalue = iref->value.pname->pvalue;
		if ( !pv_valid(pvalue) )
		   {	ref *pdvalue;
			if ( (pdvalue = dict_find_name(iref)) == 0 )
				return_with_error (e_undefined, iref);
			pvalue = pdvalue;
		   }
		/* Dispatch on the type of the value. */
		/* Again, we have to over-populate the switch. */
		switch ( r_type_xe(pvalue) )
		   {
		cases_nox():	/* access errors */
			return_with_error (e_invalidaccess, iref);
		cases_lit_1():
		cases_lit_2():
		cases_lit_3():
		cases_lit_4():
		cases_lit_5():
			/* Just push the value */
			if ( iosp >= ostop )
				return_with_error (e_stackoverflow, pvalue);
			++iosp;
			ref_assign(iosp, pvalue);
			next();
		case exec(t_array):
		case exec(t_mixedarray):
		case exec(t_shortarray):
			/* This is an executable procedure, execute it. */
			goto prst;
		case plain_exec(tx_op_add): goto x_add;
		case plain_exec(tx_op_def): goto x_def;
		case plain_exec(tx_op_dup): goto x_dup;
		case plain_exec(tx_op_exch): goto x_exch;
		case plain_exec(tx_op_if): goto x_if;
		case plain_exec(tx_op_ifelse): goto x_ifelse;
		case plain_exec(tx_op_le): goto x_le;
		case plain_exec(tx_op_pop): goto x_pop;
		case plain_exec(tx_op_roll): goto x_roll;
		case plain_exec(tx_op_sub): goto x_sub;
		case plain_exec(t_null):
			goto bot;
		case plain_exec(t_oparray):
			pvalue =
			  &op_array_table.value.refs[op_index(pvalue) -
						     op_def_count];
			goto prst;
		case plain_exec(t_operator):
		   {	/* Shortcut for operators. */
			/* See above for the logic. */
			esp = iesp;
			osp = iosp;
			switch ( code = (*real_opproc(pvalue))(iosp) )
			   {
			case 0:			/* normal case */
				iosp = osp;
				next();
			case o_push_estack:	/* store the state and go to up */
				iosp = osp;
				store_state(iesp);
				iesp = esp;
				goto up;
			case o_pop_estack:	/* just go to up */
				iosp = osp;
				if ( esp == iesp ) goto bot;
				iesp = esp;
				goto up;
			case o_reschedule:
				store_state(iesp);
				goto res;
			case e_typecheck:
				if ( osp < osbot - 1 + op_num_args(pvalue) )
					code = e_stackunderflow;
			   }
			iosp = osp;
			return_with_error (code, pvalue);
		   }
		case plain_exec(t_name):
		case exec(t_file):
		case exec(t_string):
		default:
			/* Not a procedure, reinterpret it. */
			store_state(iesp);
			icount = 0;
			iref = pvalue;
			goto top;
		   }
	case exec(t_file):
	   {	/* Executable file.  Read the next token and interpret it. */
	   	stream *s;
		code = file_check_read(iref, &s);
		if ( code < 0 ) return_with_error (code, iref);
rt:		if ( iosp >= ostop )	/* check early */
		  return_with_error (e_stackoverflow, iref);
		osp = iosp;		/* scan_token uses ostack */
		switch ( code = scan_token(s, 0, (ref *)(iosp + 1)) )
		   {
		case 0:			/* read a token */
			/* It's worth checking for literals, which make up */
			/* the majority of input tokens, before storing the */
			/* state on the e-stack.  Note that because of //, */
			/* the token may have *any* type and attributes. */
			switch ( r_type(iosp + 1) )
			   {
			case t_name: case t_string:
				if ( r_has_attr(iosp + 1, a_executable) )
					break;
			/* Executable arrays aren't executed at the */
			/* top level -- they're treated as literals. */
			case t_array: case t_mixedarray: case t_shortarray:
			case t_integer: case t_real:
				++iosp;
				goto rt;
			   }
			store_state(iesp);
			/* Push the file on the e-stack */
			if ( iesp >= estop )
				return_with_error (e_execstackoverflow, iref);
			++iesp;
			ref_assign(iesp, iref);
			iref = iosp + 1;
			icount = 0;
			goto top;
		case 1:			/* end of file */
			code = file_close(iref, s);
			if ( code < 0 ) return_with_error (code, iref);
			goto bot;
		default:		/* error */
			return_with_error (code, iref);
		   }
	   }
	case exec(t_string):
	   {	/* Executable string.  Read a token and interpret it. */
		stream ss;
		sread_string(&ss, iref->value.bytes, r_size(iref));
		osp = iosp;		/* scan_token uses ostack */
		switch ( code = scan_token(&ss, 1, &token) )
		  {
		case 0:			/* read a token */
		    store_state(iesp);
		    /* Push the updated string back on the e-stack */
		    if ( iesp >= estop )
		      return_with_error (e_execstackoverflow, iref);
		    ++iesp;
		    iesp->tas.type_attrs = iref->tas.type_attrs;
		    iesp->value.bytes = ss.cptr + 1;
		    r_set_size(iesp, ss.cbuf + ss.bsize - ss.cptr - 1);
		    iref = &token;
		    icount = 0;
		    goto top;
		case 1:			/* end of string */
		    goto bot;
		default:		/* error */
		    return_with_error (code, iref);
		  }
	   }
	/* Handle packed arrays here by re-dispatching. */
	/* This also picks up some anomalous cases of non-packed arrays. */
	default:
		switch ( *(ushort *)iref >> packed_type_shift )
		   {
		case pt_full_ref:
		case pt_full_ref+1:
			if ( iosp >= ostop )
			  return_with_error (e_stackoverflow, iref);
			++iosp;
			/* We know that refs are properly aligned: */
			/* see packed.h for details. */
			ref_assign(iosp, iref);
			next();
		case pt_executable_operator:
		   {	uint index = *(ushort *)iref & packed_int_mask;
			op_index_ref(index, &token);
			store_state_short(iesp);
			icount = 0;
			iref = &token;
		   }	goto top;
		case pt_integer:
			if ( iosp >= ostop )
			  return_with_error (e_stackoverflow, iref);
			++iosp;
			make_int(iosp, (*(short *)iref & packed_int_mask) +
					packed_min_intval);
			next_short();
		case pt_literal_name:
		case pt_literal_name+1:
			if ( iosp >= ostop )
			  return_with_error (e_stackoverflow, iref);
			++iosp;
			name_index_ref((uint)*(ushort *)iref &
				         packed_max_name_index,
				       iosp);
			next_short();
		case pt_executable_name:
		case pt_executable_name+1:
		   {	ref nref;
			name_index_ref((uint)*(ushort *)iref &
					 packed_max_name_index,
				       &nref);
			pvalue = nref.value.pname->pvalue;
			if ( !pv_valid(pvalue) )
			   {	ref *pdvalue;
				if ( (pdvalue = dict_find_name(&nref)) == 0 )
				  return_with_error (e_undefined, &nref);
				pvalue = pdvalue;
			   }
			switch ( r_type_xe(pvalue) )
			   {
			case exec(t_array):
			case exec(t_mixedarray):
			case exec(t_shortarray):
				/* This is an executable procedure, */
				/* execute it. */
				store_state_short(iesp);
				goto pr;
			default:		/* handles other literals */
				/* Not a procedure, reinterpret it. */
				store_state_short(iesp);
				icount = 0;
				iref = pvalue;
				goto top;
			   }
		   }
		/* default can't happen here */
		   }
	   }
	/* Literal type, just push it. */
	if ( iosp >= ostop ) return_with_error (e_stackoverflow, iref);
	++iosp;
	ref_assign(iosp, iref);
bot:	next();
out:	/* At most 1 more token in the current procedure. */
	/* (We already decremented icount.) */
	if ( !icount )
	   {	/* Pop the execution stack for tail recursion. */
		iesp--;
		iref++;
		goto top;
	   }
up:	/* See if there is anything left on the execution stack. */
	switch ( r_type_xe(iesp) )
	   {
	default:
		iref = iesp--;
		icount = 0;
		goto top;
	case exec(t_array):
	case exec(t_mixedarray):
	case exec(t_shortarray): ;
	   }
	iref = iesp->value.refs;		/* next element of array */
	icount = r_size(iesp) - 1;
	if ( icount <= 0 )		/* <= 1 more elements */
	   {	iesp--;			/* pop, or tail recursion */
		if ( icount < 0 ) goto up;
	   }
	goto top;
res:	/* Some operator has asked for context rescheduling. */
	code = (*gs_interp_reschedule_proc)();
	if ( code < 0 ) return_with_error (code, iref);
	/* Reload state information from memory. */
	iosp = osp;
	iesp = esp;
	goto up;
}

/* ------ Initialization procedure ------ */

op_def interp_op_defs[] = {
		/* Internal operators */
	{"0%interp_exit", interp_exit, &i_interp_exit},
	op_def_end(0)
};
