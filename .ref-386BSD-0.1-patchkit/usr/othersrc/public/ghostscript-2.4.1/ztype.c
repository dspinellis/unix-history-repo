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

/* ztype.c */
/* Type, attribute, and conversion operators for GhostScript */
#include "math_.h"
#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "dict.h"
#include "iutil.h"
#include "name.h"
#include "store.h"
#include "stream.h"

/* Imported procedures */
extern int scan_number_only(P2(ref *, ref *));

/* Forward references */
private int near access_check(P3(os_ptr, int, int));
private int convert_to_string(P2(os_ptr, os_ptr));

/* Max and min integer values expressed as reals. */
/* Note that these are biased by 1 to correct for truncation. */
#define lb_real_int (-1.0 * 0x8000L * 0x10000L - 1)
#define ub_real_int ( 1.0 * 0x8000L * 0x10000L)

/* Get the pointer to the access flags for a ref. */
#define access_ref(opp)\
  (r_has_type(opp, t_dictionary) ? dict_access_ref(opp) : opp)

/* Initialize the table of type names. */
/* We export the type names just in case they might be useful. */
ref type_names[t_next_index];
private void
ztype_init()
{	static const char *tnames[] = { type_name_strings };
	int i;
	for ( i = 0; i < t_next_index; i++ )
	   {	name_enter(tnames[i], &type_names[i]);
		r_set_attrs(&type_names[i], a_executable);
	   }
}

/* type */
int
ztype(register os_ptr op)
{	ref *ptref;
	check_op(1);
	ptref = &type_names[r_btype(op)];
	ref_assign(op, ptref);
	return 0;
}

/* cvlit */
int
zcvlit(register os_ptr op)
{	ref *aop;
	check_op(1);
	aop = access_ref(op);
	r_clear_attrs(aop, a_executable);
	return 0;
}

/* cvx */
int
zcvx(register os_ptr op)
{	ref *aop;
	check_op(1);
	aop = access_ref(op);
	r_set_attrs(aop, a_executable);
	return 0;
}

/* xcheck */
int
zxcheck(register os_ptr op)
{	check_op(1);
	make_bool(op, (r_has_attr(access_ref(op), a_executable) ? 1 : 0));
	return 0;
}

/* executeonly */
int
zexecuteonly(register os_ptr op)
{	check_op(1);
	if ( r_has_type(op, t_dictionary) ) return e_typecheck;
	return access_check(op, a_execute, 1);
}

/* noaccess */
int
znoaccess(register os_ptr op)
{	return access_check(op, 0, 1);
}

/* readonly */
int
zreadonly(register os_ptr op)
{	return access_check(op, a_read+a_execute, 1);
}

/* rcheck */
int
zrcheck(register os_ptr op)
{	int code = access_check(op, a_read, 0);
	if ( code >= 0 ) make_bool(op, code), code = 0;
	return code;
}

/* wcheck */
int
zwcheck(register os_ptr op)
{	int code = access_check(op, a_write, 0);
	if ( code >= 0 ) make_bool(op, code), code = 0;
	return code;
}

/* cvi */
int
zcvi(register os_ptr op)
{	float fval;
	switch ( r_type(op) )
	   {
	case t_integer: return 0;
	case t_real: fval = op->value.realval; break;
	default: return e_typecheck;
	case t_string:
	   {	ref nref;
		int code;
		code = scan_number_only(op, &nref);
		if ( code ) return code;	/* error condition */
		if ( r_has_type(&nref, t_integer) ) { *op = nref; return 0; }
		/* Otherwise, result was a real */
		fval = nref.value.realval;
	   }
	   }
	/* Check if a real will fit into an integer value */
	if ( fval <= lb_real_int || fval >= ub_real_int )
		return e_rangecheck;
	make_int(op, (long)fval);	/* truncates towards 0 */
	return 0;
}

/* cvn */
int
zcvn(register os_ptr op)
{	check_read_type(*op, t_string);
	return name_from_string(op, op);
}

/* cvr */
int
zcvr(register os_ptr op)
{	switch ( r_type(op) )
	   {
	case t_integer: make_real(op, op->value.intval);
	case t_real: return 0;
	default: return e_typecheck;
	case t_string:
	   {	ref nref;
		int code;
		code = scan_number_only(op, &nref);
		if ( code ) return code;	/* error condition */
		if ( r_has_type(&nref, t_real) ) { *op = nref; return 0; }
		/* Otherwise, result was an integer */
		make_real(op, nref.value.intval);
		return 0;
	   }
	   }
}

/* cvrs */
int
zcvrs(register os_ptr op)
{	int radix;
	check_type(op[-1], t_integer);
	if ( op[-1].value.intval < 2 || op[-1].value.intval > 36 )
		return e_rangecheck;
	radix = op[-1].value.intval;
	check_write_type(*op, t_string);
	if ( radix == 10 )
	   {	switch ( r_type(op - 2) )
		   {
		case t_integer: case t_real:
		   {	int code = convert_to_string(op - 2, op);
			if ( code < 0 ) return code;
			pop(2);
			return 0;
		   }
		default:
			return e_typecheck;
		   }
	   }
	else
	   {	ulong ival;
		byte digits[32];
		byte *endp = &digits[32];
		byte *dp = endp;
		switch ( r_type(op - 2) )
		   {
		case t_integer:
			ival = (ulong)op[-2].value.intval;
			break;
		case t_real:
		   {	float fval = op[-2].value.realval;
			if ( fval <= lb_real_int || fval >= ub_real_int )
				return e_rangecheck;
			ival = (ulong)(long)fval;
		   }	break;
		default:
			return e_typecheck;
		   }
		do
		   {	int dit = ival % radix;
			*--dp = dit + (dit < 10 ? '0' : ('A' - 10));
			ival /= radix;
		   }
		while ( ival );
		if ( endp - dp > r_size(op) ) return e_rangecheck;
		memcpy(op->value.bytes, dp, (uint)(endp - dp));
		r_set_size(op, endp - dp);
	   }
	op[-2] = *op;
	pop(2);
	return 0;
}

/* cvs */
int
zcvs(register os_ptr op)
{	int code;
	check_write_type(*op, t_string);
	code = convert_to_string(op - 1, op);
	if ( code >= 0 ) pop(1);
	return code;
}

/* ------ Initialization procedure ------ */

op_def ztype_op_defs[] = {
	{"1cvi", zcvi},
	{"1cvlit", zcvlit},
	{"1cvn", zcvn},
	{"1cvr", zcvr},
	{"3cvrs", zcvrs},
	{"2cvs", zcvs},
	{"1cvx", zcvx},
	{"1executeonly", zexecuteonly},
	{"1noaccess", znoaccess},
	{"1rcheck", zrcheck},
	{"1readonly", zreadonly},
	{"1type", ztype},
	{"1wcheck", zwcheck},
	{"1xcheck", zxcheck},
	op_def_end(ztype_init)
};

/* ------ Internal routines ------ */

/* Test or modify the access of an object. */
/* If modify = 1, restrict to the selected access and return 0; */
/* if modify = 0, do not change the access, and return 1 */
/* if the object had the access. */
/* Return an error code if the object is not of appropriate type, */
/* or if the object did not have the access already when modify=1. */
private int near
access_check(os_ptr op,
    int access,				/* mask for attrs */
    int modify)				/* if true, reduce access */
{	ref *aop = op;
	switch ( r_type(op) )
	   {
	default: return e_typecheck;
	case t_dictionary:
		aop = dict_access_ref(op);
	case t_array: case t_file: case t_gstate: case t_string:
	case t_mixedarray: case t_shortarray: ;
	   }
	if ( modify )
	   {	if ( !r_has_attrs(aop, access) )
			return e_invalidaccess;
		if ( aop != op )	/* i.e., t_dictionary */
		   {	ref_save(aop, "access_check(modify)");
		   }
		r_clear_attrs(aop, a_all);
		r_set_attrs(aop, access);
		return 0;
	   }
	else
	   {	return (r_has_attrs(aop, access)? 1 : 0);
	   }
}

/* Do all the work of cvs.  The destination has been checked, but not */
/* the source.  This is a separate procedure so that */
/* cvrs can use it when the radix is 10. */
private int
convert_to_string(os_ptr op1, os_ptr op)
{	int code;
	uint len;
	if ( r_has_type(op1, t_string) )
		check_read(*op1);
	code = obj_cvs(op1, op->value.bytes, r_size(op), &len);
	if ( code < 0 ) return code;
	*op1 = *op;
	r_set_size(op1, len);
	return 0;
}
