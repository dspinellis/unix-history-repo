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

/* zdict.c */
/* Dictionary operators for GhostScript */
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "dict.h"
#include "dstack.h"
#include "store.h"

/* dict */
int
zdict(register os_ptr op)
{	check_type(*op, t_integer);
	if ( op->value.intval < 0 || op->value.intval > dict_max_size )
		return e_rangecheck;
	return dict_create((uint)op->value.intval, op);
}

/* maxlength */
int
zmaxlength(register os_ptr op)
{	check_type(*op, t_dictionary);
	check_dict_read(*op);
	make_int(op, dict_maxlength(op));
	return 0;
}

/* setmaxlength */
int
zsetmaxlength(register os_ptr op)
{	uint new_size;
	int code;
	os_ptr op1 = op - 1;
	check_type(*op1, t_dictionary);
	check_dict_write(*op1);
	check_type(*op, t_integer);
	if ( op->value.intval < 0 || op->value.intval > dict_max_size )
		return e_rangecheck;
	new_size = (uint)op->value.intval;
	if ( dict_length(op - 1) > new_size )
		return e_dictfull;
	code = dict_resize(op - 1, new_size);
	if ( code >= 0 ) pop(2);
	return code;
}

/* begin */
int
zbegin(register os_ptr op)
{	check_type(*op, t_dictionary);
	check_dict_read(*op);
	if ( dsp == dstop ) return e_dictstackoverflow;
	++dsp;
	ref_assign(dsp, op);
	pop(1);
	return 0;
}

/* end */
int
zend(register os_ptr op)
{	if ( dsp == dstack + 1 ) return e_dictstackunderflow;
	dsp--;
	return 0;
}

/* def */
/* We make this into a separate procedure because */
/* the interpreter will almost always call it directly. */
int
zop_def(register os_ptr op)
{	int code;
	check_op(2);
	if ( r_has_type(op - 1, t_null) ) return e_typecheck;
	check_dict_write(*dsp);
	return dict_put(dsp, op - 1, op);
}
int
zdef(os_ptr op)
{	int code = zop_def(op);
	if ( !code ) { pop(2); }
	return code;
}

/* load */
int
zload(register os_ptr op)
{	ref *pvalue;
	check_op(1);
	if ( r_has_type(op, t_null) ) return e_typecheck;
	if ( r_has_type(op, t_name) )
	   {	/* Use the fast lookup */
		if ( (pvalue = dict_find_name(op)) == 0 )
			return e_undefined;
	   }
	else
	   {	if ( dict_lookup(dstack, dsp, op, &pvalue) <= 0 )
			return e_undefined;
	   }
	ref_assign(op, pvalue);
	return 0;
}

/* store */
int
zstore(register os_ptr op)
{	ref *pvalue;
	int code;
	check_op(2);
	if ( r_has_type(op - 1, t_null) ) return e_typecheck;
	if ( dict_lookup(dstack, dsp, op - 1, &pvalue) <= 0 )
	   {	code = dict_put(dsp, op - 1, op);
		if ( code ) return code;
	   }
	else
		ref_assign_old(pvalue, op, "store");
	pop(2);
	return 0;
}

/* get - implemented in zgeneric.c */

/* put - implemented in zgeneric.c */

/* undef */
int
zundef(register os_ptr op)
{	check_type(op[-1], t_dictionary);
	check_dict_write(op[-1]);
	if ( !r_has_type(op, t_null) )
		dict_undef(op - 1, op);	/* ignore undefined error */
	pop(2);
	return 0;
}

/* known */
int
zknown(register os_ptr op)
{	os_ptr op1 = op - 1;
	ref *pvalue;
	check_type(*op1, t_dictionary);
	check_dict_read(*op1);
	make_bool(op1,
		  (r_has_type(op, t_null) ? 0 :
		   dict_find(op1, op, &pvalue) > 0 ? 1 : 0));
	pop(1);
	return 0;
}

/* where */
int
zwhere(register os_ptr op)
{	ref *pdref = dsp;
	ref *pvalue;
	check_op(1);
	if ( r_has_type(op, t_null) )
	   {	make_bool(op, 0);
		return 0;
	   }
	while ( 1 )
	   {	check_dict_read(*pdref);
		if ( dict_find(pdref, op, &pvalue) > 0 ) break;
		if ( --pdref < dstack )
		   {	make_bool(op, 0);
			return 0;
		   }
	   }
	ref_assign(op, pdref);
	push(1);
	make_bool(op, 1);
	return 0;
}

/* copy for dictionaries -- called from zcopy in zgeneric.c. */
/* Only the type of *op has been checked. */
int
zcopy_dict(register os_ptr op)
{	os_ptr op1 = op - 1;
	check_type(*op1, t_dictionary);
	check_dict_read(*op1);
	check_dict_write(*op);
	if ( !dict_auto_expand && (dict_length(op) != 0 || dict_maxlength(op) < dict_length(op1)) )
		return e_rangecheck;
	dict_copy(op1, op);
	ref_assign(op - 1, op);
	pop(1);
	return 0;
}

/* currentdict */
int
zcurrentdict(register os_ptr op)
{	push(1);
	ref_assign(op, dsp);
	return 0;
}

/* countdictstack */
int
zcountdictstack(register os_ptr op)
{	push(1);
	make_int(op, dsp - dstack + 1);
	return 0;
}

/* dictstack */
int
zdictstack(register os_ptr op)
{	int depth = dsp - dstack + 1;
	check_write_type(*op, t_array);
	if ( depth > r_size(op) ) return e_rangecheck;
	r_set_size(op, depth);
	refcpy_to_old(op->value.refs, dstack, depth, "dictstack");
	return 0;
}

/* cleardictstack */
int
zcleardictstack(os_ptr op)
{	dsp = dstack + 1;
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zdict_op_defs[] = {
	{"0cleardictstack", zcleardictstack},
	{"1begin", zbegin},
	{"0countdictstack", zcountdictstack},
	{"0currentdict", zcurrentdict},
	{"2def", zdef},
	{"1dict", zdict},
	{"0dictstack", zdictstack},
	{"0end", zend},
	{"2known", zknown},
	{"1load", zload},
	{"1maxlength", zmaxlength},
	{"2setmaxlength", zsetmaxlength},
	{"2store", zstore},
	{"2undef", zundef},
	{"1where", zwhere},
	op_def_end(0)
};
