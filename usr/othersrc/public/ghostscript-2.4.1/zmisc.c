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

/* zmisc.c */
/* Miscellaneous operators for Ghostscript */
#include "memory_.h"
#include "string_.h"
#include "ghost.h"
#include "gp.h"
#include "errors.h"
#include "oper.h"
#include "alloc.h"
#include "dict.h"
#include "dstack.h"			/* for name lookup in bind */
#include "name.h"
#include "packed.h"
#include "store.h"
#include "gxfixed.h"			/* for gstype1.h */
#include "gstype1.h"

/* Import the C getenv function */
extern char *getenv(P1(char *));

/* bind */
int
zbind(register os_ptr op)
{	os_ptr bsp = op;			/* bottom of stack */
	ref *defp = op;
	switch ( r_type(op) )
	   {
	case t_array:
	case t_mixedarray:
	case t_shortarray:
		break;
	case t_oparray:
		defp = &op_array_table.value.refs[op_index(op) - op_def_count];
		break;
	default:
		return e_typecheck;
	   }
	++bsp;
	/* We must not make the top-level procedure read-only, */
	/* but we must bind it even if it is read-only already. */
	*bsp = *defp;
	/* Here are the invariants for the following loop: */
	/*	op < bsp <= ostop; */
	/*	for every pointer p such that op < p <= bsp, */
	/*	  *p is an array (or packedarray) ref. */
#define r_is_ex_oper(rp)\
  ((r_btype(rp) == t_operator || r_type(rp) == t_oparray) &&\
   r_has_attr(rp, a_executable))
	while ( bsp > op )
	   {	while ( r_size(bsp) )
		   {	ref *tp = bsp->value.refs;
			r_inc_size(bsp, -1);
			if ( *(ushort *)tp > packed_max_full_ref )
			 { /* Check for a packed executable name */
			   ushort elt = *(ushort *)tp;
			   if ( (elt & ~(ushort)packed_max_name_index) ==
				pt_tag(pt_executable_name) )
			    { ref nref;
			      ref *pvalue;
			      name_index_ref(elt & packed_max_name_index,
					     &nref);
			      if ( (pvalue = dict_find_name(&nref)) != 0 &&
				   r_is_ex_oper(pvalue)
				 )
				/* Note: can't undo this by restore! */
				*(ushort *)tp =
				  pt_tag(pt_executable_operator) +
				  op_index(pvalue);
			    }
			   bsp->value.refs = (ref *)((ushort *)tp + 1);
			 }
			else
			  switch ( bsp->value.refs++, r_type(tp) )
			 {
			case t_name:	/* bind the name if an operator */
			  if ( r_has_attr(tp, a_executable) )
			   {	ref *pvalue;
				if ( (pvalue = dict_find_name(tp)) != 0 &&
				     r_is_ex_oper(pvalue)
				   )
					ref_assign_old(tp, pvalue, "bind");
			   }
			  break;
			case t_array:	/* push into array if procedure */
			  if ( !r_has_attr(tp, a_write) ) break;
			case t_mixedarray:
			case t_shortarray:
			  if ( r_has_attr(tp, a_executable) && bsp < ostop )
			   {	/* Make reference read-only */
				r_clear_attrs(tp, a_write);
				*++bsp = *tp;
			   }
			 }
		   }
		bsp--;
	   }
	return 0;
}

/* currenttime */
int
zcurrenttime(register os_ptr op)
{	long date_time[2];
	gp_get_clock(date_time);
	push(1);
	make_real(op, date_time[0] * 1440.0 + date_time[1] / 60000.0);
	return 0;
}

/* getenv */
int
zgetenv(register os_ptr op)
{	char *str, *value;
	int code;
	check_read_type(*op, t_string);
	str = ref_to_string(op, "getenv name");
	if ( str == 0 ) return e_VMerror;
	value = getenv(str);
	alloc_free(str, r_size(op) + 1, 1, "getenv name");
	if ( value == 0 )		/* not found */
	   {	make_bool(op, 0);
		return 0;
	   }
	code = string_to_ref(value, op, "getenv value");
	if ( code < 0 ) return code;
	push(1);
	make_bool(op, 1);
	return 0;
}

/* makeoperator */
int
zmakeoperator(register os_ptr op)
{	check_type(op[-1], t_name);
	check_proc(*op);
	if ( op_array_count == r_size(&op_array_table) )
		return e_limitcheck;
	ref_assign_old(&op_array_table.value.refs[op_array_count],
		       op, "makeoperator");
	op_array_nx_table[op_array_count] = name_index(op - 1);
	r_set_type_attrs(op - 1, t_oparray, a_executable);
	r_set_size(op - 1, op_def_count + op_array_count);
	op_array_count++;
	pop(1);
	return 0;
}

/* setdebug */
int
zsetdebug(register os_ptr op)
{	check_read_type(op[-1], t_string);
	check_type(*op, t_boolean);
#ifdef DEBUG
	   {	int i;
		for ( i = 0; i < r_size(op - 1); i++ )
			gs_debug[op[-1].value.bytes[i] & 127] =
				op->value.index;
	   }
#endif
	pop(2);
	return 0;
}

/* type1encrypt, type1decrypt */
private int type1crypt(P2(os_ptr,
			  int (*)(P4(byte *, byte *, uint, ushort *))));
int
ztype1encrypt(os_ptr op)
{	return type1crypt(op, gs_type1_encrypt);
}
int
ztype1decrypt(os_ptr op)
{	return type1crypt(op, gs_type1_decrypt);
}
private int
type1crypt(register os_ptr op, int (*proc)(P4(byte *, byte *, uint, ushort *)))
{	crypt_state state;
	uint ssize;
	check_type(op[-2], t_integer);
	state = op[-2].value.intval;
	if ( op[-2].value.intval != state )
		return e_rangecheck;	/* state value was truncated */
	check_read_type(op[-1], t_string);
	check_write_type(*op, t_string);
	ssize = r_size(op - 1);
	if ( r_size(op) < ssize )
		return e_rangecheck;
	(void) (*proc)(op->value.bytes, op[-1].value.bytes, ssize,
		       &state);		/* can't fail */
	op[-2].value.intval = state;
	op[-1] = *op;
	r_set_size(op - 1, ssize);
	pop(1);
	return 0;
}

/* usertime */
int
zusertime(register os_ptr op)
{	long date_time[2];
	gp_get_clock(date_time);
	push(1);
	make_int(op, date_time[0] * 86400000L + date_time[1]);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zmisc_op_defs[] = {
	{"1bind", zbind},
	{"0currenttime", zcurrenttime},
	{"1getenv", zgetenv},
	{"2makeoperator", zmakeoperator},
	{"2setdebug", zsetdebug},
	{"3type1encrypt", ztype1encrypt},
	{"3type1decrypt", ztype1decrypt},
	{"0usertime", zusertime},
	op_def_end(0)
};
