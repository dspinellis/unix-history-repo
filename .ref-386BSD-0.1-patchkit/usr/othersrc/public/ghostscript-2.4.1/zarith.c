/* Copyright (C) 1989, 1990, 1991 Aladdin Enterprises.  All rights reserved.
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

/* zarith.c */
/* Arithmetic operators for GhostScript */
#include "math_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "store.h"

/****** NOTE: none of the arithmetic operators  ******/
/****** currently check for floating exceptions ******/

/* Imported operators */
extern int zcvi(P1(os_ptr));

/* Macro for accessing next-to-top stack element */
#define opm1 (op-1)
/* Macros for generating non-integer cases for arithmetic operations. */
/* 'frob' is one of the arithmetic operators, +, -, or *. */
#define non_int_cases(frob,frob_equals)\
 switch ( r_type(op) ) {\
  default: return e_typecheck;\
  case t_real: switch ( r_type(opm1) ) {\
   default: return e_typecheck;\
   case t_real: op[-1].value.realval frob_equals op->value.realval; break;\
   case t_integer: make_real(opm1, op[-1].value.intval frob op->value.realval);\
  } break;\
  case t_integer: switch ( r_type(opm1) ) {\
   default: return e_typecheck;\
   case t_real: op[-1].value.realval frob_equals op->value.intval; break;\
   case t_integer:
#define end_cases()\
  } }

/* add */
/* We make this into a separate procedure because */
/* the interpreter will almost always call it directly. */
int
zop_add(register os_ptr op)
{	non_int_cases(+, +=)
	   {	long int2 = op->value.intval;
		if ( ((op[-1].value.intval += int2) ^ int2) < 0 &&
		     ((op[-1].value.intval - int2) ^ int2) >= 0
		   )
		   {	/* Overflow, convert to real */
			make_real(opm1, (float)(op[-1].value.intval - int2) + int2);
		   }
	   }
	end_cases()
	return 0;
}
int
zadd(os_ptr op)
{	int code = zop_add(op);
	if ( code == 0 ) { pop(1); }
	return code;
}

/* div */
int
zdiv(register os_ptr op)
{	register os_ptr op1 = op - 1;
	/* We can't use the non_int_cases macro, */
	/* because we have to check explicitly for op == 0. */
	switch ( r_type(op) )
	   {
	default: return e_typecheck;
	case t_real:
		if ( op->value.realval == 0 ) return e_undefinedresult;
		switch ( r_type(op1) )
		   {
		default: return e_typecheck;
		case t_real: op1->value.realval /= op->value.realval; break;
		case t_integer: make_real(op1, op1->value.intval / op->value.realval);
		   }
		break;
	case t_integer:
		if ( op->value.intval == 0 ) return e_undefinedresult;
		switch ( r_type(op1) )
		   {
		default: return e_typecheck;
		case t_real: op1->value.realval /= op->value.intval; break;
		case t_integer: make_real(op1, (float)op1->value.intval / op->value.intval);
		   }
	   }
	pop(1);
	return 0;
}

/* mul */
int
zmul(register os_ptr op)
{	non_int_cases(*, *=)
	   {	long int1 = op[-1].value.intval;
		long int2 = op->value.intval;
		long abs1 = (int1 >= 0 ? int1 : - int1);
		long abs2 = (int2 >= 0 ? int2 : - int2);
		float fprod;
		if (	(abs1 > 0x7fff || abs2 > 0x7fff) &&
			/* At least one of the operands is very large. */
			/* Check for integer overflow. */
			abs1 != 0 &&
			abs2 > 0x7fffffffL / abs1 &&
			/* Check for the boundary case */
			(fprod = (float)int1 * int2,
			 (int1 * int2 != -0x80000000L ||
			 fprod != (float)-0x80000000L))
		   )
			make_real(opm1, fprod);
		else
			op[-1].value.intval = int1 * int2;
	   }
	end_cases()
	pop(1);
	return 0;
}

/* sub */
/* We make this into a separate procedure because */
/* the interpreter will almost always call it directly. */
int
zop_sub(register os_ptr op)
{	non_int_cases(-, -=)
	   {	long int1 = op[-1].value.intval;
		if ( (int1 ^ (op[-1].value.intval = int1 - op->value.intval)) < 0 &&
		     (int1 ^ op->value.intval) < 0
		   )
		   {	/* Overflow, convert to real */
			make_real(opm1, (float)int1 - op->value.intval);
		   }
	   }
	end_cases()
	return 0;
}
int
zsub(os_ptr op)
{	int code = zop_sub(op);
	if ( code == 0 ) { pop(1); }
	return code;
}

/* idiv */
int
zidiv(register os_ptr op)
{	/* The Red Book says this only works on integers, */
	/* but implementations also accept reals. */
	ref save_num;
	int code;
	save_num = op[-1];
	code = zdiv(op);
	if ( code < 0 ) return code;	/* division failed */
	code = zcvi(op - 1);
	if ( code < 0 )
	   {	/* cvi failed, restore numerator */
		op[-1] = save_num;
		osp = op;		/* restore osp as well */
	   }
	return code;
}

/* mod */
int
zmod(register os_ptr op)
{	check_type(op[-1], t_integer);
	check_type(*op, t_integer);
	if ( op->value.intval == 0 ) return e_undefinedresult;
	op[-1].value.intval %= op->value.intval;
	pop(1);
	return 0;
}

/* neg */
int
zneg(register os_ptr op)
{	switch ( r_type(op) )
	   {
	default: return e_typecheck;
	case t_real: op->value.realval = -op->value.realval; break;
	case t_integer:
		if ( op->value.intval == -0x80000000L )	/* min integer */
			make_real(op, -(float)-0x80000000L);
		else
			op->value.intval = -op->value.intval;
	   }
	return 0;
}

/* ceiling */
int
zceiling(register os_ptr op)
{	switch ( r_type(op) )
	   {
	default: return e_typecheck;
	case t_real: op->value.realval = ceil(op->value.realval);
	case t_integer: ;
	   }
	return 0;
}

/* floor */
int
zfloor(register os_ptr op)
{	switch ( r_type(op) )
	   {
	default: return e_typecheck;
	case t_real: op->value.realval = floor(op->value.realval);
	case t_integer: ;
	   }
	return 0;
}

/* round */
int
zround(register os_ptr op)
{	switch ( r_type(op) )
	   {
	default: return e_typecheck;
	case t_real: op->value.realval = floor(op->value.realval + 0.5);
	case t_integer: ;
	   }
	return 0;
}

/* truncate */
int
ztruncate(register os_ptr op)
{	switch ( r_type(op) )
	   {
	default: return e_typecheck;
	case t_real:
		op->value.realval =
			(op->value.realval < 0.0 ?
				ceil(op->value.realval) :
				floor(op->value.realval));
	case t_integer: ;
	   }
	return 0;
}

/* ------ Initialization table ------ */

op_def zarith_op_defs[] = {
	{"2add", zadd},
	{"1ceiling", zceiling},
	{"2div", zdiv},
	{"2idiv", zidiv},
	{"1floor", zfloor},
	{"2mod", zmod},
	{"2mul", zmul},
	{"1neg", zneg},
	{"1round", zround},
	{"2sub", zsub},
	{"1truncate", ztruncate},
	op_def_end(0)
};
