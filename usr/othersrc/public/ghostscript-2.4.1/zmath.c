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

/* zmath.c */
/* Mathematical operators for GhostScript */
#include "math_.h"
#include "ghost.h"
#include "errors.h"
#include "oper.h"
#include "store.h"

/* Factors for converting between degrees and radians */
double degrees_to_radians = M_PI / 180.0;
double radians_to_degrees = 180.0 / M_PI;

/* Current state of random number generator. */
/* We have to implement this ourselves because */
/* the Unix rand doesn't provide anything equivalent to rrand. */
private long rand_state;

/* Initialize the random number generator. */
private void
zmath_init()
{	rand_state = 1;
}

/****** NOTE: none of these operators currently ******/
/****** check for floating over- or underflow.	******/

/* sqrt */
int
zsqrt(register os_ptr op)
{	float num;
	int code = num_params(op, 1, &num);
	if ( code < 0 ) return code;
	if ( num < 0.0 ) return e_rangecheck;
	make_real(op, sqrt(num));
	return 0;
}

/* arccos */
int
zarccos(register os_ptr op)
{	float num, result;
	int code = num_params(op, 1, &num);
	if ( code < 0 ) return code;
	result = acos(num) * radians_to_degrees;
	make_real(op, result);
	return 0;
}

/* arcsin */
int
zarcsin(register os_ptr op)
{	float num, result;
	int code = num_params(op, 1, &num);
	if ( code < 0 ) return code;
	result = asin(num) * radians_to_degrees;
	make_real(op, result);
	return 0;
}

/* atan */
int
zatan(register os_ptr op)
{	float args[2];
	float result;
	int code = num_params(op, 2, args);
	if ( code < 0 ) return code;
	if ( args[0] == 0 )		/* on X-axis, special case */
	   {	if ( args[1] == 0 ) return e_undefinedresult;
		result = (args[1] < 0 ? 180 : 0);
	   }
	else
	   {	result = atan2(args[0], args[1]) * radians_to_degrees;
		if ( result < 0 ) result += 360;
	   }
	make_real(op - 1, result);
	pop(1);
	return 0;
}

/* cos */
int
zcos(register os_ptr op)
{	float angle;
	int code = num_params(op, 1, &angle);
	if ( code < 0 ) return code;
	make_real(op, cos(angle * degrees_to_radians));
	return 0;
}

/* sin */
int
zsin(register os_ptr op)
{	float angle;
	int code = num_params(op, 1, &angle);
	if ( code < 0 ) return code;
	make_real(op, sin(angle * degrees_to_radians));
	return 0;
}

/* exp */
int
zexp(register os_ptr op)
{	float args[2];
	float result;
	double ipart;
	int code = num_params(op, 2, args);
	if ( code < 0 ) return code;
	if ( args[0] == 0.0 && args[1] == 0.0 ) return e_undefinedresult;
	if ( args[0] < 0.0 && modf(args[1], &ipart) != 0.0 )
		return e_undefinedresult;
	result = pow(args[0], args[1]);
	make_real(op - 1, result);
	pop(1);
	return 0;
}

/* ln */
int
zln(register os_ptr op)
{	float num;
	int code = num_params(op, 1, &num);
	if ( code < 0 ) return code;
	if ( num <= 0.0 ) return e_rangecheck;
	make_real(op, log(num));
	return 0;
}

/* log */
int
zlog(register os_ptr op)
{	float num;
	int code = num_params(op, 1, &num);
	if ( code < 0 ) return code;
	if ( num <= 0.0 ) return e_rangecheck;
	make_real(op, log10(num));
	return 0;
}

/* rand */
int
zrand(register os_ptr op)
{	/*
	 * We use an algorithm from CACM 31 no. 10, pp. 1192-1201,
	 * October 1988.  According to a posting by Ed Taft on
	 * comp.lang.postscript, Level 2 (Adobe) PostScript interpreters use
	 * this algorithm too:
	 *	x[n+1] = (16807 * x[n]) mod (2^31 - 1)
	 */
#define A 16807
#define M 0x7fffffff
#define Q 127773			/* M / A */
#define R 2836				/* M % A */
	rand_state = A * (rand_state % Q) - R * (rand_state / Q);
	while ( rand_state <= 0 ) rand_state += M;
#undef A
#undef M
#undef Q
#undef R
	push(1);
	make_int(op, rand_state);
	return 0;
}

/* srand */
int
zsrand(register os_ptr op)
{	check_type(*op, t_integer);
	rand_state = op->value.intval;
	pop(1);
	return 0;
}

/* rrand */
int
zrrand(register os_ptr op)
{	push(1);
	make_int(op, rand_state);
	return 0;
}

/* ------ Initialization procedure ------ */

op_def zmath_op_defs[] = {
	{"1arccos", zarccos},		/* extension */
	{"1arcsin", zarcsin},		/* extension */
	{"2atan", zatan},
	{"1cos", zcos},
	{"2exp", zexp},
	{"1ln", zln},
	{"1log", zlog},
	{"0rand", zrand},
	{"0rrand", zrrand},
	{"1sin", zsin},
	{"1sqrt", zsqrt},
	{"1srand", zsrand},
	op_def_end(zmath_init)
};
