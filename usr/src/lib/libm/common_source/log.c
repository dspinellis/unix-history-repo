/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)log.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>
#include <errno.h>

#include "log_table.h"
#include "dmath.h"

/* Table-driven natural logarithm.
 *
 * This code was derived, with minor modifications, from:
 *	Peter Tang, "Table-Driven Implementation of the
 *	Logarithm in IEEE Floating-Point arithmetic." ACM Trans.
 *	Math Software, vol 16. no 4, pp 378-400, Dec 1990).
 *
 * Calculates log(2^m*F*(1+f/F)), |f/j| <= 1/256,
 * where F = j/128 for j an integer in [0, 128].
 *
 * log(2^m) = log2_hi*m + log2_tail*m
 * since m is an integer, the dominant term is exact.
 * m has at most 10 digits (for subnormal numbers),
 * and log2_hi has 11 trailing zero bits.
 *
 * log(F) = logF_hi[j] + logF_lo[j] is in tabular form in log_table.h
 * logF_hi[] + 512 is exact.
 *
 * log(1+f/F) = 2*f/(2*F + f) + 1/12 * (2*f/(2*F + f))**3 + ...
 * the leading term is calculated to extra precision in two
 * parts, the larger of which adds exactly to the dominant
 * m and F terms.
 * There are two cases:
 *	1. when m, j are non-zero (m | j), use absolute
 *	   precision for the leading term.
 *	2. when m = j = 0, |1-x| < 1/256, and log(x) ~= (x-1).
 *	   In this case, use a relative precision of 24 bits.
 * (This is done differently in the original paper)
 *
 * Special cases:
 *	0	return signalling -Inf
 *	neg	return signalling NaN
 *	+Inf	return +Inf
*/

#if defined(vax) || defined(tahoe)
#define _IEEE	0
#define TRUNC(x) (double) (float) (x)
#else
#define _IEEE	1
#define TRUNC(x) *(((int *) &x) + 1) &= 0xf8000000
#define infnan(x) 0.0
#endif

double
#ifdef _ANSI_SOURCE
log(double x)
#else
log(x) double x;
#endif
{
	int m, j;
	double F, f, g, q, u, u2, v, zero = 0.0, one = 1.0;
	double logb(), ldexp();
	volatile double u1;

	/* Catch special cases */
	if (x <= 0)
		if (_IEEE && x == zero)	/* log(0) = -Inf */
			return (-one/zero);
		else if (_IEEE)		/* log(neg) = NaN */
			return (zero/zero);
		else if (x == zero)	/* NOT REACHED IF _IEEE */
			return (infnan(-ERANGE));
		else
			return (infnan(EDOM));
	else if (!finite(x))
		if (_IEEE)		/* x = NaN, Inf */
			return (x+x);
		else
			return (infnan(ERANGE));
	
	/* Argument reduction: 1 <= g < 2; x/2^m = g;	*/
	/* y = F*(1 + f/F) for |f| <= 2^-8		*/

	m = logb(x);
	g = ldexp(x, -m);
	if (_IEEE && m == -1022) {
		j = logb(g), m += j;
		g = ldexp(g, -j);
	}
	j = N*(g-1) + .5;
	F = (1.0/N) * j + 1;	/* F*128 is an integer in [128, 512] */
	f = g - F;

	/* Approximate expansion for log(1+f/F) ~= u + q */
	g = 1/(2*F+f);
	u = 2*f*g;
	v = u*u;
	q = u*v*(A1 + v*(A2 + v*(A3 + v*A4)));

    /* case 1: u1 = u rounded to 2^-43 absolute.  Since u < 2^-8,
     * 	       u1 has at most 35 bits, and F*u1 is exact, as F has < 8 bits.
     *         It also adds exactly to |m*log2_hi + log_F_head[j] | < 750
    */
	if (m | j)
		u1 = u + 513, u1 -= 513;

    /* case 2:	|1-x| < 1/256. The m- and j- dependent terms are zero;
     * 		u1 = u to 24 bits.
    */
	else
		u1 = u, TRUNC(u1);
	u2 = (2.0*(f - F*u1) - u1*f) * g;
			/* u1 + u2 = 2f/(2F+f) to extra precision.	*/

	/* log(x) = log(2^m*F*(1+f/F)) =				*/
	/* (m*log2_hi+logF_head[j]+u1) + (m*log2_lo+logF_tail[j]+q);	*/
	/* (exact) + (tiny)						*/

	u1 += m*logF_head[N] + logF_head[j];		/* exact */
	u2 = (u2 + logF_tail[j]) + q;			/* tiny */
	u2 += logF_tail[N]*m;
	return (u1 + u2);
}

/* Extra precision variant, returning
 * struct {double a, b;}; log(x) = a+b to 63 bits, with
 * a is rounded to 26 bits.
 */
struct Double
#ifdef _ANSI_SOURCE
log__D(double x)
#else
log__D(x) double x;
#endif
{
	int m, j;
	double F, f, g, q, u, v, u2;
	double logb(), ldexp();
	volatile double u1;
	struct Double r;

	/* Argument reduction: 1 <= g < 2; x/2^m = g;	*/
	/* y = F*(1 + f/F) for |f| <= 2^-8		*/

	m = logb(x);
	g = ldexp(x, -m);
	if (_IEEE && m == -1022) {
		j = logb(g), m += j;
		g = ldexp(g, -j);
	}
	j = N*(g-1) + .5;
	F = (1.0/N) * j + 1;
	f = g - F;

	g = 1/(2*F+f);
	u = 2*f*g;
	v = u*u;
	q = u*v*(A1 + v*(A2 + v*(A3 + v*A4)));
	if (m | j)
		u1 = u + 513, u1 -= 513;
	else
		u1 = u, TRUNC(u1);
	u2 = (2.0*(f - F*u1) - u1*f) * g;

	u1 += m*logF_head[N] + logF_head[j];

	u2 +=  logF_tail[j]; u2 += q;
	u2 += logF_tail[N]*m;
	r.a = u1 + u2;			/* Only difference is here */
	TRUNC(r.a);
	r.b = (u1 - r.a) + u2;
	return (r);
}
