/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lgamma.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>
#include <errno.h>

#include "mathimpl.h"

/* Log gamma function.
 * Error:  x > 0 error < 1.3ulp.
 *	   x > 4, error < 1ulp.
 *	   x > 9, error < .6ulp.
 * 	   x < 0, all bets are off.
 * Method:
 *	x > 6:
 *		Use the asymptotic expansion (Stirling's Formula)
 *	0 < x < 6:
 *		Use gamma(x+1) = x*gamma(x)
 *		Use rational approximation in
 *		the range 1.2, 2.5
 *	x < 0:
 *		Use the reflection formula,
 *		G(1-x)G(x) = PI/sin(PI*x)
 * Special values:
 *	non-positive integer	returns +Inf.
 *	NaN			returns NaN
*/
#if defined(vax) || defined(tahoe)
/* double and float have same size exponent field */
#define TRUNC(x) (double) (float) (x)
#define _IEEE	0
#else
#define TRUNC(x) *(((int *) &x) + 1) &= 0xf8000000
#define _IEEE	1
#define infnan(x) (zero/zero)
#endif

extern double log1p(double);
static double small_lgam(double);
static double large_lgam(double);
static double neg_lgam(double);
static double zero = 0.0, one = 1.0;
int signgam;

#define lns2pi	.418938533204672741780329736405
#define UNDERFL (1e-1020 * 1e-1020)

#define LEFT	(1.0 - (x0 + .25))
#define RIGHT	(x0 - .218)
/*
/* Constants for approximation in [1.244,1.712]
*/
#define x0	0.461632144968362356785
#define x0_lo	-.000000000000000015522348162858676890521
#define a0_hi	-0.12148629128932952880859
#define a0_lo	.0000000007534799204229502
#define r0	-2.771227512955130520e-002
#define r1	-2.980729795228150847e-001
#define r2	-3.257411333183093394e-001
#define r3	-1.126814387531706041e-001
#define r4	-1.129130057170225562e-002
#define r5	-2.259650588213369095e-005
#define s0	 1.714457160001714442e+000
#define s1	 2.786469504618194648e+000
#define s2	 1.564546365519179805e+000
#define s3	 3.485846389981109850e-001
#define s4	 2.467759345363656348e-002
/*
 * Constants for approximation in [1.71, 2.5]
*/
#define a1_hi	4.227843350984671344505727574870e-01
#define a1_lo	4.670126436531227189e-18
#define p0	3.224670334241133695662995251041e-01
#define p1	3.569659696950364669021382724168e-01
#define p2	1.342918716072560025853732668111e-01
#define p3	1.950702176409779831089963408886e-02
#define p4	8.546740251667538090796227834289e-04
#define q0	1.000000000000000444089209850062e+00
#define q1	1.315850076960161985084596381057e+00
#define q2	6.274644311862156431658377186977e-01
#define q3	1.304706631926259297049597307705e-01
#define q4	1.102815279606722369265536798366e-02
#define q5	2.512690594856678929537585620579e-04
#define q6	-1.003597548112371003358107325598e-06
/*
 * Stirling's Formula, adjusted for equal-ripple. x in [6,Inf].
*/
#define pb0	.0833333333333333148296162562474
#define pb1	-.00277777777774548123579378966497
#define pb2	.000793650778754435631476282786423
#define pb3	-.000595235082566672847950717262222
#define pb4	.000841428560346653702135821806252
#define pb5	-.00189773526463879200348872089421
#define pb6	.00569394463439411649408050664078
#define pb7	-.0144705562421428915453880392761

double
lgamma(double x)
{
	double r;
	signgam = 1;
	if (!finite(x))
		if (_IEEE)
			return (x+x);
		else return (infnan(EDOM));

	if (x > 6 + RIGHT) {
		r = large_lgam(x);
		return (r);
	} else if (x > 1e-16)
		return (small_lgam(x));
	else if (x > -1e-16) {
		if (x < 0)
			signgam = -1, x = -x;
		return (-log(x));
	} else
		return (neg_lgam(x));
}

static double
large_lgam(double x)
{
	double z, p, x1;
	int i;
	struct Double t, u, v;
	u = log__D(x);
	u.a -= 1.0;
	if (x > 1e15) {
		v.a = x - 0.5;
		TRUNC(v.a);
		v.b = (x - v.a) - 0.5;
		t.a = u.a*v.a;
		t.b = x*u.b + v.b*u.a;
		if (_IEEE == 0 && !finite(t.a))
			return(infnan(ERANGE));
		return(t.a + t.b);
	}
	x1 = 1./x;
	z = x1*x1;
	p = pb0+z*(pb1+z*(pb2+z*(pb3+z*(pb4+z*(pb5+z*(pb6+z*pb7))))));
					/* error in approximation = 2.8e-19 */

	p = p*x1;			/* error < 2.3e-18 absolute */
					/* 0 < p < 1/64 (at x = 5.5) */
	x = x - 0.5;
	TRUNC(v.a);			/* truncate v.a to 26 bits. */
	v.b = x - v.a;
	t.a = v.a*u.a;			/* t = (x-.5)*(log(x)-1) */
	t.b = v.b*u.a + x*u.b;
	t.b += p; t.b += lns2pi;	/* return t + lns2pi + p */
	return (t.a + t.b);
}

static double
small_lgam(double x)
{
	int x_int;
	double y, z, t, r = 0, p, q, hi, lo;
	struct Double rr;
	x_int = (x + .5);
	y = x - x_int;
	if (x_int <= 2 && y > RIGHT) {
		t = y - x0;
		y--; x_int++;
		goto CONTINUE;
	} else if (y < -LEFT) {
		t = y +(1.0-x0);
CONTINUE:
		z = t - x0_lo;
		p = r0+z*(r1+z*(r2+z*(r3+z*(r4+z*r5))));
		q = s0+z*(s1+z*(s2+z*(s3+z*s4)));
		r = t*(z*(p/q) - x0_lo);
		t = .5*t*t;
		z = 1.0;
		switch (x_int) {
		case 6:	z  = (y + 5);
		case 5:	z *= (y + 4);
		case 4:	z *= (y + 3);
		case 3:	z *= (y + 2);
			rr = log__D(z);
			rr.b += a0_lo; rr.a += a0_hi;
			return(((r+rr.b)+t+rr.a));
		case 2: return(((r+a0_lo)+t)+a0_hi);
		case 0: r -= log1p(x);
		default: rr = log__D(x);
			rr.a -= a0_hi; rr.b -= a0_lo;
			return(((r - rr.b) + t) - rr.a);
		}
	} else {
		p = p0+y*(p1+y*(p2+y*(p3+y*p4)));
		q = q0+y*(q1+y*(q2+y*(q3+y*(q4+y*(q5+y*q6)))));
		p = p*(y/q);
		t = (double)(float) y;
		z = y-t;
		hi = (double)(float) (p+a1_hi);
		lo = a1_hi - hi; lo += p; lo += a1_lo;
		r = lo*y + z*hi;	/* q + r = y*(a0+p/q) */
		q = hi*t;
		z = 1.0;
		switch (x_int) {
		case 6:	z  = (y + 5);
		case 5:	z *= (y + 4);
		case 4:	z *= (y + 3);
		case 3:	z *= (y + 2);
			rr = log__D(z);
			r += rr.b; r += q;
			return(rr.a + r);
		case 2:	return (q+ r);
		case 0: rr = log__D(x);
			r -= rr.b; r -= log1p(x);
			r += q; r-= rr.a;
			return(r);
		default: rr = log__D(x);
			r -= rr.b;
			q -= rr.a;
			return (r+q);
		}
	}
}

#define lpi_hi 1.1447298858494001638
#define lpi_lo .0000000000000000102659511627078262
/* Error: within 3.5 ulp for x < 171.  For large x, see lgamma. */
static double
neg_lgam(double x)
{
	double y, z, one = 1.0, zero = 0.0;

	z = floor(x + .5);
	if (z == x) {		/* convention: G(-(integer)) -> +Inf */
		if (_IEEE)
			return (one/zero);
		else
			return (infnan(ERANGE));
	}
	y = .5*ceil(x);
	if (y == ceil(y))
		signgam = -1;
	x = -x;
	z = fabs(x + z);	/* 0 < z <= .5 */
	if (z < .25)
		z = sin(M_PI*z);
	else
		z = cos(M_PI*(0.5-z));
	z = -log(z*x/M_PI);

	if (x > 6. + RIGHT)
		y -= large_lgam(x);
	else
		y = -small_lgam (x);
	return (y + z);
}
