/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lgamma.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>
#include <errno.h>

#include "mathimpl.h"

/* TRUNC sets trailing bits in a floating-point number to zero.
 * ptrx points to the second half of the floating-point number.
 * x1 is a temporary variable.
*/

#if defined(vax) || defined(tahoe)
#define LSHFT_PLUS_1	(134217728.+1.)
#define TRUNC(x, dummy, x1) x1 = x*LSHFT_PLUS_1, x -= x1, x += x1
#else
#define MASK 0xf8000000
#define TRUNC(x, ptrx, dummy) *ptrx &= MASK
#endif

#define x0	0.461632144968362356785
#define x0_lo	-.000000000000000015522348162858676890521
#define a0_hi	-0.12148629053584961146
#define a0_lo	3.07435645275902737e-18
#define LEFT	(1.0 - (x0 + .25))
#define RIGHT	(x0 - .218)
#define lns2pi_hi 0.418945312500000
#define lns2pi_lo -.000006779295327258219670263595

#define UNDERFL (1e-1020 * 1e-1020)
int signgam;

#define r0	-0.02771227512955130520
#define r1	-0.2980729795228150847
#define r2	-0.3257411333183093394
#define r3	-0.1126814387531706041
#define r4	-0.01129130057170225562
#define r5	-2.259650588213369095e-05
#define s0	1.7144571600017144419
#define s1	2.7864695046181946481
#define s2	1.5645463655191798047
#define s3	0.34858463899811098496
#define s4	0.024677593453636563481

#define p0     -7.721566490153286087127140e-02
#define p1	2.077324848654884653970785e-01
#define p2	3.474331160945523535787194e-01
#define p3	1.724375677840324429295524e-01
#define p4	3.546181984297784658205969e-02
#define p5	2.866163630124151557532506e-03
#define p6	6.143168512963655570532770e-05
#define q0	1.000000000000000000000000e+00
#define q1	1.485897307300750567469226e+00
#define q2	8.336064915387758261556045e-01
#define q3	2.185469782512070977585556e-01
#define q4	2.671060746990048983840354e-02
#define q5	1.296631961305647947057711e-03
#define q6	1.484566091079246905938151e-05

#define NP2 8
double P2[] = {
.0833333333333333148296162562474,
-.00277777777774548123579378966497,
.000793650778754435631476282786423,
-.000595235082566672847950717262222,
.000841428560346653702135821806252,
-.00189773526463879200348872089421,
.00569394463439411649408050664078,
-.0144705562421428915453880392761
};

static double neg_lgam __P((double));
static double small_lgam __P((double));
static double large_lgam __P((double));

double
lgamma(x)
	double x;
{
	double zero = 0.0, one = 1.0;
	double r;
	signgam = 1;
	if (!finite(x)) {
		errno = EDOM;
		if (x < 0)
			x = -x;
		else if (x > 0)
			errno = ERANGE;
		return (x);
	}
	if (x > 6 + RIGHT) {
		if (x > 1.0e20)
			return (x*(log(x)-one));
		r = large_lgam(x);
		return (r);
	} else if (x > 1e-17)
		return (small_lgam(x));
	else if(x > -1e-17) {
		if (x < 0)
			signgam = -1, x = -x;
		return (-log(x));
	} else
		return (neg_lgam(x));
}

/* Accurate to max(ulp(1/128) absolute, 2^-75 relative) error. */
static double
large_lgam(x)
	double x;
{
	double z, p, x1;
	int i;
	long *pva, *pua;
	struct Double t, u, v;
	pua = (long *) &u.a, pua++;
	pva = (long *) &v.a, pva++;
	z = 1.0/(x*x);
	for (p = P2[i = NP2-1]; --i >= 0;)
		p = P2[i] + p*z;	/* error in approximation = 2.8e-18 */

	p = p/x;			/* ulp = 1.7e-18; error < 1.6ulp */
					/* 0 < frac < 1/64 (at x = 5.5) */
	t = log__D(x);
	t.a -= 1.0;
	u.a = t.a + t.b;
	TRUNC (u.a, pua, x1);		/* truncate u.a */
	u.b = (t.a - u.a);
	u.b += t.b;
	x -= .5;
	v.a = x;
	TRUNC(v.a, pva, x1);		/* truncate v.a */
	v.b = x - v.a;
	t.a = v.a*u.a;			/* t = (x-.5)*(log(x)-1) */
	t.b = v.b*u.a + x*u.b;
	z = t.b + lns2pi_lo;		/* return t + lns2pi + p */
	z += p; z += lns2pi_hi;
	z += t.a;
	return (z);
}
/* Good to < 1 ulp.  (provably .90 ulp; .87 ulp on 1,000,000 runs.)
   It also has correct monotonicity.
 */
static double
small_lgam(x)
	double x;
{
	int xi;
	double y, z, t, r = 0, p, q;
	struct Double rr;

	/* Do nasty area near the minimum.  No good for 1.25 < x < 2.5 */
	if (x < 2.0 - LEFT && x > 1.0 + RIGHT) {
		t = x - 1.0; t -= x0;
		z = t - x0_lo;
		p = r0+z*(r1+z*(r2+z*(r3+z*(r4+z*r5))));
		q = s0+z*(s1+z*(s2+z*(s3+z*s4)));
		r = t*(z*(p/q) - x0_lo) + a0_lo;
		r += .5*t*t; r += a0_hi;
		return (r);
	}
	xi = (x + .5);
	y = x - xi;
	rr.a = rr.b = 0;
	if (y < -LEFT) {	/* necessary for 2.5 < x < 2.72.. */
		t = y + (1.0 - x0);
		z = t - x0_lo;
		p = r0+z*(r1+z*(r2+z*(r3+z*(r4+z*r5))));
		q = s0+z*(s1+z*(s2+z*(s3+z*s4)));
		r = t*(z*(p/q) - x0_lo) + a0_lo;
		r += .5*t*t;
		q = a0_hi;
		printf("(0)q = %.18lg r = %.18lg\n", q, r);
	} else {
		p = y*(p0+y*(p1+y*(p2+y*(p3+y*(p4+y*(p5+y*p6))))));
		q = q0+y*(q1+y*(q2+y*(q3+y*(q4+y*(q5+y*q6)))));
		r = p/q;
		q = .5*y;
		printf("(1)q = %.18lg r = %.18lg\n", q, r);
	}
	printf("y = %lg, r = %.18lg\n", y, r);
	z = 1.0;
	switch (xi) {
	case 6:	z  = (y + 5);
	case 5:	z *= (y + 4);
	case 4:	z *= (y + 3);
	case 3:	z *= (y + 2);
		rr = log__D(z);
		printf("%.21lg, %lg\n", rr.a, rr.b);
		r += rr.b; q += r;
		return(q + rr.a);
	case 2:	return (q + r);

	case 0: printf("r = %lg\n", r);
		r -= log1p(x); printf("\t%lg\n", r);
	default: rr = log__D(x);
		r -= rr.b; printf("\t%lg\n", r);
	}
	if (q > .5 *rr.a) {
		printf("q = %lg, rr = %lg, r = %lg\n", q, rr.a, r);
		q -= rr.a;
		return(r + q);
	} else
		printf("q = %lg, rr = %lg, r = %lg\n", q, rr.a, r);
		return((r + q) - rr.a);
}

#define lpi_hi 1.1447298858494001638
#define lpi_lo .0000000000000000102659511627078262
/* Error: within 3.5 ulp for x < 171.  For large x, see lgamma. */
static double
neg_lgam(x)
	double x;
{
	struct Double lg, lsine;
	double y, z, one = 1.0, zero = 0.0;

	z = floor(x + .5);
	if (z == x) {
		errno = EDOM;
		return (one/zero);	/* convention: G(-(integer)) -> +oo */
	}
	y = ceil(x);
	if (y*.5 == ceil(.5*y))
		signgam = -1;

	x = -x;
	z = fabs(x + z);	/* 0 < z <= .5 */
	if (z < .25)
		z = sin(M_PI*z);
	else
		z = cos(M_PI*(.5-z));
	z = -log(z*x/M_PI);

	if (x > 6.5)
		y -= large_lgam(x);
	else
		y = -small_lgam (x);
	return (y + z);
}
