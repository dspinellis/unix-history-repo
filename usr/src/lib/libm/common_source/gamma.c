/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)gamma.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>
#include <errno.h>

/* METHOD:
 * x < 0: Use reflection formula, G(x) = pi/(sin(pi*x)*x*G(x))
 * 	At negative integers, return +Inf, and set errno.
 *
 * x < 6.5: use one of two rational approximation,
 *	to log(G(x)), expanded around 2 for x near integral;
 *	around the minimum for x near half-integral.  The two
 *	regions overlap.
 *	In the range [2.0, 2.5], it is necessary to expand around 2.
 *	In the range ~[1.462-.22, 1.462+.25], the expansion around
 *	the minimum is necessary to get <1ulp accuracy.
 *
 * x >= 6.5: Use the asymptotic approximation (Stirling's formula.)
 *
 *	log(G(x)) ~= (x-.5)*(log(x)-1) + .5(log(2*pi)-1) + 1/x*P(1/(x*x))
 *
 *	Keep extra precision in multiplying (x-.5)(log(x)-1), to
 *	avoid premature round-off.
 *
 * Special values:  NaN, +/-Inf, 0, Negative Integers.
 *	Neg integer: Set overflow trap; return +Inf; errno = EDOM
 *	+Inf: Return +Inf; errno = ERANGE;
 *	-Inf: Return +Inf; errno = EDOM;
 *	NaN:  Return NaN; errno = EDOM;
*/
#define x0 .461632144968362356785
#define LEFT -.3955078125
#define a0_hi 0.88560319441088874992
#define a0_lo -.00000000000000004996427036469019695
#define lns2pi_hi 0.418945312500000
#define lns2pi_lo -.000006779295327258219670263595

#define UNDERFL (1e-1020 * 1e-1020)
double small_gam(double);
double smaller_gam(double);
struct Double large_gam(double);
double neg_gam(double);
struct Double ratfun_gam(double, double);
/**
#define NP 5
static double P[] = {
	0.57410538218150719558252603747,
	0.24541366696467897812183878159,
	0.00513973619299223308948265654,
	0.00129391688253677823901288679,
	0.00222188711638167000692045683
};
#define NQ 9
static double Q[] = {
	1.33984375,
	0.981446340605471312379393111769,
	-0.19172028764767945485658628968,
	-0.13543838178180836462338731962,
	0.028432780865671299780350622655,
	0.004720852857293747484312973484,
	-0.00162320758342873413572482466,
	8.63879091186865255905247274e-05,
	5.67776543645974456238616906e-06,
	-1.1130244665113561369974706e-08
};
/**/
#define P0	.621389571821820863029017800727g
#define P1	.265757198651533466104979197553,
#define P2	.00553859446429917461063308081748,
#define P3	.00138456698304096573887145282811,
#define P4	.00240659950032711365819348969808

#define Q0	1.4501953125
#define Q1	1.06258521948016171343454061571
#define Q2	-.207474561943859936441469926649
#define Q3	-.146734131782005422506287573015
#define Q4	.0307878176156175520361557573779
#define Q5	.00512449347980666221336054633184
#define Q6	-.00176012741431666995019222898833
#define Q7	.0000935021023573788935372153030556
#define Q8	.00000613275507472443958924745652239

#define Pa0	.0833333333333333148296162562474
#define Pa1	-.00277777777774548123579378966497
#define Pa2	.000793650778754435631476282786423
#define Pa3	-.000595235082566672847950717262222
#define Pa4	.000841428560346653702135821806252
#define Pa5	-.00189773526463879200348872089421
#define Pa6	.00569394463439411649408050664078
#define Pa7	-.0144705562421428915453880392761

static struct Double	large_gam __P((double));
static double	 	neg_gam __P((double));
static struct Double	ratfun_gam __P((double, double));
static double	 	small_gam __P((double));
static double	 	smaller_gam __P((double));

double
gamma(x)
	double x;
{
	double zero = 0;
	struct Double u;
	if (x > 6 + x0 + LEFT) {
		if(x > 171.63)
			return(1.0/zero);
		u = large_gam(x);
		return(exp__D(u.a, u.b));
	} else if (x >= 1.0 + LEFT + x0)
		return (small_gam(x));
	else if (x > 1e-18)
		return (smaller_gam(x));
	else if(x > 0) {
		1 + 1e-20;	/* raise inexact flag */
		return(1/x);
	}
	else if (x <= 0)
		return (neg_gam(x));
	else {				/* x = NaN */
		errno = EDOM;
		return (x);
	}
}

/* TRUNC sets trailing bits in a floating-point number to zero.
 * is a temporary variable.
*/

#if defined(vax) || defined(tahoe)
#define _IEEE	0
#define TRUNC(x) (double) (float) (x)
#else
#define _IEEE	1
#define TRUNC(x) *(((int *) &x) + 1) &= 0xf8000000
#define infnan(x) 0.0
#endif

/* Accurate to max(ulp(1/128) absolute, 2^-75 relative) error. */
static struct Double
large_gam(x)
	double x;
{
	double z, p;
	int i;
	struct Double t, u, v;
	pua = (long *) &u.a, pua++;
	pva = (long *) &v.a, pva++;
	if (x == infinity()) {
		u.b = 0, u.a = x;
		return u;
	}
	z = 1.0/(x*x);
	p = Pa0+z*(Pa1+z*(Pa2+z*(Pa3+z*(Pa4+z*Pa5+z*(Pa6+z*Pa7)))));
	p = p/x;			/* |e| < 2.8e-18 */
					/* 0 < p < 1/64 (at x = 5.5) */
	u = log__D(x);
	u.a -= 1.0;
	v.a = (x -= .5);
	TRUNC(v.a);
	v.b = x - v.a;
	t.a = v.a*u.a;			/* t = (x-.5)*(log(x)-1) */
	t.b = v.b*u.a + x*u.b;
	/* return t.a + t.b + lns2pi_hi + lns2pi_lo + p */
	t.b += lns2pi_lo; t.b += p;	/* small pieces ( < 1/64, assuming t < 1e14) */
	u.a = lns2pi_hi + t.b; u.a += t.a;
	u.b = t.a - u.a;
	u.b += lns2pi_hi; u.b += t.b;
	return (u);
}
/* Good to < 1 ulp.  (provably .90 ulp; .87 ulp on 1,000,000 runs.)
   It also has correct monotonicity.
 */
static double
small_gam(x)
	double x;
{
	double y, t;
	struct Double yy, r;
	pt = ((long *) &t)+1;
	pra = ((long *) &r.a)+1;
	y = x - 1;
	if (y <= 1.0 + (LEFT + x0)) {
		yy = ratfun_gam(y - x0, 0);
		return (yy.a + yy.b);
	}
	r.a = y--;
	TRUNC(r.a);
	yy.a = r.a - 1.0;
	yy.b = r.b = y - yy.a;
	for (; --y > 1.0 + (LEFT + x0); yy.a--) {
			t = r.a*yy.a;
			r.b = r.a*yy.b + y*r.b;
			r.a = t + r.b;
			TRUNC(r.a);
			t -= r.a;
			r.b += t;
	}				/* now want r * gamma(y); */
	yy = ratfun_gam(y - x0, 0);
	y = r.b*(yy.a+yy.b) + r.a*yy.b;
	y += yy.a*r.a;
	return (y);
}
/* Good on (0, 1+x0+LEFT].  Accurate to 1ulp on [.25+x0+LEFT, 1+x0+LEFT].
 * Below this, x+LEFT-x0 introduces additional rounding errror of .5ulp.
 */
static double
smaller_gam(x)
	double x;
{
	double t, d;
	struct Double r, xx;
	if (x < x0 + LEFT) {
		t = x, TRUNC(t);
		d = (t+x)*(x-t);
		t *= t;
		xx.a = ((d+t) + x), TRUNC(xx.a);
		xx.b = x - xx.a; xx.b += t; xx.b += d;
		t = (1.0-x0); t += x;
		d = (1.0-x0); d -= t; d += x;
		x = xx.a + xx.b;
	} else {
		xx.a =  x, TRUNC(xx.a);
		xx.b = x - xx.a;
		t = x - x0;
		d = (-x0 -t); d += x;
	}
	r = ratfun_gam(t, d);
	d = r.a/x, TRUNC(d);
	r.a -= d*xx.a; r.a -= d*xx.b; r.a += r.b;
	return (d + r.a/x);
}
/* returns (z+c)^2 * P(z)/Q(z) + a0 */
static struct Double
ratfun_gam(z, c)
	double z, c;
{
	int i;
	double p, q, hi, lo;
	struct Double r;

	q = Q0 +z*(Q1+z*(Q2+z*(Q3+z*(Q4+z*(Q5+z*(Q6+z*(Q7+z*Q8)))))));
	p = P0 + z*(P1 + z*(P2 + z*(P3 + z*P4)));

	/* return r.a + r.b = a0 + (z+c)^2 * p/q, with r.a truncated to 26 bits. */
	p = p/q;
	hi = z, TRUNC(hi);		/* hi+lo ~= z + c */
		lo = z - hi; lo += c;
	lo *= (hi+z);			/* q+lo = (z+c)*(z+c) */
	q = hi*hi;
	hi = (q + lo), TRUNC(hi);	/* hi+lo = q+lo */
		q -= hi;
		lo += q;
	z = p, TRUNC(z);		/* z+q = p */
		q = p - z;
	lo = lo*p + hi*q + a0_lo;
	hi *= z;
	r.a = hi + a0_hi, TRUNC(r.a);
	r.b = ((a0_hi-r.a) + hi) + lo;
	return (r);
}
#define lpi_hi 1.1447298858494001638
#define lpi_lo .0000000000000000102659511627078262
/* Error: within 3.5 ulp for x < 171.  For large x, see lgamma. */
static double
neg_gam(x)
	double x;
{
	int sgn = 1;
	struct Double lg, lsine;
	double y, z, one = 1.0, zero = 0.0;
	y = floor(x + .5);
	if (y == x) {
		if (-x == infinity()) {	/* G(-inf) = NaN */
			errno = EDOM;
			return (signaling_nan(0));
		}
		errno = ERANGE;
		return (one/zero);	/* G(-(integer) -> oo */
	}
	z = fabs(x - y);
	y = ceil(x);
	if (y*.5 == ceil(.5*y)) {
		sgn = -1;
		printf("neg\n");
	}
	if (x < 1 - (6 + x0 + LEFT)) {
		if(x < -190) {
			UNDERFL;
			z = .5*ceil(x);
			if (z==ceil(z)) return (-0);
			else return (0);
		}
		y = 1 - x;
		if (1 - y == x) {
			lg = large_gam(y);
			lsine = log__D(sin(M_PI*z));
		} else {
			x = -x;
			lg = large_gam(x);
			lsine = log__D(x*sin(M_PI*z));
		}
		lg.b += lsine.b - lpi_lo;
		y = (-(lg.b + lsine.a) + lpi_hi) - lg.a;
		z = -lg.a - y; z+= lpi_hi; z -= lsine.a; z -= lg.b;
		y = exp__D(y, z);
		if(sgn < 0) y = -y;
		return (y);
	}
	y = 1-x;
	if (1-y == x)
		y = ngamma(y);
	else		/* 1-x is inexact */
		y = -x*ngamma(-x);
	if (sgn < 0) y = -y;
	return (M_PI / (y*sin(M_PI*z)));
}
