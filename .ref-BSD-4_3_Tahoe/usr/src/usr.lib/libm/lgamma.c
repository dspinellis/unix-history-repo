/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)lgamma.c	5.2 (Berkeley) 4/29/88";
#endif /* not lint */

/*
	C program for floating point log Gamma function

	lgamma(x) computes the log of the absolute
	value of the Gamma function.
	The sign of the Gamma function is returned in the
	external quantity signgam.

	The coefficients for expansion around zero
	are #5243 from Hart & Cheney; for expansion
	around infinity they are #5404.

	Calls log, floor and sin.
*/

#include <math.h>
#if defined(vax)||defined(tahoe)
#include <errno.h>
#endif	/* defined(vax)||defined(tahoe) */
int	signgam = 0;
static double goobie	= 0.9189385332046727417803297;	/* log(2*pi)/2 */
static double pi	= 3.1415926535897932384626434;

#define M 6
#define N 8
static double p1[] = {
	0.83333333333333101837e-1,
	-.277777777735865004e-2,
	0.793650576493454e-3,
	-.5951896861197e-3,
	0.83645878922e-3,
	-.1633436431e-2,
};
static double p2[] = {
	-.42353689509744089647e5,
	-.20886861789269887364e5,
	-.87627102978521489560e4,
	-.20085274013072791214e4,
	-.43933044406002567613e3,
	-.50108693752970953015e2,
	-.67449507245925289918e1,
	0.0,
};
static double q2[] = {
	-.42353689509744090010e5,
	-.29803853309256649932e4,
	0.99403074150827709015e4,
	-.15286072737795220248e4,
	-.49902852662143904834e3,
	0.18949823415702801641e3,
	-.23081551524580124562e2,
	0.10000000000000000000e1,
};

double
lgamma(arg)
double arg;
{
	double log(), pos(), neg(), asym();

	signgam = 1.;
	if(arg <= 0.) return(neg(arg));
	if(arg > 8.) return(asym(arg));
	return(log(pos(arg)));
}

static double
asym(arg)
double arg;
{
	double log();
	double n, argsq;
	int i;

	argsq = 1./(arg*arg);
	for(n=0,i=M-1; i>=0; i--){
		n = n*argsq + p1[i];
	}
	return((arg-.5)*log(arg) - arg + goobie + n/arg);
}

static double
neg(arg)
double arg;
{
	double t;
	double log(), sin(), floor(), pos();

	arg = -arg;
     /*
      * to see if arg were a true integer, the old code used the
      * mathematically correct observation:
      * sin(n*pi) = 0 <=> n is an integer.
      * but in finite precision arithmetic, sin(n*PI) will NEVER
      * be zero simply because n*PI is a rational number.  hence
      *	it failed to work with our newer, more accurate sin()
      * which uses true pi to do the argument reduction...
      *	temp = sin(pi*arg);
      */
	t = floor(arg);
	if (arg - t  > 0.5e0)
	    t += 1.e0;				/* t := integer nearest arg */
#if defined(vax)||defined(tahoe)
	if (arg == t) {
	    extern double infnan();
	    return(infnan(ERANGE));		/* +INF */
	}
#endif	/* defined(vax)||defined(tahoe) */
	signgam = (int) (t - 2*floor(t/2));	/* signgam =  1 if t was odd, */
						/*            0 if t was even */
	signgam = signgam - 1 + signgam;	/* signgam =  1 if t was odd, */
						/*           -1 if t was even */
	t = arg - t;				/*  -0.5 <= t <= 0.5 */
	if (t < 0.e0) {
	    t = -t;
	    signgam = -signgam;
	}
	return(-log(arg*pos(arg)*sin(pi*t)/pi));
}

static double
pos(arg)
double arg;
{
	double n, d, s;
	register i;

	if(arg < 2.) return(pos(arg+1.)/arg);
	if(arg > 3.) return((arg-1.)*pos(arg-1.));

	s = arg - 2.;
	for(n=0,d=0,i=N-1; i>=0; i--){
		n = n*s + p2[i];
		d = d*s + q2[i];
	}
	return(n/d);
}
