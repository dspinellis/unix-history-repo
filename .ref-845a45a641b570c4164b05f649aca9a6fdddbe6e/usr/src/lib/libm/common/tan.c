/*
 * Copyright (c) 1987, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)tan.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include "trig.h"
double
tan(x) 
double x;
{
	double a,z,ss,cc,c;
	int k;

	if(!finite(x))		/* tan(NaN) and tan(INF) must be NaN */
		return x-x;
	x = drem(x,PI);			/* reduce x into [-PI/2, PI/2] */
	a = copysign(x,one);		/* ... = abs(x) */
	if (a >= PIo4) {
		k = 1;
		x = copysign(PIo2-a,x);
	}
	else {
		k = 0;
		if (a < small) {
			big+a;
			return x;
		}
	}
	z = x*x;
	cc = cos__C(z);
	ss = sin__S(z);
	z *= half;			/* Next get c = cos(x) accurately */
	c = (z >= thresh ? half-((z-half)-cc) : one-(z-cc));
	if (k == 0)
		return x+(x*(z-(cc-ss)))/c;	/* ... sin/cos */
#ifdef national
	else if (x == zero)
		return copysign(fmax,x);	/* no inf on 32k */
#endif	/* national */
	else
		return c/(x+x*ss);		/* ... cos/sin */
}
