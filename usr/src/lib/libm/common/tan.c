/* 
 * Copyright (c) 1987 Regents of the University of California.
 * 
 * Use and reproduction of this software are granted  in  accordance  with
 * the terms and conditions specified in  the  Berkeley  Software  License
 * Agreement (in particular, this entails acknowledgement of the programs'
 * source, and inclusion of this notice) with the additional understanding
 * that  all  recipients  should regard themselves as participants  in  an
 * ongoing  research  project and hence should  feel  obligated  to report
 * their  experiences (good or bad) with these elementary function  codes,
 * using "sendbug 4bsd-bugs@BERKELEY", to the authors.
 */

#ifndef lint
static char sccsid[] = "@(#)tan.c	1.1	1.1 (ucb.elefunt) %G%";
#endif	/* not lint */

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
