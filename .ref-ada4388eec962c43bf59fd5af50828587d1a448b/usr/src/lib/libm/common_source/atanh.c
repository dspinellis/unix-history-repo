/*
 * Copyright (c) 1985, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)atanh.c	8.1 (Berkeley) %G%";
#endif /* not lint */

/* ATANH(X)
 * RETURN THE HYPERBOLIC ARC TANGENT OF X
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85; 
 * REVISED BY K.C. NG on 2/7/85, 3/7/85, 8/18/85.
 *
 * Required kernel function:
 *	log1p(x) 	...return log(1+x)
 *
 * Method :
 *	Return 
 *                          1              2x                          x
 *		atanh(x) = --- * log(1 + -------) = 0.5 * log1p(2 * --------)
 *                          2             1 - x                      1 - x
 *
 * Special cases:
 *	atanh(x) is NaN if |x| > 1 with signal;
 *	atanh(NaN) is that NaN with no signal;
 *	atanh(+-1) is +-INF with signal.
 *
 * Accuracy:
 *	atanh(x) returns the exact hyperbolic arc tangent of x nearly rounded.
 *	In a test run with 512,000 random arguments on a VAX, the maximum
 *	observed error was 1.87 ulps (units in the last place) at
 *	x= -3.8962076028810414000e-03.
 */
#include "mathimpl.h"

#if defined(vax)||defined(tahoe)
#include <errno.h>
#endif	/* defined(vax)||defined(tahoe) */

double atanh(x)
double x;
{
	double z;
	z = copysign(0.5,x);
	x = copysign(x,1.0);
#if defined(vax)||defined(tahoe)
	if (x == 1.0) {
	    return(copysign(1.0,z)*infnan(ERANGE));	/* sign(x)*INF */
	}
#endif	/* defined(vax)||defined(tahoe) */
	x = x/(1.0-x);
	return( z*log1p(x+x) );
}
