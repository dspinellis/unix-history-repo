/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * All recipients should regard themselves as participants in an ongoing
 * research project and hence should feel obligated to report their
 * experiences (good or bad) with these elementary function codes, using
 * the sendbug(8) program, to the authors.
 */

#ifndef lint
static char sccsid[] = "@(#)sinh.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

/* SINH(X)
 * RETURN THE HYPERBOLIC SINE OF X
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85; 
 * REVISED BY K.C. NG on 2/8/85, 3/7/85, 3/24/85, 4/16/85.
 *
 * Required system supported functions :
 *	copysign(x,y)
 *	scalb(x,N)
 *
 * Required kernel functions:
 *	expm1(x)	...return exp(x)-1
 *
 * Method :
 *	1. reduce x to non-negative by sinh(-x) = - sinh(x).
 *	2. 
 *
 *	                                      expm1(x) + expm1(x)/(expm1(x)+1)
 *	    0 <= x <= lnovfl     : sinh(x) := --------------------------------
 *			       		                      2
 *     lnovfl <= x <= lnovfl+ln2 : sinh(x) := expm1(x)/2 (avoid overflow)
 * lnovfl+ln2 <  x <  INF        :  overflow to INF
 *	
 *
 * Special cases:
 *	sinh(x) is x if x is +INF, -INF, or NaN.
 *	only sinh(0)=0 is exact for finite argument.
 *
 * Accuracy:
 *	sinh(x) returns the exact hyperbolic sine of x nearly rounded. In
 *	a test run with 1,024,000 random arguments on a VAX, the maximum
 *	observed error was 1.93 ulps (units in the last place).
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#include "mathimpl.h"

vc(mln2hi, 8.8029691931113054792E1   ,0f33,43b0,2bdb,c7e2,   7, .B00F33C7E22BDB)
vc(mln2lo,-4.9650192275318476525E-16 ,1b60,a70f,582a,279e, -50,-.8F1B60279E582A)
vc(lnovfl, 8.8029691931113053016E1   ,0f33,43b0,2bda,c7e2,   7, .B00F33C7E22BDA)

ic(mln2hi, 7.0978271289338397310E2,    10, 1.62E42FEFA39EF)
ic(mln2lo, 2.3747039373786107478E-14, -45, 1.ABC9E3B39803F)
ic(lnovfl, 7.0978271289338397310E2,     9, 1.62E42FEFA39EF)

#ifdef vccast
#define	mln2hi	vccast(mln2hi)
#define	mln2lo	vccast(mln2lo)
#define	lnovfl	vccast(lnovfl)
#endif

#if defined(vax)||defined(tahoe)
static max = 126                      ;
#else	/* defined(vax)||defined(tahoe) */
static max = 1023                     ;
#endif	/* defined(vax)||defined(tahoe) */


double sinh(x)
double x;
{
	static const double  one=1.0, half=1.0/2.0 ;
	double t, sign;
#if !defined(vax)&&!defined(tahoe)
	if(x!=x) return(x);	/* x is NaN */
#endif	/* !defined(vax)&&!defined(tahoe) */
	sign=copysign(one,x);
	x=copysign(x,one);
	if(x<lnovfl)
	    {t=expm1(x); return(copysign((t+t/(one+t))*half,sign));}

	else if(x <= lnovfl+0.7)
		/* subtract x by ln(2^(max+1)) and return 2^max*exp(x) 
	    		to avoid unnecessary overflow */
	    return(copysign(scalb(one+expm1((x-mln2hi)-mln2lo),max),sign));

	else  /* sinh(+-INF) = +-INF, sinh(+-big no.) overflow to +-INF */
	    return( expm1(x)*sign );
}
