/* 
 * Copyright (c) 1985 Regents of the University of California.
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
static char sccsid[] = "@(#)tanh.c	4.2 (Berkeley) %G%";
#endif not lint

/* TANH(X)
 * RETURN THE HYPERBOLIC TANGENT OF X
 * DOUBLE PRECISION (VAX D FORMAT 56 BITS, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85; 
 * REVISED BY K.C. NG on 2/8/85, 2/11/85, 3/7/85, 3/24/85.
 *
 * Required system supported functions :
 *	copysign(x,y)
 *	finite(x)
 *
 * Required kernel function:
 *	E(x)	...exp(x)-1
 *
 * Method :
 *	1. reduce x to non-negative by tanh(-x) = - tanh(x).
 *	2. For appropriate values of small, 
 *					          -E(-2x)
 *	    0     <  x <=     1   :  tanh(x) := ------------
 *					         E(-2x) + 2
 *							 2
 *	    1     <= x <= 22.0    :  tanh(x) := 1 -  ------------
 *						      E(2x) + 2
 *	    22.0  <  x <= INF     :  tanh(x) := 1.
 *
 *	Note: 22 are chosen so that fl(1.0+2/(E(2*22)+2)) == 1.
 *
 * Special cases:
 *	tanh(NAN) is NAN;
 *	only tanh(0)=0 is exact for finite argument.
 *
 * Accuracy:
 *	tanh(x) returns the exact hyperbolic tangent of x nealy rounded.
 *	In a test run with 1,024,000 random arguments on a VAX, the maximum
 *	observed error was 2.22 ulps (units in the last place).
 */

double tanh(x)
double x;
{
	static double one=1.0, two=2.0;
	double E(), t,  copysign(), sign;
	int finite();

	if(x!=x) return(x);

	sign=copysign(one,x);
	x=copysign(x,one);
	if(x < 22.0) 
	    if( x > one )
		return(copysign(one-two/(E(x+x)+two),sign));
	    else
		{t= -E(-(x+x)); return(copysign(t/(two-t),sign));}

	else if(finite(x))
	    return (sign+1.0E-37); /* raise the inexact flag */

	else
	    return(sign);	/* x is +- INF */
}

