From Prof. Kahan at UC at Berkeley
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
static char sccsid[] = "@(#)atanh.c	1.1 (Berkeley) %G%";
#endif not lint

/* ATANH(X)
 * RETURN THE HYPERBOLIC ARC TANGENT OF X
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85; 
 * REVISED BY K.C. NG on 2/7/85, 3/7/85.
 *
 * Required kernel function:
 *	L(x) 	...return log(1+x)
 *
 * Method :
 *	Return 
 *                         log(1+x) - log(1-x)   L(x) - L(-x)
 *		atanh(x) = ------------------- = ------------ .
 *                                  2                 2
 *
 * Special cases:
 *	atanh(x) is NAN if |x| > 1 with signal;
 *	atanh(NAN) is that NAN with no signal;
 *	atanh(+-1) is +-INF with signal.
 *
 * Accuracy:
 *	atanh(x) returns the exact hyperbolic arc tangent of x nearly rounded.
 *	In a test run with 200,000 random arguments on a VAX, the maximum
 *	observed error was 1.45 ulps (units in the last place).
 */

double atanh(x)
double x;
{
	double L();
	return( (L(x)-L(-x))/2.0 );
}
