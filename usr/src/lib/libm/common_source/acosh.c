/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * All recipients should regard themselves as participants in an ongoing
 * research project and hence should feel obligated to report their
 * experiences (good or bad) with these elementary function codes, using
 * the sendbug(8) program, to the authors.
 */

#ifndef lint
static char sccsid[] = "@(#)acosh.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/* ACOSH(X)
 * RETURN THE INVERSE HYPERBOLIC COSINE OF X
 * DOUBLE PRECISION (VAX D FORMAT 56 BITS, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 2/16/85;
 * REVISED BY K.C. NG on 3/6/85, 3/24/85, 4/16/85, 8/17/85.
 *
 * Required system supported functions :
 *	sqrt(x)
 *
 * Required kernel function:
 *	log1p(x) 		...return log(1+x)
 *
 * Method :
 *	Based on 
 *		acosh(x) = log [ x + sqrt(x*x-1) ]
 *	we have
 *		acosh(x) := log1p(x)+ln2,	if (x > 1.0E20); else		
 *		acosh(x) := log1p( sqrt(x-1) * (sqrt(x-1) + sqrt(x+1)) ) .
 *	These formulae avoid the over/underflow complication.
 *
 * Special cases:
 *	acosh(x) is NaN with signal if x<1.
 *	acosh(NaN) is NaN without signal.
 *
 * Accuracy:
 *	acosh(x) returns the exact inverse hyperbolic cosine of x nearly 
 *	rounded. In a test run with 512,000 random arguments on a VAX, the
 *	maximum observed error was 3.30 ulps (units of the last place) at
 *	x=1.0070493753568216 .
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#if defined(vax)||defined(tahoe)	/* VAX D format */
#ifdef vax
#define _0x(A,B)	0x/**/A/**/B
#else	/* vax */
#define _0x(A,B)	0x/**/B/**/A
#endif	/* vax */
/* static double */
/* ln2hi  =  6.9314718055829871446E-1    , Hex  2^  0   *  .B17217F7D00000 */
/* ln2lo  =  1.6465949582897081279E-12   ; Hex  2^-39   *  .E7BCD5E4F1D9CC */
static long     ln2hix[] = { _0x(7217,4031), _0x(0000,f7d0)};
static long     ln2lox[] = { _0x(bcd5,2ce7), _0x(d9cc,e4f1)};
#define    ln2hi    (*(double*)ln2hix)
#define    ln2lo    (*(double*)ln2lox)
#else	/* defined(vax)||defined(tahoe) */
static double
ln2hi  =  6.9314718036912381649E-1    , /*Hex  2^ -1   *  1.62E42FEE00000 */
ln2lo  =  1.9082149292705877000E-10   ; /*Hex  2^-33   *  1.A39EF35793C76 */
#endif	/* defined(vax)||defined(tahoe) */

double acosh(x)
double x;
{	
	double log1p(),sqrt(),t,big=1.E20; /* big+1==big */

#if !defined(vax)&&!defined(tahoe)
	if(x!=x) return(x);	/* x is NaN */
#endif	/* !defined(vax)&&!defined(tahoe) */

    /* return log1p(x) + log(2) if x is large */
	if(x>big) {t=log1p(x)+ln2lo; return(t+ln2hi);} 

	t=sqrt(x-1.0);
	return(log1p(t*(t+sqrt(x+1.0))));
}
