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
static char sccsid[] = "@(#)sinh.c	4.2 (Berkeley) %G%";
#endif not lint

/* SINH(X)
 * RETURN THE HYPERBOLIC SINE OF X
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85; 
 * REIVESD BY K.C. NG on 2/8/85, 3/7/85, 3/24/85, 4/16/85.
 *
 * Required system supported functions :
 *	copysign(x,y)
 *	scalb(x,N)
 *
 * Required kernel functions:
 *	E(x)	...return exp(x)-1
 *
 * Method :
 *	1. reduce x to non-negative by sinh(-x) = - sinh(x).
 *	2. 
 *
 *		                                     E(x) + E(x)/(E(x)+1)
 *	    0        <= x <= lnovfl    :  sinh(x) := ----------------------
 *			       			              2
 *	    lnovfl   <= x <= lnovfl+ln2:  sinh(x) := E(x)/2 (avoid overflow)
 *	    lnovfl+ln2 <  x <  INF     :  overflow to INF
 *	
 *
 * Special cases:
 *	sinh(x) is x if x is +INF, -INF, or NAN.
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
#ifdef VAX
/* double static */
/* mln2hi =  8.8029691931113054792E1     , Hex  2^  7   *  .B00F33C7E22BDB */
/* mln2lo = -4.9650192275318476525E-16   , Hex  2^-50   * -.8F1B60279E582A */
/* lnovfl =  8.8029691931113053016E1     ; Hex  2^  7   *  .B00F33C7E22BDA */
static long    mln2hix[] = { 0x0f3343b0, 0x2bdbc7e2};
static long    mln2lox[] = { 0x1b60a70f, 0x582a279e};
static long    lnovflx[] = { 0x0f3343b0, 0x2bdac7e2};
#define   mln2hi    (*(double*)mln2hix)
#define   mln2lo    (*(double*)mln2lox)
#define   lnovfl    (*(double*)lnovflx)
#else	/* IEEE double */
double static 
mln2hi =  7.0978271289338397310E2     , /*Hex  2^ 10   *  1.62E42FEFA39EF */
mln2lo =  2.3747039373786107478E-14   , /*Hex  2^-45   *  1.ABC9E3B39803F */
lnovfl =  7.0978271289338397310E2     ; /*Hex  2^  9   *  1.62E42FEFA39EF */
#endif

#ifdef VAX
static max = 126                      ;
#else	/* IEEE double */
static max = 1023                     ;
#endif


double sinh(x)
double x;
{
	static double  one=1.0, half=1.0/2.0 ;
	double E(), t, scalb(), copysign(), sign;
	if(x!=x) return(x);
	sign=copysign(one,x);
	x=copysign(x,one);
	if(x<lnovfl)
	    {t=E(x); return(copysign((t+t/(one+t))*half,sign));}

	else if(x <= lnovfl+0.7)
		/* subtract x by ln(2^(max+1)) and return 2^max*exp(x) 
	    		to avoid unnecessary overflow */
	    return(copysign(scalb(one+E((x-mln2hi)-mln2lo),max),sign));

	else  /* sinh(+-INF) = +-INF, sinh(+-big no.) overflow to +-INF */
	    return( E(x)*sign );
}
