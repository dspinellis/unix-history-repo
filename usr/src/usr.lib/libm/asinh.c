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
static char sccsid[] = "@(#)asinh.c	1.2 (Berkeley) 8/21/85";
#endif not lint

/* ASINH(X)
 * RETURN THE INVERSE HYPERBOLIC SINE OF X
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 2/16/85;
 * REVISED BY K.C. NG on 3/7/85, 3/24/85, 4/16/85.
 *
 * Required system supported functions :
 *	copysign(x,y)
 *	sqrt(x)
 *
 * Required kernel function:
 *	log1p(x) 		...return log(1+x)
 *
 * Method :
 *	Based on 
 *		asinh(x) = sign(x) * log [ |x| + sqrt(x*x+1) ]
 *	we have
 *	asinh(x) := x  if  1+x*x=1,
 *		 := sign(x)*(log1p(x)+ln2))	 if sqrt(1+x*x)=x, else
 *		 := sign(x)*log1p(|x| + |x|/(1/|x| + sqrt(1+(1/|x|)^2)) )  
 *
 * Accuracy:
 *	asinh(x) returns the exact inverse hyperbolic sine of x nearly rounded.
 *	In a test run with 52,000 random arguments on a VAX, the maximum 
 *	observed error was 1.58 ulps (units in the last place).
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#ifdef VAX	/* VAX D format */
/* static double */
/* ln2hi  =  6.9314718055829871446E-1    , Hex  2^  0   *  .B17217F7D00000 */
/* ln2lo  =  1.6465949582897081279E-12   ; Hex  2^-39   *  .E7BCD5E4F1D9CC */
static long     ln2hix[] = { 0x72174031, 0x0000f7d0};
static long     ln2lox[] = { 0xbcd52ce7, 0xd9cce4f1};
#define    ln2hi    (*(double*)ln2hix)
#define    ln2lo    (*(double*)ln2lox)
#else	/* IEEE double */
static double
ln2hi  =  6.9314718036912381649E-1    , /*Hex  2^ -1   *  1.62E42FEE00000 */
ln2lo  =  1.9082149292705877000E-10   ; /*Hex  2^-33   *  1.A39EF35793C76 */
#endif

double asinh(x)
double x;
{	
	double copysign(),log1p(),sqrt(),t,s;
	static double small=1.0E-10,	/* fl(1+small*small) == 1 */
		      big  =1.0E20,	/* fl(1+big) == big */
		      one  =1.0   ;	

#ifndef VAX
	if(x!=x) return(x);	/* x is NaN */
#endif
	if((t=copysign(x,one))>small) 
	    if(t<big) {
	     	s=one/t; return(copysign(log1p(t+t/(s+sqrt(one+s*s))),x)); }
	    else	/* if |x| > big */
		{s=log1p(t)+ln2lo; return(copysign(s+ln2hi,x));}
	else	/* if |x| < small */
	    return(x);
}
