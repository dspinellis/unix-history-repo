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
static char sccsid[] = "@(#)exp.c	4.3 (Berkeley) 8/21/85";
#endif not lint

/* EXP(X)
 * RETURN THE EXPONENTIAL OF X
 * DOUBLE PRECISION (IEEE 53 bits, VAX D FORMAT 56 BITS)
 * CODED IN C BY K.C. NG, 1/19/85; 
 * REVISED BY K.C. NG on 2/6/85, 2/15/85, 3/7/85, 3/24/85, 4/16/85.
 *
 * Required system supported functions:
 *	scalb(x,n)	
 *	copysign(x,y)	
 *	finite(x)
 *
 * Kernel function:
 *	exp__E(x,c)
 *
 * Method:
 *	1. Argument Reduction: given the input x, find r and integer k such 
 *	   that
 *	                   x = k*ln2 + r,  |r| <= 0.5*ln2 .  
 *	   r will be represented as r := z+c for better accuracy.
 *
 *	2. Compute expm1(r)=exp(r)-1 by 
 *
 *			expm1(r=z+c) := z + exp__E(z,r)
 *
 *	3. exp(x) = 2^k * ( expm1(r) + 1 ).
 *
 * Special cases:
 *	exp(INF) is INF, exp(NaN) is NaN;
 *	exp(-INF)=  0;
 *	for finite argument, only exp(0)=1 is exact.
 *
 * Accuracy:
 *	exp(x) returns the exponential of x nearly rounded. In a test run
 *	with 1,156,000 random arguments on a VAX, the maximum observed
 *	error was .768 ulps (units in the last place).
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#ifdef VAX	/* VAX D format */
/* double static */
/* ln2hi  =  6.9314718055829871446E-1    , Hex  2^  0   *  .B17217F7D00000 */
/* ln2lo  =  1.6465949582897081279E-12   , Hex  2^-39   *  .E7BCD5E4F1D9CC */
/* lnhuge =  9.4961163736712506989E1     , Hex  2^  7   *  .BDEC1DA73E9010 */
/* lntiny = -9.5654310917272452386E1     , Hex  2^  7   * -.BF4F01D72E33AF */
/* invln2 =  1.4426950408889634148E0     ; Hex  2^  1   *  .B8AA3B295C17F1 */
static long     ln2hix[] = { 0x72174031, 0x0000f7d0};
static long     ln2lox[] = { 0xbcd52ce7, 0xd9cce4f1};
static long    lnhugex[] = { 0xec1d43bd, 0x9010a73e};
static long    lntinyx[] = { 0x4f01c3bf, 0x33afd72e};
static long    invln2x[] = { 0xaa3b40b8, 0x17f1295c};
#define    ln2hi    (*(double*)ln2hix)
#define    ln2lo    (*(double*)ln2lox)
#define   lnhuge    (*(double*)lnhugex)
#define   lntiny    (*(double*)lntinyx)
#define   invln2    (*(double*)invln2x)
#else	/* IEEE double */
double static
ln2hi  =  6.9314718036912381649E-1    , /*Hex  2^ -1   *  1.62E42FEE00000 */
ln2lo  =  1.9082149292705877000E-10   , /*Hex  2^-33   *  1.A39EF35793C76 */
lnhuge =  7.1602103751842355450E2     , /*Hex  2^  9   *  1.6602B15B7ECF2 */
lntiny = -7.5137154372698068983E2     , /*Hex  2^  9   * -1.77AF8EBEAE354 */
invln2 =  1.4426950408889633870E0     ; /*Hex  2^  0   *  1.71547652B82FE */
#endif

double exp(x)
double x;
{
	double scalb(), copysign(), exp__E(), z,hi,lo,c;
	int k,finite();

#ifndef VAX
	if(x!=x) return(x);	/* x is NaN */
#endif
	if( x <= lnhuge ) {
		if( x >= lntiny ) {

		    /* argument reduction : x --> x - k*ln2 */

			k=invln2*x+copysign(0.5,x);	/* k=NINT(x/ln2) */

			/* express x-k*ln2 as z+c */
			hi=x-k*ln2hi;
			z=hi-(lo=k*ln2lo);
			c=(hi-z)-lo;

		    /* return 2^k*[expm1(x) + 1]  */
			z += exp__E(z,c);
			return (scalb(z+1.0,k));  
		}
		/* end of x > lntiny */

		else 
		     /* exp(-big#) underflows to zero */
		     if(finite(x))  return(scalb(1.0,-5000));

		     /* exp(-INF) is zero */
		     else return(0.0);
	}
	/* end of x < lnhuge */

	else 
	/* exp(INF) is INF, exp(+big#) overflows to INF */
	    return( finite(x) ?  scalb(1.0,5000)  : x);
}
