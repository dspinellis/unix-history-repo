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
static char sccsid[] =
"@(#)exp.c	4.3 (Berkeley) 8/21/85; 1.8 (ucb.elefunt) %G%";
#endif	/* not lint */

/* EXP(X)
 * RETURN THE EXPONENTIAL OF X
 * DOUBLE PRECISION (IEEE 53 bits, VAX D FORMAT 56 BITS)
 * CODED IN C BY K.C. NG, 1/19/85; 
 * REVISED BY K.C. NG on 2/6/85, 2/15/85, 3/7/85, 3/24/85, 4/16/85, 6/14/86.
 *
 * Required system supported functions:
 *	scalb(x,n)	
 *	copysign(x,y)	
 *	finite(x)
 *
 * Method:
 *	1. Argument Reduction: given the input x, find r and integer k such 
 *	   that
 *	                   x = k*ln2 + r,  |r| <= 0.5*ln2 .  
 *	   r will be represented as r := z+c for better accuracy.
 *
 *	2. Compute exp(r) by 
 *
 *		exp(r) = 1 + r + r*R1/(2-R1),
 *	   where
 *		R1 = x - x^2*(p1+x^2*(p2+x^2*(p3+x^2*(p4+p5*x^2)))).
 *
 *	3. exp(x) = 2^k * exp(r) .
 *
 * Special cases:
 *	exp(INF) is INF, exp(NaN) is NaN;
 *	exp(-INF)=  0;
 *	for finite argument, only exp(0)=1 is exact.
 *
 * Accuracy:
 *	exp(x) returns the exponential of x nearly rounded. In a test run
 *	with 1,156,000 random arguments on a VAX, the maximum observed
 *	error was 0.869 ulps (units in the last place).
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
/* ln2lo  =  1.6465949582897081279E-12   , Hex  2^-39   *  .E7BCD5E4F1D9CC */
/* lnhuge =  9.4961163736712506989E1     , Hex  2^  7   *  .BDEC1DA73E9010 */
/* lntiny = -9.5654310917272452386E1     , Hex  2^  7   * -.BF4F01D72E33AF */
/* invln2 =  1.4426950408889634148E0     ; Hex  2^  1   *  .B8AA3B295C17F1 */
/* p1     =  1.6666666666666602251E-1    , Hex  2^-2    *  .AAAAAAAAAAA9F1 */
/* p2     = -2.7777777777015591216E-3    , Hex  2^-8    * -.B60B60B5F5EC94 */
/* p3     =  6.6137563214379341918E-5    , Hex  2^-13   *  .8AB355792EF15F */
/* p4     = -1.6533902205465250480E-6    , Hex  2^-19   * -.DDEA0E2E935F84 */
/* p5     =  4.1381367970572387085E-8    , Hex  2^-24   *  .B1BB4B95F52683 */
static long     ln2hix[] = { _0x(7217,4031), _0x(0000,f7d0)};
static long     ln2lox[] = { _0x(bcd5,2ce7), _0x(d9cc,e4f1)};
static long    lnhugex[] = { _0x(ec1d,43bd), _0x(9010,a73e)};
static long    lntinyx[] = { _0x(4f01,c3bf), _0x(33af,d72e)};
static long    invln2x[] = { _0x(aa3b,40b8), _0x(17f1,295c)};
static long        p1x[] = { _0x(aaaa,3f2a), _0x(a9f1,aaaa)};
static long        p2x[] = { _0x(0b60,bc36), _0x(ec94,b5f5)};
static long        p3x[] = { _0x(b355,398a), _0x(f15f,792e)};
static long        p4x[] = { _0x(ea0e,b6dd), _0x(5f84,2e93)};
static long        p5x[] = { _0x(bb4b,3431), _0x(2683,95f5)};
#define    ln2hi    (*(double*)ln2hix)
#define    ln2lo    (*(double*)ln2lox)
#define   lnhuge    (*(double*)lnhugex)
#define   lntiny    (*(double*)lntinyx)
#define   invln2    (*(double*)invln2x)
#define       p1    (*(double*)p1x)
#define       p2    (*(double*)p2x)
#define       p3    (*(double*)p3x)
#define       p4    (*(double*)p4x)
#define       p5    (*(double*)p5x)

#else	/* defined(vax)||defined(tahoe)	*/
static double
p1     =  1.6666666666666601904E-1    , /*Hex  2^-3    *  1.555555555553E */
p2     = -2.7777777777015593384E-3    , /*Hex  2^-9    * -1.6C16C16BEBD93 */
p3     =  6.6137563214379343612E-5    , /*Hex  2^-14   *  1.1566AAF25DE2C */
p4     = -1.6533902205465251539E-6    , /*Hex  2^-20   * -1.BBD41C5D26BF1 */
p5     =  4.1381367970572384604E-8    , /*Hex  2^-25   *  1.6376972BEA4D0 */
ln2hi  =  6.9314718036912381649E-1    , /*Hex  2^ -1   *  1.62E42FEE00000 */
ln2lo  =  1.9082149292705877000E-10   , /*Hex  2^-33   *  1.A39EF35793C76 */
lnhuge =  7.1602103751842355450E2     , /*Hex  2^  9   *  1.6602B15B7ECF2 */
lntiny = -7.5137154372698068983E2     , /*Hex  2^  9   * -1.77AF8EBEAE354 */
invln2 =  1.4426950408889633870E0     ; /*Hex  2^  0   *  1.71547652B82FE */
#endif	/* defined(vax)||defined(tahoe)	*/

double exp(x)
double x;
{
	double scalb(), copysign(), z,hi,lo,c;
	int k,finite();

#if !defined(vax)&&!defined(tahoe)
	if(x!=x) return(x);	/* x is NaN */
#endif	/* !defined(vax)&&!defined(tahoe) */
	if( x <= lnhuge ) {
		if( x >= lntiny ) {

		    /* argument reduction : x --> x - k*ln2 */

			k=invln2*x+copysign(0.5,x);	/* k=NINT(x/ln2) */

		    /* express x-k*ln2 as hi-lo and let x=hi-lo rounded */

			hi=x-k*ln2hi;
			x=hi-(lo=k*ln2lo);

		    /* return 2^k*[1+x+x*c/(2+c)]  */
			z=x*x;
			c= x - z*(p1+z*(p2+z*(p3+z*(p4+z*p5))));
			return  scalb(1.0+(hi-(lo-(x*c)/(2.0-c))),k);

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
