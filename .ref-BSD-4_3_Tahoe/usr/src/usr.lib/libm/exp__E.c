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
static char sccsid[] = "@(#)exp__E.c	5.3 (Berkeley) 6/30/88";
#endif /* not lint */

/* exp__E(x,c)
 * ASSUMPTION: c << x  SO THAT  fl(x+c)=x.
 * (c is the correction term for x)
 * exp__E RETURNS
 *
 *			 /  exp(x+c) - 1 - x ,  1E-19 < |x| < .3465736
 *       exp__E(x,c) = 	| 		     
 *			 \  0 ,  |x| < 1E-19.
 *
 * DOUBLE PRECISION (IEEE 53 bits, VAX D FORMAT 56 BITS)
 * KERNEL FUNCTION OF EXP, EXPM1, POW FUNCTIONS
 * CODED IN C BY K.C. NG, 1/31/85;
 * REVISED BY K.C. NG on 3/16/85, 4/16/85.
 *
 * Required system supported function:
 *	copysign(x,y)	
 *
 * Method:
 *	1. Rational approximation. Let r=x+c.
 *	   Based on
 *                                   2 * sinh(r/2)     
 *                exp(r) - 1 =   ----------------------   ,
 *                               cosh(r/2) - sinh(r/2)
 *	   exp__E(r) is computed using
 *                   x*x            (x/2)*W - ( Q - ( 2*P  + x*P ) )
 *                   --- + (c + x*[---------------------------------- + c ])
 *                    2                          1 - W
 * 	   where  P := p1*x^2 + p2*x^4,
 *	          Q := q1*x^2 + q2*x^4 (for 56 bits precision, add q3*x^6)
 *	          W := x/2-(Q-x*P),
 *
 *	   (See the listing below for the values of p1,p2,q1,q2,q3. The poly-
 *	    nomials P and Q may be regarded as the approximations to sinh
 *	    and cosh :
 *		sinh(r/2) =  r/2 + r * P  ,  cosh(r/2) =  1 + Q . )
 *
 *         The coefficients were obtained by a special Remez algorithm.
 *
 * Approximation error:
 *
 *   |	exp(x) - 1			   |        2**(-57),  (IEEE double)
 *   | ------------  -  (exp__E(x,0)+x)/x  |  <= 
 *   |	     x			           |	    2**(-69).  (VAX D)
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
/* p1     =  1.5150724356786683059E-2    , Hex  2^ -6   *  .F83ABE67E1066A */
/* p2     =  6.3112487873718332688E-5    , Hex  2^-13   *  .845B4248CD0173 */
/* q1     =  1.1363478204690669916E-1    , Hex  2^ -3   *  .E8B95A44A2EC45 */
/* q2     =  1.2624568129896839182E-3    , Hex  2^ -9   *  .A5790572E4F5E7 */
/* q3     =  1.5021856115869022674E-6    ; Hex  2^-19   *  .C99EB4604AC395 */
static long        p1x[] = { _0x(3abe,3d78), _0x(066a,67e1)};
static long        p2x[] = { _0x(5b42,3984), _0x(0173,48cd)};
static long        q1x[] = { _0x(b95a,3ee8), _0x(ec45,44a2)};
static long        q2x[] = { _0x(7905,3ba5), _0x(f5e7,72e4)};
static long        q3x[] = { _0x(9eb4,36c9), _0x(c395,604a)};
#define       p1    (*(double*)p1x)
#define       p2    (*(double*)p2x)
#define       q1    (*(double*)q1x)
#define       q2    (*(double*)q2x)
#define       q3    (*(double*)q3x)
#else	/* defined(vax)||defined(tahoe)	*/
static double 
p1     =  1.3887401997267371720E-2    , /*Hex  2^ -7   *  1.C70FF8B3CC2CF */
p2     =  3.3044019718331897649E-5    , /*Hex  2^-15   *  1.15317DF4526C4 */
q1     =  1.1110813732786649355E-1    , /*Hex  2^ -4   *  1.C719538248597 */
q2     =  9.9176615021572857300E-4    ; /*Hex  2^-10   *  1.03FC4CB8C98E8 */
#endif	/* defined(vax)||defined(tahoe)	*/

double exp__E(x,c)
double x,c;
{
	static double zero=0.0, one=1.0, half=1.0/2.0, small=1.0E-19;
	double copysign(),z,p,q,xp,xh,w;
	if(copysign(x,one)>small) {
           z = x*x  ;
	   p = z*( p1 +z* p2 );
#if defined(vax)||defined(tahoe)
           q = z*( q1 +z*( q2 +z* q3 ));
#else	/* defined(vax)||defined(tahoe) */
           q = z*( q1 +z*  q2 );
#endif	/* defined(vax)||defined(tahoe) */
           xp= x*p     ; 
	   xh= x*half  ;
           w = xh-(q-xp)  ;
	   p = p+p;
	   c += x*((xh*w-(q-(p+xp)))/(one-w)+c);
	   return(z*half+c);
	}
	/* end of |x| > small */

	else {
	    if(x!=zero) one+small;	/* raise the inexact flag */
	    return(copysign(zero,x));
	}
}
