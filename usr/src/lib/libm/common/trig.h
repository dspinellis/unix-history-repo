/* 
 * Copyright (c) 1987 Regents of the University of California.
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
/* @(#)trig.h	1.1	1.1 (ucb.elefunt) %G% */
#if defined(vax)||defined(tahoe)
#ifdef vax
#define _0x(A,B)	0x/**/A/**/B
#else	/* vax */
#define _0x(A,B)	0x/**/B/**/A
#endif	/* vax */
/*thresh =  2.6117239648121182150E-1    , Hex  2^ -1   *  .85B8636B026EA0 */
/*PIo4   =  7.8539816339744830676E-1    , Hex  2^  0   *  .C90FDAA22168C2 */
/*PIo2   =  1.5707963267948966135E0     , Hex  2^  1   *  .C90FDAA22168C2 */
/*PI3o4  =  2.3561944901923449203E0     , Hex  2^  2   *  .96CBE3F9990E92 */
/*PI     =  3.1415926535897932270E0     , Hex  2^  2   *  .C90FDAA22168C2 */
/*PI2    =  6.2831853071795864540E0     ; Hex  2^  3   *  .C90FDAA22168C2 */
static long threshx[]	= { _0x(b863,3f85), _0x(6ea0,6b02)};
static long PIo4x[]	= { _0x(0fda,4049), _0x(68c2,a221)};
static long PIo2x[]	= { _0x(0fda,40c9), _0x(68c2,a221)};
static long PI3o4x[]	= { _0x(cbe3,4116), _0x(0e92,f999)};
static long PIx[]	= { _0x(0fda,4149), _0x(68c2,a221)};
static long PI2x[]	= { _0x(0fda,41c9), _0x(68c2,a221)};
#define thresh	(*(double*)threshx)
#define PIo4	(*(double*)PIo4x)
#define PIo2	(*(double*)PIo2x)
#define PI3o4	(*(double*)PI3o4x)
#define PI	(*(double*)PIx)
#define PI2	(*(double*)PI2x)
#else   /* defined(vax)||defined(tahoe) */
static double
thresh	=  2.6117239648121182150E-1    , /*Hex  2^ -2   *  1.0B70C6D604DD4 */
PIo4	=  7.8539816339744827900E-1    , /*Hex  2^ -1   *  1.921FB54442D18 */
PIo2	=  1.5707963267948965580E0     , /*Hex  2^  0   *  1.921FB54442D18 */
PI3o4	=  2.3561944901923448370E0     , /*Hex  2^  1   *  1.2D97C7F3321D2 */
PI	=  3.1415926535897931160E0     , /*Hex  2^  1   *  1.921FB54442D18 */
PI2	=  6.2831853071795862320E0     ; /*Hex  2^  2   *  1.921FB54442D18 */
#ifdef national
static long fmaxx[]	= { 0xffffffff, 0x7fefffff};
#define   fmax    (*(double*)fmaxx)
#endif	/* national */
#endif	/* defined(vax)||defined(tahoe) */
static double
	zero = 0,
	one = 1,
	negone = -1,
	half = 1.0/2.0, 
	small = 1E-10,	/* 1+small**2 == 1; better values for small:
			 *		small	= 1.5E-9 for VAX D
			 *			= 1.2E-8 for IEEE Double
			 *			= 2.8E-10 for IEEE Extended
			 */
	big = 1E20;	/* big := 1/(small**2) */

/* sin__S(x*x) ... re-implemented as a macro
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * STATIC KERNEL FUNCTION OF SIN(X), COS(X), AND TAN(X) 
 * CODED IN C BY K.C. NG, 1/21/85; 
 * REVISED BY K.C. NG on 8/13/85.
 *
 *	    sin(x*k) - x
 * RETURN  --------------- on [-PI/4,PI/4] , where k=pi/PI, PI is the rounded
 *	            x	
 * value of pi in machine precision:
 *
 *	Decimal:
 *		pi = 3.141592653589793 23846264338327 ..... 
 *    53 bits   PI = 3.141592653589793 115997963 ..... ,
 *    56 bits   PI = 3.141592653589793 227020265 ..... ,  
 *
 *	Hexadecimal:
 *		pi = 3.243F6A8885A308D313198A2E....
 *    53 bits   PI = 3.243F6A8885A30  =  2 * 1.921FB54442D18
 *    56 bits   PI = 3.243F6A8885A308 =  4 * .C90FDAA22168C2    
 *
 * Method:
 *	1. Let z=x*x. Create a polynomial approximation to 
 *	    (sin(k*x)-x)/x  =  z*(S0 + S1*z^1 + ... + S5*z^5).
 *	Then
 *      sin__S(x*x) = z*(S0 + S1*z^1 + ... + S5*z^5)
 *
 *	The coefficient S's are obtained by a special Remez algorithm.
 *
 * Accuracy:
 *	In the absence of rounding error, the approximation has absolute error 
 *	less than 2**(-61.11) for VAX D FORMAT, 2**(-57.45) for IEEE DOUBLE. 
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 *
 */

#if defined(vax)||defined(tahoe)
/*S0     = -1.6666666666666646660E-1    , Hex  2^ -2   * -.AAAAAAAAAAAA71 */
/*S1     =  8.3333333333297230413E-3    , Hex  2^ -6   *  .8888888888477F */
/*S2     = -1.9841269838362403710E-4    , Hex  2^-12   * -.D00D00CF8A1057 */
/*S3     =  2.7557318019967078930E-6    , Hex  2^-18   *  .B8EF1CA326BEDC */
/*S4     = -2.5051841873876551398E-8    , Hex  2^-25   * -.D73195374CE1D3 */
/*S5     =  1.6028995389845827653E-10   , Hex  2^-32   *  .B03D9C6D26CCCC */
/*S6     = -6.2723499671769283121E-13   ; Hex  2^-40   * -.B08D0B7561EA82 */
static long S0x[]	= { _0x(aaaa,bf2a), _0x(aa71,aaaa)};
static long S1x[]	= { _0x(8888,3d08), _0x(477f,8888)};
static long S2x[]	= { _0x(0d00,ba50), _0x(1057,cf8a)};
static long S3x[]	= { _0x(ef1c,3738), _0x(bedc,a326)};
static long S4x[]	= { _0x(3195,b3d7), _0x(e1d3,374c)};
static long S5x[]	= { _0x(3d9c,3030), _0x(cccc,6d26)};
static long S6x[]	= { _0x(8d0b,ac30), _0x(ea82,7561)};
#define S0	(*(double*)S0x)
#define S1	(*(double*)S1x)
#define S2	(*(double*)S2x)
#define S3	(*(double*)S3x)
#define S4	(*(double*)S4x)
#define S5	(*(double*)S5x)
#define S6	(*(double*)S6x)
#else	/* IEEE double  */
static double
S0     = -1.6666666666666463126E-1    , /*Hex  2^ -3   * -1.555555555550C */
S1     =  8.3333333332992771264E-3    , /*Hex  2^ -7   *  1.111111110C461 */
S2     = -1.9841269816180999116E-4    , /*Hex  2^-13   * -1.A01A019746345 */
S3     =  2.7557309793219876880E-6    , /*Hex  2^-19   *  1.71DE3209CDCD9 */
S4     = -2.5050225177523807003E-8    , /*Hex  2^-26   * -1.AE5C0E319A4EF */
S5     =  1.5868926979889205164E-10   ; /*Hex  2^-33   *  1.5CF61DF672B13 */
#endif

#if defined(vax)||defined(tahoe)
#define sin__S(z)	(z*(S0+z*(S1+z*(S2+z*(S3+z*(S4+z*(S5+z*S6)))))))
#else 	/* defined(vax)||defined(tahoe) */
#define sin__S(z)	(z*(S0+z*(S1+z*(S2+z*(S3+z*(S4+z*S5))))))
#endif 	/* defined(vax)||defined(tahoe) */

/* cos__C(x*x) ... re-implemented as a macro
 * DOUBLE PRECISION (VAX D FORMAT 56 BITS, IEEE DOUBLE 53 BITS)
 * STATIC KERNEL FUNCTION OF SIN(X), COS(X), AND TAN(X) 
 * CODED IN C BY K.C. NG, 1/21/85; 
 * REVISED BY K.C. NG on 8/13/85.
 *
 *	   		    x*x	
 * RETURN   cos(k*x) - 1 + ----- on [-PI/4,PI/4],  where k = pi/PI,
 *	  		     2	
 * PI is the rounded value of pi in machine precision :
 *
 *	Decimal:
 *		pi = 3.141592653589793 23846264338327 ..... 
 *    53 bits   PI = 3.141592653589793 115997963 ..... ,
 *    56 bits   PI = 3.141592653589793 227020265 ..... ,  
 *
 *	Hexadecimal:
 *		pi = 3.243F6A8885A308D313198A2E....
 *    53 bits   PI = 3.243F6A8885A30  =  2 * 1.921FB54442D18
 *    56 bits   PI = 3.243F6A8885A308 =  4 * .C90FDAA22168C2    
 *
 *
 * Method:
 *	1. Let z=x*x. Create a polynomial approximation to 
 *	    cos(k*x)-1+z/2  =  z*z*(C0 + C1*z^1 + ... + C5*z^5)
 *	then
 *      cos__C(z) =  z*z*(C0 + C1*z^1 + ... + C5*z^5)
 *
 *	The coefficient C's are obtained by a special Remez algorithm.
 *
 * Accuracy:
 *	In the absence of rounding error, the approximation has absolute error 
 *	less than 2**(-64) for VAX D FORMAT, 2**(-58.3) for IEEE DOUBLE. 
 *	
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 *
 */

#if defined(vax)||defined(tahoe)
/*C0     =  4.1666666666666504759E-2    , Hex  2^ -4   *  .AAAAAAAAAAA9F0 */
/*C1     = -1.3888888888865302059E-3    , Hex  2^ -9   * -.B60B60B60A0CCA */
/*C2     =  2.4801587285601038265E-5    , Hex  2^-15   *  .D00D00CDCD098F */
/*C3     = -2.7557313470902390219E-7    , Hex  2^-21   * -.93F27BB593E805 */
/*C4     =  2.0875623401082232009E-9    , Hex  2^-28   *  .8F74C8FA1E3FF0 */
/*C5     = -1.1355178117642986178E-11   ; Hex  2^-36   * -.C7C32D0A5C5A63 */
static long C0x[]	= { _0x(aaaa,3e2a), _0x(a9f0,aaaa)};
static long C1x[]	= { _0x(0b60,bbb6), _0x(0cca,b60a)};
static long C2x[]	= { _0x(0d00,38d0), _0x(098f,cdcd)};
static long C3x[]	= { _0x(f27b,b593), _0x(e805,b593)};
static long C4x[]	= { _0x(74c8,320f), _0x(3ff0,fa1e)};
static long C5x[]	= { _0x(c32d,ae47), _0x(5a63,0a5c)};
#define C0	(*(double*)C0x)
#define C1	(*(double*)C1x)
#define C2	(*(double*)C2x)
#define C3	(*(double*)C3x)
#define C4	(*(double*)C4x)
#define C5	(*(double*)C5x)
#else	/* defined(vax)||defined(tahoe) */
static double
C0     =  4.1666666666666504759E-2    , /*Hex  2^ -5   *  1.555555555553E */
C1     = -1.3888888888865301516E-3    , /*Hex  2^-10   * -1.6C16C16C14199 */
C2     =  2.4801587269650015769E-5    , /*Hex  2^-16   *  1.A01A01971CAEB */
C3     = -2.7557304623183959811E-7    , /*Hex  2^-22   * -1.27E4F1314AD1A */
C4     =  2.0873958177697780076E-9    , /*Hex  2^-29   *  1.1EE3B60DDDC8C */
C5     = -1.1250289076471311557E-11   ; /*Hex  2^-37   * -1.8BD5986B2A52E */
#endif	/* defined(vax)||defined(tahoe) */

#define cos__C(z)	(z*z*(C0+z*(C1+z*(C2+z*(C3+z*(C4+z*C5))))))

extern int finite();
extern double copysign(),drem();
