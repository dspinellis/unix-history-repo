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
"@(#)trig.c	1.2 (Berkeley) 8/22/85; 1.6 (ucb.elefunt) %G%";
#endif not lint

/* SIN(X), COS(X), TAN(X)
 * RETURN THE SINE, COSINE, AND TANGENT OF X RESPECTIVELY
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85; 
 * REVISED BY W. Kahan and K.C. NG, 8/17/85.
 *
 * Required system supported functions:
 *      copysign(x,y)
 *      finite(x)
 *      drem(x,p)
 *
 * Static kernel functions:
 *      sin__S(z)       ....sin__S(x*x) return (sin(x)-x)/x
 *      cos__C(z)       ....cos__C(x*x) return cos(x)-1-x*x/2
 *
 * Method.
 *      Let S and C denote the polynomial approximations to sin and cos 
 *      respectively on [-PI/4, +PI/4].
 *
 *      SIN and COS:
 *      1. Reduce the argument into [-PI , +PI] by the remainder function.  
 *      2. For x in (-PI,+PI), there are three cases:
 *			case 1:	|x| < PI/4
 *			case 2:	PI/4 <= |x| < 3PI/4
 *			case 3:	3PI/4 <= |x|.
 *	   SIN and COS of x are computed by:
 *
 *                   sin(x)      cos(x)       remark
 *     ----------------------------------------------------------
 *        case 1     S(x)         C(x)       
 *        case 2 sign(x)*C(y)     S(y)      y=PI/2-|x|
 *        case 3     S(y)        -C(y)      y=sign(x)*(PI-|x|)
 *     ----------------------------------------------------------
 *
 *      TAN:
 *      1. Reduce the argument into [-PI/2 , +PI/2] by the remainder function.  
 *      2. For x in (-PI/2,+PI/2), there are two cases:
 *			case 1:	|x| < PI/4
 *			case 2:	PI/4 <= |x| < PI/2
 *         TAN of x is computed by:
 *
 *                   tan (x)            remark
 *     ----------------------------------------------------------
 *        case 1     S(x)/C(x)
 *        case 2     C(y)/S(y)     y=sign(x)*(PI/2-|x|)
 *     ----------------------------------------------------------
 *
 *   Notes:
 *      1. S(y) and C(y) were computed by:
 *              S(y) = y+y*sin__S(y*y) 
 *              C(y) = 1-(y*y/2-cos__C(x*x))          ... if y*y/2 <  thresh,
 *                   = 0.5-((y*y/2-0.5)-cos__C(x*x))  ... if y*y/2 >= thresh.
 *         where
 *              thresh = 0.5*(acos(3/4)**2)
 *
 *      2. For better accuracy, we use the following formula for S/C for tan
 *         (k=0): let ss=sin__S(y*y), and cc=cos__C(y*y), then
 *
 *                            y+y*ss             (y*y/2-cc)+ss
 *             S(y)/C(y)   = -------- = y + y * ---------------.
 *                               C                     C 
 *
 *
 * Special cases:
 *      Let trig be any of sin, cos, or tan.
 *      trig(+-INF)  is NaN, with signals;
 *      trig(NaN)    is that NaN;
 *      trig(n*PI/2) is exact for any integer n, provided n*PI is 
 *      representable; otherwise, trig(x) is inexact. 
 *
 * Accuracy:
 *      trig(x) returns the exact trig(x*pi/PI) nearly rounded, where
 *
 *      Decimal:
 *              pi = 3.141592653589793 23846264338327 ..... 
 *    53 bits   PI = 3.141592653589793 115997963 ..... ,
 *    56 bits   PI = 3.141592653589793 227020265 ..... ,  
 *
 *      Hexadecimal:
 *              pi = 3.243F6A8885A308D313198A2E....
 *    53 bits   PI = 3.243F6A8885A30  =  2 * 1.921FB54442D18    error=.276ulps
 *    56 bits   PI = 3.243F6A8885A308 =  4 * .C90FDAA22168C2    error=.206ulps
 *
 *      In a test run with 1,024,000 random arguments on a VAX, the maximum
 *      observed errors (compared with the exact trig(x*pi/PI)) were
 *                      tan(x) : 2.09 ulps (around 4.716340404662354)
 *                      sin(x) : .861 ulps
 *                      cos(x) : .857 ulps
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#if (defined(VAX)||defined(TAHOE))
#ifdef VAX
#define _0x(A,B)	0x/**/A/**/B
#else	/* VAX */
#define _0x(A,B)	0x/**/B/**/A
#endif	/* VAX */
/*thresh =  2.6117239648121182150E-1    , Hex  2^ -1   *  .85B8636B026EA0 */
/*PIo4   =  7.8539816339744830676E-1    , Hex  2^  0   *  .C90FDAA22168C2 */
/*PIo2   =  1.5707963267948966135E0     , Hex  2^  1   *  .C90FDAA22168C2 */
/*PI3o4  =  2.3561944901923449203E0     , Hex  2^  2   *  .96CBE3F9990E92 */
/*PI     =  3.1415926535897932270E0     , Hex  2^  2   *  .C90FDAA22168C2 */
/*PI2    =  6.2831853071795864540E0     ; Hex  2^  3   *  .C90FDAA22168C2 */
static long    threshx[] = { _0x(b863,3f85), _0x(6ea0,6b02)};
#define   thresh    (*(double*)threshx)
static long      PIo4x[] = { _0x(0fda,4049), _0x(68c2,a221)};
#define     PIo4    (*(double*)PIo4x)
static long      PIo2x[] = { _0x(0fda,40c9), _0x(68c2,a221)};
#define     PIo2    (*(double*)PIo2x)
static long      PI3o4x[] = { _0x(cbe3,4116), _0x(0e92,f999)};
#define     PI3o4    (*(double*)PI3o4x)
static long        PIx[] = { _0x(0fda,4149), _0x(68c2,a221)};
#define       PI    (*(double*)PIx)
static long       PI2x[] = { _0x(0fda,41c9), _0x(68c2,a221)};
#define      PI2    (*(double*)PI2x)
#else   /* IEEE double  */
static double
thresh =  2.6117239648121182150E-1    , /*Hex  2^ -2   *  1.0B70C6D604DD4 */
PIo4   =  7.8539816339744827900E-1    , /*Hex  2^ -1   *  1.921FB54442D18 */
PIo2   =  1.5707963267948965580E0     , /*Hex  2^  0   *  1.921FB54442D18 */
PI3o4  =  2.3561944901923448370E0     , /*Hex  2^  1   *  1.2D97C7F3321D2 */
PI     =  3.1415926535897931160E0     , /*Hex  2^  1   *  1.921FB54442D18 */
PI2    =  6.2831853071795862320E0     ; /*Hex  2^  2   *  1.921FB54442D18 */
#ifdef NATIONAL
static long    fmaxx[] = { 0xffffffff, 0x7fefffff};
#define   fmax    (*(double*)fmaxx)
#endif	/* NATIONAL */
#endif
static double zero=0, one=1, negone= -1, half=1.0/2.0, 
	      small=1E-10, /* 1+small**2==1; better values for small:
					small = 1.5E-9 for VAX D
					      = 1.2E-8 for IEEE Double
					      = 2.8E-10 for IEEE Extended */
	      big=1E20;    /* big = 1/(small**2) */

double tan(x) 
double x;
{
        double copysign(),drem(),cos__C(),sin__S(),a,z,ss,cc,c;
	int k;
#if (!defined(VAX)&&!defined(TAHOE))
        extern int finite();
        /* tan(NaN) and tan(INF) must be NaN */
            if(!finite(x))  return(x-x);
#endif
        x=drem(x,PI);        /* reduce x into [-PI/2, PI/2] */
        a=copysign(x,one);   /* ... = abs(x) */
	if ( a >= PIo4 ) {k=1; x = copysign( PIo2 - a , x ); }
	   else { k=0; if(a < small ) { big + a; return(x); }}

        z  = x*x;
        cc = cos__C(z);
        ss = sin__S(z);
	z  = z*half ;		/* Next get c = cos(x) accurately */
	c  = (z >= thresh )? half-((z-half)-cc) : one-(z-cc);
	if (k==0) return ( x + (x*(z-(cc-ss)))/c );  /* sin/cos */
#ifdef NATIONAL
	else if(x==0.0) return copysign(fmax,x);  /* no inf on 32k */
#endif	/* NATIONAL */
	else return( c/(x+x*ss) );	/*          ... cos/sin */


}
double sin(x)
double x;
{
        double copysign(),drem(),sin__S(),cos__C(),a,c,z;
#if (!defined(VAX)&&!defined(TAHOE))
        extern int finite();
        /* sin(NaN) and sin(INF) must be NaN */
            if(!finite(x))  return(x-x);
#endif
	x=drem(x,PI2);         /*    reduce x into [-PI, PI] */
        a=copysign(x,one);
	if( a >= PIo4 ) {
	     if( a >= PI3o4 )   /* 	.. in [3PI/4,  PI ]  */
		x=copysign((a=PI-a),x);

	     else {	       /* 	.. in [PI/4, 3PI/4]  */
		a=PIo2-a;      /* return sign(x)*C(PI/2-|x|) */
		z=a*a;
		c=cos__C(z);
		z=z*half;
		a=(z>=thresh)?half-((z-half)-c):one-(z-c);
		return(copysign(a,x));
		}
             }

        /* return S(x) */
            if( a < small) { big + a; return(x);}
            return(x+x*sin__S(x*x));
}

double cos(x) 
double x;
{
        double copysign(),drem(),sin__S(),cos__C(),a,c,z,s=1.0;
#if (!defined(VAX)&&!defined(TAHOE))
        extern int finite();
        /* cos(NaN) and cos(INF) must be NaN */
            if(!finite(x))  return(x-x);
#endif
	x=drem(x,PI2);         /*    reduce x into [-PI, PI] */
        a=copysign(x,one);
	if ( a >= PIo4 ) {
	     if ( a >= PI3o4 )  /* 	.. in [3PI/4,  PI ]  */
		{ a=PI-a; s= negone; }

	     else 	       /* 	.. in [PI/4, 3PI/4]  */
                               /*        return  S(PI/2-|x|) */ 
		{ a=PIo2-a; return(a+a*sin__S(a*a));}
	     }


        /* return s*C(a) */
            if( a < small) { big + a; return(s);}
	    z=a*a;
	    c=cos__C(z);
	    z=z*half;
	    a=(z>=thresh)?half-((z-half)-c):one-(z-c);
	    return(copysign(a,s));
}


/* sin__S(x*x)
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

#if (defined(VAX)||defined(TAHOE))
/*S0     = -1.6666666666666646660E-1    , Hex  2^ -2   * -.AAAAAAAAAAAA71 */
/*S1     =  8.3333333333297230413E-3    , Hex  2^ -6   *  .8888888888477F */
/*S2     = -1.9841269838362403710E-4    , Hex  2^-12   * -.D00D00CF8A1057 */
/*S3     =  2.7557318019967078930E-6    , Hex  2^-18   *  .B8EF1CA326BEDC */
/*S4     = -2.5051841873876551398E-8    , Hex  2^-25   * -.D73195374CE1D3 */
/*S5     =  1.6028995389845827653E-10   , Hex  2^-32   *  .B03D9C6D26CCCC */
/*S6     = -6.2723499671769283121E-13   ; Hex  2^-40   * -.B08D0B7561EA82 */
static long        S0x[] = { _0x(aaaa,bf2a), _0x(aa71,aaaa)};
#define       S0    (*(double*)S0x)
static long        S1x[] = { _0x(8888,3d08), _0x(477f,8888)};
#define       S1    (*(double*)S1x)
static long        S2x[] = { _0x(0d00,ba50), _0x(1057,cf8a)};
#define       S2    (*(double*)S2x)
static long        S3x[] = { _0x(ef1c,3738), _0x(bedc,a326)};
#define       S3    (*(double*)S3x)
static long        S4x[] = { _0x(3195,b3d7), _0x(e1d3,374c)};
#define       S4    (*(double*)S4x)
static long        S5x[] = { _0x(3d9c,3030), _0x(cccc,6d26)};
#define       S5    (*(double*)S5x)
static long        S6x[] = { _0x(8d0b,ac30), _0x(ea82,7561)};
#define       S6    (*(double*)S6x)
#else	/* IEEE double  */
static double
S0     = -1.6666666666666463126E-1    , /*Hex  2^ -3   * -1.555555555550C */
S1     =  8.3333333332992771264E-3    , /*Hex  2^ -7   *  1.111111110C461 */
S2     = -1.9841269816180999116E-4    , /*Hex  2^-13   * -1.A01A019746345 */
S3     =  2.7557309793219876880E-6    , /*Hex  2^-19   *  1.71DE3209CDCD9 */
S4     = -2.5050225177523807003E-8    , /*Hex  2^-26   * -1.AE5C0E319A4EF */
S5     =  1.5868926979889205164E-10   ; /*Hex  2^-33   *  1.5CF61DF672B13 */
#endif

static double sin__S(z)
double z;
{
#if (defined(VAX)||defined(TAHOE))
	return(z*(S0+z*(S1+z*(S2+z*(S3+z*(S4+z*(S5+z*S6)))))));
#else 	/* IEEE double */
	return(z*(S0+z*(S1+z*(S2+z*(S3+z*(S4+z*S5))))));
#endif
}


/* cos__C(x*x)
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

#if (defined(VAX)||defined(TAHOE))
/*C0     =  4.1666666666666504759E-2    , Hex  2^ -4   *  .AAAAAAAAAAA9F0 */
/*C1     = -1.3888888888865302059E-3    , Hex  2^ -9   * -.B60B60B60A0CCA */
/*C2     =  2.4801587285601038265E-5    , Hex  2^-15   *  .D00D00CDCD098F */
/*C3     = -2.7557313470902390219E-7    , Hex  2^-21   * -.93F27BB593E805 */
/*C4     =  2.0875623401082232009E-9    , Hex  2^-28   *  .8F74C8FA1E3FF0 */
/*C5     = -1.1355178117642986178E-11   ; Hex  2^-36   * -.C7C32D0A5C5A63 */
static long        C0x[] = { _0x(aaaa,3e2a), _0x(a9f0,aaaa)};
#define       C0    (*(double*)C0x)
static long        C1x[] = { _0x(0b60,bbb6), _0x(0cca,b60a)};
#define       C1    (*(double*)C1x)
static long        C2x[] = { _0x(0d00,38d0), _0x(098f,cdcd)};
#define       C2    (*(double*)C2x)
static long        C3x[] = { _0x(f27b,b593), _0x(e805,b593)};
#define       C3    (*(double*)C3x)
static long        C4x[] = { _0x(74c8,320f), _0x(3ff0,fa1e)};
#define       C4    (*(double*)C4x)
static long        C5x[] = { _0x(c32d,ae47), _0x(5a63,0a5c)};
#define       C5    (*(double*)C5x)
#else	/* IEEE double  */
static double
C0     =  4.1666666666666504759E-2    , /*Hex  2^ -5   *  1.555555555553E */
C1     = -1.3888888888865301516E-3    , /*Hex  2^-10   * -1.6C16C16C14199 */
C2     =  2.4801587269650015769E-5    , /*Hex  2^-16   *  1.A01A01971CAEB */
C3     = -2.7557304623183959811E-7    , /*Hex  2^-22   * -1.27E4F1314AD1A */
C4     =  2.0873958177697780076E-9    , /*Hex  2^-29   *  1.1EE3B60DDDC8C */
C5     = -1.1250289076471311557E-11   ; /*Hex  2^-37   * -1.8BD5986B2A52E */
#endif

static double cos__C(z)
double z;
{
	return(z*z*(C0+z*(C1+z*(C2+z*(C3+z*(C4+z*C5))))));
}
