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
static char sccsid[] = "@(#)trig.c	1.1 (Berkeley) %G%";
#endif not lint

/* SIN(X), COS(X), TAN(X)
 * RETURN THE SINE, COSINE, AND TANGENT OF X RESPECTIVELY
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85; 
 * REVISED BY K.C. NG on 1/31/85, 3/17/85, 3/24/85.
 *
 * Required system supported functions:
 *      copysign(x,y)
 *      finite(x)
 *      drem(x,p)
 *
 * Kernel functions:
 *      sin__S(z)       ....sin__S(x*x) return (sin(x)-x)/x
 *      cos__C(z)       ....cos__C(x*x) return cos(x)-1-x*x/2
 *
 * Method :
 *      Let S and C denote the polynomial approximations to sin and cos 
 *      respectively on [-PI/4, +PI/4].
 *      1. Reduce the argument into [-PI , +PI] by the remainder function.  
 *      2. For x in (-PI,+PI), let k=|x|*4/PI rounded. According to the value 
 *         of k,  sin, cos, and tan of x are computed by:
 *
 *         k         sin(x)      cos(x)       tan (x)        remark
 *     -----------------------------------------------------------------------
 *        k=0        S(x)         C(x)       S(x)/C(x)
 *        k=1,2  sign(x)*C(y)     S(y)   sign(x)*C(y)/S(y)  y=PI/2-|x|
 *        k=3        S(y)        -C(y)      S(-y)/C(-y)      y=sign(x)*(PI-|x|)
 *     -----------------------------------------------------------------------
 *
 *   Notes:
 *      1. S(y) and C(y) were computed by:
 *              S(y) = y+y*sin__S(y*y) 
 *              C(y) = 1-(y*y/2-cos__C(x*x))              ... if y*y <  0.5,
 *                   = 0.5-((y*y/2-0.5)-cos__C(x*x))      ... if y*y >= 0.5.
 *
 *      2. For better accuracy, we use the following formula for S/C for tan
 *         (k=0,3): let ss=sin__S(y*y), and cc=cos__C(y*y), then
 *
 *                            y+y*ss             (y*y/2-cc)+ss
 *             S(y)/C(y)   = -------- = y + y * ---------------.
 *                               C                     C 
 *
 *
 * Special cases:
 *      Let trig be any of sin, cos, or tan.
 *      trig(+-INF) is NAN, with signals;
 *      trig(NAN)   is that NAN;
 *      trig(n*PI/2) is exact for any integer n, provided n*PI is representable;
 *      otherwise, trig(x) is inexact. 
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
 *              pi = 3.243F6A8885A308D313198A2E....
 *    53 bits   PI = 3.243F6A8885A30  =  2 * 1.921FB54442D18    error=.276ulps
 *    56 bits   PI = 3.243F6A8885A308 =  4 * .C90FDAA22168C2    error=.206ulps
 *
 *      In a test run with 1,024,000 random arguments on a VAX, the maximum
 *      observed errors (compared with the exact trig(x*pi/PI)) were
 *                      tan(x) : 2.08 ulps (units in the last place)
 *                      sin(x) : .861 ulps
 *                      cos(x) : .857 ulps
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#ifdef VAX
/*thresh =  5.2234479296242364299E-1    , Hex  2^  0   *  .85B8636B026EA0 */
/*ivPIo4 =  1.2732395447351626816E0     , Hex  2^  1   *  .A2F9836E4E4415 */
/*PIo2   =  1.5707963267948966135E0     , Hex  2^  1   *  .C90FDAA22168C2 */
/*PI     =  3.1415926535897932270E0     , Hex  2^  2   *  .C90FDAA22168C2 */
/*PI2    =  6.2831853071795864540E0     ; Hex  2^  3   *  .C90FDAA22168C2 */
static long    threshx[] = { 0xb8634005, 0x6ea06b02};
#define   thresh    (*(double*)threshx)
static long    ivPIo4x[] = { 0xf98340a2, 0x44156e4e};
#define   ivPIo4    (*(double*)ivPIo4x)
static long      PIo2x[] = { 0x0fda40c9, 0x68c2a221};
#define     PIo2    (*(double*)PIo2x)
static long        PIx[] = { 0x0fda4149, 0x68c2a221};
#define       PI    (*(double*)PIx)
static long       PI2x[] = { 0x0fda41c9, 0x68c2a221};
#define      PI2    (*(double*)PI2x)
#else   /* IEEE double  */
static double
thresh =  5.2234479296242364299E-1    , /*Hex  2^ -1   *  1.0B70C6D604DD4 */
ivPIo4 =  1.2732395447351627649E0     , /*Hex  2^  0   *  1.45F306DC9C883 */
PIo2   =  1.5707963267948965580E0     , /*Hex  2^  0   *  1.921FB54442D18 */
PI     =  3.1415926535897931160E0     , /*Hex  2^  1   *  1.921FB54442D18 */
PI2    =  6.2831853071795862320E0     ; /*Hex  2^  2   *  1.921FB54442D18 */
#endif
static double zero=0, one=1, negone= -1, half=1.0/2.0, small=1E-9, big=1E20;

double tan(x) 
double x;
{
        double copysign(),drem(),cos__C(),sin__S(),a,z,ss,cc,t;
        int finite(),k;

        if(x!=x) return(x);              /* tan(NAN) is NAN */
        if(finite(x)) 
        {

                x=drem(x,PI2);        /* reduce x into [-PI, PI] */
                a=copysign(x,one);
                k=a*ivPIo4;

                switch(k) {
                case 0: break;

                case 1: 
                case 2: 
                    a=PIo2-a;
                    z=a*a;
                    cc=cos__C(z);
                    ss=copysign(a+a*sin__S(z),x);
                                        /* return C(y)/S(y), y=sign(x)*PI-x */
                    return
                    (((z>=thresh)?half-((z*half-half)-cc):one-(z*half-cc))/ss);

                default :
                    x = copysign(PI-a, -x);
                }


                                        /* return S/C */
                if(copysign(x,one) > small) 
                {
                    z=x*x;
                    cc = cos__C(z);
                    ss = sin__S(z);
                    t=z*half;
                    return 
                (x+(x*(t-(cc-ss)))/((z>=thresh)?half-((t-half)-cc):one-(t-cc)));
                }
                else /* tan(x) := x for small x (inexact if x is not zero) */
                    { if( x != zero ) big+small; return(x); } 
        }

        else
            return(zero/zero);      /* tan(INF) is NAN with signal */

}

double sin(x)
double x;
{
        double copysign(),drem(),sin__S(),cos__C(),a,c,z;
        int finite(),k;

        if(x!=x) return(x);              /* sin(NAN) is NAN */
        if(finite(x)) 
        {
                x=drem(x,PI2);        /* reduce x into [-PI, PI] */
                a=copysign(x,one);
                k=a*ivPIo4;

                switch(k) {             
                   case 0: break;

                   case 1: 
                   case 2: 
                        a=PIo2-a;
                        z=a*a;          
                        c=cos__C(z);
                        a=(z>=thresh)?half-((z*half-half)-c):one-(z*half-c);
                                        /* return sign(x)*C(PI/2-|x|) */
                        return(copysign(a,x));
                
                   default: 
                        x=copysign(PI-a,x);
                }

                                        /* return S(x) */
                   if(copysign(x,one) > small) return(x+x*sin__S(x*x));
                   else /* sin(x) := x for small x (inexact if x is not zero) */
                       { if( x != zero ) big+small; return(x); } 

        }

        else
            return(zero/zero);      /* sin(INF) is NAN with signal */

}

double cos(x) 
double x;
{
        double copysign(),drem(),sin__S(),cos__C(),a,c,z;
        int finite(),k;

        if(x!=x) return(x);              /* cos(NAN) is NAN */
        if(finite(x)) 
        {
                x=drem(x,PI2);        /* reduce x into [-PI, PI] */
                a=copysign(x,one);
                k=a*ivPIo4;

                switch(k) {             
                   case 0: x = one; break;

                   case 1: 
                   case 2: 
                        a=PIo2-a;
                                        /* return S(PI/2-|x|) */
                        return(a+a*sin__S(a*a));
                
                   default: 
                        a=PI-a; x= negone;
                }

                                        /* return C or -C */
                if(copysign(a,one) > small) 
                {
                    z=a*a;
                    c=cos__C(z);
                    a=(z>=thresh)?half-((z*half-half)-c):one-(z*half-c);
                    return(copysign(a,x));
                }
                else
                        /* cos(x) := 1 for small x (inexact if x is not zero) */
                    { if( x != zero ) big+small; return(copysign(one,x)); } 

        }

        else
            return(zero/zero);      /* cos(INF) is NAN with signal */

}


/* sin__S(x*x)
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * KERNEL FUNCTION OF SIN(X), COS(X), AND TAN(X) 
 * CODED IN C BY K.C. NG, 1/21/85; 
 * REVISED BY K.C. NG on 1/31/85, 3/7/85.
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

#ifdef VAX
/*S0     = -1.6666666666666646660E-1    , Hex  2^ -2   * -.AAAAAAAAAAAA71 */
/*S1     =  8.3333333333297230413E-3    , Hex  2^ -6   *  .8888888888477F */
/*S2     = -1.9841269838362403710E-4    , Hex  2^-12   * -.D00D00CF8A1057 */
/*S3     =  2.7557318019967078930E-6    , Hex  2^-18   *  .B8EF1CA326BEDC */
/*S4     = -2.5051841873876551398E-8    , Hex  2^-25   * -.D73195374CE1D3 */
/*S5     =  1.6028995389845827653E-10   , Hex  2^-32   *  .B03D9C6D26CCCC */
/*S6     = -6.2723499671769283121E-13   ; Hex  2^-40   * -.B08D0B7561EA82 */
static long        S0x[] = { 0xaaaabf2a, 0xaa71aaaa};
#define       S0    (*(double*)S0x)
static long        S1x[] = { 0x88883d08, 0x477f8888};
#define       S1    (*(double*)S1x)
static long        S2x[] = { 0x0d00ba50, 0x1057cf8a};
#define       S2    (*(double*)S2x)
static long        S3x[] = { 0xef1c3738, 0xbedca326};
#define       S3    (*(double*)S3x)
static long        S4x[] = { 0x3195b3d7, 0xe1d3374c};
#define       S4    (*(double*)S4x)
static long        S5x[] = { 0x3d9c3030, 0xcccc6d26};
#define       S5    (*(double*)S5x)
static long        S6x[] = { 0x8d0bac30, 0xea827561};
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

double sin__S(z)
double z;
{
#ifdef VAX
	return(z*(S0+z*(S1+z*(S2+z*(S3+z*(S4+z*(S5+z*S6)))))));
#else 	/* IEEE double */
	return(z*(S0+z*(S1+z*(S2+z*(S3+z*(S4+z*S5))))));
#endif
}


/* cos__C(x*x)
 * DOUBLE PRECISION (VAX D FORMAT 56 BITS, IEEE DOUBLE 53 BITS)
 * KERNEL FUNCTION OF SIN(X), COS(X), AND TAN(X) 
 * CODED IN C BY K.C. NG, 1/21/85; 
 * REIVSED BY K.C. NG on 1/31/85, 3/7/85.
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
 *	less that 2**(-64) for VAX D FORMAT, 2**(-58.3) for IEEE DOUBLE. 
 *	
 *
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 *
 */

#ifdef VAX
/*C0     =  4.1666666666666504759E-2    , Hex  2^ -4   *  .AAAAAAAAAAA9F0 */
/*C1     = -1.3888888888865302059E-3    , Hex  2^ -9   * -.B60B60B60A0CCA */
/*C2     =  2.4801587285601038265E-5    , Hex  2^-15   *  .D00D00CDCD098F */
/*C3     = -2.7557313470902390219E-7    , Hex  2^-21   * -.93F27BB593E805 */
/*C4     =  2.0875623401082232009E-9    , Hex  2^-28   *  .8F74C8FA1E3FF0 */
/*C5     = -1.1355178117642986178E-11   ; Hex  2^-36   * -.C7C32D0A5C5A63 */
static long        C0x[] = { 0xaaaa3e2a, 0xa9f0aaaa};
#define       C0    (*(double*)C0x)
static long        C1x[] = { 0x0b60bbb6, 0x0ccab60a};
#define       C1    (*(double*)C1x)
static long        C2x[] = { 0x0d0038d0, 0x098fcdcd};
#define       C2    (*(double*)C2x)
static long        C3x[] = { 0xf27bb593, 0xe805b593};
#define       C3    (*(double*)C3x)
static long        C4x[] = { 0x74c8320f, 0x3ff0fa1e};
#define       C4    (*(double*)C4x)
static long        C5x[] = { 0xc32dae47, 0x5a630a5c};
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

double cos__C(z)
double z;
{
	return(z*z*(C0+z*(C1+z*(C2+z*(C3+z*(C4+z*C5))))));
}
