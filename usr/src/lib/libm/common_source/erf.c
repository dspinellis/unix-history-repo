/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)erf.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * ====================================================
 * Copyright (C) 1992 by Sun Microsystems, Inc.
 *
 * Developed at SunPro, a Sun Microsystems, Inc. business.
 * Permission to use, copy, modify, and distribute this
 * software is freely granted, provided that this notice 
 * is preserved.
 * ====================================================
 *
 * ******************* WARNING ********************
 * This is an alpha version of SunPro's FDLIBM (Freely
 * Distributable Math Library) for IEEE double precision 
 * arithmetic. FDLIBM is a basic math library written
 * in C that runs on machines that conform to IEEE 
 * Standard 754/854. This alpha version is distributed 
 * for testing purpose. Those who use this software 
 * should report any bugs to 
 *
 *		fdlibm-comments@sunpro.eng.sun.com
 *
 * -- K.C. Ng, Oct 12, 1992
 * ************************************************
 */

/* Modified Nov 30, 1992 P. McILROY:
 *	Replaced expansion for x > 6
 *	Add #ifdef's for vax/tahoe.
 */

/* double erf(double x)
 * double erfc(double x)
 *			     x
 *		      2      |\
 *     erf(x)  =  ---------  | exp(-t*t)dt
 *		   sqrt(pi) \|
 *			     0
 *
 *     erfc(x) =  1-erf(x)
 *
 * Method:
 *      1. Reduce x to |x| by erf(-x) = -erf(x)
 *	2. For x in [0, 0.84375]
 *	    erf(x)  = x + x*P(x^2)
 *          erfc(x) = 1 - erf(x)           if x<=0.25
 *                  = 0.5 + ((0.5-x)-x*P)  if x in [0.25,0.84375]
 *	   where
 *			2		 2	  4		  20  
 *              P =  P(x ) = (p0 + p1 * x + p2 * x + ... + p10 * x  )
 * 	   is an approximation to (erf(x)-x)/x with precision
 *
 *						 -56.45
 *			| P - (erf(x)-x)/x | <= 2
 *	
 *
 *	   Remark. The formula is derived by noting
 *          erf(x) = (2/sqrt(pi))*(x - x^3/3 + x^5/10 - x^7/42 + ....)
 *	   and that
 *          2/sqrt(pi) = 1.128379167095512573896158903121545171688
 *	   is close to one. The interval is chosen because the fixed
 *	   point of erf(x) is near 0.6174 (i.e., erf(x)=x when x is
 *	   near 0.6174), and by some experiment, 0.84375 is chosen to
 * 	   guarantee the error is less than one ulp for erf.
 *
 *      3. For x in [0.84375,1.25], let s = x - 1, and
 *         c = 0.84506291151 rounded to single (24 bits)
 *         	erf(x)  = c  + P1(s)/Q1(s)
 *         	erfc(x) = (1-c)  - P1(s)/Q1(s)
 *         	|P1/Q1 - (erf(x)-c)| <= 2**-59.06
 *	   Remark: here we use the taylor series expansion at x=1.
 *		erf(1+s) = erf(1) + s*Poly(s)
 *			 = 0.845.. + P1(s)/Q1(s)
 *	   That is, we use rational approximation to approximate
 *			erf(1+s) - (c = (single)0.84506291151)
 *	   Note that |P1/Q1|< 0.078 for x in [0.84375,1.25]
 *	   where 
 *		P1(s) = degree 7 poly in s
 *
 *      4. For x in [1.25,6],
 *         	erf(x)  = 1 - erfc(x)
 *		erfc(x) = exp(-x*x)*(1/x)*R1(1/x)/S1(1/x)
 *	where 
 *		R1(y) = degree 7 poly in y, (y=1/x)
 *		S1(y) = degree 8 poly in y
 *
 *      5. For x in [6,28]
 *         	erf(x)  = 1.0 - tiny
 *		erfc(x)	= (1/x)exp(-x*x-(.5*log(pi)+eps) + zP(z))
 *
 *	Where P is degree 9 polynomial in z = 1/(x*x)
 *
 *      Notes:
 *	   Here 4 and 5 make use of the asymptotic series
 *			  exp(-x*x)
 *		erfc(x) ~ ---------- * ( 1 + Poly(1/x^2) );
 *			  x*sqrt(pi)
 *
 *		where for z = 1/(x*x)
 *		P(z) ~ z/2*(-1 + z*3/2*(1 + z*5/2*(-1 + z*7/2*(1 +...))))
 *
 *	   Thus we use rational approximation to approximate
 *              erfc*x*exp(x*x) ~ 1/sqrt(pi) 
 *
 *		The error bound for the target function, G(z) for
 *		case 5 is
 * 		|eps + 1/(x*x)P(1/x*x) - G(x)|	< 2**(-58.34)
 *		For case 4,
 *      	|R2/S2 - erfc*x*exp(x*x)|	< 2**(-61.52)
 *
 *      6. For inf > x >= 28
 *         	erf(x)  = 1 - tiny  (raise inexact)
 *         	erfc(x) = tiny*tiny (raise underflow)
 *
 *      7. Special cases:
 *         	erf(0)  = 0, erf(inf)  = 1, erf(-inf) = -1,
 *         	erfc(0) = 1, erfc(inf) = 0, erfc(-inf) = 2, 
 *	   	erfc/erf(NaN) is NaN
 */

#if defined(vax) || defined(tahoe)
#define _IEEE	0
#define TRUNC(x) (double) (float) (x)
#else
#define _IEEE	1
#define TRUNC(x) *(((int *) &x) + 1) &= 0xf8000000
#define infnan(x) 0.0
#endif

#ifdef _IEEE_LIBM
/*
 * redefining "___function" to "function" in _IEEE_LIBM mode
 */
#include "ieee_libm.h"
#endif

static double
tiny	    = 1e-300,
half	    = 0.5,
one	    = 1.0,
two	    = 2.0,
c 	    = 8.45062911510467529297e-01, /* (float)0.84506291151 */
/*
 * Coefficients for approximation to  erf on [0,0.84375]
 */
p0t8 = 1.02703333676410051049867154944018394163280,
p0 =   1.283791670955125638123339436800229927041e-0001,
p1 =  -3.761263890318340796574473028946097022260e-0001,
p2 =   1.128379167093567004871858633779992337238e-0001,
p3 =  -2.686617064084433642889526516177508374437e-0002,
p4 =   5.223977576966219409445780927846432273191e-0003,
p5 =  -8.548323822001639515038738961618255438422e-0004,
p6 =   1.205520092530505090384383082516403772317e-0004,
p7 =  -1.492214100762529635365672665955239554276e-0005,
p8 =   1.640186161764254363152286358441771740838e-0006,
p9 =  -1.571599331700515057841960987689515895479e-0007,
p10=   1.073087585213621540635426191486561494058e-0008,
/*
 * Coefficients for approximation to  erf  in [0.84375,1.25] 
 */
pa0 =  -2.362118560752659485957248365514511540287e-0003,
pa1 =   4.148561186837483359654781492060070469522e-0001,
pa2 =  -3.722078760357013107593507594535478633044e-0001,
pa3 =   3.183466199011617316853636418691420262160e-0001,
pa4 =  -1.108946942823966771253985510891237782544e-0001,
pa5 =   3.547830432561823343969797140537411825179e-0002,
pa6 =  -2.166375594868790886906539848893221184820e-0003,
qa1 =   1.064208804008442270765369280952419863524e-0001,
qa2 =   5.403979177021710663441167681878575087235e-0001,
qa3 =   7.182865441419627066207655332170665812023e-0002,
qa4 =   1.261712198087616469108438860983447773726e-0001,
qa5 =   1.363708391202905087876983523620537833157e-0002,
qa6 =   1.198449984679910764099772682882189711364e-0002,
/*
 * Coefficients for approximation to  erfc in [1.25,6]
 */
ra0 =   5.641895806197543833169694096883621225329e-0001,
ra1 =   7.239004794325021293310782759791744583987e+0000,
ra2 =   4.615482605646378370356340497765510677914e+0001,
ra3 =   1.831130716384318567879039478746072928548e+0002,
ra4 =   4.827304689401256945023566678442020977744e+0002,
ra5 =   8.443683805001379929687313735294340282751e+0002,
ra6 =   9.151771804289399937165800774604677980269e+0002,
ra7 =   4.884236881266866025539987843147838061930e+0002,
sa1 =   1.283080158932067675016971332625972882793e+0001,
sa2 =   8.230730944985601552133528541648529041935e+0001,
sa3 =   3.309746710535947168967275132570416337810e+0002,
sa4 =   8.960238586988354676031385802384985611536e+0002,
sa5 =   1.652440076836585407285764071805622271834e+0003,
sa6 =   2.010492426273281289533320672757992672142e+0003,
sa7 =   1.466304171232599681829476641652969136592e+0003,
sa8 =   4.884237022526160104676542187698268809111e+0002;
/*
 * Coefficients for approximation to  erfc in [6,28]
 */
#define a0_hi	-0.5723649429247001929610405568		/* ~-.5log(pi) */
#define a0_lo	-0.0000000000000000189783711362898601

#define P0	-4.99999999999749700219098258458e-0001	/* -1	   /2 	*/
#define P1	 6.24999999807451578348604925850e-0001	/* 5/2	   /4	*/
#define P2	-1.54166659013994022942029005208e+0001	/* -37/3   /8	*/
#define P3	 5.51560710872094706047619183664e+0001	/* 353/4   /16	*/
#define P4	-2.55036053070125880992691236315e+0002	/* -4081/5 /32	*/
#define P5	 1.43505282730286381820405949838e+0002	/* 55205/6 /64	*/
#define P6	-9.36421869861889035746571607888e+0002	/* ....etc....	*/
#define P7	 6.51030087738772090233396738768e+0003	
#define P8	-3.98835620275180117459967732430e+0004
#define P9	 1.44460450428346201078966259956e+0005


double erf(x)
	double x;
{
	double R,S,P,Q,ax,s,y,z,odd,even,r,fabs(),exp();
	if(!finite(x)) {		/* erf(nan)=nan */
	    if (isnan(x))
		return(x);
	    return (x > 0 ? one : -one); /* erf(+/-inf)= +/-1 */
	}
	if ((ax = x) < 0)
		ax = - ax;
	if (ax < .84375) {
	    if (ax < 3.7e-09) {
		if (ax < 1.0e-308)
		    return 0.125*(8.0*x+p0t8*x);  /*avoid underflow */
		return x + p0*x;
	    }
	    y = x*x;
	    z = y*y;
	    even = z*(p2+z*(p4+z*(p6+z*(p8+z*p10))));
	    odd  = p1+z*(p3+z*(p5+z*(p7+z*p9)));
	    r = y*odd+even;
	    return x + x*(p0+r);
	}
	if (ax < 1.25) {		/* 0.84375 <= |x| < 1.25 */
	    s = fabs(x)-one;
	    P = pa0+s*(pa1+s*(pa2+s*(pa3+s*(pa4+s*(pa5+s*pa6)))));
	    Q = one+s*(qa1+s*(qa2+s*(qa3+s*(qa4+s*(qa5+s*qa6)))));
	    if (x>=0)
		return (c + P/Q);
	    else
		return (-c - P/Q);
	}
	if (ax >= 6.0) {		/* inf>|x|>=6 */
	    if (x >= 0.0)
		return (one-tiny);
	    else
		return (tiny-one);
	}
    /* 1.25 <= |x| < 6 */
 	s = one/fabs(x);
	R=ra0+s*(ra1+s*(ra2+s*(ra3+s*(ra4+s*(ra5+s*(ra6+s*ra7))))));
	S=one+s*(sa1+s*(sa2+s*(sa3+s*(sa4+s*(sa5+s*(sa6+s*(sa7+s*sa8)))))));
	z = exp(-x*x)*(R/S)*s;
	if (x >= 0)
		return (one-z);
	else
		return (z-one);
}

double erfc(x) 
	double x;
{
	double R,S,P,Q,s,ax,y,odd,even,z,r,fabs(),exp__D();
	if (!finite(x)) {
		if (isnan(x))		/* erfc(NaN) = NaN */
			return(x);
		else if (x > 0)		/* erfc(+-inf)=0,2 */
			return 0.0;
		else
			return 2.0;
	}
	if ((ax = x) < 0)
		ax = -ax;
	if (ax < .84375) {			/* |x|<0.84375 */
	    if (ax < 1.38777878078144568e-17)  	/* |x|<2**-56 */
		return one-x;
	    y = x*x;
	    z = y*y;
	    even = z*(p2+z*(p4+z*(p6+z*(p8+z*p10))));
	    odd  = p1+z*(p3+z*(p5+z*(p7+z*p9)));
	    r = y*odd+even;
	    if (ax < .0625) {  	/* |x|<2**-4 */
		return (one-(x+x*(p0+r)));
	    } else {
		r = x*(p0+r);
		r += (x-half);
	        return (half - r);
	    }
	}
	if (ax < 1.25) {		/* 0.84375 <= |x| < 1.25 */
	    s = ax-one;
	    P = pa0+s*(pa1+s*(pa2+s*(pa3+s*(pa4+s*(pa5+s*pa6)))));
	    Q = one+s*(qa1+s*(qa2+s*(qa3+s*(qa4+s*(qa5+s*qa6)))));
	    if (x>=0) {
	        z  = one-c; return z - P/Q; 
	    } else {
		z = c+P/Q; return one+z;
	    }
	}
	if (ax >= 28)	/* Out of range */
 		if (x>0)
			return (tiny*tiny);
		else
			return (two-tiny);
	z = ax;
	TRUNC(z);
	y = z - ax; y *= (ax+z);
	z *= -z;			/* Here z + y = -x^2 */
	if (ax >= 6) {			/* 6 <= ax */
		s = one/(-z-y);		/* 1/(x*x) */
		R = s*(P0+s*(P1+s*(P2+s*(P3+s*(P4+
			s*(P5+s*(P6+s*(P7+s*(P8+s*P9)))))))));
		y += a0_lo;
	/* return exp(-x^2 + a0_hi + R)/x;	*/
		s = ((R + y) + a0_hi) + z;
		y = (((z-s) + a0_hi) + R) + y;
		r = exp__D(s, y)/x;
	} else {			/* 1.25 <= ax <= 6 */
		s = one/(ax);
	  	R=ra0+s*(ra1+s*(ra2+s*(ra3+s*(ra4+
			s*(ra5+s*(ra6+s*ra7))))));
		S=one+s*(sa1+s*(sa2+s*(sa3+s*(sa4+
			s*(sa5+s*(sa6+s*(sa7+s*sa8)))))));
	  	r = (R/S)/x;
		s = z + y; y = (z-s) + y;
		r *= exp__D(s, y);
	}
	if (x>0)
		return r;
	else
		return two-r;
}
