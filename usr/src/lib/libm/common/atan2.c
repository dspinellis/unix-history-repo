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
"@(#)atan2.c	1.3 (Berkeley) 8/21/85; 5.1 (ucb.elefunt) %G%";
#endif	/* not lint */

/* ATAN2(Y,X)
 * RETURN ARG (X+iY)
 * DOUBLE PRECISION (VAX D format 56 bits, IEEE DOUBLE 53 BITS)
 * CODED IN C BY K.C. NG, 1/8/85; 
 * REVISED BY K.C. NG on 2/7/85, 2/13/85, 3/7/85, 3/30/85, 6/29/85.
 *
 * Required system supported functions :
 *	copysign(x,y)
 *	scalb(x,y)
 *	logb(x)
 *	
 * Method :
 *	1. Reduce y to positive by atan2(y,x)=-atan2(-y,x).
 *	2. Reduce x to positive by (if x and y are unexceptional): 
 *		ARG (x+iy) = arctan(y/x)   	   ... if x > 0,
 *		ARG (x+iy) = pi - arctan[y/(-x)]   ... if x < 0,
 *	3. According to the integer k=4t+0.25 truncated , t=y/x, the argument 
 *	   is further reduced to one of the following intervals and the 
 *	   arctangent of y/x is evaluated by the corresponding formula:
 *
 *         [0,7/16]	   atan(y/x) = t - t^3*(a1+t^2*(a2+...(a10+t^2*a11)...)
 *	   [7/16,11/16]    atan(y/x) = atan(1/2) + atan( (y-x/2)/(x+y/2) )
 *	   [11/16.19/16]   atan(y/x) = atan( 1 ) + atan( (y-x)/(x+y) )
 *	   [19/16,39/16]   atan(y/x) = atan(3/2) + atan( (y-1.5x)/(x+1.5y) )
 *	   [39/16,INF]     atan(y/x) = atan(INF) + atan( -x/y )
 *
 * Special cases:
 * Notations: atan2(y,x) == ARG (x+iy) == ARG(x,y).
 *
 *	ARG( NAN , (anything) ) is NaN;
 *	ARG( (anything), NaN ) is NaN;
 *	ARG(+(anything but NaN), +-0) is +-0  ;
 *	ARG(-(anything but NaN), +-0) is +-PI ;
 *	ARG( 0, +-(anything but 0 and NaN) ) is +-PI/2;
 *	ARG( +INF,+-(anything but INF and NaN) ) is +-0 ;
 *	ARG( -INF,+-(anything but INF and NaN) ) is +-PI;
 *	ARG( +INF,+-INF ) is +-PI/4 ;
 *	ARG( -INF,+-INF ) is +-3PI/4;
 *	ARG( (anything but,0,NaN, and INF),+-INF ) is +-PI/2;
 *
 * Accuracy:
 *	atan2(y,x) returns (PI/pi) * the exact ARG (x+iy) nearly rounded, 
 *	where
 *
 *	in decimal:
 *		pi = 3.141592653589793 23846264338327 ..... 
 *    53 bits   PI = 3.141592653589793 115997963 ..... ,
 *    56 bits   PI = 3.141592653589793 227020265 ..... ,  
 *
 *	in hexadecimal:
 *		pi = 3.243F6A8885A308D313198A2E....
 *    53 bits   PI = 3.243F6A8885A30  =  2 * 1.921FB54442D18	error=.276ulps
 *    56 bits   PI = 3.243F6A8885A308 =  4 * .C90FDAA22168C2    error=.206ulps
 *	
 *	In a test run with 356,000 random argument on [-1,1] * [-1,1] on a
 *	VAX, the maximum observed error was 1.41 ulps (units of the last place)
 *	compared with (PI/pi)*(the exact ARG(x+iy)).
 *
 * Note:
 *	We use machine PI (the true pi rounded) in place of the actual
 *	value of pi for all the trig and inverse trig functions. In general, 
 *	if trig is one of sin, cos, tan, then computed trig(y) returns the 
 *	exact trig(y*pi/PI) nearly rounded; correspondingly, computed arctrig 
 *	returns the exact arctrig(y)*PI/pi nearly rounded. These guarantee the 
 *	trig functions have period PI, and trig(arctrig(x)) returns x for
 *	all critical values x.
 *	
 * Constants:
 * The hexadecimal values are the intended ones for the following constants.
 * The decimal values may be used, provided that the compiler will convert
 * from decimal to binary accurately enough to produce the hexadecimal values
 * shown.
 */

#if defined(vax)||defined(tahoe) 	/* VAX D format */
#ifdef vax
#define _0x(A,B)	0x/**/A/**/B
#else	/* vax */
#define _0x(A,B)	0x/**/B/**/A
#endif	/* vax */
/*static double */
/*athfhi =  4.6364760900080611433E-1    , /*Hex  2^ -1   *  .ED63382B0DDA7B */
/*athflo =  1.9338828231967579916E-19   , /*Hex  2^-62   *  .E450059CFE92C0 */
/*PIo4   =  7.8539816339744830676E-1    , /*Hex  2^  0   *  .C90FDAA22168C2 */	
/*at1fhi =  9.8279372324732906796E-1    , /*Hex  2^  0   *  .FB985E940FB4D9 */
/*at1flo = -3.5540295636764633916E-18   , /*Hex  2^-57   * -.831EDC34D6EAEA */
/*PIo2   =  1.5707963267948966135E0     , /*Hex  2^  1   *  .C90FDAA22168C2 */
/*PI     =  3.1415926535897932270E0     , /*Hex  2^  2   *  .C90FDAA22168C2 */
/*a1     =  3.3333333333333473730E-1    , /*Hex  2^ -1   *  .AAAAAAAAAAAB75 */
/*a2     = -2.0000000000017730678E-1    , /*Hex  2^ -2   * -.CCCCCCCCCD946E */
/*a3     =  1.4285714286694640301E-1    , /*Hex  2^ -2   *  .92492492744262 */
/*a4     = -1.1111111135032672795E-1    , /*Hex  2^ -3   * -.E38E38EBC66292 */
/*a5     =  9.0909091380563043783E-2    , /*Hex  2^ -3   *  .BA2E8BB31BD70C */
/*a6     = -7.6922954286089459397E-2    , /*Hex  2^ -3   * -.9D89C827C37F18 */
/*a7     =  6.6663180891693915586E-2    , /*Hex  2^ -3   *  .8886B4AE379E58 */
/*a8     = -5.8772703698290408927E-2    , /*Hex  2^ -4   * -.F0BBA58481A942 */
/*a9     =  5.2170707402812969804E-2    , /*Hex  2^ -4   *  .D5B0F3A1AB13AB */
/*a10    = -4.4895863157820361210E-2    , /*Hex  2^ -4   * -.B7E4B97FD1048F */
/*a11    =  3.3006147437343875094E-2    , /*Hex  2^ -4   *  .8731743CF72D87 */
/*a12    = -1.4614844866464185439E-2    ; /*Hex  2^ -6   * -.EF731A2F3476D9 */
static long athfhix[] = { _0x(6338,3fed), _0x(da7b,2b0d)};
#define athfhi	(*(double *)athfhix)
static long athflox[] = { _0x(5005,2164), _0x(92c0,9cfe)};
#define athflo	(*(double *)athflox)
static long   PIo4x[] = { _0x(0fda,4049), _0x(68c2,a221)};
#define   PIo4	(*(double *)PIo4x)
static long at1fhix[] = { _0x(985e,407b), _0x(b4d9,940f)};
#define at1fhi	(*(double *)at1fhix)
static long at1flox[] = { _0x(1edc,a383), _0x(eaea,34d6)};
#define at1flo	(*(double *)at1flox)
static long   PIo2x[] = { _0x(0fda,40c9), _0x(68c2,a221)};
#define   PIo2	(*(double *)PIo2x)
static long     PIx[] = { _0x(0fda,4149), _0x(68c2,a221)};
#define     PI	(*(double *)PIx)
static long     a1x[] = { _0x(aaaa,3faa), _0x(ab75,aaaa)};
#define     a1	(*(double *)a1x)
static long     a2x[] = { _0x(cccc,bf4c), _0x(946e,cccd)};
#define     a2	(*(double *)a2x)
static long     a3x[] = { _0x(4924,3f12), _0x(4262,9274)};
#define     a3	(*(double *)a3x)
static long     a4x[] = { _0x(8e38,bee3), _0x(6292,ebc6)};
#define     a4	(*(double *)a4x)
static long     a5x[] = { _0x(2e8b,3eba), _0x(d70c,b31b)};
#define     a5	(*(double *)a5x)
static long     a6x[] = { _0x(89c8,be9d), _0x(7f18,27c3)};
#define     a6	(*(double *)a6x)
static long     a7x[] = { _0x(86b4,3e88), _0x(9e58,ae37)};
#define     a7	(*(double *)a7x)
static long     a8x[] = { _0x(bba5,be70), _0x(a942,8481)};
#define     a8	(*(double *)a8x)
static long     a9x[] = { _0x(b0f3,3e55), _0x(13ab,a1ab)};
#define     a9	(*(double *)a9x)
static long    a10x[] = { _0x(e4b9,be37), _0x(048f,7fd1)};
#define    a10	(*(double *)a10x)
static long    a11x[] = { _0x(3174,3e07), _0x(2d87,3cf7)};
#define    a11	(*(double *)a11x)
static long    a12x[] = { _0x(731a,bd6f), _0x(76d9,2f34)};
#define    a12	(*(double *)a12x)
#else 	/* defined(vax)||defined(tahoe) */
static double 
athfhi =  4.6364760900080609352E-1    , /*Hex  2^ -2   *  1.DAC670561BB4F */
athflo =  4.6249969567426939759E-18   , /*Hex  2^-58   *  1.5543B8F253271 */
PIo4   =  7.8539816339744827900E-1    , /*Hex  2^ -1   *  1.921FB54442D18 */
at1fhi =  9.8279372324732905408E-1    , /*Hex  2^ -1   *  1.F730BD281F69B */
at1flo = -2.4407677060164810007E-17   , /*Hex  2^-56   * -1.C23DFEFEAE6B5 */
PIo2   =  1.5707963267948965580E0     , /*Hex  2^  0   *  1.921FB54442D18 */
PI     =  3.1415926535897931160E0     , /*Hex  2^  1   *  1.921FB54442D18 */
a1     =  3.3333333333333942106E-1    , /*Hex  2^ -2   *  1.55555555555C3 */
a2     = -1.9999999999979536924E-1    , /*Hex  2^ -3   * -1.9999999997CCD */
a3     =  1.4285714278004377209E-1    , /*Hex  2^ -3   *  1.24924921EC1D7 */
a4     = -1.1111110579344973814E-1    , /*Hex  2^ -4   * -1.C71C7059AF280 */
a5     =  9.0908906105474668324E-2    , /*Hex  2^ -4   *  1.745CE5AA35DB2 */
a6     = -7.6919217767468239799E-2    , /*Hex  2^ -4   * -1.3B0FA54BEC400 */
a7     =  6.6614695906082474486E-2    , /*Hex  2^ -4   *  1.10DA924597FFF */
a8     = -5.8358371008508623523E-2    , /*Hex  2^ -5   * -1.DE125FDDBD793 */
a9     =  4.9850617156082015213E-2    , /*Hex  2^ -5   *  1.9860524BDD807 */
a10    = -3.6700606902093604877E-2    , /*Hex  2^ -5   * -1.2CA6C04C6937A */
a11    =  1.6438029044759730479E-2    ; /*Hex  2^ -6   *  1.0D52174A1BB54 */
#endif 	/* defined(vax)||defined(tahoe) */

double atan2(y,x)
double  y,x;
{  
	static double zero=0, one=1, small=1.0E-9, big=1.0E18;
	double copysign(),logb(),scalb(),t,z,signy,signx,hi,lo;
	int finite(), k,m;

#if !defined(vax)&&!defined(tahoe)
    /* if x or y is NAN */
	if(x!=x) return(x); if(y!=y) return(y);
#endif	/* !defined(vax)&&!defined(tahoe) */

    /* copy down the sign of y and x */
	signy = copysign(one,y) ;  
	signx = copysign(one,x) ;  

    /* if x is 1.0, goto begin */
	if(x==1) { y=copysign(y,one); t=y; if(finite(t)) goto begin;}

    /* when y = 0 */
	if(y==zero) return((signx==one)?y:copysign(PI,signy));

    /* when x = 0 */
	if(x==zero) return(copysign(PIo2,signy));
	    
    /* when x is INF */
	if(!finite(x))
	    if(!finite(y)) 
		return(copysign((signx==one)?PIo4:3*PIo4,signy));
	    else
		return(copysign((signx==one)?zero:PI,signy));

    /* when y is INF */
	if(!finite(y)) return(copysign(PIo2,signy));

    /* compute y/x */
	x=copysign(x,one); 
	y=copysign(y,one); 
	if((m=(k=logb(y))-logb(x)) > 60) t=big+big; 
	    else if(m < -80 ) t=y/x;
	    else { t = y/x ; y = scalb(y,-k); x=scalb(x,-k); }

    /* begin argument reduction */
begin:
	if (t < 2.4375) {		 

	/* truncate 4(t+1/16) to integer for branching */
	    k = 4 * (t+0.0625);
	    switch (k) {

	    /* t is in [0,7/16] */
	    case 0:                    
	    case 1:
		if (t < small) 
		    { big + small ;  /* raise inexact flag */
		      return (copysign((signx>zero)?t:PI-t,signy)); }

		hi = zero;  lo = zero;  break;

	    /* t is in [7/16,11/16] */
	    case 2:                    
		hi = athfhi; lo = athflo;
		z = x+x;
		t = ( (y+y) - x ) / ( z +  y ); break;

	    /* t is in [11/16,19/16] */
	    case 3:                    
	    case 4:
		hi = PIo4; lo = zero;
		t = ( y - x ) / ( x + y ); break;

	    /* t is in [19/16,39/16] */
	    default:                   
		hi = at1fhi; lo = at1flo;
		z = y-x; y=y+y+y; t = x+x;
		t = ( (z+z)-x ) / ( t + y ); break;
	    }
	}
	/* end of if (t < 2.4375) */

	else                           
	{
	    hi = PIo2; lo = zero;

	    /* t is in [2.4375, big] */
	    if (t <= big)  t = - x / y;

	    /* t is in [big, INF] */
	    else          
	      { big+small;	/* raise inexact flag */
		t = zero; }
	}
    /* end of argument reduction */

    /* compute atan(t) for t in [-.4375, .4375] */
	z = t*t;
#if defined(vax)||defined(tahoe)
	z = t*(z*(a1+z*(a2+z*(a3+z*(a4+z*(a5+z*(a6+z*(a7+z*(a8+
			z*(a9+z*(a10+z*(a11+z*a12))))))))))));
#else	/* defined(vax)||defined(tahoe) */
	z = t*(z*(a1+z*(a2+z*(a3+z*(a4+z*(a5+z*(a6+z*(a7+z*(a8+
			z*(a9+z*(a10+z*a11)))))))))));
#endif	/* defined(vax)||defined(tahoe) */
	z = lo - z; z += t; z += hi;

	return(copysign((signx>zero)?z:PI-z,signy));
}
