/*	@(#)atan.c	4.1/4.2		10/31/84  CCI-CPG	*/


/*
 * Double-precision arctangent function recoded with
 * new algorithm polynomial at CCI-CPG.
 * 
 * atan returns the value of the arctangent of its
 * argument in the range [-pi/2,pi/2].
 * 
 * atan2 returns the arctangent of arg1/arg2
 * in the range [-pi,pi].
 * 
 * There are no error returns.
 * 
 * New version by Les Powers (3/31/85)
 */

#include "atand.h"


static double pio2 =  1.570796326794896619231e0;
static double pio4 =  0.785398163397448309615e0;
static double ap0  =  1.0;
static double ap1  = -1.0/3.0;
static double ap2  =  1.0/5.0;
static double ap3  = -1.0/7.0;


/*
 * atan reduces its argument
 * and returns the arctan.
 */
double
atan(x)
double x;
{
	double t,s;
	unsigned ta;
	union {
	  double d;
	  struct { unsigned i0,i1; } i;
	  struct { unsigned char b0,b1,b2,b3; } b;
	} u;
	if ( x < 0 )
	  return( -atan(-x) );
	u.d = x;
	if (u.i.i0 >= 0x41000000 ) {		/* if (x >= 2.0) */
	  return( pio2 - atan( 1.0/x ) );
	}
	if (u.i.i0 >= 0x40000000 ) {		/* if (x >= 0.5) */
	  u.i.i0 = (u.i.i0 & 0x40ff8000) | 0x00008000;
	  u.i.i1 = 0;
	  t = (x-u.d)/(x*u.d+1.0);
	  s = t*t;
	  return( t*(ap0+s*(ap1+s*ap2)) + at1[u.b.b1] );
	}
	if (u.i.i0 >= 0x3d000000) {		/* if (x >= 1/128) */
	  u.i.i0 += 0x03800000;
	  ta = u.d;
	  u.d = ta;
	  u.i.i0 -= 0x03800000;
	  t = (x-u.d)/(x*u.d+1.0);
	}
	else {
	  ta = 0;
	  t = x;
	}
	s = t*t;
	return( t*(ap0+s*(ap1+s*(ap2+s*ap3))) + at0[ta] );
}


/*
 * atan2 discovers what quadrant the angle
 * is in and calls atan.
 */
double
atan2(y,x)
double y,x;
{
	double atan();

	if (y > 0)
	  if ( x >= y )
		return(        atan(y/x));
	  else if ( x >= 0 )
		return( pio2 - atan(x/y));
	  else if (-x <= y )
		return( pio2 + atan(-x/y));
	  else
		return( pio2 + pio2 - atan(-y/x));
	else if (y < 0)
	  if ( x >= -y )
		return(       -atan(-y/x));
	  else if ( x >= 0 )
		return(-pio2 + atan(x/(-y)));
	  else if (-x <= -y )
		return(-pio2 - atan(x/y));
	  else
		return(-pio2 - pio2 + atan(y/x));
	else
	  if ( x > 0 )
		return( 0 );
	  else if ( x < 0 )
		return( pio2 + pio2 );
	  else
		return( pio2 );
}
