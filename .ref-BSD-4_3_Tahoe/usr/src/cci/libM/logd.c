/*	@(#)log.c	4.1	12/25/82	*/

/*
 * log returns the natural logarithm of its floating
 * point argument.
 *
 * New version by Les Powers (3/31/85).
 */


#include <errno.h>
#include <math.h>
#include "logd.h"


static double	log2	= 0.693147180559945309e0;
static double	recip_ln10	= 1./2.302585092994045684;
static double	p1	= 1.;
static double	p2	= -.5;
static double	p3	= 1./3.;
static double	p4	= -1./4.;

int	errno;


double
log(arg)
double arg;
{
	double z;
	union {
	  double d;
	  struct {
	    unsigned i0;
	    unsigned i1;
	  } i;
	  struct {
	    unsigned char b0;
	    unsigned char b1;
	    unsigned char b2;
	    unsigned char b3;
	  } b;
	} f0,f1,a0;
	register int ta1;
	if (arg <= 0.) {
		errno = EDOM;
		return(-HUGE);
	}
	f1.d  = arg;
	f0.i.i0 = f1.i.i0 + f1.i.i0;
	if ( f1.i.i0 >= 0x40800000 ) {
	  f1.i.i0 = (f1.i.i0 & 0x007fffff) | 0x40800000;
	  a0.i.i0 =  f1.i.i0 & 0x40ff8000;
	  a0.i.i1 = 0;
	  f1.d	  =  (f1.d - a0.d) * rp0[f0.b.b1];
	  ta1 = f1.d;
	  z  = (f1.d - ta1) * rp1[ta1];
	  return( z*(p1+z*(p2+z*(p3+z*p4))) +
		lp1[ta1] + lp0[f0.b.b1] + le[f0.b.b0] );
	}
	else {
	  f1.i.i0 = (f1.i.i0 & 0x007fffff) | 0x40000000;
	  a0.i.i0 = (f1.i.i0 & 0x40ff8000) + 0x00008000;
	  a0.i.i1 = 0;
	  f1.d	  = (a0.d - f1.d) * rn0[f0.b.b1];
	  ta1 = f1.d;
	  z  = (ta1-f1.d) * rn1[ta1];
	  return( z*(p1+z*(p2+z*(p3+z*p4))) +
		ln1[ta1] + ln0[f0.b.b1] + le[f0.b.b0] );
	}
}

double
log10(arg)
double arg;
{
	return(log(arg)*recip_ln10);
}
