/*	@(#)floor.c	4.2	9/11/85; 1.5 (ucb.elefunt) %G% */

/*
 * floor and ceil-- greatest integer <= arg
 * (resp least >=)
 */

double	modf();

double
floor(d)
double d;
{
	double fract;

	if (d<0.0) {
		d = -d;
		fract = modf(d, &d);
		if (fract != 0.0)
			d += 1;
		d = -d;
	} else
		modf(d, &d);
	return(d);
}

double
ceil(d)
double d;
{
	return(-floor(-d));
}

#ifndef NATIONAL			/* rint() is in ./NATIONAL/support.s */
/*
 * algorithm for rint(x) in pseudo-pascal form ...
 *
 * real rint(x): real x;
 *	... delivers integer nearest x in direction of prevailing rounding
 *	... mode
 * const	L = (last consecutive integer)/2
 * 	  = 2**55; for VAX D
 * 	  = 2**52; for IEEE 754 Double
 * real	s,t;
 * begin
 * 	if x != x then return x;		... NaN
 * 	if |x| >= L then return x;		... already an integer
 * 	s := copysign(L,x);
 * 	t := x + s;				... = (x+s) rounded to integer
 * 	return t - s
 * end;
 *
 * Note: Inexact will be signaled if x is not an integer, as is
 *	customary for IEEE 754.  No other signal can be emitted.
 */
#if (defined(VAX)||defined(TAHOE))
#ifdef VAX
#define _0x(A,B)	0x/**/A/**/B
#else	/* VAX */
#define _0x(A,B)	0x/**/B/**/A
#endif	/* VAX */
static long Lx[] = {_0x(0000,5c00),_0x(0000,0000)};	/* 2**55 */
#define L *(double *) Lx
#else	/* IEEE double */
static double L = 4503599627370496.0E0;		/* 2**52 */
#endif
double
rint(x)
double x;
{
	double s,t,one = 1.0,copysign();
#if (!defined(VAX)&&!defined(TAHOE))
	if (x != x)				/* NaN */
		return (x);
#endif
	if (copysign(x,one) >= L)		/* already an integer */
	    return (x);
	s = copysign(L,x);
	t = x + s;				/* x+s rounded to integer */
	return (t - s);
}
#endif	/* NATIONAL */
