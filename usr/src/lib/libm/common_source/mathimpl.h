/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mathimpl.h	5.5 (Berkeley) %G%
 */

#include <sys/cdefs.h>
#include <math.h>

#if defined(vax)||defined(tahoe)

/* Deal with different ways to concatenate in cpp */
#  ifdef __STDC__
#    define	cat3(a,b,c) a ## b ## c
#  else
#    define	cat3(a,b,c) a/**/b/**/c
#  endif

/* Deal with vax/tahoe byte order issues */
#  ifdef vax
#    define	cat3t(a,b,c) cat3(a,b,c)
#  else
#    define	cat3t(a,b,c) cat3(a,c,b)
#  endif

#  define vccast(name) (*(const double *)(cat3(name,,x)))

   /*
    * Define a constant to high precision on a Vax or Tahoe.
    *
    * Args are the name to define, the decimal floating point value,
    * four 16-bit chunks of the float value in hex
    * (because the vax and tahoe differ in float format!), the power
    * of 2 of the hex-float exponent, and the hex-float mantissa.
    * Most of these arguments are not used at compile time; they are
    * used in a post-check to make sure the constants were compiled
    * correctly.
    *
    * People who want to use the constant will have to do their own
    *     #define foo vccast(foo)
    * since CPP cannot do this for them from inside another macro (sigh).
    * We define "vccast" if this needs doing.
    */
#  define vc(name, value, x1,x2,x3,x4, bexp, xval) \
	const static long cat3(name,,x)[] = {cat3t(0x,x1,x2), cat3t(0x,x3,x4)};

#  define ic(name, value, bexp, xval) ;

#else	/* vax or tahoe */

   /* Hooray, we have an IEEE machine */
#  undef vccast
#  define vc(name, value, x1,x2,x3,x4, bexp, xval) ;

#  define ic(name, value, bexp, xval) \
	const static double name = value;

#endif	/* defined(vax)||defined(tahoe) */


/*
 * Functions internal to the math package, yet not static.
 */
extern double	exp__E();
extern double	log__L();

struct Double {double a, b;};
double exp__D __P((double, double));
struct Double log__D __P((double));
