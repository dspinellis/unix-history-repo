/*
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 * All recipients should regard themselves as participants in an ongoing
 * research project and hence should feel obligated to report their
 * experiences (good or bad) with these elementary function codes, using
 * the sendbug(8) program, to the authors.
 *
 *	@(#)math.h	5.1 (Berkeley) %G%
 */

#define	HUGE	1.701411733192644270e38

double	acos(), acosh(), asin(), asinh(), atan(), atan2(), atanh(),
	atof(), cabs(), cbrt(), ceil(), copysign(), cos(), cosh(),
	drem(), erf(), erfc(), exp(), expm1(), fabs(), floor(), frexp(),
	hypot(), j0(), j1(), jn(), ldexp(), lgamma(), log(), log10(),
	log1p(), logb(), modf(), pow(), rint(), scalb(), sin(), sinh(),
	sqrt(), tan(), tanh(), y0(), y1(), yn();

int finite();

#if defined(vax) || defined(tahoe)
double infnan();
#endif
