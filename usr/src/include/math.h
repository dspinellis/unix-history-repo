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
 *	@(#)math.h	5.4 (Berkeley) %G%
 */

#ifndef	_MATH_H_
#define	_MATH_H_

#if defined(vax) || defined(tahoe)
#define	HUGE_VAL	1.701411733192644270e38
#else
#define	HUGE_VAL	1e500	/* positive infinity */
#endif

#if !defined(_ANSI_SOURCE) && !defined(_POSIX_SOURCE)
#define	HUGE		HUGE_VAL
#endif

#include <sys/cdefs.h>

__BEGIN_DECLS
double	acos __P((double));
double	asin __P((double));
double	atan __P((double));
double	atan2 __P((double, double));
double	ceil __P((double));
double	cos __P((double));
double	cosh __P((double));
double	exp __P((double));
double	fabs __P((double));
double	floor __P((double));
double	fmod __P((double, double));
double	frexp __P((double, int *));
double	ldexp __P((double, int));
double	log __P((double));
double	log10 __P((double));
double	modf __P((double, double *));
double	pow __P((double, double));
double	sin __P((double));
double	sinh __P((double));
double	sqrt __P((double));
double	tan __P((double));
double	tanh __P((double));

#if !defined(_ANSI_SOURCE) && !defined(_POSIX_SOURCE)
double	acosh __P((double));
double	asinh __P((double));
double	atanh __P((double));
double	cabs();		/* we can't describe cabs()'s argument properly */
double	cbrt __P((double));
double	copysign __P((double, double));
double	drem __P((double, double));
double	erf __P((double));
double	erfc __P((double));
double	expm1 __P((double));
int	finite __P((double));
double	hypot __P((double, double));
#if defined(vax) || defined(tahoe)
double	infnan __P((int));
#else
int	isnan __P((double));
#endif
double	j0 __P((double));
double	j1 __P((double));
double	jn __P((int, double));
double	lgamma __P((double));
double	log1p __P((double));
double	logb __P((double));
double	rint __P((double));
double	scalb __P((double, int));
double	y0 __P((double));
double	y1 __P((double));
double	yn __P((int, double));
#endif

__END_DECLS

#endif /* _MATH_H_ */
