/*
 * square and cube roots by Newton's method
 *
 * $XConsortium: XcmsMath.c,v 1.8 91/08/14 15:13:41 rws Exp $
 *
 * Copyright 1990 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Stephen Gildea, MIT X Consortium, January 1991
 */

#include "Xlibint.h"
#include "Xcmsint.h"

#if !defined(X_NOT_STDC_ENV) && (__STDC__ || !(defined(sun) || (defined(sony) && !defined(SYSTYPE_SYSV))))
#include <float.h>
#endif
#ifndef DBL_EPSILON
#define DBL_EPSILON 1e-6
#endif

#ifdef _X_ROOT_STATS
int cbrt_loopcount;
int sqrt_loopcount;
#endif

/* Newton's Method:  x_n+1 = x_n - ( f(x_n) / f'(x_n) ) */


/* for cube roots, x^3 - a = 0,  x_new = x - 1/3 (x - a/x^2) */

double
_XcmsCubeRoot(a)
    double a;
{
    register double abs_a, cur_guess, delta;

#ifdef DEBUG
    printf("_XcmsCubeRoot passed in %g\n", a);
#endif
#ifdef _X_ROOT_STATS
    cbrt_loopcount = 0;
#endif
    if (a == 0.)
	return 0.;

    abs_a = a<0. ? -a : a;	/* convert to positive to speed loop tests */

    /* arbitrary first guess */
    if (abs_a > 1.)
	cur_guess = abs_a/8.;
    else
	cur_guess = abs_a*8.;

    do {
#ifdef _X_ROOT_STATS
	cbrt_loopcount++;
#endif
	delta = (cur_guess - abs_a/(cur_guess*cur_guess))/3.;
	cur_guess -= delta;
	if (delta < 0.) delta = -delta;
    } while (delta >= cur_guess*DBL_EPSILON);

    if (a < 0.)
	cur_guess = -cur_guess;

#ifdef DEBUG
    printf("_XcmsCubeRoot returning %g\n", cur_guess);
#endif
    return cur_guess;
}
	


/* for square roots, x^2 - a = 0,  x_new = x - 1/2 (x - a/x) */

double
_XcmsSquareRoot(a)
    double a;
{
    register double cur_guess, delta;

#ifdef DEBUG
    printf("_XcmsSquareRoot passed in %g\n", a);
#endif
#ifdef _X_ROOT_STATS
    sqrt_loopcount = 0;
#endif
    if (a == 0.)
	return 0.;

    if (a < 0.) {
	/* errno = EDOM; */
	return 0.;
    }

    /* arbitrary first guess */
    if (a > 1.)
	cur_guess = a/4.;
    else
	cur_guess = a*4.;

    do {
#ifdef _X_ROOT_STATS
	sqrt_loopcount++;
#endif
	delta = (cur_guess - a/cur_guess)/2.;
	cur_guess -= delta;
	if (delta < 0.) delta = -delta;
    } while (delta >= cur_guess*DBL_EPSILON);

#ifdef DEBUG
    printf("_XcmsSquareRoot returning %g\n", cur_guess);
#endif
    return cur_guess;
}
	
