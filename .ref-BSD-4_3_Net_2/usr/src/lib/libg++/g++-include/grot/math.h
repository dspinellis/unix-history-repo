// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/


#ifndef _math_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _math_h 1

#if defined(hp300) && defined(__HAVE_FPU__)
#define __HAVE_68881__ 1
#endif

#if defined(masscomp)
#define __HAVE_68881__ 1
#endif

#ifdef __HAVE_68881__		/* MC68881/2 Floating-Point Coprocessor */
extern "C" {			/* fill in what we've left out */
#include <math-68881.h>

double  acosh(double);
double  asinh(double);
double  cbrt(double);
double  copysign(double,double);
double  erf(double);
double  erfc(double);
double  finite(double);
double  gamma(double);
double  hypot(double,double);
double  infnan(int);
int     isinf(double);
int     isnan(double);
double  j0(double);
double  j1(double);
double  jn(int, double);
double  lgamma(double);
double  y0(double);
double  y1(double);
double  yn(int, double);

double aint(double);
double anint(double);
int irint(double);
int nint(double);
}
/* Please add inline asm code for other machines here! */
#else
extern "C" {

double  acos(double);
double  acosh(double);
double  asin(double);
double  asinh(double);
double  atan(double);
double  atan2(double, double);
double  atanh(double);
double  cbrt(double);
double  ceil(double);
double  copysign(double,double);
double  cos(double);
double  cosh(double);
double  drem(double,double);
double  erf(double);
double  erfc(double);
double  exp(double);
double  expm1(double);
double  fabs(double);
double  finite(double);
double  floor(double);
double  frexp(double, int*);
double  gamma(double);
double  hypot(double,double);
double  infnan(int);
#if !defined(sequent) && !defined(DGUX) &&!defined(sony) && !defined(masscomp)
/* see below */
int     isinf(double);
int     isnan(double);
#endif
double  j0(double);
double  j1(double);
double  jn(int, double);
double  ldexp(double, int);
double  lgamma(double);
double  log(double);
double  log10(double);
double  log1p(double);
double  logb(double);
double  modf(double, double*);
double  pow(double, double);
double  rint(double);
double  scalb(double, int);
double  sin(double);
double  sinh(double);
double  sqrt(double);
double  tan(double);
double  tanh(double);
double  y0(double);
double  y1(double);
double  yn(int, double);

double aint(double);
double anint(double);
int irint(double);
int nint(double);
}

#endif

/* libg++ doesn't use this since it is not available on some systems */

/* the following ifdef is just for compiling OOPS */

#ifndef DONT_DECLARE_EXCEPTION
struct libm_exception
{
  int type;
  char* name;
  double arg1, arg2, retval;
};

#define DOMAIN      1
#define SING        2
#define OVERFLOW    3
#define UNDERFLOW   4
#define TLOSS       5
#define PLOSS       6

extern "C" int matherr(libm_exception*);

#endif

#include <values.h>

/* On some systems, HUGE ought to be MAXFLOAT or IEEE infinity */

#ifndef HUGE
#define HUGE    MAXDOUBLE
#endif


/* sequents don't supply these. The following should suffice */
#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)
#if defined(sequent) || defined(DGUX) || defined(sony) || defined(masscomp) \
|| defined(hpux)
static inline int isnan(double x) { return x != x; }
static inline int isinf(double x) { return x > MAXDOUBLE || x < -MAXDOUBLE; }
#endif

#endif /* __ OPTIMIZE __ */

/* These seem to be sun & sysV names of these constants */

#ifndef M_E
#define M_E         2.7182818284590452354
#endif
#ifndef M_LOG2E
#define M_LOG2E     1.4426950408889634074
#endif
#ifndef M_LOG10E
#define M_LOG10E    0.43429448190325182765
#endif
#ifndef M_LN2
#define M_LN2       0.69314718055994530942
#endif
#ifndef M_LN10
#define M_LN10      2.30258509299404568402
#endif
#ifndef M_PI
#define M_PI        3.14159265358979323846
#endif
#ifndef M_PI_2
#define M_PI_2      1.57079632679489661923
#endif
#ifndef M_1_PI
#define M_1_PI      0.31830988618379067154
#endif
#ifndef M_PI_4
#define M_PI_4      0.78539816339744830962
#endif
#ifndef M_2_PI
#define M_2_PI      0.63661977236758134308
#endif
#ifndef M_2_SQRTPI
#define M_2_SQRTPI  1.12837916709551257390
#endif
#ifndef M_SQRT2
#define M_SQRT2     1.41421356237309504880
#endif
#ifndef M_SQRT1_2
#define M_SQRT1_2   0.70710678118654752440
#endif

#ifndef PI                      // as in stroustrup
#define PI  M_PI
#endif
#ifndef PI2
#define PI2  M_PI_2
#endif

#endif
