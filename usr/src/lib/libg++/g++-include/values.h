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


#ifndef _values_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _values_h 1

#define BITSPERBYTE 8
#define BITS(type)  (BITSPERBYTE * (int)sizeof(type))

#define CHARBITS    BITS(char)
#define SHORTBITS   BITS(short)
#define INTBITS     BITS(int)
#define LONGBITS    BITS(long)
#define PTRBITS     BITS(char*)
#define DOUBLEBITS  BITS(double)
#define FLOATBITS   BITS(float)

#define MINSHORT    ((short)(1 << (SHORTBITS - 1)))
#define MININT      (1 << (INTBITS - 1))
#define MINLONG     (1L << (LONGBITS - 1))

#define MAXSHORT    ((short)~MINSHORT)
#define MAXINT      (~MININT)
#define MAXLONG     (~MINLONG)

#define HIBITS	MINSHORT
#define HIBITL	MINLONG

#if defined(sun) || defined(hp300) || defined(hpux) || defined(masscomp)
#ifdef masscomp
#define MAXDOUBLE							\
({									\
  double maxdouble_val;							\
									\
  __asm ("fmove%.d #0x7fefffffffffffff,%0"	/* Max double */	\
	 : "=f" (maxdouble_val)						\
	 : /* no inputs */);						\
  maxdouble_val;							\
})
#define MAXFLOAT ((float) 3.40e+38)
#else
#define MAXDOUBLE   1.79769313486231470e+308
#define MAXFLOAT    ((float)3.40282346638528860e+38)
#endif
#define MINDOUBLE   4.94065645841246544e-324
#define MINFLOAT    ((float)1.40129846432481707e-45)
#define _IEEE       1
#define _DEXPLEN    11
#define _FEXPLEN    8
#define _HIDDENBIT  1
#define DMINEXP     (-(DMAXEXP + DSIGNIF - _HIDDENBIT - 3))
#define FMINEXP     (-(FMAXEXP + FSIGNIF - _HIDDENBIT - 3))
#define DMAXEXP     ((1 << _DEXPLEN - 1) - 1 + _IEEE)
#define FMAXEXP     ((1 << _FEXPLEN - 1) - 1 + _IEEE)

#elif defined(sony) 
#define MAXDOUBLE   1.79769313486231470e+308
#define MAXFLOAT    ((float)3.40282346638528860e+38)
#define MINDOUBLE   2.2250738585072010e-308
#define MINFLOAT    ((float)1.17549435e-38)
#define _IEEE       1
#define _DEXPLEN    11
#define _FEXPLEN    8
#define _HIDDENBIT  1
#define DMINEXP     (-(DMAXEXP + DSIGNIF - _HIDDENBIT - 3))
#define FMINEXP     (-(FMAXEXP + FSIGNIF - _HIDDENBIT - 3))
#define DMAXEXP     ((1 << _DEXPLEN - 1) - 1 + _IEEE)
#define FMAXEXP     ((1 << _FEXPLEN - 1) - 1 + _IEEE)

#elif defined(sequent)
extern double _maxdouble, _mindouble;
extern float _maxfloat, _minfloat;
#define MAXDOUBLE	_maxdouble
#define MAXFLOAT	_maxfloat
#define MINDOUBLE	_mindouble
#define MINFLOAT	_minfloat
#define _IEEE       1
#define _DEXPLEN    11
#define _FEXPLEN    8
#define _HIDDENBIT  1
#define DMINEXP     (-(DMAXEXP - 3))
#define FMINEXP     (-(FMAXEXP - 3))
#define DMAXEXP     ((1 << _DEXPLEN - 1) - 1 + _IEEE)
#define FMAXEXP     ((1 << _FEXPLEN - 1) - 1 + _IEEE)

#elif defined(i386)
#define MAXDOUBLE   1.79769313486231570e+308
#define MAXFLOAT    ((float)3.40282346638528860e+38)
#define MINDOUBLE   2.22507385850720140e-308
#define MINFLOAT    ((float)1.17549435082228750e-38)
#define _IEEE       0
#define _DEXPLEN    11
#define _FEXPLEN    8
#define _HIDDENBIT  1
#define DMINEXP     (-DMAXEXP)
#define FMINEXP     (-FMAXEXP)
#define DMAXEXP     ((1 << _DEXPLEN - 1) - 1 + _IEEE)
#define FMAXEXP     ((1 << _FEXPLEN - 1) - 1 + _IEEE)

/* from Andrew Klossner <andrew%frip.wv.tek.com@relay.cs.net> */
#elif defined(m88k)
	/* These are "good" guesses ...
	   I'll figure out the true mins and maxes later, at the time I find
	   out the mins and maxes that the compiler can tokenize. */
#define MAXDOUBLE   1.79769313486231e+308
#define MAXFLOAT    ((float)3.40282346638528e+38)
#define MINDOUBLE   2.22507385850720e-308
#define MINFLOAT    ((float)1.17549435082228e-38)
#define _IEEE       1
#define _DEXPLEN    11
#define _FEXPLEN    8
#define _HIDDENBIT  1
#define DMINEXP     (1-DMAXEXP)
#define FMINEXP     (1-FMAXEXP)
#define DMAXEXP     ((1 << _DEXPLEN - 1) - 1 + _IEEE)
#define FMAXEXP     ((1 << _FEXPLEN - 1) - 1 + _IEEE)

#elif defined(convex)
#define MAXDOUBLE   8.9884656743115785e+306
#define MAXFLOAT    ((float) 1.70141173e+38)
#define MINDOUBLE   5.5626846462680035e-308
#define MINFLOAT    ((float) 2.93873588e-39)
#define _IEEE       0
#define _DEXPLEN    11
#define _FEXPLEN    8
#define _HIDDENBIT  1
#define DMINEXP     (-DMAXEXP)
#define FMINEXP     (-FMAXEXP)
#define DMAXEXP     ((1 << _DEXPLEN - 1) - 1 + _IEEE)
#define FMAXEXP     ((1 << _FEXPLEN - 1) - 1 + _IEEE)

// #elif defined(vax)
// use vax versions by default -- they seem to be the most conservative
#else 

#define MAXDOUBLE   1.701411834604692293e+38
#define MINDOUBLE   (2.938735877055718770e-39)

#define MAXFLOAT    1.7014117331926443e+38
#define MINFLOAT    2.9387358770557188e-39

#define _IEEE       0
#define _DEXPLEN    8
#define _FEXPLEN    8
#define _HIDDENBIT  1
#define DMINEXP     (-DMAXEXP)
#define FMINEXP     (-FMAXEXP)
#define DMAXEXP     ((1 << _DEXPLEN - 1) - 1 + _IEEE)
#define FMAXEXP     ((1 << _FEXPLEN - 1) - 1 + _IEEE)
#endif

#define DSIGNIF     (DOUBLEBITS - _DEXPLEN + _HIDDENBIT - 1)
#define FSIGNIF     (FLOATBITS  - _FEXPLEN + _HIDDENBIT - 1)
#define DMAXPOWTWO  ((double)(1L << LONGBITS -2)*(1L << DSIGNIF - LONGBITS +1))
#define FMAXPOWTWO  ((float)(1L << FSIGNIF - 1))

#endif

