/* Copyright (C) 1989 Aladdin Enterprises.  All rights reserved.
   Distributed by Free Software Foundation, Inc.

This file is part of Ghostscript.

Ghostscript is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY.  No author or distributor accepts responsibility
to anyone for the consequences of using it or for whether it serves any
particular purpose or works at all, unless he says so in writing.  Refer
to the Ghostscript General Public License for full details.

Everyone is granted permission to copy, modify and redistribute
Ghostscript, but only under the conditions described in the Ghostscript
General Public License.  A copy of this license is supposed to have been
given to you along with Ghostscript so you can know your rights and
responsibilities.  It should be in a file named COPYING.  Among other
things, the copyright notice and this notice must be preserved on all
copies.  */

/* vmsmath.h */
/* Substitute for math.h on VAX/VMS systems */

/*  DEC VAX/VMS C comes with a math.h file but GNU VAX/VMS C does not */
#  ifndef __MATH
#    define __MATH
#    if CC$gfloat
#      define HUGE_VAL 8.988465674311578540726371186585e+307
#    else
#      define HUGE_VAL 1.70141183460469229e+38
#    endif
     extern double acos(), asin(), atan(), atan2();
     extern double sin(), tan(), cos();
     extern double cosh(), sinh(), tanh();
     extern double exp(), frexp(), ldexp(), log(), log10(), pow();
     extern double modf(), fmod(), sqrt(), ceil(), floor();
     extern double fabs(), cabs(), hypot();
#  endif
