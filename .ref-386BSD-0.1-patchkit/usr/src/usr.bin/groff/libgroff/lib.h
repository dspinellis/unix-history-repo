/*-
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 *
 *	@(#)lib.h	6.4 (Berkeley) 5/8/91
 */

/* Copyright (C) 1989, 1990 Free Software Foundation, Inc.
     Written by James Clark (jjc@jclark.uucp)

This file is part of groff.

groff is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any later
version.

groff is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with groff; see the file LICENSE.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

/* This file is included in both C and C++ compilations. */

#ifdef __cplusplus
extern "C" {
  // const char *strerror(int);
  // const char *itoa(int);
  // const char *iftoa(int, int);
};

const char *itoa(int);
const char *iftoa(int, int);

char *strsave(const char *s);

int interpret_lf_args(const char *p);

inline int illegal_input_char(int c)
{
  return c == 000 || (c > 012 && c < 040) || (c >= 0200 && c < 0240);
}

#endif

#ifndef INT_MAX
#define INT_MAX 2147483647
#endif

/* It's not safe to rely on people getting INT_MIN right (ie signed). */

#ifdef INT_MIN
#undef INT_MIN
#endif

#ifdef CFRONT_ANSI_BUG

/* This works around a bug in cfront 2.0 used with ANSI C compilers. */

#define INT_MIN ((long)(-INT_MAX-1))

#else /* CFRONT_ANSI_BUG */

#define INT_MIN (-INT_MAX-1)

#endif /* CFRONT_ANSI_BUG */

/* Maximum number of digits in the decimal representation of an int
(not including the -). */

#define INT_DIGITS 10

/* From the old GNU math.h; does some standard actually require these? */
#define M_E         2.7182818284590452354
#define M_LOG2E     1.4426950408889634074
#define M_LOG10E    0.43429448190325182765
#define M_LN2       0.69314718055994530942
#define M_LN10      2.30258509299404568402
#define M_PI        3.14159265358979323846
#define M_PI_2      1.57079632679489661923
#define M_1_PI      0.31830988618379067154
#define M_PI_4      0.78539816339744830962
#define M_2_PI      0.63661977236758134308
#define M_2_SQRTPI  1.12837916709551257390
#define M_SQRT2     1.41421356237309504880
#define M_SQRT1_2   0.70710678118654752440
#define PI  M_PI
#define PI2  M_PI_2
