/* Copyright (C) 1990 Aladdin Enterprises.  All rights reserved.
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

/* gxarith.h */
/* Arithmetic macros for GhostScript library */

/* Define an in-line abs function, good for any signed numeric type. */
#define any_abs(x) ((x) < 0 ? -(x) : (x))

/* Find the high-order bit of a positive or unsigned long variable. */
#define num_bits(x)\
  ("\000\001\002\002\003\003\003\003\004\004\004\004\004\004\004\004"\
   [(x >= 0x10000L ?\
     (uint)(x >> (x >= 0x1000000L ?\
                  (x >= 0x10000000L ? 28 : 24) :\
                  (x >= 0x100000L ? 20 : 16))) :\
     (uint)x >> ((uint)x >= 0x100 ?\
                  ((uint)x >= 0x1000 ? 12 : 8) :\
                  ((uint)x >= 0x10 ? 4 : 0)))\
    & 15])

/* Test floating point values against zero. */
#ifdef vax				/* not IEEE format! */
#  define is_fzero(f) ((f) == 0)
#  define is_fzero2(f1,f2) ((f1) == 0 && (f2) == 0)
#  define is_fneg(f) ((f) < 0)
#else
#  define _f_as_l(f) *(long *)(&(f))
#  define is_fzero(f) ((_f_as_l(f) << 1) == 0)	/* +0 or -0 */
#  define is_fzero2(f1,f2) (((_f_as_l(f1) | _f_as_l(f2)) << 1) == 0)
#  define is_fneg(f) ((_f_as_l(f)) < 0)	/* -0 is negative, oh well */
#endif
