/* Definitions of target machine for GNU compiler.  For Sun 2 running Sunos 4.
   Copyright (C) 1987, 1988, 1991 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "tm-sun2.h"

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "\t.double 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : double_is_minus_zero ((VALUE))					\
   ? fprintf (FILE, "\t.long 0x80000000,0\n")				\
   : fprintf (FILE, "\t.double 0r%.20e\n", (VALUE)))

/* This is how to output an assembler line defining a `float' constant.  */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "\t.single 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : double_is_minus_zero ((VALUE))					\
   ? fprintf (FILE, "\t.long 0x80000000\n")				\
   : fprintf (FILE, "\t.single 0r%.20e\n", (VALUE)))

#undef ASM_OUTPUT_FLOAT_OPERAND
#define ASM_OUTPUT_FLOAT_OPERAND(FILE,VALUE)				\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "#0r%s99e999", ((VALUE) > 0 ? "" : "-")) 		\
   : double_is_minus_zero ((VALUE))					\
   ? fprintf (FILE, "#0r-0.0")						\
   : fprintf (FILE, "#0r%.9g", (VALUE)))

#undef ASM_OUTPUT_DOUBLE_OPERAND
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "#0r%s99e999", ((VALUE) > 0 ? "" : "-"))		\
   : double_is_minus_zero ((VALUE))					\
   ? fprintf (FILE, "#0r-0.0")						\
   : fprintf (FILE, "#0r%.20g", (VALUE)))
