/* Definitions of target machine for GNU compiler.

   Citicorp/TTI Unicom PBB version
   (using GAS and encapsulated COFF)

   Copyright (C) 1987, 1988, 1990 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "m68k.h"

/* Without STRUCTURE_SIZE_BOUNDARY, we can't ensure that structures are
   aligned such that we can correctly extract bitfields from them.
   Someone should check whether the usual compiler on this machine
   provides the equivalent behavior of STRUCTURE_SIZE_BOUNDARY.  */
/* Alternative solutions are (1) define PCC_BITFIELD_TYPE_MATTERS,
   if that fits what the usual compiler does,
   or disable the -m68000 and -mnobitfield options.  */
#error This doesn't define STRUCTURE_SIZE_BOUNDARY

/* See m68k.h.  5 means 68020 without 68881.  */

#define TARGET_DEFAULT 5

/* Define __HAVE_68881__ in preprocessor if -m68881 is specified.
   This will control the use of inline 68881 insns in certain macros.  */

#define CPP_SPEC "%{m68881:-D__HAVE_68881__}"

/* Since some host machines will have to avoid generating code that
   knows the format of floating point numbers on the tti68K,
   we force all of them to avoid it when compiling for this
   target, so that they will all generate exactly the same assembler
   code as a result.  Otherwise code cross-hosted from a Vax would
   differ from native-compiled code.  The performance penalty should
   be minor in most cases.  Note that we still do constant folding
   in the host float format, so things might be a few bits off here
   or there.  */

#define	CAN_EXAMINE_68K_REAL_GUTS	0

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dm68k -Dunix -DUnicomPBB -Dmc68k -Dmc68020 -Dmc68k32"

/* We want DBX format for use with gdb under encapsulated coff.  */

#define DBX_DEBUGGING_INFO

/* Generate calls to memcpy, memcmp and memset.  */

#define TARGET_MEM_FUNCTIONS

/* -m68000 requires special flags to the assembler.  */

#define ASM_SPEC \
 "%{m68000:-mc68010}%{mc68000:-mc68010}"

/* we use /lib/libp/lib*  when profiling */

#define LIB_SPEC "%{!shlib:%{p:-L/lib/libp} %{pg:-L/lib/libp} -lc}"

/* shared libraries need to use crt1.o  */

/*#define STARTFILE_SPEC*/
#define STARTFILE_SPEC \
  "%{!shlib:%{pg:mcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}}\
   %{shlib:crt1.o%s shlib.ifile%s} "

/* cpp has to support a #sccs directive for the /usr/include files */

#define SCCS_DIRECTIVE

/* Define subroutines to call to handle multiply, divide, and remainder.
   Use the subroutines that the 3b1's library provides.
   The `*' prevents an underscore from being prepended by the compiler.  */

#ifdef notnow
#define DIVSI3_LIBCALL "*ldiv"
#define UDIVSI3_LIBCALL "*uldiv"
#define MODSI3_LIBCALL "*lrem"
#define UMODSI3_LIBCALL "*ulrem"
#define MULSI3_LIBCALL "*lmul"
#define UMULSI3_LIBCALL "*ulmul"
#endif

#ifndef __GNUC__
#define USE_C_ALLOCA
#endif

/* Return pointer values in both d0 and a0.  */

#undef FUNCTION_EXTRA_EPILOGUE
#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)			\
{								\
  extern int current_function_returns_pointer;			\
  if ((current_function_returns_pointer) && 			\
      ! find_equiv_reg (0, get_last_insn (), 0, 0, 0, 8, Pmode))\
    fprintf (FILE, "\tmovel d0,a0\n");				\
}
