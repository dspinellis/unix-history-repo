/* Definitions for IBM PS2 running AIX/386.
   From: Minh Tran-Le <TRANLE@intellicorp.com>
   Copyright (C) 1988 Free Software Foundation, Inc.

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


#include "tm-i386.h"

/* Use the ATT assembler syntax.  */

#include "tm-att386.h"

/* By default, target has a 80387.  */

#define TARGET_DEFAULT 1

/* Use crt1.o as a startup file and crtn.o as a closing file.  */

#define STARTFILE_SPEC  \
  "%{pg:gcrt1.o%s}%{!pg:%{p:mcrt1.o%s}%{!p:crt1.o%s}}"

#define LIB_SPEC "%{p:-L/usr/lib/libp}%{pg:-L/usr/lib/libp} -lc crtn.o%s"

/* Special flags for the linker.  I don't know what they do.  */

#define LINK_SPEC "%{!K:-K} %{!T*:-T0x00400000} %{z:-lm}"

/* Specify predefined symbols in preprocessor.  */

#define CPP_PREDEFINES "-D_I386 -Di386 -DAIX -D_AIX"

/* special flags for the aix assembler to generate the short form for all
   qualifying forward reference */

#define ASM_SPEC "-s2"

/* Allow #sccs in preprocessor.  */

#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */

#define ASM_OUTPUT_IDENT(FILE, NAME) fprintf (FILE, "\t.ident \"%s\"\n", NAME);

#undef ASM_FILE_START
#define ASM_FILE_START(FILE) 					\
  do { fprintf (FILE, "\t.file\t\"%s\"\n", dump_base_name);	\
     } while (0)
/* This used to output .noopt if nonoptimizing and run ASM_FILE_START_1
   if optimizing, but that loses with the AIX assembler.  */

/* This was suggested, but it shouldn't be right for DBX output. -- RMS
   #define ASM_OUTPUT_SOURCE_FILENAME(FILE, NAME) */

/* We want to output SDB debugging information. */

#define SDB_DEBUGGING_INFO

/* We don't want to output DBX debugging information. Becaus IBM AIX dbx
   use COFF format */

#undef DBX_DEBUGGING_INFO

/* Implicit library calls should use memcpy, not bcopy, etc.  */

#define TARGET_MEM_FUNCTIONS

/* Writing `int' for a bitfield forces int alignment for the structure.  */

#define PCC_BITFIELD_TYPE_MATTERS 1

/* Don't write a `.optim' pseudo; this assembler
   is said to have a bug when .optim is used.  */

#if 0
#undef ASM_FILE_START_1
#define ASM_FILE_START_1(FILE) fprintf (FILE, "\t.noopt\n");
#endif

/* Machines that use the AT&T assembler syntax
   also return floating point values in an FP register.  */
/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

#define VALUE_REGNO(MODE) \
  (((MODE)==SFmode || (MODE)==DFmode) ? FIRST_FLOAT_REG : 0)

/* 1 if N is a possible register number for a function value. */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0 || (N)== FIRST_FLOAT_REG)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#undef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tleal %sP%d,%%eax\n\tcall mcount\n", LPREFIX, (LABELNO));
