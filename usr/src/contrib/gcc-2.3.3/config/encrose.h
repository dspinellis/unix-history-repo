/* Definitions of target machine for GNU compiler.  
   Encore Multimax (OSF/1 with OSF/rose) version.
   Copyright (C) 1991 Free Software Foundation, Inc.

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

/* Output a '?' so the encore assembler won't complain about
 * externals on branches it hasn't seen yet. */

#define EXTERNAL_PREFIX '?'

#include "encore.h"

/* External variables defined in ns32k.c */

extern int ns32k_num_files;


#define OSF_OS
#define NO_J_SWITCH

/* MACHINE MACROS controlling the compilation driver, 'gcc' */

#ifdef  CPP_SPEC  
#undef  CPP_SPEC
#endif
#define CPP_SPEC       "%{.S:	-D__LANGUAGE_ASSEMBLY__			\
				-D_LANGUAGE_ASSEMBLY			\
				%{!ansi:-DLANGUAGE_ASSEMBLY}}		\
			 %{!.S:	-D__LANGUAGE_C__			\
				-D_LANGUAGE_C				\
				%{!ansi:-DLANGUAGE_C}}"

#ifdef  CC1_SPEC
#undef  CC1_SPEC
#endif
#define CC1_SPEC       ""

#ifndef NO_J_SWITCH
#ifdef  ASM_SPEC
#undef  ASM_SPEC
#endif
#define ASM_SPEC       "%{!j: -j}"
#endif

#ifdef NO_J_SWITCH
#ifdef ASM_SPEC
#undef ASM_SPEC
#endif
#define ASM_SPEC       ""
#endif

#ifdef  LINK_SPEC
#undef  LINK_SPEC
#endif
#define LINK_SPEC      "%{nostdlib}"

#ifdef  LIB_SPEC
#undef  LIB_SPEC
#endif
#define LIB_SPEC       "%{p:-lprof1} -lc"

#ifdef  LIBG_SPEC
#undef  LIBG_SPEC
#endif
#define LIBG_SPEC      ""

#ifdef  STARTFILE_SPEC  
#undef  STARTFILE_SPEC
#endif
#define STARTFILE_SPEC "%{pg:gcrt0.o%s}%{!pg:%{p:mcrt0.o%s}%{!p:crt0.o%s}}"

#ifdef  MACHINE_TYPE
#undef  MACHINE_TYPE
#endif
#define MACHINE_TYPE   "Encore Multimax running OSF/1 with OSF/rose objects"

#ifdef  ASM_VERSION
#undef  ASM_VERSION
#endif
#define ASM_VERSION    "Encore syntax using GAS/stabs"

#ifdef  MD_EXEC_PREFIX  
#undef  MD_EXEC_PREFIX
#endif
#define MD_EXEC_PREFIX		"/usr/ccs/gcc/"

#ifdef  MD_STARTFILE_PREFIX  
#undef  MD_STARTFILE_PREFIX
#endif
#define MD_STARTFILE_PREFIX	"/usr/ccs/lib/"

/* MACHINE MACROS controlling run-time target specification */

#ifdef CPP_PREDEFINES
#undef CPP_PREDEFINES
#endif
#define CPP_PREDEFINES "-Dns32000 -Dn16 -Dns16000 -Dns32332 -Dunix -Dmultimax -DMULTIMAX -DOSF -DOSF1"

#ifdef  TARGET_DEFAULT
#undef  TARGET_DEFAULT
#endif
#define TARGET_DEFAULT   33  /* 32081 and no (sb) register */

/* MACHINE MACROS controlling the layout of C types */

#ifdef SIZE_TYPE  
#undef SIZE_TYPE
#endif
#define SIZE_TYPE	"long unsigned int"

#ifdef  PTRDIFF_TYPE
#undef  PTRDIFF_TYPE
#endif
#define PTRDIFF_TYPE	"int"

#ifdef  WCHAR_TYPE
#undef  WCHAR_TYPE
#endif
#define WCHAR_TYPE	"unsigned int"

#ifdef WCHAR_TYPE_SIZE
#undef WCHAR_TYPE_SIZE
#endif
#define WCHAR_TYPE_SIZE  BITS_PER_WORD

#if 0
#define WCHAR_TYPE	((TARGET_WC8)					\
				? "unsigned char"			\
				: ((TARGET_WC16)			\
					? "short unsigned int"		\
					: "long unsigned int"))
#define MULTIBYTE_CHARS  1
#endif

/* MACHINE MACROS describing stack layout and calling conventions */


#ifdef  FUNCTION_PROFILER
#undef  FUNCTION_PROFILER
#endif
#define FUNCTION_PROFILER(FILE, LABELNO) fprintf (FILE, "\tjsr mcount\n")

/* MACHINE MACROS describing the overall framework of an
   assembly file */

#ifdef PRINT_OPERAND
#undef PRINT_OPERAND
#endif

#define PRINT_OPERAND(FILE, X, CODE)       print_operand(FILE, X, CODE)

#ifdef PRINT_OPERAND_ADDRESS
#undef PRINT_OPERAND_ADDRESS
#endif

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address(FILE, ADDR)

#define STABS_DIRECTIVE	"\t.stabs"	/* align .stabs */
#define STABN_DIRECTIVE	"\t.stabn"	/* align .stabn */
#define STABD_DIRECTIVE	"\t.stabd"	/* align .stabd */

/* How to tell the debugger about changes of source files.  */

#ifdef ASM_OUTPUT_SOURCE_FILENAME
#undef ASM_OUTPUT_SOURCE_FILENAME
#endif

/* Perhaps it is no longer necessary to redefine this, now that dbxout.c
   uses another macro instead.  */
#define ASM_OUTPUT_SOURCE_FILENAME(FILE, NAME)				\
{									\
  if (ns32k_num_files++ == 0)						\
    fprintf (FILE, "\t.file \"%s\"\n", NAME);				\
  else if (write_symbols == DBX_DEBUG)					\
  {                                                                     \
    char ltext_label_name[100];                                         \
    ASM_GENERATE_INTERNAL_LABEL(ltext_label_name, "Ltext", 0);          \
    fprintf (FILE, "%s \"%s\",%d,0,0,%s\n", STABS_DIRECTIVE,		\
	     NAME, 0x84 /* N_SOL */, &ltext_label_name[1]);		\
  }                                                                     \
  else									\
    fprintf (FILE, "\t#.file \"%s\"\n", NAME);				\
}

/* This is how to output a note the debugger telling it the line number
   to which the following sequence of instructions corresponds.  */

#ifdef ASM_OUTPUT_SOURCE_LINE
#undef ASM_OUTPUT_SOURCE_LINE
#endif
#define ASM_OUTPUT_SOURCE_LINE(FILE, LINE)				\
{									\
  if (write_symbols != DBX_DEBUG)					\
    fprintf (FILE, "\t.ln %d\n", LINE);					\
  else									\
    fprintf (file, "%s %d,0,%d\n", STABD_DIRECTIVE, 0x44 /* N_SLINE */,	\
	     LINE);							\
}

#ifdef ASM_OUTPUT_INT
#undef ASM_OUTPUT_INT
#endif
#ifndef COLLECT
#define ASM_OUTPUT_INT(FILE,VALUE)				\
( fprintf (FILE, "\t.double "),					\
  output_addr_const (FILE, (VALUE)),				\
  fprintf (FILE, "\n"))
#else
#define ASM_OUTPUT_INT(STREAM,VALUE)					\
  fprintf (STREAM, "\t.double\t%d\n", VALUE)
#endif

/* MACHINE MACROS describing debugging information */

/* We need to have the capability to generate either
   .stabs or COFF style debug info. The file ns32k.h
   define DBX_DEBUGGING_INFO. We disambiguate between
   the two based on a combination of command line or
   default behavior. */

#ifndef SDB_DEBUGGING_INFO
#define SDB_DEBUGGING_INFO  /* Generate COFF style debug info */
#endif

#ifndef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO  /* Generate .stabs style debug info */
#endif

/* If the user just types -g, we want to use stabs */

#ifdef PREFERRED_DEBUGGING_TYPE
#undef PREFERRED_DEBUGGING_TYPE
#endif

#define PREFERRED_DEBUGGING_TYPE      DBX_DEBUG

/* Define the gdb extensions */

#define DEFAULT_GDB_EXTENSIONS        1

/* Tell collect that the object format is OSF/rose.  */
#define OBJECT_FORMAT_ROSE

/* Use atexit for static constructors/destructors, instead of defining
   our own exit function.  */
#define HAVE_ATEXIT

/* Generate calls to memcpy, etc., not bcopy, etc.  */
#define TARGET_MEM_FUNCTIONS

/* A C statement to output assembler commands which will identify
   the object file as having been compiled with GNU CC (or another
   GNU compiler).

   If you don't define this macro, the string `gcc2_compiled.:' is
   output.  This string is calculated to define a symbol which, on
   BSD systems, will never be defined for any other reason.  GDB
   checks for the presence of this symbol when reading the symbol
   table of an executable.

   On non-BSD systems, you must arrange communication with GDB in
   some other fashion.  If GDB is not used on your system, you can
   define this macro with an empty body.

   On OSF/1, gcc2_compiled. confuses the kernel debugger, so don't
   put it out.  */

#define ASM_IDENTIFY_GCC(STREAM)
