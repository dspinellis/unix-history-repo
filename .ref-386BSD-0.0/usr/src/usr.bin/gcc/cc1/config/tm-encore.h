/* Definitions of target machine for GNU compiler.  ENCORE NS32000 version.
   Copyright (C) 1988 Free Software Foundation, Inc.
   Adapted by Robert Brown (brown@harvard.harvard.edu) from the Sequent
     version by Michael Tiemann (tiemann@mcc.com).

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


/*
 *  Looks like all multiprocessors have this bug!
 */

#define SEQUENT_ADDRESS_BUG 1

#include "tm-ns32k.h"

#define SDB_DEBUGGING_INFO
#undef DBX_REGISTER_NUMBER
#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Cause long-jump assembler to be used,
   since otherwise some files fail to be assembled right.  */
#define ASM_SPEC "-j"

#undef ASM_FILE_START
#undef ASM_GENERATE_INTERNAL_LABEL
#undef ASM_OUTPUT_ADDR_DIFF_ELT
#undef ASM_OUTPUT_ALIGN
#undef ASM_OUTPUT_ASCII
#undef ASM_OUTPUT_DOUBLE
#undef ASM_OUTPUT_INT
#undef ASM_OUTPUT_INTERNAL_LABEL
#undef ASM_OUTPUT_LOCAL
#undef CPP_PREDEFINES
#undef FUNCTION_BOUNDARY
#undef PRINT_OPERAND
#undef PRINT_OPERAND_ADDRESS
#undef TARGET_VERSION
#undef FUNCTION_PROFILER


#define TARGET_DEFAULT 1
#define TARGET_VERSION fprintf (stderr, " (32000, Encore syntax)");
/* Note Encore does not standardly do -Dencore.  */
/* budd: should have a -ns32332 (or -apc) switch! but no harm for now */
#define CPP_PREDEFINES "-Dns32000 -Dn16 -Dns16000 -Dns32332 -Dunix"

/* Ignore certain cpp directives used in header files on sysV.  */
#define SCCS_DIRECTIVE

/* Output #ident as a .ident.  */
#define ASM_OUTPUT_IDENT(FILE, NAME) fprintf (FILE, "\t.ident \"%s\"\n", NAME);

/* The .file command should always begin the output.  */
#define ASM_FILE_START(FILE) sdbout_filename ((FILE), main_input_filename)

#define FUNCTION_BOUNDARY 128		/* speed optimization */

/*
 *  The Encore assembler uses ".align 2" to align on 2-byte boundaries.
 */

#define ASM_OUTPUT_ALIGN(FILE,LOG)					\
	fprintf (FILE, "\t.align %d\n", 1 << (LOG))

/*
 *  Internal labels are prefixed with a period.
 */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)			\
	sprintf (LABEL, "*.%s%d", PREFIX, NUM)
#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)			\
	fprintf (FILE, ".%s%d:\n", PREFIX, NUM)
#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)			\
	fprintf (FILE, "\t.word .L%d-.LI%d\n", VALUE, REL)

/*
 *  Different syntax for integer constants, double constants, and
 *  uninitialized locals.
 */

#define ASM_OUTPUT_INT(FILE,VALUE)				\
( fprintf (FILE, "\t.double "),					\
  output_addr_const (FILE, (VALUE)),				\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)				\
 fprintf (FILE, "\t.long 0f%.20e\n", (VALUE))

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)		\
( fputs ("\t.bss ", (FILE)),					\
  assemble_name ((FILE), (NAME)),				\
  fprintf ((FILE), ",%u,%u\n", (SIZE), (ROUNDED)))

 /*
  *  Encore assembler can't handle huge string constants like the one in
  *  gcc.c.  If the default routine in varasm.c were more conservative, this
  *  code could be eliminated.  It starts a new .ascii directive every 40
  *  characters.
  */

#define ASM_OUTPUT_ASCII(file, p, size)			\
{							\
  for (i = 0; i < size; i++)				\
    {							\
      register int c = p[i];				\
      if ((i / 40) * 40 == i)				\
      if (i == 0)					\
        fprintf (file, "\t.ascii \"");			\
      else						\
        fprintf (file, "\"\n\t.ascii \"");		\
      if (c == '\"' || c == '\\')			\
        putc ('\\', file);				\
      if (c >= ' ' && c < 0177)				\
        putc (c, file);					\
      else						\
        {						\
          fprintf (file, "\\%o", c);			\
          if (i < size - 1 				\
              && p[i + 1] >= '0' && p[i + 1] <= '9')	\
          fprintf (file, "\"\n\t.ascii \"");		\
        }						\
    }							\
  fprintf (file, "\"\n");				\
}

 /*
  *  Dollar signs are required before immediate operands, double
  *  floating point constants use $0f syntax, and external addresses
  *  should be prefixed with a question mark to avoid assembler warnings
  *  about undefined symbols.
  */

#define PRINT_OPERAND(FILE, X, CODE)					\
{ if (CODE == '$') putc ('$', FILE);					\
  else if (CODE == '?') fputc ('?', FILE);				\
  else if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    {									\
      rtx xfoo;								\
      xfoo = XEXP (X, 0);						\
      switch (GET_CODE (xfoo))						\
	{								\
	case MEM:							\
	  if (GET_CODE (XEXP (xfoo, 0)) == REG)				\
	    if (REGNO (XEXP (xfoo, 0)) == STACK_POINTER_REGNUM)		\
	      fprintf (FILE, "0(0(sp))");				\
	    else fprintf (FILE, "0(0(%s))",				\
			  reg_names[REGNO (XEXP (xfoo, 0))]);		\
	  else								\
	    {								\
	      fprintf (FILE, "0(");					\
	      output_address (xfoo);					\
	      putc (')', FILE);						\
	    }								\
	  break;							\
	case REG:							\
	  fprintf (FILE, "0(%s)", reg_names[REGNO (xfoo)]);		\
	  break;							\
	case PRE_DEC:							\
	case POST_INC:							\
	  fprintf (FILE, "tos");					\
	  break;							\
	case CONST_INT:							\
	  fprintf (FILE, "@%d", INTVAL (xfoo));				\
	  break;							\
	default:							\
	  output_address (xfoo);					\
	  break;							\
	}								\
    }									\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != DImode)	\
    if (GET_MODE (X) == DFmode)						\
      { union { double d; int i[2]; } u;				\
	u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
	fprintf (FILE, "$0f%.20e", u.d); }				\
    else { union { double d; int i[2]; } u;				\
	   u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X); \
	   fprintf (FILE, "$0f%.20e", u.d); }				\
  else if (GET_CODE (X) == CONST)					\
    output_addr_const (FILE, X);					\
  else { putc ('$', FILE); output_addr_const (FILE, X); }}

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  print_operand_address(FILE, ADDR)

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\taddr .LP%d,r0\n\tjsr mcount\n", (LABELNO))
