/* Definitions for BSD assembler syntax for Intel 386
   (actually AT&T syntax for insns and operands,
   adapted to BSD conventions for symbol names and debugging.)
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

/* Use the Sequent Symmetry assembler syntax.  */

#define TARGET_VERSION fprintf (stderr, " (80386, BSD syntax)");

/* Define some concatenation macros to concatenate an opcode
   and one, two or three operands.  In other assembler syntaxes
   they may alter the order of ther operands.  */

#ifdef __STDC__
#define AS2(a,b,c) #a " " #b "," #c
#define AS3(a,b,c,d) #a " " #b "," #c "," #d
#define AS1(a,b) #a " " #b
#else
#define AS1(a,b) "a b"
#define AS2(a,b,c) "a b,c"
#define AS3(a,b,c,d) "a b,c,d"
#endif  

/* Output the size-letter for an opcode.
   CODE is the letter used in an operand spec (L, B, W, S or Q).
   CH is the corresponding lower case letter
     (except if CODE is L then CH is `l').  */
#define PUT_OP_SIZE(CODE,CH,FILE) putc (CH,(FILE))

/* Opcode suffix for fullword insn.  */
#define L_SIZE "l"

/* Prefix for register names in this syntax.  */
#define RP "%"

/* Prefix for immediate operands in this syntax.  */
#define IP "$"

/* Prefix for internally generated assembler labels.  */
#define LPREFIX "L"

/* Output the prefix for an immediate operand, or for an offset operand.  */
#define PRINT_IMMED_PREFIX(FILE)  fputs ("$", (FILE))
#define PRINT_OFFSET_PREFIX(FILE)  fputs ("$", (FILE))

/* Indirect call instructions should use `*'.  */
#define USE_STAR 1

/* Prefix for a memory-operand X.  */
#define PRINT_PTR(X, FILE)

/* Delimiters that surround base reg and index reg.  */
#define ADDR_BEG(FILE) putc('(', (FILE))
#define ADDR_END(FILE) putc(')', (FILE))

/* Print an index register (whose rtx is IREG).  */
#define PRINT_IREG(FILE,IREG) \
  do								\
  { fputs (",", (FILE)); PRINT_REG ((IREG), 0, (FILE)); }	\
  while (0)
  
/* Print an index scale factor SCALE.  */
#define PRINT_SCALE(FILE,SCALE) \
  if ((SCALE) != 1) fprintf ((FILE), ",%d", (SCALE))

/* Print a base/index combination.
   BREG is the base reg rtx, IREG is the index reg rtx,
   and SCALE is the index scale factor (an integer).  */

#define PRINT_B_I_S(BREG,IREG,SCALE,FILE) \
  { ADDR_BEG (FILE); 				\
    if (BREG) PRINT_REG ((BREG), 0, (FILE));	\
    if ((IREG) != 0)				\
      { PRINT_IREG ((FILE), (IREG));		\
        PRINT_SCALE ((FILE), (SCALE)); }	\
    ADDR_END (FILE); }

/* Define the syntax of pseudo-ops, labels and comments.  */

/* Assembler pseudos to introduce constants of various size.  */

#define ASM_BYTE "\t.byte "
#define ASM_SHORT "\t.word "
#define ASM_LONG "\t.long "
#define ASM_DOUBLE "\t.double "

/* String containing the assembler's comment-starter.  */

#define COMMENT_BEGIN "/"

/* Output at beginning of assembler file.
   ??? I am skeptical of this -- RMS.  */

#define ASM_FILE_START(FILE) \
  fprintf (FILE, "\t.file\t\"%s\"\n", dump_base_name);

/* This was suggested, but it shouldn't be right for DBX output. -- RMS
   #define ASM_OUTPUT_SOURCE_FILENAME(FILE, NAME) */

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "/APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "/NO_APP\n"

/* Define the syntax of labels and symbol definitions/declarations.  */

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.space %u\n", (SIZE))

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* Define the syntax of labels and symbol definitions/declarations.  */

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (SIZE)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (SIZE)))

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
     if ((LOG)!=0) fprintf ((FILE), "\t.align %d\n", (LOG))

/* This is how to store into the string BUF
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(BUF,PREFIX,NUMBER)	\
    sprintf ((BUF), "*%s%d", (PREFIX), (NUMBER))

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to output a reference to a user-level label named NAME.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* Sequent has some changes in the format of DBX symbols.  */
#define DBX_NO_XREFS 1

/* Don't split DBX symbols into continuations.  */
#define DBX_CONTIN_LENGTH 0
 
/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  (fputs (".globl ", FILE), assemble_name (FILE, NAME), fputs ("\n", FILE))

/* This is how to output an assembler line defining a `double' constant.  */

#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\t.double 0d%.20e\n", (VALUE))
