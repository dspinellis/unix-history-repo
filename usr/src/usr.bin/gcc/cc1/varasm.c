/* Output variables, constants and external declarations, for GNU compiler.
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.

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


/* This file handles generation of all the assembler code
   *except* the instructions of a function.
   This includes declarations of variables and their initial values.

   We also output the assembler code for constants stored in memory
   and are responsible for combining constants with the same value.  */

#include <stdio.h>
#include <setjmp.h>
/* #include <stab.h> */
#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "expr.h"
#include "hard-reg-set.h"

#include "obstack.h"

#define MIN(a, b) ((a) < (b) ? (a) : (b))

/* File in which assembler code is being written.  */

extern FILE *asm_out_file;

extern struct obstack *current_obstack;
extern struct obstack *saveable_obstack;
extern struct obstack permanent_obstack;
#define obstack_chunk_alloc xmalloc
extern int xmalloc ();

/* Number for making the label on the next
   constant that is stored in memory.  */

int const_labelno;

/* Number for making the label on the next
   static variable internal to a function.  */

int var_labelno;

/* Nonzero if at least one function definition has been seen.  */
static int function_defined;

extern FILE *asm_out_file;

static char *compare_constant_1 ();
static void record_constant_1 ();
void assemble_name ();
void output_addressed_constants ();
void output_constant ();
void output_constructor ();

#ifdef EXTRA_SECTIONS
static enum in_section {no_section, in_text, in_data, EXTRA_SECTIONS} in_section
  = no_section;
#else
static enum in_section {no_section, in_text, in_data} in_section
  = no_section;
#endif

/* Define functions like text_section for any extra sections.  */
#ifdef EXTRA_SECTION_FUNCTIONS
EXTRA_SECTION_FUNCTIONS
#endif

/* Tell assembler to switch to text section.  */

void
text_section ()
{
  if (in_section != in_text)
    {
      fprintf (asm_out_file, "%s\n", TEXT_SECTION_ASM_OP);
      in_section = in_text;
    }
}

/* Tell assembler to switch to data section.  */

void
data_section ()
{
  if (in_section != in_data)
    {
      if (flag_shared_data)
	{
#ifdef SHARED_SECTION_ASM_OP
	  fprintf (asm_out_file, "%s\n", SHARED_SECTION_ASM_OP);
#else
	  fprintf (asm_out_file, "%s\n", DATA_SECTION_ASM_OP);
#endif
	}
      else
	fprintf (asm_out_file, "%s\n", DATA_SECTION_ASM_OP);

      in_section = in_data;
    }
}

/* Determine if we're in the text section. */

int
in_text_section ()
{
  return in_section == in_text;
}

/* Create the rtl to represent a function, for a function definition.
   DECL is a FUNCTION_DECL node which describes which function.
   The rtl is stored into DECL.  */

void
make_function_rtl (decl)
     tree decl;
{
  if (DECL_RTL (decl) == 0)
    DECL_RTL (decl)
      = gen_rtx (MEM, DECL_MODE (decl),
		 gen_rtx (SYMBOL_REF, Pmode, DECL_ASSEMBLER_NAME (decl)));

  /* Record at least one function has been defined.  */
  function_defined = 1;
}

/* Decode an `asm' spec for a declaration as a register name.
   Return the register number, or -1 if nothing specified,
   or -2 if the name is not a register.  */

int
decode_reg_name (asmspec)
     char *asmspec;
{
  if (asmspec != 0)
    {
      int i;
      extern char *reg_names[];

      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	if (!strcmp (asmspec, reg_names[i]))
	  break;

      if (i < FIRST_PSEUDO_REGISTER)
	return i;
      else
	return -2;
    }

  return -1;
}

/* Create the DECL_RTL for a declaration for a static or external variable
   or static or external function.
   ASMSPEC, if not 0, is the string which the user specified
   as the assembler symbol name.
   TOP_LEVEL is nonzero if this is a file-scope variable.

   This is never called for PARM_DECL nodes.  */

void
make_decl_rtl (decl, asmspec, top_level)
     tree decl;
     char *asmspec;
     int top_level;
{
  register char *name = DECL_ASSEMBLER_NAME (decl);
  int reg_number = decode_reg_name (asmspec);

  if (reg_number == -2)
    {
      name = (char *) obstack_alloc (saveable_obstack,
				     strlen (asmspec) + 2);
      name[0] = '*';
      strcpy (&name[1], asmspec);
    }

  /* For a duplicate declaration, we can be called twice on the
     same DECL node.  Don't alter the RTL already made
     unless the old mode is wrong (which can happen when
     the previous rtl was made when the type was incomplete).  */
  if (DECL_RTL (decl) == 0
      || GET_MODE (DECL_RTL (decl)) != DECL_MODE (decl))
    {
      DECL_RTL (decl) = 0;

      /* First detect errors in declaring global registers.  */
      if (TREE_REGDECL (decl) && reg_number == -1)
	error_with_decl (decl,
			 "register name not specified for `%s'");
      else if (TREE_REGDECL (decl) && reg_number == -2)
	error_with_decl (decl,
			 "invalid register name for `%s'");
      else if (reg_number >= 0 && ! TREE_REGDECL (decl))
	error_with_decl (decl,
			 "register name given for non-register variable `%s'");
      else if (TREE_REGDECL (decl) && TREE_CODE (decl) == FUNCTION_DECL)
	error ("function declared `register'");
      else if (TREE_REGDECL (decl) && TYPE_MODE (TREE_TYPE (decl)) == BLKmode)
	error_with_decl (decl, "data type of `%s' isn't suitable for a register");
      /* Now handle properly declared static register variables.  */
      else if (TREE_REGDECL (decl))
	{
	  int nregs;
	  if (pedantic)
	    warning ("ANSI C forbids global register variables");
	  if (DECL_INITIAL (decl) != 0)
	    {
	      DECL_INITIAL (decl) = 0;
	      error ("global register variable has initial value");
	    }
	  if (fixed_regs[reg_number] == 0
	      && function_defined && top_level)
	    error ("global register variable follows a function definition");
	  DECL_RTL (decl) = gen_rtx (REG, DECL_MODE (decl), reg_number);
	  if (top_level)
	    {
	      /* Make this register fixed, so not usable for anything else.  */
	      nregs = HARD_REGNO_NREGS (reg_number, DECL_MODE (decl));
	      while (nregs > 0)
		global_regs[reg_number + --nregs] = 1;
	      init_reg_sets_1 ();
	    }
	}

      /* Now handle ordinary static variables and functions (in memory).
	 Also handle vars declared register invalidly.  */
      if (DECL_RTL (decl) == 0)
	{
	  /* Can't use just the variable's own name for a variable
	     whose scope is less than the whole file.
	     Concatenate a distinguishing number.  */
	  if (!top_level && !TREE_EXTERNAL (decl) && asmspec == 0)
	    {
	      char *label;

	      ASM_FORMAT_PRIVATE_NAME (label, name, var_labelno);
	      name = obstack_copy0 (saveable_obstack, label, strlen (label));
	      var_labelno++;
	    }

	  DECL_RTL (decl) = gen_rtx (MEM, DECL_MODE (decl),
				     gen_rtx (SYMBOL_REF, Pmode, name));
	  if (TREE_EXTERNAL (decl))
	    EXTERNAL_SYMBOL_P (XEXP (DECL_RTL (decl), 0)) = 1; 
	  if (TREE_VOLATILE (decl))
	    MEM_VOLATILE_P (DECL_RTL (decl)) = 1;
	  if (TREE_READONLY (decl))
	    RTX_UNCHANGING_P (DECL_RTL (decl)) = 1;
	  MEM_IN_STRUCT_P (DECL_RTL (decl))
	    = (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
	       || TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE
	       || TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE);
	}
    }
}

/* Output a string of literal assembler code
   for an `asm' keyword used between functions.  */

void
assemble_asm (string)
     tree string;
{
  app_enable ();

  fprintf (asm_out_file, "\t%s\n", TREE_STRING_POINTER (string));
}

/* Output assembler code associated with defining the name of a function
   as described by DECL.  */

void
assemble_function (decl)
     tree decl;
{
  rtx x, n;
  char *fnname;
  int align;

  /* Get the function's name, as described by its RTL.
     This may be different from the DECL_NAME name used in the source file.  */

  x = DECL_RTL (decl);
  if (GET_CODE (x) != MEM)
    abort ();
  n = XEXP (x, 0);
  if (GET_CODE (n) != SYMBOL_REF)
    abort ();
  fnname = XSTR (n, 0);

  /* The following code does not need preprocessing in the assembler.  */

  app_disable ();

  text_section ();

#ifdef SDB_DEBUGGING_INFO
  /* Make sure types are defined for debugger before fcn name is defined.  */
  if (write_symbols == SDB_DEBUG)
    sdbout_tags (gettags ());
#endif

  /* Tell assembler to move to target machine's alignment for functions.  */

  align = floor_log2 (FUNCTION_BOUNDARY / BITS_PER_UNIT);
  if (align > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, align);

#ifdef SDB_DEBUGGING_INFO
  /* Output SDB definition of the function.  */
  if (write_symbols == SDB_DEBUG)
    sdbout_mark_begin_function ();
#endif

  /* Make function name accessible from other files, if appropriate.  */

  if (TREE_PUBLIC (decl))
    ASM_GLOBALIZE_LABEL (asm_out_file, fnname);

  /* Do any machine/system dependent processing of the function name */
#ifdef ASM_DECLARE_FUNCTION_NAME
  ASM_DECLARE_FUNCTION_NAME (asm_out_file, fnname, current_function_decl);
#else
  /* Standard thing is just output label for the function.  */
  ASM_OUTPUT_LABEL (asm_out_file, fnname);
#endif /* ASM_DECLARE_FUNCTION_NAME */
}

/* Assemble " .int 0\n" or whatever this assembler wants.  */

void
assemble_integer_zero ()
{
  ASM_OUTPUT_INT (asm_out_file, const0_rtx);
}

/* Assemble a string constant with the specified C string as contents.  */

void
assemble_string (p, size)
     unsigned char *p;
     int size;
{
  register int i;
  int excess = 0;
  int pos = 0;
  int maximum = 2000;

  /* If the string is very long, split it up.  */

  while (pos < size)
    {
      int thissize = size - pos;
      if (thissize > maximum)
	thissize = maximum;

#ifdef ASM_OUTPUT_ASCII
      ASM_OUTPUT_ASCII (asm_out_file, p, thissize);
#else
      fprintf (asm_out_file, "\t.ascii \"");

      for (i = 0; i < thissize; i++)
	{
	  register int c = p[i];
	  if (c == '\"' || c == '\\')
	    putc ('\\', asm_out_file);
	  if (c >= ' ' && c < 0177)
	    putc (c, asm_out_file);
	  else
	    {
	      fprintf (asm_out_file, "\\%o", c);
	      /* After an octal-escape, if a digit follows,
		 terminate one string constant and start another.
		 The Vax assembler fails to stop reading the escape
		 after three digits, so this is the only way we
		 can get it to parse the data properly.  */
	      if (i < thissize - 1
		  && p[i + 1] >= '0' && p[i + 1] <= '9')
		fprintf (asm_out_file, "\"\n\t.ascii \"");
	    }
	}
      fprintf (asm_out_file, "\"\n");
#endif /* no ASM_OUTPUT_ASCII */

      pos += thissize;
      p += thissize;
    }
}

/* Assemble everything that is needed for a variable or function declaration.
   Not used for automatic variables, and not used for function definitions.
   Should not be called for variables of incomplete structure type.

   TOP_LEVEL is nonzero if this variable has file scope.
   WRITE_SYMBOLS is DBX_DEBUG if writing dbx symbol output.
   The dbx data for a file-scope variable is written here.
   AT_END is nonzero if this is the special handling, at end of compilation,
   to define things that have had only tentative definitions.  */

void
assemble_variable (decl, top_level, write_symbols, at_end)
     tree decl;
     int top_level;
     enum debugger write_symbols;
     int at_end;
{
  register char *name;
  register int i;

  /* Do nothing for global register variables.  */

  if (GET_CODE (DECL_RTL (decl)) == REG)
    return;

  /* Normally no need to say anything for external references,
     since assembler considers all undefined symbols external.  */

  if (TREE_EXTERNAL (decl))
    return;

  /* Output no assembler code for a function declaration.
     Only definitions of functions output anything.  */

  if (TREE_CODE (decl) == FUNCTION_DECL)
    return;

  /* If type was incomplete when the variable was declared,
     see if it is complete now.  */

  if (DECL_SIZE (decl) == 0)
    layout_decl (decl, 0);

  /* Still incomplete => don't allocate it; treat the tentative defn
     (which is what it must have been) as an `extern' reference.  */

  if (DECL_SIZE (decl) == 0)
    {
      error_with_file_and_line (DECL_SOURCE_FILE (decl),
				DECL_SOURCE_LINE (decl),
				"storage size of static var `%s' isn't known",
				IDENTIFIER_POINTER (DECL_NAME (decl)));
      return;
    }

  /* The first declaration of a variable that comes through this function
     decides whether it is global (in C, has external linkage)
     or local (in C, has internal linkage).  So do nothing more
     if this function has already run.  */

  if (TREE_ASM_WRITTEN (decl))
    return;

  TREE_ASM_WRITTEN (decl) = 1;

#ifdef DBX_DEBUGGING_INFO
  /* File-scope global variables are output here.  */
  if (write_symbols == DBX_DEBUG && top_level)
    dbxout_symbol (decl, 0);
#endif
#ifdef SDB_DEBUGGING_INFO
  if (write_symbols == SDB_DEBUG && top_level)
    sdbout_symbol (decl, 0);
#endif
  if (write_symbols == GDB_DEBUG)
    /* Make sure the file is known to GDB even if it has no functions.  */
    set_current_gdbfile (DECL_SOURCE_FILE (decl));

  /* If storage size is erroneously variable, just continue.
     Error message was already made.  */

  if (! TREE_LITERAL (DECL_SIZE (decl)))
    return;

  app_disable ();

  name = XSTR (XEXP (DECL_RTL (decl), 0), 0);

  /* Handle uninitialized definitions.  */

  if (DECL_INITIAL (decl) == 0 || DECL_INITIAL (decl) == error_mark_node)
    {
      int size = (TREE_INT_CST_LOW (DECL_SIZE (decl))
		  * DECL_SIZE_UNIT (decl)
		  / BITS_PER_UNIT);
      int rounded = size;
      /* Don't allocate zero bytes of common,
	 since that means "undefined external" in the linker.  */
      if (size == 0) rounded = 1;
      /* Round size up to multiple of BIGGEST_ALIGNMENT bits
	 so that each uninitialized object starts on such a boundary.  */
      rounded = ((rounded + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
		 / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));
      if (flag_shared_data)
	data_section ();
      if (TREE_PUBLIC (decl))
	ASM_OUTPUT_COMMON (asm_out_file, name, size, rounded);
      else
	ASM_OUTPUT_LOCAL (asm_out_file, name, size, rounded);
      return;
    }

  /* Handle initialized definitions.  */

  /* First make the assembler name(s) global if appropriate.  */
  if (TREE_PUBLIC (decl) && DECL_NAME (decl))
    ASM_GLOBALIZE_LABEL (asm_out_file, name);
#if 0
  for (d = equivalents; d; d = TREE_CHAIN (d))
    {
      tree e = TREE_VALUE (d);
      if (TREE_PUBLIC (e) && DECL_NAME (e))
	ASM_GLOBALIZE_LABEL (asm_out_file,
			     XSTR (XEXP (DECL_RTL (e), 0), 0));
    }
#endif

  /* Output any data that we will need to use the address of.  */
  if (DECL_INITIAL (decl))
    output_addressed_constants (DECL_INITIAL (decl));

  /* Switch to the proper section for this data.  */
#ifdef SELECT_SECTION
  SELECT_SECTION (decl);
#else
  if (TREE_READONLY (decl) && ! TREE_VOLATILE (decl))
    text_section ();
  else
    data_section ();
#endif

  /* Output the alignment of this data.  */
  for (i = 0; DECL_ALIGN (decl) >= BITS_PER_UNIT << (i + 1); i++);
  if (i > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, i);

  /* Output the name(s) of this data.  */
  ASM_OUTPUT_LABEL (asm_out_file, name);
#if 0
  for (d = equivalents; d; d = TREE_CHAIN (d))
    {
      tree e = TREE_VALUE (d);
      ASM_OUTPUT_LABEL (asm_out_file, XSTR (XEXP (DECL_RTL (e), 0), 0));
    }
#endif

  if (DECL_INITIAL (decl))
    /* Output the actual data.  */
    output_constant (DECL_INITIAL (decl), int_size_in_bytes (TREE_TYPE (decl)));
  else
    /* Leave space for it.  */
    ASM_OUTPUT_SKIP (asm_out_file, int_size_in_bytes (TREE_TYPE (decl)));
}

/* Output something to declare an external symbol to the assembler.
   (Most assemblers don't need this, so we normally output nothing.)  */

void
assemble_external (decl)
     tree decl;
{
  rtx rtl = DECL_RTL (decl);

  if (GET_CODE (rtl) == MEM && GET_CODE (XEXP (rtl, 0)) == SYMBOL_REF)
    {
#ifdef ASM_OUTPUT_EXTERNAL
      /* Some systems do require some output.  */
      ASM_OUTPUT_EXTERNAL (asm_out_file, decl, XSTR (XEXP (rtl, 0), 0));
#endif
    }
}

/* Output to FILE a reference to the assembler name of a C-level name NAME.
   If NAME starts with a *, the rest of NAME is output verbatim.
   Otherwise NAME is transformed in an implementation-defined way
   (usually by the addition of an underscore).
   Many macros in the tm file are defined to call this function.  */

void
assemble_name (file, name)
     FILE *file;
     char *name;
{
  if (name[0] == '*')
    fputs (&name[1], file);
  else
    ASM_OUTPUT_LABELREF (file, name);
}

/* Allocate SIZE bytes writable static space with a gensym name
   and return an RTX to refer to its address.  */

rtx
assemble_static_space (size)
     int size;
{
  char name[12];
  char *namestring;
  rtx x;
  /* Round size up to multiple of BIGGEST_ALIGNMENT bits
     so that each uninitialized object starts on such a boundary.  */
  int rounded = ((size + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
		 / (BIGGEST_ALIGNMENT / BITS_PER_UNIT)
		 * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  if (flag_shared_data)
    data_section ();
  ASM_GENERATE_INTERNAL_LABEL (name, "LF", const_labelno);
  ++const_labelno;

  namestring = (char *) obstack_alloc (saveable_obstack,
				       strlen (name) + 2);
  strcpy (namestring, name);

  x = gen_rtx (SYMBOL_REF, Pmode, namestring);
  ASM_OUTPUT_LOCAL (asm_out_file, name, size, rounded);
  return x;
}

/* Here we combine duplicate floating constants to make
   CONST_DOUBLE rtx's, and force those out to memory when necessary.  */

/* Chain of all CONST_DOUBLE rtx's constructed for the current function.
   They are chained through the CONST_DOUBLE_CHAIN.
   A CONST_DOUBLE rtx has CONST_DOUBLE_MEM != cc0_rtx iff it is on this chain.
   In that case, CONST_DOUBLE_MEM is either a MEM,
   or const0_rtx if no MEM has been made for this CONST_DOUBLE yet.  */

static rtx real_constant_chain;

/* Return a CONST_DOUBLE for a value specified as a pair of ints.
   For an integer, I0 is the low-order word and I1 is the high-order word.
   For a real number, I0 is the word with the low address
   and I1 is the word with the high address.  */

rtx
immed_double_const (i0, i1, mode)
     int i0, i1;
     enum machine_mode mode;
{
  register rtx r;

  if (mode == DImode && i0 == 0 && i1 == 0)
    return const0_rtx;

  /* Search the chain for an existing CONST_DOUBLE with the right value.
     If one is found, return it.  */

  for (r = real_constant_chain; r; r = CONST_DOUBLE_CHAIN (r))
    if (CONST_DOUBLE_LOW (r) == i0 && CONST_DOUBLE_HIGH (r) == i1
	&& GET_MODE (r) == mode)
      return r;

  /* No; make a new one and add it to the chain.  */

  r = gen_rtx (CONST_DOUBLE, mode, 0, i0, i1);

  CONST_DOUBLE_CHAIN (r) = real_constant_chain;
  real_constant_chain = r;

  /* Store const0_rtx in mem-slot since this CONST_DOUBLE is on the chain.
     Actual use of mem-slot is only through force_const_double_mem.  */

  CONST_DOUBLE_MEM (r) = const0_rtx;

  return r;
}

/* Return a CONST_DOUBLE for a specified `double' value
   and machine mode.  */

rtx
immed_real_const_1 (d, mode)
     REAL_VALUE_TYPE d;
     enum machine_mode mode;
{
  union real_extract u;
  register rtx r;

  /* Get the desired `double' value as a sequence of ints
     since that is how they are stored in a CONST_DOUBLE.  */

  u.d = d;

  /* Detect zero.  */

  if (! bcmp (&CONST_DOUBLE_LOW (dconst0_rtx), &u, sizeof u))
    return (mode == DFmode ? dconst0_rtx : fconst0_rtx);

  if (sizeof u == 2 * sizeof (int))
    return immed_double_const (u.i[0], u.i[1], mode);

  /* The rest of this function handles the case where
     a float value requires more than 2 ints of space.
     It will be deleted as dead code on machines that don't need it.  */

  /* Search the chain for an existing CONST_DOUBLE with the right value.
     If one is found, return it.  */

  for (r = real_constant_chain; r; r = CONST_DOUBLE_CHAIN (r))
    if (! bcmp (&CONST_DOUBLE_LOW (r), &u, sizeof u)
	&& GET_MODE (r) == mode)
      return r;

  /* No; make a new one and add it to the chain.  */

  r = rtx_alloc (CONST_DOUBLE);
  PUT_MODE (r, mode);
  bcopy (&u, &CONST_DOUBLE_LOW (r), sizeof u);

  CONST_DOUBLE_CHAIN (r) = real_constant_chain;
  real_constant_chain = r;

  /* Store const0_rtx in slot 2 since this CONST_DOUBLE is on the chain.
     Actual use of slot 2 is only through force_const_double_mem.  */

  CONST_DOUBLE_MEM (r) = const0_rtx;

  return r;
}

/* Return a CONST_DOUBLE rtx for a value specified by EXP,
   which must be a REAL_CST tree node.  */

rtx
immed_real_const (exp)
     tree exp;
{
  return immed_real_const_1 (TREE_REAL_CST (exp), TYPE_MODE (TREE_TYPE (exp)));
}

/* Given a CONST_DOUBLE, cause a constant in memory to be created
   (unless we already have one for the same value)
   and return a MEM rtx to refer to it.
   Put the CONST_DOUBLE on real_constant_chain if it isn't already there.  */

rtx
force_const_double_mem (r)
     rtx r;
{
  if (CONST_DOUBLE_MEM (r) == cc0_rtx)
    {
      CONST_DOUBLE_CHAIN (r) = real_constant_chain;
      real_constant_chain = r;
      CONST_DOUBLE_MEM (r) = const0_rtx;
    }

  if (CONST_DOUBLE_MEM (r) == const0_rtx)
    {
      CONST_DOUBLE_MEM (r) = force_const_mem (GET_MODE (r), r);
    }
  /* CONST_DOUBLE_MEM (r) is now a MEM with a constant address.
     If that is legitimate, return it.
     Othewise it will need reloading, so return a copy of it.  */
  if (memory_address_p (GET_MODE (r), XEXP (CONST_DOUBLE_MEM (r), 0)))
    return CONST_DOUBLE_MEM (r);
  return gen_rtx (MEM, GET_MODE (r), XEXP (CONST_DOUBLE_MEM (r), 0));
}

/* At the end of a function, forget the memory-constants
   previously made for CONST_DOUBLEs.  Mark them as not on real_constant_chain.
   Also clear out real_constant_chain and clear out all the chain-pointers.  */

void
clear_const_double_mem ()
{
  register rtx r, next;

  for (r = real_constant_chain; r; r = next)
    {
      next = CONST_DOUBLE_CHAIN (r);
      CONST_DOUBLE_CHAIN (r) = 0;
      CONST_DOUBLE_MEM (r) = cc0_rtx;
    }
  real_constant_chain = 0;
}

/* Given an expression EXP with a constant value,
   reduce it to the sum of an assembler symbol and an integer.
   Store them both in the structure *VALUE.
   Abort if EXP does not reduce.  */

struct addr_const
{
  rtx base;
  int offset;
};

static void
decode_addr_const (exp, value)
     tree exp;
     struct addr_const *value;
{
  register tree target = TREE_OPERAND (exp, 0);
  register int offset = 0;
  register rtx x;

  while (1)
    {
      if (TREE_CODE (target) == COMPONENT_REF)
	{
	  offset += DECL_OFFSET (TREE_OPERAND (target, 1)) / BITS_PER_UNIT;
	  target = TREE_OPERAND (target, 0);
	}
      else if (TREE_CODE (target) == ARRAY_REF)
	{
	  if (TREE_CODE (TREE_OPERAND (target, 1)) != INTEGER_CST
	      || TREE_CODE (TYPE_SIZE (TREE_TYPE (target))) != INTEGER_CST)
	    abort ();
	  offset += ((TYPE_SIZE_UNIT (TREE_TYPE (target))
		      * TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (target)))
		      * TREE_INT_CST_LOW (TREE_OPERAND (target, 1)))
		     / BITS_PER_UNIT);
	  target = TREE_OPERAND (target, 0);
	}
      else break;
    }

  if (TREE_CODE (target) == VAR_DECL
      || TREE_CODE (target) == FUNCTION_DECL)
    x = DECL_RTL (target);
  else if (TREE_LITERAL (target))
    x = TREE_CST_RTL (target);
  else
    abort ();

  if (GET_CODE (x) != MEM)
    abort ();
  x = XEXP (x, 0);

  value->base = x;
  value->offset = offset;
}

/* Uniquize all constants that appear in memory.
   Each constant in memory thus far output is recorded
   in `const_hash_table' with a `struct constant_descriptor'
   that contains a polish representation of the value of
   the constant.

   We cannot store the trees in the hash table
   because the trees may be temporary.  */

struct constant_descriptor
{
  struct constant_descriptor *next;
  char *label;
  char contents[1];
};

#define HASHBITS 30
#define MAX_HASH_TABLE 1007
static struct constant_descriptor *const_hash_table[MAX_HASH_TABLE];

/* Compute a hash code for a constant expression.  */

int
const_hash (exp)
     tree exp;
{
  register char *p;
  register int len, hi, i;
  register enum tree_code code = TREE_CODE (exp);

  if (code == INTEGER_CST)
    {
      p = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      p = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    p = TREE_STRING_POINTER (exp), len = TREE_STRING_LENGTH (exp);
  else if (code == COMPLEX_CST)
    return const_hash (TREE_REALPART (exp)) * 5
      + const_hash (TREE_IMAGPART (exp));
  else if (code == CONSTRUCTOR)
    {
      register tree link;

      /* For record type, include the type in the hashing.
	 We do not do so for array types
	 because (1) the sizes of the elements are sufficient
	 and (2) distinct array types can have the same constructor.  */
      if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	hi = ((int) TREE_TYPE (exp) & ((1 << HASHBITS) - 1)) % MAX_HASH_TABLE;
      else
	hi = 5;

      for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	hi = (hi * 603 + const_hash (TREE_VALUE (link))) % MAX_HASH_TABLE;

      return hi;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      p = (char *) &value;
      len = sizeof value;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    return const_hash (TREE_OPERAND (exp, 0)) * 9
      +  const_hash (TREE_OPERAND (exp, 1));
  else if (code == NOP_EXPR || code == CONVERT_EXPR)
    return const_hash (TREE_OPERAND (exp, 0)) * 7 + 2;

  /* Compute hashing function */
  hi = len;
  for (i = 0; i < len; i++)
    hi = ((hi * 613) + (unsigned)(p[i]));

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_HASH_TABLE;
  return hi;
}

/* Compare a constant expression EXP with a constant-descriptor DESC.
   Return 1 if DESC describes a constant with the same value as EXP.  */

static int
compare_constant (exp, desc)
     tree exp;
     struct constant_descriptor *desc;
{
  return 0 != compare_constant_1 (exp, desc->contents);
}

/* Compare constant expression EXP with a substring P of a constant descriptor.
   If they match, return a pointer to the end of the substring matched.
   If they do not match, return 0.

   Since descriptors are written in polish prefix notation,
   this function can be used recursively to test one operand of EXP
   against a subdescriptor, and if it succeeds it returns the
   address of the subdescriptor for the next operand.  */

static char *
compare_constant_1 (exp, p)
     tree exp;
     char *p;
{
  register char *strp;
  register int len;
  register enum tree_code code = TREE_CODE (exp);

  if (code != (enum tree_code) *p++)
    return 0;

  if (code == INTEGER_CST)
    {
      /* Integer constants are the same only if the same width of type.  */
      if (*p++ != TYPE_PRECISION (TREE_TYPE (exp)))
	return 0;
      strp = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      /* Real constants are the same only if the same width of type.  */
      if (*p++ != TYPE_PRECISION (TREE_TYPE (exp)))
	return 0;
      strp = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    {
      if (flag_writable_strings)
	return 0;
      strp = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      if (bcmp (&TREE_STRING_LENGTH (exp), p,
		sizeof TREE_STRING_LENGTH (exp)))
	return 0;
      p += sizeof TREE_STRING_LENGTH (exp);
    }
  else if (code == COMPLEX_CST)
    {
      p = compare_constant_1 (TREE_REALPART (exp), p);
      if (p == 0) return 0;
      p = compare_constant_1 (TREE_IMAGPART (exp), p);
      return p;
    }
  else if (code == CONSTRUCTOR)
    {
      register tree link;
      int length = list_length (CONSTRUCTOR_ELTS (exp));
      tree type;

      if (bcmp (&length, p, sizeof length))
	return 0;
      p += sizeof length;

      /* For record constructors, insist that the types match.
	 For arrays, just verify both constructors are for arrays.  */
      if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	type = TREE_TYPE (exp);
      else
	type = 0;
      if (bcmp (&type, p, sizeof type))
	return 0;
      p += sizeof type;

      for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	if ((p = compare_constant_1 (TREE_VALUE (link), p)) == 0)
	  return 0;
      return p;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      strp = (char *) &value;
      len = sizeof value;
      /* Compare SYMBOL_REF address and offset.  */
      while (--len >= 0)
	if (*p++ != *strp++)
	  return 0;
      /* Compare symbol name.  */
      strp = XSTR (value.base, 0);
      len = strlen (strp) + 1;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    {
      if (*p++ != (char) code)
	return 0;
      p = compare_constant_1 (TREE_OPERAND (exp, 0), p);
      if (p == 0) return 0;
      p = compare_constant_1 (TREE_OPERAND (exp, 1), p);
      return p;
    }
  else if (code == NOP_EXPR || code == CONVERT_EXPR)
    {
      if (*p++ != (char) code)
	return 0;
      p = compare_constant_1 (TREE_OPERAND (exp, 0), p);
      return p;
    }

  /* Compare constant contents.  */
  while (--len >= 0)
    if (*p++ != *strp++)
      return 0;

  return p;
}

/* Construct a constant descriptor for the expression EXP.
   It is up to the caller to enter the descriptor in the hash table.  */

static struct constant_descriptor *
record_constant (exp)
     tree exp;
{
  struct constant_descriptor *ptr = 0;
  int buf;

  obstack_grow (&permanent_obstack, &ptr, sizeof ptr);
  obstack_grow (&permanent_obstack, &buf, sizeof buf);
  record_constant_1 (exp);
  return (struct constant_descriptor *) obstack_finish (&permanent_obstack);
}

/* Add a description of constant expression EXP
   to the object growing in `permanent_obstack'.
   No need to return its address; the caller will get that
   from the obstack when the object is complete.  */

static void
record_constant_1 (exp)
     tree exp;
{
  register char *strp;
  register int len;
  register enum tree_code code = TREE_CODE (exp);

  obstack_1grow (&permanent_obstack, (unsigned int) code);

  if (code == INTEGER_CST)
    {
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (char *) &TREE_INT_CST_LOW (exp);
      len = 2 * sizeof TREE_INT_CST_LOW (exp);
    }
  else if (code == REAL_CST)
    {
      obstack_1grow (&permanent_obstack, TYPE_PRECISION (TREE_TYPE (exp)));
      strp = (char *) &TREE_REAL_CST (exp);
      len = sizeof TREE_REAL_CST (exp);
    }
  else if (code == STRING_CST)
    {
      if (flag_writable_strings)
	return;
      strp = TREE_STRING_POINTER (exp);
      len = TREE_STRING_LENGTH (exp);
      obstack_grow (&permanent_obstack, (char *) &TREE_STRING_LENGTH (exp),
		    sizeof TREE_STRING_LENGTH (exp));
    }
  else if (code == COMPLEX_CST)
    {
      record_constant_1 (TREE_REALPART (exp));
      record_constant_1 (TREE_IMAGPART (exp));
      return;
    }
  else if (code == CONSTRUCTOR)
    {
      register tree link;
      int length = list_length (CONSTRUCTOR_ELTS (exp));
      tree type;

      obstack_grow (&permanent_obstack, (char *) &length, sizeof length);

      /* For record constructors, insist that the types match.
	 For arrays, just verify both constructors are for arrays.  */
      if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
	type = TREE_TYPE (exp);
      else
	type = 0;
      obstack_grow (&permanent_obstack, (char *) &type, sizeof type);

      for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	record_constant_1 (TREE_VALUE (link));
      return;
    }
  else if (code == ADDR_EXPR)
    {
      struct addr_const value;
      decode_addr_const (exp, &value);
      /* Record the SYMBOL_REF address and the offset.  */
      obstack_grow (&permanent_obstack, (char *) &value, sizeof value);
      /* Record the symbol name.  */
      obstack_grow (&permanent_obstack, XSTR (value.base, 0),
		    strlen (XSTR (value.base, 0)) + 1);
      return;
    }
  else if (code == PLUS_EXPR || code == MINUS_EXPR)
    {
      obstack_1grow (&permanent_obstack, (int) code);
      record_constant_1 (TREE_OPERAND (exp, 0));
      record_constant_1 (TREE_OPERAND (exp, 1));
      return;
    }
  else if (code == NOP_EXPR || code == CONVERT_EXPR)
    {
      obstack_1grow (&permanent_obstack, (int) code);
      record_constant_1 (TREE_OPERAND (exp, 0));
      return;
    }

  /* Record constant contents.  */
  obstack_grow (&permanent_obstack, strp, len);
}

/* Return the constant-label-string for constant value EXP.
   If no constant equal to EXP has yet been output,
   define a new label and output assembler code for it.
   The const_hash_table records which constants already have label strings.  */

static char *
get_or_assign_label (exp)
     tree exp;
{
  register int hash, i, align;
  register struct constant_descriptor *desc;
  char label[256];

  /* Make sure any other constants whose addresses appear in EXP
     are assigned label numbers.  */

  output_addressed_constants (exp);

  /* Compute hash code of EXP.  Search the descriptors for that hash code
     to see if any of them describes EXP.  If yes, the descriptor records
     the label number already assigned.  */

  hash = const_hash (exp) % MAX_HASH_TABLE;

  for (desc = const_hash_table[hash]; desc; desc = desc->next)
    if (compare_constant (exp, desc))
      return desc->label;

  /* No constant equal to EXP is known to have been output.
     Make a constant descriptor to enter EXP in the hash table.
     Assign the label number and record it in the descriptor for
     future calls to this function to find.  */

  desc = record_constant (exp);
  desc->next = const_hash_table[hash];
  const_hash_table[hash] = desc;

  /* Now output assembler code to define that label
     and follow it with the data of EXP.  */

  /* First switch to text section, except for writable strings.  */
#ifdef SELECT_SECTION
  SELECT_SECTION (exp);
#else
  if ((TREE_CODE (exp) == STRING_CST) && flag_writable_strings)
    data_section ();
  else
    text_section ();
#endif

  /* Align the location counter as required by EXP's data type.  */
#ifdef CONSTANT_ALIGNMENT
  align = CONSTANT_ALIGNMENT (TREE_CODE (exp), TYPE_ALIGN (TREE_TYPE (exp)));
#else
  align = TYPE_ALIGN (TREE_TYPE (exp));
#endif

  for (i = 0; align >= BITS_PER_UNIT << (i + 1); i++);
  if (i > 0)
    ASM_OUTPUT_ALIGN (asm_out_file, i);

  /* Output the label itself.  */
  ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LC", const_labelno);

  /* Output the value of EXP.  */
  output_constant (exp,
		   (TREE_CODE (exp) == STRING_CST
		    ? TREE_STRING_LENGTH (exp)
		    : int_size_in_bytes (TREE_TYPE (exp))));

  /* Create a string containing the label name, in LABEL.  */
  ASM_GENERATE_INTERNAL_LABEL (label, "LC", const_labelno);

  ++const_labelno;

  desc->label
    = (char *) obstack_copy0 (&permanent_obstack, label, strlen (label));

  return desc->label;
}

/* Return an rtx representing a reference to constant data in memory
   for the constant expression EXP.
   If assembler code for such a constant has already been output,
   return an rtx to refer to it.
   Otherwise, output such a constant in memory and generate
   an rtx for it.  The TREE_CST_RTL of EXP is set up to point to that rtx.  */

rtx
output_constant_def (exp)
     tree exp;
{
  register rtx def;
  int temp_p = allocation_temporary_p ();

  if (TREE_CODE (exp) == INTEGER_CST)
    abort ();			/* No TREE_CST_RTL slot in these.  */

  if (TREE_CST_RTL (exp))
    return TREE_CST_RTL (exp);

  if (TREE_PERMANENT (exp))
    end_temporary_allocation ();

  def = gen_rtx (SYMBOL_REF, Pmode, get_or_assign_label (exp));

  TREE_CST_RTL (exp)
    = gen_rtx (MEM, TYPE_MODE (TREE_TYPE (exp)), def);
  RTX_UNCHANGING_P (TREE_CST_RTL (exp)) = 1;

  if (temp_p && TREE_PERMANENT (exp))
    resume_temporary_allocation ();

  return TREE_CST_RTL (exp);
}

/* Similar hash facility for making memory-constants
   from constant rtl-expressions.  It is used on RISC machines
   where immediate integer arguments and constant addresses are restricted
   so that such constants must be stored in memory.

   This pool of constants is reinitialized for each function
   so each function gets its own constants-pool that comes right before it.  */

#define MAX_RTX_HASH_TABLE 61
static struct constant_descriptor *const_rtx_hash_table[MAX_RTX_HASH_TABLE];

void
init_const_rtx_hash_table ()
{
  bzero (const_rtx_hash_table, sizeof const_rtx_hash_table);
}

struct rtx_const
{
  enum kind { RTX_DOUBLE, RTX_INT } kind : 16;
  enum machine_mode mode : 16;
  union {
    union real_extract du;
    struct addr_const addr;
  } un;
};

/* Express an rtx for a constant integer (perhaps symbolic)
   as the sum of a symbol or label plus an explicit integer.
   They are stored into VALUE.  */

static void
decode_rtx_const (mode, x, value)
     enum machine_mode mode;
     rtx x;
     struct rtx_const *value;
{
  /* Clear the whole structure, including any gaps.  */

  {
    int *p = (int *) value;
    int *end = (int *) (value + 1);
    while (p < end)
      *p++ = 0;
  }

  value->kind = RTX_INT;	/* Most usual kind. */
  value->mode = mode;

  switch (GET_CODE (x))
    {
    case CONST_DOUBLE:
      value->kind = RTX_DOUBLE;
      value->mode = GET_MODE (x);
      bcopy (&CONST_DOUBLE_LOW (x), &value->un.du, sizeof value->un.du);
      break;

    case CONST_INT:
      value->un.addr.offset = INTVAL (x);
      break;

    case SYMBOL_REF:
      value->un.addr.base = x;
      break;

    case LABEL_REF:
      value->un.addr.base = x;
      break;

    case CONST:
      x = XEXP (x, 0);
      if (GET_CODE (x) == PLUS)
	{
	  value->un.addr.base = XEXP (XEXP (x, 0), 0);
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    abort ();
	  value->un.addr.offset = INTVAL (XEXP (x, 1));
	}
      else if (GET_CODE (x) == MINUS)
	{
	  value->un.addr.base = XEXP (x, 0);
	  if (GET_CODE (XEXP (x, 1)) != CONST_INT)
	    abort ();
	  value->un.addr.offset = - INTVAL (XEXP (x, 1));
	}
      else
	abort ();
      break;

    default:
      abort ();
    }

  if (value->kind == RTX_INT && value->un.addr.base != 0)
    switch (GET_CODE (value->un.addr.base))
      {
      case SYMBOL_REF:
      case LABEL_REF:
	/* Use the string's address, not the SYMBOL_REF's address,
	   for the sake of addresses of library routines.
	   For a LABEL_REF, compare labels.  */
	value->un.addr.base = XEXP (value->un.addr.base, 0);
      }
}

/* Compute a hash code for a constant RTL expression.  */

int
const_hash_rtx (mode, x)
     enum machine_mode mode;
     rtx x;
{
  register int hi, i;

  struct rtx_const value;
  decode_rtx_const (mode, x, &value);

  /* Compute hashing function */
  hi = 0;
  for (i = 0; i < sizeof value / sizeof (int); i++)
    hi += ((int *) &value)[i];

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_RTX_HASH_TABLE;
  return hi;
}

/* Compare a constant rtl object X with a constant-descriptor DESC.
   Return 1 if DESC describes a constant with the same value as X.  */

static int
compare_constant_rtx (mode, x, desc)
     enum machine_mode mode;
     rtx x;
     struct constant_descriptor *desc;
{
  register int *p = (int *) desc->contents;
  register int *strp;
  register int len;
  struct rtx_const value;

  decode_rtx_const (mode, x, &value);
  strp = (int *) &value;
  len = sizeof value / sizeof (int);

  /* Compare constant contents.  */
  while (--len >= 0)
    if (*p++ != *strp++)
      return 0;

  return 1;
}

/* Construct a constant descriptor for the rtl-expression X.
   It is up to the caller to enter the descriptor in the hash table.  */

static struct constant_descriptor *
record_constant_rtx (mode, x)
     enum machine_mode mode;
     rtx x;
{
  struct constant_descriptor *ptr = 0;
  int buf;
  struct rtx_const value;

  decode_rtx_const (mode, x, &value);

  obstack_grow (saveable_obstack, &ptr, sizeof ptr);
  obstack_grow (saveable_obstack, &buf, sizeof buf);

  /* Record constant contents.  */
  obstack_grow (saveable_obstack, &value, sizeof value);

  return (struct constant_descriptor *) obstack_finish (saveable_obstack);
}

/* Given a constant rtx X, make (or find) a memory constant for its value
   and return a MEM rtx to refer to it in memory.  */

rtx
force_const_mem (mode, x)
     enum machine_mode mode;
     rtx x;
{
  register int hash;
  register struct constant_descriptor *desc;
  char label[256];
  char *found = 0;
  rtx def;

  if (GET_CODE (x) == CONST_DOUBLE
      && GET_CODE (CONST_DOUBLE_MEM (x)) == MEM)
    return CONST_DOUBLE_MEM (x);

  /* Compute hash code of X.  Search the descriptors for that hash code
     to see if any of them describes X.  If yes, the descriptor records
     the label number already assigned.  */

  hash = const_hash_rtx (mode, x);

  for (desc = const_rtx_hash_table[hash]; desc; desc = desc->next)
    if (compare_constant_rtx (mode, x, desc))
      {
	found = desc->label;
	break;
      }

  if (found == 0)
    {
      int align;

      /* No constant equal to X is known to have been output.
	 Make a constant descriptor to enter X in the hash table.
	 Assign the label number and record it in the descriptor for
	 future calls to this function to find.  */

      desc = record_constant_rtx (mode, x);
      desc->next = const_rtx_hash_table[hash];
      const_rtx_hash_table[hash] = desc;

      /* Now output assembler code to define that label
	 and follow it with the data of EXP.  */

      /* First switch to text section.  */
#ifdef SELECT_RTX_SECTION
      SELECT_RTX_SECTION (mode, x);
#else
      text_section ();
#endif

      /* Align the location counter as required by EXP's data type.  */
      align = (mode == VOIDmode) ? UNITS_PER_WORD : GET_MODE_SIZE (mode);
      if (align > BIGGEST_ALIGNMENT / BITS_PER_UNIT)
	align = BIGGEST_ALIGNMENT / BITS_PER_UNIT;

      if (align > 1)
	ASM_OUTPUT_ALIGN (asm_out_file, exact_log2 (align));

      /* Output the label itself.  */
      ASM_OUTPUT_INTERNAL_LABEL (asm_out_file, "LC", const_labelno);

      /* Output the value of EXP.  */
      if (GET_CODE (x) == CONST_DOUBLE)
	{
	  union real_extract u;

	  bcopy (&CONST_DOUBLE_LOW (x), &u, sizeof u);
	  switch (mode)
	    {
	      /* Perhaps change the following to use
		 CONST_DOUBLE_LOW and CONST_DOUBLE_HIGH, rather than u.i.  */
	    case DImode:
#ifdef ASM_OUTPUT_DOUBLE_INT
	      ASM_OUTPUT_DOUBLE_INT (asm_out_file, x);
#else /* no ASM_OUTPUT_DOUBLE_INT */
#ifndef WORDS_BIG_ENDIAN
	      /* Output two ints.  */
	      ASM_OUTPUT_INT (asm_out_file,
			      gen_rtx (CONST_INT, VOIDmode, u.i[0]));
	      ASM_OUTPUT_INT (asm_out_file,
			      gen_rtx (CONST_INT, VOIDmode, u.i[1]));
#else
	      /* Output two ints.  */
	      ASM_OUTPUT_INT (asm_out_file,
			      gen_rtx (CONST_INT, VOIDmode, u.i[1]));
	      ASM_OUTPUT_INT (asm_out_file,
			      gen_rtx (CONST_INT, VOIDmode, u.i[0]));
#endif /* WORDS_BIG_ENDIAN */
#endif /* no ASM_OUTPUT_DOUBLE_INT */
	      break;

	    case DFmode:
	      ASM_OUTPUT_DOUBLE (asm_out_file, u.d);
	      break;

	    case SFmode:
	      ASM_OUTPUT_FLOAT (asm_out_file, u.d);
	    }
	}
      else
	switch (mode)
	  {
	  case SImode:
	    ASM_OUTPUT_INT (asm_out_file, x);
	    break;

	  case HImode:
	    ASM_OUTPUT_SHORT (asm_out_file, x);
	    break;

	  case QImode:
	    ASM_OUTPUT_CHAR (asm_out_file, x);
	    break;
	  }

      /* Create a string containing the label name, in LABEL.  */
      ASM_GENERATE_INTERNAL_LABEL (label, "LC", const_labelno);

      ++const_labelno;

      desc->label = found
	= (char *) obstack_copy0 (&permanent_obstack, label, strlen (label));
    }

  /* We have a symbol name; construct the SYMBOL_REF and the MEM.  */

  def = gen_rtx (MEM, mode, gen_rtx (SYMBOL_REF, Pmode, desc->label));

  RTX_UNCHANGING_P (def) = 1;
  /* Mark the symbol_ref as belonging to this constants pool.  */
  CONSTANT_POOL_ADDRESS_P (XEXP (def, 0)) = 1;

  if (GET_CODE (x) == CONST_DOUBLE)
    {
      if (CONST_DOUBLE_MEM (x) == cc0_rtx)
	{
	  CONST_DOUBLE_CHAIN (x) = real_constant_chain;
	  real_constant_chain = x;
	}
      CONST_DOUBLE_MEM (x) = def;
    }

  return def;
}

/* Find all the constants whose addresses are referenced inside of EXP,
   and make sure assembler code with a label has been output for each one.  */

void
output_addressed_constants (exp)
     tree exp;
{
  switch (TREE_CODE (exp))
    {
    case ADDR_EXPR:
      {
	register tree constant = TREE_OPERAND (exp, 0);

	while (TREE_CODE (constant) == COMPONENT_REF)
	  {
	    constant = TREE_OPERAND (constant, 0);
	  }

	if (TREE_LITERAL (constant))
	  /* No need to do anything here
	     for addresses of variables or functions.  */
	  output_constant_def (constant);
      }
      break;

    case PLUS_EXPR:
    case MINUS_EXPR:
      output_addressed_constants (TREE_OPERAND (exp, 0));
      output_addressed_constants (TREE_OPERAND (exp, 1));
      break;

    case NOP_EXPR:
    case CONVERT_EXPR:
      output_addressed_constants (TREE_OPERAND (exp, 0));
      break;

    case CONSTRUCTOR:
      {
	register tree link;
	for (link = CONSTRUCTOR_ELTS (exp); link; link = TREE_CHAIN (link))
	  output_addressed_constants (TREE_VALUE (link));
      }
      break;

    case ERROR_MARK:
      break;

    default:
      if (! TREE_LITERAL (exp))
	abort ();
    }
}

/* Output assembler code for constant EXP to FILE, with no label.
   This includes the pseudo-op such as ".int" or ".byte", and a newline.
   Assumes output_addressed_constants has been done on EXP already.

   Generate exactly SIZE bytes of assembler data, padding at the end
   with zeros if necessary.  SIZE must always be specified.

   SIZE is important for structure constructors,
   since trailing members may have been omitted from the constructor.
   It is also important for initialization of arrays from string constants
   since the full length of the string constant might not be wanted.
   It is also needed for initialization of unions, where the initializer's
   type is just one member, and that may not be as long as the union.

   There a case in which we would fail to output exactly SIZE bytes:
   for a structure constructor that wants to produce more than SIZE bytes.
   But such constructors will never be generated for any possible input.  */

void
output_constant (exp, size)
     register tree exp;
     register int size;
{
  register enum tree_code code = TREE_CODE (TREE_TYPE (exp));
  rtx x;

  if (size == 0)
    return;

  /* Eliminate the NOP_EXPR that makes a cast not be an lvalue.
     That way we get the constant (we hope) inside it.  */
  if (TREE_CODE (exp) == NOP_EXPR
      && TREE_TYPE (exp) == TREE_TYPE (TREE_OPERAND (exp, 0)))
    exp = TREE_OPERAND (exp, 0);

  switch (code)
    {
    case INTEGER_TYPE:
    case ENUMERAL_TYPE:
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* ??? What about       (int)((float)(int)&foo + 4)    */
      while (TREE_CODE (exp) == NOP_EXPR || TREE_CODE (exp) == CONVERT_EXPR)
	exp = TREE_OPERAND (exp, 0);

#ifndef ASM_OUTPUT_DOUBLE_INT
      if (TYPE_MODE (TREE_TYPE (exp)) == DImode)
	{
	  if (TREE_CODE (exp) == INTEGER_CST)
	    {
#ifndef WORDS_BIG_ENDIAN
	      ASM_OUTPUT_INT (asm_out_file,
			      gen_rtx (CONST_INT, VOIDmode,
				       TREE_INT_CST_LOW (exp)));
	      ASM_OUTPUT_INT (asm_out_file,
			      gen_rtx (CONST_INT, VOIDmode,
				       TREE_INT_CST_HIGH (exp)));
#else
	      ASM_OUTPUT_INT (asm_out_file,
			      gen_rtx (CONST_INT, VOIDmode,
				       TREE_INT_CST_HIGH (exp)));
	      ASM_OUTPUT_INT (asm_out_file,
			      gen_rtx (CONST_INT, VOIDmode,
				       TREE_INT_CST_LOW (exp)));
#endif
	      size -= 8;
	      break;
	    }
	  else
	    error ("8-byte integer constant expression too complicated");

	  break;
	}
#endif /* no ASM_OUTPUT_DOUBLE_INT */

      x = expand_expr (exp, 0, VOIDmode, EXPAND_SUM);

      if (size == 1)
	{
	  ASM_OUTPUT_CHAR (asm_out_file, x);
	  size -= 1;
	}
      else if (size == 2)
	{
	  ASM_OUTPUT_SHORT (asm_out_file, x);
	  size -= 2;
	}
      else if (size == 4)
	{
	  ASM_OUTPUT_INT (asm_out_file, x);
	  size -= 4;
	}
#ifdef ASM_OUTPUT_DOUBLE_INT
      else if (size == 8)
	{
	  ASM_OUTPUT_DOUBLE_INT (asm_out_file, x);
	  size -= 8;
	}
#endif /* ASM_OUTPUT_DOUBLE_INT */
      else
	abort ();

      break;

    case REAL_TYPE:
      if (TREE_CODE (exp) != REAL_CST)
	error ("initializer for floating value is not a floating constant");
      else
	{
	  REAL_VALUE_TYPE d;
	  jmp_buf output_constant_handler;

	  d = TREE_REAL_CST (exp);
	  if (setjmp (output_constant_handler))
	    {
	      error ("floating point trap outputting a constant");
#ifdef REAL_IS_NOT_DOUBLE
	      bzero (&d, sizeof d);
	      d = REAL_VALUE_ATOF ("0");
#else
	      d = 0;
#endif
	    }
	  set_float_handler (output_constant_handler);

	  if (size < 4)
	    break;
	  else if (size < 8)
	    {
	      ASM_OUTPUT_FLOAT (asm_out_file, d);
	      size -= 4;
	    }
	  else
	    {
	      ASM_OUTPUT_DOUBLE (asm_out_file, d);
	      size -= 8;
	    }
	  set_float_handler (0);
	}
      break;

    case COMPLEX_TYPE:
      output_constant (TREE_REALPART (exp), size / 2);
      output_constant (TREE_IMAGPART (exp), size / 2);
      size -= (size / 2) * 2;
      break;

    case ARRAY_TYPE:
      if (TREE_CODE (exp) == CONSTRUCTOR)
	{
	  output_constructor (exp, size);
	  return;
	}
      else if (TREE_CODE (exp) == STRING_CST)
	{
	  int excess = 0;

	  if (size > TREE_STRING_LENGTH (exp))
	    {
	      excess = size - TREE_STRING_LENGTH (exp);
	      size = TREE_STRING_LENGTH (exp);
	    }

	  assemble_string (TREE_STRING_POINTER (exp), size);
	  size = excess;
	}
      else
	abort ();
      break;

    case RECORD_TYPE:
    case UNION_TYPE:
      if (TREE_CODE (exp) == CONSTRUCTOR)
	output_constructor (exp, size);
      else
	abort ();
      return;
    }

  if (size > 0)
    ASM_OUTPUT_SKIP (asm_out_file, size);
}

/* Subroutine of output_constant, used for CONSTRUCTORs
   (aggregate constants).
   Generate at least SIZE bytes, padding if necessary.  */

void
output_constructor (exp, size)
     tree exp;
     int size;
{
  register tree link, field = 0;
  /* Number of bytes output or skipped so far.
     In other words, current position within the constructor.  */
  int total_bytes = 0;
  /* Non-zero means BYTE contains part of a byte, to be output.  */
  int byte_buffer_in_use = 0;
  register int byte;

  if (HOST_BITS_PER_INT < BITS_PER_UNIT)
    abort ();

  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
      || TREE_CODE (TREE_TYPE (exp)) == UNION_TYPE)
    field = TYPE_FIELDS (TREE_TYPE (exp));

  /* As LINK goes through the elements of the constant,
     FIELD goes through the structure fields, if the constant is a structure.
     But the constant could also be an array.  Then FIELD is zero.  */
  for (link = CONSTRUCTOR_ELTS (exp);
       link;
       link = TREE_CHAIN (link),
       field = field ? TREE_CHAIN (field) : 0)
    {
      tree val = TREE_VALUE (link);

      /* Eliminate the NOP_EXPR that makes a cast not be an lvalue.  */
      if (TREE_CODE (val) == NOP_EXPR
	  && TREE_TYPE (val) == TREE_TYPE (TREE_OPERAND (val, 0)))
	val = TREE_OPERAND (val, 0);

      if (field == 0
	  || (DECL_MODE (field) != BImode))
	{
	  register int fieldsize;

	  /* An element that is not a bit-field.
	     Output any buffered-up bit-fields preceding it.  */
	  if (byte_buffer_in_use)
	    {
	      ASM_OUTPUT_BYTE (asm_out_file, byte);
	      total_bytes++;
	      byte_buffer_in_use = 0;
	    }

	  /* Advance to offset of this element.
	     Note no alignment needed in an array, since that is guaranteed
	     if each element has the proper size.  */
	  if (field != 0 && DECL_OFFSET (field) / BITS_PER_UNIT != total_bytes)
	    {
	      ASM_OUTPUT_SKIP (asm_out_file,
			       (DECL_OFFSET (field) / BITS_PER_UNIT
				- total_bytes));
	      total_bytes = DECL_OFFSET (field) / BITS_PER_UNIT;
	    }

	  /* Determine size this element should occupy.  */
	  if (field)
	    {
	      if (! TREE_LITERAL (DECL_SIZE (field)))
		abort ();
	      fieldsize = TREE_INT_CST_LOW (DECL_SIZE (field))
		* DECL_SIZE_UNIT (field);
	      fieldsize = (fieldsize + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	    }
	  else
	    fieldsize = int_size_in_bytes (TREE_TYPE (TREE_TYPE (exp)));

	  /* Output the element's initial value.  */
	  output_constant (val, fieldsize);

	  /* Count its size.  */
	  total_bytes += fieldsize;
	}
      else if (TREE_CODE (val) != INTEGER_CST)
	error ("invalid initial value for member `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (field)));
      else
	{
	  /* Element that is a bit-field.  */

	  int next_offset = DECL_OFFSET (field);
	  int end_offset
	    = (next_offset
	       + (TREE_INT_CST_LOW (DECL_SIZE (field))
		  * DECL_SIZE_UNIT (field)));

	  /* If this field does not start in this (or, next) byte,
	     skip some bytes.  */
	  if (next_offset / BITS_PER_UNIT != total_bytes)
	    {
	      /* Output remnant of any bit field in previous bytes.  */
	      if (byte_buffer_in_use)
		{
		  ASM_OUTPUT_BYTE (asm_out_file, byte);
		  total_bytes++;
		  byte_buffer_in_use = 0;
		}

	      /* If still not at proper byte, advance to there.  */
	      if (next_offset / BITS_PER_UNIT != total_bytes)
		{
		  ASM_OUTPUT_SKIP (asm_out_file,
				   next_offset / BITS_PER_UNIT - total_bytes);
		  total_bytes = next_offset / BITS_PER_UNIT;
		}
	    }

	  if (! byte_buffer_in_use)
	    byte = 0;

	  /* We must split the element into pieces that fall within
	     separate bytes, and combine each byte with previous or
	     following bit-fields.  */

	  /* next_offset is the offset n fbits from the begining of
	     the structure to the next bit of this element to be processed.
	     end_offset is the offset of the first bit past the end of
	     this element.  */
	  while (next_offset < end_offset)
	    {
	      int this_time;
	      int next_byte = next_offset / BITS_PER_UNIT;
	      int next_bit = next_offset % BITS_PER_UNIT;

	      /* Advance from byte to byte
		 within this element when necessary.  */
	      while (next_byte != total_bytes)
		{
		  ASM_OUTPUT_BYTE (asm_out_file, byte);
		  total_bytes++;
		  byte = 0;
		}

	      /* Number of bits we can process at once
		 (all part of the same byte).  */
	      this_time = MIN (end_offset - next_offset,
			       BITS_PER_UNIT - next_bit);
#ifdef BYTES_BIG_ENDIAN
	      /* On big-endian machine, take the most significant bits
		 first (of the bits that are significant)
		 and put them into bytes from the most significant end.  */
	      byte |= (((TREE_INT_CST_LOW (val)
			 >> (end_offset - next_offset - this_time))
			& ((1 << this_time) - 1))
		       << (BITS_PER_UNIT - this_time - next_bit));
#else
	      /* On little-endian machines,
		 take first the least significant bits of the value
		 and pack them starting at the least significant
		 bits of the bytes.  */
	      byte |= ((TREE_INT_CST_LOW (val)
			>> (next_offset - DECL_OFFSET (field)))
		       & ((1 << this_time) - 1)) << next_bit;
#endif
	      next_offset += this_time;
	      byte_buffer_in_use = 1;
	    }
	}
    }
  if (byte_buffer_in_use)
    {
      ASM_OUTPUT_BYTE (asm_out_file, byte);
      total_bytes++;
    }
  if (total_bytes < size)
    ASM_OUTPUT_SKIP (asm_out_file, size - total_bytes);
}
