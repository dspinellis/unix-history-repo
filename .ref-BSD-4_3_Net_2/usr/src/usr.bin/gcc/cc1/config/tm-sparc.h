/* Definitions of target machine for GNU compiler, for Sun SPARC.
   Copyright (C) 1988 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com).

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

/* Note that some other tm- files include this one and then override
   many of the definitions that relate to assembler syntax.  */

/* Specify library to handle `-a' basic block profiling.  */

#define LIB_SPEC "%{a:/usr/lib/bb_link.o} \
%{!p:%{!pg:-lc}}%{p:-lc_p}%{pg:-lc_p} "

/* Provide required defaults for linker -e and -d switches.
   Also, it is hard to debug with shared libraries,
   so don't use them if going to debug.  */

#define LINK_SPEC "%{!e*:-e start} -dc -dp %{g:-Bstatic} %{static:-Bstatic} %{Bstatic}"

/* Special flags to the Sun-4 assembler when using pipe for input.  */

#define ASM_SPEC " %{pipe:-} "

/* Prevent error on `-dalign', `-sun4' and `-target sun4' options.  */

#define CC1_SPEC "%{dalign:} %{sun4:} %{target:}"

/* These compiler options take an argument.  We ignore -target for now.  */

#define WORD_SWITCH_TAKES_ARG(STR)	(!strcmp (STR, "target"))

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dsparc -Dsun -Dunix"

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (sparc)");

/* Generate DBX debugging information.  */

#define DBX_DEBUGGING_INFO

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Nonzero if we should generate code to use the fpu.  */
#define TARGET_FPU (target_flags & 1)

/* Nonzero if we should use FUNCTION_EPILOGUE.  Otherwise, we
   use fast return insns, but lose some generality.  */
#define TARGET_EPILOGUE (target_flags & 2)

/* Nonzero if we expect to be passed through the Sun
   optimizing assembler.  This requires us to generate
   code which we otherwise would not.  For example,
   calls via pointers-to-functions must be output
   specially because Sun assemble does not do proper flow
   analysis for this case. */
#define TARGET_SUN_ASM (target_flags & 4)

/* Nonzero if we should do eager peepholes for conditional branch
   scheduling.  */
#define TARGET_EAGER (target_flags & 8)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { {"fpu", 1},			\
    {"soft-float", -1},		\
    {"epilogue", 2},		\
    {"no-epilogue", -2},	\
    {"sun-asm", 4},		\
    {"eager", 8},		\
    { "", TARGET_DEFAULT}}

#define TARGET_DEFAULT 3

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN

/* Define this if most significant byte of a word is the lowest numbered.  */
/* This is true on the SPARC.  */
#define BYTES_BIG_ENDIAN

/* Define this if most significant word of a multiword number is numbered.  */
/* For SPARC we can decide arbitrarily
   since there are no machine instructions for them.  */
/* #define WORDS_BIG_ENDIAN */

/* number of bits in an addressible storage unit */
#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.
   Note that this is not necessarily the width of data type `int';
   if using 16-bit ints on a 68000, this would still be 32.
   But on a machine with 16-bit registers, this would be 16.  */
#define BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 4

/* Width in bits of a pointer.
   See also the macro `Pmode' defined below.  */
#define POINTER_SIZE 32

/* Allocation boundary (in *bits*) for storing pointers in memory.  */
#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY 32

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT

/* Things that must be doubleword aligned cannot go in the text section,
   because the linker fails to align the text section enough!
   Put them in the data section.  */
#define MAX_TEXT_ALIGN 32

#define SELECT_SECTION(T)						\
{									\
  if (TREE_CODE (T) == VAR_DECL)					\
    {									\
      if (TREE_READONLY (T) && ! TREE_VOLATILE (T)			\
	  && DECL_ALIGN (T) <= MAX_TEXT_ALIGN)				\
	text_section ();						\
      else								\
	data_section ();						\
    }									\
  if (*tree_code_type[(int) TREE_CODE (T)] == 'c')			\
    {									\
      if ((TREE_CODE (T) == STRING_CST && flag_writable_strings)	\
	  || TYPE_ALIGN (TREE_TYPE (T)) > MAX_TEXT_ALIGN)		\
	data_section ();						\
      else								\
	text_section ();						\
    }									\
}

/* Use text section for a constant
   unless we need more alignment than that offers.  */
#define SELECT_RTX_SECTION(MODE, X)		\
{						\
  if (GET_MODE_BITSIZE (MODE) <= MAX_TEXT_ALIGN)\
    text_section ();				\
  else						\
    data_section ();				\
}

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   SPARC has 32 fullword registers and 32 floating point registers.  */

#define FIRST_PSEUDO_REGISTER 64

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On SPARC, this includes all the global registers
   (registers r[0] through r[7]) and the callee return
   address register, r[15].  */
#define FIXED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
				\
  1, 1, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0}


/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 1, 1,	\
				\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On SPARC, ordinary registers hold 32 bits worth;
   this means both integer and floating point registers.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On SPARC, the cpu registers can hold any mode but the float registers
   can only hold SFmode or DFmode.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((REGNO) < 32 ? ((GET_MODE_SIZE (MODE) <= 4) ? 1 : ((REGNO) & 1) == 0) : \
   ((MODE) == SFmode ? 1 : (MODE) == DFmode && ((REGNO) & 1) == 0))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (((MODE1) == SFmode || (MODE1) == DFmode) \
   == ((MODE2) == SFmode || (MODE2) == DFmode))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* SPARC pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 14

/* Actual top-of-stack address is 92 greater
   than the contents of the stack pointer register.  */
#define STACK_POINTER_OFFSET 92

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 30

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 30

/* Register in which static-chain is passed to a function.  */
/* ??? */
#define STATIC_CHAIN_REGNUM 1
  

/* Functions which return large structures get the address
   to place the wanted value at offset 64 from the frame.  */
#define STRUCT_VALUE_OFFSET 64 /* Used only in other #defines in this file.  */
#define STRUCT_VALUE \
  gen_rtx (MEM, Pmode,					\
	   gen_rtx (PLUS, SImode, stack_pointer_rtx,	\
		    gen_rtx (CONST_INT, VOIDmode, STRUCT_VALUE_OFFSET)))
#define STRUCT_VALUE_INCOMING \
  gen_rtx (MEM, Pmode,					\
	   gen_rtx (PLUS, SImode, frame_pointer_rtx,	\
		    gen_rtx (CONST_INT, VOIDmode, STRUCT_VALUE_OFFSET)))

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.

   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.

   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.

   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.

   For any two classes, it is very desirable that there be another
   class that represents their union.  */
   
/* The SPARC has two kinds of registers, general and floating point.  */

enum reg_class { NO_REGS, GENERAL_REGS, FP_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "GENERAL_REGS", "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {{0, 0}, {-1, 0}, {0, -1}, {-1, -1}}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) \
 ((REGNO) >= 32 ? FP_REGS : GENERAL_REGS)

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS GENERAL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'f' ? FP_REGS : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For SPARC, `I' is used for the range of constants an insn
   can actually contain.
   `J' is used for the range which is just zero (since that is R0).
   `K' is used for the 5-bit operand of a compare insns.  */

#define SMALL_INT(X) ((unsigned) (INTVAL (X) + 0x1000) < 0x2000)

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? (unsigned) ((VALUE) + 0x1000) < 0x2000	\
   : (C) == 'J' ? (VALUE) == 0				\
   : (C) == 'K' ? (unsigned) (VALUE) < 0x20		\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' && XINT (VALUE, 0) == 0 && XINT (VALUE, 1) == 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On SPARC, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET -16

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On SPARC, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.
   This is 64 for the ins and locals, plus 4 for the struct-return reg
   if this function isn't going to use it.  */
#define FIRST_PARM_OFFSET(FNDECL)		\
  (DECL_MODE (DECL_RESULT (fndecl)) == BLKmode	\
   ? STRUCT_VALUE_OFFSET : STRUCT_VALUE_OFFSET + 4)

/* Offset from top-of-stack address to location to store the
   function parameter if it can't go in a register.
   Addresses for following parameters are computed relative to this one.  */
#define FIRST_PARM_CALLER_OFFSET(FNDECL)	\
  (STRUCT_VALUE_OFFSET + 4 - STACK_POINTER_OFFSET)

/* When a parameter is passed in a register, stack space is still
   allocated for it.  */
#define REG_PARM_STACK_SPACE

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.  */

#define RETURN_POPS_ARGS(FUNTYPE) 0

/* Some subroutine macros specific to this machine.  */
#define BASE_RETURN_VALUE_REG(MODE) \
 ((MODE) == SFmode || (MODE) == DFmode ? 32 : 8)
#define BASE_OUTGOING_VALUE_REG(MODE) \
 ((MODE) == SFmode || (MODE) == DFmode ? 32 : 24)
#define BASE_PASSING_ARG_REG(MODE) (8)
#define BASE_INCOMING_ARG_REG(MODE) (24)

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On SPARC the value is found in the first "output" register.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), BASE_RETURN_VALUE_REG (TYPE_MODE (VALTYPE)))

/* But the called function leaves it in the first "input" register.  */

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), BASE_OUTGOING_VALUE_REG (TYPE_MODE (VALTYPE)))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)	\
  gen_rtx (REG, MODE, BASE_RETURN_VALUE_REG (MODE))

/* 1 if N is a possible register number for a function value
   as seen by the caller.
   On SPARC, the first "output" reg is used for integer values,
   and the first floating point register is used for floating point values.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 8 || (N) == 32)

/* 1 if N is a possible register number for function argument passing.
   On SPARC, these are the "output" registers.  */

#define FUNCTION_ARG_REGNO_P(N) ((N) < 14 && (N) > 7)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On SPARC, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus 7 or more means all following args should go on the stack.  */

#define CUMULATIVE_ARGS int

/* Define the number of register that can hold parameters.
   This macro is used only in other macro definitions below.  */
#define NPARM_REGS 6

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On SPARC, the offset always starts at 0: the first parm reg is always
   the same reg.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE) ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode				\
	    ? (GET_MODE_SIZE (MODE) + 3) / 4		\
	    : (int_size_in_bytes (TYPE) + 3) / 4))

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

/* On SPARC the first six args are normally in registers
   and the rest are pushed.  Any arg that starts within the first 6 words
   is at least partially passed in a register unless its data type forbids.  */
  
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)				\
((CUM) < NPARM_REGS							\
 && ((TYPE)==0 || ! TREE_ADDRESSABLE ((tree)(TYPE)))			\
 && ((MODE) != BLKmode || (TYPE_ALIGN ((TYPE)) % 32 == 0))		\
 ? gen_rtx (REG, (MODE), BASE_PASSING_ARG_REG (MODE) + (CUM)) : 0)

/* Define where a function finds its arguments.
   This is different from FUNCTION_ARG because of register windows.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)			\
((CUM) < NPARM_REGS							\
 && ((TYPE)==0 || ! TREE_ADDRESSABLE ((tree)(TYPE)))			\
 && ((MODE) != BLKmode || (TYPE_ALIGN ((TYPE)) % 32 == 0))		\
 ? gen_rtx (REG, (MODE), BASE_INCOMING_ARG_REG (MODE) + (CUM)) : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.
   Any arg that starts in the first 6 regs but won't entirely fit in them
   needs partial registers on the Sparc.  */
  
#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 		\
  (((CUM) < NPARM_REGS							\
    && ((TYPE)==0 || ! TREE_ADDRESSABLE ((tree)(TYPE)))			\
    && ((MODE) != BLKmode || (TYPE_ALIGN ((TYPE)) % 32 == 0))		\
    && ((CUM)								\
	+ ((MODE) == BLKmode						\
	   ? (int_size_in_bytes (TYPE) + 3) / 4				\
	   : (GET_MODE_SIZE (MODE) + 3) / 4)) - NPARM_REGS > 0)		\
   ? (NPARM_REGS - (CUM))						\
   : 0)

/* Output the label for a function definition.  */

#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL) \
{							\
  extern tree double_type_node, float_type_node;	\
  if (TREE_TYPE (DECL) == float_type_node)		\
    fprintf (FILE, "\t.proc 6\n");			\
  else if (TREE_TYPE (DECL) == double_type_node)	\
    fprintf (FILE, "\t.proc 7\n");			\
  else if (TREE_TYPE (DECL) == void_type_node)		\
    fprintf (FILE, "\t.proc 0\n");			\
  else fprintf (FILE, "\t.proc 1\n");			\
  ASM_OUTPUT_LABEL (FILE, NAME);			\
}

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

/* On SPARC, move-double insns between fpu and cpu need an 8-byte block
   of memory.  If any fpu reg is used in the function, we allocate
   such a block here, at the bottom of the frame, just in case it's needed.

   If this function is a leaf procedure, then we may choose not
   to do a "save" insn.  Currently we do this only if it touches
   the "output" registers.  The "local" and "input" registers
   are off limits.  It might be better to allow one such register
   to go to the stack, but I doubt it.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)				\
{								\
  extern char call_used_regs[];					\
  extern int current_function_pretend_args_size;		\
  extern int frame_pointer_needed;				\
  int fsize = (((SIZE) + 7 - STARTING_FRAME_OFFSET) & -8);	\
  int actual_fsize;						\
  int n_fregs = 0, i;						\
  int n_iregs = 64;						\
  for (i = 32; i < FIRST_PSEUDO_REGISTER; i++)			\
    if (regs_ever_live[i] && ! call_used_regs[i])		\
      n_fregs++;						\
  for (i = 16; i < 32; i++)					\
    if (regs_ever_live[i]) { n_iregs = 96; break; }		\
  fprintf (FILE, "\t!#PROLOGUE# 0\n");				\
  actual_fsize = fsize + n_iregs + (n_fregs*4+7 & -8);		\
  fsize += current_function_pretend_args_size+7 & -8;		\
  actual_fsize += current_function_pretend_args_size+7 & -8;	\
  if (actual_fsize < 4096)					\
    fprintf (FILE, "\tsave %%sp,-%d,%%sp\n", actual_fsize);	\
  else								\
    {								\
      fprintf (FILE, "\tsethi %%hi(0x%x),%%g1\n\tadd %%g1,%%lo(0x%x),%%g1\n", \
	       -actual_fsize, -actual_fsize);			\
      fprintf (FILE, "\tsave %%sp,%%g1,%%sp\n");		\
    }								\
  fprintf (FILE, "\t!#PROLOGUE# 1\n");				\
  if (n_fregs)							\
    {								\
      for (i = 32, n_fregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
        if (regs_ever_live[i] && ! call_used_regs[i])		\
          {							\
	    if (regs_ever_live[i+1] && ! call_used_regs[i+1])	\
	      fprintf (FILE, "\tstd %s,[%%sp+0x%x]\n",		\
		       reg_names[i], n_iregs + 4 * n_fregs),	\
	      n_fregs += 2, i += 1;				\
	    else						\
	      fprintf (FILE, "\tstf %s,[%%sp+0x%x]\n",		\
		       reg_names[i], n_iregs + 4 * n_fregs++);	\
          }							\
    }								\
  if (regs_ever_live[32])					\
    fprintf (FILE, "\tst %s,[%%fp-16]\n\tst %s,[%%fp-12]\n",	\
	     reg_names[0], reg_names[0]);			\
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\tsethi %%hi(LP%d),%%o0\n\tcall mcount\n\tor %%lo(LP%d),%%o0,%%o0\n", \
	   (LABELNO), (LABELNO))

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\tsethi %%hi(LPBX0),%%o0\n\tld [%%lo(LPBX0)+%%o0],%%o1\n\ttst %%o1\n\tbne LPY%d\n\tnop\n\tcall ___bb_init_func\n\tnop\nLPY%d:\n",  \
	   (LABELNO), (LABELNO))

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO) \
{								\
  int blockn = (BLOCKNO);					\
  fprintf (FILE, "\tsethi %%hi(LPBX2+%d),%%g1\n\tld [%%lo(LPBX2+%d)+%%g1],%%g2\n\
\tadd %%g2,1,%%g2\n\tst %%g2,[%%lo(LPBX2+%d)+%%g1]\n",		\
	   4 * blockn, 4 * blockn, 4 * blockn);			\
  CC_STATUS_INIT;  /* We have clobbered %g1.  Also %g2.  */	\
}

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

extern int may_call_alloca;
extern int current_function_pretend_args_size;

#define EXIT_IGNORE_STACK	\
 (get_frame_size () != 0	\
  || may_call_alloca || current_function_pretend_args_size)

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

/* This declaration is needed due to traditional/ANSI
   incompatibilities which cannot be #ifdefed away
   because they occur inside of macros.  Sigh.  */
extern union tree_node *current_function_decl;

#define FUNCTION_EPILOGUE(FILE, SIZE)				\
{								\
  extern char call_used_regs[];					\
  extern int may_call_alloca;					\
  extern int current_function_pretend_args_size;		\
  extern int max_pending_stack_adjust;				\
  extern int frame_pointer_needed;				\
  int fsize = (((SIZE) + 7 - STARTING_FRAME_OFFSET) & -8);	\
  int actual_fsize;						\
  int n_fregs = 0, i;						\
  int n_iregs = 64;						\
  for (i = 32, n_fregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
    if (regs_ever_live[i] && ! call_used_regs[i])		\
      n_fregs++;						\
  for (i = 16; i < 32; i++)					\
    if (regs_ever_live[i]) { n_iregs = 96; break; }		\
  actual_fsize = fsize + n_iregs + (n_fregs*4+7 & -8);		\
  actual_fsize += current_function_pretend_args_size+7 & -8;	\
  fsize += current_function_pretend_args_size+7 & -8;		\
  if (n_fregs)							\
    {								\
      char *base;						\
      int offset;						\
      if (fsize < 4096)						\
	{ base = "%fp"; offset = n_iregs - actual_fsize; }	\
      else							\
	{ base = "%g1"; offset = n_iregs;			\
	  if (fsize < 4096)					\
	    fprintf (FILE, "sethi %%hi(0x%x),%%g1\n\tadd %%g1,%%lo(0x%x),%%g1\n\tadd %%fp,%%g1,%%g1\n", -actual_fsize, -actual_fsize);\
	}							\
      for (i = 32, n_fregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
	if (regs_ever_live[i] && ! call_used_regs[i])		\
	  {							\
	    if (regs_ever_live[i+1] && ! call_used_regs[i+1])	\
	      fprintf (FILE, "\tldd [%s%+d],%s\n",		\
		       base, offset + 4 * n_fregs,		\
		       reg_names[i]),				\
	      n_fregs += 2, i += 1;				\
	    else						\
	      fprintf (FILE, "\tldf [%s%+d],%s\n",		\
		       base, offset + 4 * n_fregs++,		\
		       reg_names[i]);				\
	  }							\
    }								\
  fprintf (FILE, "\tret\n\trestore\n");				\
}

/* If the memory address ADDR is relative to the frame pointer,
   correct it to be relative to the stack pointer instead.
   This is for when we don't use a frame pointer.
   ADDR should be a variable name.  */

#define FIX_FRAME_POINTER_ADDRESS(ADDR,DEPTH)  \
{ int offset = -1;							\
  rtx regs = stack_pointer_rtx;						\
  if (ADDR == frame_pointer_rtx)					\
    offset = 0;								\
  else if (GET_CODE (ADDR) == PLUS && XEXP (ADDR, 0) == frame_pointer_rtx \
	   && GET_CODE (XEXP (ADDR, 1)) == CONST_INT)			\
    offset = INTVAL (XEXP (ADDR, 1));					\
  else if (GET_CODE (ADDR) == PLUS && XEXP (ADDR, 0) == frame_pointer_rtx) \
    { rtx other_reg = XEXP (ADDR, 1);					\
      offset = 0;							\
      regs = gen_rtx (PLUS, Pmode, stack_pointer_rtx, other_reg); }	\
  else if (GET_CODE (ADDR) == PLUS && XEXP (ADDR, 1) == frame_pointer_rtx) \
    { rtx other_reg = XEXP (ADDR, 0);					\
      offset = 0;							\
      regs = gen_rtx (PLUS, Pmode, stack_pointer_rtx, other_reg); }	\
  if (offset >= 0)							\
    { int regno;							\
      extern char call_used_regs[];					\
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)		\
        if (regs_ever_live[regno] && ! call_used_regs[regno])		\
          offset += 4;							\
      offset -= 4;							\
      ADDR = plus_constant (regs, offset + (DEPTH)); } }

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_BASE_P(REGNO) \
((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)
#define REGNO_OK_FOR_FP_P(REGNO) \
(((REGNO) ^ 0x20) < 32 || (unsigned) (reg_renumber[REGNO] ^ 0x20) < 32)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the SPARC, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)  CONSTANT_P (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   Anything but a CONST_DOUBLE can be made to work.  */

#define LEGITIMATE_CONSTANT_P(X)		\
 (GET_CODE (X) != CONST_DOUBLE)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.

   Most source files want to accept pseudo regs in the hope that
   they will get allocated to the class that the insn wants them to be in.
   Source files for reload pass need to be strict.
   After reload, it makes no difference, since pseudo regs have
   been eliminated by then.  */

#ifndef REG_OK_STRICT

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  */
#define REG_OK_FOR_INDEX_P(X) (((unsigned) REGNO (X)) - 32 >= 32)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (((unsigned) REGNO (X)) - 32 >= 32)

#else

/* Nonzero if X is a hard reg that can be used as an index.  */
#define REG_OK_FOR_INDEX_P(X) REGNO_OK_FOR_INDEX_P (REGNO (X))
/* Nonzero if X is a hard reg that can be used as a base reg.  */
#define REG_OK_FOR_BASE_P(X) REGNO_OK_FOR_BASE_P (REGNO (X))

#endif

/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   On SPARC, the actual legitimate addresses must be REG+REG or REG+SMALLINT.
   But we can treat a SYMBOL_REF as legitimate if it is part of this
   function's constant-pool, because such addresses can actually
   be output as REG+SMALLINT.

   Try making SYMBOL_REF (and other things which are CONSTANT_ADDRESS_P)
   a legitimate address, regardless.  Because the only insns which can use
   memory are load or store insns, the added hair in the machine description
   is not that bad.  It should also speed up the compiler by halving the number
   of insns it must manage for each (MEM (SYMBOL_REF ...)) involved.  */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)		\
{ if (GET_CODE (X) == REG)				\
    { if (REG_OK_FOR_BASE_P (X)) goto ADDR; }		\
  else if (GET_CODE (X) == PLUS)			\
    {							\
      if (GET_CODE (XEXP (X, 0)) == REG			\
	  && REG_OK_FOR_BASE_P (XEXP (X, 0)))		\
	{						\
	  if (GET_CODE (XEXP (X, 1)) == REG		\
	      && REG_OK_FOR_INDEX_P (XEXP (X, 1)))	\
	    goto ADDR;					\
	  if (GET_CODE (XEXP (X, 1)) == CONST_INT	\
	      && INTVAL (XEXP (X, 1)) >= -0x1000	\
	      && INTVAL (XEXP (X, 1)) < 0x1000)		\
	    goto ADDR;					\
	}						\
      else if (GET_CODE (XEXP (X, 1)) == REG		\
	  && REG_OK_FOR_BASE_P (XEXP (X, 1)))		\
	{						\
	  if (GET_CODE (XEXP (X, 0)) == REG		\
	      && REG_OK_FOR_INDEX_P (XEXP (X, 0)))	\
	    goto ADDR;					\
	  if (GET_CODE (XEXP (X, 0)) == CONST_INT	\
	      && INTVAL (XEXP (X, 0)) >= -0x1000	\
	      && INTVAL (XEXP (X, 0)) < 0x1000)		\
	    goto ADDR;					\
	}						\
    }							\
  else if (CONSTANT_ADDRESS_P (X))			\
    goto ADDR;						\
}

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.  */

/* On SPARC, change REG+N into REG+REG, and REG+(X*Y) into REG+REG.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)	\
{ if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 1)))	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   copy_to_mode_reg (SImode, XEXP (X, 1)));	\
  if (GET_CODE (X) == PLUS && CONSTANT_ADDRESS_P (XEXP (X, 0)))	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   copy_to_mode_reg (SImode, XEXP (X, 0)));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   force_operand (XEXP (X, 0), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), 0));		\
  if (GET_CODE (x) == SYMBOL_REF)				\
    (X) = copy_to_reg (X);					\
  if (memory_address_p (MODE, X))				\
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the SPARC this is never true.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
#define SHIFT_COUNT_TRUNCATED

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE SImode

/* Define this if addresses of constant functions
   shouldn't be put through pseudo regs where they can be cse'd.
   Desirable on machines where ordinary constants are expensive
   but a CALL with constant address is cheap.  */
#define NO_FUNCTION_CSE

/* Define subroutines to call to handle multiply and divide.
   Use the subroutines that Sun's library provides.
   The `*' prevents an underscore from being prepended by the compiler.  */

#define DIVSI3_LIBCALL "*.div"
#define UDIVSI3_LIBCALL "*.udiv"
#define MODSI3_LIBCALL "*.rem"
#define UMODSI3_LIBCALL "*.urem"
#define MULSI3_LIBCALL "*.mul"
#define UMULSI3_LIBCALL "*.umul"

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE) \
  case CONST_INT:						\
    if (INTVAL (RTX) < 0x1000 && INTVAL (RTX) >= -0x1000) return 0; \
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 2;							\
  case CONST_DOUBLE:						\
    return 4;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* This holds the value sourcing %hi(%g1).  We keep this info
   around so that mem/mem ops, such as increment and decrement,
   etc, can be performed reasonably.  */
#define CC_STATUS_MDEP rtx

/* Nonzero if the results of the previous comparison are
   in the floating point condition code register.  */

#define CC_IN_FCCR 04000

/* Nonzero if the results of the previous comparison are
   int the coprocessor's condition code register.  */

#define CC_IN_CCCR 010000

/* Nonzero if we know (easily) that floating point register f0
   (f1) contains the value 0.  */
#define CC_F0_IS_0 020000
#define CC_F1_IS_0 040000

/* Nonzero if we know the value of %hi(%g1).  */
#define CC_KNOW_HI_G1 0100000

#define CC_STATUS_MDEP_INIT (cc_status.mdep = 0)

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{ if (GET_CODE (EXP) == SET)					\
    { if (SET_DEST (EXP) == cc0_rtx)				\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (EXP);			\
	  cc_status.value2 = SET_SRC (EXP); }			\
      else if (GET_CODE (SET_SRC (EXP)) == CALL)		\
	{ CC_STATUS_INIT; }					\
      else if (GET_CODE (SET_DEST (EXP)) == REG)		\
	{ if (cc_status.value1					\
	      && reg_overlap_mentioned_p (SET_DEST (EXP), cc_status.value1)) \
	    cc_status.value1 = 0;				\
	  if (cc_status.value2					\
	      && reg_overlap_mentioned_p (SET_DEST (EXP), cc_status.value2)) \
	    cc_status.value2 = 0;				\
	}							\
      else if (GET_CODE (SET_DEST (EXP)) == MEM)		\
	{ rtx x = cc_status.mdep; int know = cc_status.flags & CC_KNOW_HI_G1;	\
	  CC_STATUS_INIT;					\
	  if (x && know)					\
	    { cc_status.mdep = x; cc_status.flags |= CC_KNOW_HI_G1; }		\
	}							\
    }								\
  else if (GET_CODE (EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP (EXP, 0, 0)) == SET)		\
    { if (SET_DEST (XVECEXP (EXP, 0, 0)) == cc0_rtx)		\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (XVECEXP (EXP, 0, 0));	\
	  cc_status.value2 = SET_SRC (XVECEXP (EXP, 0, 0));	\
	}							\
      else if (GET_CODE (SET_SRC (XVECEXP (EXP, 0, 0))) == CALL) \
	{ /* all bets are off */ CC_STATUS_INIT; }		\
      else if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) == REG) \
	{ if (cc_status.value1					\
	      && reg_overlap_mentioned_p (SET_DEST (XVECEXP (EXP, 0, 0)), cc_status.value1)) \
	    cc_status.value1 = 0;				\
	  if (cc_status.value2					\
	      && reg_overlap_mentioned_p (SET_DEST (XVECEXP (EXP, 0, 0)), cc_status.value2)) \
	    cc_status.value2 = 0;				\
	}							\
      else if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) == MEM) \
	{ rtx x = cc_status.mdep; int know = cc_status.flags & CC_KNOW_HI_G1;	\
	  CC_STATUS_INIT;					\
	  if (x && know)					\
	    { cc_status.mdep = x; cc_status.flags |= CC_KNOW_HI_G1; }		\
	}							\
    }								\
  else if (GET_CODE (EXP) == PARALLEL)				\
  /* insn-peep has changed this insn beyond recognition
     by NOTICE_UPDATE_CC.  However, we know it is either
     a call or a branch with a delay slot filled, so we can
     give up on knowing condition codes in any case.  */	\
    { CC_STATUS_INIT; }						\
  else if (GET_CODE (EXP) == CALL)				\
    { /* all bets are off */ CC_STATUS_INIT; }			\
}

#define OUTPUT_JUMP(NORMAL, NO_OV, FLOAT)  \
{ if (cc_prev_status.flags & CC_IN_FCCR)			\
    return FLOAT;						\
  if (cc_prev_status.flags & CC_NO_OVERFLOW)			\
    return NO_OV;						\
  return NORMAL; }

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(file)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"%g0", "%g1", "%g2", "%g3", "%g4", "%g5", "%g6", "%g7",		\
 "%o0", "%o1", "%o2", "%o3", "%o4", "%o5", "%sp", "%o7",		\
 "%l0", "%l1", "%l2", "%l3", "%l4", "%l5", "%l6", "%l7",		\
 "%i0", "%i1", "%i2", "%i3", "%i4", "%i5", "%fp", "%i7",		\
 "%f0", "%f1", "%f2", "%f3", "%f4", "%f5", "%f6", "%f7",		\
 "%f8", "%f9", "%f10", "%f11", "%f12", "%f13", "%f14", "%f15",		\
 "%f16", "%f17", "%f18", "%f19", "%f20", "%f21", "%f22", "%f23",	\
 "%f24", "%f25", "%f26", "%f27", "%f28", "%f29", "%f30", "%f31"}	\

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* On Sun 4, this limit is 2048.  We use 1500 to be safe,
   since the length can run past this up to a continuation point.  */
#define DBX_CONTIN_LENGTH 1500

/* This is how to output a note to DBX telling it the line number
   to which the following sequence of instructions corresponds.

   This is needed for SunOS 4.0, and should not hurt for 3.2
   versions either.  */
#define ASM_OUTPUT_SOURCE_LINE(file, line)		\
  { static int sym_lineno = 1;				\
    fprintf (file, ".stabn 68,0,%d,LM%d\nLM%d:\n",	\
	     line, sym_lineno, sym_lineno);		\
    sym_lineno += 1; }

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs (".global ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)					\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "\t.double 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "\t.double 0r%.20e\n", (VALUE)))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)					\
  (isinf ((VALUE))							\
   ? fprintf (FILE, "\t.single 0r%s99e999\n", ((VALUE) > 0 ? "" : "-")) \
   : fprintf (FILE, "\t.single 0r%.20e\n", (VALUE)))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.half "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.word L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   (SPARC does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\t.word L%d-L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", (1<<(LOG)))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".global ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fputs ("\n.common ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u,\"bss\"\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\n.reserve ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u,\"bss\"\n", (ROUNDED)))

/* Store in OUTPUT a string (made with alloca) containing
   an assembler-name for a local static variable named NAME.
   LABELNO is an integer which is different for each call.  */

#define ASM_FORMAT_PRIVATE_NAME(OUTPUT, NAME, LABELNO)	\
( (OUTPUT) = (char *) alloca (strlen ((NAME)) + 10),	\
  sprintf ((OUTPUT), "%s.%d", (NAME), (LABELNO)))

/* Define the parentheses used to group arithmetic operations
   in assembler code.  */

#define ASM_OPEN_PAREN "("
#define ASM_CLOSE_PAREN ")"

/* Define results of standard character escape sequences.  */
#define TARGET_BELL 007
#define TARGET_BS 010
#define TARGET_TAB 011
#define TARGET_NEWLINE 012
#define TARGET_VT 013
#define TARGET_FF 014
#define TARGET_CR 015

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   On SPARC, the CODE can be `r', meaning this is a register-only operand
   and an immediate zero should be represented as `r0'.
   It can also be `m', meaning that X is a memory reference but print
   its address as a non-memory operand.

   Codes C, N, F, I, and U are used for printing the opcodes of conditional
   branches.  C prints the opcode for a given condition; N the negated opcode.
   F prints the negated floating point opcode (different because of nans).
   I prints the opcode that ignores the overflow bit, and U its negation.  */

#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (GET_CODE (X) == REG)					\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);			\
  else if ((CODE) == 'm')					\
    output_address (XEXP (X, 0));				\
  else if (GET_CODE (X) == MEM)					\
    {								\
      fputc ('[', FILE);					\
      output_address (XEXP (X, 0));				\
      fputc (']', FILE);					\
    }								\
  else if (GET_CODE (X) == CONST_DOUBLE)			\
    abort ();							\
  else if ((CODE) == 'r' && (X) == const0_rtx)			\
    fprintf (FILE, "%%g0");					\
  else if ((CODE) == 'C') switch (GET_CODE (X))			\
    {								\
    case EQ: fputs ("e", FILE); break;				\
    case NE: fputs ("ne", FILE); break;				\
    case GT: fputs ("g", FILE); break;				\
    case GE: fputs ("ge", FILE); break;				\
    case LT: fputs ("l", FILE); break;				\
    case LE: fputs ("le", FILE); break;				\
    case GTU: fputs ("gu", FILE); break;			\
    case GEU: fputs ("geu", FILE); break;			\
    case LTU: fputs ("lu", FILE); break;			\
    case LEU: fputs ("leu", FILE); break;			\
    }								\
  else if ((CODE) == 'I') switch (GET_CODE (X))			\
    {								\
    case EQ: fputs ("e", FILE); break;				\
    case NE: fputs ("ne", FILE); break;				\
    case GE: fputs ("pos", FILE); break;			\
    case LT: fputs ("neg", FILE); break;			\
    default:							\
       abort ();						\
    }								\
  else if ((CODE) == 'U') switch (GET_CODE (X))			\
    {								\
    case EQ: fputs ("ne", FILE); break;				\
    case NE: fputs ("e", FILE); break;				\
    case GE: fputs ("neg", FILE); break;			\
    case LT: fputs ("pos", FILE); break;			\
    default:							\
       abort ();						\
    }								\
  else if ((CODE) == 'N') switch (GET_CODE (X))			\
    {								\
    case EQ: fputs ("ne", FILE); break;				\
    case NE: fputs ("e", FILE); break;				\
    case GT: fputs ("le", FILE); break;				\
    case GE: fputs ("l", FILE); break;				\
    case LT: fputs ("ge", FILE); break;				\
    case LE: fputs ("g", FILE); break;				\
    case GTU: fputs ("leu", FILE); break;			\
    case GEU: fputs ("lu", FILE); break;			\
    case LTU: fputs ("geu", FILE); break;			\
    case LEU: fputs ("gu", FILE); break;			\
    }								\
  else if ((CODE) == 'F') switch (GET_CODE (X))			\
    {								\
    case EQ: fputs ("ne", FILE); break;				\
    case NE: fputs ("e", FILE); break;				\
    case GT: fputs ("ule", FILE); break;			\
    case GE: fputs ("ul", FILE); break;				\
    case LT: fputs ("uge", FILE); break;			\
    case LE: fputs ("ug", FILE); break;				\
    default: abort ();						\
    }								\
  else { output_addr_const (FILE, X); }}

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx base, index = 0;					\
  int offset = 0;						\
  register rtx addr = ADDR;					\
  if (GET_CODE (addr) == REG)					\
    {								\
      fprintf (FILE, "%s", reg_names[REGNO (addr)]);		\
    }								\
  else if (GET_CODE (addr) == PLUS)				\
    {								\
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);\
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);\
      else							\
	base = XEXP (addr, 0), index = XEXP (addr, 1);		\
      fprintf (FILE, "%s", reg_names[REGNO (base)]);		\
      if (index == 0)						\
	fprintf (FILE, "%+d", offset);				\
      else							\
	fprintf (FILE, "+%s", reg_names[REGNO (index)]);	\
    }								\
  else								\
    {								\
      output_addr_const (FILE, addr);				\
    }								\
}

