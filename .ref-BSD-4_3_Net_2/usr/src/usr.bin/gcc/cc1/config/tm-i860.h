/* Definitions of target machine for GNU compiler, for Intel 860.
   Copyright (C) 1989 Free Software Foundation, Inc.

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


/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Di860 -Dunix"

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (i860)");

/* Run-time compilation parameters selecting different hardware subsets.

   On the i860, we have one: TARGET_FPU.  */

extern int target_flags;

/* Nonzero if we should generate code to use the fpu.  */
#define TARGET_FPU (target_flags & 1)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { {"fpu", 1},			\
    {"soft-float", -1},		\
    { "", TARGET_DEFAULT}}

#define TARGET_DEFAULT 1

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is a moot question on the i860 due to the lack of bit-field insns.  */
/* #define BITS_BIG_ENDIAN */

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is not true on i860 in the mode we will use.  */
/* #define BYTES_BIG_ENDIAN */

/* Define this if most significant word of a multiword number is numbered.  */
/* For the i860 this goes with BYTES_BIG_ENDIAN.  */
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
#define STACK_BOUNDARY 128

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT

/* If bit field type is int, dont let it cross an int,
   and give entire struct the alignment of an int.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   i860 has 32 fullword registers and 32 floating point registers.  */

#define FIRST_PSEUDO_REGISTER 64

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the i860, this includes the always-0 registers
   and fp, sp, and the return address.
   Also r31, used for special purposes for constant addresses.  */
#define FIXED_REGISTERS  \
 {1, 1, 1, 1, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 1,	\
  1, 1, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   On the i860, these are r0-r3, r16-r31, f0, f1, and f16-f31.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 0, 0, 0, 0,	\
  0, 0, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 0, 0, 0, 0, 0, 0,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1,	\
  1, 1, 1, 1, 1, 1, 1, 1}

#define REG_ALLOC_ORDER  \
 {16, 17, 18, 19, 20, 21, 22, 23, 	\
  24, 25, 26, 27, 28, 29, 30, 31,	\
  0, 1, 2, 3, 4, 5, 6, 7,		\
  8, 9, 10, 11, 12, 13, 14, 15,		\
  40, 41, 42, 43, 44, 45, 46, 47,	\
  48, 49, 50, 51, 52, 53, 54, 55,	\
  56, 57, 58, 59, 60, 61, 62, 63,	\
  32, 33, 34, 35, 36, 37, 38, 39}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the i860, all registers hold 32 bits worth.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  (((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the i860, any register can hold anything, provided it is properly
   aligned.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (((GET_MODE_SIZE ((MODE)) <= 4) || ((REGNO) & 1) == 0)	\
   && ((REGNO) < 32 || TARGET_FPU))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
/* I think that is not always true; alignment restrictions for doubles
   should not prevent tying them with singles.  So try allowing that.
   On the other hand, don't let fixed and floating be tied;
   this restriction is not necessary, but may make better code.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  ((GET_MODE_CLASS ((MODE1)) == MODE_FLOAT)	\
   == (GET_MODE_CLASS ((MODE2)) == MODE_FLOAT))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* i860 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 2

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 3

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 28

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 29

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 16

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
   
/* The i860 has two kinds of registers, hence four classes.  */

enum reg_class { NO_REGS, GENERAL_REGS, FP_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "GENERAL_REGS", "FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS	\
 {{0, 0}, {0xffffffff, 0},	\
  {0, 0xffffffff}, {0xffffffff, 0xffffffff}}

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

   For the i860, `I' is used for the range of constants 
   an add/subtract insn can actually contain.
   But not including -0x8000, since we need
   to negate the constant sometimes.
   `J' is used for the range which is just zero (since that is R0).
   `K' is used for the range allowed in bte.
   `L' is used for the range allowed in logical insns.  */

#define SMALL_INT(X) ((unsigned) (INTVAL (X) + 0x7fff) < 0xffff)

#define LOGIC_INT(X) ((unsigned) INTVAL (X) < 0x10000)

#define SMALL_INTVAL(X) ((unsigned) ((X) + 0x7fff) < 0xffff)

#define LOGIC_INTVAL(X) ((unsigned) (X) < 0x10000)

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? ((unsigned) (VALUE) + 0x7fff) < 0xffff	\
   : (C) == 'J' ? (VALUE) == 0				\
   : (C) == 'K' ? (unsigned) (VALUE) < 0x20	\
   : (C) == 'L' ? (unsigned) (VALUE) < 0x10000	\
   : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  	\
  ((C) == 'G' && CONST_DOUBLE_LOW ((VALUE)) == 0	\
   && CONST_DOUBLE_HIGH ((VALUE)) == 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */
#define PREFERRED_RELOAD_CLASS(X,CLASS) (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the i860, this is the size of MODE in words.  */
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
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the i860, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.  */

#define RETURN_POPS_ARGS(FUNTYPE) 0

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the i860, the value register depends on the mode.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE),				\
	   (GET_MODE_CLASS (TYPE_MODE (VALTYPE)) == MODE_FLOAT	\
	    ? 40 : 16))

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)				\
  gen_rtx (REG, MODE,					\
	   (GET_MODE_CLASS ((MODE)) == MODE_FLOAT	\
	    ? 40 : 16))

/* 1 if N is a possible register number for a function value
   as seen by the caller.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 40 || (N) == 16)

/* 1 if N is a possible register number for function argument passing.
   On the i860, these are r16-r27 and f8-f15.  */

#define FUNCTION_ARG_REGNO_P(N)		\
  (((N) < 28 && (N) > 15) || ((N) < 48 && (N) >= 40))

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the i860, we must count separately the number of general registers used
   and the number of float registers used.  */

#define CUMULATIVE_ARGS struct { int ints, floats; }

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the i860, the general-reg offset normally starts at 0,
   but starts at 4 bytes
   when the function gets a structure-value-address as an
   invisible first argument.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE)	\
 ((CUM).ints = ((FNTYPE) != 0 && aggregate_value_p ((FNTYPE)) \
		? 4 : 0),			\
  (CUM).floats = 0)

/* Machine-specific subroutines of the following macros.  */
#define CEILING(X,Y)  (((X) + (Y) - 1) / (Y))
#define ROUNDUP(X,Y)  (CEILING ((X), (Y)) * (Y))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)
   Floats, and doubleword ints, are returned in f regs;
   other ints, in r regs.
   Aggregates, even short ones, are passed in memory.  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)		\
 ((TYPE) != 0 && (TREE_CODE ((TYPE)) == RECORD_TYPE		\
		  || TREE_CODE ((TYPE)) == UNION_TYPE)		\
  ? 0								\
  : GET_MODE_CLASS ((MODE)) == MODE_FLOAT || (MODE) == DImode	\
  ? ((CUM).floats = (ROUNDUP ((CUM).floats, GET_MODE_SIZE ((MODE)))	\
		     + ROUNDUP (GET_MODE_SIZE (MODE), 4)))	\
  : GET_MODE_CLASS ((MODE)) == MODE_INT				\
  ? ((CUM).ints = (ROUNDUP ((CUM).ints, GET_MODE_SIZE ((MODE))) \
		   + ROUNDUP (GET_MODE_SIZE (MODE), 4)))	\
  : 0)

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

/* On the i860, the first 12 words of integer arguments go in r16-r27,
   and the first 8 words of floating arguments go in f8-f15.
   DImode values are treated as floats.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)		\
 ((TYPE) != 0 && (TREE_CODE ((TYPE)) == RECORD_TYPE	\
		  || TREE_CODE ((TYPE)) == UNION_TYPE)	\
  ? 0							\
  : GET_MODE_CLASS ((MODE)) == MODE_FLOAT || (MODE) == DImode	\
  ? (ROUNDUP ((CUM).floats, GET_MODE_SIZE ((MODE))) < 32	\
     ? gen_rtx (REG, (MODE),				\
		40+(ROUNDUP ((CUM).floats,		\
			     GET_MODE_SIZE ((MODE)))	\
		    / 4))				\
     : 0)						\
  : GET_MODE_CLASS ((MODE)) == MODE_INT			\
  ? (ROUNDUP ((CUM).ints, GET_MODE_SIZE ((MODE))) < 48	\
     ? gen_rtx (REG, (MODE),				\
		16+(ROUNDUP ((CUM).ints,		\
			     GET_MODE_SIZE ((MODE)))	\
		    / 4))				\
     : 0)						\
  : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)				\
{								\
  extern char call_used_regs[];					\
  int fsize = (SIZE);						\
  int nregs, i;							\
  for (i = 0, nregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
    {								\
      if (regs_ever_live[i] && ! call_used_regs[i])		\
        nregs++;						\
    }								\
  fsize += nregs * 4 + 8;					\
  fsize = (fsize + 15) & -16;					\
  if (fsize > 0x7fff)						\
    {								\
      fprintf (FILE, "\tadds -16,sp,sp\n");			\
      fprintf (FILE, "\tst.l fp,8(sp)\n");			\
      fprintf (FILE, "\tst.l r1,12(sp)\n");			\
      fprintf (FILE, "\tadds 8,sp,fp\n");			\
      fprintf (FILE, "\torh %d,r0,r31\n", (fsize - 16) >> 16);	\
      fprintf (FILE, "\tor %d,r31,r31\n", (fsize - 16) & 0xffff); \
      fprintf (FILE, "\tsubs sp,r31,sp\n");			\
    }								\
  else								\
    {								\
      fprintf (FILE, "\tadds -%d,sp,sp\n", fsize);		\
      fprintf (FILE, "\tst.l fp,%d(sp)\n", fsize - 8);		\
      fprintf (FILE, "\tst.l r1,%d(sp)\n", fsize - 4);		\
      fprintf (FILE, "\tadds %d,sp,fp\n", fsize - 8);		\
    }								\
  for (i = 0, nregs = 0; i < 32; i++)				\
    if (regs_ever_live[i] && ! call_used_regs[i])		\
      fprintf (FILE, "\tst.l %s,%d(sp)\n",			\
	       reg_names[i], 4 * nregs++);			\
  for (i = 32; i < 64; i++)					\
    if (regs_ever_live[i] && ! call_used_regs[i])		\
      fprintf (FILE, "\tfst.l %s,%d(sp)\n",			\
	       reg_names[i], 4 * nregs++);			\
}
/* ??? maybe save pairs or quads of fp registers.  */

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   abort ();

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

/* #define EXIT_IGNORE_STACK 0 */

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, SIZE)				\
{								\
  extern char call_used_regs[];					\
  int fsize = (SIZE);						\
  int nregs, i;							\
  for (i = 0, nregs = 0; i < FIRST_PSEUDO_REGISTER; i++)	\
    {								\
      if (regs_ever_live[i] && ! call_used_regs[i])		\
        nregs++;						\
    }								\
  fsize += nregs * 4 + 8;					\
  fsize = (fsize + 15) & -16;					\
  if (fsize < 0x7fff)						\
    {								\
      for (i = 0, nregs = 0; i < 32; i++)			\
	if (regs_ever_live[i] && ! call_used_regs[i])		\
	  fprintf (FILE, "\tld.l %d(fp),%s\n",			\
		   4 * nregs++ - (fsize - 8), reg_names[i]);	\
      for (i = 32; i < 64; i++)					\
	if (regs_ever_live[i] && ! call_used_regs[i])		\
	  fprintf (FILE, "\tfld.l %d(fp),%s\n",			\
		   4 * nregs++ - (fsize - 8), reg_names[i]);	\
    }								\
  else								\
    {								\
      fprintf (FILE, "\torh %d,r0,r31\n", (fsize - 8) >> 16);	\
      fprintf (FILE, "\tor %d,r31,r31\n", (fsize - 8) & 0xffff); \
      fprintf (FILE, "\tsubs fp,r31,sp\n");			\
      for (i = 0, nregs = 0; i < 32; i++)			\
	if (regs_ever_live[i] && ! call_used_regs[i])		\
	  fprintf (FILE, "\tld.l %d(sp),%s\n",			\
		   4 * nregs++, reg_names[i]);			\
      for (i = 32; i < 64; i++)					\
	if (regs_ever_live[i] && ! call_used_regs[i])		\
	  fprintf (FILE, "\tfld.l %d(sp),%s\n",			\
		   4 * nregs++, reg_names[i]);			\
    }								\
  if (fsize < 0x7fff)						\
    {								\
      fprintf (FILE, "\tld.l 4(fp),r1\n");			\
      fprintf (FILE, "\tld.l 0(fp),fp\n");			\
      fprintf (FILE, "\tbri r1\n\taddu %d,sp,sp\n", fsize);	\
    }								\
  else								\
    {								\
      fprintf (FILE, "\tld.l 4(fp),r1\n");			\
      fprintf (FILE, "\tadds 8,fp,r31\n");			\
      fprintf (FILE, "\tld.l 0(fp),fp\n");			\
      fprintf (FILE, "\tbri r1\n\tmov r31,sp\n");		\
    }								\
}

/* If the memory address ADDR is relative to the frame pointer,
   correct it to be relative to the stack pointer instead.
   This is for when we don't use a frame pointer.
   ADDR should be a variable name.  */

#define FIX_FRAME_POINTER_ADDRESS(ADDR,DEPTH)  abort ();

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

   These macros are specific to the i860, and may be used only
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

   On the Sparc, this is anything but a CONST_DOUBLE.
   Let's try permitting CONST_DOUBLEs and see what happens.  */

#define LEGITIMATE_CONSTANT_P(X) 1

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
#define REG_OK_FOR_INDEX_P(X) (((unsigned) REGNO (X)) - 32 >= 14)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (((unsigned) REGNO (X)) - 32 >= 14)

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

   On the i860, the actual addresses must be REG+REG or REG+SMALLINT.
   But we can treat a SYMBOL_REF as legitimate if it is part of this
   function's constant-pool, because such addresses can actually
   be output as REG+SMALLINT.

   The displacement in an address must be a multiple of the alignment.

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
	  if (GET_CODE (XEXP (X, 1)) == CONST_INT	\
	      && INTVAL (XEXP (X, 1)) >= -0x8000	\
	      && INTVAL (XEXP (X, 1)) < 0x8000		\
	      && INTVAL (XEXP (X, 1)) & (GET_MODE_SIZE (MODE) - 1) == 0) \
	    goto ADDR;					\
	}						\
      else if (GET_CODE (XEXP (X, 1)) == REG		\
	  && REG_OK_FOR_BASE_P (XEXP (X, 1)))		\
	{						\
	  if (GET_CODE (XEXP (X, 0)) == CONST_INT	\
	      && INTVAL (XEXP (X, 0)) >= -0x8000	\
	      && INTVAL (XEXP (X, 0)) < 0x8000		\
	      && INTVAL (XEXP (X, 0)) & (GET_MODE_SIZE (MODE) - 1) == 0) \
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

/* On the i860, change COMPLICATED + CONSTANT to REG+CONSTANT.
   Also change a symbolic constant to a REG,
   though that may not be necessary.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)	\
{ if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   force_operand (XEXP (X, 0), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == MULT)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == PLUS)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   force_operand (XEXP (X, 0), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) == PLUS)	\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   force_operand (XEXP (X, 1), 0));		\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) != REG	\
      && GET_CODE (XEXP (X, 0)) != CONST_INT)			\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 1),			\
		   copy_to_mode_reg (SImode, XEXP (X, 0)));	\
  if (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 1)) != REG	\
      && GET_CODE (XEXP (X, 1)) != CONST_INT)			\
    (X) = gen_rtx (PLUS, SImode, XEXP (X, 0),			\
		   copy_to_mode_reg (SImode, XEXP (X, 1)));	\
  if (GET_CODE (x) == SYMBOL_REF)				\
    (X) = copy_to_reg (X);					\
  if (GET_CODE (x) == CONST)					\
    (X) = copy_to_reg (X);					\
  if (memory_address_p (MODE, X))				\
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the i860 this is never true.
   There are some addresses that are invalid in wide modes
   but valid for narrower modes, but they shouldn't cause trouble.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/* On the 860, every legit address is offsettable,
   but GCC would have trouble figuring this out.  */

#define OFFSETTABLE_ADDRESS_P(MODE, ADDR) (memory_address_p ((MODE), (ADDR)))

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

#define DIVSI3_LIBCALL "*.div"
#define UDIVSI3_LIBCALL "*.udiv"
#define REMSI3_LIBCALL "*.rem"
#define UREMSI3_LIBCALL "*.urem"

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 16

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* This is System V, so it wants sdb format.  */
#define DBX_DEBUGGING_INFO

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

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE) \
  case CONST_INT:						\
    if (INTVAL (RTX) == 0)					\
      return 0;							\
    if (INTVAL (RTX) < 0x2000 && INTVAL (RTX) >= -0x2000) return 1; \
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 2;							\
  case CONST_DOUBLE:						\
    return 2 * GET_MODE_SIZE (GET_MODE (RTX)) / UNITS_PER_WORD;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* This holds the value sourcing h%r31.  We keep this info
   around so that mem/mem ops, such as increment and decrement,
   etc, can be performed reasonably.  */
#define CC_STATUS_MDEP rtx

#define CC_STATUS_MDEP_INIT (cc_status.mdep = 0)

/* On the i860, each comparison tests just one condition,
   so only that condition can be remembered.
   We don't need GT, GE, GTU and GEU because CC_REVERSED can handle them.  */
#define CC_ONLY_EQ 0100
#define CC_ONLY_LE 0200
#define CC_ONLY_LT 0400
#define CC_ONLY_LEU 02000
#define CC_ONLY_LTU 04000
#define CC_CONDITION_MASK 07700

/* Non-zero to invert the sense of the condition code.  */
#define CC_NEGATED 010000

/* Nonzero if we know the value of h%r31.  */
#define CC_KNOW_HI_R31 0100000

/* Nonzero if h%r31 is actually ha%something, rather than h%something.  */
#define CC_HI_R31_ADJ 0200000

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* On the i860, only compare insns set a useful condition code.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{ cc_status.flags &= (CC_KNOW_HI_R31 | CC_HI_R31_ADJ);	\
  cc_status.value1 = 0; cc_status.value2 = 0; }

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */
/* The .file command should always begin the output.  */

#define ASM_FILE_START(FILE)
#if 0
#define ASM_FILE_START(FILE)				\
  do { sdbout_filename ((FILE), main_input_filename);	\
       if (optimize) ASM_FILE_START_1 (FILE);		\
     } while (0)
#endif

#define ASM_FILE_START_1(FILE)

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
{"r0", "r1", "sp", "fp", "r4", "r5", "r6", "r7", "r8", "r9",		\
 "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19",	\
 "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29",	\
 "r30", "r31",								\
 "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", "f8", "f9",		\
 "f10", "f11", "f12", "f13", "f14", "f15", "f16", "f17", "f18", "f19",	\
 "f20", "f21", "f22", "f23", "f24", "f25", "f26", "f27", "f28", "f29",	\
 "f30", "f31" }

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* Likewise, for function names.  The difference is that we output a no-op
   just before the beginning of the function, to ensure that there does not
   appear to be a delayed branch there.
   Such a thing would confuse interrupt recovery.  */
#define ASM_DECLARE_FUNCTION_NAME(FILE,NAME,DECL) \
  do { fprintf (FILE, "\tnop\n"); ASM_OUTPUT_LABEL (FILE,NAME); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs (".globl ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, ".%s%d:\n", PREFIX, NUM)

/* This is how to output an internal numbered label which
   labels a jump table.  */

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,JUMPTABLE)	\
  fprintf (FILE, ".data\n\t.align 4\n.%s%d:\n", PREFIX, NUM)

/* Output at the end of a jump table.  */

#define ASM_OUTPUT_CASE_END(FILE,NUM,INSN)	\
  fprintf (FILE, ".text\n")

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*.%s%d", PREFIX, NUM)

#define ASCII_DATA_ASM_OP ".byte"
#define	ASM_OUTPUT_ASCII(f, p, size)	\
{ register int i;			\
  int inside;				\
  inside = FALSE;			\
  for (i = 0; i < size; i++) {		\
    if (i % 64 == 0) {			\
      if (i != 0) {			\
	if (inside)			\
	  putc('"', f);			\
	putc('\n', f);			\
	inside = FALSE;			\
      }					\
      fprintf(f, "%s ", ASCII_DATA_ASM_OP);	\
    }					\
    if (p[i] < 32 || p[i] == '\\' || p[i] == '"' || p[i] == 127) {	\
      if (inside) {			\
	putc('"', f);			\
	inside = FALSE;			\
      }					\
      if (i % 64 != 0)			\
	putc(',', f);			\
      fprintf(f, "%d", p[i]);		\
    } else {				\
      if (!inside) {			\
	if (i % 64 != 0)			\
	  putc(',', f);			\
	putc('"', f);			\
	inside = TRUE;			\
      }					\
      putc(p[i], f);			\
    }					\
  }					\
  if (inside)				\
    putc('"', f);			\
  putc('\n', f);			\
}

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\t.double %.20e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\t.float %.12e\n", (VALUE))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.short "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output code to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\taddu -16,r3,r3\n\t%sst.l %s,0(r3)\n",	\
	   ((REGNO) < 32 ? "" : "f"), reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\t%sld.l 0(r3),%s\n\taddu 16,r3,r3\n",	\
	   ((REGNO) < 32 ? "" : "f"), reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.long .L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   (The i860 does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\t.word .L%d-.L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\t.align %d\n", 1 << (LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.blkb %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".lcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

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

/* This assumes the compiler is running on a little-endian machine.
   The support for the other case is left for version 2,
   since there is nothing in version 1 to indicate the sex of the host.  */

#define PRINT_OPERAND_EXTRACT_FLOAT(X)					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   On the i860, the CODE can be `r', meaning this is a register-only operand
   and an immediate zero should be represented as `r0'.
   It can also be `m', meaning this is a memory ref,
   but print its address as a constant.  */

#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if ((CODE) == 'm')						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if ((CODE) == 'r' && (X) == const0_rtx)				\
    fprintf (FILE, "r0");						\
  else if ((CODE) == 'r' && (X) == CONST0_RTX (GET_MODE (X)))		\
    fprintf (FILE, "f0");						\
  else if (GET_CODE (X) == CONST_DOUBLE)				\
    {									\
      if (GET_MODE (X) == SFmode)					\
	{ union { double d; int i[2]; } u;				\
	  union { float f; int i; } u1;					\
	  PRINT_OPERAND_EXTRACT_FLOAT (X);				\
	  u1.f = u.d;							\
          fprintf (FILE, "0x%x", u1.i); }				\
      else								\
        abort ();							\
      }									\
  else									\
    output_addr_const (FILE, X); }

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx base, index = 0;					\
  int offset = 0;						\
  register rtx addr = ADDR;					\
  if (GET_CODE (addr) == REG)					\
    {								\
      fprintf (FILE, "0(%s)", reg_names[REGNO (addr)]);		\
    }								\
  else if (GET_CODE (addr) == PLUS)				\
    {								\
      if (GET_CODE (XEXP (addr, 0)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 0)), base = XEXP (addr, 1);\
      else if (GET_CODE (XEXP (addr, 1)) == CONST_INT)		\
	offset = INTVAL (XEXP (addr, 1)), base = XEXP (addr, 0);\
      else							\
	base = XEXP (addr, 0), index = XEXP (addr, 1);		\
      if (index != 0)						\
	fprintf (FILE, "%s", reg_names[REGNO (index)]);		\
      else							\
	fprintf (FILE, "%d", offset);				\
      fprintf (FILE, "(%s)", reg_names[REGNO (base)]);		\
    }								\
  else								\
    {								\
/* ??? this may be wrong.  */  \
      output_addr_const (FILE, addr);				\
    }								\
}
