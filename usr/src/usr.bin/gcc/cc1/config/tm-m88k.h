/* Definitions of target machine for GNU compiler, for the Motorola 88000 chip.
   Copyright (C) 1988 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com)

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

#define CPP_PREDEFINES "-Dm88000 -Dm88k"

/* Print subsidiary information on the compiler version in use.  */
#define TARGET_VERSION fprintf (stderr, " (88k)");

/* Run-time compilation parameters selecting different hardware subsets.

   On the the m88000, we don't yet need any.  */

extern int target_flags;

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  {{ "", TARGET_DEFAULT}}

#define TARGET_DEFAULT 1

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the m88000.  */
#define BYTES_BIG_ENDIAN

/* Define this if most significant word of a multiword number is numbered.  */
/* For the m88000 we can decide arbitrarily
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

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 32

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 64

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.

   the m88000 has 32 fullword registers.  */

#define FIRST_PSEUDO_REGISTER 32

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.

   On the 88000, these are:
   Reg 0	= 0 (hardware).
   Reg 1	= Subroutine return pointer (hardware).
   [Reg 2-9	= Parameter registers (Motorola convention).]
   Reg 25	= condition code register (Gnu).
   Reg 26-29	= reserved by Motorola.
   Reg 30 = frame pointer (software).
   Reg 31 = stack pointer (software).  */
#define FIXED_REGISTERS  \
 {1, 1, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 1, 1, 1, 1, 1, 0, 1}

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS  \
 {1, 1, 1, 1, 1, 1, 1, 1, \
  1, 1, 1, 1, 1, 1, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 1, 1, 1, 1, 1, 0, 1}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the m88000, ordinary registers hold 32 bits worth;
   a single floating point register is always enough for
   anything that can be stored in them at all.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the m88000, the cpu registers can hold any mode, but doubles
   (and larger) must start and an even register number boundary.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (GET_MODE_SIZE (MODE) <= 4 || ((REGNO) & 1) == 0)

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2) \
  (((MODE1) == DFmode || (MODE1) == DImode) \
   == ((MODE2) == DFmode || (MODE2) == DImode))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* the m88000 pc isn't overloaded on a register that the compiler knows about.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 31

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 30

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 30

/* Register in which static-chain is passed to a function.  */
/* ??? */
#define STATIC_CHAIN_REGNUM 10

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 2
#define STRUCT_VALUE_STACK_PROTECT_REGNUM 3

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
   
/* The 88000 has one kind of registers, hence two classes.  */

enum reg_class { NO_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias.  */

#define GENERAL_REGS ALL_REGS

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES {"NO_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {0, -1}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) ALL_REGS

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS ALL_REGS
#define BASE_REG_CLASS ALL_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C) NO_REGS

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For the m88000, `I' is used for the range of constants an insn
   can actually contain.
   `J' is used for the range which is just zero (since that is R0).
   `K' is used for the 5-bit operand of a compare insns.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? (unsigned) (VALUE) < 0x10000		\
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
#define CLASS_MAX_NREGS(CLASS, MODE)	\
  ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.

   Do not define this for the Motorola 88000.  There are no
   negative literals!  */
/* #define FRAME_GROWS_DOWNWARD */

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.
   On the m88000, don't define this because there are no push insns.  */
/*  #define PUSH_ROUNDING(BYTES) */

/* If BYTES is the size of arguments for a function call,
   return the size of the argument block (which is BYTES suitably rounded).
   Define this only on machines where the entire call block is allocated
   before the args are stored into it.  */
   
#define ROUND_CALL_BLOCK_SIZE(BYTES)  \
   (((BYTES) + 7) & ~7)

/* Offset of first parameter from the argument pointer register value.  */
/* For the 88000, this must be non-zero so that addresses of the parms
   can always be distinguished.  */
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

/* ?? On the m88000 the value is found in the second "output" register.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), 2)

/* ?? But the called function leaves it in the second "input" register.  */

#define FUNCTION_OUTGOING_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), 2)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, 2)

/* 1 if N is a possible register number for a function value
   as seen by the caller.
   On the m88000, the first "output" reg is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 2)

/* 1 if N is a possible register number for function argument passing.
   On the m88000, these are the "output" registers.  */

#define FUNCTION_ARG_REGNO_P(N) ((N) <= 9 && (N) >= 2)

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the m88000, this is a single integer, which is a number of words
   of arguments scanned so far (including the invisible argument,
   if any, which holds the structure-value-address).
   Thus 8 or more means all following args should go on the stack.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the m88000, the offset normally starts at 0, but starts at 4 bytes
   when the function gets a structure-value-address as an
   invisible first argument.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE)	\
 ((CUM) = ((FNTYPE) != 0 && aggregate_value_p ((FNTYPE))))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE) + 3) / 4	\
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

/* On the m88000 the first eight words of args are normally in registers
   and the rest are pushed.  But any arg that won't entirely fit in regs
   is pushed.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)		\
(8 >= ((CUM)						\
       + ((MODE) == BLKmode				\
	  ? (int_size_in_bytes (TYPE) + 3) / 4		\
	  : (GET_MODE_SIZE (MODE) + 3) / 4))		\
 ? gen_rtx (REG, (MODE), 2 + (CUM))			\
 : 0)

/* Define where a function finds its arguments.
   This would be different from FUNCTION_ARG if we had register windows.  */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)	\
  FUNCTION_ARG (CUM, MODE, TYPE, NAMED)

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
  extern int current_function_pretend_args_size;		\
  extern int frame_pointer_needed;				\
  int fsize = ((SIZE) + current_function_pretend_args_size + 7) & ~7;	\
  int regno, nregs, i;						\
  int offset = 0;						\
  for (regno = 2, nregs = 0; regno < FRAME_POINTER_REGNUM; regno++)	\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      nregs++;							\
  nregs = (nregs + 1) & ~1;					\
  if (regs_ever_live[1] + frame_pointer_needed + nregs)		\
    {								\
      if (fsize + 8 + nregs*4 < 0x10000)			\
	offset = fsize;						\
      fprintf (FILE, "\tsub r31,r31,%d\n", 8 + nregs*4 + offset);	\
    }								\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tst r30,r31,%d\n", offset);		\
  if (regs_ever_live[1])					\
    fprintf (FILE, "\tst r1,r31,%d\n", 4 + offset);		\
  if (nregs)							\
    for (regno = 2, nregs = 2; regno < FRAME_POINTER_REGNUM; regno++)	\
      if (regs_ever_live[regno] && ! call_used_regs[regno])	\
	if (regno & 1 || !regs_ever_live[regno+1] || call_used_regs[regno+1])\
	  fprintf (FILE, "\tst r%d,r31,%d\n", regno, offset + nregs++ * 4);\
	else							\
	  {							\
	    fprintf (FILE, "\tst.d r%d,r31,%d\n", regno, offset + nregs * 4);\
	    regno += 1; nregs += 2;				\
	  }							\
  if (offset || fsize == 0) /* do nothing.  */ ;		\
  else if ((unsigned) fsize < 0x10000)				\
    fprintf (FILE, "\tsub r31,r31,%d\n", fsize);		\
  else fprintf (FILE, "\tor.u r25,r0,hi16(%d)\n\tor r25,r0,lo16(%d)\n\tsub r31,r31,r25\n", fsize, fsize); \
  if (frame_pointer_needed) fprintf (FILE, "\tor r30,r0,r31\n");	\
}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   abort ();

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

#define FUNCTION_EPILOGUE(FILE, SIZE)				\
{								\
  extern char call_used_regs[];					\
  extern int may_call_alloca;					\
  int fsize = ((SIZE) + current_function_pretend_args_size + 7) & ~7;	\
  int nregs, regno, i;						\
  for (regno = 2, nregs = 0; regno < FRAME_POINTER_REGNUM; regno++) \
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      nregs++;							\
  if (frame_pointer_needed)					\
    {								\
      if ((unsigned) fsize < 0x10000)				\
	fprintf (FILE, "\tadd r31,r30,%d\n", fsize);		\
      else fprintf (FILE, "\tor.u r25,r0,hi16(%d)\n\tor r25,r0,lo16(%d)\n\tadd r31,r30,r25\n", fsize, fsize); \
    }								\
  else if (fsize) fprintf (FILE, "\tadd r31,r31,%d\n", fsize);	\
  if (nregs)							\
    for (regno = 2, nregs = 2; regno < FRAME_POINTER_REGNUM; regno++) \
      if (regs_ever_live[regno] && ! call_used_regs[regno])	\
	if (regno & 1 || !regs_ever_live[regno+1] || call_used_regs[regno+1])\
	  fprintf (FILE, "\tld r%d,r31,%d\n", regno, nregs++ * 4);\
	else							\
	  {							\
	    fprintf (FILE, "\tld.d r%d,r31,%d\n", regno, nregs * 4);\
	    regno += 1; nregs += 2;				\
	  }							\
  if (regs_ever_live[1])					\
    fprintf (FILE, "\tld r1,r31,4\n");				\
  else								\
    fprintf (FILE, ";; r1 is set to go!\n");			\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tld r30,r31,0\n");				\
  nregs = (nregs + 1) & ~1;					\
  if (regs_ever_live[1] + frame_pointer_needed + (nregs > 2))	\
    fprintf (FILE, "\tjmp.n r1\n\taddu r31,r31,%d\n", nregs * 4);	\
  else fprintf (FILE, "\tjmp r1\n");				\
  /* let insn reorganizer know that we are at the end of a function.  */ \
  fprintf (FILE, "\tdata\n");					\
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
      for (regno = 2; regno < FRAME_POINTER_REGNUM; regno++)		\
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
#define REGNO_OK_FOR_BASE_P(REGNO)  \
  ((REGNO) < 32 || (unsigned) reg_renumber[REGNO] < 32)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the the m88000, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)  CONSTANT_P (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) (1)

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
#define REG_OK_FOR_INDEX_P(X) (1)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (1)

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

   On the m88000, the actual legitimate addresses must be REG+REG or REG+SMALLINT.
   But we can treat a SYMBOL_REF as legitimate if it is part of this
   function's constant-pool, because such addresses can actually
   be output as REG+SMALLINT.  */

#define INT_FITS_16_BITS(I) ((unsigned) (I) < 0x10000)

#define FITS_16_BITS(X)	\
   (GET_CODE (X) == CONST_INT && INT_FITS_16_BITS (INTVAL (X)))

#define LEGITIMATE_INDEX_P(X, MODE)   \
   (FITS_16_BITS (X)					\
    || (REG_P (X)					\
	&& REG_OK_FOR_INDEX_P (X))			\
    || (GET_CODE (X) == MULT				\
	&& REG_P (XEXP (X, 0))				\
	&& REG_OK_FOR_INDEX_P (XEXP (X, 0))		\
	&& GET_CODE (XEXP (X, 1)) == CONST_INT		\
	&& (INTVAL (XEXP (X, 1)) == GET_MODE_SIZE (MODE)))	\
    || (GET_CODE (X) == MULT				\
	&& REG_P (XEXP (X, 1))				\
	&& REG_OK_FOR_INDEX_P (XEXP (X, 1))		\
	&& GET_CODE (XEXP (X, 0)) == CONST_INT		\
	&& (INTVAL (XEXP (X, 0)) == GET_MODE_SIZE (MODE))	\
        && (warning ("MULT backwards"), 1)))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{							\
  if (GET_CODE (X) == CONST_INT)			\
    {							\
      if (FITS_16_BITS (X))				\
	goto ADDR;					\
    }							\
  else if (CONSTANT_ADDRESS_P (X))			\
    goto ADDR;						\
  else if (REG_P (X))					\
    {							\
      if (REG_OK_FOR_BASE_P (X))			\
	goto ADDR;					\
    }							\
  else if (GET_CODE (X) == PLUS)			\
    if (REG_P (XEXP (X, 0))				\
	&& REG_OK_FOR_BASE_P (XEXP (X, 0)))		\
      {							\
	if (LEGITIMATE_INDEX_P (XEXP (X, 1), MODE))	\
	  goto ADDR;					\
      }							\
    else if (REG_P (XEXP (X, 1))			\
	     && REG_OK_FOR_BASE_P (XEXP (X, 1)))	\
      {							\
	if (LEGITIMATE_INDEX_P (XEXP (X, 0), MODE))	\
	  goto ADDR;					\
      }							\
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

/* On the m88000, change REG+N into REG+REG, and REG+(X*Y) into REG+REG.  */

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
  if (memory_address_p (MODE, X))				\
    goto WIN; }

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the the m88000 this is never true.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if a raw index is all that is needed for a
   `tablejump' insn.  */
#define CASE_TAKES_INDEX_RAW

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

/* Do not break .stabs pseudos into continuations.  */
#define DBX_CONTIN_LENGTH 0

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE 1

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
    if ((unsigned) INTVAL (RTX) < 0x10000) return 1;		\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 2;							\
  case CONST_DOUBLE:						\
    return 4;

/* Tell emit-rtl.c how to initialize special values on a per-function bass.  */
extern int optimize;
extern struct rtx_def *cc0_reg_rtx;

typedef struct { struct rtx_def *ccr; } cc_status_mdep;
#define CC_STATUS_MDEP cc_status_mdep

#define INIT_EMIT_MDEP \
{								\
  cc0_reg_rtx = gen_rtx (REG, SImode, 25);			\
}

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

#define CC_IN_FCCR 04000

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{ if (GET_CODE (EXP) == SET)					\
    { if (GET_CODE (SET_DEST (EXP)) == CC0)			\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (EXP);			\
	  cc_status.value2 = SET_SRC (EXP);			\
	}							\
      else if (GET_CODE (SET_DEST (EXP)) == REG)		\
	{ if ((cc_status.value1					\
	       && reg_overlap_mentioned_p (SET_DEST (EXP), cc_status.value1))) \
	    cc_status.value1 = 0;				\
	  if ((cc_status.value2					\
	      && reg_overlap_mentioned_p (SET_DEST (EXP), cc_status.value2))) \
	    cc_status.value2 = 0;				\
	}							\
      else if (GET_CODE (SET_DEST (EXP)) == MEM)		\
	{ CC_STATUS_INIT; }					\
    }								\
  else if (GET_CODE (EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP (EXP, 0, 0)) == SET)		\
    { if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) == CC0)	\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = SET_DEST (XVECEXP (EXP, 0, 0));	\
	  cc_status.value2 = SET_SRC (XVECEXP (EXP, 0, 0));	\
	}							\
      else if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) == REG) \
	{ if ((cc_status.value1					\
	       && reg_overlap_mentioned_p (SET_DEST (XVECEXP (EXP, 0, 0)), cc_status.value1))) \
	    cc_status.value1 = 0;				\
	  if ((cc_status.value2					\
	       && reg_overlap_mentioned_p (SET_DEST (XVECEXP (EXP, 0, 0)), cc_status.value2))) \
	    cc_status.value2 = 0;				\
	}							\
      else if (GET_CODE (SET_DEST (XVECEXP (EXP, 0, 0))) == MEM) \
	{ CC_STATUS_INIT; }					\
    }								\
  else if (GET_CODE (EXP) == CALL)				\
    { /* all bets are off */ CC_STATUS_INIT; }			\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	\
      && cc_status.value2					\
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))	\
    printf ("here!\n", cc_status.value2 = 0);			\
}

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE)

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ""

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ""

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP "\ttext"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP "\tdata"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9",		\
 "r10", "r11", "r12", "r13", "r14", "r15", "r16", "r17", "r18", "r19",	\
 "r20", "r21", "r22", "r23", "r24", "r25", "r26", "r27", "r28", "r29",	\
 "r30", "r31"}

/* How to renumber registers for dbx and gdb.  */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs ("\tglobal\t", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "@%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*@%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\tdouble %.20e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\tfloat %.12e\n", (VALUE))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\tword "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `short' and `char' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\thalf "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\tbyte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\tbyte 0x%x\n", (VALUE))

#define ASM_OUTPUT_ASCII(FILE, P, SIZE)  \
  output_ascii (FILE, P, SIZE)

#define ASM_OUTPUT_ADDR_VEC_PROLOGUE(FILE, MODE, LEN)	\
  fprintf (FILE, "\tjmp r1\n");

/* This is how to output an element of a case-vector that is absolute.  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t@L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.
   (the m88000 does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\tword @L%d-@L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) != 0)			\
    fprintf (FILE, "\talign %d\n", 1<<(LOG))

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\tzero %u\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs ("\tcomm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( fprintf ((FILE), "\talign %d\n", (SIZE) <= 4 ? 4 : 8),	\
  assemble_name ((FILE), (NAME)),				\
  fprintf ((FILE), ":\n\tzero %u\n", (ROUNDED)))

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

   On the m88000, the CODE can be `r', meaning this is a register-only operand
   and an immediate zero should be represented as `r0'.  */

#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { union { double d; int i[2]; } u;					\
      union { float f; int i; } u1;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      u1.f = u.d;							\
      if (CODE == 'f')							\
	fprintf (FILE, "0r%.9g", u1.f);					\
      else								\
	fprintf (FILE, "0x%x", u1.i); }					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != DImode)	\
    { union { double d; int i[2]; } u;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      fprintf (FILE, "0r%.20g", u.d); }					\
  else if ((CODE) == 'r' && (X) == const0_rtx)				\
    fprintf (FILE, "r0");						\
  else { output_addr_const (FILE, X); }}

/* Print a memory address as an operand to reference that memory location.  */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx base, index = 0;						\
  register rtx addr = ADDR;						\
  register rtx reg0, reg1;						\
  switch (GET_CODE (addr))						\
    {									\
    case REG:								\
      fprintf (FILE, "r0,%s", reg_names[REGNO (addr)]);			\
      break;								\
    case PLUS:								\
      reg0 = XEXP (addr, 0);						\
      reg1 = XEXP (addr, 1);						\
      if (GET_CODE (reg0) == MULT)					\
	{ rtx tmp = reg0; reg0 = reg1; reg1 = tmp; }			\
      if (REG_P (reg0))							\
	if (REG_P (reg1))						\
	  fprintf (FILE, "%s,%s",					\
		   reg_names[REGNO (reg0)],				\
		   reg_names[REGNO (reg1)]);				\
	else if (GET_CODE (reg1) == CONST_INT)				\
	  {								\
	    int offset = INTVAL (reg1);					\
	    fprintf (FILE, "%s,%d", reg_names[REGNO (reg0)], offset);	\
	  }								\
	else if (GET_CODE (reg1) == MULT)				\
	  fprintf (FILE, "%s[%s]",					\
		   reg_names[REGNO (reg0)],				\
		   reg_names[REGNO (XEXP (reg1, 0))]);			\
	else fatal ("bad XEXP (1) to PRINT_OPERAND_ADDRESS");		\
      else fatal ("unknown PLUS case in PRINT_OPERAND_ADDRESS");	\
      break;								\
    case MULT:								\
      fprintf (FILE, "r0[%s]", reg_names[REGNO (XEXP (addr, 0))]);	\
      break;								\
    default:								\
      fprintf (FILE, "r0,");						\
      output_addr_const (FILE, addr);					\
    }}

