/* Definitions of target machine for GNU compiler.  Sun 68000/68020 version.
   Copyright (C) 1987, 1988 Free Software Foundation, Inc.

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

/* See tm-sun3.h, tm-sun2.h, tm-isi68.h for different CPP_PREDEFINES.  */

/* Print subsidiary information on the compiler version in use.  */
#ifdef MOTOROLA
#define TARGET_VERSION fprintf (stderr, " (68k, Motorola syntax)");
#else
#define TARGET_VERSION fprintf (stderr, " (68k, MIT syntax)");
#endif

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* Compile for a 68020 (not a 68000 or 68010).  */
#define TARGET_68020 (target_flags & 1)
/* Compile 68881 insns for floating point (not library calls).  */
#define TARGET_68881 (target_flags & 2)
/* Compile using 68020 bitfield insns.  */
#define TARGET_BITFIELD (target_flags & 4)
/* Compile using rtd insn calling sequence.
   This will not work unless you use prototypes at least
   for all functions that can take varying numbers of args.  */
#define TARGET_RTD (target_flags & 8)
/* Compile passing first two args in regs 0 and 1.
   This exists only to test compiler features that will
   be needed for RISC chips.  It is not usable
   and is not intended to be usable on this cpu.  */
#define TARGET_REGPARM (target_flags & 020)
/* Compile with 16-bit `int'.  */
#define TARGET_SHORT (target_flags & 040)

/* Compile with special insns for Sun FPA.  */
#define TARGET_FPA (target_flags & 0100)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES  \
  { { "68020", 5},				\
    { "c68020", 5},				\
    { "68881", 2},				\
    { "bitfield", 4},				\
    { "68000", -5},				\
    { "c68000", -5},				\
    { "soft-float", -0102},			\
    { "nobitfield", -4},			\
    { "rtd", 8},				\
    { "nortd", -8},				\
    { "short", 040},				\
    { "noshort", -040},				\
    { "fpa", 0100},				\
    { "nofpa", -0100},				\
    { "", TARGET_DEFAULT}}
/* TARGET_DEFAULT is defined in tm-sun*.h and tm-isi68.h, etc.  */

/* Blow away 68881 flag silently on TARGET_FPA (since we can't clear
   any bits in TARGET_SWITCHES above) */
#define OVERRIDE_OPTIONS		\
{					\
  if (TARGET_FPA) target_flags &= ~2;	\
}

/* target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.
   This is true for 68020 insns such as bfins and bfexts.
   We make it true always by avoiding using the single-bit insns
   except in special cases with constant bit numbers.  */
#define BITS_BIG_ENDIAN

/* Define this if most significant byte of a word is the lowest numbered.  */
/* That is true on the 68000.  */
#define BYTES_BIG_ENDIAN

/* Define this if most significant word of a multiword number is numbered.  */
/* For 68000 we can decide arbitrarily
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
#define POINTER_BOUNDARY 16

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */
#define PARM_BOUNDARY (TARGET_SHORT ? 16 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */
#define STACK_BOUNDARY 16

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 16

/* No data type wants to be aligned rounder than this.  */
#define BIGGEST_ALIGNMENT 16

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT

/* Define number of bits in most basic integer type.
   (If undefined, default is BITS_PER_WORD).  */

#define INT_TYPE_SIZE (TARGET_SHORT ? 16 : 32)

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.
   For the 68000, we give the data registers numbers 0-7,
   the address registers numbers 010-017,
   and the 68881 floating point registers numbers 020-027.  */
#define FIRST_PSEUDO_REGISTER 56 /* 24 */

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   On the 68000, only the stack pointer is such.  */
/* fpa0 is also reserved so that it can be used to move shit back and
   forth between high fpa regs and everything else. */
#define FIXED_REGISTERS  \
 {0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 1, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  /* FPA registers.  */   \
  1, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, }

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS \
 {1, 1, 0, 0, 0, 0, 0, 0, \
  1, 1, 0, 0, 0, 0, 0, 1, \
  1, 1, 0, 0, 0, 0, 0, 0, \
  /* FPA registers.  */   \
  1, 1, 1, 1, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, \
  0, 0, 0, 0, 0, 0, 0, 0, }

/* Make sure everything's fine if we *don't* have a given processor.
   This assumes that putting a register in fixed_regs will keep the
   compilers mitt's completely off it.  We don't bother to zero it out
   of register classes.  If neither TARGET_FPA or TARGET_68881 is set,
   the compiler won't touch since no instructions that use these
   registers will be valid.  */
#define CONDITIONAL_REGISTER_USAGE \
{ 						\
  int i; 					\
  HARD_REG_SET x; 				\
  if (!TARGET_FPA)				\
    { 						\
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FPA_REGS]); \
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ ) \
       if (TEST_HARD_REG_BIT (x, i)) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
    } 						\
  if (TARGET_FPA)				\
    { 						\
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FP_REGS]); \
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ ) \
       if (TEST_HARD_REG_BIT (x, i)) 		\
	fixed_regs[i] = call_used_regs[i] = 1; 	\
    } 						\
}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the 68000, ordinary registers hold 32 bits worth;
   for the 68881 registers, a single register is always enough for
   anything that can be stored in them at all.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   \
  ((REGNO) >= 16 ? 1				\
   : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On the 68000, the cpu registers can hold any mode but the 68881 registers
   can hold only SFmode or DFmode.  And the 68881 registers can't hold anything
   if 68881 use is disabled.  However, the Sun FPA register can
   (apparently) hold whatever you feel like putting in them.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (((REGNO) < 16 &&						\
    (!TARGET_FPA || (MODE) != DFmode || (REGNO) != 7))		\
   || ((REGNO) < 24						\
       ? TARGET_68881 && ((MODE) == SFmode || (MODE) == DFmode)	\
       : ((REGNO) < 56						\
	  ? TARGET_FPA : 0)))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)			\
  (! TARGET_68881					\
   || (((MODE1) == SFmode || (MODE1) == DFmode)		\
       == ((MODE2) == SFmode || (MODE2) == DFmode)))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* m68000 pc isn't overloaded on a register.  */
/* #define PC_REGNUM  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 15

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 14

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 0

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 14

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 8

/* Register in which address to store a structure value
   is passed to a function.  */
#define STRUCT_VALUE_REGNUM 9

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

/* The 68000 has three kinds of registers, so eight classes would be
   a complete set.  One of them is not needed.  */

/*
 * Notes on final choices:
 *
 *   1) Didn't feel any need to union-ize LOW_FPA_REGS with anything
 * else.
 *   2) Removed all unions that involve address registers with
 * floating point registers (left in unions of address and data with
 * floating point).
 *   3) Defined GENERAL_REGS as ADDR_OR_DATA_REGS.
 *   4) Defined ALL_REGS as FPA_OR_FP_OR_GENERAL_REGS.
 *   4) Left in everything else.
 */
enum reg_class { NO_REGS, LO_FPA_REGS, FPA_REGS, FP_REGS,
  FP_OR_FPA_REGS, DATA_REGS, DATA_OR_FPA_REGS, DATA_OR_FP_REGS,
  DATA_OR_FP_OR_FPA_REGS, ADDR_REGS, GENERAL_REGS,
  GENERAL_OR_FPA_REGS, GENERAL_OR_FP_REGS, ALL_REGS,
  LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 { "NO_REGS", "LO_FPA_REGS", "FPA_REGS", "FP_REGS",  \
   "FP_OR_FPA_REGS", "DATA_REGS", "DATA_OR_FPA_REGS", "DATA_OR_FP_REGS",  \
   "DATA_OR_FP_OR_FPA_REGS", "ADDR_REGS", "GENERAL_REGS",  \
   "GENERAL_OR_FPA_REGS", "GENERAL_OR_FP_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS \
{							\
 {0, 0},			/* NO_REGS */		\
 {0xff000000, 0x000000ff},	/* LO_FPA_REGS */	\
 {0xff000000, 0x00ffffff},	/* FPA_REGS */		\
 {0x00ff0000, 0x00000000},	/* FP_REGS */		\
 {0xffff0000, 0x00ffffff},	/* FP_OR_FPA_REGS */	\
 {0x000000ff, 0x00000000},	/* DATA_REGS */		\
 {0xff0000ff, 0x00ffffff},	/* DATA_OR_FPA_REGS */	\
 {0x00ff00ff, 0x00000000},	/* DATA_OR_FP_REGS */	\
 {0xffff00ff, 0x00ffffff},	/* DATA_OR_FP_OR_FPA_REGS */\
 {0x0000ff00, 0x00000000},	/* ADDR_REGS */		\
 {0x0000ffff, 0x00000000},	/* GENERAL_REGS */	\
 {0xff00ffff, 0x00ffffff},	/* GENERAL_OR_FPA_REGS */\
 {0x00ffffff, 0x00000000},	/* GENERAL_OR_FP_REGS */\
 {0xffffffff, 0x00ffffff},	/* ALL_REGS */		\
}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

extern enum reg_class regno_reg_class[];
#define REGNO_REG_CLASS(REGNO) (regno_reg_class[(REGNO)>>3])

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS GENERAL_REGS
#define BASE_REG_CLASS ADDR_REGS

/* Get reg_class from a letter such as appears in the machine description.
   We do a trick here to modify the effective constraints on the
   machine description; we zorch the constraint letters that aren't
   appropriate for a specific target.  This allows us to guarantee
   that a specific kind of register will not be used for a given target
   without fiddling with the register classes above. */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'a' ? ADDR_REGS :			\
   ((C) == 'd' ? DATA_REGS :			\
    ((C) == 'f' ? (TARGET_68881 ? FP_REGS :	\
		   NO_REGS) :			\
     ((C) == 'x' ? (TARGET_FPA ? FPA_REGS :	\
		    NO_REGS) :			\
      ((C) == 'y' ? (TARGET_FPA ? LO_FPA_REGS :	\
		     NO_REGS) :			\
       NO_REGS)))))

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.

   For the 68000, `I' is used for the range 1 to 8
   allowed as immediate shift counts and in addq.
   `J' is used for the range of signed numbers that fit in 16 bits.
   `K' is for numbers that moveq can't handle.
   `L' is for range -8 to -1, range of values that can be added with subq.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'I' ? (VALUE) > 0 && (VALUE) <= 8 :    \
   (C) == 'J' ? (VALUE) >= -0x8000 && (VALUE) <= 0x7FFF :	\
   (C) == 'K' ? (VALUE) < -0x80 || (VALUE) >= 0x80 :	\
   (C) == 'L' ? (VALUE) < 0 && (VALUE) >= -8 : 0)

/*
 * A small bit of explanation:
 * "G" defines all of the floating constants that are *NOT* 68881
 * constants.  this is so 68881 constants get reloaded and the
 * fpmovecr is used.  "H" defines *only* the class of constants that
 * the fpa can use, because these can be gotten at in any fpa
 * instruction and there is no need to force reloads.
 */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  \
  ((C) == 'G' ? ! (TARGET_68881 && standard_68881_constant_p (VALUE)) : \
   (C) == 'H' ? (TARGET_FPA && standard_sun_fpa_constant_p (VALUE)) : 0)

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.
   On the 68000 series, use a data reg if possible when the
   value is a constant in the range where moveq could be used
   and we ensure that QImodes are reloaded into data regs.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  \
  ((GET_CODE (X) == CONST_INT			\
    && (unsigned) (INTVAL (X) + 0x80) < 0x100	\
    && (CLASS) != ADDR_REGS)			\
   ? DATA_REGS					\
   : GET_MODE (X) == QImode			\
   ? DATA_REGS					\
   : (CLASS))

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
/* On the 68000, this is the size of MODE in words,
   except in the FP regs, where a single reg is always enough.  */
#define CLASS_MAX_NREGS(CLASS, MODE)	\
 ((CLASS) == FP_REGS || (CLASS) == FPA_REGS || (CLASS) == LO_FPA_REGS ? 1 \
  : ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD))

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
   On the 68000, sp@- in a byte insn really pushes a word.  */
#define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 8

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.

   On the 68000, the RTS insn cannot pop anything.
   On the 68010, the RTD insn may be used to pop them if the number
     of args is fixed, but if the number is variable then the caller
     must pop them all.  RTD can't be used for library calls now
     because the library is compiled with the Unix compiler.
   Use of RTD is a selectable option, since it is incompatible with
   standard Unix calling sequences.  If the option is not selected,
   the caller must always pop the args.  */

#define RETURN_POPS_ARGS(FUNTYPE)   \
  (TARGET_RTD && TREE_CODE (FUNTYPE) != IDENTIFIER_NODE		\
   && (TYPE_ARG_TYPES (FUNTYPE) == 0				\
       || TREE_VALUE (tree_last (TYPE_ARG_TYPES (FUNTYPE))) == void_type_node))

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On the 68000 the return value is in D0 regardless.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On the 68000 the return value is in D0 regardless.  */

#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, 0)

/* 1 if N is a possible register number for a function value.
   On the 68000, d0 is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for function argument passing.
   On the 68000, no registers are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the m68k, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On the m68k, the offset starts at 0.  */

#define INIT_CUMULATIVE_ARGS(CUM,FNTYPE)	\
 ((CUM) = 0)

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
 ((CUM) += ((MODE) != BLKmode			\
	    ? (GET_MODE_SIZE (MODE) + 3) & ~3	\
	    : (int_size_in_bytes (TYPE) + 3) & ~3))

/* Define where to put the arguments to a function.
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

/* On the 68000 all args are pushed, except if -mregparm is specified
   then the first two words of arguments are passed in d0, d1.
   *NOTE* -mregparm does not work.
   It exists only to test register calling conventions.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8) ? gen_rtx (REG, (MODE), (CUM) / 4) : 0)

/* For an arg passed partly in registers and partly in memory,
   this is the number of registers used.
   For args passed entirely in registers or entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
((TARGET_REGPARM && (CUM) < 8					\
  && 8 < ((CUM) + ((MODE) == BLKmode				\
		      ? int_size_in_bytes (TYPE)		\
		      : GET_MODE_SIZE (MODE))))  		\
 ? 2 - (CUM) / 4 : 0)

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

/* Note that the order of the bit mask for fmovem is the opposite
   of the order for movem!  */

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{ register int regno;						\
  register int mask = 0;					\
  extern char call_used_regs[];					\
  int fsize = ((SIZE) + 3) & -4;				\
  if (frame_pointer_needed)					\
    { if (TARGET_68020 || fsize < 0x8000)			\
        fprintf (FILE, "\tlink a6,#%d\n", -fsize);		\
      else							\
	fprintf (FILE, "\tlink a6,#0\n\tsubl #%d,sp\n", fsize); }  \
  for (regno = 24; regno < 56; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      fprintf(FILE, "\tfpmoved %s, sp@-\n",			\
	      reg_names[regno]);				\
  for (regno = 16; regno < 24; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (regno - 16);				\
  if ((mask & 0xff) != 0)					\
    fprintf (FILE, "\tfmovem #0x%x,sp@-\n", mask & 0xff);       \
  mask = 0;							\
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
       mask |= 1 << (15 - regno);				\
  if (frame_pointer_needed)					\
    mask &= ~ (1 << (15-FRAME_POINTER_REGNUM));			\
  if (exact_log2 (mask) >= 0)					\
    fprintf (FILE, "\tmovel %s,sp@-\n", reg_names[15 - exact_log2 (mask)]);  \
  else if (mask) fprintf (FILE, "\tmoveml #0x%x,sp@-\n", mask); }

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\tlea LP%d,a0\n\tjsr mcount\n", (LABELNO))

/* Output assembler code to FILE to initialize this source file's
   basic block profiling info, if that has not already been done.  */

#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)  \
  fprintf (FILE, "\ttstl LPBX0\n\tbne LPI%d\n\tpea LPBX0\n\tjsr ___bb_init_func\n\taddql #4,sp\nLPI%d:\n",  \
	   LABELNO, LABELNO);

/* Output assembler code to FILE to increment the entry-count for
   the BLOCKNO'th basic block in this source file.  */

#define BLOCK_PROFILER(FILE, BLOCKNO)	\
  fprintf (FILE, "\taddql #1,LPBX2+%d\n", 4 * BLOCKNO)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.

   The function epilogue should not depend on the current stack pointer!
   It should use the frame pointer only.  This is mandatory because
   of alloca; we also take advantage of it to omit stack adjustments
   before returning.  */

#define FUNCTION_EPILOGUE(FILE, SIZE) \
{ register int regno;						\
  register int mask, fmask;					\
  register int nregs;						\
  int offset, foffset, fpoffset;				\
  extern char call_used_regs[];					\
  extern int current_function_pops_args;			\
  extern int current_function_args_size;			\
  int fsize = ((SIZE) + 3) & -4;				\
  int big = 0;							\
  FUNCTION_EXTRA_EPILOGUE (FILE, SIZE);				\
  nregs = 0;  fmask = 0; fpoffset = 0;				\
  for (regno = 24 ; regno < 56 ; regno++)			\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      nregs++;							\
  fpoffset = nregs*8;						\
  nregs = 0;							\
  for (regno = 16; regno < 24; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; fmask |= 1 << (23 - regno); }			\
  foffset = fpoffset + nregs * 12;				\
  nregs = 0;  mask = 0;						\
  if (frame_pointer_needed) regs_ever_live[FRAME_POINTER_REGNUM] = 0; \
  for (regno = 0; regno < 16; regno++)				\
    if (regs_ever_live[regno] && ! call_used_regs[regno])	\
      { nregs++; mask |= 1 << regno; }				\
  offset = foffset + nregs * 4;					\
  if (offset + fsize >= 0x8000 					\
      && frame_pointer_needed 					\
      && (mask || fmask || fpoffset)) 				\
    { fprintf (FILE, "\tmovel #%d,a0\n", -fsize);		\
      fsize = 0, big = 1; }					\
  if (exact_log2 (mask) >= 0) {					\
    if (big)							\
      fprintf (FILE, "\tmovel a6@(-%d,a0:l),%s\n",		\
	       offset + fsize, reg_names[exact_log2 (mask)]);	\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tmovel sp@+,%s\n",			\
	       reg_names[exact_log2 (mask)]);			\
    else							\
      fprintf (FILE, "\tmovel a6@(-%d),%s\n",			\
	       offset + fsize, reg_names[exact_log2 (mask)]); }	\
  else if (mask) {						\
    if (big)							\
      fprintf (FILE, "\tmoveml a6@(-%d,a0:l),#0x%x\n",		\
	       offset + fsize, mask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tmoveml sp@+,#0x%x\n", mask);		\
    else							\
      fprintf (FILE, "\tmoveml a6@(-%d),#0x%x\n",		\
	       offset + fsize, mask); }				\
  if (fmask) {							\
    if (big)							\
      fprintf (FILE, "\tfmovem a6@(-%d,a0:l),#0x%x\n",		\
	       foffset + fsize, fmask);				\
    else if (! frame_pointer_needed)				\
      fprintf (FILE, "\tfmovem sp@+,#0x%x\n", fmask);		\
    else							\
      fprintf (FILE, "\tfmovem a6@(-%d),#0x%x\n",		\
	       foffset + fsize, fmask); }			\
  if (fpoffset != 0)						\
    for (regno = 55; regno >= 24; regno--)			\
      if (regs_ever_live[regno] && ! call_used_regs[regno]) {	\
	if (big)						\
	  fprintf(FILE, "\tfpmoved a6@(-%d,a0:l), %s\n",	\
		  fpoffset + fsize, reg_names[regno]);		\
	else if (! frame_pointer_needed)			\
	  fprintf(FILE, "\tfpmoved sp@+, %s\n",			\
		  reg_names[regno]);				\
	else							\
	  fprintf(FILE, "\tfpmoved a6@(-%d), %s\n",		\
		  fpoffset + fsize, reg_names[regno]);		\
	fpoffset -= 8;						\
      }								\
  if (frame_pointer_needed)					\
    fprintf (FILE, "\tunlk a6\n");				\
  if (current_function_pops_args && current_function_args_size)	\
    fprintf (FILE, "\trtd #%d\n", current_function_args_size);	\
  else fprintf (FILE, "\trts\n"); }

/* This is a hook for other tm files to change.  */
#define FUNCTION_EXTRA_EPILOGUE(FILE, SIZE)

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
  else if (GET_CODE (ADDR) == PLUS					\
	   && GET_CODE (XEXP (ADDR, 0)) == PLUS				\
	   && XEXP (XEXP (ADDR, 0), 0) == frame_pointer_rtx		\
	   && GET_CODE (XEXP (ADDR, 1)) == CONST_INT)			\
    { rtx other_reg = XEXP (XEXP (ADDR, 0), 1);				\
      offset = INTVAL (XEXP (ADDR, 1));					\
      regs = gen_rtx (PLUS, Pmode, stack_pointer_rtx, other_reg); }	\
  else if (GET_CODE (ADDR) == PLUS					\
	   && GET_CODE (XEXP (ADDR, 0)) == PLUS				\
	   && XEXP (XEXP (ADDR, 0), 1) == frame_pointer_rtx		\
	   && GET_CODE (XEXP (ADDR, 1)) == CONST_INT)			\
    { rtx other_reg = XEXP (XEXP (ADDR, 0), 0);				\
      offset = INTVAL (XEXP (ADDR, 1));					\
      regs = gen_rtx (PLUS, Pmode, stack_pointer_rtx, other_reg); }	\
  if (offset >= 0)							\
    { int regno;							\
      extern char call_used_regs[];					\
      for (regno = 16; regno < FIRST_PSEUDO_REGISTER; regno++)		\
        if (regs_ever_live[regno] && ! call_used_regs[regno])		\
          offset += 12;							\
      for (regno = 0; regno < 16; regno++)				\
	if (regs_ever_live[regno] && ! call_used_regs[regno])		\
	  offset += 4;							\
      offset -= 4;							\
      ADDR = plus_constant (regs, offset + (DEPTH)); } }		\

/* Addressing modes, and classification of registers for them.  */

#define HAVE_POST_INCREMENT
/* #define HAVE_POST_DECREMENT */

#define HAVE_PRE_DECREMENT
/* #define HAVE_PRE_INCREMENT */

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */

#define REGNO_OK_FOR_INDEX_P(REGNO) \
((REGNO) < 16 || (unsigned) reg_renumber[REGNO] < 16)
#define REGNO_OK_FOR_BASE_P(REGNO) \
(((REGNO) ^ 010) < 8 || (unsigned) (reg_renumber[REGNO] ^ 010) < 8)
#define REGNO_OK_FOR_DATA_P(REGNO) \
((REGNO) < 8 || (unsigned) reg_renumber[REGNO] < 8)
#define REGNO_OK_FOR_FP_P(REGNO) \
(((REGNO) ^ 020) < 8 || (unsigned) (reg_renumber[REGNO] ^ 020) < 8)
#define REGNO_OK_FOR_FPA_P(REGNO) \
(((REGNO) >= 24 && (REGNO) < 56) || (reg_renumber[REGNO] >= 24 && reg_renumber[REGNO] < 56))

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.

   These macros are specific to the 68000, and may be used only
   in code for printing assembler insns and in conditions for
   define_optimization.  */

/* 1 if X is a data register.  */

#define DATA_REG_P(X) (REG_P (X) && REGNO_OK_FOR_DATA_P (REGNO (X)))

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* 1 if X is an address register  */

#define ADDRESS_REG_P(X) (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))

/* 1 if X is a register in the Sun FPA.  */
#define FPA_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FPA_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X)  CONSTANT_P (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

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
#define REG_OK_FOR_INDEX_P(X) ((REGNO (X) ^ 020) >= 8)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) ((REGNO (X) & ~027) != 0)

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

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS.  */

#define INDIRECTABLE_1_ADDRESS_P(X)  \
  (CONSTANT_ADDRESS_P (X)						\
   || (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
   || ((GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_INC)		\
       && REG_P (XEXP (X, 0))						\
       && REG_OK_FOR_BASE_P (XEXP (X, 0)))				\
   || (GET_CODE (X) == PLUS						\
       && REG_P (XEXP (X, 0)) && REG_OK_FOR_BASE_P (XEXP (X, 0))	\
       && GET_CODE (XEXP (X, 1)) == CONST_INT				\
       && ((unsigned) INTVAL (XEXP (X, 1)) + 0x8000) < 0x10000))

#if 0
/* This should replace the last two lines
   except that Sun's assembler does not seem to handle such operands.  */
       && (TARGET_68020 ? CONSTANT_ADDRESS_P (XEXP (X, 1))		\
	   : (GET_CODE (XEXP (X, 1)) == CONST_INT			\
	      && ((unsigned) INTVAL (XEXP (X, 1)) + 0x8000) < 0x10000))))
#endif


#define GO_IF_NONINDEXED_ADDRESS(X, ADDR)  \
{ if (INDIRECTABLE_1_ADDRESS_P (X)) goto ADDR; }

#define GO_IF_INDEXABLE_BASE(X, ADDR)	\
{ if (GET_CODE (X) == LABEL_REF) goto ADDR;				\
  if (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X)) goto ADDR; }

#define GO_IF_INDEXING(X, ADDR)	\
{ if (GET_CODE (X) == PLUS && LEGITIMATE_INDEX_P (XEXP (X, 0)))		\
    { GO_IF_INDEXABLE_BASE (XEXP (X, 1), ADDR); }			\
  if (GET_CODE (X) == PLUS && LEGITIMATE_INDEX_P (XEXP (X, 1)))		\
    { GO_IF_INDEXABLE_BASE (XEXP (X, 0), ADDR); } }

#define GO_IF_INDEXED_ADDRESS(X, ADDR)	 \
{ GO_IF_INDEXING (X, ADDR);						\
  if (GET_CODE (X) == PLUS)						\
    { if (GET_CODE (XEXP (X, 1)) == CONST_INT				\
	  && (unsigned) INTVAL (XEXP (X, 1)) + 0x80 < 0x100)		\
	{ rtx go_temp = XEXP (X, 0); GO_IF_INDEXING (go_temp, ADDR); }	\
      if (GET_CODE (XEXP (X, 0)) == CONST_INT				\
	  && (unsigned) INTVAL (XEXP (X, 0)) + 0x80 < 0x100)		\
	{ rtx go_temp = XEXP (X, 1); GO_IF_INDEXING (go_temp, ADDR); } } }

#define LEGITIMATE_INDEX_REG_P(X)   \
  ((GET_CODE (X) == REG && REG_OK_FOR_INDEX_P (X))	\
   || (GET_CODE (X) == SIGN_EXTEND			\
       && GET_CODE (XEXP (X, 0)) == REG			\
       && GET_MODE (XEXP (X, 0)) == HImode		\
       && REG_OK_FOR_INDEX_P (XEXP (X, 0))))

#define LEGITIMATE_INDEX_P(X)   \
   (LEGITIMATE_INDEX_REG_P (X)				\
    || (TARGET_68020 && GET_CODE (X) == MULT		\
	&& LEGITIMATE_INDEX_REG_P (XEXP (X, 0))		\
	&& GET_CODE (XEXP (X, 1)) == CONST_INT		\
	&& (INTVAL (XEXP (X, 1)) == 2			\
	    || INTVAL (XEXP (X, 1)) == 4		\
	    || INTVAL (XEXP (X, 1)) == 8)))

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{ GO_IF_NONINDEXED_ADDRESS (X, ADDR);			\
  GO_IF_INDEXED_ADDRESS (X, ADDR); }

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For the 68000, we handle X+REG by loading X into a register R and
   using R+REG.  R will go in an address reg and indexing will be used.
   However, if REG is a broken-out memory address or multiplication,
   nothing needs to be done because REG can certainly go in an address reg.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)   \
{ register int ch = (X) != (OLDX);					\
  if (GET_CODE (X) == PLUS)						\
    { if (GET_CODE (XEXP (X, 0)) == MULT)				\
	ch = 1, XEXP (X, 0) = force_operand (XEXP (X, 0), 0);		\
      if (GET_CODE (XEXP (X, 1)) == MULT)				\
	ch = 1, XEXP (X, 1) = force_operand (XEXP (X, 1), 0);		\
      if (ch && GET_CODE (XEXP (X, 1)) == REG				\
	  && GET_CODE (XEXP (X, 0)) == REG)				\
	return X;							\
      if (ch) { GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN); }		\
      if (GET_CODE (XEXP (X, 0)) == REG					\
	       || (GET_CODE (XEXP (X, 0)) == SIGN_EXTEND		\
		   && GET_CODE (XEXP (XEXP (X, 0), 0)) == REG		\
		   && GET_MODE (XEXP (XEXP (X, 0), 0)) == HImode))	\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	  register rtx val = force_operand (XEXP (X, 1), 0);		\
	  emit_move_insn (temp, val);					\
	  XEXP (X, 1) = temp;						\
	  return X; }							\
      else if (GET_CODE (XEXP (X, 1)) == REG				\
	       || (GET_CODE (XEXP (X, 1)) == SIGN_EXTEND		\
		   && GET_CODE (XEXP (XEXP (X, 1), 0)) == REG		\
		   && GET_MODE (XEXP (XEXP (X, 1), 0)) == HImode))	\
	{ register rtx temp = gen_reg_rtx (Pmode);			\
	  register rtx val = force_operand (XEXP (X, 0), 0);		\
	  emit_move_insn (temp, val);					\
	  XEXP (X, 0) = temp;						\
	  return X; }}}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.
   On the 68000, only predecrement and postincrement address depend thus
   (the amount of decrement or increment being the length of the operand).  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)	\
 if (GET_CODE (ADDR) == POST_INC || GET_CODE (ADDR) == PRE_DEC) goto LABEL

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE HImode

/* Define this if the tablejump instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
#define CASE_VECTOR_PC_RELATIVE

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 4

/* Define this if zero-extension is slow (more than one real instruction).  */
#define SLOW_ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
#define SHIFT_COUNT_TRUNCATED

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

#define STORE_FLAG_VALUE -1

/* When a prototype says `char' or `short', really pass an `int'.  */
#define PROMOTE_PROTOTYPES

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */
#define Pmode SImode

/* A function address in a call instruction
   is a byte address (for indexing purposes)
   so give the MEM rtx a byte's mode.  */
#define FUNCTION_MODE QImode

/* Compute the cost of computing a constant rtl expression RTX
   whose rtx-code is CODE.  The body of this macro is a portion
   of a switch statement.  If the code is computed here,
   return it with a return statement.  Otherwise, break from the switch.  */

#define CONST_COSTS(RTX,CODE) \
  case CONST_INT:						\
    /* Constant zero is super cheap due to clr instruction.  */	\
    if (RTX == const0_rtx) return 0;				\
    if ((unsigned) INTVAL (RTX) < 077) return 1;		\
  case CONST:							\
  case LABEL_REF:						\
  case SYMBOL_REF:						\
    return 3;							\
  case CONST_DOUBLE:						\
    return 5;

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  */

/* Set if the cc value is actually in the 68881, so a floating point
   conditional branch must be output.  */
#define CC_IN_68881 04000

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

/* On the 68000, all the insns to store in an address register
   fail to set the cc's.  However, in some cases these instructions
   can make it possibly invalid to use the saved cc's.  In those
   cases we clear out some or all of the saved cc's so they won't be used.  */

/* It was claimed recently that addq, subq to an address register
   do update the cc's, but the 68000 and 68020 manuals say otherwise.  */

#define NOTICE_UPDATE_CC(EXP, INSN) \
{								\
  /* If the cc is being set from the fpa and the
     expression is not an explicit floating point
     test instruction (which has code to deal with
     this), reinit the CC */					\
  if (((cc_status.value1 && FPA_REG_P (cc_status.value1))	\
       || (cc_status.value2 && FPA_REG_P (cc_status.value2)))	\
      && !(GET_CODE(EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP(EXP, 0, 0)) == SET		\
	   && XEXP (XVECEXP (EXP, 0, 0), 0) == cc0_rtx))	\
    { CC_STATUS_INIT; }						\
  else if (GET_CODE (EXP) == SET)				\
    { if (ADDRESS_REG_P (SET_DEST (EXP)))			\
	{ if (cc_status.value1					\
	      && reg_overlap_mentioned_p (SET_DEST (EXP), cc_status.value1)) \
	    cc_status.value1 = 0;				\
	  if (cc_status.value2					\
	      && reg_overlap_mentioned_p (SET_DEST (EXP), cc_status.value2)) \
	    cc_status.value2 = 0; }				\
      else if (!FP_REG_P (SET_DEST (EXP))			\
	       && SET_DEST (EXP) != cc0_rtx			\
	       && (FP_REG_P (SET_SRC (EXP))			\
		   || GET_CODE (SET_SRC (EXP)) == FIX		\
		   || GET_CODE (SET_SRC (EXP)) == FLOAT_TRUNCATE \
		   || GET_CODE (SET_SRC (EXP)) == FLOAT_EXTEND)) \
	{ CC_STATUS_INIT; }					\
      /* A pair of move insns doesn't produce a useful overall cc.  */ \
      else if (!FP_REG_P (SET_DEST (EXP))			\
	       && !FP_REG_P (SET_SRC (EXP))			\
	       && GET_MODE_SIZE (GET_MODE (SET_SRC (EXP))) > 4	\
	       && (GET_CODE (SET_SRC (EXP)) == REG		\
		   || GET_CODE (SET_SRC (EXP)) == MEM		\
		   || GET_CODE (SET_SRC (EXP)) == CONST_DOUBLE))\
	{ CC_STATUS_INIT; }					\
      else if (GET_CODE (SET_SRC (EXP)) == CALL)		\
	{ CC_STATUS_INIT; }					\
      else if (XEXP (EXP, 0) != pc_rtx)				\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = XEXP (EXP, 0);			\
	  cc_status.value2 = XEXP (EXP, 1); } }			\
  else if (GET_CODE (EXP) == PARALLEL				\
	   && GET_CODE (XVECEXP (EXP, 0, 0)) == SET)		\
    {								\
      if (ADDRESS_REG_P (XEXP (XVECEXP (EXP, 0, 0), 0)))	\
	CC_STATUS_INIT;						\
      else if (XEXP (XVECEXP (EXP, 0, 0), 0) != pc_rtx)		\
	{ cc_status.flags = 0;					\
	  cc_status.value1 = XEXP (XVECEXP (EXP, 0, 0), 0);	\
	  cc_status.value2 = XEXP (XVECEXP (EXP, 0, 0), 1); } }	\
  else CC_STATUS_INIT;						\
  if (cc_status.value2 != 0					\
      && ADDRESS_REG_P (cc_status.value2)			\
      && GET_MODE (cc_status.value2) == QImode)			\
    CC_STATUS_INIT;						\
  if (cc_status.value2 != 0					\
      && !(cc_status.value1 && FPA_REG_P (cc_status.value1)))	\
    switch (GET_CODE (cc_status.value2))			\
      { case PLUS: case MINUS: case MULT: case UMULT:		\
	case DIV: case UDIV: case MOD: case UMOD: case NEG:	\
	case ASHIFT: case LSHIFT: case ASHIFTRT: case LSHIFTRT:	\
	case ROTATE: case ROTATERT:				\
	  if (GET_MODE (cc_status.value2) != VOIDmode)		\
	    cc_status.flags |= CC_NO_OVERFLOW;			\
	  break;						\
	case ZERO_EXTEND:					\
	case ZERO_EXTRACT:					\
	  /* (SET r1 (ZERO_EXTEND r2)) on this machine
	     ends with a move insn moving r2 in r2's mode.
	     Thus, the cc's are set for r2.
	     This can set N bit spuriously. */			\
	  cc_status.flags |= CC_NOT_NEGATIVE; }			\
  if (cc_status.value1 && GET_CODE (cc_status.value1) == REG	\
      && cc_status.value2					\
      && reg_overlap_mentioned_p (cc_status.value1, cc_status.value2))	\
    cc_status.value2 = 0;					\
  if (((cc_status.value1 && FP_REG_P (cc_status.value1))	\
       || (cc_status.value2 && FP_REG_P (cc_status.value2)))	\
      && !((cc_status.value1 && FPA_REG_P (cc_status.value1))	\
	   || (cc_status.value2 && FPA_REG_P (cc_status.value2)))) \
    cc_status.flags = CC_IN_68881; }

#define OUTPUT_JUMP(NORMAL, FLOAT, NO_OV)  \
do { if (cc_prev_status.flags & CC_IN_68881)			\
       return FLOAT;						\
     if (cc_prev_status.flags & CC_NO_OVERFLOW)			\
       return NO_OV;						\
     return NORMAL; } while (0)

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE)	\
  fprintf (FILE, "#NO_APP\n");

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON "#APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF "#NO_APP\n"

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP ".text"

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP ".data"

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"d0", "d1", "d2", "d3", "d4", "d5", "d6", "d7",	\
 "a0", "a1", "a2", "a3", "a4", "a5", "a6", "sp",	\
 "fp0", "fp1", "fp2", "fp3", "fp4", "fp5", "fp6", "fp7", \
 "fpa0", "fpa1", "fpa2", "fpa3", "fpa4", "fpa5", "fpa6", "fpa7", \
 "fpa8", "fpa9", "fpa10", "fpa11", "fpa12", "fpa13", "fpa14", "fpa15", \
 "fpa16", "fpa17", "fpa18", "fpa19", "fpa20", "fpa21", "fpa22", "fpa23", \
 "fpa24", "fpa25", "fpa26", "fpa27", "fpa28", "fpa29", "fpa30", "fpa31", }

/* How to renumber registers for dbx and gdb.
   On the Sun-3, the floating point registers have numbers
   18 to 25, not 16 to 23 as they do in the compiler.  */

#define DBX_REGISTER_NUMBER(REGNO) ((REGNO) < 16 ? (REGNO) : (REGNO) + 2)

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

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
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\t.double 0r%.20g\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

/* Sun's assembler can't handle floating constants written as floating.
   However, when cross-compiling, always use that in case format differs.  */

#ifdef CROSS_COMPILER

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\t.float 0r%.10g\n", (VALUE))

#else

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
do { union { float f; long l;} tem;			\
     tem.f = (VALUE);					\
     fprintf (FILE, "\t.long 0x%x\n", tem.l);	\
   } while (0)

#endif /* not CROSS_COMPILER */

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\t.long "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\t.word "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\t.byte "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\t.byte 0x%x\n", (VALUE))

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  \
  fprintf (FILE, "\tmovel %s,sp@-\n", reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)  \
  fprintf (FILE, "\tmovel sp@+,%s\n", reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute.
   (The 68000 does not use such vectors,
   but we must define this macro anyway.)  */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\t.long L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\t.word L%d-L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)	\
  if ((LOG) == 1)			\
    fprintf (FILE, "\t.even\n");	\
  else if ((LOG) != 0)			\
    abort ();

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\t.skip %u\n", (SIZE))

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

/* Output a float value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */
#define ASM_OUTPUT_FLOAT_OPERAND(FILE,VALUE)				\
  fprintf (FILE, "#0r%.9g", (VALUE))

/* Output a double value (represented as a C double) as an immediate operand.
   This macro is a 68k-specific macro.  */
#define ASM_OUTPUT_DOUBLE_OPERAND(FILE,VALUE)				\
  fprintf (FILE, "#0r%.20g", (VALUE))

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.

   On the 68000, we use several CODE characters:
   '.' for dot needed in Motorola-style opcode names.
   '-' for an operand pushing on the stack:
       sp@-, -(sp) or -(%sp) depending on the style of syntax.
   '+' for an operand pushing on the stack:
       sp@+, (sp)+ or (%sp)+ depending on the style of syntax.
   '@' for a reference to the top word on the stack:
       sp@, (sp) or (%sp) depending on the style of syntax.
   '#' for an immediate operand prefix (# in MIT and Motorola syntax
       but & in SGS syntax).
   '!' for the cc register (used in an `and to cc' insn).

   'b' for byte insn (no effect, on the Sun; this is for the ISI).
   'd' to force memory addressing to be absolute, not relative.
   'f' for float insn (print a CONST_DOUBLE as a float rather than in hex)
   'w' for FPA insn (print a CONST_DOUBLE as a SunFPA constant rather
       than directly).  Second part of 'y' below.
   'x' for float insn (print a CONST_DOUBLE as a float rather than in hex),
       or print pair of registers as rx:ry.
   'y' for a FPA insn (print pair of registers as rx:ry).  This also outputs
       CONST_DOUBLE's as SunFPA constant RAM registers if
       possible, so it should not be used except for the SunFPA. */

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)				\
  ((CODE) == '.' || (CODE) == '#' || (CODE) == '-'			\
   || (CODE) == '+' || (CODE) == '@' || (CODE) == '!')

/* This assumes the compiler is running on a big-endian machine.
   The support for the other case is left for version 2.  */
#define PRINT_OPERAND_EXTRACT_FLOAT(X)					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);

#ifdef CROSS_COMPILER
#define PRINT_OPERAND_PRINT_FLOAT(CODE, FILE)   \
  ASM_OUTPUT_FLOAT_OPERAND (FILE, u1.f);
#else
#define PRINT_OPERAND_PRINT_FLOAT(CODE, FILE)   \
{ if (CODE == 'f')							\
    ASM_OUTPUT_FLOAT_OPERAND (FILE, u1.f);				\
  else									\
    fprintf (FILE, "#0x%x", u1.i); }
#endif

#define PRINT_OPERAND(FILE, X, CODE)  \
{ int i;								\
  if (CODE == '.') ;							\
  else if (CODE == '#') fprintf (FILE, "#");				\
  else if (CODE == '-') fprintf (FILE, "sp@-");				\
  else if (CODE == '+') fprintf (FILE, "sp@+");				\
  else if (CODE == '@') fprintf (FILE, "sp@");				\
  else if (CODE == '!') fprintf (FILE, "cc");				\
  else if (GET_CODE (X) == REG)						\
    { if (REGNO (X) < 16 && (CODE == 'y' || CODE == 'x') && GET_MODE (X) == DFmode)	\
        fprintf (FILE, "%s:%s", reg_names[REGNO (X)], reg_names[REGNO (X)+1]); \
      else								\
        fprintf (FILE, "%s", reg_names[REGNO (X)]);			\
    }									\
  else if (GET_CODE (X) == MEM)						\
    {									\
      output_address (XEXP (X, 0));					\
      if (CODE == 'd' && ! TARGET_68020					\
	  && CONSTANT_ADDRESS_P (XEXP (X, 0))				\
	  && !(GET_CODE (XEXP (X, 0)) == CONST_INT			\
	       && INTVAL (XEXP (X, 0)) < 0x8000				\
	       && INTVAL (XEXP (X, 0)) >= -0x8000))			\
	fprintf (FILE, ":l");						\
    }									\
  else if ((CODE == 'y' || CODE == 'w')					\
	   && GET_CODE(X) == CONST_DOUBLE				\
	   && (i = standard_sun_fpa_constant_p (X)))			\
    fprintf (FILE, "%%%d", i & 0x1ff);					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) == SFmode)	\
    { union { double d; int i[2]; } u;					\
      union { float f; int i; } u1;					\
      PRINT_OPERAND_EXTRACT_FLOAT (X);					\
      u1.f = u.d;							\
      PRINT_OPERAND_PRINT_FLOAT (CODE, FILE); }				\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != DImode)	\
    { union { double d; int i[2]; } u;					\
      PRINT_OPERAND_EXTRACT_FLOAT (X);					\
      ASM_OUTPUT_DOUBLE_OPERAND (FILE, u.d); }				\
  else { putc ('#', FILE); output_addr_const (FILE, X); }}

/* Note that this contains a kludge that knows that the only reason
   we have an address (plus (label_ref...) (reg...))
   is in the insn before a tablejump, and we know that m68k.md
   generates a label LInnn: on such an insn.  */
#define PRINT_OPERAND_ADDRESS(FILE, ADDR)  \
{ register rtx reg1, reg2, breg, ireg;					\
  register rtx addr = ADDR;						\
  rtx offset;								\
  switch (GET_CODE (addr))						\
    {									\
    case REG:								\
      fprintf (FILE, "%s@", reg_names[REGNO (addr)]);			\
      break;								\
    case PRE_DEC:							\
      fprintf (FILE, "%s@-", reg_names[REGNO (XEXP (addr, 0))]);	\
      break;								\
    case POST_INC:							\
      fprintf (FILE, "%s@+", reg_names[REGNO (XEXP (addr, 0))]);	\
      break;								\
    case PLUS:								\
      reg1 = 0;	reg2 = 0;						\
      ireg = 0;	breg = 0;						\
      offset = 0;							\
      if (CONSTANT_ADDRESS_P (XEXP (addr, 0)))				\
	{								\
	  offset = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))			\
	{								\
	  offset = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) != PLUS) ;					\
      else if (GET_CODE (XEXP (addr, 0)) == SIGN_EXTEND)		\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == SIGN_EXTEND)		\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      else if (GET_CODE (XEXP (addr, 0)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == MULT)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      else if (GET_CODE (XEXP (addr, 0)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 0);					\
	  addr = XEXP (addr, 1);					\
	}								\
      else if (GET_CODE (XEXP (addr, 1)) == REG)			\
	{								\
	  reg1 = XEXP (addr, 1);					\
	  addr = XEXP (addr, 0);					\
	}								\
      if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT		\
	  || GET_CODE (addr) == SIGN_EXTEND)				\
	{ if (reg1 == 0) reg1 = addr; else reg2 = addr; addr = 0; }	\
/*  for OLD_INDEXING							\
      else if (GET_CODE (addr) == PLUS)					\
	{								\
	  if (GET_CODE (XEXP (addr, 0)) == REG)				\
	    {								\
	      reg2 = XEXP (addr, 0);					\
	      addr = XEXP (addr, 1);					\
	    }								\
	  else if (GET_CODE (XEXP (addr, 1)) == REG)			\
	    {								\
	      reg2 = XEXP (addr, 1);					\
	      addr = XEXP (addr, 0);					\
	    }								\
	}								\
  */									\
      if (offset != 0) { if (addr != 0) abort (); addr = offset; }	\
      if ((reg1 && (GET_CODE (reg1) == SIGN_EXTEND			\
		    || GET_CODE (reg1) == MULT))			\
	  || (reg2 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg2))))		\
	{ breg = reg2; ireg = reg1; }					\
      else if (reg1 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg1)))		\
	{ breg = reg1; ireg = reg2; }					\
      if (ireg != 0 && breg == 0 && GET_CODE (addr) == LABEL_REF)	\
        { int scale = 1;						\
	  if (GET_CODE (ireg) == MULT)					\
	    { scale = INTVAL (XEXP (ireg, 1));				\
	      ireg = XEXP (ireg, 0); }					\
	  if (GET_CODE (ireg) == SIGN_EXTEND)				\
	    fprintf (FILE, "pc@(L%d-LI%d-2:b,%s:w",			\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (XEXP (ireg, 0))]); 		\
	  else								\
	    fprintf (FILE, "pc@(L%d-LI%d-2:b,%s:l",			\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     CODE_LABEL_NUMBER (XEXP (addr, 0)),		\
		     reg_names[REGNO (ireg)]);				\
	  if (scale != 1) fprintf (FILE, ":%d", scale);			\
	  putc (')', FILE);						\
	  break; }							\
      if (breg != 0 && ireg == 0 && GET_CODE (addr) == LABEL_REF)	\
        { fprintf (FILE, "pc@(L%d-LI%d-2:b,%s:l",			\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   reg_names[REGNO (breg)]);				\
	  putc (')', FILE);						\
	  break; }							\
      if (ireg != 0 || breg != 0)					\
	{ int scale = 1;						\
	  if (breg == 0)						\
	    abort ();							\
	  if (addr && GET_CODE (addr) == LABEL_REF) abort ();		\
	  fprintf (FILE, "%s@(", reg_names[REGNO (breg)]);		\
	  if (addr != 0)						\
	    output_addr_const (FILE, addr);				\
	  if (addr != 0 && ireg != 0)					\
	    putc (',', FILE);						\
	  if (ireg != 0 && GET_CODE (ireg) == MULT)			\
	    { scale = INTVAL (XEXP (ireg, 1));				\
	      ireg = XEXP (ireg, 0); }					\
	  if (ireg != 0 && GET_CODE (ireg) == SIGN_EXTEND)		\
	    fprintf (FILE, "%s:w", reg_names[REGNO (XEXP (ireg, 0))]);	\
	  else if (ireg != 0)						\
	    fprintf (FILE, "%s:l", reg_names[REGNO (ireg)]);		\
	  if (scale != 1) fprintf (FILE, ":%d", scale);			\
	  putc (')', FILE);						\
	  break;							\
	}								\
      else if (reg1 != 0 && GET_CODE (addr) == LABEL_REF)		\
	{ fprintf (FILE, "pc@(L%d-LI%d-2:b,%s:l)",			\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   CODE_LABEL_NUMBER (XEXP (addr, 0)),			\
		   reg_names[REGNO (reg1)]);				\
	  break; }							\
    default:								\
      if (GET_CODE (addr) == CONST_INT					\
	  && INTVAL (addr) < 0x8000					\
	  && INTVAL (addr) >= -0x8000)					\
	fprintf (FILE, "%d:w", INTVAL (addr));				\
      else								\
        output_addr_const (FILE, addr);					\
    }}

/*
Local variables:
version-control: t
End:
*/
