/* Definitions of target machine for GNU compiler.  Convex version.
   Copyright (C) 1989, 1990 Free Software Foundation, Inc.

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


/* Use the proper incantation to search Posix-compliant libraries. */

#define LINK_SPEC \
"%{!traditional:-Eposix}%{traditional:-Enoposix}\
 -A__iob=___ap$iob\
 -A_use_libc_sema=___ap$use_libc_sema\
 -L /usr/lib"

/* Use the matching startup files. */

#define STARTFILE_SPEC \
"%{pg:/usr/lib/crt/gcrt0.o}\
%{!pg:%{p:/usr/lib/crt/mcrt0.o}\
%{!p:/usr/lib/crt/crt0.o}}"

/* Names to predefine in the preprocessor for this target machine.  */

#define CPP_PREDEFINES "-Dconvex -Dunix"

/* Print subsidiary information on the compiler version in use.  */

#define TARGET_VERSION fprintf (stderr, " (convex)");

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Macros used in the machine description to test the flags.  */

/* 
   -mc1		avoid C2-only instructions; default on C1 host
   -mc2		use C2-only instructions; default on C2 host
   -margcount	use standard calling sequence, with arg count word
   -mnoargcount don't push arg count (it's in the symbol table) (usually)
*/

#define TARGET_C1 (target_flags & 1)
#define TARGET_C2 (target_flags & 2)
#define TARGET_ARGCOUNT (target_flags & 4)

/* Macro to define tables used to set the flags.
   This is a list in braces of pairs in braces,
   each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES \
  { { "c1", 1 }, 	\
    { "c2", 2 },	\
    { "noc1", -1 }, 	\
    { "noc2", -2 },	\
    { "argcount", 4 },  \
    { "noargcount", -4 }, \
    { "", TARGET_DEFAULT }}

/* Default target_flags if no switches specified.  */

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT 0
#endif

/* Allow $ in identifiers */

#define DOLLARS_IN_IDENTIFIERS 1

/* Definitions for g++.  */

/* Do not put out GNU stabs for constructors and destructors.
   ld does not like them.  */

#define FASCIST_ASSEMBLER

/* Convex has negative addresses, so use positive numbers
   to mean `vtable index'.  */

#define VTABLE_USES_MASK
#define VINDEX_MAX ((unsigned) 0x80000000)

/* Target machine storage layout */

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields. */
#define BITS_BIG_ENDIAN

/* Define this if most significant byte of a word is the lowest numbered.  */
#define BYTES_BIG_ENDIAN

/* Define this if most significant word of a multiword number is numbered.  */
/* Lie, so that gcc will take the low part of double reg N in reg N.  */
/* #define WORDS_BIG_ENDIAN */

/* Number of bits in an addressible storage unit */
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
#define STACK_BOUNDARY 32

/* Allocation boundary (in *bits*) for the code of a function.  */
#define FUNCTION_BOUNDARY 16

/* Alignment of field after `int : 0' in a structure.  */
#define EMPTY_FIELD_BOUNDARY 32

/* Every structure's size must be a multiple of this.  */
#define STRUCTURE_SIZE_BOUNDARY 8

/* A bitfield declared as `int' forces `int' alignment for the struct.  */
#define PCC_BITFIELD_TYPE_MATTERS 1

/* No data type wants to be aligned rounder than this.  */
/* beware of doubles in structs -- 64 is incompatible with pcc */
#define BIGGEST_ALIGNMENT 32

/* Define this if move instructions will actually fail to work
   when given unaligned data.  */
/* #define STRICT_ALIGNMENT */

/* Standard register usage.  */

/* Number of actual hardware registers.
   The hardware registers are assigned numbers for the compiler
   from 0 to just below FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.  */
#define FIRST_PSEUDO_REGISTER 16

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.
   For Convex, these are AP, FP, and SP.  */
#define FIXED_REGISTERS {0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1} 

/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.  */
#define CALL_USED_REGISTERS {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.
   On Convex, all values fit in one register.  */
#define HARD_REGNO_NREGS(REGNO, MODE)   1

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   On Convex, S registers can hold any type, A registers can any nonfloat */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  ((REGNO) < 8 || ((MODE) != SFmode && (MODE) != DFmode && (MODE) != DImode))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)  \
    (((MODE1) == SFmode || (MODE1) == DFmode || (MODE1) == DImode) \
     == ((MODE2) == SFmode || (MODE2) == DFmode || (MODE2) == DImode))

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM 8

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM 15

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms
   may be accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */
#define FRAME_POINTER_REQUIRED 1

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM 14

/* Register in which static-chain is passed to a function.  */
#define STATIC_CHAIN_REGNUM 0

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
   
/* Convex has classes A (address) and S (scalar).  Seems to work
   better to put S first, here and in the md. */

enum reg_class { NO_REGS, S_REGS, A_REGS, ALL_REGS, LIM_REG_CLASSES };

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Since GENERAL_REGS is the same class as ALL_REGS,
   don't give it a different class number; just make it an alias.  */

#define GENERAL_REGS ALL_REGS

/* Give names of register classes as strings for dump file.   */

#define REG_CLASS_NAMES \
 {"NO_REGS", "S_REGS", "A_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */

#define REG_CLASS_CONTENTS {0, 0x00ff, 0xff00, 0xffff}

/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) \
  (S_REGNO_P (REGNO) ? S_REGS : A_REGS)

#define S_REGNO_P(REGNO) ((REGNO) < 8)
#define A_REGNO_P(REGNO) ((REGNO) >= 8)

#define S_REG_P(X) (REG_P (X) && S_REGNO_P (REGNO (X)))
#define A_REG_P(X) (REG_P (X) && A_REGNO_P (REGNO (X)))

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS A_REGS
#define BASE_REG_CLASS A_REGS

/* Get reg_class from a letter such as appears in the machine description.  */
/* S regs use the letter 'd' because 's' is taken. */

#define REG_CLASS_FROM_LETTER(C) \
  ((C) == 'a' ? A_REGS : (C) == 'd' ? S_REGS : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

/* Convex uses only I:
   32-bit value with sign bit off, usable as immediate in DImode logical 
     instructions and, or, xor */ 

#define CONST_OK_FOR_LETTER_P(VALUE, C)  ((VALUE) >= 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */
/* Convex uses only G:
   value usable in ld.d (low word 0) or ld.l (high word all sign) */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
  (LD_D_P (VALUE) || LD_L_P (VALUE))

#define LD_D_P(X) (const_double_low_int (X) == 0)

#define LD_L_P(X) (const_double_low_int (X) >= 0 \
		   ? const_double_high_int (X) == 0 \
		   : const_double_high_int (X) == -1)

extern int const_double_low_int ();
extern int const_double_high_int ();
extern int const_double_float_int ();

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS; but on some machines
   in some cases it is preferable to use a more restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X,CLASS)  (CLASS)

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.  */
#define CLASS_MAX_NREGS(CLASS, MODE) 1

/* Stack layout; function entry, exit and calling.  */

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD

/* Define this if should default to -fcaller-saves.  */

#define DEFAULT_CALLER_SAVES

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET 0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by. */
#define PUSH_ROUNDING(BYTES) (((BYTES) + 3) & ~3)

/* Offset of first parameter from the argument pointer register value.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Value is 1 if returning from a function call automatically
   pops the arguments described by the number-of-args field in the call.
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name. */
/* The standard Convex call, with arg count word, includes popping the
   args as part of the call template.  We optionally omit the arg count
   word and let gcc combine the arg pops. */
#define RETURN_POPS_ARGS(FUNTYPE) TARGET_ARGCOUNT

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */

/* On Convex the return value is in S0 regardless.  */   

#define FUNCTION_VALUE(VALTYPE, FUNC)  \
  gen_rtx (REG, TYPE_MODE (VALTYPE), 0)

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */

/* On Convex the return value is in S0 regardless.  */   

#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, 0)

/* Define this if PCC uses the nonreentrant convention for returning
   structure and union values.  */

#define PCC_STATIC_STRUCT_RETURN

/* 1 if N is a possible register number for a function value.
   On the Convex, S0 is the only register thus used.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

/* 1 if N is a possible register number for function argument passing. */

#define FUNCTION_ARG_REGNO_P(N) 0

/* Define a data type for recording info about an argument list
   during the scan of that argument list.  This data type should
   hold all necessary information about the function itself
   and about the args processed so far, enough to enable macros
   such as FUNCTION_ARG to determine where the next arg should go.

   On the vax, this is a single integer, which is a number of bytes
   of arguments scanned so far.  */

#define CUMULATIVE_ARGS int

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.

   On Convex, the offset starts at 0.  */

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

/* On Convex, all args are pushed.  */   

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) 0

/* This macro generates the assembly code for function entry.
   FILE is a stdio stream to output the code to.
   SIZE is an int: how many units of temporary storage to allocate.
   Refer to the array `regs_ever_live' to determine which registers
   to save; `regs_ever_live[I]' is nonzero if register number I
   is ever used in the function.  This macro is responsible for
   knowing which registers should not be saved even if used.  */

#define FUNCTION_PROLOGUE(FILE, SIZE)     \
{  if ((SIZE) != 0) fprintf (FILE, "\tsub.w #%d,sp\n", ((SIZE) + 3) & -4);}

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO)  \
   fprintf (FILE, "\tldea LP%d,a1\n\tcallq mcount\n", (LABELNO));

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.  */

#define EXIT_IGNORE_STACK 1

/* This macro generates the assembly code for function exit,
   on machines that need it.  If FUNCTION_EPILOGUE is not defined
   then individual return instructions are generated for each
   return statement.  Args are same as for FUNCTION_PROLOGUE.  */

/* #define FUNCTION_EPILOGUE(FILE, SIZE)  */

/* If the memory address ADDR is relative to the frame pointer,
   correct it to be relative to the stack pointer instead.
   This is for when we don't use a frame pointer.
   ADDR should be a variable name.  */

#define FIX_FRAME_POINTER_ADDRESS(ADDR,DEPTH) abort ();

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

#define REGNO_OK_FOR_INDEX_P(regno)  \
  ((((regno) ^ 010) < 8 || ((reg_renumber[regno] ^ 010) & -8) == 0) \
   && regno != 8)

#define REGNO_OK_FOR_BASE_P(regno)  REGNO_OK_FOR_INDEX_P (regno)

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 1

/* 1 if X is an rtx for a constant that is a valid address.  */

#define CONSTANT_ADDRESS_P(X) CONSTANT_P (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

/* For convex, any single-word constant is ok; the only contexts
   allowing general_operand of mode DI or DF are movdi and movdf. */

#define LEGITIMATE_CONSTANT_P(X) \
  (GET_CODE (X) != CONST_DOUBLE ? 1 : (LD_D_P (X) || LD_L_P (X)))

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
#define REG_OK_FOR_INDEX_P(X) (REGNO (X) > 8)
/* Nonzero if X is a hard reg that can be used as a base reg
   or if it is a pseudo reg.  */
#define REG_OK_FOR_BASE_P(X) (REGNO (X) > 8)

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

   For Convex, valid addresses are
       indirectable or (MEM indirectable)
   where indirectable is 
       const, reg, (PLUS reg const) */

/* 1 if X is an address that we could indirect through.  */
#define INDIRECTABLE_ADDRESS_P(X)  \
  (CONSTANT_ADDRESS_P (X)						\
   || (GET_CODE (X) == REG && REG_OK_FOR_BASE_P (X))			\
   || (GET_CODE (X) == PLUS						\
       && GET_CODE (XEXP (X, 0)) == REG					\
       && REG_OK_FOR_BASE_P (XEXP (X, 0))				\
       && CONSTANT_ADDRESS_P (XEXP (X, 1)))				\
   || (GET_CODE (X) == PLUS						\
       && GET_CODE (XEXP (X, 1)) == REG					\
       && REG_OK_FOR_BASE_P (XEXP (X, 1))				\
       && CONSTANT_ADDRESS_P (XEXP (X, 0))))

/* Go to ADDR if X is a valid address. */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)  \
{ register rtx xfoob = (X);						\
  if (GET_CODE (xfoob) == REG) goto ADDR;				\
  if (INDIRECTABLE_ADDRESS_P (xfoob)) goto ADDR;			\
  xfoob = XEXP (X, 0);							\
  if (GET_CODE (X) == MEM && INDIRECTABLE_ADDRESS_P (xfoob))		\
    goto ADDR;								\
  if (GET_CODE (X) == PRE_DEC && REG_P (xfoob)				\
      && REGNO (xfoob) == STACK_POINTER_REGNUM)				\
    goto ADDR; }

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   For Convex, nothing needs to be done.  */

#define LEGITIMIZE_ADDRESS(X,OLDX,MODE,WIN)  {}

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for. */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR,LABEL)  {}

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE SImode

/* Define this if the case instruction expects the table
   to contain offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */
/* #define CASE_VECTOR_PC_RELATIVE */

/* Define this if the case instruction drops through after the table
   when the index is out of range.  Don't define it if the case insn
   jumps to the default label instead.  */
/* #define CASE_DROPS_THROUGH */

/* Specify the tree operation to be used to convert reals to integers.  */
#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* This is the kind of divide that is easiest to do in the general case.  */
#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Define this as 1 if `char' should by default be signed; else as 0.  */
#define DEFAULT_SIGNED_CHAR 1

/* This flag, if defined, says the same insns that convert to a signed fixnum
   also convert validly to an unsigned one.  */
#define FIXUNS_TRUNC_LIKE_FIX_TRUNC

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

/* Define this if zero-extension is slow (more than one real instruction).  */
/* #define SLOW_ZERO_EXTEND */

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

/* Define if shifts truncate the shift count
   which implies one can omit a sign-extension or zero-extension
   of a shift count.  */
#define SHIFT_COUNT_TRUNCATED

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/* On Convex, it is as good to call a constant function address as to
   call an address kept in a register. */
#define NO_FUNCTION_CSE

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
 case CONST: \
 case LABEL_REF: \
 case SYMBOL_REF: \
 case CONST_INT: \
   return 0; \
 case CONST_DOUBLE: \
   return 2;

/* Check a `double' value for validity for a particular machine mode.  */

#define CHECK_FLOAT_VALUE(mode, d) \
  if ((mode) == SFmode) \
    { \
      if ((d) > 1.7014117331926443e+38) \
	{ error ("magnitude of constant too large for `float'"); \
	  (d) = 1.7014117331926443e+38; } \
      else if ((d) < -1.7014117331926443e+38) \
	{ error ("magnitude of constant too large for `float'"); \
	  (d) = -1.7014117331926443e+38; } \
      else if (((d) > 0) && ((d) < 2.9387358770557188e-39)) \
	{ warning ("`float' constant truncated to zero"); \
	  (d) = 0.0; } \
      else if (((d) < 0) && ((d) > -2.9387358770557188e-39)) \
	{ warning ("`float' constant truncated to zero"); \
	  (d) = 0.0; } \
    }

/* Tell final.c how to eliminate redundant test instructions.  */

/* Here we define machine-dependent flags and fields in cc_status
   (see `conditions.h').  No extra ones are needed for convex.  */

/* Store in cc_status the expressions
   that the condition codes will describe
   after execution of an instruction whose pattern is EXP.
   Do not alter them if the instruction would not alter the cc's.  */

#define NOTICE_UPDATE_CC(EXP,INSN)  {CC_STATUS_INIT;}

/* Control the assembler format that we output.  */

/* Output at beginning of assembler file.  */

#define ASM_FILE_START(FILE) fprintf (FILE, ";NO_APP\n")

/* Output to assembler file text saying following lines
   may contain character constants, extra white space, comments, etc.  */

#define ASM_APP_ON ";APP\n"

/* Output to assembler file text saying following lines
   no longer contain unusual constructs.  */

#define ASM_APP_OFF ";NO_APP\n"

/* Alignment with Convex's assembler goes like this:
   .text can be .aligned up to a halfword.
   .data and .bss can be .aligned up to a longword.
   .lcomm is not supported, explicit declarations in .bss must be used instead.
   We get alignment for word and longword .text data by conventionally
   using .text 2 for word-aligned data and .text 3 for longword-aligned
   data.  This requires that this data's size be a multiple of its alignment,
   which seems to be always true.  */
   
/* Boolean to keep track of whether the current section is .text or not.  */

extern int current_section_is_text;

/* Output before read-only data.  */

#define TEXT_SECTION_ASM_OP (current_section_is_text = 1, ".text")

/* Output before writable data.  */

#define DATA_SECTION_ASM_OP (current_section_is_text = 0, ".data") 

/* Output before uninitialized data.  */

#define BSS_SECTION_ASM_OP (current_section_is_text = 0, ".bss") 

#define EXTRA_SECTIONS in_bss

#define EXTRA_SECTION_FUNCTIONS						\
void									\
bss_section ()								\
{									\
  if (in_section != in_bss)						\
    {									\
      fprintf (asm_out_file, "%s\n", BSS_SECTION_ASM_OP);		\
      in_section = in_bss;						\
    }									\
}

/* This is how to output an assembler line
   that says to advance the location counter
   to a multiple of 2**LOG bytes.  */

#define ASM_OUTPUT_ALIGN(FILE,LOG)  \
  if (current_section_is_text && (LOG) > 1)				\
    fprintf (FILE, ".text %d\n", LOG);					\
  else if (current_section_is_text)					\
    fprintf (FILE, ".text\n.align %d\n", 1 << (LOG));			\
  else									\
    fprintf (FILE, ".align %d\n", 1 << (LOG))

/* How to refer to registers in assembler output.
   This sequence is indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES \
{"s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", \
 "sp", "a1", "a2", "a3", "a4", "a5", "ap", "fp"}

/* This is BSD, so it wants DBX format.  */

#define DBX_DEBUGGING_INFO

/* How to renumber registers for dbx and gdb. */

#define DBX_REGISTER_NUMBER(REGNO) (REGNO)

/* Do not break .stabs pseudos into continuations.  */

#define DBX_CONTIN_LENGTH 0

/* This is the char to use for continuation (in case we need to turn
   continuation back on).  */

#define DBX_CONTIN_CHAR '?'

/* Don't use the `xsfoo;' construct in DBX output; this system
   doesn't support it.  */

#define DBX_NO_XREFS

/* This is how to output the definition of a user-level label named NAME,
   such as the label on a static function or variable NAME.  */

#define ASM_OUTPUT_LABEL(FILE,NAME)	\
  do { assemble_name (FILE, NAME); fputs (":\n", FILE); } while (0)

/* This is how to output a command to make the user-level label named NAME
   defined for reference from other files.  */

#define ASM_GLOBALIZE_LABEL(FILE,NAME)	\
  do { fputs (".globl ", FILE); assemble_name (FILE, NAME); fputs ("\n", FILE);} while (0)

/* This is how to output a reference to a user-level label named NAME.  */

#define ASM_OUTPUT_LABELREF(FILE,NAME)	\
  fprintf (FILE, "_%s", NAME)

/* This is how to output an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.  */

#define ASM_OUTPUT_INTERNAL_LABEL(FILE,PREFIX,NUM)	\
  fprintf (FILE, "%s%d:\n", PREFIX, NUM)

/* Put case tables in .text 2, where they will be word-aligned */

#define ASM_OUTPUT_CASE_LABEL(FILE,PREFIX,NUM,TABLE) \
  ASM_OUTPUT_ALIGN (FILE, 2); \
  ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM)

#define ASM_OUTPUT_CASE_END(FILE,NUM,TABLE) \
  ASM_OUTPUT_ALIGN (FILE, 1)

/* This is how to store into the string LABEL
   the symbol_ref name of an internal numbered label where
   PREFIX is the class of label and NUM is the number within the class.
   This is suitable for output with `assemble_name'.  */

#define ASM_GENERATE_INTERNAL_LABEL(LABEL,PREFIX,NUM)	\
  sprintf (LABEL, "*%s%d", PREFIX, NUM)

/* This is how to output an assembler line defining a `double' constant.  */

#define ASM_OUTPUT_DOUBLE(FILE,VALUE)  \
  fprintf (FILE, "\tds.d %.17e\n", (VALUE))

/* This is how to output an assembler line defining a `float' constant.  */

#define ASM_OUTPUT_FLOAT(FILE,VALUE)  \
  fprintf (FILE, "\tds.s %.9e\n", (VALUE))

/* This is how to output an assembler line defining an `int' constant.  */

#define ASM_OUTPUT_INT(FILE,VALUE)  \
( fprintf (FILE, "\tds.w "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* Likewise for `char' and `short' constants.  */

#define ASM_OUTPUT_SHORT(FILE,VALUE)  \
( fprintf (FILE, "\tds.h "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

#define ASM_OUTPUT_CHAR(FILE,VALUE)  \
( fprintf (FILE, "\tds.b "),			\
  output_addr_const (FILE, (VALUE)),		\
  fprintf (FILE, "\n"))

/* This is how to output an assembler line for a numeric constant byte.  */

#define ASM_OUTPUT_BYTE(FILE,VALUE)  \
  fprintf (FILE, "\tds.b %#x\n", (VALUE))

/* This is how to output a string */

#define ASM_OUTPUT_ASCII(FILE,STR,SIZE) do {				\
  int i;								\
  fprintf (FILE, "\tds.b \"");						\
  for (i = 0; i < (SIZE); i++) {					\
      register int c = (STR)[i] & 0377;					\
      if (c >= ' ' && c < 0177 && c != '\\' && c != '"')		\
	  putc (c, FILE);						\
      else								\
	  fprintf (FILE, "\\%03o", c);}					\
  fprintf (FILE, "\"\n");} while (0)

/* This is how to output an insn to push a register on the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_PUSH(FILE,REGNO)  	\
   fprintf (FILE, "\tpsh.%c %s\n",		\
	    S_REGNO_P (REGNO) ? 'l' : 'w',	\
	    reg_names[REGNO])

/* This is how to output an insn to pop a register from the stack.
   It need not be very fast code.  */

#define ASM_OUTPUT_REG_POP(FILE,REGNO)		\
   fprintf (FILE, "\tpop.%c %s\n",		\
	    S_REGNO_P (REGNO) ? 'l' : 'w',	\
	    reg_names[REGNO])

/* This is how to output an element of a case-vector that is absolute. */

#define ASM_OUTPUT_ADDR_VEC_ELT(FILE, VALUE)  \
  fprintf (FILE, "\tds.w L%d\n", VALUE)

/* This is how to output an element of a case-vector that is relative.  
   (not used on Convex) */

#define ASM_OUTPUT_ADDR_DIFF_ELT(FILE, VALUE, REL)  \
  fprintf (FILE, "\tds.w L%d-L%d\n", VALUE, REL)

/* This is how to output an assembler line
   that says to advance the location counter by SIZE bytes.  */

#define ASM_OUTPUT_SKIP(FILE,SIZE)  \
  fprintf (FILE, "\tds.b %u(0)\n", (SIZE))

/* This says how to output an assembler line
   to define a global common symbol.  */

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)  \
( fputs (".comm ", (FILE)),			\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ",%u\n", (ROUNDED)))

/* This says how to output an assembler line
   to define a local common symbol.  */

#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)  \
( bss_section (),				\
  assemble_name ((FILE), (NAME)),		\
  fprintf ((FILE), ":\tbs.b %u\n", (ROUNDED)))

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

/* Print an instruction operand X on file FILE.
   CODE is the code from the %-spec that requested printing this operand;
   if `%z3' was used to print operand 3, then CODE is 'z'. */

#define PRINT_OPERAND(FILE, X, CODE)  \
{ if (GET_CODE (X) == REG)						\
    fprintf (FILE, "%s", reg_names[REGNO (X)]);				\
  else if (GET_CODE (X) == MEM)						\
    output_address (XEXP (X, 0));					\
  else if (GET_CODE (X) == CONST_DOUBLE && GET_MODE (X) != DImode)	\
    { union { double d; int i[2]; } u;					\
      u.i[0] = CONST_DOUBLE_LOW (X); u.i[1] = CONST_DOUBLE_HIGH (X);	\
      fprintf (FILE, "#%.9e", u.d); }			 		\
  else { putc ('#', FILE); output_addr_const (FILE, X); }}

/* Print a memory operand whose address is X, on file FILE. */

#define PRINT_OPERAND_ADDRESS(FILE, ADDR)				\
{ 									\
  register rtx addr = ADDR;						\
  register rtx index = 0;						\
  register rtx offset = 0;						\
									\
  if (GET_CODE (addr) == MEM)						\
    {  									\
      fprintf (FILE, "@");						\
      addr = XEXP (addr, 0);						\
    }									\
									\
  switch (GET_CODE (addr))						\
    {									\
    case REG:								\
      index = addr;							\
      break;								\
									\
    case PLUS:								\
      index = XEXP (addr, 0);						\
      if (REG_P (index))						\
	offset = XEXP (addr, 1);					\
      else								\
	{								\
	  offset = XEXP (addr, 0);					\
	  index = XEXP (addr, 1);					\
	  if (! REG_P (index)) abort ();				\
        }								\
      break;								\
									\
    default:								\
      offset = addr;							\
      break;								\
    }									\
									\
  if (offset)								\
    output_addr_const (FILE, offset);					\
									\
  if (index) 								\
    fprintf (FILE, "(%s)", reg_names[REGNO (index)]);			\
}


