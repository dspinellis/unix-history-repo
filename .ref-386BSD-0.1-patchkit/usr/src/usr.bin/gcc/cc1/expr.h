/* Definitions for code generation pass of GNU compiler.
   Copyright (C) 1987 Free Software Foundation, Inc.

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


/* Macros to access the slots of a QUEUED rtx.
   Here rather than in rtl.h because only the expansion pass
   should ever encounter a QUEUED.  */

/* The variable for which an increment is queued.  */
#define QUEUED_VAR(P) XEXP (P, 0)
/* If the increment has been emitted, this is the insn
   that does the increment.  It is zero before the increment is emitted.  */
#define QUEUED_INSN(P) XEXP (P, 1)
/* If a pre-increment copy has been generated, this is the copy
   (it is a temporary reg).  Zero if no copy made yet.  */
#define QUEUED_COPY(P) XEXP (P, 2)
/* This is the body to use for the insn to do the increment.
   It is used to emit the increment.  */
#define QUEUED_BODY(P) XEXP (P, 3)
/* Next QUEUED in the queue.  */
#define QUEUED_NEXT(P) XEXP (P, 4)

/* This is the 4th arg to `expand_expr'.
   EXPAND_SUM means it is ok to return a PLUS rtx or MULT rtx.
   EXPND_CONST_ADDRESS means it is ok to return a MEM whose address
    is a constant that is not a legitimate address.  */
enum expand_modifier {EXPAND_NORMAL, EXPAND_SUM, EXPAND_CONST_ADDRESS};

/* If this is nonzero, we do not bother generating VOLATILE
   around volatile memory references, and we are willing to
   output indirect addresses.  If cse is to follow, we reject
   indirect addresses so a useful potential cse is generated;
   if it is used only once, instruction combination will produce
   the same indirect address eventually.  */
extern int cse_not_expected;

/* List (chain of EXPR_LISTs) of pseudo-regs of SAVE_EXPRs.
   So we can mark them all live at the end of the function, if stupid.  */
extern rtx save_expr_regs;

extern int current_function_calls_alloca;

/* This is the offset from the arg pointer to the place where the first
   anonymous arg can be found, if there is one.  */
extern rtx current_function_arg_offset_rtx;

/* Nonzero means stack pops must not be deferred, and deferred stack
   pops must not be output.  It is nonzero inside a function call,
   inside a conditional expression, inside a statement expression,
   and in other cases as well.  */
extern int inhibit_defer_pop;

#define NO_DEFER_POP (inhibit_defer_pop += 1)
#define OK_DEFER_POP (inhibit_defer_pop -= 1)

#ifdef TREE_CODE /* Don't lose if tree.h not included.  */
/* Structure to record the size of a sequence of arguments
   as the sum of a tree-expression and a constant.  */

struct args_size
{
  int constant;
  tree var;
};
#endif

/* Add the value of the tree INC to the `struct args_size' TO.  */

#define ADD_PARM_SIZE(TO, INC)	\
{ tree inc = (INC);				\
  if (TREE_CODE (inc) == INTEGER_CST)		\
    (TO).constant += TREE_INT_CST_LOW (inc);	\
  else if ((TO).var == 0)			\
    (TO).var = inc;				\
  else						\
    (TO).var = genop (PLUS_EXPR, (TO).var, inc); }

#define SUB_PARM_SIZE(TO, DEC)	\
{ tree dec = (DEC);				\
  if (TREE_CODE (dec) == INTEGER_CST)		\
    (TO).constant -= TREE_INT_CST_LOW (dec);	\
  else if ((TO).var == 0)			\
    (TO).var = genop (MINUS_EXPR, integer_zero_node, dec); \
  else						\
    (TO).var = genop (MINUS_EXPR, (TO).var, dec); }

/* Convert the implicit sum in a `struct args_size' into an rtx.  */
#define ARGS_SIZE_RTX(SIZE)						\
((SIZE).var == 0 ? gen_rtx (CONST_INT, VOIDmode, (SIZE).constant)	\
 : plus_constant (expand_expr ((SIZE).var, 0, VOIDmode, 0),		\
		  (SIZE).constant))

/* Supply a default definition for FUNCTION_ARG_PADDING:
   usually pad upward, but pad short args downward on big-endian machines.  */

enum direction {none, upward, downward};  /* Value has this type.  */

#ifndef FUNCTION_ARG_PADDING
#ifdef BYTES_BIG_ENDIAN
#define FUNCTION_ARG_PADDING(mode, size)				\
  (((mode) == BLKmode							\
    ? (GET_CODE (size) == CONST_INT					\
       && INTVAL (size) < PARM_BOUNDARY / BITS_PER_UNIT)		\
    : GET_MODE_BITSIZE (mode) < PARM_BOUNDARY)				\
   ? downward : upward)
#else
#define FUNCTION_ARG_PADDING(mode, size) upward
#endif
#endif

/* Nonzero if type TYPE should be returned in memory
   (even though its mode is not BLKmode).
   Most machines can use the following default definition.  */

#ifndef RETURN_IN_MEMORY
#define RETURN_IN_MEMORY(type) 0
#endif

/* Optabs are tables saying how to generate insn bodies
   for various machine modes and numbers of operands.
   Each optab applies to one operation.
   For example, add_optab applies to addition.

   The insn_code slot is the enum insn_code that says how to
   generate an insn for this operation on a particular machine mode.
   It is CODE_FOR_nothing if there is no such insn on the target machine.

   The `lib_call' slot is the name of the library function that
   can be used to perform the operation.

   A few optabs, such as move_optab and cmp_optab, are used
   by special code.  */

/* Everything that uses expr.h needs to define enum insn_code
   but we don't list it in the Makefile dependencies just for that.  */
#include "insn-codes.h"

typedef struct optab
{
  enum rtx_code code;
  struct {
    enum insn_code insn_code;
    char *lib_call;
  } handlers [NUM_MACHINE_MODES];
} * optab;

/* Given an enum insn_code, access the function to construct
   the body of that kind of insn.  */
#define GEN_FCN(CODE) (*insn_gen_function[(int) (CODE)])
extern rtx (*insn_gen_function[]) ();

extern optab add_optab;
extern optab sub_optab;
extern optab smul_optab;	/* Signed multiply */
extern optab umul_optab;	/* Unsigned multiply */
extern optab smul_widen_optab;	/* Signed multiply with result 
				   one machine mode wider than args */
extern optab umul_widen_optab;
extern optab sdiv_optab;	/* Signed divide */
extern optab sdivmod_optab;	/* Signed divide-and-remainder in one */
extern optab udiv_optab;
extern optab udivmod_optab;
extern optab smod_optab;	/* Signed remainder */
extern optab umod_optab;
extern optab flodiv_optab;	/* Optab for floating divide. */
extern optab ftrunc_optab;	/* Convert float to integer in float fmt */
extern optab and_optab;		/* Logical and */
extern optab andcb_optab;	/* Logical and with complement of 2nd arg */
extern optab ior_optab;		/* Logical or */
extern optab xor_optab;		/* Logical xor */
extern optab ashl_optab;	/* Arithmetic shift left */
extern optab ashr_optab;	/* Arithmetic shift right */
extern optab lshl_optab;	/* Logical shift left */
extern optab lshr_optab;	/* Logical shift right */
extern optab rotl_optab;	/* Rotate left */
extern optab rotr_optab;	/* Rotate right */

extern optab mov_optab;		/* Move instruction.  */
extern optab movstrict_optab;	/* Move, preserving high part of register.  */

extern optab cmp_optab;		/* Compare insn; two operands.  */
extern optab tst_optab;		/* tst insn; compare one operand against 0 */

/* Unary operations */
extern optab neg_optab;		/* Negation */
extern optab abs_optab;		/* Abs value */
extern optab one_cmpl_optab;	/* Bitwise not */
extern optab ffs_optab;		/* Find first bit set */

/* Passed to expand_binop and expand_unop to say which options to try to use
   if the requested operation can't be open-coded on the requisite mode.
   Either OPTAB_LIB or OPTAB_LIB_WIDEN says try using a library call.
   Either OPTAB_WIDEN or OPTAB_LIB_WIDEN says try using a wider mode.
   OPTAB_MUST_WIDEN says try widening and don't try anything else.  */

enum optab_methods
{
  OPTAB_DIRECT,
  OPTAB_LIB,
  OPTAB_WIDEN,
  OPTAB_LIB_WIDEN,
  OPTAB_MUST_WIDEN
};

typedef rtx (*rtxfun) ();

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the gen_function to make a branch to test that condition.  */

extern rtxfun bcc_gen_fctn[NUM_RTX_CODE];

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the gen_function to make a store-condition insn
   to test that condition.  */

extern rtxfun setcc_gen_fctn[NUM_RTX_CODE];

/* Expand a binary operation given optab and rtx operands.  */
rtx expand_binop ();

/* Expand a binary operation with both signed and unsigned forms.  */
rtx sign_expand_binop ();

/* Expand a unary arithmetic operation given optab rtx operand.  */
rtx expand_unop ();

/* Arguments MODE, RTX: return an rtx for the negation of that value.
   May emit insns.  */
rtx negate_rtx ();

/* Initialize the tables that control conversion between fixed and
   floating values.  */
void init_fixtab ();
void init_floattab ();

/* Generate code for a FIX_EXPR.  */
void expand_fix ();

/* Generate code for a FLOAT_EXPR.  */
void expand_float ();

/* Create but don't emit one rtl instruction to add one rtx into another.
   Modes must match.
   Likewise for subtraction and for just copying.
   These do not call protect_from_queue; caller must do so.  */
rtx gen_add2_insn ();
rtx gen_sub2_insn ();
rtx gen_move_insn ();

/* Emit one rtl instruction to store zero in specified rtx.  */
void emit_clr_insn ();

/* Emit one rtl insn to store 1 in specified rtx assuming it contains 0.  */
void emit_0_to_1_insn ();

/* Emit one rtl insn to compare two rtx's.  */
void emit_cmp_insn ();

/* Emit some rtl insns to move data between rtx's, converting machine modes.
   Both modes must be floating or both fixed.  */
void convert_move ();

/* Convert an rtx to specified machine mode and return the result.  */
rtx convert_to_mode ();

/* Emit code to push some arguments and call a library routine,
   storing the value in a specified place.  Calling sequence is
   complicated.  */
void emit_library_call ();

/* Given an rtx that may include add and multiply operations,
   generate them as insns and return a pseudo-reg containing the value.
   Useful after calling expand_expr with 1 as sum_ok.  */
rtx force_operand ();

/* Return an rtx for the size in bytes of the value of an expr.  */
rtx expr_size ();

/* Return an rtx for the sum of an rtx and an integer.  */
rtx plus_constant ();

rtx lookup_static_chain ();

/* Return an rtx like arg but sans any constant terms.
   Returns the original rtx if it has no constant terms.
   The constant terms are added and stored via a second arg.  */
rtx eliminate_constant_term ();

/* Convert arg to a valid memory address for specified machine mode,
   by emitting insns to perform arithmetic if nec.  */
rtx memory_address ();

/* Like `memory_address' but pretent `flag_force_addr' is 0.  */
rtx memory_address_noforce ();

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR.
   (VOIDmode means don't change the mode.
   NULL for ADDR means don't change the address.)  */
rtx change_address ();

/* Return 1 if two rtx's are equivalent in structure and elements.  */
int rtx_equal_p ();

/* Given rtx, return new rtx whose address won't be affected by
   any side effects.  It has been copied to a new temporary reg.  */
rtx stabilize ();

/* Given an rtx, copy all regs it refers to into new temps
   and return a modified copy that refers to the new temps.  */
rtx copy_all_regs ();

/* Copy given rtx to a new temp reg and return that.  */
rtx copy_to_reg ();

/* Like copy_to_reg but always make the reg Pmode.  */
rtx copy_addr_to_reg ();

/* Like copy_to_reg but always make the reg the specified mode MODE.  */
rtx copy_to_mode_reg ();

/* Copy given rtx to given temp reg and return that.  */
rtx copy_to_suggested_reg ();

/* Copy a value to a register if it isn't already a register.
   Args are mode (in case value is a constant) and the value.  */
rtx force_reg ();

/* Return given rtx, copied into a new temp reg if it was in memory.  */
rtx force_not_mem ();

/* Remove some bytes from the stack.  An rtx says how many.  */
void adjust_stack ();

/* Add some bytes to the stack.  An rtx says how many.  */
void anti_adjust_stack ();

/* Emit code to copy function value to a new temp reg and return that reg.  */
rtx function_value ();

/* Return an rtx that refers to the value returned by a function
   in its original home.  This becomes invalid if any more code is emitted.  */
rtx hard_function_value ();

/* Return an rtx that refers to the value returned by a library call
   in its original home.  This becomes invalid if any more code is emitted.  */
rtx hard_libcall_value ();

/* Emit code to copy function value to a specified place.  */
void copy_function_value ();

/* Given an rtx, return an rtx for a value rounded up to a multiple
   of STACK_BOUNDARY / BITS_PER_UNIT.  */
rtx round_push ();

rtx store_bit_field ();
rtx extract_bit_field ();
rtx expand_shift ();
rtx expand_bit_and ();
rtx expand_mult ();
rtx expand_divmod ();
rtx expand_mult_add ();
rtx get_structure_value_addr ();
rtx expand_stmt_expr ();

void jumpifnot ();
void jumpif ();
void do_jump ();

rtx assemble_static_space ();
