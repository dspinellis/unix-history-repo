/* Expand the basic unary and binary arithmetic operations, for GNU compiler.
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


#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "expr.h"
#include "insn-config.h"
#include "recog.h"

/* In ANSI C we could write MODE + 1, but traditional C compilers
   seem to reject it.  */
#define INC_MODE(MODE) (enum machine_mode) ((int)(MODE) + 1)

/* Each optab contains info on how this target machine
   can perform a particular operation
   for all sizes and kinds of operands.

   The operation to be performed is often specified
   by passing one of these optabs as an argument.

   See expr.h for documentation of these optabs.  */

optab add_optab;
optab sub_optab;
optab smul_optab;
optab umul_optab;
optab smul_widen_optab;
optab umul_widen_optab;
optab sdiv_optab;
optab sdivmod_optab;
optab udiv_optab;
optab udivmod_optab;
optab smod_optab;
optab umod_optab;
optab flodiv_optab;
optab ftrunc_optab;
optab and_optab;
optab andcb_optab;
optab ior_optab;
optab xor_optab;
optab ashl_optab;
optab lshr_optab;
optab lshl_optab;
optab ashr_optab;
optab rotl_optab;
optab rotr_optab;

optab mov_optab;
optab movstrict_optab;

optab neg_optab;
optab abs_optab;
optab one_cmpl_optab;
optab ffs_optab;

optab cmp_optab;
optab ucmp_optab;  /* Used only for libcalls for unsigned comparisons.  */
optab tst_optab;

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the gen_function to make a branch to test that condition.  */

rtxfun bcc_gen_fctn[NUM_RTX_CODE];

/* Indexed by the rtx-code for a conditional (eg. EQ, LT,...)
   gives the gen_function to make a store-condition insn
   to test that condition.  */

rtxfun setcc_gen_fctn[NUM_RTX_CODE];

/* Generate code to perform an operation specified by BINOPTAB
   on operands OP0 and OP1, with result having machine-mode MODE.

   UNSIGNEDP is for the case where we have to widen the operands
   to perform the operation.  It says to use zero-extension.

   If TARGET is nonzero, the value
   is generated there, if it is convenient to do so.
   In all cases an rtx is returned for the locus of the value;
   this may or may not be TARGET.  */

rtx
expand_binop (mode, binoptab, op0, op1, target, unsignedp, methods)
     enum machine_mode mode;
     optab binoptab;
     rtx op0, op1;
     rtx target;
     int unsignedp;
     enum optab_methods methods;
{
  enum mode_class class;
  enum machine_mode wider_mode;
  register rtx temp;
  rtx last;

  class = GET_MODE_CLASS (mode);

  op0 = protect_from_queue (op0, 0);
  op1 = protect_from_queue (op1, 0);
  if (target)
    target = protect_from_queue (target, 1);

#if 0
  /* We may get better code by generating the result in a register
     when the target is not one of the operands.  */
  if (target && ! rtx_equal_p (target, op1) && ! rtx_equal_p (target, op0))
    target_is_not_an_operand = 1;
#endif

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  /* Record where to delete back to if we backtrack.  */
  last = get_last_insn ();

  /* If operation is commutative,
     try to make the first operand a register.
     Even better, try to make it the same as the target.
     Also try to make the last operand a constant.  */
  if (binoptab == add_optab
      || binoptab == and_optab
      || binoptab == ior_optab
      || binoptab == xor_optab
      || binoptab == smul_optab
      || binoptab == umul_optab
      || binoptab == smul_widen_optab
      || binoptab == umul_widen_optab)
    {
      if (((target == 0 || GET_CODE (target) == REG)
	   ? ((GET_CODE (op1) == REG
	       && GET_CODE (op0) != REG)
	      || target == op1)
	   : rtx_equal_p (op1, target))
	  ||
	  GET_CODE (op0) == CONST_INT)
	{
	  temp = op1;
	  op1 = op0;
	  op0 = temp;
	}
    }

  /* If we can do it with a three-operand insn, do so.  */

  if (methods != OPTAB_MUST_WIDEN
      && binoptab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) binoptab->handlers[(int) mode].insn_code;
      enum machine_mode mode0 = insn_operand_mode[icode][1];
      enum machine_mode mode1 = insn_operand_mode[icode][2];
      rtx pat;
      rtx xop0 = op0, xop1 = op1;

      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      /* In case the insn wants input operands in modes different from
	 the result, convert the operands.  */

      if (GET_MODE (op0) != VOIDmode
	  && GET_MODE (op0) != mode0)
	xop0 = convert_to_mode (mode0, xop0, unsignedp);

      if (GET_MODE (xop1) != VOIDmode
	  && GET_MODE (xop1) != mode1)
	xop1 = convert_to_mode (mode1, xop1, unsignedp);

      /* Now, if insn requires register operands, put operands into regs.  */

      if (! (*insn_operand_predicate[icode][1]) (xop0, mode0))
	xop0 = force_reg (mode0, xop0);

      if (! (*insn_operand_predicate[icode][2]) (xop1, mode1))
	xop1 = force_reg (mode1, xop1);

      if (! (*insn_operand_predicate[icode][0]) (temp, mode))
	temp = gen_reg_rtx (mode);

      pat = GEN_FCN (icode) (temp, xop0, xop1);
      if (pat)
	{
	  emit_insn (pat);
	  return temp;
	}
      else
	delete_insns_since (last);
    }

  /* It can't be open-coded in this mode.
     Use a library call if one is available and caller says that's ok.  */

  if (binoptab->handlers[(int) mode].lib_call
      && (methods == OPTAB_LIB || methods == OPTAB_LIB_WIDEN))
    {
      rtx insn_before, insn_first, insn_last;
      rtx funexp = gen_rtx (SYMBOL_REF, Pmode,
			    binoptab->handlers[(int) mode].lib_call);

      /* Pass the address through a pseudoreg, if desired,
	 before the "beginning" of the library call.
	 So this insn isn't "part of" the library call, in case that
	 is deleted, or cse'd.  */
#ifndef NO_FUNCTION_CSE
      if (! flag_no_function_cse)
	funexp = copy_to_mode_reg (Pmode, funexp);
#endif

      insn_before = get_last_insn ();

      /* Cannot pass FUNEXP since emit_library_call insists
	 on getting a SYMBOL_REF.  But cse will make this SYMBOL_REF
	 be replaced with the copy we made just above.  */
      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  binoptab->handlers[(int) mode].lib_call),
			 1, mode, 2, op0, mode, op1, mode);
      target = hard_libcall_value (mode);
      temp = copy_to_reg (target);

      if (insn_before == 0)
	insn_first = get_insns ();
      else
	insn_first = NEXT_INSN (insn_before);
      insn_last = get_last_insn ();

      REG_NOTES (insn_last)
	= gen_rtx (EXPR_LIST, REG_EQUAL,
		   gen_rtx (binoptab->code, mode, op0, op1),
		   gen_rtx (INSN_LIST, REG_RETVAL, insn_first,
			    REG_NOTES (insn_last)));
      REG_NOTES (insn_first)
	= gen_rtx (INSN_LIST, REG_LIBCALL, insn_last,
		   REG_NOTES (insn_first));
      return temp;
    }

  delete_insns_since (last);

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (! (methods == OPTAB_WIDEN || methods == OPTAB_LIB_WIDEN
	 || methods == OPTAB_MUST_WIDEN))
    return 0;			/* Caller says, don't even try.  */

  /* Compute the value of METHODS to pass to recursive calls.
     Don't allow widening to be tried recursively.  */

  methods = (methods == OPTAB_LIB_WIDEN ? OPTAB_LIB : OPTAB_DIRECT);

  /* Widening is now independent of specific machine modes.
     It is assumed that widening may be performed to any
     higher numbered mode in the same mode class.  */

  if (class == MODE_INT || class == MODE_FLOAT)
    {
      for (wider_mode = INC_MODE (mode);
	   ((int) wider_mode < (int) MAX_MACHINE_MODE
	    && GET_MODE_CLASS (wider_mode) == class);
	   wider_mode = INC_MODE (wider_mode))
	{
	  if ((binoptab->handlers[(int) wider_mode].insn_code
	       != CODE_FOR_nothing)
	      || (methods == OPTAB_LIB
		  && binoptab->handlers[(int) wider_mode].lib_call))
	    {
	      rtx xop0 = op0, xop1 = op1;
	      int no_extend = 0;

	      /* For certain operations, we need not actually extend
		 the narrow operands, as long as we will truncate
		 the results to the same narrowness.  */

	      if (binoptab == ior_optab || binoptab == and_optab
		  || binoptab == xor_optab || binoptab == andcb_optab
		  || binoptab == add_optab || binoptab == sub_optab
		  || binoptab == smul_optab || binoptab == umul_optab
		  || binoptab == ashl_optab || binoptab == lshl_optab)
		no_extend = 1;

	      if (GET_MODE (xop0) != VOIDmode)
		{
		  if (no_extend)
		    {
		      temp = force_reg (GET_MODE (xop0), xop0);
		      xop0 = gen_rtx (SUBREG, wider_mode, temp, 0);
		    }
		  else
		    {
		      temp = gen_reg_rtx (wider_mode);
		      convert_move (temp, xop0, unsignedp);
		      xop0 = temp;
		    }
		}
	      if (GET_MODE (xop1) != VOIDmode)
		{
		  if (no_extend)
		    {
		      temp = force_reg (GET_MODE (xop1), xop1);
		      xop1 = gen_rtx (SUBREG, wider_mode, temp, 0);
		    }
		  else
		    {
		      temp = gen_reg_rtx (wider_mode);
		      convert_move (temp, xop1, unsignedp);
		      xop1 = temp;
		    }
		}

	      temp = expand_binop (wider_mode, binoptab, xop0, xop1, 0,
				   unsignedp, methods);
	      if (temp)
		{
		  if (class == MODE_FLOAT)
		    {
		      if (target == 0)
			target = gen_reg_rtx (mode);
		      convert_move (target, temp, 0);
		      return target;
		    }
		  else
		    return gen_lowpart (mode, temp);
		}
	      else
		delete_insns_since (last);
	    }
	}
    }

  return 0;
}

/* Expand a binary operator which has both signed and unsigned forms.
   UOPTAB is the optab for unsigned operations, and SOPTAB is for
   signed operations.

   If we widen unsigned operands, we may use a signed wider operation instead
   of an unsigned wider operation, since the result would be the same.  */

rtx
sign_expand_binop (mode, uoptab, soptab, op0, op1, target, unsignedp, methods)
    enum machine_mode mode;
    optab uoptab, soptab;
    rtx op0, op1, target;
    int unsignedp;
    enum optab_methods methods;
{
  register rtx temp;
  optab direct_optab = unsignedp ? uoptab : soptab;
  struct optab wide_soptab;

  /* Do it without widening, if possible.  */
  temp = expand_binop (mode, direct_optab, op0, op1, target,
		       unsignedp, OPTAB_DIRECT);
  if (temp || methods == OPTAB_DIRECT)
    return temp;

  /* Try widening to a signed int.  Make a fake signed optab that
     hides any signed insn for direct use.  */
  wide_soptab = *soptab;
  wide_soptab.handlers[(int) mode].insn_code = CODE_FOR_nothing;
  wide_soptab.handlers[(int) mode].lib_call = 0;

  temp = expand_binop (mode, &wide_soptab, op0, op1, target,
		       unsignedp, OPTAB_WIDEN);

  /* For unsigned operands, try widening to an unsigned int.  */
  if (temp == 0 && unsignedp)
    temp = expand_binop (mode, uoptab, op0, op1, target,
			 unsignedp, OPTAB_WIDEN);
  if (temp || methods == OPTAB_WIDEN)
    return temp;

  /* Use the right width lib call if that exists.  */
  temp = expand_binop (mode, direct_optab, op0, op1, target, unsignedp, OPTAB_LIB);
  if (temp || methods == OPTAB_LIB)
    return temp;

  /* Must widen and use a lib call, use either signed or unsigned.  */
  temp = expand_binop (mode, &wide_soptab, op0, op1, target,
		       unsignedp, methods);
  if (temp != 0)
    return temp;
  if (unsignedp)
    return expand_binop (mode, uoptab, op0, op1, target,
			 unsignedp, methods);
  return 0;
}

/* Generate code to perform an operation specified by BINOPTAB
   on operands OP0 and OP1, with two results to TARG1 and TARG2.
   We assume that the order of the operands for the instruction
   is TARG0, OP0, OP1, TARG1, which would fit a pattern like
   [(set TARG0 (operate OP0 OP1)) (set TARG1 (operate ...))].

   Either TARG0 or TARG1 may be zero, but what that means is that
   that result is not actually wanted.  We will generate it into
   a dummy pseudo-reg and discard it.  They may not both be zero.

   Returns 1 if this operation can be performed; 0 if not.  */

int
expand_twoval_binop (binoptab, op0, op1, targ0, targ1, unsignedp)
     optab binoptab;
     rtx op0, op1;
     rtx targ0, targ1;
     int unsignedp;
{
  enum machine_mode mode = GET_MODE (targ0 ? targ0 : targ1);
  enum mode_class class;
  enum machine_mode wider_mode;

  class = GET_MODE_CLASS (mode);

  op0 = protect_from_queue (op0, 0);
  op1 = protect_from_queue (op1, 0);

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  if (targ0)
    targ0 = protect_from_queue (targ0, 1);
  else
    targ0 = gen_reg_rtx (mode);
  if (targ1)
    targ1 = protect_from_queue (targ1, 1);
  else
    targ1 = gen_reg_rtx (mode);

  if (binoptab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      emit_insn (GEN_FCN (binoptab->handlers[(int) mode].insn_code)
		 (targ0, op0, op1, targ1));
      return 1;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (class == MODE_INT || class == MODE_FLOAT)
    {
      for (wider_mode = INC_MODE (mode);
	   ((int) wider_mode < (int) MAX_MACHINE_MODE
	    && GET_MODE_CLASS (wider_mode) == class);
	   wider_mode = INC_MODE (wider_mode))
	{
	  if (binoptab->handlers[(int) wider_mode].insn_code
	      != CODE_FOR_nothing)
	    {
	      expand_twoval_binop_convert (binoptab, wider_mode, op0, op1,
					   targ0, targ1, unsignedp);
	      return 1;
	    }
	}
    }
  return 0;
}

int
expand_twoval_binop_convert (binoptab, mode, op0, op1, targ0, targ1, unsignedp)
     register optab binoptab;
     register rtx op0, op1, targ0, targ1;
     int unsignedp;
{
  register rtx t0 = gen_reg_rtx (SImode);
  register rtx t1 = gen_reg_rtx (SImode);
  register rtx temp;

  temp = gen_reg_rtx (SImode);
  convert_move (temp, op0, unsignedp);
  op0 = temp;
  temp = gen_reg_rtx (SImode);
  convert_move (temp, op1, unsignedp);
  op1 = temp;

  expand_twoval_binop (binoptab, op0, op1, t0, t1, unsignedp);
  convert_move (targ0, t0, unsignedp);
  convert_move (targ1, t1, unsignedp);
  return 1;
}

/* Generate code to perform an operation specified by UNOPTAB
   on operand OP0, with result having machine-mode MODE.

   UNSIGNEDP is for the case where we have to widen the operands
   to perform the operation.  It says to use zero-extension.

   If TARGET is nonzero, the value
   is generated there, if it is convenient to do so.
   In all cases an rtx is returned for the locus of the value;
   this may or may not be TARGET.  */

rtx
expand_unop (mode, unoptab, op0, target, unsignedp)
     enum machine_mode mode;
     optab unoptab;
     rtx op0;
     rtx target;
     int unsignedp;
{
  enum mode_class class;
  enum machine_mode wider_mode;
  register rtx temp;

  class = GET_MODE_CLASS (mode);

  op0 = protect_from_queue (op0, 0);

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
    }

  if (target)
    target = protect_from_queue (target, 1);

  if (unoptab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) unoptab->handlers[(int) mode].insn_code;
      enum machine_mode mode0 = insn_operand_mode[icode][1];

      if (target)
	temp = target;
      else
	temp = gen_reg_rtx (mode);

      if (GET_MODE (op0) != VOIDmode
	  && GET_MODE (op0) != mode0)
	op0 = convert_to_mode (mode0, op0, unsignedp);

      /* Now, if insn requires register operands, put operands into regs.  */

      if (! (*insn_operand_predicate[icode][1]) (op0, mode0))
	op0 = force_reg (mode0, op0);

      if (! (*insn_operand_predicate[icode][0]) (temp, mode))
	temp = gen_reg_rtx (mode);

      emit_insn (GEN_FCN (icode) (temp, op0));
      return temp;
    }
  else if (unoptab->handlers[(int) mode].lib_call)
    {
      rtx insn_before, insn_last;
      rtx funexp = gen_rtx (SYMBOL_REF, Pmode,
			    unoptab->handlers[(int) mode].lib_call);

      /* Pass the address through a pseudoreg, if desired,
	 before the "beginning" of the library call (for deletion).  */
#ifndef NO_FUNCTION_CSE
      if (! flag_no_function_cse)
	funexp = copy_to_mode_reg (Pmode, funexp);
#endif

      insn_before = get_last_insn ();

      /* Cannot pass FUNEXP since  emit_library_call insists
	 on getting a SYMBOL_REF.  But cse will make this SYMBOL_REF
	 be replaced with the copy we made just above.  */
      /* Pass 1 for NO_QUEUE so we don't lose any increments
	 if the libcall is cse'd or moved.  */
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  unoptab->handlers[(int) mode].lib_call),
			 1, mode, 1, op0, mode);
      target = hard_libcall_value (mode);
      temp = copy_to_reg (target);
      insn_last = get_last_insn ();
      REG_NOTES (insn_last)
	= gen_rtx (EXPR_LIST, REG_EQUAL,
		   gen_rtx (unoptab->code, mode, op0),
		   gen_rtx (INSN_LIST, REG_RETVAL,
			    NEXT_INSN (insn_before),
			    REG_NOTES (insn_last)));
      REG_NOTES (NEXT_INSN (insn_before))
	= gen_rtx (INSN_LIST, REG_LIBCALL, insn_last,
		   REG_NOTES (NEXT_INSN (insn_before)));
      return temp;
    }

  /* It can't be done in this mode.  Can we do it in a wider mode?  */

  if (class == MODE_INT || class == MODE_FLOAT)
    {
      for (wider_mode = INC_MODE (mode);
	   ((int) wider_mode < (int) MAX_MACHINE_MODE
	    && GET_MODE_CLASS (wider_mode) == class);
	   wider_mode = INC_MODE (wider_mode))
	{
	  if ((unoptab->handlers[(int) wider_mode].insn_code
	       != CODE_FOR_nothing)
	      || unoptab->handlers[(int) wider_mode].lib_call)
	    {
	      if (GET_MODE (op0) != VOIDmode)
		{
		  temp = gen_reg_rtx (wider_mode);
		  convert_move (temp, op0, unsignedp);
		  op0 = temp;
		}
	      
	      target = expand_unop (wider_mode, unoptab, op0, 0, unsignedp);
	      if (class == MODE_FLOAT)
		{
		  if (target == 0)
		    target = gen_reg_rtx (mode);
		  convert_move (target, temp, 0);
		  return target;
		}
	      else
		return gen_lowpart (mode, target);
	    }
	}
    }

  return 0;
}

/* Generate an instruction whose insn-code is INSN_CODE,
   with two operands: an output TARGET and an input OP0.
   TARGET *must* be nonzero, and the output is always stored there.
   CODE is an rtx code such that (CODE OP0) is an rtx that describes
   the value that is stored into TARGET.  */

void
emit_unop_insn (icode, target, op0, code)
     int icode;
     rtx target;
     rtx op0;
     enum rtx_code code;
{
  register rtx temp;
  enum machine_mode mode0 = insn_operand_mode[icode][1];
  rtx insn;
  rtx prev_insn;

  temp = target = protect_from_queue (target, 1);

  op0 = protect_from_queue (op0, 0);

  if (flag_force_mem)
    op0 = force_not_mem (op0);

  /* Now, if insn requires register operands, put operands into regs.  */

  if (! (*insn_operand_predicate[icode][1]) (op0, mode0))
    op0 = force_reg (mode0, op0);

  if (! (*insn_operand_predicate[icode][0]) (temp, GET_MODE (temp))
      || (flag_force_mem && GET_CODE (temp) == MEM))
    temp = gen_reg_rtx (GET_MODE (temp));

  prev_insn = get_last_insn ();
  insn = emit_insn (GEN_FCN (icode) (temp, op0));

  /* If we just made a multi-insn sequence,
     record in the last insn an equivalent expression for its value
     and a pointer to the first insn.  This makes cse possible.  */
  if (code != UNKNOWN && PREV_INSN (insn) != prev_insn)
    REG_NOTES (insn)
      = gen_rtx (EXPR_LIST, REG_EQUAL,
		 gen_rtx (code, GET_MODE (temp), op0),
		 REG_NOTES (insn));
  
  if (temp != target)
    emit_move_insn (target, temp);
}

/* Generate code to store zero in X.  */

void
emit_clr_insn (x)
     rtx x;
{
  emit_move_insn (x, const0_rtx);
}

/* Generate code to store 1 in X
   assuming it contains zero beforehand.  */

void
emit_0_to_1_insn (x)
     rtx x;
{
  emit_move_insn (x, const1_rtx);
}

/* Generate code to compare X with Y
   so that the condition codes are set.

   UNSIGNEDP nonzero says that X and Y are unsigned;
   this matters if they need to be widened.

   If they have mode BLKmode, then SIZE specifies the size of both X and Y,
   and ALIGN specifies the known shared alignment of X and Y.  */

void
emit_cmp_insn (x, y, size, unsignedp, align)
     rtx x, y;
     rtx size;
     int unsignedp;
     int align;
{
  enum machine_mode mode = GET_MODE (x);
  enum mode_class class;
  enum machine_mode wider_mode;

  if (mode == VOIDmode) mode = GET_MODE (y);
  /* They could both be VOIDmode if both args are immediate constants,
     but we should fold that at an earlier stage.
     With no special code here, this will call abort,
     reminding the programmer to implement such folding.  */

  class = GET_MODE_CLASS (mode);

  if (mode != BLKmode && flag_force_mem)
    {
      x = force_not_mem (x);
      y = force_not_mem (y);
    }

  /* Handle all BLKmode compares.  */

  if (mode == BLKmode)
    {
      emit_queue ();
      x = protect_from_queue (x, 0);
      y = protect_from_queue (y, 0);

      if (size == 0)
	abort ();
#ifdef HAVE_cmpstrqi
      if (HAVE_cmpstrqi
	  && GET_CODE (size) == CONST_INT
	  && INTVAL (size) < (1 << GET_MODE_BITSIZE (QImode)))
	emit_insn (gen_cmpstrqi (x, y, size,
				 gen_rtx (CONST_INT, VOIDmode, align)));
      else
#endif
#ifdef HAVE_cmpstrhi
      if (HAVE_cmpstrhi
	  && GET_CODE (size) == CONST_INT
	  && INTVAL (size) < (1 << GET_MODE_BITSIZE (HImode)))
	emit_insn (gen_cmpstrhi (x, y, size,
				 gen_rtx (CONST_INT, VOIDmode, align)));
      else
#endif
#ifdef HAVE_cmpstrsi
      if (HAVE_cmpstrsi)
	emit_insn (gen_cmpstrsi (x, y, convert_to_mode (SImode, size, 1),
				 gen_rtx (CONST_INT, VOIDmode, align)));
      else
#endif
	{
#ifdef TARGET_MEM_FUNCTIONS
	  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memcmp"), 0, 
			     SImode, 3,
			     XEXP (x, 0), Pmode, XEXP (y, 0), Pmode,
			     size, Pmode);
#else
	  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcmp"), 0,
			     SImode, 3,
			     XEXP (x, 0), Pmode, XEXP (y, 0), Pmode,
			     size, Pmode);
#endif
	  emit_cmp_insn (hard_libcall_value (SImode), const0_rtx, 0, 0, 0);
	}
      return;
    }

  /* Handle some compares against zero.  */

  if (y == CONST0_RTX (mode)
      && tst_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) tst_optab->handlers[(int) mode].insn_code;

      emit_queue ();
      x = protect_from_queue (x, 0);
      y = protect_from_queue (y, 0);

      /* Now, if insn requires register operands, put operands into regs.  */
      if (! (*insn_operand_predicate[icode][0])
	  (x, insn_operand_mode[icode][0]))
	x = force_reg (insn_operand_mode[icode][0], x);

      emit_insn (GEN_FCN (icode) (x));
      return;
    }

  /* Handle compares for which there is a directly suitable insn.  */

  if (cmp_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) cmp_optab->handlers[(int) mode].insn_code;

      emit_queue ();
      x = protect_from_queue (x, 0);
      y = protect_from_queue (y, 0);

      /* Now, if insn requires register operands, put operands into regs.  */
      if (! (*insn_operand_predicate[icode][0])
	  (x, insn_operand_mode[icode][0]))
	x = force_reg (insn_operand_mode[icode][0], x);

      if (! (*insn_operand_predicate[icode][1])
	  (y, insn_operand_mode[icode][1]))
	y = force_reg (insn_operand_mode[icode][1], y);

      emit_insn (GEN_FCN (icode) (x, y));
      return;
    }

  /* Try widening if we can find a direct insn that way.  */

  if (class == MODE_INT || class == MODE_FLOAT)
    {
      for (wider_mode = INC_MODE (mode);
	   ((int) wider_mode < (int) MAX_MACHINE_MODE
	    && GET_MODE_CLASS (wider_mode) == class);
	   wider_mode = INC_MODE (wider_mode))
	{
	  if (cmp_optab->handlers[(int) wider_mode].insn_code
	      != CODE_FOR_nothing)
	    {
	      x = convert_to_mode (wider_mode, x, unsignedp);
	      y = convert_to_mode (wider_mode, y, unsignedp);
	      emit_cmp_insn (x, y, 0, unsignedp, align);
	      return;
	    }
	}
    }

  /* Handle a lib call just for the mode we are using.  */

  if (cmp_optab->handlers[(int) mode].lib_call)
    {
      char *string = cmp_optab->handlers[(int) mode].lib_call;
      /* If we want unsigned, and this mode has a distinct unsigned
	 comparison routine, use that.  */
      if (unsignedp && ucmp_optab->handlers[(int) mode].lib_call)
	string = ucmp_optab->handlers[(int) mode].lib_call;

      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, string), 0,
			 SImode, 2, x, mode, y, mode);

      /* Integer comparison returns a result that must be compared against 1,
	 so that even if we do an unsigned compare afterward,
	 there is still a value that can represent the result "less than".  */
      if (GET_MODE_CLASS (mode) == MODE_INT)
	emit_cmp_insn (hard_libcall_value (SImode), const1_rtx, 0, unsignedp, 0);
      else
	emit_cmp_insn (hard_libcall_value (SImode), const0_rtx, 0, 0, 0);
      return;
    }

  /* Try widening and then using a libcall.  */

  if (class == MODE_FLOAT)
    {
      for (wider_mode = INC_MODE (mode);
	   ((int) wider_mode < (int) MAX_MACHINE_MODE
	    && GET_MODE_CLASS (wider_mode) == class);
	   wider_mode = INC_MODE (wider_mode))
	{
	  if ((cmp_optab->handlers[(int) wider_mode].insn_code
	       != CODE_FOR_nothing)
	      || (cmp_optab->handlers[(int) wider_mode].lib_call != 0))
	    {
	      x = convert_to_mode (wider_mode, x, unsignedp);
	      y = convert_to_mode (wider_mode, y, unsignedp);
	      emit_cmp_insn (x, y, 0, unsignedp, align);
	    }
	}
      return;
    }

  abort ();
}

/* These three functions generate an insn body and return it
   rather than emitting the insn.

   They do not protect from queued increments,
   because they may be used 1) in protect_from_queue itself
   and 2) in other passes where there is no queue.  */

/* Generate and return an insn body to add Y to X.  */

rtx
gen_add2_insn (x, y)
     rtx x, y;
{
  return (GEN_FCN (add_optab->handlers[(int) GET_MODE (x)].insn_code)
	  (x, x, y));
}

int
have_add2_insn (mode)
     enum machine_mode mode;
{
  return add_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing;
}

/* Generate and return an insn body to subtract Y from X.  */

rtx
gen_sub2_insn (x, y)
     rtx x, y;
{
  return (GEN_FCN (sub_optab->handlers[(int) GET_MODE (x)].insn_code)
	  (x, x, y));
}

int
have_sub2_insn (mode)
     enum machine_mode mode;
{
  return add_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing;
}

/* Generate the body of an instruction to copy Y into X.  */

rtx
gen_move_insn (x, y)
     rtx x, y;
{
  register enum machine_mode mode = GET_MODE (x);
  if (mode == VOIDmode)
    mode = GET_MODE (y);
  return (GEN_FCN (mov_optab->handlers[(int) mode].insn_code) (x, y));
}

#if 0
/* Tables of patterns for extending one integer mode to another.  */
enum insn_code zero_extend_optab[MAX_MACHINE_MODE][MAX_MACHINE_MODE];
enum insn_code sign_extend_optab[MAX_MACHINE_MODE][MAX_MACHINE_MODE];

/* Generate the body of an insn to extend Y (with mode MFROM)
   into X (with mode MTO).  Do zero-extension if UNSIGNEDP is nonzero.  */

rtx
gen_extend_insn (x, y, mto, mfrom, unsignedp)
     rtx x, y;
     enum machine_mode mto, mfrom;
     int unsignedp;
{
  return (GEN_FCN ((unsignedp ? zero_extend_optab : sign_extend_optab)
		   [(int)mto][(int)mfrom])
	  (x, y));
}

static void
init_extends ()
{
  bzero (sign_extend_optab, sizeof sign_extend_optab);
  bzero (zero_extend_optab, sizeof zero_extend_optab);
  sign_extend_optab[(int) SImode][(int) HImode] = CODE_FOR_extendhisi2;
  sign_extend_optab[(int) SImode][(int) QImode] = CODE_FOR_extendqisi2;
  sign_extend_optab[(int) HImode][(int) QImode] = CODE_FOR_extendqihi2;
  zero_extend_optab[(int) SImode][(int) HImode] = CODE_FOR_zero_extendhisi2;
  zero_extend_optab[(int) SImode][(int) QImode] = CODE_FOR_zero_extendqisi2;
  zero_extend_optab[(int) HImode][(int) QImode] = CODE_FOR_zero_extendqihi2;
}
#endif

/* can_fix_p and can_float_p say whether the target machine
   can directly convert a given fixed point type to
   a given floating point type, or vice versa.
   The returned value is the CODE_FOR_... value to use,
   or CODE_FOR_nothing if these modes cannot be directly converted.  */

static enum insn_code fixtab[2][2][2];
static enum insn_code fixtrunctab[2][2][2];
static enum insn_code floattab[2][2];

/* *TRUNCP_PTR is set to 1 if it is necessary to output
   an explicit FTRUNC insn before the fix insn; otherwise 0.  */

static enum insn_code
can_fix_p (fixmode, fltmode, unsignedp, truncp_ptr)
     enum machine_mode fltmode, fixmode;
     int unsignedp;
     int *truncp_ptr;
{
  *truncp_ptr = 0;
  if (fixtrunctab[fltmode != SFmode][fixmode == DImode][unsignedp]
      != CODE_FOR_nothing)
    return fixtrunctab[fltmode != SFmode][fixmode == DImode][unsignedp];
  if (ftrunc_optab->handlers[(int) fltmode].insn_code != CODE_FOR_nothing)
    {
      *truncp_ptr = 1;
      return fixtab[fltmode != SFmode][fixmode == DImode][unsignedp];
    }
  return CODE_FOR_nothing;
}

static enum insn_code
can_float_p (fltmode, fixmode)
     enum machine_mode fixmode, fltmode;
{
  return floattab[fltmode != SFmode][fixmode == DImode];
}

void
init_fixtab ()
{
  enum insn_code *p;
  for (p = fixtab[0][0];
       p < fixtab[0][0] + sizeof fixtab / sizeof (fixtab[0][0][0]); 
       p++)
    *p = CODE_FOR_nothing;
  for (p = fixtrunctab[0][0];
       p < fixtrunctab[0][0] + sizeof fixtrunctab / sizeof (fixtrunctab[0][0][0]); 
       p++)
    *p = CODE_FOR_nothing;

#ifdef HAVE_fixsfsi2
  if (HAVE_fixsfsi2)
    fixtab[0][0][0] = CODE_FOR_fixsfsi2;
#endif
#ifdef HAVE_fixsfdi2
  if (HAVE_fixsfdi2)
    fixtab[0][1][0] = CODE_FOR_fixsfdi2;
#endif
#ifdef HAVE_fixdfsi2
  if (HAVE_fixdfsi2)
    fixtab[1][0][0] = CODE_FOR_fixdfsi2;
#endif
#ifdef HAVE_fixdfdi2
  if (HAVE_fixdfdi2)
    fixtab[1][1][0] = CODE_FOR_fixdfdi2;
#endif

#ifdef HAVE_fixunssfsi2
  if (HAVE_fixunssfsi2)
    fixtab[0][0][1] = CODE_FOR_fixunssfsi2;
#endif
#ifdef HAVE_fixunssfdi2
  if (HAVE_fixunssfdi2)
    fixtab[0][1][1] = CODE_FOR_fixunssfdi2;
#endif
#ifdef HAVE_fixunsdfsi2
  if (HAVE_fixunsdfsi2)
    fixtab[1][0][1] = CODE_FOR_fixunsdfsi2;
#endif
#ifdef HAVE_fixunsdfdi2
  if (HAVE_fixunsdfdi2)
    fixtab[1][1][1] = CODE_FOR_fixunsdfdi2;
#endif

#ifdef HAVE_fix_truncsfsi2
  if (HAVE_fix_truncsfsi2)
    fixtrunctab[0][0][0] = CODE_FOR_fix_truncsfsi2;
#endif
#ifdef HAVE_fix_truncsfdi2
  if (HAVE_fix_truncsfdi2)
    fixtrunctab[0][1][0] = CODE_FOR_fix_truncsfdi2;
#endif
#ifdef HAVE_fix_truncdfsi2
  if (HAVE_fix_truncdfsi2)
    fixtrunctab[1][0][0] = CODE_FOR_fix_truncdfsi2;
#endif
#ifdef HAVE_fix_truncdfdi2
  if (HAVE_fix_truncdfdi2)
    fixtrunctab[1][1][0] = CODE_FOR_fix_truncdfdi2;
#endif

#ifdef HAVE_fixuns_truncsfsi2
  if (HAVE_fixuns_truncsfsi2)
    fixtrunctab[0][0][1] = CODE_FOR_fixuns_truncsfsi2;
#endif
#ifdef HAVE_fixuns_truncsfdi2
  if (HAVE_fixuns_truncsfdi2)
    fixtrunctab[0][1][1] = CODE_FOR_fixuns_truncsfdi2;
#endif
#ifdef HAVE_fixuns_truncdfsi2
  if (HAVE_fixuns_truncdfsi2)
    fixtrunctab[1][0][1] = CODE_FOR_fixuns_truncdfsi2;
#endif
#ifdef HAVE_fixuns_truncdfdi2
  if (HAVE_fixuns_truncdfdi2)
    fixtrunctab[1][1][1] = CODE_FOR_fixuns_truncdfdi2;
#endif

#ifdef FIXUNS_TRUNC_LIKE_FIX_TRUNC
  /* This flag says the same insns that convert to a signed fixnum
     also convert validly to an unsigned one.  */
  {
    int i;
    int j;
    for (i = 0; i < 2; i++)
      for (j = 0; j < 2; j++)
	fixtrunctab[i][j][1] = fixtrunctab[i][j][0];
  }
#endif
}

void
init_floattab ()
{
  enum insn_code *p;
  for (p = floattab[0];
       p < floattab[0] + sizeof floattab / sizeof (floattab[0][0]); 
       p++)
    *p = CODE_FOR_nothing;

#ifdef HAVE_floatsisf2
  if (HAVE_floatsisf2)
    floattab[0][0] = CODE_FOR_floatsisf2;
#endif
#ifdef HAVE_floatdisf2
  if (HAVE_floatdisf2)
    floattab[0][1] = CODE_FOR_floatdisf2;
#endif
#ifdef HAVE_floatsidf2
  if (HAVE_floatsidf2)
    floattab[1][0] = CODE_FOR_floatsidf2;
#endif
#ifdef HAVE_floatdidf2
  if (HAVE_floatdidf2)
    floattab[1][1] = CODE_FOR_floatdidf2;
#endif
}

/* Generate code to convert FROM to floating point
   and store in TO.  FROM must be fixed point.
   UNSIGNEDP nonzero means regard FROM as unsigned.
   Normally this is done by correcting the final value
   if it is negative.  */

void
expand_float (real_to, from, unsignedp)
     rtx real_to, from;
     int unsignedp;
{
  enum insn_code icode;
  register rtx to;

  /* Constants should get converted in `fold'.
     We lose here since we don't know the mode.  */
  if (GET_MODE (from) == VOIDmode)
    abort ();

  to = real_to = protect_from_queue (real_to, 1);
  from = protect_from_queue (from, 0);

  if (flag_force_mem)
    {
      from = force_not_mem (from);
    }

  /* If we are about to do some arithmetic to correct for an
     unsigned operand, do it in a pseudo-register.  */

  if (unsignedp
      && ! (GET_CODE (to) == REG && REGNO (to) >= FIRST_PSEUDO_REGISTER))
    to = gen_reg_rtx (GET_MODE (to));

  /* Now do the basic conversion.  Do it in the specified modes if possible;
     otherwise convert either input, output or both with wider mode;
     otherwise use a library call.  */

  if ((icode = can_float_p (GET_MODE (to), GET_MODE (from)))
      != CODE_FOR_nothing)
    {
      emit_unop_insn (icode, to, from, FLOAT);
    }
  else if (GET_MODE (to) == SFmode
	   && ((icode = can_float_p (DFmode, GET_MODE (from)))
	       != CODE_FOR_nothing))
    {
      to = gen_reg_rtx (DFmode);
      emit_unop_insn (icode, to, from, FLOAT);
    }
  /* If we can't float a SI, maybe we can float a DI.
     If so, convert to DI and then float.  */
  else if (GET_MODE (from) != DImode
	   && (can_float_p (GET_MODE (to), DImode) != CODE_FOR_nothing
	       || can_float_p (DFmode, DImode) != CODE_FOR_nothing))
    {
      register rtx tem = gen_reg_rtx (DImode);
      convert_move (tem, from, unsignedp);
      from = tem;
      /* If we extend FROM then we don't need to correct
	 the final value for unsignedness.  */
      unsignedp = 0;

      if ((icode = can_float_p (GET_MODE (to), GET_MODE (from)))
	  != CODE_FOR_nothing)
	{
	  emit_unop_insn (icode, to, from, FLOAT);
	}
      else if ((icode = can_float_p (DFmode, DImode))
	        != CODE_FOR_nothing)
	{
	  to = gen_reg_rtx (DFmode);
	  emit_unop_insn (icode, to, from, FLOAT);
	}
    }
  /* No hardware instruction available; call a library
     to convert from SImode or DImode into DFmode.  */
  else
    {
      if (GET_MODE_SIZE (GET_MODE (from)) < GET_MODE_SIZE (SImode))
	{
	  from = convert_to_mode (SImode, from, unsignedp);
	  unsignedp = 0;
	}
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  (GET_MODE (from) == SImode ? "__floatsidf"
				   : "__floatdidf")),
			 0, DFmode, 1, from, GET_MODE (from));
      to = copy_to_reg (hard_libcall_value (DFmode));
    }

  /* If FROM was unsigned but we treated it as signed,
     then in the case where it is negative (and therefore TO is negative),
     correct its value by 2**bitwidth.  */

  if (unsignedp)
    {
      rtx label = gen_label_rtx ();
      rtx temp;
      REAL_VALUE_TYPE offset;

      do_pending_stack_adjust ();
      emit_cmp_insn (to, GET_MODE (to) == DFmode ? dconst0_rtx : fconst0_rtx,
		     0, 0, 0);
      emit_jump_insn (gen_bge (label));
      offset = REAL_VALUE_LDEXP (1.0, GET_MODE_BITSIZE (GET_MODE (from)));
      temp = expand_binop (GET_MODE (to), add_optab, to,
			   immed_real_const_1 (offset, GET_MODE (to)),
			   to, 0, OPTAB_LIB_WIDEN);
      if (temp != to)
	emit_move_insn (to, temp);
      do_pending_stack_adjust ();
      emit_label (label);
    }

  /* Copy result to requested destination
     if we have been computing in a temp location.  */

  if (to != real_to)
    {
      if (GET_MODE (real_to) == GET_MODE (to))
	emit_move_insn (real_to, to);
      else
	convert_move (real_to, to, 0);
    }
}

/* expand_fix: generate code to convert FROM to fixed point
   and store in TO.  FROM must be floating point.  */

static rtx
ftruncify (x)
     rtx x;
{
  rtx temp = gen_reg_rtx (GET_MODE (x));
  return expand_unop (GET_MODE (x), ftrunc_optab, x, temp, 0);
}

void
expand_fix (to, from, unsignedp)
     register rtx to, from;
     int unsignedp;
{
  enum insn_code icode;
  register rtx target;
  int must_trunc = 0;

  while (1)
    {
      icode = can_fix_p (GET_MODE (to), GET_MODE (from), unsignedp, &must_trunc);
      if (icode != CODE_FOR_nothing)
	{
	  if (must_trunc)
	    from = ftruncify (from);

	  emit_unop_insn (icode, to, from, FIX);
	  return;
	}

#if 0  /* Turned off.  It fails because the positive numbers
	  that become temporarily negative are rounded up instead of down.  */

      /* If no insns for unsigned conversion,
	 we can go via a signed number.
	 But make sure we won't overflow in the compiler.  */
      if (unsignedp && GET_MODE_BITSIZE (GET_MODE (to)) <= HOST_BITS_PER_INT
	  /* Make sure we won't lose significant bits doing this.  */
	  && GET_MODE_BITSIZE (GET_MODE (from)) > GET_MODE_BITSIZE (GET_MODE (to)))
	{
	  icode = can_fix_p (GET_MODE (to), GET_MODE (from),
			     0, &must_trunc);

	  if (icode != CODE_FOR_nothing)
	    {
	      REAL_VALUE_TYPE offset;
	      rtx temp, temp1;
	      int bitsize = GET_MODE_BITSIZE (GET_MODE (to));

	      if (must_trunc)
		from = ftruncify (from);

	      /* Subtract 2**(N-1), convert to signed number,
		 then add 2**(N-1).  */
	      offset = REAL_VALUE_LDEXP (1.0, bitsize - 1);
	      temp = expand_binop (GET_MODE (from), sub_optab, from,
				   immed_real_const_1 (offset, GET_MODE (from)),
				   0, 0, OPTAB_LIB_WIDEN);

	      temp1 = gen_reg_rtx (GET_MODE (to));
	      emit_unop_insn (icode, temp1, temp, FIX);
	      temp = expand_binop (GET_MODE (to), add_optab, temp1,
				   gen_rtx (CONST_INT, VOIDmode,
					    1 << (bitsize - 1)),
				   to, 1, OPTAB_LIB_WIDEN);
	      if (temp != to)
		emit_move_insn (to, temp);
	      return;
	    }
	}
#endif
      icode = can_fix_p (DImode, GET_MODE (from), unsignedp, &must_trunc);

      if (GET_MODE (to) != DImode && icode != CODE_FOR_nothing)
	{
	  register rtx temp = gen_reg_rtx (DImode);

	  if (must_trunc)
	    from = ftruncify (from);
	  emit_unop_insn (icode, temp, from, FIX);
	  convert_move (to, temp, unsignedp);
	  return;
	}

      /* If FROM is not DFmode, convert to DFmode and try again from there.  */
      if (GET_MODE (from) == DFmode)
	break;

      from = convert_to_mode (DFmode, from, 0);
    }

  /* We can't do it with an insn, so use a library call.
     The mode of FROM is known to be DFmode.  */

  to = protect_from_queue (to, 1);
  from = protect_from_queue (from, 0);

  if (flag_force_mem)
    from = force_not_mem (from);

  if (GET_MODE (to) != DImode)
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  unsignedp ? "__fixunsdfsi"
				  : "__fixdfsi"),
			 0, SImode, 1, from, DFmode);
      target = hard_libcall_value (SImode);
    }
  else
    {
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode,
				  unsignedp ? "__fixunsdfdi"
				  : "__fixdfdi"),
			 0, DImode, 1, from, DFmode);
      target = hard_libcall_value (DImode);
    }

  if (GET_MODE (to) == GET_MODE (target))
    emit_move_insn (to, target);
  else
    convert_move (to, target, 0);
}

static optab
init_optab (code)
     enum rtx_code code;
{
  int i;
  optab op = (optab) malloc (sizeof (struct optab));
  op->code = code;
  for (i = 0; i < NUM_MACHINE_MODES; i++)
    {
      op->handlers[i].insn_code = CODE_FOR_nothing;
      op->handlers[i].lib_call = 0;
    }
  return op;
}

/* Call this once to initialize the contents of the optabs
   appropriately for the current target machine.  */

void
init_optabs ()
{
  init_fixtab ();
  init_floattab ();
  init_comparisons ();
/*  init_extends (); */

  add_optab = init_optab (PLUS);
  sub_optab = init_optab (MINUS);
  smul_optab = init_optab (MULT);
  umul_optab = init_optab (UMULT);
  smul_widen_optab = init_optab (MULT);
  umul_widen_optab = init_optab (UMULT);
  sdiv_optab = init_optab (DIV);
  sdivmod_optab = init_optab (UNKNOWN);
  udiv_optab = init_optab (UDIV);
  udivmod_optab = init_optab (UNKNOWN);
  smod_optab = init_optab (MOD);
  umod_optab = init_optab (UMOD);
  flodiv_optab = init_optab (DIV);
  ftrunc_optab = init_optab (UNKNOWN);
  and_optab = init_optab (AND);
  andcb_optab = init_optab (UNKNOWN);
  ior_optab = init_optab (IOR);
  xor_optab = init_optab (XOR);
  ashl_optab = init_optab (ASHIFT);
  ashr_optab = init_optab (ASHIFTRT);
  lshl_optab = init_optab (LSHIFT);
  lshr_optab = init_optab (LSHIFTRT);
  rotl_optab = init_optab (ROTATE);
  rotr_optab = init_optab (ROTATERT);
  mov_optab = init_optab (UNKNOWN);
  movstrict_optab = init_optab (UNKNOWN);
  cmp_optab = init_optab (UNKNOWN);
  ucmp_optab = init_optab (UNKNOWN);
  tst_optab = init_optab (UNKNOWN);
  neg_optab = init_optab (NEG);
  abs_optab = init_optab (ABS);
  one_cmpl_optab = init_optab (NOT);
  ffs_optab = init_optab (FFS);

#ifdef HAVE_addqi3
  if (HAVE_addqi3)
    add_optab->handlers[(int) QImode].insn_code = CODE_FOR_addqi3;
#endif
#ifdef HAVE_addhi3
  if (HAVE_addhi3)
    add_optab->handlers[(int) HImode].insn_code = CODE_FOR_addhi3;
#endif
#ifdef HAVE_addsi3
  if (HAVE_addsi3)
    add_optab->handlers[(int) SImode].insn_code = CODE_FOR_addsi3;
#endif
#ifdef HAVE_adddi3
  if (HAVE_adddi3)
    add_optab->handlers[(int) DImode].insn_code = CODE_FOR_adddi3;
#endif
#ifdef HAVE_addsf3
  if (HAVE_addsf3)
    add_optab->handlers[(int) SFmode].insn_code = CODE_FOR_addsf3;
#endif
#ifdef HAVE_adddf3
  if (HAVE_adddf3)
    add_optab->handlers[(int) DFmode].insn_code = CODE_FOR_adddf3;
#endif
  add_optab->handlers[(int) DImode].lib_call = "__adddi3";
  add_optab->handlers[(int) SFmode].lib_call = "__addsf3";
  add_optab->handlers[(int) DFmode].lib_call = "__adddf3";

#ifdef HAVE_subqi3
  if (HAVE_subqi3)
    sub_optab->handlers[(int) QImode].insn_code = CODE_FOR_subqi3;
#endif
#ifdef HAVE_subhi3
  if (HAVE_subhi3)
    sub_optab->handlers[(int) HImode].insn_code = CODE_FOR_subhi3;
#endif
#ifdef HAVE_subsi3
  if (HAVE_subsi3)
    sub_optab->handlers[(int) SImode].insn_code = CODE_FOR_subsi3;
#endif
#ifdef HAVE_subdi3
  if (HAVE_subdi3)
    sub_optab->handlers[(int) DImode].insn_code = CODE_FOR_subdi3;
#endif
#ifdef HAVE_subsf3
  if (HAVE_subsf3)
    sub_optab->handlers[(int) SFmode].insn_code = CODE_FOR_subsf3;
#endif
#ifdef HAVE_subdf3
  if (HAVE_subdf3)
    sub_optab->handlers[(int) DFmode].insn_code = CODE_FOR_subdf3;
#endif
  sub_optab->handlers[(int) DImode].lib_call = "__subdi3";
  sub_optab->handlers[(int) SFmode].lib_call = "__subsf3";
  sub_optab->handlers[(int) DFmode].lib_call = "__subdf3";

#ifdef HAVE_mulqi3
  if (HAVE_mulqi3)
    smul_optab->handlers[(int) QImode].insn_code = CODE_FOR_mulqi3;
#endif
#ifdef HAVE_mulhi3
  if (HAVE_mulhi3)
    smul_optab->handlers[(int) HImode].insn_code = CODE_FOR_mulhi3;
#endif
#ifdef HAVE_mulsi3
  if (HAVE_mulsi3)
    smul_optab->handlers[(int) SImode].insn_code = CODE_FOR_mulsi3;
#endif
#ifdef HAVE_muldi3
  if (HAVE_muldi3)
    smul_optab->handlers[(int) DImode].insn_code = CODE_FOR_muldi3;
#endif
#ifdef HAVE_mulsf3
  if (HAVE_mulsf3)
    smul_optab->handlers[(int) SFmode].insn_code = CODE_FOR_mulsf3;
#endif
#ifdef HAVE_muldf3
  if (HAVE_muldf3)
    smul_optab->handlers[(int) DFmode].insn_code = CODE_FOR_muldf3;
#endif

#ifdef MULSI3_LIBCALL
  smul_optab->handlers[(int) SImode].lib_call = MULSI3_LIBCALL;
#else
  smul_optab->handlers[(int) SImode].lib_call = "__mulsi3";
#endif
  smul_optab->handlers[(int) DImode].lib_call = "__muldi3";
  smul_optab->handlers[(int) SFmode].lib_call = "__mulsf3";
  smul_optab->handlers[(int) DFmode].lib_call = "__muldf3";

#ifdef HAVE_mulqihi3
  if (HAVE_mulqihi3)
    smul_widen_optab->handlers[(int) HImode].insn_code = CODE_FOR_mulqihi3;
#endif
#ifdef HAVE_mulhisi3
  if (HAVE_mulhisi3)
    smul_widen_optab->handlers[(int) SImode].insn_code = CODE_FOR_mulhisi3;
#endif
#ifdef HAVE_mulsidi3
  if (HAVE_mulsidi3)
    smul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_mulsidi3;
#endif

#ifdef HAVE_umulqi3
  if (HAVE_umulqi3)
    umul_optab->handlers[(int) QImode].insn_code = CODE_FOR_umulqi3;
#endif
#ifdef HAVE_umulhi3
  if (HAVE_umulhi3)
    umul_optab->handlers[(int) HImode].insn_code = CODE_FOR_umulhi3;
#endif
#ifdef HAVE_umulsi3
  if (HAVE_umulsi3)
    umul_optab->handlers[(int) SImode].insn_code = CODE_FOR_umulsi3;
#endif
#ifdef HAVE_umuldi3
  if (HAVE_umuldi3)
    umul_optab->handlers[(int) DImode].insn_code = CODE_FOR_umuldi3;
#endif
#ifdef HAVE_umulsf3
  if (HAVE_umulsf3)
    umul_optab->handlers[(int) SFmode].insn_code = CODE_FOR_umulsf3;
#endif
#ifdef HAVE_umuldf3
  if (HAVE_umuldf3)
    umul_optab->handlers[(int) DFmode].insn_code = CODE_FOR_umuldf3;
#endif

#ifdef UMULSI3_LIBCALL
  umul_optab->handlers[(int) SImode].lib_call = UMULSI3_LIBCALL;
#else
  umul_optab->handlers[(int) SImode].lib_call = "__umulsi3";
#endif
  umul_optab->handlers[(int) DImode].lib_call = "__umuldi3";
  umul_optab->handlers[(int) SFmode].lib_call = "__umulsf3";
  umul_optab->handlers[(int) DFmode].lib_call = "__umuldf3";

#ifdef HAVE_umulqihi3
  if (HAVE_umulqihi3)
    umul_widen_optab->handlers[(int) HImode].insn_code = CODE_FOR_umulqihi3;
#endif
#ifdef HAVE_umulhisi3
  if (HAVE_umulhisi3)
    umul_widen_optab->handlers[(int) SImode].insn_code = CODE_FOR_umulhisi3;
#endif
#ifdef HAVE_umulsidi3
  if (HAVE_umulsidi3)
    umul_widen_optab->handlers[(int) DImode].insn_code = CODE_FOR_umulsidi3;
#endif

#ifdef HAVE_divqi3
  if (HAVE_divqi3)
    sdiv_optab->handlers[(int) QImode].insn_code = CODE_FOR_divqi3;
#endif
#ifdef HAVE_divhi3
  if (HAVE_divhi3)
    sdiv_optab->handlers[(int) HImode].insn_code = CODE_FOR_divhi3;
#endif
#ifdef HAVE_divsi3
  if (HAVE_divsi3)
    sdiv_optab->handlers[(int) SImode].insn_code = CODE_FOR_divsi3;
#endif
#ifdef HAVE_divdi3
  if (HAVE_divdi3)
    sdiv_optab->handlers[(int) DImode].insn_code = CODE_FOR_divdi3;
#endif

#ifdef DIVSI3_LIBCALL
  sdiv_optab->handlers[(int) SImode].lib_call = DIVSI3_LIBCALL;
#else
  sdiv_optab->handlers[(int) SImode].lib_call = "__divsi3";
#endif
  sdiv_optab->handlers[(int) DImode].lib_call = "__divdi3";

#ifdef HAVE_udivqi3
  if (HAVE_udivqi3)
    udiv_optab->handlers[(int) QImode].insn_code = CODE_FOR_udivqi3;
#endif
#ifdef HAVE_udivhi3
  if (HAVE_udivhi3)
    udiv_optab->handlers[(int) HImode].insn_code = CODE_FOR_udivhi3;
#endif
#ifdef HAVE_udivsi3
  if (HAVE_udivsi3)
    udiv_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivsi3;
#endif
#ifdef HAVE_udivdi3
  if (HAVE_udivdi3)
    udiv_optab->handlers[(int) DImode].insn_code = CODE_FOR_udivdi3;
#endif

#ifdef UDIVSI3_LIBCALL
  udiv_optab->handlers[(int) SImode].lib_call = UDIVSI3_LIBCALL;
#else
  udiv_optab->handlers[(int) SImode].lib_call = "__udivsi3";
#endif
  udiv_optab->handlers[(int) DImode].lib_call = "__udivdi3";

#ifdef HAVE_divmodqi4
  if (HAVE_divmodqi4)
    sdivmod_optab->handlers[(int) QImode].insn_code = CODE_FOR_divmodqi4;
#endif
#ifdef HAVE_divmodhi4
  if (HAVE_divmodhi4)
    sdivmod_optab->handlers[(int) HImode].insn_code = CODE_FOR_divmodhi4;
#endif
#ifdef HAVE_divmodsi4
  if (HAVE_divmodsi4)
    sdivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_divmodsi4;
#endif
#ifdef HAVE_divmoddi4
  if (HAVE_divmoddi4)
    sdivmod_optab->handlers[(int) DImode].insn_code = CODE_FOR_divmoddi4;
#endif

#ifdef HAVE_udivmodqi4
  if (HAVE_udivmodqi4)
    udivmod_optab->handlers[(int) QImode].insn_code = CODE_FOR_udivmodqi4;
#endif
#ifdef HAVE_udivmodhi4
  if (HAVE_udivmodhi4)
    udivmod_optab->handlers[(int) HImode].insn_code = CODE_FOR_udivmodhi4;
#endif
#ifdef HAVE_udivmodsi4
  if (HAVE_udivmodsi4)
    udivmod_optab->handlers[(int) SImode].insn_code = CODE_FOR_udivmodsi4;
#endif
#ifdef HAVE_udivmoddi4
  if (HAVE_udivmoddi4)
    udivmod_optab->handlers[(int) DImode].insn_code = CODE_FOR_udivmoddi4;
#endif

#ifdef HAVE_modqi3
  if (HAVE_modqi3)
    smod_optab->handlers[(int) QImode].insn_code = CODE_FOR_modqi3;
#endif
#ifdef HAVE_modhi3
  if (HAVE_modhi3)
    smod_optab->handlers[(int) HImode].insn_code = CODE_FOR_modhi3;
#endif
#ifdef HAVE_modsi3
  if (HAVE_modsi3)
    smod_optab->handlers[(int) SImode].insn_code = CODE_FOR_modsi3;
#endif
#ifdef HAVE_moddi3
  if (HAVE_moddi3)
    smod_optab->handlers[(int) DImode].insn_code = CODE_FOR_moddi3;
#endif

#ifdef MODSI3_LIBCALL
  smod_optab->handlers[(int) SImode].lib_call = MODSI3_LIBCALL;
#else
  smod_optab->handlers[(int) SImode].lib_call = "__modsi3";
#endif
  smod_optab->handlers[(int) DImode].lib_call = "__moddi3";

#ifdef HAVE_umodqi3
  if (HAVE_umodqi3)
    umod_optab->handlers[(int) QImode].insn_code = CODE_FOR_umodqi3;
#endif
#ifdef HAVE_umodhi3
  if (HAVE_umodhi3)
    umod_optab->handlers[(int) HImode].insn_code = CODE_FOR_umodhi3;
#endif
#ifdef HAVE_umodsi3
  if (HAVE_umodsi3)
    umod_optab->handlers[(int) SImode].insn_code = CODE_FOR_umodsi3;
#endif
#ifdef HAVE_umoddi3
  if (HAVE_umoddi3)
    umod_optab->handlers[(int) DImode].insn_code = CODE_FOR_umoddi3;
#endif

#ifdef UMODSI3_LIBCALL
  umod_optab->handlers[(int) SImode].lib_call = UMODSI3_LIBCALL;
#else
  umod_optab->handlers[(int) SImode].lib_call = "__umodsi3";
#endif
  umod_optab->handlers[(int) DImode].lib_call = "__umoddi3";

#ifdef HAVE_divsf3
  if (HAVE_divsf3)
    flodiv_optab->handlers[(int) SFmode].insn_code = CODE_FOR_divsf3;
#endif
#ifdef HAVE_divdf3
  if (HAVE_divdf3)
    flodiv_optab->handlers[(int) DFmode].insn_code = CODE_FOR_divdf3;
#endif
  flodiv_optab->handlers[(int) SFmode].lib_call = "__divsf3";
  flodiv_optab->handlers[(int) DFmode].lib_call = "__divdf3";

#ifdef HAVE_ftruncsf2
  if (HAVE_ftruncsf2)
    ftrunc_optab->handlers[(int) SFmode].insn_code = CODE_FOR_ftruncsf2;
#endif
#ifdef HAVE_ftruncdf2
  if (HAVE_ftruncdf2)
    ftrunc_optab->handlers[(int) DFmode].insn_code = CODE_FOR_ftruncdf2;
#endif

#ifdef HAVE_andqi3
  if (HAVE_andqi3)
    and_optab->handlers[(int) QImode].insn_code = CODE_FOR_andqi3;
#endif
#ifdef HAVE_andhi3
  if (HAVE_andhi3)
    and_optab->handlers[(int) HImode].insn_code = CODE_FOR_andhi3;
#endif
#ifdef HAVE_andsi3
  if (HAVE_andsi3)
    and_optab->handlers[(int) SImode].insn_code = CODE_FOR_andsi3;
#endif
#ifdef HAVE_anddi3
  if (HAVE_anddi3)
    and_optab->handlers[(int) DImode].insn_code = CODE_FOR_anddi3;
#endif
  and_optab->handlers[(int) DImode].lib_call = "__anddi3";

#ifdef HAVE_andcbqi3
  if (HAVE_andcbqi3)
    andcb_optab->handlers[(int) QImode].insn_code = CODE_FOR_andcbqi3;
#endif
#ifdef HAVE_andcbhi3
  if (HAVE_andcbhi3)
    andcb_optab->handlers[(int) HImode].insn_code = CODE_FOR_andcbhi3;
#endif
#ifdef HAVE_andcbsi3
  if (HAVE_andcbsi3)
    andcb_optab->handlers[(int) SImode].insn_code = CODE_FOR_andcbsi3;
#endif
#ifdef HAVE_andcbdi3
  if (HAVE_andcbdi3)
    andcb_optab->handlers[(int) DImode].insn_code = CODE_FOR_andcbdi3;
#endif
  andcb_optab->handlers[(int) DImode].lib_call = "__andcbdi3";

#ifdef HAVE_iorqi3
  if (HAVE_iorqi3)
    ior_optab->handlers[(int) QImode].insn_code = CODE_FOR_iorqi3;
#endif
#ifdef HAVE_iorhi3
  if (HAVE_iorhi3)
    ior_optab->handlers[(int) HImode].insn_code = CODE_FOR_iorhi3;
#endif
#ifdef HAVE_iorsi3
  if (HAVE_iorsi3)
    ior_optab->handlers[(int) SImode].insn_code = CODE_FOR_iorsi3;
#endif
#ifdef HAVE_iordi3
  if (HAVE_iordi3)
    ior_optab->handlers[(int) DImode].insn_code = CODE_FOR_iordi3;
#endif
  ior_optab->handlers[(int) DImode].lib_call = "__iordi3";

#ifdef HAVE_xorqi3
  if (HAVE_xorqi3)
    xor_optab->handlers[(int) QImode].insn_code = CODE_FOR_xorqi3;
#endif
#ifdef HAVE_xorhi3
  if (HAVE_xorhi3)
    xor_optab->handlers[(int) HImode].insn_code = CODE_FOR_xorhi3;
#endif
#ifdef HAVE_xorsi3
  if (HAVE_xorsi3)
    xor_optab->handlers[(int) SImode].insn_code = CODE_FOR_xorsi3;
#endif
#ifdef HAVE_xordi3
  if (HAVE_xordi3)
    xor_optab->handlers[(int) DImode].insn_code = CODE_FOR_xordi3;
#endif
  xor_optab->handlers[(int) DImode].lib_call = "__xordi3";

#ifdef HAVE_ashlqi3
  if (HAVE_ashlqi3)
    ashl_optab->handlers[(int) QImode].insn_code = CODE_FOR_ashlqi3;
#endif
#ifdef HAVE_ashlhi3
  if (HAVE_ashlhi3)
    ashl_optab->handlers[(int) HImode].insn_code = CODE_FOR_ashlhi3;
#endif
#ifdef HAVE_ashlsi3
  if (HAVE_ashlsi3)
    ashl_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashlsi3;
#endif
#ifdef HAVE_ashldi3
  if (HAVE_ashldi3)
    ashl_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashldi3;
#endif
  ashl_optab->handlers[(int) SImode].lib_call = "__ashlsi3";
  ashl_optab->handlers[(int) DImode].lib_call = "__ashldi3";

#ifdef HAVE_ashrqi3
  if (HAVE_ashrqi3)
    ashr_optab->handlers[(int) QImode].insn_code = CODE_FOR_ashrqi3;
#endif
#ifdef HAVE_ashrhi3
  if (HAVE_ashrhi3)
    ashr_optab->handlers[(int) HImode].insn_code = CODE_FOR_ashrhi3;
#endif
#ifdef HAVE_ashrsi3
  if (HAVE_ashrsi3)
    ashr_optab->handlers[(int) SImode].insn_code = CODE_FOR_ashrsi3;
#endif
#ifdef HAVE_ashrdi3
  if (HAVE_ashrdi3)
    ashr_optab->handlers[(int) DImode].insn_code = CODE_FOR_ashrdi3;
#endif
  ashr_optab->handlers[(int) SImode].lib_call = "__ashrsi3";
  ashr_optab->handlers[(int) DImode].lib_call = "__ashrdi3";

#ifdef HAVE_lshlqi3
  if (HAVE_lshlqi3)
    lshl_optab->handlers[(int) QImode].insn_code = CODE_FOR_lshlqi3;
#endif
#ifdef HAVE_lshlhi3
  if (HAVE_lshlhi3)
    lshl_optab->handlers[(int) HImode].insn_code = CODE_FOR_lshlhi3;
#endif
#ifdef HAVE_lshlsi3
  if (HAVE_lshlsi3)
    lshl_optab->handlers[(int) SImode].insn_code = CODE_FOR_lshlsi3;
#endif
#ifdef HAVE_lshldi3
  if (HAVE_lshldi3)
    lshl_optab->handlers[(int) DImode].insn_code = CODE_FOR_lshldi3;
#endif
  lshl_optab->handlers[(int) SImode].lib_call = "__lshlsi3";
  lshl_optab->handlers[(int) DImode].lib_call = "__lshldi3";

#ifdef HAVE_lshrqi3
  if (HAVE_lshrqi3)
    lshr_optab->handlers[(int) QImode].insn_code = CODE_FOR_lshrqi3;
#endif
#ifdef HAVE_lshrhi3
  if (HAVE_lshrhi3)
    lshr_optab->handlers[(int) HImode].insn_code = CODE_FOR_lshrhi3;
#endif
#ifdef HAVE_lshrsi3
  if (HAVE_lshrsi3)
    lshr_optab->handlers[(int) SImode].insn_code = CODE_FOR_lshrsi3;
#endif
#ifdef HAVE_lshrdi3
  if (HAVE_lshrdi3)
    lshr_optab->handlers[(int) DImode].insn_code = CODE_FOR_lshrdi3;
#endif
  lshr_optab->handlers[(int) SImode].lib_call = "__lshrsi3";
  lshr_optab->handlers[(int) DImode].lib_call = "__lshrdi3";

#ifdef HAVE_rotlqi3
  if (HAVE_rotlqi3)
    rotl_optab->handlers[(int) QImode].insn_code = CODE_FOR_rotlqi3;
#endif
#ifdef HAVE_rotlhi3
  if (HAVE_rotlhi3)
    rotl_optab->handlers[(int) HImode].insn_code = CODE_FOR_rotlhi3;
#endif
#ifdef HAVE_rotlsi3
  if (HAVE_rotlsi3)
    rotl_optab->handlers[(int) SImode].insn_code = CODE_FOR_rotlsi3;
#endif
#ifdef HAVE_rotldi3
  if (HAVE_rotldi3)
    rotl_optab->handlers[(int) DImode].insn_code = CODE_FOR_rotldi3;
#endif
  rotl_optab->handlers[(int) SImode].lib_call = "__rotlsi3";
  rotl_optab->handlers[(int) DImode].lib_call = "__rotldi3";

#ifdef HAVE_rotrqi3
  if (HAVE_rotrqi3)
    rotr_optab->handlers[(int) QImode].insn_code = CODE_FOR_rotrqi3;
#endif
#ifdef HAVE_rotrhi3
  if (HAVE_rotrhi3)
    rotr_optab->handlers[(int) HImode].insn_code = CODE_FOR_rotrhi3;
#endif
#ifdef HAVE_rotrsi3
  if (HAVE_rotrsi3)
    rotr_optab->handlers[(int) SImode].insn_code = CODE_FOR_rotrsi3;
#endif
#ifdef HAVE_rotrdi3
  if (HAVE_rotrdi3)
    rotr_optab->handlers[(int) DImode].insn_code = CODE_FOR_rotrdi3;
#endif
  rotr_optab->handlers[(int) SImode].lib_call = "__rotrsi3";
  rotr_optab->handlers[(int) DImode].lib_call = "__rotrdi3";

#ifdef HAVE_negqi2
  if (HAVE_negqi2)
    neg_optab->handlers[(int) QImode].insn_code = CODE_FOR_negqi2;
#endif
#ifdef HAVE_neghi2
  if (HAVE_neghi2)
    neg_optab->handlers[(int) HImode].insn_code = CODE_FOR_neghi2;
#endif
#ifdef HAVE_negsi2
  if (HAVE_negsi2)
    neg_optab->handlers[(int) SImode].insn_code = CODE_FOR_negsi2;
#endif
#ifdef HAVE_negdi2
  if (HAVE_negdi2)
    neg_optab->handlers[(int) DImode].insn_code = CODE_FOR_negdi2;
#endif
#ifdef HAVE_negsf2
  if (HAVE_negsf2)
    neg_optab->handlers[(int) SFmode].insn_code = CODE_FOR_negsf2;
#endif
#ifdef HAVE_negdf2
  if (HAVE_negdf2)
    neg_optab->handlers[(int) DFmode].insn_code = CODE_FOR_negdf2;
#endif
  neg_optab->handlers[(int) SImode].lib_call = "__negsi2"; 
  neg_optab->handlers[(int) DImode].lib_call = "__negdi2";
  neg_optab->handlers[(int) SFmode].lib_call = "__negsf2";
  neg_optab->handlers[(int) DFmode].lib_call = "__negdf2";

#ifdef HAVE_absqi2
  if (HAVE_absqi2)
    abs_optab->handlers[(int) QImode].insn_code = CODE_FOR_absqi2;
#endif
#ifdef HAVE_abshi2
  if (HAVE_abshi2)
    abs_optab->handlers[(int) HImode].insn_code = CODE_FOR_abshi2;
#endif
#ifdef HAVE_abssi2
  if (HAVE_abssi2)
    abs_optab->handlers[(int) SImode].insn_code = CODE_FOR_abssi2;
#endif
#ifdef HAVE_absdi2
  if (HAVE_absdi2)
    abs_optab->handlers[(int) DImode].insn_code = CODE_FOR_absdi2;
#endif
#ifdef HAVE_abssf2
  if (HAVE_abssf2)
    abs_optab->handlers[(int) SFmode].insn_code = CODE_FOR_abssf2;
#endif
#ifdef HAVE_absdf2
  if (HAVE_absdf2)
    abs_optab->handlers[(int) DFmode].insn_code = CODE_FOR_absdf2;
#endif
  /* No library calls here!  If there is no abs instruction,
     expand_expr will generate a conditional negation.  */

#ifdef HAVE_one_cmplqi2
  if (HAVE_one_cmplqi2)
    one_cmpl_optab->handlers[(int) QImode].insn_code = CODE_FOR_one_cmplqi2;
#endif
#ifdef HAVE_one_cmplhi2
  if (HAVE_one_cmplhi2)
    one_cmpl_optab->handlers[(int) HImode].insn_code = CODE_FOR_one_cmplhi2;
#endif
#ifdef HAVE_one_cmplsi2
  if (HAVE_one_cmplsi2)
    one_cmpl_optab->handlers[(int) SImode].insn_code = CODE_FOR_one_cmplsi2;
#endif
#ifdef HAVE_one_cmpldi2
  if (HAVE_one_cmpldi2)
    one_cmpl_optab->handlers[(int) DImode].insn_code = CODE_FOR_one_cmpldi2;
#endif
  one_cmpl_optab->handlers[(int) SImode].lib_call = "__one_cmplsi2"; 
  one_cmpl_optab->handlers[(int) DImode].lib_call = "__one_cmpldi2";

#ifdef HAVE_ffsqi2
  if (HAVE_ffsqi2)
    ffs_optab->handlers[(int) QImode].insn_code = CODE_FOR_ffsqi2;
#endif
#ifdef HAVE_ffshi2
  if (HAVE_ffshi2)
    ffs_optab->handlers[(int) HImode].insn_code = CODE_FOR_ffshi2;
#endif
#ifdef HAVE_ffssi2
  if (HAVE_ffssi2)
    ffs_optab->handlers[(int) SImode].insn_code = CODE_FOR_ffssi2;
#endif
#ifdef HAVE_ffsdi2
  if (HAVE_ffsdi2)
    ffs_optab->handlers[(int) DImode].insn_code = CODE_FOR_ffsdi2;
#endif
  ffs_optab->handlers[(int) SImode].lib_call = "ffs"; 

#ifdef HAVE_movqi
  if (HAVE_movqi)
    mov_optab->handlers[(int) QImode].insn_code = CODE_FOR_movqi;
#endif
#ifdef HAVE_movhi
  if (HAVE_movhi)
    mov_optab->handlers[(int) HImode].insn_code = CODE_FOR_movhi;
#endif
#ifdef HAVE_movsi
  if (HAVE_movsi)
    mov_optab->handlers[(int) SImode].insn_code = CODE_FOR_movsi;
#endif
#ifdef HAVE_movdi
  if (HAVE_movdi)
    mov_optab->handlers[(int) DImode].insn_code = CODE_FOR_movdi;
#endif
#ifdef HAVE_movti
  if (HAVE_movti)
    mov_optab->handlers[(int) TImode].insn_code = CODE_FOR_movti;
#endif
#ifdef HAVE_movsf
  if (HAVE_movsf)
    mov_optab->handlers[(int) SFmode].insn_code = CODE_FOR_movsf;
#endif
#ifdef HAVE_movdf
  if (HAVE_movdf)
    mov_optab->handlers[(int) DFmode].insn_code = CODE_FOR_movdf;
#endif
#ifdef HAVE_movtf
  if (HAVE_movtf)
    mov_optab->handlers[(int) TFmode].insn_code = CODE_FOR_movtf;
#endif

#ifdef HAVE_movstrictqi
  if (HAVE_movstrictqi)
    movstrict_optab->handlers[(int) QImode].insn_code = CODE_FOR_movstrictqi;
#endif
#ifdef HAVE_movstricthi
  if (HAVE_movstricthi)
    movstrict_optab->handlers[(int) HImode].insn_code = CODE_FOR_movstricthi;
#endif
#ifdef HAVE_movstrictsi
  if (HAVE_movstrictsi)
    movstrict_optab->handlers[(int) SImode].insn_code = CODE_FOR_movstrictsi;
#endif
#ifdef HAVE_movstrictdi
  if (HAVE_movstrictdi)
    movstrict_optab->handlers[(int) DImode].insn_code = CODE_FOR_movstrictdi;
#endif

#ifdef HAVE_cmpqi
  if (HAVE_cmpqi)
    cmp_optab->handlers[(int) QImode].insn_code = CODE_FOR_cmpqi;
#endif
#ifdef HAVE_cmphi
  if (HAVE_cmphi)
    cmp_optab->handlers[(int) HImode].insn_code = CODE_FOR_cmphi;
#endif
#ifdef HAVE_cmpsi
  if (HAVE_cmpsi)
    cmp_optab->handlers[(int) SImode].insn_code = CODE_FOR_cmpsi;
#endif
#ifdef HAVE_cmpdi
  if (HAVE_cmpdi)
    cmp_optab->handlers[(int) DImode].insn_code = CODE_FOR_cmpdi;
#endif
#ifdef HAVE_cmpsf
  if (HAVE_cmpsf)
    cmp_optab->handlers[(int) SFmode].insn_code = CODE_FOR_cmpsf;
#endif
#ifdef HAVE_cmpdf
  if (HAVE_cmpdf)
    cmp_optab->handlers[(int) DFmode].insn_code = CODE_FOR_cmpdf;
#endif
#ifdef HAVE_tstqi
  if (HAVE_tstqi)
    tst_optab->handlers[(int) QImode].insn_code = CODE_FOR_tstqi;
#endif
#ifdef HAVE_tsthi
  if (HAVE_tsthi)
    tst_optab->handlers[(int) HImode].insn_code = CODE_FOR_tsthi;
#endif
#ifdef HAVE_tstsi
  if (HAVE_tstsi)
    tst_optab->handlers[(int) SImode].insn_code = CODE_FOR_tstsi;
#endif
#ifdef HAVE_tstdi
  if (HAVE_tstdi)
    tst_optab->handlers[(int) DImode].insn_code = CODE_FOR_tstdi;
#endif
#ifdef HAVE_tstsf
  if (HAVE_tstsf)
    tst_optab->handlers[(int) SFmode].insn_code = CODE_FOR_tstsf;
#endif
#ifdef HAVE_tstdf
  if (HAVE_tstdf)
    tst_optab->handlers[(int) DFmode].insn_code = CODE_FOR_tstdf;
#endif
  /* Comparison libcalls for integers MUST come in pairs, signed/unsigned.  */
  cmp_optab->handlers[(int) DImode].lib_call = "__cmpdi2";
  ucmp_optab->handlers[(int) DImode].lib_call = "__ucmpdi2";
  cmp_optab->handlers[(int) SFmode].lib_call = "__cmpsf2";
  cmp_optab->handlers[(int) DFmode].lib_call = "__cmpdf2";

#if HAVE_beq
  if (HAVE_beq)
    bcc_gen_fctn[(int) EQ] = gen_beq;
#endif
#if HAVE_bne
  if (HAVE_bne)
    bcc_gen_fctn[(int) NE] = gen_bne;
#endif
#if HAVE_bgt
  if (HAVE_bgt)
    bcc_gen_fctn[(int) GT] = gen_bgt;
#endif
#if HAVE_bge
  if (HAVE_bge)
    bcc_gen_fctn[(int) GE] = gen_bge;
#endif
#if HAVE_bgtu
  if (HAVE_bgtu)
    bcc_gen_fctn[(int) GTU] = gen_bgtu;
#endif
#if HAVE_bgeu
  if (HAVE_bgeu)
    bcc_gen_fctn[(int) GEU] = gen_bgeu;
#endif
#if HAVE_blt
  if (HAVE_blt)
    bcc_gen_fctn[(int) LT] = gen_blt;
#endif
#if HAVE_ble
  if (HAVE_ble)
    bcc_gen_fctn[(int) LE] = gen_ble;
#endif
#if HAVE_bltu
  if (HAVE_bltu)
    bcc_gen_fctn[(int) LTU] = gen_bltu;
#endif
#if HAVE_bleu
  if (HAVE_bleu)
    bcc_gen_fctn[(int) LEU] = gen_bleu;
#endif

#if HAVE_seq
  if (HAVE_seq)
    setcc_gen_fctn[(int) EQ] = gen_seq;
#endif
#if HAVE_sne
  if (HAVE_sne)
    setcc_gen_fctn[(int) NE] = gen_sne;
#endif
#if HAVE_sgt
  if (HAVE_sgt)
    setcc_gen_fctn[(int) GT] = gen_sgt;
#endif
#if HAVE_sge
  if (HAVE_sge)
    setcc_gen_fctn[(int) GE] = gen_sge;
#endif
#if HAVE_sgtu
  if (HAVE_sgtu)
    setcc_gen_fctn[(int) GTU] = gen_sgtu;
#endif
#if HAVE_sgeu
  if (HAVE_sgeu)
    setcc_gen_fctn[(int) GEU] = gen_sgeu;
#endif
#if HAVE_slt
  if (HAVE_slt)
    setcc_gen_fctn[(int) LT] = gen_slt;
#endif
#if HAVE_sle
  if (HAVE_sle)
    setcc_gen_fctn[(int) LE] = gen_sle;
#endif
#if HAVE_sltu
  if (HAVE_sltu)
    setcc_gen_fctn[(int) LTU] = gen_sltu;
#endif
#if HAVE_sleu
  if (HAVE_sleu)
    setcc_gen_fctn[(int) LEU] = gen_sleu;
#endif
}
