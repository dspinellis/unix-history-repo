/* Convert tree expression to rtl instructions, for GNU compiler.
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


#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "insn-flags.h"
#include "insn-codes.h"
#include "expr.h"
#include "insn-config.h"
#include "recog.h"
#include "gvarargs.h"
#include "typeclass.h"
#include "recog.h"

/* Decide whether a function's arguments should be processed
   from first to last or from last to first.  */

#ifdef STACK_GROWS_DOWNWARD
#ifdef PUSH_ROUNDING
#define PUSH_ARGS_REVERSED	/* If it's last to first */
#endif
#endif

/* Like STACK_BOUNDARY but in units of bytes, not bits.  */
#define STACK_BYTES (STACK_BOUNDARY / BITS_PER_UNIT)

/* If this is nonzero, we do not bother generating VOLATILE
   around volatile memory references, and we are willing to
   output indirect addresses.  If cse is to follow, we reject
   indirect addresses so a useful potential cse is generated;
   if it is used only once, instruction combination will produce
   the same indirect address eventually.  */
int cse_not_expected;

/* Nonzero to generate code for all the subroutines within an
   expression before generating the upper levels of the expression.
   Nowadays this is never zero.  */
int do_preexpand_calls = 1;

/* Number of units that we should eventually pop off the stack.
   These are the arguments to function calls that have already returned.  */
int pending_stack_adjust;

/* Nonzero means stack pops must not be deferred, and deferred stack
   pops must not be output.  It is nonzero inside a function call,
   inside a conditional expression, inside a statement expression,
   and in other cases as well.  */
int inhibit_defer_pop;

/* A list of all cleanups which belong to the arguments of
   function calls being expanded by expand_call.  */
static tree cleanups_of_this_call;

/* Nonzero means __builtin_saveregs has already been done in this function.
   The value is the pseudoreg containing the value __builtin_saveregs
   returned.  */
static rtx saveregs_value;

/* Nonzero means current function may call alloca
   as a subroutine.  (__builtin_alloca does not count.)  */
int may_call_alloca;

rtx store_expr ();
static void store_constructor ();
static rtx store_field ();
static rtx expand_call ();
static void emit_call_1 ();
static rtx prepare_call_address ();
static rtx expand_builtin ();
static rtx compare ();
static rtx compare_constants ();
static rtx compare1 ();
static rtx do_store_flag ();
static void preexpand_calls ();
static rtx expand_increment ();
static void init_queue ();

void do_pending_stack_adjust ();

/* MOVE_RATIO is the number of move instructions that is better than
   a block move.  */

#ifndef MOVE_RATIO
#if defined (HAVE_movstrqi) || defined (HAVE_movstrhi) || defined (HAVE_movstrsi)
#define MOVE_RATIO 2
#else
/* A value of around 6 would minimize code size; infinity would minimize
   execution time.  */
#define MOVE_RATIO 15
#endif
#endif

/* Table indexed by tree code giving 1 if the code is for a
   comparison operation, or anything that is most easily
   computed with a conditional branch.

   We include tree.def to give it the proper length.
   The contents thus created are irrelevant.
   The real contents are initialized in init_comparisons.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) 0,

static char comparison_code[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* This is run once per compilation.  */

void
init_comparisons ()
{
  comparison_code[(int) EQ_EXPR] = 1;
  comparison_code[(int) NE_EXPR] = 1;
  comparison_code[(int) LT_EXPR] = 1;
  comparison_code[(int) GT_EXPR] = 1;
  comparison_code[(int) LE_EXPR] = 1;
  comparison_code[(int) GE_EXPR] = 1;
}

/* This is run at the start of compiling a function.  */

void
init_expr ()
{
  init_queue ();
  may_call_alloca = 0;
  saveregs_value = 0;
}

/* Manage the queue of increment instructions to be output
   for POSTINCREMENT_EXPR expressions, etc.  */

static rtx pending_chain;

/* Queue up to increment (or change) VAR later.  BODY says how:
   BODY should be the same thing you would pass to emit_insn
   to increment right away.  It will go to emit_insn later on.

   The value is a QUEUED expression to be used in place of VAR
   where you want to guarantee the pre-incrementation value of VAR.  */

static rtx
enqueue_insn (var, body)
     rtx var, body;
{
  pending_chain = gen_rtx (QUEUED, GET_MODE (var),
			   var, 0, 0, body, pending_chain);
  return pending_chain;
}

/* Use protect_from_queue to convert a QUEUED expression
   into something that you can put immediately into an instruction.
   If the queued incrementation has not happened yet,
   protect_from_queue returns the variable itself.
   If the incrementation has happened, protect_from_queue returns a temp
   that contains a copy of the old value of the variable.

   Any time an rtx which might possibly be a QUEUED is to be put
   into an instruction, it must be passed through protect_from_queue first.
   QUEUED expressions are not meaningful in instructions.

   Do not pass a value through protect_from_queue and then hold
   on to it for a while before putting it in an instruction!
   If the queue is flushed in between, incorrect code will result.  */

rtx
protect_from_queue (x, modify)
     register rtx x;
     int modify;
{
  register RTX_CODE code = GET_CODE (x);
  if (code != QUEUED)
    {
      /* A special hack for read access to (MEM (QUEUED ...))
	 to facilitate use of autoincrement.
	 Make a copy of the contents of the memory location
	 rather than a copy of the address.  */
      if (code == MEM && GET_CODE (XEXP (x, 0)) == QUEUED && !modify)
	{
	  register rtx y = XEXP (x, 0);
	  XEXP (x, 0) = QUEUED_VAR (y);
	  if (QUEUED_INSN (y))
	    {
	      register rtx temp = gen_reg_rtx (GET_MODE (x));
	      emit_insn_before (gen_move_insn (temp, x),
				QUEUED_INSN (y));
	      return temp;
	    }
	  return x;
	}
      /* Otherwise, recursively protect the subexpressions of all
	 the kinds of rtx's that can contain a QUEUED.  */
      if (code == MEM)
	XEXP (x, 0) = protect_from_queue (XEXP (x, 0), 0);
      else if (code == PLUS || code == MULT)
	{
	  XEXP (x, 0) = protect_from_queue (XEXP (x, 0), 0);
	  XEXP (x, 1) = protect_from_queue (XEXP (x, 1), 0);
	}
      return x;
    }
  /* If the increment has not happened, use the variable itself.  */
  if (QUEUED_INSN (x) == 0)
    return QUEUED_VAR (x);
  /* If the increment has happened and a pre-increment copy exists,
     use that copy.  */
  if (QUEUED_COPY (x) != 0)
    return QUEUED_COPY (x);
  /* The increment has happened but we haven't set up a pre-increment copy.
     Set one up now, and use it.  */
  QUEUED_COPY (x) = gen_reg_rtx (GET_MODE (QUEUED_VAR (x)));
  emit_insn_before (gen_move_insn (QUEUED_COPY (x), QUEUED_VAR (x)),
		    QUEUED_INSN (x));
  return QUEUED_COPY (x);
}

/* Return nonzero if X contains a QUEUED expression:
   if it contains anything that will be altered by a queued increment.
   We handle only combinations of MEM, PLUS, MINUS and MULT operators
   since memory addresses generally contain only those.  */

static int
queued_subexp_p (x)
     rtx x;
{
  register enum rtx_code code = GET_CODE (x);
  switch (code)
    {
    case QUEUED:
      return 1;
    case MEM:
      return queued_subexp_p (XEXP (x, 0));
    case MULT:
    case PLUS:
    case MINUS:
      return queued_subexp_p (XEXP (x, 0))
	|| queued_subexp_p (XEXP (x, 1));
    }
  return 0;
}

/* Perform all the pending incrementations.  */

void
emit_queue ()
{
  register rtx p;
  while (p = pending_chain)
    {
      QUEUED_INSN (p) = emit_insn (QUEUED_BODY (p));
      pending_chain = QUEUED_NEXT (p);
    }
}

static void
init_queue ()
{
  if (pending_chain)
    abort ();
}

/* Copy data from FROM to TO, where the machine modes are not the same.
   Both modes may be integer, or both may be floating.
   UNSIGNEDP should be nonzero if FROM is an unsigned type.
   This causes zero-extension instead of sign-extension.  */

void
convert_move (to, from, unsignedp)
     register rtx to, from;
     int unsignedp;
{
  enum machine_mode to_mode = GET_MODE (to);
  enum machine_mode from_mode = GET_MODE (from);
  int to_real = GET_MODE_CLASS (to_mode) == MODE_FLOAT;
  int from_real = GET_MODE_CLASS (from_mode) == MODE_FLOAT;
  int extending = (int) to_mode > (int) from_mode;

  to = protect_from_queue (to, 1);
  from = protect_from_queue (from, 0);

  if (to_real != from_real)
    abort ();

  if (to_mode == from_mode
      || (from_mode == VOIDmode && CONSTANT_P (from)))
    {
      emit_move_insn (to, from);
      return;
    }

  if (to_real)
    {
#ifdef HAVE_extendsfdf2
      if (HAVE_extendsfdf2 && extending)
	{
	  emit_unop_insn (CODE_FOR_extendsfdf2, to, from, UNKNOWN);
	  return;
	}
#endif
#ifdef HAVE_truncdfsf2
      if (HAVE_truncdfsf2 && ! extending)
	{
	  emit_unop_insn (CODE_FOR_truncdfsf2, to, from, UNKNOWN);
	  return;
	}
#endif
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, (extending
						      ? "__extendsfdf2"
						      : "__truncdfsf2")), 0,
			 GET_MODE (to), 1,
			 from,  (extending ? SFmode : DFmode));
      emit_move_insn (to, hard_libcall_value (GET_MODE (to)));
      return;
    }

  /* Now both modes are integers.  */

  if (to_mode == DImode)
    {
      if (unsignedp)
	{
#ifdef HAVE_zero_extendsidi2
	  if (HAVE_zero_extendsidi2 && from_mode == SImode)
	    emit_unop_insn (CODE_FOR_zero_extendsidi2, to, from, ZERO_EXTEND);
	  else
#endif
#ifdef HAVE_zero_extendhidi2
	  if (HAVE_zero_extendhidi2 && from_mode == HImode)
	    emit_unop_insn (CODE_FOR_zero_extendhidi2, to, from, ZERO_EXTEND);
	  else
#endif
#ifdef HAVE_zero_extendqidi2
	  if (HAVE_zero_extendqidi2 && from_mode == QImode)
	    emit_unop_insn (CODE_FOR_zero_extendqidi2, to, from, ZERO_EXTEND);
	  else
#endif
#ifdef HAVE_zero_extendsidi2
	  if (HAVE_zero_extendsidi2)
	    {
	      convert_move (gen_lowpart (SImode, to), from, unsignedp);
	      emit_unop_insn (CODE_FOR_zero_extendsidi2, to,
			      gen_lowpart (SImode, to), ZERO_EXTEND);
	    }
	  else
#endif
	    {
	      emit_insn (gen_rtx (CLOBBER, VOIDmode, to));
	      convert_move (gen_lowpart (SImode, to), from, unsignedp);
	      emit_clr_insn (gen_highpart (SImode, to));
	    }
	}
#ifdef HAVE_extendsidi2
      else if (HAVE_extendsidi2 && from_mode == SImode)
	emit_unop_insn (CODE_FOR_extendsidi2, to, from, SIGN_EXTEND);
#endif
#ifdef HAVE_extendhidi2
      else if (HAVE_extendhidi2 && from_mode == HImode)
	emit_unop_insn (CODE_FOR_extendhidi2, to, from, SIGN_EXTEND);
#endif
#ifdef HAVE_extendqidi2
      else if (HAVE_extendqidi2 && from_mode == QImode)
	emit_unop_insn (CODE_FOR_extendqidi2, to, from, SIGN_EXTEND);
#endif
#ifdef HAVE_extendsidi2
      else if (HAVE_extendsidi2)
	{
	  convert_move (gen_lowpart (SImode, to), from, unsignedp);
	  emit_unop_insn (CODE_FOR_extendsidi2, to,
			  gen_lowpart (SImode, to), SIGN_EXTEND);
	}
#endif
#ifdef HAVE_slt
      else if (HAVE_slt && insn_operand_mode[(int) CODE_FOR_slt][0] == SImode)
	{
	  rtx temp, target;
	  emit_insn (gen_rtx (CLOBBER, VOIDmode, to));
	  convert_move (gen_lowpart (SImode, to), from, unsignedp);
	  emit_cmp_insn (gen_lowpart (SImode, to), const0_rtx, 0, 0, 0);
	  target = gen_highpart (SImode, to);
	  if (!(*insn_operand_predicate[(int) CODE_FOR_slt][0]) (target, SImode))
	    temp = gen_reg_rtx (SImode);
	  else
	    temp = target;
	  emit_insn (gen_slt (temp));
	  if (temp != target)
	    emit_move_insn (target, temp);
	}
#endif
      else
	{
	  register rtx label = gen_label_rtx ();
	  rtx temp, target;

	  emit_insn (gen_rtx (CLOBBER, VOIDmode, to));
	  emit_clr_insn (gen_highpart (SImode, to));
	  convert_move (gen_lowpart (SImode, to), from, unsignedp);
	  emit_cmp_insn (gen_lowpart (SImode, to),
			 gen_rtx (CONST_INT, VOIDmode, 0),
			 0, 0, 0);
	  NO_DEFER_POP;
	  emit_jump_insn (gen_bge (label));
	  target = gen_highpart (SImode, to);
	  temp = expand_unop (SImode, one_cmpl_optab,
			      target, gen_highpart (SImode, to),
			      1);
	  if (temp != target)
	    emit_move_insn (target, temp);
	  emit_label (label);
	  OK_DEFER_POP;
	}
      return;
    }

  if (from_mode == DImode)
    {
      convert_move (to, gen_lowpart (SImode, from), 0);
      return;
    }

  /* Now follow all the conversions between integers
     no more than a word long.  */

  /* For truncation, usually we can just refer to FROM in a narrower mode.  */
  if (GET_MODE_BITSIZE (to_mode) < GET_MODE_BITSIZE (from_mode)
      && TRULY_NOOP_TRUNCATION (GET_MODE_BITSIZE (to_mode),
				GET_MODE_BITSIZE (from_mode))
      && ((GET_CODE (from) == MEM
	   && ! MEM_VOLATILE_P (from)
	   && ! mode_dependent_address_p (XEXP (from, 0)))
	  || GET_CODE (from) == REG
	  || GET_CODE (from) == SUBREG))
    {
      emit_move_insn (to, gen_lowpart (to_mode, from));
      return;
    }

  if (to_mode == SImode && from_mode == HImode)
    {
      if (unsignedp)
	{
#ifdef HAVE_zero_extendhisi2
	  if (HAVE_zero_extendhisi2)
	    emit_unop_insn (CODE_FOR_zero_extendhisi2, to, from, ZERO_EXTEND);
	  else
#endif
	    abort ();
	}
      else
	{
#ifdef HAVE_extendhisi2
	  if (HAVE_extendhisi2)
	    emit_unop_insn (CODE_FOR_extendhisi2, to, from, SIGN_EXTEND);
	  else
#endif
	    abort ();
	}
      return;
    }

  if (to_mode == SImode && from_mode == QImode)
    {
      if (unsignedp)
	{
#ifdef HAVE_zero_extendqisi2
	  if (HAVE_zero_extendqisi2)
	    {
	      emit_unop_insn (CODE_FOR_zero_extendqisi2, to, from, ZERO_EXTEND);
	      return;
	    }
#endif
#if defined (HAVE_zero_extendqihi2) && defined (HAVE_extendhisi2)
	  if (HAVE_zero_extendqihi2 && HAVE_extendhisi2)
	    {
	      register rtx temp = gen_reg_rtx (HImode);
	      emit_unop_insn (CODE_FOR_zero_extendqihi2, temp, from, ZERO_EXTEND);
	      emit_unop_insn (CODE_FOR_extendhisi2, to, temp, SIGN_EXTEND);
	      return;
	    }
#endif
	}
      else
	{
#ifdef HAVE_extendqisi2
	  if (HAVE_extendqisi2)
	    {
	      emit_unop_insn (CODE_FOR_extendqisi2, to, from, SIGN_EXTEND);
	      return;
	    }
#endif
#if defined (HAVE_extendqihi2) && defined (HAVE_extendhisi2)
	  if (HAVE_extendqihi2 && HAVE_extendhisi2)
	    {
	      register rtx temp = gen_reg_rtx (HImode);
	      emit_unop_insn (CODE_FOR_extendqihi2, temp, from, SIGN_EXTEND);
	      emit_unop_insn (CODE_FOR_extendhisi2, to, temp, SIGN_EXTEND);
	      return;
	    }
#endif
	}
      abort ();
    }

  if (to_mode == HImode && from_mode == QImode)
    {
      if (unsignedp)
	{
#ifdef HAVE_zero_extendqihi2
	  if (HAVE_zero_extendqihi2)
	    {
	      emit_unop_insn (CODE_FOR_zero_extendqihi2, to, from, ZERO_EXTEND);
	      return;
	    }
#endif
	}
      else
	{
#ifdef HAVE_extendqihi2
	  if (HAVE_extendqihi2)
	    {
	      emit_unop_insn (CODE_FOR_extendqihi2, to, from, SIGN_EXTEND);
	      return;
	    }
#endif
	}
      abort ();
    }

#if 0 /* This seems to be redundant with code 100 lines up.  */

  /* Now we are truncating an integer to a smaller one.
     If the result is a temporary, we might as well just copy it,
     since only the low-order part of the result needs to be valid
     and it is valid with no change.  */

  if (GET_CODE (to) == REG)
    {
      if (GET_CODE (from) == REG)
	{
	  emit_move_insn (to, gen_lowpart (GET_MODE (to), from));
	  return;
	}
      else if (GET_CODE (from) == SUBREG)
	{
	  from = copy_rtx (from);
	  /* This is safe since FROM is not more than one word.  */
	  PUT_MODE (from, GET_MODE (to));
	  emit_move_insn (to, from);
	  return;
	}
#ifndef BYTES_BIG_ENDIAN
      else if (GET_CODE (from) == MEM)
	{
	  register rtx addr = XEXP (from, 0);
	  if (memory_address_p (GET_MODE (to), addr))
	    {
	      emit_move_insn (to, gen_rtx (MEM, GET_MODE (to), addr));
	      return;
	    }
	}
#endif /* not BYTES_BIG_ENDIAN */
    }
#endif /* 0 */

  if (from_mode == SImode && to_mode == HImode)
    {
#ifdef HAVE_truncsihi2
      if (HAVE_truncsihi2)
	{
	  emit_unop_insn (CODE_FOR_truncsihi2, to, from, UNKNOWN);
	  return;
	}
#endif
      abort ();
    }

  if (from_mode == SImode && to_mode == QImode)
    {
#ifdef HAVE_truncsiqi2
      if (HAVE_truncsiqi2)
	{
	  emit_unop_insn (CODE_FOR_truncsiqi2, to, from, UNKNOWN);
	  return;
	}
#endif
      abort ();
    }

  if (from_mode == HImode && to_mode == QImode)
    {
#ifdef HAVE_trunchiqi2
      if (HAVE_trunchiqi2)
	{
	  emit_unop_insn (CODE_FOR_trunchiqi2, to, from, UNKNOWN);
	  return;
	}
#endif
      abort ();
    }

  /* Mode combination is not recognized.  */
  abort ();
}

/* Return an rtx for a value that would result
   from converting X to mode MODE.
   Both X and MODE may be floating, or both integer.
   UNSIGNEDP is nonzero if X is an unsigned value.
   This can be done by referring to a part of X in place
   or by copying to a new temporary with conversion.  */

rtx
convert_to_mode (mode, x, unsignedp)
     enum machine_mode mode;
     rtx x;
     int unsignedp;
{
  register rtx temp;
  if (mode == GET_MODE (x))
    return x;
  if (integer_mode_p (mode)
      && GET_MODE_SIZE (mode) <= GET_MODE_SIZE (GET_MODE (x))
      && ! (GET_CODE (x) == MEM && MEM_VOLATILE_P (x)))
    return gen_lowpart (mode, x);
  temp = gen_reg_rtx (mode);
  convert_move (temp, x, unsignedp);
  return temp;
}

int
integer_mode_p (mode)
     enum machine_mode mode;
{
  return (int) mode > (int) VOIDmode && (int) mode <= (int) TImode;
}

/* Generate several move instructions to copy LEN bytes
   from block FROM to block TO.  (These are MEM rtx's with BLKmode).
   The caller must pass FROM and TO
    through protect_from_queue before calling.
   ALIGN (in bytes) is maximum alignment we can assume.  */

struct move_by_pieces
{
  rtx to;
  rtx to_addr;
  int autinc_to;
  int explicit_inc_to;
  rtx from;
  rtx from_addr;
  int autinc_from;
  int explicit_inc_from;
  int len;
  int offset;
  int reverse;
};

static void move_by_pieces_1 ();
static int move_by_pieces_ninsns ();

static void
move_by_pieces (to, from, len, align)
     rtx to, from;
     int len, align;
{
  struct move_by_pieces data;
  rtx to_addr = XEXP (to, 0), from_addr = XEXP (from, 0);

  data.offset = 0;
  data.to_addr = to_addr;
  data.from_addr = from_addr;
  data.to = to;
  data.from = from;
  data.autinc_to
    = (GET_CODE (to_addr) == PRE_INC || GET_CODE (to_addr) == PRE_DEC
       || GET_CODE (to_addr) == POST_INC || GET_CODE (to_addr) == POST_DEC);
  data.autinc_from
    = (GET_CODE (from_addr) == PRE_INC || GET_CODE (from_addr) == PRE_DEC
       || GET_CODE (from_addr) == POST_INC
       || GET_CODE (from_addr) == POST_DEC);

  data.explicit_inc_from = 0;
  data.explicit_inc_to = 0;
  data.reverse
    = (GET_CODE (to_addr) == PRE_DEC || GET_CODE (to_addr) == POST_DEC);
  if (data.reverse) data.offset = len;
  data.len = len;

  /* If copying requires more than two move insns,
     copy addresses to registers (to make displacements shorter)
     and use post-increment if available.  */
  if (!(data.autinc_from && data.autinc_to)
      && move_by_pieces_ninsns (len, align) > 2)
    {
#ifdef HAVE_PRE_DECREMENT
      if (data.reverse && ! data.autinc_from)
	{
	  data.from_addr = copy_addr_to_reg (plus_constant (from_addr, len));
	  data.autinc_from = 1;
	  data.explicit_inc_from = -1;
	}
#endif
#ifdef HAVE_POST_INCREMENT
      if (! data.autinc_from)
	{
	  data.from_addr = copy_addr_to_reg (from_addr);
	  data.autinc_from = 1;
	  data.explicit_inc_from = 1;
	}
#endif
      if (!data.autinc_from && CONSTANT_P (from_addr))
	data.from_addr = copy_addr_to_reg (from_addr);
#ifdef HAVE_PRE_DECREMENT
      if (data.reverse && ! data.autinc_to)
	{
	  data.to_addr = copy_addr_to_reg (plus_constant (to_addr, len));
	  data.autinc_to = 1;
	  data.explicit_inc_to = -1;
	}
#endif
#ifdef HAVE_POST_INCREMENT
      if (! data.reverse && ! data.autinc_to)
	{
	  data.to_addr = copy_addr_to_reg (to_addr);
	  data.autinc_to = 1;
	  data.explicit_inc_to = 1;
	}
#endif
      if (!data.autinc_to && CONSTANT_P (to_addr))
	data.to_addr = copy_addr_to_reg (to_addr);
    }

#ifdef STRICT_ALIGNMENT
  if (align > MOVE_MAX || align >= BIGGEST_ALIGNMENT / BITS_PER_UNIT)
    align = MOVE_MAX;
#else
  align = MOVE_MAX;
#endif

#ifdef HAVE_movti
  if (HAVE_movti && align >= GET_MODE_SIZE (TImode))
    move_by_pieces_1 (gen_movti, TImode, &data);
#endif
#ifdef HAVE_movdi
  if (HAVE_movdi && align >= GET_MODE_SIZE (DImode))
    move_by_pieces_1 (gen_movdi, DImode, &data);
#endif
#ifdef HAVE_movsi
  if (align >= GET_MODE_SIZE (SImode))
    move_by_pieces_1 (gen_movsi, SImode, &data);
#endif
#ifdef HAVE_movhi
  if (HAVE_movhi && align >= GET_MODE_SIZE (HImode))
    move_by_pieces_1 (gen_movhi, HImode, &data);
#endif
#ifdef HAVE_movqi
  move_by_pieces_1 (gen_movqi, QImode, &data);
#else
  movqi instruction required in machine description
#endif
}

/* Return number of insns required to move L bytes by pieces.
   ALIGN (in bytes) is maximum alignment we can assume.  */

static int
move_by_pieces_ninsns (l, align)
     unsigned int l;
     int align;
{
  register int n_insns = 0;

#ifdef STRICT_ALIGNMENT
  if (align > MOVE_MAX || align >= BIGGEST_ALIGNMENT / BITS_PER_UNIT)
    align = MOVE_MAX;
#else
  align = MOVE_MAX;
#endif

#ifdef HAVE_movti
  if (HAVE_movti && align >= GET_MODE_SIZE (TImode))
    n_insns += l / GET_MODE_SIZE (TImode), l %= GET_MODE_SIZE (TImode);
#endif
#ifdef HAVE_movdi
  if (HAVE_movdi && align >= GET_MODE_SIZE (DImode))
    n_insns += l / GET_MODE_SIZE (DImode), l %= GET_MODE_SIZE (DImode);
#endif
#ifdef HAVE_movsi
  if (HAVE_movsi && align >= GET_MODE_SIZE (SImode))
    n_insns += l / GET_MODE_SIZE (SImode), l %= GET_MODE_SIZE (SImode);
#endif
#ifdef HAVE_movhi
  if (HAVE_movhi && align >= GET_MODE_SIZE (HImode))
    n_insns += l / GET_MODE_SIZE (HImode), l %= GET_MODE_SIZE (HImode);
#endif
  n_insns += l;

  return n_insns;
}

/* Subroutine of move_by_pieces.  Move as many bytes as appropriate
   with move instructions for mode MODE.  GENFUN is the gen_... function
   to make a move insn for that mode.  DATA has all the other info.  */

static void
move_by_pieces_1 (genfun, mode, data)
     rtx (*genfun) ();
     enum machine_mode mode;
     struct move_by_pieces *data;
{
  register int size = GET_MODE_SIZE (mode);
  register rtx to1, from1;

  while (data->len >= size)
    {
      if (data->reverse) data->offset -= size;

      to1 = (data->autinc_to
	     ? gen_rtx (MEM, mode, data->to_addr)
	     : change_address (data->to, mode,
			       plus_constant (data->to_addr, data->offset)));
      from1 =
	(data->autinc_from
	 ? gen_rtx (MEM, mode, data->from_addr)
	 : change_address (data->from, mode,
			   plus_constant (data->from_addr, data->offset)));

#ifdef HAVE_PRE_DECREMENT
      if (data->explicit_inc_to < 0)
	emit_insn (gen_sub2_insn (data->to_addr,
				  gen_rtx (CONST_INT, VOIDmode, size)));
      if (data->explicit_inc_from < 0)
	emit_insn (gen_sub2_insn (data->from_addr,
				  gen_rtx (CONST_INT, VOIDmode, size)));
#endif

      emit_insn ((*genfun) (to1, from1));
#ifdef HAVE_POST_INCREMENT
      if (data->explicit_inc_to > 0)
	emit_insn (gen_add2_insn (data->to_addr,
				  gen_rtx (CONST_INT, VOIDmode, size)));
      if (data->explicit_inc_from > 0)
	emit_insn (gen_add2_insn (data->from_addr,
				  gen_rtx (CONST_INT, VOIDmode, size)));
#endif

      if (! data->reverse) data->offset += size;

      data->len -= size;
    }
}

/* Emit code to move a block Y to a block X.
   This may be done with string-move instructions,
   with multiple scalar move instructions, or with a library call.

   Both X and Y must be MEM rtx's (perhaps inside VOLATILE)
   with mode BLKmode.
   SIZE is an rtx that says how long they are.
   ALIGN is the maximum alignment we can assume they have,
   measured in bytes.  */

static void
emit_block_move (x, y, size, align)
     rtx x, y;
     rtx size;
     int align;
{
  if (GET_MODE (x) != BLKmode)
    abort ();

  if (GET_MODE (y) != BLKmode)
    abort ();

  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);

  if (GET_CODE (x) != MEM)
    abort ();
  if (GET_CODE (y) != MEM)
    abort ();
  if (size == 0)
    abort ();

  if (GET_CODE (size) == CONST_INT
      && (move_by_pieces_ninsns ((unsigned) INTVAL (size), align)
	  < MOVE_RATIO))
    move_by_pieces (x, y, INTVAL (size), align);
  else
    {
      /* Try the most limited insn first, because there's no point
	 including more than one in the machine description unless
	 the more limited one has some advantage.  */
#ifdef HAVE_movstrqi
      if (HAVE_movstrqi
	  && GET_CODE (size) == CONST_INT
	  && ((unsigned) INTVAL (size)
	      < (1 << (GET_MODE_BITSIZE (QImode) - 1))))
	{
	  emit_insn (gen_movstrqi (x, y, size,
				   gen_rtx (CONST_INT, VOIDmode, align)));
	  return;
	}
#endif
#ifdef HAVE_movstrhi
      if (HAVE_movstrhi
	  && GET_CODE (size) == CONST_INT
	  && ((unsigned) INTVAL (size)
	      < (1 << (GET_MODE_BITSIZE (HImode) - 1))))
	{
	  emit_insn (gen_movstrhi (x, y, size,
				   gen_rtx (CONST_INT, VOIDmode, align)));
	  return;
	}
#endif
#ifdef HAVE_movstrsi
      if (HAVE_movstrsi)
	{
	  emit_insn (gen_movstrsi (x, y, size,
				   gen_rtx (CONST_INT, VOIDmode, align)));
	  return;
	}
#endif

#ifdef TARGET_MEM_FUNCTIONS
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memcpy"), 0,
			 VOIDmode, 3, XEXP (x, 0), Pmode,
			 XEXP (y, 0), Pmode,
			 size, Pmode);
#else
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcopy"), 0,
			 VOIDmode, 3, XEXP (y, 0), Pmode,
			 XEXP (x, 0), Pmode,
			 size, Pmode);
#endif
    }
}

/* Copy all or part of a BLKmode value X into registers starting at REGNO.
   The number of registers to be filled is NREGS.  */

static void
move_block_to_reg (regno, x, nregs)
     int regno;
     rtx x;
     int nregs;
{
  int i;
  if (GET_CODE (x) == CONST_DOUBLE && x != dconst0_rtx)
    x = force_const_double_mem (x);
  for (i = 0; i < nregs; i++)
    {
      if (GET_CODE (x) == REG)
	emit_move_insn (gen_rtx (REG, SImode, regno + i),
			gen_rtx (SUBREG, SImode, x, i));
      else if (x == dconst0_rtx || x == const0_rtx)
	emit_move_insn (gen_rtx (REG, SImode, regno + i),
			const0_rtx);
      else
	emit_move_insn (gen_rtx (REG, SImode, regno + i),
			gen_rtx (MEM, SImode,
				 memory_address (SImode,
						 plus_constant (XEXP (x, 0),
								i * GET_MODE_SIZE (SImode)))));
    }
}

/* Copy all or part of a BLKmode value X out of registers starting at REGNO.
   The number of registers to be filled is NREGS.  */

void
move_block_from_reg (regno, x, nregs)
     int regno;
     rtx x;
     int nregs;
{
  int i;
  for (i = 0; i < nregs; i++)
    {
      if (GET_CODE (x) == REG)
	emit_move_insn (gen_rtx (SUBREG, SImode, x, i),
			gen_rtx (REG, SImode, regno + i));
      else
	emit_move_insn (gen_rtx (MEM, SImode,
				 memory_address (SImode,
						 plus_constant (XEXP (x, 0),
								i * GET_MODE_SIZE (SImode)))),
			gen_rtx (REG, SImode, regno + i));
    }
}

/* Mark NREGS consecutive regs, starting at REGNO, as being live now.  */

static void
use_regs (regno, nregs)
     int regno;
     int nregs;
{
  int i;
  for (i = 0; i < nregs; i++)
    emit_insn (gen_rtx (USE, VOIDmode, gen_rtx (REG, SImode, regno + i)));
}

/* Write zeros through the storage of OBJECT.
   If OBJECT has BLKmode, SIZE is its length in bytes.  */

void
clear_storage (object, size)
     rtx object;
     int size;
{
  if (GET_MODE (object) == BLKmode)
    {
#ifdef TARGET_MEM_FUNCTIONS
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memset"), 0,
			 VOIDmode, 3,
			 XEXP (object, 0), Pmode, const0_rtx, Pmode,
			 gen_rtx (CONST_INT, VOIDmode, size), Pmode);
#else
      emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bzero"), 0,
			 VOIDmode, 2,
			 XEXP (object, 0), Pmode,
			 gen_rtx (CONST_INT, VOIDmode, size), Pmode);
#endif
    }
  else
    emit_move_insn (object, const0_rtx);
}

/* Generate code to copy Y into X.
   Both Y and X must have the same mode, except that
   Y can be a constant with VOIDmode.
   This mode cannot be BLKmode; use emit_block_move for that.

   Return the last instruction emitted.  */

rtx
emit_move_insn (x, y)
     rtx x, y;
{
  enum machine_mode mode = GET_MODE (x);
  x = protect_from_queue (x, 1);
  y = protect_from_queue (y, 0);

  if (mode == BLKmode)
    abort ();
  if (mov_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
    {
      int icode = (int) mov_optab->handlers[(int) mode].insn_code;
      if (! (*insn_operand_predicate[icode][1]) (y, mode)
	  && (CONSTANT_P (y) || GET_CODE (y) == CONST_DOUBLE))
	{
	  y = force_const_mem (mode, y);
	  if (! memory_address_p (mode, XEXP (y, 0)))
	    y = gen_rtx (MEM, mode, memory_address (mode, XEXP (y, 0)));
	}
      return emit_insn (GEN_FCN (icode) (x, y));
    }
#if 0
  /* It turns out you get much better optimization (in cse and flow)
     if you define movdi and movdf instruction patterns
     even if they must turn into multiple assembler instructions.  */
  else if (GET_MODE_SIZE (mode) >= GET_MODE_SIZE (SImode))
    {
      register int count = GET_MODE_SIZE (mode) / GET_MODE_SIZE (SImode);
      register int i;
      if (GET_CODE (y) == CONST_DOUBLE && y != dconst0_rtx)
	y = force_const_double_mem (y);
      for (i = 0; i < count; i++)
	{
	  rtx x1, y1;
	  if (GET_CODE (x) == REG)
	    x1 = gen_rtx (SUBREG, SImode, x, i);
	  else
	    x1 = gen_rtx (MEM, SImode,
			  memory_address (SImode,
					  plus_constant (XEXP (x, 0),
							 i * GET_MODE_SIZE (SImode))));
	  if (GET_CODE (y) == REG)
	    y1 = gen_rtx (SUBREG, SImode, y, i);
	  else if (y == dconst0_rtx)
	    y1 = const0_rtx;
	  else
	    y1 = gen_rtx (MEM, SImode,
			  memory_address (SImode,
					  plus_constant (XEXP (y, 0),
							 i * GET_MODE_SIZE (SImode))));
	  emit_insn (gen_movsi (protect_from_queue (x1, 1), protect_from_queue (y1, 0)));
	}
    }
#endif
  else
    abort ();
}

/* Pushing data onto the stack.  */

/* Push a block of length SIZE (perhaps variable)
   and return an rtx to address the beginning of the block.
   Note that it is not possible for the value returned to be a QUEUED.
   The value may be stack_pointer_rtx.

   EXTRA is the number of bytes of padding to push in addition to the block.
   The padding is pushed "after" the specified size.

   The value we return does take account of STACK_POINTER_OFFSET.  */

rtx
push_block (size, extra)
     rtx size;
     int extra;
{
  register rtx temp;
  if (CONSTANT_P (size))
    anti_adjust_stack (plus_constant (size, extra));
  else if (GET_CODE (size) == REG && extra == 0)
    anti_adjust_stack (size);
  else
    {
      rtx temp = copy_to_mode_reg (Pmode, size);
      if (extra != 0)
	temp = expand_binop (Pmode, add_optab,
			     temp, gen_rtx (CONST_INT, VOIDmode, extra),
			     temp, 0, OPTAB_LIB_WIDEN);
      anti_adjust_stack (temp);
    }

#ifdef STACK_GROWS_DOWNWARD
  temp = stack_pointer_rtx;
  if (extra != 0)
    temp = plus_constant (temp, extra);
#else
  temp = gen_rtx (PLUS, Pmode,
		  stack_pointer_rtx,
		  negate_rtx (Pmode, size));
  if (GET_CODE (size) != CONST_INT)
    temp = force_operand (temp, 0);
  if (extra != 0)
    temp = plus_constant (temp, -extra);
#endif

#ifdef STACK_POINTER_OFFSET
  temp = plus_constant (temp, STACK_POINTER_OFFSET);
#endif /* STACK_POINTER_OFFSET */

  return memory_address (QImode, temp);
}

static rtx
gen_push_operand ()
{
  return gen_rtx (
#ifdef STACK_GROWS_DOWNWARD
		  PRE_DEC,
#else
		  PRE_INC,
#endif
		  Pmode,
		  stack_pointer_rtx);
}

/* Generate code to push X onto the stack, assuming it has mode MODE.
   MODE is redundant except when X is a CONST_INT (since they don't
   carry mode info).
   SIZE is an rtx for the size of data to be copied (in bytes),
   needed only if X is BLKmode.

   ALIGN (in bytes) is maximum alignment we can assume.

   If PARTIAL is nonzero, then copy that many of the first words
   of X into registers starting with REG, and push the rest of X.
   The amount of space pushed is decreased by PARTIAL words,
   rounded *down* to a multiple of PARM_BOUNDARY.
   REG must be a hard register in this case.

   EXTRA is the amount in bytes of extra space to leave next to this arg.
   Within the function, we set EXTRA to zero once the padding is done,
   to avoid padding twice.

   On a machine that lacks real push insns, ARGS_ADDR is the address of
   the bottom of the argument block for this call.  We use indexing off there
   to store the arg.  On machines with push insns, ARGS_ADDR is 0.

   ARGS_SO_FAR is the size of args previously pushed for this call.  */

static void
emit_push_insn (x, mode, size, align, partial, reg, extra, args_addr, args_so_far)
     register rtx x;
     enum machine_mode mode;
     rtx size;
     int align;
     int partial;
     rtx reg;
     int extra;
     rtx args_addr;
     rtx args_so_far;
{
  rtx xinner;
  enum direction stack_direction
#ifdef STACK_GROWS_DOWNWARD
    = downward;
#else
    = upward;
#endif

  /* Decide where to pad the argument: `downward' for below,
     `upward' for above, or `none' for don't pad it.
     Default is below for small data on big-endian machines; else above.  */
  enum direction where_pad = FUNCTION_ARG_PADDING (mode, size);

  xinner = x = protect_from_queue (x, 0);

  if (extra)
    {
      if (args_addr == 0)
	{
	  /* Push padding now if padding above and stack grows down,
	     or if padding below and stack grows up.  */
	  if (where_pad != none && where_pad != stack_direction)
	    {
	      anti_adjust_stack (gen_rtx (CONST_INT, VOIDmode, extra));
	      extra = 0;
	    }
	}
      else
	{
	  /* If space already allocated, just adjust the address we use.  */
	  if (where_pad == downward)
	    {
	      args_so_far = plus_constant (args_so_far, extra);
	    }
	  /* If padding comes after a space already allocated,
	     there is nothing to do.  */
	  extra = 0;
	}
    }

  if (mode == BLKmode)
    {
      /* Copy a block into the stack, entirely or partially.  */

      register rtx temp;
      int used = partial * UNITS_PER_WORD;
      int offset = used % (PARM_BOUNDARY / BITS_PER_UNIT);
      int skip;
      
      if (size == 0)
	abort ();

      used -= offset;

      /* USED is now the # of bytes we need not copy to the stack
	 because registers will take care of them.  */

      if (partial != 0)
	xinner = change_address (xinner, BLKmode,
				 plus_constant (XEXP (xinner, 0), used));

/* If the partial register-part of the arg counts in its stack size,
   skip the part of stack space corresponding to the registers.
   Otherwise, start copying to the beginning of the stack space,
   by setting SKIP to 0.  */
#ifndef FIRST_PARM_CALLER_OFFSET
      skip = 0;
#else
      skip = used;
#endif

#ifdef PUSH_ROUNDING
      /* Do it with several push insns if that doesn't take lots of insns
	 and if there is no difficulty with push insns that skip bytes
	 on the stack for alignment purposes.  */
      if (args_addr == 0
	  && GET_CODE (size) == CONST_INT
	  && args_addr == 0
	  && skip == 0
	  && (move_by_pieces_ninsns ((unsigned) INTVAL (size) - used, align)
	      < MOVE_RATIO)
	  && PUSH_ROUNDING (INTVAL (size)) == INTVAL (size))
	move_by_pieces (gen_rtx (MEM, BLKmode, gen_push_operand ()), xinner,
			INTVAL (size) - used, align);
      else
#endif /* PUSH_ROUNDING */
	{
	  /* Otherwise make space on the stack and copy the data
	     to the address of that space.  */

	  /* Deduct words put into registers from the size we must copy.  */
	  if (partial != 0)
	    {
	      if (GET_CODE (size) == CONST_INT)
		size = gen_rtx (CONST_INT, VOIDmode, INTVAL (size) - used);
	      else
		size = expand_binop (GET_MODE (size), sub_optab, size,
				     gen_rtx (CONST_INT, VOIDmode, used),
				     0, 0, OPTAB_LIB_WIDEN);
	    }

	  /* Get the address of the stack space.  */
	  if (! args_addr)
	    {
	      temp = push_block (size, extra);
	      extra = 0;
	    }
	  else if (GET_CODE (args_so_far) == CONST_INT)
	    temp = memory_address (BLKmode,
				   plus_constant (args_addr,
						  skip + INTVAL (args_so_far)));
	  else
	    temp = memory_address (BLKmode,
				   plus_constant (gen_rtx (PLUS, Pmode,
							   args_addr, args_so_far),
						  skip));

	  /* TEMP is the address of the block.  Copy the data there.  */
	  if (GET_CODE (size) == CONST_INT
	      && (move_by_pieces_ninsns ((unsigned) INTVAL (size), align)
		  < MOVE_RATIO))
	    {
	      move_by_pieces (gen_rtx (MEM, BLKmode, temp), xinner,
			      INTVAL (size), align);
	      goto ret;
	    }
	  /* Try the most limited insn first, because there's no point
	     including more than one in the machine description unless
	     the more limited one has some advantage.  */
#ifdef HAVE_movstrqi
	  if (HAVE_movstrqi
	      && GET_CODE (size) == CONST_INT
	      && ((unsigned) INTVAL (size)
		  < (1 << (GET_MODE_BITSIZE (QImode) - 1))))
	    {
	      emit_insn (gen_movstrqi (gen_rtx (MEM, BLKmode, temp),
				       xinner, size,
				       gen_rtx (CONST_INT, VOIDmode, align)));
	      goto ret;
	    }
#endif
#ifdef HAVE_movstrhi
	  if (HAVE_movstrhi
	      && GET_CODE (size) == CONST_INT
	      && ((unsigned) INTVAL (size)
		  < (1 << (GET_MODE_BITSIZE (HImode) - 1))))
	    {
	      emit_insn (gen_movstrhi (gen_rtx (MEM, BLKmode, temp),
				       xinner, size,
				       gen_rtx (CONST_INT, VOIDmode, align)));
	      goto ret;
	    }
#endif
#ifdef HAVE_movstrsi
	  if (HAVE_movstrsi)
	    {
	      emit_insn (gen_movstrsi (gen_rtx (MEM, BLKmode, temp),
				       xinner, size,
				       gen_rtx (CONST_INT, VOIDmode, align)));
	      goto ret;
	    }
#endif

	  if (reg_mentioned_p (stack_pointer_rtx, temp))
	    {
	      /* Now that emit_library_call does force_operand
		 before pushing anything, preadjustment does not work.  */
	      temp = copy_to_reg (temp);
#if 0
	      /* Correct TEMP so it holds what will be a description of
		 the address to copy to, valid after one arg is pushed.  */
	      int xsize = GET_MODE_SIZE (Pmode);
#ifdef PUSH_ROUNDING
	      xsize = PUSH_ROUNDING (xsize);
#endif
	      xsize = ((xsize + PARM_BOUNDARY / BITS_PER_UNIT - 1)
		       / (PARM_BOUNDARY / BITS_PER_UNIT)
		       * (PARM_BOUNDARY / BITS_PER_UNIT));
#ifdef TARGET_MEM_FUNCTIONS
	      /* If we are calling bcopy, we push one arg before TEMP.
		 If calling memcpy, we push two.  */
	      xsize *= 2;
#endif
#ifdef STACK_GROWS_DOWNWARD
	      temp = plus_constant (temp, xsize);
#else
	      temp = plus_constant (temp, -xsize);
#endif /* not STACK_GROWS_DOWNWARD */
#endif /* 0 */
	    }

	  /* Make inhibit_defer_pop nonzero around the library call
	     to force it to pop the bcopy-arguments right away.  */
	  NO_DEFER_POP;
#ifdef TARGET_MEM_FUNCTIONS
	  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "memcpy"), 0,
			     VOIDmode, 3, temp, Pmode, XEXP (xinner, 0), Pmode,
			     size, Pmode);
#else
	  emit_library_call (gen_rtx (SYMBOL_REF, Pmode, "bcopy"), 0,
			     VOIDmode, 3, XEXP (xinner, 0), Pmode, temp, Pmode,
			     size, Pmode);
#endif
	  OK_DEFER_POP;
	}
    }
  else if (partial > 0)
    {
      /* Scalar partly in registers.  */

      int size = GET_MODE_SIZE (mode) / UNITS_PER_WORD;
      int i;
      int not_stack;
      /* # words of start of argument
	 that we must make space for but need not store.  */
      int offset = partial % (PARM_BOUNDARY / BITS_PER_WORD);
      int args_offset = INTVAL (args_so_far);
      int skip;

      /* If we make space by pushing it, we might as well push
	 the real data.  Otherwise, we can leave OFFSET nonzero
	 and leave the space uninitialized.  */
      if (args_addr == 0)
	offset = 0;

      /* Now NOT_STACK gets the number of words that we don't need to
	 allocate on the stack.  */
      not_stack = partial - offset;

/* If the partial register-part of the arg counts in its stack size,
   skip the part of stack space corresponding to the registers.
   Otherwise, start copying to the beginning of the stack space,
   by setting SKIP to 0.  */
#ifndef FIRST_PARM_CALLER_OFFSET
      skip = 0;
#else
      skip = not_stack;
#endif

      if (GET_CODE (x) == CONST_DOUBLE && x != dconst0_rtx)
	x = force_const_double_mem (x);

      /* Loop over all the words allocated on the stack for this arg.  */
      /* We can do it by words, because any scalar bigger than a word
	 has a size a multiple of a word.  */
#ifndef PUSH_ARGS_REVERSED
      for (i = not_stack; i < size; i++)
#else
      for (i = size - 1; i >= not_stack; i--)
#endif
	if (i >= not_stack + offset)
	  {
	    rtx wd;
	    rtx addr;
	    /* Get the next word of the value in WD.  */
	    if (GET_CODE (x) == MEM)
	      {
		rtx addr = memory_address (SImode,
					   plus_constant (XEXP (x, 0),
							  i * UNITS_PER_WORD));
		/* Copy to a reg, since machine may lack
		   memory-to-memory move insns.  */
		wd = copy_to_reg (gen_rtx (MEM, SImode, addr));
	      }
	    else if (GET_CODE (x) == REG)
	      wd = gen_rtx (SUBREG, SImode, x, i);
	    else if (x == dconst0_rtx || x == const0_rtx)
	      wd = const0_rtx;
	    else
	      abort ();

	    emit_push_insn (wd,
			    SImode, 0, align, 0, 0, 0, args_addr,
			    gen_rtx (CONST_INT, VOIDmode,
				     args_offset + (i - not_stack + skip) * UNITS_PER_WORD));
	  }
    }
  else
    {
      rtx addr;
#ifdef PUSH_ROUNDING
      if (args_addr == 0)
	addr = gen_push_operand ();
      else
#endif
	if (GET_CODE (args_so_far) == CONST_INT)
	  addr
	    = memory_address (mode,
			      plus_constant (args_addr, INTVAL (args_so_far)));
      else
	addr = memory_address (mode, gen_rtx (PLUS, Pmode, args_addr,
					      args_so_far));

      emit_move_insn (gen_rtx (MEM, mode, addr), x);
    }

 ret:
  /* If part should go in registers, copy that part
     into the appropriate registers.  Do this now, at the end,
     since mem-to-mem copies above may do function calls.  */
  if (partial > 0)
    move_block_to_reg (REGNO (reg), x, partial);

  if (extra)
    anti_adjust_stack (gen_rtx (CONST_INT, VOIDmode, extra));
}

/* Output a library call to function FUN (a SYMBOL_REF rtx)
   (emitting the queue unless NO_QUEUE is nonzero),
   for a value of mode OUTMODE,
   with NARGS different arguments, passed as alternating rtx values
   and machine_modes to convert them to.
   The rtx values should have been passed through protect_from_queue already.  */

void
emit_library_call (va_alist)
     va_dcl
{
  register va_list p;
  register int args_size = 0;
  register int argnum;
  enum machine_mode outmode;
  int nargs;
  rtx fun;
  rtx orgfun;
  int inc;
  int count;
  rtx *regvec;
  rtx argblock = 0;
  CUMULATIVE_ARGS args_so_far;
  struct arg { rtx value; enum machine_mode mode; rtx reg; int partial; };
  struct arg *argvec;
  int old_inhibit_defer_pop = inhibit_defer_pop;
  int stack_padding = 0;
  int no_queue = 0;
  rtx use_insns;

  va_start (p);
  orgfun = fun = va_arg (p, rtx);
  no_queue = va_arg (p, int);
  outmode = va_arg (p, enum machine_mode);
  nargs = va_arg (p, int);

  regvec = (rtx *) alloca (nargs * sizeof (rtx));

  /* Copy all the libcall-arguments out of the varargs data
     and into a vector ARGVEC.  */
  argvec = (struct arg *) alloca (nargs * sizeof (struct arg));

  INIT_CUMULATIVE_ARGS (args_so_far, (tree)0);
  for (count = 0; count < nargs; count++)
    {
      rtx val = va_arg (p, rtx);
      enum machine_mode mode = va_arg (p, enum machine_mode);
      int arg_size;

      argvec[count].value = val;

      /* Convert the arg value to the mode the library wants.
	 Also make sure it is a reasonable operand
	 for a move or push insn.  */
      /* ??? It is wrong to do it here; must do it earlier
	 where we know the signedness of the arg.  */
      if (GET_MODE (val) != mode && GET_MODE (val) != VOIDmode)
	{
	  val = gen_reg_rtx (mode);
	  convert_move (val, argvec[count].value, 0);
	}
      else if (GET_CODE (val) != REG && GET_CODE (val) != MEM
	       
	       && ! ((CONSTANT_P (val) || GET_CODE (val) == CONST_DOUBLE)
		     && LEGITIMATE_CONSTANT_P (val)))
	val = force_operand (val, 0);

      argvec[count].value = val;
      argvec[count].mode = mode;

      regvec[count] = FUNCTION_ARG (args_so_far, mode, (tree)0, 1);

#ifdef FUNCTION_ARG_PARTIAL_NREGS
      argvec[count].partial
	= FUNCTION_ARG_PARTIAL_NREGS (args_so_far, mode, (tree)0, 1);
#else
      argvec[count].partial = 0;
#endif

      FUNCTION_ARG_ADVANCE (args_so_far, mode, (tree)0, 1);
    }
  va_end (p);

  /* If we have no actual push instructions, make space for all the args
     right now.  */
#ifndef PUSH_ROUNDING
  for (count = 0; count < nargs; count++)
    {
      register enum machine_mode mode = argvec[count].mode;
      register rtx reg = regvec[count];
      register int partial = argvec[count].partial;

      if (reg == 0 || partial != 0)
	args_size += GET_MODE_SIZE (mode);
      if (partial != 0)
	args_size -= partial * GET_MODE_SIZE (SImode);
    }

  if (args_size != 0)
    {
#ifdef STACK_ARGS_ADJUST
      struct args_size size;
      size.constant = args_size;
      size.var = 0;
      STACK_ARGS_ADJUST (size);
      args_size = size.constant;
#endif
      argblock
	= push_block (round_push (gen_rtx (CONST_INT, VOIDmode, args_size)), 0);
    }
#endif /* no PUSH_ROUNDING */

#ifdef PUSH_ARGS_REVERSED
  inc = -1;
  argnum = nargs - 1;
#else
  inc = 1;
  argnum = 0;
#endif
  args_size = stack_padding;

  for (count = 0; count < nargs; count++, argnum += inc)
    {
      register enum machine_mode mode = argvec[argnum].mode;
      register rtx val = argvec[argnum].value;
      rtx reg = regvec[argnum];
      int partial = argvec[argnum].partial;
      int arg_size;

      if (reg != 0 && partial == 0)
	emit_move_insn (reg, val);
      else
	emit_push_insn (val, mode, 0, 0, partial, reg, 0, argblock,
			gen_rtx (CONST_INT, VOIDmode, args_size));

      /* Compute size of stack space used by this argument.  */
      if (reg == 0 || partial != 0)
	arg_size = GET_MODE_SIZE (mode);
      else
	arg_size = 0;
      if (partial != 0)
	arg_size
	  -= ((partial * UNITS_PER_WORD)
	      / (PARM_BOUNDARY / BITS_PER_UNIT)
	      * (PARM_BOUNDARY / BITS_PER_UNIT));

      args_size += arg_size;

      NO_DEFER_POP;
    }

  /* For version 1.37, try deleting this entirely.  */
  if (! no_queue)
    emit_queue ();

  fun = prepare_call_address (fun, 0);

  /* Any regs containing parms remain in use through the call.  */
  start_sequence ();
  for (count = 0; count < nargs; count++)
    if (regvec[count] != 0)
      emit_insn (gen_rtx (USE, VOIDmode, regvec[count]));

  use_insns = gen_sequence ();
  end_sequence ();

#ifdef STACK_BOUNDARY
  args_size = (args_size + STACK_BYTES - 1) / STACK_BYTES * STACK_BYTES;
#endif

  /* Don't allow popping to be deferred, since then
     cse'ing of library calls could delete a call and leave the pop.  */
  NO_DEFER_POP;
  emit_call_1 (fun, get_identifier (XSTR (orgfun, 0)), args_size,
	       FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1),
	       outmode != VOIDmode ? hard_libcall_value (outmode) : 0,
	       old_inhibit_defer_pop + 1, use_insns);
  OK_DEFER_POP;
}

/* Expand an assignment that stores the value of FROM into TO.
   If WANT_VALUE is nonzero, return an rtx for the value of TO.
   (This may contain a QUEUED rtx.)
   Otherwise, the returned value is not meaningful.

   SUGGEST_REG is no longer actually used.
   It used to mean, copy the value through a register
   and return that register, if that is possible.
   But now we do this if WANT_VALUE.

   If the value stored is a constant, we return the constant.  */

rtx
expand_assignment (to, from, want_value, suggest_reg)
     tree to, from;
     int want_value;
     int suggest_reg;
{
  register rtx to_rtx = 0;

  /* Don't crash if the lhs of the assignment was erroneous.  */

  if (TREE_CODE (to) == ERROR_MARK)
    return expand_expr (from, 0, VOIDmode, 0);

  /* Assignment of a structure component needs special treatment
     if the structure component's rtx is not simply a MEM.
     Assignment of an array element at a constant index
     has the same problem.  */

  if (TREE_CODE (to) == COMPONENT_REF
      || (TREE_CODE (to) == ARRAY_REF
	  && TREE_CODE (TREE_OPERAND (to, 1)) == INTEGER_CST
	  && TREE_CODE (TYPE_SIZE (TREE_TYPE (to))) == INTEGER_CST))
    {
      register enum machine_mode mode1;
      int bitsize;
      int volstruct = 0;
      tree tem = to;
      int bitpos = 0;
      int unsignedp;

      if (TREE_CODE (to) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (to, 1);
	  bitsize = TREE_INT_CST_LOW (DECL_SIZE (field)) * DECL_SIZE_UNIT (field);
	  mode1 = DECL_MODE (TREE_OPERAND (to, 1));
	  unsignedp = TREE_UNSIGNED (field);
	}
      else
	{
	  mode1 = TYPE_MODE (TREE_TYPE (to));
	  bitsize = GET_MODE_BITSIZE (mode1);
	  unsignedp = TREE_UNSIGNED (TREE_TYPE (to));
	}

      /* Compute cumulative bit-offset for nested component-refs
	 and array-refs, and find the ultimate containing object.  */

      while (1)
	{
	  if (TREE_CODE (tem) == COMPONENT_REF)
	    {
	      bitpos += DECL_OFFSET (TREE_OPERAND (tem, 1));
	      if (TREE_THIS_VOLATILE (tem))
		volstruct = 1;
	    }
	  else if (TREE_CODE (tem) == ARRAY_REF
		   && TREE_CODE (TREE_OPERAND (tem, 1)) == INTEGER_CST
		   && TREE_CODE (TYPE_SIZE (TREE_TYPE (tem))) == INTEGER_CST)
	    {
	      bitpos += (TREE_INT_CST_LOW (TREE_OPERAND (tem, 1))
			 * TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (tem)))
			 * TYPE_SIZE_UNIT (TREE_TYPE (tem)));
	      if (TREE_THIS_VOLATILE (tem))
		volstruct = 1;
	    }
	  else
	    break;
	  tem = TREE_OPERAND (tem, 0);
	}
      /* TEM is now the containing data object.  */

      /* If we are going to use store_bit_field and extract_bit_field,
	 make sure to_rtx will be safe for multiple use.  */
      if (mode1 == BImode && want_value)
	tem = stabilize_reference (tem);

      to_rtx = expand_expr (tem, 0, VOIDmode, 0);
      if (volstruct)
	{
	  if (GET_CODE (to_rtx) == MEM)
	    MEM_VOLATILE_P (to_rtx) = 1;
	  else
	    abort ();
	}

      return store_field (to_rtx, bitsize, bitpos, mode1, from,
			  (want_value
			   /* Spurious cast makes HPUX compiler happy.  */
			   ? (enum machine_mode) TYPE_MODE (TREE_TYPE (to))
			   : VOIDmode),
			  unsignedp,
			  /* Required alignment of containing datum.  */
			  TYPE_ALIGN (TREE_TYPE (tem)) / BITS_PER_UNIT,
			  int_size_in_bytes (TREE_TYPE (tem)));
    }

  /* Ordinary treatment.  Expand TO to get a REG or MEM rtx.
     Don't re-expand if it was expanded already (in COMPONENT_REF case).  */

  if (to_rtx == 0)
    to_rtx = expand_expr (to, 0, VOIDmode, 0);

  /* Compute FROM and store the value in the rtx we got.  */

  return store_expr (from, to_rtx, want_value);
}

/* Generate code for computing expression EXP,
   and storing the value into TARGET.
   Returns TARGET or an equivalent value.
   TARGET may contain a QUEUED rtx.

   If SUGGEST_REG is nonzero, copy the value through a register
   and return that register, if that is possible.

   If the value stored is a constant, we return the constant.  */

rtx
store_expr (exp, target, suggest_reg)
     register tree exp;
     register rtx target;
     int suggest_reg;
{
  register rtx temp;
  int dont_return_target = 0;

  /* Copying a non-constant CONSTRUCTOR needs special treatment.  */

  if (TREE_CODE (exp) == CONSTRUCTOR && ! TREE_LITERAL (exp))
    {
      store_constructor (exp, target);
      return target;
    }

  if (suggest_reg && GET_CODE (target) == MEM && GET_MODE (target) != BLKmode)
    /* If target is in memory and caller wants value in a register instead,
       arrange that.  Pass TARGET as target for expand_expr so that,
       if EXP is another assignment, SUGGEST_REG will be nonzero for it.
       We know expand_expr will not use the target in that case.  */
    {
      temp = expand_expr (exp, cse_not_expected ? 0 : target,
			  GET_MODE (target), 0);
      if (GET_MODE (temp) != BLKmode && GET_MODE (temp) != VOIDmode)
	temp = copy_to_reg (temp);
      dont_return_target = 1;
    }
  else if (queued_subexp_p (target))
    /* If target contains a postincrement, it is not safe
       to use as the returned value.  It would access the wrong
       place by the time the queued increment gets output.
       So copy the value through a temporary and use that temp
       as the result.  */
    {
      temp = expand_expr (exp, 0, GET_MODE (target), 0);
      if (GET_MODE (temp) != BLKmode && GET_MODE (temp) != VOIDmode)
	temp = copy_to_reg (temp);
      dont_return_target = 1;
    }
  else
    {
      temp = expand_expr (exp, target, GET_MODE (target), 0);
      /* DO return TARGET if it's a specified hardware register.
	 expand_return relies on this.  */
      if (!(target && GET_CODE (target) == REG
	    && REGNO (target) < FIRST_PSEUDO_REGISTER)
	  && (CONSTANT_P (temp) || GET_CODE (temp) == CONST_DOUBLE))
	dont_return_target = 1;
    }

  /* If value was not generated in the target, store it there.
     Convert the value to TARGET's type first if nec.  */

  if (temp != target && TREE_CODE (exp) != ERROR_MARK)
    {
      target = protect_from_queue (target, 1);
      if (GET_MODE (temp) != GET_MODE (target)
	  && GET_MODE (temp) != VOIDmode)
	{
	  int unsignedp = TREE_UNSIGNED (TREE_TYPE (exp));
	  if (dont_return_target)
	    {
	      /* In this case, we will return TEMP,
		 so make sure it has the proper mode.
		 But don't forget to store the value into TARGET.  */
	      temp = convert_to_mode (GET_MODE (target), temp, unsignedp);
	      emit_move_insn (target, temp);
	    }
	  else
	    convert_move (target, temp, unsignedp);
	}

      else if (GET_MODE (temp) == BLKmode)
	emit_block_move (target, temp, expr_size (exp),
			 TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);
      else
	emit_move_insn (target, temp);
    }
  if (dont_return_target)
    return temp;
  return target;
}

/* Store the value of constructor EXP into the rtx TARGET.
   TARGET is either a REG or a MEM.  */

static void
store_constructor (exp, target)
     tree exp;
     rtx target;
{
  /* Don't try copying piece by piece into a hard register
     since that is vulnerable to being clobbered by EXP.
     Instead, construct in a pseudo register and then copy it all.  */
  if (GET_CODE (target) == REG && REGNO (target) < FIRST_PSEUDO_REGISTER)
    {
      rtx temp = gen_reg_rtx (GET_MODE (target));
      store_constructor (exp, temp);
      emit_move_insn (target, temp);
      return;
    }

  if (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE)
    {
      register tree elt;

      /* If the constructor has fewer fields than the structure,
	 clear the whole structure first.  */

      if (list_length (CONSTRUCTOR_ELTS (exp))
	  != list_length (TYPE_FIELDS (TREE_TYPE (exp))))
	clear_storage (target, int_size_in_bytes (TREE_TYPE (exp)));
      else
	/* Inform later passes that the old value is dead.  */
	emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

      /* Store each element of the constructor into
	 the corresponding field of TARGET.  */

      for (elt = CONSTRUCTOR_ELTS (exp); elt; elt = TREE_CHAIN (elt))
	{
	  register tree field = TREE_PURPOSE (elt);
	  register enum machine_mode mode;
	  int bitsize;
	  int bitpos;
	  int unsignedp;

	  bitsize = TREE_INT_CST_LOW (DECL_SIZE (field)) * DECL_SIZE_UNIT (field);
	  mode = DECL_MODE (field);
	  unsignedp = TREE_UNSIGNED (field);

	  bitpos = DECL_OFFSET (field);

	  store_field (target, bitsize, bitpos, mode, TREE_VALUE (elt),
		       /* The alignment of TARGET is
			  at least what its type requires.  */
		       VOIDmode, 0,
		       TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT,
		       int_size_in_bytes (TREE_TYPE (exp)));
	}
    }
  else if (TREE_CODE (TREE_TYPE (exp)) == ARRAY_TYPE)
    {
      register tree elt;
      register int i;
      tree domain = TYPE_DOMAIN (TREE_TYPE (exp));
      int minelt = TREE_INT_CST_LOW (TYPE_MIN_VALUE (domain));
      int maxelt = TREE_INT_CST_LOW (TYPE_MAX_VALUE (domain));
      tree elttype = TREE_TYPE (TREE_TYPE (exp));

      /* If the constructor has fewer fields than the structure,
	 clear the whole structure first.  */

      if (list_length (CONSTRUCTOR_ELTS (exp)) < maxelt - minelt + 1)
	clear_storage (target, maxelt - minelt + 1);
      else
	/* Inform later passes that the old value is dead.  */
	emit_insn (gen_rtx (CLOBBER, VOIDmode, target));

      /* Store each element of the constructor into
	 the corresponding element of TARGET, determined
	 by counting the elements.  */
      for (elt = CONSTRUCTOR_ELTS (exp), i = 0;
	   elt;
	   elt = TREE_CHAIN (elt), i++)
	{
	  register enum machine_mode mode;
	  int bitsize;
	  int bitpos;
	  int unsignedp;

	  mode = TYPE_MODE (elttype);
	  bitsize = GET_MODE_BITSIZE (mode);
	  unsignedp = TREE_UNSIGNED (elttype);

	  bitpos = (i * TREE_INT_CST_LOW (TYPE_SIZE (elttype))
		    * TYPE_SIZE_UNIT (elttype));

	  store_field (target, bitsize, bitpos, mode, TREE_VALUE (elt),
		       /* The alignment of TARGET is
			  at least what its type requires.  */
		       VOIDmode, 0,
		       TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT,
		       int_size_in_bytes (TREE_TYPE (exp)));
	}
    }
}

/* Store the value of EXP (an expression tree)
   into a subfield of TARGET which has mode MODE and occupies
   BITSIZE bits, starting BITPOS bits from the start of TARGET.

   If VALUE_MODE is VOIDmode, return nothing in particular.
   UNSIGNEDP is not used in this case.

   Otherwise, return an rtx for the value stored.  This rtx
   has mode VALUE_MODE if that is convenient to do.
   In this case, UNSIGNEDP must be nonzero if the value is an unsigned type.

   ALIGN is the alignment that TARGET is known to have, measured in bytes.
   TOTAL_SIZE is its size in bytes, or -1 if variable.  */

static rtx
store_field (target, bitsize, bitpos, mode, exp, value_mode, unsignedp, align,
	     total_size)
     rtx target;
     int bitsize, bitpos;
     enum machine_mode mode;
     tree exp;
     enum machine_mode value_mode;
     int unsignedp;
     int align;
     int total_size;
{
  /* If the structure is in a register or if the component
     is a bit field, we cannot use addressing to access it.
     Use bit-field techniques or SUBREG to store in it.  */

  if (mode == BImode || GET_CODE (target) == REG
      || GET_CODE (target) == SUBREG)
    {
      store_bit_field (target, bitsize, bitpos,
		       mode,
		       expand_expr (exp, 0, VOIDmode, 0),
		       align, total_size);
      if (value_mode != VOIDmode)
	return extract_bit_field (target, bitsize, bitpos, unsignedp,
				  0, value_mode, 0, align, total_size);
      return const0_rtx;
    }
  else
    {
      rtx addr = XEXP (target, 0);
      rtx to_rtx;

      /* If a value is wanted, it must be the lhs;
	 so make the address stable for multiple use.  */

      if (value_mode != VOIDmode && GET_CODE (addr) != REG
	  && ! CONSTANT_ADDRESS_P (addr))
	addr = copy_to_reg (addr);

      /* Now build a reference to just the desired component.  */

      to_rtx = change_address (target, mode,
			       plus_constant (addr,
					      (bitpos / BITS_PER_UNIT)));
      MEM_IN_STRUCT_P (to_rtx) = 1;

      return store_expr (exp, to_rtx, value_mode != VOIDmode);
    }
}

/* Given an rtx VALUE that may contain additions and multiplications,
   return an equivalent value that just refers to a register or memory.
   This is done by generating instructions to perform the arithmetic
   and returning a pseudo-register containing the value.  */

rtx
force_operand (value, target)
     rtx value, target;
{
  register optab binoptab = 0;
  register rtx op2;
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  register rtx subtarget = (target != 0 && GET_CODE (target) == REG ? target : 0);

  if (GET_CODE (value) == PLUS)
    binoptab = add_optab;
  else if (GET_CODE (value) == MINUS)
    binoptab = sub_optab;
  else if (GET_CODE (value) == MULT)
    {
      op2 = XEXP (value, 1);
      if (!CONSTANT_P (op2)
	  && !(GET_CODE (op2) == REG && op2 != subtarget))
	subtarget = 0;
      return expand_mult (GET_MODE (value),
			  force_operand (XEXP (value, 0), subtarget),
			  force_operand (op2, 0),
			  target, 0);
    }

  if (binoptab)
    {
      op2 = XEXP (value, 1);
      if (!CONSTANT_P (op2)
	  && !(GET_CODE (op2) == REG && op2 != subtarget))
	subtarget = 0;
      if (binoptab == sub_optab
	  && GET_CODE (op2) == CONST_INT && INTVAL (op2) < 0)
	{
	  binoptab = add_optab;
	  op2 = gen_rtx (CONST_INT, VOIDmode, - INTVAL (op2));
	}
      return expand_binop (GET_MODE (value), binoptab,
			   force_operand (XEXP (value, 0), subtarget),
			   force_operand (op2, 0),
			   target, 0, OPTAB_LIB_WIDEN);
      /* We give UNSIGNEP = 0 to expand_binop
	 because the only operations we are expanding here are signed ones.  */
    }
  return value;
}

/* expand_expr: generate code for computing expression EXP.
   An rtx for the computed value is returned.  The value is never null.
   In the case of a void EXP, const0_rtx is returned.

   The value may be stored in TARGET if TARGET is nonzero.
   TARGET is just a suggestion; callers must assume that
   the rtx returned may not be the same as TARGET.

   If TARGET is CONST0_RTX, it means that the value will be ignored.

   If TMODE is not VOIDmode, it suggests generating the
   result in mode TMODE.  But this is done only when convenient.
   Otherwise, TMODE is ignored and the value generated in its natural mode.
   TMODE is just a suggestion; callers must assume that
   the rtx returned may not have mode TMODE.

   If MODIFIER is EXPAND_SUM then when EXP is an addition
   we can return an rtx of the form (MULT (REG ...) (CONST_INT ...))
   or a nest of (PLUS ...) and (MINUS ...) where the terms are
   products as above, or REG or MEM, or constant.
   Ordinarily in such cases we would output mul or add instructions
   and then return a pseudo reg containing the sum.

   If MODIFIER is EXPAND_CONST_ADDRESS then it is ok to return
   a MEM rtx whose address is a constant that isn't a legitimate address.  */

/* Subroutine of expand_expr:
   save the non-copied parts (LIST) of an expr (LHS), and return a list
   which can restore these values to their previous values,
   should something modify their storage.  */
static tree
save_noncopied_parts (lhs, list)
     tree lhs;
     tree list;
{
  tree tail;
  tree parts = 0;

  for (tail = list; tail; tail = TREE_CHAIN (tail))
    if (TREE_CODE (TREE_VALUE (tail)) == TREE_LIST)
      parts = chainon (parts, save_noncopied_parts (TREE_VALUE (tail)));
    else
      {
	tree part = TREE_VALUE (tail);
	tree part_type = TREE_TYPE (part);
	parts = tree_cons (save_expr (build_component_ref (lhs, part, parts, 0)),
			   build_nt (RTL_EXPR, 0, (tree) assign_stack_local (TYPE_MODE (part_type), int_size_in_bytes (part_type))),
			   parts);
	store_expr (TREE_PURPOSE (parts), RTL_EXPR_RTL (TREE_VALUE (parts)), 0);
      }
  return parts;
}

/* Subroutine of expand_expr:
   return the target to use when recursively expanding
   the first operand of an arithmetic operation.  */

static rtx
validate_subtarget (subtarget, otherop)
     rtx subtarget;
     tree otherop;
{
  if (TREE_LITERAL (otherop))
    return subtarget;
  if (TREE_CODE (otherop) == VAR_DECL
      && DECL_RTL (otherop) != subtarget)
    return subtarget;
  return 0;
}

rtx
expand_expr (exp, target, tmode, modifier)
     register tree exp;
     rtx target;
     enum machine_mode tmode;
     enum expand_modifier modifier;
{
  register rtx op0, op1, temp;
  tree type = TREE_TYPE (exp);
  register enum machine_mode mode = TYPE_MODE (type);
  register enum tree_code code = TREE_CODE (exp);
  optab this_optab;
  int negate_1;
  /* Use subtarget as the target for operand 0 of a binary operation.  */
  rtx subtarget = (target != 0 && GET_CODE (target) == REG ? target : 0);
  rtx original_target = target;
  int ignore = target == const0_rtx;

  /* Don't use hard regs as subtargets, because the combiner
     can only handle pseudo regs.  */
  if (subtarget && REGNO (subtarget) < FIRST_PSEUDO_REGISTER)
    subtarget = 0;
  /* Avoid subtargets inside loops,
     since they hide some invariant expressions.  */
  if (optimize && inside_loop ())
    subtarget = 0;

  if (ignore) target = 0, original_target = 0;

  /* If will do cse, generate all results into registers
     since 1) that allows cse to find more things
     and 2) otherwise cse could produce an insn the machine
     cannot support.  */

  if (! cse_not_expected && mode != BLKmode)
    target = subtarget;

  /* No sense saving up arithmetic to be done
     if it's all in the wrong mode to form part of an address.
     And force_operand won't know whether to sign-extend or zero-extend.  */

  if (mode != Pmode && modifier == EXPAND_SUM)
    modifier = EXPAND_NORMAL;

  /* Ensure we reference a volatile object even if value is ignored.  */
  if (ignore && TREE_THIS_VOLATILE (exp)
      && mode != VOIDmode && mode != BLKmode)
    {
      target = gen_reg_rtx (mode);
      temp = expand_expr (exp, target, VOIDmode, modifier);
      if (temp != target)
	emit_move_insn (target, temp);
      return target;
    }

  switch (code)
    {
    case PARM_DECL:
      if (DECL_RTL (exp) == 0)
	{
	  error_with_decl (exp, "prior parameter's size depends on `%s'");
	  return const0_rtx;
	}

    case FUNCTION_DECL:
    case VAR_DECL:
    case RESULT_DECL:
      if (DECL_RTL (exp) == 0)
	abort ();
      /* This is the case of an array whose size is to be determined
	 from its initializer, while the initializer is still being parsed.
	 See expand_decl.  */
      if (GET_CODE (DECL_RTL (exp)) == MEM
	  && GET_CODE (XEXP (DECL_RTL (exp), 0)) == REG)
	return change_address (DECL_RTL (exp), GET_MODE (DECL_RTL (exp)),
			       XEXP (DECL_RTL (exp), 0));
      if (GET_CODE (DECL_RTL (exp)) == MEM
	  && modifier != EXPAND_CONST_ADDRESS)
	{
	  /* DECL_RTL probably contains a constant address.
	     On RISC machines where a constant address isn't valid,
	     make some insns to get that address into a register.  */
	  if (!memory_address_p (DECL_MODE (exp), XEXP (DECL_RTL (exp), 0))
	      || (flag_force_addr
		  && CONSTANT_ADDRESS_P (XEXP (DECL_RTL (exp), 0))))
	    return change_address (DECL_RTL (exp), VOIDmode,
				   copy_rtx (XEXP (DECL_RTL (exp), 0)));
	}
      return DECL_RTL (exp);

    case INTEGER_CST:
      if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_INT)
	return gen_rtx (CONST_INT, VOIDmode, TREE_INT_CST_LOW (exp));
      /* Generate immediate CONST_DOUBLE
	 which will be turned into memory by reload if necessary.  */
      return immed_double_const (TREE_INT_CST_LOW (exp),
				 TREE_INT_CST_HIGH (exp),
				 mode);

    case CONST_DECL:
      return expand_expr (DECL_INITIAL (exp), target, VOIDmode, 0);

    case REAL_CST:
      /* If optimized, generate immediate CONST_DOUBLE
	 which will be turned into memory by reload if necessary.  */
      if (!cse_not_expected)
	return immed_real_const (exp);
    case COMPLEX_CST:
    case STRING_CST:
      if (! TREE_CST_RTL (exp))
	output_constant_def (exp);

      /* TREE_CST_RTL probably contains a constant address.
	 On RISC machines where a constant address isn't valid,
	 make some insns to get that address into a register.  */
      if (GET_CODE (TREE_CST_RTL (exp)) == MEM
	  && modifier != EXPAND_CONST_ADDRESS
	  && !memory_address_p (mode, XEXP (TREE_CST_RTL (exp), 0)))
	return change_address (TREE_CST_RTL (exp), VOIDmode,
			       copy_rtx (XEXP (TREE_CST_RTL (exp), 0)));
      return TREE_CST_RTL (exp);

    case SAVE_EXPR:
      if (SAVE_EXPR_RTL (exp) == 0)
	{
	  rtx reg = gen_reg_rtx (mode);
	  SAVE_EXPR_RTL (exp) = reg;
	  store_expr (TREE_OPERAND (exp, 0), reg, 0);
	  if (!optimize)
	    save_expr_regs = gen_rtx (EXPR_LIST, VOIDmode, reg,
				      save_expr_regs);
	}
      /* Don't let the same rtl node appear in two places.  */
      return SAVE_EXPR_RTL (exp);

    case LET_STMT:
      TREE_USED (exp) = 1;
      temp = expand_expr (STMT_BODY (exp), target, tmode, modifier);
      return temp;

    case RTL_EXPR:
      if (RTL_EXPR_SEQUENCE (exp) == const0_rtx)
	abort ();
      emit_insns (RTL_EXPR_SEQUENCE (exp));
      RTL_EXPR_SEQUENCE (exp) = const0_rtx;
      return RTL_EXPR_RTL (exp);

    case CONSTRUCTOR:
      /* All elts simple constants => refer to a constant in memory.  */
      if (TREE_STATIC (exp))
	/* For aggregate types with non-BLKmode modes,
	   this should ideally construct a CONST_INT.  */
	{
	  rtx constructor = output_constant_def (exp);
	  if (! memory_address_p (GET_MODE (constructor),
				  XEXP (constructor, 0)))
	    constructor = change_address (constructor, VOIDmode,
					  XEXP (constructor, 0));
	  return constructor;
	}

      if (ignore)
	{
	  tree elt;
	  for (elt = CONSTRUCTOR_ELTS (exp); elt; elt = TREE_CHAIN (elt))
	    expand_expr (TREE_VALUE (elt), const0_rtx, VOIDmode, 0);
	  return const0_rtx;
	}
      else
	{
	  if (target == 0)
	    target
	      = assign_stack_local (TYPE_MODE (TREE_TYPE (exp)),
				    int_size_in_bytes (TREE_TYPE (exp)));
	  store_expr (exp, target, 0);
	  return target;
	}

    case INDIRECT_REF:
      {
	tree exp1 = TREE_OPERAND (exp, 0);
	tree exp2;

	/* A SAVE_EXPR as the address in an INDIRECT_EXPR is generated
	   for  *PTR += ANYTHING  where PTR is put inside the SAVE_EXPR.
	   This code has the same general effect as simply doing
	   expand_expr on the save expr, except that the expression PTR
	   is computed for use as a memory address.  This means different
	   code, suitable for indexing, may be generated.  */
	if (TREE_CODE (exp1) == SAVE_EXPR
	    && SAVE_EXPR_RTL (exp1) == 0
	    && TREE_CODE (exp2 = TREE_OPERAND (exp1, 0)) != ERROR_MARK
	    && TYPE_MODE (TREE_TYPE (exp1)) == Pmode
	    && TYPE_MODE (TREE_TYPE (exp2)) == Pmode)
	  {
	    temp = expand_expr (TREE_OPERAND (exp1, 0), 0, VOIDmode, EXPAND_SUM);
	    op0 = memory_address (mode, temp);
	    op0 = copy_all_regs (op0);
	    SAVE_EXPR_RTL (exp1) = op0;
	  }
	else
	  {
	    op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, EXPAND_SUM);
	    op0 = memory_address (mode, op0);
	  }
      }
      temp = gen_rtx (MEM, mode, op0);
      /* If address was computed by addition,
	 mark this as an element of an aggregate.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == PLUS_EXPR
	  || (TREE_CODE (TREE_OPERAND (exp, 0)) == SAVE_EXPR
	      && TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)) == PLUS_EXPR))
	MEM_IN_STRUCT_P (temp) = 1;
      MEM_VOLATILE_P (temp) = TREE_THIS_VOLATILE (exp) || flag_volatile;
      RTX_UNCHANGING_P (temp) = TREE_READONLY (exp);
      return temp;

    case ARRAY_REF:
      if (TREE_CODE (TREE_OPERAND (exp, 1)) != INTEGER_CST
	  || TREE_CODE (TYPE_SIZE (TREE_TYPE (exp))) != INTEGER_CST)
	{
	  /* Nonconstant array index or nonconstant element size.
	     Generate the tree for *(&array+index) and expand that,
	     except do it in a language-independent way
	     and don't complain about non-lvalue arrays.
	     `mark_addressable' should already have been called
	     for any array for which this case will be reached.  */

	  /* Don't forget the const or volatile flag from the array element. */
	  tree variant_type = build_type_variant (type,
						  TREE_READONLY (exp),
						  TREE_THIS_VOLATILE (exp));
	  tree array_adr = build (ADDR_EXPR, TYPE_POINTER_TO (variant_type),
				  TREE_OPERAND (exp, 0));
	  tree index = TREE_OPERAND (exp, 1);
	  tree elt;

	  /* Convert the integer argument to a type the same size as a pointer
	     so the multiply won't overflow spuriously.  */
	  if (TYPE_PRECISION (TREE_TYPE (index)) != POINTER_SIZE)
	    index = convert (type_for_size (POINTER_SIZE, 0), index);

	  /* The array address isn't volatile even if the array is.  */
	  TREE_VOLATILE (array_adr) = 0;

	  elt = build (INDIRECT_REF, type,
		       fold (build (PLUS_EXPR, TYPE_POINTER_TO (variant_type),
				    array_adr,
				    fold (build (MULT_EXPR,
						 TYPE_POINTER_TO (variant_type),
						 index, size_in_bytes (type))))));

	  return expand_expr (elt, target, tmode, modifier);
	}

      /* Fold an expression like: "foo"[2].
	 This is not done in fold so it won't happen inside &.  */
      {
	int i;
	tree arg0 = TREE_OPERAND (exp, 0);
	tree arg1 = TREE_OPERAND (exp, 1);

	if (TREE_CODE (arg0) == STRING_CST
	    && TREE_CODE (arg1) == INTEGER_CST
	    && !TREE_INT_CST_HIGH (arg1)
	    && (i = TREE_INT_CST_LOW (arg1)) < TREE_STRING_LENGTH (arg0))
	  {
	    if (TREE_TYPE (TREE_TYPE (arg0)) == integer_type_node)
	      {
		exp = build_int_2 (((int *)TREE_STRING_POINTER (arg0))[i], 0);
		TREE_TYPE (exp) = integer_type_node;
		return expand_expr (exp, target, tmode, modifier);
	      }
	    if (TREE_TYPE (TREE_TYPE (arg0)) == char_type_node)
	      {
		exp = build_int_2 (TREE_STRING_POINTER (arg0)[i], 0);
		TREE_TYPE (exp) = integer_type_node;
		return expand_expr (convert (TREE_TYPE (TREE_TYPE (arg0)), exp), target, tmode, modifier);
	      }
	  }
      }

      /* If this is a constant index into a constant array,
	 just get the value from the array.  */
      if (TREE_READONLY (TREE_OPERAND (exp, 0))
	  && ! TREE_VOLATILE (TREE_OPERAND (exp, 0))
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == ARRAY_TYPE
	  && TREE_LITERAL (TREE_OPERAND (exp, 1))
	  && TREE_CODE (TREE_OPERAND (exp, 0)) == VAR_DECL
	  && DECL_INITIAL (TREE_OPERAND (exp, 0))
	  && TREE_CODE (DECL_INITIAL (TREE_OPERAND (exp, 0))) != ERROR_MARK)
	{
	  tree index = fold (TREE_OPERAND (exp, 1));
	  if (TREE_CODE (index) == INTEGER_CST)
	    {
	      int i = TREE_INT_CST_LOW (index);
	      tree init = CONSTRUCTOR_ELTS (DECL_INITIAL (TREE_OPERAND (exp, 0)));

	      while (init && i--)
		init = TREE_CHAIN (init);
	      if (init)
		return expand_expr (fold (TREE_VALUE (init)), target, tmode, modifier);
	    }
	}
      /* Treat array-ref with constant index as a component-ref.  */

    case COMPONENT_REF:
      {
	register enum machine_mode mode1;
	int volstruct = 0;
	int bitsize;
	tree tem = exp;
	int bitpos = 0;
	int unsignedp;

	if (TREE_CODE (exp) == COMPONENT_REF)
	  {
	    tree field = TREE_OPERAND (exp, 1);
	    bitsize = TREE_INT_CST_LOW (DECL_SIZE (field)) * DECL_SIZE_UNIT (field);
	    mode1 = DECL_MODE (TREE_OPERAND (exp, 1));
	    unsignedp = TREE_UNSIGNED (field);
	  }
	else
	  {
	    mode1 = TYPE_MODE (TREE_TYPE (exp));
	    bitsize = GET_MODE_BITSIZE (mode1);
	    unsignedp = TREE_UNSIGNED (TREE_TYPE (exp));
	  }

	/* Compute cumulative bit-offset for nested component-refs
	   and array-refs, and find the ultimate containing object.  */

	while (1)
	  {
	    if (TREE_CODE (tem) == COMPONENT_REF)
	      {
		bitpos += DECL_OFFSET (TREE_OPERAND (tem, 1));
		if (TREE_THIS_VOLATILE (tem))
		  volstruct = 1;
	      }
	    else if (TREE_CODE (tem) == ARRAY_REF
		     && TREE_CODE (TREE_OPERAND (tem, 1)) == INTEGER_CST
		     && TREE_CODE (TYPE_SIZE (TREE_TYPE (tem))) == INTEGER_CST)
	      {
		bitpos += (TREE_INT_CST_LOW (TREE_OPERAND (tem, 1))
			   * TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (tem)))
			   * TYPE_SIZE_UNIT (TREE_TYPE (tem)));
		if (TREE_THIS_VOLATILE (tem))
		  volstruct = 1;
	      }
	    else
	      break;
	    tem = TREE_OPERAND (tem, 0);
	  }

	op0 = expand_expr (tem, 0, VOIDmode,
			   (modifier == EXPAND_CONST_ADDRESS
			    ? modifier : EXPAND_NORMAL));

	if (mode1 == BImode || GET_CODE (op0) == REG
	    || GET_CODE (op0) == SUBREG)
	  return extract_bit_field (op0, bitsize, bitpos, unsignedp,
				    target, mode, tmode,
				    TYPE_ALIGN (TREE_TYPE (tem)) / BITS_PER_UNIT,
				    int_size_in_bytes (TREE_TYPE (tem)));
	/* Get a reference to just this component.  */
	if (modifier == EXPAND_CONST_ADDRESS)
	  op0 = gen_rtx (MEM, mode1, plus_constant (XEXP (op0, 0),
						    (bitpos / BITS_PER_UNIT)));
	else
	  op0 = change_address (op0, mode1,
				plus_constant (XEXP (op0, 0),
					       (bitpos / BITS_PER_UNIT)));
	MEM_IN_STRUCT_P (op0) = 1;
	MEM_VOLATILE_P (op0) |= volstruct;
	/* If OP0 is in the shared structure-value stack slot,
	   and it is not BLKmode, copy it into a register.
	   The shared slot may be clobbered at any time by another call.
	   BLKmode is safe because our caller will either copy the value away
	   or take another component and come back here.  */
	if (mode != BLKmode
	    && TREE_CODE (TREE_OPERAND (exp, 0)) == CALL_EXPR
	    && TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))) == BLKmode)
	  op0 = copy_to_reg (op0);
	if (mode == mode1 || mode1 == BLKmode || mode1 == tmode)
	  return op0;
	if (target == 0)
	  target = gen_reg_rtx (tmode != VOIDmode ? tmode : mode);
	convert_move (target, op0, unsignedp);
	return target;
      }

      /* Intended for a reference to a buffer of a file-object in Pascal.
	 But it's not certain that a special tree code will really be
	 necessary for these.  INDIRECT_REF might work for them.  */
    case BUFFER_REF:
      abort ();

    case WITH_CLEANUP_EXPR:
      RTL_EXPR_RTL (TREE_OPERAND (exp, 1))
	= expand_expr (TREE_OPERAND (exp, 0), target, tmode, modifier);
      cleanups_of_this_call = tree_cons (0, TREE_OPERAND (exp, 2), cleanups_of_this_call);
      return RTL_EXPR_RTL (TREE_OPERAND (exp, 1));

    case CALL_EXPR:
      /* Check for a built-in function.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)) == FUNCTION_DECL
	  && (DECL_FUNCTION_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	      != NOT_BUILT_IN))
	return expand_builtin (exp, target, subtarget, tmode, ignore);
      /* If this call was expanded already by preexpand_calls,
	 just return the result we got.  */
      if (CALL_EXPR_RTL (exp) != 0)
	return CALL_EXPR_RTL (exp);
      return expand_call (exp, target, ignore);

    case NOP_EXPR:
    case CONVERT_EXPR:
    case REFERENCE_EXPR:
      if (TREE_CODE (type) == VOID_TYPE || ignore)
	{
	  expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, modifier);
	  return const0_rtx;
	}
      if (mode == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))))
	return expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, modifier);
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, mode, 0);
      if (GET_MODE (op0) == mode || GET_MODE (op0) == VOIDmode)
	return op0;
      if (flag_force_mem && GET_CODE (op0) == MEM)
	op0 = copy_to_reg (op0);
      if (GET_MODE (op0) == VOIDmode)
	/* Avoid problem in convert_move due to unknown mode of OP0.  */
	op0 = copy_to_mode_reg (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))),
				op0);
      if (target == 0)
	target = gen_reg_rtx (mode);
      convert_move (target, op0, TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0))));
      return target;

    case PLUS_EXPR:
      preexpand_calls (exp);
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST
	  && modifier == EXPAND_SUM)
	{
	  op1 = expand_expr (TREE_OPERAND (exp, 1), subtarget, VOIDmode, EXPAND_SUM);
	  op1 = plus_constant (op1, TREE_INT_CST_LOW (TREE_OPERAND (exp, 0)));
	  return op1;
	}
      negate_1 = 1;
    plus_minus:
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	  && modifier == EXPAND_SUM)
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, EXPAND_SUM);
	  op0 = plus_constant (op0,
			       negate_1 * TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)));
	  return op0;
	}
      this_optab = add_optab;
      if (modifier != EXPAND_SUM) goto binop;
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, EXPAND_SUM);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, EXPAND_SUM);
      /* Put a sum last, to simplify what follows.  */
#ifdef OLD_INDEXING
      if (GET_CODE (op1) == MULT)
	{
	  temp = op0;
	  op0 = op1;
	  op1 = temp;
	}
#endif
#ifndef OLD_INDEXING
      /* Make sure any term that's a sum with a constant comes last.  */
      if (GET_CODE (op0) == PLUS
	  && CONSTANT_P (XEXP (op0, 1)))
	{
	  temp = op0;
	  op0 = op1;
	  op1 = temp;
	}
      /* If adding to a sum including a constant,
	 associate it to put the constant outside.  */
      if (GET_CODE (op1) == PLUS
	  && CONSTANT_P (XEXP (op1, 1)))
	{
	  rtx tem;
	  int constant_term = 0;

	  op0 = gen_rtx (PLUS, mode, XEXP (op1, 0), op0);
	  /* Let's also eliminate constants from op0 if possible.  */
	  tem = eliminate_constant_term (op0, &constant_term);
	  if (GET_CODE (XEXP (op1, 1)) == CONST_INT)
	    {
	      if (constant_term != 0)
		return plus_constant (tem, INTVAL (XEXP (op1, 1)) + constant_term);
	      else
		return plus_constant (op0, INTVAL (XEXP (op1, 1)));
	    }
	  else
	    return gen_rtx (PLUS, mode, op0, XEXP (op1, 1));
	}
#endif
      return gen_rtx (PLUS, mode, op0, op1);

    case MINUS_EXPR:
      preexpand_calls (exp);
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	  && GET_MODE_BITSIZE (TYPE_MODE (type)) <= HOST_BITS_PER_INT)
	{
	  int negated;
	  if (modifier == EXPAND_SUM)
	    {
	      negate_1 = -1;
	      goto plus_minus;
	    }
	  subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
	  negated = - TREE_INT_CST_LOW (TREE_OPERAND (exp, 1));
	  if (GET_MODE_BITSIZE (mode) < HOST_BITS_PER_INT)
	    negated &= (1 << GET_MODE_BITSIZE (mode)) - 1;
	  op1 = gen_rtx (CONST_INT, VOIDmode, negated);
	  this_optab = add_optab;
	  goto binop2;
	}
      this_optab = sub_optab;
      goto binop;

    case MULT_EXPR:
      preexpand_calls (exp);
      /* If first operand is constant, swap them.
	 Thus the following special case checks need only
	 check the second operand.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == INTEGER_CST)
	{
	  register tree t1 = TREE_OPERAND (exp, 0);
	  TREE_OPERAND (exp, 0) = TREE_OPERAND (exp, 1);
	  TREE_OPERAND (exp, 1) = t1;
	}

      /* Attempt to return something suitable for generating an
	 indexed address, for machines that support that.  */

      if (modifier == EXPAND_SUM
	  && TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	{
	  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, EXPAND_SUM);

	  /* Apply distributive law if OP0 is x+c.  */
	  if (GET_CODE (op0) == PLUS
	      && GET_CODE (XEXP (op0, 1)) == CONST_INT)
	    return gen_rtx (PLUS, mode,
			    gen_rtx (MULT, mode, XEXP (op0, 0),
				     gen_rtx (CONST_INT, VOIDmode,
					      TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)))),
			    gen_rtx (CONST_INT, VOIDmode,
				     (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1))
				      * INTVAL (XEXP (op0, 1)))));

	  if (GET_CODE (op0) != REG)
	    op0 = force_operand (op0, 0);
	  if (GET_CODE (op0) != REG)
	    op0 = copy_to_mode_reg (mode, op0);

	  return gen_rtx (MULT, mode, op0,
			  gen_rtx (CONST_INT, VOIDmode,
				   TREE_INT_CST_LOW (TREE_OPERAND (exp, 1))));
	}
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      /* Check for multiplying things that have been extended
	 from a narrower type.  If this machine supports multiplying
	 in that narrower type with a result in the desired type,
	 do it that way, and avoid the explicit type-conversion.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == NOP_EXPR
	  && TREE_CODE (TREE_TYPE (exp)) == INTEGER_TYPE
	  && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
	      < TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (exp, 0))))
	  && ((TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST
	       && int_fits_type_p (TREE_OPERAND (exp, 1),
				   TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
	       /* Don't use a widening multiply if a shift will do.  */
	       && exact_log2 (TREE_INT_CST_LOW (TREE_OPERAND (exp, 1))) < 0)
	      ||
	      (TREE_CODE (TREE_OPERAND (exp, 1)) == NOP_EXPR
	       && (TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 1), 0)))
		   ==
		   TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))))
	       /* If both operands are extended, they must either both
		  be zero-extended or both be sign-extended.  */
	       && (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 1), 0)))
		   ==
		   TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))))))
	{
	  enum machine_mode innermode
	    = TYPE_MODE (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)));
	  this_optab = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)))
			? umul_widen_optab : smul_widen_optab);
	  if (mode == GET_MODE_WIDER_MODE (innermode)
	      && this_optab->handlers[(int) mode].insn_code != CODE_FOR_nothing)
	    {
	      op0 = expand_expr (TREE_OPERAND (TREE_OPERAND (exp, 0), 0),
				 0, VOIDmode, 0);
	      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
		op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
	      else
		op1 = expand_expr (TREE_OPERAND (TREE_OPERAND (exp, 1), 0),
				   0, VOIDmode, 0);
	      goto binop2;
	    }
	}
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      return expand_mult (mode, op0, op1, target, TREE_UNSIGNED (type));

    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      preexpand_calls (exp);
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      /* Possible optimization: compute the dividend with EXPAND_SUM
	 then if the divisor is constant can optimize the case
	 where some terms of the dividend have coeffs divisible by it.  */
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      return expand_divmod (0, code, mode, op0, op1, target,
			    TREE_UNSIGNED (type));

    case RDIV_EXPR:
      preexpand_calls (exp);
      this_optab = flodiv_optab;
      goto binop;

    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
      preexpand_calls (exp);
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      return expand_divmod (1, code, mode, op0, op1, target,
			    TREE_UNSIGNED (type));
#if 0
#ifdef HAVE_divmoddisi4
      if (GET_MODE (op0) != DImode)
	{
	  temp = gen_reg_rtx (DImode);
	  convert_move (temp, op0, 0);
	  op0 = temp;
	  if (GET_MODE (op1) != SImode && GET_CODE (op1) != CONST_INT)
	    {
	      temp = gen_reg_rtx (SImode);
	      convert_move (temp, op1, 0);
	      op1 = temp;
	    }
	  temp = gen_reg_rtx (SImode);
	  if (target == 0)
	    target = gen_reg_rtx (SImode);
	  emit_insn (gen_divmoddisi4 (temp, protect_from_queue (op0, 0),
				      protect_from_queue (op1, 0),
				      protect_from_queue (target, 1)));
	  return target;
	}
#endif
#endif

    case FIX_ROUND_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_CEIL_EXPR:
      abort ();			/* Not used for C.  */

    case FIX_TRUNC_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      if (target == 0)
	target = gen_reg_rtx (mode);
      {
	int unsignedp = TREE_UNSIGNED (TREE_TYPE (exp));
	if (mode == HImode || mode == QImode)
	  {
	    register rtx temp = gen_reg_rtx (SImode);
	    expand_fix (temp, op0, 0);
	    convert_move (target, temp, 0);
	  }
	else
	  expand_fix (target, op0, unsignedp);
      }
      return target;

    case FLOAT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      if (target == 0)
	target = gen_reg_rtx (mode);
      if (GET_MODE (op0) == VOIDmode)
	/* Avoid problem in convert_move due to unknown mode of OP0.  */
	op0 = copy_to_mode_reg (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0))),
				op0);
      {
	int unsignedp = TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0)));
	if (GET_MODE (op0) == HImode
	    || GET_MODE (op0) == QImode)
	  {
	    register rtx temp = gen_reg_rtx (SImode);
	    convert_move (temp, op0, unsignedp);
	    expand_float (target, temp, 0);
	  }
	else
	  expand_float (target, op0, unsignedp);
      }
      return target;

    case NEGATE_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      temp = expand_unop (mode, neg_optab, op0, target, 0);
      if (temp == 0)
	abort ();
      return temp;

    case ABS_EXPR:
      /* First try to do it with a special abs instruction.
	 If that does not win, use conditional jump and negate.  */
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      temp = expand_unop (mode, abs_optab, op0, target, 0);
      if (temp != 0)
	return temp;
      temp = gen_label_rtx ();
      if (target == 0 || GET_CODE (target) != REG)
	target = gen_reg_rtx (mode);
      emit_move_insn (target, op0);
      emit_cmp_insn (target,
		     expand_expr (convert (TREE_TYPE (exp), integer_zero_node),
				  0, VOIDmode, 0),
		     0, 0, 0);
      NO_DEFER_POP;
      emit_jump_insn (gen_bge (temp));
      op0 = expand_unop (mode, neg_optab, target, target, 0);
      if (op0 != target)
	emit_move_insn (target, op0);
      emit_label (temp);
      OK_DEFER_POP;
      return target;

    case MAX_EXPR:
    case MIN_EXPR:
      mode = TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 1)));
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      if (target == 0 || GET_CODE (target) != REG || target == op1)
	target = gen_reg_rtx (mode);
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      if (target != op0)
	emit_move_insn (target, op0);
      op0 = gen_label_rtx ();
      if (code == MAX_EXPR)
	temp = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 1)))
		? compare1 (target, op1, GEU, LEU, 1, mode)
		: compare1 (target, op1, GE, LE, 0, mode));
      else
	temp = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 1)))
		? compare1 (target, op1, LEU, GEU, 1, mode)
		: compare1 (target, op1, LE, GE, 0, mode));
      if (temp == const0_rtx)
	emit_move_insn (target, op1);
      else if (temp != const1_rtx)
	{
	  if (bcc_gen_fctn[(int) GET_CODE (temp)] != 0)
	    emit_jump_insn ((*bcc_gen_fctn[(int) GET_CODE (temp)]) (op0));
	  else
	    abort ();
	  emit_move_insn (target, op1);
	}
      emit_label (op0);
      return target;

/* ??? Can optimize when the operand of this is a bitwise operation,
   by using a different bitwise operation.  */
    case BIT_NOT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      temp = expand_unop (mode, one_cmpl_optab, op0, target, 1);
      if (temp == 0)
	abort ();
      return temp;

    case FFS_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      temp = expand_unop (mode, ffs_optab, op0, target, 1);
      if (temp == 0)
	abort ();
      return temp;

/* ??? Can optimize bitwise operations with one arg constant.
   Pastel optimizes (a bitwise1 n) bitwise2 (a bitwise3 b)
   and (a bitwise1 b) bitwise2 b (etc)
   but that is probably not worth while.  */

/* BIT_AND_EXPR is for bitwise anding.
   TRUTH_AND_EXPR is for anding two boolean values
   when we want in all cases to compute both of them.
   In general it is fastest to do TRUTH_AND_EXPR by
   computing both operands as actual zero-or-1 values
   and then bitwise anding.  In cases where there cannot
   be any side effects, better code would be made by
   treating TRUTH_AND_EXPR like TRUTH_ANDIF_EXPR;
   but the question is how to recognize those cases.  */

    case TRUTH_AND_EXPR:
    case BIT_AND_EXPR:
      preexpand_calls (exp);
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
      return expand_bit_and (mode, op0, op1, target);

/* See comment above about TRUTH_AND_EXPR; it applies here too.  */
    case TRUTH_OR_EXPR:
    case BIT_IOR_EXPR:
      preexpand_calls (exp);
      this_optab = ior_optab;
      goto binop;

    case BIT_XOR_EXPR:
      preexpand_calls (exp);
      this_optab = xor_optab;
      goto binop;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      preexpand_calls (exp);
      subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
      op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
      return expand_shift (code, mode, op0, TREE_OPERAND (exp, 1), target,
			   TREE_UNSIGNED (type));

/* ??? cv's were used to effect here to combine additive constants
   and to determine the answer when only additive constants differ.
   Also, the addition of one can be handled by changing the condition.  */
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
      preexpand_calls (exp);
      temp = do_store_flag (exp, target, mode);
      if (temp != 0)
	return temp;
      /* For foo != 0, load foo, and if it is nonzero load 1 instead. */
      if (code == NE_EXPR && integer_zerop (TREE_OPERAND (exp, 1))
	  && subtarget
	  && (GET_MODE (subtarget)
	      == TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))))
	{
	  temp = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
	  if (temp != subtarget)
	    temp = copy_to_reg (temp);
	  op1 = gen_label_rtx ();
	  emit_cmp_insn (temp, const0_rtx, 0, TREE_UNSIGNED (type), 0);
	  emit_jump_insn (gen_beq (op1));
	  emit_move_insn (temp, const1_rtx);
	  emit_label (op1);
	  return temp;
	}
      /* If no set-flag instruction, must generate a conditional
	 store into a temporary variable.  Drop through
	 and handle this like && and ||.  */

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      temp = gen_reg_rtx (mode);
      emit_clr_insn (temp);
      op1 = gen_label_rtx ();
      jumpifnot (exp, op1);
      emit_0_to_1_insn (temp);
      emit_label (op1);
      return temp;

    case TRUTH_NOT_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), target, VOIDmode, 0);
      /* The parser is careful to generate TRUTH_NOT_EXPR
	 only with operands that are always zero or one.  */
      temp = expand_binop (mode, xor_optab, op0,
			   gen_rtx (CONST_INT, mode, 1),
			   target, 1, OPTAB_LIB_WIDEN);
      if (temp == 0)
	abort ();
      return temp;

    case COMPOUND_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      emit_queue ();
      return expand_expr (TREE_OPERAND (exp, 1), target, VOIDmode, 0);

    case COND_EXPR:
      /* Note that COND_EXPRs whose type is a structure or union
	 are required to be constructed to contain assignments of
	 a temporary variable, so that we can evaluate them here
	 for side effect only.  If type is void, we must do likewise.  */
      op0 = gen_label_rtx ();
      op1 = gen_label_rtx ();

      if (mode == VOIDmode || ignore)
	temp = 0;
      else if (target)
	temp = target;
      else if (mode == BLKmode)
	{
	  if (TYPE_SIZE (type) == 0 || ! TREE_LITERAL (TYPE_SIZE (type)))
	    abort ();
	  temp = assign_stack_local (BLKmode,
				     (TREE_INT_CST_LOW (TYPE_SIZE (type))
				      * TYPE_SIZE_UNIT (type)
				      + BITS_PER_UNIT - 1)
				     / BITS_PER_UNIT);
	}
      else
	temp = gen_reg_rtx (mode);

      jumpifnot (TREE_OPERAND (exp, 0), op0);
      NO_DEFER_POP;
      if (temp != 0)
	store_expr (TREE_OPERAND (exp, 1), temp, 0);
      else
	expand_expr (TREE_OPERAND (exp, 1), ignore ? const0_rtx : 0,
		     VOIDmode, 0);
      emit_queue ();
      emit_jump_insn (gen_jump (op1));
      emit_barrier ();
      emit_label (op0);
      if (temp != 0)
	store_expr (TREE_OPERAND (exp, 2), temp, 0);
      else
	expand_expr (TREE_OPERAND (exp, 2), ignore ? const0_rtx : 0,
		     VOIDmode, 0);
      emit_queue ();
      emit_label (op1);
      OK_DEFER_POP;
      return temp;

    case MODIFY_EXPR:
      {
	/* If lhs is complex, expand calls in rhs before computing it.
	   That's so we don't compute a pointer and save it over a call.
	   If lhs is simple, compute it first so we can give it as a
	   target if the rhs is just a call.  This avoids an extra temp and copy
	   and that prevents a partial-subsumption which makes bad code.
	   Actually we could treat component_ref's of vars like vars.  */

	tree lhs = TREE_OPERAND (exp, 0);
	tree rhs = TREE_OPERAND (exp, 1);
	tree noncopied_parts;

	if (TREE_CODE (lhs) != VAR_DECL
	    && TREE_CODE (lhs) != RESULT_DECL
	    && TREE_CODE (lhs) != PARM_DECL)
	  preexpand_calls (exp);

	noncopied_parts = save_noncopied_parts (lhs, TYPE_NONCOPIED_PARTS (TREE_TYPE (lhs)));
	temp = expand_assignment (lhs, rhs, ! ignore, original_target != 0);
	while (noncopied_parts != 0)
	  {
	    store_expr (TREE_VALUE (noncopied_parts),
			SAVE_EXPR_RTL (TREE_PURPOSE (noncopied_parts)), 0);
	    noncopied_parts = TREE_CHAIN (noncopied_parts);
	  }
	return temp;
      }

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
      return expand_increment (exp, 0);

    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      return expand_increment (exp, 1);

    case ADDR_EXPR:
      op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode,
			 EXPAND_CONST_ADDRESS);
      if (GET_CODE (op0) != MEM)
	abort ();
      if (modifier == EXPAND_SUM)
	return XEXP (op0, 0);
      op0 = force_operand (XEXP (op0, 0), target);
      if (flag_force_addr && GET_CODE (op0) != REG)
	return force_reg (Pmode, op0);
      return op0;

    case ENTRY_VALUE_EXPR:
      abort ();

    case ERROR_MARK:
      return const0_rtx;

    default:
      abort ();
    }

  /* Here to do an ordinary binary operator, generating an instruction
     from the optab already placed in `this_optab'.  */
 binop:
  /* Detect things like x = y | (a == b)
     and do them as (x = y), (a == b ? x |= 1 : 0), x.  */
  /* First, get the comparison or conditional into the second arg.  */
  if (comparison_code[(int) TREE_CODE (TREE_OPERAND (exp, 0))]
      || (TREE_CODE (TREE_OPERAND (exp, 0)) == COND_EXPR
	  && (integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 0), 1))
	      || integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 0), 2)))))
    {
      if (this_optab == ior_optab || this_optab == add_optab
	  || this_optab == xor_optab)
	{
	  tree exch = TREE_OPERAND (exp, 1);
	  TREE_OPERAND (exp, 1) = TREE_OPERAND (exp, 0);
	  TREE_OPERAND (exp, 0) = exch;
	}
    }
  /* Optimize X + (Y ? Z : 0) by computing X and maybe adding Z.  */
  if (comparison_code[(int) TREE_CODE (TREE_OPERAND (exp, 1))]
      || (TREE_CODE (TREE_OPERAND (exp, 1)) == COND_EXPR
	  && (integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 1), 1))
	      || integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 1), 2)))))
    {
      if (this_optab == ior_optab || this_optab == add_optab
	  || this_optab == xor_optab || this_optab == sub_optab
	  || this_optab == lshl_optab || this_optab == ashl_optab
	  || this_optab == lshr_optab || this_optab == ashr_optab
	  || this_optab == rotl_optab || this_optab == rotr_optab)
	{
	  tree thenexp;
	  rtx thenv = 0;

	  /* TARGET gets a reg in which we can perform the computation.
	     Use the specified target if it's a pseudo reg and safe.  */
	  target = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
	  if (target == 0) target = gen_reg_rtx (mode);

	  /* Compute X into the target.  */
	  store_expr (TREE_OPERAND (exp, 0), target, 0);
	  op0 = gen_label_rtx ();

	  /* If other operand is a comparison COMP, treat it as COMP ? 1 : 0 */
	  if (TREE_CODE (TREE_OPERAND (exp, 1)) != COND_EXPR)
	    {
	      do_jump (TREE_OPERAND (exp, 1), op0, 0);
	      thenv = const1_rtx;
	    }
	  else if (integer_zerop (TREE_OPERAND (TREE_OPERAND (exp, 1), 2)))
	    {
	      do_jump (TREE_OPERAND (TREE_OPERAND (exp, 1), 0), op0, 0);
	      thenexp = TREE_OPERAND (TREE_OPERAND (exp, 1), 1);
	    }
	  else
	    {
	      do_jump (TREE_OPERAND (TREE_OPERAND (exp, 1), 0), 0, op0);
	      thenexp = TREE_OPERAND (TREE_OPERAND (exp, 1), 2);
	    }

	  if (thenv == 0)
	    thenv = expand_expr (thenexp, 0, VOIDmode, 0);

	  /* THENV is now Z, the value to operate on, as an rtx.
	     We have already tested that Y isn't zero, so do the operation.  */

	  if (this_optab == rotl_optab || this_optab == rotr_optab)
	    temp = expand_binop (mode, this_optab, target, thenv, target,
				 -1, OPTAB_LIB);
	  else if (this_optab == lshl_optab || this_optab == lshr_optab)
	    temp = expand_binop (mode, this_optab, target, thenv, target,
				 1, OPTAB_LIB_WIDEN);
	  else
	    temp = expand_binop (mode, this_optab, target, thenv, target,
				 0, OPTAB_LIB_WIDEN);
	  if (target != temp)
	    emit_move_insn (target, temp);

	  emit_queue ();
	  do_pending_stack_adjust ();
	  emit_label (op0);
	  return target;
	}
    }
  subtarget = validate_subtarget (subtarget, TREE_OPERAND (exp, 1));
  op0 = expand_expr (TREE_OPERAND (exp, 0), subtarget, VOIDmode, 0);
  op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
 binop2:
  temp = expand_binop (mode, this_optab, op0, op1, target,
		       TREE_UNSIGNED (TREE_TYPE (exp)), OPTAB_LIB_WIDEN);
  if (temp == 0)
    abort ();
  return temp;
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

static rtx
expand_builtin (exp, target, subtarget, mode, ignore)
     tree exp;
     rtx target;
     rtx subtarget;
     enum machine_mode mode;
     int ignore;
{
  tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  rtx op0;

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    case BUILT_IN_ABS:
    case BUILT_IN_LABS:
    case BUILT_IN_FABS:
      /* build_function_call changes these into ABS_EXPR.  */
      abort ();

    case BUILT_IN_SAVEREGS:
      if (saveregs_value != 0)
	return saveregs_value;
      {
	/* When this function is called, it means that registers must be
	   saved on entry to this function.  So we migrate the
	   call to the first insn of this function.  */
	rtx last = get_last_insn ();
	/* Now really call the function.  `expand_call' does not call
	   expand_builtin, so there is no danger of infinite recursion here.  */
	rtx temp = expand_call (exp, target, ignore);
	reorder_insns (NEXT_INSN (last), get_last_insn (), get_insns ());
	saveregs_value = temp;
	return temp;
      }

    case BUILT_IN_NEXT_ARG:
      {
	tree fntype = TREE_TYPE (current_function_decl);
	if (!(TYPE_ARG_TYPES (fntype) != 0
	      && (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
		  != void_type_node)))
	  {
	    error ("`stdarg.h' facilities used, but function has fixed args");
	    return const0_rtx;
	  }
      }

      return expand_binop (Pmode, add_optab,
			   arg_pointer_rtx, current_function_arg_offset_rtx,
			   0, 0, OPTAB_LIB_WIDEN);

    case BUILT_IN_CLASSIFY_TYPE:
      if (arglist != 0)
	{
	  tree type = TREE_TYPE (TREE_VALUE (arglist));
	  enum tree_code code = TREE_CODE (type);
	  if (code == VOID_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, void_type_class);
	  if (code == INTEGER_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, integer_type_class);
	  if (code == CHAR_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, char_type_class);
	  if (code == ENUMERAL_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, enumeral_type_class);
	  if (code == BOOLEAN_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, boolean_type_class);
	  if (code == POINTER_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, pointer_type_class);
	  if (code == REFERENCE_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, reference_type_class);
	  if (code == OFFSET_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, offset_type_class);
	  if (code == REAL_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, real_type_class);
	  if (code == COMPLEX_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, complex_type_class);
	  if (code == FUNCTION_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, function_type_class);
	  if (code == METHOD_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, method_type_class);
	  if (code == RECORD_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, record_type_class);
	  if (code == UNION_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, union_type_class);
	  if (code == ARRAY_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, array_type_class);
	  if (code == STRING_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, string_type_class);
	  if (code == SET_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, set_type_class);
	  if (code == FILE_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, file_type_class);
	  if (code == LANG_TYPE)
	    return gen_rtx (CONST_INT, VOIDmode, lang_type_class);
	}
      return gen_rtx (CONST_INT, VOIDmode, no_type_class);

    case BUILT_IN_ALLOCA:
      if (arglist == 0
	  /* Arg could be non-integer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE)
	return const0_rtx;
      frame_pointer_needed = 1;
      current_function_calls_alloca = 1;
      /* Compute the argument.  */
      op0 = expand_expr (TREE_VALUE (arglist), 0, VOIDmode, 0);
      if (! CONSTANT_P (op0))
	{
	  op0 = force_reg (GET_MODE (op0), op0);
	  if (GET_MODE (op0) != Pmode)
	    op0 = convert_to_mode (Pmode, op0, 1);
	}
      /* Push that much space (rounding it up).  */
      do_pending_stack_adjust ();

#ifdef STACK_POINTER_OFFSET
      /* If we will have to round the result down (which is up
	 if stack grows down), make sure we have extra space so the
	 user still gets at least as much space as he asked for.  */
      if ((STACK_POINTER_OFFSET + STACK_BYTES - 1) / STACK_BYTES
	  != STACK_POINTER_OFFSET / STACK_BYTES)
	op0 = plus_constant (op0, STACK_BYTES);
#endif

#ifdef STACK_GROWS_DOWNWARD
      anti_adjust_stack (round_push (op0));
#endif
      /* Return a copy of current stack ptr, in TARGET if possible.  */
      if (target)
	emit_move_insn (target, stack_pointer_rtx);
      else
	target = copy_to_reg (stack_pointer_rtx);
#ifdef STACK_POINTER_OFFSET
      /* If the contents of the stack pointer reg are offset from the
	 actual top-of-stack address, add the offset here.  */
      if (GET_CODE (target) == REG)
	emit_insn (gen_add2_insn (target,
				  gen_rtx (CONST_INT, VOIDmode,
					   (STACK_POINTER_OFFSET + STACK_BYTES - 1) / STACK_BYTES * STACK_BYTES)));
      else
	{
	  rtx temp =
	    expand_binop (GET_MODE (target), add_optab, target,
			  gen_rtx (CONST_INT, VOIDmode,
				   (STACK_POINTER_OFFSET + STACK_BYTES - 1) / STACK_BYTES * STACK_BYTES),
			  target,
			  1, OPTAB_DIRECT);
	  if (temp == 0) abort ();
	  if (temp != target)
	    emit_move_insn (target, temp);
	}
#endif
#ifndef STACK_GROWS_DOWNWARD
      anti_adjust_stack (round_push (op0));
#endif
      /* Some systems require a particular insn to refer to the stack
	 to make the pages exist.  */
#ifdef HAVE_probe
      if (HAVE_probe)
	emit_insn (gen_probe ());
#endif
      return target;

    case BUILT_IN_FFS:
      if (arglist == 0
	  /* Arg could be non-integer if user redeclared this fcn wrong.  */
	  || TREE_CODE (TREE_TYPE (TREE_VALUE (arglist))) != INTEGER_TYPE)
	return const0_rtx;

      /* Compute the argument.  */
      op0 = expand_expr (TREE_VALUE (arglist), subtarget, VOIDmode, 0);
      /* Compute ffs, into TARGET if possible.
	 Set TARGET to wherever the result comes back.  */
      target = expand_unop (GET_MODE (op0), ffs_optab, op0, target, 1);
      if (target == 0)
	abort ();
      return target;

    default:
      abort ();
    }
}

/* Expand code for a post- or pre- increment or decrement
   and return the RTX for the result.
   POST is 1 for postinc/decrements and 0 for preinc/decrements.  */

static rtx
expand_increment (exp, post)
     register tree exp;
     int post;
{
  register rtx op0, op1;
  register rtx temp;
  register tree incremented = TREE_OPERAND (exp, 0);
  optab this_optab = add_optab;
  int icode;
  enum machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  int op0_is_copy = 0;

  /* Stabilize any component ref that might need to be
     evaluated more than once below.  */
  if (TREE_CODE (incremented) == COMPONENT_REF
      && (TREE_CODE (TREE_OPERAND (incremented, 0)) != INDIRECT_REF
	  || DECL_MODE (TREE_OPERAND (incremented, 1)) == BImode))
    incremented = stabilize_reference (incremented);

  /* Compute the operands as RTX.
     Note whether OP0 is the actual lvalue or a copy of it:
     I believe it is a copy iff it is a register and insns were
     generated in computing it.  */
  temp = get_last_insn ();
  op0 = expand_expr (incremented, 0, VOIDmode, 0);
  if (temp != get_last_insn ())
    op0_is_copy = (GET_CODE (op0) == REG || GET_CODE (op0) == SUBREG);
  op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);

  /* Decide whether incrementing or decrementing.  */
  if (TREE_CODE (exp) == POSTDECREMENT_EXPR
      || TREE_CODE (exp) == PREDECREMENT_EXPR)
    this_optab = sub_optab;

  /* If OP0 is not the actual lvalue, but rather a copy in a register,
     then we cannot just increment OP0.  We must
     therefore contrive to increment the original value.
     Then we can return OP0 since it is a copy of the old value.  */
  if (op0_is_copy)
    {
      /* This is the easiest way to increment the value wherever it is.
	 Problems with multiple evaluation of INCREMENTED
	 are prevented because either (1) it is a component_ref,
	 in which case it was stabilized above, or (2) it is an array_ref
	 with constant index in an array in a register, which is
	 safe to reevaluate.  */
      tree newexp = build ((this_optab == add_optab
			    ? PLUS_EXPR : MINUS_EXPR),
			   TREE_TYPE (exp),
			   incremented,
			   TREE_OPERAND (exp, 1));
      temp = expand_assignment (incremented, newexp, ! post, 0);
      return post ? op0 : temp;
    }

  /* Convert decrement by a constant into a negative increment.  */
  if (this_optab == sub_optab
      && GET_CODE (op1) == CONST_INT)
    {
      op1 = gen_rtx (CONST_INT, VOIDmode, - INTVAL (op1));
      this_optab = add_optab;
    }

  if (post)
    {
      /* We have a true reference to the value in OP0.
	 If there is an insn to add or subtract in this mode, queue it.  */

      /* I'm not sure this is still necessary.  */
      op0 = stabilize (op0);

      icode = (int) this_optab->handlers[(int) mode].insn_code;
      if (icode != (int) CODE_FOR_nothing
	  /* Make sure that OP0 is valid for operands 0 and 1
	     of the insn we want to queue.  */
	  && (*insn_operand_predicate[icode][0]) (op0, mode)
	  && (*insn_operand_predicate[icode][1]) (op0, mode))
	{
	  if (! (*insn_operand_predicate[icode][2]) (op1, mode))
	    op1 = force_reg (mode, op1);

	  return enqueue_insn (op0, GEN_FCN (icode) (op0, op0, op1));
	}
    }

  /* Preincrement, or we can't increment with one simple insn.  */
  if (post)
    /* Save a copy of the value before inc or dec, to return it later.  */
    temp = copy_to_reg (op0);
  else
    /* Arrange to return the incremented value.  */
    /* Copy the rtx because expand_binop will protect from the queue,
       and the results of that would be invalid for us to return
       if our caller does emit_queue before using our result.  */
    temp = copy_rtx (op0);

  /* Increment however we can.  */
  op1 = expand_binop (mode, this_optab, op0, op1, op0,
		      TREE_UNSIGNED (TREE_TYPE (exp)), OPTAB_LIB_WIDEN);
  /* Make sure the value is stored into OP0.  */
  if (op1 != op0)
    emit_move_insn (op0, op1);

  return temp;
}

/* Expand all function calls contained within EXP, innermost ones first.
   But don't look within expressions that have sequence points.
   For each CALL_EXPR, record the rtx for its value
   in the CALL_EXPR_RTL field.

   Calls that return large structures for which a structure return
   stack slot is needed are not preexpanded.  Preexpanding them loses
   because if more than one were preexpanded they would try to use the
   same stack slot.  */

static void
preexpand_calls (exp)
     tree exp;
{
  register int nops, i;

  if (! do_preexpand_calls)
    return;

  /* Only expressions and references can contain calls.  */

  if (tree_code_type[(int) TREE_CODE (exp)][0] != 'e'
      && tree_code_type[(int) TREE_CODE (exp)][0] != 'r')
    return;

  switch (TREE_CODE (exp))
    {
    case CALL_EXPR:
      /* Do nothing to built-in functions.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0)) == FUNCTION_DECL
	  && (DECL_FUNCTION_CODE (TREE_OPERAND (TREE_OPERAND (exp, 0), 0))
	      != NOT_BUILT_IN))
	return;
      /* Precompute calls that don't return values in memory.  */
      if (CALL_EXPR_RTL (exp) == 0
	  && TYPE_MODE (TREE_TYPE (exp)) != BLKmode
	  && ! RETURN_IN_MEMORY (TREE_TYPE (exp)))
	CALL_EXPR_RTL (exp) = expand_call (exp, 0, 0);
      return;

    case COMPOUND_EXPR:
    case COND_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
      /* If we find one of these, then we can be sure
	 the adjust will be done for it (since it makes jumps).
	 Do it now, so that if this is inside an argument
	 of a function, we don't get the stack adjustment
	 after some other args have already been pushed.  */
      do_pending_stack_adjust ();
      return;

    case RTL_EXPR:
      return;

    case SAVE_EXPR:
      if (SAVE_EXPR_RTL (exp) != 0)
	return;
    }

  nops = tree_code_length[(int) TREE_CODE (exp)];
  for (i = 0; i < nops; i++)
    if (TREE_OPERAND (exp, i) != 0)
      {
	register int type = *tree_code_type[(int) TREE_CODE (TREE_OPERAND (exp, i))];
	if (type == 'e' || type == 'r')
	  preexpand_calls (TREE_OPERAND (exp, i));
      }
}

/* Force FUNEXP into a form suitable for the address of a CALL,
   and return that as an rtx.  Also load the static chain register
   from either FUNEXP or CONTEXT.  */

static rtx
prepare_call_address (funexp, context)
     rtx funexp;
     rtx context;
{
  funexp = protect_from_queue (funexp, 0);
  if (context != 0)
    context = protect_from_queue (context, 0);

  /* Function variable in language with nested functions.  */
  if (GET_MODE (funexp) == EPmode)
    {
      emit_move_insn (static_chain_rtx, gen_highpart (Pmode, funexp));
      funexp = memory_address (FUNCTION_MODE, gen_lowpart (Pmode, funexp));
      emit_insn (gen_rtx (USE, VOIDmode, static_chain_rtx));
    }
  else
    {
      if (context != 0)
	/* Unless function variable in C, or top level function constant */
	emit_move_insn (static_chain_rtx, lookup_static_chain (context));

      /* Make a valid memory address and copy constants thru pseudo-regs,
	 but not for a constant address if -fno-function-cse.  */
      if (GET_CODE (funexp) != SYMBOL_REF)
	funexp = memory_address (FUNCTION_MODE, funexp);
      else
	{
#ifndef NO_FUNCTION_CSE
	  if (optimize && ! flag_no_function_cse)
	    funexp = force_reg (Pmode, funexp);
#endif
	}

      if (context != 0)
	emit_insn (gen_rtx (USE, VOIDmode, static_chain_rtx));
    }
  return funexp;
}

/* Generate instructions to call function FUNEXP,
   and optionally pop the results.
   The CALL_INSN is the first insn generated.

   FUNTYPE is the data type of the function, or, for a library call,
   the identifier for the name of the call.  This is given to the
   macro RETURN_POPS_ARGS to determine whether this function pops its own args.

   STACK_SIZE is the number of bytes of arguments on the stack,
   rounded up to STACK_BOUNDARY; zero if the size is variable.
   This is both to put into the call insn and
   to generate explicit popping code if necessary.

   NEXT_ARG_REG is the rtx that results from executing
     FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1)
   just after all the args have had their registers assigned.
   This could be whatever you like, but normally it is the first
   arg-register beyond those used for args in this call,
   or 0 if all the arg-registers are used in this call.
   It is passed on to `gen_call' so you can put this info in the call insn.

   VALREG is a hard register in which a value is returned,
   or 0 if the call does not return a value.

   OLD_INHIBIT_DEFER_POP is the value that `inhibit_defer_pop' had before
   the args to this call were processed.
   We restore `inhibit_defer_pop' to that value.

   USE_INSNS is a SEQUENCE of USE insns to be emitted immediately before
   the actual CALL insn.  */

static void
emit_call_1 (funexp, funtype, stack_size, next_arg_reg, valreg, old_inhibit_defer_pop, use_insns)
     rtx funexp;
     tree funtype;
     int stack_size;
     rtx next_arg_reg;
     rtx valreg;
     int old_inhibit_defer_pop;
     rtx use_insns;
{
  rtx stack_size_rtx = gen_rtx (CONST_INT, VOIDmode, stack_size);
  rtx call_insn;

  if (valreg)
    emit_call_insn (gen_call_value (valreg,
				    gen_rtx (MEM, FUNCTION_MODE, funexp),
				    stack_size_rtx, next_arg_reg));
  else
    emit_call_insn (gen_call (gen_rtx (MEM, FUNCTION_MODE, funexp),
			      stack_size_rtx, next_arg_reg));

  /* Find the CALL insn we just emitted and write the USE insns before it.  */
  for (call_insn = get_last_insn();
       call_insn && GET_CODE (call_insn) != CALL_INSN;
       call_insn = PREV_INSN (call_insn))
    ;

  if (! call_insn)
    abort ();

  /* Put the USE insns before the CALL.  */
  emit_insn_before (use_insns, call_insn);

  inhibit_defer_pop = old_inhibit_defer_pop;

  /* If returning from the subroutine does not automatically pop the args,
     we need an instruction to pop them sooner or later.
     Perhaps do it now; perhaps just record how much space to pop later.  */

  if (! RETURN_POPS_ARGS (funtype)
      && stack_size != 0)
    {
      if (flag_defer_pop && inhibit_defer_pop == 0)
	pending_stack_adjust += stack_size;
      else
	adjust_stack (stack_size_rtx);
    }
}

/* At the start of a function, record that we have no previously-pushed
   arguments waiting to be popped.  */

void
init_pending_stack_adjust ()
{
  pending_stack_adjust = 0;
}

/* When exiting from function, if safe, clear out any pending stack adjust
   so the adjustment won't get done.  */

void
clear_pending_stack_adjust ()
{
#ifdef EXIT_IGNORE_STACK
  if (!flag_omit_frame_pointer && EXIT_IGNORE_STACK
      && ! TREE_INLINE (current_function_decl)
      && ! flag_inline_functions)
    pending_stack_adjust = 0;
#endif
}

/* Pop any previously-pushed arguments that have not been popped yet.  */

void
do_pending_stack_adjust ()
{
  if (inhibit_defer_pop == 0)
    {
      if (pending_stack_adjust != 0)
	adjust_stack (gen_rtx (CONST_INT, VOIDmode, pending_stack_adjust));
      pending_stack_adjust = 0;
    }
}

/* Data structure and subroutines used within expand_call.  */

struct arg_data
{
  /* Tree node for this argument.  */
  tree tree_value;
  /* Precomputed RTL value, or 0 if it isn't precomputed.  */
  rtx value;
  /* Register to pass this argument in, or 0 if passed on stack.  */
  rtx reg;
  /* Number of registers to use.  0 means put the whole arg in registers.
     Also 0 if not passed in registers.  */
  int partial;
  /* Offset of this argument from beginning of stack-args.  */
  struct args_size offset;
  /* Size of this argument on the stack, rounded up for any padding it gets,
     parts of the argument passed in registers do not count.
     If the FIRST_PARM_CALLER_OFFSET is negative, then register parms
     are counted here as well.  */
  struct args_size size;
  /* Nonzero if this arg has already been stored.  */
  int stored;
  /* const0_rtx means should preallocate stack space for this arg.
     Other non0 value is the stack slot, preallocated.
     Used only for BLKmode.  */
  rtx stack;
};

static void store_one_arg ();
static rtx target_for_arg ();

/* Generate all the code for a function call
   and return an rtx for its value.
   Store the value in TARGET (specified as an rtx) if convenient.
   If the value is stored in TARGET then TARGET is returned.
   If IGNORE is nonzero, then we ignore the value of the function call.  */

static rtx
expand_call (exp, target, ignore)
     tree exp;
     rtx target;
     int ignore;
{
  /* List of actual parameters.  */
  tree actparms = TREE_OPERAND (exp, 1);
  /* RTX for the function to be called.  */
  rtx funexp;
  /* Data type of the function.  */
  tree funtype;
  /* Declaration of the function being called,
     or 0 if the function is computed (not known by name).  */
  tree fndecl = 0;

  /* Register in which non-BLKmode value will be returned,
     or 0 if no value or if value is BLKmode.  */
  rtx valreg;
  /* Address where we should return a BLKmode value;
     0 if value not BLKmode.  */
  rtx structure_value_addr = 0;
  /* Nonzero if that address is being passed by treating it as
     an extra, implicit first parameter.  Otherwise,
     it is passed by being copied directly into struct_value_rtx.  */
  int structure_value_addr_parm = 0;
  /* Save get_structure_value_addr data to prevent multiple use.  */
  rtx saved_structure_value_addr;
  int saved_structure_value_size;
  /* Nonzero if called function returns an aggregate in memory PCC style,
     by returning the address of where to find it.  */
  int pcc_struct_value = 0;
  /* Insn that was used to copy the result to the specified target,
     or 0 if no such insn.  */
  rtx result_copy_insn = 0;

  /* Number of actual parameters in this call, including struct value addr.  */
  int num_actuals;
  /* Number of named args.  Args after this are anonymous ones
     and they must all go on the stack.  */
  int n_named_args;
  /* Count arg position in order args appear.  */
  int argpos;

  /* Vector of information about each argument.
     Arguments are numbered in the order they will be pushed,
     not the order they are written.  */
  struct arg_data *args;

  /* Total size in bytes of all the stack-parms scanned so far.  */
  struct args_size args_size;
  /* Remember initial value of args_size.constant.  */
  int starting_args_size;
  /* Nonzero means count reg-parms' size in ARGS_SIZE.  */
  int stack_count_regparms = 0;
  /* Data on reg parms scanned so far.  */
  CUMULATIVE_ARGS args_so_far;
  /* Nonzero if a reg parm has been scanned.  */
  int reg_parm_seen;
  /* Nonzero if we must avoid push-insns in the args for this call.  */
  int must_preallocate;
  /* 1 if scanning parms front to back, -1 if scanning back to front.  */
  int inc;
  /* Address of space preallocated for stack parms
     (on machines that lack push insns), or 0 if space not preallocated.  */
  rtx argblock = 0;
  /* Amount to align the stack by before or after we push any args.  */
  int stack_align = 0;

  /* Nonzero if it is plausible that this is a call to alloca.  */
  int may_be_alloca;
  /* Nonzero if this is a call to setjmp or a related function.  */
  int is_setjmp;
  /* Nonzero if this is a call to an inline function.  */
  int is_integrable = 0;
  /* Nonzero if this is a call to __builtin_new.  */
  int is_builtin_new;
  /* Nonzero if this is a call to a `const' function.  */
  int is_const = 0;

  /* Nonzero if there are BLKmode args whose data types require them
     to be passed in memory, not (even partially) in registers.  */
  int BLKmode_parms_forced = 0;
  /* The offset of the first BLKmode parameter which 
     *must* be passed in memory.  */
  int BLKmode_parms_first_offset = 0;
  /* Total size of BLKmode parms which could usefully be preallocated.  */
  int BLKmode_parms_sizes = 0;

  /* Amount stack was adjusted to protect BLKmode parameters
     which are below the nominal "stack address" value.  */
  rtx protected_stack = 0;

  /* The last insn before the things that are intrinsically part of the call.
     The beginning reg-note goes on the insn after this one.  */
  rtx insn_before;

  rtx old_stack_level = 0;
  int old_pending_adj;
  int old_inhibit_defer_pop = inhibit_defer_pop;
  tree old_cleanups = cleanups_of_this_call;
  rtx use_insns;

  register tree p;
  register int i;

  /* See if we can find a DECL-node for the actual function.
     As a result, decide whether this is a call to an integrable function.  */

  p = TREE_OPERAND (exp, 0);
  if (TREE_CODE (p) == ADDR_EXPR)
    {
      fndecl = TREE_OPERAND (p, 0);
      if (TREE_CODE (fndecl) != FUNCTION_DECL)
	fndecl = 0;
      else
	{
	  extern tree current_function_decl;

	  if (fndecl != current_function_decl
	      && DECL_SAVED_INSNS (fndecl))
	    is_integrable = 1;
	  else
	    {
	      /* In case this function later becomes inlineable,
		 record that there was already a non-inline call to it.  */
	      mark_addressable (fndecl);
	    }

	  if (TREE_READONLY (fndecl) && ! TREE_THIS_VOLATILE (fndecl))
	    is_const = 1;
	}
    }

  /* When calling a const function, we must pop the stack args right away,
     so that the pop is deleted or moved with the call.  */
  if (is_const)
    NO_DEFER_POP;

  /* Set up a place to return a structure.  */

  /* Cater to broken compilers.  */
  if (aggregate_value_p (exp))
    {
      /* This call returns a big structure.  */
#ifdef PCC_STATIC_STRUCT_RETURN
      if (flag_pcc_struct_return)
	{
	  pcc_struct_value = 1;
	  is_integrable = 0;  /* Easier than making that case work right.  */
	}
      else
#endif
	{
	  if (target && GET_CODE (target) == MEM)
	    {
	      structure_value_addr = XEXP (target, 0);
	      if (reg_mentioned_p (stack_pointer_rtx, structure_value_addr))
		structure_value_addr = copy_to_reg (structure_value_addr);
	    }
	  else
	    {
	      push_structure_value (&saved_structure_value_addr,
				    &saved_structure_value_size);
	      /* Make room on the stack to hold the value.  */
	      structure_value_addr
		= get_structure_value_addr (expr_size (exp));
	      target = 0;
	    }
	}
    }

  /* If called function is inline, try to integrate it.  */

  if (is_integrable)
    {
      extern rtx expand_inline_function ();
      rtx temp;

      temp = expand_inline_function (fndecl, actparms, target,
				     ignore, TREE_TYPE (exp),
				     structure_value_addr);

      /* If inlining succeeded, return.  */
      if ((int) temp != -1)
	return temp;

      /* If inlining failed, mark FNDECL as needing to be compiled
	 separately after all.  */
      TREE_ADDRESSABLE (fndecl) = 1;
      TREE_ADDRESSABLE (DECL_NAME (fndecl)) = 1;
    }

#if 0
  /* Unless it's a call to a specific function that isn't alloca,
     if it has one argument, we must assume it might be alloca.  */

  may_be_alloca =
    (!(fndecl != 0
       && strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)),
		  "alloca"))
     && actparms != 0
     && TREE_CHAIN (actparms) == 0);
#else
  /* We assume that alloca will always be called by name.  It
     makes no sense to pass it as a pointer-to-function to
     anything that does not understand its behavior.  */
  may_be_alloca =
    (fndecl && (! strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "alloca")
		|| ! strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)),
			     "__builtin_alloca")));
#endif

  /* See if this is a call to a function that can return more than once.  */

  is_setjmp
    = (fndecl != 0
       && (!strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "setjmp")
	   || !strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "_setjmp")));

  is_builtin_new
    = (fndecl != 0
       && (!strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "__builtin_new")));

  if (may_be_alloca)
    {
      frame_pointer_needed = 1;
      may_call_alloca = 1;
      current_function_calls_alloca = 1;
    }

  /* Don't let pending stack adjusts add up to too much.
     Also, do all pending adjustments now
     if there is any chance this might be a call to alloca
     or if it is const.  */

  if (pending_stack_adjust >= 32 || is_const
      || (pending_stack_adjust > 0 && may_be_alloca))
    do_pending_stack_adjust ();

  /* Operand 0 is a pointer-to-function; get the type of the function.  */
  funtype = TREE_TYPE (TREE_OPERAND (exp, 0));
  if (TREE_CODE (funtype) != POINTER_TYPE)
    abort ();
  funtype = TREE_TYPE (funtype);

  /* If structure_value_addr is set, it means pass the address
     as if it were an extra parameter.  We typically avoid doing
     so here, which would imply that the caller has to pop it off
     the stack; but some compilers do expect caller pop. */
  if (structure_value_addr
#ifdef STRUCT_RETURN_CALLER_POP
      && flag_pcc_struct_return
#else
      && struct_value_rtx == 0
#endif
      )
    {
      rtx tem;

      INIT_CUMULATIVE_ARGS (args_so_far, funtype);
      tem = FUNCTION_ARG (args_so_far, Pmode,
			  build_pointer_type (TREE_TYPE (funtype)), 1);
      if (tem == 0)
	{
	  actparms = tree_cons (error_mark_node,
				build (SAVE_EXPR,
				       type_for_size (GET_MODE_BITSIZE (Pmode), 0),
				       0,
				       force_reg (Pmode, structure_value_addr)),
				actparms);
	  structure_value_addr_parm = 1;
	}
#ifdef STRUCT_RETURN_CALLER_POP
      /* Moved in 1.39 from before the preceding open-brace.
	 Should be safe without the conditional because,
	 if STRUCT_RETURN_CALLER_POP is not defined,
	 this can still happen only if struct_value_rtx is 0,
	 and in that case, we would crash anyway if this weren't done.  */
      structure_value_addr_parm = 1;
#endif
    }

  /* Count the arguments and set NUM_ACTUALS.  */
  for (p = actparms, i = 0; p; p = TREE_CHAIN (p)) i++;
  num_actuals = i;

  /* Compute number of named args.
     Don't include the last named arg if anonymous args follow.
     (If no anonymous args follow, the result of list_length
     is actually one too large.)  */
  if (TYPE_ARG_TYPES (funtype) != 0)
    n_named_args = list_length (TYPE_ARG_TYPES (funtype)) - 1;
  else
    /* If we know nothing, treat all args as named.  */
    n_named_args = num_actuals;

  /* Make a vector to hold all the information about each arg.  */
  args = (struct arg_data *) alloca (num_actuals * sizeof (struct arg_data));
  bzero (args, num_actuals * sizeof (struct arg_data));

  args_size.constant = 0;
  args_size.var = 0;
#ifdef FIRST_PARM_CALLER_OFFSET
  args_size.constant = FIRST_PARM_CALLER_OFFSET (funtype);
  stack_count_regparms = 1;
#endif
  starting_args_size = args_size.constant;

  /* In this loop, we consider args in the order they are written.
     We fill up ARGS from the front of from the back if necessary
     so that in any case the first arg to be pushed ends up at the front.  */

#ifdef PUSH_ARGS_REVERSED
  i = num_actuals - 1, inc = -1;
  /* In this case, must reverse order of args
     so that we compute and push the last arg first.  */
#else
  i = 0, inc = 1;
#endif

  INIT_CUMULATIVE_ARGS (args_so_far, funtype);

  /* I counts args in order (to be) pushed; ARGPOS counts in order written.  */
  for (p = actparms, argpos = 0; p; p = TREE_CHAIN (p), i += inc, argpos++)
    {
      tree type = TREE_TYPE (TREE_VALUE (p));
      args[i].tree_value = TREE_VALUE (p);
      args[i].offset = args_size;

      if (type == error_mark_node
	  || TYPE_SIZE (type) == 0)
	continue;

      /* Decide where to pass this arg.  */
      /* args[i].reg is nonzero if all or part is passed in registers.
	 args[i].partial is nonzero if part but not all is passed in registers,
	  and the exact value says how many words are passed in registers.  */

      if (TREE_CODE (TYPE_SIZE (type)) == INTEGER_CST
	  && args_size.var == 0
	  /* error_mark_node here is a flag for the fake argument
	     for a structure value address.  */
	  && TREE_PURPOSE (p) != error_mark_node)
	{
	  args[i].reg = FUNCTION_ARG (args_so_far, TYPE_MODE (type), type,
				      argpos < n_named_args);
	  /* If this argument needs more than the usual parm alignment, do
	     extrinsic padding to reach that alignment.  */

#ifdef MAX_PARM_BOUNDARY
	  /* If MAX_PARM_BOUNDARY is not defined, it means that the usual
	     alignment requirements are relaxed for parms, and that no parm
	     needs more than PARM_BOUNDARY, regardless of data type.  */

	  if (PARM_BOUNDARY < TYPE_ALIGN (type))
	    {
	      int boundary = PARM_BOUNDARY;

	      /* Determine the boundary to pad up to.  */
	      if (TYPE_ALIGN (type) > boundary)
		boundary = TYPE_ALIGN (type);
	      if (boundary > MAX_PARM_BOUNDARY)
		boundary = MAX_PARM_BOUNDARY;

	      /* If the previous args don't reach such a boundary,
		 advance to the next one.  */
	      boundary /= BITS_PER_UNIT;
	      args[i].offset.constant += boundary - 1;
	      args[i].offset.constant &= ~(boundary - 1);
	      args_size.constant += boundary - 1;
	      args_size.constant &= ~(boundary - 1);

	      if (args_size.var != 0)
		abort ();		/* This case not implemented yet */
	    }
#endif /* MAX_PARM_BOUNDARY */

#ifdef FUNCTION_ARG_PARTIAL_NREGS
	  args[i].partial
	    = FUNCTION_ARG_PARTIAL_NREGS (args_so_far,
					  TYPE_MODE (type), type,
					  i < n_named_args);
#endif
	}

      /* Compute the stack-size of this argument.  */

      if (args[i].reg != 0 && args[i].partial == 0
	  && ! stack_count_regparms)
	/* On most machines, don't count stack space for a register arg.  */
	;
      else if (TYPE_MODE (type) != BLKmode)
	{
	  register int size;

	  /* If we are counting "up to zero" and find a stack parm
	     before we reach zero, skip up to zero.
	     Negative offsets correspond to registers.  */
	  if (stack_count_regparms && args_size.constant < 0
	      /* This used to check args[i].partial != 0,
		 but on the Sparc now that seems to be 0.  */
	      && args[i].reg == 0)
	    {
	      args_size.constant = 0;
	      args[i].offset.constant = 0;
	    }
	  size = GET_MODE_SIZE (TYPE_MODE (type));
	  /* Compute how much space the push instruction will push.
	     On many machines, pushing a byte will advance the stack
	     pointer by a halfword.  */
#ifdef PUSH_ROUNDING
	  size = PUSH_ROUNDING (size);
#endif
	  /* Compute how much space the argument should get:
	     maybe pad to a multiple of the alignment for arguments.  */
	  if (none == FUNCTION_ARG_PADDING (TYPE_MODE (type), const0_rtx))
	    args[i].size.constant = size;
	  else
	    args[i].size.constant
	      = (((size + PARM_BOUNDARY / BITS_PER_UNIT - 1)
		  / (PARM_BOUNDARY / BITS_PER_UNIT))
		 * (PARM_BOUNDARY / BITS_PER_UNIT));
	}
      else
	{
	  register tree size = size_in_bytes (type);

	  /* If we are counting "up to zero" and find a stack parm
	     before we reach zero, skip up to zero.
	     Negative offsets correspond to registers.  */
	  if (stack_count_regparms && args_size.constant < 0
	      /* This used to check args[i].partial != 0,
		 but on the Sparc now that seems to be 0.  */
	      && args[i].reg == 0)
	    {
	      args_size.constant = 0;
	      args[i].offset.constant = 0;
	    }

	  /* A nonscalar.  Round its size up to a multiple
	     of PARM_BOUNDARY bits, unless it is not supposed to be padded.  */
	  if (none
	      != FUNCTION_ARG_PADDING (TYPE_MODE (type),
				       expand_expr (size, 0, VOIDmode, 0)))
	    size = convert_units (convert_units (size, BITS_PER_UNIT,
						 PARM_BOUNDARY),
				  PARM_BOUNDARY, BITS_PER_UNIT);
	  ADD_PARM_SIZE (args[i].size, size);

	  /* Certain data types may not be passed in registers
	     (eg C++ classes with constructors).
	     Also, BLKmode parameters initialized from CALL_EXPRs
	     are treated specially, if it is a win to do so.  */
	  if (TREE_CODE (TREE_VALUE (p)) == CALL_EXPR
	      || TREE_ADDRESSABLE (type))
	    {
	      if (TREE_ADDRESSABLE (type))
		BLKmode_parms_forced = 1;
	      /* This is a marker for such a parameter.  */
	      args[i].stack = const0_rtx;
	      BLKmode_parms_sizes += TREE_INT_CST_LOW (size);

	      /* If this parm's location is "below" the nominal stack pointer,
		 note to decrement the stack pointer while it is computed.  */
#ifdef FIRST_PARM_CALLER_OFFSET
	      if (BLKmode_parms_first_offset == 0)
		BLKmode_parms_first_offset
		  /* If parameter's offset is variable, assume the worst.  */
		  = (args[i].offset.var
		     ? FIRST_PARM_CALLER_OFFSET (funtype)
		     : args[i].offset.constant);
#endif
	    }
	}

      /* If a part of the arg was put into registers,
	 don't include that part in the amount pushed.  */
      if (! stack_count_regparms)
	args[i].size.constant
	  -= ((args[i].partial * UNITS_PER_WORD)
	      / (PARM_BOUNDARY / BITS_PER_UNIT)
	      * (PARM_BOUNDARY / BITS_PER_UNIT));

      /* Update ARGS_SIZE, the total stack space for args so far.  */

      args_size.constant += args[i].size.constant;
      if (args[i].size.var)
	{
	  ADD_PARM_SIZE (args_size, args[i].size.var);
	}

      /* Increment ARGS_SO_FAR, which has info about which arg-registers
	 have been used, etc.  */

      FUNCTION_ARG_ADVANCE (args_so_far, TYPE_MODE (type), type,
			    i < n_named_args);
    }

  /* If we would have to push a partially-in-regs parm
     before other stack parms, preallocate stack space instead.  */
  must_preallocate = 0;
  {
    int partial_seen = 0;
    for (i = 0; i < num_actuals; i++)
      {
	if (args[i].partial > 0)
	  partial_seen = 1;
	else if (partial_seen && args[i].reg == 0)
	  must_preallocate = 1;
      }
  }

  /* Precompute all register parameters.  It isn't safe to compute anything
     once we have started filling any specific hard regs.
     If this function call is cse'able, precompute all the parameters.  */

  reg_parm_seen = 0;
  for (i = 0; i < num_actuals; i++)
    if (args[i].reg != 0 || is_const)
      {
	int j;
	int struct_value_lossage = 0;

	/* First, see if this is a precomputed struct-returning function call
	   and other subsequent parms are also such.  */
	if ((TYPE_MODE (TREE_TYPE (args[i].tree_value)) == BLKmode
	     || RETURN_IN_MEMORY (TREE_TYPE (args[i].tree_value)))
	    && TREE_CODE (args[i].tree_value) == CALL_EXPR)
	  for (j = i + 1; j < num_actuals; j++)
	    if ((TYPE_MODE (TREE_TYPE (args[j].tree_value)) == BLKmode
		 || RETURN_IN_MEMORY (TREE_TYPE (args[j].tree_value)))
		&& TREE_CODE (args[j].tree_value) == CALL_EXPR
		&& args[j].reg != 0 || is_const)
	      {
		/* We have two precomputed structure-values call expressions
		   in our parm list.  Both of them would normally use
		   the structure-value block.  To avoid the conflict,
		   compute this parm with a different temporary block.  */
		int size = int_size_in_bytes (TREE_TYPE (args[i].tree_value));
		rtx structval = assign_stack_local (BLKmode, size);
		args[i].value = expand_expr (args[i].tree_value, structval,
					     VOIDmode, 0);
		struct_value_lossage = 1;
		break;
	      }
	if (!struct_value_lossage)
	  args[i].value = expand_expr (args[i].tree_value, 0, VOIDmode, 0);

	if (args[i].reg != 0)
	  reg_parm_seen = 1;

	if (GET_CODE (args[i].value) != MEM
	    && ! CONSTANT_P (args[i].value)
	    && GET_CODE (args[i].value) != CONST_DOUBLE)
	  args[i].value
	    = force_reg (TYPE_MODE (TREE_TYPE (args[i].tree_value)),
			 args[i].value);
	/* ANSI doesn't require a sequence point here,
	   but PCC has one, so this will avoid some problems.  */
	emit_queue ();
      }

  /* Get the function to call, in the form of RTL, if it is a constant.  */
  if (fndecl && is_const)
    {
      /* Get a SYMBOL_REF rtx for the function address.  */
      funexp = XEXP (DECL_RTL (fndecl), 0);

#ifndef NO_FUNCTION_CSE
      /* Pass the address through a pseudoreg, if desired,
	 before the "beginning" of the library call.
	 So this insn isn't "part of" the library call, in case that
	 is deleted, or cse'd.  */
      if (! flag_no_function_cse)
	funexp = copy_to_mode_reg (Pmode, funexp);
#endif
    }

  /* Now we are about to start emitting insns that can be deleted
     if the libcall is deleted.  */
  insn_before = get_last_insn ();

  /* Maybe do additional rounding on the size of the arguments.  */
#ifdef STACK_ARGS_ADJUST
  STACK_ARGS_ADJUST (args_size);
#endif

  /* If we have no actual push instructions, or shouldn't use them,
     or we need a variable amount of space, make space for all args right now.
     Round the needed size up to multiple of STACK_BOUNDARY.  */

  if (args_size.var != 0)
    {
      old_stack_level = copy_to_mode_reg (Pmode, stack_pointer_rtx);
      old_pending_adj = pending_stack_adjust;
      argblock = push_block (round_push (ARGS_SIZE_RTX (args_size)), 0);
    }
  else if (args_size.constant > 0)
    {
      int needed = args_size.constant;

#ifdef STACK_BOUNDARY
      needed = (needed + STACK_BYTES - 1) / STACK_BYTES * STACK_BYTES;
      stack_align = needed - args_size.constant;
#endif
      args_size.constant = needed;

      if (
#ifndef PUSH_ROUNDING
	  1  /* Always preallocate if no push insns.  */
#else
	  must_preallocate || BLKmode_parms_forced
	  || BLKmode_parms_sizes > (args_size.constant >> 1)
#endif
	  )
	{
	  if (inhibit_defer_pop == 0)
	    {
	      /* Try to reuse some or all of the pending_stack_adjust
		 to get this space.  Maybe we can avoid any pushing.  */
	      if (needed > pending_stack_adjust)
		{
		  needed -= pending_stack_adjust;
		  pending_stack_adjust = 0;
		}
	      else
		{
		  pending_stack_adjust -= needed;
		  needed = 0;
		}
	    }
	  argblock = push_block (gen_rtx (CONST_INT, VOIDmode, needed), 0);
	}
    }
#ifndef PUSH_ROUNDING
  else if (BLKmode_parms_forced)
    {
      /* If we have reg-parms that need to be temporarily on the stack,
	 set up an arg block address even though there is no space
	 to be allocated for it.  */
      argblock = push_block (const0_rtx, 0);
    }
#endif

#if 0
  /* If stack needs padding below the args, increase all arg offsets
     so the args are stored above the padding.  */
  if (stack_padding)
    for (i = 0; i < num_actuals; i++)
      args[i].offset.constant += stack_padding;
#endif

  /* Don't try to defer pops if preallocating, not even from the first arg,
     since ARGBLOCK probably refers to the SP.  */
  if (argblock)
    NO_DEFER_POP;

#ifdef STACK_GROWS_DOWNWARD
  /* If any BLKmode parms need to be preallocated in space
     below the nominal stack-pointer address, we need to adjust the
     stack pointer so that this location is temporarily above it.
     This ensures that computation won't clobber that space.  */
  if (BLKmode_parms_first_offset < 0 && argblock != 0)
    {
      int needed = -BLKmode_parms_first_offset;
      argblock = copy_to_reg (argblock);

#ifdef STACK_BOUNDARY
      needed = (needed + STACK_BYTES - 1) / STACK_BYTES * STACK_BYTES;
#endif
      protected_stack = gen_rtx (CONST_INT, VOIDmode, needed);
      anti_adjust_stack (protected_stack);
    }
#endif /* STACK_GROWS_DOWNWARD */

#ifdef PUSH_ARGS_REVERSED
#ifdef STACK_BOUNDARY
  /* If we push args individually in reverse order, perform stack alignment
     before the first push (the last arg).  */
  if (argblock == 0 && stack_align > 0)
    anti_adjust_stack (gen_rtx (CONST_INT, VOIDmode, stack_align));
#endif
#endif

  /* Get the function to call, in the form of RTL.  */
  if (fndecl)
    /* Get a SYMBOL_REF rtx for the function address.  */
    funexp = XEXP (DECL_RTL (fndecl), 0);
  else
    /* Generate an rtx (probably a pseudo-register) for the address.  */
    {
      funexp = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
      emit_queue ();
    }

  /* Now compute and store all non-register parms.
     These come before register parms, since they can require block-moves,
     which could clobber the registers used for register parms.
     Parms which have partial registers are not stored here,
     but we do preallocate space here if they want that.  */

  for (i = 0; i < num_actuals; i++)
    {
      /* Preallocate the stack space for a parm if appropriate
	 so it can be computed directly in the stack space.  */
      if (args[i].stack != 0 && argblock != 0)
	args[i].stack = target_for_arg (TREE_TYPE (args[i].tree_value),
					ARGS_SIZE_RTX (args[i].size),
					argblock, args[i].offset);
      else
	args[i].stack = 0;

      if (args[i].reg == 0
	  && TYPE_SIZE (TREE_TYPE (args[i].tree_value)) != 0)
	store_one_arg (&args[i], argblock, may_be_alloca);
    }

  /* Now store any partially-in-registers parm.
     This is the last place a block-move can happen.  */
  if (reg_parm_seen)
    for (i = 0; i < num_actuals; i++)
      if (args[i].partial != 0)
	store_one_arg (&args[i], argblock, may_be_alloca);

  if (protected_stack != 0)
    adjust_stack (protected_stack);

#ifndef PUSH_ARGS_REVERSED
#ifdef STACK_BOUNDARY
  /* If we pushed args in forward order, perform stack alignment
     after pushing the last arg.  */
  if (argblock == 0 && stack_align > 0)
    anti_adjust_stack (gen_rtx (CONST_INT, VOIDmode, stack_align));
#endif
#endif

  /* If the function will be returning a structure, and if the address in
     which to return the value isn't being passed as a parameter, pass it
     now.  This may result in a register move or in a push; if it's a push,
     we count on the called routine to pop it.  */
  if (structure_value_addr && ! structure_value_addr_parm)
    emit_move_insn (struct_value_rtx,
		    force_reg (Pmode, force_operand (structure_value_addr, 0)));

  /* Now set up any wholly-register parms.  They were computed already.  */
  if (reg_parm_seen)
    for (i = 0; i < num_actuals; i++)
      if (args[i].reg != 0 && args[i].partial == 0)
	store_one_arg (&args[i], argblock, may_be_alloca);

  /* Perform postincrements before actually calling the function.  */
  emit_queue ();

  /* All arguments and registers used for the call must be set up by now!  */

  /* ??? Other languages need a nontrivial second argument (static chain).  */
  funexp = prepare_call_address (funexp, 0);

  /* Mark all register-parms as living through the call.  */
  start_sequence ();
  for (i = 0; i < num_actuals; i++)
    if (args[i].reg != 0)
      {
	if (args[i].partial > 0)
	  use_regs (REGNO (args[i].reg), args[i].partial);
	else if (GET_MODE (args[i].reg) == BLKmode)
	  use_regs (REGNO (args[i].reg),
		    ((int_size_in_bytes (TREE_TYPE (args[i].tree_value))
		      + UNITS_PER_WORD - 1)
		     / UNITS_PER_WORD));
	else
	  emit_insn (gen_rtx (USE, VOIDmode, args[i].reg));
      }

  if (structure_value_addr && ! structure_value_addr_parm
      && GET_CODE (struct_value_rtx) == REG)
    emit_insn (gen_rtx (USE, VOIDmode, struct_value_rtx));

  use_insns = gen_sequence ();
  end_sequence ();

  /* Figure out the register where the value, if any, will come back.  */
  valreg = 0;
  if (TYPE_MODE (TREE_TYPE (exp)) != VOIDmode
      && ! structure_value_addr)
    {
      if (pcc_struct_value)
	valreg = hard_libcall_value (Pmode);
      else
	valreg = hard_function_value (TREE_TYPE (exp), fndecl);
    }

  /* Generate the actual call instruction.  */
  /* This also has the effect of turning off any pop-inhibition
     done in expand_call.  */
  if (args_size.constant < 0)
    args_size.constant = 0;
  emit_call_1 (funexp, funtype, args_size.constant,
	       FUNCTION_ARG (args_so_far, VOIDmode, void_type_node, 1),
	       valreg, old_inhibit_defer_pop, use_insns);

/* ???  Nothing has been done here to record control flow
   when contained functions can do nonlocal gotos.  */

  /* For calls to `setjmp', etc., inform flow.c it should complain
     if nonvolatile values are live.  */

  if (is_setjmp)
    {
      emit_note (IDENTIFIER_POINTER (DECL_NAME (fndecl)), NOTE_INSN_SETJMP);
      current_function_calls_setjmp = 1;
    }

  /* Notice functions that cannot return.
     If optimizing, insns emitted below will be dead.
     If not optimizing, they will exist, which is useful
     if the user uses the `return' command in the debugger.  */

  if (fndecl && TREE_THIS_VOLATILE (fndecl))
    emit_barrier ();

  /* If this call is to be cse'd, then make sure it balances the stack.  */
  if (is_const)
    do_pending_stack_adjust ();

  /* For calls to __builtin_new, note that it can never return 0.
     This is because a new handler will be called, and 0 it not
     among the numbers it is supposed to return.  */
#if 0
  if (is_builtin_new)
    emit_note (IDENTIFIER_POINTER (DECL_NAME (fndecl)), NOTE_INSN_BUILTIN_NEW);
#endif

  /* If value type not void, return an rtx for the value.  */

  /* If there are cleanups to be called, don't use a hard reg as target.  */
  if (cleanups_of_this_call != old_cleanups
      && target && REG_P (target)
      && REGNO (target) < FIRST_PSEUDO_REGISTER)
    target = 0;

  result_copy_insn = 0;

  if (TYPE_MODE (TREE_TYPE (exp)) == VOIDmode
      || ignore)
    {
      target = const0_rtx;
    }
  else if (structure_value_addr)
    {
      if (target == 0 || GET_CODE (target) != MEM)
	target = gen_rtx (MEM, TYPE_MODE (TREE_TYPE (exp)),
			  memory_address (BLKmode, structure_value_addr));
    }
  else if (pcc_struct_value)
    {
      valreg = hard_function_value (build_pointer_type (TREE_TYPE (exp)),
				    fndecl);
      if (target == 0)
	target = gen_rtx (MEM, TYPE_MODE (TREE_TYPE (exp)),
			  copy_to_reg (valreg));
      else if (TYPE_MODE (TREE_TYPE (exp)) != BLKmode)
	result_copy_insn
	  = emit_move_insn (target, gen_rtx (MEM, TYPE_MODE (TREE_TYPE (exp)),
					     copy_to_reg (valreg)));
      else
	emit_block_move (target, gen_rtx (MEM, BLKmode, copy_to_reg (valreg)),
			 expr_size (exp),
			 TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);
    }
  else if (target && GET_MODE (target) == TYPE_MODE (TREE_TYPE (exp)))
    {
      if (!rtx_equal_p (target, valreg))
	result_copy_insn = emit_move_insn (target, valreg);
      else
	/* This tells expand_inline_function to copy valreg to its target.  */
	emit_insn (gen_rtx (USE, VOIDmode, valreg));
    }
  else
    {
      target = copy_to_reg (valreg);
      result_copy_insn = get_last_insn ();
    }

  /* Perform all cleanups needed for the arguments of this call
     (i.e. destructors in C++).  */
  while (cleanups_of_this_call != old_cleanups)
    {
      expand_expr (TREE_VALUE (cleanups_of_this_call), 0, VOIDmode, 0);
      cleanups_of_this_call = TREE_CHAIN (cleanups_of_this_call);
    }

  /* If we pushed this, pop it.  */
  if (saved_structure_value_addr != 0)
    pop_structure_value (saved_structure_value_addr,
			 saved_structure_value_size);

  /* If size of args is variable, restore saved stack-pointer value.  */

  if (old_stack_level)
    {
      emit_move_insn (stack_pointer_rtx, old_stack_level);
      pending_stack_adjust = old_pending_adj;
    }

  /* If call is cse'able, make appropriate pair of reg-notes around it.  */
  if (is_const)
    {
      rtx insn_first = NEXT_INSN (insn_before);
      rtx insn_last = get_last_insn ();
      rtx note = 0;

      /* Don't put the notes on if we don't have insns that can hold them.  */
      if ((GET_CODE (insn_first) == INSN
	   || GET_CODE (insn_first) == CALL_INSN
	   || GET_CODE (insn_first) == JUMP_INSN)
	  && (GET_CODE (insn_last) == INSN
	      || GET_CODE (insn_last) == CALL_INSN
	      || GET_CODE (insn_last) == JUMP_INSN)
	  && insn_last == result_copy_insn)
	{
	  /* Construct an "equal form" for the value
	     which mentions all the arguments in order
	     as well as the function name.  */
	  for (i = 0; i < num_actuals; i++)
	    if (args[i].reg != 0 || is_const)
	      note = gen_rtx (EXPR_LIST, VOIDmode, args[i].value, note);
	  note = gen_rtx (EXPR_LIST, VOIDmode,
			  XEXP (DECL_RTL (fndecl), 0), note);

	  REG_NOTES (insn_last)
	    = gen_rtx (EXPR_LIST, REG_EQUAL, note,
		       gen_rtx (INSN_LIST, REG_RETVAL, insn_first,
				REG_NOTES (insn_last)));
	  REG_NOTES (insn_first)
	    = gen_rtx (INSN_LIST, REG_LIBCALL, insn_last,
		       REG_NOTES (insn_first));
	}
    }

  return target;
}

/* Return an rtx which represents a suitable home on the stack
   given TYPE, the type of the argument looking for a home.
   This is called only for BLKmode arguments.

   SIZE is the size needed for this target.
   ARGS_ADDR is the address of the bottom of the argument block for this call.
   OFFSET describes this parameter's offset into ARGS_ADDR.  It is meaningless
   if this machine uses push insns.  */

static rtx
target_for_arg (type, size, args_addr, offset)
     tree type;
     rtx size;
     rtx args_addr;
     struct args_size offset;
{
  rtx target;
  rtx offset_rtx = ARGS_SIZE_RTX (offset);

  /* We do not call memory_address if possible,
     because we want to address as close to the stack
     as possible.  For non-variable sized arguments,
     this will be stack-pointer relative addressing.  */
  if (GET_CODE (offset_rtx) == CONST_INT)
    target = plus_constant (args_addr, INTVAL (offset_rtx));
  else
    {
      /* I have no idea how to guarantee that this
	 will work in the presence of register parameters.  */
      target = gen_rtx (PLUS, Pmode, args_addr, offset_rtx);
      target = memory_address (QImode, target);
    }

  return gen_rtx (MEM, BLKmode, target);
}

/* Store a single argument for a function call
   into the register or memory area where it must be passed.
   *ARG describes the argument value and where to pass it.
   ARGBLOCK is the address of the stack-block for all the arguments,
   or 0 on a machine where arguemnts are pushed individually.
   MAY_BE_ALLOCA nonzero says this could be a call to `alloca'
   so must be careful about how the stack is used.  */

static void
store_one_arg (arg, argblock, may_be_alloca)
     struct arg_data *arg;
     rtx argblock;
     int may_be_alloca;
{
  register tree pval = arg->tree_value;
  int used = 0;

  if (TREE_CODE (pval) == ERROR_MARK)
    return;

  if (arg->reg != 0 && arg->partial == 0)
    {
      /* Being passed entirely in a register.  */
      if (arg->value != 0)
	{
	  if (GET_MODE (arg->value) == BLKmode)
	    move_block_to_reg (REGNO (arg->reg), arg->value,
			       ((int_size_in_bytes (TREE_TYPE (pval))
				 + UNITS_PER_WORD - 1)
				/ UNITS_PER_WORD));
	  else
	    emit_move_insn (arg->reg, arg->value);
	}
      else
	store_expr (pval, arg->reg, 0);

      /* Don't allow anything left on stack from computation
	 of argument to alloca.  */
      if (may_be_alloca)
	do_pending_stack_adjust ();
    }
  else if (TYPE_MODE (TREE_TYPE (pval)) != BLKmode)
    {
      register int size;
      rtx tem;

      /* Argument is a scalar, not entirely passed in registers.
	 (If part is passed in registers, arg->partial says how much
	 and emit_push_insn will take care of putting it there.)
	 
	 Push it, and if its size is less than the
	 amount of space allocated to it,
	 also bump stack pointer by the additional space.
	 Note that in C the default argument promotions
	 will prevent such mismatches.  */

      used = size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (pval)));
      /* Compute how much space the push instruction will push.
	 On many machines, pushing a byte will advance the stack
	 pointer by a halfword.  */
#ifdef PUSH_ROUNDING
      size = PUSH_ROUNDING (size);
#endif
      /* Compute how much space the argument should get:
	 round up to a multiple of the alignment for arguments.  */
      if (none != FUNCTION_ARG_PADDING (TYPE_MODE (TREE_TYPE (pval)), const0_rtx))
	used = (((size + PARM_BOUNDARY / BITS_PER_UNIT - 1)
		 / (PARM_BOUNDARY / BITS_PER_UNIT))
		* (PARM_BOUNDARY / BITS_PER_UNIT));

      tem = arg->value;
      if (tem == 0)
	{
	  tem = expand_expr (pval, 0, VOIDmode, 0);
	  /* ANSI doesn't require a sequence point here,
	     but PCC has one, so this will avoid some problems.  */
	  emit_queue ();
	}

      /* Don't allow anything left on stack from computation
	 of argument to alloca.  */
      if (may_be_alloca)
	do_pending_stack_adjust ();

      emit_push_insn (tem, TYPE_MODE (TREE_TYPE (pval)), 0, 0,
		      arg->partial, arg->reg, used - size,
		      argblock, ARGS_SIZE_RTX (arg->offset));
    }
  else if (arg->stack != 0)
    {
      /* BLKmode parm, not entirely passed in registers,
	 and with space already allocated.  */

      tree sizetree = size_in_bytes (TREE_TYPE (pval));
      /* Round the size up to multiple of PARM_BOUNDARY bits.  */
      tree s1 = convert_units (sizetree, BITS_PER_UNIT, PARM_BOUNDARY);
      tree s2 = convert_units (s1, PARM_BOUNDARY, BITS_PER_UNIT);

      /* Find out if the parm needs padding, and whether above or below.  */
      enum direction where_pad
	= FUNCTION_ARG_PADDING (TYPE_MODE (TREE_TYPE (pval)),
				expand_expr (sizetree, 0, VOIDmode, 0));

      /* If it is padded below, adjust the stack address
	 upward over the padding.  */

      if (where_pad == downward)
	{
	  rtx offset_rtx;
	  rtx address = XEXP (arg->stack, 0);
	  struct args_size stack_offset;

	  stack_offset.constant = 0;
	  stack_offset.var = 0;

	  /* Compute amount of padding.  */
	  ADD_PARM_SIZE (stack_offset, s2);
	  SUB_PARM_SIZE (stack_offset, sizetree);
	  offset_rtx = ARGS_SIZE_RTX (stack_offset);

	  /* Adjust the address to store at.  */
	  if (GET_CODE (offset_rtx) == CONST_INT)
	    address = plus_constant (address, INTVAL (offset_rtx));
	  else
	    {
	      address = gen_rtx (PLUS, Pmode, address, offset_rtx);
	      address = memory_address (QImode, address);
	    }
	  arg->stack = change_address (arg->stack, VOIDmode, address);
	}

      /* ARG->stack probably refers to the stack-pointer.  If so,
	 stabilize it, in case stack-pointer changes during evaluation.  */
      if (reg_mentioned_p (stack_pointer_rtx, arg->stack))
	arg->stack = change_address (arg->stack, VOIDmode,
				     copy_to_reg (XEXP (arg->stack, 0)));
      /* BLKmode argument that should go in a prespecified stack location.  */
      if (arg->value == 0)
	/* Not yet computed => compute it there.  */
	/* ??? This should be changed to tell expand_expr
	   that it can store directly in the target.  */
	arg->value = store_expr (arg->tree_value, arg->stack, 0);
      else if (arg->value != arg->stack)
	/* It was computed somewhere, but not where we wanted.
	   For example, the value may have come from an official
	   local variable or parameter.  In that case, expand_expr
	   does not fill our suggested target.  */
	emit_block_move (arg->stack, arg->value, ARGS_SIZE_RTX (arg->size),
			 TYPE_ALIGN (TREE_TYPE (pval)) / BITS_PER_UNIT);

      /* Now, if this value wanted to be partly in registers,
	 move the value from the stack to the registers
	 that are supposed to hold the values.  */
      if (arg->partial > 0)
	move_block_to_reg (REGNO (arg->reg), arg->stack, arg->partial);
    }
  else
    {
      /* BLKmode, at least partly to be pushed.  */

      register rtx tem
	= arg->value ? arg->value : expand_expr (pval, 0, VOIDmode, 0);
      register int excess;
      rtx size_rtx;

      /* Pushing a nonscalar.
	 If part is passed in registers, arg->partial says how much
	 and emit_push_insn will take care of putting it there.  */

      /* Round its size up to a multiple
	 of the allocation unit for arguments.  */

      if (arg->size.var != 0)
	{
	  excess = 0;
	  size_rtx = ARGS_SIZE_RTX (arg->size);
	}
      else
	{
	  register tree size = size_in_bytes (TREE_TYPE (pval));
	  /* PUSH_ROUNDING has no effect on us, because
	     emit_push_insn for BLKmode is careful to avoid it.  */
	  excess = (arg->size.constant - TREE_INT_CST_LOW (size)
		    + arg->partial * UNITS_PER_WORD);
	  size_rtx = expand_expr (size, 0, VOIDmode, 0);
	}

      if (arg->stack)
	abort ();

      emit_push_insn (tem, TYPE_MODE (TREE_TYPE (pval)), size_rtx,
		      TYPE_ALIGN (TREE_TYPE (pval)) / BITS_PER_UNIT,
		      arg->partial, arg->reg, excess, argblock,
		      ARGS_SIZE_RTX (arg->offset));
    }

  /* Once we have pushed something, pops can't safely
     be deferred during the rest of the arguments.  */
  NO_DEFER_POP;
}

/* Expand conditional expressions.  */

/* Generate code to evaluate EXP and jump to LABEL if the value is zero.
   LABEL is an rtx of code CODE_LABEL, in this function and all the
   functions here.  */

void
jumpifnot (exp, label)
     tree exp;
     rtx label;
{
  do_jump (exp, label, 0);
}

/* Generate code to evaluate EXP and jump to LABEL if the value is nonzero.  */

void
jumpif (exp, label)
     tree exp;
     rtx label;
{
  do_jump (exp, 0, label);
}

/* Generate code to evaluate EXP and jump to IF_FALSE_LABEL if
   the result is zero, or IF_TRUE_LABEL if the result is one.
   Either of IF_FALSE_LABEL and IF_TRUE_LABEL may be zero,
   meaning fall through in that case.

   This function is responsible for optimizing cases such as
   &&, || and comparison operators in EXP.  */

void
do_jump (exp, if_false_label, if_true_label)
     tree exp;
     rtx if_false_label, if_true_label;
{
  register enum tree_code code = TREE_CODE (exp);
  /* Some cases need to create a label to jump to
     in order to properly fall through.
     These cases set DROP_THROUGH_LABEL nonzero.  */
  rtx drop_through_label = 0;
  rtx temp;
  rtx comparison = 0;

  emit_queue ();

  switch (code)
    {
    case ERROR_MARK:
      break;

    case INTEGER_CST:
      temp = integer_zerop (exp) ? if_false_label : if_true_label;
      if (temp)
	emit_jump (temp);
      break;

    case ADDR_EXPR:
      /* The address of something can never be zero.  */
      if (if_true_label)
	emit_jump (if_true_label);
      break;

    case NOP_EXPR:
      do_jump (TREE_OPERAND (exp, 0), if_false_label, if_true_label);
      break;

    case TRUTH_NOT_EXPR:
      do_jump (TREE_OPERAND (exp, 0), if_true_label, if_false_label);
      break;

    case TRUTH_ANDIF_EXPR:
      if (if_false_label == 0)
	if_false_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), if_false_label, 0);
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case TRUTH_ORIF_EXPR:
      if (if_true_label == 0)
	if_true_label = drop_through_label = gen_label_rtx ();
      do_jump (TREE_OPERAND (exp, 0), 0, if_true_label);
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case COMPOUND_EXPR:
      expand_expr (TREE_OPERAND (exp, 0), const0_rtx, VOIDmode, 0);
      emit_queue ();
      do_jump (TREE_OPERAND (exp, 1), if_false_label, if_true_label);
      break;

    case COND_EXPR:
      {
	register rtx label1 = gen_label_rtx ();
	drop_through_label = gen_label_rtx ();
	do_jump (TREE_OPERAND (exp, 0), label1, 0);
	/* Now the THEN-expression.  */
	do_jump (TREE_OPERAND (exp, 1),
		 if_false_label ? if_false_label : drop_through_label,
		 if_true_label ? if_true_label : drop_through_label);
	emit_label (label1);
	/* Now the ELSE-expression.  */
	do_jump (TREE_OPERAND (exp, 2),
		 if_false_label ? if_false_label : drop_through_label,
		 if_true_label ? if_true_label : drop_through_label);
      }
      break;

    case EQ_EXPR:
      comparison = compare (exp, EQ, EQ, EQ, EQ);
      break;

    case NE_EXPR:
      comparison = compare (exp, NE, NE, NE, NE);
      break;

    case LT_EXPR:
      comparison = compare (exp, LT, LTU, GT, GTU);
      break;

    case LE_EXPR:
      comparison = compare (exp, LE, LEU, GE, GEU);
      break;

    case GT_EXPR:
      comparison = compare (exp, GT, GTU, LT, LTU);
      break;

    case GE_EXPR:
      comparison = compare (exp, GE, GEU, LE, LEU);
      break;

    default:
      temp = expand_expr (exp, 0, VOIDmode, 0);
      /* Copy to register to avoid generating bad insns by cse
	 from (set (mem ...) (arithop))  (set (cc0) (mem ...)).  */
      if (!cse_not_expected && GET_CODE (temp) == MEM)
	temp = copy_to_reg (temp);
      do_pending_stack_adjust ();
      {
	rtx zero = CONST0_RTX (GET_MODE (temp));

	if (GET_CODE (temp) == CONST_INT)
	  comparison = compare_constants (NE, 0,
					  INTVAL (temp), 0, BITS_PER_WORD);
	else if (GET_MODE (temp) != VOIDmode)
	  comparison = compare1 (temp, zero, NE, NE, 0, GET_MODE (temp));
	else
	  abort ();
      }
    }

  /* Do any postincrements in the expression that was tested.  */
  emit_queue ();

  /* If COMPARISON is nonzero here, it is an rtx that can be substituted
     straight into a conditional jump instruction as the jump condition.
     Otherwise, all the work has been done already.  */

  if (comparison == const1_rtx)
    {
      if (if_true_label)
	emit_jump (if_true_label);
    }
  else if (comparison == const0_rtx)
    {
      if (if_false_label)
	emit_jump (if_false_label);
    }
  else if (comparison)
    {
      if (if_true_label)
	{
	  if (bcc_gen_fctn[(int) GET_CODE (comparison)] != 0)
	    emit_jump_insn ((*bcc_gen_fctn[(int) GET_CODE (comparison)]) (if_true_label));
	  else
	    abort ();

	  if (if_false_label)
	    emit_jump (if_false_label);
	}
      else if (if_false_label)
	{
	  rtx pat;

	  if (bcc_gen_fctn[(int) GET_CODE (comparison)] == 0)
	    abort ();

	  pat = (*bcc_gen_fctn[(int) GET_CODE (comparison)]) (if_false_label);
	  /* Now invert the sense of the jump by exchanging the two arms
	     of each IF_THEN_ELSE.  Note that inverting the condition
	     would be incorrect for IEEE floating point with nans!  */
	  if (GET_CODE (pat) == SEQUENCE)
	    {
	      int i;
	      /* We can invert a sequence if the only jump is at the end.  */
	      for (i = 0; i < (int) (XVECLEN (pat, 0) - 1); i++)
		if (GET_CODE (XVECEXP (pat, 0, i)) == JUMP_INSN)
		  abort ();
	      invert_exp (PATTERN (XVECEXP (pat, 0, XVECLEN (pat, 0) - 1)),
			  0, 0);
	    }
	  else
	    invert_exp (pat, 0, 0);

	  emit_jump_insn (pat);
	}
    }

  if (drop_through_label)
    emit_label (drop_through_label);
}

/* Compare two integer constant rtx's, OP0 and OP1.
   The comparison operation is OPERATION.
   Return an rtx representing the value 1 or 0.
   WIDTH is the width in bits that is significant.  */

static rtx
compare_constants (operation, unsignedp, op0, op1, width)
     enum rtx_code operation;
     int unsignedp;
     int op0, op1;
     int width;
{
  int val;

  /* Sign-extend or zero-extend the operands to a full word
     from an initial width of WIDTH bits.  */
  if (width < HOST_BITS_PER_INT)
    {
      op0 &= (1 << width) - 1;
      op1 &= (1 << width) - 1;

      if (! unsignedp)
	{
	  if (op0 & (1 << (width - 1)))
	    op0 |= ((-1) << width);
	  if (op1 & (1 << (width - 1)))
	    op1 |= ((-1) << width);
	}
    }

  switch (operation)
    {
    case EQ:
      val = op0 == op1;
      break;

    case NE:
      val = op0 != op1;
      break;

    case GT:
    case GTU:
      val = op0 > op1;
      break;

    case LT:
    case LTU:
      val = op0 < op1;
      break;

    case GE:
    case GEU:
      val = op0 >= op1;
      break;

    case LE:
    case LEU:
      val = op0 <= op1;
    }

  return val ? const1_rtx : const0_rtx;
}

/* Generate code for a comparison expression EXP
   (including code to compute the values to be compared)
   and set (CC0) according to the result.
   SIGNED_FORWARD should be the rtx operation for this comparison for
   signed data; UNSIGNED_FORWARD, likewise for use if data is unsigned.
   SIGNED_REVERSE and UNSIGNED_REVERSE are used if it is desirable
   to interchange the operands for the compare instruction.

   We force a stack adjustment unless there are currently
   things pushed on the stack that aren't yet used.  */

static rtx
compare (exp, signed_forward, unsigned_forward,
	 signed_reverse, unsigned_reverse)
     register tree exp;
     enum rtx_code signed_forward, unsigned_forward;
     enum rtx_code signed_reverse, unsigned_reverse;
{

  register rtx op0 = expand_expr (TREE_OPERAND (exp, 0), 0, VOIDmode, 0);
  register rtx op1 = expand_expr (TREE_OPERAND (exp, 1), 0, VOIDmode, 0);
  register enum machine_mode mode = GET_MODE (op0);
  int unsignedp;

  /* If one operand is 0, make it the second one.  */

  if (op0 == const0_rtx
      || (GET_MODE_CLASS (mode) == MODE_FLOAT && op0 == CONST0_RTX (mode)))
    {
      rtx tem = op0;
      op0 = op1;
      op1 = tem;
      signed_forward = signed_reverse;
      unsigned_forward = unsigned_reverse;
    }

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  unsignedp = (TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 0)))
	       || TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (exp, 1))));

  if (GET_CODE (op0) == CONST_INT && GET_CODE (op1) == CONST_INT)
    return compare_constants (signed_forward, unsignedp,
			      INTVAL (op0), INTVAL (op1),
			      GET_MODE_BITSIZE (TYPE_MODE (TREE_TYPE (TREE_OPERAND (exp, 0)))));

  emit_cmp_insn (op0, op1,
		 (mode == BLKmode) ? expr_size (TREE_OPERAND (exp, 0)) : 0,
		 unsignedp,
		 TYPE_ALIGN (TREE_TYPE (exp)) / BITS_PER_UNIT);

  return gen_rtx ((unsignedp ? unsigned_forward : signed_forward),
		  VOIDmode, cc0_rtx, const0_rtx);
}

/* Like compare but expects the values to compare as two rtx's.
   The decision as to signed or unsigned comparison must be made by the caller.
   BLKmode is not allowed.  */

static rtx
compare1 (op0, op1, forward_op, reverse_op, unsignedp, mode)
     register rtx op0, op1;
     enum rtx_code forward_op, reverse_op;
     int unsignedp;
     enum machine_mode mode;
{
  /* If one operand is 0, make it the second one.  */

  if (op0 == const0_rtx
      || (GET_MODE_CLASS (mode) == MODE_FLOAT && op0 == CONST0_RTX (mode)))
    {
      rtx tem = op0;
      op0 = op1;
      op1 = tem;
      forward_op = reverse_op;
    }

  if (flag_force_mem)
    {
      op0 = force_not_mem (op0);
      op1 = force_not_mem (op1);
    }

  do_pending_stack_adjust ();

  if (GET_CODE (op0) == CONST_INT && GET_CODE (op1) == CONST_INT)
    return compare_constants (forward_op, unsignedp,
			      INTVAL (op0), INTVAL (op1),
			      GET_MODE_BITSIZE (mode));

  emit_cmp_insn (op0, op1, 0, unsignedp, 0);

  return gen_rtx (forward_op, VOIDmode, cc0_rtx, const0_rtx);
}

/* Generate code to calculate EXP using a store-flag instruction
   and return an rtx for the result.
   If TARGET is nonzero, store the result there if convenient.

   Return zero if there is no suitable set-flag instruction
   available on this machine.  */

static rtx
do_store_flag (exp, target, mode)
     tree exp;
     rtx target;
     enum machine_mode mode;
{
  register enum tree_code code = TREE_CODE (exp);
  register rtx comparison = 0;
  enum machine_mode compare_mode;
  rtx prev_insn = get_last_insn ();
  enum insn_code icode;

  switch (code)
    {
#ifdef HAVE_seq
    case EQ_EXPR:
      if (HAVE_seq)
	{
	  comparison = compare (exp, EQ, EQ, EQ, EQ);
	  icode = CODE_FOR_seq;
	  compare_mode = insn_operand_mode[(int) CODE_FOR_seq][0];
	}
      break;
#endif

#ifdef HAVE_sne
    case NE_EXPR:
      if (HAVE_sne)
	{
	  comparison = compare (exp, NE, NE, NE, NE);
	  icode = CODE_FOR_sne;
	  compare_mode = insn_operand_mode[(int) CODE_FOR_sne][0];
	}
      break;
#endif

#if defined (HAVE_slt) && defined (HAVE_sltu) && defined (HAVE_sgt) && defined (HAVE_sgtu)
    case LT_EXPR:
      if (HAVE_slt && HAVE_sltu && HAVE_sgt && HAVE_sgtu)
	{
	  comparison = compare (exp, LT, LTU, GT, GTU);
	  icode = CODE_FOR_slt;
	  compare_mode = insn_operand_mode[(int) CODE_FOR_slt][0];
	}
      break;

    case GT_EXPR:
      if (HAVE_slt && HAVE_sltu && HAVE_sgt && HAVE_sgtu)
	{
	  comparison = compare (exp, GT, GTU, LT, LTU);
	  icode = CODE_FOR_slt;
	  compare_mode = insn_operand_mode[(int) CODE_FOR_slt][0];
	}
      break;
#endif

#if defined (HAVE_sle) && defined (HAVE_sleu) && defined (HAVE_sge) && defined (HAVE_sgeu)
    case LE_EXPR:
      if (HAVE_sle && HAVE_sleu && HAVE_sge && HAVE_sgeu)
	{
	  comparison = compare (exp, LE, LEU, GE, GEU);
	  icode = CODE_FOR_sle;
	  compare_mode = insn_operand_mode[(int) CODE_FOR_sle][0];
	}
      break;

    case GE_EXPR:
      if (HAVE_sle && HAVE_sleu && HAVE_sge && HAVE_sgeu)
	{
	  comparison = compare (exp, GE, GEU, LE, LEU);
	  icode = CODE_FOR_sle;
	  compare_mode = insn_operand_mode[(int) CODE_FOR_sle][0];
	}
      break;
#endif
    }
  if (comparison == 0)
    return 0;

  if (target == 0 || GET_MODE (target) != mode
      /* Don't use specified target unless the insn can handle it.  */
      || ! (*insn_operand_predicate[(int) icode][0]) (target, mode)
      /* When modes don't match, don't use specified target,
	 because it might be the same as an operand,
	 and then the CLOBBER output below would screw up.  */
      || (mode != compare_mode && GET_CODE (comparison) != CONST_INT))
    target = gen_reg_rtx (mode);

  /* Store the comparison in its proper mode.  */
  if (GET_CODE (comparison) == CONST_INT)
    emit_move_insn (target, comparison);
  else if (GET_MODE (target) != compare_mode)
    {
      /* We want a different mode: store result in its natural mode.
	 Combine the mode conversion with the truncation we must do anyway.  */
      /* Put a CLOBBER before the compare, so we don't come between
	 the compare and the insn that uses the result.  */
      emit_insn_after (gen_rtx (CLOBBER, VOIDmode, target), prev_insn);
      emit_insn ((*setcc_gen_fctn[(int) GET_CODE (comparison)])
		 (gen_rtx (SUBREG, compare_mode, target, 0)));
      /* If the desired mode is wider than what we got,
	 use an AND to convert it, but not if we will do one anyway.  */
#if STORE_FLAG_VALUE == 1
      if (GET_MODE_SIZE (mode) > GET_MODE_SIZE (compare_mode))
	expand_bit_and (mode, target, const1_rtx, target);
#endif
    }
  else
    emit_insn ((*setcc_gen_fctn[(int) GET_CODE (comparison)]) (target));

#if STORE_FLAG_VALUE != 1
#if STORE_FLAG_VALUE & 1
  expand_bit_and (mode, target, const1_rtx, target);
#else
  expand_shift (RSHIFT_EXPR, mode, target,
		build_int_2 (GET_MODE_BITSIZE (mode) - 1, 0),
		target, TRUE);
#endif
#endif
  return target;
}

/* Generate a tablejump instruction (used for switch statements).  */

#ifdef HAVE_tablejump

/* INDEX is the value being switched on, with the lowest value
   in the table already subtracted.
   RANGE is the length of the jump table.
   TABLE_LABEL is a CODE_LABEL rtx for the table itself.

   DEFAULT_LABEL is a CODE_LABEL rtx to jump to if the
   index value is out of range.  */

void
do_tablejump (index, range, table_label, default_label)
     rtx index, range, table_label, default_label;
{
  register rtx temp;

  emit_cmp_insn (range, index, 0, 0, 0);
  emit_jump_insn (gen_bltu (default_label));
  /* If flag_force_addr were to affect this address
     it could interfere with the tricky assumptions made
     about addresses that contain label-refs,
     which may be valid only very near the tablejump itself.  */
  index = memory_address_noforce
    (CASE_VECTOR_MODE,
     gen_rtx (PLUS, Pmode,
	      gen_rtx (MULT, Pmode, index,
		       gen_rtx (CONST_INT, VOIDmode,
				GET_MODE_SIZE (CASE_VECTOR_MODE))),
	      gen_rtx (LABEL_REF, VOIDmode, table_label)));
  temp = gen_reg_rtx (CASE_VECTOR_MODE);
  convert_move (temp, gen_rtx (MEM, CASE_VECTOR_MODE, index), 0);

  emit_jump_insn (gen_tablejump (temp, table_label));
}

#endif /* HAVE_tablejump */
