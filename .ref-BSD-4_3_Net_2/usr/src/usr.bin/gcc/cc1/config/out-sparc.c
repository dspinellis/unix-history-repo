/* Subroutines for insn-output.c for Sun SPARC.
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.
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

/* Global variables for machine-dependend things.  */

/* This should go away if we pass floats to regs via
   the stack instead of the frame, and if we learn how
   to renumber all the registers when we don't do a save (hard!).  */
extern int frame_pointer_needed;

static rtx find_addr_reg ();

rtx next_real_insn_no_labels ();

/* Return non-zero only if OP is a register of mode MODE,
   or const0_rtx.  */
int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == const0_rtx || register_operand (op, mode));
}

/* Return non-zero if INSN is a conditional insn with a predicate
   valid after an addcc or subcc instruction.  */

int
ignore_overflow_conditional_p (insn)
     rtx insn;
{
  rtx x = SET_SRC (PATTERN (insn));
  RTX_CODE code;
  if (GET_CODE (x) == IF_THEN_ELSE)
    x = XEXP (x, 0);
  code = GET_CODE (x);
  return code == EQ || code == NE || code == GE || code == LT;
}

/* Return non-zero if this pattern, can be evaluated safely, even if it
   was not asked for.  */
int
safe_insn_src_p (op, mode)
     rtx op;
     enum machine_mode mode;
{
  /* Just experimenting.  */

  /* No floating point src is safe if it contains an arithmetic
     operation, since that operation may trap.  */
  switch (GET_CODE (op))
    {
    case CONST_INT:
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST:
      return 1;

    case REG:
      return 1;

    case MEM:
      return CONSTANT_ADDRESS_P (XEXP (op, 0));

      /* We never need to negate or complement constants.  */
    case NEG:
      return (mode != SFmode && mode != DFmode);
    case NOT:
      return 1;

    case COMPARE:
    case MINUS:
    case PLUS:
      return (mode != SFmode && mode != DFmode);
    case AND:
    case IOR:
    case XOR:
    case LSHIFT:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if ((GET_CODE (XEXP (op, 0)) == CONST_INT && ! SMALL_INT (XEXP (op, 0)))
	  || (GET_CODE (XEXP (op, 1)) == CONST_INT && ! SMALL_INT (XEXP (op, 1))))
	return 0;
      return 1;

    default:
      return 0;
    }
}

/* Return 1 if REG is clobbered in IN.
   Return 0 if REG is used in IN (other than being clobbered).
   Return 2 if REG does not appear in IN.  */

static int
reg_clobbered_p (reg, in)
     rtx reg;
     rtx in;
{
  register char *fmt;
  register int i, result = 0;

  register enum rtx_code code;

  if (in == 0)
    return 2;

  code = GET_CODE (in);

  switch (code)
    {
      /* Let these fail out quickly.  */
    case CONST_INT:
    case SYMBOL_REF:
    case CONST:
      return 2;

    case SUBREG:
      if (SUBREG_WORD (in) != 0)
	in = gen_rtx (REG, SImode, REGNO (SUBREG_REG (in)) + SUBREG_WORD (in));
      else
	in = SUBREG_REG (in);

    case REG:
      if (in == reg
	  || refers_to_regno_p (REGNO (reg),
				REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				in, 0))
	return 0;
      return 2;

    case SET:
      if (SET_SRC (in) == reg
	  || refers_to_regno_p (REGNO (reg),
				REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				SET_SRC (in), 0))
	return 0;

      if (SET_DEST (in) == reg)
	return 1;

      if (refers_to_regno_p (REGNO (reg),
			     REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
			     SET_DEST (in), 0))
	if (GET_CODE (SET_DEST (in)) == REG
	    || GET_CODE (SET_DEST (in)) == SUBREG)
	  return 1;
	else
	  return 0;
      return 2;

    case USE:
      if (XEXP (in, 0) == reg
	  || refers_to_regno_p (REGNO (reg),
				REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				XEXP (in, 0), 0))
	return 0;
      return 2;

    case CLOBBER:
      if (XEXP (in, 0) == reg)
	return 1;
      /* If the CLOBBER expression is a SUBREG, accept that as a
	 clobber.  But if it is some expression based on this register,
	 that is like a USE as far as this register is concerned,
	 so we won't take it.  */
      if (refers_to_regno_p (REGNO (reg),
			     REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
			     XEXP (in, 0), 0))
	if (GET_CODE (XEXP (in, 0)) == REG
	    || GET_CODE (XEXP (in, 0)) == SUBREG)
	  return 1;
	else
	  return 0;
      return 2;
    }

  fmt = GET_RTX_FORMAT (code);

  result = 2;

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (in, i) - 1; j >= 0; j--)
	    switch (reg_clobbered_p (reg, XVECEXP (in, i, j)))
	      {
	      case 0:
		return 0;
	      case 2:
		continue;
	      case 1:
		result = 1;
		break;
	      }
	}
      else if (fmt[i] == 'e')
	switch (reg_clobbered_p (reg, XEXP (in, i)))
	  {
	  case 0:
	    return 0;
	  case 2:
	    continue;
	  case 1:
	    result = 1;
	    break;
	  }
    }
  return result;
}

/* Return non-zero if OP can be written to without screwing up
   GCC's model of what's going on.  It is assumed that this operand
   appears in the dest position of a SET insn in a conditional
   branch's delay slot.  AFTER is the label to start looking from.  */
int
operand_clobbered_before_used_after (op, after)
     rtx op;
     rtx after;
{
  extern char call_used_regs[];

  /* Just experimenting.  */
  if (GET_CODE (op) == CC0)
    return 1;
  if (GET_CODE (op) == REG)
    {
      rtx insn;

      if (op == stack_pointer_rtx)
	return 0;

      for (insn = NEXT_INSN (after); insn; insn = NEXT_INSN (insn))
	{
	  if (GET_CODE (insn) == NOTE)
	    continue;
	  if (GET_CODE (insn) == INSN
	      || GET_CODE (insn) == JUMP_INSN
	      || GET_CODE (insn) == CALL_INSN)
	    {
	      switch (reg_clobbered_p (op, PATTERN (insn)))
		{
		case 0:
		  return 0;
		case 2:
		  break;
		case 1:
		  return 1;
		}
	      if (dead_or_set_p (insn, op))
		return 1;
	    }
	  else if (GET_CODE (insn) == CODE_LABEL)
	    return 0;
	  if (GET_CODE (insn) == JUMP_INSN)
	    {
	      if (condjump_p (insn))
		return 0;
	      /* This is a jump insn which has already
		 been mangled.  We can't tell what it does.  */
	      if (GET_CODE (PATTERN (insn)) == PARALLEL)
		return 0;
	      if (! JUMP_LABEL (insn))
		return 0;
	      /* Keep following jumps.  */
	      insn = JUMP_LABEL (insn);
	    }
	}
      return 1;
    }

  /* In both of these cases, the first insn executed
     for this op will be a sethi %hi(whatever),%g1,
     which is tolerable.  */
  if (GET_CODE (op) == MEM)
    return (CONSTANT_ADDRESS_P (XEXP (op, 0)));

  return 0;
}

/* Return non-zero if this pattern, as a source to a "SET",
   is known to yield an instruction of unit size.  */
int
single_insn_src_p (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case CONST_INT:
#if 1
      /* This is not always a single insn src, technically,
	 but output_delayed_branch knows how to deal with it.  */
      return 1;
#else
      if (SMALL_INT (op))
	return 1;
      /* We can put this set insn into delay slot, because this is one
	 insn; 'sethi'.  */
      if ((INTVAL (op) & 0x3ff) == 0)
	return 1;

      /* This is not a single insn src, technically,
	 but output_delayed_branch knows how to deal with it.  */
      return 1;
#endif

#if 1
    case SYMBOL_REF:
      /* This is not a single insn src, technically,
	 but output_delayed_branch knows how to deal with it.  */
      return 1;
#else
      return 0;
#endif

    case REG:
      return 1;

    case MEM:
#if 0
      /* This is not a single insn src, technically,
	 but output_delayed_branch knows how to deal with it.  */
      if (GET_CODE (XEXP (op, 0)) == SYMBOL_REF)
	return 0;
#endif
      return 1;

      /* We never need to negate or complement constants.  */
    case NEG:
      return (mode != DFmode);
    case NOT:
      return 1;

    case COMPARE:
    case MINUS:
      /* If the target is cc0, then these insns will take
	 two insns (one being a nop).  */
      return (mode != SFmode && mode != DFmode);
    case PLUS:
    case AND:
    case IOR:
    case XOR:
    case LSHIFT:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if ((GET_CODE (XEXP (op, 0)) == CONST_INT && ! SMALL_INT (XEXP (op, 0)))
	  || (GET_CODE (XEXP (op, 1)) == CONST_INT && ! SMALL_INT (XEXP (op, 1))))
	return 0;
      return 1;

    case SUBREG:
      if (SUBREG_WORD (op) != 0)
	return 0;
      return single_insn_src_p (SUBREG_REG (op), mode);

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      /* Lazy... could check for more cases.  */
      if (GET_CODE (XEXP (op, 0)) == MEM
	  && ! CONSTANT_ADDRESS_P (XEXP (XEXP (op, 0), 0)))
	return 1;
      return 0;

      /* Not doing floating point, since they probably
	 take longer than the branch slot they might fill.  */
    case FLOAT_EXTEND:
    case FLOAT_TRUNCATE:
    case FLOAT:
    case FIX:
    case UNSIGNED_FLOAT:
    case UNSIGNED_FIX:
      return 0;

    default:
      return 0;
    }
}

/* This extra test must be done to verify that a move insn
   really is just one assembler insn.  */

int
single_insn_extra_test (dest, src)
     rtx dest, src;
{
  /* Moves between FP regs and CPU regs are two insns.  */
  return (!(GET_CODE (src) == REG
	    && GET_CODE (dest) == REG
	    && (FP_REG_P (src) != FP_REG_P (dest))));
}

/* Nonzero only if this *really* is a single insn operand.  */
int
strict_single_insn_op_p (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);

  switch (GET_CODE (op))
    {
    case CC0:
      return 1;

    case CONST_INT:
      if (SMALL_INT (op))
	return 1;
      /* We can put this set insn into delay slot, because this is one
	 insn; 'sethi'.  */
      if ((INTVAL (op) & 0x3ff) == 0)
	return 1;
      return 0;

    case SYMBOL_REF:
      return 0;

    case REG:
      return (mode != DFmode && mode != DImode);

    case MEM:
      if (! CONSTANT_ADDRESS_P (XEXP (op, 0)))
	return (mode != DFmode && mode != DImode);
      return 0;

      /* We never need to negate or complement constants.  */
    case NEG:
      return (mode != DFmode);
    case NOT:
      return 1;

    case COMPARE:
    case MINUS:
      /* If the target is cc0, then these insns will take
	 two insns (one being a nop).  */
      return (mode != SFmode && mode != DFmode);
    case PLUS:
    case AND:
    case IOR:
    case XOR:
    case LSHIFT:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      if ((GET_CODE (XEXP (op, 0)) == CONST_INT && ! SMALL_INT (XEXP (op, 0)))
	  || (GET_CODE (XEXP (op, 1)) == CONST_INT && ! SMALL_INT (XEXP (op, 1))))
	return 0;
      return 1;

    case SUBREG:
      if (SUBREG_WORD (op) != 0)
	return 0;
      return strict_single_insn_op_p (SUBREG_REG (op), mode);

    case SIGN_EXTEND:
    case ZERO_EXTEND:
      if (GET_CODE (XEXP (op, 0)) == MEM
	  && ! CONSTANT_ADDRESS_P (XEXP (XEXP (op, 0), 0)))
	return 1;
      return 0;

      /* Not doing floating point, since they probably
	 take longer than the branch slot they might fill.  */
    case FLOAT_EXTEND:
    case FLOAT_TRUNCATE:
    case FLOAT:
    case FIX:
    case UNSIGNED_FLOAT:
    case UNSIGNED_FIX:
      return 0;

    default:
      return 0;
    }
}

/* Return truth value of whether OP is a relational operator.  */
int
relop (op, mode)
     rtx op;
     enum machine_mode mode;
{
  switch (GET_CODE (op))
    {
    case EQ:
    case NE:
    case GT:
    case GE:
    case LT:
    case LE:
    case GTU:
    case GEU:
    case LTU:
    case LEU:
      return 1;
    }
  return 0;
}

/* Return truth value of wheterh OP is EQ or NE.  */
int
eq_or_neq (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == EQ || GET_CODE (op) == NE);
}

/* Return truth value of whether OP can be used as an operands in a three
   address arithmetic insn (such as add %o1,7,%l2) of mode MODE.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op)));
}

/* Return truth value of whether OP can be used as an operand in a two
   address arithmetic insn (such as set 123456,%o4) of mode MODE.  */

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) || GET_CODE (op) == CONST_INT);
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in three-address insns.  */

int
small_int (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[0]) == MEM)
    {
      if (GET_CODE (operands[1]) != MEM)
	if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	  {
	    if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		   && cc_prev_status.mdep == XEXP (operands[0], 0)))
	      output_asm_insn ("sethi %%hi(%m0),%%g1", operands);
	    cc_status.flags |= CC_KNOW_HI_G1;
	    cc_status.mdep = XEXP (operands[0], 0);
	    return "st %1,[%%lo(%m0)+%%g1]";
	  }
	else
	  return "st %r1,%0";
      else
	{
	  rtx xoperands[2];

	  cc_status.flags &= ~CC_F0_IS_0;
	  xoperands[0] = gen_rtx (REG, SFmode, 32);
	  xoperands[1] = operands[1];
	  output_asm_insn (singlemove_string (xoperands), xoperands);
	  xoperands[1] = xoperands[0];
	  xoperands[0] = operands[0];
	  output_asm_insn (singlemove_string (xoperands), xoperands);
	  return "";
	}
    }
  if (GET_CODE (operands[1]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		 && cc_prev_status.mdep == XEXP (operands[1], 0)))
	    output_asm_insn ("sethi %%hi(%m1),%%g1", operands);
	  cc_status.flags |= CC_KNOW_HI_G1;
	  cc_status.mdep = XEXP (operands[1], 0);
	  return "ld [%%lo(%m1)+%%g1],%0";
	}
      return "ld %1,%0";
    }
  return "mov %1,%0";
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum { REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (operands[0]) == MEM)
    optype0 = MEMOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_P (operands[1])
	   || GET_CODE (operands[1]) == CONST_DOUBLE)
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

  /* If an operand is an unoffsettable memory ref, find a register
     we can increment temporarily to make it refer to the second word.  */

  if (optype0 == MEMOP)
    addreg0 = find_addr_reg (XEXP (operands[0], 0));

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (XEXP (operands[1], 0));

  /* Ok, we can do one word at a time.
     Normally we do the low-numbered word first,
     but if either operand is autodecrementing then we
     do the high-numbered word first.

     In either case, set up in LATEHALF the operands to use
     for the high-numbered word and in some cases alter the
     operands in OPERANDS to be suitable for the low-numbered word.  */

  if (optype0 == REGOP)
    latehalf[0] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
  else if (optype0 == OFFSOP)
    latehalf[0] = adj_offsettable_operand (operands[0], 4);
  else
    latehalf[0] = operands[0];

  if (optype1 == REGOP)
    latehalf[1] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
  else if (optype1 == OFFSOP)
    latehalf[1] = adj_offsettable_operand (operands[1], 4);
  else if (optype1 == CNSTOP)
    {
      if (CONSTANT_P (operands[1]))
	latehalf[1] = const0_rtx;
      else if (GET_CODE (operands[1]) == CONST_DOUBLE)
	{
	  latehalf[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_HIGH (operands[1]));
	  operands[1] = gen_rtx (CONST_INT, VOIDmode,
				 CONST_DOUBLE_LOW (operands[1]));
	}
    }
  else
    latehalf[1] = operands[1];

  /* If the first move would clobber the source of the second one,
     do them in the other order.

     RMS says "This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance."

     but it happens on the sparc when loading parameter registers,
     so I am going to define that circumstance, and make it work
     as expected.  */

  /* Easy case: try moving both words at once.  */
  /* First check for moving between an even/odd register pair
     and a memory location.  */
  if ((optype0 == REGOP && optype1 != REGOP && optype1 != CNSTOP
       && (REGNO (operands[0]) & 1) == 0)
      || (optype0 != REGOP && optype1 != CNSTOP && optype1 == REGOP
	  && (REGNO (operands[1]) & 1) == 0))
    {
      rtx op1, op2;
      rtx base = 0, offset = const0_rtx;

      /* OP1 gets the register pair, and OP2 gets the memory address.  */
      if (optype0 == REGOP)
	op1 = operands[0], op2 = XEXP (operands[1], 0);
      else
	op1 = operands[1], op2 = XEXP (operands[0], 0);

      /* Now see if we can trust the address to be 8-byte aligned.  */
      /* Trust global variables.  */
      if (CONSTANT_ADDRESS_P (op2))
	{
	  operands[0] = op1;
	  operands[1] = op2;
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		 && cc_prev_status.mdep == op2))
	    output_asm_insn ("sethi %%hi(%1),%%g1", operands);
	  cc_status.flags |= CC_KNOW_HI_G1;
	  cc_status.mdep = op2;
	  if (op1 == operands[0])
	    return "ldd [%%lo(%1)+%%g1],%0";
	  else
	    return "std [%%lo(%1)+%%g1],%0";
	}

      if (GET_CODE (op2) == PLUS)
	{
	  if (GET_CODE (XEXP (op2, 0)) == REG)
	    base = XEXP (op2, 0), offset = XEXP (op2, 1);
	  else if (GET_CODE (XEXP (op2, 1)) == REG)
	    base = XEXP (op2, 1), offset = XEXP (op2, 0);
	}

      /* Trust round enough offsets from the stack or frame pointer.  */
      if (base
	  && (REGNO (base) == FRAME_POINTER_REGNUM
	      || REGNO (base) == STACK_POINTER_REGNUM))
	{
	  if (GET_CODE (offset) == CONST_INT
	      && (INTVAL (offset) & 0x7) == 0)
	    {
	      if (op1 == operands[0])
		return "ldd %1,%0";
	      else
		return "std %1,%0";
	    }
	}
      else
	{
	  /* We know structs not on the stack are properly aligned.
	     Since a double asks for 8-byte alignment,
	     we know it must have got that if it is in a struct.
	     But a DImode need not be 8-byte aligned, because it could be a
	     struct containing two ints or pointers.  */

	  /* Sun fucks us here.  We cannot trust references
	     to doubles via varying addresses.  It might be on the stack
	     even if we don't know that it is; and then it might not be
	     double-word aligned.  */
#if 0
	  if (GET_CODE (operands[1]) == MEM && GET_MODE (operands[1]) == DFmode
	      && MEM_IN_STRUCT_P (operands[1]))
	    return "ldd %1,%0";
	  else if (GET_CODE (operands[0]) == MEM
		   && GET_MODE (operands[0]) == DFmode
		   && MEM_IN_STRUCT_P (operands[0]))
	    return "std %1,%0";
#endif
	}
    }

  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (latehalf[1]))
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	output_asm_insn ("add %0,0x4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add %0,0x4,%0", &addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("add %0,-0x4,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("add %0,-0x4,%0", &addreg0);

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }
  else if (optype0 == REGOP && optype1 != REGOP
	   && reg_overlap_mentioned_p (operands[0], operands[1]))
    {
      /* Do the late half first.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);
      /* Then clobber.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("add %0,0x4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,0x4,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("add %0,-0x4,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("add %0,-0x4,%0", &addreg1);

  return "";
}

static char *
output_fp_move_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))
	{
	  output_asm_insn ("fmovs %1,%0", operands);
	  operands[0] = gen_rtx (REG, VOIDmode, REGNO (operands[0]) + 1);
	  operands[1] = gen_rtx (REG, VOIDmode, REGNO (operands[1]) + 1);
	  return "fmovs %1,%0";
	}
      if (GET_CODE (operands[1]) == REG)
	{
	  if ((REGNO (operands[1]) & 1) == 0)
	    return "std %1,[%%fp-8]\n\tldd [%%fp-8],%0";
	  else
	    {
	      rtx xoperands[3];
	      xoperands[0] = operands[0];
	      xoperands[1] = operands[1];
	      xoperands[2] = gen_rtx (REG, SImode, REGNO (operands[1]) + 1);
	      output_asm_insn ("st %2,[%%fp-4]\n\tst %1,[%%fp-8]\n\tldd [%%fp-8],%0", xoperands);
	      return "";
	    }
	}
      /* Use ldd if known to be aligned.  */
      if (GET_CODE (XEXP (operands[1], 0)) == PLUS
	  && (((XEXP (XEXP (operands[1], 0), 0) == frame_pointer_rtx
		|| XEXP (XEXP (operands[1], 0), 0) == stack_pointer_rtx)
	       && GET_CODE (XEXP (XEXP (operands[1], 0), 1)) == CONST_INT
	       && (INTVAL (XEXP (XEXP (operands[1], 0), 1)) & 0x7) == 0)
#if 0 /* An array in a structure that is a parm need not be aligned!  */
	      /* Arrays are known to be aligned,
		 and reg+reg addresses are used (on this machine)
		 only for array accesses.  */
	      || (REG_P (XEXP (XEXP (operands[1], 0), 0))
		  && REG_P (XEXP (XEXP (operands[1], 0), 1)))
#endif
	      ))
	return "ldd %1,%0";
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		 && cc_prev_status.mdep == XEXP (operands[1], 0)))
	    output_asm_insn ("sethi %%hi(%m1),%%g1", operands);
	  cc_status.flags |= CC_KNOW_HI_G1;
	  cc_status.mdep = XEXP (operands[1], 0);
	  return "ldd [%%lo(%m1)+%%g1],%0";
	}
      /* Otherwise use two ld insns.  */
      {
	rtx xoperands[2];
	output_asm_insn ("ld %1,%0", operands);
	xoperands[0] = gen_rtx (REG, GET_MODE (operands[0]),
				REGNO (operands[0]) + 1);
	if (GET_CODE (XEXP (operands[1], 0)) == PLUS
	    && offsettable_address_p (1, GET_MODE (operands[1]),
				      XEXP (operands[1], 0)))
	  {
	    xoperands[1] = adj_offsettable_operand (operands[1], 4);
	    output_asm_insn ("ld %1,%0", xoperands);
	  }
	else if (GET_CODE (XEXP (operands[1], 0)) == PLUS)
	  {
	    rtx inc_reg = XEXP (XEXP (operands[1], 0), 0);
	    if (inc_reg == frame_pointer_rtx
		&& GET_CODE (XEXP (XEXP (operands[1], 0), 1)) == REG
		&& XEXP (XEXP (operands[1], 0), 0) != frame_pointer_rtx)
	      inc_reg = XEXP (XEXP (operands[1], 0), 1);
	    if (inc_reg == frame_pointer_rtx)
	      {
		output_asm_insn ("mov %%fp,%%g1", xoperands);
		inc_reg = gen_rtx (REG, SImode, 1);
	      }
	    xoperands[1] = inc_reg;
	    output_asm_insn ("add 4,%1,%1", xoperands);
	    xoperands[1] = operands[1];
	    output_asm_insn ("ld %1,%0", xoperands);
	    xoperands[1] = inc_reg;
	    output_asm_insn ("add -4,%1,%1", xoperands);
	  }
	else
	  {
	    xoperands[1] = gen_rtx (MEM, GET_MODE (operands[1]),
				plus_constant (XEXP (operands[1], 0), 4));
	    output_asm_insn ("ld %1,%0", xoperands);
	  }
	return "";
      }
    }
  else if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  if ((REGNO (operands[0]) & 1) == 0)
	    return "std %1,[%%fp-8]\n\tldd [%%fp-8],%0";
	  else
	    {
	      rtx xoperands[3];
	      xoperands[2] = operands[1];
	      xoperands[1] = gen_rtx (REG, SImode, REGNO (operands[0]) + 1);
	      xoperands[0] = operands[0];
	      output_asm_insn ("std %2,[%%fp-8]\n\tld [%%fp-4],%1\n\tld [%%fp-8],%0", xoperands);
	      return "";
	    }
	}
      /* Use std if we can be sure it is well-aligned.  */
      if (GET_CODE (XEXP (operands[0], 0)) == PLUS
	  && (((XEXP (XEXP (operands[0], 0), 0) == frame_pointer_rtx
		|| XEXP (XEXP (operands[0], 0), 0) == stack_pointer_rtx)
	       && GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == CONST_INT
	       && (INTVAL (XEXP (XEXP (operands[0], 0), 1)) & 0x7) == 0)
#if 0 /* An array in a structure that is a parm need not be aligned!  */
	      /* Arrays are known to be aligned,
		 and reg+reg addresses are used (on this machine)
		 only for array accesses.  */
	      || (REG_P (XEXP (XEXP (operands[0], 0), 0))
		  && REG_P (XEXP (XEXP (operands[0], 0), 1)))
#endif
	      ))
	return "std %1,%0";
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		 && cc_prev_status.mdep == XEXP (operands[0], 0)))
	    output_asm_insn ("sethi %%hi(%m0),%%g1", operands);
	  cc_status.flags |= CC_KNOW_HI_G1;
	  cc_status.mdep = XEXP (operands[0], 0);
	  return "std %1,[%%lo(%m0)+%%g1]";
	}
      /* Otherwise use two st insns.  */
      {
	rtx xoperands[2];
	output_asm_insn ("st %r1,%0", operands);
	xoperands[1] = gen_rtx (REG, GET_MODE (operands[1]),
				REGNO (operands[1]) + 1);
	if (GET_CODE (XEXP (operands[0], 0)) == PLUS
	    && offsettable_address_p (1, GET_MODE (operands[0]),
				      XEXP (operands[0], 0)))
	  {
	    xoperands[0] = adj_offsettable_operand (operands[0], 4);
	    output_asm_insn ("st %r1,%0", xoperands);
	  }
	else if (GET_CODE (XEXP (operands[0], 0)) == PLUS)
	  {
	    rtx inc_reg = XEXP (XEXP (operands[0], 0), 0);
	    if (inc_reg == frame_pointer_rtx
		&& GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == REG
		&& XEXP (XEXP (operands[0], 0), 0) != frame_pointer_rtx)
	      inc_reg = XEXP (XEXP (operands[0], 0), 1);
	    if (inc_reg == frame_pointer_rtx)
	      {
		output_asm_insn ("mov %%fp,%%g1", xoperands);
		inc_reg = gen_rtx (REG, SImode, 1);
	      }
	    xoperands[0] = inc_reg;
	    output_asm_insn ("add 4,%0,%0", xoperands);
	    xoperands[0] = operands[0];
	    output_asm_insn ("st %r1,%0", xoperands);
	    xoperands[0] = inc_reg;
	    output_asm_insn ("add -4,%0,%0", xoperands);
	  }
	else
	  {
	    xoperands[0] = gen_rtx (MEM, GET_MODE (operands[0]),
				plus_constant (XEXP (operands[0], 0), 4));
	    output_asm_insn ("st %r1,%0", xoperands);
	  }
	return "";
      }
    }
  else abort ();
}

/* Return a REG that occurs in ADDR with coefficient 1.
   ADDR can be effectively incremented by incrementing REG.  */

static rtx
find_addr_reg (addr)
     rtx addr;
{
  while (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 0)) == REG
	  && !(GET_CODE (XEXP (addr, 1)) == REG
	       && XEXP (addr, 0) == frame_pointer_rtx))
	addr = XEXP (addr, 0);
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
      else
	abort ();
    }
  if (GET_CODE (addr) == REG)
    return addr;
  abort ();
}

void
output_sized_memop (opname, mode)
     char *opname;
     enum machine_mode mode;
{
  extern struct _iobuf *asm_out_file;

  static char *ld_size_suffix[] = { "ub", "uh", "", "?", "d" };
  static char *st_size_suffix[] = { "b", "h", "", "?", "d" };
  char *modename
    = (opname[0] == 'l' ? ld_size_suffix : st_size_suffix)[GET_MODE_SIZE (mode) >> 1];

  fprintf (asm_out_file, "\t%s%s", opname, modename);
}

/* Output a store-in-memory whose operands are OPERANDS[0,1].
   OPERANDS[0] is a MEM, and OPERANDS[1] is a reg or zero.  */

char *
output_store (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx address = XEXP (operands[0], 0);

  cc_status.flags |= CC_KNOW_HI_G1;
  cc_status.mdep = address;

  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
	 && address == cc_prev_status.mdep))
    {
      output_asm_insn ("sethi %%hi(%m0),%%g1", operands);
      cc_prev_status.mdep = address;
    }

  /* Store zero in two parts when appropriate.  */
  if (mode == DFmode && operands[1] == dconst0_rtx)
    {
      /* We can't cross a page boundary here because the
	 SYMBOL_REF must be double word aligned, and for this
	 to be the case, SYMBOL_REF+4 cannot cross.  */
      output_sized_memop ("st", SImode);
      output_asm_insn ("%r1,[%%g1+%%lo(%m0)]", operands);
      output_sized_memop ("st", SImode);
      return "%r1,[%%g1+%%lo(%m0)+4]";
    }

  /* Code below isn't smart enough to move a doubleword in two parts,
     so use output_move_double to do that in the cases that require it.  */
  if ((mode == DImode || mode == DFmode)
      && (GET_CODE (operands[1]) == REG
	  && (REGNO (operands[1]) & 1)))
    return output_move_double (operands);

  output_sized_memop ("st", mode);
  return "%r1,[%%g1+%%lo(%m0)]";
}

/* Output a fixed-point load-from-memory whose operands are OPERANDS[0,1].
   OPERANDS[0] is a reg, and OPERANDS[1] is a mem.  */

char *
output_load_fixed (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx address = XEXP (operands[1], 0);

  /* We don't bother trying to see if we know %hi(address).
     This is because we are doing a load, and if we know the
     %hi value, we probably also know that value in memory.  */
  cc_status.flags |= CC_KNOW_HI_G1;
  cc_status.mdep = address;

  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
	 && address == cc_prev_status.mdep
	 && cc_prev_status.mdep == cc_status.mdep))
    {
      output_asm_insn ("sethi %%hi(%m1),%%g1", operands);
      cc_prev_status.mdep = address;
    }

  /* Code below isn't smart enough to do a doubleword in two parts.
     So handle that case the slow way.  */
  if (mode == DImode
      && GET_CODE (operands[0]) == REG   /* Moving to nonaligned reg pair */
      && (REGNO (operands[0]) & 1))
    return output_move_double (operands);

  output_sized_memop ("ld", mode);
  if (GET_CODE (operands[0]) == REG)
    return "[%%g1+%%lo(%m1)],%0";
  abort ();
}

/* Output a floating-point load-from-memory whose operands are OPERANDS[0,1].
   OPERANDS[0] is a reg, and OPERANDS[1] is a mem.
   We also handle the case where OPERANDS[0] is a mem.  */

char *
output_load_floating (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx address = XEXP (operands[1], 0);

  /* We don't bother trying to see if we know %hi(address).
     This is because we are doing a load, and if we know the
     %hi value, we probably also know that value in memory.  */
  cc_status.flags |= CC_KNOW_HI_G1;
  cc_status.mdep = address;

  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
	 && address == cc_prev_status.mdep
	 && cc_prev_status.mdep == cc_status.mdep))
    {
      output_asm_insn ("sethi %%hi(%m1),%%g1", operands);
      cc_prev_status.mdep = address;
    }

  if (mode == DFmode)
    {
      if (REG_P (operands[0]))
	{
	  if (REGNO (operands[0]) & 1)
	    return output_move_double (operands);
	  else
	    return "ldd [%%g1+%%lo(%m1)],%0";
	}
      cc_status.flags &= ~(CC_F0_IS_0|CC_F1_IS_0);
      output_asm_insn ("ldd [%%g1+%%lo(%m1)],%%f0", operands);
      operands[1] = gen_rtx (REG, DFmode, 32);
      return output_fp_move_double (operands);
    }

  if (GET_CODE (operands[0]) == MEM)
    {
      cc_status.flags &= ~CC_F1_IS_0;
      output_asm_insn ("ld [%%g1+%%lo(%1)],%%f1", operands);
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	{
	  cc_status.mdep = XEXP (operands[0], 0);
	  return "sethi %%hi(%m0),%%g1\n\tst %%f1,[%%g1+%%lo(%m0)]";
	}
      else
	return "st %%f1,%0";
    }
  return "ld [%%g1+%%lo(%m1)],%0";
}

/* Load the address specified by OPERANDS[3] into the register
   specified by OPERANDS[0].

   OPERANDS[3] may be the result of a sum, hence it could either be:

   (1) CONST
   (2) REG
   (2) REG + CONST_INT
   (3) REG + REG + CONST_INT
   (4) REG + REG  (special case of 3).

   Note that (3) is not a legitimate address.
   All cases are handled here.  */

void
output_load_address (operands)
     rtx *operands;
{
  rtx base, offset;

  if (CONSTANT_P (operands[3]))
    {
      output_asm_insn ("set %3,%0", operands);
      return;
    }

  if (REG_P (operands[3]))
    {
      if (REGNO (operands[0]) != REGNO (operands[3]))
	output_asm_insn ("mov %3,%0", operands);
      return;
    }

  if (GET_CODE (operands[3]) != PLUS)
    abort ();

  base = XEXP (operands[3], 0);
  offset = XEXP (operands[3], 1);

  if (GET_CODE (base) == CONST_INT)
    {
      rtx tmp = base;
      base = offset;
      offset = tmp;
    }

  if (GET_CODE (offset) != CONST_INT)
    {
      /* Operand is (PLUS (REG) (REG)).  */
      base = operands[3];
      offset = const0_rtx;
    }

  if (REG_P (base))
    {
      operands[6] = base;
      operands[7] = offset;
      if (SMALL_INT (offset))
	output_asm_insn ("add %6,%7,%0", operands);
      else
	output_asm_insn ("set %7,%0\n\tadd %0,%6,%0", operands);
    }
  else if (GET_CODE (base) == PLUS)
    {
      operands[6] = XEXP (base, 0);
      operands[7] = XEXP (base, 1);
      operands[8] = offset;

      if (SMALL_INT (offset))
	output_asm_insn ("add %6,%7,%0\n\tadd %0,%8,%0", operands);
      else
	output_asm_insn ("set %8,%0\n\tadd %0,%6,%0\n\tadd %0,%7,%0", operands);
    }
  else
    abort ();
}

/* Output code to place a size count SIZE in register REG.
   ALIGN is the size of the unit of transfer.

   Because block moves are pipelined, we don't include the
   first element in the transfer of SIZE to REG.  */

static void
output_size_for_block_move (size, reg, align)
     rtx size, reg;
     rtx align;
{
  rtx xoperands[3];

  xoperands[0] = reg;
  xoperands[1] = size;
  xoperands[2] = align;
  if (GET_CODE (size) == REG)
    output_asm_insn ("sub %1,%2,%0", xoperands);
  else
    {
      xoperands[1]
	= gen_rtx (CONST_INT, VOIDmode, INTVAL (size) - INTVAL (align));
      cc_status.flags &= ~ CC_KNOW_HI_G1;
      output_asm_insn ("set %1,%0", xoperands);
    }
}

/* Emit code to perform a block move.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.
   OPERANDS[2] is the size.
   OPERANDS[3] is the alignment safe to use.
   OPERANDS[4] is a register we can safely clobber as a temp.  */

char *
output_block_move (operands)
     rtx *operands;
{
  /* A vector for our computed operands.  Note that load_output_address
     makes use of (and can clobber) up to the 8th element of this vector.  */
  rtx xoperands[10];
  rtx zoperands[10];
  static int movstrsi_label = 0;
  int i, j;
  rtx temp1 = operands[4];
  rtx alignrtx = operands[3];
  int align = INTVAL (alignrtx);

  xoperands[0] = operands[0];
  xoperands[1] = operands[1];
  xoperands[2] = temp1;

  /* We can't move more than four bytes at a time
     because we have only one register to move them through.  */
  if (align > 4)
    {
      align = 4;
      alignrtx = gen_rtx (CONST_INT, VOIDmode, 4);
    }

  /* Since we clobber untold things, nix the condition codes.  */
  CC_STATUS_INIT;

  /* Recognize special cases of block moves.  These occur
     when GNU C++ is forced to treat something as BLKmode
     to keep it in memory, when its mode could be represented
     with something smaller.

     We cannot do this for global variables, since we don't know
     what pages they don't cross.  Sigh.  */
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) <= 16
      && ! CONSTANT_ADDRESS_P (operands[0])
      && ! CONSTANT_ADDRESS_P (operands[1]))
    {
      int size = INTVAL (operands[2]);

      cc_status.flags &= ~CC_KNOW_HI_G1;
      if (align == 1)
	{
	  if (memory_address_p (QImode, plus_constant (xoperands[0], size))
	      && memory_address_p (QImode, plus_constant (xoperands[1], size)))
	    {
	      /* We will store different integers into this particular RTX.  */
	      xoperands[2] = gen_rtx (CONST_INT, VOIDmode, 13);
	      for (i = size-1; i >= 0; i--)
		{
		  INTVAL (xoperands[2]) = i;
		  output_asm_insn ("ldub [%a1+%2],%%g1\n\tstb %%g1,[%a0+%2]",
				   xoperands);
		}
	      return "";
	    }
	}
      else if (align == 2)
	{
	  if (memory_address_p (HImode, plus_constant (xoperands[0], size))
	      && memory_address_p (HImode, plus_constant (xoperands[1], size)))
	    {
	      /* We will store different integers into this particular RTX.  */
	      xoperands[2] = gen_rtx (CONST_INT, VOIDmode, 13);
	      for (i = (size>>1)-1; i >= 0; i--)
		{
		  INTVAL (xoperands[2]) = i<<1;
		  output_asm_insn ("lduh [%a1+%2],%%g1\n\tsth %%g1,[%a0+%2]",
				   xoperands);
		}
	      return "";
	    }
	}
      else
	{
	  if (memory_address_p (SImode, plus_constant (xoperands[0], size))
	      && memory_address_p (SImode, plus_constant (xoperands[1], size)))
	    {
	      /* We will store different integers into this particular RTX.  */
	      xoperands[2] = gen_rtx (CONST_INT, VOIDmode, 13);
	      for (i = (size>>2)-1; i >= 0; i--)
		{
		  INTVAL (xoperands[2]) = i<<2;
		  output_asm_insn ("ld [%a1+%2],%%g1\n\tst %%g1,[%a0+%2]",
				   xoperands);
		}
	      return "";
	    }
	}
    }

  /* This is the size of the transfer.
     Either use the register which already contains the size,
     or use a free register (used by no operands).
     Also emit code to decrement the size value by ALIGN.  */
  output_size_for_block_move (operands[2], temp1, alignrtx);
     
  zoperands[0] = operands[0];
  zoperands[3] = plus_constant (operands[0], align);
  output_load_address (zoperands);

  xoperands[3] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);
  xoperands[4] = gen_rtx (CONST_INT, VOIDmode, align);

  if (align == 1)
    output_asm_insn ("\nLm%3:\n\tldub [%1+%2],%%g1\n\tsubcc %2,%4,%2\n\tbge Lm%3\n\tstb %%g1,[%0+%2]", xoperands);
  else if (align == 2)
    output_asm_insn ("\nLm%3:\n\tlduh [%1+%2],%%g1\n\tsubcc %2,%4,%2\n\tbge Lm%3\n\tsth %%g1,[%0+%2]", xoperands);
  else
    output_asm_insn ("\nLm%3:\n\tld [%1+%2],%%g1\n\tsubcc %2,%4,%2\n\tbge Lm%3\n\tst %%g1,[%0+%2]", xoperands);
  return "";
}

/* What the sparc lacks in hardware, make up for in software.
   Compute a fairly good sequence of shift and add insns
   to make a multiply happen.  */

#define ABS(x) ((x) < 0 ? -(x) : x)

char *
output_mul_by_constant (insn, operands, unsignedp)
     rtx insn;
     rtx *operands;
     int unsignedp;
{
  int c;			/* Size of constant */
  int shifts[BITS_PER_WORD];	/* Table of shifts */
  unsigned int p, log;		/* A power of two, and its log */
  int d1, d2;			/* Differences of c and p */
  int first = 1;		/* True if dst has unknown data in it */
  int i;

  CC_STATUS_INIT;

  c = INTVAL (operands[2]);
  if (c == 0)
    {
      /* Does happen, at least when not optimizing.  */
      if (GET_CODE (operands[0]) == MEM)
	return "st %%g0,%0";
      return "mov %%g0,%0";
    }

  output_asm_insn ("! start open coded multiply");

  /* Clear out the table of shifts. */
  for (i = 0; i < BITS_PER_WORD; ++i)
    shifts[i] = 0;

  while (c)
    {
      /* Find the power of two nearest ABS(c) */
      p = 1, log = 0;
      do
	{
	  d1 = ABS(c) - p;
	  p *= 2;
	  ++log;
	}
      while (p < ABS(c));
      d2 = p - ABS(c);

      /* Make an appropriate entry in shifts for p. */
      if (d2 < d1)
	{
	  shifts[log] = c < 0 ? -1 : 1;
	  c = c < 0 ? d2 : -d2;
	}
      else
	{
	  shifts[log - 1] = c < 0 ? -1 : 1;
	  c = c < 0 ? -d1 : d1;
	}
    }

  /* Take care of the first insn in sequence.
     We know we have at least one. */

  /* A value of -1 in shifts says to subtract that power of two, and a value
     of 1 says to add that power of two. */
  for (i = 0; ; i++)
    if (shifts[i])
      {
	if (i)
	  {
	    operands[2] = gen_rtx (CONST_INT, VOIDmode, i);
	    output_asm_insn ("sll %1,%2,%%g1", operands);
	  }
	else output_asm_insn ("mov %1,%%g1", operands);

	log = i;
	if (shifts[i] < 0)
	  output_asm_insn ("sub %%g0,%%g1,%0", operands);
	else
	  output_asm_insn ("mov %%g1,%0", operands);
	break;
      }

  /* A value of -1 in shifts says to subtract that power of two, and a value
     of 1 says to add that power of two--continued.  */
  for (i += 1; i < BITS_PER_WORD; ++i)
    if (shifts[i])
      {
	if (i - log > 0)
	  {
	    operands[2] = gen_rtx (CONST_INT, VOIDmode, i - log);
	    output_asm_insn ("sll %%g1,%2,%%g1", operands);
	  }
	else
	  {
	    operands[2] = gen_rtx (CONST_INT, VOIDmode, log - i);
	    output_asm_insn ("sra %%g1,%2,%%g1", operands);
	  }
	log = i;
	if (shifts[i] < 0)
	  output_asm_insn ("sub %0,%%g1,%0", operands);
	else
	  output_asm_insn ("add %0,%%g1,%0", operands);
      }

  output_asm_insn ("! end open coded multiply");

  return "";
}

char *
output_mul_insn (operands, unsignedp)
     rtx *operands;
     int unsignedp;
{
  int lucky1 = ((unsigned)REGNO (operands[1]) - 8) <= 1;
  int lucky2 = ((unsigned)REGNO (operands[2]) - 8) <= 1;

  CC_STATUS_INIT;

  if (lucky1)
    {
      if (lucky2)
	{
	  if (REGNO (operands[1]) == REGNO (operands[2]))
	    {
	      if (REGNO (operands[1]) == 8)
		output_asm_insn ("mov %%o0,%%o1");
	      else
		output_asm_insn ("mov %%o1,%%o0");
	    }
	  output_asm_insn ("call .mul,2\n\tnop", operands);
	}
      else
	{
	  rtx xoperands[2];
	  xoperands[0] = gen_rtx (REG, SImode,
				  8 ^ (REGNO (operands[1]) == 8));
	  xoperands[1] = operands[2];
	  output_asm_insn ("call .mul,2\n\tmov %1,%0", xoperands);
	}
    }
  else if (lucky2)
    {
      rtx xoperands[2];
      xoperands[0] = gen_rtx (REG, SImode,
			      8 ^ (REGNO (operands[2]) == 8));
      xoperands[1] = operands[1];
      output_asm_insn ("call .mul,2\n\tmov %1,%0", xoperands);
    }
  else
    {
      output_asm_insn ("mov %1,%%o0\n\tcall .mul,2\n\tmov %2,%%o1",
		       operands);
    }

  if (REGNO (operands[0]) == 8)
    return "";
  return "mov %%o0,%0";
}

/* Make floating point register f0 contain 0.
   SIZE is the number of registers (including f0)
   which should contain 0.  */

void
make_f0_contain_0 (size)
     int size;
{
  if (size == 1)
    {
      if ((cc_status.flags & (CC_F0_IS_0)) == 0)
	output_asm_insn ("ld [%%fp-16],%%f0", 0);
      cc_status.flags |= CC_F0_IS_0;
    }
  else if (size == 2)
    {
      if ((cc_status.flags & CC_F0_IS_0) == 0)
	output_asm_insn ("ld [%%fp-16],%%f0", 0);
      if ((cc_status.flags & (CC_F1_IS_0)) == 0)
	output_asm_insn ("ld [%%fp-12],%%f1", 0);
      cc_status.flags |= CC_F0_IS_0 | CC_F1_IS_0;
    }
}

/* Since condition codes don't have logical links, we need to keep
   their setting and use together for set-cc insns.  */
void
gen_scc_insn (code, mode, operands)
     enum rtx_code code;
     enum machine_mode mode;
     rtx *operands;
{
  extern rtx sequence_stack;
  rtx last_insn = XEXP (XEXP (sequence_stack, 1), 0);
  rtx last_pat;

  /* Skip back over the CLOBBERs that may precede this insn.  */
  while (last_insn && GET_CODE (last_insn) == INSN
	 && GET_CODE (PATTERN (last_insn)) == CLOBBER)
    last_insn = PREV_INSN (last_insn);
  /* We should have found the preceding compare.  */
  if (last_insn == 0 || GET_CODE (last_insn) != INSN)
    abort ();
  last_pat = PATTERN (last_insn);
  if (GET_CODE (last_pat) != SET
      || GET_CODE (SET_DEST (last_pat)) != CC0)
    abort ();

  /* Turn off that previous insn, now that we have got the data out of it.  */
  PUT_CODE (last_insn, NOTE);
  NOTE_LINE_NUMBER (last_insn) = NOTE_INSN_DELETED;

  /* Emit one replacement insn to compare operands and store result.  */
  emit_insn (gen_rtx (SET, VOIDmode, operands[0],
		      gen_rtx (code, mode, SET_SRC (last_pat), const0_rtx)));
}

/* Output reasonable peephole for set-on-condition-code insns.
   Note that these insns assume a particular way of defining
   labels.  Therefore, *both* tm-sparc.h and this function must
   be changed if a new syntax is needed.  */

char *
output_scc_insn (code, operand)
     enum rtx_code code;
     rtx operand;
{
  rtx xoperands[2];
  rtx label = gen_label_rtx ();
  int cc_in_fccr = cc_status.flags & CC_IN_FCCR;
  int antisymmetric = 0;

  xoperands[0] = operand;
  xoperands[1] = label;

  switch (code)
    {
    case NE:
      if (cc_in_fccr)
	output_asm_insn ("fbne,a %l0", &label);
      else
	output_asm_insn ("bne,a %l0", &label);
      break;
    case EQ:
      if (cc_in_fccr)
	output_asm_insn ("fbe,a %l0", &label);
      else
	output_asm_insn ("be,a %l0", &label);
      break;
    case GE:
      if (cc_in_fccr)
	output_asm_insn ("fbge,a %l0", &label);
      else
	output_asm_insn ("bge,a %l0", &label);
      antisymmetric = 1;
      break;
    case GT:
      if (cc_in_fccr)
	output_asm_insn ("fbg,a %l0", &label);
      else
	output_asm_insn ("bg,a %l0", &label);
      antisymmetric = 1;
      break;
    case LE:
      if (cc_in_fccr)
	output_asm_insn ("fble,a %l0", &label);
      else
	output_asm_insn ("ble,a %l0", &label);
      antisymmetric = 1;
      break;
    case LT:
      if (cc_in_fccr)
	output_asm_insn ("fbl,a %l0", &label);
      else
	output_asm_insn ("bl,a %l0", &label);
      antisymmetric = 1;
      break;
    case GEU:
      if (cc_in_fccr)
	abort ();
      else
	output_asm_insn ("bgeu,a %l0", &label);
      antisymmetric = 1;
      break;
    case GTU:
      if (cc_in_fccr)
	abort ();
      else
	output_asm_insn ("bgu,a %l0", &label);
      antisymmetric = 1;
      break;
    case LEU:
      if (cc_in_fccr)
	abort ();
      else
	output_asm_insn ("bleu,a %l0", &label);
      antisymmetric = 1;
      break;
    case LTU:
      if (cc_in_fccr)
	abort ();
      else
	output_asm_insn ("blu,a %l0", &label);
      antisymmetric = 1;
      break;
    default:
      abort ();
    }

  if (antisymmetric
      && (cc_status.flags & CC_REVERSED))
    output_asm_insn ("orcc %%g0,0,%0\n\torcc %%g0,1,%0\n%l1:", xoperands);
  else
    output_asm_insn ("orcc %%g0,1,%0\n\torcc %%g0,0,%0\n%l1:", xoperands);
  cc_status.flags &= ~CC_IN_FCCR;

  return "";
}

/* Output a delayed branch insn with the delay insn in its
   branch slot.  The delayed branch insn template is in TEMPLATE,
   with operands OPERANDS.  The insn in its delay slot is INSN.

   As a special case, since we know that all memory transfers are via
   ld/st insns, if we see a (MEM (SYMBOL_REF ...)) we divide the memory
   reference around the branch as

	sethi %hi(x),%%g1
	b ...
	ld/st [%g1+%lo(x)],...

   As another special case, we handle loading (SYMBOL_REF ...) and
   other large constants around branches as well:

	sethi %hi(x),%0
	b ...
	or %0,%lo(x),%1

   */

char *
output_delayed_branch (template, operands, insn)
     char *template;
     rtx *operands;
     rtx insn;
{
  extern rtx recog_operand[];
  rtx src = XVECEXP (PATTERN (insn), 0, 1);
  rtx dest = XVECEXP (PATTERN (insn), 0, 0);

  if (GET_CODE (src) == SYMBOL_REF
      || (GET_CODE (src) == CONST_INT
	  && !(SMALL_INT (src) || (INTVAL (src) & 0x3ff) == 0)))
    {
      rtx xoperands[2];
      xoperands[0] = dest;
      xoperands[1] = src;

      /* Output the `sethi' insn.  */
      output_asm_insn ("sethi %%hi(%1),%0", xoperands);

      /* Output the branch instruction next.  */
      output_asm_insn (template, operands);

      /* Now output the `or' insn.  */
      output_asm_insn ("or %0,%%lo(%1),%0", xoperands);
    }
  else if ((GET_CODE (src) == MEM
	    && CONSTANT_ADDRESS_P (XEXP (src, 0)))
	   || (GET_CODE (dest) == MEM
	       && CONSTANT_ADDRESS_P (XEXP (dest, 0))))
    {
      rtx xoperands[2];
      char *split_template;
      xoperands[0] = dest;
      xoperands[1] = src;

      /* Output the `sethi' insn.  */
      if (GET_CODE (src) == MEM)
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		 && cc_prev_status.mdep == XEXP (operands[1], 0)))
	    output_asm_insn ("sethi %%hi(%m1),%%g1", xoperands);
	  split_template = "ld [%%g1+%%lo(%m1)],%0";
	}
      else
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_G1)
		 && cc_prev_status.mdep == XEXP (operands[0], 0)))
	    output_asm_insn ("sethi %%hi(%m0),%%g1", xoperands);
	  split_template = "st %r1,[%%g1+%%lo(%m0)]";
	}

      /* Output the branch instruction next.  */
      output_asm_insn (template, operands);

      /* Now output the load or store.
	 No need to do a CC_STATUS_INIT, because we are branching anyway.  */
      output_asm_insn (split_template, xoperands);
    }
  else
    {
      extern char *insn_template[];
      extern char *(*insn_outfun[])();
      int insn_code_number;
      rtx pat = gen_rtx (SET, VOIDmode, dest, src);
      rtx delay_insn = gen_rtx (INSN, VOIDmode, 0, 0, 0, pat, -1, 0, 0);
      int i;
      extern rtx alter_subreg();
      extern int insn_n_operands[];

      /* Output the branch instruction first.  */
      output_asm_insn (template, operands);

      /* Now recognize the insn which we put in its delay slot.
	 We must do this after outputing the branch insn,
	 since operands may just be a pointer to `recog_operand'.  */
      insn_code_number = recog (pat, delay_insn);
      if (insn_code_number == -1)
	abort ();

      for (i = 0; i < insn_n_operands[insn_code_number]; i++)
	{
	  if (GET_CODE (recog_operand[i]) == SUBREG)
	    recog_operand[i] = alter_subreg (recog_operand[i]);
	}

      /* Now get the template for what this insn would
	 have been, without the branch.  Its operands are
	 exactly the same as they would be, so we don't
	 need to do an insn_extract.  */
      template = insn_template[insn_code_number];
      if (template == 0)
	template = (*insn_outfun[insn_code_number]) (recog_operand, delay_insn);
      output_asm_insn (template, recog_operand);
    }
  CC_STATUS_INIT;
  return "";
}

/* Output a newly constructed insn DELAY_INSN.  */
char *
output_delay_insn (delay_insn)
     rtx delay_insn;
{
  char *template;
  extern rtx recog_operand[];
  extern char call_used_regs[];
  extern char *insn_template[];
  extern int insn_n_operands[];
  extern char *(*insn_outfun[])();
  extern rtx alter_subreg();
  int insn_code_number;
  extern int insn_n_operands[];
  int i;

  /* Now recognize the insn which we put in its delay slot.
     We must do this after outputing the branch insn,
     since operands may just be a pointer to `recog_operand'.  */
  insn_code_number = recog_memoized (delay_insn);
  if (insn_code_number == -1)
    abort ();

  /* Extract the operands of this delay insn.  */
  INSN_CODE (delay_insn) = insn_code_number;
  insn_extract (delay_insn);

  /* It is possible that this insn has not been properly scaned by final
     yet.  If this insn's operands don't appear in the peephole's
     actual operands, then they won't be fixed up by final, so we
     make sure they get fixed up here.  -- This is a kludge.  */
  for (i = 0; i < insn_n_operands[insn_code_number]; i++)
    {
      if (GET_CODE (recog_operand[i]) == SUBREG)
	recog_operand[i] = alter_subreg (recog_operand[i]);
    }

#ifdef REGISTER_CONSTRAINTS
  if (! constrain_operands (insn_code_number))
    abort ();
#endif

  cc_prev_status = cc_status;

  /* Update `cc_status' for this instruction.
     The instruction's output routine may change it further.
     If the output routine for a jump insn needs to depend
     on the cc status, it should look at cc_prev_status.  */

  NOTICE_UPDATE_CC (PATTERN (delay_insn), delay_insn);

  /* Now get the template for what this insn would
     have been, without the branch.  */

  template = insn_template[insn_code_number];
  if (template == 0)
    template = (*insn_outfun[insn_code_number]) (recog_operand, delay_insn);
  output_asm_insn (template, recog_operand);
  return "";
}

/* Output the insn HEAD, keeping OPERANDS protected (wherever they are).
   HEAD comes from the target of some branch, so before we output it,
   we delete it from the target, lest we execute it twice.  The caller
   of this function promises that such code motion is permissable.  */
char *
output_eager_then_insn (head, operands)
     rtx head;
     rtx *operands;
{
  extern rtx alter_subreg ();
  extern int insn_n_operands[];
  extern rtx recog_operand[];
  rtx xoperands[MAX_RECOG_OPERANDS];
  int insn_code_number, i, nbytes;
  rtx nhead;

  /* Micro-hack: run peephole on head if it looks like a good idea.
     Right now there's only one such case worth doing...

     This could be made smarter if the peephole for ``2-insn combine''
     were also made smarter.  */
  if (GET_CODE (PATTERN (head)) == SET
      && REG_P (SET_SRC (PATTERN (head)))
      && REG_P (SET_DEST (PATTERN (head)))
      && (nhead = next_real_insn_no_labels (head))
      && GET_CODE (nhead) == INSN
      && GET_CODE (PATTERN (nhead)) == SET
      && GET_CODE (SET_DEST (PATTERN (nhead))) == CC0
      && (SET_SRC (PATTERN (nhead)) == SET_SRC (PATTERN (head))
	  || SET_SRC (PATTERN (nhead)) == SET_DEST (PATTERN (head))))
    /* Something's wrong if this does not fly.  */
    if (! peephole (head))
      abort ();

  /* Save our contents of `operands', since output_delay_insn sets them.  */
  insn_code_number = recog_memoized (head);
  nbytes = insn_n_operands[insn_code_number] * sizeof (rtx);
  bcopy (operands, xoperands, nbytes);

  /* Output the delay insn, and prevent duplication later.  */
  delete_insn (head);
  output_delay_insn (head);

  /* Restore this insn's operands.  */
  bcopy (xoperands, operands, nbytes);
}

/* Return the next INSN, CALL_INSN or JUMP_INSN after LABEL;
   or 0, if there is none.  Also return 0 if we cross a label.  */

rtx
next_real_insn_no_labels (label)
     rtx label;
{
  register rtx insn = NEXT_INSN (label);
  register RTX_CODE code;

  while (insn)
    {
      code = GET_CODE (insn);
      if (code == INSN)
	{
	  if (GET_CODE (PATTERN (insn)) != CLOBBER
	      && GET_CODE (PATTERN (insn)) != USE)
	    return insn;
	}
      if (code == CALL_INSN || code == JUMP_INSN)
	return insn;
      if (code == CODE_LABEL)
	return 0;
      insn = NEXT_INSN (insn);
    }

  return 0;
}

int
operands_satisfy_eager_branch_peephole (operands, conditional)
     rtx *operands;
     int conditional;
{
  rtx label;

  if (conditional)
    {
      if (GET_CODE (operands[0]) != IF_THEN_ELSE)
	return 0;

      if (GET_CODE (XEXP (operands[0], 1)) == LABEL_REF)
	label = XEXP (XEXP (operands[0], 1), 0);
      else if (GET_CODE (XEXP (operands[0], 2)) == LABEL_REF)
	label = XEXP (XEXP (operands[0], 2), 0);
      else return 0;
    }
  else
    {
      label = operands[0];
    }

  if (LABEL_NUSES (label) == 1)
    {
      rtx prev = PREV_INSN (label);
      while (prev && GET_CODE (prev) == NOTE)
	prev = PREV_INSN (prev);
      if (prev == 0
	  || GET_CODE (prev) == BARRIER)
	{
	  rtx head = next_real_insn_no_labels (label);

	  if (head
	      && ! INSN_DELETED_P (head)
	      && GET_CODE (head) == INSN
	      && GET_CODE (PATTERN (head)) == SET
	      && strict_single_insn_op_p (SET_SRC (PATTERN (head)),
					  GET_MODE (SET_DEST (PATTERN (head))))
	      && strict_single_insn_op_p (SET_DEST (PATTERN (head)),
					  GET_MODE (SET_DEST (PATTERN (head))))
	      /* Moves between FP regs and CPU regs are two insns.  */
	      && !(GET_CODE (SET_SRC (PATTERN (head))) == REG
		   && GET_CODE (SET_DEST (PATTERN (head))) == REG
		   && (FP_REG_P (SET_SRC (PATTERN (head)))
		       != FP_REG_P (SET_DEST (PATTERN (head))))))
	    {
	      if (conditional == 2)
		return (GET_CODE (operands[1]) != PC
			&& safe_insn_src_p (operands[2], VOIDmode)
			&& strict_single_insn_op_p (operands[2], VOIDmode)
			&& operand_clobbered_before_used_after (operands[1], label));
	      return 1;
	    }
	}
    }

  if (conditional == 1
      && GET_CODE (operands[1]) != PC
      && safe_insn_src_p (operands[2], VOIDmode)
      && strict_single_insn_op_p (operands[2], VOIDmode)
      && operand_clobbered_before_used_after (operands[1], label))
    return 1;

  return 0;
}

