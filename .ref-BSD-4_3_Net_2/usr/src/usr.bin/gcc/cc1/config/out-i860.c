/* Subroutines for insn-output.c for Intel 860
   Copyright (C) 1989 Free Software Foundation, Inc.
   Derived from out-sparc.c.

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

/* Return non-zero only if OP is a register of mode MODE,
   or const0_rtx.  */
int
reg_or_0_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (op == const0_rtx || register_operand (op, mode)
	  || op == CONST0_RTX (mode));
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
    case ZERO_EXTEND:
      return 1;

    case EQ:
    case NE:
    case LT:
    case GT:
    case LE:
    case GE:
    case LTU:
    case GTU:
    case LEU:
    case GEU:
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
   Return 2 if REG is used in IN. 
   Return 3 if REG is both used and clobbered in IN.
   Return 0 if neither.  */

static int
reg_clobbered_p (reg, in)
     rtx reg;
     rtx in;
{
  register enum rtx_code code;

  if (in == 0)
    return 0;

  code = GET_CODE (in);

  if (code == SET || code == CLOBBER)
    {
      rtx dest = SET_DEST (in);
      int set = 0;
      int used = 0;

      while (GET_CODE (dest) == STRICT_LOW_PART
	     || GET_CODE (dest) == SUBREG
	     || GET_CODE (dest) == SIGN_EXTRACT
	     || GET_CODE (dest) == ZERO_EXTRACT)
	dest = XEXP (dest, 0);

      if (dest == reg)
	set = 1;
      else if (GET_CODE (dest) == REG
	       && refers_to_regno_p (REGNO (reg),
				     REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				     SET_DEST (in), 0))
	{
	  set = 1;
	  /* Anything that sets just part of the register
	     is considered using as well as setting it.
	     But note that a straight SUBREG of a single-word value
	     clobbers the entire value.   */
	  if (dest != SET_DEST (in)
	      && ! (GET_CODE (SET_DEST (in)) == SUBREG
		    || UNITS_PER_WORD >= GET_MODE_SIZE (GET_MODE (dest))))
	    used = 1;
	}

      if (code == SET)
	{
	  if (set)
	    used = refers_to_regno_p (REGNO (reg),
				      REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				      SET_SRC (in), 0);
	  else
	    used = refers_to_regno_p (REGNO (reg),
				      REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
				      in, 0);
	}

      return set + used * 2;
    }

  if (refers_to_regno_p (REGNO (reg),
			 REGNO (reg) + HARD_REGNO_NREGS (reg, GET_MODE (reg)),
			 in, 0))
    return 2;
  return 0;
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

      /* Scan forward from the label, to see if the value of OP
	 is clobbered before the first use.  */

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
		default:
		  return 0;
		case 1:
		  return 1;
		case 0:
		  break;
		}
	    }
	  /* If we reach another label without clobbering OP,
	     then we cannot safely write it here.  */
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
     for this op will be a orh whatever%h,r0,r31,
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
      /* This is not always a single insn src, technically,
	 but output_delayed_branch knows how to deal with it.  */
      return 1;

    case SYMBOL_REF:
    case CONST:
      /* This is not a single insn src, technically,
	 but output_delayed_branch knows how to deal with it.  */
      return 1;

    case REG:
      return 1;

    case MEM:
      return 1;

      /* We never need to negate or complement constants.  */
    case NEG:
      return (mode != DFmode);
    case NOT:
    case ZERO_EXTEND:
      return 1;

    case PLUS:
    case MINUS:
      /* Detect cases that require multiple instructions.  */
      if (CONSTANT_P (XEXP (op, 1))
	  && !(GET_CODE (XEXP (op, 1)) == CONST_INT
	       && SMALL_INT (XEXP (op, 1))))
	return 0;
    case EQ:
    case NE:
    case LT:
    case GT:
    case LE:
    case GE:
    case LTU:
    case GTU:
    case LEU:
    case GEU:
      /* Not doing floating point, since they probably
	 take longer than the branch slot they might fill.  */
      return (mode != SFmode && mode != DFmode);

    case AND:
      if (GET_CODE (XEXP (op, 1)) == NOT)
	{
	  rtx arg = XEXP (XEXP (op, 1), 0);
	  if (CONSTANT_P (arg)
	      && !(GET_CODE (arg) == CONST_INT
		   && (SMALL_INT (arg)
		       || INTVAL (arg) & 0xffff == 0)))
	    return 0;
	}
    case IOR:
    case XOR:
      /* Both small and round numbers take one instruction;
	 others take two.  */
      if (CONSTANT_P (XEXP (op, 1))
	  && !(GET_CODE (XEXP (op, 1)) == CONST_INT
	       && (SMALL_INT (XEXP (op, 1))
		   || INTVAL (XEXP (op, 1)) & 0xffff == 0)))
	return 0;

    case LSHIFT:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      return 1;

    case SUBREG:
      if (SUBREG_WORD (op) != 0)
	return 0;
      return single_insn_src_p (SUBREG_REG (op), mode);

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
#if 0
      /* This loses when moving an freg to a general reg.  */
      return HARD_REGNO_NREGS (REGNO (op), mode) == 1;
#endif
      return (mode != DFmode && mode != DImode);

    case MEM:
      if (! CONSTANT_ADDRESS_P (XEXP (op, 0)))
	return (mode != DFmode && mode != DImode);
      return 0;

      /* We never need to negate or complement constants.  */
    case NEG:
      return (mode != DFmode);
    case NOT:
    case ZERO_EXTEND:
      return 1;

    case PLUS:
    case MINUS:
      /* Detect cases that require multiple instructions.  */
      if (CONSTANT_P (XEXP (op, 1))
	  && !(GET_CODE (XEXP (op, 1)) == CONST_INT
	       && SMALL_INT (XEXP (op, 1))))
	return 0;
    case EQ:
    case NE:
    case LT:
    case GT:
    case LE:
    case GE:
    case LTU:
    case GTU:
    case LEU:
    case GEU:
      return 1;

    case AND:
      if (GET_CODE (XEXP (op, 1)) == NOT)
	{
	  rtx arg = XEXP (XEXP (op, 1), 0);
	  if (CONSTANT_P (arg)
	      && !(GET_CODE (arg) == CONST_INT
		   && (SMALL_INT (arg)
		       || INTVAL (arg) & 0xffff == 0)))
	    return 0;
	}
    case IOR:
    case XOR:
      /* Both small and round numbers take one instruction;
	 others take two.  */
      if (CONSTANT_P (XEXP (op, 1))
	  && !(GET_CODE (XEXP (op, 1)) == CONST_INT
	       && (SMALL_INT (XEXP (op, 1))
		   || INTVAL (XEXP (op, 1)) & 0xffff == 0)))
	return 0;
    case LSHIFT:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      return 1;

    case SUBREG:
      if (SUBREG_WORD (op) != 0)
	return 0;
      return strict_single_insn_op_p (SUBREG_REG (op), mode);

    case SIGN_EXTEND:
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

/* Return truth value of whether OP can be used as an operands in a three
   address add/subtract insn (such as add %o1,7,%l2) of mode MODE.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op)));
}

/* Return 1 if OP is a valid first operand for a logical insn of mode MODE.  */

int
logic_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && LOGIC_INT (op)));
}

/* Return 1 if OP is a valid first operand for either a logical insn
   or an add insn of mode MODE.  */

int
compare_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT && SMALL_INT (op) && LOGIC_INT (op)));
}

/* Return truth value of whether OP can be used as an operand
   of a bte insn.  */

int
bte_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (unsigned) INTVAL (op) < 0x20));
}

/* Return 1 if OP is an indexed memory reference of mode MODE.  */

int
indexed_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == MEM && GET_MODE (op) == mode
	  && GET_CODE (XEXP (op, 0)) == PLUS
	  && GET_MODE (XEXP (op, 0)) == SImode
	  && register_operand (XEXP (XEXP (op, 0), 0), SImode)
	  && register_operand (XEXP (XEXP (op, 0), 1), SImode));
}

/* Return 1 if OP is a suitable source operand for a load insn
   with mode MODE.  */

int
load_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (memory_operand (op, mode) || indexed_operand (op, mode));
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in add/subtract insns.  */

int
small_int (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && SMALL_INT (op));
}

/* Return truth value of whether OP is a integer which fits the
   range constraining immediate operands in logic insns.  */

int
logic_int (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && LOGIC_INT (op));
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
	    if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		   && (cc_prev_status.flags & CC_HI_R31_ADJ)
		   && cc_prev_status.mdep == XEXP (operands[0], 0)))
	      output_asm_insn ("orh ha%%%m0,r0,r31", operands);
	    cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
	    cc_status.mdep = XEXP (operands[0], 0);
	    return "st.l %r1,l%%%m0(r31)";
	  }
	else
	  return "st.l %r1,%0";
      else
	abort ();
#if 0
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
#endif
    }
  if (GET_CODE (operands[1]) == MEM)
    {
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		 && (cc_prev_status.flags & CC_HI_R31_ADJ)
		 && cc_prev_status.mdep == XEXP (operands[1], 0)))
	    output_asm_insn ("orh ha%%%m1,r0,r31", operands);
	  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
	  cc_status.mdep = XEXP (operands[1], 0);
	  return "ld.l l%%%m1(r31),%0";
	}
      return "ld.l %1,%0";
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

/* ??? Perhaps in some cases move double words
   if there is a spare pair of floating regs.  */

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

  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (latehalf[1]))
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	output_asm_insn ("adds 0x4,%0,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("adds 0x4,%0,%0", &addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("adds -0x4,%0,%0", &addreg0);
      if (addreg1)
	output_asm_insn ("adds -0x4,%0,%0", &addreg1);

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
    output_asm_insn ("adds 0x4,%0,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("adds 0x4,%0,%0", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("adds -0x4,%0,%0", &addreg0);
  if (addreg1)
    output_asm_insn ("adds -0x4,%0,%0", &addreg1);

  return "";
}

static char *
output_fp_move_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      if (FP_REG_P (operands[1]))
	return "fmov.dd %1,%0";
      if (GET_CODE (operands[1]) == REG)
	{
	  output_asm_insn ("ixfr %1,%0", operands);
	  operands[0] = gen_rtx (REG, VOIDmode, REGNO (operands[0]) + 1);
	  operands[1] = gen_rtx (REG, VOIDmode, REGNO (operands[1]) + 1);
	  return "ixfr %1,%0";
	}
      if (operands[1] == dconst0_rtx)
	return "fmov.dd f0,%0";
      if (CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		 && (cc_prev_status.flags & CC_HI_R31_ADJ)
		 && cc_prev_status.mdep == XEXP (operands[1], 0)))
	    output_asm_insn ("orh ha%%%m1,r0,r31", operands);
	  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
	  cc_status.mdep = XEXP (operands[1], 0);
	  return "fld.d l%%%m1(r31),%0";
	}
      return "fld.d %1,%0";
    }
  else if (FP_REG_P (operands[1]))
    {
      if (GET_CODE (operands[0]) == REG)
	{
	  output_asm_insn ("fxfr %1,%0", operands);
	  operands[0] = gen_rtx (REG, VOIDmode, REGNO (operands[0]) + 1);
	  operands[1] = gen_rtx (REG, VOIDmode, REGNO (operands[1]) + 1);
	  return "fxfr %1,%0";
	}
      if (CONSTANT_ADDRESS_P (XEXP (operands[0], 0)))
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		 && (cc_prev_status.flags & CC_HI_R31_ADJ)
		 && cc_prev_status.mdep == XEXP (operands[0], 0)))
	    output_asm_insn ("orh ha%%%m0,r0,r31", operands);
	  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
	  cc_status.mdep = XEXP (operands[0], 0);
	  return "fst.d %1,l%%%m0(r31)";
	}
      return "fst.d %1,%0";
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
      if (GET_CODE (XEXP (addr, 0)) == REG)
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

/* Return a template for a load instruction with mode MODE and
   arguments from the string ARGS.

   This string is in static storage.   */

static char *
load_opcode (mode, args, reg)
     enum machine_mode mode;
     char *args;
     rtx reg;
{
  static char buf[30];
  char *opcode;

  switch (mode)
    {
    case QImode:
      opcode = "ld.b";
      break;

    case HImode:
      opcode = "ld.s";
      break;

    case SImode:
    case SFmode:
      if (FP_REG_P (reg))
	opcode = "fld.l";
      else
	opcode = "ld.l";
      break;

    case DImode:
      if (!FP_REG_P (reg))
	abort ();
    case DFmode:
      opcode = "fld.d";
      break;

    default:
      abort ();
    }

  sprintf (buf, "%s %s", opcode, args);
  return buf;
}

/* Return a template for a store instruction with mode MODE and
   arguments from the string ARGS.

   This string is in static storage.   */

static char *
store_opcode (mode, args, reg)
     enum machine_mode mode;
     char *args;
     rtx reg;
{
  static char buf[30];
  char *opcode;

  switch (mode)
    {
    case QImode:
      opcode = "st.b";
      break;

    case HImode:
      opcode = "st.s";
      break;

    case SImode:
    case SFmode:
      if (FP_REG_P (reg))
	opcode = "fst.l";
      else
	opcode = "st.l";
      break;

    case DImode:
      if (!FP_REG_P (reg))
	abort ();
    case DFmode:
      opcode = "fst.d";
      break;

    default:
      abort ();
    }

  sprintf (buf, "%s %s", opcode, args);
  return buf;
}

/* Output a store-in-memory whose operands are OPERANDS[0,1].
   OPERANDS[0] is a MEM, and OPERANDS[1] is a reg or zero.

   This function returns a template for an insn.
   This is in static storage.

   It may also output some insns directly.
   It may alter the values of operands[0] and operands[1].  */

char *
output_store (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx address = XEXP (operands[0], 0);
  char *string;

  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
  cc_status.mdep = address;

  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
	 && (cc_prev_status.flags & CC_HI_R31_ADJ)
	 && address == cc_prev_status.mdep))
    {
      output_asm_insn ("orh ha%%%m0,r0,r31", operands);
      cc_prev_status.mdep = address;
    }

  /* Store zero in two parts when appropriate.  */
  if (mode == DFmode && operands[1] == dconst0_rtx)
    return store_opcode (DFmode, "%r1,l%%%m0(r31)", operands[1]);

  /* Code below isn't smart enough to move a doubleword in two parts,
     so use output_move_double to do that in the cases that require it.  */
  if ((mode == DImode || mode == DFmode)
      && ! FP_REG_P (operands[1]))
    return output_move_double (operands);

  return store_opcode (mode, "%r1,l%%%m0(r31)", operands[1]);
}

/* Output a load-from-memory whose operands are OPERANDS[0,1].
   OPERANDS[0] is a reg, and OPERANDS[1] is a mem.

   This function returns a template for an insn.
   This is in static storage.

   It may also output some insns directly.
   It may alter the values of operands[0] and operands[1].  */

char *
output_load (operands)
     rtx *operands;
{
  enum machine_mode mode = GET_MODE (operands[0]);
  rtx address = XEXP (operands[1], 0);

  /* We don't bother trying to see if we know %hi(address).
     This is because we are doing a load, and if we know the
     %hi value, we probably also know that value in memory.  */
  cc_status.flags |= CC_KNOW_HI_R31 | CC_HI_R31_ADJ;
  cc_status.mdep = address;

  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
	 && (cc_prev_status.flags & CC_HI_R31_ADJ)
	 && address == cc_prev_status.mdep
	 && cc_prev_status.mdep == cc_status.mdep))
    {
      output_asm_insn ("orh ha%%%m1,r0,r31", operands);
      cc_prev_status.mdep = address;
    }

  /* Code below isn't smart enough to move a doubleword in two parts,
     so use output_move_double to do that in the cases that require it.  */
  if ((mode == DImode || mode == DFmode)
      && ! FP_REG_P (operands[0]))
    return output_move_double (operands);

  return load_opcode (mode, "l%%%m1(r31),%0", operands[0]);
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
      output_asm_insn ("mov %3,%0", operands);
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
	output_asm_insn ("adds %7,%6,%0", operands);
      else
	output_asm_insn ("mov %7,%0\n\tadds %0,%6,%0", operands);
    }
  else if (GET_CODE (base) == PLUS)
    {
      operands[6] = XEXP (base, 0);
      operands[7] = XEXP (base, 1);
      operands[8] = offset;

      if (SMALL_INT (offset))
	output_asm_insn ("adds %6,%7,%0\n\tadds %8,%0,%0", operands);
      else
	output_asm_insn ("mov %8,%0\n\tadds %0,%6,%0\n\tadds %0,%7,%0", operands);
    }
  else
    abort ();
}

/* Output code to place a size count SIZE in register REG.  */

static void
output_size_for_block_move (size, reg, align)
     rtx size, reg, align;
{
  rtx xoperands[3];

  xoperands[0] = reg;
  xoperands[1] = size;
  xoperands[2] = align;

#if 1
  cc_status.flags &= ~ CC_KNOW_HI_R31;
  output_asm_insn ("mov %1,%0", xoperands);
#else
  if (GET_CODE (size) == REG)
    output_asm_insn ("sub %2,%1,%0", xoperands);
  else
    {
      xoperands[1]
	= gen_rtx (CONST_INT, VOIDmode, INTVAL (size) - INTVAL (align));
      cc_status.flags &= ~ CC_KNOW_HI_R31;
      output_asm_insn ("mov %1,%0", xoperands);
    }
#endif
}

/* Emit code to perform a block move.

   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.
   OPERANDS[2] is the size.
   OPERANDS[3] is the known safe alignment.
   OPERANDS[4..6] are pseudos we can safely clobber as temps.  */

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
      rtx op0 = xoperands[0];
      rtx op1 = xoperands[1];

      cc_status.flags &= ~CC_KNOW_HI_R31;
      if (align == 1)
	{
	  if (memory_address_p (QImode, plus_constant (op0, size))
	      && memory_address_p (QImode, plus_constant (op1, size)))
	    {
	      for (i = size-1; i >= 0; i--)
		{
		  xoperands[0] = plus_constant (op0, i);
		  xoperands[1] = plus_constant (op1, i);
		  output_asm_insn ("ld.b %a1,r31\n\tst.b r31,%a0",
				   xoperands);
		}
	      return "";
	    }
	}
      else if (align == 2)
	{
	  if (memory_address_p (HImode, plus_constant (op0, size))
	      && memory_address_p (HImode, plus_constant (op1, size)))
	    {
	      for (i = (size>>1)-1; i >= 0; i--)
		{
		  xoperands[0] = plus_constant (op0, i * 2);
		  xoperands[1] = plus_constant (op1, i * 2);
		  output_asm_insn ("ld.s %a1,r31\n\tst.s r31,%a0",
				   xoperands);
		}
	      return "";
	    }
	}
      else
	{
	  if (memory_address_p (SImode, plus_constant (op0, size))
	      && memory_address_p (SImode, plus_constant (op1, size)))
	    {
	      for (i = (size>>2)-1; i >= 0; i--)
		{
		  xoperands[0] = plus_constant (op0, i * 4);
		  xoperands[1] = plus_constant (op1, i * 4);
		  output_asm_insn ("ld.l %a1,r31\n\tst.l r31,%a0",
				   xoperands);
		}
	      return "";
	    }
	}
    }

  /* This is the size of the transfer.
     Either use the register which already contains the size,
     or use a free register (used by no operands).  */
  output_size_for_block_move (operands[2], operands[4], alignrtx);

#if 0
  /* Also emit code to decrement the size value by ALIGN.  */
  zoperands[0] = operands[0];
  zoperands[3] = plus_constant (operands[0], align);
  output_load_address (zoperands);
#endif

  /* Generate number for unique label.  */

  xoperands[3] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);

  /* Copy the increment (negative) to a register for bla insn.  */

  xoperands[4] = gen_rtx (CONST_INT, VOIDmode, - align);
  xoperands[5] = operands[5];
  output_asm_insn ("mov %4,%5", xoperands);

  /* Make available a register which is a temporary.  */

  xoperands[6] = operands[6];

  /* Now the actual loop.
     In xoperands, elements 1 and 0 are the input and output vectors.
     Element 2 is the loop index.  Element 5 is the increment.  */

  if (align == 1)
    {
      output_asm_insn ("bla %5,%2,.Lm%3", xoperands);
      output_asm_insn ("adds %0,%2,%6\n.Lm%3:", xoperands);

      xoperands[3] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);

      output_asm_insn ("adds %1,%2,%1", xoperands);
      output_asm_insn ("adds %5,%2,%2\n.Lm%3:",  xoperands);
      output_asm_insn ("ld.b 0(%1),r31", xoperands);
      output_asm_insn ("adds %5,%1,%1", xoperands);
      output_asm_insn ("st.b r31,0(%6)", xoperands);
      output_asm_insn ("bla %5,%2,.Lm%3", xoperands);
      output_asm_insn ("adds %5,%6,%6", xoperands);
    }
  if (align == 2)
    {
      output_asm_insn ("bla %5,%2,.Lm%3", xoperands);
      output_asm_insn ("adds %0,%2,%6\n.Lm%3:", xoperands);

      xoperands[3] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);

      output_asm_insn ("adds %1,%2,%1", xoperands);
      output_asm_insn ("adds %5,%2,%2\n.Lm%3:",  xoperands);
      output_asm_insn ("ld.s 0(%1),r31", xoperands);
      output_asm_insn ("adds %5,%1,%1", xoperands);
      output_asm_insn ("st.s r31,0(%6)", xoperands);
      output_asm_insn ("bla %5,%2,.Lm%3", xoperands);
      output_asm_insn ("adds %5,%6,%6", xoperands);
    }
  if (align == 4)
    {
      output_asm_insn ("bla %5,%2,.Lm%3", xoperands);
      output_asm_insn ("adds %0,%2,%6\n.Lm%3:", xoperands);

      xoperands[3] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);

      output_asm_insn ("adds %1,%2,%1", xoperands);
      output_asm_insn ("adds %5,%2,%2\n.Lm%3:",  xoperands);
      output_asm_insn ("ld.l 0(%1),r31", xoperands);
      output_asm_insn ("adds %5,%1,%1", xoperands);
      output_asm_insn ("st.l r31,0(%6)", xoperands);
      output_asm_insn ("bla %5,%2,.Lm%3", xoperands);
      output_asm_insn ("adds %5,%6,%6", xoperands);
    }

  return "";
}

/* Output a delayed branch insn with the delay insn in its
   branch slot.  The delayed branch insn template is in TEMPLATE,
   with operands OPERANDS.  The insn in its delay slot is INSN.

   As a special case, since we know that all memory transfers are via
   ld/st insns, if we see a (MEM (SYMBOL_REF ...)) we divide the memory
   reference around the branch as

	orh ha%x,r0,r31
	b ...
	ld/st l%x(r31),...

   As another special case, we handle loading (SYMBOL_REF ...) and
   other large constants around branches as well:

	orh h%x,r0,%0
	b ...
	or l%x,%0,%1

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

  if (GET_CODE (src) == SYMBOL_REF || GET_CODE (src) == CONST
      || (GET_CODE (src) == CONST_INT
	  && !(SMALL_INT (src) || (INTVAL (src) & 0xffff) == 0)))
    {
      rtx xoperands[2];
      xoperands[0] = dest;
      xoperands[1] = src;

      /* Output the `orh' insn.  */
      output_asm_insn ("orh h%%%1,r0,%0", xoperands);

      /* Output the branch instruction next.  */
      output_asm_insn (template, operands);

      /* Now output the `or' insn.  */
      output_asm_insn ("or l%%%1,%0,%0", xoperands);
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

      /* Output the `orh' insn.  */
      if (GET_CODE (src) == MEM)
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		 && (cc_prev_status.flags & CC_HI_R31_ADJ)
		 && cc_prev_status.mdep == XEXP (operands[1], 0)))
	    output_asm_insn ("orh ha%%%m1,r0,r31", xoperands);
	  split_template = load_opcode (GET_MODE (dest),
					"l%%%m1(r31),%0", dest);
	}
      else
	{
	  if (! ((cc_prev_status.flags & CC_KNOW_HI_R31)
		 && (cc_prev_status.flags & CC_HI_R31_ADJ)
		 && cc_prev_status.mdep == XEXP (operands[0], 0)))
	    output_asm_insn ("orh ha%%%m0,r0,r31", xoperands);
	  split_template = store_opcode (GET_MODE (dest),
					 "%r1,l%%%m0(r31)", src);
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
      extern int insn_n_operands[];
      extern rtx alter_subreg();
      int insn_code_number;
      rtx pat = gen_rtx (SET, VOIDmode, dest, src);
      rtx delay_insn = gen_rtx (INSN, VOIDmode, 0, 0, 0, pat, -1, 0, 0);
      int i;

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
