/* Subroutines for assembler code output on the NS32000.
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

/* Some output-actions in ns32k.md need these.  */
#include <stdio.h>
extern FILE *asm_out_file;

#define FP_REG_P(X)  (GET_CODE (X) == REG && REGNO (X) > 7 && REGNO (X) < 16)

/* Generate the rtx that comes from an address expression in the md file */
/* The expression to be build is BASE[INDEX:SCALE].  To recognize this,
   scale must be converted from an exponent (from ASHIFT) to a
   muliplier (for MULT). */
rtx
gen_indexed_expr (base, index, scale)
     rtx base, index, scale;
{
  rtx addr;

  /* This generates an illegal addressing mode, if BASE is
     fp or sp.  This is handled by PRINT_OPERAND_ADDRESS.  */
  if (GET_CODE (base) != REG && GET_CODE (base) != CONST_INT)
    base = gen_rtx (MEM, SImode, base);
  addr = gen_rtx (MULT, SImode, index,
		  gen_rtx (CONST_INT, VOIDmode, 1 << INTVAL (scale)));
  addr = gen_rtx (PLUS, SImode, base, addr);
  return addr;
}

/* Return 1 if OP is a valid constant int. These can be modeless
   (void mode), so we do not mess with their modes.

   The main use of this function is as a predicate in match_operand
   expressions in the machine description.  */

int
const_int (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT);
}

/* Return 1 if OP is a valid operand of mode MODE.  This
   predicate rejects operands which do not have a mode
   (such as CONST_INT which are VOIDmode).  */
int
reg_or_mem_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return (GET_MODE (op) == mode
	  && (GET_CODE (op) == REG
	      || GET_CODE (op) == SUBREG
	      || GET_CODE (op) == MEM));
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[1]) == CONST_INT
      && INTVAL (operands[1]) <= 7
      && INTVAL (operands[1]) >= -8)
    return "movqd %1,%0";
  return "movd %1,%0";
}

char *
output_move_double (operands)
     rtx *operands;
{
  enum anon1 { REGOP, OFFSOP, POPOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
    optype0 = POPOP;
  else
    optype0 = RNDOP;

  if (REG_P (operands[1]))
    optype1 = REGOP;
  else if (CONSTANT_ADDRESS_P (operands[1])
	   || GET_CODE (operands[1]) == CONST_DOUBLE)
    optype1 = CNSTOP;
  else if (offsettable_memref_p (operands[1]))
    optype1 = OFFSOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)
    optype1 = POPOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

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
      if (CONSTANT_ADDRESS_P (operands[1]))
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

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */

  if (optype0 == POPOP || optype1 == POPOP)
    {
      output_asm_insn (singlemove_string (latehalf), latehalf);
      return singlemove_string (operands);
    }

  /* Not autodecrementing.  Do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  operands[0] = latehalf[0];
  operands[1] = latehalf[1];
  return singlemove_string (operands);
}

int
check_reg (oper, reg)
     rtx oper;
     int reg;
{
  register int i;

  if (oper == 0)
    return 0;
  switch (GET_CODE(oper))
    {
    case REG:
      return (REGNO(oper) == reg) ? 1 : 0;
    case MEM:
      return check_reg(XEXP(oper, 0), reg);
    case PLUS:
    case MULT:
      return check_reg(XEXP(oper, 0), reg) || check_reg(XEXP(oper, 1), reg);
    }
  return 0;
}

/* PRINT_OPERAND_ADDRESS is defined to call this function,
   which is easier to debug than putting all the code in
   a macro definition in tm-ns32k.h .  */

/* Nonzero if we have printed a base register.
   If zero, on some systems, it means `(sb)' must be printed.  */
int paren_base_reg_printed = 0;

print_operand_address (file, addr)
     register FILE *file;
     register rtx addr;
{
  register rtx reg1, reg2, breg, ireg;
  rtx offset;
  static char scales[] = { 'b', 'w', 'd', 0, 'q', };

 retry:
  switch (GET_CODE (addr))
    {
    case MEM:
      addr = XEXP (addr, 0);
      if (GET_CODE (addr) == REG)
	if (REGNO (addr) == STACK_POINTER_REGNUM)
	  { fprintf (file, "tos"); break; }
	else
	  { fprintf (file, "%s", reg_names[REGNO (addr)]); break; }
      else if (CONSTANT_P (addr))
	{ output_addr_const (file, addr); break; }
      else if (GET_CODE (addr) == MULT)
	{ fprintf (file, "@0"); ireg = addr; goto print_index; }
      else if (GET_CODE (addr) == MEM)
	{
	  addr = XEXP (addr, 0);
	  if (GET_CODE (addr) == PLUS)
	    {
	      offset = XEXP (addr, 1);
	      addr = XEXP (addr, 0);
	    }
	  else
	    {
	      offset = const0_rtx;
	    }
	  output_addr_const (file, offset);
	  fprintf (file, "(%s)", reg_names[REGNO (addr)]);
	  break;
	}

      if (GET_CODE (addr) != PLUS)
	abort ();

      goto retry;

    case REG:
      if (REGNO (addr) == STACK_POINTER_REGNUM)
	fprintf (file, "tos");
      else
	fprintf (file, "0(%s)", reg_names[REGNO (addr)]);
      break;

    case PRE_DEC:
    case POST_INC:
      fprintf (file, "tos");
      break;

    case MULT:
      fprintf (file, "@0");
      ireg = addr; /* [rX:Y] */
      goto print_index;
      break;

    case PLUS:
      reg1 = 0;	reg2 = 0;
      ireg = 0;	breg = 0;
      offset = const0_rtx;
      if (CONSTANT_ADDRESS_P (XEXP (addr, 0)))
	{
	  offset = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))
	{
	  offset = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      if (GET_CODE (addr) != PLUS) ;
      else if (GET_CODE (XEXP (addr, 0)) == MULT)
	{
	  reg1 = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (GET_CODE (XEXP (addr, 1)) == MULT)
	{
	  reg1 = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      /* The case for memory is somewhat tricky:  to get
	 a MEM here, the only RTX formats that could
	 get here are either (modulo commutativity)
	   (PLUS (PLUS (REG *MEM)) CONST) -or-
	   (PLUS (PLUS (CONST REG/MULT)) *MEM)
	 We take advantage of that knowledge here.  */
      else if (GET_CODE (XEXP (addr, 0)) == MEM
	       || GET_CODE (XEXP (addr, 1)) == MEM)
	{
	  rtx temp;

	  if (GET_CODE (XEXP (addr, 0)) == MEM)
	    {
	      temp = XEXP (addr, 1);
	      addr = XEXP (addr, 0);
	    }
	  else
	    {
	      temp = XEXP (addr, 0);
	      addr = XEXP (addr, 1);
	    }

	  if (GET_CODE (temp) == REG)
	    {
	      reg1 = temp;
	    }
	  else
	    {
	      if (GET_CODE (temp) != PLUS)
		abort ();

	      if (GET_CODE (XEXP (temp, 0)) == MULT)
		{
		  reg1 = XEXP (temp, 0);
		  offset = XEXP (temp, 1);
		}
	      if (GET_CODE (XEXP (temp, 1)) == MULT)
		{
		  reg1 = XEXP (temp, 1);
		  offset = XEXP (temp, 0);
		}
	      else
		abort ();
	    }
	}
      else if (GET_CODE (XEXP (addr, 0)) == REG
	       || GET_CODE (XEXP (addr, 1)) == REG)
	{
	  rtx temp;

	  if (GET_CODE (XEXP (addr, 0)) == REG)
	    {
	      temp = XEXP (addr, 0);
	      addr = XEXP (addr, 1);
	    }
	  else
	    {
	      temp = XEXP (addr, 1);
	      addr = XEXP (addr, 0);
	    }

	  if (GET_CODE (addr) == REG)
	    {
	      if (REGNO (temp) >= FRAME_POINTER_REGNUM)
		{ reg1 = addr; addr = temp; }
	      else
		{ reg1 = temp; }
	    }
	  else if (CONSTANT_P (addr))
	    {
	      if (GET_CODE (offset) == CONST_INT
		  && INTVAL (offset))
		offset = plus_constant (addr, INTVAL (offset));
	      addr = temp;
	    }
	  else if (GET_CODE (addr) != PLUS)
	    abort ();
	  else
	    {
	      if (CONSTANT_ADDRESS_P (XEXP (addr, 0)))
		{
		  offset = XEXP (addr, 0);
		  addr = XEXP (addr, 1);
		}
	      else if (CONSTANT_ADDRESS_P (XEXP (addr, 1)))
		{
		  offset = XEXP (addr, 1);
		  addr = XEXP (addr, 0);
		}
	      else abort ();

	      if (GET_CODE (addr) == REG)
		{
		  if (REGNO (temp) >= FRAME_POINTER_REGNUM)
		    { reg1 = addr; addr = temp; }
		  else
		    { reg1 = temp; }
		}
	      else
		reg1 = temp;
	    }
	}

      if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT)
	{ if (reg1 == 0) reg1 = addr; else reg2 = addr; addr = 0; }
      if (addr != 0)
	{
	  if (CONSTANT_P (addr) && reg1)
	    {
	      /* OFFSET comes second, to prevent outputting
		 operands of the form INT+SYMBOL+INT.
		 The Genix assembler dies on them.  */
	      output_addr_const (file, addr);
	      if (offset != const0_rtx)
		{
		  putc ('+', file);
		  output_addr_const (file, offset);
		}
	      ireg = reg1;
	      goto print_index;
	    }
	  else if (GET_CODE (addr) != MEM)
	    abort ();

	  output_addr_const (file, offset);
#ifndef SEQUENT_ADDRESS_BUG
	  putc ('(', file);
	  paren_base_reg_printed = 0;
	  output_address (addr);
#ifdef SEQUENT_BASE_REGS
	  if (!paren_base_reg_printed)
	    fprintf (file, "(sb)");
#endif
	  putc (')', file);
#else /* SEQUENT_ADDRESS_BUG */
	  if ((GET_CODE (offset) == SYMBOL_REF
	       || GET_CODE (offset) == CONST)
	      && GET_CODE (addr) == REG)
	    {
	      if (reg1) abort ();
	      fprintf (file, "[%s:b]", reg_names[REGNO (addr)]);
	    }
	  else
	    {
	      putc ('(', file);
	      paren_base_reg_printed = 0;
	      output_address (addr);
#ifdef SEQUENT_BASE_REGS
	      if (!paren_base_reg_printed)
	        fprintf (file, "(sb)");
#endif
	      putc (')', file);
	    }
#endif /* SEQUENT_ADDRESS_BUG */
	  ireg = reg1;
	  goto print_index;
	}
      else addr = offset;
      if (reg1 && GET_CODE (reg1) == MULT)
	{ breg = reg2; ireg = reg1; }
      else if (reg2 && GET_CODE (reg2) == MULT)
	{ breg = reg1; ireg = reg2; }
      else if (reg2 || GET_CODE (addr) == MEM)
	{ breg = reg2; ireg = reg1; }
      else
	{ breg = reg1; ireg = reg2; }
      if (ireg != 0 && breg == 0 && GET_CODE (addr) == LABEL_REF)
        {
	  int scale;
	  if (GET_CODE (ireg) == MULT)
	    {
	      scale = INTVAL (XEXP (ireg, 1)) >> 1;
	      ireg = XEXP (ireg, 0);
	    }
	  else scale = 0;
	  output_asm_label (addr);
	  fprintf (file, "[%s:%c]",
		   reg_names[REGNO (ireg)], scales[scale]);
	  break;
	}
      if (ireg && breg && offset == const0_rtx)
	fprintf (file, "0(%s)", reg_names[REGNO (breg)]);
      else
	{
	  if (addr != 0)
	    {
	      if (ireg != 0 && breg == 0
		  && GET_CODE (offset) == CONST_INT) putc('@', file);
	      output_addr_const (file, offset);
	    }
	  if (breg != 0)
	    {
	      if (GET_CODE (breg) != REG) abort ();
#ifndef SEQUENT_ADDRESS_BUG
	      fprintf (file, "(%s)", reg_names[REGNO (breg)]);
	      paren_base_reg_printed = -1;
#else
	      if (GET_CODE (offset) == SYMBOL_REF || GET_CODE (offset) == CONST)
		{
		  if (ireg) abort ();
		  fprintf (file, "[%s:b]", reg_names[REGNO (breg)]);
		}
	      else
		{
		  fprintf (file, "(%s)", reg_names[REGNO (breg)]);
		  paren_base_reg_printed = -1;
		}
#endif
	    }
	}
  print_index:
      if (ireg != 0)
	{
	  int scale;
	  if (GET_CODE (ireg) == MULT)
	    {
	      scale = INTVAL (XEXP (ireg, 1)) >> 1;
	      ireg = XEXP (ireg, 0);
	    }
	  else scale = 0;
	  if (GET_CODE (ireg) != REG) abort ();
	  fprintf (file, "[%s:%c]",
		   reg_names[REGNO (ireg)],
		   scales[scale]);
	}
      break;
    default:
      output_addr_const (file, addr);
    }
}

/* National 32032 shifting is so bad that we can get
   better performance in many common cases by using other
   techniques.  */
char *
output_shift_insn (operands)
     rtx *operands;
{
  if (GET_CODE (operands[2]) == CONST_INT
      && INTVAL (operands[2]) > 0
      && INTVAL (operands[2]) <= 3)
    if (GET_CODE (operands[0]) == REG)
      {
	if (GET_CODE (operands[1]) == REG)
	  {
	    if (REGNO (operands[0]) == REGNO (operands[1]))
	      {
		if (operands[2] == const1_rtx)
		  return "addd %0,%0";
		else if (INTVAL (operands[2]) == 2)
		  return "addd %0,%0\n\taddd %0,%0";
	      }
	    if (operands[2] == const1_rtx)
	      return "movd %1,%0\n\taddd %0,%0";

	    operands[1] = gen_indexed_expr (const0_rtx, operands[1], operands[2]);
	    return "addr %a1,%0";
	  }
	if (operands[2] == const1_rtx)
	  return "movd %1,%0\n\taddd %0,%0";
      }
    else if (GET_CODE (operands[1]) == REG)
      {
	operands[1] = gen_indexed_expr (const0_rtx, operands[1], operands[2]);
	return "addr %a1,%0";
      }
    else if (INTVAL (operands[2]) == 1
	     && GET_CODE (operands[1]) == MEM
	     && rtx_equal_p (operands [0], operands[1]))
      {
	rtx temp = XEXP (operands[1], 0);

	if (GET_CODE (temp) == REG
	    || (GET_CODE (temp) == PLUS
		&& GET_CODE (XEXP (temp, 0)) == REG
		&& GET_CODE (XEXP (temp, 1)) == CONST_INT))
	  return "addd %0,%0";
      }
    else return "ashd %2,%0";
  return "ashd %2,%0";
}
