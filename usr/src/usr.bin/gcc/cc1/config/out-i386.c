/*-
 *
 * This code is derived from software copyrighted by the Free Software
 * Foundation.
 *
 * Modified 1991 by Donn Seeley at UUNET Technologies, Inc.
 */

#ifndef lint
static char sccsid[] = "@(#)out-i386.c	6.4 (Berkeley) 5/8/91";
#endif /* not lint */

/* Subroutines for insn-output.c for Intel 80386.
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

#ifndef FILE
#include <stdio.h>
#endif

#define FP_TOP (gen_rtx(REG, DFmode, FIRST_FLOAT_REG))

#define AT_SP(mode) (gen_rtx (MEM, (mode), stack_pointer_rtx))
#define AT_BP(mode) (gen_rtx (MEM, (mode), frame_pointer_rtx))

#define RET return ""

/* #define RETCOM(X) fprintf (asm_out_file, "%sX fp_pop_level=%d\n", \
			   COMMENT_BEGIN, fp_pop_level); RET */
#define RETCOM(X) return ""

#define POP_ONE_FP	\
  { /* fp_pop_level--; */	\
    fprintf (asm_out_file, "\tfstp %sst(0)\n", RP); }

extern FILE *asm_out_file;
static char *singlemove_string ();
static void output_movf ();
static void replace_float_constant ();
static int mentions_fp_top ();
static int call_top_dead_p ();
static int fp_top_dead_p1 ();
static rtx via_memory ();
static void output_asm_insn_double_reg_op ();

/* All output functions must increment or decrement this to indicate
   the net number of pops or pushes which they perform.  Note that it won't
   necessarily balance with the optimize running, since we might have
   two different calls with the same pop shared by cross jumping.
   However on optimize the reg dead heuristic seems to work.  */

int fp_pop_level = 0;

static char *hi_reg_name[] = HI_REGISTER_NAMES;
static char *qi_reg_name[] = QI_REGISTER_NAMES;

/* for fabs, fch, .. where the argument operand[1] must first be moved to
  constraints  "=fm" "0" */

#define FP_CALL1(op)  \
  { if (FP_REG_P (operands[0]))		\
      return op;			\
    output_movf (FP_TOP, operands[1]);	\
    output_asm_insn (op, operands);	\
    /* fp_pop_level--; */		\
    return "fstp%z0 %0"; }

/* handle case of call where op0/op1 is "=mf" and opn is "mrf"
   eg. fadd */
#define FP_CALL(op, rev, n)  \
  return fp_call_internal (op, rev, n, operands, insn);

static char *
fp_call_internal (op, rev, n, operands, insn)
     char *op;
     char *rev;
     int n;
     rtx *operands;
     rtx insn;
{
  if (!FP_REG_P (operands[0]))
    {
      /* Here destination is in memory
	 and source is in the fp stack.  */
      output_movf (FP_TOP, operands[0]);
      output_asm_insn_double_reg_op (op, rev, insn);
      return "fstp%z0 %0";
    }

  if (FP_REG_P (operands[n]))
    {
      rtx temp = operands[1];
      char *tem1 = op;
      operands[1] = operands[n];
      op = rev;
      operands[n] = temp;
      rev = tem1;
    }

  if (REG_P (operands[n]))
    {
      rtx xops[2];
      via_memory (operands[n]);
      operands[n] = AT_SP (GET_MODE (operands[n]));
      xops[0] = stack_pointer_rtx;
      xops[1] = gen_rtx (CONST_INT, VOIDmode,
			 GET_MODE_SIZE (GET_MODE (operands[n])));
      output_asm_insn (op, operands + n);
      output_asm_insn (AS2 (add%L0,%1,%0), xops);
    }
  else
    output_asm_insn (op, operands + n);

  if (FP_REG_P (operands[0]))
    {
      /* It turns out not to work to use top_dead_p because
	 the death notes are not accurate enough.
	 But this ought to work, because the only thing that can
	 live across basic blocks is reg 8, and these insns
	 never involve reg 8 directly.  */
      if (fp_top_dead_p1 (insn))
	POP_ONE_FP;
    }

  RET;
}

/* Output assembler code to perform insn OP
   with two stack operands, and output on the stack.

   REV is the assembler insn that does the same thing but
   effectively interchanges the meanings of the two arguments.

   Somewhat counterintuitively, the "first" operand was pushed last.

   The output replaces either the top-of-stack or both of the arguments,
   depending on whether the other argument is wanted after this insn.  */

static void
output_asm_insn_double_reg_op (op, rev, insn)
     char *op;
     char *rev;
     rtx insn;
{
  fputc ('\t', asm_out_file);
  if (top_dead_p (insn))
    {
      /* Here we want the "reversed" insn, fsubr or fdivr.
	 But there is an assembler bug in all 80386 assemblers
	 which exchanges the meanings of fsubr and fsub, and of fdivr and fdiv!
	 So use the "unreversed" opcode (which will assemble into
	 the "reversed" insn).  */
      rev = op;

      while (*rev && *rev != '%')
	fputc (*rev++, asm_out_file);
      /* fp_pop_level--; */

      fprintf (asm_out_file, AS2 (p,%sst,%sst(1)), RP, RP);
    }
  else
    {
      while (*op && *op != '%')
	fputc (*op++, asm_out_file);
      fprintf (asm_out_file,AS2 ( ,%sst(1),%sst), RP, RP);
    }
  putc ('\n', asm_out_file);
}

/* Moves X to memory location 8 below stack pointer
   and returns an RTX for that memory location.
   X should be a register, in DFmode or SFmode.  */

static rtx
via_memory (x)
     rtx x;
{
  if (!REG_P (x))
    abort ();
  if (GET_MODE (x) == DFmode)
    {
      rtx xops[1];
      xops[0] = gen_rtx (REG, SImode, REGNO (x) + 1);
      output_asm_insn ("push%L0 %0", xops);
    }
  output_asm_insn ("push%L0 %0", &x);
}

/* Output an insn to copy the SFmode value in fp0 to OPERAND
   without clobbering fp0.  */

void
fp_store_sf (target)
     rtx target;
{
  if (REG_P (target))
    {
      rtx xoperands[3];
      xoperands[0] = stack_pointer_rtx;
      xoperands[1] = AT_SP (Pmode);
      xoperands[2] = gen_rtx (CONST_INT, VOIDmode, -4);
      output_asm_insn (AS2 (add%L0,%2,%0), xoperands);
      output_asm_insn ("fst%S0 %1", xoperands);
      output_asm_insn ("pop%L0 %0", &target);
    }
  else if (GET_CODE (target) == MEM)
    output_asm_insn ("fst%S0 %0", &target);
}

/* Output an insn to pop an SF value from fp0 into TARGET.
   This destroys the value of fp0.  */

void
fp_pop_sf (target)
     rtx target;
{
  if (REG_P (target))
    {
      rtx xoperands[3];
      xoperands[0] = stack_pointer_rtx;
      xoperands[1] = AT_SP (Pmode);
      xoperands[2] = gen_rtx (CONST_INT, VOIDmode, -4);
      output_asm_insn (AS2 (add%L0,%2,%0), xoperands);
      output_asm_insn ("fstp%S0 %1", xoperands);
      output_asm_insn ("pop%L0 %0", &target);
      /* fp_pop_level--; */
    }
  else if (GET_CODE (target) == MEM)
    {
      /* fp_pop_level--; */
      output_asm_insn ("fstp%S0 %0", &target);
    }
  else abort ();
}

/* Copy the top of the fpu stack into TARGET, without popping.  */

void
fp_store_df (target)
     rtx target;
{
  if (REG_P (target))
    {
      rtx xoperands[4];
      xoperands[0] = stack_pointer_rtx;
      xoperands[1] = gen_rtx (REG, SImode, REGNO (target) + 1);
      xoperands[2] = AT_SP (Pmode);
      xoperands[3] = gen_rtx (CONST_INT, VOIDmode, -8);
      output_asm_insn (AS2 (add%L0,%3,%0), xoperands);
      output_asm_insn ("fst%Q0 %2", xoperands);
      output_asm_insn ("pop%L0 %0", &target);
      output_asm_insn ("pop%L0 %1", xoperands);
    }
  else if (GET_CODE (target) == MEM)
    output_asm_insn ("fst%Q0 %0", &target);
}

/* Copy the top of the fpu stack into TARGET, with popping.  */

void
fp_pop_df (target)
     rtx target;
{
  if (REG_P (target))
    {
      rtx xoperands[4];
      xoperands[0] = stack_pointer_rtx;
      xoperands[1] = gen_rtx (REG, SImode, REGNO (target) + 1);
      xoperands[2] = AT_SP (Pmode);
      xoperands[3] = gen_rtx (CONST_INT, VOIDmode, -8);
      output_asm_insn (AS2 (add%L0,%3,%0), xoperands);
      /* fp_pop_level--; */
      output_asm_insn ("fstp%Q0 %2", xoperands);
      output_asm_insn ("pop%L0 %0", &target);
      output_asm_insn ("pop%L0 %1", xoperands);
    }
  else if (GET_CODE (target) == MEM)
    {
      /* fp_pop_level--; */
      output_asm_insn ("fstp%z0 %0", &target);
    }
}

#if 0
/* Pop the fp stack, convert value to integer and store in TARGET.
   TARGET may be memory or register, and may have QI, HI or SImode.  */

void
fp_pop_int (target)
     rtx target;
{
  if (REG_P (target) || GET_MODE (target) != SImode)
    {
      rtx xxops[2];
      xxops[0] = stack_pointer_rtx;
      xxops[1] = gen_rtx (CONST_INT, VOIDmode, 4);
      output_asm_insn (AS2 (sub%L0,%1,%0), xxops);
      xxops[0] = AT_SP (Pmode);
      /* fp_pop_level--; */
      output_asm_insn ("fistp%L0 %0", xxops);
      output_asm_insn ("pop%L0 %0", &target);
    }
  else if (GET_CODE (target) == MEM)
    {
      /* fp_pop_level--; */
      output_asm_insn ("fistp%L0 %0", &target);
    }
  else abort ();
}
#endif

/* Push the SFmode value X onto the fpu stack.  */

void
fp_push_sf (x)
     rtx x;
{
  /* fp_pop_level++; */
  if (REG_P (x))
    {
      rtx xoperands[2];
      rtx xfops[3];
      output_asm_insn ("push%L0 %0", &x);
      xfops[0] = AT_SP (Pmode);
      xfops[2] = gen_rtx (CONST_INT, VOIDmode, 4);
      xfops[1] = stack_pointer_rtx;
      output_asm_insn ("fld%S0 %0\n\tadd%L0 %2,%1", xfops);
    }
  else
    output_asm_insn ("fld%S0 %0", &x);
}

/* Push the DFmode value X onto the fpu stack.  */

void
fp_push_df (x)
     rtx x;
{
  /* fp_pop_level++; */

  if (REG_P (x))
    {
      rtx xoperands[2];
      rtx xfops[3];
      xoperands[0] = x;
      xoperands[1] = gen_rtx (REG, SImode, REGNO (x) + 1);
      output_asm_insn ("push%L0 %1", xoperands);
      output_asm_insn ("push%L0 %0", xoperands);
      xfops[0] = AT_SP (Pmode);
      xfops[2] = gen_rtx (CONST_INT, VOIDmode, 8);
      xfops[1] = stack_pointer_rtx;
      output_asm_insn ("fld%Q0 %0\n\tadd%L0 %2,%1", xfops);
    }
  else if (GET_CODE (x) == MEM)
    output_asm_insn ("fld%Q0 %0", &x);
}

static char *output_move_const_single ();

static char *
singlemove_string (operands)
     rtx *operands;
{
  rtx x;
  if (GET_CODE (operands[0]) == MEM
      && GET_CODE (x = XEXP (operands[0], 0)) == PRE_DEC)
    {
      if (XEXP (x, 0) != stack_pointer_rtx)
	abort ();
      return "push%L0 %1";
    }
  else if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      return output_move_const_single (operands);
    }
  else if (GET_CODE (operands[0]) == REG || GET_CODE (operands[1]) == REG)
    return AS2 (mov%L0,%1,%0);
  else if (CONSTANT_P (operands[1]))
    return AS2 (mov%L0,%1,%0);
  else
    {
      output_asm_insn ("push%L0 %1", operands);
      return "pop%L0 %0";
    }
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

/* Output an insn to add the constant N to the register X.  */

static void
asm_add (n, x)
     int n;
     rtx x;
{
  rtx xops[2];
  xops[1] = x;
  if (n < 0)
    {
      xops[0] = gen_rtx (CONST_INT, VOIDmode, -n);
      output_asm_insn (AS2 (sub%L0,%0,%1), xops);
    }
  else if (n > 0)
    {
      xops[0] = gen_rtx (CONST_INT, VOIDmode, n);
      output_asm_insn (AS2 (add%L0,%0,%1), xops);
    }
}

/* Output assembler code to perform a doubleword move insn
   with operands OPERANDS.  */

char *
output_move_double (operands)
     rtx *operands;
{
  enum {REGOP, OFFSOP, MEMOP, PUSHOP, POPOP, CNSTOP, RNDOP } optype0, optype1;
  rtx latehalf[2];
  rtx addreg0 = 0, addreg1 = 0;

  /* First classify both operands.  */

  if (REG_P (operands[0]))
    optype0 = REGOP;
  else if (offsettable_memref_p (operands[0]))
    optype0 = OFFSOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == POST_INC)
    optype0 = POPOP;
  else if (GET_CODE (XEXP (operands[0], 0)) == PRE_DEC)
    optype0 = PUSHOP;
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
  else if (GET_CODE (XEXP (operands[1], 0)) == POST_INC)
    optype1 = POPOP;
  else if (GET_CODE (XEXP (operands[1], 0)) == PRE_DEC)
    optype1 = PUSHOP;
  else if (GET_CODE (operands[1]) == MEM)
    optype1 = MEMOP;
  else
    optype1 = RNDOP;

  /* Check for the cases that the operand constraints are not
     supposed to allow to happen.  Abort if we get one,
     because generating code for these cases is painful.  */

  if (optype0 == RNDOP || optype1 == RNDOP)
    abort ();

  /* If one operand is decrementing and one is incrementing
     decrement the former register explicitly
     and change that operand into ordinary indexing.  */

  if (optype0 == PUSHOP && optype1 == POPOP)
    {
      operands[0] = XEXP (XEXP (operands[0], 0), 0);
      asm_add (-8, operands[0]);
      operands[0] = gen_rtx (MEM, DImode, operands[0]);
      optype0 = OFFSOP;
    }
  if (optype0 == POPOP && optype1 == PUSHOP)
    {
      operands[1] = XEXP (XEXP (operands[1], 0), 0);
      asm_add (-8, operands[1]);
      operands[1] = gen_rtx (MEM, DImode, operands[1]);
      optype1 = OFFSOP;
    }

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

  /* If insn is effectively movd N (sp),-(sp) then we will do the
     high word first.  We should use the adjusted operand 1 (which is N+4 (sp))
     for the low word as well, to compensate for the first decrement of sp.  */
  if (optype0 == PUSHOP
      && REGNO (XEXP (XEXP (operands[0], 0), 0)) == STACK_POINTER_REGNUM
      && reg_overlap_mentioned_p (stack_pointer_rtx, operands[1]))
    operands[1] = latehalf[1];

  /* If one or both operands autodecrementing,
     do the two words, high-numbered first.  */

  /* Likewise,  the first move would clobber the source of the second one,
     do them in the other order.  This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance.  */

  if (optype0 == PUSHOP || optype1 == PUSHOP
      || (optype0 == REGOP && optype1 == REGOP
	  && REGNO (operands[0]) == REGNO (latehalf[1])))
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	asm_add (4, addreg0);
      if (addreg1)
	asm_add (4, addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
         asm_add (-4, addreg0);
      if (addreg1)
	asm_add (-4, addreg1);

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    asm_add (4, addreg0);
  if (addreg1)
    asm_add (4, addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    asm_add (-4, addreg0);
  if (addreg1)
    asm_add (-4, addreg1);

  return "";
}

int
standard_80387_constant_p (x)
     rtx x;
{
  union { double d; int i[2];} u;
  register double d;
  u.i[0] = XINT (x, 0);
  u.i[1] = XINT (x, 1);
  d = u.d;

  if (d == 0)
    return 1;
  if (d == 1)
    return 2;
  /* Note that on the 80387, other constants, such as pi,
     are much slower to load as standard constants
     than to load from doubles in memory!  */

  return 0;
}

static char *
output_move_const_double (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      int conval = standard_80387_constant_p (operands[1]);

      /* fp_pop_level++; */
      if (conval == 1)
	return "fldz";
      if (conval == 2)
	return "fld1";
      /* fp_pop_level--; */
    }

  output_move_double (operands);
}


static char *
output_move_const_single (operands)
     rtx *operands;
{
  if (FP_REG_P (operands[0]))
    {
      int conval = standard_80387_constant_p (operands[1]);

      /* fp_pop_level++; */
      if (conval == 1)
	return "fldz";
      if (conval == 2)
	return "fld1";
      /* fp_pop_level--; */
    }
  if (GET_CODE (operands[1]) == CONST_DOUBLE)
    {
      union { int i[2]; double d;} u1;
      union { int i; float f;} u2;
      u1.i[0] = CONST_DOUBLE_LOW (operands[1]);
      u1.i[1] = CONST_DOUBLE_HIGH (operands[1]);
      u2.f = u1.d;
      operands[1] = gen_rtx (CONST_INT, VOIDmode, u2.i);
    }
  return singlemove_string (operands);
}

/* Output an insn to move an SF value from FROM to TO.
   The kinds of operands are not restricted
   except that they may not both be in memory.  */

void
output_movsf (to, from)
     rtx from, to;
{
  rtx xops[2];
  xops[0] = to;
  xops[1] = from;
  if (FP_REG_P (from) || FP_REG_P (to))
    {
      from = xops[1];
    }

  if (FP_REG_P (from))
    {
#if 0
	{
	  if (REGNO (from) != REGNO (to))
	    {
	      output_asm_insn ("fld%S0 %1\n\tfstp%S0 %0", xops);
	    }
	}
      else
#endif

      if (! FP_REG_P (to))
	fp_pop_sf (to);
    }
  else if (FP_REG_P (to))
    fp_push_sf (from);
  else
    output_asm_insn (singlemove_string (xops), xops);
}

/* Output an insn to move a DF value from FROM to TO.
   The kinds of operands are not restricted
   except that they may not both be in memory.  */

void
output_movdf (to, from)
     rtx from, to;
{
  rtx xops[2];
  xops[0] = to;
  xops[1] = from;
  if (FP_REG_P (from) || FP_REG_P (to))
    {
      from = xops[1];
      to = xops[0];
    }
  if (FP_REG_P (from))
    {
#if 0
	{
	  if (REGNO (from) != REGNO (to))
	    abort ();
/*	    output_asm_insn ("fld%Q0 %1\n\t fstp%Q0 %0", xops);*/
	}
      else
	{
#endif
      if (! FP_REG_P (to))
	fp_pop_df (to);
    }
  else if (FP_REG_P (to))
    fp_push_df (from);
  else
    output_asm_insn (output_move_double (xops), xops);
}

/* does move of FROM to TO where the mode is the minimum of the
two */

static void
output_movf (to, from)
     rtx to, from;
{
  if (GET_MODE (from) == SFmode || GET_MODE (to) == SFmode)
    output_movsf (to, from);
  else
    output_movdf (to, from);
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

void
function_prologue (file, size)
     FILE *file;
     int size;
{
  register int regno;
  int nregs, limit;
  rtx xops[4];
  extern int frame_pointer_needed;

  /* fp_pop_level = 0; */
  xops[0] = stack_pointer_rtx;
  xops[1] = frame_pointer_rtx;
  xops[2] = gen_rtx (CONST_INT, VOIDmode, size);
  if (frame_pointer_needed)
    {
      output_asm_insn ("push%L0 %1", xops);
      output_asm_insn (AS2 (mov%L0,%0,%1), xops);
      if (size)
	output_asm_insn (AS2 (sub%L0,%2,%0), xops);
    }

  /* Note If use enter it is NOT reversed args.
     This one is not reversed from intel!!
     I think enter is slower.  Also sdb doesn't like it.
     But if you want it the code is:
     {
     xops[3] = const0_rtx;
     output_asm_insn ("enter %2,%3", xops);
     }
     */
  nregs = 0;
  limit = (frame_pointer_needed ? FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);
  for (regno = limit - 1; regno >= 0; regno--)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      {
	fprintf (file, "\tpush%s %se%s\n", L_SIZE, RP, hi_reg_name[regno]);
      }
}

void
function_epilogue (file, size)
     FILE *file;
     int size;
{
  register int regno;
  register int nregs, limit;
  int assure_sp_pos;
  int return_struct_adjust;
  extern int frame_pointer_needed;
  extern int current_function_pops_args;
  extern int current_function_args_size;
  extern int flag_pcc_struct_return;

  limit = (frame_pointer_needed ? FRAME_POINTER_REGNUM : STACK_POINTER_REGNUM);
  nregs = 0;

  return_struct_adjust =
    (current_function_returns_struct
#ifdef STRUCT_RETURN_CALLER_POP
     && !flag_pcc_struct_return
#endif
     ? 4 : 0);

  for (regno = (limit -1); regno >= 0; regno--)
    if (regs_ever_live[regno] && ! call_used_regs[regno])
      nregs++;

  /* sp is often  unreliable so we must go off the frame pointer,
   */

  if (nregs && frame_pointer_needed)
    {
      rtx xops[2];
      xops[0] = adj_offsettable_operand (AT_BP (Pmode),
					 -size -(nregs*(UNITS_PER_WORD)));
      xops[1] = stack_pointer_rtx;
      output_asm_insn (AS2 (lea%L0,%0,%1), xops);
    }
  for (regno = 0; regno < limit; regno++)
    {
      if (regs_ever_live[regno] && ! call_used_regs[regno])
	{
	  fprintf (file, "\tpop%s ", L_SIZE);
	  fprintf (file, "%se%s\n", RP, hi_reg_name[regno]);
	}
    }

  if (frame_pointer_needed)
    fprintf (file, "\tleave\n");
  if (current_function_pops_args && current_function_args_size)
    fprintf (file, "\tret %s%d\n", IP,
	     (current_function_args_size + return_struct_adjust));
  else if (return_struct_adjust)
    fprintf (file, "\tret %s%d\n", IP, return_struct_adjust);
  else
    fprintf (file, "\tret\n");
}

int
hard_regno_mode_ok (regno, mode)
     int regno;
     enum machine_mode mode;
{
  return
    (regno < 2 ? 1
     /* Used to reject floating modes here */
     : regno < 4 ? 1
     : regno >= 8 ? mode == DFmode || mode == SFmode
     : mode != QImode);
}

/* Print the name of a register based on its machine mode and number.
   If CODE is 'w', pretend the mode is HImode.
   If CODE is 'b', pretend the mode is QImode.
   If CODE is 'k', pretend the mode is SImode.  */

#define PRINT_REG(X, CODE, FILE) \
  do { fprintf (FILE, "%s", RP);			\
       switch ((CODE == 'w' ? 2 			\
		: CODE == 'b' ? 1			\
		: CODE == 'k' ? 4			\
		: GET_MODE_SIZE (GET_MODE (X))))	\
	 {						\
	 case 4:					\
	 case 8:					\
	   if (!FP_REG_P (X)) fputs ("e", FILE);	\
	 case 2:					\
	   fputs (hi_reg_name[REGNO (X)], FILE);	\
	   break;					\
	 case 1:					\
	   fputs (qi_reg_name[REGNO (X)], FILE);	\
	   break;					\
	 }						\
     } while (0)

/* Meaning of CODE:
   f -- float insn (print a CONST_DOUBLE as a float rather than in hex).
   L,W,B,Q,S -- print the opcode suffix for specified size of operand.
   R -- print the prefix for register names.
   z -- print the opcode suffix for the size of the current operand.
   * -- print a star (in certain assembler syntax)
   w -- print the operand as if it's a "word" (HImode) even if it isn't.
   c -- don't print special prefixes before constant operands.
*/

void
print_operand (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  if (code)
    {
      switch (code)
	{
	case '*':
	  if (USE_STAR)
	    putc ('*', file);
	  return;

	case 'L':
	  PUT_OP_SIZE (code, 'l', file);
	  return;

	case 'W':
	  PUT_OP_SIZE (code, 'w', file);
	  return;

	case 'B':
	  PUT_OP_SIZE (code, 'b', file);
	  return;

	case 'Q':
	  PUT_OP_SIZE (code, 'l', file);
	  return;

	case 'S':
	  PUT_OP_SIZE (code, 's', file);
	  return;

	case 'R':
	  fprintf (file, "%s", RP);
	  return;

	case 'z':
	  /* this is the size of op from size of operand */
	  switch (GET_MODE_SIZE (GET_MODE (x)))
	    {
	    case 2:
	      PUT_OP_SIZE ('W', 'w', file);
	      return;
	    case 4:
	      if (GET_MODE (x) == SFmode)
		{
		  PUT_OP_SIZE ('S', 's', file);
		  return;
		}
	      else
		PUT_OP_SIZE ('L', 'l', file);
	      return;
	    case 8:
	      if (!FP_REG_P (x)) PUT_OP_SIZE ('Q', 'l', file);
	      return;
	    case 1:
	      PUT_OP_SIZE ('B', 'b', file);
	      return;
	    }
	}
    }
  if (GET_CODE (x) == REG)
    {
      PRINT_REG (x, code, file);
    }
  else if (GET_CODE (x) == MEM)
    {
      PRINT_PTR (x, file);
      if (CONSTANT_ADDRESS_P (XEXP (x, 0)))
	output_addr_const (file, XEXP (x, 0));
      else
	output_address (XEXP (x, 0));
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == SFmode)
    {
      union { double d; int i[2]; } u;
      union { float f; int i; } u1;
      u.i[0] = CONST_DOUBLE_LOW (x);
      u.i[1] = CONST_DOUBLE_HIGH (x);
      u1.f = u.d;
      if (code == 'f')
        fprintf (file, "%.22e", u1.f);
      else
        {
	  PRINT_IMMED_PREFIX (file);
	  fprintf (file, "0x%x", u1.i);
	}
    }
  else if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == DFmode)
    {
      union { double d; int i[2]; } u;
      u.i[0] = CONST_DOUBLE_LOW (x);
      u.i[1] = CONST_DOUBLE_HIGH (x);
      fprintf (file, "%.22e", u.d);
    }
  else 
    {
      if (code != 'c')
	{
	  if (GET_CODE (x) == CONST_INT)
	    PRINT_IMMED_PREFIX (file);
	  else if (GET_CODE (x) == CONST || GET_CODE (x) == SYMBOL_REF)
	    PRINT_OFFSET_PREFIX (file);
	}
      output_addr_const (file, x);
    }
}

/* Print a memory operand whose address is ADDR.  */

void
print_operand_address (file, addr)
     FILE *file;
     register rtx addr;
{
  register rtx reg1, reg2, breg, ireg;
  rtx offset;

  switch (GET_CODE (addr))
    {
    case REG:
      ADDR_BEG (file);
      fprintf (file, "%se", RP);
      fputs (hi_reg_name[REGNO (addr)], file);
      ADDR_END (file);
      break;

    case PLUS:
      reg1 = 0;
      reg2 = 0;
      ireg = 0;
      breg = 0;
      offset = 0;
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
      else if (GET_CODE (XEXP (addr, 0)) == REG)
	{
	  reg1 = XEXP (addr, 0);
	  addr = XEXP (addr, 1);
	}
      else if (GET_CODE (XEXP (addr, 1)) == REG)
	{
	  reg1 = XEXP (addr, 1);
	  addr = XEXP (addr, 0);
	}
      if (GET_CODE (addr) == REG || GET_CODE (addr) == MULT)
	{
	  if (reg1 == 0) reg1 = addr;
	  else reg2 = addr;
	  addr = 0;
	}
      if (offset != 0)
	{
	  if (addr != 0) abort ();
	  addr = offset;
	}
      if ((reg1 && GET_CODE (reg1) == MULT)
	  || (reg2 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg2))))
	{
	  breg = reg2;
	  ireg = reg1;
	}
      else if (reg1 != 0 && REGNO_OK_FOR_BASE_P (REGNO (reg1)))
	{
	  breg = reg1;
	  ireg = reg2;
	}

      if (ireg != 0 || breg != 0)
	{
	  int scale = 1;

	  if (addr != 0)
	    {
	      if (GET_CODE (addr) == LABEL_REF)
		output_asm_label (addr);
	      else
		output_addr_const (file, addr);
	    }

  	  if (ireg != 0 && GET_CODE (ireg) == MULT)
	    {
	      scale = INTVAL (XEXP (ireg, 1));
	      ireg = XEXP (ireg, 0);
	    }
	  /* output breg+ireg*scale */
	  PRINT_B_I_S (breg, ireg, scale, file);
	  break;
	}

    case MULT:
      {
	int scale;
	if (GET_CODE (XEXP (addr, 0)) == CONST_INT)
	  {
	    scale = INTVAL (XEXP (addr, 0));
	    ireg = XEXP (addr, 1);
	  }
	else
	  {
	    scale = INTVAL (XEXP (addr, 1));
	    ireg = XEXP (addr, 0);
	  }
	output_addr_const (file, const0_rtx);
	PRINT_B_I_S ((rtx) 0, ireg, scale, file);
      }
      break;

    default:
      if (GET_CODE (addr) == CONST_INT
	  && INTVAL (addr) < 0x8000
	  && INTVAL (addr) >= -0x8000)
	fprintf (file, "%d", INTVAL (addr));
      else
	output_addr_const (file, addr);
    }
}

/* Set the cc_status for the results of an insn whose pattern is EXP.
   On the 80386, we assume that only test and compare insns, as well
   as SI, HI, & DI mode ADD, SUB, NEG, AND, IOR, XOR, ASHIFT, LSHIFT,
   ASHIFTRT, and LSHIFTRT instructions set the condition codes usefully.
   Also, we assume that jumps and moves don't affect the condition codes.
   All else, clobbers the condition codes, by assumption.

   We assume that ALL add, minus, etc. instructions effect the condition
   codes.  This MUST be consistent with i386.md.  */

notice_update_cc (exp)
     rtx exp;
{
  if (GET_CODE (exp) == SET)
    {
      /* Jumps do not alter the cc's.  */
      if (SET_DEST (exp) == pc_rtx)
	return;
      /* Moving register or memory into a register:
	 it doesn't alter the cc's, but it might invalidate
	 the RTX's which we remember the cc's came from.
	 (Note that moving a constant 0 or 1 MAY set the cc's).  */
      if (REG_P (SET_DEST (exp))
	  && (REG_P (SET_SRC (exp)) || GET_CODE (SET_SRC (exp)) == MEM))
	{
	  if (cc_status.value1
	      && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value1))
	    cc_status.value1 = 0;
	  if (cc_status.value2
	      && reg_overlap_mentioned_p (SET_DEST (exp), cc_status.value2))
	    cc_status.value2 = 0;
	  return;
	}
      /* Moving register into memory doesn't alter the cc's.
	 It may invalidate the RTX's which we remember the cc's came from.  */
      if (GET_CODE (SET_DEST (exp)) == MEM && REG_P (SET_SRC (exp)))
	{
	  if (cc_status.value1 && GET_CODE (cc_status.value1) == MEM)
	    cc_status.value1 = 0;
	  if (cc_status.value2 && GET_CODE (cc_status.value2) == MEM)
	    cc_status.value2 = 0;
	  return;
	}
      /* Function calls clobber the cc's.  */
      else if (GET_CODE (SET_SRC (exp)) == CALL)
	{
	  CC_STATUS_INIT;
	  return;
	}
      /* Tests and compares set the cc's in predictable ways.  */
      else if (SET_DEST (exp) == cc0_rtx)
	{
	  CC_STATUS_INIT;
	  cc_status.value1 = SET_SRC (exp);
	  return;
	}
      /* Certain instructions effect the condition codes. */
      else if (GET_MODE (SET_SRC (exp)) == SImode
	       || GET_MODE (SET_SRC (exp)) == HImode
	       || GET_MODE (SET_SRC (exp)) == QImode)
	switch (GET_CODE (SET_SRC (exp)))
	  {
	  case ASHIFTRT: case LSHIFTRT:
	  case ASHIFT: case LSHIFT:
	    /* Shifts on the 386 don't set the condition codes if the
	       shift count is zero. */
	    if (GET_CODE (XEXP (SET_SRC (exp), 1)) != CONST_INT)
	      {
		CC_STATUS_INIT;
		break;
	      }
	    /* We assume that the CONST_INT is non-zero (this rtx would
	       have been deleted if it were zero. */

	  case PLUS: case MINUS: case NEG:
	  case AND: case IOR: case XOR:
	    cc_status.flags = CC_NO_OVERFLOW;
	    cc_status.value1 = SET_SRC (exp);
	    cc_status.value2 = SET_DEST (exp);
	    break;

	  default:
	    CC_STATUS_INIT;
	  }
      else
	{
	  CC_STATUS_INIT;
	}
    }
  else if (GET_CODE (exp) == PARALLEL
	   && GET_CODE (XVECEXP (exp, 0, 0)) == SET)
    {
      if (SET_DEST (XVECEXP (exp, 0, 0)) == pc_rtx)
	return;
      if (SET_DEST (XVECEXP (exp, 0, 0)) == cc0_rtx)
	{
	  CC_STATUS_INIT;
	  cc_status.value1 = SET_SRC (XVECEXP (exp, 0, 0));
	  return;
	}
      CC_STATUS_INIT;
    }
  else
    {
      CC_STATUS_INIT;
    }
}

/* Nonzero if the top of the fpu stack dies in this insn.  */

int
top_dead_p (insn)
     rtx insn;
{
  extern int optimize;
  if (optimize)
    return (find_regno_note (insn, REG_DEAD, FIRST_FLOAT_REG)
	    || find_regno_note (insn, REG_DEAD, FIRST_FLOAT_REG + 1));

  if (GET_CODE (insn) == CALL_INSN)
    return call_top_dead_p (insn);

  return fp_top_dead_p1 (insn);
}

/* Following is used after a call_value insn
   if obey_regdecls there will not be the REG_DEAD notes
   to go by (there won't be any cross jumping to worry about
   either), and we depend on seeing if the FP_TOP is used
   in the next two insn's.  Otherwise we depend on the
   REG_DEAD notes.
   */

static int
call_top_dead_p (insn)
     rtx insn;
{
  int i;
  for (i = 0; i < 3; i++)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0)
	return 1;
      if (GET_CODE (insn) == NOTE || GET_CODE (insn) == CODE_LABEL)
	continue;
      if (GET_CODE (insn) == BARRIER)
	abort ();
      if (GET_CODE (PATTERN (insn)) == SET
	  && SET_DEST (PATTERN (insn)) != stack_pointer_rtx)
	return (!(mentions_fp_top (SET_SRC (PATTERN (insn)))));
      if (GET_CODE (PATTERN (insn)) == CALL)
	return 1;
      if (GET_CODE (PATTERN (insn)) == USE)
	return (! FP_REG_P (XEXP (PATTERN (insn), 0)));
    }
  return 1;
}

/* Return 1 if current val of fpu top-of-stack appears unused
   in rest of this basic block.  */

static int
fp_top_dead_p1 (insn)
     rtx insn;
{
  extern int optimize;

  int past_label = 0;

  for (insn = NEXT_INSN (insn); insn; insn = NEXT_INSN (insn))
    {
      switch (GET_CODE (insn))
	{
	case CALL_INSN:
	  /* Function calls clobber this value, so it's dead.  */
	  return 1;

	case JUMP_INSN:
	  if (! optimize)
	    /* Can't use JUMP_LABEL, but there's no cross-jumping either.  */
	    return 1;
	  if (JUMP_LABEL (insn) == 0)
	    return 1;
	  insn = JUMP_LABEL (insn);
	case CODE_LABEL:
	  /* Go past one label or follow one jump in case of cross-jumping,
	     which could insert such a label or jump into one basic block.  */
	  if (! optimize)
	    return 1;
	  if (past_label)
	    return 1;
	  past_label = 1;
	  break;

	case INSN:
	  if (GET_CODE (PATTERN (insn)) == SET)
	    {
	      if ((mentions_fp_top (SET_SRC (PATTERN (insn)))))
		return 0;
	      else if (FP_REG_P (SET_DEST (PATTERN (insn))))
		return 1;
	    }
	  else if (mentions_fp_top (PATTERN (insn)))
	    return 0;
	  break;
	}
    }
  return 1;
}

/* Return 1 if X involves an FPU register.  */

static int
mentions_fp_top (x)
     rtx x;
{
  register RTX_CODE code;

  code = GET_CODE (x);
  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST:
    case CC0:
    case PC:
    case CLOBBER:
    case MEM:
      return 0;

    case REG:
      return FP_REGNO_P (REGNO (x));
    }

  /* Recursively scan the operands of this expression.  */
  {
    register char *fmt = GET_RTX_FORMAT (code);
    register int i;

    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (mentions_fp_top (XEXP (x, i)))
	      return 1;
	  }
	if (fmt[i] == 'E')
	  {
	    register int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (mentions_fp_top (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }
  return 0;
}

/* Some asm-dependent functions. */

#ifdef MASM
#include "masm386.c"
#endif
