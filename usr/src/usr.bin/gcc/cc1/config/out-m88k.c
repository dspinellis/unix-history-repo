/* Subroutines for insn-output.c for Motorola 88000.
   Copyright (C) 1987 Free Software Foundation, Inc.
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

#ifndef FILE
#include <stdio.h>
#endif

/* This is where the condition code register lives.  */
rtx cc0_reg_rtx;

static rtx find_addr_reg ();

#if 0
char *
output_compare (operands, opcode, exchange_opcode)
     rtx *operands;
     char *opcode;
     char *exchange_opcode;
{
  static char buf[40];
  rtx op1, op2;

  if (GET_CODE (cc_prev_status.value2) == COMPARE)
    {
      op1 = XEXP (cc_prev_status.value2, 0);
      op2 = XEXP (cc_prev_status.value2, 1);
    }
  else
    {
      op1 = cc_prev_status.value2;
      op2 = const0_rtx;
    }
  if (GET_CODE (op1) == CONST_INT)
    {
      operands[2] = op1;
      operands[1] = op2;
      opcode = exchange_opcode;
    }
  else
    {
      operands[1] = op1;
      operands[2] = op2;
    }
  sprintf (buf, "cmp r25,%%1,%%2\n\tbcnd %s,r25,%%l0", opcode);
  return buf;
}

char *
output_fcompare (operands, opcode, exchange_opcode)
     rtx *operands;
     char *opcode;
     char *exchange_opcode;
{
  static char buf[40];

  rtx op1, op2;

  if (GET_CODE (cc_prev_status.value2) == COMPARE)
    {
      op1 = XEXP (cc_prev_status.value2, 0);
      op2 = XEXP (cc_prev_status.value2, 1);
    }
  else
    {
      op1 = cc_prev_status.value2;
      op2 = const0_rtx;
    }
  if (GET_CODE (op1) == CONST_DOUBLE)
    {
      operands[2] = op1;
      operands[1] = op2;
      opcode = exchange_opcode;
    }
  else
    {
      operands[1] = op1;
      operands[2] = op2;
    }
  sprintf (buf, "cmp r25,%%1,%%2\n\tbcnd %s,r25,%%l0", opcode);
  return buf;
}

char *
output_store (operands, opcode, exchange_opcode)
     rtx *operands;
     char *opcode;
     char *exchange_opcode;
{
  static char buf[40];
  rtx op1, op2;

  if (GET_CODE (cc_prev_status.value2) == COMPARE)
    {
      op1 = XEXP (cc_prev_status.value2, 0);
      op2 = XEXP (cc_prev_status.value2, 1);
    }
  else
    {
      op1 = cc_prev_status.value2;
      op2 = const0_rtx;
    }

  if (GET_CODE (op1) == CONST_INT)
    {
      operands[2] = op1;
      operands[1] = op2;
      opcode = exchange_opcode;
    }
  else
    {
      operands[1] = op1;
      operands[2] = op2;
    }

  sprintf (buf, "cmp r25,%%1,%%2\n\textu %%0,r25,1<%s>", opcode);
  return buf;
}
#endif

/* Nonzero if OP is a valid second operand for an arithmetic insn.  */

int
arith_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode)
	  || (GET_CODE (op) == CONST_INT
	      && (unsigned) INTVAL (op) < 0x10000));
}

int
arith32_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (register_operand (op, mode) || GET_CODE (op) == CONST_INT);
}

int
int5_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  return (GET_CODE (op) == CONST_INT && (unsigned) INTVAL (op) < 0x20);
}

/* Return the best assembler insn template
   for moving operands[1] into operands[0] as a fullword.  */

static char *
singlemove_string (operands)
     rtx *operands;
{
  if (GET_CODE (operands[0]) == MEM)
    return "st %r1,%0";
  if (GET_CODE (operands[1]) == MEM)
    return "ld %0,%1";
  return "or %0,r0,%1";
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
    addreg0 = find_addr_reg (operands[0]);

  if (optype1 == MEMOP)
    addreg1 = find_addr_reg (operands[1]);

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
     do them in the other order.  This happens only for registers;
     such overlap can't happen in memory unless the user explicitly
     sets it up, and that is an undefined circumstance.  */

  if (optype0 == REGOP && optype1 == REGOP
      && REGNO (operands[0]) == REGNO (latehalf[1]))
    {
      /* Make any unoffsettable addresses point at high-numbered word.  */
      if (addreg0)
	output_asm_insn ("addu %0,%0,4", &addreg0);
      if (addreg1)
	output_asm_insn ("addu %0,%0,4", &addreg1);

      /* Do that word.  */
      output_asm_insn (singlemove_string (latehalf), latehalf);

      /* Undo the adds we just did.  */
      if (addreg0)
	output_asm_insn ("subu %0,%0,4", &addreg0);
      if (addreg1)
	output_asm_insn ("subu %0,%0,4", &addreg0);

      /* Do low-numbered word.  */
      return singlemove_string (operands);
    }

  /* Normal case: do the two words, low-numbered first.  */

  output_asm_insn (singlemove_string (operands), operands);

  /* Make any unoffsettable addresses point at high-numbered word.  */
  if (addreg0)
    output_asm_insn ("addu %0,%0,4", &addreg0);
  if (addreg1)
    output_asm_insn ("addu %0,%0,4", &addreg1);

  /* Do that word.  */
  output_asm_insn (singlemove_string (latehalf), latehalf);

  /* Undo the adds we just did.  */
  if (addreg0)
    output_asm_insn ("subu %0,%0,4", &addreg0);
  if (addreg1)
    output_asm_insn ("subu %0,%0,4", &addreg1);

  return "";
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
      if (GET_CODE (XEXP (addr, 1)) == REG)
	addr = XEXP (addr, 1);
      if (CONSTANT_P (XEXP (addr, 0)))
	addr = XEXP (addr, 1);
      if (CONSTANT_P (XEXP (addr, 1)))
	addr = XEXP (addr, 0);
    }
  if (GET_CODE (addr) == REG)
    return addr;
  return 0;
}

/* Output an ascii string.  */
output_ascii (file, p, size)
     FILE *file;
     char *p;
     int size;
{
  int i;

  fprintf (file, "\tstring \"");

  for (i = 0; i < size; i++)
    {
      register int c = p[i];
      if (c == '\"' || c == '\\')
	putc ('\\', file);
      if (c >= ' ' && c < 0177)
	putc (c, file);
      else
	{
	  fprintf (file, "\\%03o", c);
	  /* After an octal-escape, if a digit follows,
	     terminate one string constant and start another.
	     The Vax assembler fails to stop reading the escape
	     after three digits, so this is the only way we
	     can get it to parse the data properly.  */
	  if (i < size - 1 && p[i + 1] >= '0' && p[i + 1] <= '9')
	    fprintf (file, "\"\n\tstring \"");
	}
    }
  fprintf (file, "\"\n");
}

void
output_load_address (operands)
     rtx *operands;
{
  rtx base, offset;

  if (CONSTANT_P (operands[3]))
    {
      output_asm_insn ("lda %0,%3", operands);
      return;
    }

  if (REG_P (operands[3]))
    {
      if (REGNO (operands[0]) != REGNO (operands[3]))
	output_asm_insn ("or %0,r0,%3", operands);
      return;
    }

  base = XEXP (operands[3], 0);
  offset = XEXP (operands[3], 1);

  if (GET_CODE (base) == CONST_INT)
    {
      rtx tmp = base;
      base = offset;
      offset = tmp;
    }

  if (GET_CODE (offset) != CONST_INT)
    abort ();

  operands[6] = base;
  operands[7] = offset;

  if (REG_P (base))
    if (FITS_16_BITS (offset))
      output_asm_insn ("addu %0,%6,%7", operands);
    else if (INT_FITS_16_BITS (- INTVAL (offset)))
      output_asm_insn ("subu %0,%6,%7", operands);
    else
      output_asm_insn ("or.h %0,r0,hi16(%7)\n\tor %0,%0,lo16(%7)\n\tadd %0,%6,%0", operands);
  else
    {
      if (GET_CODE (base) == MULT)
	if (GET_MODE (base) == QImode)
	  output_asm_insn ("lda.b %0,%6");
	else if (GET_MODE (base) == HImode)
	  output_asm_insn ("lda.h %0,%6");
	else if (GET_MODE (base) == SImode)
	  output_asm_insn ("lda %0,%6");
	else
	  output_asm_insn ("lda.d %0,%6");
      else
	output_asm_insn ("lda %0,%6");

      if (FITS_16_BITS (offset))
	output_asm_insn ("addu %0,%7,%0", operands);
      else if (INT_FITS_16_BITS (- INTVAL (offset)))
	output_asm_insn ("subu %0,%7,%0", operands);
      else
	output_asm_insn ("or.h r25,r0,hi16(%7)\n\tor r25,r0,lo16(%7)\n\taddu %0,%0r25", operands);
    }
}

char *
output_block_move (operands)
     rtx *operands;
{
  static int movstrsi_label = 0;
  int align = 4;

  rtx xoperands[9];
  int available[3];
  int i, j;

  /* Since we clobber untold things, nix the condition codes.  */
  CC_STATUS_INIT;

  /* Get past the MEMs.  */
  operands[0] = XEXP (operands[0], 0);
  operands[1] = XEXP (operands[1], 0);

  xoperands[0] = 0;
  xoperands[1] = 0;
  xoperands[2] = 0;

  available[0] = 1;
  available[1] = 1;
  available[2] = 1;
#if 1
  /* Prepare to juggle registers if necessary.  */
  if (REG_P (operands[0]) && (unsigned) (REGNO (operands[0]) - 10) < 3)
    {
      xoperands[0] = operands[0];
      available[REGNO (operands[0]) - 10] = 0;
    }
  if (REG_P (operands[1]) && (unsigned) (REGNO (operands[1]) - 10) < 3)
    {
      xoperands[1] = operands[1];
      available[REGNO (operands[1]) - 10] = 0;
    }
  if (REG_P (operands[2]) && (unsigned) (REGNO (operands[2]) - 10) < 3)
    {
      xoperands[2] = operands[2];
      available[REGNO (operands[2]) - 10] = 0;
    }
  for (i = 0; i < 3; i++)
    {
      if (xoperands[i])
	continue;
      if (available[0])
	{
	  xoperands[i] = gen_rtx (REG, SImode, 10);
	  available[0] = 0;
	  continue;
	}
      if (available[1])
	{
	  xoperands[i] = gen_rtx (REG, SImode, 11);
	  available[1] = 0;
	  continue;
	}
      xoperands[i] = gen_rtx (REG, SImode, 12);
      available[2] = 0;
    }
#endif

  /* First, figure out best alignment we may assume.  */
  if (REG_P (operands[2]))
    {
      xoperands[5] = operands[2];
      output_asm_insn ("sub %5,%2,1", xoperands);
      align = 1;
    }
  else
    {
      int i = INTVAL (operands[2]);

      if (i & 1)
	align = 1;
      else if (i & 3)
	{
	  align = 2;
	  i >>= 1;
	}
      else
	i >>= 2;

      /* predecrement count.  */
      i -= 1;
      if (i < 0) abort ();

      xoperands[5] = gen_rtx (CONST_INT, VOIDmode, i);

      if (INT_FITS_16_BITS (i))
	output_asm_insn ("addu %2,r0,%5", xoperands);
      else if (INT_FITS_16_BITS (-i))
	{
	  xoperands[5] = gen_rtx (CONST_INT, VOIDmode, -i);
	  output_asm_insn ("subu %2,r0,%5", xoperands);
	}
      else
	output_asm_insn ("or.u %2,r0,hi16(%5)\n\tor %2,%2,lo16(%5)", xoperands);
    }
  /* Now, set up for pipelined operation: dest must contain
     a pre-incremented address, because its index is pre-decremented.  */

  xoperands[3] = plus_constant (operands[0], align);
  output_load_address (xoperands);

  xoperands[4] = operands[1];
  output_load_address (xoperands+1);

  xoperands[3] = gen_rtx (CONST_INT, VOIDmode, movstrsi_label++);

  if (align == 4)
    output_asm_insn ("\n@Lm%3:\n\tld r25,%1[%2]\n\tsubu %2,%2,1\n\tbcnd.n ge0,%2,@Lm%3\n\tst r25,%0[%2]", xoperands);
  else if (align == 2)
    output_asm_insn ("\n@Lm%3:\n\tld.h r25,%1[%2]\n\tsubu %2,%2,1\n\tbcnd.n ge0,%2,@Lm%3\n\tst.h r25,%0[%2]", xoperands);
  else
    output_asm_insn ("\n@Lm%3:\n\tld.b r25,%1[%2]\n\tsubu %2,%2,1\n\tbcnd.n ge0,%2,@Lm%3\n\tst.b r25,%0[%2]", xoperands);
  return "";
}

char *
output_store_const_int (mode, operands)
     enum machine_mode mode;
     rtx *operands;
{
  int i = INTVAL (operands[1]);
  if (INT_FITS_16_BITS (i))
    return "addu %0,r0,%1";
  if (INT_FITS_16_BITS (-i))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode, -i);
      return "subu %0,r0,%1";
    }
  if ((i & 0xffff) == 0)
    return "or.u %0,r0,hi16(%1)";
  /* Could check to see if number is a contiguous field
     of 1's.  Then we could use the SET instruction.  */
  if (mode == HImode)
    {
      warning ("truncating constant `%d' to fit in half-word", INTVAL (operands[1]));
      return "or %0,r0,lo16(%1)";
    }
  if (mode == QImode)
    {
      warning ("truncating constant `%d' to fit in byte");
      operands[1] = gen_rtx (CONST_INT, VOIDmode, i & 0xff);
      return "or %0,r0,%1";
    }

  return "or.u %0,r0,hi16(%1)\n\tor %0,%0,lo16(%1)";
}

/* This routine assumes that floating point numbers are represented
   in a manner which is consistent between host and target machines.  */
char *
output_store_const_float (mode, operands)
     enum machine_mode mode;
     rtx *operands;
{
  int i = INTVAL (operands[1]);
  if (INT_FITS_16_BITS (i))
    return "addu %0,r0,%1";
  if (INT_FITS_16_BITS (-i))
    {
      operands[1] = gen_rtx (CONST_INT, VOIDmode, -i);
      return "subu %0,r0,%1";
    }
  if ((i & 0xffff) == 0)
    return "or.u %0,r0,hi16(%1)";
  /* Could check to see if number is a contiguous field
     of 1's.  Then we could use the SET instruction.  */
  return "or.u %0,r0,hi16(%1)\n\tor %0,%0,lo16(%1)";
}
