/* Subroutines for insn-output.c for Convex.
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

/* Boolean to keep track of whether the current section is .text or not.
   Used by .align handler in tm-convex.h. */

int current_section_is_text;

/*
 *  set_cmp (left_rtx, right_rtx, [bhwlsd])
 *  gen_cmp (label_rtx, cmpop, [tf])
 *
 *  set_cmp saves the operands of a "cmp" insn,
 *    along with the type character to be used in the compare instruction.
 *
 *  gen_cmp finds out what comparison is to be performed and
 *    outputs the necessary instructions, eg,
 *    "eq.w a1,a2 ! jbra.t L5"
 *    for (cmpsi a1 a2) (beq L5)
 */
 
static rtx xop0, xop1;
static char typech, regch;

char *
set_cmp (op0, op1, typechr)
     rtx op0, op1;
     char typechr;
{
  xop0 = op0;
  xop1 = op1;
  typech = typechr;
  if (GET_CODE (op0) == REG)
    regch = REGNO_OK_FOR_BASE_P (REGNO (op0)) ? 'a' : 's';
  else if (GET_CODE (op1) == REG)
    regch = REGNO_OK_FOR_BASE_P (REGNO (op1)) ? 'a' : 's';
  else abort ();
  return "";
}

char *
gen_cmp (label, cmpop, tf)
     rtx label;
     char *cmpop;
     char tf;
{
  char buf[80];
  char revop[4];
  rtx ops[3];

  ops[2] = label;

  /* constant must be first; swap operands if necessary
     if lt, le, ltu, leu are swapped, change to le, lt, leu, ltu
     and reverse the sense of the jump */

  if (CONSTANT_P (xop1) || GET_CODE (xop1) == CONST_DOUBLE)
    {
      ops[0] = xop1;
      ops[1] = xop0;
      if (cmpop[0] == 'l')
	{
	  bcopy (cmpop, revop, 4);
	  revop[1] ^= 'e' ^ 't';
	  tf ^= 't' ^ 'f';
	  cmpop = revop;
	}
    }
  else
    {
      ops[0] = xop0;
      ops[1] = xop1;
    }

  sprintf (buf, "%s.%c %%0,%%1\n\tjbr%c.%c %%l2", cmpop, typech, regch, tf);
  output_asm_insn (buf, ops);
  return "";
}

/*
 * Routines to look at CONST_DOUBLEs without sinful knowledge of
 * what the inside of u.d looks like
 *
 * const_double_high_int  -- high word of machine double or long long
 * const_double_low_int   -- low word
 * const_double_float_int -- the word of a machine float
 */

static double frexp ();
static void float_extract ();

int
const_double_high_int (x)
     rtx x;
{
  if (GET_MODE (x) == DImode)
    return CONST_DOUBLE_HIGH (x);
  else
    {
      int sign, expd, expf;
      unsigned fracdh, fracdl, fracf;
      float_extract (x, &sign, &expd, &fracdh, &fracdl, &expf, &fracf);

      if (fracdh == 0)
	return 0;
      if (expd < -01777 || expd > 01777)
	return 1 << 31;
      return sign << 31 | (expd + 02000) << 20 | fracdh - (1 << 20);
    }
}

int
const_double_low_int (x)
     rtx x;
{
  if (GET_MODE (x) == DImode)
    return CONST_DOUBLE_LOW (x);
  else
    {
      int sign, expd, expf;
      unsigned fracdh, fracdl, fracf;
      float_extract (x, &sign, &expd, &fracdh, &fracdl, &expf, &fracf);
      return fracdl;
    }
}

int
const_double_float_int (x)
     rtx x;
{
  int sign, expd, expf;
  unsigned fracdh, fracdl, fracf;
  float_extract (x, &sign, &expd, &fracdh, &fracdl, &expf, &fracf);

  if (fracf == 0)
    return 0;
  if (expf < -0177 || expf > 0177)
    return 1 << 31;
  return sign << 31 | (expf + 0200) << 20 | fracf - (1 << 23);
}

#define T21  ((double) (1 << 21))
#define T24  ((double) (1 << 24))
#define T53  ((double) (1 << 27) * (double) (1 << 26))

static void
float_extract (x, sign, expd, fracdh, fracdl, expf, fracf)
     rtx x;
     int *sign, *expd, *expf;
     unsigned *fracdh, *fracdl, *fracf;
{
  int exp, round;
  double d, r;
  union real_extract u;

  bcopy (&CONST_DOUBLE_LOW (x), &u, sizeof u);

  /* Get sign and exponent.  */

  if (*sign = u.d < 0)
    u.d = -u.d;
  d = frexp (u.d, &exp);  

  /* Get 21 fraction bits for high word and 32 for low word.  */

  for (round = 0; ; round = 1)
    {
      r = frexp (round ? d + 1.0 / T53 : d, expd);
      *expd += exp;
      *fracdh = r * T21;
      *fracdl = (r - *fracdh / T21) * T53;
      if (round || ((r - *fracdh / T21) - *fracdl / T53) < 0.5 * T53)
	break;
    }

  /* Get 24 bits for float fraction. */

  for (round = 0; ; round = 1)
    {
      r = frexp (round ? d + 1.0 / T24 : d, expf);
      *expf += exp;
      *fracf = r * T24;
      if (round || (r - *fracf / T24) < 0.5 * T24)
	break;
    }
}

static double
frexp (d, exp)
     double d;
     int *exp;
{
  int e = 0;

  if (d > 0)
    {
      while (d < 0.5) d *= 2.0, e--;
      while (d >= 1.0) d /= 2.0, e++;
    }

  *exp = e;
  return d;
}
