/* Fold a constant sub-tree into a single node for C-compiler
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

/*@@ Fix lossage on folding division of big integers.  */

/*@@ This file should be rewritten to use an arbitary precision
  @@ representation for "struct tree_int_cst" and "struct tree_real_cst".
  @@ Perhaps the routines could also be used for bc/dc, and made a lib.
  @@ The routines that translate from the ap rep should
  @@ warn if precision et. al. is lost.
  @@ This would also make life easier when this technology is used
  @@ for cross-compilers.  */


/* There are only two entry points in this file:
   fold and combine.

   fold takes a tree as argument and returns a simplified tree.

   combine takes a tree code for an arithmetic operation
   and two operands that are trees for constant values
   and returns the result of the specified operation on those values,
   also as a tree.  */
   
#include <stdio.h>
#include <setjmp.h>
#include "config.h"
#include "tree.h"

static void lshift_double ();
static void rshift_double ();
static void lrotate_double ();
static void rrotate_double ();

/* To do constant folding on INTEGER_CST nodes requires 64-bit arithmetic.
   We do that by representing the 64-bit integer as 8 shorts,
   with only 8 bits stored in each short, as a positive number.  */

/* Unpack a 64-bit integer into 8 shorts.
   LOW and HI are the integer, as two `int' pieces.
   SHORTS points to the array of shorts.  */

static void
encode (shorts, low, hi)
     short *shorts;
     int low, hi;
{
  shorts[0] = low & 0xff;
  shorts[1] = (low >> 8) & 0xff;
  shorts[2] = (low >> 16) & 0xff;
  shorts[3] = (low >> 24) & 0xff;
  shorts[4] = hi & 0xff;
  shorts[5] = (hi >> 8) & 0xff;
  shorts[6] = (hi >> 16) & 0xff;
  shorts[7] = (hi >> 24) & 0xff;
}

/* Pack an array of 8 shorts into a 64-bit integer.
   SHORTS points to the array of shorts.
   The integer is stored into *LOW and *HI as two `int' pieces.  */

static void
decode (shorts, low, hi)
     short *shorts;
     int *low, *hi;
{
  *low = (shorts[3] << 24) | (shorts[2] << 16) | (shorts[1] << 8) | shorts[0];
  *hi = (shorts[7] << 24) | (shorts[6] << 16) | (shorts[5] << 8) | shorts[4];
}

/* Make the integer constant T valid for its type
   by setting to 0 or 1 all the bits in the constant
   that don't belong in the type.  */

static void
force_fit_type (t)
     tree t;
{
  register int prec = TYPE_PRECISION (TREE_TYPE (t));

  if (TREE_CODE (TREE_TYPE (t)) == POINTER_TYPE)
    prec = BITS_PER_WORD;

  /* First clear all bits that are beyond the type's precision.  */

  if (prec == 2 * HOST_BITS_PER_INT)
    ;
  else if (prec > HOST_BITS_PER_INT)
    {
      TREE_INT_CST_HIGH (t)
	&= ~((-1) << (prec - HOST_BITS_PER_INT));
    }
  else
    {
      TREE_INT_CST_HIGH (t) = 0;
      if (prec < HOST_BITS_PER_INT)
	TREE_INT_CST_LOW (t)
	  &= ~((-1) << prec);
    }

  /* If it's a signed type and value's sign bit is set, extend the sign.  */

  if (! TREE_UNSIGNED (TREE_TYPE (t))
      && prec != 2 * HOST_BITS_PER_INT
      && (prec > HOST_BITS_PER_INT
	  ? TREE_INT_CST_HIGH (t) & (1 << (prec - HOST_BITS_PER_INT - 1))
	  : TREE_INT_CST_LOW (t) & (1 << (prec - 1))))
    {
      /* Value is negative:
	 set to 1 all the bits that are outside this type's precision.  */
      if (prec > HOST_BITS_PER_INT)
	{
	  TREE_INT_CST_HIGH (t)
	    |= ((-1) << (prec - HOST_BITS_PER_INT));
	}
      else
	{
	  TREE_INT_CST_HIGH (t) = -1;
	  if (prec < HOST_BITS_PER_INT)
	    TREE_INT_CST_LOW (t)
	      |= ((-1) << prec);
	}
    }
}

/* Add two 64-bit integers with 64-bit result.
   Each argument is given as two `int' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `int' pieces in *LV and *HV.
   We use the 8-shorts representation internally.  */

static void
add_double (l1, h1, l2, h2, lv, hv)
     int l1, h1, l2, h2;
     int *lv, *hv;
{
  short arg1[8];
  short arg2[8];
  register int carry = 0;
  register int i;

  encode (arg1, l1, h1);
  encode (arg2, l2, h2);

  for (i = 0; i < 8; i++)
    {
      carry += arg1[i] + arg2[i];
      arg1[i] = carry & 0xff;
      carry >>= 8;
    }

  decode (arg1, lv, hv);
}

/* Negate a 64-bit integers with 64-bit result.
   The argument is given as two `int' pieces in L1 and H1.
   The value is stored as two `int' pieces in *LV and *HV.
   We use the 8-shorts representation internally.  */

static void
neg_double (l1, h1, lv, hv)
     int l1, h1;
     int *lv, *hv;
{
  if (l1 == 0)
    {
      *lv = 0;
      *hv = - h1;
    }
  else
    {
      *lv = - l1;
      *hv = ~ h1;
    }
}

/* Multiply two 64-bit integers with 64-bit result.
   Each argument is given as two `int' pieces.
   One argument is L1 and H1; the other, L2 and H2.
   The value is stored as two `int' pieces in *LV and *HV.
   We use the 8-shorts representation internally.  */

static void
mul_double (l1, h1, l2, h2, lv, hv)
     int l1, h1, l2, h2;
     int *lv, *hv;
{
  short arg1[8];
  short arg2[8];
  short prod[16];
  register int carry = 0;
  register int i, j, k;

  /* These two cases are used extensively, arising from pointer
     combinations.  */
  if (h2 == 0)
    {
      if (l2 == 2)
	{
	  unsigned temp = l1 + l1;
	  *hv = h1 * 2 + (temp < l1);
	  *lv = temp;
	  return;
	}
      if (l2 == 4)
	{
	  unsigned temp = l1 + l1;
	  h1 = h1 * 4 + (temp < l1) << 1;
	  l1 = temp;
	  temp += temp;
	  h1 += (temp < l1);
	  *lv = temp;
	  *hv = h1;
	  return;
	}
      if (l2 == 8)
	{
	  unsigned temp = l1 + l1;
	  h1 = h1 * 8 + (temp < l1) << 2;
	  l1 = temp;
	  temp += temp;
	  h1 += (temp < l1) << 1;
	  l1 = temp;
	  temp += temp;
	  h1 += (temp < l1);
	  *lv = temp;
	  *hv = h1;
	  return;
	}
    }

  encode (arg1, l1, h1);
  encode (arg2, l2, h2);

  bzero (prod, sizeof prod);

  for (i = 0; i < 8; i++)
    for (j = 0; j < 8; j++)
      {
	k = i + j;
	carry = arg1[i] * arg2[j];
	while (carry)
	  {
	    carry += prod[k];
	    prod[k] = carry & 0xff;
	    carry >>= 8;
	    k++;
	  }
      }

  decode (prod, lv, hv);	/* @@decode ignores prod[8] -> prod[15] */
}

/* Shift the 64-bit integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.
   Shift right if COUNT is negative.
   ARITH nonzero specifies arithmetic shifting; otherwise use logical shift.
   Store the value as two `int' pieces in *LV and *HV.  */

static void
lshift_double (l1, h1, count, prec, lv, hv, arith)
     int l1, h1, count, prec;
     int *lv, *hv;
     int arith;
{
  short arg1[8];
  register int i;
  register int carry;

  if (count < 0)
    {
      rshift_double (l1, h1, - count, prec, lv, hv, arith);
      return;
    }

  encode (arg1, l1, h1);

  if (count > prec)
    count = prec;

  while (count > 0)
    {
      carry = 0;
      for (i = 0; i < 8; i++)
	{
	  carry += arg1[i] << 1;
	  arg1[i] = carry & 0xff;
	  carry >>= 8;
	}
      count--;
    }

  decode (arg1, lv, hv);
}

/* Shift the 64-bit integer in L1, H1 right by COUNT places
   keeping only PREC bits of result.  COUNT must be positive.
   ARITH nonzero specifies arithmetic shifting; otherwise use logical shift.
   Store the value as two `int' pieces in *LV and *HV.  */

static void
rshift_double (l1, h1, count, prec, lv, hv, arith)
     int l1, h1, count, prec;
     int *lv, *hv;
     int arith;
{
  short arg1[8];
  register int i;
  register int carry;

  encode (arg1, l1, h1);

  if (count > prec)
    count = prec;

  while (count > 0)
    {
      carry = arith && arg1[7] >> 7;
      for (i = 7; i >= 0; i--)
	{
	  carry <<= 8;
	  carry += arg1[i];
	  arg1[i] = (carry >> 1) & 0xff;
	}
      count--;
    }

  decode (arg1, lv, hv);
}

/* Rotate the 64-bit integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.
   Rotate right if COUNT is negative.
   Store the value as two `int' pieces in *LV and *HV.  */

static void
lrotate_double (l1, h1, count, prec, lv, hv)
     int l1, h1, count, prec;
     int *lv, *hv;
{
  short arg1[8];
  register int i;
  register int carry;

  if (count < 0)
    {
      rrotate_double (l1, h1, - count, prec, lv, hv);
      return;
    }

  encode (arg1, l1, h1);

  if (count > prec)
    count = prec;

  carry = arg1[7] >> 7;
  while (count > 0)
    {
      for (i = 0; i < 8; i++)
	{
	  carry += arg1[i] << 1;
	  arg1[i] = carry & 0xff;
	  carry >>= 8;
	}
      count--;
    }

  decode (arg1, lv, hv);
}

/* Rotate the 64-bit integer in L1, H1 left by COUNT places
   keeping only PREC bits of result.  COUNT must be positive.
   Store the value as two `int' pieces in *LV and *HV.  */

static void
rrotate_double (l1, h1, count, prec, lv, hv)
     int l1, h1, count, prec;
     int *lv, *hv;
{
  short arg1[8];
  register int i;
  register int carry;

  encode (arg1, l1, h1);

  if (count > prec)
    count = prec;

  carry = arg1[0] & 1;
  while (count > 0)
    {
      for (i = 7; i >= 0; i--)
	{
	  carry <<= 8;
	  carry += arg1[i];
	  arg1[i] = (carry >> 1) & 0xff;
	}
      count--;
    }

  decode (arg1, lv, hv);
}

/* Divide 64 bit integer LNUM, HNUM by 64 bit integer LDEN, HDEN
   for a quotient (stored in *LQUO, *HQUO) and remainder (in *LREM, *HREM).
   CODE is a tree code for a kind of division, one of
   TRUNC_DIV_EXPR, FLOOR_DIV_EXPR, CEIL_DIV_EXPR, ROUND_DIV_EXPR
   or EXACT_DIV_EXPR
   It controls how the quotient is rounded to a integer.
   UNS nonzero says do unsigned division.  */

static void
div_and_round_double (code, uns,
		      lnum_orig, hnum_orig, lden_orig, hden_orig,
		      lquo, hquo, lrem, hrem)
     enum tree_code code;
     int uns;
     int lnum_orig, hnum_orig;		/* num == numerator == dividend */
     int lden_orig, hden_orig;		/* den == denominator == divisor */
     int *lquo, *hquo, *lrem, *hrem;
{
  int quo_neg = 0;
  short num[9], den[8], quo[8];	/* extra element for scaling.  */
  register int i, j, work;
  register int carry = 0;
  int lnum = lnum_orig, hnum = hnum_orig;
  int lden = lden_orig, hden = hden_orig;

  if ((hden == 0) && (lden == 0))
    abort ();

  /* calculate quotient sign and convert operands to unsigned.  */
  if (!uns) 
    {
      if (hden < 0) 
	{
	  quo_neg = ~ quo_neg;
	  neg_double (lden, hden, &lden, &hden);
	}
      if (hnum < 0)
	{
	  quo_neg = ~ quo_neg;
	  neg_double (lnum, hnum, &lnum, &hnum);
	}
    }

  if (hnum == 0 && hden == 0)
    {				/* single precision */
      *hquo = *hrem = 0;
      *lquo = (unsigned) lnum / lden;	/* rounds toward zero since positive args */
      goto finish_up;
    }

  if (hnum == 0)
    {				/* trivial case: dividend < divisor */
      /* hden != 0 already checked.  */
      *hquo = *lquo = 0;
      *hrem = hnum;
      *lrem = lnum;
      goto finish_up;
    }

  bzero (quo, sizeof quo);

  bzero (num, sizeof num);	/* to zero 9th element */
  bzero (den, sizeof den);

  encode (num, lnum, hnum); 
  encode (den, lden, hden);

  if (hden == 0)
    {				/* simpler algorithm */
      /* hnum != 0 already checked.  */
      for (i = 7; i >= 0; i--)
	{
	  work = num[i] + (carry << 8);
	  quo[i] = work / lden;
	  carry = work % lden;
	}
    }
  else {			/* full double precision,
				   with thanks to Don Knuth's
				   "Semi-Numericial Algorithms".  */
#define BASE 256
    int quo_est, scale, num_hi_sig, den_hi_sig, quo_hi_sig;

    /* Find the highest non-zero divisor digit.  */
    for (i = 7; ; i--)
      if (den[i] != 0) {
	den_hi_sig = i;
	break;
      }
    for (i = 7; ; i--)
      if (num[i] != 0) {
	num_hi_sig = i;
	break;
      }
    quo_hi_sig = num_hi_sig - den_hi_sig + 1;

    /* Insure that the first digit of the divisor is at least BASE/2.
       This is required by the quotient digit estimation algorithm.  */

    scale = BASE / (den[den_hi_sig] + 1);
    if (scale > 1) {		/* scale divisor and dividend */
      carry = 0;
      for (i = 0; i <= 8; i++) {
	work = (num[i] * scale) + carry;
	num[i] = work & 0xff;
	carry = work >> 8;
	if (num[i] != 0) num_hi_sig = i;
      }
      carry = 0;
      for (i = 0; i <= 7; i++) {
	work = (den[i] * scale) + carry;
	den[i] = work & 0xff;
	carry = work >> 8;
	if (den[i] != 0) den_hi_sig = i;
      }
    }

    /* Main loop */
    for (i = quo_hi_sig; i > 0; i--) {
      /* quess the next quotient digit, quo_est, by dividing the first
	 two remaining dividend digits by the high order quotient digit.
	 quo_est is never low and is at most 2 high.  */

      int num_hi;		/* index of highest remaining dividend digit */

      num_hi = i + den_hi_sig;

      work = (num[num_hi] * BASE) + (num_hi ? 0 : num[num_hi - 1]);
      if (num[num_hi] != den[den_hi_sig]) {
	quo_est = work / den[den_hi_sig];
      }
      else {
	quo_est = BASE - 1;
      }

      /* refine quo_est so it's usually correct, and at most one high.   */
      while ((den[den_hi_sig - 1] * quo_est)
	     > (((work - (quo_est * den[den_hi_sig])) * BASE)
		 + ((num_hi - 1) ? 0 : num[num_hi - 2]))) {
	quo_est--;
      }

      /* try quo_est as the quotient digit, by multiplying the
         divisor by quo_est and subtracting from the remaining dividend.  */

      carry = 0;

      for (j = 0; j <= den_hi_sig; j++) {
	int digit;

	work = num[i + j] - (quo_est * den[j]) + carry;
	digit = work & 0xff;
	carry = work >> 8;
	if (digit < 0) {
	  digit += BASE;
	  carry--;
	}
	num[i + j] = digit;
      }

      /* if quo_est was high by one, then num[i] went negative and
	 we need to correct things.  */

      if (num[num_hi] < 0) {
	quo_est--;
	carry = 0;		/* add divisor back in */
	for (j = 0; j <= den_hi_sig; j++) {
	  work = num[i + j] + den[j] + carry;
	  if (work > BASE) {
	    work -= BASE;
	    carry = 1;
	  }
	  else {
	    carry = 0;
	  }
	  num[i + j] = work;
	}
	num [num_hi] += carry;
      }

      /* store the quotient digit.  */
      quo[i - 1] = quo_est;
    }
  }

  decode (quo, lquo, hquo);

 finish_up:
  /* if result is negative, make it so.  */
  if (quo_neg)
    neg_double (*lquo, *hquo, lquo, hquo);

  /* compute trial remainder:  rem = num - (quo * den)  */
  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);

  switch (code)
    {
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:	/* round toward zero */
    case EXACT_DIV_EXPR:	/* for this one, it shouldn't matter */
      return;

    case FLOOR_DIV_EXPR:
    case FLOOR_MOD_EXPR:	/* round toward negative infinity */
      if (quo_neg && (*lrem != 0 || *hrem != 0))   /* ratio < 0 && rem != 0 */
	{
	  /* quo = quo - 1;  */
	  add_double (*lquo, *hquo, -1, -1, lquo, hquo);
	}
      else return;
      break;

    case CEIL_DIV_EXPR:
    case CEIL_MOD_EXPR:		/* round toward positive infinity */
      if (!quo_neg && (*lrem != 0 || *hrem != 0))  /* ratio > 0 && rem != 0 */
	{
	  add_double (*lquo, *hquo, 1, 0, lquo, hquo);
	}
      else return;
      break;
    
    case ROUND_DIV_EXPR:
    case ROUND_MOD_EXPR:	/* round to closest integer */
      {
	int labs_rem = *lrem, habs_rem = *hrem;
	int labs_den = lden, habs_den = hden, ltwice, htwice;

	/* get absolute values */
	if (*hrem < 0) neg_double (*lrem, *hrem, &labs_rem, &habs_rem);
	if (hden < 0) neg_double (lden, hden, &labs_den, &habs_den);

	/* if (2 * abs (lrem) >= abs (lden)) */
	mul_double (2, 0, labs_rem, habs_rem, &ltwice, &htwice);
	if (((unsigned) habs_den < (unsigned) htwice)
	    || (((unsigned) habs_den == (unsigned) htwice)
		&& ((unsigned) labs_den < (unsigned) ltwice)))
	  {
	    if (*hquo < 0)
	      /* quo = quo - 1;  */
	      add_double (*lquo, *hquo, -1, -1, lquo, hquo);
	    else
	      /* quo = quo + 1; */
	      add_double (*lquo, *hquo, 1, 0, lquo, hquo);
	  }
	else return;
      }
      break;

    default:
      abort ();
    }

  /* compute true remainder:  rem = num - (quo * den)  */
  mul_double (*lquo, *hquo, lden_orig, hden_orig, lrem, hrem);
  neg_double (*lrem, *hrem, lrem, hrem);
  add_double (lnum_orig, hnum_orig, *lrem, *hrem, lrem, hrem);
}

/* Split a tree IN into a constant and a variable part
   that could be combined with CODE to make IN.
   CODE must be a commutative arithmetic operation.
   Store the constant part into *CONP and the variable in &VARP.
   Return 1 if this was done; zero means the tree IN did not decompose
   this way.

   If CODE is PLUS_EXPR we also split trees that use MINUS_EXPR.
   Therefore, we must tell the caller whether the variable part
   was subtracted.  We do this by storing 1 or -1 into *VARSIGNP.
   The value stored is the coefficient for the variable term.
   The constant term we return should always be added;
   we negate it if necessary.  */

static int
split_tree (in, code, varp, conp, varsignp)
     tree in;
     enum tree_code code;
     tree *varp, *conp;
     int *varsignp;
{
  register tree outtype = TREE_TYPE (in);
  *varp = 0;
  *conp = 0;

  /* Strip any conversions that don't change the machine mode.  */
  while ((TREE_CODE (in) == NOP_EXPR
	  || TREE_CODE (in) == CONVERT_EXPR)
	 && (TYPE_MODE (TREE_TYPE (in))
	     == TYPE_MODE (TREE_TYPE (TREE_OPERAND (in, 0)))))
    in = TREE_OPERAND (in, 0);

  if (TREE_CODE (in) == code
      || (TREE_CODE (TREE_TYPE (in)) != REAL_TYPE
	  /* We can associate addition and subtraction together
	     (even though the C standard doesn't say so)
	     for integers because the value is not affected.
	     For reals, the value might be affected, so we can't.  */
	  &&
	  ((code == PLUS_EXPR && TREE_CODE (in) == MINUS_EXPR)
	   || (code == MINUS_EXPR && TREE_CODE (in) == PLUS_EXPR))))
    {
      enum tree_code code = TREE_CODE (TREE_OPERAND (in, 0));
      if (code == INTEGER_CST)
	{
	  *conp = TREE_OPERAND (in, 0);
	  *varp = TREE_OPERAND (in, 1);
	  if (TREE_TYPE (*varp) != outtype)
	    *varp = convert (outtype, *varp);
	  *varsignp = (TREE_CODE (in) == MINUS_EXPR) ? -1 : 1;
	  return 1;
	}
      if (TREE_LITERAL (TREE_OPERAND (in, 1)))
	{
	  *conp = TREE_OPERAND (in, 1);
	  *varp = TREE_OPERAND (in, 0);
	  *varsignp = 1;
	  if (TREE_TYPE (*varp) != outtype)
	    *varp = convert (outtype, *varp);
	  if (TREE_CODE (in) == MINUS_EXPR)
	    {
	      /* If operation is subtraction and constant is second,
		 must negate it to get an additive constant.
		 And this cannot be done unless it is a manifest constant.
		 It could also be the address of a static variable.
		 We cannot negate that, so give up.  */
	      if (TREE_CODE (*conp) == INTEGER_CST)
		*conp = fold (build (NEGATE_EXPR, TREE_TYPE (*conp), *conp));
	      else
		return 0;
	    }
	  return 1;
	}
      if (TREE_LITERAL (TREE_OPERAND (in, 0)))
	{
	  *conp = TREE_OPERAND (in, 0);
	  *varp = TREE_OPERAND (in, 1);
	  if (TREE_TYPE (*varp) != outtype)
	    *varp = convert (outtype, *varp);
	  *varsignp = (TREE_CODE (in) == MINUS_EXPR) ? -1 : 1;
	  return 1;
	}
    }
  return 0;
}

/* Combine two constants NUM and ARG2 under operation CODE
   to produce a new constant.
   We assume ARG1 and ARG2 have the same data type,
   or at least are the same kind of constant and the same machine mode.  */

/* Handle floating overflow for `combine'.  */
static jmp_buf combine_error;

tree
combine (code, arg1, arg2)
     enum tree_code code;
     register tree arg1, arg2;
{
  if (TREE_CODE (arg1) == INTEGER_CST)
    {
      register int int1l = TREE_INT_CST_LOW (arg1);
      register int int1h = TREE_INT_CST_HIGH (arg1);
      int int2l = TREE_INT_CST_LOW (arg2);
      int int2h = TREE_INT_CST_HIGH (arg2);
      int low, hi;
      int garbagel, garbageh;
      register tree t;
      int uns = TREE_UNSIGNED (TREE_TYPE (arg1));

      switch (code)
	{
	case BIT_IOR_EXPR:
	  t = build_int_2 (int1l | int2l, int1h | int2h);
	  break;

	case BIT_XOR_EXPR:
	  t = build_int_2 (int1l ^ int2l, int1h ^ int2h);
	  break;

	case BIT_AND_EXPR:
	  t = build_int_2 (int1l & int2l, int1h & int2h);
	  break;

	case BIT_ANDTC_EXPR:
	  t = build_int_2 (int1l & ~int2l, int1h & ~int2h);
	  break;

	case RSHIFT_EXPR:
	  int2l = - int2l;
	case LSHIFT_EXPR:
	  lshift_double (int1l, int1h, int2l,
			 TYPE_PRECISION (TREE_TYPE (arg1)),
			 &low, &hi,
			 !uns);
	  t = build_int_2 (low, hi);
	  break;

	case RROTATE_EXPR:
	  int2l = - int2l;
	case LROTATE_EXPR:
	  lrotate_double (int1l, int1h, int2l,
			  TYPE_PRECISION (TREE_TYPE (arg1)),
			  &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case PLUS_EXPR:
	  if (int1h == 0)
	    {
	      int2l += int1l;
	      if ((unsigned) int2l < int1l)
		int2h += 1;
	      t = build_int_2 (int2l, int2h);
	      break;
	    }
	  if (int2h == 0)
	    {
	      int1l += int2l;
	      if ((unsigned) int1l < int2l)
		int1h += 1;
	      t = build_int_2 (int1l, int1h);
	      break;
	    }
	  add_double (int1l, int1h, int2l, int2h, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case MINUS_EXPR:
	  if (int1h == 0 && int1l == 0)
	    {
	      t = build_int_2 (- int2l, - int2h - (int2l != 0));
	      break;
	    }
	  if (int2h == 0 && int2l == 0)
	    {
	      t = build_int_2 (int1l, int1h);
	      break;
	    }
	  neg_double (int2l, int2h, &int2l, &int2h);
	  add_double (int1l, int1h, int2l, int2h, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case MULT_EXPR:
  /* Optimize simple cases.  */
	  if (int1h == 0)
	    {
	      unsigned temp;

	      switch (int1l)
		{
		case 0:
		  t = build_int_2 (0, 0);
		  goto got_it;
		case 1:
		  t = build_int_2 (int2l, int2h);
		  goto got_it;
		case 2:
		  temp = int2l + int2l;
		  int2h = int2h * 2 + (temp < int2l);
		  t = build_int_2 (temp, int2h);
		  goto got_it;
		case 3:
		  temp = int2l + int2l + int2l;
		  int2h = int2h * 3 + (temp < int2l);
		  t = build_int_2 (temp, int2h);
		  goto got_it;
		case 4:
		  temp = int2l + int2l;
		  int2h = int2h * 4 + (temp < int2l) << 1;
		  int2l = temp;
		  temp += temp;
		  int2h += (temp < int2l);
		  t = build_int_2 (temp, int2h);
		  goto got_it;
		case 8:
		  temp = int2l + int2l;
		  int2h = int2h * 8 + (temp < int2l) << 2;
		  int2l = temp;
		  temp += temp;
		  int2h += (temp < int2l) << 1;
		  int2l = temp;
		  temp += temp;
		  int2h += (temp < int2l);
		  t = build_int_2 (temp, int2h);
		  goto got_it;
		default:
		  break;
		}
	    }

	  if (int2h == 0)
	    {
	      if (int2l == 0)
		{
		  t = build_int_2 (0, 0);
		  break;
		}
	      if (int2l == 1)
		{
		  t = build_int_2 (int1l, int1h);
		  break;
		}
	    }

	  mul_double (int1l, int1h, int2l, int2h, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case TRUNC_DIV_EXPR: case ROUND_DIV_EXPR: 
	case FLOOR_DIV_EXPR: case CEIL_DIV_EXPR:
	case EXACT_DIV_EXPR:
	  if (int2h == 0 && int2l == 1)
	    {
	      t = build_int_2 (int1l, int1h);
	      break;
	    }
	  if (int1l == int2l && int1h == int2h)
	    {
	      if ((int1l | int1h) == 0)
		abort ();
	      t = build_int_2 (1, 0);
	      break;
	    }
	  div_and_round_double (code, uns, int1l, int1h, int2l, int2h,
				&low, &hi, &garbagel, &garbageh);
	  t = build_int_2 (low, hi);
	  break;

	case TRUNC_MOD_EXPR: case ROUND_MOD_EXPR: 
	case FLOOR_MOD_EXPR: case CEIL_MOD_EXPR:
	  div_and_round_double (code, uns, int1l, int1h, int2l, int2h,
				&garbagel, &garbageh, &low, &hi);
	  t = build_int_2 (low, hi);
	  break;

	case MIN_EXPR:
	case MAX_EXPR:
	  if (uns)
	    {
	      low = (((unsigned) int1h < (unsigned) int2h)
		     || (((unsigned) int1h == (unsigned) int2h)
			 && ((unsigned) int1l < (unsigned) int2l)));
	    }
	  else
	    {
	      low = ((int1h < int2h)
		     || ((int1h == int2h)
			 && ((unsigned) int1l < (unsigned) int2l)));
	    }
	  if (low == (code == MIN_EXPR))
	    t = build_int_2 (int1l, int1h);
	  else
	    t = build_int_2 (int2l, int2h);
	  break;

	default:
	  abort ();
	}
    got_it:
      TREE_TYPE (t) = TREE_TYPE (arg1);
      force_fit_type (t);
      return t;
    }
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
  if (TREE_CODE (arg1) == REAL_CST)
    {
      register REAL_VALUE_TYPE d1 = TREE_REAL_CST (arg1);
      register REAL_VALUE_TYPE d2 = TREE_REAL_CST (arg2);
      register REAL_VALUE_TYPE value;

      if (setjmp (combine_error))
	{
	  warning ("floating overflow in constant folding");
	  return build (code, TREE_TYPE (arg1), arg1, arg2);
	}
      set_float_handler (combine_error);

#ifdef REAL_ARITHMETIC
      REAL_ARITHMETIC (value, code, d1, d2);
#else
      switch (code)
	{
	case PLUS_EXPR:
	  value = d1 + d2;
	  break;

	case MINUS_EXPR:
	  value = d1 - d2;
	  break;

	case MULT_EXPR:
	  value = d1 * d2;
	  break;

	case RDIV_EXPR:
	  if (d2 == 0)
	    abort ();

	  value = d1 / d2;
	  break;

	case MIN_EXPR:
	  value = d1 < d2 ? d1 : d2;
	  break;

	case MAX_EXPR:
	  value = d1 > d2 ? d1 : d2;
	  break;

	default:
	  abort ();
	}
#endif /* no REAL_ARITHMETIC */
      set_float_handler (0);
      return build_real (TREE_TYPE (arg1), value);
    }
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
  if (TREE_CODE (arg1) == COMPLEX_CST)
    {
      register tree r1 = TREE_REALPART (arg1);
      register tree i1 = TREE_IMAGPART (arg1);
      register tree r2 = TREE_REALPART (arg2);
      register tree i2 = TREE_IMAGPART (arg2);
      register tree t;

      switch (code)
	{
	case PLUS_EXPR:
	  t = build_complex (combine (PLUS_EXPR, r1, r2),
			     combine (PLUS_EXPR, i1, i2));
	  break;

	case MINUS_EXPR:
	  t = build_complex (combine (MINUS_EXPR, r1, r2),
			     combine (MINUS_EXPR, i1, i2));
	  break;

	case MULT_EXPR:
	  t = build_complex (combine (MINUS_EXPR,
				      combine (MULT_EXPR, r1, r2),
				      combine (MULT_EXPR, i1, i2)),
			     combine (PLUS_EXPR,
				      combine (MULT_EXPR, r1, i2),
				      combine (MULT_EXPR, i1, r2)));
	  break;

	case RDIV_EXPR:
	  {
	    register tree magsquared
	      = combine (PLUS_EXPR,
			 combine (MULT_EXPR, r2, r2),
			 combine (MULT_EXPR, i2, i2));
	    t = build_complex (combine (RDIV_EXPR,
					combine (PLUS_EXPR,
						 combine (MULT_EXPR, r1, r2),
						 combine (MULT_EXPR, i1, i2)),
					magsquared),
			       combine (RDIV_EXPR,
					combine (MINUS_EXPR,
						 combine (MULT_EXPR, i1, r2),
						 combine (MULT_EXPR, r1, i2)),
					magsquared));
	  }
	  break;

	default:
	  abort ();
	}
      TREE_TYPE (t) = TREE_TYPE (arg1);
      return t;
    }
  return 0;
}

/* Given T, a tree representing type conversion of a constant,
   return a constant tree representing the result of conversion.  */

static tree
fold_convert (t)
     register tree t;
{
  register tree arg1 = TREE_OPERAND (t, 0);
  register tree type = TREE_TYPE (t);

  if (TREE_CODE (type) == POINTER_TYPE
      || TREE_CODE (type) == INTEGER_TYPE
      || TREE_CODE (type) == ENUMERAL_TYPE)
    {
      if (TREE_CODE (arg1) == INTEGER_CST)
	{
	  /* Given an integer constant, make new constant with new type,
	     appropriately sign-extended or truncated.  */
	  t = build_int_2 (TREE_INT_CST_LOW (arg1),
			   TREE_INT_CST_HIGH (arg1));
	  TREE_TYPE (t) = type;
	  force_fit_type (t);
	}
#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
      else if (TREE_CODE (arg1) == REAL_CST)
	{
	  if (REAL_VALUES_LESS (real_value_from_int_cst (TYPE_MAX_VALUE (type)),
				TREE_REAL_CST (arg1))
	      || REAL_VALUES_LESS (TREE_REAL_CST (arg1),
				   real_value_from_int_cst (TYPE_MIN_VALUE (type))))
	    {
	      warning ("real constant out of range for integer conversion");
	      return t;
	    }
#ifndef REAL_ARITHMETIC
	  {
	    REAL_VALUE_TYPE d;
	    int low, high;
	    int half_word = 1 << (HOST_BITS_PER_INT / 2);

	    d = TREE_REAL_CST (arg1);
	    if (d < 0)
	      d = -d;

	    high = (int) (d / half_word / half_word);
	    d -= (REAL_VALUE_TYPE) high * half_word * half_word;
	    low = (unsigned) d;
	    if (TREE_REAL_CST (arg1) < 0)
	      neg_double (low, high, &low, &high);
	    t = build_int_2 (low, high);
	  }
#else
	  {
	    int low, high;
	    REAL_VALUE_TO_INT (low, high, TREE_REAL_CST (arg1));
	    t = build_int_2 (low, high);
	  }
#endif
	  TREE_TYPE (t) = type;
	  force_fit_type (t);
	}
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
      TREE_TYPE (t) = type;
    }
  else if (TREE_CODE (type) == REAL_TYPE)
    {
#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
      if (TREE_CODE (arg1) == INTEGER_CST)
	return build_real_from_int_cst (type, arg1);
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
      if (TREE_CODE (arg1) == REAL_CST)
	return build_real (type, TREE_REAL_CST (arg1));
    }
  TREE_LITERAL (t) = 1;
  return t;
}

/* Return nonzero if two constants (that are not manifest constants)
   are necessarily equal.  It detects only the easiest, common case of
   equality.  */

static int
operand_equal_p (arg0, arg1)
     tree arg0, arg1;
{
  while ((TREE_CODE (arg0) == NOP_EXPR
	  || TREE_CODE (arg0) == CONVERT_EXPR)
	 && TYPE_MODE (TREE_TYPE (arg0)) == TYPE_MODE (TREE_TYPE (TREE_OPERAND (arg0, 0))))
    arg0 = TREE_OPERAND (arg0, 0);
  while ((TREE_CODE (arg1) == NOP_EXPR
	  || TREE_CODE (arg1) == CONVERT_EXPR)
	 && TYPE_MODE (TREE_TYPE (arg1)) == TYPE_MODE (TREE_TYPE (TREE_OPERAND (arg1, 0))))
    arg1 = TREE_OPERAND (arg1, 0);

  if (TREE_CODE (arg0) == TREE_CODE (arg1)
      && TREE_CODE (arg0) == ADDR_EXPR
      && TREE_OPERAND (arg0, 0) == TREE_OPERAND (arg1, 0))
    return 1;
  return 0;
}

#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)

/* Return 1 if ARG is a real constant with value zero.
   This function is not defined in the case where it is impossible
   to tell whether a real constant is zero (for cross-compilation).  */

static int
real_zerop (arg)
     tree arg;
{
#ifdef REAL_IS_NOT_DOUBLE
  tree t1 = build_real_from_int_cst (TREE_TYPE (arg), integer_zero_node);
  return REAL_VALUES_EQUAL (TREE_REAL_CST (arg), TREE_REAL_CST (t1));
#else
  return TREE_REAL_CST (arg) == 0;
#endif
}
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */

/* Perform constant folding and related simplification of EXPR.
   The related simplifications include x*1 => x, x*0 => 0, etc.,
   and application of the associative law.
   NOP_EXPR conversions may be removed freely (as long as we
   are careful not to change the C type of the overall expression)
   We cannot simplify through a CONVERT_EXPR, FIX_EXPR or FLOAT_EXPR,
   but we can constant-fold them if they have constant operands.  */

tree
fold (expr) 
     tree expr;
{
  register tree t = expr;
  tree type = TREE_TYPE (expr);
  register tree arg0, arg1;
  register enum tree_code code = TREE_CODE (t);
  register int kind;

  /* WINS will be nonzero when the switch is done
     if all operands are constant.

     LOSES will be nonzero when the switch is done
     if any operand is volatile.
     This inhibits optimizations such as  (foo () * 0) => 0.
     But identity-element optimizations such as
     (foo () * 1) => (foo ()) can be done even if LOSES is set.  */

  int wins = 1;
  int loses = 0;

  /* Return right away if already constant.  */
  if (TREE_LITERAL (t))
    {
      if (code == CONST_DECL)
	return DECL_INITIAL (t);
      return t;
    }
  
  kind = *tree_code_type[(int) code];
  if (kind == 'e' || kind == 'r')
    {
      register int len = tree_code_length[(int) code];
      register int i;
      for (i = 0; i < len; i++)
	{
	  if (TREE_OPERAND (t, i) == 0)
	    continue;		/* Valid for CALL_EXPR, at least.  */
	  if (TREE_CODE (TREE_OPERAND (t, i)) != INTEGER_CST
#if ! defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
	      && TREE_CODE (TREE_OPERAND (t, i)) != REAL_CST
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */
	      )
	    /* Note that TREE_LITERAL isn't enough:
	       static var addresses are constant but we can't
	       do arithmetic on them.  */
	    wins = 0;
	  if (TREE_VOLATILE (TREE_OPERAND (t, i)))
	    loses = 1;
	}
      arg0 = TREE_OPERAND (t, 0);
      if (len > 1)
	arg1 = TREE_OPERAND (t, 1);
    }

  /* Now WINS and LOSES are set as described above,
     ARG0 is the first operand of EXPR,
     and ARG1 is the second operand (if it has more than one operand).  */

  switch (code)
    {
    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case CONSTRUCTOR:
      return t;

    case CONST_DECL:
      return fold (DECL_INITIAL (t));

    case NOP_EXPR:
    case FLOAT_EXPR:
    case CONVERT_EXPR:
    case FIX_TRUNC_EXPR:
      /* Other kinds of FIX are not handled properly by fold_convert.  */
      if (!wins)
	{
	  TREE_LITERAL (t) = TREE_LITERAL (arg0);
	  return t;
	}
      return fold_convert (t);

#if 0  /* This loses on &"foo"[0].  */
    case ARRAY_REF:
	{
	  int i;

	  /* Fold an expression like: "foo"[2] */
	  if (TREE_CODE (arg0) == STRING_CST
	      && TREE_CODE (arg1) == INTEGER_CST
	      && !TREE_INT_CST_HIGH (arg1)
	      && (i = TREE_INT_CST_LOW (arg1)) < TREE_STRING_LENGTH (arg0))
	    {
	      t = build_int_2 (TREE_STRING_POINTER (arg0)[i], 0);
	      TREE_TYPE (t) = TREE_TYPE (TREE_TYPE (arg0));
	      force_fit_type (t);
	    }
	}
      return t;
#endif /* 0 */

    case RANGE_EXPR:
      TREE_LITERAL (t) = wins;
      return t;

    case NEGATE_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    {
	      if (TREE_INT_CST_LOW (arg0) == 0)
		t = build_int_2 (0, - TREE_INT_CST_HIGH (arg0));
	      else
		t = build_int_2 (- TREE_INT_CST_LOW (arg0),
				 ~ TREE_INT_CST_HIGH (arg0));
	      TREE_TYPE (t) = type;
	      force_fit_type (t);
	    }
	  else if (TREE_CODE (arg0) == REAL_CST)
	    t = build_real (type, REAL_VALUE_NEGATE (TREE_REAL_CST (arg0)));
	  TREE_TYPE (t) = type;
	}
      return t;

    case ABS_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    {
	      if (! TREE_UNSIGNED (type)
		  && TREE_INT_CST_HIGH (arg0) < 0)
		{
		  if (TREE_INT_CST_LOW (arg0) == 0)
		    t = build_int_2 (0, - TREE_INT_CST_HIGH (arg0));
		  else
		    t = build_int_2 (- TREE_INT_CST_LOW (arg0),
				     ~ TREE_INT_CST_HIGH (arg0));
		}
	    }
	  else if (TREE_CODE (arg0) == REAL_CST)
	    {
	      if (
#if defined (REAL_IS_NOT_DOUBLE)
		  REAL_VALUES_LESS (TREE_REAL_CST (arg0),
				    REAL_VALUE_ATOF ("0.0"))
#else
		  REAL_VALUES_LESS (TREE_REAL_CST (arg0), 0)
#endif
		  )
		t = build_real (type,
				REAL_VALUE_NEGATE (TREE_REAL_CST (arg0)));
	    }
	  TREE_TYPE (t) = type;
	}
      return t;

    case BIT_NOT_EXPR:
      if (wins)
	{
	  if (TREE_CODE (arg0) == INTEGER_CST)
	    t = build_int_2 (~ TREE_INT_CST_LOW (arg0),
			     ~ TREE_INT_CST_HIGH (arg0));
	  TREE_TYPE (t) = type;
	  force_fit_type (t);
	}
      return t;

    case PLUS_EXPR:
      if (integer_zerop (arg0))
	return convert (type, arg1);
      if (integer_zerop (arg1))
	return convert (type, arg0);
    associate:
      /* In most languages, can't associate operations on floats
	 through parentheses.  Rather than remember where the parentheses
	 were, we don't associate floats at all.  It shouldn't matter much.  */
      if (TREE_CODE (type) == REAL_TYPE)
	goto binary;
      /* The varsign == -1 cases happen only for addition and subtraction.
	 It says that the arg that was split was really CON minus VAR.
	 The rest of the code applies to all associative operations.  */
      if (!wins)
	{
	  tree var, con, tem;
	  int varsign;

	  if (split_tree (arg0, code, &var, &con, &varsign))
	    {
	      if (varsign == -1)
		{
		  /* EXPR is (CON-VAR) +- ARG1.  */
		  /* If it is + and VAR==ARG1, return just CONST.  */
		  if (code == PLUS_EXPR && operand_equal_p (var, arg1))
		    return convert (TREE_TYPE (t), con);
		    
		  /* Otherwise return (CON +- ARG1) - VAR.  */
		  TREE_SET_CODE (t, MINUS_EXPR);
		  TREE_OPERAND (t, 1) = var;
		  TREE_OPERAND (t, 0)
		    = fold (build (code, TREE_TYPE (t), con, arg1));
		}
	      else
		{
		  /* EXPR is (VAR+CON) +- ARG1.  */
		  /* If it is - and VAR==ARG1, return just CONST.  */
		  if (code == MINUS_EXPR && operand_equal_p (var, arg1))
		    return convert (TREE_TYPE (t), con);
		    
		  /* Otherwise return VAR +- (ARG1 +- CON).  */
		  TREE_OPERAND (t, 1) = tem
		    = fold (build (code, TREE_TYPE (t), arg1, con));
		  TREE_OPERAND (t, 0) = var;
		  if (integer_zerop (tem)
		      && (code == PLUS_EXPR || code == MINUS_EXPR))
		    return var;
		  /* If we have x +/- (c - d) [c an explicit integer]
		     change it to x -/+ (d - c) since if d is relocatable
		     then the latter can be a single immediate insn
		     and the former cannot.  */
		  if (TREE_CODE (tem) == MINUS_EXPR
		      && TREE_CODE (TREE_OPERAND (tem, 0)) == INTEGER_CST)
		    {
		      tree tem1 = TREE_OPERAND (tem, 1);
		      TREE_OPERAND (tem, 1) = TREE_OPERAND (tem, 0);
		      TREE_OPERAND (tem, 0) = tem1;
		      TREE_SET_CODE (t,
				     (code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR));
		    }
		}
	      return t;
	    }

	  if (split_tree (arg1, code, &var, &con, &varsign))
	    {
	      /* EXPR is ARG0 +- (CON +- VAR).  */
	      if (varsign == -1)
		TREE_SET_CODE (t,
			       (code == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR));
	      if (TREE_CODE (t) == MINUS_EXPR && operand_equal_p (var, arg0))
		{
		  /* If VAR and ARG0 cancel, return just CON or -CON.  */
		  if (code == PLUS_EXPR)
		    return convert (TREE_TYPE (t), con);
		  return fold (build (NEGATE_EXPR, TREE_TYPE (t),
				      convert (TREE_TYPE (t), con)));
		}
	      TREE_OPERAND (t, 0)
		= fold (build (code, TREE_TYPE (t), arg0, con));
	      TREE_OPERAND (t, 1) = var;
	      if (integer_zerop (TREE_OPERAND (t, 0))
		  && TREE_CODE (t) == PLUS_EXPR)
		return convert (TREE_TYPE (t), var);
	      return t;
	    }
	}
    binary:
#if defined (REAL_IS_NOT_DOUBLE) && ! defined (REAL_ARITHMETIC)
      if (TREE_CODE (arg1) == REAL_CST)
	return t;
#endif /* REAL_IS_NOT_DOUBLE, and no REAL_ARITHMETIC */
      {
	register tree t1 = NULL_TREE;
	if (wins)
	  t1 = combine (code, arg0, arg1);
	if (t1 != NULL_TREE) return t1;
	return t;
      }

    case MINUS_EXPR:
      if (! wins && integer_zerop (arg0))
	return build (NEGATE_EXPR, type, arg1);
      if (integer_zerop (arg1))
	return convert (type, arg0);
      /* Fold &x - &x.  This can happen from &x.foo - &x.  */
      if (operand_equal_p (arg0, arg1))
	return convert (TREE_TYPE (t), integer_zero_node);
      goto associate;

    case MULT_EXPR:
      if (!loses && integer_zerop (arg0))
	return convert (type, arg0);
      if (!loses && integer_zerop (arg1))
	return convert (type, arg1);
      if (integer_onep (arg0))
	return convert (type, arg1);
      if (integer_onep (arg1))
	return convert (type, arg0);
      goto associate;

    case BIT_IOR_EXPR:
      if (!loses && integer_all_onesp (arg0))
	return convert (type, arg0);
      if (!loses && integer_all_onesp (arg1))
	return convert (type, arg1);
    case BIT_XOR_EXPR:
      if (integer_zerop (arg0))
	return convert (type, arg1);
      if (integer_zerop (arg1))
	return convert (type, arg0);
      goto associate;

    case BIT_AND_EXPR:
      if (integer_all_onesp (arg0))
	return convert (type, arg1);
      if (integer_all_onesp (arg1))
	return convert (type, arg0);
      if (!loses && integer_zerop (arg0))
	return convert (type, arg0);
      if (!loses && integer_zerop (arg1))
	return convert (type, arg1);
      /* Simplify ((int)c & 0x377) into (int)c, if c is unsigned char.  */
      if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == NOP_EXPR
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg1, 0))))
	{
	  int prec = TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg1, 0)));
	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_INT
	      && (~TREE_INT_CST_LOW (arg0) & ((1 << prec) - 1)) == 0)
	    return build (NOP_EXPR, type, TREE_OPERAND (arg1, 0));
	}
      if (TREE_CODE (arg1) == INTEGER_CST && TREE_CODE (arg0) == NOP_EXPR
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (arg0, 0))))
	{
	  int prec = TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (arg0, 0)));
	  if (prec < BITS_PER_WORD && prec < HOST_BITS_PER_INT
	      && (~TREE_INT_CST_LOW (arg1) & ((1 << prec) - 1)) == 0)
	    return build (NOP_EXPR, type, TREE_OPERAND (arg0, 0));
	}
      goto associate;

    case BIT_ANDTC_EXPR:
      if (integer_all_onesp (arg0))
	return convert (type, arg1);
      if (integer_zerop (arg1))
	return convert (type, arg0);
      if (!loses && integer_zerop (arg0))
	return convert (type, arg0);
      if (!loses && integer_all_onesp (arg1))
	return combine (code, arg1, arg1);
      goto binary;

    case TRUNC_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case EXACT_DIV_EXPR:
    case RDIV_EXPR:
      if (integer_onep (arg1))
	return convert (type, arg0);
      if (integer_zerop (arg1))
	return t;
#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)
      if (TREE_CODE (arg1) == REAL_CST
	  && real_zerop (arg1))
	return t;
#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */

      goto binary;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case TRUNC_MOD_EXPR:
      if (!loses && integer_onep (arg1))
	return combine (code, arg1, arg1);
      if (integer_zerop (arg1))
	return t;
      goto binary;

    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:
      if (integer_zerop (arg1))
	return convert (type, arg0);
      goto binary;

    case MIN_EXPR: case MAX_EXPR:
      goto associate;

    case TRUTH_NOT_EXPR:
      /* Note that the operand of this must be an int
	 and its values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language,
	 but we don't handle values other than 1 correctly yet.)  */
      if (TREE_CODE (arg0) == INTEGER_CST)
	{
	  t = build_int_2 ((TREE_INT_CST_LOW (arg0) == 0
			    && TREE_INT_CST_HIGH (arg0) == 0),
			   0);
	  TREE_TYPE (t) = integer_type_node;
	}
      return t;

    case TRUTH_ANDIF_EXPR:
      /* Note that the operands of this must be ints
	 and their values must be 0 or 1.
	 ("true" is a fixed value perhaps depending on the language.)  */
      /* If first arg is constant zero, return it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && integer_zerop (arg0))
	return arg0;
    case TRUTH_AND_EXPR:
      /* If either arg is constant true, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return arg1;
      if (TREE_CODE (arg1) == INTEGER_CST && ! integer_zerop (arg1))
	return arg0;
      /* Both known to be zero => return zero.  */
      if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
	return arg0;
      return t;

    case TRUTH_ORIF_EXPR:
      /* Note that the operands of this must be ints
	 and their values must be 0 or true.
	 ("true" is a fixed value perhaps depending on the language.)  */
      /* If first arg is constant true, return it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && ! integer_zerop (arg0))
	return arg0;
    case TRUTH_OR_EXPR:
      /* If either arg is constant zero, drop it.  */
      if (TREE_CODE (arg0) == INTEGER_CST && integer_zerop (arg0))
	return arg1;
      if (TREE_CODE (arg1) == INTEGER_CST && integer_zerop (arg1))
	return arg0;
      /* Both known to be true => return true.  */
      if (TREE_CODE (arg0) == INTEGER_CST && TREE_CODE (arg1) == INTEGER_CST)
	return arg0;
      return t;

    case EQ_EXPR:
    case NE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
    case LE_EXPR:
    case GE_EXPR:
      /* If one arg is a constant integer, put it last.  */
      if (TREE_CODE (arg0) == INTEGER_CST
	  && TREE_CODE (arg1) != INTEGER_CST)
	{
	  TREE_OPERAND (t, 0) = arg1;
	  TREE_OPERAND (t, 1) = arg0;
	  arg0 = TREE_OPERAND (t, 0);
	  arg1 = TREE_OPERAND (t, 1);
	  switch (code)
	    {
	    case GT_EXPR:
	      code = LT_EXPR;
	      break;
	    case GE_EXPR:
	      code = LE_EXPR;
	      break;
	    case LT_EXPR:
	      code = GT_EXPR;
	      break;
	    case LE_EXPR:
	      code = GE_EXPR;
	      break;
	    }
	  TREE_SET_CODE (t, code);
	}

      /* Convert foo++ == CONST into ++foo == CONST + INCR.
	 First, see if one arg is constant; find the constant arg
	 and the other one.  */
      {
	tree constop = 0, varop;
	tree *constoploc;

	if (TREE_LITERAL (arg1))
	  constoploc = &TREE_OPERAND (t, 1), constop = arg1, varop = arg0;
	if (TREE_LITERAL (arg0))
	  constoploc = &TREE_OPERAND (t, 0), constop = arg0, varop = arg1;

	if (constop && TREE_CODE (varop) == POSTINCREMENT_EXPR)
	  {
	    tree newconst
	      = fold (build (PLUS_EXPR, TREE_TYPE (constop),
			     constop, TREE_OPERAND (varop, 1)));
	    /* This optimization is invalid for ordered comparisons
	       if CONST+INCR overflows or if foo+incr might overflow.
	       For pointer types we assume overflow doesn't happen.  */
	    if (TREE_CODE (TREE_TYPE (varop)) == POINTER_TYPE
		|| code == EQ_EXPR || code == NE_EXPR)
	      {
		TREE_SET_CODE (varop, PREINCREMENT_EXPR);
		*constoploc = newconst;
		return t;
	      }
	  }
	else if (constop && TREE_CODE (varop) == POSTDECREMENT_EXPR)
	  {
	    tree newconst
	      = fold (build (MINUS_EXPR, TREE_TYPE (constop),
			     constop, TREE_OPERAND (varop, 1)));
	    if (TREE_CODE (TREE_TYPE (varop)) == POINTER_TYPE
		|| code == EQ_EXPR || code == NE_EXPR)
	      {
		TREE_SET_CODE (varop, PREDECREMENT_EXPR);
		*constoploc = newconst;
		return t;
	      }
	  }
      }

      /* Change X >= CST to X > (CST - 1) if CST is positive.  */
      if (TREE_CODE (arg1) == INTEGER_CST
	  && TREE_CODE (arg0) != INTEGER_CST
	  && ! tree_int_cst_lt (arg1, integer_one_node))
	{
	  switch (TREE_CODE (t))
	    {
	    case GE_EXPR:
	      code = GT_EXPR;
	      TREE_SET_CODE (t, code);
	      arg1 = combine (MINUS_EXPR, arg1, integer_one_node);
	      TREE_OPERAND (t, 1) = arg1;
	      break;

	    case LT_EXPR:
	      code = LE_EXPR;
	      TREE_SET_CODE (t, code);
	      arg1 = combine (MINUS_EXPR, arg1, integer_one_node);
	      TREE_OPERAND (t, 1) = arg1;
	    }
	}

      /* An unsigned comparison against 0 can be simplified.  */
      if (integer_zerop (arg1)
	  && (TREE_CODE (TREE_TYPE (arg1)) == INTEGER_TYPE
	      || TREE_CODE (TREE_TYPE (arg1)) == POINTER_TYPE)
	  && TREE_UNSIGNED (TREE_TYPE (arg1)))
	{
	  switch (TREE_CODE (t))
	    {
	    case GT_EXPR:
	      TREE_SET_CODE (t, NE_EXPR);
	      break;
	    case LE_EXPR:
	      TREE_SET_CODE (t, EQ_EXPR);
	      break;
	    case GE_EXPR:
	      return build (COMPOUND_EXPR, integer_type_node,
			    arg0, integer_one_node);
	    case LT_EXPR:
	      return build (COMPOUND_EXPR, integer_type_node,
			    arg0, integer_zero_node);
	    }
	}

      /* To compute GT, swap the arguments and do LT.
	 To compute GE, do LT and invert the result.
	 To compute LE, swap the arguments, do LT and invert the result.
	 To compute NE, do EQ and invert the result.  */
      if (code == LE_EXPR || code == GT_EXPR)
	{
	  register tree temp = arg0;
	  arg0 = arg1;
	  arg1 = temp;
	}

      /* Compute a result for LT or EQ if args permit;
	 otherwise return T.  */
      if (TREE_CODE (arg0) == INTEGER_CST
	  && TREE_CODE (arg1) == INTEGER_CST)
	{
	  if (code == EQ_EXPR || code == NE_EXPR)
	    t = build_int_2
	      (TREE_INT_CST_LOW (arg0) == TREE_INT_CST_LOW (arg1)
	       && TREE_INT_CST_HIGH (arg0) == TREE_INT_CST_HIGH (arg1),
	       0);
	  else
	    t = build_int_2 ((TREE_UNSIGNED (TREE_TYPE (arg0))
			      ? INT_CST_LT_UNSIGNED (arg0, arg1)
			      : INT_CST_LT (arg0, arg1)),
			     0);
	}
      else if (TREE_CODE (arg1) == INTEGER_CST
	       && TREE_LITERAL (arg0)
	       && TREE_CODE (arg0) == ADDR_EXPR
	       && (code == EQ_EXPR || code == NE_EXPR))
	{
	  t = build_int_2 (0, 0);
	}
      else if (TREE_CODE (arg0) == REAL_CST
	       && TREE_CODE (arg1) == REAL_CST)
	{
	  if (code == EQ_EXPR || code == NE_EXPR)
	    t = build_int_2 (REAL_VALUES_EQUAL (TREE_REAL_CST (arg0),
						TREE_REAL_CST (arg1)),
			     0);
	  else
	    t = build_int_2 (REAL_VALUES_LESS (TREE_REAL_CST (arg0),
					       TREE_REAL_CST (arg1)),
			     0);
	}
      else
	return t;

      /* If what we want is other than LT or EQ, invert the result.  */
      if (code == GE_EXPR || code == LE_EXPR || code == NE_EXPR)
	TREE_INT_CST_LOW (t) ^= 1;
      TREE_TYPE (t) = type;
      return t;

    case COND_EXPR:
      if (TREE_LITERAL (arg0))
	return TREE_OPERAND (expr, (integer_zerop (arg0) ? 2 : 1));
      return t;

    default:
      return t;
    } /* switch (code) */
}
