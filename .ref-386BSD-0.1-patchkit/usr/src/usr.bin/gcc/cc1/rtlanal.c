/* Analyze RTL for C-Compiler
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

extern void note_stores ();
static int reg_set_p ();

/* Return 1 if the value of X is unstable
   (would be different at a different point in the program).
   The frame pointer, arg pointer, etc. are considered stable
   (within one function) and so is anything marked `unchanging'.  */

int
rtx_unstable_p (x)
     rtx x;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register char *fmt;

  if (code == MEM)
    return ! RTX_UNCHANGING_P (x);

  if (code == QUEUED)
    return 1;

  if (code == CONST || code == CONST_INT)
    return 0;

  if (code == REG)
    return ! (REGNO (x) == FRAME_POINTER_REGNUM
	      || REGNO (x) == ARG_POINTER_REGNUM
	      || RTX_UNCHANGING_P (x));

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      if (rtx_unstable_p (XEXP (x, i)))
	return 1;
  return 0;
}

/* Return 1 if X has a value that can vary even between two
   executions of the program.  0 means X can be compared reliably
   against certain constants or near-constants.
   The frame pointer and the arg pointer are considered constant.  */

int
rtx_varies_p (x)
     rtx x;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register char *fmt;

  if (code == MEM)
    return 1;

  if (code == QUEUED)
    return 1;

  if (code == CONST || code == CONST_INT)
    return 0;

  if (code == REG)
    return ! (REGNO (x) == FRAME_POINTER_REGNUM
	      || REGNO (x) == ARG_POINTER_REGNUM);

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      if (rtx_varies_p (XEXP (x, i)))
	return 1;
  return 0;
}

/* Return 1 if X refers to a memory location whose address 
   cannot be compared reliably with constant addresses,
   or if X refers to a BLKmode memory object.  */

int
rtx_addr_varies_p (x)
     rtx x;
{
  register enum rtx_code code;
  register int i;
  register char *fmt;

  if (x == 0)
    return 0;

  code = GET_CODE (x);
  if (code == MEM)
    return GET_MODE (x) == BLKmode || rtx_varies_p (XEXP (x, 0));

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      if (rtx_addr_varies_p (XEXP (x, i)))
	return 1;
  return 0;
}

/* Nonzero if register REG appears somewhere within IN.
   Also works if REG is not a register; in this case it checks
   for a subexpression of IN that is Lisp "equal" to REG.  */

int
reg_mentioned_p (reg, in)
     register rtx reg, in;
{
  register char *fmt;
  register int i;
  register enum rtx_code code;

  if (in == 0)
    return 0;

  if (reg == in)
    return 1;

  code = GET_CODE (in);

  switch (code)
    {
      /* Compare registers by number.  */
    case REG:
      return GET_CODE (reg) == REG && REGNO (in) == REGNO (reg);

      /* These codes have no constituent expressions
	 and are unique.  */
    case CC0:
    case PC:
      return 0;

    case CONST_INT:
      return GET_CODE (reg) == CONST_INT && INTVAL (in) == INTVAL (reg);
    }

  if (GET_CODE (reg) == code && rtx_equal_p (reg, in))
    return 1;

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (in, i) - 1; j >= 0; j--)
	    if (reg_mentioned_p (reg, XVECEXP (in, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e'
	       && reg_mentioned_p (reg, XEXP (in, i)))
	return 1;
    }
  return 0;
}

/* Nonzero if register REG is used in an insn between
   FROM_INSN and TO_INSN (exclusive of those two).  */

int
reg_used_between_p (reg, from_insn, to_insn)
     rtx reg, from_insn, to_insn;
{
  register rtx insn;
  register RTX_CODE code;
  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (((code = GET_CODE (insn)) == INSN
	 || code == JUMP_INSN || code == CALL_INSN)
	&& reg_mentioned_p (reg, PATTERN (insn)))
      return 1;
  return 0;
}

/* Nonzero if register REG is set or clobbered in an insn between
   FROM_INSN and TO_INSN (exclusive of those two).
   Does not notice increments, only SET and CLOBBER.  */

int
reg_set_between_p (reg, from_insn, to_insn)
     rtx reg, from_insn, to_insn;
{
  register rtx insn;
  register RTX_CODE code;
  for (insn = NEXT_INSN (from_insn); insn != to_insn; insn = NEXT_INSN (insn))
    if (((code = GET_CODE (insn)) == INSN
	 || code == JUMP_INSN || code == CALL_INSN)
	&& reg_set_p (reg, PATTERN (insn)))
      return 1;
  return 0;
}

/* Internals of reg_set_between_p.  */

static rtx reg_set_reg;
static int reg_set_flag;

static void
reg_set_p_1 (x)
     rtx x;
{
  if (reg_overlap_mentioned_p (reg_set_reg, x))
    reg_set_flag = 1;
}

static int
reg_set_p (reg, insn)
     rtx reg, insn;
{
  reg_set_reg = reg;
  reg_set_flag = 0;
  note_stores (insn, reg_set_p_1);
  return reg_set_flag;
}

/* Return nonzero if hard register in range [REGNO, ENDREGNO)
   appears either explicitly or implicitly in X
   other than being stored into.

   References contained within the substructure at LOC do not count.
   LOC may be zero, meaning don't ignore anything.  */

int
refers_to_regno_p (regno, endregno, x, loc)
     int regno, endregno;
     rtx x;
     rtx *loc;
{
  register int i;
  register RTX_CODE code;
  register char *fmt;

 repeat:
  code = GET_CODE (x);
  if (code == REG)
    {
      i = REGNO (x);
      return (endregno > i && regno < i + HARD_REGNO_NREGS (i, GET_MODE (x)));
    }

  if (code == SET)
    {
      /* Note setting a SUBREG counts as referring to the REG it is in!  */
      if (GET_CODE (SET_DEST (x)) != REG
	  && refers_to_regno_p (regno, endregno, SET_DEST (x), loc))
	return 1;
      if (loc == &SET_SRC (x))
	return 0;
      x = SET_SRC (x);
      goto repeat;
    }

  if (code == CLOBBER)
    {
      if (GET_CODE (SET_DEST (x)) != REG
	  && refers_to_regno_p (regno, endregno, SET_DEST (x), loc))
	return 1;
      return 0;
    }

  /* X does not match, so try its subexpressions.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e' && loc != &XEXP (x, i))
	{
	  if (i == 0)
	    {
	      x = XEXP (x, 0);
	      goto repeat;
	    }
	  else
	    if (refers_to_regno_p (regno, endregno, XEXP (x, i), loc))
	      return 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >=0; j--)
	    if (loc != &XVECEXP (x, i, j)
		&& refers_to_regno_p (regno, endregno, XVECEXP (x, i, j), loc))
	      return 1;
	}
    }
  return 0;
}

/* Nonzero if X contains any reg that overlaps hard register REG.  */

int
reg_overlap_mentioned_p (reg, x)
     rtx reg, x;
{
  int regno = REGNO (reg);
  int endregno = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
  return refers_to_regno_p (regno, endregno, x, 0);
}

/* This is 1 until after reload pass.  */
int rtx_equal_function_value_matters;

/* Return 1 if X and Y are identical-looking rtx's.
   This is the Lisp function EQUAL for rtx arguments.  */

int
rtx_equal_p (x, y)
     rtx x, y;
{
  register int i;
  register int j;
  register enum rtx_code code;
  register char *fmt;

  if (x == y)
    return 1;
  if (x == 0 || y == 0)
    return 0;

  code = GET_CODE (x);
  /* Rtx's of different codes cannot be equal.  */
  if (code != GET_CODE (y))
    return 0;

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.
     (REG:SI x) and (REG:HI x) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* These three types of rtx's can be compared nonrecursively.  */
  /* Until the end of reload,
     don't consider the a reference to the return register of the current
     function the same as the return from a called function.  This eases
     the job of function integration.  Once the distinction no longer
     matters, the insn will be deleted.  */
  if (code == REG)
    return (REGNO (x) == REGNO (y)
	    && (! rtx_equal_function_value_matters
		|| REG_FUNCTION_VALUE_P (x) == REG_FUNCTION_VALUE_P (y)));
  if (code == LABEL_REF)
    return XEXP (x, 0) == XEXP (y, 0);
  if (code == SYMBOL_REF)
    return XSTR (x, 0) == XSTR (y, 0);

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      switch (fmt[i])
	{
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'E':
	  /* Two vectors must have the same length.  */
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;

	  /* And the corresponding elements must match.  */
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (rtx_equal_p (XVECEXP (x, i, j), XVECEXP (y, i, j)) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_p (XEXP (x, i), XEXP (y, i)) == 0)
	    return 0;
	  break;

	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'u':
	  /* These are just backpointers, so they don't matter.  */
	  break;

	case '0':
	  break;

	  /* It is believed that rtx's at this level will never
	     contain anything but integers and other rtx's,
	     except for within LABEL_REFs and SYMBOL_REFs.  */
	default:
	  abort ();
	}
    }
  return 1;
}

/* Call FUN on each register or MEM that is stored into or clobbered by X.
   (X would be the pattern of an insn).
   FUN receives two arguments:
     the REG, MEM, CC0 or PC being stored in or clobbered,
     the SET or CLOBBER rtx that does the store.  */
     
void
note_stores (x, fun)
     register rtx x;
     void (*fun) ();
{
  if ((GET_CODE (x) == SET || GET_CODE (x) == CLOBBER))
    {
      register rtx dest = SET_DEST (x);
      while (GET_CODE (dest) == SUBREG
	     || GET_CODE (dest) == ZERO_EXTRACT
	     || GET_CODE (dest) == SIGN_EXTRACT
	     || GET_CODE (dest) == STRICT_LOW_PART)
	dest = XEXP (dest, 0);
      (*fun) (dest, x);
    }
  else if (GET_CODE (x) == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  register rtx y = XVECEXP (x, 0, i);
	  if (GET_CODE (y) == SET || GET_CODE (y) == CLOBBER)
	    {
	      register rtx dest = SET_DEST (y);
	      while (GET_CODE (dest) == SUBREG
		     || GET_CODE (dest) == ZERO_EXTRACT
		     || GET_CODE (dest) == SIGN_EXTRACT
		     || GET_CODE (dest) == STRICT_LOW_PART)
		dest = XEXP (dest, 0);
	      (*fun) (dest, XVECEXP (x, 0, i));
	    }
	}
    }
}

/* Return nonzero if register REG's old contents don't survive after INSN.
   This can be because REG dies in INSN or because INSN entirely sets REG.

   "Entirely set" means set directly and not through a SUBREG,
   ZERO_EXTRACT or SIGN_EXTRACT, so no trace of the old contents remains.

   REG may be a hard or pseudo reg.  Renumbering is not taken into account,
   but for this use that makes no difference, since regs don't overlap
   during their lifetimes.  Therefore, this function may be used
   at any time after deaths have been computed (in flow.c).  */

int
dead_or_set_p (insn, reg)
     rtx insn;
     rtx reg;
{
  register rtx link;
  register int regno = REGNO (reg);

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if ((REG_NOTE_KIND (link) == REG_DEAD
	 || REG_NOTE_KIND (link) == REG_INC)
	&& REGNO (XEXP (link, 0)) == regno)
      return 1;

  if (GET_CODE (PATTERN (insn)) == SET)
    return (GET_CODE (SET_DEST (PATTERN (insn))) == REG
	    && REGNO (SET_DEST (PATTERN (insn))) == regno);
  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
	{
	  if (GET_CODE (XVECEXP (PATTERN (insn), 0, i)) == SET
	      && GET_CODE (SET_DEST (XVECEXP (PATTERN (insn), 0, i))) == REG
              && REGNO (SET_DEST (XVECEXP (PATTERN (insn), 0, i))) == regno)
	    return 1;
	}
    }
  return 0;
}

/* Return the reg-note of kind KIND in insn INSN, if there is one.
   If DATUM is nonzero, look for one whose datum is DATUM.  */

rtx
find_reg_note (insn, kind, datum)
     rtx insn;
     enum reg_note kind;
     rtx datum;
{
  register rtx link;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == kind
	&& (datum == 0 || datum == XEXP (link, 0)))
      return link;
  return 0;
}

/* Return the reg-note of kind KIND in insn INSN which applies to register
   number REGNO, if any.  Return 0 if there is no such reg-note.  */

rtx
find_regno_note (insn, kind, regno)
     rtx insn;
     enum reg_note kind;
     int regno;
{
  register rtx link;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == kind
	&& REGNO (XEXP (link, 0)) == regno)
      return link;
  return 0;
}

/* Nonzero if FROM precedes TO with no intervening labels.  */

int
no_labels_between (from, to)
     register rtx from, to;
{
  register rtx p = to;

  while (1)
    {
      p = PREV_INSN (p);
      if (p == 0)
	return 0;
      if (p == from)
	return 1;
      if (GET_CODE (p) == CODE_LABEL)
	return 0;
    }
}

/* Nonzero if X contains any volatile memory references
   or volatile ASM_OPERANDS expressions.  */

int
volatile_refs_p (x)
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
    case CONST_DOUBLE:
    case CC0:
    case PC:
    case REG:
    case CLOBBER:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

    case CALL:
      return 1;

    case MEM:
    case ASM_OPERANDS:
      if (MEM_VOLATILE_P (x))
	return 1;
    }

  /* Recursively scan the operands of this expression.  */

  {
    register char *fmt = GET_RTX_FORMAT (code);
    register int i;
    
    for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
      {
	if (fmt[i] == 'e')
	  {
	    if (volatile_refs_p (XEXP (x, i)))
	      return 1;
	  }
	if (fmt[i] == 'E')
	  {
	    register int j;
	    for (j = 0; j < XVECLEN (x, i); j++)
	      if (volatile_refs_p (XVECEXP (x, i, j)))
		return 1;
	  }
      }
  }
  return 0;
}

/* Return nonzero if evaluating rtx X might cause a trap.  */

int
may_trap_p (x)
     rtx x;
{
  int i;
  enum rtx_code code;
  char *fmt;

  if (x == 0)
    return 0;
  code = GET_CODE (x);
  switch (code)
    {
      /* Handle these cases fast.  */
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
    case PC:
    case CC0:
    case REG:
      return 0;

      /* Memory ref can trap unless it's a static var or a stack slot.  */
    case MEM:
      return rtx_varies_p (XEXP (x, 0));

      /* Division by a non-constant might trap.  */
    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      if (! CONSTANT_P (XEXP (x, 1))
	  && GET_CODE (XEXP (x, 1)) != CONST_DOUBLE)
	return 1;
      if (XEXP (x, 1) == const0_rtx)
	return 1;
    default:
      /* Any floating arithmetic may trap.  */
      if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
	return 1;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (may_trap_p (XEXP (x, i)))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (may_trap_p (XVECEXP (x, i, j)))
	      return 1;
	}
    }
  return 0;
}
