/* Emit RTL for the GNU C-Compiler expander.
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


/* Middle-to-low level generation of rtx code and insns.

   This file contains the functions `gen_rtx', `gen_reg_rtx'
   and `gen_label_rtx' that are the usual ways of creating rtl
   expressions for most purposes.

   It also has the functions for creating insns and linking
   them in the doubly-linked chain.

   The patterns of the insns are created by machine-dependent
   routines in insn-emit.c, which is generated automatically from
   the machine description.  These routines use `gen_rtx' to make
   the individual rtx's of the pattern; what is machine dependent
   is the kind of rtx's they make and what arguments they use.  */

#include "config.h"
#include <stdio.h>
#include "gvarargs.h"
#include "rtl.h"
#include "regs.h"
#include "insn-config.h"
#include "real.h"

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) < (B) ? (A) : (B))

/* This is reset to FIRST_PSEUDO_REGISTER at the start each function.
   After rtl generation, it is 1 plus the largest register number used.  */

int reg_rtx_no = FIRST_PSEUDO_REGISTER;

/* This is *not* reset after each function.  It gives each CODE_LABEL
   in the entire compilation a unique label number.  */

static int label_num = 1;

/* Value of `label_num' at start of current function.  */

static int first_label_num;

/* Nonzero means do not generate NOTEs for source line numbers.  */

static int no_line_numbers;

/* Commonly used rtx's, so that we only need space for one copy.
   These are initialized once for the entire compilation.
   All of these except perhaps fconst0_rtx and dconst0_rtx
   are unique; no other rtx-object will be equal to any of these.  */

rtx pc_rtx;			/* (PC) */
rtx cc0_rtx;			/* (CC0) */
rtx cc1_rtx;			/* (CC1) (not actually used nowadays) */
rtx const0_rtx;			/* (CONST_INT 0) */
rtx const1_rtx;			/* (CONST_INT 1) */
rtx fconst0_rtx;		/* (CONST_DOUBLE:SF 0) */
rtx dconst0_rtx;		/* (CONST_DOUBLE:DF 0) */

/* All references to the following fixed hard registers go through
   these unique rtl objects.  On machines where the frame-pointer and
   arg-pointer are the same register, they use the same unique object.

   After register allocation, other rtl objects which used to be pseudo-regs
   may be clobbered to refer to the frame-pointer register.
   But references that were originally to the frame-pointer can be
   distinguished from the others because they contain frame_pointer_rtx.

   In an inline procedure, the stack and frame pointer rtxs may not be
   used for anything else.  */
rtx stack_pointer_rtx;		/* (REG:Pmode STACK_POINTER_REGNUM) */
rtx frame_pointer_rtx;		/* (REG:Pmode FRAME_POINTER_REGNUM) */
rtx arg_pointer_rtx;		/* (REG:Pmode ARG_POINTER_REGNUM) */
rtx struct_value_rtx;		/* (REG:Pmode STRUCT_VALUE_REGNUM) */
rtx struct_value_incoming_rtx;	/* (REG:Pmode STRUCT_VALUE_INCOMING_REGNUM) */
rtx static_chain_rtx;		/* (REG:Pmode STATIC_CHAIN_REGNUM) */
rtx static_chain_incoming_rtx;	/* (REG:Pmode STATIC_CHAIN_INCOMING_REGNUM) */

/* The ends of the doubly-linked chain of rtl for the current function.
   Both are reset to null at the start of rtl generation for the function.
   
   start_sequence saves both of these on `sequence_stack' and then
   starts a new, nested sequence of insns.  */

static rtx first_insn = NULL;
static rtx last_insn = NULL;

/* Stack of pending (incomplete) sequences saved by `start_sequence'.
   This looks like
   (INSN_LIST saved-first-insn
              (INSN_LIST saved-last-insn ...more saved sequences...)).
   The main insn-chain is saved in the last two links of the chain,
   unless the chain is empty.  */

rtx sequence_stack = 0;

/* INSN_UID for next insn emitted.
   Reset to 1 for each function compiled.  */

static int cur_insn_uid = 1;

/* Line number and source file of the last line-number NOTE emitted.
   This is used to avoid generating duplicates.  */

static int last_linenum = 0;
static char *last_filename = 0;

/* A vector indexed by pseudo reg number.  The allocated length
   of this vector is regno_pointer_flag_length.  Since this
   vector is needed during the expansion phase when the total
   number of registers in the function is not yet known,
   it is copied and made bigger when necessary.  */

char *regno_pointer_flag;
int regno_pointer_flag_length;

/* Indexed by pseudo register number, gives the rtx for that pseudo.
   Allocated in parallel with regno_pointer_flag.  */

rtx *regno_reg_rtx;

/* Filename and line number of last line-number note,
   whether we actually emitted it or not.  */
extern char *emit_filename;
extern int emit_lineno;

rtx change_address ();

/* rtx gen_rtx (code, mode, [element1, ..., elementn])
**
**	    This routine generates an RTX of the size specified by
**	<code>, which is an RTX code.   The RTX structure is initialized
**	from the arguments <element1> through <elementn>, which are
**	interpreted according to the specific RTX type's format.   The
**	special machine mode associated with the rtx (if any) is specified
**	in <mode>.
**
**	    gen_rtx() can be invoked in a way which resembles the lisp-like
**	rtx it will generate.   For example, the following rtx structure:
**
**	      (plus:QI (mem:QI (reg:SI 1))
**		       (mem:QI (plusw:SI (reg:SI 2) (reg:SI 3))))
**
**		...would be generated by the following C code:
**
**	    	gen_rtx (PLUS, QImode,
**		    gen_rtx (MEM, QImode,
**			gen_rtx (REG, SImode, 1)),
**		    gen_rtx (MEM, QImode,
**			gen_rtx (PLUS, SImode,
**			    gen_rtx (REG, SImode, 2),
**			    gen_rtx (REG, SImode, 3)))),
*/

/*VARARGS2*/
rtx
gen_rtx (va_alist)
     va_dcl
{
  va_list p;
  enum rtx_code code;
  enum machine_mode mode;
  register int i;		/* Array indices...			*/
  register char *fmt;		/* Current rtx's format...		*/
  register rtx rt_val;		/* RTX to return to caller...		*/

  va_start (p);
  code = va_arg (p, enum rtx_code);
  mode = va_arg (p, enum machine_mode);

  if (code == CONST_INT)
    {
      int arg = va_arg (p, int);
      if (arg == 0)
	return const0_rtx;
      if (arg == 1)
	return const1_rtx;
      rt_val = rtx_alloc (code);
      INTVAL (rt_val) = arg;
    }
  else
    {
      rt_val = rtx_alloc (code);	/* Allocate the storage space.  */
      rt_val->mode = mode;		/* Store the machine mode...  */

      fmt = GET_RTX_FORMAT (code);	/* Find the right format...  */
      for (i = 0; i < GET_RTX_LENGTH (code); i++)
	{
	  switch (*fmt++)
	    {
	    case '0':		/* Unused field.  */
	      break;

	    case 'i':		/* An integer?  */
	      XINT (rt_val, i) = va_arg (p, int);
	      break;

	    case 's':		/* A string?  */
	      XSTR (rt_val, i) = va_arg (p, char *);
	      break;

	    case 'e':		/* An expression?  */
	    case 'u':		/* An insn?  Same except when printing.  */
	      XEXP (rt_val, i) = va_arg (p, rtx);
	      break;

	    case 'E':		/* An RTX vector?  */
	      XVEC (rt_val, i) = va_arg (p, rtvec);
	      break;

	    default:
	      abort();
	    }
	}
    }
  va_end (p);
  return rt_val;		/* Return the new RTX...		*/
}

/* gen_rtvec (n, [rt1, ..., rtn])
**
**	    This routine creates an rtvec and stores within it the
**	pointers to rtx's which are its arguments.
*/

/*VARARGS1*/
rtvec
gen_rtvec (va_alist)
     va_dcl
{
  int n, i;
  va_list p;
  rtx *vector;

  va_start (p);
  n = va_arg (p, int);

  if (n == 0)
    return NULL_RTVEC;		/* Don't allocate an empty rtvec...	*/

  vector = (rtx *) alloca (n * sizeof (rtx));
  for (i = 0; i < n; i++)
    vector[i] = va_arg (p, rtx);
  va_end (p);

  return gen_rtvec_v (n, vector);
}

rtvec
gen_rtvec_v (n, argp)
     int n;
     rtx *argp;
{
  register int i;
  register rtvec rt_val;

  if (n == 0)
    return NULL_RTVEC;		/* Don't allocate an empty rtvec...	*/

  rt_val = rtvec_alloc (n);	/* Allocate an rtvec...			*/

  for (i = 0; i < n; i++)
    rt_val->elem[i].rtx = *argp++;

  return rt_val;
}

/* Generate a REG rtx for a new pseudo register of mode MODE.
   This pseudo is assigned the next sequential register number.  */

rtx
gen_reg_rtx (mode)
     enum machine_mode mode;
{
  register rtx val;

  /* Make sure regno_pointer_flag and regno_reg_rtx are large
     enough to have an element for this pseudo reg number.  */

  if (reg_rtx_no == regno_pointer_flag_length)
    {
      rtx *new1;
      char *new =
	(char *) oballoc (regno_pointer_flag_length * 2);
      bzero (new, regno_pointer_flag_length * 2);
      bcopy (regno_pointer_flag, new, regno_pointer_flag_length);
      regno_pointer_flag = new;

      new1 = (rtx *) oballoc (regno_pointer_flag_length * 2 * sizeof (rtx));
      bzero (new1, regno_pointer_flag_length * 2 * sizeof (rtx));
      bcopy (regno_reg_rtx, new1, regno_pointer_flag_length * sizeof (rtx));
      regno_reg_rtx = new1;

      regno_pointer_flag_length *= 2;
    }

  val = gen_rtx (REG, mode, reg_rtx_no);
  regno_reg_rtx[reg_rtx_no++] = val;
  return val;
}

/* Identify REG as a probable pointer register.  */

void
mark_reg_pointer (reg)
     rtx reg;
{
  REGNO_POINTER_FLAG (REGNO (reg)) = 1;
}

/* Return 1 plus largest pseudo reg number used in the current function.  */

int
max_reg_num ()
{
  return reg_rtx_no;
}

/* Return 1 + the largest label number used so far.  */

int
max_label_num ()
{
  return label_num;
}

/* Return first label number used in this function (if any were used).  */

int
get_first_label_num ()
{
  return first_label_num;
}

/* Assuming that X is an rtx (MEM, REG or SUBREG) for a fixed-point number,
   return a MEM or SUBREG rtx that refers to the least-significant part of X.
   MODE specifies how big a part of X to return;
   it must not be larger than a word.
   If X is a MEM whose address is a QUEUED, the value may be so also.  */

rtx
gen_lowpart (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  /* This case loses if X is a subreg.  To catch bugs early,
     complain if an invalid MODE is used even in other cases.  */
  if (GET_MODE_SIZE (mode) > UNITS_PER_WORD
      && GET_MODE_SIZE (mode) != GET_MODE_UNIT_SIZE (GET_MODE (x)))
    abort ();
  if (GET_MODE (x) == mode)
    return x;
  if (GET_CODE (x) == CONST_INT)
    return gen_rtx (CONST_INT, VOIDmode, INTVAL (x) & GET_MODE_MASK (mode));
  if (GET_CODE (x) == CONST_DOUBLE)
/* In version 1.37, try this: */
/*  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT) abort (); */
    /* Assume it's an int, so ..._LOW means the low-order word.  */
    return gen_rtx (CONST_INT, VOIDmode,
		    CONST_DOUBLE_LOW (x) & GET_MODE_MASK (mode));
  if (GET_CODE (x) == MEM)
    {
      register int offset = 0;
#ifdef WORDS_BIG_ENDIAN
      offset = (max (GET_MODE_SIZE (GET_MODE (x)), UNITS_PER_WORD)
		- max (GET_MODE_SIZE (mode), UNITS_PER_WORD));
#endif
#ifdef BYTES_BIG_ENDIAN
      /* Adjust the address so that the address-after-the-data
	 is unchanged.  */
      offset -= (min (UNITS_PER_WORD, GET_MODE_SIZE (mode))
		 - min (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (x))));
#endif
      return change_address (x, mode, plus_constant (XEXP (x, 0), offset));
    }
  else if (GET_CODE (x) == SUBREG)
    return (GET_MODE (SUBREG_REG (x)) == mode && SUBREG_WORD (x) == 0
	    ? SUBREG_REG (x)
	    : gen_rtx (SUBREG, mode, SUBREG_REG (x), SUBREG_WORD (x)));
  else if (GET_CODE (x) == REG)
    {
#ifdef WORDS_BIG_ENDIAN
      if (GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
	{
	  return gen_rtx (SUBREG, mode, x,
			  ((GET_MODE_SIZE (GET_MODE (x))
			    - max (GET_MODE_SIZE (mode), UNITS_PER_WORD))
			   / UNITS_PER_WORD));
	}
#endif
      return gen_rtx (SUBREG, mode, x, 0);
    }
  else
    abort ();
}

/* Like `gen_lowpart', but refer to the most significant part.  */

rtx
gen_highpart (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  if (GET_CODE (x) == MEM)
    {
      register int offset = 0;
#ifndef WORDS_BIG_ENDIAN
      offset = (max (GET_MODE_SIZE (GET_MODE (x)), UNITS_PER_WORD)
		- max (GET_MODE_SIZE (mode), UNITS_PER_WORD));
#endif
#ifndef BYTES_BIG_ENDIAN
      if (GET_MODE_SIZE (mode) < UNITS_PER_WORD)
	offset -= (GET_MODE_SIZE (mode)
		   - min (UNITS_PER_WORD,
			  GET_MODE_SIZE (GET_MODE (x))));
#endif
      return change_address (x, mode, plus_constant (XEXP (x, 0), offset));
    }
  else if (GET_CODE (x) == REG)
    {
#ifndef WORDS_BIG_ENDIAN
      if (GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
	{
	  return gen_rtx (SUBREG, mode, x,
			  ((GET_MODE_SIZE (GET_MODE (x))
			    - max (GET_MODE_SIZE (mode), UNITS_PER_WORD))
			   / UNITS_PER_WORD));
	}
#endif
      return gen_rtx (SUBREG, mode, x, 0);
    }
  else
    abort ();
}

/* Return 1 iff X, assumed to be a SUBREG,
   refers to the least significant part of its containing reg.
   If X is not a SUBREG, always return 1 (it is its own low part!).  */

int
subreg_lowpart_p (x)
     rtx x;
{
  if (GET_CODE (x) != SUBREG)
    return 1;
#ifdef WORDS_BIG_ENDIAN
  if (GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
    {
      register enum machine_mode mode = GET_MODE (SUBREG_REG (x));
      return (SUBREG_WORD (x)
	      == ((GET_MODE_SIZE (GET_MODE (x))
		   - max (GET_MODE_SIZE (mode), UNITS_PER_WORD))
		  / UNITS_PER_WORD));
    }
#endif 
  return SUBREG_WORD (x) == 0;
}

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR.
   (VOIDmode means don't change the mode.
   NULL for ADDR means don't change the address.)  */

rtx
change_address (memref, mode, addr)
     rtx memref;
     enum machine_mode mode;
     rtx addr;
{
  rtx new;

  if (GET_CODE (memref) != MEM)
    abort ();
  if (mode == VOIDmode)
    mode = GET_MODE (memref);
  if (addr == 0)
    addr = XEXP (memref, 0);

  new = gen_rtx (MEM, mode, memory_address (mode, addr));
  MEM_VOLATILE_P (new) = MEM_VOLATILE_P (memref);
  RTX_UNCHANGING_P (new) = RTX_UNCHANGING_P (memref);
  MEM_IN_STRUCT_P (new) = MEM_IN_STRUCT_P (memref);
  return new;
}

/* Return a newly created CODE_LABEL rtx with a unique label number.  */

rtx
gen_label_rtx ()
{
  register rtx label = gen_rtx (CODE_LABEL, VOIDmode, 0, 0, 0, label_num++);
  LABEL_NUSES (label) = 0;
  return label;
}

/* For procedure integration.  */

/* Return a newly created INLINE_HEADER rtx.  Should allocate this
   from a permanent obstack when the opportunity arises.  */

rtx
gen_inline_header_rtx (insn, last_insn,
		       first_labelno, last_labelno,
		       max_parm_regnum, max_regnum, args_size,
		       stack_slots)
     rtx insn, last_insn;
     int first_labelno, last_labelno, max_parm_regnum, max_regnum, args_size;
     rtx stack_slots;
{
  rtx header = gen_rtx (INLINE_HEADER, VOIDmode,
			cur_insn_uid++, NULL,
			insn, last_insn,
			first_labelno, last_labelno,
			max_parm_regnum, max_regnum, args_size, stack_slots);
  return header;
}

/* Install new pointers to the first and last insns in the chain.
   Used for an inline-procedure after copying the insn chain.  */

void
set_new_first_and_last_insn (first, last)
     rtx first, last;
{
  first_insn = first;
  last_insn = last;
}

/* Go through all the RTL insn bodies and copy any invalid shared structure.
   It does not work to do this twice, because the mark bits set here
   are not cleared afterwards.  */

static int unshare_copies = 0;	/* Count rtx's that were copied.  */

static rtx copy_rtx_if_shared ();

void
unshare_all_rtl (insn)
     register rtx insn;
{
  extern rtx stack_slot_list;

  for (; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN
	|| GET_CODE (insn) == CALL_INSN)
      {
	PATTERN (insn) = copy_rtx_if_shared (PATTERN (insn));
	REG_NOTES (insn) = copy_rtx_if_shared (REG_NOTES (insn));
	LOG_LINKS (insn) = copy_rtx_if_shared (LOG_LINKS (insn));
      }

  /* Make sure the addresses of stack slots are not shared
     with anything in the insn chain.  That could happen if
     the stack slot is referenced only by its address.  */
  copy_rtx_if_shared (stack_slot_list);
}

/* Mark ORIG as in use, and return a copy of it if it was already in use.
   Recursively does the same for subexpressions.  */

static rtx
copy_rtx_if_shared (orig)
     rtx orig;
{
  register rtx x = orig;
  register int i;
  register enum rtx_code code;
  register char *format_ptr;
  int copied = 0;

  if (x == 0)
    return 0;

  code = GET_CODE (x);

  /* These types may be freely shared.  */

  switch (code)
    {
    case REG:
    case QUEUED:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
      return x;

    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case NOTE:
    case LABEL_REF:
    case BARRIER:
      /* The chain of insns is not being copied.  */
      return x;

    case MEM:
      /* A MEM is allowed to be shared if its address is constant
	 or is a constant plus one of the special registers.  */
      if (CONSTANT_ADDRESS_P (XEXP (x, 0)))
	return x;
      if (GET_CODE (XEXP (x, 0)) == PLUS
	  && (XEXP (XEXP (x, 0), 0) == frame_pointer_rtx
	      || XEXP (XEXP (x, 0), 0) == arg_pointer_rtx)
	  && CONSTANT_ADDRESS_P (XEXP (XEXP (x, 0), 1)))
	{
	  /* This MEM can appear in more than one place,
	     but its address better not be shared with anything else.  */
	  if (! x->used)
	    XEXP (x, 0) = copy_rtx_if_shared (XEXP (x, 0));
	  x->used = 1;
	  return x;
	}
      if (XEXP (x, 0) == frame_pointer_rtx
	  || XEXP (x, 0) == arg_pointer_rtx)
	return x;
    }

  /* This rtx may not be shared.  If it has already been seen,
     replace it with a copy of itself.  */

  if (x->used)
    {
      register rtx copy;

      unshare_copies++;

      copy = rtx_alloc (code);
      bcopy (x, copy, (sizeof (*copy) - sizeof (copy->fld)
		       + sizeof (copy->fld[0]) * GET_RTX_LENGTH (code)));
      x = copy;
      copied = 1;
    }
  x->used = 1;

  /* Now scan the subexpressions recursively.
     We can store any replaced subexpressions directly into X
     since we know X is not shared!  Any vectors in X
     must be copied if X was copied.  */

  format_ptr = GET_RTX_FORMAT (code);

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  XEXP (x, i) = copy_rtx_if_shared (XEXP (x, i));
	  break;

	case 'E':
	  if (XVEC (x, i) != NULL)
	    {
	      register int j;

	      if (copied)
		XVEC (x, i) = gen_rtvec_v (XVECLEN (x, i), &XVECEXP (x, i, 0));
	      for (j = 0; j < XVECLEN (x, i); j++)
		XVECEXP (x, i, j)
		  = copy_rtx_if_shared (XVECEXP (x, i, j));
	    }
	  break;
	}
    }
  return x;
}

/* Copy X if necessary so that it won't be altered by changes in OTHER.
   Return X or the rtx for the pseudo reg the value of X was copied into.
   OTHER must be valid as a SET_DEST.  */

rtx
make_safe_from (x, other)
     rtx x, other;
{
  while (1)
    switch (GET_CODE (other))
      {
      case SUBREG:
	other = SUBREG_REG (other);
	break;
      case STRICT_LOW_PART:
      case SIGN_EXTEND:
      case ZERO_EXTEND:
	other = XEXP (other, 0);
	break;
      default:
	goto done;
      }
 done:
  if ((GET_CODE (other) == MEM
       && ! CONSTANT_P (x)
       && GET_CODE (x) != CONST_DOUBLE
       && GET_CODE (x) != REG
       && GET_CODE (x) != SUBREG)
      || (GET_CODE (other) == REG
	  && (REGNO (other) < FIRST_PSEUDO_REGISTER
	      || reg_mentioned_p (other, x))))
    {
      rtx temp = gen_reg_rtx (GET_MODE (x));
      emit_move_insn (temp, x);
      return temp;
    }
  return x;
}

/* Emission of insns (adding them to the doubly-linked list).  */

/* Return the first insn of the current sequence or current function.  */

rtx
get_insns ()
{
  return first_insn;
}

/* Return the last insn emitted in current sequence or current function.  */

rtx
get_last_insn ()
{
  return last_insn;
}

/* Specify a new insn as the last in the chain.  */

void
set_last_insn (insn)
     rtx insn;
{
  if (NEXT_INSN (insn) != 0)
    abort ();
  last_insn = insn;
}

/* Return a number larger than any instruction's uid in this function.  */

int
get_max_uid ()
{
  return cur_insn_uid;
}

rtx
next_insn (insn)
     rtx insn;
{
  if (insn) return NEXT_INSN (insn);
  return 0;
}

rtx
previous_insn (insn)
     rtx insn;
{
  if (insn) return PREV_INSN (insn);
  return 0;
}

/* Make and return an INSN rtx, initializing all its slots.
   Store PATTERN in the pattern slots.
   PAT_FORMALS is an idea that never really went anywhere.  */

static rtx
make_insn_raw (pattern, pat_formals)
     rtx pattern;
     rtvec pat_formals;
{
  register rtx insn;

  insn = rtx_alloc(INSN);
  INSN_UID(insn) = cur_insn_uid++;

  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  LOG_LINKS(insn) = NULL;
  REG_NOTES(insn) = NULL;

  return insn;
}

/* Like `make_insn' but make a JUMP_INSN instead of an insn.  */

static rtx
make_jump_insn_raw (pattern, pat_formals)
     rtx pattern;
     rtvec pat_formals;
{
  register rtx insn;

  insn = rtx_alloc(JUMP_INSN);
  INSN_UID(insn) = cur_insn_uid++;

  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  LOG_LINKS(insn) = NULL;
  REG_NOTES(insn) = NULL;
  JUMP_LABEL(insn) = NULL;

  return insn;
}

/* Add INSN to the end of the doubly-linked list.
   INSN may be an INSN, JUMP_INSN, CALL_INSN, CODE_LABEL, BARRIER or NOTE.  */

static void
add_insn (insn)
     register rtx insn;
{
  PREV_INSN (insn) = last_insn;
  NEXT_INSN (insn) = 0;

  if (NULL != last_insn)
    NEXT_INSN (last_insn) = insn;

  if (NULL == first_insn)
    first_insn = insn;

  last_insn = insn;
}

/* Add INSN, an rtx of code INSN, into the doubly-linked list
   after insn AFTER.  */

static void
add_insn_after (insn, after)
     rtx insn, after;
{
  NEXT_INSN (insn) = NEXT_INSN (after);
  PREV_INSN (insn) = after;

  if (NEXT_INSN (insn))
    PREV_INSN (NEXT_INSN (insn)) = insn;
  else if (last_insn == after)
    last_insn = insn;
  else
    {
      rtx stack = sequence_stack;
      /* Scan all pending sequences too.  */
      for (; stack; stack = XEXP (XEXP (stack, 1), 1))
	if (after == XEXP (XEXP (stack, 1), 0))
	  XEXP (XEXP (stack, 1), 0) = insn;
    }

  NEXT_INSN (after) = insn;
}

/* Delete all insns made since FROM.
   FROM becomes the new last instruction.  */

void
delete_insns_since (from)
     rtx from;
{
  if (from == 0)
    first_insn = 0;
  else
    NEXT_INSN (from) = 0;
  last_insn = from;
}

/* Move a consecutive bunch of insns to a different place in the chain.
   The insns to be moved are those between FROM and TO.
   They are moved to a new position after the insn AFTER.  */

void
reorder_insns (from, to, after)
     rtx from, to, after;
{
  /* Splice this bunch out of where it is now.  */
  if (PREV_INSN (from))
    NEXT_INSN (PREV_INSN (from)) = NEXT_INSN (to);
  if (NEXT_INSN (to))
    PREV_INSN (NEXT_INSN (to)) = PREV_INSN (from);
  if (last_insn == to)
    last_insn = PREV_INSN (from);
  if (first_insn == from)
    first_insn = NEXT_INSN (to);

  /* Make the new neighbors point to it and it to them.  */
  if (NEXT_INSN (after))
    {
      PREV_INSN (NEXT_INSN (after)) = to;
      NEXT_INSN (to) = NEXT_INSN (after);
    }
  PREV_INSN (from) = after;
  NEXT_INSN (after) = from;
  if (after == last_insn)
    last_insn = to;
}

/* Emit an insn of given code and pattern
   at a specified place within the doubly-linked list.  */

/* Make an instruction with body PATTERN
   and output it before the instruction BEFORE.  */

rtx
emit_insn_before (pattern, before)
     register rtx pattern, before;
{
  register rtx insn;

  if (GET_CODE (pattern) == SEQUENCE)
    {
      register int i;
      /* For an empty sequence, emit nothing.  */
      if (XVEC (pattern, 0))
	for (i = 0; i < XVECLEN (pattern, 0); i++)
	  add_insn_after (XVECEXP (pattern, 0, i), PREV_INSN (before));
      return PREV_INSN (before);
    }

  insn = make_insn_raw (pattern, 0);

  PREV_INSN (insn) = PREV_INSN (before);
  NEXT_INSN (insn) = before;

  if (PREV_INSN (insn))
    NEXT_INSN (PREV_INSN (insn)) = insn;
  else
    first_insn = insn;
  PREV_INSN (before) = insn;

  return insn;
}

/* Make an instruction with body PATTERN and code JUMP_INSN
   and output it before the instruction BEFORE.  */

rtx
emit_jump_insn_before (pattern, before)
     register rtx pattern, before;
{
  register rtx insn = make_jump_insn_raw (pattern, 0);

  PREV_INSN (insn) = PREV_INSN (before);
  NEXT_INSN (insn) = before;

  if (PREV_INSN (insn))
    NEXT_INSN (PREV_INSN (insn)) = insn;
  else
    first_insn = insn;
  PREV_INSN (before) = insn;

  return insn;
}

/* Make an instruction with body PATTERN and code CALL_INSN
   and output it before the instruction BEFORE.  */

rtx
emit_call_insn_before (pattern, before)
     register rtx pattern, before;
{
  rtx insn = emit_insn_before (pattern, before);
  PUT_CODE (insn, CALL_INSN);
  return insn;
}

/* Make an insn of code INSN with body PATTERN
   and output it after the insn AFTER.  */

rtx
emit_insn_after (pattern, after)
     register rtx pattern, after;
{
  if (GET_CODE (pattern) == SEQUENCE)
    {
      register int i;
      /* For an empty sequence, emit nothing.  */
      if (XVEC (pattern, 0))
	for (i = 0; i < XVECLEN (pattern, 0); i++)
	  {
	    add_insn_after (XVECEXP (pattern, 0, i), after);
	    after = NEXT_INSN (after);
	  }
      return after;
    }
  else
    {
      register rtx insn = make_insn_raw (pattern, 0);
      add_insn_after (insn, after);
      return insn;
    }
}

/* Make an insn of code JUMP_INSN with body PATTERN
   and output it after the insn AFTER.  */

rtx
emit_jump_insn_after (pattern, after)
     register rtx pattern, after;
{
  register rtx insn = make_jump_insn_raw (pattern, 0);

  add_insn_after (insn, after);
  return insn;
}

/* Make an insn of code BARRIER
   and output it after the insn AFTER.  */

rtx
emit_barrier_after (after)
     register rtx after;
{
  register rtx insn = rtx_alloc (BARRIER);

  INSN_UID (insn) = cur_insn_uid++;

  add_insn_after (insn, after);
  return insn;
}

/* Emit the label LABEL after the insn AFTER.  */

void
emit_label_after (label, after)
     rtx label, after;
{
  /* This can be called twice for the same label
     as a result of the confusion that follows a syntax error!
     So make it harmless.  */
  if (INSN_UID (label) == 0)
    {
      INSN_UID (label) = cur_insn_uid++;
      add_insn_after (label, after);
    }
}

/* Emit a note of subtype SUBTYPE after the insn AFTER.  */

void
emit_note_after (subtype, after)
     int subtype;
     rtx after;
{
  register rtx note = rtx_alloc (NOTE);
  INSN_UID (note) = cur_insn_uid++;
  XSTR (note, 3) = 0;
  XINT (note, 4) = subtype;
  add_insn_after (note, after);
}

/* Make an insn of code INSN with pattern PATTERN
   and add it to the end of the doubly-linked list.
   If PATTERN is a SEQUENCE, take the elements of it
   and emit an insn for each element.

   Returns the last insn emitted.  */

rtx
emit_insn (pattern)
     rtx pattern;
{
  rtx insn;

  if (GET_CODE (pattern) == SEQUENCE)
    {
      register int i;
      /* For an empty sequence, emit nothing.  */
      if (XVEC (pattern, 0))
	for (i = 0; i < XVECLEN (pattern, 0); i++)
	  add_insn (insn = XVECEXP (pattern, 0, i));
    }
  else
    {
      insn = make_insn_raw (pattern, NULL);
      add_insn (insn);
    }
  return insn;
}

/* Emit the insns in a chain starting with INSN.  */

rtx
emit_insns (insn)
     rtx insn;
{
  while (insn)
    {
      rtx next = NEXT_INSN (insn);
      add_insn (insn);
      insn = next;
    }
}

/* Make an insn of code JUMP_INSN with pattern PATTERN
   and add it to the end of the doubly-linked list.  */

rtx
emit_jump_insn (pattern)
     rtx pattern;
{
  if (GET_CODE (pattern) == SEQUENCE)
    return emit_insn (pattern);
  else
    {
      register rtx insn = make_jump_insn_raw (pattern, NULL);
      add_insn (insn);
      return insn;
    }
}

/* Make an insn of code CALL_INSN with pattern PATTERN
   and add it to the end of the doubly-linked list.  */

rtx
emit_call_insn (pattern)
     rtx pattern;
{
  if (GET_CODE (pattern) == SEQUENCE)
    return emit_insn (pattern);
  else
    {
      register rtx insn = make_insn_raw (pattern, NULL);
      add_insn (insn);
      PUT_CODE (insn, CALL_INSN);
      return insn;
    }
}

/* Add the label LABEL to the end of the doubly-linked list.  */

rtx
emit_label (label)
     rtx label;
{
  /* This can be called twice for the same label
     as a result of the confusion that follows a syntax error!
     So make it harmless.  */
  if (INSN_UID (label) == 0)
    {
      INSN_UID (label) = cur_insn_uid++;
      add_insn (label);
    }
  return label;
}

/* Make an insn of code BARRIER
   and add it to the end of the doubly-linked list.  */

rtx
emit_barrier ()
{
  register rtx barrier = rtx_alloc (BARRIER);
  INSN_UID (barrier) = cur_insn_uid++;
  add_insn (barrier);
  return barrier;
}

/* Make an insn of code NOTE
   with data-fields specified by FILE and LINE
   and add it to the end of the doubly-linked list,
   but only if line-numbers are desired for debugging info.  */

rtx
emit_line_note (file, line)
     char *file;
     int line;
{
  emit_filename = file;
  emit_lineno = line;

#if 0
  if (no_line_numbers)
    return 0;
#endif

  return emit_note (file, line);
}

/* Make an insn of code NOTE
   with data-fields specified by FILE and LINE
   and add it to the end of the doubly-linked list.
   If it is a line-number NOTE, omit it if it matches the previous one.  */

rtx
emit_note (file, line)
     char *file;
     int line;
{
  register rtx note;

  if (line > 0)
    {
      if (file && last_filename && !strcmp (file, last_filename)
	  && line == last_linenum)
	return 0;
      last_filename = file;
      last_linenum = line;
    }

  if (no_line_numbers && line > 0)
    {
      cur_insn_uid++;
      return 0;
    }

  note = rtx_alloc (NOTE);
  INSN_UID (note) = cur_insn_uid++;
  XSTR (note, 3) = file;
  XINT (note, 4) = line;
  add_insn (note);
  return note;
}

/* Emit a NOTE, and don't omit it even if LINE it the previous note.  */

rtx
emit_line_note_force (file, line)
     char *file;
     int line;
{
  last_linenum = -1;
  return emit_line_note (file, line);
}

/* Cause next statement to emit a line note even if the line number
   has not changed.  This is used at the beginning of a function.  */

void
force_next_line_note ()
{
  last_linenum = -1;
}

/* Return an indication of which type of insn should have X as a body.
   The value is CODE_LABEL, INSN, CALL_INSN or JUMP_INSN.  */

enum rtx_code
classify_insn (x)
     rtx x;
{
  if (GET_CODE (x) == CODE_LABEL)
    return CODE_LABEL;
  if (GET_CODE (x) == CALL)
    return CALL_INSN;
  if (GET_CODE (x) == RETURN)
    return JUMP_INSN;
  if (GET_CODE (x) == SET)
    {
      if (SET_DEST (x) == pc_rtx)
	return JUMP_INSN;
      else if (GET_CODE (SET_SRC (x)) == CALL)
	return CALL_INSN;
      else
	return INSN;
    }
  if (GET_CODE (x) == PARALLEL)
    {
      register int j;
      for (j = XVECLEN (x, 0) - 1; j >= 0; j--)
	if (GET_CODE (XVECEXP (x, 0, j)) == CALL)
	  return CALL_INSN;
	else if (GET_CODE (XVECEXP (x, 0, j)) == SET
		 && SET_DEST (XVECEXP (x, 0, j)) == pc_rtx)
	  return JUMP_INSN;
	else if (GET_CODE (XVECEXP (x, 0, j)) == SET
		 && GET_CODE (SET_SRC (XVECEXP (x, 0, j))) == CALL)
	  return CALL_INSN;
    }
  return INSN;
}

/* Emit the rtl pattern X as an appropriate kind of insn.
   If X is a label, it is simply added into the insn chain.  */

void
emit (x)
     rtx x;
{
  enum rtx_code code = classify_insn (x);

  if (code == CODE_LABEL)
    emit_label (x);
  else if (code == INSN)
    emit_insn (x);
  else if (code == JUMP_INSN)
    {
      register rtx insn = emit_jump_insn (x);
      if (simplejump_p (insn) || GET_CODE (x) == RETURN)
	emit_barrier ();
    }
  else if (code == CALL_INSN)
    emit_call_insn (x);
}

/* Begin emitting insns to a sequence which can be packaged in an RTL_EXPR.
   Return an rtx containing data on any sequence already in progress.  */

rtx
start_sequence ()
{
  sequence_stack
    = gen_rtx (INSN_LIST, VOIDmode,
	       first_insn, gen_rtx (INSN_LIST, VOIDmode,
				    last_insn, sequence_stack));
  first_insn = 0;
  last_insn = 0;
  return sequence_stack;
}

/* Set up the insn chain starting with FIRST
   as the current sequence, saving the previously current one.  */

void
push_to_sequence (first)
     rtx first;
{
  rtx last;
  for (last = first; last && NEXT_INSN (last); last = NEXT_INSN (last));
  sequence_stack
    = gen_rtx (INSN_LIST, VOIDmode,
	       first_insn, gen_rtx (INSN_LIST, VOIDmode,
				    last_insn, sequence_stack));
  first_insn = first;
  last_insn = last;
}

/* After emitting to a sequence, restore previous saved state.
   The argument SAVED is no longer used.

   To get the contents of the sequence just made,
   you must call `gen_sequence' *before* calling here.  */

void
end_sequence (saved)
     rtx saved;
{
  first_insn = XEXP (sequence_stack, 0);
  last_insn = XEXP (XEXP (sequence_stack, 1), 0);
  sequence_stack = XEXP (XEXP (sequence_stack, 1), 1);
}

/* Generate a SEQUENCE rtx containing the insns already emitted
   to the current sequence.

   This is how the gen_... function from a DEFINE_EXPAND
   constructs the SEQUENCE that it returns.  */

rtx
gen_sequence ()
{
  rtx tem;
  rtvec newvec;
  int i;
  int len;

  /* Count the insns in the chain.  */
  len = 0;
  for (tem = first_insn; tem; tem = NEXT_INSN (tem))
    len++;

  /* For an empty sequence... */
  if (len == 0)
    return gen_rtx (SEQUENCE, VOIDmode, NULL);

  /* If only one insn, return its pattern rather than a SEQUENCE.  */
  if (len == 1
      && (GET_CODE (first_insn) == INSN
	  || GET_CODE (first_insn) == JUMP_INSN
	  || GET_CODE (first_insn) == CALL_INSN))
    return PATTERN (first_insn);

  /* Put them in a vector.  */
  newvec = rtvec_alloc (len);
  i = 0;
  for (tem = first_insn; tem; tem = NEXT_INSN (tem), i++)
    newvec->elem[i].rtx = tem;

  /* Make a SEQUENCE from this vector.  */
  return gen_rtx (SEQUENCE, VOIDmode, newvec);
}

/* Set up regno_reg_rtx, reg_rtx_no and regno_pointer_flag
   according to the chain of insns starting with FIRST.

   Also set cur_insn_uid to exceed the largest uid in that chain.

   This is used when an inline function's rtl is saved
   and passed to rest_of_compilation later.  */

static void restore_reg_data_1 ();

void
restore_reg_data (first)
     rtx first;
{
  register rtx insn;
  int i;
  register int max_uid = 0;

  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      if (INSN_UID (insn) >= max_uid)
	max_uid = INSN_UID (insn);

      switch (GET_CODE (insn))
	{
	case NOTE:
	case CODE_LABEL:
	case BARRIER:
	  break;

	case JUMP_INSN:
	case CALL_INSN:
	case INSN:
	  restore_reg_data_1 (PATTERN (insn));
	  break;
	}
    }

  /* Don't duplicate the uids already in use.  */
  cur_insn_uid = max_uid + 1;

  /* If any regs are missing, make them up.  */
  for (i = FIRST_PSEUDO_REGISTER; i < reg_rtx_no; i++)
    if (regno_reg_rtx[i] == 0)
      regno_reg_rtx[i] = gen_rtx (REG, SImode, i);
}

static void
restore_reg_data_1 (orig)
     rtx orig;
{
  register rtx x = orig;
  register int i;
  register enum rtx_code code;
  register char *format_ptr;

  code = GET_CODE (x);

  switch (code)
    {
    case QUEUED:
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case LABEL_REF:
      return;

    case REG:
      if (REGNO (x) >= FIRST_PSEUDO_REGISTER)
	{
	  /* Make sure regno_pointer_flag and regno_reg_rtx are large
	     enough to have an element for this pseudo reg number.  */
	  if (REGNO (x) >= reg_rtx_no)
	    {
	      reg_rtx_no = REGNO (x);

	      if (reg_rtx_no >= regno_pointer_flag_length)
		{
		  int newlen = max (regno_pointer_flag_length * 2,
				    reg_rtx_no + 30);
		  rtx *new1;
		  char *new = (char *) oballoc (newlen);
		  bzero (new, newlen);
		  bcopy (regno_pointer_flag, new, regno_pointer_flag_length);

		  new1 = (rtx *) oballoc (newlen * sizeof (rtx));
		  bzero (new1, newlen * sizeof (rtx));
		  bcopy (regno_reg_rtx, new1, regno_pointer_flag_length * sizeof (rtx));

		  regno_pointer_flag = new;
		  regno_reg_rtx = new1;
		  regno_pointer_flag_length = newlen;
		}
	      reg_rtx_no ++;
	    }
	  regno_reg_rtx[REGNO (x)] = x;
	}
      return;

    case MEM:
      if (GET_CODE (XEXP (x, 0)) == REG)
	mark_reg_pointer (XEXP (x, 0));
      restore_reg_data_1 (XEXP (x, 0));
      return;
    }

  /* Now scan the subexpressions recursively.  */

  format_ptr = GET_RTX_FORMAT (code);

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  restore_reg_data_1 (XEXP (x, i));
	  break;

	case 'E':
	  if (XVEC (x, i) != NULL)
	    {
	      register int j;

	      for (j = 0; j < XVECLEN (x, i); j++)
		restore_reg_data_1 (XVECEXP (x, i, j));
	    }
	  break;
	}
    }
}

/* Initialize data structures and variables in this file
   before generating rtl for each function.
   WRITE_SYMBOLS is nonzero if any kind of debugging info
   is to be generated.  */

void
init_emit (write_symbols)
     int write_symbols;
{
  first_insn = NULL;
  last_insn = NULL;
  sequence_stack = NULL;
  cur_insn_uid = 1;
  reg_rtx_no = FIRST_PSEUDO_REGISTER;
  last_linenum = 0;
  last_filename = 0;
  first_label_num = label_num;

  no_line_numbers = ! write_symbols;
  
  /* Init the tables that describe all the pseudo regs.  */

  regno_pointer_flag_length = FIRST_PSEUDO_REGISTER + 100;

  regno_pointer_flag 
    = (char *) oballoc (regno_pointer_flag_length);
  bzero (regno_pointer_flag, regno_pointer_flag_length);

  regno_reg_rtx 
    = (rtx *) oballoc (regno_pointer_flag_length * sizeof (rtx));
  bzero (regno_reg_rtx, regno_pointer_flag_length * sizeof (rtx));
}

/* Create some permanent unique rtl objects shared between all functions.  */

void
init_emit_once ()
{
  /* Create the unique rtx's for certain rtx codes and operand values.  */

  pc_rtx = gen_rtx (PC, VOIDmode);
  cc0_rtx = gen_rtx (CC0, VOIDmode);

  /* Don't use gen_rtx here since gen_rtx in this case
     tries to use these variables.  */
  const0_rtx = rtx_alloc (CONST_INT);
  INTVAL (const0_rtx) = 0;
  const1_rtx = rtx_alloc (CONST_INT);
  INTVAL (const1_rtx) = 1;

  fconst0_rtx = rtx_alloc (CONST_DOUBLE);
  dconst0_rtx = rtx_alloc (CONST_DOUBLE);
  {
    union real_extract u;
#ifdef REAL_IS_NOT_DOUBLE
    bzero (&u, sizeof u);
    u.d = REAL_VALUE_ATOF ("0");
#else
    u.d = 0;
#endif

    bcopy (&u, &CONST_DOUBLE_LOW (fconst0_rtx), sizeof u);
    CONST_DOUBLE_MEM (fconst0_rtx) = cc0_rtx;
    PUT_MODE (fconst0_rtx, SFmode);

    bcopy (&u, &CONST_DOUBLE_LOW (dconst0_rtx), sizeof u);
    CONST_DOUBLE_MEM (dconst0_rtx) = cc0_rtx;
    PUT_MODE (dconst0_rtx, DFmode);
  }

  stack_pointer_rtx = gen_rtx (REG, Pmode, STACK_POINTER_REGNUM);
  frame_pointer_rtx = gen_rtx (REG, Pmode, FRAME_POINTER_REGNUM);
#ifdef STRUCT_VALUE
  struct_value_rtx = STRUCT_VALUE;
#else
  struct_value_rtx = gen_rtx (REG, Pmode, STRUCT_VALUE_REGNUM);
#endif

#ifdef STRUCT_VALUE_INCOMING
  struct_value_incoming_rtx = STRUCT_VALUE_INCOMING;
#else
#ifdef STRUCT_VALUE_INCOMING_REGNUM
  struct_value_incoming_rtx
    = gen_rtx (REG, Pmode, STRUCT_VALUE_INCOMING_REGNUM);
#else
  struct_value_incoming_rtx = struct_value_rtx;
#endif
#endif

  static_chain_rtx = gen_rtx (REG, Pmode, STATIC_CHAIN_REGNUM);

#ifdef STATIC_CHAIN_INCOMING_REGNUM
  if (STATIC_CHAIN_INCOMING_REGNUM != STATIC_CHAIN_REGNUM)
    static_chain_incoming_rtx = gen_rtx (REG, Pmode, STATIC_CHAIN_INCOMING_REGNUM);
  else
#endif
    static_chain_incoming_rtx = static_chain_rtx;

  if (FRAME_POINTER_REGNUM == ARG_POINTER_REGNUM)
    arg_pointer_rtx = frame_pointer_rtx;
  else
    arg_pointer_rtx = gen_rtx (REG, Pmode, ARG_POINTER_REGNUM);
}
