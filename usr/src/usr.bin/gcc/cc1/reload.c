/* Search an insn for pseudo regs that must be in hard regs and are not.
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.

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


/* This file contains subroutines used only from the file reload1.c.
   It knows how to scan one insn for operands and values
   that need to be copied into registers to make valid code.
   It also finds other operands and values which are valid
   but for which equivalent values in registers exist and
   ought to be used instead.

   Before processing the first insn of the function, call `init_reload'.

   To scan an insn, call `find_reloads'.  This does two things:
   1. sets up tables describing which values must be reloaded
   for this insn, and what kind of hard regs they must be reloaded into;
   2. optionally record the locations where those values appear in
   the data, so they can be replaced properly later.
   This is done only if the second arg to `find_reloads' is nonzero.

   The third arg to `find_reloads' specifies the value of `indirect_ok'.

   Then you must choose the hard regs to reload those pseudo regs into,
   and generate appropriate load insns before this insn and perhaps
   also store insns after this insn.  Set up the array `reload_reg_rtx'
   to contain the REG rtx's for the registers you used.  In some
   cases `find_reloads' will return a nonzero value in `reload_reg_rtx'
   for certain reloads.  Then that tells you which register to use,
   so you do not need to allocate one.  But you still do need to add extra
   instructions to copy the value into and out of that register.

   Finally you must call `subst_reloads' to substitute the reload reg rtx's
   into the locations already recorded.

NOTE SIDE EFFECTS:

   find_reloads can alter the operands of the instruction it is called on.

   1. Two operands of any sort may be interchanged, if they are in a
   commutative instruction.
   This happens only if find_reloads thinks the instruction will compile
   better that way.

   2. Pseudo-registers that are equivalent to constants are replaced
   with those constants if they are not in hard registers.

1 happens every time find_reloads is called.
2 happens only when REPLACE is 1, which is only when
actually doing the reloads, not when just counting them.


Using a reload register for several reloads in one insn:

When an insn has reloads, it is considered as having three parts:
the input reloads, the insn itself after reloading, and the output reloads.
Reloads of values used in memory addresses are often needed for only one part.

When this is so, reload_when_needed records which part needs the reload.
Two reloads for different parts of the insn can share the same reload
register.

When a reload is used for addresses in multiple parts, or when it is
an ordinary operand, it is classified as RELOAD_OTHER, and cannot share
a register with any other reload.  */

#define REG_OK_STRICT

#include "config.h"
#include "rtl.h"
#include "insn-config.h"
#include "recog.h"
#include "reload.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "flags.h"
#include "real.h"

#define min(x,y) ((x) < (y) ? (x) : (y))

/* The variables set up by `find_reloads' are:

   n_reloads		  number of distinct reloads needed; max reload # + 1
       tables indexed by reload number
   reload_in		  rtx for value to reload from
   reload_out		  rtx for where to store reload-reg afterward if nec
			   (often the same as reload_in)
   reload_reg_class	  enum reg_class, saying what regs to reload into
   reload_inmode	  enum machine_mode; mode this operand should have
			   when reloaded, on input.
   reload_outmode	  enum machine_mode; mode this operand should have
			   when reloaded, on output.
   reload_strict_low	  char; 1 if this reload is inside a STRICT_LOW_PART.
   reload_optional	  char, nonzero for an optional reload.
			   Optional reloads are ignored unless the
			   value is already sitting in a register.
   reload_inc		  int, positive amount to increment or decrement by if
			   reload_in is a PRE_DEC, PRE_INC, POST_DEC, POST_INC.
			   Ignored otherwise (don't assume it is zero).
   reload_in_reg	  rtx.  A reg for which reload_in is the equivalent.
			   If reload_in is a symbol_ref which came from
			   reg_equiv_constant, then this is the pseudo
			   which has that symbol_ref as equivalent.
   reload_reg_rtx	  rtx.  This is the register to reload into.
			   If it is zero when `find_reloads' returns,
			   you must find a suitable register in the class
			   specified by reload_reg_class, and store here
			   an rtx for that register with mode from
			   reload_inmode or reload_outmode.
   reload_nocombine	  char, nonzero if this reload shouldn't be
			   combined with another reload.
   reload_needed_for      rtx, operand this reload is needed for address of.
			   0 means it isn't needed for addressing.
   reload_needed_for_multiple
			  int, 1 if this reload needed for more than one thing.
   reload_when_needed     enum, classifies reload as needed either for
			   addressing an input reload, addressing an output,
			   for addressing a non-reloaded mem ref,
			   or for unspecified purposes (i.e., more than one
			   of the above).  */

int n_reloads;

rtx reload_in[MAX_RELOADS];
rtx reload_out[MAX_RELOADS];
enum reg_class reload_reg_class[MAX_RELOADS];
enum machine_mode reload_inmode[MAX_RELOADS];
enum machine_mode reload_outmode[MAX_RELOADS];
char reload_strict_low[MAX_RELOADS];
rtx reload_reg_rtx[MAX_RELOADS];
char reload_optional[MAX_RELOADS];
int reload_inc[MAX_RELOADS];
rtx reload_in_reg[MAX_RELOADS];
char reload_nocombine[MAX_RELOADS];
int reload_needed_for_multiple[MAX_RELOADS];
rtx reload_needed_for[MAX_RELOADS];
enum reload_when_needed reload_when_needed[MAX_RELOADS];

/* All the "earlyclobber" operands of the current insn
   are recorded here.  */
int n_earlyclobbers;
rtx reload_earlyclobbers[MAX_RECOG_OPERANDS];

/* Replacing reloads.

   If `replace_reloads' is nonzero, then as each reload is recorded
   an entry is made for it in the table `replacements'.
   Then later `subst_reloads' can look through that table and
   perform all the replacements needed.  */

/* Nonzero means record the places to replace.  */
static int replace_reloads;

/* Each replacement is recorded with a structure like this.  */
struct replacement
{
  rtx *where;			/* Location to store in */
  int what;			/* which reload this is for */
  enum machine_mode mode;	/* mode it must have */
};

static struct replacement replacements[MAX_RECOG_OPERANDS * ((MAX_REGS_PER_ADDRESS * 2) + 1)];

/* Number of replacements currently recorded.  */
static int n_replacements;

/* MEM-rtx's created for pseudo-regs in stack slots not directly addressable;
   (see reg_equiv_address).  */
static rtx memlocs[MAX_RECOG_OPERANDS * ((MAX_REGS_PER_ADDRESS * 2) + 1)];
static int n_memlocs;

/* The instruction we are doing reloads for;
   so we can test whether a register dies in it.  */
static rtx this_insn;

/* Nonzero means (MEM (REG n)) is valid even if (REG n) is spilled.  */
static int indirect_ok;

/* If hard_regs_live_known is nonzero,
   we can tell which hard regs are currently live,
   at least enough to succeed in choosing dummy reloads.  */
static int hard_regs_live_known;

/* Indexed by hard reg number,
   element is nonegative if hard reg has been spilled.
   This vector is passed to `find_reloads' as an argument
   and is not changed here.  */
static short *static_reload_reg_p;

/* Set to 1 in subst_reg_equivs if it changes anything.  */
static int subst_reg_equivs_changed;

/* On return from push_reload, holds the reload-number for the OUT
   operand, which can be different for that from the input operand.  */
static int output_reloadnum;

static int alternative_allows_memconst ();
static rtx find_dummy_reload ();
static rtx find_reloads_toplev ();
static int find_reloads_address ();
static int find_reloads_address_1 ();
static int hard_reg_set_here_p ();
/* static rtx forget_volatility (); */
static rtx subst_reg_equivs ();
static rtx subst_indexed_address ();
rtx find_equiv_reg ();
static int find_inc_amount ();

/* Record one (sometimes two) reload that needs to be performed.
   IN is an rtx saying where the data are to be found before this instruction.
   OUT says where they must be stored after the instruction.
   (IN is zero for data not read, and OUT is zero for data not written.)
   INLOC and OUTLOC point to the places in the instructions where
   IN and OUT were found.
   CLASS is a register class required for the reloaded data.
   INMODE is the machine mode that the instruction requires
   for the reg that replaces IN and OUTMODE is likewise for OUT.

   If IN is zero, then OUT's location and mode should be passed as
   INLOC and INMODE.

   STRICT_LOW is the 1 if there is a containing STRICT_LOW_PART rtx.

   OPTIONAL nonzero means this reload does not need to be performed:
   it can be discarded if that is more convenient.

   The return value is the reload-number for this reload.

   If both IN and OUT are nonzero, in some rare cases we might
   want to make two separate reloads.  (Actually we never do this now.)
   Therefore, the reload-number for OUT is stored in
   output_reloadnum when we return; the return value applies to IN.
   Usually (presently always), when IN and OUT are nonzero,
   the two reload-numbers are equal, but the caller should be careful to
   distinguish them.  */

static int
push_reload (in, out, inloc, outloc, class,
	     inmode, outmode, strict_low, optional, needed_for)
     register rtx in, out;
     rtx *inloc, *outloc;
     enum reg_class class;
     enum machine_mode inmode, outmode;
     int strict_low;
     int optional;
     rtx needed_for;
{
  register int i;
  int dont_share = 0;

  /* Compare two RTX's.  */
#define MATCHES(x, y) (x == y || (x != 0 && GET_CODE (x) != REG && rtx_equal_p (x, y)))

  /* INMODE and/or OUTMODE could be VOIDmode if no mode
     has been specified for the operand.  In that case,
     use the operand's mode as the mode to reload.  */
  if (inmode == VOIDmode && in != 0)
    inmode = GET_MODE (in);
  if (outmode == VOIDmode && out != 0)
    outmode = GET_MODE (out);

  /* If IN is a pseudo register everywhere-equivalent to a constant, and 
     it is not in a hard register, reload straight from the constant,
     since we want to get rid of such pseudo registers.
     Often this is done earlier, but not always in find_reloads_address.  */
  if (in != 0 && GET_CODE (in) == REG)
    {
      register int regno = REGNO (in);

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	in = reg_equiv_constant[regno];
    }

  /* Likewise for OUT.  Of course, OUT will never be equivalent to
     an actual constant, but it might be equivalent to a memory location
     (in the case of a parameter).  */
  if (out != 0 && GET_CODE (out) == REG)
    {
      register int regno = REGNO (out);

      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	out = reg_equiv_constant[regno];
    }

  /* If we have a read-write operand with an address side-effect,
     change either IN or OUT so the side-effect happens only once.  */
  if (in != 0 && out != 0 && GET_CODE (in) == MEM && rtx_equal_p (in, out))
    {
      if (GET_CODE (XEXP (in, 0)) == POST_INC
	  || GET_CODE (XEXP (in, 0)) == POST_DEC)
	in = gen_rtx (MEM, GET_MODE (in), XEXP (XEXP (in, 0), 0));
      if (GET_CODE (XEXP (in, 0)) == PRE_INC
	  || GET_CODE (XEXP (in, 0)) == PRE_DEC)
	out = gen_rtx (MEM, GET_MODE (out), XEXP (XEXP (out, 0), 0));
    }

  /* If we are reloading a (SUBREG (MEM ...) ...) or (SUBREG constant ...),
     really reload just the inside expression in its own mode.
     Note that the case of (SUBREG (CONST_INT...)...) is handled elsewhere;
     we can't handle it here because CONST_INT does not indicate a mode.  */

  if (in != 0 && GET_CODE (in) == SUBREG && GET_CODE (SUBREG_REG (in)) != REG)
    {
      inloc = &SUBREG_REG (in);
      in = *inloc;
      if (GET_CODE (SUBREG_REG (in)) == MEM)
	/* This is supposed to happen only for paradoxical subregs made by
	   combine.c.  (SUBREG (MEM)) isn't supposed to occur other ways.  */
	if (GET_MODE_SIZE (GET_MODE (in)) > GET_MODE_SIZE (inmode))
	  abort ();
      inmode = GET_MODE (in);
    }

  /* If IN appears in OUT, we can't share any input-only reload for IN.  */
  if (in != 0 && out != 0 && reg_overlap_mentioned_p (in, out))
    dont_share = 1;

  /* Narrow down the class of register wanted if that is
     desirable on this machine for efficiency.  */
  if (in != 0)
    class = PREFERRED_RELOAD_CLASS (in, class);

  if (class == NO_REGS)
    abort ();

  /* We can use an existing reload if the class is right
     and at least one of IN and OUT is a match
     and the other is at worst neutral.
     (A zero compared against anything is neutral.)  */
  for (i = 0; i < n_reloads; i++)
    if (reload_reg_class[i] == class
	&& reload_strict_low[i] == strict_low
	&& ((in != 0 && MATCHES (reload_in[i], in) && ! dont_share
	     && (out == 0 || reload_out[i] == 0 || MATCHES (reload_out[i], out)))
	    ||
	    (out != 0 && MATCHES (reload_out[i], out)
	     && (in == 0 || reload_in[i] == 0 || MATCHES (reload_in[i], in)))))
      break;

  /* Reloading a plain reg for input can match a reload to postincrement
     that reg, since the postincrement's value is the right value.
     Likewise, it can match a preincrement reload, since we regard
     the preincrementation as happening before any ref in this insn
     to that register.  */
  if (i == n_reloads)
    for (i = 0; i < n_reloads; i++)
      if (reload_reg_class[i] == class
	  && reload_strict_low[i] == strict_low
	  && out == 0 && reload_out[i] == 0
	  && ((GET_CODE (in) == REG
	       && (GET_CODE (reload_in[i]) == POST_INC
		   || GET_CODE (reload_in[i]) == POST_DEC
		   || GET_CODE (reload_in[i]) == PRE_INC
		   || GET_CODE (reload_in[i]) == PRE_DEC)
	       && MATCHES (XEXP (reload_in[i], 0), in))
	      ||
	      (GET_CODE (reload_in[i]) == REG
	       && (GET_CODE (in) == POST_INC
		   || GET_CODE (in) == POST_DEC
		   || GET_CODE (in) == PRE_INC
		   || GET_CODE (in) == PRE_DEC)
	       && MATCHES (XEXP (in, 0), reload_in[i]))))
	{
	  /* Make sure reload_in ultimately has the increment,
	     not the plain register.  */
	  if (GET_CODE (in) == REG)
	    in = reload_in[i];
	  break;
	}

  if (i == n_reloads)
    {
      /* We found no existing reload suitable for re-use.
	 So add an additional reload.  */

      reload_in[i] = in;
      reload_out[i] = out;
      reload_reg_class[i] = class;
      reload_inmode[i] = inmode;
      reload_outmode[i] = outmode;
      reload_reg_rtx[i] = 0;
      reload_optional[i] = optional;
      reload_inc[i] = 0;
      reload_strict_low[i] = strict_low;
      reload_nocombine[i] = 0;
      reload_in_reg[i] = *inloc;
      reload_needed_for[i] = needed_for;
      reload_needed_for_multiple[i] = 0;

      n_reloads++;
    }
  else
    {
      /* We are reusing an existing reload,
	 but we may have additional information for it.
	 For example, we may now have both IN and OUT
	 while the old one may have just one of them.  */

      if (inmode != VOIDmode)
	reload_inmode[i] = inmode;
      if (outmode != VOIDmode)
	reload_outmode[i] = outmode;
      if (in != 0)
	reload_in[i] = in;
      if (out != 0)
	reload_out[i] = out;
      reload_optional[i] &= optional;
      if (reload_needed_for[i] != needed_for)
	reload_needed_for_multiple[i] = 1;
    }

  /* If the ostensible rtx being reload differs from the rtx found
     in the location to substitute, this reload is not safe to combine
     because we cannot reliably tell whether it appears in the insn.  */

  if (in != 0 && in != *inloc)
    reload_nocombine[i] = 1;

#if 0
  /* This was replaced by changes in find_reloads_address_1 and the new
     function inc_for_reload, which go with a new meaning of reload_inc.  */

  /* If this is an IN/OUT reload in an insn that sets the CC,
     it must be for an autoincrement.  It doesn't work to store
     the incremented value after the insn because that would clobber the CC.
     So we must do the increment of the value reloaded from,
     increment it, store it back, then decrement again.  */
  if (out != 0 && sets_cc0_p (PATTERN (this_insn)))
    {
      out = 0;
      reload_out[i] = 0;
      reload_inc[i] = find_inc_amount (PATTERN (this_insn), in);
      /* If we did not find a nonzero amount-to-increment-by,
	 that contradicts the belief that IN is being incremented
	 in an address in this insn.  */
      if (reload_inc[i] == 0)
	abort ();
    }
#endif

  /* If we will replace IN and OUT with the reload-reg,
     record where they are located so that substitution need
     not do a tree walk.  */

  if (replace_reloads)
    {
      if (inloc != 0)
	{
	  register struct replacement *r = &replacements[n_replacements++];
	  r->what = i;
	  r->where = inloc;
	  r->mode = inmode;
	}
      if (outloc != 0 && outloc != inloc)
	{
	  register struct replacement *r = &replacements[n_replacements++];
	  r->what = i;
	  r->where = outloc;
	  r->mode = outmode;
	}
    }

  /* If this reload is just being introduced and it has both
     an incoming quantity and an outgoing quantity that are
     supposed to be made to match, see if either one of the two
     can serve as the place to reload into.

     If one of them is acceptable, set reload_reg_rtx[i]
     to that one.  */

  if (in != 0 && out != 0 && in != out && reload_reg_rtx[i] == 0)
    {
      reload_reg_rtx[i] = find_dummy_reload (in, out, inloc, outloc,
					     reload_reg_class[i], i);

      /* If the outgoing register already contains the same value
	 as the incoming one, we can dispense with loading it.
	 The easiest way to tell the caller that is to give a phony
	 value for the incoming operand (same as outgoing one).  */
      if (reload_reg_rtx[i] == out
	  && (GET_CODE (in) == REG || CONSTANT_P (in))
	  && 0 != find_equiv_reg (in, this_insn, 0, REGNO (out),
				  static_reload_reg_p, i, inmode))
	reload_in[i] = out;
    }

  if (out)
    output_reloadnum = i;

  return i;
}

/* Record an additional place we must replace a value
   for which we have already recorded a reload.
   RELOADNUM is the value returned by push_reload
   when the reload was recorded.
   This is used in insn patterns that use match_dup.  */

static void
push_replacement (loc, reloadnum, mode)
     rtx *loc;
     int reloadnum;
     enum machine_mode mode;
{
  if (replace_reloads)
    {
      register struct replacement *r = &replacements[n_replacements++];
      r->what = reloadnum;
      r->where = loc;
      r->mode = mode;
    }
}

/* If there is only one output reload, try to combine it
   with a (logically unrelated) input reload
   to reduce the number of reload registers needed.

   This is safe if the input reload does not appear in
   the value being output-reloaded, because this implies
   it is not needed any more once the original insn completes.  */

static void
combine_reloads ()
{
  int i;
  int output_reload = -1;

  /* Find the output reload; return unless there is exactly one
     and that one is mandatory.  */

  for (i = 0; i < n_reloads; i++)
    if (reload_out[i] != 0)
      {
	if (output_reload >= 0)
	  return;
	output_reload = i;
      }

  if (output_reload < 0 || reload_optional[output_reload])
    return;

  /* An input-output reload isn't combinable.  */

  if (reload_in[output_reload] != 0)
    return;

  /* Check each input reload; can we combine it?  */

  for (i = 0; i < n_reloads; i++)
    if (reload_in[i] && ! reload_optional[i] && ! reload_nocombine[i]
	/* Life span of this reload must not extend past main insn.  */
	&& reload_when_needed[i] != RELOAD_FOR_OUTPUT_RELOAD_ADDRESS
	&& reload_inmode[i] == reload_outmode[output_reload]
	&& reload_inc[i] == 0
	&& reload_reg_rtx[i] == 0
	&& reload_strict_low[i] == 0
	&& reload_reg_class[i] == reload_reg_class[output_reload]
	&& ! reg_overlap_mentioned_p (reload_in[i], reload_out[output_reload]))
      {
	int j;

	/* We have found a reload to combine with!  */
	reload_out[i] = reload_out[output_reload];
	reload_outmode[i] = reload_outmode[output_reload];
	/* Mark the old output reload as inoperative.  */
	reload_out[output_reload] = 0;
	/* The combined reload is needed for the entire insn.  */
	reload_needed_for_multiple[i] = 1;
	reload_when_needed[i] = RELOAD_OTHER;

	/* Transfer all replacements from the old reload to the combined.  */
	for (j = 0; j < n_replacements; j++)
	  if (replacements[j].what == output_reload)
	    replacements[j].what = i;

	return;
      }
}

/* Try to find a reload register for an in-out reload (expressions IN and OUT).
   See if one of IN and OUT is a register that may be used;
   this is desirable since a spill-register won't be needed.
   If so, return the register rtx that proves acceptable.

   INLOC and OUTLOC are locations where IN and OUT appear in the insn.
   CLASS is the register class required for the reload.

   If FOR_REAL is >= 0, it is the number of the reload,
   and in some cases when it can be discovered that OUT doesn't need
   to be computed, clear out reload_out[FOR_REAL].

   If FOR_REAL is -1, this should not be done, because this call
   is just to see if a register can be found, not to find and install it.  */

static rtx
find_dummy_reload (in, out, inloc, outloc, class, for_real)
     rtx in, out;
     rtx *inloc, *outloc;
     enum reg_class class;
     int for_real;
{
  rtx value = 0;
  rtx orig_in = in;

  while (GET_CODE (out) == SUBREG)
    out = SUBREG_REG (out);
  while (GET_CODE (in) == SUBREG)
    in = SUBREG_REG (in);

  /* If operands exceed a word, we can't use either of them
     unless they have the same size.  */
  if (GET_MODE_SIZE (GET_MODE (out)) != GET_MODE_SIZE (GET_MODE (in))
      && (GET_MODE_SIZE (GET_MODE (out)) > UNITS_PER_WORD
	  || GET_MODE_SIZE (GET_MODE (in)) > UNITS_PER_WORD))
    return 0;

  /* See if OUT will do.  */
  if (GET_CODE (out) == REG)
    {
      register int regno = REGNO (out);

      /* When we consider whether the insn uses OUT,
	 ignore references within IN.  They don't prevent us
	 from copying IN into OUT, because those refs would
	 move into the insn that reloads IN.

	 However, we only ignore IN in its role as this operand.
	 If the insn uses IN elsewhere and it contains OUT,
	 that counts.  We can't be sure it's the "same" operand
	 so it might not go through this reload.  */
      *inloc = const0_rtx;

      if (reg_renumber[regno] >= 0)
	regno = reg_renumber[regno];
      if (regno < FIRST_PSEUDO_REGISTER
	  /* A fixed reg that can overlap other regs better not be used
	     for reloading in any way.  */
#ifdef OVERLAPPING_REGNO_P
	  && ! (fixed_regs[regno] && OVERLAPPING_REGNO_P (regno))
#endif
	  && ! refers_to_regno_p (regno, regno + HARD_REGNO_NREGS (regno, GET_MODE (out)),
				  PATTERN (this_insn), outloc)
	  && TEST_HARD_REG_BIT (reg_class_contents[(int) class], regno))
	value = out;

      *inloc = orig_in;
    }

  /* Consider using IN if OUT was not acceptable
     or if OUT dies in this insn (like the quotient in a divmod insn).
     We can't use IN unless it is free after this insn,
     which means we must know accurately which hard regs are live.
     Also, the result can't go in IN if IN is used within OUT.  */
  if (hard_regs_live_known
      && GET_CODE (in) == REG
      && ! find_reg_note (this_insn, REG_UNSET, in)
      && (value == 0
	  || find_regno_note (this_insn, REG_DEAD, REGNO (value))))
    {
      register int regno = REGNO (in);
      if (find_regno_note (this_insn, REG_DEAD, regno))
	{
	  if (reg_renumber[regno] >= 0)
	    regno = reg_renumber[regno];
	  if (regno < FIRST_PSEUDO_REGISTER
	      && ! refers_to_regno_p (regno,
				      regno + HARD_REGNO_NREGS (regno, GET_MODE (in)),
				      out, 0)
	      && ! hard_reg_set_here_p (regno, PATTERN (this_insn))
	      && TEST_HARD_REG_BIT (reg_class_contents[(int) class], regno))
	    {
	      /* If we were going to use OUT as the reload reg
		 and changed our mind, it means OUT is a dummy that
		 dies here.  So don't bother copying value to it.  */
	      if (for_real >= 0 && value == out)
		reload_out[for_real] = 0;
	      value = in;
	    }
	}
    }

  return value;
}

/* This page contains subroutines used mainly for determining
   whether the IN or an OUT of a reload can serve as the
   reload register.  */

/* Return 1 if hard reg number REGNO is stored in by expression X,
   either explicitly or in the guise of a pseudo-reg allocated to REGNO.
   X should be the body of an instruction.  */

static int
hard_reg_set_here_p (regno, x)
     register int regno;
     rtx x;
{
  if (GET_CODE (x) == SET || GET_CODE (x) == CLOBBER)
    {
      register rtx op0 = SET_DEST (x);
      while (GET_CODE (op0) == SUBREG)
	op0 = SUBREG_REG (op0);
      if (GET_CODE (op0) == REG)
	{
	  register int r = REGNO (op0);
	  /* See if this reg includes the specified one.  */
	  if (r <= regno && r + HARD_REGNO_NREGS (r, GET_MODE (op0)) > regno)
	    return 1;
	}
    }
  else if (GET_CODE (x) == PARALLEL)
    {
      register int i = XVECLEN (x, 0) - 1;
      for (; i >= 0; i--)
	if (hard_reg_set_here_p (regno, XVECEXP (x, 0, i)))
	  return 1;
    }

  return 0;
}

/* Return 1 if ADDR is a valid memory address for mode MODE,
   and check that each pseudo reg has the proper kind of
   hard reg.  */

int
strict_memory_address_p (mode, addr)
     enum machine_mode mode;
     register rtx addr;
{
  GO_IF_LEGITIMATE_ADDRESS (mode, addr, win);
  return 0;

 win:
  return 1;
}


/* Like rtx_equal_p except that it allows a REG and a SUBREG to match
   if they are the same hard reg, and has special hacks for
   autoincrement and autodecrement.
   This is specifically intended for find_reloads to use
   in determining whether two operands match.
   X is the operand whose number is the lower of the two.

   The value is 2 if Y contains a pre-increment that matches
   a non-incrementing address in X.  */

/* ??? To be completely correct, we should arrange to pass
   for X the output operand and for Y the input operand.
   For now, we assume that the output operand has the lower number
   because that is natural in (SET output (... input ...)).  */

int
operands_match_p (x, y)
     register rtx x, y;
{
  register int i;
  register RTX_CODE code = GET_CODE (x);
  register char *fmt;
  int success_2;
      
  if (x == y)
    return 1;
  if ((code == REG || (code == SUBREG && GET_CODE (SUBREG_REG (x)) == REG))
      && (GET_CODE (y) == REG || (GET_CODE (y) == SUBREG
				  && GET_CODE (SUBREG_REG (y)) == REG)))
    {
      register int j;

      if (code == SUBREG)
	{
	  i = REGNO (SUBREG_REG (x));
	  if (i >= FIRST_PSEUDO_REGISTER)
	    goto slow;
	  i += SUBREG_WORD (x);
	}
      else
	i = REGNO (x);

      if (GET_CODE (y) == SUBREG)
	{
	  j = REGNO (SUBREG_REG (y));
	  if (j >= FIRST_PSEUDO_REGISTER)
	    goto slow;
	  j += SUBREG_WORD (y);
	}
      else
	j = REGNO (y);

      return i == j;
    }
  /* If two operands must match, because they are really a single
     operand of an assembler insn, then two postincrements are invalid
     because the assembler insn would increment only once.
     On the other hand, an postincrement matches ordinary indexing
     if the postincrement is the output operand.  */
  if (code == POST_DEC || code == POST_INC)
    return operands_match_p (XEXP (x, 0), y);
  /* Two preincrements are invalid
     because the assembler insn would increment only once.
     On the other hand, an preincrement matches ordinary indexing
     if the preincrement is the input operand.
     In this case, return 2, since some callers need to do special
     things when this happens.  */
  if (GET_CODE (y) == PRE_DEC || GET_CODE (y) == PRE_INC)
    return operands_match_p (x, XEXP (y, 0)) ? 2 : 0;

 slow:

  /* Now we have disposed of all the cases 
     in which different rtx codes can match.  */
  if (code != GET_CODE (y))
    return 0;
  if (code == LABEL_REF)
    return XEXP (x, 0) == XEXP (y, 0);
  if (code == SYMBOL_REF)
    return XSTR (x, 0) == XSTR (y, 0);

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  success_2 = 0;
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      int val;
      switch (fmt[i])
	{
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 'e':
	  val = operands_match_p (XEXP (x, i), XEXP (y, i));
	  if (val == 0)
	    return 0;
	  /* If any subexpression returns 2,
	     we should return 2 if we are successful.  */
	  if (val == 2)
	    success_2 = 1;
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
  return 1 + success_2;
}

/* Return the number of times character C occurs in string S.  */

static int
n_occurrences (c, s)
     char c;
     char *s;
{
  int n = 0;
  while (*s)
    n += (*s++ == c);
  return n;
}

struct decomposition
{
  int reg_flag;
  int safe;
  rtx base;
  int start;
  int end;
};

/* Describe the range of registers or memory referenced by X.
   If X is a register, set REG_FLAG and put the first register 
   number into START and the last plus one into END.
   If X is a memory reference, put a base address into BASE 
   and a range of integer offsets into START and END.
   If X is pushing on the stack, we can assume it causes no trouble, 
   so we set the SAFE field.  */

static struct decomposition
decompose (x)
     rtx x;
{
  struct decomposition val;
  int all_const = 0;

  val.reg_flag = 0;
  val.safe = 0;
  if (GET_CODE (x) == MEM)
    {
      rtx base, offset = 0;
      rtx addr = XEXP (x, 0);

      if (GET_CODE (addr) == PRE_DEC || GET_CODE (addr) == PRE_INC
	  || GET_CODE (addr) == POST_DEC || GET_CODE (addr) == POST_INC)
	{
	  val.base = XEXP (addr, 0);
	  val.start = - GET_MODE_SIZE (GET_MODE (x));
	  val.end = GET_MODE_SIZE (GET_MODE (x));
	  val.safe = REGNO (val.base) == STACK_POINTER_REGNUM;
	  return val;
	}

      if (GET_CODE (addr) == CONST)
	{
	  addr = XEXP (addr, 0);
	  all_const = 1;
	}
      if (GET_CODE (addr) == PLUS)
	{
	  if (CONSTANT_P (XEXP (addr, 0)))
	    {
	      base = XEXP (addr, 1);
	      offset = XEXP (addr, 0);
	    }
	  else if (CONSTANT_P (XEXP (addr, 1)))
	    {
	      base = XEXP (addr, 0);
	      offset = XEXP (addr, 1);
	    }
	}

      if (offset == 0)
	{
	  base = addr;
	  offset = const0_rtx;
	} 
      if (GET_CODE (offset) == CONST)
	offset = XEXP (offset, 0);
      if (GET_CODE (offset) == PLUS)
	{
	  if (GET_CODE (XEXP (offset, 0)) == CONST_INT)
	    {
	      base = gen_rtx (PLUS, GET_MODE (base), base, XEXP (offset, 1));
	      offset = XEXP (offset, 0);
	    }
	  else if (GET_CODE (XEXP (offset, 1)) == CONST_INT)
	    {
	      base = gen_rtx (PLUS, GET_MODE (base), base, XEXP (offset, 0));
	      offset = XEXP (offset, 1);
	    }
	  else
	    {
	      base = gen_rtx (PLUS, GET_MODE (base), base, offset);
	      offset = const0_rtx;
	    }
	}
      else if (GET_CODE (offset) != CONST_INT)
	{
	  base = gen_rtx (PLUS, GET_MODE (base), base, offset);
	  offset = const0_rtx;
	}

      if (all_const && GET_CODE (base) == PLUS)
	base = gen_rtx (CONST, GET_MODE (base), base);

      if (GET_CODE (offset) != CONST_INT)
	abort ();

      val.start = INTVAL (offset);
      val.end = val.start + GET_MODE_SIZE (GET_MODE (x));
      val.base = base;
      return val;
    }
  else if (GET_CODE (x) == REG)
    {
      val.reg_flag = 1;
      val.start = true_regnum (x); 
      if (val.start < 0)
	{
	  /* A pseudo with no hard reg.  */
	  val.start = REGNO (x);
	  val.end = val.start + 1;
	}
      else
	/* A hard reg.  */
	val.end = val.start + HARD_REGNO_NREGS (val.start, GET_MODE (x));
    }
  else if (GET_CODE (x) == SUBREG)
    {
      if (GET_CODE (SUBREG_REG (x)) != REG)
	/* This could be more precise, but it's good enough.  */
	return decompose (SUBREG_REG (x));
      val.reg_flag = 1;
      val.start = true_regnum (x); 
      if (val.start < 0)
	return decompose (SUBREG_REG (x));
      else
	/* A hard reg.  */
	val.end = val.start + HARD_REGNO_NREGS (val.start, GET_MODE (x));
    }
  else
    abort ();
  return val;
}

/* Return 1 if altering Y will not modify the value of X.
   Y is also described by YDATA, which should be decompose (Y).  */

static int
immune_p (x, y, ydata)
     rtx x, y;
     struct decomposition ydata;
{
  struct decomposition xdata;

  if (ydata.reg_flag)
    return !refers_to_regno_p (ydata.start, ydata.end, x, 0);
  if (ydata.safe)
    return 1;

  if (GET_CODE (y) != MEM)
    abort ();
  /* If Y is memory and X is not, Y can't affect X.  */
  if (GET_CODE (x) != MEM)
    return 1;

  xdata =  decompose (x);

  if (! rtx_equal_p (xdata.base, ydata.base))
    {
      /* If bases are distinct symbolic constants, there is no overlap.  */
      if (CONSTANT_P (xdata.base) && CONSTANT_P (ydata.base))
	return 1;
      /* Constants and stack slots never overlap.  */
      if (CONSTANT_P (xdata.base)
	  && (ydata.base == frame_pointer_rtx
	      || ydata.base == stack_pointer_rtx))
	return 1;
      if (CONSTANT_P (ydata.base)
	  && (xdata.base == frame_pointer_rtx
	      || xdata.base == stack_pointer_rtx))
	return 1;
      /* If either base is variable, we don't know anything.  */
      return 0;
    }


  return (xdata.start >= ydata.end || ydata.start >= xdata.end);
}

/* Main entry point of this file: search the body of INSN
   for values that need reloading and record them with push_reload.
   REPLACE nonzero means record also where the values occur
   so that subst_reloads can be used.
   IND_OK says that a memory reference is a valid memory address.

   LIVE_KNOWN says we have valid information about which hard
   regs are live at each point in the program; this is true when
   we are called from global_alloc but false when stupid register
   allocation has been done.

   RELOAD_REG_P if nonzero is a vector indexed by hard reg number
   which is nonnegative if the reg has been commandeered for reloading into.
   It is copied into STATIC_RELOAD_REG_P and referenced from there
   by various subroutines.  */

void
find_reloads (insn, replace, ind_ok, live_known, reload_reg_p)
     rtx insn;
     int replace, ind_ok;
     int live_known;
     short *reload_reg_p;
{
#ifdef REGISTER_CONSTRAINTS

  enum reload_modified { RELOAD_NOTHING, RELOAD_READ, RELOAD_READ_WRITE, RELOAD_WRITE };

  register int insn_code_number;
  register int i;
  int noperands;
  /* These are the constraints for the insn.  We don't change them.  */
  char *constraints1[MAX_RECOG_OPERANDS];
  /* These start out as the constraints for the insn
     and they are chewed up as we consider alternatives.  */
  char *constraints[MAX_RECOG_OPERANDS];
  /* Nonzero for a MEM operand whose entire address needs a reload.  */
  int address_reloaded[MAX_RECOG_OPERANDS];
  int n_alternatives;
  int this_alternative[MAX_RECOG_OPERANDS];
  char this_alternative_win[MAX_RECOG_OPERANDS];
  char this_alternative_offmemok[MAX_RECOG_OPERANDS];
  char this_alternative_earlyclobber[MAX_RECOG_OPERANDS];
  int this_alternative_matches[MAX_RECOG_OPERANDS];
  int swapped;
  int goal_alternative[MAX_RECOG_OPERANDS];
  int this_alternative_number;
  int goal_alternative_number;
  int operand_reloadnum[MAX_RECOG_OPERANDS];
  int goal_alternative_matches[MAX_RECOG_OPERANDS];
  int goal_alternative_matched[MAX_RECOG_OPERANDS];
  char goal_alternative_win[MAX_RECOG_OPERANDS];
  char goal_alternative_offmemok[MAX_RECOG_OPERANDS];
  char goal_alternative_earlyclobber[MAX_RECOG_OPERANDS];
  int goal_alternative_swapped;
  enum reload_modified modified[MAX_RECOG_OPERANDS];
  int best;
  int commutative;
  char operands_match[MAX_RECOG_OPERANDS][MAX_RECOG_OPERANDS];
  rtx substed_operand[MAX_RECOG_OPERANDS];
  rtx body = PATTERN (insn);
  int goal_earlyclobber, this_earlyclobber;
  enum machine_mode operand_mode[MAX_RECOG_OPERANDS];

  this_insn = insn;
  n_reloads = 0;
  n_replacements = 0;
  n_memlocs = 0;
  n_earlyclobbers = 0;
  replace_reloads = replace;
  indirect_ok = ind_ok;
  hard_regs_live_known = live_known;
  static_reload_reg_p = reload_reg_p;

  /* Find what kind of insn this is.  NOPERANDS gets number of operands.
     Make OPERANDS point to a vector of operand values.
     Make OPERAND_LOCS point to a vector of pointers to
     where the operands were found.
     Fill CONSTRAINTS and CONSTRAINTS1 with pointers to the
     constraint-strings for this insn.
     Return if the insn needs no reload processing.  */

  switch (GET_CODE (body))
    {
    case USE:
    case CLOBBER:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    case SET:
      /* Dispose quickly of (set (reg..) (reg..)) if both have hard regs.  */
      if (GET_CODE (SET_DEST (body)) == REG
	  && REGNO (SET_DEST (body)) < FIRST_PSEUDO_REGISTER
	  && GET_CODE (SET_SRC (body)) == REG
	  && REGNO (SET_SRC (body)) < FIRST_PSEUDO_REGISTER)
	return;
    case PARALLEL:
    case ASM_OPERANDS:
      noperands = asm_noperands (body);
      if (noperands >= 0)
	{
	  /* This insn is an `asm' with operands.  */

	  insn_code_number = -1;

	  /* expand_asm_operands makes sure there aren't too many operands.  */
	  if (noperands > MAX_RECOG_OPERANDS)
	    abort ();

	  /* Now get the operand values and constraints out of the insn.  */

	  decode_asm_operands (body, recog_operand, recog_operand_loc,
			       constraints, operand_mode);
	  if (noperands > 0)
	    {
	      bcopy (constraints, constraints1, noperands * sizeof (char *));
	      n_alternatives = n_occurrences (',', constraints[0]) + 1;
	      for (i = 1; i < noperands; i++)
		if (n_alternatives != n_occurrences (',', constraints[0]) + 1)
		  {
		    error_for_asm (insn, "operand constraints differ in number of alternatives");
		    /* Avoid further trouble with this insn.  */
		    PATTERN (insn) = gen_rtx (USE, VOIDmode, const0_rtx);
		    n_reloads = 0;
		    return;
		  }
	    }
	  break;
	}

    default:
      /* Ordinary insn: recognize it, allocate space for operands and
	 constraints, and get them out via insn_extract.  */

      insn_code_number = recog_memoized (insn);
      noperands = insn_n_operands[insn_code_number];
      n_alternatives = insn_n_alternatives[insn_code_number];
      /* Just return "no reloads" if insn has no operands with constraints.  */
      if (n_alternatives == 0)
	return;
      insn_extract (insn);
      for (i = 0; i < noperands; i++)
	{
	  constraints[i] = constraints1[i]
	    = insn_operand_constraint[insn_code_number][i];
	  operand_mode[i] = insn_operand_mode[insn_code_number][i];
	}
    }

  if (noperands == 0)
    return;

  commutative = -1;

  /* If we will need to know, later, whether some pair of operands
     are the same, we must compare them now and save the result.
     Reloading the base and index registers will clobber them
     and afterward they will fail to match.  */

  for (i = 0; i < noperands; i++)
    {
      register char *p;
      register int c;

      substed_operand[i] = recog_operand[i];
      p = constraints[i];

      /* Scan this operand's constraint to see if it should match another.  */

      while (c = *p++)
	if (c == '%')
	  commutative = i;
	else if (c >= '0' && c <= '9')
	  {
	    c -= '0';
	    operands_match[c][i]
	      = operands_match_p (recog_operand[c], recog_operand[i]);
	    /* If C can be commuted with C+1, and C might need to match I,
	       then C+1 might also need to match I.  */
	    if (commutative >= 0)
	      {
		if (c == commutative || c == commutative + 1)
		  {
		    int other = c + (c == commutative ? 1 : -1);
		    operands_match[other][i]
		      = operands_match_p (recog_operand[other], recog_operand[i]);
		  }
		if (i == commutative || i == commutative + 1)
		  {
		    int other = i + (i == commutative ? 1 : -1);
		    operands_match[c][other]
		    = operands_match_p (recog_operand[c], recog_operand[other]);
		  }
		/* Note that C is supposed to be less than I.
		   No need to consider altering both C and I
		   because in that case we would alter one into the other.  */
	      }
	  }
    }

  /* Examine each operand that is a memory reference or memory address
     and reload parts of the addresses into index registers.
     While we are at it, initialize the array `modified'.
     Also here any references to pseudo regs that didn't get hard regs
     but are equivalent to constants get replaced in the insn itself
     with those constants.  Nobody will ever see them again.  */

  for (i = 0; i < noperands; i++)
    {
      register RTX_CODE code = GET_CODE (recog_operand[i]);
      modified[i] = RELOAD_READ;
      address_reloaded[i] = 0;
      if (constraints[i][0] == 'p')
	{
	  find_reloads_address (VOIDmode, 0,
				recog_operand[i], recog_operand_loc[i],
				recog_operand[i]);
	  substed_operand[i] = recog_operand[i] = *recog_operand_loc[i];
	}
      else if (code == MEM)
	{
	  if (find_reloads_address (GET_MODE (recog_operand[i]),
				    recog_operand_loc[i],
				    XEXP (recog_operand[i], 0),
				    &XEXP (recog_operand[i], 0),
				    recog_operand[i]))
	    address_reloaded[i] = 1;
	  substed_operand[i] = recog_operand[i] = *recog_operand_loc[i];
	}
      else if (code == SUBREG)
	substed_operand[i] = recog_operand[i] = *recog_operand_loc[i]
	  = find_reloads_toplev (recog_operand[i]);
      else if (code == REG)
	{
	  /* This is equivalent to calling find_reloads_toplev.
	     The code is duplicated for speed.  */
	  register int regno = REGNO (recog_operand[i]);
	  if (reg_equiv_constant[regno] != 0)
	    substed_operand[i] = recog_operand[i]
	      = reg_equiv_constant[regno];
#if 0 /* This might screw code in reload1.c to delete prior output-reload
	 that feeds this insn.  */
	  if (reg_equiv_mem[regno] != 0)
	    substed_operand[i] = recog_operand[i]
	      = reg_equiv_mem[regno];
#endif
	  if (reg_equiv_address[regno] != 0)
	    {
	      *recog_operand_loc[i] = recog_operand[i]
		= gen_rtx (MEM, GET_MODE (recog_operand[i]),
			   reg_equiv_address[regno]);
	      find_reloads_address (GET_MODE (recog_operand[i]),
				    recog_operand_loc[i],
				    XEXP (recog_operand[i], 0),
				    &XEXP (recog_operand[i], 0),
				    recog_operand[i]);
	      substed_operand[i] = recog_operand[i] = *recog_operand_loc[i];
	    }
	}
    }

  /* Now see what we need for pseudo-regs that didn't get hard regs
     or got the wrong kind of hard reg.  For this, we must consider
     all the operands together against the register constraints.  */

  best = MAX_RECOG_OPERANDS + 100;

  swapped = 0;
 try_swapped:

  /* The constraints are made of several alternatives.
     Each operand's constraint looks like foo,bar,... with commas
     separating the alternatives.  The first alternatives for all
     operands go together, the second alternatives go together, etc.

     First loop over alternatives.  */

  for (this_alternative_number = 0;
       this_alternative_number < n_alternatives;
       this_alternative_number++)
    {
      /* Loop over operands for one constraint alternative.  */
      /* LOSERS counts those that don't fit this alternative
	 and would require loading.  */
      int losers = 0;
      /* BAD is set to 1 if it some operand can't fit this alternative
	 even after reloading.  */
      int bad = 0;
      /* REJECT is a count of how undesirable this alternative says it is
	 if any reloading is required.  If the alternative matches exactly
	 then REJECT is ignored, but otherwise it gets this much
	 counted against it in addition to the reloading needed.  */
      int reject = 0;

      this_earlyclobber = 0;

      for (i = 0; i < noperands; i++)
	{
	  register char *p = constraints[i];
	  register int win = 0;
	  /* 0 => this operand can be reloaded somehow for this alternative */
	  int badop = 1;
	  /* 0 => this operand can be reloaded if the alternative allows regs.  */
	  int winreg = 0;
	  int c;
	  register rtx operand = recog_operand[i];
	  int offset = 0;
	  /* Nonzero means this is a MEM that must be reloaded into a reg
	     regardless of what the constraint says.  */
	  int force_reload = 0;
	  int offmemok = 0;
	  int earlyclobber = 0;

	  /* If the operand is a SUBREG, extract
	     the REG or MEM (or maybe even a constant) within.
	     (Constants can occur as a result of reg_equiv_constant.)  */

	  while (GET_CODE (operand) == SUBREG)
	    {
	      offset += SUBREG_WORD (operand);
	      operand = SUBREG_REG (operand);
	      if (GET_CODE (operand) != REG)
		force_reload = 1;
	    }

	  this_alternative[i] = (int) NO_REGS;
	  this_alternative_win[i] = 0;
	  this_alternative_offmemok[i] = 0;
	  this_alternative_earlyclobber[i] = 0;
	  this_alternative_matches[i] = -1;

	  /* An empty constraint or empty alternative
	     allows anything which matched the pattern.  */
	  if (*p == 0 || *p == ',')
	    win = 1, badop = 0;

	  /* Scan this alternative's specs for this operand;
	     set WIN if the operand fits any letter in this alternative.
	     Otherwise, clear BADOP if this operand could
	     fit some letter after reloads,
	     or set WINREG if this operand could fit after reloads
	     provided the constraint allows some registers.  */

	  while (*p && (c = *p++) != ',')
	    switch (c)
	      {
	      case '=':
		modified[i] = RELOAD_WRITE;
		break;

	      case '+':
		modified[i] = RELOAD_READ_WRITE;
		break;

	      case '*':
		break;

	      case '%':
		commutative = i;
		break;

	      case '?':
		reject++;
		break;

	      case '!':
		reject = 100;
		break;

	      case '#':
		/* Ignore rest of this alternative as far as
		   reloading is concerned.  */
		while (*p && *p != ',') p++;
		break;

	      case '0':
	      case '1':
	      case '2':
	      case '3':
	      case '4':
		c -= '0';
		this_alternative_matches[i] = c;
		/* We are supposed to match a previous operand.
		   If we do, we win if that one did.
		   If we do not, count both of the operands as losers.
		   (This is too conservative, since most of the time
		   only a single reload insn will be needed to make
		   the two operands win.  As a result, this alternative
		   may be rejected when it is actually desirable.)  */
		if ((swapped && (c != commutative || i != commutative + 1))
		    /* If we are matching as if two operands were swapped,
		       also pretend that operands_match had been computed
		       with swapped.
		       But if I is the second of those and C is the first,
		       don't exchange them, because operands_match is valid
		       only on one side of its diagonal.  */
		    ? (operands_match
		        [(c == commutative || c == commutative + 1)
			 ? 2*commutative + 1 - c : c]
		        [(i == commutative || i == commutative + 1)
			 ? 2*commutative + 1 - i : i])
		    : operands_match[c][i])
		  win = this_alternative_win[c];
		else
		  {
		    /* Operands don't match.  */
		    rtx value;
		    /* Retroactively mark the operand we had to match
		       as a loser, if it wasn't already.  */
		    if (this_alternative_win[c])
		      losers++;
		    this_alternative_win[c] = 0;
		    if (this_alternative[c] == (int) NO_REGS)
		      bad = 1;
		    /* But count the pair only once in the total badness of
		       this alternative, if the pair can be a dummy reload.  */
		    value
		      = find_dummy_reload (recog_operand[i], recog_operand[c],
					   recog_operand_loc[i], recog_operand_loc[c],
					   this_alternative[c], -1);

		    if (value != 0)
		      losers--;
		  }
		/* This can be fixed with reloads if the operand
		   we are supposed to match can be fixed with reloads.  */
		badop = 0;
		this_alternative[i] = this_alternative[c];
		break;

	      case 'p':
		/* All necessary reloads for an address_operand
		   were handled in find_reloads_address.  */
		this_alternative[i] = (int) ALL_REGS;
		win = 1;
		break;

	      case 'm':
		if (force_reload)
		  break;
		if (GET_CODE (operand) == MEM
		    || (GET_CODE (operand) == REG
			&& REGNO (operand) >= FIRST_PSEUDO_REGISTER
			&& reg_renumber[REGNO (operand)] < 0))
		  win = 1;
		if (GET_CODE (operand) == CONST_DOUBLE
		    || CONSTANT_P (operand))
		  badop = 0;
		break;

	      case '<':
		if (GET_CODE (operand) == MEM
		    && ! address_reloaded[i]
		    && (GET_CODE (XEXP (operand, 0)) == PRE_DEC
			|| GET_CODE (XEXP (operand, 0)) == POST_DEC))
		  win = 1;
		break;

	      case '>':
		if (GET_CODE (operand) == MEM
		    && ! address_reloaded[i]
		    && (GET_CODE (XEXP (operand, 0)) == PRE_INC
			|| GET_CODE (XEXP (operand, 0)) == POST_INC))
		  win = 1;
		break;

		/* Memory operand whose address is offsettable.  */
	      case 'o':
		if (force_reload)
		  break;
		if ((GET_CODE (operand) == MEM
		     && offsettable_memref_p (operand))
		    /* Certain mem addresses will become offsettable
		       after they themselves are reloaded.  This is important;
		       we don't want our own handling of unoffsettables
		       to override the handling of reg_equiv_address.  */
		    || (GET_CODE (operand) == MEM
			&& GET_CODE (XEXP (operand, 0)) == REG
			&& (! ind_ok
			    || reg_equiv_address[REGNO (XEXP (operand, 0))] != 0))
		    || (GET_CODE (operand) == REG
			&& REGNO (operand) >= FIRST_PSEUDO_REGISTER
			&& reg_renumber[REGNO (operand)] < 0))
		  win = 1;
		if (GET_CODE (operand) == CONST_DOUBLE
		    || CONSTANT_P (operand)
		    || GET_CODE (operand) == MEM)
		  badop = 0;
		offmemok = 1;
		break;

	      case '&':
		/* Output operand that is stored before the need for the
		   input operands (and their index registers) is over.  */
		if (GET_CODE (operand) == REG
		    || GET_CODE (operand) == MEM)
		  earlyclobber = 1, this_earlyclobber = 1;
		break;

	      case 'F':
		if (GET_CODE (operand) == CONST_DOUBLE)
		  win = 1;
		break;

	      case 'G':
	      case 'H':
		if (GET_CODE (operand) == CONST_DOUBLE
		    && CONST_DOUBLE_OK_FOR_LETTER_P (operand, c))
		  win = 1;
		break;

	      case 's':
		if (GET_CODE (operand) == CONST_INT)
		  break;
	      case 'i':
		if (CONSTANT_P (operand))
		  win = 1;
		break;

	      case 'n':
		if (GET_CODE (operand) == CONST_INT)
		  win = 1;
		break;

	      case 'I':
	      case 'J':
	      case 'K':
	      case 'L':
	      case 'M':
		if (GET_CODE (operand) == CONST_INT
		    && CONST_OK_FOR_LETTER_P (INTVAL (operand), c))
		  win = 1;
		break;

	      case 'g':
		if (! force_reload
		    && (GENERAL_REGS == ALL_REGS
			|| GET_CODE (operand) != REG
			|| (REGNO (operand) >= FIRST_PSEUDO_REGISTER
			    && reg_renumber[REGNO (operand)] < 0)))
		  win = 1;
		/* Drop through into 'r' case */

	      case 'r':
		this_alternative[i]
		  = (int) reg_class_subunion[this_alternative[i]][(int) GENERAL_REGS];
		goto reg;

	      default:
		this_alternative[i]
		  = (int) reg_class_subunion[this_alternative[i]][(int) REG_CLASS_FROM_LETTER (c)];
		
	      reg:
		if (GET_MODE (operand) == BLKmode)
		  break;
		winreg = 1;
		if (GET_CODE (operand) == REG
		    && reg_fits_class_p (operand, this_alternative[i],
					 offset, GET_MODE (recog_operand[i])))
		  win = 1;
		break;
	      }

	  constraints[i] = p;

	  /* If this operand could be handled with a reg,
	     and some reg is allowed, then this operand can be handled.  */
	  if (winreg && this_alternative[i] != (int) NO_REGS)
	    badop = 0;

	  /* Record which operands fit this alternative.  */
	  this_alternative_earlyclobber[i] = earlyclobber;
	  if (win && ! force_reload)
	    this_alternative_win[i] = 1;
	  else
	    {
	      this_alternative_offmemok[i] = offmemok;
	      losers++;
	      if (badop)
		bad = 1;
	      /* Alternative loses if it has no regs for a reg operand.  */
	      if (GET_CODE (operand) == REG
		  && this_alternative[i] == (int) NO_REGS
		  && this_alternative_matches[i] < 0)
		bad = 1;
	    }
	}

      /* Now see if any output operands that are marked "earlyclobber"
	 in this alternative conflict with any input operands
	 or any memory addresses.  */

      for (i = 0; i < noperands; i++)
	if (this_alternative_earlyclobber[i]
	    && this_alternative_win[i])
	  {
	    struct decomposition early_data; 
	    int j;

	    early_data = decompose (recog_operand[i]);

	    for (j = 0; j < noperands; j++)
	      /* Is this an input operand or a memory ref?  */
	      if ((GET_CODE (recog_operand[j]) == MEM
		   || modified[j] != RELOAD_WRITE)
		  && j != i
		  /* Don't count an input operand that is constrained to match
		     the early clobber operand.  */
		  && ! (this_alternative_matches[j] == i
			&& rtx_equal_p (recog_operand[i], recog_operand[j]))
		  /* Is it altered by storing the earlyclobber operand?  */
		  && !immune_p (recog_operand[j], recog_operand[i], early_data))
		{
		  /* If the output is in a single-reg class,
		     it's costly to reload it, so reload the input instead.  */
		  if (reg_class_size[this_alternative[i]] == 1
		      && (GET_CODE (recog_operand[j]) == REG
			  || GET_CODE (recog_operand[j]) == SUBREG))
		    {
		      losers++;
		      this_alternative_win[j] = 0;
		    }
		  else
		    break;
		}
	    /* If an earlyclobber operand conflicts with something,
	       it must be reloaded, so request this and count the cost.  */
	    if (j != noperands)
	      {
		losers++;
		this_alternative_win[i] = 0;
		for (j = 0; j < noperands; j++)
		  if (this_alternative_matches[j] == i
		      && this_alternative_win[j])
		    {
		      this_alternative_win[j] = 0;
		      losers++;
		    }
	      }
	  }

      /* If one alternative accepts all the operands, no reload required,
	 choose that alternative; don't consider the remaining ones.  */
      if (losers == 0)
	{
	  /* Unswap these so that they are never swapped at `finish'.  */
	  if (commutative >= 0)
	    {
	      recog_operand[commutative] = substed_operand[commutative];
	      recog_operand[commutative + 1]
		= substed_operand[commutative + 1];
	    }
	  for (i = 0; i < noperands; i++)
	    {
	      goal_alternative_win[i] = 1;
	      goal_alternative[i] = this_alternative[i];
	      goal_alternative_offmemok[i] = this_alternative_offmemok[i];
	      goal_alternative_matches[i] = this_alternative_matches[i];
	      goal_alternative_earlyclobber[i]
		= this_alternative_earlyclobber[i];
	    }
	  goal_alternative_number = this_alternative_number;
	  goal_alternative_swapped = swapped;
	  goal_earlyclobber = this_earlyclobber;
	  goto finish;
	}

      /* REJECT, set by the ! and ? constraint characters,
	 discourages the use of this alternative for a reload goal.  */
      if (reject > 0)
	losers += reject;

      /* If this alternative can be made to work by reloading,
	 and it needs less reloading than the others checked so far,
	 record it as the chosen goal for reloading.  */
      if (! bad && best > losers)
	{
	  for (i = 0; i < noperands; i++)
	    {
	      goal_alternative[i] = this_alternative[i];
	      goal_alternative_win[i] = this_alternative_win[i];
	      goal_alternative_offmemok[i] = this_alternative_offmemok[i];
	      goal_alternative_matches[i] = this_alternative_matches[i];
	      goal_alternative_earlyclobber[i]
		= this_alternative_earlyclobber[i];
	    }
	  goal_alternative_swapped = swapped;
	  best = losers;
	  goal_alternative_number = this_alternative_number;
	  goal_earlyclobber = this_earlyclobber;
	}
    }

  /* If insn is commutative (it's safe to exchange a certain pair of operands)
     then we need to try each alternative twice,
     the second time matching those two operands
     as if we had exchanged them.
     To do this, really exchange them in operands.

     If we have just tried the alternatives the second time,
     return operands to normal and drop through.  */

  if (commutative >= 0)
    {
      swapped = !swapped;
      if (swapped)
	{
	  recog_operand[commutative] = substed_operand[commutative + 1];
	  recog_operand[commutative + 1] = substed_operand[commutative];

	  bcopy (constraints1, constraints, noperands * sizeof (char *));
	  goto try_swapped;
	}
      else
	{
	  recog_operand[commutative] = substed_operand[commutative];
	  recog_operand[commutative + 1] = substed_operand[commutative + 1];
	}
    }

  /* The operands don't meet the constraints.
     goal_alternative describes the alternative
     that we could reach by reloading the fewest operands.
     Reload so as to fit it.  */

  if (best == MAX_RECOG_OPERANDS + 100)
    {
      /* No alternative works with reloads??  */
      if (insn_code_number >= 0)
	abort ();
      error_for_asm (insn, "inconsistent operand constraints in an `asm'");
      /* Avoid further trouble with this insn.  */
      PATTERN (insn) = gen_rtx (USE, VOIDmode, const0_rtx);
      n_reloads = 0;
      return;
    }

  /* Jump to `finish' from above if all operands are valid already.
     In that case, goal_alternative_win is all 1.  */
 finish:

  /* Right now, for any pair of operands I and J that are required to match,
     with I < J,
     goal_alternative_matches[J] is I.
     Set up goal_alternative_matched as the inverse function:
     goal_alternative_matched[I] = J.  */

  for (i = 0; i < noperands; i++)
    goal_alternative_matched[i] = -1;

  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i]
	&& goal_alternative_matches[i] >= 0)
      goal_alternative_matched[goal_alternative_matches[i]] = i;

  /* If the best alternative is with operands 1 and 2 swapped,
     consider them swapped before reporting the reloads.  */

  if (goal_alternative_swapped)
    {
      register rtx tem;

      tem = substed_operand[commutative];
      substed_operand[commutative] = substed_operand[commutative + 1];
      substed_operand[commutative + 1] = tem;
      tem = recog_operand[commutative];
      recog_operand[commutative] = recog_operand[commutative + 1];
      recog_operand[commutative + 1] = tem;
    }

  /* Perform whatever substitutions on the operands we are supposed
     to make due to commutativity or replacement of registers
     with equivalent constants or memory slots.  */

  for (i = 0; i < noperands; i++)
    {
      *recog_operand_loc[i] = substed_operand[i];
      /* While we are looping on operands, initialize this.  */
      operand_reloadnum[i] = -1;
    }

  /* Any constants that aren't allowed and can't be reloaded
     into memory locations are here changed into memory references.  */
  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i]
	&& (GET_CODE (recog_operand[i]) == CONST_DOUBLE
	    || CONSTANT_P (recog_operand[i]))
	&& (PREFERRED_RELOAD_CLASS (recog_operand[i],
				    (enum reg_class) goal_alternative[i])
	    == NO_REGS))
      {
	enum machine_mode mode = operand_mode[i];
	*recog_operand_loc[i] = recog_operand[i]
	  = (GET_CODE (recog_operand[i]) == CONST_DOUBLE
	     ? force_const_double_mem (recog_operand[i])
	     : force_const_mem (mode != VOIDmode ? mode : SImode,
				recog_operand[i]));
	find_reloads_toplev (recog_operand[i]);
	if (alternative_allows_memconst (constraints1[i], goal_alternative_number))
	  goal_alternative_win[i] = 1;
      }

  /* Now record reloads for all the operands that need them.  */
  for (i = 0; i < noperands; i++)
    if (! goal_alternative_win[i])
      {
	/* Operands that match previous ones have already been handled.  */
	if (goal_alternative_matches[i] >= 0)
	  ;
	/* Handle an operand with a nonoffsettable address
	   appearing where an offsettable address will do
	   by reloading the address into a base register.  */
	else if (goal_alternative_matched[i] == -1
		 && goal_alternative_offmemok[i]
		 && GET_CODE (recog_operand[i]) == MEM)
	  {
	    operand_reloadnum[i]
	      = push_reload (XEXP (recog_operand[i], 0), 0,
			     &XEXP (recog_operand[i], 0), 0,
			     BASE_REG_CLASS, GET_MODE (XEXP (recog_operand[i], 0)),
			     0, 0, 0, 0);
	    reload_inc[operand_reloadnum[i]]
	      = GET_MODE_SIZE (GET_MODE (recog_operand[i]));
	  }
	else if (goal_alternative_matched[i] == -1)
	  operand_reloadnum[i] =
	    push_reload (modified[i] != RELOAD_WRITE ? recog_operand[i] : 0,
			 modified[i] != RELOAD_READ ? recog_operand[i] : 0,
			 recog_operand_loc[i], 0,
			 (enum reg_class) goal_alternative[i],
			 (modified[i] == RELOAD_WRITE ? VOIDmode : operand_mode[i]),
			 (modified[i] == RELOAD_READ ? VOIDmode : operand_mode[i]),
			 (insn_code_number < 0 ? 0
			  : insn_operand_strict_low[insn_code_number][i]),
			 0, 0);
	/* In a matching pair of operands, one must be input only
	   and the other must be output only.
	   Pass the input operand as IN and the other as OUT.  */
	else if (modified[i] == RELOAD_READ
		 && modified[goal_alternative_matched[i]] == RELOAD_WRITE)
	  {
	    operand_reloadnum[i]
	      = push_reload (recog_operand[i],
			     recog_operand[goal_alternative_matched[i]],
			     recog_operand_loc[i],
			     recog_operand_loc[goal_alternative_matched[i]],
			     (enum reg_class) goal_alternative[i],
			     operand_mode[i],
			     operand_mode[goal_alternative_matched[i]],
			     VOIDmode, 0, 0);
	    operand_reloadnum[goal_alternative_matched[i]] = output_reloadnum;
	  }
	else if (modified[i] == RELOAD_WRITE
		 && modified[goal_alternative_matched[i]] == RELOAD_READ)
	  {
	    operand_reloadnum[goal_alternative_matched[i]]
	      = push_reload (recog_operand[goal_alternative_matched[i]],
			     recog_operand[i],
			     recog_operand_loc[goal_alternative_matched[i]],
			     recog_operand_loc[i],
			     (enum reg_class) goal_alternative[i],
			     operand_mode[goal_alternative_matched[i]],
			     operand_mode[i],
			     VOIDmode, 0, 0);
	    operand_reloadnum[i] = output_reloadnum;
	  }
	else if (insn_code_number >= 0)
	  abort ();
	else
	  {
	    error_for_asm (insn, "inconsistent operand constraints in an `asm'");
	    /* Avoid further trouble with this insn.  */
	    PATTERN (insn) = gen_rtx (USE, VOIDmode, const0_rtx);
	    n_reloads = 0;
	    return;
	  }
      }
    else if (goal_alternative_matched[i] < 0
	     && goal_alternative_matches[i] < 0
	     && optimize)
      {
	rtx operand = recog_operand[i];
	/* For each non-matching operand that's a pseudo-register 
	   that didn't get a hard register, make an optional reload.
	   This may get done even if the insn needs no reloads otherwise.  */
	/* (It would be safe to make an optional reload for a matching pair
	   of operands, but we don't bother yet.)  */
	while (GET_CODE (operand) == SUBREG)
	  operand = XEXP (operand, 0);
	if (GET_CODE (operand) == REG
	    && REGNO (operand) >= FIRST_PSEUDO_REGISTER
	    && reg_renumber[REGNO (operand)] < 0
	    && (enum reg_class) goal_alternative[i] != NO_REGS
	    /* Don't make optional output reloads for jump insns
	       (such as aobjeq on the vax).  */
	    && (modified[i] == RELOAD_READ
		|| GET_CODE (insn) != JUMP_INSN))
	  operand_reloadnum[i]
	    = push_reload (modified[i] != RELOAD_WRITE ? recog_operand[i] : 0,
			   modified[i] != RELOAD_READ ? recog_operand[i] : 0,
			   recog_operand_loc[i], 0,
			   (enum reg_class) goal_alternative[i],
			   (modified[i] == RELOAD_WRITE ? VOIDmode : operand_mode[i]),
			   (modified[i] == RELOAD_READ ? VOIDmode : operand_mode[i]),
			   (insn_code_number < 0 ? 0
			    : insn_operand_strict_low[insn_code_number][i]),
			   1, 0);
	/* Make an optional reload for an explicit mem ref.  */
	else if (GET_CODE (operand) == MEM
		 && (enum reg_class) goal_alternative[i] != NO_REGS
		 /* Don't make optional output reloads for jump insns
		    (such as aobjeq on the vax).  */
		 && (modified[i] == RELOAD_READ
		     || GET_CODE (insn) != JUMP_INSN))
	  operand_reloadnum[i]
	    = push_reload (modified[i] != RELOAD_WRITE ? recog_operand[i] : 0,
			   modified[i] != RELOAD_READ ? recog_operand[i] : 0,
			   recog_operand_loc[i], 0,
			   (enum reg_class) goal_alternative[i],
			   (modified[i] == RELOAD_WRITE ? VOIDmode : operand_mode[i]),
			   (modified[i] == RELOAD_READ ? VOIDmode : operand_mode[i]),
			   (insn_code_number < 0 ? 0
			    : insn_operand_strict_low[insn_code_number][i]),
			   1, 0);
      }

  /* Record the values of the earlyclobber operands for the caller.  */
  if (goal_earlyclobber)
    for (i = 0; i < noperands; i++)
      if (goal_alternative_earlyclobber[i])
	reload_earlyclobbers[n_earlyclobbers++] = recog_operand[i];

  /* If this insn pattern contains any MATCH_DUP's, make sure that
     they will be substituted if the operands they match are substituted.
     Also do now any substitutions we already did on the operands.  */
  if (insn_code_number >= 0)
    for (i = insn_n_dups[insn_code_number] - 1; i >= 0; i--)
      {
	int opno = recog_dup_num[i];
	*recog_dup_loc[i] = *recog_operand_loc[opno];
	if (operand_reloadnum[opno] >= 0)
	  push_replacement (recog_dup_loc[i], operand_reloadnum[opno],
			    insn_operand_mode[insn_code_number][opno]);
      }

#if 0
  /* This loses because reloading of prior insns can invalidate the equivalence
     (or at least find_equiv_reg isn't smart enough to find it any more),
     causing this insn to need more reload regs than it needed before.
     It may be too late to make the reload regs available.
     Now this optimization is done safely in choose_reload_targets.  */

  /* For each reload of a reg into some other class of reg,
     search for an existing equivalent reg (same value now) in the right class.
     We can use it as long as we don't need to change its contents.  */
  for (i = 0; i < n_reloads; i++)
    if (reload_reg_rtx[i] == 0
	&& reload_in[i] != 0
	&& GET_CODE (reload_in[i]) == REG
	&& reload_out[i] == 0)
      {
	reload_reg_rtx[i]
	  = find_equiv_reg (reload_in[i], insn, reload_reg_class[i], -1,
			    static_reload_reg_p, 0, reload_inmode[i]);
	/* Prevent generation of insn to load the value
	   because the one we found already has the value.  */
	if (reload_reg_rtx[i])
	  reload_in[i] = reload_reg_rtx[i];
      }
#endif

#else /* no REGISTER_CONSTRAINTS */
  int noperands;
  int insn_code_number;
  int goal_earlyclobber = 0; /* Always 0, to make combine_reloads happen.  */
  register int i;
  rtx body = PATTERN (insn);

  n_reloads = 0;
  n_replacements = 0;
  n_earlyclobbers = 0;
  replace_reloads = replace;
  indirect_ok = ind_ok;
  this_insn = insn;

  /* Find what kind of insn this is.  NOPERANDS gets number of operands.
     Store the operand values in RECOG_OPERAND and the locations
     of the words in the insn that point to them in RECOG_OPERAND_LOC.
     Return if the insn needs no reload processing.  */

  switch (GET_CODE (body))
    {
    case USE:
    case CLOBBER:
    case ASM_INPUT:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return;

    case PARALLEL:
    case SET:
      noperands = asm_noperands (body);
      if (noperands >= 0)
	{
	  /* This insn is an `asm' with operands.
	     First, find out how many operands, and allocate space.  */

	  insn_code_number = -1;
	  /* ??? This is a bug! ???
	     Give up and delete this insn if it has too many operands.  */
	  if (noperands > MAX_RECOG_OPERANDS)
	    abort ();

	  /* Now get the operand values out of the insn.  */

	  decode_asm_operands (body, recog_operand, recog_operand_loc, 0, 0);
	  break;
	}

    default:
      /* Ordinary insn: recognize it, allocate space for operands and
	 constraints, and get them out via insn_extract.  */

      insn_code_number = recog_memoized (insn);
      noperands = insn_n_operands[insn_code_number];
      insn_extract (insn);
    }

  if (noperands == 0)
    return;

  for (i = 0; i < noperands; i++)
    {
      register RTX_CODE code = GET_CODE (recog_operand[i]);

      if (insn_code_number >= 0)
	if (insn_operand_address_p[insn_code_number][i])
	  find_reloads_address (VOIDmode, 0,
				recog_operand[i], recog_operand_loc[i],
				recog_operand[i]);
      if (code == MEM)
	find_reloads_address (GET_MODE (recog_operand[i]),
			      recog_operand_loc[i],
			      XEXP (recog_operand[i], 0),
			      &XEXP (recog_operand[i], 0),
			      recog_operand[i]);
      if (code == SUBREG)
	recog_operand[i] = *recog_operand_loc[i]
	  = find_reloads_toplev (recog_operand[i]);
      if (code == REG)
	{
	  register int regno = REGNO (recog_operand[i]);
	  if (reg_equiv_constant[regno] != 0)
	    recog_operand[i] = *recog_operand_loc[i]
	      = reg_equiv_constant[regno];
#if 0 /* This might screw code in reload1.c to delete prior output-reload
	 that feeds this insn.  */
	  if (reg_equiv_mem[regno] != 0)
	    recog_operand[i] = *recog_operand_loc[i]
	      = reg_equiv_mem[regno];
#endif
	}
    }
#endif /* no REGISTER_CONSTRAINTS */

  /* Determine which part of the insn each reload is needed for,
     based on which operand the reload is needed for.
     Reloads of entire operands are classified as RELOAD_OTHER.
     So are reloads for which a unique purpose is not known.  */

  for (i = 0; i < n_reloads; i++)
    {
      reload_when_needed[i] = RELOAD_OTHER;

      if (reload_needed_for[i] != 0 && ! reload_needed_for_multiple[i])
	{
	  int j;
	  int output_address = 0;
	  int input_address = 0;
	  int operand_address = 0;

	  /* This reload is needed only for the address of something.
	     Determine whether it is needed for addressing an operand
	     being reloaded for input, whether it is needed for an
	     operand being reloaded for output, and whether it is needed
	     for addressing an operand that won't really be reloaded.  */

	  for (j = 0; j < n_reloads; j++)
	    if (reload_needed_for[i] == reload_in[j]
		|| reload_needed_for[i] == reload_out[j])
	      {
		if (reload_optional[j])
		  operand_address = 1;
		else
		  {
		    if (reload_needed_for[i] == reload_in[j])
		      input_address = 1;
		    if (reload_needed_for[i] == reload_out[j])
		      output_address = 1;
		  }
	      }

	  /* If it is needed for only one of those, record which one.  */

	  if (input_address && ! output_address && ! operand_address)
	    reload_when_needed[i] = RELOAD_FOR_INPUT_RELOAD_ADDRESS;
	  if (output_address && ! input_address && ! operand_address)
	    reload_when_needed[i] = RELOAD_FOR_OUTPUT_RELOAD_ADDRESS;
	  if (operand_address && ! input_address && ! output_address)
	    reload_when_needed[i] = RELOAD_FOR_OPERAND_ADDRESS;
	}
    }

  /* Perhaps an output reload can be combined with another
     to reduce needs by one.  */
  if (!goal_earlyclobber)
    combine_reloads ();
}

/* Return 1 if alternative number ALTNUM in constraint-string CONSTRAINT
   accepts a memory operand with constant address.  */

static int
alternative_allows_memconst (constraint, altnum)
     char *constraint;
     int altnum;
{
  register int c;
  /* Skip alternatives before the one requested.  */
  while (altnum > 0)
    {
      while (*constraint++ != ',');
      altnum--;
    }
  /* Scan the requested alternative for 'm' or 'o'.
     If one of them is present, this alternative accepts memory constants.  */
  while ((c = *constraint++) && c != ',' && c != '#')
    if (c == 'm' || c == 'o')
      return 1;
  return 0;
}

/* Scan X for memory references and scan the addresses for reloading.
   Also checks for references to "constant" regs that we want to eliminate
   and replaces them with the values they stand for.
   We may alter X descructively if it contains a reference to such.
   If X is just a constant reg, we return the equivalent value
   instead of X.  */

static rtx
find_reloads_toplev (x)
     rtx x;
{
  register RTX_CODE code = GET_CODE (x);

  register char *fmt = GET_RTX_FORMAT (code);
  register int i;

  if (code == REG)
    {
      /* This code is duplicated for speed in find_reloads.  */
      register int regno = REGNO (x);
      if (reg_equiv_constant[regno] != 0)
	x = reg_equiv_constant[regno];
#if 0
/*  This creates (subreg (mem...)) which would cause an unnecessary
    reload of the mem.  */
      else if (reg_equiv_mem[regno] != 0)
	x = reg_equiv_mem[regno];
#endif
      else if (reg_equiv_address[regno] != 0)
	{
	  x = gen_rtx (MEM, GET_MODE (x),
		       reg_equiv_address[regno]);
	  find_reloads_address (GET_MODE (x), 0,
				XEXP (x, 0),
				&XEXP (x, 0), x);
	}
      return x;
    }
  if (code == MEM)
    {
      rtx tem = x;
      find_reloads_address (GET_MODE (x), &tem, XEXP (x, 0), &XEXP (x, 0), x);
      return tem;
    }

  if (code == SUBREG && GET_CODE (SUBREG_REG (x)) == REG)
    {
      /* Check for SUBREG containing a REG that's equivalent to a constant.  */
      register int regno = REGNO (SUBREG_REG (x));
      if (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	{
	  /* If the constant has a known value, truncate it right now.  */
	  if (GET_CODE (reg_equiv_constant[regno]) == CONST_INT)
	    {
	      int size = GET_MODE_BITSIZE (GET_MODE (x));
	      if (size < BITS_PER_WORD)
		return gen_rtx (CONST_INT, VOIDmode,
				INTVAL (reg_equiv_constant[regno])
				& ((1 << size) - 1));
	      return reg_equiv_constant[regno];
	    }
	  /* If the constant is symbolic, allow it to be substituted normally.
	     push_reload will strip the subreg later.  */
	}
      /* If the subreg contains a reg that will be converted to a mem,
	 convert the subreg to a narrower memref now.
	 Otherwise, we would get (subreg (mem ...) ...),
	 which would force reload of the mem.  */
      else if (regno >= FIRST_PSEUDO_REGISTER && reg_equiv_address[regno] != 0)
	{
	  int offset = SUBREG_WORD (x) * UNITS_PER_WORD;
	  rtx addr;
#ifdef BYTES_BIG_ENDIAN
	  int size;
	  size = GET_MODE_SIZE (GET_MODE (SUBREG_REG (x)));
	  offset += min (size, UNITS_PER_WORD);
	  size = GET_MODE_SIZE (GET_MODE (x));
	  offset -= min (size, UNITS_PER_WORD);
#endif
	  addr = plus_constant (reg_equiv_address[regno], offset);
	  x = gen_rtx (MEM, GET_MODE (x), addr);
	  find_reloads_address (GET_MODE (x), 0,
				XEXP (x, 0),
				&XEXP (x, 0), x);
	}

    }

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = find_reloads_toplev (XEXP (x, i));
    }
  return x;
}

static rtx
make_memloc (ad, regno)
     rtx ad;
     int regno;
{
  register int i;
  rtx tem = reg_equiv_address[regno];
  for (i = 0; i < n_memlocs; i++)
    if (rtx_equal_p (tem, XEXP (memlocs[i], 0)))
      return memlocs[i];
  tem = gen_rtx (MEM, GET_MODE (ad), tem);
  memlocs[n_memlocs++] = tem;
  return tem;
}

/* Record all reloads needed for handling memory address AD
   which appears in *LOC in a memory reference to mode MODE
   which itself is found in location  *MEMREFLOC.
   Note that we take shortcuts assuming that no multi-reg machine mode
   occurs as part of an address.

   OPERAND is the operand of the insn within which this address appears.

   Value is nonzero if this address is reloaded or replaced as a whole.
   This is interesting to the caller if the address is an autoincrement.  */

static int
find_reloads_address (mode, memrefloc, ad, loc, operand)
     enum machine_mode mode;
     rtx *memrefloc;
     rtx ad;
     rtx *loc;
     rtx operand;
{
  register int regno;
  rtx tem;

  if (GET_CODE (ad) == REG)
    {
      regno = REGNO (ad);

      if (reg_equiv_constant[regno] != 0)
	{
	  if (strict_memory_address_p (mode, reg_equiv_constant[regno]))
	    {
	      *loc = ad = reg_equiv_constant[regno];
	      return 1;
	    }
	}
#if 0 /* This might screw code in reload1.c to delete prior output-reload
	 that feeds this insn.  */
      if (reg_equiv_mem[regno] != 0)
	{
	  if (strict_memory_address_p (mode, reg_equiv_mem[regno]))
	    {
	      *loc = ad = reg_equiv_mem[regno];
	      return 1;
	    }
	}
#endif
      if (reg_equiv_address[regno] != 0)
	{
	  rtx tem = make_memloc (ad, regno);
	  push_reload (XEXP (tem, 0), 0, &XEXP (tem, 0), 0,
		       BASE_REG_CLASS,
		       GET_MODE (XEXP (tem, 0)), 0, VOIDmode, 0,
		       operand);
	  push_reload (tem, 0, loc, 0, BASE_REG_CLASS,
		       GET_MODE (ad), 0, VOIDmode, 0,
		       operand);
	  return 1;
	}
      if (! (regno >= FIRST_PSEUDO_REGISTER && reg_renumber[regno] < 0
	     ? indirect_ok
	     : REGNO_OK_FOR_BASE_P (regno)))
	{
	  push_reload (ad, 0, loc, 0, BASE_REG_CLASS,
		       GET_MODE (ad), 0, VOIDmode, 0, operand);
	  return 1;
	}
      return 0;
    }

  if (strict_memory_address_p (mode, ad))
    {
      /* The address appears valid, so reloads are not needed.
	 But the address may contain an eliminable register.
	 This can happen because a machine with indirect addressing
	 may consider a pseudo register by itself a valid address even when
	 it has failed to get a hard reg.
	 So do a tree-walk to find and eliminate all such regs.  */

      /* But first quickly dispose of a common case.  */
      if (GET_CODE (ad) == PLUS
	  && GET_CODE (XEXP (ad, 1)) == CONST_INT
	  && GET_CODE (XEXP (ad, 0)) == REG
	  && reg_equiv_constant[REGNO (XEXP (ad, 0))] == 0)
	return 0;

      subst_reg_equivs_changed = 0;
      *loc = subst_reg_equivs (ad);

      if (! subst_reg_equivs_changed)
	return 0;

      /* Check result for validity after substitution.  */
      if (strict_memory_address_p (mode, ad))
	return 0;
    }

  /* If we have address of a stack slot but it's not valid
     (displacement is too large), compute the sum in a register.  */
  if (GET_CODE (ad) == PLUS
      && (XEXP (ad, 0) == frame_pointer_rtx
#if FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	  || XEXP (ad, 0) == arg_pointer_rtx
#endif
	  )
      && GET_CODE (XEXP (ad, 1)) == CONST_INT)
    {
      /* Unshare the MEM rtx so we can safely alter it.  */
      if (memrefloc)
	{
	  rtx oldref = *memrefloc;
	  *memrefloc = copy_rtx (*memrefloc);
	  loc = &XEXP (*memrefloc, 0);
	  if (operand == oldref)
	    operand = *memrefloc;
	}
      if (double_reg_address_ok)
	{
	  /* Unshare the sum as well.  */
	  *loc = ad = copy_rtx (ad);
	  /* Reload the displacement into an index reg.
	     We assume the frame pointer or arg pointer is a base reg.  */
	  push_reload (XEXP (ad, 1), 0, &XEXP (ad, 1), 0, INDEX_REG_CLASS, 
		       GET_MODE (ad), VOIDmode, 0, 0, operand);
	}
      else
	{
	  /* If the sum of two regs is not necessarily valid,
	     reload the sum into a base reg.
	     That will at least work.  */
	  push_reload (ad, 0, loc, 0, BASE_REG_CLASS,
		       GET_MODE (ad), VOIDmode, 0, 0, operand);
	}
      return 1;
    }

  /* See if address becomes valid when an eliminable register
     in a sum is replaced.  */

  tem = ad;
  if (GET_CODE (ad) == PLUS)
    tem = subst_indexed_address (ad);
  if (tem != ad && strict_memory_address_p (mode, tem))
    {
      /* Ok, we win that way.  Replace any additional eliminable
	 registers.  */

      subst_reg_equivs_changed = 0;
      tem = subst_reg_equivs (tem);

      /* Make sure that didn't make the address invalid again.  */

      if (! subst_reg_equivs_changed || strict_memory_address_p (mode, tem))
	{
	  *loc = tem;
	  return 0;
	}
    }

  /* If constants aren't valid addresses, reload the constant address
     into a register.  */
  if (CONSTANT_ADDRESS_P (ad) && ! strict_memory_address_p (mode, ad))
    {
      push_reload (ad, 0, loc, 0,
		   BASE_REG_CLASS,
		   Pmode, 0, VOIDmode, 0, operand);
      return 1;
    }

  return find_reloads_address_1 (ad, 0, loc, operand);
}

/* Find all pseudo regs appearing in AD
   that are eliminable in favor of equivalent values
   and do not have hard regs; replace them by their equivalents.  */

static rtx
subst_reg_equivs (ad)
     rtx ad;
{
  register RTX_CODE code = GET_CODE (ad);
  register int i;
  register char *fmt;

  switch (code)
    {
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case PC:
    case CC0:
      return ad;

    case REG:
      {
	register int regno = REGNO (ad);

	if (reg_equiv_constant[regno] != 0)
	  {
	    subst_reg_equivs_changed = 1;
	    return reg_equiv_constant[regno];
	  }
      }
      return ad;

    case PLUS:
      /* Quickly dispose of a common case.  */
      if (XEXP (ad, 0) == frame_pointer_rtx
	  && GET_CODE (XEXP (ad, 1)) == CONST_INT)
	return ad;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      XEXP (ad, i) = subst_reg_equivs (XEXP (ad, i));
  return ad;
}

/* If ADDR is a sum containing a pseudo register that should be
   replaced with a constant (from reg_equiv_constant),
   return the result of doing so, and also apply the associative
   law so that the result is more likely to be a valid address.
   (But it is not guaranteed to be one.)

   In all other cases, return ADDR.  */

static rtx
subst_indexed_address (addr)
     rtx addr;
{
  rtx const_part = 0;
  rtx var_part = 0;
  int regno;

  if (GET_CODE (addr) == PLUS)
    {
      if (CONSTANT_P (XEXP (addr, 0)))
	const_part = XEXP (addr, 0),
	var_part = XEXP (addr, 1);
      else if (CONSTANT_P (XEXP (addr, 1)))
	const_part = XEXP (addr, 1),
	var_part = XEXP (addr, 0);
      else
	var_part = addr;

      if (const_part && GET_CODE (const_part) == CONST)
	const_part = XEXP (const_part, 0);

      if (GET_CODE (var_part) == REG
	  && (regno = REGNO (var_part)) >= FIRST_PSEUDO_REGISTER
	  && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	return (const_part
	        ? gen_rtx (CONST, VOIDmode,
			   gen_rtx (PLUS, Pmode, const_part,
				    reg_equiv_constant[regno]))
		: reg_equiv_constant[regno]);

      if (GET_CODE (var_part) != PLUS)
	return addr;

      if (GET_CODE (XEXP (var_part, 0)) == REG
	  && (regno = REGNO (XEXP (var_part, 0))) >= FIRST_PSEUDO_REGISTER
	  && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	return gen_rtx (PLUS, Pmode, XEXP (var_part, 1),
			(const_part
			 ? gen_rtx (CONST, VOIDmode,
				    gen_rtx (PLUS, Pmode, const_part,
					     reg_equiv_constant[regno]))
			 : reg_equiv_constant[regno]));

      if (GET_CODE (XEXP (var_part, 1)) == REG
	  && (regno = REGNO (XEXP (var_part, 1))) >= FIRST_PSEUDO_REGISTER
	  && reg_renumber[regno] < 0
	  && reg_equiv_constant[regno] != 0)
	return gen_rtx (PLUS, Pmode, XEXP (var_part, 0),
			(const_part
			 ? gen_rtx (CONST, VOIDmode,
				    gen_rtx (PLUS, Pmode, const_part,
					     reg_equiv_constant[regno]))
			 : reg_equiv_constant[regno]));
    }
  return addr;
}

/* Record the pseudo registers we must reload into hard registers
   in a subexpression of a would-be memory address, X.
   (This function is not called if the address we find is strictly valid.)
   CONTEXT = 1 means we are considering regs as index regs,
   = 0 means we are considering them as base regs.

   OPERAND is the operand of the insn within which this address appears.

   We return nonzero if X, as a whole, is reloaded or replaced.  */

/* Note that we take shortcuts assuming that no multi-reg machine mode
   occurs as part of an address.
   Also, this is not fully machine-customizable; it works for machines
   such as vaxes and 68000's and 32000's, but other possible machines
   could have addressing modes that this does not handle right.  */

static int
find_reloads_address_1 (x, context, loc, operand)
     rtx x;
     int context;
     rtx *loc;
     rtx operand;
{
  register RTX_CODE code = GET_CODE (x);

  if (code == PLUS)
    {
      register rtx op0 = XEXP (x, 0);
      register rtx op1 = XEXP (x, 1);
      register RTX_CODE code0 = GET_CODE (op0);
      register RTX_CODE code1 = GET_CODE (op1);
      if (code0 == MULT || code0 == SIGN_EXTEND || code1 == MEM)
	{
	  find_reloads_address_1 (op0, 1, &XEXP (x, 0), operand);
	  find_reloads_address_1 (op1, 0, &XEXP (x, 1), operand);
	}
      else if (code1 == MULT || code1 == SIGN_EXTEND || code0 == MEM)
	{
	  find_reloads_address_1 (op0, 0, &XEXP (x, 0), operand);
	  find_reloads_address_1 (op1, 1, &XEXP (x, 1), operand);
	}
      else if (code0 == CONST_INT || code0 == CONST
	       || code0 == SYMBOL_REF || code0 == LABEL_REF)
	{
	  find_reloads_address_1 (op1, 0, &XEXP (x, 1), operand);
	}
      else if (code1 == CONST_INT || code1 == CONST
	       || code1 == SYMBOL_REF || code1 == LABEL_REF)
	{
	  find_reloads_address_1 (op0, 0, &XEXP (x, 0), operand);
	}
      else if (code0 == REG && code1 == REG)
	{
	  if (REG_OK_FOR_INDEX_P (op0)
	      && REG_OK_FOR_BASE_P (op1))
	    return 0;
	  else if (REG_OK_FOR_INDEX_P (op1)
	      && REG_OK_FOR_BASE_P (op0))
	    return 0;
	  else if (REG_OK_FOR_BASE_P (op1))
	    find_reloads_address_1 (op0, 1, &XEXP (x, 0), operand);
	  else if (REG_OK_FOR_BASE_P (op0))
	    find_reloads_address_1 (op1, 1, &XEXP (x, 1), operand);
	  else if (REG_OK_FOR_INDEX_P (op1))
	    find_reloads_address_1 (op0, 0, &XEXP (x, 0), operand);
	  else if (REG_OK_FOR_INDEX_P (op0))
	    find_reloads_address_1 (op1, 0, &XEXP (x, 1), operand);
	  else
	    {
	      find_reloads_address_1 (op0, 1, &XEXP (x, 0), operand);
	      find_reloads_address_1 (op1, 0, &XEXP (x, 1), operand);
	    }
	}
      else if (code0 == REG)
	{
	  find_reloads_address_1 (op0, 1, &XEXP (x, 0), operand);
	  find_reloads_address_1 (op1, 0, &XEXP (x, 1), operand);
	}
      else if (code1 == REG)
	{
	  find_reloads_address_1 (op1, 1, &XEXP (x, 1), operand);
	  find_reloads_address_1 (op0, 0, &XEXP (x, 0), operand);
	}
    }
  else if (code == POST_INC || code == POST_DEC
	   || code == PRE_INC || code == PRE_DEC)
    {
      rtx incremented = XEXP (x, 0);

      if (GET_CODE (incremented) == REG)
	{
	  register int regno = REGNO (incremented);
	  int value = 0;

	  /* A register that is incremented cannot be constant!  */
	  if (regno >= FIRST_PSEUDO_REGISTER
	      && reg_equiv_constant[regno] != 0)
	    abort ();

	  /* Handle a register that is equivalent to a memory location
	     which cannot be addressed directly.  */
	  if (reg_equiv_address[regno] != 0)
	    {
	      rtx tem = make_memloc (incremented, regno);
	      /* First reload the memory location's address.  */
	      push_reload (XEXP (tem, 0), 0, &XEXP (tem, 0), 0,
			   BASE_REG_CLASS,
			   GET_MODE (XEXP (tem, 0)), 0, VOIDmode, 0,
			   operand);
	      /* Put this inside a new increment-expression.  */
	      x = gen_rtx (GET_CODE (x), GET_MODE (x), tem);
	      /* Proceed to reload that, as if it contained a register.  */
	    }

	  /* If we have a hard register that is ok as an index,
	     don't make a reload.  If an autoincrement of a nice register
	     isn't "valid", it must be that no autoincrement is "valid".
	     If that is true and something made an autoincrement anyway,
	     this must be a special context where one is allowed.
	     (For example, a "push" instruction.)
	     We can't improve this address, so leave it alone.  */

	  /* Otherwise, reload the autoincrement into a suitable hard reg
	     and record how much to increment by.  */

	  if (reg_renumber[regno] >= 0)
	    regno = reg_renumber[regno];
	  if ((regno >= FIRST_PSEUDO_REGISTER
	       || !(context ? REGNO_OK_FOR_INDEX_P (regno)
		    : REGNO_OK_FOR_BASE_P (regno))))
	    {
	      register rtx link;

	      int reloadnum
		= push_reload (x, 0, loc, 0,
			       context ? INDEX_REG_CLASS : BASE_REG_CLASS,
			       GET_MODE (x), GET_MODE (x), VOIDmode, 0, operand);
	      reload_inc[reloadnum]
		= find_inc_amount (PATTERN (this_insn), incremented);

	      value = 1;

	      /* Update the REG_INC notes.  */

	      for (link = REG_NOTES (this_insn);
		   link; link = XEXP (link, 1))
		if (REG_NOTE_KIND (link) == REG_INC
		    && REGNO (XEXP (link, 0)) == REGNO (incremented))
		  push_replacement (&XEXP (link, 0), reloadnum, VOIDmode);
	    }
	  return value;
	}
    }
  else if (code == REG)
    {
      register int regno = REGNO (x);

      if (reg_equiv_constant[regno] != 0)
	{
	  push_reload (reg_equiv_constant[regno], 0, loc, 0,
		       context ? INDEX_REG_CLASS : BASE_REG_CLASS,
		       GET_MODE (x), 0, VOIDmode, 0, operand);
	  return 1;
	}

#if 0 /* This might screw code in reload1.c to delete prior output-reload
	 that feeds this insn.  */
      if (reg_equiv_mem[regno] != 0)
	{
	  push_reload (reg_equiv_mem[regno], 0, loc, 0,
		       context ? INDEX_REG_CLASS : BASE_REG_CLASS,
		       GET_MODE (x), 0, VOIDmode, 0, operand);
	  return 1;
	}
#endif
      if (reg_equiv_address[regno] != 0)
	{
	  x = make_memloc (x, regno);
	  push_reload (XEXP (x, 0), 0, &XEXP (x, 0), 0,
		       BASE_REG_CLASS,
		       GET_MODE (XEXP (x, 0)), 0, VOIDmode, 0, operand);
	}

      if (reg_renumber[regno] >= 0)
	regno = reg_renumber[regno];
      if ((regno >= FIRST_PSEUDO_REGISTER
	   || !(context ? REGNO_OK_FOR_INDEX_P (regno)
		: REGNO_OK_FOR_BASE_P (regno))))
	{
	  push_reload (x, 0, loc, 0,
		       context ? INDEX_REG_CLASS : BASE_REG_CLASS,
		       GET_MODE (x), 0, VOIDmode, 0, operand);
	  return 1;
	}
    }
  else
    {
      register char *fmt = GET_RTX_FORMAT (code);
      register int i;
      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	{
	  if (fmt[i] == 'e')
	    find_reloads_address_1 (XEXP (x, i), context, &XEXP (x, i), operand);
	}
    }

  return 0;
}

/* Substitute into X the registers into which we have reloaded
   the things that need reloading.  The array `replacements'
   says contains the locations of all pointers that must be changed
   and says what to replace them with.

   Return the rtx that X translates into; usually X, but modified.  */

void
subst_reloads ()
{
  register int i;

  for (i = 0; i < n_replacements; i++)
    {
      register struct replacement *r = &replacements[i];
      register rtx reloadreg = reload_reg_rtx[r->what];
      if (reloadreg)
	{
	  /* Encapsulate RELOADREG so its machine mode matches what
	     used to be there.  */
	  if (GET_MODE (reloadreg) != r->mode && r->mode != VOIDmode)
	    reloadreg = gen_rtx (SUBREG, r->mode, reloadreg, 0);
	  *r->where = reloadreg;
	}
      /* If reload got no reg and isn't optional, something's wrong.  */
      else if (! reload_optional[r->what])
	abort ();
    }
}

#if 0

/* [[This function is currently obsolete, now that volatility
   is represented by a special bit `volatil' so VOLATILE is never used;
   and UNCHANGING has never been brought into use.]]

   Alter X by eliminating all VOLATILE and UNCHANGING expressions.
   Each of them is replaced by its operand.
   Thus, (PLUS (VOLATILE (MEM (REG 5))) (CONST_INT 4))
   becomes (PLUS (MEM (REG 5)) (CONST_INT 4)).

   If X is itself a VOLATILE expression,
   we return the expression that should replace it
   but we do not modify X.  */

static rtx
forget_volatility (x)
     register rtx x;
{
  enum rtx_code code = GET_CODE (x);
  register char *fmt;
  register int i;
  register rtx value = 0;

  switch (code)
    {
    case LABEL_REF:
    case SYMBOL_REF:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case REG:
    case CC0:
    case PC:
      return x;

    case VOLATILE:
    case UNCHANGING:
      return XEXP (x, 0);
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = forget_volatility (XEXP (x, i));
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    XVECEXP (x, i, j) = forget_volatility (XVECEXP (x, i, j));
	}
    }

  return x;
}

#endif

/* Check the insns before INSN to see if there is a suitable register
   containing the same value as GOAL.
   If OTHER is -1, look for a register in class CLASS.
   Otherwise, just see if register number OTHER shares GOAL's value.

   Return an rtx for the register found, or zero if none is found.

   If RELOAD_REG_P is (short *)1,
   we reject any hard reg that appears in reload_reg_rtx
   because such a hard reg is also needed coming into this insn.

   If RELOAD_REG_P is any other nonzero value,
   it is a vector indexed by hard reg number
   and we reject any hard reg whose element in the vector is nonnegative
   as well as any that appears in reload_reg_rtx.

   If GOAL is zero, then GOALREG is a register number; we look
   for an equivalent for that register.

   MODE is the machine mode of the value we want an equivalence for.
   If GOAL is nonzero and not VOIDmode, then it must have mode MODE.

   This function is used by jump.c as well as in the reload pass.

   If GOAL is a PLUS, we assume it adds the stack pointer to a constant.  */

rtx
find_equiv_reg (goal, insn, class, other, reload_reg_p, goalreg, mode)
     register rtx goal;
     rtx insn;
     enum reg_class class;
     register int other;
     short *reload_reg_p;
     int goalreg;
     enum machine_mode mode;
{
  register rtx p = insn;
  rtx valtry, value, where;
  register rtx pat;
  register int regno = -1;
  int valueno;
  int goal_mem = 0;
  int goal_const = 0;
  int goal_mem_addr_varies = 0;
  int nregs;
  int valuenregs;

  if (goal == 0)
    regno = goalreg;
  else if (GET_CODE (goal) == REG)
    regno = REGNO (goal);
  else if (GET_CODE (goal) == MEM)
    {
      enum rtx_code code = GET_CODE (XEXP (goal, 0));
      if (MEM_VOLATILE_P (goal))
	return 0;
      if (flag_float_store
	  && (GET_MODE (goal) == DFmode || GET_MODE (goal) == SFmode))
	return 0;
      /* An address with side effects must be reexecuted.  */
      switch (code)
	{
	case POST_INC:
	case PRE_INC:
	case POST_DEC:
	case PRE_DEC:
	  return 0;
	}
      goal_mem = 1;
    }
  else if (CONSTANT_P (goal))
    goal_const = 1;
  else
    return 0;

  /* On some machines, certain regs must always be rejected
     because they don't behave the way ordinary registers do.  */
  
#ifdef OVERLAPPING_REGNO_P
   if (regno >= 0 && regno < FIRST_PSEUDO_REGISTER
       && OVERLAPPING_REGNO_P (regno))
     return 0;
#endif      

  /* Scan insns back from INSN, looking for one that copies
     a value into or out of GOAL.
     Stop and give up if we reach a label.  */

  while (1)
    {
      p = PREV_INSN (p);
      if (p == 0 || GET_CODE (p) == CODE_LABEL)
	return 0;
      if (GET_CODE (p) == INSN
	  /* If we don't want spill regs (true for all calls in this file) */
	  && (! (reload_reg_p != 0 && reload_reg_p != (short *)1)
	  /* then ignore insns introduced by reload; they aren't useful
	     and can cause results in reload_as_needed to be different
	     from what they were when calculating the need for spills.
	     If we notice an input-reload insn here, we will reject it below,
	     but it might hide a usable equivalent.  That makes bad code.
	     It may even abort: perhaps no reg was spilled for this insn
	     because it was assumed we would find that equivalent.  */
	      || INSN_UID (p) < reload_first_uid))
	{
	  pat = PATTERN (p);
	  /* First check for something that sets some reg equal to GOAL.  */
	  if (GET_CODE (pat) == SET
	      && ((regno >= 0
		   && GET_CODE (SET_SRC (pat)) == REG
		   && REGNO (SET_SRC (pat)) == regno
		   && GET_CODE (valtry = SET_DEST (pat)) == REG)
		  ||
		  (regno >= 0
		   && GET_CODE (SET_DEST (pat)) == REG
		   && REGNO (SET_DEST (pat)) == regno
		   && GET_CODE (valtry = SET_SRC (pat)) == REG)
		  ||
		  (goal_const && rtx_equal_p (SET_SRC (pat), goal)
		   && GET_CODE (valtry = SET_DEST (pat)) == REG)
		  || (goal_mem
		      && GET_CODE (valtry = SET_DEST (pat)) == REG
		      && rtx_renumbered_equal_p (goal, SET_SRC (pat)))
		  || (goal_mem
		      && GET_CODE (valtry = SET_SRC (pat)) == REG
		      && rtx_renumbered_equal_p (goal, SET_DEST (pat)))))
	    if (valueno = REGNO (valtry),
		other >= 0
		? valueno == other
		: ((unsigned) valueno < FIRST_PSEUDO_REGISTER
		   && TEST_HARD_REG_BIT (reg_class_contents[(int) class],
					 valueno)))
	      {
		value = valtry;
		where = p;
		break;
	      }
	}
    }

  /* We found a previous insn copying GOAL into a suitable other reg VALUE
     (or copying VALUE into GOAL, if GOAL is also a register).
     Now verify that VALUE is really valid.  */

  /* VALUENO is the register number of VALUE; a hard register.  */

  /* Don't find the sp as an equiv, since pushes that we don't notice
     would invalidate it.  */
  if (valueno == STACK_POINTER_REGNUM)
    return 0;

  /* Reject VALUE if the copy-insn moved the wrong sort of datum.  */
  if (GET_MODE (value) != mode)
    return 0;

  /* Reject VALUE if it was loaded from GOAL
     and is also a register that appears in the address of GOAL.  */

  if (goal_mem && value == SET_DEST (PATTERN (where))
      && refers_to_regno_p (valueno,
			    valueno + HARD_REGNO_NREGS (valueno, mode),
			    goal, 0))
    return 0;

  /* Reject registers that overlap GOAL.  */

  if (!goal_mem && !goal_const
      && regno + HARD_REGNO_NREGS (regno, mode) > valueno
      && regno < valueno + HARD_REGNO_NREGS (valueno, mode))
    return 0;

  /* Reject VALUE if it is one of the regs reserved for reloads.
     Reload1 knows how to reuse them anyway, and it would get
     confused if we allocated one without its knowledge.
     (Now that insns introduced by reload are ignored above,
     this case shouldn't happen, but I'm not positive.)  */

  if (reload_reg_p != 0 && reload_reg_p != (short *)1
      && reload_reg_p[valueno] >= 0)
    return 0;

  /* On some machines, certain regs must always be rejected
     because they don't behave the way ordinary registers do.  */
  
#ifdef OVERLAPPING_REGNO_P
  if (OVERLAPPING_REGNO_P (valueno))
    return 0;
#endif      

  nregs = HARD_REGNO_NREGS (regno, mode);
  valuenregs = HARD_REGNO_NREGS (valueno, mode);

  /* Reject VALUE if it is a register being used for an input reload
     even if it is not one of those reserved.  */

  if (reload_reg_p != 0)
    {
      int i;
      for (i = 0; i < n_reloads; i++)
	if (reload_reg_rtx[i] != 0 && reload_in[i])
	  {
	    int regno1 = REGNO (reload_reg_rtx[i]);
	    int nregs1 = HARD_REGNO_NREGS (regno1,
					   GET_MODE (reload_reg_rtx[i]));
	    if (regno1 < valueno + valuenregs
		&& regno1 + nregs1 > valueno)
	      return 0;
	  }
    }

  if (goal_mem)
    goal_mem_addr_varies = rtx_addr_varies_p (goal);

  /* Now verify that the values of GOAL and VALUE remain unaltered
     until INSN is reached.  */

  p = insn;
  while (1)
    {
      p = PREV_INSN (p);
      if (p == where)
	return value;

      /* Don't trust the conversion past a function call
	 if either of the two is in a call-clobbered register, or memory.  */
      if (GET_CODE (p) == CALL_INSN
	  && ((regno >= 0 && regno < FIRST_PSEUDO_REGISTER
	       && call_used_regs[regno])
	      ||
	      (valueno >= 0 && valueno < FIRST_PSEUDO_REGISTER
	       && call_used_regs[valueno])
	      ||
	      goal_mem))
	return 0;

      if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	  || GET_CODE (p) == CALL_INSN)
	{
	  /* If this insn P stores in either GOAL or VALUE, return 0.
	     If GOAL is a memory ref and this insn writes memory, return 0.
	     If GOAL is a memory ref and its address is not constant,
	     and this insn P changes a register, return 0.
	     That is in lieue of checking whether GOAL uses this register.  */

	  pat = PATTERN (p);
	  if (GET_CODE (pat) == SET || GET_CODE (pat) == CLOBBER)
	    {
	      register rtx dest = SET_DEST (pat);
	      while (GET_CODE (dest) == SUBREG
		     || GET_CODE (dest) == ZERO_EXTRACT
		     || GET_CODE (dest) == SIGN_EXTRACT
		     || GET_CODE (dest) == STRICT_LOW_PART)
		dest = XEXP (dest, 0);
	      if (GET_CODE (dest) == REG)
		{
		  register int xregno = REGNO (dest);
		  int xnregs;
		  if (REGNO (dest) < FIRST_PSEUDO_REGISTER)
		    xnregs = HARD_REGNO_NREGS (xregno, GET_MODE (dest));
		  else
		    xnregs = 1;
		  if (xregno < regno + nregs && xregno + xnregs > regno)
		    return 0;
		  if (xregno < valueno + valuenregs
		      && xregno + xnregs > valueno)
		    return 0;
		  if (goal_mem_addr_varies)
		    return 0;
		}
	      else if (goal_mem && GET_CODE (dest) == MEM
		       && ! push_operand (dest, GET_MODE (dest)))
		return 0;
	    }
	  else if (GET_CODE (pat) == PARALLEL)
	    {
	      register int i;
	      for (i = XVECLEN (pat, 0) - 1; i >= 0; i--)
		{
		  register rtx v1 = XVECEXP (pat, 0, i);
		  if (GET_CODE (v1) == SET || GET_CODE (v1) == CLOBBER)
		    {
		      register rtx dest = SET_DEST (v1);
		      while (GET_CODE (dest) == SUBREG
			     || GET_CODE (dest) == ZERO_EXTRACT
			     || GET_CODE (dest) == SIGN_EXTRACT
			     || GET_CODE (dest) == STRICT_LOW_PART)
			dest = XEXP (dest, 0);
		      if (GET_CODE (dest) == REG)
			{
			  register int xregno = REGNO (dest);
			  int xnregs;
			  if (REGNO (dest) < FIRST_PSEUDO_REGISTER)
			    xnregs = HARD_REGNO_NREGS (xregno, GET_MODE (dest));
			  else
			    xnregs = 1;
			  if (xregno < regno + nregs
			      && xregno + xnregs > regno)
			    return 0;
			  if (xregno < valueno + valuenregs
			      && xregno + xnregs > valueno)
			    return 0;
			  if (goal_mem_addr_varies)
			    return 0;
			}
		      else if (goal_mem && GET_CODE (dest) == MEM
			       && ! push_operand (dest, GET_MODE (dest)))
			return 0;
		    }
		}
	    }
	  /* If this insn auto-increments or auto-decrements
	     either regno or valueno, return 0 now.
	     If GOAL is a memory ref and its address is not constant,
	     and this insn P increments a register, return 0.
	     That is in lieue of checking whether GOAL uses this register.  */
	  {
	    register rtx link;

	    for (link = REG_NOTES (p); link; link = XEXP (link, 1))
	      if (REG_NOTE_KIND (link) == REG_INC)
		{
		  register int incno = REGNO (XEXP (link, 0));
		  if (incno < regno + nregs && incno >= regno)
		    return 0;
		  if (incno < valueno + valuenregs && incno >= valueno)
		    return 0;
		  if (goal_mem_addr_varies)
		    return 0;
		}
	  }
	}
    }
}

/* Find a place where INCED appears in an increment or decrement operator
   within X, and return the amount INCED is incremented or decremented by.
   The value is always positive.  */

static int
find_inc_amount (x, inced)
     rtx x, inced;
{
  register enum rtx_code code = GET_CODE (x);
  register char *fmt;
  register int i;

  if (code == MEM)
    {
      register rtx addr = XEXP (x, 0);
      if ((GET_CODE (addr) == PRE_DEC
	   || GET_CODE (addr) == POST_DEC
	   || GET_CODE (addr) == PRE_INC
	   || GET_CODE (addr) == POST_INC)
	  && XEXP (addr, 0) == inced)
	return GET_MODE_SIZE (GET_MODE (x));
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  register int tem = find_inc_amount (XEXP (x, i), inced);
	  if (tem != 0)
	    return tem;
	}
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      register int tem = find_inc_amount (XVECEXP (x, i, j), inced);
	      if (tem != 0)
		return tem;
	    }
	}
    }

  return 0;
}
