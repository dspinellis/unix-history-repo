/* Optimize by combining instructions for GNU compiler.
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


/* This module is essentially the "combiner" phase of the U. of Arizona
   Portable Optimizer, but redone to work on our list-structured
   representation for RTL instead of their string representation.

   The LOG_LINKS of each insn identify the most recent assignment
   to each REG used in the insn.  It is a list of previous insns,
   each of which contains a SET for a REG that is used in this insn
   and not used or set in between.  LOG_LINKs never cross basic blocks.
   They were set up by the preceding pass (lifetime analysis).

   We try to combine each pair of insns joined by a logical link.
   We also try to combine triples of insns A, B and C when
   C has a link back to B and B has a link back to A.

   LOG_LINKS does not have links for use of the CC0.  They don't
   need to, because the insn that sets the CC0 is always immediately
   before the insn that tests it.  So we always regard a branch
   insn as having a logical link to the preceding insn.

   We check (with use_crosses_set_p) to avoid combining in such a way
   as to move a computation to a place where its value would be different.

   Combination is done by mathematically substituting the previous
   insn(s) values for the regs they set into the expressions in
   the later insns that refer to these regs.  If the result is a valid insn
   for our target machine, according to the machine description,
   we install it, delete the earlier insns, and update the data flow
   information (LOG_LINKS and REG_NOTES) for what we did.

   To simplify substitution, we combine only when the earlier insn(s)
   consist of only a single assignment.  To simplify updating afterward,
   we never combine when a subroutine call appears in the middle.

   Since we do not represent assignments to CC0 explicitly except when that
   is all an insn does, there is no LOG_LINKS entry in an insn that uses
   the condition code for the insn that set the condition code.
   Fortunately, these two insns must be consecutive.
   Therefore, every JUMP_INSN is taken to have an implicit logical link
   to the preceding insn.  This is not quite right, since non-jumps can
   also use the condition code; but in practice such insns would not
   combine anyway.  */

#include <stdio.h>

#include "config.h"
#include "rtl.h"
#include "flags.h"
#include "regs.h"
#include "basic-block.h"
#include "insn-config.h"
#include "recog.h"

#define max(A,B) ((A) > (B) ? (A) : (B))
#define min(A,B) ((A) < (B) ? (A) : (B))

/* It is not safe to use ordinary gen_lowpart in combine.
   Use gen_lowpart_for_combine instead.  See comments there.  */
#define gen_lowpart dont_use_gen_lowpart_you_dummy

/* Number of attempts to combine instructions in this function.  */

static int combine_attempts;
static int distrib_attempts;

/* Number of attempts that got as far as substitution in this function.  */

static int combine_merges;
static int distrib_merges_1, distrib_merges_2;

/* Number of instructions combined with added SETs in this function.  */

static int combine_extras;

/* Number of instructions combined in this function.  */

static int combine_successes;
static int distrib_successes;

/* Totals over entire compilation.  */

static int total_attempts, total_merges, total_extras, total_successes;
static int total_distrib_attempts, total_distrib_merges_1, total_distrib_merges_2, total_distrib_successes;


/* Vector mapping INSN_UIDs to cuids.
   The cuids are like uids but increase monononically always.
   Combine always uses cuids so that it can compare them.
   But actually renumbering the uids, which we used to do,
   proves to be a bad idea because it makes it hard to compare
   the dumps produced by earlier passes with those from later passes.  */

static int *uid_cuid;

/* Get the cuid of an insn.  */

#define INSN_CUID(INSN) (uid_cuid[INSN_UID (INSN)])


/* Record last point of death of (hard or pseudo) register n.  */

static rtx *reg_last_death;

/* Record last point of modification of (hard or pseudo) register n.  */

static rtx *reg_last_set;

/* Record the cuid of the last insn that invalidated memory
   (anything that writes memory, and subroutine calls).  */

static int mem_last_set;

/* Record the cuid of the last CALL_INSN
   so we can tell whether a potential combination crosses any calls.  */

static int last_call_cuid;

/* When `subst' is called, this is the insn that is being modified
   (by combining in a previous insn).  The PATTERN of this insn
   is still the old pattern partially modified and it should not be
   looked at, but this may be used to examine the successors of the insn
   to judge whether a simplification is valid.  */

static rtx subst_insn;

/* Record one modification to rtl structure
   to be undone by storing old_contents into *where.
   is_int is 1 if the contents are an int.  */

struct undo
{
  rtx *where;
  rtx old_contents;
  int is_int;
};

struct undo_int
{
  int *where;
  int old_contents;
  int is_int;
};

/* Record a bunch of changes to be undone, up to MAX_UNDO of them.
   num_undo says how many are currently recorded.
   storage is nonzero if we must undo the allocation of new storage.
   The value of storage is what to pass to obfree.  */

#define MAX_UNDO 10

struct undobuf
{
  int num_undo;
  char *storage;
  struct undo undo[MAX_UNDO];
};

static struct undobuf undobuf;

/* Number of times the pseudo being substituted for
   was found and replaced.  */

static int n_occurrences;

static void move_deaths ();
static void move_deaths_2 ();
void remove_death ();
static void record_dead_and_set_regs ();
int regno_dead_p ();
static int use_crosses_set_p ();
static int try_combine ();
static rtx try_distrib ();
static rtx subst ();
static void undo_all ();
static void copy_substitutions ();
static void add_links ();
static void remove_links ();
static void add_incs ();
static int adjacent_insns_p ();
static int check_asm_operands ();
static rtx simplify_and_const_int ();
static rtx gen_lowpart_for_combine ();
static void simplify_set_cc0_and ();

/* Main entry point for combiner.  F is the first insn of the function.
   NREGS is the first unused pseudo-reg number.  */

void
combine_instructions (f, nregs)
     rtx f;
     int nregs;
{
  register rtx insn;
  register int i;
  register rtx links, nextlinks;
  rtx prev;

  combine_attempts = 0;
  combine_merges = 0;
  combine_extras = 0;
  combine_successes = 0;
  distrib_attempts = 0;
  distrib_merges_1 = 0;
  distrib_merges_2 = 0;
  distrib_successes = 0;

  reg_last_death = (rtx *) alloca (nregs * sizeof (rtx));
  reg_last_set = (rtx *) alloca (nregs * sizeof (rtx));
  bzero (reg_last_death, nregs * sizeof (rtx));
  bzero (reg_last_set, nregs * sizeof (rtx));

  init_recog ();

  /* Compute maximum uid value so uid_cuid can be allocated.  */

  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    if (INSN_UID (insn) > i)
      i = INSN_UID (insn);

  uid_cuid = (int *) alloca ((i + 1) * sizeof (int));

  /* Compute the mapping from uids to cuids.
     Cuids are numbers assigned to insns, like uids,
     except that cuids increase monotonically through the code.  */

  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    INSN_CUID (insn) = ++i;

  /* Now scan all the insns in forward order.  */

  last_call_cuid = 0;
  mem_last_set = 0;
  prev = 0;

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN
	  || GET_CODE (insn) == CALL_INSN
	  || GET_CODE (insn) == JUMP_INSN)
	{
	retry:
	  /* Try this insn with each insn it links back to.  */

	  for (links = LOG_LINKS (insn); links; links = XEXP (links, 1))
	    if (try_combine (insn, XEXP (links, 0), 0))
	      goto retry;

	  /* Try each sequence of three linked insns ending with this one.  */

	  for (links = LOG_LINKS (insn); links; links = XEXP (links, 1))
	    if (GET_CODE (XEXP (links, 0)) != NOTE)
	      for (nextlinks = LOG_LINKS (XEXP (links, 0)); nextlinks;
		   nextlinks = XEXP (nextlinks, 1))
		if (try_combine (insn, XEXP (links, 0), XEXP (nextlinks, 0)))
		  goto retry;

	  /* Try to combine a jump insn that uses CC0
	     with a preceding insn that sets CC0, and maybe with its
	     logical predecessor as well.
	     This is how we make decrement-and-branch insns.
	     We need this special code because data flow connections
	     via CC0 do not get entered in LOG_LINKS.  */

	  if (GET_CODE (insn) == JUMP_INSN
	      && prev != 0
	      && GET_CODE (prev) == INSN
	      && GET_CODE (PATTERN (prev)) == SET
	      && GET_CODE (SET_DEST (PATTERN (prev))) == CC0)
	    {
	      if (try_combine (insn, prev, 0))
		goto retry;

	      if (GET_CODE (prev) != NOTE)
		for (nextlinks = LOG_LINKS (prev); nextlinks;
		     nextlinks = XEXP (nextlinks, 1))
		  if (try_combine (insn, prev, XEXP (nextlinks, 0)))
		    goto retry;
	    }

	  /* Try to apply the distributive law to this insn
	     and two insns that compute the operands of this one.  */
	  for (links = LOG_LINKS (insn); links; links = XEXP (links, 1))
	    if (GET_CODE (XEXP (links, 0)) != NOTE)
	      for (nextlinks = XEXP (links, 1); nextlinks; nextlinks = XEXP (nextlinks, 1))
		if (GET_CODE (XEXP (nextlinks, 0)) != NOTE)
		  {
		    rtx try_from = 0;

		    if (GET_CODE (PATTERN (XEXP (links, 0))) == SET
			&& find_reg_note (insn, REG_DEAD, SET_DEST (PATTERN (XEXP (links, 0))))
			&& GET_CODE (PATTERN (XEXP (nextlinks, 0))) == SET
			&& find_reg_note (insn, REG_DEAD, SET_DEST (PATTERN (XEXP (nextlinks, 0)))))
		      try_from = try_distrib (insn, XEXP (links, 0), XEXP (nextlinks, 0));
		    if (try_from != 0)
		      {
			insn = try_from;
			goto retry;
		      }
		  }
#if 0
/* Turned off because on 68020 it takes four insns to make
   something like (a[b / 32] & (1 << (31 - (b % 32)))) != 0
   that could actually be optimized, and that's an unlikely piece of code.  */
	  /* If an insn gets or sets a bit field, try combining it
	     with two different insns whose results it uses.  */
	  if (GET_CODE (insn) == INSN
	      && GET_CODE (PATTERN (insn)) == SET
	      && (GET_CODE (SET_DEST (PATTERN (insn))) == ZERO_EXTRACT
		  || GET_CODE (SET_DEST (PATTERN (insn))) == SIGN_EXTRACT
		  || GET_CODE (SET_SRC (PATTERN (insn))) == ZERO_EXTRACT
		  || GET_CODE (SET_SRC (PATTERN (insn))) == SIGN_EXTRACT))
	    {
	      for (links = LOG_LINKS (insn); links; links = XEXP (links, 1))
		if (GET_CODE (XEXP (links, 0)) != NOTE)
		  for (nextlinks = XEXP (links, 1); nextlinks;
		       nextlinks = XEXP (nextlinks, 1))
		    if (try_combine (insn, XEXP (links, 0), XEXP (nextlinks, 0)))
		      goto retry;
	    }
#endif
	  if (GET_CODE (insn) != NOTE)
	    record_dead_and_set_regs (insn);
	  prev = insn;
	}
      else if (GET_CODE (insn) != NOTE)
	prev = 0;
    }
  total_attempts += combine_attempts;
  total_merges += combine_merges;
  total_extras += combine_extras;
  total_successes += combine_successes;
}

/* Try to combine the insns I1 and I2 into I3.
   Here I1 appears earlier than I2, which is earlier than I3.
   I1 can be zero; then we combine just I2 into I3.
 
   Return 1 if successful; if that happens, I1 and I2 are pseudo-deleted
   by turning them into NOTEs, and I3 is modified.
   Return 0 if the combination does not work.  Then nothing is changed.  */

static int
try_combine (i3, i2, i1)
     register rtx i3, i2, i1;
{
  register rtx newpat;
  int added_sets_1 = 0;
  int added_sets_2 = 0;
  int total_sets;
  int i2_is_used;
  register rtx link;
  int insn_code_number;
  rtx i2dest, i2src;
  rtx i1dest, i1src;
  int maxreg;
  rtx temp;
  int i;

  combine_attempts++;

  /* Don't combine with something already used up by combination.  */

  if (GET_CODE (i2) == NOTE
      || (i1 && GET_CODE (i1) == NOTE))
    return 0;

  /* Don't combine across a CALL_INSN, because that would possibly
     change whether the life span of some REGs crosses calls or not,
     and it is a pain to update that information.  */

  if (INSN_CUID (i2) < last_call_cuid
      || (i1 && INSN_CUID (i1) < last_call_cuid))
    return 0;

  /* Can combine only if previous insn is a SET of a REG, a SUBREG or CC0.
     That REG must be either set or dead by the final instruction
     (so that we can safely forget about setting it).
     Also test use_crosses_set_p to make sure that the value
     that is to be substituted for the register
     does not use any registers whose values alter in between.
     Do not try combining with moves from one register to another
     since it is better to let them be tied by register allocation.
     (There is a switch to permit such combination; except the insns
     that copy a function value into another register are never combined
     because moving that too far away from the function call could cause
     something else to be stored in that register in the interim.)

     A set of a SUBREG is considered as if it were a set from
     SUBREG.  Thus, (SET (SUBREG:X (REG:Y...)) (something:X...))
     is handled by substituting (SUBREG:Y (something:X...)) for (REG:Y...).  */

  if (GET_CODE (PATTERN (i2)) != SET)
    return 0;
  i2dest = SET_DEST (PATTERN (i2));
  i2src = SET_SRC (PATTERN (i2));
  if (GET_CODE (i2dest) == SUBREG)
    {
      i2dest = SUBREG_REG (i2dest);
      i2src = gen_rtx (SUBREG, GET_MODE (i2dest), i2src, 0);
    }
  /* Don't eliminate a store in the stack pointer.  */
  if (i2dest == stack_pointer_rtx)
    return 0;
  /* Don't install a subreg involving two modes not tieable.
     It can worsen register allocation, and can even make invalid reload insns,
     since the reg inside may need to be copied from in the outside mode,
     and that may be invalid if it is an fp reg copied in integer mode.  */
  if (GET_CODE (i2src) == SUBREG
      && ! MODES_TIEABLE_P (GET_MODE (i2src), GET_MODE (SUBREG_REG (i2src))))
    return 0;
  if (GET_CODE (i2dest) != CC0
      && (GET_CODE (i2dest) != REG
	  || (GET_CODE (i2src) == REG
	      /* Do allow the combination of y = x; x = y; (with x dead)
		 because the result will turn into nothing.  */
	      && !(GET_CODE (PATTERN (i3)) == SET
		   && i2src == SET_DEST (PATTERN (i3)))
	      && (!flag_combine_regs
		  /* Don't substitute a function value reg for any other.  */
		  || FUNCTION_VALUE_REGNO_P (REGNO (i2src))))
	  || GET_CODE (i2src) == CALL
	  /* Don't substitute into an incremented register.  */
	  || find_reg_note (i3, REG_INC, i2dest)
	  || use_crosses_set_p (i2src, INSN_CUID (i2))))
    return 0;
  if (GET_CODE (i2src) == ASM_OPERANDS && MEM_VOLATILE_P (i2src))
    return 0;
  /* Don't substitute for a register intended as a clobberable operand.  */
  if (GET_CODE (PATTERN (i3)) == PARALLEL)
    for (i = 0; i < XVECLEN (PATTERN (i3), 0); i++)
      if (GET_CODE (XVECEXP (PATTERN (i3), 0, i)) == CLOBBER
	  && XEXP (XVECEXP (PATTERN (i3), 0, i), 0) == i2dest)
	return 0;

  if (i1 != 0)
    {
      if (GET_CODE (PATTERN (i1)) != SET)
	return 0;
      i1dest = SET_DEST (PATTERN (i1));
      i1src = SET_SRC (PATTERN (i1));
      if (GET_CODE (i1dest) == SUBREG)
	{
	  i1dest = SUBREG_REG (i1dest);
	  i1src = gen_rtx (SUBREG, GET_MODE (i1dest), i1src, 0);
	}
      if (i1dest == stack_pointer_rtx)
	return 0;
      if (GET_CODE (i1src) == SUBREG
	  && ! MODES_TIEABLE_P (GET_MODE (i1src),
				GET_MODE (SUBREG_REG (i1src))))
	return 0;
      if (GET_CODE (i1dest) != CC0
	  && (GET_CODE (i1dest) != REG
	      || (GET_CODE (i1src) == REG
		  && (!flag_combine_regs
		      || FUNCTION_VALUE_REGNO_P (REGNO (i1src))))
	      || GET_CODE (i1src) == CALL
	      || find_reg_note (i3, REG_INC, i1dest)
	      || find_reg_note (i2, REG_INC, i1dest)
	      || use_crosses_set_p (i1src, INSN_CUID (i1))))
	return 0;
      if (GET_CODE (i1src) == ASM_OPERANDS && MEM_VOLATILE_P (i1src))
	return 0;
      /* Don't substitute for a register intended as a clobberable operand.  */
      if (GET_CODE (PATTERN (i3)) == PARALLEL)
	for (i = 0; i < XVECLEN (PATTERN (i3), 0); i++)
	  if (GET_CODE (XVECEXP (PATTERN (i3), 0, i)) == CLOBBER
	      && XEXP (XVECEXP (PATTERN (i3), 0, i), 0) == i1dest)
	    return 0;
    }

  /* If it is better that two different modes keep two different pseudos,
     avoid combining them.  */
  if (GET_CODE (PATTERN (i3)) == SET)
    {
      rtx i3dest = SET_DEST (PATTERN (i3));
      while (GET_CODE (i3dest) == SUBREG
	     || GET_CODE (i3dest) == STRICT_LOW_PART
	     || GET_CODE (i3dest) == SIGN_EXTRACT
	     || GET_CODE (i3dest) == ZERO_EXTRACT)
	i3dest = SUBREG_REG (i3dest);

      if (SET_SRC (PATTERN (i3)) == i2dest
	  && GET_CODE (i3dest) == REG
	  && ! MODES_TIEABLE_P (GET_MODE (i2dest), GET_MODE (i3dest)))
	return 0;
    }

  /* If I2 contains anything volatile, reject, unless nothing
     volatile comes between it and I3.  */
  if (volatile_refs_p (PATTERN (i2)))
    {
      rtx insn;
      for (insn = NEXT_INSN (i2); insn != i3; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	    || GET_CODE (insn) == JUMP_INSN)
	  if (volatile_refs_p (PATTERN (insn)))
	    return 0;
    }
  /* Likewise for I1; nothing volatile can come between it and I3,
     except optionally I2.  */
  if (i1 && volatile_refs_p (PATTERN (i1)))
    {
      rtx insn;
      rtx end = (volatile_refs_p (PATTERN (i2)) ? i2 : i3);
      for (insn = NEXT_INSN (i1); insn != end; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	    || GET_CODE (insn) == JUMP_INSN)
	  if (volatile_refs_p (PATTERN (insn)))
	    return 0;
    }

  /* If I1 or I2 contains an autoincrement or autodecrement,
     make sure that register is not used between there and I3,
     and not already used in I3 either.
     Also insist that I3 not be a jump; if it were one
     and the incremented register were spilled, we would lose.  */
  for (link = REG_NOTES (i2); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) == REG_INC
	&& (GET_CODE (i3) == JUMP_INSN
	    || reg_used_between_p (XEXP (link, 0), i2, i3)
	    || reg_mentioned_p (XEXP (link, 0), PATTERN (i3))))
      return 0;

  if (i1)
    for (link = REG_NOTES (i1); link; link = XEXP (link, 1))
      if (REG_NOTE_KIND (link) == REG_INC
	  && (GET_CODE (i3) == JUMP_INSN
	      || reg_used_between_p (XEXP (link, 0), i1, i3)
	      || reg_mentioned_p (XEXP (link, 0), PATTERN (i3))))
	return 0;

  /* If I3 has an inc, then give up if I1 or I2 uses the reg that is inc'd,
     EXCEPT in one case: I3 has a post-inc in an output operand.  */
  if (!(GET_CODE (PATTERN (i3)) == SET
	&& GET_CODE (SET_SRC (PATTERN (i3))) == REG
	&& GET_CODE (SET_DEST (PATTERN (i3))) == MEM
	&& (GET_CODE (XEXP (SET_DEST (PATTERN (i3)), 0)) == POST_INC
	    || GET_CODE (XEXP (SET_DEST (PATTERN (i3)), 0)) == POST_DEC)))
    /* It's not the exception.  */
    for (link = REG_NOTES (i3); link; link = XEXP (link, 1))
      if (REG_NOTE_KIND (link) == REG_INC
	  && (reg_mentioned_p (XEXP (link, 0), PATTERN (i2))
	      || (i1 != 0
		  && reg_mentioned_p (XEXP (link, 0), PATTERN (i1)))))
	return 0;

  /* Don't combine an insn I1 or I2 that follows a CC0-setting insn.
     An insn that uses CC0 must not be separated from the one that sets it.
     It would be more logical to test whether CC0 occurs inside I1 or I2,
     but that would be much slower, and this ought to be equivalent.  */
  temp = PREV_INSN (i2);
  while (temp && GET_CODE (temp) == NOTE)
    temp = PREV_INSN (temp);
  if (temp && GET_CODE (temp) == INSN && sets_cc0_p (PATTERN (temp)))
    return 0;
  if (i1)
    {
      temp = PREV_INSN (i2);
      while (temp && GET_CODE (temp) == NOTE)
	temp = PREV_INSN (temp);
      if (temp && GET_CODE (temp) == INSN && sets_cc0_p (PATTERN (temp)))
	return 0;
    }

  /* See if the SETs in i1 or i2 need to be kept around in the merged
     instruction: whenever the value set there is still needed past i3.  */
  added_sets_2 = (GET_CODE (i2dest) != CC0
		  && ! dead_or_set_p (i3, i2dest));
  if (i1)
    added_sets_1 = ! (dead_or_set_p (i3, i1dest)
		      || dead_or_set_p (i2, i1dest));

  combine_merges++;

  undobuf.num_undo = 0;
  undobuf.storage = 0;

  /* Substitute in the latest insn for the regs set by the earlier ones.  */

  maxreg = max_reg_num ();

  subst_insn = i3;
  n_occurrences = 0;		/* `subst' counts here */

  newpat = subst (PATTERN (i3), i2dest, i2src);
  /* Record whether i2's body now appears within i3's body.  */
  i2_is_used = n_occurrences;

  if (i1)
    {
      n_occurrences = 0;
      newpat = subst (newpat, i1dest, i1src);
    }

  if (GET_CODE (PATTERN (i3)) == SET
      && SET_DEST (PATTERN (i3)) == cc0_rtx
      && (GET_CODE (SET_SRC (PATTERN (i3))) == AND
	  || GET_CODE (SET_SRC (PATTERN (i3))) == LSHIFTRT)
      && next_insn_tests_no_inequality (i3))
    simplify_set_cc0_and (i3);

  if (max_reg_num () != maxreg)
    abort ();

  /* If the actions of the earler insns must be kept
     in addition to substituting them into the latest one,
     we must make a new PARALLEL for the latest insn
     to hold additional the SETs.  */

  if (added_sets_1 || added_sets_2)
    {
      combine_extras++;

      /* Arrange to free later what we allocate now
	 if we don't accept this combination.  */
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);

      if (GET_CODE (newpat) == PARALLEL)
	{
	  rtvec old = XVEC (newpat, 0);
	  total_sets = XVECLEN (newpat, 0) + added_sets_1 + added_sets_2;
	  newpat = gen_rtx (PARALLEL, VOIDmode, rtvec_alloc (total_sets));
	  bcopy (&old->elem[0], &XVECEXP (newpat, 0, 0),
		 sizeof (old->elem[0]) * old->num_elem);
	}
      else
	{
	  rtx old = newpat;
	  total_sets = 1 + added_sets_1 + added_sets_2;
	  newpat = gen_rtx (PARALLEL, VOIDmode, rtvec_alloc (total_sets));
	  XVECEXP (newpat, 0, 0) = old;
	}
     if (added_sets_1)
	{
	  XVECEXP (newpat, 0, --total_sets) = PATTERN (i1);
	}
     if (added_sets_2)
	{
	  /* If there is no I1, use I2's body as is.  */
	  if (i1 == 0
	  /* If I2 was stuck into I3, then anything within it has
	     already had I1 substituted into it when that was done to I3.  */
	      || i2_is_used)
	    {
	      XVECEXP (newpat, 0, --total_sets) = PATTERN (i2);
	    }
	  else
	    XVECEXP (newpat, 0, --total_sets)
	      = subst (PATTERN (i2), i1dest, i1src);
	}
    }

  /* Fail if an autoincrement side-effect has been duplicated.  */
  if ((i2_is_used > 1 && find_reg_note (i2, REG_INC, 0) != 0)
      || (i1 != 0 && n_occurrences > 1 && find_reg_note (i1, REG_INC, 0) != 0))
    {
      undo_all ();
      return 0;
    }

  /* Is the result of combination a valid instruction?  */
  insn_code_number = recog (newpat, i3);

  if (insn_code_number >= 0
      /* Is the result a reasonable ASM_OPERANDS?  */
      || (check_asm_operands (newpat) && ! added_sets_1 && ! added_sets_2))
    {
      /* Yes.  Install it.  */
      register int regno;
      INSN_CODE (i3) = insn_code_number;
      PATTERN (i3) = newpat;
      /* If anything was substituted more than once,
	 copy it to avoid invalid shared rtl structure.  */
      copy_substitutions ();
      /* The data flowing into I2 now flows into I3.
	 But we cannot always move all of I2's LOG_LINKS into I3,
	 since they must go to a setting of a REG from the
	 first use following.  If I2 was the first use following a set,
	 I3 is now a use, but it is not the first use
	 if some instruction between I2 and I3 is also a use.
	 Here, for simplicity, we move all the links only if
	 there are no real insns between I2 and I3.
	 Otherwise, we move only links that correspond to regs
	 that used to die in I2.  They are always safe to move.  */
      add_links (i3, i2, adjacent_insns_p (i2, i3));
      /* Most REGs that previously died in I2 now die in I3.  */ 
      move_deaths (i2src, INSN_CUID (i2), i3);
      if (GET_CODE (i2dest) == REG)
	{
	  /* If the reg formerly set in I2 died only once and that was in I3,
	     zero its use count so it won't make `reload' do any work.  */
	  regno = REGNO (i2dest);
	  if (! added_sets_2)
	    {
	      reg_n_sets[regno]--;
	      /* Used to check  && regno_dead_p (regno, i3)  also here.  */
	      if (reg_n_sets[regno] == 0
		  && ! (basic_block_live_at_start[0][regno / HOST_BITS_PER_INT]
			& (1 << (regno % HOST_BITS_PER_INT))))
		reg_n_refs[regno] = 0;
	    }
	  /* If a ref to REGNO was substituted into I3 from I2,
	     then it still dies there if it previously did.
	     Otherwise either REGNO never did die in I3 so remove_death is safe
	     or this entire life of REGNO is gone so remove its death.  */
	  if (!added_sets_2
	      && ! reg_mentioned_p (i2dest, PATTERN (i3)))
	    remove_death (regno, i3);
	}
      /* Any registers previously autoincremented in I2
	 are now incremented in I3.  */
      add_incs (i3, REG_NOTES (i2));
      if (i1)
	{
	  /* Likewise, merge the info from I1 and get rid of it.  */
	  add_links (i3, i1,
		     adjacent_insns_p (i1, i2) && adjacent_insns_p (i2, i3));
	  move_deaths (i1src, INSN_CUID (i1), i3);
	  if (GET_CODE (i1dest) == REG)
	    {
	      regno = REGNO (i1dest);
	      if (! added_sets_1)
		{
		  reg_n_sets[regno]--;
		  /* Used to also check  && regno_dead_p (regno, i3) here.  */

		  if (reg_n_sets[regno] == 0
		      && ! (basic_block_live_at_start[0][regno / HOST_BITS_PER_INT]
			    & (1 << (regno % HOST_BITS_PER_INT))))

		    reg_n_refs[regno] = 0;
		}
	      /* If a ref to REGNO was substituted into I3 from I1,
		 then it still dies there if it previously did.
		 Else either REGNO never did die in I3 so remove_death is safe
		 or this entire life of REGNO is gone so remove its death.  */
	      if (! added_sets_1
		  && ! reg_mentioned_p (i1dest, PATTERN (i3)))
		remove_death (regno, i3);
	    }
	  add_incs (i3, REG_NOTES (i1));
	  LOG_LINKS (i1) = 0;
	  PUT_CODE (i1, NOTE);
	  NOTE_LINE_NUMBER (i1) = NOTE_INSN_DELETED;
	  NOTE_SOURCE_FILE (i1) = 0;
	}
      /* Get rid of I2.  */
      LOG_LINKS (i2) = 0;
      PUT_CODE (i2, NOTE);
      NOTE_LINE_NUMBER (i2) = NOTE_INSN_DELETED;
      NOTE_SOURCE_FILE (i2) = 0;

      combine_successes++;
      return 1;
    }

  /* Failure: change I3 back the way it was.  */
  undo_all ();

  return 0;
}

/* Undo all the modifications recorded in undobuf.  */

static void
undo_all ()
{
  register int i;
  if (undobuf.num_undo > MAX_UNDO)
    undobuf.num_undo = MAX_UNDO;
  for (i = undobuf.num_undo - 1; i >= 0; i--)
    *undobuf.undo[i].where = undobuf.undo[i].old_contents;
  if (undobuf.storage)
    obfree (undobuf.storage);
  undobuf.num_undo = 0;
  undobuf.storage = 0;
}

/* If this insn had more than one substitution,
   copy all but one, so that no invalid shared substructure is introduced.  */

static void
copy_substitutions ()
{
  register int i;
  if (undobuf.num_undo > 1)
    {
      for (i = undobuf.num_undo - 1; i >= 1; i--)
	if (! undobuf.undo[i].is_int)
	  *undobuf.undo[i].where = copy_rtx (*undobuf.undo[i].where);
    }
}

/* Throughout X, replace FROM with TO, and return the result.
   The result is TO if X is FROM;
   otherwise the result is X, but its contents may have been modified.
   If they were modified, a record was made in undobuf so that
   undo_all will (among other things) return X to its original state.

   If the number of changes necessary is too much to record to undo,
   the excess changes are not made, so the result is invalid.
   The changes already made can still be undone.
   undobuf.num_undo is incremented for such changes, so by testing that
   the caller can tell whether the result is valid.

   `n_occurrences' is incremented each time FROM is replaced.  */

static rtx
subst (x, from, to)
     register rtx x, from, to;
{
  register char *fmt;
  register int len, i;
  register enum rtx_code code;
  char was_replaced[2];

#define SUBST(INTO, NEWVAL)  \
 do { if (undobuf.num_undo < MAX_UNDO)					\
	{								\
	  undobuf.undo[undobuf.num_undo].where = &INTO;			\
	  undobuf.undo[undobuf.num_undo].old_contents = INTO;		\
	  undobuf.undo[undobuf.num_undo].is_int = 0;			\
	  INTO = NEWVAL;						\
	}								\
      undobuf.num_undo++; } while (0)

#define SUBST_INT(INTO, NEWVAL)  \
 do { if (undobuf.num_undo < MAX_UNDO)					\
	{								\
	  struct undo_int *u = (struct undo_int *)&undobuf.undo[undobuf.num_undo];\
	  u->where = &INTO;						\
	  u->old_contents = INTO;					\
	  u->is_int = 1;						\
	  INTO = NEWVAL;						\
	}								\
      undobuf.num_undo++; } while (0)

/* FAKE_EXTEND_SAFE_P (MODE, FROM) is 1 if (subreg:MODE FROM 0) is a safe
   replacement for (zero_extend:MODE FROM) or (sign_extend:MODE FROM).
   If it is 0, that cannot be done.  We can now do this for any MEM
   because (SUBREG (MEM...)) is guaranteed to cause the MEM to be reloaded.
   If not for that, MEM's would very rarely be safe.  */

/* Reject MODEs bigger than a word, because we might not be able
   to reference a two-register group starting with an arbitrary register
   (and currently gen_lowpart might crash for a SUBREG).  */

#define FAKE_EXTEND_SAFE_P(MODE, FROM) \
  (GET_MODE_SIZE (MODE) <= UNITS_PER_WORD			\
   && (GET_CODE (FROM) == REG || GET_CODE (FROM) == SUBREG	\
       || GET_CODE (FROM) == MEM))

  if (x == from)
    return to;

  /* It is possible to have a subexpression appear twice in the insn.
     Suppose that FROM is a register that appears within TO.
     Then, after that subexpression has been scanned once by `subst',
     the second time it is scanned, TO may be found.  If we were
     to scan TO here, we would find FROM within it and create a
     self-referent rtl structure which is completely wrong.  */
  if (x == to)
    return to;

  code = GET_CODE (x);

  /* A little bit of algebraic simplification here.  */
  switch (code)
    {
      /* This case has no effect except to speed things up.  */
    case REG:
    case CONST_INT:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case PC:
    case CC0:
      return x;
    }

  was_replaced[0] = 0;
  was_replaced[1] = 0;

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  /* Don't replace FROM where it is being stored in rather than used.  */
  if (code == SET && SET_DEST (x) == from)
    fmt = "ie";
  if (code == SET && GET_CODE (SET_DEST (x)) == SUBREG
      && SUBREG_REG (SET_DEST (x)) == from)
    fmt = "ie";

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    {
	      register rtx new;
	      if (XVECEXP (x, i, j) == from)
		new = to, n_occurrences++;
	      else
		new = subst (XVECEXP (x, i, j), from, to);
	      if (new != XVECEXP (x, i, j))
		SUBST (XVECEXP (x, i, j), new);
	    }
	}
      else if (fmt[i] == 'e')
	{
	  register rtx new;

	  if (XEXP (x, i) == from)
	    {
	      new = to;
	      n_occurrences++;
	      if (i < 2)
		was_replaced[i] = 1;
	    }
	  else
	    new = subst (XEXP (x, i), from, to);

	  if (new != XEXP (x, i))
	    SUBST (XEXP (x, i), new);
	}
    }

  /* A little bit of algebraic simplification here.  */
  switch (code)
    {
    case SUBREG:
      /* Changing mode twice with SUBREG => just change it once,
	 or not at all if changing back to starting mode.  */
      if (SUBREG_REG (x) == to
	  && GET_CODE (to) == SUBREG)
	{
	  if (GET_MODE (x) == GET_MODE (SUBREG_REG (to)))
	    if (SUBREG_WORD (x) == 0 && SUBREG_WORD (to) == 0)
	      return SUBREG_REG (to);
	  SUBST (SUBREG_REG (x), SUBREG_REG (to));
	  if (SUBREG_WORD (to) != 0)
	    SUBST_INT (SUBREG_WORD (x), SUBREG_WORD (x) + SUBREG_WORD (to));
	}
      if (SUBREG_REG (x) == to
	  && (GET_CODE (to) == SIGN_EXTEND || GET_CODE (to) == ZERO_EXTEND)
	  && subreg_lowpart_p (x))
	{
	  /* (subreg (sign_extend X)) is X, if it has same mode as X.  */
	  if (GET_MODE (x) == GET_MODE (XEXP (to, 0)))
	    return XEXP (to, 0);
	  /* (subreg (sign_extend X)), if it has a mode wider than X,
	     can be done with (sign_extend X).  */
	  if (GET_MODE_SIZE (GET_MODE (x)) > GET_MODE_SIZE (GET_MODE (XEXP (to, 0))))
	    {
	      if (!undobuf.storage)
		undobuf.storage = (char *) oballoc (0);
	      return gen_rtx (GET_CODE (to), GET_MODE (x), XEXP (to, 0));
	    }
	  /* Extend and then truncate smaller than it was to start with:
	     no need to extend.  */
	  if (GET_MODE_SIZE (GET_MODE (x)) < GET_MODE_SIZE (GET_MODE (XEXP (to, 0))))
	    {
	      SUBST (XEXP (x, 0), XEXP (to, 0));
	    }
	}
      /* (subreg:A (mem:B X) N) becomes a modified MEM.
	 If we can't do that safely, then it becomes something nonsensical
	 so that this combination won't take place.
	 This avoids producing any (subreg (mem))s except in the special
	 paradoxical case where gen_lowpart_for_combine makes them.  */
      if (SUBREG_REG (x) == to
	  && GET_CODE (to) == MEM)
	{
	  int endian_offset = 0;
	  /* Don't combine this if mode A is wider than B.  */
	  if (GET_MODE_SIZE (GET_MODE (x)) > GET_MODE_SIZE (GET_MODE (to)))
	    return gen_rtx (CLOBBER, VOIDmode, const0_rtx);
	  /* Don't change the mode of the MEM
	     if that would change the meaning of the address.  */
	  if (mode_dependent_address_p (XEXP (to, 0)))
	    return gen_rtx (CLOBBER, VOIDmode, const0_rtx);
#ifdef BYTES_BIG_ENDIAN
	  if (GET_MODE_SIZE (GET_MODE (x)) < UNITS_PER_WORD)
	    endian_offset += UNITS_PER_WORD - GET_MODE_SIZE (GET_MODE (x));
	  if (GET_MODE_SIZE (GET_MODE (to)) < UNITS_PER_WORD)
	    endian_offset -= UNITS_PER_WORD - GET_MODE_SIZE (GET_MODE (to));
#endif
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  /* Note if the plus_constant doesn't make a valid address
	     then this combination won't be accepted.  */
	  return gen_rtx (MEM, GET_MODE (x),
			  plus_constant (XEXP (to, 0),
					 (SUBREG_WORD (x) * UNITS_PER_WORD
					  + endian_offset)));
	}
      break;

    case NOT:
      /* (not (minus X 1)) can become (neg X).  */
      if (was_replaced[0]
	  && ((GET_CODE (to) == PLUS && INTVAL (XEXP (to, 1)) == -1)
	      || (GET_CODE (to) == MINUS && XEXP (to, 1) == const1_rtx)))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (NEG, GET_MODE (to), XEXP (to, 0));
	}
      /* Don't let substitution introduce double-negatives.  */
      if (was_replaced[0]
	  && GET_CODE (to) == code)
	return XEXP (to, 0);
      break;

    case NEG:
      /* (neg (minus X Y)) can become (minus Y X).  */
      if (was_replaced[0] && GET_CODE (to) == MINUS)
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (MINUS, GET_MODE (to),
			  XEXP (to, 1), XEXP (to, 0));
	}
      /* Don't let substitution introduce double-negatives.  */
      if (was_replaced[0]
	  && GET_CODE (to) == code)
	return XEXP (to, 0);
      break;

    case FLOAT_TRUNCATE:
      /* (float_truncate:SF (float_extend:DF foo:SF)) = foo:SF.  */
      if (was_replaced[0]
	  && GET_CODE (to) == FLOAT_EXTEND
	  && GET_MODE (XEXP (to, 0)) == GET_MODE (x))
	return XEXP (to, 0);
      break;

#if 0
    case COMPARE:
      /* -x>0 if 0>x.  */
      if (GET_CODE (XEXP (x, 0)) == NEG && XEXP (x, 1) == const0_rtx)
	{
	  SUBST (XEXP (x, 1), XEXP (XEXP (x, 0), 0));
	  SUBST (XEXP (x, 0), const0_rtx);
	}
      if (GET_CODE (XEXP (x, 1)) == NEG && XEXP (x, 0) == const0_rtx)
	{
	  SUBST (XEXP (x, 0), XEXP (XEXP (x, 1), 0));
	  SUBST (XEXP (x, 1), const0_rtx);
	}
      break;
#endif

    case PLUS:
#if 0  /* Turned off for caution: turn it on after 1.36.  */
      /* Identify constant sums as such.  */
      if ((was_replaced[0] || was_replaced[1])
	  && CONSTANT_P (XEXP (x, 0))
	  && CONSTANT_P (XEXP (x, 1)))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (CONST, GET_MODE (x), x);
	}
#endif
      /* In (plus <foo> (ashift <bar> <n>))
	 change the shift to a multiply so we can recognize
	 scaled indexed addresses.  */
      if ((was_replaced[0]
	   || was_replaced[1])
	  && GET_CODE (to) == ASHIFT
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && INTVAL (XEXP (to, 1)) < HOST_BITS_PER_INT)
	{
	  rtx temp;
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  temp = gen_rtx (MULT, GET_MODE (to),
			  XEXP (to, 0),
			  gen_rtx (CONST_INT, VOIDmode,
				   1 << INTVAL (XEXP (to, 1))));
	  if (was_replaced[0])
	    SUBST (XEXP (x, 0), temp);
	  else
	    SUBST (XEXP (x, 1), temp);
	}
      /* (plus X (neg Y)) becomes (minus X Y).  */
      if (GET_CODE (XEXP (x, 1)) == NEG)
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (MINUS, GET_MODE (x),
			  XEXP (x, 0), XEXP (XEXP (x, 1), 0));
	}
      /* (plus (neg X) Y) becomes (minus Y X).  */
      if (GET_CODE (XEXP (x, 0)) == NEG)
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (MINUS, GET_MODE (x),
			  XEXP (x, 1), XEXP (XEXP (x, 0), 0));
	}
      /* (plus (plus x c1) c2) => (plus x c1+c2) */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (x, 0)) == PLUS
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	{
	  int sum = (INTVAL (XEXP (x, 1))
		     + INTVAL (XEXP (XEXP (x, 0), 1)));
	  if (sum == 0)
	    return XEXP (XEXP (x, 0), 0);
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  SUBST (XEXP (x, 1), gen_rtx (CONST_INT, VOIDmode, sum));
	  SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));
	  break;
	}
      /* If we have something (putative index) being added to a sum,
	 associate it so that any constant term is outermost.
	 That's because that's the way indexed addresses are
	 now supposed to appear.  */
      if (((was_replaced[0] && GET_CODE (XEXP (x, 1)) == PLUS)
	   || (was_replaced[1] && GET_CODE (XEXP (x, 0)) == PLUS))
	  ||
	  ((was_replaced[0] || was_replaced[1])
	   && GET_CODE (to) == PLUS))
	{
	  rtx offset = 0, base, index;
	  if (GET_CODE (to) != PLUS)
	    {
	      index = to;
	      base = was_replaced[0] ? XEXP (x, 1) : XEXP (x, 0);
	    }
	  else
	    {
	      index = was_replaced[0] ? XEXP (x, 1) : XEXP (x, 0);
	      base = to;
	    }
	  if (CONSTANT_ADDRESS_P (XEXP (base, 0)))
	    {
	      offset = XEXP (base, 0);
	      base = XEXP (base, 1);
	    }
	  else if (CONSTANT_ADDRESS_P (XEXP (base, 1)))
	    {
	      offset = XEXP (base, 1);
	      base = XEXP (base, 0);
	    }
	  if (offset != 0)
	    {
	      if (!undobuf.storage)
		undobuf.storage = (char *) oballoc (0);
	      if (GET_CODE (offset) == CONST_INT)
		return plus_constant (gen_rtx (PLUS, GET_MODE (index),
					       base, index),
				      INTVAL (offset));
	      if (GET_CODE (index) == CONST_INT)
		return plus_constant (gen_rtx (PLUS, GET_MODE (offset),
					       base, offset),
				      INTVAL (index));
	      return gen_rtx (PLUS, GET_MODE (index),
			      gen_rtx (PLUS, GET_MODE (index),
				       base, index),
			      offset);
	    }
	}
      break;

    case EQ:
    case NE:
      /* If comparing a subreg against zero, discard the subreg.  */
      if (was_replaced[0]
	  && GET_CODE (to) == SUBREG
	  && SUBREG_WORD (to) == 0
	  && XEXP (x, 1) == const0_rtx)
	SUBST (XEXP (x, 0), SUBREG_REG (to));

      /* If comparing a ZERO_EXTRACT against zero,
	 canonicalize to a SIGN_EXTRACT,
	 since the two are equivalent here.  */
      if (was_replaced[0]
	  && GET_CODE (to) == ZERO_EXTRACT
	  && XEXP (x, 1) == const0_rtx)
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  SUBST (XEXP (x, 0),
		 gen_rtx (SIGN_EXTRACT, GET_MODE (to),
			  XEXP (to, 0), XEXP (to, 1),
			  XEXP (to, 2)));
	}
#ifndef BITS_BIG_ENDIAN
      /* If we are putting (ASHIFT 1 x) into (EQ (AND ... y) 0),
	 arrange to return (EQ (SIGN_EXTRACT y 1 x) 0),
	 which is what jump-on-bit instructions are written with.  */
      else if (XEXP (x, 1) == const0_rtx
	       && GET_CODE (XEXP (x, 0)) == AND
	       && (XEXP (XEXP (x, 0), 0) == to
		   || XEXP (XEXP (x, 0), 1) == to)
	       && GET_CODE (to) == ASHIFT
	       && XEXP (to, 0) == const1_rtx)
	{
	  register rtx y = XEXP (XEXP (x, 0),
				 XEXP (XEXP (x, 0), 0) == to);
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  SUBST (XEXP (x, 0),
		 gen_rtx (SIGN_EXTRACT, GET_MODE (to),
			  y,
			  const1_rtx, XEXP (to, 1)));
	}
#endif /* not BITS_BIG_ENDIAN */
      /* Negation is a no-op before equality test against zero.  */
      if (GET_CODE (XEXP (x, 0)) == NEG && XEXP (x, 1) == const0_rtx)
	{
	  SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));
	}
      if (GET_CODE (XEXP (x, 1)) == NEG && XEXP (x, 0) == const0_rtx)
	{
	  SUBST (XEXP (x, 1), XEXP (XEXP (x, 1), 0));
	}
      break;

    case ZERO_EXTEND:
      /* Nested zero-extends are equivalent to just one.  */
      if (was_replaced[0]
	  && GET_CODE (to) == ZERO_EXTEND)
	SUBST (XEXP (x, 0), XEXP (to, 0));
      /* Zero extending a constant int can be replaced
	 by a zero-extended constant.  */
      if (was_replaced[0]
	  && HOST_BITS_PER_INT >= GET_MODE_BITSIZE (GET_MODE (from))
	  && GET_CODE (to) == CONST_INT)
	{
	  int intval = INTVAL (to) & GET_MODE_MASK (GET_MODE (from));
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (CONST_INT, VOIDmode, intval);
	}
      /* Zero-extending the result of an and with a constant can be done
	 with a wider and.  */
      if (was_replaced[0]
	  && GET_CODE (to) == AND
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && FAKE_EXTEND_SAFE_P (GET_MODE (x), XEXP (to, 0))
	  /* Avoid getting wrong result if the constant has high bits set
	     that are irrelevant in the narrow mode where it is being used.  */
	  && 0 == (INTVAL (XEXP (to, 1))
		   & ~ GET_MODE_MASK (GET_MODE (to))))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (AND, GET_MODE (x),
			  gen_lowpart_for_combine (GET_MODE (x), XEXP (to, 0)),
			  XEXP (to, 1));
	} 
      /* Change (zero_extend:M (subreg:N (zero_extract:M ...) 0))
	 to (zero_extract:M ...) if the field extracted fits in mode N.  */
      if (GET_CODE (XEXP (x, 0)) == SUBREG
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == ZERO_EXTRACT
	  && GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)) == CONST_INT
	  && (INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1))
	      <= GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0)))))
	{
	  return XEXP (XEXP (x, 0), 0);
	}
      /* Change (zero_extend:M (subreg:N (and:M ... <const>) 0))
	 to (and:M ...) if the significant bits fit in mode N.  */
      if (GET_CODE (XEXP (x, 0)) == SUBREG
	  && SUBREG_REG (XEXP (x, 0)) == to
	  && SUBREG_WORD (XEXP (x, 0)) == 0
	  && GET_CODE (to) == AND
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && FAKE_EXTEND_SAFE_P (GET_MODE (x), XEXP (to, 0))
	  /* Avoid getting wrong result if the constant has high bits set
	     that are irrelevant in the narrow mode where it is being used.  */
	  && 0 == (INTVAL (XEXP (to, 1))
		   & ~ GET_MODE_MASK (GET_MODE (to))))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (AND, GET_MODE (x),
			  gen_lowpart_for_combine (GET_MODE (x), XEXP (to, 0)),
			  XEXP (to, 1));
	}
      /* In (zero_extend:M (subreg:N (lshiftrt:M REG))),
	 where REG was assigned from (zero_extend:M (any:N ...)),
	 remove the outer zero extension.  */
      if (GET_CODE (XEXP (x, 0)) == SUBREG
	  && SUBREG_REG (XEXP (x, 0)) == to
	  && SUBREG_WORD (XEXP (x, 0)) == 0
	  && GET_CODE (to) == LSHIFTRT)
	{
	  rtx tmp = XEXP (to, 0);

	  /* See if arg of LSHIFTRT is a register whose value we can find.  */
	  if (GET_CODE (tmp) == REG)
	    if (reg_n_sets[REGNO (tmp)] == 1
		&& reg_last_set[REGNO (tmp)] != 0
		&& SET_DEST (PATTERN (reg_last_set[REGNO (tmp)])) == tmp)
	      tmp = SET_SRC (PATTERN (reg_last_set[REGNO (tmp)]));
	    else
	      break;

	  if (GET_CODE (tmp) == ZERO_EXTEND
	      && GET_MODE (tmp) == GET_MODE (x)
	      && GET_MODE (XEXP (tmp, 0)) == GET_MODE (XEXP (x, 0)))
	    return SUBREG_REG (XEXP (x, 0));
	}
      break;

    case SIGN_EXTEND:
      /* Nested sign-extends are equivalent to just one.  */
      if (was_replaced[0]
	  && GET_CODE (to) == SIGN_EXTEND)
	SUBST (XEXP (x, 0), XEXP (to, 0));
      /* Sign extending a constant int can be replaced
	 by a sign-extended constant.  */
      if (was_replaced[0]
	  && HOST_BITS_PER_INT >= GET_MODE_BITSIZE (GET_MODE (from))
	  && GET_CODE (to) == CONST_INT)
	{
	  int intval = INTVAL (to);
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  if (intval > 0
	      && (intval & (1 << (GET_MODE_BITSIZE (GET_MODE (from)) - 1))))
	    intval |= ~ GET_MODE_MASK (GET_MODE (from));
	  return gen_rtx (CONST_INT, VOIDmode, intval);
	}
      /* Sign-extending the result of an and with a constant can be done
	 with a wider and, provided the high bit of the constant is 0.  */
      if (was_replaced[0]
	  && GET_CODE (to) == AND
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && FAKE_EXTEND_SAFE_P (GET_MODE (x), XEXP (to, 0))
	  && ((INTVAL (XEXP (to, 1))
	       & (-1 << (GET_MODE_BITSIZE (GET_MODE (to)) - 1)))
	      == 0))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (AND, GET_MODE (x),
			  gen_lowpart_for_combine (GET_MODE (x), XEXP (to, 0)),
			  XEXP (to, 1));
	 } 
      /* hacks added by tiemann.  */
      /* Change (sign_extend:M (subreg:N (and:M ... <const>) 0))
	 to (and:M ...), provided the result fits in mode N,
	 and the high bit of the constant is 0 in mode N.  */
      if (GET_CODE (XEXP (x, 0)) == SUBREG
	  && SUBREG_REG (XEXP (x, 0)) == to
	  && SUBREG_WORD (XEXP (x, 0)) == 0
	  && GET_CODE (to) == AND
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && FAKE_EXTEND_SAFE_P (GET_MODE (x), XEXP (to, 0))
	  && ((INTVAL (XEXP (to, 1))
	       & (-1 << (GET_MODE_BITSIZE (GET_MODE (XEXP (x, 0))) - 1)))
	      == 0))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (AND, GET_MODE (x),
			  gen_lowpart_for_combine (GET_MODE (x), XEXP (to, 0)),
			  XEXP (to, 1));
	} 
      /* In (sign_extend:M (subreg:N (ashiftrt:M REG))),
	 where REG was assigned from (sign_extend:M (any:N ...)),
	 remove the outer sign extension.  */
      if (GET_CODE (XEXP (x, 0)) == SUBREG
	  && SUBREG_REG (XEXP (x, 0)) == to
	  && SUBREG_WORD (XEXP (x, 0)) == 0
	  && GET_CODE (to) == ASHIFTRT)
	{
	  rtx tmp = XEXP (to, 0);

	  /* See if arg of LSHIFTRT is a register whose value we can find.  */
	  if (GET_CODE (tmp) == REG)
	    if (reg_n_sets[REGNO (tmp)] == 1
		&& reg_last_set[REGNO (tmp)] != 0
		&& SET_DEST (PATTERN (reg_last_set[REGNO (tmp)])) == tmp)
	      tmp = SET_SRC (PATTERN (reg_last_set[REGNO (tmp)]));
	    else
	      break;

	  if (GET_CODE (tmp) == SIGN_EXTEND
	      && GET_MODE (tmp) == GET_MODE (x)
	      && GET_MODE (XEXP (tmp, 0)) == GET_MODE (XEXP (x, 0)))
	    return SUBREG_REG (XEXP (x, 0));
	}
      break;

    case SET:
      /* In (set (zero-extract <x> <n> <y>) (and <foo> <(2**n-1) | anything>))
	 the `and' can be deleted.  This can happen when storing a bit
	 that came from a set-flag insn followed by masking to one bit.  */
      if (GET_CODE (XEXP (x, 0)) == ZERO_EXTRACT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && was_replaced[1]
	  && GET_CODE (to) == AND
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && 0 == (((1 << INTVAL (XEXP (XEXP (x, 0), 1))) - 1)
		   & ~ INTVAL (XEXP (to, 1))))
	{
	  SUBST (XEXP (x, 1), XEXP (to, 0));
	} 
      /* In (set (zero-extract <x> <n> <y>)
		 (subreg (and <foo> <(2**n-1) | anything>)))
	 the `and' can be deleted.  */
      if (GET_CODE (XEXP (x, 0)) == ZERO_EXTRACT
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && GET_CODE (XEXP (x, 1)) == SUBREG
	  && SUBREG_WORD (XEXP (x, 1)) == 0
	  && GET_CODE (SUBREG_REG (XEXP (x, 1))) == AND
	  && GET_CODE (XEXP (SUBREG_REG (XEXP (x, 1)), 1)) == CONST_INT
	  && 0 == (((1 << INTVAL (XEXP (XEXP (x, 0), 1))) - 1)
		   & ~ INTVAL (XEXP (SUBREG_REG (XEXP (x, 1)), 1))))
	{
	  SUBST (SUBREG_REG (XEXP (x, 1)), XEXP (SUBREG_REG (XEXP (x, 1)), 0));
	} 
      /* (set (zero_extract ...) (and/or/xor (zero_extract ...) const)),
	 if both zero_extracts have the same location, size and position,
	 can be changed to avoid the byte extracts.  */
      if ((GET_CODE (XEXP (x, 0)) == ZERO_EXTRACT
	   || GET_CODE (XEXP (x, 0)) == SIGN_EXTRACT)
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && (GET_CODE (XEXP (x, 1)) == AND
	      || GET_CODE (XEXP (x, 1)) == IOR
	      || GET_CODE (XEXP (x, 1)) == XOR)
	  && rtx_equal_p (XEXP (x, 0), XEXP (XEXP (x, 1), 0))
	  && GET_CODE (XEXP (XEXP (x, 1), 0)) == GET_CODE (XEXP (x, 0))
	  && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
	  /* zero_extract can apply to a QImode even if the bits extracted
	     don't fit inside that byte.  In such a case, we may not do this
	     optimization, since the OR or AND insn really would need
	     to fit in a byte.  */
	  && (INTVAL (XEXP (XEXP (x, 0), 1)) + INTVAL (XEXP (XEXP (x, 0), 2))
	      < GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (x, 0), 0)))))
	{
	  int shiftcount;
	  int newmask;
#ifdef BITS_BIG_ENDIAN
	  shiftcount
	    = GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (x, 0), 0)))
	      - INTVAL (XEXP (XEXP (x, 0), 1)) - INTVAL (XEXP (XEXP (x, 0), 2));
#else
	  shiftcount
	    = INTVAL (XEXP (XEXP (x, 0), 2));
#endif
	  newmask = ((INTVAL (XEXP (XEXP (x, 1), 1)) << shiftcount)
		     + (GET_CODE (XEXP (x, 1)) == AND
			? (1 << shiftcount) - 1
			: 0));
	  if (GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (x, 0), 0)))
	      < HOST_BITS_PER_INT)
	    newmask &= (1 << GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (x, 0), 0)))) - 1;
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return
	    gen_rtx (SET, VOIDmode,
		     XEXP (XEXP (x, 0), 0),
		     gen_rtx (GET_CODE (XEXP (x, 1)),
			      GET_MODE (XEXP (XEXP (x, 0), 0)),
			      XEXP (XEXP (XEXP (x, 1), 0), 0),
			      gen_rtx (CONST_INT, VOIDmode, newmask)));
	}
      /* Can simplify (set (cc0) (compare (zero/sign_extend FOO) CONST))
	 to (set (cc0) (compare FOO CONST)) if CONST fits in FOO's mode
	 and we are only testing equality.
	 In fact, this is valid for zero_extend if what follows is an
	 unsigned comparison, and for sign_extend with a signed comparison.  */
      if (SET_DEST (x) == cc0_rtx
	  && GET_CODE (SET_SRC (x)) == COMPARE
	  && (GET_CODE (XEXP (SET_SRC (x), 0)) == ZERO_EXTEND
	      || GET_CODE (XEXP (SET_SRC (x), 0)) == SIGN_EXTEND)
	  && next_insn_tests_no_inequality (subst_insn)
	  && GET_CODE (XEXP (SET_SRC (x), 1)) == CONST_INT
	  /* This is overly cautious by one bit, but saves worrying about
	     whether it is zero-extension or sign extension.  */
	  && ((unsigned) INTVAL (XEXP (SET_SRC (x), 1))
	      < (1 << (GET_MODE_BITSIZE (GET_MODE (XEXP (XEXP (SET_SRC (x), 0), 0))) - 1))))
	SUBST (XEXP (SET_SRC (x), 0), XEXP (XEXP (SET_SRC (x), 0), 0));
      break;

    case AND:
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  rtx tem = simplify_and_const_int (x, to);
	  if (tem)
	    return tem;
	}
      break;

    case IOR:
    case XOR:
      /* (ior (ior x c1) c2) => (ior x c1|c2); likewise for xor.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (x, 0)) == code
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT)
	{
	  int c0 = INTVAL (XEXP (x, 1));
	  int c1 = INTVAL (XEXP (XEXP (x, 0), 1));
	  int combined = (code == IOR ? c0 | c1 : c0 ^ c1);

	  if (combined == 0)
	    return XEXP (XEXP (x, 0), 0);
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  SUBST (XEXP (x, 1), gen_rtx (CONST_INT, VOIDmode, combined));
	  SUBST (XEXP (x, 0), XEXP (XEXP (x, 0), 0));
	  break;
	}

    case FLOAT:
      /* (float (sign_extend <X>)) = (float <X>).  */
      if (was_replaced[0]
	  && GET_CODE (to) == SIGN_EXTEND)
	SUBST (XEXP (x, 0), XEXP (to, 0));
      break;

    case ZERO_EXTRACT:
      /* (ZERO_EXTRACT (TRUNCATE x)...)
	 can become (ZERO_EXTRACT x ...).  */
      if (was_replaced[0]
	  && GET_CODE (to) == TRUNCATE)
	{
#ifdef BITS_BIG_ENDIAN
	  if (GET_CODE (XEXP (x, 2)) == CONST_INT)
	    {
	      if (!undobuf.storage)
		undobuf.storage = (char *) oballoc (0);
	      /* On a big-endian machine, must increment the bit-number
		 since sign bit is farther away in the pre-truncated value.  */
	      return gen_rtx (ZERO_EXTRACT, GET_MODE (x),
			      XEXP (to, 0),
			      XEXP (x, 1),
			      gen_rtx (CONST_INT, VOIDmode,
				       (INTVAL (XEXP (x, 2))
					+ GET_MODE_BITSIZE (GET_MODE (XEXP (to, 0)))
					- GET_MODE_BITSIZE (GET_MODE (to)))));
	    }
#else
	  SUBST (XEXP (x, 0), XEXP (to, 0));
#endif
	}
      /* Extracting a single bit from the result of a shift:
	 see which bit it was before the shift and extract that directly.  */
      if (was_replaced[0]
	  && (GET_CODE (to) == ASHIFTRT || GET_CODE (to) == LSHIFTRT
	      || GET_CODE (to) == ASHIFT || GET_CODE (to) == LSHIFT)
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && XEXP (x, 1) == const1_rtx
	  && GET_CODE (XEXP (x, 2)) == CONST_INT)
	{
	  int shift = INTVAL (XEXP (to, 1));
	  int newpos;
	  if (GET_CODE (to) == ASHIFT || GET_CODE (to) == LSHIFT)
	    shift = - shift;
#ifdef BITS_BIG_ENDIAN
	  shift = - shift;
#endif
	  newpos = INTVAL (XEXP (x, 2)) + shift;
	  if (newpos >= 0 &&
	      newpos < GET_MODE_BITSIZE (GET_MODE (to)))
	    {
	      if (!undobuf.storage)
		undobuf.storage = (char *) oballoc (0);
	      return gen_rtx (ZERO_EXTRACT, GET_MODE (x),
			      XEXP (to, 0), const1_rtx,
			      gen_rtx (CONST_INT, VOIDmode, newpos));
	    }
	}
      break;

    case LSHIFTRT:
    case ASHIFTRT:
    case ROTATE:
    case ROTATERT:
#ifdef SHIFT_COUNT_TRUNCATED
      /* (lshift <X> (sign_extend <Y>)) = (lshift <X> <Y>) (most machines).
	 True for all kinds of shifts and also for zero_extend.  */
      if (was_replaced[1]
	  && (GET_CODE (to) == SIGN_EXTEND
	      || GET_CODE (to) == ZERO_EXTEND)
	  && FAKE_EXTEND_SAFE_P (GET_MODE (to), XEXP (to, 0)))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  SUBST (XEXP (x, 1),
		 /* This is a perverse SUBREG, wider than its base.  */
		 gen_lowpart_for_combine (GET_MODE (to), XEXP (to, 0)));
	}
#endif
      /* Two shifts in a row of same kind
	 in same direction with constant counts
	 may be combined.  */
      if (was_replaced[0]
	  && GET_CODE (to) == GET_CODE (x)
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && INTVAL (XEXP (to, 1)) > 0
	  && INTVAL (XEXP (x, 1)) > 0
	  && (INTVAL (XEXP (x, 1)) + INTVAL (XEXP (to, 1))
	      < GET_MODE_BITSIZE (GET_MODE (x))))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (GET_CODE (x), GET_MODE (x),
			  XEXP (to, 0),
			  gen_rtx (CONST_INT, VOIDmode,
				   INTVAL (XEXP (x, 1))
				   + INTVAL (XEXP (to, 1))));
	}
      break;

    case LSHIFT:
    case ASHIFT:
#ifdef SHIFT_COUNT_TRUNCATED
      /* (lshift <X> (sign_extend <Y>)) = (lshift <X> <Y>) (most machines).
	 True for all kinds of shifts and also for zero_extend.  */
      if (was_replaced[1]
	  && (GET_CODE (to) == SIGN_EXTEND
	      || GET_CODE (to) == ZERO_EXTEND)
	  && FAKE_EXTEND_SAFE_P (GET_MODE (to), XEXP (to, 0)))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  SUBST (XEXP (x, 1),
		 /* This is a perverse SUBREG, wider than its base.  */
		 gen_lowpart_for_combine (GET_MODE (to), XEXP (to, 0)));
	}
#endif
      /* (lshift (and (lshiftrt <foo> <X>) <Y>) <X>)
	 happens copying between bit fields in similar structures.
	 It can be replaced by one and instruction.
	 It does not matter whether the shifts are logical or arithmetic.  */
      if (GET_CODE (XEXP (x, 0)) == AND
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) > 0
	  && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
	  && XEXP (XEXP (x, 0), 0) == to
	  && (GET_CODE (to) == LSHIFTRT
	      || GET_CODE (to) == ASHIFTRT)
#if 0
/* I now believe this restriction is unnecessary.
   The outer shift will discard those bits in any case, right?  */

	      /* If inner shift is arithmetic, either it shifts left or
		 the bits it shifts the sign into are zeroed by the and.  */
		  && (INTVAL (XEXP (x, 1)) < 0
		      || ((unsigned) INTVAL (XEXP (XEXP (x, 0), 1))
			  < 1 << (GET_MODE_BITSIZE (GET_MODE (x))
				  - INTVAL (XEXP (x, 0)))))
#endif
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) == INTVAL (XEXP (to, 1)))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  /* The constant in the new `and' is <Y> << <X>
	     but clear out all bits that don't belong in our mode.  */
	  return gen_rtx (AND, GET_MODE (x), XEXP (to, 0),
			  gen_rtx (CONST_INT, VOIDmode,
				   (GET_MODE_MASK (GET_MODE (x))
				    & ((GET_MODE_MASK (GET_MODE (x))
					& INTVAL (XEXP (XEXP (x, 0), 1)))
				       << INTVAL (XEXP (x, 1))))));
	} 
      /* Two shifts in a row in same direction with constant counts
	 may be combined.  */
      if (was_replaced[0]
	  && (GET_CODE (to) == ASHIFT || GET_CODE (to) == LSHIFT)
	  && GET_CODE (XEXP (x, 1)) == CONST_INT
	  && GET_CODE (XEXP (to, 1)) == CONST_INT
	  && INTVAL (XEXP (to, 1)) > 0
	  && INTVAL (XEXP (x, 1)) > 0
	  && (INTVAL (XEXP (x, 1)) + INTVAL (XEXP (to, 1))
	      < GET_MODE_BITSIZE (GET_MODE (x))))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  return gen_rtx (GET_CODE (x), GET_MODE (x),
			  XEXP (to, 0),
			  gen_rtx (CONST_INT, VOIDmode,
				   INTVAL (XEXP (x, 1))
				   + INTVAL (XEXP (to, 1))));
	}
      /* (ashift (ashiftrt <foo> <X>) <X>)
	 (or, on some machines, (ashift (ashift <foo> <-X>) <X>) instead)
	 happens if you divide by 2**N and then multiply by 2**N.
	 It can be replaced by one `and' instruction.
	 It does not matter whether the shifts are logical or arithmetic.  */
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) > 0
	  && was_replaced[0]
	  && (((GET_CODE (to) == LSHIFTRT || GET_CODE (to) == ASHIFTRT)
	       && GET_CODE (XEXP (to, 1)) == CONST_INT
	       && INTVAL (XEXP (x, 1)) == INTVAL (XEXP (to, 1)))
	      ||
	      ((GET_CODE (to) == LSHIFT || GET_CODE (to) == ASHIFT)
	       && GET_CODE (XEXP (to, 1)) == CONST_INT
	       && INTVAL (XEXP (x, 1)) == - INTVAL (XEXP (to, 1)))))
	{
	  if (!undobuf.storage)
	    undobuf.storage = (char *) oballoc (0);
	  /* The constant in the new `and' is -1 << <X>
	     but clear out all bits that don't belong in our mode.  */
	  return gen_rtx (AND, GET_MODE (x), XEXP (to, 0),
			  gen_rtx (CONST_INT, VOIDmode,
				   (GET_MODE_MASK (GET_MODE (x))
				    & (GET_MODE_MASK (GET_MODE (x))
				       << INTVAL (XEXP (x, 1))))));
	} 

    }

  return x;
}

/* This is the AND case of the function subst.  */

static rtx
simplify_and_const_int (x, to)
     rtx x, to;
{
  register rtx varop = XEXP (x, 0);
  register int constop = INTVAL (XEXP (x, 1));

  /* (and (subreg (and <foo> <constant>) 0) <constant>)
     results from an andsi followed by an andqi,
     which happens frequently when storing bit-fields
     on something whose result comes from an andsi.  */
  if (GET_CODE (varop) == SUBREG
      && XEXP (varop, 0) == to
      && subreg_lowpart_p (varop)
      && GET_CODE (to) == AND
      && GET_CODE (XEXP (to, 1)) == CONST_INT
      /* Verify that the result of the outer `and'
	 is not affected by any bits not defined in the inner `and'.
	 True if the outer mode is narrower, or if the outer constant
	 masks to zero all the bits that the inner mode doesn't have.  */
      && (GET_MODE_SIZE (GET_MODE (x)) < GET_MODE_SIZE (GET_MODE (to))
	  || (constop & ~ GET_MODE_MASK (GET_MODE (to))) == 0))
    {
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);
      return gen_rtx (AND, GET_MODE (x),
		      gen_lowpart_for_combine (GET_MODE (x), XEXP (to, 0)),
		      gen_rtx (CONST_INT, VOIDmode,
			       constop
			       /* Remember that the bits outside that mode
				  are not being changed, so the effect
				  is as if they were all 1.  */
			       & INTVAL (XEXP (to, 1))));
    } 
  /* (and:SI (zero_extract:SI ...) <constant>)
     results from an andsi following a byte-fetch on risc machines.
     When the constant includes all bits extracted, eliminate the `and'.  */
  if (GET_CODE (varop) == ZERO_EXTRACT
      && GET_CODE (XEXP (varop, 1)) == CONST_INT
      /* The `and' must not clear any bits that the extract can give.  */
      && (~ constop & ((1 << INTVAL (XEXP (varop, 1))) - 1)) == 0)
    return varop;
  /* (and (zero_extend <foo>) <constant>)
     often results from storing in a bit-field something
     that was calculated as a short.  Replace with a single `and'
     in whose constant all bits not in <foo>'s mode are zero.  */
  if (varop == to
      && GET_CODE (to) == ZERO_EXTEND
      && FAKE_EXTEND_SAFE_P (GET_MODE (x), XEXP (to, 0)))
    {
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);
      return gen_rtx (AND, GET_MODE (x),
		      /* This is a perverse SUBREG, wider than its base.  */
		      gen_lowpart_for_combine (GET_MODE (x), XEXP (to, 0)),
		      gen_rtx (CONST_INT, VOIDmode,
			       constop & GET_MODE_MASK (GET_MODE (XEXP (to, 0)))));
    }
  /* (and (sign_extend <foo>) <constant>)
     can be replaced with (and (subreg <foo>) <constant>)
     if <constant> is narrower than <foo>'s mode,
     or with (zero_extend <foo>) if <constant> is a mask for that mode.  */
  if (varop == to
      && GET_CODE (to) == SIGN_EXTEND
      && ((unsigned) constop <= GET_MODE_MASK (GET_MODE (XEXP (to, 0))))
      && FAKE_EXTEND_SAFE_P (GET_MODE (x), XEXP (to, 0)))
    {
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);
      if (constop == GET_MODE_MASK (GET_MODE (XEXP (to, 0))))
	return gen_rtx (ZERO_EXTEND, GET_MODE (x), XEXP (to, 0));
      return gen_rtx (AND, GET_MODE (x),
		      /* This is a perverse SUBREG, wider than its base.  */
		      gen_lowpart_for_combine (GET_MODE (x), XEXP (to, 0)),
		      XEXP (x, 1));
    }
  /* (and (and <foo> <constant>) <constant>)
     comes from two and instructions in a row.  */
  if (varop == to
      && GET_CODE (to) == AND
      && GET_CODE (XEXP (to, 1)) == CONST_INT)
    {
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);
      return gen_rtx (AND, GET_MODE (x),
		      XEXP (to, 0),
		      gen_rtx (CONST_INT, VOIDmode,
			       constop
			       & INTVAL (XEXP (to, 1))));
    }
  /* (and (ashiftrt (ashift FOO N) N) CONST)
     may be simplified to (and FOO CONST) if CONST masks off the bits
     changed by the two shifts.  */
  if (GET_CODE (varop) == ASHIFTRT
      && GET_CODE (XEXP (varop, 1)) == CONST_INT
      && XEXP (varop, 0) == to
      && GET_CODE (to) == ASHIFT
      && GET_CODE (XEXP (to, 1)) == CONST_INT
      && INTVAL (XEXP (varop, 1)) == INTVAL (XEXP (to, 1))
      && ((unsigned) constop >> INTVAL (XEXP (varop, 1))) == 0)
    {
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);
      /* If CONST is a mask for the low byte,
	 change this into a zero-extend instruction
	 from just the low byte of FOO.  */
      if (constop == GET_MODE_MASK (QImode))
	{
	  rtx temp = gen_lowpart_for_combine (QImode, XEXP (to, 0));
	  if (GET_CODE (temp) != CLOBBER)
	    return gen_rtx (ZERO_EXTEND, GET_MODE (x), temp);
	}
      return gen_rtx (AND, GET_MODE (x),
		      XEXP (to, 0), XEXP (x, 1));
    }
  /* (and (ashiftrt (zero_extend FOO) N) CONST)
     may be simplified to (and (ashiftrt (subreg FOO) N) CONST)
     if CONST masks off the bits changed by extension.  */
  if ((GET_CODE (varop) == ASHIFTRT || GET_CODE (varop) == LSHIFTRT)
      && GET_CODE (XEXP (varop, 1)) == CONST_INT
      && XEXP (varop, 0) == to
      && (GET_CODE (to) == ZERO_EXTEND || GET_CODE (to) == SIGN_EXTEND)
      /* Verify the and discards all the extended bits.  */
      && (((unsigned) constop << INTVAL (XEXP (varop, 1)))
	  >> GET_MODE_BITSIZE (GET_MODE (XEXP (to, 0)))) == 0
      && FAKE_EXTEND_SAFE_P (GET_MODE (x), XEXP (to, 0)))
    {
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);
      SUBST (XEXP (varop, 0),
	     gen_lowpart_for_combine (GET_MODE (x), XEXP (to, 0)));
      return x;
    }
  /* (and x const) may be converted to (zero_extend (subreg x 0)).  */
  if (constop == GET_MODE_MASK (QImode)
      && GET_CODE (varop) == REG)
    {
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);
      return gen_rtx (ZERO_EXTEND, GET_MODE (x),
		      gen_rtx (SUBREG, QImode, varop, 0));
    }
  if (constop == GET_MODE_MASK (HImode)
      && GET_CODE (varop) == REG)
    {
      if (!undobuf.storage)
	undobuf.storage = (char *) oballoc (0);
      return gen_rtx (ZERO_EXTEND, GET_MODE (x),
		      gen_rtx (SUBREG, HImode, varop, 0));
    }
  /* No simplification applies.  */
  return 0;
}

/* Like gen_lowpart but for use by combine.  In combine it is not possible
   to create any new pseudoregs.  However, it is safe to create
   invalid memory addresses, because combine will try to recognize
   them and all they will do is make the combine attempt fail.

   If for some reason this cannot do its job, an rtx
   (clobber (const_int 0)) is returned.
   An insn containing that will not be recognized.  */

#undef gen_lowpart

static rtx
gen_lowpart_for_combine (mode, x)
     enum machine_mode mode;
     register rtx x;
{
  if (GET_CODE (x) == SUBREG || GET_CODE (x) == REG)
    return gen_lowpart (mode, x);
  if (GET_MODE (x) == mode)
    return gen_rtx (CLOBBER, VOIDmode, const0_rtx);
  if (GET_CODE (x) == MEM)
    {
      register int offset = 0;

      /* Refuse to work on a volatile memory ref.  */
      if (MEM_VOLATILE_P (x))
	return gen_rtx (CLOBBER, VOIDmode, const0_rtx);

      /* If we want to refer to something bigger than the original memref,
	 generate a perverse subreg instead.  That will force a reload
	 of the original memref X.  */
      if (GET_MODE_SIZE (GET_MODE (x)) < GET_MODE_SIZE (mode))
	return gen_rtx (SUBREG, mode, x, 0);

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
      return gen_rtx (MEM, mode, plus_constant (XEXP (x, 0),
						offset));
    }
  else
    return gen_rtx (CLOBBER, VOIDmode, const0_rtx);
}

/* After substitution, if the resulting pattern looks like
   (set (cc0) (and ...)) or (set (cc0) (lshiftrt ...)),
   this function is called to simplify the
   pattern into a bit-field operation if possible.  */

static void
simplify_set_cc0_and (insn)
     rtx insn;
{
  register rtx value = XEXP (PATTERN (insn), 1);
  register rtx op0 = XEXP (value, 0);
  register rtx op1 = XEXP (value, 1);
  int offset = 0;
  rtx var = 0;
  rtx bitnum = 0;
  int temp;
  int unit;
  rtx newpat;

  if (GET_CODE (value) == AND)
    {
      op0 = XEXP (value, 0);
      op1 = XEXP (value, 1);
    }
  else if (GET_CODE (value) == LSHIFTRT)
    {
      /* If there is no AND, but there is a shift that discards
	 all but the sign bit, we can pretend that the shift result
	 is ANDed with 1.  Otherwise we cannot handle just a shift.  */
      if (GET_CODE (XEXP (value, 1)) == CONST_INT
	  && (INTVAL (XEXP (value, 1))
	      == GET_MODE_BITSIZE (GET_MODE (value)) - 1))
	{
	  op0 = value;
	  op1 = const1_rtx;
	}
      else
	return;
    }
  else
    abort ();

  /* Look for a constant power of 2 or a shifted 1
     on either side of the AND.  Set VAR to the other side.
     Set BITNUM to the shift count of the 1 (as an rtx).
     Or, if bit number is constant, set OFFSET to the bit number.  */

  switch (GET_CODE (op0))
    {
    case CONST_INT:
      temp = exact_log2 (INTVAL (op0));
      if (temp < 0)
	return;
      offset = temp;
      var = op1;
      break;

    case ASHIFT:
    case LSHIFT:
      if (XEXP (op0, 0) == const1_rtx)
	{
	  bitnum = XEXP (op0, 1);
	  var = op1;
	}
    }
  if (var == 0)
    switch (GET_CODE (op1))
      {
      case CONST_INT:
	temp = exact_log2 (INTVAL (op1));
	if (temp < 0)
	  return;
	offset = temp;
	var = op0;
	break;

      case ASHIFT:
      case LSHIFT:
	if (XEXP (op1, 0) == const1_rtx)
	  {
	    bitnum = XEXP (op1, 1);
	    var = op0;
	  }
      }

  /* If VAR is 0, we didn't find something recognizable.  */
  if (var == 0)
    return;

  if (!undobuf.storage)
    undobuf.storage = (char *) oballoc (0);

  /* If the bit position is currently exactly 0,
     extract a right-shift from the variable portion.  */
  if (offset == 0
      && (GET_CODE (var) == ASHIFTRT || GET_CODE (var) == LSHIFTRT))
    {
      bitnum = XEXP (var, 1);
      var = XEXP (var, 0);
    }

  if (GET_CODE (var) == SUBREG && SUBREG_WORD (var) == 0)
    var = SUBREG_REG (var);

  /* Note that BITNUM and OFFSET are always little-endian thru here
     even on a big-endian machine.  */

#ifdef BITS_BIG_ENDIAN
  unit = GET_MODE_BITSIZE (GET_MODE (var)) - 1;

  if (bitnum != 0)
    bitnum = gen_rtx (MINUS, SImode,
		      gen_rtx (CONST_INT, VOIDmode, unit), bitnum);
  else
    offset = unit - offset;
#endif

  if (bitnum == 0)
    bitnum = gen_rtx (CONST_INT, VOIDmode, offset);

  newpat = gen_rtx (SET, VOIDmode, cc0_rtx,
		    gen_rtx (ZERO_EXTRACT, VOIDmode, var, const1_rtx, bitnum));
  if (recog (newpat, insn) >= 0)
    {
      if (undobuf.num_undo < MAX_UNDO)
	{
	  undobuf.undo[undobuf.num_undo].where = &XEXP (PATTERN (insn), 1);
	  undobuf.undo[undobuf.num_undo].old_contents = value;
	  XEXP (PATTERN (insn), 1) = XEXP (newpat, 1);
	}
      undobuf.num_undo++;
    }
}

/* Update the records of when each REG was most recently set or killed
   for the things done by INSN.  This is the last thing done in processing
   INSN in the combiner loop.

   We update reg_last_set, reg_last_death, and also the similar information
   mem_last_set (which insn most recently modified memory)
   and last_call_cuid (which insn was the most recent subroutine call).  */

static void
record_dead_and_set_regs (insn)
     rtx insn;
{
  register rtx link;
  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    {
      if (REG_NOTE_KIND (link) == REG_DEAD)
	reg_last_death[REGNO (XEXP (link, 0))] = insn;
      else if (REG_NOTE_KIND (link) == REG_INC)
	reg_last_set[REGNO (XEXP (link, 0))] = insn;
    }

  if (GET_CODE (insn) == CALL_INSN)
    last_call_cuid = mem_last_set = INSN_CUID (insn);

  if (GET_CODE (PATTERN (insn)) == PARALLEL)
    {
      register int i;
      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
	{
	  register rtx elt = XVECEXP (PATTERN (insn), 0, i);
	  register enum rtx_code code = GET_CODE (elt);
	  if (code == SET || code == CLOBBER)
	    {
	      rtx dest = XEXP (elt, 0);
	      while (GET_CODE (dest) == SUBREG
		     || GET_CODE (dest) == STRICT_LOW_PART
		     || GET_CODE (dest) == SIGN_EXTRACT
		     || GET_CODE (dest) == ZERO_EXTRACT)
		dest = XEXP (dest, 0);
	      
	      if (GET_CODE (dest) == REG)
		reg_last_set[REGNO (dest)] = insn;
	      else if (GET_CODE (dest) == MEM)
		mem_last_set = INSN_CUID (insn);
	    }
	}
    }
  else if (GET_CODE (PATTERN (insn)) == SET
	   || GET_CODE (PATTERN (insn)) == CLOBBER)
    {
      register rtx dest = XEXP (PATTERN (insn), 0);

      while (GET_CODE (dest) == SUBREG
	     || GET_CODE (dest) == STRICT_LOW_PART
	     || GET_CODE (dest) == SIGN_EXTRACT
	     || GET_CODE (dest) == ZERO_EXTRACT)
	dest = XEXP (dest, 0);

      if (GET_CODE (dest) == REG)
	reg_last_set[REGNO (dest)] = insn;
      else if (GET_CODE (dest) == MEM)
	mem_last_set = INSN_CUID (insn);
    }
}

/* Return nonzero if expression X refers to a REG or to memory
   that is set in an instruction more recent than FROM_CUID.  */

static int
use_crosses_set_p (x, from_cuid)
     register rtx x;
     int from_cuid;
{
  register char *fmt;
  register int i;
  register enum rtx_code code = GET_CODE (x);

  if (code == REG)
    {
      register int regno = REGNO (x);
#ifdef PUSH_ROUNDING
      /* Don't allow uses of the stack pointer to be moved,
	 because we don't know whether the move crosses a push insn.  */
      if (regno == STACK_POINTER_REGNUM)
	return 1;
#endif
      return (reg_last_set[regno]
	      && INSN_CUID (reg_last_set[regno]) > from_cuid);
    }

  if (code == MEM && mem_last_set > from_cuid)
    return 1;

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (use_crosses_set_p (XVECEXP (x, i, j), from_cuid))
	      return 1;
	}
      else if (fmt[i] == 'e'
	       && use_crosses_set_p (XEXP (x, i), from_cuid))
	return 1;
    }
  return 0;
}

/* Return nonzero if reg REGNO is marked as dying in INSN.  */

int
regno_dead_p (regno, insn)
     int regno;
     rtx insn;
{
  register rtx link;

  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if ((REG_NOTE_KIND (link) == REG_DEAD
	 || REG_NOTE_KIND (link) == REG_INC)
	&& REGNO (XEXP (link, 0)) == regno)
      return 1;

  return 0;
}

/* Return nonzero if J is the first insn following I,
   not counting labels, line numbers, etc.
   We assume that J follows I.  */

static int
adjacent_insns_p (i, j)
     rtx i, j;
{
  register rtx insn;
  for (insn = NEXT_INSN (i); insn != j; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == INSN
	|| GET_CODE (insn) == CALL_INSN
	|| GET_CODE (insn) == JUMP_INSN)
      return 0;
  return 1;
}

/* Check that X is an insn-body for an `asm' with operands
   and that the operands mentioned in it are legitimate.  */

static int
check_asm_operands (x)
     rtx x;
{
  int noperands = asm_noperands (x);
  rtx *operands;
  int i;

  if (noperands < 0)
    return 0;
  if (noperands == 0)
    return 1;

  operands = (rtx *) alloca (noperands * sizeof (rtx));
  decode_asm_operands (x, operands, 0, 0, 0);

  for (i = 0; i < noperands; i++)
    if (!general_operand (operands[i], VOIDmode))
      return 0;

  return 1;
}

/* Concatenate the list of logical links of OINSN
   into INSN's list of logical links.
   Modifies OINSN destructively.

   If ALL_LINKS is nonzero, move all the links that OINSN has.
   Otherwise, move only those that point to insns that set regs
   that die in the insn OINSN.
   Other links are clobbered so that they are no longer effective.  */

static void
add_links (insn, oinsn, all_links)
     rtx insn, oinsn;
     int all_links;
{
  register rtx links = LOG_LINKS (oinsn);
  if (! all_links)
    {
      rtx tail;
      for (tail = links; tail; tail = XEXP (tail, 1))
	{
	  rtx target = XEXP (tail, 0);
	  if (GET_CODE (target) != INSN
	      || GET_CODE (PATTERN (target)) != SET
	      || GET_CODE (SET_DEST (PATTERN (target))) != REG
	      || ! dead_or_set_p (oinsn, SET_DEST (PATTERN (target))))
	    /* OINSN is going to become a NOTE 
	       so a link pointing there will have no effect.  */
	    XEXP (tail, 0) = oinsn;
	}
    }
  if (LOG_LINKS (insn) == 0)
    LOG_LINKS (insn) = links;
  else
    {
      register rtx next, prev = LOG_LINKS (insn);
      while (next = XEXP (prev, 1))
	prev = next;
      XEXP (prev, 1) = links;
    }
}
  
/* Delete any LOG_LINKS of INSN which point at OINSN.  */

static void
remove_links (insn, oinsn)
     rtx insn, oinsn;
{
  register rtx next = LOG_LINKS (insn), prev = 0;
  while (next)
    {
      if (XEXP (next, 0) == oinsn)
	{
	  if (prev)
	    XEXP (prev, 1) = XEXP (next, 1);
	  else
	    LOG_LINKS (insn) = XEXP (next, 1);
	}
      else
	prev = next;
      next = XEXP (next, 1);
    }
}

/* Concatenate the any elements of the list of reg-notes INCS
   which are of type REG_INC
   into INSN's list of reg-notes.  */

static void
add_incs (insn, incs)
     rtx insn, incs;
{
  register rtx tail;

  for (tail = incs; tail; tail = XEXP (tail, 1))
    if (REG_NOTE_KIND (tail) == REG_INC)
      REG_NOTES (insn)
	= gen_rtx (EXPR_LIST, REG_INC, XEXP (tail, 0), REG_NOTES (insn));
}

/* Remove register number REGNO from the dead registers list of INSN.  */

void
remove_death (regno, insn)
     int regno;
     rtx insn;
{
  register rtx link, next;
  while ((link = REG_NOTES (insn))
	 && REG_NOTE_KIND (link) == REG_DEAD
	 && REGNO (XEXP (link, 0)) == regno)
    REG_NOTES (insn) = XEXP (link, 1);

  if (link)
    while (next = XEXP (link, 1))
      {
	if (REG_NOTE_KIND (next) == REG_DEAD
	    && REGNO (XEXP (next, 0)) == regno)
	  XEXP (link, 1) = XEXP (next, 1);
	else
	  link = next;
      }
}

/* For each register (hardware or pseudo) used within expression X,
   if its death is in an instruction with cuid
   between FROM_CUID (inclusive) and TO_INSN (exclusive),
   mark it as dead in TO_INSN instead.

   This is done when X is being merged by combination into TO_INSN.  */

static void
move_deaths (x, from_cuid, to_insn)
     rtx x;
     int from_cuid;
     rtx to_insn;
{
  register char *fmt;
  register int len, i;
  register enum rtx_code code = GET_CODE (x);

  if (code == REG)
    {
      register rtx where_dead = reg_last_death[REGNO (x)];

      if (where_dead && INSN_CUID (where_dead) >= from_cuid
	  && INSN_CUID (where_dead) < INSN_CUID (to_insn))
	{
	  remove_death (REGNO (x), reg_last_death[REGNO (x)]);
	  if (! dead_or_set_p (to_insn, x))
	    REG_NOTES (to_insn)
	      = gen_rtx (EXPR_LIST, REG_DEAD, x, REG_NOTES (to_insn));
	}
      return;
    }

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    move_deaths (XVECEXP (x, i, j), from_cuid, to_insn);
	}
      else if (fmt[i] == 'e')
	move_deaths (XEXP (x, i), from_cuid, to_insn);
    }
}

/* Like move_deaths, but deaths are moving both forward
   (from FROM_CUID to TO_INSN), and backwards
   (from FROM_INSN to TO_INSN).  This is what happens
   when an insn is removed after applying the distributive law.  */

static void
move_deaths_2 (x, from_cuid, from_insn, to_insn)
     rtx x;
     int from_cuid;
     rtx from_insn, to_insn;
{
  register char *fmt;
  register int len, i;
  register enum rtx_code code = GET_CODE (x);

  if (code == REG)
    {
      register rtx where_dead = reg_last_death[REGNO (x)];

      if (where_dead && INSN_CUID (where_dead) >= from_cuid
	  && INSN_CUID (where_dead) < INSN_CUID (to_insn))
	{
	  remove_death (REGNO (x), reg_last_death[REGNO (x)]);
	  if (! dead_or_set_p (to_insn, x))
	    REG_NOTES (to_insn)
	      = gen_rtx (EXPR_LIST, REG_DEAD, x, REG_NOTES (to_insn));
	}
      /* Can't use where_dead for from_insn because it has
	 not been computed yet.  */
      else if (dead_or_set_p (from_insn, x))
	{
	  remove_death (REGNO (x), from_insn);
	  if (! dead_or_set_p (to_insn, x))
	    REG_NOTES (to_insn)
	      = gen_rtx (EXPR_LIST, REG_DEAD, x, REG_NOTES (to_insn));
	}
      return;
    }

  len = GET_RTX_LENGTH (code);
  fmt = GET_RTX_FORMAT (code);

  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    move_deaths_2 (XVECEXP (x, i, j), from_cuid, from_insn, to_insn);
	}
      else if (fmt[i] == 'e')
	move_deaths_2 (XEXP (x, i), from_cuid, from_insn, to_insn);
    }
}

/* The distrib combiner rewrites groups of insns so that optimizations
   can be more easily recognized.  The front-end does not know how to
   group certain kinds of operations for efficient execution, and the
   resulting code can be quite poor.  For example, on a machine without
   bitfield instructions, bitfield references look like

	(and (lshiftrt ... n) m)

   When combining two bitfield operations, such as with ||, this can
   yield code like

	(set z
	     (or (and (lshiftrt x n) 1)
		 (and (lshiftrt y n) 1)))

   which can be more efficiently executed as

	(set z
	     (lshiftrt (and (or x y)
			    (1 << m)) n))

   From there, the combiner attempts to rewrite the insns,
   keeping flow information accurate for later passes,
   and reducing the total number of insns executed.

   This function returns the point at which we should try
   looking for more simplifications.  This will be before
   INSN if the call succeeds.  We do not need to fear
   infinite loops, since this function is guaranteed to
   eliminate at least one (non-note) instruction if it returns
   successfully.  */

static rtx
try_distrib (insn, xprev1, xprev2)
     rtx insn, xprev1, xprev2;
{
  rtx pat = PATTERN (insn);
  rtx prev1, prev2, pat1, pat2, src1, src2;
  rtx to_prev, to_insn;
  enum rtx_code code;
  int insn_code_number, prev_code_number, regno;
  rtx new_insn_pat, new_prev_pat;

  distrib_attempts++;

  /* ??? Need to implement a test that PREV2 and PREV1
     are completely independent.  Right now their
     recognition ability is sufficiently limited that
     it should not be necessary, but better safe than sorry.  */

  /* Let PREV1 be the later of the two insns, and PREV2 the earlier.  */
  if (INSN_CUID (xprev1) > INSN_CUID (xprev2))
    {
      prev1 = xprev1;
      prev2 = xprev2;
    }
  else
    {
      prev1 = xprev2;
      prev2 = xprev1;
    }

  pat1 = PATTERN (prev1);
  pat2 = PATTERN (prev2);

  /* First, see if INSN, PREV1, and PREV2 have patterns we can expect
     to simplify.  */

  if (GET_CODE (pat) != SET
      || GET_CODE (pat1) != SET
      || GET_CODE (pat2) != SET)
    return 0;

  code = GET_CODE (SET_SRC (pat));
  src1 = SET_SRC (pat1);
  src2 = SET_SRC (pat2);

  if (GET_CODE (SET_DEST (pat1)) != REG
      || GET_CODE (SET_DEST (pat2)) != REG)
    return 0;

  switch (code)
    {
    default:
      return 0;

    case IOR:
    case AND:
    case XOR:
    case PLUS:
      ;
    }

  /* Insns PREV1 and PREV2 must provide the two operands of the arithmetic
     that is done in INSN.  */
  if (! ((XEXP (SET_SRC (pat), 0) == SET_DEST (pat1)
	  && XEXP (SET_SRC (pat), 1) == SET_DEST (pat2))
	 ||
	 (XEXP (SET_SRC (pat), 0) == SET_DEST (pat2)
	  && XEXP (SET_SRC (pat), 1) == SET_DEST (pat1))))
    return 0;

  /* They must not be used in any other way in INSN.
     In particular, they must not be used in a result memory address.  */
  if (reg_mentioned_p (SET_DEST (pat1), SET_DEST (pat))
      || reg_mentioned_p (SET_DEST (pat2), SET_DEST (pat)))
    return 0;

  /* Give up if the two operands' modes don't match.  */
  if (GET_MODE (src1) != GET_MODE (src2))
    return 0;

  /* PREV1 and PREV2 must compute the same operation.
     Actually, there are other cases that could be handled,
     but are not implemented.  For example:

     (set (reg:SI 94)
	  (and:SI (reg:SI 73)
		  (const_int 223)))

     (set (reg:SI 95)
	  (zero_extend:SI (subreg:QI (reg:SI 91) 0)))

     (set (reg:SI 96)
	  (ior:SI (reg:SI 94)
		  (reg:SI 95)))

     In this case, we know that because (reg:SI 94) has
     been anded with 223, there is no need to zero_extend
     (reg:SI 91), and we could eliminate (reg:SI 95).  */

  if (GET_CODE (src1) != GET_CODE (src2))
    return 0;

  /* The SETs in PREV1 and PREV2 do not need to be kept around.  */

  undobuf.num_undo = 0;
  undobuf.storage = 0;

  /* Substitute in the latest insn for the regs set by the earlier ones.  */
  subst_insn = insn;
  n_occurrences = 0;	/* `subst' counts here */

  switch (GET_CODE (src1))
    {
    /* case XOR:  Does not distribute through anything!  */
    case LSHIFTRT:
    case ASHIFTRT:
      /* Right-shift can't distribute through addition
	 since the round-off would happen differently.  */
    case AND:
    case IOR:
      /* Boolean ops don't distribute through addition.  */
      if (code == PLUS)
	return 0;
      goto do_distrib;

    case LSHIFT:
    case ASHIFT:
      /* Left shifts are multiplication; they distribute through
	 addition.  Also, since they work bitwise, they
	 distribute through boolean operations.  */
#ifdef NEGATIVE_SHIFT_COUNTS
      /* Negative count is really a right-shift.  */
      if (NEGATIVE_SHIFT_COUNTS
	  && code == PLUS
	  && !(GET_CODE (XEXP (src1, 1))
	       == CONST_INT && INTVAL (XEXP (src1, 1)) >= 0))
	return 0;
#endif
      goto do_distrib;

    case MULT:
      /* Multiplication distributes through addition only.  */
      if (code != PLUS)
	return 0;

    do_distrib:
      if (GET_CODE (XEXP (src1, 1)) != CONST_INT
	  || GET_CODE (XEXP (src2, 1)) != CONST_INT
	  || INTVAL (XEXP (src1, 1)) != INTVAL (XEXP (src2, 1)))
	return 0;

      /* Give up if we would move a use of a reg across an alteration.
	 Note this is unnecessarily conservative, since a problem really
	 happens only if this reg is set *between* PREV2 and PREV1
	 But this test is easier.  */
      if (use_crosses_set_p (XEXP (src2, 0), INSN_CUID (prev2)))
	return 0;

      /* Try changing (+ (* x c) (* y c)) to (* (+ x y) c).  */
      to_prev = gen_rtx (code, GET_MODE (src1),
			 XEXP (src1, 0), XEXP (src2, 0));
      to_insn = gen_rtx (GET_CODE (src1), GET_MODE (src1), SET_DEST (pat1), XEXP (src1, 1));
      break;

    case ZERO_EXTEND:
    case SIGN_EXTEND:
      /* Extension can't distribute through addition;
	 the carries could be changed.  */
      if (code == PLUS)
	return 0;
      {
	rtx inner1 = XEXP (src1, 0), inner2 = XEXP (src2, 0);
	int subreg_needed = 0;

	/* Try changing (& (extend x) (extend y)) to (extend (& x y)).  */
	/* But keep extend insns together with their subregs.  */
	if (GET_CODE (inner1) == SUBREG)
	  {
	    if (SUBREG_WORD (inner1) != 0)
	      return 0;
	    else
	      {
		subreg_needed = 1;
		inner1 = SUBREG_REG (inner1);
	      }
	  }

	if (GET_CODE (inner2) == SUBREG)
	  {
	    if (SUBREG_WORD (inner2) != 0)
	      return 0;
	    else
	      {
		subreg_needed = 1;
		inner2 = SUBREG_REG (inner2);
	      }
	  }

	/* Give up if we would move a use of a reg across an alteration.
	   Note this is unnecessarily conservative, since a problem really
	   happens only if this reg is set *between* PREV2 and PREV1
	   But this test is easier.  */
	if (use_crosses_set_p (inner2, INSN_CUID (prev2)))
	  return 0;

	to_prev = gen_rtx (code, GET_MODE (src1), inner1, inner2);
	to_insn = gen_rtx (GET_CODE (src1), GET_MODE (src1),
			   subreg_needed
			   ? gen_rtx (SUBREG, GET_MODE (XEXP (src1, 0)),
				      SET_DEST (pat1), 0)
			   : SET_DEST (pat1));
      }
      break;

    default:
      return 0;
    }

  /* Are the results of this "substitution" a valid instruction?  */

  new_insn_pat = subst (PATTERN (insn), SET_SRC (PATTERN (insn)), to_insn);
  distrib_merges_1++;

  insn_code_number = recog (new_insn_pat, insn);
  if (insn_code_number < 0)
    {
      undo_all ();
      return 0;
    }

  subst_insn = prev1;
  new_prev_pat = subst (pat1, src1, to_prev);
  distrib_merges_2++;

  prev_code_number = recog (new_prev_pat, prev1);
  if (prev_code_number < 0)
    {
      undo_all ();
      return 0;
    }

  /* Everything worked; install the new patterns.  */
  INSN_CODE (insn) = insn_code_number;
  PATTERN (insn) = new_insn_pat;

  INSN_CODE (prev1) = prev_code_number;
  PATTERN (prev1) = new_prev_pat;

  /* Need to change LOG_LINKS around...PREV1 now gets
     whatever flowed into PREV2.  PREV2 is going to
     become a NOTE, so we clear out its LOG_LINKS.  */
  remove_links (insn, prev2);
  add_links (prev1, prev2, adjacent_insns_p (prev2, prev1));

  /* Registers which died in PREV2 now die in PREV1.
     Also, registers born in PREV2 dying in INSN now die in PREV1.  */
  move_deaths_2 (src2, INSN_CUID (prev2), insn, prev1);

  regno = REGNO (SET_DEST (pat2));

  reg_n_sets[regno]--;
  if (reg_n_sets[regno] == 0
      && ! (basic_block_live_at_start[0][regno / HOST_BITS_PER_INT]
	    & (1 << (regno % HOST_BITS_PER_INT))))
    reg_n_refs[regno] = 0;
  remove_death (regno, insn);

  PUT_CODE (prev2, NOTE);
  NOTE_LINE_NUMBER (prev2) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (prev2) = 0;

  distrib_successes++;
  return prev1;
}

void
dump_combine_stats (file)
     FILE *file;
{
  fprintf
    (file,
     ";; Combiner statistics: %d attempts, %d substitutions (%d requiring new space),\n;; %d successes.\n\n",
     combine_attempts, combine_merges, combine_extras, combine_successes);
  fprintf
    (file,
     ";; Distributer statistics: %d attempts, %d:%d substitutions,\n;; %d successes.\n\n",
     distrib_attempts, distrib_merges_1,
     distrib_merges_2, distrib_successes);
}

void
dump_combine_total_stats (file)
     FILE *file;
{
  fprintf
    (file,
     "\n;; Combiner totals: %d attempts, %d substitutions (%d requiring new space),\n;; %d successes.\n",
     total_attempts, total_merges, total_extras, total_successes);
  fprintf
    (file,
     "\n;; Distributer totals: %d attempts, %d:%d substitutions,\n;; %d successes.\n",
     total_distrib_attempts, total_distrib_merges_1,
     total_distrib_merges_2, total_distrib_successes);
}
