/* Optimize jump instructions, for GNU compiler.
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


/* This is the jump-optimization pass of the compiler.
   It is run two or three times: once before cse, sometimes once after cse,
   and once after reload (before final).

   jump_optimize deletes unreachable code and labels that are not used.
   It also deletes jumps that jump to the following insn,
   and simplifies jumps around unconditional jumps and jumps
   to unconditional jumps.

   Each CODE_LABEL has a count of the times it is used
   stored in the LABEL_NUSES internal field, and each JUMP_INSN
   has one label that it refers to stored in the
   JUMP_LABEL internal field.  With this we can detect labels that
   become unused because of the deletion of all the jumps that
   formerly used them.  The JUMP_LABEL info is sometimes looked
   at by later passes.

   Optionally, cross-jumping can be done.  Currently it is done
   only the last time (when after reload and before final).
   In fact, the code for cross-jumping now assumes that register
   allocation has been done, since it uses `rtx_renumbered_equal_p'.

   Jump optimization is done after cse when cse's constant-propagation
   causes jumps to become unconditional or to be deleted.

   Unreachable loops are not detected here, because the labels
   have references and the insns appear reachable from the labels.
   find_basic_blocks in flow.c finds and deletes such loops.

   The subroutines delete_insn, redirect_jump, invert_jump, next_real_insn
   and prev_real_insn are used from other passes as well.  */

#include "config.h"
#include "rtl.h"
#include "flags.h"
#include "regs.h"

/* ??? Eventually must record somehow the labels used by jumps
   from nested functions.  */
/* Pre-record the next or previous real insn for each label?
   No, this pass is very fast anyway.  */
/* Condense consecutive labels?
   This would make life analysis faster, maybe.  */
/* Optimize jump y; x: ... y: jumpif... x?
   Don't know if it is worth bothering with.  */
/* Optimize two cases of conditional jump to conditional jump?
   This can never delete any instruction or make anything dead,
   or even change what is live at any point.
   So perhaps let combiner do it.  */

/* Vector indexed by uid.
   For each CODE_LABEL, index by its uid to get first unconditional jump
   that jumps to the label.
   For each JUMP_INSN, index by its uid to get the next unconditional jump
   that jumps to the same label.
   Element 0 is the start of a chain of all return insns.
   (It is safe to use element 0 because insn uid 0 is not used.  */

rtx *jump_chain;

rtx delete_insn ();
void redirect_jump ();
void invert_jump ();
rtx next_real_insn ();
rtx prev_real_insn ();
rtx next_label ();

static void mark_jump_label ();
static void delete_jump ();
static void squeeze_block_notes ();
void invert_exp ();
static void redirect_exp ();
static rtx follow_jumps ();
static int tension_vector_labels ();
static void find_cross_jump ();
static void do_cross_jump ();
static enum rtx_code reverse_condition ();
static int jump_back_p ();
int condjump_p ();

/* Delete no-op jumps and optimize jumps to jumps
   and jumps around jumps.
   Delete unused labels and unreachable code.
   If CROSS_JUMP is nonzero, detect matching code
   before a jump and its destination and unify them.
   If NOOP_MOVES is nonzero, also delete no-op move insns.

   If `optimize' is zero, don't change any code,
   just determine whether control drops off the end of the function.
   This case occurs when we have -W and not -O.
   It works because `delete_insn' checks the value of `optimize'
   and refrains from actually deleting when that is 0.  */

void
jump_optimize (f, cross_jump, noop_moves)
     rtx f;
{
  register rtx insn;
  int changed;
  int first = 1;
  int max_uid = 0;
  rtx last_insn;

  /* Initialize LABEL_NUSES and JUMP_LABEL fields.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CODE_LABEL)
	LABEL_NUSES (insn) = 0;
      if (GET_CODE (insn) == JUMP_INSN)
	JUMP_LABEL (insn) = 0;
      if (INSN_UID (insn) > max_uid)
	max_uid = INSN_UID (insn);
    }

  max_uid++;

  jump_chain = (rtx *) alloca (max_uid * sizeof (rtx));
  bzero (jump_chain, max_uid * sizeof (rtx));

  /* Delete insns following barriers, up to next label.  */

  for (insn = f; insn;)
    {
      if (GET_CODE (insn) == BARRIER)
	{
	  insn = NEXT_INSN (insn);
	  while (insn != 0 && GET_CODE (insn) != CODE_LABEL)
	    {
	      if (GET_CODE (insn) == NOTE
		  && NOTE_LINE_NUMBER (insn) != NOTE_INSN_FUNCTION_END)
		insn = NEXT_INSN (insn);
	      else
		insn = delete_insn (insn);
	    }
	  /* INSN is now the code_label.  */
	}
      else
	insn = NEXT_INSN (insn);
    }

  /* Mark the label each jump jumps to.
     Combine consecutive labels, and count uses of labels.

     For each label, make a chain (using `jump_chain')
     of all the *unconditional* jumps that jump to it;
     also make a chain of all returns.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == JUMP_INSN && ! INSN_DELETED_P (insn))
      {
	mark_jump_label (PATTERN (insn), insn, cross_jump);
	if (JUMP_LABEL (insn) != 0 && simplejump_p (insn))
	  {
	    jump_chain[INSN_UID (insn)]
	      = jump_chain[INSN_UID (JUMP_LABEL (insn))];
	    jump_chain[INSN_UID (JUMP_LABEL (insn))] = insn;
	  }
	if (GET_CODE (PATTERN (insn)) == RETURN)
	  {
	    jump_chain[INSN_UID (insn)] = jump_chain[0];
	    jump_chain[0] = insn;
	  }
      }

  /* Delete all labels already not referenced.
     Also find the last insn.  */

  last_insn = 0;
  for (insn = f; insn; )
    {
      if (GET_CODE (insn) == CODE_LABEL && LABEL_NUSES (insn) == 0)
	insn = delete_insn (insn);
      else
	{
	  last_insn = insn;
	  insn = NEXT_INSN (insn);
	}
    }

  if (!optimize)
    {
      /* See if there is still a NOTE_INSN_FUNCTION_END in this function.
	 If so record that this function can drop off the end.  */

      insn = last_insn;
      {
	int n_labels = 1;
	while (insn
	       /* One label can follow the end-note: the return label.  */
	       && ((GET_CODE (insn) == CODE_LABEL && n_labels-- > 0)
		   /* Ordinary insns can follow it if returning a structure.  */
		   || GET_CODE (insn) == INSN
		   /* If machine uses explicit RETURN insns, no epilogue,
		      then one of them follows the note.  */
		   || (GET_CODE (insn) == JUMP_INSN
		       && GET_CODE (PATTERN (insn)) == RETURN)
		   /* Other kinds of notes can follow also.  */
		   || (GET_CODE (insn) == NOTE
		       && NOTE_LINE_NUMBER (insn) != NOTE_INSN_FUNCTION_END)))
	  insn = PREV_INSN (insn);
      }

      if (insn && GET_CODE (insn) == NOTE
	  && NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_END
	  && ! INSN_DELETED_P (insn))
	{
	  extern int current_function_returns_null;
	  current_function_returns_null = 1;
	}
      /* Zero the "deleted" flag of all the "deleted" insns.  */
      for (insn = f; insn; insn = NEXT_INSN (insn))
	INSN_DELETED_P (insn) = 0;
      return;
    }

  if (noop_moves)
    for (insn = f; insn; )
      {
	register rtx next = NEXT_INSN (insn);

	if (GET_CODE (insn) == INSN)
	  {
	    register rtx body = PATTERN (insn);

#if 0 /* Keep these insns, since they are used for conditional branch
	 scheduling peepholes on the sparc.  */
#endif
	    /* Delete insns that existed just to advise flow-analysis.  */

	    if (GET_CODE (body) == USE
		|| GET_CODE (body) == CLOBBER)
	      delete_insn (insn);
	    else

	    /* Detect and delete no-op move instructions
	       resulting from not allocating a parameter in a register.  */

	      if (GET_CODE (body) == SET
		     && (SET_DEST (body) == SET_SRC (body)
			 || (GET_CODE (SET_DEST (body)) == MEM
			     && GET_CODE (SET_SRC (body)) == MEM
			     && rtx_equal_p (SET_SRC (body), SET_DEST (body))))
		     && ! (GET_CODE (SET_DEST (body)) == MEM
			   && MEM_VOLATILE_P (SET_DEST (body)))
		     && ! (GET_CODE (SET_SRC (body)) == MEM
			   && MEM_VOLATILE_P (SET_SRC (body))))
	      delete_insn (insn);

	    /* Detect and ignore no-op move instructions
	       resulting from smart or fortuitous register allocation.  */

	    else if (GET_CODE (body) == SET)
	      {
		int sreg = true_regnum (SET_SRC (body));
		int dreg = true_regnum (SET_DEST (body));

		if (sreg == dreg && sreg >= 0)
		  delete_insn (insn);
		else if (sreg >= 0 && dreg >= 0)
		  {
		    rtx tem = find_equiv_reg (0, insn, 0,
					      sreg, 0, dreg,
					      GET_MODE (SET_SRC (body)));
		    
#ifdef PRESERVE_DEATH_INFO_REGNO_P
		    /* Deleting insn could lose a death-note for SREG or DREG
		       so don't do it if final needs accurate death-notes.  */
		    if (! PRESERVE_DEATH_INFO_REGNO_P (sreg)
			&& ! PRESERVE_DEATH_INFO_REGNO_P (dreg))
#endif
		      if (tem != 0
			  && GET_MODE (tem) == GET_MODE (SET_DEST (body)))
			delete_insn (insn);
		  }
	      }
	  }
      insn = next;
    }

  /* Now iterate optimizing jumps until nothing changes over one pass.  */
  changed = 1;
  while (changed)
    {
      register rtx next;
      changed = 0;

      for (insn = f; insn; insn = next)
	{
#if 0
	  /* If NOT the first iteration, if this is the last jump pass
	     (just before final), do the special peephole optimizations.
	     Avoiding the first iteration gives ordinary jump opts
	     a chance to work before peephole opts.  */

	  if (noop_moves && !first && !flag_no_peephole)
	    if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN)
	      peephole (insn);
#endif

	  /* That could have deleted some insns after INSN, so check now
	     what the following insn is.  */

	  next = NEXT_INSN (insn);

	  /* Tension the labels in dispatch tables.  */

	  if (GET_CODE (insn) == JUMP_INSN)
	    {
	      if (GET_CODE (PATTERN (insn)) == ADDR_VEC)
		changed |= tension_vector_labels (PATTERN (insn), 0, noop_moves);
	      if (GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC)
		changed |= tension_vector_labels (PATTERN (insn), 1, noop_moves);
	    }

	  /* Don't allow dropping through into a dispatch table.
	     That means the dispatch insn itself was deleted,
	     so delete the table too.  */

	  if (GET_CODE (insn) == JUMP_INSN)
	    {
	      /* Note: the corresponding job for ADDR_VEC is done
		 in delete_insn.  */

	      /* A vector of offsets is unused if its label
		 is used only once (i.e., from the vector).  */
	      if (GET_CODE (PATTERN (insn)) == ADDR_DIFF_VEC
		  && LABEL_NUSES (XEXP (XEXP (PATTERN (insn), 0), 0)) == 1)
		{
		  /* So delete both label and vector.  */
		  delete_insn (PREV_INSN (insn));
		  delete_insn (insn);
		  changed = 1;
		}
	    }

	  if (GET_CODE (insn) == JUMP_INSN && JUMP_LABEL (insn))
	    {
	      register rtx reallabelprev = prev_real_insn (JUMP_LABEL (insn));
	      rtx temp;

	      /* Detect jump to following insn.  */
	      if (reallabelprev == insn && condjump_p (insn))
		{
		  delete_jump (insn);
		  changed = 1;
		}
	      /* Detect worthless conditional jump.  */
	      else if ((temp = next_real_insn (insn))
		       && GET_CODE (temp) == JUMP_INSN
		       && condjump_p (insn)
		       && simplejump_p (temp)
		       && JUMP_LABEL (insn) == JUMP_LABEL (temp))
		{
		  delete_jump (insn);
		  changed = 1;
		  next = NEXT_INSN (insn);
		}
	      /* A jump to a return becomes a return.  */
	      else if (simplejump_p (insn)
		       && (temp = next_real_insn (JUMP_LABEL (insn))) != 0
		       && GET_CODE (PATTERN (temp)) == RETURN)
		{
		  PATTERN (insn) = PATTERN (temp);
		  /* Re-recognize this insn.  */
		  INSN_CODE (insn) = -1;
		}
	      /* Detect jumping over an unconditional jump.  */
	      else if (reallabelprev != 0
		       && GET_CODE (reallabelprev) == JUMP_INSN
		       && prev_real_insn (reallabelprev) == insn
		       && no_labels_between_p (insn, reallabelprev)
		       && simplejump_p (reallabelprev)
		       /* Ignore this if INSN is a hairy kind of jump,
			  since they may not be invertible.
			  This is conservative; could instead construct
			  the inverted insn and try recognizing it.  */
		       && condjump_p (insn))
		{
		  /* Delete the original unconditional jump (and barrier).  */
		  /* But don't let its destination go with it.  */
		  ++LABEL_NUSES (JUMP_LABEL (reallabelprev));
		  delete_insn (reallabelprev);
		  /* Now change the condition, and make it go to the
		     place the deleted jump went to.
		     This may cause the label after the deletion to go away.
		     But now that the unconditional jump and its barrier
		     are gone, that is ok.  */
		  invert_jump (insn, JUMP_LABEL (reallabelprev));
		  --LABEL_NUSES (JUMP_LABEL (reallabelprev));
		  next = insn;
		  changed = 1;
		}
	      else
		{
		  /* Detect a jump to a jump.  */
		  {
		    register rtx nlabel
		      = follow_jumps (JUMP_LABEL (insn), noop_moves);
		    if (nlabel != JUMP_LABEL (insn))
		      {
			redirect_jump (insn, nlabel);
			changed = 1;
			next = insn;
		      }
		  }

		  /* Look for   if (foo) bar; else break;  */
		  /* The insns look like this:
		     insn = condjump label1;
		        ...range1 (some insns)...
			jump label2;
		     label1:
		        ...range2 (some insns)...
			jump somewhere unconditionally
		     label2:  */
		  {
		    rtx label1 = next_label (insn);
		    rtx range1end = label1 ? prev_real_insn (label1) : 0;
		    /* Don't do this optimization on the first round, so that
		       jump-around-a-jump gets simplified before we ask here
		       whether a jump is unconditional.  */
		    if (! first
			/* Make sure INSN is something we can invert.  */
			&& condjump_p (insn)
			&& JUMP_LABEL (insn) == label1
			&& LABEL_NUSES (label1) == 1
			&& GET_CODE (range1end) == JUMP_INSN
			&& simplejump_p (range1end))
		      {
			rtx label2 = next_label (label1);
			rtx range2end = label2 ? prev_real_insn (label2) : 0;
			if (range1end != range2end
			    && JUMP_LABEL (range1end) == label2
			    && GET_CODE (range2end) == JUMP_INSN
			    && GET_CODE (NEXT_INSN (range2end)) == BARRIER)
			  {
			    rtx range1beg = next_real_insn (insn);
			    rtx range2beg = next_real_insn (label1);
			    rtx range1after, range2after;
			    rtx range1before, range2before;

			    /* Don't move NOTEs for blocks; shift them
			       outside the ranges, where they'll stay put.  */
			    squeeze_block_notes (range1beg, range1end);
			    squeeze_block_notes (range2beg, range2end);

			    /* Get current surrounds of the 2 ranges.  */
			    range1before = PREV_INSN (range1beg);
			    range2before = PREV_INSN (range2beg);
			    range1after = NEXT_INSN (range1end);
			    range2after = NEXT_INSN (range2end);

			    /* Splice range2 where range1 was.  */
			    NEXT_INSN (range1before) = range2beg;
			    PREV_INSN (range2beg) = range1before;
			    NEXT_INSN (range2end) = range1after;
			    PREV_INSN (range1after) = range2end;
			    /* Splice range1 where range2 was.  */
			    NEXT_INSN (range2before) = range1beg;
			    PREV_INSN (range1beg) = range2before;
			    NEXT_INSN (range1end) = range2after;
			    PREV_INSN (range2after) = range1end;
			    /* Invert the jump condition, so we
			       still execute the same insns in each case.  */
			    invert_jump (insn, label1);
			    changed = 1;
			    continue;
			  }
		      }
		  }

		  /* Now that the jump has been tensioned,
		     try cross jumping: check for identical code
		     before the jump and before its target label. */

		  /* First, cross jumping of conditional jumps:  */

		  if (cross_jump && condjump_p (insn))
		    {
		      rtx newjpos, newlpos;
		      rtx x = prev_real_insn (JUMP_LABEL (insn));

		      /* A conditional jump may be crossjumped
			 only if the place it jumps to follows
			 an opposing jump that comes back here.  */

		      if (x != 0 && ! jump_back_p (x, insn))
			/* We have no opposing jump;
			   cannot cross jump this insn.  */
			x = 0;

		      newjpos = 0;
		      /* TARGET is nonzero if it is ok to cross jump
			 to code before TARGET.  If so, see if matches.  */
		      if (x != 0)
			find_cross_jump (insn, x, 2,
					 &newjpos, &newlpos);

		      if (newjpos != 0)
			{
			  do_cross_jump (insn, newjpos, newlpos);
			  /* Make the old conditional jump
			     into an unconditional one.  */
			  SET_SRC (PATTERN (insn))
			    = gen_rtx (LABEL_REF, VOIDmode, JUMP_LABEL (insn));
			  emit_barrier_after (insn);
			  changed = 1;
			  next = insn;
			}
		    }

		  /* Cross jumping of unconditional jumps:
		     a few differences.  */

		  if (cross_jump && simplejump_p (insn))
		    {
		      rtx newjpos, newlpos;
		      rtx target;

		      newjpos = 0;

		      /* TARGET is nonzero if it is ok to cross jump
			 to code before TARGET.  If so, see if matches.  */
		      find_cross_jump (insn, JUMP_LABEL (insn), 1,
				       &newjpos, &newlpos);

		      /* If cannot cross jump to code before the label,
			 see if we can cross jump to another jump to
			 the same label.  */
		      /* Try each other jump to this label.  */
		      if (INSN_UID (JUMP_LABEL (insn)) < max_uid)
			for (target = jump_chain[INSN_UID (JUMP_LABEL (insn))];
			     target != 0 && newjpos == 0;
			     target = jump_chain[INSN_UID (target)])
			  if (target != insn
			      && JUMP_LABEL (target) == JUMP_LABEL (insn)
			      /* Ignore TARGET if it's deleted.  */
			      && ! INSN_DELETED_P (target))
			    find_cross_jump (insn, target, 2,
					     &newjpos, &newlpos);

		      if (newjpos != 0)
			{
			  do_cross_jump (insn, newjpos, newlpos);
			  changed = 1;
			  next = insn;
			}
		    }
		}
	    }
	  else if (GET_CODE (insn) == JUMP_INSN
		   && GET_CODE (PATTERN (insn)) == RETURN)
	    {
	      /* Return insns all "jump to the same place"
		 so we can cross-jump between any two of them.  */
	      if (cross_jump)
		{
		  rtx newjpos, newlpos, target;

		  newjpos = 0;

		  /* If cannot cross jump to code before the label,
		     see if we can cross jump to another jump to
		     the same label.  */
		  /* Try each other jump to this label.  */
		  for (target = jump_chain[0];
		       target != 0 && newjpos == 0;
		       target = jump_chain[INSN_UID (target)])
		    if (target != insn
			&& ! INSN_DELETED_P (target)
			&& GET_CODE (PATTERN (target)) == RETURN)
		      find_cross_jump (insn, target, 2,
				       &newjpos, &newlpos);

		  if (newjpos != 0)
		    {
		      do_cross_jump (insn, newjpos, newlpos);
		      changed = 1;
		      next = insn;
		    }
		}
	    }

	}

      first = 0;
    }

  /* See if there is still a NOTE_INSN_FUNCTION_END in this function.
     If so, delete it, and record that this function can drop off the end.  */

  insn = last_insn;
  {
    int n_labels = 1;
    while (insn
	   /* One label can follow the end-note: the return label.  */
	   && ((GET_CODE (insn) == CODE_LABEL && n_labels-- > 0)
	       /* Ordinary insns can follow it if returning a structure.  */
	       || GET_CODE (insn) == INSN
	       /* If machine uses explicit RETURN insns, no epilogue,
		  then one of them follows the note.  */
	       || (GET_CODE (insn) == JUMP_INSN
		   && GET_CODE (PATTERN (insn)) == RETURN)
	       /* Other kinds of notes can follow also.  */
	       || (GET_CODE (insn) == NOTE
		   && NOTE_LINE_NUMBER (insn) != NOTE_INSN_FUNCTION_END)))
      insn = PREV_INSN (insn);
  }
  if (insn && GET_CODE (insn) == NOTE
      && NOTE_LINE_NUMBER (insn) == NOTE_INSN_FUNCTION_END)
    {
      extern int current_function_returns_null;
      current_function_returns_null = 1;
      delete_insn (insn);
    }
}

/* Compare the instructions before insn E1 with those before E2.
   Assume E1 is a jump that jumps to label E2
   (that is not always true but it might as well be).
   Find the longest possible equivalent sequences
   and store the first insns of those sequences into *F1 and *F2.
   Store zero there if no equivalent preceding instructions are found.

   We give up if we find a label in stream 1.
   Actually we could transfer that label into stream 2.  */

static void
find_cross_jump (e1, e2, minimum, f1, f2)
     rtx e1, e2;
     int minimum;
     rtx *f1, *f2;
{
  register rtx i1 = e1, i2 = e2;
  register rtx p1, p2;

  rtx last1 = 0, last2 = 0;
  rtx afterlast1 = 0, afterlast2 = 0;

  *f1 = 0;
  *f2 = 0;

  while (1)
    {
      i1 = PREV_INSN (i1);
      while (i1 && GET_CODE (i1) == NOTE)
	i1 = PREV_INSN (i1);

      i2 = PREV_INSN (i2);
      while (i2 && (GET_CODE (i2) == NOTE || GET_CODE (i2) == CODE_LABEL))
	i2 = PREV_INSN (i2);

      if (i1 == 0)
	break;

      /* Don't allow the range of insns preceding E1 or E2
	 to include the other (E2 or E1).  */
      if (i2 == e1 || i1 == e2)
	break;

      /* If we will get to this code by jumping, those jumps will be
	 tensioned to go directly to the new label (before I2),
	 so this cross-jumping won't cost extra.  So reduce the minimum.  */
      if (GET_CODE (i1) == CODE_LABEL)
	{
	  --minimum;
	  break;
	}

      if (i2 == 0 || GET_CODE (i1) != GET_CODE (i2))
	break;

      p1 = PATTERN (i1);
      p2 = PATTERN (i2);
	
      if (GET_CODE (p1) != GET_CODE (p2)
	  || !rtx_renumbered_equal_p (p1, p2))
	{
	  /* Insns fail to match; cross jumping is limited to the following
	     insns.  */

	  /* Don't allow the insn after a compare to be shared by cross-jumping
	     unless the compare is also shared.
	     Here, if either of these non-matching insns is a compare,
	     exclude the following insn from possible cross-jumping.  */
	  if (sets_cc0_p (p1) || sets_cc0_p (p2))
	    last1 = afterlast1, last2 = afterlast2, ++minimum;

	  /* If cross-jumping here will feed a jump-around-jump optimization,
	     this jump won't cost extra, so reduce the minimum.  */
	  if (GET_CODE (i1) == JUMP_INSN
	      && JUMP_LABEL (i1)
	      && prev_real_insn (JUMP_LABEL (i1)) == e1)
	    --minimum;
	  break;
	}

      if (GET_CODE (p1) != USE && GET_CODE (p1) != CLOBBER)
	{
	  /* Ok, this insn is potentially includable in a cross-jump here.  */
	  afterlast1 = last1, afterlast2 = last2;
	  last1 = i1, last2 = i2, --minimum;
	}
    }

  if (minimum <= 0 && last1 != 0)
    *f1 = last1, *f2 = last2;
}

static void
do_cross_jump (insn, newjpos, newlpos)
     rtx insn, newjpos, newlpos;
{
  register rtx label;
  /* Find an existing label at this point
     or make a new one if there is none.  */
  label = PREV_INSN (newlpos);
  while (label && GET_CODE (label) == NOTE)
    label = PREV_INSN (label);

  if (label == 0 || GET_CODE (label) != CODE_LABEL)
    {
      label = gen_label_rtx ();
      emit_label_after (label, PREV_INSN (newlpos));
      LABEL_NUSES (label) = 0;
    }
  /* Make the same jump insn jump to the new point.  */
  if (GET_CODE (PATTERN (insn)) == RETURN)
    {
      extern rtx gen_jump ();
      PATTERN (insn) = gen_jump (label);
      INSN_CODE (insn) = -1;
      JUMP_LABEL (insn) = label;
      LABEL_NUSES (label)++;
    }
  else
    redirect_jump (insn, label);
  /* Delete the matching insns before the jump.  */
  newjpos = PREV_INSN (newjpos);
  while (NEXT_INSN (newjpos) != insn)
    /* Don't delete line numbers.  */
    if (GET_CODE (NEXT_INSN (newjpos)) != NOTE)
      delete_insn (NEXT_INSN (newjpos));
    else
      newjpos = NEXT_INSN (newjpos);
}

/* Move all block-beg and block-end notes between START and END
   out before START.  Assume neither START nor END is such a note.  */

static void
squeeze_block_notes (start, end)
     rtx start, end;
{
  rtx insn;
  rtx next;

  for (insn = start; insn != end; insn = next)
    {
      next = NEXT_INSN (insn);
      if (GET_CODE (insn) == NOTE
	  && (NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_END
	      || NOTE_LINE_NUMBER (insn) == NOTE_INSN_BLOCK_BEG))
	{
	  rtx prev = PREV_INSN (insn);
	  PREV_INSN (insn) = PREV_INSN (start);
	  NEXT_INSN (insn) = start;
	  NEXT_INSN (PREV_INSN (insn)) = insn;
	  PREV_INSN (NEXT_INSN (insn)) = insn;
	  NEXT_INSN (prev) = next;
	  PREV_INSN (next) = prev;
	}
    }
}

/* Return 1 if INSN is a jump that jumps to right after TARGET
   only on the condition that TARGET itself would drop through.
   Assumes that TARGET is a conditional jump.  */

static int
jump_back_p (insn, target)
     rtx insn, target;
{
  rtx cinsn, ctarget, prev;
  enum rtx_code codei, codet;

  if (simplejump_p (insn) || ! condjump_p (insn)
      || simplejump_p (target))
    return 0;
  if (target != prev_real_insn (JUMP_LABEL (insn)))
    return 0;

  /* Verify that the condition code was based on a fixed-point computation.
     Using reverse_condition is invalid for IEEE floating point with nans.  */
  prev = prev_real_insn (insn);
  if (! (prev != 0
	 && GET_CODE (prev) == INSN
	 && GET_CODE (PATTERN (prev)) == SET
	 && SET_DEST (PATTERN (prev)) == cc0_rtx
	 && (GET_MODE_CLASS (GET_MODE (SET_SRC (PATTERN (prev)))) == MODE_INT
	     || (GET_CODE (SET_SRC (PATTERN (prev))) == COMPARE
		 && (GET_MODE_CLASS (GET_MODE (XEXP (SET_SRC (PATTERN (prev)), 0)))
		     == MODE_INT)))))
    return 0;

  cinsn = XEXP (SET_SRC (PATTERN (insn)), 0);
  ctarget = XEXP (SET_SRC (PATTERN (target)), 0);

  codei = GET_CODE (cinsn);
  codet = GET_CODE (ctarget);
  if (XEXP (SET_SRC (PATTERN (insn)), 1) == pc_rtx)

    codei = reverse_condition (codei);
  if (XEXP (SET_SRC (PATTERN (target)), 2) == pc_rtx)
    codet = reverse_condition (codet);
  return (codei == codet
	  && rtx_renumbered_equal_p (XEXP (cinsn, 0), XEXP (ctarget, 0))
	  && rtx_renumbered_equal_p (XEXP (cinsn, 1), XEXP (ctarget, 1)));
}

/* Given an rtx-code for a comparison, return the code
   for the negated comparison.
   WATCH OUT!  reverse_condition is not safe to use on a jump
   that might be acting on the results of an IEEE floating point comparison,
   because of the special treatment of non-signaling nans in comparisons.  */

static enum rtx_code
reverse_condition (code)
     enum rtx_code code;
{
  switch (code)
    {
    case EQ:
      return NE;

    case NE:
      return EQ;

    case GT:
      return LE;

    case GE:
      return LT;

    case LT:
      return GE;

    case LE:
      return GT;

    case GTU:
      return LEU;

    case GEU:
      return LTU;

    case LTU:
      return GEU;

    case LEU:
      return GTU;

    default:
      abort ();
      return UNKNOWN;
    }
}

/* Return 1 if INSN is an unconditional jump and nothing else.  */

int
simplejump_p (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);
  if (GET_CODE (x) != SET)
    return 0;
  if (GET_CODE (SET_DEST (x)) != PC)
    return 0;
  if (GET_CODE (SET_SRC (x)) != LABEL_REF)
    return 0;
  return 1;
}

/* Return nonzero if INSN is a (possibly) conditional jump
   and nothing more.  */

int
condjump_p (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);
  if (GET_CODE (x) != SET)
    return 0;
  if (GET_CODE (SET_DEST (x)) != PC)
    return 0;
  if (GET_CODE (SET_SRC (x)) == LABEL_REF)
    return 1;
  if (GET_CODE (SET_SRC (x)) != IF_THEN_ELSE)
    return 0;
  if (XEXP (SET_SRC (x), 2) == pc_rtx
      && GET_CODE (XEXP (SET_SRC (x), 1)) == LABEL_REF)
    return 1;
  if (XEXP (SET_SRC (x), 1) == pc_rtx
      && GET_CODE (XEXP (SET_SRC (x), 2)) == LABEL_REF)
    return 1;
  return 0;
}

/* Return 1 if X is an RTX that does nothing but set the condition codes
   and CLOBBER or USE registers.
   Return -1 if X does explicitly set the condition codes,
   but also does other things.  */

int
sets_cc0_p (x)
     rtx x;
{
  if (GET_CODE (x) == SET && SET_DEST (x) == cc0_rtx)
    return 1;
  if (GET_CODE (x) == PARALLEL)
    {
      int i;
      int sets_cc0 = 0;
      int other_things = 0;
      for (i = XVECLEN (x, 0) - 1; i >= 0; i--)
	{
	  if (GET_CODE (XVECEXP (x, 0, i)) == SET
	      && SET_DEST (XVECEXP (x, 0, i)) == cc0_rtx)
	    sets_cc0 = 1;
	  else if (GET_CODE (XVECEXP (x, 0, i)) == SET)
	    other_things = 1;
	}
      return ! sets_cc0 ? 0 : other_things ? -1 : 1;
    }
  return 0;
}

/* Return 1 if in between BEG and END there is no CODE_LABEL insn.  */

int
no_labels_between_p (beg, end)
     rtx beg, end;
{
  register rtx p;
  for (p = beg; p != end; p = NEXT_INSN (p))
    if (GET_CODE (p) == CODE_LABEL)
      return 0;
  return 1;
}

/* Return the last INSN, CALL_INSN or JUMP_INSN before LABEL;
   or 0, if there is none.  */

rtx
prev_real_insn (label)
     rtx label;
{
  register rtx insn = PREV_INSN (label);
  register RTX_CODE code;

  while (1)
    {
      if (insn == 0)
	return 0;
      code = GET_CODE (insn);
      if (code == INSN || code == CALL_INSN || code == JUMP_INSN)
	break;
      insn = PREV_INSN (insn);
    }

  return insn;
}

/* Return the next INSN, CALL_INSN or JUMP_INSN after LABEL;
   or 0, if there is none.  */

rtx
next_real_insn (label)
     rtx label;
{
  register rtx insn = NEXT_INSN (label);
  register RTX_CODE code;

  while (1)
    {
      if (insn == 0)
	return insn;
      code = GET_CODE (insn);
      if (code == INSN || code == CALL_INSN || code == JUMP_INSN)
	break;
      insn = NEXT_INSN (insn);
    }

  return insn;
}

/* Return the next CODE_LABEL after the insn INSN, or 0 if there is none.  */

rtx
next_label (insn)
     rtx insn;
{
  do insn = NEXT_INSN (insn);
  while (insn != 0 && GET_CODE (insn) != CODE_LABEL);
  return insn;
}

/* Follow any unconditional jump at LABEL;
   return the ultimate label reached by any such chain of jumps.
   If LABEL is not followed by a jump, return LABEL.
   If IGNORE_LOOPS is 0, we do not chain across a NOTE_INSN_LOOP_BEG.  */

static rtx
follow_jumps (label, ignore_loops)
     rtx label;
     int ignore_loops;
{
  register rtx insn;
  register rtx next;
  register rtx value = label;
  register int depth;

  for (depth = 0;
       (depth < 10
	&& (insn = next_real_insn (value)) != 0
	&& GET_CODE (insn) == JUMP_INSN
	&& JUMP_LABEL (insn) != 0
	&& (next = NEXT_INSN (insn))
	&& GET_CODE (next) == BARRIER);
       depth++)
    {
      /* Don't chain through the insn that jumps into a loop
	 from outside the loop,
	 since that would create multiple loop entry jumps
	 and prevent loop optimization.  */
      rtx tem;
      if (!ignore_loops)
	for (tem = value; tem != insn; tem = NEXT_INSN (tem))
	  if (GET_CODE (tem) == NOTE
	      && NOTE_LINE_NUMBER (tem) == NOTE_INSN_LOOP_BEG)
	    return value;

      /* If we have found a cycle, make the insn jump to itself.  */
      if (JUMP_LABEL (insn) == label)
	break;
      value = JUMP_LABEL (insn);
    }
  return value;
}

/* Assuming that field IDX of X is a vector of label_refs,
   replace each of them by the ultimate label reached by it.
   Return nonzero if a change is made.
   If IGNORE_LOOPS is 0, we do not chain across a NOTE_INSN_LOOP_BEG.  */

static int
tension_vector_labels (x, idx, ignore_loops)
     register rtx x;
     register int idx;
     int ignore_loops;
{
  int changed = 0;
  register int i;
  for (i = XVECLEN (x, idx) - 1; i >= 0; i--)
    {
      register rtx olabel = XEXP (XVECEXP (x, idx, i), 0);
      register rtx nlabel = follow_jumps (olabel, ignore_loops);
      if (nlabel != olabel)
	{
	  XEXP (XVECEXP (x, idx, i), 0) = nlabel;
	  ++LABEL_NUSES (nlabel);
	  if (--LABEL_NUSES (olabel) == 0)
	    delete_insn (olabel);
	  changed = 1;
	}
    }
  return changed;
}

/* Find all CODE_LABELs referred to in X,
   and increment their use counts.
   Also store one of them in JUMP_LABEL (INSN) if INSN is nonzero.
   Also, when there are consecutive labels,
   canonicalize on the last of them.

   Note that two labels separated by a loop-beginning note
   must be kept distinct if we have not yet done loop-optimization,
   because the gap between them is where loop-optimize
   will want to move invariant code to.  CROSS_JUMP tells us
   that loop-optimization is done with.  */

static void
mark_jump_label (x, insn, cross_jump)
     register rtx x;
     rtx insn;
     int cross_jump;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      register rtx label = XEXP (x, 0);
      register rtx next;
      if (GET_CODE (label) != CODE_LABEL)
	return;
      /* If there are other labels following this one,
	 replace it with the last of the consecutive labels.  */
      for (next = NEXT_INSN (label); next; next = NEXT_INSN (next))
	{
	  if (GET_CODE (next) == CODE_LABEL)
	    label = next;
	  else if (GET_CODE (next) != NOTE
		   || NOTE_LINE_NUMBER (next) == NOTE_INSN_LOOP_BEG
		   || NOTE_LINE_NUMBER (next) == NOTE_INSN_FUNCTION_END)
	    break;
	}
      XEXP (x, 0) = label;
      ++LABEL_NUSES (label);
      if (insn)
	JUMP_LABEL (insn) = label;
      return;
    }

  /* Do walk the labels in a vector,
     but don't set its JUMP_LABEL.  */
  if (code == ADDR_VEC || code == ADDR_DIFF_VEC)
    insn = 0;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code); i >= 0; i--)
    {
      if (fmt[i] == 'e')
	mark_jump_label (XEXP (x, i), insn, cross_jump);
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    mark_jump_label (XVECEXP (x, i, j), insn, cross_jump);
	}
    }
}

/* If all INSN does is set the pc, delete it,
   and delete the insn that set the condition codes for it
   if that's what the previous thing was.  */

static void
delete_jump (insn)
     rtx insn;
{
  register rtx x = PATTERN (insn);
  register rtx prev;

  if (GET_CODE (x) == SET
      && GET_CODE (SET_DEST (x)) == PC)
    {
      prev = PREV_INSN (insn);
      delete_insn (insn);
      /* We assume that at this stage
	 CC's are always set explicitly
	 and always immediately before the jump that
	 will use them.  So if the previous insn
	 exists to set the CC's, delete it
	 (unless it performs auto-increments, etc.).  */
      while (prev && GET_CODE (prev) == NOTE)
	prev = PREV_INSN (prev);
      if (prev && GET_CODE (prev) == INSN
	  && sets_cc0_p (PATTERN (prev)) > 0
	  && !find_reg_note (prev, REG_INC, 0))
	delete_insn (prev);
    }
}

/* Delete insn INSN from the chain of insns and update label ref counts.
   May delete some following insns as a consequence; may even delete
   a label elsewhere and insns that follow it.

   Returns the first insn after INSN that was not deleted.  */

rtx
delete_insn (insn)
     register rtx insn;
{
  register rtx next = NEXT_INSN (insn);
  register rtx prev = PREV_INSN (insn);

  while (next && INSN_DELETED_P (next))
    next = NEXT_INSN (next);

  /* This insn is already deleted => return first following nondeleted.  */
  if (INSN_DELETED_P (insn))
    return next;

  /* Mark this insn as deleted.  */

  INSN_DELETED_P (insn) = 1;

  /* If instruction is followed by a barrier,
     delete the barrier too.  */

  if (next != 0 && GET_CODE (next) == BARRIER)
    {
      INSN_DELETED_P (next) = 1;
      next = NEXT_INSN (next);
    }

  /* Patch out INSN (and the barrier if any) */

  if (optimize)
    {
      if (prev)
	NEXT_INSN (prev) = next;

      if (next)
	PREV_INSN (next)= prev;

      if (prev && NEXT_INSN (prev) == 0)
	set_last_insn (prev);
    }

  /* If deleting a jump, decrement the count of the label,
     and delete the label if it is now unused.  */

  if (GET_CODE (insn) == JUMP_INSN && JUMP_LABEL (insn))
    if (--LABEL_NUSES (JUMP_LABEL (insn)) == 0)
      {
	/* This can delete NEXT or PREV,
	   either directly if NEXT is JUMP_LABEL (INSN),
	   or indirectly through more levels of jumps.  */
	delete_insn (JUMP_LABEL (insn));
	/* I feel a little doubtful about this loop,
	   but I see no clean and sure alternative way
	   to find the first insn after INSN that is not now deleted.
	   I hope this works.  */
	while (next && INSN_DELETED_P (next))
	  next = NEXT_INSN (next);
	return next;
      }

  while (prev && (INSN_DELETED_P (prev) || GET_CODE (prev) == NOTE))
    prev = PREV_INSN (prev);

  /* If INSN was a label and a dispatch table follows it,
     delete the dispatch table.  The tablejump must have gone already.
     It isn't useful to fall through into a table.  */

  if (GET_CODE (insn) == CODE_LABEL
      && NEXT_INSN (insn) != 0
      && GET_CODE (NEXT_INSN (insn)) == JUMP_INSN
      && GET_CODE (PATTERN (NEXT_INSN (insn))) == ADDR_VEC)
    next = delete_insn (NEXT_INSN (insn));

  /* If INSN was a label, delete insns following it if now unreachable.  */

  if (GET_CODE (insn) == CODE_LABEL && prev
      && GET_CODE (prev) == BARRIER)
    {
      register RTX_CODE code;
      while (next != 0
	     && ((code = GET_CODE (next)) == INSN
		 || code == JUMP_INSN || code == CALL_INSN
		 || code == NOTE))
	{
	  if (code == NOTE
	      && NOTE_LINE_NUMBER (next) != NOTE_INSN_FUNCTION_END)
	    next = NEXT_INSN (next);
	  else
	    /* Note: if this deletes a jump, it can cause more
	       deletion of unreachable code, after a different label.
	       As long as the value from this recursive call is correct,
	       this invocation functions correctly.  */
	    next = delete_insn (next);
	}
    }

  return next;
}

/* Advance from INSN till reaching something not deleted
   then return that.  May return INSN itself.  */

rtx
next_nondeleted_insn (insn)
     rtx insn;
{
  while (INSN_DELETED_P (insn))
    insn = NEXT_INSN (insn);
  return insn;
}

/* Delete a range of insns from FROM to TO, inclusive.
   This is for the sake of peephole optimization, so assume
   that whatever these insns do will still be done by a new
   peephole insn that will replace them.  */

void
delete_for_peephole (from, to)
     register rtx from, to;
{
  register rtx insn = from;

  while (1)
    {
      register rtx next = NEXT_INSN (insn);
      register rtx prev = PREV_INSN (insn);

      if (GET_CODE (insn) != NOTE)
	{
	  INSN_DELETED_P (insn) = 1;

	  /* Patch this insn out of the chain.  */
	  /* We don't do this all at once, because we
	     must preserve all NOTEs.  */
	  if (prev)
	    NEXT_INSN (prev) = next;

	  if (next)
	    PREV_INSN (next) = prev;
	}

      if (insn == to)
	break;
      insn = next;
    }

  /* Note that if TO is an unconditional jump
     we *do not* delete the BARRIER that follows,
     since the peephole that replaces this sequence
     is also an unconditional jump in that case.  */
}

/* Invert the condition of the jump JUMP, and make it jump
   to label NLABEL instead of where it jumps now.  */

void
invert_jump (jump, nlabel)
     rtx jump, nlabel;
{
  register rtx olabel = JUMP_LABEL (jump);
  invert_exp (PATTERN (jump), olabel, nlabel);
  JUMP_LABEL (jump) = nlabel;
  ++LABEL_NUSES (nlabel);
  INSN_CODE (jump) = -1;

  if (--LABEL_NUSES (olabel) == 0)
    delete_insn (olabel);
}

/* Invert the jump condition of rtx X,
   and replace OLABEL with NLABEL throughout.
   This is used in do_jump as well as in this file.  */

void
invert_exp (x, olabel, nlabel)
     rtx x;
     rtx olabel, nlabel;
{
  register RTX_CODE code;
  register int i;
  register char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);
  if (code == IF_THEN_ELSE)
    {
      /* Inverting the jump condition of an IF_THEN_ELSE
	 means exchanging the THEN-part with the ELSE-part.  */
      register rtx tem = XEXP (x, 1);
      XEXP (x, 1) = XEXP (x, 2);
      XEXP (x, 2) = tem;
    }

  if (code == LABEL_REF)
    {
      if (XEXP (x, 0) == olabel)
	XEXP (x, 0) = nlabel;
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	invert_exp (XEXP (x, i), olabel, nlabel);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    invert_exp (XVECEXP (x, i, j), olabel, nlabel);
	}
    }
}

/* Make jump JUMP jump to label NLABEL instead of where it jumps now.
   If the old jump target label is unused as a result,
   it and the code following it may be deleted.  */

void
redirect_jump (jump, nlabel)
     rtx jump, nlabel;
{
  register rtx olabel = JUMP_LABEL (jump);

  if (nlabel == olabel)
    return;

  redirect_exp (PATTERN (jump), olabel, nlabel);
  JUMP_LABEL (jump) = nlabel;
  ++LABEL_NUSES (nlabel);
  INSN_CODE (jump) = -1;

  if (--LABEL_NUSES (olabel) == 0)
    delete_insn (olabel);
}

/* Throughout the rtx X,
   alter (LABEL_REF OLABEL) to (LABEL_REF NLABEL).  */

static void
redirect_exp (x, olabel, nlabel)
     rtx x;
     rtx olabel, nlabel;
{
  register RTX_CODE code = GET_CODE (x);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      if (XEXP (x, 0) == olabel)
	XEXP (x, 0) = nlabel;
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	redirect_exp (XEXP (x, i), olabel, nlabel);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    redirect_exp (XVECEXP (x, i, j), olabel, nlabel);
	}
    }
}

/* Like rtx_equal_p except that it considers two REGs as equal
   if they renumber to the same value.  */

int
rtx_renumbered_equal_p (x, y)
     rtx x, y;
{
  register int i;
  register RTX_CODE code = GET_CODE (x);
  register char *fmt;
      
  if (x == y)
    return 1;
  if ((code == REG || (code == SUBREG && GET_CODE (SUBREG_REG (x)) == REG))
      && (GET_CODE (y) == REG || (GET_CODE (y) == SUBREG
				  && GET_CODE (SUBREG_REG (y)) == REG)))
    {
      register int j;

      if (GET_MODE (x) != GET_MODE (y))
	return 0;

      if (code == SUBREG)
	{
	  i = REGNO (SUBREG_REG (x));
	  if (reg_renumber[i] >= 0)
	    i = reg_renumber[i];
	  i += SUBREG_WORD (x);
	}
      else
	{
	  i = REGNO (x);
	  if (reg_renumber[i] >= 0)
	    i = reg_renumber[i];
	}
      if (GET_CODE (y) == SUBREG)
	{
	  j = REGNO (SUBREG_REG (y));
	  if (reg_renumber[j] >= 0)
	    j = reg_renumber[j];
	  j += SUBREG_WORD (y);
	}
      else
	{
	  j = REGNO (y);
	  if (reg_renumber[j] >= 0)
	    j = reg_renumber[j];
	}
      return i == j;
    }
  /* Now we have disposed of all the cases 
     in which different rtx codes can match.  */
  if (code != GET_CODE (y))
    return 0;
  switch (code)
    {
    case PC:
    case CC0:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      return 0;

    case CONST_INT:
      return XINT (x, 0) == XINT (y, 0);

    case LABEL_REF:
      /* Two label-refs are equivalent if they point at labels
	 in the same position in the instruction stream.  */
      return (next_real_insn (XEXP (x, 0))
	      == next_real_insn (XEXP (y, 0)));

    case SYMBOL_REF:
      return XSTR (x, 0) == XSTR (y, 0);
    }

  /* (MULT:SI x y) and (MULT:HI x y) are NOT equivalent.  */

  if (GET_MODE (x) != GET_MODE (y))
    return 0;

  /* Compare the elements.  If any pair of corresponding elements
     fail to match, return 0 for the whole things.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      register int j;
      switch (fmt[i])
	{
	case 'i':
	  if (XINT (x, i) != XINT (y, i))
	    return 0;
	  break;

	case 's':
	  if (strcmp (XSTR (x, i), XSTR (y, i)))
	    return 0;
	  break;

	case 'e':
	  if (! rtx_renumbered_equal_p (XEXP (x, i), XEXP (y, i)))
	    return 0;
	  break;

	case '0':
	  break;

	case 'E':
	  if (XVECLEN (x, i) != XVECLEN (y, i))
	    return 0;
	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (!rtx_renumbered_equal_p (XVECEXP (x, i, j), XVECEXP (y, i, j)))
	      return 0;
	  break;

	default:
	  abort ();
	}
    }
  return 1;
}

/* If X is a hard register or equivalent to one or a subregister of one,
   return the hard register number.  Otherwise, return -1.
   Any rtx is valid for X.  */

int
true_regnum (x)
     rtx x;
{
  if (GET_CODE (x) == REG)
    {
      if (REGNO (x) >= FIRST_PSEUDO_REGISTER)
	return reg_renumber[REGNO (x)];
      return REGNO (x);
    }
  if (GET_CODE (x) == SUBREG)
    {
      int base = true_regnum (SUBREG_REG (x));
      if (base >= 0 && base < FIRST_PSEUDO_REGISTER)
	return SUBREG_WORD (x) + base;
    }
  return -1;
}
