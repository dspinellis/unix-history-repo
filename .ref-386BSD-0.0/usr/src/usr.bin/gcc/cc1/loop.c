/* Move constant computations out of loops.
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


/* This is the loop optimization pass of the compiler.
   It finds invariant computations within loops and moves them
   to the beginning of the loop.  Then it identifies basic and 
   general induction variables.  Strength reduction is applied to the general
   induction variables, and induction variable elimination is applied to
   the basic induction variables.

   It also finds cases where
   a register is set within the loop by zero-extending a narrower value
   and changes these to zero the entire register once before the loop
   and merely copy the low part within the loop.

   Most of the complexity is in heuristics to decide when it is worth
   while to do these things.  */

/* ??? verify_loop would run faster if we made one table
   of the minimum and maximum luids from which each label is reached.
   Also, it would be faster if loop_store_addrs were a hash table.  */

#include "config.h"
#include "rtl.h"
#include "expr.h"
#include "insn-config.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "recog.h"
#include "flags.h"
#include <stdio.h>

/* Vector mapping INSN_UIDs to luids.
   The luids are like uids but increase monononically always.
   We use them to see whether a jump comes from outside a given loop.  */

static int *uid_luid;

/* Get the luid of an insn.  */

#define INSN_LUID(INSN) (uid_luid[INSN_UID (INSN)])

/* 1 + largest uid of any insn.  */

static int max_uid;

/* 1 + luid of last insn.  */

static int max_luid;

/* Nonzero if somewhere in the current loop
   there is either a subroutine call,
   or a store into a memory address that is not fixed,
   or a store in a BLKmode memory operand,
   or too many different fixed addresses stored in
   to record them all in `loop_store_addrs'.

   In any of these cases, no memory location can be regarded
   as invariant.  */

static int unknown_address_altered;

/* Nonzero if somewhere in the current loop there is a store
   into a memory address that is not fixed but is known to be
   part of an aggregate.

   In this case, no memory reference in an aggregate may be
   considered invariant.  */

static int unknown_aggregate_altered;

/* Nonzero if somewhere in the current loop there is a store
   into a memory address other than a fixed address not in an aggregate.

   In this case, no memory reference in an aggregate at a varying address
   may be considered invariant.  */

static int fixed_aggregate_altered;

/* Nonzero if there is a subroutine call in the current loop.
   (unknown_address_altered is also nonzero in this case.)  */

static int loop_has_call;

/* Added loop_continue which is the NOTE_INSN_LOOP_CONT of the
   current loop.  A continue statement will generate a branch to
   NEXT_INSN (loop_continue).  */

static rtx loop_continue;

/* Indexed by register number, contains the number of times the reg
   is set during the loop being scanned.
   During code motion, -1 indicates a reg that has been made a candidate.
   After code motion, regs moved have 0 (which is accurate now)
   while the failed candidates have the original number of times set.

   Therefore, at all times, 0 indicates an invariant register;
   -1 a conditionally invariant one.  */

static short *n_times_set;

/* Original value of n_times_set; same except that this value
   is not set to -1 for a reg whose sets have been made candidates
   and not set to 0 for a reg that is moved.  */

static short *n_times_used;

/* Index by register number, 1 indicates that the register
   cannot be moved or strength reduced.  */

static char *may_not_optimize;

/* Nonzero means reg N has already been moved out of one loop.
   This reduces the desire to move it out of another.  */

static char *moved_once;

/* Array of fixed memory addresses that are stored in this loop.
   If there are too many to fit here,
   we just turn on unknown_address_altered.  */

#define NUM_STORES 10
static rtx loop_store_addrs[NUM_STORES];
static int loop_store_widths[NUM_STORES];

/* Index of first available slot in above array.  */
static int loop_store_addrs_idx;

/* Count of movable (i.e. invariant) instructions discovered in the loop.  */
static int num_movables;

/* Count of memory write instructions discovered in the loop.  */
static int num_mem_sets;

/* Number of loops contained within the current one, including itself.  */
static int loops_enclosed;

/* Bound on pseudo register number before loop optimization.
   A pseudo has valid regscan info if its number is < old_max_reg.  */
static int old_max_reg;

/* During the analysis of a loop, a chain of `struct movable's
   is made to record all the movable insns found.
   Then the entire chain can be scanned to decide which to move.  */

struct movable
{
  rtx insn;			/* A movable insn */
  rtx set_src;                  /* The expression this reg is set from.
				   Either SET_SRC (body) or a REG_EQUAL.  */
  int consec;			/* Number of consecutive following insns 
				   that must be moved with this one.  */
  int regno;			/* The register it sets */
  short lifetime;		/* lifetime of that register;
				   may be adjusted when matching movables
				   that load the same value are found.  */
  short savings;		/* Number of insns we can move for this reg,
				   including other movables that force this
				   or match this one.  */
  unsigned int cond : 1;	/* 1 if only conditionally movable */
  unsigned int force : 1;	/* 1 means MUST move this insn */
  unsigned int global : 1;	/* 1 means reg is live outside this loop */
		/* If PARTIAL is 1, GLOBAL means something different:
		   that the reg is live outside the range from where it is set
		   to the following label.  */
  unsigned int done : 1;	/* 1 inhibits further processing of this */
  /* 1 in PARTIAL means this reg is used for zero-extending.
     In particular, moving it does not make it invariant.  */
  unsigned int partial : 1;
  enum machine_mode savemode;   /* Nonzero means it is a mode for a low part
				   that we should avoid changing when clearing
				   the rest of the reg.  */
  struct movable *match;	/* First entry for same value */
  struct movable *forces;	/* An insn that must be moved if this is */
  struct movable *next;
};

static FILE *loop_dump_stream;

/* Forward declarations.  */

struct induction;
struct iv_class;

static rtx loop_find_reg_equal ();
static int reg_in_basic_block_p ();
static rtx verify_loop ();
static int invariant_p ();
static int consec_sets_invariant_p ();
static int can_jump_into_range_p ();
static int labels_in_range_p ();
static void count_loop_regs_set ();
static void note_addr_stored ();
static int loop_reg_used_before_p ();
static void constant_high_bytes ();
static void scan_loop ();
static rtx replace_regs ();
static void replace_call_address ();
static rtx skip_consec_insns ();
static void ignore_some_movables ();
static void force_movables ();
static void combine_movables ();
static int rtx_equal_for_loop_p ();
static void move_movables ();
static void strength_reduce ();
static void find_mem_givs ();
static void record_giv ();
static void delete_insn_forces ();
static int basic_induction_var ();
static int general_induction_var ();
static int consec_sets_giv ();
static int check_dbra_loop ();
static void emit_iv_init_code ();
static int product_cheap_p ();
static void emit_iv_inc ();
static void check_eliminate_biv ();
static int can_eliminate_biv_p ();
static void eliminate_biv ();
static rtx final_biv_value ();
static int last_use_this_basic_block ();

/* Entry point of this file.  Perform loop optimization
   on the current function.  F is the first insn of the function
   and DUMPFILE is a stream for output of a trace of actions taken
   (or 0 if none should be output).  */

void
loop_optimize (f, dumpfile)
     /* f is the first instruction of a chain of insns for one function */
     rtx f;
     FILE *dumpfile;
{
  register rtx insn;
  register int i;
  rtx end;
  rtx last_insn;

  loop_dump_stream = dumpfile;

  init_recog ();

  old_max_reg = max_reg_num ();

  moved_once = (char *) alloca (old_max_reg);
  bzero (moved_once, old_max_reg);

  /* First find the last real insn, and count the number of insns,
     and assign insns their luids.  */

  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    if (INSN_UID (insn) > i)
      i = INSN_UID (insn);

  max_uid = i + 1;
  uid_luid = (int *) alloca ((i + 1) * sizeof (int));
  bzero (uid_luid, (i + 1) * sizeof (int));

  /* Compute the mapping from uids to luids.
     LUIDs are numbers assigned to insns, like uids,
     except that luids increase monotonically through the code.
     Don't assign luids to line-number NOTEs, so that the distance in luids
     between two insns is not affected by -g.  */

  for (insn = f, i = 0; insn; insn = NEXT_INSN (insn))
    {
      last_insn = insn;
      if (GET_CODE (insn) != NOTE
	  || NOTE_LINE_NUMBER (insn) < 0)
	INSN_LUID (insn) = ++i;
      else
	/* Give a line number note the same luid as preceding insn.  */
	INSN_LUID (insn) = i;
    }

  max_luid = i;

  /* Don't leave gaps in uid_luid for insns that have been
     deleted.  It is possible that the first or last insn
     using some register has been deleted by cross-jumping.
     Make sure that uid_luid for that former insn's uid
     points to the general area where that insn used to be.  */
  for (i = 0; i < max_uid; i++)
    {
      uid_luid[0] = uid_luid[i];
      if (uid_luid[0] != 0)
	break;
    }
  for (i = 0; i < max_uid; i++)
    if (uid_luid[i] == 0)
      uid_luid[i] = uid_luid[i - 1];

  /* Find and process each loop.
     We scan from the end, and process each loop when its start is seen,
     so we process innermost loops first.  */

  for (insn = last_insn; insn; insn = PREV_INSN (insn))
    if (GET_CODE (insn) == NOTE
	&& NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
      {
	/* Make sure it really is a loop -- no jumps in from outside.  */
	end = verify_loop (f, insn);
	if (end != 0)
	  /* If so, optimize this loop.  */
	  scan_loop (insn, end, max_reg_num ());
	else if (loop_dump_stream)
	  fprintf (loop_dump_stream,
		   "\nLoop at %d ignored due to multiple entry points.\n",
		   INSN_UID (insn));
      }
}

/* Optimize one loop whose start is LOOP_START and end is END.
   LOOP_START is the NOTE_INSN_LOOP_BEG and END is the matching
   NOTE_INSN_LOOP_END.  */

/* ??? can also move memory writes out of loop if destination
   address is invariant? */

static void
scan_loop (loop_start, end, nregs)
     rtx loop_start, end;
     int nregs;
{
  register int i;
  register rtx p = NEXT_INSN (loop_start);
  /* 1 if we are scanning insns that could be executed zero times.  */
  int maybe_never = 0;
  /* 1 if we are scanning insns that might never be executed
     due to a subroutine call which might exit before they are reached.  */
  int call_passed = 0;
  /* For a rotated loop that is entered near the bottom,
     this is the label at the top.  Otherwise it is zero.  */
  rtx loop_top = 0;
  /* Jump insn that enters the loop, or 0 if control drops in.  */
  rtx loop_entry_jump = 0;
  /* Place in the loop where control enters.  */
  rtx scan_start;
  /* Number of insns in the loop.  */
  int insn_count;
  int tem;
  rtx temp;
  /* Chain describing insns movable in current loop.  */
  struct movable *movables = 0;
  /* Last element in `movables' -- so we can add elements at the end.  */
  struct movable *last_movable = 0;
  /* Ratio of extra register life span we can justify
     for saving an instruction.  More if loop doesn't call subroutines
     since in that case saving an insn makes more difference
     and more registers are available.  */
  int threshold = loop_has_call ? 15 : 30;
  /* Nonzero if the insn that jumps into the real loop
     is not the very first thing after the loop-beginning note.  */
  int something_before_entry_jump = 0;

  n_times_set = (short *) alloca (nregs * sizeof (short));
  n_times_used = (short *) alloca (nregs * sizeof (short));
  may_not_optimize = (char *) alloca (nregs);

  /* Determine whether this loop starts with a jump down
     to a test at the end.  */
  while (p != end
	 && GET_CODE (p) != CODE_LABEL && GET_CODE (p) != JUMP_INSN)
    {
      if (GET_CODE (p) == CALL_INSN || GET_CODE (p) == INSN)
	something_before_entry_jump = 1;
      p = NEXT_INSN (p);
    }

  /* "Loop" contains neither jumps nor labels;
     it must have been a dummy.  Think no more about it.  */
  if (p == end)
    return;

  scan_start = p;

  /* If loop has a jump before the first label,
     the true entry is the target of that jump.
     Start scan from there.
     But record in LOOP_TOP the place where the end-test jumps
     back to so we can scan that after the end of the loop.  */
  if (GET_CODE (p) == JUMP_INSN)
    {
      loop_entry_jump = p;
      loop_top = NEXT_INSN (p);
      /* Loop entry will never be a conditional jump.
	 If we see one, this must not be a real loop.
	 Also, a return-insn isn't a jump to enter the loop.  */
      if (GET_CODE (loop_top) != BARRIER
	  || GET_CODE (PATTERN (p)) != SET)
	return;
      /* Get the label at which the loop is entered.  */
      p = XEXP (SET_SRC (PATTERN (p)), 0);
      /* Check to see whether the jump actually
	 jumps out of the loop (meaning it's no loop).
	 This case can happen for things like
	 do {..} while (0).  */
      if (p == 0
	  || INSN_LUID (p) < INSN_LUID (loop_start)
	  || INSN_LUID (p) >= INSN_LUID (end))
	{
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "\nLoop from %d to %d is phony.\n\n",
		     INSN_UID (loop_start), INSN_UID (end));
	  return;
	}

      /* Find the first label after the entry-jump.  */
      while (GET_CODE (loop_top) != CODE_LABEL)
	{
	  loop_top = NEXT_INSN (loop_top);
	  if (loop_top == 0)
	    abort ();
	}

      /* Maybe rearrange the loop to drop straight in
	 with a new test to jump around it entirely.
	 (The latter is considered outside the loop.)
	 If this is done, we no longer enter with a jump.  */
      if (! something_before_entry_jump
	  && loop_skip_over (loop_start, end, loop_entry_jump))
	{
	  scan_start = loop_top;
	  loop_top = 0;
	}
      else
	/* We really do enter with a jump;
	   scan the loop from the place where the jump jumps to.  */
	scan_start = p;
    }

  /* Count number of times each reg is set during this loop.
     Set may_not_optimize[I] if it is not safe to move out
     the setting of register I.  */

  bzero (n_times_set, nregs * sizeof (short));
  bzero (may_not_optimize, nregs);
  count_loop_regs_set (loop_top ? loop_top : loop_start, end,
		       may_not_optimize, &insn_count, nregs);
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    may_not_optimize[i] = 1, n_times_set[i] = 1;
  bcopy (n_times_set, n_times_used, nregs * sizeof (short));

  if (loop_dump_stream)
    {
      fprintf (loop_dump_stream, "\nLoop from %d to %d: %d real insns.\n",
	       INSN_UID (loop_start), INSN_UID (end), insn_count);
      if (loop_continue)
	fprintf (loop_dump_stream, "Continue at insn %d.\n",
		 INSN_UID (loop_continue));
    }

  /* Scan through the loop finding insns that are safe to move.
     In each such insn, store QImode as the mode, to mark it.
     Then set n_times_set to -1 for the reg being set, so that
     this reg will be considered invariant for subsequent insns.
     We consider whether subsequent insns use the reg
     in deciding whether it is worth actually moving.

     MAYBE_NEVER is nonzero if we have passed a conditional jump insn
     and therefore it is possible that the insns we are scanning
     would never be executed.  At such times, we must make sure
     that it is safe to execute the insn once instead of zero times.
     When MAYBE_NEVER is 0, all insns will be executed at least once
     so that is not a problem.  */

  p = scan_start;
  while (1)
    {
      p = NEXT_INSN (p);
      /* At end of a straight-in loop, we are done.
	 At end of a loop entered at the bottom, scan the top.  */
      if (p == scan_start)
	break;
      if (p == end)
	{
	  if (loop_top != 0)
	    p = NEXT_INSN (loop_top);
	  else
	    break;
	  if (p == scan_start)
	    break;
	}
      if (GET_CODE (p) == INSN
	  && GET_CODE (PATTERN (p)) == SET
	  && GET_CODE (SET_DEST (PATTERN (p))) == REG
	  && ! may_not_optimize[REGNO (SET_DEST (PATTERN (p)))])
	{
	  int tem1 = 0;
	  /* Don't try to optimize a register that was made
	     by loop-optimization for an inner loop.
	     We don't know its life-span, so we can't compute the benefit.  */
	  if (REGNO (SET_DEST (PATTERN (p))) >= old_max_reg)
	    ;
	  /* IN order to move a register, we need to have one of three cases:
	     (1) it is used only in the same basic block as the set
	     (2) it is not a user variable.
	     (3) the set is guaranteed to be executed once the loop starts,
	         and the reg is not used until after that.  */
	  else if (! ((! maybe_never
		       && ! loop_reg_used_before_p (p, loop_start, scan_start, end))
		      || ! REG_USERVAR_P (SET_DEST (PATTERN (p)))
		      || reg_in_basic_block_p (p, SET_DEST (PATTERN (p)))))
	    ;
	  else if (((tem = invariant_p (SET_SRC (PATTERN (p))))
		    || ((temp = loop_find_reg_equal (p)) 
			&& (tem = invariant_p (XEXP (temp, 0)))))
		   && (n_times_set[REGNO (SET_DEST (PATTERN (p)))] == 1
		       || (tem1
			   = consec_sets_invariant_p (SET_DEST (PATTERN (p)),
						      n_times_set[REGNO (SET_DEST (PATTERN (p)))],
						      p)))
		   /* If the insn can cause a trap (such as divide by zero),
		      can't move it unless it's guaranteed to be executed
		      once loop is entered.  Even a function call might
		      prevent the trap insn from being reached
		      (since it might exit!)  */
		   && ! ((maybe_never || call_passed)
			 && (may_trap_p (SET_SRC (PATTERN (p)))
			     || ((temp = loop_find_reg_equal (p))
				 && may_trap_p (XEXP (temp, 0))))))
	    {
	      register struct movable *m;
	      register int regno = REGNO (SET_DEST (PATTERN (p)));
	      int count;
	      m = (struct movable *) alloca (sizeof (struct movable));
	      m->next = 0;
	      m->insn = p;
	      temp = loop_find_reg_equal (p);
	      if (temp)
		m->set_src = XEXP (temp, 0);
	      else
		m->set_src = SET_SRC (PATTERN (p));
	      m->force = 0;
	      m->consec = n_times_set[REGNO (SET_DEST (PATTERN (p)))] - 1;
	      m->done = 0;
	      m->forces = 0;
	      m->partial = 0;
	      m->savemode = VOIDmode;
	      m->regno = regno;
	      /* Set M->cond if either invariant_p or consec_sets_invariant_p
		 returned 2 (only conditionally invariant).  */
	      m->cond = ((tem | tem1) > 1);
	      m->global = (uid_luid[regno_last_uid[regno]] > INSN_LUID (end)
			   || uid_luid[regno_first_uid[regno]] < INSN_LUID (loop_start));
	      m->match = 0;
	      m->lifetime = (uid_luid[regno_last_uid[regno]]
			     - uid_luid[regno_first_uid[regno]]);
	      m->savings = n_times_used[regno];
	      n_times_set[regno] = -1;
	      /* Add M to the end of the chain MOVABLES.  */
	      if (movables == 0)
		movables = m;
	      else
		last_movable->next = m;
	      last_movable = m;
	      if (m->consec > 0)
		{
		  /* Skip this insn, not checking REG_LIBCALL notes.  */
		  p = NEXT_INSN (p);
		  /* Skip the consecutive insns, if there are any.  */
		  p = skip_consec_insns (p, m->consec);
		  /* Back up, so the main loop will advance to the right place.  */
		  p = PREV_INSN (p);
		}
	    }
	  /* If this register is always set within a STRICT_LOW_PART
	     or set to zero, then its high bytes are constant.
	     So clear them outside the loop and within the loop
	     just load the low bytes.
	     We must check that the machine has an instruction to do so.
	     Also, if the value loaded into the register
	     depends on the same register, this cannot be done.  */
	  else if (SET_SRC (PATTERN (p)) == const0_rtx
		   && GET_CODE (NEXT_INSN (p)) == INSN
		   && GET_CODE (PATTERN (NEXT_INSN (p))) == SET
		   && (GET_CODE (SET_DEST (PATTERN (NEXT_INSN (p))))
		       == STRICT_LOW_PART)
		   && (GET_CODE (XEXP (SET_DEST (PATTERN (NEXT_INSN (p))), 0))
		       == SUBREG)
		   && (SUBREG_REG (XEXP (SET_DEST (PATTERN (NEXT_INSN (p))), 0))
		       == SET_DEST (PATTERN (p)))
		   && !reg_mentioned_p (SET_DEST (PATTERN (p)),
					SET_SRC (PATTERN (NEXT_INSN (p)))))
	    {
	      register int regno = REGNO (SET_DEST (PATTERN (p)));
	      if (n_times_set[regno] == 2)
		{
		  register struct movable *m;
		  int count;
		  m = (struct movable *) alloca (sizeof (struct movable));
		  m->next = 0;
		  m->insn = p;
		  m->force = 0;
		  m->consec = 0;
		  m->done = 0;
		  m->forces = 0;
		  m->partial = 1;
		  /* If the insn may not be executed on some cycles,
		     we can't clear the whole reg; clear just high part.
		     Not even if the reg is used only within this loop.
		     Consider this:
		     while (1)
		       while (s != t) {
		         if (foo ()) x = *s;
			 use (x);
		       }
		     Clearing x before the inner loop could clobber a value
		     being saved from the last time around the outer loop.
		     However, if the reg is not used outside this loop
		     and all uses of the register are in the same
		     basic block as the store, there is no problem.  */
		  m->global = (uid_luid[regno_last_uid[regno]] > INSN_LUID (end)
			       || uid_luid[regno_first_uid[regno]] < INSN_LUID (p)
			       || (labels_in_range_p
				   (p, uid_luid[regno_first_uid[regno]])));
		  if (maybe_never && m->global)
		    m->savemode = GET_MODE (SET_SRC (PATTERN (NEXT_INSN (p))));
		  else
		    m->savemode = VOIDmode;
		  m->regno = regno;
		  m->cond = 0;
		  m->match = 0;
		  m->lifetime = (uid_luid[regno_last_uid[regno]]
				 - uid_luid[regno_first_uid[regno]]);
		  m->savings = 1;
		  n_times_set[regno] = -1;
		  /* Add M to the end of the chain MOVABLES.  */
		  if (movables == 0)
		    movables = m;
		  else
		    last_movable->next = m;
		  last_movable = m;
		}
	    }
	}
      /* Past a call insn, we get to insns which might not be executed
	 because the call might exit.  This matters for insns that trap.  */
      else if (GET_CODE (p) == CALL_INSN)
	call_passed = 1;
      /* Past a label or a jump, we get to insns for which we
	 can't count on whether or how many times they will be
	 executed during each iteration.  Therefore, we can
	 only move out sets of trivial variables
	 (those not used after the loop).  */
      else if ((GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN)
	       /* If we enter the loop in the middle, and scan around
		  to the beginning, don't set maybe_never for that.  */
               && ! (NEXT_INSN (p) == end && GET_CODE (p) == JUMP_INSN
                     && simplejump_p (p)))
	maybe_never = 1;
    }

  /* If one movable subsumes another, ignore that other.  */

  ignore_some_movables (movables);

  /* For each movable insn, see if the reg that it loads
     leads when it dies right into another conditionally movable insn.
     If so, record that the second insn "forces" the first one,
     since the second can be moved only if the first is.  */

  force_movables (movables);

  /* See if there are multiple movable insns that load the same value.
     If there are, make all but the first point at the first one
     through the `match' field, and add the priorities of them
     all together as the priority of the first.  */

  combine_movables (movables, nregs);
	
  /* Now consider each movable insn to decide whether it is worth moving.
     Store 0 in n_times_set for each reg that is moved.  */

  move_movables (movables, threshold,
		 insn_count, loop_start, end, nregs);

  /* Now candidates that still have -1 are those not moved.
     Change n_times_set to indicate that those are not actually invariant.  */
  for (i = 0; i < nregs; i++)
    if (n_times_set[i] < 0)
      n_times_set[i] = n_times_used[i];

  if (flag_strength_reduce)
    strength_reduce (scan_start, end, loop_top,
		     insn_count, loop_start, end, nregs);
}

/* Return 1 if all uses of REG
   are between INSN and the end of the basic block.  */

static int 
reg_in_basic_block_p (insn, reg)
     rtx insn, reg;
{
  int regno = REGNO (reg);
  rtx p;

  if (regno_first_uid[regno] != INSN_UID (insn))
    return 0;

  /* Search this basic block for the already recorded last use of the reg.  */
  for (p = insn; p; p = NEXT_INSN (p))
    {
      switch (GET_CODE (p))
	{
	case NOTE:
	  break;

	case INSN:
	case CALL_INSN:
	  /* Ordinary insn: if this is the last use, we win.  */
	  if (regno_last_uid[regno] == INSN_UID (p))
	    return 1;
	  break;

	case JUMP_INSN:
	  /* Jump insn: if this is the last use, we win.  */
	  if (regno_last_uid[regno] == INSN_UID (p))
	    return 1;
	  /* Otherwise, it's the end of the basic block, so we lose.  */
	  return 0;

	case CODE_LABEL:
	case BARRIER:
	  /* It's the end of the basic block, so we lose.  */
	  return 0;
	}
    }

  /* The "last use" doesn't follow the "first use"??  */
  abort ();
}

/* Skip COUNT insns from INSN, counting library calls as 1 insn.  */

static rtx
skip_consec_insns (insn, count)
     rtx insn;
     int count;
{
  for (; count > 0; count--)
    {
      if (GET_CODE (insn) == NOTE)
	insn = NEXT_INSN (insn);
      else if (GET_CODE (insn) == BARRIER || GET_CODE (insn) == CODE_LABEL)
	abort ();
      else
	{
	  rtx i1, temp;

	  /* If first insn of gnulib call sequence, skip to end.  */
	  /* Do this at start of loop, since INSN is guaranteed to 
	     be an insn here.  */
	  if (temp = find_reg_note (insn, REG_LIBCALL, 0))
	    insn = XEXP (temp, 0);

	  do insn = NEXT_INSN (insn);
	  while (GET_CODE (insn) == NOTE);
	}
    }

  return insn;
}

/* Find a REG_EQUAL note in INSN but only if it is safe to use for our
   purposes.  Those put in by CSE are not safe since they may fail to
   use the registers that appear in the actual insn source.  */

static rtx
loop_find_reg_equal (insn)
     rtx insn;
{
  return (find_reg_note (insn, REG_RETVAL, 0)
	  ? find_reg_note (insn, REG_EQUAL, 0)
	  : 0);
}

/* Ignore any movable whose insn falls within a libcall
   which is part of another movable.
   We make use of the fact that the movable for the libcall value
   was made later and so appears later on the chain.  */

static void
ignore_some_movables (movables)
     struct movable *movables;
{
  register struct movable *m, *m1;

  for (m = movables; m; m = m->next)
    {
      /* Is this a movable for the value of a libcall?  */
      rtx note = find_reg_note (m->insn, REG_RETVAL, 0);
      if (note)
	{
	  /* Find the beginning of that libcall.  */
	  rtx first_insn = XEXP (note, 0);
	  /* Check for earlier movables inside that range,
	     and mark them invalid.  */
	  for (m1 = movables; m1 != m; m1 = m1->next)
	    if (INSN_LUID (m1->insn) >= INSN_LUID (first_insn)
		&& INSN_LUID (m1->insn) < INSN_LUID (m->insn))
	      m1->done = 1;
	}
    }
}	  

/* For each movable insn, see if the reg that it loads
   leads when it dies right into another conditionally movable insn.
   If so, record that the second insn "forces" the first one,
   since the second can be moved only if the first is.  */

static void
force_movables (movables)
     struct movable *movables;
{
  register struct movable *m, *m1;
  for (m1 = movables; m1; m1 = m1->next)
    /* Omit this if moving just the (SET (REG) 0) of a zero-extend.  */
    if (!m1->partial && !m1->done)
      {
	int regno = m1->regno;
	for (m = m1->next; m; m = m->next)
	  /* ??? Could this be a bug?  What if CSE caused the
	     register of M1 to be used after this insn?
	     Since CSE does not update regno_last_uid,
	     this insn M->insn might not be where it dies.
	     But very likely this doesn't matter; what matters is
	     that M's reg is computed from M1's reg.  */
	  if (INSN_UID (m->insn) == regno_last_uid[regno]
	      && !m->done)
	    break;
	if (m != 0 && m->set_src == SET_DEST (PATTERN (m1->insn)))
	  m = 0;

	/* Increase the priority of the moving the first insn
	   since it permits the second to be moved as well.  */
	if (m != 0)
	  {
	    m->forces = m1;
	    m1->lifetime += m->lifetime;
	    m1->savings += m1->savings;
	  }
      }
}

/* Find invariant expressions that are equal and can be combined into
   one register.  */

static void
combine_movables (movables, nregs)
     struct movable *movables;
     int nregs;
{
  register struct movable *m;
  char *matched_regs = (char *) alloca (nregs);
  enum machine_mode mode;

  /* Regs that are set more than once are not allowed to match
     or be matched.  I'm no longer sure why not.  */
  /* Perhaps testing m->consec_sets would be more appropriate here?  */

  for (m = movables; m; m = m->next)
    if (m->match == 0 && n_times_used[m->regno] == 1 && !m->partial)
      {
	register struct movable *m1;
	int regno = m->regno;

	bzero (matched_regs, nregs);
	matched_regs[regno] = 1;

	for (m1 = m->next; m1; m1 = m1->next)
	  if (m1->match == 0 && n_times_used[m1->regno] == 1
	      /* A reg used outside the loop mustn't be eliminated.  */
	      && !m1->global
	      /* A reg used for zero-extending mustn't be eliminated.  */
	      && !m1->partial
	      && (matched_regs[m1->regno]
		  ||
		  (
		   /* Can't combine regs with different modes
		      even if loaded from the same constant.  */
		   (GET_MODE (SET_DEST (PATTERN (m->insn)))
		    == GET_MODE (SET_DEST (PATTERN (m1->insn))))
		   /* See if the source of M1 says it matches M.  */
		   && ((GET_CODE (m1->set_src) == REG
			&& matched_regs[REGNO (m1->set_src)])
		       || rtx_equal_for_loop_p (m->set_src, m1->set_src,
						movables)
		       || (REG_NOTES (m->insn) && REG_NOTES (m1->insn)
			   && REG_NOTE_KIND (REG_NOTES (m->insn)) == REG_EQUIV
			   && REG_NOTE_KIND (REG_NOTES (m1->insn)) == REG_EQUIV
			   && rtx_equal_p (XEXP (REG_NOTES (m->insn), 0),
					   XEXP (REG_NOTES (m1->insn), 0)))))))
	    {
	      m->lifetime += m1->lifetime;
	      m->savings += m1->savings;
	      m1->match = m;
	      matched_regs[m1->regno] = 1;
	    }
      }

  /* Now combine the regs used for zero-extension.
     This can be done for those not marked `global'
     provided their lives don't overlap.  */

  for (mode = VOIDmode; (int) mode < (int) MAX_MACHINE_MODE;
       mode = (enum machine_mode) ((int) mode + 1))
    if (GET_MODE_CLASS (mode) == MODE_INT)
      {
	register struct movable *m0 = 0;

	/* Combine all the registers for extension from mode MODE.
	   Don't combine any that are used outside this loop.  */
	for (m = movables; m; m = m->next)
	  if (m->partial && ! m->global
	      && mode == GET_MODE (SET_SRC (PATTERN (NEXT_INSN (m->insn)))))
	    {
	      register struct movable *m1;
	      int first = uid_luid[regno_first_uid[m->regno]];
	      int last = uid_luid[regno_last_uid[m->regno]];

	      if (m0 == 0)
		{
		  /* First one: don't check for overlap, just record it.  */
		  m0 = m;
		  continue;
		}

	      /* Make sure they extend to the same mode.
		 (Almost always true.)  */
	      if (GET_MODE (SET_DEST (PATTERN (m->insn)))
		  != GET_MODE (SET_DEST (PATTERN (m0->insn))))
		continue;

	      /* We already have one: check for overlap with those
		 already combined together.  */
	      for (m1 = movables; m1 != m; m1 = m1->next)
		if (m1 == m0 || (m1->partial && m1->match == m0))
		  if (! (uid_luid[regno_first_uid[m1->regno]] > last
			 || uid_luid[regno_last_uid[m1->regno]] < first))
		    goto overlap;

	      /* No overlap: we can combine this with the others.  */
	      m0->lifetime += m->lifetime;
	      m0->savings += m->savings;
	      m->match = m0;

	    overlap: ;
	    }
      }
}

/* Return 1 if regs X and Y will become the same if moved.  */

static int
regs_match_p (x, y, movables)
     rtx x, y;
     struct movable *movables;
{
  int xn = REGNO (x);
  int yn = REGNO (y);
  struct movable *mx, *my;

  for (mx = movables; mx; mx = mx->next)
    if (mx->regno == xn)
      break;

  for (my = movables; my; my = my->next)
    if (my->regno == yn)
      break;

  return (mx && my
	  && ((mx->match == my->match && mx->match != 0)
	      || mx->match == my
	      || mx == my->match));
}

/* Return 1 if X and Y are identical-looking rtx's.
   This is the Lisp function EQUAL for rtx arguments.  */

static int
rtx_equal_for_loop_p (x, y, movables)
     rtx x, y;
     struct movable *movables;
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
    return ((REGNO (x) == REGNO (y)
	     && REG_FUNCTION_VALUE_P (x) == REG_FUNCTION_VALUE_P (y))
	    || regs_match_p (x, y, movables));

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
	    if (rtx_equal_for_loop_p (XVECEXP (x, i, j), XVECEXP (y, i, j), movables) == 0)
	      return 0;
	  break;

	case 'e':
	  if (rtx_equal_for_loop_p (XEXP (x, i), XEXP (y, i), movables) == 0)
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

/* Scan MOVABLES, and move the insns that deserve to be moved.
   If two matching movables are combined, replace one reg with the
   other throughout.  */

static void
move_movables (movables, threshold, insn_count, loop_start, end, nregs)
     struct movable *movables;
     int threshold;
     int insn_count;
     rtx loop_start;
     rtx end;
     int nregs;
{
  rtx new_start = 0;
  register struct movable *m;
  register rtx p;
  /* Map of pseudo-register replacements to handle combining
     when we move several insns that load the same value
     into different pseudo-registers.  */
  rtx *reg_map = (rtx *) alloca (nregs * sizeof (rtx));
  char *already_moved = (char *) alloca (nregs);

  bzero (already_moved, nregs);
  bzero (reg_map, nregs * sizeof (rtx));

  num_movables = 0;

  for (m = movables; m; m = m->next)
    {
      /* Describe this movable insn.  */

      if (loop_dump_stream)
	{
	  fprintf (loop_dump_stream, "Insn %d: regno %d (life %d), ",
		   INSN_UID (m->insn), m->regno, m->lifetime);
	  if (m->consec > 0)
	    fprintf (loop_dump_stream, "consec %d, ", m->consec);
	  if (m->cond)
	    fprintf (loop_dump_stream, "cond ");
	  if (m->force)
	    fprintf (loop_dump_stream, "force ");
	  if (m->global)
	    fprintf (loop_dump_stream, "global ");
	  if (m->done)
	    fprintf (loop_dump_stream, "done ");
	  if (m->match)
	    fprintf (loop_dump_stream, "matches %d ",
		     INSN_UID (m->match->insn));
	  if (m->forces)
	    fprintf (loop_dump_stream, "forces %d ",
		     INSN_UID (m->forces->insn));
	}

      /* Count movables.  Value used in heuristics in strength_reduce.  */
      num_movables++;

      /* Ignore the insn if it's already done (it matched something else).
	 Otherwise, see if it is now safe to move.  */

      if (!m->done
	  && (! m->cond
	      || (1 == invariant_p (m->set_src)
		  && (m->consec == 0
		      || 1 == consec_sets_invariant_p (SET_DEST (PATTERN (m->insn)),
						       m->consec + 1,
						       m->insn))))
	  && (! m->forces || m->forces->done))
	{
	  register int regno;
	  register rtx p;
	  int savings = m->savings;

	  /* We have an insn that is safe to move.
	     Compute its desirability.  */

	  p = m->insn;
	  regno = m->regno;

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "savings %d ", savings);

	  if (moved_once[regno])
	    {
	      insn_count *= 2;

	      if (loop_dump_stream)
		fprintf (loop_dump_stream, "halved since already moved ");
	    }

	  /* An insn MUST be moved if we already moved something else
	     which is safe only if this one is moved too: that is,
	     if already_moved[REGNO] is nonzero.  */

	  /* An insn is desirable to move if the new lifetime of the
	     register is no more than THRESHOLD times the old lifetime.
	     If it's not desirable, it means the loop is so big
	     that moving won't speed things up much,
	     and it is liable to make register usage worse.  */

	  /* It is also desirable to move if it can be moved at no
	     extra cost because something else was already moved.  */

	  if (already_moved[regno]
	      || (threshold * savings * m->lifetime) >= insn_count
	      || (m->forces && m->forces->done
		  && n_times_used[m->forces->regno] == 1))
	    {
	      int count;
	      register struct movable *m1;
	      rtx first;

	      /* Now move the insns that set the reg.  */

	      for (count = m->consec; count >= 0; count--)
		{
		  rtx i1, temp;

		  /* If first insn of gnulib call sequence, skip to end.  */
		  /* Do this at start of loop, since p is guaranteed to 
		     be an insn here.  */
		  if (temp = find_reg_note (p, REG_LIBCALL, 0))
		    p = XEXP (temp, 0);
		  
		  /* If last insn of gnulib call sequence, move all
		     insns except the last before the loop.  The last insn is
		     handled in the normal manner.  */
		  if (temp = find_reg_note (p, REG_RETVAL
					    , 0))
		    {
		      rtx fn_address = 0;
		      rtx fn_reg = 0;
		      first = 0;
		      for (temp = XEXP (temp, 0); temp != p;
			   temp = NEXT_INSN (temp))
			{
			  rtx body = PATTERN (temp);
			  rtx n;
			  /* Extract the function address from the insn
			     that loads it into a register.
			     If this insn was cse'd, we get incorrect code.
			     So delete it and stick the fn address right
			     into the call insn.  Since the moved insns
			     won't be cse'd, that does no harm.  */
			  if (GET_CODE (NEXT_INSN (temp)) == CALL_INSN
			      && GET_CODE (body) == SET
			      && GET_CODE (SET_DEST (body)) == REG
			      && (n = find_reg_note (temp, REG_EQUIV, 0)))
			    {
			      fn_reg = SET_SRC (body);
			      if (GET_CODE (fn_reg) != REG)
				fn_reg = SET_DEST (body);
			      fn_address = XEXP (n, 0);
			      continue;
			    }
			  /* We have the call insn.
			     Substitute the fn address for the reg
			     that we believe this insn will use.  */
			  if (GET_CODE (temp) == CALL_INSN
			      && fn_address != 0)
			    replace_call_address (body, fn_reg, fn_address);
			  if (GET_CODE (temp) == CALL_INSN)
			    i1 = emit_call_insn_before (body, loop_start);
			  else
			    i1 = emit_insn_before (body, loop_start);
			  if (first == 0)
			    first = i1;
			  REG_NOTES (i1) = REG_NOTES (temp);
			  delete_insn (temp);
			}
		    }
		  if (m->savemode != VOIDmode)
		    {
		      /* P sets REG to zero; but we should clear only the bits
			 that are not covered by the mode m->savemode.  */
		      rtx reg = SET_DEST (PATTERN (p));
		      i1 = emit_insn_before
			(gen_rtx (SET, VOIDmode, reg,
				  gen_rtx (AND, GET_MODE (reg),
					   reg,
					   gen_rtx (CONST_INT, VOIDmode,
						    (1 << GET_MODE_BITSIZE (m->savemode)) - 1))),
			 loop_start);
		    }
		  else if (GET_CODE (PATTERN (p)) == CALL_INSN)
		    i1 = emit_call_insn_before (PATTERN (p), loop_start);
		  else
		    i1 = emit_insn_before (PATTERN (p), loop_start);

		  if (new_start == 0)
		    new_start = i1;

		  if (loop_dump_stream)
		    fprintf (loop_dump_stream, " moved to %d", INSN_UID (i1));

		  /* Mark the moved, invariant reg as being equivalent to
		     its constant value.  */
		  REG_NOTES (i1) = REG_NOTES (p);
		  if (REG_NOTES (i1) == 0
		      && ! m->partial /* But not if it's a zero-extend clr. */
		      && ! m->global /* and not if used outside the loop
					(since it might get set outside).  */
		      && CONSTANT_P (SET_SRC (PATTERN (p))))
		    REG_NOTES (i1)
		      = gen_rtx (EXPR_LIST, REG_EQUIV,
				 SET_SRC (PATTERN (p)), REG_NOTES (i1));

		  /* If library call, now fix the REG_NOTES that contain
		     insn pointers, namely REG_LIBCALL on FIRST
		     and REG_RETVAL on I1.  */
		  if (temp = find_reg_note (i1, REG_RETVAL, 0))
		    {
		      XEXP (temp, 0) = first;
		      temp = find_reg_note (first, REG_LIBCALL, 0);
		      XEXP (temp, 0) = i1;
		    }

		  delete_insn (p);
		  do p = NEXT_INSN (p);
		  while (p != 0 && GET_CODE (p) == NOTE);
		}

	      /* The more regs we move, the less we like moving them.  */
	      threshold -= 3;

	      /* Any other movable that loads the same register
		 MUST be moved.  */
	      already_moved[regno] = 1;

	      /* This reg has been moved out of one loop.  */
	      moved_once[regno] = 1;

	      /* The reg set here is now invariant.  */
	      if (! m->partial)
		n_times_set[regno] = 0;

	      m->done = 1;

	      /* Change the length-of-life info for the register
		 to say it lives at least the full length of this loop.
		 This will help guide optimizations in outer loops.  */

	      if (uid_luid[regno_first_uid[regno]] > INSN_LUID (loop_start))
		/* This is the old insn before all the moved insns.
		   We can't use the moved insn because it is out of range
		   in uid_luid.  Only the old insns have luids.  */
		regno_first_uid[regno] = INSN_UID (loop_start);
	      if (uid_luid[regno_last_uid[regno]] < INSN_LUID (end))
		regno_last_uid[regno] = INSN_UID (end);

	      /* Combine with this moved insn any other matching movables.  */

	      for (m1 = m->next; m1; m1 = m1->next)
		if (m1->match == m)
		  {
		    rtx temp;

		    /* Schedule the reg loaded by M1
		       for replacement so that shares the reg of M.  */
		    reg_map[m1->regno] = SET_DEST (PATTERN (m->insn));
		    /* Get rid of the matching insn
		       and prevent further processing of it.  */
		    m1->done = 1;

		    /* if library call, delete all insn except last, which
		       is deleted below */
		    if (temp = find_reg_note (m1->insn, REG_RETVAL, 0))
		      {
			for (temp = XEXP (temp, 0); temp != m1->insn;
			     temp = NEXT_INSN (temp))
			    delete_insn (temp);
		      }
		    delete_insn (m1->insn);

		    /* Any other movable that loads the same register
		       MUST be moved.  */
		    already_moved[m1->regno] = 1;

		    /* The reg merged here is now invariant,
		       if the reg it matches is invariant.  */
		    if (! m->partial)
		      n_times_set[m1->regno] = 0;
		  }
	    }
	  else if (loop_dump_stream)
	    fprintf (loop_dump_stream, "not desirable");
	}
      else if (loop_dump_stream && !m->match)
	fprintf (loop_dump_stream, "not safe");

      if (loop_dump_stream)
	fprintf (loop_dump_stream, "\n");
    }

  if (new_start == 0)
    new_start = loop_start;

  /* Go through all the instructions in the loop, making
     all the register substitutions scheduled in REG_MAP.  */
  for (p = new_start; p != end; p = NEXT_INSN (p))
    if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
	|| GET_CODE (p) == CALL_INSN)
      {
	rtx tail;

	replace_regs (PATTERN (p), reg_map, nregs);
	/* Subsitute registers in the equivalent expression also.  */
	for (tail = REG_NOTES (p); tail; tail = XEXP (tail, 1))
	  if (REG_NOTE_KIND (tail) == REG_EQUAL
	      || REG_NOTE_KIND (tail) == REG_EQUIV)
	    replace_regs (XEXP (tail, 0), reg_map, nregs);
      }
}

/* Optionally change a loop which enters just before the endtest
   to one which falls straight in
   after skipping around the entire loop if the endtest would drop out.
   Returns 1 if the change was made, 0 if the loop was not really suitable.  */

int
loop_skip_over (start, end, loop_entry_jump)
     rtx start;
     rtx end;
     rtx loop_entry_jump;
{
  rtx entry_insn;
  rtx endtest;
  rtx endtestjump;
  register rtx p = JUMP_LABEL (loop_entry_jump);

  while (GET_CODE (p) != INSN && GET_CODE (p) != JUMP_INSN
	 && GET_CODE (p) != CALL_INSN)
    p = NEXT_INSN (p);
  entry_insn = p;

  /* Skip any ordinary arithmetic insns to find the compare.  */
  for (; p != 0; p = NEXT_INSN (p))
    if (GET_CODE (p) != NOTE)
      if (GET_CODE (p) != INSN || sets_cc0_p (PATTERN (p)))
	break;
  if (p == 0 || GET_CODE (p) != INSN)
    return 0;
  endtest = p;
  endtestjump = next_real_insn (p);

  /* Check that (1) we have reached a compare insn and (2)
     the insn (presumably a jump) following that compare
     is the last in the loop and jumps back to the loop beginning.  */

  if (sets_cc0_p (PATTERN (endtest)) > 0
      && endtestjump == prev_real_insn (end)
      && prev_real_insn (JUMP_LABEL (endtestjump)) == loop_entry_jump)
    {
      rtx newlab;
      /* This is the jump that we insert.  */
      rtx new_jump;

      /* Duplicate the ordinary arith insns before the compare.  */
      for (p = entry_insn; p != endtest; p = NEXT_INSN (p))
	if (GET_CODE (p) == INSN)
	  {
	    rtx new = emit_insn_before (copy_rtx (PATTERN (p)), start);
	    if (REG_NOTES (p))
	      REG_NOTES (new) = copy_rtx (REG_NOTES (p));
	  }
	
      /* Ok, duplicate that test before start of loop.  */
      emit_insn_before (copy_rtx (PATTERN (endtest)), start);
      /* Make a new entry-jump (before the original one)
	 whose condition is opposite to the loop-around endtest
	 and which jumps around the loop (to just after the ending NOTE).  */
      newlab = gen_label_rtx ();
      emit_label_after (newlab, end);
      emit_jump_insn_before (copy_rtx (PATTERN (endtestjump)), start);
      new_jump = PREV_INSN (start);
      JUMP_LABEL (new_jump) = JUMP_LABEL (endtestjump);
      LABEL_NUSES (JUMP_LABEL (endtestjump))++;
      invert_jump (new_jump, newlab);
      /* Delete the original entry-jump.  */
      delete_insn (loop_entry_jump);

      return 1;
    }

  return 0;
}

/* Throughout the rtx X, replace many registers according to REG_MAP.
   Return the replacement for X (which may be X with altered contents).
   REG_MAP[R] is the replacement for register R, or 0 for don't replace.
   NREGS is the length of REG_MAP; regs >= NREGS are not mapped.  */

static rtx
replace_regs (x, reg_map, nregs)
     rtx x;
     rtx *reg_map;
     int nregs;
{
  register enum rtx_code code;
  register int i;
  register char *fmt;

  if (x == 0)
    return x;

  code = GET_CODE (x);
  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
      return x;

    case REG:
      /* Verify that the register has an entry before trying to access it.  */
      if (REGNO (x) < nregs && reg_map[REGNO (x)] != 0)
	return reg_map[REGNO (x)];
      return x;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = replace_regs (XEXP (x, i), reg_map, nregs);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j) = replace_regs (XVECEXP (x, i, j), reg_map, nregs);
	}
    }
  return x;
}

/* Scan X and replace the address of any MEM in it with ADDR.
   REG is the address that MEM should have before the replacement.  */

static void
replace_call_address (x, reg, addr)
     rtx x, reg, addr;
{
  register enum rtx_code code;
  register int i;
  register char *fmt;

  if (x == 0)
    return;
  code = GET_CODE (x);
  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case REG:
      return;

    case SET:
      /* Short cut for very common case.  */
      replace_call_address (XEXP (x, 1), reg, addr);
      return;

    case CALL:
      /* Short cut for very common case.  */
      replace_call_address (XEXP (x, 0), reg, addr);
      return;

    case MEM:
      /* If this MEM uses a reg other than the one we expected,
	 something is wrong.  */
      if (XEXP (x, 0) != reg)
	abort ();
      XEXP (x, 0) = addr;
      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	replace_call_address (XEXP (x, i), reg, addr);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    replace_call_address (XVECEXP (x, i, j), reg, addr);
	}
    }
}

/* Return the number of memory refs to addresses that vary
   in the rtx X.  */

static int
count_nonfixed_reads (x)
     rtx x;
{
  register enum rtx_code code;
  register int i;
  register char *fmt;
  int value;

  if (x == 0)
    return 0;

  code = GET_CODE (x);
  switch (code)
    {
    case PC:
    case CC0:
    case CONST_INT:
    case CONST_DOUBLE:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case REG:
      return 0;

    case MEM:
      return rtx_varies_p (XEXP (x, 0)) + count_nonfixed_reads (XEXP (x, 0));
    }

  value = 0;
  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	value += count_nonfixed_reads (XEXP (x, i));
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    value += count_nonfixed_reads (XVECEXP (x, i, j));
	}
    }
  return value;
}


#if 0
/* P is an instruction that sets a register to the result of a ZERO_EXTEND.
   Replace it with an instruction to load just the low bytes
   if the machine supports such an instruction,
   and insert above LOOP_START an instruction to clear the register.  */

static void
constant_high_bytes (p, loop_start)
     rtx p, loop_start;
{
  register rtx new;
  register int insn_code_number;

  /* Try to change (SET (REG ...) (ZERO_EXTEND (..:B ...)))
     to (SET (STRICT_LOW_PART (SUBREG:B (REG...))) ...).  */

  new = gen_rtx (SET, VOIDmode,
		 gen_rtx (STRICT_LOW_PART, VOIDmode,
			  gen_rtx (SUBREG, GET_MODE (XEXP (SET_SRC (PATTERN (p)), 0)),
				   SET_DEST (PATTERN (p)),
				   0)),
		 XEXP (SET_SRC (PATTERN (p)), 0));
  insn_code_number = recog (new, p);

  if (insn_code_number)
    {
      register int i;

      /* Clear destination register before the loop.  */
      emit_insn_before (gen_rtx (SET, VOIDmode,
				 SET_DEST (PATTERN (p)),
				 const0_rtx),
			loop_start);

      /* Inside the loop, just load the low part.  */
      PATTERN (p) = new;
    }
}
#endif

/* Verify that the ostensible loop starting at START
   really is a loop: nothing jumps into it from outside.
   Return the marker for the end of the loop, or zero if not a real loop.

   Also set the variables `unknown_*_altered' and `loop_has_call',
   and fill in the array `loop_store_addrs'.  */

static rtx
verify_loop (f, start)
     rtx f, start;
{
  register int level = 1;
  register rtx insn, end;

  /* First find the LOOP_END that matches.
     Also check each insn for storing in memory and record where.  */

  unknown_address_altered = 0;
  unknown_aggregate_altered = 0;
  fixed_aggregate_altered = 0;
  loop_has_call = 0;
  loop_store_addrs_idx = 0;

  num_mem_sets = 0;
  loops_enclosed = 1;
  loop_continue = 0;

  for (insn = NEXT_INSN (start); level > 0; insn = NEXT_INSN (insn))
    {
      if (insn == 0)
	/* Parse errors can cause a loop-beg with no loop-end.  */
	return 0;
      if (GET_CODE (insn) == NOTE)
	{
	  if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_BEG)
	    {
	      ++level;
	      /* Count number of loops contained in this one.  */
	      loops_enclosed++;
	    }
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_END)
	    {
	      --level;
	      if (level == 0)
		{
		  end = insn;
		  break;
		}
	    }
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_LOOP_CONT)
	    {
	      if (level == 1)
		loop_continue = insn;
	    }

	  /* Don't optimize loops containing setjmps.
	     On some machines, longjmp does not restore the reg
	     values as of the time of the setjmp.  */
	  else if (NOTE_LINE_NUMBER (insn) == NOTE_INSN_SETJMP)
	    return 0;
	}
      else if (GET_CODE (insn) == CALL_INSN)
	{
	  unknown_address_altered = 1;
	  loop_has_call = 1;
	}
/* ???
      else if (! unknown_address_altered) */
      else
	{
	  if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN)
	    note_stores (PATTERN (insn), note_addr_stored);
	}
    }

  /* Now scan all jumps in the function and see if any of them can
     reach a label within the range of the loop.  */

  for (insn = f; insn; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == JUMP_INSN
	/* Don't get fooled by jumps inserted by loop-optimize.
	   They don't have valid LUIDs, and they never jump into loops.  */
	&& INSN_UID (insn) < max_uid
	&& (INSN_LUID (insn) < INSN_LUID (start)
	    || INSN_LUID (insn) > INSN_LUID (end))
	/* We have a jump that is outside the loop.
	   Does it jump into the loop?  */
	&& can_jump_into_range_p (PATTERN (insn),
				  INSN_LUID (start), INSN_LUID (end)))
      return 0;

#if 0      
  /* Now scan all labels between them and check for any jumps from outside.
     This uses the ref-chains set up by find_basic_blocks.
     This code is not used because it's more convenient for other reasons
     to do the loop optimization before find_basic_blocks.  */

  for (insn = start; insn != end; insn = NEXT_INSN (insn))
    if (GET_CODE (insn) == CODE_LABEL)
      {
	register rtx y;
	for (y = LABEL_REFS (insn); y != insn; y = LABEL_NEXTREF (y))
	  if (INSN_LUID (CONTAINING_INSN (y)) < INSN_LUID (start)
	      || INSN_LUID (CONTAINING_INSN (y)) > INSN_LUID (end))
	    return 0;
      }
#endif

  return end;
}

/* Return 1 if somewhere in X is a LABEL_REF to a label
   located between BEG and END.  */

static int
can_jump_into_range_p (x, beg, end)
     rtx x;
     int beg, end;
{
  register enum rtx_code code = GET_CODE (x);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      register int luid = INSN_LUID (XEXP (x, 0));
      return luid > beg && luid < end;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  if (can_jump_into_range_p (XEXP (x, i), beg, end))
	    return 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    if (can_jump_into_range_p (XVECEXP (x, i, j), beg, end))
	      return 1;
	}
    }

  return 0;
}

/* Return nonzero if there is a label in the range from
   insn INSN to the insn whose luid is END.  */

static int
labels_in_range_p (insn, end)
     rtx insn;
     int end;
{
  while (insn && INSN_LUID (insn) <= end)
    {
      if (GET_CODE (insn) == CODE_LABEL)
	return 0;
      insn = NEXT_INSN (insn);
    }

  return 0;
}

/* Record that a memory reference X is being set.  */

static void
note_addr_stored (x)
     rtx x;
{
  if (x == 0 || GET_CODE (x) != MEM)
    return;

  /* Count number of memory writes.
     This affects heuristics in strength_reduce.  */
  num_mem_sets++;
  if (unknown_address_altered)
    return;

  if (GET_MODE (x) == BLKmode)
    unknown_address_altered = 1;
  else if (rtx_addr_varies_p (x))
    {
      if (GET_CODE (XEXP (x, 0)) == PLUS)
	unknown_aggregate_altered = 1;
      else
	unknown_address_altered = 1;
    }
  else
    {
      register int i;
      register rtx addr = XEXP (x, 0);

      if (MEM_IN_STRUCT_P (x))
	fixed_aggregate_altered = 1;
      for (i = 0; i < loop_store_addrs_idx; i++)
	if (rtx_equal_p (loop_store_addrs[i], addr))
	  {
	    if (loop_store_widths[i] < GET_MODE_SIZE (GET_MODE (x)))
	      loop_store_widths[i] = GET_MODE_SIZE (GET_MODE (x));
	    break;
	  }
      if (i == NUM_STORES)
	unknown_address_altered = 1;
      else if (i == loop_store_addrs_idx)
	{
	  loop_store_widths[i] = GET_MODE_SIZE (GET_MODE (x));
	  loop_store_addrs[loop_store_addrs_idx++] = addr;
	}
    }
}

/* Return nonzero if the rtx X is invariant over the current loop.

   The value is 2 if we refer to something only conditionally invariant.

   If `unknown_address_altered' is nonzero, no memory ref is invariant.
   Otherwise if `unknown_aggregate_altered' is nonzero,
   a memory ref is invariant if it is not part of an aggregate
   and its address is fixed and not in `loop_store_addrs'.
   Otherwise if `fixed_aggregate_altered' is nonzero,
   a memory ref is invariant
   if its address is fixed and not in `loop_store_addrs'.
   Otherwise, a memory ref is invariant if its address is fixed and not in
   `loop_store_addrs' or if it is not an aggregate.  */

static int
invariant_p (x)
     register rtx x;
{
  register int i;
  register enum rtx_code code;
  register char *fmt;
  int conditional = 0;

  if (x == 0)
    return 1;
  code = GET_CODE (x);
  switch (code)
    {
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST:
      return 1;

    case PC:
    case CC0:
      return 0;

    case REG:
      /* We used to check RTX_UNCHANGING_P (x) here, but that is invalid
	 since the reg might be set by initialization within the loop.  */
      if (x == frame_pointer_rtx || x == arg_pointer_rtx)
	return 1;
      if (n_times_set[REGNO (x)] == -1)
	return 2;
      return n_times_set[REGNO (x)] == 0;

    case MEM:
      /* Constants in the constant pool are invariant.
	 ?? Really we should detect any constant address in the
	 text section.  */
      if (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
	  && CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)))
	return 1;
      /* A store in a varying-address scalar (or a subroutine call)
	 could clobber anything in memory.  */
      if (unknown_address_altered)
	return 0;
      /* Don't mess with volatile memory references.  */
      if (MEM_VOLATILE_P (x))
	return 0;
#if 0
      /* If it's declared read-only, it is invariant
	 if its address is invariant.  */
      if (RTX_UNCHANGING_P (x))
	return invariant_p (XEXP (x, 0));
#endif
      /* A store in a varying-address aggregate component
	 could clobber anything except a scalar with a fixed address.  */
      if (unknown_aggregate_altered
	  && ((MEM_IN_STRUCT_P (x) || GET_CODE (XEXP (x, 0)) == PLUS)
	      || rtx_addr_varies_p (x)))
	return 0;
      /* A store in a fixed-address aggregate component
	 could clobber anything whose address is not fixed,
	 even an aggregate component.  */
      if (fixed_aggregate_altered
	  && rtx_addr_varies_p (x))
	return 0;
      /* Any store could clobber a varying-address scalar.  */
      if (loop_store_addrs_idx
	  && !(MEM_IN_STRUCT_P (x) || GET_CODE (XEXP (x, 0)) == PLUS)
	  && rtx_addr_varies_p (x))
	return 0;
      /* A store in a fixed address clobbers overlapping references.  */
      for (i = loop_store_addrs_idx - 1; i >= 0; i--)
	if (addr_overlap_p (x, loop_store_addrs[i], loop_store_widths[i]))
	  return 0;
      /* It's not invalidated by a store in memory
	 but we must still verify the address is invariant.  */
      break;

    case ASM_OPERANDS:
      /* Don't mess with insns declared volatile.  */
      if (MEM_VOLATILE_P (x))
	return 0;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	{
	  int tem = invariant_p (XEXP (x, i));
	  if (tem == 0)
	    return 0;
	  if (tem == 2)
	    conditional = 1;
	}
      else if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      int tem = invariant_p (XVECEXP (x, i, j));
	      if (tem == 0)
		return 0;
	      if (tem == 2)
		conditional = 1;
	    }

	}
    }

  return 1 + conditional;
}

/* Return 1 if OTHER (a mem ref) overlaps the area of memory
   which is SIZE bytes starting at BASE.  */

int
addr_overlap_p (other, base, size)
     rtx other;
     rtx base;
     int size;
{
  int start = 0, end;

  if (GET_CODE (base) == CONST)
    base = XEXP (base, 0);
  if (GET_CODE (base) == PLUS
      && GET_CODE (XEXP (base, 1)) == CONST_INT)
    {
      start = INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);
    }

  end = start + size;
  return refers_to_mem_p (other, base, start, end);
}

/* Return nonzero if all the insns in the loop that set REG
   are INSN and the immediately following insns,
   and if each of those insns sets REG in an invariant way
   (not counting uses of REG in them).

   The value is 2 if some of these insns are only conditionally invariant.

   We assume that INSN itself is the first set of REG
   and that its source is invariant.  */

static int
consec_sets_invariant_p (reg, n_sets, insn)
     int n_sets;
     rtx reg, insn;
{
  register rtx p = insn;
  register int regno = REGNO (reg);
  rtx temp;
  /* Number of sets we have to insist on finding after INSN.  */
  int count = n_sets - 1;
  int old = n_times_set[regno];
  int value = 0;
  int this;

  /* If N_SETS hit the limit, we can't rely on its value.  */
  if (n_sets == 127)
    return 0;

  n_times_set[regno] = 0;

  while (count > 0)
    {
      register enum rtx_code code;
      p = NEXT_INSN (p);
      code = GET_CODE (p);

      /* If library call, skip to end of of it.  */
      if (code == INSN && (temp = find_reg_note (p, REG_LIBCALL, 0)))
	p = XEXP (temp, 0);

      this = 0;
      if (code == INSN && GET_CODE (PATTERN (p)) == SET
	  && GET_CODE (SET_DEST (PATTERN (p))) == REG
	  && REGNO (SET_DEST (PATTERN (p))) == regno)
	{
	  this = invariant_p (SET_SRC (PATTERN (p)));
	  if (this != 0)
	    value |= this;
	  else if (temp = loop_find_reg_equal (p))
	    {
	      this = invariant_p (XEXP (temp, 0));
	      if (this != 0)
		value |= this;
	    }
	}
      if (this != 0)
	count--;
      else if (code != NOTE)
	{
	  n_times_set[regno] = old;
	  return 0;
	}
    }

  n_times_set[regno] = old;
  /* If invariant_p ever returned 2, we return 2.  */
  return 1 + (value & 2);
}

#if 0
/* I don't think this condition is sufficient to allow INSN
   to be moved, so we no longer test it.  */

/* Return 1 if all insns in the basic block of INSN and following INSN
   that set REG are invariant according to TABLE.  */

static int
all_sets_invariant_p (reg, insn, table)
     rtx reg, insn;
     short *table;
{
  register rtx p = insn;
  register int regno = REGNO (reg);

  while (1)
    {
      register enum rtx_code code;
      p = NEXT_INSN (p);
      code = GET_CODE (p);
      if (code == CODE_LABEL || code == JUMP_INSN)
	return 1;
      if (code == INSN && GET_CODE (PATTERN (p)) == SET
	  && GET_CODE (SET_DEST (PATTERN (p))) == REG
	  && REGNO (SET_DEST (PATTERN (p))) == regno)
	{
	  if (!invariant_p (SET_SRC (PATTERN (p)), table))
	    return 0;
	}
    }
}
#endif /* 0 */

/* Increment N_TIMES_SET at the index of each register
   that is modified by an insn between FROM and TO.
   If the value of an element of N_TIMES_SET becomes 127 or more,
   stop incrementing it, to avoid overflow.

   Store in *COUNT_PTR the number of actual instruction
   in the loop.  We use this to decide what is worth moving out.  */

/* last_set[n] is nonzero iff reg n has been set in the current basic block.
   In that case, it is the insn that last set reg n.  */

static void
count_loop_regs_set (from, to, may_not_move, count_ptr, nregs)
     register rtx from, to;
     char *may_not_move;
     int *count_ptr;
     int nregs;
{
  register rtx *last_set = (rtx *) alloca (nregs * sizeof (rtx));
  register rtx insn;
  register int count = 0;
  register rtx dest;

  bzero (last_set, nregs * sizeof (rtx));
  for (insn = from; insn != to; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == CALL_INSN)
	{
	  /* If a register is used as a subroutine address,
	     don't allow this register's setting to be moved out of the loop.
	     This condition is not at all logically correct
	     but it averts a very common lossage pattern
	     and creates lossage much less often.  */
	  if (GET_CODE (PATTERN (insn)) == CALL
	      && GET_CODE (XEXP (PATTERN (insn), 0)) == MEM
	      && GET_CODE (XEXP (XEXP (PATTERN (insn), 0), 0)) == REG)
	    {
	      register int regno
		= REGNO (XEXP (XEXP (PATTERN (insn), 0), 0));
	      may_not_move[regno] = 1;
	    }
	  else if (GET_CODE (PATTERN (insn)) == SET
	      && GET_CODE (SET_SRC (PATTERN (insn))) == CALL
	      && GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 0)) == MEM
	      && GET_CODE (XEXP (XEXP (SET_SRC (PATTERN (insn)), 0), 0)) == REG)
	    {
	      register int regno
		= REGNO (XEXP (XEXP (SET_SRC (PATTERN (insn)), 0), 0));
	      may_not_move[regno] = 1;
	      /* The call insn itself sets a reg, which cannot be moved.  */
	      may_not_move[REGNO (SET_DEST (PATTERN (insn)))] = 1;
	      if (n_times_set[REGNO (SET_DEST (PATTERN (insn)))] < 127)
		n_times_set[REGNO (SET_DEST (PATTERN (insn)))]++;
	    }
	}
      if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN) 
	{
	  ++count;
	  if (GET_CODE (PATTERN (insn)) == CLOBBER
	      && GET_CODE (XEXP (PATTERN (insn), 0)) == REG)
	    /* Don't move a reg that has an explicit clobber.
	       We might do so sometimes, but it's not worth the pain.  */
	    may_not_move[REGNO (XEXP (PATTERN (insn), 0))] = 1;
	  else if (GET_CODE (PATTERN (insn)) == SET)
	    {
	      dest = SET_DEST (PATTERN (insn));
	      while (GET_CODE (dest) == SUBREG
		     || GET_CODE (dest) == ZERO_EXTRACT
		     || GET_CODE (dest) == SIGN_EXTRACT
		     || GET_CODE (dest) == STRICT_LOW_PART)
		dest = XEXP (dest, 0);
	      if (GET_CODE (dest) == REG)
		{
		  register int regno = REGNO (dest);
		  /* If this is the first setting of this reg
		     in current basic block, and it was set before,
		     it must be set in two basic blocks, so it cannot
		     be moved out of the loop.  */
		  if (n_times_set[regno] > 0 && last_set[regno] == 0)
		    may_not_move[regno] = 1;
		  /* If this is not first setting in current basic block,
		     see if reg was used in between previous one and this.
		     If so, neither one can be moved.  */
		  if (last_set[regno] != 0
		      && reg_used_between_p (dest, last_set[regno], insn))
		    may_not_move[regno] = 1;
		  if (n_times_set[regno] < 127)
		    ++n_times_set[regno];
		  last_set[regno] = insn;
		}
	    }
	  else if (GET_CODE (PATTERN (insn)) == PARALLEL)
	    {
	      register int i;
	      for (i = XVECLEN (PATTERN (insn), 0) - 1; i >= 0; i--)
		{
		  register rtx x = XVECEXP (PATTERN (insn), 0, i);
		  if (GET_CODE (x) == CLOBBER && GET_CODE (XEXP (x, 0)) == REG)
		    /* Don't move a reg that has an explicit clobber.
		       It's not worth the pain to try to do it correctly.  */
		    may_not_move[REGNO (XEXP (x, 0))] = 1;
		  if (GET_CODE (x) == SET)
		    {
		      dest = SET_DEST (x);
		      while (GET_CODE (dest) == SUBREG
			     || GET_CODE (dest) == ZERO_EXTRACT
			     || GET_CODE (dest) == SIGN_EXTRACT
			     || GET_CODE (dest) == STRICT_LOW_PART)
			dest = XEXP (dest, 0);
		      if (GET_CODE (dest) == REG)
			{
			  register int regno = REGNO (dest);
			  if (n_times_set[regno] < 127)
			    ++n_times_set[regno];
			  may_not_move[regno] = 1;
			  last_set[regno] = insn;
			}
		    }
		}
	    }
	}
      if (GET_CODE (insn) == CODE_LABEL || GET_CODE (insn) == JUMP_INSN)
	bzero (last_set, nregs * sizeof (rtx));
    }
  *count_ptr = count;
}

/* Given a loop that is bounded by LOOP_START and LOOP_END
   and that is entered at SCAN_START,
   return 1 if the register set by insn INSN is used by
   any insn that precedes INSN in cyclic order starting
   from the loop entry point.  */

static int
loop_reg_used_before_p (insn, loop_start, scan_start, loop_end)
     rtx insn, loop_start, scan_start, loop_end;
{
  rtx reg = SET_DEST (PATTERN (insn));
  if (INSN_LUID (scan_start) > INSN_LUID (insn))
    return (reg_used_between_p (reg, scan_start, loop_end)
	    || reg_used_between_p (reg, loop_start, insn));
  else
    return reg_used_between_p (reg, scan_start, insn);
}

/* A "basic induction variable" or biv is a pseudo reg that is set
   (within this loop) only by incrementing or decrementing it.  */
/* A "general induction variable" or giv is a pseudo reg whose
   value is a linear function of a biv.  */

/* Bivs are recognized by `basic_induction_var';
   Givs by `general_induct_var'.  */

/* An enum for the two different types of givs, those that are used
   as memory addresses and those that are calculated into registers.  */
enum g_types { DEST_ADDR, DEST_REG };

/* A `struct induction' is created for every instruction that sets
   an induction variable (either a biv or a giv).  */

struct induction
{
  rtx insn;		       /* The insn that sets a biv or giv */
  rtx new_reg;		       /* New register, containing strength reduced
				  version of this giv.  */
  int src_regno;	       /* Biv from which this giv is computed.
				  (If this is a biv, then this is the biv.)  */
  enum g_types giv_type;       /* Indicate whether DEST_ADDR or DEST_REG giv */
  int dest_regno;	       /* Destination register for insn: this is the
				  register which was the biv or giv.
				  For a biv, this equals src_reg.
				  For a DEST_ADDR type giv, this is 0.  */
  rtx *location;	       /* Place in the insn where this giv occurs.
				  If GIV_TYPE is DEST_REG, this is 0.  */
  enum machine_mode mode;      /* The mode of this biv or giv */
  rtx mult_val;		       /* Multiplicative factor for src_reg.  */
  rtx add_val;		       /* Additive constant for that product.  */
  int benefit;		       /* Gain from eliminating this insn.  */
  int consec;		       /* The number of consecutive insn that set this
				  register; they are all eliminated if this
				  one is.  */
  char replaceable;	       /* 1 if we can substitute the strength-reduced
				  variable for the original variable.
				  0 means they must be kept separate and the
				  new one must be copied into the old pseudo
				  reg each time the old one is set.  */
  char ignore;		       /* 1 prohibits further processing of this giv */
  int lifetime;		       /* Length of life of this giv */
  int times_used;	       /* # times this giv is used. */
  struct induction *family;    /* Links together all induction variables that
				  have the same src register.  */
  struct induction *forces;    /* Points to an induction variable insn which
				  is used only once, to compute this giv,
				  and hence can be deleted if this insn is
				  strength reduced.  */
  struct induction *forces2;   /* Likewise.  */
  struct induction *same;      /* Links together all induction variables that
				  have the same tuple (src, mult, add).  */
};

/* A `struct iv_class' is created for each biv.  */

struct iv_class {
  int regno;                   /* Pseudo reg which is the biv.  */
  int biv_count;               /* Number of insns setting this reg.  */
  struct induction *biv;       /* List of all insns that set this reg.  */
  int giv_count;               /* Number of DEST_REG givs computed from this
				  biv.  The resulting count is only used in
 				  check_dbra_loop.  */
  struct induction *giv;       /* List of all insns that compute a giv
				  from this reg.  */
  int total_benefit;           /* Sum of BENEFITs of all those givs */
  rtx initial_value;           /* Value of reg at loop start */
  struct iv_class *next;       /* Links all class structures together */
  rtx init_insn;	       /* insn which intializes biv, 0 if none seen. */
  char eliminable;	       /* 1 if plausible candidate for elimination.  */
  char nonneg;		       /* 1 if we added a REG_NONNEG note for this.  */
};

/* Definitions used by the basic induction variable discovery code.  */
enum iv_mode { UNKNOWN_INDUCT, BASIC_INDUCT, NOT_BASIC_INDUCT,
		 GENERAL_INDUCT };

/* Relative gain of eliminating various kinds of operations.  */
#define NO_BENEFIT    0
#define ADD_BENEFIT   1
#define SHIFT_BENEFIT 2
#define MULT_BENEFIT  4
#define LIBCALL_BENEFIT 15
/* Benefit penalty, if a giv is not replaceable, i.e. must emit an insn to
   copy the value of the strength reduced giv to its original register.  */
#define COPY_PENALTY  2

/* Indexed by register number, indicates whether or not register is an
   induction variable, and if so what type.  */

static enum iv_mode *induct_var;

/* Indexed by register number, contains pointer to `struct induction'
   if register is a general induction variable.  */

static struct induction **induct_struct;

/* Indexed by register number, contains pointer to `struct iv_class'
   if register is a basic induction variable.  */

static struct iv_class **class_struct;

/*********************************/

/* ??? Unfinished optimizations, wilson@ji.Berkeley.EDU */

/* strength reduce addresses found in sources (set () (mem ())*/

/* There is one more optimization you might be interested in doing: to
   allocate pseudo registers for frequently-accessed memory locations.
   If the same memory location is referenced each time around, it might
   be possible to copy it into a register before and out after.
   This is especially useful when the memory location is a variable which
   is in a stack slot because somewhere its address is taken.  If the
   loop doesn't contain a function call and the variable isn't volatile,
   it is safe to keep the value in a register for the duration of the
   loop. One tricky thing is that the copying of the value back from the
   register has to be done on all exits from the loop.  You need to check that
   all the exits from the loop go to the same place. */

/* WARNING: the interaction of biv elimination, and recognizing 'constant'
   bivs may cause problems */

/* add heuristic so that DEST_ADDR strength reduction does not cause
   performance problems */

/* don't eliminate things that can be combined with an addressing mode?
   find all giv that have same biv and mult_val (now must also have
   same add_val), then for each giv, check to see if its only use
   dies in a following memory address, generate a new memory address
   and check to see if valid, if valid then store modified mem addr,
   else if not valid addr mark giv as not done so that it will get its
   own iv */

/* consec_sets_giv does not calculate replaceable and forces correctly,
   forces should be a more general linked list instead of two entries */

/* try to optimize branches when it is known that a biv is always positive */

/* when replace biv in compare insn, should replace with closest giv so that
   an optimized branch can still be recognized by combiner, i.e. VAXen acb */

/* should merge final_value calculation in check_dbra_loop with the 
   new final_biv_value function */

/* many of the checks involving uid_luid could be simplified if regscan
   was rerun in loop_optimize() whenever a register was added or moved,
   also some of the optimizations could be a little less conservative */

/* Perform strength reduction and induction variable elimination.  */

/* Pseudo registers created during this function will be beyond the last
   valid index in several tables including n_times_set and regno_last_uid.
   This does not cause a problem here, because the added registers cannot be
   givs outside of their loop, and hence will never be reconsidered.
   But scan_loop must check regnos to make sure they are in bounds.  */

static void
strength_reduce (scan_start, end, loop_top, insn_count,
		 loop_start, loop_end, nregs)
     rtx scan_start;
     rtx end;
     rtx loop_top;
     int insn_count;
     rtx loop_start;
     rtx loop_end;
     int nregs;
{
  rtx p;
  rtx inc_val;
  rtx mult_val;
  int dest_regno;
  int biv_found;
  /* This is 1 if current insn could be executed zero times in the loop.  */
  int maybe_never = 0;
  /* List of all possible basic induction variables.  */
  struct iv_class *iv_list = 0;
  /* Temporary list pointers for traversing iv_list.  */
  struct iv_class *bl, *backbl;
  /* Ratio of extra register life span we can justify
     for saving an instruction.  More if loop doesn't call subroutines
     since in that case saving an insn makes more difference
     and more registers are available.  */
  /* ??? could set this to last value of threshold in move_movables */
  int threshold = loop_has_call ? 17 : 34;
  /* Map of pseudo-register replacements.  */
  rtx *reg_map;
  int call_seen;

  induct_var = (enum iv_mode *) alloca (nregs * sizeof (induct_var[0]));
  bzero ((char *)induct_var, nregs * sizeof (induct_var[0]));
  induct_struct = (struct induction **)
    alloca (nregs * sizeof (struct induction *));
  bzero ((char *)induct_struct, nregs * sizeof (struct induction *));
  class_struct = (struct iv_class **)
    alloca (nregs * sizeof (struct iv_class *));
  bzero ((char *)class_struct, nregs * sizeof (struct iv_class *));

  /* Scan through loop to find all possible bivs.  */

  for (p = NEXT_INSN (loop_start); p != end; p = NEXT_INSN (p))
    {
      if (GET_CODE (p) == INSN
	  && GET_CODE (PATTERN (p)) == SET
	  && GET_CODE (SET_DEST (PATTERN (p))) == REG)
	{
	  dest_regno = REGNO (SET_DEST (PATTERN (p)));
	  if (induct_var[dest_regno] != NOT_BASIC_INDUCT
	      && dest_regno >= FIRST_PSEUDO_REGISTER)
	    {
	      if (basic_induction_var (SET_SRC (PATTERN (p)), dest_regno,
				      &inc_val, &mult_val))
		{
		  /* It is a possible basic induction variable.
		     Create and initialize an induction structure for it.  */

		  struct induction *v =
		    (struct induction *) alloca (sizeof (struct induction));

		  v->insn = p;
		  v->src_regno = dest_regno;
		  v->dest_regno = dest_regno;
		  v->mult_val = mult_val;
		  v->add_val = inc_val;
		  v->mode = GET_MODE (SET_DEST (PATTERN (p)));

		  /* Add this to the reg's iv_class, creating a class
		     if this is the first incrementation of the reg.  */

		  bl = class_struct[dest_regno];
		  if (bl)
		    {
		      v->family = bl->biv;
		      bl->biv = v;
		      bl->biv_count++;
		    }
		  else
		    {
		      /* Create and initialize new iv_class.  */

		      bl = (struct iv_class *) alloca (sizeof (struct iv_class));

		      bl->regno = dest_regno;
		      bl->biv = v;
		      v->family = 0;
		      bl->giv = 0;
		      bl->biv_count = 1;
		      bl->giv_count = 0;

		      /* Set initial value to the reg itself.  */
		      bl->initial_value = SET_DEST (PATTERN (p));
		      /* We haven't seen the intializing insn yet */
		      bl->init_insn = 0;
		      bl->eliminable = 0;
		      bl->nonneg = 0;

		      /* Add this insn to iv_list.  */
		      bl->next = iv_list;
		      iv_list = bl;

		      /* Put it in the array of iv_lists.  */
		      class_struct[dest_regno] = bl;
		    }

		  induct_var[dest_regno] = BASIC_INDUCT;

		  if (loop_dump_stream)
		    {
		      fprintf (loop_dump_stream,
			       "Insn %d: possible biv, reg %d,",
			       INSN_UID (p), dest_regno);
		      if (GET_CODE (inc_val) == CONST_INT)
			fprintf (loop_dump_stream, " const = %d\n",
				 INTVAL (inc_val));
		      else
			{
			  fprintf (loop_dump_stream, " const = ");
			  print_rtl (loop_dump_stream, inc_val);
			  fprintf (loop_dump_stream, "\n");
			}
		    }
		}
	      else
		induct_var[dest_regno] = NOT_BASIC_INDUCT;
	    }
	}
    }

  /* Scan iv_list to remove all regs that proved not to be bivs.
     Make a sanity check against n_times_set.  */

  biv_found = 0;

  for (backbl = bl = iv_list; bl; backbl = bl, bl = bl->next)
    {
      if (induct_var[bl->regno] != BASIC_INDUCT)
	{
	  /* Not a basic induction variable, remove this iv_class.  */

	  if (backbl == bl)
	    iv_list = bl->next;
	  else
	    backbl->next = bl->next;

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv discarded, not induct\n",
		    bl->regno);
	}
      else if (n_times_set[bl->regno] != bl->biv_count)
	{
	  /* This happens if register modified by subreg, etc.  */
	  /* Make sure it is not recognized as a basic induction var: */
	  /* remove this iv_class from iv_list.  */

	  induct_var[bl->regno] = NOT_BASIC_INDUCT;

	  if (backbl == bl)
	    iv_list = bl->next;
	  else
	    backbl->next = bl->next;

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv discarded, count error\n",
		    bl->regno);
	}
      else
	{
	  /* This is a valid basic induction variable.  */

	  biv_found++;

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv verified\n", bl->regno);
	}
    }

  /* Exit if there are no bivs.  */
  if (!iv_list)
    return;

  /* Find initial value for each biv.  */
  /* Search backwards from loop_start, halting at first label
     or when all bivs have been seen.  */

  call_seen = 0;
  p = loop_start;
  while (biv_found)
    {
      p = PREV_INSN (p);
      if (p == 0)
	break;

      if (GET_CODE (p) == CALL_INSN)
	call_seen = 1;

      if (GET_CODE (p) == INSN
	  && GET_CODE (PATTERN (p)) == SET)
	{
	  rtx dest = SET_DEST (PATTERN (p));

	  while (GET_CODE (dest) == SUBREG
		 || GET_CODE (dest) == ZERO_EXTRACT
		 || GET_CODE (dest) == SIGN_EXTRACT
		 || GET_CODE (dest) == STRICT_LOW_PART)
	    dest = XEXP (dest, 0);

	  if (GET_CODE (dest) == REG)
	    {
	      int dest_regno = REGNO (dest);
	      if (induct_var[dest_regno] == BASIC_INDUCT
		  && class_struct[dest_regno]->init_insn == 0)
		{
		  /* This is the first modification found for this reg.  */

		  rtx src = SET_SRC (PATTERN (p));

		  /* Record the intializing INSN */

		  class_struct[dest_regno]->init_insn = p;

		  if (loop_dump_stream)
		    fprintf (loop_dump_stream, "Biv %d initialized at insn %d: ",
			     dest_regno, INSN_UID (p));

		  /* Save value if it is a constant or register.  */
		  if (CONSTANT_P (src)
		      || (GET_CODE (src) == REG
			  /* Don't try to use a value in a hard reg
			     across a call which clobbers it.  */
			  && ! (REGNO (src) < FIRST_PSEUDO_REGISTER
				&& call_used_regs[REGNO (src)]
				&& call_seen)
			  && ! reg_set_between_p (src, p, loop_start)))
		    {
		      class_struct[dest_regno]->initial_value = src;

		      if (loop_dump_stream)
			fprintf (loop_dump_stream, "initial value ");
		      if (loop_dump_stream)
			{
			  if (GET_CODE (src) == CONST_INT)
			    fprintf (loop_dump_stream, "%d\n", INTVAL (src));
			  else
			    {
			      print_rtl (loop_dump_stream, src);
			      fprintf (loop_dump_stream, "\n");
			    }
			}
		    }
		  else
		    {
		      /* Biv initial value is not simple move,
			 so let it keep intial value of "itself".  */

		      if (loop_dump_stream)
			fprintf (loop_dump_stream, "complex initial value\n");
		    }

		  biv_found--;
		}
	    }
	}
      else if (GET_CODE (p) == CODE_LABEL)
	break;
    }

  /* Search the loop for general induction variables.  */

  /* A register is a giv if: it is only set once, it is a function of a
     biv and a constant (or invariant), and it is not a biv.  */

  p = scan_start;
  while (1)
    {
      p = NEXT_INSN (p);
      /* At end of a straight-in loop, we are done.
	 At end of a loop entered at the bottom, scan the top.  */
      if (p == scan_start)
	break;
      if (p == end)
	{
	  if (loop_top != 0)
	    p = NEXT_INSN (loop_top);
	  else
	    break;
	  if (p == scan_start)
	    break;
	}

      /* Look for a general induction variable in a register.  */
      if (GET_CODE (p) == INSN
	  && GET_CODE (PATTERN (p)) == SET
	  && GET_CODE (SET_DEST (PATTERN (p))) == REG
	  && ! may_not_optimize[REGNO (SET_DEST (PATTERN (p)))])
	{
	  int src_regno;
	  rtx add_val;
	  rtx mult_val;
	  int benefit;
	  rtx regnote = 0;
	  struct induction *forces = 0;
	  struct induction *forces2 = 0;

	  dest_regno = REGNO (SET_DEST (PATTERN (p)));
	  if (dest_regno < FIRST_PSEUDO_REGISTER)
	    continue;

	  if (/* Normal giv.  */
	      ((benefit = general_induction_var (SET_SRC (PATTERN (p)),
						 &src_regno, &add_val,
						 &mult_val,
						 &forces, &forces2))
	       /* Giv set with call to a library routine.  */
	       || ((regnote = loop_find_reg_equal (p))
		   &&
		   (benefit = general_induction_var (XEXP (regnote, 0),
						     &src_regno,
						     &add_val, &mult_val,
						     &forces, &forces2))))
	      /* Don't try to handle any regs made by loop optimization.
		 We have nothing on them in regno_first_uid, etc.  */
	      && dest_regno < old_max_reg
	      /* Don't recognize a BASIC_INDUCT_VAR here.  */
	      && dest_regno != src_regno
	      /* This must be the only place where the register is set.  */
	      && (n_times_set[dest_regno] == 1
		  || (benefit = consec_sets_giv (benefit, p,
						 src_regno, dest_regno,
						 &add_val, &mult_val))))
	    {
	      int count;
	      struct induction *v =
		(struct induction *) alloca (sizeof (struct induction));
	      rtx temp;

	      record_giv (v, p, src_regno, dest_regno, mult_val, add_val, benefit,
			  forces, forces2, DEST_REG, maybe_never, 0, loop_end);

	      /* Skip the consecutive insns, if there are any.  */
	      for (count = v->consec - 1; count >= 0; count--)
		{
		  /* If first insn of libcall sequence, skip to end.  */
		  /* Do this at start of loop, since INSN is guaranteed to
		     be an insn here.  */
		  if (temp = find_reg_note (p, REG_LIBCALL, 0))
		    {
		      /* Eliminating a libcall does more good than
			 eliminating a single insn to do the same job.  */
		      benefit += LIBCALL_BENEFIT;
		      p = XEXP (temp, 0);
		    }

		  do p = NEXT_INSN (p);
		  while (GET_CODE (p) == NOTE);
		}
	    }
	}

#ifndef DONT_REDUCE_ADDR
      /* Look for givs which are memory addresses.  */
      /* This resulted in worse code on a VAX 8600.  I wonder if it
	 still does.  */
      if (GET_CODE (p) == INSN)
	find_mem_givs (PATTERN (p), p, maybe_never, loop_end);
#endif

      /* Past a label or a jump, we get to insns for which we can't count
	 on whether or how many times they will be executed during each
	 iteration.  Givs found afterwards cannot be marked replaceable.  */
      if (GET_CODE (p) == CODE_LABEL || GET_CODE (p) == JUMP_INSN)
	maybe_never = 1;
    }

  /* Try to prove that the loop counter variable (if any) is always
     nonnegative; if so, record that fact with a REG_NONNEG note
     so that "decrement and branch until zero" insn can be used.  */
  check_dbra_loop (loop_end, iv_list, insn_count, loop_start);

  /* Create reg_map to hold substitutions for replaceable giv regs.  */
  reg_map = (rtx *) alloca (nregs * sizeof (rtx));
  bzero ((char *)reg_map, nregs * sizeof (rtx));

  /* Examine each iv class for feasibility of strength reduction/induction
     variable elimination.  */

  for (bl = iv_list; bl; bl = bl->next)
    {
      struct induction *v;
      int benefit;
      int replaceable;
      int all_reduced;
      rtx final_value = 0;

      /* Test whether it will be possible to eliminate this biv
	 provided all givs are reduced.  This is possible if either
	 the reg is not used outside the loop, or we can compute
	 what its final value will be.

	 Don't try if we put a REG_NONNEG note on the endtest for this biv.
	 ??? That should be only on machines that have dbra insns.  */

      /* Compare against bl->init_insn rather than loop_start.
	 We aren't concerned with any uses of the biv between
	 init_insn and loop_start since these won't be affected
	 by the value of the biv elsewhere in the function, so
	 long as init_insn doesn't use the biv itself.
	 March 14, 1989 -- self@bayes.arc.nasa.gov */

      if ((uid_luid[regno_last_uid[bl->regno]] < INSN_LUID (loop_end)
	   && bl->init_insn
	   && INSN_UID (bl->init_insn) < max_uid
	   && uid_luid[regno_first_uid[bl->regno]] >= INSN_LUID (bl->init_insn)
	   && ! reg_mentioned_p (SET_DEST (PATTERN (bl->biv->insn)),
				 SET_SRC (PATTERN (bl->init_insn)))
	   && ! bl->nonneg)
	  || (final_value = final_biv_value (bl, loop_end)))
	check_eliminate_biv (bl, loop_start, end);
      else
	{
	  if (loop_dump_stream)
	    {
	      fprintf (loop_dump_stream,
		       "Cannot eliminate biv %d.\n",
		       bl->regno);
	      fprintf (loop_dump_stream,
		       "First use: insn %d, last use: insn %d.\n",
		       regno_first_uid[bl->regno],
		       regno_last_uid[bl->regno]);
	    }
	}

      /* This will be true at the end, if all givs which depend on this
	 biv have been strength reduced.
	 We can't (currently) eliminate the biv unless this is so.  */
      all_reduced = 1;

      /* Check each giv in this class.  */

      for (v = bl->giv; v; v = v->family)
	{
	  struct induction *tv;

	  if (v->ignore)
	    continue;

	  benefit = v->benefit;
	  replaceable = v->replaceable;

	  /* Reduce benefit if not replaceable, since we will insert
	     a move-insn to replace the insn that calculates this giv.  */
	  if (!replaceable && ! bl->eliminable)
	    benefit -= COPY_PENALTY;

	  /* Decrease the benefit to count the add-insns that we will
	     insert to increment the reduced reg for the giv.  */
	  benefit -= ADD_BENEFIT * bl->biv_count;

	  /* Find all equivalent givs (that bear same relation to the biv).
	     Link them via the `same' field and add their benefits together.
	     They can be replaced with a single register.  */

	  for (tv = v->family; tv; tv = tv->family)
	    {
	      if (tv->ignore == 0
		  && tv->src_regno == v->src_regno
		  && rtx_equal_p (tv->mult_val, v->mult_val)
		  && rtx_equal_p (tv->add_val, v->add_val))
		{
		  benefit += tv->benefit;
		  if (! tv->replaceable)
		    benefit -= COPY_PENALTY;
		  v->lifetime += tv->lifetime;
		  v->times_used += tv->times_used;
		  tv->ignore = 1;

		  /* Link them together via `same' field.  */
		  tv->same = v->same;
		  v->same = tv;

		  if (loop_dump_stream)
		    fprintf (loop_dump_stream,
			     "giv of insn %d combined with that of %d.\n",
			     INSN_UID (v->insn), INSN_UID (tv->insn));
		}
	    }

	  /* Decide whether to strength-reduce this giv
	     or to leave the code unchanged
	     (recompute it from the biv each time it is used).
	     This decision can be made independently for each giv.  */

	  /* ??? Perhaps attempt to guess whether autoincrement will handle
	     some of the new add insns; if so, can increase BENEFIT
	     (undo the subtraction of ADD_BENEFIT that was done above).  */

	  /* If an insn is not to be strength reduced, then set its ignore
	     flag, and clear all_reduced.  */

	  /* Is it right to consider times_used?  */

	  /* ??? What about the insns that are 'forced' by this one?
	     Although this insn is not worthwhile to reduce, it may be
	     worthwhile to reduce the simpler givs used to compute this 
	     complex giv.  */

	  /* ??? Hey! If a giv has its forces field set, then that means
	     it is not computed directly from the biv, it is instead computed
	     from a simpler giv.  If we define UNFORCE_INSNS, then the simpler
	     giv will be considered for strength reduction, and this giv should
	     not cause all_reduced to be cleared because it DOESN'T use the
	     biv!!!  If the simpler giv can not be reduced, then that simpler
	     biv will still cause all_reduced to be cleared.  */

	  if (benefit <= 0)
	    {
	      if (loop_dump_stream)
		fprintf (loop_dump_stream, "giv of insn %d, no benefit\n",
			 INSN_UID (v->insn));
	      v->ignore = 1;
	      all_reduced = 0;
	    }

	  if (v->lifetime * threshold * benefit < insn_count)
	    {
	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "giv of insn %d not worth while, %d vs %d.\n",
			 INSN_UID (v->insn),
			 v->lifetime * threshold * benefit, insn_count);
	      v->ignore = 1;
	      all_reduced = 0;
	    }

	  /* Now check that we can increment the reduced giv
	     without needing a multiply insn.  If not, reject it.  */

	  if (! v->ignore)
	    {
	      int success = 1;

	      for (tv = bl->biv; tv; tv = tv->family)
		if (tv->mult_val == const1_rtx)
		  success &= product_cheap_p (tv->add_val, v->mult_val);

	      if (! success)
		{
		  if (loop_dump_stream)
		    fprintf (loop_dump_stream,
			     "giv of insn %d: would need a multiply.\n",
			     INSN_UID (v->insn));
		  v->ignore = 1;
		  all_reduced = 0;
		}
	    }
	}

      /* Reduce each giv that we decided to reduce.  */

      for (v = bl->giv; v; v = v->family)
	{
	  struct induction *tv;
	  if (! v->ignore)
	    {
	      rtx new_reg;

	      /* Note Iris compiler dies if ?: is used inside gen_reg_rtx. */
	      if (v->giv_type == DEST_ADDR)
	        new_reg = gen_reg_rtx (Pmode);
	      else
	        new_reg = gen_reg_rtx (GET_MODE (SET_DEST (PATTERN (v->insn))));

	      /* For each place where the biv is incremented,
		 add an insn to increment the new, reduced reg for the giv.
		 Insert it before the insn that sets the biv,
		 so that the biv increment remains last before the endtest,
		 so that dbra will still be recognized.  */

	      for (tv = bl->biv; tv; tv = tv->family)
		{
		  struct induction *iv;
		  rtx before_insn = tv->insn;

		  /* If this increment is between the setting of the giv and
		     its use, don't increment until after the use.  */
		  for (iv = v; iv; iv = iv->same)
		    {
		      if (INSN_LUID (tv->insn) <= INSN_LUID (iv->insn)
			  && ((iv->forces
			       && (INSN_LUID (tv->insn)
				   >= INSN_LUID (iv->forces->insn))
			      || (iv->forces2
				  && (INSN_LUID (tv->insn)
				      >= INSN_LUID (iv->forces2->insn))))))
			{
			  before_insn = NEXT_INSN (iv->insn);
			  break;
			}
		    }

		  if (tv->mult_val == const1_rtx)
		    emit_iv_inc (tv->add_val, v->mult_val,
				 new_reg, before_insn);
		  else /* tv->mult_val == const0_rtx */
		    /* A multiply is acceptable here
		       since this is presumed to be seldom executed.  */
		    emit_iv_init_code (tv->add_val, v->mult_val,
				       v->add_val, new_reg, before_insn);
		}

	      /* Add code at loop start to initialize giv's reduced reg.  */

	      emit_iv_init_code (bl->initial_value, v->mult_val,
				 v->add_val, new_reg, loop_start);
	      /* If the initial value uses a register,
		 then we may have just extended its range of appearance.
		 Update this conservatively for the sake of outer loops.  */
	      if (GET_CODE (bl->initial_value) == REG
		  && (uid_luid[regno_last_uid[REGNO (bl->initial_value)]]
		      < INSN_LUID (loop_start)))
		uid_luid[regno_last_uid[REGNO (bl->initial_value)]]
		  = INSN_LUID (loop_start);

	      /* For each giv register that can be reduced now:
		 delete old insn that modifies the giv,
		 if replaceable, substitute reduced reg
		   wherever the old giv occurs;
		 else add new move insn "giv_reg = reduced_reg".  */

	      for (tv = v; tv; tv = tv->same)
		{
		  /* Record the identity of the reduced reg.  */
		  tv->new_reg = new_reg;

		  if (tv->giv_type == DEST_ADDR)
		    {
		      /* Store reduced reg as the address in the memref
			 where we found this giv.  */
		      * tv->location = new_reg;
		    }
		  else if (tv->replaceable)
		    {
		      reg_map[tv->dest_regno] = new_reg;
		      /* If giv lives after end of loop,
			 emit insn to copy reduced reg into old reg,
			 at the end of the loop.
			 ?? insufficient; used before loop could
			 mean live after loop, due to surrounding loop.  */
		      /* Currently a giv used outside
			 the loop will not be marked replaceable,
			 so these deficiencies don't really hurt.  */
		      if (uid_luid[regno_last_uid[tv->dest_regno]]
			  > uid_luid[INSN_UID (loop_end)])
			{
			  /* ?? This won't work.  We need to do this at
			     ALL exits.  */
			  emit_insn_after (gen_rtx (SET, VOIDmode,
						    SET_DEST (PATTERN (tv->insn)),
						    new_reg),
					   loop_end);
			  abort ();
			}
		    }
		  else
		    {
		      /* Not replaceable; emit an insn to set the
			 original giv reg from the reduced giv.  */

		      int count;
		      rtx after_insn = tv->insn;

		      for (count = tv->consec; count > 0; count--)
			after_insn = next_real_insn (after_insn);

		      /* Put new insn after, not before, in case
			 after_insn is the end of a libcall.  */
		      emit_insn_after (gen_rtx (SET, VOIDmode,
						SET_DEST (PATTERN (tv->insn)),
						new_reg),
				       after_insn);
		    }

		  /* Delete the insn that used to set the old giv reg,
		     unless we modified an address in it.
		     In any case, delete the other insns used for this one.  */
		  delete_insn_forces (tv, tv->giv_type != DEST_ADDR);

		  if (loop_dump_stream)
		    fprintf (loop_dump_stream, "giv at %d reduced to reg %d\n",
			     INSN_UID (tv->insn), REGNO (new_reg));
		}
	      /* One set of equivalent givs has been strength-reduced.  */
	    }
#if 0
	  else if (v->new_reg == 0)
	    {
	      /* This giv wasn't reduced and is not worth reducing.  */

	      for (tv = v; tv; tv = tv->same)
		if (loop_dump_stream)
		  fprintf (loop_dump_stream, "giv at %d not reduced\n",
			   INSN_UID (tv->insn));

	      all_reduced = 0;
	    }
#endif
	}

      /* All the givs in this family have been reduced if they merit it.  */

      /* Try to eliminate the biv, if it is a candidate.
	 This won't work if ! all_reduced,
	 since the givs we planned to use might not have been reduced.  */

      if (all_reduced == 1 && bl->eliminable)
	{
	  /* Get the REG rtx for the biv.  */
	  rtx reg = SET_DEST (PATTERN (bl->biv->insn));

	  for (p = loop_start; p != end; p = NEXT_INSN (p))
	    {
	      enum rtx_code code = GET_CODE (p);
	      if ((code == INSN || code == JUMP_INSN || code == CALL_INSN)
		  && reg_mentioned_p (reg, PATTERN (p))
		  && SET_DEST (PATTERN (p)) == cc0_rtx)
		/* Found a compare instruction using this biv;
		   rewrite it to use a related giv.  */
		{
		  struct induction *v1;
		  /* If this is an insn which uses the biv ONLY in the
		     calculation of a giv which is in the family of this
		     biv, it's ok becuase it will go away when the giv is
		     reduced.  */
		  for (v1 = bl->giv; v1; v1 = v1->family)
		    if (v1->insn == p)
		      {
			if (v1->giv_type == DEST_REG
			    || (v1->giv_type == DEST_ADDR
				/* I thought the test was backwards,
				   but then I found the real problem
				   was in the subroutine.  */
				&& ! other_reg_use_p (reg, *(v1->location),
						      PATTERN (p))))
			  break;
		      }
		  if (!v1)
		    eliminate_biv (p, bl, loop_start);
		}
	    }

	  /* Biv is no longer really needed inside the loop,
	     so delete all insns that set the biv.  */

	  for (v = bl->biv; v; v = v->family)
	    delete_insn (v->insn);

	  /* ?? If we created a new test to bypass the loop entirely,
	     or otherwise drop straight in, based on this test, then
	     we might want to rewrite it also.  This way some later
	     pass has more hope of removing the intialization of this
	     biv entirely. */

	  /* If final_value != 0, then biv may be used after loop end
	     and we must emit an insn to set it just in case.  */
	  if (final_value != 0)
	    emit_insn_after (gen_rtx (SET, VOIDmode, reg, final_value),
			     loop_end);

	  if (loop_dump_stream)
	    fprintf (loop_dump_stream, "Reg %d: biv eliminated\n",
		     bl->regno);
	}
    }

  /* Go through all the instructions in the loop, making all the
     register substitutions scheduled in REG_MAP.  */

  for (p = loop_start; p != end; p = NEXT_INSN (p))
    if (GET_CODE (p) == INSN || GET_CODE (p) == JUMP_INSN
 	|| GET_CODE (p) == CALL_INSN)
      replace_regs (PATTERN (p), reg_map, nregs);

  if (loop_dump_stream)
    fprintf (loop_dump_stream, "\n");
}

/* Nonzero if register REG appears somewhere within IN, other than in
   subexpressions EQ to EXPR.  This is a modification of reg_mentioned_p.  */

int
other_reg_use_p (reg, expr, in)
     register rtx reg, expr, in;
{
  register char *fmt;
  register int i;
  register enum rtx_code code;

  if (in == 0 || in == expr)
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
    case CONST_INT:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case CODE_LABEL:
      return 0;
    }

  fmt = GET_RTX_FORMAT (code);

  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = XVECLEN (in, i) - 1; j >= 0; j--)
	    if (other_reg_use_p (reg, expr, XVECEXP (in, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e'
	       && other_reg_use_p (reg, expr, XEXP (in, i)))
	return 1;
    }
  return 0;
}

/* Scan X for memory refs and check each memory address
   as a possible giv.  INSN is the insn whose pattern X comes from.
   MAYBE_NEVER is 1 if the loop might execute INSN zero times.  */

static void
find_mem_givs (x, insn, maybe_never, loop_end)
     rtx x;
     rtx insn;
     int maybe_never;
     rtx loop_end;
{
  register int i, j;
  register enum rtx_code code;
  register char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);
  switch (code)
    {
    case REG:
    case CONST_INT:
    case CONST:
    case CONST_DOUBLE:
    case SYMBOL_REF:
    case LABEL_REF:
    case PC:
    case CC0:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
    case USE:
    case CLOBBER:
      return;

    case MEM:
      {
	int src_regno;
	rtx add_val;
	rtx mult_val;
	int benefit;
	struct induction *forces = 0;
	struct induction *forces2 = 0;

	benefit = general_induction_var (XEXP (x, 0),
					 &src_regno, &add_val, &mult_val,
					 &forces, &forces2);
	if (benefit > 0)
	  {
	    /* Found one; record it.  */
	    struct induction *v =
	      (struct induction *) oballoc (sizeof (struct induction));

	    record_giv (v, insn, src_regno, 0, mult_val, add_val, benefit,
			forces, forces2, DEST_ADDR, maybe_never, &XEXP (x, 0),
			loop_end);
	  }
	return;
      }
    }

  /* Recursively scan the subexpressions for other mem refs.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    if (fmt[i] == 'e')
      find_mem_givs (XEXP (x, i), insn, maybe_never, loop_end);
    else if (fmt[i] == 'E')
      for (j = 0; j < XVECLEN (x, i); j++)
	find_mem_givs (XVECEXP (x, i, j), insn, maybe_never, loop_end);
}

/* Fill in the data about one giv.
   V is the `struct induction' in which we record the giv.  (It is
   allocated by the caller, with alloca.)
   INSN is the insn that sets it.
   BENEFIT estimates the savings from deleting this insn.
   TYPE is DEST_REG or DEST_ADDR; it says whether the giv is computed
   into a register or is used as a memory address.

   SRC_REGNO is the biv reg number which the giv is computed from.
   DEST_REGNO is the giv's reg number (if the giv is stored in a reg).
   MULT_VAL and ADD_VAL are the coefficients used to compute the giv.
   FORCES and FORCES2, if nonzero, are other `struct induction's for
   other givs which are used to compute this giv indirectly.
   LOCATION points to the place where this giv's value appears in INSN.  */

static void
record_giv (v, insn, src_regno, dest_regno, mult_val, add_val, benefit,
	    forces, forces2, type, maybe_never, location, loop_end)
     struct induction *v;
     rtx insn;
     int src_regno, dest_regno;
     rtx mult_val, add_val;
     int benefit;
     struct induction *forces, *forces2;
     enum g_types type;
     int maybe_never;
     rtx *location;
     rtx loop_end;
{
  struct induction *b;
  struct iv_class *bl;

  v->insn = insn;
  v->src_regno = src_regno;
  v->giv_type = type;
  v->dest_regno = dest_regno;
  v->mult_val = mult_val;
  v->add_val = add_val;
  v->benefit = benefit;
  v->location = location;

  if (type == DEST_ADDR)
    {
      v->mode = GET_MODE (*location);
      v->consec = 0;
      v->lifetime = 1;
      v->times_used = 1;
    }
  else /* type == DEST_REG */
    {
      v->mode = GET_MODE (SET_DEST (PATTERN (insn)));
      v->consec = n_times_set[dest_regno] - 1;
      v->lifetime = (uid_luid[regno_last_uid[dest_regno]]
		     - uid_luid[regno_first_uid[dest_regno]]);
      v->times_used = n_times_used[dest_regno];
    }

  v->same = 0;
  v->forces = 0;
  v->forces2 = 0;
  v->ignore = 0;
  v->new_reg = 0;

  /* Mark giv as forced if it is only used to compute another giv.  */

  /* This check is not sufficient as INSN may have been moved giving
     it a new uid, so make another check by calculating lifetimes.
     This is overconservative but seems to be correct.  */

  if (forces)
    {
      v->benefit += forces->benefit;
      if ((regno_last_uid[forces->dest_regno] == INSN_UID (insn)
	   ||
	   ((uid_luid[regno_last_uid[forces->dest_regno]]
	     - uid_luid[regno_first_uid[forces->dest_regno]])
	    == (INSN_LUID (insn) - INSN_LUID (forces->insn))))
	  && !reg_used_between_p (SET_DEST (PATTERN (forces->insn)),
				  forces->insn, insn))
 	{
	  v->forces = forces;
	  forces->ignore = 1;
 	}
    }

  if (forces2)
    {
      v->benefit += forces2->benefit;
      if ((regno_last_uid[forces2->dest_regno] == INSN_UID (insn)
	   ||
	   ((uid_luid[regno_last_uid[forces2->dest_regno]]
	     - uid_luid[regno_first_uid[forces2->dest_regno]])
	    == (INSN_LUID (insn) - INSN_LUID (forces2->insn))))
	  && !reg_used_between_p (SET_DEST (PATTERN (forces2->insn)),
				  forces2->insn, insn))
 	{
	  if (v->forces)
	    v->forces2 = forces2;
	  else
	    v->forces = forces2;
	  forces2->ignore = 1;
	}
    }

  if (type == DEST_REG)
    {
      induct_var[dest_regno] = GENERAL_INDUCT;
      induct_struct[dest_regno] = v;
    }

  /* Add the giv to the class of givs computed from one biv.  */

  bl = class_struct[src_regno];
  if (bl)
    {
      v->family = bl->giv;
      bl->giv = v;
      /* Don't count DEST_ADDR.  This is supposed to count the number of
	 insns that calculate givs.  */
      if (type == DEST_REG)
	bl->giv_count++;
      bl->total_benefit += benefit;
    }
  else
    /* Fatal error, biv missing for this giv?  */
    abort ();

  if (type == DEST_ADDR)
    v->replaceable = 1;
  else
    {
      /* The giv can be replaced outright by the reduced register if
 	 - the insn that sets the giv is always executed on any iteration
	   on which the giv is used at all
	   (there are two ways to deduce this:
	    either the insn is executed on every iteration,
	    or all uses follow that insn in the same basic block),
 	 - the giv is not used before the insn that sets it,
 	    i.e. no definition outside loop reaches into loop
	 - no assignments to the biv occur during the giv's lifetime.  */

      /* Is this right?  Don't we need to make sure the giv is not used
	 outside the loop.  Someday we will know where all the loop exits
	 are so we can do better, but until then....
	 March 18, 1989 -- self@bayes.arc.nasa.gov */

      if (regno_first_uid[dest_regno] == INSN_UID (insn)
	  /* Previous line always fails if INSN was moved by loop opt.  */
	  && uid_luid[regno_last_uid[dest_regno]] < INSN_LUID (loop_end)
	  && (!maybe_never || last_use_this_basic_block (dest_regno, insn)))
 	{
	  v->replaceable = 1;
	  for (b = bl->biv; b; b = b->family)
	    {
	      if ((uid_luid[INSN_UID (b->insn)] >= uid_luid[regno_first_uid[dest_regno]])
		  &&
		  (uid_luid[INSN_UID (b->insn)]
		   <= uid_luid[regno_last_uid[dest_regno]]))
		{
		  v->replaceable = 0;
		  break;
 		}
	    }
	}
      else
 	v->replaceable = 0;
    }

  if (loop_dump_stream)
    {
      if (type == DEST_REG)
 	fprintf (loop_dump_stream, "Insn %d: giv reg %d",
		 INSN_UID (insn), dest_regno);
      else
 	fprintf (loop_dump_stream, "Insn %d: dest address",
 		 INSN_UID (insn));

      fprintf (loop_dump_stream, " src reg %d benefit %d",
	       src_regno, v->benefit);
      fprintf (loop_dump_stream, " used %d lifetime %d",
	       v->times_used, v->lifetime);

      if (v->replaceable)
 	fprintf (loop_dump_stream, " replaceable");

      if (GET_CODE (mult_val) == CONST_INT)
	fprintf (loop_dump_stream, " mult %d",
 		 INTVAL (mult_val));
      else
	{
	  fprintf (loop_dump_stream, " mult ");
	  print_rtl (loop_dump_stream, mult_val);
	}

      if (GET_CODE (add_val) == CONST_INT)
	fprintf (loop_dump_stream, " add %d",
		 INTVAL (add_val));
      else
	{
	  fprintf (loop_dump_stream, " add ");
	  print_rtl (loop_dump_stream, add_val);
	}
    }

  if (loop_dump_stream && v->forces)
    fprintf (loop_dump_stream, " forces insn %d", INSN_UID (v->forces->insn));
  if (loop_dump_stream && v->forces2)
    fprintf (loop_dump_stream, " forces insn %d", INSN_UID (v->forces2->insn));
  if (loop_dump_stream && v->consec)
    fprintf (loop_dump_stream, " consec %d", v->consec);
  if (loop_dump_stream)
    fprintf (loop_dump_stream, "\n");
}

/* Delete the insns forced by the insn described by V.
   If THIS_TOO is nonzero, delete that insn itself as well.  */

static void
delete_insn_forces (v, this_too)
     struct induction *v;
     int this_too;
{
  rtx x, p;
  int count;
  rtx insn;

  if (this_too)
    {
      insn = v->insn;
      for (count = v->consec; count >= 0; count--)
	{
	  /* If first insn of libcall sequence, skip to end.  */
	  /* Do this at start of loop, since p is guaranteed to
	     be an insn here.  */
	  if (x = find_reg_note (insn, REG_LIBCALL, 0))
	    insn = XEXP (x, 0);

	  if (x = find_reg_note (insn, REG_RETVAL, 0))
	    {
	      /* This is a library call; delete all insns backward until get to
		 first insn in this group.  */
	      rtx first = XEXP (x, 0);
	      for (p = insn; p != first; p = PREV_INSN (p))
		delete_insn (p);
	      /* Delete first insn also.  */
	      delete_insn (p);
	    }
	  else
	    delete_insn (insn);

	  do insn = NEXT_INSN (insn);
	  while (GET_CODE (insn) == NOTE);
	}
    }

  if (v->forces)
    delete_insn_forces (v->forces, 1);
  if (v->forces2)
    delete_insn_forces (v->forces2, 1);
}

/* Check whether an insn is an increment legitimate for a basic induction var.
   X is the source of the insn.
   DEST_REG is the putative biv, also the destination of the insn.
   We accept patterns of these forms:
     REG = REG + INVARIANT
     REG = INVARIANT + REG
     REG = REG - CONSTANT

   If X is suitable, we return 1,
   and store the factor multiplying REF in X into *MULT_VAL
   and the additive term into *INC_VAL.
   Otherwise we return 0.  */

static int
basic_induction_var (x, dest_regno, inc_val, mult_val)
     register rtx x;
     int dest_regno;
     rtx *inc_val;
     rtx *mult_val;
{
  register enum rtx_code code;
  rtx arg;

  if (x == 0)
    return 0;
  code = GET_CODE (x);
  switch (code)
    {
    case PLUS:
      if (GET_CODE (XEXP (x, 0)) == REG
	  && REGNO (XEXP (x, 0)) == dest_regno)
 	arg = XEXP (x, 1);
      else if (GET_CODE (XEXP (x, 1)) == REG
	       && REGNO (XEXP (x, 1)) == dest_regno)
	arg = XEXP (x, 0);
      else
 	return 0;

      if (invariant_p (arg) == 1)
	*inc_val = arg;
      else
	return 0;

      *mult_val = const1_rtx;
      return 1;

    case MINUS:
      if (GET_CODE (XEXP (x, 0)) == REG
 	  && REGNO (XEXP (x, 0)) == dest_regno
 	  && GET_CODE (XEXP (x, 1)) == CONST_INT)
 	*inc_val = gen_rtx (CONST_INT, VOIDmode,
			    - INTVAL (XEXP (x, 1)));
      else
 	return 0;
      *mult_val = const1_rtx;
      return 1;

      /* Can accept constant setting of biv only when inside inner most loop.
  	 Otherwise, a biv of an inner loop may be incorrectly recognized
	 as a biv of the outer loop,
	 causing code to be moved INTO the inner loop.  */
    case REG:
      if (!invariant_p (x))
	return 0;
    case CONST_INT:
    case SYMBOL_REF:
    case CONST:
      if (loops_enclosed == 1)
 	{
	  *inc_val = x;
 	  *mult_val = const0_rtx;
 	  return 1;
 	}
      else
 	return 0;

    default:
      return 0;
    }
}

/* A general induction variable (giv) is any quantity that is a linear function
   of a basic induction variable, i.e. giv = biv * mult_val + add_val.
   The coefficients can be any loop invariant quantity.
   A giv need not be computed directly from the biv;
   it can be computed by way of other givs.  */

/* Determine whether X computes a giv.
   If it does, return a nonzero value
     which is the benefit from eliminating the computation of X;
   set *SRC_REGNO to the register number of the biv that it is computed from;
   set *ADD_VAL and *MULT_VAL to the coefficients,
     such that the value of X is biv * mult + add;
   set forces (and forces2) to identify any other givs that are used
     solely to compute this one.  */

/* This routine recognizes four types of patterns that generate givs:
   - giv = biv op invariant             v = 0,    g = 0
   - giv1 = giv2 op invariant           v = 0,    g = giv2
       where giv1 and giv2 are functions of the same biv
   - giv1 = biv op giv2                 v = giv2, g = 0
       where giv2 is a function of biv
   - giv1 = giv2 op giv3                v = giv3, g = giv2
       where giv2 and giv3 are functions of the save biv  */

static int
general_induction_var (x, src_regno, add_val, mult_val, forces, forces2)
     rtx x;
     int *src_regno;
     rtx *add_val;
     rtx *mult_val;
     struct induction **forces;
     struct induction **forces2;
{
  register enum rtx_code code;
  rtx arg;
  struct induction *g = 0;
  struct induction *v = 0;
  int subexp = 0;
  int tem;

  if (x == 0)
    return 0;

  code = GET_CODE (x);
  switch (code)
    {
    case NEG:
      /* This can generate givs also, but it is not handled.  */
      return 0;

    case MULT:
    case UMULT:
      /* Reject widening multiply in version 1.
	 That is safer than trying to handle it.  */
      {
	enum machine_mode m0 = GET_MODE (XEXP (x, 0));
	enum machine_mode m1 = GET_MODE (XEXP (x, 1));
	if (m0 != VOIDmode && m0 != GET_MODE (x))
	  return 0;
	if (m1 != VOIDmode && m1 != GET_MODE (x))
	  return 0;
      }
    case PLUS:
    case MINUS:
      /* Result is linear in both operands.  */
      /* Determine which operand is the biv, and put the other in ARG.  */
      if (GET_CODE (XEXP (x, 0)) == REG
	  && induct_var[REGNO (XEXP (x, 0))] == BASIC_INDUCT)
	{
	  *src_regno = REGNO (XEXP (x, 0));
	  arg = XEXP (x, 1);

	}
      else if (GET_CODE (XEXP (x, 1)) == REG
	       && induct_var[REGNO (XEXP (x, 1))] == BASIC_INDUCT)
	{
	  *src_regno = REGNO (XEXP (x, 1));
	  arg = XEXP (x, 0);

	}
      /* Check for an rtl subexpression that is a giv.  Memory address
	 givs often look like (plus (reg) (mult (biv) (const))).  */
      /* Do this before checking for a giv operand, as this function will
	 fail if this special operand is not recognized.  */
#ifndef DONT_REDUCE_ADDR
      else if (tem = general_induction_var (XEXP (x, 1), src_regno,
					    add_val, mult_val,
					    forces, forces2)
	       && code != MINUS)
	{
	  /* Set subexp true so that this can be handled a little
	     differently from the normal case of g set.  */
	  /* Note that SRC_REGNO is already set.  */
	  subexp = TRUE;
	  g = (struct induction *) alloca (sizeof (struct induction));
	  g->mult_val = *mult_val;
	  g->add_val = *add_val;
	  /* Fake out the test below.  */
	  g->replaceable = 1;
	  /* Count this multiply as a shift, since that's what it
	     really will do.  */
	  if (tem == MULT_BENEFIT)
	    g->benefit = SHIFT_BENEFIT;
	  else
	    g->benefit = tem;
	  arg = XEXP (x, 0);
	}
      else if (tem = general_induction_var (XEXP (x, 0), src_regno,
					    add_val, mult_val,
					    forces, forces2))
	{
	  /* Set subexp true so that this can be handled a little
	     differently from the normal case of g set.  */
	  /* Note that SRC_REGNO is already set.  */
	  subexp = TRUE;
	  g = (struct induction *) alloca (sizeof (struct induction));
	  g->mult_val = *mult_val;
	  g->add_val = *add_val;
	  /* Fake out the test below.  */
	  g->replaceable = 1;
	  /* Count this multiply as a shift, since that's what it
	     really will do.  */
	  if (tem == MULT_BENEFIT)
	    g->benefit = SHIFT_BENEFIT;
	  else
	    g->benefit = tem;
	  arg = XEXP (x, 1);
	}
#endif
      /* Also allow general induction variables.
	 Could have a mult followed by an add (i.e. an address calculation),
	 thereby generating two related general induction variables
	 of which only the second is actually used.  */
      /* Do this after checking both args for basic induction variables.  */
      else if (GET_CODE (XEXP (x, 0)) == REG
	       && induct_var[REGNO (XEXP (x, 0))] == GENERAL_INDUCT)
	{
	  g = induct_struct[REGNO (XEXP (x, 0))];
	  *src_regno = g->src_regno;
	  arg = XEXP (x, 1);
	}
      else if (GET_CODE (XEXP (x, 1)) == REG
	       && induct_var[REGNO (XEXP (x, 1))] == GENERAL_INDUCT
	       && code != MINUS)
	{
	  g = induct_struct[REGNO (XEXP (x, 1))];
	  *src_regno = g->src_regno;
	  arg = XEXP (x, 0);
	}
      else
	return 0;

      /* Overall form of expression looks good.  */
      break;

      /* Could handle these also.  */
    case DIV:
    case UDIV:
      /* For a 68020 could handle these? */
    case LSHIFT:
    case ASHIFT:
    case ASHIFTRT:
    case LSHIFTRT:
      /* These operations are linear only in first operand.
	 Check for a biv or giv there; if found, put other operand in ARG.  */
      if (GET_CODE (XEXP (x, 0)) == REG
	  && induct_var[REGNO (XEXP (x, 0))] == BASIC_INDUCT)
	{
	  *src_regno = REGNO (XEXP (x, 0));
	  arg = XEXP (x, 1);
	}
      /* Also allow general induction variable.  */
      else if (GET_CODE (XEXP (x, 0)) == REG
	       && induct_var[REGNO (XEXP (x, 0))] == GENERAL_INDUCT)
	{
	  g = induct_struct[REGNO (XEXP (x, 0))];
	  *src_regno = g->src_regno;
	  arg = XEXP (x, 1);
	}
      else
	return 0;

      /* Overall form of expression looks good.  */
      break;

    default:
      return 0;
    }

  /* ARG is the operand that is NOT a biv or giv.
     Test it for superficial validity.  */

  /* This is just a special case of invariant values,
     it is not really needed, but it's a shortcut.  */
  if (GET_CODE (arg) == CONST_INT)
    ;

  /* Depends on previous general induction variable, which has
     the same basic induction variable */
  /* This code detects mults that have been generated as shift and add.  */
  else if (GET_CODE (arg) == REG
	   && induct_var[REGNO (arg)] == GENERAL_INDUCT
	   && induct_struct[REGNO (arg)]->src_regno == *src_regno)
    {
      v = induct_struct[REGNO (arg)];
      /* Dependence indicated by forces, sort of kludgey.  */
    }

  /* Invariant expression, could be a constant-valued register. */
  else if (invariant_p (arg) == 1)
    ;

  /* Failure */
  else
    return 0;
    
  /* Until we can do the correct thing, suppress use of nonreplaceable givs
     as sources for other givs.  */
  if ((g && ! g->replaceable)
      || (v && ! v->replaceable))
    return 0;

  /* Now we know looks like a giv; extract the coefficients.
     We can still fail if the coefficients are not what we can handle.  */

  /* Only succeed if result mult_val and add_val are only one level of rtl,
     for example, (NEG:SI (REG:SI 34)) is not accepted.
     This mainly causes problems with the MINUS code.  */

  switch (code)
    {
    case PLUS:
      if (v && g)
	{
	  if (GET_CODE (g->mult_val) == CONST_INT)
	    {
	      if (g->mult_val == const0_rtx)
		*mult_val = v->mult_val;
	      else if (GET_CODE (v->mult_val) == CONST_INT)
		*mult_val = gen_rtx (CONST_INT, VOIDmode,
				       INTVAL (g->mult_val)
				       + INTVAL (v->mult_val));
	      else
		return 0;
	    }
	  else if (v->mult_val == const0_rtx)
	    *mult_val = g->mult_val;
	  else
	    return 0;

	  if (GET_CODE (g->add_val) == CONST_INT)
	    {
	      if (g->add_val == const0_rtx)
		*add_val = v->add_val;
	      else if (GET_CODE (v->add_val) == CONST_INT)
		*add_val = gen_rtx (CONST_INT, VOIDmode,
				       INTVAL (g->add_val)
				       + INTVAL (v->add_val));
	      else
		return 0;
	    }
	  else if (v->add_val == const0_rtx)
	    *add_val = g->add_val;
	  else
	    return 0;

	  if (subexp)
	    {
	      /* g deleted when return, can't return pointer to it */
	      if (*forces2 == 0)
		*forces2 = v;
	      return ADD_BENEFIT + g->benefit;
	    }
	  else
	    {
	      *forces = g;
	      *forces2 = v;
	      return ADD_BENEFIT;
	    }
	}
      else if (v)
	{
	  if (GET_CODE (v->mult_val) == CONST_INT)
	    *mult_val = gen_rtx (CONST_INT, VOIDmode,
				   INTVAL (v->mult_val) + 1);
	  else
	    return 0;
	  *add_val = v->add_val;
	  *forces = v;
	  return ADD_BENEFIT;
	}
      else if (g)
	{
	  *mult_val = g->mult_val;
	  if (GET_CODE (g->add_val) == CONST_INT
	      && nonmemory_operand (arg, GET_MODE (arg)))
	    *add_val = plus_constant (arg, INTVAL (g->add_val));
	  else if (GET_CODE (arg) == CONST_INT
		   && nonmemory_operand (g->add_val, GET_MODE (g->add_val)))
	    *add_val = plus_constant (g->add_val, INTVAL (arg));
	  else
	    /* Could succeed if arg == 0, but that will never occur.  */
	    return 0;

	  if (subexp)
	    /* g deleted when return, can't return pointer to it */
	    return ADD_BENEFIT + g->benefit;
	  else
	    {
	      *forces = g;
	      return ADD_BENEFIT;
	    }
	}
      else
	{
	  *mult_val = const1_rtx;
	  *add_val = arg;
	  return ADD_BENEFIT;
	}

      /* Takes a lot of code and will rarely succeed.  */
    case MINUS:
      if (v && g)
	{
	  /* G is the first argument of MINUS.  */

	  if (GET_CODE (g->mult_val) == CONST_INT)
	    {
	      if (g->mult_val == const0_rtx)
#if 0 /* Should not have to fail here */
		*mult_val = gen_rtx (NEG, SImode, v->mult_val);
#endif
		return 0;
	      else if (GET_CODE (v->mult_val) == CONST_INT)
		*mult_val = gen_rtx (CONST_INT, VOIDmode,
				       INTVAL (g->mult_val)
				       - INTVAL (v->mult_val));
	      else
		return 0;
	    }
	  else if (v->mult_val == const0_rtx)
	    *mult_val = g->mult_val;
	  else
	    return 0;

	  if (GET_CODE (g->add_val) == CONST_INT)
	    {
	      if (g->add_val == const0_rtx)
#if 0 /* should not have to fail here */
		*add_val = v->add_val;
#endif
		return 0;
	      else if (GET_CODE (v->add_val) == CONST_INT)
		*add_val = gen_rtx (CONST_INT, VOIDmode,
				       INTVAL (g->add_val)
				       - INTVAL (v->add_val));
	      else
		return 0;
	    }
	  else if (v->add_val == const0_rtx)
	    *add_val = g->add_val;
	  else
	    return 0;

	  if (subexp)
	    {
	      /* G deleted when return, can't return pointer to it */
	      if (*forces2 == 0)
		*forces2 = v;
	      return ADD_BENEFIT + g->benefit;
	    }
	  else
	    {
	      *forces = g;
	      *forces2 = v;
	      return ADD_BENEFIT;
	    }
	}
      else if (v)
	{
	  if (GET_CODE (v->mult_val) != CONST_INT)
	    return 0;
	  if (arg == XEXP (x, 0))             /* giv1 = giv2 - biv */
	    {
	      *mult_val = gen_rtx (CONST_INT, VOIDmode,
				     INTVAL (v->mult_val) - 1);
	      *add_val = v->add_val;
	    }
	  else                                /* giv1 = biv - giv2 */
	    {
	      *mult_val = gen_rtx (CONST_INT, VOIDmode,
				     1 - INTVAL (v->mult_val));
	      if (GET_CODE (v->add_val) == CONST_INT)
		*add_val = gen_rtx (CONST_INT, VOIDmode,
				      - INTVAL (v->add_val));
	      else
		return 0;
	    }
	  *forces = v;
	  return ADD_BENEFIT;
	}
      else if (g)
	{
	  if (arg == XEXP (x, 1))
	    *mult_val = g->mult_val;
	  else
	    {
	      if (GET_CODE (g->mult_val) == CONST_INT)
		*mult_val = gen_rtx (CONST_INT, VOIDmode,
				       - INTVAL (g->mult_val));
	      else
		return 0;
	    }
	  if (GET_CODE (g->add_val) == CONST_INT)
	    {
	      if (g->add_val == const0_rtx)
		{
		  if (arg == XEXP (x, 1))    /* giv1 = giv2 - arg */
		    {
		      /* Fail unless arg is a constant.  */
		      if (GET_CODE (arg) == CONST_INT)
			*add_val = gen_rtx (CONST_INT, VOIDmode,
					      -INTVAL (arg));
		      else
			return 0;
		    }
		  else                       /* giv1 = arg - giv2 */
		    *add_val = arg;
		}
	      else if (GET_CODE (arg) == CONST_INT)
		{
		  if (arg == XEXP (x, 1))   /* giv1 = giv2 - arg */
		    *add_val = gen_rtx (CONST_INT, VOIDmode,
					  INTVAL (g->add_val)
					  - INTVAL (arg));
		  else                      /* giv1 = arg - giv2 */
		    *add_val = gen_rtx (CONST_INT, VOIDmode,
					  INTVAL (arg),
					  - INTVAL (g->add_val));
		}
	      else
		return 0;
	    }
	  else
	    /* Could succeed if arg == 0, but that will never occur.  */
	    return 0;

	  if (subexp)
	    /* G deleted when return, can't return pointer to it.  */
	    return ADD_BENEFIT + g->benefit;
	  else
	    {
	      *forces = g;
	      return ADD_BENEFIT;
	    }
	}
      else if (GET_CODE (arg) == CONST_INT)
	{
	  if (arg == XEXP (x, 1))
	    {
	      *add_val = gen_rtx (CONST_INT, VOIDmode, - INTVAL (arg));
	      *mult_val = const1_rtx;
	    }
	  else
	    {
	      *add_val = arg;
	      *mult_val = gen_rtx (CONST_INT, VOIDmode, -1);
	    }
	  return ADD_BENEFIT;
	}
      else
	  return 0;

      /* UMULT can be handled like MULT since C ignores overflows.  */
    case MULT:
    case UMULT:
      if (v && g)
	{
	  /* Quadratic term, just fail.  */
	  return 0;
	}
      else if (v)
	{
	  /* Quadratic term, just fail.  */
	  return 0;
	}
      else if (g)
	{
	  /* Takes a lot of code and will rarely succeed.  */
	  /* dest = m * arg * b + a * arg */
	  if (GET_CODE (g->mult_val) == CONST_INT)
	    {
	      if (g->mult_val == const0_rtx)
		*mult_val = const0_rtx;
	      else if (g->mult_val == const1_rtx)
		*mult_val = arg;
	      else if (GET_CODE (arg) == CONST_INT)
		*mult_val = gen_rtx (CONST_INT, VOIDmode,
				       INTVAL (g->mult_val) * INTVAL (arg));
	      else
		return 0;
	    }
	  else
	    /* Could suceed if arg == 1 or 0, but this will never occur.  */
	    return 0;

	  if (GET_CODE (g->add_val) == CONST_INT)
	    {
	      if (g->add_val == const0_rtx)
		*add_val = const0_rtx;
	      else if (g->add_val == const1_rtx)
		*add_val = arg;
	      else if (GET_CODE (arg) == CONST_INT)
		*add_val = gen_rtx (CONST_INT, VOIDmode,
				       INTVAL (g->add_val) * INTVAL (arg));
	      else
		return 0;
	    }
	  else
	    /* Could suceed if arg == 1 or 0, but this will never occur.  */
	    return 0;

	  if (subexp)
	    /* G deleted when return, can't return pointer to it.  */
	    return MULT_BENEFIT + g->benefit;
	  else
	    {
	      *forces = g;
	      return MULT_BENEFIT;
	    }
	}
      else
	{
	  *mult_val = arg;
	  *add_val = const0_rtx;
	  return MULT_BENEFIT;
	}

      /* These are not worth the trouble.  */
    case DIV:
    case UDIV:
      return 0;

      /* Handle these, but only for left shift.  */
    case LSHIFT:
    case ASHIFT:
      if (v && g)
	{
	  /* Quadratic term, just fail.  */
	  return 0;
	}
      else if (v)
	{
	  /* Quadratic term, just fail.  */
	  return 0;
	}
      else if (g)
	{
	  /* Takes a lot of code and will rarely succeed.  */
	  /* dest = ((m * b) << arg) + (a << arg) */
	  if (GET_CODE (g->mult_val) == CONST_INT)
	    {
	      if (g->mult_val == const0_rtx)
		*mult_val = const0_rtx;
	      else if (GET_CODE (arg) == CONST_INT && INTVAL (arg) >= 0)
		*mult_val = gen_rtx (CONST_INT, VOIDmode,
				       INTVAL (g->mult_val)
				       * (1 << INTVAL (arg)));
	      else
		return 0;
	    }
	  else
	    /* Could suceed if arg == 0, but this will never occur.  */
	    return 0;

	  if (GET_CODE (g->add_val) == CONST_INT)
	    {
	      if (g->add_val == const0_rtx)
		*add_val = const0_rtx;
	      else if (GET_CODE (arg) == CONST_INT)
		*add_val = gen_rtx (CONST_INT, VOIDmode,
				       INTVAL (g->add_val)
				       * (1 << INTVAL (arg)));
	      else
		return 0;
	    }
	  else
	    /* Could suceed if arg == 0, but this will never occur.  */
	    return 0;

	  if (subexp)
	    /* G deleted when return, can't return pointer to it.  */
	    return SHIFT_BENEFIT + g->benefit;
	  else
	    {
	      *forces = g;
	      return SHIFT_BENEFIT;
	    }
	}

      if (GET_CODE (arg) == CONST_INT && INTVAL (arg) >= 0)
	*mult_val = gen_rtx (CONST_INT, VOIDmode, 1 << INTVAL (arg));
      else
	return 0;
      *add_val = const0_rtx;
      return SHIFT_BENEFIT;

      /* These are not worth the trouble.  */
    case ASHIFTRT:
    case LSHIFTRT:
      return 0;

      /* should never reach here */
    default:
      abort ();
      return 0;
    }
}

/* Help detect a giv that is calculated by several consecutive insns;
   for example,
      giv = biv * M
      giv = giv + A
   The caller has already identified the first insn P as having a giv as dest;
   we check that all other insns that set the same register follow
   immediately after P, that they alter nothing else,
   and that the result of the last is still a giv.

   The value is 0 if the reg set in P is not really a giv.
   Otherwise, the value is the amount gained by eliminating
   all the consecutive insns that compute the value.

   FIRST_BENEFIT is the amount gained by eliminating the first insn, P.
   SRC_REGNO is the regno of the biv; DEST_REGNO is that of the giv.

   The coefficients of the ultimate giv value are stored in
   *MULT_VAL and *ADD_VAL.  */

static int
consec_sets_giv (first_benefit, p, src_regno, dest_regno,
		 add_val, mult_val)
     int first_benefit;
     rtx p;
     int src_regno;
     int dest_regno;
     rtx *add_val;
     rtx *mult_val;
{
  int count;
  int benefit = first_benefit;
  enum rtx_code code;
  struct induction *forces, *forces2;
  rtx temp;
  int tem;

  /* Initialize info used by general_induction_var.  */
  struct induction *v =
    (struct induction *) oballoc (sizeof (struct induction));
  v->src_regno = src_regno;
  v->mult_val = *mult_val;
  v->add_val = *add_val;

  induct_var[dest_regno] = GENERAL_INDUCT;
  induct_struct[dest_regno] = v;

  count = n_times_set[dest_regno] - 1;

  while (count > 0)
    {
      p = NEXT_INSN (p);
      code = GET_CODE (p);

      /* If libcall, skip to end of call sequence.  */
      if (code == INSN && (temp = find_reg_note (p, REG_LIBCALL, 0)))
	p = XEXP (temp, 0);

      if (code == INSN && GET_CODE (PATTERN (p)) == SET
	  && GET_CODE (SET_DEST (PATTERN (p))) == REG
	  && REGNO (SET_DEST (PATTERN (p))) == dest_regno
	  && ((tem = general_induction_var (SET_SRC (PATTERN (p)), &src_regno,
					    add_val, mult_val,
					    &forces, &forces2))
	      /* Giv created by call to library routine.  */
	      || ((temp = loop_find_reg_equal (p))
		  && (tem = general_induction_var (XEXP (temp, 0), &src_regno,
						   add_val, mult_val,
						   &forces, &forces2))))
	  && src_regno == v->src_regno)
	{
	  count--;
	  benefit += tem;
	  v->mult_val = *mult_val;
	  v->add_val = *add_val;
	}
      else if (code != NOTE)
	{
	  induct_var[dest_regno] = UNKNOWN_INDUCT;
	  return 0;
	}
    }

  return benefit;
}

/* Generate a SEQUENCE to multiply OP0 and OP1 with result in TARGET.
   Use expand_mult to "optimally" do the multiply.
   This also works for machines that do not have multiply insns.
   If one of the operands is a constant, it must be the second.  */

static rtx
gen_iv_mult (mode, op0, op1, target)
     enum machine_mode mode;
     register rtx op0, op1, target;
{
  extern rtx gen_sequence ();
  extern rtx start_sequence ();
  rtx saved, result, temp;

  saved = start_sequence ();

  /* ??? It is very unmodular to use expand_mult here!
     This should be redesigned.  */

  /* UNSIGNEDP arg can be zero since operands/target always same width.  */
  temp = expand_mult (mode, op0, op1, target, 0);

  /* Move to target register, if expand_mult did not put it there.  */
  if (target != 0 && temp != target)
    emit_move_insn (target, temp);

  result = gen_sequence ();
  end_sequence (saved);

  return result;
}

/* Emit code to initialize an induction variable created by strength
   reduction.
   More precisely, emit code before INSERT_BEFORE
   to set REG = B * M + A.  */

static void
emit_iv_init_code (b, m, a, reg, insert_before)
     rtx b;          /* initial value of basic induction variable */
     rtx m;          /* multiplicative constant */
     rtx a;          /* additive constant */
     rtx reg;        /* destination register */
     rtx insert_before;
{
  rtx seq;
  rtx result;

  /* Prevent unexpected sharing of these rtx.  */
  a = copy_rtx (a);
  b = copy_rtx (b);

  start_sequence ();
  result = expand_mult_add (b, m, a, GET_MODE (reg), 0);
  if (reg != result)
    emit_move_insn (reg, result);
  seq = gen_sequence ();
  end_sequence ();

  emit_insn_before (seq, insert_before);
}

/* Emit code to increment the induction variable inside the loop.
   Try to emit optimal code for the expression
   REG = REG + BIV_ADD * GIV_MULT.  */

static void
emit_iv_inc (biv_add, giv_mult, reg, insn)
     rtx biv_add;                   /* increment value for biv */
     rtx giv_mult;                  /* multiply value of giv */
     rtx reg;                       /* create insn to set this reg */
     rtx insn;                      /* where to insert the new insn */
{
  emit_iv_init_code (biv_add, giv_mult, reg, reg, insn);
}

/* Test whethen BIV_ADD * GIV_MULT can be computed without
   an actual multiply insn.  Value is 1 if so.  */

static int
product_cheap_p (biv_add, giv_mult)
     rtx biv_add;
     rtx giv_mult;
{
  /* Indicates which of MULT/ADD are constants.  */
  int status = 0;
  int const_val;
  rtx tmp;

  if (GET_CODE (biv_add) == CONST_INT)
    status |= 0x1;
  if (GET_CODE (giv_mult) == CONST_INT)
    status |= 0x2;

  switch (status)
    {
    case 0:
      /* Neither is constant: would need a multiply insn, so fail.  */
      return 0;

    case 1:
      /* BIV_ADD value is constant */
      /* Equivalent to state 2, just switch values of BIV_ADD and GIV_MULT
	 and fall through.  */
      tmp = biv_add;
      biv_add = giv_mult;
      giv_mult = tmp;

    case 2:
      /* GIV_MULT value is constant.
	 If it is 1, 0 or -1 then we win.  */
      const_val = INTVAL (giv_mult);
      if (const_val < -1 || const_val > 1)
	{
	  tmp = gen_iv_mult (GET_MODE (biv_add), biv_add, giv_mult, 0);
	  /* Don't emit a multiply insn, just fail instead.  */
	  if ((GET_CODE (tmp) == SET && GET_CODE (SET_SRC (tmp)) == MULT)
	         /* Also fail if library call (which generates more
		    then two insn) is needed.  */
	      || (GET_CODE (tmp) == SEQUENCE && XVECLEN (tmp, 0) > 2))
	    return 0;
	}
      return 1;

    case 3:
      /* Both BIV_ADD and GIV_MULT are constant;
	 can compute the product at compile time.  */
      return 1;

    default:
      abort ();
    }
}


/* Check to see if loop can be terminated by a "decrement and branch until
   zero" instruction.  If so, add a REG_NONNEG note to the branch insn if so.
   Also try reversing an increment loop to a decrement loop
   to see if the optimization can be performed.
   Value is nonzero if optimization was performed.  */

static int
check_dbra_loop (loop_end, iv_list, insn_count, loop_start)
     rtx loop_end;
     struct iv_class *iv_list;
     int insn_count;
     rtx loop_start;
{
  struct iv_class *bl;
  rtx reg;
  rtx jump_label;
  rtx final_value;
  rtx start_value;
  enum rtx_code branch_code;
  rtx new_add_val;
  rtx tested_before_loop = 0;
  rtx p;

  /* See if the loop is contained in  `if (X >= 0)' for some reg X.
     If so, then we know X is initially nonnegative even though
     we don't know its initial value.
     Record X in TESTED_BEFORE_LOOP.  */

  for (p = loop_start; p != 0; p = PREV_INSN (p))
    if (GET_CODE (p) != NOTE)
      break;

  /* See if a conditional branch preceeds the loop.
     There may be no other insns or labels between it and
     the beginning of the loop.  */
  if (p != 0 && GET_CODE (p) == JUMP_INSN && condjump_p (p)
      && SET_SRC (PATTERN (p)) != pc_rtx
      && ((GET_CODE (XEXP (SET_SRC (PATTERN (p)), 0)) == LT
	   && XEXP (SET_SRC (PATTERN (p)), 2) == pc_rtx)
	  ||
	  (GET_CODE (XEXP (SET_SRC (PATTERN (p)), 0)) == GE
	   && XEXP (SET_SRC (PATTERN (p)), 1) == pc_rtx))
      && next_real_insn (JUMP_LABEL (p)) == next_real_insn (loop_end))
    {
      /* Before the branch should be a test or compare.
	 See if we are comparing something against zero.  */
      p = PREV_INSN (p);
      if (GET_CODE (p) == INSN && GET_CODE (PATTERN (p)) == SET
	  && SET_DEST (PATTERN (p)) == cc0_rtx)
	{
	  if (GET_CODE (SET_SRC (PATTERN (p))) == REG)
	    tested_before_loop = SET_SRC (PATTERN (p));
	  else if (GET_CODE (SET_SRC (PATTERN (p))) == COMPARE
		   && GET_CODE (XEXP (SET_SRC (PATTERN (p)), 0)) == REG
		   && XEXP (SET_SRC (PATTERN (p)), 1) == const0_rtx)
	    tested_before_loop = XEXP (SET_SRC (PATTERN (p)), 0);
	  else if (GET_CODE (SET_SRC (PATTERN (p))) == COMPARE
		   && GET_CODE (XEXP (SET_SRC (PATTERN (p)), 1)) == REG
		   && XEXP (SET_SRC (PATTERN (p)), 0) == const0_rtx)
	    tested_before_loop = XEXP (SET_SRC (PATTERN (p)), 1);
	}
    }

  /* If last insn is a conditional branch, and the insn before tests a register
     value, then try to optimize it.  */

  if (GET_CODE (PREV_INSN (loop_end)) == JUMP_INSN
      && GET_CODE (PATTERN (PREV_INSN (loop_end))) == SET
      && GET_CODE (SET_SRC (PATTERN (PREV_INSN (loop_end)))) == IF_THEN_ELSE
      && GET_CODE (PREV_INSN (PREV_INSN (loop_end))) == INSN
      && GET_CODE (PATTERN (PREV_INSN (PREV_INSN (loop_end)))) == SET
      && (GET_CODE (SET_DEST (PATTERN (PREV_INSN (PREV_INSN (loop_end))))) ==
	  CC0))
    {
      /* Check all of the bivs to see if the compare uses one of them.  */

      for (bl = iv_list; bl; bl = bl->next)
	{
	  if (reg_mentioned_p (SET_DEST (PATTERN (bl->biv->insn)),
			       PREV_INSN (PREV_INSN (loop_end))))
	    break;
	}

      /* If biv set more than once, then give up.
	 We can't guarantee that it will be zero on the last iteration.
	 Also give up if the biv is used between its update and the test
	 insn.  */
      if (bl && bl->biv_count == 1
	  && ! reg_used_between_p (regno_reg_rtx[bl->regno], bl->biv->insn,
				   PREV_INSN (PREV_INSN (loop_end))))
	{
	  /* Look for the case where the basic induction variable is always
	     nonnegative, and equals zero on the last iteration.
	     In this case, add a reg_note REG_NONNEG, which allows the
	     m68k DBRA instruction to be used.  */

	  /* the decrement case */

	  if (GET_CODE (bl->biv->add_val) == CONST_INT
	      && INTVAL (bl->biv->add_val) < 0)
	    {
	      /* Initial value must be greater than 0,
		 init_val % -dec_value == 0 to ensure that it equals zero on
		    the last iteration */

	      if (GET_CODE (bl->initial_value) == CONST_INT
		  && INTVAL (bl->initial_value) > 0
		  && (INTVAL (bl->initial_value) %
		      (-INTVAL (bl->biv->add_val))) == 0)
		{
		  /* register always nonnegative, add REG_NOTE to branch */
		  REG_NOTES (PREV_INSN (loop_end))
		    = gen_rtx (EXPR_LIST, REG_NONNEG, 0,
			       REG_NOTES (PREV_INSN (loop_end)));
		  bl->nonneg = 1;

		  return 1;
		}

	      /* If the decrement is 1 and the value was tested as >= 0 before
		 the loop, then we can safely optimize.  */
	      if (SET_DEST (PATTERN (bl->biv->insn)) == tested_before_loop
		  && INTVAL (bl->biv->add_val) == -1)
		{
		  REG_NOTES (PREV_INSN (loop_end))
		    = gen_rtx (EXPR_LIST, REG_NONNEG, 0,
			       REG_NOTES (PREV_INSN (loop_end)));
		  bl->nonneg = 1;

		  return 1;
		}
	    }
	  else if (num_mem_sets <= 1)
	    {
	      /* Try to change inc to dec, so can apply above optimization.  */
	      /* Can do this if:
		 all registers modified are induction variables or invariant,
		 all memory references have non-overlapping addresses
                       (obviously true if only one write)
	         allow 2 insns for the compare/jump at the end of the loop.  */
	      int num_nonfixed_reads = 0;
	      rtx p;

	      for (p = loop_start; p != loop_end; p = NEXT_INSN (p))
		if (GET_CODE (p) == INSN || GET_CODE (p) == CALL_INSN
		    || GET_CODE (p) == JUMP_INSN)
		  num_nonfixed_reads += count_nonfixed_reads (PATTERN (p));

	      /* This code only acts for innermost loops.  Also it simplifies
		 the memory address check by only reversing loops with
		 zero or one memory access.
		 Two memory accesses could involve parts of the same array,
		 and that can't be reversed.  */

	      if (num_nonfixed_reads <= 1
		  && !loop_has_call
		  && (bl->giv_count + bl->biv_count + num_mem_sets
		      + num_movables + 2 == insn_count))
		{
		  rtx src_two_before_end;
		  int constant;
		  int win;

		  /* Loop can be reversed.  */
		  if (loop_dump_stream)
		    fprintf (loop_dump_stream, "Can reverse loop\n");

		  /* Now check other conditions:
		     initial_value must be zero,
		     final_value % add_val == 0, so that when reversed, the
		     biv will be zero on the last iteration.  */

		  /* Calculating the final value non trivial.
		     If branch is (LT (CC0) (CONST 0),
		     then value in compare is final value.
		     If branch is (LE (CC0) (CONST 0),
		     then value in compare is final_value - add_val */

		  branch_code
		    = GET_CODE (XEXP (SET_SRC (PATTERN (PREV_INSN (loop_end))), 0));
		  src_two_before_end
		    = SET_SRC (PATTERN (PREV_INSN (PREV_INSN (loop_end))));

		  win = 1;
		  if (GET_CODE (src_two_before_end) == REG)
		    constant = 0;
		  else if (GET_CODE (src_two_before_end) == COMPARE
			   && GET_CODE (XEXP (src_two_before_end, 1)) == CONST_INT)
		    constant = INTVAL (XEXP (src_two_before_end, 1));
		  else
		    win = 0;

		  if (win && bl->initial_value == const0_rtx
		      && (branch_code == LT || branch_code == LE)
		      && XEXP (XEXP (SET_SRC (PATTERN (PREV_INSN (loop_end))), 0), 1) == const0_rtx
		      && (constant % INTVAL (bl->biv->add_val)) == 0)
		    {
		      /* Register will always be nonnegative, with value
			 0 on last iteration if loop reversed */

		      /* Save some info needed to produce the new insns.  */
		      reg = SET_DEST (PATTERN (bl->biv->insn));
		      jump_label = XEXP (SET_SRC (PATTERN (PREV_INSN (loop_end))), 1);
		      new_add_val = gen_rtx (CONST_INT, VOIDmode,
					     - INTVAL (bl->biv->add_val));


		      if (branch_code == LT)
			{
			  final_value
			    = gen_rtx (CONST_INT, VOIDmode, constant);
			  start_value
			    = gen_rtx (CONST_INT, VOIDmode,
				       (constant - INTVAL (bl->biv->add_val)));
			}
		      else /* branch_code == LE */
			{
			  start_value
			    = gen_rtx (CONST_INT, VOIDmode, constant);
			  final_value
			    = gen_rtx (CONST_INT, VOIDmode,
				       (constant + INTVAL (bl->biv->add_val)));
			}

		      /* Initialize biv to start_value before loop start.
			 The old initializing insn will be deleted as a
			 dead store by flow.c.  */
		      emit_insn_before (gen_rtx (SET, VOIDmode, reg,
						 start_value),
					loop_start);

		      /* Add insn to decrement register, and delete insn
			 that incremented the register.  */
		      emit_insn_before (gen_rtx (SET, VOIDmode, reg,
					  gen_rtx (PLUS, GET_MODE (reg), reg,
						   new_add_val)),
					bl->biv->insn);
		      /* Update biv info to reflect its new status.  */
		      bl->biv->insn = PREV_INSN (bl->biv->insn);
		      delete_insn (NEXT_INSN (bl->biv->insn));

		      /* Inc LABEL_NUSES so that delete_insn will
			 not delete the label.  */
		      LABEL_NUSES (XEXP (jump_label, 0)) ++;

		      if (regno_last_uid[bl->regno] != INSN_UID (PREV_INSN (loop_end)))
			emit_insn_after (gen_rtx (SET, VOIDmode, reg,
						  final_value),
					 loop_end);

		      /* Delete compare/branch at end of loop.  */
		      delete_insn (PREV_INSN (loop_end));
		      delete_insn (PREV_INSN (loop_end));

		      /* Add new compare/branch insn at end of loop.  */
		      emit_insn_before (gen_rtx (SET, VOIDmode, cc0_rtx, reg),
					loop_end);
		      emit_jump_insn_before (gen_rtx (SET, VOIDmode, pc_rtx,
					 gen_rtx (IF_THEN_ELSE, VOIDmode,
					     gen_rtx (GE, VOIDmode, cc0_rtx,
						      const0_rtx),
					     jump_label,
					     pc_rtx)),
					  loop_end);

		      JUMP_LABEL (PREV_INSN (loop_end)) = XEXP (jump_label, 0);
		      /* Increment of LABEL_NUSES done above. */

		      /* Register is now always nonnegative,
			 so add REG_NONNEG note to the branch.  */
		      REG_NOTES (PREV_INSN (loop_end))
			= gen_rtx (EXPR_LIST, REG_NONNEG, 0,
				   REG_NOTES (PREV_INSN (loop_end)));
		      bl->nonneg = 1;

		      /* Update rest of biv info.  */
		      bl->initial_value = start_value;
		      bl->biv->add_val = new_add_val;

		      if (loop_dump_stream)
			fprintf (loop_dump_stream, "Reversed loop and added reg_nonneg\n");

		      return 1;
		    }
		}
	    }
	}
    }
  return 0;
}

/* Verify whether the biv BL appears to be eliminable,
   based on the insns in the loop that refer to it.
   LOOP_START is the first insn of the loop, and END is the end insn.  */

static void
check_eliminate_biv (bl, loop_start, end)
     struct iv_class *bl;
     rtx loop_start;
     rtx end;
{
  /* Get the REG rtx for the biv.  */
  rtx reg = SET_DEST (PATTERN (bl->biv->insn));
  rtx p;
  struct induction *v;

  bl->eliminable = 0;

  for (p = loop_start; p != end; p = NEXT_INSN (p))
    {
      enum rtx_code code = GET_CODE (p);
      if ((code == INSN || code == JUMP_INSN || code == CALL_INSN)
	  && reg_mentioned_p (reg, PATTERN (p)))
	{
	  /* This insn uses the biv.  If we can't understand it,
	     then we can't eliminate the biv.  */
	  if (GET_CODE (PATTERN (p)) != SET)
	    {
	      if (loop_dump_stream)
		fprintf (loop_dump_stream,
			 "Cannot eliminate biv %d: cannot understand insn %d.\n",
			 bl->regno, INSN_UID (p));
	      break;
	    }

	  /* The insns that increment the biv are no problem.  */
	  if (SET_DEST (PATTERN (p)) == reg)
	    continue;

	  /* If this is an insn which uses the biv ONLY in the
	     calculation of a giv which is in the family of this
	     biv, it's ok becuase it will go away when the giv is
	     reduced.  March 14, 1989 -- self@bayes.arc.nasa.gov */
	  for (v = bl->giv; v; v = v->family)
	    if (v->insn == p)
	      {
		if (v->giv_type == DEST_REG
		    || (v->giv_type == DEST_ADDR
			&& ! other_reg_use_p (reg, *(v->location),
					      PATTERN (p))))
		  break;
	      }
	  if (v)
	    continue;

	  /* If can rewrite this insn not to use the biv, it's ok.  */
	  if (can_eliminate_biv_p (p, bl))
	    continue;

	  /* Biv is used in a way we cannot eliminate.  */
	  if (loop_dump_stream)
	    fprintf (loop_dump_stream,
		     "Cannot eliminate biv %d: biv used in insn %d.\n",
		     bl->regno, INSN_UID (p));
	  break;
	}
    }

  if (p == end)
    {
      bl->eliminable = 1;
      if (loop_dump_stream)
	fprintf (loop_dump_stream, "Can eliminate biv %d.\n",
		 bl->regno);
    }
}

/* Return 1 if INSN, a compare insn which tests the biv described by BL,
   can be rewritten to use instead some reduced giv related to that biv.
   Does not change the rtl.

   We make the assumption that all the givs depending on this biv
   will be reduced, since only in that case will an attempt to eliminate
   the biv actually be made.

   The following function is very parallel to this one.  */

static int
can_eliminate_biv_p (insn, bl)
     rtx insn;
     struct iv_class *bl;
{
  rtx src;
  enum rtx_code code;
  struct induction *v, *tv;
  rtx arg;
  int arg_operand;
  /* Mode of this biv.  */
  enum machine_mode mode = bl->biv->mode;

  if (SET_DEST (PATTERN (insn)) != cc0_rtx)
    return 0;

  src = SET_SRC (PATTERN (insn));
  code = GET_CODE (src);

  switch (code)
    {
      /* a test insn */
    case REG:
      /* Can replace with any giv that has (MULT_VAL != 0) and (ADD_VAL == 0)
	 Require a constant integer for MULT_VAL, so we know it's nonzero.  */

      for (v = bl->giv; v; v = v->family)
	if (GET_CODE (v->mult_val) == CONST_INT && v->mult_val != const0_rtx
	    && v->add_val == const0_rtx
	    && ! v->ignore
	    && v->mode == mode)
	  return 1;

      /* Look for a giv with (MULT_VAL != 0) and (ADD_VAL != 0)
	 where ADD_VAL is a constant or a register;
	 can replace test insn with a compare insn (cmp REDUCED_GIV ADD_VAL).
	 Require a constant integer for MULT_VAL, so we know it's nonzero.  */

      for (v = bl->giv; v; v = v->family)
	if (GET_CODE (v->mult_val) == CONST_INT && v->mult_val != const0_rtx
	    && (GET_CODE (v->add_val) == REG || GET_CODE (v->add_val) == CONST_INT)
	    && ! v->ignore
	    && v->mode == mode)
	  return 1;

      if (loop_dump_stream)
	fprintf (loop_dump_stream, "Cannot eliminate biv %d in test insn %d: no appropriate giv.\n",
		 bl->regno, INSN_UID (insn));

      return 0;

      /* a compare insn */
    case COMPARE:
      /* Figure out which operand is the biv.  */
      if ((GET_CODE (XEXP (src, 0)) == REG)
	  && (REGNO (XEXP (src, 0)) == bl->regno))
	{
	  arg = XEXP (src, 1);
	  arg_operand = 1;
	}
      else if ((GET_CODE (XEXP (src, 1)) == REG)
	       && (REGNO (XEXP (src, 1)) == bl->regno))
	{
	  arg = XEXP (src, 0);
	  arg_operand = 0;
	}
      else
	return 0;

      if (GET_CODE (arg) == CONST_INT)
	{
	  /* Can replace with any giv that has constant coefficients.  */

	  for (v = bl->giv; v; v = v->family)
	    if (GET_CODE (v->mult_val) == CONST_INT
		&& GET_CODE (v->add_val) == CONST_INT
		&& ! v->ignore
		&& v->mode == mode)
	      return 1;

	  /* Look for giv with constant mult_val and nonconst add_val,
	     since we can insert add insn before loop
	     to calculate new compare value.  */

	  for (v = bl->giv; v; v = v->family)
	    if (GET_CODE (v->mult_val) == CONST_INT
		&& ! v->ignore
		&& v->mode == mode)
	      return 1;
	}
      else if (GET_CODE (arg) == REG || GET_CODE (arg) == MEM)
	{
	  /* Comparing against invariant register or memref can be handled.  */

	  if (invariant_p (arg))
	    {
	      /* Look for giv with constant mult_val and nonconst add_val.
		 Insert add-insn before loop to compute new compare value.  */

	      for (v = bl->giv; v; v = v->family)
		if ((GET_CODE (v->mult_val) == CONST_INT)
		    && ! v->ignore
		    && v->mode == mode)
		  return 1;
	    }

	  /* Otherwise, only comparing against a biv can be handled.  */
	  if (GET_CODE (arg) != REG
	      || induct_var[REGNO (arg)] != BASIC_INDUCT)
	    return 0;

	  /* Look for a giv for each biv that have identical
	     values for mult_val and add_val.  */
	  for (v = bl->giv; v; v = v->family)
	    if (v->mode == mode
		&& ! v->ignore)
	      {
		for (tv = class_struct[REGNO (arg)]->giv; tv; tv = tv->family)
		  if ((tv->new_reg != 0)
		      && rtx_equal_p (tv->mult_val, v->mult_val)
		      && rtx_equal_p (tv->mult_val, v->mult_val)
		      && ! tv->ignore
		      && tv->mode == mode)
		    return 1;
	      }
	}
      return 0;

    default:
      return 0;
    }
}

/* Rewrite a compare insn INSN which uses the biv described by BL
   so that it doesn't use that biv any more.
   Instead it will use some reduced giv related to that biv.

   The preceding function is very parallel to this one.  */

static void
eliminate_biv (insn, bl, loop_start)
     rtx insn;
     struct iv_class *bl;
     rtx loop_start;
{
  rtx src = SET_SRC (PATTERN (insn));
  enum rtx_code code = GET_CODE (src);
  struct induction *v, *tv;
  rtx arg, temp;
  int arg_operand;
  /* Mode of this biv.  */
  enum machine_mode mode = bl->biv->mode;

  switch (code)
    {
      /* a test insn */
    case REG:
      /* Can replace with any giv that was reduced and
	 that has (MULT_VAL != 0) and (ADD_VAL == 0).
	 Require a constant integer for MULT_VAL, so we know it's nonzero.  */

      for (v = bl->giv; v; v = v->family)
	if (GET_CODE (v->mult_val) == CONST_INT && v->mult_val != const0_rtx
	    && v->add_val == const0_rtx
	    && v->new_reg != 0
	    && v->mode == mode)
	  break;
      if (v)
	{
	  /* We can test the sign of that giv's reduced reg.  */
	  SET_SRC (PATTERN (insn)) = v->new_reg;
	  /* If the giv has the opposite direction of change,
	     then reverse the comparison.  */
	  if (INTVAL (v->mult_val) < 0)
	    SET_SRC (PATTERN (insn))
	      = gen_rtx (COMPARE, GET_MODE (v->new_reg),
			 const0_rtx, v->new_reg);
	  return;
	}

      /* Look for a giv with (MULT_VAL != 0) and (ADD_VAL != 0)
	 where ADD_VAL is a constant or a register;
	 replace test insn with a compare insn (cmp REDUCED_GIV ADD_VAL).
	 Require a constant integer for MULT_VAL, so we know it's nonzero.  */

      for (v = bl->giv; v; v = v->family)
	if (GET_CODE (v->mult_val) == CONST_INT && v->mult_val != const0_rtx
	    && (GET_CODE (v->add_val) == REG || GET_CODE (v->add_val) == CONST_INT)
	    && v->new_reg != 0
	    && v->mode == mode)
	  break;
      if (v)
	{
	  /* Replace biv with the giv's reduced register.  */
	  SET_SRC (PATTERN (insn)) = gen_rtx (COMPARE, GET_MODE (v->new_reg),
					      v->new_reg,
					      copy_rtx (v->add_val));

	  /* If the giv has the opposite direction of change,
	     then reverse the comparison.  */
	  if (INTVAL (v->mult_val) < 0)
	    {
	      XEXP (SET_SRC (PATTERN (insn)), 0)
		= XEXP (SET_SRC (PATTERN (insn)), 1);
	      XEXP (SET_SRC (PATTERN (insn)), 1) = v->new_reg;
	    }
#if 0
	  /* add_val must be invariant, so don't bother storing in a register */
	  /* calculate the appropriate constant to compare against */
	  emit_insn_before (gen_rtx (SET, VOIDmode, compare_value,
				     copy_rtx (v->add_val)),
			    loop_start);
#endif
	  return;
	}
      abort ();
      break;

      /* a compare insn */
    case COMPARE:
      /* Figure out which operand is the biv.  */
      if (GET_CODE (XEXP (src, 0)) == REG
	  && REGNO (XEXP (src, 0)) == bl->regno)
	{
	  arg = XEXP (src, 1);
	  arg_operand = 1;
	}
      else if (GET_CODE (XEXP (src, 1)) == REG
	       && REGNO (XEXP (src, 1)) == bl->regno)
	{
	  arg = XEXP (src, 0);
	  arg_operand = 0;
	}
      else
	abort ();

      if (GET_CODE (arg) == CONST_INT)
	{
	  /* Can replace with any giv that has constant mult_val and add_val.
	     Make sure it was strength reduced by checking new_reg != 0.  */

	  for (v = bl->giv; v; v = v->family)
	    if (GET_CODE (v->mult_val) == CONST_INT
		&& GET_CODE (v->add_val) == CONST_INT
		&& v->new_reg
		&& v->mode == mode)
	      break;
	  if (v)
	    {
	      rtx newval;
	      /* Replace biv with the giv's reduced reg.  */
	      XEXP (src, 1-arg_operand) = v->new_reg;
	      /* Calculate the appropriate constant to compare against.  */
	      newval = gen_rtx (CONST_INT, VOIDmode,
				(INTVAL (arg) * INTVAL (v->mult_val)
				 + INTVAL (v->add_val)));
	      XEXP (src, arg_operand) = newval;
	      /* If that constant is no good in a compare,
		 put it in a register.  */
	      if (recog (PATTERN (insn), insn) < 0)
		{
		  temp = gen_reg_rtx (mode);
		  emit_iv_init_code (arg, v->mult_val, v->add_val,
				     temp, loop_start);
		  XEXP (src, arg_operand) = temp;
		}

	      /* If the giv has the opposite direction of change,
		 then reverse the comparison.  */
	      if (INTVAL (v->mult_val) < 0)
		{
		  temp = XEXP (src, 0);
		  XEXP (src, 0) = XEXP (src, 1);
		  XEXP (src, 1) = temp;
		}
	      return;
	    }

	  /* Look for giv with constant mult_val and nonconst add_val.
	     Insert add insn before loop to calculate new compare value.  */

	  for (v = bl->giv; v; v = v->family)
	    if (GET_CODE (v->mult_val) == CONST_INT
		&& v->new_reg
		&& v->mode == mode)
	      break;
	  if (v)
	    {
	      rtx compare_value = gen_reg_rtx (mode);

	      /* Replace biv with giv's reduced register.  */
	      XEXP (src, 1-arg_operand) = v->new_reg;

	      /* At start of loop, compute value to compare against.  */
	      emit_iv_init_code (arg, v->mult_val, v->add_val,
				 compare_value, loop_start);
	      /* Use it in this insn.  */
	      XEXP (src, arg_operand) = compare_value;

	      /* If the giv has the opposite direction of change,
		 then reverse the comparison.  */
	      if (INTVAL (v->mult_val) < 0)
		{
		  temp = XEXP (src, 0);
		  XEXP (src, 0) = XEXP (src, 1);
		  XEXP (src, 1) = temp;
		}
	      return;
	    }
	  abort ();
	}
      else if (GET_CODE (arg) == REG || GET_CODE (arg) == MEM)
	{
	  if (invariant_p (arg))
	    {
	      /* Look for giv with constant mult_val and nonconst add_val.
		 Insert add-insn before loop to compute new compare value.  */

	      for (v = bl->giv; v; v = v->family)
		if (GET_CODE (v->mult_val) == CONST_INT
		    && v->new_reg
		    && v->mode == mode)
		  break;
	      if (v)
		{
		  rtx compare_value = gen_reg_rtx (mode);

		  /* Replace biv with giv's reduced register.  */
		  XEXP (src, 1-arg_operand) = v->new_reg;

		  /* At start of loop, compute value to compare against.  */
		  emit_iv_init_code (arg, v->mult_val, v->add_val,
				     compare_value, loop_start);
		  XEXP (src, arg_operand) = compare_value;

		  /* If the giv has the opposite direction of change,
		     then reverse the comparison.  */
		  if (INTVAL (v->mult_val) < 0)
		    {
		      temp = XEXP (src, 0);
		      XEXP (src, 0) = XEXP (src, 1);
		      XEXP (src, 1) = temp;
		    }
		  return;
		}
	    }

	  /* Otherwise the reg compared with had better be a biv.  */
	  if (GET_CODE (arg) != REG
	      || induct_var[REGNO (arg)] != BASIC_INDUCT)
	    abort ();

	  /* Look for a pair of givs, one for each biv,
	     with identical coefficients.  */
	  for (v = bl->giv; v; v = v->family)
	    {
	      if (!v->new_reg && v->mode == mode)
		continue;
	      for (tv = class_struct[REGNO (arg)]->giv; tv; tv = tv->family)
		if ((tv->new_reg != 0)
		    && rtx_equal_p (tv->mult_val, v->mult_val)
		    && rtx_equal_p (tv->add_val, v->add_val)
		    && tv->mode == mode)
		  break;
	      if (tv)
		break;
	    }
	  if (v)
	    {
	      /* Replace biv with its giv's reduced reg.  */
	      XEXP (src, 1-arg_operand) = v->new_reg;
	      /* Replace other operand with the other giv's reduced reg.  */
	      XEXP (src, arg_operand) = tv->new_reg;

	      /* If the giv has the opposite direction of change,
		 then reverse the comparison.  */
	      if (INTVAL (v->mult_val) < 0)
		{
		  temp = XEXP (src, 0);
		  XEXP (src, 0) = XEXP (src, 1);
		  XEXP (src, 1) = temp;
		}
	      return;
	    }
	}
      abort ();

    default:
      abort ();
    }
}

/* Try to calculate the final value of the biv,
   the value it will have at the end of the loop.
   If we can do it, return that value.  */

/* ??? One case that should be simple to handle
   is when the biv is incremented by an invariant
   exactly once each time around the loop,
   and when the number of iterations can be determined
   (as the value of some invariant).
   Then the final value would be BIV + (INCREMENT * NUM_ITERATIONS).

   Once that case is handled, it would become desirable to detect empty
   loops and delete them, since this optimization could make empty loops.  */

static rtx
final_biv_value (bl, loop_end)
     struct iv_class *bl;
     rtx loop_end;
{
  /* wimpy, but guaranteed to work */
  return 0;
}

/* Return nonzero if the last use of reg REGNO
   is in an insn following INSN in the same basic block.  */

static int
last_use_this_basic_block (regno, insn)
     int regno;
     rtx insn;
{
  rtx n;
  for (n = insn;
       n && GET_CODE (n) != CODE_LABEL && GET_CODE (n) != JUMP_INSN;
       n = NEXT_INSN (n))
    {
      if (regno_last_uid[regno] == INSN_UID (n))
	return 1;
    }
  return 0;
}
