/* Allocate registers within a basic block, for GNU compiler.
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


/* Allocation of hard register numbers to pseudo registers is done in
   two passes.  In this pass we consider only regs that are born and
   die once within one basic block.  We do this one basic block at a
   time.  Then the next pass allocates the registers that remain.
   Two passes are used because this pass uses methods that work only
   on linear code, but that do a better job than the general methods
   used in global_alloc, and more quickly too.

   The assignments made are recorded in the vector reg_renumber
   whose space is allocated here.  The rtl code itself is not altered.

   We assign each instruction in the basic block a number
   which is its order from the beginning of the block.
   Then we can represent the lifetime of a pseudo register with
   a pair of numbers, and check for conflicts easily.
   We can record the availability of hard registers with a
   HARD_REG_SET for each instruction.  The HARD_REG_SET
   contains 0 or 1 for each hard reg.

   To avoid register shuffling, we tie registers together when one
   dies by being copied into another, or dies in an instruction that
   does arithmetic to produce another.  The tied registers are
   allocated as one.  Registers with different reg class preferences
   can never be tied unless the class preferred by one is a subclass
   of the one preferred by the other.

   Tying is represented with "quantity numbers".
   A non-tied register is given a new quantity number.
   Tied registers have the same quantity number.
   
   We have provision to exempt registers, even when they are contained
   within the block, that can be tied to others that are not contained in it.
   This is so that global_alloc could process them both and tie them then.
   But this is currently disabled since tying in global_alloc is not
   yet implemented.  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "flags.h"
#include "basic-block.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "insn-config.h"
#include "recog.h"

/* What about hardware registers used and set within same insn?
   Will that ever happen for a non-fixed register?
   Our lifetime-tracking for hardware registers would lose.
   [This caution is an old comment that may be obsolete;
    I think there is no longer a problem, but I'm not sure.]  */

/* Next quantity number available for allocation.  */

static int next_qty;

/* In all the following vectors indexed by quantity number,
   only elements at indices >= FIRST_PSEUDO_REGISTER are actually used.  */

/* Element Q is the hard reg number chosen for quantity Q,
   or -1 if none was found.  */

static short *qty_phys_reg;

/* Element Q is the hard reg number suggested for quantity Q,
   or -1 if no specific suggestion.  */

static short *qty_phys_sugg;

/* Element Q is the number of refs to quantity Q.  */

static short *qty_n_refs;

/* Element Q is a reg class contained in (smaller than) the
   preferred classes of all the pseudo regs that are tied in quantity Q.
   This is the preferred class for allocating that quantity.  */

static enum reg_class *qty_min_class;

/* Insn number (counting from head of basic block)
   where quantity Q was born.  -1 if birth has not been recorded.  */

static int *qty_birth;

/* Insn number (counting from head of basic block)
   where quantity Q died.  Due to the way tying is done,
   and the fact that we consider in this pass only regs that die but once,
   a quantity can die only once.  Each quantity's life span
   is a set of consecutive insns.  -1 if death has not been recorded.  */

static int *qty_death;

/* Number of words needed to hold the data in quantity Q.
   This depends on its machine mode.  It is used for these purposes:
   1. If it is 0, the qty is not really in use and is not allocated.
   2. It is used in computing the relative importances of qtys,
      which determines the order in which we look for regs for them.
   3. It is used in rules that prevent tying several registers of
      different sizes in a way that is geometrically impossible
      (see combine_regs).  */

static int *qty_size;

/* This holds the mode of the registers that are tied to qty Q,
   or VOIDmode if registers with differing modes are tied together.  */

static enum machine_mode *qty_mode;

/* Number of times a reg tied to qty Q lives across a CALL_INSN.  */

static int *qty_n_calls_crossed;

/* Nonzero means don't allocate qty Q if we can't get its preferred class.  */

static char *qty_preferred_or_nothing;

/* reg_qty[N] (where N is a pseudo reg number)
   is the qty number of that reg (which is >= FIRST_PSEUDO_REGISTER),
   or -1 if (REG N) is not local to the current basic block,
   or -2 if not known yet.

   If N is < FIRST_PSEUDO_REGISTER, reg_qty[N] is -1.  */

static int *reg_qty;

/* The offset (in words) of register N within its quantity.
   This can be nonzero if register N is SImode, and has been tied
   to a subreg of a DImode register.  */

static int *reg_offset;

/* Vector of substitutions of register numbers,
   used to map pseudo regs into hardware regs.
   This is set up as a result of register allocation.
   Element N is the hard reg assigned to pseudo reg N,
   or is -1 if no hard reg was assigned.
   If N is a hard reg number, element N is N.  */

short *reg_renumber;

/* Set of hard registers live at the current point in the scan
   of the instructions in a basic block.  */

static HARD_REG_SET regs_live;

/* Indexed by insn-number-within-basic-block,
   a set or hard registers live *after* that insn.  */

static HARD_REG_SET *regs_live_at;

/* Nonzero if a CALL_INSN has been scanned
   but we have not yet seen a reference to the value returned.  */

static int call_seen;

/* Communicate local vars `insn_number' and `insn'
   from `block_alloc' to `reg_is_set' and `wipe_dead_reg'.  */
static int this_insn_number;
static rtx this_insn;

static void block_alloc ();
static int combine_regs ();
static void wipe_dead_reg ();
static int find_free_reg ();
static void reg_is_born ();
static void reg_is_set ();
static void mark_life ();
static void post_mark_life ();
static int qty_compare ();
static int qty_compare_1 ();
static int reg_meets_class_p ();
static int reg_class_subset_p ();
static int reg_classes_overlap_p ();
static void update_qty_class ();

/* Allocate a new quantity (new within current basic block)
   for register number REGNO which is born in insn number INSN_NUMBER
   within the block.  MODE and SIZE are info on reg REGNO.  */

static void
alloc_qty (regno, mode, size, insn_number)
     int regno;
     enum machine_mode mode;
     int size, insn_number;
{
  register int qty = next_qty++;
  reg_qty[regno] = qty;
  reg_offset[regno] = 0;
  qty_size[qty] = size;
  qty_mode[qty] = mode;
  qty_birth[qty] = insn_number;
  qty_n_calls_crossed[qty] = reg_n_calls_crossed[regno];
  qty_min_class[qty] = reg_preferred_class (regno);
  qty_preferred_or_nothing[qty] = reg_preferred_or_nothing (regno);
  qty_n_refs[qty] = reg_n_refs[regno];
}

/* Main entry point of this file.  */

void
local_alloc ()
{
  register int b, i;

  /* Allocate vectors of temporary data.
     See the declarations of these variables, above,
     for what they mean.  */

  qty_phys_reg = (short *) alloca (max_regno * sizeof (short));
  qty_phys_sugg = (short *) alloca (max_regno * sizeof (short));
  qty_birth = (int *) alloca (max_regno * sizeof (int));
  qty_death = (int *) alloca (max_regno * sizeof (int));
  qty_size = (int *) alloca (max_regno * sizeof (int));
  qty_mode = (enum machine_mode *) alloca (max_regno * sizeof (enum machine_mode));
  qty_n_calls_crossed = (int *) alloca (max_regno * sizeof (int));
  qty_min_class = (enum reg_class *) alloca (max_regno * sizeof (enum reg_class));
  qty_preferred_or_nothing = (char *) alloca (max_regno);
  qty_n_refs = (short *) alloca (max_regno * sizeof (short));

  reg_qty = (int *) alloca (max_regno * sizeof (int));
  reg_offset = (int *) alloca (max_regno * sizeof (int));

  reg_renumber = (short *) oballoc (max_regno * sizeof (short));
  for (i = 0; i < max_regno; i++)
    reg_renumber[i] = -1;

  /* This controls only how many elts of the `qty_...' vectors
     need to be zero for the first basic block.  */
  next_qty = max_regno;

  /* Allocate each block's local registers, block by block.  */

  for (b = 0; b < n_basic_blocks; b++)
    {
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
	{
	  reg_qty[i] = -1;
	}
      for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
	{
	  qty_phys_sugg[i] = -1;
	  qty_birth[i] = -1;
	  qty_death[i] = -1;
	  /* Set reg_qty to -2 for pseudos in this block, -1 for others.  */
	  if (reg_basic_block[i] == b && reg_n_deaths[i] == 1)
	    reg_qty[i] = -2;
	  else
	    reg_qty[i] = -1;
	}

      bzero (reg_offset, max_regno * sizeof (int));

      /* NEXT_QTY indicates which elements of the `qty_...'
	 vectors might need to be initialized.  Initialize those,
	 with explicit loop if there are few, else with bzero.  */

      if (next_qty < FIRST_PSEUDO_REGISTER + 6)
	{
	  for (i = FIRST_PSEUDO_REGISTER; i < next_qty; i++)
	    {
	      qty_size[i] = 0;
	      qty_mode[i] = VOIDmode;
	      qty_min_class[i] = NO_REGS;
	      qty_preferred_or_nothing[i] = 0;
	      qty_n_calls_crossed[i] = 0;
	      qty_n_refs[i] = 0;
	    }
	}
      else
	{
	  int clear_length = next_qty - FIRST_PSEUDO_REGISTER;

#define CLEAR(vector)  \
   bzero ((vector) + FIRST_PSEUDO_REGISTER,    \
	  (sizeof (*(vector))) * clear_length)

	  CLEAR (qty_size);
	  CLEAR (qty_mode);
	  CLEAR (qty_min_class);
	  CLEAR (qty_preferred_or_nothing);
	  CLEAR (qty_n_calls_crossed);
	  CLEAR (qty_n_refs);
	}

      next_qty = FIRST_PSEUDO_REGISTER;

      block_alloc (b);
#ifdef USE_C_ALLOCA
      alloca (0);
#endif
    }
}

/* Allocate hard regs to the pseudo regs used only within block number B.
   Only the pseudos that die but once can be handled.  */

static void
block_alloc (b)
     int b;
{
  register int i, q;
  register rtx insn;
  int insn_number = 0;
  int insn_count = 0;
  short *qty_order;
  int *insn_map;

  call_seen = 0;

  /* Count the instructions in the basic block.  */

  insn = basic_block_end[b];
  while (1)
    {
      if (GET_CODE (insn) != NOTE)
	insn_count++;
      if (insn == basic_block_head[b])
	break;
      insn = PREV_INSN (insn);
    }

  /* +1 to leave room for a post_mark_life at the last insn.  */
  regs_live_at = (HARD_REG_SET *) alloca ((insn_count + 1)
					  * sizeof (HARD_REG_SET));
  bzero (regs_live_at, (insn_count + 1) * sizeof (HARD_REG_SET));

  /* This will be a map from uids to insn-numbers within the block.  */

  insn_map = (int *) alloca (get_max_uid () * sizeof (int));

  /* Initialize table of hardware registers currently live.  */

#ifdef HARD_REG_SET
  regs_live = *basic_block_live_at_start[b];
#else
  COPY_HARD_REG_SET (regs_live, basic_block_live_at_start[b]);
#endif

  /* This loop scans the instructions of the basic block
     and assigns quantities to registers.
     It computes which registers to tie.  */

  insn = basic_block_head[b];
  insn_number = 0;
  while (1)
    {
      register rtx body = PATTERN (insn);

      if (GET_CODE (insn) != NOTE)
	insn_number++;
      insn_map[INSN_UID (insn)] = insn_number;

      if (GET_CODE (insn) == INSN || GET_CODE (insn) == JUMP_INSN
	  || GET_CODE (insn) == CALL_INSN)
	{
	  register rtx link;
	  register int win = 0;
	  register rtx r0, r1;
	  int combined_regno = -1;
	  int insn_code_number = recog_memoized (insn);
	  int commutative = 0;

	  this_insn_number = insn_number;
	  this_insn = insn;

	  /* Set COMMUTATIVE if operands 1 and 2 are commutative.  */
	  if (insn_code_number >= 0
	      && insn_n_operands[insn_code_number] > 2
	      && insn_operand_constraint[insn_code_number][1][0] == '%')
	    commutative = 1;

	  /* Is this insn suitable for tying two registers?
	     If so, try doing that.
	     Suitable insns are (set reg0 reg1) and
	     (set reg0 (arithop reg1 ...)).
	     For a commutative operation, try (set reg0 (arithop ... reg1)).
	     Subregs in place of regs are also ok.
	     An insn with parallel sets is ok if the first set is suitable.

	     If tying is done, WIN is set nonzero.  */

	  if (GET_CODE (body) == SET
	      && (r0 = SET_DEST (body),
		  GET_CODE (r0) == REG || GET_CODE (r0) == SUBREG)
	      && (r1 = SET_SRC (body),
		  GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
	    win = combine_regs (r1, r0, b, insn_number, insn);
	  else if (GET_CODE (body) == SET)
	    {
	      r0 = SET_DEST (body);
	      if (GET_CODE (r0) == REG || GET_CODE (r0) == SUBREG)
		{
		  if (GET_RTX_FORMAT (GET_CODE (SET_SRC (body)))[0] == 'e'
		      && (r1 = XEXP (SET_SRC (body), 0),
			  GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
		    win = combine_regs (r1, r0, b, insn_number, insn);
		  if (win == 0 && commutative
		      && GET_RTX_FORMAT (GET_CODE (SET_SRC (body)))[1] == 'e'
		      && (r1 = XEXP (SET_SRC (body), 1),
			  GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
		    win = combine_regs (r1, r0, b, insn_number, insn);
		}
	    }
	  else if (GET_CODE (body) == PARALLEL)
	    {
	      rtx set1 = XVECEXP (body, 0, 0);
	      if (GET_CODE (set1) == SET 
		  && (r0 = SET_DEST (set1),
		      GET_CODE (r0) == REG || GET_CODE (r0) == SUBREG)
		  && GET_RTX_FORMAT (GET_CODE (SET_SRC (set1)))[0] == 'e'
		  && (r1 = XEXP (SET_SRC (set1), 0),
		      GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
		win = combine_regs (r1, r0, b, insn_number, insn);
	      if (win == 0 && commutative && GET_CODE (set1) == SET 
		  && (r0 = SET_DEST (set1),
		      GET_CODE (r0) == REG || GET_CODE (r0) == SUBREG)
		  && GET_RTX_FORMAT (GET_CODE (SET_SRC (set1)))[1] == 'e'
		  && (r1 = XEXP (SET_SRC (set1), 1),
		      GET_CODE (r1) == REG || GET_CODE (r1) == SUBREG))
		win = combine_regs (r1, r0, b, insn_number, insn);
	    }

	  /* If registers were just tied, set COMBINED_REGNO
	     to the number of the register used in this insn
	     that was tied to the register set in this insn.
	     This register's qty should not be "killed".  */

	  if (win)
	    {
	      while (GET_CODE (r1) == SUBREG)
		r1 = SUBREG_REG (r1);
	      combined_regno = REGNO (r1);
	    }

	  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
	    {
	      /* Mark the death of everything that dies in this instruction,
		 except for anything that was just combined.  */
	      if (XEXP (link, 0)
		  && REG_NOTE_KIND (link) == REG_DEAD
		  && combined_regno != REGNO (XEXP (link, 0)))
		{
#if 0  /* The mechanism in reg_is_set that checks whether the qty dies here
	  ought to handle this case properly.  */
		  if (combined_regno >= 0 &&
		      reg_qty[combined_regno] == reg_qty[REGNO (XEXP (link, 0))])
		    /* Here for the death of the quotient in a divmod insn:
		       something that was born and dead in this insn
		       but combined with something else that also dies here.
		       Mark the qty as dying one instruction later.  */
		    wipe_dead_reg (XEXP (link, 0), insn_number,
				   insn_number + 1);
		  else
#endif
		    wipe_dead_reg (XEXP (link, 0), insn_number, insn_number);
		}
	      /* Also, if this insn introduces a "constant" register,
		 that could just be replaced by the value it is given here
		 (which can legitimately be an immediate operand),
		 tell global-alloc not to allocate it
		 unless it is used at least twice more.  */

	      else if (REG_NOTE_KIND (link) == REG_EQUIV
		       && GET_CODE (SET_DEST (body)) == REG
		       && general_operand (XEXP (link, 0), VOIDmode)
		       /* Don't inhibit allocation of a "constant" register
			  that we have already tied to something else!  */
		       && combined_regno < 0
		       /* Don't mess with things live during setjmp.  */
		       && reg_live_length[REGNO (SET_DEST (body))] >= 0)
		{
		  i = REGNO (SET_DEST (body));
		  if (reg_n_sets[i] > 1)
		    {
		      /* Register is set in another place => not really constant.
			 cse or flow can cause this to happen.
			 Ok, forget we ever thought it was constant.  */
		      GET_MODE (link) = VOIDmode;
		    }
		  else if (reg_n_refs[i] <= 2)
		    {
		      /* For a parameter copy, do let global-alloc
			 allocate it; otherwise we would be forced to
			 have a frame pointer.  */
		      if (! frame_pointer_needed
			  && GET_CODE (SET_SRC (PATTERN (insn))) == MEM)
			reg_live_length[i] = -2;
		      else
			reg_live_length[i] = -1;

		      /* If value is not constant, we have a parameter
			 or a static chain pointer.  Tell local-alloc
			 as well not to allocate it.  */
		      if (! CONSTANT_P (SET_SRC (PATTERN (insn))))
			{
			  reg_basic_block[i] = REG_BLOCK_GLOBAL;
			  reg_qty[i] = -1;
			}
		    }
		  else
		    /* In any case, lower its priority for global-alloc.  */
		    reg_live_length[i] *= 2;
		}
	    }

	  /* Allocate qty numbers for all registers local to this block
	     that are born (set) in this instruction.
	     A pseudo that already has a qty is not changed.  */

	  note_stores (PATTERN (insn), reg_is_set);
	}
      if (GET_CODE (insn) == CALL_INSN)
	call_seen = 1;
      if (insn == basic_block_end[b])
	break;
      /* We don't need this for the block's first instruction
	 since no regs we care about are live before that instruction.
	 Also we do not allocate space in regs_live_at for that instruction. */
      IOR_HARD_REG_SET (regs_live_at[insn_number], regs_live);
      insn = NEXT_INSN (insn);
    }

  /* Now every register that is local to this basic block
     should have been given a quantity, or else -1 meaning ignore it.
     Every quantity should have a known birth (verify this now).

     If a qty's death has not been established, it indicates a dead store.
     That is ok if the insn is not entirely dead.
     So set the qty'd death to just after its birth.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_basic_block[i] == b && reg_qty[i] >= 0)
      {
	/* In the case of a register that is used uninitialized,
	   the code above will miss the actual first use.
	   So count that first use as the birth.  */  
	if (qty_birth[reg_qty[i]] > insn_map[INSN_UID (reg_first_use[i])])
	  qty_birth[reg_qty[i]] = insn_map[INSN_UID (reg_first_use[i])];
      }

  for (i = FIRST_PSEUDO_REGISTER; i < next_qty; i++)
    {
      if (qty_birth[i] == -1)
	abort ();
      if (qty_death[i] == -1)
	qty_death[i] = qty_birth[i] + 1;
    }

  /* Now order the qtys so we assign them registers
     in order of decreasing length of life.  */
  qty_order = (short *) alloca (next_qty * sizeof (short));
  for (i = FIRST_PSEUDO_REGISTER; i < next_qty; i++)
    qty_order[i] = i;

#define EXCHANGE(I1, I2)  \
  { i = qty_order[I1]; qty_order[I1] = qty_order[I2]; qty_order[I2] = i; }

  if (next_qty == 2 + FIRST_PSEUDO_REGISTER)
    {
      if (qty_compare (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1) > 0)
	EXCHANGE (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1);
    }
  else if (next_qty == 3 + FIRST_PSEUDO_REGISTER)
    {
      if (qty_compare (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1) > 0)
	EXCHANGE (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1);
      if (qty_compare (FIRST_PSEUDO_REGISTER + 1, FIRST_PSEUDO_REGISTER + 2) > 0)
	EXCHANGE (FIRST_PSEUDO_REGISTER + 2, FIRST_PSEUDO_REGISTER + 1);
      if (qty_compare (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1) > 0)
	EXCHANGE (FIRST_PSEUDO_REGISTER, FIRST_PSEUDO_REGISTER + 1);
    }
  else if (next_qty > 3 + FIRST_PSEUDO_REGISTER)
    qsort (qty_order + FIRST_PSEUDO_REGISTER,
	   next_qty - FIRST_PSEUDO_REGISTER, sizeof (short), qty_compare_1);

  /* Now for each qty that is not a hardware register,
     look for a hardware register to put it in.
     First try the register class that is cheapest for this qty,
     if there is more than one class.  */

  for (i = FIRST_PSEUDO_REGISTER; i < next_qty; i++)
    {
      q = qty_order[i];
      if (qty_size[q] >= 0)
	{
	  if (N_REG_CLASSES > 1)
	    {
	      qty_phys_reg[q] = find_free_reg (qty_min_class[q], 
					       qty_mode[q], q, 0,
					       qty_birth[q], qty_death[q]);
	      if (qty_phys_reg[q] >= 0)
		continue;
	    }

	  if (!qty_preferred_or_nothing[q])
	    qty_phys_reg[q] = find_free_reg (GENERAL_REGS, 
					     qty_mode[q], q, 0,
					     qty_birth[q], qty_death[q]);
	}
    }

  /* Now propagate the register assignments
     to the pseudo regs belonging to the qtys.  */

  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_qty[i] >= 0 && qty_phys_reg[reg_qty[i]] >= 0)
      {
	reg_renumber[i] = qty_phys_reg[reg_qty[i]] + reg_offset[i];
      }
}

/* Compare two quantities' priority for getting real registers.
   We give quantities with hard-reg suggestions priority over all others.
   We give longer-lived quantities higher priority
   so that the shorter-lived ones will tend to be in the same places
   which gives in general the maximum room for the regs to
   be allocated by global-alloc.
   Regs with more references are also preferred.  */

static int
qty_compare (q1, q2)
     int q1, q2;
{
  register int tem = (qty_phys_sugg[q2] >= 0) - (qty_phys_sugg[q1] >= 0);
  if (tem != 0) return tem;
  return -((qty_n_refs[q1] + qty_death[q1] - qty_birth[q1]) * qty_size[q2]
	   - (qty_n_refs[q2] + qty_death[q2] - qty_birth[q2]) * qty_size[q1]);
}

static int
qty_compare_1 (q1, q2)
     short *q1, *q2;
{
  register int tem = (qty_phys_sugg[*q2] >= 0) - (qty_phys_sugg[*q1] >= 0);
  if (tem != 0) return tem;
  tem = -((qty_n_refs[*q1] + qty_death[*q1] - qty_birth[*q1]) * qty_size[*q2]
	  - (qty_n_refs[*q2] + qty_death[*q2] - qty_birth[*q2]) * qty_size[*q1]);
  if (tem != 0) return tem;
  /* If qtys are equally good, sort by qty number,
     so that the results of qsort leave nothing to chance.  */
  return *q1 - *q2;
}

/* Attempt to combine the two registers (rtx's) USEDREG and SETREG.
   Returns 1 if have done so, or 0 if cannot.

   Combining registers means marking them as having the same quantity
   and adjusting the offsets within the quantity if either of
   them is a SUBREG).

   We don't actually combine a hard reg with a pseudo; instead
   we just record the hard reg as the suggestion for the pseudo's quantity.
   If we really combined them, we could lose if the pseudo lives
   across an insn that clobbers the hard reg (eg, movstr).

   There are elaborate checks for the validity of combining.  */

   
static int
combine_regs (usedreg, setreg, b, insn_number, insn)
     rtx usedreg, setreg;
     int b;
     int insn_number;
     rtx insn;
{
  register int ureg, sreg;
  register int offset = 0;
  int usize, ssize;
  register int sqty;

  /* Determine the numbers and sizes of registers being used.  */

  while (GET_CODE (usedreg) == SUBREG)
    {
      offset += SUBREG_WORD (usedreg);
      usedreg = SUBREG_REG (usedreg);
    }
  if (GET_CODE (usedreg) != REG)
    return 0;
  ureg = REGNO (usedreg);
  usize = REG_SIZE (usedreg);

  while (GET_CODE (setreg) == SUBREG)
    {
      offset -= SUBREG_WORD (setreg);
      setreg = SUBREG_REG (setreg);
    }
  if (GET_CODE (setreg) != REG)
    return 0;
  sreg = REGNO (setreg);
  ssize = REG_SIZE (setreg);

  /* Do not combine registers unless one fits within the other.  */
  if (offset > 0 && usize + offset > ssize)
    return 0;
  if (offset < 0 && usize + offset < ssize)
    return 0;
  /* Do not combine with a smaller already-assigned object
     if that smaller object is already combined with something bigger
     or if that smaller object is a hard reg.
     In the latter case, we would implicitly be using consecutive
     hard regs, and there is no code to keep track of that.
     (This is overcautious; we could check that ssize actually
     requires more hard regs at this spot.)  */
  if (ssize > usize && reg_qty[ureg] >= FIRST_PSEUDO_REGISTER
      && usize < qty_size[reg_qty[ureg]])
    return 0;

  /* Don't do anything with the non-allocatable registers.
     Also, don't suggest a call-clobberable register
     for something that must live across calls.
     Also, don't suggest a hardware register for anything larger than it.  */
  if (ureg < FIRST_PSEUDO_REGISTER)
    {
      if (fixed_regs[ureg])
	return 0;
      if (reg_n_calls_crossed[sreg] != 0 && call_used_regs[ureg])
	return 0;
      if (usize < ssize)
	return 0;
    }

  if (sreg < FIRST_PSEUDO_REGISTER)
    {
      if (fixed_regs[sreg])
	return 0;
      if (reg_n_calls_crossed[ureg] != 0 && call_used_regs[sreg])
	return 0;
      if (ssize < usize)
	return 0;
    }

  /* Don't tie something to itself.  In most cases it would make no
     difference, but it would screw up if the reg being tied to itself
     also dies in this insn.  */

  if (ureg == sreg)
    return 0; 

  /* Don't try to connect two different hardware registers.  */

  if (ureg < FIRST_PSEUDO_REGISTER && sreg < FIRST_PSEUDO_REGISTER)
    return 0;

  /* Don't connect two different machine modes if they have different
     implications as to which registers may be used.  */

  if (!MODES_TIEABLE_P (GET_MODE (usedreg), GET_MODE (setreg)))
    return 0;

  /* Now, if one of UREG and SREG is a hard reg and the other is
     a pseudo, record the hard reg as the qty_phys_sugg for the pseudo
     instead of tying them.  */
  /* Return "failure" so that the lifespan of UREG is terminated here;
     that way the two lifespans will be disjoint and nothing will prevent
     the pseudo reg from being given this hard reg.  */

  if (ureg < FIRST_PSEUDO_REGISTER)
    {
      if (reg_qty[sreg] == -2)
	reg_is_born (setreg, insn_number);
      if (reg_qty[ureg] == -2)
	reg_is_born (usedreg, insn_number);
      if (reg_qty[sreg] >= 0)
	qty_phys_sugg[reg_qty[sreg]] = ureg;
      return 0;
    }
  if (sreg < FIRST_PSEUDO_REGISTER)
    {
      if (reg_qty[sreg] == -2)
	reg_is_born (setreg, insn_number);
      if (reg_qty[ureg] == -2)
	reg_is_born (usedreg, insn_number);
      /* If UREG already has a suggested hard reg, don't override it,
	 since the most likely case is on a risc machine
	 when a pseudo gets a subroutine result and is then returned by
	 this function.  In this case, the outgoing register window
	 is probably a better place to use.  */
      if (reg_qty[ureg] >= 0
	  && (qty_phys_sugg[reg_qty[ureg]] < 0
	      /* If the old suggestion is no good, override it.  */
	      || (qty_n_calls_crossed[reg_qty[ureg]] != 0
		  && call_used_regs[qty_phys_sugg[reg_qty[ureg]]])))
	qty_phys_sugg[reg_qty[ureg]] = sreg;
      return 0;
    }

  /* Do nothing if SREG is a pseudo that already has a quantity
     or if it isn't local to this basic block or dies more than once.  */

  if (reg_qty[sreg] != -2)
    return 0;

  /* Do nothing if UREG isn't local to this block or dies more than once.
     We do this because global_alloc has no idea of tying,
     so there is no use noting those local pseudos that could
     profitably be delayed till global_alloc and get tied to global ones.  */

  if (reg_qty[ureg] == -1)
    return 0;

  /* We don't already know about SREG, so tie it to UREG
     if this is the last use of UREG, provided the classes they want
     are compatible.  */

  if (find_regno_note (insn, REG_DEAD, ureg)
      && (reg_qty[ureg] >= FIRST_PSEUDO_REGISTER
	  ? reg_meets_class_p (sreg, qty_min_class[reg_qty[ureg]])
	  : reg_meets_class_p (sreg, reg_preferred_class (ureg))))
    {
      /* If combining these two registers would leave no satisfactory
	 register available, don't do it.  */
      if (ureg >= FIRST_PSEUDO_REGISTER && sreg >= FIRST_PSEUDO_REGISTER
	  && (qty_preferred_or_nothing[reg_qty[ureg]]
	      || reg_preferred_or_nothing (sreg))
	  && ! (reg_classes_overlap_p
		(reg_preferred_class (ureg), reg_preferred_class (sreg),
		 reg_n_calls_crossed[ureg] || reg_n_calls_crossed[sreg])))
	return 0;
      if (reg_qty[ureg] == -2)
	reg_is_born (usedreg, insn_number);
      sqty = reg_qty[sreg] = reg_qty[ureg];
      if (sqty < FIRST_PSEUDO_REGISTER) abort ();
      /* If SREG's reg class is smaller, set qty_min_class[SQTY].  */
      update_qty_class (sqty, sreg);
      reg_offset[sreg] = reg_offset[ureg] + offset;
      if (sqty >= 0)
	{
	  qty_n_calls_crossed[sqty] += reg_n_calls_crossed[sreg];
	  qty_n_refs[sqty] += reg_n_refs[sreg];
	  if (! reg_preferred_or_nothing (sreg))
	    qty_preferred_or_nothing[sqty] = 0;
	  if (usize < ssize)
	    {
	      register int i;
	      for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
		if (reg_qty[i] == sqty)
		  reg_offset[i] -= offset;
	      qty_size[sqty] = ssize;
	      qty_mode[sqty] = GET_MODE (setreg);
	    }
	}
    }
  else
    return 0;

  return 1;
}

/* Return 1 if the preferred class of REG allows it to be tied
   to a quantity or register whose class is CLASS.
   True if REG's reg class either contains or is contained in CLASS.  */

static int
reg_meets_class_p (reg, class)
     int reg;
     enum reg_class class;
{
  register enum reg_class rclass = reg_preferred_class (reg);
  return (reg_class_subset_p (rclass, class)
	  || reg_class_subset_p (class, rclass));
}

/* Return nonzero if R2's preferred class is the same as or contains
   R1's preferred class.  R1 and R2 are pseudo-register numbers.  */

static int
reg_class_subset_p (c1, c2)
     register enum reg_class c1;
     register enum reg_class c2;
{
  if (c1 == c2) return 1;

  if (c2 == ALL_REGS)
  win:
    return 1;
  GO_IF_HARD_REG_SUBSET (reg_class_contents[(int)c1],
			 reg_class_contents[(int)c2],
			 win);
  return 0;
}

/* Return 1 if the two specified classes have registers in common.
   If CALL_SAVED, then consider only call-saved registers.  */

static int
reg_classes_overlap_p (c1, c2, call_saved)
     register enum reg_class c1;
     register enum reg_class c2;
     int call_saved;
{
  HARD_REG_SET c;
  int i;

  COPY_HARD_REG_SET (c, reg_class_contents[(int) c1]);
  AND_HARD_REG_SET (c, reg_class_contents[(int) c2]);

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (c, i)
	&& (! call_saved || ! call_used_regs[i]))
      return 1;

  return 0;
}

/* Update the class of QTY assuming that REG is being tied to it.  */

static void
update_qty_class (qty, reg)
     int qty;
     int reg;
{
  enum reg_class rclass = reg_preferred_class (reg);
  if (reg_class_subset_p (rclass, qty_min_class[qty]))
    qty_min_class[qty] = rclass;
}

/* Handle something which alters the value of an rtx REG.
   REG is whatever is set or clobbered.  (CLOBBER_FLAG says which.)
   If it is not really a register, we do nothing.
   The file-global variables `this_insn' and `this_insn_number'
   carry info from `block_alloc'.  */

static void
reg_is_set (reg, setter)
     rtx reg;
     rtx setter;
{
  register int regno;
  int clobber_flag = GET_CODE (setter) == CLOBBER;

  if (reg == 0 || GET_CODE (reg) != REG)
    return;

  regno = REGNO (reg);

  if (regno < FIRST_PSEUDO_REGISTER)
    {
      /* A hard reg is set or clobbered.
	 Mark it as live at the moment immediately following this insn
	 so that no pseudo can live here at that time.
	 For a CLOBBER, mark it as live before this insn,
	 to make sure it is free during the entire insn.  */

      register int lim = regno + HARD_REGNO_NREGS (regno, GET_MODE (reg));
      register int i;
      for (i = regno; i < lim; i++)
	{
	  SET_HARD_REG_BIT (regs_live_at[this_insn_number], i);
	  if (clobber_flag)
	    SET_HARD_REG_BIT (regs_live_at[this_insn_number - 1], i);
	}

      /* If the hard reg is given a useful value
	 and it does not die in this insn,
	 mark it as live indefinitely afterward.  */
      if (! clobber_flag
	  && ! find_regno_note (this_insn, REG_DEAD, regno))
	reg_is_born (reg, this_insn_number);
    }
  else if (! clobber_flag)
    {
      /* A pseudo-reg is set (not just clobbered).  */

      reg_is_born (reg, this_insn_number);

      /* If a pseudo register dies in the same insn that sets it,
	 say it dies in the following insn instead,
	 because it will have to be live right after this insn.  */
      if (qty_death[reg_qty[regno]] == this_insn_number)
	{
	  /* Calls to post_mark_life and mark_life deleted here.
	     They only know how to handle hard regs.  */
	  qty_death[reg_qty[regno]]++;
	}
    }
  else if (reg_qty[regno] >= 0 && qty_death[reg_qty[regno]] == this_insn_number
	   && qty_birth[reg_qty[regno]] == this_insn_number)
    {
      /* A psuedo-reg is clobbered by this insn and was born and dies here.
	 This is a temporary required for this insn and so will
	 conflict with any other live registers at this point.  We must
	 assume that this register is used before all the inputs of the
	 insn are dead.  So this register must not conflict with any of them.
	 Mark it as born at the previous insn.  */
      qty_birth[reg_qty[regno]]--;
      /* It should also conflict with this insn's outputs.  */
      qty_death[reg_qty[regno]]++;
    }
}

/* Handle beginning of the life of register REG.
   INSN_NUMBER is the insn at which this is happening.  */

static void
reg_is_born (reg, insn_number)
     rtx reg;
     int insn_number;
{
  register int regno = REGNO (reg);
     
  if (regno < FIRST_PSEUDO_REGISTER)
    mark_life (regno, GET_MODE (reg), 1);
  else if (reg_qty[regno] == -2)
    alloc_qty (regno, GET_MODE (reg), PSEUDO_REGNO_SIZE (regno), insn_number);
}

/* Record the death in insn DEATH_INSN_NUMBER for the register REG.  */

static void
wipe_dead_reg (reg, this_insn_number, death_insn_number)
     register rtx reg;
     int this_insn_number;
     int death_insn_number;
{
  register int regno = REGNO (reg);

  if (regno < FIRST_PSEUDO_REGISTER)
    {
      mark_life (regno, GET_MODE (reg), 0);
      if (this_insn_number != death_insn_number)
	abort ();
#if 0				/* Should never get here */
	post_mark_life (regno, GET_MODE (reg), 1,
			this_insn_number, death_insn_number);
#endif
    }
  else
    {
      /* If a pseudo reg is referred to but was never set,
	 we will find here that its qty is -2.
	 Since these regs do not conflict with anything,
	 mark them as born and dead in the same place.  */
      if (reg_qty[regno] == -2)
	{
	  alloc_qty (regno, GET_MODE (reg), REG_SIZE (reg), this_insn_number);
	  REG_NOTES (this_insn) = gen_rtx (EXPR_LIST, REG_UNSET, reg,
					   REG_NOTES (this_insn));
	}

      if (reg_qty[regno] >= 0)
	qty_death[reg_qty[regno]] = death_insn_number;
    }
}

/* Find a block of SIZE words of hard regs in reg_class CLASS
   that can hold something of machine-mode MODE
     (but actually we test only the first of the block for holding MODE)
   and still free between insn BORN_INSN and insn DEAD_INSN,
   and return the number of the first of them.
   Return -1 if such a block cannot be found. 
   If QTY crosses calls, insist on a register preserved by calls,
   unless ACCEPT_CALL_CLOBBERED is nonzero.  */

static int
find_free_reg (class, mode, qty, accept_call_clobbered, born_insn, dead_insn)
     enum reg_class class;
     enum machine_mode mode;
     int accept_call_clobbered;
     int qty;
     int born_insn, dead_insn;
{
  register int i, ins;
#ifdef HARD_REG_SET
  register		/* Declare it register if it's a scalar.  */
#endif
    HARD_REG_SET used;

  if (accept_call_clobbered)
    COPY_HARD_REG_SET (used, call_fixed_reg_set);
  else if (qty_n_calls_crossed[qty] == 0)
    COPY_HARD_REG_SET (used, fixed_reg_set);
  else
    COPY_HARD_REG_SET (used, call_used_reg_set);

  for (ins = born_insn; ins < dead_insn; ins++)
    IOR_HARD_REG_SET (used, regs_live_at[ins]);

  IOR_COMPL_HARD_REG_SET (used, reg_class_contents[(int) class]);
  /* Don't use the frame pointer reg in local-alloc even if
     we may omit the frame pointer, because if we do that and then we
     need a frame pointer, reload won't know how to move the pseudo
     to another hard reg.  It can move only regs made by global-alloc.  */
  SET_HARD_REG_BIT (used, FRAME_POINTER_REGNUM);

  /* If quantity QTY has a suggested physical register,
     try that one first.  */

  if (qty_phys_sugg[qty] >= 0)
    {
      i = qty_phys_sugg[qty];
      if (! TEST_HARD_REG_BIT (used, i)
	  && HARD_REGNO_MODE_OK (i, mode))
	{
	  register int j;
	  register int size1 = HARD_REGNO_NREGS (i, mode);
	  for (j = 1; j < size1 && ! TEST_HARD_REG_BIT (used, i + j); j++);
	  if (j == size1)
	    {
	      post_mark_life (i, mode, 1, born_insn, dead_insn);
	      return i;
	    }
	}
    }

  /* If that doesn't find one, test each hard reg.  */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    {
#ifdef REG_ALLOC_ORDER
      int regno = reg_alloc_order[i];
#else
      int regno = i;
#endif
      if (! TEST_HARD_REG_BIT (used, regno)
	  && HARD_REGNO_MODE_OK (regno, mode))
	{
	  register int j;
	  register int size1 = HARD_REGNO_NREGS (regno, mode);
	  for (j = 1; j < size1 && ! TEST_HARD_REG_BIT (used, regno + j); j++);
	  if (j == size1)
	    {
	      post_mark_life (regno, mode, 1, born_insn, dead_insn);
	      return regno;
	    }
#ifndef REG_ALLOC_ORDER
	  i += j;		/* Skip starting points we know will lose */
#endif
	}
    }

  /* If it would be profitable to allocate a call-clobbered register
     and save and restore it around calls, do that.  */

  if (! accept_call_clobbered
      && flag_caller_saves
      && qty_n_calls_crossed[qty] != 0
      && CALLER_SAVE_PROFITABLE (qty_n_refs[qty], qty_n_calls_crossed[qty]))
    {
      i = find_free_reg (class, mode, qty, 1, born_insn, dead_insn);
      if (i >= 0)
	caller_save_needed = 1;
      return i;
    }
  return -1;
}

static void
mark_life (regno, mode, life)
     register int regno;
     enum machine_mode mode;
     int life;
{
  register int j = HARD_REGNO_NREGS (regno, mode);
  if (life)
    while (--j >= 0)
      SET_HARD_REG_BIT (regs_live, regno + j);
  else
    while (--j >= 0)
      CLEAR_HARD_REG_BIT (regs_live, regno + j);
}

static void
post_mark_life (regno, mode, life, birth, death)
     register int regno, life, birth;
     enum machine_mode mode;
     int death;
{
  register int j = HARD_REGNO_NREGS (regno, mode);
#ifdef HARD_REG_SET
  register		/* Declare it register if it's a scalar.  */
#endif
    HARD_REG_SET this_reg;

  CLEAR_HARD_REG_SET (this_reg);
  while (--j >= 0)
    SET_HARD_REG_BIT (this_reg, regno + j);

  /* If a reg is born and dies in one insn,
     consider it live after that insn.  */

  if (birth == death)
    death++;

  if (life)
    while (birth < death)
      {
	IOR_HARD_REG_SET (regs_live_at[birth], this_reg);
	birth++;
      }
  else
    while (birth < death)
      {
	AND_COMPL_HARD_REG_SET (regs_live_at[birth], this_reg);
	birth++;
      }
}

void
dump_local_alloc (file)
     FILE *file;
{
  register int i;
  for (i = FIRST_PSEUDO_REGISTER; i < max_regno; i++)
    if (reg_renumber[i] != -1)
      fprintf (file, ";; Register %d in %d.\n", i, reg_renumber[i]);
}
