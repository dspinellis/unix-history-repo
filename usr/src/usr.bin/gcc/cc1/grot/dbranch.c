/* Delayed branch scheduling pass.
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */

/*  Delayed Branch Scheduling Optimization

If the HAVE_DELAYED_BRANCH macro is defined in the machine 
description, this code is called by toplev.c during optimizing 
compilation immediately after the final jump optimization pass
and just before assembler output generation, if delayed branch
scheduling is requested with the -fdelayed-branch switch.

Machines with delayed branch allow one or more instructions
placed *after* a branch instruction to be executed while the
hardware is off fetching the next instruction.  These instructions
are executed after the branch is issued, but before the branch 
actually takes effect.  The decision as to whether or not
the branch is to be taken, and the address of the branch target
are fixed at the time the branch is issued, so only instructions
that do not appear in the dependency graphs for computing the 
branch decision and/or target address may be relocated "after" 
the branch.  Some machines might have additional restrictions,
such as not allowing memory instructions or condition code
modification in the delay sequence.

Note that this scheduling pass occurs after register allocation, and
(of course) final jump optimization.  This mechanism is *not* intended
to be hacked to deal with similar memory-latency pipeline scheduling
(i.e. slots after loads/stores), as tempting as that might be.  The
right place to do load-store latency scheduling is prior to register
allocation, since allocation may introduce artificial dependencies
that could have been avoided; note that these artificial dependencies
are *not* reflected in the flow information, which is one reason for
the somewhat ad hoc analysis done in this pass. 

The strategy and methods used are as follows.  The function DBR_SCHEDULE
is called from toplev.c if the scheduling pass is to be run.  That function
sets up the dump file, then scans the current function from top to bottom
for "d-blocks", which are like basic blocks (single-entry, single-exit),
with the additional condition that the last instruction in the block has
delay slots.  Note that if calls have slots, d-blocks can be smaller than
basic blocks.  If a basic block does not end with a delay-instruction,
it is skipped.

To re-order instructions in a d-block (see DBR_DBLOCK_SCHED), the scheduler
scans backward from the "d-instruction", trying to fill the slots.  The
scheduler is somewhat conservative.  Volatile memory references are
serialized (their order is never changed to avoid possible aliasing
problems).  Definitions of registers are serialized (so there is no
possibility of deadlock).  Since hard register dependencies are
not noted by flow analysis, the scheduler does its own simplified
tracking of the registers, memory, and condition code uses/defines
by the d-instruction and the instructions it depends on).  Information
available from flow analysis is used to shortcut the analysis where
possible.  

Since only data dependencies are considered by the scheduler, any
machine-specific restrictions, e.g. to keep memory instructions from
being scheduled into slots, must be explicit in the definition of
DBR_INSN_ELIGIBLE_P.

The scheduler scans backwards over the block, looking for eligible
insns to fill the slot(s).  If none are found, nothing is done, and no
changes are made to the code.  As eligible insns are found, they are
removed from the chain, and recorded in an INSN_LIST rtx.  When all
slots are full (or the top of the d-block is reached), the *pattern*
of the d-insn is replaced with a SEQUENCE rtx, which consists of
a copy of the original d-insn followed by the slot fillers.  Slot
filling instructions remain in the original relative order in the
sequence.

When the SEQUENCE pattern is encountered by final, the instructions
are output "normally", though the output code for the instructions
may test for this and alter their behavior appropriately.

*/

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "hard-reg-set.h"
#include "flags.h"

FILE *dbr_dump_file;

/* The number of unfilled delay slots in the current sequence. */
static int slots_avail;

/* A flag, nonzero indicating that some insn that could not 
   go in a slot writes to memory.  */

static int memw;

/* A flag, nonzero indicating that the condition code is written 
   by some insn that couldn't go in a delay slot.  */

static int ccw;

/* Each bit is nonzero if the corresponding hard register
   is written by an insn that couldn't go in a delay slot.  */

static HARD_REG_SET regw;

/* A flag, set nonzero if ENOTE determines that
   the current insn can't go in a delay slot because of a
   data dependency detected by note_stores.  */

static int eflag;

/* The insn having delay slots.  Global because of the calls through
   note_stores that need it.  */

static rtx dinsn;

/* The insn being currently considered for a delay slot.  */

static rtx insn;

/* An INSN_LIST (just like the insn field) that we use to hold
   LOG_LINKS of ineligible insns.  We use what flow analysis 
   stuff we can - this prevents exhaustive searches for write-read
   dependencies in most cases.  This tactic only loses on reloads
   and code generated with hard regs (instead of pseudos).  */

static rtx dep_insn_list;

/* Called by note_stores on "ineligible" insns to keep track of
   pre-branch dependencies.  */
static void
pnote (x, in)
     rtx x;
     rtx in;
{
  switch (GET_CODE (x))
    {
    case REG:
      if (GET_CODE (in) != SET
	  || GET_CODE (SET_SRC (in)) != CALL)
	SET_HARD_REG_BIT (regw, REGNO (x));
      return;
    case MEM:
      memw = TRUE; /* this might be relaxed somewhat later */
      return;
    case CC0:
      ccw = TRUE;
      return;
    case PC:
      return;
    default:
      abort (); /* should never happen */
    }
}

/*  The d-block end insn is in DINSN.  Initialize the flags to
    start building the delay sequence.  Calls PNOTE from note_stores
    to track the written registers and memory.      */

static void
init_flags ()
{
  CLEAR_HARD_REG_SET (regw);
  memw = ccw = 0;
  note_stores (PATTERN (dinsn), pnote);
  if (LOG_LINKS (dinsn))
    dep_insn_list = copy_rtx (LOG_LINKS (dinsn));
  else
    dep_insn_list = 0;
  slots_avail = DBR_SLOTS_AFTER (dinsn);
}


/* Called through note_stores on possibly eligible insn patterns.
   Checks to see if a register written by the pattern is needed by an already
   ineligible insn.  Sets the global EFLAG nonzero if a dependency
   is found.  */

static void 
enote (x, p)
     rtx x;
     rtx p;
{
  if (eflag == 0)
    {
      if (GET_CODE (x) == REG)
	{
	  if (reg_used_between_p (x, insn, dinsn))
	    goto lose;
	  if ((!FUNCTION_VALUE_REGNO_P (REGNO (x)) || 
	       GET_CODE (dinsn) != CALL_INSN) &&
	      reg_mentioned_p (x, (PATTERN (dinsn))))
	    goto lose;
	}
      else if (x == cc0_rtx && 
	       reg_used_between_p (x, insn, NEXT_INSN (dinsn)))
	goto lose;
      return;
    lose:
      eflag = 1;
    }
}

/*  Search the current dependency list DEP_INSN_LIST for INSN,
    return nonzero if found. */

static int
in_dep_list_p (insn)
     rtx insn;
{
  rtx l;
  for (l = dep_insn_list; l ; l = XEXP (l, 1))
    if (insn == XEXP (l, 0)) return 1;
  return 0;
}

/* Returns zero if INSN is ineligible to be put in a delay slot
   of DINSN.  INSN is ineligible if it:
     - is in the dependency list of an ineligible insn.
     - writes a hard register needed by an ineligible insn.
     - reads a register written by an ineligible insn.
     - refers to memory.
     - sets the condition code.    
     - violates a machine-dependent constraint.  */

static int
insn_eligible_p ()
{
  rtx dest;
  rtx pat = PATTERN (insn);
  int i,s;

  /* See if there are any explicit dependencies on this insn. */
  if (in_dep_list_p (insn))
    return 0;
  
  /* Check for implicit dependencies by calling enote on each
     store rtx.  ENOTE makes sure that no ineligible instruction
     refers to a register in a way that flow analysis 
     has missed or ignored.         */
  eflag = 0;
  note_stores (PATTERN (insn), enote);
  if (eflag)
    return 0;

  /* Check for volatile memory refs if any already ineligible. */

  if (memw && volatile_refs_p (pat))
    {
      memw = TRUE;
      return 0;
    }

  /* See if it refers to any regs that are clobbered by ineligibles. */

  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    if (TEST_HARD_REG_BIT (regw, i) 
	&& refers_to_regno_p (i, i + 1, pat, 0))
      return 0;

#ifdef DBR_INSN_ELIGIBLE_P
  /*  Check for arbitrary machine constraints if any. */
  if (! DBR_INSN_ELIGIBLE_P (insn, dinsn))
    return 0;
#endif

  return 1;
}

/* Add the links in LIST to the dependency list.  We put them
   at the front since this should make searches faster in long
   d-blocks.
*/
static void 
prepend_to_dep_list (list)
     rtx list;
{
  rtx l = copy_rtx (list);
  while (XEXP (l, 1) != 0)
    l = XEXP (l, 1);
  XEXP (l, 1) = dep_insn_list;
  dep_insn_list = l;
}
  


/* Update the flags for ineligible INSN - it can't be put in a delay
slot.  This involves setting bits to indicate the stores of INSN, and
adding any flow-analysis dependencies of INSN's insn-list to
the ineligible list.  (Should ultimately catch reloads too.) */

static void 
update_flags (insn)
     rtx insn;
{
  rtx l;
  note_stores (PATTERN (insn), pnote);
  if (l = LOG_LINKS (insn))
    prepend_to_dep_list (l);
}

/* Put INSN and LIST together in a SEQUENCE rtx of LENGTH, and replace
   the pattern of INSN with the SEQUENCE.  Include the available
   slots AVAIL in the SEQUENCE insn.  */
static void
emit_delay_sequence (insn, list, length, avail)
     rtx insn;
     rtx list;
     int length;
     int avail;
{
  register int i = 1;
  register rtx li, tem;
  /* Allocate the the rtvec to hold the insns and the SEQUENCE. */
  rtvec seqv = rtvec_alloc (length + 1);
  rtx seq = gen_rtx (SEQUENCE, VOIDmode, seqv);

  /* Make a copy of the insn having delay slots. */
  tem = copy_rtx (insn);
  NEXT_INSN (tem) = 0;
  PREV_INSN (tem) = 0;
  /* Replace the original pattern with a sequence whose
     first insn is the copy. */
  PATTERN (insn) = seq;
  INSN_CODE (insn) = -1;
  XVECEXP (seq, 0, 0) = tem;
  /* Copy in the delay-slot filling insns. */
  for (li = list; li; li = XEXP (li, 1))
    {
      XVECEXP (seq, 0, i) = XEXP (li, 0);
      i++;
    }
}

/*  Try to reorganize code in a d-block */

static void
dbr_dblock_sched (first, last)
     rtx first, last;
{ 
  rtx delay_insn_list = 0;
  int seq_len = 0;
  dinsn = last;
  if (first == last) return;
  init_flags ();
  insn = PREV_INSN (dinsn);
  while (1)
    {
      rtx prev = PREV_INSN (insn);
      rtx next = NEXT_INSN (insn);
      if (GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) != USE
	  && GET_CODE (PATTERN (insn)) != CLOBBER
	  && GET_CODE (PATTERN (insn)) != ADDR_VEC
	  && GET_CODE (PATTERN (insn)) != ADDR_DIFF_VEC)
	{
	  if (slots_avail >= DBR_INSN_SLOTS (insn) && insn_eligible_p ())
	    {
	      /* Add this insn to the delay sequence and
		 update the number of slots available. */
	      register rtx t = delay_insn_list;
	      delay_insn_list = gen_rtx (INSN_LIST, VOIDmode, insn, t);
	      seq_len++;
	      slots_avail -= DBR_INSN_SLOTS (insn);

	      /* Now remove it from the chain. */
	      NEXT_INSN (prev) = next;
	      PREV_INSN (next) = prev;
	      NEXT_INSN (insn) = PREV_INSN (insn) = 0;
	    }
	  else
	    update_flags (insn);
	}
      else
	if (GET_CODE (insn) != NOTE)
	  abort ();
      if (slots_avail == 0 || insn == first)
	break;
      else
	insn = prev;
    }
  /* Done.  If the delay list is non-empty, emit a sequence
     in place of the dinsn.  */
  if (delay_insn_list != 0)
    emit_delay_sequence (dinsn, delay_insn_list, seq_len, slots_avail);
}


/*
Identify d-blocks of a function, which are sort of like basic
blocks, except that any instruction with delay slots defines the end
of a dblock, and dblocks that do not end in delay-instructions are
uninteresting degenerate cases.

This function finds d-blocks in the code for a function, and calls
dbr_dblock_sched on non-degenerate blocks.  Called from toplev.c
if HAVE_DELAYED_BRANCH is defined and we are doing optimizing
compilation.   F is the first insn of the function, DUMP_FILE
is the file to output debugging info on if requested.  */

void
dbr_schedule (f, dump_file)
     rtx f;
     FILE *dump_file;
{
  rtx first = f;
  rtx insn;
  /* Dump output if requested */
  if (dbr_dump_file = dump_file)
    fprintf (dbr_dump_file, "Delayed-branch reordering dump.\n");

  /* Search for d-blocks by scanning the insns from top to bottom. */
  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      if (DBR_SLOTS_AFTER (insn) > 0)
	{
	  /* An insn with delay slots always terminates a d-block.
	     Call the scheduler to fill in the slots if possible. */
	  dbr_dblock_sched (first, insn);
	  
	  /* Resume scanning after the end of the sequence. */
	  first = NEXT_INSN (dinsn);
	}
      else
	/* Not an end of a real d-block, but need to check
	   if it is the end of a degenerate one.  Note that
	   calls or jumps will only reach here if they aren't
	   delayed instructions.              */

	if (GET_CODE (insn) == CODE_LABEL ||
	    GET_CODE (insn) == JUMP_INSN ||
	    GET_CODE (insn) == CALL_INSN)
	  first = NEXT_INSN (insn);	    
    }
}
