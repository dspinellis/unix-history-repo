/* Expands front end tree to back end RTL for GNU C-Compiler
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


/* This file handles the generation of rtl code from tree structure
   above the level of expressions, using subroutines in exp*.c and emit-rtl.c.
   It also creates the rtl expressions for parameters and auto variables
   and has full responsibility for allocating stack slots.

   The functions whose names start with `expand_' are called by the
   parser to generate RTL instructions for various kinds of constructs.

   Some control and binding constructs require calling several such
   functions at different times.  For example, a simple if-then
   is expanded by calling `expand_start_cond' (with the condition-expression
   as argument) before parsing the then-clause and calling `expand_end_cond'
   after parsing the then-clause.

   `expand_function_start' is called at the beginning of a function,
   before the function body is parsed, and `expand_function_end' is
   called after parsing the body.

   Call `assign_stack_local' to allocate a stack slot for a local variable.
   This is usually done during the RTL generation for the function body,
   but it can also be done in the reload pass when a pseudo-register does
   not get a hard register.

   Call `put_var_into_stack' when you learn, belatedly, that a variable
   previously given a pseudo-register must in fact go in the stack.
   This function changes the DECL_RTL to be a stack slot instead of a reg
   then scans all the RTL instructions so far generated to correct them.  */

#include "config.h"

#include <stdio.h>

#include "rtl.h"
#include "tree.h"
#include "flags.h"
#include "insn-flags.h"
#include "insn-config.h"
#include "insn-codes.h"
#include "expr.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "recog.h"

#define MAX(x,y) (((x) > (y)) ? (x) : (y))
#define MIN(x,y) (((x) < (y)) ? (x) : (y))

/* Nonzero if function being compiled pops its args on return.
   May affect compilation of return insn or of function epilogue.  */

int current_function_pops_args;

/* Nonzero if function being compiled needs to be given an address
   where the value should be stored.  */

int current_function_returns_struct;

/* Nonzero if function being compiled needs to
   return the address of where it has put a structure value.  */

int current_function_returns_pcc_struct;

/* Nonzero if function being compiled needs to be passed a static chain.  */

int current_function_needs_context;

/* Nonzero if function being compiled can call setjmp.  */

int current_function_calls_setjmp;

/* Nonzero if function being compiled can call alloca,
   either as a subroutine or builtin.  */

int current_function_calls_alloca;

/* Nonzero if the current function returns a pointer type */

int current_function_returns_pointer;

/* If function's args have a fixed size, this is that size, in bytes.
   Otherwise, it is -1.
   May affect compilation of return insn or of function epilogue.  */

int current_function_args_size;

/* # bytes the prologue should push and pretend that the caller pushed them.
   The prologue must do this, but only if parms can be passed in registers.  */

int current_function_pretend_args_size;

/* This is the offset from the arg pointer to the place where the first
   anonymous arg can be found, if there is one.  */
rtx current_function_arg_offset_rtx;

/* Name of function now being compiled.  */

char *current_function_name;

/* Label that will go on parm cleanup code, if any.
   Jumping to this label runs cleanup code for parameters, if
   such code must be run.  Following this code is the logical return label.  */

rtx cleanup_label;

/* Label that will go on function epilogue.
   Jumping to this label serves as a "return" instruction
   on machines which require execution of the epilogue on all returns.  */

rtx return_label;

/* List (chain of EXPR_LISTs) of pseudo-regs of SAVE_EXPRs.
   So we can mark them all live at the end of the function, if nonopt.  */
rtx save_expr_regs;

/* List (chain of EXPR_LISTs) of all stack slots in this function.
   Made for the sake of unshare_all_rtl.  */
rtx stack_slot_list;

/* Filename and line number of last line-number note,
   whether we actually emitted it or not.  */
char *emit_filename;
int emit_lineno;

/* Insn after which register parms and SAVE_EXPRs are born, if nonopt.  */
static rtx parm_birth_insn;

/* The FUNCTION_DECL node for the function being compiled.  */

static tree this_function;

/* Number of binding contours started so far in this function.  */

static int block_start_count;

/* Offset to end of allocated area of stack frame.
   If stack grows down, this is the address of the last stack slot allocated.
   If stack grows up, this is the address for the next slot.  */
static int frame_offset;

/* Nonzero if a stack slot has been generated whose address is not
   actually valid.  It means that the generated rtl must all be scanned
   to detect and correct the invalid addresses where they occur.  */
static int invalid_stack_slot;

/* Label to jump back to for tail recursion, or 0 if we have
   not yet needed one for this function.  */
static rtx tail_recursion_label;

/* Place after which to insert the tail_recursion_label if we need one.  */
static rtx tail_recursion_reentry;

/* Each time we expand an expression-statement,
   record the expr's type and its RTL value here.  */

static tree last_expr_type;
static rtx last_expr_value;

/* Chain of all RTL_EXPRs that have insns in them.  */
static tree rtl_expr_chain;

/* Last insn of those whose job was to put parms into their nominal homes.  */
static rtx last_parm_insn;

/* Functions and data structures for expanding case statements.  */

/* Case label structure, used to hold info on labels within case
   statements.  We handle "range" labels; for a single-value label
   as in C, the high and low limits are the same.  */

struct case_node
{
  struct case_node	*left;
  struct case_node	*right;
  struct case_node	*parent;
  tree			low;
  tree			high;
  tree			test_label;
  tree			code_label;
};

typedef struct case_node case_node;
typedef struct case_node *case_node_ptr;

static void balance_case_nodes ();
static void emit_case_nodes ();
static void group_case_nodes ();
static void emit_jump_if_reachable ();

/* Stack of control and binding constructs we are currently inside.

   These constructs begin when you call `expand_start_WHATEVER'
   and end when you call `expand_end_WHATEVER'.  This stack records
   info about how the construct began that tells the end-function
   what to do.  It also may provide information about the construct
   to alter the behavior of other constructs within the body.
   For example, they may affect the behavior of C `break' and `continue'.

   Each construct gets one `struct nesting' object.
   All of these objects are chained through the `all' field.
   `nesting_stack' points to the first object (innermost construct).
   The position of an entry on `nesting_stack' is in its `depth' field.

   Each type of construct has its own individual stack.
   For example, loops have `loop_stack'.  Each object points to the
   next object of the same type through the `next' field.

   Some constructs are visible to `break' exit-statements and others
   are not.  Which constructs are visible depends on the language.
   Therefore, the data structure allows each construct to be visible
   or not, according to the args given when the construct is started.
   The construct is visible if the `exit_label' field is non-null.
   In that case, the value should be a CODE_LABEL rtx.  */

struct nesting
{
  struct nesting *all;
  struct nesting *next;
  int depth;
  rtx exit_label;
  union
    {
      /* For conds (if-then and if-then-else statements).  */
      struct
	{
	  /* Label on the else-part, if any, else 0.  */
	  rtx else_label;
	  /* Label at the end of the whole construct.  */
	  rtx after_label;
	} cond;
      /* For loops.  */
      struct
	{
	  /* Label at the top of the loop; place to loop back to.  */
	  rtx start_label;
	  /* Label at the end of the whole construct.  */
	  rtx end_label;
	  /* Label for `continue' statement to jump to;
	     this is in front of the stepper of the loop.  */
	  rtx continue_label;
	} loop;
      /* For variable binding contours.  */
      struct
	{
	  /* Sequence number of this binding contour within the function,
	     in order of entry.  */
	  int block_start_count;
	  /* Nonzero => value to restore stack to on exit.  */
	  rtx stack_level;
	  /* The NOTE that starts this contour.
	     Used by expand_goto to check whether the destination
	     is within each contour or not.  */
	  rtx first_insn;
	  /* Innermost containing binding contour that has a stack level.  */
	  struct nesting *innermost_stack_block;
	  /* List of cleanups to be run on exit from this contour.
	     This is a list of expressions to be evaluated.
	     The TREE_PURPOSE of each link is the ..._DECL node
	     which the cleanup pertains to.  */
	  tree cleanups;
	  /* List of cleanup-lists of blocks containing this block,
	     as they were at the locus where this block appears.
	     There is an element for each containing block,
	     ordered innermost containing block first.
	     The element's TREE_VALUE is the cleanup-list of that block,
	     which may be null.  */
	  tree outer_cleanups;
	  /* Chain of labels defined inside this binding contour.
	     For contours that have stack levels or cleanups.  */
	  struct label_chain *label_chain;
	} block;
      /* For switch (C) or case (Pascal) statements,
	 and also for dummies (see `expand_start_case_dummy').  */
      struct
	{
	  /* The insn after which the case dispatch should finally
	     be emitted.  Zero for a dummy.  */
	  rtx start;
	  /* A list of case labels, kept in ascending order by value
	     as the list is built.
	     During expand_end_case, this list may be rearranged into a
	     nearly balanced binary tree.  */
	  struct case_node *case_list;
	  /* Label to jump to if no case matches.  */
	  tree default_label;
	  /* The expression to be dispatched on.  */
	  tree index_expr;
	  /* Type that INDEX_EXPR should be converted to.  */
	  tree nominal_type;
	  /* Number of range exprs in case statement.  */
	  short num_ranges;
	} case_stmt;
    } data;
};

/* Chain of all pending binding contours.  */
struct nesting *block_stack;

/* Chain of all pending binding contours that restore stack levels
   or have cleanups.  */
struct nesting *stack_block_stack;

/* Chain of all pending conditional statements.  */
struct nesting *cond_stack;

/* Chain of all pending loops.  */
struct nesting *loop_stack;

/* Chain of all pending case or switch statements.  */
struct nesting *case_stack;

/* Separate chain including all of the above,
   chained through the `all' field.  */
struct nesting *nesting_stack;

/* Number of entries on nesting_stack now.  */
int nesting_depth;

/* Pop one of the sub-stacks, such as `loop_stack' or `cond_stack';
   and pop off `nesting_stack' down to the same level.  */

#define POPSTACK(STACK)					\
do { int initial_depth = nesting_stack->depth;		\
     do { struct nesting *this = STACK;			\
	  STACK = this->next;				\
	  nesting_stack = this->all;			\
	  nesting_depth = this->depth;			\
	  free (this); }				\
     while (nesting_depth > initial_depth); } while (0)

static int warn_if_unused_value ();
static void expand_goto_internal ();
static int expand_fixup ();
static void fixup_gotos ();
static void expand_cleanups ();
static void fixup_cleanups ();
static void expand_null_return_1 ();
static int tail_recursion_args ();
static void fixup_stack_slots ();
static rtx fixup_stack_1 ();
static rtx fixup_memory_subreg ();
static rtx walk_fixup_memory_subreg ();
static void fixup_var_refs ();
static void fixup_var_refs_insns ();
static rtx fixup_var_refs_1 ();
static rtx parm_stack_loc ();
static void optimize_bit_field ();
static void do_jump_if_equal ();

/* Emit a no-op instruction.  */

rtx
emit_nop ()
{
  rtx last_insn = get_last_insn ();
  if (!optimize
      && (GET_CODE (last_insn) == CODE_LABEL
	  || prev_real_insn (last_insn) == 0))
    emit_insn (gen_nop ());
}

/* Return the rtx-label that corresponds to a LABEL_DECL,
   creating it if necessary.  */

static rtx
label_rtx (label)
     tree label;
{
  if (TREE_CODE (label) != LABEL_DECL)
    abort ();

  if (DECL_RTL (label))
    return DECL_RTL (label);

  return DECL_RTL (label) = gen_label_rtx ();
}

/* Add an unconditional jump to LABEL as the next sequential instruction.  */

void
emit_jump (label)
     rtx label;
{
  do_pending_stack_adjust ();
  emit_jump_insn (gen_jump (label));
  emit_barrier ();
}

/* Handle goto statements and the labels that they can go to.  */

/* In some cases it is impossible to generate code for a forward goto 
   until the label definition is seen.  This happens when it may be necessary
   for the goto to reset the stack pointer: we don't yet know how to do that.
   So expand_goto puts an entry on this fixup list.
   Each time a binding contour that resets the stack is exited,
   we check each fixup.
   If the target label has now been defined, we can insert the proper code.  */

struct goto_fixup
{
  /* Points to following fixup.  */
  struct goto_fixup *next;
  /* Points to the insn before the jump insn.
     If more code must be inserted, it goes after this insn.  */
  rtx before_jump;
  /* The LABEL_DECL that this jump is jumping to, or 0
     for break, continue or return.  */
  tree target;
  /* The CODE_LABEL rtx that this is jumping to.  */
  rtx target_rtl;
  /* Number of binding contours started in current function
     before the label reference.  */
  int block_start_count;
  /* The outermost stack level that should be restored for this jump.
     Each time a binding contour that resets the stack is exited,
     if the target label is *not* yet defined, this slot is updated.  */
  rtx stack_level;
  /* List of lists of cleanup expressions to be run by this goto.
     There is one element for each block that this goto is within.
     The TREE_VALUE contains the cleanup list of that block as of the
     time this goto was seen.
     The TREE_ADDRESSABLE flag is 1 for a block that has been exited.  */
  tree cleanup_list_list;
};

static struct goto_fixup *goto_fixup_chain;

/* Within any binding contour that must restore a stack level,
   all labels are recorded with a chain of these structures.  */

struct label_chain
{
  /* Points to following fixup.  */
  struct label_chain *next;
  tree label;
};

/* Specify the location in the RTL code of a label BODY,
   which is a LABEL_DECL tree node.

   This is used for the kind of label that the user can jump to with a
   goto statement, and for alternatives of a switch or case statement.
   RTL labels generated for loops and conditionals don't go through here;
   they are generated directly at the RTL level, by other functions below.

   Note that this has nothing to do with defining label *names*.
   Languages vary in how they do that and what that even means.  */

void
expand_label (body)
     tree body;
{
  struct label_chain *p;

  do_pending_stack_adjust ();
  emit_label (label_rtx (body));

  if (stack_block_stack != 0)
    {
      p = (struct label_chain *) oballoc (sizeof (struct label_chain));
      p->next = stack_block_stack->data.block.label_chain;
      stack_block_stack->data.block.label_chain = p;
      p->label = body;
    }
}

/* Generate RTL code for a `goto' statement with target label BODY.
   BODY should be a LABEL_DECL tree node that was or will later be
   defined with `expand_label'.  */

void
expand_goto (body)
     tree body;
{
  expand_goto_internal (body, label_rtx (body), 0);
}

/* Generate RTL code for a `goto' statement with target label BODY.
   LABEL should be a LABEL_REF.
   LAST_INSN, if non-0, is the rtx we should consider as the last
   insn emitted (for the purposes of cleaning up a return).  */

static void
expand_goto_internal (body, label, last_insn)
     tree body;
     rtx label;
     rtx last_insn;
{
  struct nesting *block;
  rtx stack_level = 0;

  if (GET_CODE (label) != CODE_LABEL)
    abort ();

  /* If label has already been defined, we can tell now
     whether and how we must alter the stack level.  */

  if (PREV_INSN (label) != 0)
    {
      /* Find the innermost pending block that contains the label.
	 (Check containment by comparing insn-uids.)
	 Then restore the outermost stack level within that block,
	 and do cleanups of all blocks contained in it.  */
      for (block = block_stack; block; block = block->next)
	{
	  if (INSN_UID (block->data.block.first_insn) < INSN_UID (label))
	    break;
	  if (block->data.block.stack_level != 0)
	    stack_level = block->data.block.stack_level;
	  /* Execute the cleanups for blocks we are exiting.  */
	  if (block->data.block.cleanups != 0)
	    expand_cleanups (block->data.block.cleanups, 0);
	}

      if (stack_level)
	emit_move_insn (stack_pointer_rtx, stack_level);

      if (body != 0 && TREE_PACKED (body))
	error ("jump to `%s' invalidly jumps into binding contour",
	       IDENTIFIER_POINTER (DECL_NAME (body)));
    }
  /* Label not yet defined: may need to put this goto
     on the fixup list.  */
  else if (! expand_fixup (body, label, last_insn))
    {
      /* No fixup needed.  Record that the label is the target
	 of at least one goto that has no fixup.  */
      if (body != 0)
	TREE_ADDRESSABLE (body) = 1;
    }

  emit_jump (label);
}

/* Generate if necessary a fixup for a goto
   whose target label in tree structure (if any) is TREE_LABEL
   and whose target in rtl is RTL_LABEL.

   If LAST_INSN is nonzero, we pretend that the jump appears
   after insn LAST_INSN instead of at the current point in the insn stream.

   The fixup will be used later to insert insns at this point
   to restore the stack level as appropriate for the target label.

   Value is nonzero if a fixup is made.  */

static int
expand_fixup (tree_label, rtl_label, last_insn)
     tree tree_label;
     rtx rtl_label;
     rtx last_insn;
{
  struct nesting *block, *end_block;

  /* See if we can recognize which block the label will be output in.
     This is possible in some very common cases.
     If we succeed, set END_BLOCK to that block.
     Otherwise, set it to 0.  */

  if (cond_stack
      && (rtl_label == cond_stack->data.cond.else_label
	  || rtl_label == cond_stack->data.cond.after_label))
    end_block = cond_stack;
  /* If we are in a loop, recognize certain labels which
     are likely targets.  This reduces the number of fixups
     we need to create.  */
  else if (loop_stack
      && (rtl_label == loop_stack->data.loop.start_label
	  || rtl_label == loop_stack->data.loop.end_label
	  || rtl_label == loop_stack->data.loop.continue_label))
    end_block = loop_stack;
  else
    end_block = 0;

  /* Now set END_BLOCK to the binding level to which we will return.  */

  if (end_block)
    {
      struct nesting *next_block = end_block->all;
      block = block_stack;

      /* First see if the END_BLOCK is inside the innermost binding level.
	 If so, then no cleanups or stack levels are relevant.  */
      while (next_block && next_block != block)
	next_block = next_block->all;

      if (next_block)
	return 0;

      /* Otherwise, set END_BLOCK to the innermost binding level
	 which is outside the relevant control-structure nesting.  */
      next_block = block_stack->next;
      for (block = block_stack; block != end_block; block = block->all)
	if (block == next_block)
	  next_block = next_block->next;
      end_block = next_block;
    }

  /* Does any containing block have a stack level or cleanups?
     If not, no fixup is needed, and that is the normal case
     (the only case, for standard C).  */
  for (block = block_stack; block != end_block; block = block->next)
    if (block->data.block.stack_level != 0
	|| block->data.block.cleanups != 0)
      break;

  if (block != end_block)
    {
      /* Ok, a fixup is needed.  Add a fixup to the list of such.  */
      struct goto_fixup *fixup
	= (struct goto_fixup *) oballoc (sizeof (struct goto_fixup));
      /* In case an old stack level is restored, make sure that comes
	 after any pending stack adjust.  */
      do_pending_stack_adjust ();
      fixup->before_jump = last_insn ? last_insn : get_last_insn ();
      fixup->target = tree_label;
      fixup->target_rtl = rtl_label;
      fixup->block_start_count = block_start_count;
      fixup->stack_level = 0;
      fixup->cleanup_list_list
	= (block->data.block.outer_cleanups || block->data.block.cleanups
	   ? tree_cons (0, block->data.block.cleanups,
			block->data.block.outer_cleanups)
	   : 0);
      fixup->next = goto_fixup_chain;
      goto_fixup_chain = fixup;
    }

  return block != 0;
}

/* When exiting a binding contour, process all pending gotos requiring fixups.
   THISBLOCK is the structure that describes the block being exited.
   STACK_LEVEL is the rtx for the stack level to restore exiting this contour.
   CLEANUP_LIST is a list of expressions to evaluate on exiting this contour.
   FIRST_INSN is the insn that began this contour.

   Gotos that jump out of this contour must restore the
   stack level and do the cleanups before actually jumping.

   DONT_JUMP_IN nonzero means report error there is a jump into this
   contour from before the beginning of the contour.
   This is also done if STACK_LEVEL is nonzero.  */

static void
fixup_gotos (thisblock, stack_level, cleanup_list, first_insn, dont_jump_in)
     struct nesting *thisblock;
     rtx stack_level;
     tree cleanup_list;
     rtx first_insn;
     int dont_jump_in;
{
  register struct goto_fixup *f, *prev;

  /* F is the fixup we are considering; PREV is the previous one.  */

  for (prev = 0, f = goto_fixup_chain; f; prev = f, f = f->next)
    {
      /* Test for a fixup that is inactive because it is already handled.  */
      if (f->before_jump == 0)
	{
	  /* Delete inactive fixup from the chain, if that is easy to do.  */
	  if (prev != 0)
	    prev->next = f->next;
	}
      /* Has this fixup's target label been defined?
	 If so, we can finalize it.  */
      else if (PREV_INSN (f->target_rtl) != 0)
	{
	  /* If this fixup jumped into this contour from before the beginning
	     of this contour, report an error.  */
	  /* ??? Bug: this does not detect jumping in through intermediate
	     blocks that have stack levels or cleanups.
	     It detects only a problem with the innermost block
	     around the label.  */
	  if (f->target != 0
	      && (dont_jump_in || stack_level || cleanup_list)
	      && INSN_UID (first_insn) > INSN_UID (f->before_jump)
	      && ! TREE_ADDRESSABLE (f->target))
	    {
	      error_with_decl (f->target,
			       "label `%s' used before containing binding contour");
	      /* Prevent multiple errors for one label.  */
	      TREE_ADDRESSABLE (f->target) = 1;
	    }

	  /* Execute cleanups for blocks this jump exits.  */
	  if (f->cleanup_list_list)
	    {
	      tree lists;
	      for (lists = f->cleanup_list_list; lists; lists = TREE_CHAIN (lists))
		/* Marked elements correspond to blocks that have been closed.
		   Do their cleanups.  */
		if (TREE_ADDRESSABLE (lists)
		    && TREE_VALUE (lists) != 0)
		  fixup_cleanups (TREE_VALUE (lists), &f->before_jump);
	    }

	  /* Restore stack level for the biggest contour that this
	     jump jumps out of.  */
	  if (f->stack_level)
	    emit_insn_after (gen_move_insn (stack_pointer_rtx, f->stack_level),
			     f->before_jump);
	  f->before_jump = 0;
	}
      /* Label has still not appeared.  If we are exiting a block with
	 a stack level to restore, that started before the fixup,
	 mark this stack level as needing restoration
	 when the fixup is later finalized.
	 Also mark the cleanup_list_list element for F
	 that corresponds to this block, so that ultimately
	 this block's cleanups will be executed by the code above.  */
      /* Note: if THISBLOCK == 0 and we have a label that hasn't appeared,
	 it means the label is undefined.  That's erroneous, but possible.  */
      else if (thisblock != 0
	       && (thisblock->data.block.block_start_count
		   < f->block_start_count))
	{
	  tree lists = f->cleanup_list_list;
	  for (; lists; lists = TREE_CHAIN (lists))
	    /* If the following elt. corresponds to our containing block
	       then the elt. must be for this block.  */
	    if (TREE_CHAIN (lists) == thisblock->data.block.outer_cleanups)
	      TREE_ADDRESSABLE (lists) = 1;

	  if (stack_level)
	    f->stack_level = stack_level;
	}
    }
}

/* Generate RTL for an asm statement (explicit assembler code).
   BODY is a STRING_CST node containing the assembler code text.  */

void
expand_asm (body)
     tree body;
{
  emit_insn (gen_rtx (ASM_INPUT, VOIDmode,
		      TREE_STRING_POINTER (body)));
  last_expr_type = 0;
}

/* Generate RTL for an asm statement with arguments.
   STRING is the instruction template.
   OUTPUTS is a list of output arguments (lvalues); INPUTS a list of inputs.
   Each output or input has an expression in the TREE_VALUE and
   a constraint-string in the TREE_PURPOSE.
   CLOBBERS is a list of STRING_CST nodes each naming a hard register
   that is clobbered by this insn.

   Not all kinds of lvalue that may appear in OUTPUTS can be stored directly.
   Some elements of OUTPUTS may be replaced with trees representing temporary
   values.  The caller should copy those temporary values to the originally
   specified lvalues.

   VOL nonzero means the insn is volatile; don't optimize it.  */

void
expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line)
     tree string, outputs, inputs, clobbers;
     int vol;
     char *filename;
     int line;
{
  rtvec argvec, constraints;
  rtx body;
  int ninputs = list_length (inputs);
  int noutputs = list_length (outputs);
  int nclobbers = list_length (clobbers);
  tree tail;
  register int i;
  /* Vector of RTX's of evaluated output operands.  */
  rtx *output_rtx = (rtx *) alloca (noutputs * sizeof (rtx));
  /* The insn we have emitted.  */
  rtx insn;

  last_expr_type = 0;

  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      tree val = TREE_VALUE (tail);
      tree val1;
      int j;
      int found_equal;

      /* If there's an erroneous arg, emit no insn.  */
      if (TREE_TYPE (val) == error_mark_node)
	return;

      /* Make sure constraint has `=' and does not have `+'.  */

      found_equal = 0;
      for (j = 0; j < TREE_STRING_LENGTH (TREE_PURPOSE (tail)); j++)
	{
	  if (TREE_STRING_POINTER (TREE_PURPOSE (tail))[j] == '+')
	    {
	      error ("output operand constraint contains `+'");
	      return;
	    }
	  if (TREE_STRING_POINTER (TREE_PURPOSE (tail))[j] == '=')
	    found_equal = 1;
	}
      if (! found_equal)
	{
	  error ("output operand constraint lacks `='");
	  return;
	}

      /* If an output operand is not a variable or indirect ref,
	 or a part of one,
	 create a SAVE_EXPR which is a pseudo-reg
	 to act as an intermediate temporary.
	 Make the asm insn write into that, then copy it to
	 the real output operand.  */

      val1 = val;
      while (TREE_CODE (val1) == COMPONENT_REF
	     || TREE_CODE (val1) == ARRAY_REF)
	val1 = TREE_OPERAND (val1, 0);

      if (TREE_CODE (val1) != VAR_DECL
	  && TREE_CODE (val1) != PARM_DECL
	  && TREE_CODE (val1) != INDIRECT_REF)
	{
	  rtx reg = gen_reg_rtx (TYPE_MODE (TREE_TYPE (val)));
	  /* `build' isn't safe; it really expects args to be trees.  */
	  tree t = build_nt (SAVE_EXPR, val, reg);

	  if (GET_MODE (reg) == BLKmode)
	    abort ();

	  save_expr_regs = gen_rtx (EXPR_LIST, VOIDmode, reg, save_expr_regs);
	  TREE_VALUE (tail) = t;
	  TREE_TYPE (t) = TREE_TYPE (val);
	}
      output_rtx[i] = expand_expr (TREE_VALUE (tail), 0, VOIDmode, 0);
    }

  if (ninputs + noutputs > MAX_RECOG_OPERANDS)
    {
      error ("more than %d operands in `asm'", MAX_RECOG_OPERANDS);
      return;
    }

  /* Make vectors for the expression-rtx and constraint strings.  */

  argvec = rtvec_alloc (ninputs);
  constraints = rtvec_alloc (ninputs);

  body = gen_rtx (ASM_OPERANDS, VOIDmode,
		  TREE_STRING_POINTER (string), "", 0, argvec, constraints,
		  filename, line);
  MEM_VOLATILE_P (body) = vol;

  /* Eval the inputs and put them into ARGVEC.
     Put their constraints into ASM_INPUTs and store in CONSTRAINTS.  */

  i = 0;
  for (tail = inputs; tail; tail = TREE_CHAIN (tail))
    {
      int j;

      /* If there's an erroneous arg, emit no insn,
	 because the ASM_INPUT would get VOIDmode
	 and that could cause a crash in reload.  */
      if (TREE_TYPE (TREE_VALUE (tail)) == error_mark_node)
	return;
      if (TREE_PURPOSE (tail) == NULL_TREE)
	{
	  error ("hard register `%s' listed as input operand to `asm'",
		 TREE_STRING_POINTER (TREE_VALUE (tail)) );
	  return;
	}

      /* Make sure constraint has neither `=' nor `+'.  */

      for (j = 0; j < TREE_STRING_LENGTH (TREE_PURPOSE (tail)); j++)
	if (TREE_STRING_POINTER (TREE_PURPOSE (tail))[j] == '='
	    || TREE_STRING_POINTER (TREE_PURPOSE (tail))[j] == '+')
	  {
	    error ("input operand constraint contains `%c'",
		   TREE_STRING_POINTER (TREE_PURPOSE (tail))[j]);
	    return;
	  }

      XVECEXP (body, 3, i)      /* argvec */
	= expand_expr (TREE_VALUE (tail), 0, VOIDmode, 0);
      XVECEXP (body, 4, i)      /* constraints */
	= gen_rtx (ASM_INPUT, TYPE_MODE (TREE_TYPE (TREE_VALUE (tail))),
		   TREE_STRING_POINTER (TREE_PURPOSE (tail)));
      i++;
    }

  /* Protect all the operands from the queue,
     now that they have all been evaluated.  */

  for (i = 0; i < ninputs; i++)
    XVECEXP (body, 3, i) = protect_from_queue (XVECEXP (body, 3, i), 0);

  for (i = 0; i < noutputs; i++)
    output_rtx[i] = protect_from_queue (output_rtx[i], 1);

  /* Now, for each output, construct an rtx
     (set OUTPUT (asm_operands INSN OUTPUTNUMBER OUTPUTCONSTRAINT
			       ARGVEC CONSTRAINTS))
     If there is more than one, put them inside a PARALLEL.  */

  if (noutputs == 1 && nclobbers == 0)
    {
      XSTR (body, 1) = TREE_STRING_POINTER (TREE_PURPOSE (outputs));
      insn = emit_insn (gen_rtx (SET, VOIDmode, output_rtx[0], body));
    }
  else if (noutputs == 0 && nclobbers == 0)
    {
      /* No output operands: put in a raw ASM_OPERANDS rtx.  */
      insn = emit_insn (body);
    }
  else
    {
      rtx obody = body;
      int num = noutputs;
      if (num == 0) num = 1;
      body = gen_rtx (PARALLEL, VOIDmode, rtvec_alloc (num + nclobbers));

      /* For each output operand, store a SET.  */

      for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
	{
	  XVECEXP (body, 0, i)
	    = gen_rtx (SET, VOIDmode,
		       output_rtx[i],
		       gen_rtx (ASM_OPERANDS, VOIDmode,
				TREE_STRING_POINTER (string),
				TREE_STRING_POINTER (TREE_PURPOSE (tail)),
				i, argvec, constraints,
				filename, line));
	  MEM_VOLATILE_P (SET_SRC (XVECEXP (body, 0, i))) = vol;
	}

      /* If there are no outputs (but there are some clobbers)
	 store the bare ASM_OPERANDS into the PARALLEL.  */

      if (i == 0)
	XVECEXP (body, 0, i++) = obody;

      /* Store (clobber REG) for each clobbered register specified.  */

      for (tail = clobbers; tail; tail = TREE_CHAIN (tail), i++)
	{
	  int j;
	  char *regname = TREE_STRING_POINTER (TREE_VALUE (tail));
	  extern char *reg_names[];
	      
	  for (j = 0; j < FIRST_PSEUDO_REGISTER; j++)
	    if (!strcmp (regname, reg_names[j]))
	      break;
	      
	  if (j == FIRST_PSEUDO_REGISTER)
	    {
	      error ("unknown register name `%s' in `asm'", regname);
	      return;
	    }

	  /* Use QImode since that's guaranteed to clobber just one reg.  */
	  XVECEXP (body, 0, i)
	    = gen_rtx (CLOBBER, VOIDmode, gen_rtx (REG, QImode, j));
	}

      insn = emit_insn (body);
    }

  last_expr_type = 0;
}

/* Nonzero if within a ({...}) grouping, in which case we must
   always compute a value for each expr-stmt in case it is the last one.  */

int expr_stmts_for_value;

/* Generate RTL to evaluate the expression EXP
   and remember it in case this is the VALUE in a ({... VALUE; }) constr.  */

void
expand_expr_stmt (exp)
     tree exp;
{
  /* If -W, warn about statements with no side effects,
     except inside a ({...}) where they may be useful.  */
  if (expr_stmts_for_value == 0 && exp != error_mark_node)
    {
      if (! TREE_VOLATILE (exp) && (extra_warnings || warn_unused))
	warning_with_file_and_line (emit_filename, emit_lineno,
				    "statement with no effect");
      else if (warn_unused)
	warn_if_unused_value (exp);
    }
  last_expr_type = TREE_TYPE (exp);
  if (! flag_syntax_only)
    last_expr_value = expand_expr (exp, expr_stmts_for_value ? 0 : const0_rtx,
				   VOIDmode, 0);
  emit_queue ();
}

/* Warn if EXP contains any computations whose results are not used.
   Return 1 if a warning is printed; 0 otherwise.  */

static int
warn_if_unused_value (exp)
     tree exp;
{
  switch (TREE_CODE (exp))
    {
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case MODIFY_EXPR:
    case INIT_EXPR:
    case NEW_EXPR:
    case DELETE_EXPR:
    case PUSH_EXPR:
    case POP_EXPR:
    case CALL_EXPR:
    case METHOD_CALL_EXPR:
    case RTL_EXPR:
    case WRAPPER_EXPR:
    case ANTI_WRAPPER_EXPR:
    case WITH_CLEANUP_EXPR:
      /* We don't warn about COND_EXPR because it may be a useful
	 construct if either arm contains a side effect.  */
    case COND_EXPR:
      return 0;

    case TRUTH_ORIF_EXPR:
    case TRUTH_ANDIF_EXPR:
      /* In && or ||, warn if 2nd operand has no side effect.  */
      return warn_if_unused_value (TREE_OPERAND (exp, 1));

    case COMPOUND_EXPR:
      if (warn_if_unused_value (TREE_OPERAND (exp, 0)))
	return 1;
      return warn_if_unused_value (TREE_OPERAND (exp, 1));

    case NOP_EXPR:
    case CONVERT_EXPR:
      /* Don't warn about values cast to void.  */
      if (TREE_TYPE (exp) == void_type_node)
	return 0;
      /* Assignment to a cast results in a cast of a modify.
	 Don't complain about that.  */
      if (TREE_CODE (TREE_OPERAND (exp, 0)) == MODIFY_EXPR)
	return 0;

    default:
      warning_with_file_and_line (emit_filename, emit_lineno,
				  "value computed is not used");
      return 1;
    }
}

/* Clear out the memory of the last expression evaluated.  */

void
clear_last_expr ()
{
  last_expr_type = 0;
}

/* Begin a statement which will return a value.
   Return the RTL_EXPR for this statement expr.
   The caller must save that value and pass it to expand_end_stmt_expr.  */

tree
expand_start_stmt_expr ()
{
  rtx save = start_sequence ();
  /* Make the RTL_EXPR node temporary, not momentary,
     so that rtl_expr_chain doesn't become garbage.  */
  int momentary = suspend_momentary ();
  tree t = make_node (RTL_EXPR);
  resume_momentary (momentary);
  RTL_EXPR_RTL (t) = save;
  NO_DEFER_POP;
  expr_stmts_for_value++;
  return t;
}

/* Restore the previous state at the end of a statement that returns a value.
   Returns a tree node representing the statement's value and the
   insns to compute the value.

   The nodes of that expression have been freed by now, so we cannot use them.
   But we don't want to do that anyway; the expression has already been
   evaluated and now we just want to use the value.  So generate a RTL_EXPR
   with the proper type and RTL value.

   If the last substatement was not an expression,
   return something with type `void'.  */

tree
expand_end_stmt_expr (t)
     tree t;
{
  rtx saved = RTL_EXPR_RTL (t);

  OK_DEFER_POP;

  if (last_expr_type == 0)
    {
      last_expr_type = void_type_node;
      last_expr_value = const0_rtx;
    }
  TREE_TYPE (t) = last_expr_type;
  RTL_EXPR_RTL (t) = last_expr_value;
  RTL_EXPR_SEQUENCE (t) = get_insns ();

  rtl_expr_chain = tree_cons (NULL_TREE, t, rtl_expr_chain);

  end_sequence (saved);

  /* Don't consider deleting this expr or containing exprs at tree level.  */
  TREE_VOLATILE (t) = 1;
  /* Propagate volatility of the actual RTL expr.  */
  TREE_THIS_VOLATILE (t) = volatile_refs_p (last_expr_value);

  last_expr_type = 0;
  expr_stmts_for_value--;

  return t;
}

/* Generate RTL for the start of an if-then.  COND is the expression
   whose truth should be tested.

   If EXITFLAG is nonzero, this conditional is visible to
   `exit_something'.  */

void
expand_start_cond (cond, exitflag)
     tree cond;
     int exitflag;
{
  struct nesting *thiscond
    = (struct nesting *) xmalloc (sizeof (struct nesting));

  /* Make an entry on cond_stack for the cond we are entering.  */

  thiscond->next = cond_stack;
  thiscond->all = nesting_stack;
  thiscond->depth = ++nesting_depth;
  thiscond->data.cond.after_label = 0;
  thiscond->data.cond.else_label = gen_label_rtx ();
  thiscond->exit_label = exitflag ? thiscond->data.cond.else_label : 0;
  cond_stack = thiscond;
  nesting_stack = thiscond;

  do_jump (cond, thiscond->data.cond.else_label, NULL);
}

/* Generate RTL for the end of an if-then with no else-clause.
   Pop the record for it off of cond_stack.  */

void
expand_end_cond ()
{
  struct nesting *thiscond = cond_stack;

  do_pending_stack_adjust ();
  emit_label (thiscond->data.cond.else_label);

  POPSTACK (cond_stack);
  last_expr_type = 0;
}

/* Generate RTL between the then-clause and the else-clause
   of an if-then-else.  */

void
expand_start_else ()
{
  cond_stack->data.cond.after_label = gen_label_rtx ();
  if (cond_stack->exit_label != 0)
    cond_stack->exit_label = cond_stack->data.cond.after_label;
  emit_jump (cond_stack->data.cond.after_label);
  if (cond_stack->data.cond.else_label)
    emit_label (cond_stack->data.cond.else_label);
}

/* Generate RTL for the end of an if-then-else.
   Pop the record for it off of cond_stack.  */

void
expand_end_else ()
{
  struct nesting *thiscond = cond_stack;

  do_pending_stack_adjust ();
  /* Note: a syntax error can cause this to be called
     without first calling `expand_start_else'.  */
  if (thiscond->data.cond.after_label)
    emit_label (thiscond->data.cond.after_label);

  POPSTACK (cond_stack);
  last_expr_type = 0;
}

/* Generate RTL for the start of a loop.  EXIT_FLAG is nonzero if this
   loop should be exited by `exit_something'.  This is a loop for which
   `expand_continue' will jump to the top of the loop.

   Make an entry on loop_stack to record the labels associated with
   this loop.  */

void
expand_start_loop (exit_flag)
     int exit_flag;
{
  register struct nesting *thisloop
    = (struct nesting *) xmalloc (sizeof (struct nesting));

  /* Make an entry on loop_stack for the loop we are entering.  */

  thisloop->next = loop_stack;
  thisloop->all = nesting_stack;
  thisloop->depth = ++nesting_depth;
  thisloop->data.loop.start_label = gen_label_rtx ();
  thisloop->data.loop.end_label = gen_label_rtx ();
  thisloop->data.loop.continue_label = thisloop->data.loop.start_label;
  thisloop->exit_label = exit_flag ? thisloop->data.loop.end_label : 0;
  loop_stack = thisloop;
  nesting_stack = thisloop;

  do_pending_stack_adjust ();
  emit_queue ();
  emit_note (0, NOTE_INSN_LOOP_BEG);
  emit_label (thisloop->data.loop.start_label);
}

/* Like expand_start_loop but for a loop where the continuation point
   (for expand_continue_loop) will be specified explicitly.  */

void
expand_start_loop_continue_elsewhere (exit_flag)
     int exit_flag;
{
  expand_start_loop (exit_flag);
  loop_stack->data.loop.continue_label = gen_label_rtx ();
}

/* Specify the continuation point for a loop started with
   expand_start_loop_continue_elsewhere.
   Use this at the point in the code to which a continue statement
   should jump.  */

void
expand_loop_continue_here ()
{
  do_pending_stack_adjust ();
  emit_note (0, NOTE_INSN_LOOP_CONT);
  emit_label (loop_stack->data.loop.continue_label);
}

/* Finish a loop.  Generate a jump back to the top and the loop-exit label.
   Pop the block off of loop_stack.  */

void
expand_end_loop ()
{
  register rtx insn = get_last_insn ();
  register rtx start_label = loop_stack->data.loop.start_label;

  do_pending_stack_adjust ();

  /* If optimizing, perhaps reorder the loop.  If the loop
     starts with a conditional exit, roll that to the end
     where it will optimize together with the jump back.  */
  if (optimize
      &&
      ! (GET_CODE (insn) == JUMP_INSN
	 && GET_CODE (PATTERN (insn)) == SET
	 && SET_DEST (PATTERN (insn)) == pc_rtx
	 && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE))
    {
      /* Scan insns from the top of the loop looking for a qualified
	 conditional exit.  */
      for (insn = loop_stack->data.loop.start_label; insn; insn= NEXT_INSN (insn))
	if (GET_CODE (insn) == JUMP_INSN && GET_CODE (PATTERN (insn)) == SET
	    && SET_DEST (PATTERN (insn)) == pc_rtx
	    && GET_CODE (SET_SRC (PATTERN (insn))) == IF_THEN_ELSE
	    &&
	    ((GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 1)) == LABEL_REF
	      && (XEXP (XEXP (SET_SRC (PATTERN (insn)), 1), 0)
		  == loop_stack->data.loop.end_label))
	     ||
	     (GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 2)) == LABEL_REF
	      && (XEXP (XEXP (SET_SRC (PATTERN (insn)), 2), 0)
		  == loop_stack->data.loop.end_label))))
	  break;
      if (insn != 0)
	{
	  /* We found one.  Move everything from there up
	     to the end of the loop, and add a jump into the loop
	     to jump to there.  */
	  register rtx newstart_label = gen_label_rtx ();

	  emit_label_after (newstart_label, PREV_INSN (start_label));
	  reorder_insns (start_label, insn, get_last_insn ());
	  emit_jump_insn_after (gen_jump (start_label), PREV_INSN (newstart_label));
	  emit_barrier_after (PREV_INSN (newstart_label));
	  start_label = newstart_label;
	}
    }

  emit_jump (start_label);
  emit_note (0, NOTE_INSN_LOOP_END);
  emit_label (loop_stack->data.loop.end_label);

  POPSTACK (loop_stack);

  last_expr_type = 0;
}

/* Generate a jump to the current loop's continue-point.
   This is usually the top of the loop, but may be specified
   explicitly elsewhere.  If not currently inside a loop,
   return 0 and do nothing; caller will print an error message.  */

int
expand_continue_loop ()
{
  last_expr_type = 0;
  if (loop_stack == 0)
    return 0;
  expand_goto_internal (0, loop_stack->data.loop.continue_label, 0);
  return 1;
}

/* Generate a jump to exit the current loop.  If not currently inside a loop,
   return 0 and do nothing; caller will print an error message.  */

int
expand_exit_loop ()
{
  last_expr_type = 0;
  if (loop_stack == 0)
    return 0;
  expand_goto_internal (0, loop_stack->data.loop.end_label, 0);
  return 1;
}

/* Generate a conditional jump to exit the current loop if COND
   evaluates to zero.  If not currently inside a loop,
   return 0 and do nothing; caller will print an error message.  */

int
expand_exit_loop_if_false (cond)
     tree cond;
{
  last_expr_type = 0;
  if (loop_stack == 0)
    return 0;
  do_jump (cond, loop_stack->data.loop.end_label, NULL);
  return 1;
}

/* Return non-zero if currently inside a loop.  */

int
inside_loop ()
{
  return loop_stack != 0;
}

/* Generate a jump to exit the current loop, conditional, binding contour
   or case statement.  Not all such constructs are visible to this function,
   only those started with EXIT_FLAG nonzero.  Individual languages use
   the EXIT_FLAG parameter to control which kinds of constructs you can
   exit this way.

   If not currently inside anything that can be exited,
   return 0 and do nothing; caller will print an error message.  */

int
expand_exit_something ()
{
  struct nesting *n;
  last_expr_type = 0;
  for (n = nesting_stack; n; n = n->all)
    if (n->exit_label != 0)
      {
	expand_goto_internal (0, n->exit_label, 0);
	return 1;
      }

  return 0;
}

/* Generate RTL to return from the current function, with no value.
   (That is, we do not do anything about returning any value.)  */

void
expand_null_return ()
{
  struct nesting *block = block_stack;
  rtx last_insn = 0;

  /* Does any pending block have cleanups?  */

  while (block && block->data.block.cleanups == 0)
    block = block->next;

  /* If yes, use a goto to return, since that runs cleanups.  */

  expand_null_return_1 (last_insn, block != 0);
}

/* Output a return with no value.  If LAST_INSN is nonzero,
   pretend that the return takes place after LAST_INSN.
   If USE_GOTO is nonzero then don't use a return instruction;
   go to the return label instead.  This causes any cleanups
   of pending blocks to be executed normally.  */

static void
expand_null_return_1 (last_insn, use_goto)
     rtx last_insn;
     int use_goto;
{
  rtx end_label = cleanup_label ? cleanup_label : return_label;

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();
  last_expr_type = 0;

  /* PCC-struct return always uses an epilogue.  */
  if (current_function_returns_pcc_struct || use_goto)
    {
      if (end_label == 0)
	end_label = return_label = gen_label_rtx ();
      expand_goto_internal (0, end_label, last_insn);
      return;
    }

  /* Otherwise output a simple return-insn if one is available,
     unless it won't do the job.  */
#ifdef HAVE_return
  if (HAVE_return && cleanup_label == 0)
    {
      emit_jump_insn (gen_return ());
      emit_barrier ();
      return;
    }
#endif

  /* Otherwise jump to the epilogue.  */
  expand_goto_internal (0, end_label, last_insn);
}

/* Generate RTL to evaluate the expression RETVAL and return it
   from the current function.  */

void
expand_return (retval)
     tree retval;
{
  /* If there are any cleanups to be performed, then they will
     be inserted following LAST_INSN.  It is desirable
     that the last_insn, for such purposes, should be the
     last insn before computing the return value.  Otherwise, cleanups
     which call functions can clobber the return value.  */
  /* ??? rms: I think that is erroneous, because in C++ it would
     run destructors on variables that might be used in the subsequent
     computation of the return value.  */
  rtx last_insn = 0;
  register rtx val = 0;
  register rtx op0;
  tree retval_rhs;
  int cleanups;
  struct nesting *block;

  /* Are any cleanups needed?  E.g. C++ destructors to be run?  */
  cleanups = 0;
  for (block = block_stack; block; block = block->next)
    if (block->data.block.cleanups != 0)
      {
	cleanups = 1;
	break;
      }

  if (TREE_CODE (retval) == RESULT_DECL)
    retval_rhs = retval;
  else if ((TREE_CODE (retval) == MODIFY_EXPR || TREE_CODE (retval) == INIT_EXPR)
	   && TREE_CODE (TREE_OPERAND (retval, 0)) == RESULT_DECL)
    retval_rhs = TREE_OPERAND (retval, 1);
  else if (TREE_TYPE (retval) == void_type_node)
    /* Recognize tail-recursive call to void function.  */
    retval_rhs = retval;
  else
    retval_rhs = NULL_TREE;

  /* Only use `last_insn' if there are cleanups which must be run.  */
  if (cleanups || cleanup_label != 0)
    last_insn = get_last_insn ();

  /* For tail-recursive call to current function,
     just jump back to the beginning.
     It's unsafe if any auto variable in this function
     has its address taken; for simplicity,
     require stack frame to be empty.  */
  if (optimize && retval_rhs != 0
      && frame_offset == STARTING_FRAME_OFFSET
      && TREE_CODE (retval_rhs) == CALL_EXPR
      && TREE_CODE (TREE_OPERAND (retval_rhs, 0)) == ADDR_EXPR
      && TREE_OPERAND (TREE_OPERAND (retval_rhs, 0), 0) == this_function
      /* Finish checking validity, and if valid emit code
	 to set the argument variables for the new call.  */
      && tail_recursion_args (TREE_OPERAND (retval_rhs, 1),
			      DECL_ARGUMENTS (this_function)))
    {
      if (tail_recursion_label == 0)
	{
	  tail_recursion_label = gen_label_rtx ();
	  emit_label_after (tail_recursion_label,
			    tail_recursion_reentry);
	}
      expand_goto_internal (0, tail_recursion_label, last_insn);
      emit_barrier ();
      return;
    }
#ifdef HAVE_return
  /* This optimization is safe if there are local cleanups
     because expand_null_return takes care of them.
     ??? I think it should also be safe when there is a cleanup label,
     because expand_null_return takes care of them, too.
     Any reason why not?  */
  if (HAVE_return && cleanup_label == 0
      && ! current_function_returns_pcc_struct)
    {
      /* If this is  return x == y;  then generate
	 if (x == y) return 1; else return 0;
	 if we can do it with explicit return insns.  */
      if (retval_rhs)
	switch (TREE_CODE (retval_rhs))
	  {
	  case EQ_EXPR:
	  case NE_EXPR:
	  case GT_EXPR:
	  case GE_EXPR:
	  case LT_EXPR:
	  case LE_EXPR:
	  case TRUTH_ANDIF_EXPR:
	  case TRUTH_ORIF_EXPR:
	  case TRUTH_AND_EXPR:
	  case TRUTH_OR_EXPR:
	  case TRUTH_NOT_EXPR:
	    op0 = gen_label_rtx ();
	    val = DECL_RTL (DECL_RESULT (this_function));
	    jumpifnot (retval_rhs, op0);
	    emit_move_insn (val, const1_rtx);
	    emit_insn (gen_rtx (USE, VOIDmode, val));
	    expand_null_return ();
	    emit_label (op0);
	    emit_move_insn (val, const0_rtx);
	    emit_insn (gen_rtx (USE, VOIDmode, val));
	    expand_null_return ();
	    return;
	  }
    }
#endif /* HAVE_return */

  if (cleanups
      && retval_rhs != 0
      && TREE_TYPE (retval_rhs) != void_type_node
      && GET_CODE (DECL_RTL (DECL_RESULT (this_function))) == REG)
    {
      rtx last_insn;
      /* Calculate the return value into a pseudo reg.  */
      val = expand_expr (retval_rhs, 0, VOIDmode, 0);
      emit_queue ();
      /* Put the cleanups here.  */
      last_insn = get_last_insn ();
      /* Copy the value into hard return reg.  */
      emit_move_insn (DECL_RTL (DECL_RESULT (this_function)), val);
      val = DECL_RTL (DECL_RESULT (this_function));

      if (GET_CODE (val) == REG)
	emit_insn (gen_rtx (USE, VOIDmode, val));
      expand_null_return_1 (last_insn, cleanups);
    }
  else
    {
      /* No cleanups or no hard reg used;
	 calculate value into hard return reg
	 and let cleanups come after.  */
      val = expand_expr (retval, 0, VOIDmode, 0);
      emit_queue ();

      val = DECL_RTL (DECL_RESULT (this_function));
      if (val && GET_CODE (val) == REG)
	emit_insn (gen_rtx (USE, VOIDmode, val));
      expand_null_return ();
    }
}

/* Return 1 if the end of the generated RTX is not a barrier.
   This means code already compiled can drop through.  */

int
drop_through_at_end_p ()
{
  rtx insn = get_last_insn ();
  while (insn && GET_CODE (insn) == NOTE)
    insn = PREV_INSN (insn);
  return insn && GET_CODE (insn) != BARRIER;
}

/* Emit code to alter this function's formal parms for a tail-recursive call.
   ACTUALS is a list of actual parameter expressions (chain of TREE_LISTs).
   FORMALS is the chain of decls of formals.
   Return 1 if this can be done;
   otherwise return 0 and do not emit any code.  */

static int
tail_recursion_args (actuals, formals)
     tree actuals, formals;
{
  register tree a = actuals, f = formals;
  register int i;
  register rtx *argvec;

  /* Check that number and types of actuals are compatible
     with the formals.  This is not always true in valid C code.
     Also check that no formal needs to be addressable
     and that all formals are scalars.  */

  /* Also count the args.  */

  for (a = actuals, f = formals, i = 0; a && f; a = TREE_CHAIN (a), f = TREE_CHAIN (f), i++)
    {
      if (TREE_TYPE (TREE_VALUE (a)) != TREE_TYPE (f))
	return 0;
      if (GET_CODE (DECL_RTL (f)) != REG || DECL_MODE (f) == BLKmode)
	return 0;
    }
  if (a != 0 || f != 0)
    return 0;

  /* Compute all the actuals.  */

  argvec = (rtx *) alloca (i * sizeof (rtx));

  for (a = actuals, i = 0; a; a = TREE_CHAIN (a), i++)
    argvec[i] = expand_expr (TREE_VALUE (a), 0, VOIDmode, 0);

  /* Find which actual values refer to current values of previous formals.
     Copy each of them now, before any formal is changed.  */

  for (a = actuals, i = 0; a; a = TREE_CHAIN (a), i++)
    {
      int copy = 0;
      register int j;
      for (f = formals, j = 0; j < i; f = TREE_CHAIN (f), j++)
	if (reg_mentioned_p (DECL_RTL (f), argvec[i]))
	  { copy = 1; break; }
      if (copy)
	argvec[i] = copy_to_reg (argvec[i]);
    }

  /* Store the values of the actuals into the formals.  */

  for (f = formals, a = actuals, i = 0; f;
       f = TREE_CHAIN (f), a = TREE_CHAIN (a), i++)
    {
      if (DECL_MODE (f) == GET_MODE (argvec[i]))
	emit_move_insn (DECL_RTL (f), argvec[i]);
      else
	convert_move (DECL_RTL (f), argvec[i],
		      TREE_UNSIGNED (TREE_TYPE (TREE_VALUE (a))));
    }

  return 1;
}

/* Generate the RTL code for entering a binding contour.
   The variables are declared one by one, by calls to `expand_decl'.

   EXIT_FLAG is nonzero if this construct should be visible to
   `exit_something'.  */

void
expand_start_bindings (exit_flag)
     int exit_flag;
{
  struct nesting *thisblock
    = (struct nesting *) xmalloc (sizeof (struct nesting));

  rtx note = emit_note (0, NOTE_INSN_BLOCK_BEG);

  /* Make an entry on block_stack for the block we are entering.  */

  thisblock->next = block_stack;
  thisblock->all = nesting_stack;
  thisblock->depth = ++nesting_depth;
  thisblock->data.block.stack_level = 0;
  thisblock->data.block.cleanups = 0;
  /* We build this even if the cleanups lists are empty
     because we rely on having an element in the chain
     for each block that is pending.  */
  thisblock->data.block.outer_cleanups
    = (block_stack
       ? tree_cons (NULL_TREE, block_stack->data.block.cleanups,
		    block_stack->data.block.outer_cleanups)
       : 0);
  thisblock->data.block.label_chain = 0;
  thisblock->data.block.innermost_stack_block = stack_block_stack;
  thisblock->data.block.first_insn = note;
  thisblock->data.block.block_start_count = ++block_start_count;
  thisblock->exit_label = exit_flag ? gen_label_rtx () : 0;

  block_stack = thisblock;
  nesting_stack = thisblock;
}

/* Output a USE for any register use in RTL.
   This is used with -noreg to mark the extent of lifespan
   of any registers used in a user-visible variable's DECL_RTL.  */

void
use_variable (rtl)
     rtx rtl;
{
  if (GET_CODE (rtl) == REG)
    /* This is a register variable.  */
    emit_insn (gen_rtx (USE, VOIDmode, rtl));
  else if (GET_CODE (rtl) == MEM
	   && GET_CODE (XEXP (rtl, 0)) == REG
	   && XEXP (rtl, 0) != frame_pointer_rtx
	   && XEXP (rtl, 0) != arg_pointer_rtx)
    /* This is a variable-sized structure.  */
    emit_insn (gen_rtx (USE, VOIDmode, XEXP (rtl, 0)));
}

/* Like use_variable except that it outputs the USEs after INSN
   instead of at the end of the insn-chain.  */

static void
use_variable_after (rtl, insn)
     rtx rtl, insn;
{
  if (GET_CODE (rtl) == REG)
    /* This is a register variable.  */
    emit_insn_after (gen_rtx (USE, VOIDmode, rtl), insn);
  else if (GET_CODE (rtl) == MEM
	   && GET_CODE (XEXP (rtl, 0)) == REG
	   && XEXP (rtl, 0) != frame_pointer_rtx
	   && XEXP (rtl, 0) != arg_pointer_rtx)
    /* This is a variable-sized structure.  */
    emit_insn_after (gen_rtx (USE, VOIDmode, XEXP (rtl, 0)), insn);
}

/* Generate RTL code to terminate a binding contour.
   VARS is the chain of VAR_DECL nodes
   for the variables bound in this contour.
   MARK_ENDS is nonzero if we should put a note at the beginning
   and end of this binding contour.

   DONT_JUMP_IN is nonzero if it is not valid to jump into this contour.
   (That is true automatically if the contour has a saved stack level.)  */

void
expand_end_bindings (vars, mark_ends, dont_jump_in)
     tree vars;
     int mark_ends;
     int dont_jump_in;
{
  register struct nesting *thisblock = block_stack;
  register tree decl;

  if (warn_unused)
    for (decl = vars; decl; decl = TREE_CHAIN (decl))
      if (! TREE_USED (decl) && TREE_CODE (decl) == VAR_DECL)
	warning_with_decl (decl, "unused variable `%s'");

  /* Mark the beginning and end of the scope if requested.  */

  if (mark_ends)
    emit_note (0, NOTE_INSN_BLOCK_END);
  else
    /* Get rid of the beginning-mark if we don't make an end-mark.  */
    NOTE_LINE_NUMBER (thisblock->data.block.first_insn) = NOTE_INSN_DELETED;

  if (thisblock->exit_label)
    {
      do_pending_stack_adjust ();
      emit_label (thisblock->exit_label);
    }

  if (dont_jump_in
      || thisblock->data.block.stack_level != 0
      || thisblock->data.block.cleanups != 0)
    {
      struct label_chain *chain;

      /* Any labels in this block are no longer valid to go to.
	 Mark them to cause an error message.  */
      for (chain = thisblock->data.block.label_chain; chain; chain = chain->next)
	{
	  TREE_PACKED (chain->label) = 1;
	  /* If any goto without a fixup came to this label,
	     that must be an error, because gotos without fixups
	     come from outside all saved stack-levels and all cleanups.  */
	  if (TREE_ADDRESSABLE (chain->label))
	    error_with_decl (chain->label,
			     "label `%s' used before containing binding contour");
	}
    }

  /* Restore stack level in effect before the block
     (only if variable-size objects allocated).  */

  if (thisblock->data.block.stack_level != 0
      || thisblock->data.block.cleanups != 0)
    {
      /* Perform any cleanups associated with the block.  */

      expand_cleanups (thisblock->data.block.cleanups, 0);

      /* Restore the stack level.  */

      if (thisblock->data.block.stack_level != 0)
	{
	  do_pending_stack_adjust ();
	  emit_move_insn (stack_pointer_rtx,
			  thisblock->data.block.stack_level);
	}

      /* Any gotos out of this block must also do these things.
	 Also report any gotos with fixups that came to labels in this level.  */
      fixup_gotos (thisblock,
		   thisblock->data.block.stack_level,
		   thisblock->data.block.cleanups,
		   thisblock->data.block.first_insn,
		   dont_jump_in);
    }

  /* If doing stupid register allocation, make sure lives of all
     register variables declared here extend thru end of scope.  */

  if (obey_regdecls)
    for (decl = vars; decl; decl = TREE_CHAIN (decl))
      {
	rtx rtl = DECL_RTL (decl);
	if (TREE_CODE (decl) == VAR_DECL && rtl != 0)
	  use_variable (rtl);
      }

  /* Restore block_stack level for containing block.  */

  stack_block_stack = thisblock->data.block.innermost_stack_block;
  POPSTACK (block_stack);
}

/* Generate RTL for the automatic variable declaration DECL.
   (Other kinds of declarations are simply ignored if seen here.)
   CLEANUP is an expression to be executed at exit from this binding contour;
   for example, in C++, it might call the destructor for this variable.

   If CLEANUP contains any SAVE_EXPRs, then you must preevaluate them
   either before or after calling `expand_decl' but before compiling
   any subsequent expressions.  This is because CLEANUP may be expanded
   more than once, on different branches of execution.
   For the same reason, CLEANUP may not contain a CALL_EXPR
   except as its topmost node--else `preexpand_calls' would get confused.

   If CLEANUP is nonzero and DECL is zero, we record a cleanup
   that is not associated with any particular variable.

   There is no special support here for C++ constructors.
   They should be handled by the proper code in DECL_INITIAL.  */

void
expand_decl (decl, cleanup)
     register tree decl;
     tree cleanup;
{
  struct nesting *thisblock = block_stack;
  tree type;
  
  /* Record the cleanup if there is one.  */

  if (cleanup != 0)
    {
      thisblock->data.block.cleanups
	= temp_tree_cons (decl, cleanup, thisblock->data.block.cleanups);
      /* If this block has a cleanup, it belongs in stack_block_stack.  */
      stack_block_stack = thisblock;
    }

  if (decl == NULL_TREE)
    {
      /* This was a cleanup with no variable.  */
      if (cleanup == 0)
	abort ();
      return;
    }

  type = TREE_TYPE (decl);

  /* Aside from that, only automatic variables need any expansion done.
     Static and external variables, and external functions,
     will be handled by `assemble_variable' (called from finish_decl).
     TYPE_DECL and CONST_DECL require nothing.
     PARM_DECLs are handled in `assign_parms'.  */

  if (TREE_CODE (decl) != VAR_DECL)
    return;
  if (TREE_STATIC (decl) || TREE_EXTERNAL (decl))
    return;

  /* Create the RTL representation for the variable.  */

  if (type == error_mark_node)
    DECL_RTL (decl) = gen_rtx (MEM, BLKmode, const0_rtx);
  else if (DECL_SIZE (decl) == 0)
    /* Variable with incomplete type.  */
    {
      if (DECL_INITIAL (decl) == 0)
	/* Error message was already done; now avoid a crash.  */
	DECL_RTL (decl) = assign_stack_local (DECL_MODE (decl), 0);
      else
	/* An initializer is going to decide the size of this array.
	   Until we know the size, represent its address with a reg.  */
	DECL_RTL (decl) = gen_rtx (MEM, BLKmode, gen_reg_rtx (Pmode));
    }
  else if (DECL_MODE (decl) != BLKmode
	   /* If -ffloat-store, don't put explicit float vars
	      into regs.  */
	   && !(flag_float_store
		&& TREE_CODE (type) == REAL_TYPE)
	   && ! TREE_VOLATILE (decl)
	   && ! TREE_ADDRESSABLE (decl)
	   && (TREE_REGDECL (decl) || ! obey_regdecls))
    {
      /* Automatic variable that can go in a register.  */
      DECL_RTL (decl) = gen_reg_rtx (DECL_MODE (decl));
      if (TREE_CODE (type) == POINTER_TYPE)
	mark_reg_pointer (DECL_RTL (decl));
      REG_USERVAR_P (DECL_RTL (decl)) = 1;
    }
  else if (TREE_LITERAL (DECL_SIZE (decl)))
    {
      rtx oldaddr = 0;
      rtx addr;

      /* If we previously made RTL for this decl, it must be an array
	 whose size was determined by the initializer.
	 The old address was a register; set that register now
	 to the proper address.  */
      if (DECL_RTL (decl) != 0)
	{
	  if (GET_CODE (DECL_RTL (decl)) != MEM
	      || GET_CODE (XEXP (DECL_RTL (decl), 0)) != REG)
	    abort ();
	  oldaddr = XEXP (DECL_RTL (decl), 0);
	}

      /* Variable of fixed size that goes on the stack.  */
      DECL_RTL (decl)
	= assign_stack_local (DECL_MODE (decl),
			      (TREE_INT_CST_LOW (DECL_SIZE (decl))
			       * DECL_SIZE_UNIT (decl)
			       + BITS_PER_UNIT - 1)
			      / BITS_PER_UNIT);
      if (oldaddr)
	{
	  addr = force_operand (XEXP (DECL_RTL (decl), 0), oldaddr);
	  emit_move_insn (oldaddr, addr);
	}

      /* If this is a memory ref that contains aggregate components,
	 mark it as such for cse and loop optimize.  */
      MEM_IN_STRUCT_P (DECL_RTL (decl))
	= (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
	   || TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE
	   || TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE);
#if 0
      /* If this is in memory because of -ffloat-store,
	 set the volatile bit, to prevent optimizations from
	 undoing the effects.  */
      if (flag_float_store && TREE_CODE (type) == REAL_TYPE)
	MEM_VOLATILE_P (DECL_RTL (decl)) = 1;
#endif
    }
  else
    /* Dynamic-size object: must push space on the stack.  */
    {
      rtx address, size;

      frame_pointer_needed = 1;

      /* Record the stack pointer on entry to block, if have
	 not already done so.  */
      if (thisblock->data.block.stack_level == 0)
	{
	  do_pending_stack_adjust ();
	  thisblock->data.block.stack_level
	    = copy_to_reg (stack_pointer_rtx);
	  stack_block_stack = thisblock;
	}

      /* Compute the variable's size, in bytes.  */
      size = expand_expr (convert_units (DECL_SIZE (decl),
					 DECL_SIZE_UNIT (decl),
					 BITS_PER_UNIT),
			  0, VOIDmode, 0);

      /* Round it up to this machine's required stack boundary.  */
#ifdef STACK_BOUNDARY
      /* Avoid extra code if we can prove it's a multiple already.  */
      if (DECL_SIZE_UNIT (decl) % STACK_BOUNDARY)
	{
#ifdef STACK_POINTER_OFFSET
	  /* Avoid extra code if we can prove that adding STACK_POINTER_OFFSET
	     will not give this address invalid alignment.  */
	  if (DECL_ALIGN (decl) > ((STACK_POINTER_OFFSET * BITS_PER_UNIT) % STACK_BOUNDARY))
	    size = plus_constant (size,
				  STACK_POINTER_OFFSET % (STACK_BOUNDARY / BITS_PER_UNIT));
#endif
	  size = round_push (size);
	}
#endif /* STACK_BOUNDARY */

      /* Make space on the stack, and get an rtx for the address of it.  */
#ifdef STACK_GROWS_DOWNWARD
      anti_adjust_stack (size);
#endif
      address = copy_to_reg (stack_pointer_rtx);
#ifdef STACK_POINTER_OFFSET
      {
	/* If the contents of the stack pointer reg are offset from the
	   actual top-of-stack address, add the offset here.  */
	rtx sp_offset = gen_rtx (CONST_INT, VOIDmode, STACK_POINTER_OFFSET);
#ifdef STACK_BOUNDARY
#ifdef STACK_GROWS_DOWNWARD
	int direction = 1;
#else /* not STACK_GROWS_DOWNWARD */
	int direction = 0;
#endif /* not STACK_GROWS_DOWNWARD */
	if (DECL_ALIGN (decl) > ((STACK_POINTER_OFFSET * BITS_PER_UNIT) % STACK_BOUNDARY))
	  sp_offset = plus_constant (sp_offset,
				     (STACK_POINTER_OFFSET
				      % (STACK_BOUNDARY / BITS_PER_UNIT)
				      * direction));
#endif /* STACK_BOUNDARY */
	emit_insn (gen_add2_insn (address, sp_offset));
      }
#endif /* STACK_POINTER_OFFSET */
#ifndef STACK_GROWS_DOWNWARD
      anti_adjust_stack (size);
#endif

      /* Some systems require a particular insn to refer to the stack
	 to make the pages exist.  */
#ifdef HAVE_probe
      if (HAVE_probe)
	emit_insn (gen_probe ());
#endif

      /* Reference the variable indirect through that rtx.  */
      DECL_RTL (decl) = gen_rtx (MEM, DECL_MODE (decl), address);
    }

  if (TREE_VOLATILE (decl))
    MEM_VOLATILE_P (DECL_RTL (decl)) = 1;
  if (TREE_READONLY (decl))
    RTX_UNCHANGING_P (DECL_RTL (decl)) = 1;

  /* If doing stupid register allocation, make sure life of any
     register variable starts here, at the start of its scope.  */

  if (obey_regdecls)
    use_variable (DECL_RTL (decl));
}

/* Emit code to perform the initialization of a declaration DECL.  */

void
expand_decl_init (decl)
     tree decl;
{
  if (TREE_STATIC (decl))
    return;

  /* Compute and store the initial value now.  */

  if (DECL_INITIAL (decl) == error_mark_node)
    {
      enum tree_code code = TREE_CODE (TREE_TYPE (decl));
      if (code == INTEGER_TYPE || code == REAL_TYPE || code == ENUMERAL_TYPE
	  || code == POINTER_TYPE)
	expand_assignment (decl, convert (TREE_TYPE (decl), integer_zero_node),
			   0, 0);
      emit_queue ();
    }
  else if (DECL_INITIAL (decl) && TREE_CODE (DECL_INITIAL (decl)) != TREE_LIST)
    {
      emit_line_note (DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
      expand_assignment (decl, DECL_INITIAL (decl), 0, 0);
      emit_queue ();
    }
}

/* DECL is an anonymous union.  CLEANUP is a cleanup for DECL.
   DECL_ELTS is the list of elements that belong to DECL's type.
   In each, the TREE_VALUE is a VAR_DECL, and the TREE_PURPOSE a cleanup.  */

void
expand_anon_union_decl (decl, cleanup, decl_elts)
     tree decl, cleanup, decl_elts;
{
  struct nesting *thisblock = block_stack;
  rtx x;

  expand_decl (decl, cleanup);
  x = DECL_RTL (decl);

  while (decl_elts)
    {
      tree decl_elt = TREE_VALUE (decl_elts);
      tree cleanup_elt = TREE_PURPOSE (decl_elts);

      DECL_RTL (decl_elt)
	= (GET_MODE (x) != BLKmode
/*
#error broken
/* ??? This is incorrect if X is a MEM.
   (SUBREG (MEM)) is not allowed at rtl generation time.  */
	   ? gen_rtx (SUBREG, TYPE_MODE (TREE_TYPE (decl_elt)), x, 0)
	   : x);

      /* Record the cleanup if there is one.  */

      if (cleanup != 0)
	thisblock->data.block.cleanups
	  = temp_tree_cons (decl_elt, cleanup_elt,
			    thisblock->data.block.cleanups);

      decl_elts = TREE_CHAIN (decl_elts);
    }
}

/* Expand a list of cleanups LIST.
   Elements may be expressions or may be nested lists.

   If DONT_DO is nonnull, then any list-element
   whose TREE_PURPOSE matches DONT_DO is omitted.
   This is sometimes used to avoid a cleanup associated with
   a value that is being returned out of the scope.  */

static void
expand_cleanups (list, dont_do)
     tree list;
     tree dont_do;
{
  tree tail;
  for (tail = list; tail; tail = TREE_CHAIN (tail))
    if (dont_do == 0 || TREE_PURPOSE (tail) != dont_do)
      {
	if (TREE_CODE (TREE_VALUE (tail)) == TREE_LIST)
	  expand_cleanups (TREE_VALUE (tail), dont_do);
	else
	  expand_expr (TREE_VALUE (tail), const0_rtx, VOIDmode, 0);
      }
}

/* Expand a list of cleanups for a goto fixup.
   The expansion is put into the insn chain after the insn *BEFORE_JUMP
   and *BEFORE_JUMP is set to the insn that now comes before the jump.  */

static void
fixup_cleanups (list, before_jump)
     tree list;
     rtx *before_jump;
{
  rtx beyond_jump = get_last_insn ();
  rtx new_before_jump;

  expand_cleanups (list, 0);
  new_before_jump = get_last_insn ();

  reorder_insns (NEXT_INSN (beyond_jump), new_before_jump, *before_jump);
  *before_jump = new_before_jump;
}

/* Move all cleanups from the current block_stack
   to the containing block_stack, where they are assumed to
   have been created.  If anything can cause a temporary to
   be created, but not expanded for more than one level of
   block_stacks, then this code will have to change.  */

void
move_cleanups_up ()
{
  struct nesting *block = block_stack;
  struct nesting *outer = block->next;

  outer->data.block.cleanups
    = chainon (block->data.block.cleanups,
	       outer->data.block.cleanups);
  block->data.block.cleanups = 0;
}

int
this_contour_has_cleanups_p ()
{
  return block_stack && block_stack->data.block.cleanups != 0;
}

/* Enter a case (Pascal) or switch (C) statement.
   Push a block onto case_stack and nesting_stack
   to accumulate the case-labels that are seen
   and to record the labels generated for the statement.

   EXIT_FLAG is nonzero if `exit_something' should exit this case stmt.
   Otherwise, this construct is transparent for `exit_something'.

   EXPR is the index-expression to be dispatched on.
   TYPE is its nominal type.  We could simply convert EXPR to this type,
   but instead we take short cuts.  */

void
expand_start_case (exit_flag, expr, type)
     int exit_flag;
     tree expr;
     tree type;
{
  register struct nesting *thiscase
    = (struct nesting *) xmalloc (sizeof (struct nesting));

  /* Make an entry on case_stack for the case we are entering.  */

  thiscase->next = case_stack;
  thiscase->all = nesting_stack;
  thiscase->depth = ++nesting_depth;
  thiscase->exit_label = exit_flag ? gen_label_rtx () : 0;
  thiscase->data.case_stmt.case_list = 0;
  thiscase->data.case_stmt.index_expr = expr;
  thiscase->data.case_stmt.nominal_type = type;
  thiscase->data.case_stmt.default_label = 0;
  thiscase->data.case_stmt.num_ranges = 0;
  case_stack = thiscase;
  nesting_stack = thiscase;

  do_pending_stack_adjust ();

  /* Make sure case_stmt.start points to something that won't
     need any transformation before expand_end_case.  */
  emit_note (0, NOTE_INSN_DELETED);

  thiscase->data.case_stmt.start = get_last_insn ();
}

/* Start a "dummy case statement" within which case labels are invalid
   and are not connected to any larger real case statement.
   This can be used if you don't want to let a case statement jump
   into the middle of certain kinds of constructs.  */

void
expand_start_case_dummy ()
{
  register struct nesting *thiscase
    = (struct nesting *) xmalloc (sizeof (struct nesting));

  /* Make an entry on case_stack for the dummy.  */

  thiscase->next = case_stack;
  thiscase->all = nesting_stack;
  thiscase->depth = ++nesting_depth;
  thiscase->exit_label = 0;
  thiscase->data.case_stmt.case_list = 0;
  thiscase->data.case_stmt.start = 0;
  thiscase->data.case_stmt.nominal_type = 0;
  thiscase->data.case_stmt.default_label = 0;
  thiscase->data.case_stmt.num_ranges = 0;
  case_stack = thiscase;
  nesting_stack = thiscase;
}

/* End a dummy case statement.  */

void
expand_end_case_dummy ()
{
  POPSTACK (case_stack);
}

/* Accumulate one case or default label inside a case or switch statement.
   VALUE is the value of the case (a null pointer, for a default label).

   If not currently inside a case or switch statement, return 1 and do
   nothing.  The caller will print a language-specific error message.
   If VALUE is a duplicate or overlaps, return 2 and do nothing.
   If VALUE is out of range, return 3 and do nothing.
   Return 0 on success.

   Extended to handle range statements, should they ever
   be adopted.  */

int
pushcase (value, label)
     register tree value;
     register tree label;
{
  register struct case_node **l;
  register struct case_node *n;
  tree index_type;
  tree nominal_type;

  /* Fail if not inside a real case statement.  */
  if (! (case_stack && case_stack->data.case_stmt.start))
    return 1;

  index_type = TREE_TYPE (case_stack->data.case_stmt.index_expr);
  nominal_type = case_stack->data.case_stmt.nominal_type;

  /* If the index is erroneous, avoid more problems: pretend to succeed.  */
  if (index_type == error_mark_node)
    return 0;

  /* Convert VALUE to the type in which the comparisons are nominally done.  */
  if (value != 0)
    value = convert (nominal_type, value);

  /* Fail if this value is out of range for the actual type of the index
     (which may be narrower than NOMINAL_TYPE).  */
  if (value != 0 && ! int_fits_type_p (value, index_type))
    return 3;

  /* Fail if this is a duplicate or overlaps another entry.  */
  if (value == 0)
    {
      if (case_stack->data.case_stmt.default_label != 0)
	return 2;
      case_stack->data.case_stmt.default_label = label;
    }
  else
    {
      /* Find the elt in the chain before which to insert the new value,
	 to keep the chain sorted in increasing order.
	 But report an error if this element is a duplicate.  */
      for (l = &case_stack->data.case_stmt.case_list;
	   /* Keep going past elements distinctly less than VALUE.  */
	   *l != 0 && tree_int_cst_lt ((*l)->high, value);
	   l = &(*l)->right)
	;
      if (*l)
	{
	  /* Element we will insert before must be distinctly greater;
	     overlap means error.  */
	  if (! tree_int_cst_lt (value, (*l)->low))
	    return 2;
	}

      /* Add this label to the chain, and succeed.
	 Copy VALUE so it is on temporary rather than momentary
	 obstack and will thus survive till the end of the case statement.  */
      n = (struct case_node *) oballoc (sizeof (struct case_node));
      n->left = 0;
      n->right = *l;
      n->high = n->low = copy_node (value);
      n->code_label = label;
      n->test_label = 0;
      *l = n;
    }

  expand_label (label);
  return 0;
}

/* Like pushcase but this case applies to all values
   between VALUE1 and VALUE2 (inclusive).
   The return value is the same as that of pushcase
   but there is one additional error code:
   4 means the specified range was empty.

   Note that this does not currently work, since expand_end_case
   has yet to be extended to handle RANGE_EXPRs.  */

int
pushcase_range (value1, value2, label)
     register tree value1, value2;
     register tree label;
{
  register struct case_node **l;
  register struct case_node *n;
  tree index_type;
  tree nominal_type;

  /* Fail if not inside a real case statement.  */
  if (! (case_stack && case_stack->data.case_stmt.start))
    return 1;

  index_type = TREE_TYPE (case_stack->data.case_stmt.index_expr);
  nominal_type = case_stack->data.case_stmt.nominal_type;

  /* If the index is erroneous, avoid more problems: pretend to succeed.  */
  if (index_type == error_mark_node)
    return 0;

  /* Convert VALUEs to type in which the comparisons are nominally done.  */
  if (value1 != 0)
    value1 = convert (nominal_type, value1);
  if (value2 != 0)
    value2 = convert (nominal_type, value2);

  /* Fail if these values are out of range.  */
  if (value1 != 0 && ! int_fits_type_p (value1, index_type))
    return 3;

  if (value2 != 0 && ! int_fits_type_p (value2, index_type))
    return 3;

  /* Fail if the range is empty.  */
  if (tree_int_cst_lt (value2, value1))
    return 4;

  /* If the bounds are equal, turn this into the one-value case.  */
  if (tree_int_cst_equal (value1, value2))
    return pushcase (value1, label);

  /* Find the elt in the chain before which to insert the new value,
     to keep the chain sorted in increasing order.
     But report an error if this element is a duplicate.  */
  for (l = &case_stack->data.case_stmt.case_list;
       /* Keep going past elements distinctly less than this range.  */
       *l != 0 && tree_int_cst_lt ((*l)->high, value1);
       l = &(*l)->right)
    ;
  if (*l)
    {
      /* Element we will insert before must be distinctly greater;
	 overlap means error.  */
      if (! tree_int_cst_lt (value2, (*l)->low))
	return 2;
    }

  /* Add this label to the chain, and succeed.
     Copy VALUE1, VALUE2 so they are on temporary rather than momentary
     obstack and will thus survive till the end of the case statement.  */

  n = (struct case_node *) oballoc (sizeof (struct case_node));
  n->left = 0;
  n->right = *l;
  n->low = copy_node (value1);
  n->high = copy_node (value2);
  n->code_label = label;
  n->test_label = 0;
  *l = n;

  expand_label (label);

  case_stack->data.case_stmt.num_ranges++;

  return 0;
}

/* Check that all enumeration literals are covered by the case
   expressions of a switch.  Also, warn if there are any extra
   switch cases that are *not* elements of the enumerated type. */

static void
check_for_full_enumeration_handling (type)
     tree type;
{
  register struct case_node *n;
  register tree chain;
          
  /* The time complexity of this loop is currently O(N * M), with
     N being the number of enumerals in the enumerated type, and 
     M being the number of case expressions in the switch. */
             
  for (chain = TYPE_VALUES (type);
       chain; 
       chain = TREE_CHAIN (chain))
    {
      /* Find a match between enumeral and case expression, if possible.
	 Quit looking when we've gone too far (since case expressions
	 are kept sorted in ascending order).  Warn about enumerals not
	 handled in the switch statement case expression list. */

      for (n = case_stack->data.case_stmt.case_list; 
	   n && tree_int_cst_lt (n->high, TREE_VALUE (chain));
	   n = n->right)
	;

      if (!(n && tree_int_cst_equal (n->low, TREE_VALUE (chain))))
	warning ("enumerated value `%s' not handled in switch",
		 IDENTIFIER_POINTER (TREE_PURPOSE (chain)));
    }

  /* Now we go the other way around; we warn if there are case 
     expressions that don't correspond to enumerals.  This can
     occur since C and C++ don't enforce type-checking of 
     assignments to enumeration variables. */

  for (n = case_stack->data.case_stmt.case_list; n; n = n->right)
    {
      for (chain = TYPE_VALUES (type);
	   chain && !tree_int_cst_equal (n->low, TREE_VALUE (chain)); 
	   chain = TREE_CHAIN (chain))
	;

      if (!chain)
	warning ("case value `%d' not in enumerated type `%s'",
		 TREE_INT_CST_LOW (n->low), 
		 IDENTIFIER_POINTER (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE
				     ? TYPE_NAME (type)
				     : DECL_NAME (TYPE_NAME (type))));
    }
}

/* Terminate a case (Pascal) or switch (C) statement
   in which CASE_INDEX is the expression to be tested.
   Generate the code to test it and jump to the right place.  */

void
expand_end_case (orig_index)
     tree orig_index;
{
  tree minval, maxval, range;
  rtx default_label = 0;
  register struct case_node *n;
  int count;
  rtx index;
  rtx table_label = gen_label_rtx ();
  int ncases;
  rtx *labelvec;
  register int i;
  rtx before_case;
  register struct nesting *thiscase = case_stack;
  tree index_expr = thiscase->data.case_stmt.index_expr;
  int unsignedp = TREE_UNSIGNED (TREE_TYPE (index_expr));

  do_pending_stack_adjust ();

  /* An ERROR_MARK occurs for various reasons including invalid data type.  */
  if (TREE_TYPE (index_expr) != error_mark_node)
    {
      /* If switch expression was an enumerated type, check that all
	 enumeration literals are covered by the cases.
	 No sense trying this if there's a default case, however.  */

      if (!thiscase->data.case_stmt.default_label 
	  && TREE_CODE (TREE_TYPE (orig_index)) == ENUMERAL_TYPE
	  && TREE_CODE (index_expr) != INTEGER_CST
	  && warn_switch)
	check_for_full_enumeration_handling (TREE_TYPE (orig_index));

      /* If we don't have a default-label, create one here,
	 after the body of the switch.  */
      if (thiscase->data.case_stmt.default_label == 0)
	{
	  thiscase->data.case_stmt.default_label
	    = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
	  expand_label (thiscase->data.case_stmt.default_label);
	}
      default_label = label_rtx (thiscase->data.case_stmt.default_label);

      before_case = get_last_insn ();

      /* Simplify the case-list before we count it.  */
      group_case_nodes (thiscase->data.case_stmt.case_list);

      /* Get upper and lower bounds of case values.
	 Also convert all the case values to the index expr's data type.  */

      count = 0;
      for (n = thiscase->data.case_stmt.case_list; n; n = n->right)
	{
	  /* Check low and high label values are integers.  */
	  if (TREE_CODE (n->low) != INTEGER_CST)
	    abort ();
	  if (TREE_CODE (n->high) != INTEGER_CST)
	    abort ();

	  n->low = convert (TREE_TYPE (index_expr), n->low);
	  n->high = convert (TREE_TYPE (index_expr), n->high);

	  /* Count the elements and track the largest and smallest
	     of them (treating them as signed even if they are not).  */
	  if (count++ == 0)
	    {
	      minval = n->low;
	      maxval = n->high;
	    }
	  else
	    {
	      if (INT_CST_LT (n->low, minval))
		minval = n->low;
	      if (INT_CST_LT (maxval, n->high))
		maxval = n->high;
	    }
	  /* A range counts double, since it requires two compares.  */
	  if (! tree_int_cst_equal (n->low, n->high))
	    count++;
	}

      /* Compute span of values.  */
      if (count != 0)
	range = combine (MINUS_EXPR, maxval, minval);

      if (count == 0 || TREE_CODE (TREE_TYPE (index_expr)) == ERROR_MARK)
	{
	  expand_expr (index_expr, const0_rtx, VOIDmode, 0);
	  emit_queue ();
	  emit_jump (default_label);
	}
      /* If range of values is much bigger than number of values,
	 make a sequence of conditional branches instead of a dispatch.
	 If the switch-index is a constant, do it this way
	 because we can optimize it.  */
      else if (TREE_INT_CST_HIGH (range) != 0
#ifdef HAVE_casesi
	       || count < 4
#else
	       /* If machine does not have a case insn that compares the
		  bounds, this means extra overhead for dispatch tables
		  which raises the threshold for using them.  */
	       || count < 5
#endif
	       || (unsigned) (TREE_INT_CST_LOW (range)) > 10 * count
	       || TREE_CODE (index_expr) == INTEGER_CST)
	{
	  index = expand_expr (index_expr, 0, VOIDmode, 0);

	  /* If the index is a short or char that we do not have
	     an insn to handle comparisons directly, convert it to
	     a full integer now, rather than letting each comparison
	     generate the conversion.  */

	  if ((GET_MODE (index) == QImode || GET_MODE (index) == HImode)
	      && (cmp_optab->handlers[(int) GET_MODE(index)].insn_code
		  == CODE_FOR_nothing))
	    index = convert_to_mode (SImode, index, unsignedp);
	  
	  emit_queue ();
	  do_pending_stack_adjust ();

	  index = protect_from_queue (index, 0);
	  if (GET_CODE (index) == MEM)
	    index = copy_to_reg (index);
	  if (GET_CODE (index) == CONST_INT
	      || TREE_CODE (index_expr) == INTEGER_CST)
	    {
	      /* Make a tree node with the proper constant value
		 if we don't already have one.  */
	      if (TREE_CODE (index_expr) != INTEGER_CST)
		{
		  index_expr
		    = build_int_2 (INTVAL (index),
				   !unsignedp && INTVAL (index) >= 0 ? 0 : -1);
		  index_expr = convert (TREE_TYPE (index_expr), index_expr);
		}

	      /* For constant index expressions we need only
		 issue a unconditional branch to the appropriate
		 target code.  The job of removing any unreachable
		 code is left to the optimisation phase if the
		 "-O" option is specified.  */
	      for (n = thiscase->data.case_stmt.case_list;
		   n;
		   n = n->right)
		{
		  if (! tree_int_cst_lt (index_expr, n->low)
		      && ! tree_int_cst_lt (n->high, index_expr))
		    break;
		}
	      if (n)
		emit_jump (label_rtx (n->code_label));
	      else
		emit_jump (default_label);
	    }
	  else
	    {
	      /* If the index expression is not constant we generate
		 a binary decision tree to select the appropriate
		 target code.  This is done as follows:

		 The list of cases is rearranged into a binary tree,
		 nearly optimal assuming equal probability for each case.

		 The tree is transformed into RTL, eliminating
		 redundant test conditions at the same time.

		 If program flow could reach the end of the
		 decision tree an unconditional jump to the
		 default code is emitted.  */
	      balance_case_nodes (&thiscase->data.case_stmt.case_list, 0);
	      emit_case_nodes (index, thiscase->data.case_stmt.case_list,
			       default_label, unsignedp);
	      emit_jump_if_reachable (default_label);
	    }
	}
      else
	{
#ifdef HAVE_casesi
	  /* Convert the index to SImode.  */
	  if (TYPE_MODE (TREE_TYPE (index_expr)) == DImode)
	    {
	      index_expr = build (MINUS_EXPR, TREE_TYPE (index_expr),
				  index_expr, minval);
	      minval = integer_zero_node;
	    }
	  if (TYPE_MODE (TREE_TYPE (index_expr)) != SImode)
	    index_expr = convert (type_for_size (GET_MODE_BITSIZE (SImode), 0),
				  index_expr);
	  index = expand_expr (index_expr, 0, VOIDmode, 0);
	  emit_queue ();
	  index = protect_from_queue (index, 0);
	  do_pending_stack_adjust ();

	  emit_jump_insn (gen_casesi (index, expand_expr (minval, 0, VOIDmode, 0),
				      expand_expr (range, 0, VOIDmode, 0),
				      table_label, default_label));
#else
#ifdef HAVE_tablejump
	  index_expr = convert (type_for_size (GET_MODE_BITSIZE (SImode), 0),
				build (MINUS_EXPR, TREE_TYPE (index_expr),
				       index_expr, minval));
	  index = expand_expr (index_expr, 0, VOIDmode, 0);
	  emit_queue ();
	  index = protect_from_queue (index, 0);
	  do_pending_stack_adjust ();

	  do_tablejump (index,
			gen_rtx (CONST_INT, VOIDmode, TREE_INT_CST_LOW (range)),
			table_label, default_label);
#else
	  lossage;
#endif				/* not HAVE_tablejump */
#endif				/* not HAVE_casesi */

	  /* Get table of labels to jump to, in order of case index.  */

	  ncases = TREE_INT_CST_LOW (range) + 1;
	  labelvec = (rtx *) alloca (ncases * sizeof (rtx));
	  bzero (labelvec, ncases * sizeof (rtx));

	  for (n = thiscase->data.case_stmt.case_list; n; n = n->right)
	    {
	      register int i
		= TREE_INT_CST_LOW (n->low) - TREE_INT_CST_LOW (minval);

	      while (i + TREE_INT_CST_LOW (minval)
		     <= TREE_INT_CST_LOW (n->high))
		labelvec[i++]
		  = gen_rtx (LABEL_REF, Pmode, label_rtx (n->code_label));
	    }

	  /* Fill in the gaps with the default.  */
	  for (i = 0; i < ncases; i++)
	    if (labelvec[i] == 0)
	      labelvec[i] = gen_rtx (LABEL_REF, Pmode, default_label);

	  /* Output the table */
	  emit_label (table_label);

#ifdef CASE_VECTOR_PC_RELATIVE
	  emit_jump_insn (gen_rtx (ADDR_DIFF_VEC, CASE_VECTOR_MODE,
				   gen_rtx (LABEL_REF, Pmode, table_label),
				   gen_rtvec_v (ncases, labelvec)));
#else
	  emit_jump_insn (gen_rtx (ADDR_VEC, CASE_VECTOR_MODE,
				   gen_rtvec_v (ncases, labelvec)));
#endif
	  /* If the case insn drops through the table,
	     after the table we must jump to the default-label.
	     Otherwise record no drop-through after the table.  */
#ifdef CASE_DROPS_THROUGH
	  emit_jump (default_label);
#else
	  emit_barrier ();
#endif
	}

      reorder_insns (NEXT_INSN (before_case), get_last_insn (),
		     thiscase->data.case_stmt.start);
    }
  if (thiscase->exit_label)
    emit_label (thiscase->exit_label);

  POPSTACK (case_stack);
}

/* Generate code to jump to LABEL if OP1 and OP2 are equal.  */

static void
do_jump_if_equal (op1, op2, label, unsignedp)
     rtx op1, op2, label;
     int unsignedp;
{
  if (GET_CODE (op1) == CONST_INT
      && GET_CODE (op2) == CONST_INT)
    {
      if (INTVAL (op1) == INTVAL (op2))
	emit_jump (label);
    }
  else
    {
      emit_cmp_insn (op1, op2, 0, unsignedp, 0);
      emit_jump_insn (gen_beq (label));
    }
}

/* Scan an ordered list of case nodes
   combining those with consecutive values or ranges.

   Eg. three separate entries 1: 2: 3: become one entry 1..3:  */

static void
group_case_nodes (head)
     case_node_ptr head;
{
  case_node_ptr node = head;

  while (node)
    {
      rtx lb = next_real_insn (label_rtx (node->code_label));
      case_node_ptr np = node;

      /* Try to group the successors of NODE with NODE.  */
      while (((np = np->right) != 0)
	     /* Do they jump to the same place?  */
	     && next_real_insn (label_rtx (np->code_label)) == lb
	     /* Are their ranges consecutive?  */
	     && tree_int_cst_equal (np->low,
				    combine (PLUS_EXPR, node->high,
					     build_int_2 (1, 0)))
	     /* An overflow is not consecutive.  */
	     && tree_int_cst_lt (node->high, 
				 combine (PLUS_EXPR, node->high,
					  build_int_2 (1, 0))))
	{
	  node->high = np->high;
	}
      /* NP is the first node after NODE which can't be grouped with it.
	 Delete the nodes in between, and move on to that node.  */
      node->right = np;
      node = np;
    }
}

/* Take an ordered list of case nodes
   and transform them into a near optimal binary tree,
   on the assumtion that any target code selection value is as
   likely as any other.

   The transformation is performed by splitting the ordered
   list into two equal sections plus a pivot.  The parts are
   then attached to the pivot as left and right branches.  Each
   branch is is then transformed recursively.  */

static void
balance_case_nodes (head, parent)
     case_node_ptr *head;
     case_node_ptr parent;
{
  register case_node_ptr np;

  np = *head;
  if (np)
    {
      int i = 0;
      int ranges = 0;
      register case_node_ptr *npp;
      case_node_ptr left;

      /* Count the number of entries on branch.
	 Also count the ranges.  */
      while (np)
	{
	  if (!tree_int_cst_equal (np->low, np->high))
	    ranges++;
	  i++;
	  np = np->right;
	}
      if (i > 2)
	{
	  /* Split this list if it is long enough for that to help.  */
	  npp = head;
	  left = *npp;
	  /* If there are just three nodes, split at the middle one.  */
	  if (i == 3)
	    npp = &(*npp)->right;
	  else
	    {
	      /* Find the place in the list that bisects the list's total cost,
		 where ranges count as 2.
		 Here I gets half the total cost.  */
	      i = (i + ranges + 1) / 2;
	      while (1)
		{
		  /* Skip nodes while their cost does not reach that amount.  */
		  if (!tree_int_cst_equal ((*npp)->low, (*npp)->high))
		    i--;
		  i--;
		  if (i <= 0)
		    break;
		  npp = &(*npp)->right;
		}
	    }
	  *head = np = *npp;
	  *npp = 0;
	  np->parent = parent;
	  np->left = left;

	  /* Optimize each of the two split parts.  */
	  balance_case_nodes (&np->left, np);
	  balance_case_nodes (&np->right, np);
	}
      else
	{
	  /* Else leave this branch as one level,
	     but fill in `parent' fields.  */
	  np = *head;
	  np->parent = parent;
	  for (; np->right; np = np->right)
	    np->right->parent = np;
	}
    }
}

/* Search the parent sections of the case node tree
   to see if a test for the lower bound of NODE would be redundant.

   The instructions to synthesis the case decision tree are
   output in the same order as nodes are processed so it is
   known that if a parent node checks the range of the current
   node minus one that the current node is bounded at its lower
   span.  Thus the test would be redundant.  */

static int
node_has_low_bound (node)
     case_node_ptr node;
{
  tree low_minus_one;
  case_node_ptr pnode;

  if (node->left)
    {
      low_minus_one = combine (MINUS_EXPR, node->low, build_int_2 (1, 0));
      /* Avoid the screw case of overflow where low_minus_one is > low.  */
      if (tree_int_cst_lt (low_minus_one, node->low))
	for (pnode = node->parent; pnode; pnode = pnode->parent)
	  {
	    if (tree_int_cst_equal (low_minus_one, pnode->high))
	      return 1;
	    /* If a parent node has a left branch we know that none
	       of its parents can have a high bound of our target
	       minus one so we abort the search.  */
	    if (node->left)
	      break;
	  }
    }
  return 0;
}

/* Search the parent sections of the case node tree
   to see if a test for the upper bound of NODE would be redundant.

   The instructions to synthesis the case decision tree are
   output in the same order as nodes are processed so it is
   known that if a parent node checks the range of the current
   node plus one that the current node is bounded at its upper
   span.  Thus the test would be redundant.  */

static int
node_has_high_bound (node)
     case_node_ptr node;
{
  tree high_plus_one;
  case_node_ptr pnode;

  if (node->right == 0)
    {
      high_plus_one = combine (PLUS_EXPR, node->high, build_int_2 (1, 0));
      /* Avoid the screw case of overflow where high_plus_one is > high.  */
      if (tree_int_cst_lt (node->high, high_plus_one))
	for (pnode = node->parent; pnode; pnode = pnode->parent)
	  {
	    if (tree_int_cst_equal (high_plus_one, pnode->low))
	      return 1;
	    /* If a parent node has a right branch we know that none
	       of its parents can have a low bound of our target
	       plus one so we abort the search.  */
	    if (node->right)
	      break;
	  }
    }
  return 0;
}

/* Search the parent sections of the
   case node tree to see if both tests for the upper and lower
   bounds of NODE would be redundant.  */

static int
node_is_bounded (node)
     case_node_ptr node;
{
  if (node->left || node->right)
    return 0;
  return node_has_low_bound (node) && node_has_high_bound (node);
}

/*  Emit an unconditional jump to LABEL unless it would be dead code.  */

static void
emit_jump_if_reachable (label)
     rtx label;
{
  rtx last_insn;

  if (GET_CODE (get_last_insn ()) != BARRIER)
    emit_jump (label);
}

/* Emit step-by-step code to select a case for the value of INDEX.
   The thus generated decision tree follows the form of the
   case-node binary tree NODE, whose nodes represent test conditions.
   UNSIGNEDP is nonzero if we should do unsigned comparisons.

   Care is taken to prune redundant tests from the decision tree
   by detecting any boundary conditions already checked by
   emitted rtx.  (See node_has_high_bound, node_has_low_bound
   and node_is_bounded, above.)

   Where the test conditions can be shown to be redundant we emit
   an unconditional jump to the target code.  As a further
   optimization, the subordinates of a tree node are examined to
   check for bounded nodes.  In this case conditional and/or
   unconditional jumps as a result of the boundary check for the
   current node are arranged to target the subordinates associated
   code for out of bound conditions on the current node node.  */

static void
emit_case_nodes (index, node, default_label, unsignedp)
     rtx index;
     case_node_ptr node;
     rtx default_label;
     int unsignedp;
{
  /* If INDEX has an unsigned type, we must make unsigned branches.  */
  typedef rtx rtx_function ();
  rtx_function *gen_bgt_pat = unsignedp ? gen_bgtu : gen_bgt;
  rtx_function *gen_bge_pat = unsignedp ? gen_bgeu : gen_bge;
  rtx_function *gen_blt_pat = unsignedp ? gen_bltu : gen_blt;
  rtx_function *gen_ble_pat = unsignedp ? gen_bleu : gen_ble;

  if (node->test_label)
    {
      /* If this test node requires a label it follows that
	 it must be preceeded by an unconditional branch.
	 If control can pass to this point we can assume that
	 a "br default" is in order.  */
      emit_jump_if_reachable (default_label);
      expand_label (node->test_label);
    }
  if (tree_int_cst_equal (node->low, node->high))
    {
      /* Node is single valued.  */
      do_jump_if_equal (index, expand_expr (node->low, 0, VOIDmode, 0),
			label_rtx (node->code_label), unsignedp);
      if (node->right)
	{
	  if (node->left)
	    {
	      /* This node has children on either side.  */
	      emit_cmp_insn (index, expand_expr (node->high, 0, VOIDmode, 0), 0, unsignedp, 0);

	      if (node_is_bounded (node->right))
		{
		  emit_jump_insn ((*gen_bgt_pat) (label_rtx (node->right->code_label)));
		  if (node_is_bounded (node->left))
		    emit_jump (label_rtx (node->left->code_label));
		  else
		    emit_case_nodes (index, node->left,
				     default_label, unsignedp);
		}
	      else
		{
		  if (node_is_bounded (node->left))
		    emit_jump_insn ((*gen_blt_pat) (label_rtx (node->left->code_label)));
		  else
		    {
		      node->right->test_label =
			build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
		      emit_jump_insn ((*gen_bgt_pat) (label_rtx (node->right->test_label)));
		      emit_case_nodes (index, node->left,
				       default_label, unsignedp);
		    }
		  emit_case_nodes (index, node->right,
				   default_label, unsignedp);
		}
	    }
	  else
	    {
	      /* Here we have a right child but no left
		 so we issue conditional branch to default
		 and process the right child.  */

	      /* Omit the conditional branch to default
		 if we it avoid only one right child;
		 it costs too much space to save so little time.  */
	      if (node->right->right && !node_has_low_bound (node))
		{
		  emit_cmp_insn (index, expand_expr (node->high, 0, VOIDmode, 0), 0, unsignedp, 0);
		  emit_jump_insn ((*gen_blt_pat) (default_label));
		}
	      if (node_is_bounded (node->right))
		emit_jump (label_rtx (node->right->code_label));
	      else
		emit_case_nodes (index, node->right, default_label, unsignedp);
	    }
	}
      else if (node->left)
	{
	  if (node_is_bounded (node->left))
	    emit_jump (label_rtx (node->left->code_label));
	  else
	    emit_case_nodes (index, node->left, default_label, unsignedp);
	}
    }
  else
    {
      /* Node is a range.  */
      if (node->right)
	{
	  if (node->left)
	    {
	      emit_cmp_insn (index, expand_expr (node->high, 0, VOIDmode, 0), 0, unsignedp, 0);
	      if (node_is_bounded (node->right))
		{
		  /* Right hand node is fully bounded so we can
		     eliminate any testing and branch directly
		     to the target code.  */
		  emit_jump_insn ((*gen_bgt_pat) (label_rtx (node->right->code_label)));
		}
	      else
		{
		  /* Right hand node requires testing so create
		     a label to put on the cmp code.  */
		  node->right->test_label =
		    build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
		  emit_jump_insn ((*gen_bgt_pat) (label_rtx (node->right->test_label)));
		}
	      emit_cmp_insn (index, expand_expr (node->low, 0, VOIDmode, 0), 0, unsignedp, 0);
	      emit_jump_insn ((*gen_bge_pat) (label_rtx (node->code_label)));
	      if (node_is_bounded (node->left))
		{
		  /* Left hand node is fully bounded so we can
		     eliminate any testing and branch directly
		     to the target code.  */
		  emit_jump (label_rtx (node->left->code_label));
		}
	      else
		emit_case_nodes (index, node->left, default_label, unsignedp);
	      /* If right node has been given a test label above
		 we must process it now.  */
	      if (node->right->test_label)
		emit_case_nodes (index, node->right, default_label, unsignedp);
	    }
	  else
	    {
	      if (!node_has_low_bound (node))
		{
		  emit_cmp_insn (index, expand_expr (node->low, 0, VOIDmode, 0), 0, unsignedp, 0);
		  emit_jump_insn ((*gen_blt_pat) (default_label));
		}
	      emit_cmp_insn (index, expand_expr (node->high, 0, VOIDmode, 0), 0, unsignedp, 0);
	      emit_jump_insn ((*gen_ble_pat) (label_rtx (node->code_label)));
	      if (node_is_bounded (node->right))
		{
		  /* Right hand node is fully bounded so we can
		     eliminate any testing and branch directly
		     to the target code.  */
		  emit_jump (label_rtx (node->right->code_label));
		}
	      else
		emit_case_nodes (index, node->right, default_label, unsignedp);
	    }
	}
      else if (node->left)
	{
	  if (!node_has_high_bound (node))
	    {
	      emit_cmp_insn (index, expand_expr (node->high, 0, VOIDmode, 0), 0, unsignedp, 0);
	      emit_jump_insn ((*gen_bgt_pat) (default_label));
	    }
	  emit_cmp_insn (index, expand_expr (node->low, 0, VOIDmode, 0), 0, unsignedp, 0);
	  emit_jump_insn ((*gen_bge_pat) (label_rtx (node->code_label)));
	  if (node_is_bounded (node->left))
	    {
	      /* Left hand node is fully bounded so we can
		 eliminate any testing and branch directly
		 to the target code.  */
	      emit_jump (label_rtx (node->left->code_label));
	    }
	  else
	    emit_case_nodes (index, node->left, default_label, unsignedp);
	}
      else
	{
	  /* Node has no children so we check low and
	     high bounds to remove redundant tests. In practice
	     only one of the limits may be bounded or the parent
	     node will have emmited a jump to our target code.  */
	  if (!node_has_high_bound (node))
	    {
	      emit_cmp_insn (index, expand_expr (node->high, 0, VOIDmode, 0), 0, unsignedp, 0);
	      emit_jump_insn ((*gen_bgt_pat) (default_label));
	    }
	  if (!node_has_low_bound (node))
	    {
	      emit_cmp_insn (index, expand_expr (node->low, 0, VOIDmode, 0), 0, unsignedp, 0);
	      emit_jump_insn ((*gen_bge_pat) (label_rtx (node->code_label)));
	    }
	  /* We allow the default case to drop through since
	     it will picked up by calls to `jump_if_reachable'
	     either on the next test label or at the end of
	     the decision tree emission.  */
	}
    }
}

/* Allocate fixed slots in the stack frame of the current function.  */

/* Return size needed for stack frame based on slots so far allocated.  */

int
get_frame_size ()
{
#ifdef FRAME_GROWS_DOWNWARD
  return -frame_offset + STARTING_FRAME_OFFSET;
#else
  return frame_offset - STARTING_FRAME_OFFSET;
#endif
}

/* Allocate a stack slot of SIZE bytes and return a MEM rtx for it
   with machine mode MODE.  */

rtx
assign_stack_local (mode, size)
     enum machine_mode mode;
     int size;
{
  register rtx x, addr;
  int bigend_correction = 0;

  frame_pointer_needed = 1;

  /* Make each stack slot a multiple of the main allocation unit.  */
  size = (((size + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
	   / (BIGGEST_ALIGNMENT / BITS_PER_UNIT))
	  * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  /* On a big-endian machine, if we are allocating more space than we will use,
     use the least significant bytes of those that are allocated.  */
#ifdef BYTES_BIG_ENDIAN
  if (mode != BLKmode)
    bigend_correction = size - GET_MODE_SIZE (mode);
#endif

#ifdef FRAME_GROWS_DOWNWARD
  frame_offset -= size;
#endif
  addr = gen_rtx (PLUS, Pmode, frame_pointer_rtx,
		  gen_rtx (CONST_INT, VOIDmode,
			   (frame_offset + bigend_correction)));
#ifndef FRAME_GROWS_DOWNWARD
  frame_offset += size;
#endif

  if (! memory_address_p (mode, addr))
    invalid_stack_slot = 1;

  x = gen_rtx (MEM, mode, addr);

  stack_slot_list = gen_rtx (EXPR_LIST, VOIDmode, x, stack_slot_list);

  return x;
}

/* Retroactively move an auto variable from a register to a stack slot.
   This is done when an address-reference to the variable is seen.  */

void
put_var_into_stack (decl)
     tree decl;
{
  register rtx reg = DECL_RTL (decl);
  register rtx new;

  /* No need to do anything if decl has no rtx yet
     since in that case caller is setting TREE_ADDRESSABLE
     and a stack slot will be assigned when the rtl is made.  */
  if (reg == 0)
    return;
  if (GET_CODE (reg) != REG)
    return;

  new = parm_stack_loc (reg);
  if (new == 0)
    new = assign_stack_local (GET_MODE (reg), GET_MODE_SIZE (GET_MODE (reg)));

  XEXP (reg, 0) = XEXP (new, 0);
  /* `volatil' bit means one thing for MEMs, another entirely for REGs.  */
  REG_USERVAR_P (reg) = 0;
  PUT_CODE (reg, MEM);

  /* If this is a memory ref that contains aggregate components,
     mark it as such for cse and loop optimize.  */
  MEM_IN_STRUCT_P (reg)
    = (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
       || TREE_CODE (TREE_TYPE (decl)) == RECORD_TYPE
       || TREE_CODE (TREE_TYPE (decl)) == UNION_TYPE);

  fixup_var_refs (reg);
}

static void
fixup_var_refs (var)
     rtx var;
{
  extern rtx sequence_stack;
  rtx stack = sequence_stack;
  tree pending;

  stack = sequence_stack;

  /* Must scan all insns for stack-refs that exceed the limit.  */
  fixup_var_refs_insns (var, get_insns (), stack == 0);

  /* Scan all pending sequences too.  */
  for (; stack; stack = XEXP (XEXP (stack, 1), 1))
    {
      push_to_sequence (XEXP (stack, 0));
      fixup_var_refs_insns (var, XEXP (stack, 0),
			    XEXP (XEXP (stack, 1), 1) == 0);
      /* Update remembered end of sequence
	 in case we added an insn at the end.  */
      XEXP (XEXP (stack, 1), 0) = get_last_insn ();
      end_sequence ();
    }

  /* Scan all waiting RTL_EXPRs too.  */
  for (pending = rtl_expr_chain; pending; pending = TREE_CHAIN (pending))
    {
      rtx seq = RTL_EXPR_SEQUENCE (TREE_VALUE (pending));
      if (seq != const0_rtx && seq != 0)
	{
	  push_to_sequence (seq);
	  fixup_var_refs_insns (var, seq, 0);
	  end_sequence ();
	}
    }
}

/* Scan the insn-chain starting with INSN for refs to VAR
   and fix them up.  TOPLEVEL is nonzero if this chain is the
   main chain of insns for the current function.  */

static void
fixup_var_refs_insns (var, insn, toplevel)
     rtx var;
     rtx insn;
     int toplevel;
{
  while (insn)
    {
      rtx next = NEXT_INSN (insn);
      rtx note;
      if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	  || GET_CODE (insn) == JUMP_INSN)
	{
	  /* The insn to load VAR from a home in the arglist
	     is now a no-op.  When we see it, just delete it.  */
	  if (toplevel
	      && GET_CODE (PATTERN (insn)) == SET
	      && SET_DEST (PATTERN (insn)) == var
	      && rtx_equal_p (SET_SRC (PATTERN (insn)), var))
	    {
	      next = delete_insn (insn);
	      if (insn == last_parm_insn)
		last_parm_insn = PREV_INSN (next);
	    }
	  else
	    fixup_var_refs_1 (var, PATTERN (insn), insn);
	  /* Also fix up any invalid exprs in the REG_NOTES of this insn.
	     But don't touch other insns referred to by reg-notes;
	     we will get them elsewhere.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (GET_CODE (note) != INSN_LIST)
	      XEXP (note, 0) = walk_fixup_memory_subreg (XEXP (note, 0), insn);
	}
      insn = next;
    }
}

static rtx
fixup_var_refs_1 (var, x, insn)
     register rtx var;
     register rtx x;
     rtx insn;
{
  register int i;
  RTX_CODE code = GET_CODE (x);
  register char *fmt;
  register rtx tem;

  switch (code)
    {
    case MEM:
      if (var == x)
	{
	  x = fixup_stack_1 (x, insn);
	  tem = gen_reg_rtx (GET_MODE (x));
	  /* Put new insn before a CALL, before any USEs before it.  */
	  if (GET_CODE (insn) == CALL_INSN)
	    while (PREV_INSN (insn) != 0 && GET_CODE (PREV_INSN (insn)) == INSN
		   && GET_CODE (PATTERN (PREV_INSN (insn))) == USE)
	      insn = PREV_INSN (insn);
	  emit_insn_before (gen_move_insn (tem, x), insn);
	  return tem;
	}
      break;

    case REG:
    case CC0:
    case PC:
    case CONST_INT:
    case CONST:
    case SYMBOL_REF:
    case LABEL_REF:
    case CONST_DOUBLE:
      return x;

    case SIGN_EXTRACT:
    case ZERO_EXTRACT:
      /* Note that in some cases those types of expressions are altered
	 by optimize_bit_field, and do not survive to get here.  */
    case SUBREG:
      tem = x;
      while (GET_CODE (tem) == SUBREG || GET_CODE (tem) == SIGN_EXTRACT
	     || GET_CODE (tem) == ZERO_EXTRACT)
	tem = XEXP (tem, 0);
      if (tem == var)
	{
	  x = fixup_stack_1 (x, insn);
	  tem = gen_reg_rtx (GET_MODE (x));
	  if (GET_CODE (x) == SUBREG)
	    x = fixup_memory_subreg (x, insn);
	  emit_insn_before (gen_move_insn (tem, x), insn);
	  return tem;
	}
      break;

    case SET:
      /* First do special simplification of bit-field references.  */
      if (GET_CODE (SET_DEST (x)) == SIGN_EXTRACT
	  || GET_CODE (SET_DEST (x)) == ZERO_EXTRACT)
	optimize_bit_field (x, insn, 0);
      if (GET_CODE (SET_SRC (x)) == SIGN_EXTRACT
	  || GET_CODE (SET_SRC (x)) == ZERO_EXTRACT)
	optimize_bit_field (x, insn, 0);

      {
	rtx dest = SET_DEST (x);
	rtx src = SET_SRC (x);
	rtx outerdest = dest;
	rtx outersrc = src;

	while (GET_CODE (dest) == SUBREG || GET_CODE (dest) == STRICT_LOW_PART
	       || GET_CODE (dest) == SIGN_EXTRACT
	       || GET_CODE (dest) == ZERO_EXTRACT)
	  dest = XEXP (dest, 0);
	while (GET_CODE (src) == SUBREG
	       || GET_CODE (src) == SIGN_EXTRACT
	       || GET_CODE (src) == ZERO_EXTRACT)
	  src = XEXP (src, 0);

	/* If VAR does not appear at the top level of the SET
	   just scan the lower levels of the tree.  */

        if (src != var && dest != var)
	  break;

	/* Clean up (SUBREG:SI (MEM:mode ...) 0)
	   that may appear inside a SIGN_EXTRACT or ZERO_EXTRACT.
	   This was legitimate when the MEM was a REG.  */

	if ((GET_CODE (outerdest) == SIGN_EXTRACT
	     || GET_CODE (outerdest) == ZERO_EXTRACT)
	    && GET_CODE (XEXP (outerdest, 0)) == SUBREG
	    && SUBREG_REG (XEXP (outerdest, 0)) == var)
	  XEXP (outerdest, 0) = fixup_memory_subreg (XEXP (outerdest, 0), insn);

	if ((GET_CODE (outersrc) == SIGN_EXTRACT
	     || GET_CODE (outersrc) == ZERO_EXTRACT)
	    && GET_CODE (XEXP (outersrc, 0)) == SUBREG
	    && SUBREG_REG (XEXP (outersrc, 0)) == var)
	  XEXP (outersrc, 0) = fixup_memory_subreg (XEXP (outersrc, 0), insn);

	/* Make sure that the machine's SIGN_EXTRACT and ZERO_EXTRACT insns
	   accept a memory operand.  */
#ifdef HAVE_extzv
	if (GET_CODE (outersrc) == ZERO_EXTRACT
	    && ! ((*insn_operand_predicate[(int) CODE_FOR_extzv][0])
		  (XEXP (outersrc, 0), VOIDmode)))
	  XEXP (outersrc, 0) = src
	    = fixup_var_refs_1 (var, XEXP (outersrc, 0), insn);
#endif
#ifdef HAVE_extv
	if (GET_CODE (outersrc) == SIGN_EXTRACT
	    && ! ((*insn_operand_predicate[(int) CODE_FOR_extv][0])
		  (XEXP (outersrc, 0), VOIDmode)))
	  XEXP (outersrc, 0) = src
	    = fixup_var_refs_1 (var, XEXP (outersrc, 0), insn);
#endif
#ifdef HAVE_insv
	if (GET_CODE (outerdest) == ZERO_EXTRACT
	    && ! ((*insn_operand_predicate[(int) CODE_FOR_insv][0])
		  (XEXP (outerdest, 0), VOIDmode)))
	  {
	    rtx tem = gen_reg_rtx (GET_MODE (XEXP (outerdest, 0)));

	    emit_insn_before (gen_move_insn (tem, XEXP (outerdest, 0)), insn);
	    emit_insn_after (gen_move_insn (XEXP (outerdest, 0), tem), insn);
	    dest = XEXP (outerdest, 0) = tem;
	  }
#endif

	/* Make sure a MEM inside a SIGN_EXTRACT has QImode
	   since that's what bit-field insns want.  */

	if ((GET_CODE (outerdest) == SIGN_EXTRACT
	     || GET_CODE (outerdest) == ZERO_EXTRACT)
	    && GET_CODE (XEXP (outerdest, 0)) == MEM
	    && GET_MODE (XEXP (outerdest, 0)) != QImode)
	  {
	    XEXP (outerdest, 0) = copy_rtx (XEXP (outerdest, 0));
	    PUT_MODE (XEXP (outerdest, 0), QImode);
	    /* Adjust the address so the bit field starts within the byte
	       addressed.  This helps certain optimization patterns.  */
	    if (GET_CODE (XEXP (outerdest, 2)) == CONST_INT
		&& offsettable_memref_p (XEXP (outerdest, 0)))
	      {
		int count = INTVAL (XEXP (outerdest, 2));
		XEXP (outerdest, 0)
		  = adj_offsettable_operand (XEXP (outerdest, 0),
					     count / GET_MODE_BITSIZE (QImode));
		XEXP (outerdest, 2)
		  = gen_rtx (CONST_INT, VOIDmode,
			     count % GET_MODE_BITSIZE (QImode));
	      }
	  }

	if ((GET_CODE (outersrc) == SIGN_EXTRACT
	     || GET_CODE (outersrc) == ZERO_EXTRACT)
	    && GET_CODE (XEXP (outersrc, 0)) == MEM
	    && GET_MODE (XEXP (outersrc, 0)) != QImode)
	  {
	    XEXP (outersrc, 0) = copy_rtx (XEXP (outersrc, 0));
	    PUT_MODE (XEXP (outersrc, 0), QImode);
	    /* Adjust the address so the bit field starts within the byte
	       addressed.  This helps certain optimization patterns.  */
	    if (GET_CODE (XEXP (outersrc, 2)) == CONST_INT
		&& offsettable_memref_p (XEXP (outersrc, 0)))
	      {
		int count = INTVAL (XEXP (outersrc, 2));
		XEXP (outersrc, 0)
		  = adj_offsettable_operand (XEXP (outersrc, 0),
					     count / GET_MODE_BITSIZE (QImode));
		XEXP (outersrc, 2)
		  = gen_rtx (CONST_INT, VOIDmode,
			     count % GET_MODE_BITSIZE (QImode));
	      }
	  }

	/* STRICT_LOW_PART is a no-op on memory references
	   and it can cause combinations to be unrecognizable,
	   so eliminate it.  */

	if (dest == var && GET_CODE (SET_DEST (x)) == STRICT_LOW_PART)
	  SET_DEST (x) = XEXP (SET_DEST (x), 0);

	/* An insn to copy VAR into or out of a register
	   must be left alone, to avoid an infinite loop here.
	   But do fix up the address of VAR's stack slot if nec,
	   and fix up SUBREGs containing VAR
	   (since they are now memory subregs).  */

	if (GET_CODE (SET_SRC (x)) == REG || GET_CODE (SET_DEST (x)) == REG
	    || (GET_CODE (SET_SRC (x)) == SUBREG
		&& GET_CODE (SUBREG_REG (SET_SRC (x))) == REG)
	    || (GET_CODE (SET_DEST (x)) == SUBREG
		&& GET_CODE (SUBREG_REG (SET_DEST (x))) == REG))
	  {
	    if (src == var && GET_CODE (SET_SRC (x)) == SUBREG)
	      SET_SRC (x) = fixup_memory_subreg (SET_SRC (x), insn);
	    if (dest == var && GET_CODE (SET_DEST (x)) == SUBREG)
	      SET_DEST (x) = fixup_memory_subreg (SET_DEST (x), insn);
	    return fixup_stack_1 (x, insn);
	  }

	/* Otherwise, storing into VAR must be handled specially
	   by storing into a temporary and copying that into VAR
	   with a new insn after this one.  */

	if (dest == var)
	  {
	    rtx temp;
	    rtx fixeddest;
	    tem = SET_DEST (x);
	    /* STRICT_LOW_PART can be discarded, around a MEM.  */
	    if (GET_CODE (tem) == STRICT_LOW_PART)
	      tem = XEXP (tem, 0);
	    /* Convert (SUBREG (MEM)) to a MEM in a changed mode.  */
	    if (GET_CODE (tem) == SUBREG)
	      tem = fixup_memory_subreg (tem, insn);
	    fixeddest = fixup_stack_1 (tem, insn);
	    temp = gen_reg_rtx (GET_MODE (tem));
	    emit_insn_after (gen_move_insn (fixeddest, temp), insn);
	    SET_DEST (x) = temp;
	  }
      }
    }

  /* Nothing special about this RTX; fix its operands.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = fixup_var_refs_1 (var, XEXP (x, i), insn);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j)
	      = fixup_var_refs_1 (var, XVECEXP (x, i, j), insn);
	}
    }
  return x;
}

/* Given X, an rtx of the form (SUBREG:m1 (MEM:m2 addr)),
   return an rtx (MEM:m1 newaddr) which is equivalent.
   If any insns must be emitted to compute NEWADDR, put them before INSN.  */

static rtx
fixup_memory_subreg (x, insn)
     rtx x;
     rtx insn;
{
  int offset = SUBREG_WORD (x) * UNITS_PER_WORD;
  rtx addr = XEXP (SUBREG_REG (x), 0);
  enum machine_mode mode = GET_MODE (x);
  rtx saved, result;

#ifdef BYTES_BIG_ENDIAN
  offset += (MIN (UNITS_PER_WORD, GET_MODE_SIZE (GET_MODE (SUBREG_REG (x))))
	     - MIN (UNITS_PER_WORD, GET_MODE_SIZE (mode)));
#endif
  addr = plus_constant (addr, offset);
  if (memory_address_p (mode, addr))
    return change_address (SUBREG_REG (x), mode, addr);
  saved = start_sequence ();
  result = change_address (SUBREG_REG (x), mode, addr);
  emit_insn_before (gen_sequence (), insn);
  end_sequence (saved);
  return result;
}

/* Do fixup_memory_subreg on all (SUBREG (MEM ...) ...) contained in X.
   Replace subexpressions of X in place.
   If X itself is a (SUBREG (MEM ...) ...), return the replacement expression.
   Otherwise return X, with its contents possibly altered.

   If any insns must be emitted to compute NEWADDR, put them before INSN.  */

static rtx
walk_fixup_memory_subreg (x, insn)
     register rtx x;
     rtx insn;
{
  register enum rtx_code code;
  register char *fmt;
  register int i;

  if (x == 0)
    return 0;

  code = GET_CODE (x);

  if (code == SUBREG && GET_CODE (SUBREG_REG (x)) == MEM)
    return fixup_memory_subreg (x, insn);

  /* Nothing special about this RTX; fix its operands.  */

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = walk_fixup_memory_subreg (XEXP (x, i), insn);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j)
	      = walk_fixup_memory_subreg (XVECEXP (x, i, j), insn);
	}
    }
  return x;
}

#if 0
/* Fix up any references to stack slots that are invalid memory addresses
   because they exceed the maximum range of a displacement.  */

void
fixup_stack_slots ()
{
  register rtx insn;

  /* Did we generate a stack slot that is out of range
     or otherwise has an invalid address?  */
  if (invalid_stack_slot)
    {
      /* Yes.  Must scan all insns for stack-refs that exceed the limit.  */
      for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
	if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	    || GET_CODE (insn) == JUMP_INSN)
	  fixup_stack_1 (PATTERN (insn), insn);
    }
}
#endif

/* For each memory ref within X, if it refers to a stack slot
   with an out of range displacement, put the address in a temp register
   (emitting new insns before INSN to load these registers)
   and alter the memory ref to use that register.
   Replace each such MEM rtx with a copy, to avoid clobberage.  */

static rtx
fixup_stack_1 (x, insn)
     rtx x;
     rtx insn;
{
  register int i;
  register RTX_CODE code = GET_CODE (x);
  register char *fmt;

  if (code == MEM)
    {
      register rtx ad = XEXP (x, 0);
      /* If we have address of a stack slot but it's not valid
	 (displacement is too large), compute the sum in a register.  */
      if (GET_CODE (ad) == PLUS
	  && XEXP (ad, 0) == frame_pointer_rtx
	  && GET_CODE (XEXP (ad, 1)) == CONST_INT)
	{
	  rtx temp;
	  if (memory_address_p (GET_MODE (x), ad))
	    return x;
	  temp = gen_reg_rtx (GET_MODE (ad));
	  emit_insn_before (gen_move_insn (temp, ad), insn);
	  return change_address (x, VOIDmode, temp);
	}
      return x;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = fixup_stack_1 (XEXP (x, i), insn);
      if (fmt[i] == 'E')
	{
	  register int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    XVECEXP (x, i, j) = fixup_stack_1 (XVECEXP (x, i, j), insn);
	}
    }
  return x;
}

/* Optimization: a bit-field instruction whose field
   happens to be a byte or halfword in memory
   can be changed to a move instruction.

   We call here when INSN is an insn to examine or store into a bit-field.
   BODY is the SET-rtx to be altered.

   EQUIV_MEM is the table `reg_equiv_mem' if that is available; else 0.
   (Currently this is called only from stmt.c, and EQUIV_MEM is always 0.)  */

static void
optimize_bit_field (body, insn, equiv_mem)
     rtx body;
     rtx insn;
     rtx *equiv_mem;
{
  register rtx bitfield;
  int destflag;

  if (GET_CODE (SET_DEST (body)) == SIGN_EXTRACT
      || GET_CODE (SET_DEST (body)) == ZERO_EXTRACT)
    bitfield = SET_DEST (body), destflag = 1;
  else
    bitfield = SET_SRC (body), destflag = 0;

  /* First check that the field being stored has constant size and position
     and is in fact a byte or halfword suitably aligned.  */

  if (GET_CODE (XEXP (bitfield, 1)) == CONST_INT
      && GET_CODE (XEXP (bitfield, 2)) == CONST_INT
      && (INTVAL (XEXP (bitfield, 1)) == GET_MODE_BITSIZE (QImode)
	  || INTVAL (XEXP (bitfield, 1)) == GET_MODE_BITSIZE (HImode))
      && INTVAL (XEXP (bitfield, 2)) % INTVAL (XEXP (bitfield, 1)) == 0)
    {
      register rtx memref = 0;

      /* Now check that the containing word is memory, not a register,
	 and that it is safe to change the machine mode and to
	 add something to the address.  */

      if (GET_CODE (XEXP (bitfield, 0)) == MEM)
	memref = XEXP (bitfield, 0);
      else if (GET_CODE (XEXP (bitfield, 0)) == REG
	       && equiv_mem != 0)
	memref = equiv_mem[REGNO (XEXP (bitfield, 0))];
      else if (GET_CODE (XEXP (bitfield, 0)) == SUBREG
	       && GET_CODE (SUBREG_REG (XEXP (bitfield, 0))) == MEM)
	memref = SUBREG_REG (XEXP (bitfield, 0));
      else if (GET_CODE (XEXP (bitfield, 0)) == SUBREG
	       && equiv_mem != 0
	       && GET_CODE (SUBREG_REG (XEXP (bitfield, 0))) == REG)
	memref = equiv_mem[REGNO (SUBREG_REG (XEXP (bitfield, 0)))];

      if (memref
	  && ! mode_dependent_address_p (XEXP (memref, 0))
	  && offsettable_address_p (0, GET_MODE (bitfield), XEXP (memref, 0)))
	{
	  /* Now adjust the address, first for any subreg'ing
	     that we are now getting rid of,
	     and then for which byte of the word is wanted.  */

	  register int offset
	    = INTVAL (XEXP (bitfield, 2)) / GET_MODE_BITSIZE (QImode);
	  if (GET_CODE (XEXP (bitfield, 0)) == SUBREG)
	    {
	      offset += SUBREG_WORD (XEXP (bitfield, 0)) * UNITS_PER_WORD;
#ifdef BYTES_BIG_ENDIAN
	      offset -= (MIN (UNITS_PER_WORD,
			      GET_MODE_SIZE (GET_MODE (XEXP (bitfield, 0))))
			 - MIN (UNITS_PER_WORD,
				GET_MODE_SIZE (GET_MODE (memref))));
#endif
	    }

	  memref = gen_rtx (MEM,
			    (INTVAL (XEXP (bitfield, 1)) == GET_MODE_BITSIZE (QImode)
			     ? QImode : HImode),
			    XEXP (memref, 0));

	  /* Store this memory reference where
	     we found the bit field reference.  */

	  if (destflag)
	    {
	      SET_DEST (body)
		= adj_offsettable_operand (memref, offset);
	      if (! CONSTANT_ADDRESS_P (SET_SRC (body)))
		{
		  rtx src = SET_SRC (body);
		  while (GET_CODE (src) == SUBREG
			 && SUBREG_WORD (src) == 0)
		    src = SUBREG_REG (src);
		  if (GET_MODE (src) != GET_MODE (memref))
		    src = gen_lowpart (GET_MODE (memref), SET_SRC (body));
		  SET_SRC (body) = src;
		}
	      else if (GET_MODE (SET_SRC (body)) != VOIDmode
		       && GET_MODE (SET_SRC (body)) != GET_MODE (memref))
		/* This shouldn't happen because anything that didn't have
		   one of these modes should have got converted explicitly
		   and then referenced through a subreg.
		   This is so because the original bit-field was
		   handled by agg_mode and so its tree structure had
		   the same mode that memref now has.  */
		abort ();
	    }
	  else
	    {
	      rtx dest = SET_DEST (body);

	      while (GET_CODE (dest) == SUBREG
		     && SUBREG_WORD (dest) == 0)
		dest = SUBREG_REG (dest);
	      SET_DEST (body) = dest;

	      memref = adj_offsettable_operand (memref, offset);
	      if (GET_MODE (dest) == GET_MODE (memref))
		SET_SRC (body) = memref;
	      else
		{
		  /* Convert the mem ref to the destination mode.  */
		  rtx last = get_last_insn ();
		  rtx newreg = gen_reg_rtx (GET_MODE (dest));
		  convert_move (newreg, memref,
				GET_CODE (SET_SRC (body)) == ZERO_EXTRACT);
		  /* Put the conversion before the insn being fixed.  */
		  reorder_insns (NEXT_INSN (last), get_last_insn (),
				 PREV_INSN (insn));
		  SET_SRC (body) = newreg;
		}
	    }

	  /* Cause the insn to be re-recognized.  */

	  INSN_CODE (insn) = -1;
	}
    }
}

/* 1 + last pseudo register number used for loading a copy
   of a parameter of this function.  */

static int max_parm_reg;

/* Vector indexed by REGNO, containing location on stack in which
   to put the parm which is nominally in pseudo register REGNO,
   if we discover that that parm must go in the stack.  */
static rtx *parm_reg_stack_loc;

int
max_parm_reg_num ()
{
  return max_parm_reg;
}

/* Return the first insn following those generated by `assign_parms'.  */

rtx
get_first_nonparm_insn ()
{
  if (last_parm_insn)
    return NEXT_INSN (last_parm_insn);
  return get_insns ();
}

/* Get the stack home of a REG rtx that is one of this function's parameters.
   This is called rather than assign a new stack slot as a local.
   Return 0 if there is no existing stack home suitable for such use.  */

static rtx
parm_stack_loc (reg)
     rtx reg;
{
  if (REGNO (reg) < max_parm_reg)
    return parm_reg_stack_loc[REGNO (reg)];
  return 0;
}

/* Return 1 if EXP returns an aggregate value, for which an address
   must be passed to the function or returned by the function.  */

int
aggregate_value_p (exp)
     tree exp;
{
  if (TYPE_MODE (TREE_TYPE (exp)) == BLKmode)
    return 1;
  if (RETURN_IN_MEMORY (TREE_TYPE (exp)))
    return 1;
  if (flag_pcc_struct_return
      && (TREE_CODE (TREE_TYPE (exp)) == RECORD_TYPE
	  || TREE_CODE (TREE_TYPE (exp)) == UNION_TYPE))
    return 1;
  return 0;
}

/* Convert a mem ref into one with a valid memory address.
   Pass through anything else unchanged.  */

rtx
validize_mem (ref)
     rtx ref;
{
  if (GET_CODE (ref) != MEM)
    return ref;
  if (memory_address_p (GET_MODE (ref), XEXP (ref, 0)))
    return ref;
  /* Don't alter REF itself, since that is probably a stack slot.  */
  return gen_rtx (MEM, GET_MODE (ref),
		  memory_address (GET_MODE (ref), XEXP (ref, 0)));
}

/* Assign RTL expressions to the function's parameters.
   This may involve copying them into registers and using
   those registers as the RTL for them.  */

static void
assign_parms (fndecl)
     tree fndecl;
{
  register tree parm;
  register rtx entry_parm;
  register rtx stack_parm;
  register CUMULATIVE_ARGS args_so_far;
  enum machine_mode passed_mode, nominal_mode;
  /* Total space needed so far for args on the stack,
     given as a constant and a tree-expression.  */
  struct args_size stack_args_size;
  int first_parm_offset = FIRST_PARM_OFFSET (fndecl);
  int first_parm_caller_offset
#ifdef FIRST_PARM_CALLER_OFFSET
    = FIRST_PARM_CALLER_OFFSET (fndecl);
#else
  = first_parm_offset;
#endif
  tree fntype = TREE_TYPE (fndecl);
  /* This is used for the arg pointer when referring to stack args.  */
  rtx internal_arg_pointer;

  int nparmregs
    = list_length (DECL_ARGUMENTS (fndecl)) + FIRST_PSEUDO_REGISTER;

  /* Nonzero if function takes extra anonymous args.
     This means the last named arg must be on the stack
     right before the anonymous ones.
     Also nonzero if the first arg is named `__builtin_va_alist',
     which is used on some machines for old-fashioned non-ANSI varargs.h;
     this too should be stuck onto the stack as if it had arrived there.  */
  int vararg
    = ((DECL_ARGUMENTS (fndecl) != 0
	&& DECL_NAME (DECL_ARGUMENTS (fndecl))
	&& (! strcmp (IDENTIFIER_POINTER (DECL_NAME (DECL_ARGUMENTS (fndecl))),
		      "__builtin_va_alist")))
       ||
       (TYPE_ARG_TYPES (fntype) != 0
	&& (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
	    != void_type_node)));
  int arg_pointer_copied = 0;

#if ARG_POINTER_REGNUM == FRAME_POINTER_REGNUM
  internal_arg_pointer = arg_pointer_rtx;
#else
  /* If the arg pointer reg is not a fixed reg,
     make a copy of it, and address parms via the copy.  */
  if (fixed_regs[ARG_POINTER_REGNUM])
    internal_arg_pointer = arg_pointer_rtx;
  else
    {
      internal_arg_pointer = copy_to_reg (arg_pointer_rtx);
      arg_pointer_copied = 1;
    }
#endif

  stack_args_size.constant = 0;
  stack_args_size.var = 0;

  /* If struct value address comes on the stack, count it in size of args.  */
  if (aggregate_value_p (DECL_RESULT (fndecl))
      && GET_CODE (struct_value_incoming_rtx) == MEM)
    {
#ifdef FIRST_PARM_CALLER_OFFSET
      /* Make the right thing happen on the sparc
	 in a function with a struct value and struct arg.  */
      if (first_parm_caller_offset < 0)
	first_parm_offset += GET_MODE_SIZE (Pmode);
      else
#endif
	stack_args_size.constant += GET_MODE_SIZE (Pmode);
    }

  parm_reg_stack_loc = (rtx *) oballoc (nparmregs * sizeof (rtx));
  bzero (parm_reg_stack_loc, nparmregs * sizeof (rtx));

  INIT_CUMULATIVE_ARGS (args_so_far, fntype);

  for (parm = DECL_ARGUMENTS (fndecl); parm; parm = TREE_CHAIN (parm))
    {
      int aggregate
	= (TREE_CODE (TREE_TYPE (parm)) == ARRAY_TYPE
	   || TREE_CODE (TREE_TYPE (parm)) == RECORD_TYPE
	   || TREE_CODE (TREE_TYPE (parm)) == UNION_TYPE);
      struct args_size stack_offset;
      rtx stack_offset_rtx;
      enum direction where_pad;

      DECL_OFFSET (parm) = -1;

      if (TREE_TYPE (parm) == error_mark_node
	  /* This can happen after weird syntax errors
	     or if an enum type is defined among the parms.  */
	  || TREE_CODE (parm) != PARM_DECL
	  || DECL_ARG_TYPE (parm) == NULL)
	{
	  DECL_RTL (parm) = gen_rtx (MEM, BLKmode, const0_rtx);
	  TREE_USED (parm) = 1;
	  continue;
	}

      /* Find mode of arg as it is passed, and mode of arg
	 as it should be during execution of this function.  */
      passed_mode = TYPE_MODE (DECL_ARG_TYPE (parm));
      nominal_mode = TYPE_MODE (TREE_TYPE (parm));

      /* Get this parm's offset as an rtx.  */
      stack_offset = stack_args_size;
      stack_offset.constant += first_parm_offset;

      /* If this argument needs more than the usual parm alignment, do
	 extrinsic padding to reach that alignment.  */

#ifdef MAX_PARM_BOUNDARY
      /* If MAX_PARM_BOUNDARY is not defined, it means that the usual
	 alignment requirements are relaxed for parms, and that no parm
	 needs more alignment than PARM_BOUNDARY, regardless of data type.  */

      if (PARM_BOUNDARY < TYPE_ALIGN (DECL_ARG_TYPE (parm)))
	{
	  int boundary = PARM_BOUNDARY;

	  /* Determine the boundary to pad up to.  */
	  if (TYPE_ALIGN (DECL_ARG_TYPE (parm)) > boundary)
	    boundary = TYPE_ALIGN (DECL_ARG_TYPE (parm));
	  if (boundary > MAX_PARM_BOUNDARY)
	    boundary = MAX_PARM_BOUNDARY;

	  /* If the previous args don't reach such a boundary,
	     advance to the next one.  */
	  boundary /= BITS_PER_UNIT;
	  stack_offset.constant += boundary - 1;
	  stack_offset.constant &= ~(boundary - 1);
	  stack_args_size.constant += boundary - 1;
	  stack_args_size.constant &= ~(boundary - 1);

	  if (stack_offset.var != 0)
	    abort ();		/* This case not implemented yet */
	}
#endif /* MAX_PARM_BOUNDARY */

      /* Find out if the parm needs intrinsic padding (up to PARM_BOUNDARY),
	 and whether above or below.  */

      where_pad
	= FUNCTION_ARG_PADDING (passed_mode,
				expand_expr (size_in_bytes (DECL_ARG_TYPE (parm)),
					     0, VOIDmode, 0));

      /* If arg should be padded below, adjust the stack address upward.
	 This padding is considered part of the space occupied by the
	 argument.  It pads only up to PARM_BOUNDARY, and it does not
	 depend on the previous arguments, since they are assumed to
	 occupy a multiple of PARM_BOUNDARY.  */

      if (where_pad == downward)
	{
	  if (passed_mode != BLKmode)
	    {
	      if (GET_MODE_BITSIZE (passed_mode) % PARM_BOUNDARY)
		stack_offset.constant
		  += (((GET_MODE_BITSIZE (passed_mode) + PARM_BOUNDARY - 1)
		       / PARM_BOUNDARY * PARM_BOUNDARY / BITS_PER_UNIT)
		      - GET_MODE_SIZE (passed_mode));
	    }
	  else
	    {
	      tree sizetree = size_in_bytes (DECL_ARG_TYPE (parm));
	      /* Round the size up to multiple of PARM_BOUNDARY bits.  */
	      tree s1 = convert_units (sizetree, BITS_PER_UNIT, PARM_BOUNDARY);
	      tree s2 = convert_units (s1, PARM_BOUNDARY, BITS_PER_UNIT);
	      /* Add it in.  */
	      ADD_PARM_SIZE (stack_offset, s2);
	      SUB_PARM_SIZE (stack_offset, sizetree);
	    }
	}

      /* Let machine desc say which reg (if any) the parm arrives in.
	 0 means it arrives on the stack.  */
      entry_parm = 0;
      /* Variable-size args, and args following such, are never in regs.  */
      if (TREE_CODE (TYPE_SIZE (TREE_TYPE (parm))) == INTEGER_CST
	  || stack_offset.var != 0)
	{
	  /* Set LAST_NAMED if this is last named arg before some
	     anonymous args.  We treat it as if it were anonymous too.  */
	  int last_named = (TREE_CHAIN (parm) == 0 && vararg);
#ifdef FUNCTION_INCOMING_ARG
	  entry_parm
	    = FUNCTION_INCOMING_ARG (args_so_far, passed_mode,
				     DECL_ARG_TYPE (parm), ! last_named);
#else
	  entry_parm
	    = FUNCTION_ARG (args_so_far, passed_mode, DECL_ARG_TYPE (parm),
			    ! last_named);
#endif
	}

#ifdef REG_PARM_STACK_SPACE
      /* If we arrive at a stack parm while still counting space for reg parms,
	 skip up to the offset for the first stack parm.  */
      if (entry_parm == 0
	  && stack_args_size.constant + first_parm_caller_offset < 0)
	{
	  int adjustment
	    = -(stack_args_size.constant + first_parm_caller_offset);
	  stack_args_size.constant += adjustment;
	  stack_offset.constant += adjustment;
	}
#endif

      stack_offset_rtx = ARGS_SIZE_RTX (stack_offset);

      /* Determine parm's home in the stack,
	 in case it arrives in the stack or we should pretend it did.  */
      /* Note that this is not necessarily a valid address.
	 We make it valid later when it is used.
	 It is necessary for the DECL_RTL to be an explicit stack slot,
	 but not necessary for it to be valid.  */
      stack_parm
	= gen_rtx (MEM, passed_mode,
		   gen_rtx (PLUS, Pmode,
			    internal_arg_pointer,
			    stack_offset_rtx));

      /* If this is a memory ref that contains aggregate components,
	 mark it as such for cse and loop optimize.  */
      MEM_IN_STRUCT_P (stack_parm) = aggregate;

      /* If this parm was passed part in regs and part in memory,
	 pretend it arrived entirely in memory
	 by pushing the register-part onto the stack.

	 In the special case of a DImode or DFmode that is split,
	 we could put it together in a pseudoreg directly,
	 but for now that's not worth bothering with.  */

      if (entry_parm)
	{
	  int nregs = 0;
	  int i;
#ifdef FUNCTION_ARG_PARTIAL_NREGS
	  nregs = FUNCTION_ARG_PARTIAL_NREGS (args_so_far, passed_mode,
					      DECL_ARG_TYPE (parm), 1);
#endif

#if 0 /* Replaced by new calling convention
	 which actually passes these args on the stack.  */
	  /* If this is the last named arg and anonymous args follow,
	     likewise pretend this arg arrived on the stack
	     so varargs can find the anonymous args following it.  */
	  if (TREE_CHAIN (parm) == 0 && vararg)
	    {
	      if (GET_MODE (entry_parm) == BLKmode)
		nregs = GET_MODE_SIZE (GET_MODE (entry_parm)) / UNITS_PER_WORD;
	      else
		nregs = (int_size_in_bytes (DECL_ARG_TYPE (parm))
			 / UNITS_PER_WORD);
	    }
#endif /* 0 */

	  if (nregs > 0)
	    {
	      rtx valid_stack_parm = validize_mem (stack_parm);
	      current_function_pretend_args_size
		= (((nregs * UNITS_PER_WORD) + (PARM_BOUNDARY / BITS_PER_UNIT) - 1)
		   / (PARM_BOUNDARY / BITS_PER_UNIT)
		   * (PARM_BOUNDARY / BITS_PER_UNIT));

	      i = nregs;
	      while (--i >= 0)
		emit_move_insn (gen_rtx (MEM, SImode,
					 plus_constant (XEXP (valid_stack_parm, 0),
							i * GET_MODE_SIZE (SImode))),
				gen_rtx (REG, SImode, REGNO (entry_parm) + i));
	      entry_parm = stack_parm;
	    }
	}

      /* If we didn't decide this parm came in a register,
	 by default it came on the stack.  */
      if (entry_parm == 0)
	entry_parm = stack_parm;

      /* For a stack parm, record in DECL_OFFSET the arglist offset
	 of the parm at the time it is passed (before conversion).  */
      if (entry_parm == stack_parm)
	DECL_OFFSET (parm) = stack_offset.constant * BITS_PER_UNIT;

      /* If there is actually space on the stack for this parm,
	 count it in stack_args_size; otherwise set stack_parm to 0
	 to indicate there is no preallocated stack slot for the parm.  */

      if (entry_parm == stack_parm
#ifdef REG_PARM_STACK_SPACE
	  /* On some machines, even if a parm value arrives in a register
	     there is still an (uninitialized) stack slot allocated for it.  */
	  || 1
#endif
	  )
	{
	  tree sizetree = size_in_bytes (DECL_ARG_TYPE (parm));
#ifdef PUSH_ROUNDING
	  /* If this arg will be pushed with a push instruction,
	     note how that will add to its size.  */
	  if (DECL_MODE (parm) != BLKmode)
	    {
	      int old_bytes = int_size_in_bytes (DECL_ARG_TYPE (parm));
	      sizetree = build_int_2 (PUSH_ROUNDING (old_bytes), 0);
	    }
#endif
	  if (where_pad != none)
	    {
	      /* Round the size up to multiple of PARM_BOUNDARY bits.  */
	      tree s1 = convert_units (sizetree, BITS_PER_UNIT, PARM_BOUNDARY);
	      sizetree = convert_units (s1, PARM_BOUNDARY, BITS_PER_UNIT);
	    }
	  /* Add it in.  */
	  ADD_PARM_SIZE (stack_args_size, sizetree);
	}
      else
	/* No stack slot was pushed for this parm.  */
	stack_parm = 0;

      /* Now adjust STACK_PARM to the mode and precise location
	 where this parameter should live during execution,
	 if we discover that it must live in the stack during execution.
	 To make debuggers happier on big-endian machines, we store
	 the value in the last bytes of the space available.  */

      if (nominal_mode != BLKmode && nominal_mode != passed_mode
	  && stack_parm != 0)
	{
#ifdef BYTES_BIG_ENDIAN
	  if (GET_MODE_SIZE (nominal_mode) < UNITS_PER_WORD)
	    {
	      stack_offset.constant
		+= GET_MODE_SIZE (passed_mode)
		  - GET_MODE_SIZE (nominal_mode);
	      stack_offset_rtx = ARGS_SIZE_RTX (stack_offset);
	    }
#endif

	  stack_parm
	    = gen_rtx (MEM, nominal_mode,
		       gen_rtx (PLUS, Pmode,
				arg_pointer_rtx,
				stack_offset_rtx));

	  /* If this is a memory ref that contains aggregate components,
	     mark it as such for cse and loop optimize.  */
	  MEM_IN_STRUCT_P (stack_parm) = aggregate;
	}

      /* ENTRY_PARM is an RTX for the parameter as it arrives,
	 in the mode in which it arrives.
	 STACK_PARM is an RTX for a stack slot where the parameter can live
	 during the function (in case we want to put it there).
	 STACK_PARM is 0 if no stack slot was pushed for it.

	 Now output code if necessary to convert ENTRY_PARM to
	 the type in which this function declares it,
	 and store that result in an appropriate place,
	 which may be a pseudo reg, may be STACK_PARM,
	 or may be a local stack slot if STACK_PARM is 0.

	 Set DECL_RTL to that place.  */

      if (nominal_mode == BLKmode)
	{
	  /* If a BLKmode arrives in registers, copy it to a stack slot.  */
	  if (GET_CODE (entry_parm) == REG)
	    {
#if 0 /* This was probably wrong, but save it just in case.  */
	      rtx unpadded_stack_parm;

	      /* Determine parm's home in the stack.  */

	      if (stack_parm == 0)
		unpadded_stack_parm
		  = assign_stack_local (GET_MODE (entry_parm),
					int_size_in_bytes (TREE_TYPE (parm)));
	      else
		unpadded_stack_parm
		  = gen_rtx (MEM, passed_mode,
			     memory_address (passed_mode,
					     gen_rtx (PLUS, Pmode,
						      internal_arg_pointer,
						      ARGS_SIZE_RTX (unpadded_stack_offset))));

	      /* Here we use unpadded_stack_parm because we assume
		 that downward padding is used on big-endian machines
		 where we would want to make the real data in the reg
		 (which is in the low bits) end up at the padded address.  */
#endif
	      if (stack_parm == 0)
		stack_parm
		  = assign_stack_local (GET_MODE (entry_parm),
					int_size_in_bytes (TREE_TYPE (parm)));

	      move_block_from_reg (REGNO (entry_parm),
				   validize_mem (stack_parm),
				   ((int_size_in_bytes (TREE_TYPE (parm))
				     + UNITS_PER_WORD - 1)
				    / UNITS_PER_WORD));
	    }
	  DECL_RTL (parm) = stack_parm;
	}
      else if (! ((obey_regdecls && ! TREE_REGDECL (parm)
		   && ! TREE_INLINE (fndecl))
		  /* layout_decl may set this.  */
		  || TREE_ADDRESSABLE (parm)
		  || TREE_VOLATILE (parm)
		  /* If -ffloat-store specified, don't put explicit
		     float variables into registers.  */
		  || (flag_float_store
		      && TREE_CODE (TREE_TYPE (parm)) == REAL_TYPE)))
	{
	  /* Store the parm in a pseudoregister during the function.  */
	  register rtx parmreg = gen_reg_rtx (nominal_mode);

	  REG_USERVAR_P (parmreg) = 1;
	  DECL_RTL (parm) = parmreg;

	  /* Copy the value into the register.  */
	  if (GET_MODE (parmreg) != GET_MODE (entry_parm))
	    convert_move (parmreg, validize_mem (entry_parm), 0);
	  else
	    emit_move_insn (parmreg, validize_mem (entry_parm));

	  /* In any case, record the parm's desired stack location
	     in case we later discover it must live in the stack.  */
	  if (REGNO (parmreg) >= nparmregs)
	    {
	      rtx *new;
	      nparmregs = REGNO (parmreg) + 5;
	      new = (rtx *) oballoc (nparmregs * sizeof (rtx));
	      bcopy (parm_reg_stack_loc, new, nparmregs * sizeof (rtx));
	      parm_reg_stack_loc = new;
	    }
	  parm_reg_stack_loc[REGNO (parmreg)] = stack_parm;

	  /* Mark the register as eliminable if we did no conversion
	     and it was copied from memory at a fixed offset,
	     and the arg pointer was not copied to a pseudo-reg.
	     If the arg pointer is a pseudo reg, such memory-equivalences
	     as we make here would screw up life analysis for it.  */
	  if (nominal_mode == passed_mode
	      && GET_CODE (entry_parm) == MEM
	      && stack_offset.var == 0
	      && ! arg_pointer_copied)
	    REG_NOTES (get_last_insn ())
	      = gen_rtx (EXPR_LIST, REG_EQUIV,
			 entry_parm, REG_NOTES (get_last_insn ()));

	  /* For pointer data type, suggest pointer register.  */
	  if (TREE_CODE (TREE_TYPE (parm)) == POINTER_TYPE)
	    mark_reg_pointer (parmreg);
	}
      else
	{
	  /* Value must be stored in the stack slot STACK_PARM
	     during function execution.  */

	  if (passed_mode != nominal_mode)
	    /* Conversion is required.  */
	    entry_parm = convert_to_mode (nominal_mode,
					  validize_mem (entry_parm), 0);

	  if (entry_parm != stack_parm)
	    {
	      if (stack_parm == 0)
		stack_parm = assign_stack_local (GET_MODE (entry_parm),
						 GET_MODE_SIZE (GET_MODE (entry_parm)));
	      emit_move_insn (validize_mem (stack_parm),
			      validize_mem (entry_parm));
	    }

	  DECL_RTL (parm) = stack_parm;
	  frame_pointer_needed = 1;
	}
      
      if (TREE_VOLATILE (parm))
	MEM_VOLATILE_P (DECL_RTL (parm)) = 1;
      if (TREE_READONLY (parm))
	RTX_UNCHANGING_P (DECL_RTL (parm)) = 1;

      /* Update info on where next arg arrives in registers.  */

      FUNCTION_ARG_ADVANCE (args_so_far, passed_mode, DECL_ARG_TYPE (parm), 1);
    }

  max_parm_reg = max_reg_num ();
  last_parm_insn = get_last_insn ();

  current_function_args_size = stack_args_size.constant;

  stack_args_size.constant += first_parm_offset;
  current_function_arg_offset_rtx = ARGS_SIZE_RTX (stack_args_size);
}

/* Allocation of space for returned structure values.
   During the rtl generation pass, `get_structure_value_addr'
   is called from time to time to request the address of a block in our
   stack frame in which called functions will store the structures
   they are returning.  The same space is used for all of these blocks.  

   We allocate these blocks like stack locals.  We keep reusing
   the same block until a bigger one is needed.  */

/* Length in bytes of largest structure value returned by
   any function called so far in this function.  */
static int max_structure_value_size;

/* An rtx for the addr we are currently using for structure values.
   This is typically (PLUS (REG:SI stackptr) (CONST_INT...)).  */
static rtx structure_value;

rtx
get_structure_value_addr (sizex)
     rtx sizex;
{
  register int size;
  if (GET_CODE (sizex) != CONST_INT)
    abort ();
  size = INTVAL (sizex);

  /* Round up to a multiple of the main allocation unit.  */
  size = (((size + (BIGGEST_ALIGNMENT / BITS_PER_UNIT) - 1)
	   / (BIGGEST_ALIGNMENT / BITS_PER_UNIT))
	  * (BIGGEST_ALIGNMENT / BITS_PER_UNIT));

  /* If this size is bigger than space we know to use,
     get a bigger piece of space.  */
  if (size > max_structure_value_size)
    {
      max_structure_value_size = size;
      structure_value = assign_stack_local (BLKmode, size);
      if (GET_CODE (structure_value) == MEM)
	structure_value = XEXP (structure_value, 0);
    }

  return structure_value;
}

/* Push and pop the current structure value block.  */

void
push_structure_value (rtx_ptr, size_ptr)
     rtx *rtx_ptr;
     int *size_ptr;
{
  *rtx_ptr = structure_value;
  *size_ptr = max_structure_value_size;
  max_structure_value_size = 0;
  structure_value = 0;
}

void
pop_structure_value (rtx_value, size)
     rtx rtx_value;
     int size;
{
  structure_value = rtx_value;
  max_structure_value_size = size;
}


/* Walk the tree of LET_STMTs describing the binding levels within a function
   and warn about uninitialized variables.
   This is done after calling flow_analysis and before global_alloc
   clobbers the pseudo-regs to hard regs.  */

void
uninitialized_vars_warning (block)
     tree block;
{
  register tree decl, sub;
  for (decl = STMT_VARS (block); decl; decl = TREE_CHAIN (decl))
    {
      if (TREE_CODE (decl) == VAR_DECL
	  /* These warnings are unreliable for and aggregates
	     because assigning the fields one by one can fail to convince
	     flow.c that the entire aggregate was initialized.
	     Unions are troublesome because members may be shorter.  */
	  && TREE_CODE (TREE_TYPE (decl)) != RECORD_TYPE
	  && TREE_CODE (TREE_TYPE (decl)) != UNION_TYPE
	  && TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE
	  && DECL_RTL (decl) != 0
	  && GET_CODE (DECL_RTL (decl)) == REG
	  && regno_uninitialized (REGNO (DECL_RTL (decl))))
	warning_with_decl (decl,
			   "`%s' may be used uninitialized in this function");
      if (TREE_CODE (decl) == VAR_DECL
	  && DECL_RTL (decl) != 0
	  && GET_CODE (DECL_RTL (decl)) == REG
	  && regno_clobbered_at_setjmp (REGNO (DECL_RTL (decl))))
	warning_with_decl (decl,
			   "variable `%s' may be clobbered by `longjmp'");
    }
  for (sub = STMT_SUBBLOCKS (block); sub; sub = TREE_CHAIN (sub))
    uninitialized_vars_warning (sub);
}

/* If this function call setjmp, put all vars into the stack
   unless they were declared `register'.  */

void
setjmp_protect (block)
     tree block;
{
  register tree decl, sub;
  for (decl = STMT_VARS (block); decl; decl = TREE_CHAIN (decl))
    if ((TREE_CODE (decl) == VAR_DECL
	 || TREE_CODE (decl) == PARM_DECL)
	&& DECL_RTL (decl) != 0
	&& GET_CODE (DECL_RTL (decl)) == REG
	&& ! TREE_REGDECL (decl))
      put_var_into_stack (decl);
  for (sub = STMT_SUBBLOCKS (block); sub; sub = TREE_CHAIN (sub))
    setjmp_protect (sub);
}

/* Generate RTL for the start of the function SUBR (a FUNCTION_DECL tree node)
   and initialize static variables for generating RTL for the statements
   of the function.  */

void
init_function_start (subr, filename, line)
     tree subr;
     char *filename;
     int line;
{
  this_function = subr;
  cse_not_expected = ! optimize;

  /* We have not yet found a reason why a frame pointer cannot
     be omitted for this function in particular, but maybe we know
     a priori that it is required.
     `flag_omit_frame_pointer' has its main effect here.  */
  frame_pointer_needed = FRAME_POINTER_REQUIRED || ! flag_omit_frame_pointer;

  /* Caller save not needed yet.  */
  caller_save_needed = 0;

  /* No gotos have been expanded yet.  */
  goto_fixup_chain = 0;

  /* No stack slots have been made yet.  */
  stack_slot_list = 0;

  /* No invalid stack slots have been made yet.  */
  invalid_stack_slot = 0;

  /* No parm regs have been allocated.
     (This is important for output_inline_function.)  */
  max_parm_reg = FIRST_PSEUDO_REGISTER;

  /* Initialize the RTL mechanism.  */
  init_emit (write_symbols);

  /* Initialize the queue of pending postincrement and postdecrements,
     and some other info in expr.c.  */
  init_expr ();

  init_const_rtx_hash_table ();

  /* Decide whether function should try to pop its args on return.  */

  current_function_pops_args = RETURN_POPS_ARGS (TREE_TYPE (subr));

  current_function_name = DECL_PRINT_NAME (subr);

  /* Nonzero if this is a nested function that uses a static chain.  */

  current_function_needs_context
    = (DECL_CONTEXT (current_function_decl) != 0
       && TREE_CODE (DECL_CONTEXT (current_function_decl)) == LET_STMT);

  /* Set if a call to setjmp is seen.  */

  current_function_calls_setjmp = 0;
  current_function_calls_alloca = 0;

  current_function_returns_pcc_struct = 0;
  current_function_returns_struct = 0;

  /* No space assigned yet for structure values.  */
  max_structure_value_size = -1;
  structure_value = 0;

  /* We are not currently within any block, conditional, loop or case.  */
  block_stack = 0;
  loop_stack = 0;
  case_stack = 0;
  cond_stack = 0;
  nesting_stack = 0;
  nesting_depth = 0;

  block_start_count = 0;

  /* We have not yet needed to make a label to jump to for tail-recursion.  */
  tail_recursion_label = 0;

  /* No stack slots allocated yet.  */
  frame_offset = STARTING_FRAME_OFFSET;

  /* No SAVE_EXPRs in this function yet.  */
  save_expr_regs = 0;

  /* No RTL_EXPRs in this function yet.  */
  rtl_expr_chain = 0;

  /* Within function body, compute a type's size as soon it is laid out.  */
  immediate_size_expand++;

  init_pending_stack_adjust ();
  inhibit_defer_pop = 0;
  current_function_pretend_args_size = 0;

  /* Prevent ever trying to delete the first instruction of a function.
     Also tell final how to output a linenum before the function prologue.  */
  emit_line_note (filename, line);
  /* Make sure first insn is a note even if we don't want linenums.
     This makes sure the first insn will never be deleted.
     Also, final expects a note to appear there.  */
  emit_note (0, NOTE_INSN_DELETED);
  /* Indicate the beginning of the function body,
     as opposed to parm setup.  */
  emit_note (0, NOTE_INSN_FUNCTION_BEG);

  /* Set flags used by final.c.  */
  if (aggregate_value_p (DECL_RESULT (subr)))
    {
#ifdef PCC_STATIC_STRUCT_RETURN
      if (flag_pcc_struct_return)
	current_function_returns_pcc_struct = 1;
      else
#endif
	current_function_returns_struct = 1;
    }
}

/* Start the RTL for a new function, and set variables used for
   emitting RTL.
   SUBR is the FUNCTION_DECL node.
   PARMS_HAVE_CLEANUPS is nonzero if there are cleanups associated with
   the function's parameters, which must be run at any return statement.  */

void
expand_function_start (subr, parms_have_cleanups)
     tree subr;
     int parms_have_cleanups;
{
  register int i;
  tree tem;

  /* Make sure volatile mem refs aren't considered
     valid operands of arithmetic insns.  */
  init_recog ();

  /* If the parameters of this function need cleaning up, get a label
     for the beginning of the code which executes those cleanups.  This must
     be done before doing anything with return_label.  */
  if (parms_have_cleanups)
    cleanup_label = gen_label_rtx ();
  else
    cleanup_label = 0;

  /* Make the label for return statements to jump to, if this machine
     does not have a one-instruction return and uses an epilogue,
     or if it returns a structure, or if it has parm cleanups.  */
#ifdef HAVE_return
  if (cleanup_label == 0 && HAVE_return
      && ! current_function_returns_pcc_struct
      && ! (current_function_returns_struct && ! optimize))
    return_label = 0;
  else
    return_label = gen_label_rtx ();
#else
  return_label = gen_label_rtx ();
#endif

  /* Initialize rtx used to return the value.  */
  /* Do this before assign_parms so that we copy the struct value address
     before any library calls that assign parms might generate.  */

  /* Decide whether to return the value in memory or in a register.  */
  if (aggregate_value_p (DECL_RESULT (subr)))
    {
      /* Returning something that won't go in a register.  */
      register rtx value_address;

#ifdef PCC_STATIC_STRUCT_RETURN
      if (flag_pcc_struct_return)
	{
	  int size = int_size_in_bytes (TREE_TYPE (DECL_RESULT (subr)));
	  value_address = assemble_static_space (size);
	  current_function_returns_pcc_struct = 1;
	}
      else
#endif
	{
	  /* Expect to be passed the address of a place to store the value.  */
	  value_address = gen_reg_rtx (Pmode);
	  emit_move_insn (value_address, struct_value_incoming_rtx);
	  current_function_returns_struct = 1;
	}
      DECL_RTL (DECL_RESULT (subr))
	= gen_rtx (MEM, DECL_MODE (DECL_RESULT (subr)),
		   value_address);
    }
  else if (DECL_MODE (DECL_RESULT (subr)) == VOIDmode)
    /* If return mode is void, this decl rtl should not be used.  */
    DECL_RTL (DECL_RESULT (subr)) = 0;
  else if (parms_have_cleanups)
    /* If function will end with cleanup code for parms,
       compute the return values into a pseudo reg,
       which we will copy into the true return register
       after the cleanups are done.  */
    DECL_RTL (DECL_RESULT (subr))
      = gen_reg_rtx (DECL_MODE (DECL_RESULT (subr)));
  else
    /* Scalar, returned in a register.  */
    {
#ifdef FUNCTION_OUTGOING_VALUE
      DECL_RTL (DECL_RESULT (subr))
	= FUNCTION_OUTGOING_VALUE (TREE_TYPE (DECL_RESULT (subr)), subr);
#else
      DECL_RTL (DECL_RESULT (subr))
	= FUNCTION_VALUE (TREE_TYPE (DECL_RESULT (subr)), subr);
#endif

      current_function_returns_pointer 
	= (TREE_CODE (DECL_RESULT_TYPE (subr)) == POINTER_TYPE);

      /* Mark this reg as the function's return value.  */
      if (GET_CODE (DECL_RTL (DECL_RESULT (subr))) == REG)
	REG_FUNCTION_VALUE_P (DECL_RTL (DECL_RESULT (subr))) = 1;
    }

  /* Initialize rtx for parameters and local variables.
     In some cases this requires emitting insns.  */

  assign_parms (subr);

  /* If doing stupid allocation, mark parms as born here.  */

  if (GET_CODE (get_last_insn ()) != NOTE)
    emit_note (0, NOTE_INSN_DELETED);
  parm_birth_insn = get_last_insn ();

  if (obey_regdecls)
    {
      for (i = FIRST_PSEUDO_REGISTER; i < max_parm_reg; i++)
	use_variable (regno_reg_rtx[i]);
    }

  /* After the parm initializations is where the tail-recursion label
     should go, if we end up needing one.  */
  tail_recursion_reentry = get_last_insn ();

  /* Evaluate now the sizes of any types declared among the arguments.  */
  for (tem = get_pending_sizes (); tem; tem = TREE_CHAIN (tem))
    expand_expr (TREE_VALUE (tem), 0, VOIDmode, 0);

  /* Make sure there is a line number after the function entry setup code.
     There normally is one anyway, from the following statement,
     but there could fail to be one if there is no newline here.  */
  force_next_line_note ();
}

/* Generate RTL for the end of the current function.
   FILENAME and LINE are the current position in the source file.  */

/* ??? Nobody seems to emit the cleanup_label and the cleanups themselves.  */

void
expand_function_end (filename, line)
     char *filename;
     int line;
{
  register int i;
  rtx decl;
  extern rtx sequence_stack;

#if 0  /* I think unused parms are legitimate enough.  */
  /* Warn about unused parms.  */
  if (warn_unused)
    for (decl = DECL_ARGUMENTS (current_function_decl);
	 decl; decl = TREE_CHAIN (decl))
      if (! TREE_USED (decl) && TREE_CODE (decl) == VAR_DECL)
	warning_with_decl (decl, "unused parameter `%s'");
#endif

  /* End any sequences that failed to be closed due to syntax errors.  */
  while (sequence_stack)
    end_sequence (0);

  /* Outside function body, can't compute type's actual size
     until next function's body starts.  */
  immediate_size_expand--;

  /* If doing stupid register allocation,
     mark register parms as dying here.  */

  if (obey_regdecls)
    {
      rtx tem;
      for (i = FIRST_PSEUDO_REGISTER; i < max_parm_reg; i++)
	use_variable (regno_reg_rtx[i]);

      /* Likewise for the regs of all the SAVE_EXPRs in the function.  */

      for (tem = save_expr_regs; tem; tem = XEXP (tem, 1))
	{
	  /* ??? Tiemann thinks this does not work.  */
	  use_variable (XEXP (tem, 0));
	  use_variable_after (XEXP (tem, 0), parm_birth_insn);
	}
    }

  clear_pending_stack_adjust ();
  do_pending_stack_adjust ();

  /* Mark the end of the function body.
     If control reaches this insn, the function can drop through
     without returning a value.  */
  emit_note (0, NOTE_INSN_FUNCTION_END);

  /* Output a linenumber for the end of the function.
     SDB depends on this.  */
  emit_line_note_force (filename, line);

  /* Output the label for the actual return from the function,
     if one is expected.  This happens either because a function epilogue
     is used instead of a return instruction, or because a return was done
     with a goto in order to run local cleanups, or because of pcc-style
     structure returning.  */

  if (return_label)
    emit_label (return_label);

  /* If we had calls to alloca, and this machine needs
     an accurate stack pointer to exit the function,
     insert some code to save and restore the stack pointer.  */
#ifdef EXIT_IGNORE_STACK
  if (! EXIT_IGNORE_STACK)
#endif
    if (current_function_calls_alloca)
      {
	rtx tem = gen_reg_rtx (Pmode);
	emit_insn_after (gen_rtx (SET, VOIDmode, tem, stack_pointer_rtx),
			 parm_birth_insn);
	emit_insn (gen_rtx (SET, VOIDmode, stack_pointer_rtx, tem));
      }

  /* If scalar return value was computed in a pseudo-reg,
     copy that to the hard return register.  */
  if (DECL_RTL (DECL_RESULT (current_function_decl)) != 0
      && GET_CODE (DECL_RTL (DECL_RESULT (current_function_decl))) == REG
      && (REGNO (DECL_RTL (DECL_RESULT (current_function_decl)))
	  >= FIRST_PSEUDO_REGISTER))
    {
      rtx real_decl_result;

#ifdef FUNCTION_OUTGOING_VALUE
      real_decl_result
	= FUNCTION_OUTGOING_VALUE (TREE_TYPE (DECL_RESULT (current_function_decl)),
				   current_function_decl);
#else
      real_decl_result
	= FUNCTION_VALUE (TREE_TYPE (DECL_RESULT (current_function_decl)),
			  current_function_decl);
#endif
      REG_FUNCTION_VALUE_P (real_decl_result) = 1;
      emit_move_insn (real_decl_result,
		      DECL_RTL (DECL_RESULT (current_function_decl)));
      emit_insn (gen_rtx (USE, VOIDmode, real_decl_result));
    }

  /* If returning a structure, arrange to return the address of the value
     in a place where debuggers expect to find it.  */
  /* If returning a structure PCC style,
     the caller also depends on this value.
     And current_function_returns_pcc_struct is not necessarily set.  */
  if (current_function_returns_struct
      || current_function_returns_pcc_struct)
    {
      rtx value_address = XEXP (DECL_RTL (DECL_RESULT (current_function_decl)), 0);
      tree type = TREE_TYPE (DECL_RESULT (current_function_decl));
#ifdef FUNCTION_OUTGOING_VALUE
      rtx outgoing
	= FUNCTION_OUTGOING_VALUE (build_pointer_type (type),
				   current_function_decl);
#else
      rtx outgoing
	= hard_function_value (build_pointer_type (type),
			       current_function_decl);
#endif

      REG_FUNCTION_VALUE_P (outgoing) = 1;
      emit_move_insn (outgoing, value_address);
      use_variable (outgoing);
    }

  /* Output a return insn if we are using one.
     Otherwise, let the rtl chain end here, to drop through
     into the epilogue.  */

#ifdef HAVE_return
  if (HAVE_return)
    emit_jump_insn (gen_return ());
#endif

  /* Fix up any gotos that jumped out to the outermost
     binding level of the function.
     Must follow emitting RETURN_LABEL.  */

  /* If you have any cleanups to do at this point,
     and they need to create temporary variables,
     then you will lose.  */
  fixup_gotos (0, 0, 0, get_insns (), 0);
}
