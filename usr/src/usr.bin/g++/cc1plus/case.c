#ifndef FIRST_PSEUDO_REGISTER
#define NULL 0
#include "config.h"
#include "rtl.h"
#include "tree.h"
#include "insn-flags.h"
#endif

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

void balance_case_nodes ();
void emit_case_nodes ();
void group_case_nodes ();
void emit_jump_if_reachable ();

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

/* Not all case values are encountered equally.  This function
   uses a heuristic to weight case labels, in cases where that
   looks like a reasonable thing to do.

   Right now, all we try to guess is text, and we establish the
   following weights:

	chars above space:	16
	digits:			16
	default:		12
	space, punct:		8
	tab:			4
	newline:		2
	other "\" chars:	1
	remaining chars:	0

   Under this weighting, ranges are automagically taken care of.  */

#include <ctype.h>
static char *cost_table;
static int use_cost_table;

void
estimate_case_costs (node, default_label)
     case_node_ptr node;
     rtx default_label;
{
  tree min_ascii = build_int_2 (-1, -1);
  tree max_ascii = convert (TREE_TYPE (node->high), build_int_2 (127, 0));
  case_node_ptr n;
  use_cost_table = 0;

  /* If the user is running without default, who are we
     to guess where values are likely to land?  */
  if (default_label == 0)
    return;

  /* See if all the case expressions look like text.  It is text if the lowest
     constant is >= -1 and the highest constant is <= 127.  If the case
     expression is unsigned, suppress the test for >= -1, since it would always
     be true.  */
  for (n = node; n; n = n->right)
    if ((! TREE_UNSIGNED (TREE_TYPE (n->low))
	 && tree_int_cst_lt (n->low, min_ascii))
	|| tree_int_cst_lt (max_ascii, n->high))
	return;

  /* All interesting values with within the range of
     interesting ASCII characters.  */
  if (cost_table == NULL)
    {
      int i;
      cost_table = ((char *)malloc (129)) + 1;
      bzero (cost_table-1, 128);
      for (i = 0; i < 128; i++)
	{
	  if (isalnum (i))
	    cost_table[i] = 16;
	  else if (ispunct (i))
	    cost_table[i] = 8;
	  else if (iscntrl (i))
	    cost_table[i] = -1;
	}
      cost_table[' '] = 8;
      cost_table['\t'] = 4;
      cost_table['\0'] = 4;
      cost_table['\n'] = 2;
      cost_table['\f'] = 1;
      cost_table['\v'] = 1;
      cost_table['\b'] = 1;
    }
  use_cost_table = 1;
}

/* Scan an ordered list of case nodes
   combining those with consecutive values or ranges.

   Eg. three separate entries 1: 2: 3: become one entry 1..3:  */

void
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
					     integer_one_node)))
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

void
balance_case_nodes (head, parent)
     case_node_ptr *head;
     case_node_ptr parent;
{
  register case_node_ptr np;

  np = *head;
  if (np)
    {
      int cost = 0;
      int i = 0;
      int ranges = 0;
      register case_node_ptr *npp;
      case_node_ptr left;

      /* Count the number of entries on branch.
	 Also count the ranges.  */
      while (np)
	{
	  if (!tree_int_cst_equal (np->low, np->high))
	    {
	      ranges++;
	      if (use_cost_table)
		{
		  int hi_cost = cost_table[TREE_INT_CST_LOW (np->high)];
		  if (hi_cost < 0)
		    use_cost_table = 0;
		  else
		    cost += hi_cost;
		}
	    }
	  if (use_cost_table)
	    {
	      int lo_cost = cost_table[TREE_INT_CST_LOW (np->low)];
	      if (lo_cost < 0)
		use_cost_table = 0;
	      else
		cost += lo_cost;
	    }
	  else
	    cost += 1;
	  i++;
	  np = np->right;
	}
      if (i > 2)
	{
	  /* Split this list if it is long enough for that to help.  */
	  npp = head;
	  left = *npp;
	  if (use_cost_table)
	    {
	      /* Find the place in the list that bisects the list's total cost,
		 Here I gets half the total cost.  */
	      int n_moved = 0;
	      i = (cost + 1) / 2;
	      while (1)
		{
		  /* Skip nodes while their cost does not reach that amount.  */
		  if (!tree_int_cst_equal ((*npp)->low, (*npp)->high))
		    i -= cost_table[TREE_INT_CST_LOW ((*npp)->high)];
		  i -= cost_table[TREE_INT_CST_LOW ((*npp)->low)];
		  if (i <= 0)
		    break;
		  npp = &(*npp)->right;
		  n_moved += 1;
		}
	      if (n_moved == 0)
		{
		  /* Leave this branch lopsided, but optimize left-hand
		     side and fill in `parent' fields for right-hand side.  */
		  np = *head;
		  np->parent = parent;
		  balance_case_nodes (&np->left, np);
		  for (; np->right; np = np->right)
		    np->right->parent = np;
		  return;
		}
	    }
	  /* If there are just three nodes, split at the middle one.  */
	  else if (i == 3)
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
      low_minus_one = combine (MINUS_EXPR, node->low, integer_one_node);
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
      high_plus_one = combine (PLUS_EXPR, node->high, integer_one_node);
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

void
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

void
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
  int defaulted_left = 0;
  int defaulted_right = 0;

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
	  if (use_cost_table
	      && ! defaulted_right
	      && cost_table[TREE_INT_CST_LOW (node->high)] < 12)
	    {
	      /* If our "most probably entry" is less probable
		 than the default label, emit a jump to
		 the default label using condition codes
		 already lying around.  With no right branch,
		 a branch-greater-than will get us to the default
		 label correctly.  */
	      emit_cmp_insn (index, expand_expr (node->high, 0, VOIDmode, 0), 0, unsignedp, 0);
	      emit_jump_insn ((*gen_bgt_pat) (default_label));
	      /* No sense doing this too often.  */
	      defaulted_right = 1;
	    }
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

