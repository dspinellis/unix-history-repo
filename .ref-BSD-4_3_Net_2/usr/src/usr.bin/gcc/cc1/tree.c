/* Language-indepednent node constructors for parse phase of GNU compiler.
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


/* This file contains the low level primitives for operating on tree nodes,
   including allocation, list operations, interning of identifiers,
   construction of data type nodes and statement nodes,
   and construction of type conversion nodes.  It also contains
   tables index by tree code that describe how to take apart
   nodes of that code.

   It is intended to be language-independent, but occasionally
   calls language-dependent routines defined (for C) in typecheck.c.

   The low-level allocation routines oballoc and permalloc
   are used also for allocating many other kinds of objects
   by all passes of the compiler.  */

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "obstack.h"
#include "gvarargs.h"
#include "flags.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int xmalloc ();
extern void free ();

/* Tree nodes of permanent duration are allocated in this obstack.
   They are the identifier nodes, and everything outside of
   the bodies and parameters of function definitions.  */

struct obstack permanent_obstack;

/* The initial RTL, and all ..._TYPE nodes, in a function
   are allocated in this obstack.  Usually they are freed at the
   end of the function, but if the function is inline they are saved.  */

struct obstack maybepermanent_obstack;

/* The contents of the current function definition are allocated
   in this obstack, and all are freed at the end of the function.  */

struct obstack temporary_obstack;

/* The tree nodes of an expression are allocated
   in this obstack, and all are freed at the end of the expression.  */

struct obstack momentary_obstack;

/* This points at either permanent_obstack or maybepermanent_obstack.  */

struct obstack *saveable_obstack;

/* This is same as saveable_obstack during parse and expansion phase;
   it points to temporary_obstack during optimization.
   This is the obstack to be used for creating rtl objects.  */

struct obstack *rtl_obstack;

/* This points at either permanent_obstack or temporary_obstack.  */

struct obstack *current_obstack;

/* This points at either permanent_obstack or temporary_obstack
   or momentary_obstack.  */

struct obstack *expression_obstack;

/* Addresses of first objects in some obstacks.
   This is for freeing their entire contents.  */
char *maybepermanent_firstobj;
char *temporary_firstobj;
char *momentary_firstobj;

/* Nonzero means all ..._TYPE nodes should be allocated permanently.  */

int all_types_permanent;

/* Stack of places to restore the momentary obstack back to.  */
   
struct momentary_level
{
  /* Pointer back to previous such level.  */
  struct momentary_level *prev;
  /* First object allocated within this level.  */
  char *base;
  /* Value of expression_obstack saved at entry to this level.  */
  struct obstack *obstack;
};

struct momentary_level *momentary_stack;

/* Table indexed by tree code giving a string containing a character
   classifying the tree code.  Possibilities are
   t, d, s, c, r and e.  See tree.def for details.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) TYPE,

char *tree_code_type[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Table indexed by tree code giving number of expression
   operands beyond the fixed part of the node structure.
   Not used for types or decls.  */

#define DEFTREECODE(SYM, NAME, TYPE, LENGTH) LENGTH,

int tree_code_length[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Counter for assigning unique ids to all tree nodes.  */

int tree_node_counter = 0;

/* Hash table for uniquizing IDENTIFIER_NODEs by name.  */

#define MAX_HASH_TABLE 1009
static tree hash_table[MAX_HASH_TABLE];	/* id hash buckets */

/* 0 while creating built-in identifiers.  */
static int do_identifier_warnings;

/* Init data for node creation, at the beginning of compilation.  */

void
init_tree ()
{
  obstack_init (&permanent_obstack);

  obstack_init (&temporary_obstack);
  temporary_firstobj = (char *) obstack_alloc (&temporary_obstack, 0);
  obstack_init (&momentary_obstack);
  momentary_firstobj = (char *) obstack_alloc (&momentary_obstack, 0);
  obstack_init (&maybepermanent_obstack);
  maybepermanent_firstobj
    = (char *) obstack_alloc (&maybepermanent_obstack, 0);

  current_obstack = &permanent_obstack;
  expression_obstack = &permanent_obstack;
  rtl_obstack = saveable_obstack = &permanent_obstack;
  tree_node_counter = 1;
  bzero (hash_table, sizeof hash_table);
}

/* Start allocating on the temporary (per function) obstack.
   This is done in start_function before parsing the function body,
   and before each initialization at top level, and to go back
   to temporary allocation after doing end_temporary_allocation.  */

void
temporary_allocation ()
{
  current_obstack = &temporary_obstack;
  expression_obstack = &temporary_obstack;
  rtl_obstack = saveable_obstack = &maybepermanent_obstack;
  momentary_stack = 0;
}

/* Start allocating on the permanent obstack but don't
   free the temporary data.  After calling this, call
   `permanent_allocation' to fully resume permanent allocation status.  */

void
end_temporary_allocation ()
{
  current_obstack = &permanent_obstack;
  expression_obstack = &permanent_obstack;
  rtl_obstack = saveable_obstack = &permanent_obstack;
}

/* Resume allocating on the temporary obstack, undoing
   effects of `end_temporary_allocation'.  */

void
resume_temporary_allocation ()
{
  current_obstack = &temporary_obstack;
  expression_obstack = &temporary_obstack;
  rtl_obstack = saveable_obstack = &maybepermanent_obstack;
}

/* Nonzero if temporary allocation is currently in effect.
   Zero if currently doing permanent allocation.  */

int
allocation_temporary_p ()
{
  return current_obstack == &temporary_obstack;
}

/* Go back to allocating on the permanent obstack
   and free everything in the temporary obstack.
   This is done in finish_function after fully compiling a function.  */

void
permanent_allocation ()
{
  /* Free up previous temporary obstack data */
  obstack_free (&temporary_obstack, temporary_firstobj);
  obstack_free (&momentary_obstack, momentary_firstobj);
  obstack_free (&maybepermanent_obstack, maybepermanent_firstobj);

  current_obstack = &permanent_obstack;
  expression_obstack = &permanent_obstack;
  rtl_obstack = saveable_obstack = &permanent_obstack;
}

/* Save permanently everything on the maybepermanent_obstack.  */

void
preserve_data ()
{
  maybepermanent_firstobj
    = (char *) obstack_alloc (&maybepermanent_obstack, 0);
}

/* Allocate SIZE bytes in the current obstack
   and return a pointer to them.
   In practice the current obstack is always the temporary one.  */

char *
oballoc (size)
     int size;
{
  return (char *) obstack_alloc (current_obstack, size);
}

/* Free the object PTR in the current obstack
   as well as everything allocated since PTR.
   In practice the current obstack is always the temporary one.  */

void
obfree (ptr)
     char *ptr;
{
  obstack_free (current_obstack, ptr);
}

/* Allocate SIZE bytes in the permanent obstack
   and return a pointer to them.  */

char *
permalloc (size)
     long size;
{
  return (char *) obstack_alloc (&permanent_obstack, size);
}

/* Allocate SIZE bytes in the saveable obstack
   and return a pointer to them.  */

char *
savealloc (size)
     int size;
{
  return (char *) obstack_alloc (saveable_obstack, size);
}

/* Start a level of momentary allocation.
   In C, each compound statement has its own level
   and that level is freed at the end of each statement.
   All expression nodes are allocated in the momentary allocation level.  */

void
push_momentary ()
{
  struct momentary_level *tem
    = (struct momentary_level *) obstack_alloc (&momentary_obstack,
						sizeof (struct momentary_level));
  tem->prev = momentary_stack;
  tem->base = (char *) obstack_base (&momentary_obstack);
  tem->obstack = expression_obstack;
  momentary_stack = tem;
  expression_obstack = &momentary_obstack;
}

/* Free all the storage in the current momentary-allocation level.
   In C, this happens at the end of each statement.  */

void
clear_momentary ()
{
  obstack_free (&momentary_obstack, momentary_stack->base);
}

/* Discard a level of momentary allocation.
   In C, this happens at the end of each compound statement.
   Restore the status of expression node allocation
   that was in effect before this level was created.  */

void
pop_momentary ()
{
  struct momentary_level *tem = momentary_stack;
  momentary_stack = tem->prev;
  obstack_free (&momentary_obstack, tem);
  expression_obstack = tem->obstack;
}

/* Call when starting to parse a declaration:
   make expressions in the declaration last the length of the function.
   Returns an argument that should be passed to resume_momentary later.  */

int
suspend_momentary ()
{
  register int tem = expression_obstack == &momentary_obstack;
  expression_obstack = saveable_obstack;
  return tem;
}

/* Call when finished parsing a declaration:
   restore the treatment of node-allocation that was
   in effect before the suspension.
   YES should be the value previously returned by suspend_momentary.  */

void
resume_momentary (yes)
     int yes;
{
  if (yes)
    expression_obstack = &momentary_obstack;
}

/* Return a newly allocated node of code CODE.
   Initialize the node's unique id and its TREE_PERMANENT flag.
   For decl and type nodes, some other fields are initialized.
   The rest of the node is initialized to zero.

   Achoo!  I got a code in the node.  */

tree
make_node (code)
     enum tree_code code;
{
  register tree t;
  register int type = *tree_code_type[(int) code];
  register int length;
  register struct obstack *obstack = current_obstack;
  register int i;

  switch (type)
    {
    case 'd':  /* A decl node */
      length = sizeof (struct tree_decl);
      /* All decls in an inline function need to be saved.  */
      if (obstack != &permanent_obstack)
	obstack = saveable_obstack;
      /* PARM_DECLs always go on saveable_obstack, not permanent,
	 even though we may make them before the function turns
	 on temporary allocation.  */
      else if (code == PARM_DECL)
	obstack = &maybepermanent_obstack;
      break;

    case 't':  /* a type node */
      length = sizeof (struct tree_type);
      /* All data types are put where we can preserve them if nec.  */
      if (obstack != &permanent_obstack)
	obstack = all_types_permanent ? &permanent_obstack : saveable_obstack;
      break;

    case 's':  /* a stmt node */
      length = sizeof (struct tree_common)
	+ 2 * sizeof (int)
	  + tree_code_length[(int) code] * sizeof (char *);
      /* All stmts are put where we can preserve them if nec.  */
      if (obstack != &permanent_obstack)
	obstack = saveable_obstack;
      break;

    case 'r':  /* a reference */
    case 'e':  /* an expression */
      obstack = expression_obstack;
      length = sizeof (struct tree_exp)
	+ (tree_code_length[(int) code] - 1) * sizeof (char *);
      break;

    case 'c':  /* a constant */
      obstack = expression_obstack;
      /* We can't use tree_code_length for this, since the number of words
	 is machine-dependent due to varying alignment of `double'.  */
      if (code == REAL_CST)
	{
	  length = sizeof (struct tree_real_cst);
	  break;
	}

    case 'x':  /* something random, like an identifier.  */
      length = sizeof (struct tree_common)
	+ tree_code_length[(int) code] * sizeof (char *);
      /* Identifier nodes are always permanent since they are
	 unique in a compiler run.  */
      if (code == IDENTIFIER_NODE) obstack = &permanent_obstack;
    }

  t = (tree) obstack_alloc (obstack, length);

  TREE_UID (t) = tree_node_counter++;
  TREE_TYPE (t) = 0;
  TREE_CHAIN (t) = 0;
  for (i = (length / sizeof (int)) - 1;
       i >= sizeof (struct tree_common) / sizeof (int) - 1;
       i--)
    ((int *) t)[i] = 0;

  TREE_SET_CODE (t, code);
  if (obstack == &permanent_obstack)
    TREE_PERMANENT (t) = 1;

  if (type == 'd')
    {
      extern int lineno;

      DECL_ALIGN (t) = 1;
      DECL_SIZE_UNIT (t) = 1;
      DECL_VOFFSET_UNIT (t) = 1;
      DECL_SOURCE_LINE (t) = lineno;
      DECL_SOURCE_FILE (t) = input_filename;
    }

  if (type == 't')
    {
      TYPE_ALIGN (t) = 1;
      TYPE_SIZE_UNIT (t) = 1;
      TYPE_MAIN_VARIANT (t) = t;
    }

  if (type == 'c')
    {
      TREE_LITERAL (t) = 1;
    }

  return t;
}

/* Return a new node with the same contents as NODE
   except that its TREE_CHAIN is zero and it has a fresh uid.  */

tree
copy_node (node)
     tree node;
{
  register tree t;
  register enum tree_code code = TREE_CODE (node);
  register int length;
  register int i;

  switch (*tree_code_type[(int) code])
    {
    case 'd':  /* A decl node */
      length = sizeof (struct tree_decl);
      break;

    case 't':  /* a type node */
      length = sizeof (struct tree_type);
      break;

    case 's':
      length = sizeof (struct tree_common)
	+ 2 * sizeof (int)
	  + tree_code_length[(int) code] * sizeof (char *);
      break;

    case 'r':  /* a reference */
    case 'e':  /* a expression */
      length = sizeof (struct tree_exp)
	+ (tree_code_length[(int) code] - 1) * sizeof (char *);
      break;

    case 'c':  /* a constant */
      /* We can't use tree_code_length for this, since the number of words
	 is machine-dependent due to varying alignment of `double'.  */
      if (code == REAL_CST)
	{
	  length = sizeof (struct tree_real_cst);
	  break;
	}

    case 'x':  /* something random, like an identifier.  */
      length = sizeof (struct tree_common)
	+ tree_code_length[(int) code] * sizeof (char *);
    }

  t = (tree) obstack_alloc (current_obstack, length);

  for (i = ((length + sizeof (int) - 1) / sizeof (int)) - 1;
       i >= 0;
       i--)
    ((int *) t)[i] = ((int *) node)[i];

  TREE_UID (t) = tree_node_counter++;
  TREE_CHAIN (t) = 0;

  TREE_PERMANENT (t) = (current_obstack == &permanent_obstack);

  return t;
}

/* Return a copy of a chain of nodes, chained through the TREE_CHAIN field.
   For example, this can copy a list made of TREE_LIST nodes.  */

tree
copy_list (list)
     tree list;
{
  tree head;
  register tree prev, next;

  if (list == 0)
    return 0;

  head = prev = copy_node (list);
  next = TREE_CHAIN (list);
  while (next)
    {
      TREE_CHAIN (prev) = copy_node (next);
      prev = TREE_CHAIN (prev);
      next = TREE_CHAIN (next);
    }
  return head;
}

#define HASHBITS 30

/* Return an IDENTIFIER_NODE whose name is TEXT (a null-terminated string).
   If an identifier with that name has previously been referred to,
   the same node is returned this time.  */

tree
get_identifier (text)
     register char *text;
{
  register int hi;
  register int i;
  register tree idp;
  register int len, hash_len;

  /* Compute length of text in len.  */
  for (len = 0; text[len]; len++);

  /* Decide how much of that length to hash on */
  hash_len = len;
  if (warn_id_clash && len > id_clash_len)
    hash_len = id_clash_len;

  /* Compute hash code */
  hi = hash_len;
  for (i = 0; i < hash_len; i++)
    hi = ((hi * 613) + (unsigned)(text[i]));

  hi &= (1 << HASHBITS) - 1;
  hi %= MAX_HASH_TABLE;
  
  /* Search table for identifier */
  for (idp = hash_table[hi]; idp; idp = TREE_CHAIN (idp))
    if (IDENTIFIER_LENGTH (idp) == len
	&& !strcmp (IDENTIFIER_POINTER (idp), text))
      return idp;		/* <-- return if found */
  
  /* Not found; optionally warn about a similar identifier */
  if (warn_id_clash && do_identifier_warnings && len >= id_clash_len)
    for (idp = hash_table[hi]; idp; idp = TREE_CHAIN (idp))
      if (!strncmp (IDENTIFIER_POINTER (idp), text, id_clash_len))
	{
	  warning ("`%s' and `%s' identical in first n characters",
		   IDENTIFIER_POINTER (idp), text);
	  break;
	}

  /* Not found, create one, add to chain */
  idp = make_node (IDENTIFIER_NODE);
  IDENTIFIER_LENGTH (idp) = len;

  IDENTIFIER_POINTER (idp) = obstack_copy0 (&permanent_obstack, text, len);

  TREE_CHAIN (idp) = hash_table[hi];
  hash_table[hi] = idp;
  return idp;			/* <-- return if created */
}

/* Enable warnings on similar identifiers (if requested).
   Done after the built-in identifiers are created.  */

void
start_identifier_warnings ()
{
  do_identifier_warnings = 1;
}

/* Record the size of an identifier node for the language in use.
   This is called by the language-specific files.  */

void
set_identifier_size (size)
     int size;
{
  tree_code_length[(int) IDENTIFIER_NODE] = size;
}

/* Return a newly constructed INTEGER_CST node whose constant value
   is specified by the two ints LOW and HI.
   The TREE_TYPE is set to `int'.  */

tree
build_int_2 (low, hi)
     int low, hi;
{
  register tree t = make_node (INTEGER_CST);
  TREE_INT_CST_LOW (t) = low;
  TREE_INT_CST_HIGH (t) = hi;
  TREE_TYPE (t) = integer_type_node;
  return t;
}

/* Return a new REAL_CST node whose type is TYPE and value is D.  */

tree
build_real (type, d)
     tree type;
     REAL_VALUE_TYPE d;
{
  tree v;

  /* Check for valid float value for this type on this target machine;
     if not, can print error message and store a valid value in D.  */
#ifdef CHECK_FLOAT_VALUE
  CHECK_FLOAT_VALUE (TYPE_MODE (type), d);
#endif

  v = make_node (REAL_CST);
  TREE_TYPE (v) = type;
  TREE_REAL_CST (v) = d;
  return v;
}

/* Return a new REAL_CST node whose type is TYPE
   and whose value is the integer value of the INTEGER_CST node I.  */

#if !defined (REAL_IS_NOT_DOUBLE) || defined (REAL_ARITHMETIC)

REAL_VALUE_TYPE
real_value_from_int_cst (i)
     tree i;
{
  REAL_VALUE_TYPE d;
#ifdef REAL_ARITHMETIC
  REAL_VALUE_FROM_INT (d, TREE_INT_CST_LOW (i), TREE_INT_CST_HIGH (i));
#else /* not REAL_ARITHMETIC */
  if (TREE_INT_CST_HIGH (i) < 0)
    {
      d = (double) (~ TREE_INT_CST_HIGH (i));
      d *= ((double) (1 << (HOST_BITS_PER_INT / 2))
	    * (double) (1 << (HOST_BITS_PER_INT / 2)));
      d += (double) (unsigned) (~ TREE_INT_CST_LOW (i));
      d = (- d - 1.0);
    }
  else
    {
      d = (double) TREE_INT_CST_HIGH (i);
      d *= ((double) (1 << (HOST_BITS_PER_INT / 2))
	    * (double) (1 << (HOST_BITS_PER_INT / 2)));
      d += (double) (unsigned) TREE_INT_CST_LOW (i);
    }
#endif /* not REAL_ARITHMETIC */
  return d;
}

/* This function can't be implemented if we can't do arithmetic
   on the float representation.  */

tree
build_real_from_int_cst (type, i)
     tree type;
     tree i;
{
  tree v;
  REAL_VALUE_TYPE d;

  v = make_node (REAL_CST);
  TREE_TYPE (v) = type;

  d = real_value_from_int_cst (i);
  /* Check for valid float value for this type on this target machine;
     if not, can print error message and store a valid value in D.  */
#ifdef CHECK_FLOAT_VALUE
  CHECK_FLOAT_VALUE (TYPE_MODE (type), d);
#endif

  TREE_REAL_CST (v) = d;
  return v;
}

#endif /* not REAL_IS_NOT_DOUBLE, or REAL_ARITHMETIC */

/* Return a newly constructed STRING_CST node whose value is
   the LEN characters at STR.
   The TREE_TYPE is not initialized.  */

tree
build_string (len, str)
     int len;
     char *str;
{
  register tree s = make_node (STRING_CST);
  TREE_STRING_LENGTH (s) = len;
  TREE_STRING_POINTER (s) = obstack_copy0 (saveable_obstack, str, len);
  return s;
}

/* Return a newly constructed COMPLEX_CST node whose value is
   specified by the real and imaginary parts REAL and IMAG.
   Both REAL and IMAG should be constant nodes.
   The TREE_TYPE is not initialized.  */

tree
build_complex (real, imag)
     tree real, imag;
{
  register tree t = make_node (COMPLEX_CST);
  TREE_REALPART (t) = real;
  TREE_IMAGPART (t) = imag;
  return t;
}

/* Return 1 if EXPR is the integer constant zero.  */

int
integer_zerop (expr)
     tree expr;
{
  return (TREE_CODE (expr) == INTEGER_CST
	  && TREE_INT_CST_LOW (expr) == 0
	  && TREE_INT_CST_HIGH (expr) == 0);
}

/* Return 1 if EXPR is the integer constant one.  */

int
integer_onep (expr)
     tree expr;
{
  return (TREE_CODE (expr) == INTEGER_CST
	  && TREE_INT_CST_LOW (expr) == 1
	  && TREE_INT_CST_HIGH (expr) == 0);
}

/* Return 1 if EXPR is an integer containing all 1's
   in as much precision as it contains.  */

int
integer_all_onesp (expr)
     tree expr;
{
  register int prec;
  register int uns;

  if (TREE_CODE (expr) != INTEGER_CST)
    return 0;

  uns = TREE_UNSIGNED (TREE_TYPE (expr));
  if (!uns)
    return TREE_INT_CST_LOW (expr) == -1 && TREE_INT_CST_HIGH (expr) == -1;

  prec = TYPE_PRECISION (TREE_TYPE (expr));
  if (prec >= HOST_BITS_PER_INT)
    return TREE_INT_CST_LOW (expr) == -1
      && TREE_INT_CST_HIGH (expr) == (1 << (prec - HOST_BITS_PER_INT)) - 1;
  else
    return TREE_INT_CST_LOW (expr) == (1 << prec) - 1;
}

/* Return the length of a chain of nodes chained through TREE_CHAIN.
   We expect a null pointer to mark the end of the chain.
   This is the Lisp primitive `length'.  */

int
list_length (t)
     tree t;
{
  register tree tail;
  register int len = 0;

  for (tail = t; tail; tail = TREE_CHAIN (tail))
    len++;

  return len;
}

/* Concatenate two chains of nodes (chained through TREE_CHAIN)
   by modifying the last node in chain 1 to point to chain 2.
   This is the Lisp primitive `nconc'.  */

tree
chainon (op1, op2)
     tree op1, op2;
{
  tree t;

  if (op1)
    {
      for (t = op1; TREE_CHAIN (t); t = TREE_CHAIN (t))
	if (t == op2) abort ();	/* Circularity being created */
      TREE_CHAIN (t) = op2;
      return op1;
    }
  else return op2;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE.  */

tree
build_tree_list (parm, value)
     tree parm, value;
{
  register tree t = make_node (TREE_LIST);
  TREE_PURPOSE (t) = parm;
  TREE_VALUE (t) = value;
  return t;
}

/* Return a newly created TREE_LIST node whose
   purpose and value fields are PARM and VALUE
   and whose TREE_CHAIN is CHAIN.  */

tree
tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node = make_node (TREE_LIST);
  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;
  return node;
}

/* Same as `tree_cons' but make a permanent object.  */

tree
perm_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = &permanent_obstack;

  node = make_node (TREE_LIST);
  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;

  current_obstack = ambient_obstack;
  return node;
}

/* Same as `tree_cons', but make this node temporary, regardless.  */

tree
temp_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = &temporary_obstack;

  node = make_node (TREE_LIST);
  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;

  current_obstack = ambient_obstack;
  return node;
}

/* Same as `tree_cons', but save this node if the function's RTL is saved.  */

tree
saveable_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  register tree node;
  register struct obstack *ambient_obstack = current_obstack;
  current_obstack = saveable_obstack;

  node = make_node (TREE_LIST);
  TREE_CHAIN (node) = chain;
  TREE_PURPOSE (node) = purpose;
  TREE_VALUE (node) = value;

  current_obstack = ambient_obstack;
  return node;
}

/* Return the last node in a chain of nodes (chained through TREE_CHAIN).  */

tree
tree_last (chain)
     register tree chain;
{
  register tree next;
  if (chain)
    while (next = TREE_CHAIN (chain))
      chain = next;
  return chain;
}

/* Reverse the order of elements in the chain T,
   and return the new head of the chain (old last element).  */

tree
nreverse (t)
     tree t;
{
  register tree prev = 0, decl, next;
  for (decl = t; decl; decl = next)
    {
      next = TREE_CHAIN (decl);
      TREE_CHAIN (decl) = prev;
      prev = decl;
    }
  return prev;
}

/* Return the size nominally occupied by an object of type TYPE
   when it resides in memory.  The value is measured in units of bytes,
   and its data type is that normally used for type sizes
   (which is the first type created by make_signed_type or
   make_unsigned_type).  */

tree
size_in_bytes (type)
     tree type;
{
  if (type == error_mark_node)
    return integer_zero_node;
  type = TYPE_MAIN_VARIANT (type);
  if (TYPE_SIZE (type) == 0)
    {
      incomplete_type_error (0, type);
      return integer_zero_node;
    }
  return convert_units (TYPE_SIZE (type), TYPE_SIZE_UNIT (type),
			BITS_PER_UNIT);
}

/* Return the size of TYPE (in bytes) as an integer,
   or return -1 if the size can vary.  */

int
int_size_in_bytes (type)
     tree type;
{
  int size;
  if (type == error_mark_node)
    return 0;
  type = TYPE_MAIN_VARIANT (type);
  if (TYPE_SIZE (type) == 0)
    return -1;
  if (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST)
    return -1;
  size = TREE_INT_CST_LOW (TYPE_SIZE (type)) * TYPE_SIZE_UNIT (type);
  return (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
}

/* Return, as an INTEGER_CST node, the number of elements for
   TYPE (which is an ARRAY_TYPE).  */

tree
array_type_nelts (type)
     tree type;
{
  tree index_type = TYPE_DOMAIN (type);
  return (tree_int_cst_equal (TYPE_MIN_VALUE (index_type), integer_zero_node)
	  ? TYPE_MAX_VALUE (index_type)
	  : fold (build (MINUS_EXPR, integer_type_node,
			 TYPE_MAX_VALUE (index_type),
			 TYPE_MIN_VALUE (index_type))));
}

/* Return nonzero if arg is static -- a reference to an object in
   static storage.  This is not the same as the C meaning of `static'.  */

int
staticp (arg)
     tree arg;
{
  register enum tree_code code = TREE_CODE (arg);

  if ((code == VAR_DECL || code == FUNCTION_DECL || code == CONSTRUCTOR)
      && (TREE_STATIC (arg) || TREE_EXTERNAL (arg)))
    return 1;

  if (code == STRING_CST)
    return 1;

  if (code == COMPONENT_REF)
    return (DECL_VOFFSET (TREE_OPERAND (arg, 1)) == 0
	    && staticp (TREE_OPERAND (arg, 0)));

  if (code == INDIRECT_REF)
    return TREE_LITERAL (TREE_OPERAND (arg, 0));

  if (code == ARRAY_REF)
    {
      if (TREE_CODE (TYPE_SIZE (TREE_TYPE (arg))) == INTEGER_CST
	  && TREE_CODE (TREE_OPERAND (arg, 1)) == INTEGER_CST)
	return staticp (TREE_OPERAND (arg, 0));
    }

  return 0;
}

/* Return nonzero if REF is an lvalue valid for this language.
   Lvalues can be assigned, unless they have TREE_READONLY.
   Lvalues can have their address taken, unless they have TREE_REGDECL.  */

int
lvalue_p (ref)
     tree ref;
{
  register enum tree_code code = TREE_CODE (ref);

  if (language_lvalue_valid (ref))
    switch (code)
      {
      case COMPONENT_REF:
	return lvalue_p (TREE_OPERAND (ref, 0));

      case STRING_CST:
	return 1;

      case INDIRECT_REF:
      case ARRAY_REF:
      case VAR_DECL:
      case PARM_DECL:
      case RESULT_DECL:
      case ERROR_MARK:
	if (TREE_CODE (TREE_TYPE (ref)) != FUNCTION_TYPE
	    && TREE_CODE (TREE_TYPE (ref)) != METHOD_TYPE)
	  return 1;
	break;

      case NEW_EXPR:
	return 1;

      case CALL_EXPR:
	if (TREE_CODE (TREE_TYPE (ref)) == REFERENCE_TYPE)
	  return 1;
      }
  return 0;
}

/* Return nonzero if REF is an lvalue valid for this language;
   otherwise, print an error message and return zero.  */

int
lvalue_or_else (ref, string)
     tree ref;
     char *string;
{
  int win = lvalue_p (ref);
  if (! win)
    error ("invalid lvalue in %s", string);
  return win;
}

/* This should be applied to any node which may be used in more than one place,
   but must be evaluated only once.  Normally, the code generator would
   reevaluate the node each time; this forces it to compute it once and save
   the result.  This is done by encapsulating the node in a SAVE_EXPR.  */

tree
save_expr (expr)
     tree expr;
{
  register tree t = fold (expr);

  /* If the tree evaluates to a constant, then we don't want to hide that
     fact (i.e. this allows further folding, and direct checks for constants).
     Since it is no problem to reevaluate literals, we just return the 
     literal node. */

  if (TREE_LITERAL (t) || TREE_READONLY (t) || TREE_CODE (t) == SAVE_EXPR)
    return t;

  return build (SAVE_EXPR, TREE_TYPE (expr), t, NULL);
}

/* Stabilize a reference so that we can use it any number of times
   without causing its operands to be evaluated more than once.
   Returns the stabilized reference.

   Also allows conversion expressions whose operands are references.
   Any other kind of expression is returned unchanged.  */

tree
stabilize_reference (ref)
     tree ref;
{
  register tree result;
  register enum tree_code code = TREE_CODE (ref);

  switch (code)
    {
    case VAR_DECL:
    case PARM_DECL:
    case RESULT_DECL:
      result = ref;
      break;

    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      result = build_nt (code, stabilize_reference (TREE_OPERAND (ref, 0)));
      break;

    case INDIRECT_REF:
      result = build_nt (INDIRECT_REF, save_expr (TREE_OPERAND (ref, 0)));
      break;

    case COMPONENT_REF:
      result = build_nt (COMPONENT_REF,
			 stabilize_reference (TREE_OPERAND (ref, 0)),
			 TREE_OPERAND (ref, 1));
      break;

    case ARRAY_REF:
      result = build_nt (ARRAY_REF, stabilize_reference (TREE_OPERAND (ref, 0)),
			 save_expr (TREE_OPERAND (ref, 1)));
      break;

      /* If arg isn't a kind of lvalue we recognize, make no change.
	 Caller should recognize the error for an invalid lvalue.  */
    default:
      return ref;

    case ERROR_MARK:
      return error_mark_node;
    }

  TREE_TYPE (result) = TREE_TYPE (ref);
  TREE_READONLY (result) = TREE_READONLY (ref);
  TREE_VOLATILE (result) = TREE_VOLATILE (ref);
  TREE_THIS_VOLATILE (result) = TREE_THIS_VOLATILE (ref);

  return result;
}

/* Low-level constructors for expressions.  */

/* Build an expression of code CODE, data type TYPE,
   and operands as specified by the arguments ARG1 and following arguments.
   Expressions and reference nodes can be created this way.
   Constants, decls, types and misc nodes cannot be.  */

tree
build (va_alist)
     va_dcl
{
  register va_list p;
  enum tree_code code;
  register tree t;
  register int length;
  register int i;

  va_start (p);

  code = va_arg (p, enum tree_code);
  t = make_node (code);
  length = tree_code_length[(int) code];
  TREE_TYPE (t) = va_arg (p, tree);

  if (length == 2)
    {
      /* This is equivalent to the loop below, but faster.  */
      register tree arg0 = va_arg (p, tree);
      register tree arg1 = va_arg (p, tree);
      TREE_OPERAND (t, 0) = arg0;
      TREE_OPERAND (t, 1) = arg1;
      TREE_VOLATILE (t)
	= (arg0 && TREE_VOLATILE (arg0)) || (arg1 && TREE_VOLATILE (arg1));
    }
  else
    {
      for (i = 0; i < length; i++)
	{
	  register tree operand = va_arg (p, tree);
	  TREE_OPERAND (t, i) = operand;
	  if (operand && TREE_VOLATILE (operand))
	    TREE_VOLATILE (t) = 1;
	}
    }
  va_end (p);
  return t;
}

/* Similar except don't specify the TREE_TYPE
   and leave the TREE_VOLATILE as 0.
   It is permissible for arguments to be null,
   or even garbage if their values do not matter.  */

tree
build_nt (va_alist)
     va_dcl
{
  register va_list p;
  register enum tree_code code;
  register tree t;
  register int length;
  register int i;

  va_start (p);

  code = va_arg (p, enum tree_code);
  t = make_node (code);
  length = tree_code_length[(int) code];

  for (i = 0; i < length; i++)
    TREE_OPERAND (t, i) = va_arg (p, tree);

  va_end (p);
  return t;
}

tree
build_op_identifier (op1, op2)
     tree op1, op2;
{
  register tree t = make_node (OP_IDENTIFIER);
  TREE_PURPOSE (t) = op1;
  TREE_VALUE (t) = op2;
  return t;
}

/* Create a DECL_... node of code CODE, name NAME and data type TYPE.
   We do NOT enter this node in any sort of symbol table.

   layout_decl is used to set up the decl's storage layout.
   Other slots are initialized to 0 or null pointers.  */

tree
build_decl (code, name, type)
     enum tree_code code;
     tree name, type;
{
  register tree t;

  t = make_node (code);

/*  if (type == error_mark_node)
    type = integer_type_node; */
/* That is not done, deliberately, so that having error_mark_node
   as the type can suppress useless errors in the use of this variable.  */

  DECL_NAME (t) = name;
  if (name)
    {
      DECL_PRINT_NAME (t) = IDENTIFIER_POINTER (name);
      DECL_ASSEMBLER_NAME (t) = IDENTIFIER_POINTER (name);
    }
  TREE_TYPE (t) = type;
  DECL_ARGUMENTS (t) = NULL_TREE;
  DECL_INITIAL (t) = NULL_TREE;

  if (code == VAR_DECL || code == PARM_DECL || code == RESULT_DECL)
    layout_decl (t, 0);
  else if (code == FUNCTION_DECL)
    DECL_MODE (t) = FUNCTION_MODE;

  return t;
}

#if 0
/* Low-level constructors for statements.
   These constructors all expect source file name and line number
   as arguments, as well as enough arguments to fill in the data
   in the statement node.  */

tree
build_goto (filename, line, label)
     char *filename;
     int line;
     tree label;
{
  register tree t = make_node (GOTO_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = label;
  return t;
}

tree
build_return (filename, line, arg)
     char *filename;
     int line;
     tree arg;
{
  register tree t = make_node (RETURN_STMT);

  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = arg;
  return t;
}

tree
build_expr_stmt (filename, line, expr)
     char *filename;
     int line;
     tree expr;
{
  register tree t = make_node (EXPR_STMT);

  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = expr;
  return t;
}

tree
build_if (filename, line, cond, thenclause, elseclause)
     char *filename;
     int line;
     tree cond, thenclause, elseclause;
{
  register tree t = make_node (IF_STMT);

  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_COND (t) = cond;
  STMT_THEN (t) = thenclause;
  STMT_ELSE (t) = elseclause;
  return t;
}

tree
build_exit (filename, line, cond)
     char *filename;
     int line;
     tree cond;
{
  register tree t = make_node (EXIT_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = cond;
  return t;
}

tree
build_asm_stmt (filename, line, asmcode)
     char *filename;
     int line;
     tree asmcode;
{
  register tree t = make_node (ASM_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = asmcode;
  return t;
}

tree
build_case (filename, line, object, cases)
     char *filename;
     int line;
     tree object, cases;
{
  register tree t = make_node (CASE_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_CASE_INDEX (t) = object;
  STMT_CASE_LIST (t) = cases;
  return t;
}

tree
build_loop (filename, line, body)
     char *filename;
     int line;
     tree body;
{
  register tree t = make_node (LOOP_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = body;
  return t;
}

tree
build_compound (filename, line, body)
     char *filename;
     int line;
     tree body;
{
  register tree t = make_node (COMPOUND_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_BODY (t) = body;
  return t;
}

#endif /* 0 */

/* LET_STMT nodes are used to represent the structure of binding contours
   and declarations, once those contours have been exited and their contents
   compiled.  This information is used for outputting debugging info.  */

tree
build_let (filename, line, vars, subblocks, supercontext, tags)
     char *filename;
     int line;
     tree vars, subblocks, supercontext, tags;
{
  register tree t = make_node (LET_STMT);
  STMT_SOURCE_FILE (t) = filename;
  STMT_SOURCE_LINE (t) = line;
  STMT_VARS (t) = vars;
  STMT_SUBBLOCKS (t) = subblocks;
  STMT_SUPERCONTEXT (t) = supercontext;
  STMT_BIND_SIZE (t) = 0;
  STMT_TYPE_TAGS (t) = tags;
  return t;
}

/* Return a type like TYPE except that its TREE_READONLY is CONSTP
   and its TREE_VOLATILE is VOLATILEP.

   Such variant types already made are recorded so that duplicates
   are not made.

   A variant types should never be used as the type of an expression.
   Always copy the variant information into the TREE_READONLY
   and TREE_VOLATILE of the expression, and then give the expression
   as its type the "main variant", the variant whose TREE_READONLY
   and TREE_VOLATILE are zero.  Use TYPE_MAIN_VARIANT to find the
   main variant.  */

tree
build_type_variant (type, constp, volatilep)
     tree type;
     int constp, volatilep;
{
  register tree t, m = TYPE_MAIN_VARIANT (type);
  register struct obstack *ambient_obstack = current_obstack;

  /* Treat any nonzero argument as 1.  */
  constp = !!constp;
  volatilep = !!volatilep;

  /* First search the chain variants for one that is what we want.  */

  for (t = m; t; t = TYPE_NEXT_VARIANT (t))
    if (constp == TREE_READONLY (t)
	&& volatilep == TREE_VOLATILE (t))
      return t;

  /* We need a new one.  */
  current_obstack
    = TREE_PERMANENT (type) ? &permanent_obstack : saveable_obstack;

  t = copy_node (type);
  TREE_READONLY (t) = constp;
  TREE_VOLATILE (t) = volatilep;
  TYPE_POINTER_TO (t) = 0;
  TYPE_REFERENCE_TO (t) = 0;

  /* Add this type to the chain of variants of TYPE.  */
  TYPE_NEXT_VARIANT (t) = TYPE_NEXT_VARIANT (m);
  TYPE_NEXT_VARIANT (m) = t;

  current_obstack = ambient_obstack;
  return t;
}

/* Hashing of types so that we don't make duplicates.
   The entry point is `type_hash_canon'.  */

/* Each hash table slot is a bucket containing a chain
   of these structures.  */

struct type_hash
{
  struct type_hash *next;	/* Next structure in the bucket.  */
  int hashcode;			/* Hash code of this type.  */
  tree type;			/* The type recorded here.  */
};

/* Now here is the hash table.  When recording a type, it is added
   to the slot whose index is the hash code mod the table size.
   Note that the hash table is used for several kinds of types
   (function types, array types and array index range types, for now).
   While all these live in the same table, they are completely independent,
   and the hash code is computed differently for each of these.  */

#define TYPE_HASH_SIZE 59
struct type_hash *type_hash_table[TYPE_HASH_SIZE];

/* Here is how primitive or already-canonicalized types' hash
   codes are made.  */
#define TYPE_HASH(TYPE) TREE_UID (TYPE)

/* Compute a hash code for a list of types (chain of TREE_LIST nodes
   with types in the TREE_VALUE slots), by adding the hash codes
   of the individual types.  */

int
type_hash_list (list)
     tree list;
{
  register int hashcode;
  register tree tail;
  for (hashcode = 0, tail = list; tail; tail = TREE_CHAIN (tail))
    hashcode += TYPE_HASH (TREE_VALUE (tail));
  return hashcode;
}

/* Look in the type hash table for a type isomorphic to TYPE.
   If one is found, return it.  Otherwise return 0.  */

tree
type_hash_lookup (hashcode, type)
     int hashcode;
     tree type;
{
  register struct type_hash *h;
  for (h = type_hash_table[hashcode % TYPE_HASH_SIZE]; h; h = h->next)
    if (h->hashcode == hashcode
	&& TREE_CODE (h->type) == TREE_CODE (type)
	&& TREE_TYPE (h->type) == TREE_TYPE (type)
	&& (TYPE_MAX_VALUE (h->type) == TYPE_MAX_VALUE (type)
	    || tree_int_cst_equal (TYPE_MAX_VALUE (h->type),
				   TYPE_MAX_VALUE (type)))
	&& (TYPE_MIN_VALUE (h->type) == TYPE_MIN_VALUE (type)
	    || tree_int_cst_equal (TYPE_MIN_VALUE (h->type),
				   TYPE_MIN_VALUE (type)))
	&& (TYPE_DOMAIN (h->type) == TYPE_DOMAIN (type)
	    || (TREE_CODE (TYPE_DOMAIN (h->type)) == TREE_LIST
		&& TREE_CODE (TYPE_DOMAIN (type)) == TREE_LIST
		&& type_list_equal (TYPE_DOMAIN (h->type), TYPE_DOMAIN (type)))))
      return h->type;
  return 0;
}

/* Add an entry to the type-hash-table
   for a type TYPE whose hash code is HASHCODE.  */

void
type_hash_add (hashcode, type)
     int hashcode;
     tree type;
{
  register struct type_hash *h;

  h = (struct type_hash *) oballoc (sizeof (struct type_hash));
  h->hashcode = hashcode;
  h->type = type;
  h->next = type_hash_table[hashcode % TYPE_HASH_SIZE];
  type_hash_table[hashcode % TYPE_HASH_SIZE] = h;
}

/* Given TYPE, and HASHCODE its hash code, return the canonical
   object for an identical type if one already exists.
   Otherwise, return TYPE, and record it as the canonical object
   if it is a permanent object.

   To use this function, first create a type of the sort you want.
   Then compute its hash code from the fields of the type that
   make it different from other similar types.
   Then call this function and use the value.
   This function frees the type you pass in if it is a duplicate.  */

/* Set to 1 to debug without canonicalization.  Never set by program.  */
int debug_no_type_hash = 0;

tree
type_hash_canon (hashcode, type)
     int hashcode;
     tree type;
{
  tree t1;

  if (debug_no_type_hash)
    return type;

  t1 = type_hash_lookup (hashcode, type);
  if (t1 != 0)
    {
      struct obstack *o
	= TREE_PERMANENT (type) ? &permanent_obstack : saveable_obstack;
      obstack_free (o, type);
      return t1;
    }

  /* If this is a new type, record it for later reuse.  */
  if (current_obstack == &permanent_obstack)
    type_hash_add (hashcode, type);

  return type;
}

/* Given two lists of types
   (chains of TREE_LIST nodes with types in the TREE_VALUE slots)
   return 1 if the lists contain the same types in the same order.
   Also, the TREE_PURPOSEs must match.  */

int
type_list_equal (l1, l2)
     tree l1, l2;
{
  register tree t1, t2;
  for (t1 = l1, t2 = l2; t1 && t2; t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    {
      if (TREE_VALUE (t1) != TREE_VALUE (t2))
	return 0;
      if (TREE_PURPOSE (t1) != TREE_PURPOSE (t2)
	  && !simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2)))
	return 0;
    }

  return t1 == t2;
}

/* Nonzero if integer constants T1 and T2
   represent the same constant value.  */

int
tree_int_cst_equal (t1, t2)
     tree t1, t2;
{
  if (t1 == t2)
    return 1;
  if (t1 == 0 || t2 == 0)
    return 0;
  if (TREE_CODE (t1) == INTEGER_CST
      && TREE_CODE (t2) == INTEGER_CST
      && TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
      && TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2))
    return 1;
  return 0;
}

/* Nonzero if integer constants T1 and T2 represent values that satisfy <.
   The precise way of comparison depends on their data type.  */

int
tree_int_cst_lt (t1, t2)
     tree t1, t2;
{
  if (t1 == t2)
    return 0;

  if (!TREE_UNSIGNED (TREE_TYPE (t1)))
    return INT_CST_LT (t1, t2);
  return INT_CST_LT_UNSIGNED (t1, t2);
}

/* Compare two constructor-element-type constants.  */

int
simple_cst_equal (t1, t2)
     tree t1, t2;
{
  register enum tree_code code1, code2;

  if (t1 == t2)
    return 1;
  if (t1 == 0 || t2 == 0)
    return 0;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  if (code1 == NOP_EXPR || code1 == CONVERT_EXPR)
    if (code2 == NOP_EXPR || code2 == CONVERT_EXPR)
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
    else
      return simple_cst_equal (TREE_OPERAND (t1, 0), t2);
  else if (code2 == NOP_EXPR || code2 == CONVERT_EXPR)
    return simple_cst_equal (t1, TREE_OPERAND (t2, 0));

  if (code1 != code2)
    return 0;

  switch (code1)
    {
    case INTEGER_CST:
      return TREE_INT_CST_LOW (t1) == TREE_INT_CST_LOW (t2)
	&& TREE_INT_CST_HIGH (t1) == TREE_INT_CST_HIGH (t2);

    case REAL_CST:
      return REAL_VALUES_EQUAL (TREE_REAL_CST (t1), TREE_REAL_CST (t2));

    case STRING_CST:
      return TREE_STRING_LENGTH (t1) == TREE_STRING_LENGTH (t2)
	&& !strcmp (TREE_STRING_POINTER (t1), TREE_STRING_POINTER (t2));

    case CONSTRUCTOR:
      abort ();

    case VAR_DECL:
    case PARM_DECL:
    case CONST_DECL:
      return 0;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
      return (simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0))
	      && simple_cst_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1)));

    case NEGATE_EXPR:
    case ADDR_EXPR:
    case REFERENCE_EXPR:
    case INDIRECT_REF:
      return simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));

    default:
      abort ();
    }
}

/* Constructors for pointer, array and function types.
   (RECORD_TYPE, UNION_TYPE and ENUMERAL_TYPE nodes are
   constructed by language-dependent code, not here.)  */

/* Construct, lay out and return the type of pointers to TO_TYPE.
   If such a type has already been constructed, reuse it.  */

tree
build_pointer_type (to_type)
     tree to_type;
{
  register tree t = TYPE_POINTER_TO (to_type);
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one.  If TO_TYPE is permanent, make this permanent too.  */
  if (TREE_PERMANENT (to_type))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  t = make_node (POINTER_TYPE);
  TREE_TYPE (t) = to_type;

  /* Record this type as the pointer to TO_TYPE.  */
  TYPE_POINTER_TO (to_type) = t;

  /* Lay out the type.  This function has many callers that are concerned
     with expression-construction, and this simplifies them all.
     Also, it guarantees the TYPE_SIZE is permanent if the type is.  */
  layout_type (t);

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
  return t;
}

/* Create a type of integers to be the TYPE_DOMAIN of an ARRAY_TYPE.
   MAXVAL should be the maximum value in the domain
   (one less than the length of the array).  */

tree
build_index_type (maxval)
     tree maxval;
{
  register tree itype = make_node (INTEGER_TYPE);
  TYPE_PRECISION (itype) = BITS_PER_WORD;
  TYPE_MIN_VALUE (itype) = build_int_2 (0, 0);
  TREE_TYPE (TYPE_MIN_VALUE (itype)) = sizetype;
  TYPE_MAX_VALUE (itype) = convert (sizetype, maxval);
  TYPE_MODE (itype) = SImode;
  TYPE_SIZE (itype) = TYPE_SIZE (sizetype);
  TYPE_SIZE_UNIT (itype) = TYPE_SIZE_UNIT (sizetype);
  TYPE_ALIGN (itype) = TYPE_ALIGN (sizetype);
  if (TREE_CODE (maxval) == INTEGER_CST)
    {
      int maxint = TREE_INT_CST_LOW (maxval);
      return type_hash_canon (maxint > 0 ? maxint : - maxint, itype);
    }
  else
    return itype;
}

/* Construct, lay out and return the type of arrays of elements with ELT_TYPE
   and number of elements specified by the range of values of INDEX_TYPE.
   If such a type has already been constructed, reuse it.  */

tree
build_array_type (elt_type, index_type)
     tree elt_type, index_type;
{
  register tree t = make_node (ARRAY_TYPE);
  int hashcode;

  if (TREE_CODE (elt_type) == FUNCTION_TYPE)
    {
      error ("arrays of functions are not meaningful");
      elt_type = integer_type_node;
    }

  TREE_TYPE (t) = elt_type;
  TYPE_DOMAIN (t) = index_type;

  /* Make sure TYPE_POINTER_TO (elt_type) is filled in.  */
  build_pointer_type (elt_type);
#if 0
  /* Also that of the main variant, which is the type the element will have. */
  build_pointer_type (TYPE_MAIN_VARIANT (elt_type));
#endif

  if (index_type == 0)
    return t;

  hashcode = TYPE_HASH (elt_type) + TYPE_HASH (index_type);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);
  return t;
}

/* Construct, lay out and return
   the type of functions returning type VALUE_TYPE
   given arguments of types ARG_TYPES.
   ARG_TYPES is a chain of TREE_LIST nodes whose TREE_VALUEs
   are data type nodes for the arguments of the function.
   If such a type has already been constructed, reuse it.  */

tree
build_function_type (value_type, arg_types)
     tree value_type, arg_types;
{
  register tree t;
  int hashcode;

  if (TREE_CODE (value_type) == FUNCTION_TYPE
      || TREE_CODE (value_type) == ARRAY_TYPE)
    {
      error ("function return type cannot be function or array");
      value_type = integer_type_node;
    }

  /* Make a node of the sort we want.  */
  t = make_node (FUNCTION_TYPE);
  TREE_TYPE (t) = value_type;
  TYPE_ARG_TYPES (t) = arg_types;

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (value_type) + type_hash_list (arg_types);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);
  return t;
}

/* Build the node for the type of references-to-TO_TYPE.  */

tree
build_reference_type (to_type)
     tree to_type;
{
  register tree t = TYPE_REFERENCE_TO (to_type);
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;

  /* First, if we already have a type for pointers to TO_TYPE, use it.  */

  if (t)
    return t;

  /* We need a new one.  If TO_TYPE is permanent, make this permanent too.  */
  if (TREE_PERMANENT (to_type))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  t = make_node (REFERENCE_TYPE);
  TREE_TYPE (t) = to_type;

  /* Record this type as the pointer to TO_TYPE.  */
  TYPE_REFERENCE_TO (to_type) = t;

  layout_type (t);

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
  return t;
}

/* Construct, lay out and return the type of methods belonging to class
   BASETYPE and whose arguments and values are described by TYPE.
   If that type exists already, reuse it.
   TYPE must be a FUNCTION_TYPE node.  */

tree
build_method_type (basetype, type)
     tree basetype, type;
{
  register tree t;
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (METHOD_TYPE);

  if (TREE_CODE (type) != FUNCTION_TYPE)
    abort ();

  TYPE_METHOD_BASETYPE (t) = basetype;
  TREE_TYPE (t) = TREE_TYPE (type);

  /* The actual arglist for this function includes a "hidden" argument
     which is "this".  Put it into the list of argument types.  */

  TYPE_ARG_TYPES (t)
    = tree_cons (NULL, build_pointer_type (basetype), TYPE_ARG_TYPES (type));

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (type);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

/* Construct, lay out and return the type of methods belonging to class
   BASETYPE and whose arguments and values are described by TYPE.
   If that type exists already, reuse it.
   TYPE must be a FUNCTION_TYPE node.  */

tree
build_offset_type (basetype, type)
     tree basetype, type;
{
  register tree t;
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (OFFSET_TYPE);

  TYPE_OFFSET_BASETYPE (t) = basetype;
  TREE_TYPE (t) = type;

  /* If we already have such a type, use the old one and free this one.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (type);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

/* Return OP, stripped of any conversions to wider types as much as is safe.
   Converting the value back to OP's type makes a value equivalent to OP.

   If FOR_TYPE is nonzero, we return a value which, if converted to
   type FOR_TYPE, would be equivalent to converting OP to type FOR_TYPE.

   If FOR_TYPE is nonzero, unaligned bit-field references may be changed to the
   narrowest type that can hold the value, even if they don't exactly fit.
   Otherwise, bit-field references are changed to a narrower type
   only if they can be fetched directly from memory in that type.

   OP must have integer, real or enumeral type.  Pointers are not allowed!

   There are some cases where the obvious value we could return
   would regenerate to OP if converted to OP's type, 
   but would not extend like OP to wider types.
   If FOR_TYPE indicates such extension is contemplated, we eschew such values.
   For example, if OP is (unsigned short)(signed char)-1,
   we avoid returning (signed char)-1 if FOR_TYPE is int,
   even though extending that to an unsigned short would regenerate OP,
   since the result of extending (signed char)-1 to (int)
   is different from (int) OP.  */

tree
get_unwidened (op, for_type)
     register tree op;
     tree for_type;
{
  /* Set UNS initially if converting OP to FOR_TYPE is a zero-extension.  */
  /* TYPE_PRECISION is safe in place of type_precision since
     pointer types are not allowed.  */
  register tree type = TREE_TYPE (op);
  register int final_prec = TYPE_PRECISION (for_type != 0 ? for_type : type);
  register int uns
    = (for_type != 0 && for_type != type
       && final_prec > TYPE_PRECISION (type)
       && TREE_UNSIGNED (type));
  register tree win = op;

  while (TREE_CODE (op) == NOP_EXPR)
    {
      register int bitschange
	= TYPE_PRECISION (TREE_TYPE (op))
	  - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0)));

      /* Truncations are many-one so cannot be removed.
	 Unless we are later going to truncate down even farther.  */
      if (bitschange < 0
	  && final_prec > TYPE_PRECISION (TREE_TYPE (op)))
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */
      op = TREE_OPERAND (op, 0);

      /* If we have not stripped any zero-extensions (uns is 0),
	 we can strip any kind of extension.
	 If we have previously stripped a zero-extension,
	 only zero-extensions can safely be stripped.
	 Any extension can be stripped if the bits it would produce
	 are all going to be discarded later by truncating to FOR_TYPE.  */

      if (bitschange > 0)
	{
	  if (! uns || final_prec <= TYPE_PRECISION (TREE_TYPE (op)))
	    win = op;
	  /* TREE_UNSIGNED says whether this is a zero-extension.
	     Let's avoid computing it if it does not affect WIN
	     and if UNS will not be needed again.  */
	  if ((uns || TREE_CODE (op) == NOP_EXPR)
	      && TREE_UNSIGNED (TREE_TYPE (op)))
	    {
	      uns = 1;
	      win = op;
	    }
	}
    }

  if (TREE_CODE (op) == COMPONENT_REF
      /* Since type_for_size always gives an integer type.  */
      && TREE_CODE (type) != REAL_TYPE)
    {
      int innerprec = (TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (op, 1)))
		       * DECL_SIZE_UNIT (TREE_OPERAND (op, 1)));
      type = type_for_size (innerprec, TREE_UNSIGNED (TREE_OPERAND (op, 1)));

      /* We can get this structure field in the narrowest type it fits in.
	 If FOR_TYPE is 0, do this only for a field that matches the
	 narrower type exactly and is aligned for it (i.e. mode isn't BI).
	 The resulting extension to its nominal type (a fullword type)
	 must fit the same conditions as for other extensions.  */

      if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	  && (for_type || DECL_MODE (TREE_OPERAND (op, 1)) != BImode)
	  && (! uns || final_prec <= innerprec
	      || TREE_UNSIGNED (TREE_OPERAND (op, 1)))
	  && type != 0)
	{
	  win = build (COMPONENT_REF, type, TREE_OPERAND (op, 0),
		       TREE_OPERAND (op, 1));
	  TREE_VOLATILE (win) = TREE_VOLATILE (op);
	  TREE_THIS_VOLATILE (win) = TREE_THIS_VOLATILE (op);
	}
    }
  return win;
}

/* Return OP or a simpler expression for a narrower value
   which can be sign-extended or zero-extended to give back OP.
   Store in *UNSIGNEDP_PTR either 1 if the value should be zero-extended
   or 0 if the value should be sign-extended.  */

tree
get_narrower (op, unsignedp_ptr)
     register tree op;
     int *unsignedp_ptr;
{
  register int uns = 0;
  int first = 1;
  register tree win = op;

  while (TREE_CODE (op) == NOP_EXPR)
    {
      register int bitschange
	= TYPE_PRECISION (TREE_TYPE (op))
	  - TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op, 0)));

      /* Truncations are many-one so cannot be removed.  */
      if (bitschange < 0)
	break;

      /* See what's inside this conversion.  If we decide to strip it,
	 we will set WIN.  */
      op = TREE_OPERAND (op, 0);

      if (bitschange > 0)
	{
	  /* An extension: the outermost one can be stripped,
	     but remember whether it is zero or sign extension.  */
	  if (first)
	    uns = TREE_UNSIGNED (TREE_TYPE (op));
	  /* Otherwise, if a sign extension has been stripped,
	     only sign extensions can now be stripped;
	     if a zero extension has been stripped, only zero-extensions.  */
	  else if (uns != TREE_UNSIGNED (TREE_TYPE (op)))
	    break;
	  first = 0;
	}
      /* A change in nominal type can always be stripped.  */

      win = op;
    }

  if (TREE_CODE (op) == COMPONENT_REF
      /* Since type_for_size always gives an integer type.  */
      && TREE_CODE (TREE_TYPE (op)) != REAL_TYPE)
    {
      int innerprec = (TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (op, 1)))
		       * DECL_SIZE_UNIT (TREE_OPERAND (op, 1)));
      tree type = type_for_size (innerprec, TREE_UNSIGNED (op));

      /* We can get this structure field in a narrower type that fits it,
	 but the resulting extension to its nominal type (a fullword type)
	 must satisfy the same conditions as for other extensions.

	 Do this only for fields that are aligned (not BImode),
	 because when bit-field insns will be used there is no
	 advantage in doing this.  */

      if (innerprec < TYPE_PRECISION (TREE_TYPE (op))
	  && DECL_MODE (TREE_OPERAND (op, 1)) != BImode
	  && (first || uns == TREE_UNSIGNED (TREE_OPERAND (op, 1)))
	  && type != 0)
	{
	  if (first)
	    uns = TREE_UNSIGNED (TREE_OPERAND (op, 1));
	  win = build (COMPONENT_REF, type, TREE_OPERAND (op, 0),
		       TREE_OPERAND (op, 1));
	  TREE_VOLATILE (win) = TREE_VOLATILE (op);
	  TREE_THIS_VOLATILE (win) = TREE_THIS_VOLATILE (op);
	}
    }
  *unsignedp_ptr = uns;
  return win;
}

/* Return the precision of a type, for arithmetic purposes.
   Supports all types on which arithmetic is possible
   (including pointer types).
   It's not clear yet what will be right for complex types.  */

int
type_precision (type)
     register tree type;
{
  return ((TREE_CODE (type) == INTEGER_TYPE
	   || TREE_CODE (type) == ENUMERAL_TYPE
	   || TREE_CODE (type) == REAL_TYPE)
	  ? TYPE_PRECISION (type) : POINTER_SIZE);
}

/* Nonzero if integer constant C has a value that is permissible
   for type TYPE (an INTEGER_TYPE).  */

int
int_fits_type_p (c, type)
     tree c, type;
{
  if (TREE_UNSIGNED (type))
    return (!INT_CST_LT_UNSIGNED (TYPE_MAX_VALUE (type), c)
	    && !INT_CST_LT_UNSIGNED (c, TYPE_MIN_VALUE (type)));
  else
    return (!INT_CST_LT (TYPE_MAX_VALUE (type), c)
	    && !INT_CST_LT (c, TYPE_MIN_VALUE (type)));
}
