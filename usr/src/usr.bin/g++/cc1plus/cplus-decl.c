/* Process declarations and variables for C compiler.
   Copyright (C) 1988 Free Software Foundation, Inc.
   Hacked by Michael Tiemann (tiemann@mcc.com)

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


/* Process declarations and symbol lookup for C front end.
   Also constructs types; the standard scalar types at initialization,
   and structure, union, array and enum types when they are declared.  */

/* ??? not all decl nodes are given the most useful possible
   line numbers.  For example, the CONST_DECLs for enum values.  */

#include "config.h"
#include "tree.h"
#include "flags.h"
#include "cplus-tree.h"
#include "cplus-parse.h"
#include <signal.h>
#include "assert.h"
#include "obstack.h"
#include "rtl.h"
#include "insn-flags.h"

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int xmalloc ();
extern void free ();

/* Define this if C structs should have gratuitous typedefing
   done just like C++ structs do.  */
#define BREAK_C_TAGS

/* Stack of places to restore the search obstack back to.  */
   
/* Obstack used for remembering local class declarations (like
   enums and static (const) members.  */
#include "stack.h"
static struct obstack decl_obstack;
static struct stack_level *decl_stack;

#include "cplus-decl.h"

#define NULL 0
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

#ifndef CHAR_TYPE_SIZE
#define CHAR_TYPE_SIZE BITS_PER_UNIT
#endif

#ifndef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE (BITS_PER_UNIT * MIN ((UNITS_PER_WORD + 1) / 2, 2))
#endif

#ifndef INT_TYPE_SIZE
#define INT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_TYPE_SIZE
#define LONG_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef LONG_LONG_TYPE_SIZE
#define LONG_LONG_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef FLOAT_TYPE_SIZE
#define FLOAT_TYPE_SIZE BITS_PER_WORD
#endif

#ifndef DOUBLE_TYPE_SIZE
#define DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

#ifndef LONG_DOUBLE_TYPE_SIZE
#define LONG_DOUBLE_TYPE_SIZE (BITS_PER_WORD * 2)
#endif

static tree grokparms ();
tree grokdeclarator ();
tree pushdecl ();
void push_overloaded_decl ();
void pop_implicit_try_blocks ();

#define builtin_function(NAME, TYPE, CODE) \
  define_function (NAME, TYPE, CODE, (void (*)())&pushdecl)
#define auto_function(NAME, TYPE, CODE) \
  define_function (NAME, TYPE, CODE, (void (*)())&push_overloaded_decl)

/* static */ void grokclassfn ();
/* static */ tree grokopexpr (), grokoptypename ();

static tree lookup_tag ();
static tree lookup_tag_reverse ();
static tree lookup_name_current_level ();
static char *redeclaration_error_message ();
static int parmlist_is_exprlist ();
static int parmlist_is_random ();
static void grok_ctor_properties ();
static void grok_op_properties ();
static void expand_static_init ();
static void deactivate_exception_cleanups ();

tree finish_table ();
#ifdef FIELD_XREF
static void FIELD_end_scope();
#endif

/* a node which has tree code ERROR_MARK, and whose type is itself.
   All erroneous expressions are replaced with this node.  All functions
   that accept nodes as arguments should avoid generating error messages
   if this node is one of the arguments, since it is undesirable to get
   multiple error messages from one error in the input.  */

#undef error_mark_node
tree error_mark_node;
#define error_mark_node (&ERROR_MARK_NODE)

/* Erroneous argument lists can use this *IFF* they do not modify it.  */
tree error_mark_list;

/* INTEGER_TYPE and REAL_TYPE nodes for the standard data types */

tree short_integer_type_node;
tree integer_type_node;
tree long_integer_type_node;
tree long_long_integer_type_node;

tree short_unsigned_type_node;
tree unsigned_type_node;
tree long_unsigned_type_node;
tree long_long_unsigned_type_node;

tree unsigned_char_type_node;
tree signed_char_type_node;
tree char_type_node;

tree float_type_node;
tree double_type_node;
tree long_double_type_node;

/* a VOID_TYPE node, and the same, packaged in a TREE_LIST.  */

tree void_type_node, void_list_node;

/* A node for type `void *'.  */

tree ptr_type_node;

/* A node for type `char *'.  */

tree string_type_node;

/* Type `char[256]' or something like it.
   Used when an array of char is needed and the size is irrelevant.  */

tree char_array_type_node;

/* Type `int[256]' or something like it.
   Used when an array of int needed and the size is irrelevant.  */

tree int_array_type_node;

/* type `int ()' -- used for implicit declaration of functions.  */

tree default_function_type;

/* function types `double (double)' and `double (double, double)', etc.  */

tree double_ftype_double, double_ftype_double_double;
tree int_ftype_int, long_ftype_long;

/* Function type `void (void *, void *, int)' and similar ones.  */

tree void_ftype_ptr_ptr_int, int_ftype_ptr_ptr_int, void_ftype_ptr_int_int;

/* C++ extensions */
tree vtable_entry_type;
tree class_type_node, record_type_node, union_type_node, enum_type_node;
tree exception_type_node, unknown_type_node;

/* Function type `void * (long)', 'void (void *)' */
tree ptr_ftype_long, void_ftype_ptr;
tree ptr_ftype_ptr_int_int_ptr, void_ftype_ptr_int_int_ptr_int_int;

/* Used for virtual function tables.  */
tree vtbl_mask;

/* Array type `(void *)[]' */
tree vtbl_type_node;

#ifdef SOS
/* SOS extensions.  */
tree zlink_type, zret_type;
tree zlink, zret;
#endif

/* Static decls which do not have static initializers have no
   initializers as far as GNU C is concerned.  EMPTY_INIT_NODE
   is a static initializer which makes varasm code place the decl
   in data rather than in bss space.  Such gymnastics are necessary
   to avoid the problem that the linker will not include a library
   file if all the library appears to contribute are bss variables.  */

tree empty_init_node;

/* In a destructor, the point at which all derived class destroying
   has been done, just before any base class destroying will be done.  */

tree dtor_label;

/* In a constructor, the point at which we are ready to return
   the pointer to the initialized object.  */

tree ctor_label;

/* A FUNCTION_DECL which can call `unhandled_exception'.
   Not neccessarily the one that the user will declare,
   but sufficient to be called by routines that want to abort the program.  */

tree unhandled_exception_fndecl;

/* A FUNCTION_DECL which can call `abort'.  Not neccessarily the
   one that the user will declare, but sufficient to be called
   by routines that want to abort the program.  */

tree abort_fndecl;

/* -- end of C++ */

/* Two expressions that are constants with value zero.
   The first is of type `int', the second of type `void *'.  */

tree integer_zero_node;
tree null_pointer_node;

/* A node for the integer constants 1, 2, and 3.  */

tree integer_one_node, integer_two_node, integer_three_node;

/* An identifier whose name is <value>.  This is used as the "name"
   of the RESULT_DECLs for values of functions.  */

tree value_identifier;

/* If original DECL_RESULT of current function was a register,
   but due to being an addressable named return value, would up
   on the stack, this variable holds the named return value's
   original location.  */
struct rtx_def *original_result_rtx;

/* Sequence of insns which represents base initialization.  */
struct rtx_def *base_init_insns;

/* C++: Keep these around to reduce calls to `get_identifier'.
   Identifiers for `this' in member functions and the auto-delete
   parameter for destructors.  */
tree this_identifier, in_charge_identifier;

/* While defining an enum type, this is 1 plus the last enumerator
   constant value.  */

static tree enum_next_value;

/* Parsing a function declarator leaves a list of parameter names
   or a chain or parameter decls here.  */

tree last_function_parms;

/* Parsing a function declarator leaves here a chain of structure
   and enum types declared in the parmlist.  */

static tree last_function_parm_tags;

/* After parsing the declarator that starts a function definition,
   `start_function' puts here the list of parameter names or chain of decls.
   `store_parm_decls' finds it here.  */

static tree current_function_parms;

/* Similar, for last_function_parm_tags.  */
static tree current_function_parm_tags;

/* A list (chain of TREE_LIST nodes) of all LABEL_STMTs in the function
   that have names.  Here so we can clear out their names' definitions
   at the end of the function.  */

static tree named_labels;

/* A list (chain of TREE_LIST nodes) of named label uses.
   The TREE_PURPOSE field is the list of variables defined
   the the label's scope defined at the point of use.
   The TREE_VALUE field is the LABEL_DECL used.
   The TREE_TYPE field holds `current_binding_level' at the
   point of the label's use.

   Used only for jumps to as-yet undefined labels, since
   jumps to defined labels can have their validity checked
   by stmt.c.  */

static tree named_label_uses;

/* A list of objects which have constructors or destructors
   which reside in the global scope.  The decl is stored in
   the TREE_VALUE slot and the initializer is stored
   in the TREE_PURPOSE slot.  */
tree static_aggregates;

/* A list of functions which were declared inline, but later had their
   address taken.  Used only for non-virtual member functions, since we can
   find other functions easily enough.  */
tree pending_addressable_inlines;

/* A list of overloaded functions which we should forget ever
   existed, such as functions declared in a function's scope,
   once we leave that function's scope.  */
static tree overloads_to_forget;

/* The FUNCTION_DECL for the function currently being compiled,
   or 0 if between functions.  */
tree current_function_decl;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement that specifies a return value is seen.  */

int current_function_returns_value;

/* Set to 0 at beginning of a function definition, set to 1 if
   a return statement with no argument is seen.  */

int current_function_returns_null;

/* Set to nonzero by `grokdeclarator' for a function
   whose return type is defaulted, if warnings for this are desired.  */

static int warn_about_return_type;

/* Nonzero when starting a function delcared `extern inline'.  */

static int current_extern_inline;

char *language_string = "GNU C++";

/* Set to 0 at beginning of a constructor, set to 1
   if that function does an allocation before referencing its
   instance variable.  */
int current_function_assigns_this;
int current_function_just_assigned_this;

/* Set to 0 at beginning of a function.  Set non-zero when
   store_parm_decls is called.  Don't call store_parm_decls
   if this flag is non-zero!  */
int current_function_parms_stored;

/* Allocate a level of searching.  */
struct stack_level *
push_decl_level (stack, obstack)
     struct stack_level *stack;
     struct obstack *obstack;
{
  struct stack_level tem;
  tem.prev = stack;

  return push_stack_level (obstack, &tem, sizeof (tem));
}

/* Discard a level of decl allocation.  */

static struct stack_level *
pop_decl_level (stack)
     struct stack_level *stack;
{
  tree *bp, *tp;
  struct obstack *obstack = stack->obstack;
  bp = stack->first;
  tp = (tree *)obstack_next_free (obstack);
  while (tp != bp)
    {
      --tp;
      IDENTIFIER_CLASS_VALUE (DECL_NAME (*tp)) = NULL_TREE;
    }
  return pop_stack_level (stack);
}

/* For each binding contour we allocate a binding_level structure
 * which records the names defined in that contour.
 * Contours include:
 *  0) the global one
 *  1) one for each function definition,
 *     where internal declarations of the parameters appear.
 *  2) one for each compound statement,
 *     to record its declarations.
 *
 * The current meaning of a name can be found by searching the levels from
 * the current one out to the global one.
 *
 * Off to the side, may be the class_binding_level.  This exists
 * only to catch class-local declarations.  It is otherwise
 * nonexistent.
 * 
 * Also there may be binding levels that catch cleanups that
 * must be run when exceptions occur.
 */

/* Note that the information in the `names' component of the global contour
   is duplicated in the IDENTIFIER_GLOBAL_VALUEs of all identifiers.  */

struct binding_level
  {
    /* A chain of _DECL nodes for all variables, constants, functions,
     * and typedef types.  These are in the reverse of the order supplied.
     */
    tree names;

    /* A list of structure, union and enum definitions,
     * for looking up tag names.
     * It is a chain of TREE_LIST nodes, each of whose TREE_PURPOSE is a name,
     * or NULL_TREE; and whose TREE_VALUE is a RECORD_TYPE, UNION_TYPE,
     * or ENUMERAL_TYPE node.
     *
     * C++: the TREE_VALUE nodes can be simple types for component_bindings.
     *
     */
    tree tags;

    /* For each level, a list of shadowed outer-level local definitions
       to be restored when this level is popped.
       Each link is a TREE_LIST whose TREE_PURPOSE is an identifier and
       whose TREE_VALUE is its old definition (a kind of ..._DECL node).  */
    tree shadowed;

    /* Same, for IDENTIFIER_CLASS_VALUE.  */
    tree class_shadowed;

    /* For each level (except not the global one),
       a chain of LET_STMT nodes for all the levels
       that were entered and exited one level down.  */
    tree blocks;

    /* The binding level which this one is contained in (inherits from).  */
    struct binding_level *level_chain;

    /* Number of decls in `names' that have incomplete 
       structure or union types.  */
    unsigned short n_incomplete;

    /* 1 for the level that holds the parameters of a function.
       2 for the level that holds a class declaration.
       3 for levels that hold parameter declarations.  */
    unsigned parm_flag : 4;

    /* 1 means make a LET_STMT for this level regardless of all else.
       2 for temporary binding contours created by the compiler.  */
    unsigned keep : 3;

    /* Nonzero if this level "doesn't exist" for tags.  */
    unsigned tag_transparent : 1;

    /* Nonzero if this level can safely have additional
       cleanup-needing variables added to it.  */
    unsigned more_cleanups_ok : 1;
    unsigned have_cleanups : 1;

    /* Nonzero if this level can safely have additional
       exception-raising statements added to it.  */
    unsigned more_exceptions_ok : 1;
    unsigned have_exceptions : 1;

    /* Four bits left for this word.  */
  };

#define NULL_BINDING_LEVEL (struct binding_level *) NULL
  
/* The binding level currently in effect.  */

static struct binding_level *current_binding_level;

/* The binding level of the current class, if any.  */

static struct binding_level *class_binding_level;

/* A chain of binding_level structures awaiting reuse.  */

static struct binding_level *free_binding_level;

/* The outermost binding level, for names of file scope.
   This is created when the compiler is started and exists
   through the entire run.  */

static struct binding_level *global_binding_level;

/* Binding level structures are initialized by copying this one.  */

static struct binding_level clear_binding_level;

/* Nonzero means unconditionally make a LET_STMT for the next level pushed.  */

static int keep_next_level_flag;

#define PUSH_BINDING_LEVEL(NEWLEVEL, TAG_TRANSPARENT, KEEP) \
do {									\
  /* Add this level to the front of the chain (stack) of levels that	\
     are active.  */							\
  *NEWLEVEL = clear_binding_level;					\
  if (class_binding_level)						\
    {									\
      NEWLEVEL->level_chain = class_binding_level;			\
      class_binding_level = 0;						\
    }									\
  else									\
    {									\
      NEWLEVEL->level_chain = current_binding_level;			\
    }									\
  current_binding_level = NEWLEVEL;					\
  NEWLEVEL->tag_transparent = TAG_TRANSPARENT;				\
  NEWLEVEL->more_cleanups_ok = 1;					\
  NEWLEVEL->more_exceptions_ok = 1;					\
  NEWLEVEL->keep = KEEP;						\
} while (0)

#define POP_BINDING_LEVEL \
do {									\
  /* Pop the current level, and free the structure for reuse.  */	\
  {									\
    register struct binding_level *level = current_binding_level;	\
    current_binding_level = current_binding_level->level_chain;		\
    level->level_chain = free_binding_level;				\
    free_binding_level = level;						\
    if (current_binding_level->parm_flag == 2)				\
      {									\
	class_binding_level = current_binding_level;			\
	do								\
	  {								\
	    current_binding_level = current_binding_level->level_chain;	\
	  }								\
	while (current_binding_level->parm_flag == 2);			\
      }									\
  }									\
} while (0)

/* Create a new `struct binding_level'.  */

static
struct binding_level *
make_binding_level ()
{
  /* NOSTRICT */
  return (struct binding_level *) xmalloc (sizeof (struct binding_level));
}

/* Nonzero if we are currently in the global binding level.  */

int
global_bindings_p ()
{
  return current_binding_level == global_binding_level;
}

void
keep_next_level ()
{
  keep_next_level_flag = 1;
}

/* Identify this binding level as a level of parameters.  */

void
declare_parm_level ()
{
  current_binding_level->parm_flag = 1;
}

/* Identify this binding level as a level of a default exception handler.  */

void
declare_implicit_exception ()
{
  current_binding_level->parm_flag = 3;
}

/* Nonzero if current binding contour contains expressions
   that might raise exceptions.  */

int
have_exceptions_p ()
{
  return current_binding_level->have_exceptions;
}

/* Enter a new binding level.
   If TAG_TRANSPARENT is nonzero, do so only for the name space of variables,
   not for that of tags.  */

void
pushlevel (tag_transparent)
     int tag_transparent;
{
  register struct binding_level *newlevel = NULL_BINDING_LEVEL;

  /* If this is the top level of a function,
     just make sure that NAMED_LABELS is 0.
     They should have been set to 0 at the end of the previous function.  */

  if (current_binding_level == global_binding_level)
    assert (named_labels == NULL_TREE);

  /* Reuse or create a struct for this binding level.  */

  if (free_binding_level)
    {
      newlevel = free_binding_level;
      free_binding_level = free_binding_level->level_chain;
    }
  else
    {
      newlevel = make_binding_level ();
    }
  PUSH_BINDING_LEVEL (newlevel, tag_transparent, keep_next_level_flag);
#ifdef FIELD_XREF
  FIELD_xref_start_scope(newlevel);
#endif
  keep_next_level_flag = 0;
}

void
pushlevel_temporary (tag_transparent)
     int tag_transparent;
{
  pushlevel (tag_transparent);
  current_binding_level->keep = 2;
  clear_last_expr ();
#if 0
  /* Don't call push_momentary here!  It will cause cleanups
     to be allocated on the momentary obstack, and they
     will be overwritten by the next statement.  */
  push_momentary ();
#endif
  expand_start_bindings (0);
}

/* Exit a binding level.
   Pop the level off, and restore the state of the identifier-decl mappings
   that were in effect when this level was entered.

   If KEEP == 1, this level had explicit declarations, so
   and create a "block" (a LET_STMT node) for the level
   to record its declarations and subblocks for symbol table output.

   If KEEP == 2, this level's subblocks go to the front,
   not the back of the current binding level.  This happens,
   for instance, when code for constructors and destructors
   need to generate code at the end of a function which must
   be moved up to the front of the function.

   If FUNCTIONBODY is nonzero, this level is the body of a function,
   so create a block as if KEEP were set and also clear out all
   label names.

   If REVERSE is nonzero, reverse the order of decls before putting
   them into the LET_STMT.  */

tree
poplevel (keep, reverse, functionbody)
     int keep;
     int reverse;
     int functionbody;
{
  register tree link;
  /* The chain of decls was accumulated in reverse order.
     Put it into forward order, just for cleanliness.  */
  tree decls;
  int tmp = functionbody;
  int implicit_try_block = current_binding_level->parm_flag == 3;
  int real_functionbody = current_binding_level->keep == 2
    ? ((functionbody = 0), tmp) : functionbody;
  tree tags = functionbody >= 0 ? current_binding_level->tags : 0;
  tree subblocks = functionbody >= 0 ? current_binding_level->blocks : 0;
  tree block = 0;

#ifdef FIELD_XREF
  FIELD_xref_end_scope(current_binding_level,
		       current_binding_level->level_chain,
		       current_binding_level->parm_flag,
		       current_binding_level->keep,
		       current_binding_level->tag_transparent);
#endif

  if (current_binding_level->keep == 1)
    keep = 1;

  /* This warning is turned off because it causes warnings for
     declarations like `extern struct foo *x'.  */
#if 0
  /* Warn about incomplete structure types in this level.  */
  for (link = tags; link; link = TREE_CHAIN (link))
    if (TYPE_SIZE (TREE_VALUE (link)) == 0)
      {
	tree type = TREE_VALUE (link);
	char *errmsg;
	switch (TREE_CODE (type))
	  {
	  case RECORD_TYPE:
	    errmsg = "`struct %s' incomplete in scope ending here";
	    break;
	  case UNION_TYPE:
	    errmsg = "`union %s' incomplete in scope ending here";
	    break;
	  case ENUMERAL_TYPE:
	    errmsg = "`enum %s' incomplete in scope ending here";
	    break;
	  }
	if (TREE_CODE (TYPE_NAME (type)) == IDENTIFIER_NODE)
	  error (errmsg, IDENTIFIER_POINTER (TYPE_NAME (type)));
	else
	  /* If this type has a typedef-name, the TYPE_NAME is a TYPE_DECL.  */
	  error (errmsg, IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (type))));
      }
#endif /* 0 */

  /* Get the decls in the order they were written.
     Usually current_binding_level->names is in reverse order.
     But parameter decls were previously put in forward order.  */

  if (reverse)
    current_binding_level->names
      = decls = nreverse (current_binding_level->names);
  else
    decls = current_binding_level->names;

  /* If there were any declarations or structure tags in that level,
     or if this level is a function body,
     create a LET_STMT to record them for the life of this function.  */

  if (keep == 1 || functionbody > 0)
    block = build_let (0, 0, keep ? decls : 0,
		       subblocks, 0, keep ? tags : 0);

  /* In each subblock, record that this is its superior.  */

  if (keep >= 0)
    for (link = subblocks; link; link = TREE_CHAIN (link))
      STMT_SUPERCONTEXT (link) = block;

  /* Clear out the meanings of the local variables of this level;
     also record in each decl which block it belongs to.  */

  for (link = decls; link; link = TREE_CHAIN (link))
    {
      if (DECL_NAME (link) != 0)
	IDENTIFIER_LOCAL_VALUE (DECL_NAME (link)) = 0;
      DECL_CONTEXT (link) = block;
    }

  /* Restore all name-meanings of the outer levels
     that were shadowed by this level.  */

  for (link = current_binding_level->shadowed; link; link = TREE_CHAIN (link))
    IDENTIFIER_LOCAL_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);
  for (link = current_binding_level->class_shadowed;
       link;
       link = TREE_CHAIN (link))
    IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);

  /* If the level being exited is the top level of a function,
     check over all the labels.  */

  if (functionbody)
    {
      /* Clear out the definitions of all label names,
	 since their scopes end here.  */

      for (link = named_labels; link; link = TREE_CHAIN (link))
	{
	  if (DECL_SOURCE_LINE (TREE_VALUE (link)) == 0)
	    {
	      error ("label `%s' used somewhere above but not defined",
		     IDENTIFIER_POINTER (DECL_NAME (TREE_VALUE (link))));
	      /* Avoid crashing later.  */
	      define_label (input_filename, 1, DECL_NAME (TREE_VALUE (link)));
	    }
	  else if (warn_unused && !TREE_USED (TREE_VALUE (link)))
	    warning_with_decl (TREE_VALUE (link), 
			       "label `%s' defined but not used");
	  SET_IDENTIFIER_LABEL_VALUE (DECL_NAME (TREE_VALUE (link)), 0);
	}

      named_labels = 0;
    }

  /* Any uses of undefined labels now operate under constraints
     of next binding contour.  */
  {
    struct binding_level *level_chain;
    level_chain = current_binding_level->level_chain;
    if (level_chain)
      {
	tree labels;
	for (labels = named_label_uses; labels; labels = TREE_CHAIN (labels))
	  if (TREE_TYPE (labels) == (tree)current_binding_level)
	    {
	      TREE_TYPE (labels) = (tree)level_chain;
	      TREE_PURPOSE (labels) = level_chain->names;
	    }
      }
  }

  tmp = current_binding_level->keep;

  POP_BINDING_LEVEL;
  if (functionbody > 0)
    {
      DECL_INITIAL (current_function_decl) = block;
      /* If this is the top level block of a function,
	 the vars are the function's parameters.
	 Don't leave them in the LET_STMT because they are
	 found in the FUNCTION_DECL instead.  */
      STMT_VARS (block) = 0;
    }
  else if (block)
    current_binding_level->blocks
      = chainon (current_binding_level->blocks, block);
  /* If we did not make a block for the level just exited,
     any blocks made for inner levels
     (since they cannot be recorded as subblocks in that level)
     must be carried forward so they will later become subblocks
     of something else.  */
  else if (subblocks)
    if (keep == 2)
      current_binding_level->blocks = chainon (subblocks, current_binding_level->blocks);
    else
      current_binding_level->blocks
        = chainon (current_binding_level->blocks, subblocks);

  /* Take care of compiler's internal binding structures.  */
  if (tmp == 2 && !implicit_try_block)
    {
#if 0
      /* We did not call push_momentary for this
	 binding contour, so there is nothing to pop.  */
      pop_momentary ();
#endif
      expand_end_bindings (getdecls (), keep, 1);
      block = poplevel (keep, reverse, real_functionbody);
    }
  if (block)
    TREE_USED (block) = 1;
  return block;
}

/* Add BLOCK to the current list of blocks for this binding contour.  */
void
add_block_current_level (block)
     tree block;
{
  current_binding_level->blocks
    = chainon (current_binding_level->blocks, block);
}

/* Do a pushlevel for class declarations.  */
void
pushlevel_class ()
{
  pushlevel (0);
  decl_stack = push_decl_level (decl_stack, &decl_obstack);
  class_binding_level = current_binding_level;
  class_binding_level->parm_flag = 2;
  do
    {
      current_binding_level = current_binding_level->level_chain;
    }
  while (current_binding_level->parm_flag == 2);
}

/* ...and a poplevel for class declarations.  */
tree
poplevel_class ()
{
  register struct binding_level *level = class_binding_level;
  tree block = 0;
  tree shadowed;

  if (level == 0)
    {
      while (current_binding_level && class_binding_level == 0)
	block = poplevel (0, 0, 0);
      if (current_binding_level == 0)
	fatal ("syntax error too serious");
      level = class_binding_level;
    }
  decl_stack = pop_decl_level (decl_stack);
  for (shadowed = level->class_shadowed; shadowed; shadowed = TREE_CHAIN (shadowed))
    IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (shadowed)) = TREE_VALUE (shadowed);

#ifdef FIELD_XREF
  FIELD_xref_end_scope(class_binding_level,
		       class_binding_level->level_chain,
		       class_binding_level->parm_flag,
		       class_binding_level->keep,
		       class_binding_level->tag_transparent);
#endif

  class_binding_level = level->level_chain;

  level->level_chain = free_binding_level;
  free_binding_level = level;
  if (class_binding_level->parm_flag != 2)
    class_binding_level = 0;
  return block;
}

/* Push a definition of struct, union or enum tag "name".
   "type" should be the type node.
   We assume that the tag "name" is not already defined.

   Note that the definition may really be just a forward reference.
   In that case, the TYPE_SIZE will be zero.

   C++ gratuitously puts all these tags in the name space. */

void
pushtag (name, type)
     tree name, type;
{
  register struct binding_level *b;

  if (class_binding_level)
    b = class_binding_level;
  else
    {
      b = current_binding_level;
      while (b->tag_transparent) b = b->level_chain;
    }

  if (name)
    {
      /* Record the identifier as the type's name if it has none.  */

      if (TYPE_NAME (type) == 0)
        TYPE_NAME (type) = name;

      if (b == global_binding_level)
	b->tags = perm_tree_cons (name, type, b->tags);
      else
	b->tags = saveable_tree_cons (name, type, b->tags);

      /* Do C++ gratuitous typedefing.  Note that we put the
         TYPE_DECL in the TREE_TYPE of the IDENTIFIER_NODE.  */
      if (TREE_TYPE (name) != TYPE_NAME (type)
#ifndef BREAK_C_TAGS
	  /* This *should* only happen in C++ language scope.
	     But everybody else seems to think otherwise.  */
	  && current_lang_name == lang_name_cplusplus
#endif
	  && (TREE_CODE (type) != RECORD_TYPE
	      || class_binding_level == 0
	      || !CLASSTYPE_DECLARED_EXCEPTION (type)))
        {
          register tree t = build_decl (TYPE_DECL, name, type);
	  if (!TREE_NONLOCAL (type))
	    t = pushdecl (t);
          TYPE_NAME (type) = t;
          TREE_TYPE (name) = t;
        }
      if (b->parm_flag == 2)
	{
	  TREE_NONLOCAL (type) = 1;
	  IDENTIFIER_CLASS_VALUE (name) = TYPE_NAME (type);
	  CLASSTYPE_TAGS (current_class_type) = b->tags;
	}
    }
}

/* Subroutine of duplicate_decls: return truthvalue of whether
   or not types of these decls match.  */
static int
decls_match (newdecl, olddecl)
     tree newdecl, olddecl;
{
  int types_match;

  if (TREE_CODE (newdecl) == FUNCTION_DECL && TREE_CODE (olddecl) == FUNCTION_DECL)
    {
      tree f1 = TREE_TYPE (newdecl);
      tree f2 = TREE_TYPE (olddecl);
      tree p1 = TYPE_ARG_TYPES (f1);
      tree p2 = TYPE_ARG_TYPES (f2);

      /* When we parse a static member function definition,
	 we put together a FUNCTION_DECL which thinks its type
	 is METHOD_TYPE.  Change that to FUNCTION_TYPE, and
	 proceed.  */
      if (TREE_CODE (f1) == METHOD_TYPE
	  && DECL_STATIC_FUNCTION_P (olddecl))
	{
	  tree n1;
	  p1 = TREE_CHAIN (p1);
	  n1 = build_function_type (TREE_TYPE (f1), p1);
	  n1 = build_type_variant (n1, TREE_READONLY (f1), TREE_VOLATILE (f1));
	  n1 = build_exception_variant (TYPE_METHOD_BASETYPE (f1), n1, TYPE_RAISES_EXCEPTIONS (f1));
	  TREE_TYPE (newdecl) = n1;
	  f1 = n1;
	  DECL_STATIC_FUNCTION_P (newdecl) = 1;
	}
      /* Here we must take care of the case where new default
	 parameters are specified.  Also, warn if an old
	 declaration becomes ambiguous because default
	 parameters may cause the two to be ambiguous.  */
      if (TREE_CODE (f1) != TREE_CODE (f2))
	{
	  if (TREE_CODE (f1) == OFFSET_TYPE)
	    compiler_error_with_decl (newdecl, "`%s' redeclared as member function");
	  else
	    compiler_error_with_decl (newdecl, "`%s' redeclared as non-member function");
	  return 0;
	}

      if (comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (f1)),
		     TYPE_MAIN_VARIANT (TREE_TYPE (f2)), 1))
	{
	  types_match = compparms (p1, p2, 1);
#ifndef MERGED
	  /* C++: copy friendlist *before* we get smooshed.  */
	  if (DECL_FRIENDLIST (olddecl) && !DECL_FRIENDLIST (newdecl))
	    DECL_FRIENDLIST (newdecl) = DECL_FRIENDLIST (olddecl);
#endif
	}
      else types_match = 0;
    }
  else
    {
      if (TREE_TYPE (newdecl) == error_mark_node)
	types_match = TREE_TYPE (olddecl) == error_mark_node;
      else
	types_match = comptypes (TREE_TYPE (newdecl), TREE_TYPE (olddecl), 1);
    }

  return types_match;
}

/* Handle when a new declaration NEWDECL has the same name as an old
   one OLDDECL in the same binding contour.  Prints an error message
   if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return 1.
   Otherwise, return 0.  */

static int
duplicate_decls (newdecl, olddecl)
     register tree newdecl, olddecl;
{
  extern struct obstack permanent_obstack;
  int types_match;
  int new_is_definition;

  if (TREE_CODE (olddecl) == TREE_LIST
      && TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* If a new decl finds a list of old decls, then
	 we assume that the new decl has C linkage, and
	 that the old decls have C++ linkage.  In this case,
	 we must look through the list to see whether
	 there is an ambiguity or not.  */
      tree olddecls = olddecl;

      /* If the overload list is empty, just install the decl.  */
      if (TREE_VALUE (olddecls) == NULL_TREE)
	{
	  TREE_VALUE (olddecls) = newdecl;
	  return 1;
	}

      while (olddecls)
	{
	  if (decls_match (newdecl, TREE_VALUE (olddecls)))
	    {
	      if (TREE_CODE (newdecl) == VAR_DECL)
		;
	      else if (DECL_LANGUAGE (newdecl)
		       != DECL_LANGUAGE (TREE_VALUE (olddecls)))
		{
		  error_with_decl (newdecl, "declaration of `%s' with different language linkage");
		  error_with_decl (TREE_VALUE (olddecls), "previous declaration here");
		}
	      types_match = 1;
	      break;
	    }
	  olddecls = TREE_CHAIN (olddecls);
	}
      if (olddecls)
	olddecl = TREE_VALUE (olddecl);
      else
	return 1;
    }
  else
    types_match = decls_match (newdecl, olddecl);

  if (TREE_CODE (TREE_TYPE (newdecl)) == ERROR_MARK
      || TREE_CODE (TREE_TYPE (olddecl)) == ERROR_MARK)
    types_match = 0;

  /* If this decl has linkage, and the old one does too, maybe no error.  */
  if (TREE_CODE (olddecl) != TREE_CODE (newdecl))
    {
      error_with_decl (newdecl, "`%s' redeclared as different kind of symbol");
      if (TREE_CODE (olddecl) == TREE_LIST)
	olddecl = TREE_VALUE (olddecl);
      error_with_decl (olddecl, "previous declaration of `%s'");

      /* New decl is completely inconsistent with the old one =>
	 tell caller to replace the old one.  */

      return 0;
    }

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* Now that functions must hold information normally held
	 by field decls, there is extra work to do so that
	 declaration information does not get destroyed during
	 definition.  */
      if (DECL_VINDEX (olddecl) && ! DECL_VINDEX (newdecl))
	DECL_VINDEX (newdecl) = DECL_VINDEX (olddecl);
      if (DECL_VCONTEXT (olddecl) && ! DECL_VCONTEXT (newdecl))
	DECL_VCONTEXT (newdecl) = DECL_VCONTEXT (olddecl);
      if (DECL_FIELD_CONTEXT (olddecl) && ! DECL_FIELD_CONTEXT (newdecl))
	DECL_FIELD_CONTEXT (newdecl) = DECL_FIELD_CONTEXT (olddecl);
#ifdef SOS
      if (DECL_DINDEX (olddecl) && ! DECL_DINDEX (newdecl))
	DECL_DINDEX (newdecl) = DECL_DINDEX (newdecl);
#endif
      if (DECL_PENDING_INLINE_INFO (newdecl) == 0)
	DECL_PENDING_INLINE_INFO (newdecl) = DECL_PENDING_INLINE_INFO (olddecl);
    }

  if (flag_traditional && TREE_CODE (newdecl) == FUNCTION_DECL
      && IDENTIFIER_IMPLICIT_DECL (DECL_NAME (newdecl)) == olddecl)
    /* If -traditional, avoid error for redeclaring fcn
       after implicit decl.  */
    ;
  else if (TREE_CODE (olddecl) == FUNCTION_DECL
	   && DECL_FUNCTION_CODE (olddecl) != NOT_BUILT_IN)
    {
      if (!types_match)
	error_with_decl (newdecl, "conflicting types for built-in function `%s'");
      else if (extra_warnings)
	warning_with_decl (newdecl, "built-in function `%s' redeclared");
    }
  else if (!types_match)
    {
      tree oldtype = TREE_TYPE (olddecl);
      tree newtype = TREE_TYPE (newdecl);
      int give_error = 0;

      /* Already complained about this, so don't do so again.  */
      if (current_class_type == NULL_TREE
	  || IDENTIFIER_ERROR_LOCUS (DECL_NAME (newdecl)) != current_class_type)
	{
	  give_error = 1;
	  error_with_decl (newdecl, "conflicting types for `%s'");
	}

      /* Check for function type mismatch
	 involving an empty arglist vs a nonempty one.  */
      if (TREE_CODE (olddecl) == FUNCTION_DECL
	  && comptypes (TREE_TYPE (oldtype),
			TREE_TYPE (newtype), 1)
	  && ((TYPE_ARG_TYPES (oldtype) == 0
	       && DECL_INITIAL (olddecl) == 0)
	      || (TYPE_ARG_TYPES (newtype) == 0
		  && DECL_INITIAL (newdecl) == 0)))
	{
	  /* Classify the problem further.  */
	  register tree t = TYPE_ARG_TYPES (oldtype);
	  if (t == 0)
	    t = TYPE_ARG_TYPES (newtype);
	  for (; t; t = TREE_CHAIN (t))
	    {
	      register tree type = TREE_VALUE (t);

	      if (TREE_CHAIN (t) == 0 && type != void_type_node)
		{
		  error ("A parameter list with an ellipsis can't match");
		  error ("an empty parameter name list declaration.");
		  break;
		}

	      if (type == float_type_node
		  || (TREE_CODE (type) == INTEGER_TYPE
		      && (TYPE_PRECISION (type)
			  < TYPE_PRECISION (integer_type_node))))
		{
		  error ("An argument type that has a default promotion");
		  error ("can't match an empty parameter name list declaration.");
		  break;
		}
	    }
	}
      if (give_error)
	error_with_decl (olddecl, "previous declaration of `%s'");

      /* There is one thing GNU C++ cannot tolerate: a constructor
	 which takes the type of object being constructed.
	 Farm that case out here.  */
      if (TREE_CODE (newdecl) == FUNCTION_DECL
	  && DECL_CONSTRUCTOR_P (newdecl))
	{
	  tree tmp = TREE_CHAIN (TYPE_ARG_TYPES (newtype));

	  if (tmp != NULL_TREE
	      && (TYPE_MAIN_VARIANT (TREE_VALUE (tmp))
		  == TYPE_METHOD_BASETYPE (newtype)))
	    {
	      tree parm = TREE_CHAIN (DECL_ARGUMENTS (newdecl));
	      tree argtypes
		= hash_tree_chain (build_reference_type (TREE_VALUE (tmp)),
				   TREE_CHAIN (tmp));

	      DECL_ARG_TYPE (parm)
		= TREE_TYPE (parm)
		  = TYPE_REFERENCE_TO (TREE_VALUE (tmp));

	      TREE_TYPE (newdecl) = newtype
		= build_cplus_method_type (TYPE_METHOD_BASETYPE (newtype),
					   TREE_TYPE (newtype), argtypes);
	      error ("constructor cannot take as argument the type being constructed");
	      SET_IDENTIFIER_ERROR_LOCUS (DECL_NAME (newdecl), current_class_type);
	    }
	}
    }
  else
    {
      char *errmsg = redeclaration_error_message (newdecl, olddecl);
      if (errmsg)
	{
	  error_with_decl (newdecl, errmsg);
	  if (DECL_LANG_SPECIFIC (olddecl)
	      && DECL_COMPILER_GENERATED_P (olddecl))
	    DECL_COMPILER_GENERATED_P (olddecl) = 0;
	  else
	    error_with_decl (olddecl,
			     "here is the previous declaration of `%s'");
	}
      else if (TREE_CODE (olddecl) == FUNCTION_DECL
	       && DECL_INITIAL (olddecl) != 0
	       && TYPE_ARG_TYPES (TREE_TYPE (olddecl)) == 0
	       && TYPE_ARG_TYPES (TREE_TYPE (newdecl)) != 0)
	{
	  /* Prototype decl follows defn w/o prototype.  */
	  warning_with_decl (newdecl, "prototype for `%s'");
	  warning_with_decl (olddecl,
			     "follows non-prototype definition here");
	}

      /* These bits are logically part of the type.  */
      if (pedantic
	  && (TREE_READONLY (newdecl) != TREE_READONLY (olddecl)
	      || TREE_THIS_VOLATILE (newdecl) != TREE_THIS_VOLATILE (olddecl)))
	error_with_decl (newdecl, "type qualifiers for `%s' conflict with previous decl");
    }

  /* Deal with C++: must preserve virtual function table size.  */
  if (TREE_CODE (olddecl) == TYPE_DECL)
    {
      if (TYPE_LANG_SPECIFIC (TREE_TYPE (newdecl))
          && TYPE_LANG_SPECIFIC (TREE_TYPE (olddecl)))
	{
	  CLASSTYPE_VSIZE (TREE_TYPE (newdecl))
	    = CLASSTYPE_VSIZE (TREE_TYPE (olddecl));
	  CLASSTYPE_FRIEND_CLASSES (TREE_TYPE (newdecl))
	    = CLASSTYPE_FRIEND_CLASSES (TREE_TYPE (olddecl));
	}
    }

  new_is_definition = (TREE_CODE (newdecl) == FUNCTION_DECL
		       && DECL_INITIAL (newdecl) != 0);

  /* Copy all the DECL_... slots specified in the new decl
     except for any that we copy here from the old type.  */

  if (types_match)
    {
      /* Automatically handles default parameters.  */
      tree oldtype = TREE_TYPE (olddecl);
      /* Merge the data types specified in the two decls.  */
      tree newtype = commontype (TREE_TYPE (newdecl), TREE_TYPE (olddecl));

      if (TREE_CODE (newdecl) == VAR_DECL)
	DECL_EXTERNAL (newdecl) |= DECL_EXTERNAL (olddecl);
      /* Do this after calling `commontype' so that default
	 parameters don't confuse us.  */
      else if (TREE_CODE (newdecl) == FUNCTION_DECL
	  && (TYPE_RAISES_EXCEPTIONS (TREE_TYPE (newdecl))
	      != TYPE_RAISES_EXCEPTIONS (TREE_TYPE (olddecl))))
	{
	  tree ctype = NULL_TREE;
	  if (TREE_CODE (newtype) == METHOD_TYPE)
	    ctype = TYPE_METHOD_BASETYPE (newtype);
	  else if (DECL_STATIC_FUNCTION_P (newdecl))
	    ctype = DECL_STATIC_CONTEXT (newdecl);
	  TREE_TYPE (newdecl) = build_exception_variant (ctype, newtype,
							 TYPE_RAISES_EXCEPTIONS (TREE_TYPE (newdecl)));
	  TREE_TYPE (olddecl) = build_exception_variant (ctype, newtype,
							 TYPE_RAISES_EXCEPTIONS (oldtype));

	  if (! compexcepttypes (TREE_TYPE (newdecl), TREE_TYPE (olddecl)))
	    {
	      error_with_decl (newdecl, "declaration of `%s' raises different exceptions...");
	      error_with_decl (olddecl, "...from previous declaration here");
	    }
	}
      TREE_TYPE (newdecl) = TREE_TYPE (olddecl) = newtype;

      /* Lay the type out, unless already done.  */
      if (oldtype != TREE_TYPE (newdecl))
	{
	  if (TREE_TYPE (newdecl) != error_mark_node)
	    layout_type (TREE_TYPE (newdecl));
	  if (TREE_CODE (newdecl) != FUNCTION_DECL
	      && TREE_CODE (newdecl) != TYPE_DECL
	      && TREE_CODE (newdecl) != CONST_DECL)
	    layout_decl (newdecl, 0);
	}
      else
	{
	  /* Since the type is OLDDECL's, make OLDDECL's size go with.  */
	  DECL_SIZE (newdecl) = DECL_SIZE (olddecl);
	  DECL_SIZE_UNIT (newdecl) = DECL_SIZE_UNIT (olddecl);
	  if (DECL_ALIGN (olddecl) > DECL_ALIGN (newdecl))
	    DECL_ALIGN (newdecl) = DECL_ALIGN (olddecl);
	}

      /* Merge the type qualifiers.  */
      if (TREE_READONLY (newdecl))
	TREE_READONLY (olddecl) = 1;
      if (TREE_THIS_VOLATILE (newdecl))
	TREE_THIS_VOLATILE (olddecl) = 1;

      /* Merge the initialization information.  */
      if (DECL_INITIAL (newdecl) == 0)
	DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
      /* Keep the old rtl since we can safely use it.  */
      DECL_RTL (newdecl) = DECL_RTL (olddecl);
    }
  /* If cannot merge, then use the new type and qualifiers,
     and don't preserve the old rtl.  */
  else
    {
      /* Clean out any memory we had of the old declaration.  */
      tree oldstatic = value_member (olddecl, static_aggregates);
      if (oldstatic)
	TREE_VALUE (oldstatic) = error_mark_node;

      TREE_TYPE (olddecl) = TREE_TYPE (newdecl);
      TREE_READONLY (olddecl) = TREE_READONLY (newdecl);
      TREE_THIS_VOLATILE (olddecl) = TREE_THIS_VOLATILE (newdecl);
      TREE_VOLATILE (olddecl) = TREE_VOLATILE (newdecl);
    }

  /* Merge the storage class information.  */
  if (TREE_EXTERNAL (newdecl))
    {
      TREE_STATIC (newdecl) = TREE_STATIC (olddecl);
      TREE_EXTERNAL (newdecl) = TREE_EXTERNAL (olddecl);

      /* For functions, static overrides non-static.  */
      if (TREE_CODE (newdecl) == FUNCTION_DECL)
	{
	  TREE_PUBLIC (newdecl) &= TREE_PUBLIC (olddecl);
	  /* This is since we don't automatically
	     copy the attributes of NEWDECL into OLDDECL.  */
	  TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
	  /* If this clears `static', clear it in the identifier too.  */
	  if (! TREE_PUBLIC (olddecl))
	    TREE_PUBLIC (DECL_NAME (olddecl)) = 0;
	}
      else
	TREE_PUBLIC (newdecl) = TREE_PUBLIC (olddecl);
    }
  else
    {
      TREE_STATIC (olddecl) = TREE_STATIC (newdecl);
      TREE_EXTERNAL (olddecl) = 0;
      /* A `const' which was not declared `extern' and is
	 in static storage is invisible.  */
      if (TREE_CODE (newdecl) == VAR_DECL
	  && TREE_READONLY (newdecl) && TREE_STATIC (newdecl)
	  && ! DECL_EXTERNAL (newdecl))
	TREE_PUBLIC (newdecl) = 0;
      TREE_PUBLIC (olddecl) = TREE_PUBLIC (newdecl);
    }
  /* If either decl says `inline', this fn is inline,
     unless its definition was passed already.  */
  TREE_INLINE (olddecl) |= TREE_INLINE (newdecl);

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      if (new_is_definition)
	/* If defining a function declared with other language
	   linkage, use the previously declared language linkage.  */
	DECL_LANGUAGE (newdecl) = DECL_LANGUAGE (olddecl);
      else
	{
	  /* If redeclaring a builtin function, and not a definition,
	     it stays built in.  */
	  DECL_SET_FUNCTION_CODE (newdecl, DECL_FUNCTION_CODE (olddecl));
	  DECL_RESULT (newdecl) = DECL_RESULT (olddecl);
	  if (DECL_SAVED_INSNS (newdecl) = DECL_SAVED_INSNS (olddecl))
	    /* Previously saved insns go together with
	       the function's previous definition.  */
	    DECL_INITIAL (newdecl) = DECL_INITIAL (olddecl);
	  DECL_RESULT_TYPE (newdecl) = DECL_RESULT_TYPE (olddecl);
	  DECL_ARGUMENTS (newdecl) = DECL_ARGUMENTS (olddecl);
	}
    }

  /* Now preserve various other info from the definition.  */
  TREE_ADDRESSABLE (newdecl) = TREE_ADDRESSABLE (olddecl);
  TREE_ASM_WRITTEN (newdecl) = TREE_ASM_WRITTEN (olddecl);

  /* Don't really know how much of the language-specific
     values we should copy from old to new.  */
#if 1
  if (DECL_LANG_SPECIFIC (olddecl))
    DECL_IN_AGGR_P (newdecl) = DECL_IN_AGGR_P (olddecl);
#endif

  if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      int function_size;
      struct lang_decl *ol = DECL_LANG_SPECIFIC (olddecl);
      struct lang_decl *nl = DECL_LANG_SPECIFIC (newdecl);

#ifdef MERGED
      function_size = sizeof (struct tree_decl);
#else
      function_size = sizeof (struct tree_function_decl);
#endif

      /* Don't lose track of having output OLDDECL as GDB symbol.  */
      DECL_BLOCK_SYMTAB_ADDRESS (newdecl)
	= DECL_BLOCK_SYMTAB_ADDRESS (olddecl);

      bcopy ((char *) newdecl + sizeof (struct tree_common),
	     (char *) olddecl + sizeof (struct tree_common),
	     function_size - sizeof (struct tree_common));

      if ((char *)newdecl == obstack_next_free (&permanent_obstack)
	  - (function_size + sizeof (struct lang_decl)))
	{
	  DECL_MAIN_VARIANT (newdecl) = olddecl;
	  DECL_LANG_SPECIFIC (olddecl) = ol;
	  bcopy (nl, ol, sizeof (struct lang_decl));

	  obstack_free (&permanent_obstack, newdecl);
	}
#ifdef LANG_DECL_PERMANENT
      else if (LANG_DECL_PERMANENT (ol))
	{
	  if (DECL_MAIN_VARIANT (olddecl) == olddecl)
	    {
	      /* Save these lang_decls that would otherwise be lost.  */
	      extern tree free_lang_decl_chain;
	      tree free_lang_decl = (tree) ol;
	      TREE_CHAIN (free_lang_decl) = free_lang_decl_chain;
	      free_lang_decl_chain = free_lang_decl;
	    }
	  else
	    {
	      /* Storage leak.  */
	    }
	}
#else
      /* Storage leak.  */
#endif
    }
  else
    {
      bcopy ((char *) newdecl + sizeof (struct tree_common),
	     (char *) olddecl + sizeof (struct tree_common),
	     sizeof (struct tree_decl) - sizeof (struct tree_common)
	     + tree_code_length [(int)TREE_CODE (newdecl)] * sizeof (char *));
    }

  return 1;
}

/* Record a decl-node X as belonging to the current lexical scope.
   Check for errors (such as an incompatible declaration for the same
   name already seen in the same scope).

   Returns either X or an old decl for the same name.
   If an old decl is returned, it may have been smashed
   to agree with what X says.  */

tree
pushdecl (x)
     tree x;
{
  register tree t;
  register tree name = DECL_NAME (x);
  register struct binding_level *b = current_binding_level;

  if (name)
    {
      char *file;
      int line;

      t = lookup_name_current_level (name);
      if (t != 0 && t == error_mark_node)
	/* error_mark_node is 0 for a while during initialization!  */
	{
	  t = 0;
	  error_with_decl (x, "`%s' used prior to declaration");
	}

      if (t != 0)
	{
	  tree cntxt = t;
	  if (TREE_CODE (t) == PARM_DECL)
	    {
	      if (DECL_CONTEXT (t) == NULL_TREE)
		fatal ("parse errors have confused me too much");
	      cntxt = DECL_CONTEXT (t);
	    }
	  file = DECL_SOURCE_FILE (t);
	  line = DECL_SOURCE_LINE (t);
	}

      if (t != 0 && TREE_CODE (t) != TREE_CODE (x))
	{
	  if (TREE_CODE (t) == TYPE_DECL)
	    {
#ifdef BREAK_C_TAGS
	      ;
#else
	      warning ("type declaration of %s shadowed",
		       IDENTIFIER_POINTER (name));
#endif
	    }
	  else if (TREE_CODE (x) == TYPE_DECL)
	    {
#ifdef BREAK_C_TAGS
	      ;
#else
	      warning ("type declaration of %s shadows previous declaration",
		       IDENTIFIER_POINTER (name));
#endif
	    }
	  else if (duplicate_decls (x, t))
	    return t;
	}
      else if (t != 0 && duplicate_decls (x, t))
	{
	  /* If this decl is `static' and an `extern' was seen previously,
	     that is erroneous.  But don't complain if -traditional,
	     since traditional compilers don't complain.

	     Note that this does not apply to the C++ case of declaring
	     a variable `extern const' and then later `const'.  */
	  if (!flag_traditional && TREE_PUBLIC (name)
	      && ! TREE_PUBLIC (x) && ! TREE_EXTERNAL (x) && ! TREE_INLINE (x))
	    {
	      /* Due to interference in memory reclamation (X may be
		 obstack-deallocated at this point, we must guard against
		 one really special case).  */
	      if (current_function_decl == x)
		current_function_decl = t;
	      if (IDENTIFIER_IMPLICIT_DECL (name))
		warning ("`%s' was declared implicitly `extern' and later `static'",
			 lang_printable_name (t));
	      else
		warning ("`%s' was declared `extern' and later `static'",
			 lang_printable_name (t));
	      warning_with_file_and_line (file, line,
					  "previous declaration of `%s'",
					  lang_printable_name (t));
	    }
	  return t;
	}

      /* If declaring a type as a typedef, and the type has no known
	 typedef name, install this TYPE_DECL as its typedef name.

	 C++: If it had an anonymous aggregate or enum name,
	 give it a `better' one.  */
      if (TREE_CODE (x) == TYPE_DECL)
	{
	  tree name = TYPE_NAME (TREE_TYPE (x));

	  if (name == NULL_TREE
	      || (TREE_CODE (name) != TYPE_DECL
#ifndef BREAK_C_TAGS
		  && current_lang_name == lang_name_cplusplus
#endif
		  ))
	    {
	      /* If these are different names, make two equivalent
		 definitions.  */
	      TYPE_NAME (TREE_TYPE (x)) = x;
	    }
	  else
	    {
	      if (TREE_CODE (name) == TYPE_DECL)
		name = DECL_NAME (name);
	      if (ANON_AGGRNAME_P (name))
		{
		  /* do gratuitous C++ typedefing, and make sure that
		     we access this type either through TREE_TYPE field
		     or via the tags list.  */
		  TYPE_NAME (TREE_TYPE (x)) = x;
		  pushtag (name, TREE_TYPE (x));
		}
	    }
	}

      /* Multiple external decls of the same identifier ought to match.  */

      if (TREE_EXTERNAL (x) && IDENTIFIER_GLOBAL_VALUE (name) != 0
	  && (TREE_EXTERNAL (IDENTIFIER_GLOBAL_VALUE (name))
	      || TREE_PUBLIC (IDENTIFIER_GLOBAL_VALUE (name))))
	{
	  if (! comptypes (TREE_TYPE (x),
			   TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (name)), 1))
	    {
	      warning_with_decl (x,
				 "type mismatch with previous external decl");
	      warning_with_decl (IDENTIFIER_GLOBAL_VALUE (name),
				 "previous external decl of `%s'");
	    }
	}

      /* In PCC-compatibility mode, extern decls of vars with no current decl
	 take effect at top level no matter where they are.  */
      if (flag_traditional && TREE_EXTERNAL (x)
	  && lookup_name (name) == 0)
	b = global_binding_level;

      /* This name is new in its binding level.
	 Install the new declaration and return it.  */
      if (b == global_binding_level)
	{
	  /* Install a global value.  */

	  /* Rule for VAR_DECLs, but not for other kinds of _DECLs:
	     A `const' which was not declared `extern' is invisible.  */
	  if (TREE_CODE (x) == VAR_DECL
	      && TREE_READONLY (x) && ! DECL_EXTERNAL (x))
	    TREE_PUBLIC (x) = 0;

	  /* If the first global decl has external linkage,
	     warn if we later see static one.  */
	  if (IDENTIFIER_GLOBAL_VALUE (name) == 0 && TREE_PUBLIC (x))
	    TREE_PUBLIC (name) = 1;

	  IDENTIFIER_GLOBAL_VALUE (name) = x;

	  /* Don't forget if the function was used via an implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_USED (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_USED (x) = 1;

	  /* Don't forget if its address was taken in that way.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name)
	      && TREE_ADDRESSABLE (IDENTIFIER_IMPLICIT_DECL (name)))
	    TREE_ADDRESSABLE (x) = 1;

	  /* Warn about mismatches against previous implicit decl.  */
	  if (IDENTIFIER_IMPLICIT_DECL (name) != 0
	      /* If this real decl matches the implicit, don't complain.  */
	      && ! (TREE_CODE (x) == FUNCTION_DECL
		    && TREE_TYPE (TREE_TYPE (x)) == integer_type_node))
	    warning ("`%s' was previously implicitly declared to return `int'",
		     lang_printable_name (x));

	  /* If this decl is `static' and an `extern' was seen previously,
	     that is erroneous.  */
	  if (TREE_PUBLIC (name)
	      && ! TREE_PUBLIC (x) && ! TREE_EXTERNAL (x))
	    {
	      if (IDENTIFIER_IMPLICIT_DECL (name))
		warning ("`%s' was declared implicitly `extern' and later `static'",
			 lang_printable_name (x));
	      else
		warning ("`%s' was declared `extern' and later `static'",
			 lang_printable_name (x));
	    }
	}
      else
	{
	  /* Here to install a non-global value.  */
	  tree oldlocal = IDENTIFIER_LOCAL_VALUE (name);
	  tree oldglobal = IDENTIFIER_GLOBAL_VALUE (name);
	  IDENTIFIER_LOCAL_VALUE (name) = x;

	  /* If this is an extern function declaration, see if we
	     have a global definition for the function.  */
	  if (oldlocal == 0
	      && oldglobal != 0
	      && TREE_CODE (x) == FUNCTION_DECL
	      && TREE_CODE (oldglobal) == FUNCTION_DECL)
	    {
	      /* We have one.  Their types must agree.  */
	      if (! comptypes (TREE_TYPE (x), TREE_TYPE (oldglobal), 1))
		warning_with_decl (x, "local declaration of `%s' doesn't match global one");
	      /* If the global one is inline, make the local one inline.  */
	      else if (TREE_INLINE (oldglobal)
		       || DECL_FUNCTION_CODE (oldglobal) != NOT_BUILT_IN
		       || (TYPE_ARG_TYPES (TREE_TYPE (oldglobal)) != 0
			   && TYPE_ARG_TYPES (TREE_TYPE (x)) == 0))
		IDENTIFIER_LOCAL_VALUE (name) = oldglobal;
	    }
	  /* If we have a local external declaration,
	     and no file-scope declaration has yet been seen,
	     then if we later have a file-scope decl it must not be static.  */
	  if (oldlocal == 0
	      && oldglobal == 0
	      && TREE_EXTERNAL (x)
	      && TREE_PUBLIC (x))
	    {
	      TREE_PUBLIC (name) = 1;
	    }

	  if (TREE_INLINE (x))
	    /* Inline decls shadow nothing.  */;

	  /* Warn if shadowing an argument at the top level of the body.  */
	  else if (oldlocal != 0 && !TREE_EXTERNAL (x)
	      && TREE_CODE (oldlocal) == PARM_DECL
	      && TREE_CODE (x) != PARM_DECL
	      /* The parm level is two levels above the first user-visible
		 level.  One level was created for parm cleanups, the
		 other declared by the user.  */
	      && current_binding_level->level_chain->level_chain->parm_flag == 1)
	    warning ("declaration of `%s' shadows a parameter",
		     IDENTIFIER_POINTER (name));

	  /* Maybe warn if shadowing something else.  */
	  else if (warn_shadow && !TREE_EXTERNAL (x))
	    {
	      char *warnstring = 0;

	      if (oldlocal != 0 && TREE_CODE (oldlocal) == PARM_DECL)
		warnstring = "declaration of `%s' shadows a parameter";
	      else if (IDENTIFIER_CLASS_VALUE (name) != 0)
		warnstring = "declaration of `%s' shadows a member of `this'";
	      else if (oldlocal != 0)
		warnstring = "declaration of `%s' shadows previous local";
	      else if (oldglobal != 0)
		warnstring = "declaration of `%s' shadows global declaration";

	      if (warnstring)
		warning (warnstring, IDENTIFIER_POINTER (name));
	    }

	  /* If storing a local value, there may already be one (inherited).
	     If so, record it for restoration when this binding level ends.  */
	  if (oldlocal != 0)
	    b->shadowed = tree_cons (name, oldlocal, b->shadowed);
	}

      /* Keep count of variables in this level with incomplete type.  */
      if (TYPE_SIZE (TREE_TYPE (x)) == 0
	  && (IS_AGGR_TYPE (TREE_TYPE (x))
	      || (TREE_CODE (TREE_TYPE (x)) == ARRAY_TYPE
		  && IS_AGGR_TYPE (TREE_TYPE (TREE_TYPE (x))))))
	{
	  if (++b->n_incomplete == 0)
	    error ("too many incomplete variables at this point");
	}
    }

  /* Put decls on list in reverse order.
     We will reverse them later if necessary.  */
  TREE_CHAIN (x) = b->names;
  b->names = x;

  return x;
}

/* Like pushdecl, only it places X in GLOBAL_BINDING_LEVEL,
   if appropriate.  */
tree
pushdecl_top_level (x)
     tree x;
{
  register tree t;
  register struct binding_level *b = current_binding_level;

  current_binding_level = global_binding_level;
  t = pushdecl (x);
  current_binding_level = b;
  return t;
}

/* Make the declaration of X appear in CLASS scope.  */
tree
pushdecl_class_level (x)
     tree x;
{
  register tree name = DECL_NAME (x);

  if (name)
    {
      tree oldclass = IDENTIFIER_CLASS_VALUE (name);
      if (oldclass)
	class_binding_level->class_shadowed
	  = tree_cons (name, oldclass, class_binding_level->class_shadowed);
      IDENTIFIER_CLASS_VALUE (name) = x;
      obstack_ptr_grow (&decl_obstack, x);
    }      
  return x;
}

/* Tell caller how to interpret a TREE_LIST which contains
   chains of FUNCTION_DECLS.  */
int
overloaded_globals_p (list)
     tree list;
{
  assert (TREE_CODE (list) == TREE_LIST);

  /* Don't commit caller to seeing them as globals.  */
  if (TREE_NONLOCAL (list))
    return -1;
  /* Do commit caller to seeing them as globals.  */
  if (TREE_CODE (TREE_PURPOSE (list)) == IDENTIFIER_NODE)
    return 1;
  /* Do commit caller to not seeing them as globals.  */
  return 0;
}

/* DECL is a FUNCTION_DECL which may have other definitions already in place.
   We get around this by making IDENTIFIER_GLOBAL_VALUE (DECL_ORIGINAL_NAME (DECL))
   point to a list of all the things that want to be referenced by that name.
   It is then up to the users of that name to decide what to do with that
   list.  */
void
push_overloaded_decl (decl)
     tree decl;
{
  tree orig_name = DECL_ORIGINAL_NAME (decl);
  tree glob = IDENTIFIER_GLOBAL_VALUE (orig_name);

  DECL_OVERLOADED (decl) = 1;
  if (glob && TREE_CODE (glob) != TREE_LIST)
    {
      if (DECL_LANGUAGE (decl) == lang_c)
	{
	  if (TREE_CODE (glob) == FUNCTION_DECL)
	    {
	      if (DECL_LANGUAGE (glob) == lang_c)
		{
		  error_with_decl (decl, "C-language function `%s' overloaded here");
		  error_with_decl (glob, "Previous C-language version of this function was `%s'");
		}
	    }
	  else abort ();
	}
      if (! flag_traditional
	  && TREE_PERMANENT (glob) == 1
	  && current_binding_level != global_binding_level)
	overloads_to_forget = tree_cons (orig_name, glob, overloads_to_forget);
      if (TREE_CODE (glob) == FUNCTION_DECL
	  && DECL_LANGUAGE (glob) != DECL_LANGUAGE (decl)
	  && comptypes (TREE_TYPE (glob), TREE_TYPE (decl), 1))
	{
	  error_with_decl (decl, "conflicting language contexts for declaration of `%s';");
	  error_with_decl (glob, "conflicts with previous declaration here");
	}
      glob = tree_cons (DECL_NAME (glob), glob, NULL_TREE);
      glob = tree_cons (TREE_PURPOSE (glob), decl, glob);
      IDENTIFIER_GLOBAL_VALUE (orig_name) = glob;
      TREE_TYPE (glob) = unknown_type_node;
      return;
    }
  if (glob)
    {
      tree tmp, name;

      if (TREE_VALUE (glob) == NULL_TREE)
	{
	  TREE_VALUE (glob) = decl;
	  return;
	}
      name = DECL_NAME (decl);
      for (tmp = glob; tmp; tmp = TREE_CHAIN (tmp))
	{
	  if (TREE_CODE (TREE_VALUE (tmp)) == FUNCTION_DECL
	      && DECL_LANGUAGE (TREE_VALUE (tmp)) != DECL_LANGUAGE (decl)
	      && comptypes (TREE_TYPE (TREE_VALUE (tmp)), TREE_TYPE (decl), 1))
	    {
	      error_with_decl (decl, "conflicting language contexts for declaration of `%s';");
	      error_with_decl (TREE_VALUE (tmp), "conflicts with previous declaration here");
	    }
	  if (DECL_NAME (TREE_VALUE (tmp)) == name)
	    return;
	}
    }

  if (DECL_LANGUAGE (decl) == lang_c)
    {
      tree decls = glob;
      while (decls && DECL_LANGUAGE (TREE_VALUE (decls)) == lang_cplusplus)
	decls = TREE_CHAIN (decls);
      if (decls)
	{
	  error_with_decl (decl, "C-language function `%s' overloaded here");
	  error_with_decl (TREE_VALUE (decls), "Previous C-language version of this function was `%s'");
	}
    }

  if (! flag_traditional
      && (glob == 0 || TREE_PERMANENT (glob) == 1)
      && current_binding_level != global_binding_level)
    overloads_to_forget = tree_cons (orig_name, glob, overloads_to_forget);
  glob = tree_cons (orig_name, decl, glob);
  IDENTIFIER_GLOBAL_VALUE (orig_name) = glob;
  TREE_TYPE (glob) = unknown_type_node;
}

/* Generate an implicit declaration for identifier FUNCTIONID
   as a function of type int ().  Print a warning if appropriate.  */

tree
implicitly_declare (functionid)
     tree functionid;
{
  register tree decl;
  int temp = allocation_temporary_p ();

  /* Save the decl permanently so we can warn if definition follows.  */
  if (temp && (flag_traditional || !warn_implicit))
    end_temporary_allocation ();

  /* We used to reuse an old implicit decl here,
     but this loses with inline functions because it can clobber
     the saved decl chains.  */
/*  if (IDENTIFIER_IMPLICIT_DECL (functionid) != 0)
    decl = IDENTIFIER_IMPLICIT_DECL (functionid);
  else  */
    decl = build_lang_decl (FUNCTION_DECL, functionid, default_function_type);

  TREE_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;

  /* ANSI standard says implicit declarations are in the innermost block.
     So we record the decl in the standard fashion.
     If flag_traditional is set, pushdecl does it top-level.  */
  pushdecl (decl);
  rest_of_decl_compilation (decl, 0, 0, 0);

  if (warn_implicit
      /* Only one warning per identifier.  */
      && IDENTIFIER_IMPLICIT_DECL (functionid) == 0)
    warning ("implicit declaration of function `%s'",
	     IDENTIFIER_POINTER (functionid));

  SET_IDENTIFIER_IMPLICIT_DECL (functionid, decl);

  if (temp && (flag_traditional || ! warn_implicit))
    resume_temporary_allocation ();

  return decl;
}

/* Return zero if the declaration NEWDECL is valid
   when the declaration OLDDECL (assumed to be for the same name)
   has already been seen.
   Otherwise return an error message format string with a %s
   where the identifier should go.  */

static char *
redeclaration_error_message (newdecl, olddecl)
     tree newdecl, olddecl;
{
  if (TREE_CODE (newdecl) == TYPE_DECL)
    {
      /* Because C++ can put things into name space for free,
	 constructs like "typedef struct foo { ... } foo"
	 would look like an erroneous redeclaration.  */
      if (TREE_TYPE (olddecl) == TREE_TYPE (newdecl))
	return 0;
      else
	return "redefinition of `%s'";
    }
  else if (TREE_CODE (newdecl) == FUNCTION_DECL)
    {
      /* Declarations of functions can insist on internal linkage
	 but they can't be inconsistent with internal linkage,
	 so there can be no error on that account.
	 However defining the same name twice is no good.  */
      if (DECL_INITIAL (olddecl) != 0 && DECL_INITIAL (newdecl) != 0
	  /* However, defining once as extern inline and a second
	     time in another way is ok.  */
	  && !(TREE_INLINE (olddecl) && TREE_EXTERNAL (olddecl)
	       && !(TREE_INLINE (newdecl) && TREE_EXTERNAL (newdecl))))
	{
	  if (DECL_LANG_SPECIFIC (olddecl)
	      && DECL_COMPILER_GENERATED_P (olddecl))
	    return "`%s' not declared in class";
	  else
	    return "redefinition of `%s'";
	}
      return 0;
    }
  else if (current_binding_level == global_binding_level)
    {
      /* Objects declared at top level:  */
      /* If at least one is a reference, it's ok.  */
      if (TREE_EXTERNAL (newdecl) || TREE_EXTERNAL (olddecl))
	return 0;
      /* Reject two definitions.  */
      if (DECL_INITIAL (olddecl) != 0 && DECL_INITIAL (newdecl) != 0)
	return "redefinition of `%s'";
      /* Now we have two tentative defs, or one tentative and one real def.  */
      /* Insist that the linkage match.  */
      if (TREE_PUBLIC (olddecl) != TREE_PUBLIC (newdecl))
	return "conflicting declarations of `%s'";
      return 0;
    }
  else
    {
      /* Objects declared with block scope:  */
      /* Reject two definitions, and reject a definition
	 together with an external reference.  */
      if (!(TREE_EXTERNAL (newdecl) && TREE_EXTERNAL (olddecl)))
	return "redeclaration of `%s'";
      return 0;
    }
}

/* Get the LABEL_DECL corresponding to identifier ID as a label.
   Create one if none exists so far for the current function.
   This function is called for both label definitions and label references.  */

tree
lookup_label (id)
     tree id;
{
  register tree decl = IDENTIFIER_LABEL_VALUE (id);

  if ((decl == 0
      || DECL_SOURCE_LINE (decl) == 0)
      && (named_label_uses == 0
	  || TREE_PURPOSE (named_label_uses) != current_binding_level->names
	  || TREE_VALUE (named_label_uses) != decl))
    {
      named_label_uses
	= tree_cons (current_binding_level->names, decl, named_label_uses);
      TREE_TYPE (named_label_uses) = (tree)current_binding_level;
    }

  if (decl != 0)
    return decl;

  /* By giving the label type `void *', we can use it as a value.  */
  decl = build_decl (LABEL_DECL, id, ptr_type_node);
  DECL_MODE (decl) = VOIDmode;
  /* Mark that the label's definition has not been seen.  */
  DECL_SOURCE_LINE (decl) = 0;

  SET_IDENTIFIER_LABEL_VALUE (id, decl);

  named_labels = tree_cons (NULL_TREE, decl, named_labels);
  TREE_VALUE (named_label_uses) = decl;

  return decl;
}

/* Define a label, specifying the location in the source file.
   Return the LABEL_DECL node for the label, if the definition is valid.
   Otherwise return 0.  */

tree
define_label (filename, line, name)
     char *filename;
     int line;
     tree name;
{
  tree decl = lookup_label (name);

  /* After labels, make any new cleanups go into their
     own new (temporary) binding contour.  */
  current_binding_level->more_cleanups_ok = 0;

  if (DECL_SOURCE_LINE (decl) != 0)
    {
      error_with_decl (decl, "duplicate label `%s'");
      return 0;
    }
  else
    {
      tree uses, prev;

      /* Mark label as having been defined.  */
      DECL_SOURCE_FILE (decl) = filename;
      DECL_SOURCE_LINE (decl) = line;

      for (prev = 0, uses = named_label_uses;
	   uses;
	   prev = uses, uses = TREE_CHAIN (uses))
	if (TREE_VALUE (uses) == decl)
	  {
	    struct binding_level *b = current_binding_level;
	    while (1)
	      {
		tree new_decls = b->names;
		tree old_decls = ((tree)b == TREE_TYPE (uses)
				  ? TREE_PURPOSE (uses) : NULL_TREE);
		while (new_decls != old_decls)
		  {
		    if (TREE_CODE (new_decls) == VAR_DECL
			/* Don't complain about crossing initialization
			   of temporaries.  They can't be accessed,
			   and they should be cleaned up
			   by the time we get to the label.  */
			&& ! TEMP_NAME_P (DECL_NAME (new_decls))
			&& ((DECL_INITIAL (new_decls) != NULL_TREE
			     && DECL_INITIAL (new_decls) != error_mark_node)
			    || TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (new_decls))))
		      {
			if (IDENTIFIER_ERROR_LOCUS (decl) == NULL_TREE)
			  error_with_decl (decl, "invalid jump to label `%s'");
			SET_IDENTIFIER_ERROR_LOCUS (decl, current_function_decl);
			error_with_decl (new_decls, "crosses initialization of `%s'");
		      }
		    new_decls = TREE_CHAIN (new_decls);
		  }
		if ((tree)b == TREE_TYPE (uses))
		  break;
		b = b->level_chain;
	      }

	    if (prev)
	      TREE_CHAIN (prev) = TREE_CHAIN (uses);
	    else
	      named_label_uses = TREE_CHAIN (uses);
	  }
      return decl;
    }
}

/* Same, but for CASE labels.  */
void
define_case_label (decl)
     tree decl;
{
  /* After labels, make any new cleanups go into their
     own new (temporary) binding contour.  */

  current_binding_level->more_cleanups_ok = 0;
}

/* Return the list of declarations of the current level.
   Note that this list is in reverse order unless/until
   you nreverse it; and when you do nreverse it, you must
   store the result back using `storedecls' or you will lose.  */

tree
getdecls ()
{
  return current_binding_level->names;
}

/* Return the list of type-tags (for structs, etc) of the current level.  */

tree
gettags ()
{
  return current_binding_level->tags;
}

/* Store the list of declarations of the current level.
   This is done for the parameter declarations of a function being defined,
   after they are modified in the light of any missing parameters.  */

static void
storedecls (decls)
     tree decls;
{
  current_binding_level->names = decls;
}

/* Similarly, store the list of tags of the current level.  */

static void
storetags (tags)
     tree tags;
{
  current_binding_level->tags = tags;
}

/* Given NAME, an IDENTIFIER_NODE,
   return the structure (or union or enum) definition for that name.
   Searches binding levels from BINDING_LEVEL up to the global level.
   If THISLEVEL_ONLY is nonzero, searches only the specified context
   (but skips any tag-transparent contexts to find one that is
   meaningful for tags).
   FORM says which kind of type the caller wants;
   it is RECORD_TYPE or UNION_TYPE or ENUMERAL_TYPE.
   If the wrong kind of type is found, an error is reported.  */

static tree
lookup_tag (form, name, binding_level, thislevel_only)
     enum tree_code form;
     struct binding_level *binding_level;
     tree name;
     int thislevel_only;
{
  register struct binding_level *level;

  for (level = binding_level; level; level = level->level_chain)
    {
      register tree tail;
      for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	{
	  if (TREE_PURPOSE (tail) == name)
	    {
	      if (TREE_CODE (TREE_VALUE (tail)) != form)
		{
		  /* Definition isn't the kind we were looking for.  */
		  error ("`%s' defined as wrong kind of tag",
			 IDENTIFIER_POINTER (name));
		}
	      return TREE_VALUE (tail);
	    }
	}
      if (thislevel_only && ! level->tag_transparent)
	return NULL_TREE;
      if (current_class_type && level->level_chain == global_binding_level)
	{
	  /* Try looking in this class's tags before heading into
	     global binding level.  */
	  tree these_tags = CLASSTYPE_TAGS (current_class_type);
	  while (these_tags)
	    {
	      if (TREE_PURPOSE (these_tags) == name)
		{
		  if (TREE_CODE (TREE_VALUE (these_tags)) != form)
		    {
		      error ("`%s' defined as wrong kind of tag in class scope",
			     IDENTIFIER_POINTER (name));
		    }
		  return TREE_VALUE (tail);
		}
	      these_tags = TREE_CHAIN (these_tags);
	    }
	}
    }
  return NULL_TREE;
}

/* Given a type, find the tag that was defined for it and return the tag name.
   Otherwise return 0.  However, the value can never be 0
   in the cases in which this is used.

   C++: If NAME is non-zero, this is the new name to install.  This is
   done when replacing anonymous tags with real tag names.  */

static tree
lookup_tag_reverse (type, name)
     tree type;
     tree name;
{
  register struct binding_level *level;

  for (level = current_binding_level; level; level = level->level_chain)
    {
      register tree tail;
      for (tail = level->tags; tail; tail = TREE_CHAIN (tail))
	{
	  if (TREE_VALUE (tail) == type)
	    {
	      if (name)
		TREE_PURPOSE (tail) = name;
	      return TREE_PURPOSE (tail);
	    }
	}
    }
  return NULL_TREE;
}

/* Given type TYPE which was not declared in C++ language context,
   attempt to find a name by which it is refered.  */
tree
typedecl_for_tag (tag)
     tree tag;
{
  struct binding_level *b = current_binding_level;

  if (TREE_CODE (TYPE_NAME (tag)) == TYPE_DECL)
    return TYPE_NAME (tag);

  while (b)
    {
      tree decls = b->names;
      while (decls)
	{
	  if (TREE_CODE (decls) == TYPE_DECL && TREE_TYPE (decls) == tag)
	    break;
	  decls = TREE_CHAIN (decls);
	}
      if (decls)
	return decls;
      b = b->level_chain;
    }
  return NULL_TREE;
}

/* Look up NAME in the current binding level and its superiors
   in the namespace of variables, functions and typedefs.
   Return a ..._DECL node of some kind representing its definition,
   or return 0 if it is undefined.  */

tree
lookup_name (name)
     tree name;
{
  register tree val;
  if (current_binding_level != global_binding_level
      && IDENTIFIER_LOCAL_VALUE (name))
    val = IDENTIFIER_LOCAL_VALUE (name);
  /* In C++ class fields are between local and global scope,
     just before the global scope.  */
  else if (current_class_type)
    {
      if (IDENTIFIER_CLASS_VALUE (name))
	val = IDENTIFIER_CLASS_VALUE (name);
      else if (TYPE_SIZE (current_class_type) == 0
	       && CLASSTYPE_LOCAL_TYPEDECLS (current_class_type))
	{
	  /* Try to find values from base classes
	     if we are presently defining a type.
	     We are presently only interested in TYPE_DECLs.  */
	  val = lookup_field (current_class_type, name, 0);
	  if (val == error_mark_node)
	    return val;
	  if (val && TREE_CODE (val) != TYPE_DECL)
	    val = NULL_TREE;
	  else if (val == NULL_TREE)
	    val = IDENTIFIER_GLOBAL_VALUE (name);
	}
      else
	val = IDENTIFIER_GLOBAL_VALUE (name);
    }
  else
    val = IDENTIFIER_GLOBAL_VALUE (name);
  if (val && TREE_TYPE (val) == error_mark_node)
    return error_mark_node;
  return val;
}

/* Similar to `lookup_name' but look only at current binding level.  */

static tree
lookup_name_current_level (name)
     tree name;
{
  register tree t;

  if (current_binding_level == global_binding_level)
    return IDENTIFIER_GLOBAL_VALUE (name);

  if (IDENTIFIER_LOCAL_VALUE (name) == 0)
    return 0;

  for (t = current_binding_level->names; t; t = TREE_CHAIN (t))
    if (DECL_NAME (t) == name)
      break;

  return t;
}

static int sigsegv ()
{
  error ("Segmentation violation");
  signal (SIGSEGV, SIG_DFL);
}

/* Create the predefined scalar types of C,
   and some nodes representing standard constants (0, 1, (void *)0).
   Initialize the global binding level.
   Make definitions for built-in primitive functions.  */

union tree_node ERROR_MARK_NODE;

void
init_decl_processing ()
{
  register tree endlink, int_endlink, double_endlink, ptr_endlink;

  /* Have to make these distinct before we try using them.  */
  lang_name_cplusplus = get_identifier ("C++");
  lang_name_c = get_identifier ("C");

  /* Initially, C.  */
  current_lang_name = lang_name_c;

  current_function_decl = NULL_TREE;
  named_labels = NULL_TREE;
  named_label_uses = NULL_TREE;
  current_binding_level = NULL_BINDING_LEVEL;
  free_binding_level = NULL_BINDING_LEVEL;

  if (write_symbols == GDB_DEBUG)
    fatal ("GNU C++ does not support GDB symbol info yet, use -g");

  /* Handle signals.  */
  signal (SIGSEGV, sigsegv);

  obstack_init (&decl_obstack);

  /* Must lay these out before anything else gets laid out.  */
#if 0
  error_mark_node = make_node (ERROR_MARK);
#else
#undef error_mark_node
  error_mark_node = &ERROR_MARK_NODE;
#define error_mark_node (&ERROR_MARK_NODE)
  TREE_PERMANENT (error_mark_node) = 1;
#endif
  TREE_TYPE (error_mark_node) = error_mark_node;
  error_mark_list = build_tree_list (error_mark_node, error_mark_node);
  TREE_TYPE (error_mark_list) = error_mark_node;

  pushlevel (0);	/* make the binding_level structure for global names.  */
  global_binding_level = current_binding_level;

  value_identifier = get_identifier ("<value>");
  this_identifier = get_identifier (THIS_NAME);
  in_charge_identifier = get_identifier (IN_CHARGE_NAME);

  /* Define `int' and `char' first so that dbx will output them first.  */

  integer_type_node = make_signed_type (INT_TYPE_SIZE);
  IDENTIFIER_GLOBAL_VALUE (ridpointers[(int) RID_INT])
    = pushdecl (build_decl (TYPE_DECL, ridpointers[(int) RID_INT],
			    integer_type_node));

  /* Define `char', which is like either `signed char' or `unsigned char'
     but not the same as either.  */

  char_type_node =
    (flag_signed_char
     ? make_signed_type (CHAR_TYPE_SIZE)
     : make_unsigned_type (CHAR_TYPE_SIZE));

  IDENTIFIER_GLOBAL_VALUE (ridpointers[(int) RID_CHAR])
    = pushdecl (build_decl (TYPE_DECL, get_identifier ("char"),
			    char_type_node));

  long_integer_type_node = make_signed_type (LONG_TYPE_SIZE);
  IDENTIFIER_GLOBAL_VALUE (ridpointers[(int) RID_LONG])
    = pushdecl (build_decl (TYPE_DECL, get_identifier ("long int"),
			    long_integer_type_node));

  unsigned_type_node = make_unsigned_type (INT_TYPE_SIZE);
  IDENTIFIER_GLOBAL_VALUE (ridpointers[(int) RID_UNSIGNED])
    = pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned int"),
			    unsigned_type_node));

  long_unsigned_type_node = make_unsigned_type (LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long unsigned int"),
			long_unsigned_type_node));

  /* `unsigned long' or `unsigned int' is the standard type for sizeof.
     Traditionally, use a signed type.  */
  if (INT_TYPE_SIZE != BITS_PER_WORD)
    sizetype = flag_traditional ? long_integer_type_node : long_unsigned_type_node;
  else
    sizetype = flag_traditional ? integer_type_node : unsigned_type_node;

  TREE_TYPE (TYPE_SIZE (integer_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (char_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_unsigned_type_node)) = sizetype;
  TREE_TYPE (TYPE_SIZE (long_integer_type_node)) = sizetype;

  short_integer_type_node = make_signed_type (SHORT_TYPE_SIZE);
  IDENTIFIER_GLOBAL_VALUE (ridpointers[(int) RID_SHORT])
    = pushdecl (build_decl (TYPE_DECL, get_identifier ("short int"),
			    short_integer_type_node));

  long_long_integer_type_node = make_signed_type (LONG_LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long int"),
			long_long_integer_type_node));
  short_unsigned_type_node = make_unsigned_type (SHORT_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned short int"),
			short_unsigned_type_node));
  long_long_unsigned_type_node = make_unsigned_type (LONG_LONG_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long long unsigned int"),
			long_long_unsigned_type_node));

  /* Define both `signed char' and `unsigned char'.  */
  signed_char_type_node = make_signed_type (CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("signed char"),
			signed_char_type_node));
  unsigned_char_type_node = make_unsigned_type (CHAR_TYPE_SIZE);
  pushdecl (build_decl (TYPE_DECL, get_identifier ("unsigned char"),
			unsigned_char_type_node));

  float_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (float_type_node) = FLOAT_TYPE_SIZE;
  IDENTIFIER_GLOBAL_VALUE (ridpointers[(int) RID_FLOAT])
    = pushdecl (build_decl (TYPE_DECL, ridpointers[(int) RID_FLOAT],
			    float_type_node));
  layout_type (float_type_node);

  double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (double_type_node) = DOUBLE_TYPE_SIZE;
  IDENTIFIER_GLOBAL_VALUE (ridpointers[(int) RID_DOUBLE])
    = pushdecl (build_decl (TYPE_DECL, ridpointers[(int) RID_DOUBLE],
			    double_type_node));
  layout_type (double_type_node);

  long_double_type_node = make_node (REAL_TYPE);
  TYPE_PRECISION (long_double_type_node) = DOUBLE_TYPE_SIZE;
  pushdecl (build_decl (TYPE_DECL, get_identifier ("long double"),
			long_double_type_node));
  layout_type (long_double_type_node);

  integer_zero_node = build_int_2 (0, 0);
  TREE_TYPE (integer_zero_node) = integer_type_node;
  integer_one_node = build_int_2 (1, 0);
  TREE_TYPE (integer_one_node) = integer_type_node;
  integer_two_node = build_int_2 (2, 0);
  TREE_TYPE (integer_two_node) = integer_type_node;
  integer_three_node = build_int_2 (3, 0);
  TREE_TYPE (integer_three_node) = integer_type_node;
  empty_init_node = build_nt (CONSTRUCTOR, NULL_TREE, NULL_TREE);

  size_zero_node = build_int_2 (0, 0);
  TREE_TYPE (size_zero_node) = sizetype;
  size_one_node = build_int_2 (1, 0);
  TREE_TYPE (size_one_node) = sizetype;

  void_type_node = make_node (VOID_TYPE);
  IDENTIFIER_GLOBAL_VALUE (ridpointers[(int) RID_VOID])
    = pushdecl (build_decl (TYPE_DECL,
			    ridpointers[(int) RID_VOID], void_type_node));
  layout_type (void_type_node); /* Uses integer_zero_node.  */
  void_list_node = build_tree_list (NULL_TREE, void_type_node);
  TREE_PARMLIST (void_list_node) = 1;

  null_pointer_node = build_int_2 (0, 0);
  TREE_TYPE (null_pointer_node) = build_pointer_type (void_type_node);
  layout_type (TREE_TYPE (null_pointer_node));

  string_type_node = build_pointer_type (char_type_node);

  /* make a type for arrays of 256 characters.
     256 is picked randomly because we have a type for integers from 0 to 255.
     With luck nothing will ever really depend on the length of this
     array type.  */
  char_array_type_node
    = build_array_type (char_type_node, unsigned_char_type_node);
  /* Likewise for arrays of ints.  */
  int_array_type_node
    = build_array_type (integer_type_node, unsigned_char_type_node);

  default_function_type
    = build_function_type (integer_type_node, NULL_TREE);
  build_pointer_type (default_function_type);

  ptr_type_node = build_pointer_type (void_type_node);
  endlink = void_list_node;
  int_endlink = tree_cons (NULL_TREE, integer_type_node, endlink);
  double_endlink = tree_cons (NULL_TREE, double_type_node, endlink);
  ptr_endlink = tree_cons (NULL_TREE, ptr_type_node, endlink);

  double_ftype_double
    = build_function_type (double_type_node, double_endlink);

  double_ftype_double_double
    = build_function_type (double_type_node,
			   tree_cons (NULL_TREE, double_type_node, double_endlink));

  int_ftype_int
    = build_function_type (integer_type_node, int_endlink);

  long_ftype_long
    = build_function_type (long_integer_type_node,
			   tree_cons (NULL_TREE, long_integer_type_node, endlink));

  void_ftype_ptr_ptr_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, ptr_type_node,
						 int_endlink)));

  int_ftype_ptr_ptr_int
    = build_function_type (integer_type_node, TYPE_ARG_TYPES (void_ftype_ptr_ptr_int));

  void_ftype_ptr_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 int_endlink)));

  ptr_ftype_long
    = build_function_type (ptr_type_node, TYPE_ARG_TYPES (long_ftype_long));

  ptr_ftype_ptr_int_int_ptr
    = build_function_type (ptr_type_node,
			   tree_cons (NULL_TREE, ptr_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE, integer_type_node,
							    ptr_endlink))));

  void_ftype_ptr
    = build_function_type (void_type_node, ptr_endlink);

  void_ftype_ptr_int_int_ptr_int_int
    = build_function_type (void_type_node,
	   tree_cons (NULL_TREE, ptr_type_node,
	      tree_cons (NULL_TREE, integer_type_node,
		 tree_cons (NULL_TREE, integer_type_node,
			    TYPE_ARG_TYPES (void_ftype_ptr_int_int)))));

#ifdef VTABLE_USES_MASK
  /* This is primarily for virtual function definition.  We
     declare an array of `void *', which can later be
     converted to the appropriate function pointer type.
     To do pointers to members, we need a mask which can
     distinguish an index value into a virtual function table
     from an address.  */
  vtbl_mask = build_int_2 (~(VINDEX_MAX - 1), -1);
#endif

  vtbl_type_node
    = build_array_type (ptr_type_node, NULL_TREE);
  layout_type (vtbl_type_node);
  vtbl_type_node = build_type_variant (vtbl_type_node, 1, 0);

  builtin_function ("__builtin_alloca",
                    build_function_type (ptr_type_node, int_endlink),
                    BUILT_IN_ALLOCA);

  builtin_function ("__builtin_abs", int_ftype_int, BUILT_IN_ABS);
  builtin_function ("__builtin_fabs", double_ftype_double, BUILT_IN_FABS);
  builtin_function ("__builtin_labs", long_ftype_long, BUILT_IN_LABS);
  builtin_function ("__builtin_ffs", int_ftype_int, BUILT_IN_FFS);
#if 0
  /* Support for these has not been written in either expand_builtin
     or build_function_call.  */
  builtin_function ("__builtin_div", default_ftype, BUILT_IN_DIV);
  builtin_function ("__builtin_ldiv", default_ftype, BUILT_IN_LDIV);
  builtin_function ("__builtin_ffloor", double_ftype_double, BUILT_IN_FFLOOR);
  builtin_function ("__builtin_fceil", double_ftype_double, BUILT_IN_FCEIL);
  builtin_function ("__builtin_fmod", double_ftype_double_double, BUILT_IN_FMOD);
  builtin_function ("__builtin_frem", double_ftype_double_double, BUILT_IN_FREM);
  builtin_function ("__builtin_memcpy", void_ftype_ptr_ptr_int, BUILT_IN_MEMCPY);
  builtin_function ("__builtin_memcmp", int_ftype_ptr_ptr_int, BUILT_IN_MEMCMP);
  builtin_function ("__builtin_memset", void_ftype_ptr_int_int, BUILT_IN_MEMSET);
  builtin_function ("__builtin_fsqrt", double_ftype_double, BUILT_IN_FSQRT);
  builtin_function ("__builtin_getexp", double_ftype_double, BUILT_IN_GETEXP);
  builtin_function ("__builtin_getman", double_ftype_double, BUILT_IN_GETMAN);
#endif

  /* C++ extensions */

  unknown_type_node = make_node (UNKNOWN_TYPE);
  pushdecl (build_decl (TYPE_DECL,
			get_identifier ("unknown type"),
			unknown_type_node));
  TYPE_SIZE (unknown_type_node) = TYPE_SIZE (void_type_node);
  TYPE_SIZE_UNIT (unknown_type_node) = TYPE_SIZE_UNIT (void_type_node);
  TYPE_ALIGN (unknown_type_node) = 1;
  TYPE_MODE (unknown_type_node) = TYPE_MODE (void_type_node);
  /* Indirecting an UNKNOWN_TYPE node yields an UNKNOWN_TYPE node.  */
  TREE_TYPE (unknown_type_node) = unknown_type_node;
  /* Looking up TYPE_POINTER_TO and TYPE_REFERENCE_TO yield the same result.  */
  TYPE_POINTER_TO (unknown_type_node) = unknown_type_node;
  TYPE_REFERENCE_TO (unknown_type_node) = unknown_type_node;

  /* Define these now, but use 0 as their DECL_FUNCTION_CODE.  This
     will install them in the global binding level, but cause them
     to be expanded normally.  */
  builtin_function ("__main", default_function_type, NOT_BUILT_IN);
  pushdecl (lookup_name (get_identifier ("__main")));

  builtin_function ("__builtin_saveregs", default_function_type,
		    BUILT_IN_SAVEREGS);
  builtin_function ("__builtin_classify_type", default_function_type,
		    BUILT_IN_CLASSIFY_TYPE);

  {
    /* Simplify life by making a "vtable_entry_type".  Give its
       fields names so that the debugger can use them.  */
    tree fields[4];
    int i;

    vtable_entry_type = make_lang_type (RECORD_TYPE);
    CLASSTYPE_OFFSET (vtable_entry_type) = integer_zero_node;
    fields[0] = build_lang_field_decl (FIELD_DECL, get_identifier (VTABLE_DELTA_NAME), short_integer_type_node);
    fields[1] = build_lang_field_decl (FIELD_DECL, get_identifier (VTABLE_INDEX_NAME), short_integer_type_node);
    fields[2] = build_lang_field_decl (FIELD_DECL, get_identifier (VTABLE_PFN_NAME), ptr_type_node);
    TYPE_FIELDS (vtable_entry_type) = fields[0];
    for (i = 0; i < 2; i++)
      {
        DECL_FIELD_CONTEXT (fields[i]) = vtable_entry_type;
        TREE_CHAIN (fields[i]) = fields[i+1];
      }
    DECL_FIELD_CONTEXT (fields[i]) = vtable_entry_type;
    TYPE_ALIGN (vtable_entry_type) = TYPE_ALIGN (double_type_node);
    layout_type (vtable_entry_type);
    CLASSTYPE_VBASE_SIZE (vtable_entry_type) = integer_zero_node;
    TYPE_NAME (vtable_entry_type) = build_decl (TYPE_DECL,
						get_identifier (VTBL_PTR_TYPE),
						vtable_entry_type);
    layout_decl (TYPE_NAME (vtable_entry_type));

    /* Make this part of an invisible union.  */
    fields[3] = copy_node (fields[2]);
    TREE_TYPE (fields[3]) = short_integer_type_node;
    DECL_NAME (fields[3]) = get_identifier (VTABLE_DELTA2_NAME);
    DECL_MODE (fields[3]) = TYPE_MODE (short_integer_type_node);
    DECL_SIZE (fields[3]) = TYPE_SIZE (short_integer_type_node);
    DECL_SIZE_UNIT (fields[3]) = TYPE_SIZE_UNIT (short_integer_type_node);
    TREE_UNSIGNED (fields[3]) = 0;
    TREE_CHAIN (fields[2]) = fields[3];

    vtable_entry_type = build_type_variant (vtable_entry_type, 1, 0);
  }

#ifdef SOS
  if (flag_all_virtual == 2)
    {
      tree fields[5];
      tree ptr_ftype_default
        = build_function_type (ptr_type_node, NULL_TREE);
      int i;

      builtin_function ("sosFindCode", ptr_ftype_default, NOT_BUILT_IN);
      builtin_function ("sosLookup", ptr_ftype_default, NOT_BUILT_IN);
      builtin_function ("sosImport", ptr_ftype_default, NOT_BUILT_IN);
      builtin_function ("sosDynError", ptr_ftype_default, NOT_BUILT_IN);

      zlink_type = make_lang_type (RECORD_TYPE);
      CLASSTYPE_OFFSET (zlink_type) = integer_zero_node;
      fields[0] = build_lang_field_decl (FIELD_DECL, get_identifier ("n"), string_type_node);
      fields[1] = build_lang_field_decl (FIELD_DECL, get_identifier ("t"), char_type_node);
      fields[2] = build_lang_field_decl (FIELD_DECL, get_identifier ("ptr"), TYPE_POINTER_TO (default_function_type));

      TYPE_FIELDS (zlink_type) = fields[0];
      for (i = 0; i < 2; i++)
        {
	  DECL_FIELD_CONTEXT (fields[i]) = zlink_type;
	  TREE_CHAIN (fields[i]) = fields[i+1];
        }
      DECL_FIELD_CONTEXT (fields[i]) = zlink_type;
      TYPE_ALIGN (zlink_type) = 1;
      layout_type (zlink_type);
      CLASSTYPE_VBASE_SIZE (zlink_type) = integer_zero_node;

      zret_type = make_lang_type (RECORD_TYPE);
      CLASSTYPE_OFFSET (zret_type) = integer_zero_node;
      fields[0] = build_lang_field_decl (FIELD_DECL, get_identifier ("cn"), string_type_node);
      fields[1] = build_lang_field_decl (FIELD_DECL, get_identifier ("ptr"), build_pointer_type (TYPE_POINTER_TO (default_function_type)));
      fields[2] = build_lang_field_decl (FIELD_DECL, get_identifier ("n"), integer_type_node);
      fields[3] = build_lang_field_decl (FIELD_DECL, get_identifier ("bcl"), string_type_node);
      fields[4] = build_lang_field_decl (FIELD_DECL, get_identifier ("f"), char_type_node);

      TYPE_FIELDS (zret_type) = fields[0];
      for (i = 0; i < 4; i++)
        {
	  TREE_CHAIN (fields[i]) = fields[i+1];
	  DECL_FIELD_CONTEXT (fields[i]) = zret_type;
        }
      DECL_FIELD_CONTEXT (fields[i]) = zret_type;
      TYPE_ALIGN (zret_type) = 1;
      layout_type (zret_type);
      CLASSTYPE_VBASE_SIZE (zret_type) = integer_zero_node;
    }
#endif

  /* Now, C++.  */
  current_lang_name = lang_name_cplusplus;

  auto_function ("__builtin_new", ptr_ftype_long, NOT_BUILT_IN);
  auto_function ("__builtin_vec_new", ptr_ftype_ptr_int_int_ptr, NOT_BUILT_IN);
  auto_function ("__builtin_delete", void_ftype_ptr, NOT_BUILT_IN);
  auto_function ("__builtin_vec_delete", void_ftype_ptr_int_int_ptr_int_int, NOT_BUILT_IN);

  abort_fndecl
    = define_function ("abort",
		       build_function_type (void_type_node, void_list_node),
		       NOT_BUILT_IN, 0);

  unhandled_exception_fndecl
    = define_function ("__unhandled_exception",
		       build_function_type (void_type_node, NULL_TREE),
		       NOT_BUILT_IN, 0);

  /* Perform other language dependent initializations.  */
  init_class_processing ();
  init_init_processing ();
  init_search_processing ();
  if (flag_handle_exceptions)
    {
      init_exception_processing ();
      if (flag_handle_exceptions == 2)
	/* Too much trouble to inline all the trys needed for this.  */
	flag_this_is_variable = 2;
    }
  if (flag_no_inline)
    flag_inline_functions = 0;
  if (flag_cadillac)
    init_cadillac ();
}

/* Make a definition for a builtin function named NAME and whose data type
   is TYPE.  TYPE should be a function type with argument types.
   FUNCTION_CODE tells later passes how to compile calls to this function.
   See tree.h for its possible values.  */

tree
define_function (name, type, function_code, pfn)
     char *name;
     tree type;
     enum built_in_function function_code;
     void (*pfn)();
{
  tree decl = build_lang_decl (FUNCTION_DECL, get_identifier (name), type);
  TREE_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  make_function_rtl (decl);
  if (pfn) pfn (decl);
  DECL_SET_FUNCTION_CODE (decl, function_code);
  return decl;
}

/* Called when a declaration is seen that contains no names to declare.
   If its type is a reference to a structure, union or enum inherited
   from a containing scope, shadow that tag name for the current scope
   with a forward reference.
   If its type defines a new named structure or union
   or defines an enum, it is valid but we need not do anything here.
   Otherwise, it is an error.

   C++: may have to grok the declspecs to learn about static,
   complain for anonymous unions.  */

void
shadow_tag (declspecs)
     tree declspecs;
{
  int found_tag = 0;
  int warned = 0;
  register tree link;
  register enum tree_code code, ok_code = ERROR_MARK;
  register tree t = NULL_TREE;

  for (link = declspecs; link; link = TREE_CHAIN (link))
    {
      register tree value = TREE_VALUE (link);

      code = TREE_CODE (value);
      if (IS_AGGR_TYPE_CODE (code) || code == ENUMERAL_TYPE)
	/* Used to test also that TYPE_SIZE (value) != 0.
	   That caused warning for `struct foo;' at top level in the file.  */
	{
	  register tree name = TYPE_NAME (value);

	  if (name == NULL_TREE)
	    name = lookup_tag_reverse (value, NULL_TREE);

	  if (name && TREE_CODE (name) == TYPE_DECL)
	    name = DECL_NAME (name);

	  if (class_binding_level)
	    t = lookup_tag (code, name, class_binding_level, 1);
	  else
	    t = lookup_tag (code, name, current_binding_level, 1);

	  if (t == 0)
	    {
	      int temp = allocation_temporary_p ();
	      if (temp)
		end_temporary_allocation ();
	      if (IS_AGGR_TYPE_CODE (code))
		t = make_lang_type (code);
	      else
		t = make_node (code);
	      pushtag (name, t);
	      if (temp)
		resume_temporary_allocation ();
	      ok_code = code;
	      break;
	    }
	  else if (name != 0 || code == ENUMERAL_TYPE)
	    ok_code = code;

	  if (ok_code != ERROR_MARK)
	    found_tag++;
	  else
	    {
	      if (!warned)
		warning ("useless keyword or type name in declaration");
	      warned = 1;
	    }
	}
    }

  /* This is where the variables in an anonymous union are
     declared.  An anonymous union declaration looks like:
     union { ... } ;
     because there is no declarator after the union, the parser
     sends that declaration here.  */
  if (ok_code == UNION_TYPE
      && t != NULL_TREE
      && ((TREE_CODE (TYPE_NAME (t)) == IDENTIFIER_NODE
	   && ANON_AGGRNAME_P (TYPE_NAME (t)))
	  || (TREE_CODE (TYPE_NAME (t)) == TYPE_DECL
	      && ANON_AGGRNAME_P (DECL_NAME (TYPE_NAME (t)))))
      && TYPE_FIELDS (t))
    {
      tree decl = grokdeclarator (NULL_TREE, declspecs, NORMAL, 0, NULL_TREE);
      finish_anon_union (decl);
    }
  else if (ok_code == RECORD_TYPE
	   && found_tag == 1
	   && TYPE_LANG_SPECIFIC (t)
	   && CLASSTYPE_DECLARED_EXCEPTION (t))
    {
      if (TYPE_SIZE (t))
	error_with_aggr_type (t, "redeclaration of exception `%s'");
      else
	{
	  tree ename, decl;
	  int temp = allocation_temporary_p ();
	  int momentary = suspend_momentary ();
	  if (temp)
	    end_temporary_allocation ();

	  pushclass (t, 0);
	  finish_exception (t, NULL_TREE);

	  ename = TYPE_NAME (t);
	  if (TREE_CODE (ename) == TYPE_DECL)
	    ename = DECL_NAME (ename);
	  decl = build_lang_field_decl (VAR_DECL, ename, t);
	  finish_exception_decl (current_class_name, decl);
	  end_exception_decls ();

	  if (temp)
	    resume_temporary_allocation ();
	  if (momentary)
	    resume_momentary ();
	}
    }
  else if (!warned)
    {
      if (found_tag > 1)
	warning ("multiple types in one declaration");
      if (found_tag == 0)
	warning ("empty declaration");
    }
}

/* Decode a "typename", such as "int **", returning a ..._TYPE node.  */

tree
groktypename (typename)
     tree typename;
{
  if (TREE_CODE (typename) != TREE_LIST)
    return typename;
  return grokdeclarator (TREE_VALUE (typename),
			 TREE_PURPOSE (typename),
			 TYPENAME, 0, NULL_TREE);
}

/* Decode a declarator in an ordinary declaration or data definition.
   This is called as soon as the type information and variable name
   have been parsed, before parsing the initializer if any.
   Here we create the ..._DECL node, fill in its type,
   and put it on the list of decls for the current context.
   The ..._DECL node is returned as the value.

   Exception: for arrays where the length is not specified,
   the type is left null, to be filled in by `finish_decl'.

   Function definitions do not come here; they go to start_function
   instead.  However, external and forward declarations of functions
   do go through here.  Structure field declarations are done by
   grokfield and not through here.  */

/* Set this to zero to debug not using the temporary obstack
   to parse initializers.  */
int debug_temp_inits = 1;

tree
start_decl (declarator, declspecs, initialized, raises)
     tree declspecs, declarator;
     int initialized;
     tree raises;
{
  register tree decl = grokdeclarator (declarator, declspecs,
				       NORMAL, initialized, raises);
  register tree type, tem;
  int init_written = initialized;

  if (decl == NULL_TREE) return decl;

  type = TREE_TYPE (decl);

  /* Don't lose if destructors must be executed at file-level.  */
  if (TREE_STATIC (decl)
      && TYPE_NEEDS_DESTRUCTOR (type)
      && TREE_PERMANENT (decl) == 0)
    {
      end_temporary_allocation ();
      decl = copy_node (decl);
      if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  tree itype = TYPE_DOMAIN (type);
	  if (itype && ! TREE_PERMANENT (itype))
	    {
	      itype = build_index_type (copy_to_permanent (TYPE_MAX_VALUE (itype)));
	      type = build_cplus_array_type (TREE_TYPE (type), itype);
	      TREE_TYPE (decl) = type;
	    }
	}
      resume_temporary_allocation ();
    }

  /* Interesting work for this is done in `finish_exception_decl'.  */
  if (TREE_CODE (type) == RECORD_TYPE
      && CLASSTYPE_DECLARED_EXCEPTION (type))
    return decl;

  if (DECL_CONTEXT (decl))
    {
      /* If it was not explicitly declared `extern',
	 revoke any previous claims of TREE_EXTERNAL.  */
      if (DECL_EXTERNAL (decl) == 0)
	TREE_EXTERNAL (decl) = 0;
      if (DECL_LANG_SPECIFIC (decl))
	DECL_IN_AGGR_P (decl) = 0;
      pushclass (DECL_CONTEXT (decl), 2);
    }

  /* If this type of object needs a cleanup, and control may
     jump past it, make a new binding level so that it is cleaned
     up only when it is initialized first.  */
  if (TYPE_NEEDS_DESTRUCTOR (type)
      && current_binding_level->more_cleanups_ok == 0)
    pushlevel_temporary (1);

  if (initialized)
    /* Is it valid for this decl to have an initializer at all?
       If not, set INITIALIZED to zero, which will indirectly
       tell `finish_decl' to ignore the initializer once it is parsed.  */
    switch (TREE_CODE (decl))
      {
      case TYPE_DECL:
	/* typedef foo = bar  means give foo the same type as bar.
	   We haven't parsed bar yet, so `finish_decl' will fix that up.
	   Any other case of an initialization in a TYPE_DECL is an error.  */
	if (pedantic || list_length (declspecs) > 1)
	  {
	    error ("typedef `%s' is initialized",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
	break;

      case FUNCTION_DECL:
	error ("function `%s' is initialized like a variable",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
	initialized = 0;
	break;

      default:
	/* Don't allow initializations for incomplete types
	   except for arrays which might be completed by the initialization.  */
	if (TYPE_SIZE (type) != 0)
	  ;                     /* A complete type is ok.  */
	else if (TREE_CODE (type) != ARRAY_TYPE)
	  {
	    error ("variable `%s' has initializer but incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
	else if (TYPE_SIZE (TREE_TYPE (type)) == 0)
	  {
	    error ("elements of array `%s' have incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (decl)));
	    initialized = 0;
	  }
      }

  if (!initialized && TREE_CODE (decl) != TYPE_DECL
      && IS_AGGR_TYPE (type) && ! TREE_EXTERNAL (decl))
    {
      if (TYPE_SIZE (type) == 0)
	{
	  error ("aggregate `%s' has incomplete type and cannot be initialized",
		 IDENTIFIER_POINTER (DECL_NAME (decl)));
	  /* Change the type so that assemble_variable will give
	     DECL an rtl we can live with: (mem (const_int 0)).  */
	  TREE_TYPE (decl) = error_mark_node;
	  type = error_mark_node;
	}
      else
	{
	  /* If any base type in the hierarchy of TYPE needs a constructor,
	     then we set initialized to 1.  This way any nodes which are
	     created for the purposes of initializing this aggregate
	     will live as long as it does.  This is necessary for global
	     aggregates which do not have their initializers processed until
	     the end of the file.  */
	  initialized = TYPE_NEEDS_CONSTRUCTING (type);
	}
    }

  if (initialized)
    {
      if (current_binding_level != global_binding_level
	  && TREE_EXTERNAL (decl))
	warning ("declaration of `%s' has `extern' and is initialized",
		 IDENTIFIER_POINTER (DECL_NAME (decl)));
      TREE_EXTERNAL (decl) = 0;
      if (current_binding_level == global_binding_level)
	TREE_STATIC (decl) = 1;

      /* Tell `pushdecl' this is an initialized decl
	 even though we don't yet have the initializer expression.
	 Also tell `finish_decl' it may store the real initializer.  */
      DECL_INITIAL (decl) = error_mark_node;
    }

  /* Add this decl to the current binding level, but not if it
     comes from another scope, e.g. a static member variable.
     TEM may equal DECL or it may be a previous decl of the same name.  */
  if ((TREE_CODE (decl) != PARM_DECL && DECL_CONTEXT (decl) != NULL_TREE)
      || TREE_CODE (type) == LANG_TYPE)
    tem = decl;
  else
    {
      tem = pushdecl (decl);
      if (TREE_CODE (tem) == TREE_LIST)
	{
	  tree tem2 = value_member (decl, tem);
	  if (tem2 != NULL_TREE)
	    tem = TREE_VALUE (tem2);
	  else
	    {
	      while (tem && ! decls_match (decl, TREE_VALUE (tem)))
		tem = TREE_CHAIN (tem);
	      if (tem == NULL_TREE)
		tem = decl;
	      else
		tem = TREE_VALUE (tem);
	    }
	}
    }

#if 0
  /* We don't do this yet for GNU C++.  */
  /* For a local variable, define the RTL now.  */
  if (current_binding_level != global_binding_level
      /* But not if this is a duplicate decl
	 and we preserved the rtl from the previous one
	 (which may or may not happen).  */
      && DECL_RTL (tem) == 0)
    {
      if (TYPE_SIZE (TREE_TYPE (tem)) != 0)
	expand_decl (tem);
      else if (TREE_CODE (TREE_TYPE (tem)) == ARRAY_TYPE
	       && DECL_INITIAL (tem) != 0)
	expand_decl (tem);
    }
#endif

    if (TREE_CODE (decl) == FUNCTION_DECL && DECL_OVERLOADED (decl))
      /* @@ Also done in start_function.  */
      push_overloaded_decl (tem);

  if (init_written
      && ! (TREE_CODE (tem) == PARM_DECL
	    || (TREE_READONLY (tem)
		&& (TREE_CODE (tem) == VAR_DECL
		    || TREE_CODE (tem) == FIELD_DECL))))
    {
      /* When parsing and digesting the initializer,
	 use temporary storage.  Do this even if we will ignore the value.  */
      if (current_binding_level == global_binding_level && debug_temp_inits)
	{
	  if (TYPE_NEEDS_CONSTRUCTING (type))
	    /* In this case, the initializer must lay down in permanent
	       storage, since it will be saved until `finish_file' is run.   */
	    ;
	  else
	    temporary_allocation ();
	}
    }

  if (flag_cadillac)
    cadillac_start_decl (tem);

  return tem;
}

/* Handle initialization of references.
   These three arguments from from `finish_decl', and have the
   same meaning here that they do there.  */
static void
grok_reference_init (decl, type, init, cleanupp)
     tree decl, type, init;
     tree *cleanupp;
{
  char *errstr = 0;
  int is_reference;
  tree tmp;
  tree this_ptr_type, actual_init;

  if (init == NULL_TREE)
    {
      if (DECL_LANG_SPECIFIC (decl) == 0 || DECL_IN_AGGR_P (decl) == 0)
	{
	  error ("variable declared as reference not initialized");
	  if (TREE_CODE (decl) == VAR_DECL)
	    SET_DECL_REFERENCE_SLOT (decl, error_mark_node);
	}
      return;
    }

  if (TREE_CODE (init) == TREE_LIST)
    init = build_compound_expr (init);
  is_reference = TREE_CODE (TREE_TYPE (init)) == REFERENCE_TYPE;
  tmp = is_reference ? convert_from_reference (init) : init;

  if (is_reference)
    {
      if (! comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (type)),
		       TYPE_MAIN_VARIANT (TREE_TYPE (tmp)), 0))
	errstr = "initialization of `%s' from dissimilar reference type";
      else if (TREE_READONLY (TREE_TYPE (type))
	       >= TREE_READONLY (TREE_TYPE (TREE_TYPE (init))))
	{
	  is_reference = 0;
	  init = tmp;
	}
    }
  else
    {
      if (TREE_CODE (TREE_TYPE (type)) != ARRAY_TYPE
	  && TREE_CODE (TREE_TYPE (init)) == ARRAY_TYPE)
	{
	  /* Note: default conversion is only called in very
	     special cases.  */
	  init = default_conversion (init);
	}
      if (TREE_CODE (TREE_TYPE (type)) == TREE_CODE (TREE_TYPE (init)))
	{
	  init = convert (TREE_TYPE (type), init);
	}
      else if (init != error_mark_node
	       && ! comptypes (TYPE_MAIN_VARIANT (TREE_TYPE (type)),
			       TYPE_MAIN_VARIANT (TREE_TYPE (init)), 0))
	errstr = "invalid type conversion for reference";
    }

  if (errstr)
    {
      /* Things did not go smoothly; look for type conversion.  */
      if (IS_AGGR_TYPE (TREE_TYPE (tmp)))
	{
	  tmp = build_type_conversion (CONVERT_EXPR, type, init, 0);
	  if (tmp != NULL_TREE)
	    {
	      init = tmp;
	      if (tmp == error_mark_node)
		errstr = "ambiguous pointer conversion";
	      else
		errstr = 0;
	      is_reference = 1;
	    }
	  else
	    {
	      tmp = build_type_conversion (CONVERT_EXPR, TREE_TYPE (type), init, 0);
	      if (tmp != NULL_TREE)
		{
		  init = tmp;
		  if (tmp == error_mark_node)
		    errstr = "ambiguous pointer conversion";
		  else
		    errstr = 0;
		  is_reference = 0;
		}
	    }
	}
    }

  if (errstr)
    {
      error_with_decl (decl, errstr);
      if (TREE_CODE (decl) == VAR_DECL)
	SET_DECL_REFERENCE_SLOT (decl, error_mark_node);
      return;
    }

  /* In the case of initialization, it is permissable
     to assign one reference to another.  */
  this_ptr_type = build_pointer_type (TREE_TYPE (type));

  if (is_reference)
    {
      if (TREE_VOLATILE (init))
	DECL_INITIAL (decl) = save_expr (init);
      else
	DECL_INITIAL (decl) = init;
    }
  else if (lvalue_p (init))
    {
      DECL_INITIAL (decl) = convert_pointer_to (TREE_TYPE (this_ptr_type), build_unary_op (ADDR_EXPR, init, 0));
      DECL_INITIAL (decl) = save_expr (DECL_INITIAL (decl));
      if (DECL_INITIAL (decl) == current_class_decl)
	DECL_INITIAL (decl) = copy_node (current_class_decl);
      TREE_TYPE (DECL_INITIAL (decl)) = type;
    }
  else if ((actual_init = unary_complex_lvalue (ADDR_EXPR, init)))
    {
      /* The initializer for this decl goes into its
	 DECL_REFERENCE_SLOT.  Make sure that we can handle
	 multiple evaluations without ill effect.  */
      if (TREE_CODE (actual_init) == ADDR_EXPR
	  && TREE_CODE (TREE_OPERAND (actual_init, 0)) == NEW_EXPR)
	actual_init = save_expr (actual_init);
      DECL_INITIAL (decl) = convert_pointer_to (TREE_TYPE (this_ptr_type), actual_init);
      DECL_INITIAL (decl) = save_expr (DECL_INITIAL (decl));
      TREE_TYPE (DECL_INITIAL (decl)) = type;
    }
  else if (TREE_READONLY (TREE_TYPE (type)))
    /* Section 8.4.3 allows us to make a temporary for
       the initialization of const&.  */
    {
      tree target_type = TREE_TYPE (type);
      tree tmp_addr;
      tmp = get_temp_name (target_type,
			   current_binding_level == global_binding_level);
      tmp_addr = build_unary_op (ADDR_EXPR, tmp, 0);
      TREE_TYPE (tmp_addr) = build_pointer_type (target_type);
      DECL_INITIAL (decl) = convert_pointer_to (TREE_TYPE (this_ptr_type), tmp_addr);
      TREE_TYPE (DECL_INITIAL (decl)) = type;
      if (TYPE_NEEDS_CONSTRUCTING (target_type))
	{
	  if (current_binding_level == global_binding_level)
	    {
	      /* lay this variable out now.  Otherwise `output_addressed_constants'
		 gets confused by its initializer.  */
	      make_decl_rtl (tmp, 0, 1);
	      static_aggregates = perm_tree_cons (init, tmp, static_aggregates);
	    }
	  else
	    {
	      init = build_method_call (tmp, DECL_NAME (TYPE_NAME (target_type)), build_tree_list (NULL_TREE, init), NULL_TREE, LOOKUP_NORMAL);
	      DECL_INITIAL (decl) = build (COMPOUND_EXPR, type, init, DECL_INITIAL (decl));
	      *cleanupp = maybe_build_cleanup (tmp);
	    }
	}
      else
	{
	  DECL_INITIAL (tmp) = init;
	  TREE_STATIC (tmp) = current_binding_level == global_binding_level;
	  finish_decl (tmp, init, 0);
	}
    }
  else
    {
      error_with_decl (decl, "type mismatch in initialization of `%s' (use `const')");
      DECL_INITIAL (decl) = error_mark_node;
    }

  /* ?? Can this be optimized in some cases to
     hand back the DECL_INITIAL slot??  */
  if (TYPE_SIZE (TREE_TYPE (type)))
    SET_DECL_REFERENCE_SLOT (decl, convert_from_reference (decl));

  if (TREE_STATIC (decl) && ! TREE_LITERAL (DECL_INITIAL (decl)))
    {
      expand_static_init (decl, DECL_INITIAL (decl));
      DECL_INITIAL (decl) = 0;
    }
}

/* Finish processing of a declaration;
   install its line number and initial value.
   If the length of an array type is not known before,
   it must be determined now, from the initial value, or it is an error.

   For C++, `finish_decl' must be fairly evasive:  it must keep initializers
   for aggregates that have constructors alive on the permanent obstack,
   so that the global initializing functions can be written at the end.

   INIT0 holds the value of an initializer that should be allowed to escape
   the normal rules.

   For functions that take defualt parameters, DECL points to its
   "maximal" instantiation.  finish_decl must then also declared its
   subsequently lower and lower forms of instantiation, checking for
   ambiguity as it goes.  This can be sped up later.  */

void
finish_decl (decl, init, asmspec_tree)
     tree decl, init;
     tree asmspec_tree;
{
  register tree type;
  tree cleanup = NULL_TREE, ttype;
  int was_incomplete;
  int temporary = allocation_temporary_p ();
  char *asmspec = 0;
  int was_readonly = 0;

  /* If this is 0, then we did not change obstacks.  */
  if (! decl)
    {
      if (init)
	error ("assignment (not initialization) in declaration");
      return;
    }

  if (asmspec_tree)
    {
      asmspec = TREE_STRING_POINTER (asmspec_tree);
      /* Zero out old RTL, since we will rewrite it.  */
      DECL_RTL (decl) = 0;
    }

  /* If the type of the thing we are declaring either has
     a constructor, or has a virtual function table pointer,
     AND its initialization was accepted by `start_decl',
     then we stayed on the permanent obstack through the
     declaration, otherwise, changed obstacks as GCC would.  */

  type = TREE_TYPE (decl);

  was_incomplete = (DECL_SIZE (decl) == 0);

  /* Take care of TYPE_DECLs up front.  */
  if (TREE_CODE (decl) == TYPE_DECL)
    {
      if (init && DECL_INITIAL (decl))
	{
	  /* typedef foo = bar; store the type of bar as the type of foo.  */
	  TREE_TYPE (decl) = type = TREE_TYPE (init);
	  DECL_INITIAL (decl) = init = 0;
	}
      if (IS_AGGR_TYPE (type))
	{
#ifndef BREAK_C_TAGS
	  if (current_lang_name == lang_name_cplusplus)
#endif
	    {
	      if (TREE_TYPE (DECL_NAME (decl)) && TREE_TYPE (decl) != type)
		warning ("shadowing previous type declaration of `%s'",
			 IDENTIFIER_POINTER (DECL_NAME (decl)));
	      TREE_TYPE (DECL_NAME (decl)) = decl;
	    }
	  CLASSTYPE_GOT_SEMICOLON (type) = 1;
	}

#ifdef FIELD_XREF
      FIELD_xref_decl(current_function_decl,decl);
#endif

      rest_of_decl_compilation (decl, 0,
				current_binding_level == global_binding_level, 0);
      goto finish_end;
    }
  if (IS_AGGR_TYPE (type) && CLASSTYPE_DECLARED_EXCEPTION (type))
    {
      finish_exception_decl (NULL_TREE, decl);
      CLASSTYPE_GOT_SEMICOLON (type) = 1;
      goto finish_end;
    }
  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      ttype = target_type (type);
      if (TYPE_NAME (ttype)
	  && TREE_CODE (TYPE_NAME (ttype)) == TYPE_DECL
	  && ANON_AGGRNAME_P (DECL_NAME (TYPE_NAME (ttype))))
	{
	  tree old_id = DECL_NAME (TYPE_NAME (ttype));
	  char *newname = (char *)alloca (IDENTIFIER_LENGTH (old_id) + 2);
	  newname[0] = '_';
	  bcopy (IDENTIFIER_POINTER (old_id), newname + 1,
		 IDENTIFIER_LENGTH (old_id) + 1);
	  old_id = get_identifier (newname);
	  lookup_tag_reverse (ttype, old_id);
	  DECL_NAME (TYPE_NAME (ttype)) = old_id;
	}
    }

  if (! TREE_EXTERNAL (decl) && TREE_READONLY (decl)
      && TYPE_NEEDS_CONSTRUCTING (type))
    {

      /* Currently, GNU C++ puts constants in text space, making them
	 impossible to initialize.  In the future, one would hope for
	 an operating system which understood the difference between
	 initialization and the running of a program.  */
      was_readonly = 1;
      TREE_READONLY (decl) = 0;
    }

  if (TREE_CODE (decl) == FIELD_DECL)
    {
      if (init && init != error_mark_node)
	assert (TREE_PERMANENT (init));

      if (asmspec)
	{
	  /* This must override the asm specifier which was placed
	     by grokclassfn.  Lay this out fresh.
	     
	     @@ Should emit an error if this redefines an asm-specified
	     @@ name, or if we have already used the function's name.  */
	  DECL_RTL (TREE_TYPE (decl)) = 0;
	  DECL_ASSEMBLER_NAME (decl) = asmspec;
	  make_decl_rtl (decl, asmspec, 0);
	}
    }
  /* If `start_decl' didn't like having an initialization, ignore it now.  */
  else if (init != 0 && DECL_INITIAL (decl) == 0)
    init = 0;
  else if (TREE_EXTERNAL (decl))
    ;
  else if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      grok_reference_init (decl, type, init, &cleanup);
      init = 0;
    }

#ifdef FIELD_XREF
  FIELD_xref_decl(current_function_decl,decl);
#endif

  if (TREE_CODE (decl) == FIELD_DECL || TREE_EXTERNAL (decl))
    ;
  else if (TREE_CODE (decl) == CONST_DECL)
    {
      assert (TREE_CODE (decl) != REFERENCE_TYPE);

      DECL_INITIAL (decl) = init;

      /* This will keep us from needing to worry about our obstacks.  */
      assert (init != 0);
      init = 0;
    }
  else if (init)
    {
      if (TYPE_NEEDS_CONSTRUCTING (type))
	{
	  if (TREE_CODE (type) == ARRAY_TYPE)
	    init = digest_init (type, init, 0);
	  else if (TREE_CODE (init) == CONSTRUCTOR
		   && CONSTRUCTOR_ELTS (init) != NULL_TREE)
	    {
	      error_with_decl (decl, "`%s' must be initialized by constructor, not by `{...}'");
	      init = error_mark_node;
	    }
#if 0
	  /* fix this in `build_functional_cast' instead.
	     Here's the trigger code:

		struct ostream
		{
		  ostream ();
		  ostream (int, char *);
		  ostream (char *);
		  operator char *();
		  ostream (void *);
		  operator void *();
		  operator << (int);
		};
		int buf_size = 1024;
		static char buf[buf_size];
		const char *debug(int i) {
		  char *b = &buf[0];
		  ostream o = ostream(buf_size, b);
		  o << i;
		  return buf;
		}
		*/

	  else if (TREE_CODE (init) == NEW_EXPR
		   && TREE_CODE (TREE_OPERAND (init, 1) == CPLUS_NEW_EXPR))
	    {
	      /* User wrote something like `foo x = foo (args)'  */
	      assert (TREE_CODE (TREE_OPERAND (init, 0)) == VAR_DECL);
	      assert (DECL_NAME (TREE_OPERAND (init, 0)) == NULL_TREE);

	      /* User wrote exactly `foo x = foo (args)'  */
	      if (TYPE_MAIN_VARIANT (type) == TREE_TYPE (init))
		{
		  init = build (CALL_EXPR, TREE_TYPE (init),
				TREE_OPERAND (TREE_OPERAND (init, 1), 0),
				TREE_OPERAND (TREE_OPERAND (init, 1), 1), 0);
		  TREE_VOLATILE (init) = 1;
		}
	    }
#endif

	  /* We must hide the initializer so that expand_decl
	     won't try to do something it does not understand.  */
	  if (current_binding_level == global_binding_level)
	    {
	      tree value = digest_init (type, empty_init_node, 0);
	      DECL_INITIAL (decl) = value;
	    }
	  else
	    DECL_INITIAL (decl) = error_mark_node;
	}
      else
	{
	  if (TREE_CODE (init) != TREE_VEC)
	    init = store_init_value (decl, init);

	  if (init)
	    /* Don't let anyone try to initialize this variable
	       until we are ready to do so.  */
	    DECL_INITIAL (decl) = error_mark_node;
	}
    }
  else if (IS_AGGR_TYPE (type) || TYPE_NEEDS_CONSTRUCTING (type))
    {
      tree ctype = type;
      while (TREE_CODE (ctype) == ARRAY_TYPE)
	ctype = TREE_TYPE (ctype);
      if (! TYPE_NEEDS_CONSTRUCTOR (ctype))
	{
	  if (CLASSTYPE_READONLY_FIELDS_NEED_INIT (ctype))
	    error_with_decl (decl, "structure `%s' with uninitialized const members");
	  if (CLASSTYPE_REF_FIELDS_NEED_INIT (ctype))
	    error_with_decl (decl, "structure `%s' with uninitialized reference members");
	}

      if (TREE_CODE (decl) == VAR_DECL
	  && !TYPE_NEEDS_CONSTRUCTING (type)
	  && (TREE_READONLY (type) || TREE_READONLY (decl)))
	error_with_decl (decl, "uninitialized const `%s'");

      /* Initialize variables in need of static initialization
	 with `empty_init_node' to keep assemble_variable from putting them
	 in the wrong program space.  */
      if (TREE_STATIC (decl)
	  && TREE_CODE (decl) == VAR_DECL
	  && TYPE_NEEDS_CONSTRUCTING (type)
	  && (DECL_INITIAL (decl) == 0
	      || DECL_INITIAL (decl) == error_mark_node))
	{
	  tree value = digest_init (type, empty_init_node, 0);
	  DECL_INITIAL (decl) = value;
	}
    }
  else if (TREE_CODE (decl) == VAR_DECL
	   && TREE_CODE (type) != REFERENCE_TYPE
	   && (TREE_READONLY (type) || TREE_READONLY (decl)))
    {
      if (! TREE_STATIC (decl))
	error_with_decl (decl, "uninitialized const `%s'");
    }

  /* For top-level declaration, the initial value was read in
     the temporary obstack.  MAXINDEX, rtl, etc. to be made below
     must go in the permanent obstack; but don't discard the
     temporary data yet.  */

  if (current_binding_level == global_binding_level && temporary)
    end_temporary_allocation ();

  /* Deduce size of array from initialization, if not already known.  */

  if (TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == 0
      && TREE_CODE (decl) != TYPE_DECL)
    {
      int do_default = ! ((!pedantic && TREE_STATIC (decl))
			  || TREE_EXTERNAL (decl));
      tree initializer = init ? init : DECL_INITIAL (decl);
      int failure = complete_array_type (type, initializer, do_default);

      if (failure == 1)
	error_with_decl (decl, "initializer fails to determine size of `%s'");

      if (failure == 2)
	{
	  if (do_default)
	    error_with_decl (decl, "array size missing in `%s'");
	  else if (!pedantic && TREE_STATIC (decl))
	    TREE_EXTERNAL (decl) = 1;
	}

      if (pedantic && TYPE_DOMAIN (type) != 0
	  && tree_int_cst_lt (TYPE_MAX_VALUE (TYPE_DOMAIN (type)),
			      integer_zero_node))
	error_with_decl (decl, "zero-size array `%s'");

      layout_decl (decl, 0);
    }

  if (TREE_CODE (decl) == VAR_DECL)
    {
      if (TREE_STATIC (decl) && DECL_SIZE (decl) == 0)
	{
	  /* A static variable with an incomplete type:
	     that is an error if it is initialized or `static'.
	     Otherwise, let it through, but if it is not `extern'
	     then it may cause an error message later.  */
	  if (! (TREE_PUBLIC (decl) && DECL_INITIAL (decl) == 0))
	    error_with_decl (decl, "storage size of `%s' isn't known");
	  init = 0;
	}
      else if (!TREE_EXTERNAL (decl) && DECL_SIZE (decl) == 0)
	{
	  /* An automatic variable with an incomplete type:
	     that is an error.  */
	  error_with_decl (decl, "storage size of `%s' isn't known");
	  TREE_TYPE (decl) = error_mark_node;
	}

      if ((TREE_EXTERNAL (decl) || TREE_STATIC (decl))
	  && DECL_SIZE (decl) != 0 && ! TREE_LITERAL (DECL_SIZE (decl)))
	error_with_decl (decl, "storage size of `%s' isn't constant");

      if (TYPE_NEEDS_DESTRUCTOR (type))
	{
	  int yes = suspend_momentary ();
	  cleanup = maybe_build_cleanup (decl);
	  resume_momentary (yes);
	}
    }
  /* PARM_DECLs get cleanups, too.  */
  else if (TREE_CODE (decl) == PARM_DECL
	   && TYPE_NEEDS_DESTRUCTOR (type))
    {
      if (temporary)
	end_temporary_allocation ();
      cleanup = maybe_build_cleanup (decl);
      if (temporary)
	resume_temporary_allocation ();
    }

  /* Output the assembler code and/or RTL code for variables and functions,
     unless the type is an undefined structure or union.
     If not, it will get done when the type is completed.  */

  if (TREE_CODE (decl) == VAR_DECL || TREE_CODE (decl) == FUNCTION_DECL
      || TREE_CODE (decl) == RESULT_DECL)
    {
      int toplev = current_binding_level == global_binding_level;
      int was_temp
	= ((flag_traditional
	    || (TREE_STATIC (decl) && TYPE_NEEDS_DESTRUCTOR (type)))
	   && allocation_temporary_p ());

      if (was_temp)
	end_temporary_allocation ();

      /* If we are in need of a cleanup, get out of any implicit
	 handlers that have been established so far.  */
      if (cleanup && current_binding_level->parm_flag == 3)
	{
	  pop_implicit_try_blocks (decl);
	  current_binding_level->more_exceptions_ok = 0;
	}

      if (TREE_CODE (decl) == VAR_DECL && DECL_VIRTUAL_P (decl))
	make_decl_rtl (decl, 0, toplev);
      else if (TREE_CODE (decl) == VAR_DECL
	       && TREE_READONLY (decl)
	       && DECL_INITIAL (decl) != 0
	       && DECL_INITIAL (decl) != error_mark_node
	       && DECL_INITIAL (decl) != empty_init_node)
	{
	  DECL_INITIAL (decl) = save_expr (DECL_INITIAL (decl));

	  if (asmspec)
	    DECL_ASSEMBLER_NAME (decl) = asmspec;

	  if (! toplev
	      && TREE_STATIC (decl)
	      && ! TREE_VOLATILE (decl)
	      && ! TREE_PUBLIC (decl)
	      && ! TREE_EXTERNAL (decl)
	      && ! TYPE_NEEDS_DESTRUCTOR (type)
	      && DECL_MODE (decl) != BLKmode)
	    {
	      /* If this variable is really a constant, then fill its DECL_RTL
		 slot with something which won't take up storage.
		 If something later should take its address, we can always give
		 it legitimate RTL at that time.  */
	      DECL_RTL (decl) = (struct rtx_def *)gen_reg_rtx (DECL_MODE (decl));
	      store_expr (DECL_INITIAL (decl), DECL_RTL (decl), 0);
	      TREE_ASM_WRITTEN (decl) = 1;
	    }
	  else if (toplev)
	    {
	      /* Keep GCC from complaining that this variable
		 is defined but never used.  */
	      TREE_USED (decl) = 1;
	      make_decl_rtl (decl, asmspec, toplev);
	    }
	  else
	    rest_of_decl_compilation (decl, asmspec, toplev, 0);
	}
      else if (TREE_CODE (decl) == VAR_DECL
	       && DECL_LANG_SPECIFIC (decl)
	       && DECL_IN_AGGR_P (decl))
	{
	  if (TREE_STATIC (decl))
	    if (init == 0 && TYPE_NEEDS_CONSTRUCTING (type))
	      {
		TREE_EXTERNAL (decl) = 1;
		make_decl_rtl (decl, asmspec, 1);
	      }
	    else
	      rest_of_decl_compilation (decl, asmspec, toplev, 0);
	  else
	    /* Just a constant field.  Should not need any rtl.  */
	    goto finish_end0;
	}
      else
	rest_of_decl_compilation (decl, asmspec, toplev, 0);

      if (was_temp)
	resume_temporary_allocation ();

      if (TYPE_LANG_SPECIFIC (type) && CLASSTYPE_ABSTRACT_VIRTUALS (type))
	abstract_virtuals_error (decl, type);
      else if (TREE_CODE (type) == FUNCTION_TYPE
	       && TYPE_LANG_SPECIFIC (TREE_TYPE (type))
	       && CLASSTYPE_ABSTRACT_VIRTUALS (TREE_TYPE (type)))
	abstract_virtuals_error (decl, TREE_TYPE (type));

      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  /* C++: Handle overloaded functions with defualt parameters.  */
	  if (DECL_OVERLOADED (decl))
	    {
	      tree parmtypes = TYPE_ARG_TYPES (type);
	      tree prev = NULL_TREE;
	      char *original_name = IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (decl));
	      struct lang_decl *tmp_lang_decl = DECL_LANG_SPECIFIC (decl);
	      /* All variants will share an uncollectable lang_decl.  */
	      copy_decl_lang_specific (decl);

	      while (parmtypes && parmtypes != void_list_node)
		{
		  if (TREE_PURPOSE (parmtypes))
		    {
		      tree fnname, fndecl;
		      tree *argp = prev
			? & TREE_CHAIN (prev)
			  : & TYPE_ARG_TYPES (type);

		      *argp = NULL_TREE;
		      fnname = build_decl_overload (original_name, TYPE_ARG_TYPES (type), 0);
		      *argp = parmtypes;
		      fndecl = build_decl (FUNCTION_DECL, fnname, type);
		      TREE_EXTERNAL (fndecl) = TREE_EXTERNAL (decl);
		      TREE_PUBLIC (fndecl) = TREE_PUBLIC (decl);
		      TREE_INLINE (fndecl) = TREE_INLINE (decl);
		      /* Keep G++ from thinking this function is unused.
			 It is only used to speed up search in name space.  */
		      TREE_USED (fndecl) = 1;
		      TREE_ASM_WRITTEN (fndecl) = 1;
		      DECL_INITIAL (fndecl) = NULL_TREE;
		      DECL_LANG_SPECIFIC (fndecl) = DECL_LANG_SPECIFIC (decl);
		      fndecl = pushdecl (fndecl);
		      DECL_INITIAL (fndecl) = error_mark_node;
		      DECL_RTL (fndecl) = DECL_RTL (decl);
		    }
		  prev = parmtypes;
		  parmtypes = TREE_CHAIN (parmtypes);
		}
	      DECL_LANG_SPECIFIC (decl) = tmp_lang_decl;
	    }
	}
      else if (TREE_EXTERNAL (decl))
	;
      else if (TREE_STATIC (decl))
	{
	  /* Cleanups for static variables are handled by `finish_file'.  */
	  if (TYPE_NEEDS_CONSTRUCTING (type) || init != NULL_TREE)
	    expand_static_init (decl, init);
	}
      else if (current_binding_level != global_binding_level)
	{
	  /* This is a declared decl which must live until the
	     end of the binding contour.  It may need a cleanup.  */

	  /* Recompute the RTL of a local array now
	     if it used to be an incomplete type.  */
	  if (was_incomplete && ! TREE_STATIC (decl))
	    {
	      /* If we used it already as memory, it must stay in memory.  */
	      TREE_ADDRESSABLE (decl) = TREE_USED (decl);
	      /* If it's still incomplete now, no init will save it.  */
	      if (DECL_SIZE (decl) == 0)
		DECL_INITIAL (decl) = 0;
	      expand_decl (decl);
	    }
	  else if (! TREE_ASM_WRITTEN (decl)
		   && (TYPE_SIZE (type) != 0 || TREE_CODE (type) == ARRAY_TYPE))
	    {
	      /* Do this here, because we did not expand this decl's
		 rtl in start_decl.  */
	      if (DECL_RTL (decl) == 0)
		expand_decl (decl);
	      else if (cleanup)
		{
		  expand_decl_cleanup (NULL_TREE, cleanup);
		  /* Cleanup used up here.  */
		  cleanup = 0;
		}
	    }

	  /* Compute and store the initial value.  */
	  expand_decl_init (decl);

	  if (init || TYPE_NEEDS_CONSTRUCTING (type))
	    {
	      emit_line_note (DECL_SOURCE_FILE (decl), DECL_SOURCE_LINE (decl));
	      expand_aggr_init (decl, init, 0);
	    }

	  /* Set this to 0 so we can tell whether an aggregate
	     which was initialized was ever used.  */
	  if (TYPE_NEEDS_CONSTRUCTING (type))
	    TREE_USED (decl) = 0;

	  /* Store the cleanup, if there was one.  */
	  if (cleanup)
	    {
	      if (! expand_decl_cleanup (decl, cleanup))
		error_with_decl (decl, "parser lost in parsing declaration of `%s'");
	    }
	}
    finish_end0:

      /* Undo call to `pushclass' that was done in `start_decl'
	 due to initialization of qualified member variable.
	 I.e., Foo::x = 10;  */
      if (TREE_CODE (decl) == VAR_DECL && DECL_CONTEXT (decl))
	popclass (1);
    }

 finish_end:

  /* Resume permanent allocation, if not within a function.  */
  if (temporary && current_binding_level == global_binding_level)
    {
      permanent_allocation ();
#if 0
      /* @@ I don't know whether this is true for GNU C++.  */
      /* We need to remember that this array HAD an initialization,
	 but discard the actual nodes, since they are temporary anyway.  */
      if (DECL_INITIAL (decl) != 0)
	DECL_INITIAL (decl) = error_mark_node;
#endif
    }
  if (was_readonly)
    TREE_READONLY (decl) = 1;

  if (flag_cadillac)
    cadillac_finish_decl (decl);
}

static void
expand_static_init (decl, init)
     tree decl;
     tree init;
{
  tree oldstatic = value_member (decl, static_aggregates);
  if (oldstatic)
    {
      if (TREE_PURPOSE (oldstatic))
	error_with_decl (decl, "multiple initializations given for `%s'");
    }
  else if (current_binding_level != global_binding_level)
    {
      /* Emit code to perform this initialization but once.  */
      tree temp = get_temp_name (integer_type_node, 1);
      rest_of_decl_compilation (temp, NULL_TREE, 0, 0);
      expand_start_cond (build_binary_op (EQ_EXPR, temp, integer_zero_node), 0);
      expand_assignment (temp, integer_one_node, 0, 0);
      if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl)))
	{
	  expand_aggr_init (decl, init, 0);
	  do_pending_stack_adjust ();
	}
      else
	expand_assignment (decl, init, 0, 0);
      expand_end_cond ();
      if (TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (decl)))
	{
	  int temporary = allocation_temporary_p ();
	  if (temporary)
	    end_temporary_allocation ();
	  static_aggregates = tree_cons (temp, decl, static_aggregates);
	  TREE_STATIC (static_aggregates) = 1;
	  if (temporary)
	    resume_temporary_allocation ();
	}
    }
  else
    {
      /* This code takes into account memory allocation
	 policy of `start_decl'.  Namely, if TYPE_NEEDS_CONSTRUCTING
	 does not hold for this object, then we must make permanent
	 the storage currently in the temporary obstack.  */
      if (! TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (decl)))
	preserve_initializer ();
      static_aggregates = perm_tree_cons (init, decl, static_aggregates);
    }
}

/* Make TYPE a complete type based on INITIAL_VALUE.
   Return 0 if successful, 1 if INITIAL_VALUE can't be decyphered,
   2 if there was no information (in which case assume 1 if DO_DEFAULT).  */

int
complete_array_type (type, initial_value, do_default)
     tree type;
     tree initial_value;
     int do_default;
{
  register tree maxindex = NULL_TREE;
  int value = 0;
  int temporary = (TREE_PERMANENT (type) && allocation_temporary_p ());

  /* Don't put temporary nodes in permanent type.  */
  if (temporary)
    end_temporary_allocation ();

  if (initial_value)
    {
      /* Note MAXINDEX  is really the maximum index,
	 one less than the size.  */
      if (TREE_CODE (initial_value) == STRING_CST)
	maxindex = build_int_2 (TREE_STRING_LENGTH (initial_value) - 1, 0);
      else if (TREE_CODE (initial_value) == CONSTRUCTOR)
	{
	  register int nelts
	    = list_length (CONSTRUCTOR_ELTS (initial_value));
	  maxindex = build_int_2 (nelts - 1, 0);
	}
      else
	{
	  /* Make an error message unless that happened already.  */
	  if (initial_value != error_mark_node)
	    value = 1;

	  /* Prevent further error messages.  */
	  maxindex = build_int_2 (1, 0);
	}
    }

  if (!maxindex)
    {
      if (do_default)
	maxindex = build_int_2 (1, 0);
      value = 2;
    }

  if (maxindex)
    {
      TYPE_DOMAIN (type) = build_index_type (maxindex);
      if (!TREE_TYPE (maxindex))
	TREE_TYPE (maxindex) = TYPE_DOMAIN (type);
    }

  /* Lay out the type now that we can get the real answer.  */

  layout_type (type);

  if (temporary)
    resume_temporary_allocation ();

  return value;
}

/* Return zero if something is declared to be a member of type
   CTYPE when in the context of CUR_TYPE.  STRING is the error
   message to print in that case.  Otherwise, quietly return 1.  */
static int
member_function_or_else (ctype, cur_type, string)
     tree ctype, cur_type;
     char *string;
{
  if (ctype && ctype != cur_type)
    {
      error (string, TYPE_NAME_STRING (ctype));
      return 0;
    }
  return 1;
}

/* Subroutine of `grokdeclarator'.  */

/* CTYPE is class type, or null if non-class.
   TYPE is type this FUNCTION_DECL should have, either FUNCTION_TYPE
   or METHOD_TYPE.
   DECLARATOR is the function's name.
   VIRTUALP is truthvalue of whether the function is virtual or not.
   FLAGS are to be passed through to `grokclassfn'.
   QUALS are qualifiers indicating whether the function is `const'
   or `volatile'.
   RAISES is a list of exceptions that this function can raise.
   CHECK is 1 if we must find this method in CTYPE, 0 if we should
   not look, and -1 if we should not call `grokclassfn' at all.  */
static tree
grokfndecl (ctype, type, declarator, virtualp, flags, quals, raises, check)
     tree ctype, type;
     tree declarator;
     int virtualp;
     enum overload_flags flags;
     tree quals, raises;
     int check;
{
  tree cname, decl;
  int staticp = ctype && TREE_CODE (type) == FUNCTION_TYPE;

  if (ctype)
    cname = TREE_CODE (TYPE_NAME (ctype)) == TYPE_DECL
      ? DECL_NAME (TYPE_NAME (ctype)) : TYPE_NAME (ctype);
  else
    cname = NULL_TREE;

  if (raises)
    {
      type = build_exception_variant (ctype, type, raises);
      raises = TYPE_RAISES_EXCEPTIONS (type);
    }
  decl = build_lang_decl (FUNCTION_DECL, declarator, type);
  if (staticp)
    {
      DECL_STATIC_FUNCTION_P (decl) = 1;
      DECL_STATIC_CONTEXT (decl) = ctype;
    }
  TREE_EXTERNAL (decl) = 1;
  if (quals != NULL_TREE && TREE_CODE (type) == FUNCTION_TYPE)
    {
      /* Dont have DECL_ORIGINAL_NAME yet, so we cannot pretty print it.  */
      error ("functions cannot have method qualifiers");
      quals = NULL_TREE;
    }

  /* Caller will do the rest of this.  */
  if (check < 0)
    return decl;

  if (flags == NO_SPECIAL && ctype && declarator == cname)
    {
      tree tmp;
      /* Just handle constructors here.  We could do this
	 inside the following if stmt, but I think
	 that the code is more legible by breaking this
	 case out.  See comments below for what each of
	 the following calls is supposed to do.  */
      DECL_CONSTRUCTOR_P (decl) = 1;

      grokclassfn (ctype, declarator, decl, flags, check, quals);
      grok_ctor_properties (ctype, decl);
      if (check == 0)
	{
	  tmp = lookup_name (DECL_NAME (decl));
	  if (tmp == 0)
	    IDENTIFIER_GLOBAL_VALUE (DECL_NAME (decl)) = decl;
	  else if (TREE_CODE (tmp) != TREE_CODE (decl))
	    error_with_decl (decl, "inconsistant declarations for `%s'");
	  else
	    {
	      duplicate_decls (decl, tmp);
	      decl = tmp;
	    }
	  make_decl_rtl (decl, NULL_TREE, 1);
	}
    }
  else
    {
      tree tmp;
      int i;

      /* Function gets the ugly name, field gets the nice one.
	 This call may change the type of the function (because
	 of default parameters)!
	 
	 Wrappers get field names which will not conflict
	 with constructors and destructors.  */
      if (ctype != NULL_TREE)
	grokclassfn (ctype, cname, decl, flags, check, quals);

      if (OPERATOR_NAME_P (DECL_NAME (decl)))
	{
	  TREE_OPERATOR (decl) = 1;
	  grok_op_properties (decl);
	}

      if (ctype == NULL_TREE || check)
	return decl;

      /* Now install the declaration of this function so that
	 others may find it (esp. its DECL_FRIENDLIST).
	 Pretend we are at top level, we will get true
	 reference later, perhaps.  */
      tmp = lookup_name (DECL_NAME (decl));
      if (tmp == 0)
	IDENTIFIER_GLOBAL_VALUE (DECL_NAME (decl)) = decl;
      else if (TREE_CODE (tmp) != TREE_CODE (decl))
	error_with_decl (decl, "inconsistant declarations for `%s'");
      else
	{
	  duplicate_decls (decl, tmp);
	  decl = tmp;
	}
      make_decl_rtl (decl, NULL_TREE, 1);

      /* If this declaration supersedes the declaration of
	 a method declared virtual in the base class, then
	 mark this field as being virtual as well.  */
      for (i = 1; i <= CLASSTYPE_N_BASECLASSES (ctype); i++)
	{
	  tree basetype = CLASSTYPE_BASECLASS (ctype, i);
	  if (TYPE_VIRTUAL_P (basetype) || flag_all_virtual == 1)
	    {
	      tmp = get_first_matching_virtual (basetype, decl,
						flags == DTOR_FLAG);
	      if (tmp)
		{
		  /* The TMP we really want is the one from the deepest
		     baseclass on this path, taking care not to
		     duplicate if we have already found it (via another
		     path to its virtual baseclass.  */
		  if (staticp)
		    {
		      error_with_decl (decl, "method `%s' may not be declared static");
		      error_with_decl (tmp, "(since `%s' declared virtual in base class.)");
		      break;
		    }
		  virtualp = 1;

		  if ((TYPE_USES_VIRTUAL_BASECLASSES (basetype)
		       || TYPE_USES_MULTIPLE_INHERITANCE (ctype))
		      && TYPE_MAIN_VARIANT (basetype) != DECL_VCONTEXT (tmp))
		    tmp = get_first_matching_virtual (DECL_VCONTEXT (tmp),
						      decl, flags == DTOR_FLAG);
		  if (value_member (tmp, DECL_VINDEX (decl)) == NULL_TREE)
		    {
		      /* The argument types may have changed... */
		      tree argtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));
		      tree base_variant = TREE_TYPE (TREE_VALUE (argtypes));

		      argtypes = commonparms (TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (tmp))),
					      TREE_CHAIN (argtypes));
		      /* But the return type has not.  */
		      type = build_cplus_method_type (base_variant, TREE_TYPE (type), argtypes);
		      if (raises)
			{
			  type = build_exception_variant (ctype, type, raises);
			  raises == TYPE_RAISES_EXCEPTIONS (type);
			}
		      TREE_TYPE (decl) = type;
		      SET_DECL_VINDEX (decl, tree_cons (NULL_TREE, tmp, DECL_VINDEX (decl)));
		    }
		}
	    }
	}
      if (virtualp)
	{
	  DECL_VIRTUAL_P (decl) = 1;
	  DECL_VIRTUAL_P (declarator) = 1;
	  if (ctype && CLASSTYPE_VTABLE_NEEDS_WRITING (ctype)
	      && (write_virtuals == 2
		  || (write_virtuals == 3
		      && ! CLASSTYPE_INTERFACE_UNKNOWN (ctype))))
	    TREE_PUBLIC (decl) = 1;
	}
    }
  return decl;
}

static tree
grokvardecl (ctype, type, declarator, specbits, initialized)
     tree ctype, type;
     tree declarator;
     int specbits;
{
  tree decl;

  if (TREE_CODE (type) == OFFSET_TYPE)
    {
      /* If you declare a static member so that it
	 can be initialized, the code will reach here.  */
      tree field = lookup_field (TYPE_OFFSET_BASETYPE (type),
				 declarator, 0);
      if (field == NULL_TREE || TREE_CODE (field) != VAR_DECL)
	{
	  tree basetype = TYPE_OFFSET_BASETYPE (type);
	  error ("`%s' is not a static member of class `%s'",
		 IDENTIFIER_POINTER (declarator),
		 TYPE_NAME_STRING (basetype));
	  type = TREE_TYPE (type);
	  decl = build_decl (VAR_DECL, declarator, type);
	  DECL_CONTEXT (decl) = basetype;
	}
      else
	{
	  decl = field;
	  if (initialized && DECL_INITIAL (decl)
	      /* Complain about multiply-initialized
		 member variables, but don't be faked
		 out if initializer is faked up from `empty_init_node'.  */
	      && (TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR
		  || CONSTRUCTOR_ELTS (DECL_INITIAL (decl)) != NULL_TREE))
	    error_with_aggr_type (DECL_CONTEXT (decl),
				  "multiple initializations of static member `%s::%s'",
				  IDENTIFIER_POINTER (DECL_NAME (decl)));
	}
    }
  else decl = build_decl (VAR_DECL, declarator, type);

  if (specbits & (1 << (int) RID_EXTERN))
    {
      DECL_EXTERNAL (decl) = 1;
      TREE_EXTERNAL (decl) = !initialized;
    }

  /* In class context, static means one per class,
     public visibility, and static storage.  */
  if (DECL_FIELD_CONTEXT (decl) != 0
      && IS_AGGR_TYPE (DECL_FIELD_CONTEXT (decl)))
    {
      TREE_PUBLIC (decl) = 1;
      TREE_STATIC (decl) = 1;
    }
  /* At top level, either `static' or no s.c. makes a definition
     (perhaps tentative), and absence of `static' makes it public.  */
  else if (current_binding_level == global_binding_level)
    {
      TREE_PUBLIC (decl) = !(specbits & (1 << (int) RID_STATIC));
      TREE_STATIC (decl) = ! TREE_EXTERNAL (decl);
    }
  /* Not at top level, only `static' makes a static definition.  */
  else
    {
      TREE_STATIC (decl) = (specbits & (1 << (int) RID_STATIC)) != 0;
      TREE_PUBLIC (decl) = TREE_EXTERNAL (decl);
      /* `extern' with initialization is invalid if not at top level.  */
      if ((specbits & (1 << (int) RID_EXTERN)) && initialized)
	error_with_decl (decl, "`%s' has both `extern' and initializer");
    }
  return decl;
}

/* Given declspecs and a declarator,
   determine the name and type of the object declared
   and construct a ..._DECL node for it.
   (In one case we can return a ..._TYPE node instead.
    For invalid input we sometimes return 0.)

   DECLSPECS is a chain of tree_list nodes whose value fields
    are the storage classes and type specifiers.

   DECL_CONTEXT says which syntactic context this declaration is in:
     NORMAL for most contexts.  Make a VAR_DECL or FUNCTION_DECL or TYPE_DECL.
     FUNCDEF for a function definition.  Like NORMAL but a few different
      error messages in each case.  Return value may be zero meaning
      this definition is too screwy to try to parse.
     MEMFUNCDEF for a function definition.  Like FUNCDEF but prepares to
      handle member functions (which have FIELD context).
      Return value may be zero meaning this definition is too screwy to
      try to parse.
     PARM for a parameter declaration (either within a function prototype
      or before a function body).  Make a PARM_DECL, or return void_type_node.
     TYPENAME if for a typename (in a cast or sizeof).
      Don't make a DECL node; just return the ..._TYPE node.
     FIELD for a struct or union field; make a FIELD_DECL.
   INITIALIZED is 1 if the decl has an initializer.

   In the TYPENAME case, DECLARATOR is really an absolute declarator.
   It may also be so in the PARM case, for a prototype where the
   argument type is specified but not the name.

   This function is where the complicated C meanings of `static'
   and `extern' are intrepreted.

   For C++, if there is any monkey business to do, the function which
   calls this one must do it, i.e., prepending instance variables,
   renaming overloaded function names, etc.

   Note that for this C++, it is an error to define a method within a class
   which does not belong to that class.

   Execpt in the case where SCOPE_REFs are implicitly known (such as
   methods within a class being redundantly qualified),
   declarations which involve SCOPE_REFs are returned as SCOPE_REFs
   (class_name::decl_name).  The caller must also deal with this.

   If a constructor or destructor is seen, and the context is FIELD,
   then the type gains the attribtue TREE_HAS_x.  If such a declaration
   is erroneous, NULL_TREE is returned.

   QUALS is used only for FUNCDEF and MEMFUNCDEF cases.  For a member
   function, these are the qualifiers to give to the `this' pointer.

   May return void_type_node if the declarator turned out to be a friend.
   See grokfield for details.  */

enum return_types { return_normal, return_ctor, return_dtor, return_conversion, };

tree
grokdeclarator (declarator, declspecs, decl_context, initialized, raises)
     tree declspecs;
     tree declarator;
     enum decl_context decl_context;
     int initialized;
     tree raises;
{
  int specbits = 0;
  int nclasses = 0;
  tree spec;
  tree type = NULL_TREE;
  int longlong = 0;
  int constp;
  int volatilep;
  int virtualp, friendp, inlinep, staticp;
  int explicit_int = 0;
  int explicit_char = 0;
  char *name;
  tree typedef_type = 0;
  int funcdef_flag = 0;
  int resume_temporary = 0;
  enum tree_code innermost_code = ERROR_MARK;
  /* Set this to error_mark_node for FIELD_DECLs we could not handle properly.
     All FIELD_DECLs we build here have `init' put into their DECL_INITIAL.  */
  tree init = 0;

  /* Keep track of what sort of function is being processed
     so that we can warn about default return values, or explicit
     return values which do not match prescribed defaults.  */
  enum return_types return_type = return_normal;

  tree dname = NULL_TREE;
  tree ctype = current_class_type;
  tree ctor_return_type = NULL_TREE;
  enum overload_flags flags = NO_SPECIAL;
  int seen_scope_ref = 0;
  tree quals = 0;

  if (decl_context == FUNCDEF)
    funcdef_flag = 1, decl_context = NORMAL;
  else if (decl_context == MEMFUNCDEF)
    funcdef_flag = -1, decl_context = FIELD;

  if (flag_traditional && allocation_temporary_p ())
    {
      resume_temporary = 1;
      end_temporary_allocation ();
    }

  /* Look inside a declarator for the name being declared
     and get it as a string, for an error message.  */
  {
    tree type, last = 0;
    register tree decl = declarator;
    name = 0;

    /* If we see something of the form `aggr_type xyzzy (a, b, c)'
       it is either an old-style function declaration or a call to
       a constructor.  The following conditional makes recognizes this
       case as being a call to a constructor.  Too bad if it is not.  */

    /* For Doug Lea, also grok `aggr_type xyzzy (a, b, c)[10][10][10]'.  */
    while (decl && TREE_CODE (decl) == ARRAY_REF)
      {
	last = decl;
	decl = TREE_OPERAND (decl, 0);
      }

    if (current_lang_name == lang_name_cplusplus
        && decl && declspecs
        && TREE_CODE (decl) == CALL_EXPR
        && TREE_OPERAND (decl, 0)
        && (TREE_CODE (TREE_OPERAND (decl, 0)) == IDENTIFIER_NODE
	    || TREE_CODE (TREE_OPERAND (decl, 0)) == SCOPE_REF))
      {
        type = TREE_CODE (TREE_VALUE (declspecs)) == IDENTIFIER_NODE
          ? lookup_name (TREE_VALUE (declspecs)) :
        (IS_AGGR_TYPE (TREE_VALUE (declspecs))
         ? TYPE_NAME (TREE_VALUE (declspecs)) : NULL_TREE);

        if (type && TREE_CODE (type) == TYPE_DECL
            && IS_AGGR_TYPE (TREE_TYPE (type))
            && parmlist_is_exprlist (TREE_OPERAND (decl, 1)))
          {
            if (decl_context == FIELD
                && TREE_CHAIN (TREE_OPERAND (decl, 1)))
              {
                /* That was an initializer list.  */
                sorry ("initializer lists for field declarations");
                decl = TREE_OPERAND (decl, 0);
		if (last)
		  {
		    TREE_OPERAND (last, 0) = decl;
		    decl = declarator;
		  }
                declarator = decl;
                init = error_mark_node;
                goto bot;
              }
            else
              {
		tree init = TREE_OPERAND (decl, 1);
		if (last)
		  {
		    TREE_OPERAND (last, 0) = TREE_OPERAND (decl, 0);
		    if (pedantic && init)
		      error ("arrays cannot take initializers");
		  }
		else
		  declarator = TREE_OPERAND (declarator, 0);
                decl = start_decl (declarator, declspecs, 1, NULL_TREE);
                finish_decl (decl, init, NULL_TREE);
                return 0;
              }
          }

        if (parmlist_is_random (TREE_OPERAND (decl, 1)))
          {
	    decl = TREE_OPERAND (decl, 0);
	    if (TREE_CODE (decl) == SCOPE_REF)
	      decl = TREE_OPERAND (decl, 1);
	    if (TREE_CODE (decl) == IDENTIFIER_NODE)
	      name = IDENTIFIER_POINTER (decl);
	    if (name)
	      error ("bad parameter list specification for function `%s'",
		     name);
	    else
	      error ("bad parameter list specification for function");
            return 0;
          }
      bot:
        ;
      }
    else
      /* It didn't look like we thought it would, leave the ARRAY_REFs on.  */
      decl = declarator;

    while (decl)
      switch (TREE_CODE (decl))
        {
	case WRAPPER_EXPR:      /* for C++ wrappers.  */
	  if (current_lang_name != lang_name_cplusplus)
	    error ("wrapper declared in \"%s\" language context",
		   IDENTIFIER_POINTER (current_lang_name));

	  ctype = NULL_TREE;
	  assert (flags == NO_SPECIAL);
	  flags = WRAPPER_FLAG;
	  decl = TREE_OPERAND (decl, 0);
	  break;

	case ANTI_WRAPPER_EXPR: /* for C++ wrappers.  */
	  if (current_lang_name != lang_name_cplusplus)
	    error ("anti-wrapper declared in \"%s\" language context",
		   IDENTIFIER_POINTER (current_lang_name));

	  ctype = NULL_TREE;
	  assert (flags == NO_SPECIAL);
	  flags = ANTI_WRAPPER_FLAG;
	  decl = TREE_OPERAND (decl, 0);
	  break;

	case COND_EXPR:
	  if (current_lang_name != lang_name_cplusplus)
	    error ("wrapper predicate declared in \"%s\" language context",
		   IDENTIFIER_POINTER (current_lang_name));

	  ctype = NULL_TREE;
	  assert (flags == WRAPPER_FLAG);
	  flags = WRAPPER_PRED_FLAG;
	  decl = TREE_OPERAND (decl, 0);
	  break;

	case BIT_NOT_EXPR:      /* for C++ destructors!  */
	  {
	    tree name = TREE_OPERAND (decl, 0);
	    tree rename = NULL_TREE;

	    if (current_lang_name != lang_name_cplusplus)
	      error ("destructor declared in \"%s\" language context",
		     IDENTIFIER_POINTER (current_lang_name));

	    assert (flags == NO_SPECIAL);
	    flags = DTOR_FLAG;
	    return_type = return_dtor;
	    assert (TREE_CODE (name) == IDENTIFIER_NODE);
	    if (ctype == NULL_TREE)
	      {
		if (current_class_type == NULL_TREE)
		  {
		    error ("destructors must be member functions");
		    flags = NO_SPECIAL;
		  }
		else if (current_class_name != name)
		  rename = current_class_name;
	      }
	    else if (DECL_NAME (TYPE_NAME (ctype)) != name)
	      rename = DECL_NAME (TYPE_NAME (ctype));

	    if (rename)
	      {
		error ("destructor `%s' must match class name `%s'",
		       IDENTIFIER_POINTER (name),
		       IDENTIFIER_POINTER (rename));
		TREE_OPERAND (decl, 0) = rename;
	      }
	    decl = name;
	  }
	  break;

	case ADDR_EXPR:         /* C++ reference declaration */
	  /* fall through */
	case ARRAY_REF:
	case INDIRECT_REF:
	  ctype = NULL_TREE;
	  innermost_code = TREE_CODE (decl);
	  decl = TREE_OPERAND (decl, 0);
	  break;

	case CALL_EXPR:
	  innermost_code = TREE_CODE (decl);
	  decl = TREE_OPERAND (decl, 0);
	  if (decl_context == FIELD && ctype == NULL_TREE)
	    ctype = current_class_type;
	  if (ctype != NULL_TREE
	      && decl != NULL_TREE && flags != DTOR_FLAG
	      && TREE_TYPE (decl) && TREE_TYPE (TREE_TYPE (decl)) == ctype)
	    {
	      return_type = return_ctor;
	      ctor_return_type = ctype;
	    }
	  ctype = NULL_TREE;
	  break;

	case IDENTIFIER_NODE:
	  dname = decl;
	  name = IDENTIFIER_POINTER (decl);
	  decl = 0;
	  break;

	case RECORD_TYPE:
	case UNION_TYPE:
	case ENUMERAL_TYPE:
	  /* Parse error puts this typespec where
	     a declarator should go.  */
	  error ("declarator name missing");
	  dname = TYPE_NAME (decl);
	  if (dname && TREE_CODE (dname) == TYPE_DECL)
	    dname = DECL_NAME (dname);
	  name = dname ? IDENTIFIER_POINTER (dname) : "<nameless>";
	  declspecs = temp_tree_cons (NULL_TREE, decl, declspecs);
	  decl = 0;
	  break;

	case OP_IDENTIFIER:
	  if (current_lang_name != lang_name_cplusplus)
	    error ("operator declared in \"%s\" language context",
		   IDENTIFIER_POINTER (current_lang_name));

	  /* C++ operators: if these are member functions, then
	     they overload the same way normal methods do.  However,
	     if they are declared outside of a classes scope, then
	     they are implicitly treated like `friends', i.e.,
	     they do not take any unseen arguments.  */
	  assert (flags == NO_SPECIAL);
	  flags = OP_FLAG;
	  name = "operator name";
	  decl = 0;
	  break;

	case TYPE_EXPR:
	  if (current_lang_name != lang_name_cplusplus)
	    error ("type conversion operator declared in \"%s\" language context",
		   IDENTIFIER_POINTER (current_lang_name));

	  ctype = NULL_TREE;
	  assert (flags == NO_SPECIAL);
	  flags = TYPENAME_FLAG;
	  name = "operator <typename>";
	  /* Go to the absdcl.  */
	  decl = TREE_OPERAND (decl, 0);
	  return_type = return_conversion;
	  break;

	  /* C++ extension */
	case SCOPE_REF:
	  if (current_lang_name != lang_name_cplusplus)
	    error ("member function declared in \"%s\" language context",
		   IDENTIFIER_POINTER (current_lang_name));
	  if (seen_scope_ref == 1)
	    error ("multiple `::' terms in declarator invalid");
	  seen_scope_ref += 1;
	  {
	    /* Perform error checking, and convert class names to types.
	       We may call grokdeclarator multiple times for the same
	       tree structure, so only do the conversion once.  In this
	       case, we have exactly what we want for `ctype'.  */
	    tree cname = TREE_OPERAND (decl, 0);
	    if (cname == NULL_TREE)
	      ctype = NULL_TREE;
	    else if (IS_AGGR_TYPE (cname))
	      ctype = cname;
	    else if (! is_aggr_typedef (cname, 1))
	      {
		TREE_OPERAND (decl, 0) = 0;
	      }
	    /* Must test TREE_OPERAND (decl, 1), in case user gives
	       us `typedef (class::memfunc)(int); memfunc *memfuncptr;'  */
	    else if (TREE_OPERAND (decl, 1)
		     && TREE_CODE (TREE_OPERAND (decl, 1)) == INDIRECT_REF)
	      {
		TREE_OPERAND (decl, 0) = TREE_TYPE (TREE_TYPE (cname));
	      }
	    else if (ctype == NULL_TREE)
	      {
		ctype = TREE_TYPE (TREE_TYPE (cname));
		TREE_OPERAND (decl, 0) = ctype;
	      }
	    else
	      {
		tree new_type = get_base_type (TREE_TYPE (TREE_TYPE (cname)), ctype, 0);
		if (new_type == NULL_TREE)
		  {
		    error ("type `%s' is not derived from type `%s'",
			   IDENTIFIER_POINTER (cname),
			   TYPE_NAME_STRING (ctype));
		    TREE_OPERAND (decl, 0) = 0;
		  }
		else
		  {
		    ctype = new_type;
		    TREE_OPERAND (decl, 0) = ctype;
		  }
	      }
	    decl = TREE_OPERAND (decl, 1);
	    if (ctype != NULL_TREE && DECL_NAME (TYPE_NAME (ctype)) == decl)
	      {
		return_type = return_ctor;
		ctor_return_type = ctype;
	      }
	  }
	  break;

	case ERROR_MARK:
	  decl = NULL_TREE;
	  break;

	default:
	  assert (0);
	}
    if (name == 0)
      name = "type name";
  }

  /* A function definition's declarator must have the form of
     a function declarator.  */

  if (funcdef_flag && innermost_code != CALL_EXPR)
    return 0;

  /* Anything declared one level down from the top level
     must be one of the parameters of a function
     (because the body is at least two levels down).  */

  /* This heuristic cannot be applied to C++ nodes! Fixed, however,
     by not allowing C++ class definitions to specify their parameters
     with xdecls (must be spec.d in the parmlist).

     Since we now wait to push a class scope until we are sure that
     we are in a legitimate method context, we must set oldcname
     explicitly (since current_class_name is not yet alive).  */

  if (decl_context == NORMAL
      && current_binding_level->level_chain == global_binding_level)
    decl_context = PARM;

  /* Look through the decl specs and record which ones appear.
     Some typespecs are defined as built-in typenames.
     Others, the ones that are modifiers of other types,
     are represented by bits in SPECBITS: set the bits for
     the modifiers that appear.  Storage class keywords are also in SPECBITS.

     If there is a typedef name or a type, store the type in TYPE.
     This includes builtin typedefs such as `int'.

     Set EXPLICIT_INT if the type is `int' or `char' and did not
     come from a user typedef.

     Set LONGLONG if `long' is mentioned twice.

     For C++, constructors and destructors have their own fast treatment.  */

  for (spec = declspecs; spec; spec = TREE_CHAIN (spec))
    {
      register int i;
      register tree id = TREE_VALUE (spec);

      /* Certain parse errors slip through.  For example,
	 `int class;' is not caught by the parser. Try
	 weakly to recover here.  */
      if (TREE_CODE (spec) != TREE_LIST)
	return 0;

      if (TREE_CODE (id) == IDENTIFIER_NODE)
	{
	  if (id == ridpointers[(int) RID_INT])
	    {
	      if (type)
		error ("extraneous `int' ignored");
	      else
		{
		  explicit_int = 1;
		  type = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (id));
		}
	      goto found;
	    }
	  if (id == ridpointers[(int) RID_CHAR])
	    {
	      if (type)
		error ("extraneous `char' ignored");
	      else
		{
		  explicit_char = 1;
		  type = TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (id));
		}
	      goto found;
	    }
	  /* C++ aggregate types.  */
	  if (TREE_TYPE (id))
	    {
	      if (type)
		error ("multiple declarations `%s' and `%s'",
		       IDENTIFIER_POINTER (type),
		       IDENTIFIER_POINTER (id));
	      else
		type = TREE_TYPE (TREE_TYPE (id));
	      goto found;
	    }

	  for (i = (int) RID_FIRST_MODIFIER; i < (int) RID_MAX; i++)
	    {
	      if (ridpointers[i] == id)
		{
		  if (i == (int) RID_LONG && specbits & (1<<i))
		    {
		      if (pedantic)
			warning ("duplicate `%s'", IDENTIFIER_POINTER (id));
		    else if (longlong)
		      warning ("`long long long' is too long for GCC");
		      else
			longlong = 1;
		    }
		  else if (specbits & (1 << i))
		    warning ("duplicate `%s'", IDENTIFIER_POINTER (id));
		  specbits |= 1 << i;
		  goto found;
		}
	    }
	}
      if (type)
	error ("two or more data types in declaration of `%s'", name);
      else if (TREE_CODE (id) == IDENTIFIER_NODE)
	{
	  register tree t = lookup_name (id);
	  if (!t || TREE_CODE (t) != TYPE_DECL)
	    error ("`%s' fails to be a typedef or built in type",
		   IDENTIFIER_POINTER (id));
	  else type = TREE_TYPE (t);
	}
      else if (TREE_CODE (id) != ERROR_MARK)
	/* Can't change CLASS nodes into RECORD nodes here!  */
	type = id;

    found: {}
    }

  typedef_type = type;

  /* No type at all: default to `int', and set EXPLICIT_INT
     because it was not a user-defined typedef.  */

  if (type == 0)
    {
      explicit_int = -1;
      if (return_type == return_dtor)
	type = void_type_node;
      else if (return_type == return_ctor)
	type = TYPE_POINTER_TO (ctor_return_type);
      else
	{
	  if (funcdef_flag && warn_return_type
	      && return_type == return_normal
	      && ! (specbits & ((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
				| (1 << (int) RID_SIGNED) | (1 << (int) RID_UNSIGNED))))
	    warn_about_return_type = 1;
	  /* Save warning until we know what is really going on.  */
	  type = integer_type_node;
	}
    }
  else if (return_type == return_dtor)
    {
      error ("return type specification for destructor invalid");
      type = void_type_node;
    }
  else if (return_type == return_ctor)
    {
      warning ("return type specification for constructor invalid");
      type = TYPE_POINTER_TO (ctor_return_type);
    }

  ctype = NULL_TREE;

  /* Now process the modifiers that were specified
     and check for invalid combinations.  */

  /* Long double is a special combination.  */

  if ((specbits & 1 << (int) RID_LONG) && type == double_type_node)
    {
      specbits &= ~ (1 << (int) RID_LONG);
      type = long_double_type_node;
    }

  /* Check all other uses of type modifiers.  */

  if (specbits & ((1 << (int) RID_LONG) | (1 << (int) RID_SHORT)
		  | (1 << (int) RID_UNSIGNED) | (1 << (int) RID_SIGNED)))
    {
      if (!explicit_int && !explicit_char && !pedantic)
	error ("long, short, signed or unsigned used invalidly for `%s'", name);
      else if ((specbits & 1 << (int) RID_LONG) && (specbits & 1 << (int) RID_SHORT))
	error ("long and short specified together for `%s'", name);
      else if (((specbits & 1 << (int) RID_LONG) || (specbits & 1 << (int) RID_SHORT))
	       && explicit_char)
	error ("long or short specified with char for `%s'", name);
      else if ((specbits & 1 << (int) RID_SIGNED) && (specbits & 1 << (int) RID_UNSIGNED))
	error ("signed and unsigned given together for `%s'", name);
      else
	{
	  if (specbits & 1 << (int) RID_UNSIGNED)
	    {
	      if (longlong)
		type = long_long_unsigned_type_node;
	      else if (specbits & 1 << (int) RID_LONG)
		type = long_unsigned_type_node;
	      else if (specbits & 1 << (int) RID_SHORT)
		type = short_unsigned_type_node;
	      else if (type == char_type_node)
		type = unsigned_char_type_node;
	      else
		type = unsigned_type_node;
	    }
	  else if ((specbits & 1 << (int) RID_SIGNED)
		   && type == char_type_node)
	    type = signed_char_type_node;
	  else if (longlong)
	    type = long_long_integer_type_node;
	  else if (specbits & 1 << (int) RID_LONG)
	    type = long_integer_type_node;
	  else if (specbits & 1 << (int) RID_SHORT)
	    type = short_integer_type_node;
	}
    }

  /* Set CONSTP if this declaration is `const', whether by
     explicit specification or via a typedef.
     Likewise for VOLATILEP.  */

  constp = !! (specbits & 1 << (int) RID_CONST) + TREE_READONLY (type);
  volatilep = !! (specbits & 1 << (int) RID_VOLATILE) + TREE_VOLATILE (type);
  staticp = 0;
  inlinep = !! (specbits & (1 << (int) RID_INLINE));
  if (constp > 1)
    warning ("duplicate `const'");
  if (volatilep > 1)
    warning ("duplicate `volatile'");
  virtualp = specbits & (1 << (int) RID_VIRTUAL);
  if (specbits & (1 << (int) RID_STATIC))
    staticp = 1 + (decl_context == FIELD);

  if (virtualp && staticp == 2)
    {
      error ("member `%s' cannot be declared both virtual and static", name);
      staticp = 0;
    }
  friendp = specbits & (1 << (int) RID_FRIEND);
  specbits &= ~ ((1 << (int) RID_VIRTUAL) | (1 << (int) RID_FRIEND));

  /* Warn if two storage classes are given. Default to `auto'.  */

  if (specbits)
    {
      if (specbits & 1 << (int) RID_STATIC) nclasses++;
      if (specbits & 1 << (int) RID_EXTERN) nclasses++;
      if (decl_context == PARM && nclasses > 0)
	error ("storage class specifiers invalid in parameter declarations");
      if (specbits & 1 << (int) RID_TYPEDEF)
	{
	  if (decl_context == PARM)
	    error ("typedef declaration invalid in parameter declaration");
	  nclasses++;
	}
      if (specbits & 1 << (int) RID_AUTO) nclasses++;
      if (specbits & 1 << (int) RID_REGISTER) nclasses++;
    }

  /* Give error if `virtual' is used outside of class declaration.  */
  if (virtualp && current_class_name == NULL_TREE)
    {
      error ("virtual outside class declaration");
      virtualp = 0;
    }

  /* Warn about storage classes that are invalid for certain
     kinds of declarations (parameters, typenames, etc.).  */

  if (nclasses > 1)
    error ("multiple storage classes in declaration of `%s'", name);
  else if (decl_context != NORMAL && nclasses > 0)
    {
      if (decl_context == PARM && (specbits & ((1 << (int) RID_REGISTER)|(1 << (int) RID_AUTO))))
	;
      else if (decl_context == FIELD
	       && (specbits
		   & (/* C++ allows static class elements  */
		      (1 << (int) RID_STATIC)
		      /* ...and inlines  */
		      | (1 << (int) RID_INLINE)
		      /* ...and signed and unsigned elements.  */
		      | (1 << (int) RID_SIGNED)
		      | (1 << (int) RID_UNSIGNED))))
	;
      else if (decl_context == FIELD && (specbits & (1 << (int) RID_TYPEDEF)))
	{
	  /* A typedef which was made in a class's scope.  */
	  tree loc_typedecl;
	  register int i = sizeof (struct lang_decl_flags) / sizeof (int);
	  register int *pi;

	  /* keep `grokdeclarator' from thinking we are in PARM context.  */
	  pushlevel (0);
	  loc_typedecl = start_decl (declarator, declspecs, initialized, NULL_TREE);

	  pi = (int *) permalloc (sizeof (struct lang_decl_flags));
	  while (i > 0)
	    pi[--i] = 0;
	  DECL_LANG_SPECIFIC (loc_typedecl) = (struct lang_decl *) pi;
	  poplevel (0, 0, 0);

	  if (TREE_CODE (TREE_TYPE (loc_typedecl)) == ENUMERAL_TYPE)
	    {
	      tree ref = lookup_tag (ENUMERAL_TYPE, DECL_NAME (loc_typedecl), current_binding_level, 0);
	      if (! ref)
		pushtag (DECL_NAME (loc_typedecl), TREE_TYPE (loc_typedecl));
	    }
	  if (IDENTIFIER_CLASS_VALUE (DECL_NAME (loc_typedecl)))
	    error_with_decl (loc_typedecl,
			     "typedef of `%s' in class scope hides previous declaration");
#if 0
	  /* Must push this into scope via `pushdecl_class_level'.  */
	  IDENTIFIER_CLASS_VALUE (DECL_NAME (loc_typedecl)) = loc_typedecl;
#endif
	  return loc_typedecl;
	}
      else
	{
	  error ((decl_context == FIELD
		  ? "storage class specified for structure field `%s'"
		  : (decl_context == PARM
		     ? "storage class specified for parameter `%s'"
		     : "storage class specified for typename")),
		 name);
	  specbits &= ~ ((1 << (int) RID_REGISTER) | (1 << (int) RID_AUTO)
			 | (1 << (int) RID_EXTERN));
	}
    }
  else if (current_binding_level == global_binding_level)
    {
      if (specbits & (1 << (int) RID_AUTO))
	error ("top-level declaration of `%s' specifies `auto'", name);
#if 0
      if (specbits & (1 << (int) RID_REGISTER))
	error ("top-level declaration of `%s' specifies `register'", name);
#endif
#if 0
      /* I'm not sure under what circumstances we should turn
	 on the extern bit, and under what circumstances we should
	 warn if other bits are turned on.  */
      if (decl_context == NORMAL
	  && (specbits & (1 << (int) RID_EXTERN)) == 0
	  && ! root_lang_context_p ())
	{
	  specbits |= (1 << (int) RID_EXTERN);
	}
#endif
    }

  /* Now figure out the structure of the declarator proper.
     Descend through it, creating more complex types, until we reach
     the declared identifier (or NULL_TREE, in an absolute declarator).  */

  while (declarator && TREE_CODE (declarator) != IDENTIFIER_NODE)
    {
      /* Each level of DECLARATOR is either an ARRAY_REF (for ...[..]),
	 an INDIRECT_REF (for *...),
	 a CALL_EXPR (for ...(...)),
	 an identifier (for the name being declared)
	 or a null pointer (for the place in an absolute declarator
	 where the name was omitted).
	 For the last two cases, we have just exited the loop.

	 For C++ it could also be
	 a SCOPE_REF (for class :: ...).  In this case, we have converted
	 sensible names to types, and those are the values we use to
	 qualify the member name.
	 an ADDR_EXPR (for &...),
	 a BIT_NOT_EXPR (for destructors)
	 a TYPE_EXPR (for operator typenames)
	 a WRAPPER_EXPR (for wrappers)
	 an ANTI_WRAPPER_EXPR (for averting wrappers)

	 At this point, TYPE is the type of elements of an array,
	 or for a function to return, or for a pointer to point to.
	 After this sequence of ifs, TYPE is the type of the
	 array or function or pointer, and DECLARATOR has had its
	 outermost layer removed.  */

      if (TREE_CODE (type) == ERROR_MARK
	  && TREE_CODE (declarator) != OP_IDENTIFIER)
	{
	  if (TREE_CODE (declarator) == SCOPE_REF)
	    declarator = TREE_OPERAND (declarator, 1);
	  else
	    declarator = TREE_OPERAND (declarator, 0);
	  continue;
	}
      if (quals != NULL_TREE
	  && (declarator == NULL_TREE
	      || TREE_CODE (declarator) != SCOPE_REF))
	{
	  if (ctype == NULL_TREE && TREE_CODE (type) == METHOD_TYPE)
	    ctype = TYPE_METHOD_BASETYPE (type);
	  if (ctype != NULL_TREE)
	    {
	      tree dummy = build_decl (TYPE_DECL, NULL_TREE, type);
	      ctype = grok_method_quals (ctype, dummy, quals);
	      type = TREE_TYPE (dummy);
	      quals = NULL_TREE;
	    }
	}
      switch (TREE_CODE (declarator))
	{
	case ARRAY_REF:
	  {
	    register tree itype = NULL_TREE;
	    register tree size = TREE_OPERAND (declarator, 1);

	    declarator = TREE_OPERAND (declarator, 0);

	    /* Check for some types that there cannot be arrays of.  */

	    if (type == void_type_node)
	      {
		error ("declaration of `%s' as array of voids", name);
		type = error_mark_node;
	      }

	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		error ("declaration of `%s' as array of functions", name);
		type = error_mark_node;
	      }

	    if (size == error_mark_node)
	      type = error_mark_node;

	    if (type == error_mark_node)
	      continue;

	    if (size)
	      {
		/* Must suspend_momentary here because the index
		   type may need to live until the end of the function.
		   For example, it is used in the declaration of a
		   variable which requires destructing at the end of
		   the function; then build_vec_delete will need this
		   value.  */
		int yes = suspend_momentary ();
		/* might be a cast */
		if (TREE_CODE (size) == NOP_EXPR
		    && TREE_TYPE (size) == TREE_TYPE (TREE_OPERAND (size, 0)))
		  size = TREE_OPERAND (size, 0);

		if (TREE_CODE (TREE_TYPE (size)) != INTEGER_TYPE
		    && TREE_CODE (TREE_TYPE (size)) != ENUMERAL_TYPE)
		  {
		    error ("size of array `%s' has non-integer type", name);
		    size = integer_one_node;
		  }
		if (TREE_READONLY_DECL_P (size))
		  size = decl_constant_value (size);
		if (pedantic && integer_zerop (size))
		  warning ("ANSI C forbids zero-size array `%s'", name);
		if (TREE_LITERAL (size))
		  {
		    if (INT_CST_LT (size, integer_zero_node))
		      {
			error ("size of array `%s' is negative", name);
			size = integer_one_node;
		      }
		    itype = build_index_type (build_int_2 (TREE_INT_CST_LOW (size) - 1, 0));
		  }
		else
		  {
		    if (pedantic)
		      warning ("ANSI C forbids variable-size array `%s'", name);
		    itype = build_binary_op (MINUS_EXPR, size, integer_one_node);
		    itype = build_index_type (itype);
		  }
		resume_momentary (yes);
	      }

	    /* Build the array type itself.
	       Merge any constancy or volatility into the target type.  */

	    if (constp || volatilep)
	      type = build_type_variant (type, constp, volatilep);

#if 0   /* don't clear these; leave them set so that the array type
	   or the variable is itself const or volatile.  */
	    constp = 0;
	    volatilep = 0;
#endif

	    type = build_cplus_array_type (type, itype);
	    ctype = NULL_TREE;
	  }
	  break;

	case CALL_EXPR:
	  {
	    tree arg_types;

	    /* Declaring a function type.
	       Make sure we have a valid type for the function to return.  */
	    /* Is this an error?  Should they be merged into TYPE here?  */
	    if (pedantic && (constp || volatilep))
	      warning ("function declared to return const or volatile result");

	    /* Warn about some types functions can't return.  */

	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		error ("`%s' declared as function returning a function", name);
		type = integer_type_node;
	      }
	    if (TREE_CODE (type) == ARRAY_TYPE)
	      {
		error ("`%s' declared as function returning an array", name);
		type = integer_type_node;
	      }

	    if (ctype == NULL_TREE
		&& decl_context == FIELD
		&& (friendp == 0 || dname == current_class_name))
	      ctype = current_class_type;

	    if (ctype && flags == TYPENAME_FLAG)
	      TYPE_HAS_CONVERSION (ctype) = 1;
	    if (ctype && DECL_NAME (TYPE_NAME (ctype)) == dname)
	      {
		/* We are within a class's scope. If our declarator name
		   is the same as the class name, and we are defining
		   a function, then it is a constructor/destructor, and
		   therefore returns a void type.  */

		if (flags == DTOR_FLAG)
		  {
		    if (staticp == 2)
		      error ("destructor cannot be static member function");
		    if (decl_context == FIELD)
		      {
			if (! member_function_or_else (ctype, current_class_type,
						       "destructor for alien class `%s' cannot be a member"))
			  return NULL_TREE;
			if (TYPE_HAS_DESTRUCTOR (ctype))
			  error_with_aggr_type (ctype, "class `%s' already has destructor defined");
		      }
		  }
		else if (flags == WRAPPER_FLAG || flags == ANTI_WRAPPER_FLAG)
		  {
		    if (staticp == 2)
		      error ("wrapper cannot be static member function");
		    if (decl_context == FIELD)
		      {
			if (! member_function_or_else (ctype, current_class_type,
						       "wrapper for alien class `%s' cannot be member"))
			  return NULL_TREE;
			TYPE_WRAP_TYPE (ctype) = TYPE_MAIN_VARIANT (ctype);
		      }
		  }
		else if (flags == WRAPPER_PRED_FLAG)
		  {
		    if (staticp == 2)
		      error ("wrapper predicate cannot be static member function");
		    if (TREE_CODE (type) != INTEGER_TYPE)
		      {
			error ("wrapper predicated must return an integer type");
			type = integer_type_node;
		      }
		    if (decl_context == FIELD)
		      {
			if (! member_function_or_else (ctype, current_class_type,
						       "wrapper predicate for alien class `%s' cannot be member"))
			  return NULL_TREE;
			TYPE_HAS_WRAPPER_PRED (ctype) = 1;
		      }
		  }
		else            /* its a constructor. */
		  {
		    if (staticp == 2)
		      error ("constructor cannot be static member function");
		    if (virtualp || friendp)
		      {
			error ("constructors cannot be declared virtual or friend");
			virtualp = 0;
			friendp = 0;
		      }
		    if (specbits & ~((1 << (int) RID_INLINE)|(1 << (int) RID_STATIC)))
		      error ("return value type specifier for `%s' ignored",
			     flags == DTOR_FLAG ? "destructor" : "constructor");
		    type = TYPE_POINTER_TO (ctype);
		    if (decl_context == FIELD)
		      {
			if (! member_function_or_else (ctype, current_class_type,
						       "constructor for alien class `%s' cannot be member"))
			  return NULL_TREE;
			TYPE_HAS_CONSTRUCTOR (ctype) = 1;
			assert (return_type == return_ctor);
		      }
		  }
		if (decl_context == FIELD)
		  staticp = 0;
	      }
	    else if (friendp && virtualp)
	      {
		/* Cannot be both friend and virtual.  */
		error ("virtual functions cannot be friends");
		specbits ^= (1 << (int) RID_FRIEND);
	      }

	    if (decl_context == NORMAL && friendp)
	      error ("friend declaration not in class definition");

	    /* Picky up type qualifiers which should be applied to `this'.  */
	    quals = TREE_OPERAND (declarator, 2);

	    /* Traditionally, declaring return type float means double.  */

	    if (flag_traditional && type == float_type_node)
	      type = double_type_node;

	    /* Construct the function type and go to the next
	       inner layer of declarator.  */

	    {
	      int funcdef_p;
	      tree inner_parms = TREE_OPERAND (declarator, 1);
	      tree inner_decl = TREE_OPERAND (declarator, 0);

	      declarator = TREE_OPERAND (declarator, 0);

	      if (inner_decl && TREE_CODE (inner_decl) == SCOPE_REF)
		inner_decl = TREE_OPERAND (inner_decl, 1);

	      /* Say it's a definition only for the CALL_EXPR
		 closest to the identifier.  */
	      funcdef_p =
		(inner_decl &&
		 (TREE_CODE (inner_decl) == IDENTIFIER_NODE
		  || TREE_CODE (inner_decl) == OP_IDENTIFIER
		  || TREE_CODE (inner_decl) == TYPE_EXPR)) ? funcdef_flag : 0;

	      arg_types = grokparms (inner_parms, funcdef_p);
	    }

	    if (declarator)
	      {
		/* Get past destructors, wrappers, etc.
		   We know we have one because FLAGS will be non-zero.

		   Complain about improper parameter lists here.  */
		if (TREE_CODE (declarator) == BIT_NOT_EXPR)
		  {
		    declarator = TREE_OPERAND (declarator, 0);

		    if (strict_prototype == 0 && arg_types == NULL_TREE)
		      arg_types = void_list_node;
		    else if (arg_types == NULL_TREE
			     || arg_types != void_list_node)
		      {
			error ("destructors cannot be specified with parameters");
			arg_types = void_list_node;
		      }
		  }
		else if (TREE_CODE (declarator) == WRAPPER_EXPR)
		  {
		    /* Report misuse of wrappers and their associates.
		       Note that because wrappers may be invoked
		       quite a bit implicitly, if we give an error
		       message, we make an effort to fix that error
		       so that spurious errors do not show up.  */
		    if (TREE_CODE (TREE_OPERAND (declarator, 0)) == COND_EXPR)
		      {
			/* First parameter must be a pointer to a member function.
			   Rest of parameters must all be default parameters.  */
			if (arg_types == NULL_TREE
			    || arg_types == void_list_node
			    || TREE_CODE (TREE_VALUE (arg_types)) != POINTER_TYPE
			    || TREE_CODE (TREE_TYPE (TREE_VALUE (arg_types))) != METHOD_TYPE)
			  {
			    error ("wrapper predicate takes a pointer-to-member-function as first argument");
			    arg_types = NULL_TREE;
			  }
			else if (TREE_CHAIN (arg_types)
				 && TREE_CHAIN (arg_types) != void_list_node
				 && TREE_PURPOSE (TREE_CHAIN (arg_types)) == NULL_TREE)
			  {
			    error ("all arguments past first must be default for wrapper predicate");
			    TREE_CHAIN (arg_types) = NULL_TREE;
			  }
			declarator = wrapper_pred_name;
		      }
		    else
		      {
			/* First parameter must be an int.
			   Second parameter must be a pointer to a member function.  */
			if (arg_types == NULL_TREE || TREE_CHAIN (arg_types) == NULL_TREE)
			  {
			    error ("wrappers must have at least two arguments (int, pointer-to-member-function)");
			    arg_types = NULL_TREE;
			  }
			else
			  {
			    if (TREE_CODE (TREE_VALUE (arg_types)) != INTEGER_TYPE)
			      {
				error ("first argument to wrapper must be an integer");
				TREE_VALUE (arg_types) = integer_type_node;
			      }
			    if (TREE_CODE (TREE_VALUE (TREE_CHAIN (arg_types))) != POINTER_TYPE
				|| TREE_CODE (TREE_TYPE (TREE_VALUE (TREE_CHAIN (arg_types)))) != METHOD_TYPE)
			      {
				error ("second argument to wrapper must be a pointer-to-member-function type");
				TREE_CHAIN (arg_types) = NULL_TREE;
			      }
			  }
			declarator = wrapper_name;
		      }
		  }
		else if (TREE_CODE (declarator) == ANTI_WRAPPER_EXPR)
		  declarator = anti_wrapper_name;
	      }

	    type = build_function_type (type, flag_traditional ? 0 : arg_types);
	  }
	  break;

	case ADDR_EXPR:
	case INDIRECT_REF:
	  /* Filter out pointers-to-references and references-to-references.
	     We can get these if a TYPE_DECL is used.  */

	  if (TREE_CODE (type) == REFERENCE_TYPE)
	    {
	      error ("cannot declare %s to references",
		     TREE_CODE (declarator) == ADDR_EXPR
		     ? "references" : "pointers");
	      declarator = TREE_OPERAND (declarator, 0);
	      continue;
	    }

	  /* Merge any constancy or volatility into the target type
	     for the pointer.  */

	  if (constp || volatilep)
	    {
	      type = build_type_variant (type, constp, volatilep);
	      if (IS_AGGR_TYPE (type))
		build_pointer_type (type);
	    }
	  constp = 0;
	  volatilep = 0;

	  if (TREE_CODE (declarator) == ADDR_EXPR)
	    if (TREE_CODE (type) == FUNCTION_TYPE)
	      {
		error ("cannot declare references to functions; use pointer to function instead");
		type = build_pointer_type (type);
	      }
	    else
	      {
		if (TYPE_MAIN_VARIANT (type) == void_type_node)
		  error ("invalid type: `void &'");
		else
		  type = build_reference_type (type);
	      }
	  else
	    type = build_pointer_type (type);

	  /* Process a list of type modifier keywords (such as
	     const or volatile) that were given inside the `*' or `&'.  */

	  if (TREE_TYPE (declarator))
	    {
	      register tree typemodlist;
	      int erred = 0;
	      for (typemodlist = TREE_TYPE (declarator); typemodlist;
		   typemodlist = TREE_CHAIN (typemodlist))
		{
		  if (TREE_VALUE (typemodlist) == ridpointers[(int) RID_CONST])
		    constp++;
		  else if (TREE_VALUE (typemodlist) == ridpointers[(int) RID_VOLATILE])
		    volatilep++;
		  else if (!erred)
		    {
		      erred = 1;
		      error ("invalid type modifier within %s declarator",
			     TREE_CODE (declarator) == ADDR_EXPR
			     ? "reference" : "pointer");
		    }
		}
	      if (constp > 1)
		warning ("duplicate `const'");
	      if (volatilep > 1)
		warning ("duplicate `volatile'");
	    }
	  declarator = TREE_OPERAND (declarator, 0);
	  ctype = NULL_TREE;
	  break;

	case SCOPE_REF:
	  {
	    /* We have converted type names to NULL_TREE if the
	       name was bogus, or to a _TYPE node, if not.

	       The variable CTYPE holds the type we will ultimately
	       resolve to.  The code here just needs to build
	       up appropriate member types.  */
	    tree sname = TREE_OPERAND (declarator, 1);

	    if (TREE_OPERAND (declarator, 0) == NULL_TREE)
	      {
		/* We had a reference to a global decl, or
		   perhaps we were given a non-aggregate typedef,
		   in which case we cleared this out, and should just
		   keep going as though it wasn't there.  */
		declarator = sname;
		continue;
	      }
	    ctype = TREE_OPERAND (declarator, 0);

	    if (sname == NULL_TREE)
	      goto done_scoping;

	    /* Destructors can have their visibilities changed as well.  */
	    if (TREE_CODE (sname) == BIT_NOT_EXPR)
	      sname = TREE_OPERAND (sname, 0);

	    if (TREE_CODE (sname) == IDENTIFIER_NODE)
	      {
		/* This is the `standard' use of the scoping operator:
		   basetype :: member .  */

		if (ctype == current_class_type || friendp)
		  if (TREE_CODE (type) == FUNCTION_TYPE)
		    type = build_cplus_method_type (ctype, TREE_TYPE (type), TYPE_ARG_TYPES (type));
		  else
		    type = build_member_type (ctype, type);
		else if (TYPE_SIZE (ctype) != 0
		    || (specbits & (1<<(int)RID_TYPEDEF)))
		  {
		    tree t;
		    /* have to move this code elsewhere in this function.
		       this code is used for i.e., typedef int A::M; M *pm; */

		    if (decl_context == FIELD)
		      {
			t = lookup_field (ctype, sname, 0);
			if (t)
			  {
			    t = build_lang_field_decl (FIELD_DECL, build_nt (SCOPE_REF, ctype, t), type);
			    DECL_INITIAL (t) = init;
			    return t;
			  }
			/* No such field, try member functions.  */
			t = lookup_fnfields (CLASSTYPE_AS_LIST (ctype), sname, 0);
			if (t)
			  {
			    if (flags == DTOR_FLAG)
			      t = TREE_VALUE (t);
			    else if (CLASSTYPE_METHOD_VEC (ctype)
				     && TREE_VALUE (t) == TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (ctype), 0))
			      {
				/* Don't include destructor with constructors.  */
				t = TREE_CHAIN (TREE_VALUE (t));
				if (t == NULL_TREE)
				  error ("class `%s' does not have any constructors", IDENTIFIER_POINTER (sname));
				t = build_tree_list (NULL_TREE, t);
			      }
			    t = build_lang_field_decl (FIELD_DECL, build_nt (SCOPE_REF, ctype, t), type);
			    DECL_INITIAL (t) = init;
			    return t;
			  }

			if (flags == TYPENAME_FLAG)
			  error_with_aggr_type (ctype, "type conversion is not a member of structure `%s'");
			else
			  error ("field `%s' is not a member of structure `%s'",
				 IDENTIFIER_POINTER (sname),
				 TYPE_NAME_STRING (ctype));
		      }
		    if (TREE_CODE (type) == FUNCTION_TYPE)
		      type = build_cplus_method_type (ctype, TREE_TYPE (type), TYPE_ARG_TYPES (type));
		    else
		      type = build_member_type (ctype, type);
		  }
		else
		  sorry ("structure `%s' not yet defined",
			 TYPE_NAME_STRING (ctype));
		declarator = sname;
	      }
	    else if (TREE_CODE (sname) == TYPE_EXPR)
	      {
		/* A TYPE_EXPR will change types out from under us.
		   So do the TYPE_EXPR now, and make this SCOPE_REF
		   inner to the TYPE_EXPR's CALL_EXPR.

		   This does not work if we don't get a CALL_EXPR back.
		   I did not think about error recovery, hence the
		   assert (0).  */

		/* Get the CALL_EXPR.  */
		sname = grokoptypename (sname, 0);
		assert (TREE_CODE (sname) == CALL_EXPR);
		type = TREE_TYPE (TREE_OPERAND (sname, 0));
		/* Scope the CALL_EXPR's name.  */
		TREE_OPERAND (declarator, 1) = TREE_OPERAND (sname, 0);
		/* Put the SCOPE_EXPR in the CALL_EXPR's innermost position.  */
		TREE_OPERAND (sname, 0) = declarator;
		/* Now work from the CALL_EXPR.  */
		declarator = sname;
		continue;
	      }
	    else if (TREE_CODE (sname) == SCOPE_REF)
	      abort ();
	    else
	      {
	      done_scoping:
		declarator = TREE_OPERAND (declarator, 1);
		if (declarator && TREE_CODE (declarator) == CALL_EXPR)
		  /* In this case, we will deal with it later.  */
		  ;
		else
		  {
		    if (TREE_CODE (type) == FUNCTION_TYPE)
		      type = build_cplus_method_type (ctype, TREE_TYPE (type), TYPE_ARG_TYPES (type));
		    else
		      type = build_member_type (ctype, type);
		  }
	      }
	  }
	  break;

	case OP_IDENTIFIER:
	  /* This is exceptional, in that we must finalize a 
	     member type before calling grokopexpr, if we want
	     to use the declared type information to resolve
	     ambiguities.  Do not get fooled by friends,
	     which do not have a member type built for them
	     unless they were explicitly scoped (in which case that
	     will have been taken care of in the SCOPE_REF case.  */
	  if (TREE_CODE (type) != FUNCTION_TYPE
	      && TREE_CODE (type) != METHOD_TYPE)
	    {
	      error ("operator name missing at this point in file");
	      if (ctype)
		type = build_cplus_method_type (ctype, type, NULL_TREE);
	      else
		type = build_function_type (type, NULL_TREE);
	    }
	  if (TREE_CODE (TREE_OPERAND (declarator, 0)) == NEW_EXPR)
	    {
	      int was_method = ctype || TREE_CODE (type) == METHOD_TYPE;
	      type = coerce_new_type (ctype, type);
	      if (was_method)
		staticp = 2;
	    }
	  else if (TREE_CODE (TREE_OPERAND (declarator, 0)) == DELETE_EXPR)
	    {
	      int was_method = ctype || TREE_CODE (type) == METHOD_TYPE;
	      type = coerce_delete_type (ctype, type);
	      if (was_method)
		staticp = 2;
	    }
	  else if (TREE_CODE (type) == FUNCTION_TYPE
		   && ctype != 0
		   && (friendp == 0 || staticp < 2))
	    type = build_cplus_method_type (ctype, TREE_TYPE (type), TYPE_ARG_TYPES (type));
	  {
	    tree tmp = declarator;
	    declarator = grokopexpr (&tmp, ctype, type, 0,
				     staticp == 2 ? 2 : 1);
	  }
	  if (declarator == NULL_TREE)
	    return NULL_TREE;
	  name = IDENTIFIER_POINTER (declarator);
	  break;

	case BIT_NOT_EXPR:
	  declarator = TREE_OPERAND (declarator, 0);
	  break;

	case TYPE_EXPR:
	  declarator = grokoptypename (declarator, 0);
	  if (explicit_int != -1)
	    if (comp_target_types (type, TREE_TYPE (TREE_OPERAND (declarator, 0)), 1) == 0)
	      error ("type conversion function declared to return incongruent type");
	    else
	      warning ("return type specified for type conversion function");
	  type = TREE_TYPE (TREE_OPERAND (declarator, 0));
	  break;

	case WRAPPER_EXPR:
	  if (TREE_CODE (TREE_OPERAND (declarator, 0)) == COND_EXPR)
	    declarator = wrapper_pred_name;
	  else
	    declarator = wrapper_name;
	  break;

	case ANTI_WRAPPER_EXPR:
	  declarator = anti_wrapper_name;
	  break;

	case RECORD_TYPE:
	case UNION_TYPE:
	case ENUMERAL_TYPE:
	  declarator = 0;
	  break;

	case ERROR_MARK:
	  declarator = 0;
	  break;

	default:
	  assert (0);
	}

      /* layout_type (type); */
#if 0
      /* @@ Should perhaps replace the following code by changes in
       * @@ stor_layout.c. */
      if (TREE_CODE (type) == FUNCTION_DECL)
	{
	  /* A function variable in C should be Pmode rather than EPmode
	     because it has just the address of a function, no static chain.*/
	  TYPE_MODE (type) = Pmode;
	}
#endif
    }

  /* Now TYPE has the actual type.  */

  /* If this is declaring a typedef name, return a TYPE_DECL.  */

  if (specbits & (1 << (int) RID_TYPEDEF))
    {
      tree decl;

      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters.  */
      if (constp || volatilep)
	type = build_type_variant (type, constp, volatilep);

      /* If the user declares "struct {...} foo" then `foo' will have
	 an anonymous name.  Fill that name in now.  Nothing can
	 refer to it, so nothing needs know about the name change.
	 The TYPE_NAME field was filled in by build_struct_xref.  */
      if (TYPE_NAME (type)
#ifndef BREAK_C_TAGS
	  && current_lang_name == lang_name_cplusplus
#endif
	  && TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
	  && ANON_AGGRNAME_P (DECL_NAME (TYPE_NAME (type))))
	{
	  /* replace the anonymous name with the real name everywhere.  */
	  lookup_tag_reverse (type, declarator);
	  DECL_NAME (TYPE_NAME (type)) = declarator;

	  /* Replace names of default constructors and/or destructors.  */
	  if (TYPE_LANG_SPECIFIC (type) && CLASSTYPE_METHOD_VEC (type))
	    {
	      tree method_vec = CLASSTYPE_METHOD_VEC (type);
	      tree fnptr = TREE_VEC_ELT (method_vec, 0);
	      while (fnptr)
		{
		  DECL_ORIGINAL_NAME (fnptr) = declarator;
		  fnptr = TREE_CHAIN  (fnptr);
		}
	    }
	}

      decl = build_decl (TYPE_DECL, declarator, type);
      if (quals)
	{
	  if (ctype == NULL_TREE)
	    {
	      assert (TREE_CODE (type) == METHOD_TYPE);
	      ctype = TYPE_METHOD_BASETYPE (type);
	    }
	  grok_method_quals (ctype, decl, quals);
	}

      if (resume_temporary)
	resume_temporary_allocation ();
      return decl;
    }

  /* Detect the case of an array type of unspecified size
     which came, as such, direct from a typedef name.
     We must copy the type, so that each identifier gets
     a distinct type, so that each identifier's size can be
     controlled separately by its own initializer.  */

  if (type == typedef_type && TREE_CODE (type) == ARRAY_TYPE
      && TYPE_DOMAIN (type) == 0)
    {
      type = build_cplus_array_type (TREE_TYPE (type), TYPE_DOMAIN (type));
    }

  /* If this is a type name (such as, in a cast or sizeof),
     compute the type and return it now.  */

  if (decl_context == TYPENAME)
    {
      /* Note that the grammar rejects storage classes
	 in typenames, fields or parameters.  */
      if (constp || volatilep)
	type = build_type_variant (type, constp, volatilep);

      /* Special case: "friend class foo" looks like a TYPENAME context.  */
      if (friendp)
	{
	  /* A friendly class?  */
	  make_friend_class (current_class_type, TYPE_MAIN_VARIANT (type));
	  type = void_type_node;
	}
      else if (quals)
	{
	  tree dummy = build_decl (TYPE_DECL, declarator, type);
	  if (ctype == NULL_TREE)
	    {
	      assert (TREE_CODE (type) == METHOD_TYPE);
	      ctype = TYPE_METHOD_BASETYPE (type);
	    }
	  grok_method_quals (ctype, dummy, quals);
	  type = TREE_TYPE (dummy);
	}

      if (resume_temporary)
	resume_temporary_allocation ();
      return type;
    }

  /* `void' at top level (not within pointer)
     is allowed only in typedefs or type names.
     We don't complain about parms either, but that is because
     a better error message can be made later.  */

  if (type == void_type_node && decl_context != PARM)
    {
      if (declarator != NULL_TREE
	  && TREE_CODE (declarator) == IDENTIFIER_NODE)
	error ("variable or field `%s' declared void", name);
      else
	error ("variable or field declared void");
      type = integer_type_node;
    }

  /* Now create the decl, which may be a VAR_DECL, a PARM_DECL
     or a FUNCTION_DECL, depending on DECL_CONTEXT and TYPE.  */

  {
    register tree decl;

    if (decl_context == PARM)
      {
	if (ctype)
	  error ("cannot use `::' in parameter declaration");
	if (virtualp)
	  error ("parameter declared `virtual'");
	if (quals)
	  error ("`const' and `volatile' function specifiers invalid in parameter declaration");
	if (friendp)
	  error ("invalid friend declaration");
	if (raises)
	  error ("invalid raises declaration");

	/* A parameter declared as an array of T is really a pointer to T.
	   One declared as a function is really a pointer to a function.
	   One declared as a member is really a pointer to member.  */

	if (TREE_CODE (type) == ARRAY_TYPE)
	  {
	    /* Transfer const-ness of array into that of type pointed to.  */
	    type = build_pointer_type
		    (build_type_variant (TREE_TYPE (type), constp, volatilep));
	    volatilep = constp = 0;
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE)
	  type = build_pointer_type (type);
	else if (TREE_CODE (type) == OFFSET_TYPE)
	  type = build_pointer_type (type);

	decl = build_decl (PARM_DECL, declarator, type);

	/* Compute the type actually passed in the parmlist,
	   for the case where there is no prototype.
	   (For example, shorts and chars are passed as ints.)
	   When there is a prototype, this is overridden later.  */

	DECL_ARG_TYPE (decl) = type;
	if (type == float_type_node)
	  DECL_ARG_TYPE (decl) = double_type_node;
	else if (TREE_CODE (type) == INTEGER_TYPE
		 && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	  DECL_ARG_TYPE (decl) = integer_type_node;
      }
    else if (decl_context == FIELD)
      {
	if (type == error_mark_node)
	  {
	    /* Happens when declaring arrays of sizes which
	       are error_mark_node, for example.  */
	    decl = NULL_TREE;
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    if (current_lang_name == lang_name_c)
	      {
		error ("field `%s' declared as a function in %s language context",
		       name, IDENTIFIER_POINTER (current_lang_name));
		type = build_pointer_type (type);
	      }
	    else
	      {
		if (friendp == 0)
		  {
		    if (ctype == NULL_TREE)
		      ctype = current_class_type;
		    if (staticp < 2)
		      type = build_cplus_method_type (ctype, TREE_TYPE (type), TYPE_ARG_TYPES (type));
		  }
		decl = grokfndecl (ctype, type, declarator, virtualp, flags, quals, raises, friendp ? -1 : 0);
		TREE_INLINE (decl) = inlinep;
		if (specbits & (1 << (int) RID_EXTERN))
		  TREE_PUBLIC (decl) = 1;
	      }
	  }
	else if (TREE_CODE (type) == METHOD_TYPE)
	  {
	    decl = grokfndecl (ctype, type, declarator, virtualp, flags, quals, raises, friendp ? -1 : 0);
	    TREE_INLINE (decl) = inlinep;
	    if (specbits & (1 << (int) RID_EXTERN))
	      TREE_PUBLIC (decl) = 1;
	  }
	else if (TREE_CODE (type) == RECORD_TYPE
		 && CLASSTYPE_DECLARED_EXCEPTION (type))
	  {
	    /* Handle a class-local exception declaration.  */
	    decl = build_lang_field_decl (VAR_DECL, declarator, type);
	    if (ctype == NULL_TREE)
	      ctype = current_class_type;
	    finish_exception_decl (TREE_CODE (TYPE_NAME (ctype)) == TYPE_DECL
				   ? DECL_NAME (TYPE_NAME (ctype)) : TYPE_NAME (ctype), decl);
	    return void_type_node;
	  }
	else if (TYPE_SIZE (type) == 0 && !staticp
		 && (TREE_CODE (type) != ARRAY_TYPE || initialized == 0))
	  {
	    if (declarator)
	      error ("field `%s' has incomplete type",
		     IDENTIFIER_POINTER (declarator));
	    else
	      error ("field has incomplete type");
	    type = error_mark_node;
	    decl = NULL_TREE;
	  }
	else
	  {
	    if (friendp)
	      {
		if (declarator)
		  error ("`%s' is neither function nor method; cannot be declared friend",
			 IDENTIFIER_POINTER (declarator));
		else
		  {
		    error ("invalid friend declaration");
		    return void_type_node;
		  }
		friendp = 0;
	      }
	    decl = NULL_TREE;
	  }

	if (friendp)
	  {
	    /* Friends are treated specially.  */
	    if (ctype == current_class_type)
	      warning ("member functions are implicitly friends of their class");
	    else if (DECL_NAME (decl))
	      return do_friend (ctype, declarator, decl, last_function_parms, flags, quals);
	    else return void_type_node;
	  }

	/* Structure field.  It may not be a function, except for C++ */

	if (decl == 0)
	  {
	    if (virtualp)
	      error ("field declared `virtual'");
	    if (quals)
	      error ("`const' and `volatile' function specifiers invalid in field declaration");
	    if (friendp)
	      error ("invalid friend declaration");
	    if (raises)
	      error ("invalid raises declaration");

	    if (staticp || (constp && initialized))
	      {
		/* C++ allows static class members.
		   All other work for this is done by grokfield.
		   This VAR_DECL is built by build_lang_field_decl.
		   All other VAR_DECLs are built by build_decl.  */
		if (current_lang_name == lang_name_c)
		  {
		    if (staticp)
		      error ("field `%s' declared static in %s language context",
			     name, IDENTIFIER_POINTER (current_lang_name));
		    else
		      error ("field `%s' declared with initializer in %s language context",
			     name, IDENTIFIER_POINTER (current_lang_name));
		  }
		decl = build_lang_field_decl (VAR_DECL, declarator, type);
		if (staticp)
		  TREE_STATIC (decl) = 1;
		/* In class context, static means public visibility.  */
		TREE_PUBLIC (decl) = 1;
	      }
	    else
	      decl = build_lang_field_decl (FIELD_DECL, declarator, type);
	  }
      }
    else if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
      {
	int was_overloaded = 0;
	tree original_name = declarator;

	if (! declarator) return NULL_TREE;

	if (specbits & ((1 << (int) RID_AUTO) | (1 << (int) RID_REGISTER)))
	  error ("invalid storage class for function `%s'", name);
	/* Function declaration not at top level.
	   Storage classes other than `extern' are not allowed
	   and `extern' makes no difference.  */
	if (current_binding_level != global_binding_level
	    && (specbits & ((1 << (int) RID_STATIC) | (1 << (int) RID_INLINE)))
	    && pedantic)
	  warning ("invalid storage class for function `%s'", name);

	if (ctype == NULL_TREE)
	  {
	    if (virtualp)
	      {
		error ("virtual non-class function `%s'", name);
		virtualp = 0;
	      }
#ifdef NO_AUTO_OVERLOAD
	    if (is_overloaded (declarator))
	      {
		/* Plain overloading: will not be grok'd by grokclassfn.  */
		if (current_lang_name == lang_name_cplusplus)
		  declarator = build_decl_overload (name, TYPE_ARG_TYPES (type), 0);
		was_overloaded = 1;
	      }
#else
	    if (current_lang_name == lang_name_cplusplus
		&& ! (IDENTIFIER_LENGTH (original_name) == 4
		      && IDENTIFIER_POINTER (original_name)[0] == 'm'
		      && strcmp (IDENTIFIER_POINTER (original_name), "main") == 0)
		&& ! (IDENTIFIER_LENGTH (original_name) > 10
		      && IDENTIFIER_POINTER (original_name)[0] == '_'
		      && IDENTIFIER_POINTER (original_name)[1] == '_'
		      && strncmp (IDENTIFIER_POINTER (original_name)+2, "builtin_", 8) == 0))
	      {
		/* Plain overloading: will not be grok'd by grokclassfn.  */
		declarator = build_decl_overload (name, TYPE_ARG_TYPES (type), 0);
		was_overloaded = 1;
	      }
#endif
	  }
	else if (TREE_CODE (type) == FUNCTION_TYPE && staticp < 2)
	  type = build_cplus_method_type (ctype, TREE_TYPE (type), TYPE_ARG_TYPES (type));

	decl = grokfndecl (ctype, type, declarator, virtualp, flags, quals, raises, friendp ? 2 : 1);
	/* Record presence of `static'.  In C++, `inline' is like `static'.  */
	TREE_PUBLIC (decl) = !(specbits & ((1 << (int) RID_STATIC) | (1 << (int) RID_INLINE)));
	/* Record presence of `inline', if it is reasonable.  */
	if (inlinep)
	  {
	    tree last = tree_last (TYPE_ARG_TYPES (type));

	    if (! was_overloaded
		&& ! ctype
		&& ! strcmp (IDENTIFIER_POINTER (original_name), "main"))
	      warning ("cannot inline function `main'");
	    else if (last && last != void_list_node)
	      warning ("inline declaration ignored for function with `...'");
	    else
	      /* Assume that otherwise the function can be inlined.  */
	      TREE_INLINE (decl) = 1;

	    if (specbits & (1 << (int) RID_EXTERN))
	      current_extern_inline = 1;
	  }
	if (was_overloaded)
	  {
	    DECL_OVERLOADED (decl) = 1;
	    DECL_ORIGINAL_NAME (decl) = original_name;
	  }
      }
    else
      {
	/* It's a variable.  */

	if (virtualp)
	  error ("variable declared `virtual'");
	if (inlinep)
	  warning ("variable declared `inline'");
	if (quals)
	  error ("`const' and `volatile' function specifiers invalid in field declaration");
	if (friendp)
	  error ("invalid friend declaration");
	if (raises)
	  error ("invalid raises declaration");

	/* An uninitialized decl with `extern' is a reference.  */
	decl = grokvardecl (ctype, type, declarator, specbits, initialized);
      }

    /* Record `register' declaration for warnings on &
       and in case doing stupid register allocation.  */

    if (specbits & (1 << (int) RID_REGISTER))
      TREE_REGDECL (decl) = 1;

    /* Record constancy and volatility.  */

    if (constp)
      TREE_READONLY (decl) = TREE_CODE (type) != REFERENCE_TYPE;
    if (volatilep)
      {
	TREE_VOLATILE (decl) = 1;
	TREE_THIS_VOLATILE (decl) = 1;
      }

    if (resume_temporary)
      resume_temporary_allocation ();

    return decl;
  }
}

/* Tell if a parmlist/exprlist looks like an exprlist or a parmlist.
   An empty exprlist is a parmlist.  An exprlist which
   contains only identifiers at the global level
   is a parmlist.  Otherwise, it is an exprlist. */
static int
parmlist_is_exprlist (exprs)
     tree exprs;
{
  if (exprs == NULL_TREE || TREE_PARMLIST (exprs))
    return 0;

  if (current_binding_level == global_binding_level)
    {
      /* At the global level, if these are all identifiers,
	 then it is a parmlist.  */
      while (exprs)
	{
	  if (TREE_CODE (TREE_VALUE (exprs)) != IDENTIFIER_NODE)
	    return 1;
	  exprs = TREE_CHAIN (exprs);
	}
      return 0;
    }
  return 1;
}

/* Make sure that the this list of PARMS has a chance of being
   grokked by `grokparms'.

   @@ This is really weak, but the grammar does not allow us
   @@ to easily reject things that this has to catch as syntax errors.  */
static int
parmlist_is_random (parms)
     tree parms;
{
  if (parms == NULL_TREE)
    return 0;

  if (TREE_CODE (parms) != TREE_LIST)
    return 1;

  while (parms)
    {
      if (parms == void_list_node)
	return 0;

      if (TREE_CODE (TREE_VALUE (parms)) != TREE_LIST)
	return 1;
      /* Don't get faked out by overloaded functions, which
	 masquerade as TREE_LISTs!  */
      if (TREE_TYPE (TREE_VALUE (parms)) == unknown_type_node)
	return 1;
      parms = TREE_CHAIN (parms);
    }
  return 0;
}

/* Subroutine of `grokparms'.  In a fcn definition, arg types must
   be complete.

   C++: also subroutine of `start_function'.  */
static void
require_complete_types_for_parms (parms)
     tree parms;
{
  while (parms)
    {
      tree type = TREE_TYPE (parms);
      if (TYPE_SIZE (type) == 0)
	{
	  if (DECL_NAME (parms))
	    error ("parameter `%s' has incomplete type",
		   IDENTIFIER_POINTER (DECL_NAME (parms)));
	  else
	    error ("parameter has incomplete type");
	  TREE_TYPE (parms) = error_mark_node;
	}
#if 0
      /* If the arg types are incomplete in a declaration,
	 they must include undefined tags.
	 These tags can never be defined in the scope of the declaration,
	 so the types can never be completed,
	 and no call can be compiled successfully.  */
      /* This is not the right behavior for C++, but not having
	 it is also probably wrong.  */
      else
	{
	  /* Now warn if is a pointer to an incomplete type.  */
	  while (TREE_CODE (type) == POINTER_TYPE
		 || TREE_CODE (type) == REFERENCE_TYPE)
	    type = TREE_TYPE (type);
	  type = TYPE_MAIN_VARIANT (type);
	  if (TYPE_SIZE (type) == 0)
	    {
	      if (DECL_NAME (parm) != 0)
		warning ("parameter `%s' points to incomplete type",
			 IDENTIFIER_POINTER (DECL_NAME (parm)));
	      else
		warning ("parameter points to incomplete type");
	    }
	}
#endif
      parms = TREE_CHAIN (parms);
    }
}

/* Decode the list of parameter types for a function type.
   Given the list of things declared inside the parens,
   return a list of types.

   The list we receive can have three kinds of elements:
   an IDENTIFIER_NODE for names given without types,
   a TREE_LIST node for arguments given as typespecs or names with typespecs,
   or void_type_node, to mark the end of an argument list
   when additional arguments are not permitted (... was not used).

   FUNCDEF_FLAG is nonzero for a function definition, 0 for
   a mere declaration.  A nonempty identifier-list gets an error message
   when FUNCDEF_FLAG is zero.
   If FUNCDEF_FLAG is 1, then parameter types must be complete.
   If FUNCDEF_FLAG is -1, then parameter types may be incomplete.

   If all elements of the input list contain types,
   we return a list of the types.
   If all elements contain no type (except perhaps a void_type_node
   at the end), we return a null list.
   If some have types and some do not, it is an error, and we
   return a null list.

   Also set last_function_parms to either
   a list of names (IDENTIFIER_NODEs) or a chain of PARM_DECLs.
   A list of names is converted to a chain of PARM_DECLs
   by store_parm_decls so that ultimately it is always a chain of decls.

   Note that in C++, paramters can take default values.  These default
   values are in the TREE_PURPOSE field of the TREE_LIST.  It is
   an error to specify default values which are followed by parameters
   that have no defualt values, or an ELLIPSES.  For simplicities sake,
   only parameters which are specified with their types can take on
   default values.  */

static tree
grokparms (first_parm, funcdef_flag)
     tree first_parm;
     int funcdef_flag;
{
  tree result = NULL_TREE;
  tree decls = NULL_TREE;

  if (first_parm != 0
      && TREE_CODE (TREE_VALUE (first_parm)) == IDENTIFIER_NODE)
    {
      if (! funcdef_flag)
	warning ("parameter names (without types) in function declaration");
      last_function_parms = first_parm;
      return 0;
    }
  else
    {
      /* Types were specified.  This is a list of declarators
	 each represented as a TREE_LIST node.  */
      register tree parm, chain;
      int any_init = 0, any_error = 0, saw_void = 0;

      if (first_parm != NULL_TREE)
	{
	  tree last_result = NULL_TREE;
	  tree last_decl = NULL_TREE;

	  for (parm = first_parm; parm != NULL_TREE; parm = chain)
	    {
	      tree type, list_node = parm;
	      register tree decl = TREE_VALUE (parm);
	      tree init = TREE_PURPOSE (parm);

	      chain = TREE_CHAIN (parm);
	      /* @@ weak defense against parse errors.  */
	      if (decl != void_type_node && TREE_CODE (decl) != TREE_LIST)
		{
		  /* Give various messages as the need arises.  */
		  if (TREE_CODE (decl) == STRING_CST)
		    error ("invalid string constant `%s'",
			   TREE_STRING_POINTER (decl));
		  else if (TREE_CODE (decl) == INTEGER_CST)
		    error ("invalid integer constant in parameter list, did you forget to give parameter name?");
		  continue;
		}

	      if (decl != void_type_node)
		{
		  /* @@ May need to fetch out a `raises' here.  */
		  decl = grokdeclarator (TREE_VALUE (decl),
					 TREE_PURPOSE (decl),
					 PARM, 0, NULL_TREE);
		  if (! decl) continue;
		  type = TREE_TYPE (decl);
		  if (type == void_type_node)
		    decl = void_type_node;
		  else if (TREE_CODE (type) == METHOD_TYPE)
		    {
		      if (DECL_NAME (decl))
			/* Cannot use `error_with_decl' here because
			   we don't have DECL_CONTEXT set up yet.  */
			error ("parameter `%s' invalidly declared method type",
			       IDENTIFIER_POINTER (DECL_NAME (decl)));
		      else
			error ("parameter invalidly declared method type");
		      type = build_pointer_type (type);
		      TREE_TYPE (decl) = type;
		    }
		  else if (TREE_CODE (type) == OFFSET_TYPE)
		    {
		      if (DECL_NAME (decl))
			error ("parameter `%s' invalidly declared offset type",
			       IDENTIFIER_POINTER (DECL_NAME (decl)));
		      else
			error ("parameter invalidly declared offset type");
		      type = build_pointer_type (type);
		      TREE_TYPE (decl) = type;
		    }
		}

	      if (decl == void_type_node)
		{
		  if (result == NULL_TREE)
		    {
		      result = void_list_node;
		      last_result = result;
		    }
		  else
		    {
		      TREE_CHAIN (last_result) = void_list_node;
		      last_result = void_list_node;
		    }
		  saw_void = 1;
		  if (chain
		      && (chain != void_list_node || TREE_CHAIN (chain)))
		    error ("`void' in parameter list must be entire list");
		  break;
		}

	      /* Since there is a prototype, args are passed in their own types.  */
	      DECL_ARG_TYPE (decl) = TREE_TYPE (decl);
#ifdef PROMOTE_PROTOTYPES
	      if (TREE_CODE (type) == INTEGER_TYPE
		  && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
		DECL_ARG_TYPE (decl) = integer_type_node;
#endif
	      if (!any_error)
		{
		  if (init)
		    {
		      any_init++;
		      if (TREE_CODE (init) == SAVE_EXPR)
			PARM_DECL_EXPR (init) = 1;
		      else
			init = require_instantiated_type (type, init, integer_zero_node);
		    }
		  else if (any_init)
		    {
		      error ("all trailing parameters must have default arguments");
		      any_error = 1;
		    }
		}
	      else
		init = NULL_TREE;

	      if (decls == NULL_TREE)
		{
		  decls = decl;
		  last_decl = decls;
		}
	      else
		{
		  TREE_CHAIN (last_decl) = decl;
		  last_decl = decl;
		}
	      if (TREE_PERMANENT (list_node))
		{
		  TREE_PURPOSE (list_node) = init;
		  TREE_VALUE (list_node) = type;
		  TREE_CHAIN (list_node) = 0;
		}
	      else
		list_node = saveable_tree_cons (init, type, NULL_TREE);
	      if (result == NULL_TREE)
		{
		  result = list_node;
		  last_result = result;
		}
	      else
		{
		  TREE_CHAIN (last_result) = list_node;
		  last_result = list_node;
		}
	    }
	  if (last_result)
	    TREE_CHAIN (last_result) = NULL_TREE;
	  /* If there are no parameters, and the function does not end
	     with `...', then last_decl will be NULL_TREE.  */
	  if (last_decl != NULL_TREE)
	    TREE_CHAIN (last_decl) = NULL_TREE;
	}
    }

  last_function_parms = decls;

  /* In a fcn definition, arg types must be complete.  */
  if (funcdef_flag > 0)
    require_complete_types_for_parms (last_function_parms);

  return result;
}

/* These memoizing functions keep track of special properties which
   a class may have.  `grok_ctor_properties' notices whether a class
   has a constructor of the for X(X&), and also complains
   if the class has a constructor of the form X(X).
   `grok_op_properties' takes notice of the various forms of
   operator= which are defined, as well as what sorts of type conversion
   may apply.  Both functions take a FUNCTION_DECL as an argument.  */
static void
grok_ctor_properties (ctype, decl)
     tree ctype, decl;
{
  tree parmtypes = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl)));
  tree parmtype = parmtypes ? TREE_VALUE (parmtypes) : void_type_node;

  if (TREE_CODE (parmtype) == REFERENCE_TYPE
      && TYPE_MAIN_VARIANT (TREE_TYPE (parmtype)) == ctype)
    {
      if (TREE_CHAIN (parmtypes) == NULL_TREE
	  || TREE_CHAIN (parmtypes) == void_list_node
	  || TREE_PURPOSE (TREE_CHAIN (parmtypes)))
	{
	  TYPE_HAS_INIT_REF (ctype) = 1;
	  TYPE_GETS_INIT_REF (ctype) = 1;
	  if (TREE_READONLY (TREE_TYPE (parmtype)))
	    TYPE_GETS_CONST_INIT_REF (ctype) = 1;
	}
      else
	TYPE_GETS_INIT_AGGR (ctype) = 1;
    }
  else if (TYPE_MAIN_VARIANT (parmtype) == ctype)
    {
      if (TREE_CHAIN (parmtypes) != NULL_TREE
	  && TREE_CHAIN (parmtypes) == void_list_node)
	error ("invalid constructor; you probably meant `%s (%s&)'",
	       IDENTIFIER_POINTER (current_class_name),
	       IDENTIFIER_POINTER (current_class_name));
      SET_IDENTIFIER_ERROR_LOCUS (DECL_NAME (decl), ctype);
      TYPE_GETS_INIT_AGGR (ctype) = 1;
    }
  else if (TREE_CODE (parmtype) == VOID_TYPE
	   || TREE_PURPOSE (parmtypes) != NULL_TREE)
    TYPE_HAS_DEFAULT_CONSTRUCTOR (ctype) = 1;
}

static void
grok_op_properties (decl)
     tree decl;
{
  char *name = IDENTIFIER_POINTER (DECL_NAME (decl));
  tree argtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));

  if (DECL_STATIC_FUNCTION_P (decl))
    {
      if (! strncmp (name, OPERATOR_NEW_FORMAT, OPERATOR_NEW_LENGTH))
	{
	  /* Take care of function decl if we had syntax errors.  */
	  if (argtypes == NULL_TREE)
	    TREE_TYPE (decl) = build_function_type (ptr_type_node,
						    hash_tree_chain (integer_type_node, void_list_node));
	}
      else if (! strncmp (name, OPERATOR_DELETE_FORMAT, OPERATOR_DELETE_LENGTH))
	{
	  if (argtypes == NULL_TREE)
	    TREE_TYPE (decl) = build_function_type (void_type_node,
						    hash_tree_chain (ptr_type_node, void_list_node));
	}
      else
	error_with_decl (decl, "`%s' cannot be a static member function");
    }
  else if (! strncmp (name, OPERATOR_MODIFY_FORMAT, OPERATOR_MODIFY_LENGTH))
    {
      tree parmtypes = TREE_CHAIN (argtypes);
      tree parmtype = parmtypes ? TREE_VALUE (parmtypes) : void_type_node;

      if (TREE_CODE (parmtype) == REFERENCE_TYPE
	  && TREE_TYPE (parmtype) == current_class_type)
	{
	  TYPE_HAS_ASSIGN_REF (current_class_type) = 1;
	  TYPE_GETS_ASSIGN_REF (current_class_type) = 1;
	  if (TREE_READONLY (TREE_TYPE (parmtype)))
	    TYPE_GETS_CONST_INIT_REF (current_class_type) = 1;
	}
    }
}

/* Get the struct, enum or union (CODE says which) with tag NAME.
   Define the tag as a forward-reference if it is not defined.

   C++: If a class derivation is given, process it here, and report
   an error if multiple derivation declarations are not identical.

   If we are compiling for SOS, then
     if CODE_TYPE_NODE is a TREE_LIST, then we have a dynamic class
     declaration.  The name associated with the class is the tree
     purpose, and the real CODE is in the tree value slot.  */
tree
xref_tag (code_type_node, name, binfo)
     tree code_type_node;
     tree name, binfo;
{
  enum tag_types tag_code;
  enum tree_code code;
  int temp = 0;
  int i, len;
  register tree ref;
  struct binding_level *b
    = (class_binding_level ? class_binding_level : current_binding_level);
#ifdef SOS
  tree dynamic_name = error_mark_node;
  if (TREE_CODE (code_type_node) == TREE_LIST)
    {
      dynamic_name = TREE_PURPOSE (code_type_node);
      code_type_node = TREE_VALUE (code_type_node);
    }
#endif

  tag_code = (enum tag_types) TREE_INT_CST_LOW (code_type_node);
  switch (tag_code)
    {
    case record_type:
    case class_type:
    case exception_type:
      code = RECORD_TYPE;
      len = list_length (binfo) + 1;
      break;
    case union_type:
      code = UNION_TYPE;
      if (binfo)
	{
	  error ("derived union `%s' invalid", IDENTIFIER_POINTER (name));
	  binfo = NULL_TREE;
	}
      len = 1;
      break;
    case enum_type:
      code = ENUMERAL_TYPE;
      break;
    default:
      abort ();
    }

  /* If a cross reference is requested, look up the type
     already defined for this tag and return it.  */
  ref = lookup_tag (code, name, b, 0);

  if (! ref)
    {
      /* Try finding it as a type declaration.  If that wins, use it.  */
      ref = lookup_name (name);
      if (ref && TREE_CODE (ref) == TYPE_DECL
	     && TREE_CODE (TREE_TYPE (ref)) == code)
	ref = TREE_TYPE (ref);
      else
	ref = NULL_TREE;
    }

  if (! ref)
    {
      /* If no such tag is yet defined, create a forward-reference node
	 and record it as the "definition".
	 When a real declaration of this type is found,
	 the forward-reference will be altered into a real type.  */

      /* In C++, since these migrate into the global scope, we must
	 build them on the permanent obstack.  */
      if (temp == 0)
	temp = allocation_temporary_p ();
      if (temp)
	end_temporary_allocation ();

      if (code == ENUMERAL_TYPE)
	{
	  ref = make_node (ENUMERAL_TYPE);

	  /* Give the type a default layout like unsigned int
	     to avoid crashing if it does not get defined.  */
	  TYPE_MODE (ref) = SImode;
	  TYPE_ALIGN (ref) = TYPE_ALIGN (unsigned_type_node);
	  TREE_UNSIGNED (ref) = 1;
	  TYPE_PRECISION (ref) = TYPE_PRECISION (unsigned_type_node);
	  TYPE_MIN_VALUE (ref) = TYPE_MIN_VALUE (unsigned_type_node);
	  TYPE_MAX_VALUE (ref) = TYPE_MAX_VALUE (unsigned_type_node);

	  /* Enable us to recognize when a type is created in class context.
	     To do nested classes correctly, this should probably be cleared
	     out when we leave this classes scope.  Currently this in only
	     done in `start_enum'.  */

	  pushtag (name, ref);
	  if (flag_cadillac)
	    cadillac_start_enum (ref);
	}
      else if (tag_code == exception_type)
	{
	  ref = make_lang_type (code);
	  CLASSTYPE_OFFSET (ref) = integer_zero_node;
	  /* Enable us to recognize when an exception type is created in
	     class context.  To do nested classes correctly, this should
	     probably be cleared out when we leave this class's scope.  */
	  CLASSTYPE_DECLARED_EXCEPTION (ref) = 1;
	  pushtag (name, ref);
	  if (flag_cadillac)
	    cadillac_start_struct (ref);
	}
      else
	{
	  extern tree pending_vtables;
	  struct binding_level *old_b = class_binding_level;
	  int needs_writing;

	  ref = make_lang_type (code);

	  CLASSTYPE_BASECLASSES (ref) = (tree *) malloc (len * sizeof (tree));
	  CLASSTYPE_N_BASECLASSES (ref) = len - 1;
	  CLASSTYPE_OFFSET (ref) = integer_zero_node;
	  CLASSTYPE_VIAS (ref) = (unsigned char *) malloc (len);

	  /* Record how to set the visibility of this class's
	     virtual functions.  If write_virtuals == 2 or 3, then
	     inline virtuals are ``extern inline''.  */
	  switch (write_virtuals)
	    {
	    case 0:
	    case 1:
	      needs_writing = 1;
	      break;
	    case 2:
	      needs_writing = !! value_member (name, pending_vtables);
	      break;
	    case 3:
	      needs_writing
		= ! (CLASSTYPE_INTERFACE_ONLY (ref) || CLASSTYPE_INTERFACE_UNKNOWN (ref));
	      break;
	    default:
	      needs_writing = 0;
	    }

	  CLASSTYPE_VTABLE_NEEDS_WRITING (ref) = needs_writing;

	  /* Class types don't nest the way enums do.  */
	  class_binding_level = 0;
	  pushtag (name, ref);
	  class_binding_level = old_b;

	  if (flag_cadillac)
	    cadillac_start_struct (ref);
	}
    }
  else
    {
      if (IS_AGGR_TYPE_CODE (code))
	{
#if 0
	  if (TREE_CODE (TYPE_NAME (ref)) == IDENTIFIER_NODE
#ifndef BREAK_C_TAGS
	      && current_lang_name == lang_name_cplusplus
#endif
	      && ! CLASSTYPE_DECLARED_EXCEPTION (ref))
	    {
	      /* Silently typedef a tag which came from C.  */
	      register tree t = pushdecl (build_decl (TYPE_DECL, name, ref));
	      TYPE_NAME (ref) = t;
	      TREE_TYPE (name) = t;
	    }
#endif
	  if (IS_AGGR_TYPE (ref)
	      && ((tag_code == exception_type)
		  != (CLASSTYPE_DECLARED_EXCEPTION (ref) == 1)))
	    {
	      error ("type `%s' is both exception and aggregate type",
		     IDENTIFIER_POINTER (name));
	      CLASSTYPE_DECLARED_EXCEPTION (ref) = (tag_code == exception_type);
	    }
	}
      if (binfo)
	{
	  tree tt1 = binfo;
	  tree *tt2 = CLASSTYPE_BASECLASSES (ref);

	  if (CLASSTYPE_N_BASECLASSES (ref))
	    for (i = 1; tt1; i++, tt1 = TREE_CHAIN (tt1))
	      if (TREE_VALUE (tt1) != DECL_NAME (TYPE_NAME (tt2[i])))
		{
		  error ("redeclaration of derivation chain of type `%s'",
			 IDENTIFIER_POINTER (name));
		  break;
		}

	  if (tt1 != NULL_TREE)
	    {
	      free (CLASSTYPE_BASECLASSES (ref));
	      free (CLASSTYPE_VIAS (ref));
	      CLASSTYPE_BASECLASSES (ref) = (tree *) malloc (len * sizeof (tree));
	      CLASSTYPE_N_BASECLASSES (ref) = len - 1;
	      CLASSTYPE_OFFSET (ref) = integer_zero_node;
	      CLASSTYPE_ASSOC (ref) = NULL_TREE;
	      CLASSTYPE_VIAS (ref) = (unsigned char *) malloc (len);
	    }
	  else
	    {
	      /* The user told us something we already knew.  */
	      goto just_return;
	    }
	}
#ifdef SOS
      else if (TREE_CODE (ref) != ENUMERAL_TYPE
	       && (dynamic_name != error_mark_node) != TYPE_DYNAMIC (ref))
	error ("type `%s' declared both dynamic and non-dynamic",
	       IDENTIFIER_POINTER (name));
#endif
    }

  if (binfo)
    {
      CLASSTYPE_MARKED (ref) = 1;
      for (i = 1; binfo; binfo = TREE_CHAIN (binfo))
	{
	  /* The base of a derived struct is public.  */
	  int via_public = (tag_code != class_type
			    || TREE_PURPOSE (binfo) == (tree)visibility_public
			    || TREE_PURPOSE (binfo) == (tree)visibility_public_virtual);
	  int via_virtual = (TREE_PURPOSE (binfo) == (tree)visibility_private_virtual
			     || TREE_PURPOSE (binfo) == (tree)visibility_public_virtual
			     || TREE_PURPOSE (binfo) == (tree)visibility_default_virtual);
	  tree basetype = TREE_TYPE (TREE_VALUE (binfo));

#ifdef FIELD_XREF
	  FIELD_xref_hier(IDENTIFIER_POINTER(name),
			  IDENTIFIER_POINTER(TREE_VALUE(binfo)),
			  via_public,via_virtual,0);
#endif

	  if (basetype && TREE_CODE (basetype) == TYPE_DECL)
	    basetype = TREE_TYPE (basetype);
	  if (!basetype || TREE_CODE (basetype) != RECORD_TYPE)
	    {
	      error ("base type `%s' fails to be a struct or class type",
		     IDENTIFIER_POINTER (TREE_VALUE (binfo)));
	      continue;
	    }
#if 0
	  else if (TYPE_SIZE (basetype) == 0)
	    {
	      error_with_aggr_type (basetype, "base class `%s' has incomplete type");
	      continue;
	    }
#endif
	  else
	    {
#ifdef SOS
	      if (dynamic_name == error_mark_node && TYPE_DYNAMIC (basetype))
		error_with_aggr_type (ref, "non-dynamic type `%s' cannot derive from dynamic type `%s'", TYPE_NAME_STRING (basetype));
#endif
	      if (CLASSTYPE_MARKED (basetype))
		{
		  if (basetype == ref)
		    error_with_aggr_type (basetype, "recursive type `%s' undefined");
		  else
		    error_with_aggr_type (basetype, "duplicate base type `%s' invalid");
		  continue;
		}
	      CLASSTYPE_BASECLASS (ref, i) = basetype;
	      CLASSTYPE_MARKED (basetype) = 1;
#if 0
/* XYZZY TEST VIRTUAL BASECLASSES */
if (CLASSTYPE_N_BASECLASSES (basetype) == 0
    && TYPE_HAS_DEFAULT_CONSTRUCTOR (basetype)
    && via_virtual == 0)
  {
    warning ("making type `%s' a virtual baseclass",
	     TYPE_NAME_STRING (basetype));
    via_virtual = 1;
  }
#endif
	      SET_CLASSTYPE_VIAS (ref, i, via_public, via_virtual);
	      if (via_virtual || TYPE_USES_VIRTUAL_BASECLASSES (basetype))
		TYPE_USES_VIRTUAL_BASECLASSES (ref) = 1;

	      TYPE_GETS_ASSIGNMENT (ref) |= TYPE_GETS_ASSIGNMENT (basetype);
	      TYPE_OVERLOADS_METHOD_CALL_EXPR (ref) |= TYPE_OVERLOADS_METHOD_CALL_EXPR (basetype);
	      TYPE_HAS_WRAPPER_PRED (ref) |= TYPE_HAS_WRAPPER_PRED (basetype);
	      TREE_GETS_NEW (ref) |= TREE_GETS_NEW (basetype);
	      TREE_GETS_DELETE (ref) |= TREE_GETS_DELETE (basetype);
	      CLASSTYPE_LOCAL_TYPEDECLS (ref) |= CLASSTYPE_LOCAL_TYPEDECLS (basetype);
	      i += 1;
	    }
	}
      /* Set the true number of baseclasses this type really has.  */
      CLASSTYPE_N_BASECLASSES (ref) = --i;

      if (i > 1)
	TYPE_USES_MULTIPLE_INHERITANCE (ref) = 1;
      else if (i == 1)
	TYPE_USES_MULTIPLE_INHERITANCE (ref)
	  = TYPE_USES_MULTIPLE_INHERITANCE (CLASSTYPE_BASECLASS (ref, 1));

      while (i > 0)
	{
	  CLASSTYPE_MARKED (CLASSTYPE_BASECLASS (ref, i)) = 0;
	  i -= 1;
	}
      CLASSTYPE_MARKED (ref) = 0;
    }

 just_return:

#ifdef SOS
  if (dynamic_name != error_mark_node)
    {
      if (temp == 0)
	temp = allocation_temporary_p ();
      if (temp)
	end_temporary_allocation ();

      if (dynamic_name)
	CLASSTYPE_DYNAMIC_FILENAME (ref) = combine_strings (dynamic_name);
      else
	CLASSTYPE_DYNAMIC_FILENAME (ref) = NULL_TREE;
      TYPE_DYNAMIC (ref) = 1;
      CLASSTYPE_TYPENAME_AS_STRING (ref) = combine_strings (build_string (IDENTIFIER_LENGTH (name), IDENTIFIER_POINTER (name)));

    }
#endif

  /* Until the type is defined, tentatively accept whatever
     structure tag the user hands us.  */
  if (TYPE_SIZE (ref) == NULL_TREE
      && ref != current_class_type
      && IS_AGGR_TYPE (ref))
    {
      if (tag_code == class_type)
	CLASSTYPE_DECLARED_CLASS (ref) = 1;
      else if (tag_code == record_type)
	CLASSTYPE_DECLARED_CLASS (ref) = 0;
    }

  if (temp)
    resume_temporary_allocation ();

  return ref;
}

/* Begin compiling the definition of an enumeration type.
   NAME is its name (or null if anonymous).
   Returns the type object, as yet incomplete.
   Also records info about it so that build_enumerator
   may be used to declare the individual values as they are read.  */

tree
start_enum (name)
     tree name;
{
  register tree enumtype = 0;
  struct binding_level *b
    = (class_binding_level ? class_binding_level : current_binding_level);

  /* If this is the real definition for a previous forward reference,
     fill in the contents in the same object that used to be the
     forward reference.  */

  if (name != 0)
    enumtype = lookup_tag (ENUMERAL_TYPE, name, b, 1);

  if (enumtype == 0 || TREE_CODE (enumtype) != ENUMERAL_TYPE)
    {
      enumtype = make_node (ENUMERAL_TYPE);
      pushtag (name, enumtype);
    }

  if (TYPE_VALUES (enumtype) != 0)
    {
      /* This enum is a named one that has been declared already.  */
      error ("redeclaration of `enum %s'", IDENTIFIER_POINTER (name));

      /* Completely replace its old definition.
	 The old enumerators remain defined, however.  */
      TYPE_VALUES (enumtype) = 0;
    }

  /* Initially, set up this enum as like `int'
     so that we can create the enumerators' declarations and values.
     Later on, the precision of the type may be changed and
     it may be laid out again.  */

  TYPE_PRECISION (enumtype) = TYPE_PRECISION (integer_type_node);
  TYPE_SIZE (enumtype) = 0;
  fixup_unsigned_type (enumtype);

  /* We copy this value because enumerated type constants
     are really of the type of the enumerator, not integer_type_node.  */
  enum_next_value = copy_node (integer_zero_node);

#ifdef FIELD_XREF
  FIELD_xref_decl(current_function_decl,enumtype);
#endif

  return enumtype;
}

/* After processing and defining all the values of an enumeration type,
   install their decls in the enumeration type and finish it off.
   ENUMTYPE is the type object and VALUES a list of name-value pairs.
   Returns ENUMTYPE.  */

tree
finish_enum (enumtype, values)
     register tree enumtype, values;
{
  register tree pair;
  register long maxvalue = 0;
  register long minvalue = 0;
  register int i;

  TYPE_VALUES (enumtype) = values;

  /* Calculate the maximum value of any enumerator in this type.  */

  if (values)
    {
      /* Speed up the main loop by performing some precalculations */

      int value = TREE_INT_CST_LOW (TREE_VALUE (values));
      TREE_TYPE (TREE_VALUE (values)) = enumtype;
      minvalue = maxvalue = value;
      
      for (pair = TREE_CHAIN (values); pair; pair = TREE_CHAIN (pair))
	{
	  value = TREE_INT_CST_LOW (TREE_VALUE (pair));
	  if (value > maxvalue)
	    maxvalue = value;
	  else if (value < minvalue)
	    minvalue = value;
	  TREE_TYPE (TREE_VALUE (pair)) = enumtype;
	}
    }

  if (flag_short_enums)
    {
      /* Determine the precision this type needs, lay it out, and define it.  */

      for (i = maxvalue; i; i >>= 1)
	TYPE_PRECISION (enumtype)++;

      if (!TYPE_PRECISION (enumtype))
	TYPE_PRECISION (enumtype) = 1;

      /* Cancel the laying out previously done for the enum type,
	 so that fixup_unsigned_type will do it over.  */
      TYPE_SIZE (enumtype) = 0;

      fixup_unsigned_type (enumtype);
    }

  TREE_INT_CST_LOW (TYPE_MAX_VALUE (enumtype)) = maxvalue;

  /* An enum can have some negative values; then it is signed.  */
  if (minvalue < 0)
    {
      TREE_INT_CST_LOW (TYPE_MIN_VALUE (enumtype)) = minvalue;
      TREE_INT_CST_HIGH (TYPE_MIN_VALUE (enumtype)) = -1;
      TREE_UNSIGNED (enumtype) = 0;
    }
  if (flag_cadillac)
    cadillac_finish_enum (enumtype);

  return enumtype;
}

/* Build and install a CONST_DECL for one value of the
   current enumeration type (one that was begun with start_enum).
   Return a tree-list containing the name and its value.
   Assignment of sequential values by default is handled here.  */

tree
build_enumerator (name, value)
     tree name, value;
{
  tree decl, result;

  /* Validate and default VALUE.  */
  if (value != 0)
    {
      if (TREE_READONLY_DECL_P (value))
	value = decl_constant_value (value);

      if (TREE_CODE (value) != INTEGER_CST)
	{
	  error ("enumerator value for `%s' not integer constant",
		 IDENTIFIER_POINTER (name));
	  value = 0;
	}
    }
  /* The order of things is reversed here so that we
     can check for possible sharing of enum values,
     to keep that from happening.  */
  /* Default based on previous value.  */
  if (value == 0)
    value = enum_next_value;

  /* Remove no-op casts from the value.  */
  while (value != 0 && TREE_CODE (value) == NOP_EXPR)
    value = TREE_OPERAND (value, 0);

  /* Make up for hacks in cplus-lex.c.  */
  if (value == integer_zero_node)
    value = build_int_2 (0, 0);
  else if (value == integer_one_node)
    value = build_int_2 (1, 0);
  else if (TREE_CODE (value) == INTEGER_CST
	   && TREE_CODE (TREE_TYPE (value)) == ENUMERAL_TYPE)
    {
      value = copy_node (value);
      TREE_TYPE (value) = integer_type_node;
    }

  result = saveable_tree_cons (name, value, NULL_TREE);

  /* C++ associates enums with global, function, or class declarations.  */
  if (current_class_type == NULL_TREE || current_function_decl != NULL_TREE)
    {
      /* Create a declaration for the enum value name.  */

      decl = build_decl (CONST_DECL, name, integer_type_node);
      DECL_INITIAL (decl) = value;

      pushdecl (decl);
    }

  /* Set basis for default for next value.  */
  enum_next_value = build_binary_op_nodefault (PLUS_EXPR, value,
					       integer_one_node, PLUS_EXPR);
  if (TREE_UID (enum_next_value) < TREE_UID (result))
    enum_next_value = copy_node (enum_next_value);

  return result;
}

tree
grok_enum_decls (type, decl)
     tree type, decl;
{
  struct binding_level *b = class_binding_level;
  tree tag = NULL_TREE;
  tree values;

  while (b)
    {
      tag = value_member (type, b->tags);
      if (tag)
	break;
      b = b->level_chain;
    }

  if (b == 0)
    {
      tree name = TYPE_NAME (type);
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
      error ("class-local enum declaration `%s' is not in scope here",
	     IDENTIFIER_POINTER (name));
    }
  else if (b != class_binding_level)
    {
      warning ("class-local declaration for enumeral type `%s' found",
	       IDENTIFIER_POINTER (TREE_PURPOSE (tag)));
      warning ("(probably missing '}' before that enum declaration)");
      return decl;
    }
  else if (TREE_ADDRESSABLE (tag))
    return decl;
  else
    TREE_ADDRESSABLE (tag) = 1;

  values = TYPE_VALUES (type);
  while (values)
    {
      /* Create a declaration for the enum value name.  */
      tree next = build_lang_field_decl (CONST_DECL, TREE_PURPOSE (values), type);
      DECL_INITIAL (next) = TREE_VALUE (values);
      TREE_CHAIN (next) = decl;
      decl = next;
      pushdecl_class_level (decl);
      values = TREE_CHAIN (values);
    }
  return decl;
}

/* Create the FUNCTION_DECL for a function definition.
   DECLSPECS and DECLARATOR are the parts of the declaration;
   they describe the function's name and the type it returns,
   but twisted together in a fashion that parallels the syntax of C.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns 1 on success.  If the DECLARATOR is not suitable for a function
   (it defines a datum instead), we return 0, which tells
   yyparse to report a parse error.

   For C++, we must first check whether that datum makes any sense.
   For example, "class A local_a(1,2);" means that variable local
   a is an aggregate of type A, which should have a constructor
   applied to it with the argument list [1, 2].

   @@ There is currently no way to retrieve the storage
   @@ allocated to FUNCTION (or all of its parms) if we return
   @@ something we had previously.  */

int
start_function (declspecs, declarator, raises, pre_parsed_p)
     tree declarator, declspecs, raises;
     int pre_parsed_p;
{
  extern int interface_only, interface_unknown;
  extern tree EHS_decl;
  tree decl1, olddecl;
  tree ctype = NULL_TREE;
  tree fntype;
  tree restype;

  if (flag_handle_exceptions && EHS_decl == NULL_TREE)
    init_exception_processing_1 ();

  /* Sanity check.  */
  assert (TREE_VALUE (void_list_node) == void_type_node);
  assert (TREE_CHAIN (void_list_node) == NULL_TREE);

  /* Assume, until we see it does. */
  current_function_returns_value = 0;
  current_function_returns_null = 0;
  warn_about_return_type = 0;
  current_extern_inline = 0;
  current_function_assigns_this = 0;
  current_function_just_assigned_this = 0;
  current_function_parms_stored = 0;
  original_result_rtx = 0;

  clear_temp_name ();

  if (pre_parsed_p)
    {
      decl1 = declarator;
      last_function_parms = DECL_ARGUMENTS (decl1);
      last_function_parm_tags = 0;
      fntype = TREE_TYPE (decl1);
      if (TREE_CODE (fntype) == METHOD_TYPE)
	ctype = TYPE_METHOD_BASETYPE (fntype);

      if ( !(DECL_VIRTUAL_P (decl1)
	     && write_virtuals >= 2
	     && CLASSTYPE_VTABLE_NEEDS_WRITING (ctype)))
	current_extern_inline = TREE_PUBLIC (decl1);

      raises = TYPE_RAISES_EXCEPTIONS (fntype);

      /* In a fcn definition, arg types must be complete.  */
      require_complete_types_for_parms (last_function_parms);
    }
  else
    {
      decl1 = grokdeclarator (declarator, declspecs, FUNCDEF, 1, raises);
      /* If the declarator is not suitable for a function definition,
	 cause a syntax error.  */
      if (decl1 == 0 || TREE_CODE (decl1) != FUNCTION_DECL) return 0;

      fntype = TREE_TYPE (decl1);

      restype = TREE_TYPE (fntype);
      if (IS_AGGR_TYPE (restype)
	  && ! CLASSTYPE_GOT_SEMICOLON (restype))
	{
	  error_with_aggr_type (restype, "semicolon missing after declaration of `%s'");
	  shadow_tag (build_tree_list (NULL_TREE, restype));
	  CLASSTYPE_GOT_SEMICOLON (restype) = 1;
	  if (TREE_CODE (fntype) == FUNCTION_TYPE)
	    fntype = build_function_type (integer_type_node,
					  TYPE_ARG_TYPES (fntype));
	  else
	    fntype = build_cplus_method_type (TYPE_METHOD_BASETYPE (fntype),
					      integer_type_node,
					      TYPE_ARG_TYPES (fntype));
	  TREE_TYPE (decl1) = fntype;
	}

      if (TREE_CODE (fntype) == METHOD_TYPE)
	ctype = TYPE_METHOD_BASETYPE (fntype);
      else if (IDENTIFIER_LENGTH (DECL_NAME (decl1)) == 4
	       && ! strcmp (IDENTIFIER_POINTER (DECL_NAME (decl1)), "main"))
	{
	  /* If this doesn't return an integer type, complain.  */
	  if (TREE_CODE (TREE_TYPE (fntype)) != INTEGER_TYPE)
	    {
#if 0
	      error ("return type for `main' must be integer type");
#else
	      warning ("return type for `main' changed to integer type");
#endif
	      TREE_TYPE (decl1) = fntype = default_function_type;
	    }
	  warn_about_return_type = 0;
	}
    }

  /* Warn if function was previously implicitly declared
     (but not if we warned then).  */
  if (! warn_implicit && IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1)) != 0)
    warning_with_decl (IDENTIFIER_IMPLICIT_DECL (DECL_NAME (decl1)),
		       "`%s' implicitly declared before its definition");

  current_function_decl = decl1;

  if (flag_cadillac)
    cadillac_start_function (decl1);
  else
    announce_function (decl1);

  if (TYPE_SIZE (TREE_TYPE (fntype)) == 0)
    {
      if (IS_AGGR_TYPE (TREE_TYPE (fntype)))
	error_with_aggr_type (TREE_TYPE (fntype),
			      "return-type `%s' is an incomplete type");
      else
	error ("return-type is an incomplete type");

      /* Make it return void instead.  */
      if (ctype)
	TREE_TYPE (decl1)
	  = build_cplus_method_type (ctype,
				     void_type_node,
				     TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl1))));
      else
	TREE_TYPE (decl1)
	  = build_function_type (void_type_node,
				 TYPE_ARG_TYPES (TREE_TYPE (decl1)));
    }

  if (warn_about_return_type)
    warning ("return-type defaults to `int'");

  /* Make the init_value nonzero so pushdecl knows this is not tentative.
     error_mark_node is replaced below (in poplevel) with the LET_STMT.  */
  DECL_INITIAL (decl1) = error_mark_node;

#ifdef NO_AUTO_OVERLOAD
  /* If this definition isn't a prototype and we had a prototype declaration
     before, copy the arg type info from that prototype.  */
  olddecl = lookup_name_current_level (DECL_NAME (decl1));
  if (olddecl != 0
      && TREE_CODE (olddecl) != TREE_LIST
      && TREE_TYPE (TREE_TYPE (decl1)) == TREE_TYPE (TREE_TYPE (olddecl))
      && TYPE_ARG_TYPES (TREE_TYPE (decl1)) == 0)
    {
      fntype = TREE_TYPE (olddecl);
      TREE_TYPE (decl1) = fntype;
    }
#endif

  /* Didn't get anything from C.  */
  olddecl = 0;

  /* This is a definition, not a reference.
     So normally clear TREE_EXTERNAL.
     However, `extern inline' acts like a declaration
     except for defining how to inline.  So set TREE_EXTERNAL in that case.  */
  TREE_EXTERNAL (decl1) = current_extern_inline;

  /* This function exists in static storage.
     (This does not mean `static' in the C sense!)  */
  TREE_STATIC (decl1) = 1;

  /* If this inline function belongs to the implementation, make it public.  */
  if (TREE_INLINE (decl1) && interface_unknown == 0)
    {
      TREE_PUBLIC (decl1) = ! interface_only;
      TREE_EXTERNAL (decl1) = interface_only;
    }

  /* Now see if this is the implementation of a function
     declared with "C" linkage.  */
  if (ctype == NULL_TREE && current_lang_name == lang_name_cplusplus)
    {
      olddecl = lookup_name_current_level (DECL_ORIGINAL_NAME (decl1));
      if (olddecl && TREE_CODE (olddecl) != FUNCTION_DECL)
	olddecl = NULL_TREE;
      if (olddecl
	  && DECL_ORIGINAL_NAME (decl1) != DECL_ORIGINAL_NAME (olddecl))
	{
	  /* Collision between user and internal naming scheme.  */
	  olddecl = lookup_name_current_level (DECL_NAME (decl1));
	  if (olddecl == NULL_TREE)
	    olddecl = decl1;
	}
      if (olddecl && olddecl != decl1
	  && DECL_ORIGINAL_NAME (decl1) == DECL_ORIGINAL_NAME (olddecl))
	{
	  if (TREE_CODE (olddecl) == FUNCTION_DECL
	      && (decls_match (decl1, olddecl)
		  || comp_target_parms (TYPE_ARG_TYPES (TREE_TYPE (decl1)),
					TYPE_ARG_TYPES (TREE_TYPE (olddecl)), 1)))
	    {
	      olddecl = DECL_MAIN_VARIANT (olddecl);
	      DECL_NAME (decl1) = DECL_NAME (olddecl);
	      DECL_PRINT_NAME (decl1) = DECL_PRINT_NAME (olddecl);
	      DECL_OVERLOADED (decl1) = DECL_OVERLOADED (olddecl);
	      if (DECL_INITIAL (olddecl))
		redeclaration_error_message (decl1, olddecl);
	      if (! duplicate_decls (decl1, olddecl))
		abort ();
	      decl1 = olddecl;
	    }
	  else
	    olddecl = NULL_TREE;
	}
    }

  /* Record the decl so that the function name is defined.
     If we already have a decl for this name, and it is a FUNCTION_DECL,
     use the old decl.  */

  if (olddecl)
    current_function_decl = olddecl;
  else if (pre_parsed_p == 0)
    {
      current_function_decl = pushdecl (decl1);
      if (TREE_CODE (current_function_decl) == TREE_LIST)
	{
	  /* @@ revert to modified original declaration.  */
	  decl1 = DECL_MAIN_VARIANT (decl1);
	  current_function_decl = decl1;
	}
      else
	{
	  decl1 = current_function_decl;
	  DECL_MAIN_VARIANT (decl1) = decl1;
	}
      fntype = TREE_TYPE (decl1);
    }
  else
    current_function_decl = decl1;

  if (DECL_OVERLOADED (decl1))
    push_overloaded_decl (decl1);

  if (ctype != 0 && DECL_STATIC_FUNCTION_P (decl1))
    {
      if (TREE_CODE (fntype) == METHOD_TYPE)
	TREE_TYPE (decl1) = fntype
	  = build_function_type (TREE_TYPE (fntype),
				 TREE_CHAIN (TYPE_ARG_TYPES (fntype)));
      last_function_parms = TREE_CHAIN (last_function_parms);
      DECL_ARGUMENTS (decl1) = last_function_parms;
      ctype = 0;
    }
  restype = TREE_TYPE (fntype);

  pushlevel (0);
  current_binding_level->parm_flag = 1;

  /* Save the parm names or decls from this function's declarator
     where store_parm_decls will find them.  */
  current_function_parms = last_function_parms;
  current_function_parm_tags = last_function_parm_tags;

#ifdef FIELD_XREF
  FIELD_xref_function(decl1,current_function_parms);
#endif

  make_function_rtl (decl1);

  if (ctype)
    {
      pushclass (ctype, 1);
      /* We know that this was set up by `grokclassfn'.
	 We do not wait until `store_parm_decls', since evil
	 parse errors may never get us to that point.  Here
	 we keep the consistency between `current_class_type'
	 and `current_class_decl'.  */
      current_class_decl = last_function_parms;
      assert (TREE_CODE (current_class_decl) == PARM_DECL);
      if (TREE_CODE (TREE_TYPE (current_class_decl)) == POINTER_TYPE)
	{
	  tree variant = TREE_TYPE (TREE_TYPE (current_class_decl));
	  if (CLASSTYPE_INST_VAR (ctype) == NULL_TREE)
	    {
	      /* Can't call build_indirect_ref here, because it has special
		 logic to return C_C_D given this argument.  */
	      C_C_D = build1 (INDIRECT_REF, current_class_type, current_class_decl);
	      CLASSTYPE_INST_VAR (ctype) = C_C_D;
	    }
	  else
	    {
	      C_C_D = CLASSTYPE_INST_VAR (ctype);
	      /* `current_class_decl' is different for every
		 function we compile.  */
	      TREE_OPERAND (C_C_D, 0) = current_class_decl;
	    }
	  TREE_READONLY (C_C_D) = TREE_READONLY (variant);
	  TREE_VOLATILE (C_C_D) = TREE_VOLATILE (variant);
	}
      else
	C_C_D = current_class_decl;
    }
  else
    {
      if (DECL_STATIC_FUNCTION_P (decl1))
	pushclass (DECL_STATIC_CONTEXT (decl1), 2);
      else
	push_memoized_context (0, 1);
    }

  /* Promote the value to int before returning it.  */
  if (TREE_CODE (restype) == INTEGER_TYPE
      && TYPE_PRECISION (restype) < TYPE_PRECISION (integer_type_node))
    restype = integer_type_node;
  DECL_RESULT_TYPE (decl1) = restype;
  DECL_RESULT (decl1) = build_decl (RESULT_DECL, value_identifier, restype);

  if (DESTRUCTOR_NAME_P (DECL_NAME (decl1)))
    {
      dtor_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);
      ctor_label = NULL_TREE;
    }
  else
    {
      dtor_label = NULL_TREE;
      if (DECL_CONSTRUCTOR_P (decl1))
	{
	  ctor_label = build_decl (LABEL_DECL, NULL_TREE, NULL_TREE);

	  /* Initializations from `emit_base_init' might go inline.
	     Protect the binding level of the parms.  */
	  pushlevel (0);
	}
    }

  /* Allocate further tree nodes temporarily during compilation
     of this function only.  */
  temporary_allocation ();

  /* If this fcn was already referenced via a block-scope `extern' decl
     (or an implicit decl), propagate certain information about the usage.  */
  if (TREE_ADDRESSABLE (DECL_NAME (decl1)))
    TREE_ADDRESSABLE (decl1) = 1;
      
  return 1;
}

/* Store the parameter declarations into the current function declaration.
   This is called after parsing the parameter declarations, before
   digesting the body of the function.

   Also install to binding contour return value identifier, if any.  */

void
store_parm_decls ()
{
  register tree fndecl = current_function_decl;
  register tree parm;
  int parms_have_cleanups = 0;
  tree eh_decl;

  /* This is either a chain of PARM_DECLs (when a prototype is used).  */
  tree specparms = current_function_parms;

  /* This is a list of types declared among parms in a prototype.  */
  tree parmtags = current_function_parm_tags;

  /* This is a chain of any other decls that came in among the parm
     declarations.  If a parm is declared with  enum {foo, bar} x;
     then CONST_DECLs for foo and bar are put here.  */
  tree nonparms = 0;

  if (current_binding_level == global_binding_level)
    fatal ("parse errors have confused me too much");

  /* Initialize RTL machinery.  */
  init_function_start (fndecl);

  /* Create a binding level for the parms.  */
  expand_start_bindings (0);

  /* Prepare to catch raises, if appropriate.  */
  if (flag_handle_exceptions)
    {
      /* Get this cleanup to be run last, since it
	 is a call to `longjmp'.  */
      setup_exception_throw_decl ();
      eh_decl = current_binding_level->names;
      current_binding_level->names = TREE_CHAIN (current_binding_level->names);
    }
  if (flag_handle_exceptions)
    expand_start_try (integer_one_node, 0, 1);

  if (specparms != 0)
    {
      /* This case is when the function was defined with an ANSI prototype.
	 The parms already have decls, so we need not do anything here
	 except record them as in effect
	 and complain if any redundant old-style parm decls were written.  */

      register tree next;

      for (parm = nreverse (specparms); parm; parm = next)
	{
	  tree cleanup = maybe_build_cleanup (parm);
	  next = TREE_CHAIN (parm);
	  if (DECL_NAME (parm) == 0)
	    {
#if 0
	      error_with_decl (parm, "parameter name omitted");
#else
	      /* for C++, this is not an error.  */
	      pushdecl (parm);
#endif
	    }
	  else if (TREE_TYPE (parm) == void_type_node)
	    error_with_decl (parm, "parameter `%s' declared void");
	  else
	    {
	      /* Now fill in DECL_REFERENCE_SLOT for any of the parm decls.
		 A parameter is assumed not to have any side effects.
		 If this should change for any reason, then this
		 will have to wrap the bashed reference type in a save_expr.
		 
		 Also, if the parameter type is declared to be an X
		 and there is an X(X&) constructor, we cannot lay it
		 into the stack (any more), so we make this parameter
		 look like it is really of reference type.  Functions
		 which pass parameters to this function will know to
		 create a temporary in their frame, and pass a reference
		 to that.  */

	      if (TREE_CODE (TREE_TYPE (parm)) == REFERENCE_TYPE
		  && TYPE_SIZE (TREE_TYPE (TREE_TYPE (parm))))
		SET_DECL_REFERENCE_SLOT (parm, convert_from_reference (parm));

	      pushdecl (parm);
	    }

	  if (cleanup)
	    {
	      expand_decl (parm);
	      expand_decl_cleanup (parm, cleanup);
	      parms_have_cleanups = 1;
	    }
	}

      /* Get the decls in their original chain order
	 and record in the function.  */
      DECL_ARGUMENTS (fndecl) = getdecls ();

      storetags (chainon (parmtags, gettags ()));
    }
  else
    DECL_ARGUMENTS (fndecl) = 0;

  /* Now store the final chain of decls for the arguments
     as the decl-chain of the current lexical scope.
     Put the enumerators in as well, at the front so that
     DECL_ARGUMENTS is not modified.  */

  storedecls (chainon (nonparms, DECL_ARGUMENTS (fndecl)));

  /* Initialize the RTL code for the function.  */
  DECL_SAVED_INSNS (fndecl) = 0;
  expand_function_start (fndecl, parms_have_cleanups);

  if (flag_handle_exceptions)
    {
      /* Make the throw decl visibile at this level, just
	 not in the way of the parameters.  */
      pushdecl (eh_decl);
      expand_decl_init (eh_decl);
    }

  /* Create a binding contour which can be used to catch
     cleanup-generated temporaries.  Also, if the return value needs or
     has initialization, deal with that now.  */
  pushlevel (0);
  expand_start_bindings (0);
  current_function_parms_stored = 1;

  /* If this function is `main', emit a call to `__main'
     to run global initializers, etc.  */
  if (IDENTIFIER_LENGTH (DECL_NAME (fndecl)) == 4
      && strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "main") == 0)
    expand_expr (build_function_call (lookup_name (get_identifier ("__main")), NULL_TREE),
		 0, VOIDmode, 0);
}

/* Bind a name and initialization to the return value of
   the current function.  */
void
store_return_init (init)
     tree init;
{
  tree decl = DECL_RESULT (current_function_decl);

  /* Can't let this happen for constructors.  */
  if (DECL_CONSTRUCTOR_P (current_function_decl))
    {
      error ("can't redefine default return value for constructors");
      return;
    }

  /* If we have a named return value, put that in our scope as well.  */
  if (DECL_NAME (decl) != value_identifier)
    {
      /* If this named return value comes in a register,
	 put it in a pseudo-register.  */
      if (TREE_REGDECL (decl))
	{
	  original_result_rtx = DECL_RTL (decl);
	  DECL_RTL (decl) = (struct rtx_def *)gen_reg_rtx (DECL_MODE (decl));
	}

      /* Let `finish_decl' know that this initializer is ok.  */
      DECL_INITIAL (decl) = init;
      pushdecl (decl);
      finish_decl (decl, init, 0);
    }
}

/* Generate code for default X(X&) constructor.  */
static void
build_default_constructor (fndecl)
     tree fndecl;
{
  int i = CLASSTYPE_N_BASECLASSES (current_class_type);
  tree parm = TREE_CHAIN (DECL_ARGUMENTS (fndecl));
  tree fields = TYPE_FIELDS (current_class_type);
  if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
    parm = TREE_CHAIN (parm);
  parm = DECL_REFERENCE_SLOT (parm);

  while (i > 0)
    {
      tree basetype = CLASSTYPE_BASECLASS (current_class_type, i);
      if (TYPE_GETS_INIT_REF (basetype))
	{
	  tree name = TYPE_NAME (basetype);
	  if (TREE_CODE (name) == TYPE_DECL)
	    name = DECL_NAME (name);
	  current_base_init_list = tree_cons (name, parm, current_base_init_list);
	}
      i -= 1;
    }
  for (fields = TYPE_FIELDS (current_class_type); fields;
       fields = TREE_CHAIN (fields))
    {
      tree name, init;
      if (TREE_STATIC (fields))
	continue;
      if (TREE_CODE (fields) != FIELD_DECL)
	continue;
      if (DECL_NAME (fields))
	{
	  if (VFIELD_NAME_P (DECL_NAME (fields)))
	    continue;
	  if (VBASE_NAME_P (DECL_NAME (fields)))
	    continue;

	  /* True for duplicate members.  */
	  if (IDENTIFIER_CLASS_VALUE (DECL_NAME (fields)) != fields)
	    continue;
	}

      init = build (COMPONENT_REF, TREE_TYPE (fields), parm, fields);

      if (TREE_ANON_UNION_ELEM (fields))
	name = build (COMPONENT_REF, TREE_TYPE (fields), C_C_D, fields);
      else
	{
	  name = DECL_NAME (fields);
	  init = build_tree_list (NULL_TREE, init);
	}

      current_member_init_list
	= tree_cons (name, init, current_member_init_list);
    }
}


/* Finish up a function declaration and compile that function
   all the way to assembler language output.  The free the storage
   for the function definition.

   This is called after parsing the body of the function definition.
   LINENO is the current line number.

   C++: CALL_POPLEVEL is non-zero if an extra call to poplevel
   (and expand_end_bindings) must be made to take care of the binding
   contour for the base initialazers.  This is only relevant for
   constructors.  */

void
finish_function (lineno, call_poplevel)
     int lineno;
     int call_poplevel;
{
  register tree fndecl = current_function_decl;
  tree fntype = TREE_TYPE (fndecl), ctype = NULL_TREE;
  struct rtx_def *head, *last_parm_insn, *mark;
  extern struct rtx_def *get_last_insn ();
  extern struct rtx_def *cleanup_label, *return_label;
  extern int sets_exception_throw_decl;

/*  TREE_READONLY (fndecl) = 1;
    This caused &foo to be of type ptr-to-const-function
    which then got a warning when stored in a ptr-to-function variable.  */

  /* This happens on strange parse errors.  */
  if (! current_function_parms_stored)
    {
      call_poplevel = 0;
      store_parm_decls ();
    }

  /* Clean house because we will need to reorder insns here.  */
  do_pending_stack_adjust ();

  if (dtor_label)
    {
      tree cond = integer_one_node;
      tree exprstmt, vfields;
      tree in_charge_node = lookup_name (in_charge_identifier);
      int ok_to_optimize_dtor = 0;

      if (current_function_assigns_this)
	cond = build (NE_EXPR, integer_type_node,
		      current_class_decl, integer_zero_node);
      else
	{
	  int n_baseclasses = CLASSTYPE_N_BASECLASSES (current_class_type);

	  /* If this destructor is empty, then we don't need to check
	     whether `this' is NULL in some cases.  */
	  mark = get_last_insn ();
	  last_parm_insn = (struct rtx_def *)get_first_nonparm_insn ();

	  if ((flag_this_is_variable & 1) == 0)
	    ok_to_optimize_dtor = 1;
	  else if (mark == last_parm_insn)
	    ok_to_optimize_dtor
	      = (n_baseclasses == 0
		 || (n_baseclasses == 1
		     && TYPE_HAS_DESTRUCTOR (CLASSTYPE_BASECLASS (current_class_type, 1))));
	}

      /* These initializations might go inline.  Protect
	 the binding level of the parms.  */
      pushlevel (0);

      if (current_function_assigns_this)
	{
	  TYPE_ANY_ASSIGNS_THIS (current_class_type) = 1;
	  current_function_assigns_this = 0;
	  current_function_just_assigned_this = 0;
	}

      /* Generate the code to call destructor on base class.
	 If this destructor belongs to a class with virtual
	 functions, then set the virtual function table
	 pointer to represent the type of our base class.  */

      /* This side-effect makes call to `build_delete' generate the
	 code we have to have at the end of this destructor.  */
      TYPE_HAS_DESTRUCTOR (current_class_type) = 0;

      /* These are two cases where we cannot delegate deletion.  */
      if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type)
	  || TREE_GETS_DELETE (current_class_type))
	exprstmt = build_delete (current_class_type, C_C_D, integer_zero_node,
				 LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);
      else
	exprstmt = build_delete (current_class_type, C_C_D, in_charge_node,
				 LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);

      /* If we did not assign to this, then `this' is non-zero at
	 the end of a destructor.  As a special optimization, don't
	 emit test if this is an empty destructor.  If it does nothing,
	 it does nothing.  If it calls a base destructor, the base
	 destructor will perform the test.  */

      if (exprstmt != error_mark_node
	  && (TREE_CODE (exprstmt) != NOP_EXPR
	      || TREE_OPERAND (exprstmt, 0) != integer_zero_node
	      || TYPE_USES_VIRTUAL_BASECLASSES (current_class_type)))
	{
	  expand_label (dtor_label);
	  if (cond != integer_one_node)
	    expand_start_cond (cond, 0);
	  expand_expr_stmt (exprstmt);

	  /* Run destructor on all virtual baseclasses.  */
	  if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
	    {
	      tree vbases = nreverse (copy_list (CLASSTYPE_VBASECLASSES (current_class_type)));
	      expand_start_cond (build (BIT_AND_EXPR, integer_type_node,
					in_charge_node, integer_two_node), 0);
	      while (vbases)
		{
		  if (TYPE_NEEDS_DESTRUCTOR (TREE_VALUE (vbases)))
		    {
		      tree ptr = convert_pointer_to_vbase (TREE_TYPE (vbases), current_class_decl);
		      expand_expr_stmt (build_delete (TYPE_POINTER_TO (TREE_VALUE (vbases)),
						      ptr, integer_zero_node,
						      LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR|LOOKUP_HAS_IN_CHARGE, 0));
		    }
		  vbases = TREE_CHAIN (vbases);
		}
	      expand_end_cond ();
	    }

	  do_pending_stack_adjust ();
	  if (cond != integer_one_node)
	    expand_end_cond ();
	}

      TYPE_HAS_DESTRUCTOR (current_class_type) = 1;

      /* At the end, call delete if that's what's requested.  */
      if (TREE_GETS_DELETE (current_class_type))
	exprstmt = build_method_call (build1 (NOP_EXPR, TYPE_POINTER_TO (current_class_type), error_mark_node),
				      get_identifier (OPERATOR_DELETE_FORMAT),
				      build_tree_list (NULL_TREE, integer_zero_node),
				      NULL_TREE, LOOKUP_NORMAL);
      else if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
	exprstmt = build_x_delete (ptr_type_node, current_class_decl, 0);
      else
	exprstmt = 0;

      if (exprstmt)
	{
	  cond = build (BIT_AND_EXPR, integer_type_node,
			in_charge_node, integer_one_node);
	  expand_start_cond (cond, 0);
	  expand_expr_stmt (exprstmt);
	  expand_end_cond ();
	}

      /* End of destructor.  */
      poplevel (2, 0, 0);

      /* Back to the top of destructor.  */
      /* Dont execute destructor code if `this' is NULL.  */
      mark = get_last_insn ();
      last_parm_insn = (struct rtx_def *)get_first_nonparm_insn ();
      if (last_parm_insn == 0) last_parm_insn = mark;
      else last_parm_insn = (struct rtx_def *) previous_insn (last_parm_insn);

      /* Make all virtual function table pointers point to CURRENT_CLASS_TYPE's
	 virtual function tables.  */
      if (CLASSTYPE_VFIELDS (current_class_type))
	{
	  for (vfields = CLASSTYPE_VFIELDS (current_class_type);
	       TREE_CHAIN (vfields);
	       vfields = TREE_CHAIN (vfields))
	    expand_expr_stmt (build_virtual_init (current_class_type,
						  TREE_VALUE (vfields),
						  current_class_decl));
	  expand_expr_stmt (build_virtual_init (current_class_type,
						current_class_type,
						current_class_decl));
	}
      if (TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
	expand_expr_stmt (build_vbase_vtables_init (current_class_type,
						    current_class_type,
						    C_C_D, current_class_decl, 0));
#ifdef sparc
      expand_asm_operands (build_string (32, "! end of vtable initialization"), 0, 0, 0, 0, input_filename, lineno);
#endif

      if (! ok_to_optimize_dtor)
	{
	  cond = build_binary_op (NE_EXPR, current_class_decl, integer_zero_node);
	  expand_start_cond (cond, 0);
	}
      if (mark != get_last_insn ())
	reorder_insns (next_insn (mark), get_last_insn (), last_parm_insn);
      if (! ok_to_optimize_dtor)
	  expand_end_cond ();
    }
  else if (current_function_assigns_this)
    {
      /* Does not need to call emit_base_init, because
	 that is done (if needed) just after assignment to this
	 is seen.  */

      TYPE_ANY_ASSIGNS_THIS (current_class_type) = 1;

      if (DECL_CONSTRUCTOR_P (current_function_decl))
	{
	  /* Undo call to pushlevel from `start_function'.  */
	  poplevel (2, 0, 0);

	  expand_label (ctor_label);
	  ctor_label = NULL_TREE;

	  if (call_poplevel)
	    {
	      tree decls = getdecls ();
	      if (flag_handle_exceptions == 2)
		deactivate_exception_cleanups ();
	      expand_end_bindings (decls, decls != 0, 0);
	      poplevel (decls != 0, 0, 0);
	    }

	  c_expand_return (current_class_decl);
	}

      current_function_assigns_this = 0;
      current_function_just_assigned_this = 0;
      base_init_insns = 0;
    }
  else if (DECL_CONSTRUCTOR_P (fndecl))
    {
      tree allocated_this;
      tree cond, thenclause;
      /* Allow constructor for a type to get a new instance of the object
	 using `build_new'.  */
      tree abstract_virtuals = CLASSTYPE_ABSTRACT_VIRTUALS (current_class_type);
      CLASSTYPE_ABSTRACT_VIRTUALS (current_class_type) = NULL_TREE;

      DECL_RETURNS_FIRST_ARG (fndecl) = 1;

      if (flag_this_is_variable)
	{
	  cond = build_binary_op (EQ_EXPR, current_class_decl, integer_zero_node);
	  thenclause = build_modify_expr (current_class_decl, NOP_EXPR,
					  build_new (NULL_TREE, current_class_type, void_type_node, 0));
	  if (flag_handle_exceptions == 2)
	    {
	      tree cleanup, cleanup_deallocate;

	      allocated_this = build_decl (VAR_DECL, NULL_TREE, ptr_type_node);
	      TREE_REGDECL (allocated_this) = 1;
	      DECL_INITIAL (allocated_this) = error_mark_node;
	      expand_decl (allocated_this);
	      expand_decl_init (allocated_this);
	      /* How we cleanup `this' if an exception was raised before
		 we are ready to bail out.  */
	      cleanup = TREE_GETS_DELETE (current_class_type)
		? build_opfncall (DELETE_EXPR, LOOKUP_NORMAL, allocated_this)
		  : build_delete (TREE_TYPE (allocated_this), allocated_this, integer_three_node, LOOKUP_NORMAL|LOOKUP_HAS_IN_CHARGE, 1);
	      cleanup_deallocate
		= build_modify_expr (current_class_decl, NOP_EXPR, integer_zero_node);
	      cleanup = tree_cons (NULL_TREE, cleanup,
				   build_tree_list (NULL_TREE, cleanup_deallocate));

	      expand_decl_cleanup (allocated_this,
				   build (COND_EXPR, integer_type_node,
					  build (NE_EXPR, integer_type_node,
						 allocated_this, integer_zero_node),
					  build_compound_expr (cleanup),
					  integer_zero_node));
	    }
	}
      else if (TREE_GETS_NEW (current_class_type))
	/* Just check visibility here.  */
	build_method_call (build1 (NOP_EXPR, TYPE_POINTER_TO (current_class_type), error_mark_node),
			   get_identifier (OPERATOR_NEW_FORMAT),
			   build_tree_list (NULL_TREE, integer_zero_node),
			   NULL_TREE, LOOKUP_NORMAL);

      CLASSTYPE_ABSTRACT_VIRTUALS (current_class_type) = abstract_virtuals;

      /* must keep the first insn safe.  */
      head = (struct rtx_def *)get_insns ();

      /* this note will come up to the top with us.  */
      mark = get_last_insn ();

      if (flag_this_is_variable)
	{
	  expand_start_cond (cond, 0);
	  expand_expr_stmt (thenclause);
	  if (flag_handle_exceptions == 2)
	    expand_assignment (allocated_this, current_class_decl, 0, 0);
	  expand_end_cond ();
	}

      if (DECL_COMPILER_GENERATED_P (fndecl)
	  && TREE_CHAIN (DECL_ARGUMENTS (fndecl)) != NULL_TREE)
	build_default_constructor (fndecl);

      /* Emit insns from `emit_base_init' which sets up virtual
	 function table pointer(s).  */
      emit_insns (base_init_insns);
      base_init_insns = 0;

      /* This is where the body of the constructor begins.
	 If there were no insns in this function body, then the
	 last_parm_insn is also the last insn.

	 If optimization is enabled, last_parm_insn may move, so
	 we don't hold on to it (across emit_base_init).  */
      last_parm_insn = (struct rtx_def *)get_first_nonparm_insn ();
      if (last_parm_insn == 0) last_parm_insn = mark;
      else last_parm_insn = (struct rtx_def *) previous_insn (last_parm_insn);

      if (mark != get_last_insn ())
	reorder_insns (next_insn (mark), get_last_insn (), last_parm_insn);

      /* This is where the body of the constructor ends.  */
      expand_label (ctor_label);
      ctor_label = NULL_TREE;
      if (flag_handle_exceptions == 2)
	{
	  expand_assignment (allocated_this, integer_zero_node, 0, 0);
	  deactivate_exception_cleanups ();
	}

      pop_implicit_try_blocks (NULL_TREE);

      /* Undo call to pushlevel from `start_function'.  */
      poplevel (2, 0, 0);

      if (call_poplevel)
	{
	  expand_end_bindings (getdecls (), 1, 0);
	  poplevel (1, 1, 0);
	}

      if (any_pending_cleanups ())
	/* Do things the hard way.  */
	c_expand_return (current_class_decl);
      else
	{
	  /* Just store CURRENT_CLASS_DECL in the
	     DECL_RESULT of our current function decl
	     and fall through to end.  */
	  struct rtx_def *val = DECL_RTL (DECL_RESULT (fndecl));
	  store_expr (current_class_decl, val, 0);
	  emit_queue ();
	  use_variable (val);
	}

      current_function_assigns_this = 0;
      current_function_just_assigned_this = 0;
    }
  else if (IDENTIFIER_LENGTH (DECL_NAME (fndecl)) == 4
	   && ! strcmp (IDENTIFIER_POINTER (DECL_NAME (fndecl)), "main"))
    {
      /* Make it so that `main' always returns 0 by default.  */
#ifdef VMS
      c_expand_return (integer_one_node);
#else
      c_expand_return (integer_zero_node);
#endif
    }

  /* That's the end of the vtable decl's life.  Need to mark it such
     if doing stupid register allocation.

     Note that current_vtable_decl is really an INDIRECT_REF
     on top of a VAR_DECL here.  */
  if (obey_regdecls && current_vtable_decl)
    use_variable (DECL_RTL (TREE_OPERAND (current_vtable_decl, 0)));

  /* remove the binding contour which is used
     to catch cleanup-generated temporaries.  */
  expand_end_bindings (0, 0, 0);
  poplevel (0, 0, 0);

  /* Must mark the RESULT_DECL as being in this function.  */

  DECL_CONTEXT (DECL_RESULT (fndecl)) = DECL_INITIAL (fndecl);

  /* Obey `register' declarations if `setjmp' is called in this fn.  */
  if (flag_traditional && current_function_calls_setjmp)
    setjmp_protect (DECL_INITIAL (fndecl));

  /* Generate rtl for function exit.  */
  head = get_last_insn ();
  expand_function_end (input_filename, lineno);

  if (cleanup_label)
    {
      mark = get_last_insn ();
      /* Emit label at beginning of cleanup code for parmeters.  */
      emit_label (cleanup_label);
    }

#if 1
  /* Cheap hack to get better code from GNU C++.  Remove when cse is fixed.  */
  if (exception_throw_decl && sets_exception_throw_decl == 0)
    expand_assignment (exception_throw_decl, integer_zero_node, 0, 0);
#endif

  if (flag_handle_exceptions)
    {
      expand_end_try ();
      expand_start_except (0, 0);
      expand_end_except ();
    }
  expand_end_bindings (0, 0, 0);

  /* Get return value into reigster if that's where it's supposed to be.  */
  if (original_result_rtx)
    fixup_result_decl (DECL_RESULT (fndecl), original_result_rtx);

  /* reset scope for C++: if we were in the scope of a class,
     then when we finish this function, we are not longer so.
     This cannot be done until we know for sure that no more
     class members will ever be referenced in this function
     (i.e., calls to destructors).  */
  if (current_class_name)
    {
      ctype = current_class_type;
      popclass (1);
    }
  else
    pop_memoized_context (1);

  /* Forget about all overloaded functions defined in
     this scope which go away.  */
  while (overloads_to_forget)
    {
      IDENTIFIER_GLOBAL_VALUE (TREE_PURPOSE (overloads_to_forget))
	= TREE_VALUE (overloads_to_forget);
      overloads_to_forget = TREE_CHAIN (overloads_to_forget);
    }

  poplevel (1, 0, 1);

  if (cleanup_label)
    {
      /* To keep us from getting the compiler confused about
	 what constitutes control dropping off the end,
	 send control to RETURN_LABEL.  All this really
	 avoids is the NOTE_INSN_FUNCTION_END note.  */
      if (return_label)
	emit_jump_insn (gen_jump (return_label));
#ifdef HAVE_return
      else
	emit_jump_insn (gen_return ());
#endif

      emit_barrier ();

      reorder_insns (next_insn (mark), get_last_insn (), head);
    }

  /* So we can tell if jump_optimize sets it to 1.  */
  current_function_returns_null = 0;

  if (TREE_EXTERNAL (fndecl) && ! TREE_PUBLIC (fndecl)
      /* This function is just along for the ride.  If we can make
	 it inline, that's great.  Otherwise, just punt it.  */
      && (TREE_INLINE (fndecl) == 0
	  || function_cannot_inline_p (fndecl)))
    {
      extern int rtl_dump_and_exit;
      int old_rtl_dump_and_exit = rtl_dump_and_exit;

      /* This throws away the code for FNDECL.  */
      rtl_dump_and_exit = 1;
      rest_of_compilation (fndecl);
      rtl_dump_and_exit = old_rtl_dump_and_exit;
    }
  else
    /* Run the optimizers and output the assembler code for this function.  */
    rest_of_compilation (fndecl);

  if (ctype && TREE_ASM_WRITTEN (fndecl))
    CLASSTYPE_ASM_WRITTEN (ctype) = 1;

  /* Since we don't normally go through c_expand_return for constructors,
     this normally gets the wrong value.
     Also, named return values have their return codes emitted after
     NOTE_INSN_FUNCTION_END, confusing jump.c.  */
  if (DECL_CONSTRUCTOR_P (fndecl)
      || DECL_NAME (DECL_RESULT (fndecl)) != value_identifier)
    current_function_returns_null = 0;

  if (TREE_THIS_VOLATILE (fndecl) && current_function_returns_null)
    warning ("`volatile' function does return");
  else if (warn_return_type && current_function_returns_null
	   && TREE_TYPE (fntype) != void_type_node)
    /* If this function returns non-void and control can drop through,
       complain.  */
    warning ("control reaches end of non-void function");
  /* With just -W, complain only if function returns both with
     and without a value.  */
  else if (extra_warnings
	   && current_function_returns_value && current_function_returns_null)
    warning ("this function may return with or without a value");

  /* Free all the tree nodes making up this function.  */
  /* Switch back to allocating nodes permanently
     until we start another function.  */
  permanent_allocation ();

  if (flag_cadillac)
    cadillac_finish_function (fndecl);

  if (DECL_SAVED_INSNS (fndecl) == 0)
    {
      /* Stop pointing to the local nodes about to be freed.  */
      /* But DECL_INITIAL must remain nonzero so we know this
	 was an actual function definition.  */
      DECL_INITIAL (fndecl) = error_mark_node;
      if (! DECL_CONSTRUCTOR_P (fndecl)
	  || !TYPE_USES_VIRTUAL_BASECLASSES (TYPE_METHOD_BASETYPE (fntype)))
	DECL_ARGUMENTS (fndecl) = 0;
    }

  /* Let the error reporting routines know that we're outside a function.  */
  current_function_decl = NULL_TREE;
  named_label_uses = NULL_TREE;
  clear_anon_parm_name ();
}

/* Create the FUNCTION_DECL for a function definition.
   LINE1 is the line number that the definition absolutely begins on.
   LINE2 is the line number that the name of the function appears on.
   DECLSPECS and DECLARATOR are the parts of the declaration;
   they describe the function's name and the type it returns,
   but twisted together in a fashion that parallels the syntax of C.

   This function creates a binding context for the function body
   as well as setting up the FUNCTION_DECL in current_function_decl.

   Returns a FUNCTION_DECL on success.

   If the DECLARATOR is not suitable for a function (it defines a datum
   instead), we return 0, which tells yyparse to report a parse error.

   May return void_type_node indicating that this method is actually
   a friend.  See grokfield for more details.

   Came here with a `.pushlevel' .

   DO NOT MAKE ANY CHANGES TO THIS CODE WITHOUT MAKING CORRESPONDING
   CHANGES TO CODE IN `grokfield'.  */
tree
start_method (declspecs, declarator, raises)
     tree declarator, declspecs, raises;
{
  tree fndecl = grokdeclarator (declarator, declspecs, MEMFUNCDEF, 0, raises);

  /* Something too ugly to handle.  */
  if (fndecl == 0)
    return 0;

  /* Pass friends other than inline friend functions back.  */
  if (fndecl == void_type_node)
    return void_type_node;

  if (TREE_CODE (fndecl) != FUNCTION_DECL)
    /* Not a function, tell parser to report parse error.  */
    return 0;

  if (DECL_IN_AGGR_P (fndecl))
    {
      if (IDENTIFIER_ERROR_LOCUS (DECL_NAME (fndecl)) != current_class_type)
	error_with_decl (fndecl, "`%s' is already defined in aggregate scope");
      return void_type_node;
    }

  if (flag_default_inline)
    TREE_INLINE (fndecl) = 1;

  /* We read in the parameters on the maybepermanent_obstack,
     but we won't be getting back to them until after we
     may have clobbered them.  So the call to preserve_data
     will keep them safe.  */
  preserve_data ();

  if (! DECL_FRIEND_P (fndecl))
    {
      if (TREE_CHAIN (fndecl) != NULL_TREE)
	/* Need a fresh node here so that we don't get circularity
	   when we link these together.  If FNDECL was a friend, then
	   `pushdecl' does the right thing, which is nothing wrt its
	   current value of TREE_CHAIN.  */
	fndecl = copy_node (fndecl);

      if (DECL_CONSTRUCTOR_P (fndecl))
	grok_ctor_properties (current_class_type, fndecl);
      else if (OPERATOR_NAME_P (DECL_NAME (fndecl)))
	{
	  TREE_OPERATOR (fndecl) = 1;
	  grok_op_properties (fndecl);
	}
    }

  finish_decl (fndecl, NULL, NULL);

  /* Make a place for the parms */
  pushlevel (0);
  current_binding_level->parm_flag = 1;
  
  DECL_IN_AGGR_P (fndecl) = 1;
  return fndecl;
}

/* Go through the motions of finishing a function definition.
   We don't compile this method until after the whole class has
   been processed.

   FINISH_METHOD must return something that looks as though it
   came from GROKFIELD (since we are defining a method, after all).

   This is called after parsing the body of the function definition.
   STMTS is the chain of statements that makes up the function body.

   DECL is the ..._DECL that `start_method' provided.  */

tree
finish_method (decl)
     tree decl;
{
  register tree fndecl = decl;
  tree old_initial;

  register tree link;

  if (decl == void_type_node)
    return decl;

  old_initial = DECL_INITIAL (fndecl);

  /* Undo the level for the parms (from start_method).
     This is like poplevel, but it causes nothing to be
     saved.  Saving information here confuses symbol-table
     output routines.  Besides, this information will
     be correctly output when this method is actually
     compiled.  */

  /* Clear out the meanings of the local variables of this level;
     also record in each decl which block it belongs to.  */

  for (link = current_binding_level->names; link; link = TREE_CHAIN (link))
    {
      if (DECL_NAME (link) != 0)
	IDENTIFIER_LOCAL_VALUE (DECL_NAME (link)) = 0;
      DECL_CONTEXT (link) = 0;
    }

  /* Restore all name-meanings of the outer levels
     that were shadowed by this level.  */

  for (link = current_binding_level->shadowed; link; link = TREE_CHAIN (link))
      IDENTIFIER_LOCAL_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);
  for (link = current_binding_level->class_shadowed;
       link;
       link = TREE_CHAIN (link))
    IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (link)) = TREE_VALUE (link);

#ifdef FIELD_XREF
  FIELD_xref_end_scope(current_binding_level,
		       current_binding_level->level_chain,
		       current_binding_level->parm_flag,
		       current_binding_level->keep,
		       current_binding_level->tag_transparent);
#endif

  POP_BINDING_LEVEL;

  DECL_INITIAL (fndecl) = old_initial;
  if (DECL_FRIEND_P (fndecl))
    {
      if (current_lang_name == lang_name_cplusplus)
	CLASSTYPE_INLINE_FRIENDS (current_class_type)
	  = tree_cons (NULL_TREE, fndecl, CLASSTYPE_INLINE_FRIENDS (current_class_type));
      return void_type_node;
    }
  return decl;
}

/* Called when a new struct TYPE is defined.
   If this structure or union completes the type of any previous
   variable declaration, lay it out and output its rtl.  */

void
hack_incomplete_structures (type)
     tree type;
{
  tree decl;

  if (current_binding_level->n_incomplete == 0)
    return;

  for (decl = current_binding_level->names; decl; decl = TREE_CHAIN (decl))
    if (TREE_TYPE (decl) == type
	|| (TREE_CODE (TREE_TYPE (decl)) == ARRAY_TYPE
	    && TREE_TYPE (TREE_TYPE (decl)) == type))
      {
	if (TREE_CODE (decl) == TYPE_DECL)
	  layout_type (TREE_TYPE (decl));
	else
	  {
	    int toplevel = global_binding_level == current_binding_level;
	    layout_decl (decl, 0);
	    rest_of_decl_compilation (decl, 0, toplevel, 0);
	    if (! toplevel)
	      {
		expand_decl (decl);
		expand_decl_cleanup (decl, maybe_build_cleanup (decl));
		expand_decl_init (decl);
	      }
	  }
	--current_binding_level->n_incomplete;
	assert (current_binding_level->n_incomplete >= 0);
      }
}

/* Nonzero if presently building a cleanup.  Needed because
   SAVE_EXPRs are not the right things to use inside of cleanups.
   They are only ever evaluated once, where the cleanup
   might be evaluated several times.  In this case, a later evaluation
   of the cleanup might fill in the SAVE_EXPR_RTL, and it will
   not be valid for an earlier cleanup.  */

int building_cleanup;

/* If DECL is of a type which needs a cleanup, build that cleanup here.
   We don't build cleanups if just going for syntax checking, since
   fixup_cleanups does not know how to not handle them.

   Don't build these on the momentary obstack; they must live
   the life of the binding contour.  */
tree
maybe_build_cleanup (decl)
     tree decl;
{
  tree type = TREE_TYPE (decl);
  if (TYPE_NEEDS_DESTRUCTOR (type))
    {
      int temp;
      tree rval;
      int old_building_cleanup = building_cleanup;
      building_cleanup = 1;
      if (TREE_CODE (decl) == PARM_DECL)
	{
	  temp = allocation_temporary_p ();
	  end_temporary_allocation ();
	}
      else
	temp = suspend_momentary ();

      if (TREE_CODE (type) == ARRAY_TYPE)
	rval = decl;
      else
	{
	  mark_addressable (decl);
	  rval = build1 (ADDR_EXPR, TYPE_POINTER_TO (type), decl);
	}
      rval = build_delete (TREE_TYPE (rval), rval, integer_two_node,
			   LOOKUP_NORMAL, 0);

      if (TYPE_LANG_SPECIFIC (type)
	  && ! TYPE_HAS_DESTRUCTOR (type)
	  && TYPE_USES_VIRTUAL_BASECLASSES (type))
	rval = build_compound_expr (tree_cons (NULL_TREE, rval,
					       build_tree_list (NULL_TREE, build_vbase_delete (type, decl))));

      current_binding_level->have_cleanups = 1;
      current_binding_level->more_exceptions_ok = 0;

      if (TREE_CODE (decl) == PARM_DECL)
	{
	  if (temp)
	    resume_temporary_allocation ();
	}
      else
	resume_momentary (temp);

      building_cleanup = old_building_cleanup;

      return rval;
    }
  return 0;
}

tree
cleanup_after_call (expr)
     tree expr;
{
  tree type = TREE_TYPE (expr);
  tree decl = get_temp_name (type, 0);
  tree rval = build (WITH_CLEANUP_EXPR, type,
		     build (INIT_EXPR, type, decl, expr), 0,
		     maybe_build_cleanup (decl));
  return rval;
}

/* Expand a C++ expression at the statement level.
   This is needed to ferret out nodes which have UNKNOWN_TYPE.
   The C++ type checker should get all of these out when
   expressions are combined with other, type-providing, expressions,
   leaving only orphan expressions, such as:

   &class::bar;		/ / takes its address, but do nothing with it.

   */
void
cplus_expand_expr_stmt (exp)
     tree exp;
{
  if (TREE_TYPE (exp) == unknown_type_node)
    {
      if (TREE_CODE (exp) == ADDR_EXPR)
	{
	  if (TREE_CODE (TREE_OPERAND (exp, 0)) == OP_IDENTIFIER)
	    error ("unresolved reference to user-defined operator");
	  else
	    error ("address of overloaded function with no contextual type information");
	}
      else if (TREE_CODE (exp) == TREE_LIST)
	error ("address of overloaded function with no contextual type information");
      else if (TREE_CODE (exp) == OP_IDENTIFIER)
	error ("unresolved reference to user-defined operator");
      else if (TREE_CODE (exp) == COMPONENT_REF)
	warning ("useless reference to a member function name, did you forget the ()?");
    }
  else
    {
      int remove_implicit_immediately = 0;

      if (TREE_CODE (exp) == CALL_EXPR
	  && TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (exp)))
	exp = cleanup_after_call (exp);
      else if (TREE_CODE (exp) == FUNCTION_DECL)
	warning_with_decl (exp, "reference, not call, to function `%s'");
      if (TREE_RAISES (exp))
	{
	  assert (flag_handle_exceptions);
	  if (flag_handle_exceptions == 2)
	    {
	      if (! current_binding_level->more_exceptions_ok)
		{
		  extern struct nesting *nesting_stack, *block_stack;

		  remove_implicit_immediately
		    = (nesting_stack != block_stack);
		  cplus_expand_start_try (1);
		}
	      current_binding_level->have_exceptions = 1;
	    }
	}
      expand_expr_stmt (exp);
      if (remove_implicit_immediately)
	pop_implicit_try_blocks (NULL_TREE);
    }

  /* Clean up any pending cleanups.  This happens when a function call
     returns a cleanup-needing value that nobody uses.  */
  expand_cleanups_to (NULL_TREE);
}

/* When a stmt has been parsed, this function is called.

   Currently, this function only does something within a
   constructor's scope: if a stmt has just assigned to this,
   and we are in a derived class, we call `emit_base_init'.  */

void
finish_stmt ()
{
  extern struct nesting *cond_stack, *loop_stack, *case_stack;

  
  if (current_function_assigns_this
      || ! current_function_just_assigned_this)
    return;
  if (DECL_CONSTRUCTOR_P (current_function_decl))
    {
      /* Constructors must wait until we are out of control
	 zones before calling base constructors.  */
      if (cond_stack || loop_stack || case_stack)
	return;
      emit_insns (base_init_insns);
      check_base_init (current_class_type);
    }
  current_function_assigns_this = 1;

  if (flag_cadillac)
    cadillac_finish_stmt ();
}

void
pop_implicit_try_blocks (decl)
     tree decl;
{
  if (decl)
    {
      assert (current_binding_level->parm_flag == 3);
      current_binding_level->names = TREE_CHAIN (decl);
    }

  while (current_binding_level->parm_flag == 3)
    {
      tree name = get_identifier ("(compiler error)");
      tree orig_ex_type = current_exception_type;
      tree orig_ex_decl = current_exception_decl;
      tree orig_ex_obj = current_exception_object;
      tree decl = cplus_expand_end_try (2), decls;
      tree current_exception_ptr;

      /* @@ It would be nice to make all these point
	 to exactly the same handler.  */
      /* Start hidden EXCEPT.  */
      cplus_expand_start_except (name, decl);
      /* reraise ALL.  */
      cplus_expand_reraise (NULL_TREE);
      current_exception_type = orig_ex_type;
      current_exception_decl = orig_ex_decl;
      current_exception_object = orig_ex_obj;
      /* This will reraise for us.  */
      cplus_expand_end_except (error_mark_node);
    }

  if (decl)
    {
      TREE_CHAIN (decl) = current_binding_level->names;
      current_binding_level->names = decl;
    }
}

#ifdef FIELD_XREF
static void
FIELD_end_scope(lvl)
   struct binding_level * lvl;
{
   FIELD_xref_end_scope(lvl,
			lvl->level_chain,
			lvl->parm_flag,
			lvl->keep,
			lvl->tag_transparent);
};

#endif

/* Push a cleanup onto the current binding contour that will cause
   ADDR to be cleaned up, in the case that an exception propagates
   through its binding contour.  */

void
push_exception_cleanup (addr)
     tree addr;
{
  tree decl = build_decl (VAR_DECL, get_identifier (EXCEPTION_CLEANUP_NAME), ptr_type_node);
  tree cleanup;

  decl = pushdecl (decl);
  TREE_REGDECL (decl) = 1;
  store_init_value (decl, addr);
  expand_decl (decl);
  expand_decl_init (decl);

  cleanup = build (COND_EXPR, integer_type_node,
		   build (NE_EXPR, integer_type_node,
			  decl, integer_zero_node),
		   build_delete (TREE_TYPE (addr), decl,
				 lookup_name (in_charge_identifier),
				 LOOKUP_NORMAL, 0),
		   integer_zero_node);
  expand_decl_cleanup (decl, cleanup);
}

/* For each binding contour, emit code that deactivates the
   exception cleanups.  All other cleanups are left as they were.  */

static void
deactivate_exception_cleanups ()
{
  struct binding_level *b = current_binding_level;
  tree xyzzy = get_identifier (EXCEPTION_CLEANUP_NAME);
  while (b != class_binding_level)
    {
      if (b->parm_flag == 3)
	{
	  tree decls = b->names;
	  while (decls)
	    {
	      if (DECL_NAME (decls) == xyzzy)
		expand_assignment (decls, integer_zero_node, 0, 0);
	      decls = TREE_CHAIN (decls);
	    }
	}
      b = b->level_chain;
    }
}
