/* Breadth-first and depth-first routines for
   searching multiple-inheritance lattice for GNU C++.
   Copyright (C) 1987 Free Software Foundation, Inc.
   Contributed by Michael Tiemann (tiemann@mcc.com)

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


/* High-level class interface. */

#include "config.h"
#include "tree.h"
#include "cplus-tree.h"
#include "obstack.h"
#include "flags.h"
#include "assert.h"
#include <stdio.h>

/* For expand_asm_operands.  */
extern char *input_filename;
extern int lineno;

#define NULL 0

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int xmalloc ();
extern void free ();

void init_search ();
extern struct obstack *current_obstack;

#include "stack.h"

/* Obstack used for remembering decision points of breadth-first.  */
static struct obstack search_obstack;

/* Obstack used to bridge from one function context to another.  */
static struct obstack bridge_obstack;

/* Methods for pushing and popping objects to and from obstacks.  */

struct stack_level *
push_stack_level (obstack, tp, size)
     struct obstack *obstack;
     void *tp;
     int size;
{
  struct stack_level *stack;
  stack = (struct stack_level *) obstack_next_free (obstack);
  obstack_grow (obstack, tp, size);
  obstack_finish (obstack);
  stack->obstack = obstack;
  stack->first = (tree *) obstack_base (obstack);
  stack->limit = obstack_room (obstack) / sizeof (tree *);
  return stack;
}

struct stack_level *
pop_stack_level (stack)
     struct stack_level *stack;
{
  struct stack_level *tem = stack;
  struct obstack *obstack = tem->obstack;
  stack = tem->prev;
  obstack_free (obstack, tem);
  return stack;
}

#define search_level stack_level
static struct search_level *search_stack;

static tree lookup_field_1 ();
static int lookup_fnfields_1 ();

/* Allocate a level of searching.  */
static struct search_level *
push_search_level (stack, obstack)
     struct stack_level *stack;
     struct obstack *obstack;
{
  struct search_level tem;
  tem.prev = stack;

  return push_stack_level (obstack, &tem, sizeof (tem));
}

/* Discard a level of search allocation.  */
#define pop_search_level pop_stack_level

/* Search memoization.  */
struct type_level
{
  struct stack_level base;

  /* First object allocated in obstack of entries.  */
  char *entries;

  /* Number of types memoized in this context.  */
  int len;

  /* Type being memoized; save this if we are saving
     memoized contexts.  */
  tree type;
};

/* Obstack used for memoizing member and member function lookup.  */

static struct obstack type_obstack, type_obstack_entries;
static struct type_level *type_stack;
static tree _vptr_name;

/* Make things that look like tree nodes, but allocate them
   on type_obstack_entries.  */
static int my_tree_node_counter;
static tree my_tree_cons (), my_build_string ();

extern int flag_memoize_lookups, flag_save_memoized_contexts;

/* Variables for gathering statistics.  */
static int my_memoized_entry_counter;
static int memoized_fast_finds[2], memoized_adds[2], memoized_fast_rejects[2];
static int memoized_fields_searched[2];
static int n_fields_searched;
static int n_calls_lookup_field, n_calls_lookup_field_1;
static int n_calls_lookup_fnfields, n_calls_lookup_fnfields_1;
static int n_calls_get_base_type;
static int n_outer_fields_searched;
static int n_contexts_saved;

/* Local variables to help save memoization contexts.  */
static tree prev_type_memoized;
static struct type_level *prev_type_stack;

/* Allocate a level of type memoziation context.  */
static struct type_level *
push_type_level (stack, obstack)
     struct stack_level *stack;
     struct obstack *obstack;
{
  struct type_level tem;

  tem.base.prev = stack;

  obstack_finish (&type_obstack_entries);
  tem.entries = (char *) obstack_base (&type_obstack_entries);
  tem.len = 0;
  tem.type = NULL_TREE;

  return (struct type_level *)push_stack_level (obstack, &tem, sizeof (tem));
}

/* Discard a level of type memoziation context.  */

static struct type_level *
pop_type_level (stack)
     struct type_level *stack;
{
  obstack_free (&type_obstack_entries, stack->entries);
  return (struct type_level *)pop_stack_level (stack);
}

/* Make something that looks like a TREE_LIST, but
   do it on the type_obstack_entries obstack.  */
static tree
my_tree_cons (purpose, value, chain)
     tree purpose, value, chain;
{
  tree p = (tree)obstack_alloc (&type_obstack_entries, sizeof (struct tree_list));
  TREE_UID (p) = ++my_tree_node_counter;
  TREE_TYPE (p) = 0;
  ((int *)p)[3] = 0;
  TREE_SET_CODE (p, TREE_LIST);
  TREE_PURPOSE (p) = purpose;
  TREE_VALUE (p) = value;
  TREE_CHAIN (p) = chain;
  return p;
}

static tree
my_build_string (str)
     char *str;
{
  tree p = (tree)obstack_alloc (&type_obstack_entries, sizeof (struct tree_string));
  TREE_UID (p) = ++my_tree_node_counter;
  TREE_TYPE (p) = 0;
  ((int *)p)[3] = 0;
  TREE_SET_CODE (p, STRING_CST);
  TREE_STRING_POINTER (p) = str;
  TREE_STRING_LENGTH (p) = strlen (str);
  return p;
}

static tree
my_copy_node (node)
     tree node;
{
  struct obstack *ambient_obstack = current_obstack;
  tree t;

  current_obstack = &type_obstack_entries;

  t = copy_node (node);
  TREE_UID (t) = ++my_tree_node_counter;

  current_obstack = ambient_obstack;
  return t;
}

/* Memoizing machinery to make searches for multiple inheritance
   reasonably efficient.  */
#define MEMOIZE_HASHSIZE 8
typedef struct memoized_entry
{
  struct memoized_entry *chain;
  int uid;
  tree data_members[MEMOIZE_HASHSIZE];
  tree function_members[MEMOIZE_HASHSIZE];
} *ME;

#define MEMOIZED_CHAIN(ENTRY) (((ME)ENTRY)->chain)
#define MEMOIZED_UID(ENTRY) (((ME)ENTRY)->uid)
#define MEMOIZED_FIELDS(ENTRY,INDEX) (((ME)ENTRY)->data_members[INDEX])
#define MEMOIZED_FNFIELDS(ENTRY,INDEX) (((ME)ENTRY)->function_members[INDEX])
#define MEMOIZED_HASH_FN(NODE) (TREE_UID (NODE)&(MEMOIZE_HASHSIZE - 1))

static struct memoized_entry *
my_new_memoized_entry (chain)
     struct memoized_entry *chain;
{
  struct memoized_entry *p =
    (struct memoized_entry *)obstack_alloc (&type_obstack_entries,
					    sizeof (struct memoized_entry));
  bzero (p, sizeof (struct memoized_entry));
  MEMOIZED_CHAIN (p) = chain;
  MEMOIZED_UID (p) = ++my_memoized_entry_counter;
  return p;
}

/* When a new function or class context is entered, we build
   a table of types which have been searched for members.
   The table is an array (obstack) of types.  When a type is
   entered into the obstack, its CLASSTYPE_MTABLE_ENTRY
   field is set to point to a new record, of type struct memoized_entry.

   A non-NULL TREE_TYPE of the entry contains a visibility error message.

   The slots for the data members are arrays of tree nodes.
   These tree nodes are lists, with the TREE_PURPOSE
   of this list the known member name, and the TREE_VALUE
   as the FIELD_DECL for the member.

   For member functions, the TREE_PURPOSE is again the
   name of the member functions for that class,
   and the TREE_VALUE of the list is a pairs
   whose TREE_PURPOSE is a member functions of this name,
   and whose TREE_VALUE is a list of known argument lists this
   member function has been called with.  The TREE_TYPE of the pair,
   if non-NULL, is an error message to print.  */

/* Tell search machinery that we are entering a new context, and
   to update tables appropriately.

   TYPE is the type of the context we are entering, which can
   be NULL_TREE if we are not in a class's scope.

   USE_OLD, if nonzero tries to use previous context.  */
void
push_memoized_context (type, use_old)
     tree type;
     int use_old;
{
  int len;
  tree *tem;

  if (prev_type_stack)
    {
      if (use_old && prev_type_memoized == type)
	{
#ifdef GATHER_STATISTICS
	  n_contexts_saved++;
#endif
	  type_stack = prev_type_stack;
	  prev_type_stack = 0;

	  tem = &type_stack->base.first[0];
	  len = type_stack->len;
	  while (len--)
	    CLASSTYPE_MTABLE_ENTRY (tem[len*2]) = tem[len*2+1];
	  return;
	}
      /* Otherwise, need to pop old stack here.  */
      type_stack = pop_type_level (prev_type_stack);
      prev_type_memoized = 0;
      prev_type_stack = 0;
    }

  type_stack = push_type_level (type_stack, &type_obstack);
  type_stack->type = type;
}

/* Tell search machinery that we have left a context.
   We do not currently save these contexts for later use.
   If we wanted to, we could not use pop_search_level, since
   poping that level allows the data we have collected to
   be clobbered; a stack of obstacks would be needed.  */
pop_memoized_context (use_old)
     int use_old;
{
  int len;
  tree *tem = &type_stack->base.first[0];

  if (! flag_save_memoized_contexts)
    use_old = 0;
  else if (use_old)
    {
      len = type_stack->len;
      while (len--)
	tem[len*2+1] = (tree)CLASSTYPE_MTABLE_ENTRY (tem[len*2]);

      prev_type_stack = type_stack;
      prev_type_memoized = type_stack->type;
    }

  if (flag_memoize_lookups)
    {
      len = type_stack->len;
      while (len--)
	CLASSTYPE_MTABLE_ENTRY (tem[len*2])
	  = MEMOIZED_CHAIN (CLASSTYPE_MTABLE_ENTRY (tem[len*2]));
    }
  if (! use_old)
    type_stack = pop_type_level (type_stack);
  else
    type_stack = (struct type_level *)type_stack->base.prev;
}

/* Some simple list processing predicates.  */

/* Check whether TYPE is immediately derived from PARENT.
   Return actual base information if so.  Otherwise, return 0.  */
tree
get_base_type_1 (parent, type)
     register tree parent, type;
{
  register int i;

  parent = TYPE_MAIN_VARIANT (parent);
  type = TYPE_MAIN_VARIANT (type);

  for (i = 1; i <= CLASSTYPE_N_BASECLASSES (type); i++)
    if (TYPE_MAIN_VARIANT (CLASSTYPE_BASECLASS (type, i)) == parent)
      return CLASSTYPE_BASECLASS (type, i);

  return 0;
}

/* Check whether TYPE is derived from PARENT.
   Return the actual base information if so.  Otherwise return 0.
   If PROTECT is 1, then emit an error message if access to
   a public field of PARENT would be private.
   If PROTECT is 2, then emit an error message if
   TYPE is derived from PARENT via private visibility rules.
   If PROTECT is 3, then immediately private baseclass is ok,
   but deeper than that, if private, emit error message.  */
tree
get_base_type (parent, type, protect)
     register tree parent, type;
{
  tree xtype = type;
  tree otype;
  int head = 0, tail = 0;
  int is_private = 0;
  tree rval = NULL_TREE;
  int rval_private = 0;
  tree friends = current_class_type
    ? CLASSTYPE_FRIEND_CLASSES (type) : NULL_TREE;

#ifdef GATHER_STATISTICS
  n_calls_get_base_type++;
#endif

  parent = TYPE_MAIN_VARIANT (parent);
  search_stack = push_search_level (search_stack, &search_obstack);

  while (1)
    {
      int i, n_baselinks = CLASSTYPE_N_BASECLASSES (type);

      /* Process and/or queue base types.  */
      for (i = 1; i <= n_baselinks; i++)
	if (CLASSTYPE_MARKED5 (CLASSTYPE_BASECLASS (type, i)) == 0)
	  {
	    int via_private = is_private || !CLASSTYPE_VIA_PUBLIC (type, i);

	    if (via_private == 0)
	      ;
	    else if (protect == 0)
	      via_private = 0;
	    else if (protect == 1 && type == current_class_type)
	      /* The immediate base class of the class we are in
		 does let its public members through.  */
	      via_private = 0;
#ifndef NOJJG
	    else if (protect
		     && friends != NULL_TREE
		     && type == xtype
		     && value_member (current_class_type, friends))
	      /* Friend types of the most derived type have access
		 to its baseclass pointers.  */
	      via_private = 0;
#endif

	    CLASSTYPE_MARKED5 (CLASSTYPE_BASECLASS (type, i)) = 1;
	    otype = type;
	    obstack_ptr_grow (&search_obstack, CLASSTYPE_BASECLASS (type, i));
	    obstack_int_grow (&search_obstack, via_private);
	    tail += 2;
	    if (tail >= search_stack->limit)
	      abort ();
	  }
	else if (protect && ! CLASSTYPE_VIA_VIRTUAL (type, i))
	  {
	    error_with_aggr_type (parent, "type `%s' is ambiguous base class for type `%s'",
				  TYPE_NAME_STRING (xtype));
	    error ("(base class for types `%s' and `%s')",
		   TYPE_NAME_STRING (type),
		   TYPE_NAME_STRING (otype));
	    rval = error_mark_node;
	    break;
	  }

    dont_queue:
      /* Process head of queue, if one exists.  */
      if (head >= tail)
	break;

      type = search_stack->first[head++];
      is_private = (int)search_stack->first[head++];
      if (TYPE_MAIN_VARIANT (type) == parent)
	{
	  if (rval == 0)
	    {
	      rval = type;
	      rval_private = is_private;
	    }
	  goto dont_queue;
	}
    }
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;

    while (tp < search_tail)
      {
	CLASSTYPE_MARKED5 (*tp) = 0;
	tp += 2;
      }
  }
  search_stack = pop_search_level (search_stack);

  if (rval == error_mark_node)
    return error_mark_node;

  if (rval && protect && rval_private)
    {
      if (protect == 3)
	{
	  int i;

	  for (i = 1; i <= CLASSTYPE_N_BASECLASSES (xtype); i++)
	    if (parent == TYPE_MAIN_VARIANT (CLASSTYPE_BASECLASS (xtype, i)))
	      /* It's ok, since it's immedate.  */
	      return rval;
	}
      error ("type `%s' is derived from private `%s'",
	     TYPE_NAME_STRING (xtype),
	     TYPE_NAME_STRING (parent));
      return error_mark_node;
    }

  return rval;
}

/* Return the number of levels between type PARENT and type TYPE,
   following the leftmost path to PARENT.  If PARENT is its own main
   type variant, then if PARENT appears in different places from TYPE's
   point of view, the leftmost PARENT will be the one chosen.

   Return -1 if TYPE is not derived from PARENT.
   Return -2 if PARENT is an ambiguous base class of TYPE.
   Return -3 if PARENT is private to TYPE, and protect is non-zero.

   If PATH_PTR is non-NULL, then also build the list of types
   from PARENT to TYPE, with TREE_VIA_VIRUAL and TREE_VIA_PUBLIC
   set.  */
get_base_distance (parent, type, protect, path_ptr)
     register tree parent, type;
     int protect;
     tree *path_ptr;
{
  int head = 0, tail = 0;
  int is_private = 0;
  int rval;
  int depth = 0;
  int rval_private = 0;
  tree basetypes;
  tree friends = current_class_type
    ? CLASSTYPE_FRIEND_CLASSES (type) : NULL_TREE;
  int use_leftmost;

  if (TREE_READONLY (parent) || TREE_VOLATILE (parent))
    parent = TYPE_MAIN_VARIANT (parent);
  use_leftmost = (parent == TYPE_MAIN_VARIANT (parent));

  if (path_ptr)
    basetypes = CLASSTYPE_AS_LIST (type);

  if (TYPE_MAIN_VARIANT (parent) == type)
    {
      /* If the distance is 0, then we don't really need
	 a path pointer, but we shouldn't let garbage go back.  */
      if (path_ptr)
	*path_ptr = basetypes;
      return 0;
    }

  search_stack = push_search_level (search_stack, &search_obstack);

  while (1)
    {
      int i, n_baselinks = CLASSTYPE_N_BASECLASSES (type);

      /* Process and/or queue base types.  */
      for (i = 1; i <= n_baselinks; i++)
	if (CLASSTYPE_MARKED5 (CLASSTYPE_BASECLASS (type, i)) == 0)
	  {
	    tree btypes;

	    int via_private = is_private || !CLASSTYPE_VIA_PUBLIC (type, i);

	    if (via_private == 0)
	      ;
	    else if (protect == 0)
	      via_private = 0;
#if 0
	    /* 13 Jan, 1990: I guess this is turned off because
	       `get_base_type' will emit a more eloquent message
	       if a message desired [--Michael].  */
	    /* The immediate base class of the class we are in
	       does let its public members through.  */
	    else if (type == current_class_type)
	      via_private = 0;
	    else if (protect
		     && friends != NULL_TREE
		     && type == xtype
		     && value_member (current_class_type, friends))
	      /* Friend types of the most derived type have access
		 to its baseclass pointers.  */
	      via_private = 0;
#endif

	    CLASSTYPE_MARKED5 (CLASSTYPE_BASECLASS (type, i)) = 1;
	    obstack_ptr_grow (&search_obstack, CLASSTYPE_BASECLASS (type, i));

	    obstack_ptr_grow (&search_obstack, depth);
	    obstack_int_grow (&search_obstack, via_private);
	    if (path_ptr)
	      {
		btypes = tree_cons (NULL_TREE, CLASSTYPE_BASECLASS (type, i),
				    basetypes);
		TREE_VIA_PUBLIC (btypes) = CLASSTYPE_VIA_PUBLIC (type, i);
		TREE_VIA_VIRTUAL (btypes) = CLASSTYPE_VIA_VIRTUAL (type, i);
		obstack_ptr_grow (&search_obstack, btypes);
		tail += 1;
	      }
	    tail += 3;
	    if (tail >= search_stack->limit)
	      abort ();
	  }
	else if (! CLASSTYPE_VIA_VIRTUAL (type, i))
	  {
	    rval = -2;
	    goto done;
	  }

      /* Process head of queue, if one exists.  */
      if (head >= tail)
	{
	  rval = -1;
	  break;
	}

      type = search_stack->first[head++];
      depth = (int)search_stack->first[head++] + 1;
      is_private = (int)search_stack->first[head++];
      if (path_ptr)
	basetypes = search_stack->first[head++];
      if (type == parent
	  || (use_leftmost && TYPE_MAIN_VARIANT (type) == parent))
	{
	  rval = depth;
	  rval_private = is_private;
	  break;
	}
    }
 done:
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;
    int increment = path_ptr ? 4 : 3;

    while (tp < search_tail)
      {
	CLASSTYPE_MARKED5 (*tp) = 0;
	tp += increment;
      }

    /* Now, guarantee that we are following the leftmost
       path in the chain.  */
    if (use_leftmost
	&& rval > 0
	&& (DECL_OFFSET (TYPE_NAME (type)) != 0
	    || TREE_VIA_VIRTUAL (type)))
      {
	/* Reduce all types yet to be fully processed into
	   the base type we are looking for, or NULL_TREE.  */
	for (tp = search_stack->first; tp < search_tail; tp += increment)
	  {
	    tree *sub_tp, sub_path_ptr;
	    int sub_rval;

	    /* Don't chase down more right-most paths.  */
	    if (TREE_VIA_VIRTUAL (*tp)
		|| DECL_OFFSET (TYPE_NAME (*tp)) > DECL_OFFSET (TYPE_NAME (type)))
	      {
		*tp = NULL_TREE;
		continue;
	      }

	    /* Don't hassle with duplicates.  */
	    if (*tp == type)
	      goto skip;

	    for (sub_tp = search_stack->first; sub_tp < tp; sub_tp += increment)
	      if (*tp == *sub_tp)
		goto skip;

	    /* Find this type's TYPE basetype, if it has one.  */
	    sub_rval = get_base_distance (parent, *tp, 0, &sub_path_ptr);
	    if (sub_rval == -1)
	      *tp = NULL_TREE;
	    else
	      {
		if (path_ptr && TREE_CHAIN (tp[3]))
		  {
		    tree last;
		    tree next_to_last = sub_path_ptr;
		    while (TREE_CHAIN (next_to_last)
			   && TREE_CHAIN (TREE_CHAIN (next_to_last)))
		      next_to_last = TREE_CHAIN (next_to_last);
		    if (next_to_last == sub_path_ptr)
		      {
			sub_path_ptr = copy_node (sub_path_ptr);
			last = sub_path_ptr;
		      }
		    else
		      {
			last = copy_node (TREE_CHAIN (next_to_last));
			TREE_CHAIN (next_to_last) = last;
		      }
		    TREE_CHAIN (last) = TREE_CHAIN (tp[3]);
		  }
		*tp = TREE_VALUE (sub_path_ptr);
		if (path_ptr)
		  tp[3] = sub_path_ptr;
	      }
	  skip: {}
	  }

	/* For all the types which reduce to TYPE, choose
	   the leftmost non-virtual one of them.  */
	for (tp = search_stack->first; tp < search_tail; tp += increment)
	  {
	    if (*tp == NULL_TREE)
	      continue;

	    if (DECL_OFFSET (TYPE_NAME (*tp)) < DECL_OFFSET (TYPE_NAME (type)))
	      {
		rval = -2;
		type = *tp;
		if (path_ptr)
		  basetypes = tp[3];
	      }
	  }
	if (rval == -2)
	  rval_private = 0;
      }
  }
  search_stack = pop_search_level (search_stack);

  if (rval && protect && rval_private)
    return -3;

  if (path_ptr)
    *path_ptr = basetypes;
  return rval;
}

/* Search for a member with name NAME in a multiple inheritance lattice
   specified by TYPE.  If it does not exist, return NULL_TREE.
   If the member is ambiguously referenced, return `error_mark_node'.
   Otherwise, return the FIELD_DECL.  */

/* Do a 1-level search for NAME as a member of TYPE.  The caller
   must figure out whether it has a visible path to this field.
   (Since it is only one level, this is reasonable.)  */
static tree
lookup_field_1 (type, name)
     tree type, name;
{
  register tree field = TYPE_FIELDS (type);

#ifdef GATHER_STATISTICS
  n_calls_lookup_field_1++;
#endif
  while (field)
    {
#ifdef GATHER_STATISTICS
      n_fields_searched++;
#endif
      if (DECL_ANON_UNION_ELEM (field))
	{
	  tree temp = lookup_field_1 (TREE_TYPE (field), name);
	  if (temp)
	    return temp;
	}
      if (DECL_NAME (field) == name)
	return field;
      field = TREE_CHAIN (field);
    }
  /* Not found.  */
  if (name == _vptr_name)
    {
      /* Give the user what s/he thinks s/he wants.  */
      if (TYPE_VIRTUAL_P (type))
	return CLASSTYPE_VFIELD (type);
    }
  return NULL_TREE;
}

/* Compute the visibility of FIELD.  This is done by computing
   the visibility available to each type in BASETYPES (which comes
   as a list of [via_public/basetype] in reverse order, namely base
   class before derived class).  The first one which defines a
   visibility defines the visibility for the field.  Otherwise, the
   visibility of the field is that which occurs normally.

   Uses global variables CURRENT_CLASS_TYPE and
   CURRENT_FUNCTION_DECL to use friend relationships
   if necessary.

   This will be static when lookup_fnfield comes into this file.  */

#define PUBLIC_RETURN do { TREE_FIELD_PUBLIC (field) = 1; return visibility_public; } while (0)
#define PROTECTED_RETURN do { TREE_FIELD_PROTECTED (field) = 1; return visibility_protected; } while (0)
#define PRIVATE_RETURN do { TREE_FIELD_PRIVATE (field) = 1; return visibility_private; } while (0)

enum visibility_type
compute_visibility (basetypes, field)
     tree basetypes, field;
{
  enum visibility_type visibility = visibility_public;
  tree types, type;
  tree context = DECL_FIELD_CONTEXT (field);

  /* Virtual function tables are never private.
     But we should know that we are looking for this,
     and not even try to hide it.  */
  if (VFIELD_NAME_P (DECL_NAME (field)) == 1)
    return visibility_public;

  /* Make these special cases fast.  */
  if (TREE_VALUE (basetypes) == current_class_type)
    {
      if (TREE_FIELD_PUBLIC (field))
	return visibility_public;
      if (TREE_FIELD_PROTECTED (field))
	return visibility_protected;
      if (TREE_FIELD_PRIVATE (field))
	return visibility_private;
    }

  /* Member function manipulating its own members.  */
  if (current_class_type == context)
    PUBLIC_RETURN;

  /* Member found immediately within object.  */
  if (TREE_CHAIN (basetypes) == NULL_TREE)
    {
      /* At object's top level, public members are public.  */
      if (TREE_PROTECTED (field) == 0 && TREE_PRIVATE (field) == 0)
	PUBLIC_RETURN;

      /* Friend function manipulating members it gets (for being a friend).  */
      if (is_friend (context, current_function_decl))
	PUBLIC_RETURN;

      /* Inner than that, without special visibility,

	   protected members are ok if type of object is current_class_type
	   is derived therefrom.  This means that if the type of the object
	   is a base type for our current class type, we cannot access
	   protected members.

	   private members are not ok.  */
      if (current_class_type && DECL_VISIBILITY (field) == NULL_TREE)
	{
	  if (TREE_PRIVATE (field))
	    PRIVATE_RETURN;

	  if (TREE_PROTECTED (field))
	    {
	      if (context == current_class_type
		  || (type = get_base_type (current_class_type, context, 0)))
		PUBLIC_RETURN;
	      else
		PROTECTED_RETURN;
	    }
	  else abort ();
	}
    }
  /* Friend function manipulating members it gets (for being a friend).  */
  if (is_friend (context, current_function_decl))
    PUBLIC_RETURN;

  /* must reverse more than one element */
  basetypes = nreverse (basetypes);

  types = basetypes;

  while (types)
    {
      tree member;
      type = TYPE_MAIN_VARIANT (TREE_VALUE (types));

      member = purpose_member (type, DECL_VISIBILITY (field));
      if (member)
	{
	  visibility = (enum visibility_type)TREE_VALUE (member);
	  if (visibility == visibility_public
	      || is_friend (type, current_function_decl)
	      || (visibility == visibility_protected
		  && current_class_type
		  && get_base_type (context, current_class_type, 0)))
	    visibility = visibility_public;
	  goto ret;
	}

      /* Friends inherit the visibility of the class they inherit from.  */
      if (is_friend (type, current_function_decl))
	{
	  if (type == context)
	    {
	      visibility = visibility_public;
	      goto ret;
	    }
	  if (TREE_PROTECTED (field))
	    {
	      visibility = visibility_public;
	      goto ret;
	    }
#if 0
	  /* This short-cut is too short.  */
	  if (visibility == visibility_public)
	    goto ret;
#endif
	  /* else, may be a friend of a deeper base class */
	}

      if (type == context)
	break;

      types = TREE_CHAIN (types);
      /* If the next type was not VIA_PUBLIC, then fields of all
	 remaining class past that one are private.  */
      if (types && ! TREE_VIA_PUBLIC (types))
	visibility = visibility_private;
    }

  /* No special visibilities apply.  Use normal rules.
     No assignment needed for BASETYPEs here from the nreverse.
     This is because we use it only for information about the
     path to the base.  The code earlier dealt with what
     happens when we are at the base level.  */

  if (visibility == visibility_public)
    {
      basetypes = nreverse (basetypes);
      if (TREE_PRIVATE (field))
	PRIVATE_RETURN;
      if (TREE_PROTECTED (field))
	{
	  /* Used to check if the current class type was derived from
	     the type that contains the field.  This is wrong for
	     multiple inheritance because is gives one class reference
	     to protected members via another classes protected path.
	     I.e., if A; B1 : A; B2 : A;  Then B1 and B2 can access
	     their own members which are protected in A, but not
	     those same members in one another.  */
	  if (
#if 1
	      current_class_type
	      && get_base_type (context, current_class_type, 0)
#else
	      current_class_type
	      && value_member (current_class_type, basetypes)
#endif
	      )
	    PUBLIC_RETURN;
	  PROTECTED_RETURN;
	}
      PUBLIC_RETURN;
    }

  if (visibility == visibility_private
      && current_class_type != NULL_TREE)
    {
      if (TREE_PRIVATE (field))
	{
	  nreverse (basetypes);
	  PRIVATE_RETURN;
	}

      /* See if the field isn't protected.  */
      if (TREE_PROTECTED (field))
	{
	  tree test;
#if 0
	  test = get_base_type (type, current_class_type, 0);
#else
	  test = value_member (current_class_type, basetypes);
#endif
	  nreverse (basetypes);
	  if (test)
	    PUBLIC_RETURN;
	  PROTECTED_RETURN;
	}

      /* See if the field isn't a public member of
	 a private base class.  */

      visibility = visibility_public;
      types = TREE_CHAIN (basetypes);
      while (types)
	{
	  if (! TREE_VIA_PUBLIC (types))
	    {
	      if (visibility == visibility_private)
		{
		  visibility = visibility_private;
		  goto ret;
		}
	      visibility = visibility_private;
	    }
	  if (TYPE_MAIN_VARIANT (TREE_VALUE (types)) == context)
	    {
	      visibility = visibility_public;
	      goto ret;
	    }
	  types = TREE_CHAIN (types);
	}
      abort ();
    }

 ret:
  nreverse (basetypes);

  if (visibility == visibility_public)
    TREE_FIELD_PUBLIC (field) = 1;
  else if (visibility == visibility_protected)
    TREE_FIELD_PROTECTED (field) = 1;
  else if (visibility == visibility_private)
    TREE_FIELD_PRIVATE (field) = 1;
  else abort ();
  return visibility;
}

/* Make an entry in the memoized table for type TYPE
   that the entry for NAME is FIELD.  */

tree
make_memoized_table_entry (type, name, function_p)
     tree type, name;
     int function_p;     
{
  int index = MEMOIZED_HASH_FN (name);
  tree entry, *prev_entry;

  memoized_adds[function_p] += 1;
  if (CLASSTYPE_MTABLE_ENTRY (type) == NULL_TREE)
    {
      obstack_ptr_grow (&type_obstack, type);
      obstack_blank (&type_obstack, sizeof (struct memoized_entry *));
      CLASSTYPE_MTABLE_ENTRY (type) = my_new_memoized_entry (0);
      type_stack->len++;
      if (type_stack->len * 2 >= type_stack->base.limit)
	abort ();
    }
  if (function_p)
    prev_entry = &MEMOIZED_FNFIELDS (CLASSTYPE_MTABLE_ENTRY (type), index);
  else
    prev_entry = &MEMOIZED_FIELDS (CLASSTYPE_MTABLE_ENTRY (type), index);

  entry = my_tree_cons (name, 0, *prev_entry);
  *prev_entry = entry;

  /* Don't know the error message to give yet.  */
  TREE_TYPE (entry) = error_mark_node;

  return entry;
}

tree
lookup_field (xbasetype, name, protect)
     register tree xbasetype, name;
     int protect;
{
  int head = 0, tail = 0;
  tree type, rval;
  tree basetype, basetypes;
  enum visibility_type this_v = visibility_default;
  tree entry;
  enum visibility_type own_visibility = visibility_default;
  int vbase_name_p = VBASE_NAME_P (name);

  /* Things for memoization.  */
  char *errstr = 0;

  /* Set this to nonzero if we don't know how to compute
     accurate error messages for visibility.  */
  int index = MEMOIZED_HASH_FN (name);

  if (TREE_CODE (xbasetype) == TREE_LIST)
    basetypes = xbasetype, basetype = TREE_VALUE (xbasetype);
  else
    basetypes = CLASSTYPE_AS_LIST (xbasetype), basetype = xbasetype;

  if (CLASSTYPE_MTABLE_ENTRY (basetype))
    {
      tree tem = MEMOIZED_FIELDS (CLASSTYPE_MTABLE_ENTRY (basetype), index);

      while (tem && TREE_PURPOSE (tem) != name)
	{
	  memoized_fields_searched[0]++;
	  tem = TREE_CHAIN (tem);
	}
      if (tem)
	{
	  if (protect && TREE_TYPE (tem))
	    {
	      error (TREE_STRING_POINTER (TREE_TYPE (tem)),
		     IDENTIFIER_POINTER (name),
		     TYPE_NAME_STRING (DECL_FIELD_CONTEXT (TREE_VALUE (tem))));
	      return error_mark_node;
	    }
	  if (TREE_VALUE (tem) == NULL_TREE)
	    memoized_fast_rejects[0] += 1;
	  else
	    memoized_fast_finds[0] += 1;
	  return TREE_VALUE (tem);
	}
    }

#ifdef GATHER_STATISTICS
  n_calls_lookup_field++;
#endif
  if (flag_memoize_lookups && ! global_bindings_p ())
    entry = make_memoized_table_entry (basetype, name, 0);
  else
    entry = 0;

  rval = lookup_field_1 (basetype, name);

  if (rval)
    {
      if (flag_memoize_lookups || protect)
	{
	  if (TREE_PRIVATE (rval) | TREE_PROTECTED (rval))
	    this_v = compute_visibility (basetypes, rval);
	  if (TREE_CODE (rval) == CONST_DECL)
	    {
	      if (this_v == visibility_private)
		errstr = "enum `%s' is a private value of class `%s'";
	      else if (this_v == visibility_protected)
		errstr = "enum `%s' is a protected value of class `%s'";
	    }
	  else
	    {
	      if (this_v == visibility_private)
		errstr = "member `%s' is a private member of class `%s'";
	      else if (this_v == visibility_protected)
		errstr = "member `%s' is a protected member of class `%s'";
	    }
	}

      if (entry)
	{
	  if (errstr)
	    {
	      /* This depends on behavior of lookup_field_1!  */
	      tree error_string = my_build_string (errstr);
	      TREE_TYPE (entry) = error_string;
	    }
	  else
	    {
	      /* Let entry know there is no problem with this access.  */
	      TREE_TYPE (entry) = NULL_TREE;
#if 0
	      /* And since everything is ok, bear the
		 cost of generating correct code.  */
	      if (DECL_OFFSET (TYPE_NAME (basetype)) != 0
		  || TREE_VIA_VIRTUAL (basetype))
		{
		  rval = my_copy_node (rval);
		  DECL_FIELD_CONTEXT (rval) = basetype;
		}
#endif
	    }
	  TREE_VALUE (entry) = rval;
	}
#if 0
      else if ((DECL_OFFSET (TYPE_NAME (basetype)) != 0
		|| TREE_VIA_VIRTUAL (basetype))
	       && ! (errstr && protect))
	{
	  rval = my_copy_node (rval);
	  DECL_FIELD_CONTEXT (rval) = basetype;
	}
#endif

      if (errstr && protect)
	{
	  error (errstr, IDENTIFIER_POINTER (name), TYPE_NAME_STRING (basetype));
	  return error_mark_node;
	}
      return rval;
    }

  type = TYPE_MAIN_VARIANT (basetype);

  search_stack = push_search_level (search_stack, &search_obstack);
  TREE_VIA_PUBLIC (basetypes) = 1;

  while (1)
    {
      int i, n_baselinks = CLASSTYPE_N_BASECLASSES (type);

      /* Process and/or queue base types.  */
      for (i = 1; i <= n_baselinks; i++)
	{
	  if (CLASSTYPE_MARKED2 (CLASSTYPE_BASECLASS (type, i)) == 0)
	    {
	      tree btypes;

	      CLASSTYPE_MARKED2 (CLASSTYPE_BASECLASS (type, i)) = 1;
	      btypes = my_tree_cons (NULL_TREE, CLASSTYPE_BASECLASS (type, i),
				     basetypes);
	      TREE_VIA_PUBLIC (btypes) = CLASSTYPE_VIA_PUBLIC (type, i);
	      TREE_VIA_VIRTUAL (btypes) = CLASSTYPE_VIA_VIRTUAL (type, i);
	      obstack_ptr_grow (&search_obstack, btypes);
	      tail += 1;
	      if (tail >= search_stack->limit)
		abort ();
	    }
	}

      /* Process head of queue, if one exists.  */
      if (head >= tail)
	break;

      basetypes = search_stack->first[head++];
      type = TREE_VALUE (basetypes);

      /* See if we can find NAME in TYPE.  If RVAL is nonzero,
	 and we do find NAME in TYPE, verify that such a second
	 sighting is in fact legal.  */

      if (rval)
	{
	  /* Just another way of finding the same member.  */
	  if (DECL_FIELD_CONTEXT (rval) == type)
	    {
	      enum visibility_type new_v
		= compute_visibility (basetypes, rval);
	      if (this_v != new_v)
		errstr = "conflicting visibilities to member `%s'";
	    }
	  /* Same baseclass, different places in the lattice.  */
	  else if (DECL_FIELD_CONTEXT (rval) == TYPE_MAIN_VARIANT (type))
	    errstr = "member `%s' belongs to distinct base classes `%s'";
	  else
	    {
	      tree nval = lookup_field_1 (type, name);

	      if (nval && get_base_type (type, DECL_FIELD_CONTEXT (rval), 0) == 0)
		{
		  /* We found it in other than a baseclass of RVAL's.  */
		  errstr = "request for member `%s' is ambiguous";
		}
	    }
	  if (errstr && entry)
	    {
	      tree error_string = my_build_string (errstr);
	      TREE_TYPE (entry) = error_string;
	    }
	  if (errstr && protect)
	    break;
	}
      else
	{
	  rval = lookup_field_1 (type, name);
	  if (rval)
	    {
#if 0
	      if (DECL_OFFSET (TYPE_NAME (type)) != 0
		  || TREE_VIA_VIRTUAL (type))
		{
		  rval = my_copy_node (rval);
		  DECL_FIELD_CONTEXT (rval) = type;
		}
#endif

	      if (entry || protect)
		this_v = compute_visibility (basetypes, rval);
	      if (entry)
		TREE_VALUE (entry) = rval;

	      /* These may look ambiguous, but they really are not.  */
	      if (vbase_name_p)
		break;
	    }
	}
    }
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;

    /* If this FIELD_DECL defines its own visibility, deal with that.  */
    if (rval && errstr == 0
	&& DECL_VISIBILITY (rval)
	&& (protect || entry))
      {
	while (tp < search_tail)
	  {
	    /* If is possible for one of the derived types on the
	       path to have defined special visibility for this
	       field.  Look for such declarations and report an
	       error if a conflict is found.  */
	    enum visibility_type new_v;

	    if (this_v != visibility_default)
	      new_v = compute_visibility (*tp, rval);
	    if (this_v != visibility_default && new_v != this_v)
	      {
		errstr = "conflicting visibilities to member `%s'";
		this_v = visibility_default;
	      }
	    own_visibility = new_v;
	    CLASSTYPE_MARKED2 (TREE_VALUE (*tp++)) = 0;
	  }
      }
    else
      {
	while (tp < search_tail)
	  CLASSTYPE_MARKED2 (TREE_VALUE (*tp++)) = 0;
      }
  }
  search_stack = pop_search_level (search_stack);

  if (errstr == 0)
    {
      if (own_visibility == visibility_private)
	errstr = "member `%s' declared private";
      else if (own_visibility == visibility_protected)
	errstr = "member `%s' declared protected";
      else if (this_v == visibility_private)
	errstr = TREE_PRIVATE (rval) ? "member `%s' is private" : "member `%s' is from private base class";
      else if (this_v == visibility_protected)
	errstr = "member `%s' is protected";
    }

  if (entry)
    {
      if (errstr)
	{
	  tree error_string = my_build_string (errstr);
	  /* Save error message with entry.  */
	  TREE_TYPE (entry) = error_string;
	}
      else
	{
	  /* Mark entry as having no error string.  */
	  TREE_TYPE (entry) = NULL_TREE;
	}
    }

  if (errstr && protect)
    {
      error (errstr, IDENTIFIER_POINTER (name), TYPE_NAME_STRING (type));
      rval = error_mark_node;
    }
  return rval;
}

/* TYPE is a class type. Return the index of the fields within
   the method vector with name NAME, or -1 is no such field exists.  */
static int
lookup_fnfields_1 (type, name)
     tree type, name;
{
  register tree method_vec = CLASSTYPE_METHOD_VEC (type);

  if (method_vec != 0)
    {
      register tree *methods = &TREE_VEC_ELT (method_vec, 0);
      register tree *end = TREE_VEC_END (method_vec);

#ifdef GATHER_STATISTICS
      n_calls_lookup_fnfields_1++;
#endif
      if (*methods == 0)
	methods++;
      while (methods != end)
	{
#ifdef GATHER_STATISTICS
	  n_outer_fields_searched++;	
#endif
	  if (DECL_ORIGINAL_NAME (*methods) == name)
	    break;
	  methods++;
	}
      if (methods != end)
	return methods - &TREE_VEC_ELT (method_vec, 0);
    }

  return -1;
}

/* Given a list of member functions FIELDS (which are implicitly
   named TREE_PURPOSE (FIELDS), and come from base type
   DECL_FIELD_CONTEXT (TREE_VALUE (FIELDS))), attempt to find the
   actual method which can accept (using conversions) PARMS.
   The types of PARMS are already computed in PARMTYPES.  */
tree
lookup_fnfield (fields, parms, parmtypes)
     tree fields, parms, parmtypes;
{
  abort ();
}

/* Starting from BASETYPE, return a TREE_BASELINK-like object
   which gives the following information (in a list):

   TREE_TYPE: list of basetypes needed to get to...
   TREE_VALUE: list of all functions in of given type
   which have name NAME.

   No visibility information is computed by this function,
   other then to adorn the list of basetypes with
   TREE_VIA_PUBLIC.

   If FIND_AMBIGUOUS is non-zero, then if we find two ways to get
   to the same member function, both those ways are found,
   and the caller must know what to do about this.  */
tree
lookup_fnfields (basetypes, name, find_ambiguous)
     tree basetypes, name;
     int find_ambiguous;
{
  int head = 0, tail = 0;
  tree type, rval, rvals = NULL_TREE;
  tree basetype;
  tree entry;

  /* For now, don't try this.  */
  int protect = find_ambiguous;

  /* Things for memoization.  */
  char *errstr = 0;

  /* Set this to nonzero if we don't know how to compute
     accurate error messages for visibility.  */
  int index = MEMOIZED_HASH_FN (name);

  basetype = TREE_VALUE (basetypes);

  if (CLASSTYPE_MTABLE_ENTRY (basetype))
    {
      tree tem = MEMOIZED_FNFIELDS (CLASSTYPE_MTABLE_ENTRY (basetype), index);

      while (tem && TREE_PURPOSE (tem) != name)
	{
	  memoized_fields_searched[1]++;
	  tem = TREE_CHAIN (tem);
	}
      if (tem)
	{
	  if (protect && TREE_TYPE (tem))
	    {
	      error (TREE_STRING_POINTER (TREE_TYPE (tem)),
		     IDENTIFIER_POINTER (name),
		     TYPE_NAME_STRING (DECL_FIELD_CONTEXT (TREE_VALUE (TREE_VALUE (tem)))));
	      return error_mark_node;
	    }
	  if (TREE_VALUE (tem) == NULL_TREE)
	    {
	      memoized_fast_rejects[1] += 1;
	      return NULL_TREE;
	    }
	  else
	    {
	      /* Want to return this, but we must make sure
		 that visibility information is consistent.  */
	      tree baselink = TREE_VALUE (tem);
	      tree memoized_basetypes = TREE_PURPOSE (baselink);
	      tree these_basetypes = basetypes;
	      while (memoized_basetypes && these_basetypes)
		{
		  memoized_fields_searched[1]++;
		  if (TREE_VALUE (memoized_basetypes) != TREE_VALUE (these_basetypes))
		    break;
		  memoized_basetypes = TREE_CHAIN (memoized_basetypes);
		  these_basetypes = TREE_CHAIN (these_basetypes);
		}
	      if (memoized_basetypes == these_basetypes)
		{
		  memoized_fast_finds[1] += 1;
		  return TREE_VALUE (tem);
		}
	      /* else, we must re-find this field by hand.  */
	      baselink = tree_cons (basetypes, TREE_VALUE (baselink), TREE_CHAIN (baselink));
	      return baselink;
	    }
	}
    }

#ifdef GATHER_STATISTICS
  n_calls_lookup_fnfields++;
#endif
  if (flag_memoize_lookups && ! global_bindings_p ())
    entry = make_memoized_table_entry (basetype, name, 1);
  else
    entry = 0;

  index = lookup_fnfields_1 (basetype, name);

  if (index >= 0)
    {
      rval = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), index);
      rvals = my_tree_cons (basetypes, rval, NULL_TREE);
      if (CLASSTYPE_BASELINK_VEC (basetype))
	TREE_TYPE (rvals) = TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (basetype), index);

      if (entry)
	{
	  TREE_VALUE (entry) = rvals;
	  TREE_TYPE (entry) = NULL_TREE;
	}

      if (errstr && protect)
	{
	  error (errstr, IDENTIFIER_POINTER (name), TYPE_NAME_STRING (basetype));
	  return error_mark_node;
	}
      return rvals;
    }
  rval = NULL_TREE;
  type = TYPE_MAIN_VARIANT (basetype);

  search_stack = push_search_level (search_stack, &search_obstack);
  TREE_VIA_PUBLIC (basetypes) = 1;

  while (1)
    {
      int i, n_baselinks = CLASSTYPE_N_BASECLASSES (type);

      /* Process and/or queue base types.  */
      for (i = 1; i <= n_baselinks; i++)
	{
	  if (CLASSTYPE_MARKED2 (CLASSTYPE_BASECLASS (type, i)) == 0)
	    {
	      tree btypes;

	      CLASSTYPE_MARKED2 (CLASSTYPE_BASECLASS (type, i)) = 1;
	      btypes = my_tree_cons (NULL_TREE, CLASSTYPE_BASECLASS (type, i),
				     basetypes);
	      TREE_VIA_PUBLIC (btypes) = CLASSTYPE_VIA_PUBLIC (type, i);
	      TREE_VIA_VIRTUAL (btypes) = CLASSTYPE_VIA_VIRTUAL (type, i);
	      obstack_ptr_grow (&search_obstack, btypes);
	      tail += 1;
	      if (tail >= search_stack->limit)
		abort ();
	    }
	}

      /* Process head of queue, if one exists.  */
      if (head >= tail)
	break;

      basetypes = search_stack->first[head++];
      type = TREE_VALUE (basetypes);

      /* See if we can find NAME in TYPE.  If RVAL is nonzero,
	 and we do find NAME in TYPE, verify that such a second
	 sighting is in fact legal.  */

      if (rval)
	{
	  tree context = DECL_FIELD_CONTEXT (rval);
	  /* Just another way of finding the same member.  */
	  if (context == type)
	    ;
	  /* Same baseclass, maybe different places in the lattice.  */
	  else if (context == TYPE_MAIN_VARIANT (type))
	    {
	      if (TREE_VIA_VIRTUAL (TREE_PURPOSE (rvals)))
		if (TREE_VIA_VIRTUAL (basetypes))
		  ;
		else
		  errstr = "member `%s' belongs to virtual and non-virtual baseclasses `%s'";
	      else if (TREE_VIA_VIRTUAL (basetypes))
		errstr = "member `%s' belongs to virtual and non-virtual baseclasses `%s'";
	      else
		errstr = "member `%s' belongs to MI-distinct base classes `%s'";
	    }
	  else
	    {
	      int index = lookup_fnfields_1 (type, name);

	      if (index >= 0 && get_base_type (type, context, 0) == 0)
		{
		  /* We found it in other than a baseclass of RVAL's.  */
		  rvals = my_tree_cons (basetypes, TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), index), rvals);
		  if (CLASSTYPE_BASELINK_VEC (type))
		    TREE_TYPE (rvals) = TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (type), index);
		}
	    }
	  if (errstr && entry)
	    {
	      tree error_string = my_build_string (errstr);
	      TREE_TYPE (entry) = error_string;
	    }
	  if (errstr && find_ambiguous)
	    {
	      rvals = error_mark_node;
	      break;
	    }
	}
      else
	{
	  int index = lookup_fnfields_1 (type, name);
	  if (index >= 0)
	    {
	      rval = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), index);
	      rvals = my_tree_cons (basetypes, rval, NULL_TREE);
	      if (CLASSTYPE_BASELINK_VEC (type))
		TREE_TYPE (rvals) = TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (type), index);
	      if (entry)
		TREE_VALUE (entry) = rvals;
	    }
	  else
	    rval = NULL_TREE;
	}
    }
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;

    while (tp < search_tail)
      CLASSTYPE_MARKED2 (TREE_VALUE (*tp++)) = 0;
  }
  search_stack = pop_search_level (search_stack);

  if (entry)
    {
      if (errstr)
	{
	  tree error_string = my_build_string (errstr);
	  /* Save error message with entry.  */
	  TREE_TYPE (entry) = error_string;
	}
      else
	{
	  /* Mark entry as having no error string.  */
	  TREE_TYPE (entry) = NULL_TREE;
	}
    }

  if (errstr && protect)
    {
      error (errstr, IDENTIFIER_POINTER (name), TYPE_NAME_STRING (type));
      rvals = error_mark_node;
    }

  return rvals;
}

/* BREADTH-FIRST SEARCH ROUTINES.  */

/* Search a multiple inheritance hierarchy by breadth-first search.

   TYPE is an aggregate type, possibly in a multiple-inheritance hierarchy.
   TESTFN is a function, which, if true, means that our condition has been met,
   and its return value should be returned.
   QFN, if non-NULL, is a predicate dictating whether the type should
   even be queued.  */

int
breadth_first_search (type, testfn, qfn)
     tree type;
     int (*testfn)();
     int (*qfn)();
{
  int head = 0, tail = 0;
  int rval = 0;

  search_stack = push_search_level (search_stack, &search_obstack);

  while (1)
    {
      int n_baselinks = CLASSTYPE_N_BASECLASSES (type);
      int i;

      /* Process and/or queue base types.  */
      for (i = 1; i <= n_baselinks; i++)
	if (CLASSTYPE_MARKED (CLASSTYPE_BASECLASS (type, i)) == 0
	    && (qfn == 0 || (*qfn) (type, i)))
	  {
	    CLASSTYPE_MARKED (CLASSTYPE_BASECLASS (type, i)) = 1;
	    obstack_ptr_grow (&search_obstack, type);
	    obstack_int_grow (&search_obstack, i);
	    tail += 2;
	    if (tail >= search_stack->limit)
	      abort ();
	  }

      /* Process head of queue, if one exists.  */
      if (head >= tail)
	{
	  rval = 0;
	  break;
	}

      type = search_stack->first[head++];
      i = (int)search_stack->first[head++];
      if (rval = (*testfn) (type, i))
	break;
      type = CLASSTYPE_BASECLASS (type, i);
    }
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;
    while (tp < search_tail)
      {
	tree type = *tp++;
	int i = (int)(*tp++);
	CLASSTYPE_MARKED (CLASSTYPE_BASECLASS (type, i)) = 0;
      }
  }

  search_stack = pop_search_level (search_stack);
  return rval;
}

/* Functions to use in breadth first searches.  */
typedef tree (*pft)();
typedef int (*pfi)();

int tree_needs_constructor_p (type, i)
     tree type;
{
  tree basetype = i == 0 ? type : CLASSTYPE_BASECLASS (type, i);
  return TYPE_NEEDS_CONSTRUCTOR (basetype);
}

static tree declarator;

static tree
get_virtuals_named_this (type, i)
     tree type;
     int i;
{
  tree basetype = i == 0? type : CLASSTYPE_BASECLASS (type, i);
  tree fields = lookup_fnfields (CLASSTYPE_AS_LIST (basetype), declarator, 0);

  if (fields == 0 || fields == error_mark_node)
    return 0;

  /* Get to the function decls, and return the first virtual function
     with this name, if there is one.  */
  while (fields)
    {
      tree fndecl;

      for (fndecl = TREE_VALUE (fields); fndecl; fndecl = TREE_CHAIN (fndecl))
	if (DECL_VIRTUAL_P (fndecl))
	  return fields;
      fields = next_baselink (fields);
    }
  return NULL_TREE;
}

static tree get_virtual_destructor (type, i)
     tree type;
     int i;
{
  type = i == 0 ? type : CLASSTYPE_BASECLASS (type, i);
  if (TYPE_HAS_DESTRUCTOR (type)
      && DECL_VIRTUAL_P (TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), 0)))
    return TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), 0);
  return 0;
}

int tree_has_any_destructor_p (type, i)
     tree type;
     int i;
{
  if (i == 0)
    return TYPE_NEEDS_DESTRUCTOR (type);
  return TYPE_NEEDS_DESTRUCTOR (CLASSTYPE_BASECLASS (type, i));
}

/* Given a class type TYPE, and a function decl FNDECL,
   look for the first function the TYPE's heirarchy which
   FNDECL could match as a virtual function.

   DTORP is nonzero if we are looking for a destructor.  Destructors
   need special treatment because they do not match by name.  */
tree
get_first_matching_virtual (type, fndecl, dtorp)
     tree type, fndecl;
     int dtorp;
{
  tree tmp = NULL_TREE;

  /* Breadth first search routines start searching basetypes
     of TYPE, so we must perform first ply of search here.  */
  if (dtorp)
    {
      if (tree_has_any_destructor_p (type, 0))
	tmp = get_virtual_destructor (type, 0);

      if (tmp)
	return tmp;

      tmp = (tree) breadth_first_search (type,
					 (pfi) get_virtual_destructor,
					 tree_has_any_destructor_p);
      return tmp;
    }
  else
    {
      tree drettype, dtypes, btypes, instptr_type;
      tree basetype = TYPE_METHOD_BASETYPE (fndecl);
      tree baselink, best = NULL_TREE;
      tree name = DECL_NAME (fndecl);

      declarator = DECL_ORIGINAL_NAME (fndecl);
      if (DECL_VIRTUAL_P (declarator) == 0)
	return 0;

      drettype = TREE_TYPE (TREE_TYPE (fndecl));
      dtypes = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
      if (DECL_STATIC_FUNCTION_P (fndecl))
	instptr_type = NULL_TREE;
      else
	instptr_type = TREE_TYPE (TREE_VALUE (dtypes));

      for (baselink = get_virtuals_named_this (type, 0);
	   baselink; baselink = next_baselink (baselink))
	{
	  for (tmp = TREE_VALUE (baselink); tmp; tmp = TREE_CHAIN (tmp))
	    {
	      if (! DECL_VIRTUAL_P (tmp))
		continue;

	      btypes = TYPE_ARG_TYPES (TREE_TYPE (tmp));
	      if (instptr_type == NULL_TREE
		  && compparms (TREE_CHAIN (btypes), dtypes, 1))
		/* Caller knows to give error in this case.  */
		return tmp;

	      if ((TREE_READONLY (TREE_TYPE (TREE_VALUE (btypes)))
		   == TREE_READONLY (instptr_type))
		  && compparms (TREE_CHAIN (btypes), TREE_CHAIN (dtypes), 1))
		{
		  if (IDENTIFIER_ERROR_LOCUS (name) == NULL_TREE
		      && ! comptypes (TREE_TYPE (TREE_TYPE (tmp)), drettype, 1))
		    {
		      error_with_decl (fndecl, "conflicting return type specified for virtual function `%s'");
		      SET_IDENTIFIER_ERROR_LOCUS (name, basetype);
		    }
		  break;
		}
	    }
	  if (tmp)
	    {
	      /* If this is ambiguous, we will warn about it later.  */
	      if (best)
		{
		  if (get_base_distance (TYPE_METHOD_BASETYPE (TREE_TYPE (best)),
					 TYPE_METHOD_BASETYPE (TREE_TYPE (tmp)), 0, 0) > 0)
		    best = tmp;
		}
	      else
		best = tmp;
	    }
	}
      if (IDENTIFIER_ERROR_LOCUS (name) == NULL_TREE
	  && best == NULL_TREE && warn_overloaded_virtual)
	{
	  error_with_decl (fndecl, "conficting specification deriving virtual function `%s'");
	  SET_IDENTIFIER_ERROR_LOCUS (name, basetype);
	}
      return best;
    }
}

/* Return the list of virtual functions which are abstract in type TYPE.
   This information is cached, and so must be built on a
   non-temporary obstack.  */
tree
get_abstract_virtuals (type)
     tree type;
{
  /* For each layer of base class (i.e., the first base class, and each
     virtual base class from that one), modify the virtual function table
     of the derived class to contain the new virtual function.
     A class has as many vfields as it has virtual base classes (total).  */
  tree vfields, vbases, base, tmp;
  tree vfield = CLASSTYPE_VFIELD (type);
  tree fcontext = vfield ? DECL_FCONTEXT (vfield) : NULL_TREE;
  tree abstract_virtuals = CLASSTYPE_ABSTRACT_VIRTUALS (type);

  for (vfields = CLASSTYPE_VFIELDS (type); vfields; vfields = TREE_CHAIN (vfields))
    {
      int normal;

      /* Find the right base class for this derived class, call it BASE.  */
      base = TREE_VALUE (vfields);
      if (base == type)
	continue;

      /* We call this case NORMAL iff this virtual function table
	 pointer field has its storage reserved in this class.
	 This is normally the case without virtual baseclasses
	 or off-center multiple baseclasses.  */
      normal = (base == fcontext
		&& (TREE_PURPOSE (vfields) == NULL_TREE
		    || ! TREE_VIA_VIRTUAL (TREE_PURPOSE (vfields))));

      if (normal)
	tmp = TREE_CHAIN (CLASS_ASSOC_VIRTUALS (type));
      else
	{
	  tree assoc = assoc_value (base, type);
	  tmp = TREE_CHAIN (ASSOC_VIRTUALS (assoc));
	}

      while (tmp)
	{
	  tree base_pfn = FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (tmp));
	  tree base_fndecl = TREE_OPERAND (base_pfn, 0);
	  if (DECL_ABSTRACT_VIRTUAL_P (base_fndecl))
	    abstract_virtuals = tree_cons (NULL_TREE, base_fndecl, abstract_virtuals);
	  tmp = TREE_CHAIN (tmp);
	}
    }
  for (vbases = CLASSTYPE_VBASECLASSES (type); vbases; vbases = TREE_CHAIN (vbases))
    {
      if (! ASSOC_VIRTUALS (vbases));
	continue;

      base = TREE_TYPE (vbases);
      tmp = TREE_CHAIN (ASSOC_VIRTUALS (vbases));
      while (tmp)
	{
	  tree base_pfn = FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (tmp));
	  tree base_fndecl = TREE_OPERAND (base_pfn, 0);
	  if (DECL_ABSTRACT_VIRTUAL_P (base_fndecl))
	    abstract_virtuals = tree_cons (NULL_TREE, base_fndecl, abstract_virtuals);
	  tmp = TREE_CHAIN (tmp);
	}
    }
  return nreverse (abstract_virtuals);
}

/* For the type TYPE, return a list of member functions available from
   base classes with name NAME.  The TREE_VALUE of the list is a chain of
   member functions with name NAME.  The TREE_PURPOSE of the list is a
   basetype, or a list of base types (in reverse order) which were
   traversed to reach the chain of member functions.  If we reach a base
   type which provides a member function of name NAME, and which has at
   most one base type itself, then we can terminate the search.  */

tree
get_baselinks (type, name)
     tree type, name;
{
  tree hash_tree_cons ();
  int head = 0, tail = 0, index;
  tree rval = 0, nval = 0;
  tree basetypes = CLASSTYPE_AS_LIST (type);

  search_stack = push_search_level (search_stack, &search_obstack);

  while (1)
    {
      int i, n_baselinks = CLASSTYPE_N_BASECLASSES (type);

      /* Process and/or queue base types.  */
      for (i = 1; i <= n_baselinks; i++)
	if (CLASSTYPE_MARKED (CLASSTYPE_BASECLASS (type, i)) == 0)
	  {
	    tree btypes;

	    btypes = hash_tree_cons (CLASSTYPE_VIA_PUBLIC (type, i),
				     CLASSTYPE_VIA_VIRTUAL (type, i),
				     NULL_TREE, CLASSTYPE_BASECLASS (type, i),
				     basetypes);
	    CLASSTYPE_MARKED (CLASSTYPE_BASECLASS (type, i)) = 1;
	    obstack_ptr_grow (&search_obstack, btypes);

	    tail += 1;
	    if (tail >= search_stack->limit)
	      abort ();
	  }

    dont_queue:
      /* Process head of queue, if one exists.  */
      if (head >= tail)
	break;

      basetypes = search_stack->first[head++];
      type = TREE_VALUE (basetypes);
      index = lookup_fnfields_1 (type, name);
      if (index >= 0)
	{
	  nval = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), index);
	  rval = hash_tree_cons (0, 0, basetypes, nval, rval);
	  if (CLASSTYPE_N_BASECLASSES (type) <= 1)
	    {
	      if (CLASSTYPE_BASELINK_VEC (type))
		TREE_TYPE (rval) = TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (type), index);
	      goto dont_queue;
	    }
	}
      nval = NULL_TREE;
    }
  {
    tree *tp = search_stack->first;
    tree *search_tail = tp + tail;

    while (tp < search_tail)
      {
	CLASSTYPE_MARKED (TREE_VALUE (*tp++)) = 0;
      }
  }
  search_stack = pop_search_level (search_stack);
  return rval;
}

tree
next_baselink (baselink)
     tree baselink;
{
  tree tmp = TREE_TYPE (baselink);
  baselink = TREE_CHAIN (baselink);
  while (tmp)
    {
      /* @@ does not yet add previous base types.  */
      baselink = tree_cons (TREE_PURPOSE (tmp), TREE_VALUE (tmp),
			    baselink);
      TREE_TYPE (baselink) = TREE_TYPE (tmp);
      tmp = TREE_CHAIN (tmp);
    }
  return baselink;
}

/* DEPTH-FIRST SEARCH ROUTINES.  */

/* Assign unique numbers to _CLASSTYPE members of the lattice
   specified by TYPE.  The root nodes are marked first; the nodes
   are marked depth-fisrt, left-right.  */

static int cid;

/* Matrix implementing a relation from CLASSTYPE X CLASSTYPE => INT.
   Relation yields 1 if C1 <= C2, 0 otherwise.  */
typedef char mi_boolean;
static mi_boolean *mi_matrix;

/* Type for which this matrix is defined.  */
static tree mi_type;

/* Size of the matrix for indexing purposes.  */
static int mi_size;

/* Return nonzero if class C2 derives from class C1.  */
#define DERIVES_FROM(C1, C2)	\
  ((mi_matrix+mi_size*(CLASSTYPE_CID (C1)-1))[CLASSTYPE_CID (C2)-1])
#define DERIVES_FROM_STAR(C)	\
  (mi_matrix+(CLASSTYPE_CID (C)-1))

/* The main function which implements depth first search.  */
static void
dfs_walk (type, fn, qfn)
     tree type;
     void (*fn)();
     int (*qfn)();
{
  int i, n_baselinks = CLASSTYPE_N_BASECLASSES (type);

  for (i = 1; i <= n_baselinks; i++)
    if ((*qfn)(CLASSTYPE_BASECLASS (type, i)))
      {
	dfs_walk (CLASSTYPE_BASECLASS (type, i), fn, qfn);
      }

  fn (type);
}

/* Predicate functions which serve for dfs_walk.  */
static int numberedp (type) tree type;
{ return CLASSTYPE_CID (type); }
static int unnumberedp (type) tree type;
{ return CLASSTYPE_CID (type) == 0; }

static int markedp (type) tree type;
{ return CLASSTYPE_MARKED (type); }
static int bfs_markedp (type, i) tree type; int i;
{ return CLASSTYPE_MARKED (CLASSTYPE_BASECLASS (type, i)); }
static int unmarkedp (type) tree type;
{ return CLASSTYPE_MARKED (type) == 0; }
static int bfs_unmarkedp (type, i) tree type; int i;
{ return CLASSTYPE_MARKED (CLASSTYPE_BASECLASS (type, i)) == 0; }
static int marked2p (type) tree type;
{ return CLASSTYPE_MARKED2 (type); }
static int bfs_marked2p (type, i) tree type; int i;
{ return CLASSTYPE_MARKED2 (CLASSTYPE_BASECLASS (type, i)); }
static int unmarked2p (type) tree type;
{ return CLASSTYPE_MARKED2 (type) == 0; }
static int bfs_unmarked2p (type, i) tree type; int i;
{ return CLASSTYPE_MARKED2 (CLASSTYPE_BASECLASS (type, i)) == 0; }
static int marked3p (type) tree type;
{ return CLASSTYPE_MARKED3 (type); }
static int bfs_marked3p (type, i) tree type; int i;
{ return CLASSTYPE_MARKED3 (CLASSTYPE_BASECLASS (type, i)); }
static int unmarked3p (type) tree type;
{ return CLASSTYPE_MARKED3 (type) == 0; }
static int bfs_unmarked3p (type, i) tree type; int i;
{ return CLASSTYPE_MARKED3 (CLASSTYPE_BASECLASS (type, i)) == 0; }
static int marked4p (type) tree type;
{ return CLASSTYPE_MARKED4 (type); }
static int bfs_marked4p (type, i) tree type; int i;
{ return CLASSTYPE_MARKED4 (CLASSTYPE_BASECLASS (type, i)); }
static int unmarked4p (type) tree type;
{ return CLASSTYPE_MARKED4 (type) == 0; }
static int bfs_unmarked4p (type, i) tree type; int i;
{ return CLASSTYPE_MARKED4 (CLASSTYPE_BASECLASS (type, i)) == 0; }

static int dfs_search_slot_nonempty_p (type) tree type;
{ return CLASSTYPE_SEARCH_SLOT (type) != 0; }


/* The worker functions for `dfs_walk'.  These do not need to
   test anything (vis a vis marking) if they are paired with
   a predicate function (above).  */

/* Assign each type within the lattice a number which is unique
   in the lattice.  The first number assigned is 1.  */

static void
dfs_number (type)
     tree type;
{
  CLASSTYPE_CID (type) = ++cid;
}

static void
dfs_unnumber (type)
     tree type;
{
  CLASSTYPE_CID (type) = 0;
}

static void
dfs_mark (type) tree type;
{ CLASSTYPE_MARKED (type) = 1; }

static void
dfs_unmark (type) tree type;
{ CLASSTYPE_MARKED (type) = 0; }

static void
dfs_mark2 (type) tree type;
{ CLASSTYPE_MARKED2 (type) = 1; }

static void
dfs_unmark2 (type) tree type;
{ CLASSTYPE_MARKED2 (type) = 0; }

static void
dfs_mark3 (type) tree type;
{ CLASSTYPE_MARKED3 (type) = 1; }

static void
dfs_unmark3 (type) tree type;
{ CLASSTYPE_MARKED3 (type) = 0; }

static void
dfs_mark4 (type) tree type;
{ CLASSTYPE_MARKED4 (type) = 1; }

static void
dfs_unmark4 (type) tree type;
{ CLASSTYPE_MARKED4 (type) = 0; }

static void
dfs_unmark12 (type) tree type;
{ CLASSTYPE_MARKED (type) = 0;
  CLASSTYPE_MARKED2 (type) = 0; }

static void
dfs_unmark34 (type) tree type;
{ CLASSTYPE_MARKED3 (type) = 0;
  CLASSTYPE_MARKED4 (type) = 0; }

static tree
dfs_clear_search_slot (type) tree type;
{ CLASSTYPE_SEARCH_SLOT (type) = 0; }

static tree vbase_types;
static tree vbase_decl, vbase_decl_ptr;
static tree vbase_init_result;

static void
dfs_find_vbases (type)
     tree type;
{
  int i, n_baselinks = CLASSTYPE_N_BASECLASSES (type);

  for (i = n_baselinks; i > 0; i--)
    if (CLASSTYPE_VIA_VIRTUAL (type, i)
	&& CLASSTYPE_SEARCH_SLOT (CLASSTYPE_BASECLASS (type, i)) == 0)
      {
	tree vbase = CLASSTYPE_BASECLASS (type, i);
	/* ??? ASSOC_VALUE and TREE_VALUE must be the same for this to work.  */
	tree assoc = value_member (TYPE_MAIN_VARIANT (vbase), vbase_types);

	CLASSTYPE_SEARCH_SLOT (vbase)
	  = build (PLUS_EXPR, TYPE_POINTER_TO (vbase),
		   vbase_decl_ptr, ASSOC_OFFSET (assoc));
      }
  CLASSTYPE_MARKED3 (type) = 1;
  CLASSTYPE_MARKED4 (type) = 1;
}

static void
dfs_init_vbase_pointers (type)
     tree type;
{
  tree fields = TYPE_FIELDS (type);
  tree path, this_vbase_ptr;
  int distance;

  CLASSTYPE_MARKED3 (type) = 0;

  if (fields == NULL_TREE
      || DECL_NAME (fields) == NULL_TREE
      || ! VBASE_NAME_P (DECL_NAME (fields)))
    return;

  distance = get_base_distance (type, TREE_TYPE (vbase_decl), 0, &path);
  while (path)
    {
      if (TREE_VIA_VIRTUAL (path))
	break;
      distance -= 1;
      path = TREE_CHAIN (path);
    }

  if (distance > 0)
    this_vbase_ptr = convert_pointer_to (type, CLASSTYPE_SEARCH_SLOT (TREE_VALUE (path)));
  else
    this_vbase_ptr = convert_pointer_to (type, vbase_decl_ptr);

  while (fields && DECL_NAME (fields)
	 && VBASE_NAME_P (DECL_NAME (fields)))
    {
      tree ref = build (COMPONENT_REF, TREE_TYPE (fields),
			build_indirect_ref (this_vbase_ptr, 0), fields);
      tree init = CLASSTYPE_SEARCH_SLOT (TREE_TYPE (TREE_TYPE (fields)));
      vbase_init_result = tree_cons (TREE_TYPE (TREE_TYPE (fields)),
				     build_modify_expr (ref, NOP_EXPR, init),
				     vbase_init_result);
      fields = TREE_CHAIN (fields);
    }
}

/* Sometimes this needs to clear both 3 and 4.  Other times,
   just 4, but optimizer should make both with equal efficiency
   (though it does not currently).  */
static void
dfs_clear_vbase_slots (type)
     tree type;
{
  CLASSTYPE_SEARCH_SLOT (type) = 0;
  CLASSTYPE_MARKED3 (type) = 0;
  CLASSTYPE_MARKED4 (type) = 0;
}

tree
init_vbase_pointers (type, decl_ptr)
     tree type;
     tree decl_ptr;
{
  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    {
      int old_flag = flag_this_is_variable;
      flag_this_is_variable = 0;
      vbase_types = CLASSTYPE_VBASECLASSES (type);
      vbase_decl_ptr = decl_ptr;
      vbase_decl = build_indirect_ref (decl_ptr, 0);
      vbase_init_result = NULL_TREE;
#ifdef sparc
      expand_asm_operands (build_string (32, "! start of vbase initialization"), 0, 0, 0, 0, input_filename, lineno);
#endif
      dfs_walk (type, dfs_find_vbases, unmarked3p);
      dfs_walk (type, dfs_init_vbase_pointers, marked3p);
      dfs_walk (type, dfs_clear_vbase_slots, marked4p);
      flag_this_is_variable = old_flag;
      return vbase_init_result;
    }
  return 0;
}

/* Build a COMPOUND_EXPR which when expanded will generate the code
   needed to initialize all the virtual function table slots of all
   the virtual baseclasses.  FOR_TYPE is the type which determines the
   virtual baseclasses to use; TYPE is the type of the object to which
   the initialization applies.  TRUE_EXP is the true object we are
   initializing, and DECL_PTR is the pointer to the sub-object we
   are initializing.

   CTOR_P is non-zero if the caller of this function is a top-level
   constructor.  It is zero when called from a destructor.  When
   non-zero, we can use computed offsets to store the vtables.  When
   zero, we must store new vtables through virtual baseclass pointers.  */

tree
build_vbase_vtables_init (for_type, type, true_exp, decl_ptr, ctor_p)
     tree for_type, type;
     tree true_exp, decl_ptr;
     int ctor_p;
{
  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    {
      int old_flag = flag_this_is_variable;
      tree vtable_init_result = NULL_TREE;
      tree vbases = CLASSTYPE_VBASECLASSES (type);

      vbase_types = CLASSTYPE_VBASECLASSES (for_type);
      vbase_decl_ptr = true_exp ? build_unary_op (ADDR_EXPR, true_exp, 0) : decl_ptr;
      vbase_decl = true_exp ? true_exp : build_indirect_ref (decl_ptr, 0);
#ifdef sparc
      expand_asm_operands (build_string (32, "! start of vtable initialization"), 0, 0, 0, 0, input_filename, lineno);
#endif
      flag_this_is_variable = 0;

      if (ctor_p)
	/* This is an object of type IN_TYPE,  */
	dfs_walk (for_type, dfs_find_vbases, unmarked4p);

      /* Initialized with vtables of type TYPE.  */
      while (vbases)
	{
	  tree basetype = get_base_type (ASSOC_VALUE (vbases), type, 0);

	  /* This time through, not every class's vtable
	     is going to be initialized.  That is, we only initialize
	     the "last" vtable pointer.  */

	  assert (basetype == ASSOC_TYPE (vbases));

	  if (basetype
	      && CLASSTYPE_VSIZE (basetype)
	      && TYPE_MAIN_VARIANT (basetype) == ASSOC_VALUE (vbases))
	    {
	      tree addr;
	      tree vtbl = ASSOC_VTABLE (vbases);
	      tree init = build_unary_op (ADDR_EXPR, vtbl, 0);
	      TREE_USED (vtbl) = 1;

	      if (ctor_p == 0)
		addr = convert_pointer_to (basetype, vbase_decl_ptr);
	      else
		addr = CLASSTYPE_SEARCH_SLOT (basetype);

	      if (addr)
		{
		  tree ref = build_vfield_ref (build_indirect_ref (addr, 0), basetype);
		  init = convert_force (TREE_TYPE (ref), init);
		  vtable_init_result = tree_cons (NULL_TREE, build_modify_expr (ref, NOP_EXPR, init),
						  vtable_init_result);
		}
	    }
	  vbases = TREE_CHAIN (vbases);
	}

      dfs_walk (type, dfs_clear_vbase_slots, marked4p);

      flag_this_is_variable = old_flag;
      if (vtable_init_result)
	return build_compound_expr (vtable_init_result);
    }
  return error_mark_node;
}

tree
clear_search_slots (type)
     tree type;
{
  dfs_walk (type, dfs_clear_search_slot, dfs_search_slot_nonempty_p);
}

static void
dfs_get_vbase_types (type)
     tree type;
{
  int i;
  tree these_vbase_types = CLASSTYPE_VBASECLASSES (type);
  tree basetype;

  if (these_vbase_types)
    {
      while (these_vbase_types)
	{
	  basetype = ASSOC_TYPE (these_vbase_types);
	  if (! CLASSTYPE_MARKED2 (basetype))
	    {
	      vbase_types = make_assoc (integer_zero_node,
					basetype,
					CLASS_ASSOC_VTABLE (basetype),
					CLASS_ASSOC_VIRTUALS (basetype),
					vbase_types);
	      CLASSTYPE_MARKED2 (basetype) = 1;
	    }
	  these_vbase_types = TREE_CHAIN (these_vbase_types);
	}
    }
  else for (i = CLASSTYPE_N_BASECLASSES (type); i > 0; i--)
    {
      basetype = CLASSTYPE_BASECLASS (type, i);
      if (CLASSTYPE_VIA_VIRTUAL (type, i) && ! CLASSTYPE_MARKED2 (basetype))
	{
	  vbase_types = make_assoc (integer_zero_node,
				    basetype,
				    CLASS_ASSOC_VTABLE (basetype),
				    CLASS_ASSOC_VIRTUALS (basetype),
				    vbase_types);
	  CLASSTYPE_MARKED2 (basetype) = 1;
	}
    }
  CLASSTYPE_MARKED (type) = 1;
}

/* Some virtual baseclasses might be virtual baseclasses for
   other virtual baseclasses.  We sort the virtual baseclasses
   topologically: in the list returned, the first virtual base
   classes have no virtual baseclasses themselves, and any entry
   on the list has no dependency on virtual base classes later in the
   list.  */
tree
get_vbase_types (type)
     tree type;
{
  tree ordered_vbase_types = NULL_TREE, prev, next;
  tree vbases;

  vbase_types = NULL_TREE;
  dfs_walk (type, dfs_get_vbase_types, unmarkedp);
  dfs_walk (type, dfs_unmark, markedp);

  while (vbase_types)
    {
      /* Now sort these types.  This is essentially a bubble merge.  */

      /* Farm out virtual baseclasses which have no marked ancestors.  */
      for (vbases = vbase_types, prev = NULL_TREE;
	   vbases; vbases = next)
	{
	  next = TREE_CHAIN (vbases);
	  if (! TYPE_USES_VIRTUAL_BASECLASSES (ASSOC_TYPE (vbases))
	      || CLASSTYPE_MARKED2 (ASSOC_TYPE (vbases)) == 0)
	    {
	      if (prev)
		TREE_CHAIN (prev) = TREE_CHAIN (vbases);
	      else
		vbase_types = TREE_CHAIN (vbases);
	      TREE_CHAIN (vbases) = NULL_TREE;
	      ordered_vbase_types = chainon (ordered_vbase_types, vbases);
	      CLASSTYPE_MARKED2 (ASSOC_TYPE (vbases)) = 0;
	    }
	  else
	    prev = vbases;
	}

      /* Now unmark types all of whose ancestors are now on the
	 `ordered_vbase_types' list.  */
      for (vbases = vbase_types; vbases; vbases = TREE_CHAIN (vbases))
	{
	  /* If all our virtual baseclasses are unmarked, ok.  */
	  tree t = CLASSTYPE_VBASECLASSES (ASSOC_VALUE (vbases));
	  while (t && (CLASSTYPE_MARKED2 (ASSOC_TYPE (t)) == 0
		       || !TYPE_USES_VIRTUAL_BASECLASSES (ASSOC_TYPE (t))))
	    t = TREE_CHAIN (t);
	  if (t == NULL_TREE)
	    CLASSTYPE_MARKED2 (ASSOC_TYPE (vbases)) = 0;
	}
    }

  return ordered_vbase_types;
}

static void
dfs_record_inheritance (type)
     tree type;
{
  int i, n_baselinks = CLASSTYPE_N_BASECLASSES (type);
  mi_boolean *derived_row = DERIVES_FROM_STAR (type);

  for (i = n_baselinks; i > 0; i--)
    {
      int j;
      tree baseclass = TYPE_MAIN_VARIANT (CLASSTYPE_BASECLASS (type, i));
      mi_boolean *base_row = DERIVES_FROM_STAR (baseclass);

      /* Don't search if there's nothing there!  MI_SIZE can be
	 zero as a result of parse errors.  */
      if (CLASSTYPE_N_BASECLASSES (baseclass) && mi_size > 0)
	for (j = mi_size*(CLASSTYPE_CID (baseclass)-1); j >= 0; j -= mi_size)
	  derived_row[j] |= base_row[j];
      DERIVES_FROM (baseclass, type) = 1;
    }

  CLASSTYPE_MARKED (type) = 1;
}

/* Given a _CLASSTYPE node in a multiple inheritance lattice,
   convert the lattice into a simple relation such that,
   given to CIDs, C1 and C2, one can determine if C1 <= C2
   or C2 <= C1 or C1 <> C2.

   Once constructed, we walk the lattice depth fisrt,
   applying various functions to elements as they are encountered.

   We use malloc here, in case we want to randomly free these tables.  */

#define SAVE_MI_MATRIX

void
build_mi_matrix (type)
     tree type;
{
  cid = 0;

#ifdef SAVE_MI_MATRIX
  if (CLASSTYPE_MI_MATRIX (type))
    {
      mi_size = CLASSTYPE_N_SUPERCLASSES (type) + CLASSTYPE_N_VBASECLASSES (type);
      mi_matrix = CLASSTYPE_MI_MATRIX (type);
      mi_type = type;
      dfs_walk (type, dfs_number, unnumberedp);
      return;
    }
#endif

  mi_size = CLASSTYPE_N_SUPERCLASSES (type) + CLASSTYPE_N_VBASECLASSES (type);
  mi_matrix = (char *)malloc ((mi_size+1) * (mi_size+1));
  mi_type = type;
  bzero (mi_matrix, mi_size * mi_size);
  dfs_walk (type, dfs_number, unnumberedp);
  dfs_walk (type, dfs_record_inheritance, unmarkedp);
  dfs_walk (type, dfs_unmark, markedp);
}

void
free_mi_matrix ()
{
  dfs_walk (mi_type, dfs_unnumber, numberedp);

#ifdef SAVE_MI_MATRIX
  CLASSTYPE_MI_MATRIX (mi_type) = mi_matrix;
#else
  free (mi_matrix);
  mi_size = 0;
  cid = 0;
#endif
}

/* Local variables for detecting ambiguities of virtual functions
   when two or more classes are joined at a multiple inheritance
   seam.  */
typedef tree mi_ventry[3];
static mi_ventry *mi_vmatrix;
static int *mi_vmax;
static int mi_vrows, mi_vcols;
#define MI_VMATRIX(ROW,COL) ((mi_vmatrix + (ROW)*mi_vcols)[COL])

/* Build a table of virtual functions for a multiple-inheritance
   structure.  Here, there are N base classes, and at most
   M entries per class.

   This function does nothing if N is 0 or 1.  */
void
build_mi_virtuals (rows, cols)
     int rows, cols;
{
  if (rows < 2)
    return;
  mi_vrows = rows;
  mi_vcols = cols;
  mi_vmatrix = (mi_ventry *)malloc ((rows+1) * cols * sizeof (mi_ventry));
  mi_vmax = (int *)malloc ((rows+1) * sizeof (int));

  bzero (mi_vmax, rows * sizeof (int));

  /* Row indicies start at 1, so adjust this.  */
  mi_vmatrix -= cols;
  mi_vmax -= 1;
}

/* Comparison function for ordering virtual function table entries.  */
static int
rank_mi_virtuals (v1, v2)
     mi_ventry *v1, *v2;
{
  tree p1, p2;
  int i;

  i = (TREE_UID (DECL_ORIGINAL_NAME ((*v1)[0]))
       - TREE_UID (DECL_ORIGINAL_NAME ((*v2)[0])));
  if (i)
    return i;
  p1 = (*v1)[1];
  p2 = (*v2)[1];

  if (p1 == p2)
    return 0;

  while (p1 && p2)
    {
      i = (TREE_UID (TREE_VALUE (p1))
	   - TREE_UID (TREE_VALUE (p2)));
      if (i)
	return i;

      if (TREE_CHAIN (p1))
	{
	  if (! TREE_CHAIN (p2))
	    return 1;
	  p1 = TREE_CHAIN (p1);
	  p2 = TREE_CHAIN (p2);
	}
      else if (TREE_CHAIN (p2))
	return -1;
      else
	{
	  /* When matches of argument lists occur, pick lowest
	     TREE_UID to keep searching time to a minimum on
	     later passes--like hashing, only different.
	     *MUST BE STABLE*.  */
	  if (TREE_UID ((*v2)[1]) < TREE_UID ((*v1)[1]))
	    (*v1)[1] = (*v2)[1];
	  else
	    (*v2)[1] = (*v1)[1];
	  return 0;
	}
    }
  return 0;
}

/* Install the virtuals functions got from the initializer VIRTUALS to
   the table at index ROW.  */
void
add_mi_virtuals (row, virtuals)
     int row;
     tree virtuals;
{
  int col = 0;

  if (mi_vmatrix == 0)
    return;
  while (virtuals)
    {
      tree decl = TREE_OPERAND (FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals)), 0);
      MI_VMATRIX (row, col)[0] = decl;
      MI_VMATRIX (row, col)[1] = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (decl)));
      MI_VMATRIX (row, col)[2] = TREE_VALUE (virtuals);
      virtuals = TREE_CHAIN (virtuals);
      col += 1;
    }
  mi_vmax[row] = col;

  qsort (mi_vmatrix + row * mi_vcols,
	 col,
	 sizeof (mi_ventry),
	 rank_mi_virtuals);
}

/* If joining two types results in an ambiguity in the virtual
   function table, report such here.  */
void
report_ambiguous_mi_virtuals (rows, type)
     int rows;
     tree type;
{
  int *mi_vmin;
  int row1, col1, row, col;

  if (mi_vmatrix == 0)
    return;

  /* Now virtuals are all sorted, so we merge to find ambiguous cases.  */
  mi_vmin = (int *)alloca ((rows+1) * sizeof (int));
  bzero (mi_vmin, rows * sizeof (int));

  /* adjust.  */
  mi_vmin -= 1;

  /* For each base class with virtual functions (and this includes views
     of the virtual baseclasses from different base classes), see that
     each virtual function in that base class has a unique meet.

     When the column loop is finished, THIS_DECL is in fact the meet.
     If that value does not appear in the virtual function table for
     the row, install it.  This happens when that virtual function comes
     from a virtual baseclass, or a non-leftmost baseclass.  */
     
  for (row1 = 1; row1 < rows; row1++)
    {
      tree this_decl = 0;

      for (col1 = mi_vmax[row1]-1; col1 >= mi_vmin[row1]; col1--)
	{
	  tree these_args = MI_VMATRIX (row1, col1)[1];
	  tree this_context;

	  this_decl = MI_VMATRIX (row1, col1)[0];
	  if (this_decl == 0)
	    continue;
	  this_context = DECL_CONTEXT (this_decl);

	  if (this_context != type)
	    this_context = get_base_type (this_context, type, 0);

	  for (row = row1+1; row <= rows; row++)
	    for (col = mi_vmax[row]-1; col >= mi_vmin[row]; col--)
	      {
		mi_ventry this_entry;

		if (MI_VMATRIX (row, col)[0] == 0)
		  continue;

		this_entry[0] = this_decl;
		this_entry[1] = these_args;
		this_entry[2] = MI_VMATRIX (row1, col1)[2];
		if (rank_mi_virtuals (&this_entry,
				      &MI_VMATRIX (row, col)) == 0)
		  {
		    /* They are equal.  There are four possibilities:
		       
		       (1) Derived class is defining this virtual function.
		       (2) Two paths to the same virtual function in the
		       same base class.
		       (3) A path to a virtual function declared in one base
		       class, and another path to a virtual function in a
		       base class of the base class.
		       (4) Two paths to the same virtual function in different
		       base classes.
		       
		       The first three cases are ok (non-ambiguous).  */

		    tree that_context, tmp;
		    int this_before_that;

		    if (type == this_context)
		      /* case 1.  */
		      goto ok;
		    that_context = get_base_type (DECL_CONTEXT (MI_VMATRIX (row, col)[0]), type, 0);
		    if (that_context == this_context)
		      /* case 2.  */
		      goto ok;
		    if (that_context != NULL_TREE)
		      {
			tmp = get_base_type (that_context, this_context, 0);
			this_before_that = (that_context != tmp);
			if (this_before_that == 0)
			  /* case 3a.  */
			  goto ok;
			tmp = get_base_type (this_context, that_context, 0);
			this_before_that = (this_context == tmp);
			if (this_before_that != 0)
			  /* case 3b.  */
			  goto ok;

			/* case 4.  */
			error_with_decl (MI_VMATRIX (row, col)[0], "ambiguous virtual function `%s'");
			error_with_decl (this_decl, "ambiguating function `%s' (joined by type `%s')", IDENTIFIER_POINTER (current_class_name));
		      }
		  ok:
		    MI_VMATRIX (row, col)[0] = 0;

		    /* Let zeros propagate.  */
		    if (col == mi_vmax[row]-1)
		      {
			int i = col;
			while (i >= mi_vmin[row]
			       && MI_VMATRIX (row, i)[0] == 0)
			  i--;
			mi_vmax[row] = i;
		      }
		    else if (col == mi_vmin[row])
		      {
			int i = col;
			while (i < mi_vmax[row]
			       && MI_VMATRIX (row, i)[0] == 0)
			  i++;
			mi_vmin[row] = i;
		      }
		  }
	      }
	}
    }
  free (mi_vmatrix + mi_vcols);
  mi_vmatrix = 0;
  free (mi_vmax + 1);
  mi_vmax = 0;
}

/* Subroutines of push_class_decls ().  */

/* Add the instance variables which this class contributed to the
   current class binding contour.  When a redefinition occurs,
   if the redefinition is strictly within a single inheritance path,
   we just overwrite (in the case of a data field) or
   cons (in the case of a member function) the old declaration with
   the new.  If the fields are not within a single inheritance path,
   we must cons them in either case.  */

static void
dfs_pushdecls (type)
     tree type;
{
  tree fields, *methods, *end;
  tree method_vec;

  for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
    {
      /* Unmark so that if we are in a constructor, and then find that
	 this field was initialized by a base initializer,
	 we can emit an error message.  */
      if (TREE_CODE (fields) == FIELD_DECL)
	TREE_USED (fields) = 0;

      if (DECL_ANON_UNION_ELEM (fields))
	{
	  dfs_pushdecls (TREE_TYPE (fields));
	  continue;
	}
      if (TREE_CODE (fields) != TYPE_DECL)
	{
	  TREE_FIELD_PUBLIC (fields) = 0;
	  TREE_FIELD_PROTECTED (fields) = 0;
	  TREE_FIELD_PRIVATE (fields) = 0;
	}

      if (DECL_NAME (fields))
	{
	  tree value = IDENTIFIER_CLASS_VALUE (DECL_NAME (fields));
	  if (value)
	    {
	      /* Possible ambiguity.  If its defining type(s)
		 is (are all) derived from us, no problem.  */

	      if (TREE_CODE (value) != TREE_LIST)
		{
		  if (DECL_FIELD_CONTEXT (value) == type
		      || DERIVES_FROM (DECL_FIELD_CONTEXT (value), type))
		    value = fields;
		  else
		    value = tree_cons (NULL_TREE, fields,
				       build_tree_list (NULL_TREE, value));
		}
	      else
		{
		  /* All children may derive from us, in which case
		     there is no problem.  Otherwise, we have to
		     keep lists around of what the ambiguities might be.  */
		  tree values;
		  int problem = 0;

		  for (values = value; values; values = TREE_CHAIN (values))
		    {
		      tree sub_values = TREE_VALUE (values);
		      if (TREE_CODE (sub_values) == TREE_LIST)
			{
			  for (; sub_values; sub_values = TREE_CHAIN (sub_values))
			    if (! DERIVES_FROM (DECL_FIELD_CONTEXT (TREE_VALUE (sub_values)), type))
			      {
				value = tree_cons (NULL_TREE, TREE_VALUE (values), value);
				problem = 1;
				break;
			      }
			}
		      else
			{
			  if (! DERIVES_FROM (DECL_FIELD_CONTEXT (sub_values), type))
			    {
			      value = tree_cons (NULL_TREE, values, value);
			      problem = 1;
			      break;
			    }
			}
		    }
		  if (! problem) value = fields;
		}

	      /* Mark this as a potentially ambiguous member.  */
	      if (TREE_CODE (value) == TREE_LIST)
		{
		  /* Leaving TREE_TYPE blank is intentional.
		     We cannot use `error_mark_node' (lookup_name)
		     or `unknown_type_node' (all member functions use this).  */
		  TREE_NONLOCAL (value) = 1;
		}

	      IDENTIFIER_CLASS_VALUE (DECL_NAME (fields)) = value;
	    }
	  else IDENTIFIER_CLASS_VALUE (DECL_NAME (fields)) = fields;
	}
    }

  method_vec = CLASSTYPE_METHOD_VEC (type);
  if (method_vec != 0)
    {
      /* Farm out constructors and destructors.  */
      methods = &TREE_VEC_ELT (method_vec, 1);
      end = TREE_VEC_END (method_vec);

      /* This does not work for multiple inheritance yet.  */
      while (methods != end)
	{
	  /* This will cause lookup_name to return a pointer
	     to the tree_list of possible methods of this name.
	     If the order is a problem, we can nreverse them.  */
	  tree tmp;
	  tree old = IDENTIFIER_CLASS_VALUE (DECL_ORIGINAL_NAME (*methods));

	  if (old && TREE_CODE (old) == TREE_LIST)
	    tmp = tree_cons (DECL_ORIGINAL_NAME (*methods), *methods, old);
	  else
	    {
	      /* Only complain if we shadow something we can access.  */
	      if (old && (DECL_CONTEXT (old) == current_class_type
			  || ! TREE_PRIVATE (old)))
		/* Should figure out visibility more accurately.  */
		warning ("shadowing member `%s' with member function",
			 IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (*methods)));
	      tmp = build_tree_list (DECL_ORIGINAL_NAME (*methods), *methods);
	    }

	  TREE_TYPE (tmp) = unknown_type_node;
#if 0
	  TREE_OVERLOADED (tmp) = DECL_OVERLOADED (*methods);
#endif
	  TREE_NONLOCAL (tmp) = 1;
	  IDENTIFIER_CLASS_VALUE (DECL_ORIGINAL_NAME (*methods)) = tmp;

	  tmp = *methods;
	  while (tmp != 0)
	    {
	      TREE_FIELD_PUBLIC (tmp) = 0;
	      TREE_FIELD_PROTECTED (tmp) = 0;
	      TREE_FIELD_PRIVATE (tmp) = 0;
	      tmp = TREE_CHAIN (tmp);
	    }

	  methods++;
	}
    }
  CLASSTYPE_MARKED (type) = 1;
}

/* Consolidate unique (by name) member functions.  */
static void
dfs_compress_decls (type)
     tree type;
{
  tree method_vec = CLASSTYPE_METHOD_VEC (type);

  if (method_vec != 0)
    {
      /* Farm out constructors and destructors.  */
      tree *methods = &TREE_VEC_ELT (method_vec, 1);
      tree *end = TREE_VEC_END (method_vec);

      for (; methods != end; methods++)
	{
	  tree tmp = IDENTIFIER_CLASS_VALUE (DECL_ORIGINAL_NAME (*methods));

	  /* This was replaced in scope by somebody else.  Just leave it
	     alone.  */
	  if (TREE_CODE (tmp) != TREE_LIST)
	    continue;

	  if (TREE_CHAIN (tmp) == NULL_TREE
	      && TREE_VALUE (tmp)
	      && TREE_CHAIN (TREE_VALUE (tmp)) == NULL_TREE)
	    {
	      IDENTIFIER_CLASS_VALUE (DECL_ORIGINAL_NAME (*methods))
		= TREE_VALUE (tmp);
	    }
	}
    }
  CLASSTYPE_MARKED (type) = 0;
}

/* When entering the scope of a class, we cache all of the
   fields that that class provides within its inheritance
   lattice.  Where ambiguities result, we mark them
   with `error_mark_node' so that if they are encountered
   without explicit qualification, we can emit an error
   message.  */
void
push_class_decls (type)
     tree type;
{
  struct obstack *ambient_obstack = current_obstack;

#if 0
  tree tags = CLASSTYPE_TAGS (type);

  while (tags)
    {
      tree code_type_node;
      tree tag;

      switch (TREE_CODE (TREE_VALUE (tags)))
	{
	case ENUMERAL_TYPE:
	  code_type_node = enum_type_node;
	  break;
	case RECORD_TYPE:
	  code_type_node = record_type_node;
	  break;
	case CLASS_TYPE:
	  code_type_node = class_type_node;
	  break;
	case UNION_TYPE:
	  code_type_node = union_type_node;
	  break;
	default:
	  assert (0);
	}
      tag = xref_tag (code_type_node, TREE_PURPOSE (tags),
		      CLASSTYPE_BASECLASS (TREE_VALUE (tags), 1));
      pushdecl (build_decl (TYPE_DECL, TREE_PURPOSE (tags), TREE_VALUE (tags)));
    }
#endif

  current_obstack = &bridge_obstack;
  search_stack = push_search_level (search_stack, &bridge_obstack);

  /* Push class fields into CLASS_VALUE scope, and mark.  */
  dfs_walk (type, dfs_pushdecls, unmarkedp);

  /* Compress fields which have only a single entry
     by a given name, and unmark.  */
  dfs_walk (type, dfs_compress_decls, markedp);
  current_obstack = ambient_obstack;
}

static void
dfs_popdecls (type)
     tree type;
{
  tree fields = TYPE_FIELDS (type);
  tree method_vec = CLASSTYPE_METHOD_VEC (type);

  while (fields)
    {
      if (DECL_ANON_UNION_ELEM (fields))
	{
	  dfs_popdecls (TREE_TYPE (fields));
	}
      else if (DECL_NAME (fields))
	IDENTIFIER_CLASS_VALUE (DECL_NAME (fields)) = NULL_TREE;
      fields = TREE_CHAIN (fields);
    }
  if (method_vec != 0)
    {
      tree *methods = &TREE_VEC_ELT (method_vec, 0);
      tree *end = TREE_VEC_END (method_vec);

      if (*methods == 0)
	methods += 1;

      for (; methods != end; methods++)
	IDENTIFIER_CLASS_VALUE (DECL_ORIGINAL_NAME (*methods)) = NULL_TREE;
    }

  CLASSTYPE_MARKED (type) = 1;
}

void
pop_class_decls (type)
     tree type;
{
  /* Clear out the IDENTIFIER_CLASS_VALUE which this
     class may have occupied, and mark.  */
  dfs_walk (type, dfs_popdecls, unmarkedp);

  /* Unmark.  */
  dfs_walk (type, dfs_unmark, markedp);
  search_stack = pop_search_level (search_stack);
}

/* Given a base type PARENT, and a derived type TYPE, build
   a name which distinguishes exactly the PARENT member of TYPE's type.

   FORMAT is a string which controls how sprintf formats the name
   we have generated.

   For example, given

	class A; class B; class C : A, B;

   it is possible to distinguish "A" from "C's A".  And given

	class L;
	class A : L; class B : L; class C : A, B;

   it is possible to distinguish "L" from "A's L", and also from
   "C's L from A".  */
tree
build_type_pathname (format, parent, type)
     char *format;
     tree parent, type;
{
  extern struct obstack temporary_obstack;
  char *first, *base, *name;
  int i;
  tree id;

  parent = TYPE_MAIN_VARIANT (parent);

  /* Remember where to cut the obstack to.  */
  first = obstack_base (&temporary_obstack);

  /* Put on TYPE+PARENT.  */
  obstack_grow (&temporary_obstack,
		TYPE_NAME_STRING (type), TYPE_NAME_LENGTH (type));
  obstack_1grow (&temporary_obstack, JOINER);
  obstack_grow0 (&temporary_obstack,
		 TYPE_NAME_STRING (parent), TYPE_NAME_LENGTH (parent));
  i = obstack_object_size (&temporary_obstack);
  base = obstack_base (&temporary_obstack);
  obstack_finish (&temporary_obstack);

  /* Put on FORMAT+TYPE+PARENT.  */
  obstack_blank (&temporary_obstack, strlen (format) + i + 1);
  name = obstack_base (&temporary_obstack);
  sprintf (name, format, base);
  id = get_identifier (name);
  obstack_free (&temporary_obstack, first);

  return id;
}

static int
bfs_unmark_finished_struct (type, i)
     tree type;
     int i;
{
  type = i == 0 ? type : CLASSTYPE_BASECLASS (type, i);
  if (CLASSTYPE_MARKED4 (type))
    {
      tree assoc, decl, context;

      if (type == current_class_type)
	assoc = CLASSTYPE_ASSOC (type);
      else if (TREE_VIA_VIRTUAL (type))
	assoc = value_member (TYPE_MAIN_VARIANT (type), CLASSTYPE_VBASECLASSES (current_class_type));
      else
	assoc = assoc_value (TYPE_MAIN_VARIANT (type), current_class_type);
      decl = ASSOC_VTABLE (assoc);
      context = DECL_CONTEXT (decl);
      DECL_CONTEXT (decl) = 0;
      if (write_virtuals >= 0
	  && DECL_INITIAL (decl) != ASSOC_VIRTUALS (assoc))
	DECL_INITIAL (decl) = build_nt (CONSTRUCTOR, NULL_TREE,
					ASSOC_VIRTUALS (assoc));
      finish_decl (decl, DECL_INITIAL (decl), NULL_TREE);
      DECL_CONTEXT (decl) = context;
    }
  CLASSTYPE_MARKED3 (type) = 0;
  CLASSTYPE_MARKED4 (type) = 0;
  return 0;
}

void
unmark_finished_struct (type)
     tree type;
{
  bfs_unmark_finished_struct (type, 0);
  breadth_first_search (type, bfs_unmark_finished_struct, bfs_marked3p);
}

void
print_search_statistics ()
{
#ifdef GATHER_STATISTICS
  if (flag_memoize_lookups)
    {
      fprintf (stderr, "%d memoized contexts saved\n",
	       n_contexts_saved);
      fprintf (stderr, "%d local tree nodes made\n", my_tree_node_counter);
      fprintf (stderr, "%d local hash nodes made\n", my_memoized_entry_counter);
      fprintf (stderr, "fields statistics:\n");
      fprintf (stderr, "  memoized finds = %d; rejects = %d; (searches = %d)\n",
	       memoized_fast_finds[0], memoized_fast_rejects[0],
	       memoized_fields_searched[0]);
      fprintf (stderr, "  memoized_adds = %d\n", memoized_adds[0]);
      fprintf (stderr, "fnfields statistics:\n");
      fprintf (stderr, "  memoized finds = %d; rejects = %d; (searches = %d)\n",
	       memoized_fast_finds[1], memoized_fast_rejects[1],
	       memoized_fields_searched[1]);
      fprintf (stderr, "  memoized_adds = %d\n", memoized_adds[1]);
    }
  fprintf (stderr, "%d fields searched in %d[%d] calls to lookup_field[_1]\n",
	   n_fields_searched, n_calls_lookup_field, n_calls_lookup_field_1);
  fprintf (stderr, "%d fnfields searched in %d calls to lookup_fnfields\n",
	   n_outer_fields_searched, n_calls_lookup_fnfields);
  fprintf (stderr, "%d calls to get_base_type\n", n_calls_get_base_type);
#else
  fprintf (stderr, "no search statistics\n");
#endif
}

void
init_search_processing ()
{
  obstack_init (&search_obstack);
  obstack_init (&type_obstack);
  obstack_init (&type_obstack_entries);
  obstack_init (&bridge_obstack);

  /* This gives us room to build our chains of basetypes,
     whether or not we decide to memoize them.  */
  type_stack = push_type_level (0, &type_obstack);
  _vptr_name = get_identifier ("_vptr");
}

tree
get_wrapper (type)
     tree type;
{
  tree wrap_type;
  char *name;
  assert (IS_AGGR_TYPE (type));
  wrap_type = TYPE_WRAP_TYPE (type);
  name = (char *)alloca (TYPE_NAME_LENGTH (wrap_type)
			 + strlen (WRAPPER_NAME_FORMAT));
  sprintf (name, WRAPPER_NAME_FORMAT, TYPE_NAME_STRING (wrap_type));
  return lookup_fnfields (CLASSTYPE_AS_LIST (wrap_type),
			  get_identifier (name), 0);
}

void
reinit_search_statistics ()
{
  my_memoized_entry_counter = 0;
  memoized_fast_finds[0] = 0;
  memoized_fast_finds[1] = 0;
  memoized_adds[0] = 0;
  memoized_adds[1] = 0;
  memoized_fast_rejects[0] = 0;
  memoized_fast_rejects[1] = 0;
  memoized_fields_searched[0] = 0;
  memoized_fields_searched[1] = 0;
  n_fields_searched = 0;
  n_calls_lookup_field = 0, n_calls_lookup_field_1 = 0;
  n_calls_lookup_fnfields = 0, n_calls_lookup_fnfields_1 = 0;
  n_calls_get_base_type = 0;
  n_outer_fields_searched = 0;
  n_contexts_saved = 0;
}
