/* Language-depednent node constructors for parse phase of GNU compiler.
   Copyright (C) 1987, 1988 Free Software Foundation, Inc.
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

#include "config.h"
#include <stdio.h>
#include "obstack.h"
#include "tree.h"
#include "cplus-tree.h"
#include "flags.h"
#include "assert.h"

#define MAX(x,y) ((x) > (y) ? (x) : (y))
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#define CEIL(x,y) (((x) + (y) - 1) / (y))

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

      case VAR_DECL:
	if (TREE_READONLY (ref) && ! TREE_STATIC (ref)
	    && DECL_LANG_SPECIFIC (ref)
	    && DECL_IN_AGGR_P (ref))
	  return 0;
      case INDIRECT_REF:
      case ARRAY_REF:
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
	if (TREE_CODE (TREE_TYPE (ref)) == REFERENCE_TYPE
	    /* unary_complex_lvalue knows how to deal with this case.  */
	    || TREE_ADDRESSABLE (TREE_TYPE (ref)))
	  return 1;
	break;

	/* A currently unresolved scope ref.  */
      case SCOPE_REF:
	abort ();
      case OFFSET_REF:
	if (TREE_CODE (TREE_OPERAND (ref, 1)) == FUNCTION_DECL)
	  return 1;
	if (TREE_CODE (TREE_OPERAND (ref, 1)) == VAR_DECL)
	  if (TREE_READONLY (ref) && ! TREE_STATIC (ref)
	      && DECL_LANG_SPECIFIC (ref)
	      && DECL_IN_AGGR_P (ref))
	    return 0;
	  else
	    return 1;
	break;
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

/* INIT is a CALL_EXPR which needs info about its target.
   TYPE is the type that this initialization should appear to have.

   Build an encapsultation of the initialization to perfom
   and return it so that it can be processed by language-independent
   and language-specific expression expanders.  */
tree
build_cplus_new (type, init)
     tree type;
     tree init;
{
  tree slot = build (VAR_DECL, type);
  tree rval = build (CPLUS_NEW_EXPR, type,
		     TREE_OPERAND (init, 0), TREE_OPERAND (init, 1), slot);
  TREE_ADDRESSABLE (rval) = 1;
  rval = build (NEW_EXPR, type, slot, rval, 0);
  TREE_ADDRESSABLE (rval) = 1;
  return rval;
}

extern struct obstack *current_obstack;
extern struct obstack permanent_obstack, class_obstack;
extern struct obstack *saveable_obstack;

/* Return a type like TYPE except that its CLASSTYPE_OFFSET
   is OFFSET.

   Such variant types already made are recorded so that duplicates
   are not made.

   A variant types should never be used as the type of an expression.
   Use TYPE_MAIN_VARIANT to find the main variant.  */

tree
build_classtype_variant (type, offset, virtualp)
     tree type;
     tree offset;
     int virtualp;
{
  register tree t, m = CLASSTYPE_MAIN_VARIANT (type);
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;
  register int lo = TREE_INT_CST_LOW (offset);
  register int hi = TREE_INT_CST_HIGH (offset);
  /* First search the chain variants for one that is what we want.  */

  if (hi == 0 && lo == 0)
    offset = integer_zero_node;

  for (t = m; t; t = CLASSTYPE_NEXT_VARIANT (t))
    if (virtualp == TREE_VIA_VIRTUAL (t)
	&& lo == TREE_INT_CST_LOW (CLASSTYPE_OFFSET (t))
	&& hi == TREE_INT_CST_HIGH (CLASSTYPE_OFFSET (t)))
      return t;

  /* We need a new one.  */
  if (TREE_PERMANENT (type))
    saveable_obstack = &permanent_obstack;
  current_obstack = saveable_obstack;

  t = copy_node (type);
  copy_type_lang_specific (t);
  CLASSTYPE_AS_LIST (t) = build_tree_list (NULL_TREE, t);

  TYPE_POINTER_TO (t) = 0;
  TYPE_REFERENCE_TO (t) = 0;
  CLASSTYPE_OFFSET (t) = offset;
  TREE_VIA_VIRTUAL (t) = virtualp;

  /* Always promise to have TYPE_POINTER_TO filled in.  */
  build_pointer_type (t);

  /* Add this type to the chain of variants of TYPE.  */
  CLASSTYPE_NEXT_VARIANT (t) = CLASSTYPE_NEXT_VARIANT (m);
  CLASSTYPE_NEXT_VARIANT (m) = t;

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
  return t;
}

/* Here is how primitive or already-canonicalized types' hash
   codes are made.  MUST BE CONSISTENT WITH tree.c !!! */
#define TYPE_HASH(TYPE) TREE_UID (TYPE)

/* Construct, lay out and return the type of methods belonging to class
   BASETYPE and whose arguments and values are described by TYPE.
   If that type exists already, reuse it.
   TYPE must be a FUNCTION_TYPE node.  */

tree
build_cplus_method_type (basetype, rettype, argtypes)
     tree basetype, rettype, argtypes;
{
  register tree t;
  tree ptype = build_pointer_type (basetype);
  int hashcode;

  /* Make a node of the sort we want.  */
  t = make_node (METHOD_TYPE);

  TYPE_METHOD_BASETYPE (t) = TYPE_MAIN_VARIANT (basetype);
  TREE_TYPE (t) = rettype;
  ptype = build_type_variant (ptype, !flag_this_is_variable, 0);

  /* The actual arglist for this function includes a "hidden" argument
     which is "this".  Put it into the list of argument types.  */

  TYPE_ARG_TYPES (t) = tree_cons (NULL, ptype, argtypes);

  /* If we already have such a type, use the old one and free this one.
     Note that it also frees up the above cons cell if found.  */
  hashcode = TYPE_HASH (basetype) + TYPE_HASH (rettype) + type_hash_list (argtypes);
  t = type_hash_canon (hashcode, t);

  if (TYPE_SIZE (t) == 0)
    layout_type (t);

  return t;
}

tree
build_cplus_array_type (elt_type, index_type)
     tree elt_type;
     tree index_type;
{
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;
  tree t;

  /* We need a new one.  If ELT_TYPE is permanent, make this permanent too.  */
  if (TREE_PERMANENT (elt_type))
    {
      current_obstack = &permanent_obstack;
      saveable_obstack = &permanent_obstack;
    }

  t = build_array_type (elt_type, index_type);

  /* Push these needs up so that initialization takes place
     more easily.  */
  TYPE_NEEDS_CONSTRUCTING (t) = TYPE_NEEDS_CONSTRUCTING (TYPE_MAIN_VARIANT (elt_type));
  TYPE_NEEDS_DESTRUCTOR (t) = TYPE_NEEDS_DESTRUCTOR (TYPE_MAIN_VARIANT (elt_type));
  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;
  return t;
}

/* This is temporary until later.  */
/* Construct, lay out and return the type of objects which are of type TYPE
   as members of type BASETYPE.  If that type exists already, reuse it.  */
tree
build_member_type (basetype, type)
     tree basetype, type;
{
  register tree t;
  int hashcode;

  assert (TREE_CODE (type) != FUNCTION_TYPE);

  /* Make a node of the sort we want.  */
  t = make_node (OFFSET_TYPE);
  TYPE_OFFSET_BASETYPE (t) = basetype;
  TREE_TYPE (t) = type;
  hashcode = TREE_UID (basetype) + TREE_UID (type);
  t = type_hash_canon (hashcode, t);

  return t;
}

/* Compute the actual offsets that our virtual base classes
   will have *for this type*.  This must be performed after
   the fields are laid out, since virtual baseclasses must
   lay down at the end of the record.

   Returns the maximum number of virtual functions any of the virtual
   baseclasses provide.  */
int
layout_vbasetypes (rec, max)
     tree rec;
     int max;
{
  /* Get all the virtual base types that this type uses.
     The TREE_VALUE slot holds the virtual baseclass type.  */
  tree vbase_types = get_vbase_types (rec);

#ifdef STRUCTURE_SIZE_BOUNDARY
  int record_align = MAX (STRUCTURE_SIZE_BOUNDARY, TYPE_ALIGN (rec));
#else
  int record_align = MAX (BITS_PER_UNIT, TYPE_ALIGN (rec));
#endif

  /* Record size so far is CONST_SIZE + VAR_SIZE * SIZE_UNIT bits,
     where CONST_SIZE is an integer
     and VAR_SIZE is a tree expression.
     If VAR_SIZE is null, the size is just CONST_SIZE.
     Naturally we try to avoid using VAR_SIZE.  */
  register int const_size = 0;
  register tree var_size = 0;
  register int size_unit = BITS_PER_UNIT;
  int nonvirtual_const_size;
  tree nonvirtual_var_size;

  CLASSTYPE_VBASECLASSES (rec) = vbase_types;

  if (TREE_CODE (TYPE_SIZE (rec)) == INTEGER_CST)
    const_size = TREE_INT_CST_LOW (TYPE_SIZE (rec)) * TYPE_SIZE_UNIT (rec);
  else
    {
      var_size = TYPE_SIZE (rec);
      size_unit = record_align;
    }

  nonvirtual_const_size = const_size;
  nonvirtual_var_size = var_size;

  while (vbase_types)
    {
      int inc;
      tree basetype = ASSOC_TYPE (vbase_types);
      tree offset;

      if (const_size == 0)
	offset = integer_zero_node;
      else
	offset = convert_units (build_int (const_size), 1, BITS_PER_UNIT);

      if (CLASSTYPE_VSIZE (basetype) > max)
	max = CLASSTYPE_VSIZE (basetype);
      ASSOC_OFFSET (vbase_types) = offset;

      inc = MAX (record_align,
		 (TREE_INT_CST_LOW (TYPE_SIZE (basetype))
		  - TREE_INT_CST_LOW (CLASSTYPE_VBASE_SIZE (basetype)))
		 * TYPE_SIZE_UNIT (basetype));

      const_size += inc;
      vbase_types = TREE_CHAIN (vbase_types);
    }

  if (const_size - nonvirtual_const_size)
    {
      CLASSTYPE_VBASE_SIZE (rec) = convert_units (build_int (const_size - nonvirtual_const_size),
						  1, BITS_PER_UNIT);
      TYPE_SIZE (rec) = convert_units (build_int (const_size), 1, BITS_PER_UNIT);
    }
  else
    CLASSTYPE_VBASE_SIZE (rec) = integer_zero_node;
  return max;
}

#if 0
/* This function should never be needed.  */
void
fixup_vbase_offsets (type)
     tree type;
{
  tree virtuals = TREE_CHAIN (CLASS_ASSOC_VIRTUALS (type));

  while (virtuals)
    {
      tree pfn = FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals));
      tree decl = TREE_OPERAND (pfn, 0);
      tree vcontext = get_base_type (DECL_VCONTEXT (decl), DECL_CONTEXT (decl), 0);
      if (vcontext != NULL_TREE && TREE_VIA_VIRTUAL (vcontext))
	{
	  tree offset;
	  tree vbase_offset_info;
	  tree parent_type;

	  if (DECL_CONTEXT (decl) == TYPE_MAIN_VARIANT (type))
	    parent_type = type;
	  else
	    {
	      parent_type = get_base_type (DECL_CONTEXT (decl), type, 0);
	      if (parent_type == 0)
		parent_type = type;
	    }
	  vbase_offset_info = value_member (vcontext,
					    CLASSTYPE_VBASECLASSES (parent_type));
	  offset = genop (MINUS_EXPR, CLASSTYPE_OFFSET (parent_type),
			  TREE_PURPOSE (vbase_offset_info));
	  TREE_VALUE (virtuals) = build_vtable_entry (offset, pfn);
	}
      virtuals = TREE_CHAIN (virtuals);
    }
}
#endif

/* Lay out the base types of a record type, REC.
   Tentatively set the size and alignment of REC
   according to the base types alone.

   Returns list of virtual base classes in a FIELD_DECL chain.  */
tree
layout_basetypes (rec)
     tree rec;
{
  /* Chain to hold all the new FIELD_DECLs which point at virtual
     base classes.  */
  tree vbase_decls = NULL_TREE;

#ifdef STRUCTURE_SIZE_BOUNDARY
  int record_align = MAX (STRUCTURE_SIZE_BOUNDARY, TYPE_ALIGN (rec));
#else
  int record_align = MAX (BITS_PER_UNIT, TYPE_ALIGN (rec));
#endif

  /* Record size so far is CONST_SIZE + VAR_SIZE * SIZE_UNIT bits,
     where CONST_SIZE is an integer
     and VAR_SIZE is a tree expression.
     If VAR_SIZE is null, the size is just CONST_SIZE.
     Naturally we try to avoid using VAR_SIZE.  */
  register int const_size = 0;
  register tree var_size = 0;
  register int size_unit = BITS_PER_UNIT;
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (rec);

  /* Handle basetypes almost like fields, but record their
     offsets differently.  */

  for (i = 1; i <= n_baseclasses; i++)
    {
      int inc, desired_align;
      register tree basetype = CLASSTYPE_BASECLASS (rec, i);
      tree decl;

      if (TYPE_SIZE (basetype) == 0)
	{
	  error_with_aggr_type (basetype, "base class `%s' has incomplete type");
	  SET_CLASSTYPE_VIAS (rec, i, 1, 0);
	  continue;
	}

      /* All basetypes are recorded in the association list of the
	 derived type.  */

      if (CLASSTYPE_VIA_VIRTUAL (rec, i))
	{
	  int j;
	  char *name = (char *)alloca (TYPE_NAME_LENGTH (basetype)
				       + sizeof (VBASE_NAME) + 1);
	  sprintf (name, VBASE_NAME_FORMAT, TYPE_NAME_STRING (basetype));

	  /* The offset for a virtual base class is only
	     used in computing virtual function tables and
	     for initializing virtual base pointers.  The assoc
	     for this base type is built once `get_vbase_types'
	     is called.  */
	  CLASSTYPE_BASECLASS (rec, i) = basetype
	    = build_classtype_variant (basetype, integer_zero_node, 1);

	  /* If this basetype can come from another vbase pointer
	     without an additional indirection, we will share
	     that pointer.  If an indirection is involved, we
	     make our own pointer.  */
	  for (j = 1; j <= n_baseclasses; j++)
	    if (! CLASSTYPE_VIA_VIRTUAL (rec, j)
		&& TYPE_USES_VIRTUAL_BASECLASSES (CLASSTYPE_BASECLASS (rec, j))
		&& value_member (TYPE_MAIN_VARIANT (basetype),
				 CLASSTYPE_VBASECLASSES (CLASSTYPE_BASECLASS (rec, j))))
	      {
		goto got_it;
	      }

	  decl = build_lang_decl (FIELD_DECL, get_identifier (name),
				  build_pointer_type (basetype));
	  DECL_FIELD_CONTEXT (decl) = rec;
	  SET_DECL_FCONTEXT (decl, TYPE_MAIN_VARIANT (basetype));
	  DECL_VBASE_P (decl) = 1;
	  TREE_CHAIN (decl) = vbase_decls;
	  vbase_decls = decl;

	got_it:
	  /* The space this decl occupies has already been accounted for.  */
	  continue;
	}
      else
	{
	  tree class_offset;
	  tree assoc;

	  if (const_size == 0)
	    class_offset = integer_zero_node;
	  else
	    {
	      /* Give each base type the alignment it wants.  */
	      const_size = CEIL (const_size, TYPE_ALIGN (basetype))
		* TYPE_ALIGN (basetype);
	      class_offset = convert_units (build_int (const_size), 1, BITS_PER_UNIT);
	      CLASSTYPE_BASECLASS (rec, i) = basetype
		= build_classtype_variant (basetype, class_offset, 0);
	    }

	  if (CLASSTYPE_VSIZE (basetype))
	    assoc = make_assoc (class_offset, basetype,
				CLASS_ASSOC_VTABLE (basetype),
				CLASS_ASSOC_VIRTUALS (basetype),
				CLASSTYPE_ASSOC (rec));
	  else
	    assoc = make_assoc (class_offset, basetype, 0, 0,
				CLASSTYPE_ASSOC (rec));
	  CLASSTYPE_ASSOC (rec) = assoc;
	  if (const_size != 0)
	    {
	      TYPE_NAME (basetype) = copy_node (TYPE_NAME (basetype));
	      TREE_TYPE (TYPE_NAME (basetype)) = basetype;
	      DECL_OFFSET (TYPE_NAME (basetype)) = const_size;
	    }
	}

      /* Add only the amount of storage not present in
	 the virtual baseclasses.  */
      inc = MAX (record_align,
		 (TREE_INT_CST_LOW (TYPE_SIZE (basetype))
		  - TREE_INT_CST_LOW (CLASSTYPE_VBASE_SIZE (basetype)))
		 * TYPE_SIZE_UNIT (basetype));

      /* Record must have at least as much alignment as any field.  */
      desired_align = TYPE_ALIGN (basetype);
      record_align = MAX (record_align, desired_align);

      const_size += inc;
    }

  if (const_size)
    CLASSTYPE_SIZE (rec) = build_int_2 (const_size, 0);
  else
    CLASSTYPE_SIZE (rec) = integer_zero_node;
  CLASSTYPE_ALIGN (rec) = record_align;

  return vbase_decls;
}

/* Hashing of lists so that we don't make duplicates.
   The entry point is `list_hash_canon'.  */

/* Each hash table slot is a bucket containing a chain
   of these structures.  */

struct list_hash
{
  struct list_hash *next;	/* Next structure in the bucket.  */
  int hashcode;			/* Hash code of this list.  */
  tree list;			/* The list recorded here.  */
};

/* Now here is the hash table.  When recording a list, it is added
   to the slot whose index is the hash code mod the table size.
   Note that the hash table is used for several kinds of lists.
   While all these live in the same table, they are completely independent,
   and the hash code is computed differently for each of these.  */

#define TYPE_HASH_SIZE 59
struct list_hash *list_hash_table[TYPE_HASH_SIZE];

/* Here is how primitive or already-canonicalized lists' hash
   codes are made.  */
#define TYPE_HASH(TYPE) TREE_UID (TYPE)

/* Compute a hash code for a list (chain of TREE_LIST nodes
   with goodies in the TREE_PURPOSE, TREE_VALUE, and bits of the
   TREE_COMMON slots), by adding the hash codes of the individual entries.  */

int
list_hash (list)
     tree list;
{
  register int hashcode = 0;

  if (TREE_CHAIN (list))
    hashcode = TYPE_HASH (TREE_CHAIN (list));
  else
    hashcode = 0;
  if (TREE_VALUE (list))
    hashcode += TYPE_HASH (TREE_VALUE (list));
  else
    hashcode += 1007;
  if (TREE_PURPOSE (list))
    hashcode += TYPE_HASH (TREE_PURPOSE (list));
  else
    hashcode += 1009;
  return hashcode;
}

/* Look in the type hash table for a type isomorphic to TYPE.
   If one is found, return it.  Otherwise return 0.  */

tree
list_hash_lookup (hashcode, list)
     int hashcode;
     tree list;
{
  register struct list_hash *h;
  for (h = list_hash_table[hashcode % TYPE_HASH_SIZE]; h; h = h->next)
    if (h->hashcode == hashcode
	&& TREE_VIA_VIRTUAL (h->list) == TREE_VIA_VIRTUAL (list)
	&& TREE_VIA_PUBLIC (h->list) == TREE_VIA_PUBLIC (list)
	&& TREE_PURPOSE (h->list) == TREE_PURPOSE (list)
	&& TREE_VALUE (h->list) == TREE_VALUE (list))
      {
	assert (TREE_TYPE (h->list) == TREE_TYPE (list));
	assert (TREE_CHAIN (h->list) == TREE_CHAIN (list));
	return h->list;
      }
  return 0;
}

/* Add an entry to the list-hash-table
   for a list TYPE whose hash code is HASHCODE.  */

void
list_hash_add (hashcode, list)
     int hashcode;
     tree list;
{
  register struct list_hash *h;

  h = (struct list_hash *) obstack_alloc (&class_obstack, sizeof (struct list_hash));
  h->hashcode = hashcode;
  h->list = list;
  h->next = list_hash_table[hashcode % TYPE_HASH_SIZE];
  list_hash_table[hashcode % TYPE_HASH_SIZE] = h;
}

/* Given TYPE, and HASHCODE its hash code, return the canonical
   object for an identical list if one already exists.
   Otherwise, return TYPE, and record it as the canonical object
   if it is a permanent object.

   To use this function, first create a list of the sort you want.
   Then compute its hash code from the fields of the list that
   make it different from other similar lists.
   Then call this function and use the value.
   This function frees the list you pass in if it is a duplicate.  */

/* Set to 1 to debug without canonicalization.  Never set by program.  */
int debug_no_list_hash = 0;

tree
list_hash_canon (hashcode, list)
     int hashcode;
     tree list;
{
  tree t1;

  if (debug_no_list_hash)
    return list;

  t1 = list_hash_lookup (hashcode, list);
  if (t1 != 0)
    {
      obstack_free (&class_obstack, list);
      return t1;
    }

  /* If this is a new list, record it for later reuse.  */
  list_hash_add (hashcode, list);

  return list;
}

tree
hash_tree_cons (via_public, via_virtual, purpose, value, chain)
     int via_public, via_virtual;
     tree purpose, value, chain;
{
  struct obstack *ambient_obstack = current_obstack;
  tree t;
  int hashcode;

  current_obstack = &class_obstack;
  t = tree_cons (purpose, value, chain);
  TREE_VIA_PUBLIC (t) = via_public;
  TREE_VIA_VIRTUAL (t) = via_virtual;
  hashcode = list_hash (t);
  t = list_hash_canon (hashcode, t);
  current_obstack = ambient_obstack;
  return t;
}

/* Constructor for hashed lists.  */
tree
hash_tree_chain (value, chain)
     tree value, chain;
{
  struct obstack *ambient_obstack = current_obstack;
  tree t;
  int hashcode;

  current_obstack = &class_obstack;
  t = tree_cons (NULL_TREE, value, chain);
  hashcode = list_hash (t);
  t = list_hash_canon (hashcode, t);
  current_obstack = ambient_obstack;
  return t;
}

/* Similar, but used for concatenating two lists.  */
tree
hash_chainon (list1, list2)
     tree list1, list2;
{
  if (list2 == 0)
    return list1;
  if (list1 == 0)
    return list2;
  if (TREE_CHAIN (list1) == NULL_TREE)
    return hash_tree_chain (TREE_VALUE (list1), list2);
  return hash_tree_chain (TREE_VALUE (list1),
			  hash_chainon (TREE_CHAIN (list1), list2));
}

tree
build_decl_list_1 (value)
     tree value;
{
  tree list = NULL_TREE;

  if (TREE_CODE (value) == IDENTIFIER_NODE)
    {
      list = IDENTIFIER_AS_LIST (value);
      if (list != NULL_TREE
	  && (TREE_CODE (list) != TREE_LIST
	      || TREE_VALUE (list) != value))
	list = NULL_TREE;
      else if (TREE_TYPE (value) != NULL_TREE
	       && TREE_CODE (TREE_TYPE (TREE_TYPE (value))) == RECORD_TYPE)
	{
	  tree type = TREE_TYPE (TREE_TYPE (value));
	  if (CLASSTYPE_AS_ID_LIST (type) == NULL_TREE)
	    CLASSTYPE_AS_ID_LIST (type) = perm_tree_cons (NULL_TREE, value, NULL_TREE);
	  list = CLASSTYPE_AS_ID_LIST (type);
	}
    }
  else if (TREE_CODE (value) == RECORD_TYPE
	   && TYPE_LANG_SPECIFIC (value))
    list = CLASSTYPE_AS_LIST (value);

  if (list != NULL_TREE)
    {
      assert (TREE_CHAIN (list) == NULL_TREE);
      return list;
    }

  return build_decl_list (NULL_TREE, value);
}

/* Look in the type hash table for a type isomorphic to
   `build_tree_list (NULL_TREE, VALUE)'.
   If one is found, return it.  Otherwise return 0.  */

tree
list_hash_lookup_or_cons (value)
     tree value;
{
  register int hashcode = TYPE_HASH (value);
  register struct list_hash *h;
  struct obstack *ambient_obstack;
  tree list = NULL_TREE;

  if (TREE_CODE (value) == IDENTIFIER_NODE)
    {
      list = IDENTIFIER_AS_LIST (value);
      if (list != NULL_TREE
	  && (TREE_CODE (list) != TREE_LIST
	      || TREE_VALUE (list) != value))
	list = NULL_TREE;
      else if (TREE_TYPE (value) != NULL_TREE
	       && TREE_CODE (TREE_TYPE (TREE_TYPE (value))) == RECORD_TYPE)
	{
	  tree type = TREE_TYPE (TREE_TYPE (value));
	  if (CLASSTYPE_AS_ID_LIST (type) == NULL_TREE)
	    CLASSTYPE_AS_ID_LIST (type) = perm_tree_cons (NULL_TREE, value, NULL_TREE);
	  list = CLASSTYPE_AS_ID_LIST (type);
	}
    }
  else if (TREE_CODE (value) == TYPE_DECL
	   && TREE_CODE (TREE_TYPE (value)) == RECORD_TYPE
	   && TYPE_LANG_SPECIFIC (TREE_TYPE (value)))
    list = CLASSTYPE_AS_ID_LIST (TREE_TYPE (value));
  else if (TREE_CODE (value) == RECORD_TYPE
	   && TYPE_LANG_SPECIFIC (value))
    list = CLASSTYPE_AS_LIST (value);

  if (list != NULL_TREE)
    {
      assert (TREE_CHAIN (list) == NULL_TREE);
      return list;
    }

  if (debug_no_list_hash)
    return hash_tree_chain (value, NULL_TREE);

  for (h = list_hash_table[hashcode % TYPE_HASH_SIZE]; h; h = h->next)
    if (h->hashcode == hashcode
	&& TREE_VIA_VIRTUAL (h->list) == 0
	&& TREE_VIA_PUBLIC (h->list) == 0
	&& TREE_PURPOSE (h->list) == 0
	&& TREE_VALUE (h->list) == value)
      {
	assert (TREE_TYPE (h->list) == 0);
	assert (TREE_CHAIN (h->list) == 0);
	return h->list;
      }

  ambient_obstack = current_obstack;
  current_obstack = &class_obstack;
  list = build_tree_list (NULL_TREE, value);
  list_hash_add (hashcode, list);
  current_obstack = ambient_obstack;
  return list;
}

/* Build an association between TYPE and some parameters:

   OFFSET is the offset added to `this' to convert it to a pointer
   of type `TYPE *'

   VTABLE is the virtual function table with which to initialize
   sub-objects of type TYPE.

   VIRTUALS are the virtual functions sitting in VTABLE.

   CHAIN are more associations we must retain.  */

tree
make_assoc (offset, type, vtable, virtuals, chain)
     tree offset, type;
     tree vtable, virtuals;
     tree chain;
{
  tree assoc = make_tree_vec (4);

  TREE_TYPE (assoc) = type;
  TREE_CHAIN (assoc) = chain;
  if (chain)
    TREE_USED (assoc) = TREE_USED (chain);

  /* n.b.: TREE_VEC_ELT (assoc, 0) <=> TREE_VALUE (assoc).  */
  TREE_VEC_ELT (assoc, 0) = TYPE_MAIN_VARIANT (type);
  TREE_VEC_ELT (assoc, 1) = offset;
  TREE_VEC_ELT (assoc, 2) = vtable;
  TREE_VEC_ELT (assoc, 3) = virtuals;
  return assoc;
}

tree
copy_assoc (list)
     tree list;
{
  tree assoc = copy_list (list);
  tree rval = assoc;
  while (assoc)
    {
      TREE_USED (assoc) = 0;
      assoc = TREE_CHAIN (assoc);
    }
  return rval;
}

tree
assoc_value (elem, type)
     tree elem;
     tree type;
{
  tree assoc = CLASSTYPE_ASSOC (type);
  tree rval = NULL_TREE;

  /* Dispose quickly of degenerate case.  */
  if (elem == type)
    return assoc;

  while (assoc)
    {
      if (elem == ASSOC_VALUE (assoc))
	/* If we find it on the main spine, then
	   there can be no ambiguity.  */
	return assoc;

      if (ASSOC_VALUE (assoc) != type)
	{
	  tree nval = assoc_value (elem, ASSOC_TYPE (assoc));

	  if (nval)
	    if (rval && ASSOC_TYPE (rval) != ASSOC_TYPE (nval))
	      /* If we find it underneath, we must make sure that
		 there are no two ways to do it.  */
	      compiler_error ("base class `%s' ambiguous in assoc_value",
			      TYPE_NAME_STRING (elem));
	    else
	      rval = nval;
	}
      assoc = TREE_CHAIN (assoc);
    }
  return rval;
}

tree
virtual_member (elem, list)
     tree elem;
     tree list;
{
  tree t;
  tree rval, nval;

  for (t = list; t; t = TREE_CHAIN (t))
    if (elem == TREE_VALUE (t))
      return t;
  rval = 0;
  for (t = list; t; t = TREE_CHAIN (t))
    {
      int i;
      for (i = CLASSTYPE_N_BASECLASSES (TREE_TYPE (t)); i > 0; i--)
	{
	  nval = assoc_value (elem, CLASSTYPE_BASECLASS (TREE_TYPE (t), i));
	  if (nval)
	    {
	      if (rval && TREE_TYPE (nval) != TREE_TYPE (rval))
		abort ();
	      rval = nval;
	    }
	}
    }
  return rval;
}

void
debug_dump_assoc (elem)
     tree elem;
{
  int i;
  tree virtuals;

  fprintf (stderr, "type \"%s\"; offset = %d\n",
	   IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (ASSOC_VALUE (elem)))),
	   TREE_INT_CST_LOW (ASSOC_OFFSET (elem)));
  fprintf (stderr, "vtable type:\n");
  dump_tree (stderr, ASSOC_TYPE (elem));
  if (ASSOC_VTABLE (elem))
    fprintf (stderr, "vtable decl \"%s\"\n", IDENTIFIER_POINTER (DECL_NAME (ASSOC_VTABLE (elem))));
  else
    fprintf (stderr, "no vtable decl yet\n");
  fprintf (stderr, "virtuals:\n");
  virtuals = ASSOC_VIRTUALS (elem);
  if (virtuals != 0)
    virtuals = TREE_CHAIN (virtuals);
  i = 1;
  while (virtuals)
    {
      tree fndecl = TREE_OPERAND (FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals)), 0);
      fprintf (stderr, "%s [%d =? %d]\n",
	       IDENTIFIER_POINTER (DECL_NAME (fndecl)),
	       i, TREE_INT_CST_LOW (DECL_VINDEX (fndecl)));
      virtuals = TREE_CHAIN (virtuals);
      i += 1;
    }
}

char *
lang_printable_name (decl)
     tree decl;
{
  if (TREE_CODE (decl) != FUNCTION_DECL
      || DECL_LANG_SPECIFIC (decl) == 0)
    {
      if (DECL_NAME (decl))
	{
	  if (THIS_NAME_P (DECL_NAME (decl)))
	    return "this";
	  return IDENTIFIER_POINTER (DECL_NAME (decl));
	}
      return "((anonymous))";
    }
  if (DECL_PRINT_NAME (decl) == 0)
    {
      int print_ret_type_p
	= (!DECL_CONSTRUCTOR_P (decl)
	   && !DESTRUCTOR_NAME_P (DECL_NAME (decl)));
      int temp = allocation_temporary_p ();
      char *buf = (char *)alloca (8192);
      char *name = (char *)fndecl_as_string (buf, 0, decl, print_ret_type_p);
      end_temporary_allocation ();
      DECL_PRINT_NAME (decl) = oballoc (strlen (name) + 1);
      strcpy (DECL_PRINT_NAME (decl), name);
      if (temp)
	resume_temporary_allocation ();
    }
  else if (DECL_NAME (decl) == 0)
    DECL_PRINT_NAME (decl) = "((anonymous))";

  return DECL_PRINT_NAME (decl);
}

/* Return truthvalue about whether debugger should
   output full info about this type or not.

   Current strategy is to permit types which define
   no member functions to be output normally.  For
   those which do define member functions, if no
   member functions have yet been output, then don't
   output the definition of the type.  If member functions
   for the type are later seen, a full definition of the
   type will eventually be output.  */
int
lang_output_debug_info (type)
     tree type;
{
  extern tree pending_vtables;

  if (! IS_AGGR_TYPE (type))
    return 1;
  if (TYPE_LANG_SPECIFIC (type) == 0)
    return 1;
  if (CLASSTYPE_METHOD_VEC (type) == 0)
    return 1;

  if (flag_minimal_debug)
    {
      /* Don't output full info about any type
	 which does not have its implementation defined here.  */
      if (TYPE_VIRTUAL_P (type) && write_virtuals == 2)
	return value_member (DECL_NAME (TYPE_NAME (type)), pending_vtables) != 0;
      if (CLASSTYPE_INTERFACE_ONLY (type))
	return 0;

      return CLASSTYPE_ASM_WRITTEN (type);
    }
  else
    /* Can't work until GDB is modified.  */
    return 1;
}

/* Comparison function for sorting identifiers in RAISES lists.
   Note that because IDENTIFIER_NODEs are unique, we can sort
   them by address, saving an indirection.  */
static int
id_cmp (p1, p2)
     int *p1, *p2;
{
  return *p1 - *p2;
}

/* Build the FUNCTION_TYPE or METHOD_TYPE which may raise exceptions
   listed in RAISES.  */
tree
build_exception_variant (ctype, type, raises)
     tree ctype, type;
     tree raises;
{
  int i;
  tree v = TYPE_MAIN_VARIANT (type);
  tree t, t2, cname;
  tree *a = (tree *)alloca ((list_length (raises)+1) * sizeof (tree));
  int constp = TREE_READONLY (type);
  int volatilep = TREE_VOLATILE (type);

  if (raises && TREE_CHAIN (raises))
    {
      for (i = 0, t = raises; t; t = TREE_CHAIN (t), i++)
	a[i] = t;
      /* NULL terminator for list.  */
      a[i] = NULL_TREE;
      qsort (a, i, sizeof (tree), id_cmp);
      while (i--)
	TREE_CHAIN (a[i]) = a[i+1];
      raises = a[0];
    }
  else if (raises)
    /* do nothing.  */;
  else
    return build_type_variant (v, constp, volatilep);

  if (ctype)
    {
      cname = TYPE_NAME (ctype);
      if (TREE_CODE (cname) == TYPE_DECL)
	cname = DECL_NAME (cname);
    }
  else
    cname = NULL_TREE;

  for (t = raises; t; t = TREE_CHAIN (t))
    {
      /* See that all the exceptions we are thinking about
	 raising have been declared.  */
      tree this_cname = lookup_exception_cname (ctype, cname, t);
      tree decl = lookup_exception_object (this_cname, TREE_VALUE (t), 1);

      if (decl == NULL_TREE)
	decl = lookup_exception_object (this_cname, TREE_VALUE (t), 0);
      /* Place canonical exception decl into TREE_TYPE of RAISES list.  */
      TREE_TYPE (t) = decl;
    }

  for (v = TYPE_NEXT_VARIANT (v); v; v = TYPE_NEXT_VARIANT (v))
    {
      if (TREE_READONLY (v) != constp
	  || TREE_VOLATILE (v) != volatilep)
	continue;

      t = raises;
      t2 = TYPE_RAISES_EXCEPTIONS (v);
      while (t && t2)
	{
	  if (TREE_TYPE (t) == TREE_TYPE (t2))
	    {
	      t = TREE_CHAIN (t);
	      t2 = TREE_CHAIN (t2);
	    }
	  else break;
	}
      if (t || t2)
	continue;
      /* List of exceptions raised matches previously found list.

         @@ Nice to free up storage used in consing up the
	 @@ list of exceptions raised.  */
      return v;
    }

  /* Need to build a new variant.  */
  v = copy_node (type);
  TYPE_NEXT_VARIANT (v) = TYPE_NEXT_VARIANT (type);
  TYPE_NEXT_VARIANT (type) = v;
  if (raises && ! TREE_PERMANENT (raises))
    {
      int temporary = allocation_temporary_p ();
      if (temporary)
	end_temporary_allocation ();
      raises = copy_list (raises);
      if (temporary)
	resume_temporary_allocation ();
    }
  TYPE_RAISES_EXCEPTIONS (v) = raises;
  return v;
}

/* Subroutine of make_permanent_node.

   Assuming T is a node build bottom-up, make it all exist on
   permanent obstack, if it is not permanent already.  */
static tree
make_deep_copy (t)
     tree t;
{
  enum tree_code code;

  if (t == NULL_TREE || TREE_PERMANENT (t))
    return t;

  switch (code = TREE_CODE (t))
    {
    case ERROR_MARK:
      return error_mark_node;

    case VAR_DECL:
    case PARM_DECL:
    case FUNCTION_DECL:
    case CONST_DECL:
      break;

    case TREE_LIST:
      {
	tree chain = TREE_CHAIN (t);
	t = copy_node (t);
	TREE_PURPOSE (t) = make_deep_copy (TREE_PURPOSE (t));
	TREE_VALUE (t) = make_deep_copy (TREE_VALUE (t));
	TREE_CHAIN (t) = make_deep_copy (chain);
	return t;
      }

    case TREE_VEC:
      {
	int len = TREE_VEC_LENGTH (t);

	t = copy_node (t);
	while (len--)
	  TREE_VEC_ELT (t, len) = make_deep_copy (TREE_VEC_ELT (t, len));
	return t;
      }

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      return copy_node (t);

    case COND_EXPR:
    case NEW_EXPR:
      t = copy_node (t);
      TREE_OPERAND (t, 0) = make_deep_copy (TREE_OPERAND (t, 0));
      TREE_OPERAND (t, 1) = make_deep_copy (TREE_OPERAND (t, 1));
      TREE_OPERAND (t, 2) = make_deep_copy (TREE_OPERAND (t, 2));
      return t;

    case SAVE_EXPR:
      t = copy_node (t);
      TREE_OPERAND (t, 0) = make_deep_copy (TREE_OPERAND (t, 0));
      return t;

    case MODIFY_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case TRUNC_MOD_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case COMPOUND_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case CALL_EXPR:
      t = copy_node (t);
      TREE_OPERAND (t, 0) = make_deep_copy (TREE_OPERAND (t, 0));
      TREE_OPERAND (t, 1) = make_deep_copy (TREE_OPERAND (t, 1));
      return t;

    case CONVERT_EXPR:
    case ADDR_EXPR:
    case INDIRECT_REF:
    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case NOP_EXPR:
    case COMPONENT_REF:
      t = copy_node (t);
      TREE_OPERAND (t, 0) = make_deep_copy (TREE_OPERAND (t, 0));
      return t;

      /*  This list is incomplete, but should suffice for now.
	  It is very important that `sorry' does not call
	  `report_error_function'.  That could cause an infinite loop.  */
    default:
      sorry ("initializer contains unrecognized tree code");
      return error_mark_node;

    }
  abort ();
}

/* Assuming T is a node built bottom-up, make it all exist on
   permanent obstack, if it is not permanent already.  */
tree
copy_to_permanent (t)
     tree t;
{
  register struct obstack *ambient_obstack = current_obstack;
  register struct obstack *ambient_saveable_obstack = saveable_obstack;

  if (t == NULL_TREE || TREE_PERMANENT (t))
    return t;

  saveable_obstack = &permanent_obstack;
  current_obstack = saveable_obstack;

  t = make_deep_copy (t);

  current_obstack = ambient_obstack;
  saveable_obstack = ambient_saveable_obstack;

  return t;
}

int
lang_simple_cst_equal (t1, t2)
     tree t1, t2;
{
  register enum tree_code code1, code2;
  int cmp;

  if (t1 == t2)
    return 1;
  if (t1 == 0 || t2 == 0)
    return 0;

  code1 = TREE_CODE (t1);
  code2 = TREE_CODE (t2);

  switch (code1)
    {
    case CPLUS_NEW_EXPR:
      cmp = simple_cst_equal (TREE_OPERAND (t1, 0), TREE_OPERAND (t2, 0));
      if (cmp <= 0)
	return cmp;
      return simple_cst_list_equal (TREE_OPERAND (t1, 1), TREE_OPERAND (t2, 1));

    default:
      return -1;
    }
}
