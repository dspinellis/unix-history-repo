/* Functions related to building and playing with classes.
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
#include "flags.h"
#include "rtl.h"
#include "assert.h"
#include <stdio.h>

#include "obstack.h"
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int xmalloc ();
extern void free ();

#define NULL 0
#define MIN(X,Y) ((X) < (Y) ? (X) : (Y))
#define MAX(X,Y) ((X) > (Y) ? (X) : (Y))

/* See cplus-decl.c for comment of this variable.  */
extern int flag_int_enum_equivalence;

/* some statistics gathering help.  */
static int n_vtables, n_vtable_entries, n_vtable_searches, n_vtable_elems;
static int n_convert_harshness, n_compute_conversion_costs, n_build_method_call;
static int n_inner_fields_searched;

/* Compute the ease with which a conversion can be performed
   between an expected and the given type.  */
static int convert_harshness ();

/* in decl.c.  */
extern tree lookup_tag_current_binding_level ();

/* in method.c.  */
extern void do_inline_function_hair ();

/* Way of stacking class types.  */
static tree *current_class_base, *current_class_stack;
static int current_class_stacksize;

struct class_level
{
  /* The previous class level.  */
  struct class_level *level_chain;

  /* The class instance variable, as a PARM_DECL.  */
  tree decl;
  /* The class instance variable, as an object.  */
  tree object;
  /* The virtual function table pointer
     for the class instance variable.  */
  tree vtable_decl;

  /* Name of the current class.  */
  tree name;
  /* Type of the current class.  */
  tree type;

  /* Flags for this class level.  */
  int this_is_variable;
  int memoized_lookups;
  int save_memoized;
  int unused;
};

tree current_class_decl, C_C_D;	/* PARM_DECL: the class instance variable */
tree current_vtable_decl;

/* The following two can be derived from the previous one */
tree current_class_name;	/* IDENTIFIER_NODE: name of current class */
tree current_class_type;	/* _TYPE: the type of the current class */
tree prev_class_type;		/* _TYPE: the previous type that was a class */

static tree get_vtable_name (), get_vfield_name ();
tree the_null_vtable_entry;

/* Way of stacking langauge names.  */
static tree *current_lang_base, *current_lang_stack;
static int current_lang_stacksize;

/* Names of languages we recognize.  */
tree lang_name_c, lang_name_cplusplus;
tree current_lang_name;

tree minus_one_node;

/* When layout out an aggregate type, the size of the
   basetypes (virtual and non-virtual) is passed to layout_record
   via this node.  */
static tree base_layout_decl;

#if 0
/* Make sure that the tag NAME is defined *in the current binding level*
   at least as a forward reference.
   CODE says which kind of tag NAME ought to be.

   Not used for C++.  Not maintained.  */

tree
start_struct (code, name)
     enum tree_code code;
     tree name;
{
  /* If there is already a tag defined at this binding level
     (as a forward reference), just return it.  */
  register tree ref = 0;

  if (name != 0)
    ref = lookup_tag (code, name, current_binding_level, 1);
  if (ref && TREE_CODE (ref) == code)
    {
      if (TYPE_FIELDS (ref))
	error ((code == UNION_TYPE ? "redefinition of `union %s'"
		: "redefinition of `struct %s'"),
	       IDENTIFIER_POINTER (name));

      return ref;
    }

  /* Otherwise create a forward-reference just so the tag is in scope.  */

  ref = make_lang_type (code);
  /* Must re-synch this with xref_tag if you are going to use it.  */
  assert (0);
  pushtag (name, ref);
  return ref;
}
#endif

/* Virtual baseclass things.  */
tree
build_vbase_pointer (exp, type)
     tree exp, type;
{
  char *name;

  name = (char *) alloca (TYPE_NAME_LENGTH (type) + sizeof (VBASE_NAME) + 1);
  sprintf (name, VBASE_NAME_FORMAT, TYPE_NAME_STRING (type));
  return build_component_ref (exp, get_identifier (name), 0, 0);
}

/* Build multi-level access to EXPR using hierarchy path PATH.
   CODE is PLUS_EXPR if we are going with the grain,
   and MINUS_EXPR if we are not (in which case, we cannot traverse
   virtual baseclass links).

   TYPE is the type we want this path to have on exit.

   ALIAS_THIS is non-zero if EXPR in an expression involving `this'.  */
tree
build_vbase_path (code, type, expr, path, alias_this)
     enum tree_code code;
     tree type;
     tree expr;
     tree path;
     int alias_this;
{
  register int changed = 0;
  tree last = NULL_TREE, last_virtual = NULL_TREE;
  int fixed_type_p = 0 && resolves_to_fixed_type_p (expr);
  tree basetype;
  tree offset = integer_zero_node;

  if (TREE_CHAIN (path))
    path = nreverse (copy_list (path));

  basetype = TREE_VALUE (path);
  while (path)
    {
      if (TREE_VIA_VIRTUAL (path))
	{
	  last_virtual = TYPE_MAIN_VARIANT (TREE_VALUE (path));
	  if (code == PLUS_EXPR)
	    {
	      changed = ! fixed_type_p;

	      if (last)
		expr = convert_pointer_to (TREE_VALUE (last), expr);
	      if (changed)
		expr = build_vbase_pointer (build_indirect_ref (expr, 0),
					    last_virtual);
	      else
		offset = ASSOC_OFFSET (value_member (last_virtual,
						     CLASSTYPE_VBASECLASSES (basetype)));
	      /* Happens in the case of parse errors.  */
	      if (expr == error_mark_node)
		return expr;
	    }
	  else
	    {
	      error_with_aggr_type (last_virtual, "cannot cast up from virtual baseclass `%s'");
	      return error_mark_node;
	    }
	}
      last = path;
      path = TREE_CHAIN (path);
    }
  /* LAST is now the last basetype on the path.  */
  last = TREE_VALUE (last);

  /* If we go through any virtual base pointers, make sure that
     casts to BASETYPE from the last virtual base class use
     the right value for BASETYPE.  */
  if (changed)
    {
      tree intype = TREE_TYPE (TREE_TYPE (expr));
      if (TYPE_MAIN_VARIANT (intype) == TYPE_MAIN_VARIANT (last))
	basetype = intype;
      else
	{
	  basetype = get_base_type (last, TYPE_MAIN_VARIANT (intype), 0);
	  offset = CLASSTYPE_OFFSET (basetype);
	}
    }
  else
    {
      if (last_virtual && last != last_virtual)
	basetype = get_base_type (last, last_virtual, 0);
      else
	basetype = last;

      offset = genop (code, offset, CLASSTYPE_OFFSET (basetype));
      /* 900324 JRV: If code was MINUS_EXPR, genop returned a negative
	 offset, so shouldn't the code always be PLUS_EXPR now? */
      code = PLUS_EXPR;
    }

  if (TREE_INT_CST_LOW (offset))
    {
      /* For multiple inheritance: if `this' can be set by
	 any function, then it could be 0 on entry
	 to any function.  Preserve such zeroness here.
	 Otherwise, only in the case of constructors need
	 we worry, and in those cases, it will be zero,
	 or initialized to some legal value to which we may
	 add.  */
      tree addr = TREE_CODE (expr) == ADDR_EXPR
	? expr : save_expr (expr);
      expr = build (code, type, addr, offset);

      if (alias_this == 0 || flag_this_is_variable)
	return build (COND_EXPR, type,
		      build (EQ_EXPR, integer_type_node, addr, integer_zero_node),
		      build1 (NOP_EXPR, type, addr),
		      expr);
    }
  return build1 (NOP_EXPR, type, expr);
}

/* Virtual function things.  */

/* Virtual functions to be dealt with after laying out our
   virtual base classes (only if the type has any).  */
static tree pending_hard_virtuals;

/* The names of the entries in the virtual table structure.  */
static tree delta_name, pfn_name;

/* Temporary assoc list to memoize lookups of the left-most non-virtual
   baseclass B in a lattice topped by T.  B can appear multiple times
   in the lattice.
   TREE_PURPOSE is B's TYPE_MAIN_VARIANT.
   TREE_VALUE is the path by which B is reached from T.
   TREE_TYPE is B's real type.

   If TREE_TYPE is NULL_TREE, it means that B was reached via
   a virtual baseclass.
   N.B.: This list consists of nodes on the temporary obstack.  */
static tree leftmost_baseclasses;

/* Build an entry in the virtual function table.
   DELTA is the offset for the `this' pointer.
   PFN is an ADDR_EXPR containing a pointer to the virtual function.
   Note that the index (DELTA2) in the virtual function table
   is always 0.  */
tree
build_vtable_entry (delta, pfn)
     tree delta, pfn;
{
  tree elems = tree_cons (NULL_TREE, delta,
			  tree_cons (NULL_TREE, integer_zero_node,
				     build_tree_list (NULL_TREE, pfn)));
  tree entry = build (CONSTRUCTOR, vtable_entry_type, NULL_TREE, elems);
  TREE_LITERAL (entry) = 1;
  TREE_STATIC (entry) = 1;
  TREE_READONLY (entry) = 1;

#ifdef GATHER_STATISTICS
  n_vtable_entries += 1;
#endif

  return entry;
}

/* Given an object INSTANCE, return an expression which yields
   the virtual function corresponding to INDEX.  There are many special
   cases for INSTANCE which we take care of here, mainly to avoid
   creating extra tree nodes when we don't have to.  */
tree
build_vfn_ref (ptr_to_instptr, instance, index)
     tree *ptr_to_instptr, instance;
     tree index;
{
  extern int building_cleanup;
  tree vtbl, aref;
  tree basetype = TREE_TYPE (instance);

  if (TREE_CODE (basetype) == REFERENCE_TYPE)
    basetype = TREE_TYPE (basetype);

  if (instance == C_C_D)
    {
      if (current_vtable_decl == NULL_TREE
	  || current_vtable_decl == error_mark_node
	  || get_base_type (DECL_FCONTEXT (CLASSTYPE_VFIELD (current_class_type)), basetype, 0) == NULL_TREE)
	vtbl = build_indirect_ref (build_vfield_ref (instance, basetype));
      else
	vtbl = current_vtable_decl;
    }
  else
    {
      if (optimize)
	{
	  /* Try to figure out what a reference refers to, and
	     access its virtual function table directly.  */
	  tree ref = 0;

	  if (TREE_CODE (instance) == INDIRECT_REF
	      && TREE_CODE (TREE_TYPE (TREE_OPERAND (instance, 0))) == REFERENCE_TYPE)
	    ref = TREE_OPERAND (instance, 0);
	  else if (TREE_CODE (TREE_TYPE (instance)) == REFERENCE_TYPE)
	    ref = instance;

	  if (ref && TREE_CODE (ref) == VAR_DECL
	      && DECL_INITIAL (ref))
	    {
	      tree init = DECL_INITIAL (ref);

	      while (TREE_CODE (init) == NOP_EXPR
		     || TREE_CODE (init) == REFERENCE_EXPR)
		init = TREE_OPERAND (init, 0);
	      if (TREE_CODE (init) == ADDR_EXPR)
		{
		  init = TREE_OPERAND (init, 0);
		  if (IS_AGGR_TYPE (TREE_TYPE (init))
		      && (TREE_CODE (init) == PARM_DECL
			  || TREE_CODE (init) == VAR_DECL))
		    instance = init;
		}
	    }
	}

      if (IS_AGGR_TYPE (instance)
	  && (TREE_CODE (instance) == RESULT_DECL
	      || TREE_CODE (instance) == PARM_DECL
	      || TREE_CODE (instance) == VAR_DECL))
	vtbl = CLASS_ASSOC_VTABLE (basetype);
      else
	vtbl = build_indirect_ref (build_vfield_ref (instance, basetype), 0);
    }
  aref = build_array_ref (vtbl, index);
  if (!building_cleanup && TREE_CODE (aref) == INDIRECT_REF)
    TREE_OPERAND (aref, 0) = save_expr (TREE_OPERAND (aref, 0));

  *ptr_to_instptr = build (PLUS_EXPR, TREE_TYPE (*ptr_to_instptr),
			   *ptr_to_instptr,
			   convert (integer_type_node, build_component_ref (aref, delta_name, 0, 0)));
  return build_component_ref (aref, pfn_name, 0, 0);
}

/* Build a virtual function for type TYPE.
   If ASSOC is non-NULL, build the vtable starting with the intial
   approximation that it is the same as the one which is the head of
   the assocation list.  */
static tree
build_vtable (assoc, type)
     tree assoc, type;
{
  tree name = get_vtable_name (type);
  tree virtuals, decl;

  if (assoc)
    {
      virtuals = copy_list (ASSOC_VIRTUALS (assoc));
      decl = build_decl (VAR_DECL, name, TREE_TYPE (ASSOC_VTABLE (assoc)));
    }
  else
    {
      virtuals = NULL_TREE;
      decl = build_decl (VAR_DECL, name, void_type_node);
    }

#ifdef GATHER_STATISTICS
  n_vtables += 1;
  n_vtable_elems += list_length (virtuals);
#endif

  if (write_virtuals >= 2)
    {
      if (CLASSTYPE_INTERFACE_UNKNOWN (type) == 0)
	{
	  TREE_PUBLIC (decl) = CLASSTYPE_VTABLE_NEEDS_WRITING (type);
	  TREE_EXTERNAL (decl) = ! CLASSTYPE_VTABLE_NEEDS_WRITING (type);
	}
    }
  else if (write_virtuals > 0)
    TREE_PUBLIC (decl) = 1;
  else if (write_virtuals < 0)
    TREE_EXTERNAL (decl) = 1;

  IDENTIFIER_GLOBAL_VALUE (name) = decl = pushdecl_top_level (decl);
  /* Initialize the association list for this type, based
     on our first approximation.  */
  CLASS_ASSOC_VTABLE (type) = decl;
  CLASS_ASSOC_VIRTUALS (type) = virtuals;

  TREE_STATIC (decl) = 1;
  DECL_ALIGN (decl) = MAX (TYPE_ALIGN (double_type_node),
			   DECL_ALIGN (decl));

  if (assoc && write_virtuals >= 0)
    DECL_VIRTUAL_P (decl) = 1;
  /* Remember which class this vtable is really for.  */
  DECL_VPARENT (decl) = type;
  DECL_CONTEXT (decl) = type;
  CLASSTYPE_MARKED3 (type) = 1;
  CLASSTYPE_MARKED4 (type) = 1;
  return decl;
}

/* Give TYPE a new virtual function table which is initialized
   with a skeleton-copy of its original initialization.  The only
   entry that changes is the `delta' entry, so we can really
   share a lot of structure.

   FOR_TYPE is the derived type which caused this table to
   be needed.

   ASSOC is the type association which provided TYPE for FOR_TYPE.

   The way we update BASE_ASSOC's vtable information is just to change the
   association information in FOR_TYPE's association list.  */
static void
prepare_fresh_vtable (assoc, base_assoc, for_type)
     tree assoc, base_assoc, for_type;
{
  tree basetype = ASSOC_TYPE (assoc);
  tree orig_decl = ASSOC_VTABLE (assoc);
  tree name = build_type_pathname (VTABLE_NAME_FORMAT, basetype, for_type);
  tree new_decl = build_decl (VAR_DECL, name, TREE_TYPE (orig_decl));
  tree path;
  int result;

  assert (TREE_USED (assoc) == 0);

  /* Remember which class this vtable is really for.  */
  DECL_VPARENT (new_decl) = ASSOC_TYPE (base_assoc);
  DECL_CONTEXT (new_decl) = for_type;

  TREE_STATIC (new_decl) = 1;
  ASSOC_VTABLE (assoc) = pushdecl_top_level (new_decl);
  DECL_VIRTUAL_P (new_decl) = 1;
  DECL_ALIGN (new_decl) = DECL_ALIGN (orig_decl);

  /* Make fresh virtual list, so we can smash it later.  */
  assert (ASSOC_VIRTUALS (assoc));
  ASSOC_VIRTUALS (assoc) = copy_list (ASSOC_VIRTUALS (assoc));

#ifdef GATHER_STATISTICS
  n_vtables += 1;
  n_vtable_elems += list_length (ASSOC_VIRTUALS (assoc));
#endif

  /* Set `new_decl's PUBLIC and EXTERNAL bits.  */
  if (write_virtuals >= 2)
    {
      if (CLASSTYPE_INTERFACE_UNKNOWN (for_type) == 0)
	{
	  TREE_PUBLIC (new_decl) = CLASSTYPE_VTABLE_NEEDS_WRITING (for_type);
	  TREE_EXTERNAL (new_decl) = ! CLASSTYPE_VTABLE_NEEDS_WRITING (for_type);
	}
    }
  else if (write_virtuals > 0)
    TREE_PUBLIC (new_decl) = 1;
  else if (write_virtuals < 0)
    TREE_EXTERNAL (new_decl) = 1;

  CLASSTYPE_MARKED3 (basetype) = 1;
  CLASSTYPE_MARKED4 (basetype) = 1;

  /* Mark all types between FOR_TYPE and TYPE as having been
     touched, so that if we change virtual function table entries,
     new vtables will be initialized.  We may reach the virtual
     baseclass via ambiguous intervening baseclasses.  This
     loop makes sure we get through to the actual baseclass we marked.  */

  do
    {
      result = get_base_distance (basetype, for_type, 0, &path);
      for_type = TREE_VALUE (path);
      while (path)
	{
	  CLASSTYPE_MARKED3 (TREE_VALUE (path)) = 1;
	  path = TREE_CHAIN (path);
	}
    }
  while (result == -2);
}

/* Access the virtual function table entry that logically
   contains BASE_FNDECL.  VIRTUALS is the virtual function table's
   initializer.  */
static tree
get_vtable_entry (virtuals, base_fndecl)
     tree virtuals, base_fndecl;
{
  int i = (HOST_BITS_PER_INT >= BITS_PER_WORD
#ifdef VTABLE_USES_MASK
	   && 0
#endif
	   ? (TREE_INT_CST_LOW (DECL_VINDEX (base_fndecl))
	      & ((1<<(BITS_PER_WORD-1))-1))
	   : TREE_INT_CST_LOW (DECL_VINDEX (base_fndecl)));

#ifdef GATHER_STATISTICS
  n_vtable_searches += i;
#endif

  while (i > 0)
    {
      virtuals = TREE_CHAIN (virtuals);
      i -= 1;
    }
  return virtuals;
}

/* Put new entry ENTRY into virtual function table initializer
   VIRTUALS.  The virtual function table is for type CONTEXT.

   Also update DECL_VINDEX (FNDECL).  */

static void
modify_vtable_entry (old_entry_in_list, new_entry, fndecl, context)
     tree old_entry_in_list, new_entry, fndecl, context;
{
  tree base_pfn = FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (old_entry_in_list));
  tree vindex;

  /* We can't put in the really right offset information
     here, since we have not yet laid out the class to
     take into account virtual base classes.  */
  TREE_VALUE (old_entry_in_list) = new_entry;
  vindex = DECL_VINDEX (TREE_OPERAND (base_pfn, 0));
  if (TREE_CODE (DECL_VINDEX (fndecl)) != INTEGER_CST)
    SET_DECL_VINDEX (fndecl, vindex);
  else
    {
      if (! tree_int_cst_equal (DECL_VINDEX (fndecl), vindex))
	{
	  tree elts = CONSTRUCTOR_ELTS (new_entry);
	  tree vfield = CLASSTYPE_VFIELD (context);
	  /* Compute the relative offset of vtable we are really looking for.  */
	  TREE_VALUE (elts) = genop (PLUS_EXPR,
				     build_int (DECL_OFFSET (vfield)
						/ DECL_SIZE_UNIT (vfield)),
				     TREE_VALUE (elts));
	  /* Say what index to use when we use that vtable.  */
#ifndef VTABLE_USES_MASK
	  vindex = build_int_2 (TREE_INT_CST_LOW (vindex) & ~(1 << (BITS_PER_WORD -1)), 0);
#endif
	  TREE_VALUE (TREE_CHAIN (elts)) = vindex;
	}
    }
}

/* Modify virtual function tables in lattice topped by T to
   place FNDECL in tables which previously held BASE_FNDECL.
   PFN is just FNDECL wrapped in an ADDR_EXPR, so that it
   is suitable for placement directly into an initializer.

   All distinct virtual function tables that this type uses
   must be updated.  */
static void
modify_vtable_entries (t, fndecl, base_fndecl, pfn)
     tree t;
     tree fndecl, base_fndecl, pfn;
{
  tree base_offset, offset;
  tree context = DECL_CONTEXT (base_fndecl);
  tree vfield = CLASSTYPE_VFIELD (t);
  tree vfields, vbases;

  DECL_VCONTEXT (fndecl) = DECL_VCONTEXT (base_fndecl);

  if (DECL_CONTEXT (fndecl) == t
      || !(TYPE_USES_VIRTUAL_BASECLASSES (t)
	   || TYPE_USES_MULTIPLE_INHERITANCE (t)))
    offset = integer_zero_node;
  else
    {
      tree assoc = virtual_member (DECL_CONTEXT (fndecl), CLASSTYPE_VBASECLASSES (t));
      if (assoc == NULL_TREE)
	assoc = assoc_value (DECL_CONTEXT (fndecl), t);
      assert (assoc != NULL_TREE);
      offset = ASSOC_OFFSET (assoc);
    }

  /* For each layer of base class (i.e., the first base class, and each
     virtual base class from that one), modify the virtual function table
     of the derived class to contain the new virtual function.
     A class has as many vfields as it has virtual base classes (total).  */
  for (vfields = CLASSTYPE_VFIELDS (t); vfields; vfields = TREE_CHAIN (vfields))
    {
      int normal = 1;
      tree assoc, this_offset;
      tree base, path;

      /* Find the right base class for this derived class, call it BASE.  */
      base = TREE_VALUE (vfields);

      if (base != context)
	{
	  /* If BASE_FNDECL is not contained in the vtable accessed by
	     the vslot, don't try to modify the vtable.
	     
	     Virtual functions from virtual baseclasses are not in derived
	     virtual function tables.  This is an implementation decision;
	     it keeps there from being a combinatorial exposion in the
	     number of different vtables which must be maintained.  */

	  if (get_base_distance (base, context, 0, 0) == -1)
	    continue;

	  /* BASE_FNDECL is defined in a class derived from
	     the base class owning this VFIELD.  */
	}
      /* Get the path starting from the deepest base class CONTEXT
	 of T (i.e., first defn of BASE_FNDECL).  */
      get_base_distance (context, t, 0, &path);

      /* Get our best approximation of what to use for constructing
	 the virtual function table for T.  */
      do
	{
	  /* Walk from base toward derived, stopping at the
	     most derived baseclass that matters.  */
	  if (TREE_VIA_VIRTUAL (path))
	    {
	      base = TREE_VALUE (path);
	      assoc = value_member (TYPE_MAIN_VARIANT (base), CLASSTYPE_VBASECLASSES (t));
	      break;
	    }
	  if (TREE_CHAIN (path) == NULL_TREE
	      || (CLASSTYPE_BASECLASS (TREE_VALUE (TREE_CHAIN (path)), 1)
		  != TREE_VALUE (path))
	      || TREE_CHAIN (TREE_CHAIN (path)) == NULL_TREE)
	    {
	      base = TREE_VALUE (path);
	      assoc = assoc_value (TYPE_MAIN_VARIANT (base), t);
	      break;
	    }
	  path = TREE_CHAIN (path);
	}
      while (1);

      /* Find the right offset for the this pointer based on the base
	 class we just found.  */
      base_offset = ASSOC_OFFSET (assoc);
      if (base_offset == integer_zero_node)
	this_offset = offset;
      else
	this_offset = genop (MINUS_EXPR, offset, base_offset);

      /* Make sure we can modify the derived association with immunity.  */
      if (TREE_USED (CLASSTYPE_ASSOC (t)))
	CLASSTYPE_ASSOC (t) = copy_assoc (CLASSTYPE_ASSOC (t));

      /* We call this case NORMAL iff this virtual function table
	 pointer field has its storage reserved in this class.
	 This is normally the case without virtual baseclasses
	 or off-center multiple baseclasses.  */
      normal = (vfield != NULL_TREE
		&& TREE_VALUE (vfields) == DECL_FCONTEXT (vfield)
		&& (TREE_PURPOSE (vfields) == NULL_TREE
		    || ! TREE_VIA_VIRTUAL (TREE_PURPOSE (vfields))));

      if (normal && TREE_PURPOSE (vfields))
	/* Everything looks normal so far...check that we are really
	   working from VFIELD's basetype, and not some other appearance
	   of that basetype in the lattice.  */
	normal = (TREE_PURPOSE (vfields) == get_base_type (TREE_VALUE (vfields), t, 0));

      if (normal)
	{
	  /* In this case, it is *type*'s vtable we are modifying.  */
	  context = t;
	  if (! CLASSTYPE_MARKED4 (t))
	    build_vtable (assoc, t);
	  assoc = CLASSTYPE_ASSOC (t);
	}
      else
	{
	  /* This is our very own copy of `basetype' to play with.  */
	  if (! CLASSTYPE_MARKED4 (ASSOC_TYPE (assoc)))
	    prepare_fresh_vtable (assoc, CLASSTYPE_ASSOC (base), t);
	}

      modify_vtable_entry (get_vtable_entry (ASSOC_VIRTUALS (assoc), base_fndecl),
			   build_vtable_entry (this_offset, pfn),
			   fndecl, context);
    }
  for (vbases = CLASSTYPE_VBASECLASSES (t); vbases; vbases = TREE_CHAIN (vbases))
    {
      tree this_offset;
      tree base, path;

      if (! ASSOC_VTABLE (vbases))
	/* There are only two ways that a type can fail to have
	   virtual functions: neither it nor any of its base
	   types define virtual functions (in which case
	   no updating need be done), or virtual functions
	   accessible to it come from virtual base classes
	   (in which case we have or will get them modified
	   in other passes of this loop).  */
	continue;

      base = TREE_VALUE (vbases);
      path = NULL_TREE;

      if (base != context
	  && get_base_distance (context, base, 0, &path) == -1)
	continue;

      /* Doesn't matter if not actually from this virtual base class,
         but shouldn't come from deeper virtual baseclasses.  The enclosing
	 loop should take care of such baseclasses.  */
      while (path)
	{
	  if (TREE_VIA_VIRTUAL (path))
	    goto skip;
	  path = TREE_CHAIN (path);
	}

      base_offset = ASSOC_OFFSET (vbases);
      if (base_offset == integer_zero_node)
	this_offset = offset;
      else
	this_offset = genop (MINUS_EXPR, offset, base_offset);

      /* Make sure we can modify the derived association with immunity.  */
      if (TREE_USED (CLASSTYPE_ASSOC (t)))
	CLASSTYPE_ASSOC (t) = copy_assoc (CLASSTYPE_ASSOC (t));

      /* This is our very own copy of `basetype' to play with.  */
      if (! CLASSTYPE_MARKED4 (ASSOC_TYPE (vbases)))
	{
	  tree context_assoc = assoc_value (context, base);
	  prepare_fresh_vtable (vbases, context_assoc, t);
	}
      modify_vtable_entry (get_vtable_entry (ASSOC_VIRTUALS (vbases), base_fndecl),
			   build_vtable_entry (this_offset, pfn),
			   fndecl, context);
    skip: {}
    }
}

static tree
add_virtual_function (pending_virtuals, has_virtual, x, first)
     tree pending_virtuals;
     int *has_virtual;
     tree x;
     int first;
{
  int debug_vbase = 1;

  /* FUNCTION_TYPEs and OFFSET_TYPEs no longer freely
     convert to void *.  Make such a conversion here.  */
  tree vfn = build1 (ADDR_EXPR, ptr_type_node, x);
  TREE_LITERAL (vfn) = 1;
  TREE_ADDRESSABLE (x) = CLASSTYPE_VTABLE_NEEDS_WRITING (current_class_type);

  /* If the virtual function is a redefinition of a prior one,
     figure out in which base class the new definition goes,
     and if necessary, make a fresh virtual function table
     to hold that entry.  */
  if (DECL_VINDEX (x) == NULL_TREE)
    {
      tree entry = build_vtable_entry (integer_zero_node, vfn);

      /* Build a new INT_CST for this DECL_VINDEX.  */
#ifdef VTABLE_USES_MASK
      SET_DECL_VINDEX (x, build_int_2 (++(*has_virtual), 0));
#else
      SET_DECL_VINDEX (x, build_int_2 (((1 << (BITS_PER_WORD - 1)) | ++(*has_virtual)), ~0));
#endif
      pending_virtuals = tree_cons (DECL_VINDEX (x), entry, pending_virtuals);
    }
  /* Happens if declared twice in class.  We will give error
     later.  */
  else if (TREE_CODE (DECL_VINDEX (x)) == INTEGER_CST)
    return pending_virtuals;
  else if (debug_vbase && TYPE_USES_VIRTUAL_BASECLASSES (current_class_type))
    {
      /* Need an entry in some other virtual function table.
         Deal with this after we have laid out our virtual base classes.  */
      pending_hard_virtuals = temp_tree_cons (x, vfn, pending_hard_virtuals);
    }
  else
    {
      /* Need an entry in some other virtual function table.
         We can do this now.  */
      tree base_fndecl_list = DECL_VINDEX (x), base_fndecls, prev = 0;
      tree vtable_context = DECL_FCONTEXT (CLASSTYPE_VFIELD (current_class_type));
      tree true_base_fndecl = 0;

      /* First assign DECL_VINDEX from the base vfn with which
	 we share our vtable.  */
      base_fndecls = base_fndecl_list;
      while (base_fndecls)
	{
	  if (TREE_CHAIN (base_fndecls) == NULL_TREE
	      || DECL_FCONTEXT (CLASSTYPE_VFIELD (DECL_CONTEXT (TREE_VALUE (base_fndecls)))) == vtable_context)
	    {
	      true_base_fndecl = TREE_VALUE (base_fndecls);
	      modify_vtable_entries (current_class_type, x,
				     true_base_fndecl, vfn);
	      if (prev)
		TREE_CHAIN (prev) = TREE_CHAIN (base_fndecls);
	      else
		base_fndecl_list = prev;
	      break;
	    }
	  prev = base_fndecls;
	  base_fndecls = TREE_CHAIN (base_fndecls);
	}

      /* Now fill in the rest of the vtables.  */
      base_fndecls = base_fndecl_list;
      while (base_fndecls)
	{
	  /* If we haven't found one we like, first one wins.  */
	  if (true_base_fndecl == 0)
	    true_base_fndecl = TREE_VALUE (base_fndecls);

	  modify_vtable_entries (current_class_type, x,
				 TREE_VALUE (base_fndecls), vfn);
	  base_fndecls = TREE_CHAIN (base_fndecls);
	}

      DECL_VCONTEXT (x) = DECL_VCONTEXT (true_base_fndecl);
    }
  return pending_virtuals;
}

/* Obstack on which to build the vector of class methods.  */
struct obstack class_obstack;
extern struct obstack *current_obstack;

/* Add method METHOD to class TYPE.  This is used when a method
   has been defined which did not initially appear in the class definition,
   and helps cut down on spurious error messages.

   FIELDS is the entry in the METHOD_VEC vector entry of the class type where
   the method should be added.  */
void
add_method (type, fields, method)
     tree type, *fields, method;
{
  /* We must make a copy of METHOD here, since we must be sure that
     we have exclusive title to this method's TREE_CHAIN.  */
  int temp = allocation_temporary_p ();
  tree decl;

  if (temp)
    end_temporary_allocation ();
  {
    decl = copy_node (method);
    if (DECL_RTL (decl) == 0)
      make_function_rtl (decl);
  }

  if (fields && *fields)
    {
      /* Take care not to hide destructor.  */
      TREE_CHAIN (decl) = TREE_CHAIN (*fields);
      TREE_CHAIN (*fields) = decl;
    }
  else if (CLASSTYPE_METHOD_VEC (type) == 0)
    {
      tree method_vec = make_node (TREE_VEC);
      if (DECL_NAME (TYPE_NAME (type)) == DECL_ORIGINAL_NAME (decl))
	{
	  TREE_VEC_ELT (method_vec, 0) = decl;
	  TREE_VEC_LENGTH (method_vec) = 1;
	}
      else
	{
	  obstack_free (current_obstack, method_vec);
	  obstack_blank (current_obstack, sizeof (struct tree_vec) + sizeof (tree *));
	  TREE_VEC_ELT (method_vec, 1) = decl;
	  TREE_VEC_LENGTH (method_vec) = 2;
	  obstack_finish (current_obstack);
	}
      CLASSTYPE_METHOD_VEC (type) = method_vec;
    }
  else
    {
      tree method_vec = CLASSTYPE_METHOD_VEC (type);
      int len = TREE_VEC_LENGTH (method_vec);

      /* Adding a new ctor or dtor.  */
      if (DECL_NAME (TYPE_NAME (type)) == DECL_ORIGINAL_NAME (decl))
	TREE_VEC_ELT (method_vec, 0) = decl;
      else
	{
	  tree *end = (tree *)obstack_next_free (&class_obstack);
	  if (end != TREE_VEC_END (method_vec))
	    {
	      tree tmp_vec = copy_node (method_vec);
	      obstack_copy (current_obstack, &TREE_VEC_ELT (method_vec, 1), len);
	      obstack_blank (current_obstack, sizeof (tree *));
	      obstack_finish (current_obstack);
	      method_vec = tmp_vec;
	    }
	  else
	    {
	      /* We can easily extend the last such method_vec created.  */
	      obstack_free (&class_obstack, method_vec);
	      obstack_blank (&class_obstack,
			     ((char *)end - (char *)method_vec) + sizeof (tree *));
	      method_vec = (tree)obstack_base (&class_obstack);
	      obstack_finish (&class_obstack);
	    }
	  TREE_VEC_ELT (method_vec, len) = decl;
	  TREE_VEC_LENGTH (method_vec) = len + 1;
	  CLASSTYPE_METHOD_VEC (type) = method_vec;

	  if (CLASSTYPE_BASELINK_VEC (type))
	    {
	      /* ??? May be better to know whether these can be extended?  */
	      tree baselink_vec = copy_node (CLASSTYPE_BASELINK_VEC (type));

	      obstack_copy (current_obstack, &TREE_VEC_ELT (CLASSTYPE_BASELINK_VEC (type), 1), len);
	      TREE_VEC_ELT (baselink_vec, len) = 0;
	      TREE_VEC_LENGTH (baselink_vec) = len + 1;
	      CLASSTYPE_BASELINK_VEC (type) = baselink_vec;
	    }
	}
    }
  DECL_CONTEXT (decl) = type;
  DECL_VCONTEXT (decl) = type;

  if (temp)
    resume_temporary_allocation ();
}

/* Subroutines of finish_struct.  */

/* Look through the list of fields for this struct, deleting
   duplicates as we go.  This must be recursive to handle
   anonymous unions.

   FIELD is the field which may not appear anywhere in FIELDS.
   FIELD_PTR, if non-null, is the starting point at which
   chained deletions may take place.
   The value returned is the first acceptable entry found
   in FIELDS.

   Note that anonymous fields which are not of UNION_TYPE are
   not duplicates, they are just anonymous fields.  This happens
   when we have unnamed bitfields, for example.  */
static tree
delete_duplicate_fields_1 (field, field_ptr, fields)
     tree field, *field_ptr, fields;
{
  tree x;
  tree prev = field_ptr ? *field_ptr : 0;
  if (DECL_NAME (field) == 0)
    {
      if (TREE_CODE (TREE_TYPE (field)) != UNION_TYPE)
	return fields;

      for (x = TYPE_FIELDS (TREE_TYPE (field)); x; x = TREE_CHAIN (x))
	fields = delete_duplicate_fields_1 (x, field_ptr, fields);
      if (prev)
	TREE_CHAIN (prev) = fields;
      return fields;
    }
  else
    {
      for (x = fields; x; prev = x, x = TREE_CHAIN (x))
	{
	  if (DECL_NAME (x) == 0)
	    {
	      if (TREE_CODE (TREE_TYPE (x)) != UNION_TYPE)
		continue;
	      TYPE_FIELDS (TREE_TYPE (x))
		= delete_duplicate_fields_1 (field, 0, TYPE_FIELDS (TREE_TYPE (x)));
	      if (TYPE_FIELDS (TREE_TYPE (x)) == 0)
		{
		  if (prev == 0)
		    fields = TREE_CHAIN (fields);
		  else
		    TREE_CHAIN (prev) = TREE_CHAIN (x);
		}
	    }
	  else
	    {
	      if (DECL_NAME (field) == DECL_NAME (x))
		{
		  if (TREE_CODE (field) == CONST_DECL
		      && TREE_CODE (x) == CONST_DECL)
		    error_with_decl (x, "duplicate enum value `%s'");
		  else if (TREE_CODE (field) == CONST_DECL
			   || TREE_CODE (x) == CONST_DECL)
		    error_with_decl (x, "duplicate field `%s' (as enum and non-enum)");
		  else
		    error_with_decl (x, "duplicate member `%s'");
		  if (prev == 0)
		    fields = TREE_CHAIN (fields);
		  else
		    TREE_CHAIN (prev) = TREE_CHAIN (x);
		}
	    }
	}
    }
  return fields;
}

static void
delete_duplicate_fields (fields)
     tree fields;
{
  tree x;
  for (x = fields; x && TREE_CHAIN (x); x = TREE_CHAIN (x))
    TREE_CHAIN (x) = delete_duplicate_fields_1 (x, &x, TREE_CHAIN (x));
}

/* Add OFFSET to all child types of T.

   OFFSET, which is a type offset, is number of bytes.

   Note that we don't have to worry about having two paths to the
   same base type, since this type owns its association list.  */
static void
propagate_basetype_offsets (for_type, t, offset)
     tree for_type, t;
     tree offset;
{
  int i, n_baselinks = CLASSTYPE_N_BASECLASSES (t);

  for (i = 1; i <= n_baselinks; i++)
    if (! CLASSTYPE_VIA_VIRTUAL (t, i))
      {
	int j;
	tree basetype = CLASSTYPE_BASECLASS (t, i);
	tree assoc = assoc_value (TYPE_MAIN_VARIANT (basetype), for_type);
	tree delta;

	for (j = i+1; j <= n_baselinks; j++)
	  if (! CLASSTYPE_VIA_VIRTUAL (t, j))
	    {
	      /* The next basetype offset must take into account the space
		 between the classes, not just the size of each class.  */
	      delta = genop (MINUS_EXPR,
			     CLASSTYPE_OFFSET (CLASSTYPE_BASECLASS (t, j)),
			     CLASSTYPE_OFFSET (basetype));
	      break;
	    }

	if (CLASSTYPE_OFFSET (basetype) == integer_zero_node)
	  basetype = build_classtype_variant (basetype, offset, 0);
	else
	  basetype = build_classtype_variant (basetype,
					      genop (PLUS_EXPR, CLASSTYPE_OFFSET (basetype), offset), 0);
	/* Now make our own copy of this base type we can munge.  */
	basetype = copy_node (basetype);
	copy_type_lang_specific (basetype);

	CLASSTYPE_BASECLASS (t, i) = basetype;
	ASSOC_TYPE (assoc) = basetype;
	ASSOC_OFFSET (assoc) = CLASSTYPE_OFFSET (basetype);
	TYPE_NAME (basetype) = copy_node (TYPE_NAME (basetype));
	TREE_TYPE (TYPE_NAME (basetype)) = basetype;
	DECL_OFFSET (TYPE_NAME (basetype))
	  = TREE_INT_CST_LOW (CLASSTYPE_OFFSET (basetype)) * BITS_PER_UNIT;
	propagate_basetype_offsets (for_type, basetype, offset);

	/* Go to our next class that counts for offset propagation.  */
	i = j;
	if (i <= n_baselinks)
	  offset = genop (PLUS_EXPR, offset, delta);
      }
}

/* Change the visibility of T::FDECL to VISIBILITY.
   Return 1 if change was legit, otherwise return 0.  */
static int
alter_visibility (t, fdecl, visibility)
     tree t;
     tree fdecl;
     enum visibility_type visibility;
{
  tree elem = purpose_member (t, DECL_VISIBILITY (fdecl));
  if (elem && TREE_VALUE (elem) != (tree)visibility)
    {
      if (TREE_CODE (TREE_TYPE (fdecl)) == FUNCTION_DECL)
	{
	  error_with_decl (TREE_TYPE (fdecl), "conflicting visibility specifications for method `%s', ignored");
	}
      else error ("conflicting visibility specifications for field `%s', ignored", IDENTIFIER_POINTER (DECL_NAME (fdecl)));
    }
  else if (TREE_PRIVATE (fdecl) && visibility != visibility_private)
    error_with_decl (fdecl, "cannot make private %s non-private");
  else if (TREE_PROTECTED (fdecl) && visibility == visibility_public)
		    
    error_with_decl (fdecl, "cannot make protected %s public");
  else if (elem == NULL_TREE)
    {
      DECL_VISIBILITY (fdecl) = tree_cons (t, (tree)visibility,
					   DECL_VISIBILITY (fdecl));
      return 1;
    }
  return 0;
}

/* If FOR_TYPE needs to reinitialize virtual function table pointers
   for TYPE's sub-objects, add such reinitializations to BASE_INIT_LIST.
   Returns BASE_INIT_LIST appropriately modified.  */

static tree
maybe_fixup_vptrs (for_type, type, base_init_list)
     tree for_type, type, base_init_list;
{
  /* Now reinitialize any slots that don't fall under our virtual
     function table pointer.  */
  tree vfields = CLASSTYPE_VFIELDS (type);
  while (vfields)
    {
      tree basetype = get_base_type (TREE_VALUE (vfields), for_type, 0);
      if (CLASSTYPE_NEEDS_VIRTUAL_REINIT (basetype)
	  && ((DECL_OFFSET (CLASSTYPE_VFIELD (basetype))
	       + DECL_OFFSET (TYPE_NAME (basetype)))
	      != DECL_OFFSET (CLASSTYPE_VFIELD (for_type)))
	  && ((DECL_OFFSET (CLASSTYPE_VFIELD (basetype))
	       + DECL_OFFSET (TYPE_NAME (basetype)))
	      != (DECL_OFFSET (CLASSTYPE_VFIELD (type))
		  + DECL_OFFSET (TYPE_NAME (type)))))
	base_init_list = tree_cons (error_mark_node, basetype,
				    base_init_list);
      vfields = TREE_CHAIN (vfields);
    }
  return base_init_list;
}

/* If TYPE does not have a constructor, then the compiler must
   manually deal with all of the initialization this type requires.

   If a base initializer exists only to fill in the virtual function
   table pointer, then we mark that fact with the TREE_VIRTUAL bit.
   This way, we avoid multiple initializations of the same field by
   each virtual function table up the class hierarchy.

   Virtual base class pointers are not initialized here.  They are
   initialized only at the "top level" of object creation.  If we
   initialized them here, we would have to skip a lot of work.  */

static void
build_class_init_list (type)
     tree type;
{
  tree base_init_list = NULL_TREE;
  tree member_init_list = NULL_TREE;

  /* Since we build member_init_list and base_init_list using
     tree_cons, backwards fields the all through work.  */
  tree x;
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (type);

  for (x = TYPE_FIELDS (type); x; x = TREE_CHAIN (x))
    {
      if (TREE_CODE (x) != FIELD_DECL)
	continue;

      if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (x))
	  || DECL_INITIAL (x) != NULL_TREE)
	member_init_list = tree_cons (x, type, member_init_list);
    }
  member_init_list = nreverse (member_init_list);

  /* We will end up doing this last.  Need special marker
     to avoid infinite regress.  */
  if (TYPE_VIRTUAL_P (type))
    {
      base_init_list = build_tree_list (error_mark_node, type);
      if (CLASSTYPE_NEEDS_VIRTUAL_REINIT (type) == 0)
	TREE_VALUE (base_init_list) = NULL_TREE;
      TREE_ADDRESSABLE (base_init_list) = 1;
    }

  /* Each base class which needs to have initialization
     of some kind gets to make such requests known here.  */
  for (i = n_baseclasses; i > 0; i--)
    {
      tree basetype = CLASSTYPE_BASECLASS (type, i);
      tree blist;

      /* Don't initialize virtual baseclasses this way.  */
      if (TREE_VIA_VIRTUAL (basetype))
	continue;

      if (TYPE_HAS_CONSTRUCTOR (basetype))
	{
	  /* ...and the last shall come first...  */
	  base_init_list = maybe_fixup_vptrs (type, basetype, base_init_list);
	  base_init_list = tree_cons (NULL_TREE, basetype,
				      base_init_list);
	  continue;
	}

      if ((blist = CLASSTYPE_BASE_INIT_LIST (basetype)) == NULL_TREE)
	/* Nothing to initialize.  */
	continue;

      /* ...ditto...  */
      base_init_list = maybe_fixup_vptrs (type, basetype, base_init_list);

      /* This is normally true for single inheritance.
	 The win is we can shrink the chain of initializations
	 to be done by only converting to the actual type
	 we are interested in.  */
      if (TREE_VALUE (blist)
	  && TREE_CODE (TREE_VALUE (blist)) == RECORD_TYPE
	  && (DECL_OFFSET (TYPE_NAME (basetype))
	      == DECL_OFFSET (TYPE_NAME (TREE_VALUE (blist)))))
	{
	  if (base_init_list)
	    {
	      /* Does it do more than just fill in a
		 virtual function table pointer?  */
	      if (! TREE_ADDRESSABLE (blist))
		base_init_list = build_tree_list (blist, base_init_list);
	      /* Can we get by just with the virtual function table
		 pointer that it fills in?  */
	      else if (TREE_ADDRESSABLE (base_init_list)
		       && TREE_VALUE (base_init_list) == 0)
		base_init_list = blist;
	      /* Maybe, but it is not obvious as the previous case.  */
	      else if (! CLASSTYPE_NEEDS_VIRTUAL_REINIT (type))
		{
		  tree last = tree_last (base_init_list);
		  while (TREE_VALUE (last)
			 && TREE_CODE (TREE_VALUE (last)) == TREE_LIST)
		    last = tree_last (TREE_VALUE (last));
		  if (TREE_VALUE (last) == 0)
		    base_init_list = build_tree_list (blist, base_init_list);
		}
	    }
	  else
	    base_init_list = blist;
	}
      else
	{
	  /* The function expand_aggr_init knows how to do the
	     initialization of `basetype' without getting
	     an explicit `blist'.  */
	  if (base_init_list)
	    base_init_list = tree_cons (NULL_TREE, basetype, base_init_list);
	  else
	    base_init_list = CLASSTYPE_AS_LIST (basetype);
	}
    }

  if (base_init_list)
    if (member_init_list)
      CLASSTYPE_BASE_INIT_LIST (type) = build_tree_list (base_init_list, member_init_list);
    else
      CLASSTYPE_BASE_INIT_LIST (type) = base_init_list;
  else if (member_init_list)
    CLASSTYPE_BASE_INIT_LIST (type) = member_init_list;
}

struct base_info
{
  int has_virtual;
  int max_has_virtual;
  int n_ancestors;
  tree vfield;
  tree vfields;
  char needs_default_ctor;
  char cant_have_default_ctor;
  char needs_const_ctor;
  char cant_have_const_ctor;
};

/* Record information about type T derived from its base classes.
   Store most of that information in T itself, and place the
   remaining information in the struct BASE_INFO.

   Returns the index of the first base class to have virtual functions,
   or zero if no such base class.  */

static int
finish_base_struct (t, b)
     tree t;
     struct base_info *b;
{
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  int first_vfn_base_index = 0;

  bzero (b, sizeof (struct base_info));

  for (i = 1; i <= n_baseclasses; i++)
    {
      tree basetype = CLASSTYPE_BASECLASS (t, i);

      /* If the type of basetype is incomplete, then
	 we already complained about that fact
	 (and we should have fixed it up as well).  */
      if (TYPE_SIZE (basetype) == 0)
	{
	  int j;
	  /* The base type is of incomplete type.  It is
	     probably best to pretend that it does not
	     exist.  */
	  if (i == n_baseclasses)
	    CLASSTYPE_BASECLASS (t, i) = NULL_TREE;
	  CLASSTYPE_N_BASECLASSES (t) -= 1;
	  n_baseclasses -= 1;
	  for (j = i; j < n_baseclasses; j++)
	    {
	      CLASSTYPE_BASECLASS (t, j) = CLASSTYPE_BASECLASS (t, j+1);
	      SET_CLASSTYPE_VIAS (t, j,
				  CLASSTYPE_VIA_PUBLIC (t, j+1),
				  CLASSTYPE_VIA_VIRTUAL (t, j+1));
	    }
	}

      if (TYPE_WRAP_TYPE (t) == NULL_TREE)
	TYPE_WRAP_TYPE (t) = TYPE_WRAP_TYPE (basetype);
      else if (TYPE_WRAP_TYPE (basetype)
	       && TYPE_WRAP_TYPE (t) != TYPE_WRAP_TYPE (basetype))
	/* Must have its own.  */
	TYPE_WRAP_TYPE (t) = error_mark_node;

      if (TYPE_HAS_DEFAULT_CONSTRUCTOR (basetype))
	b->needs_default_ctor = 1;
      else if (TYPE_HAS_CONSTRUCTOR (basetype))
	b->cant_have_default_ctor = 1;
      if (TYPE_GETS_CONST_INIT_REF (basetype))
	b->needs_const_ctor = 1;
      else if (TYPE_GETS_INIT_REF (basetype))
	b->cant_have_const_ctor = 1;

      CLASSTYPE_ALTERS_VISIBILITIES_P (t)
	|= CLASSTYPE_ALTERS_VISIBILITIES_P (basetype);

      b->n_ancestors += CLASSTYPE_N_SUPERCLASSES (basetype);
      TYPE_NEEDS_CONSTRUCTING (t) |= TYPE_NEEDS_CONSTRUCTING (basetype);
      TYPE_NEEDS_CONSTRUCTOR (t) |= TYPE_NEEDS_CONSTRUCTOR (basetype);
      TYPE_NEEDS_DESTRUCTOR (t) |= TYPE_NEEDS_DESTRUCTOR (basetype);
      TYPE_ANY_ASSIGNS_THIS (t) |= TYPE_ANY_ASSIGNS_THIS (basetype);
      TYPE_GETS_ASSIGNMENT (t) |= TYPE_GETS_ASSIGNMENT (basetype);
      TYPE_GETS_INIT_REF (t) |= TYPE_GETS_INIT_REF (basetype);

      TYPE_OVERLOADS_CALL_EXPR (t) |= TYPE_OVERLOADS_CALL_EXPR (basetype);
      TYPE_OVERLOADS_ARRAY_REF (t) |= TYPE_OVERLOADS_ARRAY_REF (basetype);

      if (CLASSTYPE_OFFSET (basetype) != integer_zero_node)
	{
	  /* Completely unshare potentially shared data, and
	     update what is ours.  */
	  tree assoc = assoc_value (TYPE_MAIN_VARIANT (basetype), t);
	  basetype = copy_node (basetype);
	  copy_type_lang_specific (basetype);
	  CLASSTYPE_BASECLASS (t, i) = basetype;
	  ASSOC_TYPE (assoc) = basetype;

	  /* Propagate this offset through all the children.  Do this
	     before uniquizing baseclasses for virtual functions.  */
	  CLASSTYPE_ASSOC (basetype) = copy_assoc (CLASSTYPE_ASSOC (TYPE_MAIN_VARIANT (basetype)));

	  propagate_basetype_offsets (basetype, basetype, CLASSTYPE_OFFSET (basetype));
	}

      if (! CLASSTYPE_VIA_VIRTUAL (t, i))
	CLASSTYPE_N_SUPERCLASSES (t) += 1;

      if (TYPE_VIRTUAL_P (basetype))
	{
	  if (CLASSTYPE_VSIZE (basetype) > b->max_has_virtual)
	    b->max_has_virtual = CLASSTYPE_VSIZE (basetype);

	  /* Don't borrow virtuals from virtual baseclasses.  */
	  if (TREE_VIA_VIRTUAL (basetype))
	    continue;

	  if (first_vfn_base_index == 0)
	    {
	      first_vfn_base_index = i;

	      b->has_virtual = CLASSTYPE_VSIZE (basetype);
	      b->vfield = CLASSTYPE_VFIELD (basetype);
	      b->vfields = CLASSTYPE_VFIELDS (basetype);
	      CLASSTYPE_VFIELD (t) = b->vfield;
	    }
	  else
	    {
	      /* Only add unique vfields, and flatten them out as we go.  */
	      tree vfields = CLASSTYPE_VFIELDS (basetype);
	      while (vfields)
		{
		  if (TREE_PURPOSE (vfields) == NULL_TREE
		      || ! TREE_VIA_VIRTUAL (TREE_PURPOSE (vfields)))
		    {
		      tree value = TREE_VALUE (vfields);
		      if (TYPE_MAIN_VARIANT (basetype) == value)
			b->vfields = tree_cons (basetype, value, b->vfields);
		      else
			b->vfields = tree_cons (get_base_type (value, basetype, 0),
						value, b->vfields);
		      TREE_TYPE (b->vfields) = basetype;
		    }
		  vfields = TREE_CHAIN (vfields);
		}

	      if (b->has_virtual == 0)
		{
		  first_vfn_base_index = i;
		  b->has_virtual = CLASSTYPE_VSIZE (basetype);
		  b->vfield = CLASSTYPE_VFIELD (basetype);
		  CLASSTYPE_VFIELD (t) = b->vfield;
		}
	    }
	}
    }
  if (b->vfield == 0)
    /* If all virtual functions come only from virtual baseclasses.  */
    return 0;
  return first_vfn_base_index;
}

static int
typecode_p (type, code)
     tree type;
     enum tree_code code;
{
  return (TREE_CODE (type) == code
	  || (TREE_CODE (type) == REFERENCE_TYPE
	      && TREE_CODE (TREE_TYPE (type)) == code));
}

/* Set memoizing fields and bits of T (and its variants) for later use.
   FIRST_VFN_BASE_INDEX is the first baseclass of T with virtual functions.
   MAX_HAS_VIRTUAL is the largest size of any T's virtual function tables.  */
static void
finish_struct_bits (t, first_vfn_base_index, max_has_virtual)
     tree t;
     int first_vfn_base_index, max_has_virtual;
{
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  tree method_vec = CLASSTYPE_METHOD_VEC (t);

  /* Fix up variants (if any).  */
  tree variants = TYPE_NEXT_VARIANT (t);
  while (variants)
    {
      TYPE_NEEDS_CONSTRUCTOR (variants) = TYPE_NEEDS_CONSTRUCTOR (t);
      TYPE_NEEDS_CONSTRUCTING (variants) = TYPE_NEEDS_CONSTRUCTING (t);
      TYPE_NEEDS_DESTRUCTOR (variants) = TYPE_NEEDS_DESTRUCTOR (t);
      variants = TYPE_NEXT_VARIANT (variants);
    }

  if (n_baseclasses && max_has_virtual)
    {
      /* Done by `finish_struct' for classes without baseclasses.  */
      int has_abstract_virtuals = CLASSTYPE_ABSTRACT_VIRTUALS (t) != 0;
      if (has_abstract_virtuals == 0)
	for (i = CLASSTYPE_N_BASECLASSES (t); i >= 1; i--)
	  has_abstract_virtuals
	    |= (CLASSTYPE_ABSTRACT_VIRTUALS (CLASSTYPE_BASECLASS (t, i)) != 0);
      if (has_abstract_virtuals)
	CLASSTYPE_ABSTRACT_VIRTUALS (t) = get_abstract_virtuals (t);
    }

  if (n_baseclasses)
    {
      /* Notice whether this class has type conversion functions defined.
	 Also report whether joining two types yields an ambiguity in the
	 virtual function table, e.g.,
	 
	 struct A { virtual int f (); };
	 struct B { virtual int f (); };
	 struct C : A, B { / * no f (); * / };	/ / error, ambiguous
	 */
      tree basetypes = CLASSTYPE_VBASECLASSES (t), basetype;
      int n_vbases = list_length (basetypes), j;

      build_mi_virtuals (n_baseclasses + (n_vbases*n_baseclasses), max_has_virtual);
      /* Fill in virutal function table with values which do not come
	 "normal"ly, i.e., those which come from virtual and/or
	 non-leftmost base classes.  */
      for (i = n_baseclasses; basetypes; basetypes = TREE_CHAIN (basetypes))
	{
	  basetype = TREE_VALUE (basetypes);
	  if (CLASSTYPE_VSIZE (basetype))
	    for (j = n_baseclasses; j > 0; j--)
	      {
		tree this_base = CLASSTYPE_BASECLASS (t, j);
		if (get_base_distance (basetype, TYPE_MAIN_VARIANT (this_base), 0, 0) != -1)
		  add_mi_virtuals (++i, TREE_CHAIN (ASSOC_VIRTUALS (basetypes)));
	      }
	}
      for (i = n_baseclasses; i > 0; i--)
	{
	  basetype = CLASSTYPE_BASECLASS (t, i);

	  if (TYPE_HAS_CONVERSION (basetype))
	    {
	      TYPE_HAS_CONVERSION (t) = 1;
	      TYPE_HAS_INT_CONVERSION (t) |= TYPE_HAS_INT_CONVERSION (basetype);
	      TYPE_HAS_REAL_CONVERSION (t) |= TYPE_HAS_REAL_CONVERSION (basetype);
	    }
	  if (TREE_VIA_VIRTUAL (basetype))
	    /* Virtual functions from virtual baseclasses are done above.  */;
	  else if (i == first_vfn_base_index)
	    add_mi_virtuals (i, TREE_CHAIN (CLASS_ASSOC_VIRTUALS (t)));
	  else if (CLASSTYPE_VSIZE (basetype) != 0)
	    add_mi_virtuals (i, TREE_CHAIN (CLASS_ASSOC_VIRTUALS (basetype)));
	}
      report_ambiguous_mi_virtuals (n_baseclasses + (n_vbases*n_baseclasses), t);
#if 0
      /* Now that we know what the virtual functiond table looks like,
	 fix up offsets in the presence of virtual base classes.  */
      if (n_vbases)
	fixup_vbase_offsets (t);
#endif
    }

  /* Need to test METHOD_VEC here in case all methods
     (conversions and otherwise) are inherited.  */
  if (TYPE_HAS_CONVERSION (t) && method_vec != NULL_TREE)
    {
      tree first_conversions[last_conversion_type];
      tree last_conversions[last_conversion_type];
      enum conversion_type conv_index;
      tree *tmp;
      int i;

      bzero (first_conversions, sizeof (first_conversions));
      for (tmp = &TREE_VEC_ELT (method_vec, 1);
	   tmp != TREE_VEC_END (method_vec); tmp += 1)
	{
	  if (OPERATOR_TYPENAME_P (DECL_ORIGINAL_NAME (*tmp)))
	    {
	      tree fntype = TREE_TYPE (*tmp);
	      tree return_type = TREE_TYPE (fntype);
	      assert (TREE_CODE (fntype) == METHOD_TYPE);

	      if (typecode_p (return_type, POINTER_TYPE))
		{
		  if (TREE_READONLY (TREE_TYPE (return_type)))
		    conv_index = constptr_conv;
		  else
		    conv_index = ptr_conv;
		}
	      else if (typecode_p (return_type, INTEGER_TYPE))
		{
		  TYPE_HAS_INT_CONVERSION (t) = 1;
		  conv_index = int_conv;
		}
	      else if (typecode_p (return_type, REAL_TYPE))
		{
		  TYPE_HAS_REAL_CONVERSION (t) = 1;
		  conv_index = real_conv;
		}
	      else
		continue;

	      if (first_conversions[(int) conv_index] == NULL_TREE)
		first_conversions[(int) conv_index] = *tmp;
	      last_conversions[(int) conv_index] = *tmp;
	    }
	}

      for (i = 0; i < (int) last_conversion_type; i++)
	if (first_conversions[i] != last_conversions[i])
	  CLASSTYPE_CONVERSION (t, i) = error_mark_node;
	else
	  CLASSTYPE_CONVERSION (t, i) = first_conversions[i];
    }

  /* If this type has constructors, force its mode to be BLKmode,
     and force its TREE_ADDRESSABLE bit to be nonzero.  */
  if (TYPE_NEEDS_CONSTRUCTING (t) || TYPE_NEEDS_DESTRUCTOR (t))
    {
      tree variants = t;

      if (TREE_CODE (TYPE_NAME (t)) == TYPE_DECL)
	DECL_MODE (TYPE_NAME (t)) = BLKmode;
      while (variants)
	{
	  TYPE_MODE (variants) = BLKmode;
	  TREE_ADDRESSABLE (variants) = 1;
	  variants = TYPE_NEXT_VARIANT (variants);
	}
    }
}

/* Create a RECORD_TYPE or UNION_TYPE node for a C struct or union declaration
   (or C++ class declaration).

   For C++, we must handle the building of derived classes.
   Also, C++ allows static class members.  The way that this is
   handled is to keep the field name where it is (as the DECL_NAME
   of the field), and place the overloaded decl in the DECL_OFFSET
   of the field.  layout_record and layout_union will know about this.

   More C++ hair: inline functions have text in their
   DECL_PENDING_INLINE_INFO nodes which must somehow be parsed into
   meaningful tree structure.  After the struct has been laid out, set
   things up so that this can happen.

   And still more: virtual functions.  In the case of single inheritance,
   when a new virtual function is seen which redefines a virtual function
   from the base class, the new virtual function is placed into
   the virtual function table at exactly the same address that
   it had in the base class.  When this is extended to multiple
   inheritance, the same thing happens, except that multiple virtual
   function tables must be maintained.  The first virtual function
   table is treated in exactly the same way as in the case of single
   inheritance.  Additional virtual function tables have different
   DELTAs, which tell how to adjust `this' to point to the right thing.

   LIST_OF_FIELDLISTS is just that.  The elements of the list are
   TREE_LIST elements, whose TREE_PURPOSE field tells what visibility
   the list has, and the TREE_VALUE slot gives the actual fields.

   EMPTY is non-zero if this structure has no declarations following it.

   If flag_all_virtual == 1, then we lay all functions into
   the virtual function table, as though they were declared
   virtual.  Constructors do not lay down in the virtual function table.

   If flag_all_virtual == 2, then we lay all functions into
   the virtual function table, such that virtual functions
   occupy a space by themselves, and then all functions
   of the class occupy a space by themselves.  This is illustrated
   in the following diagram:

   class A; class B : A;

	Class A's vtbl:			Class B's vtbl:
    --------------------------------------------------------------------
   | A's virtual functions|		| B's virtual funcitions	|
   |			  |		| (may inherit some from A).	|
    --------------------------------------------------------------------
   | All of A's functions |		| All of A's functions		|
   | (such as a->A::f).	  |		| (such as b->A::f)		|
    --------------------------------------------------------------------
					| B's new virtual functions	|
					| (not defined in A.)		|
					 -------------------------------
					| All of B's functions		|
					| (such as b->B::f)		|
					 -------------------------------

   this allows the program to make references to any function, virtual
   or otherwise in a type-consistant manner.  */

tree
finish_struct (t, list_of_fieldlists, empty, warn_anon)
     tree t;
     tree list_of_fieldlists;
     int empty;
     int warn_anon;
{
  extern int interface_only, interface_unknown;
  int old;
  int round_up_size = 1;
  /* Set non-zero to debug using default functions.
     Not set by program.  */
  static int debug_default_functions = 0;

  enum tree_code code = TREE_CODE (t);
  register tree x, y, method_vec;
  int needs_ctor = 0, needs_dtor = 0;
  int members_need_dtors = 0;
  tree name = TYPE_NAME (t), fields, fn_fields, tail;
  enum visibility_type visibility;
  int all_virtual;
  int has_virtual;
  int max_has_virtual;
  tree pending_virtuals = NULL_TREE;
  tree abstract_virtuals = NULL_TREE;
  tree vfield;
  tree vfields;
  int needs_default_ctor;
  int cant_have_default_ctor;
  int needs_const_ctor;
  int cant_have_const_ctor;

  /* The index of the first base class which has virtual
     functions.  Only applied to non-virtual baseclasses.  */
  int first_vfn_base_index;

  int i, n_baseclasses;
  int any_default_members = 0;
  char *err_name;
  int const_sans_init = 0;
  int ref_sans_init = 0;
  int nonprivate_method = 0;

  if (TREE_CODE (name) == TYPE_DECL)
    {
      extern int lineno;

      DECL_SOURCE_FILE (name) = input_filename;
      DECL_SOURCE_LINE (name) = lineno;
      name = DECL_NAME (name);
    }
  err_name = IDENTIFIER_POINTER (name);

  if (warn_anon && code != UNION_TYPE && ANON_AGGRNAME_P (name))
    {
      warning ("un-usable class ignored (anonymous classes and unions are useless)");
      err_name = "(anon)";
    }

  leftmost_baseclasses = NULL_TREE;
  if (TYPE_SIZE (t))
    {
      if (TREE_CODE (t) == UNION_TYPE)
	error ("redefinition of `union %s'", err_name);
      else if (TREE_CODE (t) == RECORD_TYPE)
	error ("redefinition of `struct %s'", err_name);
      else
	assert (0);
      popclass (0);
      return t;
    }

#ifdef FIELD_XREF
  FIELD_xref_decl(current_function_decl,t);
#endif

  /* If this type was previously laid out as a forward reference,
     make sure we lay it out again.  */

  TYPE_SIZE (t) = 0;
  CLASSTYPE_GOT_SEMICOLON (t) = 0;
  CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
  CLASSTYPE_INTERFACE_UNKNOWN (t) = interface_unknown;

  old = suspend_momentary ();

  /* Install struct as DECL_FIELD_CONTEXT of each field decl.
     Also process specified field sizes.
     Set DECL_SIZE_UNIT to the specified size, or 0 if none specified.
     The specified size is found in the DECL_INITIAL.
     Store 0 there, except for ": 0" fields (so we can find them
     and delete them, below).  */

  n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  if (n_baseclasses >= 1)
    {
      struct base_info base_info;

      /* If using multiple inheritance, this may cause variants of our
	 basetypes to be used (instead of their canonical forms).  */
      fields = layout_basetypes (t);
      y = tree_last (fields);

      first_vfn_base_index = finish_base_struct (t, &base_info);
      has_virtual = base_info.has_virtual;
      max_has_virtual = base_info.max_has_virtual;
      CLASSTYPE_N_SUPERCLASSES (t) += base_info.n_ancestors;
      vfield = base_info.vfield;
      vfields = base_info.vfields;
      needs_default_ctor = base_info.needs_default_ctor;
      cant_have_default_ctor = base_info.cant_have_default_ctor;
      needs_const_ctor = base_info.needs_const_ctor;
      cant_have_const_ctor = base_info.cant_have_const_ctor;
      n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
    }
  else
    {
      first_vfn_base_index = 0;
      has_virtual = 0;
      max_has_virtual = 0;
      vfield = NULL_TREE;
      vfields = NULL_TREE;
      fields = NULL_TREE;
      y = NULL_TREE;
      needs_default_ctor = 0;
      cant_have_default_ctor = 0;
      needs_const_ctor = 0;
      cant_have_const_ctor = 0;
    }

  if (write_virtuals == 3 && ! CLASSTYPE_INTERFACE_UNKNOWN (t))
    {
      CLASSTYPE_INTERFACE_ONLY (t) = interface_only;
      CLASSTYPE_VTABLE_NEEDS_WRITING (t) = ! interface_only;
    }

  /* The three of these are approximations which may later be
     modified.  Needed at this point to make add_virtual_function
     and modify_vtable_entries work.  */
  CLASSTYPE_ASSOC (t) = make_assoc (integer_zero_node, t,
				    0, 0, CLASSTYPE_ASSOC (t));
  CLASSTYPE_VFIELDS (t) = vfields;
  CLASSTYPE_VFIELD (t) = vfield;

  fn_fields = NULL_TREE;
  tail = NULL_TREE;
  if (y && list_of_fieldlists)
    TREE_CHAIN (y) = TREE_VALUE (list_of_fieldlists);

#ifdef SOS
  if (flag_all_virtual == 2)
    all_virtual = 2;
  else
#endif
    {
      if (flag_all_virtual == 1 && TYPE_OVERLOADS_METHOD_CALL_EXPR (t))
	all_virtual = 1;
      else
	all_virtual = 0;
    }

  if (CLASSTYPE_DECLARED_CLASS (t) == 0)
    {
      nonprivate_method = 1;
      if (list_of_fieldlists
	  && TREE_PURPOSE (list_of_fieldlists) == (tree)visibility_default)
	TREE_PURPOSE (list_of_fieldlists) = (tree)visibility_public;
    }
  else if (list_of_fieldlists
	   && TREE_PURPOSE (list_of_fieldlists) == (tree)visibility_default)
    TREE_PURPOSE (list_of_fieldlists) = (tree)visibility_private;

  while (list_of_fieldlists)
    {
      visibility = (enum visibility_type)TREE_PURPOSE (list_of_fieldlists);

      for (x = TREE_VALUE (list_of_fieldlists); x; x = TREE_CHAIN (x))
	{
	  TREE_PRIVATE (x) = visibility == visibility_private;
	  TREE_PROTECTED (x) = visibility == visibility_protected;
#ifdef FIELD_XREF
	  FIELD_xref_member(current_class_name,x);
#endif

	  if (TREE_CODE (x) == FUNCTION_DECL)
	    {
	      /* Clear out this flag.

	         @@ Doug may figure out how to break
		 @@ this with nested classes and friends.  */
	      DECL_IN_AGGR_P (x) = 0;

	      nonprivate_method |= ! TREE_PRIVATE (x);

	      /* If this was an evil function, don't keep it in class.  */
	      if (IDENTIFIER_ERROR_LOCUS (DECL_NAME (x)))
		continue;

	      if (y) TREE_CHAIN (y) = TREE_CHAIN (x);
	      if (! fn_fields) fn_fields = x;
	      else TREE_CHAIN (tail) = x;
	      tail = x;
	      if (DECL_CONTEXT (x))
		continue;

	      DECL_CONTEXT (x) = t;
	      DECL_VCONTEXT (x) = t;

	      DECL_SIZE_UNIT (x) = 0;

	      /* The name of the field is the original field name
		 Save this in auxiliary field for later overloading.  */
	      if (DECL_VIRTUAL_P (x)
		  || (all_virtual == 1 && ! DECL_CONSTRUCTOR_P (x)))
		{
		  pending_virtuals = add_virtual_function (pending_virtuals,
							   &has_virtual, x,
							   first_vfn_base_index);
		  if (DECL_ABSTRACT_VIRTUAL_P (x))
		    abstract_virtuals = tree_cons (NULL_TREE, x, abstract_virtuals);
		}
	      continue;
	    }

	  /* Handle visibility declarations.  */
	  if (DECL_NAME (x) && TREE_CODE (DECL_NAME (x)) == SCOPE_REF)
	    {
	      tree fdecl = TREE_OPERAND (DECL_NAME (x), 1);

	      if (y) TREE_CHAIN (y) = TREE_CHAIN (x);
	      /* Make type T see field decl FDECL with
		 the visibility VISIBILITY.  */
	      if (TREE_CODE (fdecl) == TREE_LIST)
		{
		  fdecl = TREE_VALUE (fdecl);
		  while (fdecl)
		    {
		      if (alter_visibility (t, fdecl, visibility) == 0)
			break;
		      fdecl = TREE_CHAIN (fdecl);
		    }
		}
	      else alter_visibility (t, fdecl, visibility);
	      CLASSTYPE_ALTERS_VISIBILITIES_P (t) = 1;
	      continue;
	    }

	  /* Perform error checking that did not get done in grokdeclarator.  */
	  if (TREE_CODE (TREE_TYPE (x)) == FUNCTION_TYPE)
	    {
	      error_with_decl (x, "field `%s' invalidly declared function type");
	      TREE_TYPE (x) = build_pointer_type (TREE_TYPE (x));
	    }
	  else if (TREE_CODE (TREE_TYPE (x)) == METHOD_TYPE)
	    {
	      error_with_decl (x, "field `%s' invalidly declared method type");
	      TREE_TYPE (x) = build_pointer_type (TREE_TYPE (x));
	    }
	  else if (TREE_CODE (TREE_TYPE (x)) == OFFSET_TYPE)
	    {
	      error_with_decl (x, "field `%s' invalidly declared offset type");
	      TREE_TYPE (x) = build_pointer_type (TREE_TYPE (x));
	    }
	  /* If this is of reference type, check if it needs an init.  */
	  if (TREE_CODE (TREE_TYPE (x)) == REFERENCE_TYPE
	      && DECL_INITIAL (x) == 0)
	    ref_sans_init = 1;

	  /* When this goes into scope, it will be a non-local reference.  */
	  TREE_NONLOCAL (x) = 1;

	  if (TREE_CODE (x) == FIELD_DECL)
	    {
	      /* Never let anything with uninheritable virutals
		 make it through without complaint.  */
	      if (TYPE_LANG_SPECIFIC (TREE_TYPE (x))
		  && CLASSTYPE_ABSTRACT_VIRTUALS (TREE_TYPE (x)))
		abstract_virtuals_error (x, TREE_TYPE (x));

	      if (TYPE_LANG_SPECIFIC (TREE_TYPE (x)))
		{
		  if (TYPE_HAS_DEFAULT_CONSTRUCTOR (TREE_TYPE (x)))
		    needs_default_ctor = 1;
		  if (TYPE_GETS_CONST_INIT_REF (TREE_TYPE (x)))
		    needs_const_ctor = 1;
		  else if (TYPE_GETS_INIT_REF (TREE_TYPE (x)))
		    cant_have_const_ctor = 1;
		}
	      else if (DECL_INITIAL (x) == NULL_TREE
		       && (TYPE_HAS_CONSTRUCTOR (TREE_TYPE (x))
			   || TREE_CODE (TREE_TYPE (x)) == REFERENCE_TYPE))
		cant_have_default_ctor = 1;

	      /* If any field is const, the structure type is pseudo-const.  */
	      if (TREE_READONLY (x))
		{
		  C_TYPE_FIELDS_READONLY (t) = 1;
		  if (DECL_INITIAL (x) == 0)
		    const_sans_init = 1;
		}
	      else 
		{
		  /* A field that is pseudo-const makes the structure likewise.  */
		  tree t1 = TREE_TYPE (x);
		  while (TREE_CODE (t1) == ARRAY_TYPE)
		    t1 = TREE_TYPE (t1);
		  if (IS_AGGR_TYPE (t1))
		    {
		      if (C_TYPE_FIELDS_READONLY (t1))
			C_TYPE_FIELDS_READONLY (t) = 1;
		      if (CLASSTYPE_READONLY_FIELDS_NEED_INIT (t1))
			const_sans_init = 1;
		    }
		}
	    }
	  else if (TREE_STATIC (x) && TREE_CODE (t) == UNION_TYPE)
	    /* Unions cannot have static members.  */
	    error_with_decl (x, "field `%s' declared static in union");

	  if (! fields) fields = x;
	  DECL_FIELD_CONTEXT (x) = t;
	  DECL_SIZE_UNIT (x) = 0;

	  if (TREE_PACKED (x))
	    {
	      /* Invalid bit-field size done by grokfield.  */
	      /* Detect invalid bit-field type.  */
	      if (DECL_INITIAL (x)
		  && TREE_CODE (TREE_TYPE (x)) != INTEGER_TYPE
		  && TREE_CODE (TREE_TYPE (x)) != ENUMERAL_TYPE)
		{
		  error_with_decl (x, "bit-field `%s' has invalid type");
		  DECL_INITIAL (x) = NULL;
		}
	      if (DECL_INITIAL (x) && pedantic
		  && TREE_TYPE (x) != integer_type_node
		  && TREE_TYPE (x) != unsigned_type_node)
		warning_with_decl (x, "bit-field `%s' type invalid in ANSI C");

	      /* Detect and ignore out of range field width.  */
	      if (DECL_INITIAL (x))
		{
		  register int width = TREE_INT_CST_LOW (DECL_INITIAL (x));

		  if (width < 0)
		    {
		      DECL_INITIAL (x) = NULL;
		      warning_with_decl (x, "negative width in bit-field `%s'");
		    }
		  else if (width == 0 && DECL_NAME (x) != 0)
		    {
		      error_with_decl (x, "zero width for bit-field `%s'");
		      DECL_INITIAL (x) = NULL;
		    }
		  else if (width > TYPE_PRECISION (TREE_TYPE (x)))
		    {
		      DECL_INITIAL (x) = NULL;
		      warning_with_decl (x, "width of `%s' exceeds its type");
		    }
		}

	      /* Process valid field width.  */
	      if (DECL_INITIAL (x))
		{
		  register int width = TREE_INT_CST_LOW (DECL_INITIAL (x));

		  if (width == 0)
		    {
		      /* field size 0 => mark following field as "aligned" */
		      if (TREE_CHAIN (x))
			DECL_ALIGN (TREE_CHAIN (x))
			  = MAX (DECL_ALIGN (TREE_CHAIN (x)), EMPTY_FIELD_BOUNDARY);
		      /* field of size 0 at the end => round up the size.  */
		      else
			round_up_size = EMPTY_FIELD_BOUNDARY;
		    }
		  else
		    {
		      DECL_INITIAL (x) = NULL_TREE;
		      DECL_SIZE_UNIT (x) = width;
		      TREE_PACKED (x) = 1;
		      /* Traditionally a bit field is unsigned
			 even if declared signed.  */
		      if (flag_traditional
			  && TREE_CODE (TREE_TYPE (x)) == INTEGER_TYPE)
			TREE_TYPE (x) = unsigned_type_node;
		    }
		}
	      else
		/* Non-bit-fields are aligned for their type.  */
		DECL_ALIGN (x) = MAX (DECL_ALIGN (x), TYPE_ALIGN (TREE_TYPE (x)));
	    }
	  else if (TREE_CODE (x) == FIELD_DECL)
	    {
	      tree type = TREE_TYPE (x);
	      if (TREE_CODE (type) == ARRAY_TYPE)
		type = TREE_TYPE (type);
	      if (code == UNION_TYPE)
		{
		  if (TYPE_NEEDS_CONSTRUCTING (type))
		    error ("member %s::%s with constructor not allowed in union",
			   IDENTIFIER_POINTER (name), IDENTIFIER_POINTER (DECL_NAME (x)));
		  if (TYPE_NEEDS_DESTRUCTOR (type))
		    error ("member %s::%s with destructor (also) not allowed in union",
			   IDENTIFIER_POINTER (name), IDENTIFIER_POINTER (DECL_NAME (x)));
		}
	      else if (code == RECORD_TYPE)
		{
		  /* Array of record type doesn't matter for this bit.  */
		  TYPE_NEEDS_CONSTRUCTING (t) |= TYPE_NEEDS_CONSTRUCTING (type);
		  if (IS_AGGR_TYPE (type))
		    {
		      needs_ctor |= TYPE_NEEDS_CONSTRUCTOR (type);
		      needs_dtor |= TYPE_NEEDS_DESTRUCTOR (type);
		      members_need_dtors |= TYPE_NEEDS_DESTRUCTOR (type);
		      TYPE_GETS_ASSIGNMENT (t) |= TYPE_GETS_ASSIGNMENT (type);
		      TYPE_GETS_INIT_REF (t) |= TYPE_GETS_INIT_REF (type);
		      TYPE_GETS_CONST_INIT_REF (t) |= TYPE_GETS_CONST_INIT_REF (type);
		    }
		}
	      if (DECL_INITIAL (x) != NULL_TREE)
		{
		  /* `build_class_init_list' does not recognize non-FIELD_DECLs.  */
		  if (code == UNION_TYPE && any_default_members != 0)
		    error ("multiple fields in union initialized");
		  any_default_members = 1;
		}
	    }
	  y = x;
	}
      list_of_fieldlists = TREE_CHAIN (list_of_fieldlists);
      /* link the tail while we have it! */
      if (y)
	{
	  TREE_CHAIN (y) = NULL_TREE;

	  if (list_of_fieldlists
	      && TREE_VALUE (list_of_fieldlists)
	      && TREE_CODE (TREE_VALUE (list_of_fieldlists)) != FUNCTION_DECL)
	    TREE_CHAIN (y) = TREE_VALUE (list_of_fieldlists);
	}
    }

  if (tail) TREE_CHAIN (tail) = NULL_TREE;

  /* If this type has any constant members which did not come
     with their own initialization, mark that fact here.  It is
     not an error here, since such types can be saved either by their
     constructors, or by fortuitous initialization.  */
  CLASSTYPE_READONLY_FIELDS_NEED_INIT (t) = const_sans_init;
  CLASSTYPE_REF_FIELDS_NEED_INIT (t) = ref_sans_init;
  CLASSTYPE_ABSTRACT_VIRTUALS (t) = abstract_virtuals;

  if (vfield == 0
      && (has_virtual
#ifdef SOS
	  || TYPE_DYNAMIC (t)
#endif
	  ))
    {
      /* We build this decl with ptr_type_node, and
	 change the type when we know what it should be.  */
      vfield = build_decl (FIELD_DECL, get_vfield_name (t), ptr_type_node);
      CLASSTYPE_VFIELD (t) = vfield;
      DECL_VIRTUAL_P (vfield) = 1;
      DECL_FIELD_CONTEXT (vfield) = t;
      SET_DECL_FCONTEXT (vfield, t);
      DECL_SIZE_UNIT (vfield) = 0;
      if (y)
	{
	  assert (TREE_CHAIN (y) == 0);
	  TREE_CHAIN (y) = vfield;
	  y = vfield;
	}
      else fields = vfield;
      vfields = chainon (vfields, CLASSTYPE_AS_LIST (t));
    }

  /* Now DECL_INITIAL is null on all members except for zero-width bit-fields.
     And they have already done their work.

     C++: maybe we will support default field initialization some day...  */

  /* Delete all zero-width bit-fields from the front of the fieldlist */
  while (fields && TREE_PACKED (fields)
	 && DECL_INITIAL (fields))
    fields = TREE_CHAIN (fields);
  /* Delete all such fields from the rest of the fields.  */
  for (x = fields; x;)
    {
      if (TREE_CHAIN (x) && TREE_PACKED (TREE_CHAIN (x))
	  && DECL_INITIAL (TREE_CHAIN (x)))
	TREE_CHAIN (x) = TREE_CHAIN (TREE_CHAIN (x));
      else x = TREE_CHAIN (x);
    }
  /* Delete all duplicate fields from the fields */
  delete_duplicate_fields (fields);

  /* Now we have the final fieldlist for the data fields.  Record it,
     then lay out the structure or union (including the fields).  */

  TYPE_FIELDS (t) = fields;

  /* If there's a :0 field at the end, round the size to the
     EMPTY_FIELD_BOUNDARY.  */
  TYPE_ALIGN (t) = round_up_size;

  if (debug_default_functions)
    {
      if ((TYPE_NEEDS_CONSTRUCTOR (t) || TYPE_HAS_CONSTRUCTOR (t) || needs_ctor)
	  && ! TYPE_HAS_INIT_REF (t))
	{
	  tree default_fn = cons_up_default_function (t, name, 1);
	  TREE_CHAIN (default_fn) = fn_fields;
	  DECL_CONTEXT (default_fn) = t;
	  DECL_VCONTEXT (default_fn) = t;
	  fn_fields = default_fn;
	  TYPE_HAS_INIT_REF (t) = 1;
	  default_fn = cons_up_default_function (t, name, 3);
	  TREE_CHAIN (default_fn) = fn_fields;
	  DECL_CONTEXT (default_fn) = t;
	  DECL_VCONTEXT (default_fn) = t;
	  fn_fields = default_fn;
	  nonprivate_method = 1;
	}

      if (! TYPE_HAS_DEFAULT_CONSTRUCTOR (t)
	  && needs_default_ctor && ! cant_have_default_ctor)
	{
	  tree default_fn = cons_up_default_function (t, name, 2);
	  TREE_CHAIN (default_fn) = fn_fields;
	  DECL_CONTEXT (default_fn) = t;
	  DECL_VCONTEXT (default_fn) = t;
	  fn_fields = default_fn;
	  TYPE_HAS_DEFAULT_CONSTRUCTOR (t) = 1;
	  nonprivate_method = 1;
	}
    }
  /* Warn about duplicate methods in fn_fields.  Also compact
     method lists so that lookup can be made faster.

     Algorithm:  Outer loop builds lists by method name.
     Inner loop checks for redundant method names within a list.

     Data Structure:  List of method lists.  The outer list
     is a TREE_LIST, whose TREE_PURPOSE field is the field name
     and the TREE_VALUE is the TREE_CHAIN of the FUNCTION_DECLs.
     Friends are chained in the same way as member functions, but
     they live in the TREE_TYPE field of the outer list.
     That allows them to be quicky deleted, and requires
     no extra storage.

     If there are any constructors/destructors, they are moved to
     the front of the list.  This makes pushclass more efficient.

     We also link each field which has shares a name with its
     baseclass to the head of the list of fields for that base class.
     This allows us to reduce search time in places like `build_method_call'
     to consider only reasonably likely functions.  */

  if (fn_fields)
    {
      /* Now prepare to gather fn_fields into vector.  */
      struct obstack *ambient_obstack = current_obstack;
      current_obstack = &class_obstack;
      method_vec = make_node (TREE_VEC);
      /* Room has been saved for constructors and destructors.  */
      current_obstack = ambient_obstack;
      /* Now make this a live vector.  */
      obstack_free (&class_obstack, method_vec);
      obstack_blank (&class_obstack, sizeof (struct tree_vec));

      while (fn_fields)
	{
	  /* NEXT Pointer, TEST Pointer, and BASE Pointer.  */
	  tree nextp, *testp;

	  nextp = TREE_CHAIN (fn_fields);
	  TREE_CHAIN (fn_fields) = NULL_TREE;
	  /* Constrcutors are handled easily in search routines.
	     Besides, we know we wont find any, so do not bother looking.  */
	  if (DECL_ORIGINAL_NAME (fn_fields) == name
	      && TREE_VEC_ELT (method_vec, 0) == 0)
	    TREE_VEC_ELT (method_vec, 0) = fn_fields;
	  else
	    {
	      testp = &TREE_VEC_ELT (method_vec, 0);
	      if (*testp == NULL_TREE)
		testp++;
	      while ((int)testp < (int)obstack_next_free (&class_obstack)
		     && DECL_ORIGINAL_NAME (*testp) != DECL_ORIGINAL_NAME (fn_fields))
		testp++;
	      if ((int)testp < (int)obstack_next_free (&class_obstack))
		{
		  for (x = *testp; x; x = TREE_CHAIN (x))
		    {
		      if (DECL_NAME (fn_fields) == DECL_NAME (x))
			{
			  /* We complain about multiple destructors on sight,
			     so we do not repeat the warning here.  Friend-friend
			     ambiguities are warned about outside this loop.  */
			  if (! DESTRUCTOR_NAME_P (DECL_NAME (fn_fields)))
			    error_with_file_and_line (DECL_SOURCE_FILE (fn_fields),
						      DECL_SOURCE_LINE (fn_fields),
						      "ambiguous method `%s' in structure",
						      lang_printable_name (fn_fields));
			  break;
			}
		      y = x;
		    }
		  if (x == 0)
		    if (*testp)
		      TREE_CHAIN (y) = fn_fields;
		    else
		      *testp = fn_fields;
		}
	      else
		{
		  obstack_ptr_grow (&class_obstack, fn_fields);
		  method_vec = (tree)obstack_base (&class_obstack);
		}
	    }
	  fn_fields = nextp;
	}

      TREE_VEC_LENGTH (method_vec)
	= (tree *)obstack_next_free (&class_obstack) - (&TREE_VEC_ELT (method_vec, 0));
      obstack_finish (&class_obstack);
      CLASSTYPE_METHOD_VEC (t) = method_vec;

      if (nonprivate_method == 0
	  && CLASSTYPE_FRIEND_CLASSES (t) == NULL_TREE
	  && DECL_FRIENDLIST (TYPE_NAME (t)) == NULL_TREE)
	{
	  for (i = 0; i <= n_baseclasses; i++)
	    if (CLASSTYPE_VIA_PUBLIC (t, i))
	      {
		nonprivate_method = 1;
		break;
	      }
	  if (nonprivate_method == 0)
	    warning ("all class member functions are private");
	}
    }
  else
    {
      method_vec = 0;

      /* Just in case these got accidently
	 filled in by syntax errors.  */
      TYPE_HAS_CONSTRUCTOR (t) = 0;
      TYPE_HAS_DESTRUCTOR (t) = 0;
    }

  /* If there are constructors (and destructors), they are at the
     front.  Place destructors at very front.  Also warn if all
     constructors and/or destructors are private (in which case this
     class is effectively unusable.  */
  if (TYPE_HAS_DESTRUCTOR (t))
    {
      tree dtor, prev;

      for (dtor = TREE_VEC_ELT (method_vec, 0); dtor; prev = dtor, dtor = TREE_CHAIN (dtor))
	{
	  if (DESTRUCTOR_NAME_P (DECL_NAME (dtor)))
	    {
	      if (TREE_PRIVATE (dtor)
		  && CLASSTYPE_FRIEND_CLASSES (t) == NULL_TREE
		  && DECL_FRIENDLIST (TYPE_NAME (t)) == NULL_TREE)
		warning_with_decl (TYPE_NAME (t), "class `%s' only defines a private destructor and has no friends");
	      break;
	    }
	}
      /* Wild parse errors can cause this to happen.  */
      if (dtor == NULL_TREE)
	TYPE_HAS_DESTRUCTOR (t) = 0;
      else if (dtor != TREE_VEC_ELT (method_vec, 0))
	{
	  TREE_CHAIN (prev) = TREE_CHAIN (dtor);
	  TREE_CHAIN (dtor) = TREE_VEC_ELT (method_vec, 0);
	  TREE_VEC_ELT (method_vec, 0) = dtor;
	}
    }
  else if (members_need_dtors
	   || TYPE_USES_VIRTUAL_BASECLASSES (t)
	   || TYPE_USES_MULTIPLE_INHERITANCE (t))
    {
      /* Here we must cons up a destructor on the fly.  */
      tree dtor = cons_up_default_function (t, name, 0);

      /* If we couldn't make it work, then pretend we didn't need it.  */
      if (dtor == void_type_node)
	TYPE_NEEDS_DESTRUCTOR (t) = 0;
      else
	{
	  DECL_CONTEXT (dtor) = t;
	  DECL_VCONTEXT (dtor) = t;
	  if (DECL_VIRTUAL_P (dtor))
	    pending_virtuals = add_virtual_function (pending_virtuals,
						     &has_virtual, dtor);
	  if (TYPE_HAS_CONSTRUCTOR (t))
	    TREE_CHAIN (dtor) = TREE_VEC_ELT (method_vec, 0);
	  else if (method_vec == 0)
	    {
	      /* Now prepare to gather fn_fields into vector.  */
	      struct obstack *ambient_obstack = current_obstack;
	      current_obstack = &class_obstack;
	      method_vec = make_node (TREE_VEC);
	      /* Room has been saved for constructors and destructors.  */
	      current_obstack = ambient_obstack;
	      TREE_VEC_LENGTH (method_vec) = 1;
	      CLASSTYPE_METHOD_VEC (t) = method_vec;
	    }
	  TREE_VEC_ELT (method_vec, 0) = dtor;
	  TYPE_HAS_DESTRUCTOR (t) = 1;
	}
    }
  if (TYPE_HAS_CONSTRUCTOR (t)
      && ! CLASSTYPE_DECLARED_EXCEPTION (t)
      && CLASSTYPE_FRIEND_CLASSES (t) == NULL_TREE
      && DECL_FRIENDLIST (TYPE_NAME (t)) == NULL_TREE)
    {
      int nonprivate_ctor = 0;
      tree ctor;

      for (ctor = TREE_VEC_ELT (method_vec, 0); ctor; ctor = TREE_CHAIN (ctor))
	if (! TREE_PRIVATE (ctor))
	  {
	    nonprivate_ctor = 1;
	    break;
	  }
      if (nonprivate_ctor == 0)
	warning ("class %s only defines private constructors and has no friends",
		 TYPE_NAME_STRING (t));
    }

  /* Now for each member function (except for constructors and
     destructors), compute where member functions of the same
     name reside in base classes.  */
  if (n_baseclasses != 0
      && method_vec != NULL_TREE
      && TREE_VEC_LENGTH (method_vec) > 1)
    {
      int len = TREE_VEC_LENGTH (method_vec);
      tree baselink_vec = make_tree_vec (len);
      int any_links = 0;

      for (i = 1; i < len; i++)
	{
	  TREE_VEC_ELT (baselink_vec, i)
	    = get_baselinks (t, DECL_ORIGINAL_NAME (TREE_VEC_ELT (method_vec, i)));
	  if (TREE_VEC_ELT (baselink_vec, i) != 0)
	    any_links = 1;
	}
      if (any_links != 0)
	CLASSTYPE_BASELINK_VEC (t) = baselink_vec;
      else
	obstack_free (current_obstack, baselink_vec);
    }

  /* We can't know this information until we have seen all of the
     constructors.  */
  TYPE_NONE_ASSIGN_THIS (t) = 0;

  /* Pass layout information about base classes to layout_type, if any.  */

  if (n_baseclasses)
    {
      tree pseudo_basetype = TREE_TYPE (base_layout_decl);

      TREE_CHAIN (base_layout_decl) = TYPE_FIELDS (t);
      TYPE_FIELDS (t) = base_layout_decl;

      TYPE_SIZE (pseudo_basetype) = CLASSTYPE_SIZE (t);
      TYPE_SIZE_UNIT (pseudo_basetype) = TYPE_SIZE_UNIT (t);
      TYPE_MODE (pseudo_basetype) = TYPE_MODE (t);
      TYPE_ALIGN (pseudo_basetype) = CLASSTYPE_ALIGN (t);
      DECL_ALIGN (base_layout_decl) = TYPE_ALIGN (pseudo_basetype);
    }

  layout_type (t);

  if (n_baseclasses)
    TYPE_FIELDS (t) = TREE_CHAIN (TYPE_FIELDS (t));

  /* C++: do not let empty structures exist.  */
  if (integer_zerop (TYPE_SIZE (t)))
    TYPE_SIZE (t) = TYPE_SIZE (char_type_node);

  /* Set the TYPE_DECL for this type to contain the right
     value for DECL_OFFSET, so that we can use it as part
     of a COMPONENT_REF for multiple inheritance.  */

  if (TREE_CODE (TYPE_NAME (t)) == TYPE_DECL)
    layout_decl (TYPE_NAME (t));

  if (TYPE_USES_VIRTUAL_BASECLASSES (t))
    {
      tree vbases;

      max_has_virtual = layout_vbasetypes (t, max_has_virtual);
      vbases = CLASSTYPE_VBASECLASSES (t);
      CLASSTYPE_N_VBASECLASSES (t) = list_length (vbases);

      /* Now fix up any virtual base class types that we
	 left lying around.  We must get these done
	 before we try to lay out the virtual function table.  */
      pending_hard_virtuals = nreverse (pending_hard_virtuals);
#if 1
      /* This loop makes all the entries in the virtual function tables
	 of interest contain the "latest" version of the functions
	 we have defined.  */

      while (vbases)
	{
	  tree virtuals = ASSOC_VIRTUALS (vbases);

	  if (virtuals)
	    virtuals = TREE_CHAIN (virtuals);

	  while (virtuals != NULL_TREE)
	    {
	      tree pfn = FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (virtuals));
	      tree base_fndecl = TREE_OPERAND (pfn, 0);
	      tree decl = get_first_matching_virtual (t, base_fndecl, 0);
	      tree context = DECL_CONTEXT (decl);
	      if (decl != base_fndecl && context != t)
		{
		  tree assoc = NULL_TREE, these_virtuals;
		  int i = TREE_INT_CST_LOW (DECL_VINDEX (base_fndecl)) & ((1<<(BITS_PER_WORD-1))-1);

		  if (TYPE_USES_VIRTUAL_BASECLASSES (context))
		    assoc = virtual_member (DECL_CONTEXT (base_fndecl),
					    CLASSTYPE_VBASECLASSES (context));
		  if (assoc == NULL_TREE)
		    assoc = assoc_value (DECL_CONTEXT (base_fndecl), context);
		  if (assoc != NULL_TREE)
		    {
		      these_virtuals = ASSOC_VIRTUALS (assoc);

		      while (i-- > 0)
			these_virtuals = TREE_CHAIN (these_virtuals);
		      pfn = FNADDR_FROM_VTABLE_ENTRY (TREE_VALUE (these_virtuals));
		      modify_vtable_entries (t, decl, base_fndecl, pfn);
		    }
		}
	      virtuals = TREE_CHAIN (virtuals);
	    }
	  vbases = TREE_CHAIN (vbases);
	}
#endif /* 1 */
      while (pending_hard_virtuals)
	{
	  /* Need an entry in some other virtual function table.  */
	  tree base_fndecls = DECL_VINDEX (TREE_PURPOSE (pending_hard_virtuals));
	  while (base_fndecls)
	    {
	      modify_vtable_entries (t, TREE_PURPOSE (pending_hard_virtuals),
				     TREE_VALUE (base_fndecls),
				     TREE_VALUE (pending_hard_virtuals));
	      base_fndecls = TREE_CHAIN (base_fndecls);
	    }
	  pending_hard_virtuals = TREE_CHAIN (pending_hard_virtuals);
	}
    }
  else
    CLASSTYPE_VBASE_SIZE (t) = integer_zero_node;

  if (pending_virtuals)
    {
      pending_virtuals = nreverse (pending_virtuals);
      /* We must enter these virtuals into the table.  */
      if (first_vfn_base_index == 0)
	{
	  pending_virtuals = tree_cons (NULL_TREE, the_null_vtable_entry,
					pending_virtuals);
	  build_vtable (0, t);
	}
      else
	{
	  /* Here we know enough to change the type of our virtual
	     function table, but we will wait until later this function.  */
	  if (! CLASSTYPE_MARKED4 (t))
	    build_vtable (assoc_value (TYPE_MAIN_VARIANT (CLASSTYPE_BASECLASS (t, first_vfn_base_index)), t), t);
	}

      /* If this type has basetypes with constructors, then those
	 constructors might clobber the virtual function table.  But
	 they don't if the derived class shares the exact vtable of the base
	 class.  */

      CLASSTYPE_NEEDS_VIRTUAL_REINIT (t) = 1;
    }
  else if (first_vfn_base_index)
    {
      tree basetype = get_base_type (DECL_FIELD_CONTEXT (vfield), t, 0);
      tree assoc;
      
      if (TREE_VIA_VIRTUAL (basetype))
	assoc = virtual_member (DECL_FIELD_CONTEXT (vfield), CLASSTYPE_VBASECLASSES (t));
      else
	assoc = assoc_value (TYPE_MAIN_VARIANT (basetype), t);

      /* This class contributes nothing new to the virtual function
	 table.  However, it may have declared functions which
	 went into the virtual function table "inherited" from the
	 base class.  If so, we grab a copy of those updated functions,
	 and pretend they are ours.  */

#ifdef SOS
      /* Don't define this ahead of time if we have more
	 fields to add later.  */
      if (all_virtual == 2 && fn_fields != NULL_TREE)
	;
      else
#endif
	{
	  /* See if we should steal the virtual info from base class.  */
	  if (CLASS_ASSOC_VTABLE (t) == NULL_TREE)
	    CLASS_ASSOC_VTABLE (t) = ASSOC_VTABLE (assoc);
	  if (CLASS_ASSOC_VIRTUALS (t) == NULL_TREE)
	    CLASS_ASSOC_VIRTUALS (t) = ASSOC_VIRTUALS (assoc);
	}
      if (CLASS_ASSOC_VTABLE (t) != ASSOC_VTABLE (assoc))
	CLASSTYPE_NEEDS_VIRTUAL_REINIT (t) = 1;
    }

  if (has_virtual > max_has_virtual)
    max_has_virtual = has_virtual;
  if (max_has_virtual || first_vfn_base_index)
    {
#ifdef VTABLE_USES_MASK
      if (max_has_virtual >= VINDEX_MAX)
	{
	  error ("too many virtual functions for class `%s' (VINDEX_MAX < %d)", TYPE_NAME_STRING (t), has_virtual);
	}
#endif
      TYPE_VIRTUAL_P (t) = 1;
      CLASSTYPE_VSIZE (t) = has_virtual;
      if (first_vfn_base_index)
	{
	  if (pending_virtuals)
	    CLASS_ASSOC_VIRTUALS (t) = chainon (CLASS_ASSOC_VIRTUALS (t),
						pending_virtuals);
	}
      else if (has_virtual)
	{
	  CLASS_ASSOC_VIRTUALS (t) = pending_virtuals;
	  if (write_virtuals >= 0)
	    DECL_VIRTUAL_P (CLASS_ASSOC_VTABLE (t)) = 1;
	}
    }

#ifdef SOS
  if (all_virtual == 2 && (max_has_virtual || method_vec))
    {
      /* Now that we know the size of the virtual table, lay out
	 the absolute table following it.  */
      int i;
      tree tmp;
      tree pending_absolutes = NULL_TREE;
      int has_absolute = has_virtual;

      /* Local variables for building a table filled with strings
	 containing the names of interesting things.  */
      tree decl, init;
      tree start = NULL_TREE, next = NULL_TREE;
      tree *outer = &TREE_VEC_ELT (method_vec, 0);

      while (outer != TREE_VEC_END (method_vec))
	{
	  tree inner;
	  for (inner = *outer; inner; inner = TREE_CHAIN (inner))
	    {
	      tree entry;
	      tree fn;

	      /* Don't bother with functions which appear
		 for visibility reasons.  */
	      if (DECL_FIELD_CONTEXT (inner) != t)
		continue;

	      /* Must lay this function into its absolute table as well.
		 This forces an inline function to be written out.  */
	      fn = build1 (ADDR_EXPR, ptr_type_node, inner);
	      TREE_LITERAL (fn) = 1;
	      DECL_DINDEX (inner) = build_int_2 (++has_absolute, 0);
	      entry = build_vtable_entry (integer_zero_node, fn);
	      pending_absolutes = tree_cons (DECL_DINDEX (inner), entry,
					     pending_absolutes);
	    }
	  outer++;
	}

      CLASS_ASSOC_VIRTUALS (t) = chainon (CLASS_ASSOC_VIRTUALS (t),
					  nreverse (pending_absolutes));
      if (TYPE_DYNAMIC (t))
	{
	  for (outer = &TREE_VEC_ELT (method_vec, 0);
	       outer != TREE_VEC_END (method_vec);
	       outer++)
	    {
	      tree inner;
	      for (inner = *outer; inner; inner = TREE_CHAIN (inner))
		{
		  tree str = make_node (STRING_CST);
		  TREE_STRING_LENGTH (str) = IDENTIFIER_LENGTH (DECL_NAME (inner));
		  TREE_STRING_POINTER (str) = IDENTIFIER_POINTER (DECL_NAME (inner));
		  TREE_LITERAL (str) = 1;
		  TREE_STATIC (str) = 1;
		  TREE_TYPE (str)
		    = build_cplus_array_type (char_type_node,
					      build_index_type (build_int_2 (TREE_STRING_LENGTH (str) - 1, 0)));
	      
		  if (start)
		    {
		      TREE_CHAIN (next) = build_tree_list (NULL_TREE, str);
		      next = TREE_CHAIN (next);
		    }
		  else
		    {
		      start = build_tree_list (NULL_TREE, str);
		      next = start;
		    }
		}
	    }

	  /* Lay out dynamic link table for SOS.  */

	  decl = finish_table (get_linktable_name (t),
			       string_type_node, start, 0);
	}
      has_virtual = has_absolute;
      CLASSTYPE_VSIZE (t) = has_virtual;
      if (has_virtual > max_has_virtual)
	max_has_virtual = has_virtual;
      if (vfield == 0)
	{
	  /* We build this decl with ptr_type_node, and
	     change the type when we know what it should be.  */
	  vfield = build_decl (FIELD_DECL, get_vfield_name (t), ptr_type_node);
	  CLASSTYPE_VFIELD (t) = vfield;
	  DECL_VIRTUAL_P (vfield) = 1;
	  DECL_FIELD_CONTEXT (vfield) = t;
	  SET_DECL_FCONTEXT (vfield, t);
	  DECL_SIZE_UNIT (vfield) = 0;
	  y = tree_last (fields);
	  if (y)
	    TREE_CHAIN (y) = vfield;
	  else
	    fields = vfield;
	  vfields = chainon (vfields, CLASSTYPE_AS_LIST (t));
	}
    }
#endif

  /* Now lay out the virtual function table.  */
  if (has_virtual)
    {
      tree atype, itype;

      if (TREE_TYPE (vfield) == ptr_type_node)
	{
	  /* We must create a pointer to this table because
	     the one inherited from base class does not exist.
	     We will fill in the type when we know what it
	     should really be.  */
	  itype = build_index_type (build_int_2 (has_virtual, 0));
	  atype = build_array_type (vtable_entry_type, itype);
	  layout_type (atype);
	  TREE_TYPE (vfield) = build_pointer_type (atype);
	}
      else
	{
	  atype = TREE_TYPE (TREE_TYPE (vfield));

	  if (has_virtual != TREE_INT_CST_LOW (TYPE_MAX_VALUE (TYPE_DOMAIN (atype))))
	    {
	      /* We must extend (or create) the boundaries on this array,
		 because we picked up virtual functions from multiple
		 base classes.  */
	      itype = build_index_type (build_int_2 (has_virtual, 0));
	      atype = build_array_type (vtable_entry_type, itype);
	      layout_type (atype);
	      vfield = copy_node (vfield);
	      TREE_TYPE (vfield) = build_pointer_type (atype);
#if 0
	      /* In the case of single inheritance, we can
		 just move up the tree, since we share the
		 same vptr slot.  */
	      if (TREE_CHAIN (vfields) == NULL_TREE)
		vfields = CLASSTYPE_AS_LIST (t);
#endif
	    }
	}

      CLASSTYPE_VFIELD (t) = vfield;
      if (TREE_TYPE (CLASS_ASSOC_VTABLE (t)) != atype)
	{
	  TREE_TYPE (CLASS_ASSOC_VTABLE (t)) = atype;
	  layout_decl (CLASS_ASSOC_VTABLE (t));
	  DECL_ALIGN (CLASS_ASSOC_VTABLE (t))
	    = MAX (TYPE_ALIGN (double_type_node),
		   DECL_ALIGN (CLASS_ASSOC_VTABLE (t)));
	}
    }
  else if (first_vfn_base_index)
    CLASSTYPE_VFIELD (t) = vfield;
  CLASSTYPE_VFIELDS (t) = vfields;

  /* Set all appropriate CLASSTYPE_... flags for this type
     and its variants.  */
  TYPE_NEEDS_CONSTRUCTOR (t) |= needs_ctor || TYPE_HAS_CONSTRUCTOR (t);
  TYPE_NEEDS_CONSTRUCTING (t)
    |= ((TYPE_NEEDS_CONSTRUCTOR (t)|TYPE_USES_VIRTUAL_BASECLASSES (t))
	|| (has_virtual | first_vfn_base_index)
	|| any_default_members);
  TYPE_NEEDS_DESTRUCTOR (t) |= needs_dtor || TYPE_HAS_DESTRUCTOR (t);
  finish_struct_bits (t, first_vfn_base_index, max_has_virtual);

  /* Promote each bit-field's type to int if it is narrower than that.
     Also warn (or error) if static members are specified for a class
     which takes a constructor.  */
  for (x = fields; x; x = TREE_CHAIN (x))
    {
      if (TREE_PACKED (x)
	  && TREE_CODE (TREE_TYPE (x)) == INTEGER_TYPE
	  && (TREE_INT_CST_LOW (DECL_SIZE (x)) * DECL_SIZE_UNIT (x)
	      < TYPE_PRECISION (integer_type_node)))
	TREE_TYPE (x) = integer_type_node;
    }

  if (TYPE_HAS_CONSTRUCTOR (t))
    {
      tree vfields = CLASSTYPE_VFIELDS (t);

      while (vfields)
	{
	  /* Mark the fact that constructor for T
	     could affect anybody inheriting from T
	     who wants to initialize vtables for VFIELDS's type.  */
	  if (TREE_TYPE (vfields))
	    TREE_ADDRESSABLE (vfields) = 1;
	  vfields = TREE_CHAIN (vfields);
	}
      if (any_default_members != 0)
	build_class_init_list (t);
    }
  else if (TYPE_NEEDS_CONSTRUCTING (t))
    build_class_init_list (t);

  if (current_lang_name == lang_name_cplusplus)
    {
      if (! CLASSTYPE_DECLARED_EXCEPTION (t))
	embrace_waiting_friends (t);

      /* Write out inline function definitions.  */
      do_inline_function_hair (t, CLASSTYPE_INLINE_FRIENDS (t));
      CLASSTYPE_INLINE_FRIENDS (t) = 0;
    }

  if (CLASSTYPE_VSIZE (t) != 0)
    {
      TYPE_NONCOPIED_PARTS (t) = build_tree_list (default_conversion (CLASS_ASSOC_VTABLE (t)), vfield);

      if ((flag_this_is_variable & 1) == 0)
	{
	  tree vtbl_ptr = build_decl (VAR_DECL, get_identifier (VPTR_NAME),
				      TREE_TYPE (vfield));
	  TREE_REGDECL (vtbl_ptr) = 1;
	  CLASSTYPE_VTBL_PTR (t) = vtbl_ptr;
	}
      if (DECL_FIELD_CONTEXT (vfield) != t)
	{
	  tree assoc = assoc_value (DECL_FIELD_CONTEXT (vfield), t);
	  tree offset = ASSOC_OFFSET (assoc);

	  vfield = copy_node (vfield);

	  if (! integer_zerop (offset))
	    offset = convert_units (offset, BITS_PER_UNIT, 1);
	  if (DECL_OFFSET (vfield))
	    offset = genop (PLUS_EXPR, offset, build_int (DECL_OFFSET (vfield)));
	  DECL_FIELD_CONTEXT (vfield) = t;
	  DECL_OFFSET (vfield) = TREE_INT_CST_LOW (offset);
	  CLASSTYPE_VFIELD (t) = vfield;
	}
    }

  /* Make the rtl for any new vtables we have created, and unmark
     the base types we marked.  */
  unmark_finished_struct (t);

  /* Now out of this class's scope.  However, if this class defined
     any new typedefs, then we must export those to the outer
     binding level.  This is unpleasant.  */
  x = gettags ();

  popclass (0);

#if 0
  /* Remove aggregate types from the list of tags,
     since these appear at global scope.  */
  while (x && IS_AGGR_TYPE (TREE_VALUE (x)))
    x = TREE_CHAIN (x);
  CLASSTYPE_TAGS (t) = x;
  y = x;
  while (x)
    {
      if (IS_AGGR_TYPE (TREE_VALUE (x)))
	TREE_CHAIN (y) = TREE_CHAIN (x);
      x = TREE_CHAIN (x);
    }
#endif

  hack_incomplete_structures (t);

  resume_momentary (old);

  if (flag_cadillac)
    cadillac_finish_struct (t);

  return t;
}

/* Return non-zero if the effective type of INSTANCE is static.
   Used to determine whether the virtual function table is needed
   or not.  */
int
resolves_to_fixed_type_p (instance)
     tree instance;
{
  switch (TREE_CODE (instance))
    {
    case ADDR_EXPR:
      return resolves_to_fixed_type_p (TREE_OPERAND (instance, 0));

    case COMPONENT_REF:
      /* Don't let pointers to members look like they hold a fixed type.  */
      if (TREE_CODE (TREE_OPERAND (instance, 1)) != FIELD_DECL)
	return 0;

    case VAR_DECL:
    case PARM_DECL:
    case NEW_EXPR:
      if (IS_AGGR_TYPE (TREE_TYPE (instance)))
	return 1;

    default:
      return 0;
    }
}

/* Ordering function for overload resolution.  */
int
rank_for_overload (x, y)
     struct candidate *x, *y;
{
  if (y->evil - x->evil)
    return y->evil - x->evil;
  if ((y->harshness[0] & 128) ^ (x->harshness[0] & 128))
    return y->harshness[0] - x->harshness[0];
  if (y->user - x->user)
    return y->user - x->user;
  if (y->b_or_d - x->b_or_d)
    return y->b_or_d - x->b_or_d;
  return y->easy - x->easy;
}

/* TYPE is the type we wish to convert to.  PARM is the parameter
   we have to work with.  We use a somewhat arbitrary cost function
   to measure this conversion.  */
static int
convert_harshness (type, parmtype, parm)
     register tree type, parmtype;
     tree parm;
{
  register enum tree_code codel = TREE_CODE (type);
  register enum tree_code coder = TREE_CODE (parmtype);

#ifdef GATHER_STATISTICS
  n_convert_harshness++;
#endif

  if (TYPE_MAIN_VARIANT (parmtype) == TYPE_MAIN_VARIANT (type))
    return 0;

  if (coder == ERROR_MARK)
    return 1;

  if (codel == POINTER_TYPE
      && (coder == METHOD_TYPE || coder == FUNCTION_TYPE))
    {
      tree p1, p2;
      int harshness, new_harshness;

      /* Get to the METHOD_TYPE or FUNCTION_TYPE that this might be.  */
      type = TREE_TYPE (type);

      if (coder != TREE_CODE (type))
	return 1;

      harshness = 0;

      /* We allow the default conversion between function type
	 and pointer-to-function type for free.  */
      if (type == parmtype)
	return 0;

      /* Compare return types.  */
      harshness |= convert_harshness (TREE_TYPE (type), TREE_TYPE (parmtype), 0);
      if (harshness & 1)
	return 1;
      p1 = TYPE_ARG_TYPES (type);
      p2 = TYPE_ARG_TYPES (parmtype);
      while (p1 && p2)
	{
	  new_harshness = convert_harshness (TREE_VALUE (p1), TREE_VALUE (p2), 0);
	  if (new_harshness & 1)
	    return 1;
	  if ((new_harshness & 7) == 0)
	    harshness += new_harshness;
	  else
	    harshness |= new_harshness;
	  p1 = TREE_CHAIN (p1);
	  p2 = TREE_CHAIN (p2);
	}
      if (p1 == p2)
	return harshness;
      if (p2)
	return 1;
      if (p1)
	return harshness | (TREE_PURPOSE (p1) == NULL_TREE);
    }
  else if (codel == POINTER_TYPE && coder == OFFSET_TYPE)
    {
      int harshness;

      /* Get to the OFFSET_TYPE that this might be.  */
      type = TREE_TYPE (type);

      if (coder != TREE_CODE (type))
	return 1;

      harshness = 0;

      if (TYPE_OFFSET_BASETYPE (type) == TYPE_OFFSET_BASETYPE (parmtype))
	harshness = 0;
      else if (get_base_type (TYPE_OFFSET_BASETYPE (type),
			      TYPE_OFFSET_BASETYPE (parmtype), 0))
	harshness = (1<<3);
      else
	return 1;
      /* Now test the OFFSET_TYPE's target compatability.  */
      type = TREE_TYPE (type);
      parmtype = TREE_TYPE (parmtype);
    }

  if (coder == UNKNOWN_TYPE)
    {
      if (codel == FUNCTION_TYPE
	  || codel == METHOD_TYPE
	  || (codel == POINTER_TYPE
	      && (TREE_CODE (TREE_TYPE (type)) == FUNCTION_TYPE
		  || TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE)))
	return 0;
      return 1;
    }

  if (coder == VOID_TYPE)
    return 1;

  if (codel == ENUMERAL_TYPE || codel == INTEGER_TYPE)
    {
      /* Control equivalence of ints an enums.  */

      if (codel == ENUMERAL_TYPE
	  && flag_int_enum_equivalence == 0)
	{
	  /* Enums can be converted to ints, but not vice-versa.  */
	  if (coder != ENUMERAL_TYPE
	      || TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (parmtype))
	    return 1;
	}

      /* else enums and ints (almost) freely interconvert.  */

      if (coder == INTEGER_TYPE || coder == ENUMERAL_TYPE)
	{
	  int easy = TREE_UNSIGNED (type) ^ TREE_UNSIGNED (parmtype);
	  if (codel != coder)
	    easy += 1;
	  if (TYPE_MODE (type) != TYPE_MODE (parmtype))
	    easy += 2;
	  return (easy << 4);
	}
      else if (coder == REAL_TYPE)
	return (4<<4);
    }

  if (codel == REAL_TYPE)
    if (coder == REAL_TYPE)
      /* Shun converting between float and double if a choice exists.  */
      {
	if (TYPE_MODE (type) != TYPE_MODE (parmtype))
	  return (2<<4);
	return 0;
      }
    else if (coder == INTEGER_TYPE || coder == ENUMERAL_TYPE)
      return (4<<4);

  /* convert arrays which have not previously been converted.  */
  if (codel == ARRAY_TYPE)
    codel = POINTER_TYPE;
  if (coder == ARRAY_TYPE)
    coder = POINTER_TYPE;

  /* Conversions among pointers */
  if (codel == POINTER_TYPE && coder == POINTER_TYPE)
    {
      register tree ttl = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      register tree ttr = TYPE_MAIN_VARIANT (TREE_TYPE (parmtype));
      int penalty = 4 * (ttl != ttr);
      /* Anything converts to void *.  void * converts to anything.
	 Since these may be `const void *' (etc.) use VOID_TYPE
	 instead of void_type_node.
	 Otherwise, the targets must be the same,
	 except that we do allow (at some cost) conversion
	 between signed and unsinged pointer types.  */

      if ((TREE_CODE (ttl) == METHOD_TYPE
	   || TREE_CODE (ttl) == FUNCTION_TYPE)
	  && TREE_CODE (ttl) == TREE_CODE (ttr))
	{
	  if (comptypes (ttl, ttr, -1))
	    return penalty<<4;
	  return 1;
	}

      if (!(TREE_CODE (ttl) == VOID_TYPE
	    || TREE_CODE (ttr) == VOID_TYPE
	    || (TREE_UNSIGNED (ttl) ^ TREE_UNSIGNED (ttr)
		&& (ttl = unsigned_type (ttl),
		    ttr = unsigned_type (ttr),
		    penalty = 10, 0))
	    || (comp_target_types (ttl, ttr, 0))))
	return 1;

      if (ttr == ttl)
	return 4;

      if (IS_AGGR_TYPE (ttl) && IS_AGGR_TYPE (ttr))
	{
	  int b_or_d = get_base_distance (ttl, ttr, 0, 0);
	  if (b_or_d < 0)
	    return 1;
	  return (b_or_d<<3) | 4;
	}

      return (penalty<<4);
    }

  if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      /* This is not a bad match, but don't let it beat
	 integer-enum combinations.  */
      if (parm && integer_zerop (parm))
	return (4<<4);
    }

  /* C++: one of the types must be a reference type.  */
  {
    tree ttl, ttr;
    register tree intype = TYPE_MAIN_VARIANT (parmtype);
    register enum tree_code form = TREE_CODE (intype);
    int penalty;

    if (codel == REFERENCE_TYPE || coder == REFERENCE_TYPE)
      {
	ttl = TYPE_MAIN_VARIANT (type);

	if (codel == REFERENCE_TYPE)
	  {
	    ttl = TYPE_MAIN_VARIANT (TREE_TYPE (ttl));

	    if (form == OFFSET_TYPE)
	      {
		intype = TREE_TYPE (intype);
		form = TREE_CODE (intype);
	      }

	    if (form == REFERENCE_TYPE)
	      {
		intype = TYPE_MAIN_VARIANT (TREE_TYPE (intype));

		if (ttl == intype)
		  return 0;
		penalty = 2;
	      }
	    else
	      {
		/* Can reference be built up?  */
		if (ttl == intype)
		  {
		    return 0;
		  }
		else
		  penalty = 2;
	      }
	  }
	else if (form == REFERENCE_TYPE)
	  {
	    if (parm)
	      {
		tree tmp = convert_from_reference (parm);
		intype = TYPE_MAIN_VARIANT (TREE_TYPE (tmp));
	      }
	    else
	      {
		intype = parmtype;
		do
		  {
		    intype = TREE_TYPE (intype);
		  }
		while (TREE_CODE (intype) == REFERENCE_TYPE);
		intype = TYPE_MAIN_VARIANT (intype);
	      }

	    if (ttl == intype)
	      return 0;
	    else
	      penalty = 2;
	  }

	if (TREE_UNSIGNED (ttl) ^ TREE_UNSIGNED (intype))
	  {
	    ttl = unsigned_type (ttl);
	    intype = unsigned_type (intype);
	    penalty += 2;
	  }

	ttr = intype;

	/* If the initializer is not an lvalue, then it does not
	   matter if we make life easier for the programmer
	   by creating a temporary variable with which to
	   hold the result.  */
	if (parm && (coder == INTEGER_TYPE
		     || coder == ENUMERAL_TYPE
		     || coder == REAL_TYPE)
	    && ! lvalue_p (parm))
	  return (convert_harshness (ttl, ttr, 0) | (penalty << 4));

	if (ttl == ttr)
	  return 4;

	/* Pointers to voids always convert for pointers.  But
	   make them less natural than more specific matches.  */
	if (TREE_CODE (ttl) == POINTER_TYPE && TREE_CODE (ttr) == POINTER_TYPE)
	  if (TREE_TYPE (ttl) == void_type_node
	      || TREE_TYPE (ttr) == void_type_node)
	    return ((penalty+1)<<4);

	if (parm && codel != REFERENCE_TYPE)
	  return (convert_harshness (ttl, ttr, 0) | (penalty << 4));

	/* Here it does matter.  If this conversion is from
	   derived to base, allow it.  Otherwise, types must
	   be compatible in the strong sense.  */
	if (IS_AGGR_TYPE (ttl) && IS_AGGR_TYPE (ttr))
	  {
	    int b_or_d = get_base_distance (ttl, ttr, 0, 0);
	    if (b_or_d < 0)
	      return 1;
#if AMBIGUOUS_WORKING
	    if (ttl == TYPE_MAIN_VARIANT (type)
		&& TYPE_GETS_INIT_REF (type))
	      return (b_or_d<<3) | 6;
#endif
	    return (b_or_d<<3) | 4;
	  }

	if (comp_target_types (ttl, intype, 1))
	  return (penalty<<4);
      }
  }
  if (codel == RECORD_TYPE && coder == RECORD_TYPE)
    {
      int b_or_d = get_base_distance (type, parmtype, 0, 0);
      if (b_or_d < 0)
	return 1;
#if AMBIGUOUS_WORKING
      if (TYPE_GETS_INIT_REF (type))
	return (b_or_d<<3) | 6;
#endif
      return (b_or_d<<3) | 4;
    }
  return 1;
}

/* Algorithm: Start out with no stikes against.  For each argument
   which requires a (subjective) hard conversion (such as between
   floating point and integer), issue a strike.  If there are the same
   number of formal and actual parameters in the list, there will be at
   least on strike, otherwise an exact match would have been found.  If
   there are not the same number of arguments in the type lists, we are
   not dead yet: a `...' means that we can have more parms then were
   declared, and if we wind up in the default argument section of the
   list those can be used as well.  If an exact match could be found for
   one of those cases, return it immediately.  Otherwise, Rank the fields
   so that fields with fewer strikes are tried first.

   Conversions between builtin and user-defined types are allowed, but
   no function involving such a conversion is prefered to one which
   does not require such a conversion.  Furthermore, such conversions
   must be unique.  */

void
compute_conversion_costs (function, tta_in, cp, arglen)
     tree function;
     tree tta_in;
     struct candidate *cp;
     int arglen;
{
  tree ttf_in = TYPE_ARG_TYPES (TREE_TYPE (function));
  tree ttf = ttf_in;
  tree tta = tta_in;

  /* Start out with no strikes against.  */
  int evil_strikes = 0;
  int user_strikes = 0;
  int b_or_d_strikes = 0;
  int easy_strikes = 0;

  int strike_index = 0, win, lose;

#ifdef GATHER_STATISTICS
  n_compute_conversion_costs++;
#endif

  cp->function = function;
  cp->arg = tta ? TREE_VALUE (tta) : NULL_TREE;
  cp->u.bad_arg = 0;		/* optimistic!  */

  bzero (cp->harshness, (arglen+1) * sizeof (short));

  while (ttf && tta)
    {
      int harshness;

      if (ttf == void_list_node)
	break;

      if (type_unknown_p (TREE_VALUE (tta)))
	{	  
	  /* Must perform some instantiation here.  */
	  tree rhs = TREE_VALUE (tta);
	  tree lhstype = TREE_VALUE (ttf);

	  /* @@ This is to undo what `grokdeclarator' does to
	     parameter types.  It really should go through
	     something more general.  */

	  TREE_TYPE (tta) = unknown_type_node;
	  if (TREE_CODE (rhs) == OP_IDENTIFIER)
	    rhs = build_instantiated_decl (lhstype, rhs);
	  else
	    {
	      /* Keep quiet about possible contravariance violations.  */
	      extern int inhibit_warnings;
	      int old_inhibit_warnings = inhibit_warnings;
	      inhibit_warnings = 1;

	      rhs = instantiate_type (lhstype, rhs, 0);

	      inhibit_warnings = old_inhibit_warnings;
	    }

	  if (TREE_CODE (rhs) == ERROR_MARK)
	    harshness = 1;
	  else
	    {
	      harshness = convert_harshness (lhstype, TREE_TYPE (rhs), rhs);
	      /* harshness |= 2; */
	    }
	}
      else
	harshness = convert_harshness (TREE_VALUE (ttf), TREE_TYPE (TREE_VALUE (tta)), TREE_VALUE (tta));

      cp->harshness[strike_index] = harshness;
      if (harshness & 1)
	{
	  cp->u.bad_arg = strike_index;
	  evil_strikes = 1;
	}
      else if (harshness & 2)
	{
	  user_strikes += 1;
	}
      else if (harshness & 4)
	{
	  b_or_d_strikes += (harshness >> 3);
	}
      else
	easy_strikes += harshness >> 4;
      ttf = TREE_CHAIN (ttf);
      tta = TREE_CHAIN (tta);
      strike_index += 1;
    }

  if (tta)
    {
      /* ran out of formals, and parmlist is fixed size.  */
      if (ttf /* == void_type_node */)
	{
	  cp->evil = 1;
	  cp->u.bad_arg = -1;
	  return;
	}
    }
  else if (ttf && ttf != void_list_node)
    {
      /* ran out of actuals, and no defaults.  */
      if (TREE_PURPOSE (ttf) == NULL_TREE)
	{
	  cp->evil = 1;
	  cp->u.bad_arg = -2;
	  return;
	}
      /* Store index of first default.  */
      cp->harshness[arglen] = strike_index+1;
    }
  else cp->harshness[arglen] = 0;

  /* Argument list lengths work out, so don't need to check them again.  */
  if (evil_strikes)
    {
      /* We do not check for derived->base conversions here, since in
	 no case would they give evil strike counts, unless such conversions
	 are somehow ambiguous.  */

      /* See if any user-defined conversions apply.
         But make sure that we do not loop.  */
      static int dont_convert_types = 0;

      if (dont_convert_types)
	{
	  cp->evil = 1;
	  return;
	}

      win = 0;			/* Only get one chance to win.  */
      ttf = TYPE_ARG_TYPES (TREE_TYPE (function));
      tta = tta_in;
      strike_index = 0;
      evil_strikes = 0;

      while (ttf && tta)
	{
	  if (ttf == void_list_node)
	    break;

	  lose = cp->harshness[strike_index];
	  if (lose&1)
	    {
	      tree actual_type = TREE_TYPE (TREE_VALUE (tta));
	      tree formal_type = TREE_VALUE (ttf);

	      dont_convert_types = 1;

	      if (TREE_CODE (formal_type) == REFERENCE_TYPE)
		formal_type = TREE_TYPE (formal_type);
	      if (TREE_CODE (actual_type) == REFERENCE_TYPE)
		actual_type = TREE_TYPE (actual_type);

	      if (formal_type != error_mark_node
		  && actual_type != error_mark_node)
		{
		  formal_type = TYPE_MAIN_VARIANT (formal_type);
		  actual_type = TYPE_MAIN_VARIANT (actual_type);

		  if (TYPE_HAS_CONSTRUCTOR (formal_type))
		    {
		      /* If it has a constructor for this type, try to use it.  */
		      if (convert_to_aggr (formal_type, TREE_VALUE (tta), 0, 1)
			  != error_mark_node)
			{
			  /* @@ There is no way to save this result yet.
			     @@ So success is NULL_TREE for now.  */
			  win++;
			}
		    }
		  if (TYPE_LANG_SPECIFIC (actual_type) && TYPE_HAS_CONVERSION (actual_type))
		    {
		      if (TREE_CODE (formal_type) == INTEGER_TYPE
			  && TYPE_HAS_INT_CONVERSION (actual_type))
			win++;
		      else if (TREE_CODE (formal_type) == REAL_TYPE
			       && TYPE_HAS_REAL_CONVERSION (actual_type))
			win++;
		      else
			{
			  tree conv = build_type_conversion (CALL_EXPR, TREE_VALUE (ttf), TREE_VALUE (tta), 0);
			  if (conv)
			    {
			      if (conv == error_mark_node)
				win += 2;
			      else
				win++;
			    }
			  else if (TREE_CODE (TREE_VALUE (ttf)) == REFERENCE_TYPE)
			    {
			      conv = build_type_conversion (CALL_EXPR, formal_type, TREE_VALUE (tta), 0);
			      if (conv)
				{
				  if (conv == error_mark_node)
				    win += 2;
				  else
				    win++;
				}
			    }
			}
		    }
		}
	      dont_convert_types = 0;

	      if (win == 1)
		{
		  user_strikes += 1;
		  cp->harshness[strike_index] = 2;
		  win = 0;
		}
	      else
		{
		  if (cp->u.bad_arg > strike_index)
		    cp->u.bad_arg = strike_index;

		  evil_strikes = win ? 2 : 1;
		  break;
		}
	    }

	  ttf = TREE_CHAIN (ttf);
	  tta = TREE_CHAIN (tta);
	  strike_index += 1;
	}
    }

  /* Calling a non-const member function from a const member function
     is probably invalid, but for now we let it only draw a warning.
     We indicate that such a mismatch has occured by setting the
     harshness to a maximum value.  */
  if (TREE_CODE (TREE_TYPE (function)) == METHOD_TYPE
      && TREE_CODE (TREE_TYPE (TREE_VALUE (tta_in))) == POINTER_TYPE
      && (TREE_READONLY (TREE_TYPE (TREE_TYPE (TREE_VALUE (tta_in))))
	  > TREE_READONLY (TREE_TYPE (TREE_VALUE (ttf_in)))))
    cp->harshness[0] |= 128;

  cp->evil = evil_strikes;
  cp->user = user_strikes;
  cp->b_or_d = b_or_d_strikes;
  cp->easy = easy_strikes;
}

struct candidate *
ideal_candidate (basetype, candidates, n_candidates, parms, len)
     tree basetype;
     struct candidate *candidates;
     int n_candidates;
     tree parms;
     int len;
{
  struct candidate *cp = candidates + n_candidates;
  int index, i;
  tree ttf;

  qsort (candidates,		/* char *base */
	 n_candidates,		/* int nel */
	 sizeof (struct candidate), /* int width */
	 rank_for_overload);	/* int (*compar)() */

  /* If the best candidate requires user-defined conversions,
     and its user-defined conversions are a strict subset
     of all other candidates requiring user-defined conversions,
     then it is, in fact, the best.  */
  for (i = -1; cp + i != candidates; i--)
    if (cp[i].user == 0)
      break;

  if (i < -1)
    {
      tree ttf0;

      /* Check that every other candidate requires those conversions
	 as a strict subset of their conversions.  */
      if (cp[i].user == cp[-1].user)
	goto non_subset;

      /* Look at subset relationship more closely.  */
      while (i != -1)
	{
	  for (ttf = TYPE_ARG_TYPES (TREE_TYPE (cp[i].function)),
	       ttf0 = TYPE_ARG_TYPES (TREE_TYPE (cp[-1].function)),
	       index = 0;
	       index < len;
	       ttf = TREE_CHAIN (ttf), ttf0 = TREE_CHAIN (ttf0), index++)
	    if (cp[i].harshness[index] & 2)
	      {
		/* If our "best" candidate also needs a conversion,
		   it must be the same one.  */
		if ((cp[-1].harshness[index] & 2)
		    && TREE_VALUE (ttf) != TREE_VALUE (ttf0))
		  goto non_subset;
	      }
	  i++;
	}
      /* The best was the best.  */
      return cp - 1;
    non_subset:
      /* Use other rules for determining "bestness".  */
      ;
    }

  /* If the best two candidates we find require user-defined
     conversions, we may need to report and error message.  */
  if (cp[-1].user && cp[-2].user
      && (cp[-1].b_or_d || cp[-2].b_or_d == 0))
    {
      /* If the best two methods found involved user-defined
	 type conversions, then we must see whether one
	 of them is exactly what we wanted.  If not, then
	 we have an ambiguity.  */
      int best = 0;
      tree tta = parms;
      tree f1, p1;

#if AMBIGUOUS_WORKING
      if (cp[-1].b_or_d == 0
	  && cp[-1].easy == 0
	  && (cp[-2].b_or_d | cp[-2].easy) > 0)
	return cp - 1;
#endif

      /* Stash all of our parameters in safe places
	 so that we can perform type conversions in place.  */
      while (tta)
	{
	  TREE_PURPOSE (tta) = TREE_VALUE (tta);
	  tta = TREE_CHAIN (tta);
	}

      i = 0;
      do
	{
	  int exact_conversions = 0;

	  i -= 1;
	  tta = parms;
	  if (DECL_STATIC_FUNCTION_P (cp[i].function))
	    tta = TREE_CHAIN (tta);
	  for (ttf = TYPE_ARG_TYPES (TREE_TYPE (cp[i].function)), index = 0;
	       index < len;
	       tta = TREE_CHAIN (tta), ttf = TREE_CHAIN (ttf), index++)
	    {
	      if (cp[i].harshness[index] & 2)
		{
		  TREE_VALUE (tta)
		    = build_type_conversion (CALL_EXPR, TREE_VALUE (ttf), TREE_PURPOSE (tta), 2);
		  if (TREE_VALUE (tta))
		    {
		      if (TREE_CODE (TREE_VALUE (tta)) != CONVERT_EXPR
			  && (TREE_CODE (TREE_VALUE (tta)) != NOP_EXPR
			      || comp_target_types (TREE_TYPE (TREE_VALUE (tta)),
						    TREE_TYPE (TREE_OPERAND (TREE_VALUE (tta), 0)), 1)))
			exact_conversions += 1;
		    }
		  else if (IS_AGGR_TYPE (TREE_VALUE (ttf))
			   || (TREE_CODE (TREE_VALUE (ttf)) == REFERENCE_TYPE
			       && IS_AGGR_TYPE (TREE_TYPE (TREE_VALUE (ttf)))))
		    {
		      /* To get here we had to have succeeded via
			 a constructor.  */
		      TREE_VALUE (tta) = TREE_PURPOSE (tta);
		      exact_conversions += 1;
		    }
		}
	    }
	  if (exact_conversions == cp[i].user)
	    {
	      if (best == 0)
		{
		  best = i;
		  f1 = cp[best].function;
		  p1 = TYPE_ARG_TYPES (TREE_TYPE (f1));
		}
	      else
		{
		  /* Don't complain if next best is from base class.  */
		  tree f2 = cp[i].function;
		  tree p2 = TYPE_ARG_TYPES (TREE_TYPE (f2));

		  if (TREE_CODE (TREE_TYPE (f1)) == METHOD_TYPE
		      && TREE_CODE (TREE_TYPE (f2)) == METHOD_TYPE
		      && (cp[i].harshness[0] & 4) != 0
		      && cp[best].harshness[0] < cp[i].harshness[0])
		    {
#if 0
		      /* For LUCID.  */
		      if (! compparms (TREE_CHAIN (p1), TREE_CHAIN (p2), 1))
			goto ret0;
		      else
#endif
			continue;
		    }
		  else goto ret0;
		}
	    }
	} while (cp + i != candidates);

      if (best)
	{
	  int exact_conversions = cp[best].user;
	  tta = parms;
	  if (DECL_STATIC_FUNCTION_P (cp[best].function))
	    tta = TREE_CHAIN (parms);
	  for (ttf = TYPE_ARG_TYPES (TREE_TYPE (cp[best].function)), index = 0;
	       exact_conversions > 0;
	       tta = TREE_CHAIN (tta), ttf = TREE_CHAIN (ttf), index++)
	    {
	      if (cp[best].harshness[index] & 2)
		{
		  /* We must now fill in the slot we left behind.
		     @@ This could be optimized to use the value previously
		     @@ computed by build_type_conversion in some cases.  */
		  TREE_VALUE (tta) = convert (TREE_VALUE (ttf), TREE_PURPOSE (tta));
		  exact_conversions -= 1;
		}
	      else TREE_VALUE (tta) = TREE_PURPOSE (tta);
	    }
	  return cp + best;
	}
      goto ret0;
    }
  /* If the best two candidates we find both use default parameters,
     we may need to report and error.  Don't need to worry if next-best
     candidate is forced to use user-defined conversion when best is not.  */
  if (cp[-2].user == 0
      && cp[-1].harshness[len] != 0 && cp[-2].harshness[len] != 0)
    {
      tree tt1 = TYPE_ARG_TYPES (TREE_TYPE (cp[-1].function));
      tree tt2 = TYPE_ARG_TYPES (TREE_TYPE (cp[-2].function));
      int i = cp[-1].harshness[len];
      if (cp[-2].harshness[len] < i)
	i = cp[-2].harshness[len];
      while (--i > 0)
	{
	  if (TYPE_MAIN_VARIANT (TREE_VALUE (tt1))
	      != TYPE_MAIN_VARIANT (TREE_VALUE (tt2)))
	    /* These lists are not identical, so we can choose our best candidate.  */
	    return cp - 1;
	  tt1 = TREE_CHAIN (tt1);
	  tt2 = TREE_CHAIN (tt2);
	}
      /* To get here, both lists had the same parameters up to the defaults
	 which were used.  This is an ambiguous request.  */
      goto ret0;
    }

  /* Otherwise, return our best candidate.  Note that if we get candidates
     from independent base classes, we have an ambiguity, even if one
     argument list look a little better than another one.  */
  if (cp[-1].b_or_d && basetype && TYPE_USES_MULTIPLE_INHERITANCE (basetype))
    {
      int i = n_candidates - 1, best;
      tree base1 = NULL_TREE;

      if (TREE_CODE (TREE_TYPE (candidates[i].function)) == FUNCTION_TYPE)
	return cp - 1;

      for (; i >= 0 && candidates[i].user == 0 && candidates[i].evil == 0; i--)
	{
	  if (TREE_CODE (TREE_TYPE (candidates[i].function)) == METHOD_TYPE)
	    {
	      tree newbase = TYPE_METHOD_BASETYPE (TREE_TYPE (candidates[i].function));

	      if (base1 != NULL_TREE)
		{
		  if (newbase != base1
		      && ! get_base_type (newbase, base1, 0))
		    {
		      char *buf = (char *)alloca (8192);
		      error ("ambiguous request for function from distinct base classes of type `%s'", TYPE_NAME_STRING (basetype));
		      error ("first candidate is `%s'", fndecl_as_string (buf, 0, candidates[best].function, 1));
		      error ("second candidates is `%s'", fndecl_as_string (buf, 0, candidates[i].function, 1));
		      return cp - 1;
		    }
		}
	      else
		{
		  best = i;
		  base1 = newbase;
		}
	    }
	  else return cp - 1;
	}
    }

#if AMBIGUOUS_WORKING
  if (cp[-1].user == cp[-2].user
      && cp[-1].b_or_d == cp[-2].b_or_d
      && cp[-1].easy == cp[-2].easy)
    goto ret0;
#endif

  return cp - 1;

 ret0:
  /* In the case where there is no ideal candidate, restore
     TREE_VALUE slots of PARMS from TREE_PURPOSE slots.  */
  while (parms)
    {
      TREE_VALUE (parms) = TREE_PURPOSE (parms);
      parms = TREE_CHAIN (parms);
    }
  return 0;
}

/* Assume that if the class referred to is not in the
   current class hierarchy, that it may be remote.
   PARENT is assumed to be of aggregate type here.  */
static int
may_be_remote (parent)
     tree parent;
{
  if (TYPE_OVERLOADS_METHOD_CALL_EXPR (parent) == 0)
    return 0;

  if (current_class_type == NULL_TREE)
    return 0;
  if (parent == current_class_type)
    return 0;

  if (get_base_type (parent, current_class_type, 0))
    return 0;
  return 1;
}

/* Return the number of bytes that the arglist in PARMS would
   occupy on the stack.  */
int
get_arglist_len_in_bytes (parms)
     tree parms;
{
  register tree parm;
  register int bytecount = 0;

  for (parm = parms; parm; parm = TREE_CHAIN (parm))
    {
      register tree pval = TREE_VALUE (parm);
      register int used, size;

      if (TREE_CODE (pval) == ERROR_MARK)
	continue;
      else if (TYPE_MODE (TREE_TYPE (pval)) != BLKmode)
	{
	  used = size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (pval)));
#ifdef PUSH_ROUNDING
	  size = PUSH_ROUNDING (size);
#endif
	  used = (((size + PARM_BOUNDARY / BITS_PER_UNIT - 1)
		   / (PARM_BOUNDARY / BITS_PER_UNIT))
		  * (PARM_BOUNDARY / BITS_PER_UNIT));
	}
      else
	{
	  register tree size = size_in_bytes (TREE_TYPE (pval));
	  register tree used_t = convert_units (convert_units (size, BITS_PER_UNIT, PARM_BOUNDARY),
						PARM_BOUNDARY, BITS_PER_UNIT);
	  used = TREE_INT_CST_LOW (used_t);
	}
      bytecount += used;
    }
  return bytecount;
}

tree
build_vfield_ref (datum, type)
     tree datum, type;
{
  if (TREE_CODE (TREE_TYPE (datum)) == REFERENCE_TYPE)
    datum = convert_from_reference (datum);

  if (! TYPE_USES_VIRTUAL_BASECLASSES (type))
    return build (COMPONENT_REF, TREE_TYPE (CLASSTYPE_VFIELD (type)),
		  datum, CLASSTYPE_VFIELD (type));
  return build_component_ref (datum, DECL_NAME (CLASSTYPE_VFIELD (type)), 0, 0);
}

/* Build a call to a member of an object.  I.e., one that overloads
   operator ()(), or is a pointer-to-function or pointer-to-method.  */
static tree
build_field_call (basetype_path, instance_ptr, name, parms, err_name)
     tree basetype_path;
     tree instance_ptr, name, parms;
     char *err_name;
{
  tree field, instance;

  if (instance_ptr == current_class_decl)
    {
      /* Check to see if we really have a reference to an instance variable
	 with `operator()()' overloaded.  */
#if 1
      field = IDENTIFIER_CLASS_VALUE (name);
#else
      field = identifier_class_value (name);
#endif

      if (field == NULL_TREE)
	{
	  error ("`this' has no member named `%s'", err_name);
	  return error_mark_node;
	}

      if (TREE_CODE (field) == FIELD_DECL)
	{
	  /* If it's a field, try overloading operator (),
	     or calling if the field is a pointer-to-function.  */
	  instance = build_component_ref_1 (C_C_D, field, 0, 1);
	  if (instance == error_mark_node)
	    return error_mark_node;

	  if (TYPE_LANG_SPECIFIC (TREE_TYPE (instance))
	      && TYPE_OVERLOADS_CALL_EXPR (TREE_TYPE (instance)))
	    return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, instance, parms);

	  if (TREE_CODE (TREE_TYPE (instance)) == POINTER_TYPE)
	    if (TREE_CODE (TREE_TYPE (TREE_TYPE (instance))) == FUNCTION_TYPE)
	      return build_function_call (instance, parms);
	    else if (TREE_CODE (TREE_TYPE (TREE_TYPE (instance))) == METHOD_TYPE)
	      return build_function_call (instance, tree_cons (NULL_TREE, current_class_decl, parms));
	}
      return NULL_TREE;
    }

  /* Check to see if this is not really a reference to an instance variable
     with `operator()()' overloaded.  */
  field = lookup_field (basetype_path, name, 1);

  /* This can happen if the reference was ambiguous
     or for visibility violations.  */
  if (field == error_mark_node)
    return error_mark_node;
  if (field)
    {
      tree basetype;
      tree ftype = TREE_TYPE (field);

      if (TYPE_LANG_SPECIFIC (ftype) && TYPE_OVERLOADS_CALL_EXPR (ftype))
	{
	  /* Make the next search for this field very short.  */
	  basetype = DECL_FIELD_CONTEXT (field);
	  instance_ptr = convert_pointer_to (basetype, instance_ptr);

	  instance = build_indirect_ref (instance_ptr, 0);
	  return build_opfncall (CALL_EXPR, LOOKUP_NORMAL,
				 build_component_ref_1 (instance, field, 0, 0),
				 parms);
	}
      if (TREE_CODE (ftype) == POINTER_TYPE)
	{
	  if (TREE_CODE (TREE_TYPE (ftype)) == FUNCTION_TYPE
	      || TREE_CODE (TREE_TYPE (ftype)) == METHOD_TYPE)
	    {
	      /* This is a member which is a pointer to function.  */
	      tree ref = build_component_ref_1 (build_indirect_ref (instance_ptr, 0, 0),
						field, LOOKUP_COMPLAIN);
	      if (ref == error_mark_node)
		return error_mark_node;
	      return build_function_call (ref, parms);
	    }
	}
      else if (TREE_CODE (ftype) == METHOD_TYPE)
	{
	  error ("invalid call via pointer-to-member function");
	  return error_mark_node;
	}
      else
	return NULL_TREE;
    }
  return NULL_TREE;
}

/* Build a method call of the form `EXP->SCOPES::NAME (PARMS)'.
   This is how virtual function calls are avoided.  */
tree
build_scoped_method_call (exp, scopes, name, parms)
     tree exp;
     tree scopes;
     tree name;
     tree parms;
{
  /* Because this syntactic form does not allow
     a pointer to a base class to be `stolen',
     we need not protect the drived->base conversion
     that happens here.
     
     @@ But we do have to check visibility privileges later.  */
  tree basename = (TREE_CODE (scopes) == SCOPE_REF) ? TREE_OPERAND (scopes, 1) : scopes;
  tree basetype, decl;
  tree type = TREE_TYPE (exp);

  if (type == error_mark_node
      || ! is_aggr_typedef (basename, 1))
    return error_mark_node;

  if (! IS_AGGR_TYPE (type))
    {
      error ("base object of scoped method call is not of aggregate type");
      return error_mark_node;
    }

  basetype = TREE_TYPE (TREE_TYPE (basename));

  if (basetype = basetype_or_else (basetype, type))
    {
      if (basetype == error_mark_node)
	return error_mark_node;
      if (TREE_CODE (exp) == INDIRECT_REF)
	decl = build_indirect_ref (convert_pointer_to (basetype,
						       build_unary_op (ADDR_EXPR, exp, 0)), 0);
      else
	decl = build_scoped_ref (exp, scopes);

      /* Call to a destructor.  */
      if (TREE_CODE (name) == BIT_NOT_EXPR)
	{
	  /* Explicit call to destructor.  */
	  name = TREE_OPERAND (name, 0);
	  if (! is_aggr_typedef (name, 1))
	    return error_mark_node;
	  if (TREE_TYPE (decl) != TREE_TYPE (TREE_TYPE (name)))
	    {
	      error_with_aggr_type (TREE_TYPE (decl),
				    "qualified type `%s' does not match destructor type `%s'",
				    IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	  if (! TYPE_HAS_DESTRUCTOR (TREE_TYPE (decl)))
	    error_with_aggr_type (TREE_TYPE (decl), "type `%s' has no destructor");
	  return build_delete (TREE_TYPE (decl), decl, integer_two_node,
			       LOOKUP_NORMAL|LOOKUP_NONVIRTUAL|LOOKUP_DESTRUCTOR, 0);
	}

      /* Call to a method.  */
      return build_method_call (decl, name, parms, NULL_TREE,
				LOOKUP_NORMAL|LOOKUP_NONVIRTUAL);
    }
  return error_mark_node;
}

/* Build something of the form ptr->method (args)
   or object.method (args).  This can also build
   calls to constructors, and find friends.

   Member functions always take their class variable
   as a pointer.

   INSTANCE is a class instance.

   NAME is the NAME field of the struct, union, or class
   whose type is that of INSTANCE.

   PARMS help to figure out what that NAME really refers to.

   BASETYPE_PATH, if non-NULL, tells which basetypes of INSTANCE
   we should be traversed before starting our search.  We need
   this information to get protected accesses correct.

   FLAGS is the logical disjunction of zero or more LOOKUP_
   flags.  See cplus-tree.h for more info.

   If this is all OK, calls build_function_call with the resolved
   member function.

   This function must also handle being called to perform
   initialization, promotion/coercion of arguments, and
   instantiation of default parameters.

   Note that NAME may refer to an instance variable name.  If
   `operator()()' is defined for the type of that field, then we return
   that result.  */
tree
build_method_call (instance, name, parms, basetype_path, flags)
     tree instance, name, parms, basetype_path;
     int flags;
{
  register tree function, fntype, value_type;
  register tree basetype, save_basetype;
  register tree baselink, result, method_name, parmtypes, parm;
  tree last;
  int pass;
  enum visibility_type visibility;
  int rank_for_overload ();

  /* Range of cases for vtable optimization.  */
  enum vtable_needs
    {
      not_needed, maybe_needed, unneeded, needed,
    };
  enum vtable_needs need_vtbl = not_needed;

  char *err_name;
  char *name_kind;
  int ever_seen = 0;
  int wrap;
  tree wrap_type;
  tree instance_ptr = NULL_TREE;
  int all_virtual = flag_all_virtual;
  int static_call_context;
  tree saw_private = 0;
  tree saw_protected = 0;
#ifdef SOS
  /* If call is a call to a constructor, then `dtbl'
     will first be initialized with the function table pointer
     of the appropriate type (calling "sosFindCode" as a last
     resort), the the call to the constructor will go through there.  */
  tree dtbl = (flags & LOOKUP_DYNAMIC) ? TREE_VALUE (parms) : NULL_TREE;

  /* Flag saying whether or not `dtbl' has been inserted into the
     parameter list.  This is needed because we cannot tell (until
     we have a match) whether this parameter should go in or not.

     If 1, then `dtbl' is living naturally.
     If 0, then `dtbl' is not among the parms that we know about.
     If -1, the `dtbl' was place into the parms unnaturally.

     Note that we may side-effect the parameter list, but in such a way
     that the caller of this function would never know.  */
  int dtbl_inserted = (flags & LOOKUP_DYNAMIC);
#endif

  /* Keep track of `const' and `volatile' objects.  */
  int constp, volatilep;

  /* Know if this is explicit destructor call.  */
  int dtor_specd = 0;

#ifdef GATHER_STATISTICS
  n_build_method_call++;
#endif

  if (instance == error_mark_node
      || name == error_mark_node
      || parms == error_mark_node
      || (instance != 0 && TREE_TYPE (instance) == error_mark_node))
    return error_mark_node;

#if 0
  /* C++ 2.1 does not allow this, but ANSI probably will.  */
  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      error ("invalid call to destructor, use qualified name `%s::~%s'",
	     IDENTIFIER_POINTER (name), IDENTIFIER_POINTER (name));
      return error_mark_node;
    }
#else
  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      flags |= LOOKUP_DESTRUCTOR;
      name = TREE_OPERAND (name, 0);
      if (! is_aggr_typedef (name, 1))
	return error_mark_node;
      if (parms)
	error ("destructors take no parameters");
      basetype = TREE_TYPE (TREE_TYPE (name));
      if (! TYPE_HAS_DESTRUCTOR (basetype))
	error_with_aggr_type (basetype, "type `%s' has no destructor");
      instance = default_conversion (instance);
      if (TREE_CODE (TREE_TYPE (instance)) == POINTER_TYPE)
	instance_ptr = instance;
      else
	instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
      return build_delete (basetype, instance_ptr, integer_two_node,
			   LOOKUP_NORMAL|LOOKUP_DESTRUCTOR, 0);
    }
#endif

  if (TREE_CODE (name) == WRAPPER_EXPR)
    {
      wrap_type = TREE_OPERAND (name, 0);
      name = TREE_OPERAND (name, 1);
      wrap = 1;
    }
  else if (TREE_CODE (name) == ANTI_WRAPPER_EXPR)
    {
      wrap_type = TREE_OPERAND (name, 0);
      name = TREE_OPERAND (name, 1);
      wrap = -1;
    }
  else
    {
      wrap_type = NULL_TREE;
      wrap = 0;
    }

  /* Initialize name for error reporting.  */
  if (TREE_CODE (name) == OP_IDENTIFIER)
    name = build_operator_fnname (&name, parms, 1);

  if (OPERATOR_NAME_P (name))
    {
      char *p = operator_name_string (name);
      err_name = (char *)alloca (strlen (p) + 10);
      sprintf (err_name, "operator %s", p);
    }
  else if (name == wrapper_name)
    err_name = "wrapper";
  else if (OPERATOR_TYPENAME_P (name))
    err_name = "type conversion operator";
  else if (TREE_CODE (name) == SCOPE_REF)
    err_name = IDENTIFIER_POINTER (TREE_OPERAND (name, 1));
  else
    err_name = IDENTIFIER_POINTER (name);

#ifdef FIELD_XREF
  FIELD_xref_call(current_function_decl,err_name);
#endif

  if (wrap)
    {
      char *p = (char *)alloca (strlen (err_name) + 32);
      sprintf (p, "%s for `%s'", wrap < 0 ? "anti-wrapper" : "wrapper", err_name);
      err_name = p;
    }

  if (instance == NULL_TREE)
    {
      static_call_context = 0;

      basetype = NULL_TREE;
      /* Check cases where this is really a call to raise
	 an exception.  */
      if (current_class_type && TREE_CODE (name) == IDENTIFIER_NODE)
	{
	  basetype = purpose_member (name, CLASSTYPE_TAGS (current_class_type));
	  if (basetype)
	    basetype = TREE_VALUE (basetype);
	}
      else if (TREE_CODE (name) == SCOPE_REF
	       && TREE_CODE (TREE_OPERAND (name, 0)) == IDENTIFIER_NODE)
	{
	  if (! is_aggr_typedef (TREE_OPERAND (name, 0), 1))
	    return error_mark_node;
	  basetype = purpose_member (TREE_OPERAND (name, 1),
				     CLASSTYPE_TAGS (TREE_TYPE (TREE_TYPE (TREE_OPERAND (name, 0)))));
	  if (basetype)
	    basetype = TREE_VALUE (basetype);
	}

      if (basetype != NULL_TREE)
	;
      /* call to a constructor... */
      else if (TREE_TYPE (name))
	basetype = TREE_TYPE (TREE_TYPE (name));
      else
	{
	  tree typedef_name = lookup_name (name);
	  if (typedef_name && TREE_CODE (typedef_name) == TYPE_DECL)
	    {
	      /* Cannonicalize the typedef name.  */
	      basetype = TREE_TYPE (typedef_name);
	      name = DECL_NAME (TYPE_NAME (basetype));
	    }
	  else
	    {
	      error ("no constructor named `%s' in visible scope",
		     IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	}
      if (wrap_type && wrap_type != basetype)
	{
	  error_with_aggr_type (wrap_type, "invalid constructor `%s::%s'",
				TYPE_NAME_STRING (basetype));
	  return error_mark_node;
	}
      if (TYPE_VIRTUAL_P (basetype))
	{
	  wrap_type = basetype;
	}

      if (! IS_AGGR_TYPE (basetype))
	{
	non_aggr_error:
	  if ((flags & LOOKUP_COMPLAIN) && TREE_CODE (basetype) != ERROR_MARK)
	    error ("request for member `%s' in something not a structure or union", err_name);

	  return error_mark_node;
	}
    }
  else if (instance == C_C_D || instance == current_class_decl)
    {
      extern tree ctor_label, dtor_label;

      /* When doing initialization, we side-effect the TREE_TYPE of
	 C_C_D, hence we cannot set up BASETYPE from CURRENT_CLASS_TYPE.  */
      basetype = TREE_TYPE (C_C_D);

      /* Anything manifestly `this' in constructors and destructors
	 has a known type, so virtual function tables are not needed.  */
      if (TYPE_VIRTUAL_P (basetype)
	  && !(flags & LOOKUP_NONVIRTUAL)
	  && wrap_type == NULL_TREE)
	need_vtbl = (dtor_label || ctor_label)
	  ? unneeded : maybe_needed;

      static_call_context = 0;
      instance = C_C_D;
      instance_ptr = current_class_decl;
      result = build_field_call (CLASSTYPE_AS_LIST (current_class_type),
				 instance_ptr, name, parms, err_name);

      if (result)
	return result;
    }
  else if (TREE_CODE (instance) == RESULT_DECL)
    {
      static_call_context = 0;
      basetype = TREE_TYPE (instance);
      if (wrap_type)
	{
	  if (basetype_or_else (basetype, wrap_type))
	    basetype = wrap_type;
	  else
	    return error_mark_node;
	}
      /* Should we ever have to make a virtual function reference
	 from a RESULT_DECL, know that it must be of fixed type
	 within the scope of this function.  */
      else if (!(flags & LOOKUP_NONVIRTUAL) && TYPE_VIRTUAL_P (basetype))
	need_vtbl = maybe_needed;
      instance_ptr = build1 (ADDR_EXPR, TYPE_POINTER_TO (basetype), instance);
    }
  else if (instance == current_exception_object)
    {
      instance_ptr = build1 (ADDR_EXPR, TYPE_POINTER_TO (current_exception_type),
			    TREE_OPERAND (current_exception_object, 0));
      mark_addressable (TREE_OPERAND (current_exception_object, 0));
      result = build_field_call (CLASSTYPE_AS_LIST (current_exception_type),
				 instance_ptr, name, parms, err_name);
      if (result)
	return result;
      error ("exception member `%s' cannot be invoked", err_name);
      return error_mark_node;
    }
  else
    {
      /* The MAIN_VARIANT of the type that `instance_ptr' winds up being.  */
      tree inst_ptr_basetype;

      /* from the file "cplus-typeck.c".  */
      extern tree unary_complex_lvalue ();

      static_call_context = (TREE_CODE (instance) == NOP_EXPR
			     && TREE_OPERAND (instance, 0) == error_mark_node);

      /* the base type of an instance variable is pointer to class */
      basetype = TREE_TYPE (instance);

      if (TREE_CODE (basetype) == REFERENCE_TYPE)
	{
	  basetype = TYPE_MAIN_VARIANT (TREE_TYPE (basetype));
	  if (! IS_AGGR_TYPE (basetype))
	    goto non_aggr_error;
	  /* Call to convert not needed because we are remaining
	     within the same type.  */
	  instance_ptr = build1 (NOP_EXPR, TYPE_POINTER_TO (basetype), instance);
	  inst_ptr_basetype = basetype;
	}
      else
	{
	  if (TREE_CODE (basetype) == POINTER_TYPE)
	    {
	      basetype = TREE_TYPE (basetype);
	      instance_ptr = instance;
	    }

	  if (! IS_AGGR_TYPE (basetype))
	    goto non_aggr_error;

	  if (! instance_ptr)
	    {
	      if ((lvalue_p (instance)
		   && (instance_ptr = build_unary_op (ADDR_EXPR, instance, 0)))
		  || (instance_ptr = unary_complex_lvalue (ADDR_EXPR, instance)))
		{
		  if (instance_ptr == error_mark_node)
		    return error_mark_node;
		}
	      else if (TREE_CODE (instance) == NOP_EXPR
		       || TREE_CODE (instance) == CONSTRUCTOR)
		{
		  /* A cast is not an lvalue.  Initialize a fresh temp
		     with the value we are casting from, and proceed with
		     that temporary.  We can't cast to a reference type,
		     so that simplifies the initialization to something
		     we can manage.  */
		  tree temp = get_temp_name (TREE_TYPE (instance), 0);
		  if (IS_AGGR_TYPE (TREE_TYPE (instance)))
		    expand_aggr_init (temp, instance, 0);
		  else
		    {
		      store_init_value (temp, instance);
		      expand_decl_init (temp);
		    }
		  instance = temp;
		  instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
		}
	      else
		{
		  assert (TREE_CODE (instance) == CALL_EXPR);
		  if (TYPE_NEEDS_CONSTRUCTOR (basetype))
		    instance = build_cplus_new (basetype, instance);
		  else
		    {
		      instance = get_temp_name (basetype, 0);
		      TREE_ADDRESSABLE (instance) = 1;
		    }
		  instance_ptr = build_unary_op (ADDR_EXPR, instance, 0);
		}
	      /* @@ Should we call comp_target_types here?  */
	      inst_ptr_basetype = TREE_TYPE (TREE_TYPE (instance_ptr));
	      if (TYPE_MAIN_VARIANT (basetype) == TYPE_MAIN_VARIANT (inst_ptr_basetype))
		basetype = inst_ptr_basetype;
	      else
		instance_ptr = convert (TYPE_POINTER_TO (basetype), instance_ptr);
	    }
	  else
	    inst_ptr_basetype = TREE_TYPE (TREE_TYPE (instance_ptr));
	}

      if (basetype_path == NULL_TREE)
	basetype_path = CLASSTYPE_AS_LIST (inst_ptr_basetype);

      result = build_field_call (basetype_path, instance_ptr, name, parms, err_name);
      if (result)
	return result;

      if (wrap_type)
	{
	  if (basetype_or_else (basetype, wrap_type))
	    basetype = wrap_type;
	  else
	    return error_mark_node;
	}
      else if (!(flags & LOOKUP_NONVIRTUAL) && TYPE_VIRTUAL_P (basetype))
	{
	  if (TREE_VOLATILE (instance_ptr))
	    {
	      /* This action is needed because the instance is needed
		 for providing the base of the virtual function table.
		 Without using a SAVE_EXPR, the function we are building
		 may be called twice, or side effects on the instance
		 variable (such as a post-increment), may happen twice.  */
	      instance_ptr = save_expr (instance_ptr);
	      instance = build_indirect_ref (instance_ptr, 0);
	    }
	  else if (TREE_CODE (TREE_TYPE (instance)) == POINTER_TYPE)
	    {
	      /* This happens when called for operator new ().  */
	      instance = build_indirect_ref (instance, 0);
	    }

	  need_vtbl = maybe_needed;
	}
    }

  if (TYPE_SIZE (basetype) == 0)
    {
      /* This is worth complaining about, I think.  */
      error_with_aggr_type (basetype, "cannot lookup method in incomplete type `%s'");
      return error_mark_node;
    }

  /* Are we building a non-virtual wrapper?  */
  if (flags & LOOKUP_NONVIRTUAL)
    {
      if (all_virtual)
	sorry ("non-virtual call with -fall-virtual");
      if (wrap)
	wrap_type = basetype;
    }

  save_basetype = basetype;

  if (all_virtual == 1
      && (! strncmp (IDENTIFIER_POINTER (name), OPERATOR_METHOD_FORMAT,
		     OPERATOR_METHOD_LENGTH)
	  || instance_ptr == NULL_TREE
	  || (TYPE_OVERLOADS_METHOD_CALL_EXPR (basetype) == 0
	      && TYPE_NEEDS_WRAPPER (basetype) == 0)))
    all_virtual = 0;

  last = NULL_TREE;
  for (parmtypes = 0, parm = parms; parm; parm = TREE_CHAIN (parm))
    {
      tree t = TREE_TYPE (TREE_VALUE (parm));
      if (TREE_CODE (t) == OFFSET_TYPE)
	{
	  /* Convert OFFSET_TYPE entities to their normal selves.  */
	  TREE_VALUE (parm) = resolve_offset_ref (TREE_VALUE (parm));
	  t = TREE_TYPE (TREE_VALUE (parm));
	}
      if (TREE_CODE (t) == ARRAY_TYPE)
	{
	  /* Perform the conversion from ARRAY_TYPE to POINTER_TYPE in place.
	     This eliminates needless calls to `compute_conversion_costs'.  */
	  TREE_VALUE (parm) = default_conversion (TREE_VALUE (parm));
	  t = TREE_TYPE (TREE_VALUE (parm));
	}
      if (t == error_mark_node)
	return error_mark_node;
      last = build_tree_list (NULL_TREE, t);
      parmtypes = chainon (parmtypes, last);
    }

  if (instance)
    {
      constp = TREE_READONLY (instance);
      volatilep = TREE_THIS_VOLATILE (instance);
      parms = tree_cons (NULL_TREE, instance_ptr, parms);
    }
  else
    {
      /* Raw constructors are always in charge.  */
      if (TYPE_USES_VIRTUAL_BASECLASSES (basetype)
	  && ! (flags & LOOKUP_HAS_IN_CHARGE))
	{
	  flags |= LOOKUP_HAS_IN_CHARGE;
	  parms = tree_cons (NULL_TREE, integer_one_node, parms);
	  parmtypes = tree_cons (NULL_TREE, integer_type_node, parmtypes);
	}

      if (flag_this_is_variable)
	{
	  constp = 0;
	  volatilep = 0;
	  parms = tree_cons (NULL_TREE, build1 (NOP_EXPR, TYPE_POINTER_TO (basetype), integer_zero_node), parms);
	}
      else
	{
	  constp = 0;
	  volatilep = 0;
	  instance_ptr = build_new (NULL_TREE, basetype, void_type_node, 0);
	  if (instance_ptr == error_mark_node)
	    return error_mark_node;
	  instance_ptr = save_expr (instance_ptr);
	  TREE_CALLS_NEW (instance_ptr) = 1;
	  instance = build_indirect_ref (instance_ptr, 0);
	  parms = tree_cons (NULL_TREE, instance_ptr, parms);
	}
    }
  parmtypes = tree_cons (NULL_TREE,
			 build_pointer_type (build_type_variant (basetype, constp, volatilep)),
			 parmtypes);
  if (last == NULL_TREE)
    last = parmtypes;

  /* Look up function name in the structure type definition.  */

  if (wrap)
    {
      if (wrap > 0)
	name_kind = "wrapper";
      else
	name_kind = "anti-wrapper";
      baselink = get_wrapper (basetype);
    }
  else
    {
      if (TREE_TYPE (name)
	  && TREE_CODE (TREE_TYPE (name)) == TYPE_DECL
	  && IS_AGGR_TYPE (TREE_TYPE (TREE_TYPE (name))))
	{
	  tree tmp = NULL_TREE;
	  if (TREE_TYPE (name) == TYPE_NAME (basetype))
	    tmp = basetype;
	  else
	    tmp = get_base_type (TREE_TYPE (TREE_TYPE (name)), basetype, 0);
	  if (tmp != 0)
	    {
	      name_kind = "constructor";

	      if (TYPE_USES_VIRTUAL_BASECLASSES (basetype)
	  	  && ! (flags & LOOKUP_HAS_IN_CHARGE))
		{
		  /* Constructors called for initialization
		     only are never in charge.  */
		  tree tmplist;

		  flags |= LOOKUP_HAS_IN_CHARGE;
		  tmplist = tree_cons (NULL_TREE, integer_zero_node,
				       TREE_CHAIN (parms));
		  TREE_CHAIN (parms) = tmplist;
		  tmplist = tree_cons (NULL_TREE, integer_type_node, TREE_CHAIN (parmtypes));
		  TREE_CHAIN (parmtypes) = tmplist;
		}

#ifdef SOS
	      if (TYPE_DYNAMIC (basetype) && dtbl_inserted == 0)
		{
		  tree parm, parmtype;
		  dtbl = get_sos_dtable (basetype);
		  parm = tree_cons (NULL_TREE, dtbl, TREE_CHAIN (parms));
		  parmtype = tree_cons (NULL_TREE, build_pointer_type (ptr_type_node), TREE_CHAIN (parmtypes));
		  TREE_CHAIN (parms) = parm;
		  TREE_CHAIN (parmtypes) = parmtype;
		  dtbl_inserted = -1;
		}
#endif
	      /* constructors are in very specific places.  */
#ifdef SOS
	      if (dtbl_inserted == -1)
		{
		  TREE_CHAIN (parmtypes) = TREE_CHAIN (TREE_CHAIN (parmtypes));
		  TREE_CHAIN (parms) = TREE_CHAIN (TREE_CHAIN (parms));
		  dtbl_inserted = 0;
		}
#endif
	      basetype = tmp;
	    }
	  else
	    name_kind = "method";
	}
      else name_kind = "method";

      if (basetype_path == NULL_TREE)
	basetype_path = CLASSTYPE_AS_LIST (basetype);
      result = lookup_fnfields (basetype_path, name,
				(flags & LOOKUP_COMPLAIN));
      if (result == error_mark_node)
	return error_mark_node;
    }

  /* Now, go look for this method name. We do not find destructors here.

     Putting `void_list_node' on the end of the parmtypes
     fakes out `build_decl_overload' into doing the right thing.  */
  TREE_CHAIN (last) = void_list_node;
  method_name = build_decl_overload (IDENTIFIER_POINTER (name),
				     parmtypes,
				     1 + (name == DECL_NAME (TYPE_NAME (save_basetype))));
  TREE_CHAIN (last) = NULL_TREE;

  for (pass = 0; pass < 2; pass++)
    {
      struct candidate *candidates;
      struct candidate *cp;
      int len, best = 2;

      /* This increments every time we go up the type hierarchy.
	 The idea is to prefer a function of the derived class if possible.  */
      int b_or_d;

      baselink = result;

      if (pass > 0)
	{
	  candidates = (struct candidate *) alloca ((ever_seen+1) * sizeof (struct candidate));
	  cp = candidates;
	  len = list_length (parms);
	  b_or_d = 0;

	  /* First see if a global function has a shot at it.  */
	  if (flags & LOOKUP_GLOBAL)
	    {
	      tree friend_parms;
	      tree parm = TREE_VALUE (parms);

	      if (TREE_CODE (TREE_TYPE (parm)) == REFERENCE_TYPE)
		friend_parms = parms;
	      else if (TREE_CODE (TREE_TYPE (parm)) == POINTER_TYPE)
		{
		  parm = build_indirect_ref (parm, "friendifying parms (compiler error)");
		  parm = convert (build_reference_type (TREE_TYPE (parm)), parm);
		  friend_parms = tree_cons (NULL_TREE, parm, TREE_CHAIN (parms));
		}
	      else
		assert (0);

	      cp->harshness
		= (unsigned short *)alloca ((len+1) * sizeof (short));
	      result = build_overload_call (name, friend_parms, 0, cp);
	      /* If it turns out to be the one we were actually looking for
		 (it was probably a friend function), the return the
		 good result.  */
	      if (TREE_CODE (result) == CALL_EXPR)
		return result;

	      while (cp->evil == 0)
		{
		  /* non-standard uses: set the field to 0 to indicate
		     we are using a non-member function.  */
		  cp->u.field = 0;
		  if (cp->harshness[len] == 0
		      && cp->harshness[len] == 0
		      && cp->user == 0 && cp->b_or_d == 0
		      && cp->easy < best)
		    best = cp->easy;
		  cp += 1;
		}
	    }
	}

      while (baselink)
	{
	  /* We have a hit (of sorts). If the parameter list is
	     "error_mark_node", or some variant thereof, it won't
	     match any methods. Since we have verified that the is
	     some method vaguely matching this one (in name at least),
	     silently return.
	     
	     Don't stop for friends, however.  */
	  tree basetypes = TREE_PURPOSE (baselink);

	  function = TREE_VALUE (baselink);
	  basetype = TREE_VALUE (basetypes);

	  /* Cast the instance variable to the approriate type.  */
	  TREE_VALUE (parmtypes) = TYPE_POINTER_TO (basetype);

	  if (DESTRUCTOR_NAME_P (DECL_NAME (function)))
	    function = TREE_CHAIN (function);

	  for (; function; function = TREE_CHAIN (function))
	    {
#ifdef GATHER_STATISTICS
	      n_inner_fields_searched++;
#endif
	      ever_seen++;

	      /* Not looking for friends here.  */
	      if (TREE_CODE (TREE_TYPE (function)) == FUNCTION_TYPE
		  && ! DECL_STATIC_FUNCTION_P (function))
		continue;

	      if (pass == 0
		  && DECL_NAME (function) == method_name)
		{
		  if (flags & LOOKUP_PROTECT)
		    {
		      visibility = compute_visibility (basetypes, function);
		      if (visibility == visibility_protected
			  && flags & LOOKUP_PROTECTED_OK)
			visibility = visibility_public;
		    }

		  if ((flags & LOOKUP_PROTECT) == 0
		      || visibility == visibility_public)
		    goto found_and_ok;
		  else if (visibility == visibility_private)
		    saw_private = function;
		  else if (visibility == visibility_protected)
		    saw_protected = function;
		  /* If we fail on the exact match, we have
		     an immediate failure.  */
		  goto found;
		}
	      if (pass > 0)
		{
		  tree these_parms = parms;

#ifdef GATHER_STATISTICS
		  n_inner_fields_searched++;
#endif
		  cp->harshness
		    = (unsigned short *)alloca ((len+1) * sizeof (short));
		  if (DECL_STATIC_FUNCTION_P (function))
		    these_parms = TREE_CHAIN (these_parms);
		  compute_conversion_costs (function, these_parms, cp, len);
		  cp->b_or_d += b_or_d;
		  if (cp->evil == 0)
		    {
		      cp->u.field = function;
		      cp->function = function;
		      if (flags & LOOKUP_PROTECT)
			{
			  enum visibility_type this_v;
			  this_v = compute_visibility (basetypes, function);
			  if (this_v == visibility_protected
			      && (flags & LOOKUP_PROTECTED_OK))
			    this_v = visibility_public;
			  if (this_v != visibility_public)
			    {
			      if (this_v == visibility_private)
				saw_private = function;
			      else
				saw_protected = function;
			      continue;
			    }
			}

		      /* No "two-level" conversions.  */
		      if (flags & LOOKUP_NO_CONVERSION && cp->user != 0)
			continue;

		      /* If we used default parameters, we must
			 check to see whether anyone else might
			 use them also, and report a possible
			 ambiguity.  */
		      if (! TYPE_USES_MULTIPLE_INHERITANCE (save_basetype)
			  && cp->harshness[len] == 0
			  && (cp->harshness[0] & 128) == 0
			  && cp->user == 0 && cp->b_or_d == 0
			  && cp->easy < best)
			{
			  if (! DECL_STATIC_FUNCTION_P (function))
			    TREE_VALUE (parms) = cp->arg;
			  if (best == 2)
			    goto found_and_maybe_warn;
			}
		      cp++;
		    }
		}
	    }
	  /* Now we have run through one link's member functions.
	     arrange to head-insert this link's links.  */
	  baselink = next_baselink (baselink);
	  b_or_d += 1;
	}
      if (pass == 0)
	{
	  /* No exact match could be found.  Now try to find match
	     using default conversions.  */
	  if ((flags & LOOKUP_GLOBAL) && IDENTIFIER_GLOBAL_VALUE (name))
	    if (TREE_CODE (IDENTIFIER_GLOBAL_VALUE (name)) == FUNCTION_DECL)
	      ever_seen += 1;
	    else if (TREE_CODE (IDENTIFIER_GLOBAL_VALUE (name)) == TREE_LIST)
	      ever_seen += list_length (IDENTIFIER_GLOBAL_VALUE (name));

	  if (ever_seen == 0)
	    {
	      if (flags & LOOKUP_GLOBAL)
		error ("no global or member function `%s' defined", err_name);
	      else
		error_with_aggr_type (save_basetype, "no member function `%s::%s'", err_name);
	      return error_mark_node;
	    }
	  continue;
	}

      if (cp - candidates != 0)
	{
	  /* Rank from worst to best.  Then cp will point to best one.
	     Private fields have their bits flipped.  For unsigned
	     numbers, this should make them look very large.
	     If the best alternate has a (signed) negative value,
	     then all we ever saw were private members.  */
	  if (cp - candidates > 1)
	    {
	      cp = ideal_candidate (save_basetype, candidates,
				    cp - candidates, parms, len);
	      if (cp == 0)
		{
		  error ("ambiguous type conversion requested for %s `%s'",
			 name_kind, err_name);
		  return error_mark_node;
		}
	    }
	  else if (cp[-1].evil == 2)
	    {
	      error ("ambiguous type conversion requested for %s `%s'",
		     name_kind, err_name);
	      return error_mark_node;
	    }
	  else cp--;

	  /* The global function was the best, so use it.  */
	  if (cp->u.field == 0)
	    {
	      /* We must convert the instance pointer into a reference type.
		 Global overloaded functions can only either take
		 aggregate objects (which come for free from references)
		 or reference data types anyway.  */
	      TREE_VALUE (parms) = copy_node (instance_ptr);
	      TREE_TYPE (TREE_VALUE (parms)) = build_reference_type (TREE_TYPE (TREE_TYPE (instance_ptr)));
	      return build_function_call (cp->function, parms);
	    }

	  function = cp->function;
	  if (DECL_STATIC_FUNCTION_P (function))
	    basetype = NULL_TREE;
	  else
	    {
	      basetype = TREE_TYPE (TREE_TYPE (cp->arg));
	      TREE_VALUE (parms) = cp->arg;
	    }
	  goto found_and_maybe_warn;
	}

      if ((flags & ~LOOKUP_GLOBAL) & (LOOKUP_COMPLAIN|LOOKUP_SPECULATIVELY))
	{
	  char *tag_name, *buf;

	  if ((flags & (LOOKUP_SPECULATIVELY|LOOKUP_COMPLAIN))
	      == LOOKUP_SPECULATIVELY)
	    return NULL_TREE;

	  if (DECL_STATIC_FUNCTION_P (cp->function))
	    parms = TREE_CHAIN (parms);
	  if (ever_seen)
	    {
	      if (((int)saw_protected|(int)saw_private) == 0)
		{
		  if (flags & LOOKUP_SPECULATIVELY)
		    return NULL_TREE;
		  if (static_call_context && TREE_CODE (TREE_TYPE (cp->function)) == METHOD_TYPE)
		    error_with_aggr_type (TREE_TYPE (TREE_TYPE (instance_ptr)),
					  "object missing in call to `%s::%s'",
					  err_name);
		  else
		    report_type_mismatch (cp, parms, name_kind, err_name);
		}
	      else
		{
		  char buf[80];
		  char *msg;
		  tree seen = saw_private;

		  if (saw_private)
		    if (saw_protected)
		      msg = "%s %%s (and the like) are private or protected";
		    else
		      msg = "the %s %%s is private";
		  else
		    {
		      msg = "the %s %%s is protected";
		      seen = saw_protected;
		    }
		  sprintf (buf, msg, name_kind);
		  error_with_decl (seen, buf);
		  error ("within this context");
		}
	      return error_mark_node;
	    }

	  if ((flags & (LOOKUP_SPECULATIVELY|LOOKUP_COMPLAIN))
	      == LOOKUP_COMPLAIN)
	    {
	      if (TREE_CODE (save_basetype) == RECORD_TYPE)
		tag_name = "structure";
	      else
		tag_name = "union";

	      if (wrap)
		buf = "%s has no appropriate wrapper function defined";
	      else
		{
		  buf = (char *)alloca (30 + strlen (err_name));
		  strcpy (buf, "%s has no method named `%s'");
		}

	      error (buf, tag_name, err_name);
	      return error_mark_node;
	    }
	  return NULL_TREE;
	}
      continue;

    found_and_maybe_warn:
      if (cp->harshness[0] & 128)
	{
	  if (flags & LOOKUP_COMPLAIN)
	    {
	      error_with_decl (cp->function, "non-const member function `%s'");
	      error ("called for const object at this point in file");
	    }
	  /* Not good enough for a match.  */
	  else return error_mark_node;
	}
      goto found_and_ok;
    }
  /* Silently return error_mark_node.  */
  return error_mark_node;

 found:
  if (visibility == visibility_private)
    {
      if (flags & LOOKUP_COMPLAIN)
	error (TREE_PRIVATE (function)
	       ? "%s `%s' is private"
	       : "%s `%s' is from private base class",
	       name_kind,
	       lang_printable_name (function));
      return error_mark_node;
    }
  else if (visibility == visibility_protected)
    {
      if (flags & LOOKUP_COMPLAIN)
	error (TREE_PROTECTED (function)
	       ? "%s `%s' is protected"
	       : "%s `%s' has protected visibility from this point",
	       name_kind,
	       lang_printable_name (function));
      return error_mark_node;
    }
  abort ();

 found_and_ok:

  /* From here on down, BASETYPE is the type that INSTANCE_PTR's
     type (if it exists) is a pointer to.  */
  basetype = DECL_CONTEXT (function);
  fntype = TREE_TYPE (function);

  if (TREE_CODE (fntype) == POINTER_TYPE)
    fntype = TREE_TYPE (fntype);

  /* If we are referencing a virtual function from an object
     of effectively static type, then there is no need
     to go through the virtual function table.  */
  if (need_vtbl == maybe_needed)
    {
      int fixed_type = resolves_to_fixed_type_p (instance);

      if (all_virtual == 1
	  && DECL_VINDEX (function)
	  && may_be_remote (basetype))
	need_vtbl = needed;
      else if (DECL_VIRTUAL_P (function))
	need_vtbl = fixed_type ? unneeded : needed;
      else
	need_vtbl = not_needed;

      if (fixed_type && DECL_ABSTRACT_VIRTUAL_P (function))
	{
	  error_with_decl (function, "invalid call to abstract function `%s'");
	  return error_mark_node;
	}
    }

  if (TREE_CODE (fntype) == METHOD_TYPE && static_call_context)
    {
      /* Let's be nice to the user for now, and give reasonable
	 default behavior.  */
      instance_ptr = current_class_decl;
      if (instance_ptr)
	{
	  if (basetype != current_class_type)
	    {
	      basetype = get_base_type (basetype, current_class_type, 1);
	      if (basetype == 0)
		{
		  error_not_base_type (DECL_CONTEXT (function), current_class_type);
		  return error_mark_node;
		}
	      else if (basetype == error_mark_node)
		return error_mark_node;
	    }
	}
      else
	{
	  error_with_aggr_type (basetype, "cannot call member function `%s::%s' without object",
				err_name);
	  return error_mark_node;
	}
    }

  value_type = TREE_TYPE (fntype) ? TREE_TYPE (fntype) : void_type_node;

  if (TYPE_SIZE (value_type) == 0)
    {
      if (flags & LOOKUP_COMPLAIN)
	incomplete_type_error (0, value_type);
      return error_mark_node;
    }

  /* We do not pass FUNCTION into `actualparameterlist', because by
     now everything should be ok.  If not, then we have a serious error.  */
  if (DECL_STATIC_FUNCTION_P (function))
    parms = actualparameterlist (NULL_TREE, TYPE_ARG_TYPES (fntype),
				 TREE_CHAIN (parms), NULL_TREE, LOOKUP_NORMAL);
  else if (need_vtbl == unneeded)
    {
      int sub_flags = DECL_CONSTRUCTOR_P (function) ? flags : LOOKUP_NORMAL;
      basetype = TREE_TYPE (instance);
      if (DECL_CONTEXT (function) != TYPE_MAIN_VARIANT (basetype)
	  && (TYPE_USES_MULTIPLE_INHERITANCE (basetype)
	      || TYPE_USES_VIRTUAL_BASECLASSES (basetype)))
	{
	  basetype = DECL_CONTEXT (function);
	  instance_ptr = convert_pointer_to (basetype, instance_ptr);
	  instance = build_indirect_ref (instance_ptr, 0);
	}
      parms = tree_cons (NULL_TREE, instance_ptr,
			 actualparameterlist (NULL_TREE, TREE_CHAIN (TYPE_ARG_TYPES (fntype)), TREE_CHAIN (parms), NULL_TREE, sub_flags));
    }
  else
    {
      if ((flags & LOOKUP_NONVIRTUAL) == 0)
	basetype = DECL_VCONTEXT (function);

      /* First parm could be integer_zerop with casts like
	 ((Object*)0)->Object::IsA()  */
      if (!integer_zerop (TREE_VALUE (parms)))
	{
	  instance_ptr = convert_pointer_to (build_type_variant (basetype, constp, volatilep),
					     TREE_VALUE (parms));
	  if (TREE_CODE (instance_ptr) == COND_EXPR)
	    {
	      instance_ptr = save_expr (instance_ptr);
	      instance = build_indirect_ref (instance_ptr);
	    }
	  else if (TREE_CODE (instance_ptr) == NOP_EXPR
		   && TREE_CODE (TREE_OPERAND (instance_ptr, 0)) == ADDR_EXPR
		   && TREE_OPERAND (TREE_OPERAND (instance_ptr, 0), 0) == instance)
	    ;
	  else if (instance == NULL_TREE
		   || TREE_CODE (instance) != INDIRECT_REF
		   || TREE_OPERAND (instance, 0) != instance_ptr)
	    instance = build_indirect_ref (instance_ptr);
	}
      parms = tree_cons (NULL_TREE, instance_ptr,
			 actualparameterlist (NULL_TREE, TREE_CHAIN (TYPE_ARG_TYPES (fntype)), TREE_CHAIN (parms), NULL_TREE, LOOKUP_NORMAL));
    }

  /* See if there is a wrapper for this thing.  */
  if (wrap < 0
      || static_call_context
      || name == wrapper_name
      || name == DECL_NAME (TYPE_NAME (basetype)))
    ;
  else if (wrap > 0 || TYPE_NEEDS_WRAPPER (basetype))
    {
      flags &= ~LOOKUP_PROTECT;
      if (wrap == 0)
	{
	  wrap = TYPE_NEEDS_WRAPPER (basetype);
	  /* If no wrapper specified, wrapper may be virtual.  */
	  flags &= ~LOOKUP_NONVIRTUAL;
	}

      if (wrap)
	{
	  tree wrapped_result, unwrapped_result;
	  register int bytecount = get_arglist_len_in_bytes (parms);

	  if (!all_virtual && TREE_CODE (function) == FUNCTION_DECL)
	    parm = build_unary_op (ADDR_EXPR, function, 0);
	  else
	    {
              fntype = build_cplus_method_type (basetype, TREE_TYPE (fntype), TYPE_ARG_TYPES (fntype));
	      parm = build1 (NOP_EXPR, build_pointer_type (fntype), DECL_VINDEX (function));
	    }

	  if (TYPE_HAS_WRAPPER_PRED (basetype))
	    {
	      unwrapped_result = build_nt (CALL_EXPR, default_conversion (function), parms, NULL_TREE);

	      assert (TREE_OPERAND (unwrapped_result, 1) != error_mark_node);

	      TREE_TYPE (unwrapped_result) = value_type;
	      TREE_VOLATILE (unwrapped_result) = 1;
	      TREE_RAISES (unwrapped_result) = !! TYPE_RAISES_EXCEPTIONS (fntype);
	    }

	  /* If this pointer walked as a result of multiple inheritance,
	     keep its displaced value.  */
	  parms = tree_cons (NULL_TREE, build_int_2 (bytecount, 0),
			     tree_cons (NULL_TREE, parm, TREE_CHAIN (parms)));

	  wrapped_result = get_wrapper (basetype);
	  assert (wrapped_result != NULL_TREE);
	  assert (wrapped_result != error_mark_node);

	  /* @@ Should BASETYPE_PATH get TREE_PURPOSE (wrapped_result) here?  */
	  wrapped_result
	    = build_method_call (instance,
				 DECL_ORIGINAL_NAME (TREE_VALUE (wrapped_result)),
				 parms, basetype_path, flags);
#if 0
	  /* Do this if we want the result of operator->() to inherit
	     the type of the function it is subbing for.  */
	  if (wrapped_result != error_mark_node)
	    TREE_TYPE (wrapped_result) = value_type;
#endif

	  if (TYPE_HAS_WRAPPER_PRED (basetype))
	    {
	      result = build_conditional_expr
		(build_method_call (instance, wrapper_pred_name, build_tree_list (NULL_TREE, parm), basetype_path, LOOKUP_NORMAL),
		 wrapped_result,
		 unwrapped_result);

	    }
	  else
	    {
	      result = wrapped_result;
	    }

	  TREE_VOLATILE (result) = 1;
	  return result;
	}
    }
  /* Constructors do not overload method calls.  */
  else if (TYPE_OVERLOADS_METHOD_CALL_EXPR (basetype)
	   && name != DECL_NAME (TYPE_NAME (basetype))
	   && (TREE_CODE (function) != FUNCTION_DECL
	       || strncmp (IDENTIFIER_POINTER (DECL_NAME (function)),
			   OPERATOR_METHOD_FORMAT,
			   OPERATOR_METHOD_LENGTH))
#if 0
	   && (may_be_remote (basetype)
	       || (C_C_D ? TREE_TYPE (instance) != current_class_type : 1))
#else
	   /* This change by Larry Ketcham.  */
  	   && (may_be_remote (basetype) || instance != C_C_D)
#endif
	   )
    {
#ifdef ESKIT
      register int bytecount = 0;
#else
      register int bytecount = get_arglist_len_in_bytes (parms);
#endif
      tree fn_as_int;

      parms = tree_cons (NULL_TREE, build_int_2 (bytecount, 0),
			 TREE_CHAIN (parms));

      if (!all_virtual && TREE_CODE (function) == FUNCTION_DECL)
	fn_as_int = build_unary_op (ADDR_EXPR, function, 0);
      else
	fn_as_int = convert (TREE_TYPE (default_conversion (function)), DECL_VINDEX (function));
      if (all_virtual == 1)
	fn_as_int = convert (integer_type_node, fn_as_int);

      result = build_opfncall (METHOD_CALL_EXPR, LOOKUP_NORMAL, instance, fn_as_int, parms);

      if (result == NULL_TREE)
	{
	  compiler_error ("could not overload `operator->()(...)'");
	  return error_mark_node;
	}
      else if (result == error_mark_node)
	return error_mark_node;

#if 0
      /* Do this if we want the result of operator->() to inherit
	 the type of the function it is subbing for.  */
      TREE_TYPE (result) = value_type;
#endif

#ifdef ESKIT
      {
	int used, size;

	/* Count the number of bytes of arguements to operator->(),
	   not to the method itself.  In the tally, don't count bytes
	   for pointer to member function or for the bytecount.  */
	parms = TREE_OPERAND (result, 1);
	bytecount = get_arglist_len_in_bytes (TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (parms))));
	used = size = GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (TREE_VALUE (parms))));
#ifdef PUSH_ROUNDING
	size = PUSH_ROUNDING (size);
#endif
	used = (((size + PARM_BOUNDARY / BITS_PER_UNIT - 1)
		 / (PARM_BOUNDARY / BITS_PER_UNIT))
		* (PARM_BOUNDARY / BITS_PER_UNIT));
	bytecount += used;
	TREE_CHAIN (TREE_CHAIN (parms))
	  = tree_cons (NULL_TREE, build_int_2 (bytecount, 0),
		       TREE_CHAIN (TREE_CHAIN (TREE_CHAIN (parms))));
      }
#endif

      return result;
    }

  if (need_vtbl == needed)
    {
      function = build_vfn_ref (&TREE_VALUE (parms), instance, DECL_VINDEX (function));
      TREE_TYPE (function) = build_pointer_type (fntype);
    }
#ifdef SOS
  else if (basetype && TYPE_DYNAMIC (basetype))
    {
      function = build_array_ref (dtbl, DECL_DINDEX (function));
      TREE_TYPE (function) = build_pointer_type (fntype);
    }
#endif

  if (TREE_INLINE (function) && TREE_CODE (function) == FUNCTION_DECL)
    function = build1 (ADDR_EXPR, build_pointer_type (fntype), function);
  else function = default_conversion (function);

  result =
    build_nt (CALL_EXPR, function, parms, NULL_TREE);

  TREE_TYPE (result) = value_type;
  TREE_VOLATILE (result) = 1;
  TREE_RAISES (result)
    = TYPE_RAISES_EXCEPTIONS (fntype) || (parms && TREE_RAISES (parms));
  return result;
}

/* Similar to `build_method_call', but for overloaded non-member functions.
   The name of this function comes through NAME.  The name depends
   on PARMS.

   Note that this function must handle simple `C' promotions,
   as well as variable numbers of arguments (...), and
   default arguments to boot.

   If the overloading is successful, we return a treenode which
   contains the call to the function.

   If overloading produces candidates which are probabe, but not definite,
   we hold these candidates.  If FINAL_CP is non-zero, then we are free
   to assume that final_cp points to enough storage for all candidates that
   this function might generate.  The `harshness' array is preallocated for
   the first candidate, but not for subsequent ones.

   Note that the DECL_RTL of FUNCTION must be made to agree with this
   function's new name.  */

tree
build_overload_call (fnname, parms, complain, final_cp)
     tree fnname, parms;
     int complain;
     struct candidate *final_cp;
{
  /* must check for overloading here */
  tree overload_name, functions, function, parm;
  tree parmtypes = NULL_TREE, last = NULL_TREE;
  register tree outer;
  int length;
  int parmlength = list_length (parms);

  struct candidate *candidates, *cp;
  int rank_for_overload ();

  if (final_cp)
    {
      final_cp[0].evil = 0;
      final_cp[0].user = 0;
      final_cp[0].b_or_d = 0;
      final_cp[0].easy = 0;
      final_cp[0].function = 0;
      /* end marker.  */
      final_cp[1].evil = 1;
    }

  for (parm = parms; parm; parm = TREE_CHAIN (parm))
    {
      register tree t = TREE_TYPE (TREE_VALUE (parm));

      if (t == error_mark_node)
	return error_mark_node;
      if (TREE_CODE (t) == ARRAY_TYPE || TREE_CODE (t) == OFFSET_TYPE)
	{
	  /* Perform the conversion from ARRAY_TYPE to POINTER_TYPE in place.
	     Also convert OFFSET_TYPE entities to their normal selves.
	     This eliminates needless calls to `compute_conversion_costs'.  */
	  TREE_VALUE (parm) = default_conversion (TREE_VALUE (parm));
	  t = TREE_TYPE (TREE_VALUE (parm));
	}
      last = build_tree_list (NULL_TREE, t);
      parmtypes = chainon (parmtypes, last);
    }
  if (last)
    TREE_CHAIN (last) = void_list_node;
  else
    parmtypes = void_list_node;
  overload_name = build_decl_overload (IDENTIFIER_POINTER (fnname), parmtypes, 0);

  /* Now check to see whether or not we can win.
     Note that if we are called from `build_method_call',
     then we cannot have a mis-match, because we would have
     already found such a winning case.  */

  if (IDENTIFIER_GLOBAL_VALUE (overload_name))
    if (TREE_CODE (IDENTIFIER_GLOBAL_VALUE (overload_name)) != TREE_LIST)
      return build_function_call (DECL_MAIN_VARIANT (IDENTIFIER_GLOBAL_VALUE (overload_name)), parms);

  functions = IDENTIFIER_GLOBAL_VALUE (fnname);

  if (functions == NULL_TREE)
    {
      if (complain)
	error ("only member functions apply");
      if (final_cp)
	final_cp->evil = 1;
      return error_mark_node;
    }

  if (TREE_CODE (functions) == FUNCTION_DECL)
    {
      functions = DECL_MAIN_VARIANT (functions);
      if (final_cp)
	{
	  /* We are just curious whether this is a viable alternative or not.  */
	  compute_conversion_costs (functions, parms, final_cp, parmlength);
	  return functions;
	}
      else
	return build_function_call (functions, parms);
    }

  if (TREE_VALUE (functions) == NULL_TREE)
    {
      if (complain)
	error ("function `%s' declared overloaded, but no instances of that function declared",
	       IDENTIFIER_POINTER (TREE_PURPOSE (functions)));
      return error_mark_node;
    }

  if (TREE_CODE (TREE_VALUE (functions)) == TREE_LIST)
    {
      register tree outer;
      length = 0;

      /* The list-of-lists should only occur for class things.  */
      assert (functions == IDENTIFIER_CLASS_VALUE (fnname));

      for (outer = functions; outer; outer = TREE_CHAIN (outer))
	{
	  /* member functions.  */
	  length += list_length (TREE_VALUE (TREE_VALUE (outer)));
	  /* friend functions.  */
	  length += list_length (TREE_TYPE (TREE_VALUE (outer)));
	}
    }
  else
    {
      length = list_length (functions);
    }

  if (final_cp)
    candidates = final_cp;
  else
    candidates = (struct candidate *)alloca ((length+1) * sizeof (struct candidate));

  cp = candidates;

  assert (TREE_CODE (TREE_VALUE (functions)) != TREE_LIST);
  /* OUTER is the list of FUNCTION_DECLS, in a TREE_LIST.  */

  for (outer = functions; outer; outer = TREE_CHAIN (outer))
    {
      function = TREE_VALUE (outer);
      if (TREE_CODE (function) != FUNCTION_DECL)
	{
	  if (TREE_CODE (function) == CONST_DECL)
	    error_with_decl (function, "enumeral value `%s' conflicts with function of same name");
	  else if (TREE_CODE (function) == VAR_DECL)
	    if (TREE_STATIC (function))
	      error_with_decl (function, "variable `%s' conflicts with function of same name");
	    else
	      error_with_decl (function, "constant field `%s' conflicts with function of same name");
	  else if (TREE_CODE (function) == TYPE_DECL)
	    continue;
	  else abort ();
	  error ("at this point in file");
	  continue;
	}
      function = DECL_MAIN_VARIANT (function);
      /* Can't use alloca here, since result might be
	 passed to calling function.  */
      cp->harshness
	= (unsigned short *)oballoc ((parmlength+1) * sizeof (short));
      compute_conversion_costs (function, parms, cp, parmlength);
      if (cp[0].evil == 0)
	{
	  cp[1].evil = 1;
	  if (final_cp
	      && cp[0].user == 0 && cp[0].b_or_d == 0
	      && cp[0].easy <= 1)
	    {
	      final_cp[0].easy = cp[0].easy;
	      return function;
	    }
	  cp++;
	}
    }

  if (cp - candidates)
    {
      tree rval = error_mark_node;

      /* Leave marker.  */
      cp[0].evil = 1;
      if (cp - candidates > 1)
	{
	  struct candidate *best_cp
	    = ideal_candidate (NULL_TREE, candidates,
			       cp - candidates, parms, parmlength);
	  if (best_cp == 0)
	    {
	      if (complain)
		error ("call of overloaded `%s' is ambiguous", IDENTIFIER_POINTER (fnname));
	      return error_mark_node;
	    }
	  else
	    rval = best_cp->function;
	}
      else
	{
	  cp -= 1;
	  if (cp->evil > 1)
	    {
	      if (complain)
		error ("type conversion ambiguous");
	    }
	  else
	    rval = cp->function;
	}

      if (final_cp)
	return rval;

      return build_function_call (rval, parms);
    }
  else if (complain)
    {
      tree name;
      char *err_name;
      /* Initialize name for error reporting.  */
      if (TREE_CODE (functions) == TREE_LIST)
	name = TREE_PURPOSE (functions);
      else
	name = DECL_ORIGINAL_NAME (functions);

      if (OPERATOR_NAME_P (name))
	{
	  char *opname = operator_name_string (name);
	  err_name = (char *)alloca (strlen (opname) + 12);
	  sprintf (err_name, "operator %s", opname);
	}
      else
	err_name = IDENTIFIER_POINTER (name);

      report_type_mismatch (cp, parms, "function", err_name);
    }
  return error_mark_node;
}

void
init_class_processing ()
{
  current_class_stacksize = 10;
  current_class_base = (tree *)xmalloc(current_class_stacksize * sizeof (tree));
  current_class_stack = current_class_base;

  current_lang_stacksize = 10;
  current_lang_base = (tree *)xmalloc(current_lang_stacksize * sizeof (tree));
  current_lang_stack = current_lang_base;

  delta_name = get_identifier (VTABLE_DELTA_NAME);
  pfn_name = get_identifier (VTABLE_PFN_NAME);

  /* Keep these values lying around.  */
  minus_one_node = build_int_2 (-1, 0);
  the_null_vtable_entry = build_vtable_entry (integer_zero_node, integer_zero_node);
  base_layout_decl = build_lang_field_decl (FIELD_DECL, NULL_TREE, error_mark_node);
  TREE_TYPE (base_layout_decl) = make_node (RECORD_TYPE);

  obstack_init (&class_obstack);
}

/* Set current scope to NAME. CODE tells us if this is a
   STRUCT, UNION, or ENUM environment.

   NAME may end up being NULL_TREE if this is an anonymous or
   late-bound struct (as in "struct { ... } foo;")  */

/* Here's a subroutine we need because C lacks lambdas.  */
void
unuse_fields (type)
     tree type;
{
  tree fields;

  for (fields = TYPE_FIELDS (type); fields; fields = TREE_CHAIN (fields))
    {
      if (TREE_CODE (fields) != FIELD_DECL)
	continue;

      TREE_USED (fields) = 0;
      if (DECL_ANON_UNION_ELEM (fields))
	unuse_fields (TREE_TYPE (fields));
    }
}

/* Set global variables CURRENT_CLASS_NAME and CURRENT_CLASS_TYPE to
   appropriate values, found by looking up the type definition of
   NAME (as a CODE).

   If MODIFY is 1, we set IDENTIFIER_CLASS_VALUE's of names
   which can be seen locally to the class. They are shadowed by
   any subsequent local declaration (including parameter names).

   If MODIFY is 2, we set IDENTIFIER_CLASS_VALUE's of names
   which have static meaning (i.e., static members, static
   member functions, enum declarations, etc).

   So that we may avoid calls to lookup_name, we cache the TYPE_DECL
   in the TREE_TYPE field of the name.

   For multiple inheritance, we perform a two-pass depth-first search
   of the type lattice.  The first pass performs a pre-order search,
   marking types after the type has had its fields installed in
   the appropriate IDENTIFIER_CLASS_VALUE slot.  The second pass merely
   unmarks the marked types.  If a field or member function name
   appears in an ambiguous way, the IDENTIFIER_CLASS_VALUE of
   that name becomes `error_mark_node'.  */

void
pushclass (type, modify)
     tree type;
     int modify;
{
  push_memoized_context (type, modify);

  *current_class_stack++ = current_class_name;
  *current_class_stack++ = current_class_type;
  if (current_class_stack >= current_class_base + current_class_stacksize)
    {
      current_class_base =
	(tree *)xrealloc (current_class_base,
			  sizeof (tree) * (current_class_stacksize + 10));
      current_class_stack = current_class_base + current_class_stacksize;
      current_class_stacksize += 10;
    }

  type = TYPE_MAIN_VARIANT (type);
  current_class_name = TYPE_NAME (type);
  if (TREE_CODE (current_class_name) == TYPE_DECL)
    current_class_name = DECL_NAME (current_class_name);
  current_class_type = type;

  if (type != prev_class_type && prev_class_type != NULL_TREE
      && current_class_stack == current_class_base + 2)
    {
      popclass (-1);
      prev_class_type = 0;
    }

  if (modify)
    {
      tree tags;

      if (type != prev_class_type)
	{
	  build_mi_matrix (type);
	  push_class_decls (type);
	  free_mi_matrix ();
	  prev_class_type = type;
	}
      else
	unuse_fields (type);

      tags = CLASSTYPE_TAGS (type);
      while (tags)
	{
	  TREE_NONLOCAL (TREE_VALUE (tags)) = 1;
	  pushtag (TREE_PURPOSE (tags), TREE_VALUE (tags));
	  if (IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (tags)) == NULL_TREE
	      && TREE_CODE (TYPE_NAME (TREE_VALUE (tags))) == TYPE_DECL)
	    IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (tags))
	      = TYPE_NAME (TREE_VALUE (tags));
	  tags = TREE_CHAIN (tags);
	}
    }
  else
    pushlevel_class ();

  if (flag_cadillac)
    cadillac_push_class (type);
}
 
/* Get out of the current class scope. If we were in a class scope
   previously, that is the one popped to.  The flag MODIFY tells
   whether the current scope declarations needs to be modified
   as a result of popping to the new scope.  */
void
popclass (modify)
     int modify;
{
  if (flag_cadillac)
    cadillac_pop_class ();

  if (modify < 0)
    {
      /* Back this old class out completely.  */
      tree tags = CLASSTYPE_TAGS (prev_class_type);

      pop_class_decls (prev_class_type);
      while (tags)
	{
	  TREE_NONLOCAL (TREE_VALUE (tags)) = 0;
	  IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (tags)) = NULL_TREE;
	  tags = TREE_CHAIN (tags);
	}
      return;
    }
  if (modify)
    {
      /* Just remove from this class what didn't make
	 it into IDENTIFIER_CLASS_VALUE.  */
      tree tags = CLASSTYPE_TAGS (current_class_type);

      while (tags)
	{
	  TREE_NONLOCAL (TREE_VALUE (tags)) = 0;
	  IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (tags)) = NULL_TREE;
	  tags = TREE_CHAIN (tags);
	}
    }
  else
    poplevel_class ();

  current_class_type = *--current_class_stack;
  current_class_name = *--current_class_stack;

  if (current_class_type)
    {
      if (CLASSTYPE_VTBL_PTR (current_class_type))
	{
	  current_vtable_decl = lookup_name (DECL_NAME (CLASSTYPE_VTBL_PTR (current_class_type)));
	  if (current_vtable_decl)
	    current_vtable_decl = build_indirect_ref (current_vtable_decl, 0);
	}
      current_class_decl = lookup_name (get_identifier (THIS_NAME));
      if (current_class_decl)
	{
	  if (TREE_CODE (TREE_TYPE (current_class_decl)) == POINTER_TYPE)
	    {
	      /* Can't call build_indirect_ref here, because it has special
		 logic to return C_C_D given this argument.  */
	      C_C_D = build1 (INDIRECT_REF, current_class_type, current_class_decl);
	      TREE_READONLY (C_C_D) = TREE_READONLY (TREE_TYPE (TREE_TYPE (current_class_decl)));
	      TREE_VOLATILE (C_C_D) = TREE_VOLATILE (TREE_TYPE (TREE_TYPE (current_class_decl)));
	    }
	  else
	    C_C_D = current_class_decl;
	}
      else C_C_D = NULL_TREE;
    }
  else
    {
      current_class_decl = NULL_TREE;
      current_vtable_decl = NULL_TREE;
      C_C_D = NULL_TREE;
    }

  pop_memoized_context (modify);
}

/* Set global variables CURRENT_LANG_NAME to appropriate value
   so that behavior of name-mangline machinery is correct.  */

void
push_lang_context (name)
     tree name;
{
  *current_lang_stack++ = current_lang_name;
  if (current_lang_stack >= current_lang_base + current_lang_stacksize)
    {
      current_lang_base =
	(tree *)xrealloc (current_lang_base,
			  sizeof (tree) * (current_lang_stacksize + 10));
      current_lang_stack = current_lang_base + current_lang_stacksize;
      current_lang_stacksize += 10;
    }

  if (name == lang_name_cplusplus)
    {
      strict_prototype = strict_prototypes_lang_cplusplus;
      current_lang_name = name;
    }
  else if (name == lang_name_c)
    {
      strict_prototype = strict_prototypes_lang_c;
      current_lang_name = name;
    }
  else
    error ("language string `\"%s\"' not recognized", IDENTIFIER_POINTER (name));

  if (flag_cadillac)
    cadillac_push_lang (name);
}
  
/* Get out of the current language scope.  */
void
pop_lang_context ()
{
  if (flag_cadillac)
    cadillac_pop_lang ();

  current_lang_name = *--current_lang_stack;
  if (current_lang_name == lang_name_cplusplus)
    strict_prototype = strict_prototypes_lang_cplusplus;
  else if (current_lang_name == lang_name_c)
    strict_prototype = strict_prototypes_lang_c;
}

int
root_lang_context_p ()
{
  return current_lang_stack == current_lang_base;
}

/* Type instantiation routines.  */

/* This function will instantiate the type of the expression given
   in RHS to match the type of LHSTYPE.  If LHSTYPE is NULL_TREE,
   or other errors exist, the TREE_TYPE of RHS will be ERROR_MARK_NODE.

   This function is used in build_modify_expr, actualparameterlist,
   build_c_cast, and compute_conversion_costs.  */
tree
instantiate_type (lhstype, rhs, complain)
     tree lhstype, rhs;
     int complain;
{
  if (TREE_CODE (rhs) == OP_IDENTIFIER)
    return build_instantiated_decl (lhstype, rhs);

  if (TREE_CODE (lhstype) == UNKNOWN_TYPE)
    {
      if (complain)
	error ("not enough type information");
      return error_mark_node;
    }

  if (TREE_TYPE (rhs) != NULL_TREE && ! (type_unknown_p (rhs)))
    return rhs;

  /* This should really only be used when attempting to distinguish
     what sort of a pointer to function we have.  For now, any
     arithmethic operation which is not supported on pointers
     is rejected as an error.  */

  switch (TREE_CODE (rhs))
    {
    case TYPE_EXPR:
    case CONVERT_EXPR:
    case SAVE_EXPR:
    case CONSTRUCTOR:
    case BUFFER_REF:
      assert (0);
      return error_mark_node;

    case INDIRECT_REF:
    case ARRAY_REF:
      TREE_TYPE (rhs) = lhstype;
      lhstype = build_pointer_type (lhstype);
      TREE_OPERAND (rhs, 0)
	= instantiate_type (lhstype, TREE_OPERAND (rhs, 0), complain);
      if (TREE_OPERAND (rhs, 0) == error_mark_node)
	return error_mark_node;

      return rhs;

    case NOP_EXPR:
      rhs = copy_node (TREE_OPERAND (rhs, 0));
      TREE_TYPE (rhs) = unknown_type_node;
      return instantiate_type (lhstype, rhs, complain);

    case COMPONENT_REF:
      {
	tree field = TREE_OPERAND (rhs, 1);
	if (TREE_CODE (field) == TREE_LIST)
	  {
	    tree function = instantiate_type (lhstype, field, complain);
	    if (function == error_mark_node)
	      return error_mark_node;
	    assert (TREE_CODE (function) == FUNCTION_DECL);
	    if (DECL_VIRTUAL_P (function))
	      {
		tree base = TREE_OPERAND (rhs, 0);
		tree base_ptr = build_unary_op (ADDR_EXPR, base, 0);
		if (base_ptr == error_mark_node)
		  return error_mark_node;
		base_ptr = convert_pointer_to (DECL_VCONTEXT (function), base_ptr);
		if (base_ptr == error_mark_node)
		  return error_mark_node;
		return build_vfn_ref (&base_ptr, base, DECL_VINDEX (function));
	      }
	    return function;
	  }

	assert (TREE_CODE (field) == FIELD_DECL);
	assert (!(TREE_CODE (TREE_TYPE (field)) == FUNCTION_TYPE
		  || TREE_CODE (TREE_TYPE (field)) == METHOD_TYPE));

	TREE_TYPE (rhs) = lhstype;
	/* First look for an exact match  */

	while (field && TREE_TYPE (field) != lhstype)
	  field = TREE_CHAIN (field);
	if (field)
	  {
	    TREE_OPERAND (rhs, 1) = field;
	    return rhs;
	  }

	/* No exact match found, look for a compatible function.  */
	field = TREE_OPERAND (rhs, 1);
	while (field && ! comptypes (lhstype, TREE_TYPE (field), 0))
	  field = TREE_CHAIN (field);
	if (field)
	  {
	    TREE_OPERAND (rhs, 1) = field;
	    field = TREE_CHAIN (field);
	    while (field && ! comptypes (lhstype, TREE_TYPE (field), 0))
	      field = TREE_CHAIN (field);
	    if (field)
	      {
		if (complain)
		  error ("ambiguous overload for COMPONENT_REF requested");
		return error_mark_node;
	      }
	  }
	else
	  {
	    if (complain)
	      error ("no appropriate overload exists for COMPONENT_REF");
	    return error_mark_node;
	  }
	return rhs;
      }

    case TREE_LIST:
      {
	tree elem, baselink, name;
	int globals = overloaded_globals_p (rhs);

	/* If there's only one function we know about, return that.  */
	if (globals > 0 && TREE_CHAIN (rhs) == NULL_TREE)
	  return TREE_VALUE (rhs);

	/* First look for an exact match.  Search either overloaded
	   functions or member functions.  May have to undo what
	   `default_conversion' or `datatype' might do to lhstype.  */

	if (TREE_CODE (lhstype) == POINTER_TYPE)
	  if (TREE_CODE (TREE_TYPE (lhstype)) == FUNCTION_TYPE
	      || TREE_CODE (TREE_TYPE (lhstype)) == METHOD_TYPE)
	    lhstype = TREE_TYPE (lhstype);
	  else
	    {
	      if (complain)
		error ("invalid type combination for overload");
	      return error_mark_node;
	    }

	if (TREE_CODE (lhstype) != FUNCTION_TYPE && globals > 0)
	  {
	    if (complain)
	      error ("cannot resolve overloaded function `%s' based on non-function type",
		     IDENTIFIER_POINTER (TREE_PURPOSE (rhs)));
	    return error_mark_node;
	  }

	if (globals > 0)
	  {
	    assert (TREE_CODE (TREE_VALUE (rhs)) == FUNCTION_DECL);
	    elem = rhs;
	    while (elem)
	      if (TREE_TYPE (TREE_VALUE (elem)) != lhstype)
		elem = TREE_CHAIN (elem);
	      else
		return TREE_VALUE (elem);
	    /* No exact match found, look for a compatible function.  */
	    elem = rhs;
	    while (elem && ! comp_target_types (lhstype, TREE_TYPE (TREE_VALUE (elem)), 1))
	      elem = TREE_CHAIN (elem);
	    if (elem)
	      {
		tree save_elem = TREE_VALUE (elem);
		elem = TREE_CHAIN (elem);
		while (elem && ! comp_target_types (lhstype, TREE_TYPE (TREE_VALUE (elem)), 0))
		  elem = TREE_CHAIN (elem);
		if (elem)
		  {
		    if (complain)
		      error ("ambiguous overload for overloaded function requested");
		    return error_mark_node;
		  }
		return save_elem;
	      }
	    if (complain)
	      {
		if (TREE_CHAIN (rhs))
		  error ("no appropriate overload for overloaded function `%s' exists",
			 IDENTIFIER_POINTER (TREE_PURPOSE (rhs)));
		else
		  error ("function `%s' has inappropriate type signature",
			 IDENTIFIER_POINTER (TREE_PURPOSE (rhs)));
	      }
	    return error_mark_node;
	  }

	if (TREE_NONLOCAL (rhs))
	  {
	    /* Got to get it as a baselink.  */
	    rhs = lookup_fnfields (CLASSTYPE_AS_LIST (current_class_type),
				   TREE_PURPOSE (rhs), 0);
	  }
	else
	  {
	    assert (TREE_CHAIN (rhs) == NULL_TREE);
	    if (TREE_CODE (TREE_VALUE (rhs)) == TREE_LIST)
	      rhs = TREE_VALUE (rhs);
	    assert (TREE_CODE (TREE_VALUE (rhs)) == FUNCTION_DECL);
	  }

	for (baselink = rhs; baselink;
	     baselink = next_baselink (baselink))
	  {
	    elem = TREE_VALUE (baselink);
	    while (elem)
	      if (TREE_TYPE (elem) != lhstype)
		elem = TREE_CHAIN (elem);
	      else
		return elem;
	  }

	/* No exact match found, look for a compatible method.  */
	for (baselink = rhs; baselink;
	     baselink = next_baselink (baselink))
	  {
	    elem = TREE_VALUE (baselink);
	    while (elem && ! comp_target_types (lhstype, TREE_TYPE (elem), 1))
	      elem = TREE_CHAIN (elem);
	    if (elem)
	      {
		tree save_elem = elem;
		elem = TREE_CHAIN (elem);
		while (elem && ! comp_target_types (lhstype, TREE_TYPE (elem), 0))
		  elem = TREE_CHAIN (elem);
		if (elem)
		  {
		    if (complain)
		      error ("ambiguous overload for overloaded method requested");
		    return error_mark_node;
		  }
		return save_elem;
	      }
	    name = DECL_ORIGINAL_NAME (TREE_VALUE (rhs));
	    if (TREE_CODE (lhstype) == FUNCTION_TYPE && globals < 0)
	      {
		/* Try to instantiate from non-member functions.  */
		rhs = IDENTIFIER_GLOBAL_VALUE (name);
		if (rhs && TREE_CODE (rhs) == TREE_LIST)
		  {
		    /* This code seems to be missing a `return'.  */
		    abort ();
		    instantiate_type (lhstype, rhs, complain);
		  }
	      }

	    if (complain)
	      error ("no static member functions named `%s'",
		     IDENTIFIER_POINTER (name));
	    return error_mark_node;
	  }
      }

    case CALL_EXPR:
      /* This is too hard for now.  */
      assert (0);
      return error_mark_node;

    case PLUS_EXPR:
    case MINUS_EXPR:
    case COMPOUND_EXPR:
      TREE_OPERAND (rhs, 0) = instantiate_type (lhstype, TREE_OPERAND (rhs, 0), complain);
      if (TREE_OPERAND (rhs, 0) == error_mark_node)
	return error_mark_node;
      TREE_OPERAND (rhs, 1) = instantiate_type (lhstype, TREE_OPERAND (rhs, 1), complain);
      if (TREE_OPERAND (rhs, 1) == error_mark_node)
	return error_mark_node;

      TREE_TYPE (rhs) = lhstype;
      return rhs;

    case MULT_EXPR:
    case TRUNC_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case RDIV_EXPR:
    case TRUNC_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case CEIL_MOD_EXPR:
    case ROUND_MOD_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_CEIL_EXPR:
    case FIX_TRUNC_EXPR:
    case FLOAT_EXPR:
    case NEGATE_EXPR:
    case ABS_EXPR:
    case MAX_EXPR:
    case MIN_EXPR:
    case FFS_EXPR:

    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
    case LSHIFT_EXPR:
    case RSHIFT_EXPR:
    case LROTATE_EXPR:
    case RROTATE_EXPR:

    case PREINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      if (complain)
	error ("illegal operation on uninstantiated type");
      return error_mark_node;

    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
    case LT_EXPR:
    case LE_EXPR:
    case GT_EXPR:
    case GE_EXPR:
    case EQ_EXPR:
    case NE_EXPR:
    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_NOT_EXPR:
      if (complain)
	error ("not enough type information");
      return error_mark_node;

    case COND_EXPR:
      if (type_unknown_p (TREE_OPERAND (rhs, 0)))
	{
	  if (complain)
	    error ("not enough type information");
	  return error_mark_node;
	}
      TREE_OPERAND (rhs, 1) = instantiate_type (lhstype, TREE_OPERAND (rhs, 1), complain);
      if (TREE_OPERAND (rhs, 1) == error_mark_node)
	return error_mark_node;
      TREE_OPERAND (rhs, 2) = instantiate_type (lhstype, TREE_OPERAND (rhs, 2), complain);
      if (TREE_OPERAND (rhs, 2) == error_mark_node)
	return error_mark_node;

      TREE_TYPE (rhs) = lhstype;
      return rhs;

    case MODIFY_EXPR:
      TREE_OPERAND (rhs, 1) = instantiate_type (lhstype, TREE_OPERAND (rhs, 1), complain);
      if (TREE_OPERAND (rhs, 1) == error_mark_node)
	return error_mark_node;

      TREE_TYPE (rhs) = lhstype;
      return rhs;
      
    case ADDR_EXPR:
      if (TREE_CODE (lhstype) != POINTER_TYPE)
	{
	  if (complain)
	    error ("type for resolving address of overloaded function must be pointer type");
	  return error_mark_node;
	}
      TREE_TYPE (rhs) = lhstype;
      lhstype = TREE_TYPE (lhstype);
      TREE_OPERAND (rhs, 0) = instantiate_type (lhstype, TREE_OPERAND (rhs, 0), complain);
      if (TREE_OPERAND (rhs, 0) == error_mark_node)
	return error_mark_node;

      mark_addressable (TREE_OPERAND (rhs, 0));
      return rhs;

    case ENTRY_VALUE_EXPR:
      assert (0);
      return error_mark_node;

    case ERROR_MARK:
      return error_mark_node;

    default:
      assert (0);
      return error_mark_node;
    }
}

/* This routine is called when we finally know the type of expression
   we are looking for.  If the operator encoded by EXP can take an
   argument of type TYPE, return the FUNCTION_DECL for that operator.  */
tree
build_instantiated_decl (type, exp)
     tree type, exp;
{
  tree parmtypes, decl, name;

  assert (TREE_CODE (exp) == OP_IDENTIFIER);
  type = datatype (type);
  if (TREE_CODE (type) != POINTER_TYPE
      || (TREE_CODE (TREE_TYPE (type)) != FUNCTION_TYPE
	  && TREE_CODE (TREE_TYPE (type)) != METHOD_TYPE))
    {
      error ("invalid type used to resolve overloaded function");
      return error_mark_node;
    }


  /* Now we know the type of this function, so overload it.  */
  parmtypes = TYPE_ARG_TYPES (TREE_TYPE (type));
  name = build_operator_fnname (&exp, parmtypes, 0);
  if (name)
    {
      name = build_decl_overload (IDENTIFIER_POINTER (name), parmtypes, 1);
      decl = lookup_name (name);
      if (decl)
	return decl;
      error ("no suitable declaration of `operator %s' for overloading",
	     operator_name_string (name));
      return error_mark_node;
    }
  return error_mark_node;
}

/* Return the name of the virtual function table (as an IDENTIFIER_NODE)
   for the given TYPE.  */
static tree
get_vtable_name (type)
     tree type;
{
  char *buf = (char *)alloca (sizeof (VTABLE_NAME_FORMAT)
			      + TYPE_NAME_LENGTH (type) + 2);
  sprintf (buf, VTABLE_NAME_FORMAT, TYPE_NAME_STRING (type));
  return get_identifier (buf);
}

/* Return the name of the virtual function pointer field
   (as an IDENTIFIER_NODE) for the given TYPE.  Note that
   this may have to look back through base types to find the
   ultimate field name.  (For single inheritance, these could
   all be the same name.  Who knows for multiple inheritance).  */
static tree
get_vfield_name (type)
     tree type;
{
  char *buf;

  while (CLASSTYPE_N_BASECLASSES (type)
	 && TYPE_VIRTUAL_P (CLASSTYPE_BASECLASS (type, 1))
	 && ! CLASSTYPE_VIA_VIRTUAL (type, 1))
    type = CLASSTYPE_BASECLASS (type, 1);

  buf = (char *)alloca (sizeof (VFIELD_NAME_FORMAT)
			+ TYPE_NAME_LENGTH (type) + 2);
  sprintf (buf, VFIELD_NAME_FORMAT, TYPE_NAME_STRING (type));
  return get_identifier (buf);
}

void
print_class_statistics ()
{
#ifdef GATHER_STATISTICS
  fprintf (stderr, "convert_harshness = %d\n", n_convert_harshness);
  fprintf (stderr, "compute_conversion_costs = %d\n", n_compute_conversion_costs);
  fprintf (stderr, "build_method_call = %d (inner = %d)\n",
	   n_build_method_call, n_inner_fields_searched);
  if (n_vtables)
    {
      fprintf (stderr, "vtables = %d; vtable searches = %d\n",
	       n_vtables, n_vtable_searches);
      fprintf (stderr, "vtable entries = %d; vtable elems = %d\n",
	       n_vtable_entries, n_vtable_elems);
    }
#endif
}
