/* Handle initialization things in C++.
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
#include "assert.h"

/* For expand_asm_operands.  */
extern char *input_filename;
extern int lineno;

#define NULL 0

/* In C++, structures with well-defined constructors are initialized by
   those constructors, unasked.  CURRENT_BASE_INIT_LIST
   holds a list of stmts for a BASE_INIT term in the grammar.
   This list has one element for each base class which must be
   initialized.  The list elements are [basename, init], with
   type basetype.  This allows the possibly anachronistic form
   (assuming d : a, b, c) "d (int a) : c(a+5), b (a-4), a (a+3)"
   where each successive term can be handed down the constructor
   line.  Perhaps this was not intended.  */
tree current_base_init_list, current_member_init_list;

void init_init_processing ();
void emit_base_init ();
void check_base_init ();
static void expand_aggr_vbase_init ();
void expand_member_init ();
void expand_aggr_init ();
tree build_virtual_init ();
tree build_vbase_delete ();

static void expand_aggr_init_1 ();
static void expand_recursive_init_1 ();
static void expand_recursive_init ();
tree expand_vec_init ();
tree build_vec_delete ();

static void add_friend (), add_friends ();

int is_aggr_typedef ();
/* Cache _builtin_new and _builtin_delete exprs.  */
static tree BIN, BIVN, BID;

#ifdef SOS
tree get_linktable_name (), get_dtable_name (), get_sos_dtable ();
static tree __sosFindCode, __sosLookup, __sosImport;
static tree build_dynamic_new ();
#endif
static tree minus_one;

extern struct rtx_def *start_sequence (), *get_insns (), *get_last_insn ();
extern struct rtx_def *const0_rtx;

/* Set up local variable for this file.  MUST BE CALLED AFTER
   INIT_DECL_PROCESSING.  */

void init_init_processing ()
{
  BIN = default_conversion (TREE_VALUE (lookup_name (get_identifier ("__builtin_new"))));
  BIVN = default_conversion (TREE_VALUE (lookup_name (get_identifier ("__builtin_vec_new"))));
  BID = default_conversion (TREE_VALUE (lookup_name (get_identifier ("__builtin_delete"))));
  minus_one = build_int_2 (-1, -1);
#ifdef SOS
  if (flag_all_virtual == 2)
    {
      __sosFindCode = default_conversion (lookup_name (get_identifier ("sosFindCode")));
      __sosLookup = default_conversion (lookup_name (get_identifier ("sosLookup")));
      __sosImport = default_conversion (lookup_name (get_identifier ("sosImport")));
    }
#endif
}

/* Perform whatever initialization have yet to be done on the
   base class of the class variable.  These actions are in
   the global variable CURRENT_BASE_INIT_LIST.  Such an
   action could be NULL_TREE, meaning that the user has explicitly
   called the base class constructor with no arguments.

   If there is a need for a call to a constructor, we
   must surround that call with a pushlevel/poplevel pair,
   since we are technically at the PARM level of scope.

   Argument ASSIGNS_THIS_P is nonzero if the current function assigns
   `this' explicitly.  We cannot get this value by checking
   `current_function_assigns_this', since it is set up after this
   function is called.  (although I don't know if its really
   necessary to wait until afterward to do that.)

   Note that emit_base_init does *not* initialize virtual
   base classes.  That is done specially, elsewhere.  */
   
void
emit_base_init (t, immediately)
     tree t;
     int immediately;
{
  extern tree in_charge_identifier;

  tree member, decl;
  tree init_list, basetype;
  int pass, start;
  int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (t);
  tree fields_to_unmark = NULL_TREE;
  tree vfields;

  if (! immediately)
    {
      do_pending_stack_adjust ();
      start_sequence ();
      /* As a matter of principle, `start_sequence' should do this.  */
      emit_note (0, -1);
    }

  /* In this case, we always need IN_CHARGE_NODE, because we have
     to know whether to deallocate or not before exiting.  */
  if (flag_handle_exceptions == 2
      && lookup_name (in_charge_identifier) == NULL_TREE)
    {
      tree in_charge_node = pushdecl (build_decl (VAR_DECL, in_charge_identifier,
						  integer_type_node));
      store_init_value (in_charge_node, build (EQ_EXPR, integer_type_node,
					       current_class_decl,
					       integer_zero_node));
      expand_decl (in_charge_node);
      expand_decl_init (in_charge_node);
    }

  start = ! TYPE_USES_VIRTUAL_BASECLASSES (t);
  for (pass = start; pass < 2; pass++)
    {
      tree vbase_init_list = NULL_TREE;
      for (init_list = current_base_init_list; init_list;
	   init_list = TREE_CHAIN (init_list))
	{
	  tree basename = TREE_PURPOSE (init_list);
	  tree basetype;
	  tree init = TREE_VALUE (init_list);

	  if (basename == NULL_TREE)
	    {
	      /* Initializer for single base class.  Must not
		 use multiple inheritance or this is ambiguous.  */
	      switch (n_baseclasses)
		{
		case 0:
		  error ("type `%s' does not have a base class to initialize",
			 IDENTIFIER_POINTER (current_class_name));
		  return;
		case 1:
		  break;
		default:
		  error ("unnamed initializer ambiguous for type `%s' which uses multiple inheritance", IDENTIFIER_POINTER (current_class_name));
		  return;
		}
	      basetype = CLASSTYPE_BASECLASS (t, 1);
	    }
	  else if (is_aggr_typedef (basename, 1))
	    {
	      basetype = basetype_or_else (TREE_TYPE (TREE_TYPE (basename)), t);
	      if (basetype == NULL_TREE)
		continue;

	      /* Virtual base classes are special cases.  Their initializers
		 are recorded with this constructor, and they are used when
		 this constructor is the top-level constructor called.  */
	      if (! TREE_VIA_VIRTUAL (basetype))
		{
		  /* Otherwise, if it is not an immediate base class, complain.  */
		  for (i = n_baseclasses; i > 0; i--)
		    if (basetype == CLASSTYPE_BASECLASS (t, i))
		      break;
		  if (i == 0)
		    {
		      error ("type `%s' is not an immediate base class of type `%s'",
			     IDENTIFIER_POINTER (basename),
			     IDENTIFIER_POINTER (current_class_name));
		      continue;
		    }
		}
	    }
	  else
	    continue;

	  /* The base initialization list goes up to the first
	     base class which can actually use it.  */

	  if (CLASSTYPE_MARKED6 (basetype))
	    {
	      error ("class `%s' initializer already specified",
		     IDENTIFIER_POINTER (basename));
	      continue;
	    }

	  if (pass == start)
	    {
	      char *msgp = (! TYPE_HAS_CONSTRUCTOR (basetype))
		? "cannot pass initialization up to class `%s'" : 0;

	      while (! TYPE_HAS_CONSTRUCTOR (basetype)
		     && CLASSTYPE_N_BASECLASSES (basetype) == 1)
		basetype = CLASSTYPE_BASECLASS (basetype, 1);

	      if (basetype)
		{
		  if (msgp)
		    if (pedantic)
		      {
			error_with_aggr_type (basetype, msgp);
			continue;
		      }
		    else if (! TYPE_HAS_CONSTRUCTOR (basetype))
		      {
			if (CLASSTYPE_N_BASECLASSES (basetype) == 0)
			  error_with_aggr_type (basetype, msgp);
			else
			  sorry ("passing initializations up multiple inheritance lattice");
			continue;
		      }
		}
	      else
		{
		  error ("no constructor found for initialization of `%s'",
			 IDENTIFIER_POINTER (basename));
		  continue;
		}

	      if (TREE_VIA_VIRTUAL (basetype))
		{
		  CLASSTYPE_MARKED6 (basetype) = 1;
		  vbase_init_list = tree_cons (init, basetype, vbase_init_list);
		}
	      if (pass == 0)
		continue;
	    }
	  else if (TREE_VIA_VIRTUAL (basetype))
	    continue;

	  CLASSTYPE_MARKED6 (basetype) = 1;
	  member = convert_pointer_to (basetype, current_class_decl);
	  expand_aggr_init_1 (t, 0,
			      build_indirect_ref (member, 0), init,
			      ! DECL_OFFSET (TYPE_NAME (basetype)),
			      LOOKUP_PROTECTED_OK|LOOKUP_COMPLAIN);
	  if (flag_handle_exceptions == 2 && TYPE_NEEDS_DESTRUCTOR (basetype))
	    {
	      cplus_expand_start_try (1);
	      push_exception_cleanup (member);
	    }
	}

      if (pass == 0)
	{
	  tree first_arg = TREE_CHAIN (DECL_ARGUMENTS (current_function_decl));
	  tree vbases;

	  if (DECL_COMPILER_GENERATED_P (current_function_decl)
	      && TREE_CHAIN (first_arg) != NULL_TREE)
	    {
	      /* If there are virtual baseclasses without initialization
		 specified, and this is a default X(X&) construcotr,
		 build the initialization list so that each virtual baseclass
		 of the new object is initialized from the virtual baseclass
		 of the incoming arg.  */
	      tree init_arg = build_unary_op (ADDR_EXPR, TREE_CHAIN (first_arg), 0);
	      for (vbases = CLASSTYPE_VBASECLASSES (t);
		   vbases; vbases = TREE_CHAIN (vbases))
		{
		  basetype = TREE_TYPE (vbases);
		  if (CLASSTYPE_MARKED6 (basetype) == 0)
		    {
		      member = convert_pointer_to (basetype, init_arg);
		      if (member == init_arg)
			member = TREE_CHAIN (first_arg);
		      else
			TREE_TYPE (member) = build_reference_type (basetype);
		      vbase_init_list = tree_cons (convert_from_reference (member),
						   basetype, vbase_init_list);
		      CLASSTYPE_MARKED6 (basetype) = 1;
		    }
		}
	    }
	  expand_start_cond (first_arg, 0);
	  expand_aggr_vbase_init (t, C_C_D, current_class_decl,
				  vbase_init_list);
	  expand_expr_stmt (build_vbase_vtables_init (t, t, C_C_D,
						      current_class_decl, 1));
#ifdef sparc
	  expand_asm_operands (build_string (32, "! end of vtable initialization"), 0, 0, 0, 0, input_filename, lineno);
#endif
	  expand_end_cond ();
	}
    }
  current_base_init_list = NULL_TREE;

  /* Now, perform default initialization of all base classes which
     have not yet been initialized, and unmark baseclasses which
     have been initialized.  */
  for (i = 1; i <= n_baseclasses; i++)
    {
      tree base = current_class_decl;

      basetype = CLASSTYPE_BASECLASS (t, i);
      if (TYPE_NEEDS_CONSTRUCTING (basetype))
	{
	  if (! TREE_VIA_VIRTUAL (basetype)
	      && ! CLASSTYPE_MARKED6 (basetype))
	    {
	      tree ref;

	      if (CLASSTYPE_OFFSET (basetype) == integer_zero_node)
		base = build1 (NOP_EXPR, TYPE_POINTER_TO (basetype), current_class_decl);
	      else
		base = build (PLUS_EXPR, TYPE_POINTER_TO (basetype), current_class_decl, CLASSTYPE_OFFSET (basetype));

	      ref = build_indirect_ref (base, 0);
	      expand_aggr_init_1 (t, 0, ref, NULL_TREE,
				  ! DECL_OFFSET (TYPE_NAME (basetype)),
				  LOOKUP_PROTECTED_OK|LOOKUP_COMPLAIN);
	      if (flag_handle_exceptions == 2 && TYPE_NEEDS_DESTRUCTOR (basetype))
		{
		  cplus_expand_start_try (1);
		  push_exception_cleanup (base);
		}
	    }
	}
      CLASSTYPE_MARKED6 (basetype) = 0;
    }

  /* Initialize all the virtual function table fields that
     do not come from virtual base classes.  */
  vfields = CLASSTYPE_VFIELDS (t);
  while (vfields)
    {
      /* If the vtable installed by the constructor was not
	 the right one, fix that here.  */
      if (TREE_ADDRESSABLE (vfields)
	  && (TYPE_HAS_CONSTRUCTOR (TREE_TYPE (vfields))
	      /* BASE_INIT_LIST has already initialized these guys.  */
	      || ! value_member (TREE_VALUE (vfields), CLASSTYPE_ASSOC (t))))
	{
	  tree basetype = TYPE_MAIN_VARIANT (TREE_TYPE (vfields));
	  tree assoc = assoc_value (basetype, t);
	  if (ASSOC_VTABLE (assoc) != CLASS_ASSOC_VTABLE (basetype))
	    {
	      tree ptr = convert_pointer_to (basetype, current_class_decl);
	      expand_expr_stmt (build_virtual_init (t, basetype, ptr));
	    }
	}
      vfields = TREE_CHAIN (vfields);
    }

  if (CLASSTYPE_NEEDS_VIRTUAL_REINIT (t)
#ifdef SOS
       || TYPE_DYNAMIC (t)
#endif
       )
    expand_expr_stmt (build_virtual_init (t, t, current_class_decl));

  /* Members we through expand_member_init.  We initialize all the members
     needing initialization that did not get it so far.  */
  for (; current_member_init_list;
       current_member_init_list = TREE_CHAIN (current_member_init_list))
    {
      tree name = TREE_PURPOSE (current_member_init_list);
      tree init = TREE_VALUE (current_member_init_list);
      tree field = (TREE_CODE (name) == COMPONENT_REF
		    ? TREE_OPERAND (name, 1) : IDENTIFIER_CLASS_VALUE (name));
      tree type;
      
      /* If one member shadows another, get the outermost one.  */
      if (TREE_CODE (field) == TREE_LIST)
	{
	  field = TREE_VALUE (field);
	  if (decl_type_context (field) != current_class_type)
	    error ("field `%s' not in immediate context");
	}

      type = TREE_TYPE (field);

      if (TREE_STATIC (field))
	{
	  error_with_aggr_type (DECL_FIELD_CONTEXT (field),
				"field `%s::%s' is static; only point of initialization is its declaration", IDENTIFIER_POINTER (name));
	  continue;
	}

      if (DECL_NAME (field))
	{
	  if (TREE_HAS_CONSTRUCTOR (field))
	    error ("multiple initializations given for member `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (field)));
	}

      /* Mark this node as having been initialized.  */
      TREE_HAS_CONSTRUCTOR (field) = 1;
      if (DECL_FIELD_CONTEXT (field) != t)
	fields_to_unmark = tree_cons (NULL_TREE, field, fields_to_unmark);

      if (TREE_CODE (name) == COMPONENT_REF)
	{
	  /* Initialization of anonymous union.  */
	  expand_assignment (name, init, 0, 0);
	  continue;
	}
      decl = build_component_ref (C_C_D, name, 0, 1);

      if (TYPE_NEEDS_CONSTRUCTING (type))
	{
	  if (TREE_CODE (type) == ARRAY_TYPE
	      && TREE_CHAIN (init) == NULL_TREE
	      && TREE_CODE (TREE_TYPE (TREE_VALUE (init))) == ARRAY_TYPE)
	    {
	      /* Initialization of one array from another.  */
	      expand_vec_init (TREE_OPERAND (decl, 1), decl,
			       array_type_nelts (type), TREE_VALUE (init), 1);
	    }
	  else
	    expand_aggr_init (decl, init, 0);
	}
      else
	{
	  if (init == NULL_TREE)
	    {
	      error ("types without constructors must have complete initializers");
	      init = error_mark_node;
	    }
	  else if (TREE_CHAIN (init))
	    {
	      warning ("initializer list treated as compound expression");
	      init = build_compound_expr (init);
	    }
	  else
	    init = TREE_VALUE (init);

	  expand_expr_stmt (build_modify_expr (decl, INIT_EXPR, init));
	}
      if (flag_handle_exceptions == 2 && TYPE_NEEDS_DESTRUCTOR (type))
	{
	  cplus_expand_start_try (1);
	  push_exception_cleanup (build_unary_op (ADDR_EXPR, decl, 0));
	}
    }

  for (member = TYPE_FIELDS (t); member; member = TREE_CHAIN (member))
    {
      /* All we care about is this unique member.  It contains
	 all the information we need to know, and that right early.  */
      tree type = TREE_TYPE (member);
      tree init = TREE_HAS_CONSTRUCTOR (member)
	? error_mark_node : DECL_INITIAL (member);

      /* Unmark this field.  If it is from an anonymous union,
         then unmark the field recursively.  */
      TREE_HAS_CONSTRUCTOR (member) = 0;
      if (TREE_ANON_UNION_ELEM (member))
	emit_base_init (TREE_TYPE (member), 1);

      /* Member had explicit initializer.  */
      if (init == error_mark_node)
	continue;

      if (TREE_CODE (member) != FIELD_DECL)
	continue;

      if (type == error_mark_node)
	continue;

      if (TYPE_NEEDS_CONSTRUCTING (type))
	{
	  if (init)
	    init = build_tree_list (NULL_TREE, init);
	  decl = build_component_ref (C_C_D, DECL_NAME (member), 0, 0);
	  expand_aggr_init (decl, init, 0);
	}
      else
	{
	  if (init)
	    {
	      decl = build_component_ref (C_C_D, DECL_NAME (member), 0, 0);
	      expand_expr_stmt (build_modify_expr (decl, INIT_EXPR, init));
	    }
	  else if (TREE_CODE (TREE_TYPE (member)) == REFERENCE_TYPE)
	    warning ("uninitialized reference member `%s'",
		     IDENTIFIER_POINTER (DECL_NAME (member)));
	}
      if (flag_handle_exceptions == 2 && TYPE_NEEDS_DESTRUCTOR (type))
	{
	  cplus_expand_start_try (1);
	  push_exception_cleanup (build_unary_op (ADDR_EXPR, decl, 0));
	}
    }
  /* Unmark fields which are initialized for the base class.  */
  while (fields_to_unmark)
    {
      TREE_HAS_CONSTRUCTOR (TREE_VALUE (fields_to_unmark)) = 0;
      fields_to_unmark = TREE_CHAIN (fields_to_unmark);
    }

  /* It is possible for the initializers to need cleanups.
     Expand those cleanups now that all the initialization
     has been done.  */
  expand_cleanups_to (NULL_TREE);

  if (! immediately)
    {
      extern struct rtx_def *base_init_insns;

      do_pending_stack_adjust ();
      assert (base_init_insns == 0);
      base_init_insns = get_insns ();
      end_sequence ();
    }

  /* All the implicit try blocks we built up will be zapped
     when we come to a real binding contour boundary.  */
}

/* Check that all fields are properly initialized after
   an assignment to `this'.  */
void
check_base_init (t)
     tree t;
{
  tree member;
  for (member = TYPE_FIELDS (t); member; member = TREE_CHAIN (member))
    {
      if (DECL_NAME (member) && TREE_USED (member))
	error ("field `%s' used before initialized (after assignment to `this')",
	       IDENTIFIER_POINTER (DECL_NAME (member)));
    }
}

/* This code sets up the virtual function tables appropriate for
   the pointer DECL.  It is a one-ply initialization.

   TYPE is the exact type that DECL is supposed to be.  In
   muliple inheritance, this might mean "C's A" if C : A, B.  */
tree
build_virtual_init (for_type, type, decl)
     tree for_type, type;
     tree decl;
{
  tree vtbl, vtbl_ptr;
  tree vtype;

#ifdef SOS
  if (TYPE_DYNAMIC (type))
    vtbl = build1 (NOP_EXPR, ptr_type_node, lookup_name (get_identifier (AUTO_VTABLE_NAME)));
  else
#endif
    {
#if 1
      vtbl = ASSOC_VTABLE (assoc_value (DECL_FIELD_CONTEXT (CLASSTYPE_VFIELD (type)),
					for_type));
#else
      assert (for_type == type);
      vtbl = CLASS_ASSOC_VTABLE (for_type);
#endif /* 1 */
      TREE_USED (vtbl) = 1;
      vtbl = build1 (ADDR_EXPR, TYPE_POINTER_TO (TREE_TYPE (vtbl)), vtbl);
    }
  vtype = DECL_CONTEXT (CLASSTYPE_VFIELD (type));
  decl = convert_pointer_to (vtype, decl);
  vtbl_ptr = build_vfield_ref (build_indirect_ref (decl, 0), vtype);

  /* Have to convert VTBL since array sizes may be different.  */
  return build_modify_expr (vtbl_ptr, NOP_EXPR,
			    convert (TREE_TYPE (vtbl_ptr), vtbl));
}

/* Subroutine of `expand_aggr_vbase_init'.
   BASETYPE is type that is being initialized.  INIT_LIST
   is the list of initializers for the virtual baseclass.  */
static void
expand_aggr_vbase_init_1 (basetype, exp, addr, init_list)
     tree basetype, init_list;
{
  tree init = value_member (basetype, init_list);
  tree ref = build_indirect_ref (addr, 0);
  if (init)
    init = TREE_PURPOSE (init);
  /* Call constructors, but don't set up vtables.  */
  expand_aggr_init_1 (basetype, exp, ref, init, 0,
		      LOOKUP_PROTECTED_OK|LOOKUP_COMPLAIN|LOOKUP_SPECULATIVELY);
  CLASSTYPE_MARKED6 (basetype) = 0;
}

/* Initialize this object's virtual base class pointers.  This must be
   done only at the top-level of the object being constructed.

   INIT_LIST is list of initialization for constructor to perform.  */
static void
expand_aggr_vbase_init (type, exp, addr, init_list)
     tree type;
     tree exp;
     tree addr;
     tree init_list;
{
  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
    {
      tree result = init_vbase_pointers (type, addr);
      tree basetype, vbases;

      if (result)
	expand_expr_stmt (build_compound_expr (result));

      /* Mark everything as having an initializer
	 (either explicit or default).  */
      for (vbases = CLASSTYPE_VBASECLASSES (type);
	   vbases; vbases = TREE_CHAIN (vbases))
	CLASSTYPE_MARKED6 (TREE_TYPE (vbases)) = 1;

      /* First, initialize baseclasses which could be baseclasses
	 for other virtual baseclasses.  */
      for (vbases = CLASSTYPE_VBASECLASSES (type);
	   vbases; vbases = TREE_CHAIN (vbases))
	/* Don't initialize twice.  */
	if (CLASSTYPE_MARKED6 (TREE_TYPE (vbases)))
	  {
	    tree assoc = purpose_member (TREE_TYPE (vbases), result);
	    assert (assoc != NULL_TREE);
	    expand_aggr_vbase_init_1 (TREE_PURPOSE (assoc), exp,
				      TREE_OPERAND (TREE_VALUE (assoc), 0),
				      init_list);
	  }

      /* Now initialize the baseclasses which don't have virtual baseclasses.  */
      for (; result; result = TREE_CHAIN (result))
	/* Don't initialize twice.  */
	if (CLASSTYPE_MARKED6 (TREE_PURPOSE (result)))
	  {
	    abort ();
	    expand_aggr_vbase_init_1 (TREE_PURPOSE (result), exp,
				      TREE_OPERAND (TREE_VALUE (result), 0),
				      init_list);
	  }

#ifdef sparc
	  expand_asm_operands (build_string (30, "! end of vbase initialization"), 0, 0, 0, 0, input_filename, lineno);
#endif
    }
}

/* Function to give error message if member initialization specification
   is erroneous.  FIELD is the member we decided to initialize.
   TYPE is the type for which the initialization is being performed.
   FIELD must be a member of TYPE, or the base type from which FIELD
   comes must not need a constructor.
   
   MEMBER_NAME is the name of the member.  */

static int
member_init_ok_or_else (field, type, member_name)
     tree field;
     tree type;
     char *member_name;
{
  if (field == error_mark_node) return 0;
  if (field == NULL_TREE)
    {
      error_with_aggr_type (type, "class `%s' does not have any field named `%s'",
			    member_name);
      return 0;
    }
  if (DECL_CONTEXT (field) != type
      && TYPE_NEEDS_CONSTRUCTOR (DECL_CONTEXT (field)))
    {
      error ("member `%s' comes from base class needing constructor", member_name);
      return 0;
    }
  return 1;
}

/* If NAME is a viable field name for the aggregate DECL,
   and PARMS is a viable parameter list, then expand an _EXPR
   which describes this initialization.

   Note that we do not need to chase through the class's base classes
   to look for NAME, because if it's in that list, it will be handled
   by the constructor for that base class.

   We do not yet have a fixed-point finder to instantiate types
   being fed to overloaded constructors.  If there is a unique
   constructor, then argument types can be got from that one.

   If INIT is non-NULL, then it the initialization should
   be placed in `current_base_init_list', where it will be processed
   by `emit_base_init'.  */
void
expand_member_init (exp, name, init)
     tree exp, name, init;
{
  extern tree ptr_type_node;	/* should be in tree.h */

  tree basetype = NULL_TREE, field;
  tree parm;
  tree rval, type;
  tree actual_name;

  if (exp == NULL_TREE)
    return;			/* complain about this later */

  type = TYPE_MAIN_VARIANT (TREE_TYPE (exp));

  if (name == NULL_TREE && IS_AGGR_TYPE (type))
    switch (CLASSTYPE_N_BASECLASSES (type))
      {
      case 0:
	error ("base class initializer specified, but no base class to initialize");
	return;
      case 1:
	basetype = CLASSTYPE_BASECLASS (type, 1);
	break;
      default:
	error ("initializer for unnamed base class ambiguous");
	error_with_aggr_type (type, "(type `%s' uses multiple inheritance)");
	return;
      }

  if (init)
    {
      /* The grammar should not allow fields which have names
	 that are TYPENAMEs.  Therefore, if the field has
	 a non-NULL TREE_TYPE, we may assume that this is an
	 attempt to initialize a base class member of the current
	 type.  Otherwise, it is an attempt to initialize a
	 member field.  */

      if (init == void_type_node)
	init = NULL_TREE;

      if (name == NULL_TREE || TREE_TYPE (name))
	{
	  tree base_init;

	  if (name == NULL_TREE)
	    if (basetype)
	      name = DECL_NAME (TYPE_NAME (basetype));
	    else
	      {
		error ("no base class to initialize");
		return;
	      }
	  else
	    {
	      basetype = TREE_TYPE (TREE_TYPE (name));
	      if (basetype != type
		  && ! value_member (basetype, TYPE_BASETYPES (type))
		  && ! value_member (basetype, CLASSTYPE_VBASECLASSES (type)))
		{
		  if (IDENTIFIER_CLASS_VALUE (name))
		    goto try_member;
		  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
		    error ("type `%s' is not an immediate or virtual basetype for `%s'",
			   IDENTIFIER_POINTER (name),
			   TYPE_NAME_STRING (type));
		  else
		    error ("type `%s' is not an immediate basetype for `%s'",
			   IDENTIFIER_POINTER (name),
			   TYPE_NAME_STRING (type));
		  return;
		}
	    }

	  if (purpose_member (name, current_base_init_list))
	    {
	      error ("base class `%s' already initialized",
		     IDENTIFIER_POINTER (name));
	      return;
	    }

	  base_init = build_tree_list (name, init);
	  TREE_TYPE (base_init) = basetype;
	  current_base_init_list = chainon (current_base_init_list, base_init);
	}
      else
	{
	  tree member_init;

	try_member:
	  field = lookup_field (type, name, 1);

	  if (! member_init_ok_or_else (field, type, IDENTIFIER_POINTER (name)))
	    return;

	  if (purpose_member (name, current_member_init_list))
	    {
	      error ("field `%s' already initialized", DECL_NAME (name));
	      return;
	    }

	  member_init = build_tree_list (name, init);
	  TREE_TYPE (member_init) = TREE_TYPE (field);
	  current_member_init_list = chainon (current_member_init_list, member_init);
	}
      return;
    }
  else if (name == NULL_TREE)
    {
      compiler_error ("expand_member_init: name == NULL_TREE");
      return;
    }

  basetype = type;
  field = lookup_field (basetype, name, 0);

  if (! member_init_ok_or_else (field, basetype, IDENTIFIER_POINTER (name)))
    return;

  /* now see if there is a constructor for this type
     which will take these args. */

  if (TYPE_HAS_CONSTRUCTOR (TREE_TYPE (field)))
    {
      tree parmtypes, fndecl;

      if (TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == PARM_DECL)
	{
	  /* just know that we've seen something for this node */
	  DECL_INITIAL (exp) = error_mark_node;
	  TREE_USED (exp) = 1;
	}
      type = TYPE_MAIN_VARIANT (TREE_TYPE (field));
      actual_name = DECL_NAME (TYPE_NAME (type));
      parm = build_component_ref (exp, name, 0, 0);

      /* Now get to the constructor.  */
      field = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), 0);
      /* Get past destructor, if any.  */
      if (TYPE_HAS_DESTRUCTOR (type))
	field = TREE_CHAIN (field);

      /* If the field is unique, we can use the parameter
	 types to guide possible type instantiation.  */
      if (TREE_CHAIN (field) == NULL_TREE)
	{
	  fndecl = TREE_TYPE (field);
	  parmtypes = TREE_CHAIN (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
	}
      else
	{
	  parmtypes = NULL_TREE;
	  fndecl = NULL_TREE;
	}

      init = actualparameterlist (parm, parmtypes, NULL_TREE, fndecl, LOOKUP_NORMAL);
      if (init == NULL_TREE || TREE_TYPE (init) != error_mark_node)
	rval = build_method_call (NULL_TREE, actual_name, init, NULL_TREE, LOOKUP_NORMAL);
      else
	return;

      if (rval != error_mark_node)
	{
	  /* Now, fill in the first parm with our guy */
	  TREE_VALUE (TREE_OPERAND (rval, 1))
	    = build_unary_op (ADDR_EXPR, parm, 0);
	  TREE_TYPE (rval) = ptr_type_node;
	  TREE_VOLATILE (rval) = 1;
	}
    }
  else if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (field)))
    {
      parm = build_component_ref (exp, name, 0, 0);
      expand_aggr_init (parm, NULL_TREE, 0);
      rval = error_mark_node;
    }

  /* Now initialize the member.  It does not have to
     be of aggregate type to receive initialization.  */
  if (rval != error_mark_node)
    expand_expr_stmt (rval);
}

/* This is like `expand_member_init', only it stores one aggregate
   value into another.

   INIT comes in two flavors: it is either a value which
   is to be stored in EXP, or it is a parameter list
   to go to a constructor, which will operate on EXP.
   If `init' is a CONSTRUCTOR, then we emit a warning message,
   explaining that such initializaitions are illegal.

   ALIAS_THIS is nonzero iff we are initializing something which is
   essentially an alias for C_C_D.  In this case, the base constructor
   may move it on us, and we must keep track of such deviations.

   If INIT resolves to a CALL_EXPR which happens to return
   something of the type we are looking for, then we know
   that we can safely use that call to perform the
   initialization.

   The virtual function table pointer cannot be set up here, because
   we do not really know its type.

   Virtual baseclass pointers are also set up here.

   This never calls operator=().

   When initializing, nothing is CONST.  */

void
expand_aggr_init (exp, init, alias_this)
     tree exp, init;
     int alias_this;
{
  tree type = TREE_TYPE (exp);
  tree addr;
  int was_const = TREE_READONLY (exp);

  if (init == error_mark_node)
    return;

  TREE_READONLY (exp) = 0;

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      /* Must arrange to initialize each element of EXP
	 from elements of INIT.  */
      int was_const_elts = TREE_READONLY (TREE_TYPE (type));
      tree itype = init ? TREE_TYPE (init) : NULL_TREE;
      if (was_const_elts)
	{
	  tree atype = build_cplus_array_type (TYPE_MAIN_VARIANT (TREE_TYPE (type)),
					       TYPE_DOMAIN (type));
	  if (TREE_TYPE (exp) == TREE_TYPE (init))
	    TREE_TYPE (init) = atype;
	  TREE_TYPE (exp) = atype;
	}
      expand_vec_init (exp, exp, array_type_nelts (type), init,
		       init && TREE_TYPE (init) == TREE_TYPE (exp));
      TREE_READONLY (exp) = was_const;
      TREE_TYPE (exp) = type;
      if (init) TREE_TYPE (init) = itype;
      return;
    }

  if (TREE_CODE (exp) == VAR_DECL || TREE_CODE (exp) == PARM_DECL)
    /* just know that we've seen something for this node */
    TREE_USED (exp) = 1;

  /* If initializing from a GNU C CONSTRUCTOR, consider the elts in the
     constructor as parameters to an implicit GNU C++ constructor.  */
  if (init && TREE_CODE (init) == CONSTRUCTOR
      && TYPE_HAS_CONSTRUCTOR (type)
      && TREE_TYPE (init) == type)
    init = CONSTRUCTOR_ELTS (init);
  expand_aggr_init_1 (type, exp, exp, init, alias_this, LOOKUP_NORMAL);
  TREE_READONLY (exp) = was_const;
}

/* This function is responsible for initializing EXP with INIT
   (if any).

   FOR_TYPE is the type for who we are performing the initialization.
   For example, if W is a virtual base class of A and B, and C : A, B
   if we are initializing B, then W must contain B's W vtable, whereas
   were we initializing C, W must contain C's W vtable.

   TRUE_EXP is nonzero if it is the true expression being initialized.
   In this case, it may be EXP, or may just contain EXP.  The reason we
   need this is because if EXP is a base element of TRUE_EXP, we
   don't necessarily know by looking at EXP where its virtual
   baseclass fields should really be pointing.  But we do know
   from TRUE_EXP.  In constructors, we don't know anything about
   the value being initialized.

   ALIAS_THIS serves the same purpose it serves for expand_aggr_init.

   FLAGS is just passes to `build_method_call'.  See that function for
   its description.  */

static void
expand_aggr_init_1 (for_type, true_exp, exp, init, alias_this, flags)
     tree for_type;
     tree true_exp, exp;
     tree init;
     int alias_this;
     int flags;
{
  tree type = TREE_TYPE (exp);
  tree init_type = NULL_TREE;
  tree rval;

  assert (init != error_mark_node && type != error_mark_node);

  /* Use a function returning the desired type to initialize EXP for us.
     If the function is a constructor, and its first argument is
     NULL_TREE, know that it was meant for us--just slide exp on
     in and expand the constructor.  Constructors now come
     as NEW_EXPRs.  */
  if (init)
    {
      tree init_list = NULL_TREE;

      if (TREE_CODE (init) == TREE_LIST)
	{
	  init_list = init;
	  if (TREE_CHAIN (init) == NULL_TREE)
	    init = TREE_VALUE (init);
	}

      init_type = TREE_TYPE (init);

      if (TREE_CODE (init) != TREE_LIST)
	{
	  if (TREE_CODE (init_type) == ERROR_MARK)
	    return;

#if 0
	  /* These lines are found troublesome 5/11/89.  */
	  if (TREE_CODE (init_type) == REFERENCE_TYPE)
	    init_type = TREE_TYPE (init_type);
#endif

	  /* This happens when we use C++'s functional cast notation
	     to act as the initializer for something not of that same
	     type.  In that case, we need to create the initializer
	     separately from the object being initialized.  */
	  if (TREE_CODE (init) == NEW_EXPR && init_type != type)
	    {
	      init = TREE_OPERAND (init, 1);
	      init = build (CALL_EXPR, init_type,
			    TREE_OPERAND (init, 0), TREE_OPERAND (init, 1), 0);
	      TREE_VOLATILE (init) = 1;
#if 0
	      TREE_RAISES (init) = ??
#endif
	      if (init_list)
		TREE_VALUE (init_list) = init;
	    }

	  if (init_type == type && TREE_CODE (init) == CALL_EXPR
#if 0
	      /* It is legal to directly initialize from a CALL_EXPR
		 without going through X(X&), apparently.  */
	      && ! TYPE_GETS_INIT_REF (type)
#endif
	      )
	    {
	      /* A CALL_EXPR is a legitmate form of initialization, so
		 we should not print this warning message.  */
#if 0
	      /* Should have gone away due to 5/11/89 change.  */
	      if (TREE_CODE (TREE_TYPE (init)) == REFERENCE_TYPE)
		init = convert_from_reference (init);
#endif
#if 0
	      if (TYPE_GETS_ASSIGNMENT (type))
		warning ("bitwise copy: `%s' defines operator=()",
			 TYPE_NAME_STRING (type));
#endif
	      expand_assignment (exp, init, 0, 0);
	      if (exp == DECL_RESULT (current_function_decl))
		{
		  /* Failing this assertion means that the return value
		     from receives multiple initializations.  */
		  assert (DECL_INITIAL (exp) == NULL_TREE || DECL_INITIAL (exp) == error_mark_node);
		  DECL_INITIAL (exp) = init;
		}
	      return;
	    }
	  else if (init_type == type
		   && TREE_CODE (init) == COND_EXPR)
	    {
	      /* Push value to be initialized into the cond, where possible.
	         Avoid spurious warning messages when initializing the
		 result of this function.  */
	      TREE_OPERAND (init, 1)
		= build_modify_expr (exp, INIT_EXPR, TREE_OPERAND (init, 1));
	      if (exp == DECL_RESULT (current_function_decl))
		DECL_INITIAL (exp) = NULL_TREE;
	      TREE_OPERAND (init, 2)
		= build_modify_expr (exp, INIT_EXPR, TREE_OPERAND (init, 2));
	      if (exp == DECL_RESULT (current_function_decl))
		DECL_INITIAL (exp) = init;
	      expand_expr (init, const0_rtx, VOIDmode, 0);
	      return;
	    }
	}

      /* We did not know what we were initializing before.  Now we do.  */
      if (TREE_CODE (init) == NEW_EXPR)
	{
	  tree tmp = TREE_OPERAND (TREE_OPERAND (init, 1), 1);

	  assert (tmp != NULL_TREE);

	  if (TREE_CODE (TREE_VALUE (tmp)) == NOP_EXPR
	      && TREE_OPERAND (TREE_VALUE (tmp), 0) == integer_zero_node)
	    {
	      /* In order for this to work for RESULT_DECLs, if their
		 type has a constructor, then they must be BLKmode
		 so that they will be meaningfully addressable.  */
	      tree arg = build_unary_op (ADDR_EXPR, exp, 0);
	      init = TREE_OPERAND (init, 1);
	      init = build (CALL_EXPR, build_pointer_type (TREE_TYPE (init)),
			    TREE_OPERAND (init, 0), TREE_OPERAND (init, 1), 0);
	      TREE_VOLATILE (init) = 1;
#if 0
	      TREE_RAISES (init) = ??
#endif
	      TREE_VALUE (TREE_OPERAND (init, 1))
		= convert_pointer_to (TREE_TYPE (TREE_TYPE (TREE_VALUE (tmp))), arg);

	      if (alias_this)
		{
		  expand_assignment (current_function_decl, init, 0, 0);
		  return;
		}
	      if (exp == DECL_RESULT (current_function_decl))
		{
		  if (DECL_INITIAL (DECL_RESULT (current_function_decl)))
		    fatal ("return value from function receives multiple initializations");
		  DECL_INITIAL (exp) = init;
		}
	      expand_expr_stmt (init);
	      return;
	    }
	}

      /* Handle this case: when calling a constructor: xyzzy foo(bar);
	 which really means:  xyzzy foo = bar; Ugh!

	 We can also be called with an initializer for an object
	 which has virtual functions, but no constructors.  In that
	 case, we perform the assignment first, then initialize
	 the virtual function table pointer fields.  */

      if (! TYPE_NEEDS_CONSTRUCTING (type))
	{
	  if (init_list && TREE_CHAIN (init_list))
	    {
	      warning ("initializer list being treated as compound expression");
	      init = convert (TREE_TYPE (exp), build_compound_expr (init_list));
	      if (init == error_mark_node)
		return;
	    }
	  if (TREE_CODE (exp) == VAR_DECL
	      && TREE_CODE (init) == CONSTRUCTOR
	      && TREE_HAS_CONSTRUCTOR (init))
	    store_init_value (exp, init);
	  else
	    expand_assignment (exp, init, 0, 0);

	  if (TYPE_VIRTUAL_P (type))
	    expand_recursive_init (for_type, true_exp, exp, init, CLASSTYPE_BASE_INIT_LIST (type), alias_this);
	  return;
	}

      /* See whether we can go through a type conversion operator.
	 This wins over going through a constructor because we may be
	 able to avoid an X(X&) constructor.  */
      if (TREE_CODE (init) != TREE_LIST)
	{
	  tree ttype = TREE_CODE (init_type) == REFERENCE_TYPE
	    ? TREE_TYPE (init_type) : init_type;

	  if (ttype != type && IS_AGGR_TYPE (ttype))
	    {
	      tree rval = build_type_conversion (CONVERT_EXPR, type, init, 0);

	      if (rval)
		{
		  expand_assignment (exp, rval, 0, 0);
		  return;
		}
	    }
	}
    }

  if (TYPE_HAS_CONSTRUCTOR (type))
    {
      /* It fails because there may not be a constructor which takes
	 its own type as the first (or only parameter), but which does
	 take other types via a conversion.  So, if the thing initializing
	 the expression is a unit element of type X, first try X(X&),
	 followed by initialization by X.  If neither of these work
	 out, then look hard.  */
      tree parms = (init == NULL_TREE || TREE_CODE (init) == TREE_LIST)
	? init : build_tree_list (NULL_TREE, init);
      int xxref_init_possible;

      if (parms) init = TREE_VALUE (parms);

      if (TYPE_HAS_INIT_REF (type)
	  || init == NULL_TREE
	  || TREE_CHAIN (parms) != NULL_TREE)
	xxref_init_possible = 0;
      else
	{
	  xxref_init_possible = LOOKUP_SPECULATIVELY;
	  flags &= ~LOOKUP_COMPLAIN;
	}

      if (TYPE_USES_VIRTUAL_BASECLASSES (type))
	{
	  assert (TYPE_USES_VIRTUAL_BASECLASSES (for_type));
	  if (true_exp == exp)
	    parms = tree_cons (NULL_TREE, integer_one_node, parms);
	  else
	    parms = tree_cons (NULL_TREE, integer_zero_node, parms);
	  flags |= LOOKUP_HAS_IN_CHARGE;
	}
      rval = build_method_call (exp, DECL_NAME (TYPE_NAME (type)), parms,
				CLASSTYPE_AS_LIST (for_type), flags|xxref_init_possible);
      if (rval == NULL_TREE && xxref_init_possible)
	{
	  tree init_type = TREE_TYPE (init);
	  if (TREE_CODE (init_type) == REFERENCE_TYPE)
	    init_type = TREE_TYPE (init_type);
	  if (TYPE_MAIN_VARIANT (init_type) == TYPE_MAIN_VARIANT (type)
	      || (IS_AGGR_TYPE (init_type)
		  && get_base_type (type, init_type, 0)))
	    {
	      if (type == for_type
		  && TYPE_USES_VIRTUAL_BASECLASSES (type))
		{
		  tree addr = build_unary_op (ADDR_EXPR, exp, 0);
		  expand_aggr_vbase_init (type, exp, addr, NULL_TREE);

		  expand_expr_stmt (build_vbase_vtables_init (type, type, exp, addr, 1));
#ifdef sparc
		  expand_asm_operands (build_string (32, "! end of vtable initialization"), 0, 0, 0, 0, input_filename, lineno);
#endif
		}
	      expand_expr_stmt (build_modify_expr (exp, INIT_EXPR, init));
	      return;
	    }
	  else
	    rval = build_method_call (exp, DECL_NAME (TYPE_NAME (type)), parms,
				      CLASSTYPE_AS_LIST (for_type), flags);
	}

      /* Private, protected, or otherwise unavailable.  */
      if (rval == error_mark_node && (flags&LOOKUP_COMPLAIN))
	error_with_aggr_type (for_type, "in base initialization for class `%s'");
      /* A valid initialization using constructor.  */
      else if (rval != error_mark_node && rval != NULL_TREE)
	{
	  /* p. 222: if the base class assigns to `this', then that
	     value is used in the derived class.  */
	  if ((flag_this_is_variable & 1) && alias_this)
	    {
	      TREE_TYPE (rval) = TREE_TYPE (current_class_decl);
	      expand_assignment (current_class_decl, rval, 0, 0);
	    }
	  else
	    expand_expr_stmt (rval);
	}
      else if (parms && TREE_CHAIN (parms) == NULL_TREE)
	{
	  /* If we are initializing one aggregate value
	     from another, and though there are constructors,
	     and none accept the initializer, just do a bitwise
	     copy.
	     
	     @@ This should reject initializer which a constructor
	     @@ rejected on visibility gounds, but there is
	     @@ no way right now to recognize that case with
	     @@ just `error_mark_node'.  */
	  tree itype;
	  init = TREE_VALUE (parms);
	  itype = TREE_TYPE (init);
	  if (TREE_CODE (itype) == REFERENCE_TYPE)
	    {
	      init = convert_from_reference (init);
	      itype = TREE_TYPE (init);
	    }
	  itype = TYPE_MAIN_VARIANT (itype);
	  if (comptypes (TYPE_MAIN_VARIANT (type), itype, 0))
	    {
	      warning ("bitwise copy in initialization of type `%s'",
		       TYPE_NAME_STRING (type));
	      rval = build (INIT_EXPR, type, exp, init);
	      expand_expr_stmt (rval);
	    }
	  else
	    {
	      error_with_aggr_type (for_type, "in base initialization for class `%s',");
	      error_with_aggr_type (type, "invalid initializer to constructor for type `%s'");
	      return;
	    }
	}
      else
	{
	  if (init == NULL_TREE)
	    assert (parms == NULL_TREE);
	  if (parms == NULL_TREE && TREE_VIA_VIRTUAL (for_type))
	    error_with_aggr_type (for_type, "virtual baseclass `%s' does not have default initializer");
	  else
	    {
	      error_with_aggr_type (for_type, "in base initialization for class `%s',");
	      /* This will make an error message for us.  */
	      build_method_call (exp, DECL_NAME (TYPE_NAME (type)), parms,
				 CLASSTYPE_AS_LIST (for_type),
				 (TYPE_USES_VIRTUAL_BASECLASSES (type)
				  ? LOOKUP_NORMAL|LOOKUP_HAS_IN_CHARGE
				  : LOOKUP_NORMAL));
	    }
	  return;
	}
      /* Constructor has been called, but vtables may be for TYPE
	 rather than for FOR_TYPE.  */
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (type)))
	expand_vec_init (exp, exp, array_type_nelts (type), init, 0);
      else if (TYPE_VIRTUAL_P (TREE_TYPE (type)))
	sorry ("arrays of objects with virtual functions but no constructors");
    }
  else
    expand_recursive_init (for_type, true_exp, exp, init, CLASSTYPE_BASE_INIT_LIST (type), alias_this);
}

/* A pointer which holds the initializer.  First call to
   expand_aggr_init gets this value pointed to, and sets it to init_null.  */
static tree *init_ptr, init_null;

/* Subroutine of expand_recursive_init:

   ADDR is the address of the expression being initialized.
   INIT_LIST is the cons-list of initializations to be performed.
   ALIAS_THIS is its same, lovable self.  */
static void
expand_recursive_init_1 (for_type, true_exp, addr, init_list, alias_this)
     tree for_type, true_exp, addr;
     tree init_list;
     int alias_this;
{
  while (init_list)
    {
      if (TREE_PURPOSE (init_list))
	{
	  if (TREE_CODE (TREE_PURPOSE (init_list)) == FIELD_DECL)
	    {
	      tree member = TREE_PURPOSE (init_list);
	      tree subexp = build_indirect_ref (convert_pointer_to (TREE_VALUE (init_list), addr), 0);
	      tree member_base = build (COMPONENT_REF, TREE_TYPE (member), subexp, member);
	      if (IS_AGGR_TYPE (TREE_TYPE (member)))
		expand_aggr_init (member_base, DECL_INITIAL (member), 0);
	      else if (TREE_CODE (TREE_TYPE (member)) == ARRAY_TYPE
		       && TYPE_NEEDS_CONSTRUCTING (TREE_TYPE (member)))
		{
		  member_base = save_expr (default_conversion (member_base));
		  expand_vec_init (member, member_base,
				   array_type_nelts (TREE_TYPE (member)),
				   DECL_INITIAL (member), 0);
		}
	      else expand_expr_stmt (build_modify_expr (member_base, INIT_EXPR, DECL_INITIAL (member)));
	    }
	  else if (TREE_CODE (TREE_PURPOSE (init_list)) == TREE_LIST)
	    {
	      expand_recursive_init_1 (for_type, true_exp, addr, TREE_PURPOSE (init_list), alias_this);
	      expand_recursive_init_1 (for_type, true_exp, addr, TREE_VALUE (init_list), alias_this);
	    }
	  else if (TREE_CODE (TREE_PURPOSE (init_list)) == ERROR_MARK)
	    {
	      /* Only initialize the virtual function tables if we
		 are initializing the ultimate users of those vtables.  */
	      if (TREE_VALUE (init_list))
		{
		  expand_expr_stmt (build_virtual_init (for_type, TREE_VALUE (init_list), addr));
		  if (TREE_VALUE (init_list) == for_type
		      && TYPE_USES_VIRTUAL_BASECLASSES (for_type))
		    expand_expr_stmt (build_vbase_vtables_init (for_type, TREE_VALUE (init_list), true_exp, addr, 1));
#ifdef sparc
		  expand_asm_operands (build_string (32, "! end of vtable initialization"), 0, 0, 0, 0, input_filename, lineno);
#endif
		}
	    }
	  else abort ();
	}
      else if (TREE_VALUE (init_list)
	       && TREE_CODE (TREE_VALUE (init_list)) == RECORD_TYPE)
	{
	  tree subexp = build_indirect_ref (convert_pointer_to (TREE_VALUE (init_list), addr), 0);
	  expand_aggr_init_1 (for_type, true_exp, subexp, *init_ptr,
			      alias_this && ! DECL_OFFSET (TYPE_NAME (TREE_VALUE (init_list))),
			      LOOKUP_PROTECTED_OK|LOOKUP_COMPLAIN);

	  /* INIT_PTR is used up.  */
	  init_ptr = &init_null;
	}
      else
	abort ();
      init_list = TREE_CHAIN (init_list);
    }
}

/* Initialize EXP with INIT.  Type EXP does not have a constructor,
   but it has a baseclass with a constructor or a virtual function
   table which needs initializing.

   INIT_LIST is a cons-list describing what parts of EXP actually
   need to be initialized.  INIT is given to the *unique*, first
   constructor within INIT_LIST.  If there are multiple first
   constructors, such as with multiple inheritance, INIT must
   be zero or an ambiguity error is reported.

   ALIAS_THIS is passed from `expand_aggr_init'.  See comments
   there.  */

static void
expand_recursive_init (for_type, true_exp, exp, init, init_list, alias_this)
     tree for_type, true_exp, exp, init;
     tree init_list;
     int alias_this;
{
  tree *old_init_ptr = init_ptr;
  tree addr = build_unary_op (ADDR_EXPR, exp, 0);
  init_ptr = &init;

  if (true_exp == exp && TYPE_USES_VIRTUAL_BASECLASSES (for_type))
    {
      expand_aggr_vbase_init (for_type, exp, addr, init_list);
      expand_expr_stmt (build_vbase_vtables_init (for_type, for_type, true_exp, addr, 1));
    }
  expand_recursive_init_1 (for_type, true_exp, addr, init_list, alias_this);

  if (*init_ptr)
    {
      tree type = TREE_TYPE (exp);

      if (TREE_CODE (type) == REFERENCE_TYPE)
	type = TREE_TYPE (type);
      if (IS_AGGR_TYPE (type))
	error_with_aggr_type (type, "unexpected argument to constructor `%s'");
      else
	error ("unexpected argument to constructor");
    }
  init_ptr = old_init_ptr;
}

/* Report an error if NAME is not the name of a user-defined,
   aggregate type.  If OR_ELSE is nonzero, give an error message.  */
int
is_aggr_typedef (name, or_else)
     tree name;
{
  tree type = TREE_TYPE (name);

  if (type == NULL_TREE || TREE_CODE (type) != TYPE_DECL)
    {
      if (or_else)
	error ("`%s' fails to be an aggregate typedef",
	       IDENTIFIER_POINTER (name));
      return 0;
    }
  type = TREE_TYPE (type);
  if (! IS_AGGR_TYPE (type))
    {
      fatal ("type `%s' is of non-aggregate type",
	     IDENTIFIER_POINTER (name));
      return 0;
    }
  return 1;
}

/* This code could just as well go in `cplus-class.c', but is placed here for
   modularity.  */

/* For an expression of the form CNAME :: NAME (PARMLIST), build
   the appropriate function call.  */
tree
build_member_call (cname, name, parmlist)
     tree cname, name, parmlist;
{
  tree type, t;
  tree method_name = name;
  int dtor = 0;
  int dont_use_this = 0;
  tree basetype_path, decl;

  if (TREE_CODE (method_name) == BIT_NOT_EXPR)
    {
      method_name = TREE_OPERAND (method_name, 0);
      dtor = 1;
    }

  if (TREE_CODE (cname) == SCOPE_REF)
    {
      sorry ("multiple scope qualifications in build_member_call");
      return error_mark_node;
    }

  if (! is_aggr_typedef (cname, 1))
    return error_mark_node;

  /* An operator we did not like.  */
  if (name == NULL_TREE)
    return error_mark_node;

  if (dtor)
    {
      if (! TYPE_HAS_DESTRUCTOR (TREE_TYPE (TREE_TYPE (cname))))
	error ("type `%s' does not have a destructor",
	       IDENTIFIER_POINTER (cname));
      else
	error ("destructor specification error");
      return error_mark_node;
    }

  if (TREE_CODE (name) == OP_IDENTIFIER)
    method_name = build_operator_fnname (&name, parmlist, 1);
  type = TREE_TYPE (TREE_TYPE (cname));

  /* No object?  Then just fake one up, and let build_method_call
     figure out what to do.  */
  if (current_class_type == 0
      || get_base_distance (type, current_class_type, 0, &basetype_path) == -1)
    dont_use_this = 1;

  if (dont_use_this)
    {
      basetype_path = NULL_TREE;
      decl = build1 (NOP_EXPR,
		     TYPE_POINTER_TO (TREE_TYPE (TREE_TYPE (cname))),
		     error_mark_node);
    }
  else if (current_class_decl == 0)
    {
      dont_use_this = 1;
      decl = build1 (NOP_EXPR,
		     TYPE_POINTER_TO (TREE_TYPE (TREE_TYPE (cname))),
		     error_mark_node);
    }
  else decl = current_class_decl;

  if (t = lookup_fnfields (CLASSTYPE_AS_LIST (type), method_name, 0))
    return build_method_call (decl, method_name, parmlist, basetype_path,
			      LOOKUP_NORMAL|LOOKUP_NONVIRTUAL);
  if (TREE_CODE (name) == IDENTIFIER_NODE
      && (t = lookup_field (CLASSTYPE_AS_LIST (type), name, 1)))
    {
      if (t == error_mark_node)
	return error_mark_node;
      if (TREE_CODE (t) == FIELD_DECL)
	{
	  if (dont_use_this)
	    {
	      error ("invalid use of non-static field `%s'",
		     IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	  decl = build (COMPONENT_REF, TREE_TYPE (t), decl, t);
	}
      else if (TREE_CODE (t) == VAR_DECL)
	decl = t;
      else
	{
	  error ("invalid use of member `%s::%s'",
		 IDENTIFIER_POINTER (cname), name);
	  return error_mark_node;
	}
      return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, decl, parmlist);
    }
  else
    {
      char *err_name;
      if (TREE_CODE (name) == OP_IDENTIFIER)
	{
	  char *op_name = operator_name_string (method_name);
	  err_name = (char *)alloca (13 + strlen (op_name));
	  sprintf (err_name, "operator %s", op_name);
	}
      else if (TREE_CODE (name) == IDENTIFIER_NODE)
	err_name = IDENTIFIER_POINTER (name);
      else
	abort ();

      error ("no method `%s::%s'", IDENTIFIER_POINTER (cname), err_name);
      return error_mark_node;
    }
}

/* Build a reference to a member of an aggregate.  This is not a
   C++ `&', but really something which can have its address taken,
   and then act as a pointer to member, for example CNAME :: FIELD
   can have its address taken by saying & CNAME :: FIELD.

   @@ Prints out lousy diagnostics for operator <typename>
   @@ fields.

   @@ This function should be rewritten and placed in cplus-search.c.  */
tree
build_offset_ref (cname, name)
     tree cname, name;
{
  tree decl, type, fnfields, fields, t = error_mark_node;
  tree basetypes = NULL_TREE;
  int dtor = 0;

  if (TREE_CODE (cname) == SCOPE_REF)
    {
      sorry ("multiple scope qualifications in build_offset_ref");
      return error_mark_node;
    }

  if (! is_aggr_typedef (cname, 1))
    return error_mark_node;

  type = TREE_TYPE (TREE_TYPE (cname));

  if (TREE_CODE (name) == BIT_NOT_EXPR)
    {
      dtor = 1;
      name = TREE_OPERAND (name, 0);
    }

  if (TYPE_SIZE (type) == 0)
    {
      t = IDENTIFIER_CLASS_VALUE (name);
      if (t == 0)
	{
	  error_with_aggr_type (type, "incomplete type `%s' does not have member `%s'", IDENTIFIER_POINTER (name));
	  return error_mark_node;
	}
      if (TREE_CODE (t) == TYPE_DECL)
	{
	  error_with_decl (t, "member `%s' is just a type declaration");
	  return error_mark_node;
	}
      if (TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == CONST_DECL)
	{
	  TREE_USED (t) = 1;
	  return t;
	}
      if (TREE_CODE (t) == FIELD_DECL)
	sorry ("use of member in incomplete aggregate type");
      else if (TREE_CODE (t) == FUNCTION_DECL)
	sorry ("use of member function in incomplete aggregate type");
      else
	abort ();
      return error_mark_node;
    }

  /* Unresolved multi-arity operator.  */
  if (TREE_CODE (name) == OP_IDENTIFIER)
    {
      t = copy_node (name);
      TREE_TYPE (t) = unknown_type_node;
      return t;
    }
  if (TREE_CODE (name) == TYPE_EXPR)
    /* Pass a TYPE_DECL to build_component_type_expr.  */
    return build_component_type_expr (TREE_TYPE (cname), name, NULL_TREE, 1);

  fnfields = lookup_fnfields (CLASSTYPE_AS_LIST (type), name, 0);
  fields = lookup_field (type, name, 0);

  if (fields == error_mark_node)
    return error_mark_node;

  if (fnfields)
    {
      basetypes = TREE_PURPOSE (fnfields);

      /* Go from the TREE_BASELINK to the member function info.  */
      t = TREE_VALUE (fnfields);

      if (fields)
	{
	  if (DECL_FIELD_CONTEXT (fields) == DECL_FIELD_CONTEXT (t))
	    {
	      error ("ambiguous member reference: member `%s' defined as both field and function",
		     IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	  if (get_base_type (DECL_FIELD_CONTEXT (fields), DECL_FIELD_CONTEXT (t), 0))
	    ;
	  else if (get_base_type (DECL_FIELD_CONTEXT (t), DECL_FIELD_CONTEXT (fields), 0))
	    t = fields;
	  else
	    {
	      error ("ambiguous member reference: member `%s' derives from distinct classes in multiple inheritance lattice");
	      return error_mark_node;
	    }
	}

      if (t == TREE_VALUE (fnfields))
	{
	  extern int flag_save_memoized_contexts;

	  /* This does not handle visibility checking yet.  */
	  if (TREE_CHAIN (t) == NULL_TREE || dtor)
	    {
	      enum visibility_type visibility;

	      /* unique functions are handled easily.  */
	    unique:
	      visibility = compute_visibility (basetypes, t);
	      if (visibility == visibility_protected)
		{
		  error_with_decl (t, "member function `%s' is protected");
		  error ("in this context");
		  return error_mark_node;
		}
	      if (visibility == visibility_private)
		{
		  error_with_decl (t, "member function `%s' is private");
		  error ("in this context");
		  return error_mark_node;
		}
	      return build (OFFSET_REF, TREE_TYPE (t), NULL_TREE, t);
	    }

	  /* overloaded functions may need more work.  */
	  if (cname == name)
	    {
	      if (TYPE_HAS_DESTRUCTOR (type)
		  && TREE_CHAIN (TREE_CHAIN (t)) == NULL_TREE)
		{
		  t = TREE_CHAIN (t);
		  goto unique;
		}
	    }
	  if (flag_save_memoized_contexts
	      && !TREE_PERMANENT (fnfields)
	      && global_bindings_p ())
	    fnfields = copy_list (fnfields);
	  t = build_tree_list (error_mark_node, fnfields);
	  TREE_TYPE (t) = build_member_type (type, unknown_type_node);
	  return t;
	}
    }

  /* Now that we know we are looking for a field, see if we
     have access to that field.  Lookup_field will give us the
     error message.  */

  if (current_class_type == 0
      || get_base_distance (type, current_class_type, 0, &basetypes) == -1)
    {
      basetypes = CLASSTYPE_AS_LIST (type);
      decl = build1 (NOP_EXPR,
		     TREE_TYPE (TREE_TYPE (cname)),
		     error_mark_node);
    }
  else if (current_class_decl == 0)
    decl = build1 (NOP_EXPR, TREE_TYPE (TREE_TYPE (cname)),
		   error_mark_node);
  else decl = C_C_D;

  t = lookup_field (basetypes, name, 1);

  if (t == error_mark_node)
    return error_mark_node;

  if (t == NULL_TREE)
    {
      if (OPERATOR_TYPENAME_P (name))
	error ("type conversion operator not a member of type `%s'",
	       IDENTIFIER_POINTER (cname));
      else
	error ("field `%s' is not a member of type `%s'",
	       IDENTIFIER_POINTER (name),
	       IDENTIFIER_POINTER (cname));
      return error_mark_node;
    }

  if (TREE_CODE (t) == TYPE_DECL)
    {
      error_with_decl (t, "member `%s' is just a type declaration");
      return error_mark_node;
    }
  /* static class members and class-specific enum
     values can be returned without further ado.  */
  if (TREE_CODE (t) == VAR_DECL || TREE_CODE (t) == CONST_DECL)
    {
      TREE_USED (t) = 1;
      return t;
    }

  /* static class functions too.  */
  if (TREE_CODE (t) == FUNCTION_DECL && TREE_CODE (TREE_TYPE (t)) == FUNCTION_TYPE)
    abort ();

  /* In member functions, the form `cname::name' is no longer
     equivalent to `this->cname::name'.  */
  return build (OFFSET_REF, build_member_type (type, TREE_TYPE (t)), decl, t);
}

/* Given an object EXP and a member function reference MEMBER,
   return the address of the actual member function.  */
tree
get_member_function (exp_addr_ptr, exp, member)
     tree *exp_addr_ptr;
     tree exp, member;
{
  tree ctype = TREE_TYPE (exp);
  tree function = save_expr (build_unary_op (ADDR_EXPR, member, 0));

  if (TYPE_VIRTUAL_P (ctype)
      || (flag_all_virtual == 1
	  && (TYPE_OVERLOADS_METHOD_CALL_EXPR (ctype)
	      || TYPE_NEEDS_WRAPPER (ctype))))
    {
      tree e0, e1, e3;
      tree exp_addr;

      /* Save away the unadulterated `this' pointer.  */
      exp_addr = save_expr (*exp_addr_ptr);

      /* Cast function to signed integer.  */
      e0 = build1 (NOP_EXPR, integer_type_node, function);

#ifdef VTABLE_USES_MASK
      /* If we are willing to limit the number of
	 virtual functions a class may have to some
	 *small* number, then if, for a function address,
	 we are passed some small number, we know that
	 it is a virtual function index, and work from there.  */
      e1 = build (BIT_AND_EXPR, integer_type_node, e0, vtbl_mask);
#else
      /* There is a hack here that takes advantage of
	 twos complement arithmetic, and the fact that
	 there are more than one UNITS to the WORD.
	 If the high bit is set for the `function',
	 then we pretend it is a virtual function,
	 and the array indexing will knock this bit
	 out the top, leaving a valid index.  */
#if UNITS_PER_WORD <= 1
      virtual_functions_lose !;
#endif
      e1 = build (GT_EXPR, integer_type_node, e0, integer_zero_node);
      e1 = build_compound_expr (tree_cons (NULL_TREE, exp_addr,
					   build_tree_list (NULL_TREE, e1)));
      e1 = save_expr (e1);
#endif

      if (TREE_VOLATILE (*exp_addr_ptr))
	{
	  exp = build_indirect_ref (exp_addr, 0);
	  *exp_addr_ptr = exp_addr;
	}

      /* This is really hairy: if the function pointer is a pointer
	 to a non-virtual member function, then we can't go mucking
	 with the `this' pointer (any more than we aleady have to
	 this point).  If it is a pointer to a virtual member function,
	 then we have to adjust the `this' pointer according to
	 what the virtual function table tells us.  */

      e3 = build_vfn_ref (exp_addr_ptr, exp, e0);
      assert (e3 != error_mark_node);

      /* Change this pointer type from `void *' to the
	 type it is really supposed to be.  */
      TREE_TYPE (e3) = TREE_TYPE (function);

      /* If non-virtual, use what we had originally.  Otherwise,
	 use the value we get from the virtual function table.  */
      *exp_addr_ptr = build_conditional_expr (e1, exp_addr, *exp_addr_ptr);

      function = build_conditional_expr (e1, function, e3);
    }
  return build_indirect_ref (function, 0);
}

/* If a OFFSET_REF made it through to here, then it did
   not have its address taken.  */

tree
resolve_offset_ref (exp)
     tree exp;
{
  tree base;
  tree member;
  tree basetype, addr;

  if (TREE_CODE (exp) == TREE_LIST)
    return build_unary_op (ADDR_EXPR, exp, 0);

  assert (TREE_CODE (exp) == OFFSET_REF);
  member = TREE_OPERAND (exp, 1);
  if (TREE_STATIC (member))
    {
      /* These were static members.  */
      if (mark_addressable (member) == 0)
	return error_mark_node;
      return member;
    }

  /* Syntax error can cause a member which should
     have been seen as static to be grok'd as non-static.  */
  if (TREE_CODE (member) == FIELD_DECL && C_C_D == NULL_TREE)
    {
      if (TREE_ADDRESSABLE (member) == 0)
	{
	  error_with_decl (member, "member `%s' is non-static in static member function context");
	  TREE_ADDRESSABLE (member) = 1;
	}
      return error_mark_node;
    }

  base = TREE_OPERAND (exp, 0);
  assert (base != NULL_TREE);

  /* The first case is really just a reference to a member of `this'.  */
  if (TREE_CODE (member) == FIELD_DECL
      && (base == C_C_D
	  || (TREE_CODE (base) == NOP_EXPR
	      && TREE_OPERAND (base, 0) == error_mark_node)))
    {
      tree basetype_path;
      enum visibility_type visibility;

      basetype = DECL_CONTEXT (member);
      if (get_base_distance (basetype, current_class_type, 0, &basetype_path) < 0)
	{
	  error_not_base_type (basetype, current_class_type);
	  return error_mark_node;
	}
      addr = convert_pointer_to (basetype, current_class_decl);
      visibility = compute_visibility (basetype_path, member);
      if (visibility == visibility_public)
	return build (COMPONENT_REF, TREE_TYPE (member),
		      build_indirect_ref (addr, 0), member);
      if (visibility == visibility_protected)
	{
	  error_with_decl ("member `%s' is protected");
	  error ("in this context");
	  return error_mark_node;
	}
      if (visibility == visibility_private)
	{
	  error_with_decl ("member `%s' is private");
	  error ("in this context");
	  return error_mark_node;
	}
      abort ();
    }
  /* If this is a reference to a member function, then return
     the address of the member function (which may involve going
     through the object's vtable), otherwise, return an expression
     for the derefernced pointer-to-member construct.  */
  addr = build_unary_op (ADDR_EXPR, base, 0);
  if (TREE_CODE (TREE_TYPE (member)) == METHOD_TYPE)
    {
      basetype = TYPE_METHOD_BASETYPE (TREE_TYPE (member));
      addr = convert_pointer_to (basetype, addr);
      return build_unary_op (ADDR_EXPR, get_member_function (&addr, build_indirect_ref (addr, 0), member), 0);
    }
  else if (TREE_CODE (TREE_TYPE (member)) == OFFSET_TYPE)
    {
      basetype = TYPE_OFFSET_BASETYPE (TREE_TYPE (member));
      addr = convert_pointer_to (basetype, addr);
      member = convert (ptr_type_node, build_unary_op (ADDR_EXPR, member, 0));
      return build1 (INDIRECT_REF, TREE_TYPE (exp),
		     build (PLUS_EXPR, ptr_type_node, addr, member));
    }
  abort ();
}

/* Return either DECL or its known constant value (if it has one).  */

tree
decl_constant_value (decl)
     tree decl;
{
  if (
#if 0
      /* These may be necessary for C, but they break C++.  */
      ! TREE_PUBLIC (decl)
      /* Don't change a variable array bound or initial value to a constant
	 in a place where a variable is invalid.  */
      && current_function_decl != 0
      && ! pedantic
      &&
#endif /* 0 */
      ! TREE_THIS_VOLATILE (decl)
      && DECL_INITIAL (decl) != 0
      && TREE_CODE (DECL_INITIAL (decl)) != ERROR_MARK
      /* This is invalid if initial value is not constant.
	 If it has either a function call, a memory reference,
	 or a variable, then re-evaluating it could give different results.  */
      && TREE_LITERAL (DECL_INITIAL (decl))
#if 0
      /* Check for cases where this is sub-optimal, even though valid.  */
      && TREE_CODE (DECL_INITIAL (decl)) != CONSTRUCTOR
      && DECL_MODE (decl) != BLKmode
#endif
      )
    return DECL_INITIAL (decl);
  return decl;
}

/* Friend handling routines.  */
/* Friend data structures:

   Friend lists come from TYPE_DECL nodes.  Since all aggregate
   types are automatically typedef'd, these node are guaranteed
   to exist.

   The TREE_PURPOSE of a friend list is the name of the friend,
   and its TREE_VALUE is another list.

   The TREE_PURPOSE of that list is a type, which allows
   all functions of a given type to be friends.
   The TREE_VALUE of that list is an individual function
   which is a friend.

   Non-member friends will match only by their DECL.  Their
   member type is NULL_TREE, while the type of the inner
   list will either be of aggregate type or error_mark_node.  */

/* Tell if this function specified by FUNCTION_DECL
   can be a friend of type TYPE.
   Return nonzero if friend, zero otherwise.

   DECL can be zero if we are calling a constructor or accessing a
   member in global scope.  */
int
is_friend (type, decl)
     tree type, decl;
{
  tree typedecl = TYPE_NAME (type);
  tree ctype = NULL_TREE;
  tree list;
  tree name;

  if (decl == NULL_TREE)
    return 0;

  if (TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE)
    ctype = TYPE_METHOD_BASETYPE (TREE_TYPE (decl));
  else if (DECL_STATIC_FUNCTION_P (decl))
    ctype = DECL_CONTEXT (decl);
  if (ctype)
    {
      list = CLASSTYPE_FRIEND_CLASSES (TREE_TYPE (typedecl));
      while (list)
	{
	  if (ctype == TREE_VALUE (list))
	    return 1;
	  list = TREE_CHAIN (list);
	}
    }

  list = DECL_FRIENDLIST (typedecl);
  name = DECL_ORIGINAL_NAME (decl);
  while (list)
    {
      if (name == TREE_PURPOSE (list))
	{
	  tree friends = TREE_VALUE (list);
	  name = DECL_NAME (decl);
	  while (friends)
	    {
	      if (ctype == TREE_PURPOSE (friends))
		return 1;
	      if (name == DECL_NAME (TREE_VALUE (friends)))
		return 1;
	      friends = TREE_CHAIN (friends);
	    }
	  return 0;
	}
      list = TREE_CHAIN (list);
    }
  return 0;
}

/* Add a new friend to the friends of the aggregate type TYPE.
   DECL is the FUNCTION_DECL of the friend being added.  */
static void
add_friend (type, decl)
     tree type, decl;
{
  tree typedecl = TYPE_NAME (type);
  tree list = DECL_FRIENDLIST (typedecl);
  tree name = DECL_ORIGINAL_NAME (decl);
  tree ctype = TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE
    ? TYPE_METHOD_BASETYPE (TREE_TYPE (decl)) : error_mark_node;

  while (list)
    {
      if (name == TREE_PURPOSE (list))
	{
	  tree friends = TREE_VALUE (list);
	  while (friends)
	    {
	      if (decl == TREE_VALUE (friends))
		{
		  warning_with_decl (decl, "`%s' is already a friend of class `%s'", IDENTIFIER_POINTER (DECL_NAME (typedecl)));
		  return;
		}
	      friends = TREE_CHAIN (friends);
	    }
	  TREE_VALUE (list) = tree_cons (ctype, decl, TREE_VALUE (list));
	  return;
	}
      list = TREE_CHAIN (list);
    }
  DECL_FRIENDLIST (typedecl)
    = tree_cons (DECL_ORIGINAL_NAME (decl),
		 build_tree_list (error_mark_node, decl),
		 DECL_FRIENDLIST (typedecl));
  if (! strncmp (IDENTIFIER_POINTER (DECL_NAME (decl)),
		 OPERATOR_MODIFY_FORMAT,
		 OPERATOR_MODIFY_LENGTH))
    {
      tree parmtypes = TYPE_ARG_TYPES (TREE_TYPE (decl));
      TYPE_HAS_ASSIGNMENT (TREE_TYPE (typedecl)) = 1;
      TYPE_GETS_ASSIGNMENT (TREE_TYPE (typedecl)) = 1;
      if (parmtypes && TREE_CHAIN (parmtypes))
	{
	  tree parmtype = TREE_VALUE (TREE_CHAIN (parmtypes));
	  if (TREE_CODE (parmtype) == REFERENCE_TYPE
	      && TREE_TYPE (parmtypes) == TREE_TYPE (typedecl))
	    {
	      TYPE_HAS_ASSIGN_REF (TREE_TYPE (typedecl)) = 1;
	      TYPE_GETS_ASSIGN_REF (TREE_TYPE (typedecl)) = 1;
	    }
	}
    }
}

/* Declare that every member function NAME in FRIEND_TYPE
   (which may be NULL_TREE) is a friend of type TYPE.  */
static void
add_friends (type, name, friend_type)
     tree type, name, friend_type;
{
  tree typedecl = TYPE_NAME (type);
  tree list = DECL_FRIENDLIST (typedecl);

  while (list)
    {
      if (name == TREE_PURPOSE (list))
	{
	  tree friends = TREE_VALUE (list);
	  while (friends && TREE_PURPOSE (friends) != friend_type)
	    friends = TREE_CHAIN (friends);
	  if (friends)
	    if (friend_type)
	      warning ("method `%s::%s' is already a friend of class",
		       TYPE_NAME_STRING (friend_type),
		       IDENTIFIER_POINTER (name));
	    else
	      warning ("function `%s' is already a friend of class `%s'",
		       IDENTIFIER_POINTER (name),
		       IDENTIFIER_POINTER (DECL_NAME (typedecl)));
	  else
	    TREE_VALUE (list) = tree_cons (friend_type, NULL_TREE,
					   TREE_VALUE (list));
	  return;
	}
      list = TREE_CHAIN (list);
    }
  DECL_FRIENDLIST (typedecl) =
    tree_cons (name,
	       build_tree_list (friend_type, NULL_TREE),
	       DECL_FRIENDLIST (typedecl));
  if (! strncmp (name, OPERATOR_MODIFY_FORMAT, OPERATOR_MODIFY_LENGTH))
    {
      TYPE_HAS_ASSIGNMENT (TREE_TYPE (typedecl)) = 1;
      TYPE_GETS_ASSIGNMENT (TREE_TYPE (typedecl)) = 1;
      sorry ("declaring \"friend operator =\" will not find \"operator = (X&)\" if it exists");
    }
}

/* Set up a cross reference so that type TYPE will
   make member function CTYPE::DECL a friend when CTYPE
   is finally defined.  */
void
xref_friend (type, decl, ctype)
     tree type, decl, ctype;
{
  tree typedecl = TYPE_NAME (type);
  tree friend_decl = TYPE_NAME (ctype);
  tree t = tree_cons (NULL_TREE, ctype, DECL_UNDEFINED_FRIENDS (typedecl));

  DECL_UNDEFINED_FRIENDS (typedecl) = t;
  SET_DECL_WAITING_FRIENDS (friend_decl, tree_cons (type, t, DECL_WAITING_FRIENDS (friend_decl)));
  TREE_TYPE (DECL_WAITING_FRIENDS (friend_decl)) = decl;
}

/* Set up a cross reference so that functions with name NAME and
   type CTYPE know that they are friends of TYPE.  */
void
xref_friends (type, name, ctype)
     tree type, name, ctype;
{
  tree typedecl = TYPE_NAME (type);
  tree friend_decl = TYPE_NAME (ctype);
  tree t = tree_cons (NULL_TREE, ctype,
		      DECL_UNDEFINED_FRIENDS (typedecl));

  DECL_UNDEFINED_FRIENDS (typedecl) = t;
  SET_DECL_WAITING_FRIENDS (friend_decl, tree_cons (type, t, DECL_WAITING_FRIENDS (friend_decl)));
  TREE_TYPE (DECL_WAITING_FRIENDS (friend_decl)) = name;
}

/* Make FRIEND_TYPE a friend class to TYPE.  If FRIEND_TYPE has already
   been defined, we make all of its member functions friends of
   TYPE.  If not, we make it a pending friend, which can later be added
   when its definition is seen.  If a type is defined, then its TYPE_DECL's
   DECL_UNDEFINED_FRIENDS contains a (possibly empty) list of friend
   classes that are not defined.  If a type has not yet been defined,
   then the DECL_WAITING_FRIENDS contains a list of types
   waiting to make it their friend.  Note that these two can both
   be in use at the same time!  */
void
make_friend_class (type, friend_type)
     tree type, friend_type;
{
  tree classes;

  if (type == friend_type)
    {
      warning ("class `%s' is implicitly friends with itself",
	       TYPE_NAME_STRING (type));
      return;
    }

#ifdef FIELD_XREF
  FIELD_xref_hier(TYPE_NAME_STRING(type),
		  TYPE_NAME_STRING(friend_type),0,0,1);
#endif

  classes = CLASSTYPE_FRIEND_CLASSES (type);
  while (classes && TREE_VALUE (classes) != friend_type)
    classes = TREE_CHAIN (classes);
  if (classes)
    warning ("class `%s' is already friends with class `%s'",
	     TYPE_NAME_STRING (TREE_VALUE (classes)), TYPE_NAME_STRING (type));
  else
    {
      CLASSTYPE_FRIEND_CLASSES (type)
	= tree_cons (NULL_TREE, friend_type, CLASSTYPE_FRIEND_CLASSES (type));
    }
}

/* Main friend processor.  This is large, and for modularity purposes,
   has been removed from grokdeclarator.  It returns `void_type_node'
   to indicate that something happened, though a FIELD_DECL is
   not returned.

   CTYPE is the class this friend belongs to.

   DECLARATOR is the name of the friend.

   DECL is the FUNCTION_DECL that the friend is.

   In case we are parsing a friend which is part of an inline
   definition, we will need to store PARM_DECL chain that comes
   with it into the DECL_ARGUMENTS slot of the FUNCTION_DECL.

   FLAGS is just used for `grokclassfn'.

   QUALS say what special qualifies should apply to the object
   pointed to by `this'.  */
tree
do_friend (ctype, declarator, decl, parmdecls, flags, quals)
     tree ctype, declarator, decl, parmdecls;
     enum overload_flags flags;
     tree quals;
{
  if (ctype)
    {
      tree cname = TYPE_NAME (ctype);
      if (TREE_CODE (cname) == TYPE_DECL)
	cname = DECL_NAME (cname);

      /* A method friend.  */
      if (TREE_CODE (decl) == FUNCTION_DECL)
	{
	  if (flags == NO_SPECIAL && ctype && declarator == cname)
	    DECL_CONSTRUCTOR_P (decl) = 1;

	  /* This will set up DECL_ARGUMENTS for us.  */
	  grokclassfn (ctype, cname, decl, flags, TYPE_SIZE (ctype) != 0, quals);
	  if (TREE_TYPE (decl) != error_mark_node)
	    {
	      if (TYPE_SIZE (ctype))
		{
		  /* We don't call pushdecl here yet, or ever on this
		     actual FUNCTION_DECL.  We must preserve its TREE_CHAIN
		     until the end.  */
		  make_decl_rtl (decl, NULL_TREE, 1);
		  add_friend (current_class_type, decl);
		}
	      else
		xref_friend (current_class_type, decl, ctype);
	      DECL_FRIEND_P (decl) = 1;
	    }
	}
      else
	{
	  /* Possibly a bunch of method friends.  */

	  /* Get the class they belong to.  */
	  tree ctype = TREE_TYPE (TREE_TYPE (cname));

	  /* This class is defined, use its methods now.  */
	  if (TYPE_SIZE (ctype))
	    {
	      tree fields = lookup_fnfields (CLASSTYPE_AS_LIST (ctype), declarator, 0);
	      if (fields)
		add_friends (current_class_type, declarator, ctype);
	      else
		error ("method `%s' is not a member of class `%s'",
		       IDENTIFIER_POINTER (declarator),
		       IDENTIFIER_POINTER (cname));
	    }
	  else
	    xref_friends (current_class_type, declarator, ctype);
	  decl = void_type_node;
	}
    }
  else if (TREE_CODE (decl) == FUNCTION_DECL
	   && ((IDENTIFIER_LENGTH (declarator) == 4
		&& IDENTIFIER_POINTER (declarator)[0] == 'm'
		&& ! strcmp (IDENTIFIER_POINTER (declarator), "main"))
	       || (IDENTIFIER_LENGTH (declarator) > 10
		   && IDENTIFIER_POINTER (declarator)[0] == '_'
		   && IDENTIFIER_POINTER (declarator)[1] == '_'
		   && strncmp (IDENTIFIER_POINTER (declarator)+2,
			       "builtin_", 8) == 0)))
    {
      /* raw "main", and builtin functions never gets overloaded,
	 but they can become friends.  */
      TREE_PUBLIC (decl) = 1;
      add_friend (current_class_type, decl);
      DECL_FRIEND_P (decl) = 1;
      if (IDENTIFIER_POINTER (declarator)[0] == '_')
	{
	  if (! strcmp (IDENTIFIER_POINTER (declarator)+10, "new"))
	    TREE_GETS_NEW (current_class_type) = 0;
	  else if (! strcmp (IDENTIFIER_POINTER (declarator)+10, "delete"))
	    TREE_GETS_DELETE (current_class_type) = 0;
	}
      decl = void_type_node;
    }
  /* A global friend.
     @@ or possibly a friend from a base class ?!?  */
  else if (TREE_CODE (decl) == FUNCTION_DECL)
    {
      /* Friends must all go through the overload machinery,
	 even though they may not technically be overloaded.

	 Note that because classes all wind up being top-level
	 in their scope, their friend wind up in top-level scope as well.  */
      DECL_NAME (decl) =
	build_decl_overload (IDENTIFIER_POINTER (declarator),
			     TYPE_ARG_TYPES (TREE_TYPE (decl)),
			     TREE_CODE (TREE_TYPE (decl)) == METHOD_TYPE);
      DECL_ASSEMBLER_NAME (decl) = IDENTIFIER_POINTER (DECL_NAME (decl));
      DECL_ARGUMENTS (decl) = parmdecls;
      /* We can call pushdecl here, because the TREE_CHAIN of this
	 FUNCTION_DECL is not needed for other purposes.  */
      decl = pushdecl_top_level (decl);

      make_decl_rtl (decl, NULL_TREE, 1);
      add_friend (current_class_type, decl);

      if (! TREE_OVERLOADED (declarator)
	  && IDENTIFIER_GLOBAL_VALUE (declarator)
	  && TREE_CODE (IDENTIFIER_GLOBAL_VALUE (declarator)) == FUNCTION_DECL)
	{
	  error ("friend `%s' implicitly overloaded",
		 IDENTIFIER_POINTER (declarator));
	  error_with_decl (IDENTIFIER_GLOBAL_VALUE (declarator),
			   "after declaration of non-overloaded `%s'");
	}
      DECL_FRIEND_P (decl) = 1;
      DECL_OVERLOADED (decl) = 1;
      TREE_OVERLOADED (declarator) = 1;
      push_overloaded_decl (decl);
    }
  else
    {
      /* @@ Should be able to ingest later definitions of this function
	 before use.  */
      tree decl = IDENTIFIER_GLOBAL_VALUE (declarator);
      if (decl == NULL_TREE)
	{
	  warning ("implicitly declaring `%s' as struct",
		   IDENTIFIER_POINTER (declarator));
	  decl = xref_tag (record_type_node, declarator, NULL_TREE);
	  decl = TYPE_NAME (decl);
	}

      /* Allow abbreviated declarations of overloaded functions,
	 but not if those functions are really class names.  */
      if (TREE_CODE (decl) == TREE_LIST && TREE_TYPE (TREE_PURPOSE (decl)))
	{
	  warning ("`friend %s' archaic, use `friend class %s' instead",
		   IDENTIFIER_POINTER (declarator),
		   IDENTIFIER_POINTER (declarator));
	  decl = TREE_TYPE (TREE_PURPOSE (decl));
	}

      if (TREE_CODE (decl) == TREE_LIST)
	add_friends (current_class_type, TREE_PURPOSE (decl), NULL_TREE);
      else
	make_friend_class (current_class_type, TREE_TYPE (decl));
      decl = void_type_node;
    }
  return decl;
}

/* TYPE has now been defined.  It may, however, have a number of things
   waiting make make it their friend.  We resolve these references
   here.  */
void
embrace_waiting_friends (type)
     tree type;
{
  tree decl = TYPE_NAME (type);
  tree waiters;

  if (TREE_CODE (decl) != TYPE_DECL)
    return;

  for (waiters = DECL_WAITING_FRIENDS (decl); waiters;
       waiters = TREE_CHAIN (waiters))
    {
      tree waiter = TREE_PURPOSE (waiters);
      tree waiter_prev = TREE_VALUE (waiters);
      tree decl = TREE_TYPE (waiters);
      tree name = decl ? (TREE_CODE (decl) == IDENTIFIER_NODE
			  ? decl : DECL_ORIGINAL_NAME (decl)) : NULL_TREE;
      if (name)
	{
	  /* @@ There may be work to be done since we have not verified
	     @@ consistency between original and friend declarations
	     @@ of the functions waiting to become friends.  */
	  tree field = lookup_fnfields (CLASSTYPE_AS_LIST (type), name, 0);
	  if (field)
	    if (decl == name)
	      add_friends (waiter, name, type);
	    else
	      add_friend (waiter, decl);
	  else
	    error_with_file_and_line (DECL_SOURCE_FILE (TYPE_NAME (waiter)),
				      DECL_SOURCE_LINE (TYPE_NAME (waiter)),
				      "no method `%s' defined in class `%s' to be friend",
				      IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (TREE_TYPE (waiters))),
				      TYPE_NAME_STRING (type));
	}
      else
	make_friend_class (type, waiter);

      if (TREE_CHAIN (waiter_prev))
	TREE_CHAIN (waiter_prev) = TREE_CHAIN (TREE_CHAIN (waiter_prev));
      else
	DECL_UNDEFINED_FRIENDS (TYPE_NAME (waiter)) = NULL_TREE;
    }
}

/* Generate a C++ "new" expression. DECL is either a TREE_LIST
   (which needs to go through some sort of groktypename) or it
   is the name of the class we are newing. INIT is an initialization value.
   It is either an EXPRLIST, an EXPR_NO_COMMAS, or something in braces.
   If INIT is void_type_node, it means do *not* call a constructor
   for this instance.

   For types with constructors, the data returned is initialized
   by the approriate constructor.

   Whether the type has a constructor or not, if it has a pointer
   to a virtual function table, then that pointer is set up
   here.

   Unless I am mistaken, a call to new () will return initialized
   data regardless of whether the constructor itself is private or
   not.

   PLACEMENT is the `placement' list for user-defined operator new ().  */

tree
build_new (placement, decl, init, use_global_new)
     tree placement;
     tree decl, init;
     int use_global_new;
{
  extern tree require_complete_type ();	/* typecheck.c */
  tree type, true_type, size, rval;
  tree init1 = NULL_TREE, nelts;
  int has_call = 0, has_array = 0;
  tree alignment = NULL_TREE;
  tree pending_sizes = NULL_TREE;

  if (decl == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (decl) == TREE_LIST)
    {
      tree absdcl = TREE_VALUE (decl);
      tree last_absdcl = NULL_TREE;
      int old_immediate_size_expand;

      if (current_function_decl
	  && DECL_CONSTRUCTOR_P (current_function_decl))
	{
	  old_immediate_size_expand = immediate_size_expand;
	  immediate_size_expand = 0;
	}

      nelts = integer_one_node;

      if (absdcl && TREE_CODE (absdcl) == CALL_EXPR)
	{
	  /* probably meant to be a call */
	  has_call = 1;
	  init1 = TREE_OPERAND (absdcl, 1);
	  absdcl = TREE_OPERAND (absdcl, 0);
	  TREE_VALUE (decl) = absdcl;
	}
      while (absdcl && TREE_CODE (absdcl) == INDIRECT_REF)
	{
	  last_absdcl = absdcl;
	  absdcl = TREE_OPERAND (absdcl, 0);
	}

      if (last_absdcl)
	{
	  tree save_absdecl = TREE_VALUE (decl);
	  TREE_VALUE (decl) = last_absdcl;
	  true_type = groktypename (decl);
	  TREE_VALUE (decl) = save_absdecl;
	}
      else
	true_type = groktypename (decl);

      while (absdcl && TREE_CODE (absdcl) == ARRAY_REF)
	{
	  /* probably meant to be a vec new */
	  tree this_nelts;

	  has_array = 1;
	  this_nelts = save_expr (TREE_OPERAND (absdcl, 1));
	  absdcl = TREE_OPERAND (absdcl, 0);
	  if (this_nelts == NULL_TREE)
	    error ("new of array type fails to specify size");
	  else if (this_nelts == integer_zero_node)
	    {
	      warning ("zero size array reserves no space");
	      nelts = integer_zero_node;
	    }
	  else
	    nelts = build_binary_op (MULT_EXPR, nelts, this_nelts);
	}

      if (last_absdcl)
	TREE_OPERAND (last_absdcl, 0) = absdcl;
      else
	TREE_VALUE (decl) = absdcl;

      type = groktypename (decl);
      if (! type || type == error_mark_node
	  || true_type == error_mark_node)
	return error_mark_node;

      type = TYPE_MAIN_VARIANT (type);
      if (type == void_type_node)
	{
	  error ("invalid type: `void []'");
	  return error_mark_node;
	}
      if (current_function_decl
	  && DECL_CONSTRUCTOR_P (current_function_decl))
	{
	  pending_sizes = get_pending_sizes ();
	  immediate_size_expand = old_immediate_size_expand;
	}
    }
  else if (TREE_CODE (decl) == IDENTIFIER_NODE)
    {
      if (TREE_TYPE (decl))
	{
	  /* An aggregate type.  */
	  decl = TREE_TYPE (decl);
	  type = TREE_TYPE (decl);
	}
      else
	{
	  /* A builtin type.  */
	  decl = lookup_name (decl);
	  assert (TREE_CODE (decl) == TYPE_DECL);
	  type = TREE_TYPE (decl);
	}
      true_type = type;
    }
  else if (TREE_CODE (decl) == TYPE_DECL)
    {
      type = TREE_TYPE (decl);
      true_type = type;
    }
  else
    {
      type = decl;
      true_type = type;
      decl = TYPE_NAME (type);
    }

  if (TYPE_SIZE (type) == 0)
    {
      if (type == void_type_node)
	error ("invalid type for new: `void'");
      else
	incomplete_type_error (0, type);
      return error_mark_node;
    }

  if (TYPE_LANG_SPECIFIC (type) && CLASSTYPE_ABSTRACT_VIRTUALS (type))
    {
      abstract_virtuals_error (NULL_TREE, type);
      return error_mark_node;
    }

  size = size_in_bytes (type);
  if (has_array)
    size = fold (build_binary_op (MULT_EXPR, size, nelts));

  /* If this type has special alignment requirements, deal with them here.  */
  if (TYPE_ALIGN (type) > BITS_PER_WORD)
    {
      alignment = fold (build (MINUS_EXPR, integer_type_node,
			       c_alignof (type), integer_one_node));
      size = fold (build (PLUS_EXPR, integer_type_node, size, alignment));
    }

  if (has_call)
    init = init1;

  if (TREE_CODE (true_type) == ARRAY_TYPE)
    type = TREE_TYPE (true_type);
  else
    type = true_type;

#ifdef SOS
  rval = NULL_TREE;
  if (placement == void_type_node)
    {
      /* Simple "new dynamic" construct.  */
      if (! IS_AGGR_TYPE (type))
	{
	  error ("dynamic new can only allocate objects of aggregate type");
	  return error_mark_node;
	}
      else if (! is_aggr_typedef (DECL_NAME (TYPE_NAME (type)), 1))
	return error_mark_node;
      else
	rval = build_dynamic_new (type, size, NULL_TREE, init);
    }
  else if (placement && TREE_CODE (placement) == STRING_CST)
    {
      /* A "new dynamic" construct with filename argument.  */
      if (! IS_AGGR_TYPE (type))
	{
	  error ("dynamic new can only allocate objects of aggregate type");
	  return error_mark_node;
	}
      else if (! is_aggr_typedef (DECL_NAME (TYPE_NAME (type)), 1))
	return error_mark_node;
      else
	rval = build_dynamic_new (type, size, placement, init);
    }
  if (rval)
    {
      if (alignment)
	{
	  rval = build (PLUS_EXPR, TYPE_POINTER_TO (type), rval, alignment);
	  rval = build (BIT_AND_EXPR, TYPE_POINTER_TO (type),
			rval, build1 (BIT_NOT_EXPR, integer_type_node, alignment));
	}
      goto done;
    }
#endif

  if (has_array)
    {
      if (placement)
	{
	  error ("placement syntax invalid for arrays");
	  return error_mark_node;
	}
      rval = build (CALL_EXPR, build_pointer_type (type),
		    BIN, build_tree_list (NULL_TREE, size), 0);
      if (alignment)
	{
	  rval = build (PLUS_EXPR, TYPE_POINTER_TO (type), rval, alignment);
	  rval = build (BIT_AND_EXPR, TYPE_POINTER_TO (type),
			rval, build1 (BIT_NOT_EXPR, integer_type_node, alignment));
	}
      TREE_CALLS_NEW (rval) = 1;
      TREE_VOLATILE (rval) = 1;
    }
  else
    {
      if (TYPE_LANG_SPECIFIC (type)
	  && (TREE_GETS_NEW (type) && !use_global_new))
	rval = build_opfncall (NEW_EXPR, LOOKUP_NORMAL, TYPE_POINTER_TO (type), size, placement);
      else if (placement)
	{
	  rval = build_opfncall (NEW_EXPR, LOOKUP_GLOBAL|LOOKUP_COMPLAIN, ptr_type_node, size, placement);
	}
      else if (flag_this_is_variable
	       && TYPE_HAS_CONSTRUCTOR (type) && init != void_type_node)
	{
	  if (init == NULL_TREE || TREE_CODE (init) == TREE_LIST)
	    rval = NULL_TREE;
	  else
	    {
	      error ("constructors take parameter lists");
	      return error_mark_node;
	    }
	}
      else
	{
	  rval = build (CALL_EXPR, build_pointer_type (type),
			BIN, build_tree_list (NULL_TREE, size), 0);
	  if (alignment)
	    {
	      rval = build (PLUS_EXPR, TYPE_POINTER_TO (type), rval, alignment);
	      rval = build (BIT_AND_EXPR, TYPE_POINTER_TO (type),
			    rval, build1 (BIT_NOT_EXPR, integer_type_node, alignment));
	    }
	  TREE_CALLS_NEW (rval) = 1;
	  TREE_VOLATILE (rval) = 1;
	}
      /* We've figured out where the allocation is to go.
	 If we're not eliding constructors, then if a constructor
	 is defined, we must go through it.  */
      if ((rval == NULL_TREE || !flag_elide_constructors)
	  && TYPE_HAS_CONSTRUCTOR (type) && init != void_type_node)
	{
	  int flags = LOOKUP_NORMAL;

	  if (rval && TYPE_USES_VIRTUAL_BASECLASSES (type))
	    {
	      init = tree_cons (NULL_TREE, integer_one_node, init);
	      flags |= LOOKUP_HAS_IN_CHARGE;
	    }
	  rval = build_method_call (rval, DECL_NAME (TYPE_NAME (type)), init,
				    NULL_TREE, flags);
	  goto done;
	}
    }
  if (rval == error_mark_node)
    return error_mark_node;
  rval = save_expr (rval);
  TREE_HAS_CONSTRUCTOR (rval) = 1;

  /* Don't call any constructors or do any initialization.  */
  if (init == void_type_node)
    goto done;

  if (TYPE_NEEDS_CONSTRUCTING (type))
    {
      extern tree static_aggregates;

      if (current_function_decl == NULL_TREE)
	{
	  /* In case of static initialization, SAVE_EXPR is good enough.  */
	  init = copy_to_permanent (init);
	  rval = copy_to_permanent (rval);
	  static_aggregates = perm_tree_cons (init, rval, static_aggregates);
	}
      else
	{
	  /* Have to wrap this in RTL_EXPR for two cases:
	     in base or member initialization and if we
	     are a branch of a ?: operator.  Since we
	     can't easily know the latter, just do it always.  */
	  tree xval = make_node (RTL_EXPR);

	  TREE_TYPE (xval) = TREE_TYPE (rval);
	  do_pending_stack_adjust ();
	  start_sequence ();

	  /* As a matter of principle, `start_sequence' should do this.  */
	  emit_note (0, -1);

	  if (has_array)
	    rval = expand_vec_init (decl, rval,
				    build_binary_op (MINUS_EXPR, nelts, integer_one_node),
				    init, 0);
	  else
	    expand_aggr_init (build_indirect_ref (rval, 0), init, 0);

	  do_pending_stack_adjust ();

	  TREE_VOLATILE (xval) = 1;
	  RTL_EXPR_SEQUENCE (xval) = get_insns ();
	  end_sequence ();

	  if (TREE_CODE (rval) == SAVE_EXPR)
	    {
	      /* Errors may cause this to not get evaluated.  */
	      if (SAVE_EXPR_RTL (rval) == 0)
		SAVE_EXPR_RTL (rval) = const0_rtx;
	      RTL_EXPR_RTL (xval) = SAVE_EXPR_RTL (rval);
	    }
	  else
	    {
	      assert (TREE_CODE (rval) == VAR_DECL);
	      RTL_EXPR_RTL (xval) = DECL_RTL (rval);
	    }
	  rval = xval;
	}
    }
  else if (has_call || init)
    {
      if (IS_AGGR_TYPE (type))
	{
	  error_with_aggr_type (type, "no constructor for type `%s'");
	  rval = error_mark_node;
	}
      else
	{
	  /* New 2.0 interpretation: `new int (10)' means
	     allocate an int, and initialize it with 10.  */
	  init = build_c_cast (type, init);
	  rval = build (COMPOUND_EXPR, TREE_TYPE (rval),
			build_modify_expr (build_indirect_ref (rval, 0),
					   NOP_EXPR, init),
			rval);
	  TREE_VOLATILE (rval) = 1;
	}
    }
 done:
  if (pending_sizes)
    rval = build_compound_expr (chainon (pending_sizes,
					 build_tree_list (NULL_TREE, rval)));

  return rval;
}

#ifdef SOS
/* Build a "new dynamic" call for type TYPE.  The size
   of the object we are newing is SIZE.  If "new dynamic" was
   given with an argument, that argument is in NAME.
   PARMS contains the parameters to the constructor.
   The first parameter must be an `ImportRequest *'.

   This is slightly hairy, because we must find the correct
   constructor by hand.  */
static tree
build_dynamic_new (type, size, name, parms)
     tree type, size;
     tree name, parms;
{
  tree import_parms, inner_parms;
  tree import_ptr = integer_zero_node;
  /* This variable is supposed to be the address of a "struct ref"
     object, but how and where should it be defined?  */
  tree lookup_tmp = integer_zero_node;
  tree import_tmp = build_unary_op (ADDR_EXPR, get_temp_name (ptr_type_node, 0), 0);

  if (name)
    {
      inner_parms = tree_cons (NULL_TREE, name,
			       build_tree_list (NULL_TREE, integer_zero_node));
      inner_parms = tree_cons (NULL_TREE, lookup_tmp, inner_parms);
      inner_parms = build_function_call (__sosLookup, inner_parms);
    }
  else
    inner_parms = integer_zero_node;

  import_parms = build_tree_list (NULL_TREE, inner_parms);

  if (CLASSTYPE_DYNAMIC_FILENAME (type))
    {
      inner_parms = tree_cons (NULL_TREE, CLASSTYPE_DYNAMIC_FILENAME (type),
			       build_tree_list (NULL_TREE, integer_zero_node));
      inner_parms = tree_cons (NULL_TREE, lookup_tmp, inner_parms);
      inner_parms = build_function_call (__sosLookup, inner_parms);
    }
  else
    inner_parms = integer_zero_node;

  import_parms = tree_cons (NULL_TREE, inner_parms, import_parms);
  import_parms = tree_cons (NULL_TREE, CLASSTYPE_TYPENAME_AS_STRING (type), import_parms);
  /* This is one parameter which could be (but should not be) evaluated twice.  */
  TREE_VALUE (parms) = save_expr (TREE_VALUE (parms));

  import_parms = tree_cons (NULL_TREE, TREE_VALUE (parms), import_parms);

  /* SOS?? Pass the address of a temporary which can hold the pointer
     to dynamic class table, but how and where is it defined? */
  import_parms = tree_cons (NULL_TREE, import_tmp, import_parms);

  import_ptr = build_function_call (__sosImport, import_parms);

  /* SOS?? Now, generate call to ctor, but using `import_ptr' as the function
     table.  Return the result of the call to the ctor.  */
  import_ptr = build1 (NOP_EXPR, TYPE_POINTER_TO (type), import_ptr);
  return build_method_call (import_ptr, DECL_NAME (TYPE_NAME (type)),
			    tree_cons (NULL_TREE, import_tmp, parms),
			    NULL_TREE, LOOKUP_DYNAMIC);
}

/* Return the name of the link table (as an IDENTIFIER_NODE)
   for the given TYPE.  */
tree
get_linktable_name (type)
     tree type;
{
  char *buf = (char *)alloca (4 + TYPE_NAME_LENGTH (type) + 1);
  tree name;

  assert (TYPE_DYNAMIC (type));
  sprintf (buf, "ZN_%s_", TYPE_NAME_STRING (type));
  return get_identifier (buf);
}

/* For a given type TYPE, grovel for a function table which
   can be used to support dynamic linking.  */
tree
get_sos_dtable (type, parms)
     tree type, parms;
{
  tree classname = CLASSTYPE_TYPENAME_AS_STRING (type);
  tree filename = CLASSTYPE_DYNAMIC_FILENAME (type);
  tree dyn_vtbl;
  /* This variable is supposed to be the address of a "struct ref"
     object, but how and where should it be defined?  */
  tree lookup_tmp = integer_zero_node;

  assert (TYPE_DYNAMIC (type));

  if (filename)
    {
      tree inner_parms = tree_cons (NULL_TREE, filename,
				    build_tree_list (NULL_TREE, integer_zero_node));
      inner_parms = tree_cons (NULL_TREE, lookup_tmp, inner_parms);
      parms = build_tree_list (NULL_TREE, build_function_call (__sosLookup, inner_parms));
    }
  else
    parms = build_tree_list (NULL_TREE, integer_zero_node);

  parms = tree_cons (NULL_TREE, classname, parms);

  dyn_vtbl = build_function_call (__sosFindCode, parms);
  TREE_TYPE (dyn_vtbl) = build_pointer_type (ptr_type_node);
  return dyn_vtbl;
}
#endif

/* `expand_vec_init' performs initialization of a vector of aggregate
   types.

   DECL is passed only for error reporting, and provides line number
   and source file name information.
   BASE is the space where the vector will be.
   MAXINDEX is the maximum index of the array (one less than the
	    number of elements).
   INIT is the (possibly NULL) initializer.

   FROM_ARRAY is 0 if we should init everything with INIT
   (i.e., every element initialized from INIT).
   FROM_ARRAY is 1 if we should index into INIT in parallel
   with initialization of DECL.
   FROM_ARRAY is 2 if we should index into INIT in parallel,
   but use assignment instead of initialization.  */

tree
expand_vec_init (decl, base, maxindex, init, from_array)
     tree decl, base, maxindex, init;
{
  tree rval;
  tree iterator, base2 = NULL_TREE;
  tree type = TREE_TYPE (TREE_TYPE (base));
  tree size;

  maxindex = convert (integer_type_node, maxindex);
  if (maxindex == error_mark_node)
    return error_mark_node;

  if (current_function_decl == NULL_TREE)
    {
      rval = make_tree_vec (3);
      TREE_VEC_ELT (rval, 0) = base;
      TREE_VEC_ELT (rval, 1) = maxindex;
      TREE_VEC_ELT (rval, 2) = init;
      return rval;
    }

  size = size_in_bytes (type);

  /* Set to zero in case size is <= 0.  Optimizer will delete this if
     it is not needed.  */
  rval = get_temp_regvar (TYPE_POINTER_TO (type), null_pointer_node);
  base = default_conversion (base);
  expand_assignment (rval, base, 0, 0);
  base = get_temp_regvar (TYPE_POINTER_TO (type), base);

  if (init != NULL_TREE
      && TREE_CODE (init) == CONSTRUCTOR
      && TREE_TYPE (init) == TREE_TYPE (decl))
    {
      /* Initialization of array from {...}.  */
      tree elts = CONSTRUCTOR_ELTS (init);
      tree baseref = build1 (INDIRECT_REF, type, base);
      tree baseinc = build (PLUS_EXPR, TYPE_POINTER_TO (type), base, size);
      int host_i = TREE_INT_CST_LOW (maxindex);

      if (IS_AGGR_TYPE (type))
	{
	  while (elts)
	    {
	      host_i -= 1;
	      expand_aggr_init (baseref, TREE_VALUE (elts), 0);

	      expand_assignment (base, baseinc, 0, 0);
	      elts = TREE_CHAIN (elts);
	    }
	  /* Initialize any elements by default if possible.  */
	  if (host_i >= 0)
	    {
	      if (TYPE_NEEDS_CONSTRUCTING (type) == 0)
		{
		  if (obey_regdecls)
		    use_variable (DECL_RTL (base));
		  goto done_init;
		}

	      iterator = get_temp_regvar (integer_type_node,
					  build_int_2 (host_i, 0));
	      init = NULL_TREE;
	      goto init_by_default;
	    }
	}
      else
	while (elts)
	  {
	    expand_assignment (baseref, TREE_VALUE (elts), 0, 0);

	    expand_assignment (base, baseinc, 0, 0);
	    elts = TREE_CHAIN (elts);
	  }

      if (obey_regdecls)
	use_variable (DECL_RTL (base));
    }
  else
    {
      iterator = get_temp_regvar (integer_type_node, maxindex);

    init_by_default:

      /* If initializing one array from another,
	 initialize element by element.  */
      if (from_array)
	{
	  if (decl == NULL_TREE
	      || (init && TREE_TYPE (init) != TREE_TYPE (decl)))
	    {
	      sorry ("initialization of array from dissimilar array type");
	      return error_mark_node;
	    }
	  if (init)
	    {
	      base2 = default_conversion (init);
	      base2 = get_temp_regvar (TYPE_POINTER_TO (type), base2);
	    }
	  else if (TYPE_LANG_SPECIFIC (type)
		   && TYPE_NEEDS_CONSTRUCTING (type)
		   && ! TYPE_HAS_DEFAULT_CONSTRUCTOR (type))
	    {
	      error ("initializer ends prematurely");
	      return;
	    }
	}

      expand_start_cond (build (GE_EXPR, integer_type_node,
				iterator, integer_zero_node), 0);
      expand_start_loop_continue_elsewhere (1);

      if (from_array)
	{
	  tree to = build1 (INDIRECT_REF, type, base);
	  tree from;

	  if (base2)
	    from = build1 (INDIRECT_REF, type, base2);
	  else
	    from = NULL_TREE;

	  if (from_array == 2)
	    expand_expr_stmt (build_modify_expr (to, NOP_EXPR, from));
	  else if (TYPE_NEEDS_CONSTRUCTING (type))
	    expand_aggr_init (to, from, 0);
	  else if (from)
	    expand_assignment (to, from, 0, 0);
	  else abort ();
	}
      else if (TREE_CODE (type) == ARRAY_TYPE)
	{
	  if (init != 0)
	    sorry ("cannot initialize multi-dimensional array with initializer");
	  expand_vec_init (decl, build1 (NOP_EXPR, TYPE_POINTER_TO (TREE_TYPE (type)), base),
			   array_type_nelts (type), 0, 0);
	}
      else
	expand_aggr_init (build1 (INDIRECT_REF, type, base), init, 0);

      expand_assignment (base,
			 build (PLUS_EXPR, TYPE_POINTER_TO (type), base, size),
			 0, 0);
      if (base2)
	expand_assignment (base2,
			   build (PLUS_EXPR, TYPE_POINTER_TO (type), base2, size), 0, 0);
      expand_loop_continue_here ();
      expand_exit_loop_if_false (build (NE_EXPR, integer_type_node,
					build (PREDECREMENT_EXPR, integer_type_node, iterator, integer_one_node), minus_one));

      if (obey_regdecls)
	{
	  use_variable (DECL_RTL (base));
	  if (base2)
	    use_variable (DECL_RTL (base2));
	}
      expand_end_loop ();
      expand_end_cond ();
      if (obey_regdecls)
	use_variable (DECL_RTL (iterator));
    }
 done_init:

  if (obey_regdecls)
    use_variable (DECL_RTL (rval));
  return rval;
}

/* Free up storage of type TYPE, at address ADDR.
   TYPE is a POINTER_TYPE.

   This does not call any destructors.  */
tree
build_x_delete (type, addr, use_global_delete)
     tree type, addr;
     int use_global_delete;
{
  tree rval;

  if (!use_global_delete
      && TYPE_LANG_SPECIFIC (TREE_TYPE (type))
      && TREE_GETS_DELETE (TREE_TYPE (type)))
    rval = build_opfncall (DELETE_EXPR, LOOKUP_NORMAL, addr);
  else
    {
      rval = build (CALL_EXPR, void_type_node, BID,
		    build_tree_list (NULL_TREE, addr), 0);
      TREE_VOLATILE (rval) = 1;
    }
  return rval;
}

/* Generate a call to a destructor. TYPE is the type to cast ADDR to.
   ADDR is an expression which yields the store to be destroyed.
   AUTO_DELETE is nonzero if a call to DELETE should be made or not.
   If in the program, (AUTO_DELETE & 2) is non-zero, we tear down the
   virtual baseclasses.
   If in the program, (AUTO_DELETE & 1) is non-zero, then we deallocate.

   FLAGS is the logical disjunction of zero or more LOOKUP_
   flags.  See cplus-tree.h for more info.

   This function does not delete an object's virtual base classes.  */
tree
build_delete (type, addr, auto_delete, flags, use_global_delete)
     tree type, addr;
     tree auto_delete;
     int flags;
     int use_global_delete;
{
  tree function, parms;
  tree member;
  tree expr;
  tree ref;
  int ptr;

  if (addr == error_mark_node)
    return error_mark_node;

  /* Can happen when CURRENT_EXCEPTION_OBJECT gets its type
     set to `error_mark_node' before it gets properly cleaned up.  */
  if (type == error_mark_node)
    return error_mark_node;

  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (type) == POINTER_TYPE)
    {
      type = TREE_TYPE (type);
      if (TYPE_SIZE (type) == 0)
	{
	  incomplete_type_error (0, type);
	  return error_mark_node;
	}
      if (! IS_AGGR_TYPE (type))
	{
	  expr = build (CALL_EXPR, void_type_node,
			BID, build_tree_list (NULL_TREE, addr), 0);
	  TREE_VOLATILE (expr) = 1;
	  return expr;
	}
      if (TREE_VOLATILE (addr))
	addr = save_expr (addr);
      ref = build_indirect_ref (addr, 0);
      ptr = 1;
    }
  else if (TREE_CODE (type) == ARRAY_TYPE)
    {
      if (TREE_VOLATILE (addr))
	addr = save_expr (addr);
      return build_vec_delete (addr, array_type_nelts (type), c_sizeof (TREE_TYPE (type)),
			       NULL_TREE, auto_delete, integer_two_node);
    }
  else
    {
      /* Don't check PROTECT here; leave that decision to the
	 destructor.  If the destructor is visible, call it,
	 else report error.  */
      addr = build_unary_op (ADDR_EXPR, addr, 0);
      if (TREE_VOLATILE (addr))
	addr = save_expr (addr);

      if (TREE_LITERAL (addr))
	addr = convert_pointer_to (type, addr);
      else
	addr = convert_force (build_pointer_type (type), addr);

      if (TREE_CODE (addr) == NOP_EXPR
	  && TREE_OPERAND (addr, 0) == current_class_decl)
	ref = C_C_D;
      else
	ref = build_indirect_ref (addr, 0);
      ptr = 0;
    }

  assert (IS_AGGR_TYPE (type));

  if (! TYPE_NEEDS_DESTRUCTOR (type))
    {
      if (auto_delete == integer_zero_node)
	return build1 (NOP_EXPR, void_type_node, integer_zero_node);
      else if (TREE_GETS_DELETE (type) && !use_global_delete)
	return build_opfncall (DELETE_EXPR, LOOKUP_NORMAL, addr);
      parms = build_tree_list (NULL_TREE, addr);
      expr = build (CALL_EXPR, void_type_node, BID, parms, 0);
      TREE_VOLATILE (expr) = 1;
      return expr;
    }
  parms = build_tree_list (NULL_TREE, addr);

  /* Below, we will reverse the order in which these calls are made.
     If we have a destructor, then that destructor will take care
     of the base classes; otherwise, we must do that here.  */
  if (TYPE_HAS_DESTRUCTOR (type))
    {
      tree field = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (type), 0);
      tree basetypes = CLASSTYPE_AS_LIST (type);

      if (flags & LOOKUP_PROTECT)
	{
	  enum visibility_type visibility = compute_visibility (basetypes, field);

	  if (visibility == visibility_private)
	    {
	      if (flags & LOOKUP_COMPLAIN)
		error_with_aggr_type (type, "destructor for type `%s' is private in this scope");
	      return error_mark_node;
	    }
	  else if (visibility == visibility_protected
		   && (flags & LOOKUP_PROTECTED_OK) == 0)
	    {
	      if (flags & LOOKUP_COMPLAIN)
		error_with_aggr_type (type, "destructor for type `%s' is protected in this scope");
	      return error_mark_node;
	    }
	}

      /* Once we are in a destructor, try not going through
	 the virtual function table to find the next destructor.  */
      if (DECL_VIRTUAL_P (field)
	  && ! (flags & LOOKUP_NONVIRTUAL)
	  && TREE_CODE (auto_delete) != PARM_DECL
	  && (ptr == 1 || ! resolves_to_fixed_type_p (ref)))
	{
	  /* This destructor must be called via virtual function table.  */
	  field = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (DECL_VCONTEXT (field)), 0);
	  expr = convert_pointer_to (DECL_CONTEXT (field), TREE_VALUE (parms));
	  if (expr != TREE_VALUE (parms))
	    {
	      ref = build_indirect_ref (expr, 0);
	      TREE_VALUE (parms) = expr;
	    }
	  function = build_vfn_ref (&TREE_VALUE (parms), ref, DECL_VINDEX (field));
	  if (function == error_mark_node)
	    return error_mark_node;
	  TREE_TYPE (function) = build_pointer_type (TREE_TYPE (field));
	  TREE_CHAIN (parms) = build_tree_list (NULL_TREE, auto_delete);
	  expr = build_function_call (function, parms);
	  if (ptr && (flags & LOOKUP_DESTRUCTOR) == 0)
	    {
	      /* Handle the case where a virtual destructor is
		 being called on an item that is 0.

		 @@ Does this really need to be done?  */
	      tree ifexp = build_binary_op (NE_EXPR, addr, integer_zero_node);
#if 0
	      if (TREE_CODE (ref) == VAR_DECL
		  || TREE_CODE (ref) == COMPONENT_REF)
		warning ("losing in build_delete");
#endif
	      expr = build (COND_EXPR, void_type_node,
			    ifexp, expr,
			    build1 (NOP_EXPR, void_type_node, integer_zero_node));
	    }
	}
      else
	{
	  tree ifexp;

	  if ((flags & LOOKUP_DESTRUCTOR)
	      || TREE_CODE (ref) == VAR_DECL
	      || TREE_CODE (ref) == PARM_DECL
	      || TREE_CODE (ref) == COMPONENT_REF
	      || TREE_CODE (ref) == ARRAY_REF)
	    /* These can't be 0.  */
	    ifexp = integer_one_node;
	  else
	    /* Handle the case where a non-virtual destructor is
	       being called on an item that is 0.  */
	    ifexp = build_binary_op (NE_EXPR, addr, integer_zero_node);

	  function = field;

	  /* Used to mean that this destructor was known to be empty,
	     but that's now obsolete.  */
	  assert (DECL_INITIAL (function) != void_type_node);

	  TREE_CHAIN (parms) = build_tree_list (NULL_TREE, auto_delete);
	  expr = build_function_call (function, parms);

	  if (ifexp != integer_one_node)
	    expr = build (COND_EXPR, void_type_node,
			  ifexp, expr,
			  build1 (NOP_EXPR, void_type_node, integer_zero_node));
	}
      return expr;
    }
  else
    {
      /* This can get visibilties wrong.  */
      int i, n_baseclasses = CLASSTYPE_N_BASECLASSES (type);
      tree basetype = n_baseclasses > 0 ? CLASSTYPE_BASECLASS (type, 1) : NULL_TREE;
      tree exprstmt = NULL_TREE;
      tree parent_auto_delete = auto_delete;
      tree cond;

      /* If this type does not have a destructor, but does have
	 operator delete, call the parent parent destructor (if any),
	 but let this node do the deleting.  Otherwise, it is ok
	 to let the parent destructor do the deleting.  */
      if (TREE_GETS_DELETE (type) && !use_global_delete)
	{
	  parent_auto_delete = integer_zero_node;
	  if (auto_delete == integer_zero_node)
	    cond = NULL_TREE;
	  else
	    {
	      expr = build_opfncall (DELETE_EXPR, LOOKUP_NORMAL, addr);
	      if (expr == error_mark_node)
		return error_mark_node;
	      if (auto_delete != integer_one_node)
		cond = build (COND_EXPR, void_type_node,
			      build (BIT_AND_EXPR, integer_type_node, auto_delete, integer_one_node),
			      expr,
			      build1 (NOP_EXPR, void_type_node, integer_zero_node));
	      else cond = expr;
	    }
	}
      else if (basetype == NULL_TREE
	       || (CLASSTYPE_VIA_VIRTUAL (type, 1) == 0
		   && ! TYPE_NEEDS_DESTRUCTOR (basetype)))
	{
	  cond = build (COND_EXPR, void_type_node,
			build (BIT_AND_EXPR, integer_type_node, auto_delete, integer_one_node),
			build_function_call (BID, build_tree_list (NULL_TREE, addr)),
			build1 (NOP_EXPR, void_type_node, integer_zero_node));
	}
      else cond = NULL_TREE;

      if (cond)
	exprstmt = build_tree_list (NULL_TREE, cond);

      if (basetype
	  && ! CLASSTYPE_VIA_VIRTUAL (type, 1)
	  && TYPE_NEEDS_DESTRUCTOR (basetype))
	{
	  tree this_auto_delete;

	  if (CLASSTYPE_OFFSET (basetype) == integer_zero_node)
	    this_auto_delete = parent_auto_delete;
	  else
	    this_auto_delete = integer_zero_node;

	  expr = build_delete (TYPE_POINTER_TO (basetype), addr,
			       this_auto_delete, flags|LOOKUP_PROTECTED_OK, 0);
	  exprstmt = tree_cons (NULL_TREE, expr, exprstmt);
	}

      for (i = 2; i <= n_baseclasses; i++)
	{
	  basetype = CLASSTYPE_BASECLASS (type, i);
	  if (! TYPE_NEEDS_DESTRUCTOR (basetype)
	      || CLASSTYPE_VIA_VIRTUAL (type, i))
	    continue;

	  /* May be zero offset if other baseclasses are virtual.  */
	  expr = fold (build (PLUS_EXPR, TYPE_POINTER_TO (basetype),
			      addr, CLASSTYPE_OFFSET (basetype)));

	  expr = build_delete (TYPE_POINTER_TO (basetype), expr,
			       integer_zero_node,
			       flags|LOOKUP_PROTECTED_OK, 0);

	  exprstmt = tree_cons (NULL_TREE, expr, exprstmt);
	}

      for (member = TYPE_FIELDS (type); member; member = TREE_CHAIN (member))
	{
	  if (TREE_CODE (member) != FIELD_DECL)
	    continue;
	  if (TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (member)))
	    {
	      tree this_member = build_component_ref (ref, DECL_NAME (member), 0, 0);
	      tree this_type = TREE_TYPE (member);
	      expr = build_delete (this_type, this_member, integer_two_node, flags, 0);
	      exprstmt = tree_cons (NULL_TREE, expr, exprstmt);
	    }
	}

      if (exprstmt)
	return build_compound_expr (exprstmt);
      /* Virtual base classes make this function do nothing.  */
      return build1 (NOP_EXPR, void_type_node, integer_zero_node);
    }
}

/* For type TYPE, delete the virtual baseclass objects of DECL.  */

tree
build_vbase_delete (type, decl)
     tree type, decl;
{
  tree vbases = CLASSTYPE_VBASECLASSES (type);
  tree result = NULL_TREE;
  tree addr = build_unary_op (ADDR_EXPR, decl, 0);
  assert (addr != error_mark_node);
  while (vbases)
    {
      tree this_addr = convert_force (TYPE_POINTER_TO (TREE_VALUE (vbases)), addr);
      result = tree_cons (NULL_TREE,
			  build_delete (TREE_TYPE (this_addr), this_addr,
					integer_zero_node, LOOKUP_NORMAL, 0),
			  result);
      vbases = TREE_CHAIN (vbases);
    }
  return build_compound_expr (nreverse (result));
}

/* Build a C++ vector delete expression.
   MAXINDEX is the number of elements to be deleted.
   ELT_SIZE is the nominal size of each element in the vector.
   BASE is the expression that should yield the store to be deleted.
   DTOR_DUMMY is a placeholder for a destructor.  The library function
   __builtin_vec_delete has a pointer to function in this position.
   This function expands (or synthesizes) these calls itself.
   AUTO_DELETE_VEC says whether the container (vector) should be deallocated.
   AUTO_DELETE say whether each item in the container should be deallocated.

   This also calls delete for virtual baseclasses of elements of the vector.  */
tree
build_vec_delete (base, maxindex, elt_size, dtor_dummy, auto_delete_vec, auto_delete)
     tree base, maxindex, elt_size;
     tree dtor_dummy;
     tree auto_delete_vec, auto_delete;
{
  tree ptype = TREE_TYPE (base);
  tree type;
  tree rval;
  /* Temporary variables used by the loop.  */
  tree iterator, tbase;
  tree size_exp;

  /* This is the body of the loop that implements the deletion of a
     single element, and moves temp variables to next elements.  */
  tree body;

  /* This is the LOOP_STMT that governs the deletetion of the elements.  */
  tree loop;

  /* This is the IF_STMT that governs whether the loop is executed at all.  */
  tree if_stmt;

  /* This is the ELSE_STMT that governs what to do after the loop has run.  */
  tree else_stmt = 0;

  /* This is the LET_STMT which holds the outermost iterator of the
     loop.  It is convenient to set this variable up and test it before
     executing any other code in the loop.  */
  tree block;

  if (TREE_CODE (ptype) == POINTER_TYPE)
    {
      if (maxindex == 0)
	{
	  error ("must specify size for non array type");
	  return error_mark_node;
	}
      maxindex = convert (integer_type_node, maxindex);
      if (maxindex == error_mark_node)
	return error_mark_node;
    }
  else if (TREE_CODE (ptype) == ARRAY_TYPE)
    {
      tree amaxindex = array_type_nelts (ptype);
      int multi = 0;

      maxindex = fold (convert (integer_type_node, maxindex));

      while (1)
	{
	  if (maxindex == error_mark_node || integer_zerop (maxindex))
	    return error_mark_node;
	  ptype = TREE_TYPE (ptype);
	  if (TREE_CODE (ptype) != ARRAY_TYPE)
	    break;

	  if (multi == 0)
	    {
	      /* Convert from 0-based to 1-based indexing.  */
	      maxindex = fold (build (PLUS_EXPR, integer_type_node,
				      maxindex, integer_one_node));
	      multi = 1;
	    }
	  amaxindex = array_type_nelts (ptype);
	  maxindex = fold (build (MULT_EXPR, integer_type_node,
				  fold (build (PLUS_EXPR, integer_type_node,
					       amaxindex, integer_one_node)),
				  maxindex));
	}

      if (multi)
	{
	  /* Convert back to 0-based indexing.  */
	  maxindex = fold (build (MINUS_EXPR, integer_type_node,
				  maxindex, integer_one_node));
	  ptype = TYPE_POINTER_TO (ptype);
	  base = build1 (NOP_EXPR, ptype, build_unary_op (ADDR_EXPR, base, 0));
	}
      else
	{
	  if (amaxindex != 0
	      && (TREE_CODE (maxindex) == INTEGER_CST || TREE_CODE (amaxindex) == INTEGER_CST)
	      && ! tree_int_cst_equal (maxindex, amaxindex))
	    warning ("argument to vector delete disagrees with declared type of array");
	  base = default_conversion (base);
	  ptype = TREE_TYPE (base);
	}
    }
  else
    {
      error ("type to vector delete is neither pointer or array type");
      return error_mark_node;
    }
  type = TREE_TYPE (ptype);

  if (! IS_AGGR_TYPE (type) || ! TYPE_NEEDS_DESTRUCTOR (type))
    {
      if (extra_warnings)
	warning ("array size expression for delete ignored");
      rval = build (CALL_EXPR, void_type_node,
		    BID, build_tree_list (NULL_TREE, base), 0);
      TREE_VOLATILE (rval) = 1;
      return rval;
    }

  size_exp = size_in_bytes (type);
  iterator = build_decl (VAR_DECL, NULL_TREE, integer_type_node);
  TREE_REGDECL (iterator) = 1;
  DECL_INITIAL (iterator) = maxindex;
  tbase = build_decl (VAR_DECL, NULL_TREE, ptype);
  TREE_REGDECL (tbase) = 1;
  DECL_INITIAL (tbase) = build (PLUS_EXPR, ptype, base,
				build (MULT_EXPR, integer_type_node, size_exp,
				       build (PLUS_EXPR, integer_type_node, maxindex, integer_one_node)));

  block = build_let (input_filename, lineno, iterator, 0, 0, 0);
  TREE_TYPE (block) = void_type_node;

  if (auto_delete != integer_zero_node
      && auto_delete != integer_two_node)
    {
      body = build_tree_list (NULL_TREE,
			      build (CALL_EXPR, void_type_node,
				     BID, build_tree_list (NULL_TREE, base), 0));
      body = build (COND_EXPR, void_type_node,
		    build (BIT_AND_EXPR, integer_type_node,
			   auto_delete, integer_one_node),
		    body, integer_zero_node);
    }
  else
    body = NULL_TREE;

  body = tree_cons (NULL_TREE,
		    build_delete (ptype, tbase, auto_delete, LOOKUP_NORMAL, 0),
		    body);

  body = tree_cons (NULL_TREE,
		    build_modify_expr (tbase, NOP_EXPR, build (MINUS_EXPR, ptype, tbase, size_exp)),
		    body);

  loop = build_loop (input_filename, lineno,
		     tbase,
		     build (NE_EXPR, integer_type_node,
			    build (PREDECREMENT_EXPR, integer_type_node,
				   iterator, integer_one_node),
			    minus_one),
		     build_compound_expr (body));
  TREE_TYPE (loop) = void_type_node;
  TREE_VOLATILE (loop) = 1;

  /* ELSE_STMT holds the code which deletes the container.
     If you don't want to delete the container, make ELSE_STMT
     zero here.  */
  if (auto_delete_vec == integer_one_node)
    else_stmt = build_delete (ptr_type_node, base, integer_one_node, 0, 1);
  else if (auto_delete_vec != integer_zero_node
	   && auto_delete_vec != integer_two_node)
    {
      else_stmt = build (COND_EXPR, void_type_node,
			 build (BIT_AND_EXPR, integer_type_node, auto_delete_vec, integer_one_node),
			 build_delete (ptr_type_node, base, integer_one_node, 0, 1),
			 integer_zero_node);
      TREE_VOLATILE (else_stmt) = 1;
    }
  else
    else_stmt = 0;

  if_stmt = build_if (input_filename, lineno,
		      build (GE_EXPR, integer_type_node,
			     iterator, integer_zero_node),
		      loop, else_stmt);

  TREE_TYPE (if_stmt) = void_type_node;
  TREE_VOLATILE (if_stmt) = 1;

  /* BASE may be a SAVE_EXPR which must be evaluated before either
     branch of the IF/THEN/ELSE.  */
  STMT_BODY (block) = build (COMPOUND_EXPR, void_type_node, base, if_stmt);
  TREE_VOLATILE (block) = 1;

  return block;
}
