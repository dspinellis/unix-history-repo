/* Build expressions with type checking for C compiler.
   Copyright (C) 1987, 1988, 1989 Free Software Foundation, Inc.
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


/* This file is part of the C front end.
   It contains routines to build C expressions given their operands,
   including computing the types of the result, C-specific error checks,
   and some optimization.

   There are also routines to build RETURN_STMT nodes and CASE_STMT nodes,
   and to process initializations in declarations (since they work
   like a strange sort of assignment).  */

extern void error ();
extern void warning ();

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "cplus-tree.h"
#include "flags.h"
#include "assert.h"

#include "obstack.h"
#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free

extern int xmalloc ();
extern void free ();

extern struct obstack *current_obstack;

int mark_addressable ();
static tree convert_for_assignment ();
/* static */ tree convert_for_initialization ();
int compparms ();
int compparms1 ();
int comp_target_types ();
static tree shorten_compare ();
static void binary_op_error ();
static tree pointer_int_sum ();
static tree pointer_diff ();
static tree convert_sequence ();
/* static */ tree unary_complex_lvalue ();
tree truthvalue_conversion ();
static tree invert_truthvalue ();
extern void readonly_warning_or_error ();

static void
message_2_types (pfn, s, type1, type2)
     void (*pfn) ();
     char *s;
     tree type1, type2;
{
  tree name1 = TYPE_NAME (type1);
  tree name2 = TYPE_NAME (type2);
  if (TREE_CODE (name1) == TYPE_DECL)
    name1 = DECL_NAME (name1);
  if (TREE_CODE (name2) == TYPE_DECL)
    name2 = DECL_NAME (name2);
  (*pfn) (s, IDENTIFIER_POINTER (name1), IDENTIFIER_POINTER (name2));
}

/* Return the _TYPE node describing the data type
   of the data which NODE represents as a C expression.
   Arrays and functions are converted to pointers
   just as they are when they appear as C expressions.

   C++: Member types are converted to the data
   type of the member they are.  */

tree
datatype (node)
     tree node;
{
  register tree type = TREE_TYPE (node);
  if (TREE_CODE (type) == ARRAY_TYPE)
    return TYPE_POINTER_TO (TREE_TYPE (type));
  if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    return build_pointer_type (type);
  if (TREE_CODE (type) == OFFSET_TYPE)
    return datatype (type);
  return type;
}

/* Return the target type of TYPE, which meas return T for:
   T*, T&, T[], T (...), and otherwise, just T.  */
tree
target_type (type)
     tree type;
{
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);
  while (TREE_CODE (type) == POINTER_TYPE
	 || TREE_CODE (type) == ARRAY_TYPE
	 || TREE_CODE (type) == FUNCTION_TYPE
	 || TREE_CODE (type) == METHOD_TYPE
	 || TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);
  return type;
}

/* Do `exp = require_complete_type (exp);' to make sure exp
   does not have an incomplete type.  (That includes void types.)  */

tree
require_complete_type (value)
     tree value;
{
  tree type = TREE_TYPE (value);

  /* First, detect a valid value with a complete type.  */
  if (TYPE_SIZE (type) != 0
      && type != void_type_node)
    return value;

  /* If we see X::Y, we build an OFFSET_TYPE which has
     not been laid out.  Try to avoid an error by interpreting
     it as this->X::Y, if reasonable.  */
  if (TREE_CODE (value) == OFFSET_REF
      && C_C_D != 0
      && TREE_OPERAND (value, 0) == C_C_D)
    {
      tree base, member = TREE_OPERAND (value, 1);
      tree basetype = TYPE_OFFSET_BASETYPE (type);
      assert (TREE_CODE (member) == FIELD_DECL);
      base = convert_pointer_to (basetype, current_class_decl);
      value = build (COMPONENT_REF, TREE_TYPE (TREE_OPERAND (value, 1)),
		     build_indirect_ref (base, 0),
		     TREE_OPERAND (value, 1));
      return require_complete_type (value);
    }

  incomplete_type_error (value, type);
  return error_mark_node;
}

/* Return truthvalue of whether type of EXP is instantiated.  */
int
type_unknown_p (exp)
     tree exp;
{
  return (TREE_TYPE (exp) == unknown_type_node
	  || (TREE_CODE (TREE_TYPE (exp)) == OFFSET_TYPE
	      && TREE_TYPE (TREE_TYPE (exp)) == unknown_type_node));
}

/* Do `exp = require_instantiated_type (type, exp);' to make sure EXP
   does not have an uninstantiated type.
   TYPE is type to instantiate with, if uninstantiated.  */
tree
require_instantiated_type (type, exp, errval)
     tree type, exp, errval;
{
  if (TREE_TYPE (exp) == unknown_type_node
      || (TREE_CODE (TREE_TYPE (exp)) == OFFSET_TYPE
	  && TREE_TYPE (TREE_TYPE (exp)) == unknown_type_node))
    {
      exp = instantiate_type (type, exp, 1);
      if (TREE_TYPE (exp) == error_mark_node)
	return errval;
    }
  return exp;
}

/* Return a variant of TYPE which has all the type qualifiers of LIKE
   as well as those of TYPE.  */

static tree
qualify_type (type, like)
     tree type, like;
{
  int constflag = TREE_READONLY (type) || TREE_READONLY (like);
  int volflag = TREE_VOLATILE (type) || TREE_VOLATILE (like);
  /* @@ Must do member pointers here.  */
  return build_type_variant (type, constflag, volflag);
}

/* Return the common type of two parameter lists.

   As an optimization, free the space we allocate if the parameter
   lists are already common.  */

tree
commonparms (p1, p2)
     tree p1, p2;
{
  tree oldargs = p1, newargs, n;
  int i, len;
  int any_change = 0;
  char *first_obj = (char *)obstack_alloc (current_obstack, 0);

  len = list_length (p1);
  newargs = 0;

  for (i = 0; i < len; i++)
    newargs = tree_cons (0, 0, newargs);

  n = newargs;

  for (i = 0; p1;
       p1 = TREE_CHAIN (p1), p2 = TREE_CHAIN (p2), n = TREE_CHAIN (n), i++)
    {
      if (TREE_PURPOSE (p1) && !TREE_PURPOSE (p2))
	{
#if 0
	  if (! any_warning)
	    {
	      warning ("default argument given in prototype and not in declaration of function");
	      any_warning++;
	    }
#endif
	  TREE_PURPOSE (p2) = TREE_PURPOSE (p1);
	  any_change = 1;
	}
      else if (! TREE_PURPOSE (p1))
	{
	  if (TREE_PURPOSE (p2))
	    any_change = 1;
	}
      else
	{
	  int cmp = simple_cst_equal (TREE_PURPOSE (p1), TREE_PURPOSE (p2));
	  if (cmp < 0)
	    abort ();
	  if (cmp == 0)
	    {
	      error ("redeclaration of default argument %d", i);
	      any_change = 1;
	    }
	}
      TREE_PURPOSE (n) = TREE_PURPOSE (p2);
      if (TREE_VALUE (p1) != TREE_VALUE (p2))
	{
	  any_change = 1;
	  TREE_VALUE (n) = commontype (TREE_VALUE (p1), TREE_VALUE (p2));
	}
      else
	TREE_VALUE (n) = TREE_VALUE (p1);
    }
  if (! any_change)
    {
      obstack_free (current_obstack, first_obj);
      return oldargs;
    }

  return newargs;
}

/* Return the common type of two types.
   We assume that comptypes has already been done and returned 1;
   if that isn't so, this may crash.

   This is the type for the result of most arithmetic operations
   if the operands have the given two types.

   We do not deal with enumeral types here because they have already been
   converted to integer types.  */

tree
commontype (t1, t2)
     tree t1, t2;
{
  register enum tree_code form1;
  register enum tree_code form2;

  /* Save time if the two types are the same.  */

  if (t1 == t2) return t1;

  /* Treat an enum type as the unsigned integer type of the same width.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = type_for_size (TYPE_PRECISION (t2), 1);

  form1 = TREE_CODE (t1);
  form2 = TREE_CODE (t2);

  switch (form1)
    {
    case INTEGER_TYPE:
    case REAL_TYPE:
      /* If only one is real, use it as the result.  */

      if (form1 == REAL_TYPE && form2 != REAL_TYPE)
	return t1;

      if (form2 == REAL_TYPE && form1 != REAL_TYPE)
	return t2;

      /* Both real or both integers; use the one with greater precision.  */

      if (TYPE_PRECISION (t1) > TYPE_PRECISION (t2))
	return t1;
      else if (TYPE_PRECISION (t2) > TYPE_PRECISION (t1))
	return t2;

      /* Same precision.  Prefer longs to ints even when same size.  */

      if (t1 == long_unsigned_type_node
	  || t2 == long_unsigned_type_node)
	return long_unsigned_type_node;

      if (t1 == long_integer_type_node
	  || t2 == long_integer_type_node)
	{
	  /* But preserve unsignedness from the other type,
	     since long cannot hold all the values of an unsigned int.  */
	  if (TREE_UNSIGNED (t1) || TREE_UNSIGNED (t2))
	    return long_unsigned_type_node;
	  return long_integer_type_node;
	}

      /* Otherwise prefer the unsigned one.  */

      if (TREE_UNSIGNED (t1))
	return t1;
      else return t2;

#if 1
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      /* For two pointers, do this recursively on the target type,
	 and combine the qualifiers of the two types' targets.  */
      {
	tree target = commontype (TYPE_MAIN_VARIANT (TREE_TYPE (t1)),
				  TYPE_MAIN_VARIANT (TREE_TYPE (t2)));
	int constp
	  = TREE_READONLY (TREE_TYPE (t1)) || TREE_READONLY (TREE_TYPE (t2));
	int volatilep
	  = TREE_VOLATILE (TREE_TYPE (t1)) || TREE_VOLATILE (TREE_TYPE (t2));
	target = build_type_variant (target, constp, volatilep);
	if (form1 == POINTER_TYPE)
	  return build_pointer_type (target);
	else
	  return build_reference_type (target);
      }
#else
    case POINTER_TYPE:
      return build_pointer_type (commontype (TREE_TYPE (t1), TREE_TYPE (t2)));

    case REFERENCE_TYPE:
      return build_reference_type (commontype (TREE_TYPE (t1), TREE_TYPE (t2)));
#endif

    case ARRAY_TYPE:
      {
	tree elt = commontype (TREE_TYPE (t1), TREE_TYPE (t2));
	/* Save space: see if the result is identical to one of the args.  */
	if (elt == TREE_TYPE (t1) && TYPE_DOMAIN (t1))
	  return t1;
	if (elt == TREE_TYPE (t2) && TYPE_DOMAIN (t2))
	  return t2;
	/* Merge the element types, and have a size if either arg has one.  */
	return build_array_type (elt, TYPE_DOMAIN (TYPE_DOMAIN (t1) ? t1 : t2));
      }

    case FUNCTION_TYPE:
      /* Function types: prefer the one that specified arg types.
	 If both do, merge the arg types.  Also merge the return types.  */
      {
	tree valtype = commontype (TREE_TYPE (t1), TREE_TYPE (t2));
	tree p1 = TYPE_ARG_TYPES (t1);
	tree p2 = TYPE_ARG_TYPES (t2);
	tree rval, raises;

	/* Save space: see if the result is identical to one of the args.  */
	if (valtype == TREE_TYPE (t1) && ! TYPE_ARG_TYPES (t2))
	  return t1;
	if (valtype == TREE_TYPE (t2) && ! TYPE_ARG_TYPES (t1))
	  return t2;

	/* Simple way if one arg fails to specify argument types.  */
	if (TYPE_ARG_TYPES (t1) == 0)
	  {
	    rval = build_function_type (valtype, TYPE_ARG_TYPES (t2));
	    if (raises = TYPE_RAISES_EXCEPTIONS (t2))
	      rval = build_exception_variant (NULL_TREE, rval, raises);
	    return rval;
	  }
	raises = TYPE_RAISES_EXCEPTIONS (t1);
	if (TYPE_ARG_TYPES (t2) == 0)
	  {
	    rval = build_function_type (valtype, TYPE_ARG_TYPES (t1));
	    if (raises)
	      rval = build_exception_variant (NULL_TREE, rval, raises);
	    return rval;
	  }

	/* If both args specify argument types, we must merge the two
	   lists, argument by argument.  */
	rval = build_function_type (valtype, commonparms (p1, p2));
	return build_exception_variant (NULL_TREE, rval, raises);
      }

    case RECORD_TYPE:
    case UNION_TYPE:
      assert (TYPE_MAIN_VARIANT (t1) == t1 && TYPE_MAIN_VARIANT (t2) == t2);

      if (basetype_or_else (t1, t2))
	return t1;
      compiler_error ("commontype called with uncommon aggregate types");
      return t1;

    case METHOD_TYPE:
      if (TYPE_METHOD_BASETYPE (t1) == TYPE_METHOD_BASETYPE (t2)
	  && TREE_CODE (TREE_TYPE (t1)) == TREE_CODE (TREE_TYPE (t2)))
	{
	  /* Get this value the long way, since TYPE_METHOD_BASETYPE
	     is just the main variant of this.  */
	  tree basetype = TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t1)));
	  tree raises, t3;

	  raises = TYPE_RAISES_EXCEPTIONS (t1);

	  /* If this was a member function type, get back to the
	     original type of type member function (i.e., without
	     the class instance variable up front.  */
	  t1 = build_function_type (TREE_TYPE (t1), TREE_CHAIN (TYPE_ARG_TYPES (t1)));
	  t2 = build_function_type (TREE_TYPE (t2), TREE_CHAIN (TYPE_ARG_TYPES (t2)));
	  t3 = commontype (t1, t2);
	  t3 = build_cplus_method_type (basetype, TREE_TYPE (t3), TYPE_ARG_TYPES (t3));
	  return build_exception_variant (basetype, t3, raises);
	}
      compiler_error ("commontype called with uncommon method types");
      return t1;

    case OFFSET_TYPE:
      if (TYPE_OFFSET_BASETYPE (t1) == TYPE_OFFSET_BASETYPE (t2)
	  && TREE_CODE (TREE_TYPE (t1)) == TREE_CODE (TREE_TYPE (t2)))
	{
	  tree basetype = TYPE_OFFSET_BASETYPE (t1);
	  return build_member_type (basetype,
				    commontype (TREE_TYPE (t1), TREE_TYPE (t2)));
	}
      compiler_error ("commontype called with uncommon member types");
      return t1;

    default:
      return t1;
    }
}

/* Return 1 if TYPE1 and TYPE2 raise the same exceptions.  */
int
compexcepttypes (t1, t2, strict)
     tree t1, t2;
     int strict;
{
  return TYPE_RAISES_EXCEPTIONS (t1) == TYPE_RAISES_EXCEPTIONS (t2);
}

static int
comp_array_types (cmp, t1, t2, strict)
     register int (*cmp)();
     tree t1, t2;
{
  tree d1 = TYPE_DOMAIN (t1);
  tree d2 = TYPE_DOMAIN (t2);

  if (!(TREE_TYPE (t1) == TREE_TYPE (t2)
	|| (*cmp) (TREE_TYPE (t1), TREE_TYPE (t2), strict)))
    return 0;

  /* Sizes must match unless one is missing or variable.  */
  if (d1 == 0 || d2 == 0 || d1 == d2
      || TREE_CODE (TYPE_MIN_VALUE (d1)) != INTEGER_CST
      || TREE_CODE (TYPE_MIN_VALUE (d2)) != INTEGER_CST
      || TREE_CODE (TYPE_MAX_VALUE (d1)) != INTEGER_CST
      || TREE_CODE (TYPE_MAX_VALUE (d2)) != INTEGER_CST)
    return 1;

  return ((TREE_INT_CST_LOW (TYPE_MIN_VALUE (d1))
	   == TREE_INT_CST_LOW (TYPE_MIN_VALUE (d2)))
	  && (TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d1))
	      == TREE_INT_CST_HIGH (TYPE_MIN_VALUE (d2)))
	  && (TREE_INT_CST_LOW (TYPE_MAX_VALUE (d1))
	      == TREE_INT_CST_LOW (TYPE_MAX_VALUE (d2)))
	  && (TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d1))
	      == TREE_INT_CST_HIGH (TYPE_MAX_VALUE (d2))));
}

/* Return 1 if TYPE1 and TYPE2 are compatible types for assignment
   or various other operations.  This is what ANSI C speaks of as
   "being the same".

   For C++: argument STRICT says we should be strict about this
   comparison:

	1 : strict (compared according to ANSI C)
	0 : <= (compared according to C++)
	-1: <= or >= (relaxed)

   Otherwise, pointers involving base classes and derived classes
   can be mixed as legal: i.e. a pointer to a base class may be assigned
   to a pointer to one of its derived classes, as per C++. A pointer to
   a derived class may be passed as a paramter to a function expecting a
   pointer to a base classes. These allowances do not commute. In this
   case, TYPE1 is assumed to be the base class, and TYPE2 is assumed to
   be the derived class.  */
int
comptypes (type1, type2, strict)
     tree type1, type2;
     int strict;
{
  register tree t1 = type1;
  register tree t2 = type2;

  /* Suppress errors caused by previously reported errors */

  if (t1 == t2)
    return 1;
  /* This should never happen.  */
  assert (t1 != error_mark_node);

  /* We don't want this to happen.  */
  if (t2 == error_mark_node)
    {
      warning ("t2 == error_mark_node in `comptypes'");
      return 0;
    }

  /* Treat an enum type as the unsigned integer type of the same width.  */

  if (TREE_CODE (t1) == ENUMERAL_TYPE)
    t1 = type_for_size (TYPE_PRECISION (t1), 1);
  if (TREE_CODE (t2) == ENUMERAL_TYPE)
    t2 = type_for_size (TYPE_PRECISION (t2), 1);

  if (t1 == t2)
    return 1;

  /* Different classes of types can't be compatible.  */

  if (TREE_CODE (t1) != TREE_CODE (t2)) return 0;

  /* Qualifiers must match.  */

  if (TREE_READONLY (t1) != TREE_READONLY (t2))
    return 0;
  if (TREE_THIS_VOLATILE (t1) != TREE_THIS_VOLATILE (t2))
    return 0;

  switch (TREE_CODE (t1))
    {
    case RECORD_TYPE:
    case UNION_TYPE:
      if (t1 == t2)
	return 1;
      if (strict <= 0)
	goto look_hard;
      return 0;

    case OFFSET_TYPE:
      return (comptypes (TYPE_POINTER_TO (TYPE_OFFSET_BASETYPE (t1)),
			 TYPE_POINTER_TO (TYPE_OFFSET_BASETYPE (t2)), strict)
	      && comptypes (TREE_TYPE (t1), TREE_TYPE (t2), strict));

    case METHOD_TYPE:
      if (! compexcepttypes (t1, t2, strict))
	return 0;

      /* This case is anti-symmetrical!
	 One can pass a base member (or member function)
	 to something expecting a derived member (or member function),
	 but not vice-versa!  */

      return (comptypes (TYPE_POINTER_TO (TYPE_METHOD_BASETYPE (t2)),
			 TYPE_POINTER_TO (TYPE_METHOD_BASETYPE (t1)), strict)
	      && comptypes (TREE_TYPE (t1), TREE_TYPE (t2), strict)
	      && compparms (TREE_CHAIN (TYPE_ARG_TYPES (t1)),
			    TREE_CHAIN (TYPE_ARG_TYPES(t2)), strict));
    case POINTER_TYPE:
    case REFERENCE_TYPE:
      t1 = TREE_TYPE (t1);
      t2 = TREE_TYPE (t2);
      if (t1 == t2)
	return 1;
      if (strict <= 0)
	{
	  if (IS_AGGR_TYPE (t1) && IS_AGGR_TYPE (t2))
	    {
	      int rval;
	    look_hard:
	      rval = t1 == t2 || get_base_distance (t1, t2, 0, 0) >= 0;

	      if (rval)
		return 1;
	      if (strict < 0)
		return (get_base_type (t2, t1, 0) != 0);
	    }
	  return 0;
	}
      else
	return comptypes (t1, t2, strict);

    case FUNCTION_TYPE:
      if (! compexcepttypes (t1, t2, strict))
	return 0;

      return ((TREE_TYPE (t1) == TREE_TYPE (t2)
	       || comptypes (TREE_TYPE (t1), TREE_TYPE (t2), strict))
	      && compparms (TYPE_ARG_TYPES (t1), TYPE_ARG_TYPES (t2), strict));

    case ARRAY_TYPE:
      /* Target types must match incl. qualifiers.  */
      return comp_array_types (&comptypes, t1, t2, strict);

    }
  return 0;
}

/* Return 1 if TTL and TTR are pointers to types that are equivalent,
   ignoring their qualifiers.

   NPTRS is the number of pointers we can strip off and keep cool.
   This is used to permit (for aggr A, aggr B) A, B* to convert to A*,
   but to not permit B** to convert to A**.  */

int
comp_target_types (ttl, ttr, nptrs)
     tree ttl, ttr;
     int nptrs;
{
  ttl = TYPE_MAIN_VARIANT (ttl);
  ttr = TYPE_MAIN_VARIANT (ttr);
  if (ttl == ttr)
    return 1;

  if (TREE_CODE (ttr) != TREE_CODE (ttl))
    return 0;

  if (TREE_CODE (ttr) == POINTER_TYPE)
    return comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs - 1);

  if (TREE_CODE (ttr) == REFERENCE_TYPE)
    return comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs);
  if (TREE_CODE (ttr) == ARRAY_TYPE)
    return comp_array_types (&comp_target_types, ttl, ttr, 0);
  else if (TREE_CODE (ttr) == FUNCTION_TYPE || TREE_CODE (ttr) == METHOD_TYPE)
    if (comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs))
      switch (comp_target_parms (TYPE_ARG_TYPES (ttl), TYPE_ARG_TYPES (ttr), -1))
	{
	case 0:
	  return 0;
	case 1:
	  return 1;
	case 2:
	  warning ("contravariance violation for method types ignored");
	  return 1;
	default:
	  abort ();
	}
    else
      return 0;

  /* for C++ */
  else if (TREE_CODE (ttr) == OFFSET_TYPE)
    {
      /* Contravariance: we can assign a pointer to base member to a pointer
	 to derived member.  Note difference from simple pointer case, where
	 we can pass a pointer to derived to a pointer to base.  */
      if (comptypes (TYPE_OFFSET_BASETYPE (ttr), TYPE_OFFSET_BASETYPE (ttl), 0))
	return comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs);
      else if (comptypes (TYPE_OFFSET_BASETYPE (ttl), TYPE_OFFSET_BASETYPE (ttr), 0)
	       && comp_target_types (TREE_TYPE (ttl), TREE_TYPE (ttr), nptrs))
	{
	  warning ("contravariance violation for member types ignored");
	  return 1;
	}
    }
  else if (IS_AGGR_TYPE (ttl))
    {
      if (nptrs < 0)
	return 0;
      return comptypes (TYPE_POINTER_TO (ttl), TYPE_POINTER_TO (ttr), 0);
    }

  return 0;
}

/* If two types share a common base type, return that basetype.
   There is not a unique most-derived base type, this function
   returns ERROR_MARK_NODE.  */
tree
common_base_type (tt1, tt2)
     tree tt1, tt2;
{
  tree first = NULL_TREE, second = NULL_TREE, tmp;
  int i;

  for (i = CLASSTYPE_N_BASECLASSES (tt1); i > 0; i--)
    {
      tree basetype = CLASSTYPE_BASECLASS (tt1, i);
      switch (get_base_distance (basetype, tt2, 0, 0))
	{
	case -1:
	  tmp = common_base_type (basetype, tt2);
	  if (tmp == NULL_TREE)
	    break;
	  if (tmp == error_mark_node)
	    return error_mark_node;
	  if (first != NULL_TREE)
	    return error_mark_node;
	  first = tmp;
	  break;

	case -2:
	  first = error_mark_node;
	  break;

	default:
	  if (first != NULL_TREE)
	    return error_mark_node;
	  first = CLASSTYPE_BASECLASS (tt1, i);
	  break;
	}
    }

  for (i = CLASSTYPE_N_BASECLASSES (tt2); i > 0; i--)
    {
      tree basetype = CLASSTYPE_BASECLASS (tt2, i);
      switch (get_base_distance (basetype, tt1, 0, 0))
	{
	case -1:
	  tmp = common_base_type (basetype, tt1);
	  if (tmp == NULL_TREE)
	    break;
	  if (tmp == error_mark_node)
	    return error_mark_node;
	  if (second != NULL_TREE)
	    return error_mark_node;
	  second = tmp;
	  break;

	case -2:
	  second = error_mark_node;
	  break;

	default:
	  if (second != NULL_TREE)
	    return error_mark_node;
	  second = CLASSTYPE_BASECLASS (tt2, i);
	  break;
	}
    }

  if (first != NULL_TREE
      && second != NULL_TREE
      && TYPE_MAIN_VARIANT (first) == TYPE_MAIN_VARIANT (second))
    return first;
  if (first)
    return first;
  if (second)
    return second;

  return NULL_TREE;
}

/* Subroutines of `comptypes'.  */

/* Return 1 if two parameter type lists PARMS1 and PARMS2
   are equivalent in the sense that functions with those parameter types
   can have equivalent types.
   If either list is empty, we win.
   Otherwise, the two lists must be equivalent, element by element.

   C++: See comment above about TYPE1, TYPE2, STRICT.  */
int
compparms (parms1, parms2, strict)
     tree parms1, parms2;
     int strict;
{
  register tree t1 = parms1, t2 = parms2;

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */

  if (t1 == 0)
    return compparms1 (t2);
  if (t2 == 0)
    return compparms1 (t1);

  while (1)
    {
      if (t1 == 0 && t2 == 0)
	return 1;
      /* If one parmlist is shorter than the other,
	 they fail to match, unless STRICT is <= 0.  */
      if (t1 == 0 || t2 == 0)
	{
	  if (strict > 0)
	    return 0;
	  if (strict < 0)
	    return 1;
	  if (strict == 0)
	    return t1 && TREE_PURPOSE (t1);
	  return ((t1 && TREE_PURPOSE (t1)) || TREE_PURPOSE (t2));
	}
      if (! comptypes (TREE_VALUE (t2), TREE_VALUE (t1), strict))
	{
	  if (strict > 0)
	    return 0;
	  if (strict == 0)
	    return t2 == void_list_node && TREE_PURPOSE (t1);
	  return TREE_PURPOSE (t1) || TREE_PURPOSE (t2);
	}
      if (TREE_PURPOSE (t1) && TREE_PURPOSE (t2))
	{
	  int cmp = simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2));
	  if (cmp < 0)
	    abort ();
	  if (cmp == 0)
	    return 0;
	}

      t1 = TREE_CHAIN (t1);
      t2 = TREE_CHAIN (t2);
    }
}

/* This really wants return whether or not parameter type lists
   would make their owning functions assignment compatible or not.  */
int
comp_target_parms (parms1, parms2, strict)
     tree parms1, parms2;
     int strict;
{
  register tree t1 = parms1, t2 = parms2;
  int warn_contravariance = 0;

  /* An unspecified parmlist matches any specified parmlist
     whose argument types don't need default promotions.  */

  if (t1 == 0)
    return compparms1 (t2);
  if (t2 == 0)
    return compparms1 (t1);

  for (; t1 || t2; t1 = TREE_CHAIN (t1), t2 = TREE_CHAIN (t2))
    {
      tree p1, p2;

      /* If one parmlist is shorter than the other,
	 they fail to match, unless STRICT is <= 0.  */
      if (t1 == 0 || t2 == 0)
	{
	  if (strict > 0)
	    return 0;
	  if (strict < 0)
	    return 1 + warn_contravariance;
	  return ((t1 && TREE_PURPOSE (t1)) + warn_contravariance);
	}
      p1 = TREE_VALUE (t1);
      p2 = TREE_VALUE (t2);
      if (p1 == p2)
	continue;
      if ((TREE_CODE (p1) == POINTER_TYPE && TREE_CODE (p2) == POINTER_TYPE)
	  || (TREE_CODE (p1) == REFERENCE_TYPE && TREE_CODE (p2) == REFERENCE_TYPE))
	{
	  /* The following is wrong for contravariance,
	     but many programs depend on it.  */
	  if (TREE_TYPE (p1) == void_type_node)
	    {
	      warn_contravariance = 1;
	      continue;
	    }
	  if (TREE_TYPE (p2) == void_type_node)
	    continue;
	  if (IS_AGGR_TYPE (TREE_TYPE (p1)))
	    {
	      if (comptypes (p2, p1, 0) == 0)
		{
		  if (comptypes (p1, p2, 0) != 0)
		    warn_contravariance = 1;
		  else
		    return 0;
		}
	      continue;
	    }
	}
      /* Note backwards order due to contravariance.  */
      if (comp_target_types (p2, p1, 1) == 0)
	{
	  if (comp_target_types (p1, p2, 1))
	    {
	      warn_contravariance = 1;
	      continue;
	    }
	  if (strict > 0)
	    return 0;
#if 0
	  /* What good do these cases do?  */
	  if (strict == 0)
	    return p2 == void_type_node && TREE_PURPOSE (t1);
	  return TREE_PURPOSE (t1) || TREE_PURPOSE (t2);
#endif
	}
      /* Target types are compatible--just make sure that if
	 we use parameter lists, that they are ok as well.  */
      if (TREE_CODE (p1) == FUNCTION_TYPE || TREE_CODE (p1) == METHOD_TYPE)
	switch (comp_target_parms (TYPE_ARG_TYPES (p1), TYPE_ARG_TYPES (p2), strict))
	  {
	  case 0:
	    return 0;
	  case 1:
	    break;
	  case 2:
	    warn_contravariance = 1;
	  }

      if (TREE_PURPOSE (t1) && TREE_PURPOSE (t2))
	{
	  int cmp = simple_cst_equal (TREE_PURPOSE (t1), TREE_PURPOSE (t2));
	  if (cmp < 0)
	    abort ();
	  if (cmp == 0)
	    return 0;
	}
    }
  return 1 + warn_contravariance;
}

/* Return 1 if PARMS specifies a fixed number of parameters
   and none of their types is affected by default promotions.  */

int
compparms1 (parms)
     tree parms;
{
  register tree t;
  for (t = parms; t; t = TREE_CHAIN (t))
    {
      register tree type = TREE_VALUE (t);

      if (TREE_CHAIN (t) == 0 && type != void_type_node)
	return 0;

      if (type == float_type_node)
	return 0;

      if (TREE_CODE (type) == INTEGER_TYPE
	  && TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node))
	return 0;
    }
  return 1;
}

/* Return an unsigned type the same as TYPE in other respects.

   C++: must make these work for type variants as well.  */

tree
unsigned_type (type)
     tree type;
{
  type = TYPE_MAIN_VARIANT (type);

  if (type == signed_char_type_node || type == char_type_node)
    return unsigned_char_type_node;
  if (type == integer_type_node)
    return unsigned_type_node;
  if (type == short_integer_type_node)
    return short_unsigned_type_node;
  if (type == long_integer_type_node)
    return long_unsigned_type_node;
  if (type == long_long_integer_type_node)
    return long_long_unsigned_type_node;
  return type;
}

/* Return a signed type the same as TYPE in other respects.  */

tree
signed_type (type)
     tree type;
{
  if (type == unsigned_char_type_node || type == char_type_node)
    return signed_char_type_node;
  if (type == unsigned_type_node)
    return integer_type_node;
  if (type == short_unsigned_type_node)
    return short_integer_type_node;
  if (type == long_unsigned_type_node)
    return long_integer_type_node;
  if (type == long_long_unsigned_type_node)
    return long_long_integer_type_node;
  return type;
}

/* Return a type the same as TYPE except unsigned or
   signed according to UNSIGNEDP.  */

tree
signed_or_unsigned_type (unsignedp, type)
     int unsignedp;
     tree type;
{
  if (TREE_CODE (type) != INTEGER_TYPE)
    return type;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (integer_type_node)) 
    return unsignedp ? unsigned_type_node : integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (short_integer_type_node)) 
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_integer_type_node)) 
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;
  if (TYPE_PRECISION (type) == TYPE_PRECISION (long_long_integer_type_node)) 
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);
  return type;
}

/* Return an integer type with BITS bits of precision,
   that is unsigned if UNSIGNEDP is nonzero, otherwise signed.  */

tree
type_for_size (bits, unsignedp)
     int bits;
     int unsignedp;
{
  if (bits <= TYPE_PRECISION (signed_char_type_node))
    return unsignedp ? unsigned_char_type_node : signed_char_type_node;

  if (bits <= TYPE_PRECISION (short_integer_type_node))
    return unsignedp ? short_unsigned_type_node : short_integer_type_node;

  if (bits <= TYPE_PRECISION (integer_type_node))
    return unsignedp ? unsigned_type_node : integer_type_node;

  if (bits <= TYPE_PRECISION (long_integer_type_node))
    return unsignedp ? long_unsigned_type_node : long_integer_type_node;

  if (bits <= TYPE_PRECISION (long_long_integer_type_node))
    return (unsignedp ? long_long_unsigned_type_node
	    : long_long_integer_type_node);

  return 0;
}

tree
get_floating_type (mode)
     enum machine_mode mode;
{
  if (mode == TYPE_MODE (float_type_node))
    return float_type_node;
  if (mode == TYPE_MODE (double_type_node))
    return double_type_node;
  if (mode == TYPE_MODE (long_double_type_node))
    return long_double_type_node;
  abort ();
}

tree
c_sizeof (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);

  if (code == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	warning ("sizeof applied to a function type");
      return build_int (1);
    }
  if (code == METHOD_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	warning ("sizeof applied to a method type");
      return build_int (1);
    }
  if (code == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	warning ("sizeof applied to a void type");
      return build_int (1);
    }

  /* C++: this is really correct!  */
  if (code == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  return size_in_bytes (type);
}

tree
c_sizeof_nowarn (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);

  if (code == FUNCTION_TYPE
      || code == METHOD_TYPE
      || code == VOID_TYPE)
    return build_int (1);
  if (code == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  return size_in_bytes (type);
}

/* Implement the __alignof keyword: Return the minimum required
   alignment of TYPE, measured in bytes.  */

tree
c_alignof (type)
     tree type;
{
  enum tree_code code = TREE_CODE (type);

  if (code == FUNCTION_TYPE || code == METHOD_TYPE)
    return build_int (FUNCTION_BOUNDARY / BITS_PER_UNIT);

  if (code == VOID_TYPE)
    return build_int (1);

  /* C++: this is really correct!  */
  if (code == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  return build_int (TYPE_ALIGN (type) / BITS_PER_UNIT);
}

/* Perform default promotions for C data used in expressions.
   Arrays and functions are converted to pointers;
   enumeral types or short or char, to int.
   In addition, manifest constants symbols are replaced by their values.

   C++: this will automatically bash references to their target type.  */

tree
default_conversion (exp)
     tree exp;
{
  register tree dt = TREE_TYPE (exp);
  register enum tree_code form = TREE_CODE (dt);

  if (form == OFFSET_TYPE)
    {
#if 0
      warning ("conversion from member type");
#endif
      if (TREE_CODE (exp) == OFFSET_REF)
	return default_conversion (resolve_offset_ref (exp));

      dt = TREE_TYPE (dt);
      form = TREE_CODE (dt);
    }

  if (form == REFERENCE_TYPE)
    {
      exp = convert_from_reference (exp);
      dt = TREE_TYPE (exp);
      form = TREE_CODE (dt);
    }

  if (TREE_CODE (exp) == CONST_DECL)
    exp = DECL_INITIAL (exp);
  else if (TREE_READONLY_DECL_P (exp))
    {
      exp = decl_constant_value (exp);
      dt = TREE_TYPE (exp);
    }

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since EXP is being used in non-lvalue context.  */
  if (TREE_CODE (exp) == NOP_EXPR
      && TREE_TYPE (exp) == TREE_TYPE (TREE_OPERAND (exp, 0)))
    exp = TREE_OPERAND (exp, 0);

  if (form == ENUMERAL_TYPE
      || (form == INTEGER_TYPE
	  && (TYPE_PRECISION (dt)
	      < TYPE_PRECISION (integer_type_node))))
    {
      /* Traditionally, unsignedness is preserved in default promotions.  */
      if (flag_traditional && TREE_UNSIGNED (dt))
	return convert (unsigned_type_node, exp);
      return convert (integer_type_node, exp);
    }
  if (flag_traditional && dt == float_type_node)
    return convert (double_type_node, exp);
  if (form == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (form == FUNCTION_TYPE || form == METHOD_TYPE)
    {
      return build_unary_op (ADDR_EXPR, exp, 0);
    }
  if (form == ARRAY_TYPE)
    {
      register tree adr;
      tree restype = TREE_TYPE (dt);
      tree ptrtype;

      if (TREE_CODE (exp) == INDIRECT_REF)
	{
	  /* Stripping away the INDIRECT_REF is not the right
	     thing to do for references...  */
	  tree inner = TREE_OPERAND (exp, 0);
	  if (TREE_CODE (TREE_TYPE (inner)) == REFERENCE_TYPE)
	    inner = build1 (REFERENCE_EXPR,
			    build_pointer_type (TREE_TYPE (TREE_TYPE (inner))),
			    inner);
	  return convert (TYPE_POINTER_TO (TREE_TYPE (dt)), inner);
	}

      if (TREE_CODE (exp) == COMPOUND_EXPR)
	{
	  tree op1 = default_conversion (TREE_OPERAND (exp, 1));
	  return build (COMPOUND_EXPR, TREE_TYPE (op1),
			TREE_OPERAND (exp, 0), op1);
	}

      if (!lvalue_p (exp)
	  && ! (TREE_CODE (exp) == CONSTRUCTOR && TREE_STATIC (exp)))
	{
	  error ("invalid use of non-lvalue array");
	  return error_mark_node;
	}

      if (TREE_READONLY (exp) || TREE_THIS_VOLATILE (exp))
	restype = build_type_variant (restype, TREE_READONLY (exp),
				      TREE_THIS_VOLATILE (exp));

      ptrtype = build_pointer_type (restype);

      if (TREE_CODE (exp) == VAR_DECL)
	{
	  /* ??? This is not really quite correct
	     in that the type of the operand of ADDR_EXPR
	     is not the target type of the type of the ADDR_EXPR itself.
	     Question is, can this lossage be avoided?  */
	  adr = build1 (ADDR_EXPR, ptrtype, exp);
	  if (mark_addressable (exp) == 0)
	    return error_mark_node;
	  TREE_LITERAL (adr) = staticp (exp);
	  TREE_VOLATILE (adr) = 0;   /* Default would be, same as EXP.  */
	  return adr;
	}
      /* This way is better for a COMPONENT_REF since it can
	 simplify the offset for a component.  */
      adr = build_unary_op (ADDR_EXPR, exp, 1);
      return convert (ptrtype, adr);
    }
  return exp;
}

/* Like `build_component_ref, but uses an already found field.
   Must compute visibility for C_C_D.  Otherwise, ok.  */
tree
build_component_ref_1 (datum, field, protect)
     tree datum, field;
     int protect;
{
  register tree basetype = TREE_TYPE (datum);
  register enum tree_code form = TREE_CODE (basetype);
  register tree ref;

  if (form == REFERENCE_TYPE)
    {
      datum = convert_from_reference (datum);
      basetype = TREE_TYPE (datum);
      form = TREE_CODE (basetype);
    }

  if (! IS_AGGR_TYPE_CODE (form))
    {
      if (form != ERROR_MARK)
	error_with_decl (field, "request for member `%s' in something not a class, structure or union");
      return error_mark_node;
    }

  if (TYPE_SIZE (basetype) == 0)
    {
      incomplete_type_error (0, basetype);
      return error_mark_node;
    }

  /* Look up component name in the structure type definition.  */

  if (field == error_mark_node)
    abort ();

  if (TREE_STATIC (field))
    return field;

  if (datum == C_C_D && ! TREE_FIELD_PUBLIC (field))
    {
      enum visibility_type visibility
	= compute_visibility (build_tree_list (NULL_TREE, current_class_type),
			      field);

    if (visibility == visibility_private)
      {
	error_with_decl (field, "field `%s' is private");
	return error_mark_node;
      }
    else if (visibility == visibility_protected)
      {
	error_with_decl (field, "field `%s' is protected");
	return error_mark_node;
      }
    }

  ref = build (COMPONENT_REF, TREE_TYPE (field), datum, field);

  if (TREE_READONLY (datum) || TREE_READONLY (field))
    TREE_READONLY (ref) = 1;
  if (TREE_THIS_VOLATILE (datum) || TREE_VOLATILE (field))
    TREE_THIS_VOLATILE (ref) = 1;

  return ref;
}

tree
build_component_ref (datum, component, basetype_path, protect)
     tree datum, component, basetype_path;
     int protect;
{
  tree basetypes;
  register tree basetype = TREE_TYPE (datum);
  register enum tree_code form = TREE_CODE (basetype);
  register tree field = NULL;
  register tree ref;

  if (form == REFERENCE_TYPE)
    {
#if 0
      /* REFERENCE_EXPRs are not converted by `convert_from_reference'.
	 @@ Maybe that is not right.  */
      if (TREE_CODE (datum) == REFERENCE_EXPR)
	datum = build1 (INDIRECT_REF, TREE_TYPE (basetype), datum);
      else
#endif
	datum = convert_from_reference (datum);
      basetype = TREE_TYPE (datum);
      form = TREE_CODE (basetype);
    }

  /* First, see if there is a field or component with name COMPONENT. */
  if (TREE_CODE (component) == TREE_LIST)
    {
      assert (!(TREE_CHAIN (component) == NULL_TREE
		&& TREE_CHAIN (TREE_VALUE (component)) == NULL_TREE));
      return build (COMPONENT_REF, TREE_TYPE (component), datum, component);
    }
  if (TREE_CODE (component) == TYPE_EXPR)
    return build_component_type_expr (datum, component, NULL_TREE, protect);

  if (! IS_AGGR_TYPE_CODE (form))
    {
      if (form != ERROR_MARK)
	error ("request for member `%s' in something not a class, structure or union",
	       IDENTIFIER_POINTER (component));
      return error_mark_node;
    }

  if (TYPE_SIZE (basetype) == 0)
    {
      incomplete_type_error (0, basetype);
      return error_mark_node;
    }

  /* Look up component name in the structure type definition.  */
  if (basetype_path == NULL_TREE)
    basetype_path = CLASSTYPE_AS_LIST (basetype);
  basetypes = basetype_path;
  field = lookup_field (basetypes, component,
			protect && ! VFIELD_NAME_P (component));

  if (field == error_mark_node)
    return error_mark_node;

  if (field == NULL_TREE)
    {
      /* Not found as a data field, look for it as a method.  If found,
	 then if this is the only possible one, return it, else
	 report ambiguity error.  */
      tree fields = lookup_fnfields (basetype_path, component, 1);
      tree basename = TYPE_NAME (basetype);
      if (fields)
	{
	  if (TREE_CHAIN (fields) == NULL_TREE
	      && TREE_CHAIN (TREE_VALUE (fields)) == NULL_TREE)
	    {
	      enum visibility_type visibility;

	      /* Unique, so use this one now.  */
	      basetype = TREE_PURPOSE (fields);
	      field = TREE_VALUE (fields);
	      visibility = compute_visibility (TREE_PURPOSE (fields), field);
	      if (visibility == visibility_public)
		{
		  if (DECL_VIRTUAL_P (field)
		      && ! resolves_to_fixed_type_p (datum))
		    {
		      tree addr = build_unary_op (ADDR_EXPR, datum, 0);
		      addr = convert_pointer_to (DECL_VCONTEXT (field), addr);
		      datum = build_indirect_ref (addr);
		      assert (datum != error_mark_node);
		      field = build_vfn_ref (&addr, datum, DECL_VINDEX (field));
		    }
		  return field;
		}
	      if (visibility == visibility_protected)
		error_with_decl (field, "member function `%s' is protected");
	      else
		error_with_decl (field, "member function `%s' is private");
	      return error_mark_node;
	    }
	  else
	    return build (COMPONENT_REF, unknown_type_node, datum, fields);
	}
      if (TREE_CODE (basename) == TYPE_DECL)
	basename = DECL_NAME (basename);

      if (OPERATOR_TYPENAME_P (component))
	error ("%s `%s' has no such type conversion operator",
	       form == RECORD_TYPE ? "structure" : "union",
	       IDENTIFIER_POINTER (basename));
      else
	error (form == RECORD_TYPE
	       ? "structure `%s' has no member named `%s'"
	       : "union `%s' has no member named `%s'",
	       IDENTIFIER_POINTER (basename),
	       IDENTIFIER_POINTER (component));
      return error_mark_node;
    }
  else if (TREE_TYPE (field) == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (field) == VAR_DECL || TREE_CODE (field) == CONST_DECL)
    {
      TREE_USED (field) = 1;
      return field;
    }

  if (DECL_FIELD_CONTEXT (field) != basetype
      && (TYPE_USES_MULTIPLE_INHERITANCE (basetype)
	  || TYPE_USES_VIRTUAL_BASECLASSES (basetype)))
    {
      tree addr = build_unary_op (ADDR_EXPR, datum, 0);
      addr = convert_pointer_to (DECL_FIELD_CONTEXT (field), addr);
      datum = build_indirect_ref (addr);
      assert (datum != error_mark_node);
    }
  ref = build (COMPONENT_REF, TREE_TYPE (field), datum, field);

  if (TREE_READONLY (datum) || TREE_READONLY (field))
    TREE_READONLY (ref) = 1;
  if (TREE_THIS_VOLATILE (datum) || TREE_VOLATILE (field))
    TREE_THIS_VOLATILE (ref) = 1;
  return ref;
}

/* Given an expression PTR for a pointer, return an expression
   for the value pointed to.
   ERRORSTRING is the name of the operator to appear in error messages.

   This function may need to overload OPERATOR_FNNAME.
   Must also handle REFERENCE_TYPEs for C++.  */

tree
build_x_indirect_ref (ptr, errorstring)
     tree ptr;
     char *errorstring;
{
  tree rval = build_opfncall (INDIRECT_REF, LOOKUP_NORMAL, ptr);

  if (rval) return rval;
  return build_indirect_ref (ptr, errorstring);
}

tree
build_indirect_ref (ptr, errorstring)
     tree ptr;
     char *errorstring;
{
  register tree pointer = default_conversion (ptr);
  register tree dt = TREE_TYPE (pointer);

  if (ptr == current_class_decl)
    return C_C_D;

  if (TREE_CODE (dt) == POINTER_TYPE || TREE_CODE (dt) == REFERENCE_TYPE)
    if (TREE_CODE (pointer) == ADDR_EXPR
	&& (TREE_TYPE (TREE_OPERAND (pointer, 0))
	    == TREE_TYPE (dt)))
      return TREE_OPERAND (pointer, 0);
    else
      {
	tree t = TREE_TYPE (dt);
	register tree ref = build1 (INDIRECT_REF,
				    TYPE_MAIN_VARIANT (t), pointer);

	TREE_READONLY (ref) = TREE_READONLY (t);
	TREE_VOLATILE (ref) = TREE_VOLATILE (t) || TREE_VOLATILE (pointer);
	TREE_THIS_VOLATILE (ref) = TREE_VOLATILE (t);
	return ref;
      }
  else if (pointer != error_mark_node)
    error ("invalid type argument of `%s'", errorstring);
  return error_mark_node;
}

/* This handles expressions of the form "a[i]", which denotes
   an array reference.

   This is logically equivalent in C to *(a+i), but we may do it differently.
   If A is a variable or a member, we generate a primitive ARRAY_REF.
   This avoids forcing the array out of registers, and can work on
   arrays that are not lvalues (for example, members of structures returned
   by functions).

   If INDEX is of some user-defined type, it must be converted to
   integer type.  Otherwise, to make a compatible PLUS_EXPR, it
   will inherit the type of the array, which will be some pointer type.  */

tree
build_x_array_ref (array, index)
     tree array, index;
{
  tree rval;

  rval = build_opfncall (ARRAY_REF, LOOKUP_NORMAL, array, index);
  if (rval)
    return rval;
  return build_array_ref (array, index);
}

tree
build_array_ref (array, index)
     tree array, index;
{
  tree itype;
  tree rval;

  if (index == 0)
    {
      error ("subscript missing in array reference");
      return error_mark_node;
    }

  itype = TREE_TYPE (index);
  if (TREE_CODE (itype) == REFERENCE_TYPE)
    {
      index = convert_from_reference (index);
      itype = TREE_TYPE (index);
    }

  if (IS_AGGR_TYPE (itype))
    if (TYPE_HAS_INT_CONVERSION (itype))
      index = build_type_conversion (CONVERT_EXPR, integer_type_node, index, 1);
    else
      {
	error_with_aggr_type (itype, "type `%s' requires integer conversion for array indexing");
	return error_mark_node;
      }

  if (TREE_CODE (TREE_TYPE (array)) == ARRAY_TYPE
      && TREE_CODE (array) != INDIRECT_REF)
    {
      index = default_conversion (index);
      if (index != error_mark_node
	  && TREE_CODE (TREE_TYPE (index)) != INTEGER_TYPE)
	{
	  error ("array subscript is not an integer");
	  return error_mark_node;
	}

      /* An array that is indexed by a non-constant
	 cannot be stored in a register; we must be able to do
	 address arithmetic on its address.
	 Likewise an array of elements of variable size.  */
      if (TREE_CODE (index) != INTEGER_CST
	  || (TYPE_SIZE (TREE_TYPE (TREE_TYPE (array))) != 0
	      && TREE_CODE (TYPE_SIZE (TREE_TYPE (TREE_TYPE (array)))) != INTEGER_CST))
	{
	  if (mark_addressable (array) == 0)
	    return error_mark_node;
	}

      if (pedantic && !lvalue_p (array))
	warning ("ANSI C forbids subscripting non-lvalue array");

      if (pedantic)
	{
	  tree foo = array;
	  while (TREE_CODE (foo) == COMPONENT_REF)
	    foo = TREE_OPERAND (foo, 0);
	  if (TREE_CODE (foo) == VAR_DECL && TREE_REGDECL (foo))
	    warning ("ANSI C forbids subscripting non-lvalue array");
	}

      rval = build (ARRAY_REF, TREE_TYPE (TREE_TYPE (array)), array, index);
      /* Array ref is const/volatile if the array elements are.  */
      TREE_READONLY (rval) |= TREE_READONLY (TREE_TYPE (TREE_TYPE (array)));
      TREE_VOLATILE (rval) |= TREE_VOLATILE (TREE_TYPE (TREE_TYPE (array)));
      TREE_THIS_VOLATILE (rval) |= TREE_VOLATILE (TREE_TYPE (TREE_TYPE (array)));
      return require_complete_type (fold (rval));
    }

  {
    tree ar = default_conversion (array);
    tree ind = default_conversion (index);

    if ((TREE_CODE (TREE_TYPE (ar)) == POINTER_TYPE
	 && TREE_CODE (TREE_TYPE (ind)) != INTEGER_TYPE)
	|| (TREE_CODE (TREE_TYPE (ind)) == POINTER_TYPE
	    && TREE_CODE (TREE_TYPE (ar)) != INTEGER_TYPE))
      {
	error ("array subscript is not an integer");
	return error_mark_node;
      }

    return build_indirect_ref (build_binary_op_nodefault (PLUS_EXPR, ar, ind, PLUS_EXPR),
			       "array indexing");
  }
}

/* Build a function call to function FUNCTION with parameters PARAMS.
   PARAMS is a list--a chain of TREE_LIST nodes--in which the
   TREE_VALUE of each node is a parameter-expression.
   FUNCTION's data type may be a function type or a pointer-to-function.

   For C++: If FUNCTION's data type is a TREE_LIST, then the tree list
   is the list of possible methods that FUNCTION could conceivably
   be.  If the list of methods comes from a class, then it will be
   a list of lists (where each element is associated with the class
   that produced it), otherwise it will be a simple list (for
   functions overloaded in global scope).

   In the first case, TREE_VALUE (function) is the head of one of those
   lists, and TREE_PURPOSE is the name of the function.

   In the second case, TREE_PURPOSE (function) is the function's
   name directly.

   DECL is the class instance variable, usually CURRENT_CLASS_DECL.  */

tree
build_x_function_call (function, params, decl)
     tree function, params, decl;
{
  tree type = TREE_TYPE (function);
  int may_be_method
    = ((TREE_CODE (function) == TREE_LIST
	&& current_class_type != NULL_TREE
	&& IDENTIFIER_CLASS_VALUE (TREE_PURPOSE (function)) == function)
       || TREE_CODE (function) == IDENTIFIER_NODE
       || TREE_CODE (type) == METHOD_TYPE);

  /* Handle methods, friends, and overloaded functions, respectively.  */
  if (may_be_method)
    {
      if (TREE_CODE (function) == FUNCTION_DECL)
	function = DECL_ORIGINAL_NAME (function);
      else if (TREE_CODE (function) == TREE_LIST)
	{
#if 0
	  if (TREE_CODE (TREE_VALUE (function)) == TREE_LIST)
	    function = TREE_PURPOSE (TREE_VALUE (function));
	  else
	    function = TREE_PURPOSE (function);
#else
	  assert (TREE_CODE (TREE_VALUE (function)) == FUNCTION_DECL);
	  function = TREE_PURPOSE (function);
#endif
	}
      else if (TREE_CODE (function) != IDENTIFIER_NODE)
	{
	  /* Call via a pointer to member function.  */
	  if (decl == NULL_TREE)
	    {
	      error ("pointer to member function called, but not in class scope");
	      return error_mark_node;
	    }
	  function = build (OFFSET_REF, TREE_TYPE (type), NULL_TREE, function);
	  goto do_x_function;
	}

      /* this is an abbreviated method call.
         must go through here in case it is a virtual function.
	 @@ Perhaps this could be optimized.  */

      if (decl == NULL_TREE)
	{
	  if (current_class_type == NULL_TREE)
	    {
	      error ("object missing in call to method `%s'",
		     IDENTIFIER_POINTER (function));
	      return error_mark_node;
	    }
	  /* Yow: call from a static member function.  */
	  decl = build1 (NOP_EXPR, TYPE_POINTER_TO (current_class_type), error_mark_node);
	}

      return build_method_call (decl, function, params, NULL_TREE, LOOKUP_NORMAL);
    }
  else if (TREE_CODE (function) == COMPONENT_REF
	   && type == unknown_type_node)
    {
      function = TREE_PURPOSE (TREE_OPERAND (function, 1));
      return build_method_call (decl, function, params, NULL_TREE, LOOKUP_NORMAL);
    }
  else if (TREE_CODE (function) == TREE_LIST)
    {
      if (TREE_CHAIN (function) != NULL_TREE)
	return build_overload_call (TREE_PURPOSE (function), params, 1, 0);
      else if (TREE_VALUE (function) != NULL_TREE)
	function = TREE_VALUE (function);
      else
	{
	  error ("function `%s' declared overloaded, but no definitions appear with which to resolve it",
		 IDENTIFIER_POINTER (TREE_PURPOSE (function)));
	  return error_mark_node;
	}
    }
  else if (TREE_CODE (type) == POINTER_TYPE
	   && TREE_CODE (TREE_TYPE (type)) == METHOD_TYPE
	   && (TREE_CODE (function) == VAR_DECL
	       || TREE_CODE (function) == PARM_DECL
	       || TREE_CODE (function) == FIELD_DECL))
    {
      error_with_decl (function, "call via pointer-to-member-function `%s' must be composed with object");
      return error_mark_node;
    }

 do_x_function:
  if (TREE_CODE (function) == OFFSET_REF)
    {
      /* If the component is a data element (or a virtual function), we play
	 games here to make things work.  */
      tree decl_addr;

      if (TREE_OPERAND (function, 0))
	decl = TREE_OPERAND (function, 0);
      else
	decl = C_C_D;

      decl_addr = build_unary_op (ADDR_EXPR, decl, 0);
      function = get_member_function (&decl_addr, decl, TREE_OPERAND (function, 1));
      params = tree_cons (NULL_TREE, decl_addr, params);
      return build_function_call (function, params);
    }

  type = TREE_TYPE (function);
  if (TREE_CODE (type) == REFERENCE_TYPE)
    type = TREE_TYPE (type);

  if (TYPE_LANG_SPECIFIC (type) && TYPE_OVERLOADS_CALL_EXPR (type))
    return build_opfncall (CALL_EXPR, LOOKUP_NORMAL, function, params);

  if (may_be_method)
    {
      tree ctypeptr = TYPE_POINTER_TO (TYPE_METHOD_BASETYPE (TREE_TYPE (function)));
      if (decl == NULL_TREE)
	{
	  if (current_function_decl
	      && DECL_STATIC_FUNCTION_P (current_function_decl))
	    error ("invalid call to member function needing `this' in static member function scope");
	  else
	    error ("pointer to member function called, but not in class scope");
	  return error_mark_node;
	}
      if (TREE_CODE (TREE_TYPE (decl)) != POINTER_TYPE)
	{
	  decl = build_unary_op (ADDR_EXPR, decl, 0);
	  decl = convert_pointer_to (TREE_TYPE (ctypeptr), decl);
	}
      else
	decl = build_c_cast (ctypeptr, decl);
      params = tree_cons (NULL_TREE, decl, params);
    }

  return build_function_call (function, params);
}

tree
build_function_call (function, params)
     tree function, params;
{
  register tree fntype, fndecl;
  register tree value_type;
  register tree coerced_params;
  tree actualparameterlist ();
  int is_method;
  
#ifdef FIELD_XREF
  if (TREE_CODE(function) == FUNCTION_DECL)
     FIELD_xref_call(current_function_decl,
		     IDENTIFIER_POINTER(DECL_NAME(function)));
#endif

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since FUNCTION is used in non-lvalue context.  */
  if (TREE_CODE (function) == NOP_EXPR
      && TREE_TYPE (function) == TREE_TYPE (TREE_OPERAND (function, 0)))
    function = TREE_OPERAND (function, 0);

  if (TREE_CODE (function) == FUNCTION_DECL)
    fndecl = function;
  else
    fndecl = NULL_TREE;

  /* Convert anything with function type to a pointer-to-function.  */
  if (TREE_CODE (function) == FUNCTION_DECL)
    {
      if (pedantic
	  && IDENTIFIER_LENGTH (DECL_NAME (function)) == 4
	  && ! strcmp (IDENTIFIER_POINTER (DECL_NAME (function)), "main"))
	{
	  error ("cannot call `main' from within program");
	  return error_mark_node;
	}

      /* Differs from default_conversion by not setting TREE_ADDRESSABLE
	 (because calling an inline function does not mean the function
	 needs to be separately compiled).  */

      if (! TREE_INLINE (function))
	TREE_USED (function) = 1;

      function = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (function)),
			function);
    }
  else
    {
      if (function == error_mark_node)
	return error_mark_node;
      function = default_conversion (function);
    }

  fntype = TREE_TYPE (function);

  is_method = (TREE_CODE (fntype) == POINTER_TYPE
	       && TREE_CODE (TREE_TYPE (fntype)) == METHOD_TYPE);

  if (!(TREE_CODE (fntype) == POINTER_TYPE
	&& (TREE_CODE (TREE_TYPE (fntype)) == FUNCTION_TYPE || is_method)))
    {
      error ("called object is not a function");
      return error_mark_node;
    }

  /* fntype now gets the type of function pointed to.  */
  fntype = TREE_TYPE (fntype);

  /* Convert the parameters to the types declared in the
     function prototype, or apply default promotions.  */

  coerced_params = actualparameterlist (NULL_TREE, TYPE_ARG_TYPES (fntype), params, fndecl, LOOKUP_NORMAL);

  /* Recognize certain built-in functions so we can make tree-codes
     other than CALL_EXPR.  We do this when it enables fold-const.c
     to do something useful.  */

  if (TREE_CODE (function) == ADDR_EXPR
      && TREE_CODE (TREE_OPERAND (function, 0)) == FUNCTION_DECL)
    switch (DECL_FUNCTION_CODE (TREE_OPERAND (function, 0)))
      {
      case BUILT_IN_ABS:
      case BUILT_IN_LABS:
      case BUILT_IN_FABS:
	if (coerced_params == 0)
	  return integer_zero_node;
	return build_unary_op (ABS_EXPR, TREE_VALUE (coerced_params), 0);
      }

  value_type = TREE_TYPE (fntype) ? TREE_TYPE (fntype) : void_type_node;

  if (is_method)
    {
      tree parm = TREE_VALUE (coerced_params);
      tree parmtype = TREE_TYPE (parm);
      if (parmtype == error_mark_node)
	return error_mark_node;

      parmtype = TREE_TYPE (parmtype);
      if (TYPE_NEEDS_WRAPPER (parmtype))
	{
	  if (fndecl == NULL_TREE || ! WRAPPER_NAME_P (DECL_NAME (fndecl)))
	    {
	      int bytecount = get_arglist_len_in_bytes (coerced_params);

	      params = tree_cons (NULL_TREE, build_int_2 (bytecount, 0),
				  tree_cons (NULL_TREE, function, TREE_CHAIN (coerced_params)));

	      return build_method_call (TREE_VALUE (coerced_params),
					wrapper_name, params,
					NULL_TREE, LOOKUP_NORMAL);
	    }
	}
    }
  {
    register tree result = 
      build (CALL_EXPR, value_type, function, coerced_params, NULL_TREE);

    TREE_VOLATILE (result) = 1;
    TREE_RAISES (result) |= !! TYPE_RAISES_EXCEPTIONS (fntype);
    if (value_type == void_type_node)
      return result;
    return require_complete_type (result);
  }
}

/* Convert the actual parameter expressions in the list VALUES
   to the types in the list TYPELIST.
   If parmdecls is exhausted, or when an element has NULL as its type,
   perform the default conversions.

   RETURN_LOC is the location of the return value, if known, NULL_TREE
   otherwise.  This is useful in the case where we can avoid creating
   a temporary variable in the case where we can initialize the return
   value directly.  If we are not eliding constructors, then we set this
   to NULL_TREE to avoid this avoidance.

   NAME is an IDENTIFIER_NODE or 0.  It is used only for error messages.

   This is also where warnings about wrong number of args are generated.
   
   Return a list of expressions for the parameters as converted.

   Both VALUES and the returned value are chains of TREE_LIST nodes
   with the elements of the list in the TREE_VALUE slots of those nodes.

   In C++, unspecified trailing parameters can be filled in with their
   default arguments, if such were specified.  Do so here.  */

tree
actualparameterlist (return_loc, typelist, values, fndecl, flags)
     tree return_loc, typelist, values, fndecl;
     int flags;
{
  register tree typetail, valtail;
  register tree result = NULL_TREE;
  char *called_thing;
  int maybe_raises = 0;

  if (! flag_elide_constructors)
    return_loc = 0;

  if (fndecl)
    if (TREE_CODE (TREE_TYPE (fndecl)) == METHOD_TYPE)
      if (TREE_TYPE (DECL_ORIGINAL_NAME (fndecl)))
	called_thing = "constructor";
      else
	called_thing = "member function";
    else
      called_thing = "function";

  for (valtail = values, typetail = typelist;
       valtail;
       valtail = TREE_CHAIN (valtail))
    {
      register tree type = typetail ? TREE_VALUE (typetail) : 0;
      register tree val = TREE_VALUE (valtail);
      register tree parm;

      if (type == void_type_node)
	{
	  if (fndecl)
	    {
	      char *buf = (char *)alloca (80);
	      sprintf (buf, "too many arguments to %s `%%s'", called_thing);
	      error_with_decl (fndecl, buf);
	      error ("at this point in file");
	    }
	  else
	    error ("too many arguments to function");
	  /* In case anybody wants to know if this argument
	     list is valid.  */
	  if (result)
	    TREE_TYPE (result) = error_mark_node;
	  break;
	}

      /* The tree type of the parameter being passed may not yet be
	 known.  In this case, its type is TYPE_UNKNOWN, and will
	 be instantiated by the type given by TYPE.  If TYPE
	 is also NULL, the tree type of VAL is ERROR_MARK_NODE.  */
      if (type && type_unknown_p (val))
	val = require_instantiated_type (type, val, integer_zero_node);
      else if (type_unknown_p (val))
	{

	  if (TREE_CODE (val) == TREE_LIST
	      && TREE_CHAIN (val) == NULL_TREE
	      && (TREE_TYPE (val) == unknown_type_node
		  || TREE_CHAIN (TREE_VALUE (val)) == NULL_TREE))
	    /* Instantiates automatically.  */
	    val = TREE_VALUE (val);
	  else
	    {
	      error ("insufficient type information in parameter list");
	      val = integer_zero_node;
	    }
	}

      {
	/* Convert FUNCTION_DECLs for virtual functions
	   to proper representation.  */
	tree basetype = NULL_TREE;
	tree ttype = TREE_TYPE (val);

	if (TREE_CODE (ttype) == METHOD_TYPE)
	  basetype = TYPE_METHOD_BASETYPE (ttype);
	else if (TREE_CODE (ttype) == OFFSET_TYPE)
	  basetype = TYPE_OFFSET_BASETYPE (ttype);

	/* If BASETYPE is set here, default_conversion will do the
	   actual conversion for us.  */
	if (basetype && TREE_CODE (val) != OFFSET_REF)
	  {
	    val = build (OFFSET_REF, ttype,
			 build1 (NOP_EXPR, basetype, error_mark_node), val);
	    type = build_pointer_type (ttype);
	  }
	else if (TREE_CODE (ttype) == FUNCTION_TYPE)
	  type = build_pointer_type (ttype);
      }

      /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
	 Strip such NOP_EXPRs, since VAL is used in non-lvalue context.  */
      if (TREE_CODE (val) == NOP_EXPR
	  && TREE_TYPE (val) == TREE_TYPE (TREE_OPERAND (val, 0)))
	val = TREE_OPERAND (val, 0);

      if ((type == 0 || TREE_CODE (type) != REFERENCE_TYPE)
	  && (TREE_CODE (TREE_TYPE (val)) == ARRAY_TYPE
	      || TREE_CODE (TREE_TYPE (val)) == FUNCTION_TYPE
	      || TREE_CODE (TREE_TYPE (val)) == METHOD_TYPE))
	val = default_conversion (val);

      val = require_complete_type (val);

      maybe_raises |= TREE_RAISES (val);

      if (type != 0)
	{
	  /* Formal parm type is specified by a function prototype.  */
	  tree parmval;

	  if (TYPE_SIZE (type) == 0)
	    {
	      error ("parameter type of called function is incomplete");
	      parmval = val;
	    }
	  else
	    {
#ifdef PROMOTE_PROTOTYPES
	      /* Rather than truncating and then reextending,
		 convert directly to int, if that's the type we will want.  */
	      if (! flag_traditional
		  && TREE_CODE (type) == INTEGER_TYPE
		  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
		type = integer_type_node;
#endif
	      parmval = convert_for_initialization (return_loc, type, val,
						    "argument passing", flags);
#ifdef PROMOTE_PROTOTYPES
	      if (TREE_CODE (type) == INTEGER_TYPE
		  && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
		parmval = default_conversion (parmval);
#endif
	    }
	  parm = build_tree_list (0, parmval);
	}
      else
	{
	  if (TREE_CODE (TREE_TYPE (val)) == REFERENCE_TYPE)
	    val = convert_from_reference (val);

	  if (TREE_CODE (TREE_TYPE (val)) == REAL_TYPE
	      && (TYPE_PRECISION (TREE_TYPE (val))
		  < TYPE_PRECISION (double_type_node)))
	    /* Convert `float' to `double'.  */
	    parm = build_tree_list (NULL_TREE, convert (double_type_node, val));
	  else if (TYPE_LANG_SPECIFIC (TREE_TYPE (val))
		   && (TYPE_GETS_INIT_REF (TREE_TYPE (val))
		       || TYPE_GETS_ASSIGN_REF (TREE_TYPE (val))))
	    {
	      if (pedantic)
		error_with_aggr_type (TREE_TYPE (val), "cannot pass objects of type `%s' through `...'");
	      else
		warning ("cannot pass objects of type `%s' through `...'",
			 TYPE_NAME_STRING (TREE_TYPE (val)));
	      parm = build_tree_list (NULL_TREE, val);
	    }
	  else
	    /* Convert `short' and `char' to full-size `int'.  */
	    parm = build_tree_list (NULL_TREE, default_conversion (val));
	}

      result = chainon (result, parm);
      if (typetail)
	typetail = TREE_CHAIN (typetail);
    }

  if (typetail != 0 && typetail != void_list_node)
    {
      /* See if there are default arguments that can be used */
      if (TREE_PURPOSE (typetail))
	{
	  while (typetail != void_list_node)
	    {
	      tree type = TREE_VALUE (typetail);
	      tree val = TREE_PURPOSE (typetail);
	      tree parm, parmval;

	      if (val == NULL_TREE)
		parmval = error_mark_node;
	      else if (TREE_CODE (val) == CONSTRUCTOR)
		{
		  parmval = digest_init (type, val, NULL_TREE);
		  parmval = convert_for_initialization (return_loc, type, parmval, "default constructor", flags);
		}
	      else
		{
		  parmval = convert_for_initialization (return_loc, type, val,
							"default argument", flags);
#ifdef PROMOTE_PROTOTYPES
		  if (TREE_CODE (type) == INTEGER_TYPE
		      && (TYPE_PRECISION (type) < TYPE_PRECISION (integer_type_node)))
		    parmval = default_conversion (parmval);
#endif
		}
	      maybe_raises |= TREE_RAISES (parmval);
	      parm = build_tree_list (0, parmval);
	      result = chainon (result, parm);
	      typetail = TREE_CHAIN (typetail);
	      /* ends with `...'.  */
	      if (typetail == NULL_TREE)
		break;
	    }
	}
      else
	{
	  if (fndecl)
	    {
	      char *buf = (char *)alloca (32 + strlen (called_thing));
	      sprintf (buf, "too few arguments to %s `%%s'", called_thing);
	      error_with_decl (fndecl, buf);
	      error ("at this point in file");
	    }
	  else
	    error ("too few arguments to function");
	  return error_mark_list;
	}
    }
  if (result)
    TREE_RAISES (result) = maybe_raises;

  return result;
}

/* Build a binary-operation expression, after performing default
   conversions on the operands.  CODE is the kind of expression to build.  */

tree
build_x_binary_op (code, arg1, arg2)
     enum tree_code code;
     tree arg1, arg2;
{
  tree rval;

  if (rval = build_opfncall (code, LOOKUP_PROTECT, arg1, arg2))
    return rval;
  rval = build_binary_op (code, arg1, arg2);
  if (rval == error_mark_node)
    build_opfncall (code, LOOKUP_NORMAL, arg1, arg2);
  return rval;
}

tree
build_binary_op (code, arg1, arg2)
     enum tree_code code;
     tree arg1, arg2;
{
  tree type1, type2;
  tree args[2];
  arg1 = default_conversion (arg1);
  arg2 = default_conversion (arg2);

  if (type_unknown_p (arg1))
    {
      arg1 = instantiate_type (TREE_TYPE (arg2), arg1, 1);
      arg1 = default_conversion (arg1);
    }
  else
    {
      arg2 = require_instantiated_type (TREE_TYPE (arg1), arg2, error_mark_node);
      arg2 = default_conversion (arg2);
    }

  type1 = TREE_TYPE (arg1);
  type2 = TREE_TYPE (arg2);

  args[0] = arg1;
  args[1] = arg2;

  if (IS_AGGR_TYPE (type1) && IS_AGGR_TYPE (type2))
    {
      /* Try to convert this to something reasonable.  */
      if (! build_default_binary_type_conversion (code, &args[0], &args[1]))
	return error_mark_node;
    }
  else if (IS_AGGR_TYPE (type1) || IS_AGGR_TYPE (type2))
    {
      int convert_index = IS_AGGR_TYPE (type2);
      /* Avoid being tripped up by things like (ARG1 != 0).  */
      tree types[2], try;

      types[0] = type1; types[1] = type2;
      try = build_type_conversion (code, types[convert_index ^ 1],
				   args[convert_index], 1);

      if (try == 0
	  && arg2 == integer_zero_node
	  && (code == NE_EXPR || code == EQ_EXPR))
	try = build_type_conversion (code, ptr_type_node,
				     args[convert_index], 1);
      if (try == 0)
	{
	  error_with_aggr_type (types[convert_index], "type conversion required for type `%s'");
	  return error_mark_node;
	}
      if (try == error_mark_node)
	error ("ambiguous pointer conversion");
      args[convert_index] = try;
    }

  return build_binary_op_nodefault (code, args[0], args[1], code);
}

/* Build a binary-operation expression without default conversions.
   CODE is the kind of expression to build.
   This function differs from `build' in several ways:
   the data type of the result is computed and recorded in it,
   warnings are generated if arg data types are invalid,
   special handling for addition and subtraction of pointers is known,
   and some optimization is done (operations on narrow ints
   are done in the narrower type when that gives the same result).
   Constant folding is also done before the result is returned.

   ERROR_CODE is the code that determines what to say in error messages.
   It is usually, but not always, the same as CODE.

   Note that the operands will never have enumeral types
   because either they have just had the default conversions performed
   or they have both just been converted to some other type in which
   the arithmetic is to be done.

   C++: must do special pointer arithmetic when implementing
   multiple inheritance.  */

tree
build_binary_op_nodefault (code, op0, op1, error_code)
     enum tree_code code;
     tree op0, op1;
     enum tree_code error_code;
{
  tree dt0 = datatype (op0), dt1 = datatype (op1);

  /* The expression codes of the data types of the arguments tell us
     whether the arguments are integers, floating, pointers, etc.  */
  register enum tree_code code0 = TREE_CODE (dt0);
  register enum tree_code code1 = TREE_CODE (dt1);

  /* Expression code to give to the expression when it is built.
     Normally this is CODE, which is what the caller asked for,
     but in some special cases we change it.  */
  register enum tree_code resultcode = code;

  /* Data type in which the computation is to be performed.
     In the simplest cases this is the common type of the arguments.  */
  register tree result_type = NULL;

  /* Nonzero means operands have already been type-converted
     in whatever way is necessary.
     Zero means they need to be converted to RESULT_TYPE.  */
  int converted = 0;

  /* Nonzero means after finally constructing the expression
     give it this type.  Otherwise, give it type RESULT_TYPE.  */
  tree final_type = 0;

  /* Nonzero if this is an operation like MIN or MAX which can
     safely be computed in short if both args are promoted shorts.
     Also implies COMMON.
     -1 indicates a bitwise operation; this makes a difference
     in the exact conditions for when it is safe to do the operation
     in a narrower mode.  */
  int shorten = 0;

  /* Nonzero if this is a comparison operation;
     if both args are promoted shorts, compare the original shorts.
     Also implies COMMON.  */
  int short_compare = 0;

  /* Nonzero if this is a right-shift operation, which can be computed on the
     original short and then promoted if the operand is a promoted short.  */
  int short_shift = 0;

  /* Nonzero means set RESULT_TYPE to the common type of the args.  */
  int common = 0;

  /* If an error was already reported for one of the arguments,
     avoid reporting another error.  */

  if (code0 == ERROR_MARK || code1 == ERROR_MARK)
    return error_mark_node;

  switch (code)
    {
    case PLUS_EXPR:
      /* Handle the pointer + int case.  */
      if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	return pointer_int_sum (PLUS_EXPR, op0, op1);
      else if (code1 == POINTER_TYPE && code0 == INTEGER_TYPE)
	return pointer_int_sum (PLUS_EXPR, op1, op0);
      else
	common = 1;
      break;

    case MINUS_EXPR:
      /* Subtraction of two similar pointers.
	 We must subtract them as integers, then divide by object size.  */
      if (code0 == POINTER_TYPE && code1 == POINTER_TYPE
	  && comp_target_types (dt0, dt1, 1))
	return pointer_diff (op0, op1);
      /* Handle pointer minus int.  Just like pointer plus int.  */
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	return pointer_int_sum (MINUS_EXPR, op0, op1);
      else
	common = 1;
      break;

    case MULT_EXPR:
      common = 1;
      break;

    case TRUNC_DIV_EXPR:
    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
    case EXACT_DIV_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	{
	  if (!(code0 == INTEGER_TYPE && code1 == INTEGER_TYPE))
	    resultcode = RDIV_EXPR;
	  else
	    shorten = 1;
	  common = 1;
	}
      break;

    case BIT_AND_EXPR:
    case BIT_ANDTC_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	shorten = -1;
      /* If one operand is a constant, and the other is a short type
	 that has been converted to an int,
	 really do the work in the short type and then convert the
	 result to int.  If we are lucky, the constant will be 0 or 1
	 in the short type, making the entire operation go away.  */
      if (TREE_CODE (op0) == INTEGER_CST
	  && TREE_CODE (op1) == NOP_EXPR
	  && TYPE_PRECISION (dt1) > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op1, 0)))
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op1, 0))))
	{
	  final_type = result_type;
	  op1 = TREE_OPERAND (op1, 0);
	  result_type = TREE_TYPE (op1);
	}
      if (TREE_CODE (op1) == INTEGER_CST
	  && TREE_CODE (op0) == NOP_EXPR
	  && TYPE_PRECISION (dt0) > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (op0, 0)))
	  && TREE_UNSIGNED (TREE_TYPE (TREE_OPERAND (op0, 0))))
	{
	  final_type = result_type;
	  op0 = TREE_OPERAND (op0, 0);
	  result_type = TREE_TYPE (op0);
	}
      break;

    case TRUNC_MOD_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	shorten = 1;
      break;

    case TRUTH_ANDIF_EXPR:
    case TRUTH_ORIF_EXPR:
    case TRUTH_AND_EXPR:
    case TRUTH_OR_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == POINTER_TYPE || code0 == REAL_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == POINTER_TYPE || code1 == REAL_TYPE))
	{
	  /* Result of these operations is always an int,
	     but that does not mean the operands should be
	     converted to ints!  */
	  result_type = integer_type_node;
	  op0 = truthvalue_conversion (op0);
	  op1 = truthvalue_conversion (op1);
	  converted = 1;

	  /* If these two expressions perform the same operation
	     on what are (or could be given alignment constraints) parts of
	     the same word, try chaining the operations.  */
	  if (optimize)
	    {
	      tree rval;

	      if (TREE_CODE (op0) == TREE_CODE (op1))
		{
		  /* Do they look like (x.p == y.p && x.q == y.q)
		     or (x.p != y.p || x.q != y.q).  */
		  if (((code == TRUTH_ANDIF_EXPR && TREE_CODE (op0) == EQ_EXPR)
		       || (code == TRUTH_ORIF_EXPR && TREE_CODE (op0) == NE_EXPR))
		      && (rval = merge_component_comparisons (code, op0, op1)))
		    return rval;
		}
	      if (TREE_CODE (op0) == code
		  && TREE_CODE (TREE_OPERAND (op0, 1)) == TREE_CODE (op1))
		/* Associate the operation.  */
		{
		  /* Now try to simplify right-hand term.  */
		  if (((code == TRUTH_ANDIF_EXPR && TREE_CODE (op1) == EQ_EXPR)
		       || (code == TRUTH_ORIF_EXPR && TREE_CODE (op1) == NE_EXPR))
		      && (rval = merge_component_comparisons (code, TREE_OPERAND (op0, 1), op1)))
		    {
		      TREE_OPERAND (op0, 1) = rval;
		      return op0;
		    }
		}
	    }
	}
      break;

      /* Shift operations: result has same type as first operand;
	 always convert second operand to int.
	 Also set SHORT_SHIFT if shifting rightward.  */

    case RSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = dt0;
	  if (TREE_CODE (op1) == INTEGER_CST
	      && TREE_INT_CST_LOW (op1) > 0)
	    short_shift = 1;
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TREE_TYPE (op1) != integer_type_node)
	    op1 = convert (integer_type_node, op1);
	}
      break;

    case LSHIFT_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = dt0;
	  if (TREE_CODE (op1) == INTEGER_CST
	      && TREE_INT_CST_LOW (op1) < 0)
	    short_shift = 1;
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TREE_TYPE (op1) != integer_type_node)
	    op1 = convert (integer_type_node, op1);
	}
      break;

    case RROTATE_EXPR:
    case LROTATE_EXPR:
      if (code0 == INTEGER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = dt0;
	  /* Convert the shift-count to an integer, regardless of
	     size of value being shifted.  */
	  if (TREE_TYPE (op1) != integer_type_node)
	    op1 = convert (integer_type_node, op1);
	}
      break;

    case EQ_EXPR:
    case NE_EXPR:
      /* Result of comparison is always int,
	 but don't convert the args to int!  */
      result_type = integer_type_node;
      converted = 1;
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	  && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  register tree tt0 = TYPE_MAIN_VARIANT (TREE_TYPE (dt0));
	  register tree tt1 = TYPE_MAIN_VARIANT (TREE_TYPE (dt1));
	  /* Anything compares with void *.  void * compares with anything.
	     Otherwise, the targets must be the same.  */
	  if (tt0 != tt1 && IS_AGGR_TYPE (tt0) && IS_AGGR_TYPE (tt1))
	    {
	      tree base = common_base_type (tt0, tt1);
	      if (base == NULL_TREE)
		warning ("comparison of distinct object pointer types");
	      else if (base == error_mark_node)
		{
		  message_2_types (error, "comparison of pointer types `%s*' and `%s*' requires conversion to ambiguous supertype", tt0, tt1);
		  return error_mark_node;
		}
	      op0 = convert (TYPE_POINTER_TO (base), op0);
	      op1 = convert (TYPE_POINTER_TO (base), op1);
	    }
	  else if (comp_target_types (dt0, dt1, 1))
	    ;
	  else if (tt0 == void_type_node)
	    {
	      if (pedantic && TREE_CODE (tt1) == FUNCTION_TYPE)
		warning ("ANSI C forbids comparison of `void *' with function pointer");
	    }
	  else if (tt1 == void_type_node)
	    {
	      if (pedantic && TREE_CODE (tt0) == FUNCTION_TYPE)
		warning ("ANSI C forbids comparison of `void *' with function pointer");
	    }
	  else
	    warning ("comparison of distinct pointer types lacks a cast");
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	op1 = null_pointer_node;
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	op0 = null_pointer_node;
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  error ("comparison between pointer and integer");
	  op1 = convert (TREE_TYPE (op0), op1);
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  error ("comparison between pointer and integer");
	  op0 = convert (TREE_TYPE (op1), op0);
	}
      else
	/* If args are not valid, clear out RESULT_TYPE
	   to cause an error message later.  */
	result_type = 0;
      break;

    case MAX_EXPR:
    case MIN_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	   && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	shorten = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (! comp_target_types (dt0, dt1, 1))
	    warning ("comparison of distinct pointer types lacks a cast");
	  else if (pedantic 
		   && TREE_CODE (TREE_TYPE (dt0)) == FUNCTION_TYPE)
	    warning ("ANSI C forbids ordered comparisons of pointers to functions");
	  result_type = commontype (dt0, dt1);
	}
      break;

    case LE_EXPR:
    case GE_EXPR:
    case LT_EXPR:
    case GT_EXPR:
      if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
	   && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
	short_compare = 1;
      else if (code0 == POINTER_TYPE && code1 == POINTER_TYPE)
	{
	  if (! comp_target_types (dt0, dt1, 1))
	    warning ("comparison of distinct pointer types lacks a cast");
	  else if (pedantic 
		   && TREE_CODE (TREE_TYPE (dt0)) == FUNCTION_TYPE)
	    warning ("ANSI C forbids ordered comparisons of pointers to functions");
	  result_type = integer_type_node;
	}
      else if (code0 == POINTER_TYPE && TREE_CODE (op1) == INTEGER_CST
	       && integer_zerop (op1))
	{
	  result_type = integer_type_node;
	  op1 = null_pointer_node;
	  if (! flag_traditional)
	    warning ("ordered comparison of pointer with integer zero");
	}
      else if (code1 == POINTER_TYPE && TREE_CODE (op0) == INTEGER_CST
	       && integer_zerop (op0))
	{
	  result_type = integer_type_node;
	  op0 = null_pointer_node;
	  if (pedantic)
	    warning ("ordered comparison of pointer with integer zero");
	}
      else if (code0 == POINTER_TYPE && code1 == INTEGER_TYPE)
	{
	  result_type = integer_type_node;
	  if (! flag_traditional)
	    warning ("comparison between pointer and integer");
	  op1 = convert (TREE_TYPE (op0), op1);
	}
      else if (code0 == INTEGER_TYPE && code1 == POINTER_TYPE)
	{
	  result_type = integer_type_node;
	  if (! flag_traditional)
	    warning ("comparison between pointer and integer");
	  op0 = convert (TREE_TYPE (op1), op0);
	}
      converted = 1;
      break;
    }

  if ((code0 == INTEGER_TYPE || code0 == REAL_TYPE)
      && (code1 == INTEGER_TYPE || code1 == REAL_TYPE))
    {
      if (shorten || common || short_compare)
	result_type = commontype (dt0, dt1);

      /* For certain operations (which identify themselves by shorten != 0)
	 if both args were extended from the same smaller type,
	 do the arithmetic in that type and then extend.

	 shorten !=0 and !=1 indicates a bitwise operation.
	 For them, this optimization is safe only if
	 both args are zero-extended or both are sign-extended.
	 Otherwise, we might change the result.
	 Eg, (short)-1 | (unsigned short)-1 is (int)-1
	 but calculated in (unsigned short) it would be (unsigned short)-1.  */

      if (shorten)
	{
	  int unsigned0, unsigned1;
	  tree arg0 = get_narrower (op0, &unsigned0);
	  tree arg1 = get_narrower (op1, &unsigned1);
	  /* UNS is 1 if the operation to be done is an unsigned one.  */
	  int uns = TREE_UNSIGNED (result_type);
	  tree type;

	  final_type = result_type;

	  /* Handle the case that OP0 does not *contain* a conversion
	     but it *requires* conversion to FINAL_TYPE.  */

	  if (op0 == arg0 && TREE_TYPE (op0) != final_type)
	    unsigned0 = TREE_UNSIGNED (TREE_TYPE (op0));
	  if (op1 == arg1 && TREE_TYPE (op1) != final_type)
	    unsigned1 = TREE_UNSIGNED (TREE_TYPE (op1));

	  /* Now UNSIGNED0 is 1 if ARG0 zero-extends to FINAL_TYPE.  */

	  /* For bitwise operations, signedness of nominal type
	     does not matter.  Consider only how operands were extended.  */
	  if (shorten == -1)
	    uns = unsigned0;

	  /* Note that in all three cases below we refrain from optimizing
	     an unsigned operation on sign-extended args.
	     That would not be valid.  */

	  /* Both args variable: if both extended in same way
	     from same width, do it in that width.
	     Do it unsigned if args were zero-extended.  */
	  if ((TYPE_PRECISION (TREE_TYPE (arg0))
	       < TYPE_PRECISION (result_type))
	      && (TYPE_PRECISION (TREE_TYPE (arg1))
		  == TYPE_PRECISION (TREE_TYPE (arg0)))
	      && unsigned0 == unsigned1
	      && (unsigned0 || !uns))
	    result_type
	      = signed_or_unsigned_type (unsigned0,
					 commontype (TREE_TYPE (arg0), TREE_TYPE (arg1)));
	  else if (TREE_CODE (arg0) == INTEGER_CST
		   && (unsigned1 || !uns)
		   && (TYPE_PRECISION (TREE_TYPE (arg1))
		       < TYPE_PRECISION (result_type))
		   && (type = signed_or_unsigned_type (unsigned1,
						       TREE_TYPE (arg1)),
		       int_fits_type_p (arg0, type)))
	    result_type = type;
	  else if (TREE_CODE (arg1) == INTEGER_CST
		   && (unsigned0 || !uns)
		   && (TYPE_PRECISION (TREE_TYPE (arg0))
		       < TYPE_PRECISION (result_type))
		   && (type = signed_or_unsigned_type (unsigned0,
						       TREE_TYPE (arg0)),
		       int_fits_type_p (arg1, type)))
	    result_type = type;
	}

      /* Shifts can be shortened if shifting right.  */

      if (short_shift)
	{
	  int unsigned_arg;
	  tree arg0 = get_narrower (op0, &unsigned_arg);

	  final_type = result_type;

	  if (arg0 == op0 && final_type == TREE_TYPE (op0))
	    unsigned_arg = TREE_UNSIGNED (TREE_TYPE (op0));

	  if (TYPE_PRECISION (TREE_TYPE (arg0)) < TYPE_PRECISION (result_type)
	      /* If arg is sign-extended and then unsigned-shifted,
		 we can simulate this with a signed shift in arg's type
		 only if the extended result is at least twice as wide
		 as the arg.  Otherwise, the shift could use up all the
		 ones made by sign-extension and bring in zeros.
		 We can't optimize that case at all, but in most machines
		 it never happens because available widths are 2**N.  */
	      && (!TREE_UNSIGNED (final_type)
		  || unsigned_arg
		  || 2 * TYPE_PRECISION (TREE_TYPE (arg0)) <= TYPE_PRECISION (result_type)))
	    {
	      /* Do an unsigned shift if the operand was zero-extended.  */
	      result_type
		= signed_or_unsigned_type (unsigned_arg,
					   TREE_TYPE (arg0));
	      /* Convert value-to-be-shifted to that type.  */
	      if (TREE_TYPE (op0) != result_type)
		op0 = convert (result_type, op0);
	      converted = 1;
	    }
	}

      /* Comparison operations are shortened too but differently.
	 They identify themselves by setting short_compare = 1.  */

      if (short_compare)
	{
	  /* Don't write &op0, etc., because that would prevent op0
	     from being kept in a register.
	     Instead, make copies of the our local variables and
	     pass the copies by reference, then copy them back afterward.  */
	  tree xop0 = op0, xop1 = op1, xresult_type = result_type;
	  enum tree_code xresultcode = resultcode;
	  tree val 
	    = shorten_compare (&xop0, &xop1, &xresult_type, &xresultcode);
	  if (val != 0)
	    return val;
	  op0 = xop0, op1 = xop1, result_type = xresult_type;
	  resultcode = xresultcode;
	}
    }

  /* At this point, RESULT_TYPE must be nonzero to avoid an error message.
     If CONVERTED is zero, both args will be converted to type RESULT_TYPE.
     Then the expression will be built.
     It will be given type FINAL_TYPE if that is nonzero;
     otherwise, it will be given type RESULT_TYPE.  */

  if (!result_type)
    {
      binary_op_error (error_code);
      return error_mark_node;
    }

  if (! converted)
    {
      if (TREE_TYPE (op0) != result_type)
	op0 = convert (result_type, op0); 
      if (TREE_TYPE (op1) != result_type)
	op1 = convert (result_type, op1); 
    }

  {
    register tree result = build (resultcode, result_type, op0, op1);
    register tree folded;

    folded = fold (result);
    if (folded == result)
      TREE_LITERAL (folded) = TREE_LITERAL (op0) & TREE_LITERAL (op1);
    if (final_type != 0)
      return convert (final_type, folded);
    return folded;
  }
}

/* Return a tree for the sum or difference (RESULTCODE says which)
   of pointer PTROP and integer INTOP.  */

static tree
pointer_int_sum (resultcode, ptrop, intop)
     enum tree_code resultcode;
     register tree ptrop, intop;
{
  tree size_exp;

  register tree result;
  register tree folded = fold (intop);

  /* The result is a pointer of the same type that is being added.  */

  register tree result_type = datatype (ptrop);

  /* Needed to make OOPS V2R3 work.  */
  intop = folded;
  if (TREE_CODE (intop) == INTEGER_CST
      && TREE_INT_CST_LOW (intop) == 0
      && TREE_INT_CST_HIGH (intop) == 0)
    return ptrop;

  if (TREE_CODE (TREE_TYPE (result_type)) == VOID_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	warning ("pointer of type `void *' used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == FUNCTION_TYPE)
    {
      if (pedantic || warn_pointer_arith)
	warning ("pointer to a function used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == METHOD_TYPE)
    {
      if (pedantic)
	warning ("pointer to a method used in arithmetic");
      size_exp = integer_one_node;
    }
  else if (TREE_CODE (TREE_TYPE (result_type)) == OFFSET_TYPE)
    {
      if (pedantic)
	warning ("pointer to a member used in arithmetic");
      size_exp = integer_one_node;
    }
  else
    size_exp = size_in_bytes (TREE_TYPE (result_type));

  /* If what we are about to multiply by the size of the elements
     contains a constant term, apply distributive law
     and multiply that constant term separately.
     This helps produce common subexpressions.  */

  if ((TREE_CODE (intop) == PLUS_EXPR || TREE_CODE (intop) == MINUS_EXPR)
      && ! TREE_LITERAL (intop)
      && TREE_LITERAL (TREE_OPERAND (intop, 1))
      && TREE_LITERAL (size_exp))
    {
      enum tree_code subcode = resultcode;
      if (TREE_CODE (intop) == MINUS_EXPR)
	subcode = (subcode == PLUS_EXPR ? MINUS_EXPR : PLUS_EXPR);
      ptrop = build_binary_op (subcode, ptrop, TREE_OPERAND (intop, 1));
      intop = TREE_OPERAND (intop, 0);
    }

  /* Convert the integer argument to a type the same size as a pointer
     so the multiply won't overflow spuriously.  */

  if (TYPE_PRECISION (TREE_TYPE (intop)) != POINTER_SIZE)
    intop = convert (type_for_size (POINTER_SIZE, 0), intop);

  /* Replace the integer argument
     with a suitable product by the object size.  */

  intop = build_binary_op (MULT_EXPR, intop, size_exp);

  /* Create the sum or difference.  */

  result = build (resultcode, result_type, ptrop, intop);

  folded = fold (result);
  if (folded == result)
    TREE_LITERAL (folded) = TREE_LITERAL (ptrop) & TREE_LITERAL (intop);
  return folded;
}

/* Return a tree for the difference of pointers OP0 and OP1.
   The resulting tree has type int.  */

static tree
pointer_diff (op0, op1)
     register tree op0, op1;
{
  tree dt0 = datatype (op0);
  register tree result, folded;
  tree restype = type_for_size (POINTER_SIZE, 0);

  if (pedantic)
    {
      if (TREE_CODE (TREE_TYPE (dt0)) == VOID_TYPE)
	warning ("pointer of type `void *' used in subtraction");
      if (TREE_CODE (TREE_TYPE (dt0)) == FUNCTION_TYPE)
	warning ("pointer to a function used in subtraction");
      if (TREE_CODE (TREE_TYPE (dt0)) == METHOD_TYPE)
	warning ("pointer to a method used in subtraction");
      if (TREE_CODE (TREE_TYPE (dt0)) == OFFSET_TYPE)
	warning ("pointer to a member used in subtraction");
    }

  /* First do the subtraction as integers;
     then drop through to build the divide operator.  */

  op0 = build_binary_op (MINUS_EXPR,
			 convert (restype, op0), convert (restype, op1));
  op1 = ((TREE_CODE (TREE_TYPE (dt0)) == VOID_TYPE
	  || TREE_CODE (TREE_TYPE (dt0)) == FUNCTION_TYPE
	  || TREE_CODE (TREE_TYPE (dt0)) == METHOD_TYPE
	  || TREE_CODE (TREE_TYPE (dt0)) == OFFSET_TYPE)
	 ? integer_one_node
	 : size_in_bytes (TREE_TYPE (dt0)));

  /* Create the sum or difference.  */

  result = build (EXACT_DIV_EXPR, restype, op0, op1);

  folded = fold (result);
  if (folded == result)
    TREE_LITERAL (folded) = TREE_LITERAL (op0) & TREE_LITERAL (op1);
  return folded;
}

/* Print an error message for invalid operands to arith operation CODE.
   NOP_EXPR is used as a special case (see truthvalue_conversion).  */

static void
binary_op_error (code)
     enum tree_code code;
{
  register char *opname;
  switch (code)
    {
    case NOP_EXPR:
      error ("invalid truth-value expression");
      return;

    case PLUS_EXPR:
      opname = "+"; break;
    case MINUS_EXPR:
      opname = "-"; break;
    case MULT_EXPR:
      opname = "*"; break;
    case MAX_EXPR:
      opname = "max"; break;
    case MIN_EXPR:
      opname = "min"; break;
    case EQ_EXPR:
      opname = "=="; break;
    case NE_EXPR:
      opname = "!="; break;
    case LE_EXPR:
      opname = "<="; break;
    case GE_EXPR:
      opname = ">="; break;
    case LT_EXPR:
      opname = "<"; break;
    case GT_EXPR:
      opname = ">"; break;
    case LSHIFT_EXPR:
      opname = "<<"; break;
    case RSHIFT_EXPR:
      opname = ">>"; break;
    case TRUNC_MOD_EXPR:
      opname = "%"; break;
    case TRUNC_DIV_EXPR:
      opname = "/"; break;
    case BIT_AND_EXPR:
      opname = "&"; break;
    case BIT_IOR_EXPR:
      opname = "|"; break;
    case TRUTH_ANDIF_EXPR:
      opname = "&&"; break;
    case TRUTH_ORIF_EXPR:
      opname = "||"; break;
    case BIT_XOR_EXPR:
      opname = "^"; break;
    }
  error ("invalid operands to binary %s", opname);
}

/* Subroutine of build_binary_op_nodefault, used for comparison operations.
   See if the operands have both been converted from subword integer types
   and, if so, perhaps change them both back to their original type.

   The arguments of this function are all pointers to local variables
   of build_binary_op_nodefault: OP0_PTR is &OP0, OP1_PTR is &OP1,
   RESTYPE_PTR is &RESULT_TYPE and RESCODE_PTR is &RESULTCODE.

   If this function returns nonzero, it means that the comparison has
   a constant value.  What this function returns is an expression for
   that value.  */

static tree
shorten_compare (op0_ptr, op1_ptr, restype_ptr, rescode_ptr)
     tree *op0_ptr, *op1_ptr;
     tree *restype_ptr;
     enum tree_code *rescode_ptr;
{
  register tree type;
  tree op0 = *op0_ptr;
  tree op1 = *op1_ptr;
  int unsignedp0, unsignedp1;
  int real1, real2;
  tree primop0, primop1;
  enum tree_code code = *rescode_ptr;

  /* Throw away any conversions to wider types
     already present in the operands.  */

  primop0 = get_narrower (op0, &unsignedp0);
  primop1 = get_narrower (op1, &unsignedp1);

  /* Handle the case that OP0 does not *contain* a conversion
     but it *requires* conversion to FINAL_TYPE.  */

  if (op0 == primop0 && TREE_TYPE (op0) != *restype_ptr)
    unsignedp0 = TREE_UNSIGNED (TREE_TYPE (op0));
  if (op1 == primop1 && TREE_TYPE (op1) != *restype_ptr)
    unsignedp1 = TREE_UNSIGNED (TREE_TYPE (op1));

  /* If one of the operands must be floated, we cannot optimize.  */
  real1 = TREE_CODE (TREE_TYPE (primop0)) == REAL_TYPE;
  real2 = TREE_CODE (TREE_TYPE (primop1)) == REAL_TYPE;

  /* If first arg is constant, swap the args (changing operation
     so value is preserved), for canonicalization.  */

  if (TREE_LITERAL (primop0))
    {
      register tree tem = primop0;
      register int temi = unsignedp0;
      primop0 = primop1;
      primop1 = tem;
      tem = op0;
      op0 = op1;
      op1 = tem;
      *op0_ptr = op0;
      *op1_ptr = op1;
      unsignedp0 = unsignedp1;
      unsignedp1 = temi;
      temi = real1;
      real1 = real2;
      real2 = temi;

      switch (code)
	{
	case LT_EXPR:
	  code = GT_EXPR;
	  break;
	case GT_EXPR:
	  code = LT_EXPR;
	  break;
	case LE_EXPR:
	  code = GE_EXPR;
	  break;
	case GE_EXPR:
	  code = LE_EXPR;
	  break;
	}
      *rescode_ptr = code;
    }

  /* If comparing an integer against a constant more bits wide,
     maybe we can deduce a value of 1 or 0 independent of the data.
     Or else truncate the constant now
     rather than extend the variable at run time.

     This is only interesting if the constant is the wider arg.
     Also, it is not safe if the constant is unsigned and the
     variable arg is signed, since in this case the variable
     would be sign-extended and then regarded as unsigned.
     Our technique fails in this case because the lowest/highest
     possible unsigned results don't follow naturally from the
     lowest/highest possible values of the variable operand.
     For just EQ_EXPR and NE_EXPR there is another technique that
     could be used: see if the constant can be faithfully represented
     in the other operand's type, by truncating it and reextending it
     and see if that preserves the constant's value.  */

  if (!real1 && !real2
      && TREE_CODE (primop1) == INTEGER_CST
      && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr))
    {
      int min_gt, max_gt, min_lt, max_lt;
      tree maxval, minval;
      /* 1 if comparison is nominally unsigned.  */
      int unsignedp = TREE_UNSIGNED (*restype_ptr);
      tree val;

      type = signed_or_unsigned_type (unsignedp0, TREE_TYPE (primop0));

      maxval = TYPE_MAX_VALUE (type);
      minval = TYPE_MIN_VALUE (type);

      if (unsignedp && !unsignedp0)
	*restype_ptr = signed_type (*restype_ptr);

      if (TREE_TYPE (primop1) != *restype_ptr)
	primop1 = convert (*restype_ptr, primop1);
      if (type != *restype_ptr)
	{
	  minval = convert (*restype_ptr, minval);
	  maxval = convert (*restype_ptr, maxval);
	}

      if (unsignedp && unsignedp0)
	{
	  min_gt = INT_CST_LT_UNSIGNED (primop1, minval);
	  max_gt = INT_CST_LT_UNSIGNED (primop1, maxval);
	  min_lt = INT_CST_LT_UNSIGNED (minval, primop1);
	  max_lt = INT_CST_LT_UNSIGNED (maxval, primop1);
	}
      else
	{
	  min_gt = INT_CST_LT (primop1, minval);
	  max_gt = INT_CST_LT (primop1, maxval);
	  min_lt = INT_CST_LT (minval, primop1);
	  max_lt = INT_CST_LT (maxval, primop1);
	}

      val = 0;
      switch (code)
	{
	case NE_EXPR:
	  if (max_lt || min_gt)
	    val = integer_one_node;
	  break;

	case EQ_EXPR:
	  if (max_lt || min_gt)
	    val = integer_zero_node;
	  break;

	case LT_EXPR:
	  if (max_lt)
	    val = integer_one_node;
	  if (!min_lt)
	    val = integer_zero_node;
	  break;

	case GT_EXPR:
	  if (min_gt)
	    val = integer_one_node;
	  if (!max_gt)
	    val = integer_zero_node;
	  break;

	case LE_EXPR:
	  if (!max_gt)
	    val = integer_one_node;
	  if (min_gt)
	    val = integer_zero_node;
	  break;

	case GE_EXPR:
	  if (!min_lt)
	    val = integer_one_node;
	  if (max_lt)
	    val = integer_zero_node;
	  break;
	}

      /* If primop0 was sign-extended and unsigned comparison specd,
	 we did a signed comparison above using the signed type bounds.
	 But the comparison we output must be unsigned.

	 Also, for inequalities, VAL is no good; but if the signed
	 comparison had *any* fixed result, it follows that the
	 unsigned comparison just tests the sign in reverse
	 (positive values are LE, negative ones GE).
	 So we can generate an unsigned comparison
	 against an extreme value of the signed type.  */

      if (unsignedp && !unsignedp0)
	{
	  if (val != 0)
	    switch (code)
	      {
	      case LT_EXPR:
	      case GE_EXPR:
		primop1 = TYPE_MIN_VALUE (type);
		val = 0;
		break;

	      case LE_EXPR:
	      case GT_EXPR:
		primop1 = TYPE_MAX_VALUE (type);
		val = 0;
		break;
	      }
	  type = unsigned_type (type);
	}

      if (max_lt && !unsignedp0)
	{
	  /* This is the case of (char)x >?< 0x80, which people used to use
	     expecting old C compilers to change the 0x80 into -0x80.  */
	  if (val == integer_zero_node)
	    warning ("comparison is always 0 due to limited range of data type");
	  if (val == integer_one_node)
	    warning ("comparison is always 1 due to limited range of data type");
	}

      if (val != 0)
	{
	  /* Don't forget to evaluate PRIMOP0 if it has side effects.  */
	  if (TREE_VOLATILE (primop0))
	    return build (COMPOUND_EXPR, TREE_TYPE (val), primop0, val);
	  return val;
	}

      /* Value is not predetermined, but do the comparison
	 in the type of the operand that is not constant.
	 TYPE is already properly set.  */
    }
  else if (real1 && real2
	   && TYPE_PRECISION (TREE_TYPE (primop0)) == TYPE_PRECISION (TREE_TYPE (primop1)))
    type = TREE_TYPE (primop0);

  /* If args' natural types are both narrower than nominal type
     and both extend in the same manner, compare them
     in the type of the wider arg.
     Otherwise must actually extend both to the nominal
     common type lest different ways of extending
     alter the result.
     (eg, (short)-1 == (unsigned short)-1  should be 0.)  */

  else if (unsignedp0 == unsignedp1 && real1 == real2
	   && TYPE_PRECISION (TREE_TYPE (primop0)) < TYPE_PRECISION (*restype_ptr)
	   && TYPE_PRECISION (TREE_TYPE (primop1)) < TYPE_PRECISION (*restype_ptr))
    {
      type = commontype (TREE_TYPE (primop0), TREE_TYPE (primop1));
      type = signed_or_unsigned_type (unsignedp0
				      || TREE_UNSIGNED (*restype_ptr),
				      type);
      /* Make sure shorter operand is extended the right way
	 to match the longer operand.  */
      primop0 = convert (signed_or_unsigned_type (unsignedp0, TREE_TYPE (primop0)),
			 primop0);
      primop1 = convert (signed_or_unsigned_type (unsignedp1, TREE_TYPE (primop1)),
			 primop1);
    }
  else
    {
      /* Here we must do the comparison on the nominal type
	 using the args exactly as we received them.  */
      type = *restype_ptr;
      primop0 = op0;
      primop1 = op1;
    }

  *op0_ptr = convert (type, primop0);
  *op1_ptr = convert (type, primop1);

  *restype_ptr = integer_type_node;

  return 0;
}

/* Handle the case of taking the address of a COMPONENT_REF.
   Called by `build_unary_op' and `build_up_reference'.

   ARG is the COMPONENT_REF whose address we want.
   ARGTYPE is the pointer type that this address should have.
   MSG is an error message to print if this COMPONENT_REF is not
   addressable (such as a bitfield).  */

tree
build_component_addr (arg, argtype, msg)
     tree arg, argtype;
     char *msg;
{
  tree field = TREE_OPERAND (arg, 1);
  tree basetype = decl_type_context (field);
  tree rval = build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0), 0);

  if (TREE_PACKED (field))
    {
      error (msg, IDENTIFIER_POINTER (DECL_NAME (field)));
      return error_mark_node;
    }

  if (TREE_CODE (field) == FIELD_DECL
      && (TYPE_USES_MULTIPLE_INHERITANCE (basetype)
	  || TYPE_USES_VIRTUAL_BASECLASSES (basetype)))
    /* Can't convert directly to ARGTYPE, since that
       may have the same pointer type as one of our
       baseclasses.  */
    rval = build1 (NOP_EXPR, argtype,
		   convert_pointer_to (basetype, rval));
  else
    /* This conversion is harmless.  */
    rval = convert (argtype, rval);

  if (DECL_OFFSET (field) != 0)
    {
      tree offset = build_int_2 ((DECL_OFFSET (field) / BITS_PER_UNIT), 0);
      TREE_TYPE (offset) = argtype;
      rval = fold (build (PLUS_EXPR, argtype, rval, offset));
    }
  return rval;
}
   
/* Construct and perhaps optimize a tree representation
   for a unary operation.  CODE, a tree_code, specifies the operation
   and XARG is the operand.  */

tree
build_x_unary_op (code, xarg)
     enum tree_code code;
     tree xarg;
{
  tree rval;

  if (rval = build_opfncall (code, LOOKUP_PROTECT, xarg))
    return rval;
  rval = build_unary_op (code, xarg, 0);
  if (rval == error_mark_node)
    build_opfncall (code, LOOKUP_NORMAL, xarg);
  return rval;
}

/* C++: Must handle pointers to members.

   Perhaps type instantiation should be extended to handle conversion
   from aggregates to types we don't yet know we want?  (Or are those
   cases typically errors which should be reported?)

   NOCONVERT nonzero suppresses the default promotions
   (such as from short to int).  */
tree
build_unary_op (code, xarg, noconvert)
     enum tree_code code;
     tree xarg;
     int noconvert;
{
  /* No default_conversion here.  It causes trouble for ADDR_EXPR.  */
  register tree arg = xarg;
  register tree argtype = 0;
  register enum tree_code typecode = TREE_CODE (TREE_TYPE (arg));
  char *errstring = NULL;
  tree val;
  int isaggrtype;

  if (typecode == ERROR_MARK)
    return error_mark_node;

  if (typecode == REFERENCE_TYPE && code != ADDR_EXPR && ! noconvert)
    {
      arg = convert_from_reference (arg);
      typecode = TREE_CODE (TREE_TYPE (arg));
    }

  if (typecode == ENUMERAL_TYPE)
    typecode = INTEGER_TYPE;

  isaggrtype = IS_AGGR_TYPE_CODE (typecode);

  switch (code)
    {
    case CONVERT_EXPR:
      /* This is used for unary plus, because a CONVERT_EXPR
	 is enough to prevent anybody from looking inside for
	 associativity, but won't generate any code.  */
      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE))
        errstring = "wrong type argument to unary plus";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case NEGATE_EXPR:
      if (isaggrtype)
	{
	  if (!noconvert)
	    arg = default_conversion (arg);
	  else
	    {
	      error_with_aggr_type (TREE_TYPE (arg), "type conversion for type `%s' not allowed");
	      return error_mark_node;
	    }
	  typecode = TREE_CODE (TREE_TYPE (arg));
	  noconvert = 1;
	}

      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE))
        errstring = "wrong type argument to unary minus";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case BIT_NOT_EXPR:
      if (isaggrtype)
	{
	  if (!noconvert)
	    arg = default_conversion (arg);
	  else
	    {
	      error_with_aggr_type (TREE_TYPE (arg), "type conversion for type `%s' not allowed");
	      return error_mark_node;
	    }
	  typecode = TREE_CODE (TREE_TYPE (arg));
	  noconvert = 1;
	}

      if (typecode != INTEGER_TYPE)
        errstring = "wrong type argument to bit-complement";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case ABS_EXPR:
      if (isaggrtype)
	{
	  if (!noconvert)
	    arg = default_conversion (arg);
	  else
	    {
	      error_with_aggr_type (TREE_TYPE (arg), "type conversion for type `%s' not allowed");
	      return error_mark_node;
	    }
	  typecode = TREE_CODE (TREE_TYPE (arg));
	  noconvert = 1;
	}

      if (!(typecode == INTEGER_TYPE || typecode == REAL_TYPE))
        errstring = "wrong type argument to abs";
      else if (!noconvert)
	arg = default_conversion (arg);
      break;

    case TRUTH_NOT_EXPR:
      if (isaggrtype)
	{
	  arg = truthvalue_conversion (arg);
	  typecode = TREE_CODE (TREE_TYPE (arg));
	}

      if (typecode != INTEGER_TYPE
	  && typecode != REAL_TYPE && typecode != POINTER_TYPE
	  /* These will convert to a pointer.  */
	  && typecode != ARRAY_TYPE && typecode != FUNCTION_TYPE)
	{
	  errstring = "wrong type argument to unary exclamation mark";
	  break;
	}
      arg = truthvalue_conversion (arg);
      val = invert_truthvalue (arg);
      if (val) return val;
      break;

    case NOP_EXPR:
      break;
      
    case PREINCREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case PREDECREMENT_EXPR:
    case POSTDECREMENT_EXPR:
      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */

      val = unary_complex_lvalue (code, arg);
      if (val != 0)
	return val;

      /* Report invalid types.  */

      if (isaggrtype)
	{
	  arg = default_conversion (arg);
	  typecode = TREE_CODE (TREE_TYPE (arg));
	}

      if (typecode != POINTER_TYPE
	  && typecode != INTEGER_TYPE && typecode != REAL_TYPE)
	{
	  if (code == PREINCREMENT_EXPR || code == POSTINCREMENT_EXPR)
	    errstring ="wrong type argument to increment";
	  else
	    errstring ="wrong type argument to decrement";
	  break;
	}

      /* Report something read-only.  */

      if (TREE_READONLY (arg))
	readonly_warning_or_error (arg, 
				   ((code == PREINCREMENT_EXPR
				     || code == POSTINCREMENT_EXPR)
				    ? "increment" : "decrement"));

      {
	register tree inc;
	tree result_type = TREE_TYPE (arg);

	arg = get_unwidened (arg, 0);
	argtype = TREE_TYPE (arg);

	/* Compute the increment.  */

	if (typecode == POINTER_TYPE)
	  {
	    if (pedantic && (TREE_CODE (argtype) == FUNCTION_TYPE
			     || TREE_CODE (argtype) == METHOD_TYPE
			     || TREE_CODE (argtype) == VOID_TYPE
			     || TREE_CODE (argtype) == OFFSET_TYPE))
	      warning ("wrong type argument to %s",
		       ((code == PREINCREMENT_EXPR
			 || code == POSTINCREMENT_EXPR)
			? "increment" : "decrement"));
	    inc = c_sizeof_nowarn (TREE_TYPE (argtype));
	  }
	else
	  inc = integer_one_node;

	inc = convert (argtype, inc);

	/* Handle incrementing a cast-expression.  */

	if (!pedantic)
	  switch (TREE_CODE (arg))
	    {
	    case NOP_EXPR:
	    case CONVERT_EXPR:
	    case FLOAT_EXPR:
	    case FIX_TRUNC_EXPR:
	    case FIX_FLOOR_EXPR:
	    case FIX_ROUND_EXPR:
	    case FIX_CEIL_EXPR:
	      {
		tree incremented, modify, value;
		arg = stabilize_reference (arg);
		if (code == PREINCREMENT_EXPR || code == PREDECREMENT_EXPR)
		  value = arg;
		else
		  value = save_expr (arg);
		incremented = build (((code == PREINCREMENT_EXPR
				       || code == POSTINCREMENT_EXPR)
				      ? PLUS_EXPR : MINUS_EXPR),
				     argtype, value, inc);
		TREE_VOLATILE (incremented) = 1;
		modify = build_modify_expr (arg, NOP_EXPR, incremented);
		return build (COMPOUND_EXPR, TREE_TYPE (arg), modify, value);
	      }
	    }

	if (TREE_CODE (arg) == OFFSET_REF)
	  arg = resolve_offset_ref (arg);

	/* Complain about anything else that is not a true lvalue.  */
	if (!lvalue_or_else (arg, ((code == PREINCREMENT_EXPR
				    || code == POSTINCREMENT_EXPR)
				   ? "increment" : "decrement")))
	  return error_mark_node;

	val = build (code, TREE_TYPE (arg), arg, inc);
	TREE_VOLATILE (val) = 1;
	return convert (result_type, val);
      }

    case ADDR_EXPR:
      /* Note that this operation never does default_conversion
	 regardless of NOCONVERT.  */

      if (TREE_CODE (arg) == REFERENCE_EXPR)
	{
	  error ("references are not lvalues");
	  return error_mark_node;
	}
      else if (typecode == REFERENCE_TYPE)
	return build1 (REFERENCE_EXPR, build_pointer_type (TREE_TYPE (TREE_TYPE (arg))), arg);

      /* Let &* cancel out to simplify resulting code.  */
      if (TREE_CODE (arg) == INDIRECT_REF)
	{
	  /* Keep `default_conversion' from converting if
	     ARG is of REFERENCE_TYPE.  */
	  arg = TREE_OPERAND (arg, 0);
	  if (TREE_CODE (TREE_TYPE (arg)) == REFERENCE_TYPE)
	    {
	      if (TREE_CODE (arg) == VAR_DECL && DECL_INITIAL (arg))
		arg = DECL_INITIAL (arg);
	      arg = build1 (REFERENCE_EXPR, build_pointer_type (TREE_TYPE (TREE_TYPE (arg))), arg);
	      TREE_LITERAL (arg) = TREE_LITERAL (TREE_OPERAND (arg, 0));
	    }
	  return arg;
	}

      /* For &x[y], return x+y */
      if (TREE_CODE (arg) == ARRAY_REF)
	{
	  if (mark_addressable (TREE_OPERAND (arg, 0)) == 0)
	    return error_mark_node;
	  return build_binary_op (PLUS_EXPR, TREE_OPERAND (arg, 0),
				  TREE_OPERAND (arg, 1));
	}

      /* Uninstantiated types are all functions.  Taking the
	 address of a function is a no-op, so just return the
	 arguemnt.  */

      if (TREE_CODE (arg) == OP_IDENTIFIER)
	/* We don't know the type yet, so just work around the problem.
	   We know that this will resolve to an lvalue.  */
	return build1 (ADDR_EXPR, unknown_type_node, arg);

      if (TREE_CODE (arg) == TREE_LIST)
	{
	  /* Look at methods with only this name.  */
	  if (TREE_CODE (TREE_VALUE (arg)) == FUNCTION_DECL)
	    {
	      tree targ = TREE_VALUE (arg);

	      /* If this function is unique, or it is a unique
		 constructor, we can takes its address easily.  */
	      if (TREE_CHAIN (targ) == NULL_TREE
		  || (DESTRUCTOR_NAME_P (DECL_NAME (targ))
		      && TREE_CHAIN (TREE_CHAIN (targ)) == NULL_TREE))
		{
		  if (TREE_CHAIN (targ))
		    targ = TREE_CHAIN (targ);
		  targ = build (OFFSET_REF, TREE_TYPE (targ), C_C_D, targ);

		  val = unary_complex_lvalue (ADDR_EXPR, targ);
		  if (val)
		    return val;
		}
	      return build1 (ADDR_EXPR, unknown_type_node, arg);
	    }
	  if (TREE_CHAIN (arg) == NULL_TREE
	      && TREE_CHAIN (TREE_VALUE (TREE_VALUE (arg))) == NULL_TREE)
	    {
	      /* Unique overloaded member function.  */
	      return build_unary_op (ADDR_EXPR, TREE_VALUE (TREE_VALUE (arg)), 0);
	    }
	  return build1 (ADDR_EXPR, unknown_type_node, arg);
	}

      /* Handle complex lvalues (when permitted)
	 by reduction to simpler cases.  */
      val = unary_complex_lvalue (code, arg);
      if (val != 0)
	return val;

      /* Address of a cast is just a cast of the address
	 of the operand of the cast.  */
      switch (TREE_CODE (arg))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case FLOAT_EXPR:
	case FIX_TRUNC_EXPR:
	case FIX_FLOOR_EXPR:
	case FIX_ROUND_EXPR:
	case FIX_CEIL_EXPR:
	  if (pedantic)
	    warning ("ANSI C forbids the address of a cast expression");
	  return convert (build_pointer_type (TREE_TYPE (arg)),
			  build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 0), 0));
	}

      /* Allow the address of a constructor if all the elements
	 are constant.  */
      if (TREE_CODE (arg) == CONSTRUCTOR && TREE_LITERAL (arg))
	;
      /* Anything not already handled and not a true memory reference
	 is an error.  */
      else if (typecode != FUNCTION_TYPE
	       && typecode != METHOD_TYPE
	       && !lvalue_or_else (arg, "unary `&'"))
	return error_mark_node;

      /* Ordinary case; arg is a COMPONENT_REF or a decl.  */
      argtype = TREE_TYPE (arg);
      if (TREE_READONLY (arg) || TREE_THIS_VOLATILE (arg))
	argtype = build_type_variant (argtype,
				      TREE_READONLY (arg),
				      TREE_THIS_VOLATILE (arg));

      argtype = build_pointer_type (argtype);

      if (mark_addressable (arg) == 0)
	return error_mark_node;

      {
	tree addr;

	if (TREE_CODE (arg) == COMPONENT_REF)
	  addr = build_component_addr (arg, argtype,
				       "attempt to take address of bit-field structure member `%s'");
	else
	  addr = build1 (code, argtype, arg);

	/* Address of a static or external variable or
	   function counts as a constant */
	TREE_LITERAL (addr) = staticp (arg);
	return addr;
      }
    }

  if (!errstring)
    {
      if (argtype == 0)
	argtype = TREE_TYPE (arg);
      return fold (build1 (code, argtype, arg));
    }

  error (errstring);
  return error_mark_node;
}

/* If CONVERSIONS is a conversion expression or a nested sequence of such,
   convert ARG with the same conversions in the same order
   and return the result.  */

static tree
convert_sequence (conversions, arg)
     tree conversions;
     tree arg;
{
  switch (TREE_CODE (conversions))
    {
    case NOP_EXPR:
    case CONVERT_EXPR:
    case FLOAT_EXPR:
    case FIX_TRUNC_EXPR:
    case FIX_FLOOR_EXPR:
    case FIX_ROUND_EXPR:
    case FIX_CEIL_EXPR:
      return convert (TREE_TYPE (conversions),
		      convert_sequence (TREE_OPERAND (conversions, 0),
					arg));

    default:
      return arg;
    }
}

/* Apply unary lvalue-demanding operator CODE to the expression ARG
   for certain kinds of expressions which are not really lvalues
   but which we can accept as lvalues.

   If ARG is not a kind of expression we can handle, return zero.  */
   
tree
unary_complex_lvalue (code, arg)
     enum tree_code code;
     tree arg;
{
  /* Handle (a, b) used as an "lvalue".  */
  if (TREE_CODE (arg) == COMPOUND_EXPR)
    {
      tree real_result = build_unary_op (code, TREE_OPERAND (arg, 1), 0);
      return build (COMPOUND_EXPR, TREE_TYPE (real_result),
		    TREE_OPERAND (arg, 0), real_result);
    }

  /* Handle (a ? b : c) used as an "lvalue".  */
  if (TREE_CODE (arg) == COND_EXPR)
    return (build_conditional_expr
	    (TREE_OPERAND (arg, 0),
	     build_unary_op (code, TREE_OPERAND (arg, 1), 0),
	     build_unary_op (code, TREE_OPERAND (arg, 2), 0)));

  if (code != ADDR_EXPR)
    return 0;

  /* Handle (a = b) used as an "lvalue" for `&'.  */
  if (TREE_CODE (arg) == MODIFY_EXPR
      || TREE_CODE (arg) == INIT_EXPR)
    {
      tree real_result = build_unary_op (code, TREE_OPERAND (arg, 0), 0);
      return build (COMPOUND_EXPR, TREE_TYPE (real_result), arg, real_result);
    }

  if (TREE_CODE (arg) == WITH_CLEANUP_EXPR)
    {
      tree real_result = build_unary_op (code, TREE_OPERAND (arg, 0), 0);
      real_result = build (WITH_CLEANUP_EXPR, TREE_TYPE (real_result),
			   real_result, 0, TREE_OPERAND (arg, 2));
      return real_result;
    }

  if (TREE_CODE (TREE_TYPE (arg)) == FUNCTION_TYPE
      || TREE_CODE (TREE_TYPE (arg)) == METHOD_TYPE
      || TREE_CODE (TREE_TYPE (arg)) == OFFSET_TYPE)
    {
      /* The representation of something of type OFFSET_TYPE
	 is really the representation of a pointer to it.
	 Here give the representation its true type.  */
      tree t;
      tree offset;

      assert (TREE_CODE (arg) != SCOPE_REF);

      if (TREE_CODE (arg) != OFFSET_REF)
	return 0;

      t = TREE_OPERAND (arg, 1);
      if (TREE_OPERAND (arg, 0)
	  && (TREE_CODE (TREE_OPERAND (arg, 0)) != NOP_EXPR
	      || TREE_OPERAND (TREE_OPERAND (arg, 0), 0) != error_mark_node))
	{
	  /* Don't know if this should return address to just
	     _DECL, or actual address resolved in this expression.  */
	  sorry ("address of bound pointer-to-member expression");
	  return error_mark_node;
	}

      if (TREE_CODE (t) == FUNCTION_DECL)
	{
	  tree context = NULL_TREE;

	  if (DECL_VIRTUAL_P (t)
#ifdef SOS
	      || flag_all_virtual == 2
#endif
	      || (flag_all_virtual == 1
		  && ((context = decl_type_context (t))
		      && (TYPE_OVERLOADS_METHOD_CALL_EXPR (context)
			  || TYPE_NEEDS_WRAPPER (context))
		      && ! DECL_CONSTRUCTOR_P (t))))
	    {
	      offset = copy_node (DECL_VINDEX (t));
	      TREE_TYPE (offset) = build_pointer_type (TREE_TYPE (arg));
	    }
	  else
	    offset = build_unary_op (ADDR_EXPR, t, 0);

	  return offset;
	}
      if (TREE_CODE (t) == VAR_DECL)
	{
	  if (TREE_STATIC (t))
	    offset = build_unary_op (ADDR_EXPR, t, 0);
	  else
	    return 0;
	}
      else
	{
	  /* Can't build a pointer to member if the member must
	     go through virtual base classes.  */
	  if (virtual_member (DECL_FIELD_CONTEXT (t),
			      CLASSTYPE_VBASECLASSES (TREE_TYPE (TREE_OPERAND (arg, 0)))))
	    {
	      sorry ("pointer to member via virtual baseclass");
	      return error_mark_node;
	    }
	  /* @@ What is the correct machine-independent way to do this?  */
	  offset = build_int_2 (DECL_OFFSET (t) / DECL_SIZE_UNIT (t), 0);
	  TREE_TYPE (offset) = build_pointer_type (TREE_TYPE (arg));
	  return offset;
	}
    }

  if (TREE_CODE (arg) == OFFSET_REF)
    {
      tree left = TREE_OPERAND (arg, 0), left_addr;
      tree right_addr = build_unary_op (ADDR_EXPR, TREE_OPERAND (arg, 1), 0);

      if (left == 0)
	if (current_class_decl)
	  left_addr = current_class_decl;
	else
	  {
	    error ("no `this' for pointer to member");
	    return error_mark_node;
	  }
      else
	left_addr = build_unary_op (ADDR_EXPR, left, 0);

    return build (PLUS_EXPR, build_pointer_type (TREE_TYPE (arg)),
		  build1 (NOP_EXPR, integer_type_node, left_addr),
		  build1 (NOP_EXPR, integer_type_node, right_addr));
    }


  /* We permit compiler to make function calls returning
     objects of aggregate type look like lvalues.  */
  if (TREE_CODE (arg) == CALL_EXPR && IS_AGGR_TYPE (TREE_TYPE (arg)))
    {
      tree temp = build_cplus_new (TREE_TYPE (arg), arg);
      return build1 (ADDR_EXPR, TYPE_POINTER_TO (TREE_TYPE (arg)), temp);
    }

  /* Don't let anything else be handled specially.  */
  return 0;
}

/* Prepare expr to be an argument of a TRUTH_NOT_EXPR,
   or validate its data type for an `if' or `while' statement or ?..: exp.

   This preparation consists of taking the ordinary
   representation of an expression expr and producing a valid tree
   boolean expression describing whether expr is nonzero.  We could
   simply always do build_binary_op (NE_EXPR, expr, integer_zero_node),
   but we optimize comparisons, &&, ||, and !  */

tree
truthvalue_conversion (expr)
     tree expr;
{
  register enum tree_code form;

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since EXPR is being used in non-lvalue context.  */
  if (TREE_CODE (expr) == NOP_EXPR
      && TREE_TYPE (expr) == TREE_TYPE (TREE_OPERAND (expr, 0)))
    expr = TREE_OPERAND (expr, 0);

  form = TREE_CODE (expr);

  if (form == EQ_EXPR && integer_zerop (TREE_OPERAND (expr, 1)))
    return build_unary_op (TRUTH_NOT_EXPR, TREE_OPERAND (expr, 0), 0);

  /* A one-bit unsigned bit-field is already acceptable.  */
  if (form == COMPONENT_REF
      && 1 == TREE_INT_CST_LOW (DECL_SIZE (TREE_OPERAND (expr, 1)))
      && 1 == DECL_SIZE_UNIT (TREE_OPERAND (expr, 1))
      && TREE_UNSIGNED (TREE_OPERAND (expr, 1)))
    return expr;

  if (form == TRUTH_ANDIF_EXPR || form == TRUTH_ORIF_EXPR
      || form == TRUTH_AND_EXPR || form == TRUTH_OR_EXPR
      || form == TRUTH_NOT_EXPR
      || form == EQ_EXPR || form == NE_EXPR
      || form == LE_EXPR || form == GE_EXPR
      || form == LT_EXPR || form == GT_EXPR
      || form == ERROR_MARK)
    return expr;

  /* Unary minus has no effect on whether its argument is nonzero.  */
  if (form == NEGATE_EXPR)
    return truthvalue_conversion (TREE_OPERAND (expr, 0));

  /* Distribute the conversion into the arms of a COND_EXPR.  */
  if (form == COND_EXPR)
    return build (COND_EXPR, TREE_TYPE (expr),
		  TREE_OPERAND (expr, 0),
		  truthvalue_conversion (TREE_OPERAND (expr, 1)),
		  truthvalue_conversion (TREE_OPERAND (expr, 2)));

  /* Sign-extension and zero-extension has no effect.  */
  if (form == NOP_EXPR
      && TREE_CODE (TREE_TYPE (expr)) == INTEGER_TYPE
      && (TYPE_PRECISION (TREE_TYPE (expr))
	  > TYPE_PRECISION (TREE_TYPE (TREE_OPERAND (expr, 0)))))
    return truthvalue_conversion (TREE_OPERAND (expr, 0));

  return build_binary_op (NE_EXPR, expr, integer_zero_node);
}

/* Return a simplified tree node for the truth-negation of ARG
   (perhaps by altering ARG).
   If it can't be simplified, return 0.  */

static tree
invert_truthvalue (arg)
     tree arg;
{
  switch (TREE_CODE (arg))
    {
    case NE_EXPR:
      TREE_SET_CODE (arg, EQ_EXPR);
      return arg;

    case EQ_EXPR:
      TREE_SET_CODE (arg, NE_EXPR);
      return arg;

    case GE_EXPR:
      TREE_SET_CODE (arg, LT_EXPR);
      return arg;

    case GT_EXPR:
      TREE_SET_CODE (arg, LE_EXPR);
      return arg;

    case LE_EXPR:
      TREE_SET_CODE (arg, GT_EXPR);
      return arg;

    case LT_EXPR:
      TREE_SET_CODE (arg, GE_EXPR);
      return arg;

#if 0
    case TRUTH_AND_EXPR:
      return build (TRUTH_OR_EXPR, TREE_TYPE (arg),
		    build_unary_op (TRUTH_NOT_EXPR,
				    TREE_OPERAND (arg, 0), 0),
		    build_unary_op (TRUTH_NOT_EXPR,
				    TREE_OPERAND (arg, 1), 0));

    case TRUTH_OR_EXPR:
      return build (TRUTH_AND_EXPR, TREE_TYPE (arg),
		    build_unary_op (TRUTH_NOT_EXPR,
				    TREE_OPERAND (arg, 0), 0),
		    build_unary_op (TRUTH_NOT_EXPR,
				    TREE_OPERAND (arg, 1), 0));
#endif

    case TRUTH_ANDIF_EXPR:
      return build (TRUTH_ORIF_EXPR, TREE_TYPE (arg),
		    build_unary_op (TRUTH_NOT_EXPR,
				    TREE_OPERAND (arg, 0), 0),
		    build_unary_op (TRUTH_NOT_EXPR,
				    TREE_OPERAND (arg, 1), 0));

    case TRUTH_ORIF_EXPR:
      return build (TRUTH_ANDIF_EXPR, TREE_TYPE (arg),
		    build_unary_op (TRUTH_NOT_EXPR,
				    TREE_OPERAND (arg, 0), 0),
		    build_unary_op (TRUTH_NOT_EXPR,
				    TREE_OPERAND (arg, 1), 0));

    case TRUTH_NOT_EXPR:
      return TREE_OPERAND (arg, 0);
    }
  return 0;
}

/* Mark EXP saying that we need to be able to take the
   address of it; it should not be allocated in a register.

   Return 1 if taking address of this expression is ok.
   Return 0 otherwise.

   C++: we do not allow `current_class_decl' to be addressable.  */

int
mark_addressable (exp)
     tree exp;
{
  register tree x = exp;

  if (TREE_ADDRESSABLE (x) == 1)
    return 1;

  while (1)
    switch (TREE_CODE (x))
      {
      case ADDR_EXPR:
      case COMPONENT_REF:
      case ARRAY_REF:
	x = TREE_OPERAND (x, 0);
	break;

      case PARM_DECL:
	if (x == current_class_decl)
	  {
	    error ("address of `this' not available");
	    TREE_ADDRESSABLE (x) = 1; /* so compiler doesn't die later */
	    put_var_into_stack (x);
	    return 1;
	  }
      case VAR_DECL:
	if (TREE_STATIC (x)
	    && TREE_READONLY (x)
	    && DECL_RTL (x) != 0
	    && ! memory_operand (DECL_RTL (x), DECL_MODE (x)))
	  {
	    /* We thought this would make a good constant variable,
	       but we were wrong.  */
	    TREE_ASM_WRITTEN (x) = 0;
	    DECL_RTL (x) = 0;
	    rest_of_decl_compilation (x, 0, IDENTIFIER_LOCAL_VALUE (x) == 0, 0);
	    TREE_ADDRESSABLE (x) = 1;
	    return 1;
	  }
	/* Caller should not be trying to mark initialized
	   constant fields addressable.  */
	assert (DECL_LANG_SPECIFIC (x) == 0 || DECL_IN_AGGR_P (x) == 0 || TREE_STATIC (x));

      case CONST_DECL:
	if (TREE_REGDECL (x))
	  {
	    if (TREE_PUBLIC (x))
	      {
		error ("address of global register variable `%s' requested",
		       IDENTIFIER_POINTER (DECL_NAME (x)));
		return 0;
	      }
	    warning ("address requested for `%s', which is declared `register'",
		     IDENTIFIER_POINTER (DECL_NAME (x)));
	  }
	put_var_into_stack (x);
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case RESULT_DECL:
	put_var_into_stack (x);
	TREE_ADDRESSABLE (x) = 1;
	return 1;

      case FUNCTION_DECL:
	if (TREE_INLINE (x))
	  mark_inline_for_output (x);
	TREE_ADDRESSABLE (x) = 1;
	TREE_USED (x) = 1;
	TREE_ADDRESSABLE (DECL_NAME (x)) = 1;
	return 1;

      default:
	return 1;
    }
}

/* Build and return a conditional expression IFEXP ? OP1 : OP2.  */

tree
build_x_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
{
  tree rval;

  if (op1 != 0 && (rval = build_opfncall (COND_EXPR, LOOKUP_PROTECT, ifexp, op1, op2)))
    return rval;
  rval = build_conditional_expr (ifexp, op1, op2);
  if (op1 != 0 && rval == error_mark_node)
    build_opfncall (COND_EXPR, LOOKUP_NORMAL, ifexp, op1, op2);
  return rval;
}

tree
build_conditional_expr (ifexp, op1, op2)
     tree ifexp, op1, op2;
{
  register tree type1;
  register tree type2;
  register enum tree_code code1;
  register enum tree_code code2;
  register tree result_type = NULL_TREE;

  /* If second operand is omitted, it is the same as the first one;
     make sure it is calculated only once.  */
  if (op1 == 0)
    {
      if (pedantic)
	warning ("ANSI C forbids omitting the middle term of a ?: expression");
      ifexp = op1 = save_expr (ifexp);
    }

  ifexp = truthvalue_conversion (default_conversion (ifexp));

  if (TREE_CODE (ifexp) == ERROR_MARK)
    return error_mark_node;

  op1 = require_instantiated_type (TREE_TYPE (op2), op1, error_mark_node);
  if (op1 == error_mark_node)
    return error_mark_node;
  op2 = require_instantiated_type (TREE_TYPE (op1), op2, error_mark_node);
  if (op2 == error_mark_node)
    return error_mark_node;

  /* C++: REFERENCE_TYPES must be dereferenced.  */
  type1 = TREE_TYPE (op1);
  code1 = TREE_CODE (type1);
  type2 = TREE_TYPE (op2);
  code2 = TREE_CODE (type2);

  if (code1 == REFERENCE_TYPE)
    {
      op1 = convert_from_reference (op1);
      type1 = TREE_TYPE (op1);
      code1 = TREE_CODE (type1);
    }
  if (code2 == REFERENCE_TYPE)
    {
      op2 = convert_from_reference (op2);
      type2 = TREE_TYPE (op2);
      code2 = TREE_CODE (type2);
    }

  /* Don't promote the operands separately if they promote
     the same way.  Return the unpromoted type and let the combined
     value get promoted if necessary.  */

  if (TYPE_MAIN_VARIANT (type1) == TYPE_MAIN_VARIANT (type2)
      && code2 != ARRAY_TYPE
#if 0
      /* For C++, let the enumeral type come through.  */
      && code2 != ENUMERAL_TYPE
#endif
      && code2 != FUNCTION_TYPE
      && code2 != METHOD_TYPE)
    {
      if (TREE_LITERAL (ifexp)
	  && (TREE_CODE (ifexp) == INTEGER_CST
	      || TREE_CODE (ifexp) == ADDR_EXPR))
	return (integer_zerop (ifexp) ? op2 : op1);

      if (TREE_CODE (op1) == CONST_DECL)
	op1 = DECL_INITIAL (op1);
      else if (TREE_READONLY_DECL_P (op1))
	op1 = decl_constant_value (op1);
      if (TREE_CODE (op2) == CONST_DECL)
	op2 = DECL_INITIAL (op2);
      else if (TREE_READONLY_DECL_P (op2))
	op2 = decl_constant_value (op2);
      if (type1 != type2)
	{
	  int constp = TREE_READONLY (type1) || TREE_READONLY (type2);
	  int volatilep = TREE_VOLATILE (type1) || TREE_VOLATILE (type2);
	  type1 = build_type_variant (type1, constp, volatilep);
	}
      return build (COND_EXPR, type1, ifexp, op1, op2);
    }

  /* They don't match; promote them both and then try to reconcile them.
     But don't permit mismatching enum types.  */
  if (code1 == ENUMERAL_TYPE)
    {
      if (code2 == ENUMERAL_TYPE)
	{
	  message_2_types (error, "enumeral mismatch in conditional expression: `%s' vs `%s'", type1, type2);
	  return error_mark_node;
	}
      else if (extra_warnings && ! IS_AGGR_TYPE_CODE (code2))
	warning ("enumeral and non-enumeral type in conditional expression");
    }
  else if (extra_warnings
	   && code2 == ENUMERAL_TYPE && ! IS_AGGR_TYPE_CODE (code1))
    warning ("enumeral and non-enumeral type in conditional expression");

  if (code1 != VOID_TYPE)
    {
      op1 = default_conversion (op1);
      type1 = TREE_TYPE (op1);
      code1 = TREE_CODE (type1);
    }
  if (code2 != VOID_TYPE)
    {
      op2 = default_conversion (op2);
      type2 = TREE_TYPE (op2);
      code2 = TREE_CODE (type2);
    }

  /* Quickly detect the usual case where op1 and op2 have the same type
     after promotion.  */
  if (TYPE_MAIN_VARIANT (type1) == TYPE_MAIN_VARIANT (type2))
    {
      if (type1 != type2)
	{
	  int constp = TREE_READONLY (type1) || TREE_READONLY (type2);
	  int volatilep = TREE_VOLATILE (type1) || TREE_VOLATILE (type2);
	  type1 = build_type_variant (type1, constp, volatilep);
	}
      result_type = type1;
    }
  else if ((code1 == INTEGER_TYPE || code1 == REAL_TYPE)
           && (code2 == INTEGER_TYPE || code2 == REAL_TYPE))
    {
      result_type = commontype (type1, type2);
    }
  else if (code1 == VOID_TYPE || code2 == VOID_TYPE)
    {
      if (pedantic && (code1 != VOID_TYPE || code2 != VOID_TYPE))
	warning ("ANSI C forbids conditional expr with only one void side");
      result_type = void_type_node;
    }
  else if (code1 == POINTER_TYPE && code2 == POINTER_TYPE)
    {
      if (comp_target_types (type1, type2, 1))
	result_type = commontype (type1, type2);
      else if (TYPE_MAIN_VARIANT (TREE_TYPE (type1)) == void_type_node)
	{
	  if (pedantic && TREE_CODE (type2) == FUNCTION_TYPE)
	    warning ("ANSI C forbids conditional expr between `void *' and function pointer");
	  result_type = qualify_type (type1, type2);
	}
      else if (TYPE_MAIN_VARIANT (TREE_TYPE (type2)) == void_type_node)
	{
	  if (pedantic && TREE_CODE (type1) == FUNCTION_TYPE)
	    warning ("ANSI C forbids conditional expr between `void *' and function pointer");
	  result_type = qualify_type (type2, type1);
	}
      /* C++ */
      else if (comptypes (type2, type1, 0))
	result_type = type2;
      else if (result_type = common_base_type (TREE_TYPE (type1), TREE_TYPE (type2)))
	{
	  if (result_type == error_mark_node)
	    {
	      message_2_types (error, "common base type of types `%s' and `%s' is ambiguous",
			       TREE_TYPE (type1), TREE_TYPE (type2));
	      result_type = ptr_type_node;
	    }
	  else result_type = TYPE_POINTER_TO (result_type);
	}
      else
	{
	  warning ("pointer type mismatch in conditional expression");
	  result_type = ptr_type_node;
	}
    }
  else if (code1 == POINTER_TYPE && code2 == INTEGER_TYPE)
    {
      if (!integer_zerop (op2))
	warning ("pointer/integer type mismatch in conditional expression");
      else
	{
	  op2 = null_pointer_node;
	  if (pedantic && TREE_CODE (type1) == FUNCTION_TYPE)
	    warning ("ANSI C forbids conditional expr between 0 and function pointer");
	}
      result_type = type1;
    }
  else if (code2 == POINTER_TYPE && code1 == INTEGER_TYPE)
    {
      if (!integer_zerop (op1))
	warning ("pointer/integer type mismatch in conditional expression");
      else
	{
	  op1 = null_pointer_node;
	  if (pedantic && TREE_CODE (type2) == FUNCTION_TYPE)
	    warning ("ANSI C forbids conditional expr between 0 and function pointer");
	}
      result_type = type2;
      op1 = null_pointer_node;
    }

  if (!result_type)
    {
      /* The match does not look good.  If either is
	 an aggregate value, try converting to a scalar type.  */
      if (code1 == RECORD_TYPE && code2 == RECORD_TYPE)
	{
	  message_2_types (error, "aggregate mismatch in conditional expression: `%s' vs `%s'", type1, type2);
	  return error_mark_node;
	}
      if (code1 == RECORD_TYPE && TYPE_HAS_CONVERSION (type1))
	{
	  tree tmp = build_type_conversion (CONVERT_EXPR, type2, op1, 0);
	  if (tmp == NULL_TREE)
	    {
	      error_with_aggr_type (type1, "aggregate type `%s' could not convert on lhs of `:'");
	      return error_mark_node;
	    }
	  if (tmp == error_mark_node)
	    error ("ambiguous pointer conversion");
	  result_type = type2;
	  op1 = tmp;
	}
      else if (code2 == RECORD_TYPE && TYPE_HAS_CONVERSION (type2))
	{
	  tree tmp = build_type_conversion (CONVERT_EXPR, type1, op2, 0);
	  if (tmp == NULL_TREE)
	    {
	      error_with_aggr_type (type2, "aggregate type `%s' could not convert on rhs of `:'");
	      return error_mark_node;
	    }
	  if (tmp == error_mark_node)
	    error ("ambiguous pointer conversion");
	  result_type = type1;
	  op2 = tmp;
	}
      else if (flag_cond_mismatch)
	result_type = void_type_node;
      else
	{
	  error ("type mismatch in conditional expression");
	  return error_mark_node;
	}
    }

  if (result_type != TREE_TYPE (op1))
    op1 = convert (result_type, op1);
  if (result_type != TREE_TYPE (op2))
    op2 = convert (result_type, op2);

#if 0
  if (IS_AGGR_TYPE_CODE (code1))
    {
      result_type = TREE_TYPE (op1);
      if (TREE_LITERAL (ifexp))
	return (integer_zerop (ifexp) ? op2 : op1);

      if (TYPE_MODE (result_type) == BLKmode)
	{
	  register tree tempvar
	    = build_decl (VAR_DECL, NULL_TREE, result_type);
	  register tree xop1 = build_modify_expr (tempvar, NOP_EXPR, op1);
	  register tree xop2 = build_modify_expr (tempvar, NOP_EXPR, op2);
	  register tree result = build (COND_EXPR, result_type,
					ifexp, xop1, xop2);

	  layout_decl (tempvar);
	  /* No way to handle variable-sized objects here.
	     I fear that the entire handling of BLKmode conditional exprs
	     needs to be redone.  */
	  assert (TREE_LITERAL (DECL_SIZE (tempvar)));
	  DECL_RTL (tempvar)
	    = assign_stack_local (DECL_MODE (tempvar),
				  (TREE_INT_CST_LOW (DECL_SIZE (tempvar))
				   * DECL_SIZE_UNIT (tempvar)
				   + BITS_PER_UNIT - 1)
				  / BITS_PER_UNIT);

	  TREE_VOLATILE (result)
	    = TREE_VOLATILE (ifexp) | TREE_VOLATILE (op1)
	      | TREE_VOLATILE (op2);
	  return build (COMPOUND_EXPR, result_type, result, tempvar);
	}
    }
#endif /* 0 */

  if (TREE_LITERAL (ifexp))
    return (integer_zerop (ifexp) ? op2 : op1);

  return build (COND_EXPR, result_type, ifexp, op1, op2);
}

/* Handle overloading of the ',' operator when needed.  Otherwise,
   this function just builds an expression list.  */
tree
build_x_compound_expr (list)
     tree list;
{
  tree type, rest = TREE_CHAIN (list);
  tree result;

  if (rest == NULL_TREE)
    return build_compound_expr (list);

  result = build_opfncall (COMPOUND_EXPR, LOOKUP_NORMAL,
			   TREE_VALUE (list), TREE_VALUE (rest));
  if (result)
    return build_x_compound_expr (tree_cons (NULL_TREE, result, TREE_CHAIN (rest)));
  else
    return build_compound_expr (tree_cons (NULL_TREE, TREE_VALUE (list),
					   build_tree_list (NULL_TREE, build_x_compound_expr (rest))));
}

/* Given a list of expressions, return a compound expression
   that performs them all and returns the value of the last of them.  */

tree
build_compound_expr (list)
     tree list;
{
  register tree rest;

  if (TREE_CODE (TREE_VALUE (list)) == CALL_EXPR
      && TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (TREE_VALUE (list))))
    TREE_VALUE (list) = cleanup_after_call (TREE_VALUE (list));
  else if (TREE_READONLY_DECL_P (TREE_VALUE (list)))
    TREE_VALUE (list) = decl_constant_value (TREE_VALUE (list));

  if (TREE_CHAIN (list) == 0)
    {
      /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
	 Strip such NOP_EXPRs, since LIST is used in non-lvalue context.  */
      if (TREE_CODE (list) == NOP_EXPR
	  && TREE_TYPE (list) == TREE_TYPE (TREE_OPERAND (list, 0)))
	list = TREE_OPERAND (list, 0);

      return TREE_VALUE (list);
    }

  rest = build_compound_expr (TREE_CHAIN (list));

  /* This is patched out so that sizeof (0, array) is distinguishable from
     sizeof array.  */
#if 0
  if (! TREE_VOLATILE (TREE_VALUE (list)))
    return rest;
#endif

  if (! TREE_VOLATILE (TREE_VALUE (list)))
    return rest;

  return build (COMPOUND_EXPR, TREE_TYPE (rest), TREE_VALUE (list), rest);
}

/* Build an expression representing a cast to type TYPE of expression EXPR.  */

tree
build_c_cast (type, expr)
     register tree type;
     tree expr;
{
  register tree value = expr;

  if (type == error_mark_node || expr == error_mark_node)
    return error_mark_node;
  type = TYPE_MAIN_VARIANT (type);

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since VALUE is being used in non-lvalue context.  */
  if (TREE_CODE (value) == NOP_EXPR
      && TREE_TYPE (value) == TREE_TYPE (TREE_OPERAND (value, 0)))
    value = TREE_OPERAND (value, 0);

  if (type == TREE_TYPE (value))
    {
      if (pedantic)
	{
	  if (TREE_CODE (type) == RECORD_TYPE
	      || TREE_CODE (type) == UNION_TYPE)
	    warning ("ANSI C forbids casting nonscalar to the same type");
	}
      return value;
    }

  /* If there's only one function in the overloaded space,
     just take it.  */
  if (TREE_CODE (value) == TREE_LIST
      && TREE_CHAIN (value) == NULL_TREE)
    value = TREE_VALUE (value);

  if (TREE_TYPE (value) == NULL_TREE
      || type_unknown_p (value))
    {
      value = instantiate_type (type, value, 1);
      /* Did we lose?  */
      if (value == error_mark_node)
	return error_mark_node;
    }
  else if (TREE_CODE (type) == REFERENCE_TYPE
	   && TREE_CODE (TREE_TYPE (value)) == REFERENCE_TYPE)
    {
      /* Reference-to-reference conversion is special.  */
      value = build_unary_op (ADDR_EXPR, value, 0);
      if (value != error_mark_node)
	value = convert_force (build_pointer_type (TREE_TYPE (type)), value);
      if (value == error_mark_node)
	return error_mark_node;
      return build1 (NOP_EXPR, type, value);
    }
  else
    {
      tree otype;
      value = default_conversion (value);
      otype = TREE_TYPE (value);

      /* Optionally warn about potentially worrysome casts.  */

      if (warn_cast_qual
	  && TREE_CODE (type) == POINTER_TYPE
	  && TREE_CODE (otype) == POINTER_TYPE)
	{
	  if (TREE_VOLATILE (TREE_TYPE (otype))
	      && ! TREE_VOLATILE (TREE_TYPE (type)))
	    warning ("cast discards `volatile' from pointer target type");
	  if (TREE_READONLY (TREE_TYPE (otype))
	      && ! TREE_READONLY (TREE_TYPE (type)))
	    warning ("cast discards `const' from pointer target type");
	}
      value = convert_force (type, value);
    }
  if (value == expr)
    /* Always produce some operator for an explicit cast,
       so we can tell (for -pedantic) that the cast is no lvalue.  */
    {
      tree nvalue = build1 (NOP_EXPR, type, value);
      TREE_LITERAL (nvalue) = TREE_LITERAL (value);
      return nvalue;
    }

  if (TREE_CODE (value) == CALL_EXPR
      && TYPE_NEEDS_DESTRUCTOR (TREE_TYPE (value)))
    value = cleanup_after_call (value);

  return value;
}

/* Build an assignment expression of lvalue LHS from value RHS.

   In C++, if the left hand side of the assignment is a REFERENCE_TYPE,
   that reference becomes deferenced down to it base type. */

/* Return a reference to the BASE_INDEX part of EXPR.  TYPE is
   the type to which BASE_INDEX applies.  */
static tree
get_base_ref (type, base_index, expr)
     tree type;
     int base_index;
     tree expr;
{
  tree basetype = CLASSTYPE_BASECLASS (type, base_index);
  tree ref;

  if (TREE_CODE (expr) == ARRAY_REF
      || CLASSTYPE_OFFSET (basetype) != integer_zero_node
      || CLASSTYPE_VIA_VIRTUAL (type, base_index)
      || TYPE_MODE (type) != TYPE_MODE (basetype))
    {
      tree addr = build_unary_op (ADDR_EXPR, expr, 0);
      ref = build_indirect_ref (convert_pointer_to (basetype, addr), 0);
    }
  else
    {
      ref = copy_node (expr);
      TREE_TYPE (ref) = basetype;
    }
  return ref;
}

/* Build an assignment expression of lvalue LHS from value RHS.
   MODIFYCODE is the code for a binary operator that we use
   to combine the old value of LHS with RHS to get the new value.
   Or else MODIFYCODE is NOP_EXPR meaning do a simple assignment.

   C++: If MODIFYCODE is INIT_EXPR, then leave references unbashed.

   `build_modify_expr_1' implements recursive part of memberwise
   assignment operation.  */
static tree
build_modify_expr_1 (lhs, modifycode, rhs, basetype_path)
     tree lhs, rhs;
     enum tree_code modifycode;
     tree basetype_path;
{
  register tree result;
  tree newrhs = rhs;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;

  /* Avoid duplicate error messages from operands that had errors.  */
  if (TREE_CODE (lhs) == ERROR_MARK || TREE_CODE (rhs) == ERROR_MARK)
    return error_mark_node;

  /* If a binary op has been requested, combine the old LHS value with the RHS
     producing the value we should actually store into the LHS.  */

  if (modifycode == INIT_EXPR)
    ;
  else if (modifycode == NOP_EXPR)
    {
      /* must deal with overloading of `operator=' here.  */
      if (TREE_CODE (lhstype) == REFERENCE_TYPE)
	lhstype = TREE_TYPE (lhstype);
      lhstype = olhstype;
    }
  else
    {
      lhs = stabilize_reference (lhs);
      newrhs = build_binary_op (modifycode, lhs, rhs);
      modifycode = NOP_EXPR;
    }

  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
	  || TREE_CODE (lhstype) == REAL_TYPE
	  || TREE_CODE (lhstype) == ENUMERAL_TYPE))
    lhstype = TREE_TYPE (get_unwidened (lhs, 0));

  /* C++: The semantics of C++ differ from those of C when an
     assignment of an aggregate is desired.  Assignment in C++ is
     now defined as memberwise assignment of non-static members
     and base class objects.  This rule applies recursively
     until a member of a built-in type is found.

     Also, we cannot do a bit-wise copy of aggregates which
     contain virtual function table pointers.  Those
     pointer values must be preserved through the copy.
     However, this is handled in expand_expr, and not here.
     This is because much better code can be generated at
     that stage than this one.  */
  if (TREE_CODE (lhstype) == RECORD_TYPE
      && TYPE_LANG_SPECIFIC (lhstype)
      && TYPE_MAIN_VARIANT (lhstype) == TYPE_MAIN_VARIANT (TREE_TYPE (newrhs)))
    {
      register tree elt;
      int i;

      /* Perform operation on object.  */
      if (modifycode == INIT_EXPR && TYPE_HAS_INIT_REF (lhstype))
	{
	  result = build_method_call (lhs, DECL_NAME (TYPE_NAME (lhstype)),
				      build_tree_list (NULL_TREE, rhs),
				      basetype_path, LOOKUP_NORMAL);
	  return build_indirect_ref (result, 0);
	}
      else if (modifycode == NOP_EXPR)
	{
	  /* `operator=' is not an inheritable operator.  */
	  if (TYPE_LANG_SPECIFIC (lhstype) && TYPE_HAS_ASSIGNMENT (lhstype))
	    {
	      result = build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL, lhs, rhs, (tree)NOP_EXPR);
	      if (result == NULL_TREE)
		return error_mark_node;
	      return result;
	    }
	}

      if (TYPE_USES_VIRTUAL_BASECLASSES (lhstype)
	  || (modifycode == NOP_EXPR && TYPE_GETS_ASSIGNMENT (lhstype))
	  || (modifycode == INIT_EXPR && TYPE_GETS_INIT_REF (lhstype)))
	{
	  result = NULL_TREE;

	  /* Perform operation on each member, depth-first, left-right.  */
	  for (i = 1; i <= CLASSTYPE_N_BASECLASSES (lhstype); i++)
	    {
	      tree base_lhs, base_rhs;
	      tree new_result;

	      /* Assignments from virtual baseclasses handled elsewhere.  */
	      if (CLASSTYPE_VIA_VIRTUAL (lhstype, i))
		continue;

	      base_lhs = get_base_ref (lhstype, i, lhs);
	      base_rhs = get_base_ref (lhstype, i, newrhs);

	      new_result
		= build_modify_expr_1 (base_lhs, modifycode, base_rhs,
				       tree_cons (NULL_TREE,
						  TREE_TYPE (base_lhs),
						  basetype_path));

	      /* We either get back a compound stmt, or a simple one.  */
	      if (new_result && TREE_CODE (new_result) == TREE_LIST)
		new_result = build_compound_expr (new_result);
	      result = tree_cons (NULL_TREE, new_result, result);
	    }

	  for (elt = TYPE_FIELDS (lhstype); elt; elt = TREE_CHAIN (elt))
	    {
	      tree vbases = NULL_TREE;
	      tree elt_lhs, elt_rhs;

	      if (TREE_CODE (elt) == VAR_DECL || TREE_CODE (elt) == CONST_DECL)
		continue;
	      if (DECL_NAME (elt)
		  && (VFIELD_NAME_P (DECL_NAME (elt))
		      || VBASE_NAME_P (DECL_NAME (elt))))
		continue;

	      if (IS_AGGR_TYPE (TREE_TYPE (elt))
		  && TYPE_LANG_SPECIFIC (TREE_TYPE (elt)))
		vbases = CLASSTYPE_VBASECLASSES (TREE_TYPE (elt));

	      elt_lhs = build (COMPONENT_REF, TREE_TYPE (elt), lhs, elt);
	      elt_rhs = build (COMPONENT_REF, TREE_TYPE (elt), newrhs, elt);
	      /* It is not always safe to go through `build_modify_expr_1'
		 when performing element-wise copying.  This is because
		 an element may be of ARRAY_TYPE, which will not
		 be properly copied as a naked element.  */
	      if (TREE_CODE (TREE_TYPE (elt)) == RECORD_TYPE
		  && TYPE_LANG_SPECIFIC (TREE_TYPE (elt)))
		basetype_path = CLASSTYPE_AS_LIST (TREE_TYPE (elt));

	      while (vbases)
		{
		  tree elt_lhs_addr = build_unary_op (ADDR_EXPR, elt_lhs);
		  tree elt_rhs_addr = build_unary_op (ADDR_EXPR, elt_rhs);

		  elt_lhs_addr = convert_pointer_to (TREE_TYPE (vbases), elt_lhs_addr);
		  elt_rhs_addr = convert_pointer_to (TREE_TYPE (vbases), elt_rhs_addr);
		  result = tree_cons (NULL_TREE, build_modify_expr_1 (build_indirect_ref (elt_lhs_addr, 0), modifycode, build_indirect_ref (elt_rhs_addr, 0), basetype_path), result);
		  if (TREE_VALUE (result) == error_mark_node)
		    return error_mark_node;
		  vbases = TREE_CHAIN (vbases);
		}
	      elt_lhs = build_modify_expr_1 (elt_lhs, modifycode, elt_rhs,
					     basetype_path);
	      result = tree_cons (NULL_TREE, elt_lhs, result);
	    }

	  if (result)
	    return build_compound_expr (result);
	  /* No fields to move.  */
	  return integer_zero_node;
	}
      else
	{
	  result = build (modifycode == NOP_EXPR ? MODIFY_EXPR : INIT_EXPR,
			  void_type_node, lhs, rhs);
	  TREE_VOLATILE (result) = 1;
	  return result;
	}
    }

  return build_modify_expr (lhs, modifycode, newrhs);
}

tree
build_modify_expr (lhs, modifycode, rhs)
     tree lhs, rhs;
     enum tree_code modifycode;
{
  register tree result;
  tree newrhs = rhs;
  tree lhstype = TREE_TYPE (lhs);
  tree olhstype = lhstype;

  /* Types that aren't fully specified cannot be used in assignments.  */
  lhs = require_complete_type (lhs);

  /* Avoid duplicate error messages from operands that had errors.  */
  if (TREE_CODE (lhs) == ERROR_MARK || TREE_CODE (rhs) == ERROR_MARK)
    return error_mark_node;

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since RHS is being used in non-lvalue context.  */
  if (TREE_CODE (rhs) == NOP_EXPR
      && TREE_TYPE (rhs) == TREE_TYPE (TREE_OPERAND (rhs, 0)))
    rhs = TREE_OPERAND (rhs, 0);

  newrhs = rhs;

  /* Handle control structure constructs used as "lvalues".  */

  if (!pedantic)
    switch (TREE_CODE (lhs))
      {
	/* Handle (a, b) used as an "lvalue".  */
      case COMPOUND_EXPR:
	return build (COMPOUND_EXPR, lhstype,
		      TREE_OPERAND (lhs, 0),
		      build_modify_expr (TREE_OPERAND (lhs, 1),
					 modifycode, rhs));

	/* Handle (a ? b : c) used as an "lvalue".  */
      case COND_EXPR:
	rhs = save_expr (rhs);
	{
	  /* Produce (a ? (b = rhs) : (c = rhs))
	     except that the RHS goes through a save-expr
	     so the code to compute it is only emitted once.  */
	  tree cond
	    = build_conditional_expr
	      (TREE_OPERAND (lhs, 0),
	       build_modify_expr (TREE_OPERAND (lhs, 1),
				  modifycode, rhs),
	       build_modify_expr (TREE_OPERAND (lhs, 2),
				  modifycode, rhs));
	  /* Make sure the code to compute the rhs comes out
	     before the split.  */
	  return build (COMPOUND_EXPR, TREE_TYPE (lhs),
			/* Case to void to suppress warning
			   from warn_if_unused_value.  */
			convert (void_type_node, rhs),
			cond);
	}
      }

  /* If a binary op has been requested, combine the old LHS value with the RHS
     producing the value we should actually store into the LHS.  */

  if (modifycode == INIT_EXPR)
    ;
  else if (modifycode == NOP_EXPR)
    {
      /* must deal with overloading of `operator=' here.  */
      if (TREE_CODE (lhstype) == REFERENCE_TYPE)
	lhstype = TREE_TYPE (lhstype);
#if 1
      /* `operator=' is not an inheritable operator.  */
      if (TYPE_LANG_SPECIFIC (lhstype) && TYPE_HAS_ASSIGNMENT (lhstype))
	{
	  result = build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL, lhs, rhs, (tree)NOP_EXPR);
	  if (result == NULL_TREE)
	    return error_mark_node;
	  return result;
	}
#else
      /* Treat `operator=' as an inheritable operator.  */
      if (TYPE_LANG_SPECIFIC (lhstype) && TYPE_GETS_ASSIGNMENT (lhstype))
	{
	  tree orig_lhstype = lhstype;
	  while (! TYPE_HAS_ASSIGNMENT (lhstype))
	    {
	      int i;
	      tree basetype = NULL_TREE;
	      for (i = 1; i <= CLASSTYPE_N_BASECLASSES (lhstype); i++)
		if (TYPE_GETS_ASSIGNMENT (CLASSTYPE_BASECLASS (lhstype, i)))
		  {
		    if (basetype != NULL_TREE)
		      {
			message_2_types (error, "base classes `%s' and `%s' both have operator ='",
					 basetype,
					 CLASSTYPE_BASECLASS (lhstype, i));
			return error_mark_node;
		      }
		    basetype = CLASSTYPE_BASECLASS (lhstype, i);
		  }
	      lhstype = basetype;
	    }
	  if (orig_lhstype != lhstype)
	    {
	      lhs = build_indirect_ref (convert_pointer_to (lhstype,
							    build_unary_op (ADDR_EXPR, lhs, 0)), 0);
	      if (lhs == error_mark_node)
		{
		  error_with_aggr_type (lhstype, "conversion to private basetype `%s'");
		  return error_mark_node;
		}
	    }
	  result = build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL, lhs, rhs, (tree)NOP_EXPR);
	  if (result == NULL_TREE)
	    return error_mark_node;
	  return result;
	}
#endif
      lhstype = olhstype;
    }
  else if (IS_AGGR_TYPE (lhstype)
	   || (TREE_CODE (lhstype) == REFERENCE_TYPE
	       && IS_AGGR_TYPE (TREE_TYPE (lhstype))))
    {
      /* This case must convert to some sort of lvalue that
	 can participate in a op= operation.  */
      tree lhs_tmp = lhs;
      tree rhs_tmp = rhs;
      if (build_default_binary_type_conversion (modifycode, &lhs_tmp, &rhs_tmp))
	{
	  lhs = stabilize_reference (lhs_tmp);
	  /* Forget is was ever anything else.  */
	  olhstype = lhstype = TREE_TYPE (lhs);
	  newrhs = build_binary_op (modifycode, lhs, rhs_tmp);
	}
      else
	return error_mark_node;
    }
  else
    {
      lhs = stabilize_reference (lhs);
      newrhs = build_binary_op (modifycode, lhs, rhs);
    }

  /* Handle a cast used as an "lvalue".
     We have already performed any binary operator using the value as cast.
     Now convert the result to the true type of the lhs and store there;
     then cast the result back to the specified type to be the value
     of the assignment.  */

  if (!pedantic)
    switch (TREE_CODE (lhs))
      {
      case NOP_EXPR:
      case CONVERT_EXPR:
      case FLOAT_EXPR:
      case FIX_TRUNC_EXPR:
      case FIX_FLOOR_EXPR:
      case FIX_ROUND_EXPR:
      case FIX_CEIL_EXPR:
	if (TREE_CODE (TREE_TYPE (newrhs)) == ARRAY_TYPE
	    || TREE_CODE (TREE_TYPE (newrhs)) == FUNCTION_TYPE
	    || TREE_CODE (TREE_TYPE (newrhs)) == METHOD_TYPE
	    || TREE_CODE (TREE_TYPE (newrhs)) == OFFSET_TYPE)
	  newrhs = default_conversion (newrhs);
	{
	  tree inner_lhs = TREE_OPERAND (lhs, 0);
	  tree result = build_modify_expr (inner_lhs, NOP_EXPR,
					   convert (TREE_TYPE (inner_lhs),
						    newrhs));
	  return convert (TREE_TYPE (lhs), result);
	}
      }

  if (TREE_CODE (lhs) == OFFSET_REF)
    if (TREE_OPERAND (lhs, 0) == NULL_TREE)
      {
	/* Static class member?  */
	tree member = TREE_OPERAND (lhs, 1);
	if (TREE_CODE (member) == VAR_DECL)
	  lhs = member;
	else
	  {
	    compiler_error ("invalid static class member");
	    return error_mark_node;
	  }
      }
    else
      {
	tree base = build_unary_op (ADDR_EXPR, TREE_OPERAND (lhs, 0), 0);
	tree member = build_unary_op (ADDR_EXPR, TREE_OPERAND (lhs, 1), 0);

	if (TREE_CODE (base) == ERROR_MARK
	    || TREE_CODE (member) == ERROR_MARK)
	  return error_mark_node;
	lhs = build_indirect_ref (build (PLUS_EXPR, build_pointer_type (TREE_TYPE (lhs)),
					 base, member));
      }

  /* Now we have handled acceptable kinds of LHS that are not truly lvalues.
     Reject anything strange now.  */

  if (!lvalue_or_else (lhs, "assignment"))
    return error_mark_node;

#ifdef FIELD_XREF
  FIELD_xref_assign(lhs);
#endif

  /* Warn about storing in something that is `const'.  */
  /* For C++, don't warn if this is initialization.  */
  if (modifycode != INIT_EXPR
      && (TREE_READONLY (lhs)
	  || ((TREE_CODE (lhstype) == RECORD_TYPE
	       || TREE_CODE (lhstype) == UNION_TYPE)
	      && C_TYPE_FIELDS_READONLY (lhstype))
	  || (TREE_CODE (lhstype) == REFERENCE_TYPE
	      && TREE_READONLY (TREE_TYPE (lhstype)))))
    readonly_warning_or_error (lhs, "assignment");

  /* If storing into a structure or union member,
     it has probably been given type `int'.
     Compute the type that would go with
     the actual amount of storage the member occupies.  */

  if (TREE_CODE (lhs) == COMPONENT_REF
      && (TREE_CODE (lhstype) == INTEGER_TYPE
	  || TREE_CODE (lhstype) == REAL_TYPE
	  || TREE_CODE (lhstype) == ENUMERAL_TYPE))
    lhstype = TREE_TYPE (get_unwidened (lhs, 0));

  /* check to see if there is an assignment to `this' */
  if (lhs == current_class_decl)
    {
      if (flag_this_is_variable
	  && current_class_name != DECL_ORIGINAL_NAME (current_function_decl))
	warning ("assignment to `this' not in constructor or destructor");
      current_function_just_assigned_this = 1;
    }

  /* The TREE_TYPE of RHS may be TYPE_UNKNOWN.  This can happen
     when the type of RHS is not yet known, i.e. its type
     is inherited from LHS.  */
  rhs = require_instantiated_type (lhstype, newrhs, error_mark_node);
  if (rhs == error_mark_node)
    return error_mark_node;
  newrhs = rhs;

  if (modifycode != INIT_EXPR)
    {
      /* Make modifycode now either a NOP_EXPR or an INIT_EXPR.  */
      modifycode = NOP_EXPR;
      /* Reference-bashing */
      if (TREE_CODE (lhstype) == REFERENCE_TYPE)
	{
	  tree tmp = convert_from_reference (lhs);
	  lhstype = TREE_TYPE (tmp);
	  if (TYPE_SIZE (lhstype) == 0)
	    {
	      incomplete_type_error (lhs, lhstype);
	      return error_mark_node;
	    }
	  lhs = tmp;
	  olhstype = lhstype;
	}
      if (TREE_CODE (TREE_TYPE (newrhs)) == REFERENCE_TYPE)
	{
	  tree tmp = convert_from_reference (newrhs);
	  if (TYPE_SIZE (TREE_TYPE (tmp)) == 0)
	    {
	      incomplete_type_error (newrhs, TREE_TYPE (tmp));
	      return error_mark_node;
	    }
	  newrhs = tmp;
	}
    }

  if (TREE_VOLATILE (lhs))
    lhs = stabilize_reference (lhs);
  if (TREE_VOLATILE (newrhs))
    newrhs = stabilize_reference (newrhs);

  /* C++: The semantics of C++ differ from those of C when an
     assignment of an aggregate is desired.  Assignment in C++ is
     now defined as memberwise assignment of non-static members
     and base class objects.  This rule applies recursively
     until a member of a built-in type is found.

     Also, we cannot do a bit-wise copy of aggregates which
     contain virtual function table pointers.  Those
     pointer values must be preserved through the copy.
     However, this is handled in expand_expr, and not here.
     This is because much better code can be generated at
     that stage than this one.  */
  if (TREE_CODE (lhstype) == RECORD_TYPE
      && ((TYPE_USES_VIRTUAL_BASECLASSES (lhstype)
	   || (modifycode != INIT_EXPR && TYPE_GETS_ASSIGNMENT (lhstype))
	   || (modifycode == INIT_EXPR && TYPE_GETS_INIT_REF (lhstype)))
	  && (TYPE_MAIN_VARIANT (lhstype)
	      == TYPE_MAIN_VARIANT (TREE_TYPE (newrhs))
	      || (TREE_CODE (TREE_TYPE (newrhs)) == RECORD_TYPE
		  && get_base_type (lhstype, TREE_TYPE (newrhs), 0)))))
    {
      tree vbases = CLASSTYPE_VBASECLASSES (lhstype);
      tree lhs_addr = build_unary_op (ADDR_EXPR, lhs);
      tree rhs_addr = build_unary_op (ADDR_EXPR, newrhs);
      result = NULL_TREE;
      if (! comptypes (TREE_TYPE (lhs_addr), TREE_TYPE (rhs_addr), 1))
	{
	  rhs_addr = convert_pointer_to (TREE_TYPE (TREE_TYPE (lhs_addr)), rhs_addr);
	  newrhs = build_indirect_ref (rhs_addr, 0);
	}

      while (vbases)
	{
	  tree elt_lhs = convert_pointer_to (TREE_TYPE (vbases), lhs_addr);
	  tree elt_rhs = convert_pointer_to (TREE_TYPE (vbases), rhs_addr);
	  result = tree_cons (NULL_TREE, build_modify_expr_1 (build_indirect_ref (elt_lhs, 0), modifycode, build_indirect_ref (elt_rhs, 0), CLASSTYPE_AS_LIST (lhstype)), result);
	  if (TREE_VALUE (result) == error_mark_node)
	    return error_mark_node;
	  vbases = TREE_CHAIN (vbases);
	}
      result = tree_cons (NULL_TREE,
			  build_modify_expr_1 (lhs, modifycode, newrhs, CLASSTYPE_AS_LIST (lhstype)),
			  result);
      return build_compound_expr (result);
    }

  /* If storing in a field that is in actuality a short or narrower than one,
     we must store in the field in its actual type.  */

  if (lhstype != TREE_TYPE (lhs))
    {
      lhs = copy_node (lhs);
      TREE_TYPE (lhs) = lhstype;
    }

  /* Convert new value to destination type.  */

  if (modifycode == INIT_EXPR)
    {
      newrhs = convert_for_initialization (lhs, lhstype, newrhs, "assignment", LOOKUP_NORMAL);
      if (lhs == DECL_RESULT (current_function_decl))
	{
	  if (DECL_INITIAL (lhs))
	    warning ("return value from function receives multiple initializations");
	  DECL_INITIAL (lhs) = newrhs;
	}
    }
  else
    {
      if (IS_AGGR_TYPE (lhstype))
	{
	  if (TYPE_GETS_ASSIGNMENT (lhstype)
	      && ! TYPE_HAS_ASSIGNMENT (lhstype))
	    {
	      error_with_aggr_type (lhstype, "assignment not defined for type `%s'");
	      return error_mark_node;
	    }
	  if (result = build_opfncall (MODIFY_EXPR, LOOKUP_NORMAL, lhs, newrhs, NOP_EXPR))
	    return result;
	}
      else if (TREE_CODE (lhstype) == ARRAY_TYPE)
	{
	  /* Have to wrap this in RTL_EXPR for two cases:
	     in base or member initialization and if we
	     are a branch of a ?: operator.  Since we
	     can't easily know the latter, just do it always.  */

	  extern struct rtx_def *get_insns (), *const0_rtx;
	  result = make_node (RTL_EXPR);

	  TREE_TYPE (result) = void_type_node;
	  do_pending_stack_adjust ();
	  start_sequence ();

	  /* As a matter of principle, `start_sequence' should do this.  */
	  emit_note (0, -1);

	  expand_vec_init (lhs, lhs, array_type_nelts (lhstype), newrhs, 2);

	  do_pending_stack_adjust ();

	  TREE_VOLATILE (result) = 1;
	  RTL_EXPR_SEQUENCE (result) = get_insns ();
	  RTL_EXPR_RTL (result) = const0_rtx;
	  end_sequence ();
	  return result;
	}
      newrhs = convert_for_assignment (lhstype, newrhs, "assignment");
      if (flag_elide_constructors == 0
	  && TREE_CODE (newrhs) == CALL_EXPR
	  && TREE_ADDRESSABLE (lhstype))
	{
	  /* Can't initialized directly from a CALL_EXPR, since
	     we don't know about what doesn't alias what.  */

	  if (TYPE_NEEDS_DESTRUCTOR (lhstype))
	    newrhs = cleanup_after_call (newrhs);
	  else
	    {
	      tree temp = get_temp_name (lhstype, 0);
	      newrhs = build (COMPOUND_EXPR, lhstype,
			      build_modify_expr (temp, INIT_EXPR, newrhs),
			      temp);
	    }
	}
    }

  if (TREE_CODE (newrhs) == ERROR_MARK)
    return error_mark_node;

  result = build (modifycode == NOP_EXPR ? MODIFY_EXPR : INIT_EXPR,
		  lhstype, lhs, newrhs);
  TREE_VOLATILE (result) = 1;

  /* If we got the LHS in a different type for storing in,
     convert the result back to the nominal type of LHS
     so that the value we return always has the same type
     as the LHS argument.  */

  if (olhstype == TREE_TYPE (result))
    return result;
  return convert_for_assignment (olhstype, result, "assignment");
}


/* Return 0 if EXP is not a valid lvalue in this language
   even though `lvalue_or_else' would accept it.  */

int
language_lvalue_valid (exp)
     tree exp;
{
  return 1;
}

/* Convert value RHS to type TYPE as preparation for an assignment
   to an lvalue of type TYPE.
   The real work of conversion is done by `convert'.
   The purpose of this function is to generate error messages
   for assignments that are not allowed in C.
   ERRTYPE is a string to use in error messages:
   "assignment", "return", etc.

   C++: attempts to allow `convert' to find conversions involving
   implicit type conversion between aggregate and scalar types
   as per 8.5.6 of C++ manual.  Does not randomly dereference
   pointers to aggregates!  */

static tree
convert_for_assignment (type, rhs, errtype)
     tree type, rhs;
     char *errtype;
{
  register enum tree_code codel = TREE_CODE (type);
  register tree rhstype = datatype (rhs);
  register enum tree_code coder = TREE_CODE (rhstype);

  if (coder == UNKNOWN_TYPE)
    {
      rhs = instantiate_type (type, rhs, 1);
      rhstype = TREE_TYPE (rhs);
      coder = TREE_CODE (rhstype);
    }

  if (coder == ERROR_MARK)
    return error_mark_node;

  if (codel == OFFSET_TYPE)
    {
      type = TREE_TYPE (type);
      codel = TREE_CODE (type);
    }

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since RHS is used in non-lvalue context.  */
  if (TREE_CODE (rhs) == NOP_EXPR
      && TREE_TYPE (rhs) == TREE_TYPE (TREE_OPERAND (rhs, 0)))
    rhs = TREE_OPERAND (rhs, 0);

  if (TREE_CODE (TREE_TYPE (rhs)) == OFFSET_TYPE)
    {
      rhs = resolve_offset_ref (rhs);
      if (rhs == error_mark_node)
	return error_mark_node;
      rhstype = TREE_TYPE (rhs);
      coder = TREE_CODE (rhstype);
    }

  if (TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == METHOD_TYPE)
    {
      rhs = default_conversion (rhs);
      if (rhs == error_mark_node)
	return rhs;
    }
  else if (coder == REFERENCE_TYPE)
    {
      rhs = convert_from_reference (rhs);
      rhstype = TREE_TYPE (rhs);
      coder = TREE_CODE (rhstype);
    }

  /* This should no longer change types on us.  */
  if (TREE_CODE (rhs) == CONST_DECL)
    rhs = DECL_INITIAL (rhs);
  else if (TREE_READONLY_DECL_P (rhs))
    rhs = decl_constant_value (rhs);

  if (type == rhstype)
    return rhs;

  if (coder == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  /* Arithmetic types all interconvert.  */
  if ((codel == INTEGER_TYPE || codel == REAL_TYPE)
       && (coder == INTEGER_TYPE || coder == REAL_TYPE))
    {
      /* But we should warn if assigning REAL_TYPE to INTEGER_TYPE.  */
      if (coder == REAL_TYPE && codel == INTEGER_TYPE)
	warning ("float or double assigned to integer data type");
      /* And we should warn if assigning a negative value to
	 an unsigned variable.  */
      else if (TREE_UNSIGNED (type))
	{
	  if (TREE_CODE (rhs) == INTEGER_CST
	      && TREE_NEGATED_INT (rhs))
	    warning ("negative value assigned to unsigned quantity");
	  if (TREE_LITERAL (rhs))
	    rhs = fold (rhs);
	}

      return convert (type, rhs);
    }
  /* Conversions involving enums.  */
  else if ((codel == ENUMERAL_TYPE
	    && (coder == ENUMERAL_TYPE || coder == INTEGER_TYPE || coder == REAL_TYPE))
	   || (coder == ENUMERAL_TYPE
	       && (codel == ENUMERAL_TYPE || codel == INTEGER_TYPE || codel == REAL_TYPE)))
    {
      extern int warn_enum_clash;

      if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype))
	return convert (type, rhs);
      if (warn_enum_clash)
	{
	  if (codel == ENUMERAL_TYPE && coder == ENUMERAL_TYPE)
	    message_2_types (warning, "conversion between incompatible enumeral types `%s' and `%s'",
			     type, rhstype);
	  else if (coder == REAL_TYPE)
	    warning ("float or double assigned to enumeral data type");
	  else if (codel == REAL_TYPE)
	    warning ("enumeral value assigned to real data type");
	  else if (coder == INTEGER_TYPE)
	    warning ("assignment of integer to enumeral data type");
	}
      return convert (type, rhs);
    }
  /* Conversions among pointers */
  else if (codel == POINTER_TYPE && coder == POINTER_TYPE)
    {
      register tree ttl = TREE_TYPE (type);
      register tree ttr = TREE_TYPE (rhstype);

      /* If both pointers are of aggregate type, then we
	 can give better error messages, and save some work
	 as well.  */
      if (IS_AGGR_TYPE (ttl) && IS_AGGR_TYPE (ttr))
	{
	  tree basetype;

	  if (TYPE_MAIN_VARIANT (ttl) == TYPE_MAIN_VARIANT (ttr))
	    basetype = ttl;
	  else
	    basetype = get_base_type (ttl, ttr, 1);

	  if (basetype == error_mark_node)
	    return error_mark_node;
	  if (basetype == 0)
	    {
	      error_not_base_type (ttl, ttr);
	      return error_mark_node;
	    }
	  if (! TREE_READONLY (ttl) && TREE_READONLY (ttr))
	    warning ("%s of non-const * pointer from const *", errtype);
	  if (! TREE_VOLATILE (ttl) && TREE_VOLATILE (ttr))
	    warning ("%s of non-volatile * pointer from volatile *", errtype);
	}
      /* Any non-function converts to a [const][volatile] void *
	 and vice versa; otherwise, targets must be the same.
	 Meanwhile, the lhs target must have all the qualifiers of the rhs.  */
      else if (TYPE_MAIN_VARIANT (ttl) == void_type_node
	       || TYPE_MAIN_VARIANT (ttr) == void_type_node
	       || comp_target_types (type, rhstype, 1))
	{
	  if (pedantic
	      && ((TYPE_MAIN_VARIANT (ttl) == void_type_node
		   && (TREE_CODE (ttr) == FUNCTION_TYPE
		       || TREE_CODE (ttr) == METHOD_TYPE))
		  ||
		  (TYPE_MAIN_VARIANT (ttr) == void_type_node
		   && (TREE_CODE (ttl) == FUNCTION_TYPE
		       || TREE_CODE (ttl) == METHOD_TYPE))))
	    warning ("%s between incompatible pointer types", errtype);
	  else
	    {
	      if (TREE_CODE (ttl) == OFFSET_TYPE
		  && virtual_member (TYPE_OFFSET_BASETYPE (ttr),
				     CLASSTYPE_VBASECLASSES (TYPE_OFFSET_BASETYPE (ttl))))
		{
		  sorry ("%s between pointer to members converting across virtual baseclasses", errtype);
		  return error_mark_node;
		}
	      if (! TREE_READONLY (ttl) && TREE_READONLY (ttr))
		warning ("%s of non-const * pointer from const *", errtype);
	      if (! TREE_VOLATILE (ttl) && TREE_VOLATILE (ttr))
		warning ("%s of non-volatile * pointer from volatile *", errtype);
	    }
	}
      else if (TREE_CODE (ttr) == OFFSET_TYPE
	       && TREE_CODE (ttl) != OFFSET_TYPE)
	{
	  /* Normally, pointers to different type codes (other
	     than void) are not compatible, but we perform
	     some type instantiation if that resolves the
	     ambiguity of (X Y::*) and (X *).  */

	  if (current_class_decl)
	    {
	      if (TREE_CODE (rhs) == INTEGER_CST)
		{
		  rhs = build (PLUS_EXPR, build_pointer_type (TREE_TYPE (ttr)),
			       current_class_decl, rhs);
		  return convert_for_assignment (type, rhs, errtype);
		}
	    }
	  if (TREE_CODE (ttl) == METHOD_TYPE)
	    error ("%s between pointer-to-method and pointer-to-member types", errtype);
	  else
	    error ("%s between pointer and pointer-to-member types", errtype);
	  return error_mark_node;
	}
      else
	{
	  int const_parity = TREE_READONLY (type) ^ TREE_READONLY (rhstype);
	  int volatile_parity = TREE_VOLATILE (type) ^ TREE_VOLATILE (rhstype);
	  int unsigned_parity;
	  int nptrs = 0;

	  while (TREE_CODE (ttl) == POINTER_TYPE
		 && TREE_CODE (ttr) == POINTER_TYPE)
	    {
	      nptrs -= 1;
	      const_parity |= TREE_READONLY (ttl) ^ TREE_READONLY (ttr);
	      volatile_parity |= TREE_VOLATILE (ttl) ^ TREE_VOLATILE (ttr);
	      ttl = TREE_TYPE (ttl);
	      ttr = TREE_TYPE (ttr);
	    }
	  unsigned_parity = TREE_UNSIGNED (ttl) - TREE_UNSIGNED (ttr);
	  if (unsigned_parity)
	    if (TREE_UNSIGNED (ttl))
	      ttr = unsigned_type (ttr);
	    else
	      ttl = unsigned_type (ttl);

	  if (comp_target_types (ttl, ttr, nptrs))
	    {
	      if (const_parity)
		warning ("%s of non-const * pointer from const *", errtype);
	      if (volatile_parity)
		warning ("%s of non-volatile * pointer from volatile *", errtype);
	      if (unsigned_parity > 0)
		warning ("%s of unsigned pointer from signed pointer", errtype);
	      else if (unsigned_parity < 0)
		warning ("%s of signed pointer from unsigned pointer", errtype);

	      /* C++ is not so friendly about converting function and
		 member function pointers as C.  Emit warnings here.  */
	      if (TREE_CODE (ttl) == FUNCTION_TYPE
		  || TREE_CODE (ttl) == METHOD_TYPE)
		if (! comptypes (ttl, ttr, 0))
		  {
		    char lhsbuf[2048];
		    char rhsbuf[2048];
		    tree null_name = get_identifier ("");
		    tree lhs = build_decl (FUNCTION_DECL, null_name, ttl);
		    tree rhs = build_decl (FUNCTION_DECL, null_name, ttr);
		    fndecl_as_string (lhsbuf, 0, lhs, 1);
		    fndecl_as_string (rhsbuf, 0, rhs, 1);
		    warning ("conflicting function types in %s:", errtype);
		    warning ("\t`%s' != `%s'", lhsbuf, rhsbuf);
		  }
	    }
	  else if (TREE_CODE (TREE_TYPE (rhs)) == METHOD_TYPE)
	    {
	      /* When does this happen?  */
	      abort ();
	      /* Conversion of a pointer-to-member type to void *.  */
	      rhs = build_unary_op (ADDR_EXPR, rhs, 0);
	      TREE_TYPE (rhs) = type;
	      return rhs;
	    }
	  else if (TREE_CODE (TREE_TYPE (rhs)) == OFFSET_TYPE)
	    {
	      /* When does this happen?  */
	      abort ();
	      /* Conversion of a pointer-to-member type to void *.  */
	      rhs = build_unary_op (ADDR_EXPR, rhs, 0);
	      TREE_TYPE (rhs) = type;
	      return rhs;
	    }
	  else
	    {
	      error ("%s between incompatible pointer types", errtype);
	      return error_mark_node;
	    }
	}
      return convert (type, rhs);
    }
  else if (codel == POINTER_TYPE && coder == INTEGER_TYPE)
    {
      if (! integer_zerop (rhs))
	{
	  warning ("%s of pointer from integer lacks a cast", errtype);
	  return convert (type, rhs);
	}
      return null_pointer_node;
    }
  else if (codel == INTEGER_TYPE && coder == POINTER_TYPE)
    {
      warning ("%s of integer from pointer lacks a cast", errtype);
      return convert (type, rhs);
    }

  /* C++ */
  else if (codel == ERROR_MARK || coder == ERROR_MARK)
    return error_mark_node;

  /* This should no longer happen.  References are initialzed via
     `convert_for_initialization'.  They should otherwise be
     bashed before coming here.  */
  else if (codel == REFERENCE_TYPE)
    assert (codel != REFERENCE_TYPE);
  else if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (TREE_TYPE (rhs)))
    return build1 (NOP_EXPR, type, rhs);
  else if (TYPE_HAS_CONSTRUCTOR (type) || IS_AGGR_TYPE (TREE_TYPE (rhs)))
    return convert (type, rhs);

  error ("incompatible types in %s", errtype);
  return error_mark_node;
}

/* Convert RHS to be of type TYPE.  If EXP is non-zero,
   it is the target of the initialization.
   ERRTYPE is a string to use in error messages.

   Two major differences between the behavior of
   `convert_for_assignment' and `convert_for_initialization'
   are that references are bashed in the former, while
   copied in the latter, and aggregates are assigned in
   the former (operator=) while initialized in the
   latter (X(X&)).  */
tree
convert_for_initialization (exp, type, rhs, errtype, flags)
     tree exp, type, rhs;
     char *errtype;
     int flags;
{
  register enum tree_code codel = TREE_CODE (type);
  register tree rhstype;
  register enum tree_code coder;

  /* build_c_cast puts on a NOP_EXPR to make the result not an lvalue.
     Strip such NOP_EXPRs, since RHS is used in non-lvalue context.  */
  if (TREE_CODE (rhs) == NOP_EXPR
      && TREE_TYPE (rhs) == TREE_TYPE (TREE_OPERAND (rhs, 0)))
    rhs = TREE_OPERAND (rhs, 0);

  if (TREE_CODE (TREE_TYPE (rhs)) == OFFSET_TYPE)
    {
      rhs = resolve_offset_ref (rhs);
      if (rhs == error_mark_node)
	return error_mark_node;
      rhstype = TREE_TYPE (rhs);
      coder = TREE_CODE (rhstype);
    }

  if ((TREE_CODE (TREE_TYPE (rhs)) == ARRAY_TYPE
       && TREE_CODE (type) != ARRAY_TYPE && TREE_CODE (type) != REFERENCE_TYPE)
      || TREE_CODE (TREE_TYPE (rhs)) == FUNCTION_TYPE
      || TREE_CODE (TREE_TYPE (rhs)) == METHOD_TYPE)
    rhs = default_conversion (rhs);

  rhstype = TREE_TYPE (rhs);
  coder = TREE_CODE (rhstype);

  if (coder == UNKNOWN_TYPE)
    {
      rhs = instantiate_type (type, rhs, 1);
      rhstype = TREE_TYPE (rhs);
      coder = TREE_CODE (rhstype);
    }

  if (coder == ERROR_MARK)
    return error_mark_node;

#if 0
  /* This is *not* the quick way out!  It is the way to disaster.  */
  if (type == rhstype)
    goto converted;
#endif

  /* We accept references to incomplete types, so we can
     return here before checking if RHS is of complete type.  */
     
  if (codel == REFERENCE_TYPE)
    return convert_to_reference ((exp ? exp : error_mark_node), type, rhs, 0, flags);

  rhs = require_complete_type (rhs);
  if (rhs == error_mark_node)
    return error_mark_node;

  if (exp != 0) exp = require_complete_type (exp);
  if (exp == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (rhstype) == REFERENCE_TYPE)
    rhstype = TREE_TYPE (rhstype);

  if (IS_AGGR_TYPE (type) && TYPE_NEEDS_CONSTRUCTOR (type))
    {
      if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype))
	{
	  /* This is sufficient to perform initialization.  No need, apparently,
	     to go through X(X&) to do first-cut initialization.  Return through
	     a NEW_EXPR so that we get cleanups if it is used.  */
	  if (TREE_CODE (rhs) == CALL_EXPR)
	    {
	      rhs = build_cplus_new (type, rhs);
	      return rhs;
	    }
	}
      if (TYPE_MAIN_VARIANT (type) == TYPE_MAIN_VARIANT (rhstype)
	  || (IS_AGGR_TYPE (rhstype) && get_base_type (type, rhstype, 0)))
	{
	  if (TYPE_HAS_INIT_REF (type))
	    {
	      tree init = build_method_call (exp, DECL_NAME (TYPE_NAME (type)),
					     build_tree_list (NULL_TREE, rhs),
					     NULL_TREE, LOOKUP_NORMAL);

	      if (init == error_mark_node)
		return error_mark_node;

	      if (exp == 0)
		{
		  exp = build_cplus_new (type, init);
		  return exp;
		}

	      return build (COMPOUND_EXPR, type, init, exp);
	    }

	  if (TYPE_GETS_ASSIGNMENT (type))
	    warning ("bitwise copy: `%s' defines operator=()",
		     TYPE_NAME_STRING (type));

	  if (TREE_CODE (TREE_TYPE (rhs)) == REFERENCE_TYPE)
	    rhs = convert_from_reference (rhs);
	  if (type != rhstype)
	    return build1 (NOP_EXPR, type, rhs);
	  return rhs;
	}

      return convert (type, rhs);
    }
  if (TYPE_LANG_SPECIFIC (type) && TYPE_GETS_ASSIGNMENT (type))
    warning ("bitwise copy: `%s' defines operator=()",
	     TYPE_NAME_STRING (type));

 converted:
  if (type == TREE_TYPE (rhs))
    {
      if (TREE_READONLY_DECL_P (rhs))
	rhs = decl_constant_value (rhs);
      return rhs;
    }

  return convert_for_assignment (type, rhs, errtype);
}

/* Expand an ASM statement with operands, handling output operands
   that are not variables or INDIRECT_REFS by transforming such
   cases into cases that expand_asm_operands can handle.

   Arguments are same as for expand_asm_operands.  */

void
c_expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line)
     tree string, outputs, inputs, clobbers;
     int vol;
     char *filename;
     int line;
{
  int noutputs = list_length (outputs);
  register int i;
  /* o[I] is the place that output number I should be written.  */
  register tree *o = (tree *) alloca (noutputs * sizeof (tree));
  register tree tail;

  /* Record the contents of OUTPUTS before it is modifed.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    o[i] = TREE_VALUE (tail);

#if 0  /* Don't do this--it screws up operands expected to be in memory.  */
  /* Perform default conversions on all inputs.  */
  for (i = 0, tail = inputs; tail; tail = TREE_CHAIN (tail), i++)
    TREE_VALUE (tail) = default_conversion (TREE_VALUE (tail));
#endif

  /* Generate the ASM_OPERANDS insn;
     store into the TREE_VALUEs of OUTPUTS some trees for
     where the values were actually stored.  */
  expand_asm_operands (string, outputs, inputs, clobbers, vol, filename, line);

  /* Copy all the intermediate outputs into the specified outputs.  */
  for (i = 0, tail = outputs; tail; tail = TREE_CHAIN (tail), i++)
    {
      extern struct rtx_def *const0_rtx;

      if (o[i] != TREE_VALUE (tail))
	expand_expr (build_modify_expr (o[i], NOP_EXPR, TREE_VALUE (tail)),
		     const0_rtx, VOIDmode, 0);
      /* Detect modification of read-only values.
	 (Otherwise done by build_modify_expr.)  */
      else
	{
	  tree type = TREE_TYPE (o[i]);
	  if (TREE_READONLY (o[i])
	      || ((TREE_CODE (type) == RECORD_TYPE
		   || TREE_CODE (type) == UNION_TYPE)
		  && C_TYPE_FIELDS_READONLY (type)))
	    readonly_warning_or_error (o[i], "modification by `asm'");
	}
    }

  /* Those MODIFY_EXPRs could do autoincrements.  */
  emit_queue ();
}

/* Expand a C `return' statement.
   RETVAL is the expression for what to return,
   or a null pointer for `return;' with no value.

   C++: upon seeing a `return', we must call destructors on all
   variables in scope which had constructors called on them.
   This means that if in a destructor, the base class destructors
   must be called before returning.

   The RETURN statement in C++ has initialization semantics.  */

void
c_expand_return (retval)
     tree retval;
{
  extern struct rtx_def *original_result_rtx;
  extern tree dtor_label, ctor_label;
  tree result = DECL_RESULT (current_function_decl);
  tree valtype = TREE_TYPE (result);
  tree save_from_destructor = NULL_TREE;

  if (TREE_THIS_VOLATILE (current_function_decl))
    warning ("function declared `volatile' has a `return' statement");

  if (retval == error_mark_node)
    {
      current_function_returns_null = 1;
      return;
    }

  if (retval == NULL_TREE)
    {
      /* A non-named return value does not count.  */
      extern tree value_identifier;

      /* Can't just return from a destructor.  */
      if (dtor_label)
	{
	  expand_goto (dtor_label);
	  return;
	}

      if (DECL_CONSTRUCTOR_P (current_function_decl))
	retval = current_class_decl;
      else if (result != NULL_TREE
	       && DECL_NAME (result) != value_identifier
	       && TREE_CODE (valtype) != VOID_TYPE)
	retval = result;
      else
	{
	  current_function_returns_null = 1;
	  if (valtype != 0 && TREE_CODE (valtype) != VOID_TYPE)
	    {
	      extern tree value_identifier;
	      if (DECL_NAME (DECL_RESULT (current_function_decl)) == value_identifier)
		warning ("`return' with no value, in function returning non-void");
	    }

	  expand_null_return ();
	  return;
	}
    }

  if (valtype == 0 || TREE_CODE (valtype) == VOID_TYPE)
    {
      current_function_returns_null = 1;
      if (pedantic || TREE_CODE (TREE_TYPE (retval)) != VOID_TYPE)
	warning ("`return' with a value, in function returning void");
      expand_return (retval);
    }
  else
    {
      register tree t;
      register int use_temp = 0;

      if (retval == result)
	;
      else if (IS_AGGR_TYPE (valtype) && TYPE_NEEDS_CONSTRUCTOR (valtype))
	{
	  expand_aggr_init (result, retval, 0);
	  save_from_destructor = DECL_INITIAL (result);
	  DECL_INITIAL (result) = NULL_TREE;
	  retval = 0;
	}
      else
	{
	  /* Watch out for constructors, which "return" aggregates
	     via initialization, but which otherwise "return" a pointer.  */
	  if (DECL_CONSTRUCTOR_P (current_function_decl))
	    {
	      if (retval != current_class_decl)
		{
		  error ("return from a constructor: use `this = ...' instead");
		  retval = current_class_decl;
		}
	    }
	  else
	    {
	      retval = convert_for_initialization (result, valtype, retval, "return", LOOKUP_NORMAL);
	      if (retval == error_mark_node)
		return;
	      save_from_destructor = DECL_INITIAL (result);
	      DECL_INITIAL (result) = NULL_TREE;
	    }

	  /* Add some useful error checking for C++.  */
	  if (TREE_CODE (valtype) == REFERENCE_TYPE)
	    {
	      tree whats_returned = retval;

	      /* Sort through common things to see what it is
		 we are returning.  */
	      if (TREE_CODE (retval) == COMPOUND_EXPR)
		{
		  whats_returned = TREE_OPERAND (retval, 1);
		  if (TREE_CODE (whats_returned) == ADDR_EXPR)
		    whats_returned = TREE_OPERAND (whats_returned, 0);
		}
	      if (TREE_CODE (retval) == VAR_DECL)
		whats_returned = retval;
	      else if (TREE_CODE (retval) == ADDR_EXPR)
		{
		  whats_returned = TREE_OPERAND (retval, 0);
		  if (TREE_CODE (whats_returned) == NEW_EXPR)
		    /* Get the target.  */
		    whats_returned = TREE_OPERAND (whats_returned, 0);
		}

	      if (TREE_CODE (whats_returned) == VAR_DECL)
		if (DECL_NAME (whats_returned) == NULL_TREE
		    || TEMP_NAME_P (DECL_NAME (whats_returned)))
		  warning ("reference to non-lvalue returned");
		else if (! TREE_STATIC (whats_returned)
			 && IDENTIFIER_LOCAL_VALUE (DECL_NAME (whats_returned)))
		  warning_with_decl (whats_returned, "reference to local variable `%s' returned");
	    }
	}

      /* Now deal with possible C++ hair:
	 (1) Compute the return value.
	 (2) If there are aggregate values with destructors which
	     must be cleaned up, clean them (taking care
	     not to clobber the return value).
	 (3) If an X(X&) constructor is defined, the return
	     value must be returned via that.  */

      /* Keep anybody from thinking that our return value
	 is up for grabs (i.e., later inline function expansion).
	 We cannot use a `save_expr' here, because the
	 return value must be calculated _now_,
	 and wrapping it in a SAVE_EXPR just make it
	 be calculated _once_.

	 We look up the binding contours to see whether there are
	 any cleanups to perform to avoid, where possible, the need
	 to pass the value through a temporary.  This is needed to make
	 tail-recursion work in GCC.  */
      /* use_variable (DECL_RTL (result)); */

      if (retval && TYPE_MODE (valtype) != BLKmode
	  && any_pending_cleanups ())
	{
	  t = get_temp_regvar (valtype, retval);
	  use_temp = obey_regdecls;
	}
      else
	t = retval;

      emit_queue ();

      if (t == result)
	{
	  if (original_result_rtx)
	    store_expr (result, original_result_rtx, 0);
	  use_variable (DECL_RTL (t));
	  if (ctor_label)
	    expand_goto (ctor_label);
	  else
	    expand_null_return ();
	}
      else
	{
	  if (original_result_rtx)
	    {
	      if (t)
		expand_assignment (result, t, 0, 0);
	      else
		store_expr (result, original_result_rtx, 0);
	      result = build (SAVE_EXPR, TREE_TYPE (result),
			      error_mark_node, original_result_rtx);
	    }

	  if (ctor_label)
	    {
	      result = build (INIT_EXPR, TREE_TYPE (result), result, t);
	      TREE_VOLATILE (result) = 1;
	      expand_expr_stmt (result);
	      expand_goto (ctor_label);
	    }
	  else if (t)
	    {
	      result = build (INIT_EXPR, TREE_TYPE (result), result, t);
	      TREE_VOLATILE (result) = 1;
	      expand_return (result);
	    }
	  else
	    expand_return (result);
	}

      current_function_returns_value = 1;
      if (original_result_rtx)
	use_variable (original_result_rtx);
      if (use_temp)
	use_variable (DECL_RTL (t));
    }
  /* One way to clear out cleanups that EXPR might
     generate.  Note that this code will really be
     dead code, but that is ok--cleanups that were
     needed were handled by the magic of `return'.  */
  expand_cleanups_to (NULL_TREE);
}

/* Start a C switch statement, testing expression EXP.
   Return EXP if it is valid, an error node otherwise.  */

tree
c_expand_start_case (exp)
     tree exp;
{
  tree type = TREE_TYPE (exp);
  register enum tree_code code = TREE_CODE (type);

  if (IS_AGGR_TYPE_CODE (code))
    exp = build_type_conversion (CONVERT_EXPR, integer_type_node, exp, 1);
  else
    exp = default_conversion (exp);
  type = TREE_TYPE (exp);
  code = TREE_CODE (type);

  if (code != INTEGER_TYPE && code != ENUMERAL_TYPE && code != ERROR_MARK)
    {
      error ("switch quantity not an integer");
      exp = error_mark_node;
    }
  else
    {
      tree index;

      index = get_unwidened (exp, 0);
      /* We can't strip a conversion from a signed type to an unsigned,
	 because if we did, int_fits_type_p would do the wrong thing
	 when checking case values for being in range,
	 and it's too hard to do the right thing.  */
      if (TREE_UNSIGNED (TREE_TYPE (exp))
	  == TREE_UNSIGNED (TREE_TYPE (index)))
	exp = index;
    }

  expand_start_case (1, exp, type);

  return exp;
}
