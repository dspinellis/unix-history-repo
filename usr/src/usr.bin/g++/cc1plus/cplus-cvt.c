/* Language-level data type conversion for GNU C++.
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


/* This file contains the functions for converting C expressions
   to different data types.  The only entry point is `convert'.
   Every language front end must have a `convert' function
   but what kind of conversions it does will depend on the language.  */

#include "config.h"
#include "tree.h"
#include "cplus-tree.h"
#include "assert.h"

#define NULL 0

static tree build_up_reference ();

/* Change of width--truncation and extension of integers or reals--
   is represented with NOP_EXPR.  Proper functioning of many things
   assumes that no other conversions can be NOP_EXPRs.

   Conversion between integer and pointer is represented with CONVERT_EXPR.
   Converting integer to real uses FLOAT_EXPR
   and real to integer uses FIX_TRUNC_EXPR.

   Here is a list of all the functions that assume that widening and
   narrowing is always done with a NOP_EXPR:
     In c-convert.c, convert_to_integer.
     In c-typeck.c, build_binary_op_nodefault (boolean ops),
        and truthvalue_conversion.
     In expr.c: expand_expr, for operands of a MULT_EXPR.
     In fold-const.c: fold.
     In tree.c: get_narrower and get_unwidened.

   C++: in multiple-inheritance, converting between pointers may involve
   adjusting them by a delta stored within the class definition.  */

/* Subroutines of `convert'.  */

static tree
convert_to_pointer (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  
  if (integer_zerop (expr))
    {
      if (type == TREE_TYPE (null_pointer_node))
	return null_pointer_node;
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  if (form == POINTER_TYPE)
    {
      intype = TYPE_MAIN_VARIANT (intype);

      if (TYPE_MAIN_VARIANT (type) != intype
	  && IS_AGGR_TYPE (TREE_TYPE (type)) && IS_AGGR_TYPE (TREE_TYPE (intype)))
	{
	  enum tree_code code = PLUS_EXPR;
	  tree basetype = get_base_type (TREE_TYPE (TYPE_MAIN_VARIANT (type)),
					 TREE_TYPE (intype), 1);
	  if (basetype == error_mark_node)
	    return error_mark_node;
	  if (basetype == NULL_TREE)
	    {
	      basetype = get_base_type (TREE_TYPE (intype),
					TREE_TYPE (TYPE_MAIN_VARIANT (type)), 1);
	      if (basetype == error_mark_node)
		return error_mark_node;
	      code = MINUS_EXPR;
	    }
	  if (basetype)
	    {
	      if (TYPE_USES_VIRTUAL_BASECLASSES (TREE_TYPE (type))
		  || TYPE_USES_VIRTUAL_BASECLASSES (TREE_TYPE (intype))
		  || DECL_OFFSET (TYPE_NAME (basetype)) != 0)
		{
		  /* Need to get the path we took.  */
		  tree path;

		  if (code == PLUS_EXPR)
		    get_base_distance (TREE_TYPE (type), TREE_TYPE (intype), 0, &path);
		  else
		    get_base_distance (TREE_TYPE (intype), TREE_TYPE (type), 0, &path);
		  return build_vbase_path (code, type, expr, path, 0);
		}
	    }
	}
      return build1 (NOP_EXPR, type, expr);
    }

  if (form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    {
      if (type_precision (intype) == POINTER_SIZE)
	return build1 (CONVERT_EXPR, type, expr);
      return convert_to_pointer (type,
				 convert (type_for_size (POINTER_SIZE, 0),
					  expr));
    }

  assert (form != OFFSET_TYPE);

  if (IS_AGGR_TYPE (intype))
    {
      /* If we cannot convert to the specific pointer type,
	 try to convert to the type `void *'.  */
      tree rval;
      rval = build_type_conversion (CONVERT_EXPR, type, expr, 1);
      if (rval)
	{
	  if (rval == error_mark_node)
	    error ("ambiguous pointer conversion");
	  return rval;
	}
    }

  error ("cannot convert to a pointer type");

  return null_pointer_node;
}

/* Like convert, except permit conversions to take place which
   are not normally allowed due to visibility restrictions
   (such as conversion from sub-type to private super-type).  */
static tree
convert_to_pointer_force (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  
  if (integer_zerop (expr))
    {
      if (type == TREE_TYPE (null_pointer_node))
	return null_pointer_node;
      expr = build_int_2 (0, 0);
      TREE_TYPE (expr) = type;
      return expr;
    }

  if (form == POINTER_TYPE)
    {
      intype = TYPE_MAIN_VARIANT (intype);

      if (TYPE_MAIN_VARIANT (type) != intype
	  && IS_AGGR_TYPE (TREE_TYPE (type)) && IS_AGGR_TYPE (TREE_TYPE (intype)))
	{
	  enum tree_code code = PLUS_EXPR;
	  tree path, basetype;
	  int distance = get_base_distance (TREE_TYPE (type),
					    TYPE_MAIN_VARIANT (TREE_TYPE (intype)), 0, &path);
	  if (distance == -2)
	    {
	    ambig:
	      error_with_aggr_type (TREE_TYPE (type), "type `%s' is ambiguous baseclass of `%s'",
				    TYPE_NAME_STRING (TREE_TYPE (intype)));
	      return error_mark_node;
	    }
	  if (distance == -1)
	    {
	      distance = get_base_distance (TREE_TYPE (intype),
					    TYPE_MAIN_VARIANT (TREE_TYPE (type)), 0, &path);
	      if (distance == -2)
		goto ambig;
	      if (distance < 0)
		/* Doesn't need any special help from us.  */
		return build1 (NOP_EXPR, type, expr);

	      code = MINUS_EXPR;
	    }
	  return build_vbase_path (code, type, expr, path, 0);
	}
      return build1 (NOP_EXPR, type, expr);
    }

  return convert_to_pointer (type, expr);
}

/* We are passing something to a function which requires a reference.
   The type we are interested in is in TYPE. The initial
   value we have to begin with is in ARG.

   FLAGS controls how we manage visibility checking.  */
static tree
build_up_reference (type, arg, flags)
     tree type, arg;
     int flags;
{
  tree rval;
  int literal_flag = 0;
  tree argtype = TREE_TYPE (arg), basetype = argtype;
  tree target_type = TREE_TYPE (type);

  assert (TREE_CODE (type) == REFERENCE_TYPE);
  if (TYPE_MAIN_VARIANT (argtype) != TYPE_MAIN_VARIANT (target_type)
      && IS_AGGR_TYPE (argtype)
      && IS_AGGR_TYPE (target_type))
    {
      basetype = get_base_type (target_type, TYPE_MAIN_VARIANT (argtype),
				(flags & LOOKUP_PROTECTED_OK) ? 3 : 2);
      if ((flags & LOOKUP_PROTECT) && basetype == error_mark_node)
	return error_mark_node;
      if (basetype == NULL_TREE)
	{
	  error_not_base_type (target_type, argtype);
	  return error_mark_node;
	}
    }

  switch (TREE_CODE (arg))
    {
    case INDIRECT_REF:
      /* This is a call to a constructor which did not know what it was
	 initializing until now: it needs to initialize a temporary.  */
      if (TYPE_HAS_CONSTRUCTOR (arg))
	{
	  tree temp = build_cplus_new (argtype, TREE_OPERAND (arg, 0));
	  TYPE_HAS_CONSTRUCTOR (arg) = 0;
	  return build_up_reference (type, temp, flags);
	}
      /* Let &* cancel out to simplify resulting code.
         Also, throw away intervening NOP_EXPRs.  */
      arg = TREE_OPERAND (arg, 0);
      if (TREE_CODE (arg) == NOP_EXPR || TREE_CODE (arg) == REFERENCE_EXPR)
	arg = TREE_OPERAND (arg, 0);

      rval = build1 (REFERENCE_EXPR, type, arg);
      literal_flag = TREE_LITERAL (arg);
      goto done;

      /* Get this out of a register if we happened to be in one by accident.
	 Also, build up references to non-lvalues it we must.  */
      /* For &x[y], return (&) x+y */
    case ARRAY_REF:
      if (mark_addressable (TREE_OPERAND (arg, 0)) == 0)
	return error_mark_node;
      rval = build_binary_op (PLUS_EXPR, TREE_OPERAND (arg, 0),
			      TREE_OPERAND (arg, 1));
      TREE_TYPE (rval) = type;
      if (TREE_LITERAL (TREE_OPERAND (arg, 1))
	  && staticp (TREE_OPERAND (arg, 0)))
	TREE_LITERAL (rval) = 1;
      return rval;

    case SCOPE_REF:
      /* Could be a reference to a static member.  */
      {
	tree field = TREE_OPERAND (arg, 1);
	if (TREE_STATIC (field))
	  {
	    rval = build1 (ADDR_EXPR, type, field);
	    literal_flag = 1;
	    goto done;
	  }
      }
      /* we should have farmed out member pointers above.  */
      assert (0);

    case COMPONENT_REF:
      rval = build_component_addr (arg, build_pointer_type (argtype),
				   "attempt to make a reference to bit-field structure member `%s'");
      rval = build1 (NOP_EXPR, type, rval);
      literal_flag = staticp (TREE_OPERAND (arg, 0));
#if 0
      goto done_but_maybe_warn;
#else
      goto done;
#endif

      /* Anything not already handled and not a true memory reference
	 needs to have a reference built up.  Do so silently for
	 things like integers and return values from function,
	 but complain if we need a reference to something declared
	 as `register'.  */

    case RESULT_DECL:
      if (staticp (arg))
	literal_flag = 1;
      TREE_ADDRESSABLE (arg) = 1;
      put_var_into_stack (arg);
      break;

    case PARM_DECL:
      if (arg == current_class_decl)
	{
	  error ("address of `this' not available");
	  TREE_ADDRESSABLE (arg) = 1; /* so compiler doesn't die later */
	  put_var_into_stack (arg);
	  break;
	}
      /* Fall through.  */
    case VAR_DECL:
    case CONST_DECL:
      if (TREE_REGDECL (arg) && !TREE_ADDRESSABLE (arg))
	warning ("address needed to build reference for `%s', which is declared `register'",
		 IDENTIFIER_POINTER (DECL_NAME (arg)));
      else if (staticp (arg))
	literal_flag = 1;

      TREE_ADDRESSABLE (arg) = 1;
      put_var_into_stack (arg);
      break;

    case COMPOUND_EXPR:
      {
	tree real_reference = build_up_reference (type, TREE_OPERAND (arg, 1), 1);
	rval = build (COMPOUND_EXPR, type, TREE_OPERAND (arg, 0), real_reference);
	TREE_LITERAL (rval) = staticp (TREE_OPERAND (arg, 1));
	return rval;
      }

    case MODIFY_EXPR:
    case INIT_EXPR:
      {
	tree real_reference = build_up_reference (type, TREE_OPERAND (arg, 0), 1);
	rval = build (COMPOUND_EXPR, type, arg, real_reference);
	TREE_LITERAL (rval) = staticp (TREE_OPERAND (arg, 0));
	return rval;
      }

    case COND_EXPR:
      return build (COND_EXPR, type,
		    TREE_OPERAND (arg, 0),
		    build_up_reference (type, TREE_OPERAND (arg, 1), 1),
		    build_up_reference (type, TREE_OPERAND (arg, 2), 1));

    case WITH_CLEANUP_EXPR:
      rval = build (WITH_CLEANUP_EXPR, type,
		    build_up_reference (type, TREE_OPERAND (arg, 0), 1),
		    0, TREE_OPERAND (arg, 2));
      return rval;

    default:
      break;
    }

  if (TREE_ADDRESSABLE (arg) == 0)
    {
      tree temp;

      if (TREE_CODE (arg) == CALL_EXPR && IS_AGGR_TYPE (argtype))
	{
	  temp = build_cplus_new (argtype, arg);
	  rval = build1 (ADDR_EXPR, type, temp);
	  goto done;
	}
      else
	{
	  temp = get_temp_name (argtype, 0);
	  if (global_bindings_p ())
	    {
	      /* Give this new temp some rtl and initialize it.  */
	      DECL_INITIAL (temp) = arg;
	      TREE_STATIC (temp) = 1;
	      finish_decl (temp, arg, NULL_TREE);
	      /* Do this after declaring it static.  */
	      rval = build_unary_op (ADDR_EXPR, temp, 0);
	      literal_flag = TREE_LITERAL (rval);
	      goto done;
	    }
	  else
	    {
	      rval = build_unary_op (ADDR_EXPR, temp, 0);
	      /* Put a value into the rtl.  */
	      if (IS_AGGR_TYPE (argtype))
		{
		  /* This may produce surprising results,
		     since we commit to initializing the temp
		     when the temp may not actually get used.  */
		  expand_aggr_init (temp, arg, 0);
		  TREE_TYPE (rval) = type;
		  literal_flag = TREE_LITERAL (rval);
		  goto done;
		}
	      else
		{
		  if (basetype != argtype)
		    rval = convert_pointer_to (target_type, rval);
		  else
		    TREE_TYPE (rval) = type;
		  return build (COMPOUND_EXPR, type,
				build (MODIFY_EXPR, argtype, temp, arg), rval);
		}
	    }
	}
    }
  else
    rval = build1 (ADDR_EXPR, type, arg);

 done_but_maybe_warn:
  if (TREE_READONLY (arg)
      && ! TREE_READONLY (target_type))
    readonly_warning_or_error (arg, "conversion to reference");

 done:
  if (TYPE_LANG_SPECIFIC (argtype)
      && (TYPE_USES_MULTIPLE_INHERITANCE (argtype)
	  || TYPE_USES_VIRTUAL_BASECLASSES (argtype)))
    {
      TREE_TYPE (rval) = TYPE_POINTER_TO (argtype);
      rval = convert_pointer_to (target_type, rval);
      rval = build1 (NOP_EXPR, type, rval);
    }
  TREE_LITERAL (rval) = literal_flag;
  return rval;
}

/* For C++: Only need to do one-level references, but cannot
   get tripped up on signed/unsigned differences.

   If DECL is NULL_TREE it means convert as though casting (by force).
   If it is ERROR_MARK_NODE, it means the conversion is implicit,
   and that temporaries may be created.
   Otherwise, DECL is a _DECL node which can be used in error reporting.  */
tree
convert_to_reference (decl, reftype, expr, strict, flags)
     tree decl;
     tree reftype, expr;
     int strict, flags;
{
  register tree type = TYPE_MAIN_VARIANT (TREE_TYPE (reftype));
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);

  assert (TREE_CODE (reftype) == REFERENCE_TYPE);

  if (form == REFERENCE_TYPE)
    intype = TREE_TYPE (intype);
  intype = TYPE_MAIN_VARIANT (intype);

  /* @@ Probably need to have a check for X(X&) here.  */

  if (IS_AGGR_TYPE (intype))
    {
      tree rval = build_type_conversion (CONVERT_EXPR, reftype, expr, 1);
      if (rval)
	{
	  if (rval == error_mark_node)
	    error ("ambiguous pointer conversion");
	  return rval;
	}
      else if (rval = build_type_conversion (CONVERT_EXPR, type, expr, 1))
	{
	  if (TYPE_NEEDS_DESTRUCTOR (type))
	    rval = cleanup_after_call (rval);
	  else
	    {
	      decl = get_temp_name (type, 0);
	      rval = build (INIT_EXPR, type, decl, rval);
	      rval = build (COMPOUND_EXPR, reftype, rval,
			    convert_to_reference (NULL_TREE, reftype, decl,
						  strict, flags));

	    }
	  return rval;
	}

      if (form == REFERENCE_TYPE
	  && type != intype
	  && TYPE_LANG_SPECIFIC (intype)
	  && (TYPE_USES_VIRTUAL_BASECLASSES (intype)
	      || TYPE_USES_MULTIPLE_INHERITANCE (intype)))
	{
	  /* If it may move around, build a fresh reference.  */
	  expr = convert_from_reference (expr);
	  form = TREE_CODE (TREE_TYPE (expr));
	}
    }

  /* @@ Perhaps this should try to go through a constructor first
     @@ for proper initialization, but I am not sure when that
     @@ is needed or desirable.

     @@ The second disjunct is provided to make references behave
     @@ as some people think they should, i.e., an interconvertability
     @@ between references to builtin types (such as short and
     @@ unsigned short).  There should be no conversion between
     @@ types whose codes are different, or whose sizes are different.  */

  if (((IS_AGGR_TYPE (type) || IS_AGGR_TYPE (intype))
       && comptypes (type, intype, strict))
      || (!IS_AGGR_TYPE (type)
	  && TREE_CODE (type) == TREE_CODE (intype)
	  && int_size_in_bytes (type) == int_size_in_bytes (intype)))
    {
      /* If EXPR is of aggregate type, and is really a CALL_EXPR,
	 then we don't need to convert it to reference type if
	 it is only being used to initialize DECL which is also
	 of the same aggregate type.  */
      if (form == REFERENCE_TYPE
	  || (decl != NULL_TREE && decl != error_mark_node
	      && IS_AGGR_TYPE (type)
	      && TREE_CODE (expr) == CALL_EXPR
	      && TYPE_MAIN_VARIANT (type) == intype))
	{
	  if (decl && decl != error_mark_node)
	    {
	      tree e1 = build (INIT_EXPR, void_type_node, decl, expr);
	      tree e2;

	      TREE_VOLATILE (e1) = 1;
	      if (form == REFERENCE_TYPE)
		e2 = build1 (NOP_EXPR, reftype, decl);
	      else
		{
		  e2 = build_unary_op (ADDR_EXPR, decl, 0);
		  e2 = build1 (REFERENCE_EXPR, reftype, e2);
		}
	      return build_compound_expr (tree_cons (NULL_TREE, e1,
						     build_tree_list (NULL_TREE, e2)));
	    }
	  expr = copy_node (expr);
	  TREE_TYPE (expr) = reftype;
	  return expr;
	}
      if (decl == error_mark_node)
	flags |= LOOKUP_PROTECTED_OK;
      return build_up_reference (reftype, expr, flags);
    }

  /* Definitely need to go through a constructor here.  */
  if (TYPE_HAS_CONSTRUCTOR (type))
    {
      tree init = build_method_call (NULL_TREE, DECL_NAME (TYPE_NAME (type)), build_tree_list (NULL_TREE, expr), CLASSTYPE_AS_LIST (type), LOOKUP_NORMAL);
      tree rval;

      if (init == error_mark_node)
	return error_mark_node;
      rval = build_cplus_new (type, init);
      if (decl == error_mark_node)
	flags |= LOOKUP_PROTECTED_OK;
      return build_up_reference (reftype, rval, flags);
    }

  assert (form != OFFSET_TYPE);

  error ("cannot convert to a reference type");

  return error_mark_node;
}

/* We are using a reference VAL for its value. Bash that reference all the
   way down to its lowest form. */
tree
convert_from_reference (val)
     tree val;
{
  tree type = TREE_TYPE (val);

#if 0
  if (TREE_CODE (val) == REFERENCE_EXPR)
    {
      val = build1 (NOP_EXPR, build_pointer_type (TREE_TYPE (type)),
		    TREE_OPERAND (val, 0));
      return val;
    }
#endif
  if (TREE_CODE (type) == OFFSET_TYPE)
    type = TREE_TYPE (type);
 if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      tree dt = TREE_TYPE (type);

      /* This can happen if we cast to a reference type.  */
      if (TREE_CODE (val) == ADDR_EXPR)
	{
	  val = build1 (NOP_EXPR, build_pointer_type (dt), val);
	  val = build_indirect_ref (val, 0);
	  return val;
	}

      val = build1 (INDIRECT_REF, TYPE_MAIN_VARIANT (dt), val);

      TREE_THIS_VOLATILE (val) = TREE_VOLATILE (dt);
      TREE_READONLY (val) = TREE_READONLY (dt);
    }
  return val;
}

static tree
convert_to_real (type, expr)
     tree type, expr;
{
  register enum tree_code form = TREE_CODE (TREE_TYPE (expr));
  extern int flag_float_store;

  if (form == REAL_TYPE)
    return build1 (flag_float_store ? CONVERT_EXPR : NOP_EXPR,
		  type, expr);

  if (form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    return build1 (FLOAT_EXPR, type, expr);

  assert (form != OFFSET_TYPE);

  if (form == POINTER_TYPE)
    error ("pointer value used where a floating point value was expected");
  /* C++: check to see if we can convert this aggregate type
     into the required scalar type.  */
  else if (IS_AGGR_TYPE (TREE_TYPE (expr)))
    {
      tree rval;
      rval = build_type_conversion (CONVERT_EXPR, type, expr, 1);
      if (rval)
	return rval;
      else
	error ("aggregate value used where a floating point value was expected");
    }

  {
    register tree tem = make_node (REAL_CST);
    TREE_TYPE (tem) = type;
    TREE_REAL_CST (tem) = 0;
    return tem;
  }
}

/* The result of this is always supposed to be a newly created tree node
   not in use in any existing structure.  */

static tree
convert_to_integer (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  extern tree build_binary_op_nodefault ();
  extern tree build_unary_op ();

  if (form == POINTER_TYPE)
    {
      if (integer_zerop (expr))
	expr = integer_zero_node;
      else
	expr = fold (build1 (CONVERT_EXPR,
			     type_for_size (POINTER_SIZE, 0), expr));
      intype = TREE_TYPE (expr);
      form = TREE_CODE (intype);
      if (intype == type)
	return expr;
    }

  if (form == INTEGER_TYPE || form == ENUMERAL_TYPE)
    {
      register int outprec = TYPE_PRECISION (type);
      register int inprec = TYPE_PRECISION (intype);
      register enum tree_code ex_form = TREE_CODE (expr);

      if (outprec >= inprec)
	return build1 (NOP_EXPR, type, expr);

/* Here detect when we can distribute the truncation down past some arithmetic.
   For example, if adding two longs and converting to an int,
   we can equally well convert both to ints and then add.
   For the operations handled here, such truncation distribution
   is always safe.
   It is desirable in these cases:
   1) when truncating down to full-word from a larger size
   2) when truncating takes no work.
   3) when at least one operand of the arithmetic has been extended
   (as by C's default conversions).  In this case we need two conversions
   if we do the arithmetic as already requested, so we might as well
   truncate both and then combine.  Perhaps that way we need only one.

   Note that in general we cannot do the arithmetic in a type
   shorter than the desired result of conversion, even if the operands
   are both extended from a shorter type, because they might overflow
   if combined in that type.  The exceptions to this--the times when
   two narrow values can be combined in their narrow type even to
   make a wider result--are handled by "shorten" in build_binary_op.  */

      switch (ex_form)
	{
	case RSHIFT_EXPR:
	  /* We can pass truncation down through right shifting
	     when the shift count is a negative constant.  */
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) != INTEGER_CST
	      || TREE_INT_CST_LOW (TREE_OPERAND (expr, 1)) > 0)
	    break;
	  goto trunc1;

	case LSHIFT_EXPR:
	  /* We can pass truncation down through left shifting
	     when the shift count is a positive constant.  */
	  if (TREE_CODE (TREE_OPERAND (expr, 1)) != INTEGER_CST
	      || TREE_INT_CST_LOW (TREE_OPERAND (expr, 1)) < 0)
	    break;
	  /* In this case, shifting is like multiplication.  */
	  goto trunc1;

	case MAX_EXPR:
	case MIN_EXPR:
	case MULT_EXPR:
	  {
	    tree arg0 = get_unwidened (TREE_OPERAND (expr, 0), type);
	    tree arg1 = get_unwidened (TREE_OPERAND (expr, 1), type);

	    /* Don't distribute unless the output precision is at least as big
	       as the actual inputs.  Otherwise, the comparison of the
	       truncated values will be wrong.  */
	    if (outprec >= TYPE_PRECISION (TREE_TYPE (arg0))
		&& outprec >= TYPE_PRECISION (TREE_TYPE (arg1))
		/* If signedness of arg0 and arg1 don't match,
		   we can't necessarily find a type to compare them in.  */
		&& (TREE_UNSIGNED (TREE_TYPE (arg0))
		    == TREE_UNSIGNED (TREE_TYPE (arg1))))
	      goto trunc1;
	    break;
	  }

	case PLUS_EXPR:
	case MINUS_EXPR:
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	case BIT_ANDTC_EXPR:
	trunc1:
	  {
	    tree arg0 = get_unwidened (TREE_OPERAND (expr, 0), type);
	    tree arg1 = get_unwidened (TREE_OPERAND (expr, 1), type);

	    if (outprec >= BITS_PER_WORD
		|| TRULY_NOOP_TRUNCATION (outprec, inprec)
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg0))
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg1)))
	      {
		/* Do the arithmetic in type TYPEX,
		   then convert result to TYPE.  */
		register tree typex = type;

		/* Can't do arithmetic in enumeral types
		   so use an integer type that will hold the values.  */
		if (TREE_CODE (typex) == ENUMERAL_TYPE)
		  typex = type_for_size (TYPE_PRECISION (typex),
					 TREE_UNSIGNED (typex));

		/* But now perhaps TYPEX is as wide as INPREC.
		   In that case, do nothing special here.
		   (Otherwise would recurse infinitely in convert.  */
		if (TYPE_PRECISION (typex) != inprec)
		  {
		    /* Don't do unsigned arithmetic where signed was wanted,
		       or vice versa.
		       Exception: if the original operands were unsigned
		       then can safely do the work as unsigned.
		       And we may need to do it as unsigned
		       if we truncate to the original size.  */
		    typex = ((TREE_UNSIGNED (TREE_TYPE (expr))
			      || TREE_UNSIGNED (TREE_TYPE (arg0)))
			     ? unsigned_type (typex) : signed_type (typex));
		    return convert (type,
				    build_binary_op_nodefault (ex_form,
							       convert (typex, arg0),
							       convert (typex, arg1),
							       ex_form));
		  }
	      }
	  }
	  break;

	case EQ_EXPR:
	case NE_EXPR:
	case GT_EXPR:
	case GE_EXPR:
	case LT_EXPR:
	case LE_EXPR:
	case TRUTH_AND_EXPR:
	case TRUTH_ANDIF_EXPR:
	case TRUTH_OR_EXPR:
	case TRUTH_ORIF_EXPR:
	case TRUTH_NOT_EXPR:
	  /* If we want result of comparison converted to a byte,
	     we can just regard it as a byte, since it is 0 or 1.  */
	  TREE_TYPE (expr) = type;
	  return expr;

	case NEGATE_EXPR:
	case BIT_NOT_EXPR:
	case ABS_EXPR:
	  {
	    register tree typex = type;

	    /* Can't do arithmetic in enumeral types
	       so use an integer type that will hold the values.  */
	    if (TREE_CODE (typex) == ENUMERAL_TYPE)
	      typex = type_for_size (TYPE_PRECISION (typex),
				     TREE_UNSIGNED (typex));

	    /* But now perhaps TYPEX is as wide as INPREC.
	       In that case, do nothing special here.
	       (Otherwise would recurse infinitely in convert.  */
	    if (TYPE_PRECISION (typex) != inprec)
	      {
		/* Don't do unsigned arithmetic where signed was wanted,
		   or vice versa.  */
		typex = (TREE_UNSIGNED (TREE_TYPE (expr))
			 ? unsigned_type (typex) : signed_type (typex));
		return convert (type,
				build_unary_op (ex_form,
						convert (typex, TREE_OPERAND (expr, 0)),
						1));
	      }
	  }

	case NOP_EXPR:
	  /* If truncating after truncating, might as well do all at once.
	     If truncating after extending, we may get rid of wasted work.  */
	  return convert (type, get_unwidened (TREE_OPERAND (expr, 0), type));

	case COND_EXPR:
	  /* Can treat the two alternative values like the operands
	     of an arithmetic expression.  */
	  {
	    tree arg1 = get_unwidened (TREE_OPERAND (expr, 1), type);
	    tree arg2 = get_unwidened (TREE_OPERAND (expr, 2), type);

	    if (outprec >= BITS_PER_WORD
		|| TRULY_NOOP_TRUNCATION (outprec, inprec)
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg1))
		|| inprec > TYPE_PRECISION (TREE_TYPE (arg2)))
	      {
		/* Do the arithmetic in type TYPEX,
		   then convert result to TYPE.  */
		register tree typex = type;

		/* Can't do arithmetic in enumeral types
		   so use an integer type that will hold the values.  */
		if (TREE_CODE (typex) == ENUMERAL_TYPE)
		  typex = type_for_size (TYPE_PRECISION (typex),
					 TREE_UNSIGNED (typex));

		/* But now perhaps TYPEX is as wide as INPREC.
		   In that case, do nothing special here.
		   (Otherwise would recurse infinitely in convert.  */
		if (TYPE_PRECISION (typex) != inprec)
		  {
		    /* Don't do unsigned arithmetic where signed was wanted,
		       or vice versa.  */
		    typex = (TREE_UNSIGNED (TREE_TYPE (expr))
			     ? unsigned_type (typex) : signed_type (typex));
		    return convert (type,
				    build (COND_EXPR, typex,
					   TREE_OPERAND (expr, 0),
					   convert (typex, arg1),
					   convert (typex, arg2)));
		  }
	      }
	  }

	}

      return build1 (NOP_EXPR, type, expr);
    }

  if (form == REAL_TYPE)
    return build1 (FIX_TRUNC_EXPR, type, expr);

  if (form == OFFSET_TYPE)
    error_with_decl (TYPE_NAME (TYPE_OFFSET_BASETYPE (intype)),
		     "pointer-to-member expression object not composed with type `%s' object");
  else
    {
      if (IS_AGGR_TYPE (intype))
	{
	  tree rval;
	  rval = build_type_conversion (CONVERT_EXPR, type, expr, 1);
	  if (rval) return rval;
	}

      error ("aggregate value used where an integer was expected");
    }

  {
    register tree tem = build_int_2 (0, 0);
    TREE_TYPE (tem) = type;
    return tem;
  }
}

/* See if there is a constructor of type TYPE which will convert
   EXPR.  The reference manual seems to suggest (8.5.6) that we need
   not worry about finding constructors for base classes, then converting
   to the derived class.

   MSGP is a pointer to a message that would be an appropriate error
   string.  If MSGP is NULL, then we are not interested in reporting
   errors.  */
tree
convert_to_aggr (type, expr, msgp, protect)
     tree type, expr;
     char **msgp;
{
  tree basetype = TYPE_MAIN_VARIANT (type);
  tree name = DECL_NAME (TYPE_NAME (basetype));
  tree field;
  tree function, fntype, parmtypes, parmlist, result;
  tree method_name;
  enum visibility_type visibility;
  int can_be_private, can_be_protected;

  if (! TYPE_HAS_CONSTRUCTOR (basetype))
    {
      if (msgp)
	*msgp = "type `%s' does not have a constructor";
      return error_mark_node;
    }

  visibility = visibility_public;
  can_be_private = 0;
  can_be_protected = IDENTIFIER_CLASS_VALUE (name) || name == current_class_name;

  parmlist = build_tree_list (NULL_TREE, expr);
  parmtypes = tree_cons (NULL_TREE, TREE_TYPE (expr), void_list_node);

  if (TYPE_USES_VIRTUAL_BASECLASSES (basetype))
    {
      parmtypes = tree_cons (NULL_TREE, integer_type_node, parmtypes);
      parmlist = tree_cons (NULL_TREE, integer_one_node, parmlist);
    }

  /* The type of the first argument will be filled in inside the loop.  */
  parmlist = tree_cons (NULL_TREE, integer_zero_node, parmlist);
  parmtypes = tree_cons (NULL_TREE, TYPE_POINTER_TO (basetype), parmtypes);

  method_name = build_decl_overload (IDENTIFIER_POINTER (name), parmtypes, 1);

  /* constructors are up front.  */
  field = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0);
  if (TYPE_HAS_DESTRUCTOR (basetype))
    field = TREE_CHAIN (field);

  while (field)
    {
      if (DECL_NAME (field) == method_name)
	{
	  function = field;
	  if (protect)
	    {
	      if (TREE_PRIVATE (field))
		{
		  can_be_private =
		    (basetype == current_class_type
		     || is_friend (basetype, current_function_decl)
		     || purpose_member (basetype, DECL_VISIBILITY (field)));
		  if (! can_be_private)
		    goto found;
		}
	      else if (TREE_PROTECTED (field))
		{
		  if (! can_be_protected)
		    goto found;
		}
	    }
	  goto found_and_ok;
	}
      field = TREE_CHAIN (field);
    }

  /* No exact conversion was found.  See if an approximate
     one will do.  */
  field = TREE_VEC_ELT (CLASSTYPE_METHOD_VEC (basetype), 0);
  if (TYPE_HAS_DESTRUCTOR (basetype))
    field = TREE_CHAIN (field);

  {
    int saw_private = 0;
    int saw_protected = 0;
    struct candidate *candidates =
      (struct candidate *) alloca ((list_length (field)+1) * sizeof (struct candidate));
    struct candidate *cp = candidates;

    while (field)
      {
	function = field;
	cp->harshness = (unsigned short *)alloca (3 * sizeof (short));
	compute_conversion_costs (function, parmlist, cp, 2);
	if (cp->evil == 0)
	  {
	    cp->u.field = field;
	    if (protect)
	      {
		if (TREE_PRIVATE (field))
		  visibility = visibility_private;
		else if (TREE_PROTECTED (field))
		  visibility = visibility_protected;
		else
		  visibility = visibility_public;
	      }
	    else
	      visibility = visibility_public;

	    if (visibility == visibility_private
		? (basetype == current_class_type
		   || is_friend (basetype, cp->function)
		   || purpose_member (basetype, DECL_VISIBILITY (field)))
		: visibility == visibility_protected
		? (can_be_protected
		   || purpose_member (basetype, DECL_VISIBILITY (field)))
		: 1)
	      {
		if (cp->user == 0 && cp->b_or_d == 0
		    && cp->easy <= 1)
		  {
		    goto found_and_ok;
		  }
		cp++;
	      }
	    else
	      {
		if (visibility == visibility_private)
		  saw_private = 1;
		else
		  saw_protected = 1;
	      }
	  }
	field = TREE_CHAIN (field);
      }
    if (cp - candidates)
      {
	/* Rank from worst to best.  Then cp will point to best one.
	   Private fields have their bits flipped.  For unsigned
	   numbers, this should make them look very large.
	   If the best alternate has a (signed) negative value,
	   then all we ever saw were private members.  */
	if (cp - candidates > 1)
	  qsort (candidates,	/* char *base */
		 cp - candidates, /* int nel */
		 sizeof (struct candidate), /* int width */
		 rank_for_overload); /* int (*compar)() */

	--cp;
	if (cp->evil > 1)
	  {
	    if (msgp)
	      *msgp = "ambiguous type conversion possible for `%s'";
	    return error_mark_node;
	  }

	function = cp->function;
	field = cp->u.field;
	goto found_and_ok;
      }
    else if (msgp)
      {
	if (saw_private)
	  if (saw_protected)
	    *msgp = "only private and protected conversions apply";
	  else
	    *msgp = "only private conversions apply";
	else if (saw_protected)
	  *msgp = "only protected conversions apply";
      }
    return error_mark_node;
  }
  /* NOTREACHED */

 not_found:
  if (msgp) *msgp = "no appropriate conversion to type `%s'";
  return error_mark_node;
 found:
  if (visibility == visibility_private)
    if (! can_be_private)
      {
	if (msgp)
	  *msgp = TREE_PRIVATE (field)
	    ? "conversion to type `%s' is private"
	    : "conversion to type `%s' is from private base class";
	return error_mark_node;
      }
  if (visibility == visibility_protected)
    if (! can_be_protected)
      {
	if (msgp)
	  *msgp = TREE_PRIVATE (field)
	    ? "conversion to type `%s' is protected"
	    : "conversion to type `%s' is from protected base class";
	return error_mark_node;
      }
  function = field;
 found_and_ok:

  /* It will convert, but we don't do anything about it yet.  */
  if (msgp == 0)
    return NULL_TREE;

  fntype = TREE_TYPE (function);
  if (TREE_INLINE (function) && TREE_CODE (function) == FUNCTION_DECL)
    function = build1 (ADDR_EXPR, build_pointer_type (fntype), function);
  else
    function = default_conversion (function);

  result = build_nt (CALL_EXPR, function,
		     actualparameterlist (NULL_TREE, TYPE_ARG_TYPES (fntype), parmlist, NULL_TREE, LOOKUP_NORMAL),
		     NULL_TREE);
  TREE_TYPE (result) = TREE_TYPE (fntype);
  TREE_VOLATILE (result) = 1;
  TREE_RAISES (result) = !! TYPE_RAISES_EXCEPTIONS (fntype);
  return result;
}

/* Call this when we know (for any reason) that expr is
   not, in fact, zero.  */
tree
convert_pointer_to (type, expr)
     tree type, expr;
{
  register tree intype = TREE_TYPE (expr);
  register enum tree_code form = TREE_CODE (intype);
  tree ptr_type = build_pointer_type (type);
  tree rval;

  if (TYPE_MAIN_VARIANT (ptr_type) == TYPE_MAIN_VARIANT (intype))
    return expr;

  if (intype == error_mark_node)
    return error_mark_node;

  assert (form == POINTER_TYPE);
  assert (!integer_zerop (expr));

  if (IS_AGGR_TYPE (type)
      && IS_AGGR_TYPE (TREE_TYPE (intype))
      && TYPE_MAIN_VARIANT (type) != TYPE_MAIN_VARIANT (TREE_TYPE (intype)))
    {
      tree path, basetype;
      int distance = get_base_distance (type, TYPE_MAIN_VARIANT (TREE_TYPE (intype)), 0, &path);

      /* This function shouldn't be called with
	 unqualified arguments.  */
      assert (distance >= 0);

      return build_vbase_path (PLUS_EXPR, ptr_type, expr, path, 1);
    }
  rval = build1 (NOP_EXPR, ptr_type,
		 TREE_CODE (expr) == NOP_EXPR
		 ? TREE_OPERAND (expr, 0) : expr);
  TREE_LITERAL (rval) = TREE_LITERAL (expr);
  return rval;
}

/* Same as above, but don't abort if we get an "ambiguous" baseclass.
   There's only one virtual baseclass we are looking for, and once
   we find one such virtual baseclass, we have found them all.  */

tree
convert_pointer_to_vbase (type, expr)
     tree type;
     tree expr;
{
  tree intype = TREE_TYPE (TREE_TYPE (expr));
  int i;

  for (i = CLASSTYPE_N_BASECLASSES (intype); i > 0; i--)
    {
      tree basetype = CLASSTYPE_BASECLASS (intype, i);
      if (type == basetype)
	return convert_pointer_to (type, expr);
      if (value_member (TYPE_MAIN_VARIANT (type),
			CLASSTYPE_VBASECLASSES (basetype)))
	return convert_pointer_to_vbase (type, convert_pointer_to (TYPE_MAIN_VARIANT (basetype), expr));
    }
  abort ();
}

/* Create an expression whose value is that of EXPR,
   converted to type TYPE.  The TREE_TYPE of the value
   is always TYPE.  This function implements all reasonable
   conversions; callers should filter out those that are
   not permitted by the language being compiled.  */

tree
convert (type, expr)
     tree type, expr;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (type == TREE_TYPE (expr) || TREE_CODE (expr) == ERROR_MARK)
    return expr;
  if (TREE_CODE (TREE_TYPE (expr)) == ERROR_MARK)
    return error_mark_node;
  if (TREE_CODE (TREE_TYPE (expr)) == VOID_TYPE)
    {
      error ("void value not ignored as it ought to be");
      return error_mark_node;
    }
  if (code == VOID_TYPE)
    {
      tree rval = build_type_conversion (NOP_EXPR, type, e, 0);
      /* If we can convert to void type via a type conversion, do so.  */
      if (rval)
	return rval;
      return build1 (CONVERT_EXPR, type, e);
    }
#if 0
  /* This is incorrect.  A truncation can't be stripped this way.
     Extensions will be stripped by the use of get_unwidened.  */
  if (TREE_CODE (expr) == NOP_EXPR)
    return convert (type, TREE_OPERAND (expr, 0));
#endif

  /* Just convert to the type of the member.  */
  if (code == OFFSET_TYPE)
    {
      type = TREE_TYPE (type);
      code = TREE_CODE (type);
    }

  /* C++ */
  if (code == REFERENCE_TYPE)
    return fold (convert_to_reference (error_mark_node, type, e, -1, LOOKUP_NORMAL));
  else if (TREE_CODE (TREE_TYPE (e)) == REFERENCE_TYPE)
    e = convert_from_reference (e);

  if (code == INTEGER_TYPE || code == ENUMERAL_TYPE)
    return fold (convert_to_integer (type, e));
  if (code == POINTER_TYPE)
    return fold (convert_to_pointer (type, e));
  if (code == REAL_TYPE)
    return fold (convert_to_real (type, e));

  /* New C++ semantics:  since assignment is now based on
     memberwise copying,  if the rhs type is derived from the
     lhs type, then we may still do a conversion.  */
  if (IS_AGGR_TYPE_CODE (code))
    {
      tree dtype = TREE_TYPE (e);

      if (TREE_CODE (dtype) == REFERENCE_TYPE)
	{
	  e = convert_from_reference (e);
	  dtype = TREE_TYPE (e);
	}
      dtype = TYPE_MAIN_VARIANT (dtype);

      /* Conversion between aggregate types.  New C++ semantics allow
	 objects of derived type to be cast to objects of base type.
	 Old semantics only allowed this bwteen pointers.

	 There may be some ambiguity between using a constructor
	 vs. using a type conversion operator when both apply.  */

      if (IS_AGGR_TYPE (dtype))
	{
	  tree basetype;

	  tree conversion = TYPE_HAS_CONVERSION (dtype)
	    ? build_type_conversion (CONVERT_EXPR, type, e, 1) : NULL_TREE;

	  if (TYPE_HAS_CONSTRUCTOR (type))
	    {
	      tree rval = build_method_call (NULL_TREE, DECL_NAME (TYPE_NAME (type)), build_tree_list (NULL_TREE, e), CLASSTYPE_AS_LIST (type),
					     conversion ? LOOKUP_NO_CONVERSION : 0);

	      if (rval != error_mark_node)
		{
		  if (conversion)
		    {
		      error ("both constructor and type conversion operator apply");
		      return error_mark_node;
		    }
		  /* call to constructor successful.  */
		  rval = build_cplus_new (type, rval);
		  return rval;
		}
	    }
	  /* Type conversion successful/applies.  */
	  if (conversion)
	    {
	      if (conversion == error_mark_node)
		error ("ambiguous pointer conversion");
	      return conversion;
	    }

	  /* now try normal C++ assignment semantics.  */
	  basetype = dtype;
	  if (type == basetype
	      || (basetype = get_base_type (type, dtype, 1)))
	    {
	      if (basetype == error_mark_node)
		return error_mark_node;

#if 0
	      if (TYPE_VIRTUAL_P (type))
		warning ("assignment to virtual aggregate type");
#endif
	      return build (COMPONENT_REF, type, e, TYPE_NAME (basetype));
	    }
	  error ("conversion between incompatible aggregate types requested");
	  return error_mark_node;
	}
      /* conversion from non-aggregate to aggregate type requires constructor.  */
      else if (TYPE_HAS_CONSTRUCTOR (type))
	{
	  tree rval;
	  tree init = build_method_call (NULL_TREE, DECL_NAME (TYPE_NAME (type)), build_tree_list (NULL_TREE, e), CLASSTYPE_AS_LIST (type), LOOKUP_NORMAL);
	  if (init == error_mark_node)
	    {
	      error_with_aggr_type (type, "in conversion to type `%s'");
	      return error_mark_node;
	    }
	  rval = build_cplus_new (type, init);
	  return rval;
	}
    }

  error ("conversion to non-scalar type requested");
  return error_mark_node;
}

/* Like convert, except permit conversions to take place which
   are not normally allowed due to visibility restrictions
   (such as conversion from sub-type to private super-type).  */
tree
convert_force (type, expr)
     tree type;
     tree expr;
{
  register tree e = expr;
  register enum tree_code code = TREE_CODE (type);

  if (code == REFERENCE_TYPE)
    return fold (convert_to_reference (0, type, e, -1, 0));
  else if (TREE_CODE (TREE_TYPE (e)) == REFERENCE_TYPE)
    e = convert_from_reference (e);

  if (code == POINTER_TYPE)
    return fold (convert_to_pointer_force (type, e));
  return convert (type, e);
}

/* Subroutine of build_type_conversion.  */
static tree
build_type_conversion_1 (xtype, basetype, expr, typename, for_sure)
     tree xtype, basetype;
     tree expr;
     tree typename;
     int for_sure;
{
  tree first_arg = expr;
  tree rval;
  int flags;

  if (for_sure == 0)
    {
      if (! lvalue_p (expr))
	first_arg = build1 (NOP_EXPR, TYPE_POINTER_TO (basetype), integer_zero_node);
      flags = LOOKUP_PROTECT;
    }
  else
    flags = LOOKUP_NORMAL;

  rval = build_method_call (first_arg, typename, NULL_TREE, NULL_TREE, flags);
  if (rval == error_mark_node)
    {
      if (for_sure == 0)
	return NULL_TREE;
      return error_mark_node;
    }
  if (first_arg != expr)
    {
      expr = build_up_reference (build_reference_type (TREE_TYPE (expr)), expr, 0);
      TREE_VALUE (TREE_OPERAND (rval, 1)) = build_unary_op (ADDR_EXPR, expr, 0);
    }
  if (TREE_CODE (TREE_TYPE (rval)) == REFERENCE_TYPE
      && TREE_CODE (xtype) != REFERENCE_TYPE)
    rval = default_conversion (rval);
  return convert (xtype, rval);
}

/* Convert an aggregate EXPR to type XTYPE.  If a conversion
   exists, return the attempted conversion.  This may
   return ERROR_MARK_NODE if the conversion is not
   allowed (references private members, etc).
   If no conversion exists, NULL_TREE is returned.

   If (FOR_SURE & 1) is non-zero, then we allow this type conversion
   to take place immediately.  Otherwise, we build a SAVE_EXPR
   which can be evaluated if the results are ever needed.

   If FOR_SURE >= 2, then we only look for exact conversions.

   TYPE may be a reference type, in which case we first look
   for something that will convert to a reference type.  If
   that fails, we will try to look for something of the
   reference's target type, and then return a reference to that.  */
tree
build_type_conversion (code, xtype, expr, for_sure)
     enum tree_code code;
     tree xtype, expr;
     int for_sure;
{
  /* C++: check to see if we can convert this aggregate type
     into the required scalar type.  */
  tree type, type_default;
  tree typename = build_typename_overload (xtype), *typenames;
  int n_variants = 0;
  tree basetype, save_basetype;
  tree rval;
  int exact_conversion = for_sure >= 2;
  for_sure &= 1;

  if (expr == error_mark_node)
    return error_mark_node;

  basetype = TREE_TYPE (expr);
  if (TREE_CODE (basetype) == REFERENCE_TYPE)
    basetype = TREE_TYPE (basetype);

  basetype = TYPE_MAIN_VARIANT (basetype);
  if (! TYPE_LANG_SPECIFIC (basetype) || ! TYPE_HAS_CONVERSION (basetype))
    return 0;

  if (TREE_CODE (xtype) == POINTER_TYPE
      || TREE_CODE (xtype) == REFERENCE_TYPE)
    {
      /* Prepare to match a variant of this type.  */
      type = TYPE_MAIN_VARIANT (TREE_TYPE (xtype));
      for (n_variants = 0; type; type = TYPE_NEXT_VARIANT (type))
	n_variants++;
      typenames = (tree *)alloca (n_variants * sizeof (tree));
      for (n_variants = 0, type = TYPE_MAIN_VARIANT (TREE_TYPE (xtype));
	   type; n_variants++, type = TYPE_NEXT_VARIANT (type))
	{
	  if (type == TREE_TYPE (xtype))
	    typenames[n_variants] = typename;
	  else if (TREE_CODE (xtype) == POINTER_TYPE)
	    typenames[n_variants] = build_typename_overload (build_pointer_type (type));
	  else
	    typenames[n_variants] = build_typename_overload (build_reference_type (type));
	}
    }

  save_basetype = basetype;
  type = xtype;

  while (TYPE_HAS_CONVERSION (basetype))
    {
      int i;
      if (lookup_fnfields (CLASSTYPE_AS_LIST (basetype), typename, 0))
	return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
      for (i = 0; i < n_variants; i++)
	if (typenames[i] != typename
	    && lookup_fnfields (CLASSTYPE_AS_LIST (basetype), typenames[i], 0))
	  return build_type_conversion_1 (xtype, basetype, expr, typenames[i], for_sure);

      if (CLASSTYPE_N_BASECLASSES (basetype))
	basetype = CLASSTYPE_BASECLASS (basetype, 1);
      else break;
    }

  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      tree first_arg = expr;
      type = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      basetype = save_basetype;

      /* May need to build a temporary for this.  */
      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (lookup_fnfields (CLASSTYPE_AS_LIST (basetype), typename, 0))
	    {
	      int flags;

	      if (for_sure == 0)
		{
		  if (! lvalue_p (expr))
		    first_arg = build1 (NOP_EXPR, TYPE_POINTER_TO (basetype), integer_zero_node);
		  flags = LOOKUP_PROTECT;
		}
	      else
		flags = LOOKUP_NORMAL;
	      rval = build_method_call (first_arg, typename, NULL_TREE, NULL_TREE, flags);
	      if (rval == error_mark_node)
		{
		  if (for_sure == 0)
		    return NULL_TREE;
		  return error_mark_node;
		}
	      TREE_VALUE (TREE_OPERAND (rval, 1)) = expr;

	      if (IS_AGGR_TYPE (type))
		{
		  tree init = build_method_call (NULL_TREE, DECL_NAME (TYPE_NAME (type)), build_tree_list (NULL_TREE, rval), NULL_TREE, LOOKUP_NORMAL);
		  tree temp = build_cplus_new (type, init);
		  return build_up_reference (TYPE_REFERENCE_TO (type), temp, 0);
		}
	      return convert (xtype, rval);
	    }
	  if (CLASSTYPE_N_BASECLASSES (basetype))
	    basetype = CLASSTYPE_BASECLASS (basetype, 1);
	  else break;
	}
      /* No free conversions for reference types, right?.  */
      return NULL_TREE;
    }

  if (exact_conversion)
    return NULL_TREE;

  /* No perfect match found, try default.  */
  if (code == CONVERT_EXPR && TREE_CODE (type) == POINTER_TYPE)
    type_default = ptr_type_node;
  else if (type == void_type_node)
    return NULL_TREE;
  else
    {
      extern tree default_conversion ();
      tree tmp = default_conversion (build1 (NOP_EXPR, type, integer_zero_node));
      if (tmp == error_mark_node)
	return NULL_TREE;
      type_default = TREE_TYPE (tmp);
    }

  basetype = save_basetype;

  if (type_default != type)
    {
      type = type_default;
      typename = build_typename_overload (type);

      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (lookup_fnfields (CLASSTYPE_AS_LIST (basetype), typename, 0))
	    return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	  if (CLASSTYPE_N_BASECLASSES (basetype))
	    basetype = CLASSTYPE_BASECLASS (basetype, 1);
	  else break;
	}
    }

 try_pointer:

  if (type == ptr_type_node)
    {
      /* Try converting to some other pointer type
	 with which void* is compatible, or in situations
	 in which void* is appropriate (such as &&,||, and !).  */

      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (CLASSTYPE_CONVERSION (basetype, ptr_conv) != 0)
	    {
	      if (CLASSTYPE_CONVERSION (basetype, ptr_conv) == error_mark_node)
		return error_mark_node;
	      typename = DECL_ORIGINAL_NAME (CLASSTYPE_CONVERSION (basetype, ptr_conv));
	      return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	    }
	  if (CLASSTYPE_N_BASECLASSES (basetype))
	    basetype = CLASSTYPE_BASECLASS (basetype, 1);
	  else break;
	}
    }
  if (TREE_CODE (type) == POINTER_TYPE
      && TREE_READONLY (TREE_TYPE (type))
      && TYPE_MAIN_VARIANT (TREE_TYPE (type)) == void_type_node)
    {
      /* Try converting to some other pointer type
	 with which const void* is compatible.  */

      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (CLASSTYPE_CONVERSION (basetype, constptr_conv) != 0)
	    {
	      if (CLASSTYPE_CONVERSION (basetype, constptr_conv) == error_mark_node)
		return error_mark_node;
	      typename = DECL_ORIGINAL_NAME (CLASSTYPE_CONVERSION (basetype, constptr_conv));
	      return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	    }
	  if (CLASSTYPE_N_BASECLASSES (basetype))
	    basetype = CLASSTYPE_BASECLASS (basetype, 1);
	  else break;
	}
    }
  /* Use the longer or shorter conversion that is appropriate.  */
  if (TREE_CODE (type) == INTEGER_TYPE
      && TYPE_HAS_INT_CONVERSION (basetype)
      && CLASSTYPE_CONVERSION (basetype, int_conv) != error_mark_node)
    {
      typename = DECL_ORIGINAL_NAME (CLASSTYPE_CONVERSION (basetype, int_conv));
      return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
    }
  if (TREE_CODE (type) == REAL_TYPE
      && TYPE_HAS_REAL_CONVERSION (basetype)
      && CLASSTYPE_CONVERSION (basetype, real_conv) != error_mark_node)
    {
      typename = DECL_ORIGINAL_NAME (CLASSTYPE_CONVERSION (basetype, real_conv));
      return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
    }

  /* THIS IS A KLUDGE.  */
  if (TREE_CODE (type) != POINTER_TYPE
      && (code == TRUTH_ANDIF_EXPR
	  || code == TRUTH_ORIF_EXPR
	  || code == TRUTH_NOT_EXPR))
    {
      /* Here's when we can convert to a pointer.  */
      type = ptr_type_node;
      goto try_pointer;
    }

  /* THESE ARE TOTAL KLUDGES.  */
  /* Default promotion yields no new alternatives, try
     conversions which are anti-default, such as

     double -> float or int -> unsigned or unsigned -> long

     */
  if (type_default == type)
    {
      int not_again = 0;

      if (type == double_type_node)
	typename = build_typename_overload (float_type_node);
      else if (type == integer_type_node)
	typename = build_typename_overload (unsigned_type_node);
      else if (type == unsigned_type_node)
	typename = build_typename_overload (long_integer_type_node);

    again:
      basetype = save_basetype;
      while (TYPE_HAS_CONVERSION (basetype))
	{
	  if (lookup_fnfields (CLASSTYPE_AS_LIST (basetype), typename, 0))
	    return build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	  if (CLASSTYPE_N_BASECLASSES (basetype))
	    basetype = CLASSTYPE_BASECLASS (basetype, 1);
	  else break;
	}
      if (! not_again && type == integer_type_node)
	{
	  typename = build_typename_overload (long_integer_type_node);
	  not_again = 1;
	  goto again;
	}
    }

  /* Now, try C promotions...

     float -> int
     int -> float, void *
     void * -> int

     Truthvalue conversions let us try to convert
     to pointer if we were going for int, and to int
     if we were looking for pointer.  */

    basetype = save_basetype;
    if (TREE_CODE (type) == REAL_TYPE
	|| (TREE_CODE (type) == POINTER_TYPE
	    && (code == TRUTH_ANDIF_EXPR
		|| code == TRUTH_ORIF_EXPR
		|| code == TRUTH_NOT_EXPR)))
      type = integer_type_node;
    else if (TREE_CODE (type) == INTEGER_TYPE)
      if (TYPE_HAS_REAL_CONVERSION (basetype))
	type = double_type_node;
      else
	return NULL_TREE;
    else
      return NULL_TREE;

    typename = build_typename_overload (type);
    while (TYPE_HAS_CONVERSION (basetype))
      {
	if (lookup_fnfields (CLASSTYPE_AS_LIST (basetype), typename, 0))
	  {
	    rval = build_type_conversion_1 (xtype, basetype, expr, typename, for_sure);
	    return rval;
	  }
	if (CLASSTYPE_N_BASECLASSES (basetype))
	  basetype = CLASSTYPE_BASECLASS (basetype, 1);
	else
	  break;
      }

  return NULL_TREE;
}

/* Must convert two aggregate types to non-aggregate type.
   Attempts to find a non-ambiguous, "best" type conversion.

   Return 1 on success, 0 on failure.

   @@ What are the real semantics of this supposed to be??? */
int
build_default_binary_type_conversion (code, arg1, arg2)
     enum tree_code code;
     tree *arg1, *arg2;
{
  tree type1 = TREE_TYPE (*arg1);
  tree type2 = TREE_TYPE (*arg2);
  char *name1, *name2;

  if (TREE_CODE (type1) == REFERENCE_TYPE)
    type1 = TREE_TYPE (type1);
  if (TREE_CODE (type2) == REFERENCE_TYPE)
    type2 = TREE_TYPE (type2);

  if (TREE_CODE (TYPE_NAME (type1)) != TYPE_DECL)
    {
      tree decl = typedecl_for_tag (type1);
      if (decl)
	error ("type conversion nonexistant for type `%s'",
	       IDENTIFIER_POINTER (DECL_NAME (decl)));
      else
	error ("type conversion nonexistant for non-C++ type");
      return 0;
    }
  if (TREE_CODE (TYPE_NAME (type2)) != TYPE_DECL)
    {
      tree decl = typedecl_for_tag (type2);
      if (decl)
	error ("type conversion nonexistant for type `%s'",
	       IDENTIFIER_POINTER (decl));
      else
	error ("type conversion nonexistant for non-C++ type");
      return 0;
    }

  name1 = TYPE_NAME_STRING (type1);
  name2 = TYPE_NAME_STRING (type2);

  if (! TYPE_HAS_CONVERSION (type1))
    {
      if (! TYPE_HAS_CONVERSION (type2))
	error ("type conversion required for binary operation on types `%s' and `%s'",
	       name1, name2);
      else
	error ("type conversion required for type `%s'", name1);
      return 0;
    }
  else if (! TYPE_HAS_CONVERSION (type2))
    {
      error ("type conversion required for type `%s'", name2);
      return 0;
    }

  if (TYPE_HAS_INT_CONVERSION (type1) && TYPE_HAS_REAL_CONVERSION (type1))
    warning ("ambiguous type conversion for type `%s', defaulting to int", name1);
  if (TYPE_HAS_INT_CONVERSION (type1))
    {
      *arg1 = build_type_conversion (code, integer_type_node, *arg1, 1);
      *arg2 = build_type_conversion (code, integer_type_node, *arg2, 1);
    }
  else if (TYPE_HAS_REAL_CONVERSION (type1))
    {
      *arg1 = build_type_conversion (code, double_type_node, *arg1, 1);
      *arg2 = build_type_conversion (code, double_type_node, *arg2, 1);
    }
  else
    {
      *arg1 = build_type_conversion (code, ptr_type_node, *arg1, 1);
      if (*arg1 == error_mark_node)
	error ("ambiguous pointer conversion");
      *arg2 = build_type_conversion (code, ptr_type_node, *arg2, 1);
      if (*arg1 != error_mark_node && *arg2 == error_mark_node)
	error ("ambiguous pointer conversion");
    }
  if (*arg1 == 0)
    {
      if (*arg2 == 0 && type1 != type2)
	error ("default type conversion for types `%s' and `%s' failed",
	       name1, name2);
      else
	error ("default type conversion for type `%s' failed", name1);
      return 0;
    }
  else if (*arg2 == 0)
    {
      error ("default type conversion for type `%s' failed", name2);
      return 0;
    }
  return 1;
}

/* Must convert two aggregate types to non-aggregate type.
   Attempts to find a non-ambiguous, "best" type conversion.

   Return 1 on success, 0 on failure.

   The type of the argument is expected to be of aggregate type here.

   @@ What are the real semantics of this supposed to be??? */
int
build_default_unary_type_conversion (code, arg)
     enum tree_code code;
     tree *arg;
{
  tree type = TREE_TYPE (*arg);
  tree id = TREE_CODE (TYPE_NAME (type)) == TYPE_DECL
    ? DECL_NAME (TYPE_NAME (type)) : TYPE_NAME (type);
  char *name = IDENTIFIER_POINTER (id);

  if (! TYPE_HAS_CONVERSION (type))
    {
      error ("type conversion required for type `%s'", name);
      return 0;
    }

  if (TYPE_HAS_INT_CONVERSION (type) && TYPE_HAS_REAL_CONVERSION (type))
    warning ("ambiguous type conversion for type `%s', defaulting to int", name);
  if (TYPE_HAS_INT_CONVERSION (type))
    *arg = build_type_conversion (code, integer_type_node, *arg, 1);
  else if (TYPE_HAS_REAL_CONVERSION (type))
    *arg = build_type_conversion (code, double_type_node, *arg, 1);
  else
    {
      *arg = build_type_conversion (code, ptr_type_node, *arg, 1);
      if (*arg == error_mark_node)
	error ("ambiguous pointer conversion");
    }
  if (*arg == 0)
    {
      error ("default type conversion for type `%s' failed", name);
      return 0;
    }
  return 1;
}
