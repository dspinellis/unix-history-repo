/* Report error messages, build initializers, and perform
   some front-end optimizations for C++ compiler.
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

#include "config.h"
#include <stdio.h>
#include "tree.h"
#include "cplus-tree.h"
#include "flags.h"
#include "rtl.h"
#include "assert.h"

static tree process_init_constructor ();
tree digest_init ();
void incomplete_type_error ();
void readonly_warning_or_error ();
extern tree convert_for_initialization ();

/* Print an error message stemming from an attempt to use
   BASETYPE as a base class for TYPE.  */
void
error_not_base_type (basetype, type)
     tree basetype, type;
{
  tree name1 = TYPE_NAME (basetype);
  tree name2 = TYPE_NAME (type);
  if (TREE_CODE (name1) == TYPE_DECL)
    name1 = DECL_NAME (name1);
  if (TREE_CODE (name2) == TYPE_DECL)
    name2 = DECL_NAME (name2);
  error ("type `%s' is not a base type for type `%s'",
	 IDENTIFIER_POINTER (name1), IDENTIFIER_POINTER (name2));
}

tree
basetype_or_else (parent_or_type, type)
     tree parent_or_type, type;
{
  tree basetype;
  if (TYPE_MAIN_VARIANT (parent_or_type) == TYPE_MAIN_VARIANT (type))
    return parent_or_type;
  if (basetype = get_base_type (parent_or_type, TYPE_MAIN_VARIANT (type), 0))
    {
      if (basetype == error_mark_node)
	return NULL_TREE;
      return basetype;
    }
  error_not_base_type (parent_or_type, type);
  return NULL_TREE;
}

/* Print an error message stemming from an invalid use of an
   aggregate type.

   TYPE is the type which draws the error.
   MSG is the message to print.
   ARG is an optional argument which may provide more information.  */
void
error_with_aggr_type (type, msg, arg)
     tree type;
     char *msg;
     int arg;
{
  tree name = TYPE_NAME (type);
  if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);
  error (msg, IDENTIFIER_POINTER (name), arg);
}

/* Warn or give error about storing in something that is `const'.  */

void
readonly_warning_or_error (arg, string)
     tree arg;
     char *string;
{
  char buf[80];
  strcpy (buf, string);

  if (TREE_CODE (arg) == COMPONENT_REF)
    {
      if (TREE_READONLY (TREE_OPERAND (arg, 0)))
        strcat (buf, " of member `%s' in read-only structure");
      else
        strcat (buf, " of read-only member `%s'");
      error (buf, lang_printable_name (TREE_OPERAND (arg, 1)));
    }
  else if (TREE_CODE (arg) == VAR_DECL)
    {
      if (DECL_LANG_SPECIFIC (arg)
	  && DECL_IN_AGGR_P (arg)
	  && !TREE_STATIC (arg))
	strcat (buf, " of constant field `%s'");
      else
	strcat (buf, " of read-only variable `%s'");
      error (buf, lang_printable_name (arg));
    }
  else if (TREE_CODE (arg) == PARM_DECL)
    {
      strcat (buf, " of read-only parameter `%s'");
      error (buf, lang_printable_name (arg));
    }
  else if (TREE_CODE (arg) == INDIRECT_REF
           && TREE_CODE (TREE_TYPE (TREE_OPERAND (arg, 0))) == REFERENCE_TYPE
           && (TREE_CODE (TREE_OPERAND (arg, 0)) == VAR_DECL
               || TREE_CODE (TREE_OPERAND (arg, 0)) == PARM_DECL))
    {
      strcat (buf, " of read-only reference `%s'");
      error (buf, lang_printable_name (TREE_OPERAND (arg, 0)));
    }
  else	       
    {
      warning ("%s of read-only location", buf);
    }
}

/* Print an error message for invalid use of a type which declares
   virtual functions which are not inheritable.  */
void
abstract_virtuals_error (decl, type)
     tree decl;
     tree type;
{
  char *typename = TYPE_NAME_STRING (type);
  tree u = CLASSTYPE_ABSTRACT_VIRTUALS (type);

  if (decl)
    {
      if (TREE_CODE (decl) == RESULT_DECL)
	return;

      if (TREE_CODE (decl) == VAR_DECL)
	error_with_decl (decl, "cannot declare variable `%s' to be of type `%s'", typename);
      else if (TREE_CODE (decl) == PARM_DECL)
	error_with_decl (decl, "cannot declare parameter `%s' to be of type `%s'", typename);
      else if (TREE_CODE (decl) == FIELD_DECL)
	error_with_decl (decl, "cannot declare field `%s' to be of type `%s'", typename);
      else if (TREE_CODE (decl) == FUNCTION_DECL)
	error_with_decl (decl, "invalid return type for function `%s'");
    }
  else error ("cannot allocate an object of type `%s'", typename);
  /* Only go through this once.  */
  if (TREE_PURPOSE (u) == NULL_TREE)
    {
      error ("since the following virtual functions are abstract:");
      TREE_PURPOSE (u) = error_mark_node;
      while (u)
	{
	  error_with_decl (TREE_VALUE (u), "%s");
	  u = TREE_CHAIN (u);
	}
    }
  else error ("since type `%s' has abstract virtual functions", typename);
}

/* Print an error message for invalid use of an incomplete type.
   VALUE is the expression that was used (or 0 if that isn't known)
   and TYPE is the type that was invalid.  */

void
incomplete_type_error (value, type)
     tree value;
     tree type;
{
  char *errmsg;

  /* Avoid duplicate error message.  */
  if (TREE_CODE (type) == ERROR_MARK)
    return;

  if (value != 0 && (TREE_CODE (value) == VAR_DECL
		     || TREE_CODE (value) == PARM_DECL))
    error ("`%s' has an incomplete type",
	   IDENTIFIER_POINTER (DECL_NAME (value)));
  else
    {
    retry:
      /* We must print an error message.  Be clever about what it says.  */

      switch (TREE_CODE (type))
	{
	case RECORD_TYPE:
	  errmsg = "invalid use of undefined type `struct %s'";
	  break;

	case UNION_TYPE:
	  errmsg = "invalid use of undefined type `union %s'";
	  break;

	case ENUMERAL_TYPE:
	  errmsg = "invalid use of undefined type `enum %s'";
	  break;

	case VOID_TYPE:
	  error ("invalid use of void expression");
	  return;

	case ARRAY_TYPE:
	  if (TYPE_DOMAIN (type))
	    {
	      type = TREE_TYPE (type);
	      goto retry;
	    }
	  error ("invalid use of array with unspecified bounds");
	  return;

	case OFFSET_TYPE:
	  error ("invalid use of member type (did you forget the `&' ?)");
	  return;

	default:
	  abort ();
	}

      error_with_aggr_type (type, errmsg);
    }
}

/* Return nonzero if VALUE is a valid constant-valued expression
   for use in initializing a static variable; one that can be an
   element of a "constant" initializer.

   Return 1 if the value is absolute; return 2 if it is relocatable.
   We assume that VALUE has been folded as much as possible;
   therefore, we do not need to check for such things as
   arithmetic-combinations of integers.  */

static int
initializer_constant_valid_p (value)
     tree value;
{
  switch (TREE_CODE (value))
    {
    case CONSTRUCTOR:
      return TREE_STATIC (value);

    case INTEGER_CST:
    case REAL_CST:
    case STRING_CST:
      return 1;

    case ADDR_EXPR:
      return 2;

    case CONVERT_EXPR:
    case NOP_EXPR:
      /* Allow conversions between types of the same kind.  */
      if (TREE_CODE (TREE_TYPE (value))
	  == TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))))
	return initializer_constant_valid_p (TREE_OPERAND (value, 0));
      /* Allow (int) &foo.  */
      if (TREE_CODE (TREE_TYPE (value)) == INTEGER_TYPE
	  && TREE_CODE (TREE_TYPE (TREE_OPERAND (value, 0))) == POINTER_TYPE)
	return initializer_constant_valid_p (TREE_OPERAND (value, 0));
      return 0;

    case PLUS_EXPR:
      {
	int valid0 = initializer_constant_valid_p (TREE_OPERAND (value, 0));
	int valid1 = initializer_constant_valid_p (TREE_OPERAND (value, 1));
	if (valid0 == 1 && valid1 == 2)
	  return 2;
	if (valid0 == 2 && valid1 == 1)
	  return 2;
	return 0;
      }

    case MINUS_EXPR:
      {
	int valid0 = initializer_constant_valid_p (TREE_OPERAND (value, 0));
	int valid1 = initializer_constant_valid_p (TREE_OPERAND (value, 1));
	if (valid0 == 2 && valid1 == 1)
	  return 2;
	return 0;
      }
    }

  return 0;
}

/* Perform appropriate conversions on the initial value of a variable,
   store it in the declaration DECL,
   and print any error messages that are appropriate.
   If the init is invalid, store an ERROR_MARK.

   C++: Note that INIT might be a TREE_LIST, which would mean that it is
   a base class initializer for some aggregate type, hopefully compatible
   with DECL.  If INIT is a single element, and DECL is an aggregate
   type, we silently convert INIT into a TREE_LIST, allowing a constructor
   to be called.

   If INIT is a TREE_LIST and there is no constructor, turn INIT
   into a CONSTRUCTOR and use standard initialization techniques.
   Perhaps a warning should be generated?

   Returns value of initializer if initialization could not be
   performed for static variable.  In that case, caller must do
   the storing.  */

tree
store_init_value (decl, init)
     tree decl, init;
{
  register tree value, type;

  /* If variable's type was invalidly declared, just ignore it.  */

  type = TREE_TYPE (decl);
  if (TREE_CODE (type) == ERROR_MARK)
    return NULL_TREE;

  /* Take care of C++ business up here.  */
  type = TYPE_MAIN_VARIANT (type);

  /* implicitly tests if IS_AGGR_TYPE.  */
  if (TYPE_NEEDS_CONSTRUCTING (type))
    abort ();
  else if (IS_AGGR_TYPE (type))
    {
      /* @@ This may be wrong, but I do not know what is right.  */
      if (TREE_CODE (init) == TREE_LIST)
	{
	  error_with_aggr_type (type, "constructor syntax used, but no constructor declared for type `%s'");
	  init = build_nt (CONSTRUCTOR, NULL_TREE, nreverse (init));
	}
    }
  else if (TREE_CODE (init) == TREE_LIST
	   && TREE_TYPE (init) != unknown_type_node)
    {
      if (TREE_CODE (decl) == RESULT_DECL)
	{
	  if (TREE_CHAIN (init))
	    {
	      warning ("comma expression used to initialize return value");
	      init = build_compound_expr (init);
	    }
	  else
	    init = TREE_VALUE (init);
	}
      else if (TREE_TYPE (init) != 0
	       && TREE_CODE (TREE_TYPE (init)) == OFFSET_TYPE)
	{
	  /* Use the type of our variable to instantiate
	     the type of our initializer.  */
	  init = instantiate_type (type, init, 1);
	}
      else abort ();
    }

  /* End of special C++ code.  */

  /* Digest the specified initializer into an expression.  */

  value = digest_init (type, init, 0);

  /* Store the expression if valid; else report error.  */

  if (TREE_CODE (value) == ERROR_MARK)
    ;
  else if (TREE_STATIC (decl)
	   && (! TREE_LITERAL (value)
	       || ! initializer_constant_valid_p (value)))
    return value;
  else
    {
      if (pedantic && TREE_CODE (value) == CONSTRUCTOR)
	{
	  if (! TREE_LITERAL (value))
	    warning ("aggregate initializer is not constant");
	  else if (! TREE_STATIC (value))
	    warning ("aggregate initializer uses complicated arithmetic");
	}
    }
  DECL_INITIAL (decl) = value;
  return NULL_TREE;
}

/* Digest the parser output INIT as an initializer for type TYPE.
   Return a C expression of type TYPE to represent the initial value.

   If TAIL is nonzero, it points to a variable holding a list of elements
   of which INIT is the first.  We update the list stored there by
   removing from the head all the elements that we use.
   Normally this is only one; we use more than one element only if
   TYPE is an aggregate and INIT is not a constructor.  */

tree
digest_init (type, init, tail)
     tree type, init, *tail;
{
  enum tree_code code = TREE_CODE (type);
  tree element = 0;
  tree old_tail_contents;
  /* Nonzero if INIT is a braced grouping, which comes in as a CONSTRUCTOR
     tree node which has no TREE_TYPE.  */
  int raw_constructor
    = TREE_CODE (init) == CONSTRUCTOR && TREE_TYPE (init) == 0;

  /* By default, assume we use one element from a list.
     We correct this later in the sole case where it is not true.  */

  if (tail)
    {
      old_tail_contents = *tail;
      *tail = TREE_CHAIN (*tail);
    }

  if (init == error_mark_node)
    return error_mark_node;

  if (init && raw_constructor
      && CONSTRUCTOR_ELTS (init) != 0
      && TREE_CHAIN (CONSTRUCTOR_ELTS (init)) == 0)
    {
      element = TREE_VALUE (CONSTRUCTOR_ELTS (init));
      if (element == error_mark_node)
	return error_mark_node;
    }

  /* Any type can be initialized from an expression of the same type,
     optionally with braces.  */

  if (init && TREE_TYPE (init)
      && (TYPE_MAIN_VARIANT (TREE_TYPE (init)) == type
	  || (code == ARRAY_TYPE && comptypes (TREE_TYPE (init), type, 1))))
    {
      if (pedantic && code == ARRAY_TYPE
	  && TREE_CODE (init) != STRING_CST)
	warning ("ANSI C forbids initializing array from array expression");
      if (TREE_CODE (init) == CONST_DECL)
	init = DECL_INITIAL (init);
      else if (TREE_READONLY_DECL_P (init))
	init = decl_constant_value (init);
      return init;
    }

  if (element && (TREE_TYPE (element) == type
		  || (code == ARRAY_TYPE && TREE_TYPE (element)
		      && comptypes (TREE_TYPE (element), type, 1))))
    {
      if (pedantic && code == ARRAY_TYPE)
	warning ("ANSI C forbids initializing array from array expression");
      if (pedantic && (code == RECORD_TYPE || code == UNION_TYPE))
	warning ("single-expression nonscalar initializer has braces");
      if (TREE_CODE (element) == CONST_DECL)
	element = DECL_INITIAL (element);
      else if (TREE_READONLY_DECL_P (element))
	element = decl_constant_value (element);
      return element;
    }

  /* Check for initializing a union by its first field.
     Such an initializer must use braces.  */

  if (code == UNION_TYPE)
    {
      tree result;

      if (TYPE_FIELDS (type) == 0)
	{
	  error ("union with no members cannot be initialized");
	  return error_mark_node;
	}
      if (! raw_constructor)
	{
	  error ("type mismatch in initialization");
	  return error_mark_node;
	}
      if (element == 0)
	{
	  if (!TYPE_NEEDS_CONSTRUCTING (type))
	    {
	      error ("union initializer requires one element");
	      return error_mark_node;
	    }
	}
      else
	{
	  /* Take just the first element from within the constructor
	     and it should match the type of the first element.  */
	  element = digest_init (TREE_TYPE (TYPE_FIELDS (type)), element, 0);
	  result = build (CONSTRUCTOR, type, 0, build_tree_list (0, element));
	  TREE_LITERAL (result) = TREE_LITERAL (element);
	  TREE_STATIC (result) = (initializer_constant_valid_p (element)
				  && TREE_LITERAL (element));
	  return result;
	}
    }

  /* Initialization of an array of chars from a string constant
     optionally enclosed in braces.  */

  if (code == ARRAY_TYPE)
    {
      tree typ1 = TYPE_MAIN_VARIANT (TREE_TYPE (type));
      if ((typ1 == char_type_node
	   || typ1 == signed_char_type_node
	   || typ1 == unsigned_char_type_node
	   || typ1 == unsigned_type_node
	   || typ1 == integer_type_node)
	  && ((init && TREE_CODE (init) == STRING_CST)
	      || (element && TREE_CODE (element) == STRING_CST)))
	{
	  tree string = element ? element : init;

	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (string)))
	       != char_type_node)
	      && TYPE_PRECISION (typ1) == BITS_PER_UNIT)
	    {
	      error ("char-array initialized from wide string");
	      return error_mark_node;
	    }
	  if ((TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (string)))
	       == char_type_node)
	      && TYPE_PRECISION (typ1) != BITS_PER_UNIT)
	    {
	      error ("int-array initialized from non-wide string");
	      return error_mark_node;
	    }

	  if (pedantic && typ1 != char_type_node)
	    warning ("ANSI C forbids string initializer except for `char' elements");
	  TREE_TYPE (string) = type;
	  if (TYPE_DOMAIN (type) != 0
	      && TREE_LITERAL (TYPE_SIZE (type)))
	    {
	      register int size
		= TREE_INT_CST_LOW (TYPE_SIZE (type)) * TYPE_SIZE_UNIT (type);
	      size = (size + BITS_PER_UNIT - 1) / BITS_PER_UNIT;
	      /* Subtract 1 because it's ok to ignore the terminating null char
		 that is counted in the length of the constant.  */
	      if (size < TREE_STRING_LENGTH (string) - 1)
		warning ("initializer-string for array of chars is too long");
	    }
	  return string;
	}
    }

  /* Handle scalar types, including conversions.  */

  if (code == INTEGER_TYPE || code == REAL_TYPE || code == POINTER_TYPE
      || code == ENUMERAL_TYPE || code == REFERENCE_TYPE)
    {
      if (raw_constructor)
	{
	  if (element == 0)
	    {
	      error ("initializer for scalar variable requires one element");
	      return error_mark_node;
	    }
	  init = element;
	}

      return convert_for_initialization (0, type, init, "initialization", LOOKUP_NORMAL);
    }

  /* Come here only for records and arrays (and unions with constructors).  */

  if (TYPE_SIZE (type) && ! TREE_LITERAL (TYPE_SIZE (type)))
    {
      error ("variable-sized object may not be initialized");
      return error_mark_node;
    }

  if (code == ARRAY_TYPE || code == RECORD_TYPE || code == UNION_TYPE)
    {
      if (raw_constructor)
	return process_init_constructor (type, init, 0);
      else if (TYPE_NEEDS_CONSTRUCTING (type))
	{
	  /* This can only be reached when caller is initializing
	     ARRAY_TYPE.  In that case, we don't want to convert
	     INIT to TYPE.  We will let `expand_vec_init' do it.  */
	  return init;
	}
      else if (tail != 0)
	{
	  *tail = old_tail_contents;
	  return process_init_constructor (type, 0, tail);
	}
      else if (flag_traditional)
	/* Traditionally one can say `char x[100] = 0;'.  */
	return process_init_constructor (type,
					 build_nt (CONSTRUCTOR, 0,
						   tree_cons (0, init, 0)),
					 0);
      if (code != ARRAY_TYPE)
	return convert_for_initialization (0, type, init, "initialization", LOOKUP_NORMAL);
    }

  error ("invalid initializer");
  return error_mark_node;
}

/* Process a constructor for a variable of type TYPE.
   The constructor elements may be specified either with INIT or with ELTS,
   only one of which should be non-null.

   If INIT is specified, it is a CONSTRUCTOR node which is specifically
   and solely for initializing this datum.

   If ELTS is specified, it is the address of a variable containing
   a list of expressions.  We take as many elements as we need
   from the head of the list and update the list.

   In the resulting constructor, TREE_LITERAL is set if all elts are
   constant, and TREE_STATIC is set if, in addition, all elts are simple enough
   constants that the assembler and linker can compute them.  */

static tree
process_init_constructor (type, init, elts)
     tree type, init, *elts;
{
  extern tree empty_init_node;
  register tree tail;
  /* List of the elements of the result constructor,
     in reverse order.  */
  register tree members = NULL;
  tree result;
  int allconstant = 1;
  int allsimple = 1;
  int erred = 0;

  /* Make TAIL be the list of elements to use for the initialization,
     no matter how the data was given to us.  */

  if (elts)
    tail = *elts;
  else
    tail = CONSTRUCTOR_ELTS (init);

  /* Gobble as many elements as needed, and make a constructor or initial value
     for each element of this aggregate.  Chain them together in result.
     If there are too few, use 0 for each scalar ultimate component.  */

  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      tree domain = TYPE_DOMAIN (type);
      register long len;
      register int i;

      if (domain)
	len = TREE_INT_CST_LOW (TYPE_MAX_VALUE (domain))
	  - TREE_INT_CST_LOW (TYPE_MIN_VALUE (domain))
	    + 1;
      else
	len = -1;  /* Take as many as there are */

      for (i = 0; (len < 0 || i < len) && tail != 0; i++)
	{
	  register tree next1;

	  if (TREE_VALUE (tail) != 0)
	    {
	      tree tail1 = tail;
	      next1 = digest_init (TYPE_MAIN_VARIANT (TREE_TYPE (type)),
				   TREE_VALUE (tail), &tail1);
	      assert (tail1 == 0 || TREE_CODE (tail1) == TREE_LIST);
	      tail = tail1;
	    }
	  else
	    {
	      next1 = error_mark_node;
	      tail = TREE_CHAIN (tail);
	    }

	  if (next1 == error_mark_node)
	    erred = 1;
	  else if (!TREE_LITERAL (next1))
	    allconstant = 0;
	  else if (! initializer_constant_valid_p (next1))
	    allsimple = 0;
	  members = tree_cons (NULL_TREE, next1, members);
	}
    }
  if (TREE_CODE (type) == RECORD_TYPE && init != empty_init_node)
    {
      register tree field;

      if (tail)
	{
	  if (TYPE_USES_VIRTUAL_BASECLASSES (type))
	    {
	      sorry ("initializer list for object of class with virtual baseclasses");
	      return error_mark_node;
	    }

	  if (CLASSTYPE_N_BASECLASSES (type) > 0)
	    {
	      sorry ("initializer list for object of class with baseclasses");
	      return error_mark_node;
	    }

	  if (TYPE_VIRTUAL_P (type))
	    {
	      sorry ("initializer list for object using virtual functions");
	      return error_mark_node;
	    }
	}

      for (field = TYPE_FIELDS (type); field && tail;
	   field = TREE_CHAIN (field))
	{
	  register tree next1;

	  if (! DECL_NAME (field))
	    {
	      members = tree_cons (field, integer_zero_node, members);
	      continue;
	    }

	  if (TREE_CODE (field) == CONST_DECL || TREE_CODE (field) == TYPE_DECL)
	    continue;
	  if (TREE_CODE (field) == VAR_DECL && !TREE_STATIC (field))
	    continue;

	  if (TREE_VALUE (tail) != 0)
	    {
	      tree tail1 = tail;
	      next1 = digest_init (TREE_TYPE (field),
				   TREE_VALUE (tail), &tail1);
	      assert (tail1 == 0 || TREE_CODE (tail1) == TREE_LIST);
	      if (TREE_CODE (field) == VAR_DECL
		  && ! global_bindings_p ())
		warning_with_decl (field, "initialization of static member `%s'");
	      tail = tail1;
	    }
	  else
	    {
	      next1 = error_mark_node;
	      tail = TREE_CHAIN (tail);
	    }

	  if (next1 == error_mark_node)
	    erred = 1;
	  else if (!TREE_LITERAL (next1))
	    allconstant = 0;
	  else if (! initializer_constant_valid_p (next1))
	    allsimple = 0;
	  members = tree_cons (field, next1, members);
	}
      for (; field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  /* Does this field have a default initializtion?  */
	  if (DECL_INITIAL (field))
	    {
	      register tree next1 = DECL_INITIAL (field);
	      if (TREE_CODE (next1) == ERROR_MARK)
		erred = 1;
	      else if (!TREE_LITERAL (next1))
		allconstant = 0;
	      else if (! initializer_constant_valid_p (next1))
		allsimple = 0;
	      members = tree_cons (field, next1, members);
	    }
	  else if (TREE_READONLY (field))
	    error ("uninitialized const member `%s'",
		   IDENTIFIER_POINTER (DECL_NAME (field)));
	  else if (TYPE_LANG_SPECIFIC (TREE_TYPE (field))
		   && CLASSTYPE_READONLY_FIELDS_NEED_INIT (TREE_TYPE (field)))
	    error ("member `%s' with uninitialized const fields",
		   IDENTIFIER_POINTER (DECL_NAME (field)));
	  else if (TREE_CODE (TREE_TYPE (field)) == REFERENCE_TYPE)
	    error ("member `%s' is uninitialized reference",
		   IDENTIFIER_POINTER (DECL_NAME (field)));
	}
    }

  /* If arguments were specified as a list, just remove the ones we used.  */
  if (elts)
    *elts = tail;
  /* If arguments were specified as a constructor,
     complain unless we used all the elements of the constructor.  */
  else if (tail)
    warning ("excess elements in aggregate initializer");

  if (erred)
    return error_mark_node;

  result = build (CONSTRUCTOR, type, NULL_TREE, nreverse (members));
  if (init)
    TREE_HAS_CONSTRUCTOR (result) = TREE_HAS_CONSTRUCTOR (init);
  if (allconstant) TREE_LITERAL (result) = 1;
  if (allconstant && allsimple) TREE_STATIC (result) = 1;
  return result;
}

/* Given a structure or union value DATUM, construct and return
   the structure or union component which results from narrowing
   that value by the types specified in TYPES.  For example, given the
   hierarchy

   class L { int ii; };
   class A : L { ... };
   class B : L { ... };
   class C : A, B { ... };

   and the declaration

   C x;

   then the expression

   x::C::A::L::ii refers to the ii member of the L part of
   of A part of the C object named by X.  In this case,
   DATUM would be x, and TYPES would be a SCOPE_REF consisting of

	SCOPE_REF
		SCOPE_REF
			C	A
		L

   The last entry in the SCOPE_REF is always an IDENTIFIER_NODE.

*/

tree
build_scoped_ref (datum, types)
     tree datum;
     tree types;
{
  tree orig_ref, ref;
  tree type = TREE_TYPE (datum);

  if (datum == error_mark_node)
    return error_mark_node;
  type = TYPE_MAIN_VARIANT (type);

  if (TREE_CODE (types) == SCOPE_REF)
    {
      /* We have some work to do.  */
      struct type_chain { tree type; struct type_chain *next; } *chain = 0, *head = 0;
      orig_ref = ref = build_unary_op (ADDR_EXPR, datum, 0);
      while (TREE_CODE (types) == SCOPE_REF)
	{
	  tree t = TREE_OPERAND (types, 1);
	  if (is_aggr_typedef (t, 1))
	    {
	      head = (struct type_chain *)alloca (sizeof (struct type_chain));
	      head->type = TREE_TYPE (TREE_TYPE (t));
	      head->next = chain;
	      chain = head;
	      types = TREE_OPERAND (types, 0);
	    }
	  else return error_mark_node;
	}
      if (! is_aggr_typedef (types, 1))
	return error_mark_node;

      head = (struct type_chain *)alloca (sizeof (struct type_chain));
      head->type = TREE_TYPE (TREE_TYPE (types));
      head->next = chain;
      chain = head;
      while (chain)
	{
	  tree basetype = chain->type;
	  type = TREE_TYPE (TREE_TYPE (ref));
	  if (basetype != type)
	    {
	      basetype = get_base_type (basetype, type, 1);
	      if (basetype == error_mark_node)
		return error_mark_node;
	      if (basetype == 0)
		{
		  error_not_base_type (TYPE_NAME_STRING (chain->type), TYPE_NAME_STRING (type));
		  return error_mark_node;
		}
	      ref = convert_pointer_to (basetype, ref);
	    }
	  chain = chain->next;
	}
      return build_indirect_ref (ref, "(compiler error in build_scoped_ref)");
    }

  /* This is an easy conversion.  */
  if (is_aggr_typedef (types, 1))
    {
      tree basetype = TREE_TYPE (TREE_TYPE (types));
      if (basetype != type)
	{
	  basetype = get_base_type (basetype, type, 1);
	  if (basetype == error_mark_node)
	    return error_mark_node;
	  if (basetype == 0)
	    {
	      error_not_base_type (TREE_TYPE (TREE_TYPE (types)), type);
	      return error_mark_node;
	    }
	}

      switch (TREE_CODE (datum))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case FLOAT_EXPR:
	case FIX_TRUNC_EXPR:
	case FIX_FLOOR_EXPR:
	case FIX_ROUND_EXPR:
	case FIX_CEIL_EXPR:
	  ref = convert_pointer_to (basetype,
				    build_unary_op (ADDR_EXPR, TREE_OPERAND (datum, 0), 0));
	  break;
	default:
	  ref = convert_pointer_to (basetype,
				    build_unary_op (ADDR_EXPR, datum, 0));
	}
      return build_indirect_ref (ref, "(compiler error in build_scoped_ref)");
    }
  return error_mark_node;
}

/* Build a reference to an object specified by the C++ `->' operator.
   Usually this just involves dereferencing the object, but if the
   `->' operator is overloaded, then such overloads must be
   performed until an object which does not have the `->' operator
   overloaded is found.  An error is reported when circular pointer
   delegation is detected.  */
tree
build_x_arrow (datum)
     tree datum;
{
  tree types_memoized = NULL_TREE;
  tree rval = datum;
  tree last_rval = default_conversion (datum);

  if (last_rval == error_mark_node)
    return error_mark_node;

  while (rval = build_opfncall (COMPONENT_REF, LOOKUP_NORMAL, rval))
    {
      if (rval == error_mark_node)
	return error_mark_node;

      if (value_member (TREE_TYPE (rval), types_memoized))
	{
	  error ("circular pointer delegation detected");
	  return error_mark_node;
	}
      else
	{
	  types_memoized = tree_cons (NULL_TREE, TREE_TYPE (rval),
				      types_memoized);
	}
      last_rval = rval;
    }

 more:
  if (TREE_CODE (TREE_TYPE (last_rval)) == REFERENCE_TYPE)
    last_rval = convert_from_reference (last_rval);

  if (TREE_CODE (TREE_TYPE (last_rval)) == POINTER_TYPE)
    return build_indirect_ref (last_rval, 0);

  if (TREE_CODE (TREE_TYPE (last_rval)) == OFFSET_TYPE)
    {
      if (TREE_CODE (last_rval) == OFFSET_REF
	  && TREE_STATIC (TREE_OPERAND (last_rval, 1)))
	{
	  last_rval = TREE_OPERAND (last_rval, 1);
	  goto more;
	}
      compiler_error ("invalid member type in build_x_arrow");
      return error_mark_node;
    }

  if (types_memoized)
    error ("result of `operator->()' yields non-pointer result");
  else
    error ("base operand of `->' is not a pointer");
  return error_mark_node;
}

/* Make an expression to refer to the COMPONENT field of
   structure or union value DATUM.  COMPONENT is an arbitrary
   expression.  DATUM has already been checked out to be of
   aggregate type.

   For C++, COMPONENT may be a TREE_LIST.  This happens when we must
   return an object of member type to a method of the current class,
   but there is not yet enough typing information to know which one.
   As a special case, if there is only one method by that name,
   it is returned.  Otherwise we return an expression which other
   routines will have to know how to deal with later.  */
tree
build_m_component_ref (datum, component)
     tree datum, component;
{
  tree type = TREE_TYPE (component);
  tree objtype = TREE_TYPE (datum);

  if (datum == error_mark_node || component == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (type) != OFFSET_TYPE && TREE_CODE (type) != METHOD_TYPE)
    {
      error ("non-member type composed with object");
      return error_mark_node;
    }

  if (TREE_CODE (objtype) == REFERENCE_TYPE)
    objtype = TREE_TYPE (objtype);

  if (! comptypes (TYPE_METHOD_BASETYPE (type), objtype, 0))
    {
      error ("member type `%s::' incompatible with object type `%s'",
	     TYPE_NAME_STRING (TYPE_METHOD_BASETYPE (type)),
	     TYPE_NAME_STRING (objtype));
      return error_mark_node;
    }

  return build (OFFSET_REF, TREE_TYPE (TREE_TYPE (component)), datum, component);
}

/* Return a tree node for the expression TYPENAME '(' PARMS ')'.  */
tree
build_functional_cast (exp, parms)
     tree exp;
     tree parms;
{
  /* This is either a call to a constructor,
     or a C cast in C++'s `functional' notation.  */
  tree type, name = NULL_TREE;

  if (parms == error_mark_node)
    return error_mark_node;

  if (TREE_CODE (exp) == IDENTIFIER_NODE)
    {
      name = exp;
      if (! TREE_TYPE (exp))
	{
	  type = lookup_name (exp);
	  if (!type || TREE_CODE (type) != TYPE_DECL)
	    {
	      error ("`%s' fails to be a typedef or built-in type",
		     IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	  type = TREE_TYPE (type);
	}
      else
	/* Either an enum or an aggregate type.  */
	type = TREE_TYPE (TREE_TYPE (exp));
    }
  else type = exp;

  if (! IS_AGGR_TYPE (type))
    {
      /* this must build a C cast */
      if (parms == NULL_TREE)
	{
	  error ("cannot cast null list to type `%s'",
		 IDENTIFIER_POINTER (name));
	  return error_mark_node;
	}
      return build_c_cast (type, build_compound_expr (parms));
    }

  /* Call to a consructor.  If this expression
     is actually used, for example,
	 
     return X (arg1, arg2, ...);
	 
     then the slot being initialized will be filled in.  */

  if (name == NULL_TREE)
    {
      name = TYPE_NAME (type);
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
    }

  if (TYPE_SIZE (type) == NULL_TREE)
    {
      error ("type `%s' is not yet defined", IDENTIFIER_POINTER (name));
      return error_mark_node;
    }

  if (! TYPE_NEEDS_CONSTRUCTOR (type))
    {
      if (TREE_CHAIN (parms) == NULL_TREE)
	{
	  tree rval = build_type_conversion (CONVERT_EXPR, type,
					     TREE_VALUE (parms), 1);
	  if (rval)
	    return rval;
	}
      error ("type `%s' does not have a constructor",
	     IDENTIFIER_POINTER (name));
      return error_mark_node;
    }

  if (! TYPE_HAS_CONSTRUCTOR (type))
    {
      /* Look through this type until we find the
	 base type which has a constructor.  */
      do
	{
	  int i, index = 0;

	  while (CLASSTYPE_N_BASECLASSES (type) == 1
		 && ! TYPE_HAS_CONSTRUCTOR (type))
	    type = CLASSTYPE_BASECLASS (type, 1);
	  if (TYPE_HAS_CONSTRUCTOR (type))
	    break;
	  /* Hack for MI.  */
	  i = CLASSTYPE_N_BASECLASSES (type);
	  while (i > 0)
	    {
	      if (TYPE_HAS_CONSTRUCTOR (CLASSTYPE_BASECLASS (type, i)))
		{
		  if (index == 0)
		    index = i;
		  else
		    {
		      error ("multiple base classes with constructor, ambiguous");
		      type = 0;
		      break;
		    }
		}
	      i -= 1;
	    }
	  if (type == 0)
	    break;
	} while (! TYPE_HAS_CONSTRUCTOR (type));
      if (type == 0)
	return error_mark_node;
    }
  name = TYPE_NAME (type);
  if (TREE_CODE (name) == TYPE_DECL)
    name = DECL_NAME (name);

  assert (TREE_CODE (name) == IDENTIFIER_NODE);

  {
    int flags = (parms && TREE_CHAIN (parms) == NULL_TREE
		 ? LOOKUP_PROTECT|LOOKUP_NO_CONVERSION : LOOKUP_COMPLAIN);
    tree rval;

  try_again:
    rval = build_method_call (NULL_TREE, name, parms, NULL_TREE, flags);

    if (rval != error_mark_node)
      {
	rval = build_cplus_new (type, rval);
	return rval;
      }

    /* If it didn't work going through constructor, try type conversion.  */
    if (! (flags & LOOKUP_COMPLAIN))
      {
	rval = build_type_conversion (CONVERT_EXPR, type, TREE_VALUE (parms),
				      !! (flags & LOOKUP_NO_CONVERSION));
	if (rval)
	  return rval;
	if (flags & LOOKUP_NO_CONVERSION)
	  {
	    flags = LOOKUP_NORMAL;
	    goto try_again;
	  }
      }

    return error_mark_node;
  }
}

/* Perform optimizations for front end.

   @@ These should be moved to fold-const.c when they are working.  */

static tree
make_merged_ref (ref, bitsize, bitpos, mode, volstruct)
     tree ref;
     int bitsize, bitpos;
     enum machine_mode mode;
     int volstruct;
{
  tree rval = make_node (RTL_EXPR);
  rtx op, extract_bit_field (), change_address ();

  /* This type is probably not right, but what effect will it have?  */
  TREE_TYPE (rval) = integer_type_node;
  do_pending_stack_adjust ();
  start_sequence ();
  /* Get base reference for these fields.  */
  op = expand_expr (ref, const0_rtx, VOIDmode, 0);
  if (GET_CODE (op) == REG || GET_CODE (op) == SUBREG
      || GET_MODE_SIZE (mode) * BITS_PER_UNIT != bitsize)
    {
      op = extract_bit_field (op, bitsize << 1, bitpos, 1, 0, mode, VOIDmode,
			      TYPE_ALIGN (TREE_TYPE (ref)) / BITS_PER_UNIT);
    }
  else
    {
      op = change_address (op, mode,
			   plus_constant (XEXP (op, 0),
					  bitpos / BITS_PER_UNIT));
      MEM_IN_STRUCT_P (op) = 1;
      MEM_VOLATILE_P (op) |= volstruct;
    }
  do_pending_stack_adjust ();

  TREE_VOLATILE (rval) = 1;
  RTL_EXPR_SEQUENCE (rval) = get_insns ();
  end_sequence ();
  RTL_EXPR_RTL (rval) = op;
  return rval;
}

/* Optimize COMPONENT_REFs in expressions that look like
   (x.p == y.p && x.q == y.q) or (x.p != y.p || x.q != y.q).

   Also optimize (x.p == c1 && x.q == c2).

   Return NULL_TREE if we could not perform the optimization,
   Otherwise, return the optimized result.  */
tree
merge_component_comparisons (code, op0, op1)
     enum tree_code code;
     tree op0;
     tree op1;
{
  tree l0, r0, l1, r1;

  l0 = TREE_OPERAND (op0, 0);
  r0 = TREE_OPERAND (op0, 1);
  l1 = TREE_OPERAND (op1, 0);
  r1 = TREE_OPERAND (op1, 1);

  if (TREE_CODE (l0) != TREE_CODE (l1)
      || TREE_CODE (r0) != TREE_CODE (r1))
    return 0;

  if (TREE_CODE (l0) != COMPONENT_REF
      && TREE_CODE (r0) == COMPONENT_REF)
    {
      l0 = TREE_OPERAND (op1, 0);
      r0 = TREE_OPERAND (op1, 1);
      l1 = TREE_OPERAND (op0, 0);
      r1 = TREE_OPERAND (op0, 1);
    }

  if (TREE_CODE (l0) == COMPONENT_REF
      && ((TREE_LITERAL (r0) && TREE_LITERAL (r1))
	  || (TREE_CODE (r0) == COMPONENT_REF
	      && TREE_OPERAND (l0, 1) == TREE_OPERAND (r0, 1)
	      && TREE_OPERAND (l1, 1) == TREE_OPERAND (r1, 1)
	      && simple_cst_equal (TREE_OPERAND (r0, 0), TREE_OPERAND (r1, 0)) > 0))
      && simple_cst_equal (TREE_OPERAND (l0, 0), TREE_OPERAND (l1, 0)) > 0)
    {
      tree f0 = TREE_OPERAND (l0, 1);
      tree f1 = TREE_OPERAND (l1, 1);
      tree tem0 = TREE_OPERAND (l0, 0);
      tree tem1 = 0;
      int align, bitsize, bitsize0, bitsize1;
      int bitpos, bitpos0, bitpos1;
      enum machine_mode trymode;
      int volstruct0 = 0, volstruct1 = 0;

      bitsize0 = TREE_INT_CST_LOW (DECL_SIZE (f0)) * DECL_SIZE_UNIT (f0);
      bitsize1 = TREE_INT_CST_LOW (DECL_SIZE (f1)) * DECL_SIZE_UNIT (f1);

      /* Only handles easy cases right now:
	 QImode+QImode => HImode
	 HImode+HImode => SImode.  */
      if ((bitsize0 != bitsize1 && TREE_CODE (r0) != INTEGER_CST)
	  || bitsize0 + bitsize1 > BITS_PER_WORD)
	return 0;

      /* Compute cumulative bit-offset for nested component-refs
	 and array-refs, and find the ultimate containing object.  */

      if (DECL_OFFSET (f1) > DECL_OFFSET (f0))
	bitpos0 = DECL_OFFSET (f0), bitpos1 = DECL_OFFSET (f1);
      else
	bitpos0 = DECL_OFFSET (f1), bitpos1 = DECL_OFFSET (f0);

      align = TYPE_ALIGN (TREE_TYPE (tem0));

      if (TREE_CODE (r0) == COMPONENT_REF)
	{
	  tem1 = TREE_OPERAND (r0, 0);
	  if (TYPE_ALIGN (TREE_TYPE (tem1)) < align)
	    align = TYPE_ALIGN (tem1);
	}

      bitpos = bitpos0;
      while (1)
	{
	  if (TREE_CODE (tem0) == COMPONENT_REF)
	    {
	      bitpos0 += DECL_OFFSET (TREE_OPERAND (tem0, 1));
	      if (TREE_THIS_VOLATILE (tem0))
		volstruct0 = 1;
	    }
	  else if (TREE_CODE (tem0) == ARRAY_REF
		   && TREE_CODE (TREE_OPERAND (tem0, 1)) == INTEGER_CST
		   && TREE_CODE (TYPE_SIZE (TREE_TYPE (tem0))) == INTEGER_CST)
	    {
	      bitpos0 += (TREE_INT_CST_LOW (TREE_OPERAND (tem0, 1))
			  * TREE_INT_CST_LOW (TYPE_SIZE (TREE_TYPE (tem0)))
			  * TYPE_SIZE_UNIT (TREE_TYPE (tem0)));
	    }
	  else
	    break;
	  tem0 = TREE_OPERAND (tem0, 0);
	  if (tem1 == 0)
	    continue;
	  if (TREE_CODE (tem1) == COMPONENT_REF)
	    {
	      if (TREE_THIS_VOLATILE (tem1))
		volstruct1 = 1;
	    }
	  else if (TREE_CODE (tem1) == ARRAY_REF
		   && TREE_CODE (TREE_OPERAND (tem1, 1)) == INTEGER_CST
		   && TREE_CODE (TYPE_SIZE (TREE_TYPE (tem1))) == INTEGER_CST)
	    ;
	  else
	    break;
	}
      bitpos1 += bitpos0 - bitpos;

      /* If these two accesses are not contiguous, 
	 or they are not to the same byte, just fail.  */

      if (bitpos0 + bitsize0 == bitpos1)
	/* This is the size of the access we want to make.  */
	bitsize = bitsize0 << 1;
      else if (((bitpos0 + bitsize0 + BITS_PER_UNIT - 1) / BITS_PER_UNIT)
	       == ((bitpos1 + BITS_PER_UNIT - 1) / BITS_PER_UNIT))
	/* Bits from the same byte.  */
	bitsize = bitpos1 - bitpos0;
      else
	/* Failure.  */
	return 0;

      if (align < bitsize)
	return 0;

      /* Figure out the mode we will use for this access.  */
      for (trymode = QImode;
	   trymode && GET_MODE_SIZE (trymode) * BITS_PER_UNIT < bitsize;
	   trymode = GET_MODE_WIDER_MODE (trymode))
	if (GET_MODE_SIZE (trymode) * BITS_PER_UNIT > align)
	  return 0;

      assert ((int)trymode != 0);

      /* Now build tree structure which distributes TREE_CODE (op0) over CODE.  */
      tem0 = make_merged_ref (tem0, bitsize, bitpos0, trymode, volstruct0);
      if (TREE_CODE (r0) == COMPONENT_REF)
	{
	  tem1 = make_merged_ref (tem1, bitsize, bitpos0, trymode, volstruct1);
	  return build (TREE_CODE (op0), unsigned_type_node, tem0, tem1);
	}
      if (integer_zerop (r0) && integer_zerop (r1))
	tem1 = integer_zero_node;
      else
	tem1 = build_int_2 (TREE_INT_CST_LOW (r0)|(TREE_INT_CST_LOW (r1)<<(bitpos1-bitpos0)), 0);
      if (bitpos0 + bitsize0 == bitpos1)
	return build (TREE_CODE (op0), unsigned_type_node, tem0, tem1);
      /* Test this quantity against a mask.  */
      return build (NE_EXPR, integer_type_node,
		    build (BIT_AND_EXPR, unsigned_type_node,
			   tem0, tem1),
		    integer_zero_node);
    }

  return 0;
}

/* Return the character string for the name that encodes the
   enumeral value VALUE in the domain TYPE.  */
char *
enum_name_string (value, type)
     tree value;
     tree type;
{
  register tree values = TYPE_VALUES (type);
  register int intval = TREE_INT_CST_LOW (value);

  assert (TREE_CODE (type) == ENUMERAL_TYPE);
  while (values
	 && TREE_INT_CST_LOW (TREE_VALUE (values)) != intval)
    values = TREE_CHAIN (values);
  if (values == NULL_TREE)
    {
      char *buf = (char *)oballoc (16 + TYPE_NAME_LENGTH (type));

      /* Value must have been cast.  */
      sprintf (buf, "(enum %s)%d",
	       TYPE_NAME_STRING (type), intval);
      return buf;
    }
  return IDENTIFIER_POINTER (TREE_PURPOSE (values));
}

/* Print out a language-specific error message for
   (Pascal) case or (C) switch statements.
   CODE tells what sort of message to print. 
   TYPE is the type of the switch index expression.
   NEW is the new value that we were trying to add.
   OLD is the old value that stopped us from adding it.  */
void
report_case_error (code, type, new_value, old_value)
     int code;
     tree type;
     tree new_value, old_value;
{
  if (code == 1)
    {
      if (new_value)
	error ("case label not within a switch statement");
      else
	error ("default label not within a switch statement");
    }
  else if (code == 2)
    {
      if (new_value == 0)
	{
	  error ("multiple default labels in one switch");
	  return;
	}
      if (TREE_CODE (new_value) == RANGE_EXPR)
	if (TREE_CODE (old_value) == RANGE_EXPR)
	  {
	    char *buf = (char *)alloca (4 * (8 + TYPE_NAME_LENGTH (type)));
	    if (TREE_CODE (type) == ENUMERAL_TYPE)
	      sprintf (buf, "overlapping ranges [%s..%s], [%s..%s] in case expression",
		       enum_name_string (TREE_OPERAND (new_value, 0), type),
		       enum_name_string (TREE_OPERAND (new_value, 1), type),
		       enum_name_string (TREE_OPERAND (old_value, 0), type),
		       enum_name_string (TREE_OPERAND (old_value, 1), type));
	    else
	      sprintf (buf, "overlapping ranges [%d..%d], [%d..%d] in case expression",
		       TREE_INT_CST_LOW (TREE_OPERAND (new_value, 0)),
		       TREE_INT_CST_LOW (TREE_OPERAND (new_value, 1)),
		       TREE_INT_CST_LOW (TREE_OPERAND (old_value, 0)),
		       TREE_INT_CST_LOW (TREE_OPERAND (old_value, 1)));
	    error (buf);
	  }
	else
	  {
	    char *buf = (char *)alloca (4 * (8 + TYPE_NAME_LENGTH (type)));
	    if (TREE_CODE (type) == ENUMERAL_TYPE)
	      sprintf (buf, "range [%s..%s] includes element `%s' in case expression",
		       enum_name_string (TREE_OPERAND (new_value, 0), type),
		       enum_name_string (TREE_OPERAND (new_value, 1), type),
		       enum_name_string (old_value, type));
	    else
	      sprintf (buf, "range [%d..%d] includes (%d) in case expression",
		       TREE_INT_CST_LOW (TREE_OPERAND (new_value, 0)),
		       TREE_INT_CST_LOW (TREE_OPERAND (new_value, 1)),
		       TREE_INT_CST_LOW (old_value));
	    error (buf);
	  }
      else if (TREE_CODE (old_value) == RANGE_EXPR)
	{
	  char *buf = (char *)alloca (4 * (8 + TYPE_NAME_LENGTH (type)));
	  if (TREE_CODE (type) == ENUMERAL_TYPE)
	    sprintf (buf, "range [%s..%s] includes element `%s' in case expression",
		     enum_name_string (TREE_OPERAND (old_value, 0), type),
		     enum_name_string (TREE_OPERAND (old_value, 1), type),
		     enum_name_string (new_value, type));
	  else
	    sprintf (buf, "range [%d..%d] includes (%d) in case expression",
		     TREE_INT_CST_LOW (TREE_OPERAND (old_value, 0)),
		     TREE_INT_CST_LOW (TREE_OPERAND (old_value, 1)),
		     TREE_INT_CST_LOW (new_value));
	  error (buf);
	}
      else
	{
	  if (TREE_CODE (type) == ENUMERAL_TYPE)
	    error ("duplicate label `%s' in switch statement",
		   enum_name_string (new_value, type));
	  else
	    error ("duplicate label (%d) in switch statement",
		   TREE_INT_CST_LOW (new_value));
	}
    }
  else if (code == 3)
    {
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	warning ("case value out of range for enum %s",
		 TYPE_NAME_STRING (type));
      else
	warning ("case value out of range");
    }
  else if (code == 4)
    {
      if (TREE_CODE (type) == ENUMERAL_TYPE)
	error ("range values `%s' and `%s' reversed",
	       enum_name_string (new_value, type),
	       enum_name_string (old_value, type));
      else
	error ("range values reversed");
    }
}
