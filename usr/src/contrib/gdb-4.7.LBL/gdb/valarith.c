/* Perform arithmetic and other operations on values, for GDB.
   Copyright 1986, 1989, 1991, 1992 Free Software Foundation, Inc.

This file is part of GDB.

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "defs.h"
#include "value.h"
#include "symtab.h"
#include "gdbtypes.h"
#include "expression.h"
#include "target.h"
#include <string.h>

static value
value_subscripted_rvalue PARAMS ((value, value));


value
value_add (arg1, arg2)
	value arg1, arg2;
{
  register value valint, valptr;
  register int len;

  COERCE_ARRAY (arg1);
  COERCE_ARRAY (arg2);

  if ((TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_PTR
       || TYPE_CODE (VALUE_TYPE (arg2)) == TYPE_CODE_PTR)
      &&
      (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_INT
       || TYPE_CODE (VALUE_TYPE (arg2)) == TYPE_CODE_INT))
    /* Exactly one argument is a pointer, and one is an integer.  */
    {
      if (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_PTR)
	{
	  valptr = arg1;
	  valint = arg2;
	}
      else
	{
	  valptr = arg2;
	  valint = arg1;
	}
      len = TYPE_LENGTH (TYPE_TARGET_TYPE (VALUE_TYPE (valptr)));
      if (len == 0) len = 1;	/* For (void *) */
      return value_from_longest (VALUE_TYPE (valptr),
			      value_as_long (valptr)
			      + (len * value_as_long (valint)));
    }

  return value_binop (arg1, arg2, BINOP_ADD);
}

value
value_sub (arg1, arg2)
	value arg1, arg2;
{

  COERCE_ARRAY (arg1);
  COERCE_ARRAY (arg2);

  if (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_PTR)
    {
      if (TYPE_CODE (VALUE_TYPE (arg2)) == TYPE_CODE_INT)
	{
	  /* pointer - integer.  */
	  return value_from_longest
	    (VALUE_TYPE (arg1),
	     value_as_long (arg1)
	     - (TYPE_LENGTH (TYPE_TARGET_TYPE (VALUE_TYPE (arg1)))
		* value_as_long (arg2)));
	}
      else if (VALUE_TYPE (arg1) == VALUE_TYPE (arg2))
	{
	  /* pointer to <type x> - pointer to <type x>.  */
	  return value_from_longest
	    (builtin_type_long,		/* FIXME -- should be ptrdiff_t */
	     (value_as_long (arg1) - value_as_long (arg2))
	     / TYPE_LENGTH (TYPE_TARGET_TYPE (VALUE_TYPE (arg1))));
	}
      else
	{
	  error ("\
First argument of `-' is a pointer and second argument is neither\n\
an integer nor a pointer of the same type.");
	}
    }

  return value_binop (arg1, arg2, BINOP_SUB);
}

/* Return the value of ARRAY[IDX].  */

value
value_subscript (array, idx)
     value array, idx;
{
  if (TYPE_CODE (VALUE_TYPE (array)) == TYPE_CODE_ARRAY
      && VALUE_LVAL (array) != lval_memory)
    return value_subscripted_rvalue (array, idx);
  else
    return value_ind (value_add (array, idx));
}

/* Return the value of EXPR[IDX], expr an aggregate rvalue
   (eg, a vector register).  This routine used to promote floats
   to doubles, but no longer does.  */

static value
value_subscripted_rvalue (array, idx)
     value array, idx;
{
  struct type *elt_type = TYPE_TARGET_TYPE (VALUE_TYPE (array));
  int elt_size = TYPE_LENGTH (elt_type);
  int elt_offs = elt_size * longest_to_int (value_as_long (idx));
  value v;

  if (elt_offs >= TYPE_LENGTH (VALUE_TYPE (array)))
    error ("no such vector element");

  v = allocate_value (elt_type);
  memcpy (VALUE_CONTENTS (v), VALUE_CONTENTS (array) + elt_offs, elt_size);

  if (VALUE_LVAL (array) == lval_internalvar)
    VALUE_LVAL (v) = lval_internalvar_component;
  else
    VALUE_LVAL (v) = not_lval;
  VALUE_ADDRESS (v) = VALUE_ADDRESS (array);
  VALUE_OFFSET (v) = VALUE_OFFSET (array) + elt_offs;
  VALUE_BITSIZE (v) = elt_size * 8;
  return v;
}

/* Check to see if either argument is a structure.  This is called so
   we know whether to go ahead with the normal binop or look for a 
   user defined function instead.

   For now, we do not overload the `=' operator.  */

int
binop_user_defined_p (op, arg1, arg2)
     enum exp_opcode op;
     value arg1, arg2;
{
  if (op == BINOP_ASSIGN)
    return 0;
  return (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_STRUCT
	  || TYPE_CODE (VALUE_TYPE (arg2)) == TYPE_CODE_STRUCT
	  || (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_REF
	      && TYPE_CODE (TYPE_TARGET_TYPE (VALUE_TYPE (arg1))) == TYPE_CODE_STRUCT)
	  || (TYPE_CODE (VALUE_TYPE (arg2)) == TYPE_CODE_REF
	      && TYPE_CODE (TYPE_TARGET_TYPE (VALUE_TYPE (arg2))) == TYPE_CODE_STRUCT));
}

/* Check to see if argument is a structure.  This is called so
   we know whether to go ahead with the normal unop or look for a 
   user defined function instead.

   For now, we do not overload the `&' operator.  */

int unop_user_defined_p (op, arg1)
     enum exp_opcode op;
     value arg1;
{
  if (op == UNOP_ADDR)
    return 0;
  return (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_STRUCT
	  || (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_REF
	      && TYPE_CODE (TYPE_TARGET_TYPE (VALUE_TYPE (arg1))) == TYPE_CODE_STRUCT));
}

/* We know either arg1 or arg2 is a structure, so try to find the right
   user defined function.  Create an argument vector that calls 
   arg1.operator @ (arg1,arg2) and return that value (where '@' is any
   binary operator which is legal for GNU C++).

   OP is the operatore, and if it is BINOP_ASSIGN_MODIFY, then OTHEROP
   is the opcode saying how to modify it.  Otherwise, OTHEROP is
   unused.  */

value
value_x_binop (arg1, arg2, op, otherop)
     value arg1, arg2;
     enum exp_opcode op, otherop;
{
  value * argvec;
  char *ptr;
  char tstr[13];
  int static_memfuncp;

  COERCE_REF (arg1);
  COERCE_REF (arg2);
  COERCE_ENUM (arg1);
  COERCE_ENUM (arg2);

  /* now we know that what we have to do is construct our
     arg vector and find the right function to call it with.  */

  if (TYPE_CODE (VALUE_TYPE (arg1)) != TYPE_CODE_STRUCT)
    error ("Can't do that binary op on that type");  /* FIXME be explicit */

  argvec = (value *) alloca (sizeof (value) * 4);
  argvec[1] = value_addr (arg1);
  argvec[2] = arg2;
  argvec[3] = 0;

  /* make the right function name up */  
  strcpy(tstr, "operator__");
  ptr = tstr+8;
  switch (op)
    {
    case BINOP_ADD:	strcpy(ptr,"+"); break;
    case BINOP_SUB:	strcpy(ptr,"-"); break;
    case BINOP_MUL:	strcpy(ptr,"*"); break;
    case BINOP_DIV:	strcpy(ptr,"/"); break;
    case BINOP_REM:	strcpy(ptr,"%"); break;
    case BINOP_LSH:	strcpy(ptr,"<<"); break;
    case BINOP_RSH:	strcpy(ptr,">>"); break;
    case BINOP_LOGAND:	strcpy(ptr,"&"); break;
    case BINOP_LOGIOR:	strcpy(ptr,"|"); break;
    case BINOP_LOGXOR:	strcpy(ptr,"^"); break;
    case BINOP_AND:	strcpy(ptr,"&&"); break;
    case BINOP_OR:	strcpy(ptr,"||"); break;
    case BINOP_MIN:	strcpy(ptr,"<?"); break;
    case BINOP_MAX:	strcpy(ptr,">?"); break;
    case BINOP_ASSIGN:	strcpy(ptr,"="); break;
    case BINOP_ASSIGN_MODIFY:	
      switch (otherop)
	{
	case BINOP_ADD:      strcpy(ptr,"+="); break;
	case BINOP_SUB:      strcpy(ptr,"-="); break;
	case BINOP_MUL:      strcpy(ptr,"*="); break;
	case BINOP_DIV:      strcpy(ptr,"/="); break;
	case BINOP_REM:      strcpy(ptr,"%="); break;
	case BINOP_LOGAND:   strcpy(ptr,"&="); break;
	case BINOP_LOGIOR:   strcpy(ptr,"|="); break;
	case BINOP_LOGXOR:   strcpy(ptr,"^="); break;
	default:
	  error ("Invalid binary operation specified.");
	}
      break;
    case BINOP_SUBSCRIPT: strcpy(ptr,"[]"); break;
    case BINOP_EQUAL:	  strcpy(ptr,"=="); break;
    case BINOP_NOTEQUAL:  strcpy(ptr,"!="); break;
    case BINOP_LESS:      strcpy(ptr,"<"); break;
    case BINOP_GTR:       strcpy(ptr,">"); break;
    case BINOP_GEQ:       strcpy(ptr,">="); break;
    case BINOP_LEQ:       strcpy(ptr,"<="); break;
    default:
      error ("Invalid binary operation specified.");
    }
  argvec[0] = value_struct_elt (&arg1, argvec+1, tstr, &static_memfuncp, "structure");
  if (argvec[0])
    {
      if (static_memfuncp)
	{
	  argvec[1] = argvec[0];
	  argvec++;
	}
      return call_function_by_hand (argvec[0], 2 - static_memfuncp, argvec + 1);
    }
  error ("member function %s not found", tstr);
#ifdef lint
  return call_function_by_hand (argvec[0], 2 - static_memfuncp, argvec + 1);
#endif
}

/* We know that arg1 is a structure, so try to find a unary user
   defined operator that matches the operator in question.  
   Create an argument vector that calls arg1.operator @ (arg1)
   and return that value (where '@' is (almost) any unary operator which
   is legal for GNU C++).  */

value
value_x_unop (arg1, op)
     value arg1;
     enum exp_opcode op;
{
  value * argvec;
  char *ptr;
  char tstr[13];
  int static_memfuncp;

  COERCE_ENUM (arg1);

  /* now we know that what we have to do is construct our
     arg vector and find the right function to call it with.  */

  if (TYPE_CODE (VALUE_TYPE (arg1)) != TYPE_CODE_STRUCT)
    error ("Can't do that unary op on that type");  /* FIXME be explicit */

  argvec = (value *) alloca (sizeof (value) * 3);
  argvec[1] = value_addr (arg1);
  argvec[2] = 0;

  /* make the right function name up */  
  strcpy(tstr,"operator__");
  ptr = tstr+8;
  switch (op)
    {
    case UNOP_PREINCREMENT:	strcpy(ptr,"++"); break;
    case UNOP_PREDECREMENT:	strcpy(ptr,"++"); break;
    case UNOP_POSTINCREMENT:	strcpy(ptr,"++"); break;
    case UNOP_POSTDECREMENT:	strcpy(ptr,"++"); break;
    case UNOP_ZEROP:	strcpy(ptr,"!"); break;
    case UNOP_LOGNOT:	strcpy(ptr,"~"); break;
    case UNOP_NEG:	strcpy(ptr,"-"); break;
    default:
      error ("Invalid binary operation specified.");
    }
  argvec[0] = value_struct_elt (&arg1, argvec+1, tstr, &static_memfuncp, "structure");
  if (argvec[0])
    {
      if (static_memfuncp)
	{
	  argvec[1] = argvec[0];
	  argvec++;
	}
      return call_function_by_hand (argvec[0], 1 - static_memfuncp, argvec + 1);
    }
  error ("member function %s not found", tstr);
  return 0;  /* For lint -- never reached */
}

/* Perform a binary operation on two integers or two floats.
   Does not support addition and subtraction on pointers;
   use value_add or value_sub if you want to handle those possibilities.  */

value
value_binop (arg1, arg2, op)
     value arg1, arg2;
     enum exp_opcode op;
{
  register value val;

  COERCE_ENUM (arg1);
  COERCE_ENUM (arg2);

  if ((TYPE_CODE (VALUE_TYPE (arg1)) != TYPE_CODE_FLT
       &&
       TYPE_CODE (VALUE_TYPE (arg1)) != TYPE_CODE_INT)
      ||
      (TYPE_CODE (VALUE_TYPE (arg2)) != TYPE_CODE_FLT
       &&
       TYPE_CODE (VALUE_TYPE (arg2)) != TYPE_CODE_INT))
    error ("Argument to arithmetic operation not a number.");

  if (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_FLT
      ||
      TYPE_CODE (VALUE_TYPE (arg2)) == TYPE_CODE_FLT)
    {
      double v1, v2, v;
      v1 = value_as_double (arg1);
      v2 = value_as_double (arg2);
      switch (op)
	{
	case BINOP_ADD:
	  v = v1 + v2;
	  break;

	case BINOP_SUB:
	  v = v1 - v2;
	  break;

	case BINOP_MUL:
	  v = v1 * v2;
	  break;

	case BINOP_DIV:
	  v = v1 / v2;
	  break;

	default:
	  error ("Integer-only operation on floating point number.");
	}

      val = allocate_value (builtin_type_double);
      SWAP_TARGET_AND_HOST (&v, sizeof (v));
      *(double *) VALUE_CONTENTS_RAW (val) = v;
    }
  else
    /* Integral operations here.  */
    {
      /* Should we promote to unsigned longest?  */
      if ((TYPE_UNSIGNED (VALUE_TYPE (arg1))
	   || TYPE_UNSIGNED (VALUE_TYPE (arg2)))
	  && (TYPE_LENGTH (VALUE_TYPE (arg1)) >= sizeof (unsigned LONGEST)
	      || TYPE_LENGTH (VALUE_TYPE (arg2)) >= sizeof (unsigned LONGEST)))
	{
	  unsigned LONGEST v1, v2, v;
	  v1 = (unsigned LONGEST) value_as_long (arg1);
	  v2 = (unsigned LONGEST) value_as_long (arg2);
	  
	  switch (op)
	    {
	    case BINOP_ADD:
	      v = v1 + v2;
	      break;
	      
	    case BINOP_SUB:
	      v = v1 - v2;
	      break;
	      
	    case BINOP_MUL:
	      v = v1 * v2;
	      break;
	      
	    case BINOP_DIV:
	      v = v1 / v2;
	      break;
	      
	    case BINOP_REM:
	      v = v1 % v2;
	      break;
	      
	    case BINOP_LSH:
	      v = v1 << v2;
	      break;
	      
	    case BINOP_RSH:
	      v = v1 >> v2;
	      break;
	      
	    case BINOP_LOGAND:
	      v = v1 & v2;
	      break;
	      
	    case BINOP_LOGIOR:
	      v = v1 | v2;
	      break;
	      
	    case BINOP_LOGXOR:
	      v = v1 ^ v2;
	      break;
	      
	    case BINOP_AND:
	      v = v1 && v2;
	      break;
	      
	    case BINOP_OR:
	      v = v1 || v2;
	      break;
	      
	    case BINOP_MIN:
	      v = v1 < v2 ? v1 : v2;
	      break;
	      
	    case BINOP_MAX:
	      v = v1 > v2 ? v1 : v2;
	      break;
	      
	    default:
	      error ("Invalid binary operation on numbers.");
	    }

	  val = allocate_value (BUILTIN_TYPE_UNSIGNED_LONGEST);
	  SWAP_TARGET_AND_HOST (&v, sizeof (v));
	  *(unsigned LONGEST *) VALUE_CONTENTS_RAW (val) = v;
	}
      else
	{
	  LONGEST v1, v2, v;
	  v1 = value_as_long (arg1);
	  v2 = value_as_long (arg2);
	  
	  switch (op)
	    {
	    case BINOP_ADD:
	      v = v1 + v2;
	      break;
	      
	    case BINOP_SUB:
	      v = v1 - v2;
	      break;
	      
	    case BINOP_MUL:
	      v = v1 * v2;
	      break;
	      
	    case BINOP_DIV:
	      v = v1 / v2;
	      break;
	      
	    case BINOP_REM:
	      v = v1 % v2;
	      break;
	      
	    case BINOP_LSH:
	      v = v1 << v2;
	      break;
	      
	    case BINOP_RSH:
	      v = v1 >> v2;
	      break;
	      
	    case BINOP_LOGAND:
	      v = v1 & v2;
	      break;
	      
	    case BINOP_LOGIOR:
	      v = v1 | v2;
	      break;
	      
	    case BINOP_LOGXOR:
	      v = v1 ^ v2;
	      break;
	      
	    case BINOP_AND:
	      v = v1 && v2;
	      break;
	      
	    case BINOP_OR:
	      v = v1 || v2;
	      break;
	      
	    case BINOP_MIN:
	      v = v1 < v2 ? v1 : v2;
	      break;
	      
	    case BINOP_MAX:
	      v = v1 > v2 ? v1 : v2;
	      break;
	      
	    default:
	      error ("Invalid binary operation on numbers.");
	    }
	  
	  val = allocate_value (BUILTIN_TYPE_LONGEST);
	  SWAP_TARGET_AND_HOST (&v, sizeof (v));
	  *(LONGEST *) VALUE_CONTENTS_RAW (val) = v;
	}
    }

  return val;
}

/* Simulate the C operator ! -- return 1 if ARG1 contains zero.  */

int
value_zerop (arg1)
     value arg1;
{
  register int len;
  register char *p;

  COERCE_ARRAY (arg1);

  if (TYPE_CODE (VALUE_TYPE (arg1)) == TYPE_CODE_FLT)
    return 0 == value_as_double (arg1);

  len = TYPE_LENGTH (VALUE_TYPE (arg1));
  p = VALUE_CONTENTS (arg1);

  while (--len >= 0)
    {
      if (*p++)
	break;
    }

  return len < 0;
}

/* Simulate the C operator == by returning a 1
   iff ARG1 and ARG2 have equal contents.  */

int
value_equal (arg1, arg2)
     register value arg1, arg2;

{
  register int len;
  register char *p1, *p2;
  enum type_code code1;
  enum type_code code2;

  COERCE_ARRAY (arg1);
  COERCE_ARRAY (arg2);

  code1 = TYPE_CODE (VALUE_TYPE (arg1));
  code2 = TYPE_CODE (VALUE_TYPE (arg2));

  if (code1 == TYPE_CODE_INT && code2 == TYPE_CODE_INT)
    return value_as_long (arg1) == value_as_long (arg2);
  else if ((code1 == TYPE_CODE_FLT || code1 == TYPE_CODE_INT)
	   && (code2 == TYPE_CODE_FLT || code2 == TYPE_CODE_INT))
    return value_as_double (arg1) == value_as_double (arg2);

  /* FIXME: Need to promote to either CORE_ADDR or LONGEST, whichever
     is bigger.  */
  else if (code1 == TYPE_CODE_PTR && code2 == TYPE_CODE_INT)
    return value_as_pointer (arg1) == (CORE_ADDR) value_as_long (arg2);
  else if (code2 == TYPE_CODE_PTR && code1 == TYPE_CODE_INT)
    return (CORE_ADDR) value_as_long (arg1) == value_as_pointer (arg2);

  else if (code1 == code2
	   && ((len = TYPE_LENGTH (VALUE_TYPE (arg1)))
	       == TYPE_LENGTH (VALUE_TYPE (arg2))))
    {
      p1 = VALUE_CONTENTS (arg1);
      p2 = VALUE_CONTENTS (arg2);
      while (--len >= 0)
	{
	  if (*p1++ != *p2++)
	    break;
	}
      return len < 0;
    }
  else
    {
      error ("Invalid type combination in equality test.");
      return 0;  /* For lint -- never reached */
    }
}

/* Simulate the C operator < by returning 1
   iff ARG1's contents are less than ARG2's.  */

int
value_less (arg1, arg2)
     register value arg1, arg2;
{
  register enum type_code code1;
  register enum type_code code2;

  COERCE_ARRAY (arg1);
  COERCE_ARRAY (arg2);

  code1 = TYPE_CODE (VALUE_TYPE (arg1));
  code2 = TYPE_CODE (VALUE_TYPE (arg2));

  if (code1 == TYPE_CODE_INT && code2 == TYPE_CODE_INT)
    {
      if (TYPE_UNSIGNED (VALUE_TYPE (arg1))
       || TYPE_UNSIGNED (VALUE_TYPE (arg2)))
	return ((unsigned LONGEST) value_as_long (arg1)
		< (unsigned LONGEST) value_as_long (arg2));
      else
	return value_as_long (arg1) < value_as_long (arg2);
    }
  else if ((code1 == TYPE_CODE_FLT || code1 == TYPE_CODE_INT)
	   && (code2 == TYPE_CODE_FLT || code2 == TYPE_CODE_INT))
    return value_as_double (arg1) < value_as_double (arg2);
  else if (code1 == TYPE_CODE_PTR && code2 == TYPE_CODE_PTR)
    return value_as_pointer (arg1) < value_as_pointer (arg2);

  /* FIXME: Need to promote to either CORE_ADDR or LONGEST, whichever
     is bigger.  */
  else if (code1 == TYPE_CODE_PTR && code2 == TYPE_CODE_INT)
    return value_as_pointer (arg1) < (CORE_ADDR) value_as_long (arg2);
  else if (code2 == TYPE_CODE_PTR && code1 == TYPE_CODE_INT)
    return (CORE_ADDR) value_as_long (arg1) < value_as_pointer (arg2);

  else
    {
      error ("Invalid type combination in ordering comparison.");
      return 0;
    }
}

/* The unary operators - and ~.  Both free the argument ARG1.  */

value
value_neg (arg1)
     register value arg1;
{
  register struct type *type;

  COERCE_ENUM (arg1);

  type = VALUE_TYPE (arg1);

  if (TYPE_CODE (type) == TYPE_CODE_FLT)
    return value_from_double (type, - value_as_double (arg1));
  else if (TYPE_CODE (type) == TYPE_CODE_INT)
    return value_from_longest (type, - value_as_long (arg1));
  else {
    error ("Argument to negate operation not a number.");
    return 0;  /* For lint -- never reached */
  }
}

value
value_lognot (arg1)
     register value arg1;
{
  COERCE_ENUM (arg1);

  if (TYPE_CODE (VALUE_TYPE (arg1)) != TYPE_CODE_INT)
    error ("Argument to complement operation not an integer.");

  return value_from_longest (VALUE_TYPE (arg1), ~ value_as_long (arg1));
}

