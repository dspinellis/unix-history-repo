/* Handle the hair of processing (but not expanding) inline functions.
   Also manage function and varaible name overloading.
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


/* Handle method declarations.  */
#include <stdio.h>
#include "config.h"
#include "tree.h"
#include "cplus-tree.h"
#include "assert.h"

/* TREE_LIST of the current inline functions that need to be
   processed.  */
struct pending_inline *pending_inlines;

# define MAX_INLINE_BUF_SIZE 8188
# define OB_INIT() (inline_bufp = inline_buffer)
# define OB_PUTC(C) (*inline_bufp++ = (C))
# define OB_PUTC2(C1,C2) (OB_PUTC (C1), OB_PUTC (C2))
# define OB_PUTS(S) (strcpy (inline_bufp, S), inline_bufp += sizeof (S) - 1)
# define OB_PUTCP(S) (strcpy (inline_bufp, S), inline_bufp += strlen (S))
# define OB_FINISH() (*inline_bufp++ = '\0')

/* Counter to help build parameter names in case they were omitted.  */
static int dummy_name;
static int in_parmlist;
/* Just a pointer into INLINE_BUFFER.  */
static char *inline_bufp;
/* Also a pointer into INLINE_BUFFER.  This points to a safe place to
   cut back to if we assign it 0, in case of error.  */
static char *inline_errp;
static char *inline_buffer;
static void dump_type (), dump_decl ();
static void dump_init (), dump_unary_op (), dump_binary_op ();

tree wrapper_name, wrapper_pred_name, anti_wrapper_name;

#ifdef NO_AUTO_OVERLOAD
int is_overloaded ();
#endif

void
init_method ()
{
  char buf[sizeof (ANTI_WRAPPER_NAME_FORMAT) + 8];
  sprintf (buf, WRAPPER_NAME_FORMAT, "");
  wrapper_name = get_identifier (buf);
  sprintf (buf, WRAPPER_PRED_NAME_FORMAT, "");
  wrapper_pred_name = get_identifier (buf);
  sprintf (buf, ANTI_WRAPPER_NAME_FORMAT, "");
  anti_wrapper_name = get_identifier (buf);
}

/* Return a pointer to the end of the new text in INLINE_BUFFER.
   We cannot use `fatal' or `error' in here because that
   might cause an infinite loop.  */
static char *
new_text_len (s)
     char *s;
{
  while (*s++) ;

  if (s >= inline_buffer + MAX_INLINE_BUF_SIZE)
    {
      fprintf (stderr, "recompile c++ with larger MAX_INLINE_BUF_SIZE (%d)", MAX_INLINE_BUF_SIZE);
      abort ();
    }
  return s - 1;
}

/* Check that we have not overflowed INLINE_BUFFER.
   We cannot use `fatal' or `error' in here because that
   might cause an infinite loop.  */
static void
check_text_len (s)
     char *s;
{
  if (s >= inline_buffer + MAX_INLINE_BUF_SIZE)
    {
      fprintf (stderr, "recompile c++ with larger MAX_INLINE_BUF_SIZE (%d)", MAX_INLINE_BUF_SIZE);
      abort ();
    }
}

tree
make_anon_parm_name ()
{
  char buf[32];

  sprintf (buf, ANON_PARMNAME_FORMAT, dummy_name++);
  return get_identifier (buf);
}

void
clear_anon_parm_name ()
{
  /* recycle these names.  */
  dummy_name = 0;
}

static void
dump_readonly_or_volatile (t)
     tree t;
{
  if (TREE_READONLY (t))
    OB_PUTS ("const ");
  if (TREE_VOLATILE (t))
    OB_PUTS ("volatile ");
}

static void
dump_type_prefix (t, p)
     tree t;
     int *p;
{
  int old_p = 0;
  int print_struct = 1;
  tree name;

  if (t == NULL_TREE)
    return;

  switch (TREE_CODE (t))
    {
    case ERROR_MARK:
      sprintf (inline_bufp, ANON_PARMNAME_FORMAT, dummy_name++);
      break;

    case UNKNOWN_TYPE:
      OB_PUTS ("<unknown type>");
      return;

    case TREE_LIST:
      dump_type (TREE_VALUE (t), &old_p);
      if (TREE_CHAIN (t))
	{
	  if (TREE_CHAIN (t) != void_list_node)
	    {
	      OB_PUTC (',');
	      dump_type (TREE_CHAIN (t), &old_p);
	    }
	}
      else OB_PUTS ("...");
      return;

    case POINTER_TYPE:
      *p += 1;
      dump_type_prefix (TREE_TYPE (t), p);
      while (*p)
	{
	  OB_PUTC ('*');
	  *p -= 1;
	}
      if (TREE_READONLY (t))
	OB_PUTS ("const ");
      if (TREE_VOLATILE (t))
	OB_PUTS ("volatile ");
      return;

    case OFFSET_TYPE:
      {
	tree type = TREE_TYPE (t);
	if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
	    type = TREE_TYPE (type);
	    if (in_parmlist)
	      OB_PUTS ("auto ");
	  }

	dump_type_prefix (type, &old_p);

	OB_PUTC ('(');
	dump_type (TYPE_OFFSET_BASETYPE (t), &old_p);
	OB_PUTC2 (':', ':');
	while (*p)
	  {
	    OB_PUTC ('*');
	    *p -= 1;
	  }
	if (TREE_READONLY (t) | TREE_VOLATILE (t))
	  dump_readonly_or_volatile (t);
	return;
      }

    case METHOD_TYPE:
      {
	tree type = TREE_TYPE (t);
	if (in_parmlist)
	  OB_PUTS ("auto ");

	dump_type_prefix (type, &old_p);

	OB_PUTC ('(');
	dump_type (TYPE_METHOD_BASETYPE (t), &old_p);
	OB_PUTC2 (':', ':');
	while (*p)
	  {
	    OB_PUTC ('*');
	    *p -= 1;
	  }
	if (TREE_READONLY (t) | TREE_VOLATILE (t))
	  dump_readonly_or_volatile (t);
	return;
      }

    case REFERENCE_TYPE:
      dump_type_prefix (TREE_TYPE (t), p);
      OB_PUTC ('&');
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      return;

    case ARRAY_TYPE:
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      dump_type_prefix (TREE_TYPE (t), p);
      return;

    case FUNCTION_TYPE:
      if (in_parmlist)
	OB_PUTS ("auto ");
      dump_type_prefix (TREE_TYPE (t), &old_p);
      OB_PUTC ('(');
      while (*p)
	{
	  OB_PUTC ('*');
	  *p -= 1;
	}
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      return;

    case IDENTIFIER_NODE:
      sprintf (inline_bufp, "%s ", IDENTIFIER_POINTER (t));
      break;

    case RECORD_TYPE:
      if (TREE_READONLY (t))
	OB_PUTS ("const ");
      if (TREE_VOLATILE (t))
	OB_PUTS ("volatile ");
      if (TYPE_LANG_SPECIFIC (t) && CLASSTYPE_DECLARED_CLASS (t))
	print_struct = 0;
      name = TYPE_NAME (t);
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
      if (print_struct)
	sprintf (inline_bufp, "struct %s ", IDENTIFIER_POINTER (name));
      else
	sprintf (inline_bufp, "class %s ", IDENTIFIER_POINTER (name));
      break;

    case UNION_TYPE:
      if (TREE_READONLY (t))
	OB_PUTS ("const ");
      if (TREE_VOLATILE (t))
	OB_PUTS ("volatile ");
      name = TYPE_NAME (t);
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
      sprintf (inline_bufp, "union %s ", IDENTIFIER_POINTER (name));
      break;

    case ENUMERAL_TYPE:
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      name = TYPE_NAME (t);
      if (TREE_CODE (name) == TYPE_DECL)
	name = DECL_NAME (name);
      sprintf (inline_bufp, "enum %s ", IDENTIFIER_POINTER (name));
      break;

    case TYPE_DECL:
      if (TREE_READONLY (t))
	OB_PUTS ("const ");
      if (TREE_VOLATILE (t))
	OB_PUTS ("volatile ");
      sprintf (inline_bufp, "%s ", IDENTIFIER_POINTER (DECL_NAME (t)));
      break;

    case INTEGER_TYPE:
      /* Normally, `unsigned' is part of the deal.  Not so if it comes
	 with `const' or `volatile'.  */
      if (TREE_UNSIGNED (t)
	  && (TREE_READONLY (t) || TREE_VOLATILE (t)))
	OB_PUTS ("unsigned ");
      /* fall through.  */
    case REAL_TYPE:
    case VOID_TYPE:
      if (TREE_READONLY (t))
	OB_PUTS ("const ");
      if (TREE_VOLATILE (t))
	OB_PUTS ("volatile ");
      sprintf (inline_bufp, "%s ", TYPE_NAME_STRING (t));
      break;

    default:
      abort ();
    }
  inline_bufp = new_text_len (inline_bufp);
}

static void
dump_type_suffix (t, p)
     tree t;
     int *p;
{
  int old_p = 0;

  if (t == NULL_TREE)
    return;

  switch (TREE_CODE (t))
    {
    case ERROR_MARK:
      sprintf (inline_bufp, ANON_PARMNAME_FORMAT, dummy_name++);
      break;

    case UNKNOWN_TYPE:
      return;

    case POINTER_TYPE:
      dump_type_suffix (TREE_TYPE (t), p);
      return;

    case OFFSET_TYPE:
      {
	tree type = TREE_TYPE (t);

	OB_PUTC (')');
	if (TREE_CODE (type) == FUNCTION_TYPE)
	  {
#if 0
	    tree next_arg = TREE_CHAIN (TYPE_ARG_TYPES (type));
	    OB_PUTC ('(');
	    if (next_arg)
	      {
		if (next_arg != void_list_node)
		  {
		    in_parmlist++;
		    dump_type (next_arg, &old_p);
		    in_parmlist--;
		  }
	      }
	    else OB_PUTS ("...");
	    OB_PUTC (')');
	    dump_type_suffix (TREE_TYPE (type), p);
#else
	    abort ();
#endif
	  }
	return;
      }

    case METHOD_TYPE:
      {
	tree next_arg;
	OB_PUTC (')');
	next_arg = TREE_CHAIN (TYPE_ARG_TYPES (t));
	OB_PUTC ('(');
	if (next_arg)
	  {
	    if (next_arg != void_list_node)
	      {
		in_parmlist++;
		dump_type (next_arg, &old_p);
		in_parmlist--;
	      }
	  }
	else OB_PUTS ("...");
	OB_PUTC (')');
	dump_readonly_or_volatile (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (t))));
	dump_type_suffix (TREE_TYPE (t), p);
	return;
      }

    case REFERENCE_TYPE:
      dump_type_suffix (TREE_TYPE (t), p);
      return;

    case ARRAY_TYPE:
      dump_type_suffix (TREE_TYPE (t), p);
      OB_PUTC2 ('[', ']');
      return;

    case FUNCTION_TYPE:
      OB_PUTC2 (')', '(');
      if (TYPE_ARG_TYPES (t) && TYPE_ARG_TYPES (t) != void_list_node)
	{
	  in_parmlist++;
	  dump_type (TYPE_ARG_TYPES (t), &old_p);
	  in_parmlist--;
	}
      OB_PUTC (')');
      dump_type_suffix (TREE_TYPE (t), p);
      return;

    case IDENTIFIER_NODE:
    case RECORD_TYPE:
    case UNION_TYPE:
    case ENUMERAL_TYPE:
    case TYPE_DECL:
    case INTEGER_TYPE:
    case REAL_TYPE:
    case VOID_TYPE:
      return;

    default:
      abort ();
    }
  inline_bufp = new_text_len (inline_bufp);
}

static void
dump_type (t, p)
     tree t;
     int *p;
{
  int old_p = 0;
  int print_struct = 1;

  if (t == NULL_TREE)
    return;

  switch (TREE_CODE (t))
    {
    case ERROR_MARK:
      sprintf (inline_bufp, ANON_PARMNAME_FORMAT, dummy_name++);
      break;

    case UNKNOWN_TYPE:
      OB_PUTS ("<unknown type>");
      return;

    case TREE_LIST:
      dump_type (TREE_VALUE (t), &old_p);
      if (TREE_CHAIN (t))
	{
	  if (TREE_CHAIN (t) != void_list_node)
	    {
	      OB_PUTC (',');
	      dump_type (TREE_CHAIN (t), &old_p);
	    }
	}
      else OB_PUTS ("...");
      return;

    case POINTER_TYPE:
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      *p += 1;
      dump_type (TREE_TYPE (t), p);
      while (*p)
	{
	  OB_PUTC ('*');
	  *p -= 1;
	}
      return;

    case REFERENCE_TYPE:
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      dump_type (TREE_TYPE (t), p);
      OB_PUTC ('&');
      return;

    case ARRAY_TYPE:
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      dump_type (TREE_TYPE (t), p);
      OB_PUTC2 ('[', ']');
      return;

    case OFFSET_TYPE:
    case METHOD_TYPE:
    case FUNCTION_TYPE:
      dump_type_prefix (t, p);
      dump_type_suffix (t, p);
      return;

    case IDENTIFIER_NODE:
      sprintf (inline_bufp, "%s ", IDENTIFIER_POINTER (t));
      break;

    case RECORD_TYPE:
      {
	if (TREE_READONLY (t) | TREE_VOLATILE (t))
	  dump_readonly_or_volatile (t);
	if (TYPE_LANG_SPECIFIC (t) && CLASSTYPE_DECLARED_CLASS (t))
	  print_struct = 0;
	t = TYPE_NAME (t);
	if (TREE_CODE (t) == TYPE_DECL)
	  t = DECL_NAME (t);
	if (print_struct)
	  sprintf (inline_bufp, "struct %s ", IDENTIFIER_POINTER (t));
	else
	  sprintf (inline_bufp, "class %s ", IDENTIFIER_POINTER (t));
	break;
      }

    case UNION_TYPE:
      {
	if (TREE_READONLY (t) | TREE_VOLATILE (t))
	  dump_readonly_or_volatile (t);
	t = TYPE_NAME (t);
	if (TREE_CODE (t) == TYPE_DECL)
	  t = DECL_NAME (t);
	sprintf (inline_bufp, "union %s ", IDENTIFIER_POINTER (t));
      }
      break;

    case ENUMERAL_TYPE:
      {
	if (TREE_READONLY (t) | TREE_VOLATILE (t))
	  dump_readonly_or_volatile (t);
	t = TYPE_NAME (t);
	if (TREE_CODE (t) == TYPE_DECL)
	  t = DECL_NAME (t);
	sprintf (inline_bufp, "enum %s ", IDENTIFIER_POINTER (t));
      }
      break;

    case TYPE_DECL:
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      sprintf (inline_bufp, "%s ", IDENTIFIER_POINTER (DECL_NAME (t)));
      break;

    case INTEGER_TYPE:
      /* Normally, `unsigned' is part of the deal.  Not so if it comes
	 with `const' or `volatile'.  */
      if (TREE_READONLY (t) | TREE_VOLATILE (t))
	dump_readonly_or_volatile (t);
      if (TREE_UNSIGNED (t)
	  && (TREE_READONLY (t) | TREE_VOLATILE (t)))
	OB_PUTS ("unsigned ");
      /* fall through.  */
    case REAL_TYPE:
    case VOID_TYPE:
      sprintf (inline_bufp, "%s ", TYPE_NAME_STRING (t));
      break;

    default:
      abort ();
    }
  inline_bufp = new_text_len (inline_bufp);
}

static void
dump_decl (t)
     tree t;
{
  int p = 0;

  if (t == NULL_TREE)
    return;

  switch (TREE_CODE (t))
    {
    case ERROR_MARK:
      strcpy (inline_bufp, " /* decl error */ ");
      break;

    case PARM_DECL:
      dump_type_prefix (TREE_TYPE (t), &p);
      if (DECL_NAME (t))
	dump_decl (DECL_NAME (t));
      else
	{
	  sprintf (inline_bufp, ANON_PARMNAME_FORMAT, dummy_name++);
	  break;
	}
      dump_type_suffix (TREE_TYPE (t), &p);
      return;

    case CALL_EXPR:
      dump_decl (TREE_OPERAND (t, 0));
      OB_PUTC ('(');
      in_parmlist++;
      dump_decl (TREE_OPERAND (t, 1));
      in_parmlist--;
      t = tree_last (TYPE_ARG_TYPES (TREE_TYPE (t)));
      if (!t || t != void_list_node)
	OB_PUTS ("...");
      OB_PUTC (')');
      return;

    case ARRAY_REF:
      dump_decl (TREE_OPERAND (t, 0));
      OB_PUTC ('[');
      dump_decl (TREE_OPERAND (t, 1));
      OB_PUTC (']');
      return;

    case TYPE_DECL:
      sprintf (inline_bufp, "%s ", IDENTIFIER_POINTER (DECL_NAME (t)));
      break;

    case TYPE_EXPR:
      abort ();
      break;

    case IDENTIFIER_NODE:
      if (OPERATOR_NAME_P (t))
	sprintf (inline_bufp, "operator %s ", operator_name_string (t));
      else if (OPERATOR_TYPENAME_P (t))
	{
	  OB_PUTS ("operator ");
	  dump_type (TREE_TYPE (t), &p);
	  return;
	}
      else
	sprintf (inline_bufp, "%s ", IDENTIFIER_POINTER (t));
      break;

    case BIT_NOT_EXPR:
      OB_PUTC2 ('~', ' ');
      dump_decl (TREE_OPERAND (t, 0));
      return;

    case SCOPE_REF:
      sprintf (inline_bufp, "%s :: ", IDENTIFIER_POINTER (TREE_OPERAND (t, 0)));
      inline_bufp += sizeof ("%s :: ") + IDENTIFIER_LENGTH (TREE_OPERAND (t, 0));
      dump_decl (TREE_OPERAND (t, 1));
      return;

    case INDIRECT_REF:
      OB_PUTC ('*');
      dump_decl (TREE_OPERAND (t, 0));
      return;

    case ADDR_EXPR:
      OB_PUTC ('&');
      dump_decl (TREE_OPERAND (t, 0));
      return;

    default:
      abort ();
    }
  inline_bufp = new_text_len (inline_bufp);
}

static void
dump_init_list (l)
     tree l;
{
  while (l)
    {
      dump_init (TREE_VALUE (l));
      if (TREE_CHAIN (l))
	OB_PUTC (',');
      l = TREE_CHAIN (l);
    }
}

static void
dump_init (t)
     tree t;
{
  int dummy;

  switch (TREE_CODE (t))
    {
    case VAR_DECL:
    case PARM_DECL:
      sprintf (inline_bufp, " %s ", IDENTIFIER_POINTER (DECL_NAME (t)));
      break;

    case FUNCTION_DECL:
      {
	tree name = DECL_NAME (t);

	if (DESTRUCTOR_NAME_P (name))
	  sprintf (inline_bufp, " ~%s ",
		   IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (t)));
	else if (OPERATOR_NAME_P (name))
	  sprintf (inline_bufp, "operator %s ", operator_name_string (name));
	else if (OPERATOR_TYPENAME_P (name))
	  {
	    dummy = 0;
	    OB_PUTS ("operator ");
	    dump_type (TREE_TYPE (name), &dummy);
	  }
#if 0
	else if (WRAPPER_NAME_P (name))
	  sprintf (inline_bufp, " ()%s ",
		   IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (t)));
	else if (WRAPPER_PRED_NAME_P (name))
	  sprintf (inline_bufp, " ()?%s ",
		   IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (t)));
	else if (ANTI_WRAPPER_NAME_P (name))
	  sprintf (inline_bufp, " ~()%s ",
		   IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (t)));
#endif
	else sprintf (inline_bufp, " %s ",
		      IDENTIFIER_POINTER (DECL_ORIGINAL_NAME (t)));
      }
      break;

    case CONST_DECL:
      dummy = 0;
      OB_PUTC2 ('(', '(');
      dump_type (TREE_TYPE (t), &dummy);
      OB_PUTC (')');
      dump_init (DECL_INITIAL (t));
      OB_PUTC (')');
      return;

    case INTEGER_CST:
      sprintf (inline_bufp, " %d ", TREE_INT_CST_LOW (t));
      break;

    case REAL_CST:
      sprintf (inline_bufp, " %g ", TREE_REAL_CST (t));
      break;

    case STRING_CST:
      {
	char *p = TREE_STRING_POINTER (t);
	int len = TREE_STRING_LENGTH (t) - 1;
	int i;

	check_text_len (inline_bufp + len + 2);
	OB_PUTC ('\"');
	for (i = 0; i < len; i++)
	  {
	    register char c = p[i];
	    if (c == '\"' || c == '\\')
	      OB_PUTC ('\\');
	    if (c >= ' ' && c < 0177)
	      OB_PUTC (c);
	    else
	      {
		sprintf (inline_bufp, "\\%03o", c);
		inline_bufp = new_text_len (inline_bufp);
	      }
	  }
	OB_PUTC ('\"');
      }
      return;

    case COMPOUND_EXPR:
      dump_binary_op (",", t, 1);
      break;

    case COND_EXPR:
      OB_PUTC ('(');
      dump_init (TREE_OPERAND (t, 0));
      OB_PUTS (" ? ");
      dump_init (TREE_OPERAND (t, 1));
      OB_PUTS (" : ");
      dump_init (TREE_OPERAND (t, 2));
      OB_PUTC (')');
      return;

    case SAVE_EXPR:
      if (TREE_HAS_CONSTRUCTOR (t))
	{
	  dummy = 0;
	  OB_PUTS ("new ");
	  dump_type (TREE_TYPE (TREE_TYPE (t)), &dummy);
	  PARM_DECL_EXPR (t) = 1;
	}
      else
	{
	  sorry ("operand of SAVE_EXPR not understood");
	  *inline_errp = '\0';
	  inline_bufp = inline_errp + 1;
	}
      return;

    case NEW_EXPR:
      strcpy (inline_bufp, TYPE_NAME_STRING (TREE_TYPE (t)));
      inline_bufp = new_text_len (inline_bufp);
      OB_PUTC ('(');
      dump_init_list (TREE_CHAIN (TREE_OPERAND (t, 1)));
      OB_PUTC (')');
      return;

    case CALL_EXPR:
      OB_PUTC ('(');
      dump_init (TREE_OPERAND (t, 0));
      dump_init_list (TREE_OPERAND (t, 1));
      OB_PUTC (')');
      return;

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
      dump_binary_op (opname_tab[(int) TREE_CODE (t)], t,
		      strlen (opname_tab[(int) TREE_CODE (t)]));
      return;

    case CEIL_DIV_EXPR:
    case FLOOR_DIV_EXPR:
    case ROUND_DIV_EXPR:
      dump_binary_op ("/", t, 1);
      return;

    case CEIL_MOD_EXPR:
    case FLOOR_MOD_EXPR:
    case ROUND_MOD_EXPR:
      dump_binary_op ("%", t, 1);
      return;

    case COMPONENT_REF:
      dump_binary_op (".", t, 1);
      return;

    case CONVERT_EXPR:
      dump_unary_op ("+", t, 1);
      return;

    case ADDR_EXPR:
      if (TREE_CODE (TREE_OPERAND (t, 0)) == FUNCTION_DECL
	  || TREE_CODE (TREE_OPERAND (t, 0)) == STRING_CST)
	dump_init (TREE_OPERAND (t, 0));
      else
	dump_unary_op ("&", t, 1);
      return;

    case INDIRECT_REF:
      dump_unary_op ("*", t, 1);
      return;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
    case TRUTH_NOT_EXPR:
    case PREDECREMENT_EXPR:
    case PREINCREMENT_EXPR:
      dump_unary_op (opname_tab [(int)TREE_CODE (t)], t,
		     strlen (opname_tab[(int) TREE_CODE (t)]));
      return;

    case POSTDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
      OB_PUTC ('(');
      dump_init (TREE_OPERAND (t, 0));
      OB_PUTCP (opname_tab[(int)TREE_CODE (t)]);
      OB_PUTC (')');
      return;

    case NOP_EXPR:
      dummy = 0;
      OB_PUTC2 ('(', '(');
      dump_type (TREE_TYPE (t), &dummy);
      OB_PUTC (')');
      dump_init (TREE_OPERAND (t, 0));
      OB_PUTC (')');
      return;

    case CONSTRUCTOR:
      OB_PUTC ('{');
      dump_init_list (CONSTRUCTOR_ELTS (t));
      OB_PUTC ('}');
      return;

      /*  This list is incomplete, but should suffice for now.
	  It is very important that `sorry' does not call
	  `report_error_function'.  That could cause an infinite loop.  */
    default:
      sorry ("that operation not supported for default parameters");

      /* fall through to ERROR_MARK...  */
    case ERROR_MARK:
      *inline_errp = '\0';
      inline_bufp = inline_errp + 1;
      return;
    }
  inline_bufp = new_text_len (inline_bufp);
}

static void
dump_binary_op (opstring, t, len)
     char *opstring;
     tree t;
     int len;
{
  OB_PUTC ('(');
  dump_init (TREE_OPERAND (t, 0));
  sprintf (inline_bufp, " %s ", opstring);
  inline_bufp += len + 2;
  dump_init (TREE_OPERAND (t, 1));
  OB_PUTC (')');
  check_text_len (inline_bufp);
}

static void
dump_unary_op (opstring, t, len)
     char *opstring;
     tree t;
     int len;
{
  OB_PUTC ('(');
  sprintf (inline_bufp, " %s ", opstring);
  inline_bufp += len + 2;
  dump_init (TREE_OPERAND (t, 0));
  OB_PUTC (')');
  check_text_len (inline_bufp);
}

#ifdef DO_METHODS_THE_OLD_WAY
/* Process the currently pending inline function definitions.
   This entails:
   (1) Creating a temporary file which contains return type,
       delarator name, and argment names and types of the
       function to be inlined.
   (2) Reading that file into a buffer which can then be
       made to look line another piece of inline code to
       process, stuffing that on the top of the inline
       stack, then letting the lexer and parser read from those
       two.
*/

static struct pending_inline *
stash_inline_prefix (cname, field)
     tree cname, field;
{
  extern int lineno;
  struct pending_inline *t;
  tree name, fndecl, fntype;
  int p = 0;
  inline_buffer = (char *)alloca (MAX_INLINE_BUF_SIZE + 4);
  dummy_name = 0;

  name = DECL_ORIGINAL_NAME (field);
  /* We still don't do friends right.  */
  fndecl = field;
  fntype = TREE_TYPE (fndecl);

  if (TREE_INLINE (fndecl))
    strcpy (inline_buffer, "inline ");
  else
    strcpy (inline_buffer, "static ");
  inline_bufp = inline_buffer + strlen (inline_buffer);
  if (! OPERATOR_TYPENAME_P (name))
    dump_type_prefix (TREE_TYPE (fntype), &p);
  if (TREE_CODE (fntype) == METHOD_TYPE)
    {
      dump_type (cname, &p);
      inline_bufp[-1] = ':';
      *inline_bufp++ = ':';
      if (DESTRUCTOR_NAME_P (DECL_NAME (fndecl)))
	OB_PUTC ('~');
#if 0
      else if (WRAPPER_NAME_P (DECL_NAME (fndecl)))
	OB_PUTC2 ('(', ')');
      else if (WRAPPER_PRED_NAME_P (DECL_NAME (fndecl)))
	OB_PUTS ("()?");
      else if (ANTI_WRAPPER_NAME_P (DECL_NAME (fndecl)))
	OB_PUTS ("~()");
#endif
    }
  dump_decl (name);
  OB_PUTC ('(');
  if (! DESTRUCTOR_NAME_P (DECL_NAME (fndecl)))
    {
      tree parmlist = DECL_ARGUMENTS (fndecl);
      tree typelist = TYPE_ARG_TYPES (fntype);

      if (TREE_CODE (field) == FIELD_DECL)
	{
	  parmlist = TREE_CHAIN (parmlist);
	  typelist = TREE_CHAIN (typelist);
	}

      in_parmlist++;
      while (parmlist)
	{
	  dump_decl (parmlist);
#if 0
	  if (TREE_PURPOSE (typelist))
	    {
	      inline_errp = inline_bufp;
	      OB_PUTS (" = (");
	      dump_init (TREE_PURPOSE (typelist));
	      OB_PUTC (')');
	      if (*inline_errp == '\0')
		inline_bufp = inline_errp;
	    }
#endif
	  if (TREE_CHAIN (parmlist))
	    OB_PUTC (',');
	  parmlist = TREE_CHAIN (parmlist);
	  typelist = TREE_CHAIN (typelist);
	}
      in_parmlist--;
      if (!typelist || typelist != void_list_node)
	OB_PUTS ("...");
    }
  OB_PUTC (')');

  if (! OPERATOR_TYPENAME_P (name))
    dump_type_suffix (TREE_TYPE (fntype), &p);
  if (TREE_CODE (fntype) == METHOD_TYPE)
    dump_readonly_or_volatile (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fntype))));
  {
    extern tree value_identifier;

    if (DECL_RESULT (fndecl) != value_identifier)
      {
	tree result = DECL_RESULT (fndecl);

	OB_PUTS ("return ");
	OB_PUTS (IDENTIFIER_POINTER (DECL_NAME (result)));
	if (DECL_INITIAL (result))
	  {
	    OB_PUTC ('=');
	    dump_init (DECL_INITIAL (result));
	    OB_PUTC (';');
	  }
      }
  }
  OB_FINISH ();
  check_text_len (inline_bufp);

  t = (struct pending_inline *)xmalloc (sizeof (struct pending_inline));
  t->len = inline_bufp - inline_buffer;
  t->buf = (char *)xmalloc (t->len);
  bcopy (inline_buffer, t->buf, t->len);
  t->lineno = lineno;
  t->filename = input_filename;
  t->token = 0;
  return t;
}
#endif

#define OVERLOAD_MAX_LEN 1024

/* Pretty printing for announce_function.  If BUF is nonzero, then
   the text is written there.  The buffer is assued to be of size
   OVERLOAD_MAX_LEN.  CNAME is the name of the class that FNDECL
   belongs to, if we could not figure that out from FNDECL
   itself.  FNDECL is the declaration of the function we
   are interested in seeing.  PRINT_RET_TYPE_P is non-zero if
   we should print the type that this function returns.  */
char *
fndecl_as_string (buf, cname, fndecl, print_ret_type_p)
     char *buf;
     tree cname, fndecl;
     int print_ret_type_p;
{
  tree name = DECL_NAME (fndecl);
  tree fntype = TREE_TYPE (fndecl);
  tree parmtypes = TYPE_ARG_TYPES (fntype);
  int p = 0;
  int spaces = 0;

  inline_buffer = buf;
  OB_INIT ();

  if (DECL_STATIC_FUNCTION_P (fndecl))
    cname = TYPE_NAME (DECL_STATIC_CONTEXT (fndecl));
  else if (! cname && TREE_CODE (fntype) == METHOD_TYPE)
    cname = TYPE_NAME (TYPE_METHOD_BASETYPE (fntype));

  if (print_ret_type_p && ! OPERATOR_TYPENAME_P (name))
    dump_type_prefix (TREE_TYPE (fntype), &p);
  if (DECL_STATIC_FUNCTION_P (fndecl))
      OB_PUTS ("static ");
    
  if (cname)
    {
      dump_type (cname, &p);
      inline_bufp[-1] = ':';
      *inline_bufp++ = ':';
      if (TREE_CODE (fntype) == METHOD_TYPE && parmtypes)
	parmtypes = TREE_CHAIN (parmtypes);
      if (DECL_CONSTRUCTOR_FOR_VBASE_P (fndecl))
	parmtypes = TREE_CHAIN (parmtypes);
    }

  if (DESTRUCTOR_NAME_P (name))
    {
      OB_PUTC ('~');
      parmtypes = TREE_CHAIN (parmtypes);
      dump_decl (DECL_ORIGINAL_NAME (fndecl));
    }
  else if (OPERATOR_NAME_P (name))
    {
      sprintf (inline_bufp, "operator %s ", operator_name_string (name));
      inline_bufp += strlen (inline_bufp);
    }
  else if (OPERATOR_TYPENAME_P (name))
    {
      /* This cannot use the hack that the operator's return
	 type is stashed off of its name because it may be
	 used for error reporting.  In the case of conflicting
	 declarations, both will have the same name, yet
	 the types will be different, hence the TREE_TYPE field
	 of the first name will be clobbered by the second.  */
      OB_PUTS ("operator ");
      dump_type (TREE_TYPE (TREE_TYPE (fndecl)), &p);
    }
  else if (DECL_CONSTRUCTOR_P (fndecl))
    {
#ifdef SOS
      if (TYPE_DYNAMIC (TREE_TYPE (TREE_TYPE (DECL_ORIGINAL_NAME (fndecl)))))
	{
	  OB_PUTS ("dynamic ");
	  parmtypes = TREE_CHAIN (parmtypes);
	}
#endif
      dump_decl (DECL_ORIGINAL_NAME (fndecl));
      /* Skip past "in_charge" identifier.  */
      if (TYPE_USES_VIRTUAL_BASECLASSES (TREE_TYPE (cname)))
	parmtypes = TREE_CHAIN (parmtypes);
    }
  else
    {
#if 0
      if (WRAPPER_NAME_P (name))
	OB_PUTC2 ('(', ')');
      if (WRAPPER_PRED_NAME_P (name))
	OB_PUTS ("()?");
      else if (ANTI_WRAPPER_NAME_P (name))
	OB_PUTS ("~()");
#endif
      dump_decl (DECL_ORIGINAL_NAME (fndecl));
    }

  OB_PUTC ('(');
  if (parmtypes)
    {
      in_parmlist++;
      if (parmtypes != void_list_node)
	spaces = 2;
      while (parmtypes && parmtypes != void_list_node)
	{
	  dump_type (TREE_VALUE (parmtypes), &p);
	  while (inline_bufp[-1] == ' ')
	    inline_bufp--;
	  if (TREE_PURPOSE (parmtypes))
	    {
	      inline_errp = inline_bufp;
	      OB_PUTS (" (= ");
	      dump_init (TREE_PURPOSE (parmtypes));
	      OB_PUTC (')');
	    }
	  OB_PUTC2 (',', ' ');
	  parmtypes = TREE_CHAIN (parmtypes);
	}
      in_parmlist--;
    }
  
  if (parmtypes)
    inline_bufp -= spaces;
  else
    OB_PUTS ("...");

  OB_PUTC (')');

  if (print_ret_type_p && ! OPERATOR_TYPENAME_P (name))
    dump_type_suffix (TREE_TYPE (fntype), &p);

  if (TREE_CODE (fntype) == METHOD_TYPE)
    dump_readonly_or_volatile (TREE_TYPE (TREE_VALUE (TYPE_ARG_TYPES (fntype))));

  OB_FINISH ();
  check_text_len (inline_bufp);
  
  if (strlen (buf) >= OVERLOAD_MAX_LEN)
    {
      fprintf (stderr, "fndecl_as_string returns something too large");
      abort ();
    }
  return buf;
}

#ifdef FIELD_XREF

char *
type_as_string (buf, typ)
     char *buf;
     tree typ;
{
  int p = 0;
  int spaces = 0;

  inline_buffer = buf;
  OB_INIT ();

  dump_type(typ,&p);

  OB_FINISH ();

  return buf;
}

#endif

/* Move inline function defintions out of structure so that they
   can be processed normally.  CNAME is the name of the class
   we are working from, METHOD_LIST is the list of method lists
   of the structure.  We delete friend methods here, after
   saving away their inline function definitions (if any).  */

/* Subroutine of `do_inline_function_hair'.  */
static void
prepare_inline (cname, fndecl)
     tree cname, fndecl;
{
  if (DECL_PENDING_INLINE_INFO (fndecl))
    {
      struct pending_inline *t1, *t2;
      tree args;

      t2 = DECL_PENDING_INLINE_INFO (fndecl);
      t2->next = pending_inlines;
      t2->fndecl = fndecl;
      args = DECL_ARGUMENTS (fndecl);
      while (args)
	{
	  DECL_CONTEXT (args) = fndecl;
	  args = TREE_CHAIN (args);
	}
#ifdef DO_METHODS_THE_OLD_WAY
      t1 = stash_inline_prefix (cname, methods);
      t1->next = t2;
#else
      t1 = t2;
#endif
      pending_inlines = t1;

      /* Allow this decl to be seen in global scope */
      IDENTIFIER_GLOBAL_VALUE (DECL_NAME (fndecl)) = fndecl;
    }
}

void
do_inline_function_hair (type, friend_list)
     tree type, friend_list;
{
  tree cname = DECL_NAME (TYPE_NAME (type));
  tree method_vec = CLASSTYPE_METHOD_VEC (type);
  if (method_vec != 0)
    {
      tree *methods = &TREE_VEC_ELT (method_vec, 0);
      tree *end = TREE_VEC_END (method_vec);
      while (methods != end)
	{
	  /* Do inline member functions.  */
	  tree method = *methods;
	  while (method)
	    {
	      prepare_inline (cname, method);
	      method = TREE_CHAIN (method);
	    }
	  methods++;
	}
    }
  while (friend_list)
    {
      prepare_inline (NULL_TREE, TREE_VALUE (friend_list));
      friend_list = TREE_CHAIN (friend_list);
    }
}

/* Report a argument type mismatch between the best declared function
   we could find and the current argument list that we have.  */
void
report_type_mismatch (cp, parmtypes, name_kind, err_name)
     struct candidate *cp;
     tree parmtypes;
     char *name_kind, *err_name;
{
  char buf[OVERLOAD_MAX_LEN];
  int i = cp->u.bad_arg;
  tree ttf, tta;

  if (i == -3)
    {
      if (TREE_READONLY (TREE_TYPE (TREE_VALUE (parmtypes))))
	error ("call to const %s `%s' with non-const object", name_kind, err_name);
      else
	error ("call to non-const %s `%s' with const object", name_kind, err_name);
      return;
    }
  if (i == -2)
    {
      error ("too few arguments for %s `%s'", name_kind, err_name);
      return;
    }
  else if (i == -1)
    {
      error ("too many arguments for %s `%s'", name_kind, err_name);
      return;
    }
  if (i == 0)
    {
      if (TREE_CODE (TREE_TYPE (cp->function)) == METHOD_TYPE)
	{
	  /* Happens when we have an ambiguous base class.  */
	  assert (get_base_type (DECL_CONTEXT (cp->function), TREE_TYPE (TREE_TYPE (TREE_VALUE (parmtypes))), 1) == error_mark_node);
	  return;
	}
    }
  ttf = TYPE_ARG_TYPES (TREE_TYPE (cp->function));
  tta = parmtypes;

  while (i-- > 0)
    {
      ttf = TREE_CHAIN (ttf);
      tta = TREE_CHAIN (tta);
    }
  fndecl_as_string (buf, 0, cp->function, 0);
  inline_bufp = inline_buffer + strlen (inline_buffer) + 1;
  inline_buffer = inline_bufp;

  /* Reset `i' so that type printing routines do the right thing.  */
  if (tta)
    {
      enum tree_code code = TREE_CODE (TREE_TYPE (TREE_VALUE (tta)));
      if (code == ERROR_MARK)
	OB_PUTS ("(failed type instatiation)");
      else
	{
	  i = (code == FUNCTION_TYPE || code == METHOD_TYPE);
	  dump_type (TREE_TYPE (TREE_VALUE (tta)), &i);
	}
    }
  else OB_PUTS ("void");

  OB_FINISH ();
  sprintf (inline_bufp, "bad argument %d for function `%s' (type was %s)",
	   cp->u.bad_arg - (TREE_CODE (TREE_TYPE (cp->function)) == METHOD_TYPE), buf, inline_buffer);
  strcpy (buf, inline_bufp);
  error (buf);
}

/* Here is where overload code starts.  */

#define OVERLOAD_MAX_LEN 1024

/* Array of types seen so far in top-level call to `build_overload_name'.
   Allocated and deallocated by caller.  */
static tree *typevec;

/* Number of types interned by `build_overload_name' so far.  */
static int maxtype;

/* Number of occurances of last type seen.  */
static int nrepeats;

/* Nonzero if we should not try folding parameter types.  */
static int nofold;

#define ALLOCATE_TYPEVEC(PARMTYPES) \
  do { maxtype = 0, nrepeats = 0; \
       typevec = (tree *)alloca (list_length (PARMTYPES) * sizeof (tree)); } while (0)

#define DEALLOCATE_TYPEVEC(PARMTYPES) \
  do { tree t = (PARMTYPES); \
       while (t) { TREE_USED (TREE_VALUE (t)) = 0; t = TREE_CHAIN (t); } \
  } while (0)

/* Code to concatenate an asciified integer to a string,
   and return the end of the string.  */
static
#ifdef __GNUC__
__inline
#endif
char *
icat (s, i)
     char *s;
     int i;
{
  if (i < 10)
    {
      *s++ = '0' + i;
      return s;
    }
  s = icat (s, i / 10);
  *s++ = '0' + (i % 10);
  return s;
}

static
#ifdef __GNUC__
__inline
#endif
char *
flush_repeats (s, type)
     char *s;
     tree type;
{
  int tindex = 0;
  char *rval;

  while (typevec[tindex] != type)
    tindex++;

  if (nrepeats > 1)
    {
      *s++ = 'N';
      s = icat (s, nrepeats);
      if (nrepeats > 9)
	*s++ = '_';
    }
  else
    *s++ = 'T';
  nrepeats = 0;
  rval = icat (s, tindex);
  if (tindex > 9)
    *rval++ = '_';
  return rval;
}

/* Given a list of parameters in PARMS, and a buffer in TEXT, of
   length LEN bytes, create an unambiguous overload string. Should
   distinguish any type that C (or C++) can distinguish. I.e.,
   pointers to functions are treated correctly.

   Caller must deal with whether a final `e' goes on the end or not.

   Any default conversions must take place before this function
   is called.  */

static char *
build_overload_name (parmtypes, text, text_end)
     tree parmtypes;
     char *text, *text_end;
{
  char *textp = text;
  int just_one;
  tree parmtype;

  if (just_one = (TREE_CODE (parmtypes) != TREE_LIST))
    {
      parmtype = parmtypes;
      goto only_one;
    }

  while (parmtypes)
    {
      if (text_end - text < 4)
	fatal ("Out of string space in build_overload_name!");
      parmtype = TREE_VALUE (parmtypes);

    only_one:

      if (! nofold)
	{
	  if (! just_one)
	    /* Every argument gets counted.  */
	    typevec[maxtype++] = parmtype;

	  if (TREE_USED (parmtype))
	    {
	      if (! just_one && parmtype == typevec[maxtype-2])
		nrepeats++;
	      else
		{
		  if (nrepeats)
		    textp = flush_repeats (textp, parmtype);
		  if (! just_one && TREE_CHAIN (parmtypes)
		      && parmtype == TREE_VALUE (TREE_CHAIN (parmtypes)))
		    nrepeats++;
		  else
		    {
		      int tindex = 0;

		      while (typevec[tindex] != parmtype)
			tindex++;
		      *textp++ = 'T';
		      textp = icat (textp, tindex);
		      if (tindex > 9)
			*textp++ = '_';
		    }
		}
	      goto next;
	    }
	  if (nrepeats)
	    textp = flush_repeats (textp, typevec[maxtype-2]);
	  if (! just_one
	      /* Only cache types which take more than one character.  */
	      && (parmtype != TYPE_MAIN_VARIANT (parmtype)
		  || (TREE_CODE (parmtype) != INTEGER_TYPE
		      && TREE_CODE (parmtype) != REAL_TYPE)))
	    TREE_USED (parmtype) = 1;
	}

      if (TREE_READONLY (parmtype))
	*textp++ = 'C';
      if (TREE_CODE (parmtype) == INTEGER_TYPE && TREE_UNSIGNED (parmtype))
	*textp++ = 'U';
      if (TREE_VOLATILE (parmtype))
	*textp++ = 'V';

      switch (TREE_CODE (parmtype))
	{
	case OFFSET_TYPE:
	  *textp++ = 'O';
	  textp = build_overload_name (TYPE_OFFSET_BASETYPE (parmtype), textp, text_end);
	  *textp++ = '_';
	  textp = build_overload_name (TREE_TYPE (parmtype), textp, text_end);
	  break;

	case REFERENCE_TYPE:
	  *textp++ = 'R';
	  goto more;

	case ARRAY_TYPE:
#ifdef PARM_CAN_BE_ARRAY_TYPE
	  {
	    tree length;

	    *textp++ = 'A';
	    length = array_type_nelts (parmtype);
	    if (TREE_CODE (length) == INTEGER_CST)
	      textp = icat (textp, TREE_INT_CST_LOW (length));
	    *textp++ = '_';
	    goto more;
	  }
#else
	  *textp++ = 'P';
	  goto more;
#endif

	case POINTER_TYPE:
	  *textp++ = 'P';
	more:
	  textp = build_overload_name (TREE_TYPE (parmtype), textp, text_end);
	  break;

	case FUNCTION_TYPE:
	case METHOD_TYPE:
	  {
	    tree firstarg = TYPE_ARG_TYPES (parmtype);
	    /* Otherwise have to implement reentrant typevecs,
	       unmark and remark types, etc.  */
	    int old_nofold = nofold;
	    nofold = 1;

	    if (nrepeats)
	      textp = flush_repeats (textp, typevec[maxtype-1]);

	    /* @@ It may be possible to pass a function type in
	       which is not preceded by a 'P'.  */
	    if (TREE_CODE (parmtype) == FUNCTION_TYPE)
	      {
		*textp++ = 'F';
		if (firstarg == NULL_TREE)
		  *textp++ = 'e';
		else if (firstarg == void_list_node)
		  *textp++ = 'v';
		else
		  textp = build_overload_name (firstarg, textp, text_end);
	      }
	    else
	      {
		int constp = TREE_READONLY (TREE_TYPE (TREE_VALUE (firstarg)));
		int volatilep = TREE_VOLATILE (TREE_TYPE (TREE_VALUE (firstarg)));
		*textp++ = 'M';
		firstarg = TREE_CHAIN (firstarg);

		textp = build_overload_name (TYPE_METHOD_BASETYPE (parmtype), textp, text_end);
		if (constp)
		  *textp++ = 'C';
		if (volatilep)
		  *textp++ = 'V';

		/* For cfront 2.0 compatability.  */
		*textp++ = 'F';

		if (firstarg == NULL_TREE)
		  *textp++ = 'e';
		else if (firstarg == void_list_node)
		  *textp++ = 'v';
		else
		  textp = build_overload_name (firstarg, textp, text_end);
	      }

	    /* Separate args from return type.  */
	    *textp++ = '_';
	    textp = build_overload_name (TREE_TYPE (parmtype), textp, text_end);
	    nofold = old_nofold;
	    break;
	  }

	case INTEGER_TYPE:
	  parmtype = TYPE_MAIN_VARIANT (parmtype);
	  switch (TYPE_MODE (parmtype))
	    {
	    case TImode:
	      if (parmtype == long_integer_type_node
		  || parmtype == long_unsigned_type_node)
		*textp++ = 'l';
	      else
		*textp++ = 'q';
	      break;
	    case DImode:
	      if (parmtype == long_integer_type_node
		  || parmtype == long_unsigned_type_node)
		*textp++ = 'l';
	      else if (parmtype == integer_type_node
		       || parmtype == unsigned_type_node)
		*textp++ = 'i';
	      else if (parmtype == short_integer_type_node
		       || parmtype == short_unsigned_type_node)
		*textp++ = 's';
	      else
		*textp++ = 'x';
	      break;
	    case SImode:
	      if (parmtype == long_integer_type_node
		  || parmtype == long_unsigned_type_node)
		*textp++ = 'l';
	      else if (parmtype == short_integer_type_node
		       || parmtype == short_unsigned_type_node)
		*textp++ = 's';
	      else
		*textp++ = 'i';
	      break;
	    case HImode:
	      if (parmtype == integer_type_node
		  || parmtype == unsigned_type_node)
		*textp++ = 'i';
	      else
		*textp++ = 's';
	      break;
	    case QImode:
	      *textp++ = 'c';
	      break;
	    default:
	      abort ();
	    }
	  break;

	case REAL_TYPE:
	  parmtype = TYPE_MAIN_VARIANT (parmtype);
	  if (parmtype == long_double_type_node)
	    *textp++ = 'r';
	  else if (parmtype == double_type_node)
	    *textp++ = 'd';
	  else if (parmtype == float_type_node)
	    *textp++ = 'f';
	  else abort ();
	  break;

	case VOID_TYPE:
	  if (! just_one)
	    {
#if 0
	      extern tree void_list_node;

	      /* See if anybody is wasting memory.  */
	      assert (parmtypes == void_list_node);
#endif
	      /* This is the end of a parameter list.  */
	      *textp = '\0';
	      return textp;
	    }
	  *textp++ = 'v';
	  break;

	case ERROR_MARK:	/* not right, but nothing is anyway */
	  break;

	  /* have to do these */
	case UNION_TYPE:
	case RECORD_TYPE:
	  if (! just_one)
	    /* Make this type signature look incompatible
	       with AT&T.  */
	    *textp++ = 'G';
	  goto common;
	case ENUMERAL_TYPE:
	common:
	  {
	    tree name = TYPE_NAME (parmtype);
	    if (TREE_CODE (name) == TYPE_DECL)
	      name = DECL_NAME (name);
	    assert (TREE_CODE (name) == IDENTIFIER_NODE);
	    textp = icat (textp, IDENTIFIER_LENGTH (name));
	    strcpy (textp, IDENTIFIER_POINTER (name));
	    textp += IDENTIFIER_LENGTH (name);
	    break;
	  }

	case UNKNOWN_TYPE:
	  /* This will take some work.  */
	  *textp++ = '?';
	  break;

	default:
	  abort ();
	}

    next:
      if (just_one) break;
      parmtypes = TREE_CHAIN (parmtypes);
    }
  if (! just_one)
    {
      if (nrepeats)
	textp = flush_repeats (textp, typevec[maxtype-1]);

      /* To get here, parms must end with `...'. */
      *textp++ = 'e';
    }

  *textp = '\0';
  return textp;
}

/* Change the name of a function definition so that it may be
   overloaded. NAME is the name of the function to overload,
   PARMS is the parameter list (which determines what name the
   final function obtains).

   FOR_METHOD is 1 if this overload is being performed
   for a method, rather than a function type.  It is 2 if
   this overload is being performed for a constructor.  */
tree
build_decl_overload (name, parms, for_method)
     char *name;
     tree parms;
     int for_method;
{
  int tmp;
  char tname[OVERLOAD_MAX_LEN];

  if (for_method == 2)
    /* We can divine that this is a constructor,
       and figure out its name without any extra encoding.  */
    tmp = 0;
  else
    {
      strcpy (tname, name);
      tmp = strlen (tname);
    }
  tname[tmp++] = '_';
  tname[tmp++] = '_';
  if (for_method)
    {
#if 0
      /* We can get away without doing this.  */
      tname[tmp++] = 'M';
#endif
      parms = temp_tree_cons (NULL_TREE, TREE_TYPE (TREE_VALUE (parms)), TREE_CHAIN (parms));
    }
  else
    tname[tmp++] = 'F';

  if (parms == NULL_TREE)
    tname[tmp++] = 'e', tname[tmp] = '\0';
  else if (parms == void_list_node)
    tname[tmp++] = 'v', tname[tmp] = '\0';
  else
    {
      ALLOCATE_TYPEVEC (parms);
      nofold = 0;
      if (for_method)
	{
	  tmp = build_overload_name (TREE_VALUE (parms), tname+tmp, &tname[OVERLOAD_MAX_LEN]) - tname;

#ifndef LONGERNAMES
	  typevec[maxtype++] = TREE_VALUE (parms);
	  TREE_USED (TREE_VALUE (parms)) = 1;
#endif

	  if (TREE_CHAIN (parms))
	    build_overload_name (TREE_CHAIN (parms), tname+tmp, &tname[OVERLOAD_MAX_LEN]);
	  else
	    {
	      tname[tmp++] = 'e';
	      tname[tmp] = '\0';
	    }
	}
      else
	build_overload_name (parms, tname+tmp, &tname[OVERLOAD_MAX_LEN]);
      DEALLOCATE_TYPEVEC (parms);
    }
  return get_identifier (tname);
}

/* Build an overload name for the type expression TYPE.  */
tree
build_typename_overload (type)
     tree type;
{
  char tname[OVERLOAD_MAX_LEN];
  int i = sizeof (OPERATOR_TYPENAME_FORMAT) - 1;
  sprintf (tname, OPERATOR_TYPENAME_FORMAT);
#if 0
  /* We can get away without doing this--it really gets
     overloaded later.  */
  tname[i++] = '_';
  tname[i++] = '_';
  tname[i++] = 'M';
#endif
  nofold = 1;
  build_overload_name (type, tname + i, &tname[OVERLOAD_MAX_LEN]);
  return get_identifier (tname);
}

/* Top-level interface to explicit overload requests. Allow NAME
   to be overloaded. Error if NAME is already declared for the current
   scope. Warning if function is redundanly overloaded. */

void
declare_overloaded (name)
     tree name;
{
#ifdef NO_AUTO_OVERLOAD
  if (is_overloaded (name))
    warning ("function `%s' already declared overloaded",
	     IDENTIFIER_POINTER (name));
  else if (IDENTIFIER_GLOBAL_VALUE (name))
    error ("overloading function `%s' that is already defined",
	   IDENTIFIER_POINTER (name));
  else
    {
      TREE_OVERLOADED (name) = 1;
      IDENTIFIER_GLOBAL_VALUE (name) = build_tree_list (name, NULL_TREE);
      TREE_TYPE (IDENTIFIER_GLOBAL_VALUE (name)) = unknown_type_node;
    }
#else
  if (current_lang_name == lang_name_cplusplus)
    {
      if (0)
	warning ("functions are implicitly overloaded in C++");
    }
  else if (current_lang_name == lang_name_c)
    error ("overloading function `%s' cannot be done in C language context");
  else
    abort ();
#endif
}

#ifdef NO_AUTO_OVERLOAD
/* Check to see if NAME is overloaded. For first approximation,
   check to see if its TREE_OVERLOADED is set.  This is used on
   IDENTIFIER nodes.  */
int
is_overloaded (name)
     tree name;
{
  /* @@ */
  return (TREE_OVERLOADED (name)
	  && (! IDENTIFIER_CLASS_VALUE (name) || current_class_type == 0)
	  && ! IDENTIFIER_LOCAL_VALUE (name));
}
#endif

/* Given a tree_code CODE, and some arguments (at least one),
   attempt to use an overloaded operator on the arguments.

   For unary operators, only the first argument need be checked.
   For binary operators, both arguments may need to be checked.

   Member functions can convert class references to class pointers,
   for one-level deep indirection.  More than that is not supported.
   Operators [](), ()(), and ->() must be member functions.

   We call function call building calls with nonzero complain if
   they are our only hope.  This is true when we see a vanilla operator
   applied to something of aggregate type.  If this fails, we are free to
   return `error_mark_node', because we will have reported the error.

   Operators NEW and DELETE overload in funny ways: operator new takes
   a single `size' parameter, and operator delete takes a pointer to the
   storage being deleted.  When overloading these operators, success is
   assumed.  If there is a failure, report an error message and return
   `error_mark_node'.  */

/* NOSTRICT */
tree
build_opfncall (code, flags, xarg1, xarg2, arg3)
     enum tree_code code;
     tree xarg1, xarg2;
     tree arg3;
{
  tree rval = 0;
  tree arg1, arg2;
  tree type1, type2, fnname;
  tree fields1 = 0, parms = 0;
  tree global_fn;
  int try_second;
  int binary_is_unary;

  if (xarg1 == error_mark_node)
    return error_mark_node;

  if (code == COND_EXPR)
    {
      if (TREE_CODE (xarg2) == ERROR_MARK
	  || TREE_CODE (arg3) == ERROR_MARK)
	return error_mark_node;
    }
  if (code == COMPONENT_REF)
    if (TREE_CODE (TREE_TYPE (xarg1)) == POINTER_TYPE)
      return rval;

  /* First, see if we can work with the first argument */
  type1 = TREE_TYPE (xarg1);

  /* Some tree codes have length > 1, but we really only want to
     overload them if their first argument has a user defined type.  */
  switch (code)
    {
    case PREINCREMENT_EXPR:
      code = POSTINCREMENT_EXPR;
      binary_is_unary = 1;
      try_second = 0;
      break;

    case POSTDECREMENT_EXPR:
      code = PREDECREMENT_EXPR;
      binary_is_unary = 1;
      try_second = 0;
      break;

    case PREDECREMENT_EXPR:
    case POSTINCREMENT_EXPR:
    case COMPONENT_REF:
      binary_is_unary = 1;
      try_second = 0;
      break;

      /* ARRAY_REFs and CALL_EXPRs must overload successfully.
	 If they do not, return error_mark_node instead of NULL_TREE.  */
    case ARRAY_REF:
      if (xarg2 == error_mark_node)
	return error_mark_node;
    case CALL_EXPR:
      rval = error_mark_node;
      binary_is_unary = 0;
      try_second = 0;
      break;

    case NEW_EXPR:
      {
	/* For operators `new' (`delete'), only check visibility
	   if we are in a constructor (destructor), and we are
	   allocating for that constructor's (destructor's) type.  */

	fnname = get_identifier (OPERATOR_NEW_FORMAT);
	if (flags & LOOKUP_GLOBAL)
	  return build_overload_call (fnname, tree_cons (NULL_TREE, xarg2, arg3),
				      flags & LOOKUP_COMPLAIN, 0);

	if (current_function_decl == NULL_TREE
	    || !DECL_CONSTRUCTOR_P (current_function_decl)
	    || current_class_type != TYPE_MAIN_VARIANT (type1))
	  flags = LOOKUP_COMPLAIN;
	rval = build_method_call (build1 (NOP_EXPR, xarg1, error_mark_node),
				  fnname, tree_cons (NULL_TREE, xarg2, arg3),
				  NULL_TREE, flags);
	if (rval == error_mark_node)
	  /* User might declare fancy operator new, but invoke it
	     like standard one.  */
	  return rval;

	TREE_TYPE (rval) = xarg1;
	TREE_CALLS_NEW (rval) = 1;
	return rval;
      }
      break;

    case DELETE_EXPR:
      {
	/* See comment above.  */

	fnname = get_identifier (OPERATOR_DELETE_FORMAT);
	if (flags & LOOKUP_GLOBAL)
	  return build_overload_call (fnname, build_tree_list (NULL_TREE, xarg1),
				      flags & LOOKUP_COMPLAIN, 0);

	if (current_function_decl == NULL_TREE
	    || !DESTRUCTOR_NAME_P (DECL_NAME (current_function_decl))
	    || current_class_type != TYPE_MAIN_VARIANT (type1))
	  flags = LOOKUP_COMPLAIN;
	rval = build_method_call (build1 (NOP_EXPR, TREE_TYPE (xarg1), error_mark_node),
				  fnname, build_tree_list (NULL_TREE, xarg1),
				  NULL_TREE, flags);
	/* This happens when the user mis-declares `operator delete'.
	   Should now be impossible.  */
	assert (rval != error_mark_node);
	TREE_TYPE (rval) = void_type_node;
	return rval;
      }
      break;

    default:
      binary_is_unary = 0;
      try_second = tree_code_length [(int) code] == 2;
      if (xarg2 == error_mark_node)
	return error_mark_node;
      break;
    }

  if (try_second && xarg2 == error_mark_node)
    return error_mark_node;

  /* What ever it was, we do not know how to deal with it.  */
  if (type1 == NULL_TREE)
    return rval;

  if (TREE_CODE (type1) == OFFSET_TYPE)
    type1 = TREE_TYPE (type1);

  if (TREE_CODE (type1) == REFERENCE_TYPE)
    {
      arg1 = convert_from_reference (xarg1);
      type1 = TREE_TYPE (arg1);
    }
  else
    {
      arg1 = xarg1;
    }

  if (!IS_AGGR_TYPE (type1))
    {
      /* Try to fail. First, fail if unary */
      if (! try_second)
	return rval;
      /* Second, see if second argument is non-aggregate. */
      type2 = TREE_TYPE (xarg2);
      if (TREE_CODE (type2) == OFFSET_TYPE)
	type2 = TREE_TYPE (type2);
      if (TREE_CODE (type2) == REFERENCE_TYPE)
	{
	  arg2 = convert_from_reference (xarg2);
	  type2 = TREE_TYPE (arg2);
	}
      else
	{
	  arg2 = xarg2;
	}

      if (!IS_AGGR_TYPE (type2))
	return rval;
      try_second = 0;
    }

  if (try_second)
    {
      /* First arg may succeed; see whether second should.  */
      type2 = TREE_TYPE (xarg2);
      if (TREE_CODE (type2) == OFFSET_TYPE)
	type2 = TREE_TYPE (type2);
      if (TREE_CODE (type2) == REFERENCE_TYPE)
	{
	  arg2 = convert_from_reference (xarg2);
	  type2 = TREE_TYPE (arg2);
	}
      else
	{
	  arg2 = xarg2;
	}

      if (! IS_AGGR_TYPE (type2))
	try_second = 0;
    }

  if (type1 == unknown_type_node
      || (try_second && TREE_TYPE (xarg2) == unknown_type_node))
    {
      /* This will not be implemented in the forseeable future.  */
      return rval;
    }

  if (code == MODIFY_EXPR)
    {
      tree op_id = build_opid (MODIFY_EXPR, arg3);
      fnname = build_operator_fnname (&op_id, 0, 2);
    }
  else
    {
      tree op_id = build_opid (0, code);
      if (binary_is_unary)
	fnname = build_operator_fnname (&op_id, 0, 1);
      else
	fnname = build_operator_fnname (&op_id, 0,
					tree_code_length [(int) code]);
    }

  global_fn = IDENTIFIER_GLOBAL_VALUE (fnname);

  /* This is the last point where we will accept failure.  This
     may be too eager if we wish an overloaded operator not to match,
     but would rather a normal operator be called on a type-converted
     argument.  */

  if (IS_AGGR_TYPE (type1))
    fields1 = lookup_fnfields (CLASSTYPE_AS_LIST (type1), fnname, 0);

  if (fields1 == NULL_TREE && global_fn == NULL_TREE)
    return rval;

  /* If RVAL winds up being `error_mark_node', we will return
     that... There is no way that normal semantics of these
     operators will succeed.  */

  /* This argument may be an uncommited OFFSET_REF.  This is
     the case for example when dealing with static class members
     which are referenced from their class name rather than
     from a class instance.  */
  if (TREE_CODE (xarg1) == OFFSET_REF
      && TREE_CODE (TREE_OPERAND (xarg1, 1)) == VAR_DECL)
    xarg1 = TREE_OPERAND (xarg1, 1);
  if (try_second && xarg2 && TREE_CODE (xarg2) == OFFSET_REF
      && TREE_CODE (TREE_OPERAND (xarg2, 1)) == VAR_DECL)
    xarg2 = TREE_OPERAND (xarg2, 1);

  if (global_fn)
    flags |= LOOKUP_GLOBAL;

  if (code == CALL_EXPR)
    {
      /* This can only be a member function.  */
      return build_method_call (xarg1, fnname, xarg2,
				NULL_TREE, LOOKUP_NORMAL);
    }
  else if (tree_code_length[(int) code] == 1 || binary_is_unary)
    {
      parms = NULL_TREE;
      rval = build_method_call (xarg1, fnname, NULL_TREE, NULL_TREE, flags);
    }
  else if (code == COND_EXPR)
    {
      parms = tree_cons (0, xarg2, build_tree_list (0, arg3));
      rval = build_method_call (xarg1, fnname, parms, NULL_TREE, flags);
    }
  else if (code == METHOD_CALL_EXPR)
    {
      /* must be a member function.  */
      parms = tree_cons (NULL_TREE, xarg2, arg3);
      return build_method_call (xarg1, fnname, parms, NULL_TREE, LOOKUP_NORMAL);
    }
  else if (fields1)
    {
      parms = build_tree_list (NULL_TREE, xarg2);
      rval = build_method_call (xarg1, fnname, parms, NULL_TREE, flags);
    }
  else
    {
      parms = tree_cons (NULL_TREE, xarg1,
			 build_tree_list (NULL_TREE, xarg2));
      rval = build_overload_call (fnname, parms, flags & LOOKUP_COMPLAIN, 0);
    }

  /* If we did not win, do not lose yet, since type conversion may work.  */
  if (TREE_CODE (rval) == ERROR_MARK)
    {
      if (flags & LOOKUP_COMPLAIN)
	return rval;
      return 0;
    }

  return rval;
}

/* This function takes an identifier, ID, and attempts to figure out what
   it means. There are a number of possible scenarios, presented in increasing
   order of hair:

   1) not in a class's scope
   2) in class's scope, member name of the class's method
   3) in class's scope, but not a member name of the class
   4) in class's scope, member name of a class's variable

   NAME is $1 from the bison rule. It is an IDENTIFIER_NODE.
   VALUE is $$ from the bison rule. It is the value returned by lookup_name ($1)
   yychar is the pending input character (suitably encoded :-).

   As a last ditch, try to look up the name as a label and return that
   address.

   Values which are declared as being of REFERENCE_TYPE are
   automatically dereferenced here (as a hack to make the
   compiler faster).  */

tree
hack_identifier (value, name, yychar)
     tree value, name;
{
  tree type;

  if (TREE_CODE (value) == ERROR_MARK)
    {
      if (current_class_name)
	{
	  tree fields = lookup_fnfields (CLASSTYPE_AS_LIST (current_class_type), name, 0);
	  if (fields)
	    {
	      fields = TREE_VALUE (fields);
	      assert (TREE_CODE (fields) == FUNCTION_DECL);
	      if (TREE_CHAIN (fields) == NULL_TREE)
		{
		  warning ("methods cannot be converted to function pointers");
		  return fields;
		}
	      else
		{
		  error ("ambiguous request for method pointer `%s'",
			 IDENTIFIER_POINTER (name));
		  return error_mark_node;
		}
	    }
	}
      if (flag_labels_ok && IDENTIFIER_LABEL_VALUE (name))
	{
	  return IDENTIFIER_LABEL_VALUE (name);
	}
      return error_mark_node;
    }

  type = TREE_TYPE (value);
  if (TREE_NONLOCAL (value))
    {
      if (TREE_CODE (value) == FIELD_DECL)
	{
	  if (current_class_decl == NULL_TREE)
	    {
	      error ("request for member `%s' in static member function",
		     IDENTIFIER_POINTER (DECL_NAME (value)));
	      return error_mark_node;
	    }
	  TREE_USED (current_class_decl) = 1;
	  if (yychar == '(')
	    if (! ((TYPE_LANG_SPECIFIC (type)
		    && TYPE_OVERLOADS_CALL_EXPR (type))
		   || (TREE_CODE (type) == REFERENCE_TYPE
		       && TYPE_LANG_SPECIFIC (TREE_TYPE (type))
		       && TYPE_OVERLOADS_CALL_EXPR (TREE_TYPE (type))))
		&& TREE_CODE (type) != FUNCTION_TYPE
		&& TREE_CODE (type) != METHOD_TYPE
		&& (TREE_CODE (type) != POINTER_TYPE
		    || (TREE_CODE (TREE_TYPE (type)) != FUNCTION_TYPE
			&& TREE_CODE (TREE_TYPE (type)) != METHOD_TYPE)))
	    {
	      error ("component `%s' is not a method",
		     IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	  /* Mark so that if we are in a constructor, and then find that
	     this field was initialized by a base initializer,
	     we can emit an error message.  */
	  TREE_USED (value) = 1;
	  return build_component_ref (C_C_D, name, 0, 1);
	}
      if (DECL_CONTEXT (value) != current_class_type
	  && (TREE_CODE (value) == VAR_DECL || TREE_CODE (value) == CONST_DECL))
	{
	  tree path;
	  enum visibility_type visibility;

	  get_base_distance (DECL_CONTEXT (value), current_class_type, 0, &path);
	  visibility = compute_visibility (path, value);
	  if (visibility != visibility_public)
	    {
	      if (TREE_CODE (value) == VAR_DECL)
		error ("static member `%s' is from private base class",
		       IDENTIFIER_POINTER (name));
	      else
		error ("enum `%s' is from private base class",
		       IDENTIFIER_POINTER (name));
	      return error_mark_node;
	    }
	}
      else if (TREE_CODE (value) == TREE_LIST && type == 0)
	{
	  error ("request for member `%s' is ambiguous in multiple inheritance lattice",
		 IDENTIFIER_POINTER (name));
	  return error_mark_node;
	}

      return value;
    }

  if (! TREE_USED (value))
    {
      if (TREE_EXTERNAL (value))
	assemble_external (value);
      TREE_USED (value) = 1;
    }
  if (TREE_CODE (type) == REFERENCE_TYPE)
    {
      assert (TREE_CODE (value) == VAR_DECL || TREE_CODE (value) == PARM_DECL
	      || TREE_CODE (value) == RESULT_DECL);
      if (DECL_REFERENCE_SLOT (value))
	return DECL_REFERENCE_SLOT (value);
    }
  return value;
}

tree
hack_operator (op)
     tree op;
{
  if (op == NULL_TREE)
    return error_mark_node;

  if (TREE_CODE (op) != TYPE_EXPR)
    return grokopexpr (&op, NULL_TREE, 0, 0, 0);

  return op;
}

/* NONWRAPPER is nonzero if this call is not to be wrapped.
   TYPE is the type that the wrapper belongs to (in case
   it should be non-virtual).
   DECL is the function will will be (not be) wrapped.  */
tree
hack_wrapper (nonwrapper, type, decl)
     int nonwrapper;
     tree type, decl;
{
  if (type == NULL_TREE || is_aggr_typedef (type, 1))
    {
      if (type)
	type = TREE_TYPE (type);

      switch (nonwrapper)
	{
	case 0:
	  return build_nt (WRAPPER_EXPR, type, decl);
	case 1:
	  return build_nt (ANTI_WRAPPER_EXPR, type, decl);
	case 2:
	  return build_nt (WRAPPER_EXPR, type,
			   build_nt (COND_EXPR, decl, NULL_TREE, NULL_TREE));
	default:
	  assert (0 <= nonwrapper && nonwrapper <= 2);
	}
    }
  return error_mark_node;
}

/* Return an IDENTIFIER which can be used as a name for
   anonymous structs and unions.  */
tree
make_anon_name ()
{
  static int cnt = 0;
  char buf[32];

  sprintf (buf, ANON_AGGRNAME_FORMAT, cnt++);
  return get_identifier (buf);
}

/* Given an object OF, and a type conversion operator COMPONENT
   build a call to the conversion operator, if a call is requested,
   or return the address (as a pointer to member function) if one is not.

   OF can be a TYPE_DECL or any kind of datum that would normally
   be passed to `build_component_ref'.  It may also be NULL_TREE,
   in which case `current_class_type' and `current_class_decl'
   provide default values.

   BASETYPE_PATH, if non-null, is the path of basetypes
   to go through before we get the the instance of interest.

   PROTECT says whether we apply C++ scoping rules or not.  */
tree
build_component_type_expr (of, component, basetype_path, protect)
     tree of, component, basetype_path;
     int protect;
{
  tree cname = NULL_TREE;
  tree tmp, last;
  tree name;
  int flags = protect ? LOOKUP_NORMAL : LOOKUP_COMPLAIN;

  assert (IS_AGGR_TYPE (TREE_TYPE (of)));
  assert (TREE_CODE (component) == TYPE_EXPR);

  tmp = TREE_OPERAND (component, 0);
  last = NULL_TREE;

  while (tmp)
    {
      switch (TREE_CODE (tmp))
	{
	case CALL_EXPR:
	  if (last)
	    TREE_OPERAND (last, 0) = TREE_OPERAND (tmp, 0);
	  else
	    TREE_OPERAND (component, 0) = TREE_OPERAND (tmp, 0);
	  if (TREE_OPERAND (tmp, 0)
	      && TREE_OPERAND (tmp, 0) != void_list_node)
	    {
	      error ("operator <typename> requires empty parameter list");
	      TREE_OPERAND (tmp, 0) = NULL_TREE;
	    }
	  last = groktypename (build_tree_list (TREE_TYPE (component),
						TREE_OPERAND (component, 0)));
	  name = build_typename_overload (last);
	  TREE_TYPE (name) = last;

	  if (of && TREE_CODE (of) != TYPE_DECL)
	    return build_method_call (of, name, NULL_TREE, NULL_TREE, flags);
	  else if (of)
	    {
	      tree this_this;

	      if (current_class_decl == NULL_TREE)
		{
		  error ("object required for `operator <typename>' call");
		  return error_mark_node;
		}

	      this_this = convert_pointer_to (TREE_TYPE (of), current_class_decl);
	      return build_method_call (this_this, name, NULL_TREE,
					NULL_TREE, flags | LOOKUP_NONVIRTUAL);
	    }
	  else if (current_class_decl)
	    return build_method_call (tmp, name, NULL_TREE, NULL_TREE, flags);

	  error ("object required for `operator <typename>' call");
	  return error_mark_node;

	case INDIRECT_REF:
	case ADDR_EXPR:
	case ARRAY_REF:
	  break;

	case SCOPE_REF:
	  assert (cname == 0);
	  cname = TREE_OPERAND (tmp, 0);
	  tmp = TREE_OPERAND (tmp, 1);
	  break;

	default:
	  abort ();
	}
      last = tmp;
      tmp = TREE_OPERAND (tmp, 0);
    }

  last = groktypename (build_tree_list (TREE_TYPE (component), TREE_OPERAND (component, 0)));
  name = build_typename_overload (last);
  TREE_TYPE (name) = last;
  if (of && TREE_CODE (of) == TYPE_DECL)
    {
      if (cname == NULL_TREE)
	{
	  cname = DECL_NAME (of);
	  of = NULL_TREE;
	}
      else assert (cname == DECL_NAME (of));
    }

  if (of)
    {
      tree this_this;

      if (current_class_decl == NULL_TREE)
	{
	  error ("object required for `operator <typename>' call");
	  return error_mark_node;
	}

      this_this = convert_pointer_to (TREE_TYPE (of), current_class_decl);
      return build_component_ref (this_this, name, 0, protect);
    }
  else if (cname)
    return build_offset_ref (cname, name);
  else if (current_class_name)
    return build_offset_ref (current_class_name, name);

  error ("object required for `operator <typename>' member reference");
  return error_mark_node;
}
