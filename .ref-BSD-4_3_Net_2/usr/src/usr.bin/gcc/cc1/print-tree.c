/* Prints out tree in human readable form - GNU C-compiler
   Copyright (C) 1990 Free Software Foundation, Inc.

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
#include "tree.h"
#include <stdio.h>


/* Names of tree components.
   Used for printing out the tree and error messages.  */
#define DEFTREECODE(SYM, NAME, TYPE, LEN) NAME,

char *tree_code_name[] = {
#include "tree.def"
};
#undef DEFTREECODE

/* Indexed by enum tree_code, contains a character which is
   '<' for a comparison expression, '1', for a unary arithmetic
   expression, '2' for a binary arithmetic expression, `e' for
   other types of expressions, `r' for a reference, `c' for a
   constant, `d' for a decl, `t' for a type, `s' for a statement,
   and `x' for anything else (TREE_LIST, IDENTIFIER, etc).  */

#define TREE_CODE_CLASS(CODE)	(*tree_code_type[(int) (CODE)])

extern char *mode_name[];

void print_node ();

/* Define the hash table of nodes already seen.
   Such nodes are not repeated; brief cross-references are used.  */

#define HASH_SIZE 37

struct bucket
{
  tree node;
  struct bucket *next;
};

static struct bucket **table;

/* Print the node NODE on standard error, for debugging.
   Most nodes referred to by this one are printed recursively
   down to a depth of six.  */

void
debug_tree (node)
     tree node;
{
  char *object = (char *) oballoc (0);
  table = (struct bucket **) oballoc (HASH_SIZE * sizeof (struct bucket *));
  bzero (table, HASH_SIZE * sizeof (struct bucket *));
  print_node (stderr, "", node, 0);
  table = 0;
  obfree (object);
  fprintf (stderr, "\n");
}

/* Print a node in brief fashion, with just the code, address and name.  */

void
print_node_brief (file, prefix, node, indent)
     FILE *file;
     char *prefix;
     tree node;
     int indent;
{
  char class;

  if (node == 0)
    return;

  class = TREE_CODE_CLASS (TREE_CODE (node));

  /* Always print the slot this node is in, and its code, address and
     name if any.  */
  if (indent > 0)
    fprintf (file, " ");
  fprintf (file, "%s <%s %x", prefix,
	   tree_code_name[(int) TREE_CODE (node)], (int) node);

  if (class == 'd')
    {
      if (DECL_NAME (node))
	fprintf (file, " %s", IDENTIFIER_POINTER (DECL_NAME (node)));
    }
  else if (class == 't')
    {
      if (TYPE_NAME (node))
	{
	  if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	    fprintf (file, " %s", IDENTIFIER_POINTER (TYPE_NAME (node)));
	  else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (node)))
	    fprintf (file, " %s",
		     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node))));
	}
    }
  if (TREE_CODE (node) == IDENTIFIER_NODE)
    fprintf (file, " %s", IDENTIFIER_POINTER (node));
  /* We might as well always print the value of an integer.  */
  if (TREE_CODE (node) == INTEGER_CST)
    {
      if (TREE_INT_CST_HIGH (node) == 0)
	fprintf (file, " %1u", TREE_INT_CST_LOW (node));
      else if (TREE_INT_CST_HIGH (node) == -1
	       && TREE_INT_CST_LOW (node) != 0)
	fprintf (file, " -%1u", -TREE_INT_CST_LOW (node));
      else
	fprintf (file, " 0x%x%08x",
		 TREE_INT_CST_HIGH (node),
		 TREE_INT_CST_LOW (node));
    }
  if (TREE_CODE (node) == REAL_CST)
    {
#ifndef REAL_IS_NOT_DOUBLE
      fprintf (file, " %e", TREE_REAL_CST (node));
#else
      {
	int i;
	char *p = (char *) &TREE_REAL_CST (node);
	fprintf (file, " 0x");
	for (i = 0; i < sizeof TREE_REAL_CST (node); i++)
	  fprintf (file, "%02x", *p++);
	fprintf (file, "");
      }
#endif /* REAL_IS_NOT_DOUBLE */
    }

  fprintf (file, ">");
}

void
indent_to (file, column)
     FILE *file;
     int column;
{
  int i;

  /* Since this is the long way, indent to desired column.  */
  if (column > 0)
    fprintf (file, "\n");
  for (i = 0; i < column; i++)
    fprintf (file, " ");
}

/* Print the node NODE in full on file FILE, preceded by PREFIX,
   starting in column INDENT.  */

void
print_node (file, prefix, node, indent)
     FILE *file;
     char *prefix;
     tree node;
     int indent;
{
  int hash;
  struct bucket *b;
  enum machine_mode mode;
  char class;
  int len;
  int first_rtl;
  int i;

  if (node == 0)
    return;

  class = TREE_CODE_CLASS (TREE_CODE (node));

  /* Don't get too deep in nesting.  If the user wants to see deeper,
     it is easy to use the address of a lowest-level node
     as an argument in another call to debug_tree.  */

  if (indent > 24)
    {
      print_node_brief (file, prefix, node, indent);
      return;
    }

  if (indent > 8 && (class == 't' || class == 'd'))
    {
      print_node_brief (file, prefix, node, indent);
      return;
    }

  hash = ((int) node & ~(1 << (HOST_BITS_PER_INT - 1))) % HASH_SIZE;

  /* If node is in the table, just mention its address.  */
  for (b = table[hash]; b; b = b->next)
    if (b->node == node)
      {
	print_node_brief (file, prefix, node, indent);
	return;
      }

  /* Add this node to the table.  */
  b = (struct bucket *) oballoc (sizeof (struct bucket));
  b->node = node;
  b->next = table[hash];
  table[hash] = b;

  /* Indent to the specified column, since this is the long form.  */
  indent_to (file, indent);

  /* Print the slot this node is in, and its code, and address.  */
  fprintf (file, "%s <%s %x", prefix,
	   tree_code_name[(int) TREE_CODE (node)], (int) node);

  /* Print the name, if any.  */
  if (class == 'd')
    {
      if (DECL_NAME (node))
	fprintf (file, " %s", IDENTIFIER_POINTER (DECL_NAME (node)));
    }
  else if (class == 't')
    {
      if (TYPE_NAME (node))
	{
	  if (TREE_CODE (TYPE_NAME (node)) == IDENTIFIER_NODE)
	    fprintf (file, " %s", IDENTIFIER_POINTER (TYPE_NAME (node)));
	  else if (TREE_CODE (TYPE_NAME (node)) == TYPE_DECL
		   && DECL_NAME (TYPE_NAME (node)))
	    fprintf (file, " %s",
		     IDENTIFIER_POINTER (DECL_NAME (TYPE_NAME (node))));
	}
    }
  if (TREE_CODE (node) == IDENTIFIER_NODE)
    fprintf (file, " %s", IDENTIFIER_POINTER (node));

  if (TREE_CODE (node) == INTEGER_CST)
    {
      if (indent <= 4)
	print_node_brief (file, "type", TREE_TYPE (node), indent + 4);
    }
  else
    {
      print_node (file, "type", TREE_TYPE (node), indent + 4);
      if (TREE_TYPE (node))
	indent_to (file, indent + 3);
    }

  if (TREE_EXTERNAL (node))
    fprintf (file, " external");
  if (TREE_PUBLIC (node))
    fprintf (file, " public");
  if (TREE_STATIC (node))
    fprintf (file, " static");
  if (TREE_VOLATILE (node))
    fprintf (file, " volatile");
  if (TREE_PACKED (node))
    fprintf (file, " packed");
  if (TREE_READONLY (node))
    fprintf (file, " readonly");
  if (TREE_LITERAL (node))
    fprintf (file, " literal");
  if (TREE_NONLOCAL (node))
    fprintf (file, " nonlocal");
  if (TREE_PERMANENT (node))
    fprintf (file, " permanent");
  if (TREE_ADDRESSABLE (node))
    fprintf (file, " addressable");
  if (TREE_REGDECL (node))
    fprintf (file, " regdecl");
  if (TREE_THIS_VOLATILE (node))
    fprintf (file, " this_vol");
  if (TREE_UNSIGNED (node))
    fprintf (file, " unsigned");
  if (TREE_ASM_WRITTEN (node))
    fprintf (file, " asm_written");
  if (TREE_INLINE (node))
    fprintf (file, " inline");
  if (TREE_USED (node))
    fprintf (file, " used");
  if (TREE_LANG_FLAG_1 (node))
    fprintf (file, " lang_flag_1");
  if (TREE_LANG_FLAG_2 (node))
    fprintf (file, " lang_flag_2");
  if (TREE_LANG_FLAG_3 (node))
    fprintf (file, " lang_flag_3");
  if (TREE_LANG_FLAG_4 (node))
    fprintf (file, " lang_flag_4");


  switch (TREE_CODE_CLASS (TREE_CODE (node)))
    {
    case 'd':
      mode = DECL_MODE (node);
      fprintf (file, " %s", mode_name[(int) mode]);

      fprintf (file, " file %s line %d",
	       DECL_SOURCE_FILE (node), DECL_SOURCE_LINE (node));

      print_node (file, "size", DECL_SIZE (node), indent + 4);
      indent_to (file, indent + 3);
      fprintf (file, " align %d", DECL_ALIGN (node));
      fprintf (file, " size_unit %d", DECL_SIZE_UNIT (node));
      fprintf (file, " offset %d", DECL_OFFSET (node));
      print_node_brief (file, "context", DECL_CONTEXT (node), indent + 4);
      print_node_brief (file, "voffset", DECL_VOFFSET (node), indent + 4);

      print_node (file, "arguments", DECL_ARGUMENTS (node), indent + 4);
      print_node (file, "result", DECL_RESULT (node), indent + 4);
      print_node_brief (file, "initial", DECL_INITIAL (node), indent + 4);

      /* Print the decl chain only if decl is at second level.  */
      if (indent == 4)
	print_node (file, "chain", TREE_CHAIN (node), indent + 4);
      else
	print_node_brief (file, "chain", TREE_CHAIN (node), indent + 4);
      break;

    case 't':
      mode = TYPE_MODE (node);
      fprintf (file, " %s", mode_name[(int) mode]);

      print_node (file, "size", TYPE_SIZE (node), indent + 4);
      indent_to (file, indent + 3);

      fprintf (file, " align %d", TYPE_ALIGN (node));
      fprintf (file, " size_unit %d", TYPE_SIZE_UNIT (node));
      fprintf (file, " sep_unit %d", TYPE_SEP_UNIT (node));
      fprintf (file, " symtab %d", TYPE_SYMTAB_ADDRESS (node));

      print_node (file, "sep", TYPE_SEP (node), indent + 4);

      if (TREE_CODE (node) == ARRAY_TYPE || TREE_CODE (node) == SET_TYPE)
	print_node (file, "domain", TYPE_DOMAIN (node), indent + 4);
      else if (TREE_CODE (node) == INTEGER_TYPE)
	{
	  fprintf (file, " precision %d", TYPE_PRECISION (node));
	  print_node (file, "min", TYPE_MIN_VALUE (node), indent + 4);
	  print_node (file, "max", TYPE_MAX_VALUE (node), indent + 4);
	}
      else if (TREE_CODE (node) == ENUMERAL_TYPE)
	{
	  fprintf (file, " precision %d", TYPE_PRECISION (node));
	  print_node (file, "min", TYPE_MIN_VALUE (node), indent + 4);
	  print_node (file, "max", TYPE_MAX_VALUE (node), indent + 4);
	  print_node (file, "values", TYPE_VALUES (node));
	}
      else if (TREE_CODE (node) == REAL_TYPE)
	fprintf (file, " precision %d", TYPE_PRECISION (node));
      else if (TREE_CODE (node) == RECORD_TYPE || TREE_CODE (node) == UNION_TYPE)
	print_node (file, "fields", TYPE_FIELDS (node), indent + 4);
      else if (TREE_CODE (node) == FUNCTION_TYPE || TREE_CODE (node) == METHOD_TYPE)
	print_node (file, "arg-types", TYPE_ARG_TYPES (node), indent + 4);

      if (TYPE_POINTER_TO (node) || TREE_CHAIN (node))
	indent_to (file, indent + 3);
      print_node_brief (file, "pointer_to_this", TYPE_POINTER_TO (node), indent + 4);
      print_node_brief (file, "reference_to_this", TYPE_REFERENCE_TO (node), indent + 4);
      print_node_brief (file, "chain", TREE_CHAIN (node), indent + 4);
      break;

    case 'e':
    case '<':
    case '1':
    case '2':
    case 'r':
      first_rtl = len = tree_code_length[(int) TREE_CODE (node)];
      /* These kinds of nodes contain rtx's, not trees,
	 after a certain point.  Print the rtx's as rtx's.  */
      switch (TREE_CODE (node))
	{
	case SAVE_EXPR:
	  first_rtl = 1;
	  break;
	case CALL_EXPR:
	  first_rtl = 2;
	  break;
	case METHOD_CALL_EXPR:
	  first_rtl = 3;
	  break;
	case WITH_CLEANUP_EXPR:
	  /* Should be defined to be 2.  */
	  first_rtl = 1;
	  break;
	case RTL_EXPR:
	  first_rtl = 0;
	}
      for (i = 0; i < len; i++)
	{
	  if (i >= first_rtl)
	    {
	      if (TREE_OPERAND (node, i))
		print_rtl (file, TREE_OPERAND (node, i));
	      else
		fprintf (file, "(nil)");
	      fprintf (file, "\n");
	    }
	  else
	    {
	      char temp[10];

	      sprintf (temp, "arg %d", i);
	      print_node (file, temp, TREE_OPERAND (node, i), indent + 4);
	    }
	}
      break;

    case 's':
      fprintf (file, " file %s line %d",
	       STMT_SOURCE_FILE (node), STMT_SOURCE_LINE (node));

      switch (TREE_CODE (node))
	{
	case IF_STMT:
	  print_node (file, "cond", STMT_COND (node), indent + 4);
	  print_node (file, "then", STMT_THEN (node), indent + 4);
	  print_node (file, "else", STMT_ELSE (node), indent + 4);
	  break;

	case LET_STMT:
	case WITH_STMT:
	  print_node (file, "vars", STMT_VARS (node), indent + 4);
	  print_node (file, "tags", STMT_TYPE_TAGS (node), indent + 4);
	  print_node (file, "supercontext", STMT_SUPERCONTEXT (node), indent + 4);
	  print_node (file, "body", STMT_BODY (node), indent + 4);
	  print_node (file, "subblocks", STMT_SUBBLOCKS (node), indent + 4);
	  break;

	case CASE_STMT:
	  print_node (file, "index", STMT_CASE_INDEX (node), indent + 4);
	  print_node (file, "list", STMT_CASE_LIST (node), indent + 4);
	  break;

	default:
	  print_node (file, "body", STMT_BODY (node), indent + 4);
	  break;
	}
      print_node (file, "chain", TREE_CHAIN (node), indent + 4);
      break;

    case 'c':
    case 'x':
      switch (TREE_CODE (node))
	{
	case INTEGER_CST:
	  if (TREE_INT_CST_HIGH (node) == 0)
	    fprintf (file, " %1u", TREE_INT_CST_LOW (node));
	  else if (TREE_INT_CST_HIGH (node) == -1
		   && TREE_INT_CST_LOW (node) != 0)
	    fprintf (file, " -%1u", -TREE_INT_CST_LOW (node));
	  else
	    fprintf (file, " 0x%x%08x",
		     TREE_INT_CST_HIGH (node),
		     TREE_INT_CST_LOW (node));
	  break;

	case REAL_CST:
#ifndef REAL_IS_NOT_DOUBLE
	  fprintf (file, " %e", TREE_REAL_CST (node));
#else
	  {
	    char *p = (char *) &TREE_REAL_CST (node);
	    fprintf (file, " 0x");
	    for (i = 0; i < sizeof TREE_REAL_CST (node); i++)
	      fprintf (file, "%02x", *p++);
	    fprintf (file, "");
	  }
#endif /* REAL_IS_NOT_DOUBLE */
	  break;

	case COMPLEX_CST:
	  print_node (file, "real", TREE_REALPART (node), indent + 4);
	  print_node (file, "imag", TREE_IMAGPART (node), indent + 4);
	  break;

	case STRING_CST:
	  fprintf (file, " \"%s\"", TREE_STRING_POINTER (node));
	  break;

	case IDENTIFIER_NODE:
	  print_lang_identifier (file, node, indent);
	  break;

	case TREE_LIST:
	  print_node (file, "purpose", TREE_PURPOSE (node), indent + 4);
	  print_node (file, "value", TREE_VALUE (node), indent + 4);
	  print_node (file, "chain", TREE_CHAIN (node), indent + 4);
	  break;

	case OP_IDENTIFIER:
	  print_node (file, "op1", TREE_PURPOSE (node), indent + 4);
	  print_node (file, "op2", TREE_VALUE (node), indent + 4);
	}

      print_node (file, "chain", TREE_CHAIN (node), indent + 4);
      break;
    }
}
