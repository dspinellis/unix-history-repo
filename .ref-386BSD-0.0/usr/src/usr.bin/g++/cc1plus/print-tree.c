/* Prints out tree in human readable form - GNU C-compiler
   Copyright (C) 1987 Free Software Foundation, Inc.

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

extern char **tree_code_name;

extern char *mode_name[];

extern char spaces[];

#define MIN(x,y) ((x < y) ? x : y)

static FILE *outfile;

extern int tree_node_counter;

/* markvec[i] is 1 if node number i has been seen already.  */

static char *markvec;

static void dump ();
void dump_tree ();

void
debug_dump_tree (root)
     tree root;
{
  dump_tree (stderr, root);
}

void
dump_tree (outf, root)
     FILE *outf;
     tree root;
{
  markvec = (char *) alloca (tree_node_counter + 1);
  bzero (markvec, tree_node_counter + 1);
  outfile = outf;
  dump (root, 0);
  fflush (outf);
}

static
void
wruid (node)
     tree node;
{
 
  if (node == NULL)
    fputs ("<>", outfile);
  else {
    fprintf (outfile, "%1d", TREE_UID (node));
  }
}

static 
void
part (title, node)
     char title[];
     tree node;
{
  fprintf (outfile, " %s = ", title);
  wruid (node);
  putc (';', outfile);
}

/* Similar to `part' but prefix with @ if value is not constant
   and print the constant value if it is constant.  */
static
void
cpart (title, ct, punct)
     char *title;
     tree ct;
     char punct;
{
  fprintf (outfile, " %s = ", title);
  if (ct == NULL)
    fputs ("<>", outfile);
  else
    {
      if (!TREE_LITERAL (ct))
	{
	  putc ('@', outfile);
	  wruid (ct);
	}
      else
	fprintf (outfile, "%ld", TREE_INT_CST_LOW (ct));
    }
  putc (punct, outfile);
}

static void
walk (node, leaf, indent)
     tree node;
     tree leaf;
     int indent;
{
  if (node != NULL
      /* Don't walk any global nodes reached from local nodes!
	 The global nodes will be dumped at the end, all together.
	 Also don't mention a FUNCTION_DECL node that is marked local
	 since it was fully described when it was dumped locally.  */
      && (TREE_CODE (node) != FUNCTION_DECL
	  || TREE_PERMANENT (node))
      && (TREE_PERMANENT (leaf) == TREE_PERMANENT (node)))
    dump (node, indent+1);
}

static void
cwalk (s, leaf, indent)
     tree s;
     tree leaf;
     int indent;
{
  if (s != NULL) 
    if (!TREE_LITERAL (s))
      walk (s, leaf, indent);
}
 
static void
prtypeinfo (node)
     register tree node;
{
  int first;
  
  part ("type", TREE_TYPE (node));
  first = 1;
  fputs (" [", outfile);
  if (TREE_EXTERNAL (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("external", outfile);
      first = 0;
    }
  if (TREE_PUBLIC (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("public", outfile);
      first = 0;
    }
  if (TREE_STATIC (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("static", outfile);
      first = 0;
    }
  if (TREE_VOLATILE (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("volatile", outfile);
      first = 0;
    }
  if (TREE_PACKED (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("packed", outfile);
      first = 0;
    }
  if (TREE_READONLY (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("readonly", outfile);
      first = 0;
    }
  if (TREE_LITERAL (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("literal", outfile);
      first = 0;
    }
  if (TREE_NONLOCAL (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("nonlocal", outfile);
      first = 0;
    }
  if (TREE_ADDRESSABLE (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("addressable", outfile);
      first = 0;
    }
  if (TREE_REGDECL (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("regdecl", outfile);
      first = 0;
    }
  if (TREE_THIS_VOLATILE (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("this_vol", outfile);
      first = 0;
    }
  if (TREE_UNSIGNED (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("unsigned", outfile);
      first = 0;
    }
  if (TREE_ASM_WRITTEN (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("asm_written", outfile);
      first = 0;
    }
  if (TREE_INLINE (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("inline", outfile);
      first = 0;
    }
  if (TREE_USED (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("used", outfile);
      first = 0;
    }
  if (TREE_PERMANENT (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("permanent", outfile);
      first = 0;
    }
  if (TREE_LANG_FLAG_1 (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("lang_flag_1", outfile);
      first = 0;
    }
  if (TREE_LANG_FLAG_2 (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("lang_flag_2", outfile);
      first = 0;
    }
  if (TREE_LANG_FLAG_3 (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("lang_flag_3", outfile);
      first = 0;
    }
  if (TREE_LANG_FLAG_4 (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("lang_flag_4", outfile);
      first = 0;
    }
  fputs ("] ", outfile);
}

static void
prdeclmodeinfo (node)
     tree node;
{
  register enum machine_mode mode = DECL_MODE (node);
  fprintf (outfile, " %s;", mode_name[(int) mode]);

  cpart ("size", DECL_SIZE (node), '*');
  fprintf (outfile, "%d;", DECL_SIZE_UNIT (node));

  fprintf (outfile, " alignment = %1d;", DECL_ALIGN (node));
}

static void
prtypemodeinfo (node)
     tree node;
{
  register enum machine_mode mode = TYPE_MODE (node);
  fprintf (outfile, " %s;", mode_name[(int) mode]);

  cpart ("size", TYPE_SIZE (node), '*');
  fprintf (outfile, "%d;", TYPE_SIZE_UNIT (node));

  fprintf (outfile, " alignment = %1d;", TYPE_ALIGN (node));
}

static void
skip (indent)
     int indent;
{
  putc ('\n',outfile);
  fputs (spaces + (strlen (spaces) - (12 + MIN (40,(indent+1)*2))), outfile);
}

/* Output a description of the tree node NODE
   if its description has not been output already.  */

static 
void
dump (node, indent)
     tree node;
     int indent;
{
  register enum tree_code code = TREE_CODE (node);
  register int i;
  register int len, first_rtl;
  int nochain = 0;

  if (markvec[TREE_UID (node)])
    return;
  markvec[TREE_UID (node)] = 1;

  fputs ("   ", outfile);
  fprintf (outfile, "%5d", TREE_UID (node));
  fputs (spaces + (strlen (spaces) - MIN (40, (indent+1)*2)), outfile);
  fputs (tree_code_name[(int) code], outfile);

  switch (*tree_code_type[(int) code])
    {
    case 'd':
      fputs (" name = ", outfile);
      if (DECL_NAME (node) == NULL)
	fputs ("<>;", outfile);
      else
	fprintf (outfile, "%s;",
		 IDENTIFIER_POINTER (DECL_NAME (node)));
      if (code != PARM_DECL)
	fprintf (outfile, " at %s line %d;",
		 DECL_SOURCE_FILE (node), DECL_SOURCE_LINE (node));
      skip (indent);
      prdeclmodeinfo (node);
      prtypeinfo (node);
#ifdef PRINT_LANG_DECL
      print_lang_decl (node);
#endif
      skip (indent);
      fprintf (outfile, " offset = %1d;", DECL_OFFSET (node));
      if (DECL_VOFFSET (node) != NULL)
	{
	  fputs ("voffset = ", outfile);
	  wruid (DECL_VOFFSET (node));
	  fprintf (outfile, "*%1d;", DECL_VOFFSET_UNIT (node));
	}
      part ("context", DECL_CONTEXT (node));
      if (code == FUNCTION_DECL)
	{
	  if (DECL_ARGUMENTS (node) || DECL_RESULT (node)
	      || DECL_INITIAL (node))
	    {
	      skip (indent);
	      part ("arguments", DECL_ARGUMENTS (node));
	      part ("result", DECL_RESULT (node));
	      if ((int) (DECL_INITIAL (node)) == 1)
		fprintf (outfile, " initial = const 1;");
	      else
		part ("initial", DECL_INITIAL (node));
	    }
	}
      else if (DECL_INITIAL (node))
	{
	  if ((int) (DECL_INITIAL (node)) == 1)
	    fprintf (outfile, " initial = const 1;");
	  else
	    part ("initial", DECL_INITIAL (node));
	}
#ifdef PRINT_LANG_DECL
      walk_lang_decl (node);
#endif
      part ("chain", TREE_CHAIN (node));
      /* A Decl's chain contents is not part of the decl.  */
      nochain = 1;
      fputc ('\n', outfile);
      cwalk (DECL_SIZE (node), node, indent);
      walk (TREE_TYPE (node), node, indent);
      walk (DECL_VOFFSET (node), node, indent);
      walk (DECL_CONTEXT (node), node, indent);
      if (code == FUNCTION_DECL)
	{
	  walk (DECL_ARGUMENTS (node), node, indent);
	  walk (DECL_RESULT (node), node, indent);
	}
      if ((int) (DECL_INITIAL (node)) != 1)
	walk (DECL_INITIAL (node), node, indent);
      break;

    case 't':
      prtypemodeinfo (node);
      prtypeinfo (node);
#ifdef PRINT_LANG_TYPE
      print_lang_type (node);
#endif
      skip (indent);
      part ("pointers_to_this", TYPE_POINTER_TO (node));
      if (code == ARRAY_TYPE || code == SET_TYPE)
	{
	  part ("domain", TYPE_DOMAIN (node));
	  cpart ("separation", TYPE_SEP (node), '*');
	  fprintf (outfile, "%d;", TYPE_SEP_UNIT (node));
	}
      else if (code == INTEGER_TYPE)
	{
	  cpart ("min", TYPE_MIN_VALUE (node), ';');
	  cpart ("max", TYPE_MAX_VALUE (node), ';');
	  fprintf (outfile, "precision = %d;", TYPE_PRECISION (node));
	}
      else if (code == ENUMERAL_TYPE)
	{
	  cpart ("min", TYPE_MIN_VALUE (node), ';');
	  cpart ("max", TYPE_MAX_VALUE (node), ';');
	  part ("values", TYPE_VALUES (node));
	  fprintf (outfile, "precision = %d;", TYPE_PRECISION (node));
	}
      else if (code == REAL_TYPE)
	{
	  fprintf (outfile, "precision = %d;", TYPE_PRECISION (node));
	}
      else if (code == RECORD_TYPE
	       || code == UNION_TYPE)
	{
	  part ("fields", TYPE_FIELDS (node));
	}
      else if (code == FUNCTION_TYPE)
	{
	  part ("arg_types", TYPE_ARG_TYPES (node));
	}
      else if (code == METHOD_TYPE)
	{
	  part ("arg_types", TYPE_ARG_TYPES (node));
	}
#ifdef PRINT_LANG_TYPE
      walk_lang_type (node);
#endif
      part ("chain", TREE_CHAIN (node));
      /* A type's chain's contents are not printed because the chain of types
	 is not part of the meaning of any particular type.  */
      nochain = 1;
      fputc ('\n', outfile);
      cwalk (TYPE_SIZE (node), node, indent);
      walk (TREE_TYPE (node), node, indent);
      walk (TYPE_VALUES (node), node, indent);
      walk (TYPE_SEP (node), node, indent);
      walk (TYPE_POINTER_TO (node), node, indent);
      walk (TYPE_REFERENCE_TO (node), node, indent);
      break;

    case 'e':
    case 'r':
      prtypeinfo (node);
      fputs (" ops =", outfile);
      first_rtl = len = tree_code_length[(int) code];
      /* These kinds of nodes contain rtx's, not trees,
	 after a certain point.  Print the rtx's as rtx's.  */
      switch (code)
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
	      skip (indent);
	      if (TREE_OPERAND (node, i))
		print_rtl (outfile, TREE_OPERAND (node, i));
	      else
		fprintf (outfile, "(nil)");
	      fprintf (outfile, "\n");
	    }
	  else
	    {
	      fputs (" ", outfile);
	      wruid (TREE_OPERAND (node, i));
	      fputs (";", outfile);
	    }
	}
      part ("chain", TREE_CHAIN (node));
      fputc ('\n', outfile);
      walk (TREE_TYPE (node), node, indent);
      for (i = 0; i < len && i < first_rtl; i++)
	walk (TREE_OPERAND (node, i), node, indent);
      break;

    case 's':
      prtypeinfo (node);
      fprintf (outfile, " at %s line %d;",
	       STMT_SOURCE_FILE (node), STMT_SOURCE_LINE (node));
      switch (TREE_CODE (node))
	{
	case IF_STMT:
	  part ("cond", STMT_COND (node));
	  part ("then", STMT_THEN (node));
	  part ("else", STMT_ELSE (node));
	  break;

	case LET_STMT:
	case WITH_STMT:
	  part ("vars", STMT_VARS (node));
	  part ("tags", STMT_TYPE_TAGS (node));
	  part ("supercontext", STMT_SUPERCONTEXT (node));
	  part ("bind_size", STMT_BIND_SIZE (node));
	  part ("body", STMT_BODY (node));
	  part ("subblocks", STMT_SUBBLOCKS (node));
	  break;

	case CASE_STMT:
	  part ("case_index", STMT_CASE_INDEX (node));
	  part ("case_list", STMT_CASE_LIST (node));
	  break;

	default:
	  part ("body", STMT_BODY (node));
	  break;
	}
      part ("chain", TREE_CHAIN (node));
      fputc ('\n', outfile);
      walk (TREE_TYPE (node), node, indent);
      switch (TREE_CODE (node))
	{
	case IF_STMT:
	  walk (STMT_COND (node), node, indent);
	  walk (STMT_THEN (node), node, indent);
	  walk (STMT_ELSE (node), node, indent);
	  break;

	case LET_STMT:
	case WITH_STMT:
	  walk (STMT_VARS (node), node, indent);
	  walk (STMT_TYPE_TAGS (node), node, indent);
	  walk (STMT_SUPERCONTEXT (node), node, indent);
	  walk (STMT_BIND_SIZE (node), node, indent);
	  walk (STMT_BODY (node), node, indent);
	  walk (STMT_SUBBLOCKS (node), node, indent);
	  break;

	case CASE_STMT:
	  walk (STMT_CASE_INDEX (node), node, indent);
	  walk (STMT_CASE_LIST (node), node, indent);
	  break;

	default:
	  walk (STMT_BODY (node), node, indent);
	  break;
	}
      break;

    case 'c':
      switch (code)
	{
	case INTEGER_CST:
	  if (TREE_INT_CST_HIGH (node) == 0)
	    fprintf (outfile, " = %1u;", TREE_INT_CST_LOW (node));
	  else if (TREE_INT_CST_HIGH (node) == -1
		   && TREE_INT_CST_LOW (node) != 0)
	    fprintf (outfile, " = -%1u;", -TREE_INT_CST_LOW (node));
	  else
	    fprintf (outfile, " = 0x%x%08x;",
		     TREE_INT_CST_HIGH (node),
		     TREE_INT_CST_LOW (node));
	  break;

	case REAL_CST:
#ifndef REAL_IS_NOT_DOUBLE
	  fprintf (outfile, " = %e;", TREE_REAL_CST (node));
#else
	  {
	    int i;
	    char *p = (char *) &TREE_REAL_CST (node);
	    fprintf (outfile, " = 0x");
	    for (i = 0; i < sizeof TREE_REAL_CST (node); i++)
	      fprintf (outfile, "%02x", *p++);
	    fprintf (outfile, ";");
	  }
#endif /* REAL_IS_NOT_DOUBLE */
	  break;

	case COMPLEX_CST:
	  part ("realpart", TREE_REALPART (node));
	  part ("imagpart", TREE_IMAGPART (node));
	  walk (TREE_REALPART (node), node, indent);
	  walk (TREE_IMAGPART (node), node, indent);
	  break;

	case STRING_CST:
	  fprintf (outfile, " = \"%s\";", TREE_STRING_POINTER (node));
	}
      prtypeinfo (node);
      part ("chain", TREE_CHAIN (node));
      fputc ('\n', outfile);
      walk (TREE_TYPE (node), node, indent);
      break;

    case 'x':
      if (code == IDENTIFIER_NODE)
	{
	  fprintf (outfile, " = %s;\n", IDENTIFIER_POINTER (node));
	  nochain = 1;
	}
      else if (code == TREE_LIST)
	{
	  prtypeinfo (node);
	  part ("purpose", TREE_PURPOSE (node));
	  part ("value", TREE_VALUE (node));
	  part ("chain", TREE_CHAIN (node));
	  fputc ('\n', outfile);
	  walk (TREE_TYPE (node), node, indent);
	  walk (TREE_PURPOSE (node), node, indent);
	  walk (TREE_VALUE (node), node, indent);
	}
      else if (code == TREE_VEC)
	{
	  prtypeinfo (node);
	  len = TREE_VEC_LENGTH (node);
	  fprintf (outfile, "length = %d\n", len);
	  for (i = 0; i < len; i++)
	    {
	      fputs (" ", outfile);
	      wruid (TREE_VEC_ELT (node, i));
	      fputs (";", outfile);
	    }
	  part ("chain", TREE_CHAIN (node));
	  fputc ('\n', outfile);
	  walk (TREE_TYPE (node), node, indent);
	  for (i = 0; i < len; i++)
	    walk (TREE_VEC_ELT (node, i), node, indent);
	}
      else if (code == OP_IDENTIFIER)
	{
	  prtypeinfo (node);
	  part ("op1", TREE_PURPOSE (node));
	  part ("op2", TREE_VALUE (node));
	  part ("chain", TREE_CHAIN (node));
	  fputc ('\n', outfile);
	  walk (TREE_TYPE (node), node, indent);
	  walk (TREE_PURPOSE (node), node, indent);
	  walk (TREE_VALUE (node), node, indent);
	}
      else if (code == ERROR_MARK)
	fputc ('\n', outfile);
      else abort ();

      break;

    default:
      abort ();
    } /* switch */

  if (TREE_CHAIN (node) != NULL && ! nochain)
    dump (TREE_CHAIN (node), indent);
}
