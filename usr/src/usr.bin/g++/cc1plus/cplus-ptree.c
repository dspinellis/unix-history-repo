/* Prints out tree in human readable form - GNU C++ compiler
   Copyright (C) 1987 Free Software Foundation, Inc.
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
#include "tree.h"
#include "cplus-tree.h"
#include <stdio.h>


extern char *tree_code_err_name[];
extern char *tree_code_err_asg_name[];
extern char *mode_name[];

extern char spaces[];

#define MIN(x,y) ((x < y) ? x : y)

static FILE *outfile;

/* This code is copied from print-tree.c.  */
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

void
print_lang_decl (node)
     tree node;
{
}

void walk_lang_decl (node)
     tree node;
{
}

void
print_lang_type (node)
     register tree node;
{
  int first;
  if (! (TREE_CODE (node) == RECORD_TYPE
	 || TREE_CODE (node) == UNION_TYPE))
    return;
  first = 1;
  fputs (" [", outfile);
  if (TYPE_NEEDS_CONSTRUCTOR (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("needs-constructor", outfile);
      first = 0;
    }
  if (TYPE_NEEDS_DESTRUCTOR (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("needs-destructor", outfile);
      first = 0;
    }
  if (TYPE_HAS_CONVERSION (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("has-type-conversion", outfile);
      first = 0;
    }
  if (TYPE_HAS_INT_CONVERSION (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("has-int-conversion", outfile);
      first = 0;
    }
  if (TYPE_HAS_REAL_CONVERSION (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("has-float-conversion", outfile);
      first = 0;
    }
  if (TYPE_HAS_INIT_REF (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("X(X&)", outfile);
      first = 0;
    }
  if (TREE_GETS_NEW (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("gets-new", outfile);
      first = 0;
    }
  if (TREE_GETS_DELETE (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("gets-delete", outfile);
      first = 0;
    }
  if (TYPE_HAS_ASSIGNMENT (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("has=", outfile);
      first = 0;
    }
  if (TYPE_GETS_ASSIGNMENT (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("gets=", outfile);
      first = 0;
    }
  if (TYPE_HAS_ASSIGN_REF (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("this=(X&)", outfile);
      first = 0;
    }
  if (TYPE_GETS_ASSIGN_REF (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("gets=(X&)", outfile);
      first = 0;
    }
  if (TYPE_HAS_WRAPPER (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("wrapper", outfile);
      first = 0;
    }
  if (TYPE_OVERLOADS_METHOD_CALL_EXPR (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("op->()", outfile);
      first = 0;
    }
  if (TYPE_GETS_INIT_AGGR (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("gets X(X, ...)", outfile);
      first = 0;
    }
  if (TYPE_OVERLOADS_CALL_EXPR (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("op()", outfile);
      first = 0;
    }
  if (TYPE_OVERLOADS_ARRAY_REF (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("op[]", outfile);
      first = 0;
    }
  if (TYPE_USES_MULTIPLE_INHERITANCE (node))
    {
      if (!first) putc (' ', outfile);
      fputs ("uses-multiple-inheritance", outfile);
      first = 0;
    }

  fputs ("] ", outfile);
}

void
walk_lang_type (node)
     tree node;
{
  if (! (TREE_CODE (node) == RECORD_TYPE))
    return;

  part ("member functions", CLASSTYPE_METHOD_VEC (node));
  part ("baselinks", CLASSTYPE_BASELINK_VEC (node));
  cpart ("offset", CLASSTYPE_OFFSET (node), ';');
  fprintf (outfile, "n_parents = %d; n_ancestors = %d;",
	   CLASSTYPE_N_BASECLASSES (node),
	   CLASSTYPE_N_SUPERCLASSES (node));
}
