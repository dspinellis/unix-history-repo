/* Subroutines for insn-output.c for Intel 80386 for masm assembler syntax.
   Copyright (C) 1988 Free Software Foundation, Inc.

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

  
FILE *aux_asm_out_file;

static tree *implicit_declares;
int implicit_declares_max = 0;
int implicit_declares_fillp = 0;


add_to_implicit_list (t)
     tree t;
{ 
  if (implicit_declares_max > implicit_declares_fillp)
    {
      implicit_declares[implicit_declares_fillp++] = t;
    }
  else
    {
      tree *new;
      int i;

      implicit_declares_max = 2 * (implicit_declares_max + 10);
      new = (tree *)(xmalloc (implicit_declares_max * sizeof (tree *)));
      for (i = 0; i < implicit_declares_fillp; i++)
	new[i] = implicit_declares[i];
      implicit_declares = new;
      implicit_declares[implicit_declares_fillp++] = t;
    }
}


write_implicit_declares ()
{
  int i;
  for (i = 0; i < implicit_declares_fillp; i++)
    {
      tree t;
      t = implicit_declares[i];
      if (!IDENTIFIER_GLOBAL_VALUE (t) && IDENTIFIER_IMPLICIT_DECL (t))
	{			/* avoid repeats */
	  IDENTIFIER_IMPLICIT_DECL (t) = 0;
	  fprintf (aux_asm_out_file, "EXTRN ");
	  assemble_name (aux_asm_out_file, IDENTIFIER_POINTER (t));
	  fprintf (aux_asm_out_file, ":NEAR ; implicit\n");
	}
    }
}
      

void
asm_write_decls (decls, toplevel)
     int toplevel;     
     tree decls;     
{
  tree tr, t;
  char *type, *size;
  tr = nreverse (decls);
  for (t = tr; t; t = TREE_CHAIN (t))
    { 
      type = 0;
      size = 0;
      if (TREE_CODE (t) == FUNCTION_DECL
	  && (!toplevel || TREE_ASM_WRITTEN (t)))
	{
	  if (!toplevel && IDENTIFIER_IMPLICIT_DECL (DECL_NAME (t)))
	    { 
	      add_to_implicit_list (DECL_NAME (t));
	      continue;
	    }
	  if (TREE_PUBLIC (t)
	      && (DECL_INITIAL (t)))
	    type = "PUBLIC "; 
	  else {
	    if (TREE_PUBLIC (t) || TREE_EXTERNAL (t))
	      {
		type = "EXTRN ";
		size = "NEAR";
	      }
	  }
	}
      else
	if (TREE_CODE (t) == VAR_DECL)
	  {
	    if (TREE_PUBLIC (t) && DECL_INITIAL (t))
	      type = "PUBLIC ";
	    else if (TREE_EXTERNAL (t))
	      {
		type = "EXTRN ";
		switch (int_size_in_bytes (TREE_TYPE (t)))
		  {
		  case 1: size = "BYTE"; break;
		  case 2: size = "WORD"; break;
		  case 4: size = "DWORD"; break;
		  case 8: size = "QWORD"; break;
		  default: size = "BYTE"; break;
		  }
	      }
	  }
      if (type)
	{ 
	  fputs (type, aux_asm_out_file);
	  putc (' ', aux_asm_out_file);
	  assemble_name (aux_asm_out_file, IDENTIFIER_POINTER (DECL_NAME (t)));
	  if (size)
	    fprintf (aux_asm_out_file, ":%s", size);
	  fprintf (aux_asm_out_file, "\t\t; %d,%d,%d,%d,%d",
		   TREE_EXTERNAL (t), TREE_PUBLIC (t), TREE_STATIC (t),
		   DECL_INITIAL (t), TREE_CODE (t));
	  putc ('\n', aux_asm_out_file);
	}
    }
  nreverse (tr);
}

/*  If we are using a library which does not follow the underscore convention,
(which is absolutely necessary for masm, since it has hundreds of reserved
 words), we do not put an underscore in front of the following names.
It was produced by having a foo /tmp/foo.c which included standard library h
  files, and then using
  cat /tmp/foo.m | sed -e 's/EXTRN  /"/g' -e 's/:[a-zA-Z]+/",/g'  | sort > masm-lib.h
It will be nice when we have our own libraries!
 */

/* #include <string.h> */

char *no_prefix_names[] = {
#include "masm-lib.h"
};

static int
node_compare (a, b)
     char **a, **b;
{
  return strcmp (*a, *b);
}

/*  Masm needs extrn references for ALL symbols written,
    unless they are defined in this file.  However unnecessary extrn
    references to functions, cause their code to be linked in.
    To avoid this we mark function declarations whose names have been
    written.
    Make local varialbes
    */

void
mark_name_used (name)
     char *name;
{
  tree t = get_identifier (name);
  if (IDENTIFIER_GLOBAL_VALUE (t)
      && TREE_CODE (IDENTIFIER_GLOBAL_VALUE (t)) == FUNCTION_DECL)
    TREE_ASM_WRITTEN (IDENTIFIER_GLOBAL_VALUE (t)) = 1;
}

static int number_no_prefix_names = 0;
int use_prefix = 1;

asm_output_labelref (file, name)
     int file;
     char *name;
{
  int i;
  char *ans;
  char *test[1];

  mark_name_used (name);
  ans = 0;
  test[0] = name;
  if (!number_no_prefix_names)
    {
      for (i = 0;; i++)
	if (*(no_prefix_names[i]) == 0)
	  {
	    number_no_prefix_names = i;
	    break;
	  }
    }
  if (use_prefix)
    ans = 
      (char *)
	bsearch (test, no_prefix_names, number_no_prefix_names,
		 sizeof (char *), node_compare);
  if (ans || !use_prefix)
    fprintf (file, name);
  else
    fprintf (file, "_%s", name);
}

void 
asm_library_declare (fun)
     rtx fun;     
{
  tree iden; 
  iden = get_identifier (XSTR (fun, 0));
  if (!TREE_ASM_WRITTEN (iden)) 
    {
      fprintf (aux_asm_out_file, "EXTRN "); 
      assemble_name (aux_asm_out_file, XSTR (fun, 0)); 
      fprintf (aux_asm_out_file, ":NEAR ; library\n"); 
      TREE_ASM_WRITTEN (iden) = 1;
    }
}

/* list of implicit declares identifier_node uid's
   We must track implicit declares, since we cannot call them extern,
   until we are sure they will not be static defined in this file.
 */
