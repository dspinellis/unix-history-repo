/* Generate from machine description:

   - some #define configuration flags.
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


#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "obstack.h"

struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
extern int xmalloc ();
extern void free ();

/* flags to determine output of machine description dependent #define's.  */
int max_recog_operands_flag;
int max_dup_operands_flag;
int max_clobbers_per_insn_flag;
int register_constraint_flag;

int clobbers_seen_this_insn;
int dup_operands_seen_this_insn;

void fatal ();
void fancy_abort ();

void
walk_insn_part (part)
     rtx part;
{
  register int i, j;
  register RTX_CODE code;
  register char *format_ptr;

  if (part == 0)
    return;

  code = GET_CODE (part);
  switch (code)
    {
    case CLOBBER:
      clobbers_seen_this_insn++;
      break;

    case MATCH_OPERAND:
      if (XINT (part, 0) > max_recog_operands_flag)
	max_recog_operands_flag = XINT (part, 0);
      if (XSTR (part, 2) && *XSTR (part, 2))
	register_constraint_flag = 1;
      return;

    case MATCH_OPERATOR:
      if (XINT (part, 0) > max_recog_operands_flag)
	max_recog_operands_flag = XINT (part, 0);
      /* Now scan the rtl'x in the vector inside the match_operator.  */
      break;

    case LABEL_REF:
      if (GET_CODE (XEXP (part, 0)) == MATCH_OPERAND)
	break;
      return;

    case MATCH_DUP:
      ++dup_operands_seen_this_insn;
      if (XINT (part, 0) > max_recog_operands_flag)
	max_recog_operands_flag = XINT (part, 0);

    case REG: case CONST_INT: case SYMBOL_REF:
    case PC: case CC0:
      return;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (part));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (part)); i++)
    switch (*format_ptr++)
      {
      case 'e':
      case 'u':
	walk_insn_part (XEXP (part, i));
	break;
      case 'E':
	if (XVEC (part, i) != NULL)
	  for (j = 0; j < XVECLEN (part, i); j++)
	    walk_insn_part (XVECEXP (part, i, j));
	break;
      }
}

void
gen_insn (insn)
     rtx insn;
{
  int i;

  /* Walk the insn pattern to gather the #define's status.  */
  clobbers_seen_this_insn = 0;
  dup_operands_seen_this_insn = 0;
  if (XVEC (insn, 1) != 0)
    for (i = 0; i < XVECLEN (insn, 1); i++)
      walk_insn_part (XVECEXP (insn, 1, i));

  if (clobbers_seen_this_insn > max_clobbers_per_insn_flag)
    max_clobbers_per_insn_flag = clobbers_seen_this_insn;
  if (dup_operands_seen_this_insn > max_dup_operands_flag)
    max_dup_operands_flag = dup_operands_seen_this_insn;
}

/* Similar but scan a define_expand.  */

void
gen_expand (insn)
     rtx insn;
{
  int i;

  /* Walk the insn pattern to gather the #define's status.  */

  /* Note that we don't bother recording the number of MATCH_DUPs
     that occur in a gen_expand, because only reload cares about that.  */
  if (XVEC (insn, 1) != 0)
    for (i = 0; i < XVECLEN (insn, 1); i++)
      {
	/* Compute the maximum SETs and CLOBBERS
	   in any one of the sub-insns;
	   don't sum across all of them.  */
	clobbers_seen_this_insn = 0;

	walk_insn_part (XVECEXP (insn, 1, i));

	if (clobbers_seen_this_insn > max_clobbers_per_insn_flag)
	  max_clobbers_per_insn_flag = clobbers_seen_this_insn;
      }
}

void
gen_peephole (peep)
     rtx peep;
{
  int i;

  /* Look through the patterns that are matched
     to compute the maximum operand number.  */
  for (i = 0; i < XVECLEN (peep, 0); i++)
    walk_insn_part (XVECEXP (peep, 0, i));
}

int
xmalloc (size)
{
  register int val = malloc (size);

  if (val == 0)
    fatal ("virtual memory exhausted");

  return val;
}

int
xrealloc (ptr, size)
     char *ptr;
     int size;
{
  int result = realloc (ptr, size);
  if (!result)
    fatal ("virtual memory exhausted");
  return result;
}

void
fatal (s, a1, a2)
     char *s;
{
  fprintf (stderr, "genconfig: ");
  fprintf (stderr, s, a1, a2);
  fprintf (stderr, "\n");
  exit (FATAL_EXIT_CODE);
}

/* More 'friendly' abort that prints the line and file.
   config.h can #define abort fancy_abort if you like that sort of thing.  */

void
fancy_abort ()
{
  fatal ("Internal gcc abort.");
}

int
main (argc, argv)
     int argc;
     char **argv;
{
  rtx desc;
  FILE *infile;
  extern rtx read_rtx ();
  register int c;

  obstack_init (rtl_obstack);

  if (argc <= 1)
    fatal ("No input file name.");

  infile = fopen (argv[1], "r");
  if (infile == 0)
    {
      perror (argv[1]);
      exit (FATAL_EXIT_CODE);
    }

  init_rtl ();

  printf ("/* Generated automatically by the program `genconfig'\n\
from the machine description file `md'.  */\n\n");

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_INSN)
	gen_insn (desc);
      if (GET_CODE (desc) == DEFINE_EXPAND)
	gen_expand (desc);
      if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	gen_peephole (desc);
    }

  /* 3 more than needed for this md file, for the sake of asm constructs.  */
  printf ("\n#define MAX_RECOG_OPERANDS %d\n", max_recog_operands_flag + 4);

  if (max_dup_operands_flag == 0)
    max_dup_operands_flag = 1;
  printf ("\n#define MAX_DUP_OPERANDS %d\n", max_dup_operands_flag);

  if (register_constraint_flag)
    printf ("#define REGISTER_CONSTRAINTS\n");

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
