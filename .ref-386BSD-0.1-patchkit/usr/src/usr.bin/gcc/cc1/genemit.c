/* Generate code from machine description to emit insns as rtl.
   Copyright (C) 1987, 1988 Free Software Foundation, Inc.

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

void fatal ();
void fancy_abort ();

int max_opno;
int max_dup_opno;
int register_constraints;
int insn_code_number;

#define max(a, b) ((a) > (b) ? (a) : (b))

void
max_operand_1 (x)
     rtx x;
{
  register RTX_CODE code;
  register int i;
  register int len;
  register char *fmt;

  if (x == 0)
    return;

  code = GET_CODE (x);

  if (code == MATCH_OPERAND && XSTR (x, 2) != 0)
    register_constraints = 1;
  if (code == MATCH_OPERAND || code == MATCH_OPERATOR)
    max_opno = max (max_opno, XINT (x, 0));
  if (code == MATCH_DUP)
    max_dup_opno = max (max_dup_opno, XINT (x, 0));

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == 'e' || fmt[i] == 'u')
	max_operand_1 (XEXP (x, i));
      else if (fmt[i] == 'E')
	{
	  int j;
	  for (j = 0; j < XVECLEN (x, i); j++)
	    max_operand_1 (XVECEXP (x, i, j));
	}
    }
}

int
max_operand_vec (insn, arg)
     rtx insn;
     int arg;
{
  register int len = XVECLEN (insn, arg);
  register int i;

  max_opno = -1;
  max_dup_opno = -1;

  for (i = 0; i < len; i++)
    max_operand_1 (XVECEXP (insn, arg, i));

  return max_opno + 1;
}

void
print_code (code)
     RTX_CODE code;
{
  register char *p1;
  for (p1 = GET_RTX_NAME (code); *p1; p1++)
    {
      if (*p1 >= 'a' && *p1 <= 'z')
	putchar (*p1 + 'A' - 'a');
      else
	putchar (*p1);
    }
}

/* Print a C expression to construct an RTX just like X,
   substituting any operand references appearing within.  */

void
gen_exp (x)
     rtx x;
{
  register RTX_CODE code;
  register int i;
  register int len;
  register char *fmt;

  if (x == 0)
    {
      printf ("0");
      return;
    }

  code = GET_CODE (x);

  switch (code)
    {
    case MATCH_OPERAND:
    case MATCH_DUP:
      printf ("operand%d", XINT (x, 0));
      return;

    case MATCH_OPERATOR:
      printf ("gen_rtx (GET_CODE (operand%d)", XINT (x, 0));
      printf (", %smode", GET_MODE_NAME (GET_MODE (x)));
      for (i = 0; i < XVECLEN (x, 2); i++)
	{
	  printf (",\n\t\t");
	  gen_exp (XVECEXP (x, 2, i));
	}
      printf (")");
      return;

    case ADDRESS:
      fatal ("ADDRESS expression code used in named instruction pattern");

    case PC:
      printf ("pc_rtx");
      return;

    case CC0:
      printf ("cc0_rtx");
      return;

    case CONST_INT:
      if (INTVAL (x) == 0)
	{
	  printf ("const0_rtx");
	  return;
	}
      if (INTVAL (x) == 1)
	{
	  printf ("const1_rtx");
	  return;
	}
    }

  printf ("gen_rtx (");
  print_code (code);
  printf (", %smode", GET_MODE_NAME (GET_MODE (x)));

  fmt = GET_RTX_FORMAT (code);
  len = GET_RTX_LENGTH (code);
  for (i = 0; i < len; i++)
    {
      if (fmt[i] == '0')
	break;
      printf (", ");
      if (fmt[i] == 'e' || fmt[i] == 'u')
	gen_exp (XEXP (x, i));
      else if (fmt[i] == 'i')
	printf ("%d", XINT (x, i));
      else if (fmt[i] == 's')
	printf ("\"%s\"", XSTR (x, i));
      else if (fmt[i] == 'E')
	{
	  int j;
	  printf ("gen_rtvec (%d", XVECLEN (x, i));
	  for (j = 0; j < XVECLEN (x, i); j++)
	    {
	      printf (",\n\t\t");
	      gen_exp (XVECEXP (x, i, j));
	    }
	  printf (")");
	}
      else
	abort ();
    }
  printf (")");
}  

/* Generate the `gen_...' function for a DEFINE_INSN.  */

void
gen_insn (insn)
     rtx insn;
{
  int operands;
  register int i;

  /* Don't mention instructions whose names are the null string.
     They are in the machine description just to be recognized.  */
  if (strlen (XSTR (insn, 0)) == 0)
    return;

  /* Find out how many operands this function has,
     and also whether any of them have register constraints.  */
  register_constraints = 0;
  operands = max_operand_vec (insn, 1);
  if (max_dup_opno >= operands)
    fatal ("match_dup operand number has no match_operand");

  /* Output the function name and argument declarations.  */
  printf ("rtx\ngen_%s (", XSTR (insn, 0));
  for (i = 0; i < operands; i++)
    printf (i ? ", operand%d" : "operand%d", i);
  printf (")\n");
  for (i = 0; i < operands; i++)
    printf ("     rtx operand%d;\n", i);
  printf ("{\n");

  /* Output code to construct and return the rtl for the instruction body */

  if (XVECLEN (insn, 1) == 1)
    {
      printf ("  return ");
      gen_exp (XVECEXP (insn, 1, 0));
      printf (";\n}\n\n");
    }
  else
    {
      printf ("  return gen_rtx (PARALLEL, VOIDmode, gen_rtvec (%d", XVECLEN (insn, 1));
      for (i = 0; i < XVECLEN (insn, 1); i++)
	{
	  printf (",\n\t\t");
	  gen_exp (XVECEXP (insn, 1, i));
	}
      printf ("));\n}\n\n");
    }
}

/* Generate the `gen_...' function for a DEFINE_EXPAND.  */

void
gen_expand (expand)
     rtx expand;
{
  int operands;
  register int i;

  if (strlen (XSTR (expand, 0)) == 0)
    fatal ("define_expand lacks a name");
  if (XVEC (expand, 1) == 0)
    fatal ("define_expand for %s lacks a pattern", XSTR (expand, 0));

  /* Find out how many operands this function has,
     and also whether any of them have register constraints.  */
  register_constraints = 0;

  operands = max_operand_vec (expand, 1);

  /* Output the function name and argument declarations.  */
  printf ("rtx\ngen_%s (", XSTR (expand, 0));
  for (i = 0; i < operands; i++)
    printf (i ? ", operand%d" : "operand%d", i);
  printf (")\n");
  for (i = 0; i < operands; i++)
    printf ("     rtx operand%d;\n", i);
  printf ("{\n");

  /* For each operand referred to only with MATCH_DUPs,
     make a local variable.  */
  for (i = operands; i <= max_dup_opno; i++)
    printf ("  rtx operand%d;\n", i);
  printf ("  rtx operands[%d];\n", max (operands, max_dup_opno + 1));
  printf ("  rtx _val;\n");
  printf ("  start_sequence ();\n");

  /* The fourth operand of DEFINE_EXPAND is some code to be executed
     before the actual construction.
     This code expects to refer to `operands'
     just as the output-code in a DEFINE_INSN does,
     but here `operands' is an automatic array.
     So copy the operand values there before executing it.  */
  if (XSTR (expand, 3))
    {
      /* Output code to copy the arguments into `operands'.  */
      for (i = 0; i < operands; i++)
	printf ("  operands[%d] = operand%d;\n", i, i);

      /* Output the special code to be executed before the sequence
	 is generated.  */
      printf ("%s\n", XSTR (expand, 3));

      /* Output code to copy the arguments back out of `operands'
	 (unless we aren't going to use them at all).  */
      if (XVEC (expand, 1) != 0)
	{
	  for (i = 0; i < operands; i++)
	    printf ("  operand%d = operands[%d];\n", i, i);
	  for (; i <= max_dup_opno; i++)
	    printf ("  operand%d = operands[%d];\n", i, i);
	}
    }

  /* Output code to construct the rtl for the instruction bodies.
     Use emit_insn to add them to the sequence being accumulated.
     But don't do this if the user's code has set `no_more' nonzero.  */

  for (i = 0; i < XVECLEN (expand, 1); i++)
    {
      rtx next = XVECEXP (expand, 1, i);
      if ((GET_CODE (next) == SET && GET_CODE (SET_DEST (next)) == PC)
	  || (GET_CODE (next) == PARALLEL
	      && GET_CODE (XVECEXP (next, 0, 0)) == SET
	      && GET_CODE (SET_DEST (XVECEXP (next, 0, 0))) == PC)
	  || GET_CODE (next) == RETURN)
	printf ("  emit_jump_insn (");
      else if ((GET_CODE (next) == SET && GET_CODE (SET_SRC (next)) == CALL)
	       || GET_CODE (next) == CALL
	       || (GET_CODE (next) == PARALLEL
		   && GET_CODE (XVECEXP (next, 0, 0)) == SET
		   && GET_CODE (SET_SRC (XVECEXP (next, 0, 0))) == CALL)
	       || (GET_CODE (next) == PARALLEL
		   && GET_CODE (XVECEXP (next, 0, 0)) == CALL))
	printf ("  emit_call_insn (");
      else if (GET_CODE (next) == CODE_LABEL)
	printf ("  emit_label (");
      else if (GET_CODE (next) == MATCH_OPERAND
	       || GET_CODE (next) == MATCH_OPERATOR
	       || GET_CODE (next) == MATCH_DUP
	       || GET_CODE (next) == PARALLEL)
	printf ("  emit (");
      else
	printf ("  emit_insn (");
      gen_exp (next);
      printf (");\n");
      if (GET_CODE (next) == SET && GET_CODE (SET_DEST (next)) == PC
	  && GET_CODE (SET_SRC (next)) == LABEL_REF)
	printf ("  emit_barrier ();");
    }

  /* Call `gen_sequence' to make a SEQUENCE out of all the
     insns emitted within this gen_... function.  */

  printf (" _done:\n");
  printf ("  _val = gen_sequence ();\n");
  printf ("  end_sequence ();\n");
  printf ("  return _val;\n}\n\n");
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
  fprintf (stderr, "genemit: ");
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

  /* Assign sequential codes to all entries in the machine description
     in parallel with the tables in insn-output.c.  */

  insn_code_number = 0;

  printf ("/* Generated automatically by the program `genemit'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"expr.h\"\n");
  printf ("#include \"real.h\"\n");
  printf ("#include \"insn-config.h\"\n\n");
  printf ("#include \"insn-flags.h\"\n\n");
  printf ("extern char *insn_operand_constraint[][MAX_RECOG_OPERANDS];\n\n");
  printf ("extern rtx recog_operand[];\n");
  printf ("#define operands emit_operand\n\n");
  printf ("#define FAIL do { end_sequence (); return 0;} while (0)\n\n");
  printf ("#define DONE goto _done\n\n");

  /* Read the machine description.  */

  while (1)
    {
      c = read_skip_spaces (infile);
      if (c == EOF)
	break;
      ungetc (c, infile);

      desc = read_rtx (infile);
      if (GET_CODE (desc) == DEFINE_INSN)
	{
	  gen_insn (desc);
	  ++insn_code_number;
	}
      if (GET_CODE (desc) == DEFINE_EXPAND)
	{
	  gen_expand (desc);
	  ++insn_code_number;
	}
      if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	{
	  ++insn_code_number;
	}
    }

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}
