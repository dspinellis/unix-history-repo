/* Generate code from to output assembler insns as recognized from rtl.
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


/* This program reads the machine description for the compiler target machine
   and produces a file containing three things:

   1, An array of strings `insn_template' which is indexed by insn code number
   and contains the template for output of that insn,

   2. An array of ints `insn_n_operands' which is indexed by insn code number
   and contains the number of distinct operands in the pattern for that insn,

   3. An array of ints `insn_n_dups' which is indexed by insn code number
   and contains the number of match_dup's that appear in the insn's pattern.
   This says how many elements of `recog_dup_loc' are significant
   after an insn has been recognized.

   4. An array of arrays of operand constraint strings,
   `insn_operand_constraint',
   indexed first by insn code number and second by operand number,
   containing the constraint for that operand.

   This array is generated only if register constraints appear in 
   match_operand rtx's.

   5. An array of arrays of chars which indicate which operands of
   which insn patterns appear within ADDRESS rtx's.  This array is
   called `insn_operand_address_p' and is generated only if there
   are *no* register constraints in the match_operand rtx's.

   6. An array of arrays of machine modes, `insn_operand_mode',
   indexed first by insn code number and second by operand number,
   containing the machine mode that that operand is supposed to have.
   Also `insn_operand_strict_low', which is nonzero for operands
   contained in a STRICT_LOW_PART.

   7. An array of arrays of int-valued functions, `insn_operand_predicate',
   indexed first by insn code number and second by operand number,
   containing the match_operand predicate for this operand.

   8. An array of functions `insn_gen_function' which, indexed
   by insn code number, gives the function to generate a body
   for that patter, given operands as arguments.

   9. A function `output_insn_hairy' which is called with two arguments
   (an insn code number and a vector of operand value rtx's)
   and returns a template to use for output of that insn.
   This is used only in the cases where the template is not constant.
   These cases are specified by a * at the beginning of the template string
   in the machine description.  They are identified for the sake of
   other parts of the compiler by a zero element in `insn_template'.
  
   10. An array of structures, `insn_machine_info', that gives machine-specific
   information about the insn.

   11. An array of ints, `insn_n_alternatives', that gives the number
   of alternatives in the constraints of each pattern.

The code number of an insn is simply its position in the machine description;
code numbers are assigned sequentially to entries in the description,
starting with code number 0.

Thus, the following entry in the machine description

    (define_insn "clrdf"
      [(set (match_operand:DF 0 "general_operand" "")
	    (const_int 0))]
      ""
      "clrd %0")

assuming it is the 25th entry present, would cause
insn_template[24] to be "clrd %0", and insn_n_operands[24] to be 1.
It would not make an case in output_insn_hairy because the template
given in the entry is a constant (it does not start with `*').  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "obstack.h"

/* No instruction can have more operands than this.
   Sorry for this arbitrary limit, but what machine will
   have an instruction with this many operands?  */

#define MAX_MAX_OPERANDS 40

struct obstack obstack;
struct obstack *rtl_obstack = &obstack;

#define obstack_chunk_alloc xmalloc
#define obstack_chunk_free free
extern int xmalloc ();
extern void free ();

void fatal ();
void fancy_abort ();
void error ();
void mybcopy ();
void mybzero ();

/* insns in the machine description are assigned sequential code numbers
   that are used by insn-recog.c (produced by genrecog) to communicate
   to insn-output.c (produced by this program).  */

int next_code_number;

/* Record in this chain all information that we will output,
   associated with the code number of the insn.  */

struct data
{
  int code_number;
  char *name;
  char *template;		/* string such as "movl %1,%0" */
  int n_operands;		/* Number of operands this insn recognizes */
  int n_dups;			/* Number times match_dup appears in pattern */
  int n_alternatives;		/* Number of alternatives in each constraint */
  struct data *next;
  char *constraints[MAX_MAX_OPERANDS];
  /* Number of alternatives in constraints of operand N.  */
  int op_n_alternatives[MAX_MAX_OPERANDS];
  char *predicates[MAX_MAX_OPERANDS];
  char address_p[MAX_MAX_OPERANDS];
  enum machine_mode modes[MAX_MAX_OPERANDS];
  char strict_low[MAX_MAX_OPERANDS];
  char outfun;			/* Nonzero means this has an output function */
  char *machine_info;		/* machine-specific info string. */
};

/* This variable points to the first link in the chain.  */

struct data *insn_data;

/* Pointer to the last link in the chain, so new elements
   can be added at the end.  */

struct data *end_of_insn_data;

/* Nonzero if any match_operand has a constraint string;
   implies that REGISTER_CONSTRAINTS will be defined
   for this machine description.  */

int have_constraints;

void
output_prologue ()
{

  printf ("/* Generated automatically by the program `genoutput'\n\
from the machine description file `md'.  */\n\n");

  printf ("#include \"config.h\"\n");
  printf ("#include \"rtl.h\"\n");
  printf ("#include \"regs.h\"\n");
  printf ("#include \"hard-reg-set.h\"\n");
  printf ("#include \"real.h\"\n");
  printf ("#include \"conditions.h\"\n");
  printf ("#include \"insn-flags.h\"\n");
  printf ("#include \"insn-config.h\"\n\n");

  printf ("#ifndef __STDC__\n");
  printf ("#define const\n");
  printf ("#endif\n\n");

  printf ("#include \"output.h\"\n");
  printf ("#include \"aux-output.c\"\n\n");

  /* Make sure there is at least a dummy definition of INSN_MACHINE_INFO.  */
  printf ("#ifndef INSN_MACHINE_INFO\n");
  printf ("#define INSN_MACHINE_INFO struct dummy1 {int i;}\n");
  printf ("#endif\n\n");
}

void
output_epilogue ()
{
  register struct data *d;

  printf ("\nchar * const insn_template[] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      if (d->template)
	printf ("    \"%s\",\n", d->template);
      else
	printf ("    0,\n");
    }
  printf ("  };\n");

  printf ("\nchar *(*const insn_outfun[])() =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      if (d->outfun)
	printf ("    output_%d,\n", d->code_number);
      else
	printf ("    0,\n");
    }
  printf ("  };\n");

  printf ("\nrtx (*const insn_gen_function[]) () =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      if (d->name)
	printf ("    gen_%s,\n", d->name);
      else
	printf ("    0,\n");
    }
  printf ("  };\n");

  printf ("\nconst int insn_n_operands[] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      printf ("    %d,\n", d->n_operands);
    }
  printf ("  };\n");

  printf ("\nconst int insn_n_dups[] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      printf ("    %d,\n", d->n_dups);
    }
  printf ("  };\n");

  if (have_constraints)
    {
      printf ("\nchar *const insn_operand_constraint[][MAX_RECOG_OPERANDS] =\n  {\n");
      for (d = insn_data; d; d = d->next)
	{
	  register int i, n = 0, start;
	  printf ("    {");
	  /* Make sure all the operands have the same number of
	     alternatives in their constraints.
	     Let N be that number.  */
	  for (start = 0; start < d->n_operands; start++)
	    if (d->op_n_alternatives[start] > 0)
	      {
		if (n == 0)
		  n = d->op_n_alternatives[start];
		else if (n != d->op_n_alternatives[start])
		  error ("wrong number of alternatives in operand %d of insn number %d",
			 start, d->code_number);
	      }
	  /* Record the insn's overall number of alternatives.  */
	  d->n_alternatives = n;

	  for (i = 0; i < d->n_operands; i++)
	    {
	      if (d->constraints[i] == 0)
		printf (" \"\",");
	      else
		printf (" \"%s\",", d->constraints[i]);
	    }
	  if (d->n_operands == 0)
	    printf (" 0");
	  printf (" },\n");
	}
      printf ("  };\n");
    }
  else
    {
      printf ("\nconst char insn_operand_address_p[][MAX_RECOG_OPERANDS] =\n  {\n");
      for (d = insn_data; d; d = d->next)
	{
	  register int i;
	  printf ("    {");
	  for (i = 0; i < d->n_operands; i++)
	    printf (" %d,", d->address_p[i]);
	  if (d->n_operands == 0)
	    printf (" 0");
	  printf (" },\n");
	}
      printf ("  };\n");
    }

  printf ("\nconst enum machine_mode insn_operand_mode[][MAX_RECOG_OPERANDS] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      register int i;
      printf ("    {");
      for (i = 0; i < d->n_operands; i++)
	printf (" %smode,", GET_MODE_NAME (d->modes[i]));
      if (d->n_operands == 0)
	printf (" VOIDmode");
      printf (" },\n");
    }
  printf ("  };\n");

  printf ("\nconst char insn_operand_strict_low[][MAX_RECOG_OPERANDS] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      register int i;
      printf ("    {");
      for (i = 0; i < d->n_operands; i++)
	printf (" %d,", d->strict_low[i]);
      if (d->n_operands == 0)
	printf (" 0");
      printf (" },\n");
    }
  printf ("  };\n");

  printf ("\nint (*const insn_operand_predicate[][MAX_RECOG_OPERANDS])() =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      register int i;
      printf ("    {");
      for (i = 0; i < d->n_operands; i++)
	printf (" %s,", ((d->predicates[i] && d->predicates[i][0])
			 ? d->predicates[i] : "0"));
      if (d->n_operands == 0)
	printf (" 0");
      printf (" },\n");
    }
  printf ("  };\n");

  printf ("\n#ifndef DEFAULT_MACHINE_INFO\n#define DEFAULT_MACHINE_INFO 0\n");
  printf ("#endif\n\nconst INSN_MACHINE_INFO insn_machine_info[] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      if (d->machine_info)
	printf ("    {%s},\n", d->machine_info);
      else
	printf("     { DEFAULT_MACHINE_INFO },\n");
    }
  printf("  };\n");

  printf ("\nconst int insn_n_alternatives[] =\n  {\n");
  for (d = insn_data; d; d = d->next)
    {
      if (d->n_alternatives)
	printf ("    %d,\n", d->n_alternatives);
      else
	printf("     0,\n");
    }
  printf("  };\n");
}

/* scan_operands (X) stores in max_opno the largest operand
   number present in X, if that is larger than the previous
   value of max_opno.  It stores all the constraints in `constraints'
   and all the machine modes in `modes'.

   THIS_ADDRESS_P is nonzero if the containing rtx was an ADDRESS.
   THIS_STRICT_LOW is nonzero if the containing rtx was a STRICT_LOW_PART.  */

int max_opno;
int num_dups;
char *constraints[MAX_MAX_OPERANDS];
int op_n_alternatives[MAX_MAX_OPERANDS];
char *predicates[MAX_MAX_OPERANDS];
char address_p[MAX_MAX_OPERANDS];
enum machine_mode modes[MAX_MAX_OPERANDS];
char strict_low[MAX_MAX_OPERANDS];

void
scan_operands (part, this_address_p, this_strict_low)
     rtx part;
     int this_address_p;
     int this_strict_low;
{
  register int i, j;
  register RTX_CODE code;
  register char *format_ptr;

  if (part == 0)
    return;

  code = GET_CODE (part);

  if (code == MATCH_OPERAND)
    {
      int opno = XINT (part, 0);
      if (opno > max_opno)
	max_opno = opno;
      if (max_opno >= MAX_MAX_OPERANDS)
	error ("Too many operands (%d) in one instruction pattern.\n",
	       max_opno + 1);
      modes[opno] = GET_MODE (part);
      strict_low[opno] = this_strict_low;
      predicates[opno] = XSTR (part, 1);
      constraints[opno] = XSTR (part, 2);
      if (XSTR (part, 2) != 0 && *XSTR (part, 2) != 0)
	{
	  op_n_alternatives[opno] = n_occurrences (',', XSTR (part, 2)) + 1;
	  have_constraints = 1;
	}
      address_p[opno] = this_address_p;
      return;
    }

  if (code == MATCH_OPERATOR)
    {
      int opno = XINT (part, 0);
      if (opno > max_opno)
	max_opno = opno;
      if (max_opno >= MAX_MAX_OPERANDS)
	error ("Too many operands (%d) in one instruction pattern.\n",
	       max_opno + 1);
      modes[opno] = GET_MODE (part);
      strict_low[opno] = 0;
      predicates[opno] = XSTR (part, 1);
      constraints[opno] = 0;
      address_p[opno] = 0;
      for (i = 0; i < XVECLEN (part, 2); i++)
	scan_operands (XVECEXP (part, 2, i), 0, 0);
      return;
    }

  if (code == MATCH_DUP)
    {
      ++num_dups;
      return;
    }

  if (code == ADDRESS)
    {
      scan_operands (XEXP (part, 0), 1, 0);
      return;
    }

  if (code == STRICT_LOW_PART)
    {
      scan_operands (XEXP (part, 0), 0, 1);
      return;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (part));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (part)); i++)
    switch (*format_ptr++)
      {
      case 'e':
	scan_operands (XEXP (part, i), 0, 0);
	break;
      case 'E':
	if (XVEC (part, i) != NULL)
	  for (j = 0; j < XVECLEN (part, i); j++)
	    scan_operands (XVECEXP (part, i, j), 0, 0);
	break;
      }
}

/* Look at a define_insn just read.  Assign its code number.
   Record on insn_data the template and the number of arguments.
   If the insn has a hairy output action, output a function for now.  */

void
gen_insn (insn)
     rtx insn;
{
  register struct data *d = (struct data *) xmalloc (sizeof (struct data));
  register int i;

  d->code_number = next_code_number++;
  if (XSTR (insn, 0)[0])
    d->name = XSTR (insn, 0);
  else
    d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  if (end_of_insn_data)
    end_of_insn_data->next = d;
  else
    insn_data = d;

  end_of_insn_data = d;

  max_opno = -1;
  num_dups = 0;

  mybzero (constraints, sizeof constraints);
  mybzero (op_n_alternatives, sizeof op_n_alternatives);
  mybzero (predicates, sizeof predicates);
  mybzero (address_p, sizeof address_p);
  mybzero (modes, sizeof modes);
  mybzero (strict_low, sizeof strict_low);
  for (i = 0; i < XVECLEN (insn, 1); i++)
    scan_operands (XVECEXP (insn, 1, i), 0, 0);
  d->n_operands = max_opno + 1;
  d->n_dups = num_dups;
  mybcopy (constraints, d->constraints, sizeof constraints);
  mybcopy (op_n_alternatives, d->op_n_alternatives, sizeof op_n_alternatives);
  mybcopy (predicates, d->predicates, sizeof predicates);
  mybcopy (address_p, d->address_p, sizeof address_p);
  mybcopy (modes, d->modes, sizeof modes);
  mybcopy (strict_low, d->strict_low, sizeof strict_low);
  d->machine_info = XSTR (insn, 4);

  /* We need to consider only the instructions whose assembler code template
     starts with a *.  These are the ones where the template is really
     C code to run to decide on a template to use.
     So for all others just return now.  */

  if (XSTR (insn, 3)[0] != '*')
    {
      d->template = XSTR (insn, 3);
      d->outfun = 0;
      return;
    }

  d->template = 0;
  d->outfun = 1;

  printf ("\nstatic char *\n");
  printf ("output_%d (operands, insn)\n", d->code_number);
  printf ("     rtx *operands;\n");
  printf ("     rtx insn;\n");
  printf ("{\n");
  /* The following is done in a funny way to get around problems in
     VAX-11 "C" on VMS.  It is the equivalent of:
		printf ("%s\n", &(XSTR (insn, 3)[1])); */
  {
    register char *cp = &(XSTR (insn, 3)[1]);
    while (*cp) putchar (*cp++);
    putchar ('\n');
  }
  printf ("}\n");
}

/* Look at a define_peephole just read.  Assign its code number.
   Record on insn_data the template and the number of arguments.
   If the insn has a hairy output action, output it now.  */

void
gen_peephole (peep)
     rtx peep;
{
  register struct data *d = (struct data *) xmalloc (sizeof (struct data));
  register int i;

  d->code_number = next_code_number++;
  d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  if (end_of_insn_data)
    end_of_insn_data->next = d;
  else
    insn_data = d;

  end_of_insn_data = d;

  max_opno = -1;
  mybzero (constraints, sizeof constraints);
  mybzero (op_n_alternatives, sizeof op_n_alternatives);

  /* Get the number of operands by scanning all the
     patterns of the peephole optimizer.
     But ignore all the rest of the information thus obtained.  */
  for (i = 0; i < XVECLEN (peep, 0); i++)
    scan_operands (XVECEXP (peep, 0, i), 0, 0);

  d->n_operands = max_opno + 1;
  d->n_dups = 0;
  mybcopy (constraints, d->constraints, sizeof constraints);
  mybcopy (op_n_alternatives, d->op_n_alternatives, sizeof op_n_alternatives);
  mybzero (d->predicates, sizeof predicates);
  mybzero (d->address_p, sizeof address_p);
  mybzero (d->modes, sizeof modes);
  mybzero (d->strict_low, sizeof strict_low);
  d->machine_info = XSTR (peep, 3);

  /* We need to consider only the instructions whose assembler code template
     starts with a *.  These are the ones where the template is really
     C code to run to decide on a template to use.
     So for all others just return now.  */

  if (XSTR (peep, 2)[0] != '*')
    {
      d->template = XSTR (peep, 2);
      d->outfun = 0;
      return;
    }

  d->template = 0;
  d->outfun = 1;

  printf ("\nstatic char *\n");
  printf ("output_%d (operands, insn)\n", d->code_number);
  printf ("     rtx *operands;\n");
  printf ("     rtx insn;\n");
  printf ("{\n");
  printf ("%s\n", &(XSTR (peep, 2)[1]));
  printf ("}\n");
}

/* Process a define_expand just read.  Assign its code number,
   only for the purposes of `insn_gen_function'.  */

void
gen_expand (insn)
     rtx insn;
{
  register struct data *d = (struct data *) xmalloc (sizeof (struct data));
  register int i;

  d->code_number = next_code_number++;
  if (XSTR (insn, 0)[0])
    d->name = XSTR (insn, 0);
  else
    d->name = 0;

  /* Build up the list in the same order as the insns are seen
     in the machine description.  */
  d->next = 0;
  if (end_of_insn_data)
    end_of_insn_data->next = d;
  else
    insn_data = d;

  end_of_insn_data = d;

  max_opno = -1;
  num_dups = 0;

  /* Scan the operands to get the specified predicates and modes,
     since expand_binop needs to know them.  */

  mybzero (predicates, sizeof predicates);
  mybzero (modes, sizeof modes);
  if (XVEC (insn, 1))
    for (i = 0; i < XVECLEN (insn, 1); i++)
      scan_operands (XVECEXP (insn, 1, i), 0, 0);
  d->n_operands = max_opno + 1;
  mybcopy (predicates, d->predicates, sizeof predicates);
  mybcopy (modes, d->modes, sizeof modes);

  mybzero (d->constraints, sizeof constraints);
  mybzero (d->op_n_alternatives, sizeof op_n_alternatives);
  mybzero (d->address_p, sizeof address_p);
  mybzero (d->strict_low, sizeof strict_low);

  d->n_dups = 0;
  d->template = 0;
  d->outfun = 0;
  d->machine_info = 0;
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
mybzero (b, length)
     register char *b;
     register int length;
{
  while (length-- > 0)
    *b++ = 0;
}

void
mybcopy (b1, b2, length)
     register char *b1;
     register char *b2;
     register int length;
{
  while (length-- > 0)
    *b2++ = *b1++;
}

void
fatal (s, a1, a2)
     char *s;
{
  fprintf (stderr, "genoutput: ");
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

void
error (s, a1, a2)
     char *s;
{
  fprintf (stderr, "genoutput: ");
  fprintf (stderr, s, a1, a2);
  fprintf (stderr, "\n");
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

  output_prologue ();
  next_code_number = 0;
  have_constraints = 0;

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
      if (GET_CODE (desc) == DEFINE_PEEPHOLE)
	gen_peephole (desc);
      if (GET_CODE (desc) == DEFINE_EXPAND)
	gen_expand (desc);
    }

  output_epilogue ();

  fflush (stdout);
  exit (ferror (stdout) != 0 ? FATAL_EXIT_CODE : SUCCESS_EXIT_CODE);
}

int
n_occurrences (c, s)
     char c;
     char *s;
{
  int n = 0;
  while (*s)
    n += (*s++ == c);
  return n;
}
