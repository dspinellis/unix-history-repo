/* expr.c -- arithmetic expression evaluation.

   Copyright (C) 1990, 1991 Free Software Foundation, Inc.

   This file is part of GNU Bash, the Bourne Again SHell.

   Bash is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 1, or (at your option)
   any later version.

   Bash is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with Bash; see the file COPYING.  If not, write to the Free
   Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

 All arithmetic is done as long integers with no checking for overflow
 (though division by 0 is caught and flagged as an error).

 The following operators are handled, grouped into a set of levels in
 order of decreasing precedence.

	"-"			[level 0 (unary negation)]
	"!"			[level 1]
	"*", "/", "%"		[level 2]
	"+", "-"		[level 3]
	"<=", ">=", "<", ">"	[level 4]
	"==", "!="		[level 5]
	"="			[level 6 (assignment)]

 (Note that most of these operators have special meaning to bash, and an
 entire expression should be quoted, e.g. "a=$a+1" or "a=a+1" to ensure
 that it is passed intact to the evaluator when using `let'.  When using
 the $[] form, the text between the `[' and `]' is treated as if in double
 quotes.)

 Sub-expressions within parentheses have a precedence level greater than
 all of the above levels and are evaluated first.  Within a single prece-
 dence group, evaluation is left-to-right, except for the arithmetic
 assignment operator (`='), which is evaluated right-to-left (as in C).

 The expression evaluator returns the value of the expression (assignment
 statements have as a value what is returned by the RHS).  The `let'
 builtin, on the other hand, returns 0 if the last expression evaluates to
 a non-zero, and 1 otherwise.

 Implementation is a recursive-descent parser.

 Chet Ramey
 chet@ins.CWRU.Edu
*/

#include <stdio.h>
#include "shell.h"

#define variable_starter(c) (isletter(c) || (c == '_'))
#define variable_character(c) (isletter(c) || (c == '_') || digit(c))

#if defined (NULL)
#undef NULL
#endif
#define NULL 0

static char	*expression = (char *) NULL;	/* The current expression */
static char	*tp = (char *) NULL;		/* token lexical position */
static int	curtok = 0;			/* the current token */
static int	lasttok = 0;			/* the previous token */
static char	*tokstr = (char *) NULL;	/* current token string */
static int	tokval = 0;			/* current token value */
static jmp_buf	evalbuf;

static void	readtok();			/* lexical analyzer */
static long	assignment(), exp0(), exp1(), exp2(), exp3(), exp4(), exp5();
static long	strlong();
static void	evalerror();

/*
 * Because of the $[...] construct, expressions may include newlines.  This
 * redefines `whitespace' so that a newline is added.
 */

#ifdef whitespace
#undef whitespace
#endif

#define whitespace(c)	((c) == ' ' || (c) == '\t' || (c) == '\n')

/* A structure defining a single expression context. */
typedef struct {
  int curtok, lasttok;
  char *expression, *tp;
  int tokval;
  char *tokstr;
} EXPR_CONTEXT;

/* Global var which contains the stack of expression contexts. */
static EXPR_CONTEXT **expr_stack;
static int expr_depth = 0;	   /* Location in the stack. */
static int expr_stack_size = 0;	   /* Number of slots already allocated. */

/* Size be which the expression stack grows when neccessary. */
#define EXPR_STACK_GROW_SIZE 10

/* Maximum amount of recursion allowed.  This prevents a non-integer
   variable such as "num=num+2" from infinitely adding to itself when
   "let num=num+2" is given.  I have to talk to Chet about this hack. */
#define MAX_EXPR_RECURSION_LEVEL 1024

extern long atol ();

/* The Tokens.  Singing "The Lion Sleeps Tonight". */

#define EQEQ	1	/* "==" */
#define NEQ	2	/* "!=" */
#define LEQ	3	/* "<=" */
#define GEQ	4	/* ">=" */
#define STR	5	/* string */
#define NUM	6	/* number */
#define EQ	'='
#define GT	'>'
#define LT	'<'
#define PLUS	'+'
#define MINUS	'-'
#define MUL	'*'
#define DIV	'/'
#define MOD	'%'
#define NOT	'!'
#define LPAR	'('
#define RPAR	')'

/* Push and save away the contents of the globals describing the
   current expression context. */
static void
pushexp ()
{
  EXPR_CONTEXT *context;

  context = (EXPR_CONTEXT *)xmalloc (sizeof (EXPR_CONTEXT));

  if (expr_depth >= MAX_EXPR_RECURSION_LEVEL)
    evalerror ("expression recursion level exceeded");

  if (expr_depth >= expr_stack_size)
    {
      expr_stack = (EXPR_CONTEXT **)
	xrealloc (expr_stack, (expr_stack_size += EXPR_STACK_GROW_SIZE)
		  * sizeof (EXPR_CONTEXT *));
    }

  context->curtok = curtok;
  context->lasttok = lasttok;
  context->expression = expression;
  context->tp = tp;
  context->tokval = tokval;
  context->tokstr = tokstr;
  expr_stack[expr_depth++] = context;
}

/* Pop the the contents of the expression context stack into the
   globals describing the current expression context. */
static void
popexp ()
{
  EXPR_CONTEXT *context;

  if (expr_depth == 0)
    evalerror ("Recursion stack underflow");

  context = expr_stack[--expr_depth];
  curtok = context->curtok;
  lasttok = context->lasttok;
  expression = context->expression;
  tp = context->tp;
  tokval = context->tokval;
  tokstr = context->tokstr;
  free (context);
}

/* Evaluate EXPR, and return the arithmetic result.

   The `while' loop after the longjmp is caught relies on the above
   implementation of pushexp and popexp leaving in expr_stack[0] the
   values that the variables had when the program started.  That is,
   the first things saved are the initial values of the variables that 
   were assigned at program startup or by the compiler.  Therefore, it is
   safe to let the loop terminate when expr_depth == 0, without freeing up
   any of the expr_depth[0] stuff. */
long
evalexp (expr)
     char *expr;
{
  long val = 0L;
  jmp_buf old_evalbuf;

  if (expr == NULL || *expr == NULL)
    return (0);

  /* Save the value of evalbuf to protect it around possible recursive
     calls to evalexp (). */
  bcopy ((char *)evalbuf, (char *)old_evalbuf, sizeof (jmp_buf));

  if (setjmp (evalbuf))
    {
      if (tokstr)		/* Clean up local allocation. */
	free (tokstr);

      if (expression)
	free (expression);

      while (--expr_depth)
	{
	  if (expr_stack[expr_depth]->tokstr)
	    free (expr_stack[expr_depth]->tokstr);

	  if (expr_stack[expr_depth]->expression)
	    free (expr_stack[expr_depth]->expression);
	}
      longjmp (top_level, DISCARD);
    }

  pushexp ();
  curtok = lasttok = 0;
  expression = savestring (expr);
  tp = expression;

  tokstr = (char *)NULL;
  tokval = 0l;

  readtok ();

  val = assignment ();

  if (curtok != 0) 
    evalerror ("syntax error in expression");

  if (expression)
    free (expression);

  popexp ();

  /* Restore the value of evalbuf so that any subsequent longjmp calls
     will have a valid location to jump to. */
  bcopy ((char *)old_evalbuf, (char *)evalbuf, sizeof (jmp_buf));

  return (val);
}

/* Bind/create a shell variable with the name LHS to the RHS.
   This creates or modifies a variable such that it is an integer.

   This should really be in variables.c, but it is here so that all of the
   expression evaluation stuff is localized.  Since we don't want any
   recursive evaluation from bind_variable() (possible without this code,
   since bind_variable() calls the evaluator for variables with the integer
   attribute set), we temporarily turn off the integer attribute for each
   variable we set here, then turn it back on after binding as necessary. */

void
bind_int_variable (lhs, rhs)
     char *lhs, *rhs;
{
  register SHELL_VAR *v;
  int isint = 0;

  v = find_variable (lhs);
  if (v)
    {
      isint = integer_p (v);
      v->attributes &= ~att_integer;
    }

  v = bind_variable (lhs, rhs);
  if (isint)
    v->attributes |= att_integer;
}

static long
assignment ()
{
  register long	value;
  char *lhs;
  char *rhs;

  value = exp5 ();
  if (curtok == EQ)
    {
      if (lasttok != STR)
	evalerror ("attempted assignment to non-variable");

      lhs = savestring (tokstr);
      readtok ();
      value = assignment ();
      rhs = itos (value);
      bind_int_variable (lhs, rhs);
      free (rhs);
      free (lhs);
      free (tokstr);
      tokstr = (char *)NULL;		/* For freeing on errors. */
    }
  return (value);
}

static long
exp5 ()
{
  register long val1, val2;

  val1 = exp4 ();

  while ((curtok == EQEQ) || (curtok == NEQ))
    {
      int op = curtok;

      readtok ();
      val2 = exp4 ();
      if (op == EQEQ)
	val1 = (val1 == val2);
      else if (op == NEQ)
	val1 = (val1 != val2);
    }
  return (val1);
}

static long
exp4 ()
{
  register long val1, val2;

  val1 = exp3 ();
  while ((curtok == LEQ) ||
	 (curtok == GEQ) ||
	 (curtok == LT) ||
	 (curtok == GT))
    {
      int op = curtok;

      readtok ();
      val2 = exp3 ();

      if (op == LEQ)
	val1 = val1 <= val2;
      else if (op == GEQ)
	val1 = val1 >= val2;
      else if (op == LT)
	val1 = val1 < val2;
      else if (op == GT)
	val1 = val1 > val2;
    }
  return (val1);
}

static long
exp3 ()
{
  register long val1, val2;

  val1 = exp2 ();

  while ((curtok == PLUS) || (curtok == MINUS))
    {
      int op = curtok;

      readtok ();
      val2 = exp2 ();

      if (op == PLUS)
	val1 += val2;
      else if (op == MINUS)
	val1 -= val2;
    }
  return (val1);
}

static long
exp2 ()
{
  register long val1, val2;

  val1 = exp1 ();

  while ((curtok == MUL) ||
         (curtok == DIV) ||
         (curtok == MOD))
    {
      int op = curtok;

      readtok ();

      val2 = exp1 ();

      if (((op == DIV) || (op == MOD)) && (val2 == 0))
	evalerror ("division by 0");

      if (op == MUL)
        val1 *= val2;
      else if (op == DIV)
        val1 /= val2;
      else if (op == MOD)
        val1 %= val2;
    }
  return (val1);
}

static long
exp1 ()
{
  register long val;

  if (curtok == NOT)
    {
      readtok ();
      val = !exp0 ();
    }
  else
    val = exp0 ();

  return (val);
}

static long
exp0 ()
{
  register long val = 0L;

  if (curtok == MINUS)
    {
      readtok ();
      val = - exp0 ();
    }
  else if (curtok == LPAR)
    {
      readtok ();
      val = assignment ();

      if (curtok != RPAR)
	evalerror ("missing `)'");

      /* Skip over closing paren. */
      readtok ();

    }
  else if ((curtok == NUM) || (curtok == STR))
    {
      val = tokval;
      readtok ();
    }
  else
    evalerror ("syntax error in expression");

  return (val);
}

/* Lexical analyzer/token reader for the expression evaluator.  Reads the
   next token and puts its value into curtok, while advancing past it.
   Updates value of tp.  May also set tokval (for number) or tokstr (for
   string). */
static void
readtok ()
{
  register char *cp = tp;
  register int c, c1;

  /* Skip leading whitespace. */
  c = 0;
  while (cp && (c = *cp) && (whitespace(c)))
    cp++;

  if (c)
    cp++;
	
  tp = cp - 1;

  if (c == '\0')
    {
      lasttok = curtok;
      curtok = 0;
      tp = cp;
      return;
    }

  if (variable_starter (c))
    {
      /* Semi-bogus K*rn shell compatibility feature -- variable
	 names not preceded with a dollar sign are shell variables. */
      char *value;

      while (variable_character (c))
	c = *cp++;

      c = *--cp;
      *cp = '\0';

      tokstr = savestring (tp);
      value = get_string_value (tokstr);

      if (value && *value)
	tokval = evalexp (value);
      else
	tokval = 0;

      *cp = c;
      lasttok = curtok;
      curtok = STR;
    }
  else if (digit(c))
    {
      while (digit (c) || isletter (c) || c == '#')
	c = *cp++;

      c = *--cp;
      *cp = '\0';

      tokval = strlong (tp);
      *cp = c;
      lasttok = curtok;
      curtok = NUM;

    }
  else
    {
      c1 = *cp++;
      if ((c == EQ) && (c1 == EQ)) 
	c = EQEQ;
      else if ((c == NOT) && (c1 == EQ))
	c = NEQ;
      else if ((c == GT) && (c1 == EQ))
	c = GEQ;
      else if ((c == LT) && (c1 == EQ))
	c = LEQ;
      else
	cp--;			/* `unget' the character */
      lasttok = curtok;
      curtok = c;
    }
  tp = cp;
}

static void
evalerror (msg)
     char *msg;
{
  builtin_error ("%s: %s (remainder of expression is \"%s\")",
		 expression, msg, (tp && *tp) ? tp : "");
  longjmp (evalbuf, 1);
}

/* Convert a string to a long integer, with an arbitrary base.
   0nnn -> base 8
   0xnn -> base 16
   Anything else: [base#]number (this is from the ISO Pascal spec). */
static long
strlong (num)
     char *num;
{
  register char *s = num;
  register int c;
  int base = 10;
  long val = 0L;

  if (s == NULL || *s == NULL)
    return 0L;

  if (*s == '0')
    {
      s++;

      if (s == NULL || *s == NULL)
	return 0L;
      
       /* Base 16? */
      if (*s == 'x' || *s == 'X')
	{
	  base = 16;
	  s++;
	}
      else
	base = 8;
    }

  for (c = *s++; c; c = *s++)
    {
      if (c == '#')
	{
	  base = (int)val;

	  /* Illegal base specifications are silently reset to base 10.
	     I don't think that this is a good idea? */
	  if (base < 2 || base > 36)
	    base = 10;

	  val = 0L;
	}
      else
	if (isletter(c) || digit(c))
	  {
	    if (digit(c))
	      c = digit_value(c);
	    else if (c >= 'a' && c <= 'z')
	      c -= 'a' - 10;
	    else if (c >= 'A' && c <= 'Z')
	      c -= 'A' - 10;

	    if (c >= base)
	      evalerror ("value too great for base");

	    val = (val * base) + c;
	  }
	else
	  break;
    }
  return (val);
}
