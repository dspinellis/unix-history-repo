/* expr -- evaluate expressions.
   Copyright (C) 1986, 1991 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Author: Mike Parker.

   This program evaluates expressions.  Each token (operator, operand,
   parenthesis) of the expression must be a seperate argument.  The
   parser used is a reasonably general one, though any incarnation of
   it is language-specific.  It is especially nice for expressions.

   The way it works is (parse_expr is special, parse_expr_1 is
   typical) follows.  Note that when the summary below speaks of a new
   node in this implementation no parse tree is needed; the node is
   evaluated immediately.  Note that one function can handle multiple
   operators all of equal precedence, provided they all associate
   ((x op x) op x).

	global token-list

	parse_expr ()
	while true
		lhs = parse_expr_1 ()
		if next-token = lowest-precendence-operator then
			advance token-list
			rhs = parse_expr_1 ()
			lhs = new-node (lhs, operator, rhs)
			continue while
		else if no more tokens then
			return lhs
		else
			syntax error (unrecognized operator)
		endif
	end while

	parse_expr_1 ()
	while true
		lhs = parse_expr_2 ()
		if next-token = second-lowest-precedence-operator then
			advance token-list
			rhs = parse_expr_1 ()
			lhs = new-node (lhs, operator, rhs)
			continue while
		else
			return lhs
		endif
	end while

	parse_expr_2 () similar, with a different operator
	parse_expr_3 () similar, with a different operator
	etc, until finally

	parse_expr_N ()
	if next-token is a left paren then
		advance token-list
		node = parse_expr () (the top parse_expr)
		check that next token is a right paren (syntax error if not)
		advance token-list
		return node
	else if next-token is a legal operand then
		advance token-list
		return new-leaf-node (operand)
	else
		syntax error (invalid operand)
	endif

   Define EVAL_TRACE to print an evaluation trace.  */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <regex.h>
#include "system.h"

#define NEW(type) ((type *) xmalloc (sizeof (type)))
#define OLD(x) free ((char *) x)

/* the kinds of value we can have */
typedef enum
{
  integer,
  string
} TYPE;

/* a value is.... */
typedef struct
{
  TYPE type;			/* which kind */
  union
  {				/* the value itself */
    int i;
    char *s;
  } u;
} VALUE;

/* The arguments given to the program, minus the program name. */
char **args;

/* The name this program was run with. */
char *program_name;

VALUE *docolon ();
VALUE *eval ();
VALUE *int_value ();
VALUE *str_value ();
char *xstrdup ();
char *strstr ();
char *xmalloc ();
int isstring ();
int nextarg ();
int nomoreargs ();
int null ();
int toarith ();
void error ();
void freev ();
void printv ();
void tostring ();
void trace ();

void
main (argc, argv)
     int argc;
     char **argv;
{
  VALUE *v;

  program_name = argv[0];

  if (argc == 1)
    {
      fprintf (stderr, "Usage: %s expression...\n", argv[0]);
      exit (1);
    }
  args = argv + 1;

  v = eval ();
  if (!nomoreargs ())
    error (2, 0, "syntax error");
  printv (v);

  exit (null (v));
}

/* Return a VALUE for integer I. */
VALUE *
int_value (i)
     int i;
{
  VALUE *v;

  v = NEW (VALUE);
  v->type = integer;
  v->u.i = i;
  return (v);
}

/* Return a VALUE for string S. */
VALUE *
str_value (s)
     char *s;
{
  VALUE *v;

  v = NEW (VALUE);
  v->type = string;
  v->u.s = xstrdup (s);
  return (v);
}

/* Free VALUE V, including structure components. */
void
freev (v)
     VALUE *v;
{
  if (v->type == string)
    {
      free (v->u.s);
    }
  OLD (v);
}

/* Print VALUE V. */
void
printv (v)
     VALUE *v;
{
  switch (v->type)
    {
    case integer:
      printf ("%d\n", v->u.i);
      break;
    case string:
      printf ("%s\n", v->u.s);
      break;
    }
}

/* Return nonzero if V is a null-string or zero-number. */
int
null (v)
     VALUE *v;
{
  switch (v->type)
    {
    case integer:
      return (v->u.i == 0);
    case string:
      return (v->u.s[0] == '\0');
    }
}

/* Return nonzero if V is a string value. */
int
isstring (v)
     VALUE *v;
{
  return (v->type == string);
}

/* Coerce V to a string value (can't fail). */
void
tostring (v)
     VALUE *v;
{
  char *temp;

  switch (v->type)
    {
    case integer:
      temp = xmalloc (4 * (sizeof (int) / sizeof (char)));
      sprintf (temp, "%d", v->u.i);
      v->u.s = temp;
      v->type = string;
      break;
    case string:
      break;
    }
}

/* Coerce V to an integer value.  Return 1 on success, 0 on failure. */
int
toarith (v)
     VALUE *v;
{
  int i;
  int neg;
  char *cp;

  switch (v->type)
    {
    case integer:
      return (1);
    case string:
      i = 0;
      cp = v->u.s;
      if (neg = (*cp == '-'))
	{
	  cp++;
	}
      for (; *cp; cp++)
	{
	  if (isdigit (*cp))
	    {
	      i = (i * 10) + (*cp - '0');
	    }
	  else
	    {
	      return (0);
	    }
	}
      free (v->u.s);
      v->u.i = i * (neg ? -1 : 1);
      v->type = integer;
      return (1);
    }
}

/* Return nonzero if the next token matches STR exactly.
   STR must not be NULL.  */
int
nextarg (str)
     char *str;
{
  if (*args == NULL)
    {
      return 0;
    }
  return (strcmp (*args, str) == 0);
}

/* Return nonzero if there no more tokens. */
int
nomoreargs ()
{
  return (*args == 0);
}

/* the comparison operator handling functions */
#define cmpf(name, rel)				\
int name (l, r) VALUE *l; VALUE *r;		\
{						\
  if (isstring (l) || isstring (r))		\
    {						\
       tostring (l);				\
       tostring (r);				\
       return strcmp (l->u.s, r->u.s) rel 0;	\
    }						\
 else						\
   return l->u.i rel r->u.i;			\
}

cmpf (less_than, <)
cmpf (less_equal, <=)
cmpf (equal, ==)
cmpf (not_equal, !=)
cmpf (greater_equal, >=)
cmpf (greater_than, >)

#undef cmpf

/* the arithmetic operator handling functions */
#define arithf(name, op)			\
int name (l, r) VALUE *l; VALUE *r;		\
{						\
  if (!toarith (l) || !toarith (r))		\
    error (2, 0, "non-numeric argument");	\
  return l->u.i op r->u.i;			\
}

arithf (plus, +)
arithf (minus, -)
arithf (multiply, *)
arithf (divide, /)
arithf (mod, %)

#undef arithf

#ifdef EVAL_TRACE
/* function to print evaluation trace and args remaining */
void
trace (fxn)
     char *fxn;
{
  char **a;

  printf ("%s:", fxn);
  for (a = args; *a; a++)
    {
      printf (" %s", *a);
    }
  putchar ('\n');
}

#endif

/* do the : operator.  SV is the VALUE for the lhs (the string), PV is the
    VALUE for the rhs (the pattern). */
VALUE *
docolon (sv, pv)
     VALUE *sv;
     VALUE *pv;
{
  VALUE *v;
  char *errmsg;
  struct re_pattern_buffer re_buffer;
  struct re_registers re_regs;
  int len;

  tostring (sv);
  tostring (pv);

  len = strlen (pv->u.s);
  re_buffer.allocated = 2 * len;
  re_buffer.buffer = (unsigned char *) xmalloc (re_buffer.allocated);
  re_buffer.translate = 0;
  errmsg = re_compile_pattern (pv->u.s, len, &re_buffer);
  if (errmsg)
    error (2, 0, "%s", errmsg);

  len = re_match (&re_buffer, sv->u.s, strlen (sv->u.s), 0, &re_regs);
  if (len >= 0)
    {
      /* Were \(...\) used? */
      if (re_buffer.re_nsub > 0) /* was (re_regs.start[1] >= 0) */
	{
	  sv->u.s[re_regs.end[1]] = '\0';
	  v = str_value (sv->u.s + re_regs.start[1]);
	}
      else
	{
	  v = int_value (len);
	}
    }
  else
    {
      /* match failed -- return right kind of null */
      if (strstr (pv->u.s, "\\("))
	{
	  v = str_value ("");
	}
      else
	{
	  v = int_value (0);
	}
    }
  free (re_buffer.buffer);
  return (v);
}

/* eval6 -- handles bare operands and ( expr ) syntax */
VALUE *
eval6 ()
{
  VALUE *v;
  VALUE *eval ();

#ifdef EVAL_TRACE
  trace ("eval6");
#endif
  if (nextarg ("("))
    {
      args++;
      v = eval ();
      if (!nextarg (")"))
	{
	  error (2, 0, "syntax error");
	}
      args++;
      return (v);
    }
  else if (nextarg (")"))
    {
      error (2, 0, "syntax error");
    }
  else
    {
      v = str_value (*args++);
      (void) toarith (v);
      return (v);
    }
}

/* eval5 -- handles : operator (pattern matching).  Calls ``docolon'' to do
    the real work. */
VALUE *
eval5 ()
{
  VALUE *l;
  VALUE *r;
  VALUE *v;

#ifdef EVAL_TRACE
  trace ("eval5");
#endif
  l = eval6 ();
  while (1)
    {
      if (nextarg (":"))
	{
	  args++;
	  r = eval6 ();
	  v = docolon (l, r);
	  freev (l);
	  freev (r);
	  l = v;
	}
      else
	{
	  return (l);
	}
    }
}

/* eval4 -- handles *, /, % operators */
VALUE *
eval4 ()
{
  VALUE *l;
  VALUE *r;
  int (*fxn) ();
  int val;

#ifdef EVAL_TRACE
  trace ("eval4");
#endif
  l = eval5 ();
  while (1)
    {
      fxn = 0;
      if (nextarg ("*"))
	{
	  fxn = multiply;
	}
      else if (nextarg ("/"))
	{
	  fxn = divide;
	}
      else if (nextarg ("%"))
	{
	  fxn = mod;
	}
      if (fxn)
	{
	  args++;
	  r = eval5 ();
	  val = (*fxn) (l, r);
	  freev (l);
	  freev (r);
	  l = int_value (val);
	}
      else
	{
	  return (l);
	}
    }
}

/* eval3 -- handles +, - operators */
VALUE *
eval3 ()
{
  VALUE *l;
  VALUE *r;
  int (*fxn) ();
  int val;

#ifdef EVAL_TRACE
  trace ("eval3");
#endif
  l = eval4 ();
  while (1)
    {
      fxn = 0;
      if (nextarg ("+"))
	{
	  fxn = plus;
	}
      else if (nextarg ("-"))
	{
	  fxn = minus;
	}
      if (fxn)
	{
	  args++;
	  r = eval4 ();
	  val = (*fxn) (l, r);
	  freev (l);
	  freev (r);
	  l = int_value (val);
	}
      else
	{
	  return (l);
	}
    }
}

/* eval2 -- handles comparisons */
VALUE *
eval2 ()
{
  VALUE *l;
  VALUE *r;
  int (*fxn) ();
  int val;

#ifdef EVAL_TRACE
  trace ("eval2");
#endif
  l = eval3 ();
  while (1)
    {
      fxn = 0;
      if (nextarg ("<"))
	{
	  fxn = less_than;
	}
      else if (nextarg ("<="))
	{
	  fxn = less_equal;
	}
      else if (nextarg ("="))
	{
	  fxn = equal;
	}
      else if (nextarg ("!="))
	{
	  fxn = not_equal;
	}
      else if (nextarg (">="))
	{
	  fxn = greater_equal;
	}
      else if (nextarg (">"))
	{
	  fxn = greater_than;
	}
      if (fxn)
	{
	  args++;
	  r = eval3 ();
	  val = (*fxn) (l, r);
	  freev (l);
	  freev (r);
	  l = int_value (val);
	}
      else
	{
	  return (l);
	}
    }
}

/* eval1 -- handles & */
VALUE *
eval1 ()
{
  VALUE *l;
  VALUE *r;

#ifdef EVAL_TRACE
  trace ("eval1");
#endif
  l = eval2 ();
  while (1)
    {
      if (nextarg ("&"))
	{
	  args++;
	  r = eval2 ();
	  if (null (l) || null (r))
	    {
	      freev (l);
	      freev (r);
	      l = int_value (0);
	    }
	  else
	    {
	      freev (r);
	    }
	}
      else
	{
	  return (l);
	}
    }
}

/* eval -- handles | */
VALUE *
eval ()
{
  VALUE *l;
  VALUE *r;

#ifdef EVAL_TRACE
  trace ("eval");
#endif
  l = eval1 ();
  while (1)
    {
      if (nextarg ("|"))
	{
	  args++;
	  r = eval1 ();
	  if (null (l))
	    {
	      freev (l);
	      l = r;
	    }
	  else
	    {
	      freev (r);
	    }
	}
      else
	{
	  return (l);
	}
    }
}
