/* Call a Lisp function interactively.
   Copyright (C) 1985, 1986 Free Software Foundation, Inc.

This file is part of GNU Emacs.

GNU Emacs is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "commands.h"
#include "window.h"

Lisp_Object global_map;

extern int num_input_chars;

Lisp_Object Vprefix_arg, Vcurrent_prefix_arg, Qminus;
Lisp_Object Qcall_interactively;
Lisp_Object Vcommand_history;

extern Lisp_Object ml_apply ();
extern Lisp_Object Fread_buffer (), Fread_key_sequence (), Fread_file_name ();

/* This comment supplies the doc string for interactive,
   for make-docfile to see.  We cannot put this in the real DEFUN
   due to limits in the Unix cpp.

DEFUN ("interactive", Ffoo, Sfoo, 0, 0, 0,
 "Specify a way of parsing arguments for interactive use of a function.\n\
For example, write\n\
  (defun fun (arg) \"Doc string\" (interactive \"p\") ...use arg...)\n\
to make arg be the prefix numeric argument when foo is called as a command.\n\
This is actually a declaration rather than a function;\n\
 it tells  call-interactively  how to read arguments\n\
 to pass to the function.\n\
When actually called,  interactive  just returns nil.\n\
\n\
The argument of  interactive  is usually a string containing a code letter\n\
 followed by a prompt.  (Some code letters do not use I/O to get\n\
 the argument and do not need prompts.)  To prompt for multiple arguments,\n\
 give a code letter, its prompt, a newline, and another code letter, etc.\n\
If the argument is not a string, it is evaluated to get a list of\n\
 arguments to pass to the function.\n\
Just  (interactive)  means pass no args when calling interactively.\n\
\nCode letters available are:\n\
a -- Function name: symbol with a function definition.\n\
b -- Name of existing buffer.\n\
B -- Name of buffer, possibly nonexistent.\n\
c -- Character.\n\
C -- Command name: symbol with interactive function definition.\n\
d -- Value of point as number.  Does not do I/O.\n\
D -- Directory name.\n\
f -- Existing file name.\n\
F -- Possibly nonexistent file name.\n\
k -- Key sequence (string).\n\
m -- Value of mark as number.  Does not do I/O.\n\
n -- Number read using minibuffer.\n\
N -- Prefix arg converted to number, or if none, do like code `n'.\n\
p -- Prefix arg converted to number.  Does not do I/O.\n\
P -- Prefix arg in raw form.  Does not do I/O.\n\
r -- Region: point and mark as 2 numeric args, smallest first.  Does no I/O.\n\
s -- Any string.\n\
S -- Any symbol.\n\
v -- Variable name: symbol that is user-variable-p.\n\
x -- Lisp expression read but not evaluated.\n\
X -- Lisp expression read and evaluated.\n\
In addition, if the first character of the string is '*' then an error is\n\
 signaled if the buffer is read-only.\n\
 This happens before reading any arguments.")
*/

/* ARGSUSED */
DEFUN ("interactive", Finteractive, Sinteractive, 0, UNEVALLED, 0,
  0 /* See immediately above */)
  (args)
     Lisp_Object args;
{
  return Qnil;
}

/* Quotify EXP: if EXP is constant, return it.
   If EXP is not constant, return (quote EXP).  */
Lisp_Object
quotify_arg (exp)
     register Lisp_Object exp;
{
  if (XTYPE (exp) != Lisp_Int && XTYPE (exp) != Lisp_String
      && !NULL (exp) && !EQ (exp, Qt))
    return Fcons (Qquote, Fcons (exp, Qnil));

  return exp;
}

/* Modify EXP by quotifying each element (except the first).  */
Lisp_Object
quotify_args (exp)
     Lisp_Object exp;
{
  register Lisp_Object tail;
  register struct Lisp_Cons *ptr;
  for (tail = exp; CONSP (tail); tail = ptr->cdr)
    {
      ptr = XCONS (tail);
      ptr->car = quotify_arg (ptr->car);
    }
  return exp;
}

char *callint_argfuns[]
    = {"", "point", "mark", "region-beginning", "region-end"};

static void
check_mark ()
{
  Lisp_Object tem = Fmarker_buffer (current_buffer->mark);
  if (NULL (tem) || (XBUFFER (tem) != current_buffer))
    error ("The mark is not set now");
}


DEFUN ("call-interactively", Fcall_interactively, Scall_interactively, 1, 2, 0,
  "Call FUNCTION, reading args according to its interactive calling specs.\n\
The function contains a specification of how to do the argument reading.\n\
In the case of user-defined functions, this is specified by placing a call\n\
to the function `interactive' at the top level of the function body.\n\
See `interactive'.\n\
\n\
Optional second arg RECORD-FLAG non-nil\n\
means unconditionally put this command in the command-history.\n\
Otherwise, this is done only if an arg is read using the minibuffer.")
  (function, record)
     Lisp_Object function, record;
{
  Lisp_Object *args, *visargs;
  unsigned char **argstrings;
  Lisp_Object fun;
  Lisp_Object funcar;
  Lisp_Object specs;
  Lisp_Object teml;

  Lisp_Object prefix_arg;
  unsigned char *string;
  unsigned char *tem;
  int *varies;
  register int i, j;
  int count, foo;
  char prompt[100];
  char prompt1[100];
  char *tem1;
  int arg_from_tty = 0;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;
  extern char *index ();

  /* Save this now, since use ofminibuffer will clobber it. */
  prefix_arg = Vcurrent_prefix_arg;

retry:

  fun = function;
  while (XTYPE (fun) == Lisp_Symbol && !EQ (fun, Qunbound)) fun = XSYMBOL (fun)->function;

  if (XTYPE (fun) == Lisp_Subr)
    {
      string = (unsigned char *) XSUBR (fun)->prompt;
      if (!string)
	{
	lose:
	  function = wrong_type_argument (Qcommandp, function, 0);
	  goto retry;
	}
      else if ((int) string == 1)
	return call0 (function);
    }
  else if (!CONSP (fun))
    goto lose;
  else if (funcar = Fcar (fun), EQ (funcar, Qautoload))
    {
      GCPRO2 (function, prefix_arg);
      do_autoload (fun, function);
      UNGCPRO;
      goto retry;
    }
  else if (EQ (funcar, Qlambda))
    {
      specs = Fassq (Qinteractive, Fcdr (Fcdr (fun)));
      if (NULL (specs))
	goto lose;
      specs = Fcar (Fcdr (specs));
      if (XTYPE (specs) == Lisp_String)
	string = XSTRING (specs)->data;
      else
	{
	  i = num_input_chars;
	  specs = Feval (specs);
	  if (i != num_input_chars || !NULL (record))
	    Vcommand_history
	      = Fcons (Fcons (function, quotify_args (Fcopy_sequence (specs))),
		       Vcommand_history);
	  return apply1 (function, specs);
	}
    }
  else if (EQ (funcar, Qmocklisp))
    return ml_apply (fun, Qinteractive);
  else
    goto lose;

  /* Here if function specifies a string to control parsing the defaults */

  /* First character '*' means barf if buffer read-only */
  if (*string == '*')
    { string++;
      if (!NULL (current_buffer->read_only))
	Fbarf_if_buffer_read_only ();
    }

  tem = string;
  for (j = 0; *tem; j++)
    {
      if (*tem == 'r') j++;
      tem = (unsigned char *) index (tem, '\n');
      if (tem) tem++;
      else tem = (unsigned char *) "";
    }
  count = j;

  args = (Lisp_Object *) alloca ((count + 1) * sizeof (Lisp_Object));
  visargs = (Lisp_Object *) alloca ((count + 1) * sizeof (Lisp_Object));
  argstrings = (unsigned char **) alloca ((count + 1) * sizeof (char *));
  varies = (int *) alloca ((count + 1) * sizeof (int));

  for (i = 0; i < (count + 1); i++)
    {
      args[i] = Qnil;
      visargs[i] = Qnil;
      varies[i] = 0;
    }

  GCPRO4 (prefix_arg, function, *args, *visargs);
  gcpro3.nvars = (count + 1);
  gcpro4.nvars = (count + 1);

  tem = string;
   for (i = 1; *tem; i++)
    {
      strncpy (prompt1, tem + 1, sizeof prompt1 - 1);
      prompt1[sizeof prompt1 - 1] = 0;
      tem1 = index (prompt1, '\n');
      if (tem1) *tem1 = 0;
      /* Fill argstrings with a vector of C strings
	 corresponding to the Lisp strings in visargs.  */
      for (j = 1; j < i; j++)
	argstrings[j]
	  = EQ (visargs[j], Qnil)
	    ? (unsigned char *) ""
	    : XSTRING (visargs[j])->data;

      doprnt (prompt, sizeof prompt, prompt1, j - 1, argstrings + 1);

      switch (*tem)
	{
	case 'a':		/* Symbol defined as a function */
	  visargs[i] = Fcompleting_read (build_string (prompt),
					 Vobarray, Qfboundp, Qt, Qnil);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'b':   		/* Name of existing buffer */
	  args[i] = Fcurrent_buffer ();
	  if (EQ (selected_window, minibuf_window))
	    args[i] = Fother_buffer (args[i]);
	  args[i] = Fread_buffer (build_string (prompt), args[i], Qt);
	  break;

	case 'B':		/* Name of buffer, possibly nonexistent */
	  args[i] = Fread_buffer (build_string (prompt),
				  Fother_buffer (Fcurrent_buffer ()), Qnil);
	  break;

        case 'c':		/* Character */
	  message1 (prompt);
	  args[i] = Fread_char ();
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = args[i];
	  visargs[i] = Fchar_to_string (teml);
	  break;

	case 'C':		/* Command: symbol with interactive function */
	  visargs[i] = Fcompleting_read (build_string (prompt),
					 Vobarray, Qcommandp, Qt, Qnil);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'd':		/* Value of point.  Does not do I/O.  */
	  XFASTINT (args[i]) = point;
	  /* visargs[i] = Qnil; */
	  varies[i] = 1;
	  break;

	case 'D':		/* Directory name. */
	  args[i] = Fread_file_name (build_string (prompt), Qnil,
				     current_buffer->directory, Qlambda);
	  break;

	case 'f':		/* Existing file name. */
	  /* On VMS, treat 'f' like 'F', because 'f' fails to work
	     for multivalued logical names or for explicit versions.  */
#ifndef VMS
	  args[i] = Fread_file_name (build_string (prompt),
				     Qnil, Qnil, Qlambda);
	  break;
#endif

	case 'F':		/* Possibly nonexistent file name. */
	  args[i] = Fread_file_name (build_string (prompt),
				     Qnil, Qnil, Qnil);
	  break;

	case 'k':		/* Key sequence (string) */
	  args[i] = Fread_key_sequence (build_string (prompt));
	  teml = args[i];
	  visargs[i] = Fkey_description (teml);
	  break;

	case 'm':		/* Value of mark.  Does not do I/O.  */
	  check_mark ();
	  /* visargs[i] = Qnil; */
	  XFASTINT (args[i]) = marker_position (current_buffer->mark);
	  varies[i] = 2;
	  break;

	case 'N':		/* Prefix arg, else number from minibuffer */
	  if (!NULL (prefix_arg))
	    goto have_prefix_arg;
	case 'n':		/* Read number from minibuffer.  */
	  do
	    args[i] = Fread_minibuffer (build_string (prompt), Qnil);
	  while (XTYPE (args[i]) != Lisp_Int);
	  visargs[i] = last_minibuf_string;
	  break;

	case 'P':		/* Prefix arg in raw form.  Does no I/O.  */
	  args[i] = prefix_arg;
	  /* visargs[i] = Qnil; */
	  varies[i] = -1;
	  break;

	case 'p':		/* Prefix arg converted to number.  No I/O. */
	have_prefix_arg:
	  args[i] = Fprefix_numeric_value (prefix_arg);
	  /* visargs[i] = Qnil; */
	  varies[i] = -1;
	  break;

	case 'r':		/* Region, point and mark as 2 args. */
	  check_mark ();
	  /* visargs[i+1] = Qnil; */
	  foo = marker_position (current_buffer->mark);
	  /* visargs[i] = Qnil; */
	  XFASTINT (args[i]) = point < foo ? point : foo;
	  varies[i] = 3;
	  XFASTINT (args[++i]) = point > foo ? point : foo;
	  varies[i] = 4;
	  break;

	case 's':		/* String read via minibuffer.  */
	  args[i] = Fread_string (build_string (prompt), Qnil);
	  break;

	case 'S':		/* Any symbol.  */
	  visargs[i] = read_minibuf (Vminibuffer_local_ns_map,
				     Qnil,
				     build_string (prompt),
				     0);
	  /* Passing args[i] directly stimulates compiler bug */
	  teml = visargs[i];
	  args[i] = Fintern (teml, Qnil);
	  break;

	case 'v':		/* Variable name: symbol that is
				   user-variable-p. */
	  args[i] = Fread_variable (build_string (prompt));
	  visargs[i] = last_minibuf_string;
	  break;

	case 'x':		/* Lisp expression read but not evaluated */
	  args[i] = Fread_minibuffer (build_string (prompt), Qnil);
	  visargs[i] = last_minibuf_string;
	  break;

	case 'X':		/* Lisp expression read and evaluated */
	  args[i] = Feval_minibuffer (build_string (prompt), Qnil);
	  visargs[i] = last_minibuf_string;
 	  break;

	default:
	  error ("Invalid control letter \"%c\" (%03o) in interactive calling string",
		 *tem, *tem);
	}

      if (varies[i] == 0)
	arg_from_tty = 1;

      if (NULL (visargs[i]) && XTYPE (args[i]) == Lisp_String)
	visargs[i] = args[i];

      tem = (unsigned char *) index (tem, '\n');
      if (tem) tem++;
      else tem = (unsigned char *) "";
    }

  UNGCPRO;

  QUIT;

  args[0] = function;

  if (arg_from_tty || !NULL (record))
    {
      visargs[0] = function;
      for (i = 1; i < count + 1; i++)
	if (varies[i] > 0)
	  visargs[i] = Fcons (intern (callint_argfuns[varies[i]]), Qnil);
	else
	  visargs[i] = quotify_arg (args[i]);
      Vcommand_history = Fcons (Flist (count + 1, visargs),
				Vcommand_history);
    }

  return Ffuncall (count + 1, args);
}  

DEFUN ("prefix-numeric-value", Fprefix_numeric_value, Sprefix_numeric_value,
  1, 1, 0,
  "Return numeric meaning of raw prefix argument ARG.\n\
A raw prefix argument is what you get from (interactive \"P\").")
  (raw)
     Lisp_Object raw;
{
  Lisp_Object val;
  
  if (NULL (raw))
    XFASTINT (val) = 1;
  else if (XTYPE (raw) == Lisp_Symbol)
    {
      XFASTINT (val) = 0;
      XSETINT (val, -1);
    }
  else if (CONSP (raw))
    val = XCONS (raw)->car;
  else if (XTYPE (raw) == Lisp_Int)
    val = raw;
  else
    XFASTINT (val) = 1;

  return val;
}

syms_of_callint ()
{
  Qminus = intern ("-");
  staticpro (&Qminus);

  Qcall_interactively = intern ("call-interactively");
  staticpro (&Qcall_interactively);

  DEFVAR_LISP ("prefix-arg", &Vprefix_arg,
    "The value of the prefix argument for the next editing command.\n\
It may be a number, or the symbol - for just a minus sign as arg,\n\
or a list whose car is a number for just one or more C-U's\n\
or nil if no argument has been specified.\n\
\n\
You cannot examine this variable to find the argument for this command\n\
since it has been set to nil by the time you can look.\n\
Instead, you should use the variable current-prefix-arg, although\n\
normally commands can get this prefix argument with (interactive \"P\").");
  Vprefix_arg = Qnil;

  DEFVAR_LISP ("current-prefix-arg", &Vcurrent_prefix_arg,
    "The value of the prefix argument for this editing command.\n\
It may be a number, or the symbol - for just a minus sign as arg,\n\
or a list whose car is a number for just one or more C-U's\n\
or nil if no argument has been specified.\n\
This is what (interactive \"P\") returns.");
  Vcurrent_prefix_arg = Qnil;

  DEFVAR_LISP ("command-history", &Vcommand_history,
    "List of recent commands that read arguments from terminal.\n\
Each command is represented as a form to evaluate.");
  Vcommand_history = Qnil;

  defsubr (&Sinteractive);
  defsubr (&Scall_interactively);
  defsubr (&Sprefix_numeric_value);
}
