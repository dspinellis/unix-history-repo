/* Call a Lisp function interactively.
   Copyright (C) 1985 Richard M. Stallman.

This file is part of GNU Emacs.

GNU Emacs is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU Emacs General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU Emacs, but only under the conditions described in the
GNU Emacs General Public License.   A copy of this license is
supposed to have been given to you along with GNU Emacs so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  */


#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "commands.h"
#include "window.h"

extern struct Lisp_Vector *CurrentGlobalMap;

extern int num_input_chars;

Lisp_Object Vprefix_arg, Vcurrent_prefix_arg, Qminus;
Lisp_Object Qcall_interactively;
Lisp_Object Vcommand_history;

extern Lisp_Object ml_apply ();
extern Lisp_Object Fread_buffer (), Fread_key_sequence (), Fread_file_name ();

/* ARGSUSED */
DEFUN ("interactive", Finteractive, Sinteractive, 0, UNEVALLED, 0,
  0 /* See auxdoc.c */)
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
  for (tail = exp; LISTP (tail); tail = ptr->cdr)
    {
      ptr = XCONS (tail);
      ptr->car = quotify_arg (ptr->car);
    }
  return exp;
}

char *callint_argfuns[]
    = {"", "point", "mark", "region-beginning", "region-end"};

#define argfuns callint_argfuns

DEFUN ("call-interactively", Fcall_interactively, Scall_interactively, 1, 2, 0,
  "Call FUNCTION, reading args from the terminal,\n\
if the interactive calling specs of FUNCTION request one.\n\
\n\
The function contains a specification of how to do the argument reading.\n\
In the case of user-defined functions, this is specified by placing a call to\n\
the function  interactive  at the top level of the function body.  See  interactive.")
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
	return Fapply (function, Qnil);
    }
  else if (!LISTP (fun))
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
	  return Fapply (function, specs);
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
      if (!NULL (bf_cur->read_only))
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
      for (j = 1; j < i; j++)
	argstrings[j] = XSTRING (visargs[j])->data;

      doprnt (prompt, sizeof prompt, prompt1, argstrings + 1);

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
	  visargs[i] = build_string ("point");
	  varies[i] = 1;
	  break;

	case 'D':		/* Directory name. */
	  args[i] = Fread_file_name (build_string (prompt), Qnil,
				     bf_cur->directory, Qlambda);
	  break;

	case 'f':		/* Existing file name. */
	  args[i] = Fread_file_name (build_string (prompt),
				     Qnil, Qnil, Qlambda);
	  break;

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
	  if (NULL (bf_cur->mark))
	    error ("The mark is not set now");
	  visargs[i] = build_string ("the mark");
	  XFASTINT (args[i]) = marker_position (bf_cur->mark);
	  varies[i] = 2;
	  break;

	case 'n':		/* Read number from minibuffer.  */
	  do
	    args[i] = Fread_minibuffer (build_string (prompt), Qnil);
	  while (XTYPE (args[i]) != Lisp_Int);
	  visargs[i] = last_minibuf_string;
	  break;

	case 'P':		/* Prefix arg in raw form.  Does no I/O.  */
	  args[i] = prefix_arg;
	  XFASTINT (visargs[i]) = (int) "";
	  varies[i] = -1;
	  break;

	case 'p':		/* Prefix arg converted to number.  No I/O. */
	  args[i] = Fprefix_numeric_value (prefix_arg);
	  XFASTINT (visargs[i]) = (int) "";
	  varies[i] = -1;
	  break;

	case 'r':		/* Region, point and mark as 2 args. */
	  if (NULL (bf_cur->mark))
	    error ("The mark is not set now");
	  foo = marker_position (bf_cur->mark);
	  visargs[i] = build_string ("point");
	  XFASTINT (args[i]) = point < foo ? point : foo;
	  varies[i] = 3;
	  visargs[++i] = build_string ("the mark");
	  XFASTINT (args[i]) = point > foo ? point : foo;
	  varies[i] = 4;
	  break;

	case 's':		/* String read via minibuffer.  */
	  args[i] = Fread_string (build_string (prompt), Qnil);
	  break;

	case 'S':		/* Any symbol.  */
	  visargs[i] = read_minibuf_string (Vminibuffer_local_ns_map,
					    Qnil,
					    build_string (prompt));
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
	  error ("Invalid control letter in interactive calling string");
	}

      if (varies[i] == 0)
	arg_from_tty = 1;

      if (NULL (visargs[i]))
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
	  visargs[i] = Fcons (intern (argfuns[varies[i]]), Qnil);
	else
	  visargs[i] = quotify_arg (args[i]);
      Vcommand_history = Fcons (Flist (count + 1, visargs),
				Vcommand_history);
    }

  return Ffuncall (count + 1, args);
}  

DEFUN ("prefix-numeric-value", Fprefix_numeric_value, Sprefix_numeric_value,
  1, 1, 0,
  "Return numeric meaning of raw prefix argument.\n\
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
  else if (LISTP (raw))
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

  DefLispVar ("prefix-arg", &Vprefix_arg,
    "The value of the prefix argument for the next editing command.\n\
It may be a number, or the symbol - for just a minus sign as arg,\n\
or a list whose car is a number for just one or more C-U's\n\
or nil if no argument has been specified.\n\
\n\
You cannot examine this variable to find the argument for this command\n\
since it has been set to nil by the time you can look.\n\
Instead, you should use the variable current-prefix-arg, although\n\
normally commands can get this prefix argument with (interactive \"P\").");

  DefLispVar ("current-prefix-arg", &Vcurrent_prefix_arg,
    "The value of the prefix argument for this editing command.\n\
It may be a number, or the symbol - for just a minus sign as arg,\n\
or a list whose car is a number for just one or more C-U's\n\
or nil if no argument has been specified.\n\
This is what (interactive \"P\") returns.");

  DefLispVar ("command-history", &Vcommand_history,
    "List of recent commands that read arguments from terminal.\n\
Each command is represented as a form to evaluate.");
  Vcommand_history = Qnil;

  defsubr (&Sinteractive);
  defsubr (&Scall_interactively);
  defsubr (&Sprefix_numeric_value);
}
