/* Mocklisp compatibility functions for GNU Emacs Lisp interpreter.
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


/* Compatibility for mocklisp */

#include "config.h"
#include "lisp.h"
#include "buffer.h"

/* Now in lisp code ("macrocode...")
* DEFUN ("ml-defun", Fml_defun, Sml_defun, 0, UNEVALLED, 0,
*  "Define mocklisp functions")
*  (args)
*     Lisp_Object args;
* {
*  Lisp_Object elt;
*
*   while (!NULL (args))
*     {
*       elt = Fcar (args);
*       Ffset (Fcar (elt), Fcons (Qmocklisp, Fcdr (elt)));
*       args = Fcdr (args);
*     }
*   return Qnil;
* }
*/

DEFUN ("ml-if", Fml_if, Sml_if, 0, UNEVALLED, 0, "if  for mocklisp programs")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  struct gcpro gcpro1;

  GCPRO1 (args);
  while (!NULL (args))
    {
      val = Feval (Fcar (args));
      args = Fcdr (args);
      if (NULL (args)) break;
      if (XINT (val))
	{
	  val = Feval (Fcar (args));
	  break;
	}
      args = Fcdr (args);
    }
  UNGCPRO;
  return val;
}

/* Now converted to regular "while" by hairier conversion code.
* DEFUN ("ml-while", Fml_while, Sml_while, 1, UNEVALLED, 0, "while  for mocklisp programs")
*   (args)
*      Lisp_Object args;
* {
*   Lisp_Object test, body, tem;
*   struct gcpro gcpro1, gcpro2;
*
*   GCPRO2 (test, body);
*
*   test = Fcar (args);
*   body = Fcdr (args);
*   while (tem = Feval (test), XINT (tem))
*     {
*       QUIT;
*       Fprogn (body);
*    }
*
*   UNGCPRO;
*   return Qnil;
*}

/* This is the main entry point to mocklisp execution.
 When eval sees a mocklisp function being called, it calls here
 with the unevaluated argument list */

Lisp_Object
ml_apply (function, args)
     Lisp_Object function, args;
{
  register int count = specpdl_ptr - specpdl;
  register Lisp_Object val;

  specbind (Qmocklisp_arguments, args);
  val = Fprogn (Fcdr (function));
  unbind_to (count);
  return val;
}

DEFUN ("ml-nargs", Fml_nargs, Sml_nargs, 0, 0, 0, "# arguments to this mocklisp function")
  ()
{
  if (EQ (Vmocklisp_arguments, Qinteractive))
    return make_number (0);
  return Flength (Vmocklisp_arguments);
}

DEFUN ("ml-arg", Fml_arg, Sml_arg, 1, 2, 0, "Argument #N to this mocklisp function.")
  (n, prompt)
     Lisp_Object n, prompt;
{
  if (EQ (Vmocklisp_arguments, Qinteractive))
    return Fread_string (prompt, Qnil);
  CHECK_NUMBER (n, 0);
  XSETINT (n, XINT (n) - 1);	/* Mocklisp likes to be origin-1 */
  return Fcar (Fnthcdr (n, Vmocklisp_arguments));
}

DEFUN ("ml-interactive", Fml_interactive, Sml_interactive, 0, 0, 0,
 "True if this mocklisp function was called interactively.")
  ()
{
  return (EQ (Vmocklisp_arguments, Qinteractive)) ? Qt : Qnil;
}

DEFUN ("ml-provide-prefix-argument", Fml_provide_prefix_argument, Sml_provide_prefix_argument,
  2, UNEVALLED, 0,
  "Evaluate second argument, using first argument as prefix arg value.")
  (args)
     Lisp_Object args;
{
  struct gcpro gcpro1;
  GCPRO1 (args);
  Vcurrent_prefix_arg = Feval (Fcar (args));
  UNGCPRO;
  return Feval (Fcar (Fcdr (args)));
}

DEFUN ("ml-prefix-argument-loop", Fml_prefix_argument_loop, Sml_prefix_argument_loop,
       0, UNEVALLED, 0,
  "")
  (args)
     Lisp_Object args;
{
  register Lisp_Object tem;
  register int i;
  struct gcpro gcpro1;

  /* Set `arg' in case we call a built-in function that looks at it.  Still are a few. */
  if (NULL (Vcurrent_prefix_arg))
    i = 1;
  else
    {
      tem = Vcurrent_prefix_arg;
      if (CONSP (tem))
	tem = Fcar (tem);
      if (EQ (tem, Qminus))
	i = -1;
      else i = XINT (tem);
    }

  GCPRO1 (args);
  while (i-- > 0)
    Fprogn (args);
  UNGCPRO;
  return Qnil;
}

#ifdef NOTDEF /* Now in mlsupport.el */

DEFUN ("ml-substr", Fml_substr, Sml_substr, 3, 3, 0,
  "Return a substring of STRING, starting at index FROM and of length LENGTH.\n\
If either FROM or LENGTH is negative, the length of STRING is added to it.")
  (string, from, to)
     Lisp_Object string, from, to;
{
  CHECK_STRING (string, 0);
  CHECK_NUMBER (from, 1);
  CHECK_NUMBER (to, 2);

  if (XINT (from) < 0)
    XSETINT (from, XINT (from) + XSTRING (string)->size);
  if (XINT (to) < 0)
    XSETINT (to, XINT (to) + XSTRING (string)->size);
  XSETINT (to, XINT (to) + XINT (from));
  return Fsubstring (string, from, to);
}
#endif NOTDEF
DEFUN ("insert-string", Finsert_string, Sinsert_string, 0, MANY, 0,
  "Mocklisp-compatibility insert function.\n\
Like the function `insert' except that any argument that is a number\n\
is converted into a string by expressing it in decimal.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tem;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
    retry:
      if (XTYPE (tem) == Lisp_Int)
	tem = Fint_to_string (tem);
      if (XTYPE (tem) == Lisp_String)
	{
	  insert (XSTRING (tem)->data, XSTRING (tem)->size);
	}
      else
	{
	  tem = wrong_type_argument (Qstringp, tem);
	  goto retry;
	}
    }
  return Qnil;
}


syms_of_mocklisp ()
{
  Qmocklisp = intern ("mocklisp");
  staticpro (&Qmocklisp);

/*defsubr (&Sml_defun);*/
  defsubr (&Sml_if);
/*defsubr (&Sml_while);*/
  defsubr (&Sml_arg);
  defsubr (&Sml_nargs);
  defsubr (&Sml_interactive);
  defsubr (&Sml_provide_prefix_argument);
  defsubr (&Sml_prefix_argument_loop);
/*defsubr (&Sml_substr);*/
  defsubr (&Sinsert_string);
}
