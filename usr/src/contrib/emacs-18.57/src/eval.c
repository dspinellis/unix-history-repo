/* Evaluator for GNU Emacs Lisp interpreter.
   Copyright (C) 1985, 1986, 1987, 1990 Free Software Foundation, Inc.

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

#ifndef standalone
#include "commands.h"
#else
#define FROM_KBD 1
#endif

#include <setjmp.h>

/* This definition is duplicated in alloc.c and keyboard.c */
/* Putting it in lisp.h makes cc bomb out! */

struct backtrace
  {
    struct backtrace *next;
    Lisp_Object *function;
    Lisp_Object *args;	/* Points to vector of args. */
    int nargs;		/* length of vector */
	       /* if nargs is UNEVALLED, args points to slot holding list of unevalled args */
    char evalargs;
    /* Nonzero means call value of debugger when done with this operation. */
    char debug_on_exit;
  };

struct backtrace *backtrace_list;

struct catchtag
  {
    Lisp_Object tag;
    Lisp_Object val;
    struct catchtag *next;
    struct gcpro *gcpro;
    jmp_buf jmp;
    struct backtrace *backlist;
    int lisp_eval_depth;
    int pdlcount;
    int poll_suppress_count;
  };

struct catchtag *catchlist;

Lisp_Object Qautoload, Qmacro, Qexit, Qinteractive, Qcommandp, Qdefun;
Lisp_Object Vquit_flag, Vinhibit_quit, Qinhibit_quit;
Lisp_Object Qmocklisp_arguments, Vmocklisp_arguments, Qmocklisp;
Lisp_Object Qand_rest, Qand_optional;

/* Non-nil means record all fset's and provide's, to be undone
   if the file being autoloaded is not fully loaded.
   They are recorded by being consed onto the front of Vautoload_queue:
   (FUN . ODEF) for a defun, (OFEATURES . nil) for a provide.  */

Lisp_Object Vautoload_queue;

/* Current number of specbindings allocated in specpdl.  */

int specpdl_size;

/* Pointer to beginning of specpdl.  */

struct specbinding *specpdl;

/* Pointer to first unused element in specpdl.  */

struct specbinding *specpdl_ptr;

/* Maximum size allowed for specpdl allocation */

int max_specpdl_size;

/* Depth in Lisp evaluations and function calls.  */

int lisp_eval_depth;

/* Maximum allowed depth in Lisp evaluations and function calls.  */

int max_lisp_eval_depth;

/* Nonzero means enter debugger before next function call */
int debug_on_next_call;

/* Nonzero means display a backtrace if an error
 is handled by the command loop's error handler. */
int stack_trace_on_error;

/* Nonzero means enter debugger if an error
 is handled by the command loop's error handler. */
int debug_on_error;

/* Nonzero means enter debugger if a quit signal
 is handled by the command loop's error handler. */
int debug_on_quit;

Lisp_Object Vdebugger;

void specbind (), unbind_to (), record_unwind_protect ();

Lisp_Object funcall_lambda ();
extern Lisp_Object ml_apply (); /* Apply a mocklisp function to unevaluated argument list */

init_eval_once ()
{
  specpdl_size = 50;
  specpdl = (struct specbinding *) malloc (specpdl_size * sizeof (struct specbinding));
  max_specpdl_size = 600;
  max_lisp_eval_depth = 200;
}

init_eval ()
{
  specpdl_ptr = specpdl;
  catchlist = 0;
  handlerlist = 0;
  backtrace_list = 0;
  Vquit_flag = Qnil;
  debug_on_next_call = 0;
  lisp_eval_depth = 0;
}

Lisp_Object
call_debugger (arg)
     Lisp_Object arg;
{
  if (lisp_eval_depth + 20 > max_lisp_eval_depth)
    max_lisp_eval_depth = lisp_eval_depth + 20;
  if (specpdl_size + 40 > max_specpdl_size)
    max_specpdl_size = specpdl_size + 40;
  debug_on_next_call = 0;
  return apply1 (Vdebugger, arg);
}

do_debug_on_call (code)
     Lisp_Object code;
{
  debug_on_next_call = 0;
  backtrace_list->debug_on_exit = 1;
  call_debugger (Fcons (code, Qnil));
}

/* NOTE!!! Every function that can call EVAL must protect its args
 and temporaries from garbage collection while it needs them.
 The definition of `For' shows what you have to do.  */

DEFUN ("or", For, Sor, 0, UNEVALLED, 0,
  "Eval args until one of them yields non-NIL, then return that value.\n\
The remaining args are not evalled at all.\n\
If all args return NIL, return NIL.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  Lisp_Object args_left;
  struct gcpro gcpro1;

  if (NULL(args))
    return Qnil;

  args_left = args;
  GCPRO1 (args_left);

  do
    {
      val = Feval (Fcar (args_left));
      if (!NULL (val))
	break;
      args_left = Fcdr (args_left);
    }
  while (!NULL(args_left));

  UNGCPRO;
  return val;
}

DEFUN ("and", Fand, Sand, 0, UNEVALLED, 0,
  "Eval args until one of them yields NIL, then return NIL.\n\
The remaining args are not evalled at all.\n\
If no arg yields NIL, return the last arg's value.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val;
  Lisp_Object args_left;
  struct gcpro gcpro1;

  if (NULL(args))
    return Qt;

  args_left = args;
  GCPRO1 (args_left);

  do
    {
      val = Feval (Fcar (args_left));
      if (NULL (val))
	break;
      args_left = Fcdr (args_left);
    }
  while (!NULL(args_left));

  UNGCPRO;
  return val;
}

DEFUN ("if", Fif, Sif, 2, UNEVALLED, 0,
  "(if C T E...) if C yields non-NIL do T, else do E...\n\
Returns the value of T or the value of the last of the E's.\n\
There may be no E's; then if C yields NIL, the value is NIL.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object cond;
  struct gcpro gcpro1;

  GCPRO1 (args);
  cond = Feval (Fcar (args));
  UNGCPRO;

  if (!NULL (cond))
    return Feval (Fcar (Fcdr (args)));
  return Fprogn (Fcdr (Fcdr (args)));
}

DEFUN ("cond", Fcond, Scond, 0, UNEVALLED, 0,
  "(cond CLAUSES...) tries each clause until one succeeds.\n\
Each clause looks like (C BODY...).  C is evaluated\n\
and, if the value is non-nil, this clause succeeds:\n\
then the expressions in BODY are evaluated and the last one's\n\
value is the value of the cond expression.\n\
If a clause looks like (C), C's value if non-nil is returned from cond.\n\
If no clause succeeds, cond returns nil.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object clause, val;
  struct gcpro gcpro1;

  GCPRO1 (args);
  while (!NULL (args))
    {
      clause = Fcar (args);
      val = Feval (Fcar (clause));
      if (!NULL (val))
	{
	  if (!EQ (XCONS (clause)->cdr, Qnil))
	    val = Fprogn (XCONS (clause)->cdr);
	  break;
	}
      args = XCONS (args)->cdr;
    }
  UNGCPRO;

  return val;
}

DEFUN ("progn", Fprogn, Sprogn, 0, UNEVALLED, 0,
  "Eval arguments in sequence, and return the value of the last one.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object val, tem;
  Lisp_Object args_left;
  struct gcpro gcpro1;

  /* In Mocklisp code, symbols at the front of the progn arglist
   are to be bound to zero. */
  if (!EQ (Vmocklisp_arguments, Qt))
    {
      val = make_number (0);
      while (!NULL (args) && (tem = Fcar (args), XTYPE (tem) == Lisp_Symbol))
	{
	  QUIT;
	  specbind (tem, val), args = Fcdr (args);
	}
    }

  if (NULL(args))
    return Qnil;

  args_left = args;
  GCPRO1 (args_left);

  do
    {
      val = Feval (Fcar (args_left));
      args_left = Fcdr (args_left);
    }
  while (!NULL(args_left));

  UNGCPRO;
  return val;
}

DEFUN ("prog1", Fprog1, Sprog1, 1, UNEVALLED, 0,
  "Eval arguments in sequence, then return the FIRST arg's value.\n\
This value is saved during the evaluation of the remaining args,\n\
whose values are discarded.")
  (args)
     Lisp_Object args;
{
  Lisp_Object val;
  register Lisp_Object args_left;
  struct gcpro gcpro1, gcpro2;
  register int argnum = 0;

  if (NULL(args))
    return Qnil;

  args_left = args;
  val = Qnil;
  GCPRO2 (args, val);

  do
    {
      if (!(argnum++))
        val = Feval (Fcar (args_left));
      else
	Feval (Fcar (args_left));
      args_left = Fcdr (args_left);
    }
  while (!NULL(args_left));

  UNGCPRO;
  return val;
}

DEFUN ("prog2", Fprog2, Sprog2, 2, UNEVALLED, 0,
  "Eval arguments in sequence, then return the SECOND arg's value.\n\
This value is saved during the evaluation of the remaining args,\n\
whose values are discarded.")
  (args)
     Lisp_Object args;
{
  Lisp_Object val;
  register Lisp_Object args_left;
  struct gcpro gcpro1, gcpro2;
  register int argnum = -1;

  val = Qnil;

  if (NULL(args))
    return Qnil;

  args_left = args;
  val = Qnil;
  GCPRO2 (args, val);

  do
    {
      if (!(argnum++))
        val = Feval (Fcar (args_left));
      else
	Feval (Fcar (args_left));
      args_left = Fcdr (args_left);
    }
  while (!NULL(args_left));

  UNGCPRO;
  return val;
}

DEFUN ("setq", Fsetq, Ssetq, 0, UNEVALLED, 0,
  "(setq SYM VAL SYM VAL ...) sets each SYM to the value of its VAL.\n\
The SYMs are not evaluated.  Thus (setq x y) sets x to the value of y.\n\
Each SYM is set before the next VAL is computed.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object args_left;
  register Lisp_Object val, sym;
  struct gcpro gcpro1;

  if (NULL(args))
    return Qnil;

  args_left = args;
  GCPRO1 (args);

  do
    {
      val = Feval (Fcar (Fcdr (args_left)));
      sym = Fcar (args_left);
      Fset (sym, val);
      args_left = Fcdr (Fcdr (args_left));
    }
  while (!NULL(args_left));

  UNGCPRO;
  return val;
}
     
DEFUN ("quote", Fquote, Squote, 1, UNEVALLED, 0,
  "Return the argument, without evaluating it.  (quote x)  yields  x.")
  (args)
     Lisp_Object args;
{
  return Fcar (args);
}
     
DEFUN ("function", Ffunction, Sfunction, 1, UNEVALLED, 0,
  "Quote a function object.\n\
Equivalent to the quote function in the interpreter,\n\
but causes the compiler to compile the argument as a function\n\
if it is not a symbol.")
  (args)
     Lisp_Object args;
{
  return Fcar (args);
}

DEFUN ("interactive-p", Finteractive_p, Sinteractive_p, 0, 0, 0,
  "Return t if function in which this appears was called interactively.\n\
This means that the function was called with call-interactively (which\n\
includes being called as the binding of a key)\n\
and input is currently coming from the keyboard (not in keyboard macro).")
  ()
{
  register struct backtrace *btp;
  register Lisp_Object fun;

  if (!FROM_KBD)
    return Qnil;
  /* Skip the frame of interactive-p itself (if interpreted)
     or the frame of byte-code (if called from compiled function).  */
  for (btp = backtrace_list->next;
       btp && (btp->nargs == UNEVALLED
	       || EQ (*btp->function, Qbytecode));
       btp = btp->next)
    {}
  /* btp now points at the frame of the innermost function
     that DOES eval its args.
     If it is a built-in function (such as load or eval-region)
     return nil.  */
  fun = *btp->function;
  while (XTYPE (fun) == Lisp_Symbol)
    {
      QUIT;
      fun = Fsymbol_function (fun);
    }
  if (XTYPE (fun) == Lisp_Subr)
    return Qnil;
  /* btp points to the frame of a Lisp function that called interactive-p.
     Return t if that function was called interactively.  */
  if (btp && btp->next && EQ (*btp->next->function, Qcall_interactively))
    return Qt;
  return Qnil;
}

DEFUN ("defun", Fdefun, Sdefun, 2, UNEVALLED, 0,
  "(defun NAME ARGLIST [DOCSTRING] BODY...) defines NAME as a function.\n\
The definition is (lambda ARGLIST [DOCSTRING] BODY...).\n\
See also the function  interactive .")
  (args)
     Lisp_Object args;
{
  register Lisp_Object fn_name;
  register Lisp_Object defn;

  fn_name = Fcar (args);
  defn = Fcons (Qlambda, Fcdr (args));
  if (!NULL (Vpurify_flag))
    defn = Fpurecopy (defn);
  Ffset (fn_name, defn);
  return fn_name;
}

DEFUN ("defmacro", Fdefmacro, Sdefmacro, 2, UNEVALLED, 0,
  "(defmacro NAME ARGLIST [DOCSTRING] BODY...) defines NAME as a macro.\n\
The definition is (macro lambda ARGLIST [DOCSTRING] BODY...).\n\
When the macro is called, as in (NAME ARGS...),\n\
the function (lambda ARGLIST BODY...) is applied to\n\
the list ARGS... as it appears in the expression,\n\
and the result should be a form to be evaluated instead of the original.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object fn_name;
  register Lisp_Object defn;

  fn_name = Fcar (args);
  defn = Fcons (Qmacro, Fcons (Qlambda, Fcdr (args)));
  if (!NULL (Vpurify_flag))
    defn = Fpurecopy (defn);
  Ffset (fn_name, defn);
  return fn_name;
}

DEFUN ("defvar", Fdefvar, Sdefvar, 1, UNEVALLED, 0,
  "(defvar SYMBOL INITVALUE DOCSTRING) defines SYMBOL as an advertised variable.\n\
INITVALUE is evaluated, and used to set SYMBOL, only if SYMBOL's value is void.\n\
INITVALUE and DOCSTRING are optional.\n\
If DOCSTRING starts with *, this variable is identified as a user option.\n\
 This means that M-x set-variable and M-x edit-options recognize it.\n\
If INITVALUE is missing, SYMBOL's value is not set.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object sym, tem;

  sym = Fcar (args);
  tem = Fcdr (args);
  if (!NULL (tem))
    {
      tem = Fboundp (sym);
      if (NULL (tem))
	Fset (sym, Feval (Fcar (Fcdr (args))));
    }
  tem = Fcar (Fcdr (Fcdr (args)));
  if (!NULL (tem))
    {
      if (!NULL (Vpurify_flag))
	tem = Fpurecopy (tem);
      Fput (sym, Qvariable_documentation, tem);
    }
  return sym;
}

DEFUN ("defconst", Fdefconst, Sdefconst, 2, UNEVALLED, 0,
  "(defconst SYMBOL INITVALUE DOCSTRING) defines SYMBOL as a constant variable.\n\
The intent is that programs do not change this value (but users may).\n\
Always sets the value of SYMBOL to the result of evalling INITVALUE.\n\
DOCSTRING is optional.\n\
If DOCSTRING starts with *, this variable is identified as a user option.\n\
 This means that M-x set-variable and M-x edit-options recognize it.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object sym, tem;

  sym = Fcar (args);
  Fset (sym, Feval (Fcar (Fcdr (args))));
  tem = Fcar (Fcdr (Fcdr (args)));
  if (!NULL (tem))
    {
      if (!NULL (Vpurify_flag))
	tem = Fpurecopy (tem);
      Fput (sym, Qvariable_documentation, tem);
    }
  return sym;
}

DEFUN ("user-variable-p", Fuser_variable_p, Suser_variable_p, 1, 1, 0,
  "Returns t if VARIABLE is intended to be set and modified by users,\n\
as opposed to by programs.\n\
Determined by whether the first character of the documentation\n\
for the variable is \"*\"")
  (variable)
     Lisp_Object variable;
{
  Lisp_Object documentation;
  
  documentation = Fget (variable, Qvariable_documentation);
  if (XTYPE (documentation) == Lisp_Int && XINT (documentation) < 0)
    return Qt;
  if ((XTYPE (documentation) == Lisp_String) &&
      ((unsigned char) XSTRING (documentation)->data[0] == '*'))
    return Qt;
  return Qnil;
}  

DEFUN ("let*", FletX, SletX, 1, UNEVALLED, 0,
  "(let* VARLIST BODY...) binds variables according to VARLIST then executes BODY.\n\
The value of the last form in BODY is returned.\n\
Each element of VARLIST is a symbol (which is bound to NIL)\n\
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).\n\
Each VALUEFORM can refer to the symbols already bound by this VARLIST.")
  (args)
     Lisp_Object args;
{
  Lisp_Object varlist, val, elt;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1, gcpro2, gcpro3;

  GCPRO3 (args, elt, varlist);

  varlist = Fcar (args);
  while (!NULL (varlist))
    {
      QUIT;
      elt = Fcar (varlist);
      if (XTYPE (elt) == Lisp_Symbol)
	specbind (elt, Qnil);
      else
	{
	  val = Feval (Fcar (Fcdr (elt)));
	  specbind (Fcar (elt), val);
	}
      varlist = Fcdr (varlist);
    }
  UNGCPRO;
  val = Fprogn (Fcdr (args));
  unbind_to (count);
  return val;
}

DEFUN ("let", Flet, Slet, 1, UNEVALLED, 0,
  "(let VARLIST BODY...) binds variables according to VARLIST then executes BODY.\n\
The value of the last form in BODY is returned.\n\
Each element of VARLIST is a symbol (which is bound to NIL)\n\
or a list (SYMBOL VALUEFORM) (which binds SYMBOL to the value of VALUEFORM).\n\
All the VALUEFORMs are evalled before any symbols are bound.")
  (args)
     Lisp_Object args;
{
  Lisp_Object *temps, tem;
  register Lisp_Object elt, varlist;
  int count = specpdl_ptr - specpdl;
  register int argnum;
  struct gcpro gcpro1, gcpro2;

  varlist = Fcar (args);

  /* Make space to hold the values to give the bound variables */
  elt = Flength (varlist);
  temps = (Lisp_Object *) alloca (XFASTINT (elt) * sizeof (Lisp_Object));

  /* Compute the values and store them in `temps' */

  GCPRO2 (args, *temps);
  gcpro2.nvars = 0;

  for (argnum = 0; !NULL (varlist); varlist = Fcdr (varlist))
    {
      QUIT;
      elt = Fcar (varlist);
      if (XTYPE (elt) == Lisp_Symbol)
	temps [argnum++] = Qnil;
      else
	temps [argnum++] = Feval (Fcar (Fcdr (elt)));
      gcpro2.nvars = argnum;
    }
  UNGCPRO;

  varlist = Fcar (args);
  for (argnum = 0; !NULL (varlist); varlist = Fcdr (varlist))
    {
      elt = Fcar (varlist);
      tem = temps[argnum++];
      if (XTYPE (elt) == Lisp_Symbol)
	specbind (elt, tem);
      else
	specbind (Fcar (elt), tem);
    }

  elt = Fprogn (Fcdr (args));
  unbind_to (count);
  return elt;
}

DEFUN ("while", Fwhile, Swhile, 1, UNEVALLED, 0,
  "(while TEST BODY...) if TEST yields non-NIL, execute the BODY forms and repeat.")
  (args)
     Lisp_Object args;
{
  Lisp_Object test, body, tem;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (test, body);

  test = Fcar (args);
  body = Fcdr (args);
  while (tem = Feval (test), !NULL (tem))
    {
      QUIT;
      Fprogn (body);
    }

  UNGCPRO;
  return Qnil;
}

DEFUN ("macroexpand", Fmacroexpand, Smacroexpand, 1, 2, 0,
  "If FORM is a macro call, expand it.\n\
If the result of expansion is another macro call, expand it, etc.\n\
Return the ultimate expansion.\n\
The second optional arg ENVIRONMENT species an environment of macro\n\
definitions to shadow the loaded ones for use in file byte-compilation.")
  (form, env)
     register Lisp_Object form;
     Lisp_Object env;
{
  register Lisp_Object expander, sym, def, tem;

  while (1)
    {
      /* Come back here each time we expand a macro call,
	 in case it expands into another macro call.  */
      if (XTYPE (form) != Lisp_Cons)
	break;
      sym = XCONS (form)->car;
      if (XTYPE (sym) != Lisp_Symbol)
	break;
      /* Trace symbols aliases to other symbols
	 until we get a symbol that is not an alias.  */
      while (1)
	{
	  QUIT;
	  tem = Fassq (sym, env);
	  if (NULL (tem))
	    {
	      def = XSYMBOL (sym)->function;
	      if (XTYPE (def) == Lisp_Symbol && !EQ (def, Qunbound))
		sym = def;
	      else
		break;
	    }
	  else
	    {
	      if (XTYPE (tem) == Lisp_Cons
		  && XTYPE (XCONS (tem)->cdr) == Lisp_Symbol)
		sym = XCONS (tem)->cdr;
	      else
		break;
	    }
	}
      /* Right now TEM is the result from SYM in ENV,
	 and if TEM is nil then DEF is SYM's function definition.  */
      if (NULL (tem))
	{
	  /* SYM is not mentioned in ENV.
	     Look at its function definition.  */
	  if (EQ (def, Qunbound)
	      || XTYPE (def) != Lisp_Cons)
	    /* Not defined or definition not suitable */
	    break;
	  if (EQ (XCONS (def)->car, Qautoload))
	    {
	      /* Autoloading function: will it be a macro when loaded?  */
	      tem = Fcar (Fnthcdr (make_number (4), def));
	      if (NULL (tem))
		break;
	      /* Yes, load it and try again.  */
	      do_autoload (def, sym);
	      continue;
	    }
	  else if (!EQ (XCONS (def)->car, Qmacro))
	    break;
	  else expander = XCONS (def)->cdr;
	}
      else
	{
	  expander = XCONS (tem)->cdr;
	  if (NULL (expander))
	    break;
	}
      form = apply1 (expander, XCONS (form)->cdr);
    }
  return form;
}

DEFUN ("catch", Fcatch, Scatch, 1, UNEVALLED, 0,
  "(catch TAG BODY...) perform BODY allowing nonlocal exits using (throw TAG).\n\
TAG is evalled to get the tag to use.  throw  to that tag exits this catch.\n\
Then the BODY is executed.  If no  throw  happens, the value of the last BODY\n\
form is returned from  catch.  If a  throw  happens, it specifies the value to\n\
return from  catch.")
  (args)
     Lisp_Object args;
{
  register Lisp_Object tag;
  struct gcpro gcpro1;

  GCPRO1 (args);
  tag = Feval (Fcar (args));
  UNGCPRO;
  return internal_catch (tag, Fprogn, Fcdr (args));
}

/* Set up a catch, then call C function FUNC on argument ARG.
   FUNC should return a Lisp_Object.
   This is how catches are done from within C code. */

Lisp_Object
internal_catch (tag, func, arg)
     Lisp_Object tag;
     Lisp_Object (*func) ();
     Lisp_Object arg;
{
  /* This structure is made part of the chain `catchlist'.  */
  struct catchtag c;

  /* Fill in the components of c, and put it on the list.  */
  c.next = catchlist;
  c.tag = tag;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.lisp_eval_depth = lisp_eval_depth;
  c.poll_suppress_count = poll_suppress_count;
  c.pdlcount = specpdl_ptr - specpdl;
  c.gcpro = gcprolist;
  catchlist = &c;

  /* Call FUNC.  */
  if (! _setjmp (c.jmp))
    c.val = (*func) (arg);

  /* Throw works by a longjmp that comes right here.  */
  catchlist = c.next;
  return c.val;
}

/* Discard from the catchlist all catch tags back through CATCH.
   Before each catch is discarded, unbind all special bindings
   made within that catch.  Also, when discarding a catch that
   corresponds to a condition handler, discard that handler.

   At the end, restore some static info saved in CATCH.

   This is used for correct unwinding in Fthrow and Fsignal,
   before doing the longjmp that actually destroys the stack frames
   in which these handlers and catches reside.  */

static void
unbind_catch (catch)
     struct catchtag *catch;
{
  register int last_time;

  do
    {
      last_time = catchlist == catch;
      unbind_to (catchlist->pdlcount);
      if (handlerlist != 0 && handlerlist->tag == catchlist)
	handlerlist = handlerlist->next;
      catchlist = catchlist->next;
    }
  while (! last_time);

  gcprolist = catch->gcpro;
  backtrace_list = catch->backlist;
  lisp_eval_depth = catch->lisp_eval_depth;
}

DEFUN ("throw", Fthrow, Sthrow, 2, 2, 0,
  "(throw TAG VALUE): throw to the catch for TAG and return VALUE from it.\n\
Both TAG and VALUE are evalled.")
  (tag, val)
     register Lisp_Object tag, val;
{
  register struct catchtag *c;

  while (1)
    {
      if (!NULL (tag))
	for (c = catchlist; c; c = c->next)
	  {
	    if (EQ (c->tag, tag))
	      {
		/* Restore the polling-suppression count.  */
		if (c->poll_suppress_count > poll_suppress_count)
		  abort ();
		while (c->poll_suppress_count < poll_suppress_count)
		  start_polling ();
		c->val = val;
		unbind_catch (c);
		_longjmp (c->jmp, 1);
	      }
	  }
      tag = Fsignal (Qno_catch, Fcons (tag, Fcons (val, Qnil)));
    }
}


DEFUN ("unwind-protect", Funwind_protect, Sunwind_protect, 1, UNEVALLED, 0,
  "Do BODYFORM, protecting with UNWINDFORMS.\n\
Usage looks like (unwind-protect BODYFORM UNWINDFORMS...) \n\
If BODYFORM completes normally, its value is returned\n\
after executing the UNWINDFORMS.\n\
If BODYFORM exits nonlocally, the UNWINDFORMS are executed anyway.")
  (args)
     Lisp_Object args;
{
  Lisp_Object val;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1;

  record_unwind_protect (0, Fcdr (args));
  val = Feval (Fcar (args));
  GCPRO1 (val);
  unbind_to (count);  
  UNGCPRO;
  return val;
}

/* Chain of condition handlers currently in effect.
   The elements of this chain are contained in the stack frames
   of Fcondition_case and internal_condition_case.
   When an error is signaled (by calling Fsignal, below),
   this chain is searched for an element that applies.  */

struct handler *handlerlist;

DEFUN ("condition-case", Fcondition_case, Scondition_case, 2, UNEVALLED, 0,
  "Regain control when an error is signaled.\n\
 (condition-case VAR BODYFORM HANDLERS...)\n\
executes BODYFORM and returns its value if no error happens.\n\
Each element of HANDLERS looks like (CONDITION-NAME BODY...)\n\
where the BODY is made of Lisp expressions.\n\
The handler is applicable to an error\n\
if CONDITION-NAME is one of the error's condition names.\n\
When a handler handles an error,\n\
control returns to the condition-case and the handler BODY... is executed\n\
with VAR bound to (SIGNALED-CONDITIONS . SIGNAL-DATA).\n\
The value of the last BODY form is returned from the condition-case.\n\
See SIGNAL for more info.")
  (args)
     Lisp_Object args;
{
  Lisp_Object val;
  struct catchtag c;
  struct handler h;
  register Lisp_Object tem;

  tem = Fcar (args);
  CHECK_SYMBOL (tem, 0);

  c.tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.lisp_eval_depth = lisp_eval_depth;
  c.poll_suppress_count = poll_suppress_count;
  c.pdlcount = specpdl_ptr - specpdl;
  c.gcpro = gcprolist;
  if (_setjmp (c.jmp))
    {
      if (!NULL (h.var))
        specbind (h.var, Fcdr (c.val));
      val = Fprogn (Fcdr (Fcar (c.val)));
      unbind_to (c.pdlcount);
      return val;
    }
  c.next = catchlist;
  catchlist = &c;
  h.var = Fcar (args);
  h.handler = Fcdr (Fcdr (args));
  
  for (val = h.handler; ! NULL (val); val = Fcdr (val))
    {
      tem = Fcar (val);
      if ((!NULL (tem)) &&
	  (!CONSP (tem) || (XTYPE (XCONS (tem)->car) != Lisp_Symbol)))
	error ("Invalid condition handler", tem);
    }
  
  h.next = handlerlist;
  h.poll_suppress_count = poll_suppress_count;
  h.tag = &c;
  handlerlist = &h;

  val = Feval (Fcar (Fcdr (args)));
  catchlist = c.next;
  handlerlist = h.next;
  return val;
}

Lisp_Object
internal_condition_case (bfun, handlers, hfun)
     Lisp_Object (*bfun) ();
     Lisp_Object handlers;
     Lisp_Object (*hfun) ();
{
  Lisp_Object val;
  struct catchtag c;
  struct handler h;

  c.tag = Qnil;
  c.val = Qnil;
  c.backlist = backtrace_list;
  c.lisp_eval_depth = lisp_eval_depth;
  c.poll_suppress_count = poll_suppress_count;
  c.pdlcount = specpdl_ptr - specpdl;
  c.gcpro = gcprolist;
  if (_setjmp (c.jmp))
    {
      return (*hfun) (Fcdr (c.val));
    }
  c.next = catchlist;
  catchlist = &c;
  h.handler = handlers;
  h.var = Qnil;
  h.poll_suppress_count = poll_suppress_count;
  h.next = handlerlist;
  h.tag = &c;
  handlerlist = &h;

  val = (*bfun) ();
  catchlist = c.next;
  handlerlist = h.next;
  return val;
}

static Lisp_Object find_handler_clause ();

DEFUN ("signal", Fsignal, Ssignal, 2, 2, 0,
  "Signal an error.  Args are SIGNAL-NAME, and associated DATA.\n\
A signal name is a symbol with an  error-conditions  property\n\
that is a list of condition names.\n\
A handler for any of those names will get to handle this signal.\n\
The symbol  error  should always be one of them.\n\
\n\
DATA should be a list.  Its elements are printed as part of the error message.\n\
If the signal is handled, DATA is made available to the handler.\n\
See  condition-case.")
  (sig, data)
     Lisp_Object sig, data;
{
  register struct handler *allhandlers = handlerlist;
  Lisp_Object conditions;
  extern int gc_in_progress;
  extern int waiting_for_input;
  Lisp_Object debugger_value;

  quit_error_check ();
  immediate_quit = 0;
  if (gc_in_progress || waiting_for_input)
    abort ();

  conditions = Fget (sig, Qerror_conditions);

  for (; handlerlist; handlerlist = handlerlist->next)
    {
      register Lisp_Object clause;
      clause = find_handler_clause (handlerlist->handler, conditions,
				    sig, data, &debugger_value);

      /* If have called debugger and user wants to continue,
	 just return nil.  */
      if (EQ (clause, Qlambda))
	return debugger_value;

      if (!NULL (clause))
	{
	  struct handler *h = handlerlist;
	  /* Restore the polling-suppression count.  */
	  if (h->poll_suppress_count > poll_suppress_count)
	    abort ();
	  while (h->poll_suppress_count < poll_suppress_count)
	    start_polling ();
	  handlerlist = allhandlers;
	  unbind_catch (h->tag);
	  h->tag->val = Fcons (clause, Fcons (sig, data));
	  _longjmp (h->tag->jmp, 1);
	}
    }

  handlerlist = allhandlers;
  /* If no handler is present now, try to run the debugger,
     and if that fails, throw to top level.  */
  find_handler_clause (Qerror, conditions, sig, data, &debugger_value);
  Fthrow (Qtop_level, Qt);
}

/* Value of Qlambda means we have called debugger and
   user has continued.  Store value returned fromdebugger
   into *debugger_value_ptr */

static Lisp_Object
find_handler_clause (handlers, conditions, sig, data, debugger_value_ptr)
     Lisp_Object handlers, conditions, sig, data;
     Lisp_Object *debugger_value_ptr;
{
  register Lisp_Object h;
  register Lisp_Object tem;
  register Lisp_Object tem1;

  if (EQ (handlers, Qt))  /* t is used by handlers for all conditions, set up by C code.  */
    return Qt;
  if (EQ (handlers, Qerror))  /* error is used similarly, but means display a backtrace too */
    {
      if (stack_trace_on_error)
	internal_with_output_to_temp_buffer ("*Backtrace*", Fbacktrace, Qnil);
      if (EQ (sig, Qquit) ? debug_on_quit : debug_on_error)
	{
	  *debugger_value_ptr =
	    call_debugger (Fcons (Qerror,
				  Fcons (Fcons (sig, data),
					 Qnil)));
	  return Qlambda;
	}
      return Qt;
    }
  for (h = handlers; CONSP (h); h = Fcdr (h))
    {
      tem1 = Fcar (h);
      if (!CONSP (tem1))
	continue;
      tem = Fmemq (Fcar (tem1), conditions);
      if (!NULL (tem))
        return tem1;
    }
  return Qnil;
}

/* dump an error message; called like printf */

/* VARARGS 1 */
void
error (m, a1, a2, a3)
     char *m;
{
  char buf[200];
  sprintf (buf, m, a1, a2, a3);
  while (1)
    Fsignal (Qerror, Fcons (build_string (buf), Qnil));
}

DEFUN ("commandp", Fcommandp, Scommandp, 1, 1, 0,
  "T if FUNCTION makes provisions for interactive calling.\n\
This means it contains a description for how to read arguments to give it.\n\
The value is nil for an invalid function or a symbol with no function definition.\n\
\n\
Interactively callable functions include strings (treated as keyboard macros),\n\
lambda-expressions that contain a top-level call to  interactive ,\n\
autoload definitions made by  autoload  with non-nil fourth argument,\n\
and some of the built-in functions of Lisp.\n\
\n\
Also, a symbol is commandp if its function definition is commandp.")
  (function)
     Lisp_Object function;
{
  register Lisp_Object fun;
  register Lisp_Object funcar;
  register Lisp_Object tem;
  register int i = 0;

  fun = function;
  while (XTYPE (fun) == Lisp_Symbol)
    {
      if (++i > 10) return Qnil;
      tem = Ffboundp (fun);
      if (NULL (tem)) return Qnil;
      fun = Fsymbol_function (fun);
    }
  if (XTYPE (fun) == Lisp_Subr)
    if (XSUBR (fun)->prompt)
      return Qt;
    else
      return Qnil;
  if (XTYPE (fun) == Lisp_Vector || XTYPE (fun) == Lisp_String)
    return Qt;
  if (!CONSP (fun))
    return Qnil;
  funcar = Fcar (fun);
  if (XTYPE (funcar) != Lisp_Symbol)
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  if (EQ (funcar, Qlambda))
    return Fassq (Qinteractive, Fcdr (Fcdr (fun)));
  if (EQ (funcar, Qmocklisp))
    return Qt;  /* All mocklisp functions can be called interactively */
  if (EQ (funcar, Qautoload))
    return Fcar (Fcdr (Fcdr (Fcdr (fun))));
  else
    return Qnil;
}

/* ARGSUSED */
DEFUN ("autoload", Fautoload, Sautoload, 2, 5, 0,
  "Define FUNCTION to autoload from FILE.\n\
FUNCTION is a symbol; FILE is a file name string to pass to  load.\n\
Third arg DOCSTRING is documentation for the function.\n\
Fourth arg FROM_KBD if non-nil says function can be called interactively.\n\
Fifth arg MACRO if non-nil says the function is really a macro.\n\
Third through fifth args give info about the real definition.\n\
They default to nil.\n\
If FUNCTION is already defined other than as an autoload,\n\
this does nothing and returns nil.")
  (function, file, docstring, interactive, macro)
     Lisp_Object function, file, docstring, interactive, macro;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[4];
#endif

  CHECK_SYMBOL (function, 0);
  CHECK_STRING (file, 1);

  /* If function is defined and not as an autoload, don't override */
  if (!EQ (XSYMBOL (function)->function, Qunbound)
      && !(XTYPE (XSYMBOL (function)->function) == Lisp_Cons
	   && EQ (XCONS (XSYMBOL (function)->function)->car, Qautoload)))
    return Qnil;

#ifdef NO_ARG_ARRAY
  args[0] = file;
  args[1] = docstring;
  args[2] = interactive;
  args[3] = macro;

  return Ffset (function, Fcons (Qautoload, Flist (4, &args[0])));
#else /* NO_ARG_ARRAY */
  return Ffset (function, Fcons (Qautoload, Flist (4, &file)));
#endif /* not NO_ARG_ARRAY */
}

Lisp_Object
un_autoload (oldqueue)
     Lisp_Object oldqueue;
{
  register Lisp_Object queue, first, second;

  /* Queue to unwind is current value of Vautoload_queue.
     oldqueue is the shadowed value to leave in Vautoload_queue.  */
  queue = Vautoload_queue;
  Vautoload_queue = oldqueue;
  while (CONSP (queue))
    {
      first = Fcar (queue);
      second = Fcdr (first);
      first = Fcar (first);
      if (EQ (second, Qnil))
	Vfeatures = first;
      else
	Ffset (first, second);
      queue = Fcdr (queue);
    }
  return Qnil;
}

do_autoload (fundef, funname)
     Lisp_Object fundef, funname;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object fun, val;

  fun = funname;

  /* Value saved here is to be restored into Vautoload_queue */
  record_unwind_protect (un_autoload, Vautoload_queue);
  Vautoload_queue = Qt;
  Fload (Fcar (Fcdr (fundef)), Qnil, noninteractive ? Qt : Qnil, Qnil);
  /* Once loading finishes, don't undo it.  */
  Vautoload_queue = Qt;
  unbind_to (count);

  while (XTYPE (fun) == Lisp_Symbol)
    {
      QUIT;
      val = XSYMBOL (fun)->function;
      if (EQ (val, Qunbound))
	Fsymbol_function (fun);	/* Get the right kind of error! */
      fun = val;
    }
  if (XTYPE (fun) == Lisp_Cons
      && EQ (XCONS (fun)->car, Qautoload))
    error ("Autoloading failed to define function %s",
	   XSYMBOL (funname)->name->data);
}

DEFUN ("eval", Feval, Seval, 1, 1, 0,
  "Evaluate FORM and return its value.")
  (form)
     Lisp_Object form;
{
  Lisp_Object fun, val, original_fun, original_args;
  Lisp_Object funcar;
  struct backtrace backtrace;
  struct gcpro gcpro1, gcpro2, gcpro3;

  if (XTYPE (form) == Lisp_Symbol)
    {
      if (EQ (Vmocklisp_arguments, Qt))
        return Fsymbol_value (form);
      val = Fsymbol_value (form);
      if (NULL (val))
	XFASTINT (val) = 0;
      else if (EQ (val, Qt))
	XFASTINT (val) = 1;
      return val;
    }
  if (!CONSP (form))
    return form;

  QUIT;
  if (consing_since_gc > gc_cons_threshold)
    {
      GCPRO1 (form);
      Fgarbage_collect ();
      UNGCPRO;
    }

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	error ("Lisp nesting exceeds max-lisp-eval-depth");
    }

  original_fun = Fcar (form);
  original_args = Fcdr (form);

  backtrace.next = backtrace_list;
  backtrace_list = &backtrace;
  backtrace.function = &original_fun; /* This also protects them from gc */
  backtrace.args = &original_args;
  backtrace.nargs = UNEVALLED;
  backtrace.evalargs = 1;
  backtrace.debug_on_exit = 0;

  if (debug_on_next_call)
    do_debug_on_call (Qt);

  /* At this point, only original_fun and original_args
     have values that will be used below */
 retry:
  fun = original_fun;
  while (XTYPE (fun) == Lisp_Symbol)
    {
      QUIT;
      val = XSYMBOL (fun)->function;
      if (EQ (val, Qunbound))
	Fsymbol_function (fun);	/* Get the right kind of error! */
      fun = val;
    }

  if (XTYPE (fun) == Lisp_Subr)
    {
      Lisp_Object numargs;
      Lisp_Object argvals[5];
      Lisp_Object args_left;
      register int i, maxargs;

      args_left = original_args;
      numargs = Flength (args_left);

      if (XINT (numargs) < XSUBR (fun)->min_args ||
	  (XSUBR (fun)->max_args >= 0 && XSUBR (fun)->max_args < XINT (numargs)))
	return Fsignal (Qwrong_number_of_arguments, Fcons (fun, Fcons (numargs, Qnil)));

      if (XSUBR (fun)->max_args == UNEVALLED)
	{
	  backtrace.evalargs = 0;
	  val = (*XSUBR (fun)->function) (args_left);
	  goto done;
	}

      if (XSUBR (fun)->max_args == MANY)
	{
	  /* Pass a vector of evaluated arguments */
	  Lisp_Object *vals;
	  register int argnum = 0;

	  vals = (Lisp_Object *) alloca (XINT (numargs) * sizeof (Lisp_Object));

	  GCPRO3 (args_left, fun, fun);
	  gcpro3.var = vals;
	  gcpro3.nvars = 0;

	  while (!NULL (args_left))
	    {
	      vals[argnum++] = Feval (Fcar (args_left));
	      args_left = Fcdr (args_left);
	      gcpro3.nvars = argnum;
	    }
	  UNGCPRO;

	  backtrace.args = vals;
	  backtrace.nargs = XINT (numargs);

	  val = (*XSUBR (fun)->function) (XINT (numargs), vals);
	  goto done;
	}

      GCPRO3 (args_left, fun, fun);
      gcpro3.var = argvals;
      gcpro3.nvars = 0;

      maxargs = XSUBR (fun)->max_args;
      for (i = 0; i < maxargs; args_left = Fcdr (args_left))
	{
	  argvals[i] = Feval (Fcar (args_left));
	  gcpro3.nvars = ++i;
	}

      UNGCPRO;

      backtrace.args = argvals;
      backtrace.nargs = XINT (numargs);

      switch (i)
	{
	case 0:
	  val = (*XSUBR (fun)->function) ();
	  goto done;
	case 1:
	  val = (*XSUBR (fun)->function) (argvals[0]);
	  goto done;
	case 2:
	  val = (*XSUBR (fun)->function) (argvals[0], argvals[1]);
	  goto done;
	case 3:
	  val = (*XSUBR (fun)->function) (argvals[0], argvals[1],
					  argvals[2]);
	  goto done;
	case 4:
	  val = (*XSUBR (fun)->function) (argvals[0], argvals[1],
					  argvals[2], argvals[3]);
	  goto done;
	case 5:
	  val = (*XSUBR (fun)->function) (argvals[0], argvals[1], argvals[2],
					  argvals[3], argvals[4]);
	  goto done;
	}
    }
  if (!CONSP (fun))
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  funcar = Fcar (fun);
  if (XTYPE (funcar) != Lisp_Symbol)
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  if (EQ (funcar, Qautoload))
    {
      do_autoload (fun, original_fun);
      goto retry;
    }
  if (EQ (funcar, Qmacro))
    val = Feval (apply1 (Fcdr (fun), original_args));
  else if (EQ (funcar, Qlambda))
    val = apply_lambda (fun, original_args, 1);
  else if (EQ (funcar, Qmocklisp))
    val = ml_apply (fun, original_args);
  else
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));

 done:
  if (!EQ (Vmocklisp_arguments, Qt))
    {
      if (NULL (val))
	XFASTINT (val) = 0;
      else if (EQ (val, Qt))
	XFASTINT (val) = 1;
    }
  lisp_eval_depth--;
  if (backtrace.debug_on_exit)
    val = call_debugger (Fcons (Qexit, Fcons (val, Qnil)));
  backtrace_list = backtrace.next;
  return val;
}

DEFUN ("apply", Fapply, Sapply, 2, MANY, 0,
  "Call FUNCTION, passing remaining arguments to it.  The last argument\n\
is a list of arguments to pass.\n\
Thus, (apply '+ 1 2 '(3 4)) returns 10.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int i, numargs;
  register Lisp_Object spread_arg;
  register Lisp_Object *funcall_args ;
  Lisp_Object fun;

  fun = args [0];
  funcall_args = 0;
  spread_arg = args [nargs - 1];
  CHECK_LIST (spread_arg, nargs);
  
  numargs = XINT (Flength (spread_arg));

  if (numargs == 0)
    return Ffuncall (nargs - 1, args);
  else if (numargs == 1)
    {
      args [nargs - 1] = XCONS (spread_arg)->car;
      return Ffuncall (nargs, args);
    }

  numargs = nargs - 2 + numargs;

  while (XTYPE (fun) == Lisp_Symbol)
    {
      QUIT;
      fun = XSYMBOL (fun)->function;
      if (EQ (fun, Qunbound))
	{
	  /* Let funcall get the error */
	  fun = args[0];
	  goto funcall;
	}
    }

  if (XTYPE (fun) == Lisp_Subr)
    if (numargs < XSUBR (fun)->min_args ||
	(XSUBR (fun)->max_args >= 0 && XSUBR (fun)->max_args < numargs))
      goto funcall;		/* Let funcall get the error */
    else if (XSUBR (fun)->max_args > numargs)
      {
        /* Avoid making funcall cons up a yet another new vector of arguments
	   by explicitly supplying nil's for optional values */
	funcall_args = (Lisp_Object *) alloca ((1 + XSUBR (fun)->max_args)
					       * sizeof (Lisp_Object));
	for (i = numargs; i < XSUBR (fun)->max_args;)
	  funcall_args[++i] = Qnil;
      }
 funcall:
  /* We add 1 to numargs because funcall_args includes the
     function itself as well as its arguments.  */
  if (!funcall_args)
    funcall_args = (Lisp_Object *) alloca ((1 + numargs)
					   * sizeof (Lisp_Object));
  bcopy (args, funcall_args, nargs * sizeof (Lisp_Object));
  /* Spread the last arg we got.  Its first element goes in
     the slot that it used to occupy, hence this value of I.  */
  i = nargs - 1;
  while (!NULL (spread_arg))
    {
      funcall_args [i++] = XCONS (spread_arg)->car;
      spread_arg = XCONS (spread_arg)->cdr;
    }
  return Ffuncall (numargs + 1, funcall_args);
}

/* Apply fn to arg */
Lisp_Object
apply1 (fn, arg)
     Lisp_Object fn, arg;
{
  if (NULL (arg))
    return Ffuncall (1, &fn);
#ifdef NO_ARG_ARRAY
  {
    Lisp_Object args[2];
    args[0] = fn;
    args[1] = arg;
    return Fapply (2, args);
  }
#else /* not NO_ARG_ARRAY */
  return Fapply (2, &fn);
#endif /* not NO_ARG_ARRAY */
}

/* Call function fn on no arguments */
Lisp_Object
call0 (fn)
     Lisp_Object fn;
{
  return Ffuncall (1, &fn);
}

/* Call function fn with argument arg */
/* ARGSUSED */
Lisp_Object
call1 (fn, arg)
     Lisp_Object fn, arg;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = fn;
  args[1] = arg;
  return Ffuncall (2, args);
#else /* not NO_ARG_ARRAY */
  return Ffuncall (2, &fn);
#endif /* not NO_ARG_ARRAY */
}

/* Call function fn with arguments arg, arg1 */
/* ARGSUSED */
Lisp_Object
call2 (fn, arg, arg1)
     Lisp_Object fn, arg, arg1;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[3];
  args[0] = fn;
  args[1] = arg;
  args[2] = arg1;
  return Ffuncall (3, args);
#else /* not NO_ARG_ARRAY */
  return Ffuncall (3, &fn);
#endif /* not NO_ARG_ARRAY */
}

/* Call function fn with arguments arg, arg1, arg2 */
/* ARGSUSED */
Lisp_Object
call3 (fn, arg, arg1, arg2)
     Lisp_Object fn, arg, arg1, arg2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[4];
  args[0] = fn;
  args[1] = arg;
  args[2] = arg1;
  args[3] = arg2;
  return Ffuncall (4, args);
#else /* not NO_ARG_ARRAY */
  return Ffuncall (4, &fn);
#endif /* not NO_ARG_ARRAY */
}

DEFUN ("funcall", Ffuncall, Sfuncall, 1, MANY, 0,
  "Call first argument as a function, passing remaining arguments to it.\n\
Thus,  (funcall 'cons 'x 'y)  returns  (x . y).")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object fun;
  Lisp_Object funcar;
  int numargs = nargs - 1;
  Lisp_Object lisp_numargs;
  Lisp_Object val;
  struct backtrace backtrace;
  struct gcpro gcpro1;
  register Lisp_Object *internal_args;
  register int i;

  QUIT;
  if (consing_since_gc > gc_cons_threshold)
    {
      GCPRO1 (*args);
      gcpro1.nvars = nargs;
      Fgarbage_collect ();
      UNGCPRO;
    }

  if (++lisp_eval_depth > max_lisp_eval_depth)
    {
      if (max_lisp_eval_depth < 100)
	max_lisp_eval_depth = 100;
      if (lisp_eval_depth > max_lisp_eval_depth)
	error ("Lisp nesting exceeds max-lisp-eval-depth");
    }

  backtrace.next = backtrace_list;
  backtrace_list = &backtrace;
  backtrace.function = &args[0];
  backtrace.args = &args[1];
  backtrace.nargs = nargs - 1;
  backtrace.evalargs = 0;
  backtrace.debug_on_exit = 0;

  if (debug_on_next_call)
    do_debug_on_call (Qlambda);

 retry:

  fun = args[0];
  while (XTYPE (fun) == Lisp_Symbol)
    {
      QUIT;
      val = XSYMBOL (fun)->function;
      if (EQ (val, Qunbound))
	Fsymbol_function (fun);	/* Get the right kind of error! */
      fun = val;
    }

  if (XTYPE (fun) == Lisp_Subr)
    {
      if (numargs < XSUBR (fun)->min_args ||
	  (XSUBR (fun)->max_args >= 0 && XSUBR (fun)->max_args < numargs))
	{
	  XFASTINT (lisp_numargs) = numargs;
	  return Fsignal (Qwrong_number_of_arguments, Fcons (fun, Fcons (lisp_numargs, Qnil)));
	}

      if (XSUBR (fun)->max_args == UNEVALLED)
	return Fsignal (Qinvalid_function, Fcons (fun, Qnil));

      if (XSUBR (fun)->max_args == MANY)
	{
	  val = (*XSUBR (fun)->function) (numargs, args + 1);
	  goto done;
	}

      if (XSUBR (fun)->max_args > numargs)
	{
	  internal_args = (Lisp_Object *) alloca (XSUBR (fun)->max_args * sizeof (Lisp_Object));
	  bcopy (args + 1, internal_args, numargs * sizeof (Lisp_Object));
	  for (i = numargs; i < XSUBR (fun)->max_args; i++)
	    internal_args[i] = Qnil;
	}
      else
	internal_args = args + 1;
      switch (XSUBR (fun)->max_args)
	{
	case 0:
	  val = (*XSUBR (fun)->function) ();
	  goto done;
	case 1:
	  val = (*XSUBR (fun)->function) (internal_args[0]);
	  goto done;
	case 2:
	  val = (*XSUBR (fun)->function) (internal_args[0],
					  internal_args[1]);
	  goto done;
	case 3:
	  val = (*XSUBR (fun)->function) (internal_args[0], internal_args[1],
					  internal_args[2]);
	  goto done;
	case 4:
	  val = (*XSUBR (fun)->function) (internal_args[0], internal_args[1],
					  internal_args[2],
					  internal_args[3]);
	  goto done;
	case 5:
	  val = (*XSUBR (fun)->function) (internal_args[0], internal_args[1],
					  internal_args[2], internal_args[3],
					  internal_args[4]);
	  goto done;
	}
    }
  if (!CONSP (fun))
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  funcar = Fcar (fun);
  if (XTYPE (funcar) != Lisp_Symbol)
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  if (EQ (funcar, Qlambda))
    val = funcall_lambda (fun, numargs, args + 1);
  else if (EQ (funcar, Qmocklisp))
    val = ml_apply (fun, Flist (numargs, args + 1));
  else if (EQ (funcar, Qautoload))
    {
      do_autoload (fun, args[0]);
      goto retry;
    }
  else
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));

 done:
  lisp_eval_depth--;
  if (backtrace.debug_on_exit)
    val = call_debugger (Fcons (Qexit, Fcons (val, Qnil)));
  backtrace_list = backtrace.next;
  return val;
}

Lisp_Object
apply_lambda (fun, args, eval_flag)
     Lisp_Object fun, args;
     int eval_flag;
{
  Lisp_Object args_left;
  Lisp_Object numargs;
  register Lisp_Object *arg_vector;
  struct gcpro gcpro1, gcpro2, gcpro3;
  register int i;
  register Lisp_Object tem;

  numargs = Flength (args);
  arg_vector = (Lisp_Object *) alloca (XINT (numargs) * sizeof (Lisp_Object));
  args_left = args;

  GCPRO3 (*arg_vector, args_left, fun);
  gcpro1.nvars = 0;

  for (i = 0; i < XINT (numargs);)
    {
      tem = Fcar (args_left), args_left = Fcdr (args_left);
      if (eval_flag) tem = Feval (tem);
      arg_vector[i++] = tem;
      gcpro1.nvars = i;
    }

  UNGCPRO;

  if (eval_flag)
    {
      backtrace_list->args = arg_vector;
      backtrace_list->nargs = i;
    }
  backtrace_list->evalargs = 0;
  tem = funcall_lambda (fun, XINT (numargs), arg_vector);

  /* Do the debug-on-exit now, while arg_vector still exists.  */
  if (backtrace_list->debug_on_exit)
    tem = call_debugger (Fcons (Qexit, Fcons (tem, Qnil)));
  /* Don't do it again when we return to eval.  */
  backtrace_list->debug_on_exit = 0;
  return tem;
}

Lisp_Object
funcall_lambda (fun, nargs, arg_vector)
     Lisp_Object fun;
     int nargs;
     register Lisp_Object *arg_vector;
{
  Lisp_Object val, tem;
  register Lisp_Object syms_left;
  Lisp_Object numargs;
  register Lisp_Object next;
  int count = specpdl_ptr - specpdl;
  register int i;
  int optional = 0, rest = 0;

  specbind (Qmocklisp_arguments, Qt);   /* t means NOT mocklisp! */

  XFASTINT (numargs) = nargs;

  i = 0;
  for (syms_left = Fcar (Fcdr (fun)); !NULL (syms_left); syms_left = Fcdr (syms_left))
    {
      QUIT;
      next = Fcar (syms_left);
      if (EQ (next, Qand_rest))
	rest = 1;
      else if (EQ (next, Qand_optional))
	optional = 1;
      else if (rest)
	{
	  specbind (Fcar (syms_left), Flist (nargs - i, &arg_vector[i]));
	  i = nargs;
	}
      else if (i < nargs)
	{
	  tem = arg_vector[i++];
	  specbind (next, tem);
	}
      else if (!optional)
	return Fsignal (Qwrong_number_of_arguments, Fcons (fun, Fcons (numargs, Qnil)));
      else
	specbind (next, Qnil);
    }

  if (i < nargs)
    return Fsignal (Qwrong_number_of_arguments, Fcons (fun, Fcons (numargs, Qnil)));

  val = Fprogn (Fcdr (Fcdr (fun)));
  unbind_to (count);
  return val;
}

void
grow_specpdl ()
{
  register int count = specpdl_ptr - specpdl;
  if (specpdl_size >= max_specpdl_size)
    {
      if (max_specpdl_size < 400)
	max_specpdl_size = 400;
      if (specpdl_size >= max_specpdl_size)
	{
	  Fsignal (Qerror,
		   Fcons (build_string ("Variable binding depth exceeds max-specpdl-size"), Qnil));
	  max_specpdl_size *= 2;
	}
    }
  specpdl_size *= 2;
  if (specpdl_size > max_specpdl_size)
    specpdl_size = max_specpdl_size;
  specpdl = (struct specbinding *) xrealloc (specpdl, specpdl_size * sizeof (struct specbinding));
  specpdl_ptr = specpdl + count;
}

void
specbind (symbol, value)
     Lisp_Object symbol, value;
{
  extern void store_symval_forwarding (); /* in eval.c */
  Lisp_Object ovalue;

  if (specpdl_ptr == specpdl + specpdl_size)
    grow_specpdl ();
  specpdl_ptr->symbol = symbol;
  specpdl_ptr->func = 0;
  ovalue = XSYMBOL (symbol)->value;
  specpdl_ptr->old_value = EQ (ovalue, Qunbound) ? Qunbound : Fsymbol_value (symbol);
  specpdl_ptr++;
  if (XTYPE (ovalue) == Lisp_Buffer_Objfwd)
    store_symval_forwarding (symbol, ovalue, value);
  else
    Fset (symbol, value);
}

void
record_unwind_protect (function, arg)
     Lisp_Object (*function)();
     Lisp_Object arg;
{
  if (specpdl_ptr == specpdl + specpdl_size)
    grow_specpdl ();
  specpdl_ptr->func = function;
  specpdl_ptr->symbol = Qnil;
  specpdl_ptr->old_value = arg;
  specpdl_ptr++;
}

void
unbind_to (count)
     int count;
{
  int quitf = !NULL (Vquit_flag);

  Vquit_flag = Qnil;

  while (specpdl_ptr != specpdl + count)
    {
      --specpdl_ptr;
      if (specpdl_ptr->func != 0)
	(*specpdl_ptr->func) (specpdl_ptr->old_value);
      /* Note that a "binding" of nil is really an unwind protect,
	so in that case the "old value" is a list of forms to evaluate.  */
      else if (NULL (specpdl_ptr->symbol))
	Fprogn (specpdl_ptr->old_value);
      else
        Fset (specpdl_ptr->symbol, specpdl_ptr->old_value);
    }
  if (NULL (Vquit_flag) && quitf) Vquit_flag = Qt;
}

#if 0

/* Get the value of symbol's global binding, even if that binding
 is not now dynamically visible.  */

Lisp_Object
top_level_value (symbol)
     Lisp_Object symbol;
{
  register struct specbinding *ptr = specpdl;

  CHECK_SYMBOL (symbol, 0);
  for (; ptr != specpdl_ptr; ptr++)
    {
      if (EQ (ptr->symbol, symbol))
	return ptr->old_value;
    }
  return Fsymbol_value (symbol);
}

Lisp_Object
top_level_set (symbol, newval)
     Lisp_Object symbol, newval;
{
  register struct specbinding *ptr = specpdl;

  CHECK_SYMBOL (symbol, 0);
  for (; ptr != specpdl_ptr; ptr++)
    {
      if (EQ (ptr->symbol, symbol))
	{
	  ptr->old_value = newval;
	  return newval;
	}
    }
  return Fset (symbol, newval);
}  

#endif /* 0 */

DEFUN ("backtrace-debug", Fbacktrace_debug, Sbacktrace_debug, 2, 2, 0,
  "Set the debug-on-exit flag of eval frame LEVEL levels down to FLAG.\n\
The debugger is entered when that frame exits, if the flag is non-nil.")
  (level, flag)
     Lisp_Object level, flag;
{
  register struct backtrace *backlist = backtrace_list;
  register int i;

  CHECK_NUMBER (level, 0);

  for (i = 0; backlist && i < XINT (level); i++)
    {
      backlist = backlist->next;
    }

  if (backlist)
    backlist->debug_on_exit = !NULL (flag);

  return flag;
}

DEFUN ("backtrace", Fbacktrace, Sbacktrace, 0, 0, "",
  "Print a trace of Lisp function calls currently active.\n\
Output stream used is value of standard-output.")
  ()
{
  register struct backtrace *backlist = backtrace_list;
  register int i;
  Lisp_Object tail;
  Lisp_Object tem;
  struct gcpro gcpro1;

  tail = Qnil;
  GCPRO1 (tail);

  while (backlist)
    {
      write_string (backlist->debug_on_exit ? "* " : "  ", 2);
      if (backlist->nargs == UNEVALLED)
        write_string ("(", -1);
      tem = *backlist->function;
      Fprin1 (tem, Qnil);	/* This can QUIT */
      if (backlist->nargs == UNEVALLED)
	{
	  if (backlist->evalargs)
	    write_string (" ...computing arguments...", -1);
	  else
	    write_string (" ...", -1);
	}
      else if (backlist->nargs == MANY)
	{
	  write_string ("(", -1);
	  for (tail = *backlist->args, i = 0; !NULL (tail); tail = Fcdr (tail), i++)
	    {
	      if (i) write_string (" ", -1);
	      Fprin1 (Fcar (tail), Qnil);
	    }
	}
      else
	{
	  write_string ("(", -1);
	  for (i = 0; i < backlist->nargs; i++)
	    {
	      if (i) write_string (" ", -1);
	      Fprin1 (backlist->args[i], Qnil);
	    }
	}
      write_string (")\n", -1);
      backlist = backlist->next;
    }

  UNGCPRO;
  return Qnil;
}

syms_of_eval ()
{
  DEFVAR_INT ("max-specpdl-size", &max_specpdl_size,
    "Limit on number of Lisp variable bindings & unwind-protects before error.");

  DEFVAR_INT ("max-lisp-eval-depth", &max_lisp_eval_depth,
    "Limit on depth in eval, apply and funcall before error.");

  DEFVAR_LISP ("quit-flag", &Vquit_flag,
    "Non-nil causes  eval  to abort, unless  inhibit-quit  is non-nil.\n\
Typing C-G sets  quit-flag  non-nil, regardless of  inhibit-quit.");
  Vquit_flag = Qnil;

  Qinhibit_quit = intern ("inhibit-quit");
  staticpro (&Qinhibit_quit);
  DEFVAR_LISP ("inhibit-quit", &Vinhibit_quit,
    "Non-nil inhibits C-g quitting from happening immediately.\n\
Note that  quit-flag  will still be set by typing C-g,\n\
so a quit will be signalled as soon as  inhibit-quit  is nil.\n\
To prevent this happening, set  quit-flag  to nil\n\
before making  inhibit-quit  nil.");
  Vinhibit_quit = Qnil;

  Qautoload = intern ("autoload");
  staticpro (&Qautoload);

  Qmacro = intern ("macro");
  staticpro (&Qmacro);

  Qexit = intern ("exit");
  staticpro (&Qexit);

  Qinteractive = intern ("interactive");
  staticpro (&Qinteractive);

  Qcommandp = intern ("commandp");
  staticpro (&Qcommandp);

  Qdefun = intern ("defun");
  staticpro (&Qdefun);

  Qand_rest = intern ("&rest");
  staticpro (&Qand_rest);

  Qand_optional = intern ("&optional");
  staticpro (&Qand_optional);

  DEFVAR_BOOL ("stack-trace-on-error", &stack_trace_on_error,
    "*Non-nil means automatically display a backtrace buffer\n\
after any error that is handled by the editor command loop.");
  stack_trace_on_error = 0;

  DEFVAR_BOOL ("debug-on-error", &debug_on_error,
    "*Non-nil means enter debugger if an error is signaled.\n\
Does not apply to errors handled by condition-case.\n\
See also variable debug-on-quit.");
  debug_on_error = 0;

  DEFVAR_BOOL ("debug-on-quit", &debug_on_quit,
    "*Non-nil means enter debugger if quit is signaled (C-G, for example).\n\
Does not apply if quit is handled by a condition-case.");
  debug_on_quit = 0;

  DEFVAR_BOOL ("debug-on-next-call", &debug_on_next_call,
    "Non-nil means enter debugger before next eval, apply or funcall.");

  DEFVAR_LISP ("debugger", &Vdebugger,
    "Function to call to invoke debugger.\n\
If due to frame exit, args are 'exit and value being returned;\n\
 this function's value will be returned instead of that.\n\
If due to error, args are 'error and list of signal's args.\n\
If due to apply or funcall entry, one arg, 'lambda.\n\
If due to eval entry, one arg, 't.");
  Vdebugger = Qnil;

  Qmocklisp_arguments = intern ("mocklisp-arguments");
  staticpro (&Qmocklisp_arguments);
  DEFVAR_LISP ("mocklisp-arguments", &Vmocklisp_arguments,
    "While in a mocklisp function, the list of its unevaluated args.");
  Vmocklisp_arguments = Qt;

  staticpro (&Vautoload_queue);
  Vautoload_queue = Qnil;

  defsubr (&Sor);
  defsubr (&Sand);
  defsubr (&Sif);
  defsubr (&Scond);
  defsubr (&Sprogn);
  defsubr (&Sprog1);
  defsubr (&Sprog2);
  defsubr (&Ssetq);
  defsubr (&Squote);
  defsubr (&Sfunction);
  defsubr (&Sdefun);
  defsubr (&Sdefmacro);
  defsubr (&Sdefvar);
  defsubr (&Sdefconst);
  defsubr (&Suser_variable_p);
  defsubr (&Slet);
  defsubr (&SletX);
  defsubr (&Swhile);
  defsubr (&Smacroexpand);
  defsubr (&Scatch);
  defsubr (&Sthrow);
  defsubr (&Sunwind_protect);
  defsubr (&Scondition_case);
  defsubr (&Ssignal);
  defsubr (&Sinteractive_p);
  defsubr (&Scommandp);
  defsubr (&Sautoload);
  defsubr (&Seval);
  defsubr (&Sapply);
  defsubr (&Sfuncall);
  defsubr (&Sbacktrace_debug);
  defsubr (&Sbacktrace);
}
