/* Primitive operations on Lisp data types for GNU Emacs Lisp interpreter.
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


#include <signal.h>

#include "config.h"
#include "lisp.h"

#ifndef standalone
#include "buffer.h"
#endif

Lisp_Object Qnil, Qt, Qquote, Qlambda, Qsubr, Qunbound;
Lisp_Object Qerror_conditions, Qerror_message, Qtop_level;
Lisp_Object Qerror, Qquit, Qwrong_type_argument, Qargs_out_of_range;
Lisp_Object Qvoid_variable, Qvoid_function;
Lisp_Object Qsetting_constant, Qinvalid_read_syntax;
Lisp_Object Qinvalid_function, Qwrong_number_of_arguments, Qno_catch;
Lisp_Object Qend_of_file, Qarith_error;
Lisp_Object Qbeginning_of_buffer, Qend_of_buffer, Qbuffer_read_only;
Lisp_Object Qintegerp, Qnatnump, Qsymbolp, Qlistp, Qconsp;
Lisp_Object Qstringp, Qarrayp, Qsequencep, Qbufferp;
Lisp_Object Qchar_or_string_p, Qmarkerp, Qinteger_or_marker_p, Qvectorp;
Lisp_Object Qboundp, Qfboundp;
Lisp_Object Qcdr;

Lisp_Object
wrong_type_argument (predicate, value)
     register Lisp_Object predicate, value;
{
  register Lisp_Object tem;
  do
    {
      if (!EQ (Vmocklisp_arguments, Qt))
	{
	 if (XTYPE (value) == Lisp_String &&
	     (EQ (predicate, Qintegerp) || EQ (predicate, Qinteger_or_marker_p)))
	   return Fstring_to_int (value, Qt);
	 if (XTYPE (value) == Lisp_Int && EQ (predicate, Qstringp))
	   return Fint_to_string (value);
	}
      value = Fsignal (Qwrong_type_argument, Fcons (predicate, Fcons (value, Qnil)));
      tem = call1 (predicate, value);
    }
  while (NULL (tem));
  return value;
}

pure_write_error ()
{
  error ("Attempt to modify read-only object");
}

args_out_of_range (a1, a2)
     Lisp_Object a1, a2;
{
  while (1)
    Fsignal (Qargs_out_of_range, Fcons (a1, Fcons (a2, Qnil)));
}

args_out_of_range_3 (a1, a2, a3)
     Lisp_Object a1, a2, a3;
{
  while (1)
    Fsignal (Qargs_out_of_range, Fcons (a1, Fcons (a2, Fcons (a3, Qnil))));
}

Lisp_Object
make_number (num)
     int num;
{
  register Lisp_Object val;
  XSET (val, Lisp_Int, num);
  return val;
}

/* Data type predicates */

DEFUN ("eq", Feq, Seq, 2, 2, 0,
  "T if the two args are the same Lisp object.")
  (obj1, obj2)
     Lisp_Object obj1, obj2;
{
  if (EQ (obj1, obj2))
    return Qt;
  return Qnil;
}

DEFUN ("null", Fnull, Snull, 1, 1, 0, "T if OBJECT is nil.")
  (obj)
     Lisp_Object obj;
{
  if (NULL (obj))
    return Qt;
  return Qnil;
}

DEFUN ("consp", Fconsp, Sconsp, 1, 1, 0, "T if OBJECT is a cons cell.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Cons)
    return Qt;
  return Qnil;
}

DEFUN ("atom", Fatom, Satom, 1, 1, 0, "T if OBJECT is not a cons cell.  This includes nil.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Cons)
    return Qnil;
  return Qt;
}

DEFUN ("listp", Flistp, Slistp, 1, 1, 0, "T if OBJECT is a list.  This includes nil.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Cons || NULL (obj))
    return Qt;
  return Qnil;
}

DEFUN ("nlistp", Fnlistp, Snlistp, 1, 1, 0, "T if OBJECT is not a list.  Lists include nil.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Cons || NULL (obj))
    return Qnil;
  return Qt;
}

DEFUN ("integerp", Fintegerp, Sintegerp, 1, 1, 0, "T if OBJECT is a number.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Int)
    return Qt;
  return Qnil;
}

DEFUN ("natnump", Fnatnump, Snatnump, 1, 1, 0, "T if OBJECT is a nonnegative number.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Int && XINT (obj) >= 0)
    return Qt;
  return Qnil;
}

DEFUN ("symbolp", Fsymbolp, Ssymbolp, 1, 1, 0, "T if OBJECT is a symbol.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Symbol)
    return Qt;
  return Qnil;
}

DEFUN ("vectorp", Fvectorp, Svectorp, 1, 1, 0, "T if OBJECT is a vector.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Vector)
    return Qt;
  return Qnil;
}

DEFUN ("stringp", Fstringp, Sstringp, 1, 1, 0, "T if OBJECT is a string.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_String)
    return Qt;
  return Qnil;
}

DEFUN ("arrayp", Farrayp, Sarrayp, 1, 1, 0, "T if OBJECT is an array (string or vector).")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Vector || XTYPE (obj) == Lisp_String)
    return Qt;
  return Qnil;
}

DEFUN ("sequencep", Fsequencep, Ssequencep, 1, 1, 0,
  "T if OBJECT is a sequence (list or array).")
  (obj)
     Lisp_Object obj;
{
  if (LISTP (obj) || XTYPE (obj) == Lisp_Vector || XTYPE (obj) == Lisp_String)
    return Qt;
  return Qnil;
}

DEFUN ("bufferp", Fbufferp, Sbufferp, 1, 1, 0, "T if OBJECT is an editor buffer.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Buffer)
    return Qt;
  return Qnil;
}

DEFUN ("markerp", Fmarkerp, Smarkerp, 1, 1, 0, "T if OBJECT is a marker (editor pointer).")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Marker)
    return Qt;
  return Qnil;
}

DEFUN ("integer-or-marker-p", Finteger_or_marker_p, Sinteger_or_marker_p, 1, 1, 0,
  "T if OBJECT is an integer or a marker (editor pointer).")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Marker || XTYPE (obj) == Lisp_Int)
    return Qt;
  return Qnil;
}

DEFUN ("subrp", Fsubrp, Ssubrp, 1, 1, 0, "T if OBJECT is a built-in function.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Subr)
    return Qt;
  return Qnil;
}

DEFUN ("char-or-string-p", Fchar_or_string_p, Schar_or_string_p, 1, 1, 0, "T if OBJECT is a character (a number) or a string.")
  (obj)
     Lisp_Object obj;
{
  if (XTYPE (obj) == Lisp_Int || XTYPE (obj) == Lisp_String)
    return Qt;
  return Qnil;
}

/* Extract and set components of lists */

DEFUN ("car", Fcar, Scar, 1, 1, 0,
  "Return the car of CONSCELL.  If arg is nil, return nil.")
  (list)
     Lisp_Object list;
{
  while (1)
    {
      if (XTYPE (list) == Lisp_Cons)
	return XCONS (list)->car;
      else if (EQ (list, Qnil))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("car-safe", Fcar_safe, Scar_safe, 1, 1, 0,
  "Return the car of OBJECT if it is a cons cell, or else  nil.")
  (object)
     Lisp_Object object;
{
  if (XTYPE (object) == Lisp_Cons)
    return XCONS (object)->car;
  else
    return Qnil;
}

DEFUN ("cdr", Fcdr, Scdr, 1, 1, 0,
  "Return the cdr of CONSCELL.  If arg is nil, return nil.")
  (list)
     Lisp_Object list;
{
  while (1)
    {
      if (XTYPE (list) == Lisp_Cons)
	return XCONS (list)->cdr;
      else if (EQ (list, Qnil))
	return Qnil;
      else
	list = wrong_type_argument (Qlistp, list);
    }
}

DEFUN ("cdr-safe", Fcdr_safe, Scdr_safe, 1, 1, 0,
  "Return the cdr of OBJECT if it is a cons cell, or else  nil.")
  (object)
     Lisp_Object object;
{
  if (XTYPE (object) == Lisp_Cons)
    return XCONS (object)->cdr;
  else
    return Qnil;
}

DEFUN ("setcar", Fsetcar, Ssetcar, 2, 2, 0,
  "Set the car of CONSCELL to be NEWCAR.  Returns NEWCAR.")
  (cell, newcar)
     Lisp_Object cell, newcar;
{
  if (XTYPE (cell) != Lisp_Cons)
    cell = wrong_type_argument (Qconsp, cell);

  CHECK_IMPURE (cell);
  XCONS (cell)->car = newcar;
  return newcar;
}

DEFUN ("setcdr", Fsetcdr, Ssetcdr, 2, 2, 0,
  "Set the cdr of CONSCELL to be NEWCDR.  Returns NEWCDR.")
  (cell, newcdr)
     Lisp_Object cell, newcdr;
{
  if (XTYPE (cell) != Lisp_Cons)
    cell = wrong_type_argument (Qconsp, cell);

  CHECK_IMPURE (cell);
  XCONS (cell)->cdr = newcdr;
  return newcdr;
}

/* Extract and set components of symbols */

DEFUN ("boundp", Fboundp, Sboundp, 1, 1, 0, "T if SYMBOL's value is not void.")
  (sym)
     Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  return (XTYPE (XSYMBOL (sym)->value) == Lisp_Void
	  || EQ (XSYMBOL (sym)->value, Qunbound))
	 ? Qnil : Qt;
}

DEFUN ("fboundp", Ffboundp, Sfboundp, 1, 1, 0, "T if SYMBOL's function definition is not void.")
  (sym)
     Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  return (XTYPE (XSYMBOL (sym)->function) == Lisp_Void
	  || EQ (XSYMBOL (sym)->function, Qunbound))
	 ? Qnil : Qt;
}

DEFUN ("makunbound", Fmakunbound, Smakunbound, 1, 1, 0, "Make SYMBOL's value be void.")
  (sym)
     Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  XSYMBOL (sym)->value = Qunbound;
  return sym;
}

DEFUN ("fmakunbound", Ffmakunbound, Sfmakunbound, 1, 1, 0, "Make SYMBOL's function definition be void.")
  (sym)
     Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  XSYMBOL (sym)->function = Qunbound;
  return sym;
}

DEFUN ("symbol-function", Fsymbol_function, Ssymbol_function, 1, 1, 0,
  "Return SYMBOL's function definition.")
  (sym)
     Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  if (EQ (XSYMBOL (sym)->function, Qunbound))
    return Fsignal (Qvoid_function, Fcons (sym, Qnil));
  return XSYMBOL (sym)->function;
}

DEFUN ("symbol-plist", Fsymbol_plist, Ssymbol_plist, 1, 1, 0, "Return SYMBOL's property list.")
  (sym)
     Lisp_Object sym;
{
  CHECK_SYMBOL (sym, 0);
  return XSYMBOL (sym)->plist;
}

DEFUN ("symbol-name", Fsymbol_name, Ssymbol_name, 1, 1, 0, "Return SYMBOL's name, a string.")
  (sym)
     Lisp_Object sym;
{
  Lisp_Object name;

  CHECK_SYMBOL (sym, 0);
  XSET (name, Lisp_String, XSYMBOL (sym)->name);
  return name;
}

DEFUN ("fset", Ffset, Sfset, 2, 2, 0,
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.")
  (sym, newdef)
     Lisp_Object sym, newdef;
{
  CHECK_SYMBOL (sym, 0);
  if (!NULL (Vautoload_queue) && !EQ (XSYMBOL (sym)->function, Qunbound))
    Vautoload_queue = Fcons (Fcons (sym, XSYMBOL (sym)->function),
			     Vautoload_queue);
  XSYMBOL (sym)->function = newdef;
  return newdef;
}

DEFUN ("setplist", Fsetplist, Ssetplist, 2, 2, 0,
  "Set SYMBOL's property list to NEWVAL, and return NEWVAL.")
  (sym, newplist)
     Lisp_Object sym, newplist;
{
  CHECK_SYMBOL (sym, 0);
  XSYMBOL (sym)->plist = newplist;
  return newplist;
}

/* Getting and setting values of symbols */

/* Given the raw contents of a symbol value cell,
 return the Lisp value of the symbol. */

Lisp_Object
do_symval_forwarding (valcontents)
     register Lisp_Object valcontents;
{
  Lisp_Object val;
#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (valcontents))
#else
  switch (XTYPE (valcontents))
#endif
    {
    case Lisp_Intfwd:
      XSET (val, Lisp_Int, *XINTPTR (valcontents));
      return val;

    case Lisp_Boolfwd:
      if (*XINTPTR (valcontents))
	return Qt;
      return Qnil;

    case Lisp_Objfwd:
      return *XOBJFWD (valcontents);

    case Lisp_Buffer_Objfwd:
      return *(Lisp_Object *)((int)XOBJFWD (valcontents) + (char *)bf_cur);
    }
  return valcontents;
}

store_symval_forwarding (sym, valcontents, newval)
     Lisp_Object sym;
     register Lisp_Object valcontents, newval;
{
#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (valcontents))
#else
  switch (XTYPE (valcontents))
#endif
    {
    case Lisp_Intfwd:
      CHECK_NUMBER (newval, 1);
      *XINTPTR (valcontents) = XINT (newval);
      break;

    case Lisp_Boolfwd:
      *XINTPTR (valcontents) = NULL(newval) ? 0 : 1;
      break;

    case Lisp_Objfwd:
      *XOBJFWD (valcontents) = newval;
      break;

    case Lisp_Buffer_Objfwd:
      *(Lisp_Object *)((int)XOBJFWD (valcontents) + (char *)bf_cur) = newval;
      break;

    default:
      valcontents = XSYMBOL (sym)->value;
      if (XTYPE (valcontents) == Lisp_Buffer_Local_Value ||
	  XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
	XCONS (XSYMBOL (sym)->value)->car = newval;
      else
	XSYMBOL (sym)->value = newval;
    }
}

DEFUN ("symbol-value", Fsymbol_value, Ssymbol_value, 1, 1, 0, "Return SYMBOL's value.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object valcontents, tem1;
  register Lisp_Object val;
  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;

 retry:
#ifdef SWITCH_ENUM_BUG
  switch ((int) XTYPE (valcontents))
#else
  switch (XTYPE (valcontents))
#endif
    {
    case Lisp_Buffer_Local_Value:
    case Lisp_Some_Buffer_Local_Value:
      /* valcontents is a list
        (REALVALUE BUFFER CURRENT-ALIST-ELEMENT . DEFAULT-VALUE)).

        CURRENT-ALIST-ELEMENT is a pointer to an element of BUFFER's
	local_var_alist, that being the element whose car is this variable.
        Or it can be a pointer to the (CURRENT-ALIST-ELEMENT . DEFAULT-VALUE), if BUFFER
	does not have an element in its alist for this variable.

	If the current buffer is not BUFFER, we store the current REALVALUE value into
	CURRENT-ALIST-ELEMENT, then find the appropriate alist element for
	the buffer now current and set up CURRENT-ALIST-ELEMENT.
	Then we set REALVALUE out of that element, and store into BUFFER.
 	Note that REALVALUE can be a forwarding pointer. */

      if (bf_cur != XBUFFER (XCONS (XCONS (valcontents)->cdr)->car))
	{
	  tem1 = XCONS (XCONS (XCONS (valcontents)->cdr)->cdr)->car;
          Fsetcdr (tem1, do_symval_forwarding (XCONS (valcontents)->car));
	  tem1 = Fassq (sym, bf_cur->local_var_alist);
	  if (NULL (tem1))
	    tem1 = XCONS (XCONS (valcontents)->cdr)->cdr;
	  XCONS (XCONS (XCONS (valcontents)->cdr)->cdr)->car = tem1;
	  XSET (XCONS (XCONS (valcontents)->cdr)->car, Lisp_Buffer, bf_cur);
	  store_symval_forwarding (sym, XCONS (valcontents)->car, Fcdr (tem1));
	}
      valcontents = XCONS (valcontents)->car;
      goto retry;

    case Lisp_Intfwd:
      XSET (val, Lisp_Int, *XINTPTR (valcontents));
      return val;

    case Lisp_Boolfwd:
      if (*XINTPTR (valcontents))
	return Qt;
      return Qnil;

    case Lisp_Objfwd:
      return *XOBJFWD (valcontents);

    case Lisp_Buffer_Objfwd:
      return *(Lisp_Object *)((int)XOBJFWD (valcontents) + (char *)bf_cur);

    case Lisp_Symbol:
      /* For a symbol, check whether it is 'unbound. */
      if (!EQ (valcontents, Qunbound))
	break;
      /* drops through! */
    case Lisp_Void:
      return Fsignal (Qvoid_variable, Fcons (sym, Qnil));
    }
  return valcontents;
}

DEFUN ("default-value", Fdefault_value, Sdefault_value, 1, 1, 0,
  "Return SYMBOL's default value.\n\
This is the value that is seen in buffers that do not have their own values\n\
for this variable.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object valcontents;

  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;
  if (XTYPE (valcontents) == Lisp_Buffer_Local_Value ||
      XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
    return XCONS (XCONS (XCONS (valcontents)->cdr)->cdr)->cdr;
  return Fsymbol_value (sym);
}

DEFUN ("set", Fset, Sset, 2, 2, 0,
  "Set SYMBOL's value to NEWVAL, and return NEWVAL.")
  (sym, newval)
     Lisp_Object sym, newval;
{
  register Lisp_Object valcontents, tem1, current_alist_element;

  CHECK_SYMBOL (sym, 0);
  if (NULL (sym) || EQ (sym, Qt))
    return Fsignal (Qsetting_constant, Fcons (sym, Qnil));
  valcontents = XSYMBOL (sym)->value;
  if (XTYPE (valcontents) == Lisp_Buffer_Local_Value ||
      XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
    {
      /* valcontents is a list
        (REALVALUE BUFFER CURRENT-ALIST-ELEMENT . DEFAULT-VALUE)).

        CURRENT-ALIST-ELEMENT is a pointer to an element of BUFFER's
	local_var_alist, that being the element whose car is this variable.
        Or it can be a pointer to the (CURRENT-ALIST-ELEMENT . DEFAULT-VALUE), if BUFFER
	does not have an element in its alist for this variable.

	If the current buffer is not BUFFER, we store the current REALVALUE value into
	CURRENT-ALIST-ELEMENT, then find the appropriate alist element for
	the buffer now current and set up CURRENT-ALIST-ELEMENT.
	Then we set REALVALUE out of that element, and store into BUFFER.
	Note that REALVALUE can be a forwarding pointer. */

      current_alist_element = XCONS (XCONS (XCONS (valcontents)->cdr)->cdr)->car;
      if (bf_cur != ((XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
		     ? XBUFFER (XCONS (XCONS (valcontents)->cdr)->car)
		     : XBUFFER (XCONS (current_alist_element)->car)))
	{
          Fsetcdr (current_alist_element, do_symval_forwarding (XCONS (valcontents)->car));

	  tem1 = Fassq (sym, bf_cur->local_var_alist);
	  if (NULL (tem1))
	    /* This buffer sees the default value still.
	       If type is Lisp_Some_Buffer_Local_Value, set the default value.
	       If type is Lisp_Buffer_Local_Value, give this buffer a local value
		and set that.  */
	    if (XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
	      tem1 = XCONS (XCONS (valcontents)->cdr)->cdr;
	    else
	      {
		tem1 = Fcons (sym, Fcdr (current_alist_element));
		bf_cur->local_var_alist = Fcons (tem1, bf_cur->local_var_alist);
	      }
	  XCONS (XCONS (XCONS (valcontents)->cdr)->cdr)->car = tem1;
	  XSET (XCONS (XCONS (valcontents)->cdr)->car, Lisp_Buffer, bf_cur);
	}
      valcontents = XCONS (valcontents)->car;
    }
  store_symval_forwarding (sym, valcontents, newval);
  return newval;
}

DEFUN ("set-default", Fset_default, Sset_default, 2, 2, 0,
  "Set SYMBOL's default value.\n\
This is the value that is seen in buffers that do not have their own values\n\
for this variable.")
  (sym, value)
     Lisp_Object sym, value;
{
  register Lisp_Object valcontents, current_alist_element, alist_element_buffer;

  CHECK_SYMBOL (sym, 0);
  valcontents = XSYMBOL (sym)->value;
  if (XTYPE (valcontents) != Lisp_Buffer_Local_Value &&
      XTYPE (valcontents) != Lisp_Some_Buffer_Local_Value)
    return Fset (sym, value);

  /* Store new value into the DEFAULT-VALUE slot */
  XCONS (XCONS (XCONS (valcontents)->cdr)->cdr)->cdr = value;

  /* If that slot is current, we must set the REALVALUE slot too */
  current_alist_element = XCONS (XCONS (XCONS (valcontents)->cdr)->cdr)->car;
  alist_element_buffer = Fcar (current_alist_element);
  if (EQ (alist_element_buffer, current_alist_element))
    store_symval_forwarding (sym, XCONS (valcontents)->car, value);

  return value;
}

DEFUN ("make-variable-buffer-local", Fmake_variable_buffer_local, Smake_variable_buffer_local,
  1, 1, "vMake Variable Buffer Local: ",
  "Make VARIABLE have a separate value for each buffer.\n\
The value you see with symbol-value at any time is the value for the current buffer.\n\
There is also a default value which is seen in any buffer which has not yet\n\
set its own value.\n\
The function default-value gets the default value and set-default sets it.\n\
Using set or setq to set the variable causes it to have a separate value\n\
for the current buffer if it was previously using the default value.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;
  if ((XTYPE (valcontents) == Lisp_Buffer_Local_Value) ||
      (XTYPE (valcontents) == Lisp_Buffer_Objfwd))
    return sym;
  if (XTYPE (valcontents) == Lisp_Some_Buffer_Local_Value)
    {
      XSETTYPE (valcontents, Lisp_Buffer_Local_Value);
      return sym;
    }
  if (EQ (valcontents, Qunbound))
    XSYMBOL (sym)->value = Qnil;
  tem = Fcons (Qnil, Fsymbol_value (sym));
  XCONS (tem)->car = tem;
  XSYMBOL (sym)->value = Fcons (XSYMBOL (sym)->value, Fcons (Fcurrent_buffer (), tem));
  XSETTYPE (XSYMBOL (sym)->value, Lisp_Buffer_Local_Value);
  return sym;
}

DEFUN ("make-local-variable", Fmake_local_variable, Smake_local_variable,
  1, 1, "vMake Local Variable: ",
  "Make VARIABLE have a separate value in the current buffer.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;
  if ((XTYPE (valcontents) == Lisp_Buffer_Local_Value) ||
      (XTYPE (valcontents) == Lisp_Buffer_Objfwd))
    return sym;
  /* Make sure sym is set up to hold per-buffer values */
  if (XTYPE (valcontents) != Lisp_Some_Buffer_Local_Value)
    {
      if (EQ (valcontents, Qunbound))
	XSYMBOL (sym)->value = Qnil;
      tem = Fcons (Qnil, Fsymbol_value (sym));
      XCONS (tem)->car = tem;
      XSYMBOL (sym)->value = Fcons (XSYMBOL (sym)->value, Fcons (Qnil, tem));
      XSETTYPE (XSYMBOL (sym)->value, Lisp_Some_Buffer_Local_Value);
    }
  /* Make sure this buffer has its own value of sym */
  tem = Fassq (sym, bf_cur->local_var_alist);
  if (NULL (tem))
    {
      bf_cur->local_var_alist
        = Fcons (Fcons (sym, XCONS (XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->cdr)->cdr),
		 bf_cur->local_var_alist);
      /* Make sure symbol does not think it is set up for this buffer;
	 force it to look once again for this buffer's value */
      if (bf_cur == XBUFFER (XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->car))
	XCONS (XCONS (XSYMBOL (sym)->value)->cdr)->car = Qnil;
    }
  return sym;
}

DEFUN ("kill-local-variable", Fkill_local_variable, Skill_local_variable,
  1, 1, "vKill Local Variable: ",
  "Make VARIABLE no longer have a separate value in the current buffer.\n\
From now on the default value will apply in this buffer.")
  (sym)
     Lisp_Object sym;
{
  register Lisp_Object tem, valcontents;

  CHECK_SYMBOL (sym, 0);

  valcontents = XSYMBOL (sym)->value;
  if (XTYPE (valcontents) != Lisp_Buffer_Local_Value &&
      XTYPE (valcontents) != Lisp_Some_Buffer_Local_Value)
    return sym;

  /* Get rid of this buffer's alist element, if any */

  tem = Fassq (sym, bf_cur->local_var_alist);
  if (!NULL (tem))
    bf_cur->local_var_alist = Fdelq (tem, bf_cur->local_var_alist);

  /* Put the symbol into a consistent state,
     set up for access in the current buffer with the default value */

  tem = XCONS (XCONS (valcontents)->cdr)->cdr;
  XCONS (tem)->car = tem;
  XCONS (XCONS (valcontents)->cdr)->car = Fcurrent_buffer ();
  store_symval_forwarding (sym, XCONS (valcontents)->car, XCONS (tem)->cdr);

  return sym;
}

/* Extract and set vector and string elements */

DEFUN ("aref", Faref, Saref, 2, 2, 0,
  "Return the element of ARRAY at index INDEX.\n\
ARRAY may be a vector or a string.  INDEX starts at 0.")
  (vector, idx)
     register Lisp_Object vector;
     Lisp_Object idx;
{
  register int idxval;

  CHECK_NUMBER (idx, 1);
  idxval = XINT (idx);
  if (XTYPE (vector) != Lisp_Vector && XTYPE (vector) != Lisp_String)
    vector = wrong_type_argument (Qarrayp, vector);
  if (idxval < 0 || idxval >= XVECTOR (vector)->size)
    while (1)
      Fsignal (Qargs_out_of_range, Fcons (vector, Fcons (idx, Qnil)));
  if (XTYPE (vector) == Lisp_Vector)
    return XVECTOR (vector)->contents[idxval];
  else
    {
      Lisp_Object val;
      XFASTINT (val) = (unsigned char) XSTRING (vector)->data[idxval];
      return val;
    }
}

DEFUN ("aset", Faset, Saset, 3, 3, 0,
  "Store into the element of ARRAY at index INDEX the value NEWVAL.\n\
ARRAY may be a vector or a string.  INDEX starts at 0.")
  (vector, idx, newelt)
     Lisp_Object vector, idx, newelt;
{
  register int idxval;

  CHECK_NUMBER (idx, 1);
  idxval = XINT (idx);
  if (XTYPE (vector) != Lisp_Vector && XTYPE (vector) != Lisp_String)
    vector = wrong_type_argument (Qarrayp, vector);
  if (idxval < 0 || idxval >= XVECTOR (vector)->size)
    while (1)
      Fsignal (Qargs_out_of_range, Fcons (vector, Fcons (idx, Qnil)));
  CHECK_IMPURE (vector);

  if (XTYPE (vector) == Lisp_Vector)
    XVECTOR (vector)->contents[idxval] = newelt;
  else
    XSTRING (vector)->data[idxval] = XINT (newelt);

  return newelt;
}

Lisp_Object
Farray_length (vector)
     Lisp_Object vector;
{
  register Lisp_Object size;
  if (XTYPE (vector) != Lisp_Vector && XTYPE (vector) != Lisp_String)
    vector = wrong_type_argument (Qarrayp, vector);
  XFASTINT (size) = XVECTOR (vector)->size;
  return size;
}

/* Arithmetic functions */

DEFUN ("=", Feqlsign, Seqlsign, 2, 2, 0,
  "T if two args, both numbers, are equal.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 0);

  if (XINT (num1) == XINT (num2))
    return Qt;
  return Qnil;
}

DEFUN ("<", Flss, Slss, 2, 2, 0,
  "T if first arg is less than second arg.  Both must be numbers.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 0);

  if (XINT (num1) < XINT (num2))
    return Qt;
  return Qnil;
}

DEFUN (">", Fgtr, Sgtr, 2, 2, 0,
  "T if first arg is greater than second arg.  Both must be numbers.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 0);

  if (XINT (num1) > XINT (num2))
    return Qt;
  return Qnil;
}

DEFUN ("<=", Fleq, Sleq, 2, 2, 0,
  "T if first arg is less than or equal to second arg.  Both must be numbers.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 0);

  if (XINT (num1) <= XINT (num2))
    return Qt;
  return Qnil;
}

DEFUN (">=", Fgeq, Sgeq, 2, 2, 0,
  "T if first arg is greater than or equal to second arg.  Both must be numbers.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 0);

  if (XINT (num1) >= XINT (num2))
    return Qt;
  return Qnil;
}

DEFUN ("/=", Fneq, Sneq, 2, 2, 0,
  "T if first arg is not equal to second arg.  Both must be numbers.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  CHECK_NUMBER_COERCE_MARKER (num1, 0);
  CHECK_NUMBER_COERCE_MARKER (num2, 0);

  if (XINT (num1) != XINT (num2))
    return Qt;
  return Qnil;
}

DEFUN ("zerop", Fzerop, Szerop, 1, 1, 0, "T if NUMBER is zero.")
  (num)
     Lisp_Object num;
{
  CHECK_NUMBER (num, 0);

  if (!XINT (num))
    return Qt;
  return Qnil;
}

DEFUN ("int-to-string", Fint_to_string, Sint_to_string, 1, 1, 0,
  "Convert INT to a string by printing it in decimal, with minus sign if negative.")
  (num)
     Lisp_Object num;
{
  char buffer[20];

  CHECK_NUMBER (num, 0);
  sprintf (buffer, "%d", XINT (num));
  return build_string (buffer);
}

DEFUN ("string-to-int", Fstring_to_int, Sstring_to_int, 1, 1, 0,
  "Convert STRING to an integer by parsing it as a decimal number.\n\
Optional second arg FLAG non-nil means also convert \"yes\" to 1, \"no\" to 0.")
  (str, flag)
     Lisp_Object str, flag;
{
  CHECK_STRING (str, 0);
  if (!NULL (flag) && !strcmp (XSTRING (str)->data, "yes"))
    return make_number (1);
  if (!NULL (flag) && !strcmp (XSTRING (str)->data, "no"))
    return make_number (0);
  return make_number (atoi (XSTRING (str)->data));
}
  
enum arithop
  { Aadd, Asub, Amult, Adiv, Alogand, Alogior, Alogxor, Amax, Amin };

Lisp_Object
arith_driver
  (code, nargs, args)
     enum arithop code;
     int nargs;
     Lisp_Object *args;
{
  Lisp_Object val;
  int argnum;
  int accum;
  int next;

#ifdef SWITCH_ENUM_BUG
  switch ((int) code)
#else
  switch (code)
#endif
    {
    case Alogior:
    case Alogxor:
    case Aadd:
    case Asub:
      accum = 0; break;
    case Amult:
      accum = 1; break;
    case Alogand:
      accum = -1; break;
    }

  for (argnum = 0; argnum < nargs; argnum++)
    {
      val = args[argnum];    /* using args[argnum] as argument to CHECK_NUMBER_... */
      CHECK_NUMBER_COERCE_MARKER (val, argnum);
      args[argnum] = val;    /* runs into a compiler bug. */
      next = XINT (args[argnum]);
#ifdef SWITCH_ENUM_BUG
      switch ((int) code)
#else
      switch (code)
#endif
	{
	case Aadd: accum += next; break;
	case Asub:
	  if (!argnum && nargs != 1)
	    next = - next;
	  accum -= next;
	  break;
	case Amult: accum *= next; break;
	case Adiv:
	  if (!argnum) accum = next;
	  else accum /= next;
	  break;
	case Alogand: accum &= next; break;
	case Alogior: accum |= next; break;
	case Alogxor: accum ^= next; break;
	case Amax: if (!argnum || next > accum) accum = next; break;
	case Amin: if (!argnum || next < accum) accum = next; break;
	}
    }

  XSET (val, Lisp_Int, accum);
  return val;
}

DEFUN ("+", Fplus, Splus, 0, MANY, 0,
  "Return sum of any number of numbers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Aadd, nargs, args);
}

DEFUN ("-", Fminus, Sminus, 0, MANY, 0,
  "Negate number or subtract numbers.\n\
With one arg, negates it.  With more than one arg,\n\
subtracts all but the first from the first.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Asub, nargs, args);
}

DEFUN ("*", Ftimes, Stimes, 0, MANY, 0,
  "Returns product of any number of numbers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amult, nargs, args);
}

DEFUN ("/", Fquo, Squo, 2, MANY, 0,
  "Returns first argument divided by rest of arguments.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Adiv, nargs, args);
}

DEFUN ("%", Frem, Srem, 2, 2, 0,
  "Returns remainder of first arg divided by second.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  Lisp_Object val;

  CHECK_NUMBER (num1, 0);
  CHECK_NUMBER (num2, 1);

  XSET (val, Lisp_Int, XINT (num1) % XINT (num2));
  return val;
}

DEFUN ("max", Fmax, Smax, 1, MANY, 0,
  "Return largest of all the arguments (which must be numbers.)")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amax, nargs, args);
}

DEFUN ("min", Fmin, Smin, 1, MANY, 0,
  "Return smallest of all the arguments (which must be numbers.)")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Amin, nargs, args);
}

DEFUN ("logand", Flogand, Slogand, 0, MANY, 0,
  "Return bitwise and of all the arguments (numbers).")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogand, nargs, args);
}

DEFUN ("logior", Flogior, Slogior, 0, MANY, 0,
  "Return bitwise or of all the arguments (numbers).")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogior, nargs, args);
}

DEFUN ("logxor", Flogxor, Slogxor, 0, MANY, 0,
  "Return bitwise exclusive-or of all the arguments (numbers).")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return arith_driver (Alogxor, nargs, args);
}

DEFUN ("ash", Fash, Sash, 2, 2, 0,
  "Return VALUE with its bits shifted left by COUNT.\n\
If COUNT is negative, shifting is actually to the right.\n\
In this case, the sign bit is duplicated.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  Lisp_Object val;

  CHECK_NUMBER (num1, 0);
  CHECK_NUMBER (num2, 1);

  if (XINT (num2) > 0)
    XSET (val, Lisp_Int, XINT (num1) << XFASTINT (num2));
  else
    XSET (val, Lisp_Int, XINT (num1) >> -XINT (num2));
  return val;
}

DEFUN ("lsh", Flsh, Slsh, 2, 2, 0,
  "Return VALUE with its bits shifted left by COUNT.\n\
If COUNT is negative, shifting is actually to the right.\n\
In this case,  zeros are shifted in on the left.")
  (num1, num2)
     Lisp_Object num1, num2;
{
  Lisp_Object val;

  CHECK_NUMBER (num1, 0);
  CHECK_NUMBER (num2, 1);

  if (XINT (num2) > 0)
    XSET (val, Lisp_Int, (unsigned) XFASTINT (num1) << XFASTINT (num2));
  else
    XSET (val, Lisp_Int, (unsigned) XFASTINT (num1) >> -XINT (num2));
  return val;
}

DEFUN ("1+", Fadd1, Sadd1, 1, 1, 0,
  "Return NUMBER plus one.")
  (num)
     Lisp_Object num;
{
  CHECK_NUMBER_COERCE_MARKER (num, 0);
  XSETINT (num, XFASTINT (num) + 1);
  return num;
}

DEFUN ("1-", Fsub1, Ssub1, 1, 1, 0,
  "Return NUMBER minus one.")
  (num)
     Lisp_Object num;
{
  CHECK_NUMBER_COERCE_MARKER (num, 0);
  XSETINT (num, XFASTINT (num) - 1);
  return num;
}
DEFUN ("lognot", Flognot, Slognot, 1, 1, 0,
  "Return the bitwise complement of ARG.")
  (num)
     Lisp_Object num;
{
  CHECK_NUMBER (num, 0);
  XSETINT (num, ~XFASTINT (num));
  return num;
}

void
syms_of_data ()
{
  Qquote = intern ("quote");
  Qlambda = intern ("lambda");
  Qsubr = intern ("subr");
  Qerror_conditions = intern ("error-conditions");
  Qerror_message = intern ("error-message");
  Qtop_level = intern ("top-level");

  Qerror = intern ("error");
  Qquit = intern ("quit");
  Qwrong_type_argument = intern ("wrong-type-argument");
  Qargs_out_of_range = intern ("args-out-of-range");
  Qvoid_function = intern ("void-function");
  Qvoid_variable = intern ("void-variable");
  Qsetting_constant = intern ("setting-constant");
  Qinvalid_read_syntax = intern ("invalid-read-syntax");

  Qinvalid_function = intern ("invalid-function");
  Qwrong_number_of_arguments = intern ("wrong-number-of-arguments");
  Qno_catch = intern ("no-catch");
  Qend_of_file = intern ("end-of-file");
  Qarith_error = intern ("arith-error");
  Qbeginning_of_buffer = intern ("beginning-of-buffer");
  Qend_of_buffer = intern ("end-of-buffer");
  Qbuffer_read_only = intern ("buffer-read-only");

  Qlistp = intern ("listp");
  Qconsp = intern ("consp");
  Qsymbolp = intern ("symbolp");
  Qintegerp = intern ("integerp");
  Qnatnump = intern ("natnump");
  Qstringp = intern ("stringp");
  Qarrayp = intern ("arrayp");
  Qsequencep = intern ("sequencep");
  Qbufferp = intern ("bufferp");
  Qvectorp = intern ("vectorp");
  Qchar_or_string_p = intern ("char-or-string-p");
  Qmarkerp = intern ("markerp");
  Qinteger_or_marker_p = intern ("integer-or-marker-p");
  Qboundp = intern ("boundp");
  Qfboundp = intern ("fboundp");

  Qcdr = intern ("cdr");

  /* ERROR is used as a signaler for random errors for which nothing else is right */

  Fput (Qerror, Qerror_conditions,
	Fcons (Qerror, Qnil));
  Fput (Qerror, Qerror_message,
	build_string ("error"));

  Fput (Qquit, Qerror_conditions,
	Fcons (Qquit, Qnil));
  Fput (Qquit, Qerror_message,
	build_string ("Quit"));

  Fput (Qwrong_type_argument, Qerror_conditions,
	Fcons (Qwrong_type_argument, Fcons (Qerror, Qnil)));
  Fput (Qwrong_type_argument, Qerror_message,
	build_string ("Wrong type argument"));

  Fput (Qargs_out_of_range, Qerror_conditions,
	Fcons (Qargs_out_of_range, Fcons (Qerror, Qnil)));
  Fput (Qargs_out_of_range, Qerror_message,
	build_string ("Args out of range"));

  Fput (Qvoid_function, Qerror_conditions,
	Fcons (Qvoid_function, Fcons (Qerror, Qnil)));
  Fput (Qvoid_function, Qerror_message,
	build_string ("Symbol's function definition is void"));

  Fput (Qvoid_variable, Qerror_conditions,
	Fcons (Qvoid_variable, Fcons (Qerror, Qnil)));
  Fput (Qvoid_variable, Qerror_message,
	build_string ("Symbol's value as variable is void"));

  Fput (Qsetting_constant, Qerror_conditions,
	Fcons (Qsetting_constant, Fcons (Qerror, Qnil)));
  Fput (Qsetting_constant, Qerror_message,
	build_string ("Attempt to set a constant symbol"));

  Fput (Qinvalid_read_syntax, Qerror_conditions,
	Fcons (Qinvalid_read_syntax, Fcons (Qerror, Qnil)));
  Fput (Qinvalid_read_syntax, Qerror_message,
	build_string ("Invalid read syntax"));

  Fput (Qinvalid_function, Qerror_conditions,
	Fcons (Qinvalid_function, Fcons (Qerror, Qnil)));
  Fput (Qinvalid_function, Qerror_message,
	build_string ("Invalid function"));

  Fput (Qwrong_number_of_arguments, Qerror_conditions,
	Fcons (Qwrong_number_of_arguments, Fcons (Qerror, Qnil)));
  Fput (Qwrong_number_of_arguments, Qerror_message,
	build_string ("Wrong number of arguments"));

  Fput (Qno_catch, Qerror_conditions,
	Fcons (Qno_catch, Fcons (Qerror, Qnil)));
  Fput (Qno_catch, Qerror_message,
	build_string ("No catch for tag"));

  Fput (Qend_of_file, Qerror_conditions,
	Fcons (Qend_of_file, Fcons (Qerror, Qnil)));
  Fput (Qend_of_file, Qerror_message,
	build_string ("End of file during parsing"));

  Fput (Qarith_error, Qerror_conditions,
	Fcons (Qarith_error, Fcons (Qerror, Qnil)));
  Fput (Qarith_error, Qerror_message,
	build_string ("Arithmetic error"));

  Fput (Qbeginning_of_buffer, Qerror_conditions,
	Fcons (Qbeginning_of_buffer, Fcons (Qerror, Qnil)));
  Fput (Qbeginning_of_buffer, Qerror_message,
	build_string ("Beginning of buffer"));

  Fput (Qend_of_buffer, Qerror_conditions,
	Fcons (Qend_of_buffer, Fcons (Qerror, Qnil)));
  Fput (Qend_of_buffer, Qerror_message,
	build_string ("End of buffer"));

  Fput (Qbuffer_read_only, Qerror_conditions,
	Fcons (Qbuffer_read_only, Fcons (Qerror, Qnil)));
  Fput (Qbuffer_read_only, Qerror_message,
	build_string ("Buffer is read-only"));

  staticpro (&Qnil);
  staticpro (&Qt);
  staticpro (&Qquote);
  staticpro (&Qlambda);
  staticpro (&Qsubr);
  staticpro (&Qunbound);
  staticpro (&Qerror_conditions);
  staticpro (&Qerror_message);
  staticpro (&Qtop_level);

  staticpro (&Qerror);
  staticpro (&Qquit);
  staticpro (&Qwrong_type_argument);
  staticpro (&Qargs_out_of_range);
  staticpro (&Qvoid_function);
  staticpro (&Qvoid_variable);
  staticpro (&Qsetting_constant);
  staticpro (&Qinvalid_read_syntax);
  staticpro (&Qwrong_number_of_arguments);
  staticpro (&Qinvalid_function);
  staticpro (&Qno_catch);
  staticpro (&Qend_of_file);
  staticpro (&Qarith_error);
  staticpro (&Qbeginning_of_buffer);
  staticpro (&Qend_of_buffer);
  staticpro (&Qbuffer_read_only);

  staticpro (&Qlistp);
  staticpro (&Qconsp);
  staticpro (&Qsymbolp);
  staticpro (&Qintegerp);
  staticpro (&Qnatnump);
  staticpro (&Qstringp);
  staticpro (&Qarrayp);
  staticpro (&Qsequencep);
  staticpro (&Qbufferp);
  staticpro (&Qvectorp);
  staticpro (&Qchar_or_string_p);
  staticpro (&Qmarkerp);
  staticpro (&Qinteger_or_marker_p);
  staticpro (&Qboundp);
  staticpro (&Qfboundp);
  staticpro (&Qcdr);

  defsubr (&Seq);
  defalias (&Seq, "eql");
  defsubr (&Snull);
  defalias (&Snull, "not");
  defsubr (&Slistp);
  defsubr (&Snlistp);
  defsubr (&Sconsp);
  defsubr (&Satom);
  defsubr (&Sintegerp);
  defalias (&Sintegerp, "numberp");
  defsubr (&Snatnump);
  defsubr (&Ssymbolp);
  defsubr (&Sstringp);
  defsubr (&Svectorp);
  defsubr (&Sarrayp);
  defsubr (&Ssequencep);
  defsubr (&Sbufferp);
  defsubr (&Smarkerp);
  defsubr (&Sinteger_or_marker_p);
  defsubr (&Ssubrp);
  defsubr (&Schar_or_string_p);
  defsubr (&Scar);
  defsubr (&Scdr);
  defsubr (&Scar_safe);
  defsubr (&Scdr_safe);
  defsubr (&Ssetcar);
  defalias (&Ssetcar, "rplaca");
  defalias (&Ssetcdr, "rplacd");
  defsubr (&Ssetcdr);
  defsubr (&Ssymbol_function);
  defsubr (&Ssymbol_plist);
  defsubr (&Ssymbol_name);
  defsubr (&Smakunbound);
  defsubr (&Sfmakunbound);
  defsubr (&Sboundp);
  defsubr (&Sfboundp);
  defsubr (&Sfset);
  defsubr (&Ssetplist);
  defsubr (&Ssymbol_value);
  defsubr (&Sset);
  defsubr (&Sdefault_value);
  defsubr (&Sset_default);
  defsubr (&Smake_variable_buffer_local);
  defsubr (&Smake_local_variable);
  defsubr (&Skill_local_variable);
  defsubr (&Saref);
  defsubr (&Saset);
  defsubr (&Sint_to_string);
  defsubr (&Sstring_to_int);
  defsubr (&Seqlsign);
  defsubr (&Slss);
  defsubr (&Sgtr);
  defsubr (&Sleq);
  defsubr (&Sgeq);
  defsubr (&Sneq);
  defsubr (&Szerop);
  defsubr (&Splus);
  defsubr (&Sminus);
  defsubr (&Stimes);
  defsubr (&Squo);
  defsubr (&Srem);
  defsubr (&Smax);
  defsubr (&Smin);
  defsubr (&Slogand);
  defsubr (&Slogior);
  defsubr (&Slogxor);
  defsubr (&Slsh);
  defsubr (&Sash);
  defsubr (&Sadd1);
  defsubr (&Ssub1);
  defsubr (&Slognot);
}

arith_error (signo)
     int signo;
{
#ifdef USG
  /* USG systems forget handlers when they are used;
     must reestablish each time */
  signal (signo, arith_error);
#endif /* USG */
#ifdef BSD4_1
  sigrelse (SIGFPE);
#else /* not BSD4_1 */
  sigsetmask (0);
#endif /* not BSD4_1 */

  Fsignal (Qarith_error, Qnil);
}

init_data ()
{
  signal (SIGFPE, arith_error);
}
