/* Random utility Lisp functions.
   Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.

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

#ifdef LOAD_AVE_TYPE
#ifndef HAVE_GETLOADAVG
#ifdef BSD
/* It appears param.h defines BSD and BSD4_3 in 4.3
   and is not considerate enough to avoid bombing out
   if they are already defined.  */
#undef BSD
#ifdef BSD4_3
#undef BSD4_3
#define XBSD4_3 /* XBSD4_3 says BSD4_3 is supposed to be defined.  */
#endif
#include <sys/param.h>
/* Now if BSD or BSD4_3 was defined and is no longer,
   define it again.  */
#ifndef BSD
#define BSD
#endif
#ifdef XBSD4_3
#ifndef BSD4_3
#define BSD4_3
#endif
#endif /* XBSD4_3 */
#endif /* BSD */
#ifndef VMS
#ifndef NLIST_STRUCT
#include <a.out.h> 
#else /* NLIST_STRUCT */
#include <nlist.h>
#endif /* NLIST_STRUCT */
#endif /* not VMS */
#endif /* not HAVE_GETLOADAVG */
#endif /* LOAD_AVE_TYPE */

/* Note on some machines this defines `vector' as a typedef,
   so make sure we don't use that name in this file.  */
#undef vector
#define vector *****

#ifdef NULL
#undef NULL
#endif
#include "lisp.h"
#include "commands.h"

#ifdef lint
#include "buffer.h"
#endif /* lint */

Lisp_Object Qstring_lessp;

DEFUN ("identity", Fidentity, Sidentity, 1, 1, 0,
  "Return the argument unchanged.")
  (arg)
     Lisp_Object arg;
{
  return arg;
}

DEFUN ("random", Frandom, Srandom, 0, 1, 0,
  "Return a pseudo-random number.\n\
On most systems all integers representable in Lisp are equally likely.\n\
  This is 24 bits' worth.\n\
On some systems, absolute value of result never exceeds 2 to the 14.\n\
If optional argument is supplied as  t,\n\
 the random number seed is set based on the current time and pid.")
  (arg)
     Lisp_Object arg;
{
  extern long random ();
  extern srandom ();
  extern long time ();

  if (EQ (arg, Qt))
    srandom (getpid () + time (0));
  return make_number ((int) random ());
}

/* Random data-structure functions */

DEFUN ("length", Flength, Slength, 1, 1, 0,
  "Return the length of vector, list or string SEQUENCE.")
  (obj)
     register Lisp_Object obj;
{
  register Lisp_Object tail, val;
  register int i;

 retry:
  if (XTYPE (obj) == Lisp_Vector || XTYPE (obj) == Lisp_String)
    return Farray_length (obj);
  else if (CONSP (obj))
    {
      for (i = 0, tail = obj; !NULL(tail); i++)
	{
	  QUIT;
	  tail = Fcdr (tail);
	}

      XFASTINT (val) = i;
      return val;
    }
  else if (NULL(obj))
    {
      XFASTINT (val) = 0;
      return val;
    }
  else
    {
      obj = wrong_type_argument (Qsequencep, obj);
      goto retry;
    }
}

DEFUN ("string-equal", Fstring_equal, Sstring_equal, 2, 2, 0,
  "T if two strings have identical contents.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  if (XTYPE (s1) == Lisp_Symbol)
    XSETSTRING (s1, XSYMBOL (s1)->name), XSETTYPE (s1, Lisp_String);
  if (XTYPE (s2) == Lisp_Symbol)
    XSETSTRING (s2, XSYMBOL (s2)->name), XSETTYPE (s2, Lisp_String);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  if (XSTRING (s1)->size != XSTRING (s2)->size ||
      bcmp (XSTRING (s1)->data, XSTRING (s2)->data, XSTRING (s1)->size))
    return Qnil;
  return Qt;
}

DEFUN ("string-lessp", Fstring_lessp, Sstring_lessp, 2, 2, 0,
  "T if first arg string is less than second in lexicographic order.\n\
Symbols are also allowed; their print names are used instead.")
  (s1, s2)
     register Lisp_Object s1, s2;
{
  register int i;
  register unsigned char *p1, *p2;
  register int end;

  if (XTYPE (s1) == Lisp_Symbol)
    XSETSTRING (s1, XSYMBOL (s1)->name), XSETTYPE (s1, Lisp_String);
  if (XTYPE (s2) == Lisp_Symbol)
    XSETSTRING (s2, XSYMBOL (s2)->name), XSETTYPE (s2, Lisp_String);
  CHECK_STRING (s1, 0);
  CHECK_STRING (s2, 1);

  p1 = XSTRING (s1)->data;
  p2 = XSTRING (s2)->data;
  end = XSTRING (s1)->size;
  if (end > XSTRING (s2)->size)
    end = XSTRING (s2)->size;

  for (i = 0; i < end; i++)
    {
      if (p1[i] != p2[i])
	return p1[i] < p2[i] ? Qt : Qnil;
    }
  return i < XSTRING (s2)->size ? Qt : Qnil;
}

static Lisp_Object concat ();

/* ARGSUSED */
Lisp_Object
concat2 (s1, s2)
     Lisp_Object s1, s2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return concat (2, args, Lisp_String, 0);
#else
  return concat (2, &s1, Lisp_String, 0);
#endif /* NO_ARG_ARRAY */
}

DEFUN ("append", Fappend, Sappend, 0, MANY, 0,
  "Concatenate arguments and make the result a list.\n\
The result is a list whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Cons, 1);
}

DEFUN ("concat", Fconcat, Sconcat, 0, MANY, 0,
  "Concatenate arguments and make the result a string.\n\
The result is a string whose elements are the elements of all the arguments.\n\
Each argument may be a string, a list of numbers, or a vector of numbers.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_String, 0);
}

DEFUN ("vconcat", Fvconcat, Svconcat, 0, MANY, 0,
  "Concatenate arguments and make the result a vector.\n\
The result is a vector whose elements are the elements of all the arguments.\n\
Each argument may be a list, vector or string.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  return concat (nargs, args, Lisp_Vector, 0);
}

DEFUN ("copy-sequence", Fcopy_sequence, Scopy_sequence, 1, 1, 0,
  "Return a copy of a list, vector or string.")
  (arg)
     Lisp_Object arg;
{
  if (NULL (arg)) return arg;
  if (!CONSP (arg) && XTYPE (arg) != Lisp_Vector && XTYPE (arg) != Lisp_String)
    arg = wrong_type_argument (Qsequencep, arg);
  return concat (1, &arg, CONSP (arg) ? Lisp_Cons : XTYPE (arg), 0);
}

static Lisp_Object
concat (nargs, args, target_type, last_special)
     int nargs;
     Lisp_Object *args;
     enum Lisp_Type target_type;
     int last_special;
{
  Lisp_Object val;
  Lisp_Object len;
  register Lisp_Object tail;
  register Lisp_Object this;
  int toindex;
  register int leni;
  register int argnum;
  Lisp_Object last_tail;
  Lisp_Object prev;

  /* In append, the last arg isn't treated like the others */
  if (last_special && nargs > 0)
    {
      nargs--;
      last_tail = args[nargs];
    }
  else
    last_tail = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      if (!(CONSP (this) || NULL (this)
          || XTYPE (this) == Lisp_Vector || XTYPE (this) == Lisp_String))
	{
	  if (XTYPE (this) == Lisp_Int)
            args[argnum] = Fint_to_string (this);
	  else
	    args[argnum] = wrong_type_argument (Qsequencep, this);
	}
    }

  for (argnum = 0, leni = 0; argnum < nargs; argnum++)
    {
      this = args[argnum];
      len = Flength (this);
      leni += XFASTINT (len);
    }

  XFASTINT (len) = leni;

  if (target_type == Lisp_Cons)
    val = Fmake_list (len, Qnil);
  else if (target_type == Lisp_Vector)
    val = Fmake_vector (len, Qnil);
  else
    val = Fmake_string (len, len);

  /* In append, if all but last arg are nil, return last arg */
  if (target_type == Lisp_Cons && EQ (val, Qnil))
    return last_tail;

  if (CONSP (val))
    tail = val, toindex = -1;		/* -1 in toindex is flag we are making a list */
  else
    toindex = 0;

  prev = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      Lisp_Object thislen;
      int thisleni;
      register int thisindex = 0;

      this = args[argnum];
      if (!CONSP (this))
	thislen = Flength (this), thisleni = XINT (thislen);

      while (1)
	{
	  register Lisp_Object elt;

	  /* Fetch next element of `this' arg into `elt', or break if `this' is exhausted. */
	  if (NULL (this)) break;
	  if (CONSP (this))
	    elt = Fcar (this), this = Fcdr (this);
	  else
	    {
	      if (thisindex >= thisleni) break;
	      if (XTYPE (this) == Lisp_String)
		XFASTINT (elt) = XSTRING (this)->data[thisindex++];
	      else
		elt = XVECTOR (this)->contents[thisindex++];
	    }

	  /* Store into result */
	  if (toindex < 0)
	    {
	      XCONS (tail)->car = elt;
	      prev = tail;
	      tail = XCONS (tail)->cdr;
	    }
	  else if (XTYPE (val) == Lisp_Vector)
	    XVECTOR (val)->contents[toindex++] = elt;
	  else
	    {
	      while (XTYPE (elt) != Lisp_Int)
		elt = wrong_type_argument (Qintegerp, elt);
	      {
#ifdef MASSC_REGISTER_BUG
		/* Even removing all "register"s doesn't disable this bug!
		   Nothing simpler than this seems to work. */
		unsigned char *p = & XSTRING (val)->data[toindex++];
		*p = XINT (elt);
#else
		XSTRING (val)->data[toindex++] = XINT (elt);
#endif
	      }
	    }
	}
    }
  if (!NULL (prev))
    XCONS (prev)->cdr = last_tail;

  return val;  
}

DEFUN ("copy-alist", Fcopy_alist, Scopy_alist, 1, 1, 0,
  "Return a copy of ALIST.\n\
This is a new alist which represents the same mapping\n\
from objects to objects, but does not share the alist structure with ALIST.\n\
The objects mapped (cars and cdrs of elements of the alist)\n\
are shared, however.")
  (alist)
     Lisp_Object alist;
{
  register Lisp_Object tem;

  CHECK_LIST (alist, 0);
  if (NULL (alist))
    return alist;
  alist = concat (1, &alist, Lisp_Cons, 0);
  for (tem = alist; CONSP (tem); tem = XCONS (tem)->cdr)
    {
      register Lisp_Object car;
      car = XCONS (tem)->car;

      if (CONSP (car))
	XCONS (tem)->car = Fcons (XCONS (car)->car, XCONS (car)->cdr);
    }
  return alist;
}

DEFUN ("substring", Fsubstring, Ssubstring, 2, 3, 0,
  "Return a substring of STRING, starting at index FROM and ending before TO.\n\
TO may be nil or omitted; then the substring runs to the end of STRING.\n\
If FROM or TO is negative, it counts from the end.")
  (string, from, to)
     Lisp_Object string;
     register Lisp_Object from, to;
{
  CHECK_STRING (string, 0);
  CHECK_NUMBER (from, 1);
  if (NULL (to))
    to = Flength (string);
  else
    CHECK_NUMBER (to, 2);

  if (XINT (from) < 0)
    XSETINT (from, XINT (from) + XSTRING (string)->size);
  if (XINT (to) < 0)
    XSETINT (to, XINT (to) + XSTRING (string)->size);
  if (!(0 <= XINT (from) && XINT (from) <= XINT (to)
        && XINT (to) <= XSTRING (string)->size))
    args_out_of_range_3 (string, from, to);

  return make_string (XSTRING (string)->data + XINT (from),
		      XINT (to) - XINT (from));
}

DEFUN ("nthcdr", Fnthcdr, Snthcdr, 2, 2, 0,
  "Takes cdr N times on LIST, returns the result.")
  (n, list)
     Lisp_Object n;
     register Lisp_Object list;
{
  register int i, num;
  CHECK_NUMBER (n, 0);
  num = XINT (n);
  for (i = 0; i < num && !NULL (list); i++)
    {
      QUIT;
      list = Fcdr (list);
    }
  return list;
}

DEFUN ("nth", Fnth, Snth, 2, 2, 0,
  "Returns the Nth element of LIST.\n\
N counts from zero.  If LIST is not that long, nil is returned.")
  (n, list)
     Lisp_Object n, list;
{
  return Fcar (Fnthcdr (n, list));
}

DEFUN ("elt", Felt, Selt, 2, 2, 0,
  "Returns element of SEQUENCE at index N.")
  (seq, n)
     register Lisp_Object seq, n;
{
  CHECK_NUMBER (n, 0);
  while (1)
    {
      if (XTYPE (seq) == Lisp_Cons || NULL (seq))
	return Fcar (Fnthcdr (n, seq));
      else if (XTYPE (seq) == Lisp_String ||
	       XTYPE (seq) == Lisp_Vector)
	return Faref (seq, n);
      else
	seq = wrong_type_argument (Qsequencep, seq);
    }
}

DEFUN ("memq", Fmemq, Smemq, 2, 2, 0,
  "Returns non-nil if ELT is an element of LIST.  Comparison done with EQ.\n\
The value is actually the tail of LIST whose car is ELT.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NULL (tail); tail = Fcdr (tail))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (elt, tem)) return tail;
      QUIT;
    }
  return Qnil;
}

DEFUN ("assq", Fassq, Sassq, 2, 2, 0,
  "Returns non-nil if ELT is the car of an element of LIST.  Comparison done with eq.\n\
The value is actually the element of LIST whose car is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NULL (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcar (elt);
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

/* Like Fassq but never report an error and do not allow quits.
   Use only on lists known never to be circular.  */

Lisp_Object
assq_no_quit (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; CONSP (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcar (elt);
      if (EQ (key, tem)) return elt;
    }
  return Qnil;
}

DEFUN ("assoc", Fassoc, Sassoc, 2, 2, 0,
  "Returns non-nil if ELT is the car of an element of LIST.  Comparison done with  equal.\n\
The value is actually the element of LIST whose car is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NULL (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fequal (Fcar (elt), key);
      if (!NULL (tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("rassq", Frassq, Srassq, 2, 2, 0,
  "Returns non-nil if ELT is the cdr of an element of LIST.  Comparison done with EQ.\n\
The value is actually the element of LIST whose cdr is ELT.")
  (key, list)
     register Lisp_Object key;
     Lisp_Object list;
{
  register Lisp_Object tail;
  for (tail = list; !NULL (tail); tail = Fcdr (tail))
    {
      register Lisp_Object elt, tem;
      elt = Fcar (tail);
      if (!CONSP (elt)) continue;
      tem = Fcdr (elt);
      if (EQ (key, tem)) return elt;
      QUIT;
    }
  return Qnil;
}

DEFUN ("delq", Fdelq, Sdelq, 2, 2, 0,
  "Deletes by side effect any occurrences of ELT as a member of LIST.\n\
The modified LIST is returned.\n\
If the first member of LIST is ELT, there is no way to remove it by side effect;\n\
therefore, write  (setq foo (delq element foo))  to be sure of changing  foo.")
  (elt, list)
     register Lisp_Object elt;
     Lisp_Object list;
{
  register Lisp_Object tail, prev;
  register Lisp_Object tem;

  tail = list;
  prev = Qnil;
  while (!NULL (tail))
    {
      tem = Fcar (tail);
      if (EQ (elt, tem))
	{
	  if (NULL (prev))
	    list = Fcdr (tail);
	  else
	    Fsetcdr (prev, Fcdr (tail));
	}
      else
	prev = tail;
      tail = Fcdr (tail);
      QUIT;
    }
  return list;
}

DEFUN ("nreverse", Fnreverse, Snreverse, 1, 1, 0,
  "Reverses LIST by modifying cdr pointers.  Returns the beginning of the reversed list.")
  (list)
     Lisp_Object list;
{
  register Lisp_Object prev, tail, next;

  if (NULL (list)) return list;
  prev = Qnil;
  tail = list;
  while (!NULL (tail))
    {
      QUIT;
      next = Fcdr (tail);
      Fsetcdr (tail, prev);
      prev = tail;
      tail = next;
    }
  return prev;
}

DEFUN ("reverse", Freverse, Sreverse, 1, 1, 0,
  "Reverses LIST, copying.  Returns the beginning of the reversed list.\n\
See also the function  nreverse, which is used more often.")
  (list)
     Lisp_Object list;
{
  Lisp_Object length;
  register Lisp_Object *vec;
  register Lisp_Object tail;
  register int i;

  length = Flength (list);
  vec = (Lisp_Object *) alloca (XINT (length) * sizeof (Lisp_Object));
  for (i = XINT (length) - 1, tail = list; i >= 0; i--, tail = Fcdr (tail))
    vec[i] = Fcar (tail);

  return Flist (XINT (length), vec);
}

Lisp_Object merge ();

DEFUN ("sort", Fsort, Ssort, 2, 2, 0,
  "Sort LIST, stably, comparing elements using PREDICATE.\n\
Returns the sorted list.  LIST is modified by side effects.\n\
PREDICATE is called with two elements of LIST, and should return T\n\
if the first element is \"less\" than the second.")
  (list, pred)
     Lisp_Object list, pred;
{
  Lisp_Object front, back;
  register Lisp_Object len, tem;
  struct gcpro gcpro1, gcpro2;
  register int length;

  front = list;
  len = Flength (list);
  length = XINT (len);
  if (length < 2)
    return list;

  XSETINT (len, (length / 2) - 1);
  tem = Fnthcdr (len, list);
  back = Fcdr (tem);
  Fsetcdr (tem, Qnil);

  GCPRO2 (front, back);
  front = Fsort (front, pred);
  back = Fsort (back, pred);
  UNGCPRO;
  return merge (front, back, pred);
}

Lisp_Object
merge (org_l1, org_l2, pred)
     Lisp_Object org_l1, org_l2;
     Lisp_Object pred;
{
  Lisp_Object value;
  register Lisp_Object tail;
  Lisp_Object tem;
  register Lisp_Object l1, l2;
  struct gcpro gcpro1, gcpro2, gcpro3, gcpro4;

  l1 = org_l1;
  l2 = org_l2;
  tail = Qnil;
  value = Qnil;

  /* It is sufficient to protect org_l1 and org_l2.
     When l1 and l2 are updated, we copy the new values
     back into the org_ vars.  */
  GCPRO4 (org_l1, org_l2, pred, value);

  while (1)
    {
      if (NULL (l1))
	{
	  UNGCPRO;
	  if (NULL (tail))
	    return l2;
	  Fsetcdr (tail, l2);
	  return value;
	}
      if (NULL (l2))
	{
	  UNGCPRO;
	  if (NULL (tail))
	    return l1;
	  Fsetcdr (tail, l1);
	  return value;
	}
      tem = call2 (pred, Fcar (l2), Fcar (l1));
      if (NULL (tem))
	{
	  tem = l1;
	  l1 = Fcdr (l1);
	  org_l1 = l1;
	}
      else
	{
	  tem = l2;
	  l2 = Fcdr (l2);
	  org_l2 = l2;
	}
      if (NULL (tail))
	value = tem;
      else
	Fsetcdr (tail, tem);
      tail = tem;
    }
}

DEFUN ("get", Fget, Sget, 2, 2, 0,
  "Return the value of SYMBOL's PROPNAME property.\n\
This is the last VALUE stored with  (put SYMBOL PROPNAME VALUE).")
  (sym, prop)
     Lisp_Object sym;
     register Lisp_Object prop;
{
  register Lisp_Object tail;
  for (tail = Fsymbol_plist (sym); !NULL (tail); tail = Fcdr (Fcdr (tail)))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (prop, tem))
	return Fcar (Fcdr (tail));
    }
  return Qnil;
}

DEFUN ("put", Fput, Sput, 3, 3, 0,
  "Store SYMBOL's PROPNAME property with value VALUE.\n\
It can be retrieved with  (get SYMBOL PROPNAME).")
  (sym, prop, val)
     Lisp_Object sym;
     register Lisp_Object prop;
     Lisp_Object val;
{
  register Lisp_Object tail, prev;
  Lisp_Object newcell;
  prev = Qnil;
  for (tail = Fsymbol_plist (sym); !NULL (tail); tail = Fcdr (Fcdr (tail)))
    {
      register Lisp_Object tem;
      tem = Fcar (tail);
      if (EQ (prop, tem))
	return Fsetcar (Fcdr (tail), val);
      prev = tail;
    }
  newcell = Fcons (prop, Fcons (val, Qnil));
  if (NULL (prev))
    Fsetplist (sym, newcell);
  else
    Fsetcdr (Fcdr (prev), newcell);
  return val;
}

DEFUN ("equal", Fequal, Sequal, 2, 2, 0,
  "T if two Lisp objects have similar structure and contents.\n\
They must have the same data type.\n\
Conses are compared by comparing the cars and the cdrs.\n\
Vectors and strings are compared element by element.\n\
Numbers are compared by value.  Symbols must match exactly.")
  (o1, o2)
     register Lisp_Object o1, o2;
{
do_cdr:
  QUIT;
  if (XTYPE (o1) != XTYPE (o2)) return Qnil;
  if (XINT (o1) == XINT (o2)) return Qt;
  if (XTYPE (o1) == Lisp_Cons)
    {
      Lisp_Object v1;
      v1 = Fequal (Fcar (o1), Fcar (o2));
      if (NULL (v1))
	return v1;
      o1 = Fcdr (o1), o2 = Fcdr (o2);
      goto do_cdr;
    }
  if (XTYPE (o1) == Lisp_Marker)
    {
      return (XMARKER (o1)->buffer == XMARKER (o2)->buffer
	      && XMARKER (o1)->bufpos == XMARKER (o2)->bufpos)
	? Qt : Qnil;
    }
  if (XTYPE (o1) == Lisp_Vector)
    {
      register int index;
      if (XVECTOR (o1)->size != XVECTOR (o2)->size)
	return Qnil;
      for (index = 0; index < XVECTOR (o1)->size; index++)
	{
	  Lisp_Object v, v1, v2;
	  v1 = XVECTOR (o1)->contents [index];
	  v2 = XVECTOR (o2)->contents [index];
	  v = Fequal (v1, v2);
	  if (NULL (v)) return v;
	}
      return Qt;
    }
  if (XTYPE (o1) == Lisp_String)
    {
      if (XSTRING (o1)->size != XSTRING (o2)->size)
	return Qnil;
      if (bcmp (XSTRING (o1)->data, XSTRING (o2)->data, XSTRING (o1)->size))
	return Qnil;
      return Qt;
    }
  return Qnil;
}

DEFUN ("fillarray", Ffillarray, Sfillarray, 2, 2, 0,
  "Store each element of ARRAY with ITEM.  ARRAY is a vector or string.")
  (array, item)
     Lisp_Object array, item;
{
  register int size, index, charval;
 retry:
  if (XTYPE (array) == Lisp_Vector)
    {
      register Lisp_Object *p = XVECTOR (array)->contents;
      size = XVECTOR (array)->size;
      for (index = 0; index < size; index++)
	p[index] = item;
    }
  else if (XTYPE (array) == Lisp_String)
    {
      register unsigned char *p = XSTRING (array)->data;
      CHECK_NUMBER (item, 1);
      charval = XINT (item);
      size = XSTRING (array)->size;
      for (index = 0; index < size; index++)
	p[index] = charval;
    }
  else
    {
      array = wrong_type_argument (Qarrayp, array);
      goto retry;
    }
  return array;
}

/* ARGSUSED */
Lisp_Object
nconc2 (s1, s2)
     Lisp_Object s1, s2;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
  args[0] = s1;
  args[1] = s2;
  return Fnconc (2, args);
#else
  return Fnconc (2, &s1);
#endif /* NO_ARG_ARRAY */
}

DEFUN ("nconc", Fnconc, Snconc, 0, MANY, 0,
  "Concatenate any number of lists by altering them.\n\
Only the last argument is not altered, and need not be a list.")
  (nargs, args)
     int nargs;
     Lisp_Object *args;
{
  register int argnum;
  register Lisp_Object tail, tem, val;

  val = Qnil;

  for (argnum = 0; argnum < nargs; argnum++)
    {
      tem = args[argnum];
      if (NULL (tem)) continue;

      if (NULL (val))
	val = tem;

      if (argnum + 1 == nargs) break;

      if (!CONSP (tem))
	tem = wrong_type_argument (Qlistp, tem);

      while (CONSP (tem))
	{
	  tail = tem;
	  tem = Fcdr (tail);
	  QUIT;
	}

      tem = args[argnum + 1];
      Fsetcdr (tail, tem);
      if (NULL (tem))
	args[argnum + 1] = tail;
    }

  return val;
}

/* This is the guts of all mapping functions.
 Apply fn to each element of seq, one by one,
 storing the results into elements of vals, a C vector of Lisp_Objects.
 leni is the length of vals, which should also be the length of seq. */

static void
mapcar1 (leni, vals, fn, seq)
     int leni;
     Lisp_Object *vals;
     Lisp_Object fn, seq;
{
  register Lisp_Object tail;
  Lisp_Object dummy;
  register int i;
  struct gcpro gcpro1, gcpro2, gcpro3;

  /* Don't let vals contain any garbage when GC happens.  */
  for (i = 0; i < leni; i++)
    vals[i] = Qnil;

  GCPRO3 (dummy, fn, seq);
  gcpro1.var = vals;
  gcpro1.nvars = leni;
  /* We need not explicitly protect `tail' because it is used only on lists, and
    1) lists are not relocated and 2) the list is marked via `seq' so will not be freed */

  if (XTYPE (seq) == Lisp_Vector)
    {
      for (i = 0; i < leni; i++)
	{
	  dummy = XVECTOR (seq)->contents[i];
	  vals[i] = call1 (fn, dummy);
	}
    }
  else if (XTYPE (seq) == Lisp_String)
    {
      for (i = 0; i < leni; i++)
	{
	  XFASTINT (dummy) = XSTRING (seq)->data[i];
	  vals[i] = call1 (fn, dummy);
	}
    }
  else   /* Must be a list, since Flength did not get an error */
    {
      tail = seq;
      for (i = 0; i < leni; i++)
	{
	  vals[i] = call1 (fn, Fcar (tail));
	  tail = Fcdr (tail);
	}
    }

  UNGCPRO;
}

DEFUN ("mapconcat", Fmapconcat, Smapconcat, 3, 3, 0,
  "Apply FN to each element of SEQ, and concat the results as strings.\n\
In between each pair of results, stick in SEP.\n\
Thus, \" \" as SEP results in spaces between the values return by FN.")
  (fn, seq, sep)
     Lisp_Object fn, seq, sep;
{
  Lisp_Object len;
  register int leni;
  int nargs;
  register Lisp_Object *args;
  register int i;
  int j;
  struct gcpro gcpro1;

  len = Flength (seq);
  leni = XINT (len);
  nargs = leni + leni - 1;
  if (nargs < 0) return build_string ("");

  args = (Lisp_Object *) alloca (nargs * sizeof (Lisp_Object));

  GCPRO1 (sep);
  mapcar1 (leni, args, fn, seq);
  UNGCPRO;

  /* Broken Xenix/386 compiler can't use a register variable here */
  for (j = leni - 1; j > 0; j--)
    args[j + j] = args[j];

  for (i = 1; i < nargs; i += 2)
    args[i] = sep;

  return Fconcat (nargs, args);
}

DEFUN ("mapcar", Fmapcar, Smapcar, 2, 2, 0,
  "Apply FUNCTION to each element of LIST, and make a list of the results.\n\
The result is a list just as long as LIST.")
  (fn, seq)
     Lisp_Object fn, seq;
{
  register Lisp_Object len;
  register int leni;
  register Lisp_Object *args;

  len = Flength (seq);
  leni = XFASTINT (len);
  args = (Lisp_Object *) alloca (leni * sizeof (Lisp_Object));

  mapcar1 (leni, args, fn, seq);

  return Flist (leni, args);
}

/* Anything that calls this function must protect from GC!  */

DEFUN ("y-or-n-p", Fy_or_n_p, Sy_or_n_p, 1, 1, 0,
  "Ask user a \"y or n\" question.  Return t if answer is \"y\".\n\
No confirmation of the answer is requested; a single character is enough.\n\
Also accepts Space to mean yes, or Delete to mean no.")
  (prompt)
     Lisp_Object prompt;
{
  register int ans;
  Lisp_Object xprompt;
  Lisp_Object args[2];
  int ocech = cursor_in_echo_area;
  struct gcpro gcpro1, gcpro2;

  CHECK_STRING (prompt, 0);
  xprompt = prompt;
  GCPRO2 (prompt, xprompt);

  while (1)
    {
      message ("%s(y or n) ", XSTRING (xprompt)->data);
      cursor_in_echo_area = 1;
      ans = read_command_char (0);
      cursor_in_echo_area = -1;
      message ("%s(y or n) %c", XSTRING (xprompt)->data, ans);
      cursor_in_echo_area = ocech;
      QUIT;
      if (ans >= 0)
	ans = DOWNCASE (ans);
      if (ans == 'y' || ans == ' ')
	{ ans = 'y'; break; }
      if (ans == 'n' || ans == 127)
	break;

      Fding (Qnil);
      Fdiscard_input ();
      if (EQ (xprompt, prompt))
	{
	  args[0] = build_string ("Please answer y or n.  ");
	  args[1] = prompt;
	  xprompt = Fconcat (2, args);
	}
    }
  UNGCPRO;
  return (ans == 'y' ? Qt : Qnil);
}

/* Anything that calls this function must protect from GC!  */

DEFUN ("yes-or-no-p", Fyes_or_no_p, Syes_or_no_p, 1, 1, 0,
  "Ask user a yes or no question.  Return t if answer is yes.\n\
The user must confirm the answer with a newline, and can rub it out if not confirmed.")
  (prompt)
     Lisp_Object prompt;
{
  register Lisp_Object ans;
  Lisp_Object args[2];
  struct gcpro gcpro1;

  CHECK_STRING (prompt, 0);

  args[0] = prompt;
  args[1] = build_string ("(yes or no) ");
  prompt = Fconcat (2, args);

  GCPRO1 (prompt);
  while (1)
    {
      ans = Fdowncase (read_minibuf (Vminibuffer_local_map,
				     Qnil, prompt, 0));
      if (XSTRING (ans)->size == 3 && !strcmp (XSTRING (ans)->data, "yes"))
	{
	  UNGCPRO;
	  return Qt;
	}
      if (XSTRING (ans)->size == 2 && !strcmp (XSTRING (ans)->data, "no"))
	{
	  UNGCPRO;
	  return Qnil;
	}

      Fding (Qnil);
      Fdiscard_input ();
      message ("Please answer yes or no.");
      Fsleep_for (make_number (2));
    }
}

#ifndef HAVE_GETLOADAVG
/* Avoid static vars inside a function since in HPUX they dump as pure.  */
static int ldav_initialized;
static int ldav_channel;
#ifdef LOAD_AVE_TYPE
#ifndef VMS
static struct nlist ldav_nl[2];
#endif /* not VMS */
#endif /* LOAD_AVE_TYPE */

#define channel ldav_channel
#define initialized ldav_initialized
#define nl ldav_nl
#endif /* not HAVE_GETLOADAVG */

DEFUN ("load-average", Fload_average, Sload_average, 0, 0, 0,
  "Return the current 1 minute, 5 minute and 15 minute load averages\n\
in a list (all floating point load average values are multiplied by 100\n\
and then turned into integers).")
  ()
{
#ifndef LOAD_AVE_TYPE
  error ("load-average not implemented for this operating system");

#else /* LOAD_AVE_TYPE defined */

  LOAD_AVE_TYPE load_ave[3];
#ifdef VMS
#ifndef eunice
#include <iodef.h>
#include <descrip.h>
#else
#include <vms/iodef.h>
  struct {int dsc$w_length; char *dsc$a_pointer;} descriptor;
#endif /* eunice */
#endif /* VMS */

  /* If this fails for any reason, we can return (0 0 0) */
  load_ave[0] = 0.0; load_ave[1] = 0.0; load_ave[2] = 0.0;

#ifdef VMS
  /*
   *	VMS specific code -- read from the Load Ave driver
   */

  /*
   *	Ensure that there is a channel open to the load ave device
   */
  if (initialized == 0)
    {
      /* Attempt to open the channel */
#ifdef eunice
      descriptor.size = 18;
      descriptor.ptr  = "$$VMS_LOAD_AVERAGE";
#else
      $DESCRIPTOR(descriptor, "LAV0:");
#endif
      if (sys$assign (&descriptor, &channel, 0, 0) & 1)
	initialized = 1;
    }
  /*
   *	Read the load average vector
   */
  if (initialized)
    {
      if (!(sys$qiow (0, channel, IO$_READVBLK, 0, 0, 0,
		     load_ave, 12, 0, 0, 0, 0)
	    & 1))
	{
	  sys$dassgn (channel);
	  initialized = 0;
	}
    }
#else  /* not VMS */
#ifdef HAVE_GETLOADAVG
  (void) getloadavg (load_ave, 3);
#else  /* not HAVE_GETLOADAVG */
  /*
   *	4.2BSD UNIX-specific code -- read _avenrun from /dev/kmem
   */

  /*
   *	Make sure we have the address of _avenrun
   */
  if (nl[0].n_value == 0)
    {
      /*
       *	Get the address of _avenrun
       */
#ifndef NLIST_STRUCT
      strcpy (nl[0].n_name, LDAV_SYMBOL);
      nl[1].n_zeroes = 0;
#else /* NLIST_STRUCT */
#ifdef convex
      nl[0].n_un.n_name = LDAV_SYMBOL;
      nl[1].n_un.n_name = 0;
#else /* not convex */
      nl[0].n_name = LDAV_SYMBOL;
      nl[1].n_name = 0;
#endif /* not convex */
#endif /* NLIST_STRUCT */

#ifdef IRIS_4D
	{
#include <sys/types.h>
#include <sys/sysmp.h>
	    nl[0].n_value = sysmp(MP_KERNADDR, MPKA_AVENRUN);
	    nl[0].n_value &= 0x7fffffff;
	}
#else
      nlist (KERNEL_FILE, nl);
#endif /* IRIS */

#ifdef FIXUP_KERNEL_SYMBOL_ADDR
      if ((nl[0].n_type & N_TYPE) != N_ABS)
	nl[0].n_value = (nlp->n_value >> 2) | 0xc0000000;
#endif /* FIXUP_KERNEL_SYMBOL_ADDR */
    }
  /*
   *	Make sure we have /dev/kmem open
   */
  if (initialized == 0)
    {
      /*
       *	Open /dev/kmem
       */
      channel = open ("/dev/kmem", 0);
      if (channel >= 0) initialized = 1;
    }
  /*
   *	If we can, get the load ave values
   */
  if ((nl[0].n_value != 0) && (initialized != 0))
    {
      /*
       *	Seek to the correct address
       */
      lseek (channel, (long) nl[0].n_value, 0);
      if (read (channel, load_ave, sizeof load_ave)
	  != sizeof(load_ave))
	{
	  close (channel);
	  initialized = 0;
	}
    }
#endif /* not HAVE_GETLOADAVG */
#endif /* not VMS */

  /*
   *	Return the list of load average values
   */
  return Fcons (make_number (LOAD_AVE_CVT (load_ave[0])),
		Fcons (make_number (LOAD_AVE_CVT (load_ave[1])),
		       Fcons (make_number (LOAD_AVE_CVT (load_ave[2])),
			      Qnil)));
#endif /* LOAD_AVE_TYPE */
}

#undef channel
#undef initialized
#undef nl

Lisp_Object Vfeatures;

DEFUN ("featurep", Ffeaturep, Sfeaturep, 1, 1, 0,
  "Returns t if FEATURE is present in this Emacs.\n\
Use this to conditionalize execution of lisp code based on the presence or\n\
absence of emacs or environment extensions.\n\
Use  provide  to declare that a feature is available.\n\
This function looks at the value of the variable  features.")
     (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  return (NULL (tem)) ? Qnil : Qt;
}

DEFUN ("provide", Fprovide, Sprovide, 1, 1, 0,
  "Announce that FEATURE is a feature of the current Emacs.")
     (feature)
     Lisp_Object feature;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  if (!NULL (Vautoload_queue))
    Vautoload_queue = Fcons (Fcons (Vfeatures, Qnil), Vautoload_queue);
  tem = Fmemq (feature, Vfeatures);
  if (NULL (tem))
    Vfeatures = Fcons (feature, Vfeatures);
  return feature;
}

DEFUN ("require", Frequire, Srequire, 1, 2, 0,
  "If FEATURE is not present in Emacs (ie (featurep FEATURE) is false),\n\
load FILENAME.  FILENAME is optional and defaults to FEATURE.")
     (feature, file_name)
     Lisp_Object feature, file_name;
{
  register Lisp_Object tem;
  CHECK_SYMBOL (feature, 0);
  tem = Fmemq (feature, Vfeatures);
  if (NULL (tem))
    {
      int count = specpdl_ptr - specpdl;

      /* Value saved here is to be restored into Vautoload_queue */
      record_unwind_protect (un_autoload, Vautoload_queue);
      Vautoload_queue = Qt;

      Fload (NULL (file_name) ? Fsymbol_name (feature) : file_name,
	     Qnil, Qt, Qnil);

      tem = Fmemq (feature, Vfeatures);
      if (NULL (tem))
	error ("Required feature %s was not provided",
	       XSYMBOL (feature)->name->data );

      /* Once loading finishes, don't undo it.  */
      Vautoload_queue = Qt;
      unbind_to (count);
    }
  return feature;
}

syms_of_fns ()
{
  Qstring_lessp = intern ("string-lessp");
  staticpro (&Qstring_lessp);

  DEFVAR_LISP ("features", &Vfeatures,
    "A list of symbols which are the features of the executing emacs.\n\
Used by  featurep  and  require, and altered by  provide.");
  Vfeatures = Qnil;

  defsubr (&Sidentity);
  defsubr (&Srandom);
  defsubr (&Slength);
  defsubr (&Sstring_equal);
  defsubr (&Sstring_lessp);
  defsubr (&Sappend);
  defsubr (&Sconcat);
  defsubr (&Svconcat);
  defsubr (&Scopy_sequence);
  defsubr (&Scopy_alist);
  defsubr (&Ssubstring);
  defsubr (&Snthcdr);
  defsubr (&Snth);
  defsubr (&Selt);
  defsubr (&Smemq);
  defsubr (&Sassq);
  defsubr (&Sassoc);
  defsubr (&Srassq);
  defsubr (&Sdelq);
  defsubr (&Snreverse);
  defsubr (&Sreverse);
  defsubr (&Ssort);
  defsubr (&Sget);
  defsubr (&Sput);
  defsubr (&Sequal);
  defsubr (&Sfillarray);
  defsubr (&Snconc);
  defsubr (&Smapcar);
  defsubr (&Smapconcat);
  defsubr (&Sy_or_n_p);
  defsubr (&Syes_or_no_p);
  defsubr (&Sload_average);
  defsubr (&Sfeaturep);
  defsubr (&Srequire);
  defsubr (&Sprovide);
}
