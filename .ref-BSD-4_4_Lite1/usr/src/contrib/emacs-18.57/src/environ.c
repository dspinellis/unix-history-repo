/* Environment-hacking for GNU Emacs subprocess
   Copyright (C) 1986 Free Software Foundation, Inc.

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

#ifdef MAINTAIN_ENVIRONMENT

#ifdef VMS
you lose -- this is un*x-only
#endif

/* alist of (name-string . value-string) */
Lisp_Object Venvironment_alist;
extern char **environ;

void
set_environment_alist (str, val)
     register Lisp_Object str, val;
{
  register Lisp_Object tem;

  tem = Fassoc (str, Venvironment_alist);
  if (NULL (tem))
    if (NULL (val))
      ;
    else
      Venvironment_alist = Fcons (Fcons (str, val), Venvironment_alist);
  else
    if (NULL (val))
      Venvironment_alist = Fdelq (tem, Venvironment_alist);
    else
      XCONS (tem)->cdr = val;
}



static void
initialize_environment_alist ()
{
  register unsigned char **e, *s;
  extern char *index ();

  for (e = (unsigned char **) environ; *e; e++)
    {
      s = (unsigned char *) index (*e, '=');
      if (s)
	set_environment_alist (make_string (*e, s - *e),
			       build_string (s + 1));
    }
}


unsigned char *
getenv_1 (str, ephemeral)
     register unsigned char *str;
     int ephemeral;		/* if ephmeral, don't need to gc-proof */
{
  register Lisp_Object env;
  int len = strlen (str);

  for (env = Venvironment_alist; CONSP (env); env = XCONS (env)->cdr)
    {
      register Lisp_Object car = XCONS (env)->car;
      register Lisp_Object tem = XCONS (car)->car;

      if ((len == XSTRING (tem)->size) &&
	  (!bcmp (str, XSTRING (tem)->data, len)))
	{
	  /* Found it in the lisp environment */
	  tem = XCONS (car)->cdr;
	  if (ephemeral)
	    /* Caller promises that gc won't make him lose */
	    return XSTRING (tem)->data;
	  else
	    {
	      register unsigned char **e;
	      unsigned char *s;
	      int ll = XSTRING (tem)->size;

	      /* Look for element in the original unix environment */
	      for (e = (unsigned char **) environ; *e; e++)
		if (!bcmp (str, *e, len) && *(*e + len) == '=')
		  {
		    s = *e + len + 1;
		    if (strlen (s) >= ll)
		      /* User hasn't either hasn't munged it or has set it
			 to something shorter -- we don't have to cons */
		      goto copy;
		    else
		      goto cons;
		  };
	    cons:
	      /* User has setenv'ed it to a diferent value, and our caller
		 isn't guaranteeing that he won't stash it away somewhere.
		 We can't just return a pointer to the lisp string, as that
		 will be corrupted when gc happens.  So, we cons (in such
		 a way that it can't be freed -- though this isn't such a
		 problem since the only callers of getenv (as opposed to
		 those of egetenv) are very early, before the user -could-
		 have frobbed the environment. */
	      s = (unsigned char *) xmalloc (ll + 1);
	    copy:
	      bcopy (XSTRING (tem)->data, s, ll + 1);
	      return (s);
	    }
	}
    }
  return ((unsigned char *) 0);
}

/* unsigned  -- stupid delcaration in lisp.h */ char *
getenv (str)
     register unsigned char *str;
{
  return ((char *) getenv_1 (str, 0));
}

unsigned char *
egetenv (str)
     register unsigned char *str;
{
  return (getenv_1 (str, 1));
}


#if (1 == 1) /* use caller-alloca versions, rather than callee-malloc */
int
size_of_current_environ ()
{
  register int size;
  Lisp_Object tem;

  tem = Flength (Venvironment_alist);
  
  size = (XINT (tem) + 1) * sizeof (unsigned char *);
  /* + 1 for environment-terminating 0 */

  for (tem = Venvironment_alist; !NULL (tem); tem = XCONS (tem)->cdr)
    {
      register Lisp_Object str, val;

      str = XCONS (XCONS (tem)->car)->car;
      val = XCONS (XCONS (tem)->car)->cdr;

      size += (XSTRING (str)->size +
	       XSTRING (val)->size +
	       2);	/* 1 for '=', 1 for '\000' */
    }
  return size;
}

void
get_current_environ (memory_block)
     unsigned char **memory_block;
{
  register unsigned char **e, *s;
  register int len;
  register Lisp_Object tem;

  e = memory_block;

  tem = Flength (Venvironment_alist);
  
  s = (unsigned char *) memory_block
		+ (XINT (tem) + 1) * sizeof (unsigned char *);

  for (tem = Venvironment_alist; !NULL (tem); tem = XCONS (tem)->cdr)
    {
      register Lisp_Object str, val;

      str = XCONS (XCONS (tem)->car)->car;
      val = XCONS (XCONS (tem)->car)->cdr;

      *e++ = s;
      len = XSTRING (str)->size;
      bcopy (XSTRING (str)->data, s, len);
      s += len;
      *s++ = '=';
      len = XSTRING (val)->size;
      bcopy (XSTRING (val)->data, s, len);
      s += len;
      *s++ = '\000';
    }
  *e = 0;
}

#else
/* dead code (this function mallocs, caller frees) superseded by above (which allows caller to use alloca) */
unsigned char **
current_environ ()
{
  unsigned char **env;
  register unsigned char **e, *s;
  register int len, env_len;
  Lisp_Object tem;
  Lisp_Object str, val;

  tem = Flength (Venvironment_alist);

  env_len = (XINT (tem) + 1) * sizeof (char *);
  /* + 1 for terminating 0 */

  len = 0;
  for (tem = Venvironment_alist; !NULL (tem); tem = XCONS (tem)->cdr)
    {
      str = XCONS (XCONS (tem)->car)->car;
      val = XCONS (XCONS (tem)->car)->cdr;

      len += (XSTRING (str)->size +
	      XSTRING (val)->size +
	      2);
    }

  e = env = (unsigned char **) xmalloc (env_len + len);
  s = (unsigned char *) env + env_len;

  for (tem = Venvironment_alist; !NULL (tem); tem = XCONS (tem)->cdr)
    {
      str = XCONS (XCONS (tem)->car)->car;
      val = XCONS (XCONS (tem)->car)->cdr;

      *e++ = s;
      len = XSTRING (str)->size;
      bcopy (XSTRING (str)->data, s, len);
      s += len;
      *s++ = '=';
      len = XSTRING (val)->size;
      bcopy (XSTRING (val)->data, s, len);
      s += len;
      *s++ = '\000';
    }
  *e = 0;

  return env;
}

#endif /* dead code */


DEFUN ("getenv", Fgetenv, Sgetenv, 1, 2, "sEnvironment variable: \np",
  "Return the value of environment variable VAR, as a string.\n\
When invoked interactively, print the value in the echo area.\n\
VAR is a string, the name of the variable,\n\
 or the symbol t, meaning to return an alist representing the\n\
 current environment.")
  (str, interactivep)
     Lisp_Object str, interactivep;
{
  Lisp_Object val;
  
  if (str == Qt)		/* If arg is t, return whole environment */
    return (Fcopy_alist (Venvironment_alist));

  CHECK_STRING (str, 0);
  val = Fcdr (Fassoc (str, Venvironment_alist));
  if (!NULL (interactivep))
    {
      if (NULL (val))
	message ("%s not defined in environment", XSTRING (str)->data);
      else
	message ("\"%s\"", XSTRING (val)->data);
    }
  return val;
}

DEFUN ("setenv", Fsetenv, Ssetenv, 1, 2,
  "sEnvironment variable: \nsSet %s to value: ",
  "Return the value of environment variable VAR, as a string.\n\
When invoked interactively, print the value in the echo area.\n\
VAR is a string, the name of the variable.")
  (str, val)
     Lisp_Object str;
     Lisp_Object val;
{
  Lisp_Object tem;

  CHECK_STRING (str, 0);
  if (!NULL (val))
    CHECK_STRING (val, 0);

  set_environment_alist (str, val);
  return val;
}


syms_of_environ ()
{
  staticpro (&Venvironment_alist);
  defsubr (&Ssetenv);
  defsubr (&Sgetenv);
}

init_environ ()
{
  Venvironment_alist = Qnil;
  initialize_environment_alist ();
}

#endif /* MAINTAIN_ENVIRONMENT */
