/* Record indices of function doc strings stored in a file.
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
#include "paths.h"

#include <sys/types.h>
#include <sys/file.h>	/* Must be after sys/types.h for USG and BSD4_1*/

#ifdef USG5
#include <fcntl.h>
#endif

#ifndef O_RDONLY
#define O_RDONLY 0
#endif

Lisp_Object Vdoc_file_name;

Lisp_Object
get_doc_string (filepos)
     long filepos;
{
  char buf[512 * 32 + 1];
  register int fd;
  register char *name;
  register char *p, *p1;
  register int count;
  extern char *index ();

  if (XTYPE (Vexec_directory) != Lisp_String
      || XTYPE (Vdoc_file_name) != Lisp_String)
    return Qnil;

  name = (char *) alloca (XSTRING (Vexec_directory)->size
			  + XSTRING (Vdoc_file_name)->size + 2);
  strcpy (name, XSTRING (Vexec_directory)->data);
  strcat (name, XSTRING (Vdoc_file_name)->data);

  fd = open (name, O_RDONLY, 0);
  if (fd < 0)
    error ("Cannot open doc string file \"%s\"", name);
  if (0 > lseek (fd, filepos, 0))
    {
      close (fd);
      error ("Position %ld out of range in doc string file \"%s\"",
	     filepos, name);
    }
  p = buf;
  while (p != buf + sizeof buf - 1)
    {
      count = read (fd, p, 512);
      p[count] = 0;
      if (!count)
	break;
      p1 = index (p, '\037');
      if (p1)
	{
	  *p1 = 0;
	  p = p1;
	  break;
	}
      p += count;
    }
  close (fd);
  return make_string (buf, p - buf);
}

DEFUN ("documentation", Fdocumentation, Sdocumentation, 1, 1, 0,
  "Return the documentation string of FUNCTION.")
  (fun1)
     Lisp_Object fun1;
{
  Lisp_Object fun;
  Lisp_Object funcar;
  Lisp_Object tem;

  fun = fun1;
  while (XTYPE (fun) == Lisp_Symbol)
    fun = Fsymbol_function (fun);
  if (XTYPE (fun) == Lisp_Subr)
    {
      if (XSUBR (fun)->doc == 0) return Qnil;
      if ((int) XSUBR (fun)->doc >= 0)
	return Fsubstitute_command_keys (build_string (XSUBR (fun)->doc));
      return Fsubstitute_command_keys (get_doc_string (- (int) XSUBR (fun)->doc));
    }
  if (XTYPE (fun) == Lisp_Vector)
    return build_string ("Prefix command (definition is a Lisp vector of subcommands).");
  if (XTYPE (fun) == Lisp_String)
    return build_string ("Keyboard macro.");
  if (!LISTP(fun))
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  funcar = Fcar (fun);
  if (XTYPE (funcar) != Lisp_Symbol)
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
  if (XSYMBOL (funcar) == XSYMBOL (Qkeymap))
    return build_string ("Prefix command (definition is a list whose cdr is an alist of subcommands.)");
  if (XSYMBOL (funcar) == XSYMBOL (Qlambda)
      || XSYMBOL (funcar) == XSYMBOL (Qautoload))
    {
      tem = Fcar (Fcdr (Fcdr (fun)));
      if (XTYPE (tem) == Lisp_String)
	return Fsubstitute_command_keys (tem);
      if (XTYPE (tem) == Lisp_Int && XINT (tem) >= 0)
	return Fsubstitute_command_keys (get_doc_string (XFASTINT (tem)));
      return Qnil;
    }
  if (XSYMBOL (funcar) == XSYMBOL (Qmocklisp))
    return Qnil;
  if (XSYMBOL (funcar) == XSYMBOL (Qmacro))
    return Fdocumentation (Fcdr (fun));
  else
    return Fsignal (Qinvalid_function, Fcons (fun, Qnil));
}

DEFUN ("Snarf-documentation", Fsnarf_documentation, Ssnarf_documentation,
  1, 1, 0,
  "Used during Emacs initialization, before dumping runnable Emacs,\n\
to find pointers to doc strings stored in etc/DOC... and\n\
record them in function definitions.\n\
One arg, FILENAME, a string which does not include a directory.\n\
The file is found in ../etc now; found in the exec-directory\n\
when doc strings are referred to later in the dumped Emacs.")
  (filename)
     Lisp_Object filename;
{
  int fd;
  char buf[1024 + 1];
  register int filled;
  register int pos;
  register char *p, *end;
  Lisp_Object sym, fun, tem;
  char *name;
  extern char *index ();

  CHECK_STRING (filename, 0);

#ifndef CANNOT_DUMP
  name = (char *) alloca (XSTRING (filename)->size + 8);
  strcpy (name, "../etc/");
#else /* CANNOT_DUMP */
  name = (char *) alloca (XSTRING (filename)->size + sizeof(PATH_EXEC)+1);
  strcpy (name, PATH_EXEC);
  strcat (name, "/");
#endif /* CANNOT_DUMP */
  strcat (name, XSTRING (filename)->data); 	/*** Add this line ***/

  fd = open (name, O_RDONLY, 0);
  if (fd < 0)
    report_file_error ("Opening doc string file", Fcons (filename, Qnil));
  Vdoc_file_name = filename;
  filled = 0;
  pos = 0;
  while (1)
    {
      if (filled < 512)
	filled += read (fd, &buf[filled], sizeof buf - 1 - filled);
      if (!filled)
	break;

      buf[filled] = 0;
      p = buf;
      end = buf + (filled < 512 ? filled : filled - 128);
      while (p != end && *p != '\037') p++;
      if (p != end)
	{
	  end = index (p, '\n');
	  sym = oblookup (Vobarray, p + 1, end - p - 1);
	  if (XTYPE (sym) == Lisp_Symbol)
	    {
	      fun = XSYMBOL (sym)->function;
	      if (XTYPE (fun) == Lisp_Subr)
		XSUBR (fun)->doc = (char *) - (pos + end + 1 - buf);
	      else if (LISTP (fun))
		{
		  tem = XCONS (fun)->car;
		  if (EQ (tem, Qlambda) || EQ (tem, Qautoload))
		    {
		      tem = Fcdr (Fcdr (fun));
		      if (LISTP (tem) && XTYPE (XCONS (tem)->car) == Lisp_Int)
			XFASTINT (XCONS (tem)->car) = (pos + end + 1 - buf);
		    }
		}
	    }
	}
      pos += end - buf;
      filled -= end - buf;
      bcopy (end, buf, filled);
    }
  close (fd);
  return Qnil;
}

extern Lisp_Object where_is_in_buffer ();

DEFUN ("substitute-command-keys", Fsubstitute_command_keys,
  Ssubstitute_command_keys, 1, 1, 0,
  "Return the STRING with substrings of the form \\=\\[COMMAND]\n\
replaced by either:  a keystroke sequence that will invoke COMMAND,\n\
or \"M-x COMMAND\" if COMMAND is not on any keys.\n\
Substrings of the form \\=\\{MAPVAR} are replaced by summaries\n\
\(made by describe-bindings) of the value of MAPVAR, taken as a keymap.\n\
\\=\\= quotes the following character and is discarded;\n\
thus, \\=\\=\\=\\= puts \\=\\= into the output, and \\=\\=\\=\\[ puts \\=\\[ into the output.")
  (str)
     Lisp_Object str;
{
  unsigned char *buf;
  int didone = 0;
  register unsigned char *strp;
  register unsigned char *bufp;
  register unsigned char *send;
  int bsize;
  unsigned char *new;
  Lisp_Object key, tem;
  unsigned char *funp;
  int func;
  struct buffer *oldbuf;

  if (NULL (str))
    return Qnil;

  CHECK_STRING (str, 0);
  strp = (unsigned char *) XSTRING (str)->data;
  send = strp + XSTRING (str)->size;

  bsize = XSTRING (str)->size;
  bufp = buf = (unsigned char *) xmalloc (bsize);

  while (strp < send)
    {
      if (strp[0] == '\\' && strp[1] == '=')
	{
	  /* \= quotes the next character;
	     thus, to put in \[ without its special meaning, use \=\[.  */
	  didone = 1;
	  *bufp++ = strp[2];
	  strp += 3;
	}
      else if (strp[0] == '\\' && strp[1] == '[')
	{
	  didone = 1;
	  strp += 2;		/* skip \[ */
	  funp = strp;

	  while (strp < send && *strp != ']')
	    strp++;
	  func = strp - funp;

	  key = Fintern (make_string (funp, func), Qnil);
	  key = where_is_in_buffer (key, bf_cur, 1);
	  strp++;			/* skip ] */

	  if (NULL (key))	/* but not on any keys */
	    {
	      new = (unsigned char *) xrealloc (buf, bsize += 4);
	      bufp += new - buf;
	      buf = new;
	      strcpy (bufp, "M-x ");
	      bufp += 4;
	    }
	  else
	    {		/* function is on a key */
	      key = Fkey_description (key);
	      funp = XSTRING (key)->data;
	      func = XSTRING (key)->size;
	    }

	subst:
	  new = (unsigned char *) xrealloc (buf, bsize += func);
	  bufp += new - buf;
	  buf = new;
	  bcopy (funp, bufp, func);
	  bufp += func;
	}
      else if (strp[0] == '\\' && strp[1] == '{')
	{
	  didone = 1;
	  strp += 2;		/* skip \( */
	  funp = strp;

	  while (strp < send && *strp != '}')
	    strp++;
	  func = strp - funp;
	  strp++;			/* skip } */

	  oldbuf = bf_cur;
	  SetBfp (XBUFFER (Vprin1_to_string_buffer));
	  key = Fintern (make_string (funp, func), Qnil);
	  if ((tem = (Fboundp (key)), NULL (tem)) ||
	      (tem = (Fsymbol_value (key)), NULL (tem)))
	    {
	      key = Fsymbol_name (key);
	      InsStr ("\nUses keymap \"");
	      InsCStr (XSTRING (key)->data, XSTRING (key)->size);
	      InsStr ("\", which is not currently defined.\n");
	    }
	  else
	    {
	      key = Fsymbol_value (key);
	      describe_map_tree (key, 1);
	    }
	  key = Fbuffer_string ();
	  Ferase_buffer ();
	  SetBfp (oldbuf);
	  funp = XSTRING (key)->data;
	  func = XSTRING (key)->size;
	  goto subst;
	}
      else			/* just copy other chars */
	*bufp++ = *strp++;
    }

  if (didone)			/* don't bother if nothing substituted */
    key = make_string (buf, bufp - buf);
  else
    key = str;
  free (buf);
  return key;
}

syms_of_doc ()
{
  staticpro (&Vdoc_file_name);
  Vdoc_file_name = Qnil;

  defsubr (&Sdocumentation);
  defsubr (&Ssnarf_documentation);
  defsubr (&Ssubstitute_command_keys);
}
