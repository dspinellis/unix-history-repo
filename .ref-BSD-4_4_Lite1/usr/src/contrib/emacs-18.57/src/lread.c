/* Lisp parsing and input streams.
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


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>
#undef NULL
#include "config.h"
#include "lisp.h"

#ifndef standalone
#include "buffer.h"
#include "paths.h"
#endif

#ifdef lint
#include <sys/inode.h>
#endif /* lint */

#ifndef X_OK
#define X_OK 01
#endif

Lisp_Object Qread_char, Qget_file_char, Qstandard_input;
Lisp_Object Qvariable_documentation, Vvalues, Vstandard_input;

/* non-zero if inside `load' */
int load_in_progress;

/* Search path for files to be loaded. */
Lisp_Object Vload_path;

/* File for get_file_char to read from.  Use by load */
static FILE *instream;

/* When nonzero, read conses in pure space */
static int read_pure;

/* For use within read-from-string (this reader is non-reentrant!!) */
static int read_from_string_index;
static int read_from_string_limit;

/* Handle unreading and rereading of characters.
 Write READCHAR to read a character, UNREAD(c) to unread c to be read again. */

static int unrch;

static int readchar (readcharfun)
     Lisp_Object readcharfun;
{
  Lisp_Object tem;
  register struct buffer *inbuffer;
  register int c, mpos;

  if (unrch >= 0)
    {
      c = unrch;
      unrch = -1;
      return c;
    }
  if (XTYPE (readcharfun) == Lisp_Buffer)
    {
      inbuffer = XBUFFER (readcharfun);

      if (BUF_PT (inbuffer) >= BUF_ZV (inbuffer))
	return -1;
      c = *(unsigned char *) BUF_CHAR_ADDRESS (inbuffer, BUF_PT (inbuffer));
      SET_BUF_PT (inbuffer, BUF_PT (inbuffer) + 1);
      return c;
    }
  if (XTYPE (readcharfun) == Lisp_Marker)
    {
      inbuffer = XMARKER (readcharfun)->buffer;
      mpos = marker_position (readcharfun);

      if (mpos > BUF_ZV (inbuffer) - 1)
	return -1;
      c = *(unsigned char *) BUF_CHAR_ADDRESS (inbuffer, mpos);
      if (mpos != BUF_GPT (inbuffer))
	XMARKER (readcharfun)->bufpos++;
      else
	Fset_marker (readcharfun, make_number (mpos + 1),
		     Fmarker_buffer (readcharfun));
      return c;
    }
  if (EQ (readcharfun, Qget_file_char))
    return getc (instream);

  if (XTYPE (readcharfun) == Lisp_String)
    {
      register int c;
      /* This used to be return of a conditional expression,
	 but that truncated -1 to a char on VMS.  */
      if (read_from_string_index < read_from_string_limit)
	c = XSTRING (readcharfun)->data[read_from_string_index++];
      else
	c = -1;
      return c;
    }

  tem = call0 (readcharfun);

  if (NULL (tem))
    return -1;
  return XINT (tem);
}

#define READCHAR readchar(readcharfun)
#define UNREAD(c) (unrch = c)

static Lisp_Object read0 (), read1 (), read_list (), read_vector ();

/* get a character from the tty */

DEFUN ("read-char", Fread_char, Sread_char, 0, 0, 0,
  "Read a character from the command input (keyboard or macro).\n\
It is returned as a number.")
  ()
{
  register Lisp_Object val;

#ifndef standalone
  XSET (val, Lisp_Int, read_command_char (0));
#else
  XSET (val, Lisp_Int, getchar ());
#endif

  return val;
}

DEFUN ("get-file-char", Fget_file_char, Sget_file_char, 0, 0, 0,
  "Don't use this yourself.")
  ()
{
  register Lisp_Object val;
  XSET (val, Lisp_Int, getc (instream));
  return val;
}

void readevalloop ();
static Lisp_Object load_unwind ();

DEFUN ("load", Fload, Sload, 1, 4, 0,
  "Execute a file of Lisp code named FILE.\n\
First tries FILE with .elc appended, then tries with .el,\n\
 then tries FILE unmodified.  Searches directories in  load-path.\n\
If optional second arg NOERROR is non-nil,\n\
 report no error if FILE doesn't exist.\n\
Print messages at start and end of loading unless\n\
 optional third arg NOMESSAGE is non-nil.\n\
If optional fourth arg NOSUFFIX is non-nil, don't try adding\n\
 suffixes .elc or .el to the specified name FILE.\n\
Return t if file exists.")
  (str, noerror, nomessage, nosuffix)
     Lisp_Object str, noerror, nomessage, nosuffix;
{
  register FILE *stream;
  register int fd = -1;
  register Lisp_Object lispstream;
  register FILE **ptr;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1;

  CHECK_STRING (str, 0);
  str = Fsubstitute_in_file_name (str);

  /* Avoid weird lossage with null string as arg,
     since it would try to load a directory as a Lisp file */
  if (XSTRING (str)->size > 0)
    {
      fd = openp (Vload_path, str, !NULL (nosuffix) ? "" : ".elc:.el:", 0, 0);
    }

  if (fd < 0)
    if (NULL (noerror))
      while (1)
	Fsignal (Qfile_error, Fcons (build_string ("Cannot open load file"),
				     Fcons (str, Qnil)));
    else return Qnil;

  stream = fdopen (fd, "r");
  if (stream == 0)
    {
      close (fd);
      error ("Failure to create stdio stream for %s", XSTRING (str)->data);
    }

  if (NULL (nomessage))
    message ("Loading %s...", XSTRING (str)->data);

  GCPRO1 (str);
  ptr = (FILE **) xmalloc (sizeof (FILE *));
  *ptr = stream;
  XSET (lispstream, Lisp_Internal_Stream, (int) ptr);
  record_unwind_protect (load_unwind, lispstream);
  load_in_progress++;
  readevalloop (Qget_file_char, stream, Feval, 0);
  unbind_to (count);
  UNGCPRO;

  if (!noninteractive && NULL (nomessage))
    message ("Loading %s...done", XSTRING (str)->data);
  return Qt;
}

static Lisp_Object
load_unwind (stream)  /* used as unwind-protect function in load */
     Lisp_Object stream;
{
  fclose (*(FILE **) XSTRING (stream));
  free (XPNTR (stream));
  if (--load_in_progress < 0) load_in_progress = 0;
  return Qnil;
}


static int
absolute_filename_p (pathname)
     Lisp_Object pathname;
{
  register unsigned char *s = XSTRING (pathname)->data;
  return (*s == '~' || *s == '/'
#ifdef VMS
	  || index (s, ':')
#endif /* VMS */
	  );
}

/* Search for a file whose name is STR, looking in directories
   in the Lisp list PATH, and trying suffixes from SUFFIX.
   SUFFIX is a string containing possible suffixes separated by colons.
   On success, returns a file descriptor.  On failure, returns -1.

   EXEC_ONLY nonzero means don't open the files,
   just look for one that is executable.  In this case,
   returns 1 on success.

   If STOREPTR is nonzero, it points to a slot where the name of
   the file actually found should be stored as a Lisp string.
   Nil is stored there on failure.  */

int
openp (path, str, suffix, storeptr, exec_only)
     Lisp_Object path, str;
     char *suffix;
     Lisp_Object *storeptr;
     int exec_only;
{
  register int fd;
  int fn_size = 100;
  char buf[100];
  register char *fn = buf;
  int absolute = 0;
  int want_size;
  register Lisp_Object filename;
  struct stat st;

  if (storeptr)
    *storeptr = Qnil;

  if (absolute_filename_p (str))
    absolute = 1;

  for (; !NULL (path); path = Fcdr (path))
    {
      char *nsuffix;

      filename = Fexpand_file_name (str, Fcar (path));
      if (!absolute_filename_p (filename))
	/* If there are non-absolute elts in PATH (eg ".") */
	/* Of course, this could conceivably lose if luser sets
	   default-directory to be something non-absolute... */
	{
	  filename = Fexpand_file_name (filename, current_buffer->directory);
	  if (!absolute_filename_p (filename))
	    /* Give up on this path element! */
	    continue;
	}

      /* Calculate maximum size of any filename made from
	 this path element/specified file name and any possible suffix.  */
      want_size = strlen (suffix) + XSTRING (filename)->size + 1;
      if (fn_size < want_size)
	fn = (char *) alloca (fn_size = 100 + want_size);

      nsuffix = suffix;

      /* Loop over suffixes.  */
      while (1)
	{
	  char *esuffix = (char *) index (nsuffix, ':');
	  int lsuffix = esuffix ? esuffix - nsuffix : strlen (nsuffix);

	  /* Concatenate path element/specified name with the suffix.  */
	  strncpy (fn, XSTRING (filename)->data, XSTRING (filename)->size);
	  fn[XSTRING (filename)->size] = 0;
	  if (lsuffix != 0)  /* Bug happens on CCI if lsuffix is 0.  */
	    strncat (fn, nsuffix, lsuffix);

	  /* Ignore file if it's a directory.  */
	  if (stat (fn, &st) >= 0
	      && (st.st_mode & S_IFMT) != S_IFDIR)
	    {
	      /* Check that we can access or open it.  */
	      if (exec_only)
		fd = !access (fn, X_OK) ? 1 : -1;
	      else
		fd = open (fn, 0, 0);

	      if (fd >= 0)
		{
		  /* We succeeded; return this descriptor and filename.  */
		  if (storeptr)
		    *storeptr = build_string (fn);
		  return fd;
		}
	    }

	  /* Advance to next suffix.  */
	  if (esuffix == 0)
	    break;
	  nsuffix += lsuffix + 1;
	}
      if (absolute) return -1;
    }

  return -1;
}


Lisp_Object
unreadpure ()	/* Used as unwind-protect function in readevalloop */
{
  read_pure = 0;
  return Qnil;
}

static void
readevalloop (readcharfun, stream, evalfun, printflag)
     Lisp_Object readcharfun;
     FILE *stream;     
     Lisp_Object (*evalfun) ();
     int printflag;
{
  register int c;
  register Lisp_Object val;
  register int xunrch;
  int count = specpdl_ptr - specpdl;

  specbind (Qstandard_input, readcharfun);

  unrch = -1;

  while (1)
    {
      instream = stream;
      c = READCHAR;
      if (c == ';')
	{
	  while ((c = READCHAR) != '\n' && c != -1);
	  continue;
	}
      if (c < 0) break;
      if (c == ' ' || c == '\t' || c == '\n' || c == '\f') continue;

      if (!NULL (Vpurify_flag) && c == '(')
	{
	  record_unwind_protect (unreadpure, Qnil);
	  val = read_list (-1, readcharfun);
	  unbind_to (count + 1);
	}
      else
	{
	  UNREAD (c);
	  val = read0 (readcharfun);
	}

      xunrch = unrch;
      unrch = -1;
      val = (*evalfun) (val);
      if (printflag)
	{
	  Vvalues = Fcons (val, Vvalues);
	  if (EQ (Vstandard_output, Qt))
	    Fprin1 (val, Qnil);
	  else
	    Fprint (val, Qnil);
	}
      unrch = xunrch;
    }

  unbind_to (count);
}

#ifndef standalone

DEFUN ("eval-current-buffer", Feval_current_buffer, Seval_current_buffer, 0, 1, "",
  "Execute the current buffer as Lisp code.\n\
Programs can pass argument PRINTFLAG which controls printing of output:\n\
nil means discard it; anything else is stream for print.")
  (printflag)
     Lisp_Object printflag;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object tem;
  if (NULL (printflag))
    tem = Qsymbolp;
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  record_unwind_protect (save_excursion_restore, save_excursion_save ());
  SET_PT (BEGV);
  readevalloop (Fcurrent_buffer (), 0, Feval, !NULL (printflag));
  unbind_to (count);
  return Qnil;
}

DEFUN ("eval-region", Feval_region, Seval_region, 2, 3, "r",
  "Execute the region as Lisp code.\n\
When called from programs, expects two arguments,\n\
giving starting and ending indices in the current buffer\n\
of the text to be executed.\n\
Programs can pass third argument PRINTFLAG which controls printing of output:\n\
nil means discard it; anything else is stream for print.")
  (b, e, printflag)
     Lisp_Object b, e, printflag;
{
  int count = specpdl_ptr - specpdl;
  Lisp_Object tem;

  if (NULL (printflag))
    tem = Qsymbolp;
  else
    tem = printflag;
  specbind (Qstandard_output, tem);
  if (NULL (printflag))
    record_unwind_protect (save_excursion_restore, save_excursion_save ());
  record_unwind_protect (save_restriction_restore, save_restriction_save ());
  /* This both uses b and checks its type.  */
  Fgoto_char (b);
  Fnarrow_to_region (make_number (BEGV), e);
  readevalloop (Fcurrent_buffer (), 0, Feval, !NULL (printflag));
  unbind_to (count);
  return Qnil;
}

#endif standalone

DEFUN ("read", Fread, Sread, 0, 1, 0,
  "Read one Lisp expression as text from STREAM, return as Lisp object.\n\
If STREAM is nil, use the value of standard-input (which see).\n\
STREAM or standard-input may be:\n\
 a buffer (read from point and advance it)\n\
 a marker (read from where it points and advance it)\n\
 a function (call it with no arguments for each character)\n\
 a string (takes text from string, starting at the beginning)\n\
 t (read text line using minibuffer and use it).")
  (readcharfun)
     Lisp_Object readcharfun;
{
  extern Lisp_Object Fread_minibuffer ();

  unrch = -1;	/* Allow buffering-back only within a read. */

  if (NULL (readcharfun))
    readcharfun = Vstandard_input;
  if (EQ (readcharfun, Qt))
    readcharfun = Qread_char;

#ifndef standalone
  if (EQ (readcharfun, Qread_char))
    return Fread_minibuffer (build_string ("Lisp expression: "), Qnil);
#endif

  if (XTYPE (readcharfun) == Lisp_String)
    return Fcar (Fread_from_string (readcharfun, Qnil, Qnil));

  return read0 (readcharfun);
}

DEFUN ("read-from-string", Fread_from_string, Sread_from_string, 1, 3, 0,
  "Read one Lisp expression which is represented as text by STRING.\n\
Returns a cons: (OBJECT-READ . FINAL-STRING-INDEX).\n\
START and END optionally delimit a substring of STRING from which to read;\n\
 they default to 0 and (length STRING) respectively.")
  (string, start, end)
     Lisp_Object string, start, end;
{
  int startval, endval;
  Lisp_Object tem;

  CHECK_STRING (string,0);

  if (NULL (end))
    endval = XSTRING (string)->size;
  else
    { CHECK_NUMBER (end,2);
      endval = XINT (end);
      if (endval < 0 || endval > XSTRING (string)->size)
	args_out_of_range (string, end);
    }

  if (NULL (start))
    startval = 0;
  else
    { CHECK_NUMBER (start,1);
      startval = XINT (start);
      if (startval < 0 || startval > endval)
	args_out_of_range (string, start);
    }

  read_from_string_index = startval;
  read_from_string_limit = endval;

  unrch = -1;	/* Allow buffering-back only within a read. */

  tem = read0 (string);
  return Fcons (tem, make_number (read_from_string_index));
}

/* Use this for recursive reads, in contexts where internal tokens are not allowed. */

static Lisp_Object
read0 (readcharfun)
     Lisp_Object readcharfun;
{
  register Lisp_Object val;
  char c;

  val = read1 (readcharfun);
  if (XTYPE (val) == Lisp_Internal)
    {
      c = XINT (val);
      return Fsignal (Qinvalid_read_syntax, Fcons (make_string (&c, 1), Qnil));
    }

  return val;
}

static int read_buffer_size;
static char *read_buffer;

static Lisp_Object
read1 (readcharfun)
     register Lisp_Object readcharfun;
{
  register int c;

 retry:

  c = READCHAR;
  if (c < 0) return Fsignal (Qend_of_file, Qnil);

  switch (c)
    {
    case '(':
      return read_list (0, readcharfun);

    case '[':
      return read_vector (readcharfun);

    case ')':
    case ']':
    case '.':
      {
	register Lisp_Object val;
	XSET (val, Lisp_Internal, c);
	return val;
      }

    case '#':
      return Fsignal (Qinvalid_read_syntax, Fcons (make_string ("#", 1), Qnil));

    case ';':
      while ((c = READCHAR) >= 0 && c != '\n');
      goto retry;

    case '\'':
      {
	return Fcons (Qquote, Fcons (read0 (readcharfun), Qnil));
      }

    case '?':
      {
	register Lisp_Object val;

	XSET (val, Lisp_Int, READCHAR);
	if (XFASTINT (val) == '\\')
	  XSETINT (val, read_escape (readcharfun));

	return val;
      }

    case '\"':
      {
	register char *p = read_buffer;
	register char *end = read_buffer + read_buffer_size;
	register int c;
	int cancel = 0;

	while ((c = READCHAR) >= 0 &&
		(c != '\"' || (c = READCHAR) == '\"'))
	  {
	    if (p == end)
	      {
		char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
		p += new - read_buffer;
		read_buffer += new - read_buffer;
		end = read_buffer + read_buffer_size;
	      }
	    if (c == '\\')
	      c = read_escape (readcharfun);
	    /* c is -1 if \ newline has just been seen */
	    if (c < 0)
	      {
		if (p == read_buffer)
		  cancel = 1;
	      }
	    else
	      *p++ = c;
	  }

	UNREAD (c);
	/* If purifying, and string starts with \ newline,
	   return zero instead.  This is for doc strings
	   that we are really going to find in etc/DOC.nn.nn  */
	if (!NULL (Vpurify_flag) && NULL (Vdoc_file_name) && cancel)
	  return make_number (0);

	if (read_pure)
	  return make_pure_string (read_buffer, p - read_buffer);
	else
	  return make_string (read_buffer, p - read_buffer);
      }

    default:
      if (c <= 040) goto retry;
      {
	register char *p = read_buffer;

	{
	  register char *end = read_buffer + read_buffer_size;

	  while (c > 040 && 
		 !(c == '\"' || c == '\'' || c == ';' || c == '?'
		   || c == '(' || c == ')' || c =='.'
		   || c == '[' || c == ']' || c == '#'
		   ))
	    {
	      if (p == end)
		{
		  register char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
		  p += new - read_buffer;
		  read_buffer += new - read_buffer;
		  end = read_buffer + read_buffer_size;
		}
	      if (c == '\\')
		c = READCHAR;
	      *p++ = c;
	      c = READCHAR;
	    }

	  if (p == end)
	    {
	      char *new = (char *) xrealloc (read_buffer, read_buffer_size *= 2);
	      p += new - read_buffer;
	      read_buffer += new - read_buffer;
/*	      end = read_buffer + read_buffer_size;  */
	    }
	  *p = 0;
	  UNREAD (c);
	}

	/* Is it an integer? */
	{
	  register char *p1;
	  register Lisp_Object val;
	  p1 = read_buffer;
	  if (*p1 == '+' || *p1 == '-') p1++;
	  if (p1 != p)
	    {
	      while (p1 != p && (c = *p1) >= '0' && c <= '9') p1++;
	      if (p1 == p)
		/* It is. */
		{
		  XSET (val, Lisp_Int, atoi (read_buffer));
		  return val;
		}
	    }
	}

	return intern (read_buffer);
      }
    }
}

static Lisp_Object
read_vector (readcharfun)
     Lisp_Object readcharfun;
{
  register int i;
  register int size;
  register Lisp_Object *ptr;
  register Lisp_Object tem, vector;
  register struct Lisp_Cons *otem;
  Lisp_Object len;

  tem = read_list (1, readcharfun);
  len = Flength (tem);
  vector = (read_pure ? make_pure_vector (XINT (len)) : Fmake_vector (len, Qnil));


  size = XVECTOR (vector)->size;
  ptr = XVECTOR (vector)->contents;
  for (i = 0; i < size; i++)
    {
      ptr[i] = read_pure ? Fpurecopy (Fcar (tem)) : Fcar (tem);
      otem = XCONS (tem);
      tem = Fcdr (tem);
      free_cons (otem);
    }
  return vector;
}
  
/* flag = 1 means check for ] to terminate rather than ) and .
   flag = -1 means check for starting with defun
    and make structure pure.  */

static Lisp_Object
read_list (flag, readcharfun)
     int flag;
     register Lisp_Object readcharfun;
{
  /* -1 means check next element for defun,
     0 means don't check,
     1 means already checked and found defun. */
  int defunflag = flag < 0 ? -1 : 0;
  Lisp_Object val, tail;
  register Lisp_Object elt, tem;
  struct gcpro gcpro1, gcpro2;

  val = Qnil;
  tail = Qnil;

  while (1)
    {
      GCPRO2 (val, tail);
      elt = read1 (readcharfun);
      UNGCPRO;
      if (XTYPE (elt) == Lisp_Internal)
	{
	  if (flag > 0)
	    {
	      if (XINT (elt) == ']')
		return val;
	      return Fsignal (Qinvalid_read_syntax, Fcons (make_string (") or . in a vector", 18), Qnil));
	    }
	  if (XINT (elt) == ')')
	    return val;
	  if (XINT (elt) == '.')
	    {
	      GCPRO2 (val, tail);
	      if (!NULL (tail))
		XCONS (tail)->cdr = read0 (readcharfun);
	      else
		val = read0 (readcharfun);
	      elt = read1 (readcharfun);
	      UNGCPRO;
	      if (XTYPE (elt) == Lisp_Internal && XINT (elt) == ')')
		return val;
	      return Fsignal (Qinvalid_read_syntax, Fcons (make_string (". in wrong context", 18), Qnil));
	    }
	  return Fsignal (Qinvalid_read_syntax, Fcons (make_string ("] in a list", 11), Qnil));
	}
      tem = (read_pure && flag <= 0
	     ? pure_cons (elt, Qnil)
	     : Fcons (elt, Qnil));
      if (!NULL (tail))
	XCONS (tail)->cdr = tem;
      else
	val = tem;
      tail = tem;
      if (defunflag < 0)
	defunflag = EQ (elt, Qdefun);
      else if (defunflag > 0)
	read_pure = 1;
    }
}

static int
read_escape (readcharfun)
     Lisp_Object readcharfun;
{
  register int c = READCHAR;
  switch (c)
    {
    case 'a':
      return 007;
    case 'b':
      return '\b';
    case 'e':
      return 033;
    case 'f':
      return '\f';
    case 'n':
      return '\n';
    case 'r':
      return '\r';
    case 't':
      return '\t';
    case 'v':
      return '\v';
    case '\n':
      return -1;

    case 'M':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      return c | 0200;

    case 'C':
      c = READCHAR;
      if (c != '-')
	error ("Invalid escape character syntax");
    case '^':
      c = READCHAR;
      if (c == '\\')
	c = read_escape (readcharfun);
      if (c == '?')
	return 0177;
      return (c & 0200) | (c & 037);
      
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
      {
	register int i = c - '0';
	register int count = 0;
	while (++count < 3)
	  {
	    if ((c = READCHAR) >= '0' && c <= '7')
	      {
		i *= 8;
		i += c - '0';
	      }
	    else
	      {
		UNREAD (c);
		break;
	      }
	  }
	return i;
      }
    default:
      return c;
    }
}

Lisp_Object Vobarray;
Lisp_Object initial_obarray;

Lisp_Object
check_obarray (obarray)
     Lisp_Object obarray;
{
  while (XTYPE (obarray) != Lisp_Vector || XVECTOR (obarray)->size == 0)
    {
      /* If Vobarray is now invalid, force it to be valid.  */
      if (EQ (Vobarray, obarray)) Vobarray = initial_obarray;

      obarray = wrong_type_argument (Qvectorp, obarray);
    }
  return obarray;
}

static int hash_string ();
Lisp_Object oblookup ();

Lisp_Object
intern (str)
     char *str;
{
  Lisp_Object tem;
  int len = strlen (str);
  Lisp_Object obarray = Vobarray;
  if (XTYPE (obarray) != Lisp_Vector || XVECTOR (obarray)->size == 0)
    obarray = check_obarray (obarray);
  tem = oblookup (obarray, str, len);
  if (XTYPE (tem) == Lisp_Symbol)
    return tem;
  return Fintern ((!NULL (Vpurify_flag)
		   ? make_pure_string (str, len)
		   : make_string (str, len)),
		  obarray);
}

DEFUN ("intern", Fintern, Sintern, 1, 2, 0,
  "Return the symbol whose name is STRING.\n\
A second optional argument specifies the obarray to use;\n\
it defaults to the value of  obarray.")
  (str, obarray)
     Lisp_Object str, obarray;
{
  register Lisp_Object tem, sym, *ptr;

  if (NULL (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  CHECK_STRING (str, 0);

  tem = oblookup (obarray, XSTRING (str)->data, XSTRING (str)->size);
  if (XTYPE (tem) != Lisp_Int)
    return tem;

  if (!NULL (Vpurify_flag))
    str = Fpurecopy (str);
  sym = Fmake_symbol (str);

  ptr = &XVECTOR (obarray)->contents[XINT (tem)];
  if (XTYPE (*ptr) == Lisp_Symbol)
    XSYMBOL (sym)->next = XSYMBOL (*ptr);
  else
    XSYMBOL (sym)->next = 0;
  *ptr = sym;
  return sym;
}

DEFUN ("intern-soft", Fintern_soft, Sintern_soft, 1, 2, 0,
  "Return the symbol whose name is STRING, or nil if none exists yet.\n\
A second optional argument specifies the obarray to use;\n\
it defaults to the value of  obarray.")
  (str, obarray)
     Lisp_Object str, obarray;
{
  register Lisp_Object tem;

  if (NULL (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  CHECK_STRING (str, 0);

  tem = oblookup (obarray, XSTRING (str)->data, XSTRING (str)->size);
  if (XTYPE (tem) != Lisp_Int)
    return tem;
  return Qnil;
}

Lisp_Object
oblookup (obarray, ptr, size)
     Lisp_Object obarray;
     register char *ptr;
     register int size;
{
  int hash, obsize;
  register Lisp_Object tail;
  Lisp_Object bucket, tem;

  if (XTYPE (obarray) != Lisp_Vector ||
      (obsize = XVECTOR (obarray)->size) == 0)
    {
      obarray = check_obarray (obarray);
      obsize = XVECTOR (obarray)->size;
    }
  /* Combining next two lines breaks VMS C 2.3.  */
  hash = hash_string (ptr, size);
  hash %= obsize;
  bucket = XVECTOR (obarray)->contents[hash];
  if (XFASTINT (bucket) == 0)
    ;
  else if (XTYPE (bucket) != Lisp_Symbol)
    error ("Bad data in guts of obarray"); /* Like CADR error message */
  else for (tail = bucket; ; XSET (tail, Lisp_Symbol, XSYMBOL (tail)->next))
      {
	if (XSYMBOL (tail)->name->size == size &&
	    !bcmp (XSYMBOL (tail)->name->data, ptr, size))
	  return tail;
	else if (XSYMBOL (tail)->next == 0)
	  break;
      }
  XSET (tem, Lisp_Int, hash);
  return tem;
}

static int
hash_string (ptr, len)
     unsigned char *ptr;
     int len;
{
  register unsigned char *p = ptr;
  register unsigned char *end = p + len;
  register unsigned char c;
  register int hash = 0;

  while (p != end)
    {
      c = *p++;
      if (c >= 0140) c -= 40;
      hash = ((hash<<3) + (hash>>28) + c);
    }
  return hash & 07777777777;
}

void
map_obarray (obarray, fn, arg)
     Lisp_Object obarray;
     int (*fn) ();
     Lisp_Object arg;
{
  register int i;
  register Lisp_Object tail;
  CHECK_VECTOR (obarray, 1);
  for (i = XVECTOR (obarray)->size - 1; i >= 0; i--)
    {
      tail = XVECTOR (obarray)->contents[i];
      if (XFASTINT (tail) != 0)
	while (1)
	  {
	    (*fn) (tail, arg);
	    if (XSYMBOL (tail)->next == 0)
	      break;
	    XSET (tail, Lisp_Symbol, XSYMBOL (tail)->next);
	  }
    }
}

mapatoms_1 (sym, function)
     Lisp_Object sym, function;
{
  call1 (function, sym);
}

DEFUN ("mapatoms", Fmapatoms, Smapatoms, 1, 2, 0,
  "Call FUNCTION on every symbol in OBARRAY.\n\
OBARRAY defaults to the value of  obarray.")
  (function, obarray)
     Lisp_Object function, obarray;
{
  Lisp_Object tem;

  if (NULL (obarray)) obarray = Vobarray;
  obarray = check_obarray (obarray);

  map_obarray (obarray, mapatoms_1, function);
  return Qnil;
}

#define OBARRAY_SIZE 511

void
init_obarray ()
{
  Lisp_Object oblength;
  int hash;
  Lisp_Object *tem;

  XFASTINT (oblength) = OBARRAY_SIZE;

  Qnil = Fmake_symbol (make_pure_string ("nil", 3));
  Vobarray = Fmake_vector (oblength, make_number (0));
  initial_obarray = Vobarray;
  staticpro (&Vobarray);
  staticpro (&initial_obarray);
  /* Intern nil in the obarray */
  /* These locals are to kludge around a pyramid compiler bug. */
  hash = hash_string ("nil", 3);
  /* Separate statement here to avoid VAXC bug. */
  hash %= OBARRAY_SIZE;
  tem = &XVECTOR (Vobarray)->contents[hash];
  *tem = Qnil;

  Qunbound = Fmake_symbol (make_pure_string ("unbound", 7));
  XSYMBOL (Qnil)->function = Qunbound;
  XSYMBOL (Qunbound)->value = Qunbound;
  XSYMBOL (Qunbound)->function = Qunbound;

  Qt = intern ("t");
  XSYMBOL (Qnil)->value = Qnil;
  XSYMBOL (Qnil)->plist = Qnil;
  XSYMBOL (Qt)->value = Qt;

  /* Qt is correct even if CANNOT_DUMP.  loadup.el will set to nil at end.  */
  Vpurify_flag = Qt;

  Qvariable_documentation = intern ("variable-documentation");

  read_buffer_size = 100;
  read_buffer = (char *) malloc (read_buffer_size);
}

void
defsubr (sname)
     struct Lisp_Subr *sname;
{
  Lisp_Object sym;
  sym = intern (sname->symbol_name);
  XSET (XSYMBOL (sym)->function, Lisp_Subr, sname);
}

#ifdef NOTDEF /* use fset in subr.el now */
void
defalias (sname, string)
     struct Lisp_Subr *sname;
     char *string;
{
  Lisp_Object sym;
  sym = intern (string);
  XSET (XSYMBOL (sym)->function, Lisp_Subr, sname);
}
#endif NOTDEF

/* New replacement for DefIntVar; it ignores the doc string argument
   on the assumption that make-docfile will handle that.  */
/* Define an "integer variable"; a symbol whose value is forwarded
 to a C variable of type int.  Sample call: */
  /* DEFVARINT ("indent-tabs-mode", &indent_tabs_mode, "Documentation");  */

void
defvar_int (namestring, address, doc)
     char *namestring;
     int *address;
     char *doc;
{
  Lisp_Object sym;
  sym = intern (namestring);
  XSET (XSYMBOL (sym)->value, Lisp_Intfwd, address);
}

/* Similar but define a variable whose value is T if address contains 1,
 NIL if address contains 0 */

void
defvar_bool (namestring, address, doc)
     char *namestring;
     int *address;
     char *doc;
{
  Lisp_Object sym;
  sym = intern (namestring);
  XSET (XSYMBOL (sym)->value, Lisp_Boolfwd, address);
}

/* Similar but define a variable whose value is the Lisp Object stored at address. */

void
defvar_lisp (namestring, address, doc)
     char *namestring;
     Lisp_Object *address;
     char *doc;
{
  Lisp_Object sym;
  sym = intern (namestring);
  XSET (XSYMBOL (sym)->value, Lisp_Objfwd, address);
  staticpro (address);
}

/* Similar but don't request gc-marking of the C variable.
   Used when that variable will be gc-marked for some other reason,
   since marking the same slot twice can cause trouble with strings.  */

void
defvar_lisp_nopro (namestring, address, doc)
     char *namestring;
     Lisp_Object *address;
     char *doc;
{
  Lisp_Object sym;
  sym = intern (namestring);
  XSET (XSYMBOL (sym)->value, Lisp_Objfwd, address);
}

#ifndef standalone

/* Similar but define a variable whose value is the Lisp Object stored in
 the current buffer.  address is the address of the slot in the buffer that is current now. */

void
defvar_per_buffer (namestring, address, doc)
     char *namestring;
     Lisp_Object *address;
     char *doc;
{
  Lisp_Object sym;
  int offset;
  extern struct buffer buffer_local_symbols;

  sym = intern (namestring);
  offset = (char *)address - (char *)current_buffer;

  XSET (XSYMBOL (sym)->value, Lisp_Buffer_Objfwd,
	(Lisp_Object *) offset);
  *(Lisp_Object *)(offset + (char *)&buffer_local_symbols) = sym;
  if (*(int *)(offset + (char *)&buffer_local_flags) == 0)
    /* Did a DEFVAR_PER_BUFFER without initializing the corresponding
       slot of buffer_local_flags */
    abort ();
}

#endif standalone

init_read ()
{
  char *normal = PATH_LOADSEARCH;
  Lisp_Object normal_path;

  /* Warn if dirs in the *standard* path don't exist.  */
  normal_path = decode_env_path ("", normal);
  for (; !NULL (normal_path); normal_path = XCONS (normal_path)->cdr)
    {
      Lisp_Object dirfile;
      dirfile = Fcar (normal_path);
      if (!NULL (dirfile))
	{
	  dirfile = Fdirectory_file_name (dirfile);
	  if (access (XSTRING (dirfile)->data, 0) < 0)
	    printf ("Warning: lisp library (%s) does not exist.\n",
		    XSTRING (Fcar (normal_path))->data);
	}
    }

  Vvalues = Qnil;

  Vload_path = decode_env_path ("EMACSLOADPATH", normal);
#ifndef CANNOT_DUMP
  if (!NULL (Vpurify_flag))
    Vload_path = Fcons (build_string ("../lisp"), Vload_path);
#endif /* not CANNOT_DUMP */
  load_in_progress = 0;
}

void
syms_of_read ()
{
  defsubr (&Sread);
  defsubr (&Sread_from_string);
  defsubr (&Sintern);
  defsubr (&Sintern_soft);
  defsubr (&Sload);
  defsubr (&Seval_current_buffer);
  defsubr (&Seval_region);
  defsubr (&Sread_char);
  defsubr (&Sget_file_char);
  defsubr (&Smapatoms);

  DEFVAR_LISP ("obarray", &Vobarray,
    "Symbol table for use by  intern  and  read.\n\
It is a vector whose length ought to be prime for best results.\n\
Each element is a list of all interned symbols whose names hash in that bucket.");

  DEFVAR_LISP ("values", &Vvalues,
    "List of values of all expressions which were read, evaluated and printed.\n\
Order is reverse chronological.");

  DEFVAR_LISP ("standard-input", &Vstandard_input,
    "Stream for read to get input from.\n\
See documentation of read for possible values.");
  Vstandard_input = Qt;

  DEFVAR_LISP ("load-path", &Vload_path,
    "*List of directories to search for files to load.\n\
Each element is a string (directory name) or nil (try default directory).\n\
Initialized based on EMACSLOADPATH environment variable, if any,\n\
otherwise to default specified in by file paths.h when emacs was built.");

  DEFVAR_BOOL ("load-in-progress", &load_in_progress,
    "Non-nil iff inside of  load.");

  Qstandard_input = intern ("standard-input");
  staticpro (&Qstandard_input);

  Qread_char = intern ("read-char");
  staticpro (&Qread_char);

  Qget_file_char = intern ("get-file-char");
  staticpro (&Qget_file_char);

  unrch = -1;
}
