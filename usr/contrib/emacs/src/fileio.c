/* File IO for GNU Emacs.
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


#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>
#include <ctype.h>
#include <sys/dir.h>
#include <errno.h>
#undef NULL
#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "window.h"

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* Nonzero during writing of auto-save files */
int auto_saving;

/* Nonzero means, when reading a filename in the minibuffer,
 start out by inserting the default directory into the minibuffer. */
int insert_default_directory;

Lisp_Object Qfile_error, Qfile_already_exists;

report_file_error (string, data)
     char *string;
     Lisp_Object data;
{
  Lisp_Object errstring;
  extern char *sys_errlist[];
  extern int errno;
  extern int sys_nerr;

  if (errno < sys_nerr)
    errstring = build_string (sys_errlist[errno]);
  else
    errstring = build_string ("undocumented error code");

  /* System error messages are capitalized.  Downcase the initial. */
  if (XSTRING (errstring)->data[0] >= 'A' &&
      XSTRING (errstring)->data[0] <= 'Z')
    XSTRING (errstring)->data[0] += 040;

  while (1)
    Fsignal (Qfile_error,
	     Fcons (build_string (string), Fcons (errstring, data)));
}

DEFUN ("file-name-directory", Ffile_name_directory, Sfile_name_directory,
  1, 1, 0,
  "Return the directory component in file name NAME.\n\
Return nil if NAME does not include a directory.\n\
Otherwise return a string ending in a slash.")
  (file)
     Lisp_Object file;
{
  register unsigned char *beg;
  register unsigned char *p;

  CHECK_STRING (file, 0);

  beg = XSTRING (file)->data;
  p = beg + XSTRING (file)->size;

  while (p != beg && p[-1] != '/') p--;

  if (p == beg)
    return Qnil;
  return make_string (beg, p - beg);
}

DEFUN ("file-name-nondirectory", Ffile_name_nondirectory, Sfile_name_nondirectory,
  1, 1, 0,
  "Return file name NAME sans its directory.\n\
This is everything after the last slash in NAME, if NAME contains a slash.")
  (file)
     Lisp_Object file;
{
  register unsigned char *beg, *p, *end;

  CHECK_STRING (file, 0);

  beg = XSTRING (file)->data;
  end = p = beg + XSTRING (file)->size;

  while (p != beg && p[-1] != '/') p--;

  return make_string (p, end - p);
}

DEFUN ("make-temp-name", Fmake_temp_name, Smake_temp_name, 1, 1, 0,
  "Generate temporary name (string) starting with PREFIX (a string).")
  (prefix)
     Lisp_Object prefix;
{
  Lisp_Object val;
  val = concat2 (prefix, build_string ("XXXXXX"));
  mktemp (XSTRING (val)->data);
  return val;
}

DEFUN ("expand-file-name", Fexpand_file_name, Sexpand_file_name, 1, 2, 0,
  "Convert FILENAME to absolute, and canonicalize it.\n\
Second arg DEFAULT is directory to start with if FILENAME is relative\n\
 (does not start with slash); if DEFAULT is nil or missing,\n\
the current buffer's value of default-directory is used.\n\
Filenames containing . or .. as components are simplified;\n\
initial ~ is expanded.  See also the function  substitute-in-file-name.")
     (name, defalt)
     Lisp_Object name, defalt;
{
  unsigned char *nm;
  
  register unsigned char *newdir, *p, *o;
  int tlen;
  unsigned char *target;
  struct passwd *pw;
  int lose;
  
  CHECK_STRING (name, 0);
  
  nm = XSTRING (name)->data;
  
  /* If nm is absolute, flush ...// and detect /./ and /../.
     If no /./ or /../ we can return right away. */
  if (nm[0] == '/')
    {
      p = nm;
      lose = 0;
      while (*p)
	{
	  if (p[0] == '/' && p[1] == '/'
#ifdef APOLLO
	      /* // at start of filename is meaningful on Apollo system */
	      && nm != p
#endif /* APOLLO */
	      )
	    nm = p + 1;
	  if (p[0] == '/' && p[1] == '~')
	    nm = p + 1, lose = 1;
	  if (p[0] == '/' && p[1] == '.'
	      && (p[2] == '/' || p[2] == 0
		  || (p[2] == '.' && (p[3] == '/' || p[3] == 0))))
	    lose = 1;
	  p++;
	}
      if (!lose)
	{
	  if (nm == XSTRING (name)->data)
	    return name;
	  return build_string (nm);
	}
    }

  /* Now determine directory to start with and put it in newdir */

  newdir = 0;

  if (nm[0] == '~')		/* prefix ~ */
    if (nm[1] == '/' || nm[1] == 0)/* ~/filename */
      {
	if (!(newdir = (unsigned char *) getenv ("HOME")))
	  newdir = (unsigned char *) "";
	nm++;
      }
    else			/* ~user/filename */
      {
	for (p = nm; *p && *p != '/'; p++);
	o = (unsigned char *) alloca (p - nm + 1);
	bcopy ((char *) nm, o, p - nm);
	o [p - nm] = 0;

	pw = (struct passwd *) getpwnam (o + 1);
	if (!pw)
	  error ("\"%s\" isn't a registered user", o + 1);
	
	nm = p;
	newdir = (unsigned char *) pw -> pw_dir;
      }

  if (nm[0] != '/' && !newdir)
    {
      if (NULL (defalt))
	defalt = bf_cur->directory;
      CHECK_STRING (defalt, 1);
      newdir = XSTRING (defalt)->data;
    }

  /* Now concatenate the directory and name to new space in thestack frame */

  tlen = (newdir ? strlen (newdir) + 1 : 0) + strlen (nm) + 1;
  target = (unsigned char *) alloca (tlen);
  *target = 0;

  if (newdir)
    {
      strcpy (target, newdir);

      /* Make sure there is a slash to separate directory from following */
      if (target[strlen (target) - 1] != '/' && nm[0] != '/' && nm[0])
	strcat (target, "/");
    }

  strcat (target, nm);

  /* Now canonicalize by removing /. and /foo/.. if they appear */

  p = target;
  o = target;

  while (*p)
    {
      if (*p != '/')
 	{
	  *o++ = *p++;
	}
      else if (!strncmp (p, "//", 2)
#ifdef APOLLO
	       /* // at start of filename is meaningful in Apollo system */
	       && o != target
#endif /* APOLLO */
	       )
	{
	  o = target;
	  p++;
	}
      else if (p[0] == '/' && p[1] == '.' &&
	       (p[2] == '/' || p[2] == 0))
	p += 2;
      else if (!strncmp (p, "/..", 3)
	       && (p[3] == '/' || p[3] == 0))
	{
	  while (o != target && *--o != '/');
	  p += 3;
	}
      else
 	{
	  *o++ = *p++;
	}
    }

  return make_string (target, o - target);
}

DEFUN ("substitute-in-file-name", Fsubstitute_in_file_name,
  Ssubstitute_in_file_name, 1, 1, 0,
  "Substitute environment variables referred to in STRING.\n\
A $ begins a request to substitute; the env variable name is\n\
the alphanumeric characters after the $, or else is surrounded by braces.\n\
If a ~ appears following a /, everything through that / is discarded.")
  (string)
     Lisp_Object string;
{
  unsigned char *nm;

  register unsigned char *s, *p, *o, *x, *endp;
  unsigned char *target;
  int total = 0;
  unsigned char *xnm;

  CHECK_STRING (string, 0);

  nm = XSTRING (string)->data;
  endp = nm + XSTRING (string)->size;

  /* If /~ or // appears, discard everything through first slash. */

  for (p = nm; p != endp; p++)
    {
      if ((p[0] == '~' ||
#ifdef APOLLO
	   /* // at start of file name is meaningful in Apollo system */
	   (p[0] == '/' && p - 1 != nm)
#else /* not APOLLO */
	   p[0] == '/'
#endif /* not APOLLO */
	   )
	  && p != nm && p[-1] == '/')
	{
	  nm = p;
	  total = 1;
	}
    }

  /* See if any variables are substituted into the string
     and find the total length of their values in `total' */

  for (p = nm; p != endp;)
    if (*p == '$')
      {
	p++;
	if (p == endp) goto badsubst;
	if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p;
	  }
	else
	  {
	    o = p;
	    while (p != endp && isalnum(*p)) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (unsigned char *) alloca (s - o + 1);
	strncpy (target, o, s - o);
	target[s - o] = 0;

	/* Get variable value */
	o = (unsigned char *) getenv (target);
/* The presence of this code makes vax 5.0 crash, for reasons yet unknown */
#if 0
#ifdef USG
	if (!o && !strcmp (target, "USER"))
	  o = (unsigned char *) getenv ("LOGNAME");
#endif /* USG */
#endif /* 0 */
	if (!o)
	  goto badvar;
	total += strlen (o);
      }
    else p++;

  if (!total)
    return string;

  /* If substitution required, recopy the string and do it */
  /* Make space in stack frame for the new copy */
  xnm = (unsigned char *) alloca (XSTRING (string)->size + total + 1);
  x = xnm;

  /* Copy the rest of the name through, replacing $ constructs with values */
  for (p = nm; *p;)
    if (*p == '$')
      {
	p++;
	if (p == endp) goto badsubst;
	if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p++;
	  }
	else
	  {
	    o = p;
	    while (p != endp && isalnum(*p)) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (unsigned char *) alloca (s - o + 1);
	strncpy (target, o, s - o);
	target[s - o] = 0;

	/* Get variable value */
	o = (unsigned char *) getenv (target);
/* The presence of this code makes vax 5.0 crash, for reasons yet unknown */
#if 0
#ifdef USG
	if (!o && !strcmp (target, "USER"))
	  o = (unsigned char *) getenv ("LOGNAME");
#endif /* USG */
#endif /* 0 */
	if (!o)
	  goto badvar;

	strcpy (x, o);
	x += strlen (o);
      }
  else
    *x++ = *p++;

  *x = 0;

  /* If /~ or // appears, discard everything through first slash. */

  for (p = xnm; p != x; p++)
    if ((p[0] == '~' ||
#ifdef APOLLO
	 /* // at start of file name is meaningful in Apollo system */
	 (p[0] == '/' && p - 1 != xnm)
#else /* not APOLLO */
	 p[0] == '/'
#endif /* not APOLLO */
	 )
	&& p != nm && p[-1] == '/')
      xnm = p;

  return make_string (xnm, x - xnm);

 badsubst:
  error ("Bad format environment-variable substitution");
 missingclose:
  error ("Missing \"}\" in environment-variable substitution");
 badvar:
  error ("Substituting nonexistent environment variable %s", target);

  /* NOTREACHED */
}

barf_or_query_if_file_exists (absname, querystring)
     Lisp_Object absname;
     unsigned char *querystring;
{
  Lisp_Object tem;
  if (access (XSTRING (absname)->data, 4) >= 0)
    if (!(Finteractive_p ()) ||
	(tem = Fyes_or_no_p
	         (format1 ("File %s already exists; %s anyway? ",
			   XSTRING (absname)->data, querystring)),
	 NULL (tem)))
      Fsignal (Qfile_already_exists, Fcons (absname, Qnil));
  return;
}

DEFUN ("copy-file", Fcopy_file, Scopy_file, 2, 3,
  "fCopy file: \nFCopy %s to file: ",
  "Copy FILE to NEWNAME.  Both args strings.\n\
Signals a  file-already-exists  error if NEWNAME already exists,\n\
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.")
  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
  int ifd, ofd, n;
  char buf[2048];
  struct stat st;

  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);
  if (NULL (ok_if_already_exists))
      barf_or_query_if_file_exists (newname, "copy to it");

  ifd = open (XSTRING (filename)->data, 0);
  if (ifd < 0)
    report_file_error ("Opening input file", Fcons (filename, Qnil));

  ofd = creat (XSTRING (newname)->data, 0666);
  if (ofd < 0)
    {
      close (ifd);
      report_file_error ("Opening output file", Fcons (newname, Qnil));
    }

  while ((n = read (ifd, buf, sizeof buf)) > 0)
    write (ofd, buf, n);

  if (fstat (ifd, &st) >= 0)
#if defined (BSD) && !defined (BSD4_1)
    fchmod (ofd, st.st_mode & 07777);
#else
    chmod (XSTRING (newname)->data, st.st_mode & 07777);
#endif

  close (ifd);
  close (ofd);
  return Qnil;
}

DEFUN ("delete-file", Fdelete_file, Sdelete_file, 1, 1, "fDelete file: ",
  "Delete specified file.  One argument, a file name string.\n\
If file has multiple names, it continues to exist with the other names.")
  (filename)
     Lisp_Object filename;
{
  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);
  if (0 > unlink (XSTRING (filename)->data))
    report_file_error ("Removing old name", Flist (1, &filename));
  return Qnil;
}

DEFUN ("rename-file", Frename_file, Srename_file, 2, 3,
  "fRename file: \nFRename %s to file: ",
  "Rename FILE as NEWNAME.  Both args strings.\n\
If file has names other than FILE, it continues to have those names.\n\
Signals a  file-already-exists  error if NEWNAME already exists\n\
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.")
  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
  extern int errno; 
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif

  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);
  if (NULL (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "rename to it");
#ifndef BSD4_1
  if (0 > rename (XSTRING (filename)->data, XSTRING (newname)->data))
#else
  if (0 > link (XSTRING (filename)->data, XSTRING (newname)->data)
      || 0 > unlink (XSTRING (filename)->data))
#endif
    {
      if (errno == EXDEV)
	{
	  Fcopy_file (filename, newname, ok_if_already_exists);
	  Fdelete_file (filename);
	}
      else
#ifdef NO_ARG_ARRAY
	{
	  args[0] = filename;
	  args[1] = newname;
	  report_file_error ("Renaming", Flist (2, args));
	}
#else
	report_file_error ("Renaming", Flist (2, &filename));
#endif
    }
  return Qnil;
}

DEFUN ("add-name-to-file", Fadd_name_to_file, Sadd_name_to_file, 2, 3,
  "fAdd name to file: \nFName to add to %s: ",
  "Give FILE additional name NEWNAME.  Both args strings.\n\
Signals a  file-already-exists  error if NEWNAME already exists\n\
unlesss optional third argument OK-IF-ALREADY-EXISTS is non-nil.")
  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif

  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);
  if (NULL (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "make it a new name");
  unlink (XSTRING (newname)->data);
  if (0 > link (XSTRING (filename)->data, XSTRING (newname)->data))
    {
#ifdef NO_ARG_ARRAY
      args[0] = filename;
      args[1] = newname;
      report_file_error ("Adding new name", Flist (2, args));
#else
      report_file_error ("Adding new name", Flist (2, &filename));
#endif
    }

  return Qnil;
}

#ifdef S_IFLNK
DEFUN ("make-symbolic-link", Fmake_symbolic_link, Smake_symbolic_link, 2, 3,
  "FMake symbolic link to file: \nFMake symbolic link to file %s: ",
  "Make a symbolic link to FILENAME, named LINKNAME.  Both args strings.\n\
Signals a  file-already-exists  error if NEWNAME already exists\n\
unlesss optional third argument OK-IF-ALREADY-EXISTS is non-nil.")

  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif

  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);
  if (NULL (ok_if_already_exists))
    barf_or_query_if_file_exists (newname, "make it a link");
  if (0 > symlink (XSTRING (filename)->data, XSTRING (newname)->data))
    {
#ifdef NO_ARG_ARRAY
      args[0] = filename;
      args[1] = newname;
      report_file_error ("Making symbolic link", Flist (2, args));
#else
      report_file_error ("Making symbolic link", Flist (2, &filename));
#endif
    }
  return Qnil;
}
#endif /* S_IFLNK */

DEFUN ("file-exists-p", Ffile_exists_p, Sfile_exists_p, 1, 1, 0,
  "Return t if file FILENAME exists and you can read it.\n\
Use file-attributes to check for existence not caring about readability.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);
  return (access (XSTRING (abspath)->data, 4) >= 0) ? Qt : Qnil;
}

DEFUN ("file-writable-p", Ffile_writable_p, Sfile_writable_p, 1, 1, 0,
  "Return t if file FILENAME can be written or created by you.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath, dir;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);
  if (access (XSTRING (abspath)->data, 0) >= 0)
    return (access (XSTRING (abspath)->data, 2) >= 0) ? Qt : Qnil;
  dir = Ffile_name_directory (abspath);
  return (access (!NULL (dir) ? (char *) XSTRING (dir)->data : "", 2) >= 0
	  ? Qt : Qnil);
}

DEFUN ("file-symlink-p", Ffile_symlink_p, Sfile_symlink_p, 1, 1, 0,
  "If file FILENAME is the name of a symbolic link\n\
returns the name of the file to which it is linked.\n\
Otherwise returns NIL.")
  (filename)
     Lisp_Object filename;
{
#ifdef S_IFLNK
  char *buf;
  int bufsize;
  int valsize;
  Lisp_Object val;

  CHECK_STRING (filename, 0);

  bufsize = 100;
  while (1)
    {
      buf = (char *) xmalloc (bufsize);
      bzero (buf, bufsize);
      valsize = readlink (XSTRING (filename)->data, buf, bufsize);
      if (valsize < bufsize) break;
      /* Buffer was not long enough */
      free (buf);
      bufsize *= 2;
    }
  if (valsize == -1)
    {
      free (buf);
      return Qnil;
    }
  val = make_string (buf, valsize);
  free (buf);
  return val;
#else /* not S_IFLNK */
  return Qnil;
#endif /* not S_IFLNK */
}

DEFUN ("file-directory-p", Ffile_directory_p, Sfile_directory_p, 1, 1, 0,
  "Return t if file FILENAME is the name of a directory.")
  (filename)
     Lisp_Object filename;
{
  register Lisp_Object abspath;
  struct stat st;

  abspath = Fexpand_file_name (filename, bf_cur->directory);
  /* Remove final slash, if any (unless path is root).
     stat behaves differently depending!  */
  if (XSTRING (abspath)->size > 1
      && XSTRING (abspath)->data[XSTRING (abspath)->size - 1] == '/')
    {
      if (EQ (abspath, filename))
	abspath = Fcopy_sequence (abspath);
      XSTRING (abspath)->data[XSTRING (abspath)->size - 1] = 0;
    }

  if (stat (XSTRING (abspath)->data, &st) < 0)
    return Qnil;
  return (st.st_mode & S_IFMT) == S_IFDIR ? Qt : Qnil;
}

DEFUN ("file-modes", Ffile_modes, Sfile_modes, 1, 1, 0,
  "Return mode bits of FILE, as an integer.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath;
  struct stat st;

  abspath = Fexpand_file_name (filename, bf_cur->directory);

  /* Remove final slash, if any (unless path is the root).
     stat behaves differently depending!  */
  if (XSTRING (abspath)->size > 1
      && XSTRING (abspath)->data[XSTRING (abspath)->size - 1] == '/')
    {
      if (EQ (abspath, filename))
	abspath = Fcopy_sequence (abspath);
      XSTRING (abspath)->data[XSTRING (abspath)->size - 1] = 0;
    }

  if (stat (XSTRING (abspath)->data, &st) < 0)
    return Qnil;
  return make_number (st.st_mode & 07777);
}

DEFUN ("set-file-modes", Fset_file_modes, Sset_file_modes, 2, 2, 0,
  "Set mode bits of FILE to MODE (an integer).\n\
Only the 12 low bits of MODE are used.")
  (filename, mode)
     Lisp_Object filename, mode;
{
  Lisp_Object abspath;

  abspath = Fexpand_file_name (filename, bf_cur->directory);
  CHECK_NUMBER (mode, 1);
  if (chmod (XSTRING (abspath)->data, XINT (mode)) < 0)
    report_file_error ("Doing chmod", Fcons (abspath, Qnil));
  return Qnil;
}

close_file_unwind (fd)
     Lisp_Object fd;
{
  close (XFASTINT (fd));
}

DEFUN ("insert-file-contents", Finsert_file_contents, Sinsert_file_contents,
  1, 2, 0,
  "Insert contents of file FILENAME after point.\n\
Returns list of absolute pathname and length of data inserted.\n\
If second argument VISIT is non-nil, the buffer's\n\
visited filename and last save file modtime are set,\n\
and it is marked unmodified.")
  (filename, visit)
     Lisp_Object filename, visit;
{
  struct stat st;
  register int fd;
  register int n, i;
  int count = specpdl_ptr - specpdl;

  if (!NULL (bf_cur->read_only))
    Fbarf_if_buffer_read_only();

  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);

  if (stat (XSTRING (filename)->data, &st) < 0
	|| (fd = open (XSTRING (filename)->data, 0)) < 0)
    report_file_error ("Opening input file", Fcons (filename, Qnil));

  record_unwind_protect (close_file_unwind, make_number (fd));

  if (NULL (visit))
    prepare_to_modify_buffer ();

  RecordInsert (point, st.st_size);
  bf_modified++;

  GapTo (point);
  if (bf_gap < st.st_size)
    make_gap (st.st_size);
    
  n = 0;
  while ((i = read (fd, bf_p1 + bf_s1 + 1, st.st_size - n)) > 0)
    {
      bf_s1 += i;
      bf_gap -= i;
      bf_p2 -= i;
      n += i;
    }

  if (!NULL (visit))
    DoneIsDone ();

  close (fd);
  /* Discard the unwind protect */
  specpdl_ptr = specpdl + count;

  if (i < 0)
    error ("IO error reading %s", XSTRING (filename)->data);

  if (!NULL (visit))
    {
      bf_cur->modtime = st.st_mtime;
      bf_cur->save_modified = bf_modified;
      bf_cur->auto_save_modified = bf_modified;
      XFASTINT (bf_cur->save_length) = NumCharacters;
#ifdef CLASH_DETECTION
      if (!NULL (bf_cur->filename))
	unlock_file (bf_cur->filename);
      unlock_file (filename);
#endif /* CLASH_DETECTION */
      bf_cur->filename = filename;
    }

  return Fcons (filename, Fcons (make_number (st.st_size), Qnil));
}

DEFUN ("write-region", Fwrite_region, Swrite_region, 3, 5,
  "r\nFWrite region to file: ",
  "Write current region into specified file.\n\
When called from a program, takes three arguments:\n\
START, END and FILENAME.  START and END are buffer positions.\n\
Optional fourth argument APPEND if non-nil means\n\
  append to existing file contents (if any).\n\
Optional fifth argument VISIT if t means\n\
  set last-save-file-modtime of buffer to this file's modtime\n\
  and mark buffer not modified.\n\
If VISIT is neither t nor nil, it means do not print\n\
  the \"Wrote file\" message.")
  (start, end, filename, append, visit)
     Lisp_Object start, end, filename, append, visit;
{
  register int fd;
  int failure = 0;
  unsigned char *fn;
  struct stat st;
  int tem;
  int count = specpdl_ptr - specpdl;

  /* Special kludge to simplify auto-saving */
  if (NULL (start))
    {
      XFASTINT (start) = 1;
      XFASTINT (end) = 1 + bf_s1 + bf_s2;
    }
  else
    validate_region (&start, &end);

  filename = Fexpand_file_name (filename, Qnil);
  fn = XSTRING (filename)->data;
  
#ifdef CLASH_DETECTION
  if (!auto_saving)
    lock_file (filename);
#endif /* CLASH_DETECTION */

  fd = -1;
  if (!NULL (append))
    fd = open (fn, 1);

  if (fd < 0)
    fd = creat (fn, 0666);
  
  if (fd < 0)
    {
#ifdef CLASH_DETECTION
      if (!auto_saving) unlock_file (filename);
#endif /* CLASH_DETECTION */
      report_file_error ("Opening output file", Fcons (filename, Qnil));
    }

  record_unwind_protect (close_file_unwind, make_number (fd));

  if (!NULL (append))
    if (lseek (fd, 0, 2) < 0)
      {
#ifdef CLASH_DETECTION
	if (!auto_saving) unlock_file (filename);
#endif /* CLASH_DETECTION */
	report_file_error ("Lseek error", Fcons (filename, Qnil));
      }

  failure = 0;
  if (XINT (start) != XINT (end))
    {
      if (XINT (start) - 1 < bf_s1)
	failure = 0 > e_write (fd, &CharAt (XINT (start)),
			       min (bf_s1 + 1, XINT (end)) - XINT (start));

      if (XINT (end) - 1 > bf_s1 && !failure)
	{
	  tem = max (XINT (start), bf_s1 + 1);
	  failure = 0 > e_write (fd, &CharAt (tem), XINT (end) - tem);
	}
    }

  fstat (fd, &st);
  close (fd);
  /* Discard the unwind protect */
  specpdl_ptr = specpdl + count;

#ifdef CLASH_DETECTION
  if (!auto_saving)
    unlock_file (filename);
#endif /* CLASH_DETECTION */

  if (failure)
    error ("IO error writing %s", fn);

  if (EQ (visit, Qt))
    {
      bf_cur->modtime = st.st_mtime;
      bf_cur->save_modified = bf_modified;
      XFASTINT (bf_cur->save_length) = NumCharacters;
      bf_cur->filename = filename;
    }
  else if (!NULL (visit))
    return Qnil;

  if (!auto_saving)
    message ("Wrote %s", fn);

  return Qnil;
}

int
e_write (fd, addr, len)
     int fd;
     register char *addr;
     register int len;
{
  char buf[1024];
  register char *p, *end;

  if (!EQ (bf_cur->selective_display, Qt))
    return write (fd, addr, len) - len;
  else
    {
      p = buf;
      end = p + sizeof buf;
      while (len--)
	{
	  if (p == end)
	    {
	      if (write (fd, buf, sizeof buf) != sizeof buf)
		return -1;
	      p = buf;
	    }
	  *p = *addr++;
	  if (*p++ == '\015')
	    p[-1] = '\n';
	}
      if (p != buf)
	if (write (fd, buf, p - buf) != p - buf)
	  return -1;
    }
  return 0;
}

DEFUN ("verify-visited-file-modtime", Fverify_visited_file_modtime,
  Sverify_visited_file_modtime, 1, 1, 0,
  "Return t if last mod time of BUF's visited file matches what BUF records.\n\
This means that the file has not been changed since it was visited or saved.")
  (buf)
     Lisp_Object buf;
{
  struct buffer *b = XBUFFER (buf);
  struct stat st;

  CHECK_BUFFER (buf, 0);

  if (XTYPE (b->filename) != Lisp_String) return Qt;
  if (!b->modtime) return Qt;
  if (stat (XSTRING (b->filename)->data, &st) < 0)
    return Qnil;
  if (st.st_mtime != b->modtime)
    return Qnil;
  return Qt;
}

DEFUN ("clear-visited-file-modtime", Fclear_visited_file_modtime,
  Sclear_visited_file_modtime, 0, 0, 0,
  "Clear out records of last mod time of visited file.\n\
Next attempt to save will certainly not complain of a discrepancy.")
  ()
{
  bf_cur->modtime = 0;
  return Qnil;
}

Lisp_Object
auto_save_error ()
{
  return Qnil;
}

Lisp_Object
auto_save_1 ()
{
  return
    Fwrite_region (Qnil, Qnil,
		   bf_cur->auto_save_file_name,
		   Qnil, Qlambda);
}

DEFUN ("do-auto-save", Fdo_auto_save, Sdo_auto_save, 0, 1, "",
  "Auto-save all buffers that need it.\n\
This is all buffers that have auto-saving enabled\n\
and are changed since last auto-saved.\n\
Auto-saving writes the buffer into a file\n\
so that your editing is not lost if the system crashes.\n\
This file is not the file you visited; that changes only when you save.\n\n\
Non-nil argument means do not print any message.")
  (nomsg)
     Lisp_Object nomsg;
{
  struct buffer *old = bf_cur, *b;
  Lisp_Object tail, buf;
  int auto_saved = 0;
  char *omessage = minibuf_message;
  extern MinibufDepth;

  auto_saving = 1;
  if (MinibufDepth)
    nomsg = Qt;

  bf_cur->text = bf_text;

  for (tail = Vbuffer_alist; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      buf = XCONS (XCONS (tail)->car)->cdr;
      b = XBUFFER (buf);
      /* Check for auto save enabled
	 and file changed since last auto save
	 and file changed since last real save.  */
      if (XTYPE (b->auto_save_file_name) == Lisp_String
	  && b->save_modified < b->text.modified
	  && b->auto_save_modified < b->text.modified)
	{
	  if (XFASTINT (b->save_length) * 10
	      > (b->text.size1 + b->text.size2) * 13)
	    {
	      /* It has shrunk too much; don't chckpoint. */
		/*** Should report this to user somehow ***/
	      continue;
	    }
	  SetBfp (b);
	  if (!auto_saved && NULL (nomsg))
	    message1 ("Auto-saving...");
	  internal_condition_case (auto_save_1, Qt, auto_save_error);
	  auto_saved++;
	  b->auto_save_modified = b->text.modified;
	  XFASTINT (bf_cur->save_length) = NumCharacters;
	  SetBfp (old);
	}
    }

  if (auto_saved && NULL (nomsg))
    message1 (omessage ? omessage : "Auto-saving...done");

  auto_saving = 0;
  return Qnil;
}

DEFUN ("set-buffer-auto-saved", Fset_buffer_auto_saved,
  Sset_buffer_auto_saved, 0, 0, 0,
  "Mark current buffer as auto-saved with its current text.\n\
No auto-save file will be written until the buffer changes again.")
  ()
{
  bf_cur->auto_save_modified = bf_modified;
  return Qnil;
}

DEFUN ("recent-auto-save-p", Frecent_auto_save_p, Srecent_auto_save_p,
  0, 0, 0,
  "Return t if buffer has been auto-saved since last read in or saved.")
  ()
{
  return (bf_cur->save_modified < bf_cur->auto_save_modified) ? Qt : Qnil;
}

/* Reading and completing file names */
extern Lisp_Object Ffile_name_completion (), Ffile_name_all_completions ();

DEFUN ("read-file-name-internal", Fread_file_name_internal, Sread_file_name_internal,
  3, 3, 0,
  "Internal subroutine for read-file-name.  Do not call this.")
  (string, dir, action)
     Lisp_Object string, dir, action;
  /* action is nil for complete, t for return list of completions,
     lambda for verify final value */
{
  Lisp_Object name, specdir, realdir, val;
  if (XSTRING (string)->size == 0)
    {
      name = string;
      realdir = dir;
      if (EQ (action, Qlambda))
	return Qnil;
    }
  else
    {
      string = Fsubstitute_in_file_name (string);
      name = Ffile_name_nondirectory (string);
      realdir = Ffile_name_directory (string);
      if (NULL (realdir))
	realdir = dir;
      else
	realdir = Fexpand_file_name (realdir, dir);
    }

  if (NULL (action))
    {
      specdir = Ffile_name_directory (string);
      val = Ffile_name_completion (name, realdir);
      if (XTYPE (val) == Lisp_String && !NULL (specdir))
	return concat2 (specdir, val);
      return val;
    }
  if (EQ (action, Qt))
    return Ffile_name_all_completions (name, realdir);
  /* Only other case actually used is ACTION = lambda */
  return Ffile_exists_p (string);
}

DEFUN ("read-file-name", Fread_file_name, Sread_file_name, 1, 4, 0,
  "Read file name, prompting with PROMPT and completing in directory DIR.\n\
Value is not expanded!  You must call expand-file-name yourself.\n\
Default name to DEFAULT if user enters a null string.\n\
Fourth arg MUSTMATCH non-nil means require existing file's name.\n\
 Non-nil and non-t means also require confirmation after completion.\n\
DIR defaults to current buffer's directory default.")
  (prompt, dir, defalt, mustmatch)
     Lisp_Object prompt, dir, defalt, mustmatch;
{
  Lisp_Object val, insdef, tem;
  struct gcpro gcpro1, gcpro2;
  register char *homedir;

  if (NULL (dir))
    dir = bf_cur->directory;
  if (NULL (defalt))
    defalt = bf_cur->filename;

  /* If dir starts with user's homedir, change that to ~. */
  homedir = (char *) getenv ("HOME");
  if (XTYPE (dir) == Lisp_String
      && !strncmp (homedir, XSTRING (dir)->data, strlen (homedir))
      && XSTRING (dir)->data[strlen (homedir)] == '/')
    {
      dir = make_string (XSTRING (dir)->data + strlen (homedir) - 1,
			 XSTRING (dir)->size - strlen (homedir) + 1);
      XSTRING (dir)->data[0] = '~';
    }

  if (insert_default_directory)
    insdef = dir;
  else
    insdef = build_string ("");

  GCPRO2 (insdef, defalt);
  val = Fcompleting_read (prompt, intern ("read-file-name-internal"),
			  dir, mustmatch,
			  insert_default_directory ? insdef : Qnil);
  UNGCPRO;
  if (NULL (val))
    error ("No file name specified");
  tem = Fstring_equal (val, insdef);
  if (!NULL (tem) && !NULL (defalt))
    return defalt;
  return Fsubstitute_in_file_name (val);
}

syms_of_fileio ()
{
  Qfile_error = intern ("file-error");
  staticpro (&Qfile_error);
  Qfile_already_exists = intern("file-already-exists");
  staticpro (&Qfile_already_exists);

  Fput (Qfile_error, Qerror_conditions,
	Fcons (Qfile_error, Fcons (Qerror, Qnil)));
  Fput (Qfile_error, Qerror_message,
	build_string ("File error"));

  Fput (Qfile_already_exists, Qerror_conditions,
	Fcons (Qfile_already_exists,
	       Fcons (Qfile_error, Fcons (Qerror, Qnil))));
  Fput (Qfile_already_exists, Qerror_message,
	build_string ("File already exists"));

  DefBoolVar ("insert-default-directory", &insert_default_directory,
    "*Non-nil means when reading a filename start with default dir in minibuffer.");
  insert_default_directory = 1;

  defsubr (&Sfile_name_directory);
  defsubr (&Sfile_name_nondirectory);
  defsubr (&Smake_temp_name);
  defsubr (&Sexpand_file_name);
  defsubr (&Ssubstitute_in_file_name);
  defsubr (&Scopy_file);
  defsubr (&Sdelete_file);
  defsubr (&Srename_file);
  defsubr (&Sadd_name_to_file);
#ifdef S_IFLNK
  defsubr (&Smake_symbolic_link);
#endif /* S_IFLNK */
  defsubr (&Sfile_exists_p);
  defalias (&Sfile_exists_p, "file-readable-p");
  defsubr (&Sfile_writable_p);
  defsubr (&Sfile_symlink_p);
  defsubr (&Sfile_directory_p);
  defsubr (&Sfile_modes);
  defsubr (&Sset_file_modes);
  defsubr (&Sinsert_file_contents);
  defsubr (&Swrite_region);
  defsubr (&Sverify_visited_file_modtime);
  defsubr (&Sclear_visited_file_modtime);
  defsubr (&Sdo_auto_save);
  defsubr (&Sset_buffer_auto_saved);
  defsubr (&Srecent_auto_save_p);

  defsubr (&Sread_file_name_internal);
  defsubr (&Sread_file_name);
}
