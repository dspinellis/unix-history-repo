/* File IO for GNU Emacs.
   Copyright (C) 1985, 1986, 1987, 1988, 1990 Free Software Foundation, Inc.

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


#include <sys/types.h>
#ifdef hpux
/* needed by <pwd.h> */
#include <stdio.h>
#undef NULL
#endif
#include <sys/stat.h>
#include <pwd.h>
#include <ctype.h>
#include <sys/dir.h>
#include <errno.h>

#ifndef VMS
extern int errno;
extern char *sys_errlist[];
extern int sys_nerr;
#endif

#define err_str(a) ((a) < sys_nerr ? sys_errlist[a] : "unknown error")

#ifdef APOLLO
#include <sys/time.h>
#endif

#ifdef NULL
#undef NULL
#endif
#include "config.h"
#include "lisp.h"
#include "buffer.h"
#include "window.h"

#ifdef VMS
#include <perror.h>
#include <file.h>
#include <rmsdef.h>
#include <fab.h>
#include <nam.h>
#endif

#ifdef HAVE_TIMEVAL
#ifdef HPUX
#include <time.h>
#else
#include <sys/time.h>
#endif
#endif

#ifdef HPUX
#include <netio.h>
#include <errnet.h>
#endif

#ifndef O_WRONLY
#define O_WRONLY 1
#endif

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

/* Nonzero during writing of auto-save files */
int auto_saving;

/* Nonzero means, when reading a filename in the minibuffer,
 start out by inserting the default directory into the minibuffer. */
int insert_default_directory;

/* On VMS, nonzero means write new files with record format stmlf.
   Zero means use var format.  */
int vms_stmlf_recfm;

Lisp_Object Qfile_error, Qfile_already_exists;

report_file_error (string, data)
     char *string;
     Lisp_Object data;
{
  Lisp_Object errstring;

  if (errno >= 0 && errno < sys_nerr)
    errstring = build_string (sys_errlist[errno]);
  else
    errstring = build_string ("undocumented error code");

  /* System error messages are capitalized.  Downcase the initial. */
  XSTRING (errstring)->data[0] = DOWNCASE (XSTRING (errstring)->data[0]);

  while (1)
    Fsignal (Qfile_error,
	     Fcons (build_string (string), Fcons (errstring, data)));
}

DEFUN ("file-name-directory", Ffile_name_directory, Sfile_name_directory,
  1, 1, 0,
  "Return the directory component in file name NAME.\n\
Return nil if NAME does not include a directory.\n\
Otherwise returns a directory spec.\n\
Given a Unix syntax file name, returns a string ending in slash;\n\
on VMS, perhaps instead a string ending in :, ] or >.")
  (file)
     Lisp_Object file;
{
  register unsigned char *beg;
  register unsigned char *p;

  CHECK_STRING (file, 0);

  beg = XSTRING (file)->data;
  p = beg + XSTRING (file)->size;

  while (p != beg && p[-1] != '/'
#ifdef VMS
	 && p[-1] != ':' && p[-1] != ']' && p[-1] != '>'
#endif /* VMS */
	 ) p--;

  if (p == beg)
    return Qnil;
  return make_string (beg, p - beg);
}

DEFUN ("file-name-nondirectory", Ffile_name_nondirectory, Sfile_name_nondirectory,
  1, 1, 0,
  "Return file name NAME sans its directory.\n\
For example, in a Unix-syntax file name,\n\
this is everything after the last slash,\n\
or the entire name if it contains no slash.")
  (file)
     Lisp_Object file;
{
  register unsigned char *beg, *p, *end;

  CHECK_STRING (file, 0);

  beg = XSTRING (file)->data;
  end = p = beg + XSTRING (file)->size;

  while (p != beg && p[-1] != '/'
#ifdef VMS
	 && p[-1] != ':' && p[-1] != ']' && p[-1] != '>'
#endif /* VMS */
	 ) p--;

  return make_string (p, end - p);
}

char *
file_name_as_directory (out, in)
     char *out, *in;
{
  int size = strlen (in) - 1;

  strcpy (out, in);

#ifdef VMS
  /* Is it already a directory string? */
  if (in[size] == ':' || in[size] == ']' || in[size] == '>')
    return out;
  /* Is it a VMS directory file name?  If so, hack VMS syntax.  */
  else if (! index (in, '/')
	   && ((size > 3 && ! strcmp (&in[size - 3], ".DIR"))
	       || (size > 3 && ! strcmp (&in[size - 3], ".dir"))
	       || (size > 5 && (! strncmp (&in[size - 5], ".DIR", 4)
				|| ! strncmp (&in[size - 5], ".dir", 4))
		   && (in[size - 1] == '.' || in[size - 1] == ';')
		   && in[size] == '1')))
    {
      register char *p, *dot;
      char brack;

      /* x.dir -> [.x]
	 dir:x.dir --> dir:[x]
	 dir:[x]y.dir --> dir:[x.y] */
      p = in + size;
      while (p != in && *p != ':' && *p != '>' && *p != ']') p--;
      if (p != in)
	{
	  strncpy (out, in, p - in);
	  out[p - in] = '\0';
	  if (*p == ':')
	    {
	      brack = ']';
	      strcat (out, ":[");
	    }
	  else
	    {
	      brack = *p;
	      strcat (out, ".");
	    }
	  p++;
	}
      else
	{
	  brack = ']';
	  strcpy (out, "[.");
	}
      if (dot = index (p, '.'))
	{
	  /* blindly remove any extension */
	  size = strlen (out) + (dot - p);
	  strncat (out, p, dot - p);
	}
      else
	{
	  strcat (out, p);
	  size = strlen (out);
	}
      out[size++] = brack;
      out[size] = '\0';
    }
#else /* not VMS */
  /* For Unix syntax, Append a slash if necessary */
  if (out[size] != '/')
    strcat (out, "/");
#endif /* not VMS */
  return out;
}

DEFUN ("file-name-as-directory", Ffile_name_as_directory,
       Sfile_name_as_directory, 1, 1, 0,
  "Return a string representing file FILENAME interpreted as a directory.\n\
This string can be used as the value of default-directory\n\
or passed as second argument to expand-file-name.\n\
For a Unix-syntax file name, just appends a slash.\n\
On VMS, converts \"[X]FOO.DIR\" to \"[X.FOO]\", etc.")
  (file)
     Lisp_Object file;
{
  char *buf;

  CHECK_STRING (file, 0);
  if (NULL (file))
    return Qnil;
  buf = (char *) alloca (XSTRING (file)->size + 10);
  return build_string (file_name_as_directory (buf, XSTRING (file)->data));
}

/*
 * Convert from directory name to filename.
 * On VMS:
 *       xyzzy:[mukesh.emacs] => xyzzy:[mukesh]emacs.dir.1
 *       xyzzy:[mukesh] => xyzzy:[000000]mukesh.dir.1
 * On UNIX, it's simple: just make sure there is a terminating /

 * Value is nonzero if the string output is different from the input.
 */

directory_file_name (src, dst)
     char *src, *dst;
{
  long slen;
#ifdef VMS
  long rlen;
  char * ptr, * rptr;
  char bracket;
  struct FAB fab = cc$rms_fab;
  struct NAM nam = cc$rms_nam;
  char esa[NAM$C_MAXRSS];
#endif /* VMS */

  slen = strlen (src) - 1;
#ifdef VMS
  if (! index (src, '/')
      && (src[slen] == ']' || src[slen] == ':' || src[slen] == '>'))
    {
      /* VMS style - convert [x.y.z] to [x.y]z, [x] to [000000]x */
      fab.fab$l_fna = src;
      fab.fab$b_fns = slen + 1;
      fab.fab$l_nam = &nam;
      fab.fab$l_fop = FAB$M_NAM;

      nam.nam$l_esa = esa;
      nam.nam$b_ess = sizeof esa;
      nam.nam$b_nop |= NAM$M_SYNCHK;

      /* We call SYS$PARSE to handle such things as [--] for us. */
      if (SYS$PARSE(&fab, 0, 0) == RMS$_NORMAL)
	{
	  slen = nam.nam$b_esl - 1;
	  if (esa[slen] == ';' && esa[slen - 1] == '.')
	    slen -= 2;
	  esa[slen + 1] = '\0';
	  src = esa;
	}
      if (src[slen] != ']' && src[slen] != '>')
	{
	  /* what about when we have logical_name:???? */
	  if (src[slen] == ':')
	    {			/* Xlate logical name and see what we get */
	      ptr = strcpy (dst, src); /* upper case for getenv */
	      while (*ptr)
		{
		  if ('a' <= *ptr && *ptr <= 'z')
		    *ptr -= 040;
		  ptr++;
		}
	      dst[slen] = 0;	/* remove colon */
	      if (!(src = egetenv (dst)))
		return 0;
	      /* should we jump to the beginning of this procedure?
		 Good points: allows us to use logical names that xlate
		 to Unix names,
		 Bad points: can be a problem if we just translated to a device
		 name...
		 For now, I'll punt and always expect VMS names, and hope for
		 the best! */
	      slen = strlen (src) - 1;
	      if (src[slen] != ']' && src[slen] != '>')
		{ /* no recursion here! */
		  strcpy (dst, src);
		  return 0;
		}
	    }
	  else
	    {		/* not a directory spec */
	      strcpy (dst, src);
	      return 0;
	    }
	}
      bracket = src[slen];
      if (!(ptr = index (src, bracket - 2)))
	{ /* no opening bracket */
	  strcpy (dst, src);
	  return 0;
	}
      if (!(rptr = rindex (src, '.')))
	rptr = ptr;
      slen = rptr - src;
      strncpy (dst, src, slen);
      dst[slen] = '\0';
      if (*rptr == '.')
	{
	  dst[slen++] = bracket;
	  dst[slen] = '\0';
	}
      else
	{
	  /* If we have the top-level of a rooted directory (i.e. xx:[000000]),
	     then translate the device and recurse. */
	  if (dst[slen - 1] == ':'
	      && dst[slen - 2] != ':'	/* skip decnet nodes */
	      && strcmp(src + slen, "[000000]") == 0)
	    {
	      dst[slen - 1] = '\0';
	      if ((ptr = egetenv (dst))
		  && (rlen = strlen (ptr) - 1) > 0
		  && (ptr[rlen] == ']' || ptr[rlen] == '>')
		  && ptr[rlen - 1] == '.')
		{
		  ptr[rlen - 1] = ']';
		  ptr[rlen] = '\0';
		  return directory_file_name (ptr, dst);
		}
	      else
		dst[slen - 1] = ':';
	    }
	  strcat (dst, "[000000]");
	  slen += 8;
	}
      rptr++;
      rlen = strlen (rptr) - 1;
      strncat (dst, rptr, rlen);
      dst[slen + rlen] = '\0';
      strcat (dst, ".DIR.1");
      return 1;
    }
#endif /* VMS */
  /* Process as Unix format: just remove any final slash.
     But leave "/" unchanged; do not change it to "".  */
  strcpy (dst, src);
  if (dst[slen] == '/' && slen > 1)
    dst[slen] = 0;
  return 1;
}

DEFUN ("directory-file-name", Fdirectory_file_name, Sdirectory_file_name,
  1, 1, 0,
  "Returns the file name of the directory named DIR.\n\
This is the name of the file that holds the data for the directory DIR.\n\
In Unix-syntax, this just removes the final slash.\n\
On VMS, given a VMS-syntax directory name such as \"[X.Y]\",\n\
returns a file name such as \"[X]Y.DIR.1\".")
  (directory)
     Lisp_Object directory;
{
  char *buf;

  CHECK_STRING (directory, 0);

  if (NULL (directory))
    return Qnil;
#ifdef VMS
  /* 20 extra chars is insufficient for VMS, since we might perform a
     logical name translation. an equivalence string can be up to 255
     chars long, so grab that much extra space...  - sss */
  buf = (char *) alloca (XSTRING (directory)->size + 20 + 255);
#else
  buf = (char *) alloca (XSTRING (directory)->size + 20);
#endif
  directory_file_name (XSTRING (directory)->data, buf);
  return build_string (buf);
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
#ifdef VMS
  unsigned char * colon = 0;
  unsigned char * close = 0;
  unsigned char * slash = 0;
  unsigned char * brack = 0;
  int lbrack = 0, rbrack = 0;
  int dots = 0;
#endif /* VMS */
  
  CHECK_STRING (name, 0);

#ifdef VMS
  /* Filenames on VMS are always upper case.  */
  name = Fupcase (name);
#endif

  nm = XSTRING (name)->data;
  
  /* If nm is absolute, flush ...// and detect /./ and /../.
     If no /./ or /../ we can return right away. */
  if (
      nm[0] == '/'
#ifdef VMS
      || index (nm, ':')
#endif /* VMS */
      )
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
#ifdef VMS
	  if (p[0] == '\\')
	    lose = 1;
	  if (p[0] == '/') {
	    /* if dev:[dir]/, move nm to / */
	    if (!slash && p > nm && (brack || colon)) {
	      nm = (brack ? brack + 1 : colon + 1);
	      lbrack = rbrack = 0;
	      brack = 0;
	      colon = 0;
	    }
	    slash = p;
	  }
	  if (p[0] == '-')
#ifndef VMS4_4
	    /* VMS pre V4.4,convert '-'s in filenames. */
	    if (lbrack == rbrack)
	      {
		if (dots < 2)	/* this is to allow negative version numbers */
		  p[0] = '_';
	      }
	    else
#endif /* VMS4_4 */
	      if (lbrack > rbrack &&
		  ((p[-1] == '.' || p[-1] == '[' || p[-1] == '<') &&
		   (p[1] == '.' || p[1] == ']' || p[1] == '>')))
		lose = 1;
#ifndef VMS4_4
	      else
		p[0] = '_';
#endif /* VMS4_4 */
	  /* count open brackets, reset close bracket pointer */
	  if (p[0] == '[' || p[0] == '<')
	    lbrack++, brack = 0;
	  /* count close brackets, set close bracket pointer */
	  if (p[0] == ']' || p[0] == '>')
	    rbrack++, brack = p;
	  /* detect ][ or >< */
	  if ((p[0] == ']' || p[0] == '>') && (p[1] == '[' || p[1] == '<'))
	    lose = 1;
	  if ((p[0] == ':' || p[0] == ']' || p[0] == '>') && p[1] == '~')
	    nm = p + 1, lose = 1;
	  if (p[0] == ':' && (colon || slash))
	    /* if dev1:[dir]dev2:, move nm to dev2: */
	    if (brack)
	      {
		nm = brack + 1;
		brack = 0;
	      }
	    /* if /pathname/dev:, move nm to dev: */
	    else if (slash)
	      nm = slash + 1;
	    /* if node::dev:, move colon following dev */
	    else if (colon && colon[-1] == ':')
	      colon = p;
	    /* if dev1:dev2:, move nm to dev2: */
	    else if (colon && colon[-1] != ':')
	      {
		nm = colon + 1;
		colon = 0;
	      }
	  if (p[0] == ':' && !colon)
	    {
	      if (p[1] == ':')
		p++;
	      colon = p;
	    }
	  if (lbrack == rbrack)
	    if (p[0] == ';')
	      dots = 2;
	    else if (p[0] == '.')
	      dots++;
#endif /* VMS */
	  p++;
	}
      if (!lose)
	{
#ifdef VMS
	  if (index (nm, '/'))
	    return build_string (sys_translate_unix (nm));
#endif /* VMS */
	  if (nm == XSTRING (name)->data)
	    return name;
	  return build_string (nm);
	}
    }

  /* Now determine directory to start with and put it in NEWDIR.  */

  newdir = 0;

  if (nm[0] == '~')
    {
      if (nm[1] == '/'
#ifdef VMS
	  || nm[1] == ':'
#endif /* VMS */
	  || nm[1] == 0)
	{
	  /* Handle ~ on its own.  */
	  newdir = (unsigned char *) egetenv ("HOME");
	}
      else
	{
	  /* Handle ~ followed by user name.  */
	  unsigned char *user = nm + 1;
	  /* Find end of name.  */
	  unsigned char *ptr = (unsigned char *) index (user, '/');
	  int len = ptr ? ptr - user : strlen (user);
#ifdef VMS
	  unsigned char *ptr1 = index (user, ':');
	  if (ptr1 != 0 && ptr1 - user < len)
	    len = ptr1 - user;
#endif /* VMS */
	  /* Copy the user name into temp storage.  */
	  o = (unsigned char *) alloca (len + 1);
	  bcopy ((char *) user, o, len);
	  o[len] = 0;

	  /* Look up the user name.  */
	  pw = (struct passwd *) getpwnam (o);
	  if (!pw)
	    error ("User \"%s\" is not known", o);
	  newdir = (unsigned char *) pw->pw_dir;

	  /* Discard the user name from NM.  */
	  nm += len;
	}

      /* Discard the ~ from NM.  */
      nm++;
#ifdef VMS
      if (*nm != 0)
	nm++;			/* Don't leave the slash in nm.  */
#endif /* VMS */

      if (newdir == 0)
	newdir = (unsigned char *) "";
    }

  if (nm[0] != '/'
#ifdef VMS
      && !index (nm, ':')
#endif /* not VMS */
      && !newdir)
    {
      if (NULL (defalt))
	defalt = current_buffer->directory;
      CHECK_STRING (defalt, 1);
      newdir = XSTRING (defalt)->data;
    }

  /* Now concatenate the directory and name to new space in the stack frame */

  tlen = (newdir ? strlen (newdir) + 1 : 0) + strlen (nm) + 1;
  target = (unsigned char *) alloca (tlen);
  *target = 0;

  if (newdir)
    {
#ifndef VMS
      if (nm[0] == 0 || nm[0] == '/')
	strcpy (target, newdir);
      else
#endif
      file_name_as_directory (target, newdir);
    }

  strcat (target, nm);
#ifdef VMS
  if (index (target, '/'))
    strcpy (target, sys_translate_unix (target));
#endif /* VMS */

  /* Now canonicalize by removing /. and /foo/.. if they appear */

  p = target;
  o = target;

  while (*p)
    {
#ifdef VMS
      if (*p != ']' && *p != '>' && *p != '-')
	{
	  if (*p == '\\')
	    p++;
	  *o++ = *p++;
	}
      else if ((p[0] == ']' || p[0] == '>') && p[0] == p[1] + 2)
	/* brackets are offset from each other by 2 */
	{
	  p += 2;
	  if (*p != '.' && *p != '-' && o[-1] != '.')
	    /* convert [foo][bar] to [bar] */
	    while (o[-1] != '[' && o[-1] != '<')
	      o--;
	  else if (*p == '-' && *o != '.')
	    *--p = '.';
	}
      else if (p[0] == '-' && o[-1] == '.' &&
	       (p[1] == '.' || p[1] == ']' || p[1] == '>'))
	/* flush .foo.- ; leave - if stopped by '[' or '<' */
	{
	  do
	    o--;
	  while (o[-1] != '.' && o[-1] != '[' && o[-1] != '<');
	  if (p[1] == '.')	/* foo.-.bar ==> bar*/
	    p += 2;
	  else if (o[-1] == '.') /* '.foo.-]' ==> ']' */
	    p++, o--;
	  /* else [foo.-] ==> [-] */
	}
      else
	{
#ifndef VMS4_4
	  if (*p == '-' &&
	      o[-1] != '[' && o[-1] != '<' && o[-1] != '.' &&
	      p[1] != ']' && p[1] != '>' && p[1] != '.')
	    *p = '_';
#endif /* VMS4_4 */
	  *o++ = *p++;
	}
#else /* not VMS */
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
	       /* `/../' is the "superroot" on certain file systems.  */
	       && o != target
	       && (p[3] == '/' || p[3] == 0))
	{
	  while (o != target && *--o != '/')
	    ;
#ifdef APOLLO
	  if (o == target + 1 && o[-1] == '/' && o[0] == '/')
	    ++o;
	  else
#endif APOLLO
	  if (o == target && *o == '/')
	    ++o;
	  p += 3;
	}
      else
 	{
	  *o++ = *p++;
	}
#endif /* not VMS */
    }

  return make_string (target, o - target);
}

DEFUN ("substitute-in-file-name", Fsubstitute_in_file_name,
  Ssubstitute_in_file_name, 1, 1, 0,
  "Substitute environment variables referred to in STRING.\n\
A $ begins a request to substitute; the env variable name is the alphanumeric\n\
characters and underscores after the $, or is surrounded by braces.\n\
If a ~ appears following a /, everything through that / is discarded.\n\
On VMS, $ substitution is not done; this function does little and only\n\
duplicates what expand-file-name does.")
  (string)
     Lisp_Object string;
{
  unsigned char *nm;

  register unsigned char *s, *p, *o, *x, *endp;
  unsigned char *target;
  int total = 0;
  int substituted = 0;
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
	  && p != nm &&
#ifdef VMS
	  (p[-1] == ':' || p[-1] == ']' || p[-1] == '>' ||
#endif /* VMS */
	  p[-1] == '/')
#ifdef VMS
	  )
#endif /* VMS */
	{
	  nm = p;
	  substituted = 1;
	}
    }

#ifdef VMS
  return build_string (nm);
#else

  /* See if any variables are substituted into the string
     and find the total length of their values in `total' */

  for (p = nm; p != endp;)
    if (*p != '$')
      p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '$')
	  {
	    /* "$$" means a single "$" */
	    p++;
	    total -= 1;
	    substituted = 1;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (unsigned char *) alloca (s - o + 1);
	strncpy (target, o, s - o);
	target[s - o] = 0;

	/* Get variable value */
	o = (unsigned char *) egetenv (target);
/* The presence of this code makes vax 5.0 crash, for reasons yet unknown */
#if 0
#ifdef USG
	if (!o && !strcmp (target, "USER"))
	  o = egetenv ("LOGNAME");
#endif /* USG */
#endif /* 0 */
	if (!o) goto badvar;
	total += strlen (o);
	substituted = 1;
      }

  if (!substituted)
    return string;

  /* If substitution required, recopy the string and do it */
  /* Make space in stack frame for the new copy */
  xnm = (unsigned char *) alloca (XSTRING (string)->size + total + 1);
  x = xnm;

  /* Copy the rest of the name through, replacing $ constructs with values */
  for (p = nm; *p;)
    if (*p != '$')
      *x++ = *p++;
    else
      {
	p++;
	if (p == endp)
	  goto badsubst;
	else if (*p == '$')
	  {
	    *x++ = *p++;
	    continue;
	  }
	else if (*p == '{')
	  {
	    o = ++p;
	    while (p != endp && *p != '}') p++;
	    if (*p != '}') goto missingclose;
	    s = p++;
	  }
	else
	  {
	    o = p;
	    while (p != endp && (isalnum (*p) || *p == '_')) p++;
	    s = p;
	  }

	/* Copy out the variable name */
	target = (unsigned char *) alloca (s - o + 1);
	strncpy (target, o, s - o);
	target[s - o] = 0;

	/* Get variable value */
	o = (unsigned char *) egetenv (target);
/* The presence of this code makes vax 5.0 crash, for reasons yet unknown */
#if 0
#ifdef USG
	if (!o && !strcmp (target, "USER"))
	  o = egetenv ("LOGNAME");
#endif /* USG */
#endif /* 0 */
	if (!o)
	  goto badvar;

	strcpy (x, o);
	x += strlen (o);
      }

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
  error ("Substituting nonexistent environment variable \"%s\"", target);

  /* NOTREACHED */
#endif /* not VMS */
}

Lisp_Object
expand_and_dir_to_file (filename, defdir)
     Lisp_Object filename, defdir;
{
  register Lisp_Object abspath;

  abspath = Fexpand_file_name (filename, defdir);
#ifdef VMS
  {
    register int c = XSTRING (abspath)->data[XSTRING (abspath)->size - 1];
    if (c == ':' || c == ']' || c == '>')
      abspath = Fdirectory_file_name (abspath);
  }
#else
  /* Remove final slash, if any (unless path is root).
     stat behaves differently depending!  */
  if (XSTRING (abspath)->size > 1
      && XSTRING (abspath)->data[XSTRING (abspath)->size - 1] == '/')
    {
      if (EQ (abspath, filename))
	abspath = Fcopy_sequence (abspath);
      XSTRING (abspath)->data[XSTRING (abspath)->size - 1] = 0;
    }
#endif
  return abspath;
}

barf_or_query_if_file_exists (absname, querystring, interactive)
     Lisp_Object absname;
     unsigned char *querystring;
     int interactive;
{
  register Lisp_Object tem;
  struct gcpro gcpro1;

  if (access (XSTRING (absname)->data, 4) >= 0)
    {
      if (! interactive)
	Fsignal (Qfile_already_exists,
		 Fcons (build_string ("File already exists"),
			Fcons (absname, Qnil)));
      GCPRO1 (absname);
      tem = Fyes_or_no_p (format1 ("File %s already exists; %s anyway? ",
				   XSTRING (absname)->data, querystring));
      UNGCPRO;
      if (NULL (tem))
	Fsignal (Qfile_already_exists,
		 Fcons (build_string ("File already exists"),
			Fcons (absname, Qnil)));
    }
  return;
}

DEFUN ("copy-file", Fcopy_file, Scopy_file, 2, 4,
  "fCopy file: \nFCopy %s to file: \np",
  "Copy FILE to NEWNAME.  Both args strings.\n\
Signals a  file-already-exists  error if NEWNAME already exists,\n\
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.\n\
A number as third arg means request confirmation if NEWNAME already exists.\n\
This is what happens in interactive use with M-x.\n\
Fourth arg non-nil means give the new file the same last-modified time\n\
that the old one has.  (This works on only some systems.)")
  (filename, newname, ok_if_already_exists, keep_date)
     Lisp_Object filename, newname, ok_if_already_exists, keep_date;
{
  int ifd, ofd, n;
  char buf[16 * 1024];
  struct stat st;
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);
  if (NULL (ok_if_already_exists)
      || XTYPE (ok_if_already_exists) == Lisp_Int)
    barf_or_query_if_file_exists (newname, "copy to it",
				  XTYPE (ok_if_already_exists) == Lisp_Int);

  ifd = open (XSTRING (filename)->data, 0);
  if (ifd < 0)
    report_file_error ("Opening input file", Fcons (filename, Qnil));

#ifdef VMS
  /* Create the copy file with the same record format as the input file */
  ofd = sys_creat (XSTRING (newname)->data, 0666, ifd);
#else
  ofd = creat (XSTRING (newname)->data, 0666);
#endif /* VMS */
  if (ofd < 0)
    {
      close (ifd);
      report_file_error ("Opening output file", Fcons (newname, Qnil));
    }

  while ((n = read (ifd, buf, sizeof buf)) > 0)
    if (write (ofd, buf, n) != n)
      {
	close (ifd);
	close (ofd);
	report_file_error ("I/O error", Fcons (newname, Qnil));
      }

  if (fstat (ifd, &st) >= 0)
    {
#ifdef HAVE_TIMEVAL
      if (!NULL (keep_date))
	{
#ifdef USE_UTIME
/* AIX has utimes() in compatibility package, but it dies.  So use good old
   utime interface instead. */
	  struct {
	    time_t atime;
	    time_t mtime;
	  } tv;
	  tv.atime = st.st_atime;
	  tv.mtime = st.st_mtime;
	  utime (XSTRING (newname)->data, &tv);
#else /* not USE_UTIME */
	  struct timeval timevals[2];
	  timevals[0].tv_sec = st.st_atime;
	  timevals[1].tv_sec = st.st_mtime;
	  timevals[0].tv_usec = timevals[1].tv_usec = 0;
	  utimes (XSTRING (newname)->data, timevals);
#endif /* not USE_UTIME */
	}
#endif /* HAVE_TIMEVALS */

#ifdef APOLLO
      if (!egetenv ("USE_DOMAIN_ACLS"))
#endif
      chmod (XSTRING (newname)->data, st.st_mode & 07777);
    }

  close (ifd);
  if (close (ofd) < 0)
    report_file_error ("I/O error", Fcons (newname, Qnil));

  UNGCPRO;
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
  "fRename file: \nFRename %s to file: \np",
  "Rename FILE as NEWNAME.  Both args strings.\n\
If file has names other than FILE, it continues to have those names.\n\
Signals a  file-already-exists  error if NEWNAME already exists\n\
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\n\
A number as third arg means request confirmation if NEWNAME already exists.\n\
This is what happens in interactive use with M-x.")
  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);
  if (NULL (ok_if_already_exists)
      || XTYPE (ok_if_already_exists) == Lisp_Int)
    barf_or_query_if_file_exists (newname, "rename to it",
				  XTYPE (ok_if_already_exists) == Lisp_Int);
#ifndef BSD4_1
  if (0 > rename (XSTRING (filename)->data, XSTRING (newname)->data))
#else
  if (0 > link (XSTRING (filename)->data, XSTRING (newname)->data)
      || 0 > unlink (XSTRING (filename)->data))
#endif
    {
      if (errno == EXDEV)
	{
	  Fcopy_file (filename, newname, ok_if_already_exists, Qt);
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
  UNGCPRO;
  return Qnil;
}

DEFUN ("add-name-to-file", Fadd_name_to_file, Sadd_name_to_file, 2, 3,
  "fAdd name to file: \nFName to add to %s: \np",
  "Give FILE additional name NEWNAME.  Both args strings.\n\
Signals a  file-already-exists  error if NEWNAME already exists\n\
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\n\
A number as third arg means request confirmation if NEWNAME already exists.\n\
This is what happens in interactive use with M-x.")
  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);
  if (NULL (ok_if_already_exists)
      || XTYPE (ok_if_already_exists) == Lisp_Int)
    barf_or_query_if_file_exists (newname, "make it a new name",
				  XTYPE (ok_if_already_exists) == Lisp_Int);
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

  UNGCPRO;
  return Qnil;
}

#ifdef S_IFLNK
DEFUN ("make-symbolic-link", Fmake_symbolic_link, Smake_symbolic_link, 2, 3,
  "FMake symbolic link to file: \nFMake symbolic link to file %s: \np",
  "Make a symbolic link to FILENAME, named LINKNAME.  Both args strings.\n\
Signals a  file-already-exists  error if NEWNAME already exists\n\
unless optional third argument OK-IF-ALREADY-EXISTS is non-nil.\n\
A number as third arg means request confirmation if NEWNAME already exists.\n\
This happens for interactive use with M-x.")
  (filename, newname, ok_if_already_exists)
     Lisp_Object filename, newname, ok_if_already_exists;
{
#ifdef NO_ARG_ARRAY
  Lisp_Object args[2];
#endif
  struct gcpro gcpro1, gcpro2;

  GCPRO2 (filename, newname);
  CHECK_STRING (filename, 0);
  CHECK_STRING (newname, 1);
  filename = Fexpand_file_name (filename, Qnil);
  newname = Fexpand_file_name (newname, Qnil);
  if (NULL (ok_if_already_exists)
      || XTYPE (ok_if_already_exists) == Lisp_Int)
    barf_or_query_if_file_exists (newname, "make it a link",
				  XTYPE (ok_if_already_exists) == Lisp_Int);
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
  UNGCPRO;
  return Qnil;
}
#endif /* S_IFLNK */

#ifdef VMS

DEFUN ("define-logical-name", Fdefine_logical_name, Sdefine_logical_name,
       2, 2,
       "sDefine logical name: \nsDefine logical name %s as: ",
       "Define the job-wide logical name NAME to have the value STRING.\n\
If STRING is nil or a null string, the logical name NAME is deleted.")
  (varname, string)
     Lisp_Object varname;
     Lisp_Object string;
{
  CHECK_STRING (varname, 0);
  if (NULL (string))
    delete_logical_name (XSTRING (varname)->data);
  else
    {
      CHECK_STRING (string, 1);

      if (XSTRING (string)->size == 0)
        delete_logical_name (XSTRING (varname)->data);
      else
        define_logical_name (XSTRING (varname)->data, XSTRING (string)->data);
    }

  return string;
}
#endif /* VMS */

#ifdef HPUX_NET

DEFUN ("sysnetunam", Fsysnetunam, Ssysnetunam, 2, 2, 0,
       "Open a network connection to PATH using LOGIN as the login string.")
     (path, login)
     Lisp_Object path, login;
{
  int netresult;
  
  CHECK_STRING (path, 0);
  CHECK_STRING (login, 0);  
  
  netresult = netunam (XSTRING (path)->data, XSTRING (login)->data);

  if (netresult == -1)
    return Qnil;
  else
    return Qt;
}
#endif /* HPUX_NET */

DEFUN ("file-name-absolute-p", Ffile_name_absolute_p, Sfile_name_absolute_p,
       1, 1, 0,
       "Return t if file FILENAME specifies an absolute path name.")
     (filename)
     Lisp_Object filename;
{
  unsigned char *ptr;

  CHECK_STRING (filename, 0);
  ptr = XSTRING (filename)->data;
  if (*ptr == '/' || *ptr == '~'
#ifdef VMS
/* ??? This criterion is probably wrong for '<'.  */
      || index (ptr, ':') || index (ptr, '<')
      || (*ptr == '[' && (ptr[1] != '-' || (ptr[2] != '.' && ptr[2] != ']'))
	  && ptr[1] != '.')
#endif /* VMS */
      )
    return Qt;
  else
    return Qnil;
}

DEFUN ("file-exists-p", Ffile_exists_p, Sfile_exists_p, 1, 1, 0,
  "Return t if file FILENAME exists.  (This does not mean you can read it.)\n\
See also file-readable-p and file-attributes.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);
  return (access (XSTRING (abspath)->data, 0) >= 0) ? Qt : Qnil;
}

DEFUN ("file-readable-p", Ffile_readable_p, Sfile_readable_p, 1, 1, 0,
  "Return t if file FILENAME exists and you can read it.\n\
See also file-exists-p and file-attributes.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object abspath;

  CHECK_STRING (filename, 0);
  abspath = Fexpand_file_name (filename, Qnil);
  return (access (XSTRING (abspath)->data, 4) >= 0) ? Qt : Qnil;
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
  filename = Fexpand_file_name (filename, Qnil);

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

/* Having this before file-symlink-p mysteriously caused it to be forgotten
   on the RT/PC.  */
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
#ifdef VMS
  if (!NULL (dir))
    dir = Fdirectory_file_name (dir);
#endif /* VMS */
  return (access (!NULL (dir) ? (char *) XSTRING (dir)->data : "", 2) >= 0
	  ? Qt : Qnil);
}

DEFUN ("file-directory-p", Ffile_directory_p, Sfile_directory_p, 1, 1, 0,
  "Return t if file FILENAME is the name of a directory as a file.\n\
A directory name spec may be given instead; then the value is t\n\
if the directory so specified exists and really is a directory.")
  (filename)
     Lisp_Object filename;
{
  register Lisp_Object abspath;
  struct stat st;

  abspath = expand_and_dir_to_file (filename, current_buffer->directory);

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

  abspath = expand_and_dir_to_file (filename, current_buffer->directory);

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

  abspath = Fexpand_file_name (filename, current_buffer->directory);
  CHECK_NUMBER (mode, 1);

#ifndef APOLLO
  if (chmod (XSTRING (abspath)->data, XINT (mode)) < 0)
    report_file_error ("Doing chmod", Fcons (abspath, Qnil));
#else /* APOLLO */
  if (!egetenv ("USE_DOMAIN_ACLS"))
    {
      struct stat st;
      struct timeval tvp[2];

      /* chmod on apollo also change the file's modtime; need to save the
	 modtime and then restore it. */
      if (stat (XSTRING (abspath)->data, &st) < 0)
	{
	  report_file_error ("Doing chmod", Fcons (abspath, Qnil));
	  return (Qnil);
	}
 
      if (chmod (XSTRING (abspath)->data, XINT (mode)) < 0)
 	report_file_error ("Doing chmod", Fcons (abspath, Qnil));
 
      /* reset the old accessed and modified times.  */
      tvp[0].tv_sec = st.st_atime + 1; /* +1 due to an Apollo roundoff bug */
      tvp[0].tv_usec = 0;
      tvp[1].tv_sec = st.st_mtime + 1; /* +1 due to an Apollo roundoff bug */
      tvp[1].tv_usec = 0;
 
      if (utimes (XSTRING (abspath)->data, tvp) < 0)
 	report_file_error ("Doing utimes", Fcons (abspath, Qnil));
    }
#endif /* APOLLO */

  return Qnil;
}

DEFUN ("file-newer-than-file-p", Ffile_newer_than_file_p, Sfile_newer_than_file_p, 2, 2, 0,
  "Return t if file FILE1 is newer than file FILE2.\n\
If FILE1 does not exist, the answer is nil;\n\
otherwise, if FILE2 does not exist, the answer is t.")
  (file1, file2)
     Lisp_Object file1, file2;
{
  Lisp_Object abspath;
  struct stat st;
  int mtime1;

  CHECK_STRING (file1, 0);
  CHECK_STRING (file2, 0);

  abspath = expand_and_dir_to_file (file1, current_buffer->directory);

  if (stat (XSTRING (abspath)->data, &st) < 0)
    return Qnil;

  mtime1 = st.st_mtime;

  abspath = expand_and_dir_to_file (file2, current_buffer->directory);

  if (stat (XSTRING (abspath)->data, &st) < 0)
    return Qt;

  return (mtime1 > st.st_mtime) ? Qt : Qnil;
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
If second argument VISIT is non-nil, the buffer's visited filename\n\
and last save file modtime are set, and it is marked unmodified.\n\
If visiting and the file does not exist, visiting is completed\n\
before the error is signaled.")
  (filename, visit)
     Lisp_Object filename, visit;
{
  struct stat st;
  register int fd;
  register int inserted = 0;
  register int i = 0;
  int count = specpdl_ptr - specpdl;
  struct gcpro gcpro1;

  GCPRO1 (filename);
  if (!NULL (current_buffer->read_only))
    Fbarf_if_buffer_read_only();

  CHECK_STRING (filename, 0);
  filename = Fexpand_file_name (filename, Qnil);

  fd = -1;

#ifndef APOLLO
  if (stat (XSTRING (filename)->data, &st) < 0
	|| (fd = open (XSTRING (filename)->data, 0)) < 0)
#else
  if ((fd = open (XSTRING (filename)->data, 0)) < 0
      || fstat (fd, &st) < 0)
#endif /* not APOLLO */
    {
      if (fd >= 0) close (fd);
      if (NULL (visit))
	report_file_error ("Opening input file", Fcons (filename, Qnil));
      st.st_mtime = -1;
      goto notfound;
    }

  record_unwind_protect (close_file_unwind, make_number (fd));

  /* Supposedly happens on VMS.  */
  if (st.st_size < 0)
    error ("File size is negative");
  {
    register Lisp_Object temp;

    /* Make sure point-max won't overflow after this insertion.  */
    XSET (temp, Lisp_Int, st.st_size + Z);
    if (st.st_size + Z != XINT (temp))
      error ("maximum buffer size exceeded");
  }

  if (NULL (visit))
    prepare_to_modify_buffer ();

  move_gap (point);
  if (GAP_SIZE < st.st_size)
    make_gap ((int)st.st_size - GAP_SIZE);
    
  while (1)
    {
      int try = min (st.st_size - inserted, 64 << 10);
      int this = read (fd, &FETCH_CHAR (point + inserted - 1) + 1, try);

      if (this <= 0)
	{
	  i = this;
	  break;
	}

      GPT += this;
      GAP_SIZE -= this;
      ZV += this;
      Z += this;
      inserted += this;
    }

  if (inserted > 0)
    MODIFF++;
  record_insert (point, inserted);

  close (fd);

  /* Discard the unwind protect */
  specpdl_ptr = specpdl + count;

  if (i < 0)
    error ("IO error reading %s: %s",
	   XSTRING (filename)->data, err_str (errno));

 notfound:

  if (!NULL (visit))
    {
      current_buffer->undo_list = Qnil;
#ifdef APOLLO
      stat (XSTRING (filename)->data, &st);
#endif
      current_buffer->modtime = st.st_mtime;
      current_buffer->save_modified = MODIFF;
      current_buffer->auto_save_modified = MODIFF;
      XFASTINT (current_buffer->save_length) = Z - BEG;
#ifdef CLASH_DETECTION
      if (!NULL (current_buffer->filename))
	unlock_file (current_buffer->filename);
      unlock_file (filename);
#endif /* CLASH_DETECTION */
      current_buffer->filename = filename;
      /* If visiting nonexistent file, return nil.  */
      if (st.st_mtime == -1)
	report_file_error ("Opening input file", Fcons (filename, Qnil));
    }

  UNGCPRO;
  return Fcons (filename, Fcons (make_number (inserted), Qnil));
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
  register int desc;
  int failure;
  int save_errno;
  unsigned char *fn;
  struct stat st;
  int tem;
  int count = specpdl_ptr - specpdl;
#ifdef VMS
  unsigned char *fname = 0;	/* If non-0, original filename (must rename) */
#endif /* VMS */

  /* Special kludge to simplify auto-saving */
  if (NULL (start))
    {
      XFASTINT (start) = BEG;
      XFASTINT (end) = Z;
    }
  else
    validate_region (&start, &end);

  filename = Fexpand_file_name (filename, Qnil);
  fn = XSTRING (filename)->data;

#ifdef CLASH_DETECTION
  if (!auto_saving)
    lock_file (filename);
#endif /* CLASH_DETECTION */

  desc = -1;
  if (!NULL (append))
    desc = open (fn, O_WRONLY);

  if (desc < 0)
#ifdef VMS
    if (auto_saving)	/* Overwrite any previous version of autosave file */
      {
	vms_truncate (fn);	/* if fn exists, truncate to zero length */
	desc = open (fn, O_RDWR);
	if (desc < 0)
	  desc = creat_copy_attrs (XTYPE (current_buffer->filename) == Lisp_String
				   ? XSTRING (current_buffer->filename)->data : 0,
				   fn);
      }
    else		/* Write to temporary name and rename if no errors */
      {
	Lisp_Object temp_name;
	temp_name = Ffile_name_directory (filename);

	if (!NULL (temp_name))
	  {
	    temp_name = Fmake_temp_name (concat2 (temp_name,
						  build_string ("$$SAVE$$")));
	    fname = XSTRING (filename)->data;
	    fn = XSTRING (temp_name)->data;
	    desc = creat_copy_attrs (fname, fn);
	    if (desc < 0)
	      {
		/* If we can't open the temporary file, try creating a new
		   version of the original file.  VMS "creat" creates a
		   new version rather than truncating an existing file. */
		fn = fname;
		fname = 0;
		desc = creat (fn, 0666);
		if (desc < 0)
		  {
		    /* We can't make a new version;
		       try to truncate and rewrite existing version if any.  */
		    vms_truncate (fn);
		    desc = open (fn, O_RDWR);
		  }
	      }
	  }
	else
	  desc = creat (fn, 0666);
      }
#else /* not VMS */
  desc = creat (fn, 0666);
#endif /* not VMS */

  if (desc < 0)
    {
#ifdef CLASH_DETECTION
      save_errno = errno;
      if (!auto_saving) unlock_file (filename);
      errno = save_errno;
#endif /* CLASH_DETECTION */
      report_file_error ("Opening output file", Fcons (filename, Qnil));
    }

  record_unwind_protect (close_file_unwind, make_number (desc));

  if (!NULL (append))
    if (lseek (desc, (off_t) 0, 2) < 0)
      {
#ifdef CLASH_DETECTION
	if (!auto_saving) unlock_file (filename);
#endif /* CLASH_DETECTION */
	report_file_error ("Lseek error", Fcons (filename, Qnil));
      }

#ifdef VMS
/*
 * Kludge Warning: The VMS C RTL likes to insert carriage returns
 * if we do writes that don't end with a carriage return. Furthermore
 * it cannot handle writes of more then 16K. The modified
 * version of "sys_write" in SYSDEP.C (see comment there) copes with
 * this EXCEPT for the last record (iff it doesn't end with a carriage
 * return). This implies that if your buffer doesn't end with a carriage
 * return, you get one free... tough. However it also means that if
 * we make two calls to sys_write (a la the following code) you can
 * get one at the gap as well. The easiest way to fix this (honest)
 * is to move the gap to the next newline (or the end of the buffer).
 * Thus this change.
 *
 * Yech!
 */
  if (GPT > BEG && GPT_ADDR[-1] != '\n')
    move_gap (find_next_newline (GPT, 1));
#endif

  failure = 0;
  if (XINT (start) != XINT (end))
    {
      if (XINT (start) < GPT)
	{
	  register int end1 = XINT (end);
	  tem = XINT (start);
	  failure = 0 > e_write (desc, &FETCH_CHAR (tem),
				 min (GPT, end1) - tem);
	  save_errno = errno;
	}

      if (XINT (end) > GPT && !failure)
	{
	  tem = XINT (start);
	  tem = max (tem, GPT);
	  failure = 0 > e_write (desc, &FETCH_CHAR (tem), XINT (end) - tem);
	  save_errno = errno;
	}
    }

#ifndef USG
#ifndef VMS
#ifndef BSD4_1
#ifndef alliant /* trinkle@cs.purdue.edu says fsync can return EBUSY
		   on alliant, for no visible reason.  */
  /* Note fsync appears to change the modtime on BSD4.2 (both vax and sun).
     Disk full in NFS may be reported here.  */
  if (fsync (desc) < 0)
    failure = 1, save_errno = errno;
#endif
#endif
#endif
#endif

#if 0
  /* Spurious "file has changed on disk" warnings have been 
     observed on Sun 3 as well.  Maybe close changes the modtime
     with nfs as well.  */

  /* On VMS and APOLLO, must do the stat after the close
     since closing changes the modtime.  */
#ifndef VMS
#ifndef APOLLO
  /* Recall that #if defined does not work on VMS.  */
#define FOO
  fstat (desc, &st);
#endif
#endif
#endif /* 0 */

  /* NFS can report a write failure now.  */
  if (close (desc) < 0)
    failure = 1, save_errno = errno;

#ifdef VMS
  /* If we wrote to a temporary name and had no errors, rename to real name. */
  if (fname)
    {
      if (!failure)
	failure = (rename (fn, fname) != 0), save_errno = errno;
      fn = fname;
    }
#endif /* VMS */

#ifndef FOO
  stat (fn, &st);
#endif
  /* Discard the unwind protect */
  specpdl_ptr = specpdl + count;

#ifdef CLASH_DETECTION
  if (!auto_saving)
    unlock_file (filename);
#endif /* CLASH_DETECTION */

  /* Do this before reporting IO error
     to avoid a "file has changed on disk" warning on
     next attempt to save.  */
  if (EQ (visit, Qt))
    current_buffer->modtime = st.st_mtime;

  if (failure)
    error ("IO error writing %s: %s", fn, err_str (save_errno));

  if (EQ (visit, Qt))
    {
      current_buffer->save_modified = MODIFF;
      XFASTINT (current_buffer->save_length) = Z - BEG;
      current_buffer->filename = filename;
    }
  else if (!NULL (visit))
    return Qnil;

  if (!auto_saving)
    message ("Wrote %s", fn);

  return Qnil;
}

int
e_write (desc, addr, len)
     int desc;
     register char *addr;
     register int len;
{
  char buf[16 * 1024];
  register char *p, *end;

  if (!EQ (current_buffer->selective_display, Qt))
    return write (desc, addr, len) - len;
  else
    {
      p = buf;
      end = p + sizeof buf;
      while (len--)
	{
	  if (p == end)
	    {
	      if (write (desc, buf, sizeof buf) != sizeof buf)
		return -1;
	      p = buf;
	    }
	  *p = *addr++;
	  if (*p++ == '\015')
	    p[-1] = '\n';
	}
      if (p != buf)
	if (write (desc, buf, p - buf) != p - buf)
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
  struct buffer *b;
  struct stat st;

  CHECK_BUFFER (buf, 0);
  b = XBUFFER (buf);

  if (XTYPE (b->filename) != Lisp_String) return Qt;
  if (b->modtime == 0) return Qt;

  if (stat (XSTRING (b->filename)->data, &st) < 0)
    {
      /* If the file doesn't exist now and didn't exist before,
	 we say that it isn't modified, provided the error is a tame one.  */
      if (errno == ENOENT || errno == EACCES || errno == ENOTDIR)
	st.st_mtime = -1;
      else
	st.st_mtime = 0;
    }
  if (st.st_mtime == b->modtime
      /* If both are positive, accept them if they are off by one second.  */
      || (st.st_mtime > 0 && b->modtime > 0
	  && (st.st_mtime == b->modtime + 1
	      || st.st_mtime == b->modtime - 1)))
    return Qt;
  return Qnil;
}

DEFUN ("clear-visited-file-modtime", Fclear_visited_file_modtime,
  Sclear_visited_file_modtime, 0, 0, 0,
  "Clear out records of last mod time of visited file.\n\
Next attempt to save will certainly not complain of a discrepancy.")
  ()
{
  current_buffer->modtime = 0;
  return Qnil;
}

Lisp_Object
auto_save_error ()
{
  unsigned char *name = XSTRING (current_buffer->name)->data;

  bell ();
  message ("Autosaving...error for %s", name);
  Fsleep_for (make_number (1));
  message ("Autosaving...error!for %s", name);
  Fsleep_for (make_number (1));
  message ("Autosaving...error for %s", name);
  Fsleep_for (make_number (1));
  return Qnil;
}

Lisp_Object
auto_save_1 ()
{
  return
    Fwrite_region (Qnil, Qnil,
		   current_buffer->auto_save_file_name,
		   Qnil, Qlambda);
}

DEFUN ("do-auto-save", Fdo_auto_save, Sdo_auto_save, 0, 1, "",
  "Auto-save all buffers that need it.\n\
This is all buffers that have auto-saving enabled\n\
and are changed since last auto-saved.\n\
Auto-saving writes the buffer into a file\n\
so that your editing is not lost if the system crashes.\n\
This file is not the file you visited; that changes only when you save.\n\n\
Non-nil argument means do not print any message if successful.")
  (nomsg)
     Lisp_Object nomsg;
{
  struct buffer *old = current_buffer, *b;
  Lisp_Object tail, buf;
  int auto_saved = 0;
  int tried = 0;
  char *omessage = echo_area_contents;
  /* No GCPRO needed, because (when it matters) all Lisp_Object variables
     point to non-strings reached from Vbuffer_alist.  */

  auto_saving = 1;
  if (minibuf_level)
    nomsg = Qt;

  for (tail = Vbuffer_alist; XGCTYPE (tail) == Lisp_Cons;
       tail = XCONS (tail)->cdr)
    {
      buf = XCONS (XCONS (tail)->car)->cdr;
      b = XBUFFER (buf);
      /* Check for auto save enabled
	 and file changed since last auto save
	 and file changed since last real save.  */
      if (XTYPE (b->auto_save_file_name) == Lisp_String
	  && b->save_modified < BUF_MODIFF (b)
	  && b->auto_save_modified < BUF_MODIFF (b))
	{
	  /* If we at least consider a buffer for auto-saving,
	     don't try again for a suitable time.  */
	  tried++;
	  if ((XFASTINT (b->save_length) * 10
	       > (BUF_Z (b) - BUF_BEG (b)) * 13)
	      /* A short file is likely to change a large fraction;
		 spare the user annoying messages.  */
	      && XFASTINT (b->save_length) > 5000
	      /* These messages are frequent and annoying for `*mail*'.  */
	      && !EQ (b->filename, Qnil))
	    {
	      /* It has shrunk too much; don't checkpoint. */
	      message ("Buffer %s has shrunk a lot; not autosaving it",
		       XSTRING (b->name)->data);
	      Fsleep_for (make_number (1));
	      continue;
	    }
	  set_buffer_internal (b);
	  if (!auto_saved && NULL (nomsg))
	    message1 ("Auto-saving...");
	  internal_condition_case (auto_save_1, Qt, auto_save_error);
	  auto_saved++;
	  b->auto_save_modified = BUF_MODIFF (b);
	  XFASTINT (current_buffer->save_length) = Z - BEG;
	  set_buffer_internal (old);
	}
    }

  if (tried)
    record_auto_save ();

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
  current_buffer->auto_save_modified = MODIFF;
  XFASTINT (current_buffer->save_length) = Z - BEG;
  return Qnil;
}

DEFUN ("recent-auto-save-p", Frecent_auto_save_p, Srecent_auto_save_p,
  0, 0, 0,
  "Return t if buffer has been auto-saved since last read in or saved.")
  ()
{
  return (current_buffer->save_modified < current_buffer->auto_save_modified) ? Qt : Qnil;
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
      if (XTYPE (val) != Lisp_String)
	return (val);

      if (!NULL (specdir))
	val = concat2 (specdir, val);
#ifndef VMS
      {
	register unsigned char *old, *new;
	register int n;
	int osize, count;

	osize = XSTRING (val)->size;
	/* Quote "$" as "$$" to get it past substitute-in-file-name */
	for (n = osize, count = 0, old = XSTRING (val)->data; n > 0; n--)
	  if (*old++ == '$') count++;
	if (count > 0)
	  {
	    old = XSTRING (val)->data;
	    val = Fmake_string (make_number (osize + count), make_number (0));
	    new = XSTRING (val)->data;
	    for (n = osize; n > 0; n--)
	      if (*old != '$')
		*new++ = *old++;
	      else
		{
		  *new++ = '$';
		  *new++ = '$';
		  old++;
		}
	  }
      }
#endif /* Not VMS */
      return (val);
    }

  if (EQ (action, Qt))
    return Ffile_name_all_completions (name, realdir);
  /* Only other case actually used is ACTION = lambda */
#ifdef VMS
  /* Supposedly this helps commands such as `cd' that read directory names,
     but can someone explain how it helps them? -- RMS */
  if (XSTRING (name)->size == 0)
    return Qt;
#endif /* VMS */
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
  int count;

  if (NULL (dir))
    dir = current_buffer->directory;
  if (NULL (defalt))
    defalt = current_buffer->filename;

  /* If dir starts with user's homedir, change that to ~. */
  homedir = (char *) egetenv ("HOME");
  if (homedir != 0
      && XTYPE (dir) == Lisp_String
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

#ifdef VMS
  count = specpdl_ptr - specpdl;
  specbind (intern ("completion-ignore-case"), Qt);
#endif

  GCPRO2 (insdef, defalt);
  val = Fcompleting_read (prompt, intern ("read-file-name-internal"),
			  dir, mustmatch,
			  insert_default_directory ? insdef : Qnil);

#ifdef VMS
  unbind_to (count);
#endif

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

  DEFVAR_BOOL ("insert-default-directory", &insert_default_directory,
    "*Non-nil means when reading a filename start with default dir in minibuffer.");
  insert_default_directory = 1;

  DEFVAR_BOOL ("vms-stmlf-recfm", &vms_stmlf_recfm,
    "*Non-nil means write new files with record format `stmlf'.\n\
nil means use format `var'.  This variable is meaningful only on VMS.");
  vms_stmlf_recfm = 0;

  defsubr (&Sfile_name_directory);
  defsubr (&Sfile_name_nondirectory);
  defsubr (&Sfile_name_as_directory);
  defsubr (&Sdirectory_file_name);
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
#ifdef VMS
  defsubr (&Sdefine_logical_name);
#endif /* VMS */
#ifdef HPUX_NET
  defsubr (&Ssysnetunam);
#endif /* HPUX_NET */
  defsubr (&Sfile_name_absolute_p);
  defsubr (&Sfile_exists_p);
  defsubr (&Sfile_readable_p);
  defsubr (&Sfile_writable_p);
  defsubr (&Sfile_symlink_p);
  defsubr (&Sfile_directory_p);
  defsubr (&Sfile_modes);
  defsubr (&Sset_file_modes);
  defsubr (&Sfile_newer_than_file_p);
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
