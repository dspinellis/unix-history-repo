/* Lisp functions for making directory listings.
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


#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "config.h"

#ifdef SYSV_SYSTEM_DIR

#include <dirent.h>
#define DIRENTRY struct dirent
#define NAMLEN(p) strlen (p->d_name)

#else

#ifdef NONSYSTEM_DIR_LIBRARY
#include "ndir.h"
#else /* not NONSYSTEM_DIR_LIBRARY */
#include <sys/dir.h>
#endif /* not NONSYSTEM_DIR_LIBRARY */

#define DIRENTRY struct direct
#define NAMLEN(p) p->d_namlen

extern DIR *opendir ();
extern struct direct *readdir ();

#endif

#undef NULL

#include "lisp.h"
#include "buffer.h"
#include "commands.h"

#include "regex.h"

#define min(a, b) ((a) < (b) ? (a) : (b))

/* if system does not have symbolic links, it does not have lstat.
   In that case, use ordinary stat instead.  */

#ifndef S_IFLNK
#define lstat stat
#endif

Lisp_Object Vcompletion_ignored_extensions;

Lisp_Object Qcompletion_ignore_case;

DEFUN ("directory-files", Fdirectory_files, Sdirectory_files, 1, 3, 0,
  "Return a list of names of files in DIRECTORY.\n\
If FULL is non-NIL, absolute pathnames of the files are returned.\n\
If MATCH is non-NIL, only pathnames containing that regexp are returned.")
  (dirname, full, match)
     Lisp_Object dirname, full, match;
{
  DIR *d;
  char slashfilename[MAXNAMLEN+2];
  char *filename = slashfilename;
  int length;
  Lisp_Object list, name;

  /* In search.c */
  extern struct re_pattern_buffer searchbuf;

  if (!NULL (match))
    {
      CHECK_STRING (match, 3);
      /* Compile it now so we don't get an error after opendir */
#ifdef VMS
      compile_pattern (match, &searchbuf, (char *) downcase_table);
#else
      compile_pattern (match, &searchbuf, 0);
#endif
    }

  dirname = Fexpand_file_name (dirname, Qnil);
  if (!(d = opendir (XSTRING (Fdirectory_file_name (dirname))->data)))
    report_file_error ("Opening directory", Fcons (dirname, Qnil));

  list = Qnil;
  length = XSTRING (dirname)->size;
#ifndef VMS
  if (length == 0   ||  XSTRING (dirname)->data[length - 1] != '/')
    *filename++ = '/';
#endif /* VMS */

  /* Loop reading blocks */
  while (1)
    {
      DIRENTRY *dp = readdir (d);
      int len;

      if (!dp) break;
      len = NAMLEN (dp);
      if (dp->d_ino)
	{
	  strncpy (filename, dp->d_name, len);
	  filename[len] = 0;
	  if (NULL (match) ||
	      (0 <= re_search (&searchbuf, filename, len, 0, len, 0)))
	    {
	      if (!NULL (full))
		name = concat2 (dirname, build_string (slashfilename));
	      else
		name = build_string (filename);
	      list = Fcons (name, list);
	    }
	}
    }
  closedir (d);
  return Fsort (Fnreverse (list), Qstring_lessp);
}

Lisp_Object file_name_completion ();

DEFUN ("file-name-completion", Ffile_name_completion, Sfile_name_completion,
  2, 2, 0,
  "Complete file name FILE in directory DIR.\n\
Returns the longest string common to all filenames in DIR\n\
that start with FILE.\n\
If there is only one and FILE matches it exactly, returns t.\n\
Returns nil if DIR contains no name starting with FILE.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  /* Don't waste time trying to complete a null string.
     Besides, this case happens when user is being asked for
     a directory name and has supplied one ending in a /.
     We would not want to add anything in that case
     even if there are some unique characters in that directory.  */
  if (XTYPE (file) == Lisp_String && XSTRING (file)->size == 0)
    return file;
  return file_name_completion (file, dirname, 0, 0);
}

DEFUN ("file-name-all-completions", Ffile_name_all_completions,
  Sfile_name_all_completions, 2, 2, 0,
  "Return a list of all completions of file name FILE in directory DIR.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  return file_name_completion (file, dirname, 1, 0);
}

#ifdef VMS

DEFUN ("file-name-all-versions", Ffile_name_all_versions,
  Sfile_name_all_versions, 2, 2, 0,
  "Return a list of all versions of file name FILE in directory DIR.")
  (file, dirname)
     Lisp_Object file, dirname;
{
  return file_name_completion (file, dirname, 1, 1);
}

#endif /* VMS */

Lisp_Object
file_name_completion (file, dirname, all_flag, ver_flag)
     Lisp_Object file, dirname;
     int all_flag, ver_flag;
{
  DIR *d;
  DIRENTRY *dp;
  int bestmatchsize, skip;
  register int compare, matchsize;
  unsigned char *p1, *p2;
  int matchcount = 0;
  Lisp_Object bestmatch, tem, elt, name;
  struct stat st;
  int directoryp;
  int passcount;
  int count = specpdl_ptr - specpdl;
#ifdef VMS
  extern DIRENTRY * readdirver ();

  DIRENTRY *((* readfunc) ());

  /* Filename completion on VMS ignores case, since VMS filesys does.  */
  specbind (Qcompletion_ignore_case, Qt);

  readfunc = readdir;
  if (ver_flag)
    readfunc = readdirver;
  file = Fupcase (file);
#endif /* VMS */

  CHECK_STRING (file, 0);

  dirname = Fexpand_file_name (dirname, Qnil);
  bestmatch = Qnil;

  /* passcount = 0, ignore files that end in an ignored extension.
     If nothing found then try again with passcount = 1, don't ignore them.
     If looking for all completions, start with passcount = 1,
     so always take even the ignored ones.  */
  for (passcount = !!all_flag; NULL (bestmatch) && passcount < 2; passcount++)
    {
      if (!(d = opendir (XSTRING (Fdirectory_file_name (dirname))->data)))
	report_file_error ("Opening directory", Fcons (dirname, Qnil));

      /* Loop reading blocks */
      /* (att3b compiler bug requires do a null comparison this way) */
      while (1)
	{
	  DIRENTRY *dp;
	  int len;

#ifdef VMS
	  dp = (*readfunc) (d);
#else
	  dp = readdir (d);
#endif
	  if (!dp) break;

	  len = NAMLEN (dp);

	  if (!NULL (Vquit_flag) && NULL (Vinhibit_quit))
	    goto quit;
	  if (!dp->d_ino
	      || len < XSTRING (file)->size
	      || 0 <= scmp (dp->d_name, XSTRING (file)->data,
			    XSTRING (file)->size))
	    continue;

          if (file_name_completion_stat (dirname, dp, &st) < 0)
            continue;

          directoryp = ((st.st_mode & S_IFMT) == S_IFDIR);
	  tem = Qnil;
          if (!directoryp)
            {
	      /* Compare extensions-to-be-ignored against end of this file name */
	      /* if name is not an exact match against specified string */
	      if (!passcount && len > XSTRING (file)->size)
		/* and exit this for loop if a match is found */
		for (tem = Vcompletion_ignored_extensions;
		     CONSP (tem); tem = XCONS (tem)->cdr)
		  {
		    elt = XCONS (tem)->car;
		    if (XTYPE (elt) != Lisp_String) continue;
		    skip = len - XSTRING (elt)->size;
		    if (skip < 0) continue;

		    if (0 <= scmp (dp->d_name + skip,
				   XSTRING (elt)->data,
				   XSTRING (elt)->size))
		      continue;
		    break;
		  }
	    }

	  /* Unless an ignored-extensions match was found,
             process this name as a completion */
	  if (passcount || !CONSP (tem))
	    {
	      /* Update computation of how much all possible completions match */

	      matchcount++;

	      if (all_flag || NULL (bestmatch))
		{
		  /* This is a possible completion */
		  if (directoryp)
		    {
		      /* This completion is a directory; make it end with '/' */
		      name = Ffile_name_as_directory (make_string (dp->d_name, len));
		    }
		  else
		    name = make_string (dp->d_name, len);
		  if (all_flag)
		    {
		      bestmatch = Fcons (name, bestmatch);
		    }
		  else
		    {
		      bestmatch = name;
		      bestmatchsize = XSTRING (name)->size;
		    }
		}
	      else
		{
		  compare = min (bestmatchsize, len);
		  p1 = XSTRING (bestmatch)->data;
		  p2 = (unsigned char *) dp->d_name;
		  matchsize = scmp(p1, p2, compare);
		  if (matchsize < 0)
		    matchsize = compare;
		  /* If this dirname all matches,
		     see if implicit following slash does too.  */
		  if (directoryp
		      && compare == matchsize
		      && bestmatchsize > matchsize
		      && p1[matchsize] == '/')
		    matchsize++;
		  bestmatchsize = min (matchsize, bestmatchsize);
		}
	    }
	}
      closedir (d);
    }

  unbind_to (count);

  if (all_flag || NULL (bestmatch))
    return bestmatch;
  if (matchcount == 1 && bestmatchsize == XSTRING (file)->size)
    return Qt;
  return Fsubstring (bestmatch, make_number (0), make_number (bestmatchsize));
 quit:
  if (d) closedir (d);
  Vquit_flag = Qnil;
  return Fsignal (Qquit, Qnil);
}

file_name_completion_stat (dirname, dp, st_addr)
     Lisp_Object dirname;
     DIRENTRY *dp;
     struct stat *st_addr;
{
  int len = NAMLEN (dp);
  int pos = XSTRING (dirname)->size;
  char *fullname = (char *) alloca (len + pos + 2);

  bcopy (XSTRING (dirname)->data, fullname, pos);
#ifndef VMS
  if (fullname[pos - 1] != '/')
    fullname[pos++] = '/';
#endif

  bcopy (dp->d_name, fullname + pos, len);
  fullname[pos + len] = 0;

  return stat (fullname, st_addr);
}

Lisp_Object
make_time (time)
     int time;
{
  return Fcons (make_number (time >> 16),
		Fcons (make_number (time & 0177777), Qnil));
}

DEFUN ("file-attributes", Ffile_attributes, Sfile_attributes, 1, 1, 0,
  "Return a list of attributes of file FILENAME.\n\
Value is nil if specified file cannot be opened.\n\
Otherwise, list elements are:\n\
 0. t for directory, string (name linked to) for symbolic link, or nil.\n\
 1. Number of links to file.\n\
 2. File uid.\n\
 3. File gid.\n\
 4. Last access time, as a list of two integers.\n\
  First integer has high-order 16 bits of time, second has low 16 bits.\n\
 5. Last modification time, likewise.\n\
 6. Last status change time, likewise.\n\
 7. Size in bytes.\n\
 8. File modes, as a string of ten letters or dashes as in ls -l.\n\
 9. t iff file's gid would change if file were deleted and recreated.\n\
10. inode number.\n\
\n\
If file does not exists, returns nil.")
  (filename)
     Lisp_Object filename;
{
  Lisp_Object values[11];
  Lisp_Object dirname;
  struct stat s;
  struct stat sdir;
  char modes[10];

  filename = Fexpand_file_name (filename, Qnil);
  if (lstat (XSTRING (filename)->data, &s) < 0)
    return Qnil;

  switch (s.st_mode & S_IFMT)
    {
    default:
      values[0] = Qnil; break;
    case S_IFDIR:
      values[0] = Qt; break;
#ifdef S_IFLNK
    case S_IFLNK:
      values[0] = Ffile_symlink_p (filename); break;
#endif
    }
  values[1] = make_number (s.st_nlink);
  values[2] = make_number (s.st_uid);
  values[3] = make_number (s.st_gid);
  values[4] = make_time (s.st_atime);
  values[5] = make_time (s.st_mtime);
  values[6] = make_time (s.st_ctime);
  /* perhaps we should set this to most-positive-fixnum if it is too large? */
  values[7] = make_number ((int)s.st_size);
  filemodestring (&s, modes);
  values[8] = make_string (modes, 10);
#ifdef BSD4_3 /* Gross kludge to avoid lack of "#if defined(...)" in VMS */
#define BSD4_2 /* A new meaning to the term `backwards compatability' */
#endif
#ifdef BSD4_2			/* file gid will be dir gid */
  dirname = Ffile_name_directory (filename);
  if (dirname != Qnil && stat (XSTRING (dirname)->data, &sdir) == 0)
    values[9] = (sdir.st_gid != s.st_gid) ? Qt : Qnil;
  else					/* if we can't tell, assume worst */
    values[9] = Qt;
#else					/* file gid will be egid */
  values[9] = (s.st_gid != getegid ()) ? Qt : Qnil;
#endif	/* BSD4_2 (or BSD4_3) */
#ifdef BSD4_3
#undef BSD4_2 /* ok, you can look again without throwing up */
#endif
  values[10] = make_number (s.st_ino);
  return Flist (11, values);
}

syms_of_dired ()
{
  defsubr (&Sdirectory_files);
  defsubr (&Sfile_name_completion);
#ifdef VMS
  defsubr (&Sfile_name_all_versions);
#endif /* VMS */
  defsubr (&Sfile_name_all_completions);
  defsubr (&Sfile_attributes);

#ifdef VMS
  Qcompletion_ignore_case = intern ("completion-ignore-case");
  staticpro (&Qcompletion_ignore_case);
#endif /* VMS */

  DEFVAR_LISP ("completion-ignored-extensions", &Vcompletion_ignored_extensions,
    "*Completion ignores filenames ending in any string in this list.");
  Vcompletion_ignored_extensions = Qnil;
}
