/* makepath.c -- Ensure that a directory path exists.
   Copyright (C) 1990 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* Written by David MacKenzie <djm@ai.mit.edu> and
   Jim Meyering <meyering@cs.utexas.edu>. */

#ifdef __GNUC__
#define alloca __builtin_alloca
#else
#ifdef sparc
#include <alloca.h>
#else
#ifdef _AIX
 #pragma alloca
#else
char *alloca ();
#endif
#endif
#endif

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#if !defined(S_ISDIR) && defined(S_IFDIR)
#define	S_ISDIR(m) (((m) & S_IFMT) == S_IFDIR)
#endif

#ifdef STDC_HEADERS
#include <errno.h>
#ifdef alloca
# undef alloca
# include <stdlib.h>
# define alloca __builtin_alloca
#else
# include <stdlib.h>
#endif /* alloca */
#else
extern int errno;
#endif

#if defined(USG) || defined(STDC_HEADERS)
#include <string.h>
#define index strchr
#else
#include <strings.h>
#endif

void error ();

/* Ensure that the directory ARGPATH exists.
   Remove any trailing slashes from ARGPATH before calling this function.

   Make any leading directories that don't already exist, with
   permissions PARENT_MODE.
   If the last element of ARGPATH does not exist, create it as
   a new directory with permissions MODE.
   If OWNER and GROUP are non-negative, make them the UID and GID of
   created directories.
   If VERBOSE_FMT_STRING is nonzero, use it as a printf format
   string for printing a message after successfully making a directory,
   with the name of the directory that was just made as an argument.

   Return 0 if ARGPATH exists as a directory with the proper
   ownership and permissions when done, otherwise 1. */

int
make_path (argpath, mode, parent_mode, owner, group, verbose_fmt_string)
     char *argpath;
     int mode;
     int parent_mode;
     int owner;
     int group;
     char *verbose_fmt_string;
{
  char *dirpath;		/* A copy we can scribble NULs on. */
  struct stat stats;
  int retval = 0;
  int oldmask = umask (0);

  dirpath = alloca (strlen (argpath) + 1);
  strcpy (dirpath, argpath);

  if (stat (dirpath, &stats))
    {
      char *slash;
      int tmp_mode;		/* Initial perms for leading dirs. */
      int re_protect;		/* Should leading dirs be unwritable? */
      struct ptr_list
      {
	char *dirname_end;
	struct ptr_list *next;
      };
      struct ptr_list *p, *leading_dirs = NULL;

      /* If leading directories shouldn't be writable or executable,
	 or should have set[ug]id or sticky bits set and we are setting
	 their owners, we need to fix their permissions after making them. */
      if (((parent_mode & 0300) != 0300)
	  || (owner >= 0 && group >= 0 && (parent_mode & 07000) != 0))
	{
	  tmp_mode = 0700;
	  re_protect = 1;
	}
      else
	{
	  tmp_mode = parent_mode;
	  re_protect = 0;
	}

      slash = dirpath;
      while (*slash == '/')
	slash++;
      while (slash = index (slash, '/'))
	{
	  *slash = '\0';
	  if (stat (dirpath, &stats))
	    {
	      if (mkdir (dirpath, tmp_mode))
		{
		  error (0, errno, "cannot make directory `%s'", dirpath);
		  umask (oldmask);
		  return 1;
		}
	      else
		{
		  if (verbose_fmt_string != NULL)
		    error (0, 0, verbose_fmt_string, dirpath);

		  if (owner >= 0 && group >= 0
		      && chown (dirpath, owner, group))
		    {
		      error (0, errno, "%s", dirpath);
		      retval = 1;
		    }
		  if (re_protect)
		    {
		      struct ptr_list *new = alloca (sizeof (struct ptr_list));
		      new->dirname_end = slash;
		      new->next = leading_dirs;
		      leading_dirs = new;
		    }
		}
	    }
	  else if (!S_ISDIR (stats.st_mode))
	    {
	      error (0, 0, "`%s' exists but is not a directory", dirpath);
	      umask (oldmask);
	      return 1;
	    }

	  *slash++ = '/';

	  /* Avoid unnecessary calls to `stat' when given
	     pathnames containing multiple adjacent slashes.  */
	  while (*slash == '/')
	    slash++;
	}

      /* We're done making leading directories.
	 Make the final component of the path.  */

      if (mkdir (dirpath, mode))
	{
	  error (0, errno, "cannot make directory `%s'", dirpath);
	  umask (oldmask);
	  return 1;
	}
      if (verbose_fmt_string != NULL)
	error (0, 0, verbose_fmt_string, dirpath);

      if (owner >= 0 && group >= 0)
	{
	  if (chown (dirpath, owner, group))
	    {
	      error (0, errno, "%s", dirpath);
	      retval = 1;
	    }
	  /* chown may have turned off some permission bits we wanted. */
	  if ((mode & 07000) != 0 && chmod (dirpath, mode))
	    {
	      error (0, errno, "%s", dirpath);
	      retval = 1;
	    }
	}

      /* If the mode for leading directories didn't include owner "wx"
	 privileges, we have to reset their protections to the correct
	 value.  */
      for (p = leading_dirs; p != NULL; p = p->next)
	{
	  *(p->dirname_end) = '\0';
	  if (chmod (dirpath, parent_mode))
	    {
	      error (0, errno, "%s", dirpath);
	      retval = 1;
	    }
	}
    }
  else
    {
      /* We get here if the entire path already exists. */

      if (!S_ISDIR (stats.st_mode))
	{
	  error (0, 0, "`%s' exists but is not a directory", dirpath);
	  umask (oldmask);
	  return 1;
	}

      /* chown must precede chmod because on some systems,
	 chown clears the set[ug]id bits for non-superusers,
	 resulting in incorrect permissions.
	 On System V, users can give away files with chown and then not
	 be able to chmod them.  So don't give files away.  */

      if (owner >= 0 && group >= 0 && chown (dirpath, owner, group))
	{
	  error (0, errno, "%s", dirpath);
	  retval = 1;
	}
      if (chmod (dirpath, mode))
	{
	  error (0, errno, "%s", dirpath);
	  retval = 1;
	}
    }

  umask (oldmask);
  return retval;
}
