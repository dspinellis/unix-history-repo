/* xgetcwd.c -- return current directory with unlimited length
   Copyright (C) 1992 Free Software Foundation, Inc.

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

/* Written by David MacKenzie, djm@gnu.ai.mit.edu. */

#include <errno.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#if defined(_POSIX_VERSION) || defined(STDC_HEADERS) || defined(HAVE_LIMITS_H)
#include <limits.h>
#endif /* !_POSIX_VERSION && !STDC_HEADERS && !HAVE_LIMITS_H */

#ifndef _POSIX_SOURCE
#include <sys/param.h>
#endif

#ifndef _POSIX_PATH_MAX
#define _POSIX_PATH_MAX 255
#endif

#ifndef PATH_MAX
#ifdef _PC_PATH_MAX
#define PATH_MAX pathconf ("/", _PC_PATH_MAX)
#else /* !_PC_PATH_MAX */
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else /* not MAXPATHLEN */
#define PATH_MAX _POSIX_PATH_MAX
#endif /* MAXPATHLEN */
#endif /* !_PC_PATH_MAX */
#endif /* !PATH_MAX */

#if !defined(_POSIX_VERSION) && !defined(USG)
char *getwd ();
#define getcwd(buf, max) getwd (buf)
#else
char *getcwd ();
#endif

/* Amount to increase buffer size by in each try. */
#define PATH_INCR 32

char *xmalloc ();
char *xrealloc ();

/* Return the current directory, newly allocated, arbitrarily long.
   Return NULL and set errno on error. */

char *
xgetcwd ()
{
  char *cwd;
  char *ret;
  long path_max;

  errno = 0;
#ifdef _POSIX_VERSION
  path_max = PATH_MAX;
  if (path_max < 0)
    {
      if (errno)
	return NULL;
      path_max = _POSIX_PATH_MAX;
    }
#else /* !_POSIX_VERSION */
  path_max = PATH_MAX;
#endif /* !_POSIX_VERSION */
  path_max += 2;		/* The getcwd docs say to do this. */

  cwd = (char *) xmalloc (path_max);

  errno = 0;
  while ((ret = getcwd (cwd, path_max)) == NULL && errno == ERANGE)
    {
      path_max += PATH_INCR;
      cwd = xrealloc (cwd, path_max);
      errno = 0;
    }

  if (ret == NULL)
    {
      int save_errno = errno;
      free (cwd);
      errno = save_errno;
      return NULL;
    }
  return cwd;
}
