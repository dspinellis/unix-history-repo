/* Copyright (C) 1991 Free Software Foundation, Inc.
This file is part of the GNU C Library.

The GNU C Library is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

The GNU C Library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with the GNU C Library; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <errno.h>
#include <limits.h>
#include <stddef.h>
#include <dirent.h>
#include <unistd.h>
#include <sys/types.h>
#include "maxpath.h"
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>

#if !defined (PATH_MAX)
#  if defined (_POSIX_PATH_MAX)
#    define PATH_MAX _POSIX_PATH_MAX
#  else
#    if defined (MAXPATHLEN)
#      define PATH_MAX MAXPATHLEN
#    else
#      define PATH_MAX 255
#    endif
#  endif
#endif

/* Get the pathname of the current working directory,
   and put it in SIZE bytes of BUF.  Returns NULL if the
   directory couldn't be determined or SIZE was too small.
   If successful, returns BUF.  In GNU, if BUF is NULL,
   an array is allocated with `malloc'; the array is SIZE
   bytes long, unless SIZE <= 0, in which case it is as
   big as necessary.  */
#ifdef __STDC__
char *getcwd(char *buf, int size) /* from <sys/unistd.h> */
#else
char *
getcwd (buf, size)
     char *buf;
     size_t size;
#endif
{
  dev_t rootdev, thisdev;
  ino_t rootino, thisino;
  char path[PATH_MAX + 1];
  register char *pathp;
  struct stat st;

  pathp = &path[sizeof (path)];
  *--pathp = '\0';

  if (stat (".", &st) < 0)
    return (NULL);

  thisdev = st.st_dev;
  thisino = st.st_ino;

  if (stat ("/", &st) < 0)
    return (NULL);

  rootdev = st.st_dev;
  rootino = st.st_ino;

  while (!(thisdev == rootdev && thisino == rootino))
    {
      register DIR *dirstream;
      register struct dirent *d;
      dev_t dotdev;
      ino_t dotino;
      char mount_point;

      /* Move up a directory.  */
      if (chdir ("..") < 0)
	{
	  if (pathp != &path[sizeof (path) - 1])
	    {
	      /* Try to get back to the original directory.
		 This is the only place where this is possible.  */
	      int save = errno;
	      (void) chdir (pathp);
	      errno = save;
	    }
	  return (NULL);
	}

      /* Figure out if this directory is a mount point.  */
      if (stat(".", &st) < 0)
	return (NULL);

      dotdev = st.st_dev;
      dotino = st.st_ino;
      mount_point = (dotdev != thisdev);

      /* Search for the last directory.  */
      dirstream = opendir(".");
      if (dirstream == NULL)
	return (NULL);

      while ((d = readdir(dirstream)) != NULL)
	{
	  if (d->d_name[0] == '.' &&
	      (d->d_name[1] == 0 ||
	       (d->d_name[2] == 0 && d->d_name[1] == '.')))
	    continue;

	  /* if (mount_point || d->d_fileno == thisino) */
	  if (mount_point || d->d_ino == thisino)
	    {
	      if (stat(d->d_name, &st) < 0)
		{
		  int save = errno;
		  (void) closedir(dirstream);
		  errno = save;
		  return (NULL);
		}

	      if (st.st_dev == thisdev && st.st_ino == thisino)
		break;
	    }
	}

      if (d == NULL)
	{
	  int save = errno;
	  (void) closedir (dirstream);
	  errno = save;
	  return (NULL);
	}
      else
	{
	  int d_namlen=strlen(d->d_name); /* new line */
	  pathp -= d_namlen;
	  (void) memcpy(pathp, d->d_name, d_namlen);
	  *--pathp = '/';
	  (void) closedir (dirstream);
	}

      thisdev = dotdev;
      thisino = dotino;
    }

  if (pathp == &path[sizeof (path) - 1])
    *--pathp = '/';

  if (chdir (pathp) < 0)
    return (NULL);

  {
    size_t len = &path[sizeof (path)] - pathp;
    if (buf == NULL)
      {
	if (len < (size_t) size)
	  len = size;
	buf = (char *) malloc (len);
	if (buf == NULL)
	  return (NULL);
      }
    else if ((size_t) size < len)
      {
	errno = ERANGE;
	return (NULL);
      }
    (void) memcpy ((char *) buf, (char *) pathp, len);
  }

  return (buf);
}
