/* Emulate getcwd using getwd.
   Copyright 1991 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

/*

NAME

	getcwd -- get absolute pathname for current working directory

SYNOPSIS

	char *getcwd (char pathname[len], len)

DESCRIPTION

	Copy the absolute pathname for the current working directory into
	the supplied buffer and return a pointer to the buffer.  If the 
	current directory's path doesn't fit in LEN characters, the result
	is NULL and errno is set.

BUGS

	Emulated via the getwd() call, which is reasonable for most
	systems that do not have getcwd().

*/

#include <sys/param.h>
#include <errno.h>

extern char *getwd ();
extern int errno;

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024
#endif

char *
getcwd (buf, len)
  char *buf;
  int len;
{
  char ourbuf[MAXPATHLEN];
  char *result;

  result = getwd (ourbuf);
  if (result) {
    if (strlen (ourbuf) >= len) {
      errno = ERANGE;
      return 0;
    }
    strcpy (buf, ourbuf);
  }
  return buf;
}
