/* Portable version of strchr()
   Copyright (C) 1991 Free Software Foundation, Inc.

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

	strchr -- return pointer to first occurance of a character

SYNOPSIS

	char *strchr (const char *s, int c)

DESCRIPTION

	Returns a pointer to the first occurance of character C in
	string S, or a NULL pointer if no occurance is found.
	
BUGS

	Behavior when character is the null character is implementation
	dependent.

*/

#include <ansidecl.h>

char *
strchr (s, c)
  register CONST char *s;
  int c;
{
  do {
    if (*s == c)
      {
	return (s);
      }
  } while (*s++);
  return (0);
}
