/* memcmp -- compare two memory regions.
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

	memcmp -- compare two memory regions

SYNOPSIS

	int memcmp (const void *from, const void *to, size_t count)

DESCRIPTION

	Compare two memory regions and return less than,
	equal to, or greater than zero, according to lexicographical
	ordering of the compared regions.
*/

#include <ansidecl.h>
#ifdef __STDC__
#include <stddef.h>
#else
#define size_t unsigned long
#endif

int
DEFUN(memcmp, (str1, str2, count),
      PTRCONST str1 AND PTRCONST str2 AND size_t count)
{
  register unsigned char *s1 = (unsigned char*)str1;
  register unsigned char *s2 = (unsigned char*)str2;

  while (count-- > 0)
    {
      if (*s1++ != *s2++)
	  return s1[-1] < s2[-1] ? -1 : 1;
    }
  return 0;
}

