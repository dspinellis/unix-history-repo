/* Simple implementation of strstr for systems without it.
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

	strstr -- locate first occurance of a substring

SYNOPSIS

	#include <string.h>

	char *strstr (char *s1, char *s2)

DESCRIPTION

	Locates the first occurance in the string pointed to by S1 of
	the string pointed to by S2.  Returns a pointer to the substring
	found, or a NULL pointer if not found.  If S2 points to a string
	with zero length, the function returns S1.
	
BUGS

*/


/* FIXME:  The above description is ANSI compiliant.  This routine has not
   been validated to comply with it.  -fnf */

char *
strstr (s1, s2)
  char *s1, *s2;
{
  register char *p = s1;
  extern char *strchr ();
  extern int strncmp ();
#if __GNUC__==2
  extern __SIZE_TYPE__ strlen ();
#endif
  register int len = strlen (s2);

  for (; (p = strchr (p, *s2)) != 0; p++)
    {
      if (strncmp (p, s2, len) == 0)
	{
	  return (p);
	}
    }
  return (0);
}
