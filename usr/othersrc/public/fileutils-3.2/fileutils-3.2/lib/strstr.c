/* strstr.c -- return the offset of one string within another
   Copyright (C) 1989, 1990 Free Software Foundation, Inc.

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

/* Author:
	Mike Rendell			Department of Computer Science
	michael@garfield.mun.edu	Memorial University of Newfoundland
	..!uunet!garfield!michael	St. John's, Nfld., Canada
	(709) 737-4550			A1C 5S7
*/

/* Return the starting address of string S2 in S1;
   return 0 if it is not found. */

char *
strstr (s1, s2)
     char *s1;
     char *s2;
{
  int i;
  char *p1;
  char *p2;
  char *s = s1;

  for (p2 = s2, i = 0; *s; p2 = s2, i++, s++)
    {
      for (p1 = s; *p1 && *p2 && *p1 == *p2; p1++, p2++)
	;
      if (!*p2)
	break;
    }
  if (!*p2)
    return s1 + i;

  return 0;
}
