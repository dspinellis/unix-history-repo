/* stpcpy.c -- copy a string and return pointer to end of new string
    Copyright (C) 1989, 1990 Free Software Foundation.

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
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

/* Copy SOURCE into DEST, stopping after copying the first '\0', and
   return a pointer to the '\0' at the end of DEST;
   in other words, return DEST + strlen (SOURCE). */

char *
stpcpy (dest, source)
     char *dest;
     char *source;
{
  while ((*dest++ = *source++) != '\0')
    /* Do nothing. */ ;
  return dest - 1;
}
