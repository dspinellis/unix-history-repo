/* Portable version of bzero for systems without it.
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

	bzero -- zero the contents of a specified memory region

SYNOPSIS

	void bzero (char *to, int count)

DESCRIPTION

	Zero COUNT bytes of memory pointed to by TO.

BUGS

	Significant speed enhancements may be made in some environments
	by zeroing more than a single byte at a time, or by unrolling the
	loop.

*/


void
bzero (to, count)
  char *to;
  int count;
{
  while (count-- > 0)
    {
      *to++ = 0;
    }
}
