/* linebuffer.c -- read arbitrarily long lines
   Copyright (C) 1986, 1991 Free Software Foundation, Inc.

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

/* Written by Richard Stallman. */

#include <stdio.h>
#include "linebuffer.h"

char *xmalloc ();
char *xrealloc ();
void free ();

/* Initialize linebuffer LINEBUFFER for use. */

void
initbuffer (linebuffer)
     struct linebuffer *linebuffer;
{
  linebuffer->length = 0;
  linebuffer->size = 200;
  linebuffer->buffer = (char *) xmalloc (linebuffer->size);
}

/* Read an arbitrarily long line of text from STREAM into LINEBUFFER.
   Remove any newline.  Does not null terminate.
   Return LINEBUFFER, except at end of file return 0.  */

struct linebuffer *
readline (linebuffer, stream)
     struct linebuffer *linebuffer;
     FILE *stream;
{
  int c;
  char *buffer = linebuffer->buffer;
  char *p = linebuffer->buffer;
  char *end = buffer + linebuffer->size; /* Sentinel. */

  if (feof (stream))
    {
      linebuffer->length = 0;
      return 0;
    }

  while (1)
    {
      c = getc (stream);
      if (p == end)
	{
	  linebuffer->size *= 2;
	  buffer = (char *) xrealloc (buffer, linebuffer->size);
	  p += buffer - linebuffer->buffer;
	  linebuffer->buffer = buffer;
	  end = buffer + linebuffer->size;
	}
      if (c == EOF || c == '\n')
	break;
      *p++ = c;
    }

  if (feof (stream) && p == buffer)
    {
      linebuffer->length = 0;
      return 0;
    }
  linebuffer->length = p - linebuffer->buffer;
  return linebuffer;
}

/* Free linebuffer LINEBUFFER and its data, all allocated with malloc. */

void
freebuffer (linebuffer)
     struct linebuffer *linebuffer;
{
  free (linebuffer->buffer);
  free (linebuffer);
}
