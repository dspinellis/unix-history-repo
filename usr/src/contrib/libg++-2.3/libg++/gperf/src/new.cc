/* Defines a buffered memory allocation abstraction that reduces calls to
   malloc.
   Copyright (C) 1989 Free Software Foundation, Inc.
   written by Douglas C. Schmidt (schmidt@ics.uci.edu)

This file is part of GNU GPERF.

GNU GPERF is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 1, or (at your option)
any later version.

GNU GPERF is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU GPERF; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include <stdio.h>
#include <std.h>
#include "std-err.h"
#include "trace.h"

/* Determine default alignment.  If your C++ compiler does not
   like this then try something like #define DEFAULT_ALIGNMENT 8. */
struct fooalign {char x; double d;};
const int ALIGNMENT = ((char *)&((struct fooalign *) 0)->d - (char *)0);

int Trace::nesting;

/* Provide an abstraction that cuts down on the number of
   calls to NEW by buffering the memory pool from which
   strings are allocated. */

void *
operator new (long size)
{
  T (Trace t ("operator new");)
  static char *buf_start = 0;          /* Large array used to reduce calls to NEW. */
  static char *buf_end = 0;            /* Indicates end of BUF_START. */
  static int   buf_size = 4 * BUFSIZ; /* Size of buffer pointed to by BUF_START. */
         char *temp;

  /* Align this on correct boundaries, just to be safe... */
  size = ((size + ALIGNMENT - 1) / ALIGNMENT) * ALIGNMENT;

  /* If we are about to overflow our buffer we'll just grab another
     chunk of memory.  Since we never free the original memory it
     doesn't matter that no one points to the beginning of that
     chunk. Note we use a heuristic that grows the buffer either by
     size of the request or by twice the previous size, whichever is
     larger. */
  
  if (buf_start + size >= buf_end)
    {
      buf_size *= 2;
      if (buf_start = (char *)malloc (buf_size >?= size))
        buf_end = buf_start + buf_size;
      else
        Std_Err::report_error ("Virtual memory failed at %s, %s in function %s\n%a", __FILE__, __LINE__, "operator new", 1);
    }

  temp = buf_start;
  buf_start += size;
  return temp;
}

/* We need this deletion operator in order to make the linker happy. */

void 
operator delete (void *ptr)
{
  T (Trace t ("operator delete");)
  free ((char *) ptr);
}
