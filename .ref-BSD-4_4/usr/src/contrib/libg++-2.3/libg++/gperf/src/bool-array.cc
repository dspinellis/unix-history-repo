/* Fast lookup table abstraction implemented as an Iteration Number Array
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
#include "options.h"
#include "bool-array.h"

STORAGE_TYPE * Bool_Array::storage_array;
STORAGE_TYPE Bool_Array::iteration_number;
int Bool_Array::size;

/* Prints out debugging diagnostics. */

Bool_Array::~Bool_Array (void) 
{
  T (Trace t ("Bool_Array::~Bool_Array");)
  if (option[DEBUG])
    fprintf (stderr, "\ndumping boolean array information\n"
             "size = %d\niteration number = %d\nend of array dump\n",
             size, iteration_number);
}

#ifndef __OPTIMIZE__

Bool_Array::Bool_Array (void)
{
  T (Trace t ("Bool_Array::Bool_Array");)
  storage_array = 0;
  iteration_number = size = 0;
}

void
Bool_Array::init (STORAGE_TYPE *buffer, STORAGE_TYPE s)
{
  T (Trace t ("Bool_Array::init");)
  size             = s;
  iteration_number = 1;
  storage_array    = buffer;
  bzero (storage_array, s * sizeof *storage_array);
  if (option[DEBUG])
    fprintf (stderr, "\nbool array size = %d, total bytes = %d\n",
             size, size * sizeof *storage_array);
}

int  
Bool_Array::find (int index) 
{
  T (Trace t ("Bool_Array::find");)
  if (storage_array[index] == iteration_number)
    return 1;
  else
    {
      storage_array[index] = iteration_number;
      return 0;
    }
}

void 
Bool_Array::reset (void)  
{
  T (Trace t ("Bool_Array::reset");)
  /* If we wrap around it's time to zero things out again!  However, this only
     occurs once about every 2^31 or 2^15 iterations, so it should probably
     never happen! */

  if (++iteration_number == 0)
    {
      if (option[DEBUG])
        {
          fprintf (stderr, "(re-initializing bool_array)...");
          fflush (stderr);
        }
      iteration_number = 1;
      bzero (storage_array, size * sizeof *storage_array);
      if (option[DEBUG])
        {
          fprintf (stderr, "done\n");
          fflush (stderr);
        }
    }
}
#endif /* not defined __OPTIMIZE__ */
