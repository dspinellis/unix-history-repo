/* This may look like C code, but it is really -*- C++ -*- */

/* Simple lookup table abstraction implemented as an Iteration Number Array.

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

/* Define and implement a simple boolean array abstraction,
   uses an Iteration Numbering implementation to save on initialization time. */

#ifndef bool_array_h
#define bool_array_h 1

#include <std.h>
#include "trace.h"

#ifdef LO_CAL
/* If we are on a memory diet then we'll only make these use a limited
   amount of storage space. */
typedef unsigned short STORAGE_TYPE;
#else
typedef int STORAGE_TYPE;
#endif

class Bool_Array 
{
private:
  static STORAGE_TYPE *storage_array;    /* Initialization of the index space. */
  static STORAGE_TYPE  iteration_number; /* Keep track of the current iteration. */
  static int           size;             /* Keep track of array size. */

public:
       Bool_Array (void);
      ~Bool_Array (void);
  static void init (STORAGE_TYPE *buffer, STORAGE_TYPE s);
  static int  find (int hash_value);
  static void reset (void);
};

#ifdef __OPTIMIZE__  /* efficiency hack! */

inline 
Bool_Array::Bool_Array (void)
{
  T (Trace t ("Bool_Array::Bool_Array");)
  storage_array = 0;
  iteration_number = size = 0;
}

inline void
Bool_Array::init (STORAGE_TYPE *buffer, STORAGE_TYPE s)
{
  T (Trace t ("Bool_Array::init");)
  size             = s;
  iteration_number = 1;
  storage_array    = buffer;
  memset (storage_array, 0, s * sizeof *storage_array);
  if (option[DEBUG])
    fprintf (stderr, "\nbool array size = %d, total bytes = %d\n",
             size, size * sizeof *storage_array);
}

inline int  
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

inline void 
Bool_Array::reset (void) 
{ 
  T (Trace t ("Bool_Array::reset");)
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
#endif

#endif
