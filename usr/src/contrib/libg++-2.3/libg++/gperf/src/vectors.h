#include <stdio.h>

/* This may look like C code, but it is really -*- C++ -*- */

/* Static class data members that are shared between several classes via
   inheritance.

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

#ifndef vectors_h
#define vectors_h 1

static const int ALPHA_SIZE = 128;

struct Vectors
{
  static int   occurrences[ALPHA_SIZE]; /* Counts occurrences of each key set character. */
  static int   asso_values[ALPHA_SIZE]; /* Value associated with each character. */
};

#endif
