/* Copyright (C) 1989, 1990 Free Software Foundation, Inc.
     Written by James Clark (jjc@jclark.uucp)

This file is part of groff.

groff is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 1, or (at your option) any later
version.

groff is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with groff; see the file LICENSE.  If not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA. */

#include "ptable.h"

unsigned hash_string(const char *s)
{
  assert(s != 0);
  unsigned h = 0, g;
  while (*s != 0) {
    h <<= 4;
    h += *s++;
    if ((g = h & 0xf0000000) != 0) {
      h ^= g >> 24;
      h ^= g;
    }
  }
  return h;
}

static const int table_sizes[] = { 
101, 503, 1009, 2003, 3001, 4001, 5003, 10007, 20011, 40009,
80021, 160001, 500009, 1000003, 2000003, 4000037, 8000009,
16000057, 32000011, 64000031, 128000003, 0 
};

int next_ptable_size(int n)
{
  for (const int *p = table_sizes; *p <= n && *p != 0; p++)
    ;
  assert(*p != 0);
  return *p;
}
