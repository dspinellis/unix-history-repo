/* 
Copyright (C) 1990 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.  
*/

#ifdef __GNUG__
#pragma implementation
#endif
#include <builtin.h>

/*
 some useful hash functions
*/

unsigned int hashpjw(const char* x) // From Dragon book, p436
{
  unsigned int h = 0;
  unsigned int g;

  while (*x != 0)
  {
    h = (h << 4) + *x++;
    if ((g = h & 0xf0000000) != 0)
      h = (h ^ (g >> 24)) ^ g;
  }
  return h;
}

unsigned int multiplicativehash(int x)
{
  // uses a const close to golden ratio * pow(2,32)
  return ((unsigned)x) * 2654435767;
}


unsigned int foldhash(double x)
{
  union { unsigned int i[2]; double d; } u;
  u.d = x;
  unsigned int u0 = u.i[0];
  unsigned int u1 = u.i[1]; 
  return u0 ^ u1;
}

