// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
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
#include "<T>.Bag.h"

// error handling

void <T>Bag::error(const char* msg)
{
  (*lib_error_handler)("Bag", msg);
}


Pix <T>Bag::seek(<T&> item, Pix i)
{
  if (i == 0) 
    i = first();
  else
    next(i);
  for (;i != 0 && (!(<T>EQ((*this)(i), item))); next(i));
  return i;
}

int <T>Bag::owns(Pix p)
{
  if (p == 0) return 0;
  for (Pix i = first(); i != 0; next(i)) if (i == p) return 1;
  return 0;
}

void <T>Bag::remove(<T&> item)
{
  int i = nof(item);
  while (i-- > 0) del(item);
}


int <T>Bag::nof(<T&> item)
{
  int n = 0;
  for (Pix p = first(); p; next(p)) if (<T>EQ((*this)(p), item)) ++n;
  return n;
}

void <T>Bag::clear()
{
  Pix i = first(); 
  while (i != 0)
  {
    del((*this)(i));
    i = first();
  }
}


