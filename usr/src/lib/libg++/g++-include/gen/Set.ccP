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
#include "<T>.Set.h"


Pix <T>Set::seek(<T&> item)
{
  for (Pix i = first(); i != 0 && !(<T>EQ((*this)(i), item)); next(i));
  return i;
}

int <T>Set::owns(Pix idx)
{
  if (idx == 0) return 0;
  for (Pix i = first(); i; next(i)) if (i == idx) return 1;
  return 0;
}

void <T>Set::clear()
{
  Pix i = first(); 
  while (i != 0)
  {
    del((*this)(i));
    i = first();
  }
}

int <T>Set::contains (<T&> item)
{
  return seek(item) != 0;
}

int <T>Set::operator <= (<T>Set& b)
{
  if (count > b.count) return 0;
  if (count == 0) return 1;
  for (Pix i = first(); i; next(i)) if (b.seek((*this)(i)) == 0) return 0;
  return 1;
}

int <T>Set::operator == (<T>Set& b)
{
  int n = count;
  if (n != b.count) return 0;
  if (n == 0) return 1;
  Pix i = first();
  Pix j = b.first();
  while (n-- > 0)
  {
    if ((b.seek((*this)(i)) == 0) || (seek(b(j)) == 0)) return 0;
    next(i);
    b.next(j);
  }
  return 1;
}

int <T>Set::operator != (<T>Set& b)
{
  return !(*this == b);
}

void <T>Set::operator |= (<T>Set& b)
{
  if (&b != this)
    for (Pix i = b.first(); i; b.next(i)) add(b(i));
}

void <T>Set::operator -= (<T>Set& b)
{
  if (&b == this)
    clear();
  else
    for (Pix i = b.first(); i; b.next(i)) del(b(i));
}


void <T>Set::operator &= (<T>Set& b)
{
  if (&b != this)
  {
    Pix i = first();
    Pix n = i;
    while (i != 0)
    {
      next(n);
      if (b.seek((*this)(i)) == 0) del((*this)(i));
      i = n;
    }
  }
}

void <T>Set::error(const char* msg)
{
  (*lib_error_handler)("Set", msg);
}
