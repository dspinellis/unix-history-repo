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
#include <stream.h>
#include "<T>.VStack.h"

// error handling


<T>VStack::<T>VStack(<T>VStack& b)
:size(b.size), ptr(b.ptr), s(new <T> [b.size])
{
  for (int i = 0; i < ptr; ++i) s[i] = b.s[i];
}

void <T>VStack::operator = (<T>VStack& b)
{
  if (&b == this) return;
  if (size < b.ptr)
  {
    delete [size] s;
    s = new <T> [b.size];
    size = b.size;
  }
  ptr = b.ptr;
  for (int i = 0; i < ptr; ++i) s[i] = b.s[i];
}


void <T>VStack::resize(int newsz)
{
  if (newsz < ptr) error("resize: new size too small");
  <T>* news = new <T> [newsz];
  for (int i = 0; i < ptr; ++i) news[i] = s[i];
  delete [size] s;
  s = news;
  size = newsz;
}

int <T>VStack::OK()
{
  int v = s != 0;               // have space
  v &= size >= 0;               // a legal size
  v &= ptr <= size;             // ptr within bounds
  v &= ptr >= 0;
  if (!v) error("invariant failure");
  return v;
}
