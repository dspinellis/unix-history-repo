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
#include "<T>.VQueue.h"

<T>VQueue::<T>VQueue(<T>VQueue& b)
:size(b.size), cnt(b.cnt), inp(b.inp), outp(b.outp), s(new <T> [b.size])
{
  int j = outp;
  for (int i = 0; i < cnt; ++i)
  {
    s[j] = b.s[j];
    if (++j == size) j = 0;
  }
}

void <T>VQueue::operator = (<T>VQueue& b)
{
  if (&b == this) return;
  if (size != b.size)
  {
    delete [size] s;
    s = new <T> [b.size];
    size = b.size;
  }
  inp = b.inp; outp = b.outp; cnt = b.cnt;
  int j = outp;
  for (int i = 0; i < cnt; ++i)
  {
    s[j] = b.s[j];
    if (++j == size) j = 0;
  }
}


void <T>VQueue::resize(int newsz)
{
  if (newsz < cnt)
    error("resize: new size too small");
  <T>* news = new <T> [newsz];
  int j = outp;
  for (int i = 0; i < cnt; ++i)
  {
    news[i] = s[j];
    if (++j == size) j = 0;
  }
  inp = j;
  outp = 0;
  delete [size] s;
  s = news;
  size = newsz;
}

int <T>VQueue::OK()
{
  int v = s != 0;               // have space
  v &= size >= 0;               // a legal size
  v &= inp >= 0 && inp <= size; // pointers with bounds  
  v &= outp >= 0 && outp <= size;
  int c = (size + inp - outp) % size;
  v &= cnt == size || cnt == c; // correct count
  if (!v) error("invariant failure");
  return v;
}
