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
#include "<T>.XPSet.h"

int <T>XPSet::OK()
{
  int v = p.OK();
  v &= count == p.length();
  if (!v) error("invariant failure");
  return v;
}

Pix <T>XPSet::seek(<T&> item)
{
  for (int i = p.low(); i < p.fence(); p.next(i))
    if (<T>EQ(p[i],item)) return p.index_to_Pix(i);
  return 0;
}

Pix <T>XPSet::add(<T&> item)
{
  Pix i = seek(item);
  if (i == 0)
  {
    ++count;
    i = p.index_to_Pix(p.add_high(item));
  }
  return i;
}

void <T>XPSet::del(<T&> item)
{
  for (int i = p.low(); i < p.fence(); p.next(i))
  {
    if (<T>EQ(p[i], item))
    {
      --count;
      p[i] = p.low_element();
      p.del_low();
      return;
    }
  }
}

