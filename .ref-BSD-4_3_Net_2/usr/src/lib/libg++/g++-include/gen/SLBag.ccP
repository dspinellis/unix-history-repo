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
#include "<T>.SLBag.h"

int <T>SLBag::OK()
{
  int v = p.OK();
  v &= count == p.length();
  if (!v) error("invariant failure");
  return v;
}

Pix <T>SLBag::seek(<T&> item, Pix i)
{
  if (i == 0) i = first(); else next(i);
  for (; i != 0 && (!(<T>EQ(p(i), item))); p.next(i));
  return i;
}

int <T>SLBag::nof(<T&> item)
{
  int n = 0;
  for (Pix p = first(); p; next(p)) if (<T>EQ((*this)(p), item)) ++n;
  return n;
}


void <T>SLBag::del(<T&> item)
{
  Pix i = p.first();
  if (i == 0) 
    return;
  else if (<T>EQ(p(i), item))
  {
    --count;
    p.del_front();
  }
  else
  {
    Pix trail = i;
    p.next(i);
    while (i != 0)
    {
      if (<T>EQ(p(i), item))
      {
        --count;
        p.del_after(trail);
        return;
      }
      trail = i;
      p.next(i);
    }
  }
}    

void <T>SLBag::remove(<T&> item)
{
  Pix i = p.first();
  while (i != 0 && <T>EQ(p(i), item))
  {
    --count;
    p.del_front();
    i = p.first();
  }
  if (i != 0)
  {
    Pix trail = i;
    p.next(i);
    while (i != 0)
    {
      if (<T>EQ(p(i), item))
      {
        --count;
        p.del_after(trail);
        i = trail;
        p.next(i);
      }
      else
      {
        trail = i;
        p.next(i);
      }
    }
  }
}    

