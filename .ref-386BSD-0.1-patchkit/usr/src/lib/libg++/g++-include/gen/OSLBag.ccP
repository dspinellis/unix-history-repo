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
#include "<T>.OSLBag.h"


Pix <T>OSLBag::seek(<T&> item, Pix i)
{
  if (i == 0) i = p.first(); else next(i);
  for (; i != 0; p.next(i))
  {
    int cmp = <T>CMP(item, p(i));
    if (cmp == 0)
      return i;
    else if (cmp < 0)
      return 0;
  }
  return 0;
}

int <T>OSLBag::nof(<T&> item)
{
  int n = 0;
  for (Pix i = p.first(); i != 0; p.next(i))
  {
    int cmp = <T>CMP(item, p(i));
    if (cmp == 0)
      ++n;
    else if (cmp < 0)
      break;
  }
  return n;
}

Pix <T>OSLBag::add(<T&> item)
{
  Pix i = p.first();
  if (i == 0) 
  {
    ++count;
    return p.prepend(item);
  }
  int cmp = <T>CMP(item, p(i));
  if (cmp <= 0)
  {
    ++count;
    return p.prepend(item);
  }
  else
  {
    Pix trail = i;
    p.next(i);
    for (;;)
    {
      if (i == 0)
      {
        ++count;
        return p.append(item);
      }
      cmp = <T>CMP(item, p(i));
      if (cmp <= 0)
      {
        ++count;
        return p.ins_after(trail, item);
      }
      else
      {
        trail = i;
        p.next(i);
      }
    }
  }
}

void <T>OSLBag::del(<T&> item)
{
  Pix i = p.first();
  if (i == 0)
    return;
  int cmp = <T>CMP(item, p(i));
  if (cmp < 0)
    return;
  else if (cmp == 0)
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
      cmp = <T>CMP(item, p(i));
      if (cmp < 0)
        return;
      else if (cmp == 0)
      {
        --count;
        p.del_after(trail);
        return;
      }
      else
      {
        trail = i;
        p.next(i);
      }
    }
  }
}

void <T>OSLBag::remove(<T&> item)
{
  Pix i = p.first();
  if (i == 0)
    return;
  int cmp = <T>CMP(item, p(i));
  if (cmp < 0)
    return;
  else if (cmp == 0)
  {
    do
    {
      --count;
      p.del_front();
      i = p.first();
    } while (i != 0 && <T>EQ(item, p(i)));
  }
  else
  {
    Pix trail = i;
    p.next(i);
    while (i != 0)
    {
      cmp = <T>CMP(item, p(i));
      if (cmp < 0)
        return;
      else if (cmp == 0)
      {
        do
        {
          --count;
          p.del_after(trail);
          i = trail;
          next(i);
        } while (i != 0 && <T>EQ(item, p(i)));
        return;
      }
      else
      {
        trail = i;
        p.next(i);
      }
    }
  }
}
        
int <T>OSLBag::OK()
{
  int v = p.OK();
  v &= count == p.length();
  Pix trail = p.first();
  if (trail == 0)
    v &= count == 0;
  else
  {
    Pix i = trail; next(i);
    while (i != 0)
    {
      v &= <T>CMP(p(trail), p(i)) <= 0;
      trail = i;
      next(i);
    }
  }
  if (!v) error("invariant failure");
  return v;
}
 
