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
#include "<T>.OXPSet.h"


Pix <T>OXPSet::seek(<T&> item)
{
  int l = p.low();
  int h = p.high();
  while (l <= h)
  {
    int mid = (l + h) / 2;
    int cmp = <T>CMP(item, p[mid]);
    if (cmp == 0)
      return p.index_to_Pix(mid);
    else if (cmp < 0)
      h = mid - 1;
    else
      l = mid + 1;
  }
  return 0;
}

Pix <T>OXPSet::add(<T&> item)
{
  if (count == 0) 
  {
    ++count;
    return p.index_to_Pix(p.add_high(item));
  }
  int l = p.low();
  int h = p.high();
  while (l <= h)
  {
    int mid = (l + h) / 2;
    int cmp = <T>CMP(item, p[mid]);
    if (cmp == 0)
      return p.index_to_Pix(mid);
    else if (cmp < 0)
        h = mid - 1;
    else
      l = mid + 1;
  }
  // add on whichever side is shortest
  ++count;
  if (l == p.fence())
    return p.index_to_Pix(p.add_high(item));
  else if (l == p.low())
    return p.index_to_Pix(p.add_low(item));
  else 
  {
    if (p.fence() - l < l - p.low())
    {
      h = p.add_high(p.high_element());
      for (int i = h - 1; i > l; --i) p[i] = p[i-1];
    }
    else
    {
      --l;
      h = p.add_low(p.low_element());
      for (int i = h + 1; i < l; ++i) p[i] = p[i+1];
    }
    p[l] = item;
    return p.index_to_Pix(l);
  }
}

void <T>OXPSet::del(<T&> item)
{
  int l = p.low();
  int h = p.high();
  while (l <= h)
  {
    int mid = (l + h) / 2;
    int cmp = <T>CMP(item, p[mid]);
    if (cmp == 0)
    {
      --count;
      if (p.high() - mid < mid - p.low())
      {
        for (int i = mid; i < p.high(); ++i) p[i] = p[i+1];
        p.del_high();
      }
      else
      {
        for (int i = mid; i > p.low(); --i) p[i] = p[i-1];
        p.del_low();
      }
      return;
    }
    else if (cmp < 0)
      h = mid - 1;
    else
      l = mid + 1;
  }
}

int <T>OXPSet::operator <= (<T>OXPSet& b)
{
  if (count > b.count) return 0;
  int i = p.low();
  int j = b.p.low();
  for (;;)
  {
    if (i >= p.fence())
      return 1;
    else if (j >= b.p.fence()) 
      return 0;
    int cmp = <T>CMP(p[i], b.p[j]);
    if (cmp == 0)
    {
      ++i; ++j;
    }
    else if (cmp < 0)
      return 0;
    else
      ++j;
  }
}

int <T>OXPSet::operator == (<T>OXPSet& b)
{
  int n = count;
  if (n != b.count) return 0;
  if (n == 0) return 1;
  int i = p.low();
  int j = b.p.low();
  while (n-- > 0) if (!<T>EQ(p[i++], b.p[j++])) return 0;
  return 1;
}


void <T>OXPSet::operator |= (<T>OXPSet& b)
{
  if (&b == this || b.count == 0)
    return;
  else if (b.count <= 2) // small b -- just add
    for (Pix i = b.first(); i; b.next(i)) add(b(i));
  else
  {
    // strategy: merge into top of p, simultaneously killing old bottom
    int oldfence = p.fence();
    int i = p.low();
    int j = b.p.low();
    for (;;)
    {
      if (i == oldfence)
      {
        while (j < b.p.fence()) p.add_high(b.p[j++]);
        break;
      }
      else if (j == b.p.fence())
      {
        while (i++ < oldfence) 
        {
          p.add_high(p.low_element());
          p.del_low();
        }
        break;
      }
      int cmp = <T>CMP(p[i], b.p[j]);
      if (cmp <= 0)
      {
        ++i;
        if (cmp == 0)  ++j;
        p.add_high(p.low_element());
        p.del_low();
      }
      else
        p.add_high(b.p[j++]);
    }
    count = p.length();
  }
}



void <T>OXPSet::operator -= (<T>OXPSet& b)
{
  if (&b == this)
    clear();
  else if (count != 0 && b.count != 0)
  {
    int i = p.low();
    int k = i;
    int j = b.p.low();
    int oldfence = p.fence();
    for (;;)
    {
      if (i >= oldfence)
        break;
      else if (j >= b.p.fence())
      {
        if (k != i)
          while (i < oldfence) p[k++] = p[i++];
        else
          k = oldfence;
        break;
      }
      int cmp = <T>CMP(p[i], b.p[j]);
      if (cmp == 0)
      {
        ++i; ++j;
      }
      else if (cmp < 0)
      {
        if (k != i) p[k] = p[i];
        ++i; ++k;
      }
      else
        j++;
    }
    while (k++ < oldfence)
    {
      --count;
      p.del_high();
    }
  }
}

void <T>OXPSet::operator &= (<T>OXPSet& b)
{
  if (b.count == 0)
    clear();
  else if (&b != this && count != 0)
  {
    int i = p.low();
    int k = i;
    int j = b.p.low();
    int oldfence = p.fence();
    for (;;)
    {
      if (i >= oldfence || j >= b.p.fence())
        break;
      int cmp = <T>CMP(p[i], b.p[j]);
      if (cmp == 0)
      {
        if (k != i) p[k] = p[i];
        ++i; ++k; ++j;
      }
      else if (cmp < 0)
        ++i;
      else
        ++j;
    }
    while (k++ < oldfence)
    {
      --count;
      p.del_high();
    }
  }
}

int <T>OXPSet::OK()
{
  int v = p.OK();
  v &= count == p.length();
  for (int i = p.low(); i < p.high(); ++i) v &= <T>CMP(p[i], p[i+1]) < 0;
  if (!v) error("invariant failure");
  return v;
}
