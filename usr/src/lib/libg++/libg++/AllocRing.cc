// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1989 Free Software Foundation
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
#include <std.h>
#include <AllocRing.h>
#include <new.h>

AllocRing::AllocRing(int max)
  :n(max), current(0), nodes(new AllocQNode[max])
{
  for (int i = 0; i < n; ++i)
  {
    nodes[i].ptr = 0;
    nodes[i].sz = 0;
  }
}

int AllocRing::find(void* p)
{
  if (p == 0) return -1;

  for (int i = 0; i < n; ++i)
    if (nodes[i].ptr == p)
      return i;

  return -1;
}


void AllocRing::clear()
{
  for (int i = 0; i < n; ++i)
  {
    if (nodes[i].ptr != 0)
    {
      delete(nodes[i].ptr);
      nodes[i].ptr = 0;
    }
    nodes[i].sz = 0;
  }
  current = 0;
}


void AllocRing::free(void* p)
{
  int idx = find(p);
  if (idx >= 0)
  {
    delete nodes[idx].ptr;
    nodes[idx].ptr = 0;
  }
}

AllocRing::~AllocRing()
{
  clear();
}

int AllocRing::contains(void* p)
{
  return find(p) >= 0;
}

static inline unsigned int good_size(unsigned int s)
{
  unsigned int req = s + 4;
  unsigned int good = 8;
  while (good < req) good <<= 1;
  return good - 4;
}

void* AllocRing::alloc(int s)
{
  unsigned int size = good_size(s);

  void* p;
  if (nodes[current].ptr != 0 && 
      nodes[current].sz >= size && 
      nodes[current].sz < (4 * size))
    p = nodes[current].ptr;
  else
  {
    if (nodes[current].ptr != 0) delete nodes[current].ptr;
    p = new char[size];
    nodes[current].ptr = p;
    nodes[current].sz = size;
  }
  ++current;
  if (current >= n) current = 0;
  return p;
}


    
#ifdef VMS
#include "libgxx-fmtq.cc"    // see Obstack.cc for reason for this
#endif
