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
#include "<T>.CHBag.h"

// The nodes are linked together serially via a version
// of a trick used in some vtables: odd pointers are
// actually links to the next table entry. 
// Not terrible, but not wonderful either

static inline int goodCHptr(<T>CHNode* t)
{
  return ((((unsigned)t) & 1) == 0);
}

static inline <T>CHNode* index_to_CHptr(int i)
{
  return (<T>CHNode*)((i << 1) + 1);
}

static inline int CHptr_to_index(<T>CHNode* t)
{
  return ( ((unsigned) t) >> 1);
}

<T>CHBag::<T>CHBag(unsigned int sz)
{
  tab = (<T>CHNode**)(new <T>CHNodePtr[size = sz]);
  for (unsigned int i = 0; i < size; ++i) tab[i] = index_to_CHptr(i+1);
  count = 0;
}

<T>CHBag::<T>CHBag(<T>CHBag& a)
{
  tab = (<T>CHNode**)(new <T>CHNodePtr[size = a.size]);
  for (unsigned int i = 0; i < size; ++i) tab[i] = index_to_CHptr(i+1);
  count = 0;
  for (Pix p = a.first(); p; a.next(p)) add(a(p));
}


Pix <T>CHBag::seek(<T&> key, Pix i)
{
  <T>CHNode* p = (<T>CHNode*)i;
  if (p == 0 || !<T>EQ(p->hd, key))
  {
    unsigned int h = <T>HASH(key) % size;
    for (<T>CHNode* t = tab[h]; goodCHptr(t); t = t->tl)
      if (<T>EQ(key, t->hd))
        return Pix(t);
  }
  else
  {
    for (p = p->tl; goodCHptr(p); p = p->tl) 
      if (<T>EQ(p->hd, key))
        return Pix(p);
  }
  return 0;
}

int <T>CHBag::nof(<T&> key)
{
  int n = 0;
  unsigned int h = <T>HASH(key) % size;
  for (<T>CHNode* t = tab[h]; goodCHptr(t); t = t->tl) 
    if (<T>EQ(key, t->hd)) ++n;
  return n;
}


Pix <T>CHBag::add(<T&> item)
{
  unsigned int h = <T>HASH(item) % size;
  <T>CHNode* t = new <T>CHNode(item);
  t->tl = tab[h];
  tab[h] = t;
  ++count;
  return Pix(t);
}

void <T>CHBag::del(<T&> key)
{
  unsigned int h = <T>HASH(key) % size;

  <T>CHNode* t = tab[h]; 
  <T>CHNode* trail = t;
  while (goodCHptr(t))
  {
    if (<T>EQ(key, t->hd))
    {
      if (trail == t)
        tab[h] = t->tl;
      else
        trail->tl = t->tl;
      delete t;
      --count;
      return;
    }
    trail = t;
    t = t->tl;
  }
}

void <T>CHBag::remove(<T&> key)
{
  unsigned int h = <T>HASH(key) % size;

  <T>CHNode* t = tab[h]; 
  <T>CHNode* trail = t;
  while (goodCHptr(t))
  {
    if (<T>EQ(key, t->hd))
    {
      --count;
      if (trail == t)
      {
        tab[h] = t->tl;
        delete t;
        t = trail = tab[h];
      }
      else
      {
        trail->tl = t->tl;
        delete t;
        t = trail->tl;
      }
    }
    else
    {
      trail = t;
      t = t->tl;
    }
  }
}


void <T>CHBag::clear()
{
  for (unsigned int i = 0; i < size; ++i)
  {
    <T>CHNode* p = tab[i];
    tab[i] = index_to_CHptr(i+1);
    while (goodCHptr(p))
    {
      <T>CHNode* nxt = p->tl;
      delete(p);
      p = nxt;
    }
  }
  count = 0;
}

Pix <T>CHBag::first()
{
  for (unsigned int i = 0; i < size; ++i) if (goodCHptr(tab[i])) return Pix(tab[i]);
  return 0;
}

void <T>CHBag::next(Pix& p)
{
  if (p == 0) return;
  <T>CHNode* t = ((<T>CHNode*)p)->tl;
  if (goodCHptr(t))
    p = Pix(t);
  else
  {
    for (unsigned int i = CHptr_to_index(t); i < size; ++i) 
    {
      if (goodCHptr(tab[i]))
      {
        p =  Pix(tab[i]);
        return;
      }
    }
    p = 0;
  }
}

int <T>CHBag::OK()
{
  int v = tab != 0;
  int n = 0;
  for (unsigned int i = 0; i < size; ++i)
  {
    for (<T>CHNode* p = tab[i]; goodCHptr(p); p = p->tl) ++n;
    v &= CHptr_to_index(p) == i + 1;
  }
  v &= count == n;
  if (!v) error("invariant failure");
  return v;
}
