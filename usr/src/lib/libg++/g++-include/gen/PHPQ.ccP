// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
    written by Dirk Grunwald (grunwald@cs.uiuc.edu)
    adapted for libg++ by Doug Lea (dl@rocky.oswego.edu)

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
#include <values.h>
#include "<T>.PHPQ.h"

//
//	This defines a Pairing Heap structure
//
//	See ``The Pairing Heap: A New Form of Self-Adjusting Heap''
//	Fredman, Segdewick et al,
//	Algorithmica (1986) 1:111-129
//
//	In particular, this implements the pairing heap using the circular
//	list.
//
//

<T>PHPQ::<T>PHPQ(int sz)
{
  storage = 0;
  root = 0;
  count = 0;
  size = 0;
  prealloc(sz);
}

<T>PHPQ::<T>PHPQ(<T>PHPQ& a)
{
  storage = 0;
  root  = 0;
  count = 0;
  size = 0;
  prealloc(a.size);
  for (Pix i = a.first(); i != 0; a.next(i)) enq(a(i));
}


void <T>PHPQ::prealloc(int newsize)
{
  ++newsize; // leave a spot for freelist
  if (size != 0)
  {
    int news = size;
    while (news <= newsize) news = (news * 3) / 2;
    newsize = news;
  }
  // see if indices are OK
  <T>PHPQNode test;
  test.sibling = 0;
  test.sibling = ~test.sibling;
  if ((unsigned long)newsize > (unsigned long)(test.sibling))
    error("storage size exceeds index range");

  if (storage == 0)
  {
    storage = new <T>PHPQNode[size = newsize];
    for (int i = 0; i < size; ++i) 
    {
      storage[i].sibling = i + 1;
      storage[i].valid = 0;
    }
    storage[size-1].sibling = 0;
  }
  else
  {
    <T>PHPQNode* newstor = new <T>PHPQNode[newsize];
    for (int i = 1; i < size; ++i)
      newstor[i] = storage[i];
    delete [size] storage;
    storage = newstor;
    for (i = size; i < newsize; ++i) 
    {
      storage[i].sibling = i + 1;
      storage[i].valid = 0;
    }
    storage[newsize-1].sibling = 0;
    storage[0].sibling = size;
    size = newsize;
  }
}


void <T>PHPQ::clear()
{
  for (int i = 0; i < size; ++i) 
  {
    storage[i].sibling = i + 1;
    storage[i].valid = 0;
  }
  storage[size-1].sibling = 0;
  root = 0;
  count = 0;
}

Pix <T>PHPQ::enq(<T&> item)
{
  ++count;
  if (storage[0].sibling == 0)
    prealloc(count);

  int cell = storage[0].sibling;
  storage[0].sibling = storage[cell].sibling;
  storage[cell].sibling = 0;
  storage[cell].children = 0;
  storage[cell].item = item;
  storage[cell].valid = 1;
  
  if (root == 0) 
  {
    root = cell;
    return Pix(root);
  }
  else 
  {
    int parent;
    int child;
    
    if (<T>LE(storage[root].item, storage[cell].item))
    {
      parent = root; child = cell;
    } 
    else 
    {
      parent = cell; child = root;
    }
    int popsKid = storage[parent].children;
    
    if (popsKid == 0) 
    {
      storage[parent].children = child;
      storage[child].sibling = child;
    } 
    else 
    {
      int temp = storage[popsKid].sibling;
      storage[popsKid].sibling = child;
      storage[child].sibling = temp;
      storage[parent].children = child;
    }
    root = parent;
    return Pix(cell);
  }
}

//
//	Item removal is the most complicated routine.
//
//	We remove the root (should there be one) and then select a new
//	root. The siblings of the root are in a circular list. We continue
//	to pair elements in this list until there is a single element.
//	This element will be the new root.

void <T>PHPQ::del_front()
{
  int valid = 0;
  do 
  {
    if (root == 0) return;
	if (valid = storage[root].valid)
      --count;
    storage[root].valid = 0;
	int child = storage[root].children;
    storage[root].sibling = storage[0].sibling;
    storage[0].sibling = root;

	if (child == 0)
    {
      root = 0;
      return;
    }
    else 
    {
      while(storage[child].sibling != child) 
      {
		// We have at least two kids, but we may only have
		// two kids. So, oneChild != child, but it is possible
		// that twoChild == child.
        
		int oneChild = storage[child].sibling;
		int twoChild = storage[oneChild].sibling;

		// Remove the two from the sibling list

		storage[child].sibling = storage[twoChild].sibling;
		storage[oneChild].sibling = 0;
		storage[twoChild].sibling = 0;
		
        int bestChild;
        int worstChild;
    
        if (<T>LE(storage[oneChild].item, storage[twoChild].item))
        {
          bestChild = oneChild; worstChild = twoChild;
        } 
        else 
        {
          bestChild = twoChild; worstChild = oneChild;
        }
        int popsKid = storage[bestChild].children;
        
        if (popsKid == 0) 
        {
          storage[bestChild].children = worstChild;
          storage[worstChild].sibling = worstChild;
        } 
        else 
        {
          int temp = storage[popsKid].sibling;
          storage[popsKid].sibling = worstChild;
          storage[worstChild].sibling = temp;
          storage[bestChild].children = worstChild;
        }
		if (twoChild == child) 
        {
          // We have reduced the two to one, so we'll be exiting.
          child = bestChild;
          storage[child].sibling = child;
        } 
        else 
        {
          // We've removed two siblings, now we need to insert
          // the better of the two
          storage[bestChild].sibling = storage[child].sibling;
          storage[child].sibling = bestChild;
          child = storage[bestChild].sibling;
		}
      }
      root = child;
	}
  } while ( !valid );
}

void <T>PHPQ::del(Pix p) 
{
  if (p == 0) error("null Pix");
  int i = int(p);
  if (storage[i].valid)
  {
    if (i == root)
      del_front();
    else
    {
      storage[i].valid = 0;
      --count;
    }
  }
}


Pix <T>PHPQ::seek(<T&> key)
{
  for (int i = 1; i < size; ++i)
    if (storage[i].valid && <T>EQ(storage[i].item, key))
      return Pix(i);
  return 0;
}

Pix <T>PHPQ::first()
{
  for (int i = 1; i < size; ++i)
    if (storage[i].valid)
      return Pix(i);
  return 0;
}


void <T>PHPQ::next(Pix& p)
{
  if (p == 0) return;
  for (int i = int(p)+1; i < size; ++i)
    if (storage[i].valid)
    {
      p = Pix(i); 
      return;
    }
  p = 0;
}

int <T>PHPQ::OK()
{
  int v = storage != 0;
  int n = check_sibling_list(root, 0);
  v &= n  == count;
  int ct = MAXLONG;
  n = 0;
  int f = storage[0].sibling;
  while (f != 0 && ct-- > 0)
  {
    f = storage[f].sibling;
    ++n;
  }
  v &= ct > 0;
  v &= n <= size - count;
  if (!v) error("invariant failure");
  return v;
}


int <T>PHPQ::check_sibling_list(int t, int cnt)
{
  if (t != 0)
  {
    int s = t;
    long ct = MAXLONG;      // Lots of chances to find self!
    do 
    {
      if (storage[s].valid) cnt++;
      cnt += check_sibling_list(storage[s].children, cnt);
      s = storage[s].sibling;
    } while (ct-- > 0 && s != t && s != 0);
    if (ct <= 0) return -1;
  }
  return cnt;
}


