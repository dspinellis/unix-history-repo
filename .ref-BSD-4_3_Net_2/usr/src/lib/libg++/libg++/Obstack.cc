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
#include <values.h>
#include <builtin.h>
#include <Obstack.h>

Obstack::Obstack(int size, int alignment)
{
  alignmentmask = alignment - 1;
  chunksize = size;
  chunk = 0;
  nextfree = objectbase = 0;
  chunklimit = 0;
}

void Obstack::_free(void* obj)
{
  _obstack_chunk*  lp;
  _obstack_chunk*  plp;

  lp = chunk;
  while (lp != 0 && ((void*)lp > obj || (void*)(lp)->limit < obj))
  {
    plp = lp -> prev;
    delete(lp);
    lp = plp;
  }
  if (lp)
  {
    objectbase = nextfree = (char *)(obj);
    chunklimit = lp->limit;
    chunk = lp;
  }
  else if (obj != 0)
    (*lib_error_handler)("Obstack", "deletion of nonexistent obj");
}

void Obstack::newchunk(int size)
{
  _obstack_chunk*	old_chunk = chunk;
  _obstack_chunk*	new_chunk;
  long	new_size;
  int obj_size = nextfree - objectbase;

  new_size = (obj_size + size) << 1;
  if (new_size < chunksize)
    new_size = chunksize;

  new_chunk = chunk = (_obstack_chunk*)(new char[new_size]);
  new_chunk->prev = old_chunk;
  new_chunk->limit = chunklimit = (char *) new_chunk + new_size;

  bcopy((void*)objectbase, (void*)new_chunk->contents, obj_size);
  objectbase = new_chunk->contents;
  nextfree = objectbase + obj_size;
}

void* Obstack::finish()
{
  void* value = (void*) objectbase;
  nextfree = (char*)((int)(nextfree + alignmentmask) & ~(alignmentmask));
  if (nextfree - (char*)chunk > chunklimit - (char*)chunk)
    nextfree = chunklimit;
  objectbase = nextfree;
  return value;
}

int Obstack::contains(void* obj) // true if obj somewhere in Obstack
{
  for (_obstack_chunk* ch = chunk; 
       ch != 0 && (obj < (void*)ch || obj >= (void*)(ch->limit)); 
       ch = ch->prev);

  return ch != 0;
}
         
int Obstack::OK()
{
  int v = chunksize > 0;        // valid size
  v &= alignmentmask != 0;      // and alignment
  v &= chunk != 0;
  v &= objectbase >= chunk->contents;
  v &= nextfree >= objectbase;
  v &= nextfree <= chunklimit;
  v &= chunklimit == chunk->limit;
  _obstack_chunk* p = chunk;
  // allow lots of chances to find bottom!
  long x = MAXLONG;
  while (p != 0 && x != 0) { --x; p = p->prev; }
  v &= x > 0;
  if (!v) 
    (*lib_error_handler)("Obstack", "invariant failure");
  return v;
}


#ifdef VMS
#include "libgxx-io-ob.cc"
// The reason that this needs to be included is that if the modules for libg++
// are placed in a library, and libgxx-ob-io is in a seperate module, then
// that module contains only two symbols -  the contstructor and destructor.
// they are not called explicitly anywhere else, so that module is not linked
// in, and the contstructor is not called.  Chaos ensues.
#endif

