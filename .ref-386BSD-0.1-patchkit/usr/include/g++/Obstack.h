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


#ifndef _Obstack_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _Obstack_h 1

#include <std.h>

class Obstack
{
  struct _obstack_chunk
  {
    char*           limit;
    _obstack_chunk* prev;
    char            contents[4];
  };

protected:
  long	          chunksize;
  _obstack_chunk* chunk;
  char*	          objectbase;
  char*	          nextfree;
  char*	          chunklimit;
  int             alignmentmask;

  void  _free(void* obj);
  void  newchunk(int size);

public:
        Obstack(int size = 4080, int alignment = 4); // 4080=4096-mallocslop

        ~Obstack();

  void* base();
  void* next_free();
  int   alignment_mask();
  int   chunk_size();
  int   size();
  int   room();
  int   contains(void* p);      // does Obstack hold pointer p?

  void  grow(const void* data, int size);
  void  grow(const void* data, int size, char terminator);
  void  grow(const char* s);
  void  grow(char c);
  void  grow_fast(char c);
  void  blank(int size);
  void  blank_fast(int size);

  void* finish();
  void* finish(char terminator);

  void* copy(const void* data, int size);
  void* copy(const void* data, int size, char terminator);
  void* copy(const char* s);
  void* copy(char c);
  void* alloc(int size);

  void  free(void* obj);
  void  shrink(int size = 1); // suggested by ken@cs.rochester.edu

  int   OK();                   // rep invariant
};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)


inline Obstack::~Obstack()
{
  _free(0); 
}

inline void* Obstack::base()
{
  return objectbase; 
}

inline void* Obstack::next_free()
{
  return nextfree; 
}

inline int Obstack::alignment_mask()
{
  return alignmentmask; 
}

inline int Obstack::chunk_size()
{
  return chunksize; 
}

inline int Obstack::size()
{
  return nextfree - objectbase; 
}

inline int Obstack::room()
{
  return chunklimit - nextfree; 
}

inline void Obstack:: grow(const void* data, int size)
{
  if (nextfree+size > chunklimit) 
    newchunk(size);
  bcopy(data, nextfree, size);
  nextfree += size; 
}

inline void Obstack:: grow(const void* data, int size, char terminator)
{
  if (nextfree+size+1 > chunklimit) 
    newchunk(size+1);
  bcopy(data, nextfree, size);
  nextfree += size; 
  *(nextfree)++ = terminator; 
}

inline void Obstack:: grow(const char* s)
{
  grow((void*)s, strlen(s), 0); 
}

inline void Obstack:: grow(char c)
{
  if (nextfree+1 > chunklimit) 
    newchunk(1); 
  *(nextfree)++ = c; 
}

inline void Obstack:: blank(int size)
{
  if (nextfree+size > chunklimit) 
    newchunk(size);
  nextfree += size; 
}

inline void* Obstack::finish(char terminator)
{
  grow(terminator); 
  return finish(); 
}

inline void* Obstack::copy(const void* data, int size)
{
  grow (data, size);
  return finish(); 
}

inline void* Obstack::copy(const void* data, int size, char terminator)
{
  grow(data, size, terminator); 
  return finish(); 
}

inline void* Obstack::copy(const char* s)
{
  grow((void*)s, strlen(s), 0); 
  return finish(); 
}

inline void* Obstack::copy(char c)
{
  grow(c);
  return finish(); 
}

inline void* Obstack::alloc(int size)
{
  blank(size);
  return finish(); 
}

inline void Obstack:: free(void* obj)     
{
  if (obj >= (void*)chunk && obj<(void*)chunklimit)
    nextfree = objectbase = (char *) obj;
  else 
    _free(obj); 
}

inline void Obstack:: grow_fast(char c)
{
  *(nextfree)++ = c; 
}

inline void Obstack:: blank_fast(int size)
{
  nextfree += size; 
}

inline void Obstack:: shrink(int size) // from ken@cs.rochester.edu
{
  if (nextfree >= objectbase + size)
    nextfree -= size;
}

#endif

#endif
