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


#ifndef _AllocRing_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _AllocRing_h 1


/*
  An AllocRing holds the last n malloc'ed strings, reallocating/reusing 
  one only when the queue wraps around. It thus guarantees that the
  last n allocations are intact. It is useful for things like I/O
  formatting where reasonable restrictions may be made about the
  number of allowable live allocations before auto-deletion.
*/

class AllocRing
{

  struct AllocQNode
  {
    void*  ptr;
    int    sz;
  };

  AllocQNode* nodes;
  int         n;
  int         current;

  int         find(void* p);

public:

              AllocRing(int max);
             ~AllocRing();

  void*       alloc(int size);
  int         contains(void* ptr);
  void        clear();
  void        free(void* p);
};


#endif
