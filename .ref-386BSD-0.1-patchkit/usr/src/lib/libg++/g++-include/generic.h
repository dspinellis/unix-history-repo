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

#ifndef generic_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define generic_h 1

/*
 *	See the CPP manual, argument prescan section for explanation
 */

#define name2(a,b) gEnErIc2(a,b)
#define gEnErIc2(a,b) a ## b

#define name3(a,b,c) gEnErIc3(a,b,c)
#define gEnErIc3(a,b,c) a ## b ## c

#define name4(a,b,c,d) gEnErIc4(a,b,c,d)
#define gEnErIc4(a,b,c,d) a ## b ## c ## d

#define GENERIC_STRING(a) gEnErIcStRiNg(a)
#define gEnErIcStRiNg(a) #a

#define declare(clas,t)        name2(clas,declare)(t)
#define declare2(clas,t1,t2)   name2(clas,declare2)(t1,t2)

#define implement(clas,t)      name2(clas,implement)(t)
#define implement2(clas,t1,t2) name2(clas,implement2)(t1,t2)

extern genericerror(int,char*);
typedef int (*GPT)(int,char*);

#define set_handler(gen,type,x) name4(set_,type,gen,_handler)(x)

#define errorhandler(gen,type)  name3(type,gen,handler)

#define callerror(gen,type,a,b) (*errorhandler(gen,type))(a,b)


#endif generic_h
