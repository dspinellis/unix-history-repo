// This may look like C code, but it is really -*- C++ -*-

/* 
Copyright (C) 1988 Free Software Foundation

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

#ifndef _stdarg_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _stdarg_h 1

#if defined(sparc) || defined(mips)
#ifndef __GNUG__
extern "C" __builtin_saveregs ();
#endif
#endif
typedef char *va_list;

/* Amount of space required in an argument list for an arg of type TYPE.
   TYPE may alternatively be an expression whose type is used.  */

#define __va_rounded_size(TYPE)  \
  (((sizeof (TYPE) + sizeof (int) - 1) / sizeof (int)) * sizeof (int))

#if	defined(sparc)
#  define va_start(AP, LASTARG)						\
 (__builtin_saveregs (), AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#elif defined(mips)
#  define va_start(AP,LASTARG) {\
       static char __vd_alist[16] = "__%%VARARGS";  /* Identify to codegen */\
	__builtin_saveregs();\
	(AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)));}
#else
#  define va_start(AP, LASTARG)						\
 (AP = ((char *) &(LASTARG) + __va_rounded_size (LASTARG)))
#endif

void va_end (va_list);		/* Defined in gnulib */
#define va_end(AP)

#define va_arg(AP, TYPE)						\
 (AP += __va_rounded_size (TYPE),					\
  *((TYPE *) (AP - __va_rounded_size (TYPE))))

#endif
