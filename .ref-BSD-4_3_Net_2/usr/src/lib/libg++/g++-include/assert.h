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

/* Allow this file to be included multiple times
   with different settings of NDEBUG.  */
#undef assert
#undef assertval

#ifdef NDEBUG
#define assert(ignore)
#define assertval(ex)  (ex)
#else

extern "C" void __eprintf (char*, int, char*);		/* Defined in gnulib */
extern "C" volatile void   abort();

#define assert(ex) \
        ((ex) ? 1 : \
              (__eprintf("Failed assertion " #ex " at line %d of `%s'.\n", \
               __LINE__, __FILE__), abort (), 0))
#define assertval(ex) \
        ((ex) ? 1 : \
              (__eprintf("Failed assertion " #ex " at line %d of `%s'.\n", \
               __LINE__, __FILE__), abort (), 0))

#endif NDEBUG
