/* 
Copyright (C) 1990 Free Software Foundation
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
#include <builtin.h>
#include <stdio.h>
#include <stdarg.h>
#include <AllocRing.h>

extern AllocRing _libgxx_fmtq;

char* form(const char* fmt ...)
{
  va_list args;
  va_start(args, fmt);
  char* fmtbase = (char *) _libgxx_fmtq.alloc(BUFSIZ);
#ifndef HAVE_VPRINTF
  FILE b;
#ifdef VMS
  b->_flag = _IOWRT|_IOSTRG;
  b->_ptr = fmtbase;
  b->_cnt = BUFSIZ-1;
#else
  b._flag = _IOWRT|_IOSTRG;
  b._ptr = fmtbase;
  b._cnt = BUFSIZ-1;
#endif
  _doprnt(fmt, args, &b);
  putc('\0', &b);
#else
  vsprintf(fmtbase, fmt, args);
#endif
  va_end(args);
  return fmtbase;
}
