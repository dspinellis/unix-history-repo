/* Simple implementation of vsprintf for systems without it.
   Highly system-dependent, but should work on most "traditional"
   implementations of stdio; newer ones should already have vsprintf.
   Written by Per Bothner of Cygnus Support.
   Based on libg++'s "form" (written by Doug Lea; dl@rocky.oswego.edu).
   Copyright (C) 1991 Free Software Foundation, Inc.

This file is part of the libiberty library.
Libiberty is free software; you can redistribute it and/or
modify it under the terms of the GNU Library General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

Libiberty is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Library General Public License for more details.

You should have received a copy of the GNU Library General Public
License along with libiberty; see the file COPYING.LIB.  If
not, write to the Free Software Foundation, Inc., 675 Mass Ave,
Cambridge, MA 02139, USA.  */

#include <varargs.h>
#include <stdio.h>
#undef vsprintf

int
vsprintf (buf, format, ap)
     char *buf;
     char *format;
     va_list ap;
{
  FILE b;
  int ret;
#ifdef VMS
  b->_flag = _IOWRT|_IOSTRG;
  b->_ptr = buf;
  b->_cnt = 12000;
#else
  b._flag = _IOWRT|_IOSTRG;
  b._ptr = buf;
  b._cnt = 12000;
#endif
  ret = _doprnt(format, ap, &b);
  putc('\0', &b);
  return ret;

}
