/* 
Copyright (C) 1990 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifdef __GNUG__
#pragma implementation
#endif

#include <streambuf.h>


streambuf::streambuf()
     :base(0), gptr(0), pptr(0), eptr(0), alloc(0)
{}

streambuf::streambuf(char* buf, int buflen)
     : base(buf), gptr(buf), pptr(buf), eptr(buf+buflen-1), alloc(0)
{}

streambuf::~streambuf()
{
  if (alloc && (base != 0)) delete base;
}

int streambuf::doallocate()
{
  if (alloc && base != 0) delete base;
  base = new char[BUFSIZ];
  gptr = pptr = base;
  eptr = base + BUFSIZ - 1;
  alloc = 1;
  return BUFSIZ;
}

streambuf* streambuf::setbuf(char* buf, int buflen, int preloaded_count)
{
  if (alloc && (base != 0)) delete base;
  alloc = 0;
  base = gptr = buf;
  pptr = base + preloaded_count;
  eptr = base + buflen - 1;
  return this;
}

const char* streambuf::name()
{
  return 0;
}

int streambuf::overflow(int c)
{
  if (base == 0) allocate();
  return (c == EOF)? c : ((pptr <= eptr)? (*pptr++ = (char)(c)) : EOF);
}

int streambuf::underflow()
{
  return EOF;
}

int streambuf::sputs(const char* s)
{
  if (s != 0)
  {
    for(; *s != 0; ++s)
    {
      if (must_overflow(*s)) { if (overflow(*s) == EOF) return EOF; }
      else *pptr++ = *s;
    }
  }
  return 0;
}

int streambuf::sputsn(const char* s, int len)
{
  for(; --len >= 0; ++s)
  {
    if (must_overflow(*s)) { if (overflow(*s) == EOF) return EOF; }
      else *pptr++ = *s;
  }
  return 0;
}


int streambuf::is_open()
{
  return 1;
}

int streambuf::close()
{
  return 1;
}

void streambuf::error()
{
  abort();
}

streambuf* streambuf::open(const char*, open_mode)
{
  return 0;
}

streambuf* streambuf::open(const char*, io_mode, access_mode)
{
  return 0;
}
streambuf* streambuf::open(const char*, const char*)
{
  return 0;
}
streambuf* streambuf::open(int, io_mode)
{
  return 0;
}
streambuf* streambuf::open(FILE*)
{
  return 0;
}
