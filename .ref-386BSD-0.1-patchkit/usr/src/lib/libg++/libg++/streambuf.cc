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
