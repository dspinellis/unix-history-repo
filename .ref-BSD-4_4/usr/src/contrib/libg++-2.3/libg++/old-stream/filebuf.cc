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

#if 1
#ifdef __GNUG__
#pragma implementation
#endif
#endif

#include <std.h>
#include <sys/file.h>           // needed to determine values of O_RDONLY...
#include <filebuf.h>

filebuf::filebuf() 
     :streambuf(), fd(-1), opened(0) {}

filebuf::filebuf(int newfd) 
     : streambuf(), fd(newfd), opened(1) {}

filebuf::filebuf(int newfd, char* buf, int buflen)
     : streambuf(buf, buflen), fd(newfd), opened(1) {}

int filebuf::is_open()
{
  return opened;
}

int filebuf::close()
{
  int was = opened;
  if (was) ::close(fd);
  opened = 0;
  return was;
}

streambuf* filebuf::open(const char* name, open_mode m)
{
  if (opened) return 0;
  int mode = -1; // any illegal value
  switch (m)
  {
  case input: mode = O_RDONLY; 
              break;
  case output: mode = O_WRONLY | O_CREAT | O_TRUNC;
              break;
  case append: mode = O_APPEND | O_CREAT | O_WRONLY;
              break;
  }
  fd = ::open(name, mode, 0666);
  if (opened = (fd >= 0))
  {
    allocate();
    return this;
  }
  else
    return 0;
}


streambuf*  filebuf::open(const char* filename, io_mode m, access_mode a)
{
  return 0;
}

streambuf* filebuf::open(const char* filename, const char* m)
{
  return 0;
}

streambuf*  filebuf::open(int  filedesc, io_mode m)
{
  return 0;
}

streambuf*  filebuf::open(FILE* fileptr)
{
  return 0;
}

int filebuf::underflow()
{
  if (!opened) return EOF;
  if (base == 0) allocate();
  int nwanted = eptr - base + 1;
  int nread = ::read(fd, base, nwanted);
  if (nread >= 0)
  {
    gptr = base;
    pptr = base + nread;
  }
  return (nread <= 0)? EOF : int(*gptr);
}

int filebuf::overflow(int ch)
{
  if (!opened) return EOF;
  if (base == 0) allocate();
  if (ch != EOF)             // overflow *must* be called before really full
    *pptr++ = (char)(ch);

  // loop, in case write can't handle full request
  // From: Rene' Seindal <seindal@diku.dk>

  int w, n, t;
  for (w = t = 0, n = pptr - base; n > 0; n -= w, t += w) 
  {
    if ((w = ::write(fd, base + t, n)) < 0)
      break;
  }
 
  pptr = base;
  return (n == 0 && w >= 0)? 0 : EOF;
}

filebuf::~filebuf()
{
  close();
}
