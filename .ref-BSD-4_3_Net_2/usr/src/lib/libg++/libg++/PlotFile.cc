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

#ifdef __GNUG__
#pragma implementation
#endif
#include <PlotFile.h>

/*
 PlotFile implementation module
*/


PlotFile:: PlotFile() {}
PlotFile::~PlotFile() {}


PlotFile::PlotFile(const char* filename,  io_mode m,  access_mode a) 
:(filename, m, a) {}

PlotFile::PlotFile(const char* filename,  const char* m)
:(filename, m) {}

PlotFile::PlotFile(int filedesc, const io_mode m)
:(filedesc, m) {}

PlotFile::PlotFile(FILE* fileptr)
:(fileptr) {}

PlotFile::operator void*()
{ 
  return (state & (_bad|_fail))? 0 : this ; 
}


PlotFile& PlotFile::open(const char* filename,
			  io_mode m,  access_mode a)
{
 File::open(filename, m, a); return *this; 
}

PlotFile& PlotFile::open(const char* filename, const char* m)
{
 File::open(filename, m); return *this; 
}

PlotFile& PlotFile::open(int  filedesc,  io_mode m)
{
 File::open(filedesc, m); return *this; 
}

PlotFile& PlotFile::open(FILE* fileptr)
{
 File::open(fileptr); return *this; 
}

PlotFile& PlotFile::setbuf(const int buffer_kind)
{
 File::setbuf(buffer_kind); return *this;
}

PlotFile& PlotFile::setbuf(const int size, char* buf)
{
  File::setbuf(size, buf); return *this;
}


PlotFile& PlotFile:: cmd(char c)
{ 
  File::put(c); 
  return *this; 
}

PlotFile& PlotFile:: operator<<(const int x)
{ 
#if defined(convex)
  File::put((char)(x>>8)); 
  File::put((char)(x&0377)); 
#else
  File::put((char)(x&0377)); 
  File::put((char)(x>>8)); 
#endif
  return *this; 
}

PlotFile& PlotFile:: operator<<(const char *s)
{ 
  File::put(s); 
  return *this;
}


PlotFile& PlotFile:: arc(const int xi, const int yi,
			 const int x0, const int y0,
			 const int x1, const int y1)
{ 
  return cmd('a') << xi << yi << x0 << y0 << x1 << y1; 
}


PlotFile& PlotFile:: box(const int x0, const int y0,
			 const int x1, const int y1)
{ 
  line(x0, y0, x0, y1);
  line(x0, y1, x1, y1);
  line(x1, y1, x1, y0);
  return line(x1, y0, x0, y0);
}

PlotFile& PlotFile:: circle(const int x, const int y, const int r)
{ 
  return cmd('c') << x << y << r; 
}

PlotFile& PlotFile:: cont(const int xi, const int yi)
{ 
  return cmd('n') << xi << yi;
}

PlotFile& PlotFile:: dot(const int xi, const int yi, const int dx,
			 int n, const int* pat)
{ 
  cmd('d') << xi << yi << dx << n;
  while (n-- > 0) *this << *pat++;
  return *this; 
}

PlotFile& PlotFile:: erase()
{ 
  return cmd('e'); 
}

PlotFile& PlotFile:: label(const char* s)
{ 
  return cmd('t') << s << "\n"; 
}

PlotFile& PlotFile:: line(const int x0, const int y0,
			  const int x1, const int y1)
{ 
  return cmd('l') << x0 << y0 << x1 << y1; 
}

PlotFile& PlotFile:: linemod(const char* s)
{ 
  return cmd('f') << s << "\n"; 
}

PlotFile& PlotFile:: move(const int xi, const int yi)
{ 
  return cmd('m') << xi << yi;
}

PlotFile& PlotFile:: point(const int xi, const int yi)
{ 
  return cmd('p') << xi << yi; 
}

PlotFile& PlotFile:: space(const int x0, const int y0,
			   const int x1, const int y1)
{ 
  return cmd('s') << x0 << y0 << x1 << y1; 
}
