// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988, 1992 Free Software Foundation
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

/* 
   a very simple implementation of a class to output unix "plot"
   format plotter files. See corresponding unix man pages for
   more details. 
*/

#ifndef _PlotFile_h
#ifdef __GNUG__
#pragma interface
#endif
#define _PlotFile_h

#include <File.h>

/*   
   Some plot libraries have the `box' command to draw boxes. Some don't.
   `box' is included here via moves & lines to allow both possiblilties.
*/


class PlotFile : private File
{
protected:
  PlotFile& cmd(char c);
  PlotFile& operator << (const int x);
  PlotFile& operator << (const char *s);
  
public:
  
  PlotFile();
  PlotFile(const char* filename);
  PlotFile(const char* filename, io_mode m, access_mode a);
  PlotFile(const char* filename, const char* m);
  PlotFile(int filedesc, const io_mode m = io_writeonly);
  PlotFile(FILE* fileptr);
  
  ~PlotFile();
  
  operator void*();
  
  PlotFile& close() { File::close(); return *this; }
  PlotFile& remove() { File::remove(); return *this; }
  
  int           filedesc() { return File::filedesc(); }
  const char*   name() { return File::name(); }
  void          setname(const char* newname) { File::setname(newname); }
  int           iocount() { return File::iocount(); }
  
  int           rdstate() { return File::rdstate(); }
  int           eof() { return File::eof(); }
  int           fail() { return File::fail(); }
  int           bad() { return File::bad(); }
  int           good() { return File::good(); }
  
  // other status queries
  
  int           readable() { return File::readable(); }
  int           writable() { return File::writable(); }
  int           is_open() { return File::is_open(); }
  
  void          error() {  File::error(); }
  void          clear(state_value f = _good) {  File::clear(f); }
  void          set(state_value f) { File::set(f); }
  void          unset(state_value f) { File::unset(f); }
  PlotFile&     failif(int cond) {  File::failif(cond); return *this; }
  void          check_state() { File::check_state(); }
  
  PlotFile&     raw() { File::raw(); return *this; }
  
  PlotFile& open(const char* filename, io_mode m, access_mode a);
  PlotFile& open(const char* filename, const char* m);
  PlotFile& open(int  filedesc,  io_mode m);
  PlotFile& open(FILE* fileptr);
  PlotFile& setbuf(const int buffer_kind); // vals: _IONBF, _IOFBF, _IOLBF
  PlotFile& setbuf(const int size, char* buf);
  
  PlotFile& arc(const int xi, const int yi,
                const int x0, const int y0,
                const int x1, const int y1);
  PlotFile& box(const int x0, const int y0,
                const int x1, const int y1);
  PlotFile& circle(const int x, const int y, const int r);
  PlotFile& cont(const int xi, const int yi);
  PlotFile& dot(const int xi, const int yi, const int dx,
                int n, const int* pat);
  PlotFile& erase(); 
  PlotFile& label(const char* s);
  PlotFile& line(const int x0, const int y0,
                 const int x1, const int y1);
  PlotFile& linemod(const char* s);
  PlotFile& move(const int xi, const int yi);
  PlotFile& point(const int xi, const int yi);
  PlotFile& space(const int x0, const int y0,
                  const int x1, const int y1);
};


#endif

































