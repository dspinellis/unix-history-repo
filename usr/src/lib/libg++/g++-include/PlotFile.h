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

/* 
   a very simple implementation of a class to output unix "plot"
   format plotter files. See corresponding unix man pages for
   more details. 
*/

#ifndef _PlotFile_h
#ifdef __GNUG__
#pragma once
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

































