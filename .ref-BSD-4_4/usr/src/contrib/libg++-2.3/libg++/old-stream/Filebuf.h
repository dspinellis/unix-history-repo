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

#ifndef _Filebuf_h
#ifdef __GNUG__
#pragma interface
#endif
#define _Filebuf_h 1

#include <File.h>
#include <streambuf.h>

class Filebuf: public streambuf // libg++ File version
{
public:
  File*       Fp;

  void        init_streambuf_ptrs();

  int         overflow(int c = EOF);
  int         underflow();

              Filebuf();
              Filebuf(const char* filename, io_mode m, access_mode a);
              Filebuf(const char* filename, const char* m);   
              Filebuf(int filedesc, io_mode m);
              Filebuf(FILE* fileptr);
  
             ~Filebuf();

  const char* name();
  streambuf*  setbuf(char* buf, int buflen, int preloaded_count = 0);

  streambuf*  open(const char* name, open_mode m);
  streambuf*  open(const char* filename, io_mode m, access_mode a);
  streambuf*  open(const char* filename, const char* m);
  streambuf*  open(int  filedesc, io_mode m);
  streambuf*  open(FILE* fileptr);

  int         is_open();
  int         close();

  void        error();
};


#endif
