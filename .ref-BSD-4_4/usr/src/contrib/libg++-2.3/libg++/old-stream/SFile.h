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

#ifndef _SFile_h
#ifdef __GNUG__
#pragma interface
#endif
#define _SFile_h 1

#include <File.h>

class SFile: public File
{
protected:
  int       sz;                   // unit size for structured binary IO

public:
            SFile();
            SFile(const char* filename, int size, io_mode m, access_mode a);
            SFile(const char* filename, int size, const char* m);
            SFile(int filedesc, int size, io_mode m);
            SFile(FILE* fileptr,  int size);

            ~SFile();

  int       size();
  int       setsize(int s);

  SFile&    get(void* x);
  SFile&    put(void* x);
  SFile&    operator[](long i);
};

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)


inline  int SFile::size()
{ 
  return sz; 
}          

inline  int SFile::setsize(int s)
{ 
  int old = sz; 
  sz = s;  
  return old; 
}

inline  SFile& SFile::get(void* x)
{ 
  read(x, sz, 1);  return *this; 
}

inline  SFile& SFile::put(void* x)
{ 
  write(x, sz, 1);  return *this; 
}

inline SFile&  SFile::operator[](long i)
{ 
  seek(i * sz, 0);  return *this; 
}


#endif

#endif
