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

#ifndef _File_h 
#ifdef __GNUG__
#pragma interface
#endif
#define _File_h 1

#include <builtin.h>
#include <stdio.h>
#include <stddef.h>

#include <Fmodes.h>

class Filebuf;

class File
{
  friend class  Filebuf;
protected:
  FILE*         fp;              // _iobuf file pointer
  char*         nm;              // file name (dynamically allocated)
  char          rw;              //  1 = read; 2 = write; 3 = readwrite
                                 //  bit 2 (4) means read/write into string
  state_value   state;           // _good/_eof/_fail/_bad
  long          stat;            // last read/write/... return value

  void          initialize();
  void          reinitialize(const char*);
  char         *readline (int chunk_number, char terminator);

public:
                File();
                File(const char* filename, io_mode m, access_mode a);
                File(const char* filename, const char* m);   
                File(int filedesc, io_mode m);
                File(FILE* fileptr);
                File(int sz, char* buf, io_mode m);

                ~File();

// binding, rebinding, unbinding to physical files

  File&         open(const char* filename, io_mode m, access_mode a);
  File&         open(const char* filename, const char* m);
  File&         open(int  filedesc, io_mode m);
  File&         open(FILE* fileptr);

  File&         close();
  File&         remove();

// class variable access

  int           filedesc();
  const char*   name();
  void          setname(const char* newname);
  int           iocount();

  int           rdstate();
  int           eof();
  int           fail();
  int           bad();
  int           good();

// other status queries

  int           readable();
  int           writable();
  int           is_open();

                operator void*();

// error handling

  void          error();
  void          clear(state_value f = _good); // poorly named
  void          set(state_value f); // set corresponding but
  void          unset(state_value f); // clear corresponding bit
  File&         failif(int cond);
  void          check_state();

// character IO

  File&         get(char& c);
  File&         put(char  c);
  File&         unget(char c);
  File&         putback(char c); // a synonym for unget

// char* IO

  File&         put(const char* s);
  File&         get    (char* s, int n, char terminator = '\n');
  File&         getline(char* s, int n, char terminator = '\n');
  File&         gets   (char **s, char terminator = '\n');

// binary IO

  File&         read(void* x, int sz, int n);
  File&         write(const void* x, int sz, int n);

// formatted IO

  File&         form(const char* ...);
  File&         scan(const char* ...);

// buffer IO

  File&         flush();
  File&         flush(char ch); // call stdio _flsbuf
  int           fill();         // call stdio _filbuf

// position control

  File&         seek(long pos, int seek_mode=0); // default seek mode=absolute
  long          tell();

// buffer control

  File&         setbuf(int buffer_kind); // legal vals: _IONBF, _IOFBF, _IOLBF
  File&         setbuf(int size, char* buf);
  File&         raw();
};


// error handlers

extern void  verbose_File_error_handler(const char*);
extern void  quiet_File_error_handler(const char*);
extern void  fatal_File_error_handler(const char*);
extern one_arg_error_handler_t File_error_handler;
extern one_arg_error_handler_t set_File_error_handler(one_arg_error_handler_t);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)



inline int File::filedesc()
{ 
  return fileno(fp);
}

inline const char* File::name()
{ 
  return nm; 
}

inline int File::iocount()
{ 
  return stat; 
}

inline void File::clear(state_value flag)
{ 
  state = flag;
}

inline void File::set(state_value flag)
{ 
  state = state_value(int(state) | int(flag));
}

inline void File::unset(state_value flag)
{ 
  state = state_value(int(state) & ~int(flag));
}

inline int File::readable()
{ 
  if (fp != 0) { if (feof(fp)) set(_eof); if (ferror(fp)) set(_bad);}
  return (state == _good && (rw & 01));
}

inline int File::writable()
{ 
  if (fp != 0 && ferror(fp)) set(_bad);
  return ((int(state) & (int(_fail)|int(_bad))) == 0 && (rw & 02));
}

inline int File::is_open()
{ 
  return (fp != 0);
}


inline File& File::raw()
{ 
  return this->File::setbuf(_IONBF); 
}


inline File& File::failif(int cond)
{ 
  if (cond) set(_fail);  return *this; 
}

inline File& File::get(char& c)
{ 
  if (readable())
  {
    int ch = getc(fp);
    c = ch;
    failif (ch == EOF);
  }
  return *this;
}

inline File& File::put(char  c) 
{ 
  return failif (!writable() ||  putc(c, fp) == EOF);
}

inline File& File::unget(char c)
{ 
  return failif(!is_open() || !(rw & 01) || ungetc(c, fp) == EOF);
} 

inline File& File::putback(char c)
{ 
  return failif (!is_open() || !(rw & 01) || ungetc(c, fp) == EOF);
}

inline File& File::read(void* x, int sz, int n)
{ 
  return failif (!readable() || (stat = fread(x, sz, n, fp)) != n);
} 

inline File& File::write(void* x, int sz, int n) 
{ 
  return failif (!writable() || (stat = fwrite(x, sz, n, fp)) != n);
}

inline File& File::flush()
{ 
  return failif(!is_open() || fflush(fp) == EOF);
}

inline File& File::flush(char ch)
{ 
#ifdef VMS
  return failif(!is_open() || c$$flsbuf(ch, fp) == EOF);
#else
  return failif(!is_open() || _flsbuf(ch, fp) == EOF);
#endif
}

inline int File::fill()
{ 
#ifdef VMS
  failif(!is_open() || (stat = c$$filbuf(fp)) == EOF);
#else
  failif(!is_open() || (stat = _filbuf(fp)) == EOF);
#endif
  return stat;
}

inline File& File::seek(long pos, int seek_mode)
{ 
  return failif (!is_open() || fseek(fp, pos, seek_mode) < 0); 
}

inline long File::tell()
{ 
  failif (!is_open() || ((stat = ftell(fp)) < 0));
  return stat;
}

inline int File::rdstate()
{ 
  check_state();  return state; // check_state is necessary in rare but
}                               // possible circumstances

inline File::operator void*()
{ 
  check_state();  return (int(state) & (int(_bad)|int(_fail)))? 0 : this ; 
}

inline int File::eof()
{ 
  check_state(); return state & _eof; 
}

inline int File::fail()
{ 
  check_state(); return state & _fail; 
}

inline int File::bad()
{ 
  check_state(); return state & _bad; 
}

inline int File::good()
{ 
  check_state(); return rdstate() == _good; 
}


#endif

#endif
