// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1989 Free Software Foundation
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

/* *** Version 1.2 -- nearly 100% AT&T 1.2 compatible *** */

/* ostream.h now  separately includable */

#ifndef _ostream_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _ostream_h 1

/* uncomment the next line to disable    ostream << char */
//#define NO_OUTPUT_CHAR


#include <File.h>
#include <streambuf.h>
#include <filebuf.h>
#include <Filebuf.h>

class istream;

class ostream
{
  friend class istream;
protected:
  streambuf*    bp;
  state_value   state;           // _good/_eof/_fail/_bad
  char          ownbuf;          // true if we own *bp
  
public:
                ostream(const char* filename, io_mode m, access_mode a);
                ostream(const char* filename, const char* m);
                ostream(int filedesc, io_mode m);
                ostream(FILE* fileptr);
                ostream(int sz, char* buf);
                ostream(int filedesc, char* buf, int buflen);
                ostream(int filedesc);
                ostream(streambuf* s);

               ~ostream();

  ostream&      open(const char* filename, io_mode m, access_mode a);
  ostream&      open(const char* filename, const char* m);
  ostream&      open(int  filedesc, io_mode m);
  ostream&      open(FILE* fileptr);
  ostream&      open(const char* filenam, open_mode m);

  ostream&      close();
  ostream&      flush();

// stream status

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
  int           operator !();

  const char*   name();

  char*         bufptr();

// error handling

  void          error();
  void          clear(state_value f = _good); // poorly named
  void          set(state_value f); // set corresponding bit
  void          unset(state_value); // clear corresponding bit
  ostream&      failif(int cond);

// unformatted IO

  ostream&      put(char  c);
  ostream&      put(const char* s);
  ostream&      put(const char* s, int slen);
           
// formatted IO

  ostream&      form(const char* fmt, ...);           

  ostream&      operator << (short  n);
  ostream&      operator << (unsigned short n);
  ostream&      operator << (int    n);
  ostream&      operator << (unsigned int n);
  ostream&      operator << (long   n);
  ostream&      operator << (unsigned long n);
#ifdef __GNUG__
  ostream&      operator << (long long n);
  ostream&      operator << (unsigned long long n);
#endif __GNUG__
  ostream&      operator << (float  n);
  ostream&      operator << (double n);
  ostream&      operator << (const char* s);
  ostream&      operator << (const void* ptr);

#ifndef NO_OUTPUT_CHAR
  ostream&      operator << (char   c);
#endif

};

extern ostream  cout;            // stdout
extern ostream  cerr;            // stderr

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)


inline void ostream::clear(state_value flag)
{
  state = flag;
}

inline void ostream::set(state_value flag)
{
  state = state_value(int(state) | int(flag));
}

inline void ostream::unset(state_value flag)
{
  state = state_value(int(state) & ~int(flag));
}

inline int ostream::rdstate()
{
  return int(state);
}

inline int ostream::good()
{
  return state == _good;
}

inline int ostream::eof()
{
  return int(state) & int(_eof);
}

inline int ostream::fail()
{
  return int(state) & int(_fail);
}

inline int ostream::bad()
{
  return int(state) & int(_bad);
}

inline ostream::operator void*()
{
  return (state == _good)? this : 0;
}

inline int ostream::operator !()
{
  return (state != _good);
}

inline ostream& ostream::failif(int cond)
{
  if (cond) set(_fail); return *this;
}

inline int ostream::is_open()
{
  return bp->is_open();
}

inline int ostream::readable()
{
  return 0;
}

inline int ostream::writable()
{
  return (bp != 0) && (state == _good);
}


inline char* ostream::bufptr()
{
  return bp->base;
}

inline ostream& ostream::flush()
{
  bp->overflow(); return *this;
}

inline ostream& ostream::close()
{
  bp->overflow(); bp->close();  return *this;
}

inline ostream& ostream::put(char ch)
{
  return failif((state != _good) || bp->sputc((int)ch &0xff) == EOF);
}

#ifndef NO_OUTPUT_CHAR
inline ostream& ostream::operator << (char ch)
{
  return failif((state != _good) || bp->sputc((int)ch &0xff) == EOF);
}
#endif

inline ostream& ostream::put(const char* s)
{
  return failif((state != _good) || bp->sputs(s) == EOF);
}

inline ostream& ostream::put(const char* s, int len)
{
  return failif((state != _good) || bp->sputsn(s, len) == EOF);
}

inline ostream& ostream::operator << (const char* s)
{
  return failif((state != _good) || bp->sputs(s) == EOF);
}

#endif

#endif
