// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1989, 1992 Free Software Foundation
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

/* *** Version 1.2 -- nearly 100% AT&T 1.2 compatible *** */

/* istream.h now separately includable */

#ifndef _istream_h
#ifdef __GNUG__
#pragma interface
#endif
#define _istream_h 1


#include <File.h>
#include <streambuf.h>
#include <filebuf.h>
#include <Filebuf.h>

class whitespace                // a class used only to input and
{                               // discard white space characters
  char filler;                     
};

class ostream;

class istream
{
  friend void   eatwhite(istream& s);
protected:
  streambuf*    bp;
  state_value   state;           // _good/_eof/_fail/_bad
  ostream*      tied_to;
  char          skipws;
  char          ownbuf;
  void          _flush();
  char*         readline (int chunk_number, char terminator);
  
public:
                istream(const char* filename, io_mode m, access_mode a, 
                        int sk=1, ostream* t = 0);
                istream(const char* filename, const char* m, 
                        int sk=1, ostream* t = 0);
                istream(int filedesc, io_mode m, int sk=1, ostream* t = 0);
                istream(FILE* fileptr, int sk=1, ostream* t = 0);
                istream(int sz, char* buf, int sk=1, ostream* t = 0);
                istream(int filedesc, int sk=1, ostream* t = 0);
                istream(int filedesc, char* buf, int buflen, 
                        int sk, ostream* t = 0);
                istream(streambuf* s, int sk=1, ostream* t = 0);

               ~istream();

  istream&      open(const char* filename, io_mode m, access_mode a);
  istream&      open(const char* filename, const char* m);
  istream&      open(int  filedesc, io_mode m);
  istream&      open(FILE* fileptr);
  istream&      open(const char* filenam, open_mode m);

  istream&      close();

  ostream*      tie(ostream* s);
  int           skip(int);

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
  void          unset(state_value f); // clear corresponding bit
  istream&      failif(int cond);

// unformatted IO

  istream&      get(char& c);
  istream&      unget(char c);
  istream&      putback(char c); // a synonym for unget

  istream&      get    (char* s, int n, char terminator = '\n');
  istream&      getline(char* s, int n, char terminator = '\n');
  istream&      gets   (char **s, char terminator = '\n');


  istream&      operator >> (char&   c);
  istream&      operator >> (short&  n);
  istream&      operator >> (unsigned short& n);
  istream&      operator >> (int&    n);
  istream&      operator >> (unsigned int& n);
  istream&      operator >> (long&   n);
  istream&      operator >> (unsigned long& n);
#ifdef __GNUG__
  istream&      operator >> (long long& n);
  istream&      operator >> (unsigned long long& n);
#endif
  istream&      operator >> (float&  n);
  istream&      operator >> (double& n);
  istream&      operator >> (char*   s);
  istream&      operator >> (whitespace& w);
};

// pre-declared streams

extern istream  cin;             // stdin

extern whitespace WS;            // for convenience (Old name)
extern whitespace ws;            // for convenience (iostream-style name)

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)


inline void istream::clear(state_value flag)
{
  state = flag;
}

inline void istream::set(state_value flag)
{
  state = state_value(int(state) | int(flag));
}

inline void istream::unset(state_value flag)
{
  state = state_value(int(state) & ~int(flag));
}

inline int istream::rdstate()
{
  return int(state);
}

inline int istream::good()
{
  return state == _good;
}

inline int istream::eof()
{
  return int(state) & int(_eof);
}

inline int istream::fail()
{
  return int(state) & int(_fail);
}

inline int istream::bad()
{
  return int(state) & int(_bad);
}

inline istream::operator void*()
{
  return (state == _good)? this : 0;
}

inline int istream::operator !()
{
  return (state != _good);
}

inline istream& istream::failif(int cond)
{
  if (cond) set(_fail); return *this;
}

inline int istream::is_open()
{
  return bp->is_open();
}

inline int istream::readable()
{
  return (bp != 0) && (bp->is_open()) && (state == _good);
}

inline int istream::writable()
{
  return 0;
}


inline char* istream::bufptr()
{
  return bp->base;
}


inline istream& istream::close()
{
  bp->close();  return *this;
}


inline int istream::skip(int sk)
{
  int was = skipws; skipws = sk; return was;
}


inline istream& istream::unget(char c)
{
  if (bp->sputbackc(c) == EOF) set(_fail); return *this;
}

inline istream& istream::putback(char c)
{
  if (bp->sputbackc(c) == EOF) set(_fail); return *this;
}

inline void eatwhite(istream& s)
{
  s >> WS;
}

#endif


#endif
