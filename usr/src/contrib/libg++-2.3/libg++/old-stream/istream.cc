// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1989 Free Software Foundation
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

#ifdef __GNUG__
#pragma implementation
#endif
#include <stream.h>
#include <stdarg.h>
#include <values.h>
#include <ctype.h>
#include <Obstack.h>


istream::istream(streambuf* s, int sk, ostream* t)
     : bp(s), state(_good), skipws(sk), tied_to(t), ownbuf(0) {}

istream::istream(int sz, char* buf, int sk, ostream* t)
     : state(_good), skipws(sk), tied_to(t), ownbuf(1)
{
  bp = new streambuf;
  if (buf == 0)
  {
    bp->alloc = 1;
    buf = new char[sz];
  }
  else
    bp->alloc = 0;

  bp->setbuf(buf, sz, sz);
}

istream::~istream()
{
  if (ownbuf) delete bp;
}

istream::istream(const char* filename, io_mode m, access_mode a, int sk, ostream* t)
     : state(_good), skipws(sk), tied_to(t), ownbuf(1)
{
  bp = new Filebuf(filename, m, a);
}

istream::istream(const char* filename, const char* m, int sk, ostream* t)
     : state(_good), skipws(sk), tied_to(t), ownbuf(1)
{
  bp = new Filebuf(filename, m);
}

istream::istream(int filedesc, io_mode m, int sk, ostream* t)
     : state(_good), skipws(sk), tied_to(t), ownbuf(1)
{
  bp = new Filebuf(filedesc, m);
}

istream::istream(FILE* fileptr, int sk, ostream* t)
     : state(_good), skipws(sk), tied_to(t), ownbuf(1)
{
  bp = new Filebuf(fileptr);
                  
}

istream::istream(int filedesc, int sk, ostream* t)
     : state(_good), skipws(sk), tied_to(t), ownbuf(1)
{
  bp = new filebuf(filedesc);
}

istream::istream(int filedesc, char* buf, int buflen, int sk, ostream* t)
     : state(_good), skipws(sk), tied_to(t), ownbuf(1)
{
  bp = new filebuf(filedesc, buf, buflen);
}

istream&  istream::open(const char* filename, io_mode m, access_mode a)
{
  return failif(bp->open(filename, m, a) == 0);
}

istream&  istream::open(const char* filename, const char* m)
{
  return failif(bp->open(filename, m) == 0);
}

istream&  istream::open(int  filedesc, io_mode m)
{
  return failif(bp->open(filedesc, m) == 0);
}

istream&  istream::open(FILE* fileptr)
{
  return failif(bp->open(fileptr) == 0);
}

istream&  istream::open(const char* filenam, open_mode m)
{
  return failif(bp->open(filenam, m) == 0);
}

istream& istream::get(char& c)
{
  if (good())
  {
    if(tied_to != 0) tied_to->flush();
    int ch = bp->sgetc();
    if (ch == EOF) 
      set(_eof);
    else
    {
      c = ch;
      bp->stossc();
    }
  }
  return *this;
}


istream& istream::operator >> (whitespace&)
{
  if (good())
  {
    int ch;
    if(tied_to != 0) tied_to->flush();
    while (((ch = bp->sgetc()) != EOF) && isspace(ch)) bp->stossc();
    if (ch == EOF) set(_eof);
  }
  return *this;
}


istream& istream::operator >> (char& c)
{
  if (skipws) (*this >> WS);
  return get(c);
}

istream& istream::get(char* s, int n, char terminator)
{
  if (!readable())
  {
    set(_fail);
    return *this;
  }

  char ch = 0;
  char* start = s;
  if (--n > 0 && get(ch))
  {
    if (ch == terminator) 
      unget(ch);
    else
    {
      *s++ = ch; --n;
      while (n-- > 0 && get(ch))
      {
        if (ch == terminator)
        {
          unget(ch);
          break;
        }
        else
          *s++ = ch;
      }
    }
  }

  *s = 0;
  if (s != start) clear();
  return *this;
}


istream& istream::operator >> (char* s)
{
  if (!readable() || s == 0)
  {
    set(_fail);
    return *this;
  }

  if (skipws && !(*this >> WS)) return *this;

  char ch;
  char* start = s;
  if (get(ch))
  {
    *s++ = ch;
    while (get(ch))
    {
      if (isspace(ch))
      {
        unget(ch);
        break;
      }
      else
        *s++ = ch;
    }
  }

  *s = 0;
  if (s != start) clear();
  return *this;
}


istream& istream::getline(char* s, int n, char terminator)
{
  if (!readable())
  {
    set(_fail);
    return *this;
  }
  char* start = s;
  char ch;
  while (--n > 0 && get(ch) && ((*s++ = ch) != terminator));

  *s = 0;
  if (s != start) clear();
  return *this;
}

// from Doug Schmidt

// This should probably be a page size....
#define CHUNK_SIZE 512

/* Reads an arbitrarily long input line terminated by a user-specified
   TERMINATOR.  Super-nifty trick using recursion avoids unnecessary calls
   to NEW! */

char *istream::readline (int chunk_number, char terminator) 
{
  char buf[CHUNK_SIZE];
  register char *bufptr = buf;
  register char *ptr;
  char ch;
  int continu;

  while ((continu = !!get(ch)) && ch != terminator) /* fill the current buffer */
    {
      *bufptr++ = ch;
      if (bufptr - buf >= CHUNK_SIZE) /* prepend remainder to ptr buffer */
        {
          if (ptr = readline (chunk_number + 1, terminator))

            for (; bufptr != buf; *--ptr = *--bufptr);

          return ptr;
        }
    }
  if (!continu && bufptr == buf)
    return NULL;

  int size = (chunk_number * CHUNK_SIZE + bufptr - buf) + 1;

  if (ptr = new char[size])
    {

      for (*(ptr += (size - 1)) = '\0'; bufptr != buf; *--ptr = *--bufptr)
        ;

      return ptr;
    } 
  else 
    return NULL;
}

/* Reads an arbitrarily long input line terminated by TERMINATOR.
   This routine allocates its own memory, so the user should
   only supply the address of a (char *). */

istream& istream::gets(char **s, char terminator)
{
  return failif(!readable() || !(*s = readline (0, terminator)));
}
  
istream& istream::operator >> (long& y)
{
  if (!readable())
  {
    set(_bad);
    return *this;
  }

  int got_one = 0;
  char sgn = 0;
  char ch;
  y = 0;
  if (skipws) *this >> WS;
  if (!good()) 
  {
    set(_bad);
    return *this;
  }
  while (get(ch))
  {
    if (ch == '-')
    {
      if (sgn == 0 && got_one == 0)
        sgn = '-';
      else
        break;
    }
    else if (ch >= '0' && ch <= '9')
      y = y * 10 + ((got_one = ch) - '0');
    else
      break;
  }
  if (good())
    unget(ch);
  if (!got_one)
    set(_bad);
  else
    clear();

  if (sgn == '-')
    y = -y;

  return *this;
}

istream& istream::operator >> (unsigned long& y)
{
  if (!readable())
  {
    set(_bad);
    return *this;
  }

  int got_one = 0;
  char ch;
  y = 0;
  if (skipws) *this >> WS;
  if (!good())
  while (get(ch))
  {
    if (ch >= '0' && ch <= '9')
      y = y * 10 + ((got_one = ch) - '0');
    else
      break;
  }
  if (good())
    unget(ch);
  if (!got_one)
    set(_bad);
  else
    clear();
  return *this;
}


/* for input to a double, we must trust atof (cannot even use
the better strtod since it is not universally supported). So
guaranteed legal chars are gathered up into an obstack. The
only possible, undiagnosable error is that the input number
might give a floating overflow or underflow inside atof. 
I know of no way to avoid this */

extern Obstack _libgxx_io_ob;

istream& istream::operator >> (double & y)
{
  if (!readable())
  {
    set(_bad);
    return *this;
  }


  char seenint = 0;
  char seendec = 0;
  char seenexp = 0;
  char seensgn = 0;
  char seene = 0;
  char seenexpsgn = 0;
  char seendot = 0;
  char ch;

  if (skipws) *this >> WS;
  if (!good()) 
  {
    set(_bad);
    return *this;
  }
  while (get(ch))
  {
    if (ch == '-' || ch == '+')
    {
      if (seene && !seenexpsgn)
        _libgxx_io_ob.grow(seenexpsgn = ch);
      else if (!seensgn && !seenint)
        _libgxx_io_ob.grow(seensgn = ch);
      else
        break;
    }
    else if (ch == '.' && !seendot)
    {
      _libgxx_io_ob.grow(seendot = ch);
    }
    else if ((ch == 'e' || ch == 'E') && !seene)
    {
      _libgxx_io_ob.grow(seene = ch);
    }
    else if (ch >= '0' && ch <= '9')
    {
      _libgxx_io_ob.grow(ch);
      if (seene) seenexp = ch;
      else if (seendot) seendec = ch;
      else seenint = ch;
    }
    else
      break;
  }
  char* str = (char *) _libgxx_io_ob.finish(0);
  if (good())
    unget(ch);
  if ((seenint || seendec) && (!seene || seenexp))
    y = atof(str);
  else
    set(_bad);
  _libgxx_io_ob.free(str);
  return *this;
}

istream& istream::operator >> (int& y)
{
  long l; (*this >> l); y = int(l); return *this;
}

istream& istream:: operator >> (unsigned int& y)
{
  long l; (*this >> l); y = (unsigned int)(l); return *this;
}

istream& istream:: operator >> (short& y)
{
  long l; (*this >> l); y = short(l); return *this;
}

istream& istream:: operator >> (unsigned short& y)
{
  long l; (*this >> l); y = (unsigned short)(l); return *this;
}

istream& istream:: operator >> (float& y)
{
  double d; (*this >> d); y = float(d); return *this;
}

const char* istream::name()
{
  return bp->name();
}

void istream::error()
{
  bp->error();
}

ostream* istream::tie(ostream* s)
{
  ostream* was = tied_to; tied_to = s; return was;
}

void istream::_flush()
{
  if(tied_to != 0) tied_to->flush();
}


//--------------------------------------------------------------

extern ostream cout;

#ifndef DEFAULT_filebuf

istream  cin(stdin, 1, &cout);

#else

static char cinbuf[BUFSIZE];
istream cin (0, cinbuf,  BUFSIZE, 1, &cout);

#endif

whitespace WS;
whitespace ws;
