/* 
Copyright (C) 1990 Free Software Foundation
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

#ifdef __GNUG__
#pragma implementation
#endif

#include <Filebuf.h>

#include <std.h>
#include <sys/file.h>           // needed to determine values of O_RDONLY...

#ifndef _bufsiz
#ifdef masscomp
#ifdef _UCB
#define _bufsiz(p) 4096
#endif
#else
#define _bufsiz(p) ((p)->_bufsiz)
#endif
#endif

#ifdef VMS
#include <assert.h>
#define FPOINT (*(Fp->fp))
#undef _bufsiz
#define _bufsiz(p) BUFSIZ	// we do not have this available
#else
#define FPOINT Fp->fp 
#endif

void Filebuf::init_streambuf_ptrs()
{
  if (Fp->fp == 0 || FPOINT->_cnt == 0)
  {
    base = gptr = pptr = eptr = 0; // let first over/under flow deal with it
  }
  else
  {
#ifdef VMS
    base = new char[BUFSIZ];
    alloc = 1;
#else
    base = FPOINT->_base;
#endif
    eptr = base - 1 + _bufsiz(Fp->fp);
    pptr = gptr = base;
  }
}


int Filebuf::is_open()
{
  return (Fp != 0 && Fp->is_open());
}


streambuf* Filebuf::open(const char* name, io_mode m, access_mode a)
{
  if (Fp == 0)
    Fp = new File(name, m, a);
  else
    Fp->open(name, m, a);
#ifndef VMS
  if (base != 0) Fp->setbuf(eptr-base+1, base);
#endif
  init_streambuf_ptrs();
  return this;
}

streambuf* Filebuf::open(const char* name, const char* m)
{
  if (Fp == 0)
    Fp = new File(name, m);
  else
    Fp->open(name, m);
#ifndef VMS
  if (base != 0) Fp->setbuf(eptr-base+1, base);
#endif
  init_streambuf_ptrs();
  return this;
}

streambuf*  Filebuf::open(const char* name, open_mode m)
{
  switch(m)
  {
  case input: return open(name, "r"); 
  case output: return open(name, "w");
  case append: return open(name, "a");
  }
}

streambuf* Filebuf::open(int filedesc, io_mode m)
{
  if (Fp == 0)
    Fp = new File(filedesc, m);
  else
    Fp->open(filedesc, m);
#ifndef VMS
  if (base != 0) Fp->setbuf(eptr-base+1, base);
#endif
  init_streambuf_ptrs();
  return this;
}

streambuf* Filebuf::open(FILE* fileptr)
{
  if (Fp == 0)
    Fp = new File(fileptr);
  else
    Fp->open(fileptr);
#ifndef VMS
  if (base != 0) Fp->setbuf(eptr-base+1, base);
#endif
  init_streambuf_ptrs();
  return this;
}

Filebuf::Filebuf() : streambuf(), Fp(0) {}

Filebuf::Filebuf(const char* filename, io_mode m, access_mode a)
     : streambuf()
{
  Fp = new File(filename, m, a);
  init_streambuf_ptrs();
}

Filebuf::Filebuf(const char* filename, const char* m)
     : streambuf()
{
  Fp = new File(filename, m);
  init_streambuf_ptrs();
}

Filebuf::Filebuf(int filedesc, io_mode m)
     : streambuf()
{
  Fp = new File(filedesc, m);
  init_streambuf_ptrs();
}

Filebuf::Filebuf(FILE* fileptr)
     : streambuf()
{
  Fp = new File(fileptr);
  init_streambuf_ptrs();
}

int Filebuf::close()
{
  int was = Fp->is_open();
  if (was) { overflow(); Fp->close(); }
#ifdef VMS
  if (was) {
	if(alloc && (base != 0)) {delete base; base=0; alloc = 0;};
	gptr=0;};
#endif
  return was;
}


Filebuf::~Filebuf()
{
  if (Fp != 0)
  {
    close();
    delete Fp;
  }
}

#ifdef VMS
/*
	VMS implementation notes:
 The underflow routine works fine as long as there is no mixing of input and
 output for the same stream.  If this were to happen, then great confusion
 would occur, since we maintain a seperate buffer for the case of output.
 the stream classes do not allow this as they are currently written, so for
 now we are OK.  VMS does not handle buffered output in quite the same way
 that UNIX does, so a seperate buffer is allocated, and used by the program.
 when it comes time to flush it we call write(...) to empty it.  The flush
 function under VMS does not flush the buffer unless it is full, and whatsmore
 the _iobuf is not 14 bytes long as one might suspect from the structure def,
 but at *least* 66 bytes long.  Some of these hidden structure elements need
 to be set for the output to work properly, and it seemed to be poor
 programming practice to fool with them
*/
#endif
/*
  The underflow and overflow methods sync the streambuf with the _iobuf
  ptrs on the way in and out of the read. I believe that this is
  done in a portable way.
*/
int Filebuf::underflow()
{
  int ch;
  if (Fp == 0) return EOF;
  if (gptr == 0) // stdio _iobuf ptrs not initialized until after 1st read
  {
#ifdef VMS
	assert(alloc==0);
#endif
    ch = Fp->fill();
    base = FPOINT->_base;
    eptr = base - 1 + _bufsiz(Fp->fp);
  }
  else
  {
    FPOINT->_ptr = gptr;
    FPOINT->_cnt = eptr - gptr + 1;
    ch = Fp->fill();
  }
  gptr = base;
  *gptr = ch;
  if (ch == EOF)
    pptr = base;
  else
    pptr = base + FPOINT->_cnt + 1;
  if (Fp->good())
    return ch;
  else
  {
    Fp->clear();
    return EOF;
  }
}

int Filebuf::overflow(int ch)
{
  if (Fp == 0) return EOF;
  if (FPOINT->_flag & _IONBF)  // handle unbuffered IO specially
  {
    if (pptr == 0) // 1st write
    {
      if (ch == EOF)
        return 0;
      else
      {
        Fp->flush(ch);
      }
    }
    else
    {
      if (ch == EOF)
        Fp->flush();		// Probably not necessary
      else
        Fp->flush(ch);
    }
  }
  else
  {
    if (pptr == 0) // 1st write
    {
      if (ch == EOF)
        return 0;
      else
      {
        Fp->flush(ch);
#ifdef VMS
        base = new char[BUFSIZ];
        alloc = 1;
#else
        base = FPOINT->_base;
#endif
        eptr = base - 1 + _bufsiz(Fp->fp);
        gptr = base;
      }
    }
    else
    {
      if (ch != EOF) *pptr++ = ch;
#ifdef VMS
      if(gptr==base) write(FPOINT->_file,base,pptr-base);
#else
      FPOINT->_ptr = pptr;
      FPOINT->_cnt = eptr - pptr + 1;
#endif
      Fp->flush();
    }
#ifdef VMS
    pptr = base;
#else
    pptr = FPOINT->_ptr;
#endif
  }
  if (Fp->fail() || Fp->bad())
  {
    Fp->clear(); // this allows recovery from ostream level
    return EOF;
  }
  else
    return 0;
}

const char* Filebuf::name()
{
  return Fp->name();
}

streambuf* Filebuf::setbuf(char* buf, int buflen, int preload)
{
  if (preload != 0) return 0; // cannot preload, sorry
  if (Fp != 0) Fp = new File;
  Fp->setbuf(buflen, buf);
  init_streambuf_ptrs();
  return (Fp->good())? this : 0;
}

void Filebuf::error()
{
  Fp->error();
}
