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

#ifndef _filebuf_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _filebuf_h 1

#include <streambuf.h>
#include <stdio.h>

class filebuf: public streambuf
{
public:
  int         fd;
  char        opened;

  int         overflow(int c = EOF);
  int         underflow();

              filebuf();
              filebuf(int newfd);
              filebuf(int newfd, char* buf, int buflen);
              filebuf(const char* filename, io_mode m, access_mode a);
              filebuf(const char* filename, const char* m);   
              filebuf(int filedesc, io_mode m);
              filebuf(FILE* fileptr);

             ~filebuf();

  streambuf*  open(const char* name, open_mode m);
  streambuf*  open(const char* filename, io_mode m, access_mode a);
  streambuf*  open(const char* filename, const char* m);
  streambuf*  open(int  filedesc, io_mode m);
  streambuf*  open(FILE* fileptr);
  int         is_open();
  int         close();
};


#endif
