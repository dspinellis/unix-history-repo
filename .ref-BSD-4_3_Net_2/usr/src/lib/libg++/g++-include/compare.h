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

#ifndef _compare_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _compare_h 1

#include <builtin.h>

int compare(int a, int b);
int compare(short a, short b);
int compare(char a, char b);
int compare(unsigned long a, unsigned long b);
int compare(unsigned int a, unsigned int b);
int compare(unsigned short a, unsigned short b);
int compare(unsigned char a, unsigned char b);
int compare(float a, float b);
int compare(double a, double b);
int compare(const char* a, const char* b);

#if defined(__OPTIMIZE__) || defined(USE_LIBGXX_INLINES)

inline int compare(int a, int b)
{
  return a - b;
}

inline int compare(short a, short b)
{
  return a - b;
}

inline int compare(char a, char b)
{
  return a - b;
}

inline int compare(unsigned long a, unsigned long b)
{
  return (a < b)? -1 : (a > b)? 1 : 0;
}

inline int compare(unsigned int a, unsigned int b)
{
  return (a < b)? -1 : (a > b)? 1 : 0;
}

inline int compare(unsigned short a, unsigned short b)
{
  return (a < b)? -1 : (a > b)? 1 : 0;
}

inline int compare(unsigned char a, unsigned char b)
{
  return (a < b)? -1 : (a > b)? 1 : 0;
}

inline int compare(float a, float b)
{
  return (a < b)? -1 : (a > b)? 1 : 0;
}

inline int compare(double a, double b)
{
  return (a < b)? -1 : (a > b)? 1 : 0;
}

inline int compare(const char* a, const char* b)
{
  return strcmp(a,b);
}

#endif
#endif
