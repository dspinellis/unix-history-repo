/* 
Copyright (C) 1990 Free Software Foundation
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

#ifdef __GNUG__
#pragma implementation
#endif
#include <values.h>
#include <builtin.h>
#include <AllocRing.h>

extern AllocRing _libgxx_fmtq;

char* itoa(long x, int base, int width)
{
  int wrksiz;
  if (base == 10)
    wrksiz = width + 13;
  else
    wrksiz = (BITS(long) + 1) / lg(base) + 1 + width + 1;

  char* fmtbase = (char *) _libgxx_fmtq.alloc(wrksiz);
  char* e = fmtbase + wrksiz - 1;
  char* s = e;
  *--s = 0;
  char sgn = 0;

  if (x == 0)
    *--s = '0';
  else
  {
    unsigned int z;
    if (x < 0)
    {
      sgn = '-';
      z = -x;
    }
    else
      z = x;
    while (z != 0)
    {
      char ch = char(z % base);
      z = z / base;
      if (ch >= 10)
        ch += 'a' - 10;
      else
        ch += '0';
      *--s = ch;
    }
  }

  if (sgn) *--s = sgn;
  int w = e - s - 1;
  while (w++ < width)
    *--s = ' ';
  return s;
}

char* itoa(unsigned long x,  int base, int width)
{
  int wrksiz;
  if (base == 10)
    wrksiz = width + 13;
  else
    wrksiz = (BITS(long) + 1) / lg(base) + 1 + width + 1;

  char* fmtbase = (char *) _libgxx_fmtq.alloc(wrksiz);
  char* e = fmtbase + wrksiz - 1;
  char* s = e;
  *--s = 0;

  if (x == 0)
    *--s = '0';
  else
  {
    unsigned int b = base;
    while (x != 0)
    {
      char ch = char(x % b);
      x = x / b;
      if (ch >= 10)
        ch += 'a' - 10;
      else
        ch += '0';
      *--s = ch;
    }
  }

  int w = e - s - 1;
  while (w++ < width)
    *--s = ' ';
  return s;
}

#ifdef __GNUG__
#ifndef VMS
char* itoa(long long x, int base, int width)
{
  int wrksiz;
  if (base == 10)
    wrksiz = width + 23;
  else
    wrksiz = (BITS(long long) + 1) / lg(base) + 1 + width + 1;

  char* fmtbase = (char *) _libgxx_fmtq.alloc(wrksiz);
  char* e = fmtbase + wrksiz - 1;
  char* s = e;
  *--s = 0;
  char sgn = 0;

  if (x == 0)
    *--s = '0';
  else
  {
    long long z;
    if (x < 0)
    {
      sgn = '-';
      z = -x;
    }
    else
      z = x;
    while (z != 0)
    {
      char ch = char(z % base);
      z = z / base;
      if (ch >= 10)
        ch += 'a' - 10;
      else
        ch += '0';
      *--s = ch;
    }
  }

  if (sgn) *--s = sgn;
  int w = e - s - 1;
  while (w++ < width)
    *--s = ' ';
  return s;
}

char* itoa(unsigned long long x,  int base, int width)
{
  int wrksiz;
  if (base == 10)
    wrksiz = width + 23;
  else
    wrksiz = (BITS(long long) + 1) / lg(base) + 1 + width + 1;

  char* fmtbase = (char *) _libgxx_fmtq.alloc(wrksiz);
  char* e = fmtbase + wrksiz - 1;
  char* s = e;
  *--s = 0;

  if (x == 0)
    *--s = '0';
  else
  {
    unsigned int b = base;
    while (x != 0)
    {
      char ch = char(x % b);
      x = x / b;
      if (ch >= 10)
        ch += 'a' - 10;
      else
        ch += '0';
      *--s = ch;
    }
  }

  int w = e - s - 1;
  while (w++ < width)
    *--s = ' ';
  return s;
}
#endif
#endif

char* hex(long i, int width)
{
  return itoa(i, 16, width);
}

char* oct(long i, int width)
{
  return itoa(i, 8, width);
}

char* dec(long i, int width)
{
  return itoa(i, 10, width);
}

char* hex(unsigned long i, int width)
{
  return itoa(i, 16, width);
}

char* oct(unsigned long i, int width)
{
  return itoa(i, 8, width);
}

char* dec(unsigned long i, int width)
{
  return itoa(i, 10, width);
}
