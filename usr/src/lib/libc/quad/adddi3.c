/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)adddi3.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

/* Copyright (C) 1989, 1992 Free Software Foundation, Inc.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

/* As a special exception, if you link this library with files
   compiled with GCC to produce an executable, this does not cause
   the resulting executable to be covered by the GNU General Public License.
   This exception does not however invalidate any other reasons why
   the executable file might be covered by the GNU General Public License.  */

#include "longlong.h"

static int badd ();

long long 
__adddi3 (u, v)
     long long u, v;
{
  long a[2], b[2], c[2];
  long_long w;
  long_long uu, vv;

  uu.ll = u;
  vv.ll = v;

  a[HIGH] = uu.s.high;
  a[LOW] = uu.s.low;
  b[HIGH] = vv.s.high;
  b[LOW] = vv.s.low;

  badd (a, b, c, sizeof c);

  w.s.high = c[HIGH];
  w.s.low = c[LOW];
  return w.ll;
}

static int 
badd (a, b, c, n)
     unsigned short *a, *b, *c;
     unsigned long n;
{
  unsigned long acc;
  int i;

  n /= sizeof *c;

  acc = 0;
  for (i = little_end (n); is_not_msd (i, n); i = next_msd (i))
    {
      /* Widen before adding to avoid loss of high bits.  */
      acc += (unsigned long) a[i] + b[i];
      c[i] = acc & low16;
      acc = acc >> 16;
    }
  return acc;
}
