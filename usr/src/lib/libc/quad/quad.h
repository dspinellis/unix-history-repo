/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)quad.h	5.4 (Berkeley) %G%
 */

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

/* More subroutines needed by GCC output code on some machines.  */
/* Compile this one with gcc.  */
#include <sys/param.h>

#define	BITS_PER_WORD	(NBBY * sizeof(long))

/* We need this union to unpack/pack longlongs, since we don't have
   any arithmetic yet.  Incoming long long parameters are stored
   into the `ll' field, and the unpacked result is read from the struct
   longlong.  */

typedef union {
	long long ll;
	struct { long val[2]; } s;
} long_long;
#define high val[_QUAD_HIGHWORD]
#define low val[_QUAD_LOWWORD]

#define HIGH _QUAD_HIGHWORD
#define LOW _QUAD_LOWWORD

/* Internally, long long ints are strings of unsigned shorts in the
   order determined by BYTE_ORDER.  */

#define B 0x10000
#define low16 (B - 1)

#if BYTE_ORDER == BIG_ENDIAN
#define big_end(n)	0 
#define little_end(n)	((n) - 1)
#define next_msd(i)	((i) - 1)
#define next_lsd(i)	((i) + 1)
#define is_not_msd(i,n)	((i) >= 0)
#define is_not_lsd(i,n)	((i) < (n))
#else
#define big_end(n)	((n) - 1)
#define little_end(n)	0 
#define next_msd(i)	((i) + 1)
#define next_lsd(i)	((i) - 1)
#define is_not_msd(i,n)	((i) < (n))
#define is_not_lsd(i,n)	((i) >= 0)
#endif
