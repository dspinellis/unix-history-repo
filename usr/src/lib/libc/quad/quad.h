/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)quad.h	5.3 (Berkeley) %G%
 */

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
