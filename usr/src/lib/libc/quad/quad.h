/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This software was developed by the Computer Systems Engineering group
 * at Lawrence Berkeley Laboratory under DARPA contract BG 91-66 and
 * contributed to Berkeley.
 *
 * %sccs.include.redist.c%
 */

/*
 * Quad arithmetic.
 *
 * This library makes the following assumptions:
 *
 *  - The type long long (aka quad) exists.
 *
 *  - A quad variable is exactly twice as long as `long'.
 *
 *  - The machine's arithmetic is two's complement.
 *
 * All other machine parameters are encapsulated here.  This library can
 * provide 128-bit arithmetic on a machine with 128-bit quads and 64-bit
 * longs, for instance, or 96-bit arithmetic on machines with 48-bit longs.
 */

#ifndef SPARC_XXX
#include <machine/endian.h>		/* see #else case */
#else
/*
 * These are for testing and for illustration: we expect <machine/endian.h>
 * to define these.  Actually, these match most big-endian machines; for
 * most little-endian machines, all you need do is exchange _QUAD_HIGHWORD
 * and _QUAD_LOWWORD.
 */
#define _QUAD_HIGHWORD 0
#define _QUAD_LOWWORD 1
#endif

typedef long long quad;
typedef unsigned long long u_quad;
typedef unsigned long u_long;

#include <limits.h>
/*
 * We expect something like these from <limits.h>, which should be provided on
 * any ANSI C system.
#define	USHRT_MAX	0xffff
#define	CHAR_BIT	8
 */

/*
 * Depending on the desired operation, we view a `long long' (aka quad) in
 * one or more of the following formats.
 */
union uu {
	quad	q;		/* as a (signed) quad */
	quad	uq;		/* as an unsigned quad */
	long	sl[2];		/* as two signed longs */
	u_long	ul[2];		/* as two unsigned longs */
};

/*
 * Define high and low longwords.
 */
#define	H		_QUAD_HIGHWORD
#define	L		_QUAD_LOWWORD

/*
 * Total number of bits in a quad and in the pieces that make it up.
 * These are used for shifting, and also below for halfword extraction
 * and assembly.
 */
#define	QUAD_BITS	(sizeof(quad) * CHAR_BIT)
#define	LONG_BITS	(sizeof(long) * CHAR_BIT)
#define	HALF_BITS	(sizeof(long) * CHAR_BIT / 2)

/*
 * Extract high and low shortwords from longword, and move low shortword of
 * longword to upper half of long, i.e., produce the upper longword of
 * ((quad)(x) << (number_of_bits_in_long/2)).  (`x' must actually be u_long.)
 *
 * These are used in the multiply code, to split a longword into upper
 * and lower halves, and to reassemble a product as a quad, shifted left
 * (sizeof(long)*CHAR_BIT/2).
 */
#define	HHALF(x)	((x) >> HALF_BITS)
#define	LHALF(x)	((x) & ((1 << HALF_BITS) - 1))
#define	LHUP(x)		((x) << HALF_BITS)

extern u_quad __qdivrem(u_quad u, u_quad v, u_quad *rem);
