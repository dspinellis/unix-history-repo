/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Landon Curt Noll.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)primes.h	8.2 (Berkeley) %G%
 */

/*
 * primes - generate a table of primes between two values
 *
 * By: Landon Curt Noll chongo@toad.com, ...!{sun,tolsoft}!hoptoad!chongo
 *
 * chongo <for a good prime call: 391581 * 2^216193 - 1> /\oo/\
 */

/* ubig is the type that holds a large unsigned value */
typedef unsigned long ubig;		/* must be >=32 bit unsigned value */
#define	BIG		ULONG_MAX	/* largest value will sieve */

/* bytes in sieve table (must be > 3*5*7*11) */
#define	TABSIZE		256*1024
