/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Landon Curt Noll.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)primes.h	8.1 (Berkeley) %G%
 */

/*
 * primes - generate a table of primes between two values
 *
 * By: Landon Curt Noll   chongo@toad.com,   ...!{sun,tolsoft}!hoptoad!chongo
 *
 *   chongo <for a good prime call: 391581 * 2^216193 - 1> /\oo/\
 */

/* ubig is the type that holds a large unsigned value */
typedef unsigned long ubig;           /* must be >=32 bit unsigned value */

/*
 * sieve parameters
 */
#define BIG ((ubig)0xffffffff)        /* highest value we will sieve */
#define SEMIBIG ((ubig)0x7fffffff)    /* highest signed value */
#define NEG_SEMIBIG ((ubig)0x80000000) /* lowest signed value */
#define TABSIZE 256*1024 /* bytes in sieve table (must be > 3*5*7*11) */
