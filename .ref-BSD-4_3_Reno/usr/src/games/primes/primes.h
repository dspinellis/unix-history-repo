/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Landon Curt Noll.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)primes.h	5.2 (Berkeley) 6/1/90
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
