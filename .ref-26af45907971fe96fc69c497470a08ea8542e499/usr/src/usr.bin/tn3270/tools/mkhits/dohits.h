/*-
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dohits.h	4.2 (Berkeley) %G%
 */

#define	numberof(x)	(sizeof x/sizeof x[0])
#define	highestof(x)	(numberof(x)-1)

#define	firstentry(x)	(table[dohash(0, (x))%highestof(table)])

struct Hits {
    struct hits hits;
    char *name[4];
};

struct thing {
    struct thing *next;
    struct Hits *hits;
    unsigned char value;
    char name[100];
};

extern struct Hits Hits[256];		/* one for each of 0x00-0xff */
extern struct thing *table[100];

extern unsigned int dohash();
