/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Margo Seltzer.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)page.h	8.1 (Berkeley) %G%
 */

/*
 * Definitions for hashing page file format.
 */

/*
 * routines dealing with a data page
 *
 * page format:
 *	+------------------------------+
 * p	| n | keyoff | datoff | keyoff |
 * 	+------------+--------+--------+
 *	| datoff | free  |  ptr  | --> |
 *	+--------+---------------------+
 *	|	 F R E E A R E A       |
 *	+--------------+---------------+
 *	|  <---- - - - | data          |
 *	+--------+-----+----+----------+
 *	|  key   | data     | key      |
 *	+--------+----------+----------+
 *
 * Pointer to the free space is always:  p[p[0] + 2]
 * Amount of free space on the page is:  p[p[0] + 1]
 */

/*
 * How many bytes required for this pair?
 *	2 shorts in the table at the top of the page + room for the
 *	key and room for the data
 *
 * We prohibit entering a pair on a page unless there is also room to append
 * an overflow page. The reason for this it that you can get in a situation
 * where a single key/data pair fits on a page, but you can't append an
 * overflow page and later you'd have to split the key/data and handle like
 * a big pair.
 * You might as well do this up front.
 */

#define	PAIRSIZE(K,D)	(2*sizeof(u_short) + (K)->size + (D)->size)
#define BIGOVERHEAD	(4*sizeof(u_short))
#define KEYSIZE(K)	(4*sizeof(u_short) + (K)->size);
#define OVFLSIZE	(2*sizeof(u_short))
#define FREESPACE(P)	((P)[(P)[0]+1])
#define	OFFSET(P)	((P)[(P)[0]+2])
#define PAIRFITS(P,K,D) \
	(((P)[1] >= REAL_KEY) && \
	    (PAIRSIZE((K),(D)) + OVFLSIZE) <= FREESPACE((P)))
#define PAGE_META(N)	(((N)+3) * sizeof(u_short))

typedef struct {
	BUFHEAD *newp;
	BUFHEAD *oldp;
	BUFHEAD *nextp;
	u_short next_addr;
}       SPLIT_RETURN;
