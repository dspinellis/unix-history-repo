/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bzero.c	8.1 (Berkeley) %G%
 */

/*
 * This is designed to be small, not fast.
 */
void
bzero(s1, n)
	void *s1;
	unsigned n;
{
	register char *t = s1;

	while (n != 0) {
		*t++ = 0;
		n--;
	}
}
