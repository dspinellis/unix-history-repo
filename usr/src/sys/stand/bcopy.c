/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bcopy.c	7.1 (Berkeley) %G%
 */

/*
 * This is designed to be small, not fast.
 */
void
bcopy(s1, s2, n)
	const void *s1;
	void *s2;
	unsigned n;
{
	register const char *f = s1;
	register char *t = s2;

	while (n != 0) {
		*t++ = *f++;
		n--;
	}
}
