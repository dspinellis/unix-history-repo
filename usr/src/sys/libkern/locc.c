/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)locc.c	7.2 (Berkeley) %G%
 */

int
locc(mask0, cp0, size)
	int mask0;
	char *cp0;
	unsigned int size;
{
	register unsigned char *cp, *end, mask;

	mask = mask0;
	cp = (unsigned char *)cp0;
	for (end = &cp[size]; cp < end && *cp != mask; ++cp);
	return (end - cp);
}
