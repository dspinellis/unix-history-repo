/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)skpc.c	7.2 (Berkeley) %G%
 */

int
skpc(mask0, size, cp0)
	int mask0;
	int size;
	char *cp0;
{
	register unsigned char *cp, *end, mask;

	mask = mask0;
	cp = (unsigned char *)cp0;
	for (end = &cp[size]; cp < end && *cp == mask; ++cp);
	return (end - cp);
}
