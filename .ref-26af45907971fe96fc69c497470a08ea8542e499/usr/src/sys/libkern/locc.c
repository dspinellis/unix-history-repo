/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)locc.c	7.4 (Berkeley) %G%
 */

#include <libkern/libkern.h>

int
locc(mask0, cp0, size)
	int mask0;
	char *cp0;
	u_int size;
{
	register u_char *cp, *end, mask;

	mask = mask0;
	cp = (u_char *)cp0;
	for (end = &cp[size]; cp < end && *cp != mask; ++cp);
	return (end - cp);
}
