/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)skpc.c	7.4 (Berkeley) %G%
 */

#include <libkern/libkern.h>

int
skpc(mask0, size, cp0)
	int mask0;
	int size;
	char *cp0;
{
	register u_char *cp, *end, mask;

	mask = mask0;
	cp = (u_char *)cp0;
	for (end = &cp[size]; cp < end && *cp == mask; ++cp);
	return (end - cp);
}
