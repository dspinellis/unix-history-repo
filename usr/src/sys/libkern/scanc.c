/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scanc.c	7.2 (Berkeley) %G%
 */

int
scanc(size, cp, table, mask0)
	unsigned int size;
	register unsigned char *cp, table[];
	int mask0;
{
	register unsigned char *end;
	register unsigned char mask;

	mask = mask0;
	for (end = &cp[size]; cp < end && (table[*cp] & mask) == 0; ++cp);
	return (end - cp);
}
