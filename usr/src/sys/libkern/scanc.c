/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)scanc.c	7.1 (Berkeley) %G%
 */

int
scanc(size, cp, table, mask)
	unsigned int size;
	register unsigned char *cp, table[];
	register unsigned char mask;
{
	register unsigned char *end;

	for (end = &cp[size]; cp < end && (table[*cp] & mask) == 0; ++cp);
	return (end - cp);
}
