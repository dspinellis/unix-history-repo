/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)RSNG4.c	1.6 (Berkeley) %G%";
#endif /* not lint */

extern char ERANG[];	/* ERANG is defined in RANG4.c */

long
RSNG4(value, upper)
	long		value;
	unsigned long	upper;
{
	if (value > upper) {
		ERROR(ERANG, value);
	}
	return	value;
}
