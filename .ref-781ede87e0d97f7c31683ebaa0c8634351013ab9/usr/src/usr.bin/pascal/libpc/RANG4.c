/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)RANG4.c	1.6 (Berkeley) %G%";
#endif /* not lint */

char ERANG[] = "Value of %D is out of range\n";

long
RANG4(value, lower, upper)
	long	value;
	long	lower;
	long	upper;
{
	if (value < lower || value > upper) {
		ERROR(ERANG, value);
	}
	return	value;
}
