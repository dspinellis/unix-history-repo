/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)SUBSC.c	1.6 (Berkeley) %G%";
#endif /* not lint */

char ESUBSC[] = "Subscript value of %D is out of range\n";

long
SUBSC(i, lower, upper)

	long	i, lower, upper;
{
	if (i < lower || i > upper) {
		ERROR(ESUBSC, i);
	}
	return i;
}
