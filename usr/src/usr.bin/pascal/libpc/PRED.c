/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)PRED.c	1.6 (Berkeley) %G%";
#endif /* not lint */

long
PRED(value, lower, upper)

	long	value;
	long	lower;
	long	upper;
{
	if (value == lower) {
		ERROR("Cannot take pred of first element of a range\n");
	}
	value--;
	if (value < lower || value > upper) {
		ERROR("Value of %D is out of range\n", value);
	}
	return	value;
}
