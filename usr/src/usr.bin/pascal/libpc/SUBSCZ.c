/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)SUBSCZ.c	8.1 (Berkeley) %G%";
#endif /* not lint */

extern char ESUBSC[];	/* ESUBSC is defined in SUBSCZ.c */

long
SUBSCZ(value, upper)
	long		value;
	unsigned long	upper;
{
	if (value > upper) {
		ERROR(ESUBSC, value);
	}
	return value;
}
