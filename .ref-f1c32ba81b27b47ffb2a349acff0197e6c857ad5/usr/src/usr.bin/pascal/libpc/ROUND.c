/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ROUND.c	1.4 (Berkeley) %G%";
#endif /* not lint */

long
ROUND(value)

	double	value;
{
	if (value >= 0.0)
		return (long)(value + 0.5);
	return (long)(value - 0.5);
}
