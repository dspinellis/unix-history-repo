/*-
 * Copyright (c) 1979, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)SQRT.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>

double
SQRT(value)

	double	value;
{
	if (value < 0) {
		ERROR("Negative argument of %e to sqrt\n", value);
	}
	return sqrt(value);
}
