/*-
 * Copyright (c) 1982 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ATAN.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>
extern int errno;

double
ATAN(value)
	double	value;
{
	double result;

	errno = 0;
	result = atan(value);
	if (errno != 0) {
		ERROR("Argument %e is out of the domain of atan\n", value);
	}
	return result;
}
