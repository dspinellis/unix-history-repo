/*-
 * Copyright (c) 1982 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)COS.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>
extern int errno;

double
COS(value)
	double	value;
{
	double result;

	errno = 0;
	result = cos(value);
	if (errno != 0) {
		ERROR("Cannot compute cos(%e)\n", value);
	}
	return result;
}
