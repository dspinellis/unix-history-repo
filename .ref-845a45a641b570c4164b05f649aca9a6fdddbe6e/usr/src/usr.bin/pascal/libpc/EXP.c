/*-
 * Copyright (c) 1982 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)EXP.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include <math.h>
extern int errno;

double
EXP(value)
	double	value;
{
	double result;

	errno = 0;
	result = exp(value);
	if (errno != 0) {
		ERROR("exp(%e) yields a result that is out of the range of reals\n", value);
	}
	return result;
}
