/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)SEED.c	1.6 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"

long
SEED(value)
	long value;
{
	long tmp;

	tmp = _seed;
	_seed = value;
	return tmp;
}
