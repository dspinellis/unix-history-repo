/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)TRUNC.c	1.3 (Berkeley) %G%";
#endif /* not lint */

long
TRUNC(value)

	double	value;
{
	return (long)(value);
}
