/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)SCLCK.c	1.3 (Berkeley) %G%";
#endif /* not lint */

long
SCLCK()
{
	long	tim[4];

	times(tim);
	return (tim[1] * 50) / 3;
}
