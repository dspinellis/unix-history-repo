/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)CLCK.c	1.3 (Berkeley) %G%";
#endif /* not lint */

long
CLCK()
{
	long	tim[4];

	times(tim);
	return (tim[0] * 50) / 3;
}
