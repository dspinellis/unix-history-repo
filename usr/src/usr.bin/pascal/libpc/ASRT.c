/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ASRT.c	1.4 (Berkeley) %G%";
#endif /* not lint */

char EASRT[] = "Assertion failed\n";

ASRT(cond)
	short	cond;
{
	if (cond)
		return;
	ERROR(EASRT, 0);
	return;
}
