/*-
 * Copyright (c) 1982, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ASRTS.c	8.1 (Berkeley) %G%";
#endif /* not lint */

char EASRTS[] = "Assertion failed: %s\n";

ASRTS(cond, stmt)
	short	cond;
	char	*stmt;
{
	if (cond)
		return;
	ERROR(EASRTS, stmt);
	return;
}
