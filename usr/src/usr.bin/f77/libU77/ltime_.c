/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)ltime_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * return broken down time
 *
 * calling sequence:
 *	integer time, t[9]
 *	call ltime(time, t)
 * where:
 *	time is a  system time. (see time(3F))
 *	t will receive the broken down time corrected for local timezone.
 *	(see ctime(3))
 */

int *localtime();

ltime_(clock, t)
long *clock; long *t;
{
	int i;
	int *l;

	l = localtime(clock);
	for (i=0; i<9; i++)
		*t++ = *l++;
}
