/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)gmtime_.c	5.1	%G%
 */

/*
 * return broken down time
 *
 * calling sequence:
 *	integer time, t[9]
 *	call gmtime(time, t)
 * where:
 *	time is a system time. (see time(3F))
 *	t will receive the broken down time assuming GMT.
 *	(see ctime(3))
 */

int *gmtime();

gmtime_(clock, t)
long *clock; long *t;
{
	int i;
	int *g;

	g = gmtime(clock);
	for (i=0; i<9; i++)
		*t++ = *g++;
}
