/*-
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * This module is believed to contain source code proprietary to AT&T.
 * Use and redistribution is subject to the Berkeley Software License
 * Agreement and your Software Agreement with AT&T (Western Electric).
 */

#ifndef lint
static char sccsid[] = "@(#)wait_.c	5.2 (Berkeley) 4/12/91";
#endif /* not lint */

/*
 * wait for a child to die
 *
 * calling sequence:
 *	integer wait, status, chilid
 *	chilid = wait(status)
 * where:
 *	chilid will be	- >0 if child process id
 *			- <0 if (negative of) system error code
 *	status will contain the exit status of the child
 *		(see wait(2))
 */

extern int errno;

long wait_(status)
long *status;
{
	int stat;
	int chid = wait(&stat);
	if (chid < 0)
		return((long)(-errno));
	*status = (long)stat;
	return((long)chid);
}
