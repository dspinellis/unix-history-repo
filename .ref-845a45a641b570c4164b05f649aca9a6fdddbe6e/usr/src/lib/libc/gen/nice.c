/*
 * Copyright (c) 1983, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)nice.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

/*
 * Backwards compatible nice.
 */
int
nice(incr)
	int incr;
{
	int prio;
	extern int errno;

	errno = 0;
	prio = getpriority(PRIO_PROCESS, 0);
	if (prio == -1 && errno)
		return (-1);
	return (setpriority(PRIO_PROCESS, 0, prio + incr));
}
