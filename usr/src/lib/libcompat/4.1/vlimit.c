/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)vlimit.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * (Almost) backwards compatible vlimit.
 */
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>

/* LIM_NORAISE is not emulated */
#define	LIM_NORAISE	0	/* if <> 0, can't raise limits */
#define	LIM_CPU		1	/* max secs cpu time */
#define	LIM_FSIZE	2	/* max size of file created */
#define	LIM_DATA	3	/* max growth of data space */
#define	LIM_STACK	4	/* max growth of stack */
#define	LIM_CORE	5	/* max size of ``core'' file */
#define	LIM_MAXRSS	6	/* max desired data+stack core usage */

#define	NLIMITS		6

vlimit(limit, value)
	int limit, value;
{
	struct rlimit rlim;

	if (limit <= 0 || limit > NLIMITS)
		return (EINVAL);
	if (value == -1) {
		if (getrlimit(limit - 1, &rlim) < 0)
			return (-1);
		return (rlim.rlim_cur);
	}
	rlim.rlim_cur = value;
	rlim.rlim_max = RLIM_INFINITY;
	return (setrlimit(limit - 1, &rlim));
}
