/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)times.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/time.h>
#include <sys/times.h>
#include <sys/resource.h>

clock_t
times(tmsp)
	register struct tms *tmsp;
{
	struct rusage ru;
	clock_t scale60();

	if (getrusage(RUSAGE_SELF, &ru) < 0)
		return ((clock_t)-1);
	tmsp->tms_utime = scale60(&ru.ru_utime);
	tmsp->tms_stime = scale60(&ru.ru_stime);
	if (getrusage(RUSAGE_CHILDREN, &ru) < 0)
		return ((clock_t)-1);
	tmsp->tms_cutime = scale60(&ru.ru_utime);
	tmsp->tms_cstime = scale60(&ru.ru_stime);
	return ((clock_t)0);
}

static clock_t
scale60(tvp)
	register struct timeval *tvp;
{
	return (tvp->tv_sec * 60 + tvp->tv_usec / 16667);
}
