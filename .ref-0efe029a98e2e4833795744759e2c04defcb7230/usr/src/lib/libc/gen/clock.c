/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)clock.c	5.4 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/time.h>
#include <sys/resource.h>

/*
 * Convert usec to clock ticks; could do (usec * CLK_TCK) / 1000000,
 * but this would overflow if we switch to nanosec.
 */
#define	CONVTCK(r)	(r.tv_sec * CLK_TCK + r.tv_usec / (1000000 / CLK_TCK))

clock_t
clock()
{
	struct rusage ru;

	if (getrusage(RUSAGE_SELF, &ru))
		return ((clock_t) -1);
	return((clock_t)((CONVTCK(ru.ru_utime) + CONVTCK(ru.ru_stime))));
}
