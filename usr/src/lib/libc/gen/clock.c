/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)clock.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <machine/machlimits.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

clock_t
clock()
{
	struct rusage rusage;
	clock_t val;

	if (getrusage(RUSAGE_SELF, &rusage))
		return ((clock_t) -1);
	val = (rusage.ru_utime.tv_sec + rusage.ru_stime.tv_sec) * CLK_TCK;
	/*
	 * Convert usec to clock ticks; could do (usec * CLK_TCK) / 1000000,
	 * but this would overflow if we switch to nanosec.
	 */
	val += (rusage.ru_utime.tv_usec + rusage.ru_stime.tv_usec) /
		(1000000 / CLK_TCK);
	return (val);
}
