/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)CLCK.c	1.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/time.h>
#include <sys/resource.h>

long
CLCK()
{
	struct rusage ru;

	if (getrusage(RUSAGE_SELF, &ru) < 0)
		return (-1);
	return (ru.ru_utime.tv_sec * 1000 + ru.ru_utime.tv_usec / 1000);
}
