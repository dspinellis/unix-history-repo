/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)PCEXIT.c	1.3 (Berkeley) %G%";
#endif /* not lint */

#include "h00vars.h"
#include <sys/time.h>
#include <sys/resource.h>

PCEXIT(code)

	int	code;
{
	double l;
	struct rusage ru;

	PCLOSE(GLVL);
	PFLUSH();
	if (_stcnt > 0) {
		if (getrusage(RUSAGE_SELF, &ru) < 0)
			exit(code);
		l = ru.ru_utime.tv_usec;
		l /= 1000000;
		l += ru.ru_utime.tv_sec;
		fprintf(stderr, "\n%1ld %s %04.2f seconds cpu time.\n",
				_stcnt, "statements executed in", l);
	}
	exit(code);
}
