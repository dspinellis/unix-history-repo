/*-
 * Copyright (c) 1979 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)PCEXIT.c	1.3 (Berkeley) 6/29/90";
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
