/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego and Lance
 * Visser of Convex Computer Corporation.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "dd.h"
#include "extern.h"

/* ARGSUSED */
void
summary(notused)
	int notused;
{
	time_t secs;
	char buf[100];

	(void)time(&secs);
	if ((secs -= st.start) == 0)
		secs = 1;
	/* Use snprintf(3) so that we don't reenter stdio(3). */
	(void)snprintf(buf, sizeof(buf),
	    "%u+%u records in\n%u+%u records out\n%u bytes transferred in %u secs (%u bytes/sec)\n",
	    st.in_full, st.in_part, st.out_full, st.out_part, st.bytes,
	    secs, st.bytes / secs);
	(void)write(STDERR_FILENO, buf, strlen(buf));
	if (st.swab) {
		(void)snprintf(buf, sizeof(buf), "%u odd length swab %s\n",
		     st.swab, (st.swab == 1) ? "block" : "blocks");
		(void)write(STDERR_FILENO, buf, strlen(buf));
	}
	if (st.trunc) {
		(void)snprintf(buf, sizeof(buf), "%u truncated %s\n",
		     st.trunc, (st.trunc == 1) ? "block" : "blocks");
		(void)write(STDERR_FILENO, buf, strlen(buf));
	}
}

/* ARGSUSED */
void
terminate(notused)
	int notused;
{
	summary(0);
	exit(0);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	extern int errstats;
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "dd: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	if (errstats)
		summary(0);
	exit(1);
	/* NOTREACHED */
}

void
#if __STDC__
warn(const char *fmt, ...)
#else
warn(fmt, va_alist)
	char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "dd: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
}
