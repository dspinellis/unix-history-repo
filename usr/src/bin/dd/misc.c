/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Keith Muller of the University of California, San Diego.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.1 (Berkeley) %G%";
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
	int len;
	char buf[100];

	/* Use snprintf(3) so that we don't reenter stdio(3). */
	len = snprintf(buf, sizeof(buf),
	    "%u+%u records in\n%u+%u records out\n",
	    in.f_stats, in.p_stats, out.f_stats, out.p_stats);
	(void)write(STDERR_FILENO, buf, len);
	if (in.t_stats) {
		len = snprintf(buf, sizeof(buf), "%u truncated %s\n",
		     in.t_stats, (in.t_stats == 1) ? "block" : "blocks");
		(void)write(STDERR_FILENO, buf, len);
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
