/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern char *special;

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const int fatal, const char *fmt, ...)
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
	(void)fprintf(stderr, "%s: ", special);
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	if (errno)
		(void)fprintf(stderr, " %s", strerror(errno));
	(void)fprintf(stderr, "\n");
	if (fatal)
		exit(1);
}

void
get(fd, off, p, len)
	int fd;
	off_t off;
	void *p;
	size_t len;
{
	int rbytes;

	if (lseek(fd, off, SEEK_SET) < 0)
		err(1, "%s: %s", special, strerror(errno));
	if ((rbytes = read(fd, p, len)) < 0)
		err(1, "%s: %s", special, strerror(errno));
	if (rbytes != len)
		err(1, "%s: short read (%d, not %d)", special, rbytes, len);
}
