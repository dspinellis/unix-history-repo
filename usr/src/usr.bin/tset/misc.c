/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extern.h"

void
cat(file)
	char *file;
{
	register int fd, nr, nw;
	char buf[1024];

	if ((fd = open(file, O_RDONLY, 0)) < 0)
		err("%s: %s", file, strerror(errno));

	while ((nr = read(fd, buf, sizeof(buf))) > 0)
		if ((nw = write(STDERR_FILENO, buf, nr)) == -1)
			err("write to stderr: %s", strerror(errno));
	if (nr != 0)
		err("%s: %s", file, strerror(errno));
	(void)close(fd);
}

void
outc(c)
	int c;
{
	(void)putc(c, stderr);
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
	(void)fprintf(stderr, "tset: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
