/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include "extern.h"

void
eofmsg(file)
	char *file;
{
	if (!sflag)
		(void)fprintf(stderr, "cmp: EOF on %s\n", file);
	exit(1);
}

void
diffmsg(file1, file2, byte, line)
	char *file1, *file2;
	off_t byte, line;
{
	if (!sflag)
		(void)printf("%s %s differ: char %qd, line %qd\n",
		    file1, file2, byte, line);
	exit(1);
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
	(void)fprintf(stderr, "cmp: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(2);
	/* NOTREACHED */
}
