/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)util.c	5.9 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include "ls.h"
#include "extern.h"

void
prcopy(src, dest, len)
	register char *src, *dest;
	register int len;
{
	register int ch;

	while(len--) {
		ch = *src++;
		*dest++ = isprint(ch) ? ch : '?';
	}
}

void *
emalloc(size)
	u_int size;
{
	void *retval;

	if ((retval = malloc(size)) == NULL)
		err("%s", strerror(errno));
	return (retval);
}

void
usage()
{
	(void)fprintf(stderr, "usage: ls [-1ACFLRTacdfgiklqrstu] [file ...]\n");
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
	(void)fprintf(stderr, "ls: ");
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
	(void)fprintf(stderr, "ls: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
}
