/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)err.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

extern char *__progname;		/* Program name, from crt0. */

__dead void
#ifdef __STDC__
err(int eval, const char *fmt, ...)
#else
err(eval, fmt, va_alist)
	int eval;
	const char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	verr(eval, fmt, ap);
	va_end(ap);
}

__dead void
verr(eval, fmt, ap)
	int eval;
	const char *fmt;
	va_list ap;
{
	int sverrno;

	sverrno = errno;
	(void)fprintf(stderr, "%s: ", __progname);
	if (fmt != NULL) {
		(void)vfprintf(stderr, fmt, ap);
		(void)fprintf(stderr, ": ");
	}
	(void)fprintf(stderr, "%s\n", strerror(sverrno));
	exit(eval);
}

__dead void
#if __STDC__
errx(int eval, const char *fmt, ...)
#else
errx(eval, fmt, va_alist)
	int eval;
	const char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	verrx(eval, fmt, ap);
	va_end(ap);
}

__dead void
verrx(eval, fmt, ap)
	int eval;
	const char *fmt;
	va_list ap;
{
	(void)fprintf(stderr, "%s: ", __progname);
	if (fmt != NULL)
		(void)vfprintf(stderr, fmt, ap);
	(void)fprintf(stderr, "\n");
	exit(eval);
}

void
#if __STDC__
warn(const char *fmt, ...)
#else
warn(fmt, va_alist)
	const char *fmt;
	va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	vwarn(fmt, ap);
	va_end(ap);
}

void
vwarn(fmt, ap)
	const char *fmt;
	va_list ap;
{
	int sverrno;

	sverrno = errno;
	(void)fprintf(stderr, "%s: ", __progname);
	if (fmt != NULL) {
		(void)vfprintf(stderr, fmt, ap);
		(void)fprintf(stderr, ": ");
	}
	(void)fprintf(stderr, "%s\n", strerror(sverrno));
}

void
#ifdef __STDC__
warnx(const char *fmt, ...)
#else
warnx(fmt, va_alist)
	const char *fmt;
	va_dcl
#endif
{
	va_list ap;
#ifdef __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	vwarnx(fmt, ap);
	va_end(ap);
}

void
vwarnx(fmt, ap)
	const char *fmt;
	va_list ap;
{
	(void)fprintf(stderr, "%s: ", __progname);
	if (fmt != NULL)
		(void)vfprintf(stderr, fmt, ap);
	(void)fprintf(stderr, "\n");
}
