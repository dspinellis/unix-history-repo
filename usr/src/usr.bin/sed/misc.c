/*-
 * Copyright (c) 1992 Diomidis Spinellis.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Diomidis Spinellis of Imperial College, University of London.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <errno.h>
#include <regex.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "defs.h"
#include "extern.h"

/*
 * malloc with result test
 */
void *
xmalloc(size)
	u_int size;
{
	void *p;

	if ((p = malloc(size)) == NULL)
		err(FATAL, "%s", strerror(errno));
	return (p);
}

/*
 * realloc with result test
 */
void *
xrealloc(p, size)
	void *p;
	u_int size;
{
	if (p == NULL)			/* Compatibility hack. */
		return (xmalloc(size));

	if ((p = realloc(p, size)) == NULL)
		err(FATAL, "%s", strerror(errno));
	return (p);
}

/*
 * Return a string for a regular expression error passed.  This is a overkill,
 * because of the silly semantics of regerror (we can never know the size of
 * the buffer).
 */
char *
strregerror(errcode, preg)
	int errcode;
	regex_t *preg;
{
	static char *oe;
	size_t s;

	if (oe != NULL)
		free(oe);
	s = regerror(errcode, preg, "", 0);
	oe = xmalloc(s);
	(void)regerror(errcode, preg, oe, s);
	return (oe);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
/*
 * Error reporting function
 */
void
#if __STDC__
err(int severity, const char *fmt, ...)
#else
err(severity, fmt, va_alist)
	int severity;
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
	(void)fprintf(stderr, "sed: ");
	switch (severity) {
	case WARNING:
	case COMPILE:
		(void)fprintf(stderr, "%s(%lu): ", fname, linenum);
	}
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	switch (severity) {
	case COMPILE:
		compile_errors++;
#define	MAX_COMPILE_ERRS	20
		if (compile_errors > MAX_COMPILE_ERRS)
			err(FATAL, "too many compilation errors; exiting");
	case FATAL:
		exit(1);
	}
	/* NOTREACHED */
}
