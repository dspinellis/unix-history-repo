/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)ctrace.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#ifdef DEBUG
#include <stdio.h>

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#ifndef TFILE
#define	TFILE	"__curses.out"
#endif

static FILE *tracefp;			/* Curses debugging file descriptor. */

void
#if __STDC__
__TRACE(const char *fmt, ...)
#else
__TRACE(fmt, va_alist)
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
	if (tracefp == NULL)
		tracefp = fopen(TFILE, "w");
	if (tracefp == NULL)
		return;
	(void)vfprintf(tracefp, fmt, ap);
	va_end(ap);
	(void)fflush(tracefp);
}
#endif
