/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)fprintf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#if __STDC__
fprintf(FILE *fp, const char *fmt, ...)
#else
fprintf(fp, fmt, va_alist)
	FILE *fp;
	char *fmt;
	va_dcl
#endif
{
	int ret;
	va_list ap;

#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = vfprintf(fp, fmt, ap);
	va_end(ap);
	return (ret);
}
