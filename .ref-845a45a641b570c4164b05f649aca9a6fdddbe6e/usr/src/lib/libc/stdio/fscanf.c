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
static char sccsid[] = "@(#)fscanf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#if __STDC__
fscanf(FILE *fp, char const *fmt, ...) {
	int ret;
	va_list ap;

	va_start(ap, fmt);
#else
fscanf(fp, fmt, va_alist)
	FILE *fp;
	char *fmt;
	va_dcl
{
	int ret;
	va_list ap;

	va_start(ap);
#endif
	ret = __svfscanf(fp, fmt, ap);
	va_end(ap);
	return (ret);
}
