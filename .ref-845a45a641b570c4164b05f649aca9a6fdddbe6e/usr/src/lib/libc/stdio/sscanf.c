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
static char sccsid[] = "@(#)sscanf.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <string.h>
#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif
#include "local.h"

/* ARGSUSED */
static int
eofread(cookie, buf, len)
	void *cookie;
	char *buf;
	int len;
{

	return (0);
}

#if __STDC__
sscanf(const char *str, char const *fmt, ...)
#else
sscanf(str, fmt, va_alist)
	char *str;
	char *fmt;
	va_dcl
#endif
{
	int ret;
	va_list ap;
	FILE f;

	f._flags = __SRD;
	f._bf._base = f._p = (unsigned char *)str;
	f._bf._size = f._r = strlen(str);
	f._read = eofread;
	f._ub._base = NULL;
	f._lb._base = NULL;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	ret = __svfscanf(&f, fmt, ap);
	va_end(ap);
	return (ret);
}
