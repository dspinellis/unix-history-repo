/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Donn Seeley at UUNET Technologies, Inc.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)vsscanf.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <string.h>

/* ARGSUSED */
static int
eofread(cookie, buf, len)
	void *cookie;
	char *buf;
	int len;
{

	return (0);
}

vsscanf(str, fmt, ap)
	const char *str;
	const char *fmt;
	_VA_LIST_ ap;
{
	int ret;
	FILE f;

	f._flags = __SRD;
	f._bf._base = f._p = (unsigned char *)str;
	f._bf._size = f._r = strlen(str);
	f._read = eofread;
	f._ub._base = NULL;
	f._lb._base = NULL;
	return (__svfscanf(&f, fmt, ap));
}
