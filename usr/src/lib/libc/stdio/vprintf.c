/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)vprintf.c	5.3 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>
#include <varargs.h>

int
vprintf(fmt, ap)
	char *fmt;
	va_list ap;
{
	int len;

	len = _doprnt(fmt, ap, stdout);
	return (ferror(stdout) ? EOF : len);
}
