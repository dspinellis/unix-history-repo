/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)printf.c	5.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdio.h>

printf(fmt, args)
	char *fmt;
	int args;
{
	int len;

	len = _doprnt(fmt, &args, stdout);
	return(ferror(stdout) ? EOF : len);
}
