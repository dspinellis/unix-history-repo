/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)atoi.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>
#include <stddef.h>

atoi(str)
	const char *str;
{
	return((int)strtol(str, (char **)NULL, 10));
}
