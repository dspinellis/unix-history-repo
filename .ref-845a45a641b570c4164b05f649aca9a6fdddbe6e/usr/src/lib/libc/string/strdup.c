/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)strdup.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

char *
strdup(str)
	const char *str;
{
	size_t len;
	char *copy;

	len = strlen(str) + 1;
	if (!(copy = malloc((u_int)len)))
		return (NULL);
	bcopy(str, copy, len);
	return (copy);
}
