/*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)valloc.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <stdlib.h>
#include <unistd.h>

void *
valloc(i)
	size_t i;
{
	int valsiz = getpagesize(), j;
	void *cp = malloc(i + (valsiz-1));

	j = ((int)cp + (valsiz-1)) &~ (valsiz-1);
	return ((void *)j);
}
