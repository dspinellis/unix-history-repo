/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)assert.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/types.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

void
__assert(file, line, failedexpr)
	const char *file, *failedexpr;
	int line;
{
	(void)fprintf(stderr,
	    "assertion \"%s\" failed: file \"%s\", line %d\n",
	    failedexpr, file, line);
	abort();
	/* NOTREACHED */
}
