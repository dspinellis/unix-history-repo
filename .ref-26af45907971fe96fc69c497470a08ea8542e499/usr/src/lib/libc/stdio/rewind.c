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
static char sccsid[] = "@(#)rewind.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <errno.h>
#include <stdio.h>

void
rewind(fp)
	register FILE *fp;
{
	(void) fseek(fp, 0L, SEEK_SET);
	clearerr(fp);
	errno = 0;      /* not required, but seems reasonable */
}
