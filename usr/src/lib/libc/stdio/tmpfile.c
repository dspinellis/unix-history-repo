/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Chris Torek.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)tmpfile.c	5.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <stdio.h>
#include <errno.h>

FILE *
tmpfile()
{
	FILE *fp;
	int e;
	char *f, buf[MAXPATHLEN];

	if ((f = tmpnam(buf)) == NULL)
		return (NULL);
	fp = fopen(f, "w+");
	e = errno;
	(void) unlink(f);
	errno = e;
	return (fp);
}
