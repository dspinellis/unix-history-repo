/*-
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getwd.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

char *
getwd(buf)
	char *buf;
{
	char *p;

	if (p = getcwd(buf, MAXPATHLEN))
		return(p);
	(void)strcpy(buf, strerror(errno));
	return((char *)NULL);
}
