/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)tempnam.c	8.1 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <paths.h>

char *
tempnam(dir, pfx)
	const char *dir, *pfx;
{
	int sverrno;
	char *f, *name;

	if (!(name = malloc(MAXPATHLEN)))
		return(NULL);

	if (!pfx)
		pfx = "tmp.";

	if (f = getenv("TMPDIR")) {
		(void)snprintf(name, MAXPATHLEN, "%s%s%sXXXXXX", f,
		    *(f + strlen(f) - 1) == '/'? "": "/", pfx);
		if (f = mktemp(name))
			return(f);
	}

	if (f = (char *)dir) {
		(void)snprintf(name, MAXPATHLEN, "%s%s%sXXXXXX", f,
		    *(f + strlen(f) - 1) == '/'? "": "/", pfx);
		if (f = mktemp(name))
			return(f);
	}

	f = P_tmpdir;
	(void)snprintf(name, MAXPATHLEN, "%s%sXXXXXX", f, pfx);
	if (f = mktemp(name))
		return(f);

	f = _PATH_TMP;
	(void)snprintf(name, MAXPATHLEN, "%s%sXXXXXX", f, pfx);
	if (f = mktemp(name))
		return(f);

	sverrno = errno;
	free(name);
	errno = sverrno;
	return(NULL);
}
