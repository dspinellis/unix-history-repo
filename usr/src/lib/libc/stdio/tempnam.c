/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)tempnam.c	4.8 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <stdio.h>

#define	P_tmpdir	"/usr/tmp"

FILE *
tmpfile()
{
	FILE *fp;
	char *f, *tmpnam();

	if (!(f = tmpnam((char *)NULL)) || !(fp = fopen(f, "w+"))) {
		fprintf(stderr, "tmpfile: cannot open %s.\n", f);
		return(NULL);
	}
	(void)unlink(f);
	return(fp);
}

char *
tmpnam(s)
	char *s;
{
	char *malloc(), *mktemp();

	if (!s && !(s = malloc((u_int)MAXPATHLEN)))
		return(NULL);
	(void)sprintf(s, "%s/XXXXXX", P_tmpdir);
	return(mktemp(s));
}

char *
tempnam(dir, pfx)
	char *dir, *pfx;
{
	char *f, *name, *getenv(), *malloc(), *mktemp();

	if (!(name = malloc((u_int)MAXPATHLEN)))
		return(NULL);

	if (f = getenv("TMPDIR")) {
		(void)sprintf(name, "%s/%sXXXXXX", f, pfx ? "" : pfx);
		if (f = mktemp(name))
			return(f);
	}
	if (dir) {
		(void)sprintf(name, "%s/%sXXXXXX", dir, pfx ? "" : pfx);
		if (f = mktemp(name))
			return(f);
	}
	(void)sprintf(name, "%s/%sXXXXXX", P_tmpdir, pfx ? "" : pfx);
	if (f = mktemp(name))
		return(f);
	(void)sprintf(name, "/tmp/%sXXXXXX", pfx ? "" : pfx);
	if (!(f = mktemp(name)))
		(void)free(name);
	return(f);
}
