/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)tmpnam.c	4.8 (Berkeley) 6/22/90";
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
