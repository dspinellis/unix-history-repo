/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)tempnam.c	4.6 (Berkeley) %G%";
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
	return(mktemp(name));
}
