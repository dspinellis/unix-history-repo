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
static char sccsid[] = "@(#)tempnam.c	4.5 (Berkeley) %G%";
#endif /* LIBC_SCCS and not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <sys/file.h>
#include <stdio.h>

#define	P_tmpdir	"/usr/tmp"

FILE *
tmpfile()
{
	FILE *fp;
	char *f, name[MAXPATHLEN], *tmpnam();

	if (!(fp = fopen(f = tmpnam(name), "w+"))) {
		fprintf(stderr, "tmpfile: cannot open %s.\n", name);
		return(NULL);
	}
	(void)unlink(f);
	return(fp);
}

char *
tmpnam(s)
	char *s;
{
	static char name[MAXPATHLEN];
	char *mktemp();

	if (!s)
		s = name;
	(void)sprintf(s, "%s/XXXXXX", P_tmpdir);
	return(mktemp(s));
}

char *
tempnam(dir, pfx)
	char *dir, *pfx;
{
	struct stat buf;
	char *f, *name, *getenv(), *malloc(), *mktemp(), *strcat(), *strcpy();

	if (!(name = malloc((u_int)MAXPATHLEN)))
		return(NULL);
	if ((f = getenv("TMPDIR")) && !stat(f, &buf) &&
	    (buf.st_mode&S_IFMT) == S_IFDIR && !access(f, W_OK|X_OK)) {
		(void)strcpy(name, f);
		goto done;
	}
	if (dir && !stat(dir, &buf) &&
	    (buf.st_mode&S_IFMT) == S_IFDIR && !access(dir, W_OK|X_OK)) {
		(void)strcpy(name, dir);
		goto done;
	}
	if (!stat(P_tmpdir, &buf) &&
	    (buf.st_mode&S_IFMT) == S_IFDIR && !access(P_tmpdir, W_OK|X_OK)) {
		(void)strcpy(name, P_tmpdir);
		goto done;
	}
	if (!stat("/tmp", &buf) &&
	    (buf.st_mode&S_IFMT) == S_IFDIR && !access("/tmp", W_OK|X_OK)) {
		(void)strcpy(name, "/tmp");
		goto done;
	}
	return(NULL);
done:	(void)strcat(name, "/");
	if (pfx)
		(void)strcat(name, pfx);
	(void)strcat(name, "XXXXXX");
	return(mktemp(name));
}
