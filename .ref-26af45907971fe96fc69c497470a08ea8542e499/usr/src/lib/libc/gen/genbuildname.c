/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)genbuildname.c	5.3 (Berkeley) %G%";
#endif LIBC_SCCS and not lint

#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

char *objdir = "obj";

#define	UNKNOWN	0
#define	NODIR	1
#define	USEDIR	2

char *
genbuildname(name)
	char *name;
{
	static int dirlen, chkobjdir = UNKNOWN;
	struct stat stbuf;
	char *newname;

	if (chkobjdir == NODIR || index(name, '/') != (char *)0)
		return (name);
	if (chkobjdir == UNKNOWN &&
	    (stat(objdir, &stbuf) < 0 ||
	    (stbuf.st_mode & S_IFMT) != S_IFDIR)) {
		chkobjdir = NODIR;
		return (name);
	} else {
		chkobjdir = USEDIR;
		dirlen = strlen(objdir) + 2;
	}
	newname = (char *)malloc(dirlen + strlen(name));
	if (newname == (char *)0)
		return (name);
	strcpy(newname, objdir);
	strcat(newname, "/");
	strcat(newname, name);
	return (newname);
}
