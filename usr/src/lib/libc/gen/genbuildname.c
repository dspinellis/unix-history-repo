/*
 * Copyright (c) 1990 The Regents of the University of California.
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
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)genbuildname.c	5.2 (Berkeley) %G%";
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
