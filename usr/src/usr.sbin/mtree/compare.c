/*
 * Copyright (c) 1989 The Regents of the University of California.
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

#ifndef lint
static char sccsid[] = "@(#)compare.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include "mtree.h"

#define	LABEL { \
	if (!label++) \
		(void)printf("%s: ", path + 2); \
}

compare(name, s1, s2)
	char *name;
	register INFO *s1;
	register struct stat *s2;
{
	extern int exitval, uflag;
	extern char path[];
	int label;
	char *p, *ftype(), *inotype(), *rlink();

	label = 0;
	switch(s1->type) {
	case F_BLOCK:
		if (!S_ISBLK(s2->st_mode))
			goto typeerr;
		break;
	case F_CHAR:
		if (!S_ISCHR(s2->st_mode))
			goto typeerr;
		break;
	case F_DIR:
		if (!S_ISDIR(s2->st_mode))
			goto typeerr;
		break;
	case F_FILE:
		if (!S_ISREG(s2->st_mode))
			goto typeerr;
		break;
	case F_LINK:
		if (!S_ISLNK(s2->st_mode))
			goto typeerr;
		break;
	case F_SOCK:
		if (!S_ISFIFO(s2->st_mode)) {
typeerr:		LABEL;
			(void)printf("\n\ttype (%s, %s)",
			    ftype(s1->type), inotype(s2->st_mode));
		}
		break;
	}
	if (s1->flags&F_MODE && s1->st_mode != (s2->st_mode&07777)) {
		LABEL;
		(void)printf("\n\tpermissions (%#o, %#o%s",
		    s1->st_mode, s2->st_mode&07777, uflag ? "" : ")");
		if (uflag)
			if (chmod(path, s1->st_mode))
				(void)printf(", not modified: %s)",
				    strerror(errno));
			else
				(void)printf(", modified)");
	}
	if (s1->flags&F_OWNER && s1->st_uid != s2->st_uid) {
		LABEL;
		(void)printf("\n\towner (%u, %u%s",
		    s1->st_uid, s2->st_uid, uflag ? "" : ")");
		if (uflag)
			if (chown(path, s1->st_uid, -1))
				(void)printf(", not modified: %s)",
				    strerror(errno));
			else
				(void)printf(", modified)");
	}
	if (s1->flags&F_GROUP && s1->st_gid != s2->st_gid) {
		LABEL;
		(void)printf("\n\tgroup (%u, %u%s",
		    s1->st_gid, s2->st_gid, uflag ? "" : ")");
		if (uflag)
			if (chown(path, -1, s1->st_gid))
				(void)printf(", not modified: %s)",
				    strerror(errno));
			else
				(void)printf(", modified)");
	}
	if (s1->flags&F_NLINK && s1->type != F_DIR &&
	    s1->st_nlink != s2->st_nlink) {
		LABEL;
		(void)printf("\n\tlink count (%u, %u)",
		    s1->st_nlink, s2->st_nlink);
	}
	if (s1->flags&F_SIZE && s1->st_size != s2->st_size) {
		LABEL;
		(void)printf("\n\tsize (%ld, %ld)", s1->st_size, s2->st_size);
	}
	if (s1->flags&F_SLINK) {
		p = rlink(name);
		if (strcmp(p, s1->slink)) {
			LABEL;
			(void)printf("\n\tlink ref (%s, %s)", p, s1->slink);
		}
	}
	if (label) {
		exitval = 2;
		putchar('\n');
	}
}

char *
inotype(type)
	mode_t type;
{
	switch(type&S_IFMT) {
	case S_IFCHR:
		return("char");
	case S_IFBLK:
		return("block");
	case S_IFDIR:
		return("directory");
	case S_IFREG:
		return("file");
	case S_IFLNK:
		return("link");
	case S_IFSOCK:
		return("socket");
	default:
		return("unknown");
	}
	/* NOTREACHED */
}

char *
ftype(type)
	u_int type;
{
	switch(type) {
	case F_BLOCK:
		return("block");
	case F_CHAR:
		return("char");
	case F_DIR:
		return("dir");
	case F_FILE:
		return("file");
	case F_LINK:
		return("link");
	case F_SOCK:
		return("socket");
	}
	/* NOTREACHED */
}
