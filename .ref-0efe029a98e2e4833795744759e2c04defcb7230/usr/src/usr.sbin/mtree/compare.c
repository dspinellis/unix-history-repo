/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)compare.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <fts.h>
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include "mtree.h"

#define	LABEL \
	if (!label++) \
		(void)printf("%s: ", RP(p)); \

compare(name, s, p)
	char *name;
	register NODE *s;
	register FTSENT *p;
{
	extern int exitval, uflag;
	int label;
	char *ftype(), *inotype(), *rlink();

	label = 0;
	switch(s->type) {
	case F_BLOCK:
		if (!S_ISBLK(p->fts_statb.st_mode))
			goto typeerr;
		break;
	case F_CHAR:
		if (!S_ISCHR(p->fts_statb.st_mode))
			goto typeerr;
		break;
	case F_DIR:
		if (!S_ISDIR(p->fts_statb.st_mode))
			goto typeerr;
		break;
	case F_FIFO:
		if (!S_ISFIFO(p->fts_statb.st_mode))
			goto typeerr;
		break;
	case F_FILE:
		if (!S_ISREG(p->fts_statb.st_mode))
			goto typeerr;
		break;
	case F_LINK:
		if (!S_ISLNK(p->fts_statb.st_mode))
			goto typeerr;
		break;
	case F_SOCK:
		if (!S_ISFIFO(p->fts_statb.st_mode)) {
typeerr:		LABEL;
			(void)printf("\n\ttype (%s, %s)",
			    ftype(s->type), inotype(p->fts_statb.st_mode));
		}
		break;
	}
	if (s->flags & F_MODE && s->st_mode != (p->fts_statb.st_mode & MBITS)) {
		LABEL;
		(void)printf("\n\tpermissions (%#o, %#o%s",
		    s->st_mode, p->fts_statb.st_mode & MBITS, uflag ? "" : ")");
		if (uflag)
			if (chmod(p->fts_accpath, s->st_mode))
				(void)printf(", not modified: %s)",
				    strerror(errno));
			else
				(void)printf(", modified)");
	}
	if (s->flags & F_OWNER && s->st_uid != p->fts_statb.st_uid) {
		LABEL;
		(void)printf("\n\towner (%u, %u%s",
		    s->st_uid, p->fts_statb.st_uid, uflag ? "" : ")");
		if (uflag)
			if (chown(p->fts_accpath, s->st_uid, -1))
				(void)printf(", not modified: %s)",
				    strerror(errno));
			else
				(void)printf(", modified)");
	}
	if (s->flags & F_GROUP && s->st_gid != p->fts_statb.st_gid) {
		LABEL;
		(void)printf("\n\tgroup (%u, %u%s",
		    s->st_gid, p->fts_statb.st_gid, uflag ? "" : ")");
		if (uflag)
			if (chown(p->fts_accpath, -1, s->st_gid))
				(void)printf(", not modified: %s)",
				    strerror(errno));
			else
				(void)printf(", modified)");
	}
	if (s->flags & F_NLINK && s->type != F_DIR &&
	    s->st_nlink != p->fts_statb.st_nlink) {
		LABEL;
		(void)printf("\n\tlink count (%u, %u)",
		    s->st_nlink, p->fts_statb.st_nlink);
	}
	if (s->flags & F_SIZE && s->st_size != p->fts_statb.st_size) {
		LABEL;
		(void)printf("\n\tsize (%ld, %ld)",
		    s->st_size, p->fts_statb.st_size);
	}
	if (s->flags & F_SLINK) {
		char *cp;

		if (strcmp(cp = rlink(name), s->slink)) {
			LABEL;
			(void)printf("\n\tlink ref (%s, %s)", cp, s->slink);
		}
	}
	if (s->flags & F_TIME && s->st_mtime != p->fts_statb.st_mtime) {
		LABEL;
		(void)printf("\n\tmodification time (%.24s, ",
		    ctime(&s->st_mtime));
		(void)printf("%.24s)", ctime(&p->fts_statb.st_mtime));
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
	switch(type & S_IFMT) {
	case S_IFBLK:
		return("block");
	case S_IFCHR:
		return("char");
	case S_IFDIR:
		return("dir");
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
	case F_FIFO:
		return("fifo");
	case F_FILE:
		return("file");
	case F_LINK:
		return("link");
	case F_SOCK:
		return("socket");
	default:
		return("unknown");
	}
	/* NOTREACHED */
}

char *
rlink(name)
	char *name;
{
	register int len;
	static char lbuf[MAXPATHLEN];

	len = readlink(name, lbuf, sizeof(lbuf));
	if (len == -1) {
		(void)fprintf(stderr, "mtree: %s: %s.\n",
		    name, strerror(errno));
		exit(1);
	}
	lbuf[len] = '\0';
	return(lbuf);
}
