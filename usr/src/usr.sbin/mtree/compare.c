/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)compare.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <fts.h>
#include <errno.h>
#include <stdio.h>
#include <time.h>
#include <unistd.h>
#include "mtree.h"
#include "extern.h"

extern int uflag;

static char *ftype __P((u_int));

#define	INDENTNAMELEN	8
#define	LABEL \
	if (!label++) { \
		len = printf("%s: ", RP(p)); \
		if (len > INDENTNAMELEN) { \
			tab = "\t"; \
			(void)printf("\n"); \
		} else { \
			tab = ""; \
			(void)printf("%*s", INDENTNAMELEN - len, ""); \
		} \
	}

int
compare(name, s, p)
	char *name;
	register NODE *s;
	register FTSENT *p;
{
	extern int uflag;
	u_long len, val;
	int fd, label;
	char *cp, *tab;

	label = 0;
	switch(s->type) {
	case F_BLOCK:
		if (!S_ISBLK(p->fts_statp->st_mode))
			goto typeerr;
		break;
	case F_CHAR:
		if (!S_ISCHR(p->fts_statp->st_mode))
			goto typeerr;
		break;
	case F_DIR:
		if (!S_ISDIR(p->fts_statp->st_mode))
			goto typeerr;
		break;
	case F_FIFO:
		if (!S_ISFIFO(p->fts_statp->st_mode))
			goto typeerr;
		break;
	case F_FILE:
		if (!S_ISREG(p->fts_statp->st_mode))
			goto typeerr;
		break;
	case F_LINK:
		if (!S_ISLNK(p->fts_statp->st_mode))
			goto typeerr;
		break;
	case F_SOCK:
		if (!S_ISSOCK(p->fts_statp->st_mode)) {
typeerr:		LABEL;
			(void)printf("\ttype (%s, %s)\n",
			    ftype(s->type), inotype(p->fts_statp->st_mode));
		}
		break;
	}
	/* Set the uid/gid first, then set the mode. */
	if (s->flags & (F_UID | F_UNAME) && s->st_uid != p->fts_statp->st_uid) {
		LABEL;
		(void)printf("%suser (%u, %u",
		    tab, s->st_uid, p->fts_statp->st_uid);
		if (uflag)
			if (chown(p->fts_accpath, s->st_uid, -1))
				(void)printf(", not modified: %s)\n",
				    strerror(errno));
			else
				(void)printf(", modified)\n");
		else
			(void)printf(")\n");
		tab = "\t";
	}
	if (s->flags & (F_GID | F_GNAME) && s->st_gid != p->fts_statp->st_gid) {
		LABEL;
		(void)printf("%sgid (%u, %u",
		    tab, s->st_gid, p->fts_statp->st_gid);
		if (uflag)
			if (chown(p->fts_accpath, -1, s->st_gid))
				(void)printf(", not modified: %s)\n",
				    strerror(errno));
			else
				(void)printf(", modified)\n");
		else
			(void)printf(")\n");
		tab = "\t";
	}
	if (s->flags & F_MODE &&
	    s->st_mode != (p->fts_statp->st_mode & MBITS)) {
		LABEL;
		(void)printf("%spermissions (%#o, %#o",
		    tab, s->st_mode, p->fts_statp->st_mode & MBITS);
		if (uflag)
			if (chmod(p->fts_accpath, s->st_mode))
				(void)printf(", not modified: %s)\n",
				    strerror(errno));
			else
				(void)printf(", modified)\n");
		else
			(void)printf(")\n");
		tab = "\t";
	}
	if (s->flags & F_NLINK && s->type != F_DIR &&
	    s->st_nlink != p->fts_statp->st_nlink) {
		LABEL;
		(void)printf("%slink count (%u, %u)\n",
		    tab, s->st_nlink, p->fts_statp->st_nlink);
		tab = "\t";
	}
	if (s->flags & F_SIZE && s->st_size != p->fts_statp->st_size) {
		LABEL;
		(void)printf("%ssize (%qd, %qd)\n",
		    tab, s->st_size, p->fts_statp->st_size);
		tab = "\t";
	}
	/*
	 * XXX
	 * Catches nano-second differences, but doesn't display them.
	 */
	if (s->flags & F_TIME &&
	    s->st_mtimespec.ts_sec != p->fts_statp->st_mtimespec.ts_sec ||
	    s->st_mtimespec.ts_nsec != p->fts_statp->st_mtimespec.ts_nsec) {
		LABEL;
		(void)printf("%smodification time (%.24s, ",
		    tab, ctime(&s->st_mtimespec.ts_sec));
		(void)printf("%.24s)\n",
		    ctime(&p->fts_statp->st_mtimespec.ts_sec));
		tab = "\t";
	}
	if (s->flags & F_CKSUM)
		if ((fd = open(p->fts_accpath, O_RDONLY, 0)) < 0) {
			LABEL;
			(void)printf("%scksum: %s: %s\n",
			    tab, p->fts_accpath, strerror(errno));
			tab = "\t";
		} else if (crc(fd, &val, &len)) {
			(void)close(fd);
			LABEL;
			(void)printf("%scksum: %s: %s\n",
			    tab, p->fts_accpath, strerror(errno));
			tab = "\t";
		} else {
			(void)close(fd);
			if (s->cksum != val) {
				LABEL;
				(void)printf("%scksum (%lu, %lu)\n", 
				    tab, s->cksum, val);
			}
			tab = "\t";
		}
	if (s->flags & F_SLINK && strcmp(cp = rlink(name), s->slink)) {
		LABEL;
		(void)printf("%slink ref (%s, %s)\n", tab, cp, s->slink);
	}
	return (label);
}

char *
inotype(type)
	u_int type;
{
	switch(type & S_IFMT) {
	case S_IFBLK:
		return ("block");
	case S_IFCHR:
		return ("char");
	case S_IFDIR:
		return ("dir");
	case S_IFIFO:
		return ("fifo");
	case S_IFREG:
		return ("file");
	case S_IFLNK:
		return ("link");
	case S_IFSOCK:
		return ("socket");
	default:
		return ("unknown");
	}
	/* NOTREACHED */
}

static char *
ftype(type)
	u_int type;
{
	switch(type) {
	case F_BLOCK:
		return ("block");
	case F_CHAR:
		return ("char");
	case F_DIR:
		return ("dir");
	case F_FIFO:
		return ("fifo");
	case F_FILE:
		return ("file");
	case F_LINK:
		return ("link");
	case F_SOCK:
		return ("socket");
	default:
		return ("unknown");
	}
	/* NOTREACHED */
}

char *
rlink(name)
	char *name;
{
	static char lbuf[MAXPATHLEN];
	register int len;

	if ((len = readlink(name, lbuf, sizeof(lbuf))) == -1)
		err("%s: %s", name, strerror(errno));
	lbuf[len] = '\0';
	return (lbuf);
}
