/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)spec.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include "mtree.h"

extern NODE *root;			/* root of the tree */

static int lineno;			/* current spec line number */

spec()
{
	register NODE *centry, *last;
	register char *p;
	NODE ginfo, *emalloc();
	char buf[2048];

	bzero((void *)&ginfo, sizeof(ginfo));
	for (lineno = 1; fgets(buf, sizeof(buf), stdin); ++lineno) {
		if (!(p = index(buf, '\n'))) {
			(void)fprintf(stderr,
			    "mtree: line %d too long.\n", lineno);
			exit(1);
		}
		*p = '\0';
		for (p = buf; *p && isspace(*p); ++p);
		if (!*p || *p == '#')
			continue;

		/* grab file name, "$", "set", or "unset" */
		if (!(p = strtok(p, "\n\t ")))
			specerr();

		if (p[0] == '/')
			switch(p[1]) {
			case 's':
				if (strcmp(p + 1, "set"))
					break;
				set(&ginfo);
				continue;
			case 'u':
				if (strncmp(p + 1, "unset"))
					break;
				unset(&ginfo);
				continue;
			}

		if (index(p, '/')) {
			(void)fprintf(stderr,
			    "mtree: file names may not contain slashes.\n");
			specerr();
		}

		if (!strcmp(p, "..")) {
			/* don't go up, if haven't gone down */
			if (!root)
				noparent();
			if (last->type != F_DIR || last->flags & F_DONE) {
				if (last == root)
					noparent();
				last = last->parent;
			}
			last->flags |= F_DONE;
			continue;
		}

		centry = emalloc(sizeof(NODE) + strlen(p));
		*centry = ginfo;
		(void)strcpy(centry->name, p);
#define	MAGIC	"?*["
		if (strpbrk(p, MAGIC))
			centry->flags |= F_MAGIC;
		set(centry);

		if (!root) {
			last = root = centry;
			root->parent = root;
		} else if (last->type == F_DIR && !(last->flags & F_DONE)) {
			centry->parent = last;
			last = last->child = centry;
		} else {
			centry->parent = last->parent;
			centry->prev = last;
			last = last->next = centry;
		}
	}
}

set(ip)
	register NODE *ip;
{
	register int type;
	register char *kw, *val;
	gid_t getgroup();
	uid_t getowner();
	long atol(), strtol();

	while (kw = strtok((char *)NULL, "= \t\n")) {
		ip->flags |= type = key(kw);
		val = strtok((char *)NULL, " \t\n");
		if (!val)
			specerr();
		switch(type) {
		case F_CKSUM:
			ip->cksum = atol(val);
			break;
		case F_GROUP:
			ip->st_gid = getgroup(val);
			break;
		case F_IGN:
			/* just set flag bit */
			break;
		case F_MODE: {
			mode_t *m, *setmode();

			if (!(m = setmode(val))) {
				(void)fprintf(stderr,
				    "mtree: invalid file mode %s.\n", val);
				specerr();
			}
			ip->st_mode = getmode(m, 0);
			break;
		}
		case F_NLINK:
			ip->st_nlink = atoi(val);
			break;
		case F_OWNER:
			ip->st_uid = getowner(val);
			break;
		case F_SIZE:
			ip->st_size = atol(val);
			break;
		case F_SLINK:
			if (!(ip->slink = strdup(val)))
				nomem();
			break;
		case F_TIME:
			ip->st_mtime = atol(val);
			break;
		case F_TYPE:
			switch(*val) {
			case 'b':
				if (!strcmp(val, "block"))
					ip->type = F_BLOCK;
				break;
			case 'c':
				if (!strcmp(val, "char"))
					ip->type = F_CHAR;
				break;
			case 'd':
				if (!strcmp(val, "dir"))
					ip->type = F_DIR;
				break;
			case 'f':
				if (!strcmp(val, "file"))
					ip->type = F_FILE;
				if (!strcmp(val, "fifo"))
					ip->type = F_FIFO;
				break;
			case 'l':
				if (!strcmp(val, "link"))
					ip->type = F_LINK;
				break;
			case 's':
				if (!strcmp(val, "socket"))
					ip->type = F_SOCK;
				break;
			default:
				(void)fprintf(stderr,
				    "mtree: unknown file type %s.\n", val);
				specerr();
			}
			break;
		}
	}
}

unset(ip)
	register NODE *ip;
{
	register char *p;

	while (p = strtok((char *)NULL, "\n\t "))
		ip->flags &= ~key(p);
}

key(p)
	char *p;
{
	switch(*p) {
	case 'c':
		if (!strcmp(p, "cksum"))
			return(F_CKSUM);
		break;
	case 'g':
		if (!strcmp(p, "group"))
			return(F_GROUP);
		break;
	case 'i':
		if (!strcmp(p, "ignore"))
			return(F_IGN);
		break;
	case 'l':
		if (!strcmp(p, "link"))
			return(F_SLINK);
		break;
	case 'm':
		if (!strcmp(p, "mode"))
			return(F_MODE);
		break;
	case 'n':
		if (!strcmp(p, "nlink"))
			return(F_NLINK);
		break;
	case 'o':
		if (!strcmp(p, "owner"))
			return(F_OWNER);
		break;
	case 's':
		if (!strcmp(p, "size"))
			return(F_SIZE);
		break;
	case 't':
		if (!strcmp(p, "type"))
			return(F_TYPE);
		if (!strcmp(p, "time"))
			return(F_TIME);
		break;
	}
	(void)fprintf(stderr, "mtree: unknown keyword %s.\n", p);
	specerr();
	/* NOTREACHED */
}


uid_t
getowner(p)
	register char *p;
{
	struct passwd *pw;
	int val;

	if (isdigit(*p)) {
		if ((val = atoi(p)) >= 0)
			return((uid_t)val);
		(void)fprintf(stderr, "mtree: illegal uid value %s.\n", p);
	} else if (pw = getpwnam(p))
		return(pw->pw_uid);
	else
		(void)fprintf(stderr, "mtree: unknown user %s.\n", p);
	specerr();
	/* NOTREACHED */
}

gid_t
getgroup(p)
	register char *p;
{
	struct group *gr;
	int val;

	if (isdigit(*p)) {
		if ((val = atoi(p)) >= 0)
			return((gid_t)val);
		(void)fprintf(stderr, "mtree: illegal gid value %s.\n", p);
	} else if (gr = getgrnam(p))
		return(gr->gr_gid);
	else
		(void)fprintf(stderr, "mtree: unknown group %s.\n", p);
	specerr();
	/* NOTREACHED */
}

noparent()
{
	(void)fprintf(stderr, "mtree: no parent node.\n");
	specerr();
}

specerr()
{
	(void)fprintf(stderr,
	    "mtree: line %d of the specification is incorrect.\n", lineno);
	exit(1);
}

NODE *
emalloc(size)
	int size;
{
	void *p;

	/* NOSTRICT */
	if (!(p = malloc((u_int)size)))
		nomem();
	bzero(p, size);
	return((NODE *)p);
}

nomem()
{
	(void)fprintf(stderr, "mtree: %s.\n", strerror(ENOMEM));
	exit(1);
}
