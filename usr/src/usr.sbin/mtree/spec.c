/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)spec.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <pwd.h>
#include <grp.h>
#include <ctype.h>
#include <stdio.h>
#include "mtree.h"

extern ENTRY *root;			/* root of the tree */
mode_t dmode;
mode_t fmode;

static int lineno;			/* current spec line number */

spec()
{
	register char *p;
	ENTRY *centry, *last;
	INFO info;
	int ch, ignore;
	char buf[1024], *emalloc();

	info.flags = 0;
	last = NULL;
	for (lineno = 1; fgets(buf, sizeof(buf), stdin); ++lineno) {
		if (!(p = index(buf, '\n'))) {
			(void)fprintf(stderr,
			    "mtree: line %d too long, ignored.\n", lineno);
			while ((ch = getchar()) != '\n' && ch != EOF);
			continue;
		}
		*p = '\0';
		for (p = buf; *p && isspace(*p); ++p);
		if (!*p || *p == '#')
			continue;

		/* grab file name, "$", "set", or "unset" */
		if (!(p = strtok(buf, "\n\t ")))
			specerr();

		ignore = 0;
		if (p[0] == '/')
			switch(p[1]) {
			case 'i':
				ignore = 1;
				if (!(p = strtok((char *)NULL, "\t ")))
					specerr();
				break;
			case 's':
				if (strcmp(p + 1, "set"))
					break;
				if (!(p = strtok((char *)NULL, "\t ")))
					specerr();
				set(p, &info, 1);
				continue;
			case 'u':
				if (strcmp(p + 1, "unset"))
					break;
				if (!(p = strtok((char *)NULL, "\t ")))
					specerr();
				unset(p, &info);
				continue;
			}

		if (index(p, '/')) {
			(void)fprintf(stderr,
			    "mtree: file names may not contain slashes.\n");
			specerr();
		}

		if (!(info.flags&F_TYPE)) {
			(void)fprintf(stderr, "mtree: no type set.\n");
			specerr();
		}

		if (!strcmp(p, "..")) {
			/* don't go up, if haven't gone down */
			if (!root)
				noparent();
			if (last->info.type != F_DIR || last->flags&F_DONE) {
				if (last == root)
					noparent();
				last = last->parent;
			}
			last->flags |= F_DONE;
			continue;
		}

		centry = (ENTRY *)emalloc(sizeof(ENTRY));
		if (!(centry->name = strdup(p)))
			nomem();
		centry->info = info;
		centry->info.st_mode = info.type == F_DIR ? dmode : fmode;
		centry->flags = ignore;
		while (p = strtok((char *)NULL, "\t "))
			set(p, &centry->info, 0);

		if (!root) {
			last = root = centry;
			root->parent = root;
		} else if (last->info.type == F_DIR && !(last->flags&F_DONE)) {
			centry->parent = last;
			last = last->child = centry;
		} else {
			centry->parent = last->parent;
			last = last->next = centry;
		}
	}
}

static
set(p, ip, override)
	register char *p;
	INFO *ip;
	int override;
{
	extern mode_t dmode, fmode;
	int val;
	char *kw;
	gid_t getgroup();
	uid_t getowner();
	long atol(), strtol();

	for (kw = p; *p && *p != '='; ++p);
	if (!*p)
		specerr();
	*p++ = '\0';
	ip->flags |= val = key(kw);

	switch(val) {
	case F_CKSUM:
		ip->cksum = atol(p);
		break;
	case F_DMODE:
		if (!override) {
			(void)fprintf(stderr,
			    "mtree: keyword dmode is global only.\n");
			specerr();
		}
		dmode = (mode_t)strtol(p, (char **)NULL, 8);
		break;
	case F_FMODE:
		if (!override) {
			(void)fprintf(stderr,
			    "mtree: keyword fmode is global only.\n");
			specerr();
		}
		fmode = (mode_t)strtol(p, (char **)NULL, 8);
		break;
	case F_GROUP:
		ip->st_gid = getgroup(p);
		break;
	case F_MODE:
		if (override) {
			(void)fprintf(stderr,
			    "mtree: keyword mode is local only.\n");
			specerr();
		}
		ip->st_mode = (mode_t)strtol(p, (char **)NULL, 8);
		break;
	case F_NLINK:
		if ((ip->st_nlink = atoi(p)) <= 0)
			specerr();
		break;
	case F_OWNER:
		ip->st_uid = getowner(p);
		break;
	case F_SIZE:
		if ((ip->st_size = atol(p)) < 0)
			specerr();
		break;
	case F_SLINK:
		if (!(ip->slink = strdup(p)))
			nomem();
		break;
	case F_TYPE:
		ip->type = fkey(p);
		break;
	}
}

static
unset(p, ip)
	char *p;
	INFO *ip;
{
	ip->flags &= !key(p);
}

static
key(p)
	char *p;
{
	switch(*p) {
	case 'c':
		if (!strcmp(p, "cksum"))
			return(F_CKSUM);
		break;
	case 'd':
		if (!strcmp(p, "dmode"))
			return(F_DMODE);
		break;
	case 'f':
		if (!strcmp(p, "fmode"))
			return(F_FMODE);
		break;
	case 'g':
		if (!strcmp(p, "group"))
			return(F_GROUP);
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
		if (!strcmp(p, "slink"))
			return(F_SLINK);
		break;
	case 't':
		if (!strcmp(p, "type"))
			return(F_TYPE);
		break;
	}
	(void)fprintf(stderr, "mtree: unknown keyword.\n");
	specerr();
	/* NOTREACHED */
}

static
fkey(p)
	char *p;
{
	switch(*p) {
	case 'b':
		if (!strcmp(p, "block"))
			return(F_BLOCK);
		break;
	case 'c':
		if (!strcmp(p, "char"))
			return(F_CHAR);
		break;
	case 'd':
		if (!strcmp(p, "dir"))
			return(F_DIR);
		break;
	case 'f':
		if (!strcmp(p, "file"))
			return(F_FILE);
		break;
	case 'l':
		if (!strcmp(p, "link"))
			return(F_LINK);
		break;
	case 's':
		if (!strcmp(p, "socket"))
			return(F_SOCK);
		break;
	}
	(void)fprintf(stderr, "mtree: unknown file type.\n");
	specerr();
	/* NOTREACHED */
}

static uid_t
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

static gid_t
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

static
noparent()
{
	(void)fprintf(stderr, "mtree: no parent node.\n");
	specerr();
}

static
specerr()
{
	(void)fprintf(stderr,
	    "mtree: line %d of the specification is incorrect.\n", lineno);
	exit(1);
}
