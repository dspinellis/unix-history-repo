/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)spec.c	5.18 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fts.h>
#include <pwd.h>
#include <grp.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <ctype.h>
#include "mtree.h"
#include "extern.h"

int lineno;				/* Current spec line number. */

static void	 set __P((char *, NODE *));
static void	 unset __P((char *, NODE *));

NODE *
spec()
{
	register NODE *centry, *last;
	register char *p;
	NODE ginfo, *root;
	int c_cur, c_next;
	char buf[2048];

	root = NULL;
	bzero(&ginfo, sizeof(ginfo));
	c_cur = c_next = 0;
	for (lineno = 1; fgets(buf, sizeof(buf), stdin);
	    ++lineno, c_cur = c_next, c_next = 0) {
		/* Skip empty lines. */
		if (buf[0] == '\n')
			continue;

		/* Find end of line. */
		if ((p = index(buf, '\n')) == NULL)
			err("line %d too long", lineno);

		/* See if next line is continuation line. */
		if (p[-1] == '\\') {
			--p;
			c_next = 1;
		}

		/* Null-terminate the line. */
		*p = '\0';

		/* Skip leading whitespace. */
		for (p = buf; *p && isspace(*p); ++p);

		/* If nothing but whitespace or comment char, continue. */
		if (!*p || *p == '#')
			continue;

#ifdef DEBUG
		(void)fprintf(stderr, "line %d: {%s}\n", lineno, p);
#endif
		if (c_cur) {
			set(p, centry);
			continue;
		}
			
		/* Grab file name, "$", "set", or "unset". */
		if ((p = strtok(p, "\n\t ")) == NULL)
			err("missing field");

		if (p[0] == '/')
			switch(p[1]) {
			case 's':
				if (strcmp(p + 1, "set"))
					break;
				set(NULL, &ginfo);
				continue;
			case 'u':
				if (strcmp(p + 1, "unset"))
					break;
				unset(NULL, &ginfo);
				continue;
			}

		if (index(p, '/'))
			err("slash character in file name");

		if (!strcmp(p, "..")) {
			/* Don't go up, if haven't gone down. */
			if (!root)
				goto noparent;
			if (last->type != F_DIR || last->flags & F_DONE) {
				if (last == root)
					goto noparent;
				last = last->parent;
			}
			last->flags |= F_DONE;
			continue;

noparent:		err("no parent node");
		}

		if ((centry = calloc(1, sizeof(NODE) + strlen(p))) == NULL)
			err("%s", strerror(errno));
		*centry = ginfo;
		(void)strcpy(centry->name, p);
#define	MAGIC	"?*["
		if (strpbrk(p, MAGIC))
			centry->flags |= F_MAGIC;
		set(NULL, centry);

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
	return (root);
}

static void
set(t, ip)
	char *t;
	register NODE *ip;
{
	register int type;
	register char *kw, *val;
	struct group *gr;
	struct passwd *pw;
	mode_t *m;
	int value;
	char *ep;

	for (; kw = strtok(t, "= \t\n"); t = NULL) {
		ip->flags |= type = parsekey(kw, &value);
		if (value && (val = strtok(NULL, " \t\n")) == NULL)
			err("missing value");
		switch(type) {
		case F_CKSUM:
			ip->cksum = strtoul(val, &ep, 10);
			if (*ep)
				err("invalid checksum %s", val);
			break;
		case F_GID:
			ip->st_gid = strtoul(val, &ep, 10);
			if (*ep)
				err("invalid gid %s", val);
			break;
		case F_GNAME:
			if ((gr = getgrnam(val)) == NULL)
			    err("unknown group %s", val);
			ip->st_gid = gr->gr_gid;
			break;
		case F_IGN:
			/* just set flag bit */
			break;
		case F_MODE:
			if ((m = setmode(val)) == NULL)
				err("invalid file mode %s", val);
			ip->st_mode = getmode(m, 0);
			break;
		case F_NLINK:
			ip->st_nlink = strtoul(val, &ep, 10);
			if (*ep)
				err("invalid link count %s", val);
			break;
		case F_SIZE:
			ip->st_size = strtoul(val, &ep, 10);
			if (*ep)
				err("invalid size %s", val);
			break;
		case F_SLINK:
			if ((ip->slink = strdup(val)) == NULL)
				err("%s", strerror(errno));
			break;
		case F_TIME:
			ip->st_mtimespec.ts_sec = strtoul(val, &ep, 10);
			if (*ep != '.')
				err("invalid time %s", val);
			val = ep + 1;
			ip->st_mtimespec.ts_nsec = strtoul(val, &ep, 10);
			if (*ep)
				err("invalid time %s", val);
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
				err("unknown file type %s", val);
			}
			break;
		case F_UID:
			ip->st_uid = strtoul(val, &ep, 10);
			if (*ep)
				err("invalid uid %s", val);
			break;
		case F_UNAME:
			if ((pw = getpwnam(val)) == NULL)
			    err("unknown user %s", val);
			ip->st_uid = pw->pw_uid;
			break;
		}
	}
}

static void
unset(t, ip)
	char *t;
	register NODE *ip;
{
	register char *p;

	while (p = strtok(t, "\n\t "))
		ip->flags &= ~parsekey(p, NULL);
}
