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
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mtree.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <stdio.h>
#include <string.h>
#include "mtree.h"

extern int errno;
ENTRY *root;
dev_t device;
int cflag, dflag, eflag, rflag, uflag, xflag, exitval;
char path[MAXPATHLEN];

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	struct stat sbuf;
	int ch;
	char *p;

	p = NULL;
	while ((ch = getopt(argc, argv, "cdef:p:rux")) != EOF)
		switch((char)ch) {
		case 'c':
			cflag = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 'e':
			eflag = 1;
			break;
		case 'f':
			if (!(freopen(optarg, "r", stdin))) {
				(void)fprintf(stderr,
				    "mtree: can't read %s.\n", optarg);
				exit(1);
			}
			break;
		case 'p':
			p = optarg;
			break;
		case 'r':
			rflag = 1;
			break;
		case 'u':
			uflag = 1;
			break;
		case 'x':
			xflag = 1;
			break;
		case '?':
		default:
			(void)fprintf(stderr,
			    "usage: mtree [-cderux] [-p path] [-f spec]\n");
			exit(1);
		}
	if (!cflag)
		spec();
	if (p && chdir(p)) {
		(void)fprintf(stderr,
		    "mtree: %s: %s\n", p, strerror(errno));
		exit(1);
	}
	path[0] = '.';
	if (cflag) {
		headers();
		cwalk((ENTRY *)NULL, path + 1);
		shostats();
		pwalk(root, 0);
	} else {
		if (xflag) {
			if (lstat(".", &sbuf)) {
				(void)fprintf(stderr, "mtree: root: %s\n",
				    strerror(errno));
				exit(1);
			}
			device = sbuf.st_dev;
		}
		vwalk(root, path + 1);
		miss(root, path + 1);
	}
	exit(exitval);
}

vwalk(level, tail)
	register ENTRY *level;
	register char *tail;
{
	register ENTRY *ep;
	register DIR *dirp;
	register struct dirent *dp;
	struct stat sbuf;

	if (!(dirp = opendir("."))) {
		(void)fprintf(stderr, "mtree: %s: %s\n",
		    level == root ? "root" : path, strerror(errno));
		exit(1);
	}
	*tail++ = '/';
	while ((dp = readdir(dirp))) {
		if (dp->d_name[0] == '.' &&
		    (!dp->d_name[1] || dp->d_name[1] == '.' && !dp->d_name[2]))
			continue;
		bcopy(dp->d_name, tail, dp->d_namlen + 1);
		for (ep = level; ep; ep = ep->next)
			if (!strcmp(ep->name, dp->d_name))
				break;
		if (ep && ep->flags&F_IGN) {
			ep->flags |= F_VISIT;
			continue;
		}
		if (lstat(dp->d_name, &sbuf)) {
			(void)fprintf(stderr, "mtree: %s: %s\n",
			    path + 2, strerror(errno));
			exit(1);
		}
		if (!dflag || S_ISDIR(sbuf.st_mode))
			if (ep) {
				compare(ep->name, &ep->info, &sbuf);
				ep->flags |= F_VISIT;
			} else if (!eflag) {
				(void)printf("extra: %s%s",
				    path + 2, rflag ? "" : "\n");
				if (rflag)
					if (unlink(path))
					    (void)printf(", not removed: %s\n",
						strerror(errno));
					else
					    (void)printf(", removed\n");
			}
		if (S_ISDIR(sbuf.st_mode) &&
		    (!xflag || device == sbuf.st_dev)) {
			if (chdir(dp->d_name)) {
				(void)fprintf(stderr, "mtree: %s: %s\n",
				    path + 2, strerror(errno));
				exit(1);
			}
			vwalk(ep ? ep->child : ep, tail + dp->d_namlen);
			if (chdir("..")) {
				(void)fprintf(stderr, "mtree: ..: %s\n",
				    strerror(errno));
				exit(1);
			}
		}
	}
	(void)closedir(dirp);
}

miss(level, tail)
	register ENTRY *level;
	register char *tail;
{
	register int create;
	register char *p;

	for (*tail++ = '/'; level; level = level->next) {
		if (level->info.type != F_DIR &&
			(dflag || level->flags&F_VISIT))
				continue;
		(void)strcpy(tail, level->name);
		if (!(level->flags&F_VISIT))
			(void)printf("missing: %s%s", path + 2,
			    uflag ? "" : "\n");
		if (level->info.type != F_DIR)
			continue;
		create = 0;
		if (uflag)
			if (mkdir(path, 0777))
				(void)printf(" (not created: %s)\n",
				    strerror(errno));
			else {
				create = 1;
				(void)printf(" (created)\n");
			}
		for (p = tail; *p; ++p);
		miss(level->child, p);
		if (create) {
			*p = '\0';
			dirset(&level->info);
		}
	}
}

dirset(s1)
	register INFO *s1;
{
	extern int errno;
	register struct stat *s2;
	struct stat sbuf;

	if (stat(path, &sbuf)) {
		(void)fprintf(stderr,
		    "mtree: %s: %s\n", path, strerror(errno));
		return;
	}
	s2 = &sbuf;

	if (s1->flags&F_MODE && s1->st_mode != (s2->st_mode&07777) &&
	    chmod(path, s1->st_mode))
		(void)printf("%s: permissions not set: %s\n",
		    path + 2, strerror(errno));
	if (s1->flags&F_OWNER && s1->st_uid != s2->st_uid &&
	    chown(path, s1->st_uid, -1))
		(void)printf("%s: owner not modified: %s\n",
		    path + 2, strerror(errno));
	if (s1->flags&F_GROUP && s1->st_gid != s2->st_gid &&
	    chown(path, -1, s1->st_gid))
		(void)printf("%s: group not modified: %s\n",
		    path + 2, strerror(errno));
}
