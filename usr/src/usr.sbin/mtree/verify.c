/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)verify.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <dirent.h>
#include <stdio.h>
#include "mtree.h"

extern ENTRY *root;
extern char path[];

static dev_t device;

verify()
{
	extern int xflag;
	struct stat sbuf;

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

static
vwalk(level, tail)
	register ENTRY *level;
	register char *tail;
{
	extern int dflag, eflag, rflag, xflag;
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

static
miss(level, tail)
	register ENTRY *level;
	register char *tail;
{
	extern int dflag, uflag;
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

static
dirset(s1)
	register INFO *s1;
{
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

