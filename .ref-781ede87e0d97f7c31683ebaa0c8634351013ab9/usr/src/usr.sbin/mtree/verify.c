/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)verify.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <fts.h>
#include <unistd.h>
#include <errno.h>
#include <stdio.h>
#include "mtree.h"

extern NODE *root;

static char path[MAXPATHLEN];

verify()
{
	vwalk();
	miss(root, path);
}

vwalk()
{
	extern int ftsoptions, dflag, eflag, rflag;
	register FTS *t;
	register FTSENT *p;
	register NODE *ep, *level;
	char *argv[2];
	int ftsdepth = 0, specdepth = 0;

	argv[0] = ".";
	argv[1] = (char *)NULL;
	if (!(t = fts_open(argv, ftsoptions, (int (*)())NULL))) {
		(void)fprintf(stderr,
		    "mtree: fts_open: %s.\n", strerror(errno));
		exit(1);
	}
	level = root;
	while (p = fts_read(t)) {
		switch(p->fts_info) {
		case FTS_D:
			if (!strcmp(p->fts_name, "."))
				continue;
			ftsdepth++; 
			break;
		case FTS_DC:
			(void)fprintf(stderr,
			    "mtree: directory cycle: %s.\n", RP(p));
			continue;
		case FTS_DNR:
		case FTS_DNX:
			(void)fprintf(stderr,
			    "mtree: %s: unable to read or search.\n", RP(p));
		case FTS_DP:
			ftsdepth--; 
			if (specdepth > ftsdepth) {
				for (level = level->parent; level->prev;
				      level = level->prev);  
				specdepth--;
			}
			continue;
		case FTS_ERR:
			(void)fprintf(stderr, "mtree: %s: %s.\n",
			    RP(p), strerror(errno));
			continue;
		case FTS_NS:
			(void)fprintf(stderr,
			    "mtree: can't stat: %s.\n", RP(p));
			continue;
		default:
			if (dflag)
				continue;
		}

		for (ep = level; ep; ep = ep->next)
			if (ep->flags & F_MAGIC && fnmatch(ep->name,
			    p->fts_name, FNM_PATHNAME|FNM_QUOTE) ||
			    !strcmp(ep->name, p->fts_name)) {
				ep->flags |= F_VISIT;
				if (ep->flags & F_IGN) {
					(void)fts_set(t, p, FTS_SKIP);
					continue;
				}
				compare(ep->name, ep, p);
				if (ep->child && ep->type == F_DIR &&
				    p->fts_info == FTS_D) {
					level = ep->child;
					specdepth++;
				}
				break;
			}

		if (ep)
			continue;
		if (!eflag) {
			(void)printf("extra: %s", RP(p));
			if (rflag) {
				if (unlink(p->fts_accpath)) {
					(void)printf(", not removed: %s",
					    strerror(errno));
				} else
					(void)printf(", removed");
			}
			(void)putchar('\n');
		}
		(void)fts_set(t, p, FTS_SKIP);
	}
	(void)fts_close(t);
}

miss(p, tail)
	register NODE *p;
	register char *tail;
{
	extern int dflag, uflag;
	register int create;
	register char *tp;

	for (; p; p = p->next) {
		if (p->type != F_DIR && (dflag || p->flags & F_VISIT))
			continue;
		(void)strcpy(tail, p->name);
		if (!(p->flags & F_VISIT))
			(void)printf("missing: %s", path);
		if (p->type != F_DIR) {
			putchar('\n');
			continue;
		}

		create = 0;
		if (!(p->flags & F_VISIT) && uflag)
#define	MINBITS	(F_GROUP|F_MODE|F_OWNER)
			if ((p->flags & MINBITS) != MINBITS)
				(void)printf(" (not created -- group, mode or owner not specified)");
			else if (mkdir(path, S_IRWXU))
				(void)printf(" (not created: %s)",
				    strerror(errno));
			else {
				create = 1;
				(void)printf(" (created)");
			}

		if (!(p->flags & F_VISIT))
			(void)putchar('\n');

		for (tp = tail; *tp; ++tp);
		*tp = '/';
		miss(p->child, tp + 1);
		*tp = '\0';

		if (!create)
			continue;
		if (chown(path, p->st_uid, p->st_gid)) {
			(void)printf("%s: owner/group/mode not modified: %s\n",
			    path, strerror(errno));
			continue;
		}
		if (chmod(path, p->st_mode))
			(void)printf("%s: permissions not set: %s\n",
			    path, strerror(errno));
	}
}
