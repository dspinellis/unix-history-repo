/*
 * Copyright (c) 1990 Jan-Simon Pendry
 * Copyright (c) 1990 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)info_union.c	5.4 (Berkeley) %G%
 *
 * $Id: info_union.c,v 5.2.2.1 1992/02/09 15:08:34 jsp beta $
 *
 */

/*
 * Get info from the system namespace
 *
 * NOTE: Cannot handle reads back through the automounter.
 * THIS WILL CAUSE A DEADLOCK!
 */

#include "am.h"

#ifdef HAS_UNION_MAPS

#ifdef _POSIX_SOURCE
#include <dirent.h>
#define	DIRENT struct dirent
#else
#include <sys/dir.h>
#define	DIRENT struct direct
#endif

#define	UNION_PREFIX	"union:"
#define	UNION_PREFLEN	6

/*
 * No way to probe - check the map name begins with "union:"
 */
int union_init P((char *map, time_t *tp));
int union_init(map, tp)
char *map;
time_t *tp;
{
	*tp = 0;
	return strncmp(map, UNION_PREFIX, UNION_PREFLEN) == 0 ? 0 : ENOENT;
}

int union_search P((mnt_map *m, char *map, char *key, char **pval, time_t *tp));
int union_search(m, map, key, pval, tp)
mnt_map *m;
char *map;
char *key;
char **pval;
time_t *tp;
{
	char *mapd = strdup(map + UNION_PREFLEN);
	char **v = strsplit(mapd, ':', '\"');
	char **p;
	for (p = v; p[1]; p++)
		;
	*pval = xmalloc(strlen(*p) + 5);
	sprintf(*pval, "fs:=%s", *p);
	free(mapd);
	free(v);
	return 0;
}

int union_reload P((mnt_map *m, char *map, void (*fn)()));
int union_reload(m, map, fn)
mnt_map *m;
char *map;
void (*fn)();
{
	char *mapd = strdup(map + UNION_PREFLEN);
	char **v = strsplit(mapd, ':', '\"');
	char **dir;

	/*
	 * Add fake /defaults entry
	 */
	(*fn)(m, strdup("/defaults"), strdup("type:=link;opts:=nounmount;sublink:=${key}"));

	for (dir = v; *dir; dir++) {
		int dlen;
		DIRENT *dp;
		DIR *dirp = opendir(*dir);
		if (!dirp) {
			plog(XLOG_USER, "Cannot read directory %s: %m", *dir);
			continue;
		}
		dlen = strlen(*dir);
#ifdef DEBUG
		dlog("Reading directory %s...", *dir);
#endif
		while (dp = readdir(dirp)) {
			char *val;
			if (dp->d_name[0] == '.' &&
					(dp->d_name[1] == '\0' ||
					(dp->d_name[1] == '.' && dp->d_name[2] == '\0')))
				continue;

#ifdef DEBUG
			dlog("... gives %s", dp->d_name);
#endif
			val = xmalloc(dlen + 5);
			sprintf(val, "fs:=%s", *dir);
			(*fn)(m, strdup(dp->d_name), val);
		}
		closedir(dirp);
	}
	/*
	 * Add wildcard entry
	 */
	{ char *val = xmalloc(strlen(dir[-1]) + 5);
	  sprintf(val, "fs:=%s", dir[-1]);
	  (*fn)(m, strdup("*"), val);
	}
	free(mapd);
	free(v);
	return 0;
}

#endif /* HAS_UNION_MAPS */
