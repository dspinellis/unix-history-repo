/*
 * $Id: info_ndbm.c,v 5.2 90/06/23 22:19:31 jsp Rel $
 *
 * Copyright (c) 1989 Jan-Simon Pendry
 * Copyright (c) 1989 Imperial College of Science, Technology & Medicine
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry at Imperial College, London.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)info_ndbm.c	5.1 (Berkeley) 6/29/90
 */

/*
 * Get info from NDBM map
 */

#include "am.h"

#ifdef HAS_NDBM_MAPS

#include <ndbm.h>
#include <fcntl.h>
#include <sys/stat.h>

static int search_ndbm(db, key, val)
DBM *db;
char *key;
char **val;
{
	datum k, v;
	k.dptr = key;
	k.dsize = strlen(key) + 1;
	v = dbm_fetch(db, k);
	if (v.dptr) {
		*val = strdup(v.dptr);
		return 0;
	}
	return ENOENT;
}

int ndbm_search(m, map, key, pval, tp)
mnt_map *m;
char *map;
char *key;
char **pval;
time_t *tp;
{
	DBM *db;

	db = dbm_open(map, O_RDONLY, 0);
	if (db) {
		struct stat stb;
		int error;
		error = fstat(dbm_pagfno(db), &stb);
		if (!error && *tp < stb.st_mtime) {
			*tp = stb.st_mtime;
			error = -1;
		} else {
			error = search_ndbm(db, key, pval);
		}
		(void) dbm_close(db);
		return error;
	}

	return errno;
}

int ndbm_init(map)
char *map;
{
	DBM *db;

	db = dbm_open(map, O_RDONLY, 0);
	if (db) {
		dbm_close(db);
		return 0;
	}

	return errno;
}

#endif /* HAS_NDBM_MAPS */
