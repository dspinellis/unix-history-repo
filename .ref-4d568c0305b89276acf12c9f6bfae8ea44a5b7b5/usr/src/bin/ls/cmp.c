/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cmp.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <fts.h>
#include <string.h>

#include "ls.h"
#include "extern.h"

int
namecmp(a, b)
	const FTSENT *a, *b;
{
	return (strcmp(a->fts_name, b->fts_name));
}

int
revnamecmp(a, b)
	const FTSENT *a, *b;
{
	return (strcmp(b->fts_name, a->fts_name));
}

int
modcmp(a, b)
	const FTSENT *a, *b;
{
	return (b->fts_statp->st_mtime - a->fts_statp->st_mtime);
}

int
revmodcmp(a, b)
	const FTSENT *a, *b;
{
	return (a->fts_statp->st_mtime - b->fts_statp->st_mtime);
}

int
acccmp(a, b)
	const FTSENT *a, *b;
{
	return (b->fts_statp->st_atime - a->fts_statp->st_atime);
}

int
revacccmp(a, b)
	const FTSENT *a, *b;
{
	return (a->fts_statp->st_atime - b->fts_statp->st_atime);
}

int
statcmp(a, b)
	const FTSENT *a, *b;
{
	return (b->fts_statp->st_ctime - a->fts_statp->st_ctime);
}

int
revstatcmp(a, b)
	const FTSENT *a, *b;
{
	return (a->fts_statp->st_ctime - b->fts_statp->st_ctime);
}
