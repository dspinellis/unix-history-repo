/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cmp.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include "ls.h"
#include "extern.h"

int
namecmp(a, b)
	LS *a, *b;
{
	return (strcmp(a->name, b->name));
}

int
revnamecmp(a, b)
	LS *a, *b;
{
	return (strcmp(b->name, a->name));
}

int
modcmp(a, b)
	LS *a, *b;
{
	return (b->lstat.st_mtime - a->lstat.st_mtime);
}

int
revmodcmp(a, b)
	LS *a, *b;
{
	return (a->lstat.st_mtime - b->lstat.st_mtime);
}

int
acccmp(a, b)
	LS *a, *b;
{
	return (b->lstat.st_atime - a->lstat.st_atime);
}

int
revacccmp(a, b)
	LS *a, *b;
{
	return (a->lstat.st_atime - b->lstat.st_atime);
}

int
statcmp(a, b)
	LS *a, *b;
{
	return (b->lstat.st_ctime - a->lstat.st_ctime);
}

int
revstatcmp(a, b)
	LS *a, *b;
{
	return (a->lstat.st_ctime - b->lstat.st_ctime);
}
