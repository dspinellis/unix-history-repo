/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
%sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)cmp.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include "ls.h"

namecmp(a, b)
	LS *a, *b;
{
	return(strcmp(a->name, b->name));
}

revnamecmp(a, b)
	LS *a, *b;
{
	return(strcmp(b->name, a->name));
}

modcmp(a, b)
	LS *a, *b;
{
	return(a->lstat.st_mtime < b->lstat.st_mtime);
}

revmodcmp(a, b)
	LS *a, *b;
{
	return(b->lstat.st_mtime < a->lstat.st_mtime);
}

acccmp(a, b)
	LS *a, *b;
{
	return(a->lstat.st_atime < b->lstat.st_atime);
}

revacccmp(a, b)
	LS *a, *b;
{
	return(b->lstat.st_atime < a->lstat.st_atime);
}

statcmp(a, b)
	LS *a, *b;
{
	return(a->lstat.st_ctime < b->lstat.st_ctime);
}

revstatcmp(a, b)
	LS *a, *b;
{
	return(b->lstat.st_ctime < a->lstat.st_ctime);
}
