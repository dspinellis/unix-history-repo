/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Michael Fischbein.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)cmp.c	5.2 (Berkeley) 4/8/90";
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
