/*-
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)misc.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <err.h>
#include <stdio.h>
#include <stdlib.h>

#include "extern.h"

void
eofmsg(file)
	char *file;
{
	if (!sflag)
		warnx("EOF on %s", file);
	exit(DIFF_EXIT);
}

void
diffmsg(file1, file2, byte, line)
	char *file1, *file2;
	off_t byte, line;
{
	if (!sflag)
		(void)printf("%s %s differ: char %qd, line %qd\n",
		    file1, file2, byte, line);
	exit(DIFF_EXIT);
}
