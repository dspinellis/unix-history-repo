/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)anyread.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include "uucp.h"
#include <sys/stat.h>

/*LINTLIBRARY*/

/*
 *	anyread		check if anybody can read
 *	return SUCCESS ok: FAIL not ok
 */
anyread(file)
char *file;
{
	struct stat s;

	if (stat(subfile(file), &s) < 0)
		/* for security check a non existant file is readable */
		return SUCCESS;
	if (!(s.st_mode & ANYREAD))
		return FAIL;
	return SUCCESS;
}
