/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)lastpart.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include "uucp.h"

/*LINTLIBRARY*/

/*
 *	find last part of file name
 *
 *	return - pointer to last part
 */

char *
lastpart(file)
register char *file;
{
	register char *c;

	c = rindex(file, '/');
	if (c++)
		return c;
	else
		return file;
}
