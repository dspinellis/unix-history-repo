/*
 * Copyright (c) 1988 Mark Nudleman
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)help.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <less.h>
#include "pathnames.h"

help()
{
	char cmd[MAXPATHLEN + 20];

	(void)sprintf(cmd, "-more %s", _PATH_HELPFILE);
	lsystem(cmd);
}
