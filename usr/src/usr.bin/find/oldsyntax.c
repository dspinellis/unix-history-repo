/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)oldsyntax.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

/*
 * oldsyntax --
 *	move the path names to the beginning of the argv array, and return
 *	a pointer to them.  The old find syntax assumes all command arguments
 *	up to the first one beginning with a '-', '(' or '!' are pathnames.
 */
void
oldsyntax(argvp)
	char ***argvp;
{
	register char **argv;

	/*
	 * find first '-', '(' or '!' to delimit paths; if no paths, it's
	 * an error.  Shift the array back one at the same time, creating
	 * a separate array of pathnames.
	 */
	for (argv = *argvp + 1;; ++argv) {
		argv[-1] = argv[0];
		if (!*argv || **argv == '-' || **argv == '!' || **argv == '(')
			break;
	}

	if (argv == *argvp + 1)
		usage();

	argv[-1] = NULL;
	*argvp = argv;			/* move argv value */
}
