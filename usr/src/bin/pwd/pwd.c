/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pwd.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * Print working (current) directory
 */
#include <stdio.h>
#include <sys/param.h>

char *getwd();

main()
{
	char pathname[MAXPATHLEN + 1];

	if (getwd(pathname) == NULL) {
		fprintf(stderr, "pwd: %s\n", pathname);
		exit(1);
	}
	printf("%s\n", pathname);
	exit(0);
}
