/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)pwd.c	5.1 (Berkeley) 4/30/85";
#endif not lint

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
