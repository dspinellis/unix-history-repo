/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)dirname.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	char *p, *rindex();

	if (argc != 2) {
		fprintf(stderr, "usage: dirname path\n");
		exit(1);
	}
	if (p = rindex(*++argv, '/'))
		if (p > *argv)
			*p = '\0';
		else
			*++p = '\0';
	else
		*argv = ".";
	printf("%s\n", *argv);
	exit(0);
}
