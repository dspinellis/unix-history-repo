/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rmdir.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * Remove directory
 */
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int errors;

	if (argc < 2) {
		fprintf(stderr, "usage: rmdir directory ...\n");
		exit(1);
	}
	for (errors = 0; *++argv;)
		if (rmdir(*argv) < 0) {
			fprintf(stderr, "rmdir: ");
			perror(*argv);
			errors = 1;
		}
	exit(errors);
}
