/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1992, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rmdir.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, errors;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc == 0)
		usage();

	for (errors = 0; *argv; ++argv)
		if (rmdir(*argv) < 0) {
			warn("%s", *argv);
			errors = 1;
		}
	exit(errors);
}

void
usage()
{
	(void)fprintf(stderr, "usage: rmdir directory ...\n");
	exit(1);
}
