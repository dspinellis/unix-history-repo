/*
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1990, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkfifo.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <errno.h>
#include <stdio.h>
#include <string.h>

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	int ch, exitval;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;
	if (argv[0] == NULL)
		usage();

	for (exitval = 0; *argv != NULL; ++argv)
		if (mkfifo(*argv, S_IRWXU | S_IRWXG | S_IRWXO) < 0) {
			warn("%s", *argv);
			exitval = 1;
		}
	exit(exitval);
}

usage()
{
	(void)fprintf(stderr, "usage: mkfifo fifoname ...\n");
	exit(1);
}
