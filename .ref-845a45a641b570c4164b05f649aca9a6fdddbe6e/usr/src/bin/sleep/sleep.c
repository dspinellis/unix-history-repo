/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)sleep.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, secs;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 1)
		usage();

	if ((secs = atoi(*argv)) > 0)
		(void)sleep(secs);
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: sleep seconds\n");
	exit(1);
}
