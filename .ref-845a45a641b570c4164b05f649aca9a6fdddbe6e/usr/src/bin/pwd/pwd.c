/*
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1991, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pwd.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

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
	int ch;
	char *p;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch (ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 0)
		usage();

	if ((p = getcwd(NULL, 0)) == NULL) {
		err(1, NULL);
		exit(1);
	}
	(void)printf("%s\n", p);
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: pwd\n");
	exit(1);
}
