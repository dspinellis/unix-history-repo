/*-
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rmdir.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <errno.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

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
			(void)fprintf(stderr,
			    "rmdir: %s: %s\n", *argv, strerror(errno));
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
