/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)accton.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
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
	int ch;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	switch(argc) {
	case 0: 
		if (acct(NULL)) {
			(void)fprintf(stderr,
			    "accton: %s\n", strerror(errno));
			exit(1);
		}
		break;
	case 1:
		if (acct(*argv)) {
			(void)fprintf(stderr,
			    "accton: %s: %s\n", *argv, strerror(errno));
			exit(1);
		}
		break;
	default:
		usage();
	}
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: accton [file]\n");
	exit(1);
}
