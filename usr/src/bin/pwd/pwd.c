/*
 * Copyright (c) 1991 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pwd.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch;
	char *p;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 0)
		usage();

	p = getcwd(NULL, 0);
	if (p) {
		(void)printf("%s\n", p);
		exit(0);
	}
	(void)fprintf(stderr, "pwd: %s\n", strerror(errno));
	exit(1);
}

void
usage()
{
	(void)fprintf(stderr, "usage: pwd\n");
	exit(1);
}
