/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)logname.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

	if ((p = getlogin()) == NULL) {
		(void)fprintf(stderr, "logname: %s\n", strerror(errno));
		exit(1);
	}
	(void)printf("%s\n", p);
	exit(0);
}

usage()
{
	(void)fprintf(stderr, "usage: logname\n");
	exit(1);
}
