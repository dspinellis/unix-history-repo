/*-
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1991, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)logname.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <err.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
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
		switch (ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if ((p = getlogin()) == NULL)
		err(1, NULL);
	(void)printf("%s\n", p);
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr, "usage: logname\n");
	exit(1);
}
