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
static char sccsid[] = "@(#)hostname.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void usage __P((void));

int
main(argc,argv)
	int argc;
	char *argv[];
{
	int ch, sflag;
	char *p, hostname[MAXHOSTNAMELEN];

	sflag = 0;
	while ((ch = getopt(argc, argv, "s")) != -1)
		switch (ch) {
		case 's':
			sflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (*argv) {
		if (sethostname(*argv, strlen(*argv)))
			err(1, "sethostname");
	} else {
		if (gethostname(hostname, sizeof(hostname)))
			err(1, "gethostname");
		if (sflag && (p = strchr(hostname, '.')))
			*p = '\0';
		(void)printf("%s\n", hostname);
	}
	exit(0);
}

void
usage()
{

	(void)fprintf(stderr, "usage: hostname [-s] [hostname]\n");
	exit(1);
}
