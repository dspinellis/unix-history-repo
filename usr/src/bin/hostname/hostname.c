/*
 * Copyright (c) 1983, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)hostname.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <sys/param.h>

main(argc,argv)
	int argc;
	char **argv;
{
	extern int optind;
	int ch, sflag;
	char hostname[MAXHOSTNAMELEN], *p, *index();

	sflag = 0;
	while ((ch = getopt(argc, argv, "s")) != EOF)
		switch((char)ch) {
		case 's':
			sflag = 1;
			break;
		case '?':
		default:
			fputs("hostname [-s] [hostname]\n", stderr);
			exit(1);
		}
	argv += optind;

	if (*argv) {
		if (sethostname(*argv, strlen(*argv))) {
			perror("sethostname");
			exit(1);
		}
	} else {
		if (gethostname(hostname, sizeof(hostname))) {
			perror("gethostname");
			exit(1);
		}
		if (sflag && (p = index(hostname, '.')))
			*p = '\0';
		puts(hostname);
	}
	exit(0);
}
