/*
 * Copyright (c) 1988, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)env.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <err.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

extern char **environ;

int
main(argc, argv)
	int argc;
	char **argv;
{
	char **ep, *p;
	char *cleanenv[1];
	int ch;

	while ((ch = getopt(argc, argv, "-")) != EOF)
		switch(ch) {
		case '-':
			environ = cleanenv;
			cleanenv[0] = NULL;
			break;
		case '?':
		default:
			(void)fprintf(stderr,
			    "usage: env [-] [name=value ...] [command]\n");
			exit(1);
		}
	for (argv += optind; *argv && (p = strchr(*argv, '=')); ++argv)
		(void)setenv(*argv, ++p, 1);
	if (*argv) {
		execvp(*argv, argv);
		err(1, "%s", *argv);
	}
	for (ep = environ; *ep; ep++)
		(void)printf("%s\n", *ep);
	exit(0);
}
