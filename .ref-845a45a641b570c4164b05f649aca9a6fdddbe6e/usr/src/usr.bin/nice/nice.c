/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)nice.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define	DEFNICE	10

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int niceness;

	niceness = DEFNICE;
	if (argv[1][0] == '-')
		if (isdigit(argv[1][1])) {
			niceness = atoi(argv[1] + 1);
			++argv;
		}
		else {
			(void)fprintf(stderr, "nice: illegal option -- %c\n",
			    argv[1][1]);
			usage();
		}

	if (!argv[1])
		usage();

	errno = 0;
	niceness += getpriority(PRIO_PROCESS, 0);
	if (errno) {
		(void)fprintf(stderr, "nice: getpriority: %s\n",
		    strerror(errno));
		exit(1);
	}
	if (setpriority(PRIO_PROCESS, 0, niceness)) {
		(void)fprintf(stderr,
		    "nice: setpriority: %s\n", strerror(errno));
		exit(1);
	}
	execvp(argv[1], &argv[1]);
	(void)fprintf(stderr,
	    "nice: %s: %s\n", argv[1], strerror(errno));
	exit(1);
}

void
usage()
{
	(void)fprintf(stderr,
	    "nice [ -# ] command [ options ] [ operands ]\n");
	exit(1);
}
