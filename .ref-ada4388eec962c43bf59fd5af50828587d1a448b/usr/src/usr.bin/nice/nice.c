/*
 * Copyright (c) 1989, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1989, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)nice.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>

#include <ctype.h>
#include <err.h>
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
	int niceness = DEFNICE;

	if (argv[1] == NULL)
		usage();
	if (argv[1][0] == '-')
		if (argv[1][1] == '-' || isdigit(argv[1][1])) {
			niceness = atoi(argv[1] + 1);
			++argv;
			if (argv[1] == NULL)
				usage();
		} else
			errx(1, "illegal option -- %s", argv[1]);

	errno = 0;
	niceness += getpriority(PRIO_PROCESS, 0);
	if (errno)
		err(1, "getpriority");
	if (setpriority(PRIO_PROCESS, 0, niceness))
		err(1, "setpriority");
	execvp(argv[1], &argv[1]);
	err(1, "%s", argv[1]);
}

void
usage()
{
	(void)fprintf(stderr,
	    "nice [ -# ] command [ options ] [ operands ]\n");
	exit(1);
}
