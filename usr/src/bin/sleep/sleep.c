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
static char sccsid[] = "@(#)sleep.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int secs;

	if (argc != 2) {
		(void)fprintf(stderr, "usage: sleep time\n");
		exit(1);
	}
	if ((secs = atoi(argv[1])) > 0)
		(void)sleep(secs);
	exit(0);
}
