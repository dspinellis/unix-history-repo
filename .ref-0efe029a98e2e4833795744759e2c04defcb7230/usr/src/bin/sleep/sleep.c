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
static char sccsid[] = "@(#)sleep.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int secs;

	if (argc != 2) {
		fputs("usage: sleep time\n", stderr);
		exit(1);
	}
	if ((secs = atoi(argv[1])) > 0)
		(void)sleep(secs);
	exit(0);
}
