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
static char sccsid[] = "@(#)accton.c	4.3 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	if (argc > 2) {
		fputs("usage: accton [file]\n", stderr);
		exit(1);
	}
	if (acct(argc == 2 ? argv[1] : (char *)NULL)) {
		perror("accton");
		exit(1);
	}
	exit(0);
}
