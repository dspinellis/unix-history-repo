/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdir.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int err;

	if (argc < 2) {
		fputs("usage: mkdir directory ...\n", stderr);
		exit(1);
	}
	for (err = 0; *++argv;)
		if (mkdir(*argv, 0777) < 0) {
			fputs("mkdir: ", stderr);
			perror(*argv);
			++err;
		}
	exit(err ? 1 : 0);
}
