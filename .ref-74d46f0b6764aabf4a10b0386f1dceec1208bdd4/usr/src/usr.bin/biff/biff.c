/*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)biff.c	5.3 (Berkeley) %G%";
#endif /* not lint */

/*
 * biff
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

char	*ttyname();

main(argc, argv)
	int argc;
	char **argv;
{
	char *cp = ttyname(2);
	struct stat stb;

	argc--, argv++;
	if (cp == 0)
		fprintf(stderr, "Where are you?\n"), exit(1);
	if (stat(cp, &stb) < 0)
		perror(cp), exit(1);
	if (argc == 0) {
		printf("is %s\n", stb.st_mode&0100 ? "y" : "n");
		exit((stb.st_mode&0100) ? 0 : 1);
	}
	switch (argv[0][0]) {

	case 'y':
		if (chmod(cp, stb.st_mode|0100) < 0)
			perror(cp);
		break;

	case 'n':
		if (chmod(cp, stb.st_mode&~0100) < 0)
			perror(cp);
		break;

	default:
		fprintf(stderr, "usage: biff [y] [n]\n");
	}
	exit((stb.st_mode&0100) ? 0 : 1);
}
