/*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)biff.c	5.3 (Berkeley) 6/1/90";
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
