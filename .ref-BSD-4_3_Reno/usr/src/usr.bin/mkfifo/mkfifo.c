/*
 * Copyright (c) 1990 Regents of the University of California.
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
"@(#) Copyright (c) 1990 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkfifo.c	5.3 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	int ch, exitval, pflag;

	pflag = 0;
	while ((ch = getopt(argc, argv, "p")) != EOF)
		switch(ch) {
		case 'p':
			pflag = 1;
			break;
		case '?':
		default:
			usage();
		}

	if (!*(argv += optind))
		usage();

	for (exitval = 0; *argv; ++argv) {
		if (pflag && build(*argv)) {
			exitval |= 1;
			continue;
		}
		if (mkfifo(*argv, 0777) < 0) {
			(void)fprintf(stderr, "mkfifo: %s: %s\n",
			    *argv, strerror(errno));
			exitval |= 1;
		}
	}
	exit(exitval);
}

build(path)
	char *path;
{
	register char *p;
	struct stat sb;

	for (p = path; *p; p++) {
		if (*p  != '/')
			continue;
		if (stat(path, &sb)) {
			if (errno != ENOENT || mkdir(path, 0777) < 0) {
				(void)fprintf(stderr, "mkdir: %s: %s\n",
				    path, strerror(errno));
				return(1);
			}
		}
		*p = '/';
	}
	return(0);
}

usage()
{
	(void)fprintf(stderr, "usage: mkfifo [-p] fifoname ...\n");
	exit(1);
}
