/*
 * Copyright (c) 1983 Regents of the University of California.
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
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdir.c	5.7 (Berkeley) 5/31/90";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <string.h>

extern int errno;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
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

	for (exitval = 0; *argv; ++argv)
		if (pflag)
			exitval |= build(*argv);
		else if (mkdir(*argv, 0777) < 0) {
			(void)fprintf(stderr, "mkdir: %s: %s\n",
			    *argv, strerror(errno));
			exitval = 1;
		}
	exit(exitval);
}

build(path)
	char *path;
{
	register char *p;
	struct stat sb;
	int create, ch;

	for (create = 0, p = path;; ++p)
		if (!*p || *p  == '/') {
			ch = *p;
			*p = '\0';
			if (stat(path, &sb)) {
				if (errno != ENOENT || mkdir(path, 0777) < 0) {
					(void)fprintf(stderr, "mkdir: %s: %s\n",
					    path, strerror(errno));
					return(1);
				}
				create = 1;
			}
			if (!(*p = ch))
				break;
		}
	if (!create) {
		(void)fprintf(stderr, "mkdir: %s: %s\n", path,
		    strerror(EEXIST));
		return(1);
	}
	return(0);
}

usage()
{
	(void)fprintf(stderr, "usage: mkdir [-p] dirname ...\n");
	exit(1);
}
