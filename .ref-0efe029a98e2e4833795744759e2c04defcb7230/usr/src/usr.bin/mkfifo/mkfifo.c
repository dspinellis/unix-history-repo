/*
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkfifo.c	5.3 (Berkeley) %G%";
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
