/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdir.c	5.7 (Berkeley) %G%";
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
