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
static char sccsid[] = "@(#)biff.c	5.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage __P((void));
static void err __P((char *));

main(argc, argv)
	int argc;
	char *argv[];
{
	struct stat sb;
	int ch;
	char *name;


	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if ((name = ttyname(STDERR_FILENO)) == NULL) {
		(void)fprintf(stderr, "biff: unknown tty\n");
		exit(2);
	}

	if (stat(name, &sb))
		err(name);

	if (*argv == NULL) {
		(void)printf("is %s\n", sb.st_mode&0100 ? "y" : "n");
		exit(sb.st_mode & 0100 ? 0 : 1);
	}

	switch(argv[0][0]) {
	case 'n':
		if (chmod(name, sb.st_mode & ~0100) < 0)
			err(name);
		break;
	case 'y':
		if (chmod(name, sb.st_mode | 0100) < 0)
			err(name);
		break;
	default:
		usage();
	}
	exit(sb.st_mode & 0100 ? 0 : 1);
}

static void
err(name)
	char *name;
{
	(void)fprintf(stderr, "biff: %s: %s\n", name, strerror(errno));
	exit(2);
}

static void
usage()
{
	(void)fprintf(stderr, "usage: biff [y | n]\n");
	exit(2);
}
