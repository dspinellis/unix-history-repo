/*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1980, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)biff.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static void usage __P((void));

int
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

	if ((name = ttyname(STDERR_FILENO)) == NULL)
		err(2, "tty");

	if (stat(name, &sb))
		err(2, "stat");

	if (*argv == NULL) {
		(void)printf("is %s\n", sb.st_mode&0100 ? "y" : "n");
		exit(sb.st_mode & 0100 ? 0 : 1);
	}

	switch(argv[0][0]) {
	case 'n':
		if (chmod(name, sb.st_mode & ~0100) < 0)
			err(2, name);
		break;
	case 'y':
		if (chmod(name, sb.st_mode | 0100) < 0)
			err(2, name);
		break;
	default:
		usage();
	}
	exit(sb.st_mode & 0100 ? 0 : 1);
}

static void
usage()
{
	(void)fprintf(stderr, "usage: biff [y | n]\n");
	exit(2);
}
