/*
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mount_null.c	8.4 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>
#include <miscfs/nullfs/null.h>

#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

static int subdir __P((const char *, const char *));
void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct null_args args;
	int ch, mntflags;
	char target[MAXPATHLEN];

	mntflags = 0;
	while ((ch = getopt(argc, argv, "F:")) != EOF)
		switch(ch) {
		case 'F':
			mntflags = atoi(optarg);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 2)
		usage();

	if (realpath(argv[0], target) == 0) {
		(void)fprintf(stderr, "mount_null: %s: %s\n",
				target, strerror(errno));
		exit(1);
	}

	if (subdir(target, argv[1]) || subdir(argv[1], target)) {
		(void)fprintf(stderr,
			"mount_null: %s (%s) and %s are not distinct paths\n",
				argv[0], target, argv[1]);
		exit(1);
	}

	args.target = target;

	if (mount(MOUNT_NULL, argv[1], mntflags, &args)) {
		(void)fprintf(stderr, "mount_null: %s\n", strerror(errno));
		exit(1);
	}
	exit(0);
}

static int
subdir(p, dir)
	const char *p;
	const char *dir;
{
	int l;

	l = strlen(dir);
	if (l <= 1)
		return (1);

	if ((strncmp(p, dir, l) == 0) && (p[l] == '/' || p[l] == '\0'))
		return (1);

	return (0);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: mount_null [ -F fsoptions ] target_fs mount_point\n");
	exit(1);
}
