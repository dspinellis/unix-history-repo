/*
 * Copyright (c) 1992, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mount_union.c	8.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>
#include <miscfs/union/union.h>

#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct union_args args;
	int ch, mntflags;
	int error = 0;

	mntflags = 0;
	args.mntflags = UNMNT_ABOVE;

	while ((ch = getopt(argc, argv, "F:a:b:r:")) != EOF) {
		switch(ch) {
		case 'F':
			mntflags = atoi(optarg);
			break;
		case 'a':
			if (strcmp(optarg, "bove") == 0) {
				args.mntflags &= ~UNMNT_OPMASK;
				args.mntflags |= UNMNT_ABOVE;
			} else {
				error = 1;
			}
			break;

		case 'b':
			if (strcmp(optarg, "elow") == 0) {
				args.mntflags &= ~UNMNT_OPMASK;
				args.mntflags |= UNMNT_BELOW;
			} else {
				error = 1;
			}
			break;

		case 'r':
			if (strcmp(optarg, "eplace") == 0) {
				args.mntflags &= ~UNMNT_OPMASK;
				args.mntflags |= UNMNT_REPLACE;
			} else {
				error = 1;
			}
			break;

		case '?':
		default:
			error = 1;
			break;
		}
	}
	argc -= optind;
	argv += optind;

	if (argc != 2)
		error = 1;

	if (error)
		usage();

	args.target = argv[0];

	if (mount(MOUNT_UNION, argv[1], mntflags, &args)) {
		(void)fprintf(stderr, "mount_union: %s\n", strerror(errno));
		exit(1);
	}
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: mount_union [ -F fsoptions ] target_fs mount_point\n");
	exit(1);
}
