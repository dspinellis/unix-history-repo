/*
 * Copyright (c) 1992 The Regents of the University of California
 * Copyright (c) 1990, 1992 Jan-Simon Pendry
 * All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mount_lofs.c	5.1 (Berkeley) %G%
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/mount.h>
#include <lofs/lofs.h>

main(c, v)
int c;
char *v[];
{
	extern char *optarg;
	extern int optind;
	int ch;
	int usage = 0;
	int mntflags;
	char *target;
	char *mountpt;
	struct lofs_args args;
	int rc;

	/*
	 * Crack -F option
	 */
	while ((ch = getopt(c, v, "F:")) != EOF)
	switch (ch) {
	case 'F':
		mntflags = atoi(optarg);
		break;
	default:
	case '?':
		usage++;
		break;
	}

	/*
	 * Need two more arguments
	 */
	if (optind != (c - 2))
		usage++;

	if (usage) {
		fputs("usage: mount_lofs [ fsoptions ] target-fs mount-point\n", stderr);
		exit(1);
	}

	/*
	 * Get target and mount point
	 */
	target = v[optind];
	mountpt = v[optind+1];

	args.target = target;

	rc = mount(MOUNT_LOFS, mountpt, mntflags, &args);
	if (rc < 0) {
		perror("mount_lofs");
		exit(1);
	}
	exit(0);
}
