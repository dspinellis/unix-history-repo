/*
 * Copyright (c) 1990, 1992 Jan-Simon Pendry
 * Copyright (c) 1992 The Regents of the University of California
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mount_fdesc.c	5.1 (Berkeley) %G%
 *
 * $Id: mount_fdesc.c,v 1.1 1992/05/17 17:50:13 jsp Exp jsp $
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/mount.h>

main(c, v)
int c;
char *v[];
{
	extern char *optarg;
	extern int optind;
	int ch;
	int usage = 0;
	int mntflags;
	char *dummy;
	char *mountpt;
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
		fputs("usage: mount_fdesc [ fsoptions ] /dev/fd mount-point\n", stderr);
		exit(1);
	}

	/*
	 * Get target and mount point
	 */
	dummy = v[optind];
	mountpt = v[optind+1];

	rc = mount(MOUNT_FDESC, mountpt, mntflags, (caddr_t) 0);
	if (rc < 0) {
		perror("mount_fdesc");
		exit(1);
	}
	exit(0);
}
