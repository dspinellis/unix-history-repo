/*
 * Copyright (c) 1990, 1992 Jan-Simon Pendry
 * Copyright (c) 1992, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Jan-Simon Pendry.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1992, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mount_fdesc.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mount.h>

#include <err.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "mntopts.h"

struct mntopt mopts[] = {
	MOPT_STDOPTS,
	{ NULL }
};

void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch, mntflags;

	mntflags = 0;
	while ((ch = getopt(argc, argv, "o:")) != EOF)
		switch (ch) {
		case 'o':
			getmntopts(optarg, mopts, &mntflags, 0);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 2)
		usage();

	if (mount("fdesc", argv[1], mntflags, NULL))
		err(1, NULL);
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr,
		"usage: mount_fdesc [-o options] fdesc mount_point\n");
	exit(1);
}
