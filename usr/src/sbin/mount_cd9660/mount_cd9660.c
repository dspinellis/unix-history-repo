/*
 * Copyright (c) 1992, 1993, 1994
 *      The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Pace Willisson (pace@blitz.com).  The Rock Ridge Extension
 * Support code is derived from software contributed to Berkeley
 * by Atsushi Murai (amurai@spec.co.jp).
 *
 * %sccs.include.redist.c%
 *
 *      @(#)mount_cd9660.c	8.4 (Berkeley) %G%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1992, 1993, 1994\n\
        The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mount_cd9660.c	8.4 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#define CD9660
#include <sys/mount.h>

#include <err.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "mntopts.h"

struct mntopt mopts[] = {
	MOPT_STDOPTS,
	MOPT_UPDATE,
	{ NULL }
};

void	usage __P((void));

int
main(argc, argv)
	int argc;
	char **argv;
{
	struct iso_args args;
	int ch, mntflags, opts;
	char *dev, *dir, *options;

	options = NULL;
	mntflags = opts = 0;
	while ((ch = getopt(argc, argv, "ego:r")) != EOF)
		switch (ch) {
		case 'e':
			opts |= ISOFSMNT_EXTATT;
			break;
		case 'g':
			opts |= ISOFSMNT_GENS;
			break;
		case 'o':
			getmntopts(options, mopts, &mntflags);
			break;
		case 'r':
			opts |= ISOFSMNT_NORRIP;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 2)
		usage();

	dev = argv[0];
	dir = argv[1];

#define DEFAULT_ROOTUID	-2
	args.fspec = dev;
	args.export.ex_root = DEFAULT_ROOTUID;

	if (mntflags & MNT_RDONLY)
		args.export.ex_flags = MNT_EXRDONLY;
	else
		args.export.ex_flags = 0;
	args.flags = opts;

	if (mount(MOUNT_CD9660, dir, mntflags, &args) < 0)
		err(1, NULL);
	exit(0);
}

void
usage()
{
	(void)fprintf(stderr,
		"usage: mount_cd9660 [-egrt] [-o options] special node\n");
	exit(1);
}
