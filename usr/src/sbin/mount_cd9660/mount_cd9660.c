/*
 * Copyright (c) 1992, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley
 * by Pace Willisson (pace@blitz.com).  The Rock Ridge Extension
 * Support code is derived from software contributed to Berkeley
 * by Atsushi Murai (amurai@spec.co.jp).
 *
 * %sccs.include.redist.c%
 *
 *      @(#)mount_cd9660.c	8.3 (Berkeley) %G%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1992, 1993\n\
        The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mount_cd9660.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#define CD9660
#include <sys/mount.h>

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define DEFAULT_ROOTUID	-2

void
usage()
{

	fprintf(stderr,
		"usage: mount_cd9660 [-F fsoptions] [-norrip] [-gen] [-extattr] special node\n");
	exit(1);
}
		
int
main(argc, argv)
	int argc;
	char **argv;
{
	char *dev;
	char *dir;
	struct iso_args args;
	int mntflags = 0, opts = 0;

	argc--;
	argv++;
	while (argc > 2) {
		if (!strcmp("-F", argv[0])) {
			argc--; argv++;
			mntflags |= atoi(argv[0]);
			argc--; argv++;
		} else if (!strcmp(argv[0], "-norrip")) {
			opts |= ISOFSMNT_NORRIP;
			argc--; argv++;
		} else if (!strcmp(argv[0], "-gen")) {
			opts |= ISOFSMNT_GENS;
			argc--; argv++;
		} else if (!strcmp(argv[0], "-extattr")) {
			opts |= ISOFSMNT_EXTATT;
			argc--; argv++;
		} else if (!strcmp(argv[0], "-notrans")) {
			opts |= ISOFSMNT_NOTRANS;
			argc--; argv++;
		} else
			usage();
	}

	dev = argv[0];
	dir = argv[1];

	args.fspec = dev;
	args.export.ex_root = DEFAULT_ROOTUID;
	if (mntflags & MNT_RDONLY)
		args.export.ex_flags = MNT_EXRDONLY;
	else
		args.export.ex_flags = 0;
	args.flags = opts;

	if (mount(MOUNT_CD9660, dir, mntflags, &args) < 0) {
		(void)fprintf(stderr, "mount_cd9660: %s\n", strerror(errno));
		exit(1);
	}

	exit(0);
}
