/*-
 * Copyright (c) 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mount_lfs.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mount.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "pathnames.h"

#define DEFAULT_ROOTUID -2	/* copied from mount's UFS code */

void usage __P((void));
void invoke_cleaner __P((char *));

int short_rds, cleaner_debug;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct ufs_args args;
	int ch, mntflags;
	int noclean;
	char *fs_name;

	mntflags = noclean = 0;
	while ((ch = getopt(argc, argv, "F:nsd")) != EOF)
		switch(ch) {
		case 'F':
			mntflags = atoi(optarg);
			break;
		case 'n':
			noclean = 1;
			break;
		case 's':
			short_rds = 1;
			break;
		case 'd':
			cleaner_debug = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 2)
		usage();

        args.fspec = argv[0];	/* the name of the device file */
	fs_name = argv[1];	/* the mount point */

	/* copied from mount's UFS code */
	args.export.ex_root = DEFAULT_ROOTUID;
	if (mntflags & MNT_RDONLY)
		args.export.ex_flags = MNT_EXRDONLY;
	else
		args.export.ex_flags = 0;

	if (mount(MOUNT_LFS, fs_name, mntflags, &args)) {
		(void)fprintf(stderr, "mount_lfs: %s\n", strerror(errno));
		exit(1);
	}

	if (!noclean) {
		/*
		 * invoke the lfs_cleanerd for this filesystem as its arg!
		 */
		invoke_cleaner(fs_name);
		/* never returns */
	}

	exit(0);
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: mount_lfs [ -nsd ] [ -F fsoptions ] device mount_point\n");
	exit(1);
}

void
invoke_cleaner(name)
	char *name;
{
	char *args[6], **ap = args;

	/* build the argument list */
	*ap++ = _PATH_LFS_CLEANERD;
	if (short_rds)
		*ap++ = "-s";
	if (cleaner_debug)
		*ap++ = "-d";
	*ap++ = name;
	*ap = NULL;

	execv(args[0], args);
	err(1, "exec of %x failed", _PATH_LFS_CLEANERD);
}
