/*-
 * Copyright (c) 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mount_lfs.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mount.h>

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mntopts.h"
#include "pathnames.h"

struct mntopt mopts[] = {
	MOPT_STDOPTS,
	MOPT_UPDATE,
	{ NULL }
};

void	usage __P((void));
void	invoke_cleaner __P((char *));

int short_rds, cleaner_debug;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct ufs_args args;
	int ch, mntflags, noclean;
	char *fs_name, *options;

	options = NULL;
	mntflags = noclean = 0;
	while ((ch = getopt(argc, argv, "dno:s")) != EOF)
		switch (ch) {
		case 'd':
			cleaner_debug = 1;
			break;
		case 'n':
			noclean = 1;
			break;
		case 'o':
			getmntopts(optarg, mopts, &mntflags);
			break;
		case 's':
			short_rds = 1;
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

#define DEFAULT_ROOTUID	-2
	args.export.ex_root = DEFAULT_ROOTUID;
	if (mntflags & MNT_RDONLY)
		args.export.ex_flags = MNT_EXRDONLY;
	else
		args.export.ex_flags = 0;

	if (mount(MOUNT_LFS, fs_name, mntflags, &args))
		err(1, NULL);

	if (!noclean)
		invoke_cleaner(fs_name);
		/* NOTREACHED */

	exit(0);
}

void
invoke_cleaner(name)
	char *name;
{
	char *args[6], **ap = args;

	/* Build the argument list. */
	*ap++ = _PATH_LFS_CLEANERD;
	if (short_rds)
		*ap++ = "-s";
	if (cleaner_debug)
		*ap++ = "-d";
	*ap++ = name;
	*ap = NULL;

	execv(args[0], args);
	err(1, "exec %s", _PATH_LFS_CLEANERD);
}

void
usage()
{
	(void)fprintf(stderr,
		"usage: mount_lfs [-dns] [-o options] special node\n");
	exit(1);
}
