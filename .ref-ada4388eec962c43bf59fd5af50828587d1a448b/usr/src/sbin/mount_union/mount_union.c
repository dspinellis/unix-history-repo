/*
 * Copyright (c) 1992, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software donated to Berkeley by
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
static char sccsid[] = "@(#)mount_union.c	8.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/mount.h>

#include <miscfs/union/union.h>

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "mntopts.h"

struct mntopt mopts[] = {
	MOPT_STDOPTS,
	{ NULL }
};

int	subdir __P((const char *, const char *));
void	usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	struct union_args args;
	int ch, mntflags;
	char target[MAXPATHLEN];

	mntflags = 0;
	args.mntflags = UNMNT_ABOVE;
	while ((ch = getopt(argc, argv, "bo:r")) != EOF)
		switch (ch) {
		case 'b':
			args.mntflags &= ~UNMNT_OPMASK;
			args.mntflags |= UNMNT_BELOW;
			break;
		case 'o':
			getmntopts(optarg, mopts, &mntflags, 0);
			break;
		case 'r':
			args.mntflags &= ~UNMNT_OPMASK;
			args.mntflags |= UNMNT_REPLACE;
			break;
		case '?':
		default:
			usage();
			/* NOTREACHED */
		}
	argc -= optind;
	argv += optind;

	if (argc != 2)
		usage();

	if (realpath(argv[0], target) == 0)
		err(1, "%s", target);

	if (subdir(target, argv[1]) || subdir(argv[1], target))
		errx(1, "%s (%s) and %s are not distinct paths",
		    argv[0], target, argv[1]);

	args.target = target;

	if (mount("union", argv[1], mntflags, &args))
		err(1, NULL);
	exit(0);
}

int
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
		"usage: mount_union [-br] [-o options] target_fs mount_point\n");
	exit(1);
}
