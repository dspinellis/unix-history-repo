/*
 * Copyright (c) 1980, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1980, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)swapon.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <fstab.h>
#include <errno.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	register struct fstab *fsp;
	register int stat;
	int ch, doall;

	doall = 0;
	while ((ch = getopt(argc, argv, "a")) != EOF)
		switch((char)ch) {
		case 'a':
			doall = 1;
			break;
		case '?':
		default:
			usage();
		}
	argv += optind;

	stat = 0;
	if (doall)
		while (fsp = getfsent()) {
			if (strcmp(fsp->fs_type, FSTAB_SW))
				continue;
			if (add(fsp->fs_spec, 1))
				stat = 1;
			else
				printf("swapon: adding %s as swap device\n",
				    fsp->fs_spec);
		}
	else if (!*argv)
		usage();
	for (; *argv; ++argv)
		stat |= add(*argv, 0);
	exit(stat);
}

add(name, ignoreebusy)
	char *name;
	int ignoreebusy;
{
	extern int errno;

	if (swapon(name) == -1) {
		switch (errno) {
		case EINVAL:
			fprintf(stderr, "swapon: %s: device not configured\n",
			    name);
			break;
		case EBUSY:
			if (!ignoreebusy)
				fprintf(stderr,
				    "swapon: %s: device already in use\n",
				     name);
			break;
		default:
			fprintf(stderr, "swapon: %s: ", name);
			perror((char *)NULL);
			break;
		}
		return(1);
	}
	return(0);
}

usage()
{
	fprintf(stderr, "usage: swapon [-a] [special_file ...]\n");
	exit(1);
}
