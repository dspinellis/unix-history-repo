/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)swapon.c	5.3 (Berkeley) 6/30/88";
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

static
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

static
usage()
{
	fprintf(stderr, "usage: swapon [-a] [special_file ...]\n");
	exit(1);
}
