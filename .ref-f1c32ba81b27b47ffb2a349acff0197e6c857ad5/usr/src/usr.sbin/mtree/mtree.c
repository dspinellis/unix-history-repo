/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mtree.c	5.8 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdio.h>
#include <fts.h>
#include "mtree.h"

NODE *root;
int exitval;
int cflag, dflag, eflag, rflag, uflag;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int ftsoptions, optind;
	extern char *optarg;
	int ch;
	char *dir;

	dir = (char *)NULL;
	while ((ch = getopt(argc, argv, "cdef:p:rux")) != EOF)
		switch((char)ch) {
		case 'c':
			cflag = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 'e':
			eflag = 1;
			break;
		case 'f':
			if (!(freopen(optarg, "r", stdin))) {
				(void)fprintf(stderr,
				    "mtree: can't read %s.\n", optarg);
				exit(1);
			}
			break;
		case 'p':
			dir = optarg;
			break;
		case 'r':
			rflag = 1;
			break;
		case 'u':
			uflag = 1;
			break;
		case 'x':
			ftsoptions |= FTS_XDEV;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	if (argc)
		usage();

	if (!cflag)
		spec();

	if (dir && chdir(dir)) {
		(void)fprintf(stderr,
		    "mtree: %s: %s\n", dir, strerror(errno));
		exit(1);
	}

	if (cflag)
		cwalk();
	else
		verify();
	exit(exitval);
}

usage()
{
	(void)fprintf(stderr,
	    "usage: mtree [-cderux] [-p path] [-f spec]\n");
	exit(1);
}
