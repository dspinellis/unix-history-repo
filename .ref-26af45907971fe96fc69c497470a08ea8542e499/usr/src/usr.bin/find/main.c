/*-
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)main.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <fts.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

#include "find.h"

time_t now;			/* time find was run */
int dotfd;			/* starting directory */
int ftsoptions;			/* options for the ftsopen(3) call */
int isdeprecated;		/* using deprecated syntax */
int isdepth;			/* do directories on post-order visit */
int isoutput;			/* user specified output operator */
int isxargs;			/* don't permit xargs delimiting chars */

static void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register char **p, **start;
	int ch;

	(void)time(&now);	/* initialize the time-of-day */

	p = start = argv;
	ftsoptions = FTS_NOSTAT|FTS_PHYSICAL;
	while ((ch = getopt(argc, argv, "Hdf:hXx")) != EOF)
		switch(ch) {
		case 'H':
			ftsoptions |= FTS_COMFOLLOW;
			break;
		case 'd':
			isdepth = 1;
			break;
		case 'f':
			*p++ = optarg;
			break;
		case 'h':
			ftsoptions &= ~FTS_PHYSICAL;
			ftsoptions |= FTS_LOGICAL;
			break;
		case 'X':
			isxargs = 1;
			break;
		case 'x':
			ftsoptions |= FTS_XDEV;
			break;
		case '?':
		default:
			break;
		}

	argc -= optind;	
	argv += optind;

	/* Find first option to delimit the file list. */
	while (*argv) {
		if (option(*argv))
			break;
		*p++ = *argv++;
	}

	if (p == start)
		usage();
	*p = NULL;

	if ((dotfd = open(".", O_RDONLY, 0)) < 0)
		err(1, ".:");

	find_execute(find_formplan(argv), start);
	exit(0);
}

static void
usage()
{
	(void)fprintf(stderr,
	    "usage: find [-HdhXx] [-f file] [file ...] expression\n");
	exit(1);
}









