/*-
 * Copyright (c) 1990, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Cimarron D. Taylor of the University of California, Berkeley.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1990, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)main.c	8.2 (Berkeley) %G%";
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
	int Hflag, Lflag, Pflag, ch;

	(void)time(&now);	/* initialize the time-of-day */

	p = start = argv;
	Hflag = Lflag = Pflag = 0;
	ftsoptions = FTS_NOSTAT | FTS_PHYSICAL;
	while ((ch = getopt(argc, argv, "HLPXdf:x")) != EOF)
		switch (ch) {
		case 'H':
			Hflag = 1;
			Lflag = Pflag = 0;
			break;
		case 'L':
			Lflag = 1;
			Hflag = Pflag = 0;
			break;
		case 'P':
			Pflag = 1;
			Hflag = Lflag = 0;
			break;
		case 'X':
			isxargs = 1;
			break;
		case 'd':
			isdepth = 1;
			break;
		case 'f':
			*p++ = optarg;
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

	if (Hflag)
		ftsoptions |= FTS_COMFOLLOW;
	if (Lflag) {
		ftsoptions &= ~FTS_PHYSICAL;
		ftsoptions |= FTS_LOGICAL;
	}

	/* Find first option to delimit the file list. */
	for (; *argv != NULL; *p++ = *argv++)
		if (option(*argv))
			break;

	if (p == start)
		usage();
	*p = NULL;

	if ((dotfd = open(".", O_RDONLY, 0)) < 0)
		err(1, ".");

	exit(find_execute(find_formplan(argv), start));
}

static void
usage()
{
	(void)fprintf(stderr,
"usage: find [-H | -L | -P] [-Xdx] [-f file] [file ...] [expression]\n");
	exit(1);
}
