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
static char sccsid[] = "@(#)mtree.c	5.10 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <fts.h>
#include "mtree.h"
#include "extern.h"

extern int crc_total;

int ftsoptions = FTS_PHYSICAL;
int cflag, dflag, eflag, rflag, sflag, uflag;
u_short keys;
char fullpath[MAXPATHLEN];

static void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern int optind;
	extern char *optarg;
	int ch;
	char *dir, *p;

	dir = NULL;
	keys = KEYDEFAULT;
	while ((ch = getopt(argc, argv, "cdef:K:k:p:rs:ux")) != EOF)
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
			if (!(freopen(optarg, "r", stdin)))
				err("%s: %s", optarg, strerror(errno));
			break;
		case 'K':
			while ((p = strsep(&optarg, " \t,")) != NULL)
				if (*p != '\0')
					keys |= parsekey(p, NULL);
			break;
		case 'k':
			keys = F_TYPE;
			while ((p = strsep(&optarg, " \t,")) != NULL)
				if (*p != '\0')
					keys |= parsekey(p, NULL);
			break;
		case 'p':
			dir = optarg;
			break;
		case 'r':
			rflag = 1;
			break;
		case 's':
			sflag = 1;
			crc_total = ~strtol(optarg, &p, 0);
			if (*p)
				err("illegal seed value -- %s", optarg);
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
	argv += optind;

	if (argc)
		usage();

	if (dir && chdir(dir))
		err("%s: %s", dir, strerror(errno));

	if ((cflag || sflag) && !getwd(fullpath))
		err("%s", fullpath);

	if (cflag) {
		cwalk();
		exit(0);
	}
	exit(verify());
}

static void
usage()
{
	(void)fprintf(stderr,
"usage: mtree [-cderux] [-f spec] [-K key] [-k key] [-p path] [-s seed]\n");
	exit(1);
}
