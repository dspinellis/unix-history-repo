/*-
 * Copyright (c) 1991, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James W. Williams of NASA Goddard Space Flight Center.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1991, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cksum.c	8.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/cdefs.h>
#include <sys/types.h>

#include <err.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "extern.h"

void usage __P((void));

int
main(argc, argv)
	int argc;
	char **argv;
{
	register int ch, fd, rval;
	u_long len, val;
	char *fn, *p;
	int (*cfncn) __P((int, unsigned long *, unsigned long *));
	void (*pfncn) __P((char *, unsigned long, unsigned long));

	if ((p = rindex(argv[0], '/')) == NULL)
		p = argv[0];
	else
		++p;
	if (*p == 'c') {
		cfncn = crc;
		pfncn = pcrc;
	} else {
		cfncn = csum1;
		pfncn = psum1;
	}

	while ((ch = getopt(argc, argv, "o:")) != -1)
		switch (ch) {
		case 'o':
			if (!strcmp(optarg, "1")) {
				cfncn = csum1;
				pfncn = psum1;
			} else if (!strcmp(optarg, "2")) {
				cfncn = csum2;
				pfncn = psum2;
			} else {
				warnx("illegal argument to -o option");
				usage();
			}
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	fd = STDIN_FILENO;
	fn = NULL;
	rval = 0;
	do {
		if (*argv) {
			fn = *argv++;
			if ((fd = open(fn, O_RDONLY, 0)) < 0) {
				warn("%s", fn);
				rval = 1;
				continue;
			}
		}
		if (cfncn(fd, &val, &len)) {
			warn("%s", fn ? fn : "stdin");
			rval = 1;
		} else
			pfncn(fn, val, len);
		(void)close(fd);
	} while (*argv);
	exit(rval);
}

void
usage()
{
	(void)fprintf(stderr, "usage: cksum [-o 1 | 2] [file ...]\n");
	exit(1);
}
