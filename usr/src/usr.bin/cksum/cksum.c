/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James W. Williams of NASA Goddard Space Flight Center.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cksum.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/cdefs.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "extern.h"

void usage __P((void));

int
main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	u_long len, val;
	register int ch, fd, rval;
	char *fn;
	int (*cfncn) __P((int, unsigned long *, unsigned long *));
	void (*pfncn) __P((char *, unsigned long, unsigned long));

	cfncn = crc;
	pfncn = pcrc;
	while ((ch = getopt(argc, argv, "o:")) != EOF)
		switch(ch) {
		case 'o':
			if (*optarg == '1') {
				cfncn = csum1;
				pfncn = psum1;
			} else if (*optarg == '2') {
				cfncn = csum2;
				pfncn = psum2;
			} else {
				(void)fprintf(stderr,
				    "cksum: illegal argument to -o option\n");
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
				(void)fprintf(stderr, "cksum: %s: %s\n",
				    fn, strerror(errno));
				rval = 1;
				continue;
			}
		}
		if (cfncn(fd, &val, &len)) {
			(void)fprintf(stderr, "cksum: %s: %s\n",
			    fn ? fn : "stdin", strerror(errno));
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
