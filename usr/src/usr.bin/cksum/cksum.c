/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * James W. Williams of the University of Maryland.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cksum.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	u_long len, val;
	register int ch, fd, rval;
	char *fn;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	fd = STDIN_FILENO;
	fn = "stdin";
	rval = 0;
	do {
		if (*argv) {
			fn = *argv++;
			if ((fd = open(fn, O_RDONLY, 0)) < 0) {
				(void)fprintf(stderr,
				    "cksum: %s: %s\n", fn, strerror(errno));
				rval = 1;
				continue;
			}
		}
		if (crc(fd, &val, &len)) {
			(void)fprintf(stderr,
			    "cksum: %s: %s\n", fn, strerror(errno));
			rval = 1;
		} else
			(void)printf("%lu %lu %s\n", val, len, fn);
		(void)close(fd);
	} while (*argv);
	exit(rval);
}

usage()
{
	(void)fprintf(stderr, "usage: cksum [file ...]\n");
	exit(1);
}
