/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cat.c	5.2 (Berkeley) %G%
 */

int eval;

main(argc, argv)
	int argc;
	char **argv;
{
	register int fd, nr;
	char buf[2048];

	if (*++argv && **argv == '-') {
		err("no options available", 0);
		_exit(1);
	}
	for (eval = 0; *argv; ++argv)
		if ((fd = open(*argv, 0, 0)) < 0)
			err(*argv, 1);
		else {
			while ((nr = read(fd, buf, sizeof(buf))) > 0)
				if (write(1, buf, nr) != nr) {
					err(*argv, 1);
					break;
				}
			if (nr == -1)
				err(*argv, 1);
		}
	_exit(eval);
}

#define	PROGNAME	"cat: "
#include "errfunction"
