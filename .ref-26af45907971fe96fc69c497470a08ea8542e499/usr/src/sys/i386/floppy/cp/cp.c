/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)cp.c	5.1 (Berkeley) %G%
 */

#include <fcntl.h>

int eval;

main(argc, argv)
	int argc;
	char **argv;
{
	register int from, to, nr;
	char buf[2048];

	if (*++argv && **argv == '-') {
		err("no options available", 0);
		_exit(1);
	}
	if (argc != 3) {
		err("usage: cp file1 file2", 0);
		_exit(1);
	}
	if ((from = open(argv[0], O_RDONLY, 0)) < 0)
		err(argv[0], 1);
	else if ((to = open(argv[1], O_CREAT|O_TRUNC|O_WRONLY, 0666)) < 0)
		err(argv[1], 1);
	else {
		while ((nr = read(from, buf, sizeof(buf))) > 0)
			if (write(to, buf, nr) != nr) {
				err(argv[1], 1);
				break;
			}
		if (nr == -1)
			err(argv[0], 1);
	}
	_exit(eval);
}

#define	PROGNAME	"cp: "
#include "errfunction"
