/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mv.c	5.2 (Berkeley) %G%
 */

int eval;

main(argc, argv)
	int argc;
	char **argv;
{
	if (*++argv && **argv == '-') {
		err("no options available", 0);
		_exit(1);
	}
	if (argc != 3) {
		err("usage: mv file1 file2", 0);
		_exit(1);
	}
	if (rename(argv[0], argv[1])) {
		err(argv[1], 1);
		_exit(1);
	}
	_exit(0);
}

#define	PROGNAME	"mv: "
#include "errfunction"
