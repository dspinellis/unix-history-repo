/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mkdir.c	5.1 (Berkeley) %G%
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
	for (eval = 0; *argv; ++argv)
		if (mkdir(*argv, 0777) < 0)
			err(*argv, 1);
	_exit(eval);
}

#define	PROGNAME	"mkdir: "
#include "errfunction"
