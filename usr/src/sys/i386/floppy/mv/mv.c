/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mv.c	5.1 (Berkeley) %G%";
#endif /* not lint */

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
