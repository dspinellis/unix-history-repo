/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mknod.c	5.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/stat.h>

int eval;

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p;
	register int major, minor;
	unsigned short mode;

	if (*++argv && **argv == '-') {
		err("no options available", 0);
		_exit(1);
	}
	if (argc != 5) {
usage:		err("usage: mknod name [b|c] major minor", 0);
		_exit(1);
	}
		
	mode = 0666;
	if (argv[1][0] == 'c')
		mode |= S_IFCHR;
	else if (argv[1][0] == 'b')
		mode |= S_IFBLK;
	else
		goto usage;

	for (major = 0, p = argv[2]; *p; ++p)
                major = major * 10 + *p - '0';
	for (minor = 0, p = argv[3]; *p; ++p)
                minor = minor * 10 + *p - '0';

	if (mknod(argv[0], mode, makedev(major, minor)) < 0) {
		err(argv[0], 1);
		_exit(1);
	}
	_exit(0);
}

#define	PROGNAME	"mknod: "
#include "errfunction"
