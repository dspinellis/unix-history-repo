/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)mount.c	5.1 (Berkeley) %G%
 */

#include <sys/param.h>
#include <sys/mount.h>

#define	DEFAULT_ROOTUID	-2

int eval;

main(argc, argv)
	int argc;
	char **argv;
{
	struct ufs_args args;

	if (*++argv && **argv == '-') {
		err("no options available", 0);
		_exit(1);
	}
	if (argc != 3) {
		err("usage: mount dev dir", 0);
		_exit(1);
	}
	args.fspec = argv[0];
	args.exflags = 0;
	args.exroot = DEFAULT_ROOTUID;;
		
	if (mount(MOUNT_UFS, argv[1], 0, &args)) {
		err(argv[0], 1);
		_exit(1);
	}
	_exit(0);
}

#define	PROGNAME	"mount: "
#include "errfunction"
