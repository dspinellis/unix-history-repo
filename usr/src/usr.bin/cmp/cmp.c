/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987, 1990 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)cmp.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extern.h"

int	lflag, sflag;

static void usage __P((void));

main(argc, argv)
	int argc;
	char *argv[];
{
	struct stat sb1, sb2;
	off_t skip1, skip2;
	int ch, fd1, fd2, special;
	char *file1, *file2;

	while ((ch = getopt(argc, argv, "-ls")) != EOF)
		switch (ch) {
		case 'l':		/* print all differences */
			lflag = 1;
			break;
		case 's':		/* silent run */
			sflag = 1;
			break;
		case '-':		/* stdin (must be after options) */
			--optind;
			goto endargs;
		case '?':
		default:
			usage();
		}
endargs:
	argv += optind;
	argc -= optind;

	if (lflag && sflag)
		err("only one of -l and -s may be specified");

	if (argc < 2 || argc > 4)
		usage();

	/* Backward compatibility -- handle "-" meaning stdin. */
	special = 0;
	if (strcmp(file1 = argv[0], "-") == 0) {
		special = 1;
		fd1 = 0;
		file1 = "stdin";
	}
	else if ((fd1 = open(file1, O_RDONLY, 0)) < 0)
		err("%s: %s", file1, strerror(errno));
	if (strcmp(file2 = argv[1], "-") == 0) {
		if (special)
			err("standard input may only be specified once");
		special = 1;
		fd2 = 0;
		file2 = "stdin";
	}
	else if ((fd2 = open(file2, O_RDONLY, 0)) < 0)
		err("%s: %s", file2, strerror(errno));

	skip1 = argc > 2 ? strtol(argv[2], NULL, 10) : 0;
	skip2 = argc == 4 ? strtol(argv[3], NULL, 10) : 0;

	if (!special) {
		if (fstat(fd1, &sb1))
			err("%s: %s", file1, strerror(errno));
		if (!S_ISREG(sb1.st_mode))
			special = 1;
		else {
			if (fstat(fd2, &sb2))
				err("%s: %s", file2, strerror(errno));
			if (!S_ISREG(sb2.st_mode))
				special = 1;
		}
	}

	if (special)
		c_special(fd1, file1, skip1, fd2, file2, skip2);
	else
		c_regular(fd1, file1, skip1, sb1.st_size,
		    fd2, file2, skip2, sb2.st_size);
	exit(0);
}

static void
usage()
{
	(void)fprintf(stderr,
	    "usage: cmp [-l | s] file1 file2 [skip1 [skip2]]\n");
	exit(2);
}
