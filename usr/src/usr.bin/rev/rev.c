/*-
 * Copyright (c) 1987 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rev.c	4.5 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	register char *t, *bp;
	char buf[BUFSIZ];

	bp = buf;
	do {
		if (argc > 1 && !freopen(*++argv, "r", stdin)) {
			fprintf(stderr, "rev: cannot open %s.\n", *argv);
			exit(1);
		}
		while (fgets(bp, sizeof(buf), stdin)) {
			for (t = bp; *t; ++t);
			if (*--t == '\n')
				--t;
			for (; t >= bp; --t)
				putchar(*t);
			putchar('\n');
		}
	} while(--argc > 1);
	exit(0);
}
