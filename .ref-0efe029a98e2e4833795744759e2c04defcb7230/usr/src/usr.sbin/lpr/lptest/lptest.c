/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)lptest.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * lptest -- line printer test program (and other devices).
 */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int len, count;
	register i, j, fc, nc;
	char outbuf[BUFSIZ];

	setbuf(stdout, outbuf);
	if (argc >= 2)
		len = atoi(argv[1]);
	else
		len = 79;
	if (argc >= 3)
		count = atoi(argv[2]);
	else
		count = 200;
	fc = ' ';
	for (i = 0; i < count; i++) {
		if (++fc == 0177)
			fc = ' ';
		nc = fc;
		for (j = 0; j < len; j++) {
			putchar(nc);
			if (++nc == 0177)
				nc = ' ';
		}
		putchar('\n');
	}
	(void) fflush(stdout);
}
