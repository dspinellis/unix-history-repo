/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rev.c	4.4 (Berkeley) %G%";
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
