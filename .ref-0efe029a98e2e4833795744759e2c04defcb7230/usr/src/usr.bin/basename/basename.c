/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)basename.c	4.7 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	register char *p, *t;
	char *base;

	if (argc < 2 || argc > 3) {
		fprintf(stderr, "usage: basename string [suffix]\n");
		exit(1);
	}
	for (p = base = *++argv; *p;)
		if (*p++ == '/')
			base = p;
	if (argc == 3) {
		for (t = *++argv; *t; ++t);
		do {
			if (t == *argv) {
				*p = '\0';
				break;
			}
		} while (p >= base && *--t == *--p);
	}
	printf("%s\n", base);
	exit(0);
}
