/*-
 * Copyright (c) 1987, 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987, 1992 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)rev.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <err.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register char *filename, *p, *t;
	FILE *fp;
	size_t len;
	int ch, rval;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}

	argc -= optind;
	argv += optind;

	fp = stdin;
	filename = "stdin";
	rval = 0;
	do {
		if (*argv) {
			if ((fp = fopen(*argv, "r")) == NULL) {
				warn("%s", *argv);
				rval = 1;
				++argv;
				continue;
			}
			filename = *argv++;
		}
		while ((p = fgetline(fp, &len)) != NULL) {
			t = p + len - 1;
			for (t = p + len - 1; t >= p; --t)
				putchar(*t);
			putchar('\n');
		}
		if (ferror(fp)) {
			warn("%s", filename);
			rval = 1;
		}
		(void)fclose(fp);
	} while(*argv);
	exit(rval);
}

void
usage()
{
	(void)fprintf(stderr, "usage: rev [file ...]\n");
	exit(1);
}
