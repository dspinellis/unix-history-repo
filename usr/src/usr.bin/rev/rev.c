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
static char sccsid[] = "@(#)rev.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void usage __P((void));
void warn __P((const char *, ...));

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
				warn("%s: %s", *argv, strerror(errno));
				rval = 1;
				++argv;
				continue;
			}
			filename = *argv++;
		}
		while (p = fgetline(fp, &len)) {
			t = p + len - 1;
			for (t = p + len - 1; t >= p; --t)
				putchar(*t);
			putchar('\n');
		}
		if (ferror(fp)) {
			warn("%s: %s", filename, strerror(errno));
			rval = 1;
		}
		(void)fclose(fp);
	} while(*argv);
	exit(0);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
warn(const char *fmt, ...)
#else
warn(fmt, va_alist)
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "rev: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
}

void
usage()
{
	(void)fprintf(stderr, "usage: rev [file ...]\n");
	exit(1);
}
