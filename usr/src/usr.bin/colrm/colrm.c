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
static char sccsid[] = "@(#)colrm.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define	TAB	8

void err __P((const char *, ...));
void check __P((FILE *));
void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register u_long column, start, stop;
	register int ch;
	char *p;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	start = stop = 0;
	switch(argc) {
	case 2:
		stop = strtol(argv[1], &p, 10);
		if (stop <= 0 || *p)
			err("illegal column -- %s", argv[1]);
		/* FALLTHROUGH */
	case 1:
		start = strtol(argv[0], &p, 10);
		if (start <= 0 || *p)
			err("illegal column -- %s", argv[0]);
		break;
	case 0:
		break;
	default:
		usage();
	}

	if (stop && start > stop)
		err("illegal start and stop columns");

	for (column = 0;;) {
		switch (ch = getchar()) {
		case EOF:
			check(stdin);
			break;
		case '\b':
			if (column)
				--column;
			break;
		case '\n':
			column = 0;
			break;
		case '\t':
			column = (column + TAB) & ~(TAB - 1);
			break;
		default:
			++column;
			break;
		}

		if ((!start || column < start || stop && column > stop) &&
		    putchar(ch) == EOF)
			check(stdout);
	}
}

void
check(stream)
	FILE *stream;
{
	if (feof(stream))
		exit(0);
	if (ferror(stream))
		err("%s: %s",
		    stream == stdin ? "stdin" : "stdout", strerror(errno));
}

void
usage()
{
	(void)fprintf(stderr, "usage: colrm [start [stop]]\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(const char *fmt, ...)
#else
err(fmt, va_alist)
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
	(void)fprintf(stderr, "colrm: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
