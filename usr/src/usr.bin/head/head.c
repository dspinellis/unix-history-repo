/*
 * Copyright (c) 1980, 1987, 1992 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987, 1992 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)head.c	5.7 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/*
 * head - give the first few lines of a stream or of each of a set of files
 *
 * Bill Joy UCB August 24, 1977
 */

void err __P((int, const char *, ...));
void head __P((FILE *, int));
void obsolete __P((char *[]));
void usage __P((void));

int eval;

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int ch;
	FILE *fp;
	int first, linecnt;
	char *ep;

	obsolete(argv);
	linecnt = 10;
	while ((ch = getopt(argc, argv, "n:")) != EOF)
		switch(ch) {
		case 'n':
			linecnt = strtol(optarg, &ep, 10);
			if (*ep || linecnt <= 0)
				err(1, "illegal line count -- %s", optarg);
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (*argv)
		for (first = 1; *argv; ++argv) {
			if ((fp = fopen(*argv, "r")) == NULL) {
				err(0, "%s: %s", *argv, strerror(errno));
				continue;
			}
			if (argc > 1) {
				(void)printf("%s==> %s <==\n",
				    first ? "" : "\n", *argv);
				first = 0;
			}
			head(fp, linecnt);
			(void)fclose(fp);
		}
	else
		head(stdin, linecnt);
	exit(eval);
}

void
head(fp, cnt)
	FILE *fp;
	register int cnt;
{
	register int ch;

	while (cnt--)
		while ((ch = getc(fp)) != EOF) {
			if (putchar(ch) == EOF)
				err(1, "stdout: %s", strerror(errno));
			if (ch == '\n')
				break;
		}
}

void
obsolete(argv)
	char *argv[];
{
	char *ap;

	while (ap = *++argv) {
		/* Return if "--" or not "-[0-9]*". */
		if (ap[0] != '-' || ap[1] == '-' || !isdigit(ap[1]))
			return;
		if ((ap = malloc(strlen(*argv) + 2)) == NULL)
			err(1, "%s", strerror(errno));
		ap[0] = '-';
		ap[1] = 'n';
		(void)strcpy(ap + 2, *argv + 1);
		*argv = ap;
	}
}

void
usage()
{
	(void)fprintf(stderr, "usage: head [-n lines] [file ...]\n");
	exit(1);
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(int fatal, const char *fmt, ...)
#else
err(fatal, fmt, va_alist)
	int fatal;
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
	(void)fprintf(stderr, "head: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	if (fatal)
		exit(1);
	eval = 1;
}
