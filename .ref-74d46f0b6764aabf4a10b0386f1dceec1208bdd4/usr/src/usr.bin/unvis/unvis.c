/*-
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)unvis.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <vis.h>

char	*Program;
#define usage()	fprintf(stderr, "usage: %s %s\n", Program, USAGE)
#define USAGE "[file...]"

main(argc, argv)
	char *argv[];
{
	FILE *fp;
	extern char *optarg;
	extern int optind;
	int ch;

	Program = argv[0];
	while ((ch = getopt(argc, argv, "")) != EOF)
		switch((char)ch) {
		case '?':
		default:
			usage();
			exit(1);
		}
	argc -= optind;
	argv += optind;

	if (*argv)
		while (*argv) {
			if ((fp=fopen(*argv, "r")) != NULL)
				process(fp, *argv);
			else
				syserror("%s", *argv);
			argv++;
		}
	else
		process(stdin, "<stdin>");
	exit(0);
}

process(fp, filename)
	FILE *fp;
	char *filename;
{
	register int offset = 0, c, ret;
	int state = 0;
	char outc;

	while ((c = getc(fp)) != EOF) {
		offset++;
	again:
		switch(ret = unvis(&outc, (char)c, &state, 0)) {
		case UNVIS_VALID:
			putchar(outc);
			break;
		case UNVIS_VALIDPUSH:
			putchar(outc);
			goto again;
		case UNVIS_SYNBAD:
			error("%s: offset: %d: can't decode", filename, offset);
			state = 0;
			break;
		case 0:
		case UNVIS_NOCHAR:
			break;
		default:
			error("bad return value (%d), can't happen", ret);
			exit(1);
		}
	}
	if (unvis(&outc, (char)0, &state, UNVIS_END) == UNVIS_VALID)
		putchar(outc);
}

#include <varargs.h>

error(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;
	extern errno;

	fprintf(stderr, "%s: ", Program);
	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, "\n");
}

syserror(va_alist)
	va_dcl
{
	char *fmt;
	va_list ap;
	extern errno;

	fprintf(stderr, "%s: ", Program);
	va_start(ap);
	fmt = va_arg(ap, char *);
	(void) vfprintf(stderr, fmt, ap);
	va_end(ap);
	fprintf(stderr, ": %s\n", strerror(errno));
}
