/*-
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Edward Sze-Tyan Wang.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1991 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tail.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extern.h"

int fflag, rflag, rval;
char *fname;

static void obsolete __P((char **));
static void usage __P((void));

main(argc, argv)
	int argc;
	char **argv;
{
	struct stat sb;
	FILE *fp;
	long off;
	enum STYLE style;
	int ch;
	char *p, *num;

	obsolete(argv);

	style = NOTSET;
	while ((ch = getopt(argc, argv, "b:c:fn:r")) != EOF)
		switch(ch) {
		case 'b':
			if (style)
				usage();
			off = strtol(num = optarg, &p, 10) * 512;
			if (*p)
				err("illegal offset -- %s", optarg);
			style = *num == '+' ? FBYTES : RBYTES;
			break;
		case 'c':
			if (style)
				usage();
			off = strtol(num = optarg, &p, 10);
			if (*p)
				err("illegal offset -- %s", optarg);
			style = *num == '+' ? FBYTES : RBYTES;
			break;
		case 'f':
			fflag = 1;
			break;
		case 'n':
			if (style)
				usage();
			off = strtol(num = optarg, &p, 10);
			if (*p)
				err("illegal offset -- %s", optarg);
			style = *num == '+' ? FLINES : RLINES;
			break;
		case 'r':
			rflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	/*
	 * Don't permit follow option if displaying in reverse.  An offset
	 * with an explicit leading minus is meaningless.
	 */
	if (rflag) {
		if (fflag)
			usage();
		if (style && *num == '-')
			err("illegal offset for -r option -- %s", num);
		if (style == FBYTES)
			style = RBYTES;
		if (style == FLINES)
			style = RLINES;
	}

	if (fname = *argv) {
		if ((fp = fopen(fname, "r")) == NULL)
			ierr();
	} else {
		fp = stdin;
		fname = "stdin";
	}

	if (fstat(fileno(fp), &sb))
		ierr();

	/*
	 * Determine if input is a pipe.  4.4BSD will set the SOCKET
	 * bit in the st_mode field for pipes.  Fix this then.
	 */
	if (lseek(fileno(fp), 0L, SEEK_CUR) == -1 && errno == ESPIPE) {
		errno = 0;
		fflag = 0;		/* POSIX.2 requires this. */
	}

	/*
	 * Tail's options are weird.  First, -n10 is the same as -n-10, not
	 * -n+10.  Second, the number options for the -r option specify the
	 * number of bytes/chars/lines that get displayed, not the offset from
	 * the beginning/end of the file.  Finally, the default for -r is the
	 * entire file, not 10 lines.
	 */
	if (!style)
		if (rflag) {
			off = 0;
			style = REVERSE;
		} else {
			off = 10;
			style = RLINES;
		}
	else if (off < 0)
		off = -off;

	if (rflag)
		reverse(fp, style, off, &sb);
	else
		forward(fp, style, off, &sb);
	exit(rval);
}

/*
 * Convert the obsolete argument form into something that getopt can handle.
 * This means that anything of the form [+-][0-9][0-9]*[lbc][fr] that isn't
 * the option argument for a -b, -c or -n option gets converted.
 */
static void
obsolete(argv)
	char **argv;
{
	register char *ap, *p, *t;
	int len;
	char *start;

	while (ap = *++argv) {
		/* Return if "--" or not an option of any form. */
		if (ap[0] != '-') {
			if (ap[0] != '+')
				return;
		} else if (ap[1] == '-')
			return;

		switch(*++ap) {
		/* Old-style option. */
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':

			/* Malloc space for dash, new option and argument. */
			len = strlen(*argv);
			if ((start = p = malloc(len + 3)) == NULL)
				err("%s", strerror(errno));
			*p++ = '-';

			/*
			 * Go to the end of the option argument.  Save off any
			 * trailing options (-3lf) and translate any trailing
			 * output style characters.
			 */
			t = *argv + len - 1;
			if (*t == 'f' || *t == 'r')
				*p++ = *t--;
			switch(*t) {
			case 'b':
				*p++ = 'b';
				*t = '\0';
				break;
			case 'c':
				*p++ = 'c';
				*t = '\0';
				break;
			case 'l':
				*t = '\0';
				/* FALLTHROUGH */
			case '0': case '1': case '2': case '3': case '4':
			case '5': case '6': case '7': case '8': case '9':
				*p++ = 'n';
				break;
			default:
				err("illegal option -- %s", *argv);
			}
			*p++ = *argv[0];
			(void)strcpy(p, ap);
			*argv = start;
			continue;

		/*
		 * Options w/ arguments, skip the argument and continue
		 * with the next option.
		 */
		case 'b':
		case 'c':
		case 'n':
			if (!ap[1])
				++argv;
			/* FALLTHROUGH */
		/* Options w/o arguments, continue with the next option. */
		case 'f':
		case 'r':
			continue;

		/* Illegal option, return and let getopt handle it. */
		default:
			return;
		}
	}
}

static void
usage()
{
	(void)fprintf(stderr,
	    "usage: tail [-f | -r] [-b # | -c # | -n #] [file]\n");
	exit(1);
}
