/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Case Larsen.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)uniq.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#define	MAXLINELEN	(8 * 1024)

int cflag, dflag, uflag;
int numchars, numfields, repeats;

void	 err __P((const char *, ...));
FILE	*file __P((char *, char *));
void	 show __P((FILE *, char *));
char	*skip __P((char *));
void	 obsolete __P((char *[]));
void	 usage __P((void));

int
main (argc, argv)
	int argc;
	char *argv[];
{
	register char *t1, *t2;
	FILE *ifp, *ofp;
	int ch;
	char *prevline, *thisline, *p;

	obsolete(argv);
	while ((ch = getopt(argc, argv, "-cdf:s:u")) != EOF)
		switch (ch) {
		case '-':
			--optind;
			goto done;
		case 'c':
			cflag = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 'f':
			numfields = strtol(optarg, &p, 10);
			if (numfields < 0 || *p)
				err("illegal field skip value: %s", optarg);
			break;
		case 's':
			numchars = strtol(optarg, &p, 10);
			if (numchars < 0 || *p)
				err("illegal character skip value: %s", optarg);
			break;
		case 'u':
			uflag = 1;
			break;
		case '?':
		default:
			usage();
	}

done:	argc -= optind;
	argv +=optind;

	/* If no flags are set, default is -d -u. */
	if (cflag) {
		if (dflag || uflag)
			usage();
	} else if (!dflag && !uflag)
		dflag = uflag = 1;

	switch(argc) {
	case 0:
		ifp = stdin;
		ofp = stdout;
		break;
	case 1:
		ifp = file(argv[0], "r");
		ofp = stdout;
		break;
	case 2:
		ifp = file(argv[0], "r");
		ofp = file(argv[1], "w");
		break;
	default:
		usage();
	}

	prevline = malloc(MAXLINELEN);
	thisline = malloc(MAXLINELEN);
	if (prevline == NULL || thisline == NULL)
		err("%s", strerror(errno));

	if (fgets(prevline, MAXLINELEN, ifp) == NULL)
		exit(0);

	while (fgets(thisline, MAXLINELEN, ifp)) {
		/* If requested get the chosen fields + character offsets. */
		if (numfields || numchars) {
			t1 = skip(thisline);
			t2 = skip(prevline);
		} else {
			t1 = thisline;
			t2 = prevline;
		}

		/* If different, print; set previous to new value. */
		if (strcmp(t1, t2)) {
			show(ofp, prevline);
			t1 = prevline;
			prevline = thisline;
			thisline = t1;
			repeats = 0;
		} else
			++repeats;
	}
	show(ofp, prevline);
	exit(0);
}

/*
 * show --
 *	Output a line depending on the flags and number of repetitions
 *	of the line.
 */
void
show(ofp, str)
	FILE *ofp;
	char *str;
{
	if (cflag)
		(void)fprintf(ofp, "%4d %s", repeats + 1, str);
	if (dflag && repeats || uflag && !repeats)
		(void)fprintf(ofp, "%s", str);
}

char *
skip(str)
	register char *str;
{
	register int infield, nchars, nfields;

	for (nfields = numfields, infield = 0; nfields && *str; ++str)
		if (isspace(*str)) {
			if (infield) {
				infield = 0;
				--nfields;
			}
		} else if (!infield)
			infield = 1;
	for (nchars = numchars; nchars-- && *str; ++str);
	return(str);
}

FILE *
file(name, mode)
	char *name, *mode;
{
	FILE *fp;

	if ((fp = fopen(name, mode)) == NULL)
		err("%s: %s", name, strerror(errno));
	return(fp);
}

void
obsolete(argv)
	char *argv[];
{
	int len;
	char *ap, *p, *start;

	while (ap = *++argv) {
		/* Return if "--" or not an option of any form. */
		if (ap[0] != '-') {
			if (ap[0] != '+')
				return;
		} else if (ap[1] == '-')
			return;
		if (!isdigit(ap[1]))
			continue;
		/*
		 * Digit signifies an old-style option.  Malloc space for dash,
		 * new option and argument.
		 */
		len = strlen(ap);
		if ((start = p = malloc(len + 3)) == NULL)
			err("%s", strerror(errno));
		*p++ = '-';
		*p++ = ap[0] == '+' ? 's' : 'f';
		(void)strcpy(p, ap + 1);
		*argv = start;
	}
}

void
usage()
{
	(void)fprintf(stderr,
	    "usage: uniq [-c | -du] [-f fields] [-s chars] [input [output]]\n");
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
	(void)fprintf(stderr, "uniq: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
