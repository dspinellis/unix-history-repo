/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tr.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "extern.h"

static int string1[NCHARS] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,		/* ASCII */
	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
	0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
	0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27,
	0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f,
	0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
	0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f,
	0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47,
	0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e, 0x4f,
	0x50, 0x51, 0x52, 0x53, 0x54, 0x55, 0x56, 0x57,
	0x58, 0x59, 0x5a, 0x5b, 0x5c, 0x5d, 0x5e, 0x5f,
	0x60, 0x61, 0x62, 0x63, 0x64, 0x65, 0x66, 0x67,
	0x68, 0x69, 0x6a, 0x6b, 0x6c, 0x6d, 0x6e, 0x6f,
	0x70, 0x71, 0x72, 0x73, 0x74, 0x75, 0x76, 0x77,
	0x78, 0x79, 0x7a, 0x7b, 0x7c, 0x7d, 0x7e, 0x7f,
}, string2[NCHARS];

static void setup __P((int *, char *, STR *, int, u_int));
static void usage __P((void));

int
main(argc, argv)
	int argc;
	char **argv;
{
	register int ch, cnt, lastch, *p;
	STR s1, s2;
	int cflag, dflag, sflag, isstring2;

	cflag = dflag = sflag = 0;
	while ((ch = getopt(argc, argv, "cds")) != EOF)
		switch((char)ch) {
		case 'c':
			cflag = 1;
			break;
		case 'd':
			dflag = 1;
			break;
		case 's':
			sflag = 1;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	switch(argc) {
	case 0:
	default:
		usage();
		/* NOTREACHED */
	case 1:
		isstring2 = 0;
		break;
	case 2:
		isstring2 = 1;
		break;
	}

	if (argv[0][0] == '\0')
		err("empty string1");
	if (isstring2 && argv[1][0] == '\0')
		err("empty string2");
	
	/*
	 * tr -ds [-c] string1 string2
	 * Delete all characters (or complemented characters) in string1.
	 * Squeeze all characters in string2.
	 */
	if (dflag && sflag) {
		if (!isstring2)
			usage();

		setup(string1, argv[0], &s1, cflag, T_CLASS | T_UL);
		setup(string2, argv[1], &s2, 0, T_CLASS | T_SEQ | T_UL);
		
		for (lastch = OOBCH; (ch = getchar()) != EOF;)
			if (!string1[ch] && (!string2[ch] || lastch != ch)) {
				lastch = ch;
				(void)putchar(ch);
			}
		exit(0);
	}

	/*
	 * tr -d [-c] string1
	 * Delete all characters (or complemented characters) in string1.
	 */
	if (dflag) {
		if (isstring2)
			usage();

		setup(string1, argv[0], &s1, cflag, T_CLASS | T_UL);

		while ((ch = getchar()) != EOF)
			if (!string1[ch])
				(void)putchar(ch);
		exit(0);
	}

	/*
	 * tr -s [-c] string1
	 * Squeeze all characters (or complemented characters) in string1.
	 */
	if (sflag && !isstring2) {
		setup(string1, argv[0], &s1, cflag, T_CLASS | T_UL);

		for (lastch = OOBCH; (ch = getchar()) != EOF;)
			if (!string1[ch] || lastch != ch) {
				lastch = ch;
				(void)putchar(ch);
			}
		exit(0);
	}

	/*
	 * tr [-cs] string1 string2
	 * Replace all characters (or complemented characters) in string1 with
	 * the character in the same position in string2.  If the -s option is
	 * specified, squeeze all the characters in string2.
	 */
	if (!isstring2)
		usage();

	s1.str = argv[0];
	s1.state = NORMAL;
	s1.lastch = OOBCH;
	s1.type = T_CLASS | T_UL;

	s2.str = argv[1];
	s2.state = NORMAL;
	s2.lastch = OOBCH;
	s2.type = T_SEQ;

	if (cflag) {
		for (cnt = NCHARS, p = string1; cnt--;)
			*p++ = OOBCH;
		/*
		 * More than a single character is meaningless with -c, but
		 * allow "tr abc [\n*]".
		 */
		if (!next(&s2))
			err("empty string2");
		for (ch = s2.lastch; next(&s2) && s2.state != INFINITE;);
		if (ch != s2.lastch)
			err("-c option and string2 has multiple characters");
		while (next(&s1))
			string1[s1.lastch] = ch;
		if (sflag)
			string2[ch] = 1;
		for (cnt = 0, p = string1; cnt < NCHARS; ++p, ++cnt)
			*p = *p == OOBCH ? ch : cnt;
	} else {
		s2.type |= T_UL;
		while (next(&s1)) {
			/*
			 * If the second string runs out of characters, just
			 * use the last one specified.
			 */
			if (!next(&s2))
				err("empty string2");
			if ((s1.state == ULSET || s2.state == ULSET) &&
			    s1.state != s2.state)
				err("mismatched lower/upper classes");
			string1[s1.lastch] = s2.lastch;
			if (sflag)
				string2[s2.lastch] = 1;
		}
		if (*s2.str)
			err("string2 longer than string1");
	}

	if (sflag)
		for (lastch = OOBCH; (ch = getchar()) != EOF;) {
			ch = string1[ch];
			if (!string2[ch] || lastch != ch) {
				lastch = ch;
				(void)putchar(ch);
			}
		}
	else
		while ((ch = getchar()) != EOF)
			(void)putchar(string1[ch]);
	exit (0);
}

static void
setup(string, arg, str, cflag, type)
	int *string;
	char *arg;
	STR *str;
	int cflag;
	u_int type;
{
	register int cnt, *p;

	str->str = arg;
	str->state = NORMAL;
	str->lastch = OOBCH;
	str->type = type;

	bzero(string, NCHARS * sizeof(int));
	while (next(str))
		string[str->lastch] = 1;
	if (cflag)
		for (p = string, cnt = NCHARS; cnt--; ++p)
			*p = !*p;
}

static void
usage()
{
	(void)fprintf(stderr, "usage: tr [-cs] string1 string2\n");
	(void)fprintf(stderr, "       tr [-c] -d string1\n");
	(void)fprintf(stderr, "       tr [-c] -s string1\n");
	(void)fprintf(stderr, "       tr [-c] -ds string1 string2\n");
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
	(void)fprintf(stderr, "tr: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	exit(1);
	/* NOTREACHED */
}
