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
static char sccsid[] = "@(#)basename.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int optind;
	register char *p;
	int ch;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 1 && argc != 2)
		usage();

	/*
	 * (1) If string is // it is implementation defined whether steps (2)
	 *     through (5) are skipped or processed.
	 *
	 * (2) If string consists entirely of slash characters, string shall
	 *     be set to a single slash character.  In this case, skip steps
	 *     (3) through (5).
	 */
	for (p = *argv;; ++p) {
		if (!*p) {
			if (p > *argv)
				(void)printf("/\n");
			else
				(void)printf("\n");
			exit(0);
		}
		if (*p != '/')
			break;
	}

	/*
	 * (3) If there are any trailing slash characters in string, they
	 *     shall be removed.
	 */
	for (; *p; ++p);
	while (*--p == '/');
	*++p = '\0';

	/*
	 * (4) If there are any slash characters remaining in string, the
	 *     prefix of string up to an including the last slash character
	 *     in string shall be removed.
	 */
	while (--p >= *argv)
		if (*p == '/')
			break;
	++p;

	/*
	 * (5) If the suffix operand is present, is not identical to the
	 *     characters remaining in string, and is identical to a suffix
	 *     of the characters remaining in string, the suffix suffix
	 *     shall be removed from string.
	 */
	if (*++argv) {
		int suffixlen, stringlen, off;

		suffixlen = strlen(*argv);
		stringlen = strlen(p);

		if (suffixlen < stringlen) {
			off = stringlen - suffixlen;
			if (!strcmp(p + off, *argv))
				p[off] = '\0';
		}
	}
	(void)printf("%s\n", p);
	exit(0);
}

usage()
{
	(void)fprintf(stderr, "usage: basename string [suffix]\n");
	exit(1);
}
