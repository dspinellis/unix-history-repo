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
static char sccsid[] = "@(#)dirname.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <stdlib.h>

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

	if (argc != 1)
		usage();

	/*
	 * (1) If string is //, skip steps (2) through (5).
	 * (2) If string consists entirely of slash characters, string
	 *     shall be set to a single slash character.  In this case,
	 *     skip steps (3) through (8).
	 */
	for (p = *argv;; ++p) {
		if (!*p) {
			if (p > *argv)
				(void)printf("/\n");
			else
				(void)printf(".\n");
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
	 * (4) If there are no slash characters remaining in string,
	 *     string shall be set to a single period character.  In this
	 *     case skip steps (5) through (8).
	 *
	 * (5) If there are any trailing nonslash characters in string,
	 *     they shall be removed.
	 */
	while (--p >= *argv)
		if (*p == '/')
			break;
	++p;
	if (p == *argv) {
		(void)printf(".\n");
		exit(0);
	}

	/*
	 * (6) If the remaining string is //, it is implementation defined
	 *     whether steps (7) and (8) are skipped or processed.
	 *
	 * This case has already been handled, as part of steps (1) and (2).
	 */
	
	/*
	 * (7) If there are any trailing slash characters in string, they
	 *     shall be removed.
	 */
	while (--p >= *argv)
		if (*p != '/')
			break;
	++p;

	/*
	 * (8) If the remaining string is empty, string shall be set to
	 *     a single slash character.
	 */
	*p = '\0';
	(void)printf("%s\n", p == *argv ? "/" : *argv);
	exit(0);
}

usage()
{
	(void)fprintf(stderr, "usage: dirname path\n");
	exit(1);
}
