/*-
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1992, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pig.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void pigout __P((char *, int));
void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register int len;
	int ch;
	char buf[1024];

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	for (len = 0; (ch = getchar()) != EOF;) {
		if (isalpha(ch)) {
			if (len >= sizeof(buf)) {
				(void)fprintf(stderr, "pig: ate too much!\n");
				exit(1);
			}
			buf[len++] = ch;
			continue;
		}
		if (len != 0) {
			pigout(buf, len);
			len = 0;
		}
		(void)putchar(ch);
	}
	exit(0);
}

void
pigout(buf, len)
	char *buf;
	int len;
{
	register int ch, start;
	int olen;

	/*
	 * If the word starts with a vowel, append "way".  Don't treat 'y'
	 * as a vowel if it appears first.
	 */
	if (index("aeiouAEIOU", buf[0]) != NULL) {
		(void)printf("%.*sway", len, buf);
		return;
	}

	/*
	 * Copy leading consonants to the end of the word.  The unit "qu"
	 * isn't treated as a vowel.
	 */
	for (start = 0, olen = len;
	    !index("aeiouyAEIOUY", buf[start]) && start < olen;) {
		ch = buf[len++] = buf[start++];
		if ((ch == 'q' || ch == 'Q') && start < olen &&
		    (buf[start] == 'u' || buf[start] == 'U'))
			buf[len++] = buf[start++];
	}
	(void)printf("%.*say", olen, buf + start);
}

void
usage()
{
	(void)fprintf(stderr, "usage: pig\n");
	exit(1);
}
