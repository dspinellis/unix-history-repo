/*-
 * Copyright (c) 1993 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Barry Brachman.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mkdict.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Filter out words that:
 *	1) Are not completely made up of lower case letters
 *	2) Contain a 'q' not immediately followed by a 'u'
 *	3) Are less that 3 characters long
 *	4) Are greater than MAXWORDLEN characters long
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "bog.h"

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register char *p, *q;
	register int ch, common, n, nwords;
	int current, len, prev, qcount;
	char buf[2][MAXWORDLEN + 1];

	prev = 0;
	current = 1;
	buf[prev][0] = '\0';
	if (argc == 2)
		n = atoi(argv[1]);

	for (nwords = 1;
	    fgets(buf[current], MAXWORDLEN + 1, stdin) != NULL; ++nwords) {
		if ((p = index(buf[current], '\n')) == NULL) {
			fprintf(stderr,
			    "mkdict: word too long: %s\n", buf[current]);
			while ((ch = getc(stdin)) != EOF && ch != '\n')
				;
			if (ch == EOF)
				break;
			continue;
		}
		len = 0;
		for (p = buf[current]; *p != '\n'; p++) {
			if (!islower(*p))
				break;
			if (*p == 'q') {
				q = p + 1;
				if (*q != 'u')
					break;
				else {
					while (*q = *(q + 1))
						q++;
				}
				len++;
			}
			len++;
		}
		if (*p != '\n' || len < 3 || len > MAXWORDLEN)
			continue;
		if (argc == 2 && nwords % n)
			continue;

		*p = '\0';
		p = buf[current];
		q = buf[prev];
		qcount = 0;
		while ((ch = *p++) == *q++ && ch != '\0')
			if (ch == 'q')
				qcount++;
		common = p - buf[current] - 1;
		printf("%c%s", common + qcount, p - 1);
		prev = !prev;
		current = !current;
	}
	fprintf(stderr, "%d words\n", nwords);
	exit(0);
}
