/*
 * Copyright (c) 1980 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)unexpand.c	5.2 (Berkeley) 10/26/88";
#endif /* not lint */

/*
 * unexpand - put tabs into a file replacing blanks
 */
#include <stdio.h>

char	genbuf[BUFSIZ];
char	linebuf[BUFSIZ];
int	all;

main(argc, argv)
	int argc;
	char *argv[];
{
	register char *cp;

	argc--, argv++;
	if (argc > 0 && argv[0][0] == '-') {
		if (strcmp(argv[0], "-a") != 0) {
			fprintf(stderr, "usage: unexpand [ -a ] file ...\n");
			exit(1);
		}
		all++;
		argc--, argv++;
	}
	do {
		if (argc > 0) {
			if (freopen(argv[0], "r", stdin) == NULL) {
				perror(argv[0]);
				exit(1);
			}
			argc--, argv++;
		}
		while (fgets(genbuf, BUFSIZ, stdin) != NULL) {
			for (cp = linebuf; *cp; cp++)
				continue;
			if (cp > linebuf)
				cp[-1] = 0;
			tabify(all);
			printf("%s", linebuf);
		}
	} while (argc > 0);
	exit(0);
}

tabify(c)
	char c;
{
	register char *cp, *dp;
	register int dcol;
	int ocol;

	ocol = 0;
	dcol = 0;
	cp = genbuf, dp = linebuf;
	for (;;) {
		switch (*cp) {

		case ' ':
			dcol++;
			break;

		case '\t':
			dcol += 8;
			dcol &= ~07;
			break;

		default:
			while (((ocol + 8) &~ 07) <= dcol) {
				if (ocol + 1 == dcol)
					break;
				*dp++ = '\t';
				ocol += 8;
				ocol &= ~07;
			}
			while (ocol < dcol) {
				*dp++ = ' ';
				ocol++;
			}
			if (*cp == 0 || c == 0) {
				strcpy(dp, cp);
				return;
			}
			*dp++ = *cp;
			ocol++, dcol++;
		}
		cp++;
	}
}
