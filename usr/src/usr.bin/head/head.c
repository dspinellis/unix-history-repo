/*
 * Copyright (c) 1980, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)head.c	5.5 (Berkeley) 6/1/90";
#endif /* not lint */

#include <stdio.h>
#include <ctype.h>
/*
 * head - give the first few lines of a stream or of each of a set of files
 *
 * Bill Joy UCB August 24, 1977
 */

main(argc, argv)
	int	argc;
	char	**argv;
{
	register int	ch, cnt;
	int	firsttime, linecnt = 10;

	if (argc > 1 && argv[1][0] == '-') {
		if (!isdigit(argv[1][1])) {
			fprintf(stderr, "head: illegal option -- %c\n", argv[1][1]);
			goto usage;
		}
		if ((linecnt = atoi(argv[1] + 1)) < 0) {
usage:			fputs("usage: head [-line_count] [file ...]\n", stderr);
			exit(1);
		}
		--argc; ++argv;
	}
	/* setlinebuf(stdout); */
	for (firsttime = 1, --argc, ++argv;; firsttime = 0) {
		if (!*argv) {
			if (!firsttime)
				exit(0);
		}
		else {
			if (!freopen(*argv, "r", stdin)) {
				fprintf(stderr, "head: can't read %s.\n", *argv);
				exit(1);
			}
			if (argc > 1) {
				if (!firsttime)
					putchar('\n');
				printf("==> %s <==\n", *argv);
			}
			++argv;
		}
		for (cnt = linecnt; cnt; --cnt)
			while ((ch = getchar()) != EOF)
				if (putchar(ch) == '\n')
					break;
	}
	/*NOTREACHED*/
}
