/*
 * Copyright (c) 1980, 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)head.c	5.3 (Berkeley) %G%";
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
