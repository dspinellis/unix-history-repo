/*
 * Copyright (c) 1980, 1987 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* !lint */

#ifndef lint
static char sccsid[] = "@(#)head.c	5.2 (Berkeley) %G%";
#endif /* !lint */

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
