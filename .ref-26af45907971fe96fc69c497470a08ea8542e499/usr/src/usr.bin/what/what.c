/*
 * Copyright (c) 1980, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)what.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

/*
 * what
 */
/* ARGSUSED */
main(argc, argv)
	int argc;
	char **argv;
{
	if (!*++argv) 
		search();
	else do {
		if (!freopen(*argv, "r", stdin)) {
			perror(*argv);
			exit(1);
		}
		printf("%s\n", *argv);
		search();
	} while(*++argv);
	exit(0);
}

search()
{
	register int c;

	while ((c = getchar()) != EOF) {
loop:		if (c != '@')
			continue;
		if ((c = getchar()) != '(')
			goto loop;
		if ((c = getchar()) != '#')
			goto loop;
		if ((c = getchar()) != ')')
			goto loop;
		putchar('\t');
		while ((c = getchar()) != EOF && c && c != '"' &&
		    c != '>' && c != '\n')
			putchar(c);
		putchar('\n');
	}
}
