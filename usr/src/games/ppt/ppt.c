/*
 * Copyright (c) 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1988, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ppt.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

static void	putppt();

main(argc, argv)
	int argc;
	char **argv;
{
	register int c;
	register char *p;

	(void) puts("___________");
	if (argc > 1)
		while (p = *++argv)
			for (; *p; ++p)
				putppt((int)*p);
	else while ((c = getchar()) != EOF)
		putppt(c);
	(void) puts("___________");
	exit(0);
}

static void
putppt(c)
	register int c;
{
	register int i;

	(void) putchar('|');
	for (i = 7; i >= 0; i--) {
		if (i == 2)
			(void) putchar('.');	/* feed hole */
		if ((c&(1<<i)) != 0)
			(void) putchar('o');
		else
			(void) putchar(' ');
	}
	(void) putchar('|');
	(void) putchar('\n');
}
