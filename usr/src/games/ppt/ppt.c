/*
 * Copyright (c) 1988 Regents of the University of California.
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
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)ppt.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

void	putppt();

/*ARGSUSED*/
main(argc, argv)
	int argc;
	char **argv;
{
	register int c;

	(void) puts("___________");
	while ((c = getchar()) != EOF)
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
