/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)echo.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

/* ARGSUSED */
main(argc, argv)
	int argc;
	char **argv;
{
	int nflag;

	++argv;
	if (!strcmp(*argv, "-n")) {
		++argv;
		nflag = 1;
	}
	else
		nflag = 0;

	while (*argv) {
		(void)printf("%s", *argv);
		if (*++argv)
			putchar(' ');
	}
	if (!nflag)
		putchar('\n');
	exit(0);
}
