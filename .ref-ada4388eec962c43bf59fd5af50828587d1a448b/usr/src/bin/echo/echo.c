/*
 * Copyright (c) 1989, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1989, 1993\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)echo.c	8.1 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int nflag;

	/* This utility may NOT do getopt(3) option parsing. */
	if (*++argv && !strcmp(*argv, "-n")) {
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
