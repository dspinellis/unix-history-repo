/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tty.c	4.4 (Berkeley) %G%";
#endif /* not lint */

#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	int ch, sflag;
	char *t, *ttyname();

	sflag = 0;
	while ((ch = getopt(argc, argv, "s")) != EOF)
		switch((char)ch) {
		case 's':
			sflag = 1;
			break;
		case '?':
		default:
			fputs("usage: tty [-s]\n", stderr);
			exit(2);
		}

	t = ttyname(0);
	if (!sflag)
		puts(t ? t : "not a tty");
	exit(t ? 0 : 1);
}
