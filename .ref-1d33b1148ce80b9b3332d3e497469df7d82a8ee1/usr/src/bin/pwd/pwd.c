/*
 * Copyright (c) 1991, 1993, 1994
 *	The Regents of the University of California.  All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1991, 1993, 1994\n\
	The Regents of the University of California.  All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)pwd.c	8.3 (Berkeley) %G%";
#endif /* not lint */

#include <err.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	int ch;
	char *p;

	/*
	 * Flags for pwd are a bit strange.  The POSIX 1003.2B/D9 document
	 * has an optional -P flag for physical, which is what this program
	 * will produce by default.  The logical flag, -L, should fail, as
	 * there's no way to display a logical path after forking.  We don't
	 * document either flag, only adding -P for future portability.
	 */
	while ((ch = getopt(argc, argv, "P")) != EOF)
		switch (ch) {
		case 'P':
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc != 0)
		usage();

	if ((p = getcwd(NULL, 0)) == NULL)
		err(1, NULL);
	(void)printf("%s\n", p);
	exit(0);
}

void
usage()
{

	(void)fprintf(stderr, "usage: pwd\n");
	exit(1);
}
