/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)printenv.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

void	usage __P((void));

/*
 * printenv
 *
 * Bill Joy, UCB
 * February, 1979
 */
int
main(argc, argv)
	int argc;
	char *argv[];
{
	extern char **environ;
	register char *cp, **ep;
	register size_t len;
	int ch;

	while ((ch = getopt(argc, argv, "")) != EOF)
		switch(ch) {
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (argc == 0) {
		for (ep = environ; *ep; ep++)
			(void)printf("%s\n", *ep);
		exit(0);
	}
	len = strlen(*argv);
	for (ep = environ; *ep; ep++)
		if (!memcmp(*ep, *argv, len)) {
			cp = *ep + len;
			if (!*cp || *cp == '=') {
				(void)printf("%s\n", *cp ? cp + 1 : cp);
				exit(0);
			}
		}
	exit(1);
}

void
usage()
{
	(void)fprintf(stderr, "usage: printenv [name]\n");
	exit(1);
}
