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
static char sccsid[] = "@(#)printenv.c	5.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * printenv
 *
 * Bill Joy, UCB
 * February, 1979
 */
main(argc, argv)
	int argc;
	char **argv;
{
	extern char **environ;
	register char *cp, **ep;
	register int len;

	if (argc < 2) {
		for (ep = environ; *ep; ep++)
			puts(*ep);
		exit(0);
	}
	len = strlen(*++argv);
	for (ep = environ; *ep; ep++)
		if (!strncmp(*ep, *argv, len)) {
			cp = *ep + len;
			if (!*cp || *cp == '=') {
				puts(*cp ? cp + 1 : cp);
				exit(0);
			}
		}
	exit(1);
}
