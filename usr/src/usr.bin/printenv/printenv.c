/*
 * Copyright (c) 1987 Regents of the University of California.
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
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)printenv.c	5.2 (Berkeley) %G%";
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
