/*
 * Copyright (c) 1988 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1988 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)chroot.c	5.8 (Berkeley) 6/1/90";
#endif /* not lint */

#include <stdio.h>
#include <paths.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno;
	char *shell, *getenv(), *strerror();

	if (argc < 2) {
		(void)fprintf(stderr, "usage: chroot newroot [command]\n");
		exit(1);
	}
	if (chdir(argv[1]) || chroot("."))
		fatal(argv[1]);
	if (argv[2]) {
		execvp(argv[2], &argv[2]);
		fatal(argv[2]);
	} else {
		if (!(shell = getenv("SHELL")))
			shell = _PATH_BSHELL;
		execlp(shell, shell, "-i", (char *)NULL);
		fatal(shell);
	}
	/* NOTREACHED */
}

fatal(msg)
	char *msg;
{
	extern int errno;

	(void)fprintf(stderr, "chroot: %s: %s\n", msg, strerror(errno));
	exit(1);
}
