/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1989 The Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)nohup.c	5.1 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <unistd.h>
#include <stdio.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno;
	char *strerror();

	if (argc < 2)
		usage();

	if (isatty(STDOUT_FILENO))
		dofile();
	if (isatty(STDERR_FILENO) && dup2(STDOUT_FILENO, STDERR_FILENO) == -1) {
		/* may have just closed stderr */
		(void)fprintf(stdin, "nohup: %s\n", strerror(errno));
		exit(1);
	}

	(void)signal(SIGHUP, SIG_IGN);
	(void)signal(SIGQUIT, SIG_IGN);

	execvp(argv[1], &argv[1]);
	(void)fprintf(stderr,
	    "nohup: %s: %s\n", argv[1], strerror(errno));
	exit(1);
}

dofile()
{
	int fd;
	char *p, path[MAXPATHLEN];
	char *getenv(), *strcpy(), *strcat(), *strerror();

#define	FILENAME	"nohup.out"
	p = FILENAME;
	if ((fd = open(p, O_RDWR|O_CREAT|O_TRUNC, 0600)) >= 0)
		goto dupit;
	if (p = getenv("HOME")) {
		(void)strcpy(path, p);
		(void)strcat(path, "/");
		(void)strcat(path, FILENAME);
		if ((fd = open(p = path, O_RDWR|O_CREAT|O_TRUNC, 0600)) >= 0)
			goto dupit;
	}
	(void)fprintf(stderr, "nohup: can't open a nohup.out file.\n");
	exit(1);
dupit:	if (dup2(fd, STDOUT_FILENO) == -1) {
		(void)fprintf(stderr, "nohup: %s\n", strerror(errno));
		exit(1);
	}
	(void)fprintf(stderr, "sending output to %s\n", p);
}

usage()
{
	(void)fprintf(stderr, "usage: nohup command\n");
	exit(1);
}
