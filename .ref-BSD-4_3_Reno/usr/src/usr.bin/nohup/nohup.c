/*
 * Copyright (c) 1989 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that: (1) source distributions retain this entire copyright
 * notice and comment, and (2) distributions including binaries display
 * the following acknowledgement:  ``This product includes software
 * developed by the University of California, Berkeley and its contributors''
 * in the documentation or other materials provided with the distribution
 * and in all advertising materials mentioning features or use of this
 * software. Neither the name of the University nor the names of its
 * contributors may be used to endorse or promote products derived
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
static char sccsid[] = "@(#)nohup.c	5.4 (Berkeley) 6/1/90";
#endif /* not lint */

#include <sys/param.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <unistd.h>
#include <stdio.h>

extern int errno;

main(argc, argv)
	int argc;
	char **argv;
{
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
	off_t lseek();
	char *getenv(), *strcpy(), *strcat(), *strerror();

#define	FILENAME	"nohup.out"
	p = FILENAME;
	if ((fd = open(p, O_RDWR|O_CREAT, 0600)) >= 0)
		goto dupit;
	if (p = getenv("HOME")) {
		(void)strcpy(path, p);
		(void)strcat(path, "/");
		(void)strcat(path, FILENAME);
		if ((fd = open(p = path, O_RDWR|O_CREAT, 0600)) >= 0)
			goto dupit;
	}
	(void)fprintf(stderr, "nohup: can't open a nohup.out file.\n");
	exit(1);

dupit:	(void)lseek(fd, 0L, SEEK_END);
	if (dup2(fd, STDOUT_FILENO) == -1) {
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
