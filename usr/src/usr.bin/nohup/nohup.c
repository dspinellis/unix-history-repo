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
static char sccsid[] = "@(#)nohup.c	5.5 (Berkeley) %G%";
#endif /* not lint */

#include <sys/param.h>
#include <sys/stat.h>

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

void dofile __P((void));
void usage __P((void));

int
main(argc, argv)
	int argc;
	char *argv[];
{
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

void
dofile()
{
	int fd;
	char *p, path[MAXPATHLEN];

#define	FILENAME	"nohup.out"
	p = FILENAME;
	if ((fd = open(p, O_RDWR|O_CREAT, S_IRUSR | S_IWUSR)) >= 0)
		goto dupit;
	if (p = getenv("HOME")) {
		(void)strcpy(path, p);
		(void)strcat(path, "/");
		(void)strcat(path, FILENAME);
		if ((fd = open(p = path,
		    O_RDWR|O_CREAT, S_IRUSR | S_IWUSR)) >= 0)
			goto dupit;
	}
	(void)fprintf(stderr, "nohup: can't open a nohup.out file.\n");
	exit(1);

dupit:	(void)lseek(fd, (off_t)0, SEEK_END);
	if (dup2(fd, STDOUT_FILENO) == -1) {
		(void)fprintf(stderr, "nohup: %s\n", strerror(errno));
		exit(1);
	}
	(void)fprintf(stderr, "sending output to %s\n", p);
}

void
usage()
{
	(void)fprintf(stderr, "usage: nohup command\n");
	exit(1);
}
