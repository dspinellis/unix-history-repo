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
static char sccsid[] = "@(#)tee.c	5.10 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <signal.h>
#include <stdio.h>
#include <unistd.h>

typedef struct _list {
	struct _list *next;
	int fd;
	char *name;
} LIST;
LIST *head;

main(argc, argv)
	int argc;
	char **argv;
{
	extern int errno, optind;
	register LIST *p;
	register int n, fd, rval, wval;
	register char *bp;
	int append, ch, exitval;
	char *buf, *malloc(), *strerror();
	off_t lseek();

	append = 0;
	while ((ch = getopt(argc, argv, "ai")) != EOF)
		switch((char)ch) {
		case 'a':
			append = 1;
			break;
		case 'i':
			(void)signal(SIGINT, SIG_IGN);
			break;
		case '?':
		default:
			(void)fprintf(stderr, "usage: tee [-ai] [file ...]\n");
			exit(1);
		}
	argv += optind;
	argc -= optind;

	if (!(buf = malloc((u_int)8 * 1024))) {
		(void)fprintf(stderr, "tee: out of space.\n");
		exit(1);
	}
	add(STDOUT_FILENO, "stdout");
	for (; *argv; ++argv)
		if ((fd = open(*argv, append ? O_WRONLY|O_CREAT|O_APPEND :
		    O_WRONLY|O_CREAT|O_TRUNC, 0600)) < 0)
			(void)fprintf(stderr, "tee: %s: %s.\n",
			    *argv, strerror(errno));
		else
			add(fd, *argv);
	exitval = 0;
	while ((rval = read(STDIN_FILENO, buf, sizeof(buf))) > 0)
		for (p = head; p; p = p->next) {
			n = rval;
			bp = buf;
			do {
				if ((wval = write(p->fd, bp, n)) == -1) {
					(void)fprintf(stderr, "tee: %s: %s.\n",
					    p->name, strerror(errno));
					exitval = 1;
					break;
				}
				bp += wval;
			} while (n -= wval);
		}
	if (rval < 0) {
		(void)fprintf(stderr, "tee: read: %s\n", strerror(errno));
		exit(1);
	}
	exit(exitval);
}

add(fd, name)
	int fd;
	char *name;
{
	LIST *p;
	char *malloc(), *strerror();

	/* NOSTRICT */
	if (!(p = (LIST *)malloc((u_int)sizeof(LIST)))) {
		(void)fprintf(stderr, "tee: out of space.\n");
		exit(1);
	}
	p->fd = fd;
	p->name = name;
	p->next = head;
	head = p;
}
