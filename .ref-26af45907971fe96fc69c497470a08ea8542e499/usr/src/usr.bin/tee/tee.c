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
static char sccsid[] = "@(#)tee.c	5.13 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/stat.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct _list {
	struct _list *next;
	int fd;
	char *name;
} LIST;
LIST *head;

void add __P((int, char *));
void err __P((int, const char *, ...));

int
main(argc, argv)
	int argc;
	char *argv[];
{
	register LIST *p;
	register int n, fd, rval, wval;
	register char *bp;
	int append, ch, exitval;
	char *buf;
#define	BSIZE (8 * 1024)

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

	if ((buf = malloc((u_int)BSIZE)) == NULL)
		err(1, "%s", strerror(errno));

	add(STDOUT_FILENO, "stdout");

	for (exitval = 0; *argv; ++argv)
		if ((fd = open(*argv, append ? O_WRONLY|O_CREAT|O_APPEND :
		    O_WRONLY|O_CREAT|O_TRUNC, DEFFILEMODE)) < 0) {
			err(0, "%s: %s", *argv, strerror(errno));
			exitval = 1;
		} else
			add(fd, *argv);

	while ((rval = read(STDIN_FILENO, buf, BSIZE)) > 0)
		for (p = head; p; p = p->next) {
			n = rval;
			bp = buf;
			do {
				if ((wval = write(p->fd, bp, n)) == -1) {
					err(0, "%s: %s",
					    p->name, strerror(errno));
					exitval = 1;
					break;
				}
				bp += wval;
			} while (n -= wval);
		}
	if (rval < 0)
		err(1, "read: %s", strerror(errno));
	exit(exitval);
}

void
add(fd, name)
	int fd;
	char *name;
{
	LIST *p;

	if ((p = malloc((u_int)sizeof(LIST))) == NULL)
		err(1, "%s", strerror(errno));
	p->fd = fd;
	p->name = name;
	p->next = head;
	head = p;
}

#if __STDC__
#include <stdarg.h>
#else
#include <varargs.h>
#endif

void
#if __STDC__
err(int doexit, const char *fmt, ...)
#else
err(doexit, fmt, va_alist)
	int doexit;
	char *fmt;
        va_dcl
#endif
{
	va_list ap;
#if __STDC__
	va_start(ap, fmt);
#else
	va_start(ap);
#endif
	(void)fprintf(stderr, "tee: ");
	(void)vfprintf(stderr, fmt, ap);
	va_end(ap);
	(void)fprintf(stderr, "\n");
	if (doexit)
		exit(1);
}
