/*
 * Copyright (c) 1988 Regents of the University of California.
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
"@(#) Copyright (c) 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tee.c	5.6 (Berkeley) %G%";
#endif /* not lint */

#include <sys/types.h>
#include <sys/file.h>
#include <signal.h>
#include <stdio.h>

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
	register int append, n, fd;
	int ch, exitval;
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
	add(1, "stdout");
	for (; *argv; ++argv)
		if ((fd = open(*argv, append ? O_WRONLY|O_CREAT :
		    O_WRONLY|O_CREAT|O_TRUNC, 0600)) < 0)
			(void)fprintf(stderr, "tee: %s: %s.\n",
			    *argv, strerror(errno));
		else {
			if (append)
				(void)lseek(fd, 0L, L_XTND);
			add(fd, *argv);
		}
	exitval = 0;
	while ((n = read(0, buf, sizeof(buf))) > 0)
		for (p = head; p; p = p->next)
			if (write(p->fd, buf, n) != n) {
				(void)fprintf(stderr, "tee: %s: %s.\n",
				    p->name, strerror(errno));
				exitval = 1;
			}
	if (n < 0) {
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
