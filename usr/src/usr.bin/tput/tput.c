/*
 * Copyright (c) 1980, 1988 Regents of the University of California.
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
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1980, 1988 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)tput.c	5.3 (Berkeley) %G%";
#endif /* not lint */

#include <sys/termios.h>
#include <stdio.h>
#include <unistd.h>

main(argc, argv)
	int argc;
	char **argv;
{
	extern char *optarg;
	extern int optind;
	int ch, exitval, myputchar();
	char *ap, *p, *term, buf[1024], tbuf[1024];
	char *getenv(), *tgetstr(), *realname();

	term = NULL;
	while ((ch = getopt(argc, argv, "T:")) != EOF)
		switch(ch) {
		case 'T':
			term = optarg;
			break;
		case '?':
		default:
			usage();
		}
	argc -= optind;
	argv += optind;

	if (!term && !(term = getenv("TERM"))) {
		(void)fprintf(stderr, "tput: no terminal type specified.\n");
		exit(3);
	}
	if (tgetent(tbuf, term) != 1) {
		(void)fprintf(stderr, "tput: tgetent failure.\n");
		exit(3);
	}
	setospeed();
	for (p = buf, exitval = 0; *argv; ++argv)
		if (ap = tgetstr(realname(*argv), &p))
			tputs(ap, 1, myputchar);
		else
			exitval = 2;
	exit(exitval);
}

char *
realname(s)
	char *s;
{
	switch(*s) {
	case 'c':
		if (!strcmp(s, "clear"))
			return("cl");
		break;
	case 'i':
		if (!strcmp(s, "init"))
			return("is");
		break;
	case 'r':
		if (!strcmp(s, "reset"))
			return("rs");
		break;
	}
	return(s);
}

myputchar(c)
	int c;
{
	putchar(c);
}

setospeed()
{
	extern int errno, ospeed;
	struct termios t;
	char *strerror();

	if (tcgetattr(STDOUT_FILENO, &t) == -1) {
		(void)fprintf(stderr, "tput: %s\n", strerror(errno));
		exit(1);
	}
	ospeed = cfgetospeed(&t);
}

usage()
{
	(void)fprintf(stderr, "usage: tput [-T term] attribute ...\n");
	exit(1);
}
