/*
 * Copyright (c) 1987 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific written prior permission. This software
 * is provided ``as is'' without express or implied warranty.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif /* not lint */

#ifndef lint
static char sccsid[] = "@(#)mesg.c	4.4 (Berkeley) %G%";
#endif /* not lint */

/*
 * mesg -- set current tty to accept or
 *	forbid write permission.
 *
 *	mesg [y] [n]
 *		y allow messages
 *		n forbid messages
 */

#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>

static char *tty;

main(argc, argv)
	int argc;
	char **argv;
{
	struct stat sbuf;
	char *ttyname();

	if (!(tty = ttyname(2))) {
		fputs("mesg: not a device in /dev.\n", stderr);
		exit(-1);
	}
	if (stat(tty, &sbuf) < 0) {
		perror("mesg");
		exit(-1);
	}
	if (argc < 2) {
		if (sbuf.st_mode & 020) {
			fputs("is y\n", stderr);
			exit(0);
		}
		fputs("is n\n", stderr);
		exit(1);
	}
#define	OTHER_WRITE	020
	switch(*argv[1]) {
	case 'y':
		newmode(sbuf.st_mode | OTHER_WRITE);
		exit(0);
	case 'n':
		newmode(sbuf.st_mode &~ OTHER_WRITE);
		exit(1);
	default:
		fputs("usage: mesg [y] [n]\n", stderr);
		exit(-1);
	}
	/*NOTREACHED*/
}

static
newmode(m)
	u_short m;
{
	if (chmod(tty, m) < 0) {
		perror("mesg");
		exit(-1);
	}
}
