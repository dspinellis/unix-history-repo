/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)getpass.c	5.4 (Berkeley) 3/7/86";
#endif not lint

#include <stdio.h>
#include <signal.h>
#include <sgtty.h>

static	struct sgttyb ttyb;
static	int flags;
static	FILE *fi;

static intfix()
{
	ttyb.sg_flags = flags;
	if (fi != NULL)
		(void) stty(fileno(fi), &ttyb);
	exit(SIGINT);
}

char *
mygetpass(prompt)
char *prompt;
{
	register char *p;
	register c;
	static char pbuf[50+1];
	int (*signal())();
	int (*sig)();

	if ((fi = fopen("/dev/tty", "r")) == NULL)
		fi = stdin;
	else
		setbuf(fi, (char *)NULL);
	sig = signal(SIGINT, intfix);
	(void) gtty(fileno(fi), &ttyb);
	flags = ttyb.sg_flags;
	ttyb.sg_flags &= ~ECHO;
	(void) stty(fileno(fi), &ttyb);
	fprintf(stderr, "%s", prompt); (void) fflush(stderr);
	for (p=pbuf; (c = getc(fi))!='\n' && c!=EOF;) {
		if (p < &pbuf[sizeof(pbuf)-1])
			*p++ = c;
	}
	*p = '\0';
	fprintf(stderr, "\n"); (void) fflush(stderr);
	ttyb.sg_flags = flags;
	(void) stty(fileno(fi), &ttyb);
	(void) signal(SIGINT, sig);
	if (fi != stdin)
		(void) fclose(fi);
	return(pbuf);
}
