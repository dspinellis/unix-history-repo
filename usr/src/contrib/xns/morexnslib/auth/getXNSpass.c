/* $Header: getXNSpass.c,v 2.0 85/11/21 07:22:25 jqj Exp $ */
/*
 * contains: getXNSpass
 * based on the standard library routine getpass(), it is modified
 * for XNS passwords, which may be of almost arbitrary length
 */

/* $Log:	getXNSpass.c,v $
 * Revision 2.0  85/11/21  07:22:25  jqj
 * 4.3BSD standard release
 * 
 * Revision 1.1  85/03/26  06:29:09  jqj
 * Initial revision
 * 
 * Revision 1.1  85/03/26  06:29:09  jqj
 * Initial revision
 * 
 */

/* @(#)getpass.c	4.3 (Berkeley) 5/16/84 */
#include <stdio.h>
#include <signal.h>
#include <sgtty.h>
#define PASSLEN 80

char *
getXNSpass(prompt)
char *prompt;
{
	struct sgttyb ttyb;
	int flags;
	register char *p;
	register c;
	FILE *fi;
	static char pbuf[PASSLEN+1];
	int (*signal())();
	int (*sig)();

	if ((fi = fdopen(open("/dev/tty", 2), "r")) == NULL)
		fi = stdin;
	else
		setbuf(fi, (char *)NULL);
	sig = signal(SIGINT, SIG_IGN);
	ioctl(fileno(fi), TIOCGETP, &ttyb);
	flags = ttyb.sg_flags;
	ttyb.sg_flags &= ~ECHO;
	ioctl(fileno(fi), TIOCSETP, &ttyb);
	fprintf(stderr, "%s", prompt); fflush(stderr);
	for (p=pbuf; (c = getc(fi))!='\n' && c!=EOF;) {
		if (p < &pbuf[PASSLEN])
			*p++ = c;
	}
	*p = '\0';
	fprintf(stderr, "\n"); fflush(stderr);
	ttyb.sg_flags = flags;
	ioctl(fileno(fi), TIOCSETP, &ttyb);
	signal(SIGINT, sig);
	if (fi != stdin)
		fclose(fi);
	return(pbuf);
}
