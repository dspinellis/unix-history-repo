/*
 * Copyright (c) 1983 The Regents of the University of California.
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
static char sccsid[] = "@(#)biz31.c	5.2 (Berkeley) %G%";
#endif /* not lint */

#include "tip.h"

#define MAXRETRY	3		/* sync up retry count */
#define DISCONNECT_CMD	"\21\25\11\24"	/* disconnection string */

static	int sigALRM();
static	int timeout = 0;
static	jmp_buf timeoutbuf;

/*
 * Dial up on a BIZCOMP Model 1031 with either
 * 	tone dialing (mod = "f")
 *	pulse dialing (mod = "w")
 */
static int
biz_dialer(num, mod)
	char *num, *mod;
{
	register int connected = 0;

	if (!bizsync(FD)) {
		logent(value(HOST), "", "biz", "out of sync");
		printf("bizcomp out of sync\n");
		delock(uucplock);
		exit(0);
	}
	if (boolean(value(VERBOSE)))
		printf("\nstarting call...");
	echo("#\rk$\r$\n");			/* disable auto-answer */
	echo("$>$.$ #\r");			/* tone/pulse dialing */
	echo(mod);
	echo("$\r$\n");
	echo("$>$.$ #\re$ ");			/* disconnection sequence */
	echo(DISCONNECT_CMD);
	echo("\r$\n$\r$\n");
	echo("$>$.$ #\rr$ ");			/* repeat dial */
	echo(num);
	echo("\r$\n");
	if (boolean(value(VERBOSE)))
		printf("ringing...");
	/*
	 * The reply from the BIZCOMP should be:
	 *	`^G NO CONNECTION\r\n^G\r\n'	failure
	 *	` CONNECTION\r\n^G'		success
	 */
	connected = detect(" ");
#ifdef ACULOG
	if (timeout) {
		char line[80];

		sprintf(line, "%d second dial timeout",
			number(value(DIALTIMEOUT)));
		logent(value(HOST), num, "biz", line);
	}
#endif
	if (!connected)
		flush(" NO CONNECTION\r\n\07\r\n");
	else
		flush("CONNECTION\r\n\07");
	if (timeout)
		biz31_disconnect();	/* insurance */
	return (connected);
}

biz31w_dialer(num, acu)
	char *num, *acu;
{

	return (biz_dialer(num, "w"));
}

biz31f_dialer(num, acu)
	char *num, *acu;
{

	return (biz_dialer(num, "f"));
}

biz31_disconnect()
{

	write(FD, DISCONNECT_CMD, 4);
	sleep(2);
	ioctl(FD, TIOCFLUSH);
}

biz31_abort()
{

	write(FD, "\33", 1);
}

static int
echo(s)
	register char *s;
{
	char c;

	while (c = *s++) switch (c) {

	case '$':
		read(FD, &c, 1);
		s++;
		break;

	case '#':
		c = *s++;
		write(FD, &c, 1);
		break;

	default:
		write(FD, &c, 1);
		read(FD, &c, 1);
	}
}

static int
sigALRM()
{

	timeout = 1;
	longjmp(timeoutbuf, 1);
}

static int
detect(s)
	register char *s;
{
	char c;
	int (*f)();

	f = signal(SIGALRM, sigALRM);
	timeout = 0;
	while (*s) {
		if (setjmp(timeoutbuf)) {
			printf("\07timeout waiting for reply\n");
			biz31_abort();
			break;
		}
		alarm(number(value(DIALTIMEOUT)));
		read(FD, &c, 1);
		alarm(0);
		if (c != *s++)
			break;
	}
	signal(SIGALRM, f);
	return (timeout == 0);
}

static int
flush(s)
	register char *s;
{
	char c;
	int (*f)();

	f = signal(SIGALRM, sigALRM);
	while (*s++) {
		if (setjmp(timeoutbuf))
			break;
		alarm(10);
		read(FD, &c, 1);
		alarm(0);
	}
	signal(SIGALRM, f);
	timeout = 0;			/* guard against disconnection */
}

/*
 * This convoluted piece of code attempts to get
 *  the bizcomp in sync.  If you don't have the capacity or nread
 *  call there are gory ways to simulate this.
 */
static int
bizsync(fd)
{
#ifdef FIOCAPACITY
	struct capacity b;
#	define chars(b)	((b).cp_nbytes)
#	define IOCTL	FIOCAPACITY
#endif
#ifdef FIONREAD
	long b;
#	define chars(b)	(b)
#	define IOCTL	FIONREAD
#endif
	register int already = 0;
	char buf[10];

retry:
	if (ioctl(fd, IOCTL, (caddr_t)&b) >= 0 && chars(b) > 0)
		ioctl(fd, TIOCFLUSH);
	write(fd, "\rp>\r", 4);
	sleep(1);
	if (ioctl(fd, IOCTL, (caddr_t)&b) >= 0) {
		if (chars(b) != 10) {
	nono:
			if (already > MAXRETRY)
				return (0);
			write(fd, DISCONNECT_CMD, 4);
			sleep(2);
			already++;
			goto retry;
		} else {
			read(fd, buf, 10);
			if (strncmp(buf, "p >\r\n\r\n>", 8))
				goto nono;
		}
	}
	return (1);
}
