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
static char sccsid[] = "@(#)df.c	5.2 (Berkeley) %G%";
#endif /* not lint */

/*
 * Dial the DF02-AC or DF03-AC
 */

#include "tip.h"

static jmp_buf Sjbuf;
static timeout();

df02_dialer(num, acu)
	char *num, *acu;
{

	return (df_dialer(num, acu, 0));
}

df03_dialer(num, acu)
	char *num, *acu;
{

	return (df_dialer(num, acu, 1));
}

df_dialer(num, acu, df03)
	char *num, *acu;
	int df03;
{
	register int f = FD;
	struct sgttyb buf;
	int speed = 0, rw = 2;
	char c = '\0';

	ioctl(f, TIOCHPCL, 0);		/* make sure it hangs up when done */
	if (setjmp(Sjbuf)) {
		printf("connection timed out\r\n");
		df_disconnect();
		return (0);
	}
	if (boolean(value(VERBOSE)))
		printf("\ndialing...");
	fflush(stdout);
#ifdef TIOCMSET
	if (df03) {
		int st = TIOCM_ST;	/* secondary Transmit flag */

		ioctl(f, TIOCGETP, &buf);
		if (buf.sg_ospeed != B1200) {	/* must dial at 1200 baud */
			speed = buf.sg_ospeed;
			buf.sg_ospeed = buf.sg_ispeed = B1200;
			ioctl(f, TIOCSETP, &buf);
			ioctl(f, TIOCMBIC, &st); /* clear ST for 300 baud */
		} else
			ioctl(f, TIOCMBIS, &st); /* set ST for 1200 baud */
	}
#endif
	signal(SIGALRM, timeout);
	alarm(5 * strlen(num) + 10);
	ioctl(f, TIOCFLUSH, &rw);
	write(f, "\001", 1);
	sleep(1);
	write(f, "\002", 1);
	write(f, num, strlen(num));
	read(f, &c, 1);
#ifdef TIOCMSET
	if (df03 && speed) {
		buf.sg_ispeed = buf.sg_ospeed = speed;
		ioctl(f, TIOCSETP, &buf);
	}
#endif
	return (c == 'A');
}

df_disconnect()
{
	int rw = 2;

	write(FD, "\001", 1);
	sleep(1);
	ioctl(FD, TIOCFLUSH, &rw);
}


df_abort()
{

	df_disconnect();
}


static
timeout()
{

	longjmp(Sjbuf, 1);
}
