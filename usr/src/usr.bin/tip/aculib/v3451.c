/*
 * Copyright (c) 1983 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 */

#ifndef lint
static char sccsid[] = "@(#)v3451.c	5.5 (Berkeley) %G%";
#endif /* not lint */

/*
 * Routines for calling up on a Vadic 3451 Modem
 */
#include "tip.h"

static	jmp_buf Sjbuf;

v3451_dialer(num, acu)
	register char *num;
	char *acu;
{
	sig_t func;
	int ok;
	int slow = number(value(BAUDRATE)) < 1200, rw = 2;
	char phone[50];
#ifdef ACULOG
	char line[80];
#endif
	static int expect();
	static void vawrite();

	/*
	 * Get in synch
	 */
	vawrite("I\r", 1 + slow);
	vawrite("I\r", 1 + slow);
	vawrite("I\r", 1 + slow);
	vawrite("\005\r", 2 + slow);
	if (!expect("READY")) {
		printf("can't synchronize with vadic 3451\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "can't synch up");
#endif
		return (0);
	}
	ioctl(FD, TIOCHPCL, 0);
	sleep(1);
	vawrite("D\r", 2 + slow);
	if (!expect("NUMBER?")) {
		printf("Vadic will not accept dial command\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "will not accept dial");
#endif
		return (0);
	}
	strcpy(phone, num);
	strcat(phone, "\r");
	vawrite(phone, 1 + slow);
	if (!expect(phone)) {
		printf("Vadic will not accept phone number\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "will not accept number");
#endif
		return (0);
	}
	func = signal(SIGINT,SIG_IGN);
	/*
	 * You cannot interrupt the Vadic when its dialing;
	 * even dropping DTR does not work (definitely a
	 * brain damaged design).
	 */
	vawrite("\r", 1 + slow);
	vawrite("\r", 1 + slow);
	if (!expect("DIALING:")) {
		printf("Vadic failed to dial\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "failed to dial");
#endif
		return (0);
	}
	if (boolean(value(VERBOSE)))
		printf("\ndialing...");
	ok = expect("ON LINE");
	signal(SIGINT, func);
	if (!ok) {
		printf("call failed\n");
#ifdef ACULOG
		logent(value(HOST), num, "vadic", "call failed");
#endif
		return (0);
	}
	ioctl(FD, TIOCFLUSH, &rw);
	return (1);
}

v3451_disconnect()
{

	close(FD);
}

v3451_abort()
{

	close(FD);
}

static void
vawrite(cp, delay)
	register char *cp;
	int delay;
{

	for (; *cp; sleep(delay), cp++)
		write(FD, cp, 1);
}

static
expect(cp)
	register char *cp;
{
	char buf[300];
	register char *rp = buf;
	int timeout = 30, online = 0;
	static int notin();
	static void alarmtr();

	if (strcmp(cp, "\"\"") == 0)
		return (1);
	*rp = 0;
	/*
	 * If we are waiting for the Vadic to complete
	 * dialing and get a connection, allow more time
	 * Unfortunately, the Vadic times out 24 seconds after
	 * the last digit is dialed
	 */
	online = strcmp(cp, "ON LINE") == 0;
	if (online)
		timeout = number(value(DIALTIMEOUT));
	signal(SIGALRM, alarmtr);
	if (setjmp(Sjbuf))
		return (0);
	alarm(timeout);
	while (notin(cp, buf) && rp < buf + sizeof (buf) - 1) {
		if (online && notin("FAILED CALL", buf) == 0)
			return (0);
		if (read(FD, rp, 1) < 0) {
			alarm(0);
			return (0);
		}
		if (*rp &= 0177)
			rp++;
		*rp = '\0';
	}
	alarm(0);
	return (1);
}

static void
alarmtr()
{
	longjmp(Sjbuf, 1);
}

static int
notin(sh, lg)
	char *sh, *lg;
{
	static int prefix();

	for (; *lg; lg++)
		if (prefix(sh, lg))
			return (0);
	return (1);
}

static
prefix(s1, s2)
	register char *s1, *s2;
{
	register char c;

	while ((c = *s1++) == *s2++)
		if (c == '\0')
			return (1);
	return (c == '\0');
}
