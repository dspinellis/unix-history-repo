/*	biz22.c	4.1	81/11/29	*/
#include "tip.h"

#if BIZ1022
#define DISCONNECT	"\20\04"	/* disconnection string */

static int sigALRM();
static int timeout = 0;

/*
 * Dial up on a BIZCOMP Model 1022 with either
 * 	tone dialing (mod = "V")
 *	pulse dialing (mod = "W")
 */
static int
biz_dialer(num, mod)
	char *num, *mod;
{
	register int connected = 0;
	char cbuf[40];

	if (boolean(value(VERBOSE)))
		printf("\nstarting call...");
	/*
	 * Disable auto-answer and configure for tone/pulse
	 *  dialing
	 */
	if (cmd("\02K\r")) {
		printf("can't initialize bizcomp...");
		return (0);
	}
	strcpy(cbuf, "\02.\r");
	cbuf[1] = *mod;
	if (cmd(cbuf)) {
		printf("can't set dialing mode...");
		return (0);
	}
	strcpy(cbuf, "\02D");
	strcat(cbuf, num);
	strcat(cbuf, "\r");
	write(FD, cbuf, strlen(cbuf));
	if (!detect("7\r")) {
		printf("can't get dial tone...");
		return (0);
	}
	if (boolean(value(VERBOSE)))
		printf("ringing...");
	/*
	 * The reply from the BIZCOMP should be:
	 *	2 \r or 7 \r	failure
	 *	1 \r		success
	 */
	connected = detect("1\r");
#ifdef ACULOG
	if (timeout) {
		char line[80];

		sprintf(line, "%d second dial timeout",
			number(value(DIALTIMEOUT)));
		logent(value(HOST), num, "biz1022", line);
	}
#endif
	if (timeout)
		biz22_disconnect();	/* insurance */
	return (connected);
}

biz22w_dialer(num, acu)
	char *num, *acu;
{
	return (biz_dialer(num, "W"));
}

biz22f_dialer(num, acu)
	char *num, *acu;
{
	return (biz_dialer(num, "V"));
}

biz22_disconnect()
{
	write(FD, DISCONNECT, 4);
	sleep(2);
	ioctl(FD, TIOCFLUSH);
}

biz22_abort()
{
	write(FD, "\02", 1);
	timeout = 1;
}

static int
sigALRM()
{
	signal(SIGALRM, SIG_IGN);
	printf("\07timeout waiting for reply\n");
	timeout = 1;
}

static int
cmd(s)
	register char *s;
{
	char c;

	write(FD, s, strlen(s));
	timeout = 0;
	signal(SIGALRM, biz22_abort);
	alarm(number(value(DIALTIMEOUT)));
	read(FD, &c, 1);
	alarm(0);
	signal(SIGALRM, SIG_DFL);
	if (timeout)
		return (1);
	c &= 0177;
	return (c != '\r');
}

static int
detect(s)
	register char *s;
{
	char c;

	signal(SIGALRM, biz22_abort);
	timeout = 0;
	while (*s) {
		alarm(number(value(DIALTIMEOUT)));
		read(FD, &c, 1);
		alarm(0);
		if (timeout)
			return (0);
		c &= 0177;
		if (c != *s++)
			return (0);
	}
	signal(SIGALRM, SIG_DFL);
	return (1);
}
