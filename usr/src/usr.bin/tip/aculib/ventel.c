/*	ventel.c	1.1	82/04/19	*/

#if VENTEL
/*
 * Routines for calling up on a Ventel Modem
 */
#include "tip.h"
#include <setjmp.h>
#include <errno.h>

#define	MAXRETRY	5
#define	DISCONNECT	"\03"		/* ^C */

static int sigALRM();
static int timeout = 0;

ven_dialer(num, acu)
	register char *num;
	char *acu;
{
	register char *cp;
	register int connected = 0;
#ifdef ACULOG
	char line[80];
#endif
	/*
	 * Get in synch with a couple of carriage returns
	 */
	if (!vensync(FD)) {
		printf("can't synchronize with ventel\n");
#ifdef ACULOG
		logent(value(HOST), num, "ventel", "can't synch up");
#endif
		return (0);
	}
	ioctl(FD, TIOCHPCL, 0);
	echo("k$\n$D$I$A$L$:$ <");
	for (cp = num; *cp; cp++) {
		sleep(1);
		write(FD, cp, 1);
		read(FD, cp, 1);
	}
	echo(">\r$\n");
	if (gobble('\n'))
		connected = gobble('!');
	ioctl(FD, TIOCFLUSH);
#ifdef ACULOG
	if (timeout) {
		sprintf(line, "%d second dial timeout",
			number(value(DIALTIMEOUT)));
		logent(value(HOST), num, "ventel", line);
	}
#endif
	if (timeout)
		ven_disconnect();	/* insurance */
	return (connected);
}

ven_disconnect()
{
	close(FD);
}

ven_abort()
{
	write(FD, "\03", 1);
	close(FD);
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
	signal(SIGALRM, SIG_IGN);
	printf("\07timeout waiting for reply\n");
	timeout = 1;
}

static int
gobble(s)
	register char s;
{
	char c;

	signal(SIGALRM, sigALRM);
	timeout = 0;
	do {
		alarm(number(value(DIALTIMEOUT)));
		read(FD, &c, 1);
		c &= 0177;
#ifdef notdef
		if (boolean(value(VERBOSE)))
#endif
			putchar(c);
		alarm(0);
		if (timeout)
			return (0);
	} while (c != '\n' && c != s);
	signal(SIGALRM, SIG_DFL);
	return (c == s);
}

#define min(a,b)	((a)>(b)?(b):(a))
/*
 * This convoluted piece of code attempts to get
 * the ventel in sync.  If you don't have the capacity or nread
 * call there are gory ways to simulate this.
 */
static int
vensync(fd)
{
	long nread;
	register int already = 0, nbytes;
	char buf[60];

	/*
	 * Toggle DTR to force anyone off that might have left
	 * the modem connected, and insure a consistent state
	 * to start from.
	 *
	 * If you don't have the ioctl calls to diddle directly
	 * with DTR, you can always try setting the baud rate to 0.
	 */
	ioctl(FD, TIOCCDTR, 0);
	sleep(2);
	ioctl(FD, TIOCSDTR, 0);
	while (already < MAXRETRY) {
		/*
		 * After reseting the modem, send it two \r's to
		 * autobaud on. Make sure to delay between them
		 * so the modem can frame the incoming characters.
		 */
		write(fd, "\r", 1);
		sleep(1);
		write(fd, "\r", 1);
		sleep(3);
		if (ioctl(fd, FIONREAD, (caddr_t)&nread) >= 0) {
			nbytes = nread;
			while (nbytes > 0) {
				read(fd, buf, min(nbytes, 60));
				if ((buf[nbytes-1]&0177) == '$')
					return (1);
				nbytes -= min(nbytes, 60);
			}
			sleep(1);
			already++;
		}
	}
	return (0);
}
#endif
