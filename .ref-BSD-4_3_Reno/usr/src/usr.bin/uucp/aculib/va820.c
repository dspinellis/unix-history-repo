#ifndef lint
static char sccsid[] = "@(#)va820.c	4.4 (Berkeley) 2/24/88";
#endif

#include "../condevs.h"

/*
 * Racal-Vadic 'RV820' with 831 adaptor.
 * BUGS:
 * dialer baud rate is hardcoded
 */
#define	MAXDIG 30	/* set by switches inside adapter */
char	c_abort	= '\001';
char	c_start	= '\002';
char	c_empty	= '\017';
char	c_end	= '\003';

va820opn(ph, flds, dev)
char *ph, *flds[];
struct Devices *dev;
{
	register int va, i, child;
	char c, acu[20], com[20];
	char vadbuf[MAXDIG+2];
	int nw, lt;
	unsigned timelim;
	struct sgttyb sg;

	child = -1;
	if (strlen(ph) > MAXDIG) {
		DEBUG(4, "BAD PHONE NUMBER %s\n", ph);
		logent("rvadopn", "BAD PHONE NUMBER");
		i = CF_DIAL;
		goto ret;
	}

	if (setjmp(Sjbuf)) {
		logent("rvadopn", "TIMEOUT");
		i = CF_DIAL;
		goto ret;
	}
	DEBUG(4, "ACU %s\n", dev->D_calldev);
	DEBUG(4, "LINE %s\n", dev->D_line);
	sprintf(acu, "/dev/%s", dev->D_calldev);
	getnextfd();
	signal(SIGALRM, alarmtr);
	alarm(10);
	va = open(acu, 2);
	alarm(0);
	next_fd = -1;
	if (va < 0) {
		DEBUG(4, "ACU OPEN FAIL %d\n", errno);
		logent(acu, "CAN'T OPEN");
		i = CF_NODEV;
		goto ret;
	}
	/*
	 * Set speed and modes on dialer and clear any
	 * previous requests
	 */
	DEBUG(4, "SETTING UP VA831 (%d)\n", va);
	ioctl(va, TIOCGETP, &sg);
	sg.sg_ispeed = sg.sg_ospeed = B1200;
	sg.sg_flags |= RAW;
	sg.sg_flags &= ~ECHO;
	ioctl(va, TIOCSETP, &sg);
	DEBUG(4, "CLEARING VA831\n", 0);
	if ( write(va, &c_abort, 1) != 1) {
		DEBUG(4,"BAD VA831 WRITE %d\n", errno);
		logent(acu, "CAN'T CLEAR");
		i = CF_DIAL;
		goto ret;
	}
	sleep(1);			/* XXX */
	read(va, &c, 1);
	if (c != 'B') {
		DEBUG(4,"BAD VA831 RESPONSE %c\n", c);
		logent(acu, "CAN'T CLEAR");
		i = CF_DIAL;
		goto ret;
	}
	/*
	 * Build the dialing sequence for the adapter
	 */
	DEBUG(4, "DIALING %s\n", ph);
	sprintf(vadbuf, "%c%s<%c%c", c_start, ph, c_empty, c_end);
	timelim = 5 * strlen(ph);
	alarm(timelim < 30 ? 30 : timelim);
	nw = write(va, vadbuf, strlen(vadbuf));	/* Send Phone Number */
	if (nw != strlen(vadbuf)) {
		DEBUG(4,"BAD VA831 WRITE %d\n", nw);
		logent(acu, "BAD WRITE");
		goto failret;
	}

	sprintf(com, "/dev/%s", dev->D_line);

	/* create child to open comm line */
	if ((child = fork()) == 0) {
		signal(SIGINT, SIG_DFL);
		open(com, 0);
		sleep(5);
		_exit(1);
	}

	DEBUG(4, "WAITING FOR ANSWER\n", 0);
	if (read(va, &c, 1) != 1) {
		logent("ACU READ", _FAILED);
		goto failret;
	}
	switch(c) {
	case 'A':
		/* Fine! */
		break;
	case 'B':
		DEBUG(2, "Line Busy / No Answer\n", 0);
		goto failret;
	case 'D':
		DEBUG(2, "Dialer format error\n", 0);
		goto failret;
	case 'E':
		DEBUG(2, "Dialer parity error\n", 0);
		goto failret;
	case 'F':
		DEBUG(2, "Phone number too long\n", 0);
		goto failret;
	case 'G':
		DEBUG(2, "Modem Busy\n", 0);
		goto failret;
	default:
		DEBUG(2, "Unknown MACS return code '%c'\n", c&0177);
		goto failret;
	}
	/*
	 * open line - will return on carrier
	 */
	if ((i = open(com, 2)) < 0) {
		if (errno == EIO)
			logent("carrier", "LOST");
		else
			logent("dialup open", _FAILED);
		goto failret;
	}
	DEBUG(2, "RVADIC opened %d\n", i);
	fixline(i, dev->D_speed);
	goto ret;
failret:
	i = CF_DIAL;
ret:
	alarm(0);
	if (child != -1)
		kill(child, SIGKILL);
	close(va);
	while ((nw = wait(&lt)) != child && nw != -1)
		;
	return i;
}

va820cls(fd)
register int fd;
{

	DEBUG(2, "RVADIC close %d\n", fd);
	close(fd);
}
