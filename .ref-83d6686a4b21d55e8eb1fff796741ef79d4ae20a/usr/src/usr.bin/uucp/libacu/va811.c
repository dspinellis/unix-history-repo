#ifndef lint
static char sccsid[] = "@(#)va811.c	4.2 (Berkeley) %G%";
#endif

#include "../condevs.h"

/*
 * Racal-Vadic VA811 dialer with 831 adaptor.
 * A typical 300 baud L-devices entry is
 *	ACU /dev/tty10 unused 300 va811s
 * where tty10 is the communication line (D_Line),
 * and 300 is the line speed.
 * This is almost identical to RVMACS except that we don't need to
 * send addresses and modem types, and don't need the fork.
 *	Joe Kelsey, fluke!joe, vax4.1526, Apr 11 1984.
 */

#define	STX	02	/* Access Adaptor */
#define	ETX	03	/* Transfer to Dialer */
#define	SI	017	/* Buffer Empty (end of phone number) */
#define	SOH	01	/* Abort */

va811opn(ph, flds, dev)
char *ph, *flds[];
struct Devices *dev;
{
	int va;
	register int i, tries;
	char c, dcname[20];
	char vabuf[35];		/* STX, 31 phone digits, SI, ETX, NUL */

	va = 0;
	sprintf(dcname, "/dev/%s", dev->D_line);
	if (setjmp(Sjbuf)) {
		DEBUG(1, "timeout va811 open\n", 0);
		logent("va811opn", "TIMEOUT");
		if (va >= 0)
			close(va);
		delock(dev->D_line);
		return CF_NODEV;
	}
	DEBUG(4, "va811: STARTING CALL\n", 0);
	getnextfd();
	signal(SIGALRM, alarmtr);
	alarm(10);
	va = open(dcname, 2);
	alarm(0);

	/* line is open */
	next_fd = -1;
	if (va < 0) {
		DEBUG(4, errno == 4 ? "%s: no carrier\n" : "%s: can't open\n",
			dcname);
		delock(dev->D_line);
		logent(dcname, "CAN'T OPEN");
		return(errno == 4 ? CF_DIAL : CF_NODEV);
	}
	fixline(va, dev->D_speed);

	/* first, reset everything */
	sendthem("\\01\\c", va);
	DEBUG(4, "wanted %c ", 'B');
	i = expect("B", va);
	DEBUG(4, "got %s\n", i ? "?" : "that");
	if (i != 0) {
	    DEBUG(4, "va811: NO RESPONSE\n", 0);
	    logent("va811 reset", "TIMEOUT");
	    close(va);
	    delock(dev->D_line);
	    return CF_DIAL;
	}

	sprintf(vabuf, "%c%.31s%c%c\\c", STX, ph, SI, SOH);
	sendthem(vabuf, va);
	DEBUG(4, "wanted %c ", 'B');
	i = expect("B", va);
	DEBUG(4, "got %s\n", i ? "?" : "that");

	if (i != 0) {
	    DEBUG(4, "va811: STORE NUMBER\n", 0);
	    logent("va811 STORE", _FAILED);
	    close(va);
	    delock(dev->D_line);
	    return CF_DIAL;
	}

	for (tries = 0; tries < TRYCALLS; tries++) {
	    sprintf(vabuf, "%c%c\\c", STX, ETX);
	    sendthem(vabuf, va);
	    DEBUG(4, "DIALING...", CNULL);
	    i = expect("A", va);
	    DEBUG(4, " %s\n", i ? _FAILED : "SUCCEEDED");
	    if (i != 0) {
		DEBUG(4, "va811: RESETTING\n", CNULL);
		logent("va811 DIAL", _FAILED);
	        sendthem("\\01\\c", va);
	        expect("B", va);
	    }
	    else
	        break;
	}

	if (tries >= TRYCALLS) {
	    close(va);
	    delock(dev->D_line);
	    return CF_DIAL;
	}

	DEBUG(4, "va811 ok\n", CNULL);
	return va;
}

va811cls(fd)
register int fd;
{
	DEBUG(2, "va811 close %d\n", fd);
	p_chwrite(fd, SOH);
/*	ioctl(fd, TIOCCDTR, NULL);*/
	close(fd);
	delock(devSel);
}
