#ifndef lint
static char sccsid[] = "@(#)nov.c	4.2 (Berkeley) 2/24/88";
#endif

#include "../condevs.h"

/***
 *	novopn(telno, flds, dev) connect to novation Smart-Cat
 *	(similar to hayes smartmodem)
 *	char *flds[], *dev[];
 *
 *	return codes:
 *		>0  -  file number  -  ok
 *		CF_DIAL,CF_DEVICE  -  failed
 */

novopn(telno, flds, dev)
char *telno;
char *flds[];
struct Devices *dev;
{
	int	dh = -1;
	extern errno;
	char dcname[20];
	int pulse = 0;

	sprintf(dcname, "/dev/%s", dev->D_line);
	DEBUG(4, "dc - %s\n", dcname);
	if (strcmp(dev->D_calldev, "pulse") == 0)
		pulse = 1;
	if (setjmp(Sjbuf)) {
		DEBUG(1, "timeout novation open %s\n", dcname);
		logent("novation open", "TIMEOUT");
		if (dh >= 0)
			close(dh);
		delock(dev->D_line);
		return CF_DIAL;
	}
	signal(SIGALRM, alarmtr);
	getnextfd();
	alarm(10);
	dh = open(dcname, 2); /* read/write */
	alarm(0);

	/* modem is open */
	next_fd = -1;
	if (dh >= 0) {
		fixline(dh, dev->D_speed);
		/* set guard time small so line is in transparant mode */
		slowrite(dh, "\rATS12=1\r");
		if (expect("OK", dh) != 0) {
			logent("NOV no line", _FAILED);
			strcpy(devSel, dev->D_line);
			novcls(dh);
			return CF_DIAL;
		}

		if (pulse)
			slowrite(dh, "ATDP");
		else
			slowrite(dh, "ATDT");
		slowrite(dh, telno);
		slowrite(dh, "\r");

		if (expect("CONNECT", dh) != 0) {
			logent("NOV no carrier", _FAILED);
			strcpy(devSel, dev->D_line);
			novcls(dh);
			return CF_DIAL;
		}

	}
	if (dh < 0) {
		DEBUG(4, "novation failed\n", CNULL);
		delock(dev->D_line);
	}
	DEBUG(4, "novation ok\n", CNULL);
	return dh;
}

novcls(fd)
int fd;
{
	char dcname[20];
	struct sgttyb hup, sav;

	if (fd > 0) {
		sprintf(dcname, "/dev/%s", devSel);
		DEBUG(4, "Hanging up fd = %d\n", fd);
		/*
		 * code to drop DTR -- change to 0 baud then back to default.
		 */
		gtty(fd, &hup);
		gtty(fd, &sav);
		hup.sg_ispeed = B0;
		hup.sg_ospeed = B0;
		stty(fd, &hup);
		sleep(2);
		stty(fd, &sav);
		/*
		 * now raise DTR -- close the device & open it again.
		 */
		sleep(2);
		close(fd);
		sleep(2);
		fd = open(dcname, 2);
		/*
		 * Since we have a getty sleeping on this line, when it wakes up it sends
		 * all kinds of garbage to the modem.  Unfortunatly, the modem likes to
		 * execute the previous command when it sees the garbage.  The previous
		 * command was to dial the phone, so let's make the last command reset
		 * the modem.
		 */
		sleep(2);
		slowrite(fd, "\rATZ\r");
		close(fd);
		delock(devSel);
	}
}
