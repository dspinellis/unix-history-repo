#ifndef lint
static char sccsid[] = "@(#)att2224.c	1.2 (Berkeley) %G%";
#endif

#include "../condevs.h"

attopn(telno, flds, dev)
char *telno, *flds[];
struct Devices *dev;
{
	char dcname[20], phone[MAXPH+10], c = 0;
	int dnf, failret = 0, timelim;

	sprintf(dcname, "/dev/%s", dev->D_line);

	if (setjmp(Sjbuf)) {
		delock(dev->D_line);
		logent("DEVICE", "NO");
		DEBUG(4, "Open timed out %s", dcname);
		alarm (0);
		return CF_NODEV;
	}

	signal(SIGALRM, alarmtr);
	getnextfd();
	alarm(10);

	if ((dnf = open(dcname, 2)) <= 0) {
		delock(dev->D_line);
		logent("DEVICE", "NO");
		DEBUG(4, "Can't open %s", dcname);
		alarm (0);
		return CF_NODEV;
	}

	alarm(0);
	next_fd = -1;
	fixline(dnf, dev->D_speed);
	DEBUG(4, "modem port - %s\n", dcname);

	if (setjmp(Sjbuf)) {
		delock(dev->D_line);
		logent("ACU WRITE", "FAILED");
		return CF_DIAL;
	}
	signal(SIGALRM, alarmtr);
	alarm(10);
	do {
		slowrite(dnf, "\r");		/* wake up modem */
	} while (expect(":~3", dnf));
	alarm(0);

	sprintf (phone, "atzt%s\r", telno);
	slowrite (dnf, phone);		/* type telno string to modem */

	if ((expect(phone, dnf)) != SUCCESS) {
		delock(dev->D_line);
		logent("ACU READ", "FAILED");
		return CF_DIAL;
	}

	if (setjmp(Sjbuf)) {
		delock(dev->D_line);
		logent("NO ANSWER", "FAILED");
		alarm (0);
		return CF_DIAL;
	}
	timelim = strlen(telno) * 4;
	signal(SIGALRM, alarmtr);
	alarm(timelim > 30 ? timelim : 30);

readchar:
	if ((read(dnf, &c, 1)) != 1) {
		delock(dev->D_line);
		logent("ACU READ", "FAILED");
		return CF_DIAL;
	}

	switch (c) {
		case 'D':	/* no dial tone */
			logent("NO DIAL TONE", "FAILED");
			failret++;
			break;
		case 'B': 	/* line busy */
			logent("LINE BUSY", "FAILED");
			failret++;
			break;
		case 'N': 	/* no answer */
			logent("NO ANSWER", "FAILED");
			failret++;
			break;
		case 'H':	/* handshake failed */
			logent("MODEM HANDSHAKE", "FAILED");
			failret++;
			break;
		case '3':	/* 2400 baud */
			DEBUG(4, "Baudrate set to 2400 baud", CNULL);
			fixline(dnf, 2400);
			break;
		case '2':	/* 1200 baud */
			DEBUG(4, "Baudrate set to 1200 baud", CNULL);
			fixline(dnf, 1200);
			break;
		case '1':	/* 300 baud */
			DEBUG(4, "Baudrate set to 300 baud", CNULL);
			fixline(dnf, 300);
			break;
		default:	/* Not one of the above, so must be garbage */
			goto readchar;
		}
	if (failret) {
		alarm (0);
		delock(dev->D_line);
		return CF_DIAL;
	}
	alarm (0);
	return dnf;
}

attcls(fd)
int fd;
{
	char dcname[20];
#ifdef USG
	struct termio hup, sav;
#else  !USG
	struct sgttyb hup, sav;
#endif !USG

	if (fd > 0) {
		sprintf(dcname, "/dev/%s", devSel);
		DEBUG(4, "Hanging up fd = %d\n", fd);
		/*
		 * code to drop DTR -- change to 0 baud then back to default.
		 */
		gtty(fd, &hup);
		gtty(fd, &sav);
#ifdef USG
		hup.c_cflag = B0;
#else  !USG
		hup.sg_ispeed = B0;
		hup.sg_ospeed = B0;
#endif !USG
		stty(fd, &hup);
		sleep(2);
		stty(fd, &sav);
		/*
		 * now raise DTR -- close the device
		 */
		sleep(2);
		close(fd);
		delock(devSel);
	}
}
