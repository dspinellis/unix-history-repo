/*-
 * Copyright (c) 1985 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 */

#ifndef lint
static char sccsid[] = "@(#)hys24.c	1.8 (Berkeley) %G%";
#endif /* not lint */

#include "condevs.h"

/*
 * hyspopn24(telno, flds, dev) connect to hayes smartmodem (pulse call)
 * hystopn24(telno, flds, dev) connect to hayes smartmodem (tone call)
 *
 * return codes: >0  -  file number  -  ok CF_DIAL,CF_DEVICE  -  failed 
 */

#include <sys/file.h>
#include <sys/ioctl.h>

hyspopn24(telno, flds, dev)
char *telno, *flds[];
struct Devices *dev;
{
	return hysopn24(telno, flds, dev, 0);
}

hystopn24(telno, flds, dev)
char *telno, *flds[];
struct Devices *dev;
{
	return hysopn24(telno, flds, dev, 1);
}

/* ARGSUSED */
hysopn24(telno, flds, dev, toneflag)
char *telno;
char *flds[];
struct Devices *dev;
int toneflag;
{
	int dh = -1;
	int result, ix, speed;
	char *ii;
	extern errno;
	char dcname[20];
	char resultbuf[16];

	sprintf(dcname, "/dev/%s", dev->D_line);
	DEBUG(4, "dc - %s\n", dcname);
	if (setjmp(Sjbuf)) {
		logent(dcname, "TIMEOUT");
		if (dh >= 0)
			hyscls24(dh, 0);
		return CF_DIAL;
	}
	signal(SIGALRM, alarmtr);
	getnextfd();
	alarm(10);
	dh = open(dcname, 2);	/* read/write */
	alarm(0);

	for (ii = telno; *ii; ii++)
		if (*ii == '=')
			*ii = ',';

	/* modem is open */
	next_fd = -1;
	if (dh >= 0) {
		ioctl(dh, TIOCHPCL, 0);
		fixline(dh, dev->D_speed);
		if (dochat(dev, flds, dh)) {
			logent(dcname, "CHAT FAILED");
			hyscls24(dh, 0);
			return CF_DIAL;
		}
		hyscls24(dh, 1);/* make sure the line is reset */
		write(dh, "AT&F&D3&C1E0M0X3QV0Y\r", 21);
		if (expect("0\r", dh) != 0) {
			logent(dcname, "HSM not responding OK");
			hyscls24(dh, 0);
			return CF_DIAL;
		}
		if (toneflag)
			write(dh, "\rATDT", 5);
		else
			write(dh, "\rATDP", 5);
		write(dh, telno, strlen(telno));
		write(dh, "\r", 1);

		if (setjmp(Sjbuf)) {
			logent(dcname, "Modem Hung");
			if (dh >= 0)
				hyscls24(dh, 0);
			return CF_DIAL;
		}
		signal(SIGALRM, alarmtr);
		alarm(120);
		do {
			for (ix = 0; ix < 16; ix++) {
				read(dh, resultbuf + ix, 1);
				DEBUG(6, "character read = 0x%X \n", resultbuf[ix]);
				if ((0x7f & resultbuf[ix]) == 0xd)
					break;
			}

			result = atol(resultbuf);
			switch (result) {
			case 0:
				logent("HSM Spurious OK response", _FAILED);
				speed = 0;
				break;
			case 1:
				logent("HSM connected at 300 baud!", _FAILED);
				speed = -1;
				break;
			case 2:
				speed = 0;
				DEBUG(4, "Ringing", 0);
				break;
			case 3:
				logent("HSM No Carrier", _FAILED);
				speed = -1;
				break;
			case 4:
				logent("HSM Error", _FAILED);
				speed = -1;
				break;
			case 5:
				speed = 1200;
				break;
			case 6:
				logent("HSM No dialtone", _FAILED);
				speed = -1;
				break;
			case 7:
				logent("HSM detected BUSY", _FAILED);
				speed = -1;
				break;
			case 8:
				logent("HSM No quiet answer", _FAILED);
				speed = -1;
				break;
			case 10:
				speed = 2400;
				break;
			default:
				logent("HSM Unknown response", _FAILED);
				speed = -1;
				break;
			}

		} while (speed == 0);

		alarm(0);

		if (speed < 0) {
			strcpy(devSel, dev->D_line);
			hyscls24(dh, 0);
			return CF_DIAL;
		} else if (speed != dev->D_speed) {
			DEBUG(4, "changing line speed to %d baud\n", speed);
			fixline(dh, speed);
		}
	}
	if (dh < 0) {
		logent(dcname, "CAN'T OPEN");
		return dh;
	}
	DEBUG(4, "hayes ok\n", CNULL);
	return dh;
}

hyscls24(fd, flag)
int fd, flag;
{
	char dcname[20];
	int fff = 1;

	if (fd > 0) {
		sprintf(dcname, "/dev/%s", devSel);
		if (flag)
			DEBUG(4, "Resetting fd = %d\n", fd);
		else
			DEBUG(4, "Hanging up fd = %d\n", fd);
		/*
		 * Since we have a getty sleeping on this line, when it wakes
		 * up it sends all kinds of garbage to the modem. 
		 * Unfortunatly, the modem likes to execute the previous
		 * command when it sees the garbage.  The previous command
		 * was to dial the phone, so let's make the last command
		 * reset the modem. 
		 */
		if (!flag)
			fixline(fd, 2400);
		write(fd, "\r", 1);
		sleep(2);
		write(fd, "+++", 3);
		sleep(3);
		write(fd, "\rATH\rATZ\r", 9);
		sleep(2);
		ioctl(fd, TIOCFLUSH, &fff);

		if (!flag) {
			close(fd);
			delock(devSel);
		}
	}
}
