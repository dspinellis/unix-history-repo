#ifndef lint
static char sccsid[] = "@(#)hys24.c	1.4 (Berkeley) %G%";
#endif !lint

#include "../condevs.h"

/*
 *	hyspopn24(telno, flds, dev) connect to hayes smartmodem (pulse call)
 *	hystopn24(telno, flds, dev) connect to hayes smartmodem (tone call)
 *	char *flds[], *dev[];
 *
 *	return codes:
 *		>0  -  file number  -  ok
 *		CF_DIAL,CF_DEVICE  -  failed
 */

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
	int	dh = -1;
	char *ii;
	extern errno;
	char dcname[20];

	sprintf(dcname, "/dev/%s", dev->D_line);
	DEBUG(4, "dc - %s\n", dcname);
	if (setjmp(Sjbuf)) {
		logent(dcname, "TIMEOUT");
		if (dh >= 0)
			hyscls24(dh);
		return CF_DIAL;
	}
	signal(SIGALRM, alarmtr);
	getnextfd();
	alarm(10);
	dh = open(dcname, 2); /* read/write */
	alarm(0);

	for (ii = telno; *ii; ii++)
		if (*ii == '=')
		    *ii = ',';

	/* modem is open */
	next_fd = -1;
	if (dh >= 0) {
		fixline(dh, dev->D_speed);
		write(dh, "\rATZH\r", 6);
		sleep(2);
		if (dochat(dev, flds, dh)) {
			logent(dcname, "CHAT FAILED");
			hyscls24(dh);
			return CF_DIAL;
		}
		write(dh, "AT&F&D3&C1E0X1\r", 15);
		if (expect("OK\r\n", dh) != 0) {
			logent(dcname, "HSM not responding OK");
			hyscls24(dh);
			return CF_DIAL;
		}
		if (toneflag)
			write(dh, "\rATDT", 5);
		else
			write(dh, "\rATDP", 5);
		write(dh, telno, strlen(telno));
		write(dh, "\r", 1);

		if (expect("CONNECT", dh) != 0) {
			logent("HSM no carrier", _FAILED);
			strcpy(devSel, dev->D_line);
			hyscls24(dh);
			return CF_DIAL;
		}

	}
	if (dh < 0) {
		logent(dcname, "CAN'T OPEN");
		return dh;
	}
	DEBUG(4, "hayes ok\n", CNULL);
	return dh;
}

hyscls24(fd)
int fd;
{
	char dcname[20];

	if (fd > 0) {
		sprintf(dcname, "/dev/%s", devSel);
		DEBUG(4, "Hanging up fd = %d\n", fd);
		sleep(1);
/*
 * Since we have a getty sleeping on this line, when it wakes up it sends
 * all kinds of garbage to the modem.  Unfortunatly, the modem likes to
 * execute the previous command when it sees the garbage.  The previous
 * command was to dial the phone, so let's make the last command reset
 * the modem.
 */
		write(fd, "\r+++", 4);
		sleep(2);
		write(fd, "\rATH\rATZ\r", 9);
		sleep(2);
		close(fd);
		delock(devSel);
	}
}
