#ifndef lint
static char sccsid[] = "@(#)hys.c	4.3 (Berkeley) %G%";
#endif

#include "../condevs.h"

#ifdef HAYES
/*
 *	hyspopn(telno, flds, dev) connect to hayes smartmodem (pulse call)
 *	hystopn(telno, flds, dev) connect to hayes smartmodem (tone call)
 *	char *flds[], *dev[];
 *
 *	return codes:
 *		>0  -  file number  -  ok
 *		CF_DIAL,CF_DEVICE  -  failed
 */

hyspopn(telno, flds, dev)
char *telno, *flds[];
struct Devices *dev;
{
	return hysopn(telno, flds, dev, 0);
}

hystopn(telno, flds, dev)
char *telno, *flds[];
struct Devices *dev;
{
	return hysopn(telno, flds, dev, 1);
}

/* ARGSUSED */
hysopn(telno, flds, dev, toneflag)
char *telno;
char *flds[];
struct Devices *dev;
int toneflag;
{
	int	dh = -1;
	extern errno;
	char dcname[20];
	char cbuf[MAXPH];
	register char *cp;
	register int i;

	sprintf(dcname, "/dev/%s", dev->D_line);
	DEBUG(4, "dc - %s\n", dcname);
	if (setjmp(Sjbuf)) {
		logent(dcname, "TIMEOUT");
		if (dh >= 0)
			close(dh);
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
		if (dochat(dev, flds, dh)) {
			logent(dcname, "CHAT FAILED");
			close(dh);
			return CF_DIAL;
		}
		write(dh, "ATV1H\r", 6);
		if (expect("OK\r\n", dh) != 0) {
			logent(dcname, "HSM seems dead");
			close(dh);
			return CF_DIAL;
		}
		if (toneflag)
			write(dh, "\rATDT", 5);
		else
			write(dh, "\rATDP", 5);
		write(dh, telno, strlen(telno));
		write(dh, "\r", 1);

		if (setjmp(Sjbuf)) {
			logent(dcname, "TIMEOUT");
			strcpy(devSel, dev->D_line);
			hyscls(dh);
			return CF_DIAL;
		}
		signal(SIGALRM, alarmtr);
		alarm(MAXMSGTIME);
		cp = cbuf;
		while (read(dh, cp ,1) == 1)
			if (*cp >= ' ')
				break;
		cp++;
		while (cp < &cbuf[MAXPH] && read(dh, cp, 1) == 1 && *cp++ != '\n')
			;
		alarm(0);
		*cp = '\0';
		if (strncmp(cbuf, "CONNECT", 7) != 0) {
			logent("HSM no carrier", _FAILED);
			strcpy(devSel, dev->D_line);
			hyscls(dh);
			return CF_DIAL;
		}
		DEBUG(4,"\nGOT: %s", cbuf);
		i = atoi(&cbuf[8]);
		if (i > 0 && i != dev->D_speed) {	
			DEBUG(4,"Baudrate reset to %d\n", i);
			fixline(dh, i);
		}

	}
	if (dh < 0) {
		logent(dcname, "CAN'T OPEN");
		return dh;
	}
	DEBUG(4, "hayes ok\n", CNULL);
	return dh;
}

hyscls(fd)
int fd;
{
	char dcname[20];
#ifdef DROPDTR
	struct sgttyb hup, sav;
#endif

	if (fd > 0) {
		sprintf(dcname, "/dev/%s", devSel);
		DEBUG(4, "Hanging up fd = %d\n", fd);
#ifdef DROPDTR
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
#else
		sleep(3);
		write(fd, "+++", 3);
#endif
		sleep(3);
		write(fd, "ATZ\r", 4);
		if (expect("OK",fd) != 0)
			logent(devSel, "HSM did not respond to ATZ");
		sleep(1);
		close(fd);
		delock(devSel);
	}
}

#endif HAYES

