#ifndef lint
static char sccsid[] = "@(#)vad.c	4.3 (Berkeley) %G%";
#endif

#include "../condevs.h"

/*
 *	vadopn: establish dial-out connection through a Racal-Vadic 3450.
 *	Returns descriptor open to tty for reading and writing.
 *	Negative values (-1...-7) denote errors in connmsg.
 *	Be sure to disconnect tty when done, via HUPCL or stty 0.
 */

vadopn(telno, flds, dev)
char *telno;
char *flds[];
struct Devices *dev;
{
	int	dh = -1;
	int	i, ok, er = 0, delay;
	extern errno;
	char dcname[20];

	sprintf(dcname, "/dev/%s", dev->D_line);
	if (setjmp(Sjbuf)) {
		DEBUG(1, "timeout vadic open\n", "");
		logent("vadic open", "TIMEOUT");
		if (dh >= 0)
			close(dh);
		delock(dev->D_line);
		return CF_NODEV;
	}
	signal(SIGALRM, alarmtr);
	getnextfd();
	alarm(10);
	dh = open(dcname, 2);
	alarm(0);

	/* modem is open */
	next_fd = -1;
	if (dh < 0) {
		delock(dev->D_line);
		return CF_NODEV;
	}
	fixline(dh, dev->D_speed);

	DEBUG(4, "calling %s -> ", telno);
	if (dochat(dev, flds, dh)) {
		logent(dcname, "CHAT FAILED");
		close(dh);
		return CF_DIAL;
	}
	delay = 0;
	for (i = 0; i < strlen(telno); ++i) {
		switch(telno[i]) {
		case '=':	/* await dial tone */
		case '-':
		case ',':
		case '<':
		case 'K':
			telno[i] = 'K';
			delay += 5;
			break;
		}
	}
	DEBUG(4, "%s\n", telno);
	for(i = 0; i < 5; ++i) {	/* make 5 tries */
		/* wake up Vadic */
		write(dh, "\005", 1);
		sleep(1);
		write(dh, "\r", 1);
		DEBUG(4, "wanted * ", CNULL);
		ok = expect("*~5", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

		write(dh, "D\r", 2); /* "D" (enter number) command */
		DEBUG(4, "wanted NUMBER?\\r\\n ", CNULL);
		ok = expect("NUMBER?\r\n~5", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

		/* send telno, send \r */
		write(dh, telno, strlen(telno));
		sleep(1);
		write(dh, "\r", 1);
		DEBUG(4, "wanted %s ", telno);
		ok = expect(telno, dh);
		if (ok == 0)
			ok = expect("\r\n", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

		write(dh, "\r", 1); /* confirm number */
		DEBUG(4, "wanted DIALING: ", CNULL);
		ok = expect("DIALING: ", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok == 0)
			break;
	}

	if (ok == 0) {
		sleep(10 + delay);	/* give vadic some time */
		DEBUG(4, "wanted ON LINE\\r\\n ", CNULL);
		ok = expect("ON LINE\r\n", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
	}

	if (ok != 0) {
		if (dh > 2)
			close(dh);
		DEBUG(4, "vadDial failed\n", CNULL);
		delock(dev->D_line);
		return CF_DIAL;
	}
	DEBUG(4, "vadic ok\n", CNULL);
	return dh;
}

vadcls(fd)
{
	if (fd > 0) {
		close(fd);
		sleep(5);
		delock(devSel);
	}
}
