#ifndef lint
static char sccsid[] = "@(#)va212.c	4.3 (Berkeley) %G%";
#endif

#include "../condevs.h"

va212opn(telno, flds, dev)
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
		DEBUG(1, "timeout va212 open\n", 0);
		logent("va212 open", "TIMEOUT");
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
		DEBUG (4, errno == 4 ? "%s: no carrier\n" : "%s: can't open\n",
		dcname);
		delock(dev->D_line);
		return errno == 4 ? CF_DIAL : CF_NODEV;
	}
	fixline(dh, dev->D_speed);

	/* translate - to K for Vadic */
	DEBUG(4, "calling %s -> ", telno);
	delay = 0;
	for (i = 0; i < strlen(telno); ++i) {
		switch(telno[i]) {
		case '=':	/* await dial tone */
		case '-':	/* delay */
		case '<':
			telno[i] = 'K';
			delay += 5;
			break;
		}
	}
	DEBUG(4, "%s\n", telno);
	for(i = 0; i < TRYCALLS; ++i) {	/* make TRYCALLS tries */
		/* wake up Vadic */
		sendthem("\005\\d", dh);
		DEBUG(4, "wanted * ", CNULL);
		ok = expect("*", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

		sendthem("D\\d", dh);	/* "D" (enter number) command */
		DEBUG(4, "wanted NUMBER? ", CNULL);
		ok = expect("NUMBER?", dh);
		if (ok == 0)
			ok = expect("\n", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

		/* send telno, send \r */
		sendthem(telno, dh);
		DEBUG(4, "wanted %s ", telno);
		ok = expect(telno, dh);
		if (ok == 0)
			ok = expect("\n", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

		sendthem("", dh); /* confirm number */
		DEBUG(4, "wanted %s ", "DIALING...");
		ok = expect("DIALING...", dh);
		if (ok == 0) {
			ok = expect("\n", dh);
			DEBUG(4, "wanted ANSWER TONE", CNULL);
			ok = expect("ANSWER TONE", dh);
			if (ok == 0)
				ok = expect("\n", dh);
		}
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok == 0)
			break;
	}

	if (ok == 0) {
		DEBUG(4, "wanted ON LINE\\r\\n ", CNULL);
		ok = expect("ON LINE\r\n", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
	}

	if (ok != 0) {
		sendthem("I\\d", dh);	/* back to idle */
		if (dh > 2)
			close(dh);
		DEBUG(4, "vadDial failed\n", CNULL);
		delock(dev->D_line);
		return CF_DIAL;
	}
	DEBUG(4, "va212 ok\n", 0);
	return dh;
}

va212cls(fd)
{
	if (fd > 0) {
		close(fd);
		sleep(5);
		delock(devSel);
	}
}
