#ifndef lint
static char sccsid[] = "@(#)pen.c	4.3 (Berkeley) %G%";
#endif

/*
 *	Speaker's quick and dirty penril hack.  STA 4/1/85.
 */
#include "../condevs.h"

penopn(telno, flds, dev)
char *flds[], *telno;
struct Devices *dev;
{
	int	dh;
	int	i, ok = -1;
	char dcname[20];

	sprintf(dcname, "/dev/%s", dev->D_line);
	if (setjmp(Sjbuf)) {
		DEBUG(1, "timeout penril open\n", "");
		logent("penril open", "TIMEOUT");
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
	next_fd = -1;
	if (dh < 0) {
		DEBUG(4,"%s\n", errno == 4 ? "no carrier" : "can't open modem");
		delock(dev->D_line);
		return errno == 4 ? CF_DIAL : CF_NODEV;
	}

	/* modem is open */
	fixline(dh, dev->D_speed);

	/* translate - to P and = to W for Penril */
	DEBUG(4, "calling %s -> ", telno);
	for (i = 0; i < strlen(telno); ++i) {
		switch(telno[i]) {
		case '-':	/* delay */
			telno[i] = 'P';
			break;
		case '=':	/* await dial tone */
			telno[i] = 'W';
			break;
		case '<':
			telno[i] = 'P';
			break;
		}
	}
	DEBUG(4, "%s\n", telno);
	sleep(1);
	for(i = 0; i < 5; ++i) {	/* make up to 5 tries */
		slowrite(dh, "\r");/* awake, thou lowly Penril! */

		DEBUG(4, "wanted %s ", ">");
		ok = expect(">", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;
		slowrite(dh, "K");	/* "K" (enter number) command */
		DEBUG(4, "wanted %s ", "NO.: ");
		ok = expect("NO.: ", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok == 0)
			break;
	}

	if (ok == 0) {
		slowrite(dh, telno); /* send telno, send \r */
		slowrite(dh, "\r");
		DEBUG(4, "wanted %s ", "OK");
		ok = expect("OK", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
	}
	if (ok != 0) {
		if (dh > 2)
			close(dh);
		DEBUG(4, "penDial failed\n", "");
		return CF_DIAL;
	}
	else
		DEBUG(4, "penDial ok\n", "");
	return dh;
}

pencls(fd)
int fd;
{
	if (fd > 0) {
		close(fd);
		sleep(5);
		delock(devSel);
	}
}
