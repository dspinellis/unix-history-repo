#ifndef lint
static char sccsid[] = "@(#)hysq.c	4.2 (Berkeley) 2/24/88";
#endif

#include "../condevs.h"

/*
 * New dialout routine to work with Hayes' SMART MODEM
 * 13-JUL-82, Mike Mitchell
 * Modified 23-MAR-83 to work with Tom Truscott's (rti!trt)
 * version of UUCP	(ncsu!mcm)
 *
 * The modem should be set to NOT send any result codes to
 * the system (switch 3 up, 4 down). This end will figure out
 * what is wrong.
 *
 * I had lots of problems with the modem sending
 * result codes since I am using the same modem for both incomming and
 * outgoing calls.  I'd occasionally miss the result code (getty would
 * grab it), and the connect would fail.  Worse yet, the getty would
 * think the result code was a user name, and send garbage to it while
 * it was in the command state.  I turned off ALL result codes, and hope
 * for the best.  99% of the time the modem is in the correct state.
 * Occassionally it doesn't connect, or the phone was busy, etc., and
 * uucico sits there trying to log in.  It eventually times out, calling
 * clsacu() in the process, so it resets itself for the next attempt.
 */

/*
 * NOTE: this version is not for the faint-hearted.
 * Someday it would be nice to have a single version of hayes dialer
 * with a way to specify the switch settings that are on the dialer
 * as well as tone/pulse.
 * In the meantime, using HAYES rather than HAYESQ is probably best.
 */

hysqpopn(telno, flds, dev)
char *telno, *flds[];
struct Devices *dev;
{
	return hysqopn(telno, flds, dev, 0);
}

hysqtopn(telno, flds, dev)
char *telno, *flds[];
struct Devices *dev;
{
	return hysqopn(telno, flds, dev, 1);
}

hysqopn(telno, flds, dev, toneflag)
char *telno, *flds[];
struct Devices *dev;
int toneflag;
{
	char dcname[20], phone[MAXPH+10], c = 0;
#ifdef	USG
	struct termio ttbuf;
#endif USG
	int status, dnf;
	unsigned timelim;

	signal(SIGALRM, alarmtr);
	sprintf(dcname, "/dev/%s", dev->D_line);

	getnextfd();
	if (setjmp(Sjbuf)) {
		logent("DEVICE", "NO");
		DEBUG(4, "Open timed out %s", dcname);
		return CF_NODEV;
	}
	alarm(10);

	if ((dnf = open(dcname, 2)) <= 0) {
		logent("DEVICE", "NO");
		DEBUG(4, "Can't open %s", dcname);
		return CF_NODEV;
	}

	alarm(0);
	next_fd = -1;
	fixline(dnf, dev->D_speed);
	DEBUG(4, "Hayes port - %s, ", dcname);

	if (toneflag)
		sprintf(phone, "\rATDT%s\r", telno);
	else
		sprintf(phone, "\rATDP%s\r", telno);

	write(dnf, phone, strlen(phone));

	/* calculate delay time for the other system to answer the phone.
	 * Default is 15 seconds, add 2 seconds for each comma in the phone
	 * number.
	 */
	timelim = 150;
	while(*telno) {
		c = *telno++;
		if (c == ',')
			timelim += 20;
		else if (toneflag)
			timelim += 2;	/* .2 seconds per tone */
		else {
			if (c == '0') timelim += 10;   /* .1 sec per digit */
			else if (c > '0' && c <= '9')
				timelim += (c - '0');
		}
	}
	alarm(timelim/10 + 1);
	if (setjmp(Sjbuf) == 0) {
		read(dnf, &c, 1);
		alarm(0);
	}

	return dnf;
}

hysqcls(fd)
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
		write(fd, "\rATZ\r", 5);
		close(fd);
		delock(devSel);
	}
}
