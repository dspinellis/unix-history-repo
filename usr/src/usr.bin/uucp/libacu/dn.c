#ifndef lint
static char sccsid[] = "@(#)dn.c	4.2 (Berkeley) %G%";
#endif

#include "../condevs.h"
#ifdef DN11
#define ACULAST "-<"

/***
 *	dnopn(ph, flds, dev)	dial remote machine
 *
 *	return codes:
 *		file descriptor  -  succeeded
 *		FAIL  -  failed
 */
dnopn(ph, flds, dev)
char *ph;
char *flds[];
struct Devices *dev;
{
	char dcname[20], dnname[20], phone[MAXPH+2], c = 0;
#ifdef	USG
	struct termio ttbuf;
#endif 	USG
	int dnf, dcf;
	int nw, lt, pid, status;
	unsigned timelim;
#ifdef TIOCFLUSH
	int zero = 0;
#endif TIOCFLUSH

	sprintf(dnname, "/dev/%s", dev->D_calldev);
	errno = 0;

	if (setjmp(Sjbuf)) {
		logent(dnname, "CAN'T OPEN");
		DEBUG(4, "%s Open timed out\n", dnname);
		return(CF_NODEV);
	}
	signal(SIGALRM, alarmtr);
	getnextfd();
	alarm(10);
	dnf = open(dnname, 1);
	alarm(0);
	next_fd = -1;
	if (dnf < 0 && errno == EACCES) {
		logent(dnname, "CAN'T OPEN");
		logent("DEVICE", "NO");
		return CF_NODEV;
	}
	fioclex(dnf);

	sprintf(dcname, "/dev/%s", dev->D_line);
	sprintf(phone, "%s%s", ph, ACULAST);
	DEBUG(4, "dc - %s, ", dcname);
	DEBUG(4, "acu - %s\n", dnname);
	pid = 0;
	if (setjmp(Sjbuf)) {
		logent("DIALUP DN write", "TIMEOUT");
		if (pid)
			kill(pid, 9);
		delock(dev->D_line);
		if (dnf)
			close(dnf);
		return CF_DIAL;
	}
	signal(SIGALRM, alarmtr);
	timelim = 5 * strlen(phone);
	alarm(timelim < 30 ? 30 : timelim);
	if ((pid = fork()) == 0) {
		sleep(2);
		fclose(stdin);
		fclose(stdout);
#ifdef	TIOCFLUSH
		ioctl(dnf, TIOCFLUSH, &zero);
#endif	TIOCFLUSH
		nw = write(dnf, phone, lt = strlen(phone));
		if (nw != lt) {
			logent("DIALUP ACU write", _FAILED);
			exit(1);
		}
		DEBUG(4, "ACU write ok\n", CNULL);
		exit(0);
	}
	/*  open line - will return on carrier */
	/* RT needs a sleep here because it returns immediately from open */

#if RT
	sleep(15);
#endif

	getnextfd();
	errno = 0;
	dcf = open(dcname, 2);
	next_fd = -1;
	if (dcf < 0 && errno == EACCES)
		logent(dcname, "CAN'T OPEN");
	DEBUG(4, "dcf is %d\n", dcf);
	if (dcf < 0) {
		logent("DIALUP LINE open", _FAILED);
		alarm(0);
		kill(pid, 9);
		close(dnf);
		delock(dev->D_line);
		return CF_DIAL;
	}
	while ((nw = wait(&lt)) != pid && nw != -1)
		;
#ifdef	USG
	ioctl(dcf, TCGETA, &ttbuf);
	if(!(ttbuf.c_cflag & HUPCL)) {
		ttbuf.c_cflag |= HUPCL;
		ioctl(dcf, TCSETA, &ttbuf);
	}
#endif USG
	alarm(0);
	fflush(stdout);
	fixline(dcf, dev->D_speed);
	DEBUG(4, "Fork Stat %o\n", lt);
	if (lt != 0) {
		close(dcf);
		if (dnf)
			close(dnf);
		delock(dev->D_line);
		return CF_DIAL;
	}
	return dcf;
}

/***
 *	dncls()		close dn type call unit
 *
 *	return codes:	None
 */
dncls(fd)
register int fd;
{
	if (fd > 0) {
		close(fd);
		sleep(5);
		delock(devSel);
	}
}
#endif DN11
