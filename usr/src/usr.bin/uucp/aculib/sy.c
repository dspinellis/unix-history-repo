#ifndef lint
static char sccsid[] = "@(#)sy.c	4.2 (Berkeley) 2/24/88";
#endif

#include "../condevs.h"

#ifdef SYTEK

/*
 *	sykopn: establish connection through a sytek port.
 *	Returns descriptor open to tty for reading and writing.
 *	Negative values (-1...-7) denote errors in connmsg.
 *	Will try to change baud rate of local port to match
 *	that of the remote side.
 */
char sykspeed[50];	/* speed to reset to on close */

sykopn(flds)
register char *flds[];
{
	extern errno;
	char *rindex(), *fdig(), dcname[20];
	int dh, ok = 0, speed;
	register FILE *dfp;
	struct Devices dev;
	char speedbuf[50];

	dfp = fopen(DEVFILE, "r");
	ASSERT(dfp != NULL, "Can't open", DEVFILE, 0);

	signal(SIGALRM, alarmtr);
	dh = -1;
	while(rddev(dfp, &dev) != FAIL) {
/* we'll set our own speed; F_CLASS is how cynthia configures it every night
		if (strcmp(flds[F_CLASS], dev.D_class) != SAME)
			continue;
*/
		if (snccmp(flds[F_LINE], dev.D_type) != SAME)
			continue;
		if (mlock(dev.D_line) == FAIL)
			continue;

		sprintf(dcname, "/dev/%s", dev.D_line);
		getnextfd();
		alarm(10);
		if (setjmp(Sjbuf)) {
			delock(dev.D_line);
			logent(dev.D_line,"sytek open TIMEOUT");
			dh = -1;
			break;
			}
		dh = open(dcname, 2);
		alarm(0);
		next_fd = -1;
		if (dh > 0) {
			break;
			}
		devSel[0] = '\0';
		delock(dev.D_line);
	}
	fclose(dfp);
	if (dh < 0)
		return(CF_NODEV);

	speed = atoi(fdig(dev.D_class));
	fixline(dh, speed);
	sleep(1);

	/* negotiate with sytek */
	genbrk(dh, 3);
		
	DEBUG(4, "wanted %s ", "#");
	ok = expect("#", dh);
	DEBUG(4, "got %s\n", ok ? "?" : "that");
	if(ok != 0){
		if(atoi(fdig(dev.D_class)) == 9600){
			fixline(dh, 2400);
			speed = 2400;
		} else {
			fixline(dh, 9600);
			speed = 9600;
		}
		sleep(1);
		genbrk(dh, 3);
		ok = expect("#", dh);
		if(ok){
			close(dh);
			DEBUG(4, "sytek BREAK failed\n", "");
			delock(dev.D_line);
			return(CF_DIAL);
		}
	}
	write(dh, "done \r", 6);
	ok = expect("#", dh);
	DEBUG(4, "got %s\n", ok ? "?" : "that");
	if(speed != atoi(fdig(flds[F_CLASS]))){
		DEBUG(4, "changing speed\n", "");
		sprintf(speedbuf, "baud %s\r", fdig(flds[F_CLASS]));
		write(dh, speedbuf, strlen(speedbuf));
		sleep(1);
		speed = atoi(fdig(flds[F_CLASS]));
		fixline(dh, speed);
		genbrk(dh, 3);
		ok = expect("#", dh);
		DEBUG(4, "speed set %s\n", ok ? "failed" : flds[F_CLASS]);
	}
	strcpy(sykspeed, dev.D_class);
	write(dh, "command break\r", 14);
	ok = expect("#", dh);
	DEBUG(4, "got %s\n", ok ? "?" : "that");
	if (ok == 0) {
		write(dh, "call ", 5);
		write(dh, flds[F_PHONE], strlen(flds[F_PHONE]));
		write(dh, "\r", 1);
		DEBUG(4, "sytek dial %s\n", flds[F_PHONE]);
		DEBUG(4, "wanted %s ", "COMPLETED TO ");
		ok = expect("COMPLETED TO ", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
	}

	if (ok != 0) {
		close(dh);
		DEBUG(4, "sytek failed\n", "");
		delock(dev.D_line);
		return(CF_DIAL);
	} else
		DEBUG(4, "sytek ok\n", "");

	CU_end = sykcls;
	strcpy(devSel, dev.D_line);	/* for later unlock */
	return(dh);

}

sykcls(fd)
register int fd;
{
	register int ok, speed;


	if (fd > 0) {
		genbrk(fd, 3);
		ok = expect("#", fd);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if(ok != 0){
			genbrk(fd, 3);
			ok = expect("#", fd);
		}
		if(ok == 0){
			write(fd, "done 1\r", 7);
			ok = expect("#", fd);
			DEBUG(4, "got %s\n", ok ? "?" : "that");
			DEBUG(4, "reset baud to %s\n", sykspeed);
			write(fd, "baud ", 5);
			write(fd, sykspeed, strlen(sykspeed));
			write(fd, "\r", 1);
			sleep(1);
		}
		close(fd);
		delock(devSel);
	}
}
#endif SYTEK
