#ifndef lint
static char sccsid[] = "@(#)vmacs.c	4.5 (Berkeley) 2/24/88";
#endif

#include "../condevs.h"

/*
 *
 * A typical 300 baud L-devices entry is
 *	ACU /dev/tty10 /dev/tty11,48,1200 300 vmacs
 * where tty10 is the communication line (D_Line),
 * tty11 is the dialer line (D_calldev),
 * the '4' is the dialer address + modem type (viz. dialer 0, Bell 103),
 * and the '8' is the communication port
 * (Note: Based on RVMACS version for 820 dialer.  This version
 *  developed by Doug Kingston @ BRL, 13 December 83.)
 */

#define	SOH	01	/* Abort */
#define	STX	02	/* Access Adaptor */
#define	ETX	03	/* Transfer to Dialer */
#define	SI	017	/* Buffer Empty (end of phone number) */

vmacsopn(ph, flds, dev)
char *ph, *flds[];
struct Devices *dev;
{
	register int va, i, child;
	register char *p;
	char c, acu[20], com[20];
	char	modem, dialer;
	int	dialspeed;
	char c_STX = STX;
	char c_ETX = ETX;
	char c_SI = SI;
	char c_SOH = SOH;

	/* create child to open comm line */
	child = -1;
	sprintf(com, "/dev/%s", dev->D_line);
	if ((child = fork()) == 0) {
		signal(SIGINT, SIG_DFL);
		open(com, 0);
		DEBUG(5, "%s Opened.", com);
		sleep(5);
		exit(1);
	}

	if ((p = index(dev->D_calldev, ',')) == NULL) {
		DEBUG(2, "No dialer/modem specification\n", 0);
		goto failret;
	}
	*p++ = '\0';
	if (*p < '0' || *p > '7') {
		logent(p, "Bad dialer address/modem type");
		goto failret;
	}
	dialer = *p++;
	if (*p < '0' || *p > '>') {
		logent(p, "Bad modem address");
		goto failret;
	}
	modem = *p++;
	if (*p++ == ',')
		dialspeed = atoi (p);
	else
		dialspeed = dev->D_speed;
	if (setjmp(Sjbuf)) {
		logent("vmacsopn", "TIMEOUT");
		i = CF_DIAL;
		goto ret;
	}
	DEBUG(4, "STARTING CALL\n", 0);
	sprintf(acu, "/dev/%s", dev->D_calldev);
	getnextfd();
	signal(SIGALRM, alarmtr);
	alarm(60);
	if ((va = open(acu, 2)) < 0) {
		logent(acu, "CAN'T OPEN");
		i = CF_NODEV;
		goto ret;
	}
	DEBUG(5, "ACU %s opened.\n", acu);
	next_fd = -1;
	fixline(va, dialspeed);

	write(va, &c_SOH, 1);		/* abort, reset the dialer */
	do {
		if (read (va, &c, 1) != 1) {
			logent ("MACS initialization", _FAILED);
			goto failret;
		}
	} while ((c&0177) != 'B');
	DEBUG(5, "ACU initialized\n", 0);

	write(va, &c_STX, 1);		/* start text, access adaptor */
	write(va, &dialer, 1);		/* send dialer address digit */
	write(va, &modem, 1);		/* send modem address digit */
	for (p=ph; *p; p++) {
		if (*p == '=' || (*p >= '0' && *p <= '9'))
			write(va, p, 1);
	}
	write(va, &c_SI, 1);		/* send buffer empty */
	write(va, &c_ETX, 1);		/* end of text, initiate call */

	if (read(va, &c, 1) != 1) {
		logent("ACU READ", _FAILED);
		goto failret;
	}
	switch(c) {
	case 'A':
		/* Fine! */
		DEBUG(5, "Call connected\n", 0);
		break;
	case 'B':
		DEBUG(2, "Dialer Timeout or Abort\n", 0);
		goto failret;
	case 'D':
		DEBUG(2, "Dialer format error\n", 0);
		goto failret;
	case 'E':
		DEBUG(2, "Dialer parity error\n", 0);
		goto failret;
	case 'F':
		DEBUG(2, "Phone number too long\n", 0);
		goto failret;
	case 'G':
		DEBUG(2, "Busy signal\n", 0);
		goto failret;
	default:
		DEBUG(2, "Unknown MACS return code '%c'\n", i);
		goto failret;
	}
	/*
	 * open line - will return on carrier
	 */
	if ((i = open(com, 2)) < 0) {
		if (errno == EIO)
			logent("carrier", "LOST");
		else
			logent("dialup open", _FAILED);
		goto failret;
	}
	fixline(i, dev->D_speed);
	goto ret;
failret:
	i = CF_DIAL;
ret:
	alarm(0);
	if (child > 1)
		kill(child, SIGKILL);
	close(va);
	sleep(2);
	return i;
}

vmacscls(fd)
register int fd;
{
	char c_SOH = SOH;

	DEBUG(2, "MACS close %d\n", fd);
	write(fd, &c_SOH, 1);
/*	ioctl(fd, TIOCCDTR, NULL);*/
	close(fd);
}
