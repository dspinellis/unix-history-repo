static char sccsid[] = "@(#)dial.c	4.1	(Berkeley)	9/11/82";

/***********************************************************************
* dial: do the stuff for the various autodiallers                            *
***********************************************************************/

#include "uucp.h"
#include <signal.h>
#include <sgtty.h>
#include <setjmp.h>
#include <ctype.h>
#include <utmp.h>
#include <sys/types.h>
#include <sys/timeb.h>

static char SiD[] = "@(#)dial  1.0";

#define	MODEMWAIT	5

int alarmtr();

/*****************************************************************************
* decdial: dial a DEC DF-03                                                  *
*****************************************************************************/

decdial(dev, phone, speed)
char *dev, *phone;
int speed;
{
	printf("DECDIAL %s on %s at %d\n", phone, dev, speed);
	return(-1);
}

/*
 * Code to handle Vadic AD3451P's
 * Note: this assumes sendthem() terminates messages with CR, not NL
 */

vad3451P(dev, phone, speed, line)
char *dev, *phone, *line;
{
	int dcf, i;
	int nw, ng;
	extern errno;
	register char *p;
	int delay, bad;
	char sendnum[32];
	char *strcat(), *strcpy();

	/* check phone number, calculate dialing time */
	delay = 5+10;	/* 5 sec dial tone delay, 10 sec for carrier det. */
	bad = 0;
	for (p = phone; *p != '\0'; ++p)
		switch (*p) {
		case '0': case '1': case '2': case '3': case '4':
		case '5': case '6': case '7': case '8': case '9':
			delay += 2;	/* up to 1 sec for digit, 3/4 sec */
			break;		/* interdigit space */
		case 'k':
		case 'K':
			delay += 5;	/* 5 sec additional delay */
			break;
		case ' ':
		case '-':
			break;
		default:
			++bad;
		}

	if (bad || strlen(phone) > sizeof(sendnum)-3) {
		logent(phone, "BAD PHONE NUMBER");
		DEBUG(4, "BAD PHONE NUMBER %s\n", phone);
		return(FAIL);
	}

	if ((dcf = open(dev, 2)) < 0) {
		logent(dev, "OPEN FAILED");
		DEBUG(4, "OPEN FAILED, errno=%d\n", errno);
		return(FAIL);
	}
	fixline(dcf, speed);

	/* get the modem's attention and set its speed */
	sendthem("\005\\d", dcf);
	if (expect("HELLO: I'M READY\r\n*", dcf, MODEMWAIT))
		goto fail;

	sendthem("D\\d", dcf);
	if (expect("NUMBER?\r\n", dcf, MODEMWAIT))
		goto fail;

	/* build phone number sent to the modem */
	strcpy(sendnum, phone);
	for (p = sendnum; *p != '\0'; ++p)
		if (*p == 'k')
			*p = 'K';		/* echo is upper case */
	sendthem(sendnum, dcf);
	strcat(sendnum, "\r\n");
	if (expect(sendnum, dcf, MODEMWAIT))
		goto fail;
	sendthem("", dcf);
	if (expect("DIALING:  ", dcf, MODEMWAIT))
		goto fail;
	if (expect("ON LINE\r\n", dcf, delay)) {
		logent(dev, "DIAL FAILED");
		DEBUG(4, "%s DIAL FAILED\n", dev);
		sendthem("I", dcf);
		close(dcf);
		return(FAIL);
	}
	return(dcf);

fail:
	logent(dev, "DIALER PROBLEM");
	DEBUG(4, "%s DIALER PROBLEM\n", dev);
	sendthem("I", dcf);
	close(dcf);
	return(FAIL);
}

ven212(dev, phone, speed)
char *dev, *phone;
int speed;
{
	int dcf;
	int realflags;
	struct sgttyb ttbuf;
 /*     static struct TTY_delays vtdelays = { 0, 0205, 0, 0 };  */
 /*     static struct TTY_delays nodelays = { 0, 0, 0, 0 };     */
	static struct timeb loctime;
        char *erc = "error code %d\n";
	extern errno;

	dcf = open(dev, 2);
	if(dcf < 0) {
		logent(dev, "OPEN FAILED");
		DEBUG(4, "OPEN %d FAILED\n", errno);
		return(FAIL);
	}
	/* set speeds, etc. */
	fixline(dcf, speed);
	/* now doctor the speeds */
	ASSERT(!ioctl(dcf, TIOCGETP, &ttbuf), erc, errno);
	realflags = ttbuf.sg_flags;
	ttbuf.sg_flags = ODDP|EVENP|CBREAK;
	ASSERT(!ioctl(dcf, TIOCSETN, &ttbuf), erc, errno);
	/* IS-1 crockola
	ASSERT(!ioctl(dcf, TIOCSDEL, &vtdelays), erc, errno);
	*/

	/* now try to get its attention */
	DEBUG(4, "POKE VENTEL, ", 0);

	/* IS-1 way
	write(dcf, "\r\r", 2);
	*/
	/* Yale way */
	ftime(&loctime);
	write(dcf, "\r",1);
	{register i = loctime.millitm;
	   while (abs(loctime.millitm - i)<250) ftime(&loctime);
	}
        write(dcf, "\r",1);

        DEBUG(4, "IS VENTEL THERE?\n", 0);
	if(expect("\r\n$", dcf, MODEMWAIT)) {
dead:
		DEBUG(4, "DIAL DEAD %s\n", dev);
		logent(dev, "DIALLER DEAD");
		close(dcf);
		return(FAIL);
	}

	/* now dial the number */
	ioctl(dcf, TIOCSETP, &ttbuf);    /* discard buffered stuff */
	write(dcf, "K", 1);
	if(expect("DIAL: ", dcf, MODEMWAIT))
		goto dead;

	write(dcf, phone, strlen(phone));
	write(dcf, "\r", 1);
	if(expect("ONLINE!", dcf, 60))	/* should be calculated delay */
		goto dead;

	/* have connection */
	ttbuf.sg_flags = realflags;
	ioctl(dcf, TIOCSETN, &ttbuf);
	/*      IS-1
	ioctl(dcf, TIOCSDEL, &nodelays);
	*/
        return(dcf);
}

/*****************************************************************************
* disable and reenable:   allow a single line to be turned around so as to   *
* work both for dialin and dialout.                                          *
*****************************************************************************/

/* Yale: Disable this crock for now */

char enbdev[30];        /* disabled device */

disable(dev)
char *dev;
{
	int i;
	struct utmp u;

	return;                 /* Yale */
/*      DEBUG(4, "Disable %s\n", dev);
/*        i = open("/etc/utmp", 0);
/*        if(i < 0)
/*                return;         /* tough */
/*        while(read(i, &u, sizeof(u)) == sizeof(u)) {
/*                if(strcmp(u.ut_line, dev))
/*                        continue;
/*                if(u.ut_stat == ENABLE) {
/*                        DEBUG(4, "Was enabled: %s\n", dev);
/*                          enbcall("disable", dev);
/*                        logent(dev, "DISABLED LOGIN");
/*                        strcpy(enbdev, dev);
/*                        break;
/*                }
/*        }
/*        close(i);
*/
}

reenable()
{
	return;                 /* Yale */
/*        if(!enbdev[0])
/*                return;         /* nothing to reenable */
/*        DEBUG(4, "Reenable %s\n", enbdev);
/*        enbcall("enable", enbdev);
/*        logent(enbdev, "REENABLED LOGIN");
/*        enbdev[0] = 0;
*/
}

/* enbcall(type, dev)
/* char *type;
/* char *dev;
/* {
/*	int pid;
/*
/*	if((pid = fork()) == 0) {
/*		execl("/priv/enable", type, dev, 0);
/*		exit(99);
/*	}
/*	while(wait(0) != pid);
/*}
*/
