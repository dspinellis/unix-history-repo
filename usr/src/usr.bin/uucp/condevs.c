#ifndef lint
static char sccsid[] = "@(#)condevs.c	5.6 (Berkeley) 8/12/83";
#endif

/*
 * Here are various dialers to establish the machine-machine connection.
 * conn.c/condevs.c was glued together by Mike Mitchell.
 * The dialers were supplied by many people, to whom we are grateful.
 *
 * ---------------------------------------------------------------------
 * NOTE:
 * There is a bug that occurs at least on PDP11s due to a limitation of
 * setjmp/longjmp.   If the routine that does a setjmp is interrupted
 * and longjmp-ed to,  it loses its register variables (on a pdp11).
 * What works is if the routine that does the setjmp
 * calls a routine and it is the *subroutine* that is interrupted.
 * 
 * Anyway, in conclusion, condevs.c is plagued with register variables
 * that are used inside
 * 	if (setjmp(...)) {
 * 		....
 * 	}
 * 
 * THE FIX: In dnopn(), for example, delete the 'register' Devices *dev.
 * (That was causing a core dump; deleting register fixed it.)
 * Also for dnopn delete 'register' int dnf... .
 * In pkopn, delete 'register' flds... .
 * There may be others, especially mcm's version of hysopen.
 * You could just delete all references to register, that is safest.
 * This problem might not occur on 4.1bsd, I am not sure.
 * 	Tom Truscott
 */
#include <sys/types.h>
#include <errno.h>
#include <setjmp.h>
#include <signal.h>
#include <sgtty.h>
#include "uucp.h"

extern char devSel[];	/* name to pass to delock() in close */
extern int errno, next_fd;
extern jmp_buf Sjbuf;
extern int alarmtr();
int nulldev(), nodev(), Acuopn(), diropn(), dircls();

#ifdef DATAKIT
int dkopn();
#endif
#ifdef DN11
int dnopn(), dncls();
#endif
#ifdef HAYES
int hysopn(), hyscls();
#endif
#ifdef HAYESQ
int hysqopn(), hysqcls();  /* a version of hayes that doesn't use ret codes */
#endif
#ifdef DF02
int df2opn(), df2cls();
#endif
#ifdef PNET
int pnetopn();
#endif
#ifdef VENTEL
int ventopn(), ventcls();
#endif
#ifdef	UNET
#include <UNET/unetio.h>
#include <UNET/tcp.h>
int unetopn(), unetcls();
#endif UNET
#ifdef VADIC
int vadopn(), vadcls();
#endif VADIC
#ifdef	RVMACS
int rvmacsopn(), rvmacscls();
#endif
#ifdef MICOM
int micopn(), miccls();
#endif MICOM

struct condev condevs[] = {
{ "DIR", "direct", diropn, nulldev, dircls },
#ifdef DATAKIT
{ "DK", "datakit", dkopn, nulldev, nulldev },
#endif
#ifdef PNET
{ "PNET", "pnet", pnetopn, nulldev, nulldev },
#endif
#ifdef	UNET
{ "UNET", "UNET", unetopn, nulldev, unetcls },
#endif UNET
#ifdef MICOM
{ "MICOM", "micom", micopn, nulldev, miccls },
#endif MICOM
#ifdef DN11
{ "ACU", "dn11", Acuopn, dnopn, dncls },
#endif
#ifdef HAYES
{ "ACU", "hayes", Acuopn, hysopn, hyscls },
#endif HAYES
#ifdef HAYESQ	/* a version of hayes that doesn't use result codes */
{ "ACU", "hayesq", Acuopn, hysqopn, hysqcls },
#endif HATESQ
#ifdef DF02
{ "ACU", "DF02", Acuopn, df2opn, df2cls },
#endif
#ifdef VENTEL
{ "ACU", "ventel", Acuopn, ventopn, ventcls },
#endif VENTEL
#ifdef VADIC
{ "ACU", "vadic", Acuopn, vadopn, vadcls },
#endif VADIC
#ifdef RVMACS
{ "ACU", "rvmacs", Acuopn, rvmacsopn, rvmacscls },
#endif RVMACS

/* Insert new entries before this line */
{ NULL, NULL, NULL, NULL, NULL } };

/***
 *	nulldev		a null device (returns CF_DIAL)
 */
int nulldev()
{
	return(CF_DIAL);
}

/***
 *	nodev		a null device (returns CF_NODEV)
 */
int nodev()
{
	return(CF_NODEV);
}


/*
 * The first things in this file are the generic devices. 
 * Generic devices look through L-devices and call the CU_open routines for
 * appropriate devices.  Some things, like the Unet interface, or direct
 * connect, do not use the CU_open entry.  ACUs must search to find the'
 * right routine to call.
 */

/***
 *	diropn(flds)	connect to hardware line
 *	char *flds[];
 *
 *	return codes:
 *		>0  -  file number  -  ok
 *		FAIL  -  failed
 */

diropn(flds)
register char *flds[];
{
	register int dcr, status;
	struct Devices dev;
	char dcname[20];
	FILE *dfp;
	dfp = fopen(DEVFILE, "r");
	ASSERT(dfp != NULL, "CAN'T OPEN", DEVFILE, 0);
	while ((status = rddev(dfp, &dev)) != FAIL) {
		if (strcmp(flds[F_CLASS], dev.D_class) != SAME)
			continue;
		if (strcmp(flds[F_PHONE], dev.D_line) != SAME)
			continue;
		if (mlock(dev.D_line) != FAIL)
			break;
	}
	fclose(dfp);
	if (status == FAIL) {
		logent("DEVICE", "NO");
		return(CF_NODEV);
	}

	sprintf(dcname, "/dev/%s", dev.D_line);
	if (setjmp(Sjbuf)) {
		delock(dev.D_line);
		return(FAIL);
	}
	signal(SIGALRM, alarmtr);
	alarm(10);
	getnextfd();
	errno = 0;
	dcr = open(dcname, 2); /* read/write */
	next_fd = -1;
	if (dcr < 0 && errno == EACCES)
		logent(dcname, "CAN'T OPEN");
	alarm(0);
	if (dcr < 0) {
		delock(dev.D_line);
		return(FAIL);
	}
	fflush(stdout);
	fixline(dcr, dev.D_speed);
	strcpy(devSel, dev.D_line);	/* for latter unlock */
	CU_end = dircls;
	return(dcr);
}

dircls(fd)
register int fd;
{
	if (fd > 0) {
		close(fd);
		delock(devSel);
		}
	}

#ifdef DATAKIT

#include <dk.h>
#define DKTRIES 2

/***
 *	dkopn(flds)	make datakit connection
 *
 *	return codes:
 *		>0 - file number - ok
 *		FAIL - failed
 */

dkopn(flds)
char *flds[];
{
	int dkphone;
	register char *cp;
	register ret, i;

	if (setjmp(Sjbuf))
		return(FAIL);

	signal(SIGALRM, alarmtr);
	dkphone = 0;
	cp = flds[F_PHONE];
	while(*cp)
		dkphone = 10 * dkphone + (*cp++ - '0');
	DEBUG(4, "dkphone (%d) ", dkphone);
	for (i = 0; i < DKTRIES; i++) {
		getnextfd();
		ret = dkdial(D_SH, dkphone, 0);
		next_fd = -1;
		DEBUG(4, "dkdial (%d)\n", ret);
		if (ret > -1)
			break;
	}
	return(ret);
}
#endif

#ifdef PNET
/***
 *	pnetopn(flds)
 *
 *	call remote machine via Purdue network
 *	use dial string as host name, speed as socket number
 * Author: Steve Bellovin
 */

pnetopn(flds)
char *flds[];
{
	int fd;
	int socket;
	register char *cp;

	fd = pnetfile();
	DEBUG(4, "pnet fd - %d\n", fd);
	if (fd < 0) {
		logent("AVAILABLE DEVICE", "NO");
		return(CF_NODEV);
	}
	socket = 0;
	for (cp = flds[F_CLASS]; *cp; cp++)
		socket = 10*socket + (*cp - '0');
	DEBUG(4, "socket - %d\n", socket);
	if (setjmp(Sjbuf)) {
		DEBUG(4, "pnet timeout  - %s\n", flds[F_PHONE]);
		return(FAIL);
	}
	signal(SIGALRM, alarmtr);
	DEBUG(4, "host - %s\n", flds[F_PHONE]);
	alarm(15);
	if (pnetscon(fd, flds[F_PHONE], socket) < 0) {
		DEBUG(4, "pnet connect failed - %s\n", flds[F_PHONE]);
		return(FAIL);
	}
	alarm(0);
	return(fd);
}
#endif	PNET

#ifdef UNET
/***
 *	unetopn -- make UNET (tcp-ip) connection
 *
 *	return codes:
 *		>0 - file number - ok
 *		FAIL - failed
 */

/* Default port of uucico server */
#define	DFLTPORT	33

unetopn(flds)
register char *flds[];
{
	register int ret, port;
	int unetcls();

	port = atoi(flds[F_PHONE]);
	if (port <= 0 || port > 255)
		port = DFLTPORT;
	DEBUG(4, "unetopn host %s, ", flds[F_NAME]);
	DEBUG(4, "port %d\n", port);
	if (setjmp(Sjbuf)) {
		logent("tcpopen", "TIMEOUT");
		endhnent();	/* see below */
		return(CF_DIAL);
	}
	signal(SIGALRM, alarmtr);
	alarm(30);
	ret = tcpopen(flds[F_NAME], port, 0, TO_ACTIVE, "rw");
	alarm(0);
	endhnent();	/* wave magic wand at 3com and incant "eat it, bruce" */
	if (ret < 0) {
		DEBUG(5, "tcpopen failed: errno %d\n", errno);
		logent("tcpopen", "FAILED");
		return(CF_DIAL);
	}
	CU_end = unetcls;
	return(ret);
}

/*
 * unetcls -- close UNET connection.
 */
unetcls(fd)
register int fd;
{
	DEBUG(4, "UNET CLOSE called\n", 0);
	if (fd > 0) {
		/* disable this until a timeout is put in
		if (ioctl(fd, UIOCCLOSE, STBNULL))
			logent("UNET CLOSE", "FAILED");
		 */
		close(fd);
		DEBUG(4, "closed fd %d\n", fd);
	}
}
#endif UNET

#ifdef MICOM

/*
 *	micopn: establish connection through a micom.
 *	Returns descriptor open to tty for reading and writing.
 *	Negative values (-1...-7) denote errors in connmsg.
 *	Be sure to disconnect tty when done, via HUPCL or stty 0.
 */
micopn(flds)
register char *flds[];
{
	extern errno;
	char *rindex(), *fdig(), dcname[20];
	int dh, ok = 0, speed;
	register struct condev *cd;
	register FILE *dfp;
	struct Devices dev;

	dfp = fopen(DEVFILE, "r");
	ASSERT(dfp != NULL, "Can't open", DEVFILE, 0);

	signal(SIGALRM, alarmtr);
	dh = -1;
	for(cd = condevs; ((cd->CU_meth != NULL)&&(dh < 0)); cd++) {
		if (snccmp(flds[F_LINE], cd->CU_meth) == SAME) {
			fseek(dfp, (off_t)0, 0);
			while(rddev(dfp, &dev) != FAIL) {
				if (strcmp(flds[F_CLASS], dev.D_class) != SAME)
					continue;
				if (snccmp(flds[F_LINE], dev.D_type) != SAME)
					continue;
				if (mlock(dev.D_line) == FAIL)
					continue;

				sprintf(dcname, "/dev/%s", dev.D_line);
				getnextfd();
				alarm(10);
				if (setjmp(Sjbuf)) {
					delock(dev.D_line);
					logent(dev.D_line,"micom open TIMEOUT");
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
			}
		}
	fclose(dfp);
	if (dh < 0)
		return(CF_NODEV);

	speed = atoi(fdig(flds[F_CLASS]));
	fixline(dh, speed);
	sleep(1);

	/* negotiate with micom */
	if (speed != 4800)	/* damn their eyes! */
		write(dh, "\r", 1);
	else
		write(dh, " ", 1);
		
	DEBUG(4, "wanted %s ", "NAME");
	ok = expect("NAME", dh);
	DEBUG(4, "got %s\n", ok ? "?" : "that");
	if (ok == 0) {
		write(dh, flds[F_PHONE], strlen(flds[F_PHONE]));
		sleep(1);
		write(dh, "\r", 1);
		DEBUG(4, "wanted %s ", "GO");
		ok = expect("GO", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
	}

	if (ok != 0) {
		if (dh > 2)
			close(dh);
		DEBUG(4, "micom failed\n", "");
		delock(dev.D_line);
		return(CF_DIAL);
	} else
		DEBUG(4, "micom ok\n", "");

	CU_end = cd->CU_clos;
	strcat(devSel, dev.D_line);	/* for later unlock */
	return(dh);

}

miccls(fd)
register int fd;
{

	if (fd > 0) {
		close(fd);
		delock(devSel);
		}
	}
#endif MICOM

/***
 *	Acuopn - open an ACU and dial the number.  The condevs table
 *		will be searched until a dialing unit is found that is
 *		free.
 *
 *	return codes:	>0 - file number - o.k.
 *			FAIL - failed
 */

char devSel[20];	/* used for later unlock() */

Acuopn(flds)
register char *flds[];
{
    char phone[MAXPH+1];
    register struct condev *cd;
    register int fd;
    register FILE *dfp;
    struct Devices dev;

    exphone(flds[F_PHONE], phone);
    devSel[0] = '\0';
    DEBUG(4, "Dialing %s\n", phone);
    dfp = fopen(DEVFILE, "r");
    ASSERT(dfp != NULL, "Can't open", DEVFILE, 0);

    for(cd = condevs; cd->CU_meth != NULL; cd++) {
	if (snccmp(flds[F_LINE], cd->CU_meth) == SAME) {
	    fseek(dfp, (off_t)0, 0);
	    while(rddev(dfp, &dev) != FAIL) {
		if (strcmp(flds[F_CLASS], dev.D_class) != SAME)
		    continue;
		if (snccmp(flds[F_LINE], dev.D_type) != SAME)
		    continue;
		if (dev.D_brand[0] == '\0')
		    logent("Acuopn","No 'brand' name on ACU");
		else if (snccmp(dev.D_brand, cd->CU_brand) != SAME)
		    continue;
		if (mlock(dev.D_line) == FAIL)
		    continue;

		DEBUG(4, "Using %s\n", cd->CU_brand);
		fd = (*(cd->CU_open))(phone, flds, &dev);
		if (fd > 0) {
		    CU_end = cd->CU_clos;   /* point CU_end at close func */
		    fclose(dfp);
		    strcpy(devSel, dev.D_line);   /* save for later unlock() */
		    return(fd);
		    }
		delock(dev.D_line);
		}
	    }
	}
    fclose(dfp);
    return(FAIL);
    }

#ifdef DN11

/***
 *	dnopn(ph, flds, dev)	dial remote machine
 *	char *ph;
 *	char *flds[];
 *	struct Devices *dev;
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
#ifdef	SYSIII
	struct termio ttbuf;
#endif
	int dnf, dcf;
	int nw, lt, pid, status;
	unsigned timelim;

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
		return(CF_NODEV);
		}
	/* rti!trt: avoid passing acu file descriptor to children */
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
		return(FAIL);
	}
	signal(SIGALRM, alarmtr);
	timelim = 5 * strlen(phone);
	alarm(timelim < 30 ? 30 : timelim);
	if ((pid = fork()) == 0) {
		sleep(2);
		fclose(stdin);
		fclose(stdout);
#ifdef	TIOCFLUSH
		ioctl(dnf, TIOCFLUSH, STBNULL);
#endif
		nw = write(dnf, phone, lt = strlen(phone));
		if (nw != lt) {
			logent("DIALUP ACU write", "FAILED");
			exit(1);
		}
		DEBUG(4, "ACU write ok%s\n", "");
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
		logent("DIALUP LINE open", "FAILED");
		alarm(0);
		kill(pid, 9);
		close(dnf);
		delock(dev->D_line);
		return(FAIL);
	}
	/* brl-bmd.351 (Doug Kingston) says the next ioctl is unneeded . */
/*	ioctl(dcf, TIOCHPCL, STBNULL);*/
	while ((nw = wait(&lt)) != pid && nw != -1)
		;
#ifdef	SYSIII
	ioctl(dcf, TCGETA, &ttbuf);
	if(!(ttbuf.c_cflag & HUPCL)) {
		ttbuf.c_cflag |= HUPCL;
		ioctl(dcf, TCSETA, &ttbuf);
	}
#endif
	alarm(0);
	fflush(stdout);
	fixline(dcf, dev->D_speed);
	DEBUG(4, "Fork Stat %o\n", lt);
	if (lt != 0) {
		close(dcf);
		if (dnf)
			close(dnf);
		delock(dev->D_line);
		return(FAIL);
	}
	return(dcf);
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

#ifdef DF02
/***
 *	df2opn(ph, flds, dev)	dial remote machine
 *	char *ph;
 *	char *flds[];
 *	struct Devices *dev;
 *
 *	return codes:
 *		file descriptor  -  succeeded
 *		FAIL  -  failed
 *
 *	Modified 9/28/81 by Bill Shannon (DEC)
 *	Changed to use DEC DF02 or DF03 ACU
 */


df2opn(ph, flds, dev)
char *ph;
char *flds[];
struct Devices *dev;
{
	char dcname[20], dnname[20], phone[MAXPH+2], c = 0;
#ifdef	SYSIII
	struct termio ttbuf;
#endif
	int dcf, dnf;
	int nw, lt, pid, st, status;
	unsigned timelim;

	sprintf(dnname, "/dev/%s", dev->D_calldev);
	if (setjmp(Sjbuf)) {
		logent(dnname, "CAN'T OPEN");
		DEBUG(4, "%s Open timed out\n", dnname);
		return(CF_NODEV);
	}
	signal(SIGALRM, alarmtr);
	getnextfd();
	errno = 0;
	alarm(10);
	dnf = open(dnname, 2 );
	alarm(0);
	next_fd = -1;
	if (dnf < 0 && errno == EACCES) {
		logent(dnname, "CAN'T OPEN");
		delock(dev->D_line);
		logent("DEVICE", "NO");
		return(CF_NODEV);
		}
	/* rti!trt: avoid passing acu file descriptor to children */
	fioclex(dnf);

	sprintf(dcname, "/dev/%s", dev->D_line);
	fixline(dnf, dev->D_speed);
	sprintf(phone, "\02%s", ph);
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
		return(FAIL);
	}
	signal(SIGALRM, alarmtr);
	timelim = 5 * strlen(phone);
	alarm(timelim < 30 ? 30 : timelim);
	if ((pid = fork()) == 0) {
		sleep(2);
		fclose(stdin);
		fclose(stdout);
#ifdef	TIOCFLUSH
		ioctl(dnf, TIOCFLUSH, STBNULL);
#endif
		write(dnf, "\01", 1);
		sleep(1);
		nw = write(dnf, phone, lt = strlen(phone));
		if (nw != lt) {
			logent("DIALUP ACU write", "FAILED");
			exit(1);
		}
		DEBUG(4, "ACU write ok%s\n", "");
		exit(0);
	}
	/*  open line - will return on carrier */
	/* RT needs a sleep here because it returns immediately from open */

#if RT
	sleep(15);
#endif

	if (read(dnf, &c, 1) != 1 || c != 'A')
		dcf = -1;
	else
		dcf = 0;
	DEBUG(4, "dcf is %d\n", dcf);
	if (dcf < 0) {
		logent("DIALUP LINE open", "FAILED");
		alarm(0);
		kill(pid, 9);
		close(dnf);
		delock(dev->D_line);
		return(FAIL);
	}
	dcf = dnf;
	dnf = 0;
	/* brl-bmd.351 (Doug Kingston) says the next ioctl is unneeded . */
/*	ioctl(dcf, TIOCHPCL, STBNULL);*/
	while ((nw = wait(&lt)) != pid && nw != -1)
		;
#ifdef	SYSIII
	ioctl(dcf, TCGETA, &ttbuf);
	if(!(ttbuf.c_cflag & HUPCL)) {
		ttbuf.c_cflag |= HUPCL;
		ioctl(dcf, TCSETA, &ttbuf);
	}
#endif
	alarm(0);
	fflush(stdout);
	fixline(dcf, dev->D_speed);
	DEBUG(4, "Fork Stat %o\n", lt);
	if (lt != 0) {
		close(dcf);
		if (dnf)
			close(dnf);
		delock(dev->D_line);
		return(FAIL);
	}
	return(dcf);
}

/*
 * df2cls()	close the DF02/DF03 call unit
 *
 *	return codes: none
 */

df2cls(fd)
register int fd;
{
	if (fd > 0) {
		close(fd);
		sleep(5);
		delock(devSel);
		}
}
#endif DF02

#ifdef HAYES
/***
 *	hysopn(telno, flds, dev) connect to hayes smartmodem
 *	char *flds[], *dev[];
 *
 *	return codes:
 *		>0  -  file number  -  ok
 *		CF_DIAL,CF_DEVICE  -  failed
 */
/*
 * Define HAYSTONE if you have touch tone dialing.
 */
/*#define HAYSTONE	*/

hysopn(telno, flds, dev)
char *telno;
char *flds[];
struct Devices *dev;
{
	int	dh = -1;
	extern errno;
	char dcname[20];

	sprintf(dcname, "/dev/%s", dev->D_line);
	DEBUG(4, "dc - %s\n", dcname);
	if (setjmp(Sjbuf)) {
		DEBUG(1, "timeout hayes open %s\n", dcname);
		logent("hayes open", "TIMEOUT");
		if (dh >= 0)
			close(dh);
		delock(dev->D_line);
		return(CF_DIAL);
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
#ifdef HAYSTONE
		write(dh, "\rATDT", 5);
#else
		write(dh, "\rATDP", 5);
#endif
		write(dh, telno, strlen(telno));
		write(dh, "\r", 1);

		if (expect("CONNECT", dh) != 0) {
			logent("HSM no carrier", "FAILED");
			strcpy(devSel, dev->D_line);
			hyscls(dh);
			return(CF_DIAL);
		}

	}
	if (dh < 0) {
		DEBUG(4, "hayes failed\n", "");
		delock(dev->D_line);
	}
	DEBUG(4, "hayes ok\n", "");
	return(dh);
}

hyscls(fd)
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

#endif HAYES

#ifdef HAYESQ
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
 * Define HAYSTONE if touch-tone dialing is to be used.  If it is not defined,
 * Pulse dialing is assumed.
 */
/*#define HAYSTONE*/

hysqopn(telno, flds, dev)
char *telno, *flds[];
struct Devices *dev;
{
	char dcname[20], phone[MAXPH+10], c = 0;
#ifdef	SYSIII
	struct termio ttbuf;
#endif
	int status, dnf;
	unsigned timelim;

	signal(SIGALRM, alarmtr);
	sprintf(dcname, "/dev/%s", dev->D_line);

	getnextfd();
	if (setjmp(Sjbuf)) {
		delock(dev->D_line);
		logent("DEVICE", "NO");
		DEBUG(4, "Open timed out %s", dcname);
		return(CF_NODEV);
		}
	alarm(10);

	if ((dnf = open(dcname, 2)) <= 0) {
		delock(dev->D_line);
		logent("DEVICE", "NO");
		DEBUG(4, "Can't open %s", dcname);
		return(CF_NODEV);
		}

	alarm(0);
	next_fd = -1;
	fixline(dnf, dev->D_speed);
	DEBUG(4, "Hayes port - %s, ", dcname);

#ifdef HAYSTONE
	sprintf(phone, "\rATDT%s\r", telno);
#else
	sprintf(phone, "\rATDP%s\r", telno);
#endif

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
		else {
#ifdef HAYSTONE
			timelim += 2;	/* .2 seconds per tone */
			}
#else
			if (c == '0') timelim += 10;   /* .1 second per digit */
			else if (c > '0' && c <= '9')
				timelim += (c - '0');
			}
#endif
		}
	alarm(timelim/10);
	if (setjmp(Sjbuf) == 0) {
		read(dnf, &c, 1);
		alarm(0);
		}

	return(dnf);
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

#endif HAYESQ

#ifdef	VENTEL
ventopn(telno, flds, dev)
char *flds[], *telno;
struct Devices *dev;
{
	int	dh;
	int	i, ok = -1;
	char dcname[20];

	sprintf(dcname, "/dev/%s", dev->D_line);
	if (setjmp(Sjbuf)) {
		DEBUG(1, "timeout ventel open\n", "");
		logent("ventel open", "TIMEOUT");
		if (dh >= 0)
			close(dh);
		delock(dev->D_line);
		return(CF_NODEV);
	}
	signal(SIGALRM, alarmtr);
	getnextfd();
	alarm(10);
	dh = open(dcname, 2);
	next_fd = -1;
	if (dh < 0) {
		DEBUG(4,"%s\n", errno == 4 ? "no carrier" : "can't open modem");
		delock(dev->D_line);
		return(errno == 4 ? CF_DIAL : CF_NODEV);
	}

	/* modem is open */
	fixline(dh, dev->D_speed);

	/* translate - to % and = to & for VenTel */
	DEBUG(4, "calling %s -> ", telno);
	for (i = 0; i < strlen(telno); ++i) {
		switch(telno[i]) {
		case '-':	/* delay */
			telno[i] = '%';
			break;
		case '=':	/* await dial tone */
			telno[i] = '&';
			break;
		case '<':
			telno[i] = '%';
			break;
		}
	}
	DEBUG(4, "%s\n", telno);
	sleep(1);
	for(i = 0; i < 5; ++i) {	/* make up to 5 tries */
 		slowrite(dh, "\r\r");/* awake, thou lowly VenTel! */

		DEBUG(4, "wanted %s ", "$");
		ok = expect("$", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;
 		slowrite(dh, "K");	/* "K" (enter number) command */
		DEBUG(4, "wanted %s ", "DIAL: ");
		ok = expect("DIAL: ", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok == 0)
			break;
	}

	if (ok == 0) {
 		slowrite(dh, telno); /* send telno, send \r */
 		slowrite(dh, "\r");
		DEBUG(4, "wanted %s ", "ONLINE");
		ok = expect("ONLINE!", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
	}
	if (ok != 0) {
		if (dh > 2)
			close(dh);
		DEBUG(4, "venDial failed\n", "");
		return(CF_DIAL);
	} else
		DEBUG(4, "venDial ok\n", "");
	return(dh);
}


/*
 * uucpdelay:  delay execution for numerator/denominator seconds.
 */

#ifdef INTERVALTIMER
#define uucpdelay(num,denom) intervaldelay(1000000*num/denom)
#include <sys/time.h>
catch alarm sig
SIGALRM
struct itimerval itimerval;
itimerval.itimer_reload =
itimerval.rtime.itimer_interval =
itimerval.rtime.itimer_value =
settimer(ITIMER_REAL, &itimerval);
pause();
alarm comes in
turn off timer.
#endif INTERVALTIMER

#ifdef FASTTIMER
#define uucpdelay(num,denom) nap(60*num/denom)
/*	Sleep in increments of 60ths of second.	*/
nap (time)
	register int time;
{
	static int fd;

	if (fd == 0)
		fd = open (FASTTIMER, 0);

	read (fd, 0, time);
}
#endif FASTTIMER

#ifdef FTIME
#define uucpdelay(num,denom) ftimedelay(1000*num/denom)
#include <sys/timeb.h>
ftimedelay(n)
{
	static struct timeb loctime;
	ftime(&loctime);
	{register i = loctime.millitm;
	   while (abs((int)(loctime.millitm - i))<n) ftime(&loctime);
	}
}
#endif FTIME

#ifdef BUSYLOOP
#define uucpdelay(num,denom) busyloop(CPUSPEED*num/denom)
#define CPUSPEED 1000000	/* VAX 780 is 1MIPS */
#define	DELAY(n)	{ register long N = (n); while (--N > 0); }
busyloop(n)
	{
	DELAY(n);
	}
#endif BUSYLOOP

slowrite(fd, str)
register char *str;
{
	DEBUG(6, "slowrite ", "");
	while (*str) {
		DEBUG(6, "%c", *str);
		uucpdelay(1,10);	/* delay 1/10 second */
		write(fd, str, 1);
		str++;
		}
	DEBUG(6, "\n", "");
}


ventcls(fd)
int fd;
{

	if (fd > 0) {
		close(fd);
		sleep(5);
		delock(devSel);
		}
}
#endif VENTEL

#ifdef VADIC

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
		return(CF_NODEV);
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
		return(CF_NODEV);
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
	for(i = 0; i < 5; ++i) {	/* make 5 tries */
		/* wake up Vadic */
		sendthem("\005\\d", dh);
		DEBUG(4, "wanted %s ", "*");
		ok = expect("*", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

		sendthem("D\\d", dh);	/* "D" (enter number) command */
		DEBUG(4, "wanted %s ", "NUMBER?\\r\\n");
		ok = expect("NUMBER?\r\n", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

	/* send telno, send \r */
		sendthem(telno, dh);
		ok = expect(telno, dh);
		if (ok == 0)
			ok = expect("\r\n", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok != 0)
			continue;

		sendthem("", dh); /* confirm number */
		DEBUG(4, "wanted %s ", "DIALING: ");
		ok = expect("DIALING: ", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
		if (ok == 0)
			break;
	}

	if (ok == 0) {
		sleep(10 + delay);	/* give vadic some time */
		DEBUG(4, "wanted ON LINE\\r\\n ", 0);
		ok = expect("ON LINE\r\n", dh);
		DEBUG(4, "got %s\n", ok ? "?" : "that");
	}

	if (ok != 0) {
		sendthem("I\\d", dh);	/* back to idle */
		if (dh > 2)
			close(dh);
		DEBUG(4, "vadDial failed\n", "");
		delock(dev->D_line);
		return(CF_DIAL);
	}
	DEBUG(4, "vadic ok\n", "");
	return(dh);
}

vadcls(fd) {

	if (fd > 0) {
		close(fd);
		sleep(5);
		delock(devSel);
		}
	}

#endif VADIC

#ifdef	RVMACS
/*
 * Racal-Vadic 'RV820' MACS system with 831 adaptor.
 * A typical 300 baud L-devices entry is
 *	ACU /dev/tty10 /dev/tty11,48 300 rvmacs
 * where tty10 is the communication line (D_Line),
 * tty11 is the dialer line (D_calldev),
 * the '4' is the dialer address + modem type (viz. dialer 0, Bell 103),
 * and the '8' is the communication port (they are 1-indexed).
 * BUGS:
 * Only tested with one dialer, one modem
 * uses common speed for dialer and communication line.
 * UNTESTED
 */

#define	STX	02	/* Access Adaptor */
#define	ETX	03	/* Transfer to Dialer */
#define	SI	017	/* Buffer Empty (end of phone number) */
#define	SOH	01	/* Abort */

rvmacsopn(ph, flds, dev)
char *ph, *flds[];
struct Devices *dev;
{
	register int va, i, child;
	register char *p;
	char c, acu[20], com[20];

	child = -1;
	if ((p = index(dev->D_calldev, ',')) == NULL) {
		DEBUG(2, "No dialer/modem specification\n", 0);
		goto failret;
	}
	*p++ = '\0';
	if (setjmp(Sjbuf)) {
		logent("rvmacsopn", "TIMEOUT");
		i = CF_DIAL;
		goto ret;
	}
	DEBUG(4, "STARTING CALL\n", 0);
	sprintf(acu, "/dev/%s", dev->D_calldev);
	getnextfd();
	signal(SIGALRM, alarmtr);
	alarm(30);
	if ((va = open(acu, 2)) < 0) {
		logent(acu, "CAN'T OPEN");
		i = CF_NODEV;
		goto ret;
	}
	fixline(va, dev->D_speed);

	p_chwrite(va, STX);	/* access adaptor */
	i = *p++ - '0';
	if (i < 0 || i > 7) {
		logent(p-1, "Bad dialer address/modem type\n");
		goto failret;
	}
	p_chwrite(va, i);		/* Send Dialer Address Digit */
	i = *p - '0';
	if (i <= 0 || i > 14) {
		logent(p-1, "Bad modem address\n");
		goto failret;
	}
	p_chwrite(va, i-1);	/* Send Modem Address Digit */
	write(va, ph, strlen(ph));	/* Send Phone Number */
	p_chwrite(va, SI);	/* Send Buffer Empty */
	p_chwrite(va, ETX);	/* Initiate Call */
	sprintf(com, "/dev/%s", dev->D_line);

	/* create child to open comm line */
	if ((child = fork()) == 0) {
		signal(SIGINT, SIG_DFL);
		open(com, 0);
		sleep(5);
		exit(1);
	}

	if (read(va, &c, 1) != 1) {
		logent("ACU READ", "FAILED");
		goto failret;
	}
	switch(c) {
	case 'A':
		/* Fine! */
		break;
	case 'B':
		DEBUG(2, "CALL ABORTED\n", 0);
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
			logent("dialup open", "FAILED");
		goto failret;
	}
	fixline(i, dev->D_speed);
	goto ret;
failret:
	i = CF_DIAL;
ret:
	alarm(0);
	if (child != -1)
		kill(child, SIGKILL);
	close(va);
	return(i);
}

rvmacscls(fd)
register int fd;
{
	DEBUG(2, "MACS close %d\n", fd);
	p_chwrite(fd, SOH);
/*	ioctl(fd, TIOCCDTR, NULL);*/
	close(fd);
}
#endif
