#ifndef lint
static char sccsid[] = "@(#)cico.c	5.3 (Berkeley) 10/3/83";
#endif

#include "uucp.h"
#include <signal.h>
#include <setjmp.h>
#include <sys/types.h>
#ifdef	SYSIII
#include <termio.h>
#endif
#ifndef	SYSIII
#include <sgtty.h>
#endif


#ifdef UNET
#include <UNET/unetio.h>
#include <UNET/tcp.h>
static struct uiocstate ust;
#endif

jmp_buf Sjbuf;
	/*  call fail text  */
char *Stattext[] = {
	"",
	"BAD SYSTEM",
	"WRONG TIME",
	"SYSTEM LOCKED",
	"NO DEVICE",
	"DIAL FAILED",
	"LOGIN FAILED",
	"BAD SEQUENCE"
	};

int Role = 0;
	/*  call fail codes  */
int Stattype[] = {0, 0, 0, 0,
	SS_NODEVICE, SS_FAIL, SS_FAIL, SS_BADSEQ
	};


int Errorrate = 0;
#ifdef	SYSIII
struct termio Savettyb;
#endif
#ifndef	SYSIII
struct sgttyb Savettyb;
#endif

/*******
 *	cico - this program is used  to place a call to a
 *	remote machine, login, and copy files between the two machines.
 */

main(argc, argv)
register char *argv[];
{
	register int ret;
	int seq;
	int onesys = 0;
	char wkpre[NAMESIZE], file[NAMESIZE];
	char msg[BUFSIZ], *q;
	register char *p;
	extern onintr(), timeout(), setdebug();
	extern intrEXIT();
	extern char *pskip();
	char rflags[30];
	char *ttyn;
	int orig_uid = getuid();

	strcpy(Progname, "uucico");
	uucpname(Myname);

	/* Try to run as uucp -- rti!trt */
	setgid(getegid());
	setuid(geteuid());

	signal(SIGILL, intrEXIT);
	signal(SIGTRAP, intrEXIT);
	signal(SIGIOT, intrEXIT);
	signal(SIGEMT, intrEXIT);
	signal(SIGFPE, intrEXIT);
	signal(SIGBUS, intrEXIT);
	signal(SIGSEGV, intrEXIT);
	signal(SIGSYS, intrEXIT);
	signal(SIGINT, onintr);
	signal(SIGHUP, onintr);
	signal(SIGQUIT, onintr);
	signal(SIGTERM, onintr);
	signal(SIGPIPE, onintr);	/* 4.1a tcp-ip stupidity */
	signal(SIGFPE, setdebug);
	ret = guinfo(getuid(), User, msg);
	strcpy(Loginuser, User);
	ASSERT(ret == 0, "BAD UID ", "", ret);

	rflags[0] = '\0';
	umask(WFMASK);
	strcpy(Rmtname, Myname);
	Ifn = Ofn = -1;
	while(argc>1 && argv[1][0] == '-'){
		switch(argv[1][1]){
		case 'd':
			Spool = &argv[1][2];
			break;
#ifdef PROTODEBUG
		case 'E':
			Errorrate = atoi(&argv[1][2]);
			if (Errorrate <= 0)
				Errorrate = 100;
			break;
		case 'g':
			Pkdrvon = 1;
			break;
		case 'G':
			Pkdrvon = 1;
			strcat(rflags, " -g ");
			break;
#endif
		case 'r':
			Role = atoi(&argv[1][2]);
			break;
		case 's':
			sprintf(Rmtname, "%.7s", &argv[1][2]);
			if (Rmtname[0] != '\0')
				onesys = 1;
			break;
		case 'x':
			chkdebug(orig_uid);
			Debug = atoi(&argv[1][2]);
			if (Debug <= 0)
				Debug = 1;
			strcat(rflags, argv[1]);
			logent("ENABLED", "DEBUG");
			break;
		default:
			printf("unknown flag %s\n", argv[1]);
			break;
		}
		--argc;  argv++;
	}

	subchdir(Spool);
	strcpy(Wrkdir, Spool);

#ifdef	UNET
/*
 * Determine if we are on UNET
 */
	ret = ioctl(0, UIOCSTATE, &ust);
	if (ret == 0) {
		Unet = 1;
		DEBUG(4, "UNET connection -- ioctl-s disabled\n", "");
	}
#endif
	if (Role == SLAVE) {
		/* initial handshake */
		onesys = 1;
		if (!Unet) {
#ifdef	SYSIII
			ret = ioctl(0, TCGETA, &Savettyb);
			Savettyb.c_cflag = (Savettyb.c_cflag & ~CS8) | CS7;
			Savettyb.c_oflag |= OPOST;
			Savettyb.c_lflag |= (ISIG|ICANON|ECHO);
#endif
#ifndef	SYSIII
			ret = ioctl(0, TIOCGETP, &Savettyb);
			Savettyb.sg_flags |= ECHO;
			Savettyb.sg_flags &= ~RAW;
#endif
		}
		Ifn = 0;
		Ofn = 1;
		fixmode(Ifn);
		fclose(stderr);
		fopen(RMTDEBUG, "w");
		omsg('S', "here", Ofn);
		signal(SIGALRM, timeout);
		alarm(MAXMSGTIME);
		if (setjmp(Sjbuf)) {
			/* timed out */
			if (!Unet) {
#ifdef	SYSIII
				ret = ioctl(0, TCSETA, &Savettyb);
#endif
#ifndef	SYSIII
				ret = ioctl(0, TIOCSETP, &Savettyb);
#endif
			}
			exit(0);
		}
		for (;;) {
			ret = imsg(msg, Ifn);
			if (ret != 0) {
				alarm(0);
				if (!Unet) {
#ifdef	SYSIII
					ret = ioctl(0, TCSETA, &Savettyb);
#endif
#ifndef	SYSIII
					ret = ioctl(0, TIOCSETP, &Savettyb);
#endif
				}
				exit(0);
			}
			if (msg[0] == 'S')
				break;
		}
		alarm(0);
		q = &msg[1];
		p = pskip(q);
		sprintf(Rmtname, "%.7s", q);
		DEBUG(4, "sys-%s\n", Rmtname);
		if (mlock(Rmtname)) {
			omsg('R', "LCK", Ofn);
			cleanup(0);
		}
		else if (callback(Loginuser)) {
			signal(SIGINT, SIG_IGN);
			signal(SIGHUP, SIG_IGN);
			omsg('R', "CB", Ofn);
			logent("CALLBACK", "REQUIRED");
			/*  set up for call back  */
			systat(Rmtname, SS_CALLBACK, "CALL BACK");
			gename(CMDPRE, Rmtname, 'C', file);
			close(creat(subfile(file), 0666));
			xuucico(Rmtname);
			cleanup(0);
		}
		seq = 0;
		while (*p == '-') {
			q = pskip(p);
			switch(*(++p)) {
			case 'g':
				Pkdrvon = 1;
				break;
			case 'x':
				Debug = atoi(++p);
				if (Debug <= 0)
					Debug = 1;
				break;
			case 'Q':
				seq = atoi(++p);
				break;
			default:
				break;
			}
			p = q;
		}
		if (callok(Rmtname) == SS_BADSEQ) {
			logent("BADSEQ", "PREVIOUS");
			omsg('R', "BADSEQ", Ofn);
			cleanup(0);
		}
		if ((ret = gnxseq(Rmtname)) == seq) {
			omsg('R', "OK", Ofn);
			cmtseq();
		}
		else {
			systat(Rmtname, Stattype[7], Stattext[7]);
			logent("BAD SEQ", "HANDSHAKE FAILED");
			ulkseq();
			omsg('R', "BADSEQ", Ofn);
			cleanup(0);
		}
		ttyn = ttyname(Ifn);
		if (ttyn != NULL)
			chmod(ttyn, 0600);
	}
loop:
	if (!onesys) {
		ret = gnsys(Rmtname, Spool, CMDPRE);
		if (ret == FAIL)
			cleanup(100);
		if (ret == 0)
			cleanup(0);
	}
	else if (Role == MASTER && callok(Rmtname) != 0) {
		logent("SYSTEM STATUS", "CAN NOT CALL");
		cleanup(0);
	}

	sprintf(wkpre, "%c.%.7s", CMDPRE, Rmtname);

	if (Role == MASTER) {
		/*  master part */
		signal(SIGINT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
		signal(SIGQUIT, SIG_IGN);
		if (!iswrk(file, "chk", Spool, wkpre) && !onesys) {
			logent(Rmtname, "NO WORK");
			goto next;
		}
		if (Ifn != -1 && Role == MASTER) {
			write(Ofn, EOTMSG, strlen(EOTMSG));
			clsacu();
			close(Ofn);
			close(Ifn);
			Ifn = Ofn = -1;
			rmlock(CNULL);
			sleep(3);
		}
		sprintf(msg, "call to %s ", Rmtname);
		if (mlock(Rmtname) != 0) {
			logent(msg, "LOCKED");
			goto next;
		}
		Ofn = Ifn = conn(Rmtname);
		if (Ofn < 0) {
			logent(msg, "FAILED");
			systat(Rmtname, Stattype[-Ofn],
				Stattext[-Ofn]);
			goto next;
		}
		else {
			logent(msg, "SUCCEEDED");
		}
	
		if (setjmp(Sjbuf))
			goto next;
		signal(SIGALRM, timeout);
		alarm(2 * MAXMSGTIME);
		for (;;) {
			ret = imsg(msg, Ifn);
			if (ret != 0) {
				alarm(0);
				logent("imsg 1", "FAILED");
				goto next;
			}
			if (msg[0] == 'S')
				break;
		}
		alarm(MAXMSGTIME);
		seq = gnxseq(Rmtname);
		sprintf(msg, "%.7s -Q%d %s", Myname, seq, rflags);
		omsg('S', msg, Ofn);
		for (;;) {
			ret = imsg(msg, Ifn);
			DEBUG(4, "msg-%s\n", msg);
			if (ret != 0) {
				alarm(0);
				ulkseq();
				logent("imsg 2", "FAILED");
				goto next;
			}
			if (msg[0] == 'R')
				break;
		}
		alarm(0);
		if (msg[1] == 'B') {
			/* bad sequence */
			logent("BAD SEQ", "HANDSHAKE FAILED");
			systat(Rmtname, Stattype[7], Stattext[7]);
			ulkseq();
			goto next;
		}
		if (strcmp(&msg[1], "OK") != SAME)  {
			logent(&msg[1], "HANDSHAKE FAILED");
			ulkseq();
			goto next;
		}
		cmtseq();
	}
	DEBUG(1, " Rmtname %s, ", Rmtname);
	DEBUG(1, "Role %s,  ", Role ? "MASTER" : "SLAVE");
	DEBUG(1, "Ifn - %d, ", Ifn);
	DEBUG(1, "Loginuser - %s\n", Loginuser);

	alarm(MAXMSGTIME);
	if (setjmp(Sjbuf))
		goto Failure;
	ret = startup(Role);
	alarm(0);
	if (ret != SUCCESS) {
Failure:
		logent("startup", "FAILED");
		systat(Rmtname, SS_FAIL, "STARTUP");
		goto next;
	}
	else {
		logent("startup", "OK");
		systat(Rmtname, SS_INPROGRESS, "TALKING");
		ret = cntrl(Role, wkpre);
		DEBUG(1, "cntrl - %d\n", ret);
		signal(SIGINT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
		signal(SIGALRM, timeout);
		if (ret == 0) {
			logent("conversation complete", "OK");
			rmstat(Rmtname);

		}
		else {
			logent("conversation complete", "FAILED");
			systat(Rmtname, SS_FAIL, "CONVERSATION");
		}
		alarm(MAXMSGTIME);
		omsg('O', "OOOOO", Ofn);
		DEBUG(4, "send OO %d,", ret);
		if (!setjmp(Sjbuf)) {
			for (;;) {
				omsg('O', "OOOOO", Ofn);
				ret = imsg(msg, Ifn);
				if (ret != 0)
					break;
				if (msg[0] == 'O')
					break;
			}
		}
		alarm(0);
		clsacu();	/* rti!trt: is this needed? */
	}
next:
	if (!onesys) {
		goto loop;
	}
	cleanup(0);
}

#ifndef	SYSIII
struct sgttyb Hupvec;
#endif

/***
 *	cleanup(code)	cleanup and exit with "code" status
 *	int code;
 */

cleanup(code)
register int code;
{
	register int ret;
	register char *ttyn;

	signal(SIGINT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);
	rmlock(CNULL);
	clsacu();
	logcls();
	if (Role == SLAVE) {
		if (!Unet) {
#ifdef	SYSIII
			Savettyb.c_cflag |= HUPCL;
			ret = ioctl(0, TCSETA, &Savettyb);
#endif
#ifndef	SYSIII
			/* rti!trt:  use more robust hang up sequence */
			ret = ioctl(0, TIOCHPCL, STBNULL);
			ret = ioctl(0, TIOCGETP, &Hupvec);
			Hupvec.sg_ispeed = B0;
			Hupvec.sg_ospeed = B0;
			ret = ioctl(0, TIOCSETP, &Hupvec);
			sleep(2);
			ret = ioctl(0, TIOCSETP, &Savettyb);
#endif
			DEBUG(4, "ret ioctl - %d\n", ret);
		}
		ttyn = ttyname(Ifn);
		if (ttyn != NULL)
			chmod(ttyn, 0600);
	}
	if (Ofn != -1) {
		if (Role == MASTER)
			write(Ofn, EOTMSG, strlen(EOTMSG));
		close(Ifn);
		close(Ofn);
	}
	DEBUG(1, "exit code %d\n", code);
	if (code == 0)
		xuuxqt();
	exit(code);
}

/***
 *	onintr(inter)	interrupt - remove locks and exit
 */

onintr(inter)
register int inter;
{
	char str[30];
	signal(inter, SIG_IGN);
	sprintf(str, "SIGNAL %d", inter);
	logent(str, "CAUGHT");
	systat(Rmtname, SS_FAIL, str);
	cleanup(inter);
}

/* changed to single version of intrEXIT.  Is this okay? rti!trt */
intrEXIT(signo)
int signo;
{
#ifdef	SIGIO
	/* if using 4.2 signal mechanism, must unblock all signal handlers */
	sigsetmask(0);
#endif
	signal(signo, SIG_DFL);
	setgid(getgid());
	setuid(getuid());
	abort();
}
/*
 * Catch a special signal
 * (SIGFPE, ugh), and toggle debugging between 0 and 30.
 * Handy for looking in on long running uucicos.
 */
setdebug()
{
	if (Debug < 30)
		Debug = 30;
	else
		Debug = 0;
}


/***
 *	fixmode(tty)	fix kill/echo/raw on line
 *
 *	return codes:  none
 */

fixmode(tty)
register int tty;
{
#ifdef	SYSIII
	struct termio ttbuf;
#endif
#ifndef	SYSIII
	struct sgttyb ttbuf;
#endif
	register int ret;

	if (Unet)
		return;
#ifdef	SYSIII
	ioctl(tty, TCGETA, &ttbuf);
	ttbuf.c_iflag = ttbuf.c_oflag = ttbuf.c_lflag = (ushort)0;
	ttbuf.c_cflag &= (CBAUD);
	ttbuf.c_cflag |= (CS8|CREAD);
	ttbuf.c_cc[VMIN] = 6;
	ttbuf.c_cc[VTIME] = 1;
	ret = ioctl(tty, TCSETA, &ttbuf);
#endif
#ifndef	SYSIII
	ioctl(tty, TIOCGETP, &ttbuf);
	ttbuf.sg_flags = (ANYP | RAW);
	ret = ioctl(tty, TIOCSETP, &ttbuf);
#endif
	ASSERT(ret >= 0, "STTY FAILED", "", ret);
#ifndef	SYSIII
	ioctl(tty, TIOCEXCL, STBNULL);
#endif
}


/***
 *	timeout()	catch SIGALRM routine
 */

timeout()
{
	logent(Rmtname, "TIMEOUT");
	systat(Rmtname, SS_FAIL, "TIMEOUT");
	longjmp(Sjbuf, 1);
}

static char *
pskip(p)
register char *p;
{
	while( *p && *p != ' ' )
		++p;
	if( *p ) *p++ = 0;
	return(p);
}
