#ifndef lint
static char sccsid[] = "@(#)cico.c	5.8 (Berkeley) %G%";
#endif

#include <signal.h>
#include "uucp.h"
#include <setjmp.h>
#ifdef	USG
#include <termio.h>
#endif
#ifndef	USG
#include <sgtty.h>
#endif
#ifdef BSDTCP
#include <netdb.h>
#include <netinet/in.h>
#include <sys/socket.h>
#endif BSDTCP
#include <sys/stat.h>
#include "uust.h"
#include "uusub.h"

#if defined(VMS) && defined(BSDTCP)
#define NOGETPEER
#endif

#ifdef BSD2_9
#define NOGETPEER
#endif

jmp_buf Sjbuf;
jmp_buf Pipebuf;

/*  call fail text  */
char *Stattext[] = {
	"",
	"BAD SYSTEM",
	"WRONG TIME TO CALL",
	"SYSTEM LOCKED",
	"NO DEVICE",
	"DIAL FAILED",
	"LOGIN FAILED",
	"BAD SEQUENCE"
};

/*  call fail codes  */
int Stattype[] = {
	0,
	0,
	SS_WRONGTIME,
	0,
	SS_NODEVICE,
	SS_FAIL,
	SS_FAIL,
	SS_BADSEQ
};


int ReverseRole = 0;
int StdErrIsTty = 0;
int Role = SLAVE;
int onesys = 0;
int turntime = 30 * 60;	/* 30 minutes expressed in seconds */
extern int LocalOnly;
extern char MaxGrade, DefMaxGrade;
extern char Myfullname[];

#ifdef	USG
struct termio Savettyb;
#endif
#ifndef	USG
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
	char wkpre[NAMESIZE], file[NAMESIZE];
	char msg[MAXFULLNAME], *q, **alias;
	register char *p;
	extern onintr(), timeout(), setdebug();
	extern char *pskip();
	char rflags[MAXFULLNAME];
	char *ttyn;
#ifdef NOGETPEER
	u_long Hostnumber = 0;
#endif NOGETPEER

	strcpy(Progname, "uucico");

	signal(SIGINT, onintr);
	signal(SIGHUP, onintr);
	signal(SIGQUIT, onintr);
	signal(SIGTERM, onintr);
	signal(SIGPIPE, onintr);	/* 4.1a tcp-ip stupidity */
	signal(SIGFPE, setdebug);
	ret = guinfo(getuid(), User, msg);
	strcpy(Loginuser, User);
	uucpname(Myname);
	ASSERT(ret == 0, "BAD UID", CNULL, ret);

#ifdef BSD4_2
	setlinebuf(stderr);
#endif
	rflags[0] = '\0';
	umask(WFMASK);
	strcpy(Rmtname, Myname);
	Ifn = Ofn = -1;
	while(argc>1 && argv[1][0] == '-'){
		switch(argv[1][1]){
		case 'd':
			Spool = &argv[1][2];
			break;
		case 'g':
		case 'p':
			MaxGrade = DefMaxGrade = argv[1][2];
			break;
		case 'r':
			Role = atoi(&argv[1][2]);
			break;
		case 'R':
			ReverseRole++;
			Role = MASTER;
			break;
		case 's':
			strncpy(Rmtname, &argv[1][2], MAXBASENAME);
			Rmtname[MAXBASENAME] = '\0';
			if (Rmtname[0] != '\0')
				onesys = 1;
			break;
		case 'x':
			chkdebug();
			Debug = atoi(&argv[1][2]);
			if (Debug <= 0)
				Debug = 1;
			strcat(rflags, argv[1]);
			logent("ENABLED", "DEBUG");
			break;
		case 't':
			turntime = atoi(&argv[1][2])*60;/* minutes to seconds */
			break;
		case 'L':	/* local calls only */
			LocalOnly++;
			break;
#ifdef NOGETPEER
		case 'h':
			Hostnumber = inet_addr(&argv[1][2]);
			break;
#endif NOGETPEER
		default:
			printf("unknown flag %s (ignored)\n", argv[1]);
			break;
		}
		--argc;  argv++;
	}

	while (argc > 1) {
		printf("unknown argument %s (ignored)\n", argv[1]);
		--argc; argv++;
	}

	/* Try to run as uucp -- rti!trt */
	setgid(getegid());
	setuid(geteuid());
#ifdef	TIOCNOTTY
	/*
	 * detach uucico from controlling terminal
	 * to defend against rlogind sending us a SIGKILL (!!!)
	 */
	if (Role == MASTER && (ret = open("/dev/tty", 2)) >= 0) {
		ioctl(ret, TIOCNOTTY, STBNULL);
		close(ret);
	}
#endif TIOCNOTTY
#ifdef BSD4_2
	if (getpgrp(0) == 0) { /*We have no controlling terminal */
		setpgrp(0, getpid());
	}
#endif BSD4_2

	ret = subchdir(Spool);
	ASSERT(ret >= 0, "CHDIR FAILED", Spool, ret);
	strcpy(Wrkdir, Spool);

	if (Role == SLAVE) {
		/* check for /etc/nologin */
		ultouch();	/* sets nologinflag as a side effect */
		if (nologinflag) {
			logent(NOLOGIN, "UUCICO SHUTDOWN");
			if (Debug > 4)
				logent("DEBUGGING", "continuing anyway");
			else
				cleanup(1);
		}
#ifdef	TCPIP
		/*
		 * Determine if we are on TCPIP
		 */
		if (isatty(0) ==  0) {
			IsTcpIp = 1;
			DEBUG(4, "TCPIP connection -- ioctl-s disabled\n", CNULL);
		} else
			IsTcpIp = 0;
#endif TCPIP
		/* initial handshake */
		onesys = 1;
		if (!IsTcpIp) {
#ifdef	USG
			ret = ioctl(0, TCGETA, &Savettyb);
			Savettyb.c_cflag = (Savettyb.c_cflag & ~CS8) | CS7;
			Savettyb.c_oflag |= OPOST;
			Savettyb.c_lflag |= (ISIG|ICANON|ECHO);
#else !USG
			ret = ioctl(0, TIOCGETP, &Savettyb);
			Savettyb.sg_flags |= ECHO;
			Savettyb.sg_flags &= ~RAW;
#endif !USG
		}
		Ifn = 0;
		Ofn = 1;
		fixmode(Ifn);
		sprintf(file,"%s/%d", RMTDEBUG, getpid());
#ifdef VMS
		/* hold the version number down */
		unlink(file);
#endif VMS
		freopen(file, "w", stderr);
#ifdef BSD4_2
		setlinebuf(stderr);
#else  !BSD4_2
		setbuf(stderr, NULL);
#endif !BSD4_2
		sprintf(msg, "here=%s", Myfullname);
		omsg('S', msg, Ofn);
		signal(SIGALRM, timeout);
		alarm(MAXMSGTIME);
		if (setjmp(Sjbuf)) {
			/* timed out */
			if (!IsTcpIp) {
#ifdef	USG
				ret = ioctl(0, TCSETA, &Savettyb);
#else	!USG
				ret = ioctl(0, TIOCSETP, &Savettyb);
#endif !USG
			}
			cleanup(0);
		}
		for (;;) {
			ret = imsg(msg, Ifn);
			if (ret != 0) {
				alarm(0);
				if (!IsTcpIp) {
#ifdef	USG
					ret = ioctl(0, TCSETA, &Savettyb);
#else	!USG
					ret = ioctl(0, TIOCSETP, &Savettyb);
#endif !USG
				}
				cleanup(0);
			}
			if (msg[0] == 'S')
				break;
		}
		alarm(0);
		q = &msg[1];
		p = pskip(q);
		strncpy(Rmtname, q, MAXBASENAME);
		Rmtname[MAXBASENAME] = '\0';
		sprintf(wkpre,"%s/%s", RMTDEBUG, Rmtname);
		unlink(wkpre);
		if (link(file, wkpre) == 0)
			unlink(file);
		DEBUG(4, "sys-%s\n", Rmtname);
		/* The versys will also do an alias on the incoming name */
		if (versys(&Rmtname)) {
			/* If we don't know them, we won't talk to them... */
#ifdef	NOSTRANGERS
			logent(Rmtname, "UNKNOWN HOST");
			omsg('R', "You are unknown to me", Ofn);
			cleanup(0);
#endif	NOSTRANGERS
		}
#ifdef BSDTCP
		/* we must make sure they are really who they say they
		 * are. We compare the hostnumber with the number in the hosts
		 * table for the site they claim to be.
		 */
		if (IsTcpIp) {
			struct hostent *hp;
			char *cpnt, *inet_ntoa();
			int fromlen;
			struct sockaddr_in from;

#ifdef	NOGETPEER
			from.sin_addr.s_addr = Hostnumber;
			from.sin_family = AF_INET;
#else	!NOGETPEER
			fromlen = sizeof(from);
			if (getpeername(0, &from, &fromlen) < 0) {
				logent(Rmtname, "NOT A TCP CONNECTION");
				omsg('R', "NOT TCP", Ofn);
				cleanup(0);
			}
#endif	!NOGETPEER
			hp = gethostbyaddr(&from.sin_addr,
				sizeof (struct in_addr), from.sin_family);
			if (hp == 0) {
				/* security break or just old host table? */
				logent(Rmtname, "UNKNOWN IP-HOST Name =");
				cpnt = inet_ntoa(from.sin_addr),
				logent(cpnt, "UNKNOWN IP-HOST Number =");
				sprintf(wkpre, "%s/%s isn't in my host table",
					Rmtname, cpnt);
				omsg('R' ,wkpre ,Ofn);
				cleanup(0);
			}
			if (Debug>99)
				logent(Rmtname,"Request from IP-Host name =");
			/* The following is to determine if the name given us by
			 * the Remote uucico matches any of the names(aliases)
			 * given its network number (remote machine) in our
			 * host table.
			 */
			if (strncmp(q, hp->h_name, SYSNSIZE) == 0) {
				if (Debug > 99)
					logent(q,"Found in host Tables");
			} else { /* Scan The host aliases */
				for(alias=hp->h_aliases; *alias!=0 &&
				    strncmp(q, *alias, SYSNSIZE) != 0; ++alias)
					;
				if (strncmp(q, *alias, SYSNSIZE) != 0) {
					logent(q, "FORGED HOSTNAME");
					logent(inet_ntoa(from.sin_addr), "ORIGINATED AT");
					omsg('R',"You're not who you claim to be", Ofn);
					cleanup(0);
				}
#ifdef DEBUG
				if (Debug> 99)
					logent(q,"Found in host Tables");
#endif DEBUG
			}
		}
#endif	BSDTCP

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
			systat(Rmtname, SS_CALLBACK, "CALLING BACK");
			gename(CMDPRE, Rmtname, 'C', file);
			close(creat(subfile(file), 0666));
			xuucico(Rmtname);
			cleanup(0);
		}
		seq = 0;
		while (*p == '-') {
			q = pskip(p);
			switch(*(++p)) {
			case 'x':
				Debug = atoi(++p);
				if (Debug <= 0)
					Debug = 1;
				break;
			case 'Q':
				seq = atoi(++p);
				break;
			case 'p':
				MaxGrade = DefMaxGrade = *++p;
				DEBUG(4, "MaxGrade set to %c\n", MaxGrade);
				break;
			case 'v':
				if (strncmp(p, "grade", 5) == 0) {
					p += 6;
					MaxGrade = DefMaxGrade = *p++;
					DEBUG(4, "MaxGrade set to %c\n", MaxGrade);
				}
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
#ifdef GNXSEQ
		if ((ret = gnxseq(Rmtname)) == seq) {
			omsg('R', "OK", Ofn);
			cmtseq();
		} else {
#else !GNXSEQ
		if (seq == 0)
			omsg('R', "OK", Ofn);
		else {
#endif !GNXSEQ
			systat(Rmtname, Stattype[7], Stattext[7]);
			logent("BAD SEQ", "FAILED HANDSHAKE");
#ifdef GNXSEQ
			ulkseq();
#endif GNXSEQ
			omsg('R', "BADSEQ", Ofn);
			cleanup(0);
		}
		ttyn = ttyname(Ifn);
		if (ttyn != NULL)
			chmod(ttyn, 0600);
	} else { /* Role == MASTER */
		struct stat stbuf;
		if (isatty(fileno(stderr)) || (fstat(fileno(stderr),&stbuf) == 0
		    && stbuf.st_mode&S_IFREG) )
			StdErrIsTty =  1;
		setdebug(0);
	}

loop:
	if(setjmp(Pipebuf)) {	/* come here on SIGPIPE	*/
		clsacu();
		close(Ofn);
		close(Ifn);
		Ifn = Ofn = -1;
		rmlock(CNULL);
		sleep(3);
	}
	if (!onesys) {
		struct stat sbuf;

		if (!StdErrIsTty) {
			sprintf(file, "%s/%s", RMTDEBUG, Rmtname);
			if (stat(file, &sbuf) == 0 && sbuf.st_size == 0)
				unlink(file);
		}
		ret = gnsys(Rmtname, Spool, CMDPRE);
		setdebug(0);
		if (ret == FAIL)
			cleanup(100);
		if (ret == SUCCESS)
			cleanup(0);
	} else if (Role == MASTER && callok(Rmtname) != 0) {
		logent("SYSTEM STATUS", "CAN NOT CALL");
		cleanup(0);
	}

	sprintf(wkpre, "%c.%.*s", CMDPRE, SYSNSIZE, Rmtname);

	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, SIG_IGN);
	if (Role == MASTER) {
		/* check for /etc/nologin */
		ultouch();	/* sets nologinflag as a side effect */
		if (nologinflag) {
			logent(NOLOGIN, "UUCICO SHUTDOWN");
			if (Debug > 4)
				logent("DEBUGGING", "continuing anyway");
			else
				cleanup(1);
		}
		/*  master part */
		signal(SIGHUP, SIG_IGN);
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
			US_SST(us_s_lock);
			goto next;
		}
		Ofn = Ifn = conn(Rmtname);
		if (Ofn < 0) {
			if (Ofn != CF_TIME)
				logent(msg, _FAILED);
			/* avoid excessive 'wrong time' info */
			if (Stattype[-Ofn] != SS_WRONGTIME){
				systat(Rmtname, Stattype[-Ofn], Stattext[-Ofn]);
				US_SST(-Ofn);
				UB_SST(-Ofn);
			}
			goto next;
		} else {
			logent(msg, "SUCCEEDED");
			US_SST(us_s_cok);
			UB_SST(ub_ok);
		}
#ifdef	TCPIP
		/*
		 * Determine if we are on TCPIP
		 */
		if (isatty(Ifn) ==  0) {
			IsTcpIp = 1;
			DEBUG(4, "TCPIP connection -- ioctl-s disabled\n", CNULL);
		} else
			IsTcpIp = 0;
#endif

		if (setjmp(Sjbuf))
			goto next;
		signal(SIGALRM, timeout);
		alarm(2 * MAXMSGTIME);
		for (;;) {
			ret = imsg(msg, Ifn);
			if (ret != 0) {
				alarm(0);
				logent("imsg 1", _FAILED);
				goto Failure;
			}
			if (msg[0] == 'S')
				break;
		}
		alarm(MAXMSGTIME);
#ifdef GNXSEQ
		seq = gnxseq(Rmtname);
#else !GNXSEQ
		seq = 0;
#endif !GNXSEQ
		if (MaxGrade != '\177') {
			char buf[MAXFULLNAME];
			sprintf(buf, " -p%c -vgrade=%c", MaxGrade, MaxGrade);
			strcat(rflags, buf);
		}

		sprintf(msg, "%s -Q%d %s", Myname, seq, rflags);
		if (MaxGrade != '\177')
			DEBUG(2, "Max Grade this transfer is %c\n", MaxGrade);
		omsg('S', msg, Ofn);
		for (;;) {
			ret = imsg(msg, Ifn);
			DEBUG(4, "msg-%s\n", msg);
			if (ret != SUCCESS) {
				alarm(0);
#ifdef GNXSEQ
				ulkseq();
#endif GNXSEQ
				logent("imsg 2", _FAILED);
				goto Failure;
			}
			if (msg[0] == 'R')
				break;
		}
		alarm(0);
		if (msg[1] == 'B') {
			/* bad sequence */
			logent("BAD SEQ", "FAILED HANDSHAKE");
			US_SST(us_s_hand);
			systat(Rmtname, SS_BADSEQ, Stattext[SS_BADSEQ]);
#ifdef GNXSEQ
			ulkseq();
#endif GNXSEQ
			goto next;
		}
		if (strcmp(&msg[1], "OK") != SAME)  {
			logent(&msg[1], "FAILED HANDSHAKE");
			US_SST(us_s_hand);
#ifdef GNXSEQ
			ulkseq();
#endif GNXSEQ
			systat(Rmtname, SS_INPROGRESS,
				strcmp(&msg[1], "CB") == SAME?
				"AWAITING CALLBACK": "FAILED HANDSHAKE");
			goto next;
		}
#ifdef GNXSEQ
		cmtseq();
#endif GNXSEQ
	}
	DEBUG(1, "Rmtname %s, ", Rmtname);
	DEBUG(1, "Role %s,  ", Role ? "MASTER" : "SLAVE");
	DEBUG(1, "Ifn - %d, ", Ifn);
	DEBUG(1, "Loginuser - %s\n", Loginuser);

	alarm(MAXMSGTIME);
	if (ret=setjmp(Sjbuf))
		goto Failure;
	ret = startup(Role);
	alarm(0);
	if (ret != SUCCESS) {
		logent("startup", _FAILED);
Failure:
		US_SST(us_s_start);
		systat(Rmtname, SS_FAIL, ret > 0 ? "CONVERSATION FAILED" :
			"STARTUP FAILED");
		goto next;
	} else {
		logent("startup", "OK");
		US_SST(us_s_gress);
		systat(Rmtname, SS_INPROGRESS, "TALKING");
		ret = cntrl(Role, wkpre);
		DEBUG(1, "cntrl - %d\n", ret);
		signal(SIGINT, SIG_IGN);
		signal(SIGHUP, SIG_IGN);
		signal(SIGALRM, timeout);
		if (ret == SUCCESS) {
			logent("conversation complete", "OK");
			US_SST(us_s_ok);
			rmstat(Rmtname);

		} else {
			logent("conversation complete", _FAILED);
			US_SST(us_s_cf);
			systat(Rmtname, SS_FAIL, "CONVERSATION FAILED");
		}
		alarm(MAXMSGTIME);
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
		clsacu();
		rmlock(CNULL);
	}
next:
	if (!onesys) {
		goto loop;
	}
	cleanup(0);
}

#ifndef	USG
struct sgttyb Hupvec;
#endif

/***
 *	cleanup(code)	cleanup and exit with "code" status
 *	int code;
 */

cleanup(code)
register int code;
{
	register char *ttyn;
	char bfr[BUFSIZ];
	struct stat sbuf;

	signal(SIGINT, SIG_IGN);
	signal(SIGHUP, SIG_IGN);
	rmlock(CNULL);
	clsacu();
	logcls();
	if (Role == SLAVE) {
		if (!IsTcpIp) {
#ifdef USG
			Savettyb.c_cflag |= HUPCL;
			(void) ioctl(0, TCSETA, &Savettyb);
#else !USG
			(void) ioctl(0, TIOCHPCL, STBNULL);
#ifdef TIOCSDTR
			(void) ioctl(0, TIOCCDTR, STBNULL);
			sleep(2);
			(void) ioctl(0, TIOCSDTR, STBNULL);
#else !TIOCSDTR
			(void) ioctl(0, TIOCGETP, &Hupvec);
#endif !TIOCSDTR
			Hupvec.sg_ispeed = B0;
			Hupvec.sg_ospeed = B0;
			(void) ioctl(0, TIOCSETP, &Hupvec);
			sleep(2);
			(void) ioctl(0, TIOCSETP, &Savettyb);
			/* make *sure* exclusive access is off */
			(void) ioctl(0, TIOCNXCL, STBNULL);
#endif !USG
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
#ifdef DIALINOUT
	/* reenable logins on dialout */
	reenable();
#endif DIALINOUT
	if (code == 0)
		xuuxqt();
	else
		DEBUG(1, "exit code %d\n", code);
	sprintf(bfr, "%s/%s", RMTDEBUG, Rmtname);
	if (stat(bfr, &sbuf) == 0 && sbuf.st_size == 0)
		unlink(bfr);
	sprintf(bfr, "%s/%d", RMTDEBUG, getpid());
	unlink(bfr);
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
	US_SST(us_s_intr);
	if (*Rmtname && strncmp(Rmtname, Myname, MAXBASENAME))
		systat(Rmtname, SS_FAIL, str);
	if (inter == SIGPIPE && !onesys)
		longjmp(Pipebuf, 1);
	cleanup(inter);
}

/*
 * Catch a special signal
 * (SIGFPE, ugh), and toggle debugging between 0 and 30.
 * Handy for looking in on long running uucicos.
 */
setdebug(code)
int code;
{
	char buf[BUFSIZ];

	if (code) {
		if (Debug == 0)
			Debug = 30;
		else
			Debug = 0;
	}
	if (Debug && !StdErrIsTty) {
		sprintf(buf,"%s/%s", RMTDEBUG, Rmtname);
		unlink(buf);
		freopen(buf, "w", stderr);
#ifdef BSD4_2
		setlinebuf(stderr);
#else  !BSD4_2
		setbuf(stderr, NULL);
#endif !BSD4_2
	}
}


/***
 *	fixmode(tty)	fix kill/echo/raw on line
 *
 *	return codes:  none
 */

fixmode(tty)
register int tty;
{
#ifdef	USG
	struct termio ttbuf;
#endif
#ifndef	USG
	struct sgttyb ttbuf;
#endif

	if (IsTcpIp)
		return;
#ifdef	USG
	ioctl(tty, TCGETA, &ttbuf);
	ttbuf.c_iflag = ttbuf.c_oflag = ttbuf.c_lflag = (ushort)0;
	ttbuf.c_cflag &= (CBAUD);
	ttbuf.c_cflag |= (CS8|CREAD);
	ttbuf.c_cc[VMIN] = 6;
	ttbuf.c_cc[VTIME] = 1;
	ioctl(tty, TCSETA, &ttbuf);
#endif
#ifndef	USG
	ioctl(tty, TIOCGETP, &ttbuf);
	ttbuf.sg_flags = (ANYP | RAW);
	ioctl(tty, TIOCSETP, &ttbuf);
#endif
#ifndef	USG
	ioctl(tty, TIOCEXCL, STBNULL);
#endif
}


/*
 *	timeout()	catch SIGALRM routine
 */

timeout()
{
	extern int HaveSentHup;
	if (!HaveSentHup) {
		logent(Rmtname, "TIMEOUT");
		if (*Rmtname && strncmp(Rmtname, Myname, MAXBASENAME)) {
			US_SST(us_s_tmot);
			systat(Rmtname, SS_FAIL, "TIMEOUT");
		}
	}
	longjmp(Sjbuf, 1);
}

static char *
pskip(p)
register char *p;
{
	while(*p && *p != ' ')
		++p;
	while(*p && *p == ' ')
		*p++ = 0;
	return p;
}
