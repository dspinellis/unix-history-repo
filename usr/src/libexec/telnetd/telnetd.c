/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)telnetd.c	5.1 (Berkeley) %G%";
#endif not lint

/*
 * Stripped-down telnet server.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <sys/stat.h>

#include <netinet/in.h>

#include <arpa/telnet.h>

#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <sgtty.h>
#include <netdb.h>
#include <syslog.h>

#define	BELL	'\07'
#define BANNER	"\r\n\r\n4.2 BSD UNIX (%s)\r\n\r\r\n\r%s"

char	hisopts[256];
char	myopts[256];

char	doopt[] = { IAC, DO, '%', 'c', 0 };
char	dont[] = { IAC, DONT, '%', 'c', 0 };
char	will[] = { IAC, WILL, '%', 'c', 0 };
char	wont[] = { IAC, WONT, '%', 'c', 0 };

/*
 * I/O data buffers, pointers, and counters.
 */
char	ptyibuf[BUFSIZ], *ptyip = ptyibuf;
char	ptyobuf[BUFSIZ], *pfrontp = ptyobuf, *pbackp = ptyobuf;
char	netibuf[BUFSIZ], *netip = netibuf;
char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;
int	pcc, ncc;

int	pty, net;
int	inter;
extern	char **environ;
extern	int errno;
char	*line;

main(argc, argv)
	char *argv[];
{
	struct sockaddr_in from;
	int on = 1, fromlen;

	fromlen = sizeof (from);
	if (getpeername(0, &from, &fromlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		_exit(1);
	}
	if (setsockopt(0, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof (on)) < 0) {
		openlog(argv[0], LOG_PID, 0);
		syslog(LOG_WARNING, "setsockopt (SO_KEEPALIVE): %m");
	}
	doit(0, &from);
}

char	*envinit[] = { "TERM=network", 0 };
int	cleanup();

/*
 * Get a pty, scan input lines.
 */
doit(f, who)
	int f;
	struct sockaddr_in *who;
{
	char *host, *inet_ntoa();
	int i, p, t;
	struct sgttyb b;
	struct hostent *hp;
	char c;

	for (c = 'p'; c <= 's'; c++) {
		struct stat stb;

		line = "/dev/ptyXX";
		line[strlen("/dev/pty")] = c;
		line[strlen("/dev/ptyp")] = '0';
		if (stat(line, &stb) < 0)
			break;
		for (i = 0; i < 16; i++) {
			line[strlen("/dev/ptyp")] = "0123456789abcdef"[i];
			p = open(line, 2);
			if (p > 0)
				goto gotpty;
		}
	}
	fatal(f, "All network ports in use");
	/*NOTREACHED*/
gotpty:
	dup2(f, 0);
	line[strlen("/dev/")] = 't';
	t = open("/dev/tty", O_RDWR);
	if (t >= 0) {
		ioctl(t, TIOCNOTTY, 0);
		close(t);
	}
	t = open(line, O_RDWR);
	if (t < 0)
		fatalperror(f, line, errno);
	ioctl(t, TIOCGETP, &b);
	b.sg_flags = CRMOD|XTABS|ANYP;
	ioctl(t, TIOCSETP, &b);
	ioctl(p, TIOCGETP, &b);
	b.sg_flags &= ~ECHO;
	ioctl(p, TIOCSETP, &b);
	hp = gethostbyaddr(&who->sin_addr, sizeof (struct in_addr),
		who->sin_family);
	if (hp)
		host = hp->h_name;
	else
		host = inet_ntoa(who->sin_addr);
	if ((i = fork()) < 0)
		fatalperror(f, "fork", errno);
	if (i)
		telnet(f, p);
	close(f);
	close(p);
	dup2(t, 0);
	dup2(t, 1);
	dup2(t, 2);
	close(t);
	environ = envinit;
	execl("/bin/login", "login", "-h", host, 0);
	fatalperror(f, "/bin/login", errno);
	/*NOTREACHED*/
}

fatal(f, msg)
	int f;
	char *msg;
{
	char buf[BUFSIZ];

	(void) sprintf(buf, "telnetd: %s.\r\n", msg);
	(void) write(f, buf, strlen(buf));
	exit(1);
}

fatalperror(f, msg, errno)
	int f;
	char *msg;
	int errno;
{
	char buf[BUFSIZ];
	extern char *sys_errlist[];

	(void) sprintf(buf, "%s: %s\r\n", msg, sys_errlist[errno]);
	fatal(f, buf);
}

/*
 * Main loop.  Select from pty and network, and
 * hand data to telnet receiver finite state machine.
 */
telnet(f, p)
{
	int on = 1;
	char hostname[32];

	net = f, pty = p;
	ioctl(f, FIONBIO, &on);
	ioctl(p, FIONBIO, &on);
	signal(SIGTSTP, SIG_IGN);
	signal(SIGCHLD, cleanup);

	/*
	 * Request to do remote echo.
	 */
	dooption(TELOPT_ECHO);
	myopts[TELOPT_ECHO] = 1;
	/*
	 * Show banner that getty never gave.
	 */
	gethostname(hostname, sizeof (hostname));
	sprintf(nfrontp, BANNER, hostname, "");
	nfrontp += strlen(nfrontp);
	for (;;) {
		int ibits = 0, obits = 0;
		register int c;

		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */
		if (nfrontp - nbackp || pcc > 0)
			obits |= (1 << f);
		else
			ibits |= (1 << p);
		if (pfrontp - pbackp || ncc > 0)
			obits |= (1 << p);
		else
			ibits |= (1 << f);
		if (ncc < 0 && pcc < 0)
			break;
		select(16, &ibits, &obits, 0, 0);
		if (ibits == 0 && obits == 0) {
			sleep(5);
			continue;
		}

		/*
		 * Something to read from the network...
		 */
		if (ibits & (1 << f)) {
			ncc = read(f, netibuf, BUFSIZ);
			if (ncc < 0 && errno == EWOULDBLOCK)
				ncc = 0;
			else {
				if (ncc <= 0)
					break;
				netip = netibuf;
			}
		}

		/*
		 * Something to read from the pty...
		 */
		if (ibits & (1 << p)) {
			pcc = read(p, ptyibuf, BUFSIZ);
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else {
				if (pcc <= 0)
					break;
				ptyip = ptyibuf;
			}
		}

		while (pcc > 0) {
			if ((&netobuf[BUFSIZ] - nfrontp) < 2)
				break;
			c = *ptyip++ & 0377, pcc--;
			if (c == IAC)
				*nfrontp++ = c;
			*nfrontp++ = c;
		}
		if ((obits & (1 << f)) && (nfrontp - nbackp) > 0)
			netflush();
		if (ncc > 0)
			telrcv();
		if ((obits & (1 << p)) && (pfrontp - pbackp) > 0)
			ptyflush();
	}
	cleanup();
}
	
/*
 * State for recv fsm
 */
#define	TS_DATA		0	/* base state */
#define	TS_IAC		1	/* look for double IAC's */
#define	TS_CR		2	/* CR-LF ->'s CR */
#define	TS_BEGINNEG	3	/* throw away begin's... */
#define	TS_ENDNEG	4	/* ...end's (suboption negotiation) */
#define	TS_WILL		5	/* will option negotiation */
#define	TS_WONT		6	/* wont " */
#define	TS_DO		7	/* do " */
#define	TS_DONT		8	/* dont " */

telrcv()
{
	register int c;
	static int state = TS_DATA;
	struct sgttyb b;

	while (ncc > 0) {
		if ((&ptyobuf[BUFSIZ] - pfrontp) < 2)
			return;
		c = *netip++ & 0377, ncc--;
		switch (state) {

		case TS_DATA:
			if (c == IAC) {
				state = TS_IAC;
				break;
			}
			if (inter > 0)
				break;
			*pfrontp++ = c;
			if (!myopts[TELOPT_BINARY] && c == '\r')
				state = TS_CR;
			break;

		case TS_CR:
			if (c && c != '\n')
				*pfrontp++ = c;
			state = TS_DATA;
			break;

		case TS_IAC:
			switch (c) {

			/*
			 * Send the process on the pty side an
			 * interrupt.  Do this with a NULL or
			 * interrupt char; depending on the tty mode.
			 */
			case BREAK:
			case IP:
				interrupt();
				break;

			/*
			 * Are You There?
			 */
			case AYT:
				strcpy(nfrontp, "\r\n[Yes]\r\n");
				nfrontp += 9;
				break;

			/*
			 * Erase Character and
			 * Erase Line
			 */
			case EC:
			case EL:
				ptyflush();	/* half-hearted */
				ioctl(pty, TIOCGETP, &b);
				*pfrontp++ = (c == EC) ?
					b.sg_erase : b.sg_kill;
				break;

			/*
			 * Check for urgent data...
			 */
			case DM:
				break;

			/*
			 * Begin option subnegotiation...
			 */
			case SB:
				state = TS_BEGINNEG;
				continue;

			case WILL:
			case WONT:
			case DO:
			case DONT:
				state = TS_WILL + (c - WILL);
				continue;

			case IAC:
				*pfrontp++ = c;
				break;
			}
			state = TS_DATA;
			break;

		case TS_BEGINNEG:
			if (c == IAC)
				state = TS_ENDNEG;
			break;

		case TS_ENDNEG:
			state = c == SE ? TS_DATA : TS_BEGINNEG;
			break;

		case TS_WILL:
			if (!hisopts[c])
				willoption(c);
			state = TS_DATA;
			continue;

		case TS_WONT:
			if (hisopts[c])
				wontoption(c);
			state = TS_DATA;
			continue;

		case TS_DO:
			if (!myopts[c])
				dooption(c);
			state = TS_DATA;
			continue;

		case TS_DONT:
			if (myopts[c]) {
				myopts[c] = 0;
				sprintf(nfrontp, wont, c);
				nfrontp += sizeof (wont) - 2;
			}
			state = TS_DATA;
			continue;

		default:
			printf("telnetd: panic state=%d\n", state);
			exit(1);
		}
	}
}

willoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_BINARY:
		mode(RAW, 0);
		goto common;

	case TELOPT_ECHO:
		mode(0, ECHO|CRMOD);
		/*FALL THRU*/

	case TELOPT_SGA:
	common:
		hisopts[option] = 1;
		fmt = doopt;
		break;

	case TELOPT_TM:
		fmt = dont;
		break;

	default:
		fmt = dont;
		break;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (dont) - 2;
}

wontoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_ECHO:
		mode(ECHO|CRMOD, 0);
		goto common;

	case TELOPT_BINARY:
		mode(0, RAW);
		/*FALL THRU*/

	case TELOPT_SGA:
	common:
		hisopts[option] = 0;
		fmt = dont;
		break;

	default:
		fmt = dont;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
}

dooption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_TM:
		fmt = wont;
		break;

	case TELOPT_ECHO:
		mode(ECHO|CRMOD, 0);
		goto common;

	case TELOPT_BINARY:
		mode(RAW, 0);
		/*FALL THRU*/

	case TELOPT_SGA:
	common:
		fmt = will;
		break;

	default:
		fmt = wont;
		break;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
}

mode(on, off)
	int on, off;
{
	struct sgttyb b;

	ptyflush();
	ioctl(pty, TIOCGETP, &b);
	b.sg_flags |= on;
	b.sg_flags &= ~off;
	ioctl(pty, TIOCSETP, &b);
}

/*
 * Send interrupt to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write intr char.
 */
interrupt()
{
	struct sgttyb b;
	struct tchars tchars;

	ptyflush();	/* half-hearted */
	ioctl(pty, TIOCGETP, &b);
	if (b.sg_flags & RAW) {
		*pfrontp++ = '\0';
		return;
	}
	*pfrontp++ = ioctl(pty, TIOCGETC, &tchars) < 0 ?
		'\177' : tchars.t_intrc;
}

ptyflush()
{
	int n;

	if ((n = pfrontp - pbackp) > 0)
		n = write(pty, pbackp, n);
	if (n < 0)
		return;
	pbackp += n;
	if (pbackp == pfrontp)
		pbackp = pfrontp = ptyobuf;
}

netflush()
{
	int n;

	if ((n = nfrontp - nbackp) > 0)
		n = write(net, nbackp, n);
	if (n < 0) {
		if (errno == EWOULDBLOCK)
			return;
		/* should blow this guy away... */
		return;
	}
	nbackp += n;
	if (nbackp == nfrontp)
		nbackp = nfrontp = netobuf;
}

cleanup()
{

	rmut();
	vhangup();	/* XXX */
	shutdown(net, 2);
	kill(0, SIGKILL);
	exit(1);
}

#include <utmp.h>

struct	utmp wtmp;
char	wtmpf[]	= "/usr/adm/wtmp";
char	utmp[] = "/etc/utmp";
#define SCPYN(a, b)	strncpy(a, b, sizeof (a))
#define SCMPN(a, b)	strncmp(a, b, sizeof (a))

rmut()
{
	register f;
	int found = 0;

	f = open(utmp, O_RDWR);
	if (f >= 0) {
		while(read(f, (char *)&wtmp, sizeof (wtmp)) == sizeof (wtmp)) {
			if (SCMPN(wtmp.ut_line, line+5) || wtmp.ut_name[0]==0)
				continue;
			lseek(f, -(long)sizeof (wtmp), L_INCR);
			SCPYN(wtmp.ut_name, "");
			SCPYN(wtmp.ut_host, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof (wtmp));
			found++;
		}
		close(f);
	}
	if (found) {
		f = open(wtmpf, O_WRONLY|O_APPEND);
		if (f >= 0) {
			SCPYN(wtmp.ut_line, line+5);
			SCPYN(wtmp.ut_name, "");
			SCPYN(wtmp.ut_host, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof (wtmp));
			close(f);
		}
	}
	chmod(line, 0666);
	chown(line, 0, 0);
	line[strlen("/dev/")] = 'p';
	chmod(line, 0666);
	chown(line, 0, 0);
}
