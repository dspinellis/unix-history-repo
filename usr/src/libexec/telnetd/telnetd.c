static char sccsid[] = "@(#)telnetd.c	4.4 (Berkeley) %G%";
/*
 * Stripped-down telnet server.
 */
#include <stdio.h>
#include <signal.h>
#include <errno.h>
#include <sgtty.h>
#include <wait.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <net/in.h>
#include "telnet.h"

#define	INFINITY	10000000
#define	BELL		'\07'
#define	swab(x)		((((x) >> 8) | ((x) << 8)) & 0xffff)

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
char	netobuf[BUFSIZ] =
	{ IAC, DO, TELOPT_ECHO, '\r', '\n' },
	*nfrontp = netobuf + 5, *nbackp = netobuf;
int	pcc, ncc;

int	pty, net;
int	inter;
extern	int errno;
char	line[] = "/dev/ptyp0";

struct	sockaddr_in sin = { AF_INET, swab(IPPORT_TELNET) };
int	options = SO_ACCEPTCONN|SO_KEEPALIVE;

/*
 * Debugging hooks.  Turned on with a SIGTERM.
 * Successive SIGTERM's toggle the switch.
 */
int	toggle();
int	debug;
FILE	*log;
char	logfile[80] = "/tmp/teldebugx";

main(argc, argv)
	char *argv[];
{
	int s, pid;
	union wait status;

	argc--, argv++;
	if (argc > 0 && !strcmp(argv[0], "-d"))
		options |= SO_DEBUG;
	for (;;) {
		errno = 0;
		if ((s = socket(SOCK_STREAM, 0, &sin, options)) < 0) {
			perror("socket");
			sleep(5);
			continue;
		}
		if (accept(s, 0) < 0) {
			perror("accept");
			close(s);
			sleep(1);
			continue;
		}
		if ((pid = fork()) < 0)
			printf("Out of processes\n");
		else if (pid == 0)
			doit(s);
		close(s);
		while (wait3(status, WNOHANG, 0) > 0)
			continue;
	}
	/*NOTREACHED*/
}

int	cleanup();

/*
 * Get a pty, scan input lines.
 */
doit(f)
{
	char *cp = line;
	int i, p, cc, t;
	struct sgttyb b;

	for (i = 0; i < 16; i++) {
		cp[strlen("/dev/ptyp")] = "0123456789abcdef"[i];
		p = open(cp, 2);
		if (p > 0)
			goto gotpty;
	}
	dup2(f, 1);
	printf("All network ports in use.\n");
	exit(1);
gotpty:
	logfile[strlen("/tmp/teldebug")] = "0123456789abcdef"[i];
	dup2(f, 0);
	cp[strlen("/dev/")] = 't';
	t = open("/dev/tty", 2);
	if (t >= 0) {
		ioctl(t, TIOCNOTTY, 0);
		close(t);
	}
	t = open(cp, 2);
	if (t < 0) {
		dup2(f, 2);
		perror(cp);
		exit(1);
	}
	ioctl(t, TIOCGETP, &b);
	b.sg_flags = ECHO|CRMOD|XTABS|ANYP;
	ioctl(t, TIOCSETP, &b);
	if ((i = fork()) < 0) {
		dup2(f, 2);
		perror("fork");
		exit(1);
	}
	if (i)
		telnet(f, p);
	close(f);
	close(p);
	dup2(t, 0);
	dup2(t, 1);
	dup2(t, 2);
	close(t);
	execl("/bin/login", "telnet-login", 0);
	perror("/bin/login");
	exit(1);
}

/*
 * Main loop.  Select from pty and network, and
 * hand data to telnet receiver finite state machine.
 */
telnet(f, p)
{
	int on = 1;

	net = f, pty = p;
	ioctl(f, FIONBIO, &on);
	ioctl(p, FIONBIO, &on);
	signal(SIGTSTP, SIG_IGN);
	sigset(SIGCHLD, cleanup);
	sigset(SIGTERM, toggle);

	for (;;) {
		int ibits = 0, obits = 0;
		register int c;

		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */
		if (nfrontp - nbackp)
			obits |= (1 << f);
		else
			ibits |= (1 << p);
		if (pfrontp - pbackp)
			obits |= (1 << p);
		else
			ibits |= (1 << f);
		if (ncc < 0 && pcc < 0)
			break;
		if (debug)
			fprintf(log, "select: ibits=%d, obits=%d\n",
				ibits, obits);
		select(32, &ibits, &obits, INFINITY);
		if (debug)
			fprintf(log, "ibits=%d, obits=%d\n", ibits, obits);
		if (ibits == 0 && obits == 0) {
			sleep(5);
			continue;
		}

		/*
		 * Something to read from the network...
		 */
		if (ibits & (1 << f)) {
			ncc = read(f, netibuf, BUFSIZ);
			if (debug)
				fprintf(log, "read %d from net\n", ncc);
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
			if (debug)
				fprintf(log, "read %d from pty\n", pcc);
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
				*pfrontp++ = BELL;
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
				nfrontp += sizeof(wont) - 2;
			}
			state = TS_DATA;
			continue;

		default:
			printf("netser: panic state=%d\n", state);
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
	nfrontp += sizeof(dont) - 2;
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
	nfrontp += sizeof(doopt) - 2;
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
	nfrontp += sizeof(doopt) - 2;
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
	if (n < 0 && errno == EWOULDBLOCK)
		n = 0;
	pbackp += n;
	if (pbackp == pfrontp)
		pbackp = pfrontp = ptyobuf;
}

netflush()
{
	int n;

	if ((n = nfrontp - nbackp) > 0)
		n = write(net, nbackp, n);
	if (n < 0 && errno == EWOULDBLOCK)
		n = 0;
	nbackp += n;
	if (nbackp == nfrontp)
		nbackp = nfrontp = netobuf;
}

toggle()
{
	if (debug) {
		fprintf(log, "log stopped\n");
		if (log)
			fclose(log);
	} else {
		if ((log = fopen(logfile, "a")) != NULL) {
			setbuf(log, 0);
			fprintf(log, "log started on /dev/pty%c\n",
				logfile[strlen("/tmp/teldebug")]);
			fprintf(log, "net=%d, pty=%d\n", net, pty);
		}
	}
	debug = !debug;
}

cleanup()
{
	int how = 2;

	rmut();
	vhangup();
	ioctl(net, SIOCDONE, &how);
	kill(0, SIGKILL);
	exit(1);
}

#include <utmp.h>

struct	utmp wtmp;
char	wtmpf[]	= "/usr/adm/wtmp";
char	utmp[] = "/etc/utmp";
#define SCPYN(a, b)	strncpy(a, b, sizeof(a))
#define SCMPN(a, b)	strncmp(a, b, sizeof(a))

rmut()
{
	register f;
	int found = 0;

	f = open(utmp, 2);
	if (f >= 0) {
		while(read(f, (char *)&wtmp, sizeof(wtmp)) == sizeof(wtmp)) {
			if (SCMPN(wtmp.ut_line, line+5) || wtmp.ut_name[0]==0)
				continue;
			lseek(f, -(long)sizeof(wtmp), 1);
			SCPYN(wtmp.ut_name, "");
			time(&wtmp.ut_time);
			write(f, (char *)&wtmp, sizeof(wtmp));
			found++;
		}
		close(f);
	}
	if (found) {
		f = open(wtmpf, 1);
		if (f >= 0) {
			SCPYN(wtmp.ut_line, line+5);
			SCPYN(wtmp.ut_name, "");
			time(&wtmp.ut_time);
			lseek(f, (long)0, 2);
			write(f, (char *)&wtmp, sizeof(wtmp));
			close(f);
		}
	}
	chmod(line, 0666);
	chown(line, 0, 0);
	line[strlen("/dev/")] = 'p';
	chmod(line, 0666);
	chown(line, 0, 0);
}
