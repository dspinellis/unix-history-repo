/*
 * X.29 server
 *
 * Frank Pronk (...!ubc-vision!pronk)
 * April, September 1984
 *
 * Laboratory for Computational Vision
 * University of British Columbia
 * Copyright (c)
 */

#include <sys/param.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/wait.h>

#include <netccitt/x25.h>

#include <errno.h>
#include <netdb.h>
#include <signal.h>
#include <sys/ioctl.h>
#include <sys/termios.h>
#include <paths.h>

#include "../h/x29.h"

#define BUFSIZ		1024
#define MAXARGS		10	/* maximum size of server argument list */

#define X25NET		0	/* no ITI parameters */
#define CCITT1978	1	/* 1978 CCITT standard parameter set */
#define CCITT1980	2	/* 1980 CCITT standard parameter set */


char	pibuf[BUFSIZ], fibuf[BUFSIZ];
int	pty, net;
extern	char **environ;
extern	int errno;
char	line[MAXPATHLEN];
char	console[] = "/dev/console";
short	packet_size;
short	debug;
char	*tracefn;		/* trace file name */
char	*server;
short	send_banner;
struct	termios pt, old_pt;
struct	sockaddr_x25 sock;

int	reapchild();
struct	net *lookup ();

char	ccitt1978_prof[] = {		/* initial profile */
	Q_BIT,	X29_SET_AND_READ_PARMS,
	X29_ECHO_CODE,			1,	/* echo on */
	X29_FORWARDING_SIGNAL_CODE,	126,	/* forward on all cntl */
	X29_IDLE_TIMER_CODE,		0,	/* off */
	X29_AUX_DEV_CONTROL_CODE,	0,	/* off */
	X29_RECEIVE_NET_MSGS_CODE,	1,	/* xmit network msgs */
	X29_BREAK_PROCEDURE_CODE,	21,
	X29_PADDING_CODE,		0,	/* off */
	X29_LINE_FOLDING_CODE,		0,	/* off */
	X29_TRANSMISSION_SPEED_CODE,	0,
	X29_XON_XOFF_CODE,		1,	/* enable XON/XOFF */
};

char	ccitt1980_prof[] = {		/* initial profile */
	Q_BIT,	X29_SET_AND_READ_PARMS,
	X29_ECHO_CODE,			1,	/* echo on */
	X29_FORWARDING_SIGNAL_CODE,	126,	/* forward on all cntl */
	X29_IDLE_TIMER_CODE,		0,	/* off */
	X29_AUX_DEV_CONTROL_CODE,	0,	/* off */
	X29_RECEIVE_NET_MSGS_CODE,	1,	/* xmit network msgs */
	X29_BREAK_PROCEDURE_CODE,	21,
	X29_PADDING_CODE,		0,	/* off */
	X29_LINE_FOLDING_CODE,		0,	/* off */
	X29_TRANSMISSION_SPEED_CODE,	0,
	X29_XON_XOFF_CODE,		1,	/* enable XON/XOFF */

	X29_LF_AFTER_CR,		4,	/* lf after cr from terminal */
	X29_EDITING,			1,	/* on */
	X29_CHARACTER_DELETE,		CERASE,
	X29_LINE_DELETE,		CKILL,
	X29_LINE_DISPLAY,		CRPRNT,
};

char	datapac_prof[] = {		/* Canadian X.25 network */
	Q_BIT,	X29_SET_AND_READ_PARMS,
	X29_ECHO_CODE,			1,	/* echo on */
	X29_FORWARDING_SIGNAL_CODE,	126,	/* forward on all cntl */
	X29_IDLE_TIMER_CODE,		0,	/* off */
	X29_AUX_DEV_CONTROL_CODE,	0,	/* off */
	X29_RECEIVE_NET_MSGS_CODE,	1,	/* xmit network msgs */
	X29_BREAK_PROCEDURE_CODE,	21,
	X29_PADDING_CODE,		0,	/* off */
	X29_LINE_FOLDING_CODE,		0,	/* off */
	X29_TRANSMISSION_SPEED_CODE,	0,
	X29_XON_XOFF_CODE,		1,	/* enable XON/XOFF */

	X29_LF_AFTER_CR,		4,	/* lf after cr from terminal */
	X29_EDITING,			1,	/* on */
	X29_CHARACTER_DELETE,		CERASE,
	X29_LINE_DELETE,		CKILL,
	X29_LINE_DISPLAY,		CRPRNT,

	/*
	 * This rubbish can be removed when Datapac
	 * adopts the 1980 standard parameter set.
	 */

	0,				0,	/* national parameter marker */
	123,				0,	/* parity off */
};

struct	net {
	char	*n_name;	/* generic name */
	short	n_type;		/* see defines above */
	char	*n_profile;	/* initial profile */
	short	n_proflen;	/* length of n_profile */
} *netp, nets[] = {
	"x.25",		X25NET,		0,		0,
	"1978",		CCITT1978,	ccitt1978_prof,	sizeof(ccitt1978_prof),
	"ccitt1978",	CCITT1978,	ccitt1978_prof,	sizeof(ccitt1978_prof),
	"1980",		CCITT1980,	ccitt1980_prof,	sizeof(ccitt1980_prof),
	"ccitt1980",	CCITT1980,	ccitt1980_prof,	sizeof(ccitt1980_prof),
	"datapac",	CCITT1980,	datapac_prof,	sizeof(datapac_prof),
	0,		0,		0,		0
};

main(argc, argv)
register char **argv;
{
	register int s, pid;
	register char *p;

	/*
	 * If this host doesn't support X.25, give up.
	 */
	s = socket(AF_CCITT, SOCK_STREAM, 0);
	if (s < 0 && errno == EPROTONOSUPPORT)
		fatal(2, "X.25 is not supported on this machine");
	close(s);
	netp = lookup ("ccitt1978");
	sock.x25_family = AF_CCITT;
	sock.x25_len = sizeof(sock);
	sock.x25_opts.op_flags = X25_MQBIT;
	sock.x25_udata[0] = ITI_CALL;
	sock.x25_udlen = 4;

	for (argv++; argc > 1; argc--, argv++)
		if (**argv == '-')
			for (p = *argv+1; *p; p++)
			switch (*p) {
			case 'b':
				send_banner++;
				break;

			case 'c':
				if (argc > 1) {
					argc--; argv++;
					if ((netp = lookup (*argv)) == 0)
						fatal(1, "Unknown network type");
				}
				break;

			case 'p':
				if (argc > 1) {
					argc--; argv++;
					strcpy (sock.x25_udata, *argv);
				}
				break;

			case 'r':
				sock.x25_opts.op_flags |= X25_REVERSE_CHARGE;
				break;

			case 'd':
				debug++;
				break;

			case 't':
				if (argc > 1) {
					argc--; argv++;
					tracefn = *argv;
				}
				else fatal(1, "missing trace file");
				break;

			default:
				fatal (1, "usage: x29d -b -c nettype -p protocol -r -t trace_file server");
			}
		else
			server = *argv;
	if (server == 0)
		fatal (1, "no server specified");
	if (debug == 0)
		daemon(0, 0);

	while ((s = socket(AF_CCITT, SOCK_STREAM, 0)) < 0)
		sleep(60);
	while (bind(s, (caddr_t)&sock, sizeof (sock)) < 0)
		sleep(60);
	signal(SIGCHLD, reapchild);
	listen(s, 5);

	for (;;) {
		struct sockaddr_x25 from;
		int fromlen = sizeof (from);

		if ((net = accept(s, (caddr_t)&from, &fromlen)) < 0) {
			if (errno != EINTR)
				sleep (60);
			continue;
		}
		while ((pid = fork()) < 0)
			sleep(60);
		if (pid == 0) {
			signal(SIGCHLD, SIG_DFL);
			doit(&from);
		}
		close(net);
	}
	/*NOTREACHED*/
}

struct net *
lookup (name)
char *name;
{
	register struct net *np;

	for (np = nets; np->n_name; np++)
		if (strcmp (np->n_name, name) == 0)
			return (np);
	return (0);
}

reapchild()
{
	union wait status;

	while (wait3(&status, WNOHANG, 0) > 0)
		;
}

char	*envinit[] = { "TERM=ccitt", 0 };
int	cleanup();
struct termios term;

/*
 * Get a pty, scan input lines.
 */
doit(who)
struct sockaddr_x25 *who;
{
	register char *cp;
	int i, p, t;

	packet_size = 1 << who->x25_opts.op_psize;
	i = forkpty(&pty, line, &term, 0);
	if (i > 0)
		x29d();
	if (i < 0)
		fatalperror("fork", errno);
	environ = envinit;
	call_server (who);
	/*NOTREACHED*/
}

call_server (who)
struct sockaddr_x25 *who;
{
	register struct hostent *hp = 0;
	register char *p, **ap;
	char *args[MAXARGS];
	struct stat st;
	struct hostent *getx25hostbyaddr();
	int ccitt = 0;

	p = server;
	while (*p && *p != ' ' && *p != '\t')	/* split program from args */
		p++;
	if (*p)
		*p++ = '\0';
	ap = args;
	while (*p) {
		while (*p == ' ' || *p == '\t')
			p++;
		if (ap < &args[MAXARGS-2])
			*ap++ = p;
		if (strcmp(p, "-ccitt") == 0)
			ccitt = 1;
		while (*p && *p != ' ' && *p != '\t')
			p++;
		if (*p)
			*p++ = '\0';
	}
	if (stat (server, &st) < 0)
		fatalperror (server, errno);
	/*
	 * For security: if running as root, switch to user
	 * and group id of server.  This prevents privately
	 * maintainted or bogus servers from getting super-
	 * user permissions.
	 */
	if (getuid() == 0) {
		setgid (st.st_gid);
		setuid (st.st_uid);
	}
	if (hp = getx25hostbyaddr (who->x25_addr))
		*ap++ = hp->h_name;
	else
		*ap++ = (char *)who->x25_addr;
	/*
	 * If the -ccitt flag was given, add another argument
	 * to tell login if charging is being reversed or not.
	 */
	if (ccitt)
		*ap++ = (who->x25_opts.op_flags & X25_REVERSE_CHARGE) ? "y" : "n";
	*ap = 0;
	execv (server, args);
	fatalperror (server, errno);
	/*NOTREACHED*/
}

fatal(f, msg)
	int f;
	char *msg;
{
	register char *p;
	char buf[BUFSIZ], *index();

	p = buf;
	if (f == net)
		*p++ = 0;
	strcpy(p, "x29d: ");
	strcat(p, msg);
	strcat(p, "\n");
	(void) write(f, p, (index(p, '\n')-p)+1);
	exit(1);
}

fatalperror(msg, err)
char *msg;
{
	char buf[BUFSIZ];
	extern char *sys_errlist[];

	strcpy(buf, msg);
	strcat(buf, ": ");
	strcat(buf, sys_errlist[err]);
	fatal(net, buf);
}

/*
 * Main loop.  Select from pty and network, and
 * hand data to iti receiver.
 */
x29d()
{
	register int pcc, fcc, cc;
	register char *fbp;
	int pgrp, x25_interrupt(), on = 1;
	char hostname[32];

	ioctl(net, FIONBIO, (char *)&on);
	ioctl(pty, FIONBIO, (char *)&on);
	/*ioctl(pty, TIOCREMECHO, (char *)&on);	/* enable special pty mode */
	/* new equivalent is no processing in pty, no echo, but let
	   user set modes and have either remote end do line mode processing
	   or do it in daemon */
	ioctl(pty, TIOCEXT, (char *)&on);
	ioctl(pty, TIOCPKT, (char *)&on);
	ioctl(pty, TIOCGETA, (char *)&pt);
	signal(SIGPIPE, SIG_IGN);	/* why not cleanup?  --kwl */
	signal(SIGTSTP, SIG_IGN);
	signal(SIGCHLD, cleanup);
	signal(SIGHUP, cleanup);

	signal(SIGTTOU, SIG_IGN);
	signal(SIGURG, x25_interrupt);	/* for out-of-band data */

	if (netp->n_proflen)
		(void) write(net, netp->n_profile, netp->n_proflen);

	/*
	 * Show banner that getty never gave.
	 */
	if (send_banner) {
		gethostname(hostname, sizeof (hostname));
#ifdef BSD4_3
		strcpy(pibuf+1, "\r\n\r\n4.3 BSD UNIX (");
#else
		strcpy(pibuf+1, "\r\n\r\n4.2 BSD UNIX (");
#endif
		strcat(pibuf+1, hostname);
		strcat(pibuf+1, ")\r\n\r\n");
		pcc = strlen(pibuf+1) + 1;
	} else
		pcc = 0;

	fcc = 0;
	for (;;) {
		int ibits, obits;

		ibits = obits = 0;
		/*
		 * Never look for input if there's still
		 * stuff in the corresponding output buffer
		 */
		if (fcc >= 0)			/* net connection alive? */
			if (fcc && pcc >= 0)	/* output pending? */
				obits |= (1 << pty);
			else
				if (pcc >= 0)	/* pty still alive? */
					ibits |= (1 << net);
		if (pcc >= 0)			/* pty connection alive? */
			if (pcc && fcc >= 0)	/* output pending? */
				obits |= (1 << net);
			else
				if (fcc >= 0)	/* net still alive? */
					ibits |= (1 << pty);
		if (ibits == 0 && obits == 0)
			break;
		(void) select(16, &ibits, &obits, (int *)0, 0);
		if (ibits == 0 && obits == 0) {
			sleep(5);
			continue;
		}

		/*
		 * Something to read from the network...
		 */
		if (fcc == 0 && (ibits & (1 << net))) {
			fcc = read(net, fibuf, BUFSIZ);
			fbp = fibuf+1;
			if (fcc < 0 && errno == EWOULDBLOCK)
				fcc = 0;
			else if (fcc <= 0)
				fcc = -1;
			else {
				if (tracefn)
					x29d_trace("netread", fibuf, fcc);
				if (fibuf[0] & Q_BIT) {
					x29_qbit(fcc);
					fcc = 0;
				} else
					fcc--;
			}
		}

		/*
		 * Something to read from the pty...
		 */
		if (ibits & (1 << pty)) {
			pcc = read(pty, pibuf, packet_size+1);
			if (pcc < 0 && errno == EWOULDBLOCK)
				pcc = 0;
			else if (pcc <= 0)
				pcc = -1;
			else if (pibuf[0] != 0) {	/* non-data packet */
				if (pibuf[0] & TIOCPKT_IOCTL) {
					if (--pcc > sizeof(pt))
						pcc = sizeof(pt);
					old_pt = pt;
					bcopy(pibuf + 1, (char *)&pt, pcc);
					pcc = set_x29_parameters();
				} else
					pcc = 0;
			} else				/* data packet */
				pibuf[0] = 0;
		}

		if ((obits & (1<<net)) && pcc > 0)
			if ((cc = write(net, pibuf, pcc)) == pcc) {
				if (tracefn)
					x29d_trace("netwrite", pibuf, pcc);
				pcc = 0;
			} else {
				extern char *sys_errlist[];

				if (tracefn)
					x29d_trace("netwrite",
						sys_errlist[errno],
						strlen(sys_errlist[errno]));
					
			}

		if ((obits & (1 << pty)) && fcc > 0) {
			cc = ptywrite(fbp, fcc);
			if (cc > 0) {
				fcc -= cc;
				fbp += cc;
			}
		}
	}
	cleanup();
}


/*
 * Send interrupt to process on other side of pty.
 * If it is in raw mode, just write NULL;
 * otherwise, write intr char.
 */

x25_interrupt()
{
	struct termios tt;
	int zero = 0;

	signal(SIGURG, x25_interrupt);
	tcgetattr(pty, &tt);
	if (tt.c_lflag & ISIG) {
		tcsetattr(pty, TCSAFLUSH, &tt);
		(void) write(pty, &tt.c_cc[VINTR], 1);
	} else
		(void) write(pty, "\0", 1);
}

cleanup()
{
	char *p;

	p = line + sizeof(_PATH_DEV) - 1;
	if (logout(p))
		logwtmp(p, "", "");
	(void)chmod(line, 0666);
	(void)chown(line, 0, 0);
	*p = 'p';
	(void)chmod(line, 0666);
	(void)chown(line, 0, 0);
	shutdown(net, 2);
	exit(1);
}

/*
 * Map unix tty modes and special characters
 * into x29 parameters.
 */

set_x29_parameters()
{
	register char *p;
	int f;
	char *lim = p + sizeof (pt);

	if (netp->n_type == X25NET)
		return (0);
	if ((old_pt.c_lflag & ICANON) != (pt.c_lflag & ICANON)) {
		f = pt.c_lflag & ICANON;
		ioctl(pty, TIOCEXT, &f);
		/* this precipitates more junk of the same
		 * sort that caused our call here, but we can't
		 * turn it off since something may be going on in our progeny.
		 *
		 * Instead, we'll check the next time around to see if nothing
		 * has changed, and skip informing the network.
		 */
	}
	if (bcmp((char *)&pt, (char *)&old_pt, sizeof (pt)) == 0)
		return;
	p = pibuf;
	*p++ = Q_BIT;
	*p++ = X29_SET_PARMS;
	/* *p++ = X29_ESCAPE_TO_CMD_CODE; *p++ = (f & (RAW|CBREAK)) == 0;*/
	*p++ = X29_ESCAPE_TO_CMD_CODE; *p++ = (pt.c_lflag & ICANON) != 0;

	*p++ = X29_ECHO_CODE; *p++ = (pt.c_lflag & ECHO) != 0;
	*p++ = X29_FORWARDING_SIGNAL_CODE;
			*p++ = (pt.c_lflag & ISIG) ? 0 : 126;

	/*
	 * The value of 10 (0.5 seconds) for the idle timer when
	 * in raw or cbreak mode is a compromise value.  For good
	 * interactive response this value should be as low as
	 * possible; for reasonable efficiency with file transfers
	 * this value should be at fairly high.  This number should
	 * be changed to suit local requirements.
	 */

	/**p++ = X29_IDLE_TIMER_CODE;	*p++ = (f & (RAW|CBREAK)) ? 10 : 0;*/
	*p++ = X29_IDLE_TIMER_CODE; *p++ = (pt.c_lflag & ICANON)  ? 0 : 10;

	/**p++ = X29_AUX_DEV_CONTROL_CODE;*p++ = (f & TANDEM) != 0;*/
	*p++ = X29_AUX_DEV_CONTROL_CODE;*p++ = (pt.c_iflag & IXOFF) != 0;
	*p++ = X29_XON_XOFF_CODE;	*p++ = (pt.c_iflag & IXON) != 0;
	if (netp->n_type == CCITT1980) {
		*p++ = X29_LF_AFTER_CR;
		/* *p++ = (f & (RAW|CBREAK) || (f & ECHO) == 0) ? 0 : 4; */
		*p++ = ((pt.c_lflag & (ICANON | ECHO)) != (ICANON | ECHO)) ?
			0 : 4;

		*p++ = X29_EDITING; *p++ = (pt.c_lflag & ICANON) != 0;
#define ctlchar(x) \
  (0 == (pt.c_lflag & ICANON) || pt.c_cc[x] == _POSIX_VDISABLE) ? 0 : pt.c_cc[x]
		*p++ = X29_CHARACTER_DELETE; *p++ = ctlchar(VERASE);
		*p++ = X29_LINE_DELETE; *p++ = ctlchar(VKILL);
		*p++ = X29_LINE_DISPLAY; *p++ = ctlchar(VREPRINT);
	}
#undef ctlchar
	return (p - pibuf);
}

/* Have to be careful writing to pty.  The pad will forward control
 * characters without necessarily sending an interrupt so if ISIG and
 * ICANNON are set, must inspect line for quit or interrupt or suspend.
 */
ptywrite(buf, n)
char *buf;
int n;
{
	register char *cp, *lim;
	char *last;
#define is_ctl(x) (pt.c_cc[x] == *(cc_t *)cp)

	if ((pt.c_lflag & EXTPROC) && (pt.c_lflag & ISIG)) {
		for (cp = buf, lim = buf + n; cp < lim; cp ++) {
			if (is_ctl(VLNEXT))
				{ cp++; continue; }
			if (is_ctl(VSUSP) || is_ctl(VDSUSP) || 
			    is_ctl(VINTR) || is_ctl(VQUIT))  {
				int onoff = 0;
				tcflag_t old_echo = pt.c_lflag & ECHO;

				ioctl(pty, TIOCPKT, (char *)&onoff);
				ioctl(pty, FIONBIO, (char *)&onoff);
				ioctl(pty, TIOCEXT, (char *)&onoff);
				if (old_echo) {
					pt.c_lflag &= ~ECHO;
					ioctl(pty, TIOCSETA, (char *)&pt);
				}
				n = write(pty, buf, n);
				onoff = 1;
				if (old_echo) {
					pt.c_lflag |= ECHO;
					ioctl(pty, TIOCSETA, (char *)&pt);
				}
				ioctl(pty, TIOCEXT, (char *)&onoff);
				ioctl(pty, FIONBIO, (char *)&onoff);
				ioctl(pty, TIOCPKT, (char *)&onoff);
				return (n);
			}
		}
	}
	return write(pty, buf, n);
}

/*
 * Process Q BIT (control) packets from the net.
 * The only message that we are interested in are
 * those indicating output is being discarded.
 */

x29_qbit(n)
{
	register char *p;

	switch (fibuf[1]) {
	case X29_SET_PARMS:
	case X29_SET_AND_READ_PARMS:
	case X29_PARAMETER_INDICATION:
	case X29_INDICATION_OF_BREAK:
		for (p = &fibuf[2]; p < fibuf+n; p++) {
			if (*p == X29_TRANSMISSION_SPEED_CODE) {
				static char speeds[] = {
					B110, B0, B300, B1200, B600,
					B0, B0, B0, B0, B0, B0, B0,
					B2400, B4800, B9600, EXTA };

				if (*++p >= 0 && *p < sizeof(speeds)) {
					cfsetspeed(&pt, speeds[*p]);
					tcsetattr(pty, TCSANOW, &pt);
				}
			} else if (*p == X29_DISCARD_OUTPUT_CODE && *++p != 0) {
				char message[4];

				/*
				 * Always re-enable normal output
				 */
				message[0] = Q_BIT;
				message[1] = X29_SET_PARMS;
				message[2] = X29_DISCARD_OUTPUT_CODE;
				message[3] = 0;
				(void) write(net, message, sizeof(message));
				if (tracefn)
					x29d_trace("netwrite", message, 4);
			}
		}
		return;

	default: {
			register char *p2;
			char buf[BUFSIZ*4];
			static int fd;

			/*
			 * Bad news - we received an x29 error message or
			 * some other unknown packet.  Dump the contents
			 * of the packet on the console.
			 */
			p = buf;
			for (p2 = "x29d: unknown q-bit packet: "; *p++ = *p2++; );
			for (p2 = fibuf+1; p2 < fibuf+n; p2++)
				if (*p2 >= ' ' && *p2 < 0177)
					*p++ = *p2;
				else {
					*p++ = '\\';
					*p++ = ((*p2 & 0300) >> 6) + '0';
					*p++ = ((*p2 & 070) >> 3) + '0';
					*p++ = (*p2 & 07) + '0';
				}
			*p++ = '\n';
			if (fd <= 0)
				fd = open(console, 1);
			(void) write(fd, buf, p-buf);
		}
	}
}

x29d_trace(s, bp, n)
char *s, *bp;
{
	static int fd;
	char buf[BUFSIZ*4];
	register char *p1, *p2;

	for (p1 = buf; *s; *p1++ = *s++);
	*p1++ = ':';
	*p1++ = ' ';
	for (p2=bp; p2 < bp+n; p2++)
		if (*p2 >= ' ' && *p2 < 0177)
			*p1++ = *p2;
		else {
			*p1++ = '\\';
			*p1++ = ((*p2 & 0300) >> 6) + '0';
			*p1++ = ((*p2 & 070) >> 3) + '0';
			*p1++ = (*p2 & 07) + '0';
		}
	*p1++ = '\n';
	if (fd <= 0)
		fd = creat(tracefn, 0666);
	(void) write(fd, buf, p1-buf);
}
