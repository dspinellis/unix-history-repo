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
static char sccsid[] = "@(#)telnet.c	5.10 (Berkeley) %G%";
#endif not lint

/*
 * User telnet program.
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <sys/time.h>

#include <netinet/in.h>

#define	TELOPTS
#include <arpa/telnet.h>
#include <arpa/inet.h>

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <setjmp.h>
#include <netdb.h>
#include <strings.h>



/*
 * The following is defined just in case someone should want to run
 * this telnet on a 4.2 system.
 *
 * This has never been tested, so good luck...
 */
#ifndef	FD_SETSIZE

typedef long	fd_set;

#define	FD_SET(n, p)	(*(p) |= (1<<(n)))
#define	FD_CLR(n, p)	(*(p) &= ~(1<<(n)))
#define	FD_ISSET(n, p)	(*(p) & (1<<(n)))
#define FD_ZERO(p)	(*(p) = 0)

#endif

#define	strip(x)	((x)&0x3f)

char	ttyobuf[BUFSIZ], *tfrontp = ttyobuf, *tbackp = ttyobuf;
#define	TTYADD(c)	{ if (!SYNCHing) { *tfrontp++ = c; } }

char	netobuf[BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;
#define	NETADD(c)	{ *nfrontp++ = c; }
#define	NET2ADD(c1,c2)	{ NETADD(c1); NETADD(c2); }
#define NETLOC()	(nfrontp)
char	*neturg = 0;		/* one past last byte of urgent data */

char	hisopts[256];
char	myopts[256];

char	doopt[] = { IAC, DO, '%', 'c', 0 };
char	dont[] = { IAC, DONT, '%', 'c', 0 };
char	will[] = { IAC, WILL, '%', 'c', 0 };
char	wont[] = { IAC, WONT, '%', 'c', 0 };

struct cmd {
	char	*name;		/* command name */
	char	*help;		/* help string */
	int	(*handler)();	/* routine which executes command */
	int	dohelp;		/* Should we give general help information? */
	int	needconnect;	/* Do we need to be connected to execute? */
};

int	connected;
int	net;
int	tout;
int	showoptions = 0;
int	debug = 0;
int	crmod = 0;
int	netdata = 0;
static FILE	*NetTrace;
int	telnetport = 1;


char	*prompt;
char	escape = CTRL(]);
char	echoc = CTRL(E);

int	SYNCHing = 0;		/* we are in TELNET SYNCH mode */
int	flushout = 0;		/* flush output */
int	autosynch = 0;		/* send interrupt characters with SYNCH? */
int	localsigs = 0;		/* we recognize interrupt/quit */
int	donelclsigs = 0;	/* the user has set "localsigs" */
int	doechocharrecognition = 1;	/* in line mode recognize echo toggle */
int	dontlecho = 0;		/* do we suppress local echoing right now? */

char	line[200];
int	margc;
char	*margv[20];

jmp_buf	toplevel;
jmp_buf	peerdied;

extern	int errno;


struct sockaddr_in sin;

struct	cmd *getcmd();
struct	servent *sp;

struct	tchars otc, ntc;
struct	ltchars oltc;
struct	sgttyb ottyb, nttyb;
int	globalmode = 0;
int	flushline = 1;

char	*hostname;
char	hnamebuf[32];

/*
 * The following are some clocks used to decide how to interpret
 * the relationship between various variables.
 */

struct {
    int
	system,			/* what the current time is */
	echotoggle,		/* last time user entered echo character */
	modenegotiated,		/* last time operating mode negotiated */
	didnetreceive,		/* last time we read data from network */
	gotDM;			/* when did we last see a data mark */
} clocks;

#define	settimer(x)	clocks.x = clocks.system++

/*
 * Various utility routines.
 */

char *ambiguous;		/* special return value */
#define Ambiguous(t)	((t)&ambiguous)


char **
genget(name, table, next)
char	*name;		/* name to match */
char	**table;		/* name entry in table */
char	**(*next)();	/* routine to return next entry in table */
{
	register char *p, *q;
	register char **c, **found;
	register int nmatches, longest;

	longest = 0;
	nmatches = 0;
	found = 0;
	for (c = table; p = *c; c = (*next)(c)) {
		for (q = name; *q == *p++; q++)
			if (*q == 0)		/* exact match? */
				return (c);
		if (!*q) {			/* the name was a prefix */
			if (q - name > longest) {
				longest = q - name;
				nmatches = 1;
				found = c;
			} else if (q - name == longest)
				nmatches++;
		}
	}
	if (nmatches > 1)
		return Ambiguous(char **);
	return (found);
}

/*
 * Make a character string into a number.
 *
 * Todo:  1.  Could take random integers (12, 0x12, 012, 0b1).
 */

special(s)
register char *s;
{
	register char c;
	char b;

	switch (*s) {
	case '^':
		b = *++s;
		if (b == '?') {
		    c = b | 0x80;		/* DEL */
		} else {
		    c = b & 0x1f;
		}
		break;
	default:
		c = *s;
		break;
	}
	return c;
}

/*
 * Construct a control character sequence
 * for a special character.
 */
char *
control(c)
	register int c;
{
	static char buf[3];

	if (c == 0x3f)
		return ("^?");
	if (c == '\377') {
		return "off";
	}
	if (c >= 0x20) {
		buf[0] = c;
		buf[1] = 0;
	} else {
		buf[0] = '^';
		buf[1] = '@'+c;
		buf[2] = 0;
	}
	return (buf);
}

/*
 * Check to see if any out-of-band data exists on a socket (for
 * Telnet "synch" processing).
 */

int
stilloob(s)
int	s;		/* socket number */
{
    static struct timeval timeout = { 0 };
    fd_set	excepts;
    int value;

    do {
	FD_ZERO(&excepts);
	FD_SET(s, &excepts);
	value = select(s+1, (fd_set *)0, (fd_set *)0, &excepts, &timeout);
    } while ((value == -1) && (errno = EINTR));

    if (value < 0) {
	perror("select");
	quit();
    }
    if (FD_ISSET(s, &excepts)) {
	return 1;
    } else {
	return 0;
    }
}


/*
 *  netflush
 *		Send as much data as possible to the network,
 *	handling requests for urgent data.
 */


netflush(fd)
{
    int n;

    if ((n = nfrontp - nbackp) > 0) {
	if (!neturg) {
	    n = write(fd, nbackp, n);	/* normal write */
	} else {
	    n = neturg - nbackp;
	    /*
	     * In 4.2 (and 4.3) systems, there is some question about
	     * what byte in a sendOOB operation is the "OOB" data.
	     * To make ourselves compatible, we only send ONE byte
	     * out of band, the one WE THINK should be OOB (though
	     * we really have more the TCP philosophy of urgent data
	     * rather than the Unix philosophy of OOB data).
	     */
	    if (n > 1) {
		n = send(fd, nbackp, n-1, 0);	/* send URGENT all by itself */
	    } else {
		n = send(fd, nbackp, n, MSG_OOB);	/* URGENT data */
	    }
	}
    }
    if (n < 0) {
	if (errno != ENOBUFS && errno != EWOULDBLOCK) {
	    setcommandmode();
	    perror(hostname);
	    close(fd);
	    neturg = 0;
	    longjmp(peerdied, -1);
	    /*NOTREACHED*/
	}
	n = 0;
    }
    if (netdata && n) {
	Dump('>', nbackp, n);
    }
    nbackp += n;
    if (nbackp >= neturg) {
	neturg = 0;
    }
    if (nbackp == nfrontp) {
	nbackp = nfrontp = netobuf;
    }
}

/*
 * Send as much data as possible to the terminal.
 */


ttyflush()
{
    int n;

    if ((n = tfrontp - tbackp) > 0) {
	if (!SYNCHing) {
	    n = write(tout, tbackp, n);
	} else {
	    ioctl(fileno(stdout), TIOCFLUSH, (char *) 0);
	}
    }
    if (n < 0) {
	return;
    }
    tbackp += n;
    if (tbackp == tfrontp) {
	tbackp = tfrontp = ttyobuf;
    }
}

/*
 * Various signal handling routines.
 */

deadpeer()
{
	setcommandmode();
	longjmp(peerdied, -1);
}

intr()
{
    if (localsigs) {
	intp();
	if (autosynch) {
	    dosynch();
	}
	return;
    }
    setcommandmode();
    longjmp(toplevel, -1);
}

intr2()
{
    if (localsigs) {
	sendbrk();
	if (autosynch) {
	    dosynch();
	}
	return;
    }
}

doescape()
{
    command(0);
}

/*
 * The following are routines used to print out debugging information.
 */


static
Dump(direction, buffer, length)
char	direction;
char	*buffer;
int	length;
{
#   define BYTES_PER_LINE	32
#   define min(x,y)	((x<y)? x:y)
    char *pThis;
    int offset;

    offset = 0;

    while (length) {
	/* print one line */
	fprintf(NetTrace, "%c 0x%x\t", direction, offset);
	pThis = buffer;
	buffer = buffer+min(length, BYTES_PER_LINE);
	while (pThis < buffer) {
	    fprintf(NetTrace, "%.2x", (*pThis)&0xff);
	    pThis++;
	}
	fprintf(NetTrace, "\n");
	length -= BYTES_PER_LINE;
	offset += BYTES_PER_LINE;
	if (length < 0) {
	    return;
	}
	/* find next unique line */
    }
}


/*VARARGS*/
printoption(direction, fmt, option, what)
	char *direction, *fmt;
	int option, what;
{
	if (!showoptions)
		return;
	printf("%s ", direction);
	if (fmt == doopt)
		fmt = "do";
	else if (fmt == dont)
		fmt = "dont";
	else if (fmt == will)
		fmt = "will";
	else if (fmt == wont)
		fmt = "wont";
	else
		fmt = "???";
	if (option < TELOPT_SUPDUP)
		printf("%s %s", fmt, telopts[option]);
	else
		printf("%s %d", fmt, option);
	if (*direction == '<') {
		printf("\r\n");
		return;
	}
	printf(" (%s)\r\n", what ? "reply" : "don't reply");
}

/*
 * Mode - set up terminal to a specific mode.
 */

struct	tchars notc =	{ -1, -1, -1, -1, -1, -1 };
struct	tchars notc2;
struct	ltchars noltc =	{ -1, -1, -1, -1, -1, -1 };
struct	ltchars noltc2;

mode(f)
	register int f;
{
	static int prevmode = 0;
	struct tchars *tc;
	struct ltchars *ltc;
	struct sgttyb sb;
	int onoff, old;

	globalmode = f;
	if (prevmode == f)
		return;
	old = prevmode;
	prevmode = f;
	sb = nttyb;
	switch (f) {

	case 0:
		onoff = 0;
		tc = &otc;
		ltc = &oltc;
		break;

	case 1:		/* remote character processing, remote echo */
	case 2:		/* remote character processing, local echo */
		sb.sg_flags |= CBREAK;
		if (f == 1)
			sb.sg_flags &= ~(ECHO|CRMOD);
		else
			sb.sg_flags |= ECHO|CRMOD;
		sb.sg_erase = sb.sg_kill = -1;
		tc = &notc;
		/*
		 * If user hasn't specified one way or the other,
		 * then default to not trapping signals.
		 */
		if (!donelclsigs)
			localsigs = 0;
		if (localsigs) {
			notc2 = notc;
			notc2.t_intrc = ntc.t_intrc;
			notc2.t_quitc = ntc.t_quitc;
			notc2.t_eofc = ntc.t_eofc;
			tc = &notc2;
		} else
			tc = &notc;
		ltc = &noltc;
		onoff = 1;
		break;
	case 3:		/* local character processing, remote echo */
	case 4:		/* local character processing, local echo */
	case 5:		/* local character processing, no echo */
		sb.sg_flags &= ~CBREAK;
		sb.sg_flags |= CRMOD;
		if (f == 4)
			sb.sg_flags |= ECHO;
		else
			sb.sg_flags &= ~ECHO;
		/*
		 * If user hasn't specified one way or the other,
		 * then default to trapping signals.
		 */
		if (!donelclsigs)
			localsigs = 1;
		if (localsigs)
			tc = &ntc;
		else {
			notc2 = ntc;
			notc2.t_intrc = notc2.t_quitc = -1;
			tc = &notc2;
		}
		noltc2 = oltc;
		noltc2.t_suspc = escape;
		noltc2.t_dsuspc = -1;
		ltc = &noltc2;
		onoff = 1;
		break;

	default:
		return;
	}
	ioctl(fileno(stdin), TIOCSLTC, (char *)ltc);
	ioctl(fileno(stdin), TIOCSETC, (char *)tc);
	ioctl(fileno(stdin), TIOCSETP, (char *)&sb);
	ioctl(fileno(stdin), FIONBIO, (char *)&onoff);
	ioctl(fileno(stdout), FIONBIO, (char *)&onoff);
	if (f >= 3)
		signal(SIGTSTP, doescape);
	else if (old >= 3) {
		signal(SIGTSTP, SIG_DFL);
		sigsetmask(sigblock(0) & ~(1<<(SIGTSTP-1)));
	}
}

/*
 * These routines decides on what the mode should be (based on the values
 * of various global variables).
 */

char *modedescriptions[] = {
	"telnet command mode",					/* 0 */
	"character-at-a-time mode",				/* 1 */
	"character-at-a-time mode (local echo)",		/* 2 */
	"line-by-line mode (remote echo)",			/* 3 */
	"line-by-line mode",					/* 4 */
	"line-by-line mode (local echoing suppressed)",		/* 5 */
};

getconnmode()
{
    static char newmode[8] = { 4, 5, 3, 3, 2, 2, 1, 1 };
    int modeindex = 0;

    if (hisopts[TELOPT_ECHO]) {
	modeindex += 2;
    }
    if (hisopts[TELOPT_SGA]) {
	modeindex += 4;
    }
    if (dontlecho && (clocks.echotoggle > clocks.modenegotiated)) {
	modeindex += 1;
    }
    return newmode[modeindex];
}

setconnmode()
{
    mode(getconnmode());
}


setcommandmode()
{
    mode(0);
}

char	sibuf[BUFSIZ], *sbp;
char	tibuf[BUFSIZ], *tbp;
int	scc, tcc;

/*
 * Select from tty and network...
 */
telnet()
{
	register int c;
	int tin = fileno(stdin);
	int on = 1;

	tout = fileno(stdout);
	setconnmode();
	ioctl(net, FIONBIO, (char *)&on);
#if	defined(IOCTL_TO_DO_UNIX_OOB_IN_TCP_WAY)
	ioctl(net, asdf, asdf);		/* handle urgent data in band */
#endif	/* defined(IOCTL_TO_DO_UNIX_OOB_IN_TCP_WAY) */
	if (telnetport && !hisopts[TELOPT_SGA]) {
		willoption(TELOPT_SGA);
	}
	for (;;) {
		fd_set ibits, obits, xbits;

		if (scc < 0 && tcc < 0) {
			break;
		}

		FD_ZERO(&ibits);
		FD_ZERO(&obits);
		FD_ZERO(&xbits);

		if (((globalmode < 4) || flushline) && (nfrontp - nbackp)) {
			FD_SET(net, &obits);
		} else {
			FD_SET(tin, &ibits);
		}
		if (tfrontp - tbackp) {
			FD_SET(tout, &obits);
		} else {
			FD_SET(net, &ibits);
		}
		if (!SYNCHing) {
			FD_SET(net, &xbits);
		}
		if ((c = select(16, &ibits, &obits, &xbits,
						(struct timeval *)0)) < 1) {
			if (c == -1) {
				/*
				 * we can get EINTR if we are in line mode,
				 * and the user does an escape (TSTP), or
				 * some other signal generator.
				 */
				if (errno == EINTR) {
					continue;
				}
			}
			sleep(5);
			continue;
		}

		/*
		 * Any urgent data?
		 */
		if (FD_ISSET(net, &xbits)) {
		    SYNCHing = 1;
		    ttyflush();	/* flush already enqueued data */
		}

		/*
		 * Something to read from the network...
		 */
		if (FD_ISSET(net, &ibits)) {
#if	!defined(IOCTL_TO_DO_UNIX_OOB_IN_TCP_WAY)
			/*
			 * In 4.2 (and some early 4.3) systems, the
			 * OOB indication and data handling in the kernel
			 * is such that if two separate TCP Urgent requests
			 * come in, one byte of TCP data will be overlaid.
			 * This is fatal for Telnet, but we try to live
			 * with it.
			 *
			 * In addition, in 4.2 (and...), a special protocol
			 * is needed to pick up the TCP Urgent data in
			 * the correct sequence.
			 *
			 * What we do is:  if we think we are in urgent
			 * mode, we look to see if we are "at the mark".
			 * If we are, we do an OOB receive.  If we run
			 * this twice, we will do the OOB receive twice,
			 * but the second will fail, since the second
			 * time we were "at the mark", but there wasn't
			 * any data there (the kernel doesn't reset
			 * "at the mark" until we do a normal read).
			 * Once we've read the OOB data, we go ahead
			 * and do normal reads.
			 *
			 * There is also another problem, which is that
			 * since the OOB byte we read doesn't put us
			 * out of OOB state, and since that byte is most
			 * likely the TELNET DM (data mark), we would
			 * stay in the TELNET SYNCH (SYNCHing) state.
			 * So, clocks to the rescue.  If we've "just"
			 * received a DM, then we test for the
			 * presence of OOB data when the receive OOB
			 * fails (and AFTER we did the normal mode read
			 * to clear "at the mark").
			 */
		    if (SYNCHing) {
			int atmark;

			ioctl(net, SIOCATMARK, (char *)&atmark);
			if (atmark) {
			    scc = recv(net, sibuf, sizeof (sibuf), MSG_OOB);
			    if ((scc == -1) && (errno == EINVAL)) {
				scc = read(net, sibuf, sizeof (sibuf));
				if (clocks.didnetreceive < clocks.gotDM) {
				    SYNCHing = stilloob(net);
				}
			    }
			} else {
			    scc = read(net, sibuf, sizeof (sibuf));
			}
		    } else {
			scc = read(net, sibuf, sizeof (sibuf));
		    }
		    settimer(didnetreceive);
#else	/* !defined(IOCTL_TO_DO_UNIX_OOB_IN_TCP_WAY) */
		    scc = read(net, sibuf, sizeof (sibuf));
#endif	/* !defined(IOCTL_TO_DO_UNIX_OOB_IN_TCP_WAY) */
		    if (scc < 0 && errno == EWOULDBLOCK)
			scc = 0;
		    else {
			if (scc <= 0) {
			    break;
			}
			sbp = sibuf;
			if (netdata) {
			    Dump('<', sbp, scc);
			}
		    }
		}

		/*
		 * Something to read from the tty...
		 */
		if (FD_ISSET(tin, &ibits)) {
			tcc = read(tin, tibuf, sizeof (tibuf));
			if (tcc < 0 && errno == EWOULDBLOCK)
				tcc = 0;
			else {
				if (tcc <= 0)
					break;
				tbp = tibuf;
			}
		}

		while (tcc > 0) {
			register int sc;

			if ((&netobuf[BUFSIZ] - nfrontp) < 2) {
				flushline = 1;
				break;
			}
			c = *tbp++ & 0xff, sc = strip(c), tcc--;
			if (sc == escape) {
				command(0);
				tcc = 0;
				flushline = 1;
				break;
			} else if ((globalmode >= 4) && doechocharrecognition &&
							(sc == echoc)) {
				if (tcc > 0 && strip(*tbp) == echoc) {
					tbp++;
					tcc--;
				} else {
					dontlecho = !dontlecho;
					settimer(echotoggle);
					setconnmode();
					tcc = 0;
					flushline = 1;
					break;
				}
			}
			if (localsigs) {
				if (sc == ntc.t_intrc) {
					intp();
					break;
				} else if (sc == ntc.t_quitc) {
					sendbrk();
					break;
				} else if (globalmode > 2) {
					;
				} else if (sc == nttyb.sg_kill) {
					NET2ADD(IAC, EL);
					break;
				} else if (sc == nttyb.sg_erase) {
					NET2ADD(IAC, EC);
					break;
				}
			}
			switch (c) {
			case '\n':
				/*
				 * If echoing is happening locally,
				 * then a newline (unix) is CRLF (TELNET).
				 */
				if (!hisopts[TELOPT_ECHO]) {
					NETADD('\r');
				}
				NETADD('\n');
				flushline = 1;
				break;
			case '\r':
				NET2ADD('\r', '\0');
				flushline = 1;
				break;
			case IAC:
				NET2ADD(IAC, IAC);
				break;
			default:
				NETADD(c);
				break;
			}
		}
		if (((globalmode < 4) || flushline) &&
		    (FD_ISSET(net, &obits) && (nfrontp - nbackp) > 0)) {
			netflush(net);
		}
		if (scc > 0)
			telrcv();
		if (FD_ISSET(tout, &obits) && (tfrontp - tbackp) > 0)
			ttyflush();
	}
	setcommandmode();
}

/*
 * Telnet receiver states for fsm
 */
#define	TS_DATA		0
#define	TS_IAC		1
#define	TS_WILL		2
#define	TS_WONT		3
#define	TS_DO		4
#define	TS_DONT		5
#define	TS_CR		6

telrcv()
{
	register int c;
	static int state = TS_DATA;

	while (scc > 0) {
		c = *sbp++ & 0xff, scc--;
		switch (state) {

		case TS_CR:
			state = TS_DATA;
			if ((c == '\0') || (c == '\n')) {
			    break;	/* by now, we ignore \n */
			}

		case TS_DATA:
			if (c == IAC) {
				state = TS_IAC;
				continue;
			}
			if (c == '\r') {
				if (scc > 0) {
					c = *sbp&0xff;
					if (c == 0) {
						sbp++, scc--;
						TTYADD('\r');
				/*
				 * The following hack is needed since we can't
				 * set CRMOD on output only.  Machines like
				 * MULTICS like to send \r without \n; since
				 * we must turn off CRMOD to get proper input,
				 * the mapping is done here (sigh).
				 */
						if (crmod) {
							TTYADD('\n');
						}
					} else if (!hisopts[TELOPT_ECHO] &&
								(c == '\n')) {
						sbp++, scc--;
						TTYADD('\n');
					} else {
						TTYADD('\r');
					}
				} else {
					state = TS_CR;
					TTYADD('\r');
				}
			} else {
				TTYADD(c);
			}
			continue;

		case TS_IAC:
			switch (c) {
			
			case WILL:
				state = TS_WILL;
				continue;

			case WONT:
				state = TS_WONT;
				continue;

			case DO:
				state = TS_DO;
				continue;

			case DONT:
				state = TS_DONT;
				continue;

			case DM:
				/*
				 * We may have missed an urgent notification,
				 * so make sure we flush whatever is in the
				 * buffer currently.
				 */
				SYNCHing = 1;
				ttyflush();
				SYNCHing = stilloob(net);
				settimer(gotDM);
				break;

			case NOP:
			case GA:
				break;

			default:
				break;
			}
			state = TS_DATA;
			continue;

		case TS_WILL:
			printoption("RCVD", will, c, !hisopts[c]);
			if (c == TELOPT_TM) {
				if (flushout) {
					flushout = 0;
				}
			} else if (!hisopts[c]) {
				willoption(c);
			}
			state = TS_DATA;
			continue;

		case TS_WONT:
			printoption("RCVD", wont, c, hisopts[c]);
			if (c == TELOPT_TM) {
				if (flushout) {
					flushout = 0;
				}
			} else if (hisopts[c]) {
				wontoption(c);
			}
			state = TS_DATA;
			continue;

		case TS_DO:
			printoption("RCVD", doopt, c, !myopts[c]);
			if (!myopts[c])
				dooption(c);
			state = TS_DATA;
			continue;

		case TS_DONT:
			printoption("RCVD", dont, c, myopts[c]);
			if (myopts[c]) {
				myopts[c] = 0;
				sprintf(nfrontp, wont, c);
				nfrontp += sizeof (wont) - 2;
				flushline = 1;
				setconnmode();	/* set new tty mode (maybe) */
				printoption("SENT", wont, c);
			}
			state = TS_DATA;
			continue;
		}
	}
}

willoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_ECHO:
	case TELOPT_SGA:
		settimer(modenegotiated);
		hisopts[option] = 1;
		fmt = doopt;
		setconnmode();		/* possibly set new tty mode */
		break;

	case TELOPT_TM:
		return;			/* Never reply to TM will's/wont's */

	default:
		fmt = dont;
		break;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (dont) - 2;
	printoption("SENT", fmt, option);
}

wontoption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_ECHO:
	case TELOPT_SGA:
		settimer(modenegotiated);
		hisopts[option] = 0;
		fmt = dont;
		setconnmode();			/* Set new tty mode */
		break;

	case TELOPT_TM:
		return;		/* Never reply to TM will's/wont's */

	default:
		fmt = dont;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
	printoption("SENT", fmt, option);
}

dooption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_TM:
		fmt = will;
		break;

	case TELOPT_SGA:		/* no big deal */
		fmt = will;
		myopts[option] = 1;
		break;

	case TELOPT_ECHO:		/* We're never going to echo... */
	default:
		fmt = wont;
		break;
	}
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
	printoption("SENT", fmt, option);
}

/*
 *	The following are data structures and routines for
 *	the "send" command.
 *
 */
 
struct sendlist {
    char	*name;		/* How user refers to it (case independent) */
    int		what;		/* Character to be sent (<0 ==> special) */
    char	*help;		/* Help information (0 ==> no help) */
    int		(*routine)();	/* Routine to perform (for special ops) */
};

/*ARGSUSED*/
dosynch(s)
struct sendlist *s;
{
    /* XXX We really should purge the buffer to the network */
    NET2ADD(IAC, DM);
    neturg = NETLOC()-1;	/* Some systems are off by one XXX */
}

intp()
{
    NET2ADD(IAC, IP);
}

sendbrk()
{
#if	0
    /*
     * There is a question here.  Should we send a TM to flush the stream?
     * Should we also send a TELNET SYNCH also?
     */
    *nfrontp++ = IAC;
    *nfrontp++ = DO;
    *nfrontp++ = TELOPT_TM;
    flushout = 1;
    printoption("SENT", doopt, TELOPT_TM);
#endif	/* 0 */
    NET2ADD(IAC, BREAK);
    flushline = 1;
}


#define	SENDQUESTION	-1
#define	SEND2QUESTION	-2
#define	SENDESCAPE	-3

struct sendlist Sendlist[] = {
    { "synch", SYNCH, "Perform Telnet 'Synch operation'", dosynch },
    { "brk", BREAK, "Send Telnet Break" },
	{ "break", BREAK, 0 },
    { "ip", IP, "Send Telnet Interrupt Process" },
	{ "intp", IP, 0 },
	{ "interrupt", IP, 0 },
	{ "intr", IP, 0 },
    { "ao", AO, "Send Telnet Abort output" },
	{ "abort", AO, 0 },
    { "ayt", AYT, "Send Telnet 'Are You There'" },
	{ "are", AYT, 0 },
	{ "hello", AYT, 0 },
    { "ec", EC, "Send Telnet Erase Character" },
    { "el", EL, "Send Telnet Erase Line" },
    { "ga", GA, "Send Telnet 'Go Ahead' sequence" },
	{ "go", GA, 0 },
    { "nop", NOP, "Send Telnet 'No operation'" },
    { "escape", SENDESCAPE, "Send current escape character" },
    { "?", SENDQUESTION, "Display send options" },
	{ "help", SENDQUESTION, 0 },
    { "??", SEND2QUESTION, "Display all send options (including aliases)" },
    { 0 }
};

char **
getnextsend(name)
char *name;
{
    struct sendlist *c = (struct sendlist *) name;

    return (char **) (c+1);
}

struct sendlist *
getsend(name)
char *name;
{
    return (struct sendlist *) genget(name, (char **) Sendlist, getnextsend);
}

sendcmd(argc, argv)
int	argc;
char	**argv;
{
    int what;		/* what we are sending this time */
    int count;		/* how many bytes we are going to need to send */
    int hadsynch;	/* are we going to process a "synch"? */
    int i;
    struct sendlist *s;	/* pointer to current command */

    if (argc < 2) {
	printf("need at least one argument for 'send' command\n");
	printf("'send ?' for help\n");
	return;
    }
    /*
     * First, validate all the send arguments.
     * In addition, we see how much space we are going to need, and
     * whether or not we will be doing a "SYNCH" operation (which
     * flushes the network queue).
     */
    count = 0;
    hadsynch = 0;
    for (i = 1; i < argc; i++) {
	s = getsend(argv[i]);
	if (s == 0) {
	    printf("Unknown send argument '%s'\n'send ?' for help.\n",
			argv[i]);
	    return;
	} else if (s == Ambiguous(struct sendlist *)) {
	    printf("Ambiguous send argument '%s'\n'send ?' for help.\n",
			argv[i]);
	    return;
	}
	switch (s->what) {
	case SENDQUESTION:
	case SEND2QUESTION:
	    break;
	case SENDESCAPE:
	    count += 1;
	    break;
	case SYNCH:
	    hadsynch = 1;
	    count += 2;
	    break;
	default:
	    count += 2;
	    break;
	}
    }
    /* Now, do we have enough room? */
    if (netobuf+sizeof netobuf-nfrontp-1 < count) {
	printf("There is not enough room in the buffer TO the network\n");
	printf("to process your request.  Nothing will be done.\n");
	printf("('send synch' will throw away most data in the network\n");
	printf("buffer, if this might help.)\n");
	return;
    }
    /* OK, they are all OK, now go through again and actually send */
    for (i = 1; i < argc; i++) {
	if (!(s = getsend(argv[i]))) {
	    fprintf(stderr, "Telnet 'send' error - argument disappeared!\n");
	    quit();
	    /*NOTREACHED*/
	}
	if (s->routine) {
	    (*s->routine)(s);
	} else {
	    switch (what = s->what) {
	    case SENDQUESTION:
	    case SEND2QUESTION:
		for (s = Sendlist; s->name; s++) {
		    if (s->help || (what == SEND2QUESTION)) {
			printf(s->name);
			if (s->help) {
			    printf("\t%s", s->help);
			}
			printf("\n");
		    }
		}
		break;
	    case SENDESCAPE:
		NETADD(escape);
		break;
	    default:
		NET2ADD(IAC, what);
		break;
	    }
	}
    }
}

/*
 * The following are the routines and data structures referred
 * to by the arguments to the "toggle" command.
 */

lclsigs()
{
    donelclsigs = 1;
}

/*VARARGS*/
togcrmod()
{
    crmod = !crmod;
    printf("Deprecated usage - please use 'toggle crmod' in the future.\n");
    printf("%s map carriage return on output.\n", crmod ? "Will" : "Won't");
    fflush(stdout);
}

togdebug()
{
    if (net > 0 &&
	setsockopt(net, SOL_SOCKET, SO_DEBUG, (char *)&debug, sizeof(debug))
									< 0) {
	    perror("setsockopt (SO_DEBUG)");
    }
}



int togglehelp();

char	crmodhelp[] =	"toggle mapping of received carriage returns";

struct togglelist {
    char	*name;		/* name of toggle */
    char	*help;		/* help message */
    int		(*handler)();	/* routine to do actual setting */
    int		dohelp;		/* should we display help information */
    int		*variable;
    char	*actionexplanation;
};

struct togglelist Togglelist[] = {
    { "localchars",
	"toggle local recognition of control characters",
	    lclsigs,
		1,
		    &localsigs,
			"recognize interrupt/quit characters" },
    { "echochar",
	"toggle recognition of echo toggle character",
	    0,
		1,
		    &doechocharrecognition,
			"recognize echo toggle character" },
    { "autosynch",
	"toggle automatic sending of interrupt characters in urgent mode",
	    0,
		1,
		    &autosynch,
			"send interrupt characters in urgent mode" },
    { "crmod",
	crmodhelp,
	    0,
		1,
		    &crmod,
			"map carriage return on output" },
    { " ", "", 0, 1 },		/* empty line */
    { "debug",
	"(debugging) toggle debugging",
	    togdebug,
		1,
		    &debug,
			"turn on socket level debugging" },
    { "options",
	"(debugging) toggle viewing of options processing",
	    0,
		1,
		    &showoptions,
			"show option processing" },
    { "netdata",
	"(debugging) toggle printing of hexadecimal network data",
	    0,
		1,
		    &netdata,
			"print hexadecimal representation of network traffic" },
    { "?",
	"display help information",
	    togglehelp,
		1 },
    { "help",
	"display help information",
	    togglehelp,
		0 },
    { 0 }
};

togglehelp()
{
    struct togglelist *c;

    for (c = Togglelist; c->name; c++) {
	if (c->dohelp) {
	    printf("%s\t%s\n", c->name, c->help);
	}
    }
}

char **
getnexttoggle(name)
char *name;
{
    struct togglelist *c = (struct togglelist *) name;

    return (char **) (c+1);
}

struct togglelist *
gettoggle(name)
char *name;
{
    return (struct togglelist *)
			genget(name, (char **) Togglelist, getnexttoggle);
}

toggle(argc, argv)
int	argc;
char	*argv[];
{
    char *name;
    struct togglelist *c;

    if (argc < 2) {
	fprintf(stderr,
	    "Need an argument to 'toggle' command.  'toggle ?' for help.\n");
	return;
    }
    argc--;
    argv++;
    while (argc--) {
	name = *argv++;
	c = gettoggle(name);
	if (c == Ambiguous(struct togglelist *)) {
	    fprintf(stderr, "'%s': ambiguous argument ('toggle ?' for help).\n",
					name);
	} else if (c == 0) {
	    fprintf(stderr, "'%s': unknown argument ('toggle ?' for help).\n",
					name);
	} else {
	    if (c->variable) {
		*c->variable = !*c->variable;		/* invert it */
		printf("%s %s.\n", *c->variable? "Will" : "Won't",
							c->actionexplanation);
	    }
	    if (c->handler) {
		(*c->handler)(c);
	    }
	}
    }
}

/*
 * The following perform the "set" command.
 */

struct setlist {
    char *name;				/* name */
    char *help;				/* help information */
    char *charp;			/* where it is located at */
};

struct setlist Setlist[] = {
    { "echo", 	"character to toggle local echoing on/off", &echoc },
    { "escape",	"character to escape back to telnet command mode", &escape },
    { "interrupt", "character to cause an Interrupt Process", &ntc.t_intrc },
    { "quit",	"character to cause a Break", &ntc.t_quitc },
    { "erase",	"character to cause an Erase Character", &nttyb.sg_erase },
    { "kill",	"character to cause an Erase Line", &nttyb.sg_kill },
    { 0 }
};

char **
getnextset(name)
char *name;
{
    struct setlist *c = (struct setlist *)name;

    return (char **) (c+1);
}

struct setlist *
getset(name)
char *name;
{
    return (struct setlist *) genget(name, (char **) Setlist, getnextset);
}

setcmd(argc, argv)
int	argc;
char	*argv[];
{
    int value;
    struct setlist *ct;

    /* XXX back we go... sigh */
    if (argc != 3) {
	printf("Format is 'set Name Value', where 'Name' is one of:\n\n");
	for (ct = Setlist; ct->name; ct++) {
	    printf("%s\t%s\n", ct->name, ct->help);
	}
	return;
    }

    ct = getset(argv[1]);
    if (ct == 0) {
	fprintf(stderr, "'%s': unknown argument ('set ?' for help).\n",
			argv[1]);
    } else if (ct == Ambiguous(struct setlist *)) {
	fprintf(stderr, "'%s': ambiguous argument ('set ?' for help).\n",
			argv[1]);
    } else {
	if (strcmp("off", argv[2])) {
	    value = special(argv[2]);
	} else {
	    value = -1;
	}
	*(ct->charp) = value;
	printf("%s character is '%s'.\n", ct->name, control(*(ct->charp)));
    }
}

/*
 * The following are the data structures and routines for the
 * 'mode' command.
 */

dolinemode()
{
    if (hisopts[TELOPT_SGA]) {
	wontoption(TELOPT_SGA);
    }
    if (hisopts[TELOPT_ECHO]) {
	wontoption(TELOPT_ECHO);
    }
}

docharmode()
{
    if (!hisopts[TELOPT_SGA]) {
	willoption(TELOPT_SGA);
    }
    if (!hisopts[TELOPT_ECHO]) {
	willoption(TELOPT_ECHO);
    }
}

struct cmd Modelist[] = {
    { "line",		"line-by-line mode",		dolinemode, 1, 1 },
    { "character",	"character-at-a-time mode",	docharmode, 1, 1 },
    { 0 },
};

char **
getnextmode(name)
char *name;
{
    struct cmd *c = (struct cmd *) name;

    return (char **) (c+1);
}

struct cmd *
getmodecmd(name)
char *name;
{
    return (struct cmd *) genget(name, (char **) Modelist, getnextmode);
}

modecmd(argc, argv)
int	argc;
char	*argv[];
{
    struct cmd *mt;

    if ((argc != 2) || !strcmp(argv[1], "?") || !strcmp(argv[1], "help")) {
	printf("format is:  'mode Mode', where 'Mode' is one of:\n\n");
	for (mt = Modelist; mt->name; mt++) {
	    printf("%s\t%s\n", mt->name, mt->help);
	}
	return;
    }
    mt = getmodecmd(argv[1]);
    if (mt == 0) {
	fprintf(stderr, "Unknown mode '%s' ('mode ?' for help).\n", argv[1]);
    } else if (mt == Ambiguous(struct cmd *)) {
	fprintf(stderr, "Ambiguous mode '%s' ('mode ?' for help).\n", argv[1]);
    } else {
	(*mt->handler)();
    }
}

/*
 * The following data structures and routines implement the
 * "display" command.
 */

display(argc, argv)
int	argc;
char	*argv[];
{
#define	dotog(tl)	if (tl->variable && tl->actionexplanation) { \
			    if (*tl->variable) { \
				printf("will"); \
			    } else { \
				printf("won't"); \
			    } \
			    printf(" %s.\n", tl->actionexplanation); \
			}

#define	doset(sl)	printf("[%s]\t%s.\n", control(*sl->charp), sl->name);

    struct togglelist *tl;
    struct setlist *sl;

    if (argc == 1) {
	for (tl = Togglelist; tl->name; tl++) {
	    dotog(tl);
	}
	for (sl = Setlist; sl->name; sl++) {
	    doset(sl);
	}
    } else {
	int i;

	for (i = 1; i < argc; i++) {
	    sl = getset(argv[i]);
	    tl = gettoggle(argv[i]);
	    if ((sl == Ambiguous(struct setlist *)) ||
				(tl == Ambiguous(struct togglelist *))) {
		printf("?Ambiguous argument '%s'.\n", argv[i]);
	    } else if (!sl && !tl) {
		printf("?Unknown argument '%s'.\n", argv[i]);
	    } else {
		if (tl) {
		    dotog(tl);
		}
		if (sl) {
		    doset(sl);
		}
	    }
	}
    }
#undef	doset(sl)
#undef	dotog(tl)
}

/*
 * The following are the data structures, and many of the routines,
 * relating to command processing.
 */

/*
 * Set the escape character.
 */
setescape(argc, argv)
	int argc;
	char *argv[];
{
	register char *arg;
	char buf[50];

	printf(
	    "Deprecated usage - please use 'set escape%s%s' in the future.\n",
				(argc > 2)? " ":"", (argc > 2)? argv[1]: "");
	if (argc > 2)
		arg = argv[1];
	else {
		printf("new escape character: ");
		gets(buf);
		arg = buf;
	}
	if (arg[0] != '\0')
		escape = arg[0];
	printf("Escape character is '%s'.\n", control(escape));
	fflush(stdout);
}

/*VARARGS*/
suspend()
{
	setcommandmode();
	kill(0, SIGTSTP);
	/* reget parameters in case they were changed */
	ioctl(0, TIOCGETP, (char *)&ottyb);
	ioctl(0, TIOCGETC, (char *)&otc);
	ioctl(0, TIOCGLTC, (char *)&oltc);
}

/*VARARGS*/
bye()
{
	register char *op;

	if (connected) {
		shutdown(net, 2);
		printf("Connection closed.\n");
		close(net);
		connected = 0;
		/* reset his options */
		for (op = hisopts; op < &hisopts[256]; op++)
			*op = 0;
	}
}

/*VARARGS*/
quit()
{
	call(bye, "bye", 0);
	exit(0);
}

/*
 * Print status about the connection.
 */
/*ARGSUSED*/
status(argc, argv)
int	argc;
char	*argv[];
{
    if (connected) {
	printf("Connected to %s.\n", hostname);
	if (argc < 2) {
	    printf("Operating in %s.\n", modedescriptions[getconnmode()]);
	    if (localsigs || ((!donelclsigs) && (getconnmode() >= 3))) {
		printf("Catching signals locally.\n");
	    }
	}
    } else {
	printf("No connection.\n");
    }
    printf("Escape character is '%s'.\n", control(escape));
    fflush(stdout);
}

tn(argc, argv)
	int argc;
	char *argv[];
{
	register struct hostent *host = 0;

	if (connected) {
		printf("?Already connected to %s\n", hostname);
		return;
	}
	if (argc < 2) {
		(void) strcpy(line, "Connect ");
		printf("(to) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if (argc > 3) {
		printf("usage: %s host-name [port]\n", argv[0]);
		return;
	}
	sin.sin_addr.s_addr = inet_addr(argv[1]);
	if (sin.sin_addr.s_addr != -1) {
		sin.sin_family = AF_INET;
		(void) strcpy(hnamebuf, argv[1]);
		hostname = hnamebuf;
	} else {
		host = gethostbyname(argv[1]);
		if (host) {
			sin.sin_family = host->h_addrtype;
			bcopy(host->h_addr_list[0], (caddr_t)&sin.sin_addr,
				host->h_length);
			hostname = host->h_name;
		} else {
			printf("%s: unknown host\n", argv[1]);
			return;
		}
	}
	sin.sin_port = sp->s_port;
	if (argc == 3) {
		sin.sin_port = atoi(argv[2]);
		if (sin.sin_port == 0) {
			sp = getservbyname(argv[2], "tcp");
			if (sp)
				sin.sin_port = sp->s_port;
			else {
				printf("%s: bad port number\n", argv[2]);
				return;
			}
		} else {
			sin.sin_port = atoi(argv[2]);
			sin.sin_port = htons(sin.sin_port);
		}
		telnetport = 0;
	} else {
		telnetport = 1;
	}
	signal(SIGINT, intr);
	signal(SIGQUIT, intr2);
	signal(SIGPIPE, deadpeer);
	printf("Trying...\n");
	do {
		net = socket(AF_INET, SOCK_STREAM, 0);
		if (net < 0) {
			perror("telnet: socket");
			return;
		}
		if (debug &&
				setsockopt(net, SOL_SOCKET, SO_DEBUG,
					(char *)&debug, sizeof(debug)) < 0) {
			perror("setsockopt (SO_DEBUG)");
		}
		if (connect(net, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
			if (host && host->h_addr_list[1]) {
				int oerrno = errno;

				fprintf(stderr,
				    "telnet: connect to address %s: ",
				    inet_ntoa(sin.sin_addr));
				errno = oerrno;
				perror((char *)0);
				host->h_addr_list++;
				bcopy(host->h_addr_list[0],
				    (caddr_t)&sin.sin_addr, host->h_length);
				fprintf(stderr, "Trying %s...\n",
					inet_ntoa(sin.sin_addr));
				(void) close(net);
				continue;
			}
			perror("telnet: connect");
			signal(SIGINT, SIG_DFL);
			return;
		}
		connected++;
	} while (connected == 0);
	call(status, "status", "notmuch", 0);
	if (setjmp(peerdied) == 0)
		telnet();
	fprintf(stderr, "Connection closed by foreign host.\n");
	exit(1);
}


#define HELPINDENT (sizeof ("connect"))

char	openhelp[] =	"connect to a site";
char	closehelp[] =	"close current connection";
char	quithelp[] =	"exit telnet";
char	zhelp[] =	"suspend telnet";
char	escapehelp[] =	"set escape character";
char	statushelp[] =	"print status information";
char	helphelp[] =	"print help information";
char	sendhelp[] =	"transmit special characters ('send ?' for more)";
char	sethelp[] = 	"set operating parameters ('set ?' for more)";
char	togglestring[] ="toggle operating parameters ('toggle ?' for more)";
char	displayhelp[] =	"display operating parameters";
char	modehelp[] =
		"try to enter line-by-line or character-at-a-time mode";

int	help();

struct cmd cmdtab[] = {
	{ "open",	openhelp,	tn,		1, 0 },
	{ "close",	closehelp,	bye,		1, 1 },
	{ "quit",	quithelp,	quit,		1, 0 },
	{ "z",		zhelp,		suspend,	1, 0 },
	{ "escape",	escapehelp,	setescape,	1, 0 },
	{ "status",	statushelp,	status,		1, 0 },
	{ "crmod",	crmodhelp,	togcrmod,	1, 0 },
	{ "send",	sendhelp,	sendcmd,	1, 1 },
	{ "transmit",	sendhelp,	sendcmd,	0, 1 },
	{ "xmit",	sendhelp,	sendcmd,	0, 1 },
	{ "set",	sethelp,	setcmd,		1, 0 },
	{ "toggle",	togglestring,	toggle,		1, 0 },
	{ "display",	displayhelp,	display,	1, 0 },
	{ "mode",	modehelp,	modecmd,	1, 1 },
	{ "?",		helphelp,	help,		1, 0 },
	{ "help",	helphelp,	help,		0, 0 },
	0
};


/*
 * Help command.
 */
help(argc, argv)
	int argc;
	char *argv[];
{
	register struct cmd *c;

	if (argc == 1) {
		printf("Commands may be abbreviated.  Commands are:\n\n");
		for (c = cmdtab; c->name; c++)
			if (c->dohelp) {
				printf("%-*s\t%s\n", HELPINDENT, c->name,
								    c->help);
			}
		return;
	}
	while (--argc > 0) {
		register char *arg;
		arg = *++argv;
		c = getcmd(arg);
		if (c == Ambiguous(struct cmd *))
			printf("?Ambiguous help command %s\n", arg);
		else if (c == (struct cmd *)0)
			printf("?Invalid help command %s\n", arg);
		else
			printf("%s\n", c->help);
	}
}
/*
 * Call routine with argc, argv set from args (terminated by 0).
 * VARARGS2
 */
call(routine, args)
	int (*routine)();
	char *args;
{
	register char **argp;
	register int argc;

	for (argc = 0, argp = &args; *argp++ != 0; argc++)
		;
	(*routine)(argc, &args);
}

makeargv()
{
	register char *cp;
	register char **argp = margv;

	margc = 0;
	for (cp = line; *cp;) {
		while (isspace(*cp))
			cp++;
		if (*cp == '\0')
			break;
		*argp++ = cp;
		margc += 1;
		while (*cp != '\0' && !isspace(*cp))
			cp++;
		if (*cp == '\0')
			break;
		*cp++ = '\0';
	}
	*argp++ = 0;
}

char **
getnextcmd(name)
char *name;
{
    struct cmd *c = (struct cmd *) name;

    return (char **) (c+1);
}

struct cmd *
getcmd(name)
char *name;
{
    return (struct cmd *) genget(name, (char **) cmdtab, getnextcmd);
}

command(top)
	int top;
{
	register struct cmd *c;

	setcommandmode();
	if (!top)
		putchar('\n');
	else
		signal(SIGINT, SIG_DFL);
	for (;;) {
		printf("%s> ", prompt);
		if (gets(line) == 0) {
			if (feof(stdin))
				quit();
			break;
		}
		if (line[0] == 0)
			break;
		makeargv();
		c = getcmd(margv[0]);
		if (c == Ambiguous(struct cmd *)) {
			printf("?Ambiguous command\n");
			continue;
		}
		if (c == 0) {
			printf("?Invalid command\n");
			continue;
		}
		if (c->needconnect && !connected) {
			printf("?Need to be connected first.\n");
			continue;
		}
		(*c->handler)(margc, margv);
		if (c->handler != help)
			break;
	}
	if (!top) {
		if (!connected) {
			longjmp(toplevel, 1);
			/*NOTREACHED*/
		}
		setconnmode();
	}
}

/*
 * main.  Parse arguments, invoke the protocol or command parser.
 */


main(argc, argv)
	int argc;
	char *argv[];
{
	sp = getservbyname("telnet", "tcp");
	if (sp == 0) {
		fprintf(stderr, "telnet: tcp/telnet: unknown service\n");
		exit(1);
	}
	NetTrace = stdout;
	ioctl(0, TIOCGETP, (char *)&ottyb);
	ioctl(0, TIOCGETC, (char *)&otc);
	ioctl(0, TIOCGLTC, (char *)&oltc);
	ntc = otc;
	ntc.t_eofc = -1;		/* we don't want to use EOF */
	nttyb = ottyb;
	setbuf(stdin, (char *)0);
	setbuf(stdout, (char *)0);
	prompt = argv[0];
	if (argc > 1 && !strcmp(argv[1], "-d")) {
		debug = 1;
		argv++;
		argc--;
	}
	if (argc > 1 && !strcmp(argv[1], "-n")) {
	    argv++;
	    argc--;
	    if (argc > 1) {		/* get file name */
		NetTrace = fopen(argv[1], "w");
		argv++;
		argc--;
		if (NetTrace == NULL) {
		    NetTrace = stdout;
		}
	    }
	}
	if (argc != 1) {
		if (setjmp(toplevel) != 0)
			exit(0);
		tn(argc, argv);
	}
	setjmp(toplevel);
	for (;;)
		command(1);
}
