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
static char sccsid[] = "@(#)telnet.c	5.17 (Berkeley) %G%";
#endif not lint

/*
 * User telnet program.
 *
 * Many of the FUNCTIONAL changes in this newest version of telnet
 * were suggested by Dave Borman of Cray Research, Inc.
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



#ifndef	FD_SETSIZE
/*
 * The following is defined just in case someone should want to run
 * this telnet on a 4.2 system.
 *
 */

#define	FD_SET(n, p)	((p)->fds_bits[0] |= (1<<(n)))
#define	FD_CLR(n, p)	((p)->fds_bits[0] &= ~(1<<(n)))
#define	FD_ISSET(n, p)	((p)->fds_bits[0] & (1<<(n)))
#define FD_ZERO(p)	((p)->fds_bits[0] = 0)

#endif

#define	strip(x)	((x)&0x7f)

char	ttyobuf[2*BUFSIZ], *tfrontp = ttyobuf, *tbackp = ttyobuf;
#define	TTYADD(c)	{ if (!(SYNCHing||flushout)) { *tfrontp++ = c; } }
#define	TTYLOC()	(tfrontp)
#define	TTYMAX()	(ttyobuf+sizeof ttyobuf-1)
#define	TTYMIN()	(netobuf)
#define	TTYBYTES()	(tfrontp-tbackp)
#define	TTYROOM()	(TTYMAX()-TTYLOC()+1)

char	netobuf[2*BUFSIZ], *nfrontp = netobuf, *nbackp = netobuf;
#define	NETADD(c)	{ *nfrontp++ = c; }
#define	NET2ADD(c1,c2)	{ NETADD(c1); NETADD(c2); }
#define NETLOC()	(nfrontp)
#define	NETMAX()	(netobuf+sizeof netobuf-1)
#define	NETBYTES()	(nfrontp-nbackp)
#define	NETROOM()	(NETMAX()-NETLOC()+1)
char	*neturg = 0;		/* one past last byte of urgent data */

char	subbuffer[100], *subpointer, *subend;	/* buffer for sub-options */
#define	SB_CLEAR()	subpointer = subbuffer;
#define	SB_TERM()	subend = subpointer;
#define	SB_ACCUM(c)	if (subpointer < (subbuffer+sizeof subbuffer)) { \
				*subpointer++ = (c); \
			}

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
int	autoflush = 0;		/* flush output when interrupting? */
int	autosynch = 0;		/* send interrupt characters with SYNCH? */
int	localchars = 0;		/* we recognize interrupt/quit */
int	donelclchars = 0;	/* the user has set "localchars" */
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
struct	ltchars oltc, nltc;
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

	if (name == 0) {
	    return 0;
	}
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
		    c = b | 0x40;		/* DEL */
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

	if (c == 0x7f)
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
 * upcase()
 *
 *	Upcase (in place) the argument.
 */

void
upcase(argument)
register char *argument;
{
    register int c;

    while (c = *argument) {
	if (islower(c)) {
	    *argument = toupper(c);
	}
	argument++;
    }
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
 * nextitem()
 *
 *	Return the address of the next "item" in the TELNET data
 * stream.  This will be the address of the next character if
 * the current address is a user data character, or it will
 * be the address of the character following the TELNET command
 * if the current address is a TELNET IAC ("I Am a Command")
 * character.
 */

char *
nextitem(current)
char	*current;
{
    if ((*current&0xff) != IAC) {
	return current+1;
    }
    switch (*(current+1)&0xff) {
    case DO:
    case DONT:
    case WILL:
    case WONT:
	return current+3;
    case SB:		/* loop forever looking for the SE */
	{
	    register char *look = current+2;

	    for (;;) {
		if ((*look++&0xff) == IAC) {
		    if ((*look++&0xff) == SE) {
			return look;
		    }
		}
	    }
	}
    default:
	return current+2;
    }
}
/*
 * netclear()
 *
 *	We are about to do a TELNET SYNCH operation.  Clear
 * the path to the network.
 *
 *	Things are a bit tricky since we may have sent the first
 * byte or so of a previous TELNET command into the network.
 * So, we have to scan the network buffer from the beginning
 * until we are up to where we want to be.
 *
 *	A side effect of what we do, just to keep things
 * simple, is to clear the urgent data pointer.  The principal
 * caller should be setting the urgent data pointer AFTER calling
 * us in any case.
 */

netclear()
{
    register char *thisitem, *next;
    char *good;
#define	wewant(p)	((nfrontp > p) && ((*p&0xff) == IAC) && \
				((*(p+1)&0xff) != EC) && ((*(p+1)&0xff) != EL))

    thisitem = netobuf;

    while ((next = nextitem(thisitem)) <= nbackp) {
	thisitem = next;
    }

    /* Now, thisitem is first before/at boundary. */

    good = netobuf;	/* where the good bytes go */

    while (nfrontp > thisitem) {
	if (wewant(thisitem)) {
	    int length;

	    next = thisitem;
	    do {
		next = nextitem(next);
	    } while (wewant(next) && (nfrontp > next));
	    length = next-thisitem;
	    bcopy(thisitem, good, length);
	    good += length;
	    thisitem = next;
	} else {
	    thisitem = nextitem(thisitem);
	}
    }

    nbackp = netobuf;
    nfrontp = good;		/* next byte to be sent */
    neturg = 0;
}

/*
 * Send as much data as possible to the terminal.
 */


ttyflush()
{
    int n;

    if ((n = tfrontp - tbackp) > 0) {
	if (!(SYNCHing||flushout)) {
	    n = write(tout, tbackp, n);
	} else {
	    ioctl(fileno(stdout), TIOCFLUSH, (char *) 0);
	    /* we leave 'n' alone! */
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
    if (localchars) {
	intp();
	return;
    }
    setcommandmode();
    longjmp(toplevel, -1);
}

intr2()
{
    if (localchars) {
	sendbrk();
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
	printf("%s ", direction+1);
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
	if (option < (sizeof telopts/sizeof telopts[0]))
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


mode(f)
	register int f;
{
	static int prevmode = 0;
	struct tchars *tc;
	struct ltchars *ltc;
	struct sgttyb sb;
	int onoff, old;
	struct	tchars notc2;
	struct	ltchars noltc2;
	static struct	tchars notc =	{ -1, -1, -1, -1, -1, -1 };
	static struct	ltchars noltc =	{ -1, -1, -1, -1, -1, -1 };

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
		if (!donelclchars) {
			localchars = 0;
		}
		if (localchars) {
			notc2 = notc;
			notc2.t_intrc = ntc.t_intrc;
			notc2.t_quitc = ntc.t_quitc;
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
		notc2 = ntc;
		tc = &notc2;
		noltc2 = oltc;
		ltc = &noltc2;
		/*
		 * If user hasn't specified one way or the other,
		 * then default to trapping signals.
		 */
		if (!donelclchars) {
			localchars = 1;
		}
		if (localchars) {
			notc2.t_brkc = nltc.t_flushc;
			noltc2.t_flushc = -1;
		} else {
			notc2.t_intrc = notc2.t_quitc = -1;
		}
		noltc2.t_suspc = escape;
		noltc2.t_dsuspc = -1;
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
	fd_set ibits, obits, xbits;

	tout = fileno(stdout);
	setconnmode();
	scc = 0;
	tcc = 0;
	FD_ZERO(&ibits);
	FD_ZERO(&obits);
	FD_ZERO(&xbits);

	ioctl(net, FIONBIO, (char *)&on);
#if	defined(SO_OOBINLINE)
	setsockopt(net, SOL_SOCKET, SO_OOBINLINE, &on, sizeof on);
#endif	/* defined(SO_OOBINLINE) */
	if (telnetport) {
	    if (!hisopts[TELOPT_SGA]) {
		willoption(TELOPT_SGA, 0);
	    }
	    if (!myopts[TELOPT_TTYPE]) {
		dooption(TELOPT_TTYPE, 0);
	    }
	}
	for (;;) {
		if (scc < 0 && tcc < 0) {
			break;
		}

		if (((globalmode < 4) || flushline) && NETBYTES()) {
			FD_SET(net, &obits);
		} else {
			FD_SET(tin, &ibits);
		}
		if (TTYBYTES()) {
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
		    FD_CLR(net, &xbits);
		    SYNCHing = 1;
		    ttyflush();	/* flush already enqueued data */
		}

		/*
		 * Something to read from the network...
		 */
		if (FD_ISSET(net, &ibits)) {
			int canread;

			FD_CLR(net, &ibits);
			if (scc == 0) {
			    sbp = sibuf;
			}
			canread = sibuf + sizeof sibuf - sbp;
#if	!defined(SO_OOBINLINE)
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
			    c = recv(net, sibuf, canread, MSG_OOB);
			    if ((c == -1) && (errno == EINVAL)) {
				c = read(net, sibuf, canread);
				if (clocks.didnetreceive < clocks.gotDM) {
				    SYNCHing = stilloob(net);
				}
			    }
			} else {
			    c = read(net, sibuf, canread);
			}
		    } else {
			c = read(net, sibuf, canread);
		    }
		    settimer(didnetreceive);
#else	/* !defined(SO_OOBINLINE) */
		    c = read(net, sbp, canread);
#endif	/* !defined(SO_OOBINLINE) */
		    if (c < 0 && errno == EWOULDBLOCK) {
			c = 0;
		    } else if (c <= 0) {
			break;
		    }
		    if (netdata) {
			Dump('<', sbp, c);
		    }
		    scc += c;
		}

		/*
		 * Something to read from the tty...
		 */
		if (FD_ISSET(tin, &ibits)) {
			FD_CLR(tin, &ibits);
			if (tcc == 0) {
			    tbp = tibuf;	/* nothing left, reset */
			}
			c = read(tin, tbp, tibuf+sizeof tibuf - tbp);
			if (c < 0 && errno == EWOULDBLOCK) {
				c = 0;
			} else {
				/* EOF detection for line mode!!!! */
				if (c == 0 && globalmode >= 3) {
					/* must be an EOF... */
					*tbp = ntc.t_eofc;
					c = 1;
				}
				if (c <= 0) {
					tcc = c;
					break;
				}
			}
			tcc += c;
		}

		while (tcc > 0) {
			register int sc;

			if (NETROOM() < 2) {
				flushline = 1;
				break;
			}
			c = *tbp++ & 0xff, sc = strip(c), tcc--;
			if (sc == escape) {
				command(0);
				tcc = 0;
				flushline = 1;
				break;
			} else if ((globalmode >= 4) && (sc == echoc)) {
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
			if (localchars) {
				if (sc == ntc.t_intrc) {
					intp();
					break;
				} else if (sc == ntc.t_quitc) {
					sendbrk();
					break;
				} else if (sc == nltc.t_flushc) {
					NET2ADD(IAC, AO);
					if (autoflush) {
					    doflush();
					}
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
				 * If we are in CRMOD mode (\r ==> \n)
				 * on our local machine, then probably
				 * a newline (unix) is CRLF (TELNET).
				 */
				if (globalmode >= 3) {
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
		    FD_ISSET(net, &obits) && (NETBYTES() > 0)) {
			FD_CLR(net, &obits);
			netflush(net);
		}
		if (scc > 0)
			telrcv();
		if (FD_ISSET(tout, &obits) && (TTYBYTES() > 0)) {
			FD_CLR(tout, &obits);
			ttyflush();
		}
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
#define	TS_SB		7		/* sub-option collection */
#define	TS_SE		8		/* looking for sub-option end */

telrcv()
{
	register int c;
	static int state = TS_DATA;

	while ((scc > 0) && (TTYROOM() > 2)) {
		c = *sbp++ & 0xff, scc--;
		switch (state) {

		case TS_CR:
			state = TS_DATA;
			if (c == '\0') {
			    break;	/* Ignore \0 after CR */
			} else if (c == '\n') {
			    if (hisopts[TELOPT_ECHO] && !crmod) {
				TTYADD(c);
			    }
			    break;
			}
			/* Else, fall through */

		case TS_DATA:
			if (c == IAC) {
				state = TS_IAC;
				continue;
			}
			    /*
			     * The 'crmod' hack (see following) is needed
			     * since we can't * set CRMOD on output only.
			     * Machines like MULTICS like to send \r without
			     * \n; since we must turn off CRMOD to get proper
			     * input, the mapping is done here (sigh).
			     */
			if (c == '\r') {
				if (scc > 0) {
					c = *sbp&0xff;
					if (c == 0) {
						sbp++, scc--;
						/* a "true" CR */
						TTYADD('\r');
					} else if (!hisopts[TELOPT_ECHO] &&
								(c == '\n')) {
						sbp++, scc--;
						TTYADD('\n');
					} else {
						TTYADD('\r');
						if (crmod) {
							TTYADD('\n');
						}
					}
				} else {
					state = TS_CR;
					TTYADD('\r');
					if (crmod) {
						TTYADD('\n');
					}
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

			case SB:
				SB_CLEAR();
				state = TS_SB;
				continue;

			default:
				break;
			}
			state = TS_DATA;
			continue;

		case TS_WILL:
			printoption(">RCVD", will, c, !hisopts[c]);
			if (c == TELOPT_TM) {
				if (flushout) {
					flushout = 0;
				}
			} else if (!hisopts[c]) {
				willoption(c, 1);
			}
			state = TS_DATA;
			continue;

		case TS_WONT:
			printoption(">RCVD", wont, c, hisopts[c]);
			if (c == TELOPT_TM) {
				if (flushout) {
					flushout = 0;
				}
			} else if (hisopts[c]) {
				wontoption(c, 1);
			}
			state = TS_DATA;
			continue;

		case TS_DO:
			printoption(">RCVD", doopt, c, !myopts[c]);
			if (!myopts[c])
				dooption(c);
			state = TS_DATA;
			continue;

		case TS_DONT:
			printoption(">RCVD", dont, c, myopts[c]);
			if (myopts[c]) {
				myopts[c] = 0;
				sprintf(nfrontp, wont, c);
				nfrontp += sizeof (wont) - 2;
				flushline = 1;
				setconnmode();	/* set new tty mode (maybe) */
				printoption(">SENT", wont, c);
			}
			state = TS_DATA;
			continue;
		case TS_SB:
			if (c == IAC) {
				state = TS_SE;
			} else {
				SB_ACCUM(c);
			}
			continue;

		case TS_SE:
			if (c != SE) {
				if (c != IAC) {
					SB_ACCUM(IAC);
				}
				SB_ACCUM(c);
				state = TS_SB;
			} else {
				SB_TERM();
				suboption();	/* handle sub-option */
				state = TS_DATA;
			}
		}
	}
}

willoption(option, reply)
	int option, reply;
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
	if (reply)
		printoption(">SENT", fmt, option);
	else
		printoption("<SENT", fmt, option);
}

wontoption(option, reply)
	int option, reply;
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
	if (reply)
		printoption(">SENT", fmt, option);
	else
		printoption("<SENT", fmt, option);
}

dooption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_TM:
		fmt = will;
		break;

	case TELOPT_TTYPE:		/* terminal type option */
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
	printoption(">SENT", fmt, option);
}

/*
 * suboption()
 *
 *	Look at the sub-option buffer, and try to be helpful to the other
 * side.
 *
 *	Currently we recognize:
 *
 *		Terminal type, send request.
 */

suboption()
{
    switch (subbuffer[0]&0xff) {
    case TELOPT_TTYPE:
	if ((subbuffer[1]&0xff) != TELQUAL_SEND) {
	    ;
	} else {
	    char *name;
	    char namebuf[41];
	    char *getenv();
	    int len;

	    name = getenv("TERM");
	    if ((name == 0) || ((len = strlen(name)) > 40)) {
		name = "UNKNOWN";
	    }
	    if ((len + 4+2) < NETROOM()) {
		strcpy(namebuf, name);
		upcase(namebuf);
		sprintf(nfrontp, "%c%c%c%c%s%c%c", IAC, SB, TELOPT_TTYPE,
				    TELQUAL_IS, namebuf, IAC, SE);
		nfrontp += 4+strlen(namebuf)+2;
	    }
	}

    default:
	break;
    }
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
    netclear();			/* clear the path to the network */
    NET2ADD(IAC, DM);
    neturg = NETLOC()-1;	/* Some systems are off by one XXX */
}

doflush()
{
    NET2ADD(IAC, DO);
    NETADD(TELOPT_TM);
    flushline = 1;
    flushout = 1;
    ttyflush();
    /* do printoption AFTER flush, otherwise the output gets tossed... */
    printoption("<SENT", doopt, TELOPT_TM);
}

intp()
{
    NET2ADD(IAC, IP);
    if (autoflush) {
	doflush();
    }
    if (autosynch) {
	dosynch();
    }
}

sendbrk()
{
    NET2ADD(IAC, BREAK);
    if (autoflush) {
	doflush();
    }
    if (autosynch) {
	dosynch();
    }
}


#define	SENDQUESTION	-1
#define	SENDESCAPE	-3

struct sendlist Sendlist[] = {
    { "ao", AO, "Send Telnet Abort output" },
    { "ayt", AYT, "Send Telnet 'Are You There'" },
    { "brk", BREAK, "Send Telnet Break" },
    { "ec", EC, "Send Telnet Erase Character" },
    { "el", EL, "Send Telnet Erase Line" },
    { "escape", SENDESCAPE, "Send current escape character" },
    { "ga", GA, "Send Telnet 'Go Ahead' sequence" },
    { "ip", IP, "Send Telnet Interrupt Process" },
    { "nop", NOP, "Send Telnet 'No operation'" },
    { "synch", SYNCH, "Perform Telnet 'Synch operation'", dosynch },
    { "?", SENDQUESTION, "Display send options" },
    { 0 }
};

struct sendlist Sendlist2[] = {		/* some synonyms */
	{ "break", BREAK, 0 },

	{ "intp", IP, 0 },
	{ "interrupt", IP, 0 },
	{ "intr", IP, 0 },

	{ "help", SENDQUESTION, 0 },

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
    struct sendlist *sl;

    if (sl = (struct sendlist *)
				genget(name, (char **) Sendlist, getnextsend)) {
	return sl;
    } else {
	return (struct sendlist *)
				genget(name, (char **) Sendlist2, getnextsend);
    }
}

sendcmd(argc, argv)
int	argc;
char	**argv;
{
    int what;		/* what we are sending this time */
    int count;		/* how many bytes we are going to need to send */
    int hadsynch;	/* are we going to process a "synch"? */
    int i;
    int question = 0;	/* was at least one argument a question */
    struct sendlist *s;	/* pointer to current command */

    if (argc < 2) {
	printf("need at least one argument for 'send' command\n");
	printf("'send ?' for help\n");
	return 0;
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
	    return 0;
	} else if (s == Ambiguous(struct sendlist *)) {
	    printf("Ambiguous send argument '%s'\n'send ?' for help.\n",
			argv[i]);
	    return 0;
	}
	switch (s->what) {
	case SENDQUESTION:
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
    if (NETROOM() < count) {
	printf("There is not enough room in the buffer TO the network\n");
	printf("to process your request.  Nothing will be done.\n");
	printf("('send synch' will throw away most data in the network\n");
	printf("buffer, if this might help.)\n");
	return 0;
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
	    case SYNCH:
		dosynch();
		break;
	    case SENDQUESTION:
		for (s = Sendlist; s->name; s++) {
		    if (s->help) {
			printf(s->name);
			if (s->help) {
			    printf("\t%s", s->help);
			}
			printf("\n");
		    }
		}
		question = 1;
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
    return !question;
}

/*
 * The following are the routines and data structures referred
 * to by the arguments to the "toggle" command.
 */

lclchars()
{
    donelclchars = 1;
    return 1;
}

togdebug()
{
#ifndef	NOT43
    if (net > 0 &&
	setsockopt(net, SOL_SOCKET, SO_DEBUG, (char *)&debug, sizeof(debug))
									< 0) {
	    perror("setsockopt (SO_DEBUG)");
    }
#else	NOT43
    if (debug) {
	if (net > 0 && setsockopt(net, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
	    perror("setsockopt (SO_DEBUG)");
    } else
	printf("Cannot turn off socket debugging\n");
#endif	NOT43
    return 1;
}



int togglehelp();

struct togglelist {
    char	*name;		/* name of toggle */
    char	*help;		/* help message */
    int		(*handler)();	/* routine to do actual setting */
    int		dohelp;		/* should we display help information */
    int		*variable;
    char	*actionexplanation;
};

struct togglelist Togglelist[] = {
    { "autoflush",
	"toggle flushing of output when sending interrupt characters",
	    0,
		1,
		    &autoflush,
			"flush output when sending interrupt characters" },
    { "autosynch",
	"toggle automatic sending of interrupt characters in urgent mode",
	    0,
		1,
		    &autosynch,
			"send interrupt characters in urgent mode" },
    { "crmod",
	"toggle mapping of received carriage returns",
	    0,
		1,
		    &crmod,
			"map carriage return on output" },
    { "localchars",
	"toggle local recognition of certain control characters",
	    lclchars,
		1,
		    &localchars,
			"recognize certain control characters" },
    { " ", "", 0, 1 },		/* empty line */
    { "debug",
	"(debugging) toggle debugging",
	    togdebug,
		1,
		    &debug,
			"turn on socket level debugging" },
    { "netdata",
	"(debugging) toggle printing of hexadecimal network data",
	    0,
		1,
		    &netdata,
			"print hexadecimal representation of network traffic" },
    { "options",
	"(debugging) toggle viewing of options processing",
	    0,
		1,
		    &showoptions,
			"show option processing" },
    { " ", "", 0, 1 },		/* empty line */
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
    return 0;
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
    int retval = 1;
    char *name;
    struct togglelist *c;

    if (argc < 2) {
	fprintf(stderr,
	    "Need an argument to 'toggle' command.  'toggle ?' for help.\n");
	return 0;
    }
    argc--;
    argv++;
    while (argc--) {
	name = *argv++;
	c = gettoggle(name);
	if (c == Ambiguous(struct togglelist *)) {
	    fprintf(stderr, "'%s': ambiguous argument ('toggle ?' for help).\n",
					name);
	    return 0;
	} else if (c == 0) {
	    fprintf(stderr, "'%s': unknown argument ('toggle ?' for help).\n",
					name);
	    return 0;
	} else {
	    if (c->variable) {
		*c->variable = !*c->variable;		/* invert it */
		printf("%s %s.\n", *c->variable? "Will" : "Won't",
							c->actionexplanation);
	    }
	    if (c->handler) {
		retval &= (*c->handler)(c);
	    }
	}
    }
    return retval;
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
    { " ", "" },
    { " ", "The following need 'localchars' to be toggled true", 0 },
    { "erase",	"character to cause an Erase Character", &nttyb.sg_erase },
    { "flushoutput", "character to cause an Abort Oubput", &nltc.t_flushc },
    { "interrupt", "character to cause an Interrupt Process", &ntc.t_intrc },
    { "kill",	"character to cause an Erase Line", &nttyb.sg_kill },
    { "quit",	"character to cause a Break", &ntc.t_quitc },
    { "eof",	"character to cause an EOF ", &ntc.t_eofc },
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
	if ((argc == 2) &&
		    ((!strcmp(argv[1], "?")) || (!strcmp(argv[1], "help")))) {
	    for (ct = Setlist; ct->name; ct++) {
		printf("%s\t%s\n", ct->name, ct->help);
	    }
	    printf("?\tdisplay help information\n");
	} else {
	    printf("Format is 'set Name Value'\n'set ?' for help.\n");
	}
	return 0;
    }

    ct = getset(argv[1]);
    if (ct == 0) {
	fprintf(stderr, "'%s': unknown argument ('set ?' for help).\n",
			argv[1]);
	return 0;
    } else if (ct == Ambiguous(struct setlist *)) {
	fprintf(stderr, "'%s': ambiguous argument ('set ?' for help).\n",
			argv[1]);
	return 0;
    } else {
	if (strcmp("off", argv[2])) {
	    value = special(argv[2]);
	} else {
	    value = -1;
	}
	*(ct->charp) = value;
	printf("%s character is '%s'.\n", ct->name, control(*(ct->charp)));
    }
    return 1;
}

/*
 * The following are the data structures and routines for the
 * 'mode' command.
 */

dolinemode()
{
    if (hisopts[TELOPT_SGA]) {
	wontoption(TELOPT_SGA, 0);
    }
    if (hisopts[TELOPT_ECHO]) {
	wontoption(TELOPT_ECHO, 0);
    }
}

docharmode()
{
    if (!hisopts[TELOPT_SGA]) {
	willoption(TELOPT_SGA, 0);
    }
    if (!hisopts[TELOPT_ECHO]) {
	willoption(TELOPT_ECHO, 0);
    }
}

struct cmd Modelist[] = {
    { "character",	"character-at-a-time mode",	docharmode, 1, 1 },
    { "line",		"line-by-line mode",		dolinemode, 1, 1 },
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
	return 0;
    }
    mt = getmodecmd(argv[1]);
    if (mt == 0) {
	fprintf(stderr, "Unknown mode '%s' ('mode ?' for help).\n", argv[1]);
	return 0;
    } else if (mt == Ambiguous(struct cmd *)) {
	fprintf(stderr, "Ambiguous mode '%s' ('mode ?' for help).\n", argv[1]);
	return 0;
    } else {
	(*mt->handler)();
    }
    return 1;
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

#define	doset(sl)   if (sl->name && *sl->name != ' ') { \
			printf("[%s]\t%s.\n", control(*sl->charp), sl->name); \
		    }

    struct togglelist *tl;
    struct setlist *sl;

    if (argc == 1) {
	for (tl = Togglelist; tl->name; tl++) {
	    dotog(tl);
	}
	printf("\n");
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
		return 0;
	    } else if (!sl && !tl) {
		printf("?Unknown argument '%s'.\n", argv[i]);
		return 0;
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
    return 1;
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
	return 1;
}

/*VARARGS*/
togcrmod()
{
    crmod = !crmod;
    printf("Deprecated usage - please use 'toggle crmod' in the future.\n");
    printf("%s map carriage return on output.\n", crmod ? "Will" : "Won't");
    fflush(stdout);
    return 1;
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
	return 1;
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
	return 1;
}

/*VARARGS*/
quit()
{
	(void) call(bye, "bye", 0);
	exit(0);
	/*NOTREACHED*/
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
	    if (localchars) {
		printf("Catching signals locally.\n");
	    }
	}
    } else {
	printf("No connection.\n");
    }
    printf("Escape character is '%s'.\n", control(escape));
    fflush(stdout);
    return 1;
}

tn(argc, argv)
	int argc;
	char *argv[];
{
	register struct hostent *host = 0;

	if (connected) {
		printf("?Already connected to %s\n", hostname);
		return 0;
	}
	if (argc < 2) {
		(void) strcpy(line, "Connect ");
		printf("(to) ");
		gets(&line[strlen(line)]);
		makeargv();
		argc = margc;
		argv = margv;
	}
	if ((argc < 2) || (argc > 3)) {
		printf("usage: %s host-name [port]\n", argv[0]);
		return 0;
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
#ifndef	NOT43
			bcopy(host->h_addr_list[0], (caddr_t)&sin.sin_addr,
				host->h_length);
#else	NOT43
			bcopy(host->h_addr, (caddr_t)&sin.sin_addr,
				host->h_length);
#endif	NOT43
			hostname = host->h_name;
		} else {
			printf("%s: unknown host\n", argv[1]);
			return 0;
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
				return 0;
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
			return 0;
		}
#ifndef	NOT43
		if (debug &&
				setsockopt(net, SOL_SOCKET, SO_DEBUG,
					(char *)&debug, sizeof(debug)) < 0)
#else	NOT43
		if (debug && setsockopt(net, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
#endif	NOT43
			perror("setsockopt (SO_DEBUG)");

		if (connect(net, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
#ifndef	NOT43
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
#endif	NOT43
			perror("telnet: connect");
			signal(SIGINT, SIG_DFL);
			signal(SIGQUIT, SIG_DFL);
			return 0;
		}
		connected++;
	} while (connected == 0);
	call(status, "status", "notmuch", 0);
	if (setjmp(peerdied) == 0)
		telnet();
	fprintf(stderr, "Connection closed by foreign host.\n");
	exit(1);
	/*NOTREACHED*/
}


#define HELPINDENT (sizeof ("connect"))

char	openhelp[] =	"connect to a site";
char	closehelp[] =	"close current connection";
char	quithelp[] =	"exit telnet";
char	zhelp[] =	"suspend telnet";
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
	{ "close",	closehelp,	bye,		1, 1 },
	{ "display",	displayhelp,	display,	1, 0 },
	{ "mode",	modehelp,	modecmd,	1, 1 },
	{ "open",	openhelp,	tn,		1, 0 },
	{ "quit",	quithelp,	quit,		1, 0 },
	{ "send",	sendhelp,	sendcmd,	1, 1 },
	{ "set",	sethelp,	setcmd,		1, 0 },
	{ "status",	statushelp,	status,		1, 0 },
	{ "toggle",	togglestring,	toggle,		1, 0 },
	{ "z",		zhelp,		suspend,	1, 0 },
	{ "?",		helphelp,	help,		1, 0 },
	0
};

char	crmodhelp[] =	"deprecated command -- use 'toggle crmod' instead";
char	escapehelp[] =	"deprecated command -- use 'set escape' instead";

struct cmd cmdtab2[] = {
	{ "help",	helphelp,	help,		0, 0 },
	{ "escape",	escapehelp,	setescape,	1, 0 },
	{ "crmod",	crmodhelp,	togcrmod,	1, 0 },
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
		return 0;
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
	return 0;
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
	return (*routine)(argc, &args);
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
    struct cmd *cm;

    if (cm = (struct cmd *) genget(name, (char **) cmdtab, getnextcmd)) {
	return cm;
    } else {
	return (struct cmd *) genget(name, (char **) cmdtab2, getnextcmd);
    }
}

command(top)
	int top;
{
	register struct cmd *c;

	setcommandmode();
	if (!top) {
		putchar('\n');
	} else {
		signal(SIGINT, SIG_DFL);
		signal(SIGQUIT, SIG_DFL);
	}
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
		if ((*c->handler)(margc, margv)) {
			break;
		}
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
#if	defined(LNOFLSH)
	ioctl(0, TIOCLGET, (char *)&autoflush);
	autoflush = !(autoflush&LNOFLSH);	/* if LNOFLSH, no autoflush */
#else	/* LNOFLSH */
	autoflush = 1;
#endif	/* LNOFLSH */
	ntc = otc;
	nltc = oltc;
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
