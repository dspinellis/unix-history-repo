#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1984-1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif	/* not lint */

#ifndef lint
static char sccsid[] = "@(#)telnet.c	1.2 (Berkeley) 9/25/87";
#endif	/* not lint */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>

#include <netinet/in.h>

#if	defined(unix)
/* By the way, we need to include curses.h before telnet.h since,
 * among other things, telnet.h #defines 'DO', which is a variable
 * declared in curses.h.
 */
#include <curses.h>
#endif	/* defined(unix) */

#include <arpa/telnet.h>

#if	!defined(NOT43)
#include <arpa/inet.h>
#else	/* !defined(NOT43) */
extern unsigned long inet_addr();
extern char	*inet_ntoa();
#endif	/* !defined(NOT43) */

#include <ctype.h>
#include <errno.h>
#include <netdb.h>

#if	defined(unix)
#include <strings.h>
#else	/* defined(unix) */
#include <string.h>
#endif	/* defined(unix) */

#include "ring.h"

#include "defines.h"
#include "externs.h"
#include "types.h"
#include "general.h"


void	setcommandmode(), command();	/* forward declarations */

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
#define min(x,y)	((x<y)? x:y)

#if	defined(TN3270)
static char	Ibuf[8*BUFSIZ], *Ifrontp, *Ibackp;
#endif	/* defined(TN3270) */


static char	subbuffer[SUBBUFSIZE],
		*subpointer, *subend;	 /* buffer for sub-options */
#define	SB_CLEAR()	subpointer = subbuffer;
#define	SB_TERM()	subend = subpointer;
#define	SB_ACCUM(c)	if (subpointer < (subbuffer+sizeof subbuffer)) { \
				*subpointer++ = (c); \
			}

static char	sb_terminal[] = { IAC, SB,
			TELOPT_TTYPE, TELQUAL_IS,
			'I', 'B', 'M', '-', '3', '2', '7', '8', '-', '2',
			IAC, SE };
#define	SBTERMMODEL	13


char	hisopts[256];
char	myopts[256];

char	doopt[] = { IAC, DO, '%', 'c', 0 };
char	dont[] = { IAC, DONT, '%', 'c', 0 };
char	will[] = { IAC, WILL, '%', 'c', 0 };
char	wont[] = { IAC, WONT, '%', 'c', 0 };

static Ring	netiring, ttyiring;
static char	netibuf[BUFSIZ];
static char	ttyibuf[BUFSIZ];
static fd_set ibits, obits, xbits;


int
	connected,
	net,
	showoptions,
	In3270,		/* Are we in 3270 mode? */
	ISend,		/* trying to send network data in */
	debug = 0,
	crmod,
	netdata,	/* Print out network data flow */
	crlf,		/* Should '\r' be mapped to <CR><LF> (or <CR><NUL>)? */
	noasynch = 0,	/* User specified "-noasynch" on command line */
	askedSGA = 0,	/* We have talked about suppress go ahead */
	telnetport = 1;

#define	CONTROL(x)	((x)&0x1f)		/* CTRL(x) is not portable */

char
	*prompt = 0,
	escape,
	echoc;

int
	SYNCHing,		/* we are in TELNET SYNCH mode */
	flushout,		/* flush output */
	autoflush = 0,		/* flush output when interrupting? */
	autosynch,		/* send interrupt characters with SYNCH? */
	localchars,		/* we recognize interrupt/quit */
	donelclchars,		/* the user has set "localchars" */
	donebinarytoggle,	/* the user has put us in binary */
	dontlecho,		/* do we suppress local echoing right now? */
	globalmode;

/*	The following are some tn3270 specific flags */
#if	defined(TN3270)

static int
	Sent3270TerminalType;	/* Have we said we are a 3270? */

#endif	/* defined(TN3270) */
int

	tout,			/* Output file descriptor */
	tin;			/* Input file descriptor */



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

static int	telrcv_state;

jmp_buf	toplevel = { 0 };
jmp_buf	peerdied;

int	flushline;

/*
 * The following are some clocks used to decide how to interpret
 * the relationship between various variables.
 */

Clocks clocks;

Modelist modelist[] = {
	{ "telnet command mode", COMMAND_LINE },
	{ "character-at-a-time mode", 0 },
	{ "character-at-a-time mode (local echo)", LOCAL_ECHO|LOCAL_CHARS },
	{ "line-by-line mode (remote echo)", LINE | LOCAL_CHARS },
	{ "line-by-line mode", LINE | LOCAL_ECHO | LOCAL_CHARS },
	{ "line-by-line mode (local echoing suppressed)", LINE | LOCAL_CHARS },
	{ "3270 mode", 0 },
};


/*
 * Initialize telnet environment.
 */

init_telnet()
{
    /* Don't change telnetport */
    SB_CLEAR();
    ClearArray(hisopts);
    ClearArray(myopts);
    ring_init(&netiring, netibuf, sizeof netibuf);
    ring_init(&ttyiring, ttyibuf, sizeof ttyibuf);

    connected = net = In3270 = ISend = donebinarytoggle = 0;
    telnetport = 0;

#if	defined(unix) && defined(TN3270)
    HaveInput = 0;
#endif	/* defined(unix) && defined(TN3270) */

    SYNCHing = 0;

    errno = 0;

    /* Don't change NetTrace */

    escape = CONTROL(']');
    echoc = CONTROL('E');

    flushline = 1;
    telrcv_state = TS_DATA;
}


void
willoption(option, reply)
	int option, reply;
{
	char *fmt;

	switch (option) {

	case TELOPT_ECHO:
#	if defined(TN3270)
	    /*
	     * The following is a pain in the rear-end.
	     * Various IBM servers (some versions of Wiscnet,
	     * possibly Fibronics/Spartacus, and who knows who
	     * else) will NOT allow us to send "DO SGA" too early
	     * in the setup proceedings.  On the other hand,
	     * 4.2 servers (telnetd) won't set SGA correctly.
	     * So, we are stuck.  Empirically (but, based on
	     * a VERY small sample), the IBM servers don't send
	     * out anything about ECHO, so we postpone our sending
	     * "DO SGA" until we see "WILL ECHO" (which 4.2 servers
	     * DO send).
	     */
	    {
		if (askedSGA == 0) {
		    askedSGA = 1;
		    if (!hisopts[TELOPT_SGA]) {
			willoption(TELOPT_SGA, 0);
		    }
		}
	    }
		/* Fall through */
	case TELOPT_EOR:
	case TELOPT_BINARY:
#endif	/* defined(TN3270) */
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
	netoprint(fmt, option);
	if (reply)
		printoption(">SENT", fmt, option, reply);
	else
		printoption("<SENT", fmt, option, reply);
}

void
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
	netoprint(fmt, option);
	if (reply)
		printoption(">SENT", fmt, option, reply);
	else
		printoption("<SENT", fmt, option, reply);
}

static void
dooption(option)
	int option;
{
	char *fmt;

	switch (option) {

	case TELOPT_TM:
		fmt = will;
		break;

#	if defined(TN3270)
	case TELOPT_EOR:
	case TELOPT_BINARY:
#	endif	/* defined(TN3270) */
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
	netoprint(fmt, option);
	printoption(">SENT", fmt, option, 0);
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

static void
suboption()
{
    printsub("<", subbuffer, subend-subbuffer+1);
    switch (subbuffer[0]&0xff) {
    case TELOPT_TTYPE:
	if ((subbuffer[1]&0xff) != TELQUAL_SEND) {
	    ;
	} else {
	    char *name;
	    char namebuf[41];
	    extern char *getenv();
	    int len;

#if	defined(TN3270)
	    /*
	     * Try to send a 3270 type terminal name.  Decide which one based
	     * on the format of our screen, and (in the future) color
	     * capaiblities.
	     */
#if	defined(unix)
	    if (initscr() != ERR) {	/* Initialize curses to get line size */
		MaxNumberLines = LINES;
		MaxNumberColumns = COLS;
	    }
#else	/* defined(unix) */
	    InitTerminal();
#endif	/* defined(unix) */
	    if ((MaxNumberLines >= 24) && (MaxNumberColumns >= 80)) {
		Sent3270TerminalType = 1;
		if ((MaxNumberLines >= 27) && (MaxNumberColumns >= 132)) {
		    MaxNumberLines = 27;
		    MaxNumberColumns = 132;
		    sb_terminal[SBTERMMODEL] = '5';
		} else if (MaxNumberLines >= 43) {
		    MaxNumberLines = 43;
		    MaxNumberColumns = 80;
		    sb_terminal[SBTERMMODEL] = '4';
		} else if (MaxNumberLines >= 32) {
		    MaxNumberLines = 32;
		    MaxNumberColumns = 80;
		    sb_terminal[SBTERMMODEL] = '3';
		} else {
		    MaxNumberLines = 24;
		    MaxNumberColumns = 80;
		    sb_terminal[SBTERMMODEL] = '2';
		}
		NumberLines = 24;		/* before we start out... */
		NumberColumns = 80;
		ScreenSize = NumberLines*NumberColumns;
		if ((MaxNumberLines*MaxNumberColumns) > MAXSCREENSIZE) {
		    ExitString("Programming error:  MAXSCREENSIZE too small.\n",
									1);
		    /*NOTREACHED*/
		}
		printsub(">", sb_terminal+2, sizeof sb_terminal-2);
		ring_add_data(&netoring, sb_terminal, sizeof sb_terminal);
		return;
	    }
#endif	/* defined(TN3270) */

	    name = getenv("TERM");
	    if ((name == 0) || ((len = strlen(name)) > 40)) {
		name = "UNKNOWN";
	    }
	    if ((len + 4+2) < NETROOM()) {
		strcpy(namebuf, name);
		upcase(namebuf);
		netoprint("%c%c%c%c%s%c%c", IAC, SB, TELOPT_TTYPE,
				    TELQUAL_IS, namebuf, IAC, SE);
		/* XXX */
		/* printsub(">", nfrontp+2, 4+strlen(namebuf)+2-2-2); */
	    } else {
		ExitString("No room in buffer for terminal type.\n",
							1);
		/*NOTREACHED*/
	    }
	}

    default:
	break;
    }
}

#if	defined(TN3270)
static void
SetIn3270()
{
    if (Sent3270TerminalType && myopts[TELOPT_BINARY]
			    && hisopts[TELOPT_BINARY] && !donebinarytoggle) {
	if (!In3270) {
	    In3270 = 1;
	    Init3270();		/* Initialize 3270 functions */
	    /* initialize terminal key mapping */
	    InitTerminal();	/* Start terminal going */
	    setconnmode();
	}
    } else {
	if (In3270) {
	    StopScreen(1);
	    In3270 = 0;
	    Stop3270();		/* Tell 3270 we aren't here anymore */
	    setconnmode();
	}
    }
}
#endif	/* defined(TN3270) */


static int
telrcv()
{
    register int c;
    register int scc;
    register char *sbp;
    int count;
    int returnValue = 0;

    scc = 0;
    count = 0;
    while (TTYROOM() > 2) {
	if (scc == 0) {
	    if (count) {
		ring_sent_acked(&netiring, count);
		returnValue = 1;
		count = 0;
	    }
	    sbp = netiring.send;
	    scc = ring_unsent_consecutive(&netiring);
	    if (scc == 0) {
		/* No more data coming in */
		break;
	    }
	}

	c = *sbp++ & 0xff, scc--; count++;

	switch (telrcv_state) {

	case TS_CR:
	    telrcv_state = TS_DATA;
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
		telrcv_state = TS_IAC;
		continue;
	    }
#	    if defined(TN3270)
	    if (In3270) {
		*Ifrontp++ = c;
		while (scc > 0) {
		    c = *sbp++ & 0377, scc--; count++;
		    if (c == IAC) {
			telrcv_state = TS_IAC;
			break;
		    }
		    *Ifrontp++ = c;
		}
	    } else
#	    endif /* defined(TN3270) */
		    /*
		     * The 'crmod' hack (see following) is needed
		     * since we can't * set CRMOD on output only.
		     * Machines like MULTICS like to send \r without
		     * \n; since we must turn off CRMOD to get proper
		     * input, the mapping is done here (sigh).
		     */
	    if ((c == '\r') && !hisopts[TELOPT_BINARY]) {
		if (scc > 0) {
		    c = *sbp&0xff;
		    if (c == 0) {
			sbp++, scc--; count++;
			/* a "true" CR */
			TTYADD('\r');
		    } else if (!hisopts[TELOPT_ECHO] &&
					(c == '\n')) {
			sbp++, scc--; count++;
			TTYADD('\n');
		    } else {
			TTYADD('\r');
			if (crmod) {
				TTYADD('\n');
			}
		    }
		} else {
		    telrcv_state = TS_CR;
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
		telrcv_state = TS_WILL;
		continue;

	    case WONT:
		telrcv_state = TS_WONT;
		continue;

	    case DO:
		telrcv_state = TS_DO;
		continue;

	    case DONT:
		telrcv_state = TS_DONT;
		continue;

	    case DM:
		    /*
		     * We may have missed an urgent notification,
		     * so make sure we flush whatever is in the
		     * buffer currently.
		     */
		SYNCHing = 1;
		ttyflush(1);
		SYNCHing = stilloob(net);
		settimer(gotDM);
		break;

	    case NOP:
	    case GA:
		break;

	    case SB:
		SB_CLEAR();
		telrcv_state = TS_SB;
		continue;

#	    if defined(TN3270)
	    case EOR:
		if (In3270) {
		    Ibackp += DataFromNetwork(Ibackp, Ifrontp-Ibackp, 1);
		    if (Ibackp == Ifrontp) {
			Ibackp = Ifrontp = Ibuf;
			ISend = 0;	/* should have been! */
		    } else {
			ISend = 1;
		    }
		}
		break;
#	    endif /* defined(TN3270) */

	    case IAC:
#	    if !defined(TN3270)
		TTYADD(IAC);
#	    else /* !defined(TN3270) */
		if (In3270) {
		    *Ifrontp++ = IAC;
		} else {
		    TTYADD(IAC);
		}
#	    endif /* !defined(TN3270) */
		break;

	    default:
		break;
	    }
	    telrcv_state = TS_DATA;
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
	    SetIn3270();
	    telrcv_state = TS_DATA;
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
	    SetIn3270();
	    telrcv_state = TS_DATA;
	    continue;

	case TS_DO:
	    printoption(">RCVD", doopt, c, !myopts[c]);
	    if (!myopts[c])
		dooption(c);
	    SetIn3270();
	    telrcv_state = TS_DATA;
	    continue;

	case TS_DONT:
	    printoption(">RCVD", dont, c, myopts[c]);
	    if (myopts[c]) {
		myopts[c] = 0;
		netoprint(wont, c);
		flushline = 1;
		setconnmode();	/* set new tty mode (maybe) */
		printoption(">SENT", wont, c, 0);
	    }
	    SetIn3270();
	    telrcv_state = TS_DATA;
	    continue;

	case TS_SB:
	    if (c == IAC) {
		telrcv_state = TS_SE;
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
		telrcv_state = TS_SB;
	    } else {
		SB_TERM();
		suboption();	/* handle sub-option */
		SetIn3270();
		telrcv_state = TS_DATA;
	    }
	}
    }
    ring_sent_acked(&netiring, count);
    return returnValue||count;
}

static int
telsnd(ring)
Ring	*ring;			/* Input ring */
{
    int tcc;
    int count;
    int returnValue = 0;
    char *tbp;

    tcc = 0;
    count = 0;
    while (NETROOM() > 2) {
	register int sc;
	register int c;

	if (tcc == 0) {
	    if (count) {
		ring_sent_acked(&ttyiring, count);
		returnValue = 1;
		count = 0;
	    }
	    tbp = ttyiring.send;
	    tcc = ring_unsent_consecutive(&ttyiring);
	    if (tcc == 0) {
		break;
	    }
	}
	c = *tbp++ & 0xff, sc = strip(c), tcc--; count++;
	if (sc == escape) {
	    command(0);
	    tcc = 0;
	    flushline = 1;
	    break;
	} else if (MODE_LINE(globalmode) && (sc == echoc)) {
	    if (tcc > 0 && strip(*tbp) == echoc) {
		tcc--; tbp++; count++;
	    } else {
		dontlecho = !dontlecho;
		settimer(echotoggle);
		setconnmode();
		flushline = 1;
		break;
	    }
	}
	if (localchars) {
	    if (TerminalSpecialChars(sc) == 0) {
		break;
	    }
	}
	if (!myopts[TELOPT_BINARY]) {
	    switch (c) {
	    case '\n':
		    /*
		     * If we are in CRMOD mode (\r ==> \n)
		     * on our local machine, then probably
		     * a newline (unix) is CRLF (TELNET).
		     */
		if (MODE_LOCAL_CHARS(globalmode)) {
		    NETADD('\r');
		}
		NETADD('\n');
		flushline = 1;
		break;
	    case '\r':
		if (!crlf) {
		    NET2ADD('\r', '\0');
		} else {
		    NET2ADD('\r', '\n');
		}
		flushline = 1;
		break;
	    case IAC:
		NET2ADD(IAC, IAC);
		break;
	    default:
		NETADD(c);
		break;
	    }
	} else if (c == IAC) {
	    NET2ADD(IAC, IAC);
	} else {
	    NETADD(c);
	}
    }
    ring_sent_acked(&ttyiring, count);
    return returnValue||count;		/* Non-zero if we did anything */
}

#if	defined(TN3270)
static void
SetForExit()
{
    setconnmode();
    if (In3270) {
	Finish3270();
    }
    setcommandmode();
    fflush(stdout);
    fflush(stderr);
    if (In3270) {
	StopScreen(1);
    }
    setconnmode();
    setcommandmode();
}

static void
Exit(returnCode)
int returnCode;
{
    SetForExit();
    exit(returnCode);
}

void
ExitString(string, returnCode)
char *string;
int returnCode;
{
    SetForExit();
    fwrite(string, 1, strlen(string), stderr);
    exit(returnCode);
}

void
ExitPerror(string, returnCode)
char *string;
int returnCode;
{
    SetForExit();
    perror(string);
    exit(returnCode);
}
#endif	/* defined(TN3270) */


/*
 * Scheduler()
 *
 * Try to do something.
 *
 * If we do something useful, return 1; else return 0.
 *
 */


int
Scheduler(block)
int	block;			/* should we block in the select ? */
{
    register int c;
		/* One wants to be a bit careful about setting returnValue
		 * to one, since a one implies we did some useful work,
		 * and therefore probably won't be called to block next
		 * time (TN3270 mode only).
		 */
    int returnValue = 0;
    static struct timeval TimeValue = { 0 };

    if ((!MODE_LINE(globalmode) || flushline) && NETBYTES()) {
	FD_SET(net, &obits);
    } 
#if	!defined(MSDOS)
    if (TTYBYTES()) {
	FD_SET(tout, &obits);
    }
#if	defined(TN3270)
    if ((tcc == 0) && NETROOM() && (shell_active == 0)) {
	FD_SET(tin, &ibits);
    }
#else	/* defined(TN3270) */
    if (ring_empty_count(&netiring) && NETROOM()) {
	FD_SET(tin, &ibits);
    }
#endif	/* defined(TN3270) */
#endif	/* !defined(MSDOS) */
#   if !defined(TN3270)
    if (TTYROOM()) {
	FD_SET(net, &ibits);
    }
#   else /* !defined(TN3270) */
    if (!ISend && TTYROOM()) {
	FD_SET(net, &ibits);
    }
#   endif /* !defined(TN3270) */
    if (!SYNCHing) {
	FD_SET(net, &xbits);
    }
#   if defined(TN3270) && defined(unix)
    if (HaveInput) {
	HaveInput = 0;
	signal(SIGIO, inputAvailable);
    }
#endif	/* defined(TN3270) && defined(unix) */
    if ((c = select(16, &ibits, &obits, &xbits,
			block? (struct timeval *)0 : &TimeValue)) < 0) {
	if (c == -1) {
		    /*
		     * we can get EINTR if we are in line mode,
		     * and the user does an escape (TSTP), or
		     * some other signal generator.
		     */
	    if (errno == EINTR) {
		return 0;
	    }
#	    if defined(TN3270)
		    /*
		     * we can get EBADF if we were in transparent
		     * mode, and the transcom process died.
		    */
	    if (errno == EBADF) {
			/*
			 * zero the bits (even though kernel does it)
			 * to make sure we are selecting on the right
			 * ones.
			*/
		FD_ZERO(&ibits);
		FD_ZERO(&obits);
		FD_ZERO(&xbits);
		return 0;
	    }
#	    endif /* defined(TN3270) */
		    /* I don't like this, does it ever happen? */
	    printf("sleep(5) from telnet, after select\r\n");
#if	defined(unix)
	    sleep(5);
#endif	/* defined(unix) */
	}
	return 0;
    }

    /*
     * Any urgent data?
     */
    if (FD_ISSET(net, &xbits)) {
	FD_CLR(net, &xbits);
	SYNCHing = 1;
	ttyflush(1);	/* flush already enqueued data */
    }

    /*
     * Something to read from the network...
     */
    if (FD_ISSET(net, &ibits)) {
	int canread;

	FD_CLR(net, &ibits);
	canread = ring_empty_consecutive(&netiring);
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
		c = recv(net, sbp+scc, canread, MSG_OOB);
		if ((c == -1) && (errno == EINVAL)) {
		    c = recv(net, sbp+scc, canread, 0);
		    if (clocks.didnetreceive < clocks.gotDM) {
			SYNCHing = stilloob(net);
		    }
		}
	    } else {
		c = recv(net, sbp+scc, canread, 0);
	    }
	} else {
	    c = recv(net, sbp+scc, canread, 0);
	}
	settimer(didnetreceive);
#else	/* !defined(SO_OOBINLINE) */
	c = recv(net, netiring.add, canread, 0);
#endif	/* !defined(SO_OOBINLINE) */
	if (c < 0 && errno == EWOULDBLOCK) {
	    c = 0;
	} else if (c <= 0) {
	    return -1;
	}
	if (netdata) {
	    Dump('<', netiring.add, c);
	}
	ring_added(&netiring, c);
	returnValue = 1;
    }

    /*
     * Something to read from the tty...
     */
#if	defined(MSDOS)
    if ((tcc == 0) && NETROOM() && (shell_active == 0) && TerminalCanRead())
#else	/* defined(MSDOS) */
    if (FD_ISSET(tin, &ibits))
#endif	/* defined(MSDOS) */
				    {
	FD_CLR(tin, &ibits);
	c = TerminalRead(tin, ttyiring.add, ring_empty_consecutive(&ttyiring));
	if (c < 0 && errno == EWOULDBLOCK) {
	    c = 0;
	} else {
#if	defined(unix)
	    /* EOF detection for line mode!!!! */
	    if (c == 0 && MODE_LOCAL_CHARS(globalmode)) {
			/* must be an EOF... */
		*ttyiring.add = termEofChar;
		c = 1;
	    }
#endif	/* defined(unix) */
	    if (c <= 0) {
		return -1;
	    }
	}
	ring_added(&ttyiring, c);
	returnValue = 1;		/* did something useful */
    }

#   if defined(TN3270)
    if (ring_unsent_count(&ttyiring)) {
	if (In3270) {
	    c = DataFromTerminal(ttyiring.send,
					ring_unsent_consecutive(&ttyiring));
	    if (c) {
		returnValue = 1;
	    }
	    ring_sent_acked(&ttyiring, c);
	} else {
#   endif /* defined(TN3270) */
	    returnValue |= telsnd(&ttyiring);
#   if defined(TN3270)
	}
    }
#   endif /* defined(TN3270) */

    if ((!MODE_LINE(globalmode) || flushline || myopts[TELOPT_BINARY]) &&
	FD_ISSET(net, &obits) && (NETBYTES() > 0)) {
	FD_CLR(net, &obits);
	returnValue = netflush();
    }
    if (ring_unsent_count(&netiring)) {
#	if !defined(TN3270)
	returnValue |= telrcv();
#	else /* !defined(TN3270) */
	returnValue = Push3270();
#	endif /* !defined(TN3270) */
    }
#if	defined(MSDOS)
    if (TTYBYTES())
#else	/* defined(MSDOS) */
    if (FD_ISSET(tout, &obits) && (TTYBYTES() > 0))
#endif	/* defined(MSDOS) */
						    {
	FD_CLR(tout, &obits);
	returnValue = ttyflush(SYNCHing|flushout);
    }
    return returnValue;
}

/*
 * Select from tty and network...
 */
void
telnet()
{
#if	defined(MSDOS)
#define	SCHED_BLOCK	0		/* Don't block in MSDOS */
#else	/* defined(MSDOS) */
#define	SCHED_BLOCK	1
#endif	/* defined(MSDOS) */

#if	defined(TN3270) && defined(unix)
    int myPid;
#endif	/* defined(TN3270) */

    tout = fileno(stdout);
    tin = fileno(stdin);
    setconnmode();
    FD_ZERO(&ibits);
    FD_ZERO(&obits);
    FD_ZERO(&xbits);

    NetNonblockingIO(net, 1);

#if	defined(TN3270)
    if (noasynch == 0) {			/* DBX can't handle! */
	NetSigIO(net, 1);
    }
    NetSetPgrp(net);
#endif	/* defined(TN3270) */


#if	defined(SO_OOBINLINE) && !defined(MSDOS)
    SetSockOpt(net, SOL_SOCKET, SO_OOBINLINE, 1);
#endif	/* defined(SO_OOBINLINE) && !defined(MSDOS) */

#   if !defined(TN3270)
    if (telnetport) {
	if (!hisopts[TELOPT_SGA]) {
	    willoption(TELOPT_SGA, 0);
	}
	if (!myopts[TELOPT_TTYPE]) {
	    dooption(TELOPT_TTYPE, 0);
	}
    }
#   endif /* !defined(TN3270) */

#   if !defined(TN3270)
    for (;;) {
	int schedValue;

	while ((schedValue = Scheduler(0)) != 0) {
	    if (schedValue == -1) {
		setcommandmode();
		return;
	    }
	}

	if (Scheduler(SCHED_BLOCK) == -1) {
	    setcommandmode();
	    return;
	}
    }
#   else /* !defined(TN3270) */
    for (;;) {
	int schedValue;

	while (!In3270 && !shell_active) {
	    if (Scheduler(SCHED_BLOCK) == -1) {
		setcommandmode();
		return;
	    }
	}

	while ((schedValue = Scheduler(0)) != 0) {
	    if (schedValue == -1) {
		setcommandmode();
		return;
	    }
	}
		/* If there is data waiting to go out to terminal, don't
		 * schedule any more data for the terminal.
		 */
	if (tfrontp-tbackp) {
	    schedValue = 1;
	} else {
	    if (shell_active) {
		if (shell_continue() == 0) {
		    ConnectScreen();
		}
	    } else if (In3270) {
		schedValue = DoTerminalOutput();
	    }
	}
	if (schedValue && (shell_active == 0)) {
	    if (Scheduler(SCHED_BLOCK) == -1) {
		setcommandmode();
		return;
	    }
	}
    }
#   endif /* !defined(TN3270) */
}

/*
 * These routines add various telnet commands to the data stream.
 */

void
xmitAO()
{
    NET2ADD(IAC, AO);
    if (autoflush) {
	doflush();
    }
}


void
xmitEL()
{
    NET2ADD(IAC, EL);
}

void
xmitEC()
{
    NET2ADD(IAC, EC);
}


#if	defined(NOT43)
int
#else	/* defined(NOT43) */
void
#endif	/* defined(NOT43) */
dosynch()
{
    netclear();			/* clear the path to the network */
    NET2ADD(IAC, DM);

#if	defined(NOT43)
    return 0;
#endif	/* defined(NOT43) */
}

void
doflush()
{
    NET2ADD(IAC, DO);
    NETADD(TELOPT_TM);
    flushline = 1;
    flushout = 1;
    ttyflush(1);			/* Flush/drop output */
    /* do printoption AFTER flush, otherwise the output gets tossed... */
    printoption("<SENT", doopt, TELOPT_TM, 0);
}

void
intp()
{
    NET2ADD(IAC, IP);
    flushline = 1;
    if (autoflush) {
	doflush();
    }
    if (autosynch) {
	dosynch();
    }
}

void
sendbrk()
{
    NET2ADD(IAC, BREAK);
    flushline = 1;
    if (autoflush) {
	doflush();
    }
    if (autosynch) {
	dosynch();
    }
}
