#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1984-1987 Regents of the University of California.\n\
 All rights reserved.\n";
#endif	/* not lint */

#ifndef lint
static char sccsid[] = "@(#)telnet.c	1.2 (Berkeley) 9/25/87";
#endif	/* not lint */

#include <sys/types.h>

#if	defined(unix)
/* By the way, we need to include curses.h before telnet.h since,
 * among other things, telnet.h #defines 'DO', which is a variable
 * declared in curses.h.
 */
#include <curses.h>
#endif	/* defined(unix) */

#include <arpa/telnet.h>

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


#define	strip(x)	((x)&0x7f)


static char	subbuffer[SUBBUFSIZE],
		*subpointer, *subend;	 /* buffer for sub-options */
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

int
	connected,
	showoptions,
	In3270,		/* Are we in 3270 mode? */
	ISend,		/* trying to send network data in */
	debug = 0,
	crmod,
	netdata,	/* Print out network data flow */
	crlf,		/* Should '\r' be mapped to <CR><LF> (or <CR><NUL>)? */
	noasynch = 0,	/* User specified "-noasynch" on command line */
	askedSGA = 0,	/* We have talked about suppress go ahead */
	telnetport = 1,
	SYNCHing,	/* we are in TELNET SYNCH mode */
	flushout,	/* flush output */
	autoflush = 0,	/* flush output when interrupting? */
	autosynch,	/* send interrupt characters with SYNCH? */
	localchars,	/* we recognize interrupt/quit */
	donelclchars,	/* the user has set "localchars" */
	donebinarytoggle,	/* the user has put us in binary */
	dontlecho,	/* do we suppress local echoing right now? */
	globalmode;

#define	CONTROL(x)	((x)&0x1f)		/* CTRL(x) is not portable */

char
	*prompt = 0,
	escape,
	echoc;

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

    connected = In3270 = ISend = donebinarytoggle = 0;
    telnetport = 0;

#if	defined(unix) && defined(TN3270)
    HaveInput = 0;
#endif	/* defined(unix) && defined(TN3270) */

    SYNCHing = 0;

    /* Don't change NetTrace */

    escape = CONTROL(']');
    echoc = CONTROL('E');

    flushline = 1;
    telrcv_state = TS_DATA;
}


#include <varargs.h>

static void
printring(va_alist)
va_dcl
{
    va_list ap;
    char buffer[100];		/* where things go */
    char *ptr;
    char *format;
    char *string;
    Ring *ring;
    int i;

    va_start(ap);

    ring = va_arg(ap, Ring *);
    format = va_arg(ap, char *);
    ptr = buffer;

    while ((i = *format++) != 0) {
	if (i == '%') {
	    i = *format++;
	    switch (i) {
	    case 'c':
		*ptr++ = va_arg(ap, int);
		break;
	    case 's':
		string = va_arg(ap, char *);
		ring_supply_data(ring, buffer, ptr-buffer);
		ring_supply_data(ring, string, strlen(string));
		ptr = buffer;
		break;
	    case 0:
		ExitString("printring: trailing %%.\n", 1);
		/*NOTREACHED*/
	    default:
		ExitString("printring: unknown format character.\n", 1);
		/*NOTREACHED*/
	    }
	} else {
	    *ptr++ = i;
	}
    }
    ring_supply_data(ring, buffer, ptr-buffer);
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
	printring(&netoring, fmt, option);
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
	printring(&netoring, fmt, option);
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
	printring(&netoring, fmt, option);
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
	    if (tn3270_ttype()) {
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
		printring(&netoring, "%c%c%c%c%s%c%c", IAC, SB, TELOPT_TTYPE,
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
		ring_consumed(&netiring, count);
		returnValue = 1;
		count = 0;
	    }
	    sbp = netiring.consume;
	    scc = ring_full_consecutive(&netiring);
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
		if ((!hisopts[TELOPT_ECHO]) && !crmod) {
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
		SYNCHing = stilloob();
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
		printring(&netoring, wont, c);
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
    if (count)
	ring_consumed(&netiring, count);
    return returnValue||count;
}

static int
telsnd()
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
		ring_consumed(&ttyiring, count);
		returnValue = 1;
		count = 0;
	    }
	    tbp = ttyiring.consume;
	    tcc = ring_full_consecutive(&ttyiring);
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
    if (count)
	ring_consumed(&ttyiring, count);
    return returnValue||count;		/* Non-zero if we did anything */
}

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
    int returnValue;
    int netin, netout, netex, ttyin, ttyout;

    /* Decide which rings should be processed */

    netout = ring_full_count(&netoring) &&
	    (!MODE_LINE(globalmode) || flushline || myopts[TELOPT_BINARY]);
    ttyout = ring_full_count(&ttyoring);

#if	defined(TN3270)
    ttyin = ring_empty_count(&ttyiring) && (shell_active == 0);
#else	/* defined(TN3270) */
    ttyin = ring_empty_count(&ttyiring);
#endif	/* defined(TN3270) */

#if	defined(TN3270)
    netin = ring_empty_count(&netiring);
#   else /* !defined(TN3270) */
    netin = !ISend && ring_empty_count(&netiring);
#   endif /* !defined(TN3270) */

    netex = !SYNCHing;

    /* If we have seen a signal recently, reset things */
#   if defined(TN3270) && defined(unix)
    if (HaveInput) {
	HaveInput = 0;
	signal(SIGIO, inputAvailable);
    }
#endif	/* defined(TN3270) && defined(unix) */

    /* Call to system code to process rings */

    returnValue = process_rings(netin, netout, netex, ttyin, ttyout, !block);

    /* Now, look at the input rings, looking for work to do. */

    if (ring_full_count(&ttyiring)) {
#   if defined(TN3270)
	if (In3270) {
	    c = DataFromTerminal(ttyiring.send,
					ring_full_consecutive(&ttyiring));
	    if (c) {
		returnValue = 1;
	        ring_consumed(&ttyiring, c);
	    }
	} else {
#   endif /* defined(TN3270) */
	    returnValue |= telsnd();
#   if defined(TN3270)
	}
#   endif /* defined(TN3270) */
    }

    if (ring_full_count(&netiring)) {
#	if !defined(TN3270)
	returnValue |= telrcv();
#	else /* !defined(TN3270) */
	returnValue = Push3270();
#	endif /* !defined(TN3270) */
    }
    return returnValue;
}

/*
 * Select from tty and network...
 */
void
telnet()
{
    sys_telnet_init();

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

	if (Scheduler(1) == -1) {
	    setcommandmode();
	    return;
	}
    }
#   else /* !defined(TN3270) */
    for (;;) {
	int schedValue;

	while (!In3270 && !shell_active) {
	    if (Scheduler(1) == -1) {
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
	    if (Scheduler(1) == -1) {
		setcommandmode();
		return;
	    }
	}
    }
#   endif /* !defined(TN3270) */
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

static char *
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

static void
netclear()
{
#if	0	/* XXX */
    register char *thisitem, *next;
    char *good;
#define	wewant(p)	((nfrontp > p) && ((*p&0xff) == IAC) && \
				((*(p+1)&0xff) != EC) && ((*(p+1)&0xff) != EL))

    thisitem = netobuf;

    while ((next = nextitem(thisitem)) <= netobuf.send) {
	thisitem = next;
    }

    /* Now, thisitem is first before/at boundary. */

    good = netobuf;	/* where the good bytes go */

    while (netoring.add > thisitem) {
	if (wewant(thisitem)) {
	    int length;

	    next = thisitem;
	    do {
		next = nextitem(next);
	    } while (wewant(next) && (nfrontp > next));
	    length = next-thisitem;
	    memcpy(good, thisitem, length);
	    good += length;
	    thisitem = next;
	} else {
	    thisitem = nextitem(thisitem);
	}
    }

#endif	/* 0 */
}

/*
 * These routines add various telnet commands to the data stream.
 */

static void
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
