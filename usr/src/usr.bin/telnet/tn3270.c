#include "fdset.h"

void
tn3270_init()
{
#if	defined(TN3270)
    Sent3270TerminalType = 0;
    Ifrontp = Ibackp = Ibuf;
    init_ctlr();		/* Initialize some things */
    init_keyboard();
    init_screen();
    init_system();
#endif	/* defined(TN3270) */
}

#if	defined(TN3270)


static char	Ibuf[8*BUFSIZ], *Ifrontp, *Ibackp;

static char	sb_terminal[] = { IAC, SB,
			TELOPT_TTYPE, TELQUAL_IS,
			'I', 'B', 'M', '-', '3', '2', '7', '8', '-', '2',
			IAC, SE };
#define	SBTERMMODEL	13

static int
	Sent3270TerminalType;	/* Have we said we are a 3270? */


#if	defined(unix)
static
settranscom(argc, argv)
	int argc;
	char *argv[];
{
	int i, len = 0;
	char *strcpy(), *strcat();

	if (argc == 1 && transcom) {
	   transcom = 0;
	}
	if (argc == 1) {
	   return;
	}
	for (i = 1; i < argc; ++i) {
	    len += 1 + strlen(argv[1]);
	}
	transcom = tline;
	(void) strcpy(transcom, argv[1]);
	for (i = 2; i < argc; ++i) {
	    (void) strcat(transcom, " ");
	    (void) strcat(transcom, argv[i]);
	}
}
#endif	/* defined(unix) */

/*
 * DataToNetwork - queue up some data to go to network.  If "done" is set,
 * then when last byte is queued, we add on an IAC EOR sequence (so,
 * don't call us with "done" until you want that done...)
 *
 * We actually do send all the data to the network buffer, since our
 * only client needs for us to do that.
 */

int
DataToNetwork(buffer, count, done)
register char	*buffer;	/* where the data is */
register int	count;		/* how much to send */
int		done;		/* is this the last of a logical block */
{
    register int c;
    int origCount;
    fd_set o;

    origCount = count;
    FD_ZERO(&o);

    while (count) {
	if ((netobuf+sizeof netobuf - nfrontp) < 6) {
	    netflush();
	    while ((netobuf+sizeof netobuf - nfrontp) < 6) {
		FD_SET(net, &o);
		(void) select(net+1, (fd_set *) 0, &o, (fd_set *) 0,
						(struct timeval *) 0);
		netflush();
	    }
	}
	c = *buffer++;
	count--;
	if (c == IAC) {
	    *nfrontp++ = IAC;
	    *nfrontp++ = IAC;
	} else {
	    *nfrontp++ = c;
	}
    }

    if (done && !count) {
	*nfrontp++ = IAC;
	*nfrontp++ = EOR;
	netflush();		/* try to move along as quickly as ... */
    }
    return(origCount - count);
}


#if	defined(unix)
static void
inputAvailable()
{
    HaveInput = 1;
}
#endif	/* defined(unix) */

void
outputPurge()
{
    ttyflush(1);
}


/*
 * The following routines are places where the various tn3270
 * routines make calls into telnet.c.
 */

/* TtyChars() - returns the number of characters in the TTY buffer */
TtyChars()
{
    return(tfrontp-tbackp);
}

/* DataToTerminal - queue up some data to go to terminal. */

int
DataToTerminal(buffer, count)
register char	*buffer;		/* where the data is */
register int	count;			/* how much to send */
{
    int origCount;
#if	defined(unix)
    fd_set	o;

    FD_ZERO(&o);
#endif	/* defined(unix) */
    origCount = count;

    while (count) {
	if (tfrontp >= ttyobuf+sizeof ttyobuf) {
	    ttyflush(0);
	    while (tfrontp >= ttyobuf+sizeof ttyobuf) {
#if	defined(unix)
		FD_SET(tout, &o);
		(void) select(tout+1, (fd_set *) 0, &o, (fd_set *) 0,
						(struct timeval *) 0);
#endif	/* defined(unix) */
		ttyflush(0);
	    }
	}
	*tfrontp++ = *buffer++;
	count--;
    }
    return(origCount - count);
}

/* EmptyTerminal - called to make sure that the terminal buffer is empty.
 *			Note that we consider the buffer to run all the
 *			way to the kernel (thus the select).
 */

void
EmptyTerminal()
{
#if	defined(unix)
    fd_set	o;

    FD_ZERO(&o);
#endif	/* defined(unix) */

    if (tfrontp == tbackp) {
#if	defined(unix)
	FD_SET(tout, &o);
	(void) select(tout+1, (int *) 0, &o, (int *) 0,
			(struct timeval *) 0);	/* wait for TTLOWAT */
#endif	/* defined(unix) */
    } else {
	while (tfrontp != tbackp) {
	    ttyflush(0);
#if	defined(unix)
	    FD_SET(tout, &o);
	    (void) select(tout+1, (int *) 0, &o, (int *) 0,
				(struct timeval *) 0);	/* wait for TTLOWAT */
#endif	/* defined(unix) */
	}
    }
}


/*
 * Push3270 - Try to send data along the 3270 output (to screen) direction.
 */

static int
Push3270()
{
    int save = scc;

    if (scc) {
	if (Ifrontp+scc > Ibuf+sizeof Ibuf) {
	    if (Ibackp != Ibuf) {
		memcpy(Ibuf, Ibackp, Ifrontp-Ibackp);
		Ifrontp -= (Ibackp-Ibuf);
		Ibackp = Ibuf;
	    }
	}
	if (Ifrontp+scc < Ibuf+sizeof Ibuf) {
	    telrcv();
	}
    }
    return save != scc;
}


/*
 * Finish3270 - get the last dregs of 3270 data out to the terminal
 *		before quitting.
 */

static void
Finish3270()
{
    while (Push3270() || !DoTerminalOutput()) {
#if	defined(unix)
	HaveInput = 0;
#endif	/* defined(unix) */
	;
    }
}


/* StringToTerminal - output a null terminated string to the terminal */

void
StringToTerminal(s)
char *s;
{
    int count;

    count = strlen(s);
    if (count) {
	(void) DataToTerminal(s, count);	/* we know it always goes... */
    }
}


#if	((!defined(NOT43)) || defined(PUTCHAR))
/* _putchar - output a single character to the terminal.  This name is so that
 *	curses(3x) can call us to send out data.
 */

void
_putchar(c)
char c;
{
    if (tfrontp >= ttyobuf+sizeof ttyobuf) {
	(void) DataToTerminal(&c, 1);
    } else {
	*tfrontp++ = c;		/* optimize if possible. */
    }
}
#endif	/* ((!defined(NOT43)) || defined(PUTCHAR)) */

void
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

void
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

void
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

/*
 * tn3270_ttype()
 *
 *	Send a response to a terminal type negotiation.
 *
 *	Return '0' if no more responses to send; '1' if a response sent.
 */

int
tn3270_ttype()
{
    /*
     * Try to send a 3270 type terminal name.  Decide which one based
     * on the format of our screen, and (in the future) color
     * capaiblities.
     */
    InitTerminal();		/* Sets MaxNumberColumns, MaxNumberLines */
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
	ring_supply_data(&netoring, sb_terminal, sizeof sb_terminal);
	return 1;
    } else {
	return 0;
    }
}
#endif	/* defined(TN3270) */
