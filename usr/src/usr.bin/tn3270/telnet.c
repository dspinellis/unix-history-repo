/*
 *	Copyright (c) 1984, 1985, 1986 by the Regents of the
 *	University of California and by Gregory Glenn Minshall.
 *
 *	Permission to use, copy, modify, and distribute these
 *	programs and their documentation for any purpose and
 *	without fee is hereby granted, provided that this
 *	copyright and permission appear on all copies and
 *	supporting documentation, the name of the Regents of
 *	the University of California not be used in advertising
 *	or publicity pertaining to distribution of the programs
 *	without specific prior permission, and notice be given in
 *	supporting documentation that copying and distribution is
 *	by permission of the Regents of the University of California
 *	and by Gregory Glenn Minshall.  Neither the Regents of the
 *	University of California nor Gregory Glenn Minshall make
 *	representations about the suitability of this software
 *	for any purpose.  It is provided "as is" without
 *	express or implied warranty.
 */

#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1984, 1985, 1986 Regents of the University of California.\n\
 All rights reserved.\n";
#endif	/* not lint */

#ifndef lint
static char sccsid[] = "@(#)telnet.c	3.1  10/29/86";
#endif	/* not lint */

/*
 * User telnet program, modified for use by tn3270.c.
 *
 * Many of the FUNCTIONAL changes in this newest version of TELNET
 * were suggested by Dave Borman of Cray Research, Inc.
 *
 * Other changes in the tn3270 side come from Alan Crosswell (Columbia),
 * Bob Braden (ISI), Steve Jacobson (Berkeley), and Cliff Frost (Berkeley).
 *
 * This code is common between telnet(1c) and tn3270(1c).  There are the
 * following defines used to generate the various versions:
 *
 *	TN3270		- 	This is to be linked with tn3270.
 *
 *	NOT43		-	Allows the program to compile and run on
 *				a 4.2BSD system.
 *
 *	PUTCHAR		-	Within tn3270, on a NOT43 system,
 *				allows the use of the 4.3 curses
 *				(greater speed updating the screen).
 *				You need the 4.3 curses for this to work.
 *
 *	FD_SETSIZE	-	On whichever system, if this isn't defined,
 *				we patch over the FD_SET, etc., macros with
 *				some homebrewed ones.
 *
 *	SO_OOBINLINE	-	This is a socket option which we would like
 *				to set to allow TCP urgent data to come
 *				to us "inline".  This is NECESSARY for
 *				CORRECT operation, and desireable for
 *				simpler operation.
 *
 *	LNOFLSH		-	Detects the presence of the LNOFLSH bit
 *				in the tty structure.
 *
 *	unix		-	Compiles in unix specific stuff.
 *
 *	MSDOS		-	Compiles in MSDOS specific stuff.
 *
 */

#if	!defined(TN3270)
#define	ExitString(f,s,r)	{ fprintf(f, s); exit(r); }
#define	Exit(x)			exit(x)
#define	SetIn3270()

void	setcommandmode(), command();	/* forward declarations */
#endif	/* !defined(TN3270) */

#include <sys/types.h>
#include <sys/socket.h>

#include <netinet/in.h>

#if	defined(unix)
/* By the way, we need to include curses.h before telnet.h since,
 * among other things, telnet.h #defines 'DO', which is a variable
 * declared in curses.h.
 */
#include <curses.h>
#endif	/* defined(unix) */

#define	TELOPTS
#include <arpa/telnet.h>

#if	!defined(NOT43)
#include <arpa/inet.h>
#else	/* !defined(NOT43) */
extern unsigned long inet_addr();
extern char	*inet_ntoa();
#endif	/* !defined(NOT43) */

#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <netdb.h>

#if	defined(unix)
#include <strings.h>
#else	/* defined(unix) */
#include <string.h>
#endif	/* defined(unix) */

#if	defined(TN3270)
#include "ascii/termin.ext"
#include "ctlr/screen.h"
#include "ctlr/oia.h"
#include "ctlr/options.ext"
#include "ctlr/outbound.ext"
#include "general/globals.h"
#include "telnet.ext"
#endif	/* defined(TN3270) */

#include "general/general.h"



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

static char	ttyobuf[2*BUFSIZ], *tfrontp, *tbackp;
#define	TTYADD(c)	{ if (!(SYNCHing||flushout)) { *tfrontp++ = c; } }
#define	TTYLOC()	(tfrontp)
#define	TTYMAX()	(ttyobuf+sizeof ttyobuf-1)
#define	TTYMIN()	(netobuf)
#define	TTYBYTES()	(tfrontp-tbackp)
#define	TTYROOM()	(TTYMAX()-TTYLOC()+1)

static char	netobuf[2*BUFSIZ], *nfrontp, *nbackp;
#define	NETADD(c)	{ *nfrontp++ = c; }
#define	NET2ADD(c1,c2)	{ NETADD(c1); NETADD(c2); }
#define NETLOC()	(nfrontp)
#define	NETMAX()	(netobuf+sizeof netobuf-1)
#define	NETBYTES()	(nfrontp-nbackp)
#define	NETROOM()	(NETMAX()-NETLOC()+1)
static char	*neturg;		/* one past last byte of urgent data */

static char	subbuffer[100],
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


static char	hisopts[256];
static char	myopts[256];

static char	doopt[] = { IAC, DO, '%', 'c', 0 };
static char	dont[] = { IAC, DONT, '%', 'c', 0 };
static char	will[] = { IAC, WILL, '%', 'c', 0 };
static char	wont[] = { IAC, WONT, '%', 'c', 0 };

struct cmd {
	char	*name;		/* command name */
	char	*help;		/* help string */
	int	(*handler)();	/* routine which executes command */
	int	dohelp;		/* Should we give general help information? */
	int	needconnect;	/* Do we need to be connected to execute? */
};

static char	sibuf[BUFSIZ], *sbp;
static char	tibuf[BUFSIZ], *tbp;
static fd_set ibits, obits, xbits;


static int
	connected,
	net,
	scc,
	tcc,
	showoptions,
	In3270,		/* Are we in 3270 mode? */
	ISend,		/* trying to send network data in */
	debug = 0,
	crmod,
	netdata,
	noasynch = 0,	/* User specified "-noasynch" on command line */
	askedSGA = 0,	/* We have talked about suppress go ahead */
	telnetport = 1;

static FILE	*NetTrace = 0;		/* Not in bss, since needs to stay */

#define	CONTROL(x)	((x)&0x1f)		/* CTRL(x) is not portable */

static char
	*prompt = 0,
	escape,
	echoc;

static int
	SYNCHing,		/* we are in TELNET SYNCH mode */
	flushout,		/* flush output */
	autoflush = 0,		/* flush output when interrupting? */
	autosynch,		/* send interrupt characters with SYNCH? */
	localchars,		/* we recognize interrupt/quit */
	donelclchars,	/* the user has set "localchars" */
	dontlecho,		/* do we suppress local echoing right now? */
	globalmode;

/*	The following are some tn3270 specific flags */
#if	defined(TN3270)

static int
	Sent3270TerminalType;	/* Have we said we are a 3270? */

/* Some real, live, globals. */
int
	tout,			/* Output file descriptor */
	tin;			/* Input file descriptor */

#else	/* defined(TN3270) */
static int tin, tout;		/* file descriptors */
#endif	/* defined(TN3270) */


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

static int	telrcv_state = TS_DATA;

static char	line[200];
static int	margc;
static char	*margv[20];

static jmp_buf	toplevel = { 0 };
static jmp_buf	peerdied;

extern	int errno;


static struct sockaddr_in sin;

static struct	servent *sp = 0;

static int	flushline;

static char	*hostname;
static char	hnamebuf[32];

/*
 * The following are some clocks used to decide how to interpret
 * the relationship between various variables.
 */

static struct {
    int
	system,			/* what the current time is */
	echotoggle,		/* last time user entered echo character */
	modenegotiated,		/* last time operating mode negotiated */
	didnetreceive,		/* last time we read data from network */
	gotDM;			/* when did we last see a data mark */
} clocks;

#define	settimer(x)	clocks.x = clocks.system++

/*	Various modes */
#define	MODE_LINE(m)	(modelist[m].modetype & LINE)
#define	MODE_LOCAL_CHARS(m)	(modelist[m].modetype &  LOCAL_CHARS)
#define	MODE_LOCAL_ECHO(m)	(modelist[m].modetype & LOCAL_ECHO)
#define	MODE_COMMAND_LINE(m)	(modelist[m].modetype & COMMAND_LINE)

#define	LOCAL_CHARS	0x01		/* Characters processed locally */
#define	LINE		0x02		/* Line-by-line mode of operation */
#define	LOCAL_ECHO	0x04		/* Echoing locally */
#define	COMMAND_LINE	0x08		/* Command line mode */

static struct {
    char *modedescriptions;
    char modetype;
} modelist[] = {
	{ "telnet command mode", COMMAND_LINE },
	{ "character-at-a-time mode", 0 },
	{ "character-at-a-time mode (local echo)", LOCAL_ECHO|LOCAL_CHARS },
	{ "line-by-line mode (remote echo)", LINE | LOCAL_CHARS },
	{ "line-by-line mode", LINE | LOCAL_ECHO | LOCAL_CHARS },
	{ "line-by-line mode (local echoing suppressed)", LINE | LOCAL_CHARS },
	{ "3270 mode", 0 },
};


/*
 * The following routines try to encapsulate what is system dependent
 * (at least between 4.x and dos) which is used in telnet.c.
 */

#if	defined(unix)
#include <sys/ioctl.h>
#include <sys/time.h>
#include <signal.h>

int
	HaveInput;		/* There is input available to scan */

#if	defined(TN3270)
static char	tline[200];
char	*transcom = 0;	/* transparent mode command (default: none) */
#endif	/* defined(TN3270) */

static struct	tchars otc = { 0 }, ntc = { 0 };
static struct	ltchars oltc = { 0 }, nltc = { 0 };
static struct	sgttyb ottyb = { 0 }, nttyb = { 0 };


#define	TerminalWrite(fd,buf,n)	write(fd,buf,n)
#define	TerminalRead(fd,buf,n)	read(fd,buf,n)

/*
 *
 */

static int
TerminalAutoFlush()					/* unix */
{
#if	defined(LNOFLSH)
    ioctl(0, TIOCLGET, (char *)&autoflush);
    return !(autoflush&LNOFLSH);	/* if LNOFLSH, no autoflush */
#else	/* LNOFLSH */
    return 1;
#endif	/* LNOFLSH */
}

/*
 * TerminalSpecialChars()
 *
 * Look at an input character to see if it is a special character
 * and decide what to do.
 *
 * Output:
 *
 *	0	Don't add this character.
 *	1	Do add this character
 */

int
TerminalSpecialChars(c)			/* unix */
int	c;
{
    void doflush(), intp(), sendbrk();

    if (c == ntc.t_intrc) {
	intp();
	return 0;
    } else if (c == ntc.t_quitc) {
	sendbrk();
	return 0;
    } else if (c == nltc.t_flushc) {
	NET2ADD(IAC, AO);
	if (autoflush) {
	    doflush();
	}
	return 0;
    } else if (!MODE_LOCAL_CHARS(globalmode)) {
	if (c == nttyb.sg_kill) {
	    NET2ADD(IAC, EL);
	    return 0;
	} else if (c == nttyb.sg_erase) {
	    NET2ADD(IAC, EC);
	    return 0;
	}
    }
    return 1;
}


/*
 * Flush output to the terminal
 */
 
static void
TerminalFlushOutput()				/* unix */
{
    (void) ioctl(fileno(stdout), TIOCFLUSH, (char *) 0);
}

static void
TerminalSaveState()				/* unix */
{
    ioctl(0, TIOCGETP, (char *)&ottyb);
    ioctl(0, TIOCGETC, (char *)&otc);
    ioctl(0, TIOCGLTC, (char *)&oltc);

    ntc = otc;
    nltc = oltc;
    nttyb = ottyb;
}

static void
TerminalRestoreState()				/* unix */
{
}

/*
 * TerminalNewMode - set up terminal to a specific mode.
 */


static void
TerminalNewMode(f)					/* unix */
register int f;
{
    static int prevmode = 0;
    struct tchars *tc;
    struct tchars tc3;
    struct ltchars *ltc;
    struct sgttyb sb;
    int onoff;
    int old;
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
    case 6:		/* 3270 mode - like 1, but with xon/xoff local */
		    /* (might be nice to have "6" in telnet also...) */
	    sb.sg_flags |= CBREAK;
	    if ((f == 1) || (f == 6)) {
		sb.sg_flags &= ~(ECHO|CRMOD);
	    } else {
		sb.sg_flags |= ECHO|CRMOD;
	    }
	    sb.sg_erase = sb.sg_kill = -1;
	    if (f == 6) {
		tc = &tc3;
		tc3 = notc;
		    /* get XON, XOFF characters */
		tc3.t_startc = otc.t_startc;
		tc3.t_stopc = otc.t_stopc;
	    } else {
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
		} else {
		    tc = &notc;
		}
	    }
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
    ioctl(tin, TIOCSLTC, (char *)ltc);
    ioctl(tin, TIOCSETC, (char *)tc);
    ioctl(tin, TIOCSETP, (char *)&sb);
#if	(!defined(TN3270)) || ((!defined(NOT43)) || defined(PUTCHAR))
    ioctl(tin, FIONBIO, (char *)&onoff);
    ioctl(tout, FIONBIO, (char *)&onoff);
#endif	/* (!defined(TN3270)) || ((!defined(NOT43)) || defined(PUTCHAR)) */
#if	defined(TN3270)
    if (noasynch == 0) {
	ioctl(tin, FIOASYNC, (char *)&onoff);
    }
#endif	/* defined(TN3270) */

    if (MODE_LINE(f)) {
	void doescape();

	signal(SIGTSTP, doescape);
    } else if (MODE_LINE(old)) {
	signal(SIGTSTP, SIG_DFL);
	sigsetmask(sigblock(0) & ~(1<<(SIGTSTP-1)));
    }
}


int
NetClose(net)
int	net;
{
    return close(net);
}


static void
NetNonblockingIO(fd, onoff)				/* unix */
int
	fd,
	onoff;
{
    ioctl(net, FIONBIO, (char *)&onoff);
}

static void
NetSigIO(fd, onoff)				/* unix */
int
	fd,
	onoff;
{
    ioctl(net, FIOASYNC, (char *)&onoff);	/* hear about input */
}

static void
NetSetPgrp(fd)				/* unix */
int fd;
{
    int myPid;

    myPid = getpid();
#if	defined(NOT43)
    myPid = -myPid;
#endif	/* defined(NOT43) */
    ioctl(net, SIOCSPGRP, (char *)&myPid);	/* set my pid */
}


#endif	/* defined(unix) */

#if	defined(MSDOS)
#include <time.h>
#include <signal.h>
#include <process.h>

#if	!defined(SO_OOBINLINE)
#define	SO_OOBINLINE
#endif	/* !defined(SO_OOBINLINE) */


static char
    termEofChar,
    termEraseChar,
    termFlushChar,
    termIntChar,
    termKillChar,
    termLiteralNextChar,
    termQuitChar;


/*
 * MSDOS doesn't have anyway of deciding whether a full-edited line
 * is ready to be read in, so we need to do character-by-character
 * reads, and then do the editing in the program (in the case where
 * we are supporting line-by-line mode).
 *
 * The following routines, which are internal to the MSDOS-specific
 * code, accomplish this miracle.
 */

#define Hex(c)	HEX[(c)&0xff]

static survivorSetup = 0;		/* Do we have ^C hooks in? */

static int
	lineend = 0,		/* There is a line terminator */
	ctrlCCount = 0;

static char	linein[200],		/* Where input line is assembled */
		*nextin = linein,	/* Next input character */
		*nextout = linein;	/* Next character to be consumed */

#define consumechar() \
    if ((++nextout) >= nextin) { \
	nextout = nextin = linein; \
	lineend = 0; \
    }

#define	characteratatime()	(!MODE_LINE(globalmode))	/* one by one */


/*
 * killone()
 *
 *  Erase the last character on the line.
 */

static void
killone()
{
    if (lineend) {
	return;			/* ??? XXX */
    }
    if (nextin == linein) {
	return;			/* Nothing to do */
    }
    nextin--;
    if (!(isspace(*nextin) || isprint(*nextin))) {
	putchar('\b');
	putchar(' ');
	putchar('\b');
    }
    putchar('\b');
    putchar(' ');
    putchar('\b');
}


/*
 * setlineend()
 *
 *  Decide if it's time to send the current line up to the user
 * process.
 */

static void
setlineend()
{
    if (nextin == nextout) {
	return;
    }
    if (characteratatime()) {
	lineend = 1;
    } else if (nextin >= (linein+sizeof linein)) {
	lineend = 1;
    } else {
	int c = *(nextin-1);
	if ((c == termIntChar)
		|| (c == termQuitChar)
		|| (c == termEofChar)) {
	    lineend = 1;
	} else if (c == termFlushChar) {
	    lineend = 1;
	} else if ((c == '\n') || (c == '\r')) {
	    lineend = 1;
	}
    }
    /* Otherwise, leave it alone (reset by 'consumechar') */
}

/*
 * OK, what we do here is:
 *
 *   o  If we are echoing, then
 *	o  Look for character erase, line kill characters
 *	o  Echo the character (using '^' if a control character)
 *   o  Put the character in the input buffer
 *   o  Set 'lineend' as necessary
 */

static void
DoNextChar(c)
int	c;			/* Character to process */
{
    static char literalnextcharacter = 0;

    if (nextin >= (linein+sizeof linein)) {
	putchar('\7');		/* Ring bell */
	setlineend();
	return;
    }
    if (MODE_LOCAL_CHARS(globalmode)) {
	/* Look for some special character */
	if (!literalnextcharacter) {
	    if (c == termEraseChar) {
		killone();
		setlineend();
		return;
	    } else if (c == termKillChar) {
		while (nextin != linein) {
		    killone();
		}
		setlineend();
		return;
	    } else if (c == termLiteralNextChar) {
		literalnextcharacter = 1;
		return;
	    }
	}

	if (MODE_LOCAL_ECHO(globalmode)) {
	    if ((literalnextcharacter == 0) && ((c == '\r') || (c == '\n'))) {
		putchar('\r');
		putchar('\n');
		c = '\n';
	    } else if (!isprint(c) && !isspace(c)) {
		putchar('^');
		putchar(c^0x40);
	    } else {
		putchar(c);
	    }
	}
	literalnextcharacter = 0;
    }
    *nextin++ = c;
    setlineend();
}

static int
inputExists()
{
    int input;
    static state = 0;

    while (ctrlCCount) {
	DoNextChar(0x03);
	ctrlCCount--;
    }
    if (lineend) {
	return 1;
    }
#if	1	/* For BIOS variety of calls */
    if (kbhit() == 0) {
	return lineend;
    }
    input = getch();			/* MSC - get console character */
    if ((input&0xff) == 0) {
	DoNextChar(0x01);		/* ^A */
    } else {
	DoNextChar(input&0xff);
    }
#else	/* 0 */
    if ((input = dirconio()) == -1) {
	return lineend;
    }
    if ((input&0xff) == 0) {
	if ((input&0xff00) == 0x0300) {		/* Null */
	    DoNextChar(0);
	} else {
	    DoNextChar(0x01);
	    if (input&0x8000) {
		DoNextChar(0x01);
		DoNextChar((input>>8)&0x7f);
	    } else {
		DoNextChar((input>>8)&0xff);
	    }
	}
    } else {
	DoNextChar(input&0xff);
    }
#endif	/* 0 */
    return lineend;
}


void
CtrlCInterrupt()
{
    if (!MODE_COMMAND_LINE(globalmode)) {
	ctrlCCount++;		/* XXX */
	signal(SIGINT, CtrlCInterrupt);
    } else {
	closeallsockets();
	exit(1);
    }
}

/*
 * The MSDOS routines, called from elsewhere.
 */


static int
TerminalAutoFlush()				/* MSDOS */
{
    return 1;
}

static int
TerminalCanRead()
{
    return inputExists();
}


/*
 * Flush output to the terminal
 */
 
static void
TerminalFlushOutput()				/* MSDOS */
{
}


static void
TerminalNewMode(f)				/* MSDOS */
register int f;
{
    globalmode = f;
    signal(SIGINT, CtrlCInterrupt);
}


int
TerminalRead(fd, buffer, count)
int	fd;
char	*buffer;
int	count;
{
    int done = 0;

    for (;;) {
	while (inputExists() && (done < count)) {
	    *buffer++ = *nextout;
	    consumechar();
	    done++;
	}
	if (done) {
	    return(done);
	} else {
	    return 0;
	}
    }
}


static void
TerminalSaveState()				/* MSDOS */
{
}

int
TerminalSpecialChars(c)			/* MSDOS */
{
    return 1;
}


static void
TerminalRestoreState()				/* MSDOS */
{
}


static int
TerminalWrite(fd, buffer, count)		/* MSDOS */
int	fd;
char	*buffer;
int	count;
{
    return fwrite(buffer, sizeof (char), count, stdout);
}


static int
NetClose(fd)
{
    return closesocket(fd);
}

static void
NetNonblockingIO(fd, onoff)				/* MSDOS */
int
	fd,
	onoff;
{
    if (SetSockOpt(net, SOL_SOCKET, SO_NONBLOCKING, onoff)) {
	perror("setsockop (SO_NONBLOCKING) ");
	ExitString(stderr, "exiting\n", 1);
    }
}

static void
NetSigIO(fd)				/* MSDOS */
int fd;
{
}

static void
NetSetPgrp(fd)				/* MSDOS */
int fd;
{
}


#endif	/* defined(MSDOS) */

/*
 * Initialize variables.
 */

static void
tninit()
{
#if	defined(TN3270)
    Sent3270TerminalType = 0;
    Ifrontp = Ibackp = Ibuf;
#endif	/* defined(TN3270) */

    tfrontp = tbackp = ttyobuf;
    nfrontp = nbackp = netobuf;
    
    /* Don't change telnetport */
    SB_CLEAR();
    ClearArray(hisopts);
    ClearArray(myopts);
    sbp = sibuf;
    tbp = tibuf;

    connected = net = scc = tcc = In3270 = ISend = 0;
    telnetport = 0;
#if	defined(unix)
    HaveInput = 0;
#endif	/* defined(unix) */

    SYNCHing = 0;

    errno = 0;

    flushline = 0;

    /* Don't change NetTrace */

    escape = CONTROL(']');
    echoc = CONTROL('E');

    flushline = 1;
    sp = getservbyname("telnet", "tcp");
    if (sp == 0) {
	ExitString(stderr, "telnet: tcp/telnet: unknown service\n",1);
	/*NOTREACHED*/
    }

#if	defined(TN3270)
    init_ctlr();		/* Initialize some things */
    init_keyboard();
    init_screen();
    init_system();
#endif	/* defined(TN3270) */
}

/*
 * Various utility routines.
 */

static void
makeargv()
{
    register char *cp;
    register char **argp = margv;

    margc = 0;
    cp = line;
    if (*cp == '!') {		/* Special case shell escape */
	*argp++ = "!";		/* No room in string to get this */
	margc++;
	cp++;
    }
    while (*cp) {
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

static char *ambiguous;		/* special return value */
#define Ambiguous(t)	((t)&ambiguous)


static char **
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
	for (c = table; (p = *c) != 0; c = (*next)(c)) {
		for (q = name;
		    (*q == *p) || (isupper(*q) && tolower(*q) == *p); p++, q++)
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

static
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
static char *
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

static void
upcase(argument)
register char *argument;
{
    register int c;

    while ((c = *argument) != 0) {
	if (islower(c)) {
	    *argument = toupper(c);
	}
	argument++;
    }
}

/*
 * SetSockOpt()
 *
 * Compensate for differences in 4.2 and 4.3 systems.
 */

static int
SetSockOpt(fd, level, option, yesno)
int
	fd,
	level,
	option,
	yesno;
{
#ifndef	NOT43
    return setsockopt(fd, level, option,
				(char *)&yesno, sizeof yesno);
#else	/* NOT43 */
    if (yesno == 0) {		/* Can't do that in 4.2! */
	fprintf(stderr, "Error: attempt to turn off an option 0x%x.\n",
				option);
	return -1;
    }
    return setsockopt(fd, level, option, 0, 0);
#endif	/* NOT43 */
}

/*
 * The following are routines used to print out debugging information.
 */


static void
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
static void
printoption(direction, fmt, option, what)
	char *direction, *fmt;
	int option, what;
{
	if (!showoptions)
		return;
	fprintf(NetTrace, "%s ", direction+1);
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
		fprintf(NetTrace, "%s %s", fmt, telopts[option]);
	else
		fprintf(NetTrace, "%s %d", fmt, option);
	if (*direction == '<') {
		fprintf(NetTrace, "\r\n");
		return;
	}
	fprintf(NetTrace, " (%s)\r\n", what ? "reply" : "don't reply");
}

static void
printsub(direction, pointer, length)
char	*direction,		/* "<" or ">" */
	*pointer;		/* where suboption data sits */
int	length;			/* length of suboption data */
{
    if (showoptions) {
	fprintf(NetTrace, "%s suboption ",
				(direction[0] == '<')? "Received":"Sent");
	switch (pointer[0]) {
	case TELOPT_TTYPE:
	    fprintf(NetTrace, "Terminal type ");
	    switch (pointer[1]) {
	    case TELQUAL_IS:
		{
		    char tmpbuf[sizeof subbuffer];
		    int minlen = min(length, sizeof tmpbuf);

		    memcpy(tmpbuf, pointer+2, minlen);
		    tmpbuf[minlen-1] = 0;
		    fprintf(NetTrace, "is %s.\n", tmpbuf);
		}
		break;
	    case TELQUAL_SEND:
		fprintf(NetTrace, "- request to send.\n");
		break;
	    default:
		fprintf(NetTrace,
				"- unknown qualifier %d (0x%x).\n", pointer[1]);
	    }
	    break;
	default:
	    fprintf(NetTrace, "Unknown option %d (0x%x)\n",
					pointer[0], pointer[0]);
	}
    }
}

/*
 * Check to see if any out-of-band data exists on a socket (for
 * Telnet "synch" processing).
 */

static int
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
    } while ((value == -1) && (errno == EINTR));

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
 *
 *		The return value indicates whether we did any
 *	useful work.
 */


int
netflush()
{
    int n;

    if ((n = nfrontp - nbackp) > 0) {
	if (!neturg) {
	    n = send(net, nbackp, n, 0);	/* normal write */
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
		n = send(net, nbackp, n-1, 0);	/* send URGENT all by itself */
	    } else {
		n = send(net, nbackp, n, MSG_OOB);	/* URGENT data */
	    }
	}
    }
    if (n < 0) {
	if (errno != ENOBUFS && errno != EWOULDBLOCK) {
	    setcommandmode();
	    perror(hostname);
	    NetClose(net);
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
    return n > 0;
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
	    memcpy(good, thisitem, length);
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
 * These routines add various telnet commands to the data stream.
 */

#if	defined(NOT43)
static int
#else	/* defined(NOT43) */
static void
#endif	/* defined(NOT43) */
dosynch()
{
    netclear();			/* clear the path to the network */
    NET2ADD(IAC, DM);
    neturg = NETLOC()-1;	/* Some systems are off by one XXX */

#if	defined(NOT43)
    return 0;
#endif	/* defined(NOT43) */
}

static void
doflush()
{
    NET2ADD(IAC, DO);
    NETADD(TELOPT_TM);
    flushline = 1;
    flushout = 1;
    ttyflush();
    /* do printoption AFTER flush, otherwise the output gets tossed... */
    printoption("<SENT", doopt, TELOPT_TM, 0);
}

static void
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

static void
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

/*
 *		Send as much data as possible to the terminal.
 *
 *		The return value indicates whether we did any
 *	useful work.
 */


static int
ttyflush()
{
    int n;

    if ((n = tfrontp - tbackp) > 0) {
	if (!(SYNCHing||flushout)) {
	    n = TerminalWrite(tout, tbackp, n);
	} else {
	    TerminalFlushOutput();
	    /* we leave 'n' alone! */
	}
    }
    if (n >= 0) {
	tbackp += n;
	if (tbackp == tfrontp) {
	    tbackp = tfrontp = ttyobuf;
	}
    }
    return n > 0;
}

#if	defined(TN3270)

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
    int tmp = flushout;

    flushout = 1;

    ttyflush();

    flushout = tmp;
}

#endif	/* defined(TN3270) */

#if	defined(unix)
/*
 * Various signal handling routines.
 */

static void
deadpeer()
{
	setcommandmode();
	longjmp(peerdied, -1);
}

static void
intr()
{
    if (localchars) {
	intp();
	return;
    }
    setcommandmode();
    longjmp(toplevel, -1);
}

static void
intr2()
{
    if (localchars) {
	sendbrk();
	return;
    }
}

static void
doescape()
{
    command(0);
}
#endif	/* defined(unix) */

/*
 * These routines decides on what the mode should be (based on the values
 * of various global variables).
 */


static
getconnmode()
{
    static char newmode[16] =
			{ 4, 5, 3, 3, 2, 2, 1, 1, 6, 6, 6, 6, 6, 6, 6, 6 };
    int modeindex = 0;

    if (dontlecho && (clocks.echotoggle > clocks.modenegotiated)) {
	modeindex += 1;
    }
    if (hisopts[TELOPT_ECHO]) {
	modeindex += 2;
    }
    if (hisopts[TELOPT_SGA]) {
	modeindex += 4;
    }
    if (In3270) {
	modeindex += 8;
    }
    return newmode[modeindex];
}

void
setconnmode()
{
    TerminalNewMode(getconnmode());
}


void
setcommandmode()
{
    TerminalNewMode(0);
}

static void
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
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (dont) - 2;
	if (reply)
		printoption(">SENT", fmt, option, reply);
	else
		printoption("<SENT", fmt, option, reply);
}

static void
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
	sprintf(nfrontp, fmt, option);
	nfrontp += sizeof (doopt) - 2;
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
		    ExitString(stderr,
			"Programming error:  MAXSCREENSIZE too small.\n", 1);
		    /*NOTREACHED*/
		}
		memcpy(nfrontp, sb_terminal, sizeof sb_terminal);
		printsub(">", nfrontp+2, sizeof sb_terminal-2);
		nfrontp += sizeof sb_terminal;
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
		sprintf(nfrontp, "%c%c%c%c%s%c%c", IAC, SB, TELOPT_TTYPE,
				    TELQUAL_IS, namebuf, IAC, SE);
		printsub(">", nfrontp+2, 4+strlen(namebuf)+2-2-2);
		nfrontp += 4+strlen(namebuf)+2;
	    } else {
		ExitString(stderr, "No room in buffer for terminal type.\n",
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
					&& hisopts[TELOPT_BINARY]) {
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


static void
telrcv()
{
    register int c;
    static int telrcv_state = TS_DATA;
#   if defined(TN3270)
    register int Scc;
    register char *Sbp;
#   endif /* defined(TN3270) */

    while ((scc > 0) && (TTYROOM() > 2)) {
	c = *sbp++ & 0xff, scc--;
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
		Sbp = sbp;
		Scc = scc;
		while (Scc > 0) {
		    c = *Sbp++ & 0377, Scc--;
		    if (c == IAC) {
			telrcv_state = TS_IAC;
			break;
		    }
		    *Ifrontp++ = c;
		}
		sbp = Sbp;
		scc = Scc;
	    } else
#	    endif /* defined(TN3270) */
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
		ttyflush();
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
		sprintf(nfrontp, wont, c);
		nfrontp += sizeof (wont) - 2;
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
}

#if	defined(TN3270)

/*
 * The following routines are places where the various tn3270
 * routines make calls into telnet.c.
 */

/* TtyChars() - returns the number of characters in the TTY buffer */
TtyChars()
{
    return(tfrontp-tbackp);
}

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
	    ttyflush();
	    while (tfrontp >= ttyobuf+sizeof ttyobuf) {
#if	defined(unix)
		FD_SET(tout, &o);
		(void) select(tout+1, (fd_set *) 0, &o, (fd_set *) 0,
						(struct timeval *) 0);
#endif	/* defined(unix) */
		ttyflush();
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
	    ttyflush();
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


#if	defined(TN3270) && ((!defined(NOT43)) || defined(PUTCHAR))
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
#endif	/* defined(TN3270) && ((!defined(NOT43)) || defined(PUTCHAR)) */

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
ExitString(file, string, returnCode)
FILE *file;
char *string;
int returnCode;
{
    SetForExit();
    fwrite(string, 1, strlen(string), file);
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

    if (scc < 0 && tcc < 0) {
	return -1;
    }

    if ((!MODE_LINE(globalmode) || flushline) && NETBYTES()) {
	FD_SET(net, &obits);
    } 
#if	!defined(MSDOS)
    if (TTYBYTES()) {
	FD_SET(tout, &obits);
    }
    if ((tcc == 0) && NETROOM() && (shell_active == 0)) {
	FD_SET(tin, &ibits);
    }
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
	canread = sibuf + sizeof sibuf - (sbp+scc);
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
	c = recv(net, sbp+scc, canread, 0);
#endif	/* !defined(SO_OOBINLINE) */
	if (c < 0 && errno == EWOULDBLOCK) {
	    c = 0;
	} else if (c <= 0) {
	    return -1;
	}
	if (netdata) {
	    Dump('<', sbp+scc, c);
	}
	scc += c;
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
	if (tcc == 0) {
	    tbp = tibuf;	/* nothing left, reset */
	}
	c = TerminalRead(tin, tbp, tibuf+sizeof tibuf - tbp);
	if (c < 0 && errno == EWOULDBLOCK) {
	    c = 0;
	} else {
#if	defined(unix)
	    /* EOF detection for line mode!!!! */
	    if (c == 0 && MODE_LOCAL_CHARS(globalmode)) {
			/* must be an EOF... */
		*tbp = ntc.t_eofc;
		c = 1;
	    }
#endif	/* defined(unix) */
	    if (c <= 0) {
		tcc = c;
		return -1;
	    }
	}
	tcc += c;
	returnValue = 1;		/* did something useful */
    }

#   if defined(TN3270)
    if (tcc > 0) {
	if (In3270) {
	    c = DataFromTerminal(tbp, tcc);
	    if (c) {
		returnValue = 1;
	    }
	    tcc -= c;
	    tbp += c;
	} else {
#   endif /* defined(TN3270) */
	    returnValue = 1;
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
		} else if (MODE_LINE(globalmode) && (sc == echoc)) {
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
		    if (TerminalSpecialChars(sc) == 0) {
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
		    if (MODE_LOCAL_CHARS(globalmode)) {
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
#   if defined(TN3270)
	}
    }
#   endif /* defined(TN3270) */

    if ((!MODE_LINE(globalmode) || flushline) &&
	FD_ISSET(net, &obits) && (NETBYTES() > 0)) {
	FD_CLR(net, &obits);
	returnValue = netflush();
    }
    if (scc > 0) {
#	if !defined(TN3270)
	telrcv();
	returnValue = 1;
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
	returnValue = ttyflush();
    }
    return returnValue;
}

/*
 * Select from tty and network...
 */
static void
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
    scc = 0;
    tcc = 0;
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
	if (Scheduler(SCHED_BLOCK) == -1) {
	    setcommandmode();
	    return;
	}
    }
#   else /* !defined(TN3270) */
    for (;;) {
	int schedValue;

	while (!In3270) {
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
#if	defined(MSDOS)
		static int haventstopped = 1;

		setcommandmode();
		if (haventstopped) {
		    StopScreen(1);
		    haventstopped = 0;
		}
#endif	/* defined(MSDOS) */
		if (shell_continue() == 0) {
		    ConnectScreen();
#if	defined(MSDOS)
		    haventstopped = 1;
#endif	/* defined(MSDOS) */
		}
#if	defined(MSDOS)
		setconnmode();
#endif	/* defined(MSDOS) */
	    } else {
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
 *	The following are data structures and routines for
 *	the "send" command.
 *
 */
 
struct sendlist {
    char	*name;		/* How user refers to it (case independent) */
    int		what;		/* Character to be sent (<0 ==> special) */
    char	*help;		/* Help information (0 ==> no help) */
#if	defined(NOT43)
    int		(*routine)();	/* Routine to perform (for special ops) */
#else	/* defined(NOT43) */
    void	(*routine)();	/* Routine to perform (for special ops) */
#endif	/* defined(NOT43) */
};

#define	SENDQUESTION	-1
#define	SENDESCAPE	-3

static struct sendlist Sendlist[] = {
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

static struct sendlist Sendlist2[] = {		/* some synonyms */
	{ "break", BREAK, 0 },

	{ "intp", IP, 0 },
	{ "interrupt", IP, 0 },
	{ "intr", IP, 0 },

	{ "help", SENDQUESTION, 0 },

	{ 0 }
};

static char **
getnextsend(name)
char *name;
{
    struct sendlist *c = (struct sendlist *) name;

    return (char **) (c+1);
}

static struct sendlist *
getsend(name)
char *name;
{
    struct sendlist *sl;

    if ((sl = (struct sendlist *)
			genget(name, (char **) Sendlist, getnextsend)) != 0) {
	return sl;
    } else {
	return (struct sendlist *)
				genget(name, (char **) Sendlist2, getnextsend);
    }
}

static
sendcmd(argc, argv)
int	argc;
char	**argv;
{
    int what;		/* what we are sending this time */
    int count;		/* how many bytes we are going to need to send */
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
	if ((s = getsend(argv[i])) == 0) {
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

static
lclchars()
{
    donelclchars = 1;
    return 1;
}

static
togdebug()
{
#ifndef	NOT43
    if (net > 0 &&
	(SetSockOpt(net, SOL_SOCKET, SO_DEBUG, debug)) < 0) {
	    perror("setsockopt (SO_DEBUG)");
    }
#else	/* NOT43 */
    if (debug) {
	if (net > 0 && SetSockOpt(net, SOL_SOCKET, SO_DEBUG, 0, 0) < 0)
	    perror("setsockopt (SO_DEBUG)");
    } else
	printf("Cannot turn off socket debugging\n");
#endif	/* NOT43 */
    return 1;
}



extern int togglehelp();

struct togglelist {
    char	*name;		/* name of toggle */
    char	*help;		/* help message */
    int		(*handler)();	/* routine to do actual setting */
    int		dohelp;		/* should we display help information */
    int		*variable;
    char	*actionexplanation;
};

static struct togglelist Togglelist[] = {
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

static
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

static char **
getnexttoggle(name)
char *name;
{
    struct togglelist *c = (struct togglelist *) name;

    return (char **) (c+1);
}

static struct togglelist *
gettoggle(name)
char *name;
{
    return (struct togglelist *)
			genget(name, (char **) Togglelist, getnexttoggle);
}

static
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

static struct setlist Setlist[] = {
    { "echo", 	"character to toggle local echoing on/off", &echoc },
    { "escape",	"character to escape back to telnet command mode", &escape },
    { " ", "" },
    { " ", "The following need 'localchars' to be toggled true", 0 },
#if	defined(unix)
    { "erase",	"character to cause an Erase Character", &nttyb.sg_erase },
    { "flushoutput", "character to cause an Abort Oubput", &nltc.t_flushc },
    { "interrupt", "character to cause an Interrupt Process", &ntc.t_intrc },
    { "kill",	"character to cause an Erase Line", &nttyb.sg_kill },
    { "quit",	"character to cause a Break", &ntc.t_quitc },
    { "eof",	"character to cause an EOF ", &ntc.t_eofc },
#endif	/* defined(unix) */
#if	defined(MSDOS)
    { "erase",	"character to cause an Erase Character", &termEraseChar },
    { "flushoutput", "character to cause an Abort Oubput", &termFlushChar },
    { "interrupt", "character to cause an Interrupt Process", &termIntChar },
    { "kill",	"character to cause an Erase Line", &termKillChar },
    { "quit",	"character to cause a Break", &termQuitChar },
    { "eof",	"character to cause an EOF ", &termEofChar },
#endif	/* defined(MSDOS) */
    { 0 }
};

static char **
getnextset(name)
char *name;
{
    struct setlist *c = (struct setlist *)name;

    return (char **) (c+1);
}

static struct setlist *
getset(name)
char *name;
{
    return (struct setlist *) genget(name, (char **) Setlist, getnextset);
}

static
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

static
dolinemode()
{
    if (hisopts[TELOPT_SGA]) {
	wontoption(TELOPT_SGA, 0);
    }
    if (hisopts[TELOPT_ECHO]) {
	wontoption(TELOPT_ECHO, 0);
    }
    return 1;
}

static
docharmode()
{
    if (!hisopts[TELOPT_SGA]) {
	willoption(TELOPT_SGA, 0);
    }
    if (!hisopts[TELOPT_ECHO]) {
	willoption(TELOPT_ECHO, 0);
    }
    return 1;
}

static struct cmd Modelist[] = {
    { "character",	"character-at-a-time mode",	docharmode, 1, 1 },
    { "line",		"line-by-line mode",		dolinemode, 1, 1 },
    { 0 },
};

static char **
getnextmode(name)
char *name;
{
    struct cmd *c = (struct cmd *) name;

    return (char **) (c+1);
}

static struct cmd *
getmodecmd(name)
char *name;
{
    return (struct cmd *) genget(name, (char **) Modelist, getnextmode);
}

static
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

static
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
#undef	doset
#undef	dotog
}

/*
 * The following are the data structures, and many of the routines,
 * relating to command processing.
 */

/*
 * Set the escape character.
 */
static
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
	if (!In3270) {
		printf("Escape character is '%s'.\n", control(escape));
	}
	fflush(stdout);
	return 1;
}

/*VARARGS*/
static
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
#if	defined(unix)
	kill(0, SIGTSTP);
#endif	/* defined(unix) */
	/* reget parameters in case they were changed */
	TerminalSaveState();
	setconnmode();
	return 1;
}

/*VARARGS*/
static
bye(argc, argv)
int	argc;		/* Number of arguments */
char	*argv[];	/* arguments */
{
    if (connected) {
	shutdown(net, 2);
	printf("Connection closed.\n");
	NetClose(net);
	connected = 0;
	/* reset options */
	tninit();
#if	defined(TN3270)
	SetIn3270();		/* Get out of 3270 mode */
#endif	/* defined(TN3270) */
    }
    if ((argc != 2) || (strcmp(argv[1], "fromquit") != 0)) {
	longjmp(toplevel, 1);
	/* NOTREACHED */
    }
    return 1;			/* Keep lint, etc., happy */
}

/*VARARGS*/
quit()
{
	(void) call(bye, "bye", "fromquit", 0);
	Exit(0);
	/*NOTREACHED*/
	return 1;			/* just to keep lint happy */
}

/*
 * Print status about the connection.
 */
static
status(argc, argv)
int	argc;
char	*argv[];
{
    if (connected) {
	printf("Connected to %s.\n", hostname);
	if (argc < 2) {
	    printf("Operating in %s.\n",
				modelist[getconnmode()].modedescriptions);
	    if (localchars) {
		printf("Catching signals locally.\n");
	    }
	}
    } else {
	printf("No connection.\n");
    }
#   if !defined(TN3270)
    printf("Escape character is '%s'.\n", control(escape));
    fflush(stdout);
#   else /* !defined(TN3270) */
    if ((!In3270) && ((argc < 2) || strcmp(argv[1], "notmuch"))) {
	printf("Escape character is '%s'.\n", control(escape));
    }
#   if defined(unix)
    if (In3270 && transcom) {
       printf("Transparent mode command is '%s'.\n", transcom);
    }
#   endif /* defined(unix) */
    fflush(stdout);
    if (In3270) {
	return 0;
    }
#   endif /* defined(TN3270) */
    return 1;
}

#if	defined(TN3270) && defined(unix)
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
#endif	/* defined(TN3270) && defined(unix) */



static
tn(argc, argv)
	int argc;
	char *argv[];
{
    register struct hostent *host = 0;
#if defined(MSDOS)
    char *cp;
#endif	/* defined(MSDOS) */

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
    if (argc > 3) {
	printf("usage: %s host-name [port]\n", argv[0]);
	return 0;
    }
#if	defined(MSDOS)
    for (cp = argv[1]; *cp; cp++) {
	if (isupper(*cp)) {
	    *cp = tolower(*cp);
	}
    }
#endif	/* defined(MSDOS) */
    sin.sin_addr.s_addr = inet_addr(argv[1]);
    if (sin.sin_addr.s_addr != -1) {
	sin.sin_family = AF_INET;
	(void) strcpy(hnamebuf, argv[1]);
	hostname = hnamebuf;
    } else {
	host = gethostbyname(argv[1]);
	if (host) {
	    sin.sin_family = host->h_addrtype;
#if	defined(h_addr)		/* In 4.3, this is a #define */
	    memcpy((caddr_t)&sin.sin_addr,
				host->h_addr_list[0], host->h_length);
#else	/* defined(h_addr) */
	    memcpy((caddr_t)&sin.sin_addr, host->h_addr, host->h_length);
#endif	/* defined(h_addr) */
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
#if	defined(unix)
    signal(SIGINT, intr);
    signal(SIGQUIT, intr2);
    signal(SIGPIPE, deadpeer);
#endif	/* defined(unix) */
    printf("Trying...\n");
    do {
	net = socket(AF_INET, SOCK_STREAM, 0);
	if (net < 0) {
	    perror("telnet: socket");
	    return 0;
	}
	if (debug && SetSockOpt(net, SOL_SOCKET, SO_DEBUG, 1) < 0) {
		perror("setsockopt (SO_DEBUG)");
	}

	if (connect(net, (struct sockaddr *)&sin, sizeof (sin)) < 0) {
#if	defined(h_addr)		/* In 4.3, this is a #define */
	    if (host && host->h_addr_list[1]) {
		int oerrno = errno;

		fprintf(stderr, "telnet: connect to address %s: ",
						inet_ntoa(sin.sin_addr));
		errno = oerrno;
		perror((char *)0);
		host->h_addr_list++;
		memcpy((caddr_t)&sin.sin_addr, 
			host->h_addr_list[0], host->h_length);
		fprintf(stderr, "Trying %s...\n",
			inet_ntoa(sin.sin_addr));
		(void) NetClose(net);
		continue;
	    }
#endif	/* defined(h_addr) */
	    perror("telnet: Unable to connect to remote host");
#if defined(unix)
	    signal(SIGINT, SIG_DFL);
	    signal(SIGQUIT, SIG_DFL);
#endif	/* defined(unix) */
	    return 0;
	    }
	connected++;
    } while (connected == 0);
    call(status, "status", "notmuch", 0);
    if (setjmp(peerdied) == 0)
	telnet();
    NetClose(net);
    ExitString(stderr, "Connection closed by foreign host.\n",1);
    /*NOTREACHED*/
}


#define HELPINDENT (sizeof ("connect"))

static char
	openhelp[] =	"connect to a site",
	closehelp[] =	"close current connection",
	quithelp[] =	"exit telnet",
	statushelp[] =	"print status information",
	helphelp[] =	"print help information",
	sendhelp[] =	"transmit special characters ('send ?' for more)",
	sethelp[] = 	"set operating parameters ('set ?' for more)",
	togglestring[] ="toggle operating parameters ('toggle ?' for more)",
	displayhelp[] =	"display operating parameters",
#if	defined(TN3270) && defined(unix)
	transcomhelp[] = "specify Unix command for transparent mode pipe",
#endif	/* defined(TN3270) && defined(unix) */
#if	defined(unix)
	zhelp[] =	"suspend telnet",
#endif	/* defined(unix */
#if	defined(TN3270)
	shellhelp[] =	"invoke a subshell",
#endif	/* defined(TN3270) */
	modehelp[] = "try to enter line-by-line or character-at-a-time mode";

extern int	help(), shell();

static struct cmd cmdtab[] = {
	{ "close",	closehelp,	bye,		1, 1 },
	{ "display",	displayhelp,	display,	1, 0 },
	{ "mode",	modehelp,	modecmd,	1, 1 },
	{ "open",	openhelp,	tn,		1, 0 },
	{ "quit",	quithelp,	quit,		1, 0 },
	{ "send",	sendhelp,	sendcmd,	1, 1 },
	{ "set",	sethelp,	setcmd,		1, 0 },
	{ "status",	statushelp,	status,		1, 0 },
	{ "toggle",	togglestring,	toggle,		1, 0 },
#if	defined(TN3270) && defined(unix)
	{ "transcom",	transcomhelp,	settranscom,	1, 0 },
#endif	/* defined(TN3270) && defined(unix) */
#if	defined(unix)
	{ "z",		zhelp,		suspend,	1, 0 },
#endif	/* defined(unix) */
#if	defined(TN3270)
	{ "!",		shellhelp,	shell,		1, 1 },
#endif	/* defined(TN3270) */
	{ "?",		helphelp,	help,		1, 0 },
	0
};

static char	crmodhelp[] =	"deprecated command -- use 'toggle crmod' instead";
static char	escapehelp[] =	"deprecated command -- use 'set escape' instead";

static struct cmd cmdtab2[] = {
	{ "help",	helphelp,	help,		0, 0 },
	{ "escape",	escapehelp,	setescape,	1, 0 },
	{ "crmod",	crmodhelp,	togcrmod,	1, 0 },
	0
};

/*
 * Call routine with argc, argv set from args (terminated by 0).
 * VARARGS2
 */
static
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

static char **
getnextcmd(name)
char *name;
{
    struct cmd *c = (struct cmd *) name;

    return (char **) (c+1);
}

static struct cmd *
getcmd(name)
char *name;
{
    struct cmd *cm;

    if ((cm = (struct cmd *) genget(name, (char **) cmdtab, getnextcmd)) != 0) {
	return cm;
    } else {
	return (struct cmd *) genget(name, (char **) cmdtab2, getnextcmd);
    }
}

void
command(top)
	int top;
{
    register struct cmd *c;

    setcommandmode();
    if (!top) {
	putchar('\n');
    } else {
#if	defined(unix)
	signal(SIGINT, SIG_DFL);
	signal(SIGQUIT, SIG_DFL);
#endif	/* defined(unix) */
    }
    for (;;) {
	printf("%s> ", prompt);
	if (gets(line) == NULL) {
	    if (feof(stdin) || ferror(stdin))
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
	if (shell_active == 0) {
	    setconnmode();
	}
    }
}

/*
 * Help command.
 */
static
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
 * main.  Parse arguments, invoke the protocol or command parser.
 */


void
main(argc, argv)
	int argc;
	char *argv[];
{
    tninit();		/* Clear out things */

    NetTrace = stdout;
    TerminalSaveState();
    autoflush = TerminalAutoFlush();

    prompt = argv[0];
    while ((argc > 1) && (argv[1][0] == '-')) {
	if (!strcmp(argv[1], "-d")) {
	    debug = 1;
	} else if (!strcmp(argv[1], "-n")) {
	    if ((argc > 1) && (argv[2][0] != '-')) {	/* get file name */
		NetTrace = fopen(argv[2], "w");
		argv++;
		argc--;
		if (NetTrace == NULL) {
		    NetTrace = stdout;
		}
	    }
	} else {
#if	defined(TN3270) && defined(unix)
	    if (!strcmp(argv[1], "-t")) {
		if ((argc > 1) && (argv[1][0] != '-')) { /* get file name */
		    transcom = tline;
		    (void) strcpy(transcom, argv[1]);
		    argv++;
		    argc--;
		}
	    } else if (!strcmp(argv[1], "-noasynch")) {
		noasynch = 1;
	    } else
#endif	/* defined(TN3270) && defined(unix) */
	    if (argv[1][1] != '\0') {
		fprintf(stderr, "Unknown option *%s*.\n", argv[1]);
	    }
	}
	argc--;
	argv++;
    }
    if (argc != 1) {
	if (setjmp(toplevel) != 0)
	    Exit(0);
	tn(argc, argv);
    }
    setjmp(toplevel);
    for (;;) {
#if	!defined(TN3270)
	command(1);
#else	/* !defined(TN3270) */
	if (!shell_active) {
	    command(1);
	} else {
#if	defined(TN3270)
	    shell_continue();
#endif	/* defined(TN3270) */
	}
#endif	/* !defined(TN3270) */
    }
}
