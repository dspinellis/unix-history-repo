/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that: (1) source distributions retain this entire copyright notice and
 * comment, and (2) distributions including binaries display the following
 * acknowledgement:  ``This product includes software developed by the
 * University of California, Berkeley and its contributors'' in the
 * documentation or other materials provided with the distribution and in
 * all advertising materials mentioning features or use of this software.
 * Neither the name of the University nor the names of its contributors may
 * be used to endorse or promote products derived from this software without
 * specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifndef lint
static char sccsid[] = "@(#)sys_bsd.c	1.28 (Berkeley) 7/28/90";
#endif /* not lint */

/*
 * The following routines try to encapsulate what is system dependent
 * (at least between 4.x and dos) which is used in telnet.c.
 */


#include <fcntl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <signal.h>
#include <errno.h>
#include <arpa/telnet.h>

#include "ring.h"

#include "fdset.h"

#include "defines.h"
#include "externs.h"
#include "types.h"

#if	defined(CRAY)
#define	SIG_FUNC_RET	void
#else
#define	SIG_FUNC_RET	int
#endif

int
	tout,			/* Output file descriptor */
	tin,			/* Input file descriptor */
	net;

#ifndef	USE_TERMIO
struct	tchars otc = { 0 }, ntc = { 0 };
struct	ltchars oltc = { 0 }, nltc = { 0 };
struct	sgttyb ottyb = { 0 }, nttyb = { 0 };
int	olmode = 0;

#else	/* USE_TERMIO */
struct	termio old_tc = { 0 };
extern struct termio new_tc;

#ifndef	TCGETA
# ifdef TCGETS
#  define	TCGETA	TCGETS
#  define	TCSETA	TCSETS
#  define	TCSETAW	TCSETSW
# else
#  define	TCGETA	TIOCGETA
#  define	TCSETA	TIOCSETA
#  define	TCSETAW	TIOCSETAW
# endif
#endif	/* TCGETA */
#endif	/* USE_TERMIO */

static fd_set ibits, obits, xbits;


init_sys()
{
    tout = fileno(stdout);
    tin = fileno(stdin);
    FD_ZERO(&ibits);
    FD_ZERO(&obits);
    FD_ZERO(&xbits);

    errno = 0;
}


TerminalWrite(buf, n)
char	*buf;
int	n;
{
    return write(tout, buf, n);
}

TerminalRead(buf, n)
char	*buf;
int	n;
{
    return read(tin, buf, n);
}

/*
 *
 */

int
TerminalAutoFlush()
{
#if	defined(LNOFLSH)
    int flush;

    ioctl(0, TIOCLGET, (char *)&flush);
    return !(flush&LNOFLSH);	/* if LNOFLSH, no autoflush */
#else	/* LNOFLSH */
    return 1;
#endif	/* LNOFLSH */
}

#ifdef	KLUDGELINEMODE
extern int kludgelinemode;
#endif
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
TerminalSpecialChars(c)
int	c;
{
    void xmitAO(), xmitEL(), xmitEC(), intp(), sendbrk();

    if (c == termIntChar) {
	intp();
	return 0;
    } else if (c == termQuitChar) {
#ifdef	KLUDGELINEMODE
	if (kludgelinemode)
	    sendbrk();
	else
#endif
	    sendabort();
	return 0;
    } else if (c == termEofChar) {
	if (my_want_state_is_will(TELOPT_LINEMODE)) {
	    sendeof();
	    return 0;
	}
	return 1;
    } else if (c == termSuspChar) {
	sendsusp();
	return(0);
    } else if (c == termFlushChar) {
	xmitAO();		/* Transmit Abort Output */
	return 0;
    } else if (!MODE_LOCAL_CHARS(globalmode)) {
	if (c == termKillChar) {
	    xmitEL();
	    return 0;
	} else if (c == termEraseChar) {
	    xmitEC();		/* Transmit Erase Character */
	    return 0;
	}
    }
    return 1;
}


/*
 * Flush output to the terminal
 */
 
void
TerminalFlushOutput()
{
#ifdef	TIOCFLUSH
    (void) ioctl(fileno(stdout), TIOCFLUSH, (char *) 0);
#else
    (void) ioctl(fileno(stdout), TCFLSH, (char *) 0);
#endif
}

void
TerminalSaveState()
{
#ifndef	USE_TERMIO
    ioctl(0, TIOCGETP, (char *)&ottyb);
    ioctl(0, TIOCGETC, (char *)&otc);
    ioctl(0, TIOCGLTC, (char *)&oltc);
    ioctl(0, TIOCLGET, (char *)&olmode);

    ntc = otc;
    nltc = oltc;
    nttyb = ottyb;

#else	/* USE_TERMIO */
    ioctl(0, TCGETA, &old_tc);

    new_tc = old_tc;

    termFlushChar = CONTROL('O');
    termWerasChar = CONTROL('W');
    termRprntChar = CONTROL('R');
    termLiteralNextChar = CONTROL('V');
    termStartChar = CONTROL('Q');
    termStopChar = CONTROL('S');
#endif	/* USE_TERMIO */
}

cc_t *
tcval(func)
register int func;
{
    switch(func) {
    case SLC_IP:	return(&termIntChar);
    case SLC_ABORT:	return(&termQuitChar);
    case SLC_EOF:	return(&termEofChar);
    case SLC_EC:	return(&termEraseChar);
    case SLC_EL:	return(&termKillChar);
    case SLC_XON:	return(&termStartChar);
    case SLC_XOFF:	return(&termStopChar);
    case SLC_FORW1:	return(&termForw1Char);
#ifndef	SYSV_TERMIO
    case SLC_AO:	return(&termFlushChar);
    case SLC_SUSP:	return(&termSuspChar);
    case SLC_EW:	return(&termWerasChar);
    case SLC_RP:	return(&termRprntChar);
    case SLC_LNEXT:	return(&termLiteralNextChar);
#endif	/* SYSV_TERMIO */
#ifdef	USE_TERMIO
    case SLC_FORW2:	return(&termForw2Char);
#endif

    case SLC_SYNCH:
    case SLC_BRK:
    case SLC_AYT:
    case SLC_EOR:
    default:
	return((cc_t *)0);
    }
}

void
TerminalDefaultChars()
{
#ifndef	USE_TERMIO
    ntc = otc;
    nltc = oltc;
    nttyb.sg_kill = ottyb.sg_kill;
    nttyb.sg_erase = ottyb.sg_erase;
#else	/* USE_TERMIO */
    memcpy(new_tc.c_cc, old_tc.c_cc, sizeof(old_tc.c_cc));
# ifndef	VFLUSHO
    termFlushChar = CONTROL('O');
# endif
# ifndef	VWERASE
    termWerasChar = CONTROL('W');
# endif
# ifndef	VREPRINT
    termRprntChar = CONTROL('R');
# endif
# ifndef	VLNEXT
    termLiteralNextChar = CONTROL('V');
# endif
# ifndef	VSTART
    termStartChar = CONTROL('Q');
# endif
# ifndef	VSTOP
    termStopChar = CONTROL('S');
# endif
#endif	/* USE_TERMIO */
}

#ifdef notdef
void
TerminalRestoreState()
{
}
#endif

/*
 * TerminalNewMode - set up terminal to a specific mode.
 *	MODE_ECHO: do local terminal echo
 *	MODE_FLOW: do local flow control
 *	MODE_TRAPSIG: do local mapping to TELNET IAC sequences
 *	MODE_EDIT: do local line editing
 *
 *	Command mode:
 *		MODE_ECHO|MODE_EDIT|MODE_FLOW|MODE_TRAPSIG
 *		local echo
 *		local editing
 *		local xon/xoff
 *		local signal mapping
 *
 *	Linemode:
 *		local/no editing
 *	Both Linemode and Single Character mode:
 *		local/remote echo
 *		local/no xon/xoff
 *		local/no signal mapping
 */


void
TerminalNewMode(f)
register int f;
{
    static int prevmode = 0;
#ifndef	USE_TERMIO
    struct tchars tc;
    struct ltchars ltc;
    struct sgttyb sb;
    int lmode;
#else	/* USE_TERMIO */
    struct termio tmp_tc;
#endif	/* USE_TERMIO */
    int onoff;
    int old;

    globalmode = f&~MODE_FORCE;
    if (prevmode == f)
	return;

    /*
     * Write any outstanding data before switching modes
     * ttyflush() returns 0 only when there is no more data
     * left to write out, it returns -1 if it couldn't do
     * anything at all, otherwise it returns 1 + the number
     * of characters left to write.
     */
    old = ttyflush(SYNCHing|flushout);
    if (old < 0 || old > 1) {
#ifndef	USE_TERMIO
	ioctl(tin, TIOCGETP, (char *)&sb);
#else	/* USE_TERMIO */
	ioctl(tin, TCGETA, (char *)&tmp_tc);
#endif	/* USE_TERMIO */
	do {
	    /*
	     * Wait for data to drain, then flush again.
	     */
#ifndef	USE_TERMIO
	    ioctl(tin, TIOCSETP, (char *)&sb);
#else	/* USE_TERMIO */
	    ioctl(tin, TCSETAW, (char *)&tmp_tc);
#endif	/* USE_TERMIO */
	    old = ttyflush(SYNCHing|flushout);
	} while (old < 0 || old > 1);
    }

    old = prevmode;
    prevmode = f&~MODE_FORCE;
#ifndef	USE_TERMIO
    sb = nttyb;
    tc = ntc;
    ltc = nltc;
    lmode = olmode;
#else
    tmp_tc = new_tc;
#endif

    if (f&MODE_ECHO) {
#ifndef	USE_TERMIO
	sb.sg_flags |= ECHO;
#else
	tmp_tc.c_lflag |= ECHO;
	tmp_tc.c_oflag |= ONLCR;
# ifdef notdef
	tmp_tc.c_iflag |= ICRNL;
# endif
#endif
    } else {
#ifndef	USE_TERMIO
	sb.sg_flags &= ~ECHO;
#else
	tmp_tc.c_lflag &= ~ECHO;
	tmp_tc.c_oflag &= ~ONLCR;
# ifdef notdef
	tmp_tc.c_iflag &= ~ICRNL;
# endif
#endif
    }

    if ((f&MODE_FLOW) == 0) {
#ifndef	USE_TERMIO
	tc.t_startc = -1;
	tc.t_stopc = -1;
#else
	tmp_tc.c_iflag &= ~(IXANY|IXOFF|IXON);
    } else {
	tmp_tc.c_iflag |= IXANY|IXOFF|IXON;
#endif
    }

    if ((f&MODE_TRAPSIG) == 0) {
#ifndef	USE_TERMIO
	tc.t_intrc = -1;
	tc.t_quitc = -1;
	tc.t_eofc = -1;
	ltc.t_suspc = -1;
	ltc.t_dsuspc = -1;
#else
	tmp_tc.c_lflag &= ~ISIG;
#endif
	localchars = 0;
    } else {
#ifdef	USE_TERMIO
	tmp_tc.c_lflag |= ISIG;
#endif
	localchars = 1;
    }

    if (f&MODE_EDIT) {
#ifndef	USE_TERMIO
	sb.sg_flags &= ~CBREAK;
	sb.sg_flags |= CRMOD;
#else
	tmp_tc.c_lflag |= ICANON;
#endif
    } else {
#ifndef	USE_TERMIO
	sb.sg_flags |= CBREAK;
	if (f&MODE_ECHO)
	    sb.sg_flags |= CRMOD;
	else
	    sb.sg_flags &= ~CRMOD;
#else
	tmp_tc.c_lflag &= ~ICANON;
	tmp_tc.c_iflag &= ~ICRNL;
	tmp_tc.c_cc[VMIN] = 1;
	tmp_tc.c_cc[VTIME] = 0;
#endif
    }

    if ((f&(MODE_EDIT|MODE_TRAPSIG)) == 0) {
#ifndef	USE_TERMIO
	ltc.t_lnextc = -1;
#else
# ifdef VLNEXT
	tmp_tc.c_cc[VLNEXT] = (cc_t)(-1);
# endif
#endif
    }

    if (f&MODE_SOFT_TAB) {
#ifndef USE_TERMIO
	sb.sg_flags |= XTABS;
#else
# ifdef	OXTABS
	tmp_tc.c_oflag |= OXTABS;
# endif
# ifdef	TABDLY
	tmp_tc.c_oflag &= ~TABDLY;
	tmp_tc.c_oflag |= TAB3;
# endif
#endif
    } else {
#ifndef USE_TERMIO
	sb.sg_flags &= ~XTABS;
#else
# ifdef	OXTABS
	tmp_tc.c_oflag &= ~OXTABS;
# endif
# ifdef	TABDLY
	tmp_tc.c_oflag &= ~TABDLY;
# endif
#endif
    }

    if (f&MODE_LIT_ECHO) {
#ifndef USE_TERMIO
	sb.sg_flags &= ~CTLECH;
#else
# ifdef	ECHOCTL
	tmp_tc.c_lflag &= ~ECHOCTL;
# endif
#endif
    } else {
#ifndef USE_TERMIO
	sb.sg_flags |= CTLECH;
#else
# ifdef	ECHOCTL
	tmp_tc.c_lflag |= ECHOCTL;
# endif
#endif
    }

    if (f == -1) {
	onoff = 0;
    } else {
#ifndef	USE_TERMIO
	if (f & MODE_OUTBIN)
		lmode |= LLITOUT;
	else
		lmode &= ~LLITOUT;

	if (f & MODE_INBIN)
		lmode |= LPASS8;
	else
		lmode &= ~LPASS8;
#else
	if (f & MODE_OUTBIN)
		tmp_tc.c_lflag &= ~ISTRIP;
	else
		tmp_tc.c_lflag |= ISTRIP;
	if (f & MODE_INBIN) {
		tmp_tc.c_cflag &= ~(CSIZE|PARENB);
		tmp_tc.c_cflag |= CS8;
		tmp_tc.c_cflag &= ~OPOST;
	} else {
		tmp_tc.c_cflag &= ~CSIZE;
		tmp_tc.c_cflag |= CS7|PARENB;
		tmp_tc.c_cflag |= OPOST;
	}
#endif
	onoff = 1;
    }

    if (f != -1) {
#ifdef	SIGTSTP
	static void susp();

	(void) signal(SIGTSTP, (SIG_FUNC_RET (*)())susp);
#endif	/* SIGTSTP */
	/*
	 * We don't want to process ^Y here.  It's just another
	 * character that we'll pass on to the back end.  It has
	 * to process it because it will be processed when the
	 * user attempts to read it, not when we send it.
	 */
#ifndef	USE_TERMIO
	ltc.t_dsuspc = -1;
#else
# ifdef	VDSUSP
	tmp_tc.c_cc[VDSUSP] = (cc_t)(-1);
# endif
#endif
#ifdef USE_TERMIO
/*
 * Disable VSTATUS (^T)
 */
# ifdef VSTATUS
	tmp_tc.c_cc[VSTATUS] = (cc_t)(-1);
# endif
#endif
#ifdef	USE_TERMIO
	/*
	 * If the VEOL character is already set, then use VEOL2,
	 * otherwise use VEOL.
	 */
	if (tmp_tc.c_cc[VEOL] == (cc_t)(-1))
		tmp_tc.c_cc[VEOL] = escape;
# ifdef	VEOL2
	else if (tmp_tc.c_cc[VEOL2] == (cc_t)(-1))
		tmp_tc.c_cc[VEOL2] = escape;
# endif
#else
	if (tc.t_brkc == (cc_t)(-1))
		tc.t_brkc = escape;
#endif
    } else {
#ifdef	SIGTSTP
	(void) signal(SIGTSTP, SIG_DFL);
	(void) sigsetmask(sigblock(0) & ~(1<<(SIGTSTP-1)));
#endif	/* SIGTSTP */
#ifndef USE_TERMIO
	ltc = oltc;
	tc = otc;
	sb = ottyb;
	lmode = olmode;
#else
	tmp_tc = old_tc;
#endif
    }
#ifndef USE_TERMIO
    ioctl(tin, TIOCLSET, (char *)&lmode);
    ioctl(tin, TIOCSLTC, (char *)&ltc);
    ioctl(tin, TIOCSETC, (char *)&tc);
    ioctl(tin, TIOCSETP, (char *)&sb);
#else
    if (ioctl(tin, TCSETAW, &tmp_tc) < 0)
	ioctl(tin, TCSETA, &tmp_tc);
#endif

#if	(!defined(TN3270)) || ((!defined(NOT43)) || defined(PUTCHAR))
    ioctl(tin, FIONBIO, (char *)&onoff);
    ioctl(tout, FIONBIO, (char *)&onoff);
#endif	/* (!defined(TN3270)) || ((!defined(NOT43)) || defined(PUTCHAR)) */
#if	defined(TN3270)
    if (noasynchtty == 0) {
	ioctl(tin, FIOASYNC, (char *)&onoff);
    }
#endif	/* defined(TN3270) */
}

#ifndef	B19200
# define B19200 B9600
#endif

#ifndef	B38400
# define B38400 B19200
#endif

/*
 * This code assumes that the values B0, B50, B75...
 * are in ascending order.  They do not have to be
 * contiguous.
 */
struct termspeeds {
	long speed;
	long value;
} termspeeds[] = {
	{ 0,     B0 },     { 50,    B50 },   { 75,    B75 },
	{ 110,   B110 },   { 134,   B134 },  { 150,   B150 },
	{ 200,   B200 },   { 300,   B300 },  { 600,   B600 },
	{ 1200,  B1200 },  { 1800,  B1800 }, { 2400,  B2400 },
	{ 4800,  B4800 },  { 9600,  B9600 }, { 19200, B19200 },
	{ 38400, B38400 }, { -1,    B38400 }
};

#ifndef	USE_TERMIO
# define	ISPEED	ottyb.sg_ispeed
# define	OSPEED	ottyb.sg_ospeed
#else
# ifdef	CBAUD
#  define	ISPEED	(old_tc.c_cflag&CBAUD)
#  define	OSPEED	ISPEED
# else
#  define	ISPEED	old_tc.c_ispeed
#  define OSPEED	old_tc.c_ospeed
# endif
#endif

void
TerminalSpeeds(ispeed, ospeed)
long *ispeed;
long *ospeed;
{
    register struct termspeeds *tp;

    tp = termspeeds;
    while ((tp->speed != -1) && (tp->value < ISPEED))
	tp++;
    *ispeed = tp->speed;

    tp = termspeeds;
    while ((tp->speed != -1) && (tp->value < OSPEED))
	tp++;
    *ospeed = tp->speed;
}

int
TerminalWindowSize(rows, cols)
long *rows, *cols;
{
#ifdef	TIOCGWINSZ
    struct winsize ws;

    if (ioctl(fileno(stdin), TIOCGWINSZ, (char *)&ws) >= 0) {
	*rows = ws.ws_row;
	*cols = ws.ws_col;
	return 1;
    }
#endif	/* TIOCGWINSZ */
    return 0;
}

int
NetClose(fd)
int	fd;
{
    return close(fd);
}


void
NetNonblockingIO(fd, onoff)
int
	fd,
	onoff;
{
    ioctl(fd, FIONBIO, (char *)&onoff);
}

#if	defined(TN3270)
void
NetSigIO(fd, onoff)
int
	fd,
	onoff;
{
    ioctl(fd, FIOASYNC, (char *)&onoff);	/* hear about input */
}

void
NetSetPgrp(fd)
int fd;
{
    int myPid;

    myPid = getpid();
    fcntl(fd, F_SETOWN, myPid);
}
#endif	/*defined(TN3270)*/

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
#ifdef	KLUDGELINEMODE
	if (kludgelinemode)
	    sendbrk();
	else
#endif
	    sendabort();
	return;
    }
}

static void
susp()
{
    if (localchars)
	sendsusp();
}

static void
sendwin()
{
    if (connected) {
	sendnaws();
    }
}


void
sys_telnet_init()
{
    (void) signal(SIGINT, (SIG_FUNC_RET (*)())intr);
    (void) signal(SIGQUIT, (SIG_FUNC_RET (*)())intr2);
    (void) signal(SIGPIPE, (SIG_FUNC_RET (*)())deadpeer);
#ifdef	SIGWINCH
    (void) signal(SIGWINCH, (SIG_FUNC_RET (*)())sendwin);
#endif
#ifdef	SIGTSTP
    (void) signal(SIGTSTP, (SIG_FUNC_RET (*)())susp);
#endif

    setconnmode(0);

    NetNonblockingIO(net, 1);

#if	defined(TN3270)
    if (noasynchnet == 0) {			/* DBX can't handle! */
	NetSigIO(net, 1);
	NetSetPgrp(net);
    }
#endif	/* defined(TN3270) */

#if	defined(SO_OOBINLINE)
    if (SetSockOpt(net, SOL_SOCKET, SO_OOBINLINE, 1) == -1) {
	perror("SetSockOpt");
    }
#endif	/* defined(SO_OOBINLINE) */
}

/*
 * Process rings -
 *
 *	This routine tries to fill up/empty our various rings.
 *
 *	The parameter specifies whether this is a poll operation,
 *	or a block-until-something-happens operation.
 *
 *	The return value is 1 if something happened, 0 if not.
 */

int
process_rings(netin, netout, netex, ttyin, ttyout, poll)
int poll;		/* If 0, then block until something to do */
{
    register int c;
		/* One wants to be a bit careful about setting returnValue
		 * to one, since a one implies we did some useful work,
		 * and therefore probably won't be called to block next
		 * time (TN3270 mode only).
		 */
    int returnValue = 0;
    static struct timeval TimeValue = { 0 };

    if (netout) {
	FD_SET(net, &obits);
    } 
    if (ttyout) {
	FD_SET(tout, &obits);
    }
#if	defined(TN3270)
    if (ttyin) {
	FD_SET(tin, &ibits);
    }
#else	/* defined(TN3270) */
    if (ttyin) {
	FD_SET(tin, &ibits);
    }
#endif	/* defined(TN3270) */
#if	defined(TN3270)
    if (netin) {
	FD_SET(net, &ibits);
    }
#   else /* !defined(TN3270) */
    if (netin) {
	FD_SET(net, &ibits);
    }
#   endif /* !defined(TN3270) */
    if (netex) {
	FD_SET(net, &xbits);
    }
    if ((c = select(16, &ibits, &obits, &xbits,
			(poll == 0)? (struct timeval *)0 : &TimeValue)) < 0) {
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
	    sleep(5);
	}
	return 0;
    }

    /*
     * Any urgent data?
     */
    if (FD_ISSET(net, &xbits)) {
	FD_CLR(net, &xbits);
	SYNCHing = 1;
	(void) ttyflush(1);	/* flush already enqueued data */
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
	    static int bogus_oob = 0, first = 1;

	    ioctl(net, SIOCATMARK, (char *)&atmark);
	    if (atmark) {
		c = recv(net, netiring.supply, canread, MSG_OOB);
		if ((c == -1) && (errno == EINVAL)) {
		    c = recv(net, netiring.supply, canread, 0);
		    if (clocks.didnetreceive < clocks.gotDM) {
			SYNCHing = stilloob(net);
		    }
		} else if (first && c > 0) {
		    /*
		     * Bogosity check.  Systems based on 4.2BSD
		     * do not return an error if you do a second
		     * recv(MSG_OOB).  So, we do one.  If it
		     * succeeds and returns exactly the same
		     * data, then assume that we are running
		     * on a broken system and set the bogus_oob
		     * flag.  (If the data was different, then
		     * we probably got some valid new data, so
		     * increment the count...)
		     */
		    int i;
		    i = recv(net, netiring.supply + c, canread - c, MSG_OOB);
		    if (i == c &&
			  bcmp(netiring.supply, netiring.supply + c, i) == 0) {
			bogus_oob = 1;
			first = 0;
		    } else if (i < 0) {
			bogus_oob = 0;
			first = 0;
		    } else
			c += i;
		}
		if (bogus_oob && c > 0) {
		    int i;
		    /*
		     * Bogosity.  We have to do the read
		     * to clear the atmark to get out of
		     * an infinate loop.
		     */
		    i = read(net, netiring.supply + c, canread - c);
		    if (i > 0)
			c += i;
		}
	    } else {
		c = recv(net, netiring.supply, canread, 0);
	    }
	} else {
	    c = recv(net, netiring.supply, canread, 0);
	}
	settimer(didnetreceive);
#else	/* !defined(SO_OOBINLINE) */
	c = recv(net, netiring.supply, canread, 0);
#endif	/* !defined(SO_OOBINLINE) */
	if (c < 0 && errno == EWOULDBLOCK) {
	    c = 0;
	} else if (c <= 0) {
	    return -1;
	}
	if (netdata) {
	    Dump('<', netiring.supply, c);
	}
	if (c)
	    ring_supplied(&netiring, c);
	returnValue = 1;
    }

    /*
     * Something to read from the tty...
     */
    if (FD_ISSET(tin, &ibits)) {
	FD_CLR(tin, &ibits);
	c = TerminalRead(ttyiring.supply, ring_empty_consecutive(&ttyiring));
	if (c < 0 && errno == EWOULDBLOCK) {
	    c = 0;
	} else {
	    /* EOF detection for line mode!!!! */
	    if ((c == 0) && MODE_LOCAL_CHARS(globalmode) && isatty(tin)) {
			/* must be an EOF... */
		*ttyiring.supply = termEofChar;
		c = 1;
	    }
	    if (c <= 0) {
		return -1;
	    }
	    if (termdata) {
		Dump('<', ttyiring.supply, c);
	    }
	    ring_supplied(&ttyiring, c);
	}
	returnValue = 1;		/* did something useful */
    }

    if (FD_ISSET(net, &obits)) {
	FD_CLR(net, &obits);
	returnValue |= netflush();
    }
    if (FD_ISSET(tout, &obits)) {
	FD_CLR(tout, &obits);
	returnValue |= (ttyflush(SYNCHing|flushout) > 0);
    }

    return returnValue;
}
