/*
 * The following routines try to encapsulate what is system dependent
 * (at least between 4.x and dos) which is used in telnet.c.
 */

#if	defined(unix)

#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <signal.h>
#include <errno.h>

#include "ring.h"

#include "fdset.h"

#include "defines.h"
#include "externs.h"
#include "types.h"

int
	tout,			/* Output file descriptor */
	tin,			/* Input file descriptor */
	net,
	HaveInput;		/* There is input available to scan */

#if	defined(TN3270)
static char	tline[200];
char	*transcom = 0;	/* transparent mode command (default: none) */
#endif	/* defined(TN3270) */

static struct	tchars otc = { 0 }, ntc = { 0 };
static struct	ltchars oltc = { 0 }, nltc = { 0 };
static struct	sgttyb ottyb = { 0 }, nttyb = { 0 };

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


TerminalWrite(fd, buf, n)
int	fd;
char	*buf;
int	n;
{
    return write(fd, buf, n);
}

TerminalRead(fd, buf, n)
int	fd;
char	*buf;
int	n;
{
    return read(fd, buf, n);
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

    if (c == ntc.t_intrc) {
	intp();
	return 0;
    } else if (c == ntc.t_quitc) {
	sendbrk();
	return 0;
    } else if (c == nltc.t_flushc) {
	xmitAO();		/* Transmit Abort Output */
	return 0;
    } else if (!MODE_LOCAL_CHARS(globalmode)) {
	if (c == nttyb.sg_kill) {
	    xmitEL();
	    return 0;
	} else if (c == nttyb.sg_erase) {
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
    (void) ioctl(fileno(stdout), TIOCFLUSH, (char *) 0);
}

void
TerminalSaveState()
{
    ioctl(0, TIOCGETP, (char *)&ottyb);
    ioctl(0, TIOCGETC, (char *)&otc);
    ioctl(0, TIOCGLTC, (char *)&oltc);

    ntc = otc;
    nltc = oltc;
    nttyb = ottyb;

    termEofChar = ntc.t_eofc;
    termEraseChar = nttyb.sg_erase;
    termFlushChar = nltc.t_flushc;
    termIntChar = ntc.t_intrc;
    termKillChar = nttyb.sg_kill;
    termQuitChar = ntc.t_quitc;
}

void
TerminalRestoreState()
{
}

/*
 * TerminalNewMode - set up terminal to a specific mode.
 */


void
TerminalNewMode(fd_in, fd_out, f)
int	fd_in, fd_out;		/* File descriptor */
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
    ioctl(fd_in, TIOCSLTC, (char *)ltc);
    ioctl(fd_in, TIOCSETC, (char *)tc);
    ioctl(fd_in, TIOCSETP, (char *)&sb);
#if	(!defined(TN3270)) || ((!defined(NOT43)) || defined(PUTCHAR))
    ioctl(fd_in, FIONBIO, (char *)&onoff);
    ioctl(fd_out, FIONBIO, (char *)&onoff);
#endif	/* (!defined(TN3270)) || ((!defined(NOT43)) || defined(PUTCHAR)) */
#if	defined(TN3270)
    if (noasynch == 0) {
	ioctl(fd_in, FIOASYNC, (char *)&onoff);
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


void
NetNonblockingIO(fd, onoff)
int
	fd,
	onoff;
{
    ioctl(fd, FIONBIO, (char *)&onoff);
}

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
#if	defined(NOT43)
    myPid = -myPid;
#endif	/* defined(NOT43) */
    ioctl(fd, SIOCSPGRP, (char *)&myPid);	/* set my pid */
}

/*
 * Various signal handling routines.
 */

void
deadpeer()
{
	setcommandmode();
	longjmp(peerdied, -1);
}

void
intr()
{
    if (localchars) {
	intp();
	return;
    }
    setcommandmode();
    longjmp(toplevel, -1);
}

void
intr2()
{
    if (localchars) {
	sendbrk();
	return;
    }
}

void
doescape()
{
    command(0);
}

void
sys_telnet_init()
{
#if	defined(TN3270)
    int myPid;
#endif	/* defined(TN3270) */

    signal(SIGINT, intr);
    signal(SIGQUIT, intr2);
    signal(SIGPIPE, deadpeer);

    setconnmode();

    NetNonblockingIO(net, 1);

#if	defined(TN3270)
    if (noasynch == 0) {			/* DBX can't handle! */
	NetSigIO(net, 1);
	NetSetPgrp(net);
    }
#endif	/* defined(TN3270) */

#if	defined(SO_OOBINLINE)
    SetSockOpt(net, SOL_SOCKET, SO_OOBINLINE, 1);
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
		c = recv(net, netiring.supply, canread, MSG_OOB);
		if ((c == -1) && (errno == EINVAL)) {
		    c = recv(net, netiring.supply, canread, 0);
		    if (clocks.didnetreceive < clocks.gotDM) {
			SYNCHing = stilloob(net);
		    }
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
	c = TerminalRead(tin, ttyiring.supply,
			ring_empty_consecutive(&ttyiring));
	if (c < 0 && errno == EWOULDBLOCK) {
	    c = 0;
	} else {
	    /* EOF detection for line mode!!!! */
	    if (c == 0 && MODE_LOCAL_CHARS(globalmode)) {
			/* must be an EOF... */
		*ttyiring.supply = termEofChar;
		c = 1;
	    }
	    if (c <= 0) {
		return -1;
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
	returnValue |= ttyflush(SYNCHing|flushout);
    }

    return returnValue;
}
#endif	/* defined(unix) */
