/*
 * The following routines try to encapsulate what is system dependent
 * (at least between 4.x and dos) which is used in telnet.c.
 */

#if	defined(unix)

#include <sys/ioctl.h>
#include <sys/time.h>
#include <signal.h>

#include "defines.h"
#include "externs.h"
#include "types.h"

int
	HaveInput;		/* There is input available to scan */

#if	defined(TN3270)
static char	tline[200];
char	*transcom = 0;	/* transparent mode command (default: none) */
#endif	/* defined(TN3270) */

static struct	tchars otc = { 0 }, ntc = { 0 };
static struct	ltchars oltc = { 0 }, nltc = { 0 };
static struct	sgttyb ottyb = { 0 }, nttyb = { 0 };


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
TerminalAutoFlush()					/* unix */
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
TerminalFlushOutput()				/* unix */
{
    (void) ioctl(fileno(stdout), TIOCFLUSH, (char *) 0);
}

void
TerminalSaveState()				/* unix */
{
    ioctl(0, TIOCGETP, (char *)&ottyb);
    ioctl(0, TIOCGETC, (char *)&otc);
    ioctl(0, TIOCGLTC, (char *)&oltc);

    ntc = otc;
    nltc = oltc;
    nttyb = ottyb;
}

void
TerminalRestoreState()				/* unix */
{
}

/*
 * TerminalNewMode - set up terminal to a specific mode.
 */


void
TerminalNewMode(fd_in, fd_out, f)			/* unix */
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
NetNonblockingIO(fd, onoff)				/* unix */
int
	fd,
	onoff;
{
    ioctl(fd, FIONBIO, (char *)&onoff);
}

void
NetSigIO(fd, onoff)				/* unix */
int
	fd,
	onoff;
{
    ioctl(fd, FIOASYNC, (char *)&onoff);	/* hear about input */
}

void
NetSetPgrp(fd)				/* unix */
int fd;
{
    int myPid;

    myPid = getpid();
#if	defined(NOT43)
    myPid = -myPid;
#endif	/* defined(NOT43) */
    ioctl(fd, SIOCSPGRP, (char *)&myPid);	/* set my pid */
}


#endif	/* defined(unix) */
