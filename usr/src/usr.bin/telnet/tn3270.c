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
    int tmp = flushout;

    flushout = 1;

    ttyflush();

    flushout = tmp;
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

#endif	/* defined(TN3270) */
