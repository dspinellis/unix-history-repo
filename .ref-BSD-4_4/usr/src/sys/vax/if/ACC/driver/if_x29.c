/*
 * 
 *     X.29 option for dda driver for UNIX and Ultrix
 *      ________________________________________________________
 *     /                                                        \
 *    |          AAA          CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |
 *    |         AAAAA        CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |
 *    |        AAAAAAA       CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |
 *    |       AAAA AAAA      CCCC              CCCC              |
 *    |      AAAA   AAAA     CCCC              CCCC              |
 *    |     AAAA     AAAA    CCCC              CCCC              |
 *    |    AAAA       AAAA   CCCC              CCCC              |
 *    |   AAAA  AAAAAAAAAAA  CCCCCCCCCCCCCCCCC CCCCCCCCCCCCCCCCC |
 *    |  AAAA    AAAAAAAAAAA CCCCCCCCCCCCCCCC  CCCCCCCCCCCCCCCC  |
 *    | AAAA      AAAAAAAAA   CCCCCCCCCCCCCC    CCCCCCCCCCCCCC   |
 *     \________________________________________________________/
 * 
 *      Copyright (c) 1987 by Advanced Computer Communications
 *      720 Santa Barbara Street, Santa Barbara, California  93101
 *      (805) 963-9431
 * 
 * File:
 *      if_x29.c
 * 
 * Author:
 * 
 * Project:
 *      Development of PAD on 6250 software.
 * 
 * Function:
 *      To enable network connections on ACP_XX to communicate with UNIX.
 * 
 * Components:
 *      - files if_x29.c
 * 
 * Configuration Entry:
 *
 *      device dda0 at uba? csr 0166740 vector ddainta ddaintb
 *
 * Usage Notes:
 *
 *      - make devices in /dev and edit /etc/ttys for those x29
 *        devices which you want in your configuration
 * 
 * System Notes:
 * 
 *       Refer to the installation instructions, readme.txt, which
 *       are included on the driver distribution medium.
 *
 * Revision History at end of file
 */

/*
 *	For efficiency, it is a good idea to modify XXBOARDS when using
 *	less than 4 boards with the X29 option.  If using more than 32
 *	lines per board, you should modify XXBOARDS, XXLPERBRD, LOG2_XXBOARDS
 *	and LOG2_XXLPERBRD.
 *
 *	Minor numbers are laid out as follows (by default):
 *		(MSB) PBBLLLLL (LSB)
 *	Where P is a flag to determine if the line is outbound (pad) or
 *	inbound (tty).  BB is the board number (0-3), and LLLLL is the
 *	X29 line on a board (0-31).  Some customers may need more than
 *	32 lines/board.  If there are less than 2 boards,  one may shift
 *	the break-point between lines and boards:
 *
 *	up to 4 boards, 32 lines/board	(default)
 *		(MSB) PBBLLLLL (LSB)
 *			XXBOARDS  = 4,   LOG2_XXBOARDS  = 2
 *			XXLPERBRD = 32,	 LOG2_XXLPERBRD = 5
 *	up to 2 boards, 64 lines/board:
 *		(MSB) PBLLLLLL (LSB)
 *			XXBOARDS  = 2,   LOG2_XXBOARDS  = 1
 *			XXLPERBRD = 64,  LOG2_XXLPERBRD = 6
 *	only 1 board, 128 (actually, 126, as 126 = max svc):
 *		(MSB) PLLLLLLL (LSB)
 *			XXBOARDS  = 1,   LOG2_XXBOARDS  = 0
 *			XXLPERBRD = 128, LOG2_XXLPERBRD = 7
 *
 *	(obviously, these are all powers of two)
 */

#define	XXBOARDS	4	/* # boards running x29 */
#define	LOG2_XXBOARDS	2	/* # bits of board info */

#define XXLPERBRD	32	/* # lines per board */
#define	LOG2_XXLPERBRD	5	/* # bits of line info */

/*
 * If you require an 8-bit data path and have no parity misconfigurations,
 * you may change PARITY_MASKs to 0377.  This will leave parity stripping
 * to the ttdriver.  However,  the ttdriver won't strip parity when in
 * raw mode (e.g. at the Password: prompt),  so one symptom of a parity
 * misconfiguration is that users can't login (CR gets received as 0x8D).
 */

#define	INPUT_PARITY_MASK  0177	/* strip off the 8th bit */
#define	OUTPUT_PARITY_MASK 0377	/* don't strip off the 8th bit */

/*
 * macro to translate a device number to the unit (i.e. ACP_n250)
 * with which it is associated and the port on said unit
 */

#define UNIT(x) ((minor(x) >> LOG2_XXLPERBRD) & LOG2_XXBOARDS)

#define LINE(x)	  (minor(x) & 0177)	/* index into line table */
#define XXSHOW(x) (minor(x) == 255)	/* special "show" device */
#define IS_PAD(x) (minor(x) & 0200)	/* msb is the pad/tty selector */
#define MAJLINE(x) ((x) & ~0x80)	/* major plus corrected minor # */

#define NXXLINES        (XXBOARDS * XXLPERBRD)	/* number of total x29 lines */

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   LOCAL  FUNCTIONS                          %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void    xxcntl();
PRIVATE void    xxclear();
PRIVATE void    xxshow();
PRIVATE void    xxpadhandle();
PRIVATE int     xxpadparse();
PRIVATE int     xxpadcall();
PRIVATE void    xxpadmsg();
PRIVATE void	xx_qbit_msg();
PRIVATE void    xx_tp_hangup();
PRIVATE void    x29_init();
PRIVATE void    x29_dhandle();
PRIVATE int	x29_break_reply_is_required();

#if ACC_ULTRIX >= 30
static  int	ttbreakc();		/* always keep this private */
#endif

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   LOCAL  VARIABLES                          %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#define SET_PAD		2
#define READ_PAD	4
#define SET_READ_PAD	6
#define PAR_INDICATION	0
#define INVITE_CLEAR	1
#define BREAK_INDIC	3
#define PAD_ERROR	5

/* command codes */
#define XX_C_BREAK	001
#define XX_C_PAD	002
#define XX_C_CLOSE	003
#define XX_C_HOST	004

struct tty      xx_tty[NXXLINES];	/* tty structures */

#define MODE_UNUSED 0			/* !just for sanity checks only! */
#define	MODE_HOST 1			/* port in host mode (incoming) */
#define	MODE_PAD  2			/* port in pad mode (outgoing) */

char            xxmode[NXXLINES];	/* mode of port */

int             xxstart();

typedef struct {
    char            ref;
    char            val;
} x29_pad_pair;

PRIVATE x29_pad_pair x29_break_ack_params[] =
{
 8, 0				/* ref 8 -- normal output to terminal */
};

PRIVATE x29_pad_pair x29_callout_params[] =
{
 1, 0				/* ref 1 -- no recall char */
};

PRIVATE x29_pad_pair x29_callin_setparams[] =
{ /* these are the preferred paramters when calling in to Unix */
 2, 0,				/* ref 2 -- no echo */
 3, 127,			/* ref 3 -- forward data on any char */
 8, 0,				/* ref 8 -- normal data delivery to terminal */
 9, 0,				/* ref 9 -- no padding after carriage return */
 10, 0,				/* ref 10 -- no line folding */
 13, 0,				/* ref 13 -- no line feed after CR */
 15, 0				/* ref 15 -- no local edit */
};

/******************************************************************************
 *  PAD CONTROL INFORMATION AND DEFINITIONS
 ******************************************************************************/

/* definitions for the pad state field p_state */
#define PS_IDLE	0		/* not opened state */
#define PS_COM  1		/* the pad for this line is in command state */
#define PS_PAD  2		/* this line has data passing though the pad */
#define PS_WAIT	3		/* waiting state */
#define PS_XFR	4		/* data transfer state */

#define P_LINELEN	20
#define P_NOBLOCK	0

typedef struct padinfo {
    short           p_state;	/* pad state */
    char            p_line[P_LINELEN];	/* built up line */
    char            p_idx;	/* index into p_line */
    int		    p_flow;	/* index into mbuf when flow off,
				   P_NOBLOCK if not flowed off */
    struct mbuf    *p_msav;	/* place to hang mbuf when flow controlled */
    struct mbuf    *p_mchsav;	/* place to save mbuf chain '' '' '' */
} padinfo;
padinfo         xx_padinfo[NXXLINES];


/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   GLOBAL ROUTINES                           %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       XXOPEN()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Open a line.                                                   */
/*                                                                 */
/*  Call:           xxopen(dev, flag)                              */
/*  Argument:       dev:   device                                  */
/*                  flag:  indicates type of open, "nonblocking"   */
/*                         "or block if in use"                    */
/*  Returns:        0 for success, else nonzero error code         */
/*  Called by:      kernel software software,  this routine is in  */
/*                  the cdevsw table                               */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*ARGSUSED*/
xxopen(dev, flag)
dev_t           dev;
int             flag;
{
    register struct tty *tp;
    register        d;
    register        s;
    int             unit,
                    i;
#if ACC_ULTRIX > 00
    int             inuse;	/* store inuse bit while sleeping */
#endif

    unit = UNIT(dev);
    d = LINE(dev);

    if (XXSHOW(dev)) {		/* minor device 255 */
	xxshow();
	return (EPIPE);
    }

    /* PST NOTE TO SELF: change the test as follows:
     *	make this d >= NXXLINES, then check to see if unit is present,
     *  Keep that sleep() in the thingy below, so we don't get bouncing
     *  gettys eating up cpu time.
     */
    if ((d >= NXXLINES))
	return (ENXIO);

    /* wait for interface to come up */
    while (dda_softc[unit].dda_state != S_LINK_UP)
	sleep(&dda_softc[unit].dda_state, TTIPRI);

    tp = &xx_tty[d];
    if ((tp->t_state & TS_XCLUDE) && u.u_uid != 0)
	return EBUSY;

    /* make sure the port isn't already open in a conflicting manner */
    /* i.e. can't open /dev/padJ0 and /dev/ttyJ0 at the same time */
    if (tp->t_state & (TS_WOPEN | TS_ISOPEN)) {
	if ((IS_PAD(dev) && (xxmode[d] == MODE_HOST)) ||
	    ((!IS_PAD(dev)) && (xxmode[d] == MODE_PAD)))
	    return EBUSY;
    }

#ifdef	DDADEBUG
	if (DDADBCH(96, unit)) {
		DDALOG(LOG_DEBUG)
		    "dda%d:(x29) open line %d flag %o in %s mode\n",
		    unit, d, flag, (IS_PAD(dev) ? "pad" : "host")
		DDAELOG;
	}
#endif  DDADEBUG

    tp->t_oproc = xxstart;
    tp->t_state |= TS_WOPEN;

    /* if first open initialize state */
    if ((tp->t_state & TS_ISOPEN) == 0) {
	ttychars(tp);

#if ACC_ULTRIX >= 30		/* posix compliant tty driver */
	if (tp->t_cflag & CBAUD == 0) {
	    tp->t_iflag = IGNPAR | ICRNL | IXON | IXANY | IXOFF;
	    tp->t_oflag = OPOST | ONLCR;
	    tp->t_cflag = B9600 | CS8 | CREAD | HUPCL;
	    tp->t_lflag = ISIG | ICANON | ECHO | ECHOE | ECHOK | ECHONL;
	    tp->t_line = 0;
	}
#else				/* v7 tty driver */
	if (tp->t_ispeed == 0) {
	    tp->t_ispeed = B9600;
	    tp->t_ospeed = B9600;
	    tp->t_flags = CRMOD | ANYP;
	}
#endif
	xxparam(dev);
    }
    if (IS_PAD(dev)) {
	tp->t_state |= TS_CARR_ON;
	xxmode[d] = MODE_PAD;
	xxcntl(tp, XX_C_PAD, unit);
    } else {
	if ((tp->t_state & TS_CARR_ON) == 0) {
	    xxmode[d] = MODE_HOST;
	    xxcntl(tp, XX_C_HOST, unit);
	    tp->t_flags |= ECHO;
#if ACC_ULTRIX < 31	/* on everything other than Ultrix 3.1 */
	    /* on close tell ACP_XX to drop line */
	    tp->t_state |= TS_HUPCLS;
#endif
	}
    }
    /* if xxcntl did not get called (state had carrier off) or xxcntl's
     * search for a free lcn failed, then t_addr will be 0, so punt */
    if (tp->t_addr == 0) {
	tp->t_pgrp = 0;
	tp->t_state = 0;
	xxmode[d] = MODE_UNUSED;
	return (EBUSY);
    }
    xx_padinfo[d].p_flow = P_NOBLOCK;
    s = splimp();

#if ACC_ULTRIX > 00
    if (flag & O_NDELAY) {
	if (!IS_PAD(dev))
	    tp->t_state |= TS_ONDELAY;
    } else
#endif
	while ((tp->t_state & TS_CARR_ON) == 0) {
	    tp->t_state |= TS_WOPEN;
#if ACC_ULTRIX > 00
	    inuse = tp->t_state & TS_INUSE;
#endif
	    sleep(&tp->t_rawq, TTIPRI);

	    /* wakeup came from xxclear */
	    if ((tp->t_state & TS_WOPEN) == 0) {
		splx(s);
		return (EPIPE);
	    }
#if ACC_ULTRIX > 00
	    /* if port became "inuse" while we slept, return */
	    if ((flag & O_BLKINUSE) && (!inuse) &&
		(tp->t_state & TS_INUSE)) {
		splx(s);
		return (EALREADY);
	    }
#endif
	}

    splx(s);
    i = ((*linesw[tp->t_line].l_open) (dev, tp));
    if (tp->t_pgrp == 0)
	tp->t_pgrp = u.u_procp->p_pid;
    return (i);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       XXCLOSE()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Close a line.                                                  */
/*                                                                 */
/*  Call:           xxclose(dev, flag)                             */
/*  Argument:       dev:   device                                  */
/*                  flag:  unused                                  */
/*  Returns:        nothing                                        */
/*  Called by:      kernel software,  this routine is in the	   */
/*		    cdevsw table                                   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*ARGSUSED*/
xxclose(dev, flag, mode, p)
dev_t           dev;
int             flag, mode;
struct proc	*p;
{
    register struct tty *tp;
    register        d;
    d = LINE(dev);
    tp = &xx_tty[d];

#ifdef	DDADEBUG
	if (DDADBCH(97, UNIT(dev))) {
		DDALOG(LOG_DEBUG) "dda%d:(x29) closing line %d\n", UNIT(dev), d
		DDAELOG;
	}
#endif  DDADEBUG

	/* PST NOTE TO SELF:
	 *	Add the 629 driver code for timing out the close below,
	 *	because the line could be flowed off and it would hang
	 * 	forever */

    (*linesw[tp->t_line].l_close) (tp, flag);

#if ACC_ULTRIX >= 31
    if ((tp->t_cflag & HUPCL) || ((tp->t_state & TS_ISOPEN) == 0)) {
#else
    if ((tp->t_state & TS_HUPCLS) || ((tp->t_state & TS_ISOPEN) == 0)) {
#endif

#ifdef	DDADEBUG
	if (DDADBCH(97, UNIT(dev))) {
		DDALOG(LOG_DEBUG) "dda%d:(x29) close: tp->t_state = %x\n",
				  UNIT(dev), tp->t_state
		DDAELOG;
	}
#endif  DDADEBUG

	if (tp->t_state & TS_CARR_ON)
	    xxcntl(tp, XX_C_CLOSE, UNIT(dev));
	tp->t_state &= ~TS_CARR_ON;
	xxmode[d] = MODE_UNUSED;
    }
    ttyclose(tp);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       XXREAD()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Read from a line.                                              */
/*                                                                 */
/*  Call:           xxread(dev, uio)                               */
/*  Argument:       dev:   device                                  */
/*                  uio:   pointer to uio structure                */
/*  Returns:        0 for success, else nonzero error code         */
/*  Called by:      kernel software,  this routine is in	   */
/*                  the cdevsw table                               */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

xxread(dev, uio)
dev_t           dev;
struct uio     *uio;
{
    register struct tty *tp;
    register int    l,
                    error;

    if (dda_softc[UNIT(dev)].dda_state != S_LINK_UP)
	return (ENXIO);

    l = LINE(dev);
    tp = &xx_tty[l];
    error = (*linesw[tp->t_line].l_read)(tp, uio);

    if (xx_padinfo[l].p_flow != P_NOBLOCK) {	/* currently blocked? */
	if (tp->t_flags & (RAW | CBREAK)) {	/* using raw q? */
	    if (tp->t_rawq.c_cc < TTYHOG / 8) {	/* if rawq is low, then
						 * it's time to unblock */
		x29_dhandle(&dda_softc[UNIT(dev)],
			    (struct dda_cb *) (tp->t_addr), 1);
	    }
	/* else cooked mode, different test */
	/* canonical q empty? then it's time to unblock */
	} else if (tp->t_canq.c_cc == 0) {
	    x29_dhandle(&dda_softc[UNIT(dev)],
			(struct dda_cb *) (tp->t_addr), 1);
	}
    }
    return (error);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       XXWRITE()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Write on a line.                                               */
/*                                                                 */
/*  Call:           xxwrite(dev, uio)                              */
/*  Argument:       dev:   device                                  */
/*                  uio:   pointer to uio structure                */
/*  Returns:        0 for success, else nonzero error code         */
/*  Called by:      kernel software software,  this routine is in  */
/*                  the cdevsw table                               */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

xxwrite(dev, uio)
dev_t           dev;
struct uio     *uio;
{
    register struct tty *tp;
    if (dda_softc[UNIT(dev)].dda_state != S_LINK_UP)
	return (ENXIO);
    tp = &xx_tty[LINE(dev)];
    return (*linesw[tp->t_line].l_write)(tp, uio);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                       XXIOCTL()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Process ioctl request.                                         */
/*                                                                 */
/*  Call:           xxioctl(dev, cmd, data, flag)                  */
/*  Argument:       dev:   device                                  */
/*                  cmd:   ioctl command                           */
/*                  data:  pointer to data                         */
/*                  flag:  ignored                                 */
/*  Returns:        0 for sucess, else nonzero error code          */
/*  Called by:      kernel software software,  this routine is in  */
/*                  the cdevsw table                               */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#define TIOACCQBIT (int)(0x80800000|('t'<<8)|125)

xxioctl(dev, cmd, data, flag)
dev_t           dev;
caddr_t         data;
{
    register struct tty *tp;
    int             error;
    tp = &xx_tty[LINE(dev)];
    if (cmd == TIOACCQBIT) {
#ifdef	DDADEBUG
	if (DDADBCH(98, UNIT(dev))) {
		DDALOG(LOG_DEBUG) "dda%d:(x29) ioctl qbit msg: cmd=%x ACC=%x\n",
				  UNIT(dev), cmd, TIOACCQBIT
		DDAELOG;
	}
#endif  DDADEBUG
	xx_qbit_msg(tp, UNIT(dev), data);
	return (0);
    }
    error = (*linesw[tp->t_line].l_ioctl) (tp, cmd, data, flag);
    if (error >= 0)
	return (error);
    error = ttioctl(tp, cmd, data, flag);
    if (error >= 0) {
	if (cmd == TIOCSETP || cmd == TIOCSETN)
	    xxparam(dev);
	return (error);
    }
    switch (cmd) {
    case TIOCREMOTE:
	if (xxmode[LINE(dev)] == 0)
	    return (EBUSY);
	xxcntl(tp, XX_C_PAD, UNIT(dev));
	break;
    case TIOCSBRK:
	xxcntl(tp, XX_C_BREAK, UNIT(dev));
	break;
    case TIOCCBRK:
    case TIOCSDTR:
    case TIOCCDTR:
	break;
    default:
	return (ENOTTY);
    }
    return (0);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                        XXPARAM()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Set parameters from open or stty.                              */
/*  This routine is being left in as a dummy in case in the future */
/*  there is a mechanism for the host to send information i.e.     */
/*  "hangup line" to the ACP _XX                                   */
/*                                                                 */
/*  Call:           xxparam(dev)                                   */
/*  Argument:       dev:   device                                  */
/*  Returns:        none                                           */
/*  Called by:      none                                           */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*ARGSUSED*/
xxparam(dev)
dev_t           dev;
{
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                        XXSTART()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Start (restart) transmission on a given line.  This is the     */
/*  start routine which is called from above by the tty driver and */
/*  from below on a transmission complete interrupt for a given    */
/*  line.                                                          */
/*                                                                 */
/*  Call:           xxstart(tp)                                    */
/*  Argument:       tp:   pointer to tty structure                 */
/*  Returns:        none                                           */
/*  Called by:      tty driver                                     */
/*                  xxreset()                                      */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

xxstart(tp)
register struct tty *tp;
{
    register struct dda_softc *ds;
    register int    nch,
                    cc,
		    k;
    register struct dda_cb *dc;
    register char  *cp,
                   *p;
    struct ifqueue *oq;
    struct mbuf    *m;
    padinfo	   *pp;
    int             unit,
                    line,
                    s,
		    j;
    extern int      ttrstrt();

    line = tp - xx_tty;
    unit = UNIT(line);
    dc = (struct dda_cb *) tp->t_addr;
    ds = &dda_softc[unit];
    pp = &xx_padinfo[line];

    s = splimp();

#ifdef	DDADEBUG
    if (DDADBCH(99, unit)) {
	DDALOG(LOG_DEBUG) "dda%d:(x29) xxstart: line %d t_state = %x\n",
			  unit, line, tp->t_state
	DDAELOG;
    }
#endif  DDADEBUG

    /* If it's currently active, or delaying, no need to do anything. */
    if ((tp->t_state & TS_CARR_ON) == 0) {
	tp->t_state &= ~(TS_TTSTOP | TS_BUSY);
	ttyflush(tp, FREAD | FWRITE);
	tp->t_state &= ~TS_ASLEEP;
	wakeup((caddr_t) &tp->t_outq);
	goto out;
    }
    if (tp->t_state & (TS_TIMEOUT | TS_BUSY | TS_TTSTOP))
	goto out;

    /* wait for free */
    if (dda_softc[unit].dda_state != S_LINK_UP) {
	ttyflush(tp, FREAD | FWRITE);
        DMESG(unit, 96, (DDALOG(LOG_ERR)
			"dda%d:(x29) xxstart: unit offline\n", unit DDAELOG) );
	goto out;
    }
    /* If the writer was sleeping on output overflow, wake him when low tide
     * is reached. */
    if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
	if (tp->t_state & TS_ASLEEP) {
	    tp->t_state &= ~TS_ASLEEP;
	    wakeup((caddr_t) &tp->t_outq);
	}
	if (tp->t_wsel) {
	    selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
	    tp->t_wsel = 0;
	    tp->t_state &= ~TS_WCOLL;
	}
    }
    /* restart transmission unless output queue is empty */
    if (tp->t_outq.c_cc == 0)
	goto out;

    /* if this is an outbound pad line and it's in command mode */
    if (pp->p_state == PS_COM) {
	xxpadhandle(ds, tp, pp);
	goto out;
    }

    /* Allocate an mbuf to stuff the chars into */
    m = 0;
    MGET(m, M_DONTWAIT, MT_DATA);
    if (m == 0) {
        DMESG(unit, 97, (DDALOG(LOG_ERR)
			"dda%d:(x29) xxstart: could not get mbuf\n",
			unit DDAELOG) );
	goto out;
    }
    cp = mtod(m, char *);
    cc = 0;

    /* copy at most MLEN-1 chars out -- must save one byte for subfunc */
    while ((cc < MLEN - 1) && (tp->t_outq.c_cc > 0)) {
	if (tp->t_flags & (RAW | LITOUT))
	    nch = ndqb(&tp->t_outq, 0);
	else {
	    nch = ndqb(&tp->t_outq, 0200);
	    if (nch == 0) {	/* if first item was a delay */
		(void) getc(&tp->t_outq);	/* discard the character */
		continue;
	    }
	}
	if (nch > (MLEN - 1) - cc)
	    nch = (MLEN - 1) - cc;

	/* If any characters were set up, start transmission; */
	if (nch) {
	    j = q_to_b(&tp->t_outq, cp, nch);

#if OUTPUT_PARITY_MASK != 0377
	    /* strip all characters as desired */
	    for (p = cp, k = j; k; k--, p++)
		*p &= OUTPUT_PARITY_MASK;
#endif

#ifdef	DDADEBUG
	    if (DDADBCH(100, unit) && j != nch) {
		DDALOG(LOG_DEBUG)
		    "dda%d:(x29) xxstart: asked for %d got %d chars\n",
		    unit, nch, j
		DDAELOG;
	    }
#endif  DDADEBUG

	    cc += nch;
	    cp += nch;
	} else
	    break;
    }

#ifdef	DDADEBUG
    if (DDADBCH(101, unit)) {
	DDALOG(LOG_DEBUG) "dda%d:(x29) xxstart: mbuf %x len %d\n",
			  unit, m, m->m_len
	DDAELOG;
    }
#endif

    /* if any data was stuffed into the mbuf then send it */
    if (cc) {
	m->m_dat[MLEN - 1] = 0;	/* subfunction: no Q-bit */
	m->m_len = cc;
	oq = &(dc->dc_oq);	/* point to output queue */
	if (IF_QFULL(oq)) {	/* if q full */
	    IF_DROP(oq);	/* drop the data */
	    m_freem(m);
	    ds->dda_if.if_collisions++;	/* for netstat display */
	    splx(s);
	    return (ENOBUFS);
	}
	IF_ENQUEUE(oq, m);	/* otherwise queue it */
	tp->t_state |= TS_BUSY;
	dda_start(ds, dc);	/* and try to output */
    } else
	m_freem(m);

out:
    if (dc->dc_lcn != 0)	/* something left in oq? */
	dda_start(ds, dc);	/* restart output */
    splx(s);
    return (0);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                        XXRESET()                            %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  In response to UNIBUS reset, reset state and restart           */
/*  transmitters.                                                  */
/*                                                                 */
/*  Call:           xxreset(uban)                                  */
/*  Argument:       uban:  UNIBUS adaptor number                   */
/*  Returns:        none                                           */
/*  Called by:      kernel software in response to UNIBUS reset    */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*ARGSUSED*/
xxreset(uban)
int uban;
{
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                        XXSTOP()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Dummy stop routine.                                            */
/*                                                                 */
/*  Call:           xxstop(tp, flag)                               */
/*  Argument:       tp:    pointer to tty structure                */
/*                  flag:  indicates                               */
/*  Returns:        none                                           */
/*  Called by:      none                                           */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*ARGSUSED*/
xxstop(tp, flag)
struct tty     *tp;
int             flag;
{
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                        XXSELECT()                           %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Circumvent bug in our bastardized design which causes ttselect */
/*  to fail.							   */
/*                                                                 */
/*  Call:           xxselect(dev, rw)                              */
/*  Argument:       dev:   device                                  */
/*                  rw:    read or write indicator                 */
/*  Returns:        0 or 1                                         */
/*  Called by:      none                                           */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

xxselect(dev, rw)
dev_t           dev;
int             rw;
{
#ifdef	DDADEBUG
    int unit = UNIT(dev);
    if (DDADBCH(102, unit)) 
	DDALOG(LOG_DEBUG) "dda%d:(x29) select()\n", unit DDAELOG;
#endif  DDADEBUG

    return (ttselect(MAJLINE(dev), rw));
}

/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                                                             %%*/
/*%%                   LOCAL  FUNCTIONS                          %%*/
/*%%                                                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      X29_SUPR()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*       This routine processes received supervisor messages.      */
/*       Depending on the message type, the appropriate action is  */
/*       taken.                                                    */
/*                                                                 */
/*  Call:              x29_supr(ds, p)                             */
/*  Arguments:         ds:  pointer to dev control block struct    */
/*                     p:   pointer to a character array           */
/*                              containing the supervisor message  */
/*  Returns:           nothing                                     */
/*  Called by:         dda_supr()                                  */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
x29_supr(ds, p)
struct dda_softc *ds;
u_char          p[];
{
    register struct dda_cb *dc;
    register struct tty *tp;
    register int    lcn;
    int   	    maxlcn;
    int		    line;

#ifdef DDADEBUG
    if (DDADBCH(103, ds->dda_if.if_unit)) {
	DDALOG(LOG_DEBUG) "dda%d:(x29) x29_supr()\n", ds->dda_if.if_unit
	DDAELOG;
    }
#endif  DDADEBUG

    switch (p[0]) {
    case LINE_STATUS:		/* link status msg */
    case RESTART:		/* restart received */
    case RSTRT_ACK:		/* restart ack */
    case STATRESP:		/* Statistics Response from FEP */
	DMESG(ds->dda_if.if_unit, 98, (DDALOG(LOG_ERR)
		"dda%d:(x29) x29_supr: unexpected message type\n",
		ds->dda_if.if_unit DDAELOG));
	break;
    case ANSWER:		/* call answered */
	lcn = p[1] / 2;
	dc = &(ds->dda_cb[lcn]);
	if (dc->dc_state == LC_CALL_PENDING) {	/* if a call pending */
	    decode_answer(p, dc);
	    dc->dc_state = LC_DATA_IDLE;	/* set state */
	    dc->dc_flags = DC_X29;
	    line = dc->dc_line;			/* which line are we? */
#ifdef DDADEBUG
	    if (DDADBCH(114, ds->dda_if.if_unit)) {
		DDALOG(LOG_DEBUG) "dda%d:(x29) x29_supr: answer: line=%d\n",
			ds->dda_if.if_unit, line
		DDAELOG;
	    }
#endif  DDADEBUG

	    if (line == -1) {				/* fubar! */
		DMESG(ds->dda_if.if_unit, 107, (DDALOG(LOG_ERR)
		    "dda%d:(x29) x29_supr: answer: line was -1, VC 0x%x\n",
		    ds->dda_if.if_unit, p[1] DDAELOG));
	    }

	    xx_padinfo[line].p_state = PS_PAD;
	    xxstart(&xx_tty[line]);
	} else {
	    DMESG(ds->dda_if.if_unit, 108, (DDALOG(LOG_ERR)
		"dda%d:(x29) x29_supr: unexpected answer on LCN %d\n",
		ds->dda_if.if_unit, lcn DDAELOG));
	}
	if (LOG_CALLS) {
	    DDALOG(LOG_INFO) "dda%d:(x29) LCN %d: connected\n",
			     ds->dda_if.if_unit, lcn
	    DDAELOG;
	}
	break;

    case RING:			/* incoming call */
	if (decode_ring(p)) {
	    /* find a free lcn associated with a XX_HOST open */
	    dc = &ds->dda_cb[1];
	    maxlcn = nddach[ds->dda_if.if_unit];
	    for (lcn = 1; lcn <= maxlcn; lcn++) {
		if (dc->dc_state == LC_IDLE && dc->dc_flags & DC_X29W)
		    break;
		dc++;
	    }
	    if (lcn > maxlcn) {				/* if no free lcn's */
		if (LOG_BUSY) {
		    DDALOG(LOG_ERR)
			"dda%d:(x29) no free X29W lcns, call rejected, vc=0x%x\n",
			ds->dda_if.if_unit, p[1]
		    DDAELOG;
		}
		send_supr(ds, CLEARVC, p[2], 0);	/* clear call */
		break;					/* exit case */
	    }
	    
	    /* got a good lcn, now use it */

#ifdef DDADEBUG
	    if (DDADBCH(103, ds->dda_if.if_unit)) {
		DDALOG(LOG_ERR) "dda%d:(x29) supr_msg: call from 0x%0x\n",
		       ds->dda_if.if_unit, (u_long) dc->dc_inaddr.s_addr
		DDAELOG;
	    }
#endif DDADEBUG

	    dc->dc_state = LC_DATA_IDLE;	/* set state */
	    dc->dc_pktsizein = 0;
	    dc->dc_pktsizeout = 0;
	    dc->dc_wsizein = 0;
	    dc->dc_wsizeout = 0;
	    dc->dc_flags = DC_X29;
	    send_supr(ds, ANSWER, lcn * 2, p[2]);	/* send answer */
	    if (LOG_CALLS) {
		DDALOG(LOG_INFO) "dda%d:(x29) Call accepted LCN %d\n",
	  		         ds->dda_if.if_unit, dc->dc_lcn
		DDAELOG;
	    }

	    line = dc->dc_line;

#ifdef DDADEBUG
	    if (DDADBCH(114, ds->dda_if.if_unit)) {
		DDALOG(LOG_DEBUG) "dda%d:(x29) x29_supr: ring: line=%d\n",
			ds->dda_if.if_unit, line
		DDAELOG;
	    }
#endif  DDADEBUG

	    if (line == -1) {				/* fubar! */
		DMESG(ds->dda_if.if_unit, 107, (DDALOG(LOG_ERR)
		    "dda%d:(x29) x29_supr: ring: line was -1, VC 0x%x\n",
		    ds->dda_if.if_unit, p[1] DDAELOG));
		break;
	    }

	    tp = &xx_tty[line];
	    xx_padinfo[line].p_state = PS_XFR;
	    wakeup((caddr_t) &tp->t_rawq);
	    tp->t_state |= TS_CARR_ON;
#if ACC_ULTRIX > 00
	    tp->t_state &= ~TS_ONDELAY;
#endif
	    /* I would prefer to wait a bit before sending this */
	    send_x29_param_msg(ds, dc, SET_PAD,
			       x29_callin_setparams,
			       sizeof(x29_callin_setparams));
	} else {		/* bad decode */
	    send_supr(ds, CLEARVC, p[2], 0);	/* clear call */
	    DMESG(ds->dda_if.if_unit, 100, (DDALOG(LOG_ERR)
		"dda%d:(x29) Bad decode, call REJECTED VC 0x%x\n",
		ds->dda_if.if_unit, p[1] DDAELOG));
	}
	break;

    case CLEARLC:		/* clear by LCN */
	lcn = p[1] / 2;		/* get LCN */
	dc = &(ds->dda_cb[lcn]);
	if (dc->dc_state != LC_CLR_PENDING) {	/* if no clear pending */
	    send_supr(ds, CLEARLC, p[1], 0);	/* ack the clear */
	}
	if (dc->dc_state == LC_CALL_PENDING)	/* call is cleared */
	    DMESG(ds->dda_if.if_unit, 101, (DDALOG(LOG_ERR)
	    	"dda%d:(x29) Call cleared LCN %d (%x %x)\n",
	        ds->dda_if.if_unit, dc->dc_lcn, p[2], p[4] DDAELOG));

	hist_lcn_state(ds->dda_if.if_unit, dc->dc_state, LC_IDLE);
	dc->dc_state = LC_IDLE;
	dc->dc_timer = TMO_OFF;	/* stop timer */
	dc->dc_wsizein = dc->dc_wsizeout = 0;
	dc->dc_pktsizein = dc->dc_pktsizeout = 0;
	abort_io(ds->dda_if.if_unit, lcn);
	xx_tp_hangup(ds, dc);	/* will clear flags */
	break;

    case CLEARVC:		/* clear by VCN */
	send_supr(ds, CLEARVC, p[1], 0);	/* send clear ack */
	if (LOG_CALLS) {
	    DDALOG(LOG_INFO)
		"dda%d:(x29) Network cleared VC %x (%x %x)\n",
	        ds->dda_if.if_unit, p[1], p[2], p[4]
	    DDAELOG;
	}
	break;

    case RESET:		/* X25 reset */
	send_supr(ds, RESET_ACK, p[1], 0);	/* send reset ack */
	abort_io(ds->dda_if.if_unit, (int) p[1] / 2);
	DMESG(ds->dda_if.if_unit, 102, (DDALOG(LOG_ERR)
	    "dda%d:(x29) X25 RESET on LCN %d (%x %x)\n",
	    ds->dda_if.if_unit, p[1] / 2, p[2], p[4] DDAELOG));
	break;

    case INTERRUPT:		/* X25 interrupt */
#ifdef INDICATE_BREAK_ON_INTERRUPT
	lcn  = p[1] / 2;
	dc   = &(ds->dda_cb[lcn]);

	line = dc->dc_line;

	if (line == -1) {				/* fubar! */
	    DMESG(ds->dda_if.if_unit, 107, (DDALOG(LOG_ERR)
		"dda%d:(x29) x29_supr: break: line was -1, VC 0x%x\n",
		ds->dda_if.if_unit, p[1] DDAELOG));
	    break;
	}

	tp   = &xx_tty[line];

	if (tp->t_flags & RAW)
	    c = 0;
	else
#if ACC_ULTRIX >= 30
	    c = tp->c_cc[VINTR];/* else make it the interrupt */
#else
	    c = tp->t_intrc;	/* else make it the interrupt */
#endif
#if NBK > 0
	if (tp->t_line == NETLDISC) {
	    BKINPUT(c, tp);
	} else
#endif
	    (*linesw[tp->t_line].l_rint) (c, tp);
	/* send_supr (ds, INTR_ACK, p[1], 0); 	not needed -- done by FE */
#endif
	break;

    case INTR_ACK:
	/* quietly drop the acknowledgement */
	break;
    default:
	DMESG(ds->dda_if.if_unit, 104, (DDALOG(LOG_ERR)
	    "dda%d:(x29) supervisor error (%x %x %x %x)\n",
	    ds->dda_if.if_unit, p[0], p[1], p[2], p[3] DDAELOG));
    }
}

	/* hangup any attached processes */
PRIVATE void
xx_tp_hangup(ds, dc)
struct dda_softc *ds;
register struct dda_cb *dc;
{
    register struct tty *tp;
    register padinfo    *pp;
    register int         line;

    line = dc->dc_line;

    if (line == -1) {				/* fubar! */
	DMESG(ds->dda_if.if_unit, 107, (DDALOG(LOG_ERR)
	    "dda%d:(x29) xx_tp_hangup: line was -1\n",
	    ds->dda_if.if_unit DDAELOG));
	return;
    }

    tp   = &xx_tty[line];
    pp   = &xx_padinfo[line];

    if (pp->p_flow != P_NOBLOCK) {	/* currently blocked? */
	register struct hdx_chan *hc;
	hc = (struct hdx_chan *) & dc->dc_rchan;
	dda_rrq(ds, hc);	/* make sure we hang a read */
    }
    pp->p_flow = P_NOBLOCK;
    tp->t_state &= ~(TS_CARR_ON | TS_ASLEEP | TS_BUSY);
    ttyflush(tp, FREAD | FWRITE);
    gsignal(tp->t_pgrp, SIGHUP);
    gsignal(tp->t_pgrp, SIGCONT);
    tp->t_state &= ~TS_ASLEEP;
    wakeup((caddr_t) &tp->t_outq);
    xxmode[line] = MODE_UNUSED;
    tp->t_addr = (caddr_t) NULL;
    pp->p_state = PS_IDLE;
    if (pp->p_mchsav) {
	m_freem(pp->p_mchsav);
	pp->p_msav = pp->p_mchsav = (struct mbuf *) NULL;
    }
    dc->dc_flags &= ~(DC_X29 | DC_X29W);	/* release to others */
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                      X29_DATA()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*    This routine is called when a data channel I/O completes.    */
/*    If the completion was for a write, an attempt is made to     */
/*    start output on the next packet waiting for output on that   */
/*    LCN.  If the completion was for a read, the received packet  */
/*    is sent to the IP input queue (if no error) and another read */
/*    is started on the LCN.                                       */
/*                                                                 */
/*  Call:              x29_data(ds, hc, cc, cnt)                   */
/*  Argument:          ds:  device control block                   */
/*                     hc:  half duplex channel control block      */
/*                     cc:   Mailbox I/O completion status         */
/*                     cnt:  byte count                            */
/*  Returns:           nothing                                     */
/*  Called by:         ddainta()                                   */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

#define QBIT	0x80

PRIVATE void
x29_data(ds, hc, cc, cnt, subcc)
register struct dda_softc *ds;
register struct hdx_chan *hc;
int             cc,
                cnt,
                subcc;
{
    register struct dda_cb *dc = &(ds->dda_cb[hc->hc_chan / 2]);
    register struct tty *tp;

#ifdef DDADEBUG
    if (DDADBCH(104, ds->dda_if.if_unit)) {
 	DDALOG(LOG_DEBUG)
	    "dda%d:(x29) x29_data: chan=%x cc=%x cnt=%x subcc=%x\n",
	    ds->dda_if.if_unit, hc->hc_chan, cc, cnt, subcc
	DDAELOG;
    }
#endif DDADEBUG

    if (hc->hc_chan & 0x01) {	/* if write, fire up next output */
#ifdef DDADEBUG
	dc->dc_out_t = TMO_OFF;	/* turn off output completion timer */
#endif

	if ((hc->hc_func != DDAABT) && (hc->hc_curr = hc->hc_curr->m_next))
	    dda_wrq(ds, hc, 0);
	else {
	    /* it is abort | no more data left */
	    char            qbit_indicator;
	    qbit_indicator = hc->hc_mbuf->m_dat[MLEN - 1];
	    m_freem(hc->hc_mbuf);
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	    if (hc->hc_func == DDAABT) {
		hc->hc_func &= ~DDAABT;
		hc->hc_inv &= ~INVALID_MBUF;
	    } else
		ds->dda_if.if_opackets++;
	    dc->dc_flags &= ~DC_OBUSY;

	    if (qbit_indicator == QBIT) {	/* Q-bit packet? */
		dda_start(ds, dc);		/* restart output */
	    } else {
		tp = &xx_tty[dc->dc_line];
		tp->t_state &= ~TS_BUSY;
		xxstart(tp);	/* restart tty output */
	    }
	}

	/* it's a packet coming in from the front end to the host */
    } else {
#ifdef DDADEBUG
	dc->dc_flags &= ~DC_IPEND;
#endif
	hc = &dc->dc_rchan;

#ifdef DDADEBUG
	if (DDADBCH(105, ds->dda_if.if_unit)) {
	    u_char         *p;
	    DDALOG(LOG_DEBUG) "dda%d:(x29) ", ds->dda_if.if_unit DDAELOG;
	    p = mtod(hc->hc_curr, u_char *);
	    prt_bytes(ds->dda_if.if_unit, "received data", p, (cnt < 64 ? cnt : 64));
	}
	if (DDADBCH(106, ds->dda_if.if_unit)) {
	    DDALOG(LOG_DEBUG)
		"dda%d:(x29) x29_data: read complete mbuf=%x curr=%x\n",
		ds->dda_if.if_unit, hc->hc_mbuf, hc->hc_curr
	    DDAELOG;
	}
#endif DDADEBUG

	if (dc->dc_state != LC_DATA_IDLE) {
	    m_freem(hc->hc_mbuf);	/* toss the packet, lcn is dead */
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	} else if (cc == DDAIOCOK || (cc == DDAIOCOKP && !(subcc & QBIT))) {
	/* Queue up I/O completion OK transfers and I/O OK with more data
	 * pending transfers (as long as it's not a Qbit message).
	 * This algorythm operates differently than the IP handler due
	 * to the fact that we don't need to wait for the entire X.25
	 * packet to arrive on the host before we assemble it.  To do
	 * so should be OK,  but unfortunately it seems some brain-dead
	 * PAD's generate packets with the M-bit set if they have more
	 * data in their internal buffers.  This can cause the system
	 * to burn up mbufs waiting for us to finally receive a packet
	 * with the M-bit not set.  However, we should hold up on processing
	 * packets with both the Q-bit and the M-bit set until we receive
	 * the entire Q-bit message.  If we get 30k Q-bit packets, we will
	 * die, but that is obscenely absurd in the first place.
	 * (sigh)	-- pst 7-19-89
	 */

#ifdef DDADEBUG
	    if (DDADBCH(107, ds->dda_if.if_unit)) {
		DDALOG(LOG_DEBUG)
		    "dda%d:(x29) x29_data: chan=%x DDAIOCOK\n",
		    ds->dda_if.if_unit, hc->hc_chan
		DDAELOG;
	    }
#endif DDADEBUG
	    hc->hc_curr->m_len += cnt;	/* update byte count */

	    ds->dda_if.if_ipackets++;
	    /* HANDLE THE DATA HERE */
	    if (subcc & QBIT) {
		int             len;
		char           *mcp;
		mcp = mtod(hc->hc_curr, char *);
		len = hc->hc_curr->m_len;

#ifdef DDADEBUG
		if (DDADBCH(108, ds->dda_if.if_unit))
		    prt_bytes(ds->dda_if.if_unit,
			      "(x29) Qbit:", mcp, (len < 64 ? len : 64));
#endif DDADEBUG

		if (*mcp == BREAK_INDIC) {	/* Break indication? */
		    register struct tty *tp;
		    if (x29_break_reply_is_required(mcp, len)) {
			/* tell pad to stop discarding output */
			send_x29_param_msg(ds, dc, SET_PAD,
					   x29_break_ack_params, 2);
		    }
		    hc->hc_curr->m_len = 1;	/* change data to single byte */
		    tp = &xx_tty[dc->dc_line];
		    if (tp->t_flags & RAW)	/* if port is in raw mode, */
			*mcp = 0;	/* make the byte a null */
		    else
#if ACC_ULTRIX >= 30
			*mcp = tp->t_cc[VINTR];	/* else make it the interrupt */
#else
			*mcp = tp->t_intrc;	/* else make it the interrupt */
#endif
		    x29_dhandle(ds, dc, 0);
		    return;
		} else if (*mcp & READ_PAD) {
		    if (len == 1)	/* just a message, no params? */
			send_x29_param_msg(ds, dc, PAR_INDICATION,
					   x29_callout_params,
					   sizeof(x29_callout_params));
		    else
			send_x29_param_msg(ds, dc, PAR_INDICATION, mcp + 1, len - 1);
		    m_freem(hc->hc_mbuf);
		    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
		} else {
		    m_freem(hc->hc_mbuf);
		    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
		}
	    } else {			/* not Qbit data, process normally */
		x29_dhandle(ds, dc, 0);
		return;
	    }
	} else if (cc == DDAIOCOKP) {	/* good completion, more data pending */
	    hc->hc_curr->m_len += cnt;
	} else {			/* toss packet */
	    m_freem(hc->hc_mbuf);
	    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) 0;
	}
	/* hang a new data read */
#ifdef DDADEBUG
	dc->dc_flags |= DC_IPEND;
#endif
	dda_rrq(ds, hc);
    }
}

/* this routine copies chars from the dc_rchan mbuf to the upper
 * level software.  If all the characters are read then the mbuf is
 * freed and a new read is hung on the channel.
 *
 * This routine is called from below by the int A handler and from above
 * by the device read routine.
 */

PRIVATE void
x29_dhandle(ds, dc, restart)
register struct dda_softc *ds;
register struct dda_cb *dc;
int             restart;
{
    register struct tty *tp;
    register struct hdx_chan *hc;
    register padinfo *pp;
    u_char         *cp,
                    c;
    struct mbuf    *m2,
                   *m;
    int             s,
                    line;
    register int    j;
    static int      recurse = 0;

    s = splimp();

    if (recurse) {	/* don't allow ourselves to be called recursively */
	splx(s);
	return;
    } else
	recurse = 1;

    hc = (struct hdx_chan *) &dc->dc_rchan;

    line = dc->dc_line;

    tp = &xx_tty[line];
    pp = &xx_padinfo[line];

    if (restart) {		/* trying to restart input? */
	j  = pp->p_flow;
	m  = pp->p_mchsav;
	m2 = pp->p_msav;

#ifdef DDADEBUG
	if (DDADBCH(109, ds->dda_if.if_unit)) {
	    DDALOG(LOG_DEBUG)
		"dda%d:(x29) flow restart [%d] in %x\n",
		ds->dda_if.if_unit, j, m
	    DDAELOG;
	}
#endif DDADEBUG

    } else {
	j = P_NOBLOCK;
	m2 = m = hc->hc_mbuf;	/* que mbuf chain */
    }

    if (m == 0) {
	DMESG(ds->dda_if.if_unit, 105, (DDALOG(LOG_ERR)
		"dda%d:(x29) x29_dhandle: null mbuf\n",
		ds->dda_if.if_unit DDAELOG));
	hc->hc_mbuf = hc->hc_curr = (struct mbuf *) NULL;
	dda_rrq(ds, hc);
	goto out;
    }
    while (m2) {
	cp = mtod(m2, u_char *);
	for (; j < m2->m_len; j++) {
	    c = cp[j] & INPUT_PARITY_MASK;
	    if (tp->t_rawq.c_cc + tp->t_canq.c_cc >= TTYHOG - 2)
		if (!ttbreakc(c, tp))
		    continue;	/* dump the character */
#if NBK > 0
	    if (tp->t_line == NETLDISC) {
		BKINPUT(c, tp);
	    } else
#endif
		(*linesw[tp->t_line].l_rint) (c, tp);


	    /* Block further input iff: Current input > threshold AND input
	     * is available to user program */

	    if ((tp->t_rawq.c_cc + tp->t_canq.c_cc) >= TTYHOG / 4 &&
		((tp->t_flags & (RAW | CBREAK)) || (tp->t_canq.c_cc > 0))) {
#ifdef DDADEBUG
		if (DDADBCH(109, ds->dda_if.if_unit)) {
		    DDALOG(LOG_DEBUG)
			"dda%d:(x29) flow on [%d] in %x of %d\n",
			ds->dda_if.if_unit, j, m2, m2->m_len
		    DDAELOG;
		}
#endif DDADEBUG
		pp->p_flow = j + 1;
		pp->p_msav = m2;
		pp->p_mchsav = m;
		if (restart == 0)
		    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) NULL;
		goto out;
	    }
	}
	m2 = m2->m_next;
	j = P_NOBLOCK;
    }
    if (restart) 
	pp->p_msav = pp->p_mchsav = (struct mbuf *) NULL;

    m_freem(m);
    hc->hc_mbuf = hc->hc_curr = (struct mbuf *) NULL;
    pp->p_flow  = P_NOBLOCK;

#ifdef DDADEBUG
    dc->dc_flags |= DC_IPEND;
#endif

    dda_rrq(ds, hc);

out:
    recurse = 0;
    splx(s);
}

PRIVATE void
xx_qbit_msg(tp, unit, msg)
register struct tty *tp;
int             unit;
char           *msg;
{
    register struct dda_cb *dc;
    register struct dda_softc *ds;
    int             s;
    
    ds = &dda_softc[unit];
    dc = (struct dda_cb *) tp->t_addr;
    s = splimp();

#ifdef DDADEBUG
    if (DDADBCH(110, unit)) {
	DDALOG(LOG_DEBUG)
	    "dda%d:(x29) xx_qbit_msg: %d %d %d\n",
	    unit, msg[0], msg[1], msg[2]
	DDAELOG;
    }
#endif DDADEBUG

    if (msg[1] < (MLEN - 4))
	send_x29_param_msg(ds, dc, msg[0], msg + 2, msg[1]);
    splx(s);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                        XXCNTL()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Do modem control functions on a line.                          */
/*                                                                 */
/*  Call:           xxcntl(tp, c, d)                               */
/*  Argument:       tp:   pointer to tty structure                 */
/*                  c:    function code                            */
/*                  unit: for unit number                          */
/*  Returns:        none                                           */
/*  Called by:      xxopen()                                       */
/*                  xxclose()                                      */
/*                  xxread()                                       */
/*                  xxint()                                        */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
xxcntl(tp, c, unit)
register struct tty *tp;
int             c,
                unit;
{
    register struct dda_cb *dc;
    register struct dda_softc *ds;
    register padinfo *pp;
    int             s,
                    l;

    l = tp - xx_tty;
    ds = &dda_softc[unit];
    pp = &xx_padinfo[l];
    s = splimp();

#ifdef DDADEBUG
    if (DDADBCH(111, unit)) {
	DDALOG(LOG_DEBUG)
	    "dda%d:(x29) xxcntl: tp=0x%x line=%d\n", unit, tp, l
	DDAELOG;
    }
#endif DDADEBUG

    switch (c) {
    case XX_C_PAD:
	if (tp->t_addr)
	    break;
	if (dc = find_free_lcn(ds)) { /* race against locate_x25_lcn */
	    dc->dc_flags = DC_X29;
	    dc->dc_line = l;
	    pp->p_state = PS_COM;
	    tp->t_addr = (caddr_t) dc;
	    tp->t_flags &= ~ECHO;
	    pp->p_flow = P_NOBLOCK;
	    pp->p_msav = pp->p_mchsav = (struct mbuf *) NULL;
	    pp->p_idx = 0;
	    pp->p_line[0] = '\0';
	} else
	    tp->t_addr = (caddr_t) NULL;
	break;
    case XX_C_HOST:
	if (tp->t_addr)
	    break;
	if (dc = find_free_lcn(ds)) { /* race against locate_x25_lcn */
	    dc->dc_flags = DC_X29W;
	    dc->dc_line = l;
	    pp->p_state = PS_WAIT;
	    pp->p_flow = P_NOBLOCK;
	    pp->p_msav = pp->p_mchsav = (struct mbuf *) NULL;
	    tp->t_addr = (caddr_t) dc;
	} else
	    tp->t_addr = (caddr_t) NULL;
	break;
    case XX_C_CLOSE:
	pp->p_state = PS_IDLE;
	if (pp->p_mchsav) {
	    m_freem(pp->p_mchsav);
	    pp->p_msav = pp->p_mchsav = (struct mbuf *) NULL;
	}
	dc = (struct dda_cb *) tp->t_addr;
	if (dc == 0) 
	    break;
	if (pp->p_flow != P_NOBLOCK) {	/* currently blocked? */
	    register struct hdx_chan *hc;
	    hc = (struct hdx_chan *) &dc->dc_rchan;
	    dda_rrq(ds, hc);		/* make sure we hang a read */
	}
#ifdef	DDADEBUG
	if (DDADBCH(111, unit)) {
	    static char *st[] = { "lcn down", "lcn restart", "idle",
				  "call pending", "data idle", "clear pending"
				};
	    DDALOG(LOG_DEBUG)
		"dda%d:(x29) xxcntl: close state: %s\n", unit, st[dc->dc_state]
	    DDAELOG;
	}
#endif DDADEBUG

	if (dc->dc_state == LC_DATA_IDLE || dc->dc_state == LC_CALL_PENDING)
	    clear_lcn(ds, dc);	/* send clear & set state to clr_pending */
				/* timers will convert it to LC_IDLE later */

#ifdef	DDADEBUG
	else 
	    if (DDADBCH(111, unit)) {
		DDALOG(LOG_DEBUG)
		    "dda%d:(x29) xxcntl: warning: state not data_idle\n", unit
		DDAELOG;
	    }
#endif

	dc->dc_flags &= ~(DC_X29 | DC_X29W);	/* release to others */
	tp->t_addr = (caddr_t) NULL;
	break;
    case XX_C_BREAK:

	/* really should look at X.3 parameters to decide if an interrupt
	 * packet should be sent. instead, we take an action which assumes
	 * PAD parameter 7 has value 21 */
	dc = (struct dda_cb *) tp->t_addr;
	send_supr(ds, INTERRUPT, dc->dc_lcn * 2, 0);
	send_x29_param_msg(ds, dc, BREAK_INDIC, 0, 0);
	break;
    }
    splx(s);
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                     X29_INIT()                              %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*  Software reset, clear lines.                                   */
/*                                                                 */
/*  Call:           x29_init(unit, active);                        */
/*  Argument:       unit:  ACP _XX device                          */
/*  Returns:        none                                           */
/*  Called by:      none                                           */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
x29_init(unit, active)
int             unit,
                active;
{
    register int    i;
    register padinfo *pp;

#ifdef	DDADEBUG
    if (DDADBCH(113, unit)) {
	DDALOG(LOG_DEBUG) "dda%d:(x29) x29_init() active=%d\n",
			  unit, active
	DDAELOG;
    }
#endif  DDADEBUG

    if (active)
	xxclear(unit);
    else {
	for (i = 0; i < XXLPERBRD; i++) {
	    xx_tty[unit * XXLPERBRD + i].t_state = PS_IDLE;
	    pp = &xx_padinfo[unit * XXLPERBRD + i];
	    pp->p_state = PS_IDLE;
	    pp->p_flow  = P_NOBLOCK;
	    pp->p_msav  = pp ->p_mchsav = (struct mbuf *) NULL;
	}
    }
}

PRIVATE void
xxclear(unit)
int             unit;
{
    register struct tty *tp;
    register struct dda_softc *ds;
    register struct dda_cb *dc;
    int             i,
                    state;

    ds = &dda_softc[unit];
    for (i = 0, tp = &xx_tty[unit * XXLPERBRD]; i < XXLPERBRD; i++, tp++) {
	state = tp->t_state;
#ifdef	DDADEBUG
	if (DDADBCH(112, unit) && state) {
	    DDALOG(LOG_DEBUG)
		"dda%d:(x29) xxclear: line=%d pgrp=%d state=%d\n",
		unit, i, tp->t_pgrp, state
	    DDAELOG;
	}
#endif  DDADEBUG
	if (state & TS_WOPEN) {
	    tp->t_state &= ~TS_WOPEN;
	    wakeup(&tp->t_rawq);
	}
	if (tp->t_state) {
	    dc = (struct dda_cb *) tp->t_addr;
	    if (dc) {
		xx_tp_hangup(ds, dc);
		dc->dc_line = -1;	/* break correspondence */
	    }
	}
    }
}

/*@@%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*%%                        XXSHOW()                             %%*/
/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/
/*                                                                 */
/*  Purpose:                                                       */
/*                                                                 */
/*      Show status of each active unit                            */
/*                                                                 */
/*  Call:           xxshow()                                       */
/*  Argument:       none		                           */
/*  Returns:        none                                           */
/*  Called by:      none                                           */
/*                                                                 */
/*##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

PRIVATE void
xxshow()
{
    register struct tty *tp;
    register padinfo    *pp;
    int                  unit,
                         i;
    static char	        *st[] = { "idle", " com", " pad", "wait", "xfer" };
	

    for (unit = 0; unit < (NDDA < XXBOARDS ? NDDA : XXBOARDS); unit++) {
	uprintf("\nACP5250/6250 X29 driver: state of unit %d -\n", unit);
	uprintf("line\tstate\tlcn\tflow\ttstate\ttflags\n");

	for (i = 0, tp = &xx_tty[unit * XXLPERBRD]; i < XXLPERBRD; i++, tp++) {
	    if (tp->t_state) {
		pp = &xx_padinfo[i]; 
		uprintf("%d:\t%s\t%d\t%d\t%x\t%x\n", i, st[pp->p_state],
		       (struct dda_cb *) (tp->t_addr) - dda_softc[unit].dda_cb,
		       pp->p_flow, tp->t_state, tp->t_flags);
	    }
	}
    }
    uprintf("remaining lines free\n");
}

/******************************************************************************
 *                           PAD CODE
 ******************************************************************************/
/* PADCHARUP - Pass a character up towards the user */
#define PADCHARUP(c,tp) (*linesw[(tp)->t_line].l_rint) ((c), (tp))

PRIVATE void
xxpadhandle(ds, tp, pi)
struct dda_softc *ds;
struct tty     *tp;		/* pointer to relevant tty structure */
padinfo        *pi;		/* pointer to relevant padinfo structure */
{
    register int    i;
    register char   c;
    register struct dda_cb *dc;
    int             nch;
    char            tbuf[CBSIZE];	/* CBSIZE is number of char in a
					 * cblock */
    nch = q_to_b(&tp->t_outq, tbuf, CBSIZE);

    /* handle characters in command state. Its OK if were slow here because
     * there is a person on the other end of the discussion */
    dc = (struct dda_cb *) tp->t_addr;
    for (i = 0; i < nch; i++) {
	if (pi->p_idx >= P_LINELEN) {
	    xxpadmsg("\r\ncommand too long\r\n@", tp);
	    pi->p_idx = 0;
	    return;
	}
	c = pi->p_line[pi->p_idx] = tbuf[i] & INPUT_PARITY_MASK;
	if (c == '\r' || c == '\n') {
	    PADCHARUP('\r', tp);
	    PADCHARUP('\n', tp);
	    pi->p_line[pi->p_idx] = '\0';
	    if (dc && dc->dc_state != LC_IDLE) {
		xxpadmsg("cannot call, line is in transition\r\n", tp);
		if (dc && dc->dc_state == LC_CALL_PENDING)
		    xxpadmsg("previous call still pending\r\n", tp);
	    } else if (xxpadparse(ds, pi, tp) == 0)
		PADCHARUP('@', tp);
	    pi->p_idx = 0;
	} else if (c == '\b' || c == '\177') {
	    if (pi->p_idx) {
		pi->p_idx--;
		xxpadmsg("\b \b", tp);
	    }
	} else {
	    pi->p_idx++;
	    PADCHARUP(c, tp);
	}
    }
}

PRIVATE int
xxpadparse(ds, pi, tp)
struct dda_softc *ds;
padinfo        *pi;
struct tty     *tp;
{
    char           *p = pi->p_line;

    if (*p == 'c' || *p == 'C') {	/* connect command */
	for (p++; *p == ' '; *p++);
	if (*p < '0' || *p > '9')
	    xxpadmsg("???\r\n", tp);
	else			/* place a call */
	    return xxpadcall(ds, p, tp);
    } else if (*p)
	xxpadmsg("invalid command\r\n", tp);
    return 0;
}

PRIVATE int
xxpadcall(ds, addr, tp)
struct dda_softc *ds;
char           *addr;
struct tty     *tp;
{
    register int    i = 0;
    struct in_addr  in;

    while (addr[i]) {
	if (addr[i] < '0' || addr[i] > '9') {
	    xxpadmsg("invalid address\r\n", tp);
	    return 0;
	}
	i++;
    }
    ddacb_called_addr[0] = i;
    bcopy(addr, ddacb_called_addr + 1, i);
    ddacb_user_data[0] = (u_char) 0;	/* no user data for now */
    in.s_addr = 0;
    return make_x25_call(ds, (struct dda_cb *) tp->t_addr, in, X25_PROTO_X29);
}

PRIVATE void
xxpadmsg(s, tp)
char           *s;
struct tty     *tp;
{
    while (*s) {
	PADCHARUP(*s, tp);
	s++;
    }
}

/*
 * This routine is used to respond to
 * READ_PARAMS and SET_READ_PARAMS requests, and also
 * to send out a SET_PARAMS request for incoming calls.
 * The outgoing pad supports NO parameters.
 */
send_x29_param_msg(ds, dc, type, msg, len)
register struct dda_cb *dc;
register struct dda_softc *ds;
x29_pad_pair   *msg;
{
    struct mbuf    *m;
    u_char         *p;
    short           i;
    register struct ifqueue *oq;
    m = 0;			/* Allocate an mbuf to stuff the chars into */
    MGET(m, M_DONTWAIT, MT_DATA);
    if (m == 0) {
	DMESG(ds->dda_if.if_unit, 106, (DDALOG(LOG_ERR)
		"dda%d:(x29) couldn't get mbuf for QBIT message\n",
		ds->dda_if.if_unit DDAELOG));
	return;
    }
    m->m_dat[MLEN - 1] = QBIT;	/* set Q-bit */
    p = mtod(m, u_char *);
    len = len / 2;
    *p++ = type;
    if (type == PAR_INDICATION) {	/* our pad supports NO parameters */
	for (i = 0; i < len; i++) {
	    *p++ = msg[i].ref | 0x80;	/* set invalid bit */
	    *p++ = 1;		/* not implemented */
	}
    } else {			/* BREAK_INDIC, SET_PAD to ack break */
	for (i = 0; i < len; i++) {
	    *p++ = msg[i].ref;
	    *p++ = msg[i].val;
	}
    }
    m->m_len = 1 + 2 * len;
    oq = &(dc->dc_oq);		/* point to output queue */
    if (IF_QFULL(oq)) {		/* if q full */
	IF_DROP(oq);		/* drop the data */
	m_freem(m);
	ds->dda_if.if_collisions++;	/* for netstat display */
    } else {
	IF_ENQUEUE(oq, m);	/* otherwise queue it */
	dda_start(ds, dc);	/* and try to output */
    }
}

PRIVATE int
x29_break_reply_is_required(mcp, len)
char           *mcp;
int             len;
{
    mcp++;			/* skip over break indication msg */
    while (len > 1) {		/* while there are parameters left, */
	if ((*mcp == 8) && (mcp[1] == 1))	/* paramter 8 set to 1? */
	    return 1;		/* yes */
	mcp += 2;
	len -= 2;
    }
    return 0;
}

/*
 * Ultrix 3.0 removed the old ttbreakc() kernel routine when moving to
 * a posix compliant driver.  Here it is again, (for our local use only!!!)
 *
 */
#if ACC_ULTRIX >= 30
static int
ttbreakc(c, tp)
register        c;
register struct tty *tp;
{
    return (c == tp->t_cc[VEOL] || c == tp->t_cc[VEOF] ||
	    c == tp->t_cc[VEOL2] || c == '\r' && (tp->t_flags & CRMOD));
}
#endif


/*
Revision History: 

09-Jun-1988: Unknown (Brad?)
	Initial implementation.
15-Feb-1989: Paul Traina
	Fixed point bug in send_x29_prm_msg
08-Mar-1989: Steve Johnson
	Fixed bug in xx_flow logic
24-May-1989: Paul Traina
	Upgraded for Ultrix 3.0
28-May-1989: Paul Traina
	Added more driver intelligence to disable pad durring call pending
31-May-1989: Paul Traina
	Added flexible mapping for # of boards per unit
04-Jun-1989: Paul Traina
	Fixed driver to dequeue Q-bit X29 packets from the mbuf chain properly.
19-Jun-1989: Paul Traina
	Fixed previous fix-- will need to go over if-elseif logic more
	carefully to make sure we're doing the right thing.  It should be
	recoded.
	Modernized entire debug code suite, changed xxshow functionality to
	use the uprintf() kernel call to display data on user's terminal for
	the xxshow hack.
12-Jul-1989: Paul Traina
	Changed format of some debug messages.  Removed LOCAL_VOID in
	favor of PRIVATE routine to aid in debugging.  Simplified some
	chunky logic.
18-Jul-1989: Paul Traina
	Flipped search order for finding a free X29W lcn at RING time.
	Moved the dc_key.ttyline field out of the union and made it dc_line.
	This fixed the Dartmouth singleuser bug.
19-Jul-1989: Paul Traina
	Changed the packet decode logic in x29_data to immediately process
	packets with more data pending (i.e. the M-bit) right away, instead
	of queuing them up.  (Note: it still queues up Q-bit packets)  This
	may fix the Dartmouth mbuf problem with blasting uploads.
27-Jul-1989: Paul Traina
	Removed 8-bit strip in x29_dhandle.
01-Aug-1989: Paul Traina
	Added additional two parameters to make_x25_call for userdata/length
	for merge with new pad software.
02-Aug-1989: Paul Traina
	Reinserted 8-bit strip on data received from the net.  (uses
	PARITY_MASK define for easy change).
	Fixed forward declaration of ttbreakc().
	Improved readability of xxshow output.
	Removed "super" pad code.
	Modified ps_state to be a real state variable.
03-Aug-1989: Paul Traina
	Reversed earlier change to xxselect which didn't pass major #.
	Modified xxshow output to not use %nd which isn't supported in BSD.
28-Aug-1989: Paul Traina
	Changed parameters of make_x25_call -- plug user data field directly.
14-Nov-1989: Paul Traina
	Added support for Ultrix 3.1 which uses HUPCL instead of HUPCLS
	because of that stupid termio interface (sigh).
16-Nov-1989: Paul Traina
	Changed parity mask to input_parity_mask, added output_parity_mask.
*/
