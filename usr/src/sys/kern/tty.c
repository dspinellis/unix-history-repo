/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty.c	7.13 (Berkeley) %G%
 */

#include "../machine/reg.h"

#include "param.h"
#include "systm.h"
#include "dir.h"
#include "user.h"
#include "ioctl.h"
#include "tty.h"
#define TTYDEFCHARS
#include "ttydefaults.h"
#undef TTYDEFCHARS
#include "termios.h"
#define TTYDEFCHARS
#include "ttydefaults.h"
#undef TTYDEFCHARS
#include "termios.h"
#include "proc.h"
#include "file.h"
#include "conf.h"
#include "dkstat.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"
#include "syslog.h"

/*
 * Table giving parity for characters and indicating
 * character classes to tty driver. The 8th bit
 * indicates parity, the 7th bit indicates the character
 * is an alphameric or underscore (for ALTWERASE), and the 
 * low 6 bits indicate delay type.  If the low 6 bits are 0
 * then the character needs no special processing on output.
 */

char partab[] = {
	0001,0201,0201,0001,0201,0001,0001,0201,	/* nul - bel */
	0202,0004,0003,0201,0005,0206,0201,0001,	/* bs - si */
	0201,0001,0001,0201,0001,0201,0201,0001,	/* dle - etb */
	0001,0201,0201,0001,0201,0001,0001,0201,	/* can - us */
	0200,0000,0000,0200,0000,0200,0200,0000,	/* sp - ' */
	0000,0200,0200,0000,0200,0000,0000,0200,	/* ( - / */
	0100,0300,0300,0100,0300,0100,0100,0300,	/* 0 - 7 */
	0300,0100,0000,0200,0000,0200,0200,0000,	/* 8 - ? */
	0200,0100,0100,0300,0100,0300,0300,0100,	/* @ - G */
	0100,0300,0300,0100,0300,0100,0100,0300,	/* H - O */
	0100,0300,0300,0100,0300,0100,0100,0300,	/* P - W */
	0300,0100,0100,0200,0000,0200,0200,0300,	/* X - _ */
	0000,0300,0300,0100,0300,0100,0100,0300,	/* ` - g */
	0300,0100,0100,0300,0100,0300,0300,0100,	/* h - o */
	0300,0100,0100,0300,0100,0300,0300,0100,	/* p - w */
	0100,0300,0300,0000,0200,0000,0000,0201,	/* x - del */
	/*
	 * meta chars
	 */
	0001,0201,0201,0001,0201,0001,0001,0201,	/* nul - bel */
	0202,0004,0003,0201,0005,0206,0201,0001,	/* bs - si */
	0201,0001,0001,0201,0001,0201,0201,0001,	/* dle - etb */
	0001,0201,0201,0001,0201,0001,0001,0201,	/* can - us */
	0200,0000,0000,0200,0000,0200,0200,0000,	/* sp - ' */
	0000,0200,0200,0000,0200,0000,0000,0200,	/* ( - / */
	0100,0300,0300,0100,0300,0100,0100,0300,	/* 0 - 7 */
	0300,0100,0000,0200,0000,0200,0200,0000,	/* 8 - ? */
	0200,0100,0100,0300,0100,0300,0300,0100,	/* @ - G */
	0100,0300,0300,0100,0300,0100,0100,0300,	/* H - O */
	0100,0300,0300,0100,0300,0100,0100,0300,	/* P - W */
	0300,0100,0100,0200,0000,0200,0200,0300,	/* X - _ */
	0000,0300,0300,0100,0300,0100,0100,0300,	/* ` - g */
	0300,0100,0100,0300,0100,0300,0300,0100,	/* h - o */
	0300,0100,0100,0300,0100,0300,0300,0100,	/* p - w */
	0100,0300,0300,0000,0200,0000,0000,0201,	/* x - del */
};

extern struct tty *constty;		/* temporary virtual console */
extern char partab[], maptab[];

/*
 * Is 'c' a line delimiter ("break" character)?
 */
#define ttbreakc(c) (c == '\n' || CCEQ(cc[VEOF], c) || \
		CCEQ(cc[VEOL], c) || CCEQ(cc[VEOL2], c))

/*
 * Debugging aids
 */
#define dprintf 	if (tp->t_trace & TTRACE_IO)printf

/*
 * Is 'c' a line delimiter ("break" character)?
 */
#define ttbreakc(c) (c == '\n' || CCEQ(cc[VEOF], c) || \
		CCEQ(cc[VEOL], c) || CCEQ(cc[VEOL2], c))

ttychars(tp)
	struct tty *tp;
{
	bcopy(ttydefchars, tp->t_cc, sizeof(ttydefchars));
}

/*
 *
 * Wait for output to drain, then flush input waiting.
 */
ttywflush(tp)
	register struct tty *tp;
{

	ttywait(tp);
	ttyflush(tp, FREAD);
}

/*
 * Wait for output to drain.
 */
/*
 * Wait for output to drain.
 */
ttywait(tp)
	register struct tty *tp;
{
	register int s = spltty();

	while ((tp->t_outq.c_cc || tp->t_state&TS_BUSY) &&
	    tp->t_state&TS_CARR_ON && tp->t_oproc) {
		(*tp->t_oproc)(tp);
		tp->t_state |= TS_ASLEEP;
		sleep((caddr_t)&tp->t_outq, TTOPRI);
	}
	splx(s);
}

/*
 * Flush all TTY queues
 */
ttyflush(tp, rw)
	register struct tty *tp;
{
	register s;

	s = spltty();
	if (rw & FREAD) {
		while (getc(&tp->t_canq) >= 0)
			;
		wakeup((caddr_t)&tp->t_rawq);
	}
	if (rw & FWRITE) {
		wakeup((caddr_t)&tp->t_outq);
		tp->t_state &= ~TS_TTSTOP;
		(*cdevsw[major(tp->t_dev)].d_stop)(tp, rw);
		while (getc(&tp->t_outq) >= 0)
			;
	}
	if (rw & FREAD) {
		while (getc(&tp->t_rawq) >= 0)
			;
		tp->t_rocount = 0;
		tp->t_rocol = 0;
		tp->t_state &= ~TS_LOCAL;
	}
	splx(s);
}

/*
 * Send stop character on input overflow.
 */
ttyblock(tp)
	register struct tty *tp;
{
	register x;

	x = tp->t_rawq.c_cc + tp->t_canq.c_cc;
	if (tp->t_rawq.c_cc > TTYHOG) {
		ttyflush(tp, FREAD|FWRITE);
		tp->t_state &= ~TS_TBLOCK;
	}
	/*
	 * Block further input iff:
	 * Current input > threshold AND input is available to user program
	 */
	if (x >= TTYHOG/2 && 
	    (!(tp->t_lflag&ICANON)) || (tp->t_canq.c_cc > 0) &&
	    tp->t_cc[VSTOP] != POSIX_V_DISABLE) {
		if (putc(tp->t_cc[VSTOP], &tp->t_outq)==0) {
			tp->t_state |= TS_TBLOCK;
			ttstart(tp);
		}
	}
}

/*
 * Restart typewriter output following a delay
 * timeout.
 * The name of the routine is passed to the timeout
 * subroutine and it is called during a clock interrupt.
 */
ttrstrt(tp)
	register struct tty *tp;
{

	if (tp == 0)
		panic("ttrstrt");
	tp->t_state &= ~TS_TIMEOUT;
	ttstart(tp);
}

/*
 * Start output on the typewriter. It is used from the top half
 * after some characters have been put on the output queue,
 * from the interrupt routine to transmit the next
 * character, and after a timeout has finished.
 */
ttstart(tp)
	register struct tty *tp;
{

	if (tp->t_oproc)		/* kludge for pty */
		(*tp->t_oproc)(tp);
}

/*
 * Common code for tty ioctls.
 */
/*ARGSUSED*/
ttioctl(tp, com, data, flag)
	register struct tty *tp;
	caddr_t data;
{
	extern int nldisp;
	int softset = 0;
	int soft;
	int s;


	/*
	 * If the ioctl involves modification,
	 * hang if in the background.
	 */
	switch (com) {

	case TIOCSETD: 
	case TIOCFLUSH:
	case TIOCSTI:
	case TIOCSWINSZ:
	case TIOCSETA:
	case TIOCSETAW:
	case TIOCSETAF:
	case TIOCSETAS:
	case TIOCSETAWS:
	case TIOCSETAFS:
		while (u.u_procp->p_pgid != tp->t_pgid &&
		   tp == u.u_ttyp &&
		   u.u_procp->p_pgrp->pg_jobc &&
		   (u.u_procp->p_flag&SVFORK) == 0 &&
		   !(u.u_procp->p_sigignore & sigmask(SIGTTOU)) &&
		   !(u.u_procp->p_sigmask & sigmask(SIGTTOU))) {
			pgsignal(u.u_procp->p_pgrp, SIGTTOU);
			sleep((caddr_t)&lbolt, TTOPRI);
		}
		break;
	}

	/*
	 * Process the ioctl.
	 */
	switch (com) {

	/* get discipline number */
	case TIOCGETD:
		*(int *)data = tp->t_line;
		break;

	/* set line discipline */
	case TIOCSETD: {
		register int t = *(int *)data;
		dev_t dev = tp->t_dev;
		dev_t dev = tp->t_dev;
		int error = 0;

		if ((unsigned)t >= nldisp)
			return (ENXIO);
		if (t != tp->t_line) {
			s = spltty();
			(*linesw[tp->t_line].l_close)(tp);
			error = (*linesw[t].l_open)(dev, tp);
			if (error) {
				(void)(*linesw[tp->t_line].l_open)(dev, tp);
				splx(s);
				return (error);
			}
			tp->t_line = t;
			splx(s);
		}
		if (tp->t_trace & TTRACE_STATE)
			ttytrace(com, tp);
		break;
	}

	/* prevent more opens on channel */
	case TIOCEXCL:
		tp->t_state |= TS_XCLUDE;
		break;

	case TIOCNXCL:
		tp->t_state &= ~TS_XCLUDE;
		break;

	case TIOCHPCL:
		tp->t_cflag |= HUPCL;
		break;

	case TIOCFLUSH: {
		register int flags = *(int *)data;

		if (flags == 0)
			flags = FREAD|FWRITE;
		else
			flags &= FREAD|FWRITE;
		ttyflush(tp, flags);
		break;
	}

	/* return number of characters immediately available */
	case FIONREAD:
		*(off_t *)data = ttnread(tp);
		break;

	case TIOCOUTQ:
		*(int *)data = tp->t_outq.c_cc;
		break;

	case TIOCSTOP:
		s = spltty();
		if ((tp->t_state&TS_TTSTOP) == 0) {
			tp->t_state |= TS_TTSTOP;
			(*cdevsw[major(tp->t_dev)].d_stop)(tp, 0);
		}
		splx(s);
		break;

	case TIOCSTART:
		s = spltty();
		if ((tp->t_state&TS_TTSTOP) || (tp->t_lflag&FLUSHO)) {
			tp->t_state &= ~TS_TTSTOP;
			tp->t_lflag &= ~FLUSHO;
			ttstart(tp);
		}
		splx(s);
		break;

	/*
	 * Simulate typing of a character at the terminal.
	 */
	case TIOCSTI:
		if (u.u_uid && (flag & FREAD) == 0)
			return (EPERM);
		if (u.u_uid && u.u_ttyp != tp)
			return (EACCES);
		(*linesw[tp->t_line].l_rint)(*(char *)data, tp);
		break;

	case TIOCGETA: {
		struct termios *t = (struct termios *)data;
		bcopy(&tp->t_termio, t, sizeof(struct termios));
		if (tp->t_trace & TTRACE_STATE)
			ttytrace(com, tp);
		break;
	}

		if (com == TIOCSETAF || com == TIOCSETAFS) 
			ttywflush(tp);
		else {
			if (com == TIOCSETAW || com == TIOCSETAWS)
				ttywait(tp);
			if ((t->c_lflag&ICANON) != (tp->t_lflag&ICANON))
				if (t->c_lflag&ICANON) {	
					tp->t_lflag |= PENDIN;
					ttwakeup(tp);
				}
				else {
					struct clist tq;

					catq(&tp->t_rawq, &tp->t_canq);
					tq = tp->t_rawq;
					tp->t_rawq = tp->t_canq;
					tp->t_canq = tq;
				}
		}
		tp->t_iflag = t->c_iflag;
		tp->t_oflag = t->c_oflag;
		tp->t_lflag = t->c_lflag;
		bcopy(t->c_cc, tp->t_cc, sizeof(t->c_cc));
		if (!soft) {
			tp->t_cflag = t->c_cflag;
			tp->t_ispeed = t->c_ispeed;
			tp->t_ospeed = t->c_ospeed;
		}
		tp->t_iflag = t->c_iflag;
		tp->t_oflag = t->c_oflag;
		tp->t_lflag = t->c_lflag;
		bcopy(t->c_cc, tp->t_cc, sizeof(t->c_cc));
		splx(s);
		if (tp->t_trace & TTRACE_STATE)
			ttytrace(com, tp);
		break;
	}

	case FIONBIO:
		if (*(int *)data)
			tp->t_state |= TS_NBIO;
		else
			tp->t_state &= ~TS_NBIO;
		break;

	case FIOASYNC:
		if (*(int *)data)
			tp->t_state |= TS_ASYNC;
		else
			tp->t_state &= ~TS_ASYNC;
		break;

	/*
	 * Set terminal process group.
	 */
	case TIOCSPGRP: {
		register struct proc *p = u.u_procp;
		register struct pgrp *pgrp = pgfind(*(int *)data);

		if (u.u_uid && 
		    (tp != u.u_ttyp ||
		    (pgrp && pgrp->pg_session != p->p_session))) {
			if (u.u_ttyp == NULL)
				return (ENOTTY);
			else
				return (EPERM);
		}
		tp->t_pgid = *(int *)data;
		break;
	}

	case TIOCGPGRP:
		*(int *)data = tp->t_pgid;
		break;

	case TIOCSWINSZ:
		if (bcmp((caddr_t)&tp->t_winsize, data,
		    sizeof (struct winsize))) {
			tp->t_winsize = *(struct winsize *)data;
			gsignal(tp->t_pgid, SIGWINCH);
		}
		break;

	case TIOCGWINSZ:
		*(struct winsize *)data = tp->t_winsize;
		break;

	case TIOCCONS:
		if (*(int *)data) {
			if (constty != NULL)
				return (EBUSY);
#ifndef	UCONSOLE
			if (!suser())
				return (EPERM);
#endif
			constty = tp;
		} else if (tp == constty)
			constty = NULL;
		break;

#ifdef COMPAT_43
	case TIOCGETP:
	case TIOCSETP:
	case TIOCSETN:
	case TIOCGETC:
	case TIOCSETC:
	case TIOCSLTC:
	case TIOCGLTC:
	case TIOCLBIS:
	case TIOCLBIC:
	case TIOCLSET:
	case TIOCLGET:
	case TIOCGETDCOMPAT:
	case TIOCSETDCOMPAT:
		return(ttcompat(tp, com, data, flag));
#endif

	/* allow old ioctls for now */
	case TIOCGETP:
	case TIOCSETP:
	case TIOCSETN:
	case TIOCGETC:
	case TIOCSETC:
	case TIOCSLTC:
	case TIOCGLTC:
	case TIOCLBIS:
	case TIOCLBIC:
	case TIOCLSET:
	case TIOCLGET:
		return(ottioctl(tp, com, data, flag));

	default:
		return (-1);
	}
	return (0);
}

/*
 * DEBUG - to be removed
 */
ttytrace(ioc, tp)
	struct tty *tp;
{
	register u_char *cc = tp->t_cc;
	char comm[MAXCOMLEN+1];
	static int seq = 0;
	
	bcopy(u.u_comm, comm, MAXCOMLEN+1);
	comm[MAXCOMLEN] = '\0';

	/* trace changes to line disciplines */
	if (ioc == TIOCSETD) {
		log(LOG_LOCAL4|LOG_DEBUG, "%s:%x:%x:%x:%x\n",
		    comm, ioc, u.u_procp->p_pid, tp->t_dev, tp->t_line);
		return;
	}

	/*
	 * format for the trace record is:
	 *
	 * u_comm:ioctl:pid:dev_t:ldisc:iflag:oflag:lflag:cflag:ispeed:
	 *    ospeed:cc's...:seq
	 *
	 * u_comm is a string and all other values are hex. "cc's..."
	 * stands for control chars 0 through NCC-1.  seq is a sequence #
	 * to force syslogd to log every entry (rather than hold them to
	 * print "last message repeated...".
	 */
	log(LOG_LOCAL4|LOG_DEBUG, "%s:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x:%x\n",
		comm, ioc, u.u_procp->p_pid, tp->t_dev, tp->t_line,
		tp->t_iflag, tp->t_oflag, tp->t_lflag, tp->t_cflag,
		tp->t_ispeed, tp->t_ospeed, cc[0], cc[1], cc[2], cc[3],
		cc[4], cc[5], cc[6], cc[7], cc[8], cc[9], cc[10], cc[11],
		cc[12], cc[13], cc[14], cc[15], cc[16], cc[17], cc[18], 
		cc[19], seq++);
}

ttnread(tp)
	struct tty *tp;
{
	int nread = 0;

	if (tp->t_lflag & PENDIN)
		ttypend(tp);
	nread = tp->t_canq.c_cc;
	if (tp->t_lflag & ICANON == 0)
		nread += tp->t_rawq.c_cc;
	return (nread);
}

ttselect(dev, rw)
	dev_t dev;
	int rw;
{
	register struct tty *tp = &cdevsw[major(dev)].d_ttys[minor(dev)];
	int nread;
	int s = spltty();

	switch (rw) {

	case FREAD:
		nread = ttnread(tp);
		if (nread > 0 || (tp->t_state & TS_CARR_ON) == 0)
			goto win;
		if (tp->t_rsel && tp->t_rsel->p_wchan == (caddr_t)&selwait)
			tp->t_state |= TS_RCOLL;
		else
			tp->t_rsel = u.u_procp;
		break;

	case FWRITE:
		if (tp->t_outq.c_cc <= tp->t_lowat)
			goto win;
		if (tp->t_wsel && tp->t_wsel->p_wchan == (caddr_t)&selwait)
			tp->t_state |= TS_WCOLL;
		else
			tp->t_wsel = u.u_procp;
		break;
	}
	splx(s);
	return (0);
win:
	splx(s);
	return (1);
}

/*
 * Initial open of tty, or (re)entry to line discipline.
 */
ttyopen(dev, tp)
	dev_t dev;
	register struct tty *tp;
{
	register struct proc *pp;

	tp->t_dev = dev;

	tp->t_state &= ~TS_WOPEN;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_ISOPEN;
		bzero((caddr_t)&tp->t_winsize, sizeof(tp->t_winsize));
		/*
		 * CHANGE: used to do a ttywflush() if it was the
		 *  old (old) line discipline.
		 */
	}
	return (0);
}

/*
 * "close" a line discipline
 */
ttylclose(tp)
	register struct tty *tp;
{

	ttywflush(tp);
}

/*
 * clean tp on last close
 */
ttyclose(tp)
	register struct tty *tp;
{
	if (constty == tp)
		constty = NULL;
	ttyflush(tp, FREAD|FWRITE);
	tp->t_pgid = 0;
	tp->t_state = 0;
}

/*
 * Handle modem control transition on a tty.
 * Flag indicates new state of carrier.
 * Returns 0 if the line should be turned off, otherwise 1.
 */
ttymodem(tp, flag)
	register struct tty *tp;
{

	if ((tp->t_state&TS_WOPEN) == 0 && (tp->t_lflag & MDMBUF)) {
		/*
		 * MDMBUF: do flow control according to carrier flag
		 */
		if (flag) {
			tp->t_state &= ~TS_TTSTOP;
			ttstart(tp);
		} else if ((tp->t_state&TS_TTSTOP) == 0) {
			tp->t_state |= TS_TTSTOP;
			(*cdevsw[major(tp->t_dev)].d_stop)(tp, 0);
		}
	} else if (flag == 0) {
		/*
		 * Lost carrier.
		 */
		tp->t_state &= ~TS_CARR_ON;
		if (tp->t_state & TS_ISOPEN) {
			if ((tp->t_lflag & NOHANG) == 0) {
				gsignal(tp->t_pgid, SIGHUP);
				gsignal(tp->t_pgid, SIGCONT);
				ttyflush(tp, FREAD|FWRITE);
				return (0);
			}
		}
	} else {
		/*
		 * Carrier now on.
		 */
		tp->t_state |= TS_CARR_ON;
		wakeup((caddr_t)&tp->t_rawq);
	}
	return (1);
}

/*
 * Default modem control routine (for other line disciplines).
 * Return argument flag, to turn off device on carrier drop.
 */
nullmodem(tp, flag)
	register struct tty *tp;
	int flag;
{
	
	if (flag)
		tp->t_state |= TS_CARR_ON;
	else
		tp->t_state &= ~TS_CARR_ON;
	return (flag);
}

/*
 * reinput pending characters after state switch
 * call at spltty().
 */
ttypend(tp)
	register struct tty *tp;
{
	struct clist tq;
	register c;

	tp->t_lflag &= ~PENDIN;
	tp->t_state |= TS_TYPEN;
	tq = tp->t_rawq;
	tp->t_rawq.c_cc = 0;
	tp->t_rawq.c_cf = tp->t_rawq.c_cl = 0;
	while ((c = getc(&tq)) >= 0)
		ttyinput(c, tp);
	tp->t_state &= ~TS_TYPEN;
}

/*
 *
 *
 * Place a character on raw TTY input queue,
 * putting in delimiters and waking up top
 * half as needed.  Also echo if required.
 * The arguments are the character and the
 * appropriate tty structure.
 */
ttyinput(c, tp)
	register c;
	register struct tty *tp;
{
	register int iflag = tp->t_iflag;
	register int lflag = tp->t_lflag;
	register u_char *cc = tp->t_cc;
	int i, err;

	/*
	 * If input is pending take it first.
	 */
	if (lflag&PENDIN)
		ttypend(tp);
	/*
	 * Gather stats.
	 */

	tk_nin++;
	if (lflag&ICANON) {
		tk_cancc++;
		tp->t_cancc++;
	} else {
		tk_rawcc++;
		tp->t_rawcc++;
	}
	/*
	 * Handle exceptional conditions (break, parity, framing).
	 */
	if (err = (c&TTY_ERRORMASK)) {
		c &= TTY_CHARMASK;
		if (err&TTY_FE && !c) {		/* break */
			if (iflag&IGNBRK)
				goto endcase;
			else if (iflag&BRKINT && lflag&ISIG && 
				(cc[VINTR] != POSIX_V_DISABLE))
				c = cc[VINTR];
			else
				c = 0;
		} else if ((err&TTY_PE && iflag&INPCK) || err&TTY_FE) {
			if (iflag&IGNPAR)
				goto endcase;
			else if (iflag&PARMRK) {
				ttyinput(0377, tp);
				ttyinput(0, tp);
			} else
				c = 0;
		}
	}
	dprintf("<%o>\n", c);

	/*
	 * In tandem mode, check high water mark.
	 */
	if (iflag&IXOFF)
		ttyblock(tp);

	/*
	 * In tandem mode, check high water mark.
	 */
	if ((tp->t_state&TS_TYPEN) == 0 && (iflag&ISTRIP))
		c &= 0177;
	/*
	 * Check for literal nexting very first
	 */
	if (tp->t_state&TS_LNCH) {
		c |= TTY_QUOTE;
		tp->t_state &= ~TS_LNCH;
	}
	/*
	 * Scan for special characters.  This code
	 * is really just a big case statement with
	 * non-constant cases.  The bottom of the
	 * case statement is labeled ``endcase'', so goto
	 * it after a case match, or similar.
	 */

	/*
	 * Extensions to POSIX input modes which aren't controlled
	 * by ICANON, ISIG, or IXON.
	 */
	if (iflag&IEXTEN) {
		if (CCEQ(cc[VLNEXT],c) && (iflag&ISTRIP)) {
			if (lflag&ECHO)
				ttyout("^\b", tp); /*XXX - presumes too much */
			tp->t_state |= TS_LNCH;
			goto endcase;
		}
		if (CCEQ(cc[VFLUSHO],c)) {
			if (lflag&FLUSHO)
				tp->t_lflag &= ~FLUSHO;
			else {
				ttyflush(tp, FWRITE);
				ttyecho(c, tp);
				if (tp->t_rawq.c_cc + tp->t_canq.c_cc)
					ttyretype(tp);
				tp->t_lflag |= FLUSHO;
			}
			goto startoutput;
		}
	}

	/*
	 * Signals.
	 */
	if (lflag&ISIG) {
		if (CCEQ(cc[VINTR], c) || CCEQ(cc[VQUIT], c)) {
			if ((lflag&NOFLSH) == 0)
				ttyflush(tp, FREAD|FWRITE);
			ttyecho(c, tp);
			gsignal(tp->t_pgrp, CCEQ(cc[VINTR],c) ? 
				SIGINT : SIGQUIT);
			goto endcase;
		}
		if (CCEQ(cc[VSUSP],c)) {
			if ((lflag&NOFLSH) == 0)
				ttyflush(tp, FREAD);
			ttyecho(c, tp);
			gsignal(tp->t_pgid, SIGTSTP);
			goto endcase;
		}
	}
	/*
	 * Handle start/stop characters.
	 */
	if (iflag&IXON) {
		if (CCEQ(cc[VSTOP],c)) {
			if ((tp->t_state&TS_TTSTOP) == 0) {
				tp->t_state |= TS_TTSTOP;
				(*cdevsw[major(tp->t_dev)].d_stop)(tp, 0);
				return;
			}
			if (!CCEQ(cc[VSTART], c))
				return;
			/* 
			 * if VSTART == VSTOP we toggle 
			 */
			goto endcase;
		}
		if (CCEQ(cc[VSTART], c))
			goto restartoutput;
	}
	/*
	 * IGNCR, ICRNL, & INLCR
	 */
	if (c == '\r') {
		if (iflag&IGNCR)
			goto endcase;
		else if (iflag&ICRNL)
			c = '\n';
	}
	else if (c == '\n' && iflag&INLCR)
		c = '\r';
	else if (c == '\n' && iflag&INLCR)
		c = '\r';
	/*
	 * Non canonical mode, don't process line editing
	 * characters; check high water mark for wakeup.
	 * 
	 * 
	 */
	if (!(lflag&ICANON)) {
		if (tp->t_rawq.c_cc > TTYHOG) {
			if (iflag&IMAXBEL) {
				if (tp->t_outq.c_cc < tp->t_hiwat)
					(void) ttyoutput(CTRL('g'), tp);
			} else
				ttyflush(tp, FREAD | FWRITE);
		} else {
			if (putc(c, &tp->t_rawq) >= 0) {
				ttwakeup(tp);
				ttyecho(c, tp);
			}
		}
		goto endcase;
	}
	/*
	 * From here on down canonical mode character
	 * processing takes place.
	 */

	/* 
	 * Oldstyle quoting of erase, kill, and eof chars.
	 *
	 * Historically is '\' , but can be changed (read: disabled)
	 * with the VQUOTE subscript.
	 */

	/*
	 * erase (^H / ^?)
	 */
	if (CCEQ(cc[VERASE], c)) {
		if (tp->t_rawq.c_cc)
			ttyrub(unputc(&tp->t_rawq), tp);
		goto endcase;
	}
	/*
	 * kill (^X / ^U)
	 */
	if (CCEQ(cc[VKILL], c)) {
		if (lflag&ECHOKE &&
		    tp->t_rawq.c_cc == tp->t_rocount) {
			while (tp->t_rawq.c_cc)
				ttyrub(unputc(&tp->t_rawq), tp);
		} else {
			ttyecho(c, tp);
			if (lflag&ECHOK || lflag&ECHOKE)
				ttyecho('\n', tp);
			while (getc(&tp->t_rawq) > 0)
				;
			tp->t_rocount = 0;
		}
		tp->t_state &= ~TS_LOCAL;
		goto endcase;
	}
	/*
	 * word erase (^W)
	 */
			c = unputc(&tp->t_rawq);
		} while (c != ' ' && c != '\t');
		(void) putc(c, &tp->t_rawq);
		goto endcase;
	}
	/*
	 * reprint line (^R)
	 */
	if (CCEQ(cc[VREPRINT], c)) {
		ttyretype(tp);
		goto endcase;
	}
	/*
	 * reprint line (^R)
	 */
	if (CCEQ(cc[VREPRINT], c)) {
		ttyretype(tp);
		goto endcase;
	}
	/*
	 * Check for input buffer overflow
	 */
	if (tp->t_rawq.c_cc+tp->t_canq.c_cc >= TTYHOG) {
		if (iflag&IMAXBEL) {
			if (tp->t_outq.c_cc < TTHIWAT(tp))
				(void) ttyoutput(CTRL('g'), tp);
		} else
			ttyflush(tp, FREAD | FWRITE);
		goto endcase;
	}
	/*
	 * Put data char in q for user and
	 * wakeup on seeing a line delimiter.
	 */
	if (putc(c, &tp->t_rawq) >= 0) {
		if (ttbreakc(c)) {
			tp->t_rocount = 0;
			catq(&tp->t_rawq, &tp->t_canq);
			ttwakeup(tp);
		} else if (tp->t_rocount++ == 0)
			tp->t_rocol = tp->t_col;
		if (CCEQ(cc[VQUOTE], c) && (iflag&ISTRIP))
			tp->t_state |= TS_QUOT;	/* '\' escape */
		if (tp->t_state&TS_ERASE) {
			/*
			 * end of prterase \.../
			 */
			/*
			 * end of prterase \.../
			 */
			tp->t_state &= ~TS_ERASE;
			(void) ttyoutput('/', tp);
		}
		i = tp->t_col;
		ttyecho(c, tp);
		if (CCEQ(cc[VEOF], c) && lflag&ECHO) {
			/*
			 * Place the cursor over the '^' of the ^D.
			 */
			i = MIN(2, tp->t_col - i);
			while (i > 0) {
				(void) ttyoutput('\b', tp);
				i--;
			}
		}
	}
endcase:
	/*
	 * IXANY means allow any character to restart output.
	 */
	if (tp->t_state&TS_TTSTOP && (iflag&IXANY == 0) 
	    && cc[VSTART] != cc[VSTOP])
		return;
	
restartoutput:
	tp->t_state &= ~TS_TTSTOP;
	tp->t_lflag &= ~FLUSHO;
startoutput:
	ttstart(tp);
}

/*
 * Put character on TTY output queue, adding delays,
 * expanding tabs, and handling the CR/NL bit.
 * This is called both from the top half for output,
 * and from interrupt level for echoing.
 * The arguments are the character and the tty structure.
 * Returns < 0 if putc succeeds, otherwise returns char to resend
 * Must be recursive.
 */
ttyoutput(c, tp)
	register c;
	register struct tty *tp;
{
	register char *colp;
	register ctype;
	if (!(tp->t_oflag&OPOST)) {
		if (tp->t_lflag&FLUSHO) 
			return (-1);
		if (putc(c, &tp->t_outq))
			return (c);
		tk_nout++;
		tp->t_outcc++;
		return (-1);
	}
	c &= 0377;
	/*
	 * Turn tabs to spaces as required
	 */
	if (c == '\t' && tp->t_oflag&OXTABS ) {
		register int s;

		c = 8 - (tp->t_col&7);
		if ((tp->t_lflag&FLUSHO) == 0) {
			s = spltty();		/* don't interrupt tabs */
			c -= b_to_q("        ", c, &tp->t_outq);
			tk_nout += c;
			tp->t_outcc += c;
			splx(s);
		}
		tp->t_col += c;
		return (c ? -1 : '\t');
	}
	if (c == CEOT && oflag&ONOEOT)
		return(-1);
	tk_nout++;
	tp->t_outcc++;
#ifdef notdef
	/*
	 * turn <nl> to <cr><lf> if desired.
	 */
#endif
	if ((tp->t_lflag&FLUSHO) == 0 && putc(c, &tp->t_outq))
		return (c);
	if ((tp->t_lflag&FLUSHO) == 0 && putc(c, &tp->t_outq))
		return (c);
	/*
	 * Calculate delays.
	 * The numbers here represent clock ticks
	 * and are not necessarily optimal for all terminals.
	 *
	 * SHOULD JUST ALLOW USER TO SPECIFY DELAYS
	 *
	 * (actually, should THROW AWAY terminals which need delays)
	 */
	colp = &tp->t_col;
	ctype = partab[c];
	c = 0;
	switch (ctype&077) {

	case ORDINARY:
		(*colp)++;

	case CONTROL:
		break;

	case BACKSPACE:
		if (*colp)
			(*colp)--;
		break;

	/*
	 * This macro is close enough to the correct thing;
	 * it should be replaced by real user settable delays
	 * in any event...
	 */
#define	mstohz(ms)	(((ms) * hz) >> 10)
	case NEWLINE:
		ctype = (tp->t_flags >> 8) & 03;
		if (ctype == 1) { /* tty 37 */
			if (*colp > 0) {
				c = (((unsigned)*colp) >> 4) + 3;
				if ((unsigned)c > 6)
					c = 6;
			}
		} else if (ctype == 2) /* vt05 */
			c = mstohz(100);
		*colp = 0;
		break;

	case TAB:
		ctype = (tp->t_flags >> 10) & 03;
		if (ctype == 1) { /* tty 37 */
			c = 1 - (*colp | ~07);
			if (c < 5)
				c = 0;
		}
		*colp |= 07;
		(*colp)++;
		break;

	case VTAB:
		if (tp->t_flags&VTDELAY) /* tty 37 */
			c = 0177;
		break;

	case RETURN:
		ctype = (tp->t_flags >> 12) & 03;
		if (ctype == 1) /* tn 300 */
			c = mstohz(83);
		else if (ctype == 2) /* ti 700 */
			c = mstohz(166);
		else if (ctype == 3) { /* concept 100 */
			int i;

			if ((i = *colp) >= 0)
				for (; i < 9; i++)
					(void) putc(0177, &tp->t_outq);
		}
		*colp = 0;
	}
	if (c && (tp->t_lflag&FLUSHO) == 0)
		(void) putc(c|TTY_QUOTE, &tp->t_outq);
	return (-1);
}
#undef mstohz

/*
 * Called from device's read routine after it has
 * calculated the tty-structure given as argument.
 */
ttread(tp, uio)
	register struct tty *tp;
	struct uio *uio;
{
	register struct clist *qp;
	register int c, t_flags;
	register long lflag = tp->t_lflag;
	register long iflag = tp->t_iflag;
	register u_char *cc = tp->t_cc;
	int s, first, error = 0;


loop:
	/*
	 * Take any pending input first.
	 */
	s = spltty();
	if (tp->t_lflag&PENDIN)
		ttypend(tp);
	splx(s);
	if ((tp->t_state&TS_CARR_ON)==0)
		return (EIO);
	/*
	 * Hang process if it's in the background.
	 */
	if (u.u_ttyp == tp && u.u_procp->p_pgid != tp->t_pgid) {
		if ((u.u_procp->p_sigignore & sigmask(SIGTTIN)) ||
		   (u.u_procp->p_sigmask & sigmask(SIGTTIN)) ||
		    u.u_procp->p_flag&SVFORK || u.u_procp->p_pgrp->pg_jobc == 0)
			return (EIO);
		pgsignal(u.u_procp->p_pgrp, SIGTTIN);
		sleep((caddr_t)&lbolt, TTIPRI);
		goto loop;
	}
	/*
	 * If canonical, use the canonical queue,
	 * else use the raw queue.
	 */
	qp = lflag&ICANON ? &tp->t_canq : &tp->t_rawq;
	/*
	 * No input, sleep on rawq awaiting hardware
	 * receipt and notification.
	 */
	s = spltty();
	if (qp->c_cc <= 0) {
		if ((tp->t_state&TS_CARR_ON) == 0 ||
		    (tp->t_state&TS_NBIO)) {
			splx(s);
			return (EWOULDBLOCK);
		}
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
		splx(s);
		goto loop;
	}
	splx(s);
	/*
	 * Input present, check for input mapping and processing.
	 */
	first = 1;
	while ((c = getc(qp)) >= 0) {
		/*
		 * delayed suspend (^Y)
		 */
		if (CCEQ(cc[VDSUSP], c) && lflag&ISIG) {
			gsignal(tp->t_pgid, SIGTSTP);
			if (first) {
				sleep((caddr_t)&lbolt, TTIPRI);
				goto loop;
			}
			break;
		}
		/*
		 * Interpret EOF only in canonical mode.
		 */
		if (CCEQ(cc[VEOF], c) && lflag&ICANON)
			break;
		/*
		 * Give user character.
		 */
 		error = ureadc(iflag&ISTRIP ? c & 0177 : c , uio);
		if (error)
			break;
 		if (uio->uio_resid == 0)
			break;
		/*
		 * In canonical mode check for a "break character"
		 * marking the end of a "line of input".
		 */
		if (lflag&ICANON && ttbreakc(c)) {
			break;
		}
		}
		first = 0;
	}
checktandem:
	/*
	 * Look to unblock output now that (presumably)
	 * the input queue has gone down.
	 */
	if (tp->t_state&TS_TBLOCK && tp->t_rawq.c_cc < TTYHOG/5) {
		if (cc[VSTART] != POSIX_V_DISABLE 
		   && putc(cc[VSTART], &tp->t_outq) == 0) {
			tp->t_state &= ~TS_TBLOCK;
			ttstart(tp);
		}
	}
	}
	return (error);
}

/*
 * Check the output queue on tp for space for a kernel message
 * (from uprintf/tprintf).  Allow some space over the normal
 * hiwater mark so we don't lose messages due to normal flow
 * control, but don't let the tty run amok.
 * Sleeps here are not interruptible, but we return prematurely
 * if new signals come in.
 */
ttycheckoutq(tp, wait)
	register struct tty *tp;
	int wait;
{
	int hiwat, s, oldsig;

	hiwat = tp->t_hiwat;
	s = spltty();
	oldsig = u.u_procp->p_sig;
	if (tp->t_outq.c_cc > hiwat + 200)
		while (tp->t_outq.c_cc > hiwat) {
			ttstart(tp);
			if (wait == 0 || u.u_procp->p_sig != oldsig) {
				splx(s);
				return (0);
			}
			timeout(wakeup, (caddr_t)&tp->t_outq, hz);
			tp->t_state |= TS_ASLEEP;
			sleep((caddr_t)&tp->t_outq, PZERO - 1);
		}
	splx(s);
	return (1);
}

/*
 * Called from the device's write routine after it has
 * calculated the tty-structure given as argument.
 */
ttwrite(tp, uio)
	register struct tty *tp;
	register struct uio *uio;
{
	register char *cp;
	register int cc, ce, c;
	int i, hiwat, cnt, error, s;
	char obuf[OBUFSIZ];

	hiwat = tp->t_hiwat;
	cnt = uio->uio_resid;
	error = 0;
loop:
	if ((tp->t_state&TS_CARR_ON) == 0)
		return (EIO);
	/*
	 * Hang the process if it's in the background.
	 */
	    (tp->t_lflag&TOSTOP) && (u.u_procp->p_flag&SVFORK)==0 &&
	    !(u.u_procp->p_sigignore & sigmask(SIGTTOU)) &&
	    !(u.u_procp->p_sigmask & sigmask(SIGTTOU)) &&
	     u.u_procp->p_pgrp->pg_jobc) {
		pgsignal(u.u_procp->p_pgrp, SIGTTOU);
		sleep((caddr_t)&lbolt, TTIPRI);
		goto loop;
	}
	/*
	 * Process the user's data in at most OBUFSIZ
	 * chunks.  Perform lower case simulation and
	 * similar hacks.  Keep track of high water
	 * mark, sleep on overflow awaiting device aid
	 * in acquiring new space.
	 */
	while (uio->uio_resid > 0) {
		if (tp->t_outq.c_cc > hiwat) {
			cc = 0;
			goto ovhiwat;
		}
		/*
		 * Grab a hunk of data from the user.
		 */
		cc = uio->uio_iov->iov_len;
		if (cc == 0) {
			uio->uio_iovcnt--;
			uio->uio_iov++;
			if (uio->uio_iovcnt <= 0)
				panic("ttwrite");
			continue;
		}
		if (cc > OBUFSIZ)
			cc = OBUFSIZ;
		cp = obuf;
		error = uiomove(cp, cc, UIO_WRITE, uio);
		if (error)
			break;
		if (tp->t_lflag&FLUSHO)
			continue;
#ifdef notdef
		/*
		 * If nothing fancy need be done, grab those characters we
		 * can handle without any of ttyoutput's processing and
		 * just transfer them to the output q.  For those chars
		 * which require special processing (as indicated by the
		 * bits in partab), call ttyoutput.  After processing
		 * a hunk of data, look for FLUSHO so ^O's will take effect
		 * immediately.
		 */
		while (cc > 0) {
			if (!(tp->t_oflag&OPOST))
				ce = cc;
			else {
				ce = cc - scanc((unsigned)cc, (u_char *)cp,
				   (u_char *)partab, 077);
				/*
				 * If ce is zero, then we're processing
				 * a special character through ttyoutput.
				 */
				if (ce == 0) {
					tp->t_rocount = 0;
					if (ttyoutput(*cp, tp) >= 0) {
					    /* no c-lists, wait a bit */
					    ttstart(tp);
					    sleep((caddr_t)&lbolt, TTOPRI);
					    if (cc != 0) {
					        uio->uio_iov->iov_base -= cc;
					        uio->uio_iov->iov_len += cc;
					        uio->uio_resid += cc;
						uio->uio_offset -= cc;
					    }
					    goto loop;
					}
					cp++, cc--;
					if (tp->t_lflag&FLUSHO ||
					    tp->t_outq.c_cc > hiwat)
						goto ovhiwat;
					continue;
				}
			}
			/*
			 * A bunch of normal characters have been found,
			 * transfer them en masse to the output queue and
			 * continue processing at the top of the loop.
			 * If there are any further characters in this
			 * <= OBUFSIZ chunk, the first should be a character
			 * requiring special handling by ttyoutput.
			 */
			tp->t_rocount = 0;
			i = b_to_q(cp, ce, &tp->t_outq);
			ce -= i;
			tp->t_col += ce;
			cp += ce, cc -= ce, tk_nout += ce;
			tp->t_outcc += ce;
			if (i > 0) {
				/* out of c-lists, wait a bit */
				ttstart(tp);
				sleep((caddr_t)&lbolt, TTOPRI);
				uio->uio_iov->iov_base -= cc;
				uio->uio_iov->iov_len += cc;
				uio->uio_resid += cc;
				uio->uio_offset -= cc;
				goto loop;
			}
			if (tp->t_lflag&FLUSHO || tp->t_outq.c_cc > hiwat)
				goto ovhiwat;
		}
		ttstart(tp);
		ttstart(tp);
	}
	return (error);
ovhiwat:
	if (cc != 0) {
		uio->uio_iov->iov_base -= cc;
		uio->uio_iov->iov_len += cc;
		uio->uio_resid += cc;
		uio->uio_offset -= cc;
	}
	ttstart(tp);
	s = spltty();
	/*
	 * This can only occur if FLUSHO is set in t_lflag,
	 * or if ttstart/oproc is synchronous (or very fast).
	 */
	if (tp->t_outq.c_cc <= hiwat) {
		splx(s);
		goto loop;
	}
	if (tp->t_state&TS_NBIO) {
		splx(s);
		if (uio->uio_resid == cnt)
			return (EWOULDBLOCK);
		return (0);
	}
	tp->t_state |= TS_ASLEEP;
	sleep((caddr_t)&tp->t_outq, TTOPRI);
	splx(s);
	goto loop;
}

/*
 * Rubout one character from the rawq of tp
 * as cleanly as possible.
 */
ttyrub(c, tp)
	register c;
	register struct tty *tp;
{
	register char *cp;
	register int savecol;
	int s;
	char *nextc();

	if ((tp->t_lflag&ECHO) == 0)
		return;
	if (tp->t_lflag&ECHOE) {
		if (tp->t_rocount == 0) {
			/*
			 * Screwed by ttwrite; retype
			 */
			ttyretype(tp);
			return;
		}
		/* if tab or newline was escaped  - XXX - not 8bit */
		if (c == ('\t'|TTY_QUOTE) || c == ('\n'|TTY_QUOTE))
			ttyrubo(tp, 2);
		else switch (partab[c&=0377]&077) {

		case ORDINARY:
#ifdef notdef
			ttyrubo(tp, 1);
			break;

		case VTAB:
		case BACKSPACE:
		case CONTROL:
		case RETURN:
			if (tp->t_lflag&ECHOCTL)
				ttyrubo(tp, 2);
			break;

		case TAB: {
			int c;

			if (tp->t_rocount < tp->t_rawq.c_cc) {
				ttyretype(tp);
				return;
			}
			s = spltty();
			savecol = tp->t_col;
			tp->t_state |= TS_CNTTB;
			tp->t_lflag |= FLUSHO;
			tp->t_col = tp->t_rocol;
			cp = tp->t_rawq.c_cf;
			tp->t_lflag &= ~FLUSHO;
			tp->t_state &= ~TS_CNTTB;
			splx(s);
			/*
			 * savecol will now be length of the tab
			 */
			savecol -= tp->t_col;
			tp->t_col += savecol;
			if (savecol > 8)
				savecol = 8;		/* overflow screw */
			while (--savecol >= 0)
				(void) ttyoutput('\b', tp);
			break;
		}

		default:
			printf("ttyrub: would panic c = %d, val = %d\n",
				c, partab[c&=0377]&077);
			/*panic("ttyrub");*/
		}
	} else if (tp->t_lflag&ECHOPRT) {
		if ((tp->t_state&TS_ERASE) == 0) {
			(void) ttyoutput('\\', tp);
			tp->t_state |= TS_ERASE;
		}
		ttyecho(c, tp);
	} else
		ttyecho(tp->t_cc[VERASE], tp);
	tp->t_rocount--;
}

/*
 * Crt back over cnt chars perhaps
 * erasing them.
 */
ttyrubo(tp, cnt)
	register struct tty *tp;
	int cnt;
{
	register char *rubostring = tp->t_lflag&ECHOE ? "\b \b" : "\b";

	while (--cnt >= 0)
		ttyout("\b \b", tp);
}

/*
 * Reprint the rawq line.
 * We assume c_cc has already been checked.
 */
ttyretype(tp)
	register struct tty *tp;
{
	register char *cp;
	char *nextc();
	int s, c;

	if (tp->t_cc[VREPRINT] != POSIX_V_DISABLE)
		ttyecho(tp->t_cc[VREPRINT], tp);
	(void) ttyoutput('\n', tp);
	s = spltty();
	/*** XXX *** FIX *** NEXTC IS BROKEN - DOESN'T CHECK QUOTE
	  BIT OF FIRST CHAR ****/
	for (cp = tp->t_canq.c_cf, c=(cp?*cp:0); cp; cp = nextc(&tp->t_canq, cp, &c)) {
		ttyecho(c, tp);
	}
	for (cp = tp->t_rawq.c_cf, c=(cp?*cp:0); cp; cp = nextc(&tp->t_rawq, cp, &c)) {
		ttyecho(c, tp);
	}
	tp->t_state &= ~TS_ERASE;
	splx(s);
	tp->t_rocount = tp->t_rawq.c_cc;
	tp->t_rocol = 0;
}

/*
 * Echo a typed character to the terminal.
 */
ttyecho(c, tp)
	register c;
	register struct tty *tp;
{
	c &= 0377;
	if ((tp->t_state&TS_CNTTB) == 0)
		tp->t_lflag &= ~FLUSHO;
	if ((tp->t_lflag&ECHO) == 0 && !(tp->t_lflag&ECHONL && c == '\n'))
		return;
	if (tp->t_lflag&ECHOCTL) {
		if ((c&TTY_CHARMASK)<=037 && c!='\t' && c!='\n' || c==0177) {
			(void) ttyoutput('^', tp);
			c &= TTY_CHARMASK;
			if (c == 0177)
				c = '?';
#ifdef notdef
#endif
			else
				c += 'A' - 1;
		}
	}
	(void) ttyoutput(c, tp);
 * send string cp to tp
 */
ttyout(cp, tp)
	register char *cp;
	register struct tty *tp;
{
	register char c;

	while (c = *cp++)
		(void) ttyoutput(c, tp);
}

ttwakeup(tp)
	struct tty *tp;
{

	if (tp->t_rsel) {
		selwakeup(tp->t_rsel, tp->t_state&TS_RCOLL);
		tp->t_state &= ~TS_RCOLL;
		tp->t_rsel = 0;
	}
	if (tp->t_state & TS_ASYNC)
		gsignal(tp->t_pgid, SIGIO); 
	wakeup((caddr_t)&tp->t_rawq);
}

/*
 * set tty hi and low water marks
 *
 * Try to arrange the dynamics so there's about one second
 * from hi to low water.
 * 
 */
ttsetwater(tp)
	struct tty *tp;
{
	register cps = tp->t_ospeed / 10;
	register x;

#define clamp(x, h, l) ((x)>h ? h : ((x)<l) ? l : (x))
	tp->t_lowat = x = clamp(cps/2, TTMAXLOWAT, TTMINLOWAT);
	x += cps;
	x = clamp(x, TTMAXHIWAT, TTMINHIWAT);
	tp->t_hiwat = roundup(x, CBSIZE);
#undef clamp
}

ttspeedtab(speed, table)
	struct speedtab table[];
{
	register int i;

	for (i = 0; table[i].sp_speed != -1; i++)
		if (table[i].sp_speed == speed)
			return(table[i].sp_code);
	return(-1);
}
