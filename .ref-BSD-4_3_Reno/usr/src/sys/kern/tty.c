/*
 * Copyright (c) 1982, 1986, 1990 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty.c	7.35 (Berkeley) 7/27/90
 */

#include "param.h"
#include "systm.h"
#include "user.h"
#include "ioctl.h"
#define TTYDEFCHARS
#include "tty.h"
#undef TTYDEFCHARS
#include "proc.h"
#include "file.h"
#include "conf.h"
#include "dkstat.h"
#include "uio.h"
#include "kernel.h"
#include "vnode.h"
#include "syslog.h"

#include "machine/reg.h"

/* symbolic sleep message strings */
char ttyin[] = "ttyin";
char ttyout[] = "ttyout";
char ttopen[] = "ttyopn";
char ttclos[] = "ttycls";
char ttybg[] = "ttybg";
char ttybuf[] = "ttybuf";

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
#define ttbreakc(c) ((c) == '\n' || ((c) == cc[VEOF] || \
	(c) == cc[VEOL] || (c) == cc[VEOL2]) && (c) != _POSIX_VDISABLE)

ttychars(tp)
	struct tty *tp;
{
	bcopy(ttydefchars, tp->t_cc, sizeof(ttydefchars));
}

/*
 * Wait for output to drain, then flush input waiting.
 */
ttywflush(tp)
	struct tty *tp;
{
	int error;

	if ((error = ttywait(tp)) == 0)
		ttyflush(tp, FREAD);
	return (error);
}

/*
 * Wait for output to drain.
 */
ttywait(tp)
	register struct tty *tp;
{
	int error = 0, s = spltty();

	while ((tp->t_outq.c_cc || tp->t_state&TS_BUSY) &&
	    (tp->t_state&TS_CARR_ON || tp->t_cflag&CLOCAL) && 
	    tp->t_oproc) {
		(*tp->t_oproc)(tp);
		tp->t_state |= TS_ASLEEP;
		if (error = ttysleep(tp, (caddr_t)&tp->t_outq, 
		    TTOPRI | PCATCH, ttyout, 0))
			break;
	}
	splx(s);
	return (error);
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
		ttwakeup(tp);
	}
	if (rw & FWRITE) {
		wakeup((caddr_t)&tp->t_outq); /* XXX? what about selwakeup? */
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
	if (x >= TTYHOG/2 && (tp->t_state & TS_TBLOCK) == 0 &&
	    ((tp->t_lflag&ICANON) == 0) || (tp->t_canq.c_cc > 0) &&
	    tp->t_cc[VSTOP] != _POSIX_VDISABLE) {
		if (putc(tp->t_cc[VSTOP], &tp->t_outq) == 0) {
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
	struct tty *tp;
{

#ifdef DIAGNOSTIC
	if (tp == 0)
		panic("ttrstrt");
#endif
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
	struct tty *tp;
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
	int s, error;

	/*
	 * If the ioctl involves modification,
	 * hang if in the background.
	 */
	switch (com) {

	case TIOCSETD: 
	case TIOCFLUSH:
	/*case TIOCSPGRP:*/
	case TIOCSTI:
	case TIOCSWINSZ:
	case TIOCSETA:
	case TIOCSETAW:
	case TIOCSETAF:
#ifdef COMPAT_43
	case TIOCSETP:
	case TIOCSETN:
	case TIOCSETC:
	case TIOCSLTC:
	case TIOCLBIS:
	case TIOCLBIC:
	case TIOCLSET:
	case OTIOCSETD:
#endif
		while (isbackground(u.u_procp, tp) && 
		   u.u_procp->p_pgrp->pg_jobc &&
		   (u.u_procp->p_flag&SVFORK) == 0 &&
		   (u.u_procp->p_sigignore & sigmask(SIGTTOU)) == 0 &&
		   (u.u_procp->p_sigmask & sigmask(SIGTTOU)) == 0) {
			pgsignal(u.u_procp->p_pgrp, SIGTTOU, 1);
			if (error = ttysleep(tp, (caddr_t)&lbolt, 
			    TTOPRI | PCATCH, ttybg, 0)) 
				return (error);
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

	case FIOASYNC:
		if (*(int *)data)
			tp->t_state |= TS_ASYNC;
		else
			tp->t_state &= ~TS_ASYNC;
		break;

	case FIONBIO:
		break;	/* XXX remove */

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
		if (u.u_uid && !isctty(u.u_procp, tp))
			return (EACCES);
		(*linesw[tp->t_line].l_rint)(*(char *)data, tp);
		break;

	case TIOCGETA: {
		struct termios *t = (struct termios *)data;

		bcopy(&tp->t_termios, t, sizeof(struct termios));
		break;
	}

	case TIOCSETA:
	case TIOCSETAW:
	case TIOCSETAF: {
		register struct termios *t = (struct termios *)data;

		s = spltty();
		if (com == TIOCSETAW || com == TIOCSETAF) {
			if (error = ttywait(tp)) {
				splx(s);
				return (error);
			}
			if (com == TIOCSETAF)
				ttyflush(tp, FREAD);
		}
		if ((t->c_cflag&CIGNORE) == 0) {
			/*
			 * set device hardware
			 */
			if (tp->t_param && (error = (*tp->t_param)(tp, t))) {
				splx(s);
				return (error);
			} else {
				if ((tp->t_state&TS_CARR_ON) == 0 &&
				    (tp->t_cflag&CLOCAL) &&
				    (t->c_cflag&CLOCAL) == 0) {
					tp->t_state &= ~TS_ISOPEN;
					tp->t_state |= TS_WOPEN;
					ttwakeup(tp);
				}
				tp->t_cflag = t->c_cflag;
				tp->t_ispeed = t->c_ispeed;
				tp->t_ospeed = t->c_ospeed;
			}
			ttsetwater(tp);
		}
		if (com != TIOCSETAF) {
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
		/*
		 * Make the EXTPROC bit read only.
		 */
		if (tp->t_lflag&EXTPROC)
			t->c_lflag |= EXTPROC;
		else
			t->c_lflag &= ~EXTPROC;
		tp->t_lflag = t->c_lflag;
		bcopy(t->c_cc, tp->t_cc, sizeof(t->c_cc));
		splx(s);
		break;
	}

	/*
	 * Set controlling terminal.
	 * Session ctty vnode pointer set in vnode layer.
	 */
	case TIOCSCTTY: {
		register struct proc *p = u.u_procp;

		if (!SESS_LEADER(p) || 
		   (p->p_session->s_ttyvp || tp->t_session) &&
		   (tp->t_session != p->p_session))
			return (EPERM);
		tp->t_session = p->p_session;
		tp->t_pgrp = p->p_pgrp;
		p->p_session->s_ttyp = tp;
		p->p_flag |= SCTTY;
		break;
	}
		
	/*
	 * Set terminal process group.
	 */
	case TIOCSPGRP: {
		register struct proc *p = u.u_procp;
		register struct pgrp *pgrp = pgfind(*(int *)data);

		if (!isctty(p, tp))
			return (ENOTTY);
		else if (pgrp == NULL || pgrp->pg_session != p->p_session)
			return (EPERM);
		tp->t_pgrp = pgrp;
		break;
	}

	case TIOCGPGRP:
		if (!isctty(u.u_procp, tp))
			return (ENOTTY);
		*(int *)data = tp->t_pgrp ? tp->t_pgrp->pg_id : NO_PID;
		break;

	case TIOCSWINSZ:
		if (bcmp((caddr_t)&tp->t_winsize, data,
		    sizeof (struct winsize))) {
			tp->t_winsize = *(struct winsize *)data;
			pgsignal(tp->t_pgrp, SIGWINCH, 1);
		}
		break;

	case TIOCGWINSZ:
		*(struct winsize *)data = tp->t_winsize;
		break;

	case TIOCCONS:
		if (*(int *)data) {
			if (constty && constty != tp &&
			    (constty->t_state & (TS_CARR_ON|TS_ISOPEN)) ==
			    (TS_CARR_ON|TS_ISOPEN))
				return (EBUSY);
#ifndef	UCONSOLE
			if (error = suser(u.u_cred, &u.u_acflag))
				return (error);
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
	case OTIOCGETD:
	case OTIOCSETD:
	case OTIOCCONS:
		return(ttcompat(tp, com, data, flag));
#endif

	default:
		return (-1);
	}
	return (0);
}

ttnread(tp)
	struct tty *tp;
{
	int nread = 0;

	if (tp->t_lflag & PENDIN)
		ttypend(tp);
	nread = tp->t_canq.c_cc;
	if ((tp->t_lflag & ICANON) == 0)
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
		if (nread > 0 || 
		   ((tp->t_cflag&CLOCAL) == 0 && (tp->t_state&TS_CARR_ON) == 0))
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

	tp->t_dev = dev;

	tp->t_state &= ~TS_WOPEN;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_ISOPEN;
		bzero((caddr_t)&tp->t_winsize, sizeof(tp->t_winsize));
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
	tp->t_session = NULL;
	tp->t_pgrp = NULL;
	tp->t_state = 0;
	tp->t_gen++;
	return (0);
}

/*
 * Handle modem control transition on a tty.
 * Flag indicates new state of carrier.
 * Returns 0 if the line should be turned off, otherwise 1.
 */
ttymodem(tp, flag)
	register struct tty *tp;
{

	if ((tp->t_state&TS_WOPEN) == 0 && (tp->t_lflag&MDMBUF)) {
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
		if (tp->t_state&TS_ISOPEN && (tp->t_cflag&CLOCAL) == 0) {
			if (tp->t_session && tp->t_session->s_leader)
				psignal(tp->t_session->s_leader, SIGHUP);
			ttyflush(tp, FREAD|FWRITE);
			return (0);
		}
	} else {
		/*
		 * Carrier now on.
		 */
		tp->t_state |= TS_CARR_ON;
		ttwakeup(tp);
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
	else {
		tp->t_state &= ~TS_CARR_ON;
		if ((tp->t_cflag&CLOCAL) == 0) {
			if (tp->t_session && tp->t_session->s_leader)
				psignal(tp->t_session->s_leader, SIGHUP);
			return (0);
		}
	}
	return (1);
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
		c &= ~TTY_ERRORMASK;
		if (err&TTY_FE && !c) {		/* break */
			if (iflag&IGNBRK)
				goto endcase;
			else if (iflag&BRKINT && lflag&ISIG && 
				(cc[VINTR] != _POSIX_VDISABLE))
				c = cc[VINTR];
			else {
				c = 0;
				if (iflag&PARMRK)
					goto parmrk;
			}
		} else if ((err&TTY_PE && iflag&INPCK) || err&TTY_FE) {
			if (iflag&IGNPAR)
				goto endcase;
			else if (iflag&PARMRK) {
parmrk:
				putc(0377|TTY_QUOTE, &tp->t_rawq);
				putc(0|TTY_QUOTE, &tp->t_rawq);
				putc(c|TTY_QUOTE, &tp->t_rawq);
				goto endcase;
			} else
				c = 0;
		}
	}
	/*
	 * In tandem mode, check high water mark.
	 */
	if (iflag&IXOFF)
		ttyblock(tp);
	if ((tp->t_state&TS_TYPEN) == 0 && (iflag&ISTRIP))
		c &= 0177;
	if ((tp->t_lflag&EXTPROC) == 0) {
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
		 * Control chars which aren't controlled
		 * by ICANON, ISIG, or IXON.
		 */
		if (lflag&IEXTEN) {
			if (CCEQ(cc[VLNEXT], c)) {
				if (lflag&ECHO) {
					if (lflag&ECHOE)
						ttyoutstr("^\b", tp);
					else
						ttyecho(c, tp);
				}
				tp->t_state |= TS_LNCH;
				goto endcase;
			}
			if (CCEQ(cc[VDISCARD], c)) {
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
				pgsignal(tp->t_pgrp,
				    CCEQ(cc[VINTR], c) ? SIGINT : SIGQUIT, 1);
				goto endcase;
			}
			if (CCEQ(cc[VSUSP], c)) {
				if ((lflag&NOFLSH) == 0)
					ttyflush(tp, FREAD);
				ttyecho(c, tp);
				pgsignal(tp->t_pgrp, SIGTSTP, 1);
				goto endcase;
			}
		}
		/*
		 * Handle start/stop characters.
		 */
		if (iflag&IXON) {
			if (CCEQ(cc[VSTOP], c)) {
				if ((tp->t_state&TS_TTSTOP) == 0) {
					tp->t_state |= TS_TTSTOP;
					(*cdevsw[major(tp->t_dev)].d_stop)(tp,
					   0);
					return;
				}
				if (!CCEQ(cc[VSTART], c))
					return;
				/* 
				 * if VSTART == VSTOP then toggle 
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
		} else if (c == '\n' && iflag&INLCR)
			c = '\r';
	}
	/*
	 * Non canonical mode; don't process line editing
	 * characters; check high water mark for wakeup.
	 * 
	 */
	if ((lflag&ICANON) == 0) {
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
	if ((tp->t_lflag&EXTPROC) == 0) {
		/*
		 * From here on down canonical mode character
		 * processing takes place.
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
		 * kill (^U)
		 */
		if (CCEQ(cc[VKILL], c)) {
			if (lflag&ECHOKE && tp->t_rawq.c_cc == tp->t_rocount &&
			    (lflag&ECHOPRT) == 0) {
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
		if (CCEQ(cc[VWERASE], c)) {	
			int ctype;

#define CTYPE(c) ((lflag&ALTWERASE) ? (partab[(c)&TTY_CHARMASK]&0100) : 0)
			/* 
			 * erase whitespace 
			 */
			while ((c = unputc(&tp->t_rawq)) == ' ' || c == '\t')
				ttyrub(c, tp);
			if (c == -1)
				goto endcase;
			/*
			 * special case last char of token
			 */
			ttyrub(c, tp);
			c = unputc(&tp->t_rawq);
			if (c == -1 || c == ' ' || c == '\t') {
				if (c != -1)
					(void) putc(c, &tp->t_rawq);
				goto endcase;
			}
			/*
			 * erase rest of token
			 */
			ctype = CTYPE(c);
			do {
				ttyrub(c, tp);
				c = unputc(&tp->t_rawq);
				if (c == -1)
					goto endcase;
			} while (c != ' ' && c != '\t' && CTYPE(c) == ctype);
			(void) putc(c, &tp->t_rawq);
			goto endcase;
#undef CTYPE
		}
		/*
		 * reprint line (^R)
		 */
		if (CCEQ(cc[VREPRINT], c)) {
			ttyretype(tp);
			goto endcase;
		}
		/*
		 * ^T - kernel info and generate SIGINFO
		 */
		if (CCEQ(cc[VSTATUS], c)) {
			pgsignal(tp->t_pgrp, SIGINFO, 1);
			if ((lflag&NOKERNINFO) == 0)
				ttyinfo(tp);
			goto endcase;
		}
	}
	/*
	 * Check for input buffer overflow
	 */
	if (tp->t_rawq.c_cc+tp->t_canq.c_cc >= TTYHOG) {
		if (iflag&IMAXBEL) {
			if (tp->t_outq.c_cc < tp->t_hiwat)
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
		if (tp->t_state&TS_ERASE) {
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
	if ((tp->t_state&TS_TTSTOP) && (iflag&IXANY) == 0 && 
	    cc[VSTART] != cc[VSTOP])
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
	register short *colp;
	register ctype;
	register long oflag = tp->t_oflag;
	
	if ((oflag&OPOST) == 0) {
		if (tp->t_lflag&FLUSHO) 
			return (-1);
		if (putc(c, &tp->t_outq))
			return (c);
		tk_nout++;
		tp->t_outcc++;
		return (-1);
	}
	c &= TTY_CHARMASK;
	/*
	 * Turn tabs to spaces as required
	 *
	 * Special case if we have external processing, we don't
	 * do the tab expansion because we'll probably get it
	 * wrong.  If tab expansion needs to be done, let it
	 * happen externally.
	 */
	if ((tp->t_lflag&EXTPROC) == 0 &&
	    c == '\t' && oflag&OXTABS ) {
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
	/*
	 * turn <nl> to <cr><lf> if desired.
	 */
	if (c == '\n' && (tp->t_oflag&ONLCR) && ttyoutput('\r', tp) >= 0)
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
ttread(tp, uio, flag)
	register struct tty *tp;
	struct uio *uio;
{
	register struct clist *qp;
	register int c;
	register long lflag;
	register u_char *cc = tp->t_cc;
	int s, first, error = 0;

loop:
	lflag = tp->t_lflag;
	s = spltty();
	/*
	 * take pending input first 
	 */
	if (lflag&PENDIN)
		ttypend(tp);
	splx(s);

	/*
	 * Hang process if it's in the background.
	 */
	if (isbackground(u.u_procp, tp)) {
		if ((u.u_procp->p_sigignore & sigmask(SIGTTIN)) ||
		   (u.u_procp->p_sigmask & sigmask(SIGTTIN)) ||
		    u.u_procp->p_flag&SVFORK || u.u_procp->p_pgrp->pg_jobc == 0)
			return (EIO);
		pgsignal(u.u_procp->p_pgrp, SIGTTIN, 1);
		if (error = ttysleep(tp, (caddr_t)&lbolt, TTIPRI | PCATCH, 
		    ttybg, 0)) 
			return (error);
		goto loop;
	}

	/*
	 * If canonical, use the canonical queue,
	 * else use the raw queue.
	 *
	 * XXX - should get rid of canonical queue.
	 * (actually, should get rid of clists...)
	 */
	qp = lflag&ICANON ? &tp->t_canq : &tp->t_rawq;

	/*
	 * If there is no input, sleep on rawq
	 * awaiting hardware receipt and notification.
	 * If we have data, we don't need to check for carrier.
	 */
	s = spltty();
	if (qp->c_cc <= 0) {
		int carrier;

		carrier = (tp->t_state&TS_CARR_ON) || (tp->t_cflag&CLOCAL);
		if (!carrier && tp->t_state&TS_ISOPEN) {
			splx(s);
			return (0);	/* EOF */
		}
		if (flag & IO_NDELAY) {
			splx(s);
			return (EWOULDBLOCK);
		}
		error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    carrier ? ttyin : ttopen, 0);
		splx(s);
		if (error)
			return (error);
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
			pgsignal(tp->t_pgrp, SIGTSTP, 1);
			if (first) {
				if (error = ttysleep(tp, (caddr_t)&lbolt,
				    TTIPRI | PCATCH, ttybg, 0))
					break;
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
 		error = ureadc(c, uio);
		if (error)
			break;
 		if (uio->uio_resid == 0)
			break;
		/*
		 * In canonical mode check for a "break character"
		 * marking the end of a "line of input".
		 */
		if (lflag&ICANON && ttbreakc(c))
			break;
		first = 0;
	}
	/*
	 * Look to unblock output now that (presumably)
	 * the input queue has gone down.
	 */
	if (tp->t_state&TS_TBLOCK && tp->t_rawq.c_cc < TTYHOG/5) {
		if (cc[VSTART] != _POSIX_VDISABLE 
		   && putc(cc[VSTART], &tp->t_outq) == 0) {
			tp->t_state &= ~TS_TBLOCK;
			ttstart(tp);
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
ttwrite(tp, uio, flag)
	register struct tty *tp;
	register struct uio *uio;
{
	register char *cp;
	register int cc = 0, ce;
	int i, hiwat, cnt, error, s;
	char obuf[OBUFSIZ];

	hiwat = tp->t_hiwat;
	cnt = uio->uio_resid;
	error = 0;
loop:
	s = spltty();
	if ((tp->t_state&TS_CARR_ON) == 0 && (tp->t_cflag&CLOCAL) == 0) {
		if (tp->t_state&TS_ISOPEN) {
			splx(s);
			return (EIO);
		} else if (flag & IO_NDELAY) {
			splx(s);
			error = EWOULDBLOCK;
			goto out;
		} else {
			/*
			 * sleep awaiting carrier
			 */
			error = ttysleep(tp, (caddr_t)&tp->t_rawq, 
					TTIPRI | PCATCH,ttopen, 0);
			splx(s);
			if (error)
				goto out;
			goto loop;
		}
	}
	splx(s);
	/*
	 * Hang the process if it's in the background.
	 */
	if (isbackground(u.u_procp, tp) && 
	    (tp->t_lflag&TOSTOP) && (u.u_procp->p_flag&SVFORK)==0 &&
	    (u.u_procp->p_sigignore & sigmask(SIGTTOU)) == 0 &&
	    (u.u_procp->p_sigmask & sigmask(SIGTTOU)) == 0 &&
	     u.u_procp->p_pgrp->pg_jobc) {
		pgsignal(u.u_procp->p_pgrp, SIGTTOU, 1);
		if (error = ttysleep(tp, (caddr_t)&lbolt, TTIPRI | PCATCH, 
		    ttybg, 0))
			goto out;
		goto loop;
	}
	/*
	 * Process the user's data in at most OBUFSIZ
	 * chunks.  Perform any output translation.
	 * Keep track of high water mark, sleep on overflow
	 * awaiting device aid in acquiring new space.
	 */
	while (uio->uio_resid > 0 || cc > 0) {
		if (tp->t_lflag&FLUSHO) {
			uio->uio_resid = 0;
			return (0);
		}
		if (tp->t_outq.c_cc > hiwat)
			goto ovhiwat;
		/*
		 * Grab a hunk of data from the user,
		 * unless we have some leftover from last time.
		 */
		if (cc == 0) {
			cc = min(uio->uio_resid, OBUFSIZ);
			cp = obuf;
			error = uiomove(cp, cc, uio);
			if (error) {
				cc = 0;
				break;
			}
		}
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
			if ((tp->t_oflag&OPOST) == 0)
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
					    if (error = ttysleep(tp, 
						(caddr_t)&lbolt,
						 TTOPRI | PCATCH, ttybuf, 0))
						    break;
					    goto loop;
					}
					cp++, cc--;
					if ((tp->t_lflag&FLUSHO) ||
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
				if (error = ttysleep(tp, (caddr_t)&lbolt,
					    TTOPRI | PCATCH, ttybuf, 0))
					break;
				goto loop;
			}
			if (tp->t_lflag&FLUSHO || tp->t_outq.c_cc > hiwat)
				break;
		}
		ttstart(tp);
	}
out:
	/*
	 * If cc is nonzero, we leave the uio structure inconsistent,
	 * as the offset and iov pointers have moved forward,
	 * but it doesn't matter (the call will either return short
	 * or restart with a new uio).
	 */
	uio->uio_resid += cc;
	return (error);

ovhiwat:
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
	if (flag & IO_NDELAY) {
		splx(s);
		uio->uio_resid += cc;
		if (uio->uio_resid == cnt)
			return (EWOULDBLOCK);
		return (0);
	}
	tp->t_state |= TS_ASLEEP;
	error = ttysleep(tp, (caddr_t)&tp->t_outq, TTOPRI | PCATCH, ttyout, 0);
	splx(s);
	if (error)
		goto out;
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

	if ((tp->t_lflag&ECHO) == 0 || (tp->t_lflag&EXTPROC))
		return;
	tp->t_lflag &= ~FLUSHO;	
	if (tp->t_lflag&ECHOE) {
		if (tp->t_rocount == 0) {
			/*
			 * Screwed by ttwrite; retype
			 */
			ttyretype(tp);
			return;
		}
		if (c == ('\t'|TTY_QUOTE) || c == ('\n'|TTY_QUOTE))
			ttyrubo(tp, 2);
		else switch (partab[c&=0377]&077) {

		case ORDINARY:
			ttyrubo(tp, 1);
			break;

		case VTAB:
		case BACKSPACE:
		case CONTROL:
		case RETURN:
		case NEWLINE:	/* XXX can't happen ? */
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
			if (cp)
				c = *cp;	/* XXX FIX NEXTC */
			for (; cp; cp = nextc(&tp->t_rawq, cp, &c))
				ttyecho(c, tp);
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
			/* XXX */
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

	while (--cnt >= 0)
		ttyoutstr("\b \b", tp);
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

	if (tp->t_cc[VREPRINT] != _POSIX_VDISABLE)
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
	if ((tp->t_state&TS_CNTTB) == 0)
		tp->t_lflag &= ~FLUSHO;
	if (((tp->t_lflag&ECHO) == 0 && ((tp->t_lflag&ECHONL) == 0 ||
					  c == '\n')) || (tp->t_lflag&EXTPROC))
		return;
	if (tp->t_lflag&ECHOCTL) {
		if ((c&TTY_CHARMASK) <= 037 && c != '\t' && c != '\n' ||
		    c == 0177) {
			(void) ttyoutput('^', tp);
			c &= TTY_CHARMASK;
			if (c == 0177)
				c = '?';
			else
				c += 'A' - 1;
		}
	}
	(void) ttyoutput(c, tp);
}

/*
 * send string cp to tp
 */
ttyoutstr(cp, tp)
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
		pgsignal(tp->t_pgrp, SIGIO, 1); 
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

int ttyhostname = 0;
/*
 * (^T)
 * Report on state of foreground process group.
 */
ttyinfo(tp)
	struct tty *tp;
{
	register struct proc *p, *pick = NULL;
	register char *cp = hostname;
	int x, s;
	struct timeval utime, stime;
#define	pgtok(a)	(((a)*NBPG)/1024)

	if (ttycheckoutq(tp,0) == 0) 
		return;
	/* 
	 * hostname 
	 */
	if (ttyhostname) {
		if (*cp == '\0')
			ttyprintf(tp, "amnesia");
		else
			while (*cp && *cp != '.')
				tputchar(*cp++, tp);
		tputchar(' ');
	}
	/* 
	 * load average 
	 */
	x = (averunnable[0] * 100 + FSCALE/2) >> FSHIFT;
	ttyprintf(tp, "load: %d.", x/100);
	ttyoutint(x%100, 10, 2, tp);
	if (tp->t_session == NULL)
		ttyprintf(tp, " not a controlling terminal\n");
	else if (tp->t_pgrp == NULL)
		ttyprintf(tp, " no foreground process group\n");
	else if ((p = tp->t_pgrp->pg_mem) == NULL)
		ttyprintf(tp, " empty foreground process group\n");
	else {
		/* pick interesting process */
		for (; p != NULL; p = p->p_pgrpnxt) {
			if (proc_compare(pick, p))
				pick = p;
		}
		ttyprintf(tp, "  cmd: %s %d [%s] ",
			pick->p_comm, pick->p_pid,
			pick->p_wmesg ? pick->p_wmesg : "running");
		/* 
		 * cpu time 
		 */
		if (u.u_procp == pick)
			s = splclock();
		utime = pick->p_utime;
		stime = pick->p_stime;
		if (u.u_procp == pick)
			splx(s);
		/* user time */
		x = (utime.tv_usec + 5000) / 10000; /* scale to 100's */
		ttyoutint(utime.tv_sec, 10, 1, tp);
		tputchar('.', tp);
		ttyoutint(x, 10, 2, tp);
		tputchar('u', tp);
		tputchar(' ', tp);
		/* system time */
		x = (stime.tv_usec + 5000) / 10000; /* scale to 100's */
		ttyoutint(stime.tv_sec, 10, 1, tp);
		tputchar('.', tp);
		ttyoutint(x, 10, 2, tp);
		tputchar('s', tp);
		tputchar(' ', tp);
		/* 
		 * pctcpu 
		 */
		x = pick->p_pctcpu * 10000 + FSCALE/2 >> FSHIFT;
		ttyoutint(x/100, 10, 1, tp);
#ifdef notdef	/* do we really want this ??? */
		tputchar('.', tp);
		ttyoutint(x%100, 10, 2, tp);
#endif
		ttyprintf(tp, "%% %dk\n", pgtok(pick->p_ssize + pick->p_dsize));
	}
	tp->t_rocount = 0;	/* so pending input will be retyped if BS */
}

ttyoutint(n, base, min, tp)
	register int n, base, min;
	register struct tty *tp;
{
	char info[16];
	register char *p = info;

	while (--min >= 0 || n) {
		*p++ = "0123456789abcdef"[n%base];
		n /= base;
	}
	while (p > info)
		ttyoutput(*--p, tp);
}

/*
 * Returns 1 if p2 is "better" than p1
 *
 * The algorithm for picking the "interesting" process is thus:
 *
 *	1) (Only foreground processes are eligable - implied)
 *	2) Runnable processes are favored over anything
 *	   else.  The runner with the highest cpu
 *	   utilization is picked (p_cpu).  Ties are
 *	   broken by picking the highest pid.
 *	3  Next, the sleeper with the shortest sleep
 *	   time is favored.  With ties, we pick out
 *	   just "short-term" sleepers (SSINTR == 0).
 *	   Further ties are broken by picking the highest
 *	   pid.
 *
 */
#define isrun(p)	(((p)->p_stat == SRUN) || ((p)->p_stat == SIDL))
proc_compare(p1, p2)
	register struct proc *p1, *p2;
{

	if (p1 == NULL)
		return (1);
	/*
	 * see if at least one of them is runnable
	 */
	switch (isrun(p1)<<1 | isrun(p2)) {
	case 0x01:
		return (1);
	case 0x10:
		return (0);
	case 0x11:
		/*
		 * tie - favor one with highest recent cpu utilization
		 */
		if (p2->p_cpu > p1->p_cpu)
			return (1);
		if (p1->p_cpu > p2->p_cpu)
			return (0);
		return (p2->p_pid > p1->p_pid);	/* tie - return highest pid */
	}
	/* 
	 * pick the one with the smallest sleep time
	 */
	if (p2->p_slptime > p1->p_slptime)
		return (0);
	if (p1->p_slptime > p2->p_slptime)
		return (1);
	/*
	 * favor one sleeping in a non-interruptible sleep
	 */
	if (p1->p_flag&SSINTR && (p2->p_flag&SSINTR) == 0)
		return (1);
	if (p2->p_flag&SSINTR && (p1->p_flag&SSINTR) == 0)
		return (0);
	return(p2->p_pid > p1->p_pid);		/* tie - return highest pid */
}
#define TOTTY	0x2	/* XXX should be in header */
/*VARARGS2*/
ttyprintf(tp, fmt, x1)
	struct tty *tp;
	char *fmt;
	unsigned x1;
{
	prf(fmt, &x1, TOTTY, (caddr_t)tp);
}

/*
 * Output char to tty; console putchar style.
 */
tputchar(c, tp)
	int c;
	struct tty *tp;
{
	register s = spltty();

	if ((tp->t_state & (TS_CARR_ON | TS_ISOPEN)) 
	    == (TS_CARR_ON | TS_ISOPEN)) {
		if (c == '\n')
			(void) ttyoutput('\r', tp);
		(void) ttyoutput(c, tp);
		ttstart(tp);
		splx(s);
		return (0);
	}
	splx(s);
	return (-1);
}

/*
 * Sleep on chan.
 *
 * Return ERESTART if tty changed while we napped.
 */
ttysleep(tp, chan, pri, wmesg, timo)
	struct tty *tp;
	caddr_t chan;
	int pri;
	char *wmesg;
	int timo;
{
	int error;
	short gen = tp->t_gen;

	if (error = tsleep(chan, pri, wmesg, timo))
		return (error);
	if (tp->t_gen != gen)
		return (ERESTART);
	return (0);
}
