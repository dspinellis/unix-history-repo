/*	tty.c	4.32	82/10/17	*/

/*
 * TTY subroutines common to more than one line discipline
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/proc.h"
#include "../h/inode.h"
#include "../h/file.h"
#include "../h/reg.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/dk.h"
#include "../h/uio.h"
#include "../h/kernel.h"

/*
 * Table giving parity for characters and indicating
 * character classes to tty driver.  In particular,
 * if the low 6 bits are 0, then the character needs
 * no special processing on output.
 */

char partab[] = {
	0001,0201,0201,0001,0201,0001,0001,0201,
	0202,0004,0003,0201,0005,0206,0201,0001,
	0201,0001,0001,0201,0001,0201,0201,0001,
	0001,0201,0201,0001,0201,0001,0001,0201,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0200,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0200,0000,0000,0200,0000,0200,0200,0000,
	0000,0200,0200,0000,0200,0000,0000,0201,

	/*
	 * 7 bit ascii ends with the last character above,
	 * but we contine through all 256 codes for the sake
	 * of the tty output routines which use special vax
	 * instructions which need a 256 character trt table.
	 */

	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007,
	0007,0007,0007,0007,0007,0007,0007,0007
};

/*
 * Input mapping table-- if an entry is non-zero, when the
 * corresponding character is typed preceded by "\" the escape
 * sequence is replaced by the table value.  Mostly used for
 * upper-case only terminals.
 */

char	maptab[] ={
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,'|',000,000,000,000,000,'`',
	'{','}',000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,000,000,
	000,000,000,000,000,000,'~',000,
	000,'A','B','C','D','E','F','G',
	'H','I','J','K','L','M','N','O',
	'P','Q','R','S','T','U','V','W',
	'X','Y','Z',000,000,000,000,000,
};

short	tthiwat[16] =
   { 100,100,100,100,100,100,100,200,200,400,400,400,650,650,650,650 };
short	ttlowat[16] =
   {  30, 30, 30, 30, 30, 30, 30, 50, 50,120,120,120,125,125,125,125 };

#define	OBUFSIZ	100

/*
 * set default control characters.
 */
ttychars(tp)
	register struct tty *tp;
{

	tun.t_intrc = CINTR;
	tun.t_quitc = CQUIT;
	tun.t_startc = CSTART;
	tun.t_stopc = CSTOP;
	tun.t_eofc = CEOT;
	tun.t_brkc = CBRK;
	tp->t_erase = CERASE;
	tp->t_kill = CKILL;
/* begin local */
	tlun.t_suspc = CTRL(z);
	tlun.t_dsuspc = CTRL(y);
	tlun.t_rprntc = CTRL(r);
	tlun.t_flushc = CTRL(o);
	tlun.t_werasc = CTRL(w);
	tlun.t_lnextc = CTRL(v);
	tp->t_local = 0;
	tp->t_lstate = 0;
/* end local */
}

/*
 * Wait for output to drain, then flush input waiting.
 */
wflushtty(tp)
	register struct tty *tp;
{

	(void) spl5();
	while (tp->t_outq.c_cc && tp->t_state&TS_CARR_ON
	    && tp->t_oproc) {		/* kludge for pty */
		(*tp->t_oproc)(tp);
		tp->t_state |= TS_ASLEEP;
		sleep((caddr_t)&tp->t_outq, TTOPRI);
	}
	flushtty(tp, FREAD);
	(void) spl0();
}

/*
 * flush all TTY queues
 */
flushtty(tp, rw)
	register struct tty *tp;
{
	register s;

	s = spl6();
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
		tp->t_delct = 0;
		tp->t_rocount = 0;		/* local */
		tp->t_rocol = 0;
		tp->t_lstate = 0;
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
		flushtty(tp, FREAD|FWRITE);
		tp->t_state &= ~TS_TBLOCK;
	}
	if (x >= TTYHOG/2) {
		if (putc(tun.t_stopc, &tp->t_outq)==0) {
			tp->t_state |= TS_TBLOCK;
			tp->t_char++;
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

	if (tp == 0) {
		printf("ttrstrt: arg was 0!\n");
		return;
	}
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
	register s;

	s = spl5();
	if ((tp->t_state&(TS_TIMEOUT|TS_TTSTOP|TS_BUSY)) == 0 &&
	    tp->t_oproc)		/* kludge for pty */
		(*tp->t_oproc)(tp);
	splx(s);
}

/*
 * Common code for tty ioctls.
 */
/*ARGSUSED*/
ttioctl(tp, com, data, flag)
	register struct tty *tp;
	caddr_t data;
{
	int dev = tp->t_dev;
	extern int nldisp;
	int s;

	/*
	 * If the ioctl involves modification,
	 * insist on being able to write the device,
	 * and hang if in the background.
	 */
	switch (com) {

	case TIOCSETD:
	case TIOCSETP:
	case TIOCSETN:
	case TIOCFLUSH:
	case TIOCSETC:
	case TIOCSLTC:
	case TIOCSPGRP:
	case TIOCLBIS:
	case TIOCLBIC:
	case TIOCLSET:
		while (tp->t_line == NTTYDISC &&
		   u.u_procp->p_pgrp != tp->t_pgrp && tp == u.u_ttyp &&
		   (u.u_procp->p_flag&SVFORK) == 0 &&
		   u.u_signal[SIGTTOU] != SIG_IGN &&
		   u.u_signal[SIGTTOU] != SIG_HOLD) {
			gsignal(u.u_procp->p_pgrp, SIGTTOU);
			sleep((caddr_t)&lbolt, TTOPRI);
		}
		break;
	}

	switch (com) {

	/* get discipline number */
	case TIOCGETD:
		*(int *)data = tp->t_line;
		break;

	/* set line discipline */
	case TIOCSETD: {
		register int t = *(int *)data;
		int error;

		if (t >= nldisp)
			return (ENXIO);
		s = spl5();
		if (tp->t_line)
			(*linesw[tp->t_line].l_close)(tp);
		if (t)
			error = (*linesw[t].l_open)(dev, tp);
		splx(s);
		if (error)
			return (error);
		tp->t_line = t;
		break;
	}

	/* prevent more opens on channel */
	case TIOCEXCL:
		tp->t_state |= TS_XCLUDE;
		break;

	case TIOCNXCL:
		tp->t_state &= ~TS_XCLUDE;
		break;

	/* set new parameters */
	case TIOCSETP:
	case TIOCSETN: {
		register struct sgttyb *sg = (struct sgttyb *)data;
		struct clist tq;

		(void) spl5();
		if (tp->t_flags&RAW || sg->sg_flags&RAW || com == TIOCSETP)
			wflushtty(tp);
		else if ((tp->t_flags&CBREAK) != (sg->sg_flags&CBREAK)) {
			if (sg->sg_flags & CBREAK) {
				catq(&tp->t_rawq, &tp->t_canq);
				tq = tp->t_rawq;
				tp->t_rawq = tp->t_canq;
				tp->t_canq = tq;
			} else {
				tp->t_local |= LPENDIN;
				ttwakeup(tp);
			}
		}
		tp->t_ispeed = sg->sg_ispeed;
		tp->t_ospeed = sg->sg_ospeed;
		tp->t_erase = sg->sg_erase;
		tp->t_kill = sg->sg_kill;
		tp->t_flags = sg->sg_flags;
		if (tp->t_flags & RAW) {
			tp->t_state &= ~TS_TTSTOP;
			ttstart(tp);
		}
		(void) spl0();
		break;
	}

	/* send current parameters to user */
	case TIOCGETP: {
		register struct sgttyb *sg = (struct sgttyb *)data;

		sg->sg_ispeed = tp->t_ispeed;
		sg->sg_ospeed = tp->t_ospeed;
		sg->sg_erase = tp->t_erase;
		sg->sg_kill = tp->t_kill;
		sg->sg_flags = tp->t_flags;
		break;
	}

	/* hang up line on last close */
	case TIOCHPCL:
		tp->t_state |= TS_HUPCLS;
		break;

	case TIOCFLUSH: {
		register int flags = *(int *)data;

		if (flags == 0)
			flags = FREAD|FWRITE;
		else
			flags &= FREAD|FWRITE;
		flushtty(tp, flags);
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

	/* set and fetch special characters */
	case TIOCSETC:
		bcopy(data, (caddr_t)&tun, sizeof (struct tchars));
		break;

	case TIOCGETC:
		bcopy((caddr_t)&tun, data, sizeof (struct tchars));
		break;

	/* set/get local special characters */
	case TIOCSLTC:
		bcopy(data, (caddr_t)&tlun, sizeof (struct ltchars));
		break;

	case TIOCGLTC:
		bcopy((caddr_t)&tlun, data, sizeof (struct ltchars));
		break;

	/* return number of characters immediately available */
	case FIONREAD:
		*(off_t *)data = ttnread(tp);
		break;

	/* should allow SPGRP and GPGRP only if tty open for reading */
	case TIOCSPGRP:
		tp->t_pgrp = *(int *)data;
		break;

	case TIOCGPGRP:
		*(int *)data = tp->t_pgrp;
		break;

	/* Modify local mode word */
	case TIOCLBIS:
		tp->t_local |= *(int *)data;
		break;

	case TIOCLBIC:
		tp->t_local &= ~(*(int *)data);
		break;

	case TIOCLSET:
		tp->t_local = *(int *)data;
		break;

	case TIOCLGET:
		*(int *)data = tp->t_local;
		break;

	case TIOCSTOP:
		s = spl5();
		if ((tp->t_state & TS_TTSTOP) == 0) {
			tp->t_state |= TS_TTSTOP;
			(*cdevsw[major(tp->t_dev)].d_stop)(tp, 0);
		}
		splx(s);
		break;

	case TIOCSTART:
		s = spl5();
		if ((tp->t_state & TS_TTSTOP) || (tp->t_local & LFLUSHO)) {
			tp->t_state &= ~TS_TTSTOP;
			tp->t_local &= ~LFLUSHO;
			ttstart(tp);
		}
		splx(s);
		break;

	default:
		return (-1);
	}
	return (0);
}

ttnread(tp)
	struct tty *tp;
{
	int nread = 0;

	if (tp->t_local & LPENDIN)
		ttypend(tp);
	nread = tp->t_canq.c_cc;
	if (tp->t_flags & (RAW|CBREAK))
		nread += tp->t_rawq.c_cc;
	return (nread);
}

ttselect(dev, rw)
	dev_t dev;
	int rw;
{
	register struct tty *tp = &cdevsw[major(dev)].d_ttys[minor(dev)];
	int nread;
	int s = spl5();

	switch (rw) {

	case FREAD:
		nread = ttnread(tp);
		if (nread > 0)
			goto win;
		if (tp->t_rsel && tp->t_rsel->p_wchan == (caddr_t)&selwait)
			tp->t_state |= TS_RCOLL;
		else
			tp->t_rsel = u.u_procp;
		break;

	case FWRITE:
		if (tp->t_outq.c_cc <= TTLOWAT(tp))
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

#define	OBUFSIZ	100

/*
 * routine called on opens while tp->t_line == NTTYDISC
 * establishes a process group for distribution of
 * quits and interrupts from the tty.
 * (actually, pp->p_pgrp can't be 0 when this routine
 * is called since NTTYDISC is not the default discipline)
 */
ttyopen(dev, tp)
	dev_t dev;
	register struct tty *tp;
{
	register struct proc *pp;

	pp = u.u_procp;
	tp->t_dev = dev;
	if (pp->p_pgrp == 0) {
		u.u_ttyp = tp;
		u.u_ttyd = dev;
		if (tp->t_pgrp == 0)
			tp->t_pgrp = pp->p_pid;
		pp->p_pgrp = tp->t_pgrp;
	}
	tp->t_state &= ~TS_WOPEN;
	tp->t_state |= TS_ISOPEN;
	if (tp->t_line != NTTYDISC)
		wflushtty(tp);
	return (0);
}

/*
 * clean tp on last close
 */
ttyclose(tp)
	register struct tty *tp;
{

	if (tp->t_line) {
		wflushtty(tp);
		tp->t_line = 0;
		return;
	}
	tp->t_pgrp = 0;
	wflushtty(tp);
	tp->t_state = 0;
}

/*
 * reinput pending characters after state switch
 * call at spl5().
 */
ttypend(tp)
	register struct tty *tp;
{
	struct clist tq;
	register c;

	tp->t_local &= ~LPENDIN;
	tp->t_lstate |= LSTYPEN;
	tq = tp->t_rawq;
	tp->t_rawq.c_cc = 0;
	tp->t_rawq.c_cf = tp->t_rawq.c_cl = 0;
	while ((c = getc(&tq)) >= 0)
		ttyinput(c, tp);
	tp->t_lstate &= ~LSTYPEN;
}

/*
 * Place a character on raw TTY input queue, putting in delimiters
 * and waking up top half as needed.
 * Also echo if required.
 * The arguments are the character and the appropriate
 * tty structure.
 */
ttyinput(c, tp)
	register c;
	register struct tty *tp;
{
	register int t_flags;
	int i;

	if (tp->t_local&LPENDIN)
		ttypend(tp);
	tk_nin++;
	c &= 0377;
	t_flags = tp->t_flags;
	if (t_flags&TANDEM)
		ttyblock(tp);
	if ((t_flags&RAW)==0) {
		if ((tp->t_lstate&LSTYPEN) == 0)
			c &= 0177;
	/* check for literal nexting very first */
		if (tp->t_lstate&LSLNCH) {
			c |= 0200;
			tp->t_lstate &= ~LSLNCH;
		}
		if (tp->t_line == NTTYDISC && c==tlun.t_lnextc) {
			if (tp->t_flags&ECHO)
				ttyout("^\b", tp);
			tp->t_lstate |= LSLNCH;
	/* check for output control functions */
		} else if (c==tun.t_stopc) {
			if ((tp->t_state&TS_TTSTOP)==0) {
				tp->t_state |= TS_TTSTOP;
				(*cdevsw[major(tp->t_dev)].d_stop)(tp, 0);
				return;
			}
			if (c!=tun.t_startc)
				return;
		} else if (c==tun.t_startc) {
			tp->t_state &= ~TS_TTSTOP;
			tp->t_local &= ~LFLUSHO;
			ttstart(tp);
			return;
		} else if (tp->t_line == NTTYDISC && c==tlun.t_flushc) {
			if (tp->t_local & LFLUSHO)
				tp->t_local &= ~LFLUSHO;
			else {
				flushtty(tp, FWRITE);
				ttyecho(c, tp);
				if (tp->t_rawq.c_cc+tp->t_canq.c_cc)
					ttyretype(tp);
				tp->t_local |= LFLUSHO;
			}
			ttstart(tp);
			return;
		} else if (c==tun.t_intrc || c==tun.t_quitc ||
		    (tp->t_line == NTTYDISC && c==tlun.t_suspc)) {
			if ((tp->t_local & LNOFLSH) == 0)
				flushtty(tp,
				    c==tlun.t_suspc ? FREAD : FREAD|FWRITE);
			ttyecho(c, tp);
			c = c==tun.t_intrc ? SIGINT :
				((c==tun.t_quitc) ? SIGQUIT : SIGTSTP);
			ttsignal(tp, c);
	/* check for buffer editing functions - cooked mode */
		} else if ((t_flags&CBREAK) == 0) {
			if ((tp->t_lstate&LSQUOT) &&
			    (c==tp->t_erase||c==tp->t_kill)) {
				ttyrub(unputc(&tp->t_rawq), tp);
				c |= 0200;
			}
			if (c==tp->t_erase) {
				if (tp->t_rawq.c_cc)
					ttyrub(unputc(&tp->t_rawq), tp);
			} else if (c==tp->t_kill) {
				if (tp->t_local&LCRTKIL &&
				    tp->t_rawq.c_cc == tp->t_rocount) {
					while (tp->t_rawq.c_cc)
						ttyrub(unputc(&tp->t_rawq), tp);
				} else {
					ttyecho(c, tp);
					ttyecho('\n', tp);
					while (getc(&tp->t_rawq) > 0)
						;
					tp->t_rocount = 0;
				}
				tp->t_lstate = 0;
			} else if (tp->t_line == NTTYDISC && c==tlun.t_werasc) {
				if (tp->t_rawq.c_cc == 0)
					goto out;
				do {
					c = unputc(&tp->t_rawq);
					if (c != ' ' && c != '\t')
						goto erasenb;
					ttyrub(c, tp);
				} while (tp->t_rawq.c_cc);
				goto out;
			    erasenb:
				do {
					ttyrub(c, tp);
					if (tp->t_rawq.c_cc == 0)
						goto out;
					c = unputc(&tp->t_rawq);
				} while (c != ' ' && c != '\t');
				(void) putc(c, &tp->t_rawq);
			} else if (tp->t_line == NTTYDISC && c==tlun.t_rprntc) {
				ttyretype(tp);
	/* check for cooked mode input buffer overflow */
			} else if (tp->t_rawq.c_cc+tp->t_canq.c_cc >= TTYHOG) {
				;
	/* put data char in q for user and wakeup if a break char */
			} else if (putc(c, &tp->t_rawq) >= 0) {
				if (tp->t_rawq.c_cc+tp->t_canq.c_cc==TTYHOG
				    && tp->t_line == NTTYDISC)
					(void) ttyoutput(CTRL(g), tp);
				if (!ttbreakc(c, tp)) {
					if (tp->t_rocount++ == 0)
						tp->t_rocol = tp->t_col;
				} else {
					tp->t_rocount = 0;
					catq(&tp->t_rawq, &tp->t_canq);
					/* IF (TP->T_CHAN) (VOID) SDATA(TP->T_CHAN); */
					ttwakeup(tp);
				}
				tp->t_lstate &= ~LSQUOT;
				if (c == '\\')
					tp->t_lstate |= LSQUOT;
				if (tp->t_lstate&LSERASE) {
					tp->t_lstate &= ~LSERASE;
					(void) ttyoutput('/', tp);
				}
				i = tp->t_col;
				ttyecho(c, tp);
				if (c==tun.t_eofc && tp->t_flags&ECHO) {
					i = MIN(2, tp->t_col - i);
					while (i > 0) {
						(void) ttyoutput('\b', tp);
						i--;
					}
				}
			}
	/* CBREAK mode */
		} else if (tp->t_rawq.c_cc > TTYHOG) {
			if (tp->t_outq.c_cc < TTHIWAT(tp) &&
			    tp->t_line == NTTYDISC)
				(void) ttyoutput(CTRL(g), tp);
		} else if (putc(c, &tp->t_rawq) >= 0) {
			ttwakeup(tp);
			ttyecho(c, tp);
		}
	/* RAW mode */
	} else if (tp->t_rawq.c_cc > TTYHOG) 
		flushtty(tp, FREAD|FWRITE);
	else {
		if (putc(c, &tp->t_rawq) >= 0)
			ttwakeup(tp);
		ttyecho(c, tp);
	}
out:
	if (tp->t_local & LDECCTQ && tp->t_state & TS_TTSTOP &&
	    tun.t_startc != tun.t_stopc)
		return;
	tp->t_state &= ~TS_TTSTOP;
	tp->t_local &= ~LFLUSHO;
	ttstart(tp);
}

/*
 * put character on TTY output queue, adding delays,
 * expanding tabs, and handling the CR/NL bit.
 * It is called both from the top half for output, and from
 * interrupt level for echoing.
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

	if (tp->t_flags&RAW || tp->t_local&LLITOUT) {
		if (tp->t_local&LFLUSHO)
			return (-1);
		if (putc(c, &tp->t_outq))
			return (c);
		tk_nout++;
		return (-1);
	}
	/*
	 * Ignore EOT in normal mode to avoid hanging up
	 * certain terminals.
	 */
	c &= 0177;
	if (c==CEOT && (tp->t_flags&CBREAK)==0)
		return (-1);
	/*
	 * Turn tabs to spaces as required
	 */
	if (c=='\t' && (tp->t_flags&TBDELAY)==XTABS) {
		register int s;

		c = 8 - (tp->t_col&7);
		if ((tp->t_local&LFLUSHO) == 0) {
			s = spl5();		/* don't interrupt tabs */
			c -= b_to_q("        ", c, &tp->t_outq);
			tk_nout += c;
			splx(s);
		}
		tp->t_col += c;
		return (c ? -1 : '\t');
	}
	tk_nout++;
	/*
	 * for upper-case-only terminals,
	 * generate escapes.
	 */
	if (tp->t_flags&LCASE) {
		colp = "({)}!|^~'`";
		while (*colp++)
			if (c == *colp++) {
				if (ttyoutput('\\', tp) >= 0)
					return (c);
				c = colp[-2];
				break;
			}
		if ('A'<=c && c<='Z') {
			if (ttyoutput('\\', tp) >= 0)
				return (c);
		} else if ('a'<=c && c<='z')
			c += 'A' - 'a';
	}
	/*
	 * turn <nl> to <cr><lf> if desired.
	 */
	if (c=='\n' && tp->t_flags&CRMOD)
		if (ttyoutput('\r', tp) >= 0)
			return (c);
	if (c=='~' && tp->t_local&LTILDE)
		c = '`';
	if ((tp->t_local&LFLUSHO) == 0 && putc(c, &tp->t_outq))
		return (c);
	/*
	 * Calculate delays.
	 * The numbers here represent clock ticks
	 * and are not necessarily optimal for all terminals.
	 * The delays are indicated by characters above 0200.
	 * In raw mode there are no delays and the
	 * transmission path is 8 bits wide.
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

	case NEWLINE:
		ctype = (tp->t_flags >> 8) & 03;
		if (ctype == 1) { /* tty 37 */
			if (*colp)
				c = max(((unsigned)*colp>>4) + 3, (unsigned)6);
		} else
		if (ctype == 2) { /* vt05 */
			c = 6;
		}
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
		if (tp->t_flags & VTDELAY) /* tty 37 */
			c = 0177;
		break;

	case RETURN:
		ctype = (tp->t_flags >> 12) & 03;
		if (ctype == 1) { /* tn 300 */
			c = 5;
		} else if (ctype == 2) { /* ti 700 */
			c = 10;
		} else if (ctype == 3) { /* concept 100 */
			int i;
			if ((i = *colp) >= 0)
				for (; i<9; i++)
					(void) putc(0177, &tp->t_outq);
		}
		*colp = 0;
	}
	if (c && (tp->t_local&LFLUSHO) == 0)
		(void) putc(c|0200, &tp->t_outq);
	return (-1);
}

/*
 * Called from device's read routine after it has
 * calculated the tty-structure given as argument.
 */
ttread(tp, uio)
	register struct tty *tp;
	struct uio *uio;
{
	register struct clist *qp;
	register c, first;
	int error = 0;

	if ((tp->t_state&TS_CARR_ON)==0)
		return (EIO);
loop:
	(void) spl5();
	if (tp->t_local&LPENDIN)
		ttypend(tp);
	(void) spl0();
	while (tp == u.u_ttyp && u.u_procp->p_pgrp != tp->t_pgrp) {
		if (u.u_signal[SIGTTIN] == SIG_IGN ||
		    u.u_signal[SIGTTIN] == SIG_HOLD ||
/*
		    (u.u_procp->p_flag&SDETACH) ||
*/
		    u.u_procp->p_flag&SVFORK)
			return (EIO);
		gsignal(u.u_procp->p_pgrp, SIGTTIN);
		sleep((caddr_t)&lbolt, TTIPRI);
	}
	if (tp->t_flags&RAW) {
		(void) spl5();
		if (tp->t_rawq.c_cc <= 0) {
			if ((tp->t_state&TS_CARR_ON)==0 ||
			    (tp->t_state&TS_NBIO)) {
				(void) spl0();
				return (EWOULDBLOCK);
			}
			sleep((caddr_t)&tp->t_rawq, TTIPRI);
			(void) spl0();
			goto loop;
		}
		(void) spl0();
		while (tp->t_rawq.c_cc && uio->uio_iovcnt) {
			error = passuc(getc(&tp->t_rawq), uio);
			if (error)
				break;
		}
		return (error);
	} else {
		qp = tp->t_flags & CBREAK ? &tp->t_rawq : &tp->t_canq;
		(void) spl5();
		if (qp->c_cc <= 0) {
			if ((tp->t_state&TS_CARR_ON)==0 ||
			    (tp->t_state&TS_NBIO)) {
				(void) spl0();
				return (EWOULDBLOCK);
			}
			sleep((caddr_t)&tp->t_rawq, TTIPRI);
			(void) spl0();
			goto loop;
		}
		(void) spl0();
		first = 1;
		while ((c = getc(qp)) >= 0) {
			if (tp->t_flags&CRMOD && c == '\r')
				c = '\n';
			if (tp->t_flags&LCASE && c <= 0177)
				if (tp->t_lstate&LSBKSL) {
					if (maptab[c])
						c = maptab[c];
					tp->t_lstate &= ~LSBKSL;
				} else if (c >= 'A' && c <= 'Z')
					c += 'a' - 'A';
				else if (c == '\\') {
					tp->t_lstate |= LSBKSL;
					continue;
				}
			if (tp->t_line == NTTYDISC && c == tlun.t_dsuspc) {
				ttsignal(tp, SIGTSTP);
				if (first) {
					sleep((caddr_t)&lbolt, TTIPRI);
					goto loop;
				}
				break;
			}
			if (c == tun.t_eofc && (tp->t_flags&CBREAK)==0)
				break;
			error = passuc(c & 0177, uio);
			if (error)
				break;
			if (uio->uio_iovcnt == 0)
				break;
			if ((tp->t_flags&CBREAK)==0 && ttbreakc(c, tp))
				break;
			first = 0;
		}
		tp->t_lstate &= ~LSBKSL;
	}
	if (tp->t_state&TS_TBLOCK && tp->t_rawq.c_cc < TTYHOG/5) {
		if (putc(tun.t_startc, &tp->t_outq)==0) {
			tp->t_state &= ~TS_TBLOCK;
			ttstart(tp);
		}
		tp->t_char = 0;
	}
	return (error);
}

/*
 * Called from the device's write routine after it has
 * calculated the tty-structure given as argument.
 */
ttwrite(tp, uio)
	register struct tty *tp;
	struct uio *uio;
{
#ifdef vax
	/*
	 * THE POSITIONING OF CP, CC, AND CE ARE CRITICAL
	 * AND MUST NOT BE CHANGED WITHOUT PATCHING
	 * THE 'ASM' INLINES BELOW.  WATCH OUT.
	 */
#endif
	register char *cp;
	register int cc, ce;
	register i;
	char obuf[OBUFSIZ];
	register c;
	int hiwat = TTHIWAT(tp);
	int cnt = uio->uio_resid;
	int error = 0;

	if ((tp->t_state&TS_CARR_ON)==0)
		return (EIO);
loop:
	while (u.u_procp->p_pgrp != tp->t_pgrp && tp == u.u_ttyp &&
	    (tp->t_local&LTOSTOP) && (u.u_procp->p_flag&SVFORK)==0 &&
	    u.u_signal[SIGTTOU] != SIG_IGN &&
	    u.u_signal[SIGTTOU] != SIG_HOLD
/*
					     &&
	    (u.u_procp->p_flag&SDETACH)==0) {
*/
	    ) {
		gsignal(u.u_procp->p_pgrp, SIGTTOU);
		sleep((caddr_t)&lbolt, TTIPRI);
	}
	while (uio->uio_resid > 0) {
		cc = uio->uio_iov->iov_len;
		if (cc == 0) {
			uio->uio_iovcnt--;
			uio->uio_iov++;
			if (uio->uio_iovcnt < 0)
				panic("ttwrite");
			continue;
		}
		if (cc > OBUFSIZ)
			cc = OBUFSIZ;
		cp = obuf;
		error = uiomove(cp, cc, UIO_WRITE, uio);
		if (error)
			break;
		if (tp->t_outq.c_cc > hiwat)
			goto ovhiwat;
		if (tp->t_local&LFLUSHO)
			continue;
		if (tp->t_flags&LCASE || tp->t_local&LTILDE) {
			while (cc) {
				c = *cp++;
				tp->t_rocount = 0;
				while ((c = ttyoutput(c, tp)) >= 0) {
					/* out of clists, wait a bit */
					ttstart(tp);
					sleep((caddr_t)&lbolt, TTOPRI);
					tp->t_rocount = 0;
				}
				--cc;
				if (tp->t_outq.c_cc > hiwat)
					goto ovhiwat;
			}
			continue;
		}
		while (cc) {
			if (tp->t_flags&RAW || tp->t_local&LLITOUT)
				ce = cc;
			else {
#ifdef vax
				asm("	scanc	r9,(r10),_partab,$077");
				asm("	subl3	r0,r9,r8");
#else
				ce=0;
				while (((partab[*(unsigned char *)(cp+ce)]&077)==0)&&(ce<cc))
					ce++;
#endif
				if (ce==0) {
					tp->t_rocount = 0;
					if (ttyoutput(*cp, tp) >= 0) {
						ttstart(tp);
						sleep((caddr_t)&lbolt, TTOPRI);
						continue;
					}
					cp++;
					cc--;
					if (tp->t_outq.c_cc > hiwat)
						goto ovhiwat;
				}
			}
			tp->t_rocount = 0;
			i=b_to_q(cp,ce,&tp->t_outq);
			ce-=i;
			tk_nout+=ce;
			tp->t_col+=ce;
			cp+=ce;
			cc-=ce;
			if (i) {
				ttstart(tp);
				sleep((caddr_t)&lbolt, TTOPRI);
			}
			if (ce || tp->t_outq.c_cc > hiwat)
				goto ovhiwat;
		}
	}
	ttstart(tp);
	return (error);

ovhiwat:
	(void) spl5();
	uio->uio_iov->iov_base -= cc;
	uio->uio_iov->iov_len += cc;
	uio->uio_resid += cc;
	uio->uio_offset -= cc;
	if (tp->t_outq.c_cc <= hiwat) {
		(void) spl0();
		goto loop;
	}
	ttstart(tp);
	if (tp->t_state & TS_NBIO) {
		if (uio->uio_resid == cnt)
			return (EWOULDBLOCK);
		return (0);
	}
	tp->t_state |= TS_ASLEEP;
	sleep((caddr_t)&tp->t_outq, TTOPRI);
	(void) spl0();
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

	if ((tp->t_flags&ECHO)==0)
		return;
	tp->t_local &= ~LFLUSHO;
	c &= 0377;
	if (tp->t_local&LCRTBS) {
		if (tp->t_rocount == 0) {
			/*
			 * Screwed by ttwrite; retype
			 */
			ttyretype(tp);
			return;
		}
		if (c==('\t'|0200) || c==('\n'|0200))
			ttyrubo(tp, 2);
		else switch (partab[c&=0177] & 0177) {

		case ORDINARY:
			if (tp->t_flags&LCASE && c >= 'A' && c <= 'Z')
				ttyrubo(tp, 2);
			else
				ttyrubo(tp, 1);
			break;

		case VTAB:
		case BACKSPACE:
		case CONTROL:
		case RETURN:
			if (tp->t_local & LCTLECH)
				ttyrubo(tp, 2);
			break;

		case TAB:
			if (tp->t_rocount < tp->t_rawq.c_cc) {
				ttyretype(tp);
				return;
			}
			s = spl5();
			savecol = tp->t_col;
			tp->t_lstate |= LSCNTTB;
			tp->t_local |= LFLUSHO;
			tp->t_col = tp->t_rocol;
			for (cp = tp->t_rawq.c_cf; cp; cp = nextc(&tp->t_rawq, cp))
				ttyecho(*cp, tp);
			tp->t_local &= ~LFLUSHO;
			tp->t_lstate &= ~LSCNTTB;
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

		default:
			panic("ttyrub");
		}
	} else if (tp->t_local&LPRTERA) {
		if ((tp->t_lstate&LSERASE) == 0) {
			(void) ttyoutput('\\', tp);
			tp->t_lstate |= LSERASE;
		}
		ttyecho(c, tp);
	} else
		ttyecho(tp->t_erase, tp);
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
		ttyout(tp->t_local&LCRTERA ? "\b \b" : "\b", tp);
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
	int s;

	if (tlun.t_rprntc != 0377)
		ttyecho(tlun.t_rprntc, tp);
	(void) ttyoutput('\n', tp);
	s = spl5();
	for (cp = tp->t_canq.c_cf; cp; cp = nextc(&tp->t_canq, cp))
		ttyecho(*cp, tp);
	for (cp = tp->t_rawq.c_cf; cp; cp = nextc(&tp->t_rawq, cp))
		ttyecho(*cp, tp);
	tp->t_lstate &= ~LSERASE;
	splx(s);
	tp->t_rocount = tp->t_rawq.c_cc;
	tp->t_rocol = 0;
}

/*
 * Echo a typed character to the terminal
 */
ttyecho(c, tp)
	register c;
	register struct tty *tp;
{

	if ((tp->t_lstate & LSCNTTB) == 0)
		tp->t_local &= ~LFLUSHO;
	if ((tp->t_flags&ECHO) == 0)
		return;
	c &= 0377;
	if (tp->t_flags&RAW) {
		(void) ttyoutput(c, tp);
		return;
	}
	if (c == '\r' && tp->t_flags&CRMOD)
		c = '\n';
	if (tp->t_local&LCTLECH) {
		if ((c&0177) <= 037 && c!='\t' && c!='\n' || (c&0177)==0177) {
			(void) ttyoutput('^', tp);
			c &= 0177;
			if (c == 0177)
				c = '?';
			else if (tp->t_flags&LCASE)
				c += 'a' - 1;
			else
				c += 'A' - 1;
		}
	}
	if ((tp->t_flags&LCASE) && (c >= 'A' && c <= 'Z'))
		c += 'a' - 'A';
	(void) ttyoutput(c & 0177, tp);
}

/*
 * Is c a break char for tp?
 */
ttbreakc(c, tp)
	register c;
	register struct tty *tp;
{
	return (c == '\n' || c == tun.t_eofc || c == tun.t_brkc ||
		c == '\r' && (tp->t_flags&CRMOD));
}

/*
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
	wakeup((caddr_t)&tp->t_rawq);
}

ttsignal(tp, signo)
	struct tty *tp;
	int signo;
{

	gsignal(tp->t_pgrp, signo);
}
