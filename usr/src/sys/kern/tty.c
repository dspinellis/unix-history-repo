/*	tty.c	4.21	82/01/30	*/

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

char	partab[];

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
	if((tp->t_state&(TS_TIMEOUT|TS_TTSTOP|TS_BUSY)) == 0 &&
	    tp->t_oproc)		/* kludge for pty */
		(*tp->t_oproc)(tp);
	splx(s);
}

/*
 * Common code for tty ioctls.
 */
/*ARGSUSED*/
ttioctl(tp, com, addr, flag)
register struct tty *tp;
caddr_t addr;
{
	int dev;
	unsigned t;
	struct sgttyb iocb;
	struct clist tq;
	extern int nldisp;
	register c;
	int temp;

	/*
	 * This is especially so that isatty() will
	 * fail when carrier is gone.
	 */
	if ((tp->t_state&TS_CARR_ON) == 0) {
		u.u_error = EBADF;
		return (1);
	}

	dev = tp->t_dev;
	/*
	 * If the ioctl involves modification,
	 * insist on being able to write the device,
	 * and hang if in the background.
	 */
	switch(com) {

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
	case TIOCSTI:
/* this is reasonable, but impractical... 
		if ((flag & FWRITE) == 0) {
			u.u_error = EBADF;
			return (1);
		}
 */
		while (tp->t_line == NTTYDISC &&
		   u.u_procp->p_pgrp != tp->t_pgrp && tp == u.u_ttyp &&
		   (u.u_procp->p_flag&SVFORK) == 0 &&
		   u.u_signal[SIGTTOU] != SIG_IGN &&
		   u.u_signal[SIGTTOU] != SIG_HOLD
/*
						   &&
		   (u.u_procp->p_flag&SDETACH)==0) {
*/
		   ) {
			gsignal(u.u_procp->p_pgrp, SIGTTOU);
			sleep((caddr_t)&lbolt, TTOPRI);
		}
		break;
	}

	/*
	 * Process the ioctl.
	 */
	switch(com) {

	/*
	 * Get discipline number
	 */
	case TIOCGETD:
		t = tp->t_line;
		if (copyout((caddr_t)&t, addr, sizeof(t)))
			u.u_error = EFAULT;
		break;

	/*
	 * Set line discipline
	 */
	case TIOCSETD:
		if (copyin(addr, (caddr_t)&t, sizeof(t))) {
			u.u_error = EFAULT;
			break;
		}
		if (t >= nldisp) {
			u.u_error = ENXIO;
			break;
		}
		(void) spl5();
		if (tp->t_line)
			(*linesw[tp->t_line].l_close)(tp);
		if (t)
			(*linesw[t].l_open)(dev, tp, addr);
		if (u.u_error==0)
			tp->t_line = t;
		(void) spl0();
		break;

	/*
	 * Prevent more opens on channel
	 */
	case TIOCEXCL:
		tp->t_state |= TS_XCLUDE;
		break;

	case TIOCNXCL:
		tp->t_state &= ~TS_XCLUDE;
		break;

	/*
	 * Set new parameters
	 */
	case TIOCSETP:
	case TIOCSETN:
		if (copyin(addr, (caddr_t)&iocb, sizeof(iocb))) {
			u.u_error = EFAULT;
			return(1);
		}
		(void) spl5();
		if (tp->t_flags&RAW || iocb.sg_flags&RAW ||
		    com == TIOCSETP)
			wflushtty(tp);
		else if ((tp->t_flags&CBREAK) != (iocb.sg_flags&CBREAK)) {
			if (iocb.sg_flags & CBREAK) {
				catq(&tp->t_rawq, &tp->t_canq);
				tq = tp->t_rawq;
				tp->t_rawq = tp->t_canq;
				tp->t_canq = tq;
			} else {
				tp->t_local |= LPENDIN;
				ttwakeup(tp);
			}
		}
		tp->t_ispeed = iocb.sg_ispeed;
		tp->t_ospeed = iocb.sg_ospeed;
		tp->t_erase = iocb.sg_erase;
		tp->t_kill = iocb.sg_kill;
		tp->t_flags = iocb.sg_flags;
		if (tp->t_flags & RAW) {
			tp->t_state &= ~TS_TTSTOP;
			ttstart(tp);
		}
		(void) spl0();
		break;

	/*
	 * Send current parameters to user
	 */
	case TIOCGETP:
		iocb.sg_ispeed = tp->t_ispeed;
		iocb.sg_ospeed = tp->t_ospeed;
		iocb.sg_erase = tp->t_erase;
		iocb.sg_kill = tp->t_kill;
		iocb.sg_flags = tp->t_flags;
		if (copyout((caddr_t)&iocb, addr, sizeof(iocb)))
			u.u_error = EFAULT;
		break;

	/*
	 * Hang up line on last close
	 */
	case TIOCHPCL:
		tp->t_state |= TS_HUPCLS;
		break;

	case TIOCFLUSH: {
		int flags;
		if (addr == 0)
			flags = FREAD|FWRITE;
		else if (copyin(addr, (caddr_t)&flags, sizeof (flags))) {
			u.u_error = EFAULT;
			return(1);
		}
		flushtty(tp, flags);
		break;
	}

	case FIONBIO: {
		int nbio;
		if (copyin(addr, (caddr_t)&nbio, sizeof (nbio))) {
			u.u_error = EFAULT;
			return(1);
		}
		if (nbio)
			tp->t_state |= TS_NBIO;
		else
			tp->t_state &= ~TS_NBIO;
		break;
	}

	/*
	 * Set and fetch special characters
	 */
	case TIOCSETC:
		if (copyin(addr, (caddr_t)&tun, sizeof(struct tchars)))
			u.u_error = EFAULT;
		break;

	case TIOCGETC:
		if (copyout((caddr_t)&tun, addr, sizeof(struct tchars)))
			u.u_error = EFAULT;
		break;

/* local ioctls */
	/*
	 * Set/get local special characters.
	 */
	case TIOCSLTC:
		if (copyin(addr, (caddr_t)&tlun, sizeof (struct ltchars)))
			u.u_error = EFAULT;
		break;

	case TIOCGLTC:
		if (copyout((caddr_t)&tlun, addr, sizeof (struct ltchars)))
			u.u_error = EFAULT;
		break;

	/*
	 * Return number of characters immediately available.
	 */
	case FIONREAD: {
		off_t nread = ttnread(tp);
		if (copyout((caddr_t)&nread, addr, sizeof (off_t)))
			u.u_error = EFAULT;
		break;
		}

	/*
	 * Should allow SPGRP and GPGRP only if tty open for reading.
	 */
	case TIOCSPGRP:
		if (copyin(addr, (caddr_t)&tp->t_pgrp, sizeof (tp->t_pgrp)))
			u.u_error = EFAULT;
		break;

	case TIOCGPGRP:
		if (copyout((caddr_t)&tp->t_pgrp, addr, sizeof(tp->t_pgrp)))
			u.u_error = EFAULT;
		break;

	/*
	 * Modify local mode word.
	 */
	case TIOCLBIS:
		if (copyin(addr, (caddr_t)&temp, sizeof (tp->t_local)))
			u.u_error = EFAULT;
		else
			tp->t_local |= temp;
		break;

	case TIOCLBIC:
		if (copyin(addr, (caddr_t)&temp, sizeof (tp->t_local)))
			u.u_error = EFAULT;
		else
			tp->t_local &= ~temp;
		break;

	case TIOCLSET:
		if (copyin(addr, (caddr_t)&temp, sizeof (tp->t_local)))
			u.u_error = EFAULT;
		else
			tp->t_local = temp;
		break;

	case TIOCLGET:
		if (copyout((caddr_t)&tp->t_local, addr, sizeof(tp->t_local)))
			u.u_error = EFAULT;
		break;

	/*
	 * Return number of characters in
	 * the output.
	 */
	case TIOCOUTQ:
		if (copyout((caddr_t)&tp->t_outq.c_cc, addr, sizeof(tp->t_outq.c_cc)))
			u.u_error = EFAULT;
		break;

	/*
	 * Simulate typing of a character at the terminal.
	 */
	case TIOCSTI:
		c = fubyte(addr);
		if (u.u_uid && u.u_ttyp != tp || c < 0)
			u.u_error = EFAULT;
		else
			(*linesw[tp->t_line].l_rint)(c, tp);
		break;

	case TIOCSTOP:
		c = spl5();
		if ((tp->t_state & TS_TTSTOP) == 0) {
			tp->t_state |= TS_TTSTOP;
			(*cdevsw[major(tp->t_dev)].d_stop)(tp, 0);
		}
		splx(c);
		break;

	case TIOCSTART:
		c = spl5();
		if ((tp->t_state & TS_TTSTOP) || (tp->t_local & LFLUSHO)) {
			tp->t_state &= ~TS_TTSTOP;
			tp->t_local &= ~LFLUSHO;
			ttstart(tp);
		}
		splx(c);
		break;

/* end of locals */

	default:
		return(0);
	}
	return(1);
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
