/*	tty_pty.c	4.15	82/01/17	*/

/*
 * Pseudo-teletype Driver
 * (Actually two drivers, requiring two entries in 'cdevsw')
 */
#include "pty.h"

#if NPTY > 0
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/tty.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/conf.h"
#include "../h/buf.h"
#include "../h/file.h"
#include "../h/proc.h"
#undef	NPTY

#define NPTY 16

#define BUFSIZ 100		/* Chunk size iomoved from user */

/*
 * pts == /dev/tty[pP]?
 * ptc == /dev/ptp[pP]?
 */
struct	tty pt_tty[NPTY];
struct	pt_ioctl {
	int	pt_flags;
	int	pt_gensym;
	struct	proc *pt_selr, *pt_selw;
	int	pt_send;
} pt_ioctl[NPTY];

#define	PF_RCOLL	0x01
#define	PF_WCOLL	0x02
#define	PF_NBIO		0x04
#define	PF_PKT		0x08		/* packet mode */
#define	PF_FLOWCTL	0x10		/* peers flow control mode */

/*ARGSUSED*/
ptsopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;

	if (minor(dev) >= NPTY) {
		u.u_error = ENXIO;
		return;
	}
	tp = &pt_tty[minor(dev)];
	if ((tp->t_state & TS_ISOPEN) == 0) {
		ttychars(tp);		/* Set up default chars */
		tp->t_flags = 0;	/* No features (nor raw mode) */
	} else if (tp->t_state&TS_XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return;
	}
	if (tp->t_oproc)			/* Ctrlr still around. */
		tp->t_state |= TS_CARR_ON;
	while ((tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	(*linesw[tp->t_line].l_open)(dev, tp);
}

ptsclose(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	(*linesw[tp->t_line].l_close)(tp);
}

ptsread(dev)
	dev_t dev;
{
	register struct tty *tp;
	register struct pt_ioctl *pti;

	tp = &pt_tty[minor(dev)];
	if (tp->t_oproc) {
		(*linesw[tp->t_line].l_read)(tp);
		wakeup((caddr_t)&tp->t_rawq.c_cf);
		if (tp->t_rawq.c_cc < TTYHOG/2 &&
		    (pti = &pt_ioctl[minor(tp->t_dev)])->pt_selw) {
			selwakeup(pti->pt_selw, pti->pt_flags & PF_WCOLL);
			pti->pt_selw = 0;
			pti->pt_flags &= ~PF_WCOLL;
		}
	}
}

/*
 * Write to pseudo-tty.
 * Wakeups of controlling tty will happen
 * indirectly, when tty driver calls ptsstart.
 */
ptswrite(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	if (tp->t_oproc)
		(*linesw[tp->t_line].l_write)(tp);
}

/*
 * Start output on pseudo-tty.
 * Wake up process selecting or sleeping for input from controlling tty.
 */
ptsstart(tp)
	struct tty *tp;
{

	if (tp->t_state & TS_TTSTOP)
		return;
	ptcwakeup(tp);
}

ptcwakeup(tp)
	struct tty *tp;
{
	struct pt_ioctl *pti = &pt_ioctl[minor(tp->t_dev)];

	if (pti->pt_selr) {
		selwakeup(pti->pt_selr, pti->pt_flags & PF_RCOLL);
		pti->pt_selr = 0;
		pti->pt_flags &= ~PF_RCOLL;
	}
	wakeup((caddr_t)&tp->t_outq.c_cf);
}

/*ARGSUSED*/
ptcopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;
	struct pt_ioctl *pti;

	if (minor(dev) >= NPTY) {
		u.u_error = ENXIO;
		return;
	}
	tp = &pt_tty[minor(dev)];
	if (tp->t_oproc) {
		u.u_error = EIO;
		return;
	}
	tp->t_oproc = ptsstart;
	if (tp->t_state & TS_WOPEN)
		wakeup((caddr_t)&tp->t_rawq);
	tp->t_state |= TS_CARR_ON;
	pti = &pt_ioctl[minor(dev)];
	pti->pt_flags = 0;
	pti->pt_send = 0;
}

ptcclose(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	if (tp->t_state & TS_ISOPEN)
		gsignal(tp->t_pgrp, SIGHUP);
	tp->t_state &= ~TS_CARR_ON;	/* virtual carrier gone */
	flushtty(tp, FREAD|FWRITE);
	tp->t_oproc = 0;		/* mark closed */
}

ptcread(dev)
	dev_t dev;
{
	register struct tty *tp;
	struct pt_ioctl *pti;

	tp = &pt_tty[minor(dev)];
	if ((tp->t_state&(TS_CARR_ON|TS_ISOPEN)) == 0)
		return;
	pti = &pt_ioctl[minor(dev)];
	if (pti->pt_flags & PF_PKT) {
		if (pti->pt_send) {
			passc(pti->pt_send);
			pti->pt_send = 0;
			return;
		}
		passc(0);
	}
	while (tp->t_outq.c_cc == 0 || (tp->t_state&TS_TTSTOP)) {
		if (pti->pt_flags&PF_NBIO) {
			u.u_error = EWOULDBLOCK;
			return;
		}
		sleep((caddr_t)&tp->t_outq.c_cf, TTIPRI);
	}
	while (tp->t_outq.c_cc && passc(getc(&tp->t_outq)) >= 0)
		;
	if (tp->t_outq.c_cc <= TTLOWAT(tp)) {
		if (tp->t_state&TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		if (tp->t_wsel) {
			selwakeup(tp->t_wsel, tp->t_state & TS_WCOLL);
			tp->t_wsel = 0;
			tp->t_state &= ~TS_WCOLL;
		}
	}
}

ptsstop(tp, flush)
	register struct tty *tp;
	int flush;
{
	struct pt_ioctl *pti = &pt_ioctl[minor(tp->t_dev)];

	if (flush == 0)
		return;
	pti->pt_send |= flush;
	ptcwakeup(tp);
}

ptcselect(dev, rw)
	dev_t dev;
	int rw;
{
	register struct tty *tp = &pt_tty[minor(dev)];
	struct pt_ioctl *pti;
	struct proc *p;
	int s;

	if ((tp->t_state&(TS_CARR_ON|TS_ISOPEN)) == 0)
		return (1);
	s = spl5();
	switch (rw) {

	case FREAD:
		if (tp->t_outq.c_cc && (tp->t_state&TS_TTSTOP) == 0) {
			splx(s);
			return (1);
		}
		pti = &pt_ioctl[minor(dev)];
		if ((p = pti->pt_selr) && p->p_wchan == (caddr_t)&selwait)
			pti->pt_flags |= PF_RCOLL;
		else
			pti->pt_selr = u.u_procp;
		break;

	case FWRITE:
		if (tp->t_rawq.c_cc + tp->t_canq.c_cc < TTYHOG/2) {
			splx(s);
			return (1);
		}
		pti = &pt_ioctl[minor(dev)];
		if ((p = pti->pt_selw) && p->p_wchan == (caddr_t)&selwait)
			pti->pt_flags |= PF_WCOLL;
		else
			pti->pt_selw = u.u_procp;
		break;
	}
	splx(s);
	return (0);
}

ptcwrite(dev)
	dev_t dev;
{
	register struct tty *tp;
	register char *cp, *ce;
	register int cc;
	char locbuf[BUFSIZ];
	int cnt = 0;

	tp = &pt_tty[minor(dev)];
	if ((tp->t_state&(TS_CARR_ON|TS_ISOPEN)) == 0)
		return;
	while (u.u_count) {
		cc = MIN(u.u_count, BUFSIZ);
		cp = locbuf;
		iomove(cp, (unsigned)cc, B_WRITE);
		if (u.u_error)
			break;
		ce = cp + cc;
		while (cp < ce) {
			while (tp->t_delct && tp->t_rawq.c_cc >= TTYHOG - 2) {
				wakeup((caddr_t)&tp->t_rawq);
				if (tp->t_state & TS_NBIO) {
					u.u_count += ce - cp;
					if (cnt == 0)
						u.u_error = EWOULDBLOCK;
					return;
				}
				/* Better than just flushing it! */
				/* Wait for something to be read */
				sleep((caddr_t)&tp->t_rawq.c_cf, TTOPRI);
			}
			(*linesw[tp->t_line].l_rint)(*cp++, tp);
			cnt++;
		}
	}
}

/*ARGSUSED*/
ptyioctl(dev, cmd, addr, flag)
	caddr_t addr;
	dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	/* IF CONTROLLER STTY THEN MUST FLUSH TO PREVENT A HANG ??? */
	if (cdevsw[major(dev)].d_open == ptcopen) {
		register struct pt_ioctl *pti = &pt_ioctl[minor(dev)];
		if (cmd == TIOCPKT) {
			int packet;
			if (copyin((caddr_t)addr, &packet, sizeof (packet))) {
				u.u_error = EFAULT;
				return;
			}
			if (packet)
				pti->pt_flags |= PF_PKT;
			else
				pti->pt_flags &= ~PF_PKT;
			return;
		}
		if (cmd == FIONBIO) {
			int nbio;
			if (copyin(addr, &nbio, sizeof (nbio))) {
				u.u_error = EFAULT;
				return;
			}
			if (nbio)
				pti->pt_flags |= PF_NBIO;
			else
				pti->pt_flags &= ~PF_NBIO;
			return;
		}
		if (cmd == TIOCSETP)
			while (getc(&tp->t_outq) >= 0);
	}
	if (ttioctl(tp, cmd, addr, dev) == 0)
		u.u_error = ENOTTY;
}
#endif
