/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty_pty.c	7.4 (Berkeley) %G%
 */

/*
 * Pseudo-teletype Driver
 * (Actually two drivers, requiring two entries in 'cdevsw')
 */
#include "pty.h"

#if NPTY > 0
#include "param.h"
#include "systm.h"
#include "ioctl.h"
#include "tty.h"
#include "dir.h"
#include "user.h"
#include "conf.h"
#include "file.h"
#include "proc.h"
#include "uio.h"
#include "kernel.h"
#include "tsleep.h"

#if NPTY == 1
#undef NPTY
#define	NPTY	32		/* crude XXX */
#endif

#define BUFSIZ 100		/* Chunk size iomoved to/from user */

/*
 * pts == /dev/tty[pqrs]?
 * ptc == /dev/pty[pqrs]?
 */
struct	tty pt_tty[NPTY];
struct	pt_ioctl {
	int	pt_flags;
	struct	proc *pt_selr, *pt_selw;
	u_char	pt_send;
	u_char	pt_ucntl;
	struct	clist pt_ioc;
} pt_ioctl[NPTY];
int	npty = NPTY;		/* for pstat -t */

#define	PF_RCOLL	0x0001
#define	PF_WCOLL	0x0002
#define	PF_NBIO		0x0004
#define	PF_PKT		0x0008		/* packet mode */
#define	PF_STOPPED	0x0010		/* user told stopped */
#define	PF_REMOTE	0x0020		/* remote and flow controlled input */
#define	PF_NOSTOP	0x0040
#define	PF_UCNTL	0x0080		/* user control mode */
#define	PF_TIOC		0x0100		/* transparent control mode */
#define	PF_LIOC		0x0200		/* transparent control locked */
#define	PF_WIOC		0x0400		/* waiting for PF_LIOC to clear */
#define	PF_BLOCK	0x0800		/* block writes to slave */
#define	PF_OWAIT	0x1000		/* waiting for PF_BLOCK to clear */

/*ARGSUSED*/
ptsopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	int error;

#ifdef lint
	npty = npty;
#endif
	if (minor(dev) >= NPTY)
		return (ENXIO);
	tp = &pt_tty[minor(dev)];
	if ((tp->t_state & TS_ISOPEN) == 0) {
		ttychars(tp);		/* Set up default chars */
		tp->t_iflag = TTYDEF_IFLAG;
		tp->t_oflag = TTYDEF_OFLAG;
		tp->t_lflag = TTYDEF_LFLAG;
		tp->t_cflag = TTYDEF_CFLAG;
		tp->t_ispeed = tp->t_ospeed = TTYDEF_SPEED;
		ttsetwater(tp);		/* would be done in xxparam() */
	} else if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	if (tp->t_oproc)			/* Ctrlr still around. */
		tp->t_state |= TS_CARR_ON;
	while ((tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	error = (*linesw[tp->t_line].l_open)(dev, tp);
	ptcwakeup(tp, FREAD|FWRITE);
	return (error);
}

ptsclose(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	(*linesw[tp->t_line].l_close)(tp);
	ttyclose(tp);
	ptcwakeup(tp, FREAD|FWRITE);
	return (0);
}

ptsread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &pt_tty[minor(dev)];
	register struct pt_ioctl *pti = &pt_ioctl[minor(dev)];
	int error = 0;

again:
	if (pti->pt_flags & PF_REMOTE) {
		while (tp == u.u_ttyp && 
		       u.u_procp->p_pgrp->pg_id != tp->t_pgid){
			if ((u.u_procp->p_sigignore & sigmask(SIGTTIN)) ||
			    (u.u_procp->p_sigmask & sigmask(SIGTTIN)) ||
			    !u.u_procp->p_pgrp->pg_jobc ||
			    u.u_procp->p_flag&SVFORK)
				return (EIO);
			pgsignal(u.u_procp->p_pgrp, SIGTTIN);
			sleep((caddr_t)&lbolt, TTIPRI);
		}
		if (tp->t_canq.c_cc == 0) {
			if (tp->t_state & TS_NBIO)
				return (EWOULDBLOCK);
			sleep((caddr_t)&tp->t_canq, TTIPRI);
			goto again;
		}
		while (tp->t_canq.c_cc > 1 && uio->uio_resid > 0)
			if (ureadc(getc(&tp->t_canq), uio) < 0) {
				error = EFAULT;
				break;
			}
		if (tp->t_canq.c_cc == 1)
			(void) getc(&tp->t_canq);
		if (tp->t_canq.c_cc)
			return (error);
	} else
		if (tp->t_oproc)
			error = (*linesw[tp->t_line].l_read)(tp, uio);
	ptcwakeup(tp, FWRITE);
	return (error);
}

/*
 * Write to pseudo-tty.
 * Wakeups of controlling tty will happen
 * indirectly, when tty driver calls ptsstart.
 */
ptswrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &pt_tty[minor(dev)];
	register struct pt_ioctl *pti = &pt_ioctl[minor(dev)];

	if (tp->t_oproc == 0)
		return (EIO);

	while (pti->pt_flags & PF_BLOCK) {
		pti->pt_flags |= PF_OWAIT;
		sleep((caddr_t)pti + 1, TTOPRI);
	}

	return ((*linesw[tp->t_line].l_write)(tp, uio));
}

/*
 * Start output on pseudo-tty.
 * Wake up process selecting or sleeping for input from controlling tty.
 */
ptsstart(tp)
	struct tty *tp;
{
	register struct pt_ioctl *pti = &pt_ioctl[minor(tp->t_dev)];

	if (tp->t_state & TS_TTSTOP)
		return;
	if (pti->pt_flags & PF_STOPPED) {
		pti->pt_flags &= ~PF_STOPPED;
		pti->pt_send = TIOCPKT_START;
	}
	ptcwakeup(tp, FREAD);
}

ptcwakeup(tp, flag)
	struct tty *tp;
{
	struct pt_ioctl *pti = &pt_ioctl[minor(tp->t_dev)];

	if (flag & FREAD) {
		if (pti->pt_selr) {
			selwakeup(pti->pt_selr, pti->pt_flags & PF_RCOLL);
			pti->pt_selr = 0;
			pti->pt_flags &= ~PF_RCOLL;
		}
		wakeup((caddr_t)&tp->t_outq.c_cf);
	}
	if (flag & FWRITE) {
		if (pti->pt_selw) {
			selwakeup(pti->pt_selw, pti->pt_flags & PF_WCOLL);
			pti->pt_selw = 0;
			pti->pt_flags &= ~PF_WCOLL;
		}
		wakeup((caddr_t)&tp->t_rawq.c_cf);
	}
}

/*ARGSUSED*/
ptcopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;
	struct pt_ioctl *pti;

	if (minor(dev) >= NPTY)
		return (ENXIO);
	tp = &pt_tty[minor(dev)];
	if (tp->t_oproc)
		return (EIO);
	tp->t_oproc = ptsstart;
	(void)(*linesw[tp->t_line].l_modem)(tp, 1);
	pti = &pt_ioctl[minor(dev)];
	pti->pt_flags = 0;
	pti->pt_send = 0;
	pti->pt_ucntl = 0;
	return (0);
}

ptcclose(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	(void)(*linesw[tp->t_line].l_modem)(tp, 0);
	tp->t_state &= ~TS_CARR_ON;
	tp->t_oproc = 0;		/* mark closed */
	return (0);
}

ptcread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &pt_tty[minor(dev)];
	struct pt_ioctl *pti = &pt_ioctl[minor(dev)];
	char buf[BUFSIZ];
	int error = 0, cc;

	/*
	 * We want to block until the slave
	 * is open, and there's something to read;
	 * but if we lost the slave or we're NBIO,
	 * then return the appropriate error instead.
	 */
	for (;;) {
		if (tp->t_state&TS_ISOPEN) {
			if (pti->pt_flags&PF_PKT && pti->pt_send) {
				error = ureadc((int)pti->pt_send, uio);
				if (error)
					return (error);
				pti->pt_send = 0;
				return (0);
			}
			if (pti->pt_flags&PF_UCNTL && pti->pt_ucntl) {
				error = ureadc((int)pti->pt_ucntl, uio);
				if (error)
					return (error);
				pti->pt_ucntl = 0;
				return (0);
			}
			if (pti->pt_flags&PF_TIOC && pti->pt_ioc.c_cc) {
				if (uio->uio_resid < pti->pt_ioc.c_cc + 1)
					return (E2BIG);
				error = ureadc(TIOCPKT_TIOC, uio);
				while (error == 0 && pti->pt_ioc.c_cc > 0) {
					cc = q_to_b(&pti->pt_ioc, buf,
					    MIN(pti->pt_ioc.c_cc, BUFSIZ));
					if (cc <= 0)	/* impossible? */
						break;
					error = uiomove(buf, cc, UIO_READ, uio);
				}
				return (error);
			}
			if (tp->t_outq.c_cc && (tp->t_state&TS_TTSTOP) == 0)
				break;
		}
		if ((tp->t_state&TS_CARR_ON) == 0)
			return (EIO);
		if (pti->pt_flags&PF_NBIO)
			return (EWOULDBLOCK);
		sleep((caddr_t)&tp->t_outq.c_cf, TTIPRI);
	}
	if (pti->pt_flags & (PF_PKT|PF_UCNTL|PF_TIOC))
		error = ureadc(0, uio);
	while (uio->uio_resid > 0 && error == 0) {
		cc = q_to_b(&tp->t_outq, buf, MIN(uio->uio_resid, BUFSIZ));
		if (cc <= 0)
			break;
		error = uiomove(buf, cc, UIO_READ, uio);
	}
	if (tp->t_outq.c_cc <= TTLOWAT(tp) && !(pti->pt_flags & PF_BLOCK))
		ptswake(tp);
	return (error);
}

ptswake(tp)
	register struct tty *tp;
{
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

ptsstop(tp, flush)
	register struct tty *tp;
	int flush;
{
	struct pt_ioctl *pti = &pt_ioctl[minor(tp->t_dev)];
	int flag;

	/* note: FLUSHREAD and FLUSHWRITE already ok */
	if (flush == 0) {
		flush = TIOCPKT_STOP;
		pti->pt_flags |= PF_STOPPED;
	} else
		pti->pt_flags &= ~PF_STOPPED;
	pti->pt_send |= flush;
	/* change of perspective */
	flag = 0;
	if (flush & FREAD)
		flag |= FWRITE;
	if (flush & FWRITE)
		flag |= FREAD;
	ptcwakeup(tp, flag);
}

ptcselect(dev, rw)
	dev_t dev;
	int rw;
{
	register struct tty *tp = &pt_tty[minor(dev)];
	struct pt_ioctl *pti = &pt_ioctl[minor(dev)];
	struct proc *p;
	int s;

	if ((tp->t_state&TS_CARR_ON) == 0)
		return (1);
	switch (rw) {

	case FREAD:
		/*
		 * Need to block timeouts (ttrstart).
		 */
		s = spltty();
		if ((tp->t_state&TS_ISOPEN) &&
		     tp->t_outq.c_cc && (tp->t_state&TS_TTSTOP) == 0) {
			splx(s);
			return (1);
		}
		splx(s);
		/* FALLTHROUGH */

	case 0:					/* exceptional */
		if ((tp->t_state&TS_ISOPEN) &&
		    (pti->pt_flags&PF_PKT && pti->pt_send ||
		     pti->pt_flags&PF_TIOC && pti->pt_ioc.c_cc ||
		     pti->pt_flags&PF_UCNTL && pti->pt_ucntl))
			return (1);
		if ((p = pti->pt_selr) && p->p_wchan == (caddr_t)&selwait)
			pti->pt_flags |= PF_RCOLL;
		else
			pti->pt_selr = u.u_procp;
		break;


	case FWRITE:
		if (tp->t_state&TS_ISOPEN) {
			if (pti->pt_flags & PF_REMOTE) {
			    if (tp->t_canq.c_cc == 0)
				return (1);
			} else {
			    if (tp->t_rawq.c_cc + tp->t_canq.c_cc < TTYHOG-2)
				    return (1);
			    if (tp->t_canq.c_cc == 0 && (tp->t_iflag&ICANON))
				    return (1);
			}
		}
		if ((p = pti->pt_selw) && p->p_wchan == (caddr_t)&selwait)
			pti->pt_flags |= PF_WCOLL;
		else
			pti->pt_selw = u.u_procp;
		break;

	}
	return (0);
}

ptcwrite(dev, uio)
	dev_t dev;
	register struct uio *uio;
{
	register struct tty *tp = &pt_tty[minor(dev)];
	register struct iovec *iov;
	register char *cp;
	register int cc = 0;
	char locbuf[BUFSIZ];
	int cnt = 0;
	struct pt_ioctl *pti = &pt_ioctl[minor(dev)];
	int error = 0;

again:
	if ((tp->t_state&TS_ISOPEN) == 0)
		goto block;
	if (pti->pt_flags & PF_REMOTE) {
		if (tp->t_canq.c_cc)
			goto block;
		while (uio->uio_iovcnt > 0 && tp->t_canq.c_cc < TTYHOG - 1) {
			iov = uio->uio_iov;
			if (iov->iov_len == 0) {
				uio->uio_iovcnt--;	
				uio->uio_iov++;
				continue;
			}
			if (cc == 0) {
				cc = MIN(iov->iov_len, BUFSIZ);
				cc = MIN(cc, TTYHOG - 1 - tp->t_canq.c_cc);
				cp = locbuf;
				error = uiomove(cp, cc, UIO_WRITE, uio);
				if (error)
					return (error);
				/* check again for safety */
				if ((tp->t_state&TS_ISOPEN) == 0)
					return (EIO);
			}
			if (cc)
				(void) b_to_q(cp, cc, &tp->t_canq);
			cc = 0;
		}
		(void) putc(0, &tp->t_canq);
		ttwakeup(tp);
		wakeup((caddr_t)&tp->t_canq);
		return (0);
	}
	while (uio->uio_iovcnt > 0) {
		iov = uio->uio_iov;
		if (cc == 0) {
			if (iov->iov_len == 0) {
				uio->uio_iovcnt--;	
				uio->uio_iov++;
				continue;
			}
			cc = MIN(iov->iov_len, BUFSIZ);
			cp = locbuf;
			error = uiomove(cp, cc, UIO_WRITE, uio);
			if (error)
				return (error);
			/* check again for safety */
			if ((tp->t_state&TS_ISOPEN) == 0)
				return (EIO);
		}
		while (cc > 0) {
			if ((tp->t_rawq.c_cc + tp->t_canq.c_cc) >= TTYHOG - 2 &&
			   (tp->t_canq.c_cc > 0 || !(tp->t_iflag&ICANON))) {
				wakeup((caddr_t)&tp->t_rawq);
				goto block;
			}
			(*linesw[tp->t_line].l_rint)(*cp++, tp);
			cnt++;
			cc--;
		}
		cc = 0;
	}
	return (0);
block:
	/*
	 * Come here to wait for slave to open, for space
	 * in outq, or space in rawq.
	 */
	if ((tp->t_state&TS_CARR_ON) == 0)
		return (EIO);
	if (pti->pt_flags & PF_NBIO) {
		iov->iov_base -= cc;
		iov->iov_len += cc;
		uio->uio_resid += cc;
		uio->uio_offset -= cc;
		if (cnt == 0)
			return (EWOULDBLOCK);
		return (0);
	}
	sleep((caddr_t)&tp->t_rawq.c_cf, TTOPRI);
	goto again;
}

/*ARGSUSED*/
ptyioctl(dev, cmd, data, flag)
	caddr_t data;
	dev_t dev;
{
	register struct tty *tp = &pt_tty[minor(dev)];
	register struct pt_ioctl *pti = &pt_ioctl[minor(dev)];
	register u_char *cc = tp->t_cc;
	int stop, error;
	extern ttyinput();

	/*
	 * IF CONTROLLER STTY THEN MUST FLUSH TO PREVENT A HANG.
	 * ttywflush(tp) will hang if there are characters in the outq.
	 */
	if (cdevsw[major(dev)].d_open == ptcopen) {
		if ((cmd & 0xffff) == (TIOCIOANS(0) & 0xffff)) {
			if (!(pti->pt_flags & PF_LIOC) || pti->pt_ioc.c_cc)
				return (EINVAL);
			(void) b_to_q(data, IOCPARM_LEN(cmd), &pti->pt_ioc);
			wakeup((caddr_t)&pti->pt_ioc);
			return (0);
		}
		switch (cmd) {

		case TIOCPKT:
			if (*(int *)data) {
				if (pti->pt_flags & PF_UCNTL)
					return (EINVAL);
				pti->pt_flags |= PF_PKT;
			} else
				pti->pt_flags &= ~PF_PKT;
			return (0);

		case TIOCUCNTL:
			if (*(int *)data) {
				if (pti->pt_flags & PF_PKT)
					return (EINVAL);
				pti->pt_flags |= PF_UCNTL;
			} else
				pti->pt_flags &= ~PF_UCNTL;
			return (0);

		case TIOCTIOC:
			if (*(int *)data) {
				if (pti->pt_flags & PF_UCNTL)
					return (EINVAL);
				pti->pt_flags |= PF_TIOC;
			} else {
				pti->pt_flags &= ~(PF_TIOC|PF_LIOC|PF_WIOC);
				while (pti->pt_ioc.c_cc)
					(void) getc(&pti->pt_ioc);
				wakeup((caddr_t)&pti->pt_ioc);
			}
			return (0);

		case TIOCBLK:
			if (*(int *)data)
				pti->pt_flags |= PF_BLOCK;
			else {
				if (pti->pt_flags & PF_OWAIT)
					wakeup((caddr_t)pti + 1);
				pti->pt_flags &= ~(PF_BLOCK|PF_OWAIT);
				ptswake(tp);
			}
			return (0);

		case TIOCREMOTE:
			if (*(int *)data)
				pti->pt_flags |= PF_REMOTE;
			else
				pti->pt_flags &= ~PF_REMOTE;
			ttyflush(tp, FREAD|FWRITE);
			return (0);

		case FIONBIO:
			if (*(int *)data)
				pti->pt_flags |= PF_NBIO;
			else
				pti->pt_flags &= ~PF_NBIO;
			return (0);

		case FIONREAD:
			*(int *)data = tp->t_outq.c_cc;
			return (0);

		case TIOCSETP:		
		case TIOCSETN:
		case TIOCSETD:
		case TIOCSETA:
		case TIOCSETAW:
		case TIOCSETAF:
		case TIOCSETAS:
		case TIOCSETAWS:
		case TIOCSETAFS:
			while (getc(&tp->t_outq) >= 0)
				;
			break;
		}
	} else if (pti->pt_flags & PF_TIOC) {
		while (pti->pt_flags & PF_LIOC) {
			pti->pt_flags |= PF_WIOC;
			switch (tsleep((caddr_t)&pti->pt_flags,TTIPRI-1,5*hz)) {
			case TS_OK:
				continue;
			case TS_SIG:
			case TS_TIME:
				return (EBUSY);
			}
		}
		pti->pt_flags |= PF_LIOC | PF_BLOCK;
		while (pti->pt_ioc.c_cc)
			(void) getc(&pti->pt_ioc);
		(void) b_to_q(&cmd, sizeof cmd, &pti->pt_ioc);
		if (cmd & IOC_IN)
			(void) b_to_q(data, IOCPARM_LEN(cmd), &pti->pt_ioc);
		ptcwakeup(tp, FREAD);
		switch (tsleep((caddr_t)&pti->pt_ioc, TTIPRI-1, 5*hz)) {
		case TS_SIG:
		case TS_TIME:
			while (pti->pt_ioc.c_cc)
				(void) getc(&pti->pt_ioc);
			if (pti->pt_flags & PF_WIOC)
				wakeup((caddr_t)&pti->pt_flags);
			if (pti->pt_flags & PF_OWAIT)
				wakeup((caddr_t)pti + 1);
			pti->pt_flags &= ~(PF_LIOC|PF_WIOC|PF_BLOCK|PF_OWAIT);
			ptswake(tp);
			return (EBUSY);
		case TS_OK:
			break;
		}
		if (pti->pt_ioc.c_cc == 0) {
			if (pti->pt_flags & PF_WIOC)
				wakeup((caddr_t)&pti->pt_flags);
			if (pti->pt_flags & PF_OWAIT)
				wakeup((caddr_t)pti + 1);
			pti->pt_flags &= ~(PF_LIOC|PF_WIOC|PF_BLOCK|PF_OWAIT);
			ptswake(tp);
			goto doioctl;
		}
		if (q_to_b(&pti->pt_ioc, &error, sizeof error) != sizeof error)
			error = EINVAL;
		if (error == 0 && cmd & IOC_OUT) {
			if (IOCPARM_LEN(cmd) != pti->pt_ioc.c_cc)
				error = EINVAL;
			else
				(void) q_to_b(&pti->pt_ioc, data,
				    pti->pt_ioc.c_cc);
		}
		while (pti->pt_ioc.c_cc)
			(void) getc(&pti->pt_ioc);
		if (pti->pt_flags & PF_WIOC)
			wakeup((caddr_t)&pti->pt_flags);
		if (pti->pt_flags & PF_OWAIT)
			wakeup((caddr_t)pti + 1);
		pti->pt_flags &= ~(PF_LIOC|PF_WIOC|PF_BLOCK|PF_OWAIT);
		ptswake(tp);
		return (error);
	}

 doioctl:
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error < 0)
		 error = ttioctl(tp, cmd, data, flag);
	/*
	 * Since we use the tty queues internally,
	 * pty's can't be switched to disciplines which overwrite
	 * the queues.  We can't tell anything about the discipline
	 * from here...
	 *
	 * Nb: this is not really good enough, the line disc open routine
	 * may have done anything at all, no guarantees that close
	 * will fix it.  This also has the effect of losing the
	 * previous discipline, which an error on a TIOCSETD shouldn't
	 * do...  Sometime it should be done via an explicit check
	 * for TIOCSETD, then check to see what linesw[new_number].l_rint
	 * really is.
	 */
	if (linesw[tp->t_line].l_rint != ttyinput) {
		(*linesw[tp->t_line].l_close)(tp);
		tp->t_line = 0;
		(void)(*linesw[tp->t_line].l_open)(dev, tp);
		error = ENOTTY;
	}

	if (error < 0) {
		if (pti->pt_flags & PF_UCNTL &&
		    (cmd & ~0xff) == UIOCCMD(0)) {
			if (cmd & 0xff) {
				pti->pt_ucntl = (u_char)cmd;
				ptcwakeup(tp, FREAD);
			}
			return (0);
		}
		error = ENOTTY;
	}
	stop = (tp->t_iflag & IXON) && CCEQ(cc[VSTOP], CTRL('s')) 
		&& CCEQ(cc[VSTART], CTRL('q'));
	if (pti->pt_flags & PF_NOSTOP) {
		if (stop) {
			pti->pt_send &= ~TIOCPKT_NOSTOP;
			pti->pt_send |= TIOCPKT_DOSTOP;
			pti->pt_flags &= ~PF_NOSTOP;
			ptcwakeup(tp, FREAD);
		}
	} else {
		if (!stop) {
			pti->pt_send &= ~TIOCPKT_DOSTOP;
			pti->pt_send |= TIOCPKT_NOSTOP;
			pti->pt_flags |= PF_NOSTOP;
			ptcwakeup(tp, FREAD);
		}
	}
	return (error);
}
#endif
