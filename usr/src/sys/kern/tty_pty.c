/*	tty_pty.c	4.10	81/11/18	*/

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
	int	pti_flags;
	struct	clist pti_ioctl, pti_ioans;
	int	pti_gensym;
	struct	proc *pti_selr;
} pt_ioctl[NPTY];

#define	PTCRCOLL	0x01

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
	if ((tp->t_state & ISOPEN) == 0) {
		ttychars(tp);		/* Set up default chars */
		tp->t_flags = 0;	/* No features (nor raw mode) */
	} else if (tp->t_state&XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return;
	}
	if (tp->t_oproc)			/* Ctrlr still around. */
		tp->t_state |= CARR_ON;
	while ((tp->t_state & CARR_ON) == 0) {
		tp->t_state |= WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	(*linesw[tp->t_line].l_open)(dev, tp);
}

ptsclose(dev)
dev_t dev;
{					/* Close slave part of PTY */
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	(*linesw[tp->t_line].l_close)(tp);
}

ptsread(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	if (tp->t_oproc) {
		(*linesw[tp->t_line].l_read)(tp);
		wakeup((caddr_t)&tp->t_rawq.c_cf);
	}
}

ptswrite(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	if (tp->t_oproc)
		(*linesw[tp->t_line].l_write)(tp);
}

ptsstart(tp)
	struct tty *tp;
{
	struct pt_ioctl *pti = &pt_ioctl[minor(tp->t_dev)];

	if (tp->t_state & TTSTOP)
		return;
	if (pti->pti_selr) {
		selwakeup(pti->pti_selr, pti->pti_flags & PTCRCOLL);
		pti->pti_selr = 0;
		pti->pti_flags &= ~PTCRCOLL;
	}
	wakeup((caddr_t)&tp->t_outq.c_cf);
}

/*ARGSUSED*/
ptcopen(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;

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
	if (tp->t_state & WOPEN)
		wakeup((caddr_t)&tp->t_rawq);
	tp->t_state |= CARR_ON;
}

ptcclose(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	if (tp->t_state & ISOPEN)
		gsignal(tp->t_pgrp, SIGHUP);
	tp->t_state &= ~CARR_ON;	/* virtual carrier gone */
	flushtty(tp, FREAD|FWRITE);
	tp->t_oproc = 0;		/* mark closed */
}

ptcread(dev)
dev_t dev;
{
	register struct tty *tp;

	tp = &pt_tty[minor(dev)];
	if ((tp->t_state&(CARR_ON|ISOPEN)) == 0)
		return;
	while (tp->t_outq.c_cc == 0 || (tp->t_state&TTSTOP))
		sleep((caddr_t)&tp->t_outq.c_cf, TTIPRI);
	while (tp->t_outq.c_cc && passc(getc(&tp->t_outq)) >= 0);
	if (tp->t_outq.c_cc <= TTLOWAT(tp)  && (tp->t_state&ASLEEP)) {
		tp->t_state &= ~ASLEEP;
		wakeup((caddr_t)&tp->t_outq);
	}
}

ptcselect(dev)
	dev_t dev;
{
	register struct tty *tp = &pt_tty[minor(dev)];
	struct pt_ioctl *pti;
	struct proc *p;

	if ((tp->t_state&(CARR_ON|ISOPEN)) == 0)
		return (1);
	if (tp->t_outq.c_cc)
		return (1);
	pti = &pt_ioctl[minor(dev)];
	if ((p = pti->pti_selr) && p->p_wchan == (caddr_t)&selwait)
		pti->pti_flags |= PTCRCOLL;
	else
		pti->pti_selr = u.u_procp;
	return (0);
}

ptcwrite(dev)
dev_t dev;
{
	register struct tty *tp;
	register char *cp, *ce;
	register int cc;
	char locbuf[BUFSIZ];

	tp = &pt_tty[minor(dev)];
	if ((tp->t_state&(CARR_ON|ISOPEN)) == 0)
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
				/* Better than just flushing it! */
				/* Wait for something to be read */
				sleep((caddr_t)&tp->t_rawq.c_cf, TTOPRI);
			}
			(*linesw[tp->t_line].l_rint)(*cp++, tp);
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
	if (cdevsw[major(dev)].d_open == ptcopen && cmd == TIOCSETP)
		while (getc(&tp->t_outq) >= 0);
	if (ttioctl(tp, cmd, addr, dev) == 0)
		u.u_error = ENOTTY;
}
#endif
