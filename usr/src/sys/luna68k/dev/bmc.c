/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)bmc.c	7.1 (Berkeley) %G%
 */

#define	BMC_NOCONSOLE
#define	BMD

#ifdef	BMD
#define	BMC_CNPORT	1
#else
#define	BMC_CNPORT	0
#endif

#include "bmc.h"
#if NBMC > 0

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/ioctl.h"
#include "sys/proc.h"
#include "sys/tty.h"
#include "sys/conf.h"
#include "sys/file.h"
#include "sys/uio.h"
#include "sys/kernel.h"
#include "sys/syslog.h"

#include "device.h"
#include "sioreg.h"
#include "siovar.h"

#ifdef	BMD
#include "kbdreg.h"
#endif

extern	struct sio_portc *sio_port_assign();

int     bmcprobe();
int     bmcopen();
void    bmcstart();
int     bmcparam();
int     bmcintr();

struct	driver bmcdriver = {
	bmcprobe, "bmc",
};

struct	bmc_softc {
	struct sio_portc *sc_pc;
	int	sc_mask;
};

struct	bmc_softc bmc_softc[NBMC];

struct	tty bmc_tty[NBMC];

int	bmc_config_done = 0;

int	bmcconsole;
int	bmcdefaultrate = B9600;				/* speed of console line is fixed */
int	bmcmajor = 0;

#define	bmcunit(x)		minor(x)


/*
 *  probe routine
 */

bmcprobe(hd)
	register struct hp_device *hd;
{
}

bmcinit(port)
	register int port;
{
	register struct bmc_softc *sc = &bmc_softc[0];

	/*
	 * if BMC is already configured, should be skipped.
         */
	if (bmc_config_done)
		return(0);

	/*
	 * Check out bitmap Interface board
	 */

	/* port checking (for keyboard) */
	if (port != 1)
		return(0);

	/* locate the major number */
	for (bmcmajor = 0; bmcmajor < nchrdev; bmcmajor++)
		if (cdevsw[bmcmajor].d_open == bmcopen)
			break;

	sc->sc_pc = sio_port_assign(port, bmcmajor, 0, bmcintr);

	printf("bmc%d: port %d, address 0x%x\n", sc->sc_pc->pc_unit, port, sc->sc_pc->pc_addr);

#ifdef	BMD
	bmdinit();
#endif

	bmc_config_done = 1;
	return(1);
}


/*
 *  entry routines
 */

/* ARGSUSED */
#ifdef __STDC__
bmcopen(dev_t dev, int flag, int mode, struct proc *p)
#else
bmcopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
#endif
{
	register struct tty *tp;
	register int unit;
	int error = 0;

	unit = bmcunit(dev);
	if (unit >= NBMC)
		return (ENXIO);
	tp = &bmc_tty[unit];
	tp->t_oproc = bmcstart;
	tp->t_param = bmcparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
/*			tp->t_cflag = (CREAD | CS8 | HUPCL);	*/
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = bmcdefaultrate;
		}
		bmcparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && p->p_ucred->cr_uid != 0)
		return (EBUSY);
	tp->t_state |= TS_CARR_ON;
	(void) spltty();
	while ((flag&O_NONBLOCK) == 0 && (tp->t_cflag&CLOCAL) == 0 &&
	       (tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	(void) spl0();
	if (error == 0)
		error = (*linesw[tp->t_line].l_open)(dev, tp);

	return (error);
}

/*ARGSUSED*/
bmcclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register int unit;

	unit = bmcunit(dev);
	tp = &bmc_tty[unit];
	(*linesw[tp->t_line].l_close)(tp, flag);
	ttyclose(tp);
	return (0);
}
 
bmcread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &bmc_tty[bmcunit(dev)];
 
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}
 
bmcwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &bmc_tty[bmcunit(dev)];

	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*
 * Stop output on a line.
 */
/*ARGSUSED*/
bmcstop(tp, flag)
	register struct tty *tp;
{
	register int s;

	s = spltty();
	if (tp->t_state & TS_BUSY) {
		if ((tp->t_state&TS_TTSTOP)==0)
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}

bmcioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct tty *tp;
	register int unit = bmcunit(dev);
	register int error;

	tp = &bmc_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {
	default:
		return (ENOTTY);
	}
	return (0);
}

/*
 *
 */
#ifdef BMD
void
bmcstart(tp)
	register struct tty *tp;
{
	int unit = bmcunit(tp->t_dev);
	register struct bmc_softc *sc = &bmc_softc[unit];
	register int cc, s;
	int hiwat = 0;

	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP)) {
		splx(s);
		return;
	}
	tp->t_state |= TS_BUSY;
	cc = tp->t_outq.c_cc;
	if (cc <= tp->t_lowat) {
		if (tp->t_state & TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	/*
	 * Limit the amount of output we do in one burst
	 * to prevent hogging the CPU.
	 */
/*
	if (cc > iteburst) {
		hiwat++;
		cc = iteburst;
	}
 */
	while (--cc >= 0) {
		register int c;

		c = getc(&tp->t_outq);
		/*
		 * iteputchar() may take a long time and we don't want to
		 * block all interrupts for long periods of time.  Since
		 * there is no need to stay at high priority while outputing
		 * the character (since we don't have to worry about
		 * interrupts), we don't.  We just need to make sure that
		 * we don't reenter iteputchar, which is guarenteed by the
		 * earlier setting of TS_BUSY.
		 */
		splx(s);
		bmdputc(c & sc->sc_mask);
		spltty();
	}
/*
	if (hiwat) {
		tp->t_state |= TS_TIMEOUT;
		timeout(ttrstrt, tp, 1);
	}
 */
	tp->t_state &= ~TS_BUSY;
	splx(s);
}
#else
void
bmcstart(tp)
	register struct tty *tp;
{
	register struct siodevice *sio;
	register int rr;
	int s, unit, c;
 
	unit = bmcunit(tp->t_dev);
	sio = bmc_softc[unit].sc_pc->pc_addr;
	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= tp->t_lowat) {
		if (tp->t_state&TS_ASLEEP) {
			tp->t_state &= ~TS_ASLEEP;
			wakeup((caddr_t)&tp->t_outq);
		}
		selwakeup(&tp->t_wsel);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	rr = siogetreg(sio);
	if (rr & RR_TXRDY) {
		c = getc(&tp->t_outq);
		tp->t_state |= TS_BUSY;
		sio->sio_data = c;
	}
out:
	splx(s);
}
#endif

bmcparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	int unit = bmcunit(tp->t_dev);
	register struct bmc_softc *sc = &bmc_softc[unit];
	register int cflag = t->c_cflag;
 
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	/*
	 * change line speed
	 */

	switch (cflag&CSIZE) {
	case CS5:
		sc->sc_mask = 0x1F ; break;
	case CS6:
		sc->sc_mask = 0x3F ; break;
	case CS7:
		sc->sc_mask = 0x7F ; break;
	case CS8:
		sc->sc_mask = 0xFF ; break;
	}

	/*
	 * parity
	 */

	/*
	 * stop bit
	 */

	return (0);
}


/*
 *  interrupt handling 
 */

#ifdef BMD
bmcintr(unit)
	register int unit;
{
	register struct siodevice *sio = bmc_softc[unit].sc_pc->pc_addr;
	register struct tty *tp;
	register u_char code;
	register int c;
	int s, rr;

	tp = &bmc_tty[unit];
	rr = siogetreg(sio);

	if (rr & RR_RXRDY) {
		code = sio->sio_data;
		c = kbd_decode(code);
		if (c & KC_TYPE)			/* skip special codes */
			return;
		code = (c & KC_CHAR);
		if ((tp->t_state & TS_ISOPEN) != 0)
			(*linesw[tp->t_line].l_rint)(code, tp);
	}
}
#else
bmcintr(unit)
	register int unit;
{
	register struct siodevice *sio = bmc_softc[unit].sc_pc->pc_addr;
	register u_char code;
	register struct tty *tp;
	int s, rr;

	tp = &bmc_tty[unit];
	rr = siogetreg(sio);

	if (rr & RR_RXRDY) {
		code = sio->sio_data;
		if ((tp->t_state & TS_ISOPEN) != 0)
			(*linesw[tp->t_line].l_rint)(code, tp);
	}

	if (rr & RR_TXRDY) {
		sio->sio_cmd = WR0_RSTPEND;
		tp->t_state &= ~(TS_BUSY|TS_FLUSH);
		if (tp->t_line)
			(*linesw[tp->t_line].l_start)(tp);
		else
			bmcstart(tp);
	}

}
#endif

/*
 * Following are all routines needed for SIO to act as console
 */
#include "../luna68k/cons.h"

bmccnprobe(cp)
	register struct consdev *cp;
{
#ifdef BMC_NOCONSOLE
	cp->cn_pri = CN_DEAD;
	return;
#else
	/* check DIP-SW setup */
	/* check bitmap interface board */

	/* locate the major number */
	for (bmcmajor = 0; bmcmajor < nchrdev; bmcmajor++)
		if (cdevsw[bmcmajor].d_open == bmcopen)
			break;

	/* initialize required fields */
	cp->cn_dev = makedev(bmcmajor, 0);
	cp->cn_tp  = &bmc_tty[0];
	cp->cn_pri = CN_INTERNAL;

	bmc_config_done = 1;
#endif
}

bmccninit(cp)
	struct consdev *cp;
{
	int unit = bmcunit(cp->cn_dev);
	register struct bmc_softc *sc = &bmc_softc[0];

	sioinit((struct siodevice *) SIO_HARDADDR, bmcdefaultrate);
#ifdef	BMD
	bmdinit();
#endif

	/* port assign */
	sc->sc_pc = sio_port_assign(BMC_CNPORT, bmcmajor, 0, bmcintr);

	bmcconsole = unit;
}

bmccngetc(dev)
	dev_t dev;
{
	struct bmc_softc *sc = &bmc_softc[bmcunit(dev)];
	struct sio_portc *pc = sc->sc_pc;
#ifdef	BMD
	register int c;
	register u_char code;

	do {
		code = sio_imgetc(pc->pc_addr);
	} while ((c = kbd_decode(code)) & KC_TYPE);

	return(c);
#else
	return(sio_imgetc(pc->pc_addr));
#endif
}

bmccnputc(dev, c)
	dev_t dev;
	int c;
{
	struct bmc_softc *sc = &bmc_softc[bmcunit(dev)];
	struct sio_portc *pc = sc->sc_pc;

#ifdef BMD
	bmdputc(c);
#else
	sio_imputc(pc->pc_addr, c);
#endif
}
#endif
