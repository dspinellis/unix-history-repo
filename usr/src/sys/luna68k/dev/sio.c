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
 *	@(#)sio.c	7.5 (Berkeley) %G%
 */

/*
 * sio.c -- NEC uPD7201A UART Device Driver
 *    remaked by A.Fujita, NOV-5-1992
 */

#include "sio.h"
#if NSIO > 0

#include "bmc.h"

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/ioctl.h>
#include <sys/proc.h>
#include <sys/tty.h>
#include <sys/conf.h>
#include <sys/file.h>
#include <sys/uio.h>
#include <sys/kernel.h>
#include <sys/syslog.h>

#include <luna68k/dev/device.h>
#include <luna68k/dev/sioreg.h>
#include <luna68k/dev/siovar.h>

struct sio_portc *sio_port_assign();

int	sioprobe();
int	sioopen();
void	siostart();
int	sioparam();
int	siointr();

struct	driver siodriver = {
	sioprobe, "sio",
};

#define NPORT	2					/* uPD7201A has 2 serial-port */
#define NLINE	1					/* number of active line */

struct	sio_portc sio_portc[NPORT] = {
	{ -1, -1, (struct siodevice *) 0x51000000, (int (*)()) 0 },
	{ -1, -1, (struct siodevice *) 0x51000004, (int (*)()) 0 }
};

struct	sio_softc sio_softc[NLINE];

int	sio_init_done = 0;
int	siounitbase = 0;				/* This counter is used unit number assignment */

int	siosoftCAR;
int	sio_active;
int	sioconsole = -1;
int	siodefaultrate = TTYDEF_SPEED;
int	siomajor = 0;

struct	tty sio_tty[NLINE];

struct speedtab siospeedtab[] = {
	2400,	WR4_BAUD24,
	4800,	WR4_BAUD48,
	9600,	WR4_BAUD96,
};

#define	siounit(x)		minor(x)

extern	struct tty *constty;

/*
 *  probe routines
 */

sioprobe(hd)
	register struct hp_device *hd;
{
	register int port;
	register struct sio_portc *pc;
	register struct sio_softc *sc;

	sioinit((struct siodevice *) hd->hp_addr, siodefaultrate);

	/* locate the major number */
	for (siomajor = 0; siomajor < nchrdev; siomajor++)
		if (cdevsw[siomajor].d_open == sioopen)
			break;

	for (port = 0; port < NPORT; port++) {
		pc = &sio_portc[port];

		if (pc->pc_major != -1) {
			printf("%s%d: port %d, address 0x%x, intr 0x%x (console)\n",
			       (pc->pc_major == siomajor ? "sio" : "bmc" ),
			       pc->pc_unit, port, pc->pc_addr, pc->pc_intr);
			continue;
		}

		pc->pc_addr =
			(struct siodevice *)((u_long) hd->hp_addr + (sizeof(struct siodevice) * port));
#if NBMC > 0
		if (bmcinit(port))
			continue;
#endif
		if (++siounitbase < NLINE) {
			pc->pc_major = siomajor;
			pc->pc_intr  = siointr;
			pc->pc_unit  = siounitbase;
			printf("sio%d: port %d, address 0x%x\n", pc->pc_unit, port, pc->pc_addr);

			sc = &sio_softc[pc->pc_unit];
			sc->sc_pc = pc;

			sio_active |= 1 << pc->pc_unit;
			siosoftCAR |= 1 << pc->pc_unit;
		}
	}
}

struct sio_portc *
sio_port_assign(port, major, unit, intr)
	int	port, major, unit;
	int	(*intr)();
{
	register struct sio_portc *pc = &sio_portc[port];

	if (pc->pc_major != -1)
		return((struct sio_portc *) 0);

	pc->pc_major = major;
	pc->pc_intr  = intr;
	pc->pc_unit  = unit;

	return(pc);
}


/*
 *  entry routines
 */

/* ARGSUSED */
#ifdef __STDC__
sioopen(dev_t dev, int flag, int mode, struct proc *p)
#else
sioopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
#endif
{
	register struct tty *tp;
	register int unit;
	int error = 0;

	unit = siounit(dev);
	if (unit >= NLINE || (sio_active & (1 << unit)) == 0)
		return (ENXIO);
	tp = &sio_tty[unit];
	tp->t_oproc = siostart;
	tp->t_param = sioparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
/*			tp->t_cflag = TTYDEF_CFLAG;		*/
			tp->t_cflag = (CREAD | CS8 | HUPCL);
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = siodefaultrate;
		}
		sioparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && p->p_ucred->cr_uid != 0)
		return (EBUSY);
	(void) siomctl(dev, WR5_DTR | WR5_RTS, DMSET);
	if ((siosoftCAR & (1 << unit)) || (siomctl(dev, 0, DMGET) & RR0_DCD))
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
sioclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register int unit;

	unit = siounit(dev);
	tp = &sio_tty[unit];
	(*linesw[tp->t_line].l_close)(tp, flag);
	(void) siomctl(dev, WR5_BREAK, DMBIS);
	if (tp->t_cflag&HUPCL || tp->t_state&TS_WOPEN ||
	    (tp->t_state&TS_ISOPEN) == 0)
		(void) siomctl(dev, 0, DMSET);
	ttyclose(tp);
	return (0);
}
 
sioread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &sio_tty[siounit(dev)];
 
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}
 
siowrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register int unit = siounit(dev);
	register struct tty *tp = &sio_tty[unit];

	if ((unit == sioconsole) && constty &&
	    (constty->t_state&(TS_CARR_ON|TS_ISOPEN))==(TS_CARR_ON|TS_ISOPEN))
		tp = constty;

	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*
 * Stop output on a line.
 */
/*ARGSUSED*/
siostop(tp, flag)
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

sioioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct tty *tp;
	register int unit = siounit(dev);
	register int error;

	tp = &sio_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case TIOCSBRK:
		(void) siomctl(dev, WR5_BREAK, DMBIS);
		break;

	case TIOCCBRK:
		(void) siomctl(dev, WR5_BREAK, DMBIC);
		break;

	case TIOCSDTR:
		(void) siomctl(dev, WR5_DTR | WR5_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) siomctl(dev, WR5_DTR | WR5_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) siomctl(dev, *(int *)data, DMSET);
		break;

	case TIOCMBIS:
		(void) siomctl(dev, *(int *)data, DMBIS);
		break;

	case TIOCMBIC:
		(void) siomctl(dev, *(int *)data, DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = siomctl(dev, 0, DMGET);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}


/*
 *
 */

void
siostart(tp)
	register struct tty *tp;
{
	register struct siodevice *sio;
	register int rr;
	int s, unit, c;
 
	unit = siounit(tp->t_dev);
	sio = sio_softc[unit].sc_pc->pc_addr;
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

sioparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	int unit = siounit(tp->t_dev);
	register struct siodevice *sio;
	register cflag = t->c_cflag;
	register u_char wr;
	int ospeed = ttspeedtab(t->c_ospeed, siospeedtab);

	sio = sio_softc[unit].sc_pc->pc_addr;

	switch (cflag & CSIZE) {
	case CS5:
	case CS6:
	case CS7:
	case CS8:
		break;
	}

	wr = ospeed;

	if (cflag & PARENB) {
		wr |= WR4_PARENAB;
		if ((cflag&PARODD) == 0)
			wr |= WR4_EPARITY;
	}

	if (cflag & CSTOPB)
		wr |= WR4_STOP2;			/* 2 stop bit */
	else
		wr |= WR4_STOP1;			/* 1 stop bit */

	(void) sioreg(sio, WR4, wr);

	return (0);
}

siomctl()
{
	return (0);
}


/*
 *  Interrupt handling
 */

void
_siointr()
{
	register int port;
	register struct sio_portc *pc;

	for (port = 0; port < NPORT; port++) {
		pc = &sio_portc[port];

		if (pc->pc_major != -1)
			(pc->pc_intr)(pc->pc_unit);
	}
}

siointr(unit)
	register int unit;
{
	register struct siodevice *sio = sio_softc[unit].sc_pc->pc_addr;
	register u_char code;
	register struct tty *tp;
	int s, rr;

	tp = &sio_tty[unit];
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
			siostart(tp);
	}
}

/*
 * Following are all routines needed for SIO to act as console
 */
#include <luna68k/luna68k/cons.h>

siocnprobe(cp)
	register struct consdev *cp;
{
	register int unit = 0;

	/* locate the major number */
	for (siomajor = 0; siomajor < nchrdev; siomajor++)
		if (cdevsw[siomajor].d_open == sioopen)
			break;

	siounitbase = -1;

	/* initialize required fields */
	cp->cn_dev = makedev(siomajor, unit);
	cp->cn_tp  = &sio_tty[unit];
	cp->cn_pri = CN_NORMAL;
}

siocninit(cp)
	struct consdev *cp;
{
	int unit = siounit(cp->cn_dev);
	register struct sio_softc *sc = &sio_softc[unit];

	sioinit((struct siodevice *) SIO_HARDADDR, siodefaultrate);

	/* port assign */
	sc->sc_pc = sio_port_assign(0, siomajor, unit, siointr);

	sioconsole = unit;
	siounitbase = 0;
	
	sio_active |= 1 << unit;
	siosoftCAR |= 1 << unit;
}

siocngetc(dev)
	dev_t dev;
{
	struct sio_softc *sc = &sio_softc[siounit(dev)];
	struct sio_portc *pc = sc->sc_pc;

	return(sio_imgetc(pc->pc_addr));
}

siocnputc(dev, c)
	dev_t dev;
	int c;
{
	struct sio_softc *sc = &sio_softc[siounit(dev)];
	struct sio_portc *pc = sc->sc_pc;

	sio_imputc(pc->pc_addr, c);
}


/*
 *  sio raw-level routines
 */

sioinit(sio0, rate)
	register struct siodevice *sio0;
	register int rate;
{
	register struct siodevice *sio1;
	int s;

	rate = ttspeedtab(rate, siospeedtab);

	if (sio_init_done)
		return;

	sio1 = (struct siodevice *) ((u_long) sio0 + sizeof(struct siodevice));

	s = splhigh();

	sioreg(sio0, WR0,  WR0_CHANRST);		/* Channel-A Reset */

	sioreg(sio0, WR2, (WR2_VEC86  | WR2_INTR_1));	/* Set CPU BUS Interface Mode */
	sioreg(sio1, WR2,  0);				/* Set Interrupt Vector */

	sioreg(sio0, WR0,  WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(sio0, WR4, (rate | WR4_STOP1 | WR4_NPARITY));	/* Tx/Rx */
	sioreg(sio0, WR3, (WR3_RX8BIT | WR3_RXENBL));		/* Rx */
	sioreg(sio0, WR5, (WR5_TX8BIT | WR5_TXENBL));		/* Tx */
	sioreg(sio0, WR0,  WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(sio0, WR1, (WR1_RXALLS | WR1_TXENBL));

	sioreg(sio1, WR0,  WR0_CHANRST);		/* Channel-B Reset */

	sioreg(sio1, WR0,  WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(sio1, WR4, (rate | WR4_STOP1 | WR4_NPARITY));	/* Tx/Rx */
	sioreg(sio1, WR3, (WR3_RX8BIT | WR3_RXENBL));		/* Rx */
	sioreg(sio1, WR5, (WR5_TX8BIT | WR5_TXENBL));		/* Tx */
	sioreg(sio1, WR0,  WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(sio1, WR1, (WR1_RXALLS | WR1_TXENBL));

	splx(s);

	sio_init_done = 1;
}

sio_imgetc(sio)
	register struct siodevice *sio;
{
	register int rr0, rr1;
	int c, s;

	s = splhigh();
	while (((rr0 = sioreg(sio, RR0, 0)) & RR0_RXAVAIL) == 0)
		;
	c = sio->sio_data;
	sioreg(sio, WR0, WR0_RSTPEND);
	splx(s);
	return (c);
}

sio_imputc(sio, c)
	register struct siodevice *sio;
	int c;
{
	register u_char code;
	register int rr;
	int s;

	s = splhigh();

	sioreg(sio, WR1, WR1_RXALLS);

	do {
		DELAY(1);
		rr = siogetreg(sio);
	} while (!(rr & RR_TXRDY));

	code = (c & 0xFF);
	sio->sio_data = code;

	do {
		DELAY(1);
		rr = siogetreg(sio);
	} while (!(rr & RR_TXRDY));

	sioreg(sio, WR1, (WR1_RXALLS | WR1_TXENBL));

	splx(s);
}

/*
 *  uPD7201A register operation
 */

int
siogetreg(sio)
	register struct siodevice *sio;
{
	register int rr = 0;

	rr = sio->sio_stat;
	rr <<= 8;
	sio->sio_cmd = 1;	/* Select RR1 */
	rr |= sio->sio_stat;

	return(rr);
}

int
sioreg(sio, reg, val)
	register struct siodevice *sio;
	register int reg, val;
{
	if (isStatusReg(reg)) {
		if (reg != 0)
		    sio->sio_cmd = reg;
		val = sio->sio_stat;
	} else {
		if (reg != 0)
		    sio->sio_cmd = reg;
		sio->sio_cmd = val;
	}

	return(val);
}
#endif
