/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)dca.c	7.7 (Berkeley) 6/30/90
 */

#include "dca.h"
#if NDCA > 0
/*
 *  98626/98644/internal serial interface
 */
#include "param.h"
#include "systm.h"
#include "ioctl.h"
#include "tty.h"
#include "user.h"
#include "conf.h"
#include "file.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "device.h"
#include "dcareg.h"
#include "machine/cpu.h"
#include "machine/isr.h"

int	dcaprobe();
struct	driver dcadriver = {
	dcaprobe, "dca",
};

int	dcastart(), dcaparam(), dcaintr();
int	dcasoftCAR;
int	dca_active;
int	ndca = NDCA;
int	dcaconsole = -1;
int	dcadefaultrate = TTYDEF_SPEED;
struct	dcadevice *dca_addr[NDCA];
struct	tty dca_tty[NDCA];
struct	isr dcaisr[NDCA];

struct speedtab dcaspeedtab[] = {
	0,	0,
	50,	DCABRD(50),
	75,	DCABRD(75),
	110,	DCABRD(110),
	134,	DCABRD(134),
	150,	DCABRD(150),
	200,	DCABRD(200),
	300,	DCABRD(300),
	600,	DCABRD(600),
	1200,	DCABRD(1200),
	1800,	DCABRD(1800),
	2400,	DCABRD(2400),
	4800,	DCABRD(4800),
	9600,	DCABRD(9600),
	19200,	DCABRD(19200),
	38400,	DCABRD(38400),
	-1,	-1
};

extern	struct tty *constty;
#ifdef KGDB
extern int kgdb_dev;
extern int kgdb_rate;
extern int kgdb_debug_init;
#endif

#define	UNIT(x)		minor(x)

dcaprobe(hd)
	register struct hp_device *hd;
{
	register struct dcadevice *dca;
	register int unit;

	dca = (struct dcadevice *)hd->hp_addr;
	if (dca->dca_irid != DCAID0 &&
	    dca->dca_irid != DCAREMID0 &&
	    dca->dca_irid != DCAID1 &&
	    dca->dca_irid != DCAREMID1)
		return (0);
	unit = hd->hp_unit;
	if (unit == dcaconsole)
		DELAY(100000);
	dca->dca_irid = 0xFF;
	DELAY(100);

	hd->hp_ipl = DCAIPL(dca->dca_ic);
	dcaisr[unit].isr_ipl = hd->hp_ipl;
	dcaisr[unit].isr_arg = unit;
	dcaisr[unit].isr_intr = dcaintr;
	dca_addr[unit] = dca;
	dca_active |= 1 << unit;
	dcasoftCAR = hd->hp_flags;
	isrlink(&dcaisr[unit]);
#ifdef KGDB
	if (kgdb_dev == makedev(1, unit)) {
		if (dcaconsole == unit)
			kgdb_dev = -1;	/* can't debug over console port */
		else {
			(void) dcainit(unit);
			dcaconsole = -2; /* XXX */
			if (kgdb_debug_init) {
				printf("dca%d: kgdb waiting...", unit);
				/* trap into kgdb */
				asm("trap #15;");
				printf("connected.\n");
			} else
				printf("dca%d: kgdb enabled\n", unit);
		}
	}
#endif
	dca->dca_ic = IC_IE;
	/*
	 * Need to reset baud rate, etc. of next print so reset dcaconsole.
	 * Also make sure console is always "hardwired"
	 */
	if (unit == dcaconsole) {
		dcaconsole = -1;
		dcasoftCAR |= (1 << unit);
	}
	return (1);
}

dcaopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;
	int error = 0;
 
	unit = UNIT(dev);
	if (unit >= NDCA || (dca_active & (1 << unit)) == 0)
		return (ENXIO);
	tp = &dca_tty[unit];
	tp->t_oproc = dcastart;
	tp->t_param = dcaparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
		tp->t_iflag = TTYDEF_IFLAG;
		tp->t_oflag = TTYDEF_OFLAG;
		tp->t_cflag = TTYDEF_CFLAG;
		tp->t_lflag = TTYDEF_LFLAG;
		tp->t_ispeed = tp->t_ospeed = dcadefaultrate;
		dcaparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	(void) dcamctl(dev, MCR_DTR | MCR_RTS, DMSET);
	if ((dcasoftCAR & (1 << unit)) || (dcamctl(dev, 0, DMGET) & MSR_DCD))
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
dcaclose(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register struct dcadevice *dca;
	register int unit;
 
	unit = UNIT(dev);
	dca = dca_addr[unit];
	tp = &dca_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	dca->dca_cfcr &= ~CFCR_SBREAK;
#ifdef KGDB
	/* do not disable interrupts if debugging */
	if (kgdb_dev != makedev(1, unit))
#endif
	dca->dca_ier = 0;
	if (tp->t_cflag&HUPCL || tp->t_state&TS_WOPEN || 
	    (tp->t_state&TS_ISOPEN) == 0)
		(void) dcamctl(dev, 0, DMSET);
	ttyclose(tp);
	return(0);
}
 
dcaread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &dca_tty[UNIT(dev)];
 
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}
 
dcawrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	int unit = UNIT(dev);
	register struct tty *tp = &dca_tty[unit];
 
	/*
	 * (XXX) We disallow virtual consoles if the physical console is
	 * a serial port.  This is in case there is a display attached that
	 * is not the console.  In that situation we don't need/want the X
	 * server taking over the console.
	 */
	if (constty && unit == dcaconsole)
		constty = NULL;
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}
 
dcaintr(unit)
	register int unit;
{
	register struct dcadevice *dca;
	register u_char code;
	register struct tty *tp;

	dca = dca_addr[unit];
	if ((dca->dca_ic & IC_IR) == 0)
		return(0);
	while (1) {
		code = dca->dca_iir;
		switch (code) {
		case IIR_NOPEND:
			return (1);
		case IIR_RXRDY:
			/* do time-critical read in-line */
			tp = &dca_tty[unit];
			code = dca->dca_data;
			if ((tp->t_state & TS_ISOPEN) == 0) {
#ifdef KGDB
				if (kgdb_dev == makedev(1, unit) &&
				    code == '!') {
					printf("kgdb trap from dca%d\n", unit);
					/* trap into kgdb */
					asm("trap #15;");
				}
#endif
			} else
				(*linesw[tp->t_line].l_rint)(code, tp);
			break;
		case IIR_TXRDY:
			tp = &dca_tty[unit];
			tp->t_state &=~ (TS_BUSY|TS_FLUSH);
			if (tp->t_line)
				(*linesw[tp->t_line].l_start)(tp);
			else
				dcastart(tp);
			break;
		case IIR_RLS:
			dcaeint(unit, dca);
			break;
		default:
			if (code & IIR_NOPEND)
				return (1);
			log(LOG_WARNING, "dca%d: weird interrupt: 0x%x\n",
			    unit, code);
			/* fall through */
		case IIR_MLSC:
			dcamint(unit, dca);
			break;
		}
	}
}

dcaeint(unit, dca)
	register int unit;
	register struct dcadevice *dca;
{
	register struct tty *tp;
	register int stat, c;

	tp = &dca_tty[unit];
	stat = dca->dca_lsr;
	c = dca->dca_data;
	if ((tp->t_state & TS_ISOPEN) == 0) {
#ifdef KGDB
		/* we don't care about parity errors */
		if (((stat & (LSR_BI|LSR_FE|LSR_PE)) == LSR_PE) &&
		    kgdb_dev == makedev(1, unit) && c == '!') {
			printf("kgdb trap from dca%d\n", unit);
			/* trap into kgdb */
			asm("trap #15;");
		}
#endif
		return;
	}
	if (stat & (LSR_BI | LSR_FE))
		c |= TTY_FE;
	else if (stat & LSR_PE)
		c |= TTY_PE;
	else if (stat & LSR_OE)
		log(LOG_WARNING, "dca%d: silo overflow\n", unit);
	(*linesw[tp->t_line].l_rint)(c, tp);
}

dcamint(unit, dca)
	register int unit;
	register struct dcadevice *dca;
{
	register struct tty *tp;
	register int stat;

	tp = &dca_tty[unit];
	stat = dca->dca_msr;
	if ((stat & MSR_DDCD) && (dcasoftCAR & (1 << unit)) == 0) {
		if (stat & MSR_DCD)
			(void)(*linesw[tp->t_line].l_modem)(tp, 1);
		else if ((*linesw[tp->t_line].l_modem)(tp, 0) == 0)
			dca->dca_mcr &= ~(MCR_DTR | MCR_RTS);
	} else if ((stat & MSR_DCTS) && (tp->t_state & TS_ISOPEN) &&
		   (tp->t_flags & CRTSCTS)) {
		/* the line is up and we want to do rts/cts flow control */
		if (stat & MSR_CTS) {
			tp->t_state &=~ TS_TTSTOP;
			ttstart(tp);
		} else
			tp->t_state |= TS_TTSTOP;
	}
}

dcaioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tty *tp;
	register int unit = UNIT(dev);
	register struct dcadevice *dca;
	register int error;
 
	tp = &dca_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	dca = dca_addr[unit];
	switch (cmd) {

	case TIOCSBRK:
		dca->dca_cfcr |= CFCR_SBREAK;
		break;

	case TIOCCBRK:
		dca->dca_cfcr &= ~CFCR_SBREAK;
		break;

	case TIOCSDTR:
		(void) dcamctl(dev, MCR_DTR | MCR_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) dcamctl(dev, MCR_DTR | MCR_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) dcamctl(dev, *(int *)data, DMSET);
		break;

	case TIOCMBIS:
		(void) dcamctl(dev, *(int *)data, DMBIS);
		break;

	case TIOCMBIC:
		(void) dcamctl(dev, *(int *)data, DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dcamctl(dev, 0, DMGET);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dcaparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register struct dcadevice *dca;
	register int cfcr, cflag = t->c_cflag;
	int unit = UNIT(tp->t_dev);
	int ospeed = ttspeedtab(t->c_ospeed, dcaspeedtab);
 
	/* check requested parameters */
        if (ospeed < 0 || (t->c_ispeed && t->c_ispeed != t->c_ospeed))
                return(EINVAL);
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	dca = dca_addr[unit];
	dca->dca_ier = IER_ERXRDY | IER_ETXRDY | IER_ERLS | IER_EMSC;
	if (ospeed == 0) {
		(void) dcamctl(unit, 0, DMSET);	/* hang up line */
		return(0);
	}
	dca->dca_cfcr |= CFCR_DLAB;
	dca->dca_data = ospeed & 0xFF;
	dca->dca_ier = ospeed >> 8;
	switch (cflag&CSIZE) {
	case CS5:
		cfcr = CFCR_5BITS; break;
	case CS6:
		cfcr = CFCR_6BITS; break;
	case CS7:
		cfcr = CFCR_7BITS; break;
	case CS8:
		cfcr = CFCR_8BITS; break;
	}
	if (cflag&PARENB) {
		cfcr |= CFCR_PENAB;
		if ((cflag&PARODD) == 0)
			cfcr |= CFCR_PEVEN;
	}
	if (cflag&CSTOPB)
		cfcr |= CFCR_STOPB;
	dca->dca_cfcr = cfcr;
	return(0);
}
 
dcastart(tp)
	register struct tty *tp;
{
	register struct dcadevice *dca;
	int s, unit, c;
 
	unit = UNIT(tp->t_dev);
	dca = dca_addr[unit];
	s = spltty();
	if (tp->t_state & (TS_TIMEOUT|TS_TTSTOP))
		goto out;
	if (tp->t_outq.c_cc <= tp->t_lowat) {
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
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (dca->dca_lsr & LSR_TXRDY) {
		c = getc(&tp->t_outq);
		tp->t_state |= TS_BUSY;
		dca->dca_data = c;
	}
out:
	splx(s);
}
 
/*
 * Stop output on a line.
 */
/*ARGSUSED*/
dcastop(tp, flag)
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
 
dcamctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct dcadevice *dca;
	register int unit;
	int s;

	unit = UNIT(dev);
	dca = dca_addr[unit];
	s = spltty();
	switch (how) {

	case DMSET:
		dca->dca_mcr = bits;
		break;

	case DMBIS:
		dca->dca_mcr |= bits;
		break;

	case DMBIC:
		dca->dca_mcr &= ~bits;
		break;

	case DMGET:
		bits = dca->dca_msr;
		break;
	}
	(void) splx(s);
	return(bits);
}

/*
 * Following are all routines needed for DCA to act as console
 */
#include "machine/cons.h"

dcacnprobe(cp)
	struct consdev *cp;
{
	int unit, i;
	extern int dcaopen();

	/* XXX: ick */
	unit = CONUNIT;
	dca_addr[CONUNIT] = CONADDR;

	/* make sure hardware exists */
	if (badaddr((short *)dca_addr[unit])) {
		cp->cn_pri = CN_DEAD;
		return;
	}

	/* locate the major number */
	for (i = 0; i < nchrdev; i++)
		if (cdevsw[i].d_open == dcaopen)
			break;

	/* initialize required fields */
	cp->cn_dev = makedev(i, unit);
	cp->cn_tp = &dca_tty[unit];
	switch (dca_addr[unit]->dca_irid) {
	case DCAID0:
	case DCAID1:
		cp->cn_pri = CN_NORMAL;
		break;
	case DCAREMID0:
	case DCAREMID1:
		cp->cn_pri = CN_REMOTE;
		break;
	default:
		cp->cn_pri = CN_DEAD;
		break;
	}
}

dcacninit(cp)
	struct consdev *cp;
{
	int unit = UNIT(cp->cn_dev);

	dcainit(unit);
	dcaconsole = unit;
}

dcainit(unit)
	int unit;
{
	register struct dcadevice *dca;
	int s, rate;
	short stat;

#ifdef lint
	stat = unit; if (stat) return;
#endif
	dca = dca_addr[unit];
	s = splhigh();
	dca->dca_irid = 0xFF;
	DELAY(100);
	dca->dca_ic = IC_IE;
	dca->dca_cfcr = CFCR_DLAB;
	rate = ttspeedtab(dcadefaultrate, dcaspeedtab);
	dca->dca_data = rate & 0xFF;
	dca->dca_ier = rate >> 8;
	dca->dca_cfcr = CFCR_8BITS;
	dca->dca_ier = IER_ERXRDY | IER_ETXRDY;
	stat = dca->dca_iir;
	splx(s);
}

dcacngetc(dev)
{
	register struct dcadevice *dca = dca_addr[UNIT(dev)];
	short stat;
	int c, s;

#ifdef lint
	stat = dev; if (stat) return(0);
#endif
	s = splhigh();
	while (((stat = dca->dca_lsr) & LSR_RXRDY) == 0)
		;
	c = dca->dca_data;
	stat = dca->dca_iir;
	splx(s);
	return(c);
}

/*
 * Console kernel output character routine.
 */
dcacnputc(dev, c)
	dev_t dev;
	register int c;
{
	register struct dcadevice *dca = dca_addr[UNIT(dev)];
	register int timo;
	short stat;
	int s = splhigh();

#ifdef lint
	stat = dev; if (stat) return;
#endif
	if (dcaconsole == -1) {
		(void) dcainit(UNIT(dev));
		dcaconsole = UNIT(dev);
	}
	/* wait for any pending transmission to finish */
	timo = 50000;
	while (((stat = dca->dca_lsr) & LSR_TXRDY) == 0 && --timo)
		;
	dca->dca_data = c;
	/* wait for this transmission to complete */
	timo = 1500000;
	while (((stat = dca->dca_lsr) & LSR_TXRDY) == 0 && --timo)
		;
	/* clear any interrupts generated by this transmission */
	stat = dca->dca_iir;
	splx(s);
}
#endif
