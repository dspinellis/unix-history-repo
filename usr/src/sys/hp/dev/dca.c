/*
 * Copyright (c) 1982, 1986, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)dca.c	7.11 (Berkeley) %G%
 */

#include "dca.h"
#if NDCA > 0
/*
 *  98626/98644/internal serial interface
 *  uses National Semiconductor INS8250/NS16550AF UART
 */
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/ioctl.h"
#include "sys/tty.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/file.h"
#include "sys/uio.h"
#include "sys/kernel.h"
#include "sys/syslog.h"

#include "device.h"
#include "dcareg.h"
#include "machine/cpu.h"
#include "../hp300/isr.h"

int	dcaprobe();
struct	driver dcadriver = {
	dcaprobe, "dca",
};

int	dcastart(), dcaparam(), dcaintr();
int	dcasoftCAR;
int	dca_active;
int	dca_hasfifo;
int	ndca = NDCA;
#ifdef DCACONSOLE
int	dcaconsole = DCACONSOLE;
#else
int	dcaconsole = -1;
#endif
int	dcaconsinit;
int	dcadefaultrate = TTYDEF_SPEED;
int	dcamajor;
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
#include "machine/remote-sl.h"

extern int kgdb_dev;
extern int kgdb_rate;
extern int kgdb_debug_init;
#endif

#define	UNIT(x)		minor(x)

#ifdef DEBUG
long	fifoin[17];
long	fifoout[17];
long	dcaintrcount[16];
long	dcamintcount[16];
#endif

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

	/* look for a NS 16550AF UART with FIFOs */
	dca->dca_fifo = FIFO_ENABLE|FIFO_RCV_RST|FIFO_XMT_RST|FIFO_TRIGGER_14;
	DELAY(100);
	if ((dca->dca_iir & IIR_FIFO_MASK) == IIR_FIFO_MASK)
		dca_hasfifo |= 1 << unit;

	hd->hp_ipl = DCAIPL(dca->dca_ic);
	dcaisr[unit].isr_ipl = hd->hp_ipl;
	dcaisr[unit].isr_arg = unit;
	dcaisr[unit].isr_intr = dcaintr;
	dca_addr[unit] = dca;
	dca_active |= 1 << unit;
	dcasoftCAR = hd->hp_flags;
	isrlink(&dcaisr[unit]);
#ifdef KGDB
	if (kgdb_dev == makedev(dcamajor, unit)) {
		if (dcaconsole == unit)
			kgdb_dev = -1;	/* can't debug over console port */
		else {
			(void) dcainit(unit, kgdb_rate);
			if (kgdb_debug_init) {
				/*
				 * Print prefix of device name,
				 * let kgdb_connect print the rest.
				 */
				printf("dca%d: ", unit);
				kgdb_connect(1);
			} else
				printf("dca%d: kgdb enabled\n", unit);
		}
	}
#endif
	dca->dca_ic = IC_IE;
	/*
	 * Need to reset baud rate, etc. of next print so reset dcaconsinit.
	 * Also make sure console is always "hardwired."
	 */
	if (unit == dcaconsole) {
		dcaconsinit = 0;
		dcasoftCAR |= (1 << unit);
	}
	return (1);
}

/* ARGSUSED */
#ifdef __STDC__
dcaopen(dev_t dev, int flag, int mode, struct proc *p)
#else
dcaopen(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
#endif
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
		if (tp->t_ispeed == 0) {
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = dcadefaultrate;
		}
		dcaparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && p->p_ucred->cr_uid != 0)
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
dcaclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	register struct tty *tp;
	register struct dcadevice *dca;
	register int unit;
 
	unit = UNIT(dev);
	dca = dca_addr[unit];
	tp = &dca_tty[unit];
	(*linesw[tp->t_line].l_close)(tp, flag);
	dca->dca_cfcr &= ~CFCR_SBREAK;
#ifdef KGDB
	/* do not disable interrupts if debugging */
	if (dev != kgdb_dev)
#endif
	dca->dca_ier = 0;
	(void) dcamctl(dev, 0, DMSET);
	if (tp->t_state & TS_HUPCLS)
		(*linesw[tp->t_line].l_modem)(tp, 0);
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
#ifdef DEBUG
		dcaintrcount[code & IIR_IMASK]++;
#endif
		switch (code & IIR_IMASK) {
		case IIR_NOPEND:
			return (1);
		case IIR_RXTOUT:
		case IIR_RXRDY:
			/* do time-critical read in-line */
			tp = &dca_tty[unit];
/*
 * Process a received byte.  Inline for speed...
 */
#ifdef KGDB
#define	RCVBYTE() \
			code = dca->dca_data; \
			if ((tp->t_state & TS_ISOPEN) == 0) { \
				if (kgdb_dev == makedev(dcamajor, unit) && \
				    code == FRAME_END) \
					kgdb_connect(0); /* trap into kgdb */ \
			} else \
				(*linesw[tp->t_line].l_rint)(code, tp)
#else
#define	RCVBYTE() \
			code = dca->dca_data; \
			if ((tp->t_state & TS_ISOPEN) != 0) \
				(*linesw[tp->t_line].l_rint)(code, tp)
#endif
			RCVBYTE();
			if (dca_hasfifo & (1 << unit)) {
#ifdef DEBUG
				register int fifocnt = 1;
#endif
				while ((code = dca->dca_lsr) & LSR_RCV_MASK) {
					if (code == LSR_RXRDY) {
						RCVBYTE();
					} else
						dcaeint(unit, code, dca);
#ifdef DEBUG
					fifocnt++;
#endif
				}
#ifdef DEBUG
				if (fifocnt > 16)
					fifoin[0]++;
				else
					fifoin[fifocnt]++;
#endif
			}
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
			dcaeint(unit, dca->dca_lsr, dca);
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

dcaeint(unit, stat, dca)
	register int unit, stat;
	register struct dcadevice *dca;
{
	register struct tty *tp;
	register int c;

	tp = &dca_tty[unit];
	c = dca->dca_data;
	if ((tp->t_state & TS_ISOPEN) == 0) {
#ifdef KGDB
		/* we don't care about parity errors */
		if (((stat & (LSR_BI|LSR_FE|LSR_PE)) == LSR_PE) &&
		    kgdb_dev == makedev(dcamajor, unit) && c == FRAME_END)
			kgdb_connect(0); /* trap into kgdb */
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
#ifdef DEBUG
	dcamintcount[stat & 0xf]++;
#endif
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
	if (dca_hasfifo & (1 << unit))
		dca->dca_fifo = FIFO_ENABLE | FIFO_TRIGGER_14;
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
		if (dca_hasfifo & (1 << unit)) {
			for (c = 1; c < 16 && tp->t_outq.c_cc; ++c)
				dca->dca_data = getc(&tp->t_outq);
#ifdef DEBUG
			if (c > 16)
				fifoout[0]++;
			else
				fifoout[c]++;
#endif
		}
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
#include "../hp300/cons.h"

dcacnprobe(cp)
	struct consdev *cp;
{
	int unit;

	/* locate the major number */
	for (dcamajor = 0; dcamajor < nchrdev; dcamajor++)
		if (cdevsw[dcamajor].d_open == dcaopen)
			break;

	/* XXX: ick */
	unit = CONUNIT;
	dca_addr[CONUNIT] = (struct dcadevice *) sctova(CONSCODE);

	/* make sure hardware exists */
	if (badaddr((short *)dca_addr[unit])) {
		cp->cn_pri = CN_DEAD;
		return;
	}

	/* initialize required fields */
	cp->cn_dev = makedev(dcamajor, unit);
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
	/*
	 * If dcaconsole is initialized, raise our priority.
	 */
	if (dcaconsole == unit)
		cp->cn_pri = CN_REMOTE;
#ifdef KGDB
	if (major(kgdb_dev) == 1)			/* XXX */
		kgdb_dev = makedev(dcamajor, minor(kgdb_dev));
#endif
}

dcacninit(cp)
	struct consdev *cp;
{
	int unit = UNIT(cp->cn_dev);

	dcainit(unit, dcadefaultrate);
	dcaconsole = unit;
	dcaconsinit = 1;
}

dcainit(unit, rate)
	int unit, rate;
{
	register struct dcadevice *dca;
	int s;
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
	rate = ttspeedtab(rate, dcaspeedtab);
	dca->dca_data = rate & 0xFF;
	dca->dca_ier = rate >> 8;
	dca->dca_cfcr = CFCR_8BITS;
	dca->dca_ier = IER_ERXRDY | IER_ETXRDY;
	dca->dca_fifo = FIFO_ENABLE|FIFO_RCV_RST|FIFO_XMT_RST|FIFO_TRIGGER_14;
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
#ifdef KGDB
	if (dev != kgdb_dev)
#endif
	if (dcaconsinit == 0) {
		(void) dcainit(UNIT(dev), dcadefaultrate);
		dcaconsinit = 1;
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
