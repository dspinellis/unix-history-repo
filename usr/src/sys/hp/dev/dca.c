/*
 * Copyright (c) 1982, 1986, 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)dca.c	8.2 (Berkeley) 1/12/94
 */

#include "dca.h"
#if NDCA > 0
/*
 *  Driver for National Semiconductor INS8250/NS16550AF/WD16C552 UARTs.
 *  Includes:
 *	98626/98644/internal serial interface on hp300/hp400
 *	internal serial ports on hp700
 *
 *  N.B. On the hp700, there is a "secret bit" with undocumented behavior.
 *  The third bit of the Modem Control Register (MCR_IEN == 0x08) must be
 *  set to enable interrupts.
 */
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

#include <hp/dev/device.h>
#include <hp/dev/dcareg.h>

#include <machine/cpu.h>
#ifdef hp300
#include <hp300/hp300/isr.h>
#endif
#ifdef hp700
#include <machine/asp.h>
#endif

int	dcaprobe();
struct	driver dcadriver = {
	dcaprobe, "dca",
};

void	dcastart();
int	dcaparam(), dcaintr();
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
#ifdef hp300
struct	isr dcaisr[NDCA];
int	dcafastservice;
#endif
int	dcaoflows[NDCA];

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

#ifdef KGDB
#include <machine/remote-sl.h>

extern dev_t kgdb_dev;
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
#ifdef hp300
	if (dca->dca_id != DCAID0 &&
	    dca->dca_id != DCAREMID0 &&
	    dca->dca_id != DCAID1 &&
	    dca->dca_id != DCAREMID1)
		return (0);
#endif
	unit = hd->hp_unit;
	if (unit == dcaconsole)
		DELAY(100000);
#ifdef hp300
	dca->dca_reset = 0xFF;
	DELAY(100);
#endif

	/* look for a NS 16550AF UART with FIFOs */
	dca->dca_fifo = FIFO_ENABLE|FIFO_RCV_RST|FIFO_XMT_RST|FIFO_TRIGGER_14;
	DELAY(100);
	if ((dca->dca_iir & IIR_FIFO_MASK) == IIR_FIFO_MASK)
		dca_hasfifo |= 1 << unit;

	dca_addr[unit] = dca;
#ifdef hp300
	hd->hp_ipl = DCAIPL(dca->dca_ic);
	dcaisr[unit].isr_ipl = hd->hp_ipl;
	dcaisr[unit].isr_arg = unit;
	dcaisr[unit].isr_intr = dcaintr;
	isrlink(&dcaisr[unit]);
#endif
	dca_active |= 1 << unit;
	if (hd->hp_flags)
		dcasoftCAR |= (1 << unit);
#ifdef KGDB
	if (kgdb_dev == makedev(dcamajor, unit)) {
		if (dcaconsole == unit)
			kgdb_dev = NODEV; /* can't debug over console port */
		else {
			(void) dcainit(unit, kgdb_rate);
			dcaconsinit = 1;	/* don't re-init in dcaputc */
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
#ifdef hp300
	dca->dca_ic = IC_IE;
#endif
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
#ifdef hp300
	/*
	 * XXX hack to speed up unbuffered builtin port.
	 * If dca_fastservice is set, a level 5 interrupt
	 * will be directed to dcaintr first.
	 */
	if (error == 0 && unit == 0 && (dca_hasfifo & 1) == 0)
		dcafastservice = 1;
#endif
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
#ifdef hp300
	if (unit == 0)
		dcafastservice = 0;
#endif
	dca = dca_addr[unit];
	tp = &dca_tty[unit];
	(*linesw[tp->t_line].l_close)(tp, flag);
	dca->dca_cfcr &= ~CFCR_SBREAK;
#ifdef KGDB
	/* do not disable interrupts if debugging */
	if (dev != kgdb_dev)
#endif
	dca->dca_ier = 0;
	if (tp->t_cflag&HUPCL || tp->t_state&TS_WOPEN ||
	    (tp->t_state&TS_ISOPEN) == 0)
		(void) dcamctl(dev, 0, DMSET);
	ttyclose(tp);
	return (0);
}
 
dcaread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	int unit = UNIT(dev);
	register struct tty *tp = &dca_tty[unit];
	int error, of;
 
	of = dcaoflows[unit];
	error = (*linesw[tp->t_line].l_read)(tp, uio, flag);
	/*
	 * XXX hardly a reasonable thing to do, but reporting overflows
	 * at interrupt time just exacerbates the problem.
	 */
	if (dcaoflows[unit] != of)
		log(LOG_WARNING, "dca%d: silo overflow\n", unit);
	return (error);
}
 
dcawrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	int unit = UNIT(dev);
	register struct tty *tp = &dca_tty[unit];
	extern struct tty *constty;
 
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
	int iflowdone = 0;

	dca = dca_addr[unit];
#ifdef hp300
	if ((dca->dca_ic & (IC_IR|IC_IE)) != (IC_IR|IC_IE))
		return (0);
#endif
	tp = &dca_tty[unit];
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
/*
 * Process a received byte.  Inline for speed...
 */
#ifdef KGDB
#define	RCVBYTE() \
			code = dca->dca_data; \
			if ((tp->t_state & TS_ISOPEN) == 0) { \
				if (code == FRAME_END && \
				    kgdb_dev == makedev(dcamajor, unit)) \
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
			if (!iflowdone && (tp->t_cflag&CRTS_IFLOW) &&
			    tp->t_rawq.c_cc > TTYHOG/2) {
				dca->dca_mcr &= ~MCR_RTS;
				iflowdone = 1;
			}
			break;
		case IIR_TXRDY:
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
		dcaoflows[unit]++;
	(*linesw[tp->t_line].l_rint)(c, tp);
}

dcamint(unit, dca)
	register int unit;
	register struct dcadevice *dca;
{
	register struct tty *tp;
	register u_char stat;

	tp = &dca_tty[unit];
	stat = dca->dca_msr;
#ifdef DEBUG
	dcamintcount[stat & 0xf]++;
#endif
	if ((stat & MSR_DDCD) &&
	    (dcasoftCAR & (1 << unit)) == 0) {
		if (stat & MSR_DCD)
			(void)(*linesw[tp->t_line].l_modem)(tp, 1);
		else if ((*linesw[tp->t_line].l_modem)(tp, 0) == 0)
			dca->dca_mcr &= ~(MCR_DTR | MCR_RTS);
	}
	/*
	 * CTS change.
	 * If doing HW output flow control start/stop output as appropriate.
	 */
	if ((stat & MSR_DCTS) &&
	    (tp->t_state & TS_ISOPEN) && (tp->t_cflag & CCTS_OFLOW)) {
		if (stat & MSR_CTS) {
			tp->t_state &=~ TS_TTSTOP;
			dcastart(tp);
		} else {
			tp->t_state |= TS_TTSTOP;
		}
	}
}

dcaioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct tty *tp;
	register int unit = UNIT(dev);
	register struct dcadevice *dca;
	register int error;
 
	tp = &dca_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
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
                return (EINVAL);
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	dca = dca_addr[unit];
	dca->dca_ier = IER_ERXRDY | IER_ETXRDY | IER_ERLS | IER_EMSC;
#ifdef hp700
	dca->dca_mcr |= MCR_IEN;
#endif
	if (ospeed == 0) {
		(void) dcamctl(unit, 0, DMSET);	/* hang up line */
		return (0);
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
	return (0);
}
 
void
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
		selwakeup(&tp->t_wsel);
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
	int flag;
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
#ifdef hp700
	/*
	 * Always make sure MCR_IEN is set (unless setting to 0)
	 */
#ifdef KGDB
	if (how == DMSET && kgdb_dev == makedev(dcamajor, unit))
		bits |= MCR_IEN;
	else
#endif
	if (how == DMBIS || (how == DMSET && bits))
		bits |= MCR_IEN;
	else if (how == DMBIC)
		bits &= ~MCR_IEN;
#endif
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
	return (bits);
}

/*
 * Following are all routines needed for DCA to act as console
 */
#include <hp/dev/cons.h>

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
#ifdef hp300
	dca_addr[CONUNIT] = (struct dcadevice *) sctova(CONSCODE);

	/* make sure hardware exists */
	if (badaddr((short *)dca_addr[unit])) {
		cp->cn_pri = CN_DEAD;
		return;
	}
#endif
#ifdef hp700
	dca_addr[CONUNIT] = CONPORT;
#endif

	/* initialize required fields */
	cp->cn_dev = makedev(dcamajor, unit);
	cp->cn_tp = &dca_tty[unit];
#ifdef hp300
	switch (dca_addr[unit]->dca_id) {
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
#endif
#ifdef hp700
	cp->cn_pri = CN_NORMAL;
#endif
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
#ifdef hp300
	dca->dca_reset = 0xFF;
	DELAY(100);
	dca->dca_ic = IC_IE;
#endif
	dca->dca_cfcr = CFCR_DLAB;
	rate = ttspeedtab(rate, dcaspeedtab);
	dca->dca_data = rate & 0xFF;
	dca->dca_ier = rate >> 8;
	dca->dca_cfcr = CFCR_8BITS;
	dca->dca_ier = IER_ERXRDY | IER_ETXRDY;
#ifdef hp700
	dca->dca_mcr |= MCR_IEN;
#endif
	dca->dca_fifo = FIFO_ENABLE|FIFO_RCV_RST|FIFO_XMT_RST|FIFO_TRIGGER_14;
	DELAY(100);
	stat = dca->dca_iir;
	splx(s);
}

dcacngetc(dev)
	dev_t dev;
{
	register struct dcadevice *dca = dca_addr[UNIT(dev)];
	register u_char stat;
	int c, s;

#ifdef lint
	stat = dev; if (stat) return (0);
#endif
	s = splhigh();
	while (((stat = dca->dca_lsr) & LSR_RXRDY) == 0)
		;
	c = dca->dca_data;
	stat = dca->dca_iir;
	splx(s);
	return (c);
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
	register u_char stat;
	int s = splhigh();

#ifdef lint
	stat = dev; if (stat) return;
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
	/*
	 * If the "normal" interface was busy transfering a character
	 * we must let our interrupt through to keep things moving.
	 * Otherwise, we clear the interrupt that we have caused.
	 */
	if ((dca_tty[UNIT(dev)].t_state & TS_BUSY) == 0)
		stat = dca->dca_iir;
	splx(s);
}
#endif
