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
 *	@(#)sio.c	7.2 (Berkeley) %G%
 */

/*
 * sio.c -- NEC uPD7201A UART Device Driver
 * by A.Fujita, NOV-25-1991
 */

#include "sio.h"
#if NSIO > 0

#undef LOCAL_CONSOLE

/*
 *  OMRON LUNA internal serial interface
 *  uese NEC uPD7201A SIO
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

#include <luna68k/dev/device.h>
#include <luna68k/dev/sioreg.h>

#define SIO_IPL		6

int	sioprobe();
struct	driver siodriver = {
	sioprobe, "sio",
};

void	siostart();
int	sioparam(), siointr();
int	sio_active;
int	sioconsole = -1;
int	sioconsinit;
int	siodefaultrate = TTYDEF_SPEED;
int	siomajor;
struct	siodevice *sio_addr[2];
struct	tty sio_tty[NSIO];

#define	siounit(x)		minor(x)


#ifndef	LOCAL_CONSOLE

/*
 * local buffering
 */

#define LOCAL_BUFSIZ	128

struct local_buf {
	u_char	*push;
	u_char	*pop;
	u_char	buf[LOCAL_BUFSIZ+4];
};

struct local_buf rbuf, *rbp = &rbuf;


siolbufinit(bp)
	register struct local_buf *bp;
{
	bp->push = bp->pop = &(bp->buf[LOCAL_BUFSIZ]);
}

siolbufpush(bp, c)
	register struct local_buf *bp;
	register int c;
{

	*(--bp->push) = c;

	if (bp->push == &(bp->buf[0]))
		bp->push = &(bp->buf[LOCAL_BUFSIZ]);

	if (bp->push == bp->pop)
		bp->pop == (u_char *) 0;
}

int
siolbufpop(bp)
	register struct local_buf *bp;
{
	register int c;

	if (bp->pop == (u_char *) 0)
		bp->pop = bp->push;

	c = *(--bp->pop);

	if (bp->pop == &(bp->buf[0]))
		bp->pop = &(bp->buf[LOCAL_BUFSIZ]);

	return(c);
}

int
siolbufempty(bp)
	register struct local_buf *bp;
{
	if (bp->push == bp->pop)
		return(1);
	else
		return(0);
}
#endif

/*
 * probing
 */

sioprobe(hd)
	register struct hp_device *hd;
{
	register struct siodevice *sio;
	register int unit;

	sio = (struct siodevice *)hd->hp_addr;
	unit = hd->hp_unit;

	hd->hp_ipl = SIO_IPL;

	/*
	 *  We must set hardware address here.
	 *  but now already it's done.

	sio_addr[unit] = sio;

	*/

	sio_active |= 1 << unit;

	/*
	 *  We must pick up information from hd->hp_flags here.
	 *  It should be used instead of TTYDEF_CFLAG or like something.
	 *  
	 */

#ifdef LOCAL_CONSOLE

	/*
	 *  Enable Interrupt
	 *    I must rewirte basic handlers of console support,
	 *    Because it does not work, if interrupt occar.
	 *    Now using LOCAL_CONSOLE, so the problem isn't happend.
	 *
	 */

	sioreg(REG(unit, WR1), WR1_RXALLS | WR1_TXENBL);

#endif
	if (unit == sioconsole) {
		sioconsinit = 0;
	}

	return (1);
}

int
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
	if (unit >= NSIO || (sio_active & (1 << unit)) == 0)
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
			tp->t_cflag = (CREAD | CS8 | HUPCL);
			tp->t_lflag = TTYDEF_LFLAG;
			tp->t_ispeed = tp->t_ospeed = siodefaultrate;
		}
		sioparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && p->p_ucred->cr_uid != 0)
		return (EBUSY);

	/*
	 *  We must set DTR & RTS here.
	 *  Need a routine like XXXmctl().
	 *
	 */

	/*
	 *  The next statment should be executed, when Carrier Detected
	 *  or using special serial line which ignore carrier.
	 *
	 *  Should be checked out RR0, I think. Omit this time.
	 */

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
	register struct siodevice *sio;
	register int unit;

	unit = siounit(dev);
	sio = sio_addr[unit];
	tp = &sio_tty[unit];

	(*linesw[tp->t_line].l_close)(tp, flag);

	/*
	 *  We must send BREAK to current line here.
	 *  Not supported yet.
	 */

	/*
	 *  We must reset DTR & RTS here.
	 *  Need a routine like XXXmctl().
	 *
	 */

	ttyclose(tp);
	return (0);
}
 
sioread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct tty *tp = &sio_tty[siounit(dev)];
 
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}
 
siowrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
	int flag;
{
	register struct tty *tp = &sio_tty[siounit(dev)];

	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

siointr()
{
	register int unit = 0;
	register struct siodevice *sio = sio_addr[unit];
	register u_char code;
	register struct tty *tp;
	int s, rr;

	rr = siogetreg(unit);
	tp = &sio_tty[unit];
	if (rr & RR_RXRDY) {
		code = sio->sio_data;
		if ((tp->t_state & TS_ISOPEN) != 0)
			(*linesw[tp->t_line].l_rint)(code, tp);
#ifndef LOCAL_CONSOLE
		else
			siolbufpush(rbp, code);
#endif
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

sioioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct tty *tp;
	register int unit = siounit(dev);
	register struct siodevice *sio;
	register int error;
 
	tp = &sio_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag, p);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	/*
	 *  We must support flow control of serial lines.
	 *  Not yet.
	 */

	sio = sio_addr[unit];
	switch (cmd) {

	case TIOCSBRK:
	case TIOCCBRK:
	case TIOCSDTR:
	case TIOCCDTR:
	case TIOCMSET:
	case TIOCMBIS:
	case TIOCMBIC:
	case TIOCMGET:
	default:
		return (ENOTTY);
	}

	return (0);
}

sioparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	return (0);
}

void
siostart(tp)
	register struct tty *tp;
{
	register struct siodevice *sio;
	int s, unit, c, rr;
 
	unit = siounit(tp->t_dev);
	sio = sio_addr[unit];

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

	rr = siogetreg(unit);
	if (rr & RR_TXRDY) {
		c = getc(&tp->t_outq);
		tp->t_state |= TS_BUSY;
		sio->sio_data = c;
	}

out:
	splx(s);
}
 
/*
 * Stop output on a line.
 */
/*ARGSUSED*/
siostop(tp, flag)
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

/*
 * Following are all routines needed for SIO to act as console
 */
#include <luna68k/luna68k/cons.h>

siocnprobe(cp)
	struct consdev *cp;
{
	int unit;

	/* Line Selection: 0: Channel-A (ttya),  1: Channel-B (keyboard) */
	unit = 0;
    
	/* locate the major number */
	for (siomajor = 0; siomajor < nchrdev; siomajor++)
		if (cdevsw[siomajor].d_open == sioopen)
			break;

	sio_addr[0] = (struct siodevice *) 0x51000000;
	sio_addr[1] = (struct siodevice *) 0x51000004;

	/* make sure hardware exists */
	if (badaddr((short *)sio_addr[0])) {
		cp->cn_pri = CN_DEAD;
		return;
	}

	/* locate the major number */

	/* initialize required fields */
	cp->cn_dev = makedev(siomajor, unit);
	cp->cn_tp  = 0;
	cp->cn_pri = CN_NORMAL;
}

siocninit(cp)
	struct consdev *cp;
{
	int unit = siounit(cp->cn_dev);

	sioinit(unit);
	sioconsole = unit;
}

/*
 * Routines for Console Support
 */


#ifdef LOCAL_CONSOLE

int local_console;

siocngetc(dev)
	dev_t dev;
{
	int c, rr0, rr1;
	int unit;
	int s;

	unit = local_console;

	s = splhigh();
loop:
	while (((rr0 = sioreg(REG(unit, RR0), 0)) & RR0_RXAVAIL) == 0);
	rr1 = sioreg(REG(unit, RR1), 0);
	c = sio_addr[unit]->sio_data;

	if ((rr0 & RR0_BREAK) == RR0_BREAK)		/* Break Detected */
		goto loop;

	if ((rr1 & RR1_OVERRUN) == RR1_OVERRUN) {	/* Data Over Run */
		sioreg(REG(unit, WR0), WR0_ERRRST);
		goto loop;
	}

	if ((rr1 & RR1_PARITY) == RR1_PARITY) {		/* Parity Error */
		sioreg(REG(unit, WR0), WR0_ERRRST);
		goto loop;
	}

	if ((rr1 & RR1_FRAMING) == RR1_FRAMING) {	/* Framing Error */
		sioreg(REG(unit, WR0), WR0_ERRRST);
		goto loop;
	}

	sioreg(REG(unit, WR0), WR0_RSTPEND);
	splx(s);
	return(c);
}

siocnputc(dev, c)
	dev_t dev;
	int c;
{
	int unit;
	int s;

	unit = local_console;

	if (sioconsole == -1) {
		(void) sioinit(unit);
		sioconsole = unit;
	}

	s = splhigh();

	/* wait for any pending transmission to finish */
	while ((sioreg(REG(unit, RR0), 0) & RR0_TXEMPTY) == 0);

	sio_addr[unit]->sio_data = (c & 0xFF);

	/* wait for any pending transmission to finish */
	while ((sioreg(REG(unit, RR0), 0) & RR0_TXEMPTY) == 0);

	splx(s);
}

sioinit(unit)
	int unit;
{
	int s;

	s = splhigh();

	sioreg(REG(unit, WR0), WR0_CHANRST);		/* Channel-A Reset */
	sioreg(WR2A, WR2_VEC86  | WR2_INTR_1);		/* Set CPU BUS Interface Mode */
	sioreg(WR2B, 0);				/* Set Interrupt Vector */
	sioreg(REG(unit, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(unit, WR4), WR4_BAUD96 | WR4_STOP1 | WR4_NPARITY);	/* Tx/Rx */
	sioreg(REG(unit, WR3), WR3_RX8BIT | WR3_RXENBL);		/* Rx */
	sioreg(REG(unit, WR5), WR5_TX8BIT | WR5_TXENBL);		/* Tx */
	sioreg(REG(unit, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */

	unit = local_console = 1 - unit;

	sioreg(REG(unit, WR0), WR0_CHANRST);		/* Channel-B Reset */

	sioreg(REG(unit, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(unit, WR4), WR4_BAUD96 | WR4_STOP1 | WR4_NPARITY);	/* Tx/Rx */
	sioreg(REG(unit, WR3), WR3_RX8BIT | WR3_RXENBL);		/* Rx */
	sioreg(REG(unit, WR5), WR5_TX8BIT | WR5_TXENBL);		/* Tx */
	sioreg(REG(unit, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */

	splx(s);
}
#else
	
/*
 * console put & get
 */

siocngetc(dev)
	dev_t dev;
{
	while (siolbufempty(rbp))
		DELAY(10);

	return(siolbufpop(rbp));
}

siocnputc(dev, c)
	dev_t dev;
	int c;
{
	register int unit = siounit(dev);
	register struct siodevice *sio = sio_addr[unit];
	register u_char code;
	int s, rr;

	s = splhigh();
	sioreg(REG(unit, WR1), WR1_RXALLS);

	do {
		DELAY(1);
		rr = siogetreg(unit);
	} while (!(rr & RR_TXRDY));
		
	code = (c & 0xff);
	sio->sio_data = code;

	do {
		DELAY(1);
		rr = siogetreg(unit);
	} while (!(rr & RR_TXRDY));

	sioreg(REG(unit, WR1), WR1_RXALLS | WR1_TXENBL);
	splx(s);
}

sioinit(unit)
	int unit;
{
	int s;

	siolbufinit(rbp);

	s = splhigh();

	unit = 0;

	sioreg(REG(unit, WR0), WR0_CHANRST);		/* Channel-A Reset */
	sioreg(WR2A, WR2_VEC86  | WR2_INTR_1);		/* Set CPU BUS Interface Mode */
	sioreg(WR2B, 0);				/* Set Interrupt Vector */
	sioreg(REG(unit, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(unit, WR4), WR4_BAUD96 | WR4_STOP1 | WR4_NPARITY);	/* Tx/Rx */
	sioreg(REG(unit, WR3), WR3_RX8BIT | WR3_RXENBL);		/* Rx */
	sioreg(REG(unit, WR5), WR5_TX8BIT | WR5_TXENBL);		/* Tx */
	sioreg(REG(unit, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(unit, WR1), WR1_RXALLS | WR1_TXENBL);

	unit = 1;

	sioreg(REG(unit, WR0), WR0_CHANRST);		/* Channel-A Reset */

	sioreg(REG(unit, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(unit, WR4), WR4_BAUD96 | WR4_STOP1 | WR4_NPARITY);	/* Tx/Rx */
	sioreg(REG(unit, WR3), WR3_RX8BIT | WR3_RXENBL);		/* Rx */
	sioreg(REG(unit, WR5), WR5_TX8BIT | WR5_TXENBL);		/* Tx */
	sioreg(REG(unit, WR0), WR0_RSTINT);		/* Reset E/S Interrupt */
	sioreg(REG(unit, WR1), WR1_RXALLS | WR1_TXENBL);

	splx(s);
}
#endif

int
siogetreg(unit)
	register int unit;
{
	register struct siodevice *sio = sio_addr[unit];
	register int rr = 0;

	rr = sio->sio_stat;
	rr <<= 8;
	sio->sio_cmd = 1;	/* Select RR1 */
	rr |= sio->sio_stat;

	return(rr);
}

int
sioreg(reg, val)
	register int reg, val;
{
	register int chan;

	chan = CHANNEL(reg);

	if (isStatusReg(reg)) {
		if (REGNO(reg) != 0)
		    sio_addr[chan]->sio_cmd = REGNO(reg);
		return(sio_addr[chan]->sio_stat);
	} else {
		if (REGNO(reg) != 0)
		    sio_addr[chan]->sio_cmd = REGNO(reg);
		sio_addr[chan]->sio_cmd = val;
		return(val);
	}
}
#endif
