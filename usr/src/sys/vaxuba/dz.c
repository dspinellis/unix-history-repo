/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dz.c	7.9 (Berkeley) 6/28/90
 */

#include "dz.h"
#if NDZ > 0
/*
 *  DZ-11/DZ-32 Driver
 *
 * This driver mimics dh.c; see it for explanation of common code.
 */
#include "param.h"
#include "systm.h"
#include "ioctl.h"
#include "tty.h"
#include "user.h"
#include "proc.h"
#include "map.h"
#include "buf.h"
#include "vm.h"
#include "conf.h"
#include "bkmac.h"
#include "file.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "pdma.h"
#include "ubavar.h"
#include "dzreg.h"
#include "machine/pte.h"

/*
 * Driver information for auto-configuration stuff.
 */
int	dzprobe(), dzattach(), dzrint();
struct	uba_device *dzinfo[NDZ];
u_short	dzstd[] = { 0 };
struct	uba_driver dzdriver =
	{ dzprobe, 0, dzattach, 0, dzstd, "dz", dzinfo };

#define	NDZLINE 	(NDZ*8)
#define	FASTTIMER	(hz/30)		/* rate to drain silos, when in use */

int	dzstart(), dzxint(), dzdma();
int	ttrstrt();
struct	tty dz_tty[NDZLINE];
int	dz_cnt = { NDZLINE };
int	dzact;
int	dzsilos;			/* mask of dz's with silo in use */
int	dzchars[NDZ];			/* recent input count */
int	dzrate[NDZ];			/* smoothed input count */
int	dztimerintvl;			/* time interval for dztimer */
int	dzhighrate = 100;		/* silo on if dzchars > dzhighrate */
int	dzlowrate = 75;			/* silo off if dzrate < dzlowrate */

#define dzwait(x)	while (((x)->dzlcs & DZ_ACK) == 0)

/*
 * Software copy of dzbrk since it isn't readable
 */
char	dz_brk[NDZ];
char	dzsoftCAR[NDZ];
char	dz_lnen[NDZ];	/* saved line enable bits for DZ32 */

/*
 * The dz11 doesn't interrupt on carrier transitions, so
 * we have to use a timer to watch it.
 */
char	dz_timer;		/* timer started? */

/*
 * Pdma structures for fast output code
 */
struct	pdma dzpdma[NDZLINE];

struct speedtab dzspeedtab[] = {
	0,	0,
	50,	020,
	75,	021,
	110,	022,
	134,	023,
	150,	024,
	300,	025,
	600,	026,
	1200,	027,
	1800,	030,
	2400,	032,
	4800,	034,
	9600,	036,
	19200,	037,
	EXTA,	037,
	-1,	-1
};
 
#ifndef	PORTSELECTOR
#define	ISPEED	TTYDEF_SPEED
#define	LFLAG	TTYDEF_LFLAG
#else
#define	ISPEED	B4800
#define	LFLAG	(TTYDEF_LFLAG&~ECHO)
#endif

dzprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct dzdevice *dzaddr = (struct dzdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	dzrint(0); dzxint((struct tty *)0);
#endif
	dzaddr->dzcsr = DZ_TIE|DZ_MSE|DZ_32;
	if (dzaddr->dzcsr & DZ_32)
		dzaddr->dzlnen = 1;
	else
		dzaddr->dztcr = 1;		/* enable any line */
	DELAY(100000);
	dzaddr->dzcsr = DZ_CLR|DZ_32;		/* reset everything */
	if (cvec && cvec != 0x200)
		cvec -= 4;
	return (sizeof (struct dzdevice));
}

dzattach(ui)
	register struct uba_device *ui;
{
	register struct pdma *pdp = &dzpdma[ui->ui_unit*8];
	register struct tty *tp = &dz_tty[ui->ui_unit*8];
	register int cntr;
	extern dzscan();

	for (cntr = 0; cntr < 8; cntr++) {
		pdp->p_addr = (struct dzdevice *)ui->ui_addr;
		pdp->p_arg = (int)tp;
		pdp->p_fcn = dzxint;
		pdp++, tp++;
	}
	dzsoftCAR[ui->ui_unit] = ui->ui_flags;
	if (dz_timer == 0) {
		dz_timer++;
		timeout(dzscan, (caddr_t)0, hz);
		dztimerintvl = FASTTIMER;
	}
}

/*ARGSUSED*/
dzopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;
	int error, dzparam();
 
	unit = minor(dev);
	if (unit >= dz_cnt || dzpdma[unit].p_addr == 0)
		return (ENXIO);
	tp = &dz_tty[unit];
	tp->t_addr = (caddr_t)&dzpdma[unit];
	tp->t_oproc = dzstart;
	tp->t_param = dzparam;
	tp->t_dev = dev;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		tp->t_state |= TS_WOPEN;
		ttychars(tp);
#ifndef PORTSELECTOR
		if (tp->t_ispeed == 0) {
#endif
			tp->t_iflag = TTYDEF_IFLAG;
			tp->t_oflag = TTYDEF_OFLAG;
			tp->t_cflag = TTYDEF_CFLAG;
			tp->t_lflag = LFLAG;
			tp->t_ispeed = tp->t_ospeed = ISPEED;
#ifdef PORTSELECTOR
			tp->t_cflag |= HUPCL;
#else 
		}
#endif
		dzparam(tp, &tp->t_termios);
		ttsetwater(tp);
	} else if (tp->t_state&TS_XCLUDE && u.u_uid != 0)
		return (EBUSY);
	(void) dzmctl(dev, DZ_ON, DMSET);
	(void) spl5();
	while ((flag&O_NONBLOCK) == 0 && (tp->t_cflag&CLOCAL) == 0 &&
	       (tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	(void) spl0();
	if (error)
		return (error);
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}
 
/*ARGSUSED*/
dzclose(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;
	register struct dzdevice *dzaddr;
	int dz;
 
	unit = minor(dev);
	dz = unit >> 3;
	tp = &dz_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	dzaddr = dzpdma[unit].p_addr;
	if (dzaddr->dzcsr&DZ_32)
		(void) dzmctl(dev, DZ_BRK, DMBIC);
	else
		dzaddr->dzbrk = (dz_brk[dz] &= ~(1 << (unit&07)));
	if (tp->t_cflag&HUPCL || tp->t_state&TS_WOPEN || 
	    (tp->t_state&TS_ISOPEN) == 0)
		(void) dzmctl(dev, DZ_OFF, DMSET);
	return (ttyclose(tp));
}
 
dzread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;
 
	tp = &dz_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}
 
dzwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;
 
	tp = &dz_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}
 
/*ARGSUSED*/
dzrint(dz)
	int dz;
{
	register struct tty *tp;
	register int c, cc;
	register struct dzdevice *dzaddr;
	register struct tty *tp0;
	register int unit;
	int overrun = 0;
 
	if ((dzact & (1<<dz)) == 0)
		return;
	unit = dz * 8;
	dzaddr = dzpdma[unit].p_addr;
	tp0 = &dz_tty[unit];
	dzaddr->dzcsr &= ~(DZ_RIE|DZ_MIE);	/* the manual says this song */
	dzaddr->dzcsr |= DZ_RIE|DZ_MIE;		/*   and dance is necessary */
	while (dzaddr->dzcsr & DZ_MSC) {	/* DZ32 modem change interrupt */
		c = dzaddr->dzmtsr;
		tp = tp0 + (c&7);
		if (tp >= &dz_tty[dz_cnt])
			break;
		dzaddr->dzlcs = c&7;	/* get status of modem lines */
		dzwait(dzaddr);		/* wait for them */
		if (c & DZ_CD)		/* carrier status change? */
		if (dzaddr->dzlcs & DZ_CD) {	/* carrier up? */
			/* carrier present */
			(void)(*linesw[tp->t_line].l_modem)(tp, 1);
		} else if ((*linesw[tp->t_line].l_modem)(tp, 0) == 0)
			dzaddr->dzlcs = DZ_ACK|(c&7);
	}
	while ((c = dzaddr->dzrbuf) < 0) {	/* char present */
		cc = c&0xff;
		dzchars[dz]++;
		tp = tp0 + ((c>>8)&07);
		if (tp >= &dz_tty[dz_cnt])
			continue;
		if ((tp->t_state & TS_ISOPEN) == 0) {
			wakeup((caddr_t)&tp->t_rawq);
#ifdef PORTSELECTOR
			if ((tp->t_state&TS_WOPEN) == 0)
#endif
				continue;
		}
		if (c&DZ_FE)
			cc |= TTY_FE;
		if (c&DZ_DO && overrun == 0) {
			log(LOG_WARNING, "dz%d,%d: silo overflow\n", dz, (c>>8)&7);
			overrun = 1;
		}
		if (c&DZ_PE)	
			cc |= TTY_PE;
		(*linesw[tp->t_line].l_rint)(cc, tp);
	}
}
 
/*ARGSUSED*/
dzioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tty *tp;
	register int unit = minor(dev);
	register int dz = unit >> 3;
	register struct dzdevice *dzaddr;
	int error;
 
	tp = &dz_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);

	switch (cmd) {

	case TIOCSBRK:
		dzaddr = ((struct pdma *)(tp->t_addr))->p_addr;
		if (dzaddr->dzcsr&DZ_32)
			(void) dzmctl(dev, DZ_BRK, DMBIS);
		else
			dzaddr->dzbrk = (dz_brk[dz] |= 1 << (unit&07));
		break;

	case TIOCCBRK:
		dzaddr = ((struct pdma *)(tp->t_addr))->p_addr;
		if (dzaddr->dzcsr&DZ_32)
			(void) dzmctl(dev, DZ_BRK, DMBIC);
		else
			dzaddr->dzbrk = (dz_brk[dz] &= ~(1 << (unit&07)));
		break;

	case TIOCSDTR:
		(void) dzmctl(dev, DZ_DTR|DZ_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) dzmctl(dev, DZ_DTR|DZ_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) dzmctl(dev, dmtodz(*(int *)data), DMSET);
		break;

	case TIOCMBIS:
		(void) dzmctl(dev, dmtodz(*(int *)data), DMBIS);
		break;

	case TIOCMBIC:
		(void) dzmctl(dev, dmtodz(*(int *)data), DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dztodm(dzmctl(dev, 0, DMGET));
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dmtodz(bits)
	register int bits;
{
	register int b;

	b = (bits >>1) & 0370;
	if (bits & DML_ST) b |= DZ_ST;
	if (bits & DML_RTS) b |= DZ_RTS;
	if (bits & DML_DTR) b |= DZ_DTR;
	if (bits & DML_LE) b |= DZ_LE;
	return(b);
}

dztodm(bits)
	register int bits;
{
	register int b;

	b = (bits << 1) & 0360;
	if (bits & DZ_DSR) b |= DML_DSR;
	if (bits & DZ_DTR) b |= DML_DTR;
	if (bits & DZ_ST) b |= DML_ST;
	if (bits & DZ_RTS) b |= DML_RTS;
	return(b);
}
 
dzparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register struct dzdevice *dzaddr;
	register int lpr;
	register int cflag = t->c_cflag;
	int unit = minor(tp->t_dev);
	int ospeed = ttspeedtab(t->c_ospeed, dzspeedtab);

	/* check requested parameters */
        if (ospeed < 0 || (t->c_ispeed && t->c_ispeed != t->c_ospeed) ||
            (cflag&CSIZE) == CS5 || (cflag&CSIZE) == CS6)
                return(EINVAL);
        /* and copy to tty */
        tp->t_ispeed = t->c_ispeed;
        tp->t_ospeed = t->c_ospeed;
        tp->t_cflag = cflag;

	dzaddr = dzpdma[unit].p_addr;
	if (dzsilos & (1 << (unit >> 3)))
		dzaddr->dzcsr = DZ_IEN | DZ_SAE;
	else
		dzaddr->dzcsr = DZ_IEN;
	dzact |= (1<<(unit>>3));
	if (ospeed == 0) {
		(void) dzmctl(unit, DZ_OFF, DMSET);	/* hang up line */
		return;
	}
	lpr = (ospeed<<8) | (unit & 07);
	if ((cflag&CSIZE) == CS7)
		lpr |= BITS7;
	else
		lpr |= BITS8;
	if (cflag&PARENB)
		lpr |= PENABLE;
	if (cflag&PARODD)
		lpr |= OPAR;
	if (cflag&CSTOPB)
		lpr |= TWOSB;
	dzaddr->dzlpr = lpr;
	return 0;
}
 
dzxint(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register dz, unit;
 
	dp = (struct pdma *)tp->t_addr;
	tp->t_state &= ~TS_BUSY;
	if (tp->t_state & TS_FLUSH)
		tp->t_state &= ~TS_FLUSH;
	else {
		ndflush(&tp->t_outq, dp->p_mem-tp->t_outq.c_cf);
		dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	}
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		dzstart(tp);
	dz = minor(tp->t_dev) >> 3;
	unit = minor(tp->t_dev) & 7;
	if (tp->t_outq.c_cc == 0 || (tp->t_state&TS_BUSY)==0)
		if (dp->p_addr->dzcsr & DZ_32)
			dp->p_addr->dzlnen = (dz_lnen[dz] &= ~(1<<unit));
		else
			dp->p_addr->dztcr &= ~(1<<unit);
}

dzstart(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register struct dzdevice *dzaddr;
	register int cc;
	int s, dz, unit;
 
	dp = (struct pdma *)tp->t_addr;
	dzaddr = dp->p_addr;
	s = spl5();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
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
	if (tp->t_flags & (RAW|LITOUT))
		cc = ndqb(&tp->t_outq, 0);
	else {
		cc = ndqb(&tp->t_outq, 0200);
		if (cc == 0) {
			cc = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (cc&0x7f) + 6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	tp->t_state |= TS_BUSY;
	dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	dp->p_end += cc;
	dz = minor(tp->t_dev) >> 3;
	unit = minor(tp->t_dev) & 7;
	if (dzaddr->dzcsr & DZ_32)
		dzaddr->dzlnen = (dz_lnen[dz] |= (1<<unit));
	else
		dzaddr->dztcr |= (1<<unit);
out:
	splx(s);
}
 
/*
 * Stop output on a line.
 */
/*ARGSUSED*/
dzstop(tp, flag)
	register struct tty *tp;
{
	register struct pdma *dp;
	register int s;

	dp = (struct pdma *)tp->t_addr;
	s = spl5();
	if (tp->t_state & TS_BUSY) {
		dp->p_end = dp->p_mem;
		if ((tp->t_state&TS_TTSTOP)==0)
			tp->t_state |= TS_FLUSH;
	}
	splx(s);
}
 
dzmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct dzdevice *dzaddr;
	register int unit, mbits;
	int b, s;

	unit = minor(dev);
	b = 1<<(unit&7);
	dzaddr = dzpdma[unit].p_addr;
	s = spl5();
	if (dzaddr->dzcsr & DZ_32) {
		dzwait(dzaddr)
		DELAY(100);		/* IS 100 TOO MUCH? */
		dzaddr->dzlcs = unit&7;
		DELAY(100);
		dzwait(dzaddr)
		DELAY(100);
		mbits = dzaddr->dzlcs;
		mbits &= 0177770;
	} else {
		mbits = (dzaddr->dzdtr & b) ? DZ_DTR : 0;
		mbits |= (dzaddr->dzmsr & b) ? DZ_CD : 0;
		mbits |= (dzaddr->dztbuf & b) ? DZ_RI : 0;
	}
	switch (how) {
	case DMSET:
		mbits = bits;
		break;

	case DMBIS:
		mbits |= bits;
		break;

	case DMBIC:
		mbits &= ~bits;
		break;

	case DMGET:
		(void) splx(s);
		return(mbits);
	}
	if (dzaddr->dzcsr & DZ_32) {
		mbits |= DZ_ACK|(unit&7);
		dzaddr->dzlcs = mbits;
	} else {
		if (mbits & DZ_DTR)
			dzaddr->dzdtr |= b;
		else
			dzaddr->dzdtr &= ~b;
	}
	if (mbits & DZ_DTR && dzsoftCAR[unit >> 3] & b)
		dz_tty[unit].t_state |= TS_CARR_ON;
	(void) splx(s);
	return(mbits);
}
 
int dztransitions, dzfasttimers;		/*DEBUG*/
dzscan()
{
	register i;
	register struct dzdevice *dzaddr;
	register bit;
	register struct tty *tp;
	register car;
	int olddzsilos = dzsilos;
	int dztimer();
 
	for (i = 0; i < dz_cnt ; i++) {
		dzaddr = dzpdma[i].p_addr;
		if (dzaddr == 0)
			continue;
		tp = &dz_tty[i];
		bit = 1<<(i&07);
		car = 0;
		if (dzsoftCAR[i>>3]&bit)
			car = 1;
		else if (dzaddr->dzcsr & DZ_32) {
			dzaddr->dzlcs = i&07;
			dzwait(dzaddr);
			car = dzaddr->dzlcs & DZ_CD;
		} else
			car = dzaddr->dzmsr&bit;
		if (car) {
			/* carrier present */
			if ((tp->t_state & TS_CARR_ON) == 0)
				(void)(*linesw[tp->t_line].l_modem)(tp, 1);
		} else if ((tp->t_state&TS_CARR_ON) &&
		    (*linesw[tp->t_line].l_modem)(tp, 0) == 0)
			dzaddr->dzdtr &= ~bit;
	}
	for (i = 0; i < NDZ; i++) {
		ave(dzrate[i], dzchars[i], 8);
		if (dzchars[i] > dzhighrate && ((dzsilos & (1 << i)) == 0)) {
			dzpdma[i << 3].p_addr->dzcsr = DZ_IEN | DZ_SAE;
			dzsilos |= (1 << i);
			dztransitions++;		/*DEBUG*/
		} else if ((dzsilos & (1 << i)) && (dzrate[i] < dzlowrate)) {
			dzpdma[i << 3].p_addr->dzcsr = DZ_IEN;
			dzsilos &= ~(1 << i);
		}
		dzchars[i] = 0;
	}
	if (dzsilos && !olddzsilos)
		timeout(dztimer, (caddr_t)0, dztimerintvl);
	timeout(dzscan, (caddr_t)0, hz);
}

dztimer()
{
	register int dz;
	register int s;

	if (dzsilos == 0)
		return;
	s = spl5();
	dzfasttimers++;		/*DEBUG*/
	for (dz = 0; dz < NDZ; dz++)
		if (dzsilos & (1 << dz))
		    dzrint(dz);
	splx(s);
	timeout(dztimer, (caddr_t) 0, dztimerintvl);
}

/*
 * Reset state of driver if UBA reset was necessary.
 * Reset parameters and restart transmission on open lines.
 */
dzreset(uban)
	int uban;
{
	register int unit;
	register struct tty *tp;
	register struct uba_device *ui;

	for (unit = 0; unit < NDZLINE; unit++) {
		ui = dzinfo[unit >> 3];
		if (ui == 0 || ui->ui_ubanum != uban || ui->ui_alive == 0)
			continue;
		if (unit%8 == 0)
			printf(" dz%d", unit>>3);
		tp = &dz_tty[unit];
		if (tp->t_state & (TS_ISOPEN|TS_WOPEN)) {
			dzparam(unit);
			(void) dzmctl(unit, DZ_ON, DMSET);
			tp->t_state &= ~TS_BUSY;
			dzstart(tp);
		}
	}
}
#endif
