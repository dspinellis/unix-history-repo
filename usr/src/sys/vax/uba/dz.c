/*	dz.c	4.34	82/03/12	*/

#include "dz.h"
#if NDZ > 0
/*
 *  DZ-11 and DZ32 Driver
 *
 * This driver mimics dh.c; see it for explanation of common code.
 */
#include "bk.h"
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/tty.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/vm.h"
#include "../h/ubavar.h"
#include "../h/conf.h"
#include "../h/pdma.h"
#include "../h/bk.h"
#include "../h/file.h"

/*
 * Driver information for auto-configuration stuff.
 */
int	dzprobe(), dzattach(), dzrint();
struct	uba_device *dzinfo[NDZ];
u_short	dzstd[] = { 0 };
struct	uba_driver dzdriver =
	{ dzprobe, 0, dzattach, 0, dzstd, "dz", dzinfo };

#define	NDZLINE 	(NDZ*8)
 
/*
 * Registers and bits
 */

/* bits in dzlpr */
#define	BITS7	0020
#define	BITS8	0030
#define	TWOSB	0040
#define	PENABLE	0100
#define	OPAR	0200

/* bits in dzrbuf */
#define	DZ_PE	010000
#define	DZ_FE	020000
#define	DZ_DO	040000

/* bits in dzcsr */
#define	DZ_32	000001		/* DZ32 mode */
#define	DZ_MIE	000002		/* Modem Interrupt Enable */
#define	DZ_CLR	000020		/* Reset dz */
#define	DZ_MSE	000040		/* Master Scan Enable */
#define	DZ_RIE	000100		/* Receiver Interrupt Enable */
#define DZ_MSC	004000		/* Modem Status Change */
#define	DZ_SAE	010000		/* Silo Alarm Enable */
#define	DZ_TIE	040000		/* Transmit Interrupt Enable */
#define	DZ_IEN	(DZ_32|DZ_MIE|DZ_MSE|DZ_RIE|DZ_TIE|DZ_SAE)

/* flags for modem-control */
#define	DZ_ON	DZ_DTR
#define	DZ_OFF	0

/* bits in dzlcs */
#define DZ_ACK	0100000		/* ACK bit in dzlcs */
#define DZ_RTS	0010000		/* Request To Send */
#define	DZ_ST	0004000		/* Secondary Transmit */
#define	DZ_BRK	0002000		/* Break */
#define DZ_DTR	0001000		/* Data Terminal Ready */
#define	DZ_LE	0000400		/* Line Enable */
#define	DZ_DSR	0000200		/* Data Set Ready */
#define	DZ_RI	0000100		/* Ring Indicate */
#define DZ_CD	0000040		/* Carrier Detect */
#define	DZ_CTS	0000020		/* Clear To Send */
#define	DZ_SR	0000010		/* Secondary Receive */
 
/* bits in dm lsr, copied from dh.c */
#define	DML_DSR		0000400		/* data set ready, not a real DM bit */
#define	DML_RNG		0000200		/* ring */
#define	DML_CAR		0000100		/* carrier detect */
#define	DML_CTS		0000040		/* clear to send */
#define	DML_SR		0000020		/* secondary receive */
#define	DML_ST		0000010		/* secondary transmit */
#define	DML_RTS		0000004		/* request to send */
#define	DML_DTR		0000002		/* data terminal ready */
#define	DML_LE		0000001		/* line enable */

int	dzstart(), dzxint(), dzdma();
int	ttrstrt();
struct	tty dz_tty[NDZLINE];
int	dz_cnt = { NDZLINE };
int	dzact;

struct device {
	short dzcsr;
	short dzrbuf;
	union {
		struct {
			char	dztcr0;
			char	dzdtr0;
			char	dztbuf0;
			char	dzbrk0;
		} dz11;
		struct {
			short	dzlcs0;
			char	dztbuf0;
			char	dzlnen0;
		} dz32;
	} dzun;
};

#define dzlpr	dzrbuf
#define dzmsr	dzun.dz11.dzbrk0
#define dztcr	dzun.dz11.dztcr0
#define dzdtr	dzun.dz11.dzdtr0
#define dztbuf	dzun.dz11.dztbuf0
#define dzlcs	dzun.dz32.dzlcs0
#define	dzbrk	dzmsr
#define dzlnen	dzun.dz32.dzlnen0
#define dzmtsr	dzun.dz32.dztbuf0;

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

char	dz_speeds[] =
	{ 0,020,021,022,023,024,0,025,026,027,030,032,034,036,0,0 };
 
dzprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
	register struct device *dzaddr = (struct device *)reg;

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
	return (1);
}

dzattach(ui)
	register struct uba_device *ui;
{
	register struct pdma *pdp = &dzpdma[ui->ui_unit*8];
	register struct tty *tp = &dz_tty[ui->ui_unit*8];
	register int cntr;
	extern dzscan();

	for (cntr = 0; cntr < 8; cntr++) {
		pdp->p_addr = (struct device *)ui->ui_addr;
		pdp->p_arg = (int)tp;
		pdp->p_fcn = dzxint;
		pdp++, tp++;
	}
	dzsoftCAR[ui->ui_unit] = ui->ui_flags;
	if (dz_timer == 0) {
		dz_timer++;
		timeout(dzscan, (caddr_t)0, hz);
	}
}

/*ARGSUSED*/
dzopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;
 
	unit = minor(dev);
	if (unit >= dz_cnt || dzpdma[unit].p_addr == 0) {
		u.u_error = ENXIO;
		return;
	}
	tp = &dz_tty[unit];
	tp->t_addr = (caddr_t)&dzpdma[unit];
	tp->t_oproc = dzstart;
	tp->t_state |= TS_WOPEN;
	if ((tp->t_state & TS_ISOPEN) == 0) {
		ttychars(tp);
		tp->t_ospeed = tp->t_ispeed = B300;
		tp->t_flags = ODDP|EVENP|ECHO;
		/* tp->t_state |= TS_HUPCLS; */
		dzparam(unit);
	} else if (tp->t_state&TS_XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return;
	}
	dzmctl(dev, DZ_ON, DMSET);
	(void) spl5();
	while ((tp->t_state & TS_CARR_ON) == 0) {
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	(void) spl0();
	(*linesw[tp->t_line].l_open)(dev, tp);
}
 
/*ARGSUSED*/
dzclose(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;
	register struct device *dzaddr;
	int dz;
 
	unit = minor(dev);
	dz = unit >> 3;
	tp = &dz_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	dzaddr = dzpdma[unit].p_addr;
	if (dzaddr->dzcsr&DZ_32)
		dzmctl(dev, DZ_BRK, DMBIC);
	else
		dzaddr->dzbrk = (dz_brk[dz] &= ~(1 << (unit&07)));
	if ((tp->t_state&TS_HUPCLS) || (tp->t_state&TS_ISOPEN) == 0)
		dzmctl(dev, DZ_OFF, DMSET);
	ttyclose(tp);
}
 
dzread(dev)
	dev_t dev;
{
	register struct tty *tp;
 
	tp = &dz_tty[minor(dev)];
	(*linesw[tp->t_line].l_read)(tp);
}
 
dzwrite(dev)
	dev_t dev;
{
	register struct tty *tp;
 
	tp = &dz_tty[minor(dev)];
	(*linesw[tp->t_line].l_write)(tp);
}
 
/*ARGSUSED*/
dzrint(dz)
	int dz;
{
	register struct tty *tp;
	register int c;
	register struct device *dzaddr;
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
			if ((tp->t_state&TS_CARR_ON) == 0) {
				wakeup((caddr_t)&tp->t_rawq);
				tp->t_state |= TS_CARR_ON;
			}
		} else {	/* no carrier */
			if (tp->t_state&TS_CARR_ON) {
				gsignal(tp->t_pgrp, SIGHUP);
				gsignal(tp->t_pgrp, SIGCONT);
				dzaddr->dzlcs = DZ_ACK|(c&7);
				flushtty(tp, FREAD|FWRITE);
			}
			tp->t_state &= ~TS_CARR_ON;
		}
	}
	while ((c = dzaddr->dzrbuf) < 0) {	/* char present */
		tp = tp0 + ((c>>8)&07);
		if (tp >= &dz_tty[dz_cnt])
			continue;
		if ((tp->t_state & TS_ISOPEN) == 0) {
			wakeup((caddr_t)&tp->t_rawq);
			continue;
		}
		if (c&DZ_FE)
			if (tp->t_flags & RAW)
				c = 0;
			else
				c = tun.t_intrc;
		if (c&DZ_DO && overrun == 0) {
			/* printf("dz%d,%d: silo overflow\n", dz, (c>>8)&7); */
			overrun = 1;
		}
		if (c&DZ_PE)	
			if (((tp->t_flags & (EVENP|ODDP)) == EVENP)
			  || ((tp->t_flags & (EVENP|ODDP)) == ODDP))
				continue;
#if NBK > 0
		if (tp->t_line == NETLDISC) {
			c &= 0177;
			BKINPUT(c, tp);
		} else
#endif
			(*linesw[tp->t_line].l_rint)(c, tp);
	}
}
 
/*ARGSUSED*/
dzioctl(dev, cmd, addr, flag)
	dev_t dev;
	caddr_t addr;
{
	register struct tty *tp;
	register int unit = minor(dev);
	register int dz = unit >> 3;
	register struct device *dzaddr;
	int temp;
 
	tp = &dz_tty[unit];
	cmd = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (cmd == 0)
		return;
	if (ttioctl(tp, cmd, addr, flag)) {
		if (cmd==TIOCSETP || cmd==TIOCSETN)
			dzparam(unit);
	} else switch(cmd) {

	case TIOCSBRK:
		dzaddr = ((struct pdma *)(tp->t_addr))->p_addr;
		if (dzaddr->dzcsr&DZ_32)
			dzmctl(dev, DZ_BRK, DMBIS);
		else
			dzaddr->dzbrk = (dz_brk[dz] |= 1 << (unit&07));
		break;
	case TIOCCBRK:
		dzaddr = ((struct pdma *)(tp->t_addr))->p_addr;
		if (dzaddr->dzcsr&DZ_32)
			dzmctl(dev, DZ_BRK, DMBIC);
		else
			dzaddr->dzbrk = (dz_brk[dz] &= ~(1 << (unit&07)));
		break;
	case TIOCSDTR:
		dzmctl(dev, DZ_DTR|DZ_RTS, DMBIS);
		break;
	case TIOCCDTR:
		dzmctl(dev, DZ_DTR|DZ_RTS, DMBIC);
		break;
	case TIOCMSET:
		if (copyin(addr, (caddr_t) &temp, sizeof(temp)))
			u.u_error = EFAULT;
		else
			dzmctl(dev, dmtodz(temp), DMSET);
		break;
	case TIOCMBIS:
		if (copyin(addr, (caddr_t) &temp, sizeof(temp)))
			u.u_error = EFAULT;
		else
			dzmctl(dev, dmtodz(temp), DMBIS);
		break;
	case TIOCMBIC:
		if (copyin(addr, (caddr_t) &temp, sizeof(temp)))
			u.u_error = EFAULT;
		else
			dzmctl(dev, dmtodz(temp), DMBIC);
		break;
	case TIOCMGET:
		temp = dztodm(dzmctl(dev, 0, DMGET));
		if (copyout((caddr_t) &temp, addr, sizeof(temp)))
			u.u_error = EFAULT;
		break;
	default:
		u.u_error = ENOTTY;
	}
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
 
dzparam(unit)
	register int unit;
{
	register struct tty *tp;
	register struct device *dzaddr;
	register int lpr;
 
	tp = &dz_tty[unit];
	dzaddr = dzpdma[unit].p_addr;
	dzaddr->dzcsr = DZ_IEN;
	dzact |= (1<<(unit>>3));
	if (tp->t_ispeed == 0) {
		dzmctl(unit, DZ_OFF, DMSET);	/* hang up line */
		return;
	}
	lpr = (dz_speeds[tp->t_ispeed]<<8) | (unit & 07);
	if ((tp->t_local&LLITOUT) || (tp->t_flags&RAW))
		lpr |= BITS8;
	else
		lpr |= (BITS7|PENABLE);
	if ((tp->t_flags & EVENP) == 0)
		lpr |= OPAR;
	if (tp->t_ispeed == B110)
		lpr |= TWOSB;
	dzaddr->dzlpr = lpr;
}
 
dzxint(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register s, dz, unit;
 
	s = spl5();		/* block pdma interrupts */
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
	splx(s);
}

dzstart(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register struct device *dzaddr;
	register int cc;
	int s, dz, unit;
 
	dp = (struct pdma *)tp->t_addr;
	dzaddr = dp->p_addr;
	s = spl5();
	if (tp->t_state & (TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
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
	if (tp->t_outq.c_cc == 0)
		goto out;
	if ((tp->t_flags&RAW) || (tp->t_local&LLITOUT))
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
	register struct device *dzaddr;
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
	(void) splx(s);
	return(mbits);
}
 
dzscan()
{
	register i;
	register struct device *dzaddr;
	register bit;
	register struct tty *tp;
	register car;
 
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
			if ((tp->t_state & TS_CARR_ON) == 0) {
				wakeup((caddr_t)&tp->t_rawq);
				tp->t_state |= TS_CARR_ON;
			}
		} else {
			if ((tp->t_state&TS_CARR_ON) &&
			    (tp->t_local&LNOHANG)==0) {
				/* carrier lost */
				if (tp->t_state&TS_ISOPEN) {
					gsignal(tp->t_pgrp, SIGHUP);
					gsignal(tp->t_pgrp, SIGCONT);
					dzaddr->dzdtr &= ~bit;
					flushtty(tp, FREAD|FWRITE);
				}
				tp->t_state &= ~TS_CARR_ON;
			}
		}
	}
	timeout(dzscan, (caddr_t)0, 2*hz);
}

dztimer()
{
	int dz;

	for (dz = 0; dz < NDZ; dz++)
		dzrint(dz);
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
			dzmctl(unit, DZ_ON, DMSET);
			tp->t_state &= ~TS_BUSY;
			dzstart(tp);
		}
	}
	dztimer();
}
#endif
