/*	dz.c	4.25	81/05/09	*/

#include "dz.h"
#if NDZ > 0
/*
 *  DZ-11 Driver
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
#include "../h/mx.h"

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

/* Bits in dzlpr */
#define	BITS7	020
#define	BITS8	030
#define	TWOSB	040
#define	PENABLE	0100
#define	OPAR	0200

/* Bits in dzrbuf */
#define	DZ_PE	010000
#define	DZ_FE	020000
#define	DZ_DO	040000

/* Bits in dzcsr */
#define	DZ_CLR	020		/* Reset dz */
#define	DZ_MSE	040		/* Master Scan Enable */
#define	DZ_RIE	0100		/* Receiver Interrupt Enable */
#define	DZ_SAE	010000		/* Silo Alarm Enable */
#define	DZ_TIE	040000		/* Transmit Interrupt Enable */
#if NBK == 0
#define	DZ_IEN	(DZ_MSE|DZ_RIE|DZ_TIE)
#else
#define	DZ_IEN	(DZ_MSE|DZ_RIE|DZ_TIE|DZ_SAE)
#endif

/* Flags for modem-control */
#define	DZ_ON	1
#define	DZ_OFF	0
 
int	dzstart(), dzxint(), dzdma();
int	ttrstrt();
struct	tty dz_tty[NDZLINE];
int	dz_cnt = { NDZLINE };
int	dzact;

struct device {
	short	dzcsr;		/* control-status register */
	short	dzrbuf;		/* receiver buffer */
#define	dzlpr	dzrbuf		/* line parameter reg is write of dzrbuf */
	char	dztcr;		/* transmit control register */
	char	dzdtr;		/* data terminal ready */
	char	dztbuf;		/* transmit buffer */
	char	dzbrk;		/* break control */
#define	dzmsr	dzbrk		/* modem status register */
};
/*
 * Software copy of dzbrk since it isn't readable
 */
char	dz_brk[NDZ];
char	dzsoftCAR[NDZ];

/*
 * The dz doesn't interrupt on carrier transitions, so
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
#endif
	dzaddr->dzcsr = DZ_TIE|DZ_MSE;
	dzaddr->dztcr = 1;		/* enable any line */
	DELAY(100000);
	dzaddr->dzcsr = DZ_CLR;		/* reset everything */
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
	return (1);
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
	tp->t_iproc = NULL;
	tp->t_state |= WOPEN;
	if ((tp->t_state & ISOPEN) == 0) {
		ttychars(tp);
		tp->t_ospeed = tp->t_ispeed = B300;
		tp->t_flags = ODDP|EVENP|ECHO;
		/* tp->t_state |= HUPCLS; */
		dzparam(unit);
	} else if (tp->t_state&XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return;
	}
	dzmodem(unit, DZ_ON);
	(void) spl5();
	while ((tp->t_state & CARR_ON) == 0) {
		tp->t_state |= WOPEN;
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
	int dz;
 
	unit = minor(dev);
	dz = unit >> 3;
	tp = &dz_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	((struct pdma *)(tp->t_addr))->p_addr->dzbrk =
	    (dz_brk[dz] &= ~(1 << (unit&07)));
	if (tp->t_state & HUPCLS)
		dzmodem(unit, DZ_OFF);
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
	while ((c = dzaddr->dzrbuf) < 0) {	/* char present */
		tp = tp0 + ((c>>8)&07);
		if (tp >= &dz_tty[dz_cnt])
			continue;
		if ((tp->t_state & ISOPEN) == 0) {
			wakeup((caddr_t)&tp->t_rawq);
			continue;
		}
		if (c&DZ_FE)
			if (tp->t_flags & RAW)
				c = 0;
			else
				c = tun.t_intrc;
		if (c&DZ_DO && overrun == 0) {
			printf("dz%d: silo overflow\n", dz);
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
 
	tp = &dz_tty[unit];
	cmd = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (cmd == 0)
		return;
	if (ttioctl(tp, cmd, addr, flag)) {
		if (cmd==TIOCSETP || cmd==TIOCSETN)
			dzparam(unit);
	} else switch(cmd) {

	case TIOCSBRK:
		((struct pdma *)(tp->t_addr))->p_addr->dzbrk =
			(dz_brk[dz] |= 1 << (unit&07));
		break;
	case TIOCCBRK:
		((struct pdma *)(tp->t_addr))->p_addr->dzbrk =
			(dz_brk[dz] &= ~(1 << (unit&07)));
		break;
	case TIOCSDTR:
		dzmodem(unit, DZ_ON);
		break;
	case TIOCCDTR:
		dzmodem(unit, DZ_OFF);
		break;
	default:
		u.u_error = ENOTTY;
	}
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
		dzmodem(unit, DZ_OFF);		/* hang up line */
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
	register s;
 
	s = spl5();		/* block pdma interrupts */
	dp = (struct pdma *)tp->t_addr;
	tp->t_state &= ~BUSY;
	if (tp->t_state & FLUSH)
		tp->t_state &= ~FLUSH;
	else
		ndflush(&tp->t_outq, dp->p_mem-tp->t_outq.c_cf);
	if (tp->t_line)
		(*linesw[tp->t_line].l_start)(tp);
	else
		dzstart(tp);
	if (tp->t_outq.c_cc == 0 || (tp->t_state&BUSY)==0)
		dp->p_addr->dztcr &= ~(1 << (minor(tp->t_dev)&07));
	splx(s);
}

dzstart(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register struct device *dzaddr;
	register int cc;
	int s;
 
	dp = (struct pdma *)tp->t_addr;
	dzaddr = dp->p_addr;
	s = spl5();
	if (tp->t_state & (TIMEOUT|BUSY|TTSTOP))
		goto out;
	if (tp->t_state&ASLEEP && tp->t_outq.c_cc <= TTLOWAT(tp)) {
		tp->t_state &= ~ASLEEP;
		if (tp->t_chan)
			mcstart(tp->t_chan, (caddr_t)&tp->t_outq);
		else
			wakeup((caddr_t)&tp->t_outq);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags&RAW || tp->t_flags&LLITOUT)
		cc = ndqb(&tp->t_outq, 0);
	else {
		cc = ndqb(&tp->t_outq, 0200);
		if (cc == 0) {
			cc = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (cc&0x7f) + 6);
			tp->t_state |= TIMEOUT;
			goto out;
		}
	}
	tp->t_state |= BUSY;
	dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	dp->p_end += cc;
	dzaddr->dztcr |= 1 << (minor(tp->t_dev) & 07);	/* force intr */
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
	if (tp->t_state & BUSY) {
		dp->p_end = dp->p_mem;
		if ((tp->t_state&TTSTOP)==0)
			tp->t_state |= FLUSH;
	}
	splx(s);
}
 
dzmodem(unit, flag)
	register int unit;
{
	register struct device *dzaddr;
	register char bit;
 
	dzaddr = dzpdma[unit].p_addr;
	bit = 1<<(unit&07);
	if (flag == DZ_OFF)
		dzaddr->dzdtr &= ~bit;
	else
		dzaddr->dzdtr |= bit;
}
 
dzscan()
{
	register i;
	register struct device *dzaddr;
	register bit;
	register struct tty *tp;
 
	for (i = 0; i < dz_cnt ; i++) {
		dzaddr = dzpdma[i].p_addr;
		if (dzaddr == 0)
			continue;
		tp = &dz_tty[i];
		bit = 1<<(i&07);
		if ((dzsoftCAR[i>>3]&bit) || (dzaddr->dzmsr&bit)) {
			/* carrier present */
			if ((tp->t_state & CARR_ON) == 0) {
				wakeup((caddr_t)&tp->t_rawq);
				tp->t_state |= CARR_ON;
			}
		} else {
			if ((tp->t_state&CARR_ON) &&
			    (tp->t_local&LNOHANG)==0) {
				/* carrier lost */
				if (tp->t_state&ISOPEN) {
					gsignal(tp->t_pgrp, SIGHUP);
					gsignal(tp->t_pgrp, SIGCONT);
					dzaddr->dzdtr &= ~bit;
					flushtty(tp, FREAD|FWRITE);
				}
				tp->t_state &= ~CARR_ON;
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
		if (tp->t_state & (ISOPEN|WOPEN)) {
			dzparam(unit);
			dzmodem(unit, DZ_ON);
			tp->t_state &= ~BUSY;
			dzstart(tp);
		}
	}
	dztimer();
}
#endif
