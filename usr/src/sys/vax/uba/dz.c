/*	dz.c	4.11	%G%	*/

#include "dz.h"
#if NDZ11 > 0
/*
 *  DZ-11 Driver
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/tty.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/uba.h"
#include "../h/conf.h"
#include "../h/pdma.h"
#include "../h/bk.h"
#include "../h/file.h"
#include "../h/mx.h"

/*
 * When running dz's using only SAE (silo alarm) on input
 * it is necessary to call dzrint() at clock interrupt time.
 * This is unsafe unless spl5()s in tty code are changed to
 * spl6()s to block clock interrupts.  Note that the dh driver
 * currently in use works the same way as the dz, even though
 * we could try to more intelligently manage its silo.
 * Thus don't take this out if you have no dz's unless you
 * change clock.c and dhtimer().
 *
 * SHOULD RATHER QUEUE SOFTWARE INTERRUPT AT CLOCK TIME.
 */
#define	spl5	spl6
 
int	dzcntrlr(), dzslave(), dzrint();
struct	uba_dinfo *dzinfo[NDZ11];
u_short	dzstd[] = { 0 };
struct	uba_driver dzdriver =
	{ dzcntrlr, dzslave, (int (*)())0, 0, 0, dzstd, "dz", dzinfo };

#define NDZ 	(NDZ11*8)
 
#define BITS7	020
#define BITS8	030
#define TWOSB	040
#define PENABLE	0100
#define OPAR	0200
#define	CLR	020		/* Reset dz */
#define MSE	040		/* Master Scan Enable */
#define RIE	0100		/* Receiver Interrupt Enable */
#define	SAE	010000		/* Silo Alarm Enable */
#define TIE	040000		/* Transmit Interrupt Enable */
#define DZ_IEN	(MSE+RIE+TIE+SAE)
#define PERROR	010000
#define FRERROR	020000
#define	OVERRUN	040000
#define SSPEED	7		/* std speed = 300 baud */

#define	dzlpr	dzrbuf
#define dzmsr	dzbrk
#define ON	1
#define OFF	0
 
int	dzstart();
int	dzxint();
int	dzdma();
int	ttrstrt();
struct	tty dz_tty[NDZ];
int	dz_cnt = { NDZ };
int	dzact;

struct device {
	short	dzcsr;
	short	dzrbuf;
	char	dztcr;
	char	dzdtr;
	char	dztbuf;
	char	dzbrk;
};

struct	pdma dzpdma[NDZ];
char	dz_timer;
char	dz_speeds[] =
	{ 0,020,021,022,023,024,0,025,026,027,030,032,034,036,0,0 };
char	dz_brk[NDZ11];
 
dzcntrlr(ui, reg)
	struct uba_dinfo *ui;
	caddr_t reg;
{
	int	i;		/* NB: NOT REGISTER */
	struct	device *dzaddr = (struct device *)reg;

	dzaddr->dzcsr = TIE|MSE;
	dzaddr->dztcr = 1;		/* enable any line */
	for (i = 0; i < 1000000; i++)
		;
	dzaddr->dzcsr = CLR;		/* reset everything */
	asm("cmpl r10,$0x200;beql 1f;subl2 $4,r10;1:;");
	return(1);
}

dzslave(ui, reg, slaveno, uban)
	register struct uba_dinfo *ui;
	caddr_t reg;
{
	register struct pdma *pdp = &dzpdma[ui->ui_unit*8];
	register struct tty *tp = &dz_tty[ui->ui_unit*8];
	register int cnt;
	caddr_t cp;

	for (cnt = 0; cnt < 8; cnt++) {
		pdp->p_addr = (struct device *)reg;
		pdp->p_arg = (int)tp;
		pdp->p_fcn = dzxint;
		pdp++, tp++;
	}
	return (1);
}

/*ARGSUSED*/
dzopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit;
	extern dzscan();
 
	unit = minor(dev);
	if (unit >= dz_cnt || dzpdma[unit].p_addr == 0) {
		u.u_error = ENXIO;
		return;
	}
	if (dz_timer == 0) {
		dz_timer++;
		timeout(dzscan, (caddr_t)0, 60);
	}
	tp = &dz_tty[unit];
	tp->t_addr = (caddr_t)&dzpdma[unit];
	tp->t_oproc = dzstart;
	tp->t_iproc = NULL;
	tp->t_state |= WOPEN;
	if ((tp->t_state & ISOPEN) == 0) {
		ttychars(tp);
		tp->t_ospeed = tp->t_ispeed = SSPEED;
		tp->t_flags = ODDP|EVENP|ECHO;
		/*tp->t_state |= HUPCLS;*/
		dzparam(unit);
	} else if (tp->t_state&XCLUDE && u.u_uid != 0) {
		u.u_error = EBUSY;
		return;
	}
	dzmodem(unit, ON);
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
		dzmodem(unit, OFF);
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
	int s;
 
	s = spl6();	/* see comment in clock.c */
	/* as long as we are here, service them all */
	for (unit = 0; unit < NDZ; unit += 8) {
		if ((dzact & (1<<(unit>>3))) == 0)
			continue;
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
			if (c&FRERROR)
				/* framing error = break */
				if (tp->t_flags & RAW)
					c = 0;		/* null for getty */
				else
					c = tun.t_intrc;
			if (c&OVERRUN)
				printf("o");
			if (c&PERROR)	
				/* parity error */
				if (((tp->t_flags & (EVENP|ODDP)) == EVENP)
				  || ((tp->t_flags & (EVENP|ODDP)) == ODDP))
					continue;
			if (tp->t_line == NETLDISC) {
				c &= 0177;
				BKINPUT(c, tp);
			} else
				(*linesw[tp->t_line].l_rint)(c, tp);
		}
	}
	splx(s);
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
		dzmodem(unit, ON);
		break;
	case TIOCCDTR:
		dzmodem(unit, OFF);
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
		dzmodem(unit, OFF);		/* hang up line */
		return;
	}
	lpr = (dz_speeds[tp->t_ispeed]<<8) | (unit & 07);
	if ((tp->t_local&LLITOUT) || (tp->t_flags&RAW))
		lpr |= BITS8;
	else
		lpr |= (BITS7|PENABLE);
	if ((tp->t_flags & EVENP) == 0)
		lpr |= OPAR;
	if (tp->t_ispeed == 3)
		lpr |= TWOSB; 			/* 110 baud: 2 stop bits */
	dzaddr->dzlpr = lpr;
}
 
dzxint(tp)
	register struct tty *tp;
{
	register struct pdma *dp;
	register s;
	s = spl6();	/* block the clock */
 
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
	if (tp->t_flags&RAW)
		cc = ndqb(&tp->t_outq, 0);
	else {
		cc = ndqb(&tp->t_outq, 0200);
		if (cc == 0) {
			cc = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (cc&0177) + 6);
			tp->t_state |= TIMEOUT;
			goto out;
		}
	}
	tp->t_state |= BUSY;
	dp->p_end = dp->p_mem = tp->t_outq.c_cf;
	dp->p_end += cc;
	dzaddr->dztcr |= 1 << (minor(tp->t_dev) & 07);
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
	s = spl6();
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
	if (flag == OFF)
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
		tp = &dz_tty[i];
		bit = 1<<(i&07);
#ifdef BERT
		if (dzaddr->dzmsr & bit || i == 6 || i == 7) {
#else
		if (dzaddr->dzmsr & bit) {
#endif
			/* carrier present */
			if ((tp->t_state & CARR_ON) == 0) {
				wakeup((caddr_t)&tp->t_rawq);
				tp->t_state |= CARR_ON;
			}
		} else {
			if ((tp->t_state&CARR_ON) && (tp->t_local&LNOHANG)==0) {
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
	timeout(dzscan, (caddr_t)0, 2*HZ);
}

dztimer()
{

	dzrint(0);
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
	register struct uba_dinfo *ui;
	int any = 0;

	for (unit = 0; unit < NDZ; unit++) {
		ui = dzinfo[unit >> 3];
		if (ui == 0 || ui->ui_ubanum != uban || ui->ui_alive == 0)
			continue;
		if (any == 0) {
			printf(" dz");
			any++;
		}
		tp = &dz_tty[unit];
		if (tp->t_state & (ISOPEN|WOPEN)) {
			dzparam(unit);
			dzmodem(unit, ON);
			tp->t_state &= ~BUSY;
			dzstart(tp);
		}
	}
	dztimer();
}
#endif
