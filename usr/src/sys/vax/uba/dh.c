/*	dh.c	4.14	81/02/15	*/

#include "dh.h"
#if NDH11 > 0
/*
 * DH-11 driver
 *
 * DOESNT HANDLE EXTENDED ADDRESS BITS.
 */

#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/uba.h"
#include "../h/bk.h"
#include "../h/clist.h"
#include "../h/mx.h"

/* This is to block the clock because we are using the silos */
/* SHOULD RATHER QUEUE SOFTWARE INTERRUPT AT CLOCK TIME */
#define	spl5	spl6

#define	UBACVT(x,uban) (cbase[uban] + (short)((x)-(char *)cfree))

int	dhcntrlr(), dhslave(), dhrint(), dhxint();
struct	uba_dinfo *dhinfo[NDH11];
u_short	dhstd[] = { 0 };
struct	uba_driver dhdriver =
	{ dhcntrlr, dhslave, (int (*)())0, 0, 0, dhstd, "dh", dhinfo };

struct	tty dh11[NDH11*16];
int	dhact;
int	dhisilo;
int	ndh11	= NDH11*16;
int	dhstart();
int	ttrstrt();
int	dh_ubinfo[MAXNUBA];
int	cbase[MAXNUBA];

/*
 * Hardware control bits
 */
#define	BITS6	01
#define	BITS7	02
#define	BITS8	03
#define	TWOSB	04
#define	PENABLE	020
/* DEC manuals incorrectly say this bit causes generation of even parity. */
#define	OPAR	040
#define	HDUPLX	040000

#define	MAINT	01000
#define	IENAB	030100
#define	NXM	02000
#define	CLRNXM	0400
#define	PERROR	010000
#define	FRERROR	020000
#define	OVERRUN	040000
#define	XINT	0100000
#define	RINT	0100
#define	SSPEED	7	/* standard speed: 300 baud */

/*
 * DM control bits
 */
#define	TURNON	03	/* CD lead + line enable */
#define	TURNOFF	01	/* line enable */
#define	DTR	02	/* data terminal ready */
#define	RQS	04	/* request to send */

/*
 * Software copy of last dhbar
 */
short	dhsar[NDH11];

struct device
{
	union {
		short	dhcsr;
		char	dhcsrl;
	} un;
	short	dhnxch;
	short	dhlpr;
	unsigned short	dhcar;
	short	dhbcr;
	unsigned short	dhbar;
	short	dhbreak;
	short	dhsilo;
};

dhcntrlr(ui, reg)
	struct uba_dinfo *ui;
	caddr_t reg;
{
	struct device *dhaddr = (struct device *)reg;
	int i;

	dhaddr->un.dhcsr = IENAB;
	dhaddr->dhbcr = -1;
	dhaddr->dhbar = 1;
	dhaddr->dhcar = 0;
	for (i = 0; i < 1000000; i++)
		;
	/* we should have had an interrupt */
	dhaddr->un.dhcsr = 0;
	asm("cmpl r10,$0x200;beql 1f;subl2 $4,r10;1:;");
}

dhslave(ui, reg, slaveno)
	struct uba_dinfo *ui;
	caddr_t reg;
{

	/* could fill in local tables for the dh here */
}

/*
 * Open a DH11 line.
 */
/*ARGSUSED*/
dhopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit, dh;
	register struct device *addr;
	register struct uba_dinfo *ui;
	int s;

	unit = minor(dev);
	dh = unit >> 4;
	if (unit >= NDH11*16 || (ui = dhinfo[dh])->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	tp = &dh11[unit];
	ui = dhinfo[dh];
	addr = (struct device *)ui->ui_addr;
	tp->t_addr = (caddr_t)addr;
	tp->t_oproc = dhstart;
	tp->t_iproc = NULL;
	tp->t_state |= WOPEN;
	s = spl6();
	if (dh_ubinfo[ui->ui_ubanum] == 0) {
		/* 512+ is a kludge to try to get around a hardware problem */
		dh_ubinfo[ui->ui_ubanum] =
		    uballoc(ui->ui_ubanum, (caddr_t)cfree,
			512+NCLIST*sizeof(struct cblock), 0);
		cbase[ui->ui_ubanum] = (short)dh_ubinfo[ui->ui_ubanum];
	}
	splx(s);
	addr->un.dhcsr |= IENAB;
	dhact |= (1<<dh);
	if ((tp->t_state&ISOPEN) == 0) {
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_ispeed = SSPEED;
			tp->t_ospeed = SSPEED;
			tp->t_flags = ODDP|EVENP|ECHO;
		}
		dhparam(unit);
	}
	if (tp->t_state&XCLUDE && u.u_uid!=0) {
		u.u_error = EBUSY;
		return;
	}
	dmopen(dev);
	(*linesw[tp->t_line].l_open)(dev, tp);
}

/*
 * Close a DH11 line.
 */
/*ARGSUSED*/
dhclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;
	register unit;

	unit = minor(dev);
	tp = &dh11[unit];
	(*linesw[tp->t_line].l_close)(tp);
	/*
	 * Turn of the break bit in case somebody did a TIOCSBRK without
	 * a TIOCCBRK.
	 */
	((struct device *)(tp->t_addr))->dhbreak &= ~(1<<(unit&017));
	if (tp->t_state&HUPCLS || (tp->t_state&ISOPEN)==0)
		dmctl(unit, TURNOFF, DMSET);
	ttyclose(tp);
}

/*
 * Read from a DH11 line.
 */
dhread(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &dh11[minor(dev)];
	(*linesw[tp->t_line].l_read)(tp);
}

/*
 * write on a DH11 line
 */
dhwrite(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &dh11[minor(dev)];
	(*linesw[tp->t_line].l_write)(tp);
}

/*
 * DH11 receiver interrupt.
 */
dhrint(dh)
	int dh;
{
	register struct tty *tp;
	register c;
	register struct device *addr;
	register struct tty *tp0;
	register struct uba_dinfo *ui;
	int s;

	s = spl6();	/* see comment in clock.c */
	ui = dhinfo[dh];
	addr = (struct device *)ui->ui_addr;
	tp0 = &dh11[dh*16];
	while ((c = addr->dhnxch) < 0) {	/* char. present */
		tp = tp0 + ((c>>8)&017);
		if (tp >= &dh11[NDH11*16])
			continue;
		if((tp->t_state&ISOPEN)==0) {
			wakeup((caddr_t)tp);
			continue;
		}
		if (c&PERROR)
			if ((tp->t_flags&(EVENP|ODDP))==EVENP
			 || (tp->t_flags&(EVENP|ODDP))==ODDP )
				continue;
		if (c&OVERRUN)
			printf("O");
		if (c&FRERROR)		/* break */
			if (tp->t_flags&RAW)
				c = 0;	/* null (for getty) */
			else
#ifdef IIASA
				continue;
#else
				c = tun.t_intrc;
#endif
		if (tp->t_line == NETLDISC) {
			c &= 0177;
			BKINPUT(c, tp);
		} else
			(*linesw[tp->t_line].l_rint)(c,tp);
	}
	splx(s);
}

/*
 * stty/gtty for DH11
 */
/*ARGSUSED*/
dhioctl(dev, cmd, addr, flag)
	caddr_t addr;
{
	register struct tty *tp;
	register unit = minor(dev);

	tp = &dh11[unit];
	cmd = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (cmd==0)
		return;
	if (ttioctl(tp, cmd, addr, flag)) {
		if (cmd==TIOCSETP||cmd==TIOCSETN)
			dhparam(unit);
	} else switch(cmd) {
	case TIOCSBRK:
		((struct device *)(tp->t_addr))->dhbreak |= 1<<(unit&017);
		break;
	case TIOCCBRK:
		((struct device *)(tp->t_addr))->dhbreak &= ~(1<<(unit&017));
		break;
	case TIOCSDTR:
		dmctl(unit, DTR|RQS, DMBIS);
		break;
	case TIOCCDTR:
		dmctl(unit, DTR|RQS, DMBIC);
		break;
	default:
		u.u_error = ENOTTY;
	}
}

/*
 * Set parameters from open or stty into the DH hardware
 * registers.
 */
dhparam(unit)
	register int unit;
{
	register struct tty *tp;
	register struct device *addr;
	register int lpar;
	int s;

	tp = &dh11[unit];
	addr = (struct device *)tp->t_addr;
	s = spl5();
	addr->un.dhcsrl = (unit&017) | IENAB;
	/*
	 * Hang up line?
	 */
	if ((tp->t_ispeed)==0) {
		tp->t_state |= HUPCLS;
		dmctl(unit, TURNOFF, DMSET);
		return;
	}
	lpar = ((tp->t_ospeed)<<10) | ((tp->t_ispeed)<<6);
	if ((tp->t_ispeed) == 4)		/* 134.5 baud */
		lpar |= BITS6|PENABLE|HDUPLX;
	else if ((tp->t_flags&RAW) || (tp->t_local&LLITOUT))
		lpar |= BITS8;
	else
		lpar |= BITS7|PENABLE;
	if ((tp->t_flags&EVENP) == 0)
		lpar |= OPAR;
	if ((tp->t_ospeed) == 3)	/* 110 baud */
		lpar |= TWOSB;
	addr->dhlpr = lpar;
	splx(s);
}

/*
 * DH11 transmitter interrupt.
 * Restart each line which used to be active but has
 * terminated transmission since the last interrupt.
 */
dhxint(dh)
	int dh;
{
	register struct tty *tp;
	register struct device *addr;
	short ttybit, bar, *sbar;
	register struct uba_dinfo *ui;
	register unit;
	int s;

	s = spl6();	/* block the clock */
	ui = dhinfo[dh];
	addr = (struct device *)ui->ui_addr;
	addr->un.dhcsr &= (short)~XINT;
	if (addr->un.dhcsr & NXM) {
		asm("halt");
		addr->un.dhcsr |= CLRNXM;
		printf("dh clr NXM\n");
	}
	sbar = &dhsar[dh];
	bar = *sbar & ~addr->dhbar;
	unit = dh * 16; ttybit = 1;
	for(; bar; unit++, ttybit <<= 1) {
		if(bar&ttybit) {
			*sbar &= ~ttybit;
			bar &= ~ttybit;
			tp = &dh11[unit];
			tp->t_state &= ~BUSY;
			if (tp->t_state&FLUSH)
				tp->t_state &= ~FLUSH;
			else {
				addr->un.dhcsrl = (unit&017)|IENAB;
				ndflush(&tp->t_outq,
				    (int)(short)addr->dhcar-
					UBACVT(tp->t_outq.c_cf,ui->ui_ubanum));
			}
			if (tp->t_line)
				(*linesw[tp->t_line].l_start)(tp);
			else
				dhstart(tp);
		}
	}
	splx(s);
}

/*
 * Start (restart) transmission on the given DH11 line.
 */
dhstart(tp)
	register struct tty *tp;
{
	register struct device *addr;
	register int nch, dh, unit;
	int s;

	/*
	 * If it's currently active, or delaying,
	 * no need to do anything.
	 */
	s = spl5();
	unit = minor(tp->t_dev);
	dh = unit >> 4;
	addr = (struct device *)tp->t_addr;
	if (tp->t_state&(TIMEOUT|BUSY|TTSTOP))
		goto out;
	if ((tp->t_state&ASLEEP) && tp->t_outq.c_cc<=TTLOWAT(tp)) {
		tp->t_state &= ~ASLEEP;
		if (tp->t_chan)
			mcstart(tp->t_chan, (caddr_t)&tp->t_outq);
		else
			wakeup((caddr_t)&tp->t_outq);
	}
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags & RAW)
		nch = ndqb(&tp->t_outq, 0);
	else {
		nch = ndqb(&tp->t_outq, 0200);
		if (nch == 0) {
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0177)+6);
			tp->t_state |= TIMEOUT;
			goto out;
		}
	}
	if (nch) {
		addr->un.dhcsrl = (unit&017)|IENAB;
		addr->dhcar = UBACVT(tp->t_outq.c_cf, 
		    dhinfo[dh]->ui_ubanum);
		addr->dhbcr = -nch;
		nch = 1<<(unit&017);
		addr->dhbar |= nch;
		dhsar[dh] |= nch;
		tp->t_state |= BUSY;
	}
out:
	splx(s);
}

/*
 * Stop output on a line.
 * Assume call is made at spl6.
 */
/*ARGSUSED*/
dhstop(tp, flag)
register struct tty *tp;
{
	register struct device *addr;
	register int unit, s;

	addr = (struct device *)tp->t_addr;
	s = spl6();
	if (tp->t_state & BUSY) {
		unit = minor(tp->t_dev);
		addr->un.dhcsrl = (unit&017) | IENAB;
		if ((tp->t_state&TTSTOP)==0)
			tp->t_state |= FLUSH;
		addr->dhbcr = -1;
	}
	splx(s);
}

int	dhsilo = 16;
/*
 * Silo control is fixed strategy
 * here, paralleling only option available
 * on DZ-11.
 */
/*ARGSUSED*/
dhtimer()
{
	register int dh;
	register struct device *addr;
	register struct uba_dinfo *ui;

	dh = 0;
	do {
		ui = dhinfo[dh];
		addr = (struct device *)ui->ui_addr;
		if (dhact & (1<<dh)) {
			if ((dhisilo & (1<<dh)) == 0) {
				addr->dhsilo = dhsilo;
				dhisilo |= 1<<dh;
			}
			dhrint(dh);
		}
		dh++;
	} while (dh < NDH11);
}

/*
 * Reset state of driver if UBA reset was necessary.
 * Reset the csrl and lpr registers on open lines, and
 * restart transmitters.
 */
dhreset(uban)
{
	register int dh, unit;
	register struct tty *tp;
	register struct uba_dinfo *ui;
	int i;

	if (dh_ubinfo[uban] == 0)
		return;
	printf(" dh");
	ubarelse(uban, &dh_ubinfo[uban]);
	dh_ubinfo[uban] = uballoc(uban, (caddr_t)cfree,
	    512+NCLIST*sizeof (struct cblock), 0);
	cbase[uban] = dh_ubinfo[uban]&0x3ffff;
	dhisilo = 0;		/* conservative */
	dh = 0;
	for (dh = 0; dh < NDH11; dh++) {
		ui = dhinfo[dh];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		((struct device *)ui->ui_addr)->un.dhcsr |= IENAB;
		unit = dh * 16;
		for (i = 0; i < 16; i++) {
			tp = &dh11[unit];
			if (tp->t_state & (ISOPEN|WOPEN)) {
				dhparam(unit);
				dmctl(unit, TURNON, DMSET);
				tp->t_state &= ~BUSY;
				dhstart(tp);
			}
			unit++;
		}
	}
	dhtimer();
}

#if DHDM
#include "../dev/dhdm.c"
#else
#include "../dev/dhfdm.c"
#endif
#endif
