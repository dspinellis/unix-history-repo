/*	dh.c	4.15	81/02/16	*/

#include "dh.h"
#if NDH11 > 0
#define	DELAY(i)	{ register int j = i; while (--j > 0); }
/*
 * DH-11 driver
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

#define	UBACVT(x,uban) (cbase[uban] + ((x)-(char *)cfree))

int	dhcntrlr(), dhslave(), dhrint(), dhxint();
struct	uba_dinfo *dhinfo[NDH11];
u_short	dhstd[] = { 0 };
struct	uba_driver dhdriver =
	{ dhcntrlr, dhslave, 0, 0, dhstd, "dh", dhinfo };

struct	tty dh11[NDH11*16];
int	dhact;
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

/* Bits in dhcsr */
#define	DH_TI	0100000		/* transmit interrupt */
#define	DH_SI	0040000		/* storage interrupt */
#define	DH_TIE	0020000		/* transmit interrupt enable */
#define	DH_SIE	0010000		/* storage interrupt enable */
#define	DH_MC	0004000		/* master clear */
#define	DH_NXM	0002000		/* non-existant memory */
#define	DH_MM	0001000		/* maintenance mode */
#define	DH_CNI	0000400		/* clear non-existant memory interrupt */
#define	DH_RI	0000200		/* receiver interrupt */
#define	DH_RIE	0000100		/* receiver interrupt enable */

#define	DH_IE	(DH_TIE|DH_SIE|DH_RIE)

/* Bits in dhrcr */
#define	DH_PE	010000		/* parity error */
#define	DH_FE	020000		/* framing error */
#define	DH_DO	040000		/* data overrun */

/*
 * DM control bits
 */
#define	DM_ON	03	/* CD lead + line enable */
#define	DM_OFF	01	/* line enable */
#define	DM_DTR	02	/* data terminal ready */
#define	DM_RQS	04	/* request to send */

/*
 * Software copy of last dhbar
 */
short	dhsar[NDH11];

struct device
{
	union {
		short	dhcsr;		/* control-status register */
		char	dhcsrl;		/* low byte for line select */
	} un;
	short	dhrcr;			/* receive character register */
	short	dhlpr;			/* line parameter register */
	u_short dhcar;			/* current address register */
	short	dhbcr;			/* byte count register */
	u_short	dhbar;			/* buffer active register */
	short	dhbreak;		/* break control register */
	short	dhsilo;			/* silo status register */
};

/*
 * Routine for configuration to force a dh to interrupt.
 * Set to transmit at 9600 baud, and cause a transmitter interrupt.
 */
dhcntrlr(ui, reg)
	struct uba_dinfo *ui;
	caddr_t reg;
{
	register int br, cvec;
	register struct device *dhaddr = (struct device *)reg;
	int i;

	dhaddr->un.dhcsr = DH_TIE;
	DELAY(5);
	dhaddr->dhlpr = (B9600 << 10) | (B9600 << 6) | BITS7|PENABLE;
	dhaddr->dhbcr = -1;
	dhaddr->dhcar = 0;
	dhaddr->dhbar = 1;
	DELAY(100000);		/* wait 1/10'th of a sec for interrupt */
	dhaddr->un.dhcsr = 0;
	if (cvec && cvec != 0x200)
		cvec -= 4;		/* transmit -> receive */
	return (1);
}

/*
 * Routine called to init slave tables.
 */
dhslave(ui, reg, slaveno)
	struct uba_dinfo *ui;
	caddr_t reg;
{

	/* no tables to set up */
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
		cbase[ui->ui_ubanum] = dh_ubinfo[ui->ui_ubanum]&0x3ffff;
	}
	if ((dhact&(1<<dh)) == 0) {
		addr->un.dhcsr |= DH_IE;
		addr->dhsilo = 16;
		dhact |= (1<<dh);
	}
	splx(s);
	if ((tp->t_state&ISOPEN) == 0) {
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_ispeed = B300;
			tp->t_ospeed = B300;
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
	((struct device *)(tp->t_addr))->dhbreak &= ~(1<<(unit&017));
	if (tp->t_state&HUPCLS || (tp->t_state&ISOPEN)==0)
		dmctl(unit, DM_OFF, DMSET);
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

	ui = dhinfo[dh];
	if (ui == 0) {
		printf("stray dhrint %d\n", dh);
		asm("halt");
		return;
	}
	addr = (struct device *)ui->ui_addr;
	tp0 = &dh11[dh*16];
	while ((c = addr->dhrcr) < 0) {	/* char. present */
		tp = tp0 + ((c>>8)&017);
		if (tp >= &dh11[NDH11*16])
			continue;
		if((tp->t_state&ISOPEN)==0) {
			wakeup((caddr_t)tp);
			continue;
		}
		if (c&DH_PE)
			if ((tp->t_flags&(EVENP|ODDP))==EVENP
			 || (tp->t_flags&(EVENP|ODDP))==ODDP )
				continue;
		if (c&DH_DO)
			printf("O");
		if (c&DH_FE)		/* break */
			if (tp->t_flags&RAW)
				c = 0;	/* null (for getty) */
			else
				c = tun.t_intrc;
		if (tp->t_line == NETLDISC) {
			c &= 0177;
			BKINPUT(c, tp);
		} else
			(*linesw[tp->t_line].l_rint)(c,tp);
	}
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
		dmctl(unit, DM_DTR|DM_RQS, DMBIS);
		break;
	case TIOCCDTR:
		dmctl(unit, DM_DTR|DM_RQS, DMBIC);
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
	addr->un.dhcsrl = (unit&017) | DH_IE;
	if ((tp->t_ispeed)==0) {
		tp->t_state |= HUPCLS;
		dmctl(unit, DM_OFF, DMSET);
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

	ui = dhinfo[dh];
	addr = (struct device *)ui->ui_addr;
	if (addr->un.dhcsr & DH_NXM) {
		addr->un.dhcsr |= DH_CNI;
		printf("dh%d NXM\n", ui->ui_ctlr);
	}
	addr->un.dhcsr &= (short)~DH_TI;
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
				addr->un.dhcsrl = (unit&017)|DH_IE;
				ndflush(&tp->t_outq,
				    /* SHOULD PASTE ON 16&17 BITS HERE */
				    addr->dhcar-
					UBACVT(tp->t_outq.c_cf,ui->ui_ubanum));
			}
			if (tp->t_line)
				(*linesw[tp->t_line].l_start)(tp);
			else
				dhstart(tp);
		}
	}
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
		/* SHOULD PASTE ON BITS 16&17 HERE */
		addr->un.dhcsrl = (unit&017)|DH_IE;
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
		addr->un.dhcsrl = (unit&017) | DH_IE;
		if ((tp->t_state&TTSTOP)==0)
			tp->t_state |= FLUSH;
		addr->dhbcr = -1;
	}
	splx(s);
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
	dh = 0;
	for (dh = 0; dh < NDH11; dh++) {
		ui = dhinfo[dh];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		((struct device *)ui->ui_addr)->un.dhcsr |= DH_IE;
		((struct device *)ui->ui_addr)->dhsilo = 16;
		unit = dh * 16;
		for (i = 0; i < 16; i++) {
			tp = &dh11[unit];
			if (tp->t_state & (ISOPEN|WOPEN)) {
				dhparam(unit);
				dmctl(unit, DM_ON, DMSET);
				tp->t_state &= ~BUSY;
				dhstart(tp);
			}
			unit++;
		}
	}
	dhtimer();
}

dhtimer()
{
	register int dh;

	for (dh = 0; dh < NDH11; dh++)
		dhrint(dh);
}

#if DHDM
#include "../dev/dhdm.c"
#else
#include "../dev/dhfdm.c"
#endif
#endif
