/*	dh.c	4.16	81/02/17	*/

#include "dh.h"
#if NDH11 > 0
#define	DELAY(i)	{ register int j = i; while (--j > 0); }
/*
 * DH-11 driver
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
#include "../h/file.h"

#define	UBACVT(x, uban)		(cbase[uban] + ((x)-(char *)cfree))

/*
 * Definition of the controller for the auto-configuration program.
 */
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

/* Bits in dhlpr */
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

/* Software copy of last dhbar */
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
/*ARGSUSED*/
dhcntrlr(ui, reg)
	struct uba_dinfo *ui;
	caddr_t reg;
{
	register int br, cvec;		/* these are ``value-result'' */
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
 * Open a DH11 line, mapping the clist onto the uba if this
 * is the first dh on this uba.  Turn on this dh if this is
 * the first use of it.  Also do a dmopen to wait for carrier.
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
	if (unit >= NDH11*16 || (ui = dhinfo[dh])== 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	tp = &dh11[unit];
	if (tp->t_state&XCLUDE && u.u_uid!=0) {
		u.u_error = EBUSY;
		return;
	}
	addr = (struct device *)ui->ui_addr;
	tp->t_addr = (caddr_t)addr;
	tp->t_oproc = dhstart;
	tp->t_iproc = NULL;
	tp->t_state |= WOPEN;
	/*
	 * While setting up state for this uba and this dh,
	 * block uba resets which can clear the state.
	 */
	s = spl5();
	if (dh_ubinfo[ui->ui_ubanum] == 0) {
		/* 512+ is a kludge to try to get around a hardware problem */
		dh_ubinfo[ui->ui_ubanum] =
		    uballoc(ui->ui_ubanum, (caddr_t)cfree,
			512+NCLIST*sizeof(struct cblock), 0);
		cbase[ui->ui_ubanum] = dh_ubinfo[ui->ui_ubanum]&0x3ffff;
	}
	if ((dhact&(1<<dh)) == 0) {
		addr->un.dhcsr |= DH_IE;
		DELAY(5);
		dhact |= (1<<dh);
		addr->dhsilo = 16;
	}
	splx(s);
	/*
	 * If this is first open, initialze tty state to default.
	 */
	if ((tp->t_state&ISOPEN) == 0) {
		ttychars(tp);
		if (tp->t_ispeed == 0) {
			tp->t_ispeed = B300;
			tp->t_ospeed = B300;
			tp->t_flags = ODDP|EVENP|ECHO;
		}
		dhparam(unit);
	}
	/*
	 * Wait for carrier, then process line discipline specific open.
	 */
	dmopen(dev);
	(*linesw[tp->t_line].l_open)(dev, tp);
}

/*
 * Close a DH11 line, turning off the DM11.
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

dhread(dev)
	dev_t dev;
{
	register struct tty *tp;

	tp = &dh11[minor(dev)];
	(*linesw[tp->t_line].l_read)(tp);
}

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
	addr = (struct device *)ui->ui_addr;
	tp0 = &dh11[dh<<4];
	/*
	 * Loop fetching characters from the silo for this
	 * dh until there are no more in the silo.
	 */
	while ((c = addr->dhrcr) < 0) {
		tp = tp0 + ((c>>8)&0xf);
		if ((tp->t_state&ISOPEN)==0) {
			wakeup((caddr_t)tp);
			continue;
		}
		if (c & DH_PE)
			if ((tp->t_flags&(EVENP|ODDP))==EVENP
			 || (tp->t_flags&(EVENP|ODDP))==ODDP )
				continue;
		if (c & DH_DO)
			printf("O");
		if (c & DH_FE)
			/*
			 * At framing error (break) generate
			 * a null (in raw mode, for getty), or a
			 * interrupt (in cooked/cbreak mode).
			 */
			if (tp->t_flags&RAW)
				c = 0;
			else
				c = tun.t_intrc;
		if (tp->t_line == NETLDISC) {
			c &= 0177;
			BKINPUT(c, tp);
		} else
			(*linesw[tp->t_line].l_rint)(c, tp);
	}
}

/*
 * Ioctl for DH11.
 */
/*ARGSUSED*/
dhioctl(dev, cmd, addr, flag)
	caddr_t addr;
{
	register struct tty *tp;
	register unit = minor(dev);

	tp = &dh11[unit];
	cmd = (*linesw[tp->t_line].l_ioctl)(tp, cmd, addr);
	if (cmd == 0)
		return;
	if (ttioctl(tp, cmd, addr, flag)) {
		if (cmd==TIOCSETP || cmd==TIOCSETN)
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
	/*
	 * Block interrupts so parameters will be set
	 * before the line interrupts.
	 */
	s = spl5();
	addr->un.dhcsrl = (unit&0xf) | DH_IE;
	if ((tp->t_ispeed)==0) {
		tp->t_state |= HUPCLS;
		dmctl(unit, DM_OFF, DMSET);
		return;
	}
	lpar = ((tp->t_ospeed)<<10) | ((tp->t_ispeed)<<6);
	if ((tp->t_ispeed) == B134)
		lpar |= BITS6|PENABLE|HDUPLX;
	else if ((tp->t_flags&RAW) || (tp->t_local&LLITOUT))
		lpar |= BITS8;
	else
		lpar |= BITS7|PENABLE;
	if ((tp->t_flags&EVENP) == 0)
		lpar |= OPAR;
	if ((tp->t_ospeed) == B110)
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
	register int unit;
	int s;
	u_short cnt;

	ui = dhinfo[dh];
	addr = (struct device *)ui->ui_addr;
	if (addr->un.dhcsr & DH_NXM) {
		DELAY(5);
		addr->un.dhcsr |= DH_CNI;
		printf("dh%d NXM\n", dh);
	}
	sbar = &dhsar[dh];
	bar = *sbar & ~addr->dhbar;
	unit = dh * 16; ttybit = 1;
	addr->un.dhcsr &= (short)~DH_TI;
	for (; bar; unit++, ttybit <<= 1) {
		if (bar & ttybit) {
			*sbar &= ~ttybit;
			bar &= ~ttybit;
			tp = &dh11[unit];
			tp->t_state &= ~BUSY;
			if (tp->t_state&FLUSH)
				tp->t_state &= ~FLUSH;
			else {
				addr->un.dhcsrl = (unit&017)|DH_IE;
				DELAY(5);
				/*
				 * Do arithmetic in a short to make up
				 * for lost 16&17 bits.
				 */
				cnt = addr->dhcar -
				    UBACVT(tp->t_outq.c_cf, ui->ui_ubanum);
				ndflush(&tp->t_outq, cnt);
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
	register int car, dh, unit, nch;
	int s;

	unit = minor(tp->t_dev);
	dh = unit >> 4;
	unit &= 0xf;
	addr = (struct device *)tp->t_addr;

	/*
	 * Must hold interrupts in following code to prevent
	 * state of the tp from changing.
	 */
	s = spl5();
	/*
	 * If it's currently active, or delaying, no need to do anything.
	 */
	if (tp->t_state&(TIMEOUT|BUSY|TTSTOP))
		goto out;
	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, wake up the sleepers.
	 */
	if ((tp->t_state&ASLEEP) && tp->t_outq.c_cc<=TTLOWAT(tp)) {
		tp->t_state &= ~ASLEEP;
		if (tp->t_chan)
			mcstart(tp->t_chan, (caddr_t)&tp->t_outq);
		else
			wakeup((caddr_t)&tp->t_outq);
	}
	/*
	 * Now restart transmission unless the output queue is
	 * empty.
	 */
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (tp->t_flags & RAW)
		nch = ndqb(&tp->t_outq, 0);
	else {
		nch = ndqb(&tp->t_outq, 0200);
		/*
		 * If first thing on queue is a delay process it.
		 */
		if (nch == 0) {
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0x7f)+6);
			tp->t_state |= TIMEOUT;
			goto out;
		}
	}
	/*
	 * If characters to transmit, restart transmission.
	 */
	if (nch) {
		car = UBACVT(tp->t_outq.c_cf, dhinfo[dh]->ui_ubanum);
		addr->un.dhcsrl = unit|((car>>12)&0x30)|DH_IE;
		DELAY(5);
		unit = 1 << unit;
		dhsar[dh] |= unit;
		addr->dhcar = car;
		addr->dhbcr = -nch;
		addr->dhbar |= unit;
		tp->t_state |= BUSY;
	}
out:
	splx(s);
}

/*
 * Stop output on a line, e.g. for ^S/^Q or output flush.
 */
/*ARGSUSED*/
dhstop(tp, flag)
	register struct tty *tp;
{
	register struct device *addr;
	register int unit, s;

	addr = (struct device *)tp->t_addr;
	/*
	 * Block input/output interrupts while messing with state.
	 */
	s = spl5();
	if (tp->t_state & BUSY) {
		/*
		 * Device is transmitting; stop output
		 * by selecting the line and setting the byte
		 * count to -1.  We will clean up later
		 * by examining the address where the dh stopped.
		 */
		unit = minor(tp->t_dev);
		addr->un.dhcsrl = (unit&017) | DH_IE;
		DELAY(5);
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
	int uban;
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
		DELAY(5);
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

/*
 * At software clock interrupt time or after a UNIBUS reset
 * empty all the dh silos.
 */
dhtimer()
{
	register int dh;

	for (dh = 0; dh < NDH11; dh++)
		dhrint(dh);
}

/*
 * DM-11 driver.
 */

/*
 * Definition of the controller for the auto-configuration program.
 */
int	dmcntrlr(), dmslave(), dmintr();
struct	uba_dinfo *dminfo[NDH11];
u_short	dmstd[] = { 0 };
struct	uba_driver dmdriver =
	{ dmcntrlr, dmslave, 0, 0, dmstd, "dm", dminfo };

/* hardware bits */
#define	DM_CARRTRANS	040000		/* carrier transition */
#define	DM_CLSCAN	004000		/* clear scan */
#define	DM_DONE		000200
#define	DM_CARRON	000100		/* carrier on */
#define	DM_SCENABLE	000040		/* scan enable */
#define	DM_SCBUSY	000020		/* scan busy */

struct dmdevice
{
	short	dmcsr;
	short	dmlstat;
	short	dmpad1[2];
};

dmcntrlr(um, addr)
	struct uba_minfo *um;
	caddr_t addr;
{

}

dmslave()
{

}

/*
 * Turn on the line associated with the dh device dev.
 */
dmopen(dev)
	dev_t dev;
{
	register struct tty *tp;
	register struct dmdevice *addr;
	register struct uba_dinfo *ui;
	register int unit;
	register int dm;

	unit = minor(dev);
	dm = unit >> 8;
	tp = &dh11[unit];
	if (dm >= NDH11 || (ui = dminfo[dm]) == 0 || ui->ui_alive == 0) {
		tp->t_state |= CARR_ON;
		return;
	}
	addr = (struct dmdevice *)ui->ui_addr;
	spl5();
	addr->dmcsr &= ~DM_SCENABLE;
	while (addr->dmcsr & DM_SCBUSY)
		;
	addr->dmcsr = unit & 0xf;
	addr->dmlstat = DM_ON;
	if (addr->dmlstat&DM_CARRON)
		tp->t_state |= CARR_ON;
	addr->dmcsr = DH_IE|DM_SCENABLE;
	while ((tp->t_state&CARR_ON)==0)
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	spl0();
}

/*
 * Dump control bits into the DM registers.
 */
dmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct uba_dinfo *ui;
	register struct dmdevice *addr;
	register int unit, s;
	int dm;

	unit = minor(dev);
	dm = unit >> 4;
	if ((ui = dminfo[dm]) == 0 || ui->ui_alive == 0)
		return;
	addr = (struct dmdevice *)ui->ui_addr;
	s = spl5();
	addr->dmcsr &= ~DM_SCENABLE;
	while (addr->dmcsr & DM_SCBUSY)
		;
	addr->dmcsr = unit & 0xf;
	switch(how) {
	case DMSET:
		addr->dmlstat = bits;
		break;
	case DMBIS:
		addr->dmlstat |= bits;
		break;
	case DMBIC:
		addr->dmlstat &= ~bits;
		break;
	}
	addr->dmcsr = DH_IE|DM_SCENABLE;
	splx(s);
}

/*
 * DM11 interrupt; deal with carrier transitions.
 */
dmintr(dm)
	register int dm;
{
	register struct uba_dinfo *ui;
	register struct tty *tp;
	register struct dmdevice *addr;

	ui = dminfo[dm];
	addr = (struct dmdevice *)ui->ui_addr;
	if (addr->dmcsr&DM_DONE && addr->dmcsr&DM_CARRTRANS) {
		tp = &dh11[(dm<<4)+(addr->dmcsr&0xf)];
		wakeup((caddr_t)&tp->t_rawq);
		if ((tp->t_state&WOPEN)==0 &&
		    (tp->t_local&LMDMBUF)) {
			if (addr->dmlstat & DM_CARRON) {
				tp->t_state &= ~TTSTOP;
				ttstart(tp);
			} else if ((tp->t_state&TTSTOP) == 0) {
				tp->t_state |= TTSTOP;
				dhstop(tp, 0);
			}
		} else if ((addr->dmlstat&DM_CARRON)==0) {
			if ((tp->t_state&WOPEN)==0 &&
			    (tp->t_local&LNOHANG)==0) {
				gsignal(tp->t_pgrp, SIGHUP);
				gsignal(tp->t_pgrp, SIGCONT);
				addr->dmlstat = 0;
				flushtty(tp, FREAD|FWRITE);
			}
			tp->t_state &= ~CARR_ON;
		} else
			tp->t_state |= CARR_ON;
		addr->dmcsr = DH_IE|DM_SCENABLE;
	}
}
