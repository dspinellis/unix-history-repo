/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dh.c	7.14 (Berkeley) 6/28/90
 */

#include "dh.h"
#if NDH > 0
/*
 * DH-11/DM-11 driver
 */
#include "machine/pte.h"

#include "uba.h"
#include "param.h"
#include "conf.h"
#include "user.h"
#include "proc.h"
#include "ioctl.h"
#include "tty.h"
#include "map.h"
#include "buf.h"
#include "vm.h"
#include "kernel.h"
#include "syslog.h"

#include "ubareg.h"
#include "ubavar.h"
#include "dhreg.h"
#include "dmreg.h"

#include "bkmac.h"
#include "clist.h"
#include "file.h"
#include "uio.h"

/*
 * Definition of the driver for the auto-configuration program.
 * There is one definition for the dh and one for the dm.
 */
int	dhprobe(), dhattach(), dhrint(), dhxint(), dhtimer();
struct	uba_device *dhinfo[NDH];
u_short	dhstd[] = { 0 };
struct	uba_driver dhdriver =
	{ dhprobe, 0, dhattach, 0, dhstd, "dh", dhinfo };

int	dmprobe(), dmattach(), dmintr();
struct	uba_device *dminfo[NDH];
u_short	dmstd[] = { 0 };
struct	uba_driver dmdriver =
	{ dmprobe, 0, dmattach, 0, dmstd, "dm", dminfo };

#ifndef	PORTSELECTOR
#define	ISPEED	TTYDEF_SPEED
#define	LFLAG	TTYDEF_LFLAG
#else
#define	ISPEED	B4800
#define	LFLAG	(TTYDEF_LFLAG&~ECHO)
#endif

#define	FASTTIMER	(hz/30)		/* scan rate with silos on */

/*
 * Local variables for the driver
 */
short	dhsar[NDH];			/* software copy of last bar */
short	dhsoftCAR[NDH];

struct	tty dh11[NDH*16];
int	ndh11	= NDH*16;
int	dhact;				/* mask of active dh's */
int	dhsilos;			/* mask of dh's with silo in use */
int	dhchars[NDH];			/* recent input count */
int	dhrate[NDH];			/* smoothed input count */
int	dhhighrate = 100;		/* silo on if dhchars > dhhighrate */
int	dhlowrate = 75;			/* silo off if dhrate < dhlowrate */
static short timerstarted;
int	dhstart(), ttrstrt();

struct speedtab dhspeedtab[] = {
	19200,	14,
	9600,	13,
	4800,	12,
	2400,	11,
	1800,	10,
	1200,	9,
	600,	8,
	300,	7,
	200,	6,
	150,	5,
	134,	4,
	110,	3,
	75,	2,
	50,	1,
	0,	0,
	EXTA,	14,
	EXTB,	15,
	-1,	-1
};

/*
 * The clist space is mapped by one terminal driver onto each UNIBUS.
 * The identity of the board which allocated resources is recorded,
 * so the process may be repeated after UNIBUS resets.
 * The UBACVT macro converts a clist space address for unibus uban
 * into an i/o space address for the DMA routine.
 */
int	dh_uballoc[NUBA];	/* which dh (if any) allocated unibus map */
int	cbase[NUBA];		/* base address of clists in unibus map */
#define	UBACVT(x, uban)		(cbase[uban] + ((x)-(char *)cfree))

/*
 * Routine for configuration to force a dh to interrupt.
 * Set to transmit at 9600 baud, and cause a transmitter interrupt.
 */
/*ARGSUSED*/
dhprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* these are ``value-result'' */
	register struct dhdevice *dhaddr = (struct dhdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	if (ndh11 == 0) ndh11 = 1;
	dhrint(0); dhxint(0);
#endif
#ifndef notdef
	dhaddr->un.dhcsr = DH_RIE|DH_MM|DH_RI;
	DELAY(1000);
	dhaddr->un.dhcsr &= ~DH_RI;
	dhaddr->un.dhcsr = 0;
#else
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
#endif
	return (sizeof (struct dhdevice));
}

/*
 * Routine called to attach a dh.
 */
dhattach(ui)
	struct uba_device *ui;
{

	dhsoftCAR[ui->ui_unit] = ui->ui_flags;
	cbase[ui->ui_ubanum] = -1;
	dh_uballoc[ui->ui_ubanum] = -1;
}

/*
 * Configuration routine to cause a dm to interrupt.
 */
dmprobe(reg)
	caddr_t reg;
{
	register int br, vec;			/* value-result */
	register struct dmdevice *dmaddr = (struct dmdevice *)reg;

#ifdef lint
	br = 0; vec = br; br = vec;
	dmintr(0);
#endif
	dmaddr->dmcsr = DM_DONE|DM_IE;
	DELAY(20);
	dmaddr->dmcsr = 0;
	return (1);
}

/*ARGSUSED*/
dmattach(ui)
	struct uba_device *ui;
{

	/* no local state to set up */
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
	register struct dhdevice *addr;
	register struct uba_device *ui;
	int s, error;
	int dhparam();

	unit = minor(dev);
	dh = unit >> 4;
	if (unit >= NDH*16 || (ui = dhinfo[dh])== 0 || ui->ui_alive == 0)
		return (ENXIO);
	tp = &dh11[unit];
	if (tp->t_state&TS_XCLUDE && u.u_uid!=0)
		return (EBUSY);
	addr = (struct dhdevice *)ui->ui_addr;
	tp->t_addr = (caddr_t)addr;
	tp->t_oproc = dhstart;
	tp->t_param = dhparam;
	tp->t_dev = dev;
	/*
	 * While setting up state for this uba and this dh,
	 * block uba resets which can clear the state.
	 */
	s = spl5();
	if (cbase[ui->ui_ubanum] == -1) {
		dh_uballoc[ui->ui_ubanum] = dh;
		cbase[ui->ui_ubanum] = UBAI_ADDR(uballoc(ui->ui_ubanum,
		    (caddr_t)cfree, nclist*sizeof(struct cblock), 0));
	}
	if (timerstarted == 0) {
		timerstarted++;
		timeout(dhtimer, (caddr_t) 0, hz);
	}
	if ((dhact&(1<<dh)) == 0) {
		addr->un.dhcsr |= DH_IE;
		dhact |= (1<<dh);
		addr->dhsilo = 0;
	}
	splx(s);
	/*
	 * If this is first open, initialize tty state to default.
	 */
	if ((tp->t_state&TS_ISOPEN) == 0) {
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
		dhparam(tp, &tp->t_termios);
		ttsetwater(tp);
	}
	/*
	 * Wait for carrier, then process line discipline specific open.
	 */
	if (error = dmopen(dev, flag))
		return (error);
	return ((*linesw[tp->t_line].l_open)(dev, tp));
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
	((struct dhdevice *)(tp->t_addr))->dhbreak &= ~(1<<(unit&017));
	if (tp->t_cflag&HUPCL || (tp->t_state&TS_ISOPEN)==0)
		dmctl(unit, DML_OFF, DMSET);
	return (ttyclose(tp));
}

dhread(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &dh11[minor(dev)];

	return ((*linesw[tp->t_line].l_read)(tp, uio, flag));
}

dhwrite(dev, uio, flag)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp = &dh11[minor(dev)];

	return ((*linesw[tp->t_line].l_write)(tp, uio, flag));
}

/*
 * DH11 receiver interrupt.
 */
dhrint(dh)
	int dh;
{
	register struct tty *tp;
	register c, cc;
	register struct dhdevice *addr;
	register struct tty *tp0;
	register struct uba_device *ui;
	int overrun = 0;

	ui = dhinfo[dh];
	if (ui == 0 || ui->ui_alive == 0)
		return;
	addr = (struct dhdevice *)ui->ui_addr;
	tp0 = &dh11[dh<<4];
	/*
	 * Loop fetching characters from the silo for this
	 * dh until there are no more in the silo.
	 */
	while ((c = addr->dhrcr) < 0) {
		tp = tp0 + ((c>>8)&0xf);
		dhchars[dh]++;
		if ((tp->t_state&TS_ISOPEN)==0) {
			wakeup((caddr_t)&tp->t_rawq);
#ifdef PORTSELECTOR
			if ((tp->t_state&TS_WOPEN) == 0)
#endif
				continue;
		}
		cc = c&0xff;
		if (c&DH_PE)
			cc |= TTY_PE;
		if ((c&DH_DO) && overrun == 0) {
			log(LOG_WARNING, "dh%d: silo overflow\n", dh);
			overrun = 1;
		}
		if (c&DH_FE)
			cc |= TTY_FE;
		(*linesw[tp->t_line].l_rint)(cc, tp);
	}
}

/*
 * Ioctl for DH11.
 */
/*ARGSUSED*/
dhioctl(dev, cmd, data, flag)
	caddr_t data;
{
	register struct tty *tp;
	register int unit = minor(dev);
	int error;

	tp = &dh11[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	switch (cmd) {

	case TIOCSBRK:
		((struct dhdevice *)(tp->t_addr))->dhbreak |= 1<<(unit&017);
		break;

	case TIOCCBRK:
		((struct dhdevice *)(tp->t_addr))->dhbreak &= ~(1<<(unit&017));
		break;

	case TIOCSDTR:
		dmctl(unit, DML_DTR|DML_RTS, DMBIS);
		break;

	case TIOCCDTR:
		dmctl(unit, DML_DTR|DML_RTS, DMBIC);
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

/*
 * Set parameters from open or stty into the DH hardware
 * registers.
 */
dhparam(tp, t)
	register struct tty *tp;
	register struct termios *t;
{
	register struct dhdevice *addr;
	register int lpar;
	register int cflag = t->c_cflag;
	int unit = minor(tp->t_dev);
	int s;
	int ispeed = ttspeedtab(t->c_ispeed, dhspeedtab);
	int ospeed = ttspeedtab(t->c_ospeed, dhspeedtab);

	/* check requested parameters */
	if (ospeed < 0 || ispeed < 0 || (cflag&CSIZE) == CS5)
		return(EINVAL);
	if (ispeed == 0)
		ispeed = ospeed;
	/* and copy to tty */
	tp->t_ispeed = t->c_ispeed;
	tp->t_ospeed = t->c_ospeed;
	tp->t_cflag = cflag;
	/*
	 * Block interrupts so parameters will be set
	 * before the line interrupts.
	 */
	addr = (struct dhdevice *)tp->t_addr;
	s = spl5();
	addr->un.dhcsrl = (unit&0xf) | DH_IE;
	if (ospeed == 0) {
		tp->t_cflag |= HUPCL;
		dmctl(unit, DML_OFF, DMSET);
		splx(s);
		return 0;
	}
	lpar = (ospeed<<10) | (ispeed<<6);
	switch (cflag&CSIZE) {
	case CS6:	lpar |= BITS6; break;
	case CS7:	lpar |= BITS7; break;
	case CS8:	lpar |= BITS8; break;
	}
	if (cflag&PARENB)
		lpar |= PENABLE;
	if (cflag&PARODD)
		lpar |= OPAR;
	if (cflag&CSTOPB)
		lpar |= TWOSB;
	addr->dhlpr = lpar;
	splx(s);
	return 0;
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
	register struct dhdevice *addr;
	short ttybit, bar, *sbar;
	register struct uba_device *ui;
	register int unit;
	u_short cntr;

	ui = dhinfo[dh];
	addr = (struct dhdevice *)ui->ui_addr;
	if (addr->un.dhcsr & DH_NXM) {
		addr->un.dhcsr |= DH_CNI;
		printf("dh%d: NXM\n", dh);
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
			tp->t_state &= ~TS_BUSY;
			if (tp->t_state&TS_FLUSH)
				tp->t_state &= ~TS_FLUSH;
			else {
				addr->un.dhcsrl = (unit&017)|DH_IE;
				/*
				 * Do arithmetic in a short to make up
				 * for lost 16&17 bits.
				 */
				cntr = addr->dhcar -
				    UBACVT(tp->t_outq.c_cf, ui->ui_ubanum);
				ndflush(&tp->t_outq, (int)cntr);
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
	register struct dhdevice *addr;
	register int car, dh, unit, nch;
	int s;

	unit = minor(tp->t_dev);
	dh = unit >> 4;
	unit &= 0xf;
	addr = (struct dhdevice *)tp->t_addr;

	/*
	 * Must hold interrupts in following code to prevent
	 * state of the tp from changing.
	 */
	s = spl5();
	/*
	 * If it's currently active, or delaying, no need to do anything.
	 */
	if (tp->t_state&(TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, wake up the sleepers.
	 */
	if (tp->t_outq.c_cc<=tp->t_lowat) {
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
	/*
	 * Now restart transmission unless the output queue is
	 * empty.
	 */
	if (tp->t_outq.c_cc == 0)
		goto out;
	if (1 || !(tp->t_oflag&OPOST))	/*XXX*/
		nch = ndqb(&tp->t_outq, 0);
	else {
		nch = ndqb(&tp->t_outq, 0200);
		/*
		 * If first thing on queue is a delay process it.
		 */
		if (nch == 0) {
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0x7f)+6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	/*
	 * If characters to transmit, restart transmission.
	 */
	if (nch) {
		car = UBACVT(tp->t_outq.c_cf, dhinfo[dh]->ui_ubanum);
		addr->un.dhcsrl = unit|((car>>12)&0x30)|DH_IE;
		/*
		 * The following nonsense with short word
		 * is to make sure the dhbar |= word below
		 * is done with an interlocking bisw2 instruction.
		 */
		{ short word = 1 << unit;
		dhsar[dh] |= word;
		addr->dhcar = car;
		addr->dhbcr = -nch;
		addr->dhbar |= word;
		}
		tp->t_state |= TS_BUSY;
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
	register struct dhdevice *addr;
	register int unit, s;

	addr = (struct dhdevice *)tp->t_addr;
	/*
	 * Block input/output interrupts while messing with state.
	 */
	s = spl5();
	if (tp->t_state & TS_BUSY) {
		/*
		 * Device is transmitting; stop output
		 * by selecting the line and setting the byte
		 * count to -1.  We will clean up later
		 * by examining the address where the dh stopped.
		 */
		unit = minor(tp->t_dev);
		addr->un.dhcsrl = (unit&017) | DH_IE;
		if ((tp->t_state&TS_TTSTOP)==0)
			tp->t_state |= TS_FLUSH;
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
	register struct uba_device *ui;
	int i;

	dh = 0;
	for (dh = 0; dh < NDH; dh++) {
		ui = dhinfo[dh];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		printf(" dh%d", dh);
		if (dh_uballoc[uban] == dh) {
			int info;

			info = uballoc(uban, (caddr_t)cfree,
			    nclist * sizeof(struct cblock), UBA_CANTWAIT);
			if (info)
				cbase[uban] = UBAI_ADDR(info);
			else {
				printf(" [can't get uba map]");
				cbase[uban] = -1;
			}
		}
		((struct dhdevice *)ui->ui_addr)->un.dhcsr |= DH_IE;
		((struct dhdevice *)ui->ui_addr)->dhsilo = 0;
		unit = dh * 16;
		for (i = 0; i < 16; i++) {
			tp = &dh11[unit];
			if (tp->t_state & (TS_ISOPEN|TS_WOPEN)) {
				dhparam(unit);
				dmctl(unit, DML_ON, DMSET);
				tp->t_state &= ~TS_BUSY;
				dhstart(tp);
			}
			unit++;
		}
	}
	dhsilos = 0;
}

int dhtransitions, dhslowtimers, dhfasttimers;		/*DEBUG*/
/*
 * At software clock interrupt time, check status.
 * Empty all the dh silos that are in use, and decide whether
 * to turn any silos off or on.
 */
dhtimer()
{
	register int dh, s;
	static int timercalls;

	if (dhsilos) {
		dhfasttimers++;		/*DEBUG*/
		timercalls++;
		s = spl5();
		for (dh = 0; dh < NDH; dh++)
			if (dhsilos & (1 << dh))
				dhrint(dh);
		splx(s);
	}
	if ((dhsilos == 0) || ((timercalls += FASTTIMER) >= hz)) {
		dhslowtimers++;		/*DEBUG*/
		timercalls = 0;
		for (dh = 0; dh < NDH; dh++) {
		    ave(dhrate[dh], dhchars[dh], 8);
		    if ((dhchars[dh] > dhhighrate) &&
		      ((dhsilos & (1 << dh)) == 0)) {
			((struct dhdevice *)(dhinfo[dh]->ui_addr))->dhsilo =
			    (dhchars[dh] > 500? 32 : 16);
			dhsilos |= (1 << dh);
			dhtransitions++;		/*DEBUG*/
		    } else if ((dhsilos & (1 << dh)) &&
		      (dhrate[dh] < dhlowrate)) {
			((struct dhdevice *)(dhinfo[dh]->ui_addr))->dhsilo = 0;
			dhsilos &= ~(1 << dh);
		    }
		    dhchars[dh] = 0;
		}
	}
	timeout(dhtimer, (caddr_t) 0, dhsilos? FASTTIMER: hz);
}

/*
 * Turn on the line associated with dh dev.
 */
dmopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register struct dmdevice *addr;
	register struct uba_device *ui;
	register int unit;
	register int dm;
	int s, error = 0;

	unit = minor(dev);
	dm = unit >> 4;
	tp = &dh11[unit];
	unit &= 0xf;
	if (dm >= NDH || (ui = dminfo[dm]) == 0 || ui->ui_alive == 0) {
		tp->t_state |= TS_CARR_ON;
		return (0);
	}
	addr = (struct dmdevice *)ui->ui_addr;
	s = spl5();
	for (;;) {
		tp->t_state |= TS_WOPEN;
		addr->dmcsr &= ~DM_SE;
		while (addr->dmcsr & DM_BUSY)
			;
		addr->dmcsr = unit;
		addr->dmlstat = DML_ON;
		if ((addr->dmlstat & DML_CAR) || (dhsoftCAR[dm] & (1 << unit)))
			tp->t_state |= TS_CARR_ON;
		addr->dmcsr = DM_IE|DM_SE;
		if (tp->t_state&TS_CARR_ON || flag&O_NONBLOCK || 
		    tp->t_cflag&CLOCAL)
			break;
		if (error = ttysleep(tp, (caddr_t)&tp->t_rawq, TTIPRI | PCATCH,
		    ttopen, 0))
			break;
	}
	splx(s);
	return (error);
}

/*
 * Dump control bits into the DM registers.
 */
dmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct uba_device *ui;
	register struct dmdevice *addr;
	register int unit, s;
	int dm;

	unit = minor(dev);
	dm = unit >> 4;
	if ((ui = dminfo[dm]) == 0 || ui->ui_alive == 0)
		return;
	addr = (struct dmdevice *)ui->ui_addr;
	s = spl5();
	addr->dmcsr &= ~DM_SE;
	while (addr->dmcsr & DM_BUSY)
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
	addr->dmcsr = DM_IE|DM_SE;
	splx(s);
}

/*
 * DM11 interrupt; deal with carrier transitions.
 */
dmintr(dm)
	register int dm;
{
	register struct uba_device *ui;
	register struct tty *tp;
	register struct dmdevice *addr;
	int unit;

	ui = dminfo[dm];
	if (ui == 0)
		return;
	addr = (struct dmdevice *)ui->ui_addr;
	if (addr->dmcsr&DM_DONE) {
		if (addr->dmcsr&DM_CF) {
			unit = addr->dmcsr & 0xf;
			tp = &dh11[(dm << 4) + unit];
			if (addr->dmlstat & DML_CAR)
				(void)(*linesw[tp->t_line].l_modem)(tp, 1);
			else if ((dhsoftCAR[dm] & (1<<unit)) == 0 &&
			    (*linesw[tp->t_line].l_modem)(tp, 0) == 0)
				addr->dmlstat = 0;
		}
		addr->dmcsr = DM_IE|DM_SE;
	}
}
#endif
