/*	dh.c	4.33	81/05/09	*/

#include "dh.h"
#if NDH > 0
/*
 * DH-11/DM-11 driver
 */
#include "bk.h"
#include "../h/param.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/tty.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/buf.h"
#include "../h/vm.h"
#include "../h/ubareg.h"
#include "../h/ubavar.h"
#include "../h/bk.h"
#include "../h/clist.h"
#include "../h/mx.h"
#include "../h/file.h"

/*
 * Definition of the driver for the auto-configuration program.
 * There is one definition for the dh and one for the dm.
 */
int	dhprobe(), dhattach(), dhrint(), dhxint();
struct	uba_device *dhinfo[NDH];
u_short	dhstd[] = { 0 };
struct	uba_driver dhdriver =
	{ dhprobe, 0, dhattach, 0, dhstd, "dh", dhinfo };

int	dmprobe(), dmattach(), dmintr();
struct	uba_device *dminfo[NDH];
u_short	dmstd[] = { 0 };
struct	uba_driver dmdriver =
	{ dmprobe, 0, dmattach, 0, dmstd, "dm", dminfo };

struct dhdevice
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

/* Bits in dhlpr */
#define	BITS6	01
#define	BITS7	02
#define	BITS8	03
#define	TWOSB	04
#define	PENABLE	020
/* DEC manuals incorrectly say this bit causes generation of even parity. */
#define	OPAR	040
#define	HDUPLX	040000

#if NBK == 0
#define	DH_IE	(DH_TIE|DH_RIE)
#else
#define	DH_IE	(DH_TIE|DH_SIE|DH_RIE)
#endif

/* Bits in dhrcr */
#define	DH_PE		0010000		/* parity error */
#define	DH_FE		0020000		/* framing error */
#define	DH_DO		0040000		/* data overrun */

struct dmdevice
{
	short	dmcsr;		/* control status register */
	short	dmlstat;	/* line status register */
	short	dmpad1[2];
};

/* bits in dm csr */
#define	DM_RF		0100000		/* ring flag */
#define	DM_CF		0040000		/* carrier flag */
#define	DM_CTS		0020000		/* clear to send */
#define	DM_SRF		0010000		/* secondary receive flag */
#define	DM_CS		0004000		/* clear scan */
#define	DM_CM		0002000		/* clear multiplexor */
#define	DM_MM		0001000		/* maintenance mode */
#define	DM_STP		0000400		/* step */
#define	DM_DONE		0000200		/* scanner is done */
#define	DM_IE		0000100		/* interrupt enable */
#define	DM_SE		0000040		/* scan enable */
#define	DM_BUSY		0000020		/* scan busy */

/* bits in dm lsr */
#define	DML_RNG		0000200		/* ring */
#define	DML_CAR		0000100		/* carrier detect */
#define	DML_CTS		0000040		/* clear to send */
#define	DML_SR		0000020		/* secondary receive */
#define	DML_ST		0000010		/* secondary transmit */
#define	DML_RTS		0000004		/* request to send */
#define	DML_DTR		0000002		/* data terminal ready */
#define	DML_LE		0000001		/* line enable */

#define	DML_ON		(DML_DTR|DML_LE)
#define	DML_OFF		(DML_LE)

/*
 * Local variables for the driver
 */
short	dhsar[NDH];			/* software copy of last bar */
short	dhsoftCAR[NDH];

struct	tty dh11[NDH*16];
int	ndh11	= NDH*16;
int	dhact;				/* mask of active dh's */
int	dhstart(), ttrstrt();

/*
 * The clist space is mapped by the driver onto each UNIBUS.
 * The UBACVT macro converts a clist space address for unibus uban
 * into an i/o space address for the DMA routine.
 */
int	dh_ubinfo[MAXNUBA];		/* info about allocated unibus map */
int	cbase[MAXNUBA];			/* base address in unibus map */
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
#endif
#ifndef notdef
	dhaddr->un.dhcsr = DH_RIE|DH_MM|DH_RI;
	DELAY(25);
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
	return (1);
}

/*
 * Routine called to attach a dh.
 */
dhattach(ui)
	struct uba_device *ui;
{

	dhsoftCAR[ui->ui_unit] = ui->ui_flags;
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
	int s;

	unit = minor(dev);
	dh = unit >> 4;
	if (unit >= NDH*16 || (ui = dhinfo[dh])== 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	tp = &dh11[unit];
	if (tp->t_state&XCLUDE && u.u_uid!=0) {
		u.u_error = EBUSY;
		return;
	}
	addr = (struct dhdevice *)ui->ui_addr;
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
			512+nclist*sizeof(struct cblock), 0);
		cbase[ui->ui_ubanum] = dh_ubinfo[ui->ui_ubanum]&0x3ffff;
	}
	if ((dhact&(1<<dh)) == 0) {
		addr->un.dhcsr |= DH_IE;
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
	((struct dhdevice *)(tp->t_addr))->dhbreak &= ~(1<<(unit&017));
	if (tp->t_state&HUPCLS || (tp->t_state&ISOPEN)==0)
		dmctl(unit, DML_OFF, DMSET);
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
		if ((tp->t_state&ISOPEN)==0) {
			wakeup((caddr_t)tp);
			continue;
		}
		if (c & DH_PE)
			if ((tp->t_flags&(EVENP|ODDP))==EVENP
			 || (tp->t_flags&(EVENP|ODDP))==ODDP )
				continue;
		if ((c & DH_DO) && overrun == 0) {
			printf("dh%d: silo overflow\n", dh);
			overrun = 1;
		}
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
#if NBK > 0
		if (tp->t_line == NETLDISC) {
			c &= 0177;
			BKINPUT(c, tp);
		} else
#endif
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
	register struct dhdevice *addr;
	register int lpar;
	int s;

	tp = &dh11[unit];
	addr = (struct dhdevice *)tp->t_addr;
	/*
	 * Block interrupts so parameters will be set
	 * before the line interrupts.
	 */
	s = spl5();
	addr->un.dhcsrl = (unit&0xf) | DH_IE;
	if ((tp->t_ispeed)==0) {
		tp->t_state |= HUPCLS;
		dmctl(unit, DML_OFF, DMSET);
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
			tp->t_state &= ~BUSY;
			if (tp->t_state&FLUSH)
				tp->t_state &= ~FLUSH;
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
	if (tp->t_flags&RAW || tp->t_local&LLITOUT)
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
	register struct dhdevice *addr;
	register int unit, s;

	addr = (struct dhdevice *)tp->t_addr;
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
	register struct uba_device *ui;
	int i;

	if (dh_ubinfo[uban] == 0)
		return;
	ubarelse(uban, &dh_ubinfo[uban]);
	dh_ubinfo[uban] = uballoc(uban, (caddr_t)cfree,
	    512+nclist*sizeof (struct cblock), 0);
	cbase[uban] = dh_ubinfo[uban]&0x3ffff;
	dh = 0;
	for (dh = 0; dh < NDH; dh++) {
		ui = dhinfo[dh];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		printf(" dh%d", dh);
		((struct dhdevice *)ui->ui_addr)->un.dhcsr |= DH_IE;
		((struct dhdevice *)ui->ui_addr)->dhsilo = 16;
		unit = dh * 16;
		for (i = 0; i < 16; i++) {
			tp = &dh11[unit];
			if (tp->t_state & (ISOPEN|WOPEN)) {
				dhparam(unit);
				dmctl(unit, DML_ON, DMSET);
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

	for (dh = 0; dh < NDH; dh++)
		dhrint(dh);
}

/*
 * Turn on the line associated with dh dev.
 */
dmopen(dev)
	dev_t dev;
{
	register struct tty *tp;
	register struct dmdevice *addr;
	register struct uba_device *ui;
	register int unit;
	register int dm;

	unit = minor(dev);
	dm = unit >> 4;
	tp = &dh11[unit];
	unit &= 0xf;
	if (dm >= NDH || (ui = dminfo[dm]) == 0 || ui->ui_alive == 0 ||
	    (dhsoftCAR[dm]&(1<<unit))) {
		tp->t_state |= CARR_ON;
		return;
	}
	addr = (struct dmdevice *)ui->ui_addr;
	(void) spl5();
	addr->dmcsr &= ~DM_SE;
	while (addr->dmcsr & DM_BUSY)
		;
	addr->dmcsr = unit;
	addr->dmlstat = DML_ON;
	if (addr->dmlstat&DML_CAR)
		tp->t_state |= CARR_ON;
	addr->dmcsr = DH_IE|DM_SE;
	while ((tp->t_state&CARR_ON)==0)
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	(void) spl0();
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
	addr->dmcsr = DH_IE|DM_SE;
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

	ui = dminfo[dm];
	if (ui == 0)
		return;
	addr = (struct dmdevice *)ui->ui_addr;
	if (addr->dmcsr&DM_DONE && addr->dmcsr&DM_CF) {
		tp = &dh11[(dm<<4)+(addr->dmcsr&0xf)];
		wakeup((caddr_t)&tp->t_rawq);
		if ((tp->t_state&WOPEN)==0 &&
		    (tp->t_local&LMDMBUF)) {
			if (addr->dmlstat & DML_CAR) {
				tp->t_state &= ~TTSTOP;
				ttstart(tp);
			} else if ((tp->t_state&TTSTOP) == 0) {
				tp->t_state |= TTSTOP;
				dhstop(tp, 0);
			}
		} else if ((addr->dmlstat&DML_CAR)==0) {
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
		addr->dmcsr = DH_IE|DM_SE;
	}
}
#endif
