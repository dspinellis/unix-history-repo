/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dmf.c	6.18 (Berkeley) %G%
 */

#include "dmf.h"
#if NDMF > 0
/*
 * DMF32 driver
 *
 *
 * TODO:
 *	test with modem
 *	load as much as possible into silo
 *	use auto XON/XOFF
 *	test reset code
 ****************************
 * DMF32 line printer driver
 *
 * the line printer on dmfx is indicated by a minor device code of 128+x
 *
 * the flags field of the config file is interpreted like so:
 * bits		meaning
 * ----		-------
 * 0-7		soft carrier bits for ttys part of dmf32
 * 8-15		number of cols/line on the line printer
 *			if 0, 132 will be used.
 * 16-23	number of lines/page on the line printer
 *			if 0, 66 will be used.
 * 24		if 1 DO NOT use the auto format mode of the
 *			line printer parallel port
 */
#include "../machine/pte.h"

#include "bk.h"
#include "uba.h"
#include "param.h"
#include "conf.h"
#include "dir.h"
#include "user.h"
#include "proc.h"
#include "ioctl.h"
#include "tty.h"
#include "map.h"
#include "buf.h"
#include "vm.h"
#include "bkmac.h"
#include "clist.h"
#include "file.h"
#include "uio.h"
#include "kernel.h"
#include "syslog.h"

#include "ubareg.h"
#include "ubavar.h"
#include "dmfreg.h"

/*
 * Definition of the driver for the auto-configuration program.
 */
int	dmfprobe(), dmfattach(), dmfrint(), dmfxint();
int	dmflint();
struct	uba_device *dmfinfo[NDMF];
u_short	dmfstd[] = { 0 };
struct	uba_driver dmfdriver =
	{ dmfprobe, 0, dmfattach, 0, dmfstd, "dmf", dmfinfo };

int	dmf_timeout = 10;		/* silo timeout, in ms */
int	dmf_mindma = 4;			/* don't dma below this point */

/*
 * Local variables for the driver
 */
char	dmf_speeds[] =
	{ 0, 0, 1, 2, 3, 4, 0, 5, 6, 7, 010, 012, 014, 016, 017, 0 };

#ifndef	PORTSELECTOR
#define	ISPEED	B9600
#define	IFLAGS	(EVENP|ODDP|ECHO)
#else
#define	ISPEED	B4800
#define	IFLAGS	(EVENP|ODDP)
#endif

struct	tty dmf_tty[NDMF*8];
char	dmfsoftCAR[NDMF];

struct dmfl_softc {
	u_int	dmfl_state; 		/* soft state bits */
	int	dmfl_info;		/* uba info */
	u_short	dmfl_lines;		/* lines per page (66 def.) */
	u_short	dmfl_cols; 		/* cols per line (132 def.) */
	u_short	dmfl_format;		/* fflag for auto form feed */
	char	dmfl_buf[DMFL_BUFSIZ];
} dmfl_softc[NDMF];

/*
 * convert device number into DMF line printer unit number
 */
#define	DMFL_UNIT(d)	(minor(d)&0xF)	/* up to 16 DMFs */

#define ASLP 1		/* waiting for interrupt from dmf */
#define OPEN 2		/* line printer is open */
#define ERROR 4		/* error while printing, driver
			 refuses to do anything till closed */
#define MOREIO 8	/* more data for printer */

#ifndef lint
int	ndmf = NDMF*8;			/* used by iostat */
#endif
int	dmfact;				/* mask of active dmf's */
int	dmfstart(), ttrstrt();

/*
 * The clist space is mapped by the driver onto each UNIBUS.
 * The UBACVT macro converts a clist space address for unibus uban
 * into an i/o space address for the DMA routine.
 */
int	dmf_ubinfo[NUBA];		/* info about allocated unibus map */
int	cbase[NUBA];			/* base address in unibus map */
#define	UBACVT(x, uban)		(cbase[uban] + ((x)-(char *)cfree))
char	dmf_dma[NDMF*8];

/*
 * Routine for configuration to set dmf interrupt.
 */
/*ARGSUSED*/
dmfprobe(reg, ctlr)
	caddr_t reg;
	struct uba_device *ctlr;
{
	register int br, cvec;		/* these are ``value-result'' */
	register struct dmfdevice *dmfaddr = (struct dmfdevice *)reg;
	register int i;
	register unsigned int a;
	static char *dmfdevs[]=
		{"parallel","printer","synch","asynch"};
	unsigned int dmfoptions;
	static int (*intrv[3])() = { (int (*)())0, (int (*)())0, (int (*)())0 };

#ifdef lint
	br = 0; cvec = br; br = cvec;
	dmfxint(0); dmfrint(0);
	dmfsrint(); dmfsxint(); dmfdaint(); dmfdbint(); dmflint(0);
#endif
	/*
	 * Pick the usual size DMF vector here (don't decrement it here).
	 * grab configuration; note that the DMF32
	 * doesn't seem to put the right bits in this
	 * register until AFTER the interrupt vector is set.
	 */
	br = 0x15;
	cvec = (uba_hd[numuba].uh_lastiv - 4*8);
	dmfaddr->dmfccsr0 = (cvec >> 2);
	dmfoptions = dmfaddr->dmfccsr0 & DMFC_CONFMASK;

	/* catch a couple of special cases:  Able vmz/32n and vmz/lp	*/
	if (dmfoptions == DMFC_ASYNC) {
		/* Async portion only */

		cvec = (uba_hd[numuba].uh_lastiv -= 8);
		dmfaddr->dmfccsr0 = (cvec - 2*8) >> 2;
		intrv[0] = ctlr->ui_intr[4];
		intrv[1] = ctlr->ui_intr[5];
		ctlr->ui_intr = intrv;
	} else if (dmfoptions == DMFC_LP) {
		/* LP portion only */

		cvec = (uba_hd[numuba].uh_lastiv -= 8);
		ctlr->ui_intr = &ctlr->ui_intr[6];
	} else if (dmfoptions == (DMFC_LP|DMFC_ASYNC)) {
		/* LP ans Async portions only */

		cvec = (uba_hd[numuba].uh_lastiv -= 2*8);
		ctlr->ui_intr = &ctlr->ui_intr[4];
	} else {
		/* All other configurations get everything */

		cvec = (uba_hd[numuba].uh_lastiv -= 4*8);
	}
	a = (dmfoptions >> 12) & 0xf;
	printf("dmf%d:", ctlr->ui_unit);
	for (i = 0; a != 0; ++i, a >>= 1) {
		if (a & 1)
			printf(" %s",dmfdevs[i]);
	}
	printf(".\n");

	if (dmfoptions & DMFC_LP)
		dmfaddr->dmfl[0] = DMFL_RESET;
	return (sizeof (struct dmfdevice));
}

/*
 * Routine called to attach a dmf.
 */
dmfattach(ui)
	struct uba_device *ui;
{
	register int cols = (ui->ui_flags>>8) & 0xff;
	register int lines = (ui->ui_flags>>16) & 0xff;

	dmfsoftCAR[ui->ui_unit] = ui->ui_flags & 0xff;
	dmfl_softc[ui->ui_unit].dmfl_cols = cols == 0 ? DMFL_DEFCOLS : cols;
	dmfl_softc[ui->ui_unit].dmfl_lines = lines == 0 ? DMFL_DEFLINES : lines;
 	if ((ui->ui_flags >> 24) & 0x1)
 		dmfl_softc[ui->ui_unit].dmfl_format = (2 << 8);
 	else
 		dmfl_softc[ui->ui_unit].dmfl_format = (2 << 8) | DMFL_FORMAT;
	cbase[ui->ui_ubanum] = -1;
}


/*
 * Open a DMF32 line, mapping the clist onto the uba if this
 * is the first dmf on this uba.  Turn on this dmf if this is
 * the first use of it.
 */
/*ARGSUSED*/
dmfopen(dev, flag)
	dev_t dev;
{
	register struct tty *tp;
	register int unit, dmf;
	register struct dmfdevice *addr;
	register struct uba_device *ui;
	int s;

	unit = minor(dev);
	if (unit & 0200)
		return (dmflopen(dev,flag));
	dmf = unit >> 3;
	if (unit >= NDMF*8 || (ui = dmfinfo[dmf])== 0 || ui->ui_alive == 0)
		return (ENXIO);
	tp = &dmf_tty[unit];
	if (tp->t_state&TS_XCLUDE && u.u_uid!=0)
		return (EBUSY);
	addr = (struct dmfdevice *)ui->ui_addr;
	tp->t_addr = (caddr_t)addr;
	tp->t_oproc = dmfstart;
	tp->t_state |= TS_WOPEN;
	/*
	 * While setting up state for this uba and this dmf,
	 * block uba resets which can clear the state.
	 */
	s = spltty();
	if (cbase[ui->ui_ubanum] == -1) {
		dmf_ubinfo[ui->ui_ubanum] =
		    uballoc(ui->ui_ubanum, (caddr_t)cfree,
			nclist*sizeof(struct cblock), 0);
		cbase[ui->ui_ubanum] = UBAI_ADDR(dmf_ubinfo[ui->ui_ubanum]);
	}
	if ((dmfact&(1<<dmf)) == 0) {
		addr->dmfcsr |= DMF_IE;
		dmfact |= (1<<dmf);
		addr->dmfrsp = dmf_timeout;
	}
	splx(s);
	/*
	 * If this is first open, initialize tty state to default.
	 */
	if ((tp->t_state&TS_ISOPEN) == 0) {
		ttychars(tp);
#ifndef PORTSELECTOR
		if (tp->t_ispeed == 0) {
#else
			tp->t_state |= TS_HUPCLS;
#endif PORTSELECTOR
			tp->t_ispeed = ISPEED;
			tp->t_ospeed = ISPEED;
			tp->t_flags = IFLAGS;
#ifndef PORTSELECTOR
		}
#endif PORTSELECTOR
		dmfparam(unit);
	}
	/*
	 * Wait for carrier, then process line discipline specific open.
	 */
	s = spltty();
	for (;;) {
		if ((dmfmctl(dev, DMF_ON, DMSET) & (DMF_CAR<<8)) ||
		    (dmfsoftCAR[dmf] & (1<<(unit&07))))
			tp->t_state |= TS_CARR_ON;
		if (tp->t_state & TS_CARR_ON)
			break;
		tp->t_state |= TS_WOPEN;
		sleep((caddr_t)&tp->t_rawq, TTIPRI);
	}
	splx(s);
	return ((*linesw[tp->t_line].l_open)(dev, tp));
}

/*
 * Close a DMF32 line.
 */
/*ARGSUSED*/
dmfclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tty *tp;
	register unit;

	unit = minor(dev);
	if (unit & 0200) {
		dmflclose(dev,flag);
		return;
	}
		
	tp = &dmf_tty[unit];
	(*linesw[tp->t_line].l_close)(tp);
	(void) dmfmctl(unit, DMF_BRK, DMBIC);
	if (tp->t_state&TS_HUPCLS || (tp->t_state&TS_ISOPEN)==0)
		(void) dmfmctl(unit, DMF_OFF, DMSET);
	ttyclose(tp);
}

dmfread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	if (minor(dev) & 0200)
		return(ENXIO);
	tp = &dmf_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_read)(tp, uio));
}

dmfwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct tty *tp;

	if (minor(dev) & 0200)
		return (dmflwrite(dev,uio));
	tp = &dmf_tty[minor(dev)];
	return ((*linesw[tp->t_line].l_write)(tp, uio));
}

/*
 * DMF32 receiver interrupt.
 */
dmfrint(dmf)
	int dmf;
{
	register c;
	register struct tty *tp;
	register struct dmfdevice *addr;
	register struct tty *tp0;
	int unit;
	int overrun = 0;
	register struct uba_device *ui;

	ui = dmfinfo[dmf];
	if (ui == 0 || ui->ui_alive == 0)
		return;
	addr = (struct dmfdevice *)ui->ui_addr;
	tp0 = &dmf_tty[dmf * 8];
	/*
	 * Loop fetching characters from the silo for this
	 * dmf until there are no more in the silo.
	 */
	while ((c = addr->dmfrbuf) < 0) {

		unit = (c >> 8) & 07;
		tp = tp0 + unit;
		if (c & DMF_DSC) {
			addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
			if (addr->dmfrms & DMF_CAR)
				(void)(*linesw[tp->t_line].l_modem)(tp, 1);
			else if ((dmfsoftCAR[dmf] & (1 << unit)) == 0 &&
			    (*linesw[tp->t_line].l_modem)(tp, 0) == 0) {
				addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
				addr->dmflctms = DMFLCR_ENA;
			}
			continue;
		}
		if ((tp->t_state&TS_ISOPEN) == 0) {
			wakeup((caddr_t)&tp->t_rawq);
#ifdef PORTSELECTOR
			if ((tp->t_state & TS_WOPEN) == 0)
#endif
				continue;
		}
		if (c & (DMF_PE|DMF_DO|DMF_FE)) {
			if (c & DMF_PE)
				if ((tp->t_flags & (EVENP|ODDP)) == EVENP
			 	|| (tp->t_flags & (EVENP|ODDP)) == ODDP)
					continue;
			if ((c & DMF_DO) && overrun == 0) {
				log(LOG_WARNING, "dmf%d: silo overflow\n", dmf);
				overrun = 1;
			}
			if (c & DMF_FE)
				/*
			 	* At framing error (break) generate
			 	* a null (in raw mode, for getty), or a
			 	* interrupt (in cooked/cbreak mode).
			 	*/
				if (tp->t_flags & RAW)
					c = 0;
				else
					c = tp->t_intrc;
		}
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
 * Ioctl for DMF32.
 */
/*ARGSUSED*/
dmfioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tty *tp;
	register int unit = minor(dev);
	int error;
 
	if (unit & 0200)
		return (ENOTTY);
	tp = &dmf_tty[unit];
	error = (*linesw[tp->t_line].l_ioctl)(tp, cmd, data, flag);
	if (error >= 0)
		return (error);
	error = ttioctl(tp, cmd, data, flag);
	if (error >= 0) {
		if (cmd == TIOCSETP || cmd == TIOCSETN || cmd == TIOCLBIS ||
		    cmd == TIOCLBIC || cmd == TIOCLSET)
			dmfparam(unit);
		return (error);
	}
	switch (cmd) {

	case TIOCSBRK:
		(void) dmfmctl(dev, DMF_BRK, DMBIS);
		break;

	case TIOCCBRK:
		(void) dmfmctl(dev, DMF_BRK, DMBIC);
		break;

	case TIOCSDTR:
		(void) dmfmctl(dev, DMF_DTR|DMF_RTS, DMBIS);
		break;

	case TIOCCDTR:
		(void) dmfmctl(dev, DMF_DTR|DMF_RTS, DMBIC);
		break;

	case TIOCMSET:
		(void) dmfmctl(dev, dmtodmf(*(int *)data), DMSET);
		break;

	case TIOCMBIS:
		(void) dmfmctl(dev, dmtodmf(*(int *)data), DMBIS);
		break;

	case TIOCMBIC:
		(void) dmfmctl(dev, dmtodmf(*(int *)data), DMBIC);
		break;

	case TIOCMGET:
		*(int *)data = dmftodm(dmfmctl(dev, 0, DMGET));
		break;

	default:
		return (ENOTTY);
	}
	return (0);
}

dmtodmf(bits)
	register int bits;
{
	register int b;

	b = bits & 012;
	if (bits & DML_ST) b |= DMF_RATE;
	if (bits & DML_RTS) b |= DMF_RTS;
	if (bits & DML_USR) b |= DMF_USRW;
	return(b);
}

dmftodm(bits)
	register int bits;
{
	register int b;

	b = (bits & 012) | ((bits >> 7) & 0760) | DML_LE;
	if (bits & DMF_USRR) b |= DML_USR;
	if (bits & DMF_RTS) b |= DML_RTS;
	return(b);
}
 

/*
 * Set parameters from open or stty into the DMF hardware
 * registers.
 */
dmfparam(unit)
	register int unit;
{
	register struct tty *tp;
	register struct dmfdevice *addr;
	register int lpar, lcr;
	int s;

	tp = &dmf_tty[unit];
	addr = (struct dmfdevice *)tp->t_addr;
	/*
	 * Block interrupts so parameters will be set
	 * before the line interrupts.
	 */
	s = spltty();
	addr->dmfcsr = (unit&07) | DMFIR_LCR | DMF_IE;
	if ((tp->t_ispeed)==0) {
		tp->t_state |= TS_HUPCLS;
		(void) dmfmctl(unit, DMF_OFF, DMSET);
		splx(s);
		return;
	}
	lpar = (dmf_speeds[tp->t_ospeed]<<12) | (dmf_speeds[tp->t_ispeed]<<8);
	lcr = DMFLCR_ENA;
	if ((tp->t_ispeed) == B134)
		lpar |= BITS6|PENABLE;
	else if (tp->t_flags & (RAW|LITOUT|PASS8))
		lpar |= BITS8;
	else {
		lpar |= BITS7|PENABLE;
		/* CHECK FOR XON/XOFF AND SET lcr |= DMF_AUTOX; */
	}
	if (tp->t_flags&EVENP)
		lpar |= EPAR;
	if ((tp->t_ospeed) == B110)
		lpar |= TWOSB;
	lpar |= (unit&07);
	addr->dmflpr = lpar;
	addr->dmflctms = (addr->dmflctms &~ 0xff) | lcr;
	splx(s);
}

/*
 * DMF32 transmitter interrupt.
 * Restart the idle line.
 */
dmfxint(dmf)
	int dmf;
{
	int unit0 = dmf * 8;
	struct tty *tp0 = &dmf_tty[unit0];
	register struct tty *tp;
	register struct dmfdevice *addr;
	register struct uba_device *ui;
	register int t;
	short cntr;

	ui = dmfinfo[dmf];
	addr = (struct dmfdevice *)ui->ui_addr;
	while ((t = addr->dmfcsr) & DMF_TI) {
		if (t & DMF_NXM)
			/* SHOULD RESTART OR SOMETHING... */
			printf("dmf%d: NXM line %d\n", dmf, t >> 8 & 7);
		t = t >> 8 & 7;
		tp = tp0 + t;
		tp->t_state &= ~TS_BUSY;
		if (tp->t_state&TS_FLUSH)
			tp->t_state &= ~TS_FLUSH;
		else if (dmf_dma[unit0 + t]) {
			/*
			 * Do arithmetic in a short to make up
			 * for lost 16&17 bits.
			 */
			addr->dmfcsr = DMFIR_TBA | DMF_IE | t;
			cntr = addr->dmftba -
			    UBACVT(tp->t_outq.c_cf, ui->ui_ubanum);
			ndflush(&tp->t_outq, (int)cntr);
		}
		if (tp->t_line)
			(*linesw[tp->t_line].l_start)(tp);
		else
			dmfstart(tp);
	}
}

/*
 * Start (restart) transmission on the given DMF32 line.
 */
dmfstart(tp)
	register struct tty *tp;
{
	register struct dmfdevice *addr;
	register int unit, nch;
	int s;
	register int dmf;

	unit = minor(tp->t_dev);
	dmf = unit >> 3;
	unit &= 07;
	addr = (struct dmfdevice *)tp->t_addr;

	/*
	 * Must hold interrupts in following code to prevent
	 * state of the tp from changing.
	 */
	s = spltty();
	/*
	 * If it's currently active, or delaying, no need to do anything.
	 */
	if (tp->t_state&(TS_TIMEOUT|TS_BUSY|TS_TTSTOP))
		goto out;
	/*
	 * If there are still characters in the silo,
	 * just reenable the transmitter.
	 */
	addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
	if (addr->dmftsc) {
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		addr->dmflctms = addr->dmflctms | DMF_TE;
		tp->t_state |= TS_BUSY;
		goto out;
	}
	/*
	 * If there are sleepers, and output has drained below low
	 * water mark, wake up the sleepers.
	 */
	if (tp->t_outq.c_cc<=TTLOWAT(tp)) {
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
	if (tp->t_flags & (RAW|LITOUT))
		nch = ndqb(&tp->t_outq, 0);
	else {
		if ((nch = ndqb(&tp->t_outq, 0200)) == 0) {
			/*
		 	* If first thing on queue is a delay process it.
		 	*/
			nch = getc(&tp->t_outq);
			timeout(ttrstrt, (caddr_t)tp, (nch&0x7f)+6);
			tp->t_state |= TS_TIMEOUT;
			goto out;
		}
	}
	/*
	 * If characters to transmit, restart transmission.
	 */
	if (nch >= dmf_mindma) {
		register car;

		dmf_dma[minor(tp->t_dev)] = 1;
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		addr->dmflctms = addr->dmflctms | DMF_TE;
		car = UBACVT(tp->t_outq.c_cf, dmfinfo[dmf]->ui_ubanum);
		addr->dmfcsr = DMF_IE | DMFIR_TBA | unit;
		addr->dmftba = car;
		addr->dmftcc = ((car >> 2) & 0xc000) | nch;
		tp->t_state |= TS_BUSY;
	} else if (nch) {
		register char *cp = tp->t_outq.c_cf;
		register int i;

		dmf_dma[minor(tp->t_dev)] = 0;
		nch = MIN(nch, DMF_SILOCNT);
		addr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
		addr->dmflctms = addr->dmflctms | DMF_TE;
		addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
		for (i = 0; i < nch; i++)
			addr->dmftbuf = *cp++;
		ndflush(&tp->t_outq, nch);
		tp->t_state |= TS_BUSY;
	}
out:
	splx(s);
}

/*
 * Stop output on a line, e.g. for ^S/^Q or output flush.
 */
/*ARGSUSED*/
dmfstop(tp, flag)
	register struct tty *tp;
{
	register struct dmfdevice *addr;
	register unit = minor(tp->t_dev) & 7;
	int s;

	addr = (struct dmfdevice *)tp->t_addr;
	/*
	 * Block input/output interrupts while messing with state.
	 */
	s = spltty();
	if (flag) {
		addr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
		if (addr->dmftsc) {
			/*
			 * Flush regardless of whether we're transmitting
			 * (TS_BUSY), if the silo contains untransmitted
			 * characters.
			 */
			addr->dmfcsr = DMFIR_LCR | unit | DMF_IE;
			addr->dmflctms = addr->dmflctms | DMF_TE | DMF_FLUSH;
			/* this will interrupt so let dmfxint handle the rest */
			tp->t_state |= TS_FLUSH|TS_BUSY;
		}
	} else {
		if (tp->t_state & TS_BUSY) {
			/*
			 * Stop transmission by disabling
			 * the transmitter.  We'll pick up where we
			 * left off by reenabling in dmfstart.
			 */
			addr->dmfcsr = DMFIR_LCR | unit | DMF_IE;
			addr->dmflctms = addr->dmflctms &~ DMF_TE;
			/* no interrupt here */
			tp->t_state &= ~TS_BUSY;
		}
	}
	splx(s);
}

/*
 * DMF32 modem control
 */
dmfmctl(dev, bits, how)
	dev_t dev;
	int bits, how;
{
	register struct dmfdevice *dmfaddr;
	register int unit, mbits, lcr;
	int s;

	unit = minor(dev);
	dmfaddr = (struct dmfdevice *)(dmf_tty[unit].t_addr);
	unit &= 07;
	s = spltty();
	dmfaddr->dmfcsr = DMF_IE | DMFIR_TBUF | unit;
	mbits = dmfaddr->dmfrms << 8;
	dmfaddr->dmfcsr = DMF_IE | DMFIR_LCR | unit;
	lcr = dmfaddr->dmflctms;
	mbits |= (lcr & 0xff00) >> 8;
	switch (how) {
	case DMSET:
		mbits = (mbits &0xff00) | bits;
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
	if (mbits & DMF_BRK)
		lcr |= DMF_RBRK;
	else
		lcr &= ~DMF_RBRK;
	dmfaddr->dmflctms = ((mbits & 037) << 8) | (lcr & 0xff);
	(void) splx(s);
	return(mbits);
}

/*
 * Reset state of driver if UBA reset was necessary.
 * Reset the csr, lpr, and lcr registers on open lines, and
 * restart transmitters.
 */
dmfreset(uban)
	int uban;
{
	register int dmf, unit;
	register struct tty *tp;
	register struct uba_device *ui;
	register struct dmfdevice *addr;
	int i;

	for (dmf = 0; dmf < NDMF; dmf++) {
		ui = dmfinfo[dmf];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		printf(" dmf%d", dmf);
		if (dmf_ubinfo[uban]) {
			dmf_ubinfo[uban] = uballoc(uban, (caddr_t)cfree,
			    nclist*sizeof (struct cblock), 0);
			cbase[uban] = UBAI_ADDR(dmf_ubinfo[uban]);
		}
		addr = (struct dmfdevice *)ui->ui_addr;
		addr->dmfcsr = DMF_IE;
		addr->dmfrsp = dmf_timeout;
		unit = dmf * 8;
		for (i = 0; i < 8; i++) {
			tp = &dmf_tty[unit];
			if (tp->t_state & (TS_ISOPEN|TS_WOPEN)) {
				dmfparam(unit);
				(void) dmfmctl(unit, DMF_ON, DMSET);
				tp->t_state &= ~TS_BUSY;
				dmfstart(tp);
			}
			unit++;
		}
	}
}

/*
 * dmflopen -- open the line printer port on a dmf32
 */
/* ARGSUSED */
dmflopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int dmf;
	register struct dmfl_softc *sc;
	register struct uba_device *ui;
	register struct dmfdevice *addr;

	dmf = DMFL_UNIT(dev);
	if (dmf >= NDMF || (ui = dmfinfo[dmf]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	sc = &dmfl_softc[dmf];
	if (sc->dmfl_state & OPEN)
		return (EBUSY);
	addr = (struct dmfdevice *)ui->ui_addr;
	if (addr->dmfl_ctrl & DMFL_OFFLINE) {
#ifdef notdef
		log(LOG_WARNING, "dmf%d: line printer offline/jammed\n",
			dmf);
#endif
		return (EIO);
	}
	if ((addr->dmfl_ctrl & DMFL_CONV)) {
		log(LOG_WARNING, "dmf%d: line printer disconnected\n", dmf);
		return (EIO);
	}

	addr->dmfl_ctrl = 0;
	sc->dmfl_state |= OPEN;
	return (0);
}

/* ARGSUSED */
dmflclose(dev, flag)
	dev_t dev;
	int flag;
{
	register int dmf = DMFL_UNIT(dev);
	register struct dmfl_softc *sc = &dmfl_softc[dmf];
	register struct uba_device *ui = dmfinfo[dmf];

	sc->dmfl_state = 0;
	if (sc->dmfl_info != 0)
		ubarelse((int)ui->ui_ubanum, &sc->dmfl_info);

	((struct dmfdevice *)ui->ui_addr)->dmfl_ctrl = 0;
}

dmflwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int n;
	register int error;
	register struct dmfl_softc *sc;

	sc = &dmfl_softc[DMFL_UNIT(dev)];
	if (sc->dmfl_state & ERROR)
		return (EIO);
	while (n = (unsigned)uio->uio_resid) {
		if (n > DMFL_BUFSIZ) {
			n = DMFL_BUFSIZ;
			sc->dmfl_state |= MOREIO;
		} else
			sc->dmfl_state &= ~MOREIO;
		if (error = uiomove(sc->dmfl_buf, (int)n, UIO_WRITE, uio))
			return (error);
		if (error = dmflout(dev, sc->dmfl_buf, n))
			return (error);
	}
	return (0);
}


/*
 * dmflout -- start io operation to dmf line printer
 *		cp is addr of buf of n chars to be sent.
 *
 *	-- dmf will be put in formatted output mode, this will
 *		be selectable from an ioctl if the
 *		need ever arises.
 */
dmflout(dev, cp, n)
	dev_t dev;
	char *cp;
	int n;
{
	register struct dmfl_softc *sc;
	register int dmf;
	register struct uba_device *ui;
	register struct dmfdevice *d;
	int s;

	dmf = DMFL_UNIT(dev);
	sc = &dmfl_softc[dmf];
	if (sc->dmfl_state & ERROR)
		return (EIO);
	ui = dmfinfo[dmf];
	/*
	 * allocate unibus resources, will be released when io
	 * operation is done.
	 */
	if (sc->dmfl_info == 0)
		sc->dmfl_info = uballoc(ui->ui_ubanum, cp, n, 0);
	d = (struct dmfdevice *)ui->ui_addr;
	d->dmfl_ctrl = sc->dmfl_format;		/* indir reg 2 */
	/* indir reg auto increments on r/w */
	/* SO DON'T CHANGE THE ORDER OF THIS CODE */
	d->dmfl_indrct = 0;			/* prefix chars & num */
	d->dmfl_indrct = 0;			/* suffix chars & num */
	d->dmfl_indrct = sc->dmfl_info; 	/* dma lo 16 bits addr */
	d->dmfl_indrct = -n;			/* number of chars */

	d->dmfl_indrct = ((sc->dmfl_info>>16)&3) | DMFL_OPTIONS;
						/* dma hi 2 bits addr */
	d->dmfl_indrct = sc->dmfl_lines 	/* lines per page */
		| (sc->dmfl_cols<<8);		/* carriage width */
	sc->dmfl_state |= ASLP;
	s = spltty();
	d->dmfl_ctrl |= DMFL_PEN | DMFL_IE;
	while (sc->dmfl_state & ASLP) {
		sleep(sc->dmfl_buf, PZERO + 8);
		while (sc->dmfl_state & ERROR) {
			timeout(dmflint, (caddr_t)dmf, 10 * hz);
			sleep((caddr_t)&sc->dmfl_state, PZERO + 8);
		}
	}
	splx(s);
	return (0);
}

/*
 * dmflint -- handle an interrupt from the line printer part of the dmf32
 */
dmflint(dmf)
	int dmf;
{
	register struct uba_device *ui;
	register struct dmfl_softc *sc;
	register struct dmfdevice *d;
	short dmfl_stats;

	ui = dmfinfo[dmf];
	sc = &dmfl_softc[dmf];
	d = (struct dmfdevice *)ui->ui_addr;

	d->dmfl_ctrl &= ~DMFL_IE;
	dmfl_stats = d->dmf_ctrl;
	if (sc->dmfl_state & ERROR) {
		if ((dmfl_stats & DMFL_OFFLINE) == 0)
			sc->dmfl_state &= ~ERROR;
		wakeup((caddr_t)&sc->dmfl_state);
		return;
	}
	if (dmfl_stats & DMFL_DMAERR)
		log(LOG_WARNING, "dmf%d: NXM\n", dmf);
	if (dmfl_stats & DMFL_OFFLINE) {
		log(LOG_WARNING, "dmf%d: printer error\n", dmf);
		sc->dmfl_state |= ERROR;
	}
#ifdef notdef
	if (dmfl_stats & DMFL_PDONE) {
		printf("bytes= %d\n", d->dmfl_indrct);
		printf("lines= %d\n", d->dmfl_indrct);
	}
#endif
	sc->dmfl_state &= ~ASLP;
	wakeup((caddr_t)sc->dmfl_buf);
	if (sc->dmfl_info && (sc->dmfl_state & MOREIO) == 0)
		ubarelse(ui->ui_ubanum, &sc->dmfl_info);
}

/* stubs for interrupt routines for devices not yet supported */

dmfsrint()
{
	printf("dmfsrint\n");
}

dmfsxint()
{
	printf("dmfsxint\n");
}

dmfdaint()
{
	printf("dmfdaint\n");
}

dmfdbint()
{
	printf("dmfdbint\n");
}
#endif NDMF
