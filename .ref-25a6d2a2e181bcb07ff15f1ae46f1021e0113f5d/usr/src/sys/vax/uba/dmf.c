/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dmf.c	7.7 (Berkeley) %G%
 */

/*
 * DMF32 driver
 *
 *
 * TODO:
 *	test with modem
 *	load as much as possible into silo
 *	use auto XON/XOFF
 *	test reset code
 */
#include "dmf.h"
#if NDMF > 0

#ifndef NDMF_LP
#define	NDMF_LP	NDMF
#endif	NDMF_LP
#include "machine/pte.h"

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

#include "dmx.h"
#include "ubareg.h"
#include "ubavar.h"
#include "dmxreg.h"
#include "dmfreg.h"
#include "dmreg.h"

extern	int dmx_timeout;		/* silo timeout, in ms */
extern	char dmx_speeds[];
int	dmfstart();

/*
 * The clist space is mapped by one terminal driver onto each UNIBUS.
 * The identity of the board which allocated resources is recorded,
 * so the process may be repeated after UNIBUS resets.
 * The UBACVT macro converts a clist space address for unibus uban
 * into an i/o space address for the DMA routine.
 */
int	dmf_uballoc[NUBA];	/* which dmf (if any) allocated unibus map */
int	cbase[NUBA];		/* base address of clists in unibus map */

/*
 * Autoconfiguration and variables for DMF32
 */
int	dmfprobe(), dmfattach(), dmfrint(), dmfxint();
int	dmflint();
struct	uba_device *dmfinfo[NDMF];
u_short	dmfstd[] = { 0 };
struct	uba_driver dmfdriver =
	{ dmfprobe, 0, dmfattach, 0, dmfstd, "dmf", dmfinfo };

struct	tty dmf_tty[NDMF*8];
struct	dmx_softc dmf_softc[NDMF];
#ifndef lint
int	ndmf = NDMF*8;			/* used by iostat */
#endif

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
		/* LP and Async portions only */

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
		dmfaddr->dmfl_ctrl = DMFL_RESET;
	return (sizeof (struct dmfdevice));
}

/*
 * Routine called to attach a dmf.
 */
dmfattach(ui)
	register struct uba_device *ui;
{
	register struct dmx_softc *sc;

	sc = &dmf_softc[ui->ui_unit];
	sc->dmx_type = 'f';
	sc->dmx_unit = ui->ui_unit;
	sc->dmx_unit0 = 0;
	sc->dmx_ubanum = ui->ui_ubanum;
	sc->dmx_softCAR = ui->ui_flags & 0xff;
	sc->dmx_tty = &dmf_tty[ui->ui_unit * 8];
	sc->dmx_octet =
	    (struct dmx_octet *)&((struct dmfdevice *)ui->ui_addr)->dmfa;

	cbase[ui->ui_ubanum] = -1;
	dmf_uballoc[ui->ui_ubanum] = -1;
#if NDMF_LP > 0
	dmflattach(ui);
#endif NDMF_LP
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
	register struct dmx_softc *sc;
	int unit, dmf;
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
	sc = &dmf_softc[dmf];
	addr = (struct dmfdevice *)ui->ui_addr;
	tp->t_addr = (caddr_t)(&addr->dmfa);
	tp->t_oproc = dmfstart;
	tp->t_dev = dev;			/* needed before dmxopen */

	/*
	 * While setting up state for this uba,
	 * block uba resets which can clear the state.
	 */
	s = spl6();
	if (cbase[ui->ui_ubanum] == -1) {
		dmf_uballoc[ui->ui_ubanum] = dmf;
		cbase[ui->ui_ubanum] = UBAI_ADDR(uballoc(ui->ui_ubanum,
		    (caddr_t)cfree, nclist*sizeof(struct cblock), 0));
	}
	splx(s);

	return (dmxopen(tp, sc));
}

/*
 * Close a DMF32 line.
 */
/*ARGSUSED*/
dmfclose(dev, flag)
	dev_t dev;
	int flag;
{
	register unit;

	unit = minor(dev);
	if (unit & 0200) {
		dmflclose(dev, flag);
		return;
	}
	dmxclose(&dmf_tty[unit]);
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
	struct uba_device *ui;

	ui = dmfinfo[dmf];
	if (ui == 0 || ui->ui_alive == 0)
		return;
	dmxrint(&dmf_softc[dmf]);
}

/*
 * Ioctl for DMF32.
 */
dmfioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	int unit = minor(dev);
 
	if (unit & 0200)
		return (ENOTTY);
	return (dmxioctl(&dmf_tty[unit], cmd, data, flag));
}

/*
 * DMF32 transmitter interrupt.
 * Restart the idle line.
 */
dmfxint(dmf)
	int dmf;
{

	dmxxint(&dmf_softc[dmf]);
}

/*
 * Start (restart) transmission on the given line.
 */
dmfstart(tp)
	struct tty *tp;
{

	dmxstart(tp, &dmf_softc[minor(tp->t_dev) >> 3]);
}

/*
 * Stop output on a line, e.g. for ^S/^Q or output flush.
 */
dmfstop(tp, flag)
	struct tty *tp;
{

	dmxstop(tp, &dmf_softc[minor(tp->t_dev) >> 3], flag);
}

/*
 * Reset state of driver if UBA reset was necessary.
 * Reset the csr, lpr, and lcr registers on open lines, and
 * restart transmitters.
 */
dmfreset(uban)
	int uban;
{
	register int dmf;
	register struct tty *tp;
	register struct uba_device *ui;
	register struct dmfdevice *addr;
	int i;

	for (dmf = 0; dmf < NDMF; dmf++) {
		ui = dmfinfo[dmf];
		if (ui == 0 || ui->ui_alive == 0 || ui->ui_ubanum != uban)
			continue;
		printf(" dmf%d", dmf);
		if (dmf_uballoc[uban] == dmf) {
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
		addr = (struct dmfdevice *)ui->ui_addr;
		addr->dmfa.csr = DMF_IE;
		addr->dmfa.rsp = dmx_timeout;
		tp = &dmf_tty[dmf * 8];
		for (i = 0; i < 8; i++, tp++) {
			if (tp->t_state & (TS_ISOPEN|TS_WOPEN)) {
				dmxparam(tp);
				(void) dmxmctl(tp, DMF_ON, DMSET);
				tp->t_state &= ~TS_BUSY;
				dmfstart(tp);
			}
		}
#if NDMF_LP > 0
		dmflint(dmf);
#endif
	}
}

#if NDMF_LP > 0
/*
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
#define	DMFL_UNIT(d)	(minor(d) & 0xf)	/* up to 16 DMFs */

#define ASLP 1		/* waiting for interrupt from dmf */
#define OPEN 2		/* line printer is open */
#define ERROR 4		/* error while printing, driver
			 refuses to do anything till closed */
#define MOREIO 8	/* more data for printer */

/*
 * Attach printer portion of dmf.
 */
dmflattach(ui)
	register struct uba_device *ui;
{
	register int unit = ui->ui_unit;
	register int cols = (ui->ui_flags>>8) & 0xff;
	register int lines = (ui->ui_flags>>16) & 0xff;
	register struct dmfl_softc *sc;

	sc = &dmfl_softc[unit];
	sc->dmfl_cols = cols == 0 ? DMFL_DEFCOLS : cols;
	sc->dmfl_lines = lines == 0 ? DMFL_DEFLINES : lines;
 	if ((ui->ui_flags >> 24) & 0x1)
 		sc->dmfl_format = (2 << 8);
 	else
 		sc->dmfl_format = (2 << 8) | DMFL_FORMAT;
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
		if (error = uiomove(sc->dmfl_buf, (int)n, uio))
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
	dmfl_stats = d->dmfl_ctrl;
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
#endif NDMF_LP

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
