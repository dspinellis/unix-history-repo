/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)va.c	7.4 (Berkeley) %G%
 */

#include "va.h"
#if NVA > 0
/*
 * Varian printer plotter
 */
#include "../include/pte.h"

#include "sys/param.h"
#include "sys/user.h"
#include "sys/buf.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/ioctl.h"
#include "sys/vcmd.h"
#include "sys/uio.h"
#include "sys/kernel.h"

#include "ubareg.h"
#include "ubavar.h"

int	vadebug = 0;
#define	dprintf	if(vadebug)printf

unsigned minvaph();

#define	VAPRI	(PZERO-1)

struct	vadevice {
	u_short	vaba;			/* buffer address */
	short	vawc;			/* word count (2's complement) */
	union {
		short	Vacsw;		/* control status as word */
		struct {		/* control status as bytes */
			char Vacsl;
			char Vacsh;
		} vacsr;
	} vacs;
	short	vadata;			/* programmed i/o data buffer */
};

#define	vacsw	vacs.Vacsw
#define	vacsh	vacs.vacsr.Vacsh
#define	vacsl	vacs.vacsr.Vacsl

/* vacsw bits */
#define	VA_ERROR	0100000		/* some error has occurred */
#define	VA_NPRTIMO	0001000		/* DMA timeout error */
#define	VA_NOTREADY	0000400		/* something besides NPRTIMO */
#define	VA_DONE		0000200
#define	VA_IENABLE	0000100		/* interrupt enable */
#define	VA_DMAGO	0000010		/* DMA go bit */
#define	VA_DMAGO	0000010		/* DMA go bit */
#define	VA_SUPPLIESLOW	0000004
#define	VA_BOTOFFORM	0000002
#define	VA_BYTEREVERSE	0000001		/* reverse byte order in words */

/* vacsh command bytes */
#define	VAPLOT		0000340
#define	VAPRINT		0000100
#define	VAPRINTPLOT	0000160
#define	VAAUTOSTEP	0000244
#define	VANOAUTOSTEP	0000045
#define	VAFORMFEED	0000263
#define	VASLEW		0000265
#define	VASTEP		0000064

struct va_softc {
	u_char	sc_openf;		/* exclusive open flag */
	u_char	sc_iostate;		/* kind of I/O going on */
#define	VAS_IDLE	0	/* no I/O, free */
#define	VAS_PIO		1	/* programmed I/O */
#define	VAS_DMA		2	/* DMA, block pio */
#define	VAS_WANT	4	/* wakeup when iostate changes */
	short	sc_tocnt;		/* time out counter */
	short	sc_info;		/* csw passed from vaintr */
	int	sc_state;		/* print/plot state of device */
} va_softc[NVA];

#define	VAUNIT(dev)	(minor(dev))

struct	buf rvabuf[NVA];

int	vaprobe(), vaslave(), vaattach(), vadgo();
struct	uba_device *vadinfo[NVA];
struct	uba_ctlr *vaminfo[NVA];
struct	buf vabhdr[NVA];
u_short	vastd[] = { 0764000, 0 };
struct	uba_driver vadriver =
    { vaprobe, vaslave, vaattach, vadgo, vastd, "vz", vadinfo, "va", vaminfo };

vaprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* value-result */
	register struct vadevice *vaaddr = (struct vadevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	vaintr(0);
#endif
#ifndef UCBVAX
	vaaddr->vacsl = VA_IENABLE;
	vaaddr->vaba = 0;
	vaaddr->vacsh = VAPLOT;
	vaaddr->vacsl = VA_IENABLE|VA_DMAGO;
	vaaddr->vawc = -1;
	DELAY(10000);
	vaaddr->vacsl = 0;
	vaaddr->vawc = 0;
#else
	br=0x14;
	cvec=0170;
#endif
	return (sizeof (struct vadevice));
}

/*ARGSUSED*/
vaslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{

	ui->ui_dk = 0;
	return (ui->ui_unit <= 0);
}

/*ARGSUSED*/
vaattach(ui)
	struct uba_device *ui;
{

	ui->ui_mi->um_tab.b_actf = &vabhdr[ui->ui_unit];
}

vaopen(dev)
	dev_t dev;
{
	register struct va_softc *sc;
	register struct vadevice *vaaddr;
	register struct uba_device *ui;
	int error;
	int unit = VAUNIT(dev);

	if (unit >= NVA || (sc = &va_softc[unit])->sc_openf ||
	    (ui = vadinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	vaaddr = (struct vadevice *)ui->ui_addr;
	sc->sc_openf = 1;
	vaaddr->vawc = 0;
	sc->sc_state = 0;
	sc->sc_tocnt = 0;
	sc->sc_iostate = VAS_IDLE;
	vaaddr->vacsl = VA_IENABLE;
	vatimo(dev);
	error = vacmd(dev, VPRINT);
	if (error)
		vaclose(dev);
	return (error);
}

vastrategy(bp)
	register struct buf *bp;
{
	register struct uba_device *ui;
	register struct uba_ctlr *um;
	int s;

	dprintf("vastrategy(%x)\n", bp);
	ui = vadinfo[VAUNIT(bp->b_dev)];
	if (ui == 0 || ui->ui_alive == 0) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	s = spl4();
	um = ui->ui_mi;
	bp->b_actf = NULL;
	if (um->um_tab.b_actf->b_actf == NULL)
		um->um_tab.b_actf->b_actf = bp;
	else {
		printf("bp = 0x%x, um->um_tab.b_actf->b_actf = 0x%x\n",
			bp, um->um_tab.b_actf->b_actf);
		panic("vastrategy");
		um->um_tab.b_actf->b_actl->b_forw = bp;
	}
	um->um_tab.b_actf->b_actl = bp;
	bp = um->um_tab.b_actf;
	dprintf("vastrategy: bp=%x actf=%x active=%d\n",
		bp, bp->b_actf, bp->b_active);
	if (bp->b_actf && bp->b_active == 0)
		(void) vastart(um);
	splx(s);
}

int	vablock = 16384;

unsigned
minvaph(bp)
	struct buf *bp;
{

	if (bp->b_bcount > vablock)
		bp->b_bcount = vablock;
}

/*ARGSUSED*/
vawrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	if (VAUNIT(dev) > NVA)
		return (ENXIO);
	return (physio(vastrategy, &rvabuf[VAUNIT(dev)], dev, B_WRITE,
		    minvaph, uio));
}

vastart(um)
	register struct uba_ctlr *um;
{
	struct buf *bp;
	struct vadevice *vaaddr;
	register struct va_softc *sc;
	int unit;

	dprintf("vastart(%x), bp=%x\n", um, um->um_tab.b_actf->b_actf);
	if ((bp = um->um_tab.b_actf->b_actf) == NULL)
		return;
	unit = VAUNIT(bp->b_dev);
	sc = &va_softc[unit];
	sc->sc_tocnt = 0;
	while (sc->sc_iostate&VAS_PIO) {
		sc->sc_iostate |= VAS_WANT;
		sleep((caddr_t)&sc->sc_iostate, VAPRI);
	}
	sc->sc_iostate |= VAS_DMA;
	vaaddr = (struct vadevice *)um->um_addr;
	vaaddr->vacsl = 0;
	vaaddr->vawc = -(bp->b_bcount / 2);
	um->um_cmd = VA_DMAGO | VA_IENABLE;
	(void) ubago(vadinfo[unit]);
}

vadgo(um)
	register struct uba_ctlr *um;
{
	register struct vadevice *vaaddr = (struct vadevice *)um->um_addr;
	register struct buf *bp;

	bp = um->um_tab.b_actf;
	va_softc[VAUNIT(bp->b_actf->b_dev)].sc_tocnt = 0;
	bp->b_active++;
	vaaddr->vaba = um->um_ubinfo;
	vaaddr->vacsl = ((um->um_ubinfo >> 12) & 0x30) | um->um_cmd;
}

/*ARGSUSED*/
vaioctl(dev, cmd, data, flag)
	register caddr_t data;
{
	register struct va_softc *sc = &va_softc[VAUNIT(dev)];

	switch (cmd) {

	case VGETSTATE:
		*(int *)data = sc->sc_state;
		break;

	case VSETSTATE:
		return (vacmd(dev, *(int *)data));

	default:
		return (ENOTTY);
	}
	return (0);
}

vacmd(dev, vcmd)
	dev_t dev;
	int vcmd;
{
	register struct va_softc *sc = &va_softc[VAUNIT(dev)];
	int s, cmd;

	s = spl4();
	while (sc->sc_iostate&VAS_DMA) {
		sc->sc_iostate |= VAS_WANT;
		sleep((caddr_t)&sc->sc_iostate, VAPRI);
	}
	sc->sc_iostate |= VAS_PIO;
	sc->sc_tocnt = 0;
	cmd = 0;
	switch (vcmd) {

	case VPLOT:
		/* Must turn on plot AND autostep modes. */
		if (vadopio(dev, VAPLOT))
			error = EIO;
		cmd = VAAUTOSTEP;
		break;

	case VPRINT:
		cmd = VAPRINT;
		break;

	case VPRINTPLOT:
		cmd = VAPRINTPLOT;
		break;
	}
	sc->sc_state = (sc->sc_state & ~(VPLOT|VPRINT|VPRINTPLOT)) | vcmd;
	if (cmd && vadopio(dev, cmd))
		error = EIO;
	sc->sc_iostate &= ~VAS_PIO;
	if (sc->sc_iostate&VAS_WANT) {
		sc->sc_iostate &= ~VAS_WANT;
		wakeup((caddr_t)&sc->sc_iostate);
	}
	splx(s);
	return (error);
}

vadopio(dev, cmd)
	dev_t dev;
	int cmd;
{
	register struct vadevice *vaaddr =
	    (struct vadevice *)vaminfo[VAUNIT(dev)]->um_addr;
	register struct va_softc *sc = &va_softc[VAUNIT(dev)];

	sc->sc_info = 0;
	vaaddr->vacsh = cmd;
	while ((sc->sc_info&(VA_DONE|VA_ERROR)) == 0)
		sleep((caddr_t)&sc->sc_info, VAPRI);
	return (sc->sc_info&VA_ERROR);
}

vatimo(dev)
	dev_t dev;
{
	register struct va_softc *sc = &va_softc[VAUNIT(dev)];

	if (sc->sc_openf)
		timeout(vatimo, (caddr_t)dev, hz/2);
	if (++sc->sc_tocnt < 2)
		return;
	sc->sc_tocnt = 0;
	dprintf("vatimo: calling vaintr\n");
	vaintr(dev);
}

/*ARGSUSED*/
vaintr(dev)
	dev_t dev;
{
	register struct uba_ctlr *um;
	struct vadevice *vaaddr;
	struct buf *bp;
	register int unit = VAUNIT(dev), e;
	register struct va_softc *sc = &va_softc[unit];

	um = vaminfo[unit];
	vaaddr = (struct vadevice *)um->um_addr;
	e = vaaddr->vacsw;
	dprintf("vaintr: um=0x%x, e=0x%x, b_active %d\n",
		um, e, um->um_tab.b_actf->b_active);
	if ((e&(VA_DONE|VA_ERROR)) == 0)
		return;
	vaaddr->vacsl = 0;
	if ((e&VA_ERROR) && (e&VA_NPRTIMO))
		printf("va%d: npr timeout\n", unit);
	if (sc->sc_iostate&VAS_PIO) {
		sc->sc_info = e;
		wakeup((caddr_t)&sc->sc_info);
		return;
	}
	if (um->um_tab.b_actf->b_active) {
		bp = um->um_tab.b_actf->b_actf;
		if (e&VA_ERROR)
			bp->b_flags |= B_ERROR;
		if (sc->sc_state&VPRINTPLOT) {
			sc->sc_state = (sc->sc_state & ~VPRINTPLOT) | VPLOT;
			vaaddr->vacsh = VAAUTOSTEP;
			return;
		}
		ubadone(um);
		um->um_tab.b_actf->b_active = 0;
		um->um_tab.b_actf->b_actf = bp->b_forw;
		bp->b_active = 0;
		bp->b_errcnt = 0;
		bp->b_resid = 0;
		iodone(bp);
	}
	if (um->um_tab.b_actf->b_actf == 0) {
		sc->sc_iostate &= ~VAS_DMA;
		if (sc->sc_iostate&VAS_WANT) {
			sc->sc_iostate &= ~VAS_WANT;
			wakeup((caddr_t)&sc->sc_iostate);
		}
		return;
	}
	if (um->um_tab.b_actf->b_active == 0)
		vastart(um);
}

vaclose(dev)
	dev_t dev;
{
	register struct va_softc *sc = &va_softc[VAUNIT(dev)];
	register struct vadevice *vaaddr =
	    (struct vadevice *)vadinfo[VAUNIT(dev)]->ui_addr;

	sc->sc_openf = 0;
	sc->sc_state = 0;
	if (sc->sc_iostate != VAS_IDLE)
		wakeup((caddr_t)&sc->sc_iostate);
	sc->sc_iostate = VAS_IDLE;
	vaaddr->vacsl = 0;
	vaaddr->vawc = 0;
}

vareset(uban)
	int uban;
{
	register int va11;
	register struct uba_ctlr *um;
	register struct vadevice *vaaddr;
	register struct va_softc *sc;

	for (va11 = 0; va11 < NVA; va11++, sc++) {
		if ((um = vaminfo[va11]) == 0 || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		sc = &va_softc[um->um_ctlr];
		if (sc->sc_openf == 0)
			continue;
		printf(" va%d", va11);
		vaaddr = (struct vadevice *)um->um_addr;
		vaaddr->vacsl = VA_IENABLE;
		if (sc->sc_state & VPLOT) {
			vaaddr->vacsh = VAPLOT;
			DELAY(10000);
			vaaddr->vacsh = VAAUTOSTEP;
		} else if (sc->sc_state & VPRINTPLOT)
			vaaddr->vacsh = VPRINTPLOT;
		else
			vaaddr->vacsh = VAPRINTPLOT;
		DELAY(10000);
		sc->sc_iostate = VAS_IDLE;
		um->um_tab.b_actf->b_active = 0;
		um->um_tab.b_actf->b_actf = um->um_tab.b_actf->b_actl = 0;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo >> 28) & 0xf);
			um->um_ubinfo = 0;
		}
		(void) vastart(um);
	}
}

vaselect()
{

	return (1);
}
#endif
