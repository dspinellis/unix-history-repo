/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)vp.c	7.4 (Berkeley) 4/3/90
 */

#include "vp.h"
#if NVP > 0
/*
 * Versatec matrix printer/plotter
 * dma interface driver
 *
 * SETUP NOTES:
 *	Set up both print and plot interrupts to go through the same vector
 *	(or kludge probe to reset second vector to first;
 *	default 174/200 is already handled).
 *	Give the address of the plcsr register in the config specification
 */
#include "machine/pte.h"

#include "param.h"
#include "user.h"
#include "buf.h"
#include "systm.h"
#include "map.h"
#include "ioctl.h"
#include "vcmd.h"
#include "uio.h"
#include "kernel.h"

#include "ubavar.h"
#include "ubareg.h"

unsigned minvpph();

#define	VPPRI	(PZERO-1)

struct	vpdevice {
	short	plbcr;
	short	pbxaddr;
	short	prbcr;
	u_short pbaddr;
	short	plcsr;
	short	plbuf;
	short	prcsr;
	u_short prbuf;
};

#define	VP_ERROR	0100000
#define	VP_DTCINTR	0040000
#define	VP_DMAACT	0020000
#define	VP_READY	0000200
#define	VP_IENABLE	0000100
#define	VP_TERMCOM	0000040
#define	VP_FFCOM	0000020
#define	VP_EOTCOM	0000010
#define	VP_CLRCOM	0000004
#define	VP_RESET	0000002
#define	VP_SPP		0000001

struct vp_softc {
	int	sc_state;
	int	sc_count;
	int	sc_bufp;
	struct	buf *sc_bp;
	int	sc_ubinfo;
} vp_softc[NVP];

/* sc_state bits */
#define	VPSC_BUSY	0001000
#define	VPSC_MODE	0000700
#define	VPSC_SPP	0000400
#define	VPSC_PLOT	0000200
#define	VPSC_PRINT	0000100
#define	VPSC_CMNDS	0000076
#define	VPSC_OPEN	0000001

struct	uba_device *vpdinfo[NVP];

#define	VPUNIT(dev)	(minor(dev))

struct	buf rvpbuf[NVP];

int	vpprobe(), vpattach();
struct	uba_device *vpdinfo[NVP];
u_short	vpstd[] = { 0777500, 0 };
struct	uba_driver vpdriver =
    { vpprobe, 0, vpattach, 0, vpstd, "vp", vpdinfo };

vpprobe(reg)
	caddr_t reg;
{
	register int br, cvec;		/* value-result */
	register struct vpdevice *vpaddr = (struct vpdevice *)(reg-010);

#ifdef lint
	br = 0; cvec = br; br = cvec;
	vpintr(0);
#endif
	vpaddr->prcsr = VP_IENABLE|VP_DTCINTR;
	vpaddr->pbaddr = 0;
	vpaddr->pbxaddr = 0;
	vpaddr->prbcr = 1;
	DELAY(10000);
	vpaddr->prcsr = 0;
	/* GET INTERRUPT AT SECOND VECTOR BUT WANT FIRST */
	if (cvec == 0200) {
		printf("vp reset vec from 200 to 174\n");
		cvec = 0174;
	}
	return (sizeof (struct vpdevice));
}

/*ARGSUSED*/
vpattach(ui)
	struct uba_device *ui;
{

	ui->ui_addr -= 010;
	ui->ui_physaddr -= 010;
}

vpopen(dev)
	dev_t dev;
{
	register struct vp_softc *sc;
	register struct vpdevice *vpaddr;
	register struct uba_device *ui;

	if (VPUNIT(dev) >= NVP ||
	    ((sc = &vp_softc[minor(dev)])->sc_state&VPSC_OPEN) ||
	    (ui = vpdinfo[VPUNIT(dev)]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	vpaddr = (struct vpdevice *)ui->ui_addr;
	sc->sc_state = VPSC_OPEN|VPSC_PRINT | VP_CLRCOM|VP_RESET;
	sc->sc_count = 0;
	vpaddr->prcsr = VP_IENABLE|VP_DTCINTR;
	vptimo(dev);
	while (sc->sc_state & VPSC_CMNDS) {
		(void) spl4();
		if (vpwait(dev)) {
			(void) vpclose(dev);
			return (EIO);
		}
		vpstart(dev);
		(void) spl0();
	}
	return (0);
}

vpstrategy(bp)
	register struct buf *bp;
{
	register int e;
	register struct vp_softc *sc = &vp_softc[VPUNIT(bp->b_dev)];
	register struct uba_device *ui = vpdinfo[VPUNIT(bp->b_dev)];
	register struct vpdevice *vpaddr = (struct vpdevice *)ui->ui_addr;

	(void) spl4();
	while (sc->sc_state & VPSC_BUSY)
		sleep((caddr_t)sc, VPPRI);
	sc->sc_state |= VPSC_BUSY;
	sc->sc_bp = bp;
	sc->sc_ubinfo = ubasetup(ui->ui_ubanum, bp, UBA_NEEDBDP);
	if (e = vpwait(bp->b_dev))
		goto brkout;
	sc->sc_count = bp->b_bcount;
	vpstart(bp->b_dev);
	while (((sc->sc_state&VPSC_PLOT) ? vpaddr->plcsr : vpaddr->prcsr) & VP_DMAACT)
		sleep((caddr_t)sc, VPPRI);
	sc->sc_count = 0;
	if ((sc->sc_state&VPSC_MODE) == VPSC_SPP)
		sc->sc_state = (sc->sc_state &~ VPSC_MODE) | VPSC_PLOT;
	(void) spl0();
brkout:
	ubarelse(ui->ui_ubanum, &sc->sc_ubinfo);
	sc->sc_state &= ~VPSC_BUSY;
	sc->sc_bp = 0;
	if (e)
		bp->b_flags |= B_ERROR;
	iodone(bp);
	wakeup((caddr_t)sc);
}

int	vpblock = 16384;

unsigned
minvpph(bp)
	struct buf *bp;
{

	if (bp->b_bcount > vpblock)
		bp->b_bcount = vpblock;
}

/*ARGSUSED*/
vpwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{

	if (VPUNIT(dev) >= NVP)
		return (ENXIO);
	return (physio(vpstrategy, &rvpbuf[VPUNIT(dev)], dev, B_WRITE,
		    minvpph, uio));
}

vpwait(dev)
	dev_t dev;
{
	register struct vpdevice *vpaddr =
	    (struct vpdevice *)vpdinfo[VPUNIT(dev)]->ui_addr;
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];
	register int e;

	for (;;) {
		e = (sc->sc_state & VPSC_PLOT) ? vpaddr->plcsr : vpaddr->prcsr;
		if (e & (VP_READY|VP_ERROR))
			break;
		sleep((caddr_t)sc, VPPRI);
	}
	/* I WISH I COULD TELL WHETHER AN ERROR INDICATED AN NPR TIMEOUT */
	return (e & VP_ERROR);
}

vpstart(dev)
	dev_t;
{
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];
	register struct vpdevice *vpaddr =
	    (struct vpdevice *)vpdinfo[VPUNIT(dev)]->ui_addr;
	short bit;

	if (sc->sc_count) {
		vpaddr->pbaddr = sc->sc_ubinfo;
		vpaddr->pbxaddr = (sc->sc_ubinfo>>12)&0x30;
		if (sc->sc_state & (VPSC_PRINT|VPSC_SPP))
			vpaddr->prbcr = sc->sc_count;
		else
			vpaddr->plbcr = sc->sc_count;
		return;
	}
	for (bit = 1; bit != 0; bit <<= 1)
		if (sc->sc_state&bit&VPSC_CMNDS) {
			vpaddr->plcsr |= bit;
			sc->sc_state &= ~bit;
			return;
		}
}

/*ARGSUSED*/
vpioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	register caddr_t data;
	int flag;
{
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];
	register struct vpdevice *vpaddr =
	    (struct vpdevice *)vpdinfo[VPUNIT(dev)]->ui_addr;

	switch (cmd) {

	case VGETSTATE:
		*(int *)data = sc->sc_state;
		return (0);

	case VSETSTATE:
		sc->sc_state =
		    (sc->sc_state & ~VPSC_MODE) |
		    ((*(int *)data) & (VPSC_MODE|VPSC_CMNDS));
		break;

	default:
		return (ENOTTY);
	}
	(void) spl4();
	(void) vpwait(dev);
	if (sc->sc_state&VPSC_SPP)
		vpaddr->plcsr |= VP_SPP;
	else
		vpaddr->plcsr &= ~VP_SPP;
	sc->sc_count = 0;
	while (sc->sc_state & VPSC_CMNDS) {
		(void) vpwait(dev);
		vpstart(dev);
	}
	(void) spl0();
	return (0);
}

vptimo(dev)
	dev_t dev;
{
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];

	if (sc->sc_state&VPSC_OPEN)
		timeout(vptimo, (caddr_t)dev, hz/10);
	vpintr(dev);
}

/*ARGSUSED*/
vpintr(dev)
	dev_t dev;
{
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];

	wakeup((caddr_t)sc);
}

vpclose(dev)
	dev_t dev;
{
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];
	register struct vpdevice *vpaddr =
	    (struct vpdevice *)vpdinfo[VPUNIT(dev)]->ui_addr;

	sc->sc_state = 0;
	sc->sc_count = 0;
	vpaddr->plcsr = 0;
	return (0);
}

vpreset(uban)
	int uban;
{
	register int vp11;
	register struct uba_device *ui;
	register struct vp_softc *sc = vp_softc;
	register struct vpdevice *vpaddr;

	for (vp11 = 0; vp11 < NVP; vp11++, sc++) {
		if ((ui = vpdinfo[vp11]) == 0 || ui->ui_alive == 0 ||
		    ui->ui_ubanum != uban || (sc->sc_state&VPSC_OPEN) == 0)
			continue;
		printf(" vp%d", vp11);
		vpaddr = (struct vpdevice *)ui->ui_addr;
		vpaddr->prcsr = VP_IENABLE|VP_DTCINTR;
		if ((sc->sc_state & VPSC_BUSY) == 0)
			continue;
		sc->sc_ubinfo = 0;
		sc->sc_count = sc->sc_bp->b_bcount;
		vpstart(sc->sc_bp->b_dev);
	}
}

vpselect()
{
	return (1);
}
#endif
