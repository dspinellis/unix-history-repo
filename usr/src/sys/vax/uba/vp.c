/*	vp.c	4.14	82/05/19	*/

#include "vp.h"
#if NVP > 0
/*
 * Versatec matrix printer/plotter
 * dma interface driver
 *
 * SETUP NOTES:
 *	Set up both print and plot interrupts to go through the same vector
 *	Give the address of the plcsr register in the config specification
 */
#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/ubavar.h"
#include "../h/ubareg.h"
#include "../h/vcmd.h"

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
#if ERNIE || CAD || UCBVAX
	/* UNTIL REWIRED, GET INTERRUPT AT 200 BUT WANT 174 */
	if (cvec == 0200) {
		printf("vp reset vec from 200 to 174\n");
		cvec = 0174;
	}
#endif
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
	    (ui = vpdinfo[VPUNIT(dev)]) == 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	vpaddr = (struct vpdevice *)ui->ui_addr;
	sc->sc_state = VPSC_OPEN|VPSC_PRINT | VP_CLRCOM|VP_RESET;
	sc->sc_count = 0;
	vpaddr->prcsr = VP_IENABLE|VP_DTCINTR;
	vptimo(dev);
	while (sc->sc_state & VPSC_CMNDS) {
		(void) spl4();
		if (vpwait(dev)) {
			vpclose(dev);
			u.u_error = EIO;
			return;
		}
		vpstart(dev);
		(void) spl0();
	}
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
	iodone(bp);
	if (e)
		u.u_error = EIO;
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
vpwrite(dev)
	dev_t dev;
{

	physio(vpstrategy, &rvpbuf[VPUNIT(dev)], dev, B_WRITE, minvpph);
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
	/* I wish i could tell whether an error indicated an npr timeout */
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
vpioctl(dev, cmd, addr, flag)
	dev_t dev;
	int cmd;
	register caddr_t addr;
	int flag;
{
	register int m;
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];
	register struct vpdevice *vpaddr =
	    (struct vpdevice *)vpdinfo[VPUNIT(dev)]->ui_addr;

	switch (cmd) {

	case VGETSTATE:
		(void) suword(addr, sc->sc_state);
		return;

	case VSETSTATE:
		m = fuword(addr);
		if (m == -1) {
			u.u_error = EFAULT;
			return;
		}
		sc->sc_state =
		    (sc->sc_state & ~VPSC_MODE) | (m&(VPSC_MODE|VPSC_CMNDS));
		break;

	default:
		u.u_error = ENOTTY;
		return;
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
		if (sc->sc_ubinfo) {
			printf("<%d>", (sc->sc_ubinfo>>28)&0xf);
			ubarelse(ui->ui_ubanum, &sc->sc_ubinfo);
		}
		sc->sc_count = sc->sc_bp->b_bcount;
		vpstart(sc->sc_bp->b_dev);
	}
}

vpselect()
{
	return (1);
}
#endif
