/*	vp.c	4.26	83/06/16	*/

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
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/map.h"
#include "../h/ioctl.h"
#include "../h/vcmd.h"
#include "../h/uio.h"
#include "../h/kernel.h"

#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/vpreg.h"

int	vpdebug = 0;
#define dprintf if(vpdebug)printf

unsigned minvpph();

struct vp_softc {
	u_char	sc_openf;
	u_char	sc_iostate;
	short	sc_tocnt;
	int	sc_state;
	int	sc_count;
} vp_softc[NVP];

/* sc_state bits */
#define	VPSC_MODE	0000700
#define	VPSC_CMNDS	0000076

/* sc_iostate bits */
#define VPS_IDLE	0
#define VPS_WANT	1
#define VPS_DMA		4

/* sc_tocnt */
#define IDLE 0
#define ACTIVE 1

struct	uba_device *vpdinfo[NVP];

#define	VPUNIT(dev)	(minor(dev))

struct	buf rvpbuf[NVP];

int	vpprobe(), vpslave(), vpattach(), vpdgo();
struct	uba_device *vpdinfo[NVP];
struct	uba_ctlr *vpminfo[NVP];
struct	buf vpbhdr[NVP];
u_short	vpstd[] = { 0777500, 0 };
struct	uba_driver vpdriver =
    { vpprobe, vpslave, vpattach, vpdgo, vpstd, "zv", vpdinfo, "vp", vpminfo };

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
#if ERNIE || CAD 
	/* UNTIL REWIRED, GET INTERRUPT AT 200 BUT WANT 174 */
	if (cvec == 0200) {
		printf("vp reset vec from 200 to 174\n");
		cvec = 0174;
	}
#endif
	return (sizeof (struct vpdevice));
}

/*ARGSUSED*/
vpslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{

	ui->ui_dk = 0;
	return (ui->ui_unit <= 0);
}

/*ARGSUSED*/
vpattach(ui)
	struct uba_device *ui;
{

	/*
	 * To adjust the standard address back to the beginning of
	 * the register field.
	 */
	ui->ui_addr -= 010;
	ui->ui_physaddr -= 010;
	ui->ui_mi->um_tab.b_actf = &vpbhdr[ui->ui_unit];
	ui->ui_mi->um_addr -= 010;
}



vpopen(dev)
	dev_t dev;
{
	register struct vp_softc *sc;
	register struct vpdevice *vpaddr;
	register struct uba_device *ui;
	int unit = VPUNIT(dev);
	int error;

	if (unit >= NVP || ((sc = &vp_softc[unit])->sc_openf) ||
	    (ui = vpdinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	vpaddr = (struct vpdevice *)ui->ui_addr;
	sc->sc_openf = 1;
	sc->sc_state = VPRINT;
	sc->sc_iostate = VPS_IDLE;
	dprintf("vpopen: called\n");
	vpaddr->prcsr = VP_IENABLE|VP_DTCINTR;
	error = vpcmd(dev,VP_CLRCOM|VP_RESET);
	if (error)
		vpclose(dev);
	dprintf("vpopen: returned %d\n",error);
	return (error);
}

vpstrategy(bp)
	register struct buf *bp;
{
	register int s;
	register struct vp_softc *sc = &vp_softc[VPUNIT(bp->b_dev)];
	register struct uba_device *ui = vpdinfo[VPUNIT(bp->b_dev)];
	register struct vpdevice *vpaddr = (struct vpdevice *)ui->ui_addr;
	register struct uba_ctlr *um;

	dprintf("vpstrategy(%x)\n", bp);
	ui = vpdinfo[VPUNIT(bp->b_dev)];
	if (ui == 0 || ui->ui_alive == 0) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	um = ui->ui_mi;
	bp->b_actf = NULL;
	s = spl4();
	if (um->um_tab.b_actf->b_actf == NULL)
		um->um_tab.b_actf->b_actf = bp;
	else {
		printf("bp = 0x%x, um->um_tab.b_actf->b_actf = 0x%x\n",
			bp, um->um_tab.b_actf->b_actf);
		panic("vpstrategy");
		um->um_tab.b_actf->b_actl->b_forw = bp;
	}
	um->um_tab.b_actf->b_actl = bp;
	bp = um->um_tab.b_actf;
	dprintf("vpstrategy: bp=%x actf=%x active=%d\n",
		bp, bp->b_actf, bp->b_active);
	if (bp->b_actf && bp->b_active == 0)
		(void) vpstart(um);
	splx(s);
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

/*
 * Start a DMA transfer to the versatec.
 * Not used for non-DMA commands.
 */
vpstart(um)
	register struct uba_ctlr *um;
{
	register struct vp_softc *sc;
	register struct vpdevice *vpaddr = (struct vpdevice *)um->um_addr;
	register struct buf *bp;
	register int unit;
	short bit;

	dprintf("vpstart(%x), bp=%x\n", um, um->um_tab.b_actf->b_actf);
	if ((bp = um->um_tab.b_actf->b_actf) == NULL)
		return;
	unit = VPUNIT(bp->b_dev);
	sc = &vp_softc[unit];
	if (sc->sc_state&VPRINTPLOT)
		vpaddr->plcsr |= VP_SPP;
	else
		vpaddr->plcsr &= ~VP_SPP;
	sc->sc_iostate |= VPS_DMA;
	sc->sc_count = bp->b_bcount;
	(void) ubago(vpdinfo[unit]);
}

vpdgo(um)
	register struct uba_ctlr *um;
{
	register struct vp_softc *sc;
	register struct vpdevice *vpaddr = (struct vpdevice *)um->um_addr;
	register struct buf *bp;

	bp = um->um_tab.b_actf;
	sc = &vp_softc[VPUNIT(bp->b_actf->b_dev)];
	if (sc->sc_count) {
		dprintf("vpdgo: active\n");
		bp->b_active++;
		sc->sc_tocnt = IDLE;
		vpaddr->pbaddr = um->um_ubinfo;
		vpaddr->pbxaddr = (um->um_ubinfo>>12)&0x30;
		if (sc->sc_state & (VPRINT|VPRINTPLOT))
			vpaddr->prbcr = sc->sc_count;
		else
			vpaddr->plbcr = sc->sc_count;
		vptimo(VPUNIT(bp->b_actf->b_dev));
	}
}

/*ARGSUSED*/
vpioctl(dev, cmd, data, flag)
	register caddr_t data;
{
	register int vcmd;
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];

	dprintf("vpioctl: called with %x\n",*(int *)data);
	switch (cmd) {

	case VGETSTATE:
		*(int *)data = sc->sc_state;
		break;

	case VSETSTATE:
		return (vpcmd(dev, *(int *)data));

	default:
		return (ENOTTY);
	}
	return (0);
}

vpcmd(dev, vcmd)
	dev_t dev;
	int vcmd;
{
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];
	int error = 0;
	int s, cmd, state;

	state = 0;
	cmd = 0;
	dprintf("vpcmd: called on %x with %x\n",dev,vcmd);
	switch (vcmd&VPSC_MODE) {

	case VPRINT:
		/*
		 * Set state to print. This will cause count to be loaded
		 * into the appropriate byte count register next time vpstart
		 * is called.
		 */
		state = VPRINT;
		break;

	case VPLOT:
		/*
		 * Set state to plot. This will cause count to be loaded
		 * into the appropriate byte count register next time vpstart
		 * is called.
		 */
		state = VPLOT;
		break;

	case VPRINTPLOT:
		state = VPRINTPLOT;
		break;
	}
	dprintf("vpcmd: state=%x, vcmd=%x\n", state, vcmd&VPSC_CMNDS);
	if (state)
		sc->sc_state = (sc->sc_state & ~(VPLOT|VPRINT|VPRINTPLOT))
			| state;
	if (vcmd&VPSC_CMNDS) {
		while (sc->sc_iostate&VPS_DMA) {
			sc->sc_iostate |= VPS_WANT;
			sleep((caddr_t)&sc->sc_iostate, VPPRI);
		}
		cmd = vcmd & VPSC_CMNDS;
		if (cmd && vpdopio(dev, cmd))
			error = EIO;
		splx(s);
		return (error);
	}
	return (0);
}

/*
 * Perform non-DMA commands.
 */
vpdopio(dev, cmd)
	dev_t dev;
	int cmd;
{
	register struct vpdevice *vpaddr =
	    (struct vpdevice *)vpminfo[VPUNIT(dev)]->um_addr;
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];
	register int gerror, bit;

	dprintf("vpdio: called on %x with %x\n",dev,cmd);
	gerror = 0;
	for (bit = 1; bit != 0; bit <<= 1) {
		if (cmd&bit&VPSC_CMNDS) {
			if (sc->sc_state & VPLOT)
				vpaddr->plcsr |= bit;
			else
				vpaddr->prcsr |= bit;
			cmd &= ~bit;
			dprintf("vpdio: wait for %x\n",bit);
			while ((vpaddr->prcsr&(VP_READY|VP_ERROR)) == 0)
				;
			gerror |= vpaddr->prcsr&VP_ERROR;
		}
	}
	dprintf("vpdio: done\n");
	return (gerror);
}

vptimo(dev)
	dev_t dev;
{
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];

	if (sc->sc_openf == 0)
		return;

	if (sc->sc_tocnt ==  IDLE)
	{
		sc->sc_tocnt = ACTIVE;
		timeout(vptimo, (caddr_t)dev, hz/2);
		return;
	}
	/*
	 * Timer timed out
	 */
	sc->sc_tocnt = IDLE;
	dprintf("vptimo: calling vpintr\n");
	/* 
	 * lost interrupt
	 */
	vpintr(dev);
}


/*ARGSUSED*/
vpintr(dev)
	dev_t dev;
{
	register struct uba_ctlr *um;
	struct vpdevice *vpaddr;
	struct buf *bp;
	register int unit = VPUNIT(dev), e;
	register struct vp_softc *sc = &vp_softc[unit];

	sc->sc_tocnt = IDLE;
	untimeout(vptimo, (caddr_t)dev);
	um = vpminfo[unit];
	vpaddr = (struct vpdevice *)um->um_addr;
	e = (sc->sc_state & VPLOT) ? vpaddr->plcsr : vpaddr->prcsr;
	dprintf("vpintr: um=0x%x, e=0x%x, b_active %d\n",
		um, e, um->um_tab.b_actf->b_active);
	if ((e&(VP_READY|VP_ERROR)) == 0)
		return;
	if (um->um_tab.b_actf->b_active) {
		bp = um->um_tab.b_actf->b_actf;
		if (e&VP_ERROR)
			bp->b_flags |= B_ERROR;
		/* Ok, print done, next must be plot */
		if (sc->sc_state&VPRINTPLOT)
			sc->sc_state = (sc->sc_state & ~VPRINTPLOT) | VPLOT;
		ubadone(um);
		um->um_tab.b_actf->b_active = 0;
		um->um_tab.b_actf->b_actf = bp->b_forw;
		bp->b_active = 0;
		bp->b_errcnt = 0;
		bp->b_resid = 0;
		sc->sc_count = 0;
		iodone(bp);
		dprintf("vpintr: iodone\n");
	} else
		return;
	if (um->um_tab.b_actf->b_actf == 0) {
		sc->sc_iostate &= ~VPS_DMA;
		if (sc->sc_iostate&VPS_WANT) {
			sc->sc_iostate &= ~VPS_WANT;
			wakeup((caddr_t)&sc->sc_iostate);
		}
		return;
	}
	if (um->um_tab.b_actf->b_active == 0)
		vpstart(um);
}

vpclose(dev)
	dev_t dev;
{
	register struct vp_softc *sc = &vp_softc[VPUNIT(dev)];
	register struct vpdevice *vpaddr =
	    (struct vpdevice *)vpdinfo[VPUNIT(dev)]->ui_addr;

	dprintf("vpclose: called on %x\n",dev);
	while (sc->sc_iostate != VPS_IDLE)
		sleep((caddr_t)&sc->sc_iostate, VPPRI);
	sc->sc_openf = 0;
	sc->sc_state = 0;
	vpaddr->plcsr = 0;
	vpaddr->prcsr = 0;
}

vpreset(uban)
	int uban;
{
	register int vp11;
	register struct uba_ctlr *um;
	register struct vpdevice *vpaddr;
	register struct vp_softc *sc;

	dprintf("vpreset: called\n");
	for (vp11 = 0; vp11 < NVP; vp11++, sc++) {
		if ((um = vpminfo[vp11]) == 0 || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		sc = &vp_softc[um->um_ctlr];
		if (sc->sc_openf == 0)
			continue;
		printf(" vp%d", vp11);
		vpaddr = (struct vpdevice *)um->um_addr;
		vpaddr->prbcr = 0;
		vpaddr->plbcr = 0;
		vpaddr->prcsr = VP_IENABLE|VP_DTCINTR;
		sc->sc_iostate = VPS_IDLE;
		um->um_tab.b_actf->b_active = 0;
/*
		um->um_tab.b_actf->b_actf = um->um_tab.b_actf->b_actl = 0;
*/
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo >> 28) & 0xf);
			um->um_ubinfo = 0;
		}
		(void) vpstart(um);
	}
}

vpselect()
{
	return (1);
}
#endif
