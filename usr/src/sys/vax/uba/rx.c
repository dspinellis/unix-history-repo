/*	rx.c	4.1	83/02/08	*/

#include "rx.h"
#if NFX > 0
/*
 * RX02 floppy disk device driver
 *
 * From drivers by Richard Wales and Bill Shannon
 * (untested, or even compiled)
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/cpu.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/clist.h"
#include "../h/tty.h"
#include "../h/file.h"

#include "../vax/nexus.h"
#include "../vaxuba/ubavar.h"
#include "../vaxuba/ubareg.h"
#include "../vaxuba/rxreg.h"

/* per-controller data */
struct	rx_ctlr {
	int	rxc_state;	/* controller state */
#define	RXS_READ	1	/* read started	*/
#define	RXS_EMPTY	2	/* empty started */
#define	RXS_FILL	3	/* fill started	*/
#define	RXS_WRITE	4	/* write started */
#define	RXS_FORMAT	5	/* format started */
#define	RXS_RDSTAT	6	/* status read started */
#define	RXS_RDERR	7	/* error read started */
	u_short	rxc_rxcs;	/* extended error status */
	u_short	rxc_rxdb;
	u_short	rxc_rxxt[4];
	u_short	rxc_tocnt;	/* for watchdog routine */
#define	RX_MAXTIMEOUT	20	/* # seconds to wait before giving up */
} rx_ctlr[NFX];
struct buf	rrxbuf[NFX];	/* buffer for I/O */
struct buf	erxbuf[NFX];	/* buffer for reading error status */

/* per-drive data */
struct rx_softc {
	int sc_flags;		/* drive status flags */
#define	RXF_DBLDEN	0x01	/* use double density */
#define	RXF_DIRECT	0x02	/* use direct sector mapping */
#define	RXF_TRKZERO	0x04	/* start mapping on track 0 */
#define	RXF_DEVTYPE	0x07	/* density and mapping flags */
#define	RXF_OPEN	0x10	/* open */
#define	RXF_DDMK	0x20	/* deleted-data mark detected */
#define	RXF_USEWDDS	0x40	/* write deleted-data sector */
	int sc_csbits;		/* constant bits for CS register */
	int sc_lsector;		/* logical sector number for I/O */
	int sc_psector;		/* physical sector number for I/O */
	int sc_ptrack;		/* physical track number for I/O */
} rx_softc[NRX];

struct	uba_device *rxdinfo[NRX];
struct	uba_ctlr *rxminfo[NFX];
int rxprobe(), rxslave(), rxattach(), rxdgo(), rxintr();
int rxwatch(), rxphys();
u_short rxstd[] = { 0177170, 0177150, 0 };
struct uba_driver fxdriver =
  { rxprobe, rxslave, rxattach, rxdgo, rxstd, "rx", rxdinfo, "fx", rxminfo };

int	rxwstart;
#define	RXUNIT(dev)	(minor(dev)>>4)

/* a drive's letter name, based on density and sector mapping */
#define	DEVNAME(dev)	('a'+((dev)&DEVTYPE))
#define	UNITNAME(unit)	DEVNAME(sc->sc_flags)

/* constants related to floppy data capacity */
#define	RXSECS	2002				/* # sectors on a floppy */
#define	DDSTATE	(sc->sc_flags&DBLDEN)		/* double-density device? */
#define	NBPS	(DDSTATE ? 256 : 128)		/* # bytes per sector */
#define	NWPS	(DDSTATE ? 128 : 64)		/* # words per sector */
#define	RXSIZE	(DDSTATE ? 512512 : 256256)	/* # bytes per disk */
#define	SECSHFT	(DDSTATE ? 8 : 7)		/* # bits to shift for sctr # */
#define	SECMASK	(DDSTATE ? 0xff : 0x7f)		/* shifted-out bits of offset */

#define	B_CTRL	0x80000000		/* control (format) request */

/*ARGSUSED*/
rxprobe (reg)
	caddr_t reg;
{
	register int br, cvec;		/* value-result */
	struct rxdevice *rxaddr = (struct rxdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
#endif lint
	rxaddr->rxcs = RX_INTR;
	DELAY(10);
	rxaddr->rxcs = 0;
	return (sizeof (*rxaddr));
}

rxslave(ui,reg)
	struct uba_device *ui;
	caddr_t reg;
{

	ui->ui_dk = 1;
	return (ui->ui_slave == 0 || ui->ui_slave == 1);
}

/*ARGSUSED*/
rxattach(ui)
	struct uba_device *ui;
{

}

/*ARGSUSED1*/
rxopen(dev, flag)
	dev_t dev;
	int flag;
{ 
	register int unit = RXUNIT(dev);
	register struct rx_softc *sc;
	register struct uba_device *ui;

	if (unit >= NRX || (minor(dev) & (0xf^DEVTYPE)) ||
	    (ui = rxdinfo[unit]) == 0 || ui->ui_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	sc = &rx_softc[unit];
	if (sc->sc_flags & RXF_OPEN) {
		u.u_error = EBUSY;
		return;
	}
	sc->sc_flags = RXF_OPEN | (minor(dev) & DEVTYPE);
	sc->sc_csbits |= (ui->ui_slave == 1 ? RX_DRV1 : RX_DRV0) |
		 ((minor(dev) & DBLDEN) ? RX_DDEN : RX_SDEN) | RX_INTR;
}

/*ARGSUSED1*/
rxclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct rx_softc *sc = &rx_softc[RXUNIT(dev)];

	sc->sc_flags &= ~RXF_OPEN;
	sc->sc_csbits = 0;
}

rxstrategy(bp)
	register struct buf *bp;
{
	struct uba_device *ui;
	register struct rx_softc *sc;
	register struct uba_ctlr *um;

	ui = rxdinfo[RXUNIT(bp->b_dev)];
	if (ui == 0 || ui->ui_alive == 0) {
		bp->b_flags |= B_ERROR;
		iodone(bp);
		return;
	}
	s = spl5();
	um = ui->ui_mi;
	bp->b_actf = NULL;
	if (um->um_tab.b_actf->b_actf == NULL)
		um->um_tab.b_actf->b_actf = bp;
	else
		um->um_tab.b_actf->b_actl->b_forw = bp;
	um->um_tab.b_actf->b_actl = bp;
	bp = um->um_tab.b_actf;
	if (bp->b_actf && bp->b_active == 0)
		(void) rxstart(um);
	splx(s);
}

/*
 * Sector mapping routine.
 * Two independent sets of choices are available:
 *
 * (a) The first logical sector may either be on track 1 or track 0.
 * (b) The sectors on a track may either be taken in 2-for-1 interleaved
 *	 fashion or directly.
 * This gives a total of four possible mapping schemes.
 *
 * Physical tracks on the RX02 are numbered 0-76.  Physical sectors on
 * each track are numbered 1-26.
 *
 * When interleaving is used, sectors on the first logical track are
 * taken in the order 1, 3, 5, ..., 25, 2, 4, 6, ..., 26.  A skew of
 * six sectors per track is also used (to allow time for the heads to
 * move); hence, the sectors on the second logical track are taken in
 * the order 7, 9, 11, ..., 25, 1, 3, 5, 8, 10, 12, ..., 26, 2, 4, 6;
 * the third logical track starts with sector 13; and so on.
 *
 * When the mapping starts with track 1, track 0 is the last logical
 * track, and this track is always handled directly (without inter-
 * leaving), even when the rest of the disk is interleaved.  (This is
 * still compatible with DEC RT-11, which does not use track 0 at all.)
 */
rxmap(sc)
	register struct rx_softc *sc;
{
	register int lt, ls, ptoff;

	lt = sc->sc_lsector / 26;
	ls = sc->sc_lsector % 26;
	/*
	 * The "physical track offset" (ptoff) takes the
	 * starting physical track (0 or 1) and the desired
	 * interleaving into account.  If lt+ptoff >= 77,
	 * then interleaving is not performed.
	 */
	ptoff = 0;
	if (sc->sc_flags&DIRECT)
		ptoff += 77;
	if (sc->sc_flags&TRKZERO)
		ptoff++;
	if (lt + ptoff < 77)
		ls = ((ls << 1) + (ls >= 13) + (6*lt)) % 26;
	sc->sc_ptrack = (lt + ptoff) % 77;
	sc->sc_psector = ls + 1;
}

rxstart(um)
	register struct uba_ctlr *um;
{
	register struct rxdevice *rxaddr;
	register struct rx_ctlr *rxc;
	register struct rx_softc *sc;
	struct buf *bp;
	int unit;

	if ((bp = um->um_tab.b_actf->b_actf) == NULL)
		return;
	unit = RXUNIT(bp->b_dev);
	sc = &rx_softc[unit];
	sc->sc_tocnt = 0;
	rxaddr = (struct rxdevice *)um->um_addr;
	rxc = &rx_ctlr[um->um_ctlr];
	if (bp->b_flags&B_CTRL) {		/* format */
		rxc->rxc_state = RXS_FORMAT;
		rxaddr->rxcs = RX_FORMAT | sc->sc_csbits;
		while ((rxaddr->rxcs&RXTREQ) == 0)
			;
		rxaddr->rxdb = 'I';
		return;
	}
	if (bp->b_flags&B_READ) {		/* read */
		rxc->rxc_state = RXS_READ;
		rxaddr->rxcs = RX_READ | sc->sc_csbits;
		while ((rxaddr->rxcs&RXTREQ) == 0)
			;
		rxaddr->rxdb = sc->sc_psector;
		while ((rxaddr->rxcs&RXTREQ) == 0)
			;
		rxaddr->rxdb = sc->sc_ptrack;
		return;
	}
	/* write */
	rxc->rxc_state = RXS_FILL;
	um->um_cmd = RX_FILL;
	(void) ubago(rxdinfo[unit]);
}

rxdgo(um)
	struct uba_ctlr *um;
{
	register struct rxdevice *rxaddr = (struct rxdevice *)um->um_addr;
	int ubinfo = um->um_ubinfo;
	struct buf *bp = &rrxbuf[um->um_ctlr];
	struct rx_softc *sc = &rx_softc[RXUNIT(bp->b_dev)];
	struct rx_ctlr *rxc = &rx_ctlr[um->um_ctlr];

	bp = um->um_tab.b_actf;
	rx_softc[RXUNIT(bp->b_actf->b_dev)].sc_tocnt = 0;
	bp->b_active++;
	if (rxc->rxc_state != RXS_RDERR) {
		while ((rxaddr->rxcs&RXTREQ) == 0)
			;
		rxaddr->rxdb = bp->b_count >> 1;
	}
	while ((rxaddr->rxcs&RXTREQ) == 0)
		;
	rxaddr->rxdb = ubinfo & 0xffff;
	rxaddr->rxcs = um->um_cmd | ((ubinfo & 0x30000) >> 4) | sc->sc_csbits;
}

rxintr(ctlr)
	int ctlr;
{
	struct uba_ctlr *um = rxminfo[ctlr];
	register struct rxdevice *rxaddr = (struct rxdevice *)um->um_addr;
	register struct buf *bp = &rrxbuf[ctlr];
	int unit = RXUNIT(bp->b_dev);
	register struct rx_softc *sc = &rx_softc[unit];
	struct uba_device *ui = rxdinfo[unit];
	struct rxerr *er = &rxerr[ctlr];
	register struct rx_ctlr *rxc = &rx_ctlr[ctlr];
	register struct rxdeferr *ed;

	if (um->um_tab.b_actf->b_active) {
		bp = um->um_tab.b_actf->b_actf;
		if ((rxaddr->rxcs & RXERR) &&
		    rxc->rxc_state != RXS_RDSTAT && rxc->rxc_state != RXS_RDERR)
			bp->b_flags |= B_ERROR;
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
		rxstart(um);
	switch (rxc->rxc_state) {

	case RXS_READ:
		if (rxaddr->rxdb & RXES_DDMARK)
			sc->sc_flags |= RXF_DDMK;
		else
			sc->sc_flags &= ~RXF_DDMK;
		rxc->rxc_state = RXS_EMPTY;
		um->um_cmd = RX_EMPTY;
		(void) ubago(ui);
		return;

	case RXS_FILL:
		rxc->rxc_state = RXS_WRITE;
		if (sc->sc_flags & RXF_USEWDDS) {
			rxaddr->rxcs = RX_WDDS | sc->sc_csbits;
			sc->sc_flags &= ~RXF_USEWDDS;
		} else
			rxaddr->rxcs = RX_WRITE | sc->sc_csbits;
		while ((rxaddr->rxcs&RXTREQ) == 0)
			;
		rxaddr->rxdb = sc->sc_psector;
		while ((rxaddr->rxcs&RXTREQ) == 0)
			;
		rxaddr->rxdb = sc->sc_ptrack;
		return;

	case RXS_EMPTY:
	case RXS_WRITE:
	case RXS_FORMAT:
		bp->b_resid = 0;
		break;

	case RXS_RDSTAT:
		if (rxaddr->rxdb&RX_READY)
			goto rderr;
		bp->b_error = EBUSY;
		bp->b_flags |= B_ERROR;
		break;

	case RXS_RDERR:
		printf("rx%d%c: hard error, lsn%d (trk %d psec %d) ",
			unit, UNITNAME(unit),
			sc->sc_lsector, sc->sc_ptrack, sc->sc_psector);
		ed = rxdeferr;
		while (; ed->errval && ed->errval != (er->rxxt[0]&0xff); ed++)
			;
		printf ("cs=%b, db=%b, err=%x (%s)\n",
			er->rxcs&0xffff, rxaddr->rxcs_BITS,
			er->rxdb&0x0ffff, RXES_BITS,
			er->rxxt[0]&0xffff, ed->errmsg);
		break;

	default:
		printf ("rx%d: state %d (reset)",
			unit, State);
		rxreset (unit);
		printf("\n");
		return;
	}
done:
	rxc->rxc_state = RXS_IDLE;
	ubadone(um);
	iodone(bp);
	return;

error:
	/*
	 * In case of an error:
	 *  (a) Give up now if a format (ioctl) was in progress, or if a
	 *	  density error was detected.
	 *  (b) Retry up to nine times if a CRC (data) error was detected,
	 *	  then give up if the error persists.
	 *  (c) In all other cases, reinitialize the drive and try the
	 *	  operation once more before giving up.
	 */
	if (rxc->rxc_state == RXS_FORMAT || (rxaddr->rxdb&RX_DENERR))
		goto giveup;
	if (rxaddr->rxdb & RXES_CRCERR) {
		sc->sc_errs++;
		if (++bp->b_errcnt >= 10)
			goto giveup;
		goto retry;
	}
	bp->b_errcnt += 9;
	if (bp->b_errcnt >= 10)
		goto giveup;
	rxaddr->rxcs = RX_INIT;
	/* no way to get an interrupt for "init done", so just wait */
	while ((rxaddr->rxdb&RX_IDONE) == 0)
		;
retry:
	rxstart(ui->ui_mi);
	return;
giveup:
	/*
	 * Hard I/O error --
	 * read the error registers, print them on the console, and give up.
	 * The reason for the "read status" is that I don't want to clutter
	 * the system log with errors for "drive not ready" conditions
	 * (which usually mean that the user tried to do I/O without a disk
	 * in the drive), and the only way to determine whether the drive is
	 * ready is by doing a "read status" before examining RX2ES.
	 * Density errors are not noted on the console either, since the
	 * only way to determine the density of an unknown disk is to try
	 * one density or the other at random and see which one doesn't give
	 * a density error.
	 */
	if (rxaddr->rxdb & RXES_DENERR) {
		bp->b_error = ENODEV;
		bp->b_flags |= B_ERROR;
		goto done;
	}
	rxc->rxc_state = RXS_RDSTAT;
	rxaddr->rxcs = RX_RDSTAT | sc->sc_csbits;
	return;
rderr:
	/*
	 * A hard error (other than not ready or density) has occurred.
	 * Read the extended error status information.
	 * Before doing this, save the current CS and DB register values,
	 * because the read error status operation may modify them.
	 */
	bp->b_error = EIO;
	bp->b_flags |= B_ERROR;
	ubadone(um);
	er->rxcs = rxaddr->rxcs;
	er->rxdb = rxaddr->rxdb;
	bp = &erxbuf[ctlr];
	bp->b_un.b_addr = (caddr_t)er->rxxt;
	bp->b_bcount = sizeof (er->rxxt);
	bp->b_flags &= ~(B_DIRTY|B_UAREA|B_PHYS|B_PAGET);
	um->um_tab.b_actf = bp->b_actf = bp;
	rxc->rxc_state = RXS_RDERR;
	um->um_cmd = RX_RDERR;
	(void) ubago(ui);
}

/*ARGSUSED*/
minrxphys(bp)
	struct buf *bp;
{

	if (bp->b_bcount > NBPS)
		bp->b_bcount = NBPS;
}

rxtimo()
{
	register struct rx_softc *sc = &rx_softc[RXUNIT(dev)];

	if (sc->sc_openf)
		timeout(rxtimo, (caddr_t)dev, hz/2);
	if (++sc->sc_tocnt < RX_MAXTIMEOUT)
		return;
	sc->sc_tocnt = 0;
	rxintr(dev);
}

rxreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register struct rxdevice *rxaddr;
	register int ctlr;

	for (ctlr = 0; ctlr < NFX; ctlr++) {
		if ((um = rxminfo[ctlr]) == 0 || um->um_ubanum != uban ||
		    um->um_alive == 0)
			continue;
		printf(" fx%d", ctlr);
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			ubadone (um);
		}
		rx_ctlr[ctlr].rxc_state = RXS_IDLE;
		rxaddr = (struct rxdevice *)um->um_addr;
		rxaddr->rxcs = RXINIT;
		while ((rxaddr->rxdb&RX_IDONE) == 0)
			;
		(void) rxstart(um);
	}
}

rxread(dev)
	dev_t dev;
{
	int unit = RXUNIT(dev), ctlr = rxdinfo[unit]->ui_ctlr;

	if (u.u_offset + u.u_count > RXSIZE) {
		u.u_error = ENXIO;
		return;
	}
	if (u.u_offset < 0 || (u.u_offset & SECMASK) != 0)
		goto bad;
	physio(rxstrategy, &rrxbuf[ctlr], dev, B_READ, minrxphys);
}

rxwrite(dev)
	dev_t dev;
{
	int unit = RXUNIT(dev), ctlr = rxdinfo[unit]->ui_ctlr;

	if (u.u_offset + u.u_count > RXSIZE) {
		u.u_error = ENXIO;
		return;
	}
	if (u.u_offset < 0 || (u.u_offset & SECMASK) != 0)
		goto bad;
	physio(rxstrategy, &rrxbuf[ctlr], dev, B_WRITE, minrxphys);
}

/*
 * Control routine:
 * processes three kinds of requests:
 *
 *	(1) Set density according to that specified by the open device.
 *	(2) Arrange for the next sector to be written with a deleted-
 *		  data mark.
 *	(3) Report whether the last sector read had a deleted-data mark
 *		  (by returning with an EIO error code if it did).
 *
 * Requests relating to deleted-data marks can be handled right here.
 * A "set density" request, however, must additionally be processed
 * through "rxstart", just like a read or write request.
 *
 * Note that there was a bug in sys/ioctl.c which failed to pass the
 * "flag" argument to the device "ioctl" routine.  This bug had to be
 * fixed to allow "rxioctl" to determine whether the device is open
 * for input or output, since certain control calls are legal only for
 * input or only for output.
 */
/*ARGSUSED3*/
rxioctl(dev, cmd, addr, flag)
	dev_t dev;
	int cmd;
	caddr_t addr;
	int flag;
{   
	int unit = RXUNIT(dev);
	struct rx_softc *sc = &rx_softc[unit];
	register struct buf *bp;
	int ctlr, s;

	switch (cmd) {

	case IO_FORMAT:
		break;

	case IO_WDDS:
		if ((flag&FWRITE) == 0)
			u.u_error = EBADF;
		else
			sc->sc_flags |= RXF_USEWDDS;
		return;

	case IO_RDDSMK:
		if ((flag&FREAD) == 0)
			u.u_error = EBADF;
		else if (sc->sc_flags & RXF_DDMK)
			u.u_error = EIO;
		return;

	default:
		u.u_error = ENXIO;
		return;
	}

	if ((flag&FWRITE) == 0) {
		u.u_error = EBADF;
		return;
	}
	ctl = rxdinfo[unit]->ui_mi->um_ctlr;
	bp = &rrxbuf[ctlr];
	s = spl5();
	while (bp->b_flags & B_BUSY)
		sleep(bp, PRIBIO);
	bp->b_flags = B_BUSY | B_CTRL;
	splx(s);
	bp->b_dev = dev;
	bp->b_error = 0;
	u.u_offset = 0;
	bp->b_bcount = NBPS;
	rxstrategy (bp);
	iowait(bp);
	bp->b_flags &= ~B_BUSY;
	wakeup(bp);
}
#endif
