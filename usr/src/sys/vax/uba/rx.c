/*	rx.c	4.9	83/03/29	*/

#include "rx.h"
#if NFX > 0
/*
 * RX02 floppy disk device driver
 *
 */

/*
 * TODO:
 *    - Make it possible to access blocks containing less than
 *	512 bytes properly.
 *
 * 	Note: If the drive subsystem is
 * 	powered off at boot time, the controller won't interrupt!
 */

#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/buf.h"
#include "../h/systm.h"
#include "../h/conf.h"
#include "../h/errno.h"
#include "../h/time.h"
#include "../h/kernel.h"
#include "../h/uio.h"
#include "../h/file.h"

#include "../vax/cpu.h"
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
#define RXS_IDLE	8	/* device is idle */
	u_short	rxc_rxcs;	/* extended error status */
	u_short	rxc_rxdb;
	u_short	rxc_rxxt[4];
	int	rxc_tocnt;	/* for watchdog routine */
#define	RX_MAXTIMEOUT	30	/* # seconds to wait before giving up */
} rx_ctlr[NFX];

/* per-drive buffers */
struct buf	rrxbuf[NRX];	/* buffers for raw I/O */
struct buf	erxbuf[NRX];	/* buffers for reading error status */
struct buf	rxutab[NRX];	/* per drive buffer queue heads */

/* per-drive data */
struct rx_softc {
	int	sc_flags;	/* drive status flags */
#define	RXF_DBLDEN	0x01	/* use double density */
#define	RXF_DIRECT	0x02	/* use direct sector mapping */
#define	RXF_TRKZERO	0x04	/* start mapping on track 0 */
#define	RXF_DEVTYPE	0x07	/* density and mapping flags */
#define	RXF_OPEN	0x10	/* open */
#define	RXF_DDMK	0x20	/* deleted-data mark detected */
#define	RXF_USEWDDS	0x40	/* write deleted-data sector */
	int	sc_csbits;	/* constant bits for CS register */
	caddr_t	sc_uaddr;	/* save orig. unibus address while */
				/* doing multisector transfers */
	long	sc_bcnt;	/* save total transfer count for */
				/* multisector transfers */
} rx_softc[NRX];

struct rxerr {
	short	rxcs;
	short	rxdb;
	short	rxxt[4];	/* error code dump from controller */
} rxerr[NRX];
/* End of per-drive data */

struct	uba_device *rxdinfo[NRX];
struct	uba_ctlr *rxminfo[NFX];
int rxprobe(), rxslave(), rxattach(), rxdgo(), rxintr(), rxwatch(), rxphys();
u_short rxstd[] = { 0177170, 0177150, 0 };
struct uba_driver fxdriver =
  { rxprobe, rxslave, rxattach, rxdgo, rxstd, "rx", rxdinfo, "fx", rxminfo };

int	rxwstart;
#define	RXUNIT(dev)	(minor(dev)>>3)
#define MASKREG(reg)	(reg&0xffff)

/* constants related to floppy data capacity */
#define	RXSECS	2002				/* # sectors on a floppy */
#define	DDSTATE	(sc->sc_flags&RXF_DBLDEN)
#define	NBPS	(DDSTATE ? 256 : 128)		/* # bytes per sector */
#define	NWPS	(DDSTATE ? 128 : 64)		/* # words per sector */
#define	RXSIZE	(DDSTATE ? 512512 : 256256)	/* # bytes per disk */
#define	SECSHFT	(DDSTATE ? 8 : 7)		/* # bits to shift for sctr # */
#define	SECMASK	(DDSTATE ? 0xff : 0x7f)		/* shifted-out bits of offset */

#define	B_CTRL	0x80000000			/* control (format) request */

/*ARGSUSED*/
rxprobe (reg)
	caddr_t reg;
{
	register int br, cvec;			/* value-result */
	struct rxdevice *rxaddr = (struct rxdevice *)reg;

#ifdef lint
	br = 0; cvec = br; br = cvec;
	rxintr(0);
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
	struct rx_ctlr *rxc;
	int ctlr;

	if (unit >= NRX || (ui = rxdinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	sc = &rx_softc[unit];
	if (sc->sc_flags & RXF_OPEN)
		return (EBUSY);
	ctlr = ui->ui_mi->um_ctlr;
	rxc = &rx_ctlr[ctlr];
	sc->sc_flags = RXF_OPEN | (minor(dev) & RXF_DEVTYPE);
	sc->sc_csbits = RX_INTR;
	sc->sc_csbits |= ui->ui_slave == 0 ? RX_DRV0 : RX_DRV1;
	sc->sc_csbits |= minor(dev) & RXF_DBLDEN ? RX_DDEN : RX_SDEN;
	rxc->rxc_tocnt = 0;
	if (rxwstart == 0) {
		rxtimo(ctlr);				/* start watchdog */
		rxwstart++;
	}
	return (0);
}

/*ARGSUSED1*/
rxclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct rx_softc *sc = &rx_softc[RXUNIT(dev)];

	sc->sc_flags &= ~RXF_OPEN;
	sc->sc_csbits = 0;
	rxwstart = 0;
}

rxstrategy(bp)
	register struct buf *bp;
{
	struct uba_device *ui;
	register struct uba_ctlr *um;
	register struct buf *dp;
	struct rx_softc *sc;
	int s, unit = RXUNIT(bp->b_dev);

	ui = rxdinfo[unit];
	sc = &rx_softc[unit];
	if (ui == 0 || ui->ui_alive == 0) 
		goto bad;
	um = ui->ui_mi;
	if (bp->b_blkno < 0 || (bp->b_blkno * DEV_BSIZE) > RXSIZE )
		goto bad;
	s = spl5();
	dp = &rxutab[ui->ui_unit];
	disksort(dp, bp);
	rxustart(ui);
	bp->b_resid = bp->b_bcount;
	sc->sc_uaddr = bp->b_un.b_addr;
	sc->sc_bcnt = bp->b_bcount;
	rxstart(um);
	splx(s);
	return;

bad:	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

/*
 * Unit start routine.
 * Put this unit on the ready queue for the controller,
 * unless it is already there.
 */
rxustart(ui)
	register struct uba_device *ui;
{
	struct buf *dp;
	struct uba_ctlr *um;

	dp = &rxutab[ui->ui_unit];
	um = ui->ui_mi;

	if (!dp->b_active) {
		dp->b_forw = NULL;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else
			um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
		dp->b_active++;
	}
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
rxmap(bp, psector, ptrack)
	struct buf *bp;
	int *psector, *ptrack;
{
	register int lt, ls, ptoff;
	struct rx_softc *sc = &rx_softc[RXUNIT(bp->b_dev)];

	ls = ( bp->b_blkno * DEV_BSIZE + ( sc->sc_bcnt - bp->b_resid )) / NBPS;
	lt = ls / 26;
	ls %= 26;
	/*
	 * The "physical track offset" (ptoff) takes the
	 * starting physical track (0 or 1) and the desired
	 * interleaving into account.  If lt+ptoff >= 77,
	 * then interleaving is not performed.
	 */
	ptoff = 0;
	if (sc->sc_flags&RXF_DIRECT)
		ptoff = 77;
	if (!(sc->sc_flags&RXF_TRKZERO))
		ptoff++;
	if (lt + ptoff < 77)
		ls = ((ls << 1) + (ls >= 13) + (6*lt)) % 26;
	*ptrack = (lt + ptoff) % 77;
	*psector = ls + 1;
}

/*
 * Controller start routine
 */
rxstart(um)
	register struct uba_ctlr *um;
{
	register struct rxdevice *rxaddr;
	register struct rx_ctlr *rxc;
	register struct rx_softc *sc;
	struct buf *dp, *bp;
	int unit, sector, track;

	if (um->um_tab.b_active)
		return;
loop:
	if ((dp = um->um_tab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	um->um_tab.b_active++;
	unit = RXUNIT(bp->b_dev);
	sc = &rx_softc[unit];
	rxaddr = (struct rxdevice *)um->um_addr;
	rxc = &rx_ctlr[um->um_ctlr];
	bp->b_bcount = bp->b_resid;
	if (bp->b_bcount > NBPS)
		bp->b_bcount = NBPS;
	rxc->rxc_tocnt = 0;
	if (rxaddr->rxcs == 0x800) {
		/*
		 * Simulated check for 'volume valid', check
		 * if the drive unit has been powered down
		 */
		rxaddr->rxcs = RX_INIT;
		while((rxaddr->rxcs&RX_DONE) == 0)
			;
	}
	if (bp->b_flags&B_CTRL) {				/* format */
		rxc->rxc_state = RXS_FORMAT;
		rxaddr->rxcs = RX_FORMAT | sc->sc_csbits;
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = 'I';
		return;
	}

	if (bp->b_flags&B_READ) {
		rxmap(bp, &sector, &track);			/* read */
		rxc->rxc_state = RXS_READ;
		rxaddr->rxcs = RX_READ | sc->sc_csbits;
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = (u_short)sector;
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = (u_short)track;
	} else {
		rxc->rxc_state = RXS_FILL;			/* write */
		um->um_cmd = RX_FILL;
		(void) ubago(rxdinfo[unit]);
	}
#ifdef RXDEBUG
	printf("rxstart: flgs=0x%x, unit=%d, tr=%d, sc=%d, bl=%d, cnt=%d\n", 
		bp->b_flags, unit, track, sector, bp->b_blkno, bp->b_bcount);
#endif
}

rxdgo(um)
	struct uba_ctlr *um;
{
	register struct rxdevice *rxaddr = (struct rxdevice *)um->um_addr;
	int ubinfo = um->um_ubinfo;
	struct buf *bp = um->um_tab.b_actf->b_actf;
	struct rx_softc *sc = &rx_softc[RXUNIT(bp->b_dev)];
	struct rx_ctlr *rxc = &rx_ctlr[um->um_ctlr];

	rxaddr->rxcs = um->um_cmd | ((ubinfo & 0x30000) >> 4) | sc->sc_csbits;
	if (rxc->rxc_state != RXS_RDERR) {
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = (u_short) bp->b_bcount >> 1;
	}
	while ((rxaddr->rxcs&RX_TREQ) == 0)
		;
	rxaddr->rxdb = (u_short) ubinfo;
}

rxintr(ctlr)
	int ctlr;
{
	int unit, sector, track;
	struct uba_ctlr *um = rxminfo[ctlr];
	register struct rxdevice *rxaddr;
	register struct buf *bp, *dp;
	register struct rx_softc *sc;
	struct uba_device *ui;
	struct rxerr *er;
	register struct rx_ctlr *rxc;

	if (!um->um_tab.b_active)
		return;
	dp = um->um_tab.b_actf;
	if (!dp->b_active)
		return;
	bp = dp->b_actf;
	unit = RXUNIT(bp->b_dev);
	sc = &rx_softc[unit];
	ui = rxdinfo[unit];
	rxaddr = (struct rxdevice *)um->um_addr;
	rxc = &rx_ctlr[um->um_ctlr];
	rxc->rxc_tocnt = 0;
	er = &rxerr[unit];
#ifdef RXDEBUG
	printf("rxintr: dev=0x%x, state=0x%x, status=0x%x\n", 
		bp->b_dev, rxc->rxc_state, rxaddr->rxcs);
#endif
	if ((rxaddr->rxcs & RX_ERR) &&
	    rxc->rxc_state != RXS_RDSTAT && rxc->rxc_state != RXS_RDERR)
		goto error;
	switch (rxc->rxc_state) {

	/*
	 * Incomplete commands.  Perform next step
	 * and return.  Note that b_active is set on
	 * entrance and, therefore, also on exit.
	 */
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
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxmap(bp, &sector, &track);
		rxaddr->rxdb = sector;
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = track;
		return;

	/*
	 * Possibly completed command.
	 */
	case RXS_RDSTAT:
		if (rxaddr->rxdb&RXES_READY)
			goto rderr;
		bp->b_error = EIO;
		bp->b_flags |= B_ERROR;
		goto done;

	/*
	 * Command completed.
	 */
	case RXS_EMPTY:
	case RXS_WRITE:	
	case RXS_FORMAT:
		goto done;

	case RXS_RDERR:
		rxmap(bp, &sector, &track);
		printf("rx%d: hard error, lsn%d (trk %d psec %d) ",
			unit, bp->b_blkno * (NBPS / DEV_BSIZE),
			track, sector);
		printf("cs=%b, db=%b, err=%x\n", MASKREG(er->rxcs), 
			RXCS_BITS, MASKREG(er->rxdb), RXES_BITS, 
			MASKREG(er->rxxt[0]));
		printf("errstatus: 0x%x, 0x%x, 0x%x, 0x%x\n", er->rxxt[0],
			er->rxxt[1], er->rxxt[2], er->rxxt[3]);
		bp = bp->b_back;		/* kludge, see 'rderr:' */
		goto done;

	default:
		printf("rx%d: state %d (reset)\n", unit, rxc->rxc_state);
		rxreset(um->um_ubanum);
		return;
	}
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
	if (rxc->rxc_state == RXS_FORMAT || (rxaddr->rxdb&RXES_DENERR))
		goto giveup;
	if (rxaddr->rxdb & RXES_CRCERR) {
		if (++bp->b_errcnt >= 10)
			goto giveup;
		goto retry;
	}
	bp->b_errcnt += 9;
	if (bp->b_errcnt >= 10)
		goto giveup;
	rxaddr->rxcs = RX_INIT;
	/* no way to get an interrupt for "init done", so just wait */
	while ((rxaddr->rxcs&RX_DONE) == 0)
		;
retry:
	/*
	 * In case we already have UNIBUS resources, give
	 * them back since we reallocate things in rxstart.
	 * Also, the active flag must be reset, otherwise rxstart
	 * will refuse to restart the transfer
	 */
	if (um->um_ubinfo)
		ubadone(um);
	um->um_tab.b_active = 0;
	rxstart(um);
	return;

giveup:
	/*
	 * Hard I/O error --
	 * Density errors are not noted on the console since the
	 * only way to determine the density of an unknown disk
	 * is to try one density or the other at random and see
	 * which one doesn't give a density error.
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
	 * Insert buffer with request at the head of the queue.
	 */
	bp->b_error = EIO;
	bp->b_flags |= B_ERROR;
	ubadone(um);
	erxbuf[unit].b_back = bp;	/* kludge to save the buffer pointer */
					/* while processing the error */
	er->rxcs = rxaddr->rxcs;
	er->rxdb = rxaddr->rxdb;
	bp = &erxbuf[unit];
	bp->b_un.b_addr = (caddr_t)er->rxxt;
	bp->b_bcount = sizeof (er->rxxt);
	bp->b_flags &= ~(B_DIRTY|B_UAREA|B_PHYS|B_PAGET);
	if (um->um_tab.b_actf->b_actf == NULL)
		um->um_tab.b_actf->b_actl = bp;
	bp->b_forw = um->um_tab.b_actf->b_actf;
	um->um_tab.b_actf->b_actf = bp;
	rxc->rxc_state = RXS_RDERR;
	um->um_cmd = RX_RDERR;
	(void) ubago(ui);
	return;
done:
	um->um_tab.b_active = 0;
	um->um_tab.b_errcnt = 0;
	ubadone(um);
	if ((bp->b_resid -= NBPS) > 0) {
		bp->b_un.b_addr += NBPS;
		rxstart(um);
		return;
	}
	bp->b_un.b_addr = sc->sc_uaddr;
	bp->b_resid = 0;
	bp->b_bcount = sc->sc_bcnt;
	iodone(bp);
	rxc->rxc_state = RXS_IDLE;
	um->um_tab.b_actf = dp->b_forw;
	dp->b_active = 0;
	dp->b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	/*
	 * If this controller has more work to do,
	 * start it up right away
	 */
	if (um->um_tab.b_actf)
		rxstart(um);
}

/*ARGSUSED*/
#ifdef notdef
minrxphys(bp)
	struct buf *bp;
{
	struct rx_softc *sc = &rx_softc[RXUNIT(bp->b_dev)];

	if (bp->b_bcount > NBPS)
		bp->b_bcount = NBPS;
}
#endif

/* 
 * Wake up every second and if an interrupt is pending
 * but nothing has happened increment a counter.
 * If nothing happens for RX_MAXTIMEOUT seconds, 
 * call the interrupt routine (hoping that it will 
 * detect an error condition in the controller)
 */
rxtimo(ctlr)
	int ctlr;
{
	register struct rx_ctlr *rxc = &rx_ctlr[ctlr];

	if (rxwstart > 0) 
		timeout(rxtimo, (caddr_t)ctlr, hz);
	if (++rxc->rxc_tocnt < RX_MAXTIMEOUT)
		return;
	printf("rx: timeout\n");
	rxintr(ctlr);
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
		if (um->um_ubinfo)
			um->um_ubinfo = 0;
		rx_ctlr[ctlr].rxc_state = RXS_IDLE;
		rxaddr = (struct rxdevice *)um->um_addr;
		rxaddr->rxcs = RX_INIT;
		while ((rxaddr->rxcs&RX_DONE) == 0)
			;
		rxstart(um);
	}
}

/*
 * make the world believe this is a 
 * 512b/s device
 */

rxread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int unit = RXUNIT(dev) ;
	struct rx_softc *sc = &rx_softc[unit];

	if (uio->uio_offset + uio->uio_resid > RXSIZE)
		return (ENXIO);
	if (uio->uio_offset < 0 || (uio->uio_offset & SECMASK) != 0)
		return (EIO);
	return (physio(rxstrategy, &rrxbuf[unit], dev, B_READ, minphys, uio));
}

rxwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int unit = RXUNIT(dev);
	struct rx_softc *sc = &rx_softc[unit];

	if (uio->uio_offset + uio->uio_resid > RXSIZE)
		return (ENXIO);
	if (uio->uio_offset < 0 || (uio->uio_offset & SECMASK) != 0)
		return (EIO);
	return(physio(rxstrategy, &rrxbuf[unit], dev, B_WRITE, minphys, uio));
}

/*
 * Control routine:
 * processes three kinds of requests:
 *
 *	(1) Set density (i.e., format the diskette) according to 
 *		  that specified by the open device.
 *	(2) Arrange for the next sector to be written with a deleted-
 *		  data mark.
 *	(3) Report whether the last sector read had a deleted-data mark
 *		  (by returning with an EIO error code if it did).
 *
 * Requests relating to deleted-data marks can be handled right here.
 * A "set density" (format) request, however, must additionally be 
 * processed through "rxstart", just like a read or write request.
 */
/*ARGSUSED3*/
rxioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{   
	int unit = RXUNIT(dev);
	struct rx_softc *sc = &rx_softc[unit];

	switch (cmd) {

	case RXIOC_FORMAT:
		if ((flag&FWRITE) == 0)
			return (EBADF);
		return (rxformat(dev));

	case RXIOC_WDDS:
		sc->sc_flags |= RXF_USEWDDS;
		return (0);

	case RXIOC_RDDSMK:
		*(int *)data = sc->sc_flags & RXF_DDMK;
		return (0);
	}
	return (ENXIO);
}

/*
 * Initiate a format command.
 */
rxformat(dev)
	dev_t dev;
{
	int unit = RXUNIT(dev);
	struct buf *bp;
	struct rx_softc *sc = &rx_softc[unit];
	int s, error = 0;

	bp = &rrxbuf[unit];
	s = spl5();
	while (bp->b_flags & B_BUSY)
		sleep(bp, PRIBIO);
	bp->b_flags = B_BUSY | B_CTRL;
	splx(s);
	sc->sc_flags = RXS_FORMAT;
	bp->b_dev = dev;
	bp->b_error = 0;
	bp->b_resid = 0;
	rxstrategy (bp);
	iowait(bp);
	if (bp->b_flags & B_ERROR)
		error = bp->b_error;
	bp->b_flags &= ~B_BUSY;
	wakeup((caddr_t)bp);
	return (error);
}
#endif
