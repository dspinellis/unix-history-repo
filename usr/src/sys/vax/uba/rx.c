/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)rx.c	7.3 (Berkeley) %G%
 */

#include "rx.h"
#if NFX > 0
/*
 * RX02 floppy disk device driver
 *
 */

/*
 * TODO:
 *	- clean up the code for multisector transfers using
 *	  a 'transfer in progress' flag
 *	- Test Deleted Data read/write 
 *	- Test error handling/reporting and 'volume valid' stuff
 *
 * 	Note: If the drive subsystem is
 * 	powered off at boot time, the controller won't interrupt!
 */

#include "machine/pte.h"

#include "param.h"
#include "buf.h"
#include "systm.h"
#include "conf.h"
#include "errno.h"
#include "time.h"
#include "kernel.h"
#include "uio.h"
#include "file.h"

#include "../vax/cpu.h"
#include "../vax/nexus.h"

#include "ubavar.h"
#include "ubareg.h"
#include "rxreg.h"

#define b_cylin	b_resid

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
#define	RXS_IDLE	8	/* device is idle */
	u_short	rxc_rxcs;	/* extended error status */
	u_short	rxc_rxdb;
	u_short	rxc_rxxt[4];
	int	rxc_tocnt;	/* for watchdog routine */
#define	RX_MAXTIMEOUT	30	/* # seconds to wait before giving up */
} rx_ctlr[NFX];

/* per-drive buffers */
struct buf	rrxbuf[NRX];	/* buffers for raw I/O */
struct buf	erxbuf[NRX];	/* buffers for reading error status */
struct buf	rxutab[NRX];	/* per drive buffers */

/* per-drive data */
struct rx_softc {
	int	sc_flags;	/* drive status flags */
#define	RXF_DIRECT	0x01	/* if set: use direct sector mapping */
#define	RXF_TRKONE	0x02	/* if set: start mapping on track 1 */
#define	RXF_DBLDEN	0x04	/* use double density */
#define	RXF_DEVTYPE	0x07	/* mapping flags */
#define	RXF_LOCK	0x10	/* exclusive use */
#define	RXF_DDMK	0x20	/* deleted-data mark detected */
#define	RXF_USEWDDS	0x40	/* write deleted-data sector */
#define	RXF_FORMAT	0x80	/* format in progress */
#define	RXF_BAD		0x100	/* drive bad, cannot be used */
	int	sc_csbits;	/* constant bits for CS register */
	int	sc_open;	/* count number of opens */
	int	sc_offset;	/* raw mode kludge to avoid restricting */
				/* single sector transfers to start on */
				/* DEV_BSIZE boundaries */
	/*
	 * The rest of this structure is used to 
	 * store temporaries while simulating multi 
	 * sector transfers
	 */
	caddr_t	sc_uaddr;	/* unibus base address */
	long	sc_bcnt;	/* total transfer count */
	long	sc_resid;	/* no. of bytes left to transfer */
} rx_softc[NRX];

struct rxerr {
	short	rxcs;
	short	rxdb;
	short	rxxt[4];	/* error code dump from controller */
} rxerr[NRX];
/* End of per-drive data */

struct	uba_device *rxdinfo[NRX];
struct	uba_ctlr *rxminfo[NFX];

struct buf *savebp;

int rxprobe(), rxslave(), rxattach(), rxdgo(), rxintr(), rxwatch(), rxphys();
u_short rxstd[] = { 0177170, 0177150, 0 };
struct uba_driver fxdriver =
  { rxprobe, rxslave, rxattach, rxdgo, rxstd, "rx", rxdinfo, "fx", rxminfo };

int	rxwstart;
#define	RXUNIT(dev)	(minor(dev)>>3)
#define	MASKREG(reg)	(reg&0xffff)

/* constants related to floppy data capacity */
#define	RXSECS	2002				/* # sectors on a floppy */
#define	DDSTATE	(sc->sc_csbits&RX_DDEN)
#define	NBPS	(DDSTATE ? 256 : 128)		/* # bytes per sector */
#define	RXSIZE	(DDSTATE ? 512512 : 256256)	/* # bytes per disk */
#define	SECMASK	(DDSTATE ? 0xff : 0x7f)		/* shifted-out bits of offset */

#define	B_CTRL		0x80000000		/* control (format) request */
#define B_RDSTAT	0x40000000		/* read drive status (open) */

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

/*ARGSUSED*/
rxslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{
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
{ 
	register int unit = RXUNIT(dev);
	register struct rx_softc *sc;
	register struct uba_device *ui;
	struct rx_ctlr *rxc;

	if (unit >= NRX || (ui = rxdinfo[unit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	sc = &rx_softc[unit];
	if (sc->sc_open == 0 && sc->sc_csbits == 0) {
		struct buf *bp = &erxbuf[unit];
		/*
		 * lock the device while an open 
		 * is in progress
		 */
		sc->sc_flags = (minor(dev) & RXF_DEVTYPE) | RXF_LOCK;
		sc->sc_csbits = RX_INTR;
		sc->sc_csbits |= ui->ui_slave == 0 ? RX_DRV0 : RX_DRV1;

		bp->b_dev = dev;
		bp->b_flags = B_RDSTAT | B_BUSY;
		bp->b_error = 0;
		bp->b_blkno = 0;
		sc->sc_offset = 0;
		sc->sc_resid  = 0;
		/*
		 * read device status to determine if
		 * a floppy is present in the drive and
		 * what density it is
		 */
		rxstrategy(bp);
		iowait(bp);
		if (bp->b_flags & B_ERROR) {
			sc->sc_csbits = 0;
			sc->sc_flags &= ~RXF_LOCK;
			return (bp->b_error);
		}
		if (rxwstart++ == 0) {
			rxc = &rx_ctlr[ui->ui_mi->um_ctlr];
			rxc->rxc_tocnt = 0;
			timeout(rxwatch, (caddr_t)0, hz);  /* start watchdog */
		}
#ifdef RXDEBUG
		printf("rxopen: csbits=0x%x\n", sc->sc_csbits);
#endif
		sc->sc_flags &= ~RXF_LOCK;
	} else	{
		if (sc->sc_flags & RXF_LOCK)
			return(EBUSY);
	}
	sc->sc_open = 1;
	return (0);
}

/*ARGSUSED1*/
rxclose(dev, flag)
	dev_t dev;
{
	register struct rx_softc *sc = &rx_softc[RXUNIT(dev)];

	sc->sc_open = 0;
#ifdef RXDEBUG
	printf("rxclose: dev=0x%x, sc_open=%d\n", dev, sc->sc_open);
#endif
}

rxstrategy(bp)
	register struct buf *bp;
{
	struct uba_device *ui;
	register struct buf *dp;
	struct rx_softc *sc;
	int s, unit = RXUNIT(bp->b_dev);

	if (unit >= NRX)
		goto bad;
	ui = rxdinfo[unit];
	if (ui == 0 || ui->ui_alive == 0) 
		goto bad;
	sc = &rx_softc[unit];
	if (bp->b_blkno < 0 || dbtob(bp->b_blkno) > RXSIZE)
		goto bad;
	if (sc->sc_flags & RXF_BAD) {
		bp->b_error = EIO;
		goto dbad;
	}
	s = spl5();
#ifdef RXDEBUG
	printf("rxstrat: bp=0x%x, fl=0x%x, un=%d, bl=%d, cnt=%d\n", 
		bp, bp->b_flags, unit, bp->b_blkno, bp->b_bcount);
#endif
	bp->b_cylin = bp->b_blkno;	/* don't care to calculate trackno */
	dp = &rxutab[unit];
	disksort(dp, bp);
	if (dp->b_active == 0) {
		rxustart(ui);
		bp = &ui->ui_mi->um_tab;
		if (bp->b_actf && bp->b_active == 0)
			rxstart(ui->ui_mi);
	}
	splx(s);
	return;

bad:
	bp->b_error = ENXIO;
dbad:
	bp->b_flags |= B_ERROR;
	iodone(bp);
	return;
}

/*
 * Unit start routine.
 * Put this unit on the ready queue for the controller
 */
rxustart(ui)
	register struct uba_device *ui;
{
	struct buf *dp = &rxutab[ui->ui_unit];
	struct uba_ctlr *um = ui->ui_mi;
	
	dp->b_forw = NULL;
	if (um->um_tab.b_actf == NULL)
		um->um_tab.b_actf = dp;
	else
		um->um_tab.b_actl->b_forw = dp;
	um->um_tab.b_actl = dp;
	dp->b_active++;
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

	ls = (dbtob(bp->b_blkno) + (sc->sc_offset - sc->sc_resid)) / NBPS;
	lt = ls / 26;
	ls %= 26;
	/*
	 * The "physical track offset" (ptoff) takes the
	 * starting physical track (0 or 1) and the desired
	 * interleaving into account.  If lt+ptoff >= 77,
	 * then interleaving is not performed.
	 */
	ptoff = 0;
	if (sc->sc_flags & RXF_DIRECT)
		ptoff = 77;
	if (sc->sc_flags & RXF_TRKONE)
		ptoff++;
	if (lt + ptoff < 77)
		ls = ((ls << 1) + (ls >= 13) + (6*lt)) % 26;
	*ptrack = (lt + ptoff) % 77;
	*psector = ls + 1;
}

/*
 * Controller start routine.
 * Start a new transfer or continue a multisector
 * transfer. If this is a new transfer (dp->b_active == 1)
 * save the start address of the data buffer and the total
 * byte count in the soft control structure. These are
 * restored into the buffer structure when the transfer has
 * been completed, before calling 'iodone'.
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
	if (sc->sc_flags & RXF_BAD) {
		rxpurge(um);
		return;
	}
	if (dp->b_active == 1) {
		sc->sc_resid = bp->b_bcount;
		sc->sc_uaddr = bp->b_un.b_addr;
		sc->sc_bcnt = bp->b_bcount;
		sc->sc_offset += sc->sc_bcnt;
		dp->b_active++;
	}
	rxaddr = (struct rxdevice *)um->um_addr;
	rxc = &rx_ctlr[um->um_ctlr];
	bp->b_bcount = sc->sc_resid;
	if (bp->b_bcount > NBPS)
		bp->b_bcount = NBPS;
	rxc->rxc_tocnt = 0;
#ifdef RXDEBUG
	printf("rxstart: ");
#endif
	if (rxaddr->rxcs == 0x800) {
		/*
		 * 'Volume valid'? (check if the 
		 * drive unit has been powered down)
		 */
		rxaddr->rxcs = RX_INIT;
		while((rxaddr->rxcs&RX_DONE) == 0)
			;
	}
	if (bp->b_flags & B_CTRL) {				/* format */
		rxc->rxc_state = RXS_FORMAT;
		rxaddr->rxcs = RX_FORMAT | sc->sc_csbits;
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = 'I';
		return;
	}
	if (bp->b_flags & B_RDSTAT) {			/* read drive status */
		rxc->rxc_state = RXS_RDSTAT;
		rxaddr->rxcs = RX_RDSTAT | sc->sc_csbits;
		return;
	}

	if (bp->b_flags & B_READ) {
		rxmap(bp, &sector, &track);			/* read */
#ifdef RXDEBUG
		printf("read tr=%d, sc=%d", track, sector);
#endif
		rxc->rxc_state = RXS_READ;
		rxaddr->rxcs = RX_READ | sc->sc_csbits;
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = (u_short)sector;
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = (u_short)track;
	} else {
#ifdef RXDEBUG
		printf("write");
#endif
		rxc->rxc_state = RXS_FILL;			/* write */
		um->um_cmd = RX_FILL;
		(void) ubago(rxdinfo[unit]);
	}
#ifdef RXDEBUG
	printf("\n");
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
	struct rx_ctlr *rxc;

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
	printf("rxint: dev=%x, st=%d, cs=0x%x, db=0x%x\n", 
		bp->b_dev, rxc->rxc_state, rxaddr->rxcs, rxaddr->rxdb);
#endif
	if ((rxaddr->rxcs & RX_ERR) &&
	    (rxc->rxc_state != RXS_RDSTAT) && (rxc->rxc_state != RXS_RDERR))
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
		rxmap(bp, &sector, &track);
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = sector;
		while ((rxaddr->rxcs&RX_TREQ) == 0)
			;
		rxaddr->rxdb = track;
		return;

	/*
	 * Possibly completed command.
	 */
	case RXS_RDSTAT:
		if (bp->b_flags & B_RDSTAT) {
			if ((rxaddr->rxdb&RXES_READY) == 0) {
				bp->b_flags |= B_ERROR;
				bp->b_error = ENODEV;
			} else {
				sc->sc_csbits |= rxaddr->rxdb&RXES_DBLDEN ?
					RX_DDEN : RX_SDEN;
			}
			goto rdone;
		}
		if (rxaddr->rxdb&RXES_READY)
			goto rderr;
		bp->b_error = ENODEV;
		bp->b_flags |= B_ERROR;
		goto done;

	/*
	 * Command completed.
	 */
	case RXS_EMPTY:
	case RXS_WRITE:	
		goto done;

	case RXS_FORMAT:
		goto rdone;

	case RXS_RDERR:
		bp = savebp;
		rxmap(bp, &sector, &track);
		printf("rx%d: hard error, trk %d psec %d ",
			unit, track, sector);
		printf("cs=%b, db=%b, err=", MASKREG(er->rxcs), 
			RXCS_BITS, MASKREG(er->rxdb), RXES_BITS);
		printf("%x, %x, %x, %x\n", MASKREG(er->rxxt[0]),
			MASKREG(er->rxxt[1]), MASKREG(er->rxxt[2]), 
			MASKREG(er->rxxt[3]));
		goto done;

	default:
		printf("rx%d: state %d (reset)\n", unit, rxc->rxc_state);
		rxreset(um->um_ubanum);
		return;
	}
error:
	/*
	 * In case of an error:
	 *  (a) Give up now if a format (ioctl) was in progress, if a
	 *	  density error was detected, or if the drive went offline
	 *  (b) Retry up to nine times if a CRC (data) error was detected,
	 *	  then give up if the error persists.
	 *  (c) In all other cases, reinitialize the drive and try the
	 *	  operation once more before giving up.
	 */
	if (rxc->rxc_state == RXS_FORMAT || (rxaddr->rxdb&RXES_DENERR))
		goto giveup;
	if (rxaddr->rxdb & RXES_CRCERR) {
		if (++um->um_tab.b_errcnt >= 10)
			goto giveup;
		goto retry;
	}
	um->um_tab.b_errcnt += 9;
	if (um->um_tab.b_errcnt >= 10)
		goto giveup;
	rxaddr->rxcs = RX_INIT;
	/* no way to get an interrupt for "init done", so just wait */
	while ((rxaddr->rxcs&RX_DONE) == 0)
		;
	/* if someone opened the drive: give up */
	if ((rxaddr->rxdb&RXES_READY) == 0)
		goto giveup;
retry:
	/*
	 * In case we already have UNIBUS resources, give
	 * them back since we reallocate things in rxstart.
	 */
	if (um->um_ubinfo)
		ubadone(um);
	um->um_tab.b_active = 0;
	rxstart(um);
	return;

giveup:
	/*
	 * Hard I/O error --
	 * ALL errors are considered fatal and will abort the
	 * transfer and purge the i/o request queue
	 */
	sc->sc_flags |= RXF_BAD;
	sc->sc_resid = 0;	/* make sure the transfer is terminated */
	rxc->rxc_state = RXS_RDSTAT;
	rxaddr->rxcs = RX_RDSTAT | sc->sc_csbits;
	return;

rderr:
	/*
	 * A hard error (other than not ready) has occurred.
	 * Read the extended error status information.
	 * Before doing this, save the current CS and DB register values,
	 * because the read error status operation may modify them.
	 * Insert buffer with request at the head of the queue.
	 */
	bp->b_error = EIO;
	bp->b_flags |= B_ERROR;
	if (um->um_ubinfo)
		ubadone(um);
	savebp = bp;
	er->rxcs = rxaddr->rxcs;
	er->rxdb = rxaddr->rxdb;
	bp = &erxbuf[unit];
	bp->b_un.b_addr = (caddr_t)er->rxxt;
	bp->b_bcount = sizeof (er->rxxt);
	bp->b_flags &= ~(B_DIRTY|B_UAREA|B_PHYS|B_PAGET);
	if (dp->b_actf == NULL)
		dp->b_actl = bp;
	bp->b_forw = dp->b_actf;
	dp->b_actf = bp;
	rxc->rxc_state = RXS_RDERR;
	um->um_cmd = RX_RDERR;
	(void) ubago(ui);
	return;

done:
	ubadone(um);
rdone:
	um->um_tab.b_active = 0;
	um->um_tab.b_errcnt = 0;
	if ((sc->sc_resid -= NBPS) > 0) {
		bp->b_un.b_addr += NBPS;
		rxstart(um);
		return;
	}
	bp->b_un.b_addr = sc->sc_uaddr;
	bp->b_resid = 0;
	bp->b_bcount = sc->sc_bcnt;
	dp->b_actf = bp->av_forw;
	iodone(bp);
	sc->sc_offset = 0;
	rxc->rxc_state = RXS_IDLE;
	um->um_tab.b_actf = dp->b_forw;
	dp->b_active = 0;
	dp->b_errcnt = 0;
#ifdef RXDEBUG
	printf(".. bp=%x, new=%x\n", bp, dp->b_actf);
#endif
	/*
	 * If this unit has more work to do,
	 * start it up right away
	 */
	if (dp->b_actf)
		rxustart(ui);

	rxstart(um);
}

/*ARGSUSED*/

rxwatch()
{
	register struct uba_device *ui;
	register struct uba_ctlr *um;
	register struct rx_softc *sc;
	struct rx_ctlr *rxc;
	int i, dopen = 0;

	for (i=0; i<NRX; i++) {
		ui = rxdinfo[i];
		if (ui == 0 || ui->ui_alive == 0)
			continue;
		sc = &rx_softc[i];
		if ((sc->sc_open == 0) && (rxutab[i].b_active == 0)) {
			sc->sc_csbits = 0;
			continue;
		}
		dopen++;
		um = ui->ui_mi;
		rxc = &rx_ctlr[um->um_ctlr];
		if (++rxc->rxc_tocnt >= RX_MAXTIMEOUT) {
			rxc->rxc_tocnt = 0;
			if (um->um_tab.b_active) {	
				printf("rx%d: timeout\n", i);/* for debugging */
				rxintr(um->um_ctlr);
			}
		}
	}
	if (dopen)
		timeout(rxwatch, (caddr_t)0, hz);
	else
		rxwstart = 0;
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
			printf("<%d>", UBAI_BDP(um->um_ubinfo));
			um->um_ubinfo = 0;
		}
		rx_ctlr[ctlr].rxc_state = RXS_IDLE;
		rxaddr = (struct rxdevice *)um->um_addr;
		rxaddr->rxcs = RX_INIT;
		while ((rxaddr->rxcs&RX_DONE) == 0)
			;
		rxstart(um);
	}
}

rxread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int unit = RXUNIT(dev);
	struct rx_softc *sc = &rx_softc[unit];

	if (uio->uio_offset + uio->uio_resid > RXSIZE)
		return (ENXIO);
	if (uio->uio_offset < 0 || (uio->uio_offset & SECMASK) != 0)
		return (ENXIO);
	sc->sc_offset = uio->uio_offset % DEV_BSIZE;
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
		return (ENXIO);
	sc->sc_offset = uio->uio_offset % DEV_BSIZE;
	return(physio(rxstrategy, &rrxbuf[unit], dev, B_WRITE, minphys, uio));
}

/*
 * Control routine:
 * processes four kinds of requests:
 *
 *	(1) Set density (i.e., format the diskette) according to 
 *		  that specified data parameter
 *	(2) Arrange for the next sector to be written with a deleted-
 *		  data mark.
 *	(3) Report whether the last sector read had a deleted-data mark
 *	(4) Report the density of the diskette in the indicated drive
 *	    (since the density it automatically determined by the driver,
 *	     this is the only way to let an application program know the
 *	     density)
 *
 * Requests relating to deleted-data marks can be handled right here.
 * A "set density" (format) request, however, must additionally be 
 * processed through "rxstart", just like a read or write request.
 */

/*ARGSUSED3*/
rxioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{   
	int unit = RXUNIT(dev);
	struct rx_softc *sc = &rx_softc[unit];

	switch (cmd) {

	case RXIOC_FORMAT:
		if ((flag&FWRITE) == 0)
			return (EBADF);
		if (sc->sc_open > 1)
			return (EBUSY);
		if (*(int *)data)
			sc->sc_csbits |= RX_DDEN;
		else
			sc->sc_csbits &= ~RX_DDEN;
		return (rxformat(dev));

	case RXIOC_WDDS:
		sc->sc_flags |= RXF_USEWDDS;
		return (0);

	case RXIOC_RDDSMK:
		*(int *)data = sc->sc_flags & RXF_DDMK;
		return (0);

	case RXIOC_GDENS:
		*(int *)data = sc->sc_csbits & RX_DDEN;
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
	int error = 0;

	bp = &rrxbuf[unit];
	bp->b_flags = B_BUSY | B_CTRL;
	sc->sc_flags = RXF_FORMAT | RXF_LOCK;
	bp->b_dev = dev;
	bp->b_error = 0;
	bp->b_blkno = 0;
	rxstrategy(bp);
	iowait(bp);
	if (bp->b_flags & B_ERROR)
		error = bp->b_error;
	bp->b_flags &= ~B_BUSY;
	sc->sc_flags &= ~RXF_LOCK;
	return (error);
}

/*
 * A permanent hard error condition has occured,
 * purge the buffer queue
 */
rxpurge(um)
	register struct uba_ctlr *um;
{
	register struct buf *bp, *dp;

	dp = um->um_tab.b_actf;
	while (dp->b_actf) {
		dp->b_errcnt++;
		bp = dp->b_actf;
		bp->b_error = EIO;
		bp->b_flags |= B_ERROR;
		iodone(bp);
		dp->b_actf = bp->av_forw;
	}
}
#endif
