/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ut.c	7.12 (Berkeley) %G%
 */

#include "tj.h"
#if NUT > 0
/*
 * System Industries Model 9700 Tape Drive
 *   emulates a TU45 on the UNIBUS
 *
 * TODO:
 *	check out attention processing
 *	try reset code and dump code
 */
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/file.h"
#include "sys/map.h"
#include "sys/ioctl.h"
#include "sys/mtio.h"
#include "sys/cmap.h"
#include "sys/time.h"
#include "sys/uio.h"
#include "sys/kernel.h"
#include "sys/syslog.h"
#include "sys/tprintf.h"

#include "../include/pte.h"
#include "../include/cpu.h"
#include "ubareg.h"
#include "ubavar.h"
#include "utreg.h"

struct	buf	cutbuf[NUT];	/* bufs for control operations */
struct	buf	tjutab[NTJ];	/* bufs for slave queue headers */

struct uba_ctlr *utminfo[NUT];
struct uba_device *tjdinfo[NTJ];
int utprobe(), utslave(), utattach(), utdgo(), utintr(), uttimer();
u_short utstd[] = { 0772440, 0 };
struct uba_driver utdriver =
  { utprobe, utslave, utattach, utdgo, utstd, "tj", tjdinfo, "ut", utminfo, 0 };

#define	MASKREG(reg)	((reg)&0xffff)

/* bits in minor device */
#define	TJUNIT(dev)	(minor(dev)&03)
#define	T_NOREWIND	04
#define	T_1600BPI	010
#define	T_6250BPI	020
short	utdens[] = { UT_NRZI, UT_PE, UT_GCR, UT_NRZI };

/* slave to controller mapping table */
short	tjtout[NTJ];
#define UTUNIT(dev)	(tjtout[TJUNIT(dev)])

#define	INF	(daddr_t)1000000L	/* a block number that wont exist */

struct	tj_softc {
	char	sc_openf;	/* exclusive open */
	char	sc_lastiow;	/* last I/O operation was a write */
	daddr_t	sc_blkno;	/* next block to transfer */
	daddr_t	sc_nxrec;	/* next record on tape */
	u_short	sc_erreg;	/* image of uter */
	u_short	sc_dsreg;	/* image of utds */
	u_short	sc_resid;	/* residual from transfer */
	u_short	sc_dens;	/* sticky selected density */
	daddr_t	sc_timo;	/* time until timeout expires */
	short	sc_tact;	/* timeout is active flag */
	tpr_t	sc_tpr;		/* tprintf handle */
	int	sc_blks;	/* number of I/O operations since open */
	int	sc_softerrs;	/* number of soft I/O errors since open */
} tj_softc[NTJ];

/*
 * Internal per/slave states found in sc_state
 */
#define	SSEEK		1	/* seeking */
#define	SIO		2	/* doing sequential I/O */
#define	SCOM		3	/* sending a control command */
#define	SREW		4	/* doing a rewind op */
#define	SERASE		5	/* erase inter-record gap */
#define	SERASED		6	/* erased inter-record gap */

/*ARGSUSED*/
utprobe(reg)
	caddr_t reg;
{
	register int br, cvec;
#ifdef lint
	br=0; cvec=br; br=cvec;
	utintr(0);
#endif
	/*
	 * The SI documentation says you must set the RDY bit
	 * (even though it's read-only) to force an interrupt.
	 */
	((struct utdevice *) reg)->utcs1 = UT_IE|UT_NOP|UT_RDY;
	DELAY(10000);
	return (sizeof (struct utdevice));
}

/*ARGSUSED*/
utslave(ui, reg)
	struct uba_device *ui;
	caddr_t reg;
{
	/*
	 * A real TU45 would support the slave present bit
	 * int the drive type register, but this thing doesn't,
	 * so there's no way to determine if a slave is present or not.
	 */
	 return(1);
}

utattach(ui)
	struct uba_device *ui;
{
	tjtout[ui->ui_unit] = ui->ui_mi->um_ctlr;
}

/*
 * Open the device with exclusive access.
 */
utopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int tjunit = TJUNIT(dev);
	register struct uba_device *ui;
	register struct tj_softc *sc;
	int olddens, dens, error;
	register int s;

	if (tjunit >= NTJ || (ui = tjdinfo[tjunit]) == 0 || ui->ui_alive == 0)
		return (ENXIO);
	if ((sc = &tj_softc[tjunit])->sc_openf)
		return (EBUSY);
	sc->sc_openf = 1;
	olddens = sc->sc_dens;
	dens = sc->sc_dens =
	    utdens[(minor(dev)&(T_1600BPI|T_6250BPI))>>3]|
	      PDP11FMT|(ui->ui_slave&07);
get:
	utcommand(dev, UT_SENSE, 1);
	if (sc->sc_dsreg&UTDS_PIP) {
		if (error = tsleep((caddr_t)&lbolt, (PZERO+1) | PCATCH,
		    devopn, 0))
			return (error);
		goto get;
	}
	sc->sc_dens = olddens;
	if ((sc->sc_dsreg&UTDS_MOL) == 0) {
		sc->sc_openf = 0;
		uprintf("tj%d: not online\n", tjunit);
		return (EIO);
	}
	if ((flag&FWRITE) && (sc->sc_dsreg&UTDS_WRL)) {
		sc->sc_openf = 0;
		uprintf("tj%d: no write ring\n", tjunit);
		return (EIO);
	}
	if ((sc->sc_dsreg&UTDS_BOT) == 0 && (flag&FWRITE) &&
	    dens != sc->sc_dens) {
		sc->sc_openf = 0;
		uprintf("tj%d: can't change density in mid-tape\n", tjunit);
		return (EIO);
	}
	sc->sc_blkno = (daddr_t)0;
	sc->sc_nxrec = INF;
	sc->sc_lastiow = 0;
	sc->sc_blks = 0;
	sc->sc_softerrs = 0;
	sc->sc_dens = dens;
	sc->sc_tpr = tprintf_open();
	/*
	 * For 6250 bpi take exclusive use of the UNIBUS.
	 */
	ui->ui_driver->ud_xclu = (dens&(T_1600BPI|T_6250BPI)) == T_6250BPI;
	s = splclock();
	if (sc->sc_tact == 0) {
		sc->sc_timo = INF;
		sc->sc_tact = 1;
		timeout(uttimer, (caddr_t)dev, 5*hz);
	}
	splx(s);
	return (0);
}

utclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register struct tj_softc *sc = &tj_softc[TJUNIT(dev)];

	if (flag == FWRITE || ((flag&FWRITE) && sc->sc_lastiow)) {
		utcommand(dev, UT_WEOF, 1);
		utcommand(dev, UT_WEOF, 1);
		utcommand(dev, UT_SREV, 1);
	}
	if ((minor(dev)&T_NOREWIND) == 0)
		utcommand(dev, UT_REW, 0);
	if (sc->sc_blks > 100 && sc->sc_softerrs > sc->sc_blks / 100)
		log(LOG_INFO, "tj%d: %d soft errors in %d blocks\n",
		    TJUNIT(dev), sc->sc_softerrs, sc->sc_blks);
	tprintf_close(sc->sc_tpr);
	sc->sc_openf = 0;
	return (0);
}

utcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;
	register int s;

	bp = &cutbuf[UTUNIT(dev)];
	s = spl5();
	while (bp->b_flags&B_BUSY) {
		if(bp->b_repcnt == 0 && (bp->b_flags&B_DONE))
			break;
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY|B_READ;
	splx(s);
	bp->b_dev = dev;
	bp->b_command = com;
	bp->b_repcnt = count;
	bp->b_blkno = 0;
	utstrategy(bp);
	if (count == 0)
		return;
	iowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

/*
 * Queue a tape operation.
 */
utstrategy(bp)
	register struct buf *bp;
{
	int tjunit = TJUNIT(bp->b_dev);
	register struct uba_ctlr *um;
	register struct buf *dp;
	int s;

	/*
	 * Put transfer at end of unit queue
	 */
	dp = &tjutab[tjunit];
	bp->av_forw = NULL;
	um = tjdinfo[tjunit]->ui_mi;
	s = spl5();
	if (dp->b_actf == NULL) {
		dp->b_actf = bp;
		/*
		 * Transport not active, so...
		 * put at end of controller queue
		 */
		dp->b_forw = NULL;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else
			um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
	} else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	/*
	 * If the controller is not busy, set it going.
	 */
	if (um->um_tab.b_state == 0)
		utstart(um);
	splx(s);
}

utstart(um)
	register struct uba_ctlr *um;
{
	register struct utdevice *addr;
	register struct buf *bp, *dp;
	register struct tj_softc *sc;
	struct uba_device *ui;
	int tjunit;
	daddr_t blkno;

loop:
	/*
	 * Scan controller queue looking for units with
	 * transaction queues to dispatch
	 */
	if ((dp = um->um_tab.b_actf) == NULL)
		return;
	if ((bp = dp->b_actf) == NULL) {
		um->um_tab.b_actf = dp->b_forw;
		goto loop;
	}
	addr = (struct utdevice *)um->um_addr;
	tjunit = TJUNIT(bp->b_dev);
	ui = tjdinfo[tjunit];
	sc = &tj_softc[tjunit];
	/* note slave select, density, and format were merged on open */
	addr->uttc = sc->sc_dens;
	sc->sc_dsreg = addr->utds;
	sc->sc_erreg = addr->uter;
	sc->sc_resid = MASKREG(addr->utfc);
	/*
	 * Default is that last command was NOT a write command;
	 * if we do a write command we will notice this in utintr().
	 */
	sc->sc_lastiow = 0;
	if (sc->sc_openf < 0 || (addr->utds&UTDS_MOL) == 0) {
		/*
		 * Have had a hard error on a non-raw tape
		 * or the tape unit is now unavailable
		 * (e.g. taken off line).
		 */
		bp->b_flags |= B_ERROR;
		goto next;
	}
	if (bp == &cutbuf[UTUNIT(bp->b_dev)]) {
		/*
		 * Execute a control operation with the specified
		 * count.
		 */
		if (bp->b_command == UT_SENSE)
			goto next;
		if (bp->b_command == UT_SFORW && (addr->utds & UTDS_EOT)) {
			bp->b_resid = bp->b_bcount;
			goto next;
		}
		/*
		 * Set next state; handle timeouts
		 */
		if (bp->b_command == UT_REW) {
			um->um_tab.b_state = SREW;
			sc->sc_timo = 5*60;
		} else {
			um->um_tab.b_state = SCOM;
			sc->sc_timo = imin(imax(10*(int)-bp->b_repcnt,60),5*60);
		}
		/* NOTE: this depends on the ut command values */
		if (bp->b_command >= UT_SFORW && bp->b_command <= UT_SREVF)
			addr->utfc = -bp->b_repcnt;
		goto dobpcmd;
	}
	/*
	 * For raw I/O, save the current block
	 * number in case we have to retry.
	 */
	if (bp->b_flags & B_RAW) {
		if (um->um_tab.b_errcnt == 0) {
			sc->sc_blkno = bdbtofsb(bp->b_blkno);
			sc->sc_nxrec = sc->sc_blkno + 1;
		}
	}
	else {
		/*
		 * Handle boundary cases for operation
		 * on non-raw tapes.
		 */
		if (bdbtofsb(bp->b_blkno) > sc->sc_nxrec) {
			/* can't read past end of file */
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
			goto next;
		}
		if (bdbtofsb(bp->b_blkno) == sc->sc_nxrec &&
		    (bp->b_flags&B_READ)) {
			/*
			 * Reading at end of file returns 0 bytes.
			 */
			bp->b_resid = bp->b_bcount;
			clrbuf(bp);
			goto next;
		}
		if ((bp->b_flags&B_READ) == 0)
			sc->sc_nxrec = bdbtofsb(bp->b_blkno) + 1;
	}
	/*
	 * If the tape is correctly positioned, set up all the
	 * registers but the csr, and give control over to the
	 * UNIBUS adaptor routines, to wait for resources to
	 * start I/O.
	 */
	if ((blkno = sc->sc_blkno) == bdbtofsb(bp->b_blkno)) {
		addr->utwc = -(((bp->b_bcount)+1)>>1);
		addr->utfc = -bp->b_bcount;
		if ((bp->b_flags&B_READ) == 0) {
			/*
			 * On write error retries erase the
			 * inter-record gap before rewriting.
			 */
			if (um->um_tab.b_errcnt) {
				if (um->um_tab.b_state != SERASED) {
					um->um_tab.b_state = SERASE;
					sc->sc_timo = 60;
					addr->utcs1 = UT_ERASE|UT_IE|UT_GO;
					return;
				}
			}
			if (addr->utds & UTDS_EOT) {
				bp->b_resid = bp->b_bcount;
				um->um_tab.b_state = 0;
				goto next;
			}
			um->um_cmd = UT_WCOM;
		} else
			um->um_cmd = UT_RCOM;
		sc->sc_timo = 60;
		um->um_tab.b_state = SIO;
		(void) ubago(ui);
		return;
	}
	/*
	 * Tape positioned incorrectly; seek forwards or
	 * backwards to the correct spot.  This happens for
	 * raw tapes only on error retries.
	 */
	um->um_tab.b_state = SSEEK;
	if (blkno < bdbtofsb(bp->b_blkno)) {
		addr->utfc = blkno - bdbtofsb(bp->b_blkno);
		bp->b_command = UT_SFORW;
	} else {
		addr->utfc = bdbtofsb(bp->b_blkno) - blkno;
		bp->b_command = UT_SREV;
	}
	sc->sc_timo = imin(imax(10 * -addr->utfc, 60), 5*60);

dobpcmd:
	/*
	 * Perform the command setup in bp.
	 */
	addr->utcs1 = bp->b_command|UT_IE|UT_GO;
	return;
next:
	/*
	 * Advance to the next command in the slave queue,
	 * posting notice and releasing resources as needed.
	 */
	if (um->um_ubinfo)
		ubadone(um);
	um->um_tab.b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	iodone(bp);
	goto loop;
}

/*
 * Start operation on controller --
 * UNIBUS resources have been allocated.
 */
utdgo(um)
	register struct uba_ctlr *um;
{
	register struct utdevice *addr = (struct utdevice *)um->um_addr;

	addr->utba = (u_short) um->um_ubinfo;
	addr->utcs1 = um->um_cmd|((um->um_ubinfo>>8)&0x300)|UT_IE|UT_GO;
}

/*
 * Ut interrupt handler
 */
/*ARGSUSED*/
utintr(ut11)
	int ut11;
{
	struct buf *dp;
	register struct buf *bp;
	register struct uba_ctlr *um = utminfo[ut11];
	register struct utdevice *addr;
	register struct tj_softc *sc;
	u_short tjunit, cs2, cs1;
	register state;

	if ((dp = um->um_tab.b_actf) == NULL)
		return;
	bp = dp->b_actf;
	tjunit = TJUNIT(bp->b_dev);
	addr = (struct utdevice *)tjdinfo[tjunit]->ui_addr;
	sc = &tj_softc[tjunit];
	/*
	 * Record status...
	 */
	sc->sc_timo = INF;
	sc->sc_dsreg = addr->utds;
	sc->sc_erreg = addr->uter;
	sc->sc_resid = MASKREG(addr->utfc);
	if ((bp->b_flags&B_READ) == 0)
		sc->sc_lastiow = 1;
	state = um->um_tab.b_state;
	um->um_tab.b_state = 0;
	/*
	 * Check for errors...
	 */
	if ((addr->utds&UTDS_ERR) || (addr->utcs1&UT_TRE)) {
		/*
		 * To clear the ERR bit, we must issue a drive clear
		 * command, and to clear the TRE bit we must set the
		 * controller clear bit.
		 */
		cs2 = addr->utcs2;
		if ((cs1 = addr->utcs1)&UT_TRE)
			addr->utcs2 |= UTCS2_CLR;
		/* is this dangerous ?? */
		while ((addr->utcs1&UT_RDY) == 0)
			;
		addr->utcs1 = UT_CLEAR|UT_GO;
		/*
		 * If we were reading at 1600 or 6250 bpi and the error
		 * was corrected, then don't consider this an error.
		 */
		if (sc->sc_erreg & UTER_COR && (bp->b_flags & B_READ) &&
		    (addr->uttc & UTTC_DEN) != UT_NRZI) {
			tprintf(sc->sc_tpr,
			  "ut%d: soft error bn%d cs1=%b er=%b cs2=%b ds=%b\n",
			  tjunit, bp->b_blkno, cs1, UT_BITS, sc->sc_erreg,
			  UTER_BITS, cs2, UTCS2_BITS, sc->sc_dsreg, UTDS_BITS);
			sc->sc_erreg &= ~UTER_COR;
		}
		/*
		 * If we were reading from a raw tape and the only error
		 * was that the record was too long, then we don't consider
		 * this an error.
		 */
		if ((bp->b_flags & (B_READ|B_RAW)) == (B_READ|B_RAW) &&
		    (sc->sc_erreg&UTER_FCE))
			sc->sc_erreg &= ~UTER_FCE;
		if (sc->sc_erreg == 0)
			goto ignoreerr;
		/*
		 * Fix up errors which occur due to backspacing
		 * "over" the front of the tape.
		 */
		if ((sc->sc_dsreg & UTDS_BOT) && bp->b_command == UT_SREV &&
		    ((sc->sc_erreg &= ~(UTER_NEF|UTER_FCE)) == 0))
			goto opdone;
		/*
		 * Retry soft errors up to 8 times
		 */
		if ((sc->sc_erreg&UTER_HARD) == 0 && state == SIO) {
			if (++um->um_tab.b_errcnt < 7) {
				sc->sc_blkno++;
				ubadone(um);
				goto opcont;
			}
		}
		/*
		 * Hard or non-I/O errors on non-raw tape
		 * cause it to close.
		 */
		if ((bp->b_flags&B_RAW) == 0 && sc->sc_openf > 0)
			sc->sc_openf = -1;
		/*
		 * Couldn't recover error.
		 */
		tprintf(sc->sc_tpr,
			"ut%d: hard error bn%d cs1=%b er=%b cs2=%b ds=%b\n",
			tjunit, bp->b_blkno, cs1, UT_BITS, sc->sc_erreg,
			UTER_BITS, cs2, UTCS2_BITS, sc->sc_dsreg, UTDS_BITS);
		bp->b_flags |= B_ERROR;
		goto opdone;
	}

ignoreerr:
	/*
	 * If we hit a tape mark update our position.
	 */
	if (sc->sc_dsreg & UTDS_TM && bp->b_flags & B_READ) {
		/*
		 * Set blkno and nxrec
		 */
		if (bp == &cutbuf[UTUNIT(bp->b_dev)]) {
			if (sc->sc_blkno > bdbtofsb(bp->b_blkno)) {
				sc->sc_nxrec =
				     bdbtofsb(bp->b_blkno) - addr->utfc;
				sc->sc_blkno = sc->sc_nxrec;
			} else {
				sc->sc_blkno =
				     bdbtofsb(bp->b_blkno) + addr->utfc;
				sc->sc_nxrec = sc->sc_blkno-1;
			}
		} else
			sc->sc_nxrec = bdbtofsb(bp->b_blkno);
		/*
		 * Note: if we get a tape mark on a read, the
		 * frame count register will be zero, so b_resid
		 * will be calculated correctly below.
		 */
		goto opdone;
	}
	/*
	 * Advance tape control FSM.
	 */
	switch (state) {

	case SIO:		/* read/write increments tape block # */
		sc->sc_blkno++;
		sc->sc_blks++;
		if (um->um_tab.b_errcnt)
			sc->sc_softerrs++;
		break;

	case SCOM:		/* motion commands update current position */
		if (bp == &cutbuf[UTUNIT(bp->b_dev)])
		switch ((int)bp->b_command) {

		case UT_SFORW:
			sc->sc_blkno -= bp->b_repcnt;
			break;

		case UT_SREV:
			sc->sc_blkno += bp->b_repcnt;
			break;

		case UT_REWOFFL:
			addr->utcs1 = UT_CLEAR|UT_GO;
			break;
		}
		break;

	case SSEEK:
		sc->sc_blkno = bdbtofsb(bp->b_blkno);
		goto opcont;

	case SERASE:
		/*
		 * Completed erase of the inter-record gap due to a
		 * write error; now retry the write operation.
		 */
		um->um_tab.b_state = SERASED;
		goto opcont;

	case SREW:			/* clear attention bit */
		addr->utcs1 = UT_CLEAR|UT_GO;
		break;

	default:
		printf("bad state %d\n", state);
		panic("utintr");
	}

opdone:
	/*
	 * Reset error count and remove
	 * from device queue
	 */
	um->um_tab.b_errcnt = 0;
	dp->b_actf = bp->av_forw;
	/*
	 * For read command, frame count register contains
	 * actual length of tape record.  Otherwise, it
	 * holds negative residual count.
	 */
	if (state == SIO && um->um_cmd == UT_RCOM) {
		bp->b_resid = 0;
		if (bp->b_bcount > MASKREG(addr->utfc))
			bp->b_resid = bp->b_bcount - MASKREG(addr->utfc);
	} else
		bp->b_resid = MASKREG(-addr->utfc);
	ubadone(um);
	iodone(bp);
	/*
	 * Circulate slave to end of controller queue
	 * to give other slaves a chance
	 */
	um->um_tab.b_actf = dp->b_forw;
	if (dp->b_actf) {
		dp->b_forw = NULL;
		if (um->um_tab.b_actf == NULL)
			um->um_tab.b_actf = dp;
		else
			um->um_tab.b_actl->b_forw = dp;
		um->um_tab.b_actl = dp;
	}
	if (um->um_tab.b_actf == 0)
		return;
opcont:
	utstart(um);
}

/*
 * Watchdog timer routine.
 */
uttimer(dev)
	int dev;
{
	register struct tj_softc *sc = &tj_softc[TJUNIT(dev)];
	register short x;

	if (sc->sc_timo != INF && (sc->sc_timo -= 5) < 0) {
		printf("tj%d: lost interrupt\n", TJUNIT(dev));
		sc->sc_timo = INF;
		x = spl5();
		utintr(UTUNIT(dev));
		(void) splx(x);
	}
	timeout(uttimer, (caddr_t)dev, 5*hz);
}

/*ARGSUSED*/
utioctl(dev, cmd, data, flag)
	dev_t dev;
	caddr_t data;
{
	register struct tj_softc *sc = &tj_softc[TJUNIT(dev)];
	register struct buf *bp = &cutbuf[UTUNIT(dev)];
	register callcount;
	int fcount, error = 0;
	struct mtop *mtop;
	struct mtget *mtget;
	/* we depend of the values and order of the MT codes here */
	static utops[] =
      {UT_WEOF,UT_SFORWF,UT_SREVF,UT_SFORW,UT_SREV,UT_REW,UT_REWOFFL,UT_SENSE};

	switch (cmd) {

	case MTIOCTOP:
		mtop = (struct mtop *)data;
		switch(mtop->mt_op) {

		case MTWEOF:
		case MTFSF: case MTBSF:
		case MTFSR: case MTBSR:
			callcount = mtop->mt_count;
			fcount = 1;
			break;

		case MTREW: case MTOFFL: case MTNOP:
			callcount = 1;
			fcount = 1;
			break;

		default:
			return (ENXIO);
		}
		if (callcount <= 0 || fcount <= 0)
			return (EINVAL);
		while (--callcount >= 0) {
			utcommand(dev, utops[mtop->mt_op], fcount);
			if ((bp->b_flags&B_ERROR) || (sc->sc_dsreg&UTDS_BOT))
				break;
		}
		if (bp->b_flags&B_ERROR)
			if ((error = bp->b_error)==0)
				return (EIO);
		return (error);

	case MTIOCGET:
		mtget = (struct mtget *)data;
		mtget->mt_dsreg = sc->sc_dsreg;
		mtget->mt_erreg = sc->sc_erreg;
		mtget->mt_resid = sc->sc_resid;
		mtget->mt_type = MT_ISUT;
		break;

	default:
		return (ENXIO);
	}
	return (0);
}

utreset(uban)
	int uban;
{
	register struct uba_ctlr *um;
	register ut11, tjunit;
	register struct uba_device *ui;
	register struct buf *dp;

	for (ut11 = 0; ut11 < NUT; ut11++) {
		if ((um = utminfo[ut11]) == 0 || um->um_alive == 0 ||
		   um->um_ubanum != uban)
			continue;
		printf(" ut%d", ut11);
		um->um_tab.b_state = 0;
		um->um_tab.b_actf = um->um_tab.b_actl = 0;
		if (um->um_ubinfo) {
			printf("<%d>", (um->um_ubinfo>>28)&0xf);
			um->um_ubinfo = 0;
		}
		((struct utdevice *)(um->um_addr))->utcs1 = UT_CLEAR|UT_GO;
		((struct utdevice *)(um->um_addr))->utcs2 |= UTCS2_CLR;
		for (tjunit = 0; tjunit < NTJ; tjunit++) {
			if ((ui = tjdinfo[tjunit]) == 0 || ui->ui_mi != um ||
			    ui->ui_alive == 0)
				continue;
			dp = &tjutab[tjunit];
			dp->b_state = 0;
			dp->b_forw = 0;
			if (um->um_tab.b_actf == NULL)
				um->um_tab.b_actf = dp;
			else
				um->um_tab.b_actl->b_forw = dp;
			um->um_tab.b_actl = dp;
			if (tj_softc[tjunit].sc_openf > 0)
				tj_softc[tjunit].sc_openf = -1;
		}
		utstart(um);
	}
}

/*
 * Do a stand-alone core dump to tape --
 * from here down, routines are used only in dump context
 */
#define	DBSIZE	20

utdump()
{
	register struct uba_device *ui;
	register struct uba_regs *up;
	register struct utdevice *addr;
	int blk, num = maxfree;
	int start = 0;

#define	phys(a,b)		((b)((int)(a)&0x7fffffff))
	if (tjdinfo[0] == 0)
		return (ENXIO);
	ui = phys(tjdinfo[0], struct uba_device *);
	up = phys(ui->ui_hd, struct uba_hd *)->uh_physuba;
	ubainit(up);
	DELAY(1000000);
	addr = (struct utdevice *)ui->ui_physaddr;
	utwait(addr);
	/*
	 * Be sure to set the appropriate density here.  We use
	 * 6250, but maybe it should be done at 1600 to insure the
	 * tape can be read by most any other tape drive available.
	 */
	addr->uttc = UT_GCR|PDP11FMT;	/* implicit slave 0 or-ed in */
	addr->utcs1 = UT_CLEAR|UT_GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		utdwrite(start, blk, addr, up);
		if ((addr->utds&UTDS_ERR) || (addr->utcs1&UT_TRE))
			return(EIO);
		start += blk;
		num -= blk;
	}
	uteof(addr);
	uteof(addr);
	utwait(addr);
	if ((addr->utds&UTDS_ERR) || (addr->utcs1&UT_TRE))
		return(EIO);
	addr->utcs1 = UT_REW|UT_GO;
	return (0);
}

utdwrite(dbuf, num, addr, up)
	register dbuf, num;
	register struct utdevice *addr;
	struct uba_regs *up;
{
	register struct pte *io;
	register int npf;

	utwait(addr);
	io = up->uba_map;
	npf = num + 1;
	while (--npf != 0)
		*(int *)io++ = (dbuf++ | (1<<UBAMR_DPSHIFT) | UBAMR_MRV);
	*(int *)io = 0;
	addr->utwc = -((num*NBPG)>>1);
	addr->utfc = -(num*NBPG);
	addr->utba = 0;
	addr->utcs1 = UT_WCOM|UT_GO;
}

utwait(addr)
	struct utdevice *addr;
{
	register s;

	do
		s = addr->utds;
	while ((s&UTDS_DRY) == 0);
}

uteof(addr)
	struct utdevice *addr;
{

	utwait(addr);
	addr->utcs1 = UT_WEOF|UT_GO;
}
#endif
