/*-
 * Copyright (c) 1982, 1986 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.proprietary.c%
 *
 *	@(#)mt.c	7.10 (Berkeley) %G%
 */

#include "mu.h"
#if NMT > 0
/*
 * TM78/TU78 tape driver
 *
 *	Original author - ?
 *	Most error recovery bug fixes - ggs (ulysses!ggs)
 *	`read reverse' error recovery - ggs (ulysses!ggs)
 *
 * OPTIONS:
 *	MTLERRM - Long error message text - twd, Brown University
 *
 * TODO:
 *	Add odd byte count kludge from VMS driver (?)
 *	Write dump routine
 */

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/conf.h"
#include "sys/file.h"
#include "sys/user.h"
#include "sys/proc.h"
#include "sys/map.h"
#include "sys/ioctl.h"
#include "sys/mtio.h"
#include "sys/cmap.h"
#include "sys/tty.h"
#include "sys/syslog.h"

#include "../include/pte.h"
#include "../include/cpu.h"
#include "mbareg.h"
#include "mbavar.h"
#include "mtreg.h"

#define MTTIMEOUT	10000		/* loop limit for controller test */
#define	INF		1000000L	/* a block number that won't exist */
#define MASKREG(r)	((r) & 0xffff)	/* the control registers have 16 bits */

/* Bits for sc_flags */

#define	H_WRITTEN	01		/* last operation was a write */
#define H_EOT		02		/* end of tape encountered */
#define H_IEOT		04		/* ignore EOT condition */

int	mt_do_readrev = 1;

/* Per unit status information */

struct	mu_softc {
	char	sc_openf;	/* unit is open if != 0 */
	char	sc_flags;	/* state flags */
	daddr_t	sc_blkno;	/* current physical block number */
	daddr_t	sc_nxrec;	/* firewall input block number */
	u_short	sc_erreg;	/* copy of mter or mtner */
	u_short	sc_dsreg;	/* copy of mtds */
	short	sc_resid;	/* residual function count for ioctl */
	short	sc_dens;	/* density code - MT_GCR or zero */
	int	sc_i_mtas;	/* mtas at slave attach time */
	int	sc_i_mtner;	/* mtner at slave attach time */
	int	sc_i_mtds;	/* mtds at slave attach time */
	caddr_t	sc_ctty;	/* record user's tty for errors */
	int	sc_blks;	/* number of I/O operations since open */
	int	sc_softerrs;	/* number of soft I/O errors since open */
} mu_softc[NMU];

struct	buf	cmtbuf[NMT];		/* tape command buffer structures */

struct	mba_device *mtinfo[NMT];	/* unit to ctlr structures */
struct	mba_slave *muinfo[NMU];		/* unit to slave structures */

char	mtds_bits[] = MTDS_BITS;	/* mtds bit names for error messages */
short	mttypes[] = { MBDT_TU78, 0 };

int	mtattach(), mtslave(), mtustart(), mtstart(), mtndtint(), mtdtint();
struct	mba_driver mtdriver =
	{ mtattach, mtslave, mtustart, mtstart, mtdtint, mtndtint,
	  mttypes, "mt", "mu", mtinfo };

/* Bits in minor device */
#define	MUUNIT(dev)	(minor(dev)&03)
#define	H_NOREWIND	04
#define	H_6250BPI	010

#define MTUNIT(dev)	(muinfo[MUUNIT(dev)]->ms_ctlr)

void	mtcreset();

/*ARGSUSED*/
mtattach(mi)
	struct mba_device *mi;
{

	/* void */
}

mtslave(mi, ms, sn)
	struct mba_device *mi;
	struct mba_slave *ms;
	int sn;
{
	register struct mu_softc *sc = &mu_softc[ms->ms_unit];
	register struct mtdevice *mtaddr = (struct mtdevice *)mi->mi_drv;
	int s = spl5(), rtn = 0, i;

	/*
	 * Just in case the controller is ill, reset it.  Then issue
	 * a sense operation and wait about a second for it to respond.
	 */
	mtcreset(mtaddr);
	mtaddr->mtas = -1;
	mtaddr->mtncs[sn] = MT_SENSE|MT_GO;
	for (i = MTTIMEOUT; i > 0; i--) {
		DELAY(50);
		if (MASKREG(mtaddr->mtas) != 0)
			break;
	}
	sc->sc_i_mtas = mtaddr->mtas;
	sc->sc_i_mtner = mtaddr->mtner;
	sc->sc_i_mtds = mtaddr->mtds;

	/*
	 * If no response, whimper.  If wrong response, call it an
	 * unsolicited interrupt and use mtndtint to log and correct.
	 * Otherwise, note whether this slave exists.
	 */
	if (i <= 0)
		printf("mt: controller hung\n");
	else if ((mtaddr->mtner & MTER_INTCODE) != MTER_DONE)
		(void) mtndtint(mi);
	else if (mtaddr->mtds & MTDS_PRES) {
		muinfo[ms->ms_unit] = ms;
		rtn = 1;
	}

	/* cancel the interrupt, then wait a little while for it to go away */
	mtaddr->mtas = mtaddr->mtas;
	DELAY(10);
	splx(s);
	return (rtn);
}

mtopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int muunit;
	register struct mu_softc *sc;
	register struct mba_slave *ms;

	muunit = MUUNIT(dev);
	if (muunit >= NMU || (ms = muinfo[muunit]) == NULL ||
	    ms->ms_alive == 0 || mtinfo[ms->ms_ctlr]->mi_alive == 0)
		return (ENXIO);
	if ((sc = &mu_softc[muunit])->sc_openf)
		return (EBUSY);
	sc->sc_openf = 1;
	sc->sc_dens = (minor(dev) & H_6250BPI) ? MT_GCR : 0;
	mtcommand(dev, MT_SENSE, 1);
	if ((sc->sc_dsreg & MTDS_ONL) == 0) {
		uprintf("mu%d: not online\n", muunit);
		sc->sc_openf = 0;
		return (EIO);
	}
	if ((sc->sc_dsreg & MTDS_AVAIL) == 0) {
		uprintf("mu%d: not online (port selector)\n", muunit);
		sc->sc_openf = 0;
		return (EIO);
	}
	if ((flag & FWRITE) && (sc->sc_dsreg & MTDS_FPT)) {
		uprintf("mu%d: no write ring\n", muunit);
		sc->sc_openf = 0;
		return (EIO);
	}
	if ((sc->sc_dsreg & MTDS_BOT) == 0 && (flag & FWRITE) &&
	    (sc->sc_dens == MT_GCR) != ((sc->sc_dsreg & MTDS_PE) == 0)) {
		uprintf("mu%d: can't change density in mid-tape\n", muunit);
		sc->sc_openf = 0;
		return (EIO);
	}
	sc->sc_blkno = (daddr_t)0;

	/*
	 * Since cooked I/O may do a read-ahead before a write, trash
	 * on a tape can make the first write fail.  Suppress the first
	 * read-ahead unless definitely doing read-write.
	 */
	sc->sc_nxrec = ((flag & (FTRUNC | FWRITE)) == (FTRUNC | FWRITE)) ?
	    (daddr_t)0 : (daddr_t)INF;
	sc->sc_flags = 0;
	sc->sc_blks = 0;
	sc->sc_softerrs = 0;
	sc->sc_ctty = (caddr_t)(u.u_procp->p_flag&SCTTY ? 
			u.u_procp->p_session->s_ttyp : 0);
	return (0);
}

mtclose(dev, flag)
	register dev_t dev;
	register int flag;
{
	register struct mu_softc *sc = &mu_softc[MUUNIT(dev)];

	if ((flag & (FREAD | FWRITE)) == FWRITE ||
	    ((flag & FWRITE) && (sc->sc_flags & H_WRITTEN)))
		mtcommand(dev, MT_CLS|sc->sc_dens, 1);
	if ((minor(dev) & H_NOREWIND) == 0)
		mtcommand(dev, MT_REW, 0);
	if (sc->sc_blks > 100 && sc->sc_softerrs > sc->sc_blks / 100)
		log(LOG_INFO, "mu%d: %d soft errors in %d blocks\n",
		    MUUNIT(dev), sc->sc_softerrs, sc->sc_blks);
	sc->sc_openf = 0;
	return (0);
}

mtcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;
	int s;

	bp = &cmtbuf[MTUNIT(dev)];
	s = spl5();
	while (bp->b_flags & B_BUSY) {
		if (bp->b_repcnt == 0 && (bp->b_flags & B_DONE))
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
	bp->b_error = 0;
	mtstrategy(bp);
	if (count == 0)
		return;
	biowait(bp);
	if (bp->b_flags & B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

mtstrategy(bp)
	register struct buf *bp;
{
	register struct buf *dp;
	struct mba_device *mi = mtinfo[MTUNIT(bp->b_dev)];
	int s;

	/*
	 * If this is a data transfer operation, set the resid to a
	 * default value (EOF) to simplify getting it right during
	 * error recovery or bail out.
	 */
	if (bp != &cmtbuf[MTUNIT(bp->b_dev)])
		bp->b_resid = bp->b_bcount;

	/*
	 * Link this request onto the end of the queue for this
	 * controller, then start I/O if not already active.
	 */
	bp->av_forw = NULL;
	dp = &mi->mi_tab;
	s = spl5();
	if (dp->b_actf == NULL)
		dp->b_actf = bp;
	else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	if (dp->b_active == 0)
		mbustart(mi);
	splx(s);
}

mtustart(mi)
	register struct mba_device *mi;
{
	register struct mtdevice *mtaddr = (struct mtdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct mu_softc *sc = &mu_softc[MUUNIT(bp->b_dev)];
	daddr_t blkno;
	int count;

	if (sc->sc_openf < 0) {
		bp->b_flags |= B_ERROR;
		return (MBU_NEXT);
	}
	if (bp != &cmtbuf[MTUNIT(bp->b_dev)]) {
		/*
		 * Data transfer.  If write at end of tape,
		 * signal "no space" unless suppressed
		 * by MTIOCIEOT.
		 */
		if ((sc->sc_flags & (H_EOT | H_IEOT)) == H_EOT &&
		    (bp->b_flags & B_READ) == 0) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENOSPC;
			return (MBU_NEXT);
		}

		if (bp->b_flags & B_RAW) {
			/* raw transfer; never seek */
			sc->sc_blkno = bdbtofsb(bp->b_blkno);
			sc->sc_nxrec = sc->sc_blkno + 1;
		} else {
			/* seek beyond end of file */
			if (bdbtofsb(bp->b_blkno) > sc->sc_nxrec) {
				bp->b_flags |= B_ERROR;
				bp->b_error = ENXIO;
				return (MBU_NEXT);
			}

			/*
			 * This should be end of file, but the buffer
			 * system wants a one-block look-ahead.  Humor it.
			 */
			if (bdbtofsb(bp->b_blkno) == sc->sc_nxrec &&
			    bp->b_flags & B_READ) {
				bp->b_resid = bp->b_bcount;
				clrbuf(bp);
				return (MBU_NEXT);
			}

			/* If writing, mark the next block invalid. */
			if ((bp->b_flags & B_READ) == 0)
				sc->sc_nxrec = bdbtofsb(bp->b_blkno) + 1;
		}
	} else {
		/* It's a command, do it now. */
		mtaddr->mtncs[MUUNIT(bp->b_dev)] =
			(bp->b_repcnt<<8)|bp->b_command|MT_GO;
		return (MBU_STARTED);
	}

	/*
	 * If raw I/O, or if the tape is positioned correctly for
	 * cooked I/O, set the byte count, unit number and repeat count
	 * then tell the MASSBUS to proceed.  Note that a negative
	 * bcount tells mbstart to map the buffer for "read backwards".
	 */
	if ((blkno = sc->sc_blkno) == bdbtofsb(bp->b_blkno)) {
		if (mi->mi_tab.b_errcnt == 2) {
			mtaddr->mtbc = -bp->b_bcount;
			mtaddr->mtca = MUUNIT(bp->b_dev);
		} else {
			mtaddr->mtbc = bp->b_bcount;
			mtaddr->mtca = (1<<2)|MUUNIT(bp->b_dev);
		}
		return (MBU_DODATA);
	}

	/* Issue skip operations to position the next block for cooked I/O. */

	if (blkno < bdbtofsb(bp->b_blkno))
		count = bdbtofsb(bp->b_blkno) - blkno;
	else
		count = blkno - bdbtofsb(bp->b_blkno);
	if ((unsigned)count > 0377)
		count = 0377;
	mtaddr->mtncs[MUUNIT(bp->b_dev)] = count | MT_SFORW|MT_GO;
	return (MBU_STARTED);
}

mtstart(mi)
	register struct mba_device *mi;
{
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct mu_softc *sc = &mu_softc[MUUNIT(bp->b_dev)];

	if (bp->b_flags & B_READ)
		if (mi->mi_tab.b_errcnt == 2)
			return (MT_READREV|MT_GO);
		else
			return (MT_READ|MT_GO);
	else
		return (MT_WRITE|sc->sc_dens|MT_GO);
}

mtdtint(mi, mbsr)
	register struct mba_device *mi;
	int mbsr;
{
	register struct mtdevice *mtaddr = (struct mtdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct mu_softc *sc;
	register int er;

	/* I'M still NOT SURE IF THIS SHOULD ALWAYS BE THE CASE SO FOR NOW... */
	if ((mtaddr->mtca & 3) != MUUNIT(bp->b_dev)) {
		printf("mt: wrong unit!\n");
		mtaddr->mtca = MUUNIT(bp->b_dev);
	}

	er = MASKREG(mtaddr->mter);
	sc = &mu_softc[MUUNIT(bp->b_dev)];
	sc->sc_erreg = er;
	if (bp->b_flags & B_READ)
		sc->sc_flags &= ~H_WRITTEN;
	else
		sc->sc_flags |= H_WRITTEN;
	switch (er & MTER_INTCODE) {

	case MTER_EOT:
		sc->sc_flags |= H_EOT;
		/* fall into MTER_DONE */

	case MTER_DONE:
		sc->sc_blkno++;
		if (mi->mi_tab.b_errcnt == 2) {
			bp->b_bcount = bp->b_resid;
			bp->b_resid -= MASKREG(mtaddr->mtbc);
			if (bp->b_resid > 0 && (bp->b_flags & B_RAW) == 0)
				bp->b_flags |= B_ERROR;
		} else
			bp->b_resid = 0;
		break;

	case MTER_SHRTREC:
		sc->sc_blkno++;
		bp->b_bcount = bp->b_resid;
		bp->b_resid -= MASKREG(mtaddr->mtbc);
		if ((bp->b_flags & B_RAW) == 0)
			bp->b_flags |= B_ERROR;
		break;

	case MTER_RETRY:
		/*
		 * Simple re-try.  Since resid is always a copy of the
		 * original byte count, use it to restore the count.
		 */
		mi->mi_tab.b_errcnt = 1;
		bp->b_bcount = bp->b_resid;
		return (MBD_RETRY);

	case MTER_RDOPP:
		/*
		 * The controller just decided to read it backwards.
		 * If the controller returns a byte count of zero,
		 * change it to 1, since zero encodes 65536, which
		 * isn't quite what we had in mind.  The byte count
		 * may be larger than the size of the input buffer, so
		 * limit the count to the buffer size.  After
		 * making the byte count reasonable, set bcount to the
		 * negative of the controller's version of the byte
		 * count so that the start address for the transfer is
		 * set up correctly.
		 */
		if (mt_do_readrev) {
			mi->mi_tab.b_errcnt = 2;
			if ((bp->b_bcount = MASKREG(mtaddr->mtbc)) == 0)
				bp->b_bcount = 1;
			if (bp->b_bcount > bp->b_resid)
				bp->b_bcount = bp->b_resid;
			bp->b_bcount = -(bp->b_bcount);
			return(MBD_RETRY);
		} else if (MASKREG(mtaddr->mtbc) <= bp->b_resid) {
			sc->sc_blkno++;
			bp->b_bcount = bp->b_resid;
			bp->b_resid -= MASKREG(mtaddr->mtbc);
			bp->b_flags |= B_ERROR;
			break;
		}
		bp->b_flags |= B_ERROR;
		/* fall into MTER_LONGREC */

	case MTER_LONGREC:
		sc->sc_blkno++;
		bp->b_bcount = bp->b_resid;
		bp->b_resid = 0;
		bp->b_error = ENOMEM;
		bp->b_flags |= B_ERROR;
		break;

	case MTER_NOTCAP:
		printf("mu%d: blank tape\n", MUUNIT(bp->b_dev));
		goto err;

	case MTER_TM:
		/*
		 * End of file.  Since the default byte count has
		 * already been set, just count the block and proceed.
		 */
		sc->sc_blkno++;
	err:
		sc->sc_nxrec = bdbtofsb(bp->b_blkno);
		break;

	case MTER_OFFLINE:
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			tprintf(sc->sc_ctty, "mu%d: offline\n",
			    MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		break;

	case MTER_NOTAVL:
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			tprintf(sc->sc_ctty, "mu%d: offline (port selector)\n",
			    MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		break;

	case MTER_FPT:
		tprintf(sc->sc_ctty, "mu%d: no write ring\n",
		    MUUNIT(bp->b_dev));
		bp->b_flags |= B_ERROR;
		break;

	case MTER_UNREAD:
		sc->sc_blkno++;
		bp->b_bcount = bp->b_resid;
		bp->b_resid -= MIN(MASKREG(mtaddr->mtbc), bp->b_bcount);

		/* code 010 means a garbage record, nothing serious. */
		if ((er & MTER_FAILCODE) == (010 << MTER_FSHIFT)) {
			tprintf(sc->sc_ctty,
			    "mu%d: rn=%d bn=%d unreadable record\n",
			    MUUNIT(bp->b_dev), sc->sc_blkno, bp->b_blkno);
			bp->b_flags |= B_ERROR;
			break;
		}

		/*
		 * Anything else might be a hardware problem,
		 * fall into the error report.
		 */

	default:
		/*
		 * The bits in sc->sc_dsreg are from the last sense
		 * command.  To get the most recent copy, you have to
		 * do a sense at interrupt level, which requires nested
		 * error processing.  This is a bit messy, so leave
		 * well enough alone.
		 */
		tprintf(sc->sc_ctty, "\
mu%d: hard error (data transfer) rn=%d bn=%d mbsr=%b er=0%o ds=%b\n",
		    MUUNIT(bp->b_dev), sc->sc_blkno, bp->b_blkno,
		    mbsr, mbsr_bits, er,
		    MASKREG(sc->sc_dsreg), mtds_bits);
#ifdef MTLERRM
		mtintfail(er);
#endif
		bp->b_flags |= B_ERROR;

		/*
		 * The TM78 manual says to reset the controller after
		 * TM fault B or MASSBUS fault.
		 */
		if ((er & MTER_INTCODE) == MTER_TMFLTB ||
		    (er & MTER_INTCODE) == MTER_MBFLT)
			mtcreset(mtaddr);
	}

	/*
	 * Just in case some strange error slipped through (drive off
	 * line during read-reverse error recovery comes to mind), make
	 * sure the byte count is reasonable.
	 */
	if (bp->b_bcount < 0)
		bp->b_bcount = bp->b_resid;

	if ((bp->b_flags & B_ERROR) == 0) {
		/* this counts reverse reads as soft errors */
		sc->sc_blks++;
		if (mi->mi_tab.b_errcnt) /* alternatively, if == 1 */
			sc->sc_softerrs++;
	}
	return (MBD_DONE);
}

mtndtint(mi)
	register struct mba_device *mi;
{
	register struct mtdevice *mtaddr = (struct mtdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct mu_softc *sc;
	register int er, fc;
	int unit;

	unit = (mtaddr->mtner >> 8) & 3;
	er = MASKREG(mtaddr->mtner);
	sc = &mu_softc[unit];
	sc->sc_erreg = er;

	/* Check for unsolicited interrupts. */
	if (bp == NULL || unit != MUUNIT(bp->b_dev)) {
		if ((er & MTER_INTCODE) == MTER_ONLINE)
			return (MBN_SKIP);

		printf("mu%d: stray intr (non data transfer) er=0%o ds=%b\n",
		    unit, er, MASKREG(sc->sc_dsreg), mtds_bits);
#ifdef MTLERRM
		mtintfail(er);
#endif
		if ((er & MTER_INTCODE) == MTER_TMFLTB ||
		    (er & MTER_INTCODE) == MTER_MBFLT) {
			/*
			 * Reset the controller, then set error status
			 * if there was anything active when the fault
			 * occurred.  This may shoot an innocent
			 * bystander, but it's better than letting
			 * an error slip through.
			 */
			mtcreset(mtaddr);
			if (bp != NULL) {
				bp->b_flags |= B_ERROR;
				return (MBN_DONE);
			}
		}
		return (MBN_SKIP);
	}

	fc = (mtaddr->mtncs[unit] >> 8) & 0xff;
	sc->sc_resid = fc;

	/*
	 * Clear the "written" flag after any operation that changes
	 * the position of the tape.
	 */
	if (bp != &cmtbuf[MTUNIT(bp->b_dev)] || bp->b_command != MT_SENSE)
		sc->sc_flags &= ~H_WRITTEN;

	switch (er & MTER_INTCODE) {

	case MTER_EOT:
		sc->sc_flags |= H_EOT;
		/* fall into MTER_DONE */

	case MTER_DONE:
		/* If this is a command buffer, just update the status.	*/
		if (bp == &cmtbuf[MTUNIT(bp->b_dev)]) {
	done:
			if (bp->b_command == MT_SENSE)
				sc->sc_dsreg = MASKREG(mtaddr->mtds);
			return (MBN_DONE);
		}

		/*
		 * It's not a command buffer, must be a cooked I/O
		 * skip operation (perhaps a shaky assumption, but it
		 * wasn't my idea).
		 */
		if ((fc = bdbtofsb(bp->b_blkno) - sc->sc_blkno) < 0)
			sc->sc_blkno -= MIN(0377, -fc);
		else
			sc->sc_blkno += MIN(0377, fc);
		return (MBN_RETRY);

	case MTER_ONLINE:		/* ddj -- shouldn't happen but did */
	case MTER_RWDING:
		return (MBN_SKIP);	/* ignore "rewind started" interrupt */

	case MTER_NOTCAP:
		tprintf(sc->sc_ctty, "mu%d: blank tape\n", MUUNIT(bp->b_dev));
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_TM:
	case MTER_LEOT:
		/*
		 * For an ioctl skip operation, count a tape mark as
		 * a record.  If there's anything left to do, update
		 * the repeat count and re-start the command.
		 */
		if (bp == &cmtbuf[MTUNIT(bp->b_dev)]) {
			if ((sc->sc_resid = bp->b_repcnt = fc - 1) == 0)
				return (MBN_DONE);
			else
				return (MBN_RETRY);
		} else {
			/*
			 * Cooked I/O again.  Just update the books and
			 * wait for someone else to return end of file or
			 * complain about a bad seek.
			 */
			if (sc->sc_blkno > bdbtofsb(bp->b_blkno)) {
				sc->sc_nxrec = bdbtofsb(bp->b_blkno) + fc - 1;
				sc->sc_blkno = sc->sc_nxrec;
			} else {
				sc->sc_nxrec = bdbtofsb(bp->b_blkno) - fc;
				sc->sc_blkno = sc->sc_nxrec + 1;
			}
		}
		return (MBN_RETRY);

	case MTER_FPT:
		tprintf(sc->sc_ctty, "mu%d: no write ring\n",
		    MUUNIT(bp->b_dev));
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_OFFLINE:
		/* If `off line' was intentional, don't complain. */
		if (bp == &cmtbuf[MTUNIT(bp->b_dev)] &&
		    bp->b_command == MT_UNLOAD)
			return(MBN_DONE);
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			tprintf(sc->sc_ctty, "mu%d: offline\n",
			    MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_NOTAVL:
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			tprintf(sc->sc_ctty, "mu%d: offline (port selector)\n",
			    MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_BOT:
		if (bp == &cmtbuf[MTUNIT(bp->b_dev)])
			goto done;
		/* fall through */

	default:
		tprintf(sc->sc_ctty, "\
mu%d: hard error (non data transfer) rn=%d bn=%d er=0%o ds=%b\n",
		    MUUNIT(bp->b_dev), sc->sc_blkno, bp->b_blkno,
		    er, MASKREG(sc->sc_dsreg), mtds_bits);
#ifdef MTLERRM
		mtintfail(er);
#endif
		if ((er & MTER_INTCODE) == MTER_TMFLTB ||
		    (er & MTER_INTCODE) == MTER_MBFLT)
			mtcreset(mtaddr);	/* reset the controller */
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);
	}
	/* NOTREACHED */
}

void
mtcreset(mtaddr)
	register struct mtdevice *mtaddr;
{
	register int i;

	mtaddr->mtid = MTID_CLR;		/* reset the TM78 */
	DELAY(200);
	for (i = MTTIMEOUT; i > 0; i--) {
		DELAY(50);			/* don't nag */
		if ((mtaddr->mtid & MTID_RDY) != 0)
			return;			/* exit when ready */
	}
	printf("mt: controller hung\n");
}

/*ARGSUSED*/
mtioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	register struct mu_softc *sc = &mu_softc[MUUNIT(dev)];
	register struct buf *bp = &cmtbuf[MTUNIT(dev)];
	register struct mtop *mtop;
	register struct mtget *mtget;
	int callcount, fcount, error = 0;
	int op;

	/* We depend on the values and order of the MT codes here. */

	static mtops[] =
	{MT_WTM,MT_SFORWF,MT_SREVF,MT_SFORW,MT_SREV,MT_REW,MT_UNLOAD,MT_SENSE};

	switch (cmd) {

	/* tape operation */

	case MTIOCTOP:
		mtop = (struct mtop *)data;
		switch (mtop->mt_op) {

		case MTWEOF:
			callcount = mtop->mt_count;
			fcount = 1;
			break;

		case MTFSF: case MTBSF:
			callcount = mtop->mt_count;
			fcount = 1;
			break;

		case MTFSR: case MTBSR:
			callcount = 1;
			fcount = mtop->mt_count;
			break;

		case MTREW: case MTOFFL:
			callcount = 1;
			fcount = 1;
			break;

		default:
			return (ENXIO);
		}
		if (callcount <= 0 || fcount <= 0)
			return (EINVAL);
		op = mtops[mtop->mt_op];
		if (op == MT_WTM)
			op |= sc->sc_dens;
		while (--callcount >= 0) {
			register int n, fc = fcount;

			do {
				n = MIN(fc, 0xff);
				mtcommand(dev, op, n);
				n -= sc->sc_resid;
				fc -= n;
				switch (mtop->mt_op) {

				case MTWEOF:
					sc->sc_blkno += (daddr_t)n;
					sc->sc_nxrec = sc->sc_blkno - 1;
					break;

				case MTOFFL:
				case MTREW:
				case MTFSF:
					sc->sc_blkno = (daddr_t)0;
					sc->sc_nxrec = (daddr_t)INF;
					break;

				case MTBSF:
					if (sc->sc_resid) {
						sc->sc_blkno = (daddr_t)0;
						sc->sc_nxrec = (daddr_t)INF;
					} else {
						sc->sc_blkno = (daddr_t)(-1);
						sc->sc_nxrec = (daddr_t)(-1);
					}
					break;

				case MTFSR:
					sc->sc_blkno += (daddr_t)n;
					break;

				case MTBSR:
					sc->sc_blkno -= (daddr_t)n;
					break;
				}
				if (sc->sc_resid)
					break;
			} while (fc);
			if (fc) {
				sc->sc_resid = callcount + fc;
				if (mtop->mt_op == MTFSR ||
				    mtop->mt_op == MTBSR)
					return (EIO);
				break;
			}
			if (bp->b_flags & B_ERROR)
				break;
		}
		if (bp->b_flags&B_ERROR)
			if ((error = bp->b_error)==0)
				return (EIO);
		return (error);

	/* tape status */
	case MTIOCGET:
		mtget = (struct mtget *)data;
		mtget->mt_erreg = sc->sc_erreg;
		mtget->mt_resid = sc->sc_resid;
		mtcommand(dev, MT_SENSE, 1);	/* update drive status */
		mtget->mt_dsreg = sc->sc_dsreg;
		mtget->mt_type = MT_ISMT;
		break;

	/* ignore EOT condition */
	case MTIOCIEOT:
		sc->sc_flags |= H_IEOT;
		break;

	/* enable EOT condition */
	case MTIOCEEOT:
		sc->sc_flags &= ~H_IEOT;
		break;

	default:
		return (ENXIO);
	}
	return (0);
}

#define	DBSIZE	20

mtdump()
{
	register struct mba_device *mi;
	register struct mba_regs *mp;
	int blk, num;
	int start;

	start = 0;
	num = maxfree;
#define	phys(a,b)		((b)((int)(a)&0x7fffffff))
	if (mtinfo[0] == 0)
		return (ENXIO);
	mi = phys(mtinfo[0], struct mba_device *);
	mp = phys(mi->mi_hd, struct mba_hd *)->mh_physmba;
	mp->mba_cr = MBCR_IE;
#if lint
	blk = 0; num = blk; start = num; blk = start;
	return (0);
#endif
#ifdef notyet
	mtaddr = (struct mtdevice *)&mp->mba_drv[mi->mi_drive];
	mtaddr->mttc = MTTC_PDP11|MTTC_1600BPI;
	mtaddr->mtcs1 = MT_DCLR|MT_GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		mtdwrite(start, blk, mtaddr, mp);
		start += blk;
		num -= blk;
	}
	mteof(mtaddr);
	mteof(mtaddr);
	mtwait(mtaddr);
	if (mtaddr->mtds&MTDS_ERR)
		return (EIO);
	mtaddr->mtcs1 = MT_REW|MT_GO;
	return (0);
}

mtdwrite(dbuf, num, mtaddr, mp)
	register dbuf, num;
	register struct mtdevice *mtaddr;
	struct mba_regs *mp;
{
	register struct pte *io;
	register int i;

	mtwait(mtaddr);
	io = mp->mba_map;
	for (i = 0; i < num; i++)
		*(int *)io++ = dbuf++ | PG_V;
	mtaddr->mtfc = -(num*NBPG);
	mp->mba_sr = -1;
	mp->mba_bcr = -(num*NBPG);
	mp->mba_var = 0;
	mtaddr->mtcs1 = MT_WCOM|MT_GO;
}

mtwait(mtaddr)
	struct mtdevice *mtaddr;
{
	register s;

	do
		s = mtaddr->mtds;
	while ((s & MTDS_DRY) == 0);
}

mteof(mtaddr)
	struct mtdevice *mtaddr;
{

	mtwait(mtaddr);
	mtaddr->mtcs1 = MT_WEOF|MT_GO;
#endif notyet
}

#ifdef MTLERRM
/*
 * Failure messages for each failure code, per interrupt code.
 * Each table ends with a code of -1 as a default.
 */
struct fmesg {
	int	f_code;
	char	*f_mesg;
};

static char unclass[] = "unclassified failure code";

/* MTER_BOT */
static struct fmesg botmsg[] = {
	01,	"tape was at BOT",
	02,	"BOT seen after tape started",
	03,	"ARA ID detected",
	-1,	unclass
};

/* MTER_NOTRDY */
static struct fmesg notrdymsg[] = {
	01,	"TU on-line but not ready",
	02,	"fatal error has occurred",
	03,	"access allowed but not ready",
	-1,	unclass
};

/* MTER_NOTCAP */
static struct fmesg notcapmsg[] = {
	01,	"no record found within 25 feet",
	02,	"ID burst neither PE nor GCR",
	03,	"ARA ID not found",
	04,	"no gap found after ID burst",
	-1,	unclass
};

/* MTER_LONGREC */
static struct fmesg longrecmsg[] = {
	00,	"extended sense data not found",
	01,	"extended sense data updated",
	-1,	unclass
};

/* MTER_UNREAD, MTER_ERROR, MTER_EOTERR, MTER_BADTAPE */
static struct fmesg code22msg[] = {
	01,	"GCR write error",
	02,	"GCR read error",
	03,	"PE read error",
	04,	"PE write error",
	05,	"at least 1 bit set in ECCSTA",
	06,	"PE write error",
	07,	"GCR write error",
	010,	"RSTAT contains bad code",
	011,	"PE write error",
	012,	"MASSBUS parity error",
	013,	"invalid data transferred",
	-1,	unclass
};

/* MTER_TMFLTA */
static struct fmesg tmfltamsg[] = {
	01,	"illegal command code",
	02,	"DT command issued when NDT command active",
	03,	"WMC error",
	04,	"RUN not received from MASSBUS controller",
	05,	"mismatch in command read - function routine",
	06,	"ECC ROM parity error",
	07,	"XMC ROM parity error",
	010,	"mismatch in command read - ID burst command",
	011,	"mismatch in command read - verify ARA burst command",
	012,	"mismatch in command read - verify ARA ID command",
	013,	"mismatch in command read - verify gap command",
	014,	"mismatch in command read - read id burst command",
	015,	"mismatch in command read - verify ARA ID command",
	016,	"mismatch in command read - verify gap command",
	017,	"mismatch in command read - find gap command",
	020,	"WMC LEFT failed to set",
	021,	"XL PE set in INTSTA register",
	022,	"XMC DONE did not set",
	023,	"WMC ROM PE or RD PE set in WMCERR register",
	-1,	unclass
};

/* MTER_TUFLTA */
static struct fmesg tufltamsg[] = {
	01,	"TU status parity error",
	02,	"TU command parity error",
	03,	"rewinding tape went offline",
	04,	"tape went not ready during DSE",
	05,	"TU CMD status changed during DSE",
	06,	"TU never came up to speed",
	07,	"TU velocity changed",
	010,	"TU CMD did not load correctly to start tape motion",
	011,	"TU CMD did not load correctly to set drive density",
	012,	"TU CMD did not load correctly to start tape motion to write BOT ID",
	013,	"TU CMD did not load correctly to backup tape to BOT after failing to write BOT ID",
	014,	"failed to write density ID burst",
	015,	"failed to write ARA burst",
	016,	"failed to write ARA ID",
	017,	"ARA error bit set in MTA status B register",
	021,	"could not find a gap after ID code was written correctly",
	022,	"TU CMD did not load correctly to start tape motion to read ID burst",
	023,	"timeout looking for BOT after detecting ARA ID burst",
	024,	"failed to write tape mark",
	025,	"tape never came up to speed while trying to reposition for retry of writing tape mark",
	026,	"TU CMD did not load correctly to start tape motion in erase gap routine",
	027,	"could not detect a gap in in erase gap routine",
	030,	"could not detect a gap after writing record",
	031,	"read path terminated before entire record was written",
	032,	"could not find a gap after writing record and read path terminated early",
	033,	"TU CMD did not load correctly to backup for retry of write tape mark",
	034,	"TU velocity changed after up to speed while trying to reposition for retry of writing tape mark",
	035,	"TU CMD did not load correctly to backup to retry a load of BOT ID",
	036,	"timeout looking for BOT after failing to write BOT ID",
	037,	"TU velocity changed while writing PE gap before starting to write record",
	040,	"TU CMD did not load correctly to set PE tape density at start of write BOT ID burst",
	041,	"TU CMD did not load correctly to set GCR tape density after writing Density ID",
	042,	"TU CMD did not load correctly to set PE tape density at start of read from BOT",
	043,	"TU CMD did not load correctly to set GCR tape density after reading a GCR Density ID burst",
};

/* MTER_TMFLTB */
static char inlinetest[] = "inline test failed";
static struct fmesg tmfltbmsg[] = {
	00,	"RST0 interrupt occurred with TM RDY set",
	01,	"power failed to interrupt",
	02,	"unknown interrupt on channel 5.5",
	03,	"unknown interrupt on channel 6.5",
	04,	"unknown interrupt on channel 7",
	05,	"unknown interrupt on channel 7.5",
	06,	"CAS contention retry count expired",
	07,	"CAS contention error not retryable",
	010,	"queue error, could not find queue entry",
	011,	"queue entry already full",
	012,	"8085 ROM parity error",
	013,	inlinetest,
	013,	inlinetest,
	014,	inlinetest,
	015,	inlinetest,
	016,	inlinetest,
	017,	inlinetest,
	020,	inlinetest,
	021,	inlinetest,
	022,	inlinetest,
	023,	inlinetest,
	024,	inlinetest,
	025,	inlinetest,
	026,	inlinetest,
	027,	inlinetest,
	030,	inlinetest,
	031,	inlinetest,
	032,	inlinetest,
	033,	inlinetest,
	034,	inlinetest,
	035,	inlinetest,
	036,	inlinetest,
	037,	inlinetest,
	040,	inlinetest,
	041,	inlinetest,
	042,	inlinetest,
	043,	inlinetest,
	044,	inlinetest,
	045,	inlinetest,
	046,	inlinetest,
	047,	inlinetest,
	050,	inlinetest,
	051,	inlinetest,
	052,	inlinetest,
	053,	inlinetest,
	054,	inlinetest,
	055,	inlinetest,
	056,	inlinetest,
	057,	inlinetest,
	-1,	unclass
};

/* MTER_MBFLT */
static struct fmesg mbfltmsg[] = {
	01,	"control bus parity error",
	02,	"illegal register referenced",
	-1,	unclass
};

/*
 * MTER_LEOT, MTER_RWDING, NTER_NOTAVL, MTER_NONEX, MTER_KEYFAIL,
 * and default: no failure message.
 */
static struct fmesg nullmsg[] = {
	-1,	""
};

/*
 * Interrupt code table.
 */
static struct errmsg {
	int	e_code;
	char	*e_mesg;
	struct	fmesg *e_fmesg;
} errmsg[] = {
	MTER_BOT,	"unexpected BOT",	botmsg,
	MTER_LEOT,	"unexpected LEOT",	nullmsg,
	MTER_RWDING,	"tape rewinding",	nullmsg,
	MTER_NOTRDY,	"drive not ready",	notrdymsg,
	MTER_NOTAVL,	"drive not available",	nullmsg,
	MTER_NONEX,	"unit does not exist",	nullmsg,
	MTER_NOTCAP,	"not capable",		notcapmsg,
	MTER_LONGREC,	"long record",		longrecmsg,
	MTER_UNREAD,	"unreadable record",	code22msg,
	MTER_ERROR,	"error",		code22msg,
	MTER_EOTERR,	"EOT error",		code22msg,
	MTER_BADTAPE,	"tape position lost",	code22msg,
	MTER_TMFLTA,	"TM fault A",		tmfltamsg,
	MTER_TUFLTA,	"TU fault A",		tufltamsg,
	MTER_TMFLTB,	"TM fault B",		tmfltbmsg,
	MTER_MBFLT,	"MB fault",		mbfltmsg,
	MTER_KEYFAIL,	"keypad entry error",	nullmsg,
	-1,		"unclassified error",	nullmsg
};

/*
 * Decode an interrupt-time failure.
 */
mtintfail(erreg)
	int erreg;
{
	register struct errmsg *e;
	register struct fmesg *f;
	register int ecode, fcode;

	ecode = erreg & MTER_INTCODE;
	fcode = (erreg & MTER_FAILCODE) >> MTER_FSHIFT;
	for (e = errmsg; e->e_code >= 0; e++)
		if (e->e_code == ecode)
			break;
	for (f = e->e_fmesg; f->f_code >= 0; f++)
		if (f->f_code == fcode)
			break;
	printf("    interrupt code = 0%o <%s>\n", ecode, e->e_mesg);
	printf("    failure code = 0%o <%s>\n", fcode, f->f_mesg);
}
#endif /* MTLERRM */
#endif /* NMT > 0 */
