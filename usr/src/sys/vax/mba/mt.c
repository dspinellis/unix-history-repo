/*
 * Copyright (c) 1982 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mt.c	6.5 (Berkeley) %G%
 */

#include "mu.h"
#if NMT > 0
/*
 * TM78/TU78 tape driver
 *
 *	Original author - ?
 *	Most error recovery bug fixes - ggs (ulysses!ggs)
 *
 * OPTIONS:
 *	MTLERRM - Long error message text - twd, Brown University
 *	MTRDREV - `read reverse' error recovery - ggs (ulysses!ggs)
 *
 * TODO:
 *	Add odd byte count kludge from VMS driver (?)
 *	Write dump routine
 */

#include "../machine/pte.h"

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "dir.h"
#include "file.h"
#include "user.h"
#include "map.h"
#include "ioctl.h"
#include "mtio.h"
#include "cmap.h"
#include "uio.h"
#include "tty.h"

#include "../vax/cpu.h"
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

/* Bits in minor device */

#define	MUUNIT(dev)	(minor(dev)&03)
#define	H_NOREWIND	04
#define	H_6250BPI	010

#define MTUNIT(dev)	(mutomt[MUUNIT(dev)])

#ifdef MTRDREV
	int mt_do_readrev = 1;
#else
	int mt_do_readrev = 0;
#endif

/* Per unit status information */

struct	mu_softc {
	char	sc_openf;		/* unit is open if != 0 */
	char	sc_flags;		/* state flags */
	daddr_t	sc_blkno;		/* current physical block number */
	daddr_t	sc_nxrec;		/* firewall input block number */
	u_short	sc_erreg;		/* copy of mter or mtner */
	u_short	sc_dsreg;		/* copy of mtds */
	short	sc_resid;		/* residual function count for ioctl */
	short	sc_dens;		/* density code - MT_GCR or zero */
	struct	mba_device *sc_mi;	/* massbus structure for unit */
	int	sc_slave;		/* slave number for unit */
	int	sc_i_mtas;		/* mtas at slave attach time */
	int	sc_i_mtner;		/* mtner at slave attach time */
	int	sc_i_mtds;		/* mtds at slave attach time */
#ifdef MTLERRM
	char	*sc_mesg;		/* text for interrupt type code */
	char	*sc_fmesg;		/* text for tape error code */
#endif
	struct	tty *sc_ttyp;		/* record user's tty for errors */
} mu_softc[NMU];

struct	buf	rmtbuf[NMT];		/* data transfer buffer structures */
struct	buf	cmtbuf[NMT];		/* tape command buffer structures */

struct	mba_device *mtinfo[NMT];	/* unit massbus structure pointers */
short	mutomt[NMU];			/* tape unit to controller number map */
char	mtds_bits[] = MTDS_BITS;	/* mtds bit names for error messages */
short	mttypes[] = { MBDT_TU78, 0 };

int	mtattach(), mtslave(), mtustart(), mtstart(), mtndtint(), mtdtint();
struct	mba_driver mtdriver =
	{ mtattach, mtslave, mtustart, mtstart, mtdtint, mtndtint,
	  mttypes, "mt", "mu", mtinfo };

void mtcreset();

/*ARGSUSED*/
mtattach(mi)
	struct mba_device *mi;
{
#ifdef lint
	mtread(0); mtwrite(0); mtioctl(0, 0, 0, 0);
#endif
}

mtslave(mi, ms, sn)
	struct mba_device *mi;
	struct mba_slave *ms;
	int sn;
{
	register struct mu_softc *sc = &mu_softc[ms->ms_unit];
	register struct mtdevice *mtaddr = (struct mtdevice *)mi->mi_drv;
	int s = spl7(), rtn = 0, i;

	/* Just in case the controller is ill, reset it.  Then issue	*/
	/* a sense operation and wait about a second for it to respond.	*/

	mtcreset(mtaddr);
	mtaddr->mtas = -1;
	mtaddr->mtncs[sn] = MT_SENSE|MT_GO;
	for (i = MTTIMEOUT; i> 0; i--) {
		DELAY(50);
		if (MASKREG(mtaddr->mtas) != 0)
			break;
	}
	sc->sc_i_mtas = mtaddr->mtas;
	sc->sc_i_mtner = mtaddr->mtner;
	sc->sc_i_mtds = mtaddr->mtds;

	/* If no response, whimper.  If wrong response, call it an	*/
	/* unsolicited interrupt and use mtndtint to log and correct.	*/
	/* Otherwise, note whether this slave exists.			*/

	if (i <= 0) {
		printf("mt: controller hung\n");
	} else if ((mtaddr->mtner & MTER_INTCODE) != MTER_DONE) {
		(void) mtndtint(mi);
	} else if (mtaddr->mtds & MTDS_PRES) {
		sc->sc_mi = mi;
		sc->sc_slave = sn;
		mutomt[ms->ms_unit] = mi->mi_unit;
		rtn = 1;
	}

	/* Cancel the interrupt, then wait a little while for it to go away. */

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
	register struct mba_device *mi;
	register struct mu_softc *sc;

	muunit = MUUNIT(dev);
	if (   (muunit >= NMU)
	    || ((mi = mtinfo[MTUNIT(dev)]) == 0)
	    || (mi->mi_alive == 0) )
		return (ENXIO);
	if ((sc = &mu_softc[muunit])->sc_openf)
		return (EBUSY);
	sc->sc_dens = (minor(dev) & H_6250BPI) ? MT_GCR : 0;
	mtcommand(dev, MT_SENSE, 1);
	if ((sc->sc_dsreg & MTDS_ONL) == 0) {
		uprintf("mu%d: not online\n", muunit);
		return (EIO);
	}
	if ((sc->sc_dsreg & MTDS_AVAIL) == 0) {
		uprintf("mu%d: not online (port selector)\n", muunit);
		return (EIO);
	}
	if ((flag & FWRITE) && (sc->sc_dsreg & MTDS_FPT)) {
		uprintf("mu%d: no write ring\n", muunit);
		return (EIO);
	}
	if (   ((sc->sc_dsreg & MTDS_BOT) == 0)
	    && (flag & FWRITE)
	    && (   (   (sc->sc_dens == MT_GCR)
		    && (sc->sc_dsreg & MTDS_PE) )
		|| (   (sc->sc_dens != MT_GCR)
		    && ((sc->sc_dsreg & MTDS_PE) == 0)))) {
		uprintf("mu%d: can't change density in mid-tape\n", muunit);
		return (EIO);
	}
	sc->sc_openf = 1;
	sc->sc_blkno = (daddr_t)0;

	/* Since cooked I/O may do a read-ahead before a write, trash	*/
	/* on a tape can make the first write fail.  Suppress the first	*/
	/* read-ahead unless definitely doing read-write		*/

	sc->sc_nxrec =  ((flag & (FTRUNC | FWRITE)) == (FTRUNC | FWRITE))
		      ? (daddr_t)0
		      : (daddr_t)INF;
	sc->sc_flags = 0;
	sc->sc_ttyp = u.u_ttyp;
	return (0);
}

mtclose(dev, flag)
	register dev_t dev;
	register int flag;
{
	register struct mu_softc *sc = &mu_softc[MUUNIT(dev)];

	if (   ((flag & (FREAD | FWRITE)) == FWRITE)
	    || (   (flag & FWRITE)
		&& (sc->sc_flags & H_WRITTEN) ))
		mtcommand(dev, MT_CLS|sc->sc_dens, 1);
	if ((minor(dev) & H_NOREWIND) == 0)
		mtcommand(dev, MT_REW, 0);
	sc->sc_openf = 0;
}

mtcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;
	register int s;

	bp = &cmtbuf[MTUNIT(dev)];
	s = spl5();
	while (bp->b_flags & B_BUSY) {
		if((bp->b_repcnt == 0) && (bp->b_flags & B_DONE))
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
	iowait(bp);
	if (bp->b_flags & B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

mtstrategy(bp)
	register struct buf *bp;
{
	register struct mba_device *mi = mtinfo[MTUNIT(bp->b_dev)];
	register struct buf *dp;
	register int s;

	/* If this is a data transfer operation, set the resid to a	*/
	/* default value (EOF) to simplify getting it right during	*/
	/* error recovery or bail out.					*/

	if (bp != &cmtbuf[MTUNIT(bp->b_dev)])
		bp->b_resid = bp->b_bcount;

	/* Link this request onto the end of the queue for this		*/
	/* controller, then start I/O if not already active.		*/

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

	if (sc->sc_openf < 0) {
		bp->b_flags |= B_ERROR;
		return (MBU_NEXT);
	}
	if (bp != &cmtbuf[MTUNIT(bp->b_dev)]) {

		/* Signal "no space" if out of tape unless suppressed	*/
		/* by MTIOCIEOT.					*/

		if (   ((sc->sc_flags & (H_EOT | H_IEOT)) == H_EOT)
		    && ((bp->b_flags & B_READ) == 0) ) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENOSPC;
			return (MBU_NEXT);
		}

		/* special case tests for cooked mode */

		if (bp != &rmtbuf[MTUNIT(bp->b_dev)]) {

			/* seek beyond end of file */

			if (bdbtofsb(bp->b_blkno) > sc->sc_nxrec) {
				bp->b_flags |= B_ERROR;
				bp->b_error = ENXIO;
				return (MBU_NEXT);
			}

			/* This should be end of file, but the buffer	   */
			/* system wants a one-block look-ahead.  Humor it. */

			if (   (bdbtofsb(bp->b_blkno) == sc->sc_nxrec)
			    && (bp->b_flags & B_READ) ) {
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

	/* If raw I/O, or if the tape is positioned correctly for	*/
	/* cooked I/O, set the byte count, unit number and repeat count	*/
	/* then tell the MASSBUS to proceed.  Note that a negative	*/
	/* bcount tells mbstart to map the buffer for "read backwards".	*/

	if (   (bp == &rmtbuf[MTUNIT(bp->b_dev)])
	    || ((blkno = sc->sc_blkno) == bdbtofsb(bp->b_blkno)) ) {
		if (mi->mi_tab.b_errcnt == 2) {
			mtaddr->mtbc = -(bp->b_bcount);
			mtaddr->mtca = MUUNIT(bp->b_dev);
		} else {
			mtaddr->mtbc = bp->b_bcount;
			mtaddr->mtca = (1<<2)|MUUNIT(bp->b_dev);
		}
		return (MBU_DODATA);
	}

	/* Issue skip operations to position the next block for cooked I/O. */

	if (blkno < bdbtofsb(bp->b_blkno))
		mtaddr->mtncs[MUUNIT(bp->b_dev)] =
		  (min((unsigned)(bdbtofsb(bp->b_blkno) - blkno), 0377) << 8) |
			MT_SFORW|MT_GO;
	else
		mtaddr->mtncs[MUUNIT(bp->b_dev)] =
		  (min((unsigned)(blkno - bdbtofsb(bp->b_blkno)), 0377) << 8) |
			MT_SREV|MT_GO;
	return (MBU_STARTED);
}

mtstart(mi)
	register struct mba_device *mi;
{
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct mu_softc *sc = &mu_softc[MUUNIT(bp->b_dev)];

	if (bp->b_flags & B_READ)
		if (mi->mi_tab.b_errcnt == 2)
			return(MT_READREV|MT_GO);
		else
			return(MT_READ|MT_GO);
	else
		return(MT_WRITE|sc->sc_dens|MT_GO);
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
			if (   (bp->b_resid > 0)
			    && (bp != &rmtbuf[MTUNIT(bp->b_dev)]) )
				bp->b_flags |= B_ERROR;
		} else {
			bp->b_resid = 0;
		}
		break;

	case MTER_SHRTREC:
		sc->sc_blkno++;
		bp->b_bcount = bp->b_resid;
		bp->b_resid -= MASKREG(mtaddr->mtbc);
		if (bp != &rmtbuf[MTUNIT(bp->b_dev)])
			bp->b_flags |= B_ERROR;
		break;

	case MTER_RETRY:

		/* Simple re-try.  Since resid is always a copy of the	*/
		/* original byte count, use it to restore the count.	*/

		mi->mi_tab.b_errcnt = 1;
		bp->b_bcount = bp->b_resid;
		return(MBD_RETRY);

	case MTER_RDOPP:

		/* The controller just decided to read it backwards.	*/
		/* If the controller returns a byte count of zero,	*/
		/* change it to 1, since zero encodes 65536, which	*/
		/* isn't quite what we had in mind.  The byte count	*/
		/* may be larger than the size of the input buffer, so	*/
		/* limit the count to the buffer size.  After		*/
		/* making the byte count reasonable, set bcount to the	*/
		/* negative of the controller's version of the byte	*/
		/* count so that the start address for the transfer is	*/
		/* set up correctly.					*/

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

		/* End of file.  Since the default byte count has	*/
		/* already been set, just count the block and proceed.	*/

		sc->sc_blkno++;
	err:
		if (bp != &rmtbuf[MTUNIT(bp->b_dev)])
			sc->sc_nxrec = bdbtofsb(bp->b_blkno);
		break;

	case MTER_OFFLINE:
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			tprintf(sc->sc_ttyp, "mu%d: offline\n", MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		break;

	case MTER_NOTAVL:
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			tprintf(sc->sc_ttyp, "mu%d: offline (port selector)\n",
			    MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		break;

	case MTER_FPT:
		tprintf(sc->sc_ttyp, "mu%d: no write ring\n", MUUNIT(bp->b_dev));
		bp->b_flags |= B_ERROR;
		break;

	case MTER_UNREAD:
		sc->sc_blkno++;
		bp->b_bcount = bp->b_resid;
		bp->b_resid -= MIN(MASKREG(mtaddr->mtbc), bp->b_bcount);

		/* Code 010 means a garbage record, nothing serious. */

		if (((er & MTER_FAILCODE) >> 10) == 010) {
			tprintf(sc->sc_ttyp, "mu%d: rn=%d bn=%d unreadable record\n",
			    MUUNIT(bp->b_dev), sc->sc_blkno, bp->b_blkno);
			bp->b_flags |= B_ERROR;
			break;
		}

		/* Anything else might be a hardware problem,	*/
		/* fall into the error report.			*/

	default:

		/* The bits in sc->sc_dsreg are from the last sense	*/
		/* command.  To get the most recent copy, you have to	*/
		/* do a sense at interrupt level, which requires nested	*/
		/* error processing.  This is a bit messy, so leave	*/
		/* well enough alone.					*/

		tprintf(sc->sc_ttyp, "mu%d: hard error (data transfer) rn=%d bn=%d mbsr=%b er=%o (octal) ds=%b\n",
		    MUUNIT(bp->b_dev), sc->sc_blkno, bp->b_blkno,
		    mbsr, mbsr_bits, er,
		    MASKREG(sc->sc_dsreg), mtds_bits);
#ifdef MTLERRM
		mtintfail(sc);
		printf("     interrupt code = %o (octal) <%s>\n     failure code = %o (octal) <%s>\n",
		    er & MTER_INTCODE, sc->sc_mesg,
		    (er & MTER_FAILCODE) >> 10, sc->sc_fmesg);
#endif
		bp->b_flags |= B_ERROR;

		/* The TM78 manual says to reset the controller after	*/
		/* TM fault B or MASSBUS fault.				*/

		if (   ((er & MTER_INTCODE) == MTER_TMFLTB)
		    || ((er & MTER_INTCODE) == MTER_MBFLT) ) {
			mtcreset(mtaddr);
		}
	}

	/* Just in case some strange error slipped through, (drive off	*/
	/* line during read-reverse error recovery comes to mind) make	*/
	/* sure the byte count is reasonable.				*/

	if (bp->b_bcount < 0)
		bp->b_bcount = bp->b_resid;
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

	if (bp == 0 || unit != MUUNIT(bp->b_dev)) {	/* consistency check */
		if ((er & MTER_INTCODE) != MTER_ONLINE) {
			printf("mt: unit %d unexpected interrupt (non data transfer) er=%o (octal) ds=%b\n",
			    unit, er, MASKREG(sc->sc_dsreg), mtds_bits);
#ifdef MTLERRM
			mtintfail(sc);
			printf("    interrupt code = %o (octal) <%s>\n    failure code = %o (octal) <%s>\n",
			    er & MTER_INTCODE, sc->sc_mesg,
			    (er & MTER_FAILCODE) >> 10, sc->sc_fmesg);
#endif
			if (   ((er & MTER_INTCODE) == MTER_TMFLTB)
			    || ((er & MTER_INTCODE) == MTER_MBFLT) ) {

				/* Reset the controller, then set error	*/
				/* status if there was anything active	*/
				/* when the fault occurred.  This may	*/
				/* shoot an innocent bystander, but	*/
				/* it's better than letting an error	*/
				/* slip through.			*/

				mtcreset(mtaddr);
				if (bp != 0) {
					bp->b_flags |= B_ERROR;
					return (MBN_DONE);
				}
			}
		}
		return (MBN_SKIP);
	}
	if (bp == 0)
		return (MBN_SKIP);

	fc = (mtaddr->mtncs[unit] >> 8) & 0xff;
	sc->sc_resid = fc;

	/* Clear the "written" flag after any operation that changes	*/
	/* the position of the tape.					*/

	if (   (bp != &cmtbuf[MTUNIT(bp->b_dev)])
	    || (bp->b_command != MT_SENSE) )
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

		/* It's not a command buffer, must be a cooked I/O	*/
		/* skip operation (perhaps a shaky assumption, but it	*/
		/* wasn't my idea).					*/

		if ((fc = bdbtofsb(bp->b_blkno) - sc->sc_blkno) < 0)
			sc->sc_blkno -= MIN(0377, -fc);
		else
			sc->sc_blkno += MIN(0377, fc);
		return (MBN_RETRY);

	case MTER_ONLINE:		/* ddj -- shouldn't happen but did */
	case MTER_RWDING:
		return (MBN_SKIP);	/* ignore "rewind started" interrupt */

	case MTER_NOTCAP:
		tprintf(sc->sc_ttyp, "mu%d: blank tape\n", MUUNIT(bp->b_dev));
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_TM:
	case MTER_LEOT:

		/* For an ioctl skip operation, count a tape mark as	*/
		/* a record.  If there's anything left to do, update	*/
		/* the repeat count and re-start the command.		*/

		if (bp == &cmtbuf[MTUNIT(bp->b_dev)]) {
			if ((sc->sc_resid = bp->b_repcnt = fc - 1) == 0)
				return (MBN_DONE);
			else
				return (MBN_RETRY);

		/* Cooked I/O again.  Just update the books and wait	*/
		/* for someone else to return end of file or complain	*/
		/* about a bad seek.					*/

		} else if (sc->sc_blkno > bdbtofsb(bp->b_blkno)) {
			sc->sc_nxrec = bdbtofsb(bp->b_blkno) + fc - 1;
			sc->sc_blkno = sc->sc_nxrec;
		} else {
			sc->sc_nxrec = bdbtofsb(bp->b_blkno) - fc;
			sc->sc_blkno = sc->sc_nxrec + 1;
		}
		return (MBN_RETRY);

	case MTER_FPT:
		tprintf(sc->sc_ttyp, "mu%d: no write ring\n", MUUNIT(bp->b_dev));
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_OFFLINE:

		/* If `off line' was intentional, don't complain. */

		if (   (bp == &cmtbuf[MTUNIT(bp->b_dev)])
		    && (bp->b_command == MT_UNLOAD) )
			return(MBN_DONE);
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			tprintf(sc->sc_ttyp, "mu%d: offline\n", MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_NOTAVL:
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			tprintf(sc->sc_ttyp, "mu%d: offline (port selector)\n", MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_BOT:
		if (bp == &cmtbuf[MTUNIT(bp->b_dev)])
			goto done;

		/* fall through */

	default:
		tprintf(sc->sc_ttyp, "mu%d: hard error (non data transfer) rn=%d bn=%d er=%o (octal) ds=%b\n",
		    MUUNIT(bp->b_dev), sc->sc_blkno, bp->b_blkno,
		    er, MASKREG(sc->sc_dsreg), mtds_bits);
#ifdef MTLERRM
		mtintfail(sc);
		printf("     interrupt code = %o (octal) <%s>\n     failure code = %o (octal) <%s>\n",
		    (er & MTER_INTCODE), sc->sc_mesg,
		    (er & MTER_FAILCODE) >> 10, sc->sc_fmesg);
#endif
		if (   ((er & MTER_INTCODE) == MTER_TMFLTB)
		    || ((er & MTER_INTCODE) == MTER_MBFLT) ) {
			mtcreset(mtaddr);	/* reset the controller */
		}
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);
	}
	/* NOTREACHED */
}

void mtcreset(mtaddr)
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

mtread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int errno;

	errno = mtphys(dev, uio);
	if (errno)
		return (errno);
	return (physio(mtstrategy, &rmtbuf[MTUNIT(dev)], dev, B_READ, minphys, uio));
}


mtwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int errno;

	errno = mtphys(dev, uio);
	if (errno)
		return (errno);
	return (physio(mtstrategy, &rmtbuf[MTUNIT(dev)], dev, B_WRITE, minphys, uio));
}

mtphys(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int mtunit;
	struct mba_device *mi;
	register int bsize = uio->uio_iov->iov_len;

	mtunit = MTUNIT(dev);
	if (   (mtunit >= NMT)
	    || ((mi = mtinfo[mtunit]) == 0)
	    || (mi->mi_alive == 0) )
		return (ENXIO);
	if (   (bsize > 0xffff)	/* controller limit */
	    || (bsize <= 0) )	/* ambiguous */
		return (EINVAL);
	return (0);
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
	int callcount, fcount;
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
		if ((callcount <= 0) || (fcount <= 0))
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
				if (   (mtop->mt_op == MTFSR)
				    || (mtop->mt_op == MTBSR) )
					return (EIO);
				else
					break;
			}
			if (bp->b_flags & B_ERROR)
				break;
		}
		return (geterror(bp));

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
mtintfail(sc)
	register struct mu_softc *sc;
{
	switch (sc->sc_erreg & MTER_INTCODE) {

	/* unexpected BOT detected */

	case MTER_BOT:
		sc->sc_mesg = "unexpected BOT";
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 01:
			sc->sc_fmesg = "tape was at BOT";
			break;
		case 02:
			sc->sc_fmesg = "BOT seen after tape started";
			break;
		case 03:
			sc->sc_fmesg = "ARA ID detected";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* unexpected LEOT detected */

	case MTER_LEOT:
		sc->sc_mesg = "unexpected LEOT";
		sc->sc_fmesg = "";
		break;

	/* rewinding */

	case MTER_RWDING:
		sc->sc_mesg = "tape rewinding";
		sc->sc_fmesg = "";
		break;

	/* not ready */

	case MTER_NOTRDY:
		sc->sc_mesg = "drive not ready";
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 01:
			sc->sc_fmesg = "TU on-line but not ready";
			break;
		case 02:
			sc->sc_fmesg = "fatal error has occurred";
			break;
		case 03:
			sc->sc_fmesg = "access allowed but not really";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* not available */

	case MTER_NOTAVL:
		sc->sc_mesg = "drive not available";
		sc->sc_fmesg = "";
		break;

	/* unit does not exist */

	case MTER_NONEX:
		sc->sc_mesg = "unit does not exist";
		sc->sc_fmesg = "";
		break;

	/* not capable */

	case MTER_NOTCAP:
		sc->sc_mesg = "not capable";
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 01:
			sc->sc_fmesg = "no record found within 25 feet";
			break;
		case 02:
			sc->sc_fmesg = "ID burst neither PE nor GCR";
			break;
		case 03:
			sc->sc_fmesg = "ARA ID not found";
			break;
		case 04:
			sc->sc_fmesg = "no gap found after ID burst";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* long tape record */

	case MTER_LONGREC:
		sc->sc_mesg = "long record";
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 00:
			sc->sc_fmesg = "extended sense data not found";
			break;
		case 01:
			sc->sc_fmesg = "extended sense data updated";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* unreadable */

	case MTER_UNREAD:
		sc->sc_mesg = "unreadable record";
		goto code22;

	/* error */

	case MTER_ERROR:
		sc->sc_mesg = "error";
		goto code22;

	/* EOT error */

	case MTER_EOTERR:
		sc->sc_mesg = "EOT error";
		goto code22;

	/* tape position lost */

	case MTER_BADTAPE:
		sc->sc_mesg = "bad tape";
	code22:
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 01:
			sc->sc_fmesg = "GCR write error";
			break;
		case 02:
			sc->sc_fmesg = "GCR read error";
			break;
		case 03:
			sc->sc_fmesg = "PE read error";
			break;
		case 04:
			sc->sc_fmesg = "PE write error";
			break;
		case 05:
			sc->sc_fmesg = "at least 1 bit set in ECCSTA";
			break;
		case 06:
			sc->sc_fmesg = "PE write error";
			break;
		case 07:
			sc->sc_fmesg = "GCR write error";
			break;
		case 010:
			sc->sc_fmesg = "RSTAT contains bad code";
			break;
		case 011:
			sc->sc_fmesg = "PE write error";
			break;
		case 012:
			sc->sc_fmesg = "MASSBUS parity error";
			break;
		case 013:
			sc->sc_fmesg = "invalid data transferred";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* TM fault A */

	case MTER_TMFLTA:
		sc->sc_mesg = "TM fault A";
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 01:
			sc->sc_fmesg = "illegal command code";
			break;
		case 02:
			sc->sc_fmesg = "DT command issued when NDT command active";
			break;
		case 03:
			sc->sc_fmesg = "WMC error";
			break;
		case 04:
			sc->sc_fmesg = "RUN not received from MASSBUS controller";
			break;
		case 05:
			sc->sc_fmesg = "mismatch in command read - function routine";
			break;
		case 06:
			sc->sc_fmesg = "ECC ROM parity error";
			break;
		case 07:
			sc->sc_fmesg = "XMC ROM parity error";
			break;
		case 010:
			sc->sc_fmesg = "mismatch in command read - ID burst command";
			break;
		case 011:
			sc->sc_fmesg = "mismatch in command read - verify ARA burst command";
			break;
		case 012:
			sc->sc_fmesg = "mismatch in command read - verify ARA ID command";
			break;
		case 013:
			sc->sc_fmesg = "mismatch in command read - verify gap command";
			break;
		case 014:
			sc->sc_fmesg = "mismatch in command read - read id burst command";
			break;
		case 015:
			sc->sc_fmesg = "mismatch in command read - verify ARA ID command";
			break;
		case 016:
			sc->sc_fmesg = "mismatch in command read - verify gap command";
			break;
		case 017:
			sc->sc_fmesg = "mismatch in command read - find gap command";
			break;
		case 020:
			sc->sc_fmesg = "WMC LEFT failed to set";
			break;
		case 021:
			sc->sc_fmesg = "XL PE set in INTSTA register";
			break;
		case 022:
			sc->sc_fmesg = "XMC DONE did not set";
			break;
		case 023:
			sc->sc_fmesg = "WMC ROM PE or RD PE set in WMCERR register";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* TU fault A */

	case MTER_TUFLTA:
		sc->sc_mesg = "TU fault A";
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 01:
			sc->sc_fmesg = "TU status parity error";
			break;
		case 02:
			sc->sc_fmesg = "TU command parity error";
			break;
		case 03:
			sc->sc_fmesg = "rewinding tape went offline";
			break;
		case 04:
			sc->sc_fmesg = "tape went not ready during DSE";
			break;
		case 05:
			sc->sc_fmesg = "TU CMD status changed during DSE";
			break;
		case 06:
			sc->sc_fmesg = "TU never came up to speed";
			break;
		case 07:
			sc->sc_fmesg = "TU velocity changed";
			break;
		case 010:
			sc->sc_fmesg = "TU CMD did not load correctly to start tape motion";
			break;
		case 011:
			sc->sc_fmesg = "TU CMD did not load correctly to set drive density";
			break;
		case 012:
			sc->sc_fmesg = "TU CMD did not load correctly to start tape motion to write BOT ID";
			break;
		case 013:
			sc->sc_fmesg = "TU CMD did not load correctly to backup tape to BOT after failing to write BOT ID";
			break;
		case 014:
			sc->sc_fmesg = "failed to write density ID burst";
			break;
		case 015:
			sc->sc_fmesg = "failed to write ARA burst";
			break;
		case 016:
			sc->sc_fmesg = "failed to write ARA ID";
			break;
		case 017:
			sc->sc_fmesg = "ARA error bit set in MTA status B register";
			break;
		case 021:
			sc->sc_fmesg = "could not find a gap after ID code was written correctly";
			break;
		case 022:
			sc->sc_fmesg = "TU CMD did not load correctly to start tape motion to read ID burst";
			break;
		case 023:
			sc->sc_fmesg = "timeout looking for BOT after detecting ARA ID burst";
			break;
		case 024:
			sc->sc_fmesg = "failed to write tape mark";
			break;
		case 025:
			sc->sc_fmesg = "tape never came up to speed while trying to reposition for retry of writing tape mark";
			break;
		case 026:
			sc->sc_fmesg = "TU CMD did not load correctly to start tape motion in erase gap routine";
			break;
		case 027:
			sc->sc_fmesg = "could not detect a gap in in erase gap routine";
			break;
		case 030:
			sc->sc_fmesg = "could not detect a gap after writing record";
			break;
		case 031:
			sc->sc_fmesg = "read path terminated before entire record was written";
			break;
		case 032:
			sc->sc_fmesg = "could not find a gap after writing record and read path terminated early";
			break;
		case 033:
			sc->sc_fmesg = "TU CMD did not load correctly to backup for retry of write tape mark";
			break;
		case 034:
			sc->sc_fmesg = "TU velocity changed after up to speed while trying to reposition for retry of writing tape mark";
			break;
		case 035:
			sc->sc_fmesg = "TU CMD did not load correctly to backup to retry a load of BOT ID";
			break;
		case 036:
			sc->sc_fmesg = "timeout looking for BOT after failing to write BOT ID";
			break;
		case 037:
			sc->sc_fmesg = "TU velocity changed while writing PE gap before starting to write record";
			break;
		case 040:
			sc->sc_fmesg = "TU CMD did not load correctly to set PE tape density at start of write BOT ID burst";
			break;
		case 041:
			sc->sc_fmesg = "TU CMD did not load correctly to set GCR tape density after writing Density ID";
			break;
		case 042:
			sc->sc_fmesg = "TU CMD did not load correctly to set PE tape density at start of read from BOT";
			break;
		case 043:
			sc->sc_fmesg = "TU CMD did not load correctly to set GCR tape density after reading a GCR Density ID burst";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* TM fault B */

	case MTER_TMFLTB:
		sc->sc_mesg = "TM fault B";
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 00:
			sc->sc_fmesg = "RST0 interrupt occurred with TM RDY set";
			break;
		case 01:
			sc->sc_fmesg = "power failed to interrupt";
			break;
		case 02:
			sc->sc_fmesg = "unknown interrupt on channel 5.5";
			break;
		case 03:
			sc->sc_fmesg = "unknown interrupt on channel 6.5";
			break;
		case 04:
			sc->sc_fmesg = "unknown interrupt on channel 7";
			break;
		case 05:
			sc->sc_fmesg = "unknown interrupt on channel 7.5";
			break;
		case 06:
			sc->sc_fmesg = "CAS contention retry count expired";
			break;
		case 07:
			sc->sc_fmesg = "CAS contention error not retryable";
			break;
		case 010:
			sc->sc_fmesg = "queue error, could not find queue entry";
			break;
		case 011:
			sc->sc_fmesg = "queue entry already full";
			break;
		case 012:
			sc->sc_fmesg = "8085 ROM parity error";
			break;
		case 013:
		case 014:
		case 015:
		case 016:
		case 017:
		case 020:
		case 021:
		case 022:
		case 023:
		case 024:
		case 025:
		case 026:
		case 027:
		case 030:
		case 031:
		case 032:
		case 033:
		case 034:
		case 035:
		case 036:
		case 037:
		case 040:
		case 041:
		case 042:
		case 043:
		case 044:
		case 045:
		case 046:
		case 047:
		case 050:
		case 051:
		case 052:
		case 053:
		case 054:
		case 055:
		case 056:
		case 057:
			sc->sc_fmesg = "inline test failed";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* MASSBUS fault */

	case MTER_MBFLT:
		sc->sc_mesg = "MB fault";
		switch ((sc->sc_erreg & MTER_FAILCODE) >> 10) {
		case 01:
			sc->sc_fmesg = "control bus parity error";
			break;
		case 02:
			sc->sc_fmesg = "illegal register referenced";
			break;
		default:
			sc->sc_fmesg = "unclassified failure code";
		}
		break;

	/* keypad entry error */

	case MTER_KEYFAIL:
		sc->sc_mesg = "keypad entry error";
		sc->sc_fmesg = "";
		break;
	default:
		sc->sc_mesg = "unclassified error";
		sc->sc_fmesg = "";
		break;
	}
}
#endif MTLERRM
#endif
