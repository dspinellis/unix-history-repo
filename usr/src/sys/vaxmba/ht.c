/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ht.c	7.11 (Berkeley) 4/12/90
 */

#include "tu.h"
#if NHT > 0
/*
 * TM03/TU?? tape driver
 *
 * TODO:
 *	cleanup messages on errors
 *	test ioctl's
 *	see how many rewind interrups we get if we kick when not at BOT
 *	fixup rle error on block tape code
 */
#include "param.h"
#include "systm.h"
#include "buf.h"
#include "conf.h"
#include "file.h"
#include "user.h"
#include "proc.h"
#include "map.h"
#include "ioctl.h"
#include "mtio.h"
#include "cmap.h"
#include "tty.h"
#include "syslog.h"

#include "machine/pte.h"
#include "../vax/cpu.h"
#include "mbareg.h"
#include "mbavar.h"
#include "htreg.h"

struct	buf	chtbuf[NHT];

short	httypes[] =
	{ MBDT_TM03, MBDT_TE16, MBDT_TU45, MBDT_TU77, 0 };
struct	mba_device *htinfo[NHT];
struct	mba_slave *tuinfo[NTU];
int	htattach(), htslave(), htustart(), htndtint(), htdtint();
struct	mba_driver htdriver =
    { htattach, htslave, htustart, 0, htdtint, htndtint,
      httypes, "ht", "tu", htinfo };

#define MASKREG(r)	((r) & 0xffff)

/* bits in minor device */
#define	TUUNIT(dev)	(minor(dev)&03)
#define	H_NOREWIND	04
#define	H_DENS(dev)	((minor(dev) >> 3) & 03)

#define HTUNIT(dev)	(tuinfo[TUUNIT(dev)]->ms_ctlr)

#define	INF	(daddr_t)1000000L	/* a block number that wont exist */

struct	tu_softc {
	char	sc_openf;
	char	sc_flags;
	daddr_t	sc_blkno;
	daddr_t	sc_nxrec;
	u_short	sc_erreg;
	u_short	sc_dsreg;
	short	sc_resid;
	short	sc_dens;
	caddr_t	sc_ctty;		/* record user's tty for errors */
	int	sc_blks;	/* number of I/O operations since open */
	int	sc_softerrs;	/* number of soft I/O errors since open */
} tu_softc[NTU];

/*
 * Bits for sc_flags.
 */
#define	H_WRITTEN 1	/* last operation was a write */
#define H_ERASED  2	/* last write retry was an erase gap */
#define H_REWIND  4	/* last unit start was a rewind */

char	hter_bits[] = HTER_BITS;
char	htds_bits[] = HTDS_BITS;

/*ARGSUSED*/
htattach(mi)
	struct mba_device *mi;
{

}

htslave(mi, ms, sn)
	struct mba_device *mi;
	struct mba_slave *ms;
	int sn;
{
	register struct htdevice *htaddr = (struct htdevice *)mi->mi_drv;

	htaddr->httc = sn;
	if (htaddr->htdt & HTDT_SPR) {
		tuinfo[ms->ms_unit] = ms;
		return (1);
	} else
		return (0);
}

int	htdens[4] = { HTTC_800BPI, HTTC_1600BPI, HTTC_6250BPI, HTTC_800BPI };

htopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int tuunit;
	register struct tu_softc *sc;
	register struct mba_slave *ms;
	int olddens, dens;

	tuunit = TUUNIT(dev);
	if (tuunit >= NTU || (ms = tuinfo[tuunit]) == NULL ||
	    ms->ms_alive == 0 || htinfo[ms->ms_ctlr]->mi_alive == 0)
		return (ENXIO);
	if ((sc = &tu_softc[tuunit])->sc_openf)
		return (EBUSY);
	sc->sc_openf = 1;
	olddens = sc->sc_dens;
	dens = sc->sc_dens = htdens[H_DENS(dev)] | HTTC_PDP11 | ms->ms_slave;
	htcommand(dev, HT_SENSE, 1);
	sc->sc_dens = olddens;
	if ((sc->sc_dsreg & HTDS_MOL) == 0) {
		sc->sc_openf = 0;
		uprintf("tu%d: not online\n", tuunit);
		return (EIO);
	}
	if ((flag&FWRITE) && (sc->sc_dsreg&HTDS_WRL)) {
		sc->sc_openf = 0;
		uprintf("tu%d: no write ring\n", tuunit);
		return (EIO);
	}
	if ((sc->sc_dsreg & HTDS_BOT) == 0 && (flag&FWRITE) &&
	    dens != sc->sc_dens) {
		sc->sc_openf = 0;
		uprintf("tu%d: can't change density in mid-tape\n", tuunit);
		return (EIO);
	}
	sc->sc_blkno = (daddr_t)0;
	sc->sc_nxrec = INF;
	sc->sc_flags = 0;
	sc->sc_dens = dens;
	sc->sc_blks = 0;
	sc->sc_softerrs = 0;
	sc->sc_ctty = (caddr_t)(u.u_procp->p_flag&SCTTY ? 
			u.u_procp->p_session->s_ttyp : 0);
	return (0);
}

htclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register struct tu_softc *sc = &tu_softc[TUUNIT(dev)];

	if (flag == FWRITE || ((flag&FWRITE) && (sc->sc_flags&H_WRITTEN))) {
		htcommand(dev, HT_WEOF, 1);
		htcommand(dev, HT_WEOF, 1);
		htcommand(dev, HT_SREV, 1);
	}
	if ((minor(dev)&H_NOREWIND) == 0)
		htcommand(dev, HT_REW, 0);
	if (sc->sc_blks > 100 && sc->sc_softerrs > sc->sc_blks / 100)
		log(LOG_INFO, "tu%d: %d soft errors in %d blocks\n",
		    TUUNIT(dev), sc->sc_softerrs, sc->sc_blks);
	sc->sc_openf = 0;
	return (0);
}

htcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;
	register int s;

	bp = &chtbuf[HTUNIT(dev)];
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
	htstrategy(bp);
	if (count == 0)
		return;
	iowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

htstrategy(bp)
	register struct buf *bp;
{
	register struct mba_device *mi = htinfo[HTUNIT(bp->b_dev)];
	register struct buf *dp;
	register int s;

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

htustart(mi)
	register struct mba_device *mi;
{
	register struct htdevice *htaddr =
	    (struct htdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct tu_softc *sc = &tu_softc[TUUNIT(bp->b_dev)];
	daddr_t blkno;

	htaddr->httc = sc->sc_dens;
#ifdef	notdef
	/* unneeded, may hang controller */
	if (bp == &chtbuf[HTUNIT(bp->b_dev)] && bp->b_command == HT_SENSE) {
		htaddr->htcs1 = HT_SENSE|HT_GO;
		mbclrattn(mi);
	}
#endif
	sc->sc_dsreg = htaddr->htds;
	sc->sc_erreg = htaddr->hter;
	sc->sc_resid = htaddr->htfc;
	sc->sc_flags &= ~(H_WRITTEN|H_REWIND);
	if ((htaddr->htdt & HTDT_SPR) == 0 || (htaddr->htds & HTDS_MOL) == 0)
		if (sc->sc_openf > 0)
			sc->sc_openf = -1;
	if (sc->sc_openf < 0) {
		bp->b_flags |= B_ERROR;
		return (MBU_NEXT);
	}
	if (bp != &chtbuf[HTUNIT(bp->b_dev)]) {
		/* transfer: check positioning */
		if (bp->b_flags & B_RAW) {
			/* raw transfer: record position for retry */
			if (mi->mi_tab.b_errcnt == 0) {
				sc->sc_blkno = bdbtofsb(bp->b_blkno);
				sc->sc_nxrec = sc->sc_blkno + 1;
			}
		} else {
			if (bdbtofsb(bp->b_blkno) > sc->sc_nxrec) {
				bp->b_flags |= B_ERROR;
				bp->b_error = ENXIO;
				return (MBU_NEXT);
			}
			if (bdbtofsb(bp->b_blkno) == sc->sc_nxrec &&
			    bp->b_flags&B_READ) {
				bp->b_resid = bp->b_bcount;
				clrbuf(bp);
				return (MBU_NEXT);
			}
			if ((bp->b_flags&B_READ)==0)
				sc->sc_nxrec = bdbtofsb(bp->b_blkno) + 1;
		}
	} else {
		if (bp->b_command == HT_SENSE)
			return (MBU_NEXT);
		if (bp->b_command == HT_REW)
			sc->sc_flags |= H_REWIND;
		else
			htaddr->htfc = -bp->b_bcount;
		htaddr->htcs1 = bp->b_command|HT_GO;
		return (MBU_STARTED);
	}
	if ((blkno = sc->sc_blkno) == bdbtofsb(bp->b_blkno)) {
		htaddr->htfc = -bp->b_bcount;
		if ((bp->b_flags&B_READ) == 0) {
			if (mi->mi_tab.b_errcnt) {
				if ((sc->sc_flags & H_ERASED) == 0) {
					sc->sc_flags |= H_ERASED;
					htaddr->htcs1 = HT_ERASE | HT_GO;
					return (MBU_STARTED);
				}
				sc->sc_flags &= ~H_ERASED;
			}
			if (htaddr->htds & HTDS_EOT) {
				bp->b_resid = bp->b_bcount;
				bp->b_flags |= B_ERROR;
				return (MBU_NEXT);
			}
		}
		return (MBU_DODATA);
	}
	if (blkno < bdbtofsb(bp->b_blkno)) {
		htaddr->htfc = blkno - bdbtofsb(bp->b_blkno);
		htaddr->htcs1 = HT_SFORW|HT_GO;
	} else {
		htaddr->htfc = bdbtofsb(bp->b_blkno) - blkno;
		htaddr->htcs1 = HT_SREV|HT_GO;
	}
	return (MBU_STARTED);
}

htdtint(mi, mbsr)
	register struct mba_device *mi;
	int mbsr;
{
	register struct htdevice *htaddr = (struct htdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct tu_softc *sc;
	int ds, er, mbs;

	sc = &tu_softc[TUUNIT(bp->b_dev)];
	ds = sc->sc_dsreg = MASKREG(htaddr->htds);
	er = sc->sc_erreg = MASKREG(htaddr->hter);
	sc->sc_resid = MASKREG(htaddr->htfc);
	mbs = mbsr;
	sc->sc_blkno++;
	if((bp->b_flags & B_READ) == 0)
		sc->sc_flags |= H_WRITTEN;
	if ((ds&(HTDS_ERR|HTDS_MOL)) != HTDS_MOL || mbs & MBSR_EBITS) {
		htaddr->htcs1 = HT_DCLR|HT_GO;
		mbclrattn(mi);
		if (bp->b_flags & B_RAW) {
			er &= ~HTER_FCE;
			mbs &= ~(MBSR_DTABT|MBSR_MBEXC);
		}
		if (bp->b_flags & B_READ && ds & HTDS_PES)
			er &= ~(HTER_CSITM|HTER_CORCRC);
		if (er&HTER_HARD || mbs&MBSR_EBITS || (ds&HTDS_MOL) == 0 ||
		    er && ++mi->mi_tab.b_errcnt >= 7) {
			if ((ds & HTDS_MOL) == 0 && sc->sc_openf > 0)
				sc->sc_openf = -1;
			if ((er&HTER_HARD) == HTER_FCE &&
			    (mbs&MBSR_EBITS) == (MBSR_DTABT|MBSR_MBEXC) &&
			    (ds&HTDS_MOL))
				goto noprint;
			tprintf(sc->sc_ctty, "tu%d: hard error bn%d mbsr=%b er=%b ds=%b\n",
			    TUUNIT(bp->b_dev), bp->b_blkno,
			    mbsr, mbsr_bits,
			    sc->sc_erreg, hter_bits,
			    sc->sc_dsreg, htds_bits);
noprint:
			bp->b_flags |= B_ERROR;
			return (MBD_DONE);
		}
		if (er)
			return (MBD_RETRY);
	}
	bp->b_resid = 0;
	sc->sc_blks++;
	if (mi->mi_tab.b_errcnt)
		sc->sc_softerrs++;
	if (bp->b_flags & B_READ)
		if (ds&HTDS_TM) {		/* must be a read, right? */
			bp->b_resid = bp->b_bcount;
			sc->sc_nxrec = bdbtofsb(bp->b_blkno);
		} else if(bp->b_bcount > MASKREG(htaddr->htfc))
			bp->b_resid = bp->b_bcount - MASKREG(htaddr->htfc);
	return (MBD_DONE);
}

htndtint(mi)
	register struct mba_device *mi;
{
	register struct htdevice *htaddr = (struct htdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct tu_softc *sc;
	int er, ds, fc;

	ds = MASKREG(htaddr->htds);
	er = MASKREG(htaddr->hter);
	fc = MASKREG(htaddr->htfc);
	if (er) {
		htaddr->htcs1 = HT_DCLR|HT_GO;
		mbclrattn(mi);
	}
	if (bp == 0)
		return (MBN_SKIP);
	sc = &tu_softc[TUUNIT(bp->b_dev)];
	sc->sc_dsreg = ds;
	sc->sc_erreg = er;
	sc->sc_resid = fc;
	if (bp == &chtbuf[HTUNIT(bp->b_dev)]) {
		switch ((int)bp->b_command) {
		case HT_REWOFFL:
			/* offline is on purpose; don't do anything special */
			ds |= HTDS_MOL;	
			break;
		case HT_SREV:
			/* if backspace file hit bot, its not an error */
		        if (er == (HTER_NEF|HTER_FCE) && ds&HTDS_BOT &&
			    bp->b_repcnt == INF)
				er &= ~HTER_NEF;
			break;
		}
		er &= ~HTER_FCE;
		if (er == 0)
			ds &= ~HTDS_ERR;
	}
	if ((ds & (HTDS_ERR|HTDS_MOL)) != HTDS_MOL) {
		if ((ds & HTDS_MOL) == 0 && sc->sc_openf > 0)
			sc->sc_openf = -1;
		tprintf(sc->sc_ctty, "tu%d: hard error bn%d er=%b ds=%b\n",
		    TUUNIT(bp->b_dev), bp->b_blkno,
		    sc->sc_erreg, hter_bits, sc->sc_dsreg, htds_bits);
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);
	}
	if (bp == &chtbuf[HTUNIT(bp->b_dev)]) {
		if (sc->sc_flags & H_REWIND)
			return (ds & HTDS_BOT ? MBN_DONE : MBN_RETRY);
		bp->b_resid = -sc->sc_resid;
		return (MBN_DONE);
	}
	if (ds & HTDS_TM)
		if (sc->sc_blkno > bdbtofsb(bp->b_blkno)) {
			sc->sc_nxrec = bdbtofsb(bp->b_blkno) - fc;
			sc->sc_blkno = sc->sc_nxrec;
		} else {
			sc->sc_blkno = bdbtofsb(bp->b_blkno) + fc;
			sc->sc_nxrec = sc->sc_blkno - 1;
		}
	else
		sc->sc_blkno = bdbtofsb(bp->b_blkno);
	return (MBN_RETRY);
}

/*ARGSUSED*/
htioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	register struct tu_softc *sc = &tu_softc[TUUNIT(dev)];
	register struct buf *bp = &chtbuf[HTUNIT(dev)];
	register callcount;
	int fcount, error = 0;
	struct mtop *mtop;
	struct mtget *mtget;
	/* we depend of the values and order of the MT codes here */
	static htops[] =
   {HT_WEOF,HT_SFORW,HT_SREV,HT_SFORW,HT_SREV,HT_REW,HT_REWOFFL,HT_SENSE};

	switch (cmd) {

	case MTIOCTOP:	/* tape operation */
		mtop = (struct mtop *)data;
		switch (mtop->mt_op) {

		case MTWEOF:
			callcount = mtop->mt_count;
			fcount = 1;
			break;

		case MTFSF: case MTBSF:
			callcount = mtop->mt_count;
			fcount = INF;
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
		while (--callcount >= 0) {
			htcommand(dev, htops[mtop->mt_op], fcount);
			if ((mtop->mt_op == MTFSR || mtop->mt_op == MTBSR) &&
			    bp->b_resid)
				return (EIO);
			if ((bp->b_flags&B_ERROR) || sc->sc_dsreg&HTDS_BOT)
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
		mtget->mt_type = MT_ISHT;
		break;

	default:
		return (ENXIO);
	}
	return (0);
}

#define	DBSIZE	20

htdump()
{
	register struct mba_device *mi;
	register struct mba_regs *mp;
	register struct htdevice *htaddr;
	int blk, num;
	int start;

	start = 0;
	num = maxfree;
#define	phys(a,b)		((b)((int)(a)&0x7fffffff))
	if (htinfo[0] == 0)
		return (ENXIO);
	mi = phys(htinfo[0], struct mba_device *);
	mp = phys(mi->mi_hd, struct mba_hd *)->mh_physmba;
	mp->mba_cr = MBCR_IE;
	htaddr = (struct htdevice *)&mp->mba_drv[mi->mi_drive];
	htaddr->httc = HTTC_PDP11|HTTC_1600BPI;
	htaddr->htcs1 = HT_DCLR|HT_GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		htdwrite(start, blk, htaddr, mp);
		start += blk;
		num -= blk;
	}
	hteof(htaddr);
	hteof(htaddr);
	htwait(htaddr);
	if (htaddr->htds&HTDS_ERR)
		return (EIO);
	htaddr->htcs1 = HT_REW|HT_GO;
	return (0);
}

htdwrite(dbuf, num, htaddr, mp)
	register dbuf, num;
	register struct htdevice *htaddr;
	struct mba_regs *mp;
{
	register struct pte *io;
	register int i;

	htwait(htaddr);
	io = mp->mba_map;
	for (i = 0; i < num; i++)
		*(int *)io++ = dbuf++ | PG_V;
	htaddr->htfc = -(num*NBPG);
	mp->mba_sr = -1;
	mp->mba_bcr = -(num*NBPG);
	mp->mba_var = 0;
	htaddr->htcs1 = HT_WCOM|HT_GO;
}

htwait(htaddr)
	struct htdevice *htaddr;
{
	register s;

	do
		s = htaddr->htds;
	while ((s & HTDS_DRY) == 0);
}

hteof(htaddr)
	struct htdevice *htaddr;
{

	htwait(htaddr);
	htaddr->htcs1 = HT_WEOF|HT_GO;
}
#endif
