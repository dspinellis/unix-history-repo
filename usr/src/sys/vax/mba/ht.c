#include "ht.h"
#if NHT > 0
/*
 * TM03/TU?? tape driver
 */
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/buf.h"
#include "../h/conf.h"
#include "../h/dir.h"
#include "../h/file.h"
#include "../h/user.h"
#include "../h/map.h"
#include "../h/pte.h"
#include "../h/mba.h"
#include "../h/mtio.h"
#include "../h/ioctl.h"
#include "../h/cmap.h"

#include "../h/htreg.h"

struct	buf	rhtbuf[NHT];
struct	buf	chtbuf[NHT];

short	httypes[] =
	{ MBDT_TE16, MBDT_TU45, MBDT_TU77, 0 };
struct	mba_info *htinfo[NHT];
int	htdkinit(), htustart(), htndtint(), htdtint();
struct	mba_driver htdriver =
	{ htdkinit, htustart, 0, htdtint, htndtint, httypes, htinfo };

#define MASKREG(r)	((r) & 0xffff)

/* bits in minor device */
#define HTUNIT(dev)	(minor(dev)&03)
#define	H_NOREWIND	04
#define	H_1600BPI	08

#define	INF	(daddr_t)1000000L	/* a block number that wont exist */

struct	ht_softc {
	char	sc_openf;
	char	sc_flags;
	daddr_t	sc_blkno;
	daddr_t	sc_nxrec;
	u_short	sc_erreg;
	u_short	sc_dsreg;
	short	sc_resid;
	short	sc_dens;
} ht_softc[NHT];

/*
 * States for b_active for device.
 */
#define	SIO	1
#define	SSFOR	2
#define	SSREV	3
#define	SRETRY	4
#define	SCOM	5
#define	SOK	6
#define	SEOF	7

/*
 * Bits for sc_flags.
 */
#define	H_WRITTEN 1	/* last operation was a write */
#define H_ERASED  2	/* last write retry was an erase gap */
#define H_REWIND  4	/* last unit start was a rewind */

/*ARGSUSED*/
htdkinit(mi)
	struct mba_info *mi;
{

}

htopen(dev, flag)
	dev_t dev;
	int flag;
{
	register int unit;
	register struct mba_info *mi;
	register struct ht_softc *sc;

	unit = HTUNIT(dev);
	if (unit >= NHT || (sc = &ht_softc[unit])->sc_openf ||
	    (mi = htinfo[unit]) == 0 || mi->mi_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	/*
	 * The NOP below serves two purposes:
	 * 1. To get a recent copy of the status registers.
	 * 2. To ensure that any outstanding rewinds are truly finished
	 *	so that the test for BOT is valid.
	 */
	htcommand(dev, HT_SENSE, 1);
	if ((sc->sc_dsreg & HTDS_MOL) == 0 || 
	   (flag & (FREAD|FWRITE)) == FWRITE && sc->sc_dsreg&HTDS_WRL) {
		u.u_error = EIO;
		return;
	}
	sc->sc_dens =
	    ((minor(dev)&H_1600BPI)?HTTC_1600BPI:HTTC_800BPI)|HTTC_PDP11|unit;
	sc->sc_openf = 1;
	sc->sc_blkno = (daddr_t)0;
	sc->sc_nxrec = INF;
	sc->sc_flags = 0;
}

htclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register struct ht_softc *sc = &ht_softc[HTUNIT(dev)];

	if (flag == FWRITE || ((flag&FWRITE) && (sc->sc_flags&H_WRITTEN))) {
		htcommand(dev, HT_WEOF, 1);
		htcommand(dev, HT_WEOF, 1);
		htcommand(dev, HT_SREV, 1);
	}
	if ((minor(dev)&H_NOREWIND) == 0)
		/* 0 as third arg means don't wait */
		htcommand(dev, HT_REW, 0);
	sc->sc_openf = 0;
}

/*
 * Do a non-data-transfer command.
 *
 * N.B.: Count should be zero ONLY for rewind during close.
 */
htcommand(dev, com, count)
	dev_t dev;
	int com, count;
{
	register struct buf *bp;

	bp = &chtbuf[HTUNIT(dev)];
	(void) spl5();
	/*
	 * The B_DONE test is to allow the rewind on close not to wait at
	 * all.  We just must make sure that it was an asynchronous rewind,
	 * otherwise if it isn't we might wake up before the process
	 * waiting for the command (we are waiting for the buffer here).
	 */
	while (bp->b_flags&B_BUSY) {
		if (bp->b_flags&B_DONE && bp->b_repcnt == 0)
			break;
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY;
	(void) spl0();
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
	register int unit = HTUNIT(bp->b_dev);
	register struct mba_info *mi;
	register struct buf *dp;
	register struct ht_softc *sc = &ht_softc[unit];

	bp->av_forw = NULL;
	dp = &mi->mi_tab;
	(void) spl5();
	if (dp->b_actf == NULL)
		dp->b_actf = bp;
	else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	if (dp->b_active == 0)
		mbustart(mi);
	(void) spl0();
}

htustart(mi)
	register struct mba_info *mi;
{
	register struct htdevice *htaddr =
	    (struct htdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	int unit = HTUNIT(bp->b_dev);
	register struct ht_softc *sc = &ht_softc[unit];
	daddr_t blkno;

	htaddr->httc = sc->sc_dens;
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
	if (bp != &chtbuf[unit]) {
		if (dbtofsb(bp->b_blkno) > sc->sc_nxrec) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
			goto next;
		} else if (dbtofsb(bp->b_blkno) == sc->sc_nxrec &&
		    bp->b_flags&B_READ) {
			bp->b_resid = bp->b_bcount;
			clrbuf(bp);
			goto next;
		} else if ((bp->b_flags&B_READ)==0)
			sc->sc_nxrec = dbtofsb(bp->b_blkno) + 1;
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
	if ((blkno = sc->sc_blkno) == dbtofsb(bp->b_blkno)) {
		htaddr->htfc = -bp->b_bcount;
		if ((bp->b_flags&B_READ) == 0) {
			if (mi->mi_tab.b_errcnt)
				if (sc->sc_flags & H_ERASED)
					sc->sc_flags &= ~H_ERASED;
				else {
					sc->sc_flags |= H_ERASED;
					htaddr->htcs1 = HT_ERASE | HT_GO;
					return (MBU_STARTED);
				}
			if (htaddr->htds & HTDS_EOT) {
				bp->b_resid = bp->b_bcount;
				return (MBU_NEXT);
			}
		}
		return (MBU_DODATA);
	}
	if (blkno < dbtofsb(bp->b_blkno)) {
		htaddr->htfc = blkno - dbtofsb(bp->b_blkno);
		htaddr->htcs1 = HT_SFORW|HT_GO;
	} else {
		htaddr->htfc = dbtofsb(bp->b_blkno) - blkno;
		htaddr->htcs1 = HT_SREV|HT_GO;
	}
	return (MBU_STARTED);
next:
	iodone(bp);
	return (MBU_NEXT);
}

/*
 * data transfer interrupt - must be read or write
 */
/*ARGSUSED*/
htdtint(mi, mbasr)
	register struct mba_info *mi;
	int mbasr;
{
	register struct htdevice *htaddr = (struct htdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct ht_softc *sc;
	int ds, er;

	if (bp == NULL)
		return;
	sc = &ht_softc[HTUNIT(bp->b_dev)];
	ds = sc->sc_dsreg = MASKREG(htaddr->htds);
	er = sc->sc_erreg = MASKREG(htaddr->hter);
	sc->sc_resid = MASKREG(htaddr->htfc);
	sc->sc_blkno++;
	if((bp->b_flags & B_READ) == 0)
		sc->sc_flags |= H_WRITTEN;
	if ((ds&(HTDS_ERR|HTDS_MOL)) != HTDS_MOL ||
	    mbasr & MBAEBITS) {
		htaddr->htcs1 = HT_DCLR|HT_GO;
		if (bp == &rhtbuf[HTUNIT(bp->b_dev)])
			er &= ~HTER_FCE;
		if (bp->b_flags & B_READ && ds & HTDS_PES)
			er &= ~(HTER_CSITM|HTER_CORCRC);
		if (er&HTER_HARD ||
		    mbasr&MBAEBITS || (ds&HTDS_MOL) == 0 ||
		    sc->sc_erreg && ++mi->mi_tab.b_errcnt >= 7) {
			if ((ds & HTDS_MOL) == 0 && sc->sc_openf > 0)
				sc->sc_openf = -1;
			printf("ht%d: hard error bn%d mbasr=%b er=%b\n",
			    HTUNIT(bp->b_dev), bp->b_blkno,
			    mbasr, mbasr_bits,
			    MASKREG(htaddr->hter), HTER_BITS);
			bp->b_flags |= B_ERROR;
			return (MBD_DONE);
		}
		if (er)
			return (MBD_RETRY);
	}
	bp->b_resid = 0;
	if (bp->b_flags & B_READ)
		if (ds&HTDS_TM) {		/* must be a read, right? */
			bp->b_resid = bp->b_bcount;
			sc->sc_nxrec = dbtofsb(bp->b_blkno);
		} else if(bp->b_bcount > MASKREG(htaddr->htfc))
			bp->b_resid = bp->b_bcount - MASKREG(htaddr->htfc);
	return (MBD_DONE);
}

/*
 * non-data-transfer interrupt
 */
htndtint(mi)
	register struct mba_info *mi;
{
	register struct htdevice *htaddr = (struct htdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct ht_softc *sc;
	int er, ds, fc;

	if (bp == NULL)
		return;
	sc = &ht_softc[HTUNIT(bp->b_dev)];
	ds = sc->sc_dsreg = MASKREG(htaddr->htds);
	er = sc->sc_erreg = MASKREG(htaddr->hter);
	sc->sc_resid = MASKREG(htaddr->htfc);
	if (sc->sc_erreg)
		htaddr->htcs1 = HT_DCLR|HT_GO;
	if (bp == &chtbuf[HTUNIT(bp->b_dev)]) {
		if (bp->b_command == HT_REWOFFL)
			/* offline is on purpose; don't do anything special */
			ds |= HTDS_MOL;	
		else if (bp->b_resid == HT_SREV &&
		    er == (HTER_NEF|HTER_FCE) &&
		    ds&HTDS_BOT && bp->b_bcount == INF)
			er &= ~HTER_NEF;
		er &= ~HTER_FCE;
		if (er == 0)
			ds &= ~HTDS_ERR;
	}
	if ((ds & (HTDS_ERR|HTDS_MOL)) != HTDS_MOL) {
		if ((ds & HTDS_MOL) == 0 && sc->sc_openf > 0)
			sc->sc_openf = -1;
		printf("ht%d: hard error bn%d er=%b ds=%b\n",
		    HTUNIT(bp->b_dev), bp->b_blkno,
		    sc->sc_erreg, HTER_BITS, sc->sc_dsreg, HTDS_BITS);
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
		if (sc->sc_blkno > dbtofsb(bp->b_blkno)) {/* reversing */
			sc->sc_nxrec = dbtofsb(bp->b_blkno) - fc;
			sc->sc_blkno = sc->sc_nxrec;
		} else {			/* spacing forward */
			sc->sc_blkno = dbtofsb(bp->b_blkno) + fc;
			sc->sc_nxrec = sc->sc_blkno - 1;
		}
	else
		sc->sc_blkno = dbtofsb(bp->b_blkno);
	return (MBN_RETRY);
}

htread(dev)
	dev_t dev;
{

	htphys(dev);
	if (u.u_error)
		return;
	physio(htstrategy, &rhtbuf[HTUNIT(dev)], dev, B_READ, minphys);
}

htwrite(dev)
{

	htphys(dev);
	if (u.u_error)
		return;
	physio(htstrategy, &rhtbuf[HTUNIT(dev)], dev, B_WRITE, minphys);
}

htphys(dev)
	dev_t dev;
{
	register int unit;
	register struct ht_softc *sc;
	daddr_t a;

	unit = HTUNIT(dev);
	if (unit >= NHT) {
		u.u_error = ENXIO;
		return;
	}
	a = u.u_offset >> 9;
	sc = &ht_softc[unit];
	sc->sc_blkno = dbtofsb(a);
	sc->sc_nxrec = dbtofsb(a)+1;
}

/*ARGSUSED*/
htioctl(dev, cmd, addr, flag)
	dev_t dev;
	int cmd;
	caddr_t addr;
	int flag;
{
	register unit = HTUNIT(dev);
	register struct ht_softc *sc = &ht_softc[unit];
	register struct buf *bp = &chtbuf[unit];
	register callcount;
	int fcount;
	struct mtop mtop;
	struct mtget mtget;
	/* we depend of the values and order of the MT codes here */
	static htops[] =
   {HT_WEOF,HT_SFORW,HT_SREV,HT_SFORW,HT_SREV,HT_REW,HT_REWOFFL,HT_SENSE};

	switch (cmd) {
		case MTIOCTOP:	/* tape operation */
		if (copyin((caddr_t)addr, (caddr_t)&mtop, sizeof(mtop))) {
			u.u_error = EFAULT;
			return;
		}
		switch(mtop.mt_op) {
		case MTWEOF:
			callcount = mtop.mt_count;
			fcount = 1;
			break;
		case MTFSF: case MTBSF:
			callcount = mtop.mt_count;
			fcount = INF;
			break;
		case MTFSR: case MTBSR:
			callcount = 1;
			fcount = mtop.mt_count;
			break;
		case MTREW: case MTOFFL:
			callcount = 1;
			fcount = 1;
			break;
		default:
			u.u_error = ENXIO;
			return;
		}
		if (callcount <= 0 || fcount <= 0) {
			u.u_error = ENXIO;
			return;
		}
		while (--callcount >= 0) {
			htcommand(dev, htops[mtop.mt_op], fcount);
			if ((mtop.mt_op == MTFSR || mtop.mt_op == MTBSR) &&
			    bp->b_resid) {
				u.u_error = EIO;
				break;
			}
			if ((chtbuf[HTUNIT(bp->b_dev)].b_flags&B_ERROR) ||
			    sc->sc_dsreg&HTDS_BOT)
				break;
		}
		geterror(bp);
		return;
	case MTIOCGET:
		mtget.mt_dsreg = sc->sc_dsreg;
		mtget.mt_erreg = sc->sc_erreg;
		mtget.mt_resid = sc->sc_resid;
		if (copyout((caddr_t)&mtget, addr, sizeof(mtget)))
			u.u_error = EFAULT;
		return;
	default:
		u.u_error = ENXIO;
	}
}

#define	DBSIZE	20

htdump()
{
	register struct mba_info *mi;
	register struct mba_regs *mp;
	register struct htdevice *htaddr;
	int blk, num;
	int start;

	start = 0;
	num = maxfree;
#define	phys(a,b)		((b)((int)(a)&0x7fffffff))
	if (htinfo[0] == 0)
		return (ENXIO);
	mi = phys(htinfo[0], struct mba_info *);
	mp = phys(mi->mi_hd, struct mba_hd *)->mh_physmba;
#if VAX780
	mbainit(mp);
	htaddr = (struct htdevice *)&mp->mba_drv[mi->mi_drive];
	htaddr->httc = HTTC_PDP11|HTTC_1600BPI;
	htaddr->htcs1 = HT_DCLR|HT_GO;
	while (num > 0) {
		blk = num > DBSIZE ? DBSIZE : num;
		htdwrite(start, blk, htaddr, mp);
		start += blk;
		num -= blk;
	}
	htwait(htaddr);
	htaddr->htcs1 = HT_REW|HT_GO;
	hteof(htaddr);
	hteof(htaddr);
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
