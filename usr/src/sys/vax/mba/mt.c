/*	mt.c	4.2	82/01/17	*/

#include "mu.h"
#if NMT > 0
/*
 * TM78/TU78 tape driver
 *
 *	Behavior in complex error situations is uncertain...
 *
 * TODO:
 *	test error recovery
 *	add odd byte count kludge from VMS driver
 *	write dump routine
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
#include "../h/mbareg.h"
#include "../h/mbavar.h"
#include "../h/mtio.h"
#include "../h/ioctl.h"
#include "../h/cmap.h"
#include "../h/cpu.h"

#include "../h/mtreg.h"

struct	buf	rmtbuf[NMT];
struct	buf	cmtbuf[NMT];

short	mttypes[] =
	{ MBDT_TU78, 0 };
struct	mba_device *mtinfo[NMT];
int	mtattach(), mtslave(), mtustart(), mtstart(), mtndtint(), mtdtint();
struct	mba_driver mtdriver =
    { mtattach, mtslave, mtustart, mtstart, mtdtint, mtndtint,
      mttypes, "mt", "mu", mtinfo };

#define MASKREG(r)	((r) & 0xffff)

/* bits in minor device */
#define	MUUNIT(dev)	(minor(dev)&03)
#define	H_NOREWIND	04
#define	H_6250BPI	08

#define MTUNIT(dev)	(mutomt[MUUNIT(dev)])

#define	INF	(daddr_t)1000000L	/* a block number that wont exist */

struct	mu_softc {
	char	sc_openf;
	char	sc_flags;
	daddr_t	sc_blkno;
	daddr_t	sc_nxrec;
	u_short	sc_erreg;
	u_short	sc_dsreg;
	short	sc_resid;
	short	sc_dens;
	struct	mba_device *sc_mi;
	int	sc_slave;
} mu_softc[NMU];
short	mutomt[NMU];

/*
 * Bits for sc_flags.
 */
#define	H_WRITTEN 1	/* last operation was a write */

char	mtds_bits[] = MTDS_BITS;

/*ARGSUSED*/
mtattach(mi)
	struct mba_device *mi;
{

}

mtslave(mi, ms)
	struct mba_device *mi;
	struct mba_slave *ms;
{
	register struct mu_softc *sc = &mu_softc[ms->ms_unit];
	register struct mtdevice *mtaddr = (struct mtdevice *)mi->mi_drv;
	int s = spl7(), rtn = 0;

	mtaddr->mtas = -1;
	mtaddr->mtncs[ms->ms_slave] = MT_SENSE|MT_GO;
	while (mtaddr->mtas == 0)
		;
	if ((mtaddr->mtner & MTER_INTCODE) == MTER_DONE &&
	    (mtaddr->mtds & MTDS_PRES)) {
		sc->sc_mi = mi;
		sc->sc_slave = ms->ms_slave;
		mutomt[ms->ms_unit] = mi->mi_unit;
		rtn = 1;
	}
	mtaddr->mtas = mtaddr->mtas;
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
	int olddens, dens;

	muunit = MUUNIT(dev);
	if (muunit >= NMU || (sc = &mu_softc[muunit])->sc_openf ||
	    (mi = mtinfo[MTUNIT(dev)]) == 0 || mi->mi_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	olddens = sc->sc_dens;
	dens = sc->sc_dens = (minor(dev)&H_6250BPI) ? MT_GCR : 0;
	mtcommand(dev, MT_SENSE, 1);
	sc->sc_dens = olddens;
	if ((sc->sc_dsreg & MTDS_ONL) == 0) {
		uprintf("mu%d: not online\n", muunit);
		u.u_error = EIO;
		return;
	}
	if ((flag&FWRITE) && (sc->sc_dsreg&MTDS_FPT)) {
		uprintf("mu%d: no write ring\n", muunit);
		u.u_error = EIO;
		return;
	}
	if ((sc->sc_dsreg & MTDS_BOT) == 0 && (flag&FWRITE) &&
	    dens != sc->sc_dens) {
		uprintf("mu%d: can't change density in mid-tape\n", muunit);
		u.u_error = EIO;
		return;
	}
	sc->sc_openf = 1;
	sc->sc_blkno = (daddr_t)0;
	sc->sc_nxrec = INF;
	sc->sc_flags = 0;
	sc->sc_dens = dens;
}

mtclose(dev, flag)
	register dev_t dev;
	register flag;
{
	register struct mu_softc *sc = &mu_softc[MUUNIT(dev)];

	if (flag == FWRITE || ((flag&FWRITE) && (sc->sc_flags&H_WRITTEN)))
		mtcommand(dev, MT_CLS|sc->sc_dens, 1);
	if ((minor(dev)&H_NOREWIND) == 0)
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
	mtstrategy(bp);
	if (count == 0)
		return;
	iowait(bp);
	if (bp->b_flags&B_WANTED)
		wakeup((caddr_t)bp);
	bp->b_flags &= B_ERROR;
}

mtstrategy(bp)
	register struct buf *bp;
{
	register struct mba_device *mi = mtinfo[MTUNIT(bp->b_dev)];
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

mtustart(mi)
	register struct mba_device *mi;
{
	register struct mtdevice *mtaddr =
	    (struct mtdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct mu_softc *sc = &mu_softc[MUUNIT(bp->b_dev)];
	daddr_t blkno;

	sc->sc_flags &= ~H_WRITTEN;
	if (sc->sc_openf < 0) {
		bp->b_flags |= B_ERROR;
		return (MBU_NEXT);
	}
	if (bp != &cmtbuf[MTUNIT(bp->b_dev)]) {
		if (dbtofsb(bp->b_blkno) > sc->sc_nxrec) {
			bp->b_flags |= B_ERROR;
			bp->b_error = ENXIO;
			return (MBU_NEXT);
		}
		if (dbtofsb(bp->b_blkno) == sc->sc_nxrec &&
		    bp->b_flags&B_READ) {
			bp->b_resid = bp->b_bcount;
			clrbuf(bp);
			return (MBU_NEXT);
		}
		if ((bp->b_flags&B_READ)==0)
			sc->sc_nxrec = dbtofsb(bp->b_blkno) + 1;
	} else {
		mtaddr->mtncs[MUUNIT(bp->b_dev)] =
			(bp->b_repcnt<<8)|bp->b_command|MT_GO;
		return (MBU_STARTED);
	}
	if ((blkno = sc->sc_blkno) == dbtofsb(bp->b_blkno)) {
		if (mi->mi_tab.b_errcnt == 2) {
			mtaddr->mtca = MUUNIT(bp->b_dev);
		} else {
			mtaddr->mtbc = bp->b_bcount;
			mtaddr->mtca = (1<<2)|MUUNIT(bp->b_dev);
		}
		return (MBU_DODATA);
	}
	if (blkno < dbtofsb(bp->b_blkno))
		mtaddr->mtncs[MUUNIT(bp->b_dev)] =
		  (min(dbtofsb(bp->b_blkno) - blkno, 0377)<<8)| MT_SFORW|MT_GO;
	else
		mtaddr->mtncs[MUUNIT(bp->b_dev)] =
		  (min(blkno - dbtofsb(bp->b_blkno), 0377)<<8)| MT_SREV|MT_GO;
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

	/* I'M NOT SURE IF THIS SHOULD ALWAYS BE THE CASE SO FOR NOW... */
	if ((mtaddr->mtca&3) != MUUNIT(bp->b_dev)) {
		printf("mt: wrong unit!\n");
		mtaddr->mtca = MUUNIT(bp->b_dev);
	}
	sc = &mu_softc[MUUNIT(bp->b_dev)];
	sc->sc_erreg = mtaddr->mter;
	if((bp->b_flags & B_READ) == 0)
		sc->sc_flags |= H_WRITTEN;
	switch (sc->sc_erreg & MTER_INTCODE) {
	case MTER_DONE:
	case MTER_LONGREC:
		if (mi->mi_tab.b_errcnt != 2)
			sc->sc_blkno++;
		bp->b_resid = 0;
		break;

	case MTER_NOTCAP:
		printf("mu%d: blank tape\n", MUUNIT(bp->b_dev));
		goto err;

	case MTER_TM:
	case MTER_EOT:
		sc->sc_blkno++;
	err:
		bp->b_resid = bp->b_bcount;
		sc->sc_nxrec = dbtofsb(bp->b_blkno);
		break;

	case MTER_SHRTREC:
		sc->sc_blkno++;
		if (bp != &rmtbuf[MTUNIT(bp->b_dev)])
			bp->b_flags |= B_ERROR;
		if (mi->mi_tab.b_errcnt == 2)
			bp->b_bcount = bp->b_resid;	/* restore saved value */
		bp->b_resid = bp->b_bcount - mtaddr->mtbc;
		break;

	case MTER_RDOPP:
		mi->mi_tab.b_errcnt = 2;	/* indicate "read opposite" */
		bp->b_resid = bp->b_bcount;	/* save it */
		bp->b_bcount = mtaddr->mtbc;	/* use this instead */
		return(MBD_RETRY);

	case MTER_RETRY:
		mi->mi_tab.b_errcnt = 1;	/* indicate simple retry */
		return(MBD_RETRY);

	case MTER_OFFLINE:
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			printf("mu%d: offline\n", MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		break;

	case MTER_FPT:
		printf("mu%d: no write ring\n", MUUNIT(bp->b_dev));
		bp->b_flags |= B_ERROR;
		break;

	default:
		printf("mu%d: hard error bn%d mbsr=%b er=%x ds=%b\n",
		    MUUNIT(bp->b_dev), bp->b_blkno,
		    mbsr, mbsr_bits, sc->sc_erreg,
		    sc->sc_dsreg, mtds_bits);
		bp->b_flags |= B_ERROR;
		mtaddr->mtid = MTID_CLR;		/* reset the TM78 */
		DELAY(250);
		while ((mtaddr->mtid & MTID_RDY) == 0)	/* wait for it */
			;
		return (MBD_DONE);
	}
	/* CHECK FOR MBA ERROR WHEN NO OTHER ERROR INDICATED? */
	return (MBD_DONE);
}

mtndtint(mi)
	register struct mba_device *mi;
{
	register struct mtdevice *mtaddr = (struct mtdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct mu_softc *sc;
	int er, fc, unit;

	unit = (mtaddr->mtner >> 8) & 3;
	er = MASKREG(mtaddr->mtner);
	/* WILL THIS OCCUR IF ANOTHER DRIVE COMES ONLINE? */
	if (bp == 0 || unit != MUUNIT(bp->b_dev)) {	/* consistency check */
		if ((er & MTER_INTCODE) != MTER_ONLINE)
			printf("mt: unit %d random interrupt\n", unit);
		return (MBN_SKIP);
	}
	if (bp == 0)
		return (MBN_SKIP);
	fc = (mtaddr->mtncs[unit] >> 8) & 0xff;
	sc = &mu_softc[unit];
	sc->sc_erreg = er;
	sc->sc_resid = fc;
	switch (er & MTER_INTCODE) {
	case MTER_DONE:
		if (bp == &cmtbuf[MTUNIT(bp->b_dev)]) {
	done:
			if (bp->b_command == MT_SENSE)
				sc->sc_dsreg = MASKREG(mtaddr->mtds);
			bp->b_resid = fc;
			return (MBN_DONE);
		}
		/* this is UGLY!  (but is it correct?) */
		if ((fc = dbtofsb(bp->b_blkno) - sc->sc_blkno) < 0)
			sc->sc_blkno -= min(0377, -fc);
		else
			sc->sc_blkno += min(0377, fc);
		return (MBN_RETRY);

	case MTER_RWDING:
		return (MBN_SKIP);	/* ignore "rewind started" interrupt */

	case MTER_NOTCAP:
		printf("mu%d: blank tape\n", MUUNIT(bp->b_dev));

	case MTER_TM:
	case MTER_EOT:
	case MTER_LEOT:
		if (sc->sc_blkno > dbtofsb(bp->b_blkno)) {
			sc->sc_nxrec = dbtofsb(bp->b_blkno) + fc;
			sc->sc_blkno = sc->sc_nxrec;
		} else {
			sc->sc_blkno = dbtofsb(bp->b_blkno) - fc;
			sc->sc_nxrec = sc->sc_blkno - 1;
		}
		return (MBN_RETRY);

	case MTER_FPT:
		printf("mu%d: no write ring\n", MUUNIT(bp->b_dev));
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_OFFLINE:
		if (sc->sc_openf > 0) {
			sc->sc_openf = -1;
			printf("mu%d: offline\n", MUUNIT(bp->b_dev));
		}
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);

	case MTER_BOT:
		if (bp == &cmtbuf[MTUNIT(bp->b_dev)])
			goto done;
		/* FALL THROUGH */

	default:
		printf("mu%d: hard error bn%d er=%o ds=%b\n",
		    MUUNIT(bp->b_dev), bp->b_blkno,
		    sc->sc_erreg, sc->sc_dsreg, mtds_bits);
		mtaddr->mtid = MTID_CLR;		/* reset the TM78 */
		DELAY(250);
		while ((mtaddr->mtid & MTID_RDY) == 0)	/* wait for it */
			;
		bp->b_flags |= B_ERROR;
		return (MBN_DONE);
	}
	/* NOTREACHED */
}

mtread(dev)
	dev_t dev;
{

	mtphys(dev);
	if (u.u_error)
		return;
	physio(mtstrategy, &rmtbuf[MTUNIT(dev)], dev, B_READ, minphys);
}

mtwrite(dev)
{

	mtphys(dev);
	if (u.u_error)
		return;
	physio(mtstrategy, &rmtbuf[MTUNIT(dev)], dev, B_WRITE, minphys);
}

mtphys(dev)
	dev_t dev;
{
	register int mtunit;
	register struct mu_softc *sc;
	register struct mba_device *mi;
	daddr_t a;

	mtunit = MTUNIT(dev);
	if (mtunit >= NMT || (mi = mtinfo[mtunit]) == 0 || mi->mi_alive == 0) {
		u.u_error = ENXIO;
		return;
	}
	a = u.u_offset >> 9;
	sc = &mu_softc[MUUNIT(dev)];
	sc->sc_blkno = dbtofsb(a);
	sc->sc_nxrec = dbtofsb(a)+1;
}

/*ARGSUSED*/
mtioctl(dev, cmd, addr, flag)
	dev_t dev;
	int cmd;
	caddr_t addr;
	int flag;
{
	register struct mu_softc *sc = &mu_softc[MUUNIT(dev)];
	register struct buf *bp = &cmtbuf[MTUNIT(dev)];
	register callcount;
	register int n, op;
	int fcount;
	struct mtop mtop;
	struct mtget mtget;
	/* we depend of the values and order of the MT codes here */
	static mtops[] =
	{MT_WTM,MT_SFORWF,MT_SREVF,MT_SFORW,MT_SREV,MT_REW,MT_UNLOAD,MT_SENSE};

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
			fcount = 1;
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
		op = mtops[mtop.mt_op];
		if (op == MT_WTM)
			op |= sc->sc_dens;
		while (--callcount >= 0) {
			register int n;

			do {
				n = min(fcount, 0xff);
				mtcommand(dev, op, n);
				fcount -= n;
			} while (fcount);
			if ((mtop.mt_op == MTFSR || mtop.mt_op == MTBSR) &&
			    bp->b_resid) {
				u.u_error = EIO;
				break;
			}
			if (bp->b_flags&B_ERROR)
				break;
		}
		geterror(bp);
		return;
	case MTIOCGET:
		mtget.mt_erreg = sc->sc_erreg;
		mtget.mt_resid = sc->sc_resid;
		mtcommand(dev, MT_SENSE, 1);	/* update drive status */
		mtget.mt_dsreg = sc->sc_dsreg;
		mtget.mt_type = MT_ISMT;
		if (copyout((caddr_t)&mtget, addr, sizeof(mtget)))
			u.u_error = EFAULT;
		return;
	default:
		u.u_error = ENXIO;
	}
}

#define	DBSIZE	20

mtdump()
{
	register struct mba_device *mi;
	register struct mba_regs *mp;
	register struct mtdevice *mtaddr;
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
	mtaddr = (struct mtdevice *)&mp->mba_drv[mi->mi_drive];
#ifdef notyet
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
#endif
