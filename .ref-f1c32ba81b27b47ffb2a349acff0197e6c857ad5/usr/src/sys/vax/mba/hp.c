/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)hp.c	7.22 (Berkeley) %G%
 */

#ifdef HPDEBUG
int	hpdebug;
#endif
#ifdef HPBDEBUG
int	hpbdebug;
#endif

#include "hp.h"
#if NHP > 0
/*
 * HP disk driver for RP0x+RMxx+ML11
 *
 * TODO:
 *	see if DCLR and/or RELEASE set attention status
 */
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/dkstat.h"
#include "sys/buf.h"
#include "sys/conf.h"
#include "sys/file.h"
#include "sys/user.h"
#include "sys/map.h"
#include "../include/mtpr.h"
#include "sys/vm.h"
#include "sys/cmap.h"
#include "sys/dkbad.h"
#include "sys/disklabel.h"
#include "sys/ioctl.h"
#include "sys/syslog.h"
#include "sys/stat.h"

#include "../include/pte.h"
#include "mbareg.h"
#include "mbavar.h"
#include "hpreg.h"

#define	COMPAT_42
#define	B_FORMAT	B_XXX

/*
 * Table of supported Massbus drive types.
 * When using unlabeled packs, slot numbers here
 * are used as indices into the partition tables.
 * Slots are left for those drives divined from other means
 * (e.g. SI, AMPEX, etc.).
 */
short	hptypes[] = {
#define	HPDT_RM03	0
	MBDT_RM03,
#define	HPDT_RM05	1
	MBDT_RM05,
#define	HPDT_RP06	2
	MBDT_RP06,
#define	HPDT_RM80	3
	MBDT_RM80,
#define	HPDT_RP04	4
	MBDT_RP04,
#define	HPDT_RP05	5
	MBDT_RP05,
#define	HPDT_RP07	6
	MBDT_RP07,
#define	HPDT_ML11A	7
	MBDT_ML11A,
#define	HPDT_ML11B	8
	MBDT_ML11B,
#define	HPDT_9775	9
	-1,
#define	HPDT_9730	10
	-1,
#define	HPDT_CAPRICORN	11
	-1,
#define HPDT_EAGLE	12
	-1,
#define	HPDT_9300	13
	-1,
#define HPDT_RM02	14
	MBDT_RM02,		/* beware, actually mapped */
#define HPDT_2361	15
	-1,
#define HPDT_2361A	16
	-1,
	0
};

struct	mba_device *hpinfo[NHP];
int	hpattach(),hpustart(),hpstart(),hpdtint(),hpstrategy();
struct	mba_driver hpdriver =
	{ hpattach, 0, hpustart, hpstart, hpdtint, 0,
	  hptypes, "hp", 0, hpinfo };

u_char	hp_offset[16] = {
    HPOF_P400, HPOF_M400, HPOF_P400, HPOF_M400,
    HPOF_P800, HPOF_M800, HPOF_P800, HPOF_M800,
    HPOF_P1200, HPOF_M1200, HPOF_P1200, HPOF_M1200,
    0, 0, 0, 0,
};

struct	disklabel hplabel[NHP];
struct	dkbad	hpbad[NHP];

struct	hpsoftc {
	u_char	sc_recal;	/* recalibrate state */
	u_char	sc_doseeks;	/* perform explicit seeks */
#ifdef COMPAT_42
	u_char	sc_hdr;		/* next i/o includes header */
#endif
	int	sc_state;	/* open fsm */
	int	sc_wlabel;	/* label sector is currently writable */
	u_long	sc_openpart;	/* bit mask of open subunits */
	u_long	sc_copenpart;	/* bit mask of open character subunits */
	u_long	sc_bopenpart;	/* bit mask of open block subunits */
	daddr_t	sc_mlsize;	/* ML11 size */
	int	sc_blkdone;	/* amount sucessfully transfered */
	daddr_t	sc_badbn;	/* replacement block number */
	int	sc_status;	/* copy of drive status reg. after format */
	int	sc_hpds;	/* copy of hpds reg. after format */
	int	sc_er1;		/* copy of error reg. 1 after format */
	int	sc_er2;		/* copy of error reg. 2 after format */
} hpsoftc[NHP];

/*
 * Drive states.  Used during steps of open/initialization.
 * States < OPEN (> 0) are transient, during an open operation.
 * OPENRAW is used for unlabeled disks,
 * to inhibit bad-sector forwarding or allow format operations.
 */
#define	CLOSED		0		/* disk is closed. */
#define	WANTOPEN	1		/* open requested, not started */
#define	WANTOPENRAW	2		/* open requested, no label */
#define	RDLABEL		3		/* reading pack label */
#define	RDBADTBL	4		/* reading bad-sector table */
#define	OPEN		5		/* initialized and ready */
#define	OPENRAW		6		/* open, no label or badsect */

#define	b_cylin b_resid
 
/* #define ML11 0  to remove ML11 support */
#define	ML11(type)	((type) == HPDT_ML11A)
#define	RP06(type)	(hptypes[type] <= MBDT_RP06)
#define	RM80(type)	((type) == HPDT_RM80)

#define hpunit(dev)	(minor(dev) >> 3)
#define hppart(dev)	(minor(dev) & 07)
#define hpminor(unit, part)	(((unit) << 3) | (part))

#define	MASKREG(reg)	((reg)&0xffff)
#ifdef lint
#define HPWAIT(mi, addr) (hpwait(mi))
#else
#define HPWAIT(mi, addr) (((addr)->hpds & HPDS_DRY) || hpwait(mi))
#endif

/*ARGSUSED*/
hpattach(mi, slave)
	struct mba_device *mi;
{
	register int unit = mi->mi_unit;
	extern int cold;

	if (cold) {
		/*
		 * Try to initialize device and read pack label.
		 */
		if (hpinit(hpminor(unit, 0), 0) == 0) {
			printf(": %s", hplabel[unit].d_typename);
#ifdef notyet
			addswap(makedev(HPMAJOR, hpminor(unit, 0)),
			    &hplabel[unit]);
#endif
		} else
			printf(": offline");
	}
}

hpopen(dev, flags, fmt)
	dev_t dev;
	int flags, fmt;
{
	register int unit = hpunit(dev);
	register struct hpsoftc *sc;
	register struct disklabel *lp;
	register struct partition *pp;
	struct mba_device *mi;
	int s, error, part = hppart(dev), mask = 1 << part;
	daddr_t start, end;

	if (unit >= NHP || (mi = hpinfo[unit]) == 0 || mi->mi_alive == 0)
		return (ENXIO);
	sc = &hpsoftc[unit];
	lp = &hplabel[unit];

	s = spl5();
	while (sc->sc_state != OPEN && sc->sc_state != OPENRAW &&
	    sc->sc_state != CLOSED)
		if (error = tsleep((caddr_t)sc, (PZERO+1) | PCATCH,
		    devopn, 0)) {
			splx(s);
			return (error);
		}
	splx(s);
	if (sc->sc_state != OPEN && sc->sc_state != OPENRAW)
		if (error = hpinit(dev, flags))
			return (error);
	if (part >= lp->d_npartitions)
		return (ENXIO);
	/*
	 * Warn if a partion is opened
	 * that overlaps another partition which is open
	 * unless one is the "raw" partition (whole disk).
	 */
#define	RAWPART		2		/* 'c' partition */	/* XXX */
	if ((sc->sc_openpart & mask) == 0 && part != RAWPART) {
		pp = &lp->d_partitions[part];
		start = pp->p_offset;
		end = pp->p_offset + pp->p_size;
		for (pp = lp->d_partitions;
		     pp < &lp->d_partitions[lp->d_npartitions]; pp++) {
			if (pp->p_offset + pp->p_size <= start ||
			    pp->p_offset >= end)
				continue;
			if (pp - lp->d_partitions == RAWPART)
				continue;
			if (sc->sc_openpart & (1 << (pp - lp->d_partitions)))
				log(LOG_WARNING,
				    "hp%d%c: overlaps open partition (%c)\n",
				    unit, part + 'a',
				    pp - lp->d_partitions + 'a');
		}
	}
	switch (fmt) {
	case S_IFCHR:
		sc->sc_copenpart |= mask;
		break;
	case S_IFBLK:
		sc->sc_bopenpart |= mask;
		break;
	}
	sc->sc_openpart |= mask;
	return (0);
}

/* ARGSUSED */
hpclose(dev, flags, fmt)
	dev_t dev;
	int flags, fmt;
{
	register int unit = hpunit(dev);
	register struct hpsoftc *sc;
	struct mba_device *mi;
	int s, mask = 1 << hppart(dev);

	sc = &hpsoftc[unit];
	mi = hpinfo[unit];
	switch (fmt) {
	case S_IFCHR:
		sc->sc_copenpart &= ~mask;
		break;
	case S_IFBLK:
		sc->sc_bopenpart &= ~mask;
		break;
	}
	sc->sc_openpart = sc->sc_copenpart | sc->sc_bopenpart;

	/*
	 * Should wait for I/O to complete on this partition
	 * even if others are open, but wait for work on blkflush().
	 */
	if (sc->sc_openpart == 0) {
		s = spl5();
		while (mi->mi_tab.b_actf)
			sleep((caddr_t)sc, PZERO - 1);
		splx(s);
		sc->sc_state = CLOSED;
		sc->sc_wlabel = 0;
	}
	return (0);
}

hpinit(dev, flags)
	dev_t dev;
	int flags;
{
	register struct hpsoftc *sc;
	register struct buf *bp;
	register struct disklabel *lp;
	struct mba_device *mi;
	struct hpdevice *hpaddr;
	struct dkbad *db;
	char *msg, *readdisklabel();
	int unit, i, error = 0;

	unit = hpunit(dev);
	sc = &hpsoftc[unit];
	lp = &hplabel[unit];
	mi = hpinfo[unit];
	hpaddr = (struct hpdevice *)mi->mi_drv;

	sc->sc_state = WANTOPEN;
	/*
	 * Use the default sizes until we've read the label,
	 * or longer if there isn't one there.
	 */
	if (lp->d_secpercyl == 0) {
		lp->d_secsize = DEV_BSIZE;
		lp->d_nsectors = 32;
		lp->d_ntracks = 20;
		lp->d_secpercyl = 32*20;
		lp->d_npartitions = 1;
		lp->d_partitions[0].p_offset = 0;
		lp->d_partitions[0].p_size = LABELSECTOR + 1;
	}

	if (flags & O_NDELAY)
		goto raw;

	/*
	 * Map all ML11's to the same type.  Also calculate
	 * transfer rates based on device characteristics.
	 * Set up dummy label with all that's needed.
	 */
	if (mi->mi_type == MBDT_ML11A || mi->mi_type == MBDT_ML11B) {
		register int trt;

		sc->sc_mlsize = hpaddr->hpmr & HPMR_SZ;
		if ((hpaddr->hpmr & HPMR_ARRTYP) == 0)
			sc->sc_mlsize >>= 2;
		if (mi->mi_dk >= 0) {
			trt = (hpaddr->hpmr & HPMR_TRT) >> 8;
			dk_wpms[mi->mi_dk] = (1<<(20-trt));
		}
		mi->mi_type = MBDT_ML11A;
		lp->d_partitions[0].p_size = sc->sc_mlsize;
		lp->d_secpercyl = sc->sc_mlsize;
		goto raw;
	}

	/*
	 * Preset, pack acknowledge will be done in hpstart
	 * during first read operation.
	 */
	if (msg = readdisklabel(dev, hpstrategy, lp)) {
		if (cold)
			printf(": %s", msg);
		else
			log(LOG_ERR, "hp%d: %s\n", unit, msg);
#ifdef COMPAT_42
		if (hpmaptype(mi, lp) == 0)
#endif
		goto raw;
	}

	/*
	 * Seconds per word = (60 / rpm) / (nsectors * secsize/2)
	 */
	if (mi->mi_dk >= 0 && lp->d_rpm)
		dk_wpms[mi->mi_dk] =
		    (lp->d_rpm * lp->d_nsectors * lp->d_secsize) / 120;
	/*
	 * Read bad sector table into memory.
	 */
	bp = geteblk(DEV_BSIZE);		/* max sector size */
	bp->b_dev = dev;
	sc->sc_state = RDBADTBL;
	i = 0;
	do {
		bp->b_flags = B_BUSY | B_READ;
		bp->b_blkno = lp->d_secperunit - lp->d_nsectors + i;
		bp->b_bcount = lp->d_secsize;
		bp->b_cylin = lp->d_ncylinders - 1;
		hpstrategy(bp);
		(void) biowait(bp);
	} while ((bp->b_flags & B_ERROR) && (i += 2) < 10 &&
	    i < lp->d_nsectors);
	db = (struct dkbad *)(bp->b_un.b_addr);
	if ((bp->b_flags & B_ERROR) == 0 && db->bt_mbz == 0 &&
	    db->bt_flag == 0) {
		hpbad[unit] = *db;
	} else
		log(LOG_ERR, "hp%d: %s bad-sector file\n", unit,
		    (bp->b_flags & B_ERROR) ? "can't read" : "format error in");
	sc->sc_state = OPEN;
	bp->b_flags = B_INVAL | B_AGE;
	brelse(bp);
	wakeup((caddr_t)sc);
	return (error);

raw:
	if (cold)
		sc->sc_state = CLOSED;
	else {
		sc->sc_state = OPENRAW;
		wakeup((caddr_t)sc);
	}
	return (error);
}

hpstrategy(bp)
	register struct buf *bp;
{
	register struct mba_device *mi;
	register struct disklabel *lp;
	register struct hpsoftc *sc;
	register int unit;
	daddr_t sz, maxsz;
	int xunit = hppart(bp->b_dev);
	int s;

	sz = (bp->b_bcount + DEV_BSIZE - 1) >> DEV_BSHIFT;
	unit = hpunit(bp->b_dev);
	if (unit >= NHP) {
		bp->b_error = ENXIO;
		goto bad;
	}
	mi = hpinfo[unit];
	sc = &hpsoftc[unit];
	lp = &hplabel[unit];
	if (mi == 0 || mi->mi_alive == 0) {
		bp->b_error = ENXIO;
		goto bad;
	}
#ifdef COMPAT_42
	if (sc->sc_hdr) {				/* XXX */
		if (bp->b_bcount == 516)
			bp->b_flags |= B_FORMAT;
		sc->sc_hdr = 0;
	}
#endif
	if (sc->sc_state < OPEN)
		goto q;
	if (sc->sc_state != OPEN && (bp->b_flags & (B_READ|B_FORMAT)) == 0) {
		bp->b_error = EROFS;
		goto bad;
	}
	if ((sc->sc_openpart & (1 << xunit)) == 0) {
		bp->b_error = ENODEV;
		goto bad;
	}
	maxsz = lp->d_partitions[xunit].p_size;
	if (bp->b_blkno + lp->d_partitions[xunit].p_offset <= LABELSECTOR &&
#if LABELSECTOR != 0
	    bp->b_blkno + lp->d_partitions[xunit].p_offset + sz > LABELSECTOR &&
#endif
	    (bp->b_flags & B_READ) == 0 && sc->sc_wlabel == 0) {
		bp->b_error = EROFS;
		goto bad;
	}
	if (bp->b_blkno < 0 || bp->b_blkno + sz > maxsz) {
		if (bp->b_blkno == maxsz) {
			bp->b_resid = bp->b_bcount;
			goto done;
		}
		sz = maxsz - bp->b_blkno;
		if (sz <= 0) {
			bp->b_error = EINVAL;
			goto bad;
		}
		bp->b_bcount = sz << DEV_BSHIFT;
	}
	bp->b_cylin = (bp->b_blkno + lp->d_partitions[xunit].p_offset) /
	    lp->d_secpercyl;
q:
	s = spl5();
	disksort(&mi->mi_tab, bp);
	if (mi->mi_tab.b_active == 0)
		mbustart(mi);
	splx(s);
	return;

bad:
	bp->b_flags |= B_ERROR;
done:
	biodone(bp);
	return;
}

hpustart(mi)
	register struct mba_device *mi;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct disklabel *lp;
	struct hpsoftc *sc = &hpsoftc[mi->mi_unit];
	daddr_t bn;
	int sn, tn, dist;

	lp = &hplabel[mi->mi_unit];
	hpaddr->hpcs1 = 0;
	if ((hpaddr->hpcs1&HP_DVA) == 0)
		return (MBU_BUSY);

	switch (sc->sc_recal) {

	case 1:
		(void)HPWAIT(mi, hpaddr);
		hpaddr->hpdc = bp->b_cylin;
		hpaddr->hpcs1 = HP_SEEK|HP_GO;
		sc->sc_recal++;
		return (MBU_STARTED);
	case 2:
		break;
	}
	sc->sc_recal = 0;
	if ((hpaddr->hpds & HPDS_VV) == 0) {
		if (sc->sc_state == OPEN && lp->d_flags & D_REMOVABLE) {
			if (sc->sc_openpart)
				log(LOG_ERR, "hp%d: volume changed\n",
				    mi->mi_unit);
			sc->sc_openpart = 0;
			bp->b_flags |= B_ERROR;
			return (MBU_NEXT);
		}
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		if (mi->mi_mba->mba_drv[0].mbd_as & (1<<mi->mi_drive))
			printf("DCLR attn\n");
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		if (!ML11(mi->mi_type))
			hpaddr->hpof = HPOF_FMT22;
		mbclrattn(mi);
		if (sc->sc_state == WANTOPENRAW) {
			sc->sc_state = OPENRAW;
			return (MBU_NEXT);
		}
		if (sc->sc_state == WANTOPEN)
			sc->sc_state = RDLABEL;
	}
	if (mi->mi_tab.b_active || mi->mi_hd->mh_ndrive == 1) {
		if (mi->mi_tab.b_errcnt >= 16 && (bp->b_flags & B_READ)) {
			hpaddr->hpof =
			    hp_offset[mi->mi_tab.b_errcnt & 017]|HPOF_FMT22;
			hpaddr->hpcs1 = HP_OFFSET|HP_GO;
			(void)HPWAIT(mi, hpaddr);
			mbclrattn(mi);
		}
		return (MBU_DODATA);
	}
	if (ML11(mi->mi_type))
		return (MBU_DODATA);
	if ((hpaddr->hpds & HPDS_DREADY) != HPDS_DREADY)
		return (MBU_DODATA);
	bn = bp->b_blkno;
	sn = bn % lp->d_secpercyl;
	tn = sn / lp->d_nsectors;
	sn = sn % lp->d_nsectors;
	if (bp->b_cylin == MASKREG(hpaddr->hpdc)) {
		if (sc->sc_doseeks)
			return (MBU_DODATA);
		dist = sn - (MASKREG(hpaddr->hpla) >> 6) - 1;
		if (dist < 0)
			dist += lp->d_nsectors;
		if (dist <= lp->d_maxdist && dist >= lp->d_mindist)
			return (MBU_DODATA);
	} else
		hpaddr->hpdc = bp->b_cylin;
	if (sc->sc_doseeks)
		hpaddr->hpcs1 = HP_SEEK|HP_GO;
	else {
		sn = (sn + lp->d_nsectors - lp->d_sdist) % lp->d_nsectors;
		hpaddr->hpda = (tn << 8) + sn;
		hpaddr->hpcs1 = HP_SEARCH|HP_GO;
	}
	return (MBU_STARTED);
}

hpstart(mi)
	register struct mba_device *mi;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct disklabel *lp = &hplabel[mi->mi_unit];
	struct hpsoftc *sc = &hpsoftc[mi->mi_unit];
	daddr_t bn;
	int sn, tn, cn;

	if (ML11(mi->mi_type))
		hpaddr->hpda = bp->b_blkno + sc->sc_blkdone;
	else {
		if (bp->b_flags & B_BAD) {
			bn = sc->sc_badbn;
			cn = bn / lp->d_secpercyl;
		} else {
			bn = bp->b_blkno;
			cn = bp->b_cylin;
		}
		sn = bn % lp->d_secpercyl;
		if ((bp->b_flags & B_BAD) == 0)
			sn += sc->sc_blkdone;
		tn = sn / lp->d_nsectors;
		sn %= lp->d_nsectors;
		cn += tn / lp->d_ntracks;
		tn %= lp->d_ntracks;
		hpaddr->hpda = (tn << 8) + sn;
		hpaddr->hpdc = cn;
	}
	mi->mi_tab.b_bdone = dbtob(sc->sc_blkdone);
	if (bp->b_flags & B_FORMAT) {
		if (bp->b_flags & B_READ)
			return (HP_RHDR|HP_GO);
		else
			return (HP_WHDR|HP_GO);
	}
	return (0);
}

hpdtint(mi, mbsr)
	register struct mba_device *mi;
	int mbsr;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register int er1, er2;
	struct hpsoftc *sc = &hpsoftc[mi->mi_unit];
	int retry = 0;
	int npf, bcr;

	bcr = MASKREG(-mi->mi_mba->mba_bcr);
	if (bp->b_flags & B_FORMAT) {
		sc->sc_status = mbsr;
		sc->sc_hpds = hpaddr->hpds;
		sc->sc_er1 = hpaddr->hper1;
		sc->sc_er2 = hpaddr->hper2;
	}
	if (hpaddr->hpds&HPDS_ERR || mbsr&MBSR_EBITS) {
		er1 = hpaddr->hper1;
		er2 = hpaddr->hper2;
		if (bp->b_flags & B_BAD)
			npf = bp->b_error;
		else {
			npf = btodb(bp->b_bcount + (DEV_BSIZE - 1) - bcr);
			if (er1 & (HPER1_DCK | HPER1_ECH))
				npf--;
		}
		if (HPWAIT(mi, hpaddr) == 0)
			goto hard;
#ifdef HPDEBUG
		if (hpdebug) {
			int dc = hpaddr->hpdc, da = hpaddr->hpda;
			daddr_t bn;

			if (bp->b_flags & B_BAD)
				bn = sc->sc_badbn;
			else
				bn = bp->b_blkno + npf;
			log(LOG_DEBUG,
		    "hperr: bp %x cyl %d blk %d blkdone %d as %o dc %x da %x\n",
				bp, bp->b_cylin, bn, sc->sc_blkdone,
				hpaddr->hpas&0xff, MASKREG(dc), MASKREG(da));
			log(LOG_DEBUG,
				"errcnt %d mbsr=%b er1=%b er2=%b bcr -%d\n",
				mi->mi_tab.b_errcnt, mbsr, mbsr_bits,
				MASKREG(er1), HPER1_BITS,
				MASKREG(er2), HPER2_BITS, bcr);
		}
#endif
		if (er1 & HPER1_HCRC) {
			er1 &= ~(HPER1_HCE|HPER1_FER);
			er2 &= ~HPER2_BSE;
		}
		if (er1 & HPER1_WLE) {
			log(LOG_WARNING, "hp%d: write locked\n",
			    hpunit(bp->b_dev));
			bp->b_flags |= B_ERROR;
		} else if (bp->b_flags & B_FORMAT) {
			bp->b_flags |= B_ERROR;
		} else if (RM80(mi->mi_type) && er2&HPER2_SSE) {
			(void) hpecc(mi, SSE);
			return (MBD_RESTARTED);
		} else if ((er2 & HPER2_BSE) && !ML11(mi->mi_type)) {
			if (hpecc(mi, BSE))
				return (MBD_RESTARTED);
			goto hard;
		} else if (MASKREG(er1) == HPER1_FER && RP06(mi->mi_type)) {
			if (hpecc(mi, BSE))
				return (MBD_RESTARTED);
			goto hard;
		} else if ((er1 & (HPER1_DCK | HPER1_ECH)) == HPER1_DCK &&
		    mi->mi_tab.b_errcnt >= 3) {
			if (hpecc(mi, ECC))
				return (MBD_RESTARTED);
			/*
			 * ECC corrected.  Only log retries below
			 * if we got errors other than soft ECC
			 * (as indicated by additional retries).
			 */
			if (mi->mi_tab.b_errcnt == 3)
				mi->mi_tab.b_errcnt = 0;
		} else if ((er1 & HPER1_HCRC) && !ML11(mi->mi_type) &&
		    hpecc(mi, BSE)) {
 			/*
 			 * HCRC means the header is screwed up and the sector
 			 * might well exist in the bad sector table, 
			 * better check....
 			 */
			return (MBD_RESTARTED);
		} else if (++mi->mi_tab.b_errcnt > 27 ||
		    (ML11(mi->mi_type) && mi->mi_tab.b_errcnt > 15) ||
		    mbsr & MBSR_HARD ||
		    er1 & HPER1_HARD ||
		    (!ML11(mi->mi_type) && (er2 & HPER2_HARD))) {
hard:
			diskerr(bp, "hp", "hard error", LOG_PRINTF, npf,
			    &hplabel[mi->mi_unit]);
			if (bp->b_flags & B_BAD)
				printf(" (on replacement sector %d)",
				    sc->sc_badbn);
			if (mbsr & (MBSR_EBITS &~ (MBSR_DTABT|MBSR_MBEXC)))
				printf(" mbsr=%b", mbsr, mbsr_bits);
			printf(" er1=%b er2=%b ds=%b\n",
			    MASKREG(hpaddr->hper1), HPER1_BITS,
			    MASKREG(hpaddr->hper2), HPER2_BITS,
			    MASKREG(hpaddr->hpds), HPDS_BITS);
			bp->b_flags |= B_ERROR;
			bp->b_flags &= ~B_BAD;
		} else
			retry = 1;
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		if (retry && (mi->mi_tab.b_errcnt & 07) == 4) {
			hpaddr->hpcs1 = HP_RECAL|HP_GO;
			sc->sc_recal = 1;
			return (MBD_REPOSITION);
		}
	}
#ifdef HPDEBUG
	else
		if (hpdebug && sc->sc_recal) {
			log(LOG_DEBUG,
			    "recal %d errcnt %d mbsr=%b er1=%b er2=%b\n",
			    sc->sc_recal, mi->mi_tab.b_errcnt, mbsr, mbsr_bits,
			    hpaddr->hper1, HPER1_BITS,
			    hpaddr->hper2, HPER2_BITS);
		}
#endif
	(void)HPWAIT(mi, hpaddr);
	if (retry)
		return (MBD_RETRY);
	if (mi->mi_tab.b_errcnt >= 16) {
		/*
		 * This is fast and occurs rarely; we don't
		 * bother with interrupts.
		 */
		hpaddr->hpcs1 = HP_RTC|HP_GO;
		(void)HPWAIT(mi, hpaddr);
		mbclrattn(mi);
	}
	if (mi->mi_tab.b_errcnt && (bp->b_flags & B_ERROR) == 0) {
		diskerr(bp, "hp", "retries", LOG_INFO, sc->sc_blkdone,
		    &hplabel[mi->mi_unit]);
		addlog(": %d retries\n", mi->mi_tab.b_errcnt);
	}
	if ((bp->b_flags & B_BAD) && hpecc(mi, CONT))
		return (MBD_RESTARTED);
	sc->sc_blkdone = 0;
	bp->b_resid = bcr;
	if (!ML11(mi->mi_type)) {
		hpaddr->hpof = HPOF_FMT22;
		hpaddr->hpcs1 = HP_RELEASE|HP_GO;
	}
	if (sc->sc_openpart == 0)
		wakeup((caddr_t)sc);
	return (MBD_DONE);
}

/*
 * Wait (for a bit) for a drive to come ready;
 * returns nonzero on success.
 */
hpwait(mi)
	register struct mba_device *mi;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register i = 100000;

	while ((hpaddr->hpds & HPDS_DRY) == 0 && --i)
		DELAY(10);
	if (i == 0)
		printf("hp%d: intr, not ready\n", mi->mi_unit);
	return (i);
}

hpioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	int unit = hpunit(dev);
	register struct disklabel *lp;
	register struct hpsoftc *sc = &hpsoftc[unit];
	int error = 0;
	int hpformat();

	lp = &hplabel[unit];

	switch (cmd) {

	case DIOCGDINFO:
		*(struct disklabel *)data = *lp;
		break;

	case DIOCGPART:
		((struct partinfo *)data)->disklab = lp;
		((struct partinfo *)data)->part =
		    &lp->d_partitions[hppart(dev)];
		break;

	case DIOCSDINFO:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else
			error = setdisklabel(lp, (struct disklabel *)data,
			    (sc->sc_state == OPENRAW) ? 0 : sc->sc_openpart);
		if (error == 0)
			sc->sc_state = OPEN;
		break;

	case DIOCWLABEL:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else
			sc->sc_wlabel = *(int *)data;
		break;

	case DIOCWDINFO:
		if ((flag & FWRITE) == 0)
			error = EBADF;
		else if ((error = setdisklabel(lp, (struct disklabel *)data,
		    (sc->sc_state == OPENRAW) ? 0 : sc->sc_openpart)) == 0) {
			int wlab;

			sc->sc_state = OPEN;
			/* simulate opening partition 0 so write succeeds */
			sc->sc_openpart |= (1 << 0);		/* XXX */
			wlab = sc->sc_wlabel;
			sc->sc_wlabel = 1;
			error = writedisklabel(dev, hpstrategy, lp);
			sc->sc_openpart = sc->sc_copenpart | sc->sc_bopenpart;
			sc->sc_wlabel = wlab;
		}
		break;

	case DIOCSBAD:
	    {
		struct dkbad *db = (struct dkbad *)data;

		if ((flag & FWRITE) == 0)
			error = EBADF;
		else if (db->bt_mbz != 0 || db->bt_flag != 0)
			error = EINVAL;
		else
			hpbad[unit] = *db;
		break;
	    }

	case DIOCRFORMAT:
	case DIOCWFORMAT:
	    {
		register struct format_op *fop;
		struct uio auio;
		struct iovec aiov;

		if (cmd == DIOCWFORMAT && (flag & FWRITE) == 0) {
			error = EBADF;
			break;
		}
		fop = (struct format_op *)data;
		aiov.iov_base = fop->df_buf;
		aiov.iov_len = fop->df_count;
		auio.uio_iov = &aiov;
		auio.uio_iovcnt = 1;
		auio.uio_resid = fop->df_count;
		auio.uio_segflg = UIO_USERSPACE;
		auio.uio_offset = fop->df_startblk * lp->d_secsize;
		/*
		 * Don't return errors, as the format op won't get copied
		 * out if we return nonzero.  Callers must check the returned
		 * count.
		 */
		(void) physio(hpformat, (struct buf *)NULL, dev,
		    (cmd == DIOCWFORMAT ? B_WRITE : B_READ), minphys, &auio);
		fop->df_count -= auio.uio_resid;
		fop->df_reg[0] = sc->sc_status;
		fop->df_reg[1] = sc->sc_hpds;
		fop->df_reg[2] = sc->sc_er1;
		fop->df_reg[3] = sc->sc_er2;
		break;
	    }

	default:
		error = ENOTTY;
		break;
	}
	return (error);
}

hpformat(bp)
	struct buf *bp;
{
	bp->b_flags |= B_FORMAT;
	hpstrategy(bp);
}

hpecc(mi, flag)
	register struct mba_device *mi;
	int flag;
{
	register struct mba_regs *mbp = mi->mi_mba;
	register struct hpdevice *rp = (struct hpdevice *)mi->mi_drv;
	register struct buf *bp = mi->mi_tab.b_actf;
	register struct disklabel *lp = &hplabel[mi->mi_unit];
	struct hpsoftc *sc = &hpsoftc[mi->mi_unit];
	int npf, o;
	int bn, cn, tn, sn;
	int bcr;

	bcr = MASKREG(-mbp->mba_bcr);
	if (bp->b_flags & B_BAD)
		npf = bp->b_error;
	else {
		npf = bp->b_bcount - bcr;
		/*
		 * Watch out for fractional sector at end of transfer;
		 * want to round up if finished, otherwise round down.
		 */
		if (bcr == 0)
			npf += 511;
		npf = btodb(npf);
	}
	o = (int)bp->b_un.b_addr & PGOFSET;
	bn = bp->b_blkno;
	cn = bp->b_cylin;
	sn = bn % lp->d_secpercyl + npf;
	tn = sn / lp->d_nsectors;
	sn %= lp->d_nsectors;
	cn += tn / lp->d_ntracks;
	tn %= lp->d_ntracks;
	bn += npf;
	switch (flag) {
	case ECC: {
		register int i;
		caddr_t addr;
		struct pte mpte;
		int bit, byte, mask;

		npf--;		/* because block in error is previous block */
		diskerr(bp, "hp", "soft ecc", LOG_WARNING, npf, lp);
		if (bp->b_flags & B_BAD)
			addlog(" (on replacement sector %d)", sc->sc_badbn);
		addlog("\n");
		mask = MASKREG(rp->hpec2);
		i = MASKREG(rp->hpec1) - 1;		/* -1 makes 0 origin */
		bit = i&07;
		i = (i&~07)>>3;
		byte = i + o;
		while (i < 512 && (int)dbtob(npf)+i < bp->b_bcount && bit > -11) {
			mpte = mbp->mba_map[npf+btop(byte)];
			addr = ptob(mpte.pg_pfnum) + (byte & PGOFSET);
			putmemc(addr, getmemc(addr)^(mask<<bit));
			byte++;
			i++;
			bit -= 8;
		}
		if (bcr == 0)
			return (0);
		npf++;
		break;
		}

	case SSE:
		rp->hpof |= HPOF_SSEI;
		if (bp->b_flags & B_BAD) {
			bn = sc->sc_badbn;
			goto fixregs;
		}
		mbp->mba_bcr = -(bp->b_bcount - (int)ptob(npf));
		break;

	case BSE:
		if (sc->sc_state == OPENRAW)
			return (0);
 		if (rp->hpof & HPOF_SSEI)
 			sn++;
#ifdef HPBDEBUG
		if (hpbdebug)
		log(LOG_DEBUG, "hpecc, BSE: bn %d cn %d tn %d sn %d\n", bn, cn, tn, sn);
#endif
		if (bp->b_flags & B_BAD)
			return (0);
		if ((bn = isbad(&hpbad[mi->mi_unit], cn, tn, sn)) < 0)
			return (0);
		bp->b_flags |= B_BAD;
		bp->b_error = npf + 1;
 		rp->hpof &= ~HPOF_SSEI;
		bn = lp->d_ncylinders * lp->d_secpercyl -
		    lp->d_nsectors - 1 - bn;
		sc->sc_badbn = bn;
	fixregs:
		cn = bn / lp->d_secpercyl;
		sn = bn % lp->d_secpercyl;
		tn = sn / lp->d_nsectors;
		sn %= lp->d_nsectors;
		bcr = bp->b_bcount - (int)ptob(npf);
		bcr = MIN(bcr, 512);
		mbp->mba_bcr = -bcr;
#ifdef HPBDEBUG
		if (hpbdebug)
		log(LOG_DEBUG, "revector to cn %d tn %d sn %d\n", cn, tn, sn);
#endif
		break;

	case CONT:
#ifdef HPBDEBUG
		if (hpbdebug)
		log(LOG_DEBUG, "hpecc, CONT: bn %d cn %d tn %d sn %d\n", bn,cn,tn,sn);
#endif
		bp->b_flags &= ~B_BAD;
		if ((int)ptob(npf) >= bp->b_bcount)
			return (0);
		mbp->mba_bcr = -(bp->b_bcount - (int)ptob(npf));
		break;
	}
	rp->hpcs1 = HP_DCLR|HP_GO;
	if (rp->hpof & HPOF_SSEI)
		sn++;
	rp->hpdc = cn;
	rp->hpda = (tn<<8) + sn;
	mbp->mba_sr = -1;
	mbp->mba_var = (int)ptob(npf) + o;
	rp->hpcs1 = bp->b_flags&B_READ ? HP_RCOM|HP_GO : HP_WCOM|HP_GO;
	mi->mi_tab.b_errcnt = 0;	/* error has been corrected */
	sc->sc_blkdone = npf;
	return (1);
}

#define	DBSIZE	20

hpdump(dev)
	dev_t dev;
{
	register struct mba_device *mi;
	register struct mba_regs *mba;
	register struct disklabel *lp;
	struct hpdevice *hpaddr;
	char *start;
	int num, unit;
	extern int dumpsize;

	num = dumpsize;
	start = 0;
	unit = hpunit(dev);
	if (unit >= NHP)
		return (ENXIO);
#define	phys(a,b)	((b)((int)(a)&0x7fffffff))
	mi = phys(hpinfo[unit],struct mba_device *);
	if (mi == 0 || mi->mi_alive == 0)
		return (ENXIO);
	lp = &hplabel[unit];
	mba = phys(mi->mi_hd, struct mba_hd *)->mh_physmba;
	mba->mba_cr = MBCR_INIT;
	hpaddr = (struct hpdevice *)&mba->mba_drv[mi->mi_drive];
	if ((hpaddr->hpds & HPDS_VV) == 0) {
		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		hpaddr->hpcs1 = HP_PRESET|HP_GO;
		hpaddr->hpof = HPOF_FMT22;
	}
	if (dumplo + num >= lp->d_partitions[hppart(dev)].p_size)
		num = lp->d_partitions[hppart(dev)].p_size - dumplo;
	while (num > 0) {
		register struct pte *hpte = mba->mba_map;
		register int i;
		int blk, cn, sn, tn;
		daddr_t bn;

		blk = num > DBSIZE ? DBSIZE : num;
		bn = dumplo + btodb(start);
		cn = (bn + lp->d_partitions[hppart(dev)].p_offset) /
		    lp->d_secpercyl;
		sn = bn % lp->d_secpercyl;
		tn = sn / lp->d_nsectors;
		sn = sn % lp->d_nsectors;
		hpaddr->hpdc = cn;
		hpaddr->hpda = (tn << 8) + sn;
		for (i = 0; i < blk; i++)
			*(int *)hpte++ = (btop(start)+i) | PG_V;
		mba->mba_sr = -1;
		mba->mba_bcr = -(blk*NBPG);
		mba->mba_var = 0;
		hpaddr->hpcs1 = HP_WCOM | HP_GO;
		while ((hpaddr->hpds & HPDS_DRY) == 0)
			DELAY(10);
		if (hpaddr->hpds&HPDS_ERR)
			return (EIO);
		start += blk*NBPG;
		num -= blk;
	}
	return (0);
}

hpsize(dev)
	dev_t dev;
{
	register int unit = hpunit(dev);
	struct mba_device *mi;

	if (unit >= NHP || (mi = hpinfo[unit]) == 0 || mi->mi_alive == 0 ||
	    hpsoftc[unit].sc_state != OPEN)
		return (-1);
	return ((int)hplabel[unit].d_partitions[hppart(dev)].p_size);
}

#ifdef COMPAT_42
/*
 * Compatibility code to fake up pack label
 * for unlabeled disks.
 */
struct	size {
	daddr_t	nblocks;
	int	cyloff;
} rp06_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 37 */
	33440,	38,		/* B=cyl 38 thru 117 */
	340670,	0,		/* C=cyl 0 thru 814 */
	15884,	118,		/* D=cyl 118 thru 155 */
	55936,	156,		/* E=cyl 156 thru 289 */
	219384,	290,		/* F=cyl 290 thru 814 */
	291192,	118,		/* G=cyl 118 thru 814 */
	0,	0,
}, rp05_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 37 */
	33440,	38,		/* B=cyl 38 thru 117 */
	171798,	0,		/* C=cyl 0 thru 410 */
	15884,	118,		/* D=cyl 118 thru 155 */
	55936,	156,		/* E=cyl 156 thru 289 */
	50512,	290,		/* F=cyl 290 thru 410 */
	122408,	118,		/* G=cyl 118 thru 410 */
	0,	0,
}, rm03_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 99 */
	33440,	100,		/* B=cyl 100 thru 308 */
	131680,	0,		/* C=cyl 0 thru 822 */
	15884,	309,		/* D=cyl 309 thru 408 */
	55936,	409,		/* E=cyl 409 thru 758 */
	10144,	759,		/* F=cyl 759 thru 822 */
	82144,	309,		/* G=cyl 309 thru 822 */
	0,	0,
}, rm05_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 26 */
	33440,	27,		/* B=cyl 27 thru 81 */
	500384,	0,		/* C=cyl 0 thru 822 */
	15884,	562,		/* D=cyl 562 thru 588 */
	55936,	589,		/* E=cyl 589 thru 680 */
	86240,	681,		/* F=cyl 681 thru 822 */
	158592,	562,		/* G=cyl 562 thru 822 */
	291346,	82,		/* H=cyl 82 thru 561 */
}, rm80_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 36 */
	33440,	37,		/* B=cyl 37 thru 114 */
	242606,	0,		/* C=cyl 0 thru 558 */
	15884,	115,		/* D=cyl 115 thru 151 */
	55936,	152,		/* E=cyl 152 thru 280 */
	120559,	281,		/* F=cyl 281 thru 558 */
	192603,	115,		/* G=cyl 115 thru 558 */
	0,	0,
}, rp07_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 9 */
	66880,	10,		/* B=cyl 10 thru 51 */
	1008000, 0,		/* C=cyl 0 thru 629 */
	15884,	235,		/* D=cyl 235 thru 244 */
	307200,	245,		/* E=cyl 245 thru 436 */
	308650,	437,		/* F=cyl 437 thru 629 */
	631850,	235,		/* G=cyl 235 thru 629 */
	291346,	52,		/* H=cyl 52 thru 234 */
}, cdc9775_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 12 */
	66880,	13,		/* B=cyl 13 thru 65 */
	1077760, 0,		/* C=cyl 0 thru 841 */
	15884,	294,		/* D=cyl 294 thru 306 */
	307200,	307,		/* E=cyl 307 thru 546 */
	377440,	547,		/* F=cyl 547 thru 841 */
	701280,	294,		/* G=cyl 294 thru 841 */
	291346,	66,		/* H=cyl 66 thru 293 */
}, cdc9730_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 49 */
	33440,	50,		/* B=cyl 50 thru 154 */
	263360,	0,		/* C=cyl 0 thru 822 */
	15884,	155,		/* D=cyl 155 thru 204 */
	55936,	205,		/* E=cyl 205 thru 379 */
	141664,	380,		/* F=cyl 380 thru 822 */
	213664,	155,		/* G=cyl 155 thru 822 */
	0,	0,
}, capricorn_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 31 */
	33440,	32,		/* B=cyl 32 thru 97 */
	524288,	0,		/* C=cyl 0 thru 1023 */
	15884,	668,		/* D=cyl 668 thru 699 */
	55936,	700,		/* E=cyl 700 thru 809 */
	109472,	810,		/* F=cyl 810 thru 1023 */
	182176,	668,		/* G=cyl 668 thru 1023 */
	291346,	98,		/* H=cyl 98 thru 667 */
}, eagle_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 16 */
	66880,	17,		/* B=cyl 17 thru 86 */
	808320,	0,		/* C=cyl 0 thru 841 */
	15884,	391,		/* D=cyl 391 thru 407 */
	307200,	408,		/* E=cyl 408 thru 727 */
	109296,	728,		/* F=cyl 728 thru 841 */
	432816,	391,		/* G=cyl 391 thru 841 */
	291346,	87,		/* H=cyl 87 thru 390 */
}, ampex_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 26 */
	33440,	27,		/* B=cyl 27 thru 81 */
	495520,	0,		/* C=cyl 0 thru 814 */
	15884,	562,		/* D=cyl 562 thru 588 */
	55936,	589,		/* E=cyl 589 thru 680 */
	81312,	681,		/* F=cyl 681 thru 814 */
	153664,	562,		/* G=cyl 562 thru 814 */
	291346,	82,		/* H=cyl 82 thru 561 */
}, fj2361_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 12 */
	66880,	13,		/* B=cyl 13 thru 65 */
	1077760, 0,		/* C=cyl 0 thru 841 */
	15884,	294,		/* D=cyl 294 thru 306 */
	307200,	307,		/* E=cyl 307 thru 546 */
	377408,	547,		/* F=cyl 547 thru 841 */
	701248,	294,		/* G=cyl 294 thru 841 */
	291346,	66,		/* H=cyl 66 thru 293 */
}, fj2361a_sizes[8] = {
	15884,	0,		/* A=cyl 0 thru 11 */
	66880,	12,		/* B=cyl 12 thru 61 */
	1145120, 0,		/* C=cyl 0 thru 841 */
	15884,	277,		/* D=cyl 277 thru 288 */
	307200,	289,		/* E=cyl 289 thru 514 */
	444516,	515,		/* F=cyl 515 thru 841 */
	768196,	277,		/* G=cyl 277 thru 841 */
	291346,	62,		/* H=cyl 62 thru 276 */
};

/*
 * These variable are all measured in sectors.  
 * Sdist is how much to "lead" in the search for a desired sector
 * (i.e. if want N, search for N-sdist.)
 * Maxdist and mindist define the region right before our desired sector within
 * which we don't bother searching.  We don't search when we are already less
 * then maxdist and more than mindist sectors "before" our desired sector.
 * Maxdist should be >= sdist.
 * 
 * Beware, sdist, mindist and maxdist are not well tuned
 * for many of the drives listed in this table.
 * Try patching things with something i/o intensive
 * running and watch iostat.
 *
 * The order of these entries must agree with the indices in hptypes[].
 */
struct hpst {
	short	nsect;		/* # sectors/track */
	short	ntrak;		/* # tracks/cylinder */
	short	nspc;		/* # sector/cylinders */
	short	ncyl;		/* # cylinders */
	struct	size *sizes;	/* partition tables */
	short	sdist;		/* seek distance metric */
	short	maxdist;	/* boundaries of non-searched area */
	short	mindist;	/* preceding the target sector */
	char	*name;		/* name of disk type */
} hpst[] = {
    { 32, 5,	32*5,	823,	rm03_sizes,	7, 4, 1, "RM03" },
    { 32, 19,	32*19,	823,	rm05_sizes,	7, 4, 1, "RM05" },
    { 22,19,	22*19,	815,	rp06_sizes,	7, 4, 1, "RP06"},
    { 31, 14, 	31*14,	559,	rm80_sizes,	7, 4, 1, "RM80"},
    { 22, 19,	22*19,	411,	rp05_sizes,	7, 4, 1, "RP04"},
    { 22, 19,	22*19,	411,	rp05_sizes,	7, 4, 1, "RP05"},
    { 50, 32,	50*32,	630,	rp07_sizes,    15, 8, 3, "RP07"},
    { 1, 1,	1,	1,	0,		0, 0, 0, "ML11A"},
    { 1, 1,	1,	1,	0,		0, 0, 0, "ML11B" },
    { 32, 40,	32*40,	843,	cdc9775_sizes,	7, 4, 1, "9775" },
    { 32, 10,	32*10,	823,	cdc9730_sizes,	7, 4, 1, "9730-160" },
    { 32, 16,	32*16,	1024,	capricorn_sizes,10,4, 3, "capricorn" },
    { 48, 20,	48*20,	842,	eagle_sizes,   15, 8, 3, "eagle" },
    { 32, 19,	32*19,	815,	ampex_sizes,	7, 4, 1, "9300" },
    { 64, 20,	64*20,	842,	fj2361_sizes,  15, 8, 3, "2361" },
    { 68, 20,	68*20,	842,	fj2361a_sizes, 15, 8, 3, "2361a" },
};

/*
 * Map apparent MASSBUS drive type into manufacturer
 * specific configuration.  For SI controllers this is done
 * based on codes in the serial number register.  For
 * EMULEX controllers, the track and sector attributes are
 * used when the drive type is an RM02 (not supported by DEC).
 */
hpmaptype(mi, lp)
	register struct mba_device *mi;
	register struct disklabel *lp;
{
	register struct hpdevice *hpaddr = (struct hpdevice *)mi->mi_drv;
	register int type = mi->mi_type;
	register struct hpst *st;
	register i;

	/*
	 * Model-byte processing for SI controllers.
	 * NB:  Only deals with RM03 and RM05 emulations.
	 */
	if (type == HPDT_RM03 || type == HPDT_RM05) {
		int hpsn = hpaddr->hpsn;

		if ((hpsn & SIMB_LU) == mi->mi_drive)
		switch ((hpsn & SIMB_MB) & ~(SIMB_S6|SIRM03|SIRM05)) {

		case SI9775D:
			type = HPDT_9775;
			break;

		case SI9730D:
			type = HPDT_9730;
			break;

		case SI9766:
			type = HPDT_RM05;
			break;

		case SI9762:
			type = HPDT_RM03;
			break;

		case SICAPD:
			type = HPDT_CAPRICORN;
			break;

		case SI9751D:
			type = HPDT_EAGLE;
			break;
		}
		mi->mi_type = type;
	}

	/*
	 * EMULEX SC750 or SC780.  Poke the holding register.
	 */
	if (type == HPDT_RM02) {
		int nsectors, ntracks, ncyl;

		hpaddr->hpof = HPOF_FMT22;
		mbclrattn(mi);
		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXTRAK;
		ntracks = MASKREG(hpaddr->hphr) + 1;
		DELAY(100);
		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXSECT;
		nsectors = MASKREG(hpaddr->hphr) + 1;
		DELAY(100);
		hpaddr->hpcs1 = HP_NOP;
		hpaddr->hphr = HPHR_MAXCYL;
		ncyl = MASKREG(hpaddr->hphr) + 1;
		for (type = 0; hptypes[type] != 0; type++)
			if (hpst[type].nsect == nsectors &&
			    hpst[type].ntrak == ntracks &&
			    hpst[type].ncyl == ncyl)
				break;

		hpaddr->hpcs1 = HP_DCLR|HP_GO;
		mbclrattn(mi);		/* conservative */
		if (hptypes[type] == 0) {
	printf("hp%d: %d sectors, %d tracks, %d cylinders: unknown device\n",
				mi->mi_unit, nsectors, ntracks, ncyl);
			lp->d_nsectors = nsectors;
			lp->d_ntracks = ntracks;
			lp->d_ncylinders = ncyl;
			lp->d_secpercyl = nsectors*ntracks;
			lp->d_secperunit = lp->d_secpercyl * lp->d_ncylinders;
#ifdef notdef		/* set elsewhere */
			lp->d_npartitions = 1;
			lp->d_partitions[0].p_offset = 0;
#endif
			lp->d_partitions[0].p_size = lp->d_secperunit;
			return (0);
		}
		mi->mi_type = type;
	}

	/*
	 * set up minimal disk label.
	 */
	st = &hpst[type];
	lp->d_nsectors = st->nsect;
	lp->d_ntracks = st->ntrak;
	lp->d_secpercyl = st->nspc;
	lp->d_ncylinders = st->ncyl;
	lp->d_secperunit = st->nspc * st->ncyl;
	lp->d_sdist = st->sdist;
	lp->d_mindist = st->mindist;
	lp->d_maxdist = st->maxdist;
	bcopy(hpst[type].name, lp->d_typename, sizeof(lp->d_typename));
	lp->d_npartitions = 8;
	for (i = 0; i < 8; i++) {
		lp->d_partitions[i].p_offset = st->sizes[i].cyloff *
		    lp->d_secpercyl;
		lp->d_partitions[i].p_size = st->sizes[i].nblocks;
	}
	return (1);
}
#endif COMPAT_42
#endif
