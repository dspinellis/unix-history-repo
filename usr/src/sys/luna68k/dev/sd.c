/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sd.c	7.3 (Berkeley) %G%
 */

/*
 * sd.c -- SCSI Disk Device Driver
 * remaked by A.Fujita, MAR-22-1992
 */

/*
 * SCSI CCS (Command Command Set) disk driver.
 */
#include "sd.h"
#if NSD > 0

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/dkstat.h"
#include "sys/proc.h"
#include "sys/disklabel.h"

#include "device.h"
#include "scsireg.h"
#include "scsivar.h"

int	sdinit(), sdstrategy(), sdstart(), sdintr();

struct	driver sddriver = {
	sdinit, "sd", sdstart, (int (*)()) 0, sdintr, (int (*)()) 0
};

struct	disklabel sdlabel[NSD];

struct	sd_softc {
	struct	hp_device *sc_hd;
	struct	scsi_queue sc_dq;
	short	sc_flags;
	short	sc_type;	/* drive type */
	short	sc_punit;	/* physical unit (scsi lun) */
	u_int	sc_blks;	/* number of blocks on device */
	int	sc_blksize;	/* device block size in bytes */
	u_int	sc_wpms;	/* average xfer rate in 16 bit wds/sec. */
} sd_softc[NSD];

/* sc_flags values */
#define	SDF_ALIVE	0x1

struct	buf sdtab[NSD];
struct	scsi_fmt_sense sdsense[NSD];

static struct scsi_fmt_cdb sd_read_cmd = { 10, CMD_READ_EXT };
static struct scsi_fmt_cdb sd_write_cmd = { 10, CMD_WRITE_EXT };

#define	sdunit(x)	((minor(x) >> 3) & 0x7)
#define sdpart(x)	(minor(x) & 0x7)
#define	sdpunit(x)	((x) & 7)
#define sdminor(unit, part)	(((unit) << 3) | (part))

#define	b_lba		b_resid

#define	SDRETRY		3	/* IO retry count */

struct sd_iostat {
	int imax;
	int imin;
	int omax;
	int omin;
};

struct sd_iostat sd_iostat[NSD] = {
	{ 14000, -1, 100, -1 },
};

/*
 * Initialize
 */

int
sdinit(hd)
	register struct hp_device *hd;
{
	register struct sd_softc *sc = &sd_softc[hd->hp_unit];
	register struct disklabel *lp;
	char *msg, *sdreadlabel();

	sc->sc_hd = hd;
	sc->sc_punit = sdpunit(hd->hp_flags);
	sc->sc_type = sdident(sc, hd);
	if (sc->sc_type < 0)
		return(0);
	sc->sc_dq.dq_ctlr = hd->hp_ctlr;
	sc->sc_dq.dq_unit = hd->hp_unit;
	sc->sc_dq.dq_slave = hd->hp_slave;
	sc->sc_dq.dq_driver = &sddriver;

	/*
	 * Use the default sizes until we've read the label,
	 * or longer if there isn't one there.
	 */
	lp = &sdlabel[hd->hp_unit];

	if (lp->d_secpercyl == 0) {
		lp->d_secsize = DEV_BSIZE;
		lp->d_nsectors = 32;
		lp->d_ntracks = 20;
		lp->d_secpercyl = 32*20;
		lp->d_npartitions = 1;
		lp->d_partitions[0].p_offset = 0;
		lp->d_partitions[0].p_size = LABELSECTOR + 1;
	}

	/*
	 * read disklabel
	 */
	if (msg = sdreadlabel(makedev(4, (hd->hp_unit<<3)), sdstrategy, lp)) {
		if (msg != NULL) {
			printf("sd%d: %s\n", hd->hp_unit, msg);
			return(0);
		}
	}

	sc->sc_flags = SDF_ALIVE;
	return(1);
}

static struct scsi_inquiry inqbuf;
static struct scsi_fmt_cdb inq = {
	6,
	CMD_INQUIRY, 0, 0, 0, sizeof(inqbuf), 0
};

static u_long capbuf[2];
struct scsi_fmt_cdb cap = {
	10,
	CMD_READ_CAPACITY, 0, 0, 0, 0, 0, 0, 0, 0, 0
};

int
sdident(sc, hd)
	struct sd_softc *sc;
	struct hp_device *hd;
{
	char idstr[32];
	int unit;
	register int ctlr, slave;
	register int i;
	register int tries = 10;

	ctlr = hd->hp_ctlr;
	slave = hd->hp_slave;
	unit = sc->sc_punit;

	/*
	 * See if unit exists and is a disk then read block size & nblocks.
	 */
	while ((i = scsi_test_unit_rdy(ctlr, slave, unit)) != 0) {
		if (i < 0 || --tries < 0)
			return (-1);
		if (i == STS_CHECKCOND) {
			u_char sensebuf[8];
			struct scsi_xsense *sp = (struct scsi_xsense *)sensebuf;

			scsi_request_sense(ctlr, slave, unit, sensebuf, 8);
			if (sp->class == 7 && sp->key == 6)
				/* drive doing an RTZ -- give it a while */
				DELAY(1000000);
		}
		DELAY(1000);
	}
	if (scsi_immed_command(ctlr, slave, unit, &inq, (u_char *)&inqbuf,
			       sizeof(inqbuf)) ||
	    scsi_immed_command(ctlr, slave, unit, &cap, (u_char *)&capbuf,
			       sizeof(capbuf)))
		/* doesn't exist or not a CCS device */
		return (-1);

	switch (inqbuf.type) {
	case 0:		/* disk */
	case 4:		/* WORM */
	case 5:		/* CD-ROM */
	case 7:		/* Magneto-optical */
		break;
	default:	/* not a disk */
		return (-1);
	}
	sc->sc_blks    = capbuf[0];
	sc->sc_blksize = capbuf[1];

	bcopy((caddr_t)&inqbuf.vendor_id, (caddr_t)idstr, 28);
	for (i = 27; i > 23; --i)
		if (idstr[i] != ' ')
			break;
	idstr[i+1] = 0;
	for (i = 23; i > 7; --i)
		if (idstr[i] != ' ')
			break;
	idstr[i+1] = 0;
	for (i = 7; i >= 0; --i)
		if (idstr[i] != ' ')
			break;
	idstr[i+1] = 0;
	printf("sd%d: %s %s rev %s", hd->hp_unit, idstr, &idstr[8],
	       &idstr[24]);

	printf(", %d %d byte blocks\n", sc->sc_blks, sc->sc_blksize);
	if (sc->sc_blksize != DEV_BSIZE) {
		printf("sd%d: need %d byte blocks - drive ignored\n", unit, DEV_BSIZE);
		return(1);
	}

	sc->sc_wpms = 32 * (60 * DEV_BSIZE / 2);	/* XXX */
	return(inqbuf.type);
}


/*
 * Open
 */

int
sdopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	register int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];

	if (unit >= NSD)
		return(ENXIO);
	if ((sc->sc_flags & SDF_ALIVE) == 0 && suser(p->p_ucred, &p->p_acflag))
		return(ENXIO);

	if (sc->sc_hd->hp_dk >= 0)
		dk_wpms[sc->sc_hd->hp_dk] = sc->sc_wpms;
	return(0);
}


/*
 * Strategy
 */

int
sdstrategy(bp)
	register struct buf *bp;
{
	register int unit = sdunit(bp->b_dev);
	register int part = sdpart(bp->b_dev);
	register struct sd_softc *sc = &sd_softc[unit];
	register struct disklabel *lp = &sdlabel[unit];
	register struct partition *pp = &(lp->d_partitions[part]);
	register struct buf *dp = &sdtab[unit];
	register daddr_t bn;
	register int sz, s;

#ifdef DEBUG
	printf("sdstrategy: bp->b_blkno = %d, bp->bcount = %d\n",
	       bp->b_blkno, bp->b_bcount);
#endif
	bn = bp->b_blkno;
	sz = howmany(bp->b_bcount, DEV_BSIZE);

	/* check that tracsfer is within a drive's partition */

	if (bn < 0 || (bn + sz) > pp->p_size) {
		sz = pp->p_size - bn;
		if (sz == 0) {
			bp->b_resid = bp->b_bcount;
			goto done;
		}
		if (sz < 0) {
			bp->b_error = EINVAL;
			bp->b_flags |= B_ERROR;
			goto done;
		}
		bp->b_bcount = dbtob(sz);
	}

	/* calculate LBA for transfer */

	bp->b_lba = bn + pp->p_offset;

	/* raise priority to block sdintr */

	s = splbio();

	/* call disksort to sort request into drive queue */

	disksort(dp, bp);

#ifdef DEBUG
	printf("sdstrategy: dp->b_active = %d\n", dp->b_active);
#endif
	if (dp->b_active == 0) {			/*  */
		dp->b_active = 1;
		sdustart(unit);
	}

	/* lower priority */

	splx(s);

	return;

done:
	biodone(bp);
}

int
sdustart(unit)
	register int unit;
{
	register struct sd_softc *sc = &sd_softc[unit];
	register struct hp_device *hp = sc->sc_hd;
	register struct scsi_queue *dq = &sc->sc_dq;
	register struct buf *bp = sdtab[unit].b_actf;
	register struct scsi_fmt_cdb *cmd;

	cmd = bp->b_flags & B_READ? &sd_read_cmd : &sd_write_cmd;
	*(int *)(&cmd->cdb[2]) = bp->b_lba;
	*(u_short *)(&cmd->cdb[7]) = howmany(bp->b_bcount, DEV_BSIZE);

	dq->dq_cdb   = cmd;
	dq->dq_bp    = bp;
	dq->dq_flags = DQ_DISCONNECT;	/* SCSI Disconnect */

	if (screq(dq))
		sdstart(unit);
}

int
sdstart(unit)
	register int unit;
{
	register struct sd_softc *sc = &sd_softc[unit];
	register struct hp_device *hp = sc->sc_hd;

	if (hp->hp_dk >= 0) {
		dk_busy |= 1 << hp->hp_dk;
	}

	scstart(hp->hp_ctlr);
}


/*
 * Return:
 *	0	if not really an error
 *	<0	if we should do a retry
 *	>0	if a fatal error
 */
static int
sderror(unit, sc, hp, stat)
	int unit, stat;
	register struct sd_softc *sc;
	register struct hp_device *hp;
{
	int cond = 1;

	sdsense[unit].status = stat;
	if (stat & STS_CHECKCOND) {
		struct scsi_xsense *sp;

		scsi_request_sense(hp->hp_ctlr, hp->hp_slave,
				   sc->sc_punit, sdsense[unit].sense,
				   sizeof(sdsense[unit].sense));
		sp = (struct scsi_xsense *)sdsense[unit].sense;
		printf("sd%d: scsi sense class %d, code %d", unit,
			sp->class, sp->code);
		if (sp->class == 7) {
			printf(", key %d", sp->key);
			if (sp->valid)
				printf(", blk %d", *(int *)&sp->info1);
			switch (sp->key) {
			/* no sense, try again */
			case 0:
				cond = -1;
				break;
			/* recovered error, not a problem */
			case 1:
				cond = 0;
				break;
			}
		}
		printf("\n");
	}
	return(cond);
}

/*
 * Interrupt
 */

int
sdintr(unit, stat)
	register int unit;
	int stat;
{
	register struct sd_softc *sc = &sd_softc[unit];
	register struct hp_device *hp = sc->sc_hd;
	register struct scsi_queue *dq = &sc->sc_dq;
	register struct buf *bp = dq->dq_bp;
	int cond;

#ifdef DEBUG
	printf("sdintr(unit = %d, stat = %d)\n", unit, stat);
#endif

	if (stat == SC_IO_TIMEOUT) {
		printf("sdintr: sd%d timeout error\n", unit, stat);
	}

	if (hp->hp_dk >= 0) {
		dk_busy &=~ (1 << hp->hp_dk);
		if (stat == 0) {
			++dk_seek[hp->hp_dk];
			++dk_xfer[hp->hp_dk];
			dk_wds[hp->hp_dk] += bp->b_bcount >> 6;
		}
	}

	if (bp->b_flags & B_READ) {
		sd_iostat[unit].imin = min(dq->dq_imin, sd_iostat[unit].imin);
		if (dq->dq_imax > sd_iostat[unit].imax) {
			sd_iostat[unit].imax = dq->dq_imax;
#ifdef SD_IOSTAT
			printf("sdintr: sd%d  INPUT	MAX = %d, MIN = %d\n",
			       unit, sd_iostat[unit].imax, sd_iostat[unit].imin);
#endif
		}
	} else {
		sd_iostat[unit].omin = min(dq->dq_omin, sd_iostat[unit].omin);
		if (dq->dq_omax > sd_iostat[unit].omax) {
			sd_iostat[unit].omax = dq->dq_omax;
#ifdef SD_IOSTAT
			printf("sdintr: sd%d  OUTPUT	MAX = %d, MIN = %d\n",
			       unit, sd_iostat[unit].omax, sd_iostat[unit].omin);
#endif
		}
	}

	if (stat != 0) {
		if (stat > 0) {
#ifdef DEBUGPRINT
			dbgprintall();
			printf("\n");
#endif
			cond = sderror(unit, sc, hp, stat);
			if (cond) {
				if (cond < 0 && sdtab[unit].b_errcnt++ < SDRETRY) {
					sdstart(unit);
					return;
				}
			}
		} else {
			if (sdtab[unit].b_errcnt++ < SDRETRY) {
				printf("sdintr: sd%d restart IO request\n", unit);
				sdstart(unit);
				return;
			}
		}
		bp->b_flags |= B_ERROR;
		bp->b_error = EIO;
	}

	sdtab[unit].b_errcnt = 0;
	sdtab[unit].b_actf = bp->b_actf;

	bp->b_resid = 0;

	biodone(bp);

	scfree(dq);

	if (sdtab[unit].b_actf) {
		sdustart(unit);
	} else {
		sdtab[unit].b_active = 0;
	}
}


/*
 * RAW Device Routines
 */

int
sdread(dev, uio, flags)
	dev_t dev;
	struct uio *uio;
	int flags;
{
	register int unit = sdunit(dev);

	return (physio(sdstrategy, NULL, dev, B_READ, minphys, uio));
}

int
sdwrite(dev, uio, flags)
	dev_t dev;
	struct uio *uio;
	int flags;
{
	register int unit = sdunit(dev);

	return (physio(sdstrategy, NULL, dev, B_WRITE, minphys, uio));
}

int
sdioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];
	register struct disklabel *lp = &sdlabel[unit];
	int error = 0;

	switch (cmd) {

	case DIOCGDINFO:
		*(struct disklabel *)data = *lp;
		break;

	case DIOCGPART:
		((struct partinfo *)data)->disklab = lp;
		((struct partinfo *)data)->part =
		    &lp->d_partitions[sdpart(dev)];
		break;

	default:
		error = ENOTTY;
		break;
	}
	return (error);
}


/*
 * Size
 */

int
sdsize(dev)
	dev_t dev;
{
	register int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];

	if (unit >= NSD || (sc->sc_flags & SDF_ALIVE) == 0)
		return(-1);

	return(sdlabel[unit].d_partitions[sdpart(dev)].p_size);
}


/*
 * Dump
 */

int
sddump(dev)
	dev_t dev;
{
}

/*
 * Disk Subs
 */

/*
 * Attempt to read a disk label from a device
 * using the indicated stategy routine.
 * The label must be partly set up before this:
 * secpercyl and anything required in the strategy routine
 * (e.g., sector size) must be filled in before calling us.
 * Returns null on success and an error string on failure.
 */
char *
sdreadlabel(dev, strat, lp)
	dev_t dev;
	int (*strat)();
	register struct disklabel *lp;
{
	register struct buf *bp;
	struct disklabel *dlp;
	char *msg = NULL;

	if (lp->d_secperunit == 0)
		lp->d_secperunit = 0x1fffffff;
	lp->d_npartitions = 1;
	if (lp->d_partitions[0].p_size == 0)
		lp->d_partitions[0].p_size = 0x1fffffff;
	lp->d_partitions[0].p_offset = 0;

	bp = geteblk((int)lp->d_secsize);
	bp->b_dev = dev;
	bp->b_blkno = LABELSECTOR;
	bp->b_bcount = lp->d_secsize;
	bp->b_flags = B_BUSY | B_READ;
	(*strat)(bp);
	if (biowait(bp)) {
		msg = "I/O error";
	} else {
		for (dlp = (struct disklabel *)bp->b_un.b_addr;
		     dlp <= (struct disklabel *)(bp->b_un.b_addr+DEV_BSIZE-sizeof(*dlp));
		     dlp = (struct disklabel *)((char *)dlp + sizeof(long))) {
			if (dlp->d_magic != DISKMAGIC || dlp->d_magic2 != DISKMAGIC) {
				if (msg == NULL)
					msg = "no disk label";
			} else if (dlp->d_npartitions > MAXPARTITIONS ||
				   dkcksum(dlp) != 0)
				msg = "disk label corrupted";
			else {
				*lp = *dlp;
				msg = NULL;
				break;
			}
		}
	}
	bp->b_flags = B_INVAL | B_AGE;
	brelse(bp);
	return (msg);
}

#ifdef notyet

/*
 * Checksum routine for OMRON native disklabel
 */

#define	OMRON_LBLSIZE	512

u_short
omcksum(omp)
	register char *omp;
{
	register u_short *start, *end;
	register u_short sum = 0;

	start = (u_short *) omp;
	end = (u_short *) &start[(OMRON_LBLSIZE/sizeof(u_short) - 1)];
	while (start < end)
		sum ^= *start++;

	printf("omcksum: saved  ... 0x%s\n", hexstr(*end, 4));
	printf("omcksum: calced ... 0x%s\n", hexstr(sum, 4));

	return (sum);
}

/*
 * Write disk label back to device after modification.
 */
sdwritelabel(dev, strat, lp)
	dev_t dev;
	int (*strat)();
	register struct disklabel *lp;
{
	struct buf *bp;
	struct disklabel *dlp;
	int labelpart;
	int error = 0;

	labelpart = sdpart(dev);
	if (lp->d_partitions[labelpart].p_offset != 0) {
		if (lp->d_partitions[0].p_offset != 0)
			return (EXDEV);			/* not quite right */
		labelpart = 0;
	}

	bp = geteblk((int)lp->d_secsize);
	bp->b_dev = makedev(major(dev), sdminor(sdunit(dev), labelpart));
	bp->b_blkno = LABELSECTOR;
	bp->b_bcount = lp->d_secsize;
	bp->b_flags = B_READ;
	(*strat)(bp);
	if (error = biowait(bp))
		goto done;

	for (dlp = (struct disklabel *)bp->b_un.b_addr;
	    dlp <= (struct disklabel *)
	      (bp->b_un.b_addr + lp->d_secsize - sizeof(*dlp));
	    dlp = (struct disklabel *)((char *)dlp + sizeof(long))) {
		if (dlp->d_magic == DISKMAGIC && dlp->d_magic2 == DISKMAGIC &&
		    dkcksum(dlp) == 0) {
			omcksum(bp->b_un.b_addr);
/*
			*dlp = *lp;
			bp->b_flags = B_WRITE;
			(*strat)(bp);
			error = biowait(bp);
			goto done;
 */
		}
	}
	error = ESRCH;
done:
	brelse(bp);
	return (error);
}
#endif

#endif
