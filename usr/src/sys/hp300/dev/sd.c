/*
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)sd.c	8.3 (Berkeley) %G%
 */

/*
 * SCSI CCS (Command Command Set) disk driver.
 */
#include "sd.h"
#if NSD > 0

#ifndef lint
static char rcsid[] = "$Header: /sys.lite/hp300/dev/RCS/sd.c,v 1.2 1994/01/10 18:29:19 mike Exp mike $";
#endif

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/stat.h>
#include <sys/dkstat.h>
#include <sys/disklabel.h>
#include <sys/malloc.h>
#include <sys/proc.h>
#include <sys/ioctl.h>
#include <sys/fcntl.h>

#include <hp/dev/device.h>
#include <hp300/dev/scsireg.h>
#include <hp300/dev/sdvar.h>
#ifdef USELEDS
#include <hp300/hp300/led.h>
#endif

#include <vm/vm_param.h>
#include <vm/lock.h>
#include <vm/vm_prot.h>
#include <vm/pmap.h>

extern int scsi_test_unit_rdy();
extern int scsi_request_sense();
extern int scsi_inquiry();
extern int scsi_read_capacity();
extern int scsi_tt_write();
extern int scsireq();
extern int scsiustart();
extern int scsigo();
extern void scsifree();
extern void scsireset();
extern void scsi_delay();

extern void disksort();
extern void biodone();
extern int physio();
extern void TBIS();

int	sdinit();
void	sdstrategy(), sdstart(), sdustart(), sdgo(), sdintr();

struct	driver sddriver = {
	sdinit, "sd", (int (*)())sdstart, (int (*)())sdgo, (int (*)())sdintr,
};

#ifdef DEBUG
int sddebug = 1;
#define SDB_ERROR	0x01
#define SDB_PARTIAL	0x02
#define SDB_CAPACITY	0x04
#endif

struct	sd_softc sd_softc[NSD];
struct	sdstats sdstats[NSD];
struct	buf sdtab[NSD];
struct	scsi_fmt_cdb sdcmd[NSD];
struct	scsi_fmt_sense sdsense[NSD];

static struct scsi_fmt_cdb sd_read_cmd = { 10, CMD_READ_EXT };
static struct scsi_fmt_cdb sd_write_cmd = { 10, CMD_WRITE_EXT };

/*
 * Table of scsi commands users are allowed to access via "format"
 * mode.  0 means not legal.  1 means "immediate" (doesn't need dma).
 * -1 means needs dma and/or wait for intr.
 */
static char legal_cmds[256] = {
/*****  0   1   2   3   4   5   6   7     8   9   A   B   C   D   E   F */
/*00*/	0,  0,  0,  0, -1,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*10*/	0,  0,  1,  0,  0,  1,  0,  0,    0,  0,  1,  0,  0,  0,  0,  0,
/*20*/	0,  0,  0,  0,  0,  1,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*30*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*40*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*50*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*60*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*70*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*80*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*90*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*a0*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*b0*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*c0*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*d0*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*e0*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
/*f0*/	0,  0,  0,  0,  0,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
};

static struct scsi_inquiry inqbuf;
static struct scsi_fmt_cdb inq = {
	6,
	CMD_INQUIRY, 0, 0, 0, sizeof(inqbuf), 0
};

static int
sdident(sc, hd)
	struct sd_softc *sc;
	struct hp_device *hd;
{
	int unit;
	register int ctlr, slave;
	register int i;
	register int tries = 10;
	char idstr[32];
	int isrm = 0;

	ctlr = hd->hp_ctlr;
	slave = hd->hp_slave;
	unit = sc->sc_punit;
	scsi_delay(-1);

	/*
	 * See if unit exists and is a disk then read block size & nblocks.
	 */
	while ((i = scsi_test_unit_rdy(ctlr, slave, unit)) != 0) {
		if (i == -1 || --tries < 0) {
			if (isrm)
				break;
			/* doesn't exist or not a CCS device */
			goto failed;
		}
		if (i == STS_CHECKCOND) {
			u_char sensebuf[128];
			struct scsi_xsense *sp = (struct scsi_xsense *)sensebuf;

			scsi_request_sense(ctlr, slave, unit, sensebuf,
					   sizeof(sensebuf));
			if (sp->class == 7)
				switch (sp->key) {
				/*
				 * Not ready -- might be removable media
				 * device with no media.  Assume as much,
				 * if it really isn't, the inquiry commmand
				 * below will fail.
				 */
				case 2:
					isrm = 1;
					break;
				/* drive doing an RTZ -- give it a while */
				case 6:
					DELAY(1000000);
					break;
				default:
					break;
				}
		}
		DELAY(1000);
	}
	/*
	 * Find out about device
	 */
	if (scsi_immed_command(ctlr, slave, unit, &inq,
			       (u_char *)&inqbuf, sizeof(inqbuf), B_READ))
		goto failed;
	switch (inqbuf.type) {
	case 0:		/* disk */
	case 4:		/* WORM */
	case 5:		/* CD-ROM */
	case 7:		/* Magneto-optical */
		break;
	default:	/* not a disk */
		goto failed;
	}
	/*
	 * Get a usable id string
	 */
	switch (inqbuf.version) {
	case 1:
	case 2:
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
		break;
	default:
		bcopy("UNKNOWN", &idstr[0], 8);
		bcopy("DRIVE TYPE", &idstr[8], 11);
	}
	if (inqbuf.qual & 0x80)
		sc->sc_flags |= SDF_RMEDIA;

	if (sdgetcapacity(sc, hd, NODEV) < 0)
		goto failed;

	switch (inqbuf.version) {
	case 1:
	case 2:
		printf("sd%d: %s %s rev %s", hd->hp_unit, idstr, &idstr[8],
			&idstr[24]);
		if (inqbuf.version == 2)
			printf(" (SCSI-2)");
		break;
	default:
		printf("sd%d: type 0x%x, qual 0x%x, ver %d", hd->hp_unit,
		       inqbuf.type, inqbuf.qual, inqbuf.version);
		break;
	}
	if (sc->sc_blks)
		printf(", %d %d byte blocks",
		       sc->sc_blks >> sc->sc_bshift, sc->sc_blksize);
	printf("\n");
	sc->sc_wpms = 32 * (60 * DEV_BSIZE / 2);	/* XXX */
	scsi_delay(0);
	return(inqbuf.type);
failed:
	scsi_delay(0);
	return(-1);
}

int
sdinit(hd)
	register struct hp_device *hd;
{
	register struct sd_softc *sc = &sd_softc[hd->hp_unit];

	sc->sc_hd = hd;
	sc->sc_flags = 0;
	/*
	 * XXX formerly 0 meant unused but now pid 0 can legitimately
	 * use this interface (sdgetcapacity).
	 */
	sc->sc_format_pid = -1;
	sc->sc_punit = sdpunit(hd->hp_flags);
	sc->sc_type = sdident(sc, hd);
	if (sc->sc_type < 0)
		return(0);
	sc->sc_dq.dq_ctlr = hd->hp_ctlr;
	sc->sc_dq.dq_unit = hd->hp_unit;
	sc->sc_dq.dq_slave = hd->hp_slave;
	sc->sc_dq.dq_driver = &sddriver;

	sc->sc_flags |= SDF_ALIVE;
	return(1);
}

void
sdreset(sc, hd)
	register struct sd_softc *sc;
	register struct hp_device *hd;
{
	sdstats[hd->hp_unit].sdresets++;
}

/*
 * Determine capacity of a drive.
 * Returns -1 on a failure, 0 on success, 1 on a failure that is probably
 * due to missing media.
 */
int
sdgetcapacity(sc, hd, dev)
	struct sd_softc *sc;
	struct hp_device *hd;
	dev_t dev;
{
	static struct scsi_fmt_cdb cap = {
		10,
		CMD_READ_CAPACITY, 0, 0, 0, 0, 0, 0, 0, 0, 0
	};
	u_char *capbuf;
	int i, capbufsize;

	/*
	 * Cannot use stack space for this buffer since stack KVA may not
	 * be valid (i.e. in context of this process) when the operation
	 * actually starts.
	 */
	capbufsize = 8;
	capbuf = malloc(capbufsize, M_DEVBUF, M_WAITOK);

	if (dev == NODEV) {
		i = scsi_immed_command(hd->hp_ctlr, hd->hp_slave, sc->sc_punit,
				       &cap, capbuf, capbufsize, B_READ);
	} else {
		struct buf *bp;

		/*
		 * XXX this is horrible
		 */
		if (sc->sc_format_pid >= 0)
			panic("sdgetcapacity");
		bp = malloc(sizeof *bp, M_DEVBUF, M_WAITOK);
		sc->sc_format_pid = curproc->p_pid;
		bcopy((caddr_t)&cap, (caddr_t)&sdcmd[hd->hp_unit], sizeof cap);
		bp->b_dev = dev;
		bp->b_flags = B_READ | B_BUSY;
		bp->b_un.b_addr = (caddr_t)capbuf;
		bp->b_bcount = capbufsize;
		sdstrategy(bp);
		i = biowait(bp) ? sdsense[hd->hp_unit].status : 0;
		free(bp, M_DEVBUF);
		sc->sc_format_pid = -1;
	}
	if (i) {
		if (i != STS_CHECKCOND || (sc->sc_flags & SDF_RMEDIA) == 0) {
#ifdef DEBUG
			if (sddebug & SDB_CAPACITY)
				printf("sd%d: read_capacity returns %d\n",
				       hd->hp_unit, i);
#endif
			free(capbuf, M_DEVBUF);
			return (-1);
		}
		/*
		 * XXX assume unformatted or non-existant media
		 */
		sc->sc_blks = 0;
		sc->sc_blksize = DEV_BSIZE;
		sc->sc_bshift = 0;
#ifdef DEBUG
		if (sddebug & SDB_CAPACITY)
			printf("sd%d: removable media not present\n",
			       hd->hp_unit);
#endif
		free(capbuf, M_DEVBUF);
		return (1);
	}
	sc->sc_blks = *(u_int *)&capbuf[0];
	sc->sc_blksize = *(int *)&capbuf[4];
	free(capbuf, M_DEVBUF);
	sc->sc_bshift = 0;

	/* return value of read capacity is last valid block number */
	sc->sc_blks++;

	if (sc->sc_blksize != DEV_BSIZE) {
		if (sc->sc_blksize < DEV_BSIZE) {
			printf("sd%d: need at least %d byte blocks - %s\n",
				hd->hp_unit, DEV_BSIZE, "drive ignored");
			return (-1);
		}
		for (i = sc->sc_blksize; i > DEV_BSIZE; i >>= 1)
			++sc->sc_bshift;
		sc->sc_blks <<= sc->sc_bshift;
	}
#ifdef DEBUG
	if (sddebug & SDB_CAPACITY)
		printf("sd%d: blks=%d, blksize=%d, bshift=%d\n", hd->hp_unit,
		       sc->sc_blks, sc->sc_blksize, sc->sc_bshift);
#endif
	return (0);
}

/*
 * Read or constuct a disklabel
 */
int
sdgetinfo(dev)
	dev_t dev;
{
	int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];
	register struct disklabel *lp = &sc->sc_info.si_label;
	register struct partition *pi;
	char *msg, *readdisklabel();
#ifdef COMPAT_NOLABEL
	int usedefault = 1;

	/*
	 * For CD-ROM just define a single partition
	 */
	if (sc->sc_type == 5)
		usedefault = 0;
#endif

	bzero((caddr_t)lp, sizeof *lp);
	msg = NULL;

	/*
	 * If removable media or the size unavailable at boot time
	 * (i.e. unformatted hard disk), attempt to set the capacity
	 * now.
	 */
	if ((sc->sc_flags & SDF_RMEDIA) || sc->sc_blks == 0) {
		switch (sdgetcapacity(sc, sc->sc_hd, dev)) {
		case 0:
			break;
		case -1:
			/*
			 * Hard error, just return (open will fail).
			 */
			return (EIO);
		case 1:
			/*
			 * XXX return 0 so open can continue just in case
			 * the media is unformatted and we want to format it.
			 * We set the error flag so they cannot do much else.
			 */
			sc->sc_flags |= SDF_ERROR;
			msg = "unformatted/missing media";
#ifdef COMPAT_NOLABEL
			usedefault = 0;
#endif
			break;
		}
	}

	/*
	 * Set some default values to use while reading the label
	 * (or to use if there isn't a label) and try reading it.
	 */
	if (msg == NULL) {
		lp->d_type = DTYPE_SCSI;
		lp->d_secsize = DEV_BSIZE;
		lp->d_nsectors = 32;
		lp->d_ntracks = 20;
		lp->d_ncylinders = 1;
		lp->d_secpercyl = 32*20;
		lp->d_npartitions = 3;
		lp->d_partitions[2].p_offset = 0;
		/* XXX we can open a device even without SDF_ALIVE */
		if (sc->sc_blksize == 0)
			sc->sc_blksize = DEV_BSIZE;
		/* XXX ensure size is at least one device block */
		lp->d_partitions[2].p_size =
			roundup(LABELSECTOR+1, btodb(sc->sc_blksize));
		msg = readdisklabel(sdlabdev(dev), sdstrategy, lp);
		if (msg == NULL)
			return (0);
	}

	pi = lp->d_partitions;
	printf("sd%d: WARNING: %s, ", unit, msg);
#ifdef COMPAT_NOLABEL
	if (usedefault) {
		printf("using old default partitioning\n");
		sdmakedisklabel(unit, lp);
		return(0);
	}
#endif
	printf("defining `c' partition as entire disk\n");
	pi[2].p_size = sc->sc_blks;
	return(0);
}

int
sdopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	register int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];
	int mask, error;

	if (unit >= NSD)
		return(ENXIO);
	if ((sc->sc_flags & SDF_ALIVE) == 0 && suser(p->p_ucred, &p->p_acflag))
		return(ENXIO);
	if (sc->sc_flags & SDF_ERROR)
		return(EIO);

	/*
	 * Wait for any pending opens/closes to complete
	 */
	while (sc->sc_flags & (SDF_OPENING|SDF_CLOSING))
		sleep((caddr_t)sc, PRIBIO);
	/*
	 * On first open, get label and partition info.
	 * We may block reading the label, so be careful
	 * to stop any other opens.
	 */
	if (sc->sc_info.si_open == 0) {
		sc->sc_flags |= SDF_OPENING;
		error = sdgetinfo(dev);
		sc->sc_flags &= ~SDF_OPENING;
		wakeup((caddr_t)sc);
		if (error)
			return(error);
	}
	if (sc->sc_hd->hp_dk >= 0)
		dk_wpms[sc->sc_hd->hp_dk] = sc->sc_wpms;

	mask = 1 << sdpart(dev);
	if (mode == S_IFCHR)
		sc->sc_info.si_copen |= mask;
	else
		sc->sc_info.si_bopen |= mask;
	sc->sc_info.si_open |= mask;
	return(0);
}

int
sdclose(dev, flag, mode, p)
	dev_t dev;
	int flag, mode;
	struct proc *p;
{
	int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];
	register struct sdinfo *si = &sc->sc_info;
	int mask, s;

	mask = 1 << sdpart(dev);
	if (mode == S_IFCHR)
		si->si_copen &= ~mask;
	else
		si->si_bopen &= ~mask;
	si->si_open = si->si_bopen | si->si_copen;
	/*
	 * On last close, we wait for all activity to cease since
	 * the label/parition info will become invalid.  Since we
	 * might sleep, we must block any opens while we are here.
	 * Note we don't have to about other closes since we know
	 * we are the last one.
	 */
	if (si->si_open == 0) {
		sc->sc_flags |= SDF_CLOSING;
		s = splbio();
		while (sdtab[unit].b_active) {
			sc->sc_flags |= SDF_WANTED;
			sleep((caddr_t)&sdtab[unit], PRIBIO);
		}
		splx(s);
		sc->sc_flags &= ~(SDF_CLOSING|SDF_WLABEL|SDF_ERROR);
		wakeup((caddr_t)sc);
	}
	sc->sc_format_pid = -1;
	return(0);
}

/*
 * This routine is called for partial block transfers and non-aligned
 * transfers (the latter only being possible on devices with a block size
 * larger than DEV_BSIZE).  The operation is performed in three steps
 * using a locally allocated buffer:
 *	1. transfer any initial partial block
 *	2. transfer full blocks
 *	3. transfer any final partial block
 */
static void
sdlblkstrat(bp, bsize)
	register struct buf *bp;
	register int bsize;
{
	register struct buf *cbp = (struct buf *)malloc(sizeof(struct buf),
							M_DEVBUF, M_WAITOK);
	caddr_t cbuf = (caddr_t)malloc(bsize, M_DEVBUF, M_WAITOK);
	register int bn, resid;
	register caddr_t addr;

	bzero((caddr_t)cbp, sizeof(*cbp));
	cbp->b_proc = curproc;		/* XXX */
	cbp->b_dev = bp->b_dev;
	bn = bp->b_blkno;
	resid = bp->b_bcount;
	addr = bp->b_un.b_addr;
#ifdef DEBUG
	if (sddebug & SDB_PARTIAL)
		printf("sdlblkstrat: bp %x flags %x bn %x resid %x addr %x\n",
		       bp, bp->b_flags, bn, resid, addr);
#endif

	while (resid > 0) {
		register int boff = dbtob(bn) & (bsize - 1);
		register int count;

		if (boff || resid < bsize) {
			sdstats[sdunit(bp->b_dev)].sdpartials++;
			count = min(resid, bsize - boff);
			cbp->b_flags = B_BUSY | B_PHYS | B_READ;
			cbp->b_blkno = bn - btodb(boff);
			cbp->b_un.b_addr = cbuf;
			cbp->b_bcount = bsize;
#ifdef DEBUG
			if (sddebug & SDB_PARTIAL)
				printf(" readahead: bn %x cnt %x off %x addr %x\n",
				       cbp->b_blkno, count, boff, addr);
#endif
			sdstrategy(cbp);
			biowait(cbp);
			if (cbp->b_flags & B_ERROR) {
				bp->b_flags |= B_ERROR;
				bp->b_error = cbp->b_error;
				break;
			}
			if (bp->b_flags & B_READ) {
				bcopy(&cbuf[boff], addr, count);
				goto done;
			}
			bcopy(addr, &cbuf[boff], count);
#ifdef DEBUG
			if (sddebug & SDB_PARTIAL)
				printf(" writeback: bn %x cnt %x off %x addr %x\n",
				       cbp->b_blkno, count, boff, addr);
#endif
		} else {
			count = resid & ~(bsize - 1);
			cbp->b_blkno = bn;
			cbp->b_un.b_addr = addr;
			cbp->b_bcount = count;
#ifdef DEBUG
			if (sddebug & SDB_PARTIAL)
				printf(" fulltrans: bn %x cnt %x addr %x\n",
				       cbp->b_blkno, count, addr);
#endif
		}
		cbp->b_flags = B_BUSY | B_PHYS | (bp->b_flags & B_READ);
		sdstrategy(cbp);
		biowait(cbp);
		if (cbp->b_flags & B_ERROR) {
			bp->b_flags |= B_ERROR;
			bp->b_error = cbp->b_error;
			break;
		}
done:
		bn += btodb(count);
		resid -= count;
		addr += count;
#ifdef DEBUG
		if (sddebug & SDB_PARTIAL)
			printf(" done: bn %x resid %x addr %x\n",
			       bn, resid, addr);
#endif
	}
	free(cbuf, M_DEVBUF);
	free(cbp, M_DEVBUF);
}

void
sdstrategy(bp)
	register struct buf *bp;
{
	int unit = sdunit(bp->b_dev);
	register struct sd_softc *sc = &sd_softc[unit];
	register struct buf *dp = &sdtab[unit];
	register struct partition *pinfo;
	register daddr_t bn;
	register int sz, s;

	if (sc->sc_format_pid >= 0) {
		if (sc->sc_format_pid != curproc->p_pid) {	/* XXX */
			bp->b_error = EPERM;
			goto bad;
		}
		bp->b_cylin = 0;
	} else {
		if (sc->sc_flags & SDF_ERROR) {
			bp->b_error = EIO;
			goto bad;
		}
		bn = bp->b_blkno;
		sz = howmany(bp->b_bcount, DEV_BSIZE);
		pinfo = &sc->sc_info.si_label.d_partitions[sdpart(bp->b_dev)];
		if (bn < 0 || bn + sz > pinfo->p_size) {
			sz = pinfo->p_size - bn;
			if (sz == 0) {
				bp->b_resid = bp->b_bcount;
				goto done;
			}
			if (sz < 0) {
				bp->b_error = EINVAL;
				goto bad;
			}
			bp->b_bcount = dbtob(sz);
		}
		/*
		 * Check for write to write protected label
		 */
		if (bn + pinfo->p_offset <= LABELSECTOR &&
#if LABELSECTOR != 0
		    bn + pinfo->p_offset + sz > LABELSECTOR &&
#endif
		    !(bp->b_flags & B_READ) && !(sc->sc_flags & SDF_WLABEL)) {
			bp->b_error = EROFS;
			goto bad;
		}
		/*
		 * Non-aligned or partial-block transfers handled specially.
		 */
		s = sc->sc_blksize - 1;
		if ((dbtob(bn) & s) || (bp->b_bcount & s)) {
			sdlblkstrat(bp, sc->sc_blksize);
			goto done;
		}
		bp->b_cylin = (bn + pinfo->p_offset) >> sc->sc_bshift;
	}
	s = splbio();
	disksort(dp, bp);
	if (dp->b_active == 0) {
		dp->b_active = 1;
		sdustart(unit);
	}
	splx(s);
	return;
bad:
	bp->b_flags |= B_ERROR;
done:
	biodone(bp);
}

void
sdustart(unit)
	register int unit;
{
	if (scsireq(&sd_softc[unit].sc_dq))
		sdstart(unit);
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
			/* possible media change */
			case 6:
				/*
				 * For removable media, if we are doing the
				 * first open (i.e. reading the label) go
				 * ahead and retry, otherwise someone has
				 * changed the media out from under us and
				 * we should abort any further operations
				 * until a close is done.
				 */
				if (sc->sc_flags & SDF_RMEDIA) {
					if (sc->sc_flags & SDF_OPENING)
						cond = -1;
					else
						sc->sc_flags |= SDF_ERROR;
				}
				break;
			}
		}
		printf("\n");
	}
	return(cond);
}

static void
sdfinish(unit, sc, bp)
	int unit;
	register struct sd_softc *sc;
	register struct buf *bp;
{
	register struct buf *dp = &sdtab[unit];

	dp->b_errcnt = 0;
	dp->b_actf = bp->b_actf;
	bp->b_resid = 0;
	biodone(bp);
	scsifree(&sc->sc_dq);
	if (dp->b_actf)
		sdustart(unit);
	else {
		dp->b_active = 0;
		if (sc->sc_flags & SDF_WANTED) {
			sc->sc_flags &= ~SDF_WANTED;
			wakeup((caddr_t)dp);
		}
	}
}

void
sdstart(unit)
	register int unit;
{
	register struct sd_softc *sc = &sd_softc[unit];
	register struct hp_device *hp = sc->sc_hd;

	/*
	 * we have the SCSI bus -- in format mode, we may or may not need dma
	 * so check now.
	 */
	if (sc->sc_format_pid >= 0 && legal_cmds[sdcmd[unit].cdb[0]] > 0) {
		register struct buf *bp = sdtab[unit].b_actf;
		register int sts;

		sdtab[unit].b_errcnt = 0;
		while (1) {
			sts = scsi_immed_command(hp->hp_ctlr, hp->hp_slave,
						 sc->sc_punit, &sdcmd[unit],
						 bp->b_un.b_addr, bp->b_bcount,
						 bp->b_flags & B_READ);
			sdsense[unit].status = sts;
			if ((sts & 0xfe) == 0 ||
			    (sts = sderror(unit, sc, hp, sts)) == 0)
				break;
			if (sts > 0 || sdtab[unit].b_errcnt++ >= SDRETRY) {
				bp->b_flags |= B_ERROR;
				bp->b_error = EIO;
				break;
			}
		}
		sdfinish(unit, sc, bp);

	} else if (scsiustart(hp->hp_ctlr))
		sdgo(unit);
}

void
sdgo(unit)
	register int unit;
{
	register struct sd_softc *sc = &sd_softc[unit];
	register struct hp_device *hp = sc->sc_hd;
	register struct buf *bp = sdtab[unit].b_actf;
	register int pad;
	register struct scsi_fmt_cdb *cmd;

	if (sc->sc_format_pid >= 0) {
		cmd = &sdcmd[unit];
		pad = 0;
	} else {
		/*
		 * Drive is in an error state, abort all operations
		 */
		if (sc->sc_flags & SDF_ERROR) {
			bp->b_flags |= B_ERROR;
			bp->b_error = EIO;
			sdfinish(unit, sc, bp);
			return;
		}
		cmd = bp->b_flags & B_READ? &sd_read_cmd : &sd_write_cmd;
		*(int *)(&cmd->cdb[2]) = bp->b_cylin;
		pad = howmany(bp->b_bcount, sc->sc_blksize);
		*(u_short *)(&cmd->cdb[7]) = pad;
		pad = (bp->b_bcount & (sc->sc_blksize - 1)) != 0;
#ifdef DEBUG
		if (pad)
			printf("sd%d: partial block xfer -- %x bytes\n",
			       unit, bp->b_bcount);
#endif
		sdstats[unit].sdtransfers++;
	}
#ifdef USELEDS
	if (inledcontrol == 0)
		ledcontrol(0, 0, LED_DISK);
#endif
	if (scsigo(hp->hp_ctlr, hp->hp_slave, sc->sc_punit, bp, cmd, pad) == 0) {
		if (hp->hp_dk >= 0) {
			dk_busy |= 1 << hp->hp_dk;
			++dk_seek[hp->hp_dk];
			++dk_xfer[hp->hp_dk];
			dk_wds[hp->hp_dk] += bp->b_bcount >> 6;
		}
		return;
	}
#ifdef DEBUG
	if (sddebug & SDB_ERROR)
		printf("sd%d: sdstart: %s adr %d blk %d len %d ecnt %d\n",
		       unit, bp->b_flags & B_READ? "read" : "write",
		       bp->b_un.b_addr, bp->b_cylin, bp->b_bcount,
		       sdtab[unit].b_errcnt);
#endif
	bp->b_flags |= B_ERROR;
	bp->b_error = EIO;
	sdfinish(unit, sc, bp);
}

void
sdintr(unit, stat)
	register int unit;
	int stat;
{
	register struct sd_softc *sc = &sd_softc[unit];
	register struct buf *bp = sdtab[unit].b_actf;
	register struct hp_device *hp = sc->sc_hd;
	int cond;
	
	if (bp == NULL) {
		printf("sd%d: bp == NULL\n", unit);
		return;
	}
	if (hp->hp_dk >= 0)
		dk_busy &=~ (1 << hp->hp_dk);
	if (stat) {
#ifdef DEBUG
		if (sddebug & SDB_ERROR)
			printf("sd%d: sdintr: bad scsi status 0x%x\n",
				unit, stat);
#endif
		cond = sderror(unit, sc, hp, stat);
		if (cond) {
			if (cond < 0 && sdtab[unit].b_errcnt++ < SDRETRY) {
#ifdef DEBUG
				if (sddebug & SDB_ERROR)
					printf("sd%d: retry #%d\n",
					       unit, sdtab[unit].b_errcnt);
#endif
				sdstart(unit);
				return;
			}
			bp->b_flags |= B_ERROR;
			bp->b_error = EIO;
		}
	}
	sdfinish(unit, sc, bp);
}

int
sdread(dev, uio, flags)
	dev_t dev;
	struct uio *uio;
	int flags;
{
	register int unit = sdunit(dev);
	register int pid;

	if ((pid = sd_softc[unit].sc_format_pid) >= 0 &&
	    pid != uio->uio_procp->p_pid)
		return (EPERM);
		
	return (physio(sdstrategy, NULL, dev, B_READ, minphys, uio));
}

int
sdwrite(dev, uio, flags)
	dev_t dev;
	struct uio *uio;
	int flags;
{
	register int unit = sdunit(dev);
	register int pid;

	if ((pid = sd_softc[unit].sc_format_pid) >= 0 &&
	    pid != uio->uio_procp->p_pid)
		return (EPERM);
		
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
	register struct disklabel *lp = &sc->sc_info.si_label;
	int error, flags;

	switch (cmd) {
	default:
		return (EINVAL);

	case DIOCGDINFO:
		*(struct disklabel *)data = *lp;
		return (0);

	case DIOCGPART:
		((struct partinfo *)data)->disklab = lp;
		((struct partinfo *)data)->part =
			&lp->d_partitions[sdpart(dev)];
		return (0);

        case DIOCWLABEL:
                if ((flag & FWRITE) == 0)
                        return (EBADF);
		if (*(int *)data)
			sc->sc_flags |= SDF_WLABEL;
		else
			sc->sc_flags &= ~SDF_WLABEL;
		return (0);

        case DIOCSDINFO:
                if ((flag & FWRITE) == 0)
                        return (EBADF);
		error = setdisklabel(lp, (struct disklabel *)data,
				     (sc->sc_flags & SDF_WLABEL) ? 0
				     : sc->sc_info.si_open);
		return (error);

        case DIOCWDINFO:
		if ((flag & FWRITE) == 0)
			return (EBADF);
		error = setdisklabel(lp, (struct disklabel *)data,
				     (sc->sc_flags & SDF_WLABEL) ? 0
				     : sc->sc_info.si_open);
		if (error)
			return (error);
		flags = sc->sc_flags;
		sc->sc_flags = SDF_ALIVE | SDF_WLABEL;
		error = writedisklabel(sdlabdev(dev), sdstrategy, lp);
		sc->sc_flags = flags;
		return (error);

	case SDIOCSFORMAT:
		/* take this device into or out of "format" mode */
		if (suser(p->p_ucred, &p->p_acflag))
			return(EPERM);

		if (*(int *)data) {
			if (sc->sc_format_pid >= 0)
				return (EPERM);
			sc->sc_format_pid = p->p_pid;
		} else
			sc->sc_format_pid = -1;
		return (0);

	case SDIOCGFORMAT:
		/* find out who has the device in format mode */
		*(int *)data = sc->sc_format_pid;
		return (0);

	case SDIOCSCSICOMMAND:
		/*
		 * Save what user gave us as SCSI cdb to use with next
		 * read or write to the char device.
		 */
		if (sc->sc_format_pid != p->p_pid)
			return (EPERM);
		if (legal_cmds[((struct scsi_fmt_cdb *)data)->cdb[0]] == 0)
			return (EINVAL);
		bcopy(data, (caddr_t)&sdcmd[unit], sizeof(sdcmd[0]));
		return (0);

	case SDIOCSENSE:
		/*
		 * return the SCSI sense data saved after the last
		 * operation that completed with "check condition" status.
		 */
		bcopy((caddr_t)&sdsense[unit], data, sizeof(sdsense[0]));
		return (0);
		
	}
	/*NOTREACHED*/
}

int
sdsize(dev)
	dev_t dev;
{
	register int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];
	int psize, didopen = 0;

	if (unit >= NSD || (sc->sc_flags & SDF_ALIVE) == 0)
		return(-1);

	/*
	 * We get called very early on (via swapconf)
	 * without the device being open so we may need
	 * to handle it here.
	 */
	if (sc->sc_info.si_open == 0) {
		if (sdopen(dev, FREAD|FWRITE, S_IFBLK, NULL))
			return(-1);
		didopen = 1;
	}
	psize = sc->sc_info.si_label.d_partitions[sdpart(dev)].p_size;
	if (didopen)
		(void) sdclose(dev, FREAD|FWRITE, S_IFBLK, NULL);
	return (psize);
}

/*
 * Non-interrupt driven, non-dma dump routine.
 */
int
sddump(dev)
	dev_t dev;
{
	int part = sdpart(dev);
	int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];
	register struct hp_device *hp = sc->sc_hd;
	register struct partition *pinfo;
	register daddr_t baddr;
	register int maddr;
	register int pages, i;
	int stat;
	extern int lowram, dumpsize;

	/* is drive ok? */
	if (unit >= NSD || (sc->sc_flags & SDF_ALIVE) == 0)
		return (ENXIO);
	pinfo = &sc->sc_info.si_label.d_partitions[part];
	/* dump parameters in range? */
	if (dumplo < 0 || dumplo >= pinfo->p_size ||
	    pinfo->p_fstype != FS_SWAP)
		return (EINVAL);
	pages = dumpsize;
	if (dumplo + ctod(pages) > pinfo->p_size)
		pages = dtoc(pinfo->p_size - dumplo);
	maddr = lowram;
	baddr = dumplo + pinfo->p_offset;
	/* scsi bus idle? */
	if (!scsireq(&sc->sc_dq)) {
		scsireset(hp->hp_ctlr);
		sdreset(sc, sc->sc_hd);
		printf("[ drive %d reset ] ", unit);
	}
	for (i = 0; i < pages; i++) {
#define NPGMB	(1024*1024/NBPG)
		/* print out how many Mbs we have dumped */
		if (i && (i % NPGMB) == 0)
			printf("%d ", i / NPGMB);
#undef NPBMG
		pmap_enter(kernel_pmap, (vm_offset_t)vmmap, maddr,
		    VM_PROT_READ, TRUE);
		stat = scsi_tt_write(hp->hp_ctlr, hp->hp_slave, sc->sc_punit,
				     vmmap, NBPG, baddr, sc->sc_bshift);
		if (stat) {
			printf("sddump: scsi write error 0x%x\n", stat);
			return (EIO);
		}
		maddr += NBPG;
		baddr += ctod(1);
	}
	return (0);
}
#endif
