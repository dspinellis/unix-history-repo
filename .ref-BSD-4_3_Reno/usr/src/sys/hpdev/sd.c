/*
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)sd.c	7.2 (Berkeley) 5/15/90
 */

/*
 * SCSI CCS (Command Command Set) disk driver.
 */
#include "sd.h"
#if NSD > 0

#ifndef lint
static char rcsid[] = "$Header: sd.c,v 1.5 90/01/10 16:06:12 mike Locked $";
#endif

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "errno.h"
#include "dkstat.h"
#include "disklabel.h"
#include "device.h"
#include "malloc.h"
#include "scsireg.h"

#include "user.h"
#include "proc.h"
#include "uio.h"

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

extern void printf();
extern void bcopy();
extern void disksort();
extern int splbio();
extern void splx();
extern void biodone();
extern int physio();
extern void TBIS();

int	sdinit();
void	sdstrategy(), sdstart(), sdustart(), sdgo(), sdintr();

struct	driver sddriver = {
	sdinit, "sd", (int (*)())sdstart, (int (*)())sdgo, (int (*)())sdintr,
};

struct	size {
	u_long	strtblk;
	u_long	endblk;
	int	nblocks;
};

struct sdinfo {
	struct	size part[8];
};

/*
 * since the SCSI standard tends to hide the disk structure, we define
 * partitions in terms of DEV_BSIZE blocks.  The default partition table
 * (for an unlabeled disk) reserves 512K for a boot area, has an 8 meg
 * root and 32 meg of swap.  The rest of the space on the drive goes in
 * the G partition.  As usual, the C partition covers the entire disk
 * (including the boot area).
 */
struct sdinfo sddefaultpart = {
	     1024,   17408,   16384   ,	/* A */
	    17408,   82944,   65536   ,	/* B */
	        0,       0,       0   ,	/* C */
	    17408,  115712,   98304   ,	/* D */
	   115712,  218112,  102400   ,	/* E */
	   218112,       0,       0   ,	/* F */
	    82944,       0,       0   ,	/* G */
	   115712,       0,       0   ,	/* H */
};

struct	sd_softc {
	struct	hp_device *sc_hd;
	struct	devqueue sc_dq;
	int	sc_format_pid;	/* process using "format" mode */
	short	sc_flags;
	short	sc_type;	/* drive type */
	short	sc_punit;	/* physical unit (scsi lun) */
	u_short	sc_bshift;	/* convert device blocks to DEV_BSIZE blks */
	u_int	sc_blks;	/* number of blocks on device */
	int	sc_blksize;	/* device block size in bytes */
	u_int	sc_wpms;	/* average xfer rate in 16 bit wds/sec. */
	struct	sdinfo sc_info;	/* drive partition table & label info */
} sd_softc[NSD];

/* sc_flags values */
#define	SDF_ALIVE	0x1

#ifdef DEBUG
int sddebug = 1;
#define SDB_ERROR	0x01
#define SDB_PARTIAL	0x02
#endif

struct sdstats {
	long	sdresets;
	long	sdtransfers;
	long	sdpartials;
} sdstats[NSD];

struct	buf sdtab[NSD];
struct	buf sdbuf[NSD];
struct	scsi_fmt_cdb sdcmd[NSD];
struct	scsi_fmt_sense sdsense[NSD];

static struct scsi_fmt_cdb sd_read_cmd = { 10, CMD_READ_EXT };
static struct scsi_fmt_cdb sd_write_cmd = { 10, CMD_WRITE_EXT };

#define	sdunit(x)	((minor(x) >> 3) & 0x7)
#define sdpart(x)	(minor(x) & 0x7)
#define	sdpunit(x)	((x) & 7)
#define	b_cylin		b_resid
#define	SDRETRY		2

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

static u_char capbuf[8];
struct scsi_fmt_cdb cap = {
	10,
	CMD_READ_CAPACITY, 0, 0, 0, 0, 0, 0, 0, 0, 0
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

	ctlr = hd->hp_ctlr;
	slave = hd->hp_slave;
	unit = sc->sc_punit;

	/*
	 * See if unit exists and is a disk then read block size & nblocks.
	 */
	while ((i = scsi_test_unit_rdy(ctlr, slave, unit)) != 0) {
		if (i == -1 || --tries < 0)
			/* doesn't exist or not a CCS device */
			return (-1);
		if (i == STS_CHECKCOND) {
			u_char sensebuf[128];
			struct scsi_xsense *sp = (struct scsi_xsense *)sensebuf;

			scsi_request_sense(ctlr, slave, unit, sensebuf,
					   sizeof(sensebuf));
			if (sp->class == 7 && sp->key == 6)
				/* drive doing an RTZ -- give it a while */
				DELAY(1000000);
		}
		DELAY(1000);
	}
	if (scsi_immed_command(ctlr, slave, unit, &inq, (u_char *)&inqbuf,
			       sizeof(inqbuf), B_READ) ||
	    scsi_immed_command(ctlr, slave, unit, &cap, (u_char *)&capbuf,
			       sizeof(capbuf), B_READ))
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
	sc->sc_blks = *(u_int *)&capbuf[0];
	sc->sc_blksize = *(int *)&capbuf[4];

	if (inqbuf.version != 1)
		printf("sd%d: type 0x%x, qual 0x%x, ver %d", hd->hp_unit,
			inqbuf.type, inqbuf.qual, inqbuf.version);
	else {
		char idstr[32];

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
	}
	printf(", %d %d byte blocks\n", sc->sc_blks, sc->sc_blksize);
	if (sc->sc_blksize != DEV_BSIZE) {
		if (sc->sc_blksize < DEV_BSIZE) {
			printf("sd%d: need %d byte blocks - drive ignored\n",
				unit, DEV_BSIZE);
			return (-1);
		}
		for (i = sc->sc_blksize; i > DEV_BSIZE; i >>= 1)
			++sc->sc_bshift;
		sc->sc_blks <<= sc->sc_bshift;
	}
	sc->sc_wpms = 32 * (60 * DEV_BSIZE / 2);	/* XXX */
	return(inqbuf.type);
}

int
sdinit(hd)
	register struct hp_device *hd;
{
	register struct sd_softc *sc = &sd_softc[hd->hp_unit];

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
	 * If we don't have a disk label, build a default partition
	 * table with 'standard' size root & swap and everything else
	 * in the G partition.
	 */
	sc->sc_info = sddefaultpart;
	/* C gets everything */
	sc->sc_info.part[2].nblocks = sc->sc_blks;
	sc->sc_info.part[2].endblk = sc->sc_blks;
	/* G gets from end of B to end of disk */
	sc->sc_info.part[6].nblocks = sc->sc_blks - sc->sc_info.part[1].endblk;
	sc->sc_info.part[6].endblk = sc->sc_blks;
	/*
	 * We also define the D, E and F paritions as an alternative to
	 * B and G.  D is 48Mb, starts after A and is intended for swapping.
	 * E is 50Mb, starts after D and is intended for /usr. F starts
	 * after E and is what ever is left.
	 */
	if (sc->sc_blks >= sc->sc_info.part[4].endblk) {
		sc->sc_info.part[5].nblocks =
			sc->sc_blks - sc->sc_info.part[4].endblk;
		sc->sc_info.part[5].endblk = sc->sc_blks;
	} else {
		sc->sc_info.part[5].strtblk = 0;
		sc->sc_info.part[3] = sc->sc_info.part[5];
		sc->sc_info.part[4] = sc->sc_info.part[5];
	}
	/*
	 * H is a single partition alternative to E and F.
	 */
	if (sc->sc_blks >= sc->sc_info.part[3].endblk) {
		sc->sc_info.part[7].nblocks =
			sc->sc_blks - sc->sc_info.part[3].endblk;
		sc->sc_info.part[7].endblk = sc->sc_blks;
	} else {
		sc->sc_info.part[7].strtblk = 0;
	}

	sc->sc_flags = SDF_ALIVE;
	return(1);
}

void
sdreset(sc, hd)
	register struct sd_softc *sc;
	register struct hp_device *hd;
{
	sdstats[hd->hp_unit].sdresets++;
}

int
sdopen(dev, flags)
	dev_t dev;
	int flags;
{
	register int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];

	if (unit >= NSD)
		return(ENXIO);
	if ((sc->sc_flags & SDF_ALIVE) == 0 && suser(u.u_cred, &u.u_acflag))
		return(ENXIO);

	if (sc->sc_hd->hp_dk >= 0)
		dk_wpms[sc->sc_hd->hp_dk] = sc->sc_wpms;
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
	cbp->b_proc = u.u_procp;
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
			count = MIN(resid, bsize - boff);
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
	register int part = sdpart(bp->b_dev);
	register int unit = sdunit(bp->b_dev);
	register int bn, sz;
	register struct sd_softc *sc = &sd_softc[unit];
	register struct buf *dp = &sdtab[unit];
	register int s;

	if (sc->sc_format_pid) {
		if (sc->sc_format_pid != u.u_procp->p_pid) {
			bp->b_error = EPERM;
			goto bad;
		}
		bp->b_cylin = 0;
	} else {
		bn = bp->b_blkno;
		sz = (bp->b_bcount + (DEV_BSIZE - 1)) >> DEV_BSHIFT;
		if (bn < 0 || bn + sz > sc->sc_info.part[part].nblocks) {
			if (bn == sc->sc_info.part[part].nblocks) {
				bp->b_resid = bp->b_bcount;
				goto done;
			}
			bp->b_error = EINVAL;
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
		bp->b_cylin = (bn + sc->sc_info.part[part].strtblk) >>
				sc->sc_bshift;
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
	iodone(bp);
}

void
sdustart(unit)
	register int unit;
{
	if (scsireq(&sd_softc[unit].sc_dq))
		sdstart(unit);
}

static void
sderror(unit, sc, hp, stat)
	int unit, stat;
	register struct sd_softc *sc;
	register struct hp_device *hp;
{
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
		}
		printf("\n");
	}
}

static void
sdfinish(unit, sc, bp)
	int unit;
	register struct sd_softc *sc;
	register struct buf *bp;
{
	sdtab[unit].b_errcnt = 0;
	sdtab[unit].b_actf = bp->b_actf;
	bp->b_resid = 0;
	iodone(bp);
	scsifree(&sc->sc_dq);
	if (sdtab[unit].b_actf)
		sdustart(unit);
	else
		sdtab[unit].b_active = 0;
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
	if (sc->sc_format_pid && legal_cmds[sdcmd[unit].cdb[0]] > 0) {
		register struct buf *bp = sdtab[unit].b_actf;
		register int sts;

		sts = scsi_immed_command(hp->hp_ctlr, hp->hp_slave,
					 sc->sc_punit, &sdcmd[unit],
					 bp->b_un.b_addr, bp->b_bcount,
					 bp->b_flags & B_READ);
		sdsense[unit].status = sts;
		if (sts & 0xfe) {
			sderror(unit, sc, hp, sts);
			bp->b_flags |= B_ERROR;
			bp->b_error = EIO;
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

	if (sc->sc_format_pid) {
		cmd = &sdcmd[unit];
		pad = 0;
	} else {
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
		sderror(unit, sc, hp, stat);
		bp->b_flags |= B_ERROR;
		bp->b_error = EIO;
	}
	sdfinish(unit, sc, bp);
}

int
sdread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = sdunit(dev);
	register int pid;

	if ((pid = sd_softc[unit].sc_format_pid) && pid != u.u_procp->p_pid)
		return (EPERM);
		
	return(physio(sdstrategy, &sdbuf[unit], dev, B_READ, minphys, uio));
}

int
sdwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = sdunit(dev);
	register int pid;

	if ((pid = sd_softc[unit].sc_format_pid) && pid != u.u_procp->p_pid)
		return (EPERM);
		
	return(physio(sdstrategy, &sdbuf[unit], dev, B_WRITE, minphys, uio));
}

int
sdioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	register int unit = sdunit(dev);
	register struct sd_softc *sc = &sd_softc[unit];

	switch (cmd) {
	default:
		return (EINVAL);

	case SDIOCSFORMAT:
		/* take this device into or out of "format" mode */
		if (suser(u.u_cred, &u.u_acflag))
			return(EPERM);

		if (*(int *)data) {
			if (sc->sc_format_pid)
				return (EPERM);
			sc->sc_format_pid = u.u_procp->p_pid;
		} else
			sc->sc_format_pid = 0;
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
		if (sc->sc_format_pid != u.u_procp->p_pid)
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

	if (unit >= NSD || (sc->sc_flags & SDF_ALIVE) == 0)
		return(-1);

	return(sc->sc_info.part[sdpart(dev)].nblocks);
}

#include "machine/pte.h"
#include "machine/vmparam.h"
#include "../sys/vmmac.h"

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
	register daddr_t baddr;
	register int maddr;
	register int pages, i;
	int stat;
	extern int lowram;

	/*
	 * Hmm... all vax drivers dump maxfree pages which is physmem minus
	 * the message buffer.  Is there a reason for not dumping the
	 * message buffer?  Savecore expects to read 'dumpsize' pages of
	 * dump, where dumpsys() sets dumpsize to physmem!
	 */
	pages = physmem;

	/* is drive ok? */
	if (unit >= NSD || (sc->sc_flags & SDF_ALIVE) == 0)
		return (ENXIO);
	/* dump parameters in range? */
	if (dumplo < 0 || dumplo >= sc->sc_info.part[part].nblocks)
		return (EINVAL);
	if (dumplo + ctod(pages) > sc->sc_info.part[part].nblocks)
		pages = dtoc(sc->sc_info.part[part].nblocks - dumplo);
	maddr = lowram;
	baddr = dumplo + sc->sc_info.part[part].strtblk;
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
		mapin(mmap, (u_int)vmmap, btop(maddr), PG_URKR|PG_CI|PG_V);
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
