/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)rz.c	7.1 (Berkeley) %G%
 */

/*
 * SCSI CCS (Command Command Set) disk driver.
 * NOTE: The name was changed from "sd" to "rz" for DEC compatibility.
 * I guess I can't avoid confusion someplace.
 */
#include "rz.h"
#if NRZ > 0

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "errno.h"
#include "dkstat.h"
#include "disklabel.h"
#include "malloc.h"

#include "device.h"
#include "scsi.h"
#include "devDiskLabel.h"

#include "proc.h"
#include "uio.h"

extern void printf();
extern void bcopy();
extern void disksort();
extern int splbio();
extern void splx();
extern int physio();

int	rzprobe();
void	rzstrategy(), rzstart(), rzdone();

struct	driver rzdriver = {
	"rz", rzprobe, rzstart, rzdone,
};

struct	size {
	u_long	strtblk;
	u_long	endblk;
	int	nblocks;
};

struct rzinfo {
	struct	size part[8];
};

/*
 * Since the SCSI standard tends to hide the disk structure, we define
 * partitions in terms of DEV_BSIZE blocks.  The default partition table
 * (for an unlabeled disk) reserves 512K for a boot area, has an 8 meg
 * root and 32 meg of swap.  The rest of the space on the drive goes in
 * the G partition.  As usual, the C partition covers the entire disk
 * (including the boot area).
 */
struct rzinfo rzdefaultpart = {
	     1024,   17408,   16384   ,	/* A */
	    17408,   82944,   65536   ,	/* B */
	        0,       0,       0   ,	/* C */
	    17408,  115712,   98304   ,	/* D */
	   115712,  218112,  102400   ,	/* E */
	   218112,       0,       0   ,	/* F */
	    82944,       0,       0   ,	/* G */
	   115712,       0,       0   ,	/* H */
};

struct rzstats {
	long	rzresets;
	long	rztransfers;
	long	rzpartials;
};

struct	rz_softc {
	struct	scsi_device *sc_sd;	/* physical unit info */
	int	sc_format_pid;		/* process using "format" mode */
	short	sc_flags;		/* see below */
	short	sc_type;		/* drive type from INQUIRY cmd */
	u_int	sc_blks;		/* number of blocks on device */
	int	sc_blksize;		/* device block size in bytes */
	int	sc_bshift;		/* convert device blocks to DEV_BSIZE */
	u_int	sc_wpms;		/* average xfer rate in 16bit wds/sec */
	struct	rzinfo sc_info;		/* drive partition table & label info */
	struct	rzstats sc_stats;	/* statisic counts */
	struct	buf sc_tab;		/* queue of pending operations */
	struct	buf sc_buf;		/* buf for doing I/O */
	struct	buf sc_errbuf;		/* buf for doing REQUEST_SENSE */
	struct	ScsiCmd sc_cmd;		/* command for controller */
	ScsiGroup1Cmd sc_rwcmd;		/* SCSI cmd if not in "format" mode */
	struct	scsi_fmt_cdb sc_cdb;	/* SCSI cmd if in "format" mode */
	struct	scsi_fmt_sense sc_sense;	/* sense data from last cmd */
} rz_softc[NRZ];

/* sc_flags values */
#define	RZF_ALIVE		0x1	/* drive found and ready */
#define	RZF_SENSEINPROGRESS	0x2	/* REQUEST_SENSE command in progress */

#ifdef DEBUG
int	rzdebug = 3;
#define RZB_ERROR	0x01
#define RZB_PARTIAL	0x02
#define RZB_PRLABEL	0x04
#endif

#define	rzunit(x)	((minor(x) >> 3) & 0x7)
#define rzpart(x)	(minor(x) & 0x7)
#define	b_cylin		b_resid

/*
 * Table of scsi commands users are allowed to access via "format" mode.
 *  0 means not legal.
 *  1 means legal.
 */
static char legal_cmds[256] = {
/*****  0   1   2   3   4   5   6   7     8   9   A   B   C   D   E   F */
/*00*/	0,  0,  0,  0,  1,  0,  0,  0,    0,  0,  0,  0,  0,  0,  0,  0,
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

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
rzprobe(sd)
	register struct scsi_device *sd;
{
	register struct rz_softc *sc = &rz_softc[sd->sd_unit];
	register int tries, i;
	ScsiInquiryData inqbuf;
	u_char capbuf[8];
	ScsiClass7Sense *sp;

	/* init some parameters that don't change */
	sc->sc_sd = sd;
	sc->sc_cmd.sd = sd;
	sc->sc_cmd.unit = sd->sd_unit;
	sc->sc_rwcmd.unitNumber = sd->sd_slave;

	/* try to find out what type of device this is */
	sc->sc_format_pid = 1;		/* force use of sc_cdb */
	sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
	scsiGroup0Cmd(SCSI_INQUIRY, sd->sd_slave, 0, sizeof(inqbuf),
		(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
	sc->sc_buf.b_flags = B_BUSY | B_PHYS | B_READ;
	sc->sc_buf.b_bcount = sizeof(inqbuf);
	sc->sc_buf.b_un.b_addr = (caddr_t)&inqbuf;
	sc->sc_buf.av_forw = (struct buf *)0;
	sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;
	rzstart(sd->sd_unit);
	if (biowait(&sc->sc_buf) ||
	    (i = sizeof(inqbuf) - sc->sc_buf.b_resid) < 5)
		goto bad;
	switch (inqbuf.type) {
	case SCSI_DISK_TYPE:		/* disk */
	case SCSI_WORM_TYPE:		/* WORM */
	case SCSI_ROM_TYPE:		/* CD-ROM */
	case SCSI_OPTICAL_MEM_TYPE:	/* Magneto-optical */
		break;

	default:			/* not a disk */
		goto bad;
	}
	sc->sc_type = inqbuf.type;

	/* see if device is ready */
	for (tries = 10; ; ) {
		sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
		scsiGroup0Cmd(SCSI_TEST_UNIT_READY, sd->sd_slave, 0, 0,
			(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
		sc->sc_buf.b_flags = B_BUSY | B_PHYS | B_READ;
		sc->sc_buf.b_bcount = 0;
		sc->sc_buf.b_un.b_addr = (caddr_t)0;
		sc->sc_buf.av_forw = (struct buf *)0;
		sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;

		sc->sc_cmd.cmd = sc->sc_cdb.cdb;
		sc->sc_cmd.cmdlen = sc->sc_cdb.len;
		sc->sc_cmd.buf = (caddr_t)0;
		sc->sc_cmd.buflen = 0;
		/* setup synchronous data transfers if the device supports it */
		if (tries == 10 && (inqbuf.flags & SCSI_SYNC))
			sc->sc_cmd.flags = SCSICMD_USE_SYNC;
		else
			sc->sc_cmd.flags = 0;

		(*sc->sc_sd->sd_cdriver->d_start)(&sc->sc_cmd);
		if (!biowait(&sc->sc_buf))
			break;
		if (--tries < 0)
			goto bad;
		if (!(sc->sc_sense.status & SCSI_STATUS_CHECKCOND))
			goto again;
		sp = (ScsiClass7Sense *)sc->sc_sense.sense;
		if (sp->error7 != 0x70)
			goto again;
		if (sp->key == SCSI_CLASS7_UNIT_ATTN && tries != 9) {
			/* drive recalibrating, give it a while */
			DELAY(1000000);
			continue;
		}
		if (sp->key == SCSI_CLASS7_NOT_READY) {
			ScsiStartStopCmd *cp;

			/* try to spin-up disk with start/stop command */
			sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
			cp = (ScsiStartStopCmd *)sc->sc_cdb.cdb;
			cp->command = SCSI_START_STOP;
			cp->unitNumber = sd->sd_slave;
			cp->immed = 0;
			cp->loadEject = 0;
			cp->start = 1;
			cp->pad1 = 0;
			cp->pad2 = 0;
			cp->pad3 = 0;
			cp->pad4 = 0;
			cp->control = 0;
			sc->sc_buf.b_flags = B_BUSY | B_PHYS | B_READ;
			sc->sc_buf.b_bcount = 0;
			sc->sc_buf.b_un.b_addr = (caddr_t)0;
			sc->sc_buf.av_forw = (struct buf *)0;
			sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;
			rzstart(sd->sd_unit);
			if (biowait(&sc->sc_buf))
				goto bad;
			continue;
		}
	again:
		DELAY(1000);
	}

	/* find out how big a disk this is */
	sc->sc_cdb.len = sizeof(ScsiGroup1Cmd);
	scsiGroup1Cmd(SCSI_READ_CAPACITY, sd->sd_slave, 0, 0,
		(ScsiGroup1Cmd *)sc->sc_cdb.cdb);
	sc->sc_buf.b_flags = B_BUSY | B_PHYS | B_READ;
	sc->sc_buf.b_bcount = sizeof(capbuf);
	sc->sc_buf.b_un.b_addr = (caddr_t)capbuf;
	sc->sc_buf.av_forw = (struct buf *)0;
	sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;
	rzstart(sd->sd_unit);
	if (biowait(&sc->sc_buf) || sc->sc_buf.b_resid != 0)
		goto bad;
	sc->sc_blks = (capbuf[0] << 24) | (capbuf[1] << 16) |
		(capbuf[2] << 8) | capbuf[3];
	sc->sc_blksize = (capbuf[4] << 24) | (capbuf[5] << 16) |
		(capbuf[6] << 8) | capbuf[7];

	printf("rz%d at %s%d drive %d slave %d", sd->sd_unit,
		sd->sd_cdriver->d_name, sd->sd_ctlr, sd->sd_drive,
		sd->sd_slave);
	if (inqbuf.version > 1 || i < 36)
		printf(" type 0x%x, qual 0x%x, ver %d",
			inqbuf.type, inqbuf.qualifier, inqbuf.version);
	else {
		char vid[9], pid[17], revl[5];

		bcopy((caddr_t)inqbuf.vendorID, (caddr_t)vid, 8);
		bcopy((caddr_t)inqbuf.productID, (caddr_t)pid, 16);
		bcopy((caddr_t)inqbuf.revLevel, (caddr_t)revl, 4);
		for (i = 8; --i > 0; )
			if (vid[i] != ' ')
				break;
		vid[i+1] = 0;
		for (i = 16; --i > 0; )
			if (pid[i] != ' ')
				break;
		pid[i+1] = 0;
		for (i = 4; --i > 0; )
			if (revl[i] != ' ')
				break;
		revl[i+1] = 0;
		printf(" %s %s rev %s", vid, pid, revl);
	}
	printf(", %d %d byte blocks\n", sc->sc_blks, sc->sc_blksize);
	if (sc->sc_blksize != DEV_BSIZE) {
		if (sc->sc_blksize < DEV_BSIZE) {
			printf("rz%d: need %d byte blocks - drive ignored\n",
				sd->sd_unit, DEV_BSIZE);
			goto bad;
		}
		for (i = sc->sc_blksize; i > DEV_BSIZE; i >>= 1)
			++sc->sc_bshift;
		sc->sc_blks <<= sc->sc_bshift;
	}
	sc->sc_wpms = 32 * (60 * DEV_BSIZE / 2);	/* XXX */
	sc->sc_format_pid = 0;
	sc->sc_flags = RZF_ALIVE;

	/* try to read disk label or partition table information */
	if (rzreadlabel(sc, sd) == 0)
		goto ok;

	/*
	 * We don't have a disk label, build a default partition
	 * table with 'standard' size root & swap and everything else
	 * in the G partition.
	 */
	sc->sc_info = rzdefaultpart;
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
	} else
		sc->sc_info.part[7].strtblk = 0;

ok:
	sc->sc_buf.b_flags = 0;
	return (1);

bad:
	/* doesn't exist or not a CCS device */
	sc->sc_format_pid = 0;
	sc->sc_buf.b_flags = 0;
	return (0);
}

/*
 * Try to read the disk label and fill in the partition table info.
 */
static int
rzreadlabel(sc, sd)
	register struct rz_softc *sc;
	register struct scsi_device *sd;
{
	register struct size *sp;
	Sun_DiskLabel *sunLabelPtr;
	Dec_DiskLabel *decLabelPtr;
	char labelBuffer[DEV_BSIZE];
	int part, error;

	/*
	 * The label of a SCSI disk normally resides in the first sector.
	 * Format and send a SCSI READ command to fetch the sector.
	 */
	sc->sc_buf.b_flags = B_BUSY | B_PHYS | B_READ;
	sc->sc_buf.b_bcount = sizeof(labelBuffer);
	sc->sc_buf.b_un.b_addr = labelBuffer;
	sc->sc_buf.b_cylin = SUN_LABEL_SECTOR;
	sc->sc_buf.av_forw = (struct buf *)0;
	sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;
	rzstart(sd->sd_unit);
	if (error = biowait(&sc->sc_buf))
		return (error);
	sunLabelPtr = (Sun_DiskLabel *)labelBuffer;
	if (sunLabelPtr->magic == SUN_DISK_MAGIC) {
		/*
		 * XXX - Should really check if label is valid.
		 */
#ifdef DEBUG
		if (rzdebug & RZB_PRLABEL) {
			printf("rz%d: SUN label %s\n", sd->sd_unit,
				sunLabelPtr->asciiLabel);
			printf("  Partitions");
		}
#endif
		sp = sc->sc_info.part;
		for (part = 0; part < DEV_NUM_DISK_PARTS; part++, sp++) {
			sp->strtblk =
				sunLabelPtr->map[part].cylinder *
				sunLabelPtr->numHeads * 
				sunLabelPtr->numSectors;
			sp->nblocks =
				sunLabelPtr->map[part].numBlocks;
			sp->endblk = sp->strtblk + sp->nblocks;
#ifdef DEBUG
			if (rzdebug & RZB_PRLABEL)
				printf(" (%d,%d)", sp->strtblk, sp->nblocks);
#endif
		}
#ifdef DEBUG
		if (rzdebug & RZB_PRLABEL)
			printf("\n");
#endif
		return (0);
	}

	/*
	 * The disk isn't in SUN or UNIX format so try Dec format.
	 * We have to read the right sector first.
	 */
	sc->sc_buf.b_flags = B_BUSY | B_PHYS | B_READ;
	sc->sc_buf.b_bcount = sizeof(labelBuffer);
	sc->sc_buf.b_un.b_addr = labelBuffer;
	sc->sc_buf.b_cylin = DEC_LABEL_SECTOR;
	sc->sc_buf.av_forw = (struct buf *)0;
	sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;
	rzstart(sd->sd_unit);
	if (error = biowait(&sc->sc_buf))
		return (error);
	decLabelPtr = (Dec_DiskLabel *)labelBuffer;
	if (decLabelPtr->magic == DEC_LABEL_MAGIC &&
	    decLabelPtr->isPartitioned) {
		/*
		 * XXX - Should really check if label is valid.
		 */
#ifdef DEBUG
		if (rzdebug & RZB_PRLABEL) {
			printf("rz%d: DEC label\n", sd->sd_unit);
			printf("  Partitions");
		}
#endif
		sp = sc->sc_info.part;
		for (part = 0; part < DEV_NUM_DISK_PARTS; part++, sp++) {
			sp->strtblk = decLabelPtr->map[part].startBlock;
			sp->nblocks = decLabelPtr->map[part].numBlocks;
			sp->endblk = sp->strtblk + sp->nblocks;
#ifdef DEBUG
			if (rzdebug & RZB_PRLABEL)
				printf(" (%d,%d)", sp->strtblk, sp->nblocks);
#endif
		}
#ifdef DEBUG
		if (rzdebug & RZB_PRLABEL)
			printf("\n");
#endif
		return (0);
	}
	return (EIO);
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
rzlblkstrat(bp, bsize)
	register struct buf *bp;
	register int bsize;
{
	register struct buf *cbp;
	caddr_t cbuf;
	register int bn, resid;
	register caddr_t addr;

	cbp = (struct buf *)malloc(sizeof(struct buf), M_DEVBUF, M_WAITOK);
	cbuf = (caddr_t)malloc(bsize, M_DEVBUF, M_WAITOK);
	bzero((caddr_t)cbp, sizeof(*cbp));
	cbp->b_proc = curproc;
	cbp->b_dev = bp->b_dev;
	bn = bp->b_blkno;
	resid = bp->b_bcount;
	addr = bp->b_un.b_addr;
#ifdef DEBUG
	if (rzdebug & RZB_PARTIAL)
		printf("rzlblkstrat: bp %x flags %x bn %x resid %x addr %x\n",
		       bp, bp->b_flags, bn, resid, addr);
#endif

	while (resid > 0) {
		register int boff = dbtob(bn) & (bsize - 1);
		register int count;

		if (boff || resid < bsize) {
			rz_softc[rzunit(bp->b_dev)].sc_stats.rzpartials++;
			count = MIN(resid, bsize - boff);
			cbp->b_flags = B_BUSY | B_PHYS | B_READ;
			cbp->b_blkno = bn - btodb(boff);
			cbp->b_un.b_addr = cbuf;
			cbp->b_bcount = bsize;
#ifdef DEBUG
			if (rzdebug & RZB_PARTIAL)
				printf(" readahead: bn %x cnt %x off %x addr %x\n",
				       cbp->b_blkno, count, boff, addr);
#endif
			rzstrategy(cbp);
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
			if (rzdebug & RZB_PARTIAL)
				printf(" writeback: bn %x cnt %x off %x addr %x\n",
				       cbp->b_blkno, count, boff, addr);
#endif
		} else {
			count = resid & ~(bsize - 1);
			cbp->b_blkno = bn;
			cbp->b_un.b_addr = addr;
			cbp->b_bcount = count;
#ifdef DEBUG
			if (rzdebug & RZB_PARTIAL)
				printf(" fulltrans: bn %x cnt %x addr %x\n",
				       cbp->b_blkno, count, addr);
#endif
		}
		cbp->b_flags = B_BUSY | B_PHYS | (bp->b_flags & B_READ);
		rzstrategy(cbp);
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
		if (rzdebug & RZB_PARTIAL)
			printf(" done: bn %x resid %x addr %x\n",
			       bn, resid, addr);
#endif
	}
	free(cbuf, M_DEVBUF);
	free(cbp, M_DEVBUF);
}

void
rzstrategy(bp)
	register struct buf *bp;
{
	register int unit = rzunit(bp->b_dev);
	register int part = rzpart(bp->b_dev);
	register int bn, sz;
	register struct rz_softc *sc = &rz_softc[unit];
	register int s;

	if (sc->sc_format_pid) {
		if (sc->sc_format_pid != curproc->p_pid) {
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
			rzlblkstrat(bp, sc->sc_blksize);
			goto done;
		}
		bp->b_cylin = (bn + sc->sc_info.part[part].strtblk) >>
				sc->sc_bshift;
	}
	/* don't let disksort() see sc_errbuf */
	while (sc->sc_flags & RZF_SENSEINPROGRESS)
		printf("SENSE\n"); /* XXX */
	s = splbio();
	disksort(&sc->sc_tab, bp);
	if (sc->sc_tab.b_active == 0) {
		sc->sc_tab.b_active = 1;
		rzstart(unit);
	}
	splx(s);
	return;
bad:
	bp->b_flags |= B_ERROR;
done:
	biodone(bp);
}

void
rzstart(unit)
	int unit;
{
	register struct rz_softc *sc = &rz_softc[unit];
	register struct buf *bp = sc->sc_tab.b_actf;
	register int n;

	sc->sc_cmd.buf = bp->b_un.b_addr;
	sc->sc_cmd.buflen = bp->b_bcount;

	if (sc->sc_format_pid || (sc->sc_flags & RZF_SENSEINPROGRESS)) {
		sc->sc_cmd.flags = !(bp->b_flags & B_READ) ?
			SCSICMD_DATA_TO_DEVICE : 0;
		sc->sc_cmd.cmd = sc->sc_cdb.cdb;
		sc->sc_cmd.cmdlen = sc->sc_cdb.len;
	} else {
		if (bp->b_flags & B_READ) {
			sc->sc_cmd.flags = 0;
			sc->sc_rwcmd.command = SCSI_READ_EXT;
		} else {
			sc->sc_cmd.flags = SCSICMD_DATA_TO_DEVICE;
			sc->sc_rwcmd.command = SCSI_WRITE_EXT;
		}
		sc->sc_cmd.cmd = (u_char *)&sc->sc_rwcmd;
		sc->sc_cmd.cmdlen = sizeof(sc->sc_rwcmd);
		n = bp->b_cylin;
		sc->sc_rwcmd.highAddr = n >> 24;
		sc->sc_rwcmd.midHighAddr = n >> 16;
		sc->sc_rwcmd.midLowAddr = n >> 8;
		sc->sc_rwcmd.lowAddr = n;
		n = howmany(bp->b_bcount, sc->sc_blksize);
		sc->sc_rwcmd.highBlockCount = n >> 8;
		sc->sc_rwcmd.lowBlockCount = n;
#ifdef DEBUG
		if ((bp->b_bcount & (sc->sc_blksize - 1)) != 0)
			printf("rz%d: partial block xfer -- %x bytes\n",
				unit, bp->b_bcount);
#endif
		sc->sc_stats.rztransfers++;
		if ((n = sc->sc_sd->sd_dk) >= 0) {
			dk_busy |= 1 << n;
			++dk_seek[n];
			++dk_xfer[n];
			dk_wds[n] += bp->b_bcount >> 6;
		}
	}

	/* tell controller to start this command */
	(*sc->sc_sd->sd_cdriver->d_start)(&sc->sc_cmd);
}

/*
 * This is called by the controller driver when the command is done.
 */
void
rzdone(unit, error, resid, status)
	register int unit;
	int error;		/* error number from errno.h */
	int resid;		/* amount not transfered */
	int status;		/* SCSI status byte */
{
	register struct rz_softc *sc = &rz_softc[unit];
	register struct buf *bp = sc->sc_tab.b_actf;
	register struct scsi_device *sd = sc->sc_sd;
	extern int cold;

	if (bp == NULL) {
		printf("rz%d: bp == NULL\n", unit);
		return;
	}
	if (sd->sd_dk >= 0)
		dk_busy &= ~(1 << sd->sd_dk);
	if (sc->sc_flags & RZF_SENSEINPROGRESS) {
		sc->sc_flags &= ~RZF_SENSEINPROGRESS;
		sc->sc_tab.b_actf = bp = bp->av_forw;	/* remove sc_errbuf */

		if (error || (status & SCSI_STATUS_CHECKCOND)) {
#ifdef DEBUG
			if (rzdebug & RZB_ERROR)
				printf("rz%d: error reading sense data: error %d scsi status 0x%x\n",
					unit, error, status);
#endif
			/*
			 * We got an error during the REQUEST_SENSE,
			 * fill in no sense for data.
			 */
			sc->sc_sense.sense[0] = 0x70;
			sc->sc_sense.sense[2] = SCSI_CLASS7_NO_SENSE;
		} else if (!cold
#ifdef DEBUG
			|| (rzdebug & RZB_ERROR)
#endif
		) {
			printf("rz%d: ", unit);
			scsiPrintSense((ScsiClass7Sense *)sc->sc_sense.sense,
				sizeof(sc->sc_sense.sense) - resid);
		}
	} else if (error || (status & SCSI_STATUS_CHECKCOND)) {
#ifdef DEBUG
		if (rzdebug & RZB_ERROR)
			printf("rz%d: error %d scsi status 0x%x\n",
				unit, error, status);
#endif
		/* save error info */
		sc->sc_sense.status = status;
		bp->b_flags |= B_ERROR;
		bp->b_error = error;
		bp->b_resid = resid;

		if (status & SCSI_STATUS_CHECKCOND) {
			/*
			 * Start a REQUEST_SENSE command.
			 * Since we are called at interrupt time, we can't
			 * wait for the command to finish; that's why we use
			 * the sc_flags field.
			 */
			sc->sc_flags |= RZF_SENSEINPROGRESS;
			sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
			scsiGroup0Cmd(SCSI_REQUEST_SENSE, sd->sd_slave, 0,
				sizeof(sc->sc_sense.sense),
				(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
			sc->sc_errbuf.b_flags = B_BUSY | B_PHYS | B_READ;
			sc->sc_errbuf.b_bcount = sizeof(sc->sc_sense.sense);
			sc->sc_errbuf.b_un.b_addr = (caddr_t)sc->sc_sense.sense;
			sc->sc_errbuf.av_forw = bp;
			sc->sc_tab.b_actf = &sc->sc_errbuf;
			rzstart(unit);
			return;
		}
	} else {
		sc->sc_sense.status = status;
		bp->b_resid = resid;
	}

	sc->sc_tab.b_actf = bp->av_forw;
	biodone(bp);
	if (sc->sc_tab.b_actf)
		rzstart(unit);
	else
		sc->sc_tab.b_active = 0;
}

int
rzopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	register int unit = rzunit(dev);
	register struct rz_softc *sc = &rz_softc[unit];

	if (unit >= NRZ)
		return (ENXIO);
	if (!(sc->sc_flags & RZF_ALIVE) || suser(p->p_ucred, &p->p_acflag))
		return (ENXIO);

	if (sc->sc_sd->sd_dk >= 0)
		dk_wpms[sc->sc_sd->sd_dk] = sc->sc_wpms;
	return (0);
}

rzclose(dev, flags)
	dev_t dev;
	int flags;
{
	return (0);
}

int
rzread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct rz_softc *sc = &rz_softc[rzunit(dev)];

	if (sc->sc_format_pid && sc->sc_format_pid != curproc->p_pid)
		return (EPERM);

	return (physio(rzstrategy, (struct buf *)0, dev,
		B_READ, minphys, uio));
}

int
rzwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register struct rz_softc *sc = &rz_softc[rzunit(dev)];

	if (sc->sc_format_pid && sc->sc_format_pid != curproc->p_pid)
		return (EPERM);

	return (physio(rzstrategy, (struct buf *)0, dev,
		B_WRITE, minphys, uio));
}

int
rzioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	register struct rz_softc *sc = &rz_softc[rzunit(dev)];

	switch (cmd) {
	default:
		return (EINVAL);

	case SDIOCSFORMAT:
		/* take this device into or out of "format" mode */
		if (suser(p->p_ucred, &p->p_acflag))
			return (EPERM);

		if (*(int *)data) {
			if (sc->sc_format_pid)
				return (EPERM);
			sc->sc_format_pid = p->p_pid;
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
		if (sc->sc_format_pid != p->p_pid)
			return (EPERM);
		if (legal_cmds[((struct scsi_fmt_cdb *)data)->cdb[0]] == 0)
			return (EINVAL);
		bcopy(data, (caddr_t)&sc->sc_cdb, sizeof(sc->sc_cdb));
		return (0);

	case SDIOCSENSE:
		/*
		 * return the SCSI sense data saved after the last
		 * operation that completed with "check condition" status.
		 */
		bcopy((caddr_t)&sc->sc_sense, data, sizeof(sc->sc_sense));
		return (0);

	}
	/*NOTREACHED*/
}

int
rzsize(dev)
	dev_t dev;
{
	register int unit = rzunit(dev);
	register struct rz_softc *sc = &rz_softc[unit];

	if (unit >= NRZ || !(sc->sc_flags & RZF_ALIVE))
		return (-1);

	return (sc->sc_info.part[rzpart(dev)].nblocks);
}

/*
 * Non-interrupt driven, non-dma dump routine.
 */
int
rzdump(dev)
	dev_t dev;
{
#ifdef notdef
	int part = rzpart(dev);
	int unit = rzunit(dev);
	register struct rz_softc *sc = &rz_softc[unit];
	register struct scsi_device *sd = sc->sc_hd;
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
	if (unit >= NRZ || (sc->sc_flags & RZF_ALIVE) == 0)
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
		scsireset(sd->sd_ctlr);
		sc->sc_stats.rzresets++;
		printf("[ drive %d reset ] ", unit);
	}
	for (i = 0; i < pages; i++) {
#define NPGMB	(1024*1024/NBPG)
		/* print out how many Mbs we have dumped */
		if (i && (i % NPGMB) == 0)
			printf("%d ", i / NPGMB);
#undef NPBMG
		mapin(mmap, (u_int)vmmap, btop(maddr), PG_URKR|PG_CI|PG_V);
		stat = scsi_tt_write(sd->sd_ctlr, sd->sd_drive, sd->sd_slave,
				     vmmap, NBPG, baddr, sc->sc_bshift);
		if (stat) {
			printf("rzdump: scsi write error 0x%x\n", stat);
			return (EIO);
		}
		maddr += NBPG;
		baddr += ctod(1);
	}
	return (0);
#else notdef
	return (ENXIO);
#endif notdef
}
#endif
