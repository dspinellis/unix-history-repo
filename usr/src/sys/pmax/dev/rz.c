/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory and Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)rz.c	7.8 (Berkeley) %G%
 */

/*
 * SCSI CCS (Command Command Set) disk driver.
 * NOTE: The name was changed from "sd" to "rz" for DEC naming compatibility.
 * I guess I can't avoid confusion someplace.
 */
#include "rz.h"
#if NRZ > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/errno.h>
#include <sys/fcntl.h>
#include <sys/ioctl.h>
#include <sys/dkstat.h>
#include <sys/disklabel.h>
#include <sys/malloc.h>
#include <sys/proc.h>
#include <sys/uio.h>
#include <sys/stat.h>
#include <sys/syslog.h>

#include <ufs/ffs/fs.h>

#include <pmax/dev/device.h>
#include <pmax/dev/scsi.h>

extern int splbio();
extern void splx();
extern int physio();
extern char *readdisklabel();

int	rzprobe();
void	rzstrategy(), rzstart(), rzdone();

struct	driver rzdriver = {
	"rz", rzprobe, rzstart, rzdone,
};

struct	size {
	u_long	strtblk;
	u_long	nblocks;
};

/*
 * Since the SCSI standard tends to hide the disk structure, we define
 * partitions in terms of DEV_BSIZE blocks.  The default partition table
 * (for an unlabeled disk) reserves 8K for a boot area, has an 8 meg
 * root and 32 meg of swap.  The rest of the space on the drive goes in
 * the G partition.  As usual, the C partition covers the entire disk
 * (including the boot area).
 */
static struct size rzdefaultpart[MAXPARTITIONS] = {
	        0,   16384,	/* A */
	    16384,   65536,	/* B */
	        0,       0,	/* C */
	    17408,       0,	/* D */
	   115712,       0,	/* E */
	   218112,       0,	/* F */
	    81920,       0,	/* G */
	   115712,       0,	/* H */
};

#define	RAWPART		2	/* 'c' partition */	/* XXX */

struct rzstats {
	long	rzresets;
	long	rztransfers;
	long	rzpartials;
};

struct	rz_softc {
	struct	scsi_device *sc_sd;	/* physical unit info */
	pid_t	sc_format_pid;		/* process using "format" mode */
	u_long	sc_openpart;		/* partitions open */
	u_long	sc_bopenpart;		/* block partitions open */
	u_long	sc_copenpart;		/* character partitions open */
	short	sc_flags;		/* see below */
	short	sc_type;		/* drive type from INQUIRY cmd */
	u_int	sc_blks;		/* number of blocks on device */
	int	sc_blksize;		/* device block size in bytes */
	int	sc_bshift;		/* convert device blocks to DEV_BSIZE */
	u_int	sc_wpms;		/* average xfer rate in 16bit wds/sec */
	struct	disklabel sc_label;	/* disk label for this disk */
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
#define	RZF_ALIVE		0x01	/* drive found and ready */
#define	RZF_SENSEINPROGRESS	0x02	/* REQUEST_SENSE command in progress */
#define	RZF_HAVELABEL		0x04	/* valid label found on disk */
#define	RZF_WLABEL		0x08	/* label is writeable */

#ifdef DEBUG
int	rzdebug = 3;
#define RZB_ERROR	0x01
#define RZB_PARTIAL	0x02
#define RZB_PRLABEL	0x04
#endif

#define	rzunit(x)	(minor(x) >> 3)
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
	sc->sc_buf.b_actf = (struct buf *)0;
	sc->sc_tab.b_actf = &sc->sc_buf;
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
		sc->sc_buf.b_actf = (struct buf *)0;
		sc->sc_tab.b_actf = &sc->sc_buf;

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
			sc->sc_buf.b_actf = (struct buf *)0;
			sc->sc_tab.b_actf = &sc->sc_buf;
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
	sc->sc_buf.b_actf = (struct buf *)0;
	sc->sc_tab.b_actf = &sc->sc_buf;
	rzstart(sd->sd_unit);
	if (biowait(&sc->sc_buf) || sc->sc_buf.b_resid != 0)
		goto bad;
	sc->sc_blks = ((capbuf[0] << 24) | (capbuf[1] << 16) |
		(capbuf[2] << 8) | capbuf[3]) + 1;
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
	sc->sc_buf.b_flags = 0;
	return (1);

bad:
	/* doesn't exist or not a CCS device */
	sc->sc_format_pid = 0;
	sc->sc_buf.b_flags = 0;
	return (0);
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
			count = min(resid, bsize - boff);
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
	register struct rz_softc *sc = &rz_softc[unit];
	register struct partition *pp = &sc->sc_label.d_partitions[part];
	register daddr_t bn;
	register long sz, s;

	if (sc->sc_format_pid) {
		if (sc->sc_format_pid != curproc->p_pid) {
			bp->b_error = EPERM;
			goto bad;
		}
		bp->b_cylin = 0;
	} else {
		bn = bp->b_blkno;
		sz = howmany(bp->b_bcount, DEV_BSIZE);
		if ((unsigned)bn + sz > pp->p_size) {
			sz = pp->p_size - bn;
			/* if exactly at end of disk, return an EOF */
			if (sz == 0) {
				bp->b_resid = bp->b_bcount;
				goto done;
			}
			/* if none of it fits, error */
			if (sz < 0) {
				bp->b_error = EINVAL;
				goto bad;
			}
			/* otherwise, truncate */
			bp->b_bcount = dbtob(sz);
		}
		/* check for write to write protected label */
		if (bn + pp->p_offset <= LABELSECTOR &&
#if LABELSECTOR != 0
		    bn + pp->p_offset + sz > LABELSECTOR &&
#endif
		    !(bp->b_flags & B_READ) && !(sc->sc_flags & RZF_WLABEL)) {
			bp->b_error = EROFS;
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
		bp->b_cylin = (bn + pp->p_offset) >> sc->sc_bshift;
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
		sc->sc_tab.b_actf = bp = bp->b_actf;	/* remove sc_errbuf */

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
		} else if (!cold) {
			printf("rz%d: ", unit);
			scsiPrintSense((ScsiClass7Sense *)sc->sc_sense.sense,
				sizeof(sc->sc_sense.sense) - resid);
		}
	} else if (error || (status & SCSI_STATUS_CHECKCOND)) {
#ifdef DEBUG
		if (!cold && (rzdebug & RZB_ERROR))
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
			sc->sc_errbuf.b_actf = bp;
			sc->sc_tab.b_actf = &sc->sc_errbuf;
			rzstart(unit);
			return;
		}
	} else {
		sc->sc_sense.status = status;
		bp->b_resid = resid;
	}

	sc->sc_tab.b_actf = bp->b_actf;
	biodone(bp);
	if (sc->sc_tab.b_actf)
		rzstart(unit);
	else {
		sc->sc_tab.b_active = 0;
		/* finish close protocol */
		if (sc->sc_openpart == 0)
			wakeup((caddr_t)&sc->sc_tab);
	}
}

int
rzopen(dev, flags, mode, p)
	dev_t dev;
	int flags, mode;
	struct proc *p;
{
	register int unit = rzunit(dev);
	register struct rz_softc *sc = &rz_softc[unit];
	register struct disklabel *lp;
	register int i;
	char *err_msg;
	int part;
	u_long mask;

	if (unit >= NRZ || !(sc->sc_flags & RZF_ALIVE))
		return (ENXIO);

	/* try to read disk label and partition table information */
	part = rzpart(dev);
	lp = &sc->sc_label;
	if (!(sc->sc_flags & RZF_HAVELABEL)) {
		sc->sc_flags |= RZF_HAVELABEL;
		lp->d_secsize = DEV_BSIZE;
		lp->d_secpercyl = 1 << sc->sc_bshift;
		lp->d_npartitions = MAXPARTITIONS;
		lp->d_partitions[part].p_offset = 0;
		lp->d_partitions[part].p_size = sc->sc_blks;
		if (err_msg = readdisklabel(dev, rzstrategy, lp)) {
			printf("rz%d: %s\n", unit, err_msg);
			sc->sc_label.d_magic = DISKMAGIC;
			sc->sc_label.d_magic2 = DISKMAGIC;
			sc->sc_label.d_type = DTYPE_SCSI;
			sc->sc_label.d_subtype = 0;
			sc->sc_label.d_typename[0] = '\0';
			sc->sc_label.d_secsize = DEV_BSIZE;
			sc->sc_label.d_secperunit = sc->sc_blks;
			sc->sc_label.d_npartitions = MAXPARTITIONS;
			sc->sc_label.d_bbsize = BBSIZE;
			sc->sc_label.d_sbsize = SBSIZE;
			for (i = 0; i < MAXPARTITIONS; i++) {
				sc->sc_label.d_partitions[i].p_size =
					rzdefaultpart[i].nblocks;
				sc->sc_label.d_partitions[i].p_offset =
					rzdefaultpart[i].strtblk;
			}
			sc->sc_label.d_partitions[RAWPART].p_size =
				sc->sc_blks;
		}
	}

	if (part >= lp->d_npartitions || lp->d_partitions[part].p_size == 0)
		return (ENXIO);
	/*
	 * Warn if a partition is opened that overlaps another
	 * already open, unless either is the `raw' partition
	 * (whole disk).
	 */
	mask = 1 << part;
	if ((sc->sc_openpart & mask) == 0 && part != RAWPART) {
		register struct partition *pp;
		u_long start, end;

		pp = &lp->d_partitions[part];
		start = pp->p_offset;
		end = pp->p_offset + pp->p_size;
		for (pp = lp->d_partitions, i = 0;
		     i < lp->d_npartitions; pp++, i++) {
			if (pp->p_offset + pp->p_size <= start ||
			    pp->p_offset >= end || i == RAWPART)
				continue;
			if (sc->sc_openpart & (1 << i))
				log(LOG_WARNING,
				    "rz%d%c: overlaps open partition (%c)\n",
				    unit, part + 'a', i + 'a');
		}
	}
	switch (mode) {
	case S_IFCHR:
		sc->sc_copenpart |= mask;
		break;
	case S_IFBLK:
		sc->sc_bopenpart |= mask;
		break;
	}
	sc->sc_openpart |= mask;
	if (sc->sc_sd->sd_dk >= 0)
		dk_wpms[sc->sc_sd->sd_dk] = sc->sc_wpms;
	return (0);
}

rzclose(dev, flags, mode)
	dev_t dev;
	int flags, mode;
{
	register struct rz_softc *sc = &rz_softc[rzunit(dev)];
	u_long mask = (1 << rzpart(dev));
	int s;

	switch (mode) {
	case S_IFCHR:
		sc->sc_copenpart &= ~mask;
		break;
	case S_IFBLK:
		sc->sc_bopenpart &= ~mask;
		break;
	}
	sc->sc_openpart = sc->sc_copenpart | sc->sc_bopenpart;

	/*
	 * Should wait for I/O to complete on this partition even if
	 * others are open, but wait for work on blkflush().
	 */
	if (sc->sc_openpart == 0) {
		s = splbio();
		while (sc->sc_tab.b_actf)
			sleep((caddr_t)&sc->sc_tab, PZERO - 1);
		splx(s);
		sc->sc_flags &= ~RZF_WLABEL;
	}
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
	int error;
	int flags;

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

	case DIOCGDINFO:
		/* get the current disk label */
		*(struct disklabel *)data = sc->sc_label;
		return (0);

	case DIOCSDINFO:
		/* set the current disk label */
		if (!(flag & FWRITE))
			return (EBADF);
		error = setdisklabel(&sc->sc_label,
			(struct disklabel *)data,
			(sc->sc_flags & RZF_WLABEL) ? 0 : sc->sc_openpart);
		return (error);

	case DIOCGPART:
		/* return the disk partition data */
		((struct partinfo *)data)->disklab = &sc->sc_label;
		((struct partinfo *)data)->part =
			&sc->sc_label.d_partitions[rzpart(dev)];
		return (0);

	case DIOCWLABEL:
		if (!(flag & FWRITE))
			return (EBADF);
		if (*(int *)data)
			sc->sc_flags |= RZF_WLABEL;
		else
			sc->sc_flags &= ~RZF_WLABEL;
		return (0);

	case DIOCWDINFO:
		/* write the disk label to disk */
		if (!(flag & FWRITE))
			return (EBADF);
		error = setdisklabel(&sc->sc_label,
			(struct disklabel *)data,
			(sc->sc_flags & RZF_WLABEL) ? 0 : sc->sc_openpart);
		if (error)
			return (error);

		/* simulate opening partition 0 so write succeeds */
		flags = sc->sc_flags;
		sc->sc_flags = RZF_ALIVE | RZF_WLABEL;
		error = writedisklabel(dev, rzstrategy, &sc->sc_label);
		sc->sc_flags = flags;
		return (error);
	}
	/*NOTREACHED*/
}

int
rzsize(dev)
	dev_t dev;
{
	register int unit = rzunit(dev);
	register int part = rzpart(dev);
	register struct rz_softc *sc = &rz_softc[unit];

	if (unit >= NRZ || !(sc->sc_flags & RZF_ALIVE) ||
	    part >= sc->sc_label.d_npartitions)
		return (-1);

	return (sc->sc_label.d_partitions[part].p_size);
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
