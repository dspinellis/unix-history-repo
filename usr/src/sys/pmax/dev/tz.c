/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tz.c	7.1 (Berkeley) %G%
 *
 * from: $Header: /sprite/src/kernel/dev/RCS/devSCSITape.c,
 *	v 8.14 89/07/31 17:26:13 mendel Exp $ SPRITE (Berkeley)
 */

/*
 * SCSI CCS (Command Command Set) tape driver.
 */
#include "tz.h"
#if NTZ > 0

#include "param.h"
#include "systm.h"
#include "buf.h"
#include "errno.h"
#include "file.h"
#include "ioctl.h"
#include "mtio.h"
#include "syslog.h"

#include "device.h"
#include "scsi.h"

int	tzprobe();
void	tzstart(), tzdone();

struct	driver tzdriver = {
	"tz", tzprobe, tzstart, tzdone,
};

struct	tz_softc {
	struct	scsi_device *sc_sd;	/* physical unit info */
	int	sc_flags;		/* see below */
	struct	buf sc_tab;		/* queue of pending operations */
	struct	buf sc_buf;		/* buf for doing I/O */
	struct	buf sc_errbuf;		/* buf for doing REQUEST_SENSE */
	struct	ScsiCmd sc_cmd;		/* command for controller */
	ScsiGroup0Cmd sc_rwcmd;		/* SCSI cmd for read/write */
	struct	scsi_fmt_cdb sc_cdb;	/* SCSI cmd if not read/write */
	struct	scsi_fmt_sense sc_sense;	/* sense data from last cmd */
} tz_softc[NTZ];

/* sc_flags values */
#define	TZF_ALIVE		0x01	/* drive found and ready */
#define	TZF_SENSEINPROGRESS	0x02	/* REQUEST_SENSE command in progress */
#define	TZF_ALTCMD		0x04	/* alternate command in progress */
#define	TZF_WRITTEN		0x08	/* tape has been written to */
#define	TZF_OPEN		0x10	/* device is open */
#define	TZF_WAIT		0x20	/* waiting for sc_tab to drain */

/* bits in minor device */
#define	tzunit(x)	(minor(x) >> 1)	/* tz%d unit number */
#define TZ_NOREWIND	0x1		/* don't rewind on close */

#define	INF	(daddr_t)1000000L	/* a block number that won't exist */

#ifdef DEBUG
int	tzdebug = 1;
#endif

/*
 * Test to see if device is present.
 * Return true if found and initialized ok.
 */
tzprobe(sd)
	struct scsi_device *sd;
{
	register struct tz_softc *sc = &tz_softc[sd->sd_unit];
	register int i;
	ScsiInquiryData inqbuf;
	ScsiClass7Sense *sp;

	printf("tzprobe()\n"); /* XXX */
	/* init some parameters that don't change */
	sc->sc_sd = sd;
	sc->sc_cmd.sd = sd;
	sc->sc_cmd.unit = sd->sd_unit;
	sc->sc_rwcmd.unitNumber = sd->sd_slave;
/*	sc->sc_rwcmd.highAddr = 1;	/* count in blocks */

	/* try to find out what type of device this is */
	sc->sc_flags = TZF_ALTCMD;	/* force use of sc_cdb */
	sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
	scsiGroup0Cmd(SCSI_INQUIRY, sd->sd_slave, 0, sizeof(inqbuf),
		(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
	sc->sc_buf.b_flags = B_BUSY | B_READ;
	sc->sc_buf.b_bcount = sizeof(inqbuf);
	sc->sc_buf.b_un.b_addr = (caddr_t)&inqbuf;
	sc->sc_buf.av_forw = (struct buf *)0;
	sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;
	tzstart(sd->sd_unit);
	if (biowait(&sc->sc_buf) ||
	    (i = sizeof(inqbuf) - sc->sc_buf.b_resid) < 5)
		goto bad;
	if (inqbuf.type != SCSI_TAPE_TYPE)
		goto bad;

	/* check for device ready to clear UNIT_ATTN */
	sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
	scsiGroup0Cmd(SCSI_TEST_UNIT_READY, sd->sd_slave, 0, 0,
		(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
	sc->sc_buf.b_flags = B_BUSY | B_READ;
	sc->sc_buf.b_bcount = 0;
	sc->sc_buf.b_un.b_addr = (caddr_t)0;
	sc->sc_buf.av_forw = (struct buf *)0;
	sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;
	tzstart(sd->sd_unit);
	(void) biowait(&sc->sc_buf);

	sc->sc_flags = TZF_ALIVE;
	sc->sc_buf.b_flags = 0;
	printf("tz%d at %s%d drive %d slave %d\n", sd->sd_unit,
		sd->sd_cdriver->d_name, sd->sd_ctlr, sd->sd_drive,
		sd->sd_slave);
	return (1);

bad:
	/* doesn't exist or not a CCS device */
	sc->sc_flags = 0;
	sc->sc_buf.b_flags = 0;
	return (0);
}

/*
 * Perform a special tape command on a SCSI Tape drive.
 */
tzcommand(dev, command, code, count)
	dev_t dev;
	int command;
	int code;
	int count;
{
	register struct tz_softc *sc = &tz_softc[tzunit(dev)];
	int s, error;

	s = splbio();
	while (sc->sc_tab.b_actf) {
		sc->sc_flags |= TZF_WAIT;
		sleep(&sc->sc_flags, PZERO);
	}
	sc->sc_flags |= TZF_ALTCMD;	/* force use of sc_cdb */
	sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
	scsiGroup0Cmd(command, sc->sc_sd->sd_slave,
		(code << 16) | ((count >> 8) & 0xFFFF), count & 0xFF,
		(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
	sc->sc_buf.b_flags = B_BUSY | B_READ;
	sc->sc_buf.b_bcount = 0;
	sc->sc_buf.b_un.b_addr = (caddr_t)0;
	sc->sc_buf.av_forw = (struct buf *)0;
	sc->sc_tab.b_actf = sc->sc_tab.b_actl = &sc->sc_buf;
	tzstart(sc->sc_sd->sd_unit);
	error = biowait(&sc->sc_buf);
	sc->sc_flags &= ~TZF_ALTCMD;	/* force use of sc_cdb */
	sc->sc_buf.b_flags = 0;
	splx(s);
	return (error);
}

void
tzstart(unit)
	int unit;
{
	register struct tz_softc *sc = &tz_softc[unit];
	register struct buf *bp = sc->sc_tab.b_actf;
	register int n;
	extern int sii_debug; /* XXX */

	sc->sc_cmd.buf = bp->b_un.b_addr;
	sc->sc_cmd.buflen = bp->b_bcount;

	if (sc->sc_flags & (TZF_SENSEINPROGRESS | TZF_ALTCMD)) {
		sc->sc_cmd.dataToDevice = !(bp->b_flags & B_READ);
		sc->sc_cmd.cmd = sc->sc_cdb.cdb;
		sc->sc_cmd.cmdlen = sc->sc_cdb.len;
	} else {
		if (bp->b_flags & B_READ) {
			sc->sc_cmd.dataToDevice = 0;
			sc->sc_rwcmd.command = SCSI_READ;
		} else {
			sc->sc_cmd.dataToDevice = 1;
			sc->sc_rwcmd.command = SCSI_WRITE;
		}
		sc->sc_cmd.cmd = (u_char *)&sc->sc_rwcmd;
		sc->sc_cmd.cmdlen = sizeof(sc->sc_rwcmd);
		n = howmany(bp->b_bcount, 512);
		sc->sc_rwcmd.midAddr = n >> 16;
		sc->sc_rwcmd.lowAddr = n >> 8;
		sc->sc_rwcmd.blockCount = n;
		if ((bp->b_bcount & (512 - 1)) != 0)
			printf("tz%d: partial block xfer -- %x bytes\n",
				unit, bp->b_bcount);
	}

	printf("tzstart(%d) flags %x, addr %x sz %d\n", unit,
		sc->sc_flags, sc->sc_cmd.buf, sc->sc_cmd.buflen); /* XXX */
	/* tell controller to start this command */
	if (sc->sc_cmd.cmd[0] == SCSI_READ)
		sii_debug = 5; /* XXX */
	(*sc->sc_sd->sd_cdriver->d_start)(&sc->sc_cmd);
}

/*
 * This is called by the controller driver when the command is done.
 */
void
tzdone(unit, error, resid, status)
	int unit;
	int error;		/* error number from errno.h */
	int resid;		/* amount not transfered */
	int status;		/* SCSI status byte */
{
	register struct tz_softc *sc = &tz_softc[unit];
	register struct buf *bp = sc->sc_tab.b_actf;
	extern int cold;
	extern int sii_debug; /* XXX */

	printf("tzdone(%d, %d, %d, %x) %x flags %x\n", unit, error, resid,
		status, sc, sc->sc_flags); /* XXX */
	if (bp == NULL) {
		printf("tz%d: bp == NULL\n", unit);
		return;
	}
	if (sc->sc_flags & TZF_SENSEINPROGRESS) {
		sc->sc_flags &= ~TZF_SENSEINPROGRESS;
		sc->sc_tab.b_actf = bp = bp->b_actf;	/* remove sc_errbuf */
		if (bp == 0) {
			sii_DumpLog();
			panic("tzdone"); /* XXX */
		}

		if (error || (status & SCSI_STATUS_CHECKCOND)) {
#ifdef DEBUG
			if (tzdebug)
				printf("tz%d: error reading sense data: error %d scsi status 0x%x\n",
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
			|| tzdebug
#endif
		) {
			printf("tz%d: ", unit);
			scsiPrintSense((ScsiClass7Sense *)sc->sc_sense.sense,
				sizeof(sc->sc_sense.sense) - resid);
		}
	} else if (error || (status & SCSI_STATUS_CHECKCOND)) {
#ifdef DEBUG
		if (tzdebug)
			printf("tz%d: error %d scsi status 0x%x\n",
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
			sc->sc_flags |= TZF_SENSEINPROGRESS;
			sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
			scsiGroup0Cmd(SCSI_REQUEST_SENSE, sc->sc_sd->sd_slave,
				0, sizeof(sc->sc_sense.sense),
				(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
			sc->sc_errbuf.b_flags = B_BUSY | B_READ;
			sc->sc_errbuf.b_bcount = sizeof(sc->sc_sense.sense);
			sc->sc_errbuf.b_un.b_addr = (caddr_t)sc->sc_sense.sense;
			sc->sc_errbuf.av_forw = bp;
			sc->sc_tab.b_actf = &sc->sc_errbuf;
			tzstart(unit);
			return;
		}
	} else {
		sc->sc_sense.status = status;
		bp->b_resid = resid;
	}

	sc->sc_tab.b_actf = bp->b_actf;
	biodone(bp);
	if (sc->sc_tab.b_actf)
		tzstart(unit);
	else {
		sii_debug = 1; /* XXX */
		sc->sc_tab.b_active = 0;
		if (sc->sc_flags & TZF_WAIT) {
			sc->sc_flags &= ~TZF_WAIT;
			wakeup(&sc->sc_flags);
		}
	}
}

tzopen(dev, flags)
	dev_t dev;
	int flags;
{
	register int unit = tzunit(dev);
	register struct tz_softc *sc = &tz_softc[unit];
	int error;

	if (unit >= NTZ)
		return (ENXIO);
	if (!(sc->sc_flags & TZF_ALIVE)) {
		/* check again, tape may have been turned off at boot time */
		if (!tzprobe(sc->sc_sd))
			return (ENXIO);
	}
	if (sc->sc_flags & TZF_OPEN)
		return (EBUSY);

	/* clear UNIT_ATTENTION */
	error = tzcommand(dev, SCSI_TEST_UNIT_READY, 0, 0);
	if (error) {
		ScsiClass7Sense *sp = (ScsiClass7Sense *)sc->sc_sense.sense;

		/* return error if last error was not UNIT_ATTENTION */
		if (!(sc->sc_sense.status & SCSI_STATUS_CHECKCOND) ||
		    sp->error7 != 0x70 || sp->key != SCSI_CLASS7_UNIT_ATTN)
			return (error);
	}

#ifdef notdef
	if ((flag&FWRITE) && (sc->sc_dsreg&HTDS_WRL)) {
		sc->sc_openf = 0;
		uprintf("tu%d: no write ring\n", tuunit);
		return (EIO);
	}
	sc->sc_ctty = (caddr_t)(u.u_procp->p_flag & SCTTY ? 
			u.u_procp->p_session->s_ttyp : 0);
#endif
	sc->sc_flags = TZF_ALIVE | TZF_OPEN;
	return (0);
}

tzclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tz_softc *sc = &tz_softc[tzunit(dev)];

	if (!(sc->sc_flags & TZF_OPEN))
		return (0);
	if (flag == FWRITE ||
	    ((flag & FWRITE) && (sc->sc_flags & TZF_WRITTEN))) {
		(void) tzcommand(dev, SCSI_WRITE_EOF, 0, 1);
	}
	if ((minor(dev) & TZ_NOREWIND) == 0)
		(void) tzcommand(dev, SCSI_REWIND, 0, 0);
	sc->sc_flags &= ~(TZF_OPEN | TZF_WRITTEN);
	return (0);
}

tzioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	register struct tz_softc *sc = &tz_softc[tzunit(dev)];
	register struct buf *bp = &sc->sc_buf;
	struct mtop *mtop;
	struct mtget *mtget;
	int code, count;
	static tzops[] = {
		SCSI_WRITE_EOF, SCSI_SPACE, SCSI_SPACE, SCSI_SPACE, SCSI_SPACE,
		SCSI_REWIND, SCSI_REWIND, SCSI_TEST_UNIT_READY
	};

	switch (cmd) {

	case MTIOCTOP:	/* tape operation */
		mtop = (struct mtop *)data;
		if ((unsigned)mtop->mt_op < MTREW && mtop->mt_count <= 0)
			return (EINVAL);
		switch (mtop->mt_op) {

		case MTWEOF:
			code = 0;
			count = mtop->mt_count;
			break;

		case MTFSF:
			code = 1;
			count = mtop->mt_count;
			break;

		case MTBSF:
			code = 1;
			count = -mtop->mt_count;
			break;

		case MTFSR:
			code = 0;
			break;

		case MTBSR:
			code = 0;
			count = -mtop->mt_count;
			break;

		case MTREW:
		case MTOFFL:
		case MTNOP:
			code = 0;
			count = 0;
			break;

		default:
			return (EINVAL);
		}
		return (tzcommand(dev, tzops[mtop->mt_op], code, count));

	case MTIOCGET:
		mtget = (struct mtget *)data;
		mtget->mt_dsreg = 0;
		mtget->mt_erreg = sc->sc_sense.status;
		mtget->mt_resid = 0;
		mtget->mt_type = 0;
		break;

	default:
		return (EINVAL);
	}
	return (0);
}

void
tzstrategy(bp)
	register struct buf *bp;
{
	register int unit = tzunit(bp->b_dev);
	register struct tz_softc *sc = &tz_softc[unit];
	register struct buf *dp;
	register int s;

	bp->av_forw = NULL;
	dp = &sc->sc_tab;
	s = splbio();
	if (dp->b_actf == NULL)
		dp->b_actf = bp;
	else
		dp->b_actl->av_forw = bp;
	dp->b_actl = bp;
	if (dp->b_active == 0) {
		dp->b_active = 1;
		tzstart(unit);
	}
	splx(s);
}
#endif
