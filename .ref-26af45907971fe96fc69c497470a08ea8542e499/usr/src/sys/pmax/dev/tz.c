/*
 * Copyright (c) 1992 Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Ralph Campbell.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)tz.c	7.9 (Berkeley) %G%
 *
 * from: $Header: /sprite/src/kernel/dev/RCS/devSCSITape.c,
 *	v 8.14 89/07/31 17:26:13 mendel Exp $ SPRITE (Berkeley)
 */

/*
 * SCSI CCS (Command Command Set) tape driver.
 */
#include "tz.h"
#if NTZ > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/errno.h>
#include <sys/file.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/syslog.h>
#include <sys/tprintf.h>

#include <pmax/dev/device.h>
#include <pmax/dev/scsi.h>

int	tzprobe();
void	tzstart(), tzdone();

struct	driver tzdriver = {
	"tz", tzprobe, tzstart, tzdone,
};

struct	tz_softc {
	struct	scsi_device *sc_sd;	/* physical unit info */
	int	sc_flags;		/* see below */
	int	sc_tapeid;		/* tape drive id */
	int	sc_blklen;		/* 0 = variable len records */
	long	sc_numblks;		/* number of blocks on tape */
	tpr_t	sc_ctty;		/* terminal for error messages */
	struct	buf sc_tab;		/* queue of pending operations */
	struct	buf sc_buf;		/* buf for doing I/O */
	struct	buf sc_errbuf;		/* buf for doing REQUEST_SENSE */
	struct	ScsiCmd sc_cmd;		/* command for controller */
	ScsiGroup0Cmd sc_rwcmd;		/* SCSI cmd for read/write */
	struct	scsi_fmt_cdb sc_cdb;	/* SCSI cmd if not read/write */
	struct	scsi_fmt_sense sc_sense;	/* sense data from last cmd */
	struct	ScsiTapeModeSelectHdr sc_mode;	/* SCSI_MODE_SENSE data */
	char	sc_modelen;		/* SCSI_MODE_SENSE data length */
} tz_softc[NTZ];

/* sc_flags values */
#define	TZF_ALIVE		0x01	/* drive found and ready */
#define	TZF_SENSEINPROGRESS	0x02	/* REQUEST_SENSE command in progress */
#define	TZF_ALTCMD		0x04	/* alternate command in progress */
#define	TZF_WRITTEN		0x08	/* tape has been written to */
#define	TZF_OPEN		0x10	/* device is open */
#define	TZF_WAIT		0x20	/* waiting for sc_tab to drain */
#define TZF_SEENEOF		0x40	/* seen file mark on read */

/* bits in minor device */
#define	tzunit(x)	(minor(x) >> 4)	/* tz%d unit number */
#define TZ_NOREWIND	0x01		/* don't rewind on close */
#define TZ_HIDENSITY	0x02
#define TZ_EXSFMK	0x04
#define TZ_FIXEDBLK	0x08

#ifdef DEBUG
int	tzdebug = 0;
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

	/* init some parameters that don't change */
	sc->sc_sd = sd;
	sc->sc_cmd.sd = sd;
	sc->sc_cmd.unit = sd->sd_unit;
	sc->sc_cmd.flags = 0;
	sc->sc_rwcmd.unitNumber = sd->sd_slave;

	/* try to find out what type of device this is */
	sc->sc_flags = TZF_ALTCMD;	/* force use of sc_cdb */
	sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
	scsiGroup0Cmd(SCSI_INQUIRY, sd->sd_slave, 0, sizeof(inqbuf),
		(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
	sc->sc_buf.b_flags = B_BUSY | B_READ;
	sc->sc_buf.b_bcount = sizeof(inqbuf);
	sc->sc_buf.b_un.b_addr = (caddr_t)&inqbuf;
	sc->sc_buf.b_actf = (struct buf *)0;
	sc->sc_buf.b_actb = &sc->sc_tab.b_actf;
	sc->sc_tab.b_actf = &sc->sc_buf;
	sc->sc_tab.b_actb = &sc->sc_buf.b_actf;
	tzstart(sd->sd_unit);
	if (biowait(&sc->sc_buf) ||
	    (i = sizeof(inqbuf) - sc->sc_buf.b_resid) < 5)
		goto bad;
	if (inqbuf.type != SCSI_TAPE_TYPE || !inqbuf.rmb)
		goto bad;

	/* check for device ready to clear UNIT_ATTN */
	sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
	scsiGroup0Cmd(SCSI_TEST_UNIT_READY, sd->sd_slave, 0, 0,
		(ScsiGroup0Cmd *)sc->sc_cdb.cdb);
	sc->sc_buf.b_flags = B_BUSY | B_READ;
	sc->sc_buf.b_bcount = 0;
	sc->sc_buf.b_un.b_addr = (caddr_t)0;
	sc->sc_buf.b_actf = (struct buf *)0;
	sc->sc_buf.b_actb = &sc->sc_tab.b_actf;
	sc->sc_tab.b_actf = &sc->sc_buf;
	sc->sc_tab.b_actb = &sc->sc_buf.b_actf;
	tzstart(sd->sd_unit);
	(void) biowait(&sc->sc_buf);

	sc->sc_flags = TZF_ALIVE;
	sc->sc_modelen = 12;
	sc->sc_buf.b_flags = 0;
	printf("tz%d at %s%d drive %d slave %d", sd->sd_unit,
		sd->sd_cdriver->d_name, sd->sd_ctlr, sd->sd_drive,
		sd->sd_slave);
	if (i == 5 && inqbuf.version == 1 && inqbuf.qualifier == 0x50) {
		printf(" TK50\n");
		sc->sc_tapeid = MT_ISTK50;
	} else if (i == 5 && inqbuf.version == 1 && inqbuf.qualifier == 0) {
		/* assume Emultex MT02 controller */
		printf(" MT02\n");
		sc->sc_tapeid = MT_ISMT02;
	} else if (inqbuf.version > 2 || i < 36) {
		printf(" GENERIC SCSI tape device: qual 0x%x, ver %d\n",
			inqbuf.qualifier, inqbuf.version);
		sc->sc_tapeid = 0;
	} else {
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
		printf(" %s %s rev %s\n", vid, pid, revl);

		if (bcmp("EXB-8200", pid, 8) == 0) {
			sc->sc_tapeid = MT_ISEXABYTE;
			sc->sc_modelen = 17;
		} else if (bcmp("VIPER 150", pid, 9) == 0) {
			sc->sc_tapeid = MT_ISVIPER1;
		} else if (bcmp("Python 25501", pid, 12) == 0) {
			sc->sc_tapeid = MT_ISPYTHON;
		} else if (bcmp("HP35450A", pid, 8) == 0) {
#if 0
			/* XXX "extra" stat makes the HP drive happy at boot time */
			stat = scsi_test_unit_rdy(ctlr, slave, unit);
#endif
			sc->sc_tapeid = MT_ISHPDAT;
		} else if (bcmp("123107 SCSI", pid, 11) == 0) {
			sc->sc_tapeid = MT_ISMFOUR;
		} else {
			printf("tz%d: assuming GENERIC SCSI tape device\n",
				sd->sd_unit,
				inqbuf.type, inqbuf.qualifier, inqbuf.version);
			sc->sc_tapeid = 0;
		}
	}
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
tzcommand(dev, command, code, count, data)
	dev_t dev;
	int command;
	int code;
	int count;
	caddr_t data;
{
	register struct tz_softc *sc = &tz_softc[tzunit(dev)];
	register ScsiGroup0Cmd *c;
	int s, error;

	s = splbio();
	/* wait for pending operations to finish */
	while (sc->sc_tab.b_actf) {
		sc->sc_flags |= TZF_WAIT;
		sleep(&sc->sc_flags, PZERO);
	}
	sc->sc_flags |= TZF_ALTCMD;	/* force use of sc_cdb */
	sc->sc_cdb.len = sizeof(ScsiGroup0Cmd);
	c = (ScsiGroup0Cmd *)sc->sc_cdb.cdb;
	c->command = command;
	c->unitNumber = sc->sc_sd->sd_slave;
	c->highAddr = code;
	c->midAddr = count >> 16;
	c->lowAddr = count >> 8;
	c->blockCount = count;
	c->control = 0;
	if (command == SCSI_MODE_SELECT)
		sc->sc_buf.b_flags = B_BUSY;
	else {
		sc->sc_buf.b_flags = B_BUSY | B_READ;
#if 0
		/* this seems to work but doesn't give us a speed advantage */
		if (command == SCSI_TEST_UNIT_READY)
			sc->sc_cmd.flags |= SCSICMD_USE_SYNC;
#endif
	}
	sc->sc_buf.b_bcount = data ? count : 0;
	sc->sc_buf.b_un.b_addr = data;
	sc->sc_buf.b_actf = (struct buf *)0;
	sc->sc_buf.b_actb = &sc->sc_tab.b_actf;
	sc->sc_tab.b_actf = &sc->sc_buf;
	sc->sc_tab.b_actb = &sc->sc_buf.b_actf;
	tzstart(sc->sc_sd->sd_unit);
	error = biowait(&sc->sc_buf);
	sc->sc_flags &= ~TZF_ALTCMD;	/* force use of sc_cdb */
	sc->sc_buf.b_flags = 0;
	sc->sc_cmd.flags = 0;
	if (sc->sc_buf.b_resid)
		printf("tzcommand: resid %d\n", sc->sc_buf.b_resid); /* XXX */
	if (error == 0)
		switch (command) {
		case SCSI_SPACE:
		case SCSI_WRITE_EOF:
		case SCSI_REWIND:
			sc->sc_flags &= ~TZF_SEENEOF;
		}
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

	sc->sc_cmd.buf = bp->b_un.b_addr;
	sc->sc_cmd.buflen = bp->b_bcount;

	if (sc->sc_flags & (TZF_SENSEINPROGRESS | TZF_ALTCMD)) {
		if (bp->b_flags & B_READ)
			sc->sc_cmd.flags &= ~SCSICMD_DATA_TO_DEVICE;
		else
			sc->sc_cmd.flags |= SCSICMD_DATA_TO_DEVICE;
		sc->sc_cmd.cmd = sc->sc_cdb.cdb;
		sc->sc_cmd.cmdlen = sc->sc_cdb.len;
	} else {
		if (bp->b_flags & B_READ) {
			sc->sc_cmd.flags = 0;
			sc->sc_rwcmd.command = SCSI_READ;
			sc->sc_flags &= ~TZF_WRITTEN;
		} else {
			sc->sc_cmd.flags = SCSICMD_DATA_TO_DEVICE;
			sc->sc_rwcmd.command = SCSI_WRITE;
			sc->sc_flags |= TZF_WRITTEN;
		}
		sc->sc_cmd.cmd = (u_char *)&sc->sc_rwcmd;
		sc->sc_cmd.cmdlen = sizeof(sc->sc_rwcmd);
		if (sc->sc_blklen) {
			/* fixed sized records */
			n = bp->b_bcount / sc->sc_blklen;
			if (bp->b_bcount % sc->sc_blklen) {
				tprintf(sc->sc_ctty,
					"tz%d: I/O not block aligned %d/%ld\n",
					unit, sc->sc_blklen, bp->b_bcount);
				tzdone(unit, EIO, bp->b_bcount, 0);
			}
			sc->sc_rwcmd.highAddr = 1;
		} else {
			/* variable sized records */
			n = bp->b_bcount;
			sc->sc_rwcmd.highAddr = 0;
		}
		sc->sc_rwcmd.midAddr = n >> 16;
		sc->sc_rwcmd.lowAddr = n >> 8;
		sc->sc_rwcmd.blockCount = n;
	}

	/* tell controller to start this command */
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
	register struct buf *dp;
	extern int cold;

	if (bp == NULL) {
		printf("tz%d: bp == NULL\n", unit);
		return;
	}
	if (sc->sc_flags & TZF_SENSEINPROGRESS) {
		sc->sc_flags &= ~TZF_SENSEINPROGRESS;
		*bp->b_actb = dp = bp->b_actf;	/* remove sc_errbuf */
#ifdef DIAGNOSTIC
		if (!dp)
			panic("tzdone");
#endif
		dp->b_actb = bp->b_actb;
		bp = dp;

		if (error || (status & SCSI_STATUS_CHECKCOND)) {
			printf("tz%d: error reading sense data: error %d scsi status 0x%x\n",
				unit, error, status);
			/*
			 * We got an error during the REQUEST_SENSE,
			 * fill in no sense for data.
			 */
			sc->sc_sense.sense[0] = 0x70;
			sc->sc_sense.sense[2] = SCSI_CLASS7_NO_SENSE;
		} else if (!cold) {
			ScsiClass7Sense *sp;
			long resid;

			sp = (ScsiClass7Sense *)sc->sc_sense.sense;
			if (sp->error7 != 0x70)
				goto prerr;
			if (sp->valid) {
				resid = (sp->info1 << 24) | (sp->info2 << 16) |
					(sp->info3 << 8) | sp->info4;
				if (sc->sc_blklen)
					resid *= sc->sc_blklen;
			} else
				resid = 0;
			switch (sp->key) {
			case SCSI_CLASS7_NO_SENSE:
				/*
				 * Hit a filemark, end of media, or
				 * end of record.
				 * Fixed length blocks, an error.
				 */
				if (sp->endOfMedia) {
					bp->b_error = ENOSPC;
					bp->b_resid = resid;
					break;
				}
				if (sc->sc_blklen && sp->badBlockLen) {
					tprintf(sc->sc_ctty,
						"tz%d: Incorrect Block Length, expected %d got %d\n",
						unit, sc->sc_blklen, resid);
					break;
				}
				if (resid < 0) {
					/*
					 * Variable length records but
					 * attempted to read less than a
					 * full record.
					 */
					tprintf(sc->sc_ctty,
						"tz%d: Partial Read of Variable Length Tape Block, expected %d read %d\n",
						unit, bp->b_bcount - resid,
						bp->b_bcount);
					bp->b_resid = 0;
					break;
				}
				if (sp->fileMark)
					sc->sc_flags |= TZF_SEENEOF;
				/*
				 * Attempting to read more than a record is
				 * OK. Just record how much was actually read.
				 */
				bp->b_flags &= ~B_ERROR;
				bp->b_error = 0;
				bp->b_resid = resid;
				break;

			case SCSI_CLASS7_UNIT_ATTN:
				if (!(sc->sc_flags & TZF_OPEN))
					break;

			default:
			prerr:
				tprintf(sc->sc_ctty, "tz%d: ", unit);
				scsiPrintSense((ScsiClass7Sense *)
					sc->sc_sense.sense,
					sizeof(sc->sc_sense.sense) - resid);
			}
		}
	} else if (error || (status & SCSI_STATUS_CHECKCOND)) {
#ifdef DEBUG
		if (!cold && tzdebug)
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
			sc->sc_errbuf.b_flags = B_BUSY | B_PHYS | B_READ;
			sc->sc_errbuf.b_bcount = sizeof(sc->sc_sense.sense);
			sc->sc_errbuf.b_un.b_addr = (caddr_t)sc->sc_sense.sense;
			sc->sc_errbuf.b_actf = bp;
			sc->sc_errbuf.b_actb = bp->b_actb;
			*bp->b_actb = &sc->sc_errbuf;
			bp->b_actb = &sc->sc_errbuf.b_actf;
			tzstart(unit);
			return;
		}
	} else {
		sc->sc_sense.status = status;
		bp->b_resid = resid;
	}

	if (dp = bp->b_actf)
		dp->b_actb = bp->b_actb;
	else
		sc->sc_tab.b_actb = bp->b_actb;
	*bp->b_actb = dp;
	biodone(bp);
	if (sc->sc_tab.b_actf)
		tzstart(unit);
	else {
		sc->sc_tab.b_active = 0;
		if (sc->sc_flags & TZF_WAIT) {
			sc->sc_flags &= ~TZF_WAIT;
			wakeup(&sc->sc_flags);
		}
	}
}

/* ARGSUSED */
tzopen(dev, flags, type, p)
	dev_t dev;
	int flags, type;
	struct proc *p;
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
	error = tzcommand(dev, SCSI_TEST_UNIT_READY, 0, 0, 0);
	while (error) {
		ScsiClass7Sense *sp = (ScsiClass7Sense *)sc->sc_sense.sense;

		/* return error if last error was not UNIT_ATTENTION */
		if (!(sc->sc_sense.status & SCSI_STATUS_CHECKCOND) ||
		    sp->error7 != 0x70 || sp->key != SCSI_CLASS7_UNIT_ATTN)
			return (error);

		/*
		 * Try it again just to be sure and
		 * try to negotiate synchonous transfers.
		 */
		error = tzcommand(dev, SCSI_TEST_UNIT_READY, 0, 0, 0);
	}

	/* get the current mode settings */
	error = tzcommand(dev, SCSI_MODE_SENSE, 0,
		sc->sc_modelen, (caddr_t)&sc->sc_mode);
	if (error)
		return (error);

	/* check for write protected tape */
	if ((flags & FWRITE) && sc->sc_mode.writeprot) {
		uprintf("tz%d: write protected\n", unit);
		return (EACCES);
	}

	/* set record length */
	switch (sc->sc_tapeid) {
	case MT_ISAR:
	case MT_ISHPDAT:
	case MT_ISVIPER1:
		sc->sc_blklen = 512;
		break;

	case MT_ISEXABYTE:
#if 0
		if (minor(dev) & TZ_FIXEDBLK)
			sc->sc_blklen = 1024;
		else
			sc->sc_blklen = st_exblklen;
#endif
		break;

	case MT_ISPYTHON:
	case MT_ISMFOUR:
	case MT_ISTK50:
		sc->sc_blklen = 0;
		break;

	default:
		sc->sc_blklen = (sc->sc_mode.block_size2 << 16) |
			(sc->sc_mode.block_size1 << 8) | sc->sc_mode.block_size0;
	}

	/* save total number of blocks on tape */
	sc->sc_numblks = (sc->sc_mode.blocks_2 << 16) |
		(sc->sc_mode.blocks_1 << 8) | sc->sc_mode.blocks_0;

	/* setup for mode select */
	sc->sc_mode.len = 0;
	sc->sc_mode.media = 0;
	sc->sc_mode.bufferedMode = 1;
	sc->sc_mode.blocks_0 = 0;
	sc->sc_mode.blocks_1 = 0;
	sc->sc_mode.blocks_2 = 0;
	sc->sc_mode.block_size0 = sc->sc_blklen >> 16;
	sc->sc_mode.block_size1 = sc->sc_blklen >> 8;
	sc->sc_mode.block_size2 = sc->sc_blklen;

	/* check for tape density changes */
	switch (sc->sc_tapeid) {
	case MT_ISAR:
		if (minor(dev) & TZ_HIDENSITY)
			sc->sc_mode.density = 0x5;
		else {
			if (flags & FWRITE) {
				uprintf("Can only write QIC-24\n");
				return (EIO);
			}
			sc->sc_mode.density = 0x4;
		}
		break;

	case MT_ISMT02:
		/*
		 * The tape density is set automatically when the tape
		 * is loaded. We only need to change it if we are writing.
		 */
		if (flags & FWRITE) {
			if (minor(dev) & TZ_HIDENSITY)
				sc->sc_mode.density = 0;
			else
				sc->sc_mode.density = 0x4;
		}
		break;

	case MT_ISEXABYTE:
#if 0
		if (minor(dev) & TZ_HIDENSITY)
			uprintf("EXB-8200 density support only\n");
		sc->sc_mode.vupb = (u_char)st_exvup;
		sc->sc_mode.rsvd5 = 0;
		sc->sc_mode.p5 = 0;
		sc->sc_mode.motionthres = (u_char)st_exmotthr;
		sc->sc_mode.reconthres = (u_char)st_exreconthr;
		sc->sc_mode.gapthres = (u_char)st_exgapthr;
#endif
		break;

	case MT_ISHPDAT:
	case MT_ISVIPER1:
	case MT_ISPYTHON:
	case MT_ISTK50:
		if (minor(dev) & TZ_HIDENSITY)
			uprintf("tz%d: Only one density supported\n", unit);
		break;

	case MT_ISMFOUR:
		break;		/* XXX could do density select? */
	}

	/* set the current mode settings */
	error = tzcommand(dev, SCSI_MODE_SELECT, 0,
		sc->sc_modelen, (caddr_t)&sc->sc_mode);
	if (error)
		return (error);

	sc->sc_ctty = tprintf_open(p);
	sc->sc_flags = TZF_ALIVE | TZF_OPEN;
	return (0);
}

tzclose(dev, flag)
	dev_t dev;
	int flag;
{
	register struct tz_softc *sc = &tz_softc[tzunit(dev)];
	int error = 0;

	if (!(sc->sc_flags & TZF_OPEN))
		return (0);
	if (flag == FWRITE ||
	    ((flag & FWRITE) && (sc->sc_flags & TZF_WRITTEN))) {
		error = tzcommand(dev, SCSI_WRITE_EOF, 0, 1, 0);
#if 0
		/*
		 * Cartridge tapes don't do double EOFs on EOT.
		 */
		switch (sc->sc_tapeid) {
		case MT_ISAR:
		case MT_ISMT02:
			break;

		default:
			error = tzcommand(dev, SCSI_WRITE_EOF, 0, 1, 0);
			if (minor(dev) & TZ_NOREWIND)
				(void) tzcommand(dev, SCSI_SPACE, 0, -1, 0);
		}
#endif
	}
	if ((minor(dev) & TZ_NOREWIND) == 0)
		(void) tzcommand(dev, SCSI_REWIND, 0, 0, 0);
	sc->sc_flags &= ~(TZF_OPEN | TZF_WRITTEN);
	tprintf_close(sc->sc_ctty);
	return (error);
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
			count = mtop->mt_count;
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
		return (tzcommand(dev, tzops[mtop->mt_op], code, count, 0));

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

	if (sc->sc_flags & TZF_SEENEOF) {
		bp->b_resid = bp->b_bcount;
		biodone(bp);
		return;
	}
	bp->b_actf = NULL;
	dp = &sc->sc_tab;
	s = splbio();
	bp->b_actb = dp->b_actb;
	*dp->b_actb = bp;
	dp->b_actb = &bp->b_actf;
	if (dp->b_active == 0) {
		dp->b_active = 1;
		tzstart(unit);
	}
	splx(s);
}
#endif
