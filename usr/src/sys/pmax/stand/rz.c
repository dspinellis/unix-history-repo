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
 * NOTE: The name was changed from "sd" to "rz" for DEC naming compatibility.
 * I guess I can't avoid confusion someplace.
 */
#include <pmax/stand/samachdep.h>
#if NRZ > 0

#include <stand/stand.h>
#include <sys/param.h>
#include <sys/disklabel.h>
#include <pmax/dev/device.h>
#include <pmax/dev/scsi.h>

void	rzstart(), rzdone();

struct	driver rzdriver = {
	"rz", (int (*)())0, rzstart, rzdone,
};

struct	rz_softc {
	struct	scsi_device sc_sd;	/* physical unit info */
	u_short	sc_part;		/* disk partition number */
	u_short	sc_type;		/* drive type from INQUIRY cmd */
	u_int	sc_blks;		/* number of blocks on device */
	int	sc_blksize;		/* device block size in bytes */
	int	sc_bshift;		/* convert device blocks to DEV_BSIZE */
	struct	disklabel sc_label;	/* disk label for this disk */
	struct	ScsiCmd sc_cmd;		/* command for controller */
	ScsiGroup1Cmd sc_rwcmd;		/* group 1 SCSI commands */
	struct	scsi_fmt_sense sc_sense;	/* sense data from last cmd */
	int	sc_error;		/* error code */
	int	sc_resid;		/* amount of data not transfered */
};

volatile int	done;

int
rzstrategy(devdata, rw, bn, reqcnt, addr, cnt)
	void *devdata;
	int rw;
	daddr_t bn;
	u_int reqcnt;
	char *addr;
	u_int *cnt;	/* out: number of bytes transfered */
{
	register struct rz_softc *sc = (struct rz_softc *)devdata;
	register int part = sc->sc_part;
	register struct partition *pp = &sc->sc_label.d_partitions[part];
	register int s;
	long cylin;

	sc->sc_error = 0;
	s = howmany(reqcnt, DEV_BSIZE);
	if ((unsigned)bn + s > pp->p_size) {
		s = pp->p_size - bn;
		/* if exactly at end of disk, return an EOF */
		if (s == 0) {
			*cnt = 0;
			return (0);
		}
		/* if none of it fits, error */
		if (s < 0) {
			*cnt = 0;
			return (EINVAL);
		}
		/* otherwise, truncate */
		reqcnt = dbtob(s);
	}
	/*
	 * Non-aligned or partial-block transfers not handled.
	 */
	s = sc->sc_blksize - 1;
	if ((dbtob(bn) & s) || (reqcnt & s)) {
		*cnt = 0;
		return (EINVAL);
	}
	cylin = (bn + pp->p_offset) >> sc->sc_bshift;

	sc->sc_cmd.buf = addr;
	sc->sc_cmd.buflen = reqcnt;
	sc->sc_cmd.cmd = (u_char *)&sc->sc_rwcmd;
	sc->sc_cmd.cmdlen = sizeof(sc->sc_rwcmd);

	if (rw & F_READ) {
		sc->sc_cmd.flags = 0;
		sc->sc_rwcmd.command = SCSI_READ_EXT;
	} else {
		sc->sc_cmd.flags = SCSICMD_DATA_TO_DEVICE;
		sc->sc_rwcmd.command = SCSI_WRITE_EXT;
	}
	sc->sc_rwcmd.highAddr = cylin >> 24;
	sc->sc_rwcmd.midHighAddr = cylin >> 16;
	sc->sc_rwcmd.midLowAddr = cylin >> 8;
	sc->sc_rwcmd.lowAddr = cylin;
	s = howmany(reqcnt, sc->sc_blksize);
	sc->sc_rwcmd.highBlockCount = s >> 8;
	sc->sc_rwcmd.lowBlockCount = s;

	rzstart(sc);

	*cnt = reqcnt - sc->sc_resid;
	return (sc->sc_error);
}

void
rzstart(sc)
	register struct rz_softc *sc;
{
	ScsiGroup0Cmd reqcmd;

	/* tell controller to start this command */
	done = 0;
	(*sc->sc_sd.sd_cdriver->d_start)(&sc->sc_cmd);
	while (!done)
		;

	/* check for errors that need to be processed */
	if (!(sc->sc_sense.status & SCSI_STATUS_CHECKCOND))
		return;

	/* start a REQUEST_SENSE command */
	scsiGroup0Cmd(SCSI_REQUEST_SENSE, sc->sc_sd.sd_slave, 0,
		sizeof(sc->sc_sense.sense), &reqcmd);
	sc->sc_cmd.cmdlen = sizeof(ScsiGroup0Cmd);
	sc->sc_cmd.cmd = (u_char *)&reqcmd;
	sc->sc_cmd.buf = (char *)sc->sc_sense.sense;
	sc->sc_cmd.buflen = sizeof(sc->sc_sense.sense);
	sc->sc_cmd.flags = 0;

	/* tell controller to start this command */
	done = 0;
	(*sc->sc_sd.sd_cdriver->d_start)(&sc->sc_cmd);
	while (!done)
		;

	if (sc->sc_error || (sc->sc_sense.status & SCSI_STATUS_CHECKCOND)) {
		printf("rz%d: error reading sense data: error %d scsi status 0x%x\n",
			sc->sc_sd.sd_unit, sc->sc_error, sc->sc_sense.status);
		/*
		 * We got an error during the REQUEST_SENSE,
		 * fill in no sense for data.
		 */
		sc->sc_sense.sense[0] = 0x70;
		sc->sc_sense.sense[2] = SCSI_CLASS7_NO_SENSE;
	} else {
		printf("rz%d: ", sc->sc_sd.sd_unit);
		scsiPrintSense((ScsiClass7Sense *)sc->sc_sense.sense,
			sizeof(sc->sc_sense.sense) - sc->sc_resid);
	}
	if (sc->sc_error == 0)
		sc->sc_error = EIO;
}

/*
 * This is called by the controller driver when the command is done.
 */
void
rzdone(data, error, resid, status)
	void *data;
	int error;		/* error number from errno.h */
	int resid;		/* amount not transfered */
	int status;		/* SCSI status byte */
{
	register struct rz_softc *sc = (struct rz_softc *)data;

	sc->sc_error = error;
	sc->sc_resid = resid;
	sc->sc_sense.status = status;
	done = 1;
}

int
rzopen(f, ctlr, unit, part)
	struct open_file *f;
	int ctlr, unit, part;
{
	register struct rz_softc *sc;
	register struct disklabel *lp;
	register struct scsi_device *sd;
	register int i;
	char *msg;
	ScsiGroup0Cmd inqcmd;
	ScsiInquiryData inqbuf;
	u_char capbuf[8];
	char buf[DEV_BSIZE];
	int cnt;
	struct driver *scsi_driver;

	if (unit >= NRZ || part >= MAXPARTITIONS)
		return (ENXIO);
#ifdef DS3100
	if (ctlr >= NSII)
		return (ENXIO);
	else {
		extern struct driver siidriver;

		scsi_driver = &siidriver;
	}
#endif
#ifdef DS5000
	{
	struct pmax_ctlr *cp, *tc_ctlr __P((int, int));

	cp = tc_ctlr(ctlr, TC_SCSI);
	if (!cp)
		return (ENXIO);
	ctlr = cp->pmax_unit;	/* convert from slot # to controller unit */
	scsi_driver = cp->pmax_driver;
	}
#endif

	sc = alloc(sizeof(struct rz_softc));
	bzero(sc, sizeof(struct rz_softc));
	f->f_devdata = (void *)sc;

	sc->sc_sd.sd_driver = &rzdriver;
	sc->sc_sd.sd_cdriver = scsi_driver;
	sc->sc_sd.sd_unit = unit;
	sc->sc_sd.sd_ctlr = ctlr;
	sc->sc_sd.sd_drive = unit;
	sc->sc_sd.sd_slave = 0;
	sc->sc_sd.sd_flags = 0;
	sc->sc_sd.sd_alive = 1;
	sc->sc_part = part;

	/* init fields that don't change */
	sc->sc_cmd.sd = sd = &sc->sc_sd;
	sc->sc_cmd.unit = (int)sc;

	/*
	 * Try to find out what type of device this is.
	 */
	scsiGroup0Cmd(SCSI_INQUIRY, sd->sd_slave, 0, sizeof(inqbuf), &inqcmd);
	sc->sc_cmd.cmdlen = sizeof(ScsiGroup0Cmd);
	sc->sc_cmd.cmd = (u_char *)&inqcmd;
	sc->sc_cmd.buflen = sizeof(inqbuf);
	sc->sc_cmd.buf = (char *)&inqbuf;
	sc->sc_cmd.flags = 0;

	/* tell controller to start this command */
	rzstart(sc);

	if (sc->sc_error || (i = sizeof(inqbuf) - sc->sc_resid) < 5)
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

	/* find out how big a disk this is */
	scsiGroup1Cmd(SCSI_READ_CAPACITY, sd->sd_slave, 0, 0, &sc->sc_rwcmd);
	sc->sc_cmd.cmdlen = sizeof(ScsiGroup1Cmd);
	sc->sc_cmd.cmd = (u_char *)&sc->sc_rwcmd;
	sc->sc_cmd.buflen = sizeof(capbuf);
	sc->sc_cmd.buf = (char *)capbuf;
	sc->sc_cmd.flags = 0;

	rzstart(sc);

	if (sc->sc_error || sc->sc_resid != 0)
		goto bad;
	sc->sc_blks = ((capbuf[0] << 24) | (capbuf[1] << 16) |
		(capbuf[2] << 8) | capbuf[3]) + 1;
	sc->sc_blksize = (capbuf[4] << 24) | (capbuf[5] << 16) |
		(capbuf[6] << 8) | capbuf[7];

#if 0
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
#endif

	if (sc->sc_blksize != DEV_BSIZE) {
		if (sc->sc_blksize < DEV_BSIZE) {
			printf("rz%d: need %d byte blocks - drive ignored\n",
				unit, DEV_BSIZE);
			goto bad;
		}
		for (i = sc->sc_blksize; i > DEV_BSIZE; i >>= 1)
			++sc->sc_bshift;
		sc->sc_blks <<= sc->sc_bshift;
	}

	/* try to read disk label and partition table information */
	lp = &sc->sc_label;
	lp->d_secsize = DEV_BSIZE;
	lp->d_secpercyl = 1 << sc->sc_bshift;
	lp->d_npartitions = MAXPARTITIONS;
	lp->d_partitions[part].p_offset = 0;
	lp->d_partitions[part].p_size = sc->sc_blks;
	i = rzstrategy(sc, F_READ, (daddr_t)LABELSECTOR, DEV_BSIZE, buf, &cnt);
	if (i || cnt != DEV_BSIZE) {
		printf("rz%d: error reading disk label\n", unit);
		goto bad;
	} else {
		msg = getdisklabel(buf, lp);
		if (msg) {
			printf("rz%d: %s\n", unit, msg);
			goto bad;
		}
	}

	if (part >= lp->d_npartitions || lp->d_partitions[part].p_size == 0) {
	bad:
		free(sc, sizeof(struct rz_softc));
		return (ENXIO);
	}
	return (0);
}

rzclose(f)
	struct open_file *f;
{
	free(f->f_devdata, sizeof(struct rz_softc));
	f->f_devdata = (void *)0;
	return (0);
}
#endif
