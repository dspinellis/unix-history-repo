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
 *	@(#)st.c	7.6 (Berkeley) %G%
 */

/*
 * st.c -- TEAC MT-2ST/N60 SCSI TAPE UNIT Device Driver
 * remaked by A.Fujita, MAR-22-1992
 */

/*
 * SCSI CCS (Command Command Set) disk driver.
 */
#include "st.h"
#if NST > 0

#include <sys/param.h>
#include <sys/systm.h>
#include <sys/buf.h>
#include <sys/file.h>
#include <sys/proc.h>
#include <sys/mtio.h>
#include <sys/tprintf.h>

#include <luna68k/dev/device.h>
#include <luna68k/dev/scsireg.h>
#include <luna68k/dev/scsivar.h>

extern int scsi_test_unit_rdy();
extern int scsi_request_sense();
extern int scsi_immed_command();
extern char *scsi_status();

extern int scgo();
extern void scfree();

char *sense_key();

int	stinit(), ststrategy(), ststart(), stintr();

struct	driver stdriver = {
	stinit, "st", ststart, (int (*)()) 0, stintr, (int (*)()) 0
};

struct	st_softc {
	struct	hp_device *sc_hd;
	struct	scsi_queue sc_dq;
	int	sc_flags;
	short	sc_type;	/* drive type */
	short	sc_punit;	/* physical unit (scsi lun) */
	tpr_t	sc_ctty;
} st_softc[NST];

/* softc flags */
#define STF_ALIVE	0x0001
#define STF_OPEN	0x0002
#define STF_WMODE	0x0004
#define STF_WRTTN	0x0008
#define STF_CMD		0x0010
#define STF_LEOT	0x0020
#define STF_MOVED	0x0040

u_char xsense_buff[60];

struct scsi_fmt_cdb st_read_cmd  = { 6, CMD_READ  };
struct scsi_fmt_cdb st_write_cmd = { 6, CMD_WRITE };

struct buf sttab[NST];
struct buf stbuf[NST];

#define	stunit(x)	(minor(x) & 3)
#define	stpunit(x)	((x) & 7)

#define STDEV_NOREWIND	0x04

#define	STRETRY		2	/* IO retry count */

struct st_iostat {
	int imax;
	int imin;
	int omax;
	int omin;
};

struct st_iostat st_iostat[NST];


/*
 * Initialize
 */

int
stinit(hd)
	register struct hp_device *hd;
{
	register struct st_softc *sc = &st_softc[hd->hp_unit];
	register struct buf *bp;

	for (bp = sttab; bp < &sttab[NST]; bp++)
		bp->b_actb = &bp->b_actf;
	sc->sc_hd = hd;
	sc->sc_punit = stpunit(hd->hp_flags);
	sc->sc_type = stident(sc, hd);
	if (sc->sc_type < 0)
		return(0);
	sc->sc_dq.dq_ctlr = hd->hp_ctlr;
	sc->sc_dq.dq_unit = hd->hp_unit;
	sc->sc_dq.dq_slave = hd->hp_slave;
	sc->sc_dq.dq_driver = &stdriver;
	sc->sc_flags = STF_ALIVE;
	return(1);
}

static struct scsi_inquiry inqbuf;
static struct scsi_fmt_cdb inq = {
	6,
	CMD_INQUIRY, 0, 0, 0, sizeof(inqbuf), 0
};

int
stident(sc, hd)
	struct st_softc *sc;
	struct hp_device *hd;
{
	char idstr[32];
	int unit;
	register int ctlr, slave;
	register int i, stat;
	register int tries = 10;

	ctlr = hd->hp_ctlr;
	slave = hd->hp_slave;
	unit = sc->sc_punit;

	/*
	 * See if unit exists and is a disk then read block size & nblocks.
	 */
	while ((stat = scsi_immed_command(ctlr, slave, unit,
				  &inq, (u_char *)&inqbuf, sizeof(inqbuf))) != 0) {
		if (stat < 0 || --tries < 0)
			return (-1);
		DELAY(1000);
	}

	switch (inqbuf.type) {
	case 1:		/* tape */
		break;
	default:	/* not a disk */
		printf("stident: inqbuf.type = %d\n", inqbuf.type);
		return (-1);
	}

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
	printf("st%d: %s %s rev %s\n", hd->hp_unit, idstr, &idstr[8],
	       &idstr[24]);

	return(inqbuf.type);
}


/*
 * Open
 */

int
stopen(dev, flag, type, p)
	dev_t dev;
	int flag, type;
	struct proc *p;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_xsense *sp = (struct scsi_xsense *) xsense_buff;
	int ctlr  = sc->sc_dq.dq_ctlr;
	int slave = sc->sc_dq.dq_slave;
	int stat, retry = 9;

	if (unit >= NST || (sc->sc_flags & STF_ALIVE) == 0)
		return(-1);
	if (sc->sc_flags & STF_OPEN)
		return(-1);

	sc->sc_ctty = tprintf_open(p);

	/* drive ready ? */
	while ((stat = scsi_test_unit_rdy(ctlr, slave, 0)) != 0) {
		scsi_request_sense(ctlr, slave, 0, sp, 8);

		if (stat != STS_CHECKCOND) {
			tprintf(sc->sc_ctty,
				"st%d:[stopen]   %s\n", unit, scsi_status(stat));
			tprintf_close(sc->sc_ctty);
			return(EIO);
		}

		if (retry-- < 0) {
			tprintf(sc->sc_ctty,
				"st%d:[stopen]   %s\n", unit, sense_key(sp->key));
			tprintf_close(sc->sc_ctty);
			return(EIO);
		}

		DELAY(1000000);
	}

	sc->sc_flags |= STF_OPEN;
	if (flag & FWRITE)
		sc->sc_flags |= STF_WMODE;
	sc->sc_flags &= ~STF_MOVED;

	return(0);
}

/*ARGSUSED*/
stclose(dev)
	dev_t dev;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_xsense *sp = (struct scsi_xsense *) xsense_buff;
	int ctlr  = sc->sc_hd->hp_ctlr;
	int slave = sc->sc_hd->hp_slave;
	int stat, retry = 9;

	if ((sc->sc_flags & (STF_WMODE|STF_WRTTN)) == (STF_WMODE|STF_WRTTN)) {
		st_write_EOF(dev);
	}

	if ((minor(dev) & STDEV_NOREWIND) == 0) {
		st_rewind(dev);
	}

	sc->sc_flags &= ~(STF_OPEN|STF_WMODE|STF_WRTTN);

	tprintf_close(sc->sc_ctty);

	return(0);
}

/*
 * Strategy
 */

int
ststrategy(bp)
	register struct buf *bp;
{
	register int unit = stunit(bp->b_dev);
	register struct buf *dp = &sttab[unit];
	int s;

	bp->b_actf = NULL;
	s = splbio();
	bp->b_actb = dp->b_actb;
	*dp->b_actb = bp;
	dp->b_actb = &bp->b_actf;
	if (dp->b_active == 0) {
		dp->b_active = 1;
		stustart(unit);
	}

	splx(s);
}

int
stustart(unit)
	register int unit;
{
	register struct st_softc *sc = &st_softc[unit];
	register struct hp_device *hp = sc->sc_hd;
	register struct scsi_queue *dq = &sc->sc_dq;
	register struct buf *dp, *bp = sttab[unit].b_actf;
	register struct scsi_fmt_cdb *cmd;
	long nblks;

	cmd = bp->b_flags & B_READ ? &st_read_cmd : &st_write_cmd;
	cmd->cdb[1] = 1;		/* unknown setup */

	if (bp->b_flags & B_READ)
		sc->sc_flags &= ~STF_WRTTN;
	else
		sc->sc_flags |= STF_WRTTN;

	nblks = bp->b_bcount >> DEV_BSHIFT;

	if (bp->b_bcount % DEV_BSIZE) {
		tprintf(sc->sc_ctty,
			"st%d:[stustart] I/O not block aligned %d/%ld\n",
			unit, DEV_BSIZE, bp->b_bcount);

		bp->b_flags |= B_ERROR;
		bp->b_error = EIO;
		
		sttab[unit].b_errcnt = 0;
		if (dp = bp->b_actf)
			dp->b_actb = bp->b_actb;
		else
			sttab[unit].b_actb = bp->b_actb;
		*bp->b_actb = dp;
		bp->b_resid = 0;
		
		biodone(bp);
		
		if (sttab[unit].b_actf) {
			stustart(unit);
		} else {
			sttab[unit].b_active = 0;
		}
	}

	*(u_char *)(&cmd->cdb[2]) = (u_char) (nblks >> 16);
	*(u_char *)(&cmd->cdb[3]) = (u_char) (nblks >>  8);
	*(u_char *)(&cmd->cdb[4]) = (u_char)  nblks; 

	cmd->cdb[5] = 0;		/* unknown setup */

	sc->sc_flags |= STF_MOVED;

	dq->dq_cdb = cmd;
	dq->dq_bp  = bp;
	dq->dq_flags = 0;		/* No Disconnect */

	if (screq(dq))
		ststart(unit);
}

int
ststart(unit)
	register int unit;
{
	register struct st_softc *sc = &st_softc[unit];
	register struct hp_device *hp = sc->sc_hd;

	scstart(hp->hp_ctlr);
}

/*
 * Interrupt
 */

char *
sense_key(key)
	int key;
{
	if (key == 0)
		return("No Sense");
	else if (key == 2)
		return("Not Ready");
	else if (key == 3)
		return("Medium Error");
	else if (key == 4)
		return("Hardware Error");
	else if (key == 5)
		return("Illegal Request");
	else if (key == 6)
		return("Unit Attention");
	else if (key == 7)
		return("Data Protect");
	else if (key == 8)
		return("No Data");
	else if (key == 11)
		return("Aborted Command");
	else if (key == 13)
		return("Volume Overflow");
	else
		return("Unknown Error");
}

int
stintr(unit, stat)
	register int unit;
	int stat;
{
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_xsense *xp = (struct scsi_xsense *) xsense_buff;
	register struct scsi_queue *dq = &sc->sc_dq;
	register struct buf *dp, *bp = dq->dq_bp;
	int ctlr  = dq->dq_ctlr;
	int slave = dq->dq_slave;

	if (bp->b_flags & B_READ) {
		st_iostat[unit].imin = min(dq->dq_imin, st_iostat[unit].imin);
		if (dq->dq_imax > st_iostat[unit].imax) {
			st_iostat[unit].imax = dq->dq_imax;
#ifdef ST_IOSTAT
			printf("stintr: st%d  INPUT	MAX = %d, MIN = %d\n",
			       unit, st_iostat[unit].imax, st_iostat[unit].imin);
#endif
		}
	} else {
		st_iostat[unit].omin = min(dq->dq_omin, st_iostat[unit].omin);
		if (dq->dq_omax > st_iostat[unit].omax) {
			st_iostat[unit].omax = dq->dq_omax;
#ifdef ST_IOSTAT
			printf("stintr: st%d  OUTPUT	MAX = %d, MIN = %d\n",
			       unit, st_iostat[unit].omax, st_iostat[unit].omin);
#endif
		}
	}
	if (stat < 0) {
		bp->b_flags |= B_ERROR;
		bp->b_error = EIO;
		goto done;
	}

	switch (stat) {
	/* scsi command completed ok */
	case 0:
		bp->b_resid = 0;
		break;

	/* more status */
	case STS_CHECKCOND:
		scsi_request_sense(ctlr, slave, 0, xp, 8);
#ifdef DEBUG
		printf("stintr: xsense_buff[0] = 0x%s\n", hexstr(xsense_buff[0], 2));
		printf("stintr: xsense_buff[2] = 0x%s\n", hexstr(xsense_buff[2], 2));
		printf("stintr: Sense Key = [%s]\n", sense_key(xp->key));
#endif
		if (xp->valid) {
			bp->b_resid = (u_long)((xp->info1 << 24) |
					       (xp->info2 << 16) |
					       (xp->info3 << 8) |
					       (xp->info4));
			bp->b_resid <<= DEV_BSHIFT;
		}

		if (xp->filemark) {		/* End of File */
/*
			tprintf(sc->sc_ctty, "st%d:[stintr]   End of File\n", unit);
			bp->b_flags |= B_ERROR;
			bp->b_error = EIO;
 */
			break;
		}

		if (xp->key) {
			tprintf(sc->sc_ctty, "st%d:[stintr]   %s\n", unit, sense_key(xp->key));
			bp->b_flags |= B_ERROR;
			bp->b_error = EIO;
			break;
		}

		if (xp->eom) {		/* End of TAPE */
			tprintf(sc->sc_ctty, "st%d:[stintr]   End of Tape\n", unit);
			bp->b_flags |= B_ERROR;
			bp->b_error = ENOSPC;
			break;
		}

		tprintf(sc->sc_ctty, "st%d:[stintr]   unknown scsi error\n", unit);
		bp->b_flags |= B_ERROR;
		bp->b_error = EIO;
		break;

	default:
		tprintf(sc->sc_ctty, "st%d:[stintr]   stintr unknown stat 0x%x\n", unit, stat);
		break;
	}

done:
	sttab[unit].b_errcnt = 0;
	if (dp = bp->b_actf)
		dp->b_actb = bp->b_actb;
	else
		sttab[unit].b_actb = bp->b_actb;
	*bp->b_actb = dp;
	bp->b_resid = 0;

	biodone(bp);

	scfree(&sc->sc_dq);

	if (sttab[unit].b_actf) {
		stustart(unit);
	} else {
		sttab[unit].b_active = 0;
	}
}


/*
 * RAW Device Routines
 */


stread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int unit = stunit(dev);

	return(physio(ststrategy, &stbuf[unit], dev, B_READ, minphys, uio));
}

stwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	int unit = stunit(dev);

	return(physio(ststrategy, &stbuf[unit], dev, B_WRITE, minphys, uio));
}

int
stioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
	return(ENXIO);
}

struct scsi_fmt_cdb st_cmd;

st_rewind(dev)
	dev_t dev;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_fmt_cdb *cdb = &st_cmd;
	register struct scsi_xsense *sp = (struct scsi_xsense *) xsense_buff;
	int ctlr, slave, stat;
	int retry = 9;

	ctlr  = sc->sc_hd->hp_ctlr;
	slave = sc->sc_hd->hp_slave;

	cdb->len = 6;

	cdb->cdb[0] = CMD_REWIND;

	cdb->cdb[1] = 1;	/* command finished soon */

	cdb->cdb[2] = 0;
	cdb->cdb[3] = 0;
	cdb->cdb[4] = 0;

	cdb->cdb[5] = 0;		/* unknown setup */

 rewind:
	stat = scsi_immed_command(ctlr, slave, 0, cdb, (char *) 0, 0);

	if (stat == 0) {
		return(1);
	} else {
		tprintf(sc->sc_ctty, "st%d:[st_rewind]   rewind error\n", unit);
		scsi_request_sense(ctlr, slave, 0, sp, 8);
		tprintf(sc->sc_ctty,
			"st%d:[st_rewind]   status = 0x%x, sens key = 0x%x\n",
			unit, stat, sp->key);

		if (retry > 0) {
			DELAY(1000000);
			retry--;
			goto rewind;
		}

		return(0);
	}
}

st_write_EOF(dev)
	dev_t dev;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_fmt_cdb *cdb = &st_cmd;
	int ctlr, slave, stat;
	int marks = 1;

	ctlr  = sc->sc_hd->hp_ctlr;
	slave = sc->sc_hd->hp_slave;

	cdb->len = 6;

	cdb->cdb[0] = CMD_WRITE_FILEMARK;

	cdb->cdb[1] = 0;

	cdb->cdb[2] = 0;
	cdb->cdb[3] = 0;
	cdb->cdb[4] = marks;

	cdb->cdb[5] = 0;		/* unknown setup */

	stat = scsi_immed_command(ctlr, slave, 0, cdb, (char *) 0, 0);

	if (stat == 0)
		return(1);

	tprintf(sc->sc_ctty, "st%d:[st_write_EOF]   write EOF error\n", unit);

	return(0);
}

/*
 * Dump
 */

int
stdump(dev)
	dev_t dev;
{
}
#endif
