/*
 * Copyright (c) 1992 OMRON Corporation.
 * Copyright (c) 1992, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * OMRON Corporation.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *	@(#)st.c	8.1 (Berkeley) 6/10/93
 */

/*
 * st.c -- SCSI Disk Device Driver for LUNA-68K
 * remaked by A.Fujita, MAR-22-1992
 */

/*
 * SCSI CCS (Command Command Set) disk driver.
 */
#define NST	1

#include <sys/param.h>
#include <sys/mtio.h>
#include <luna68k/dev/scsireg.h>
#include <luna68k/stand/device.h>

extern int scsi_test_unit_rdy();
extern int scsi_request_sense();
extern int scsi_immed_command();

extern int scgo();
extern void scfree();

int	stinit(), ststrategy(), ststart(), stintr();

struct	driver stdriver = {
	stinit, "st", ststart, (int (*)()) 0, stintr, (int (*)()) 0
};

struct	st_softc {
	struct	hp_device *sc_hd;
	struct	devqueue sc_dq;
	int	sc_flags;
	short	sc_type;	/* drive type */
	short	sc_punit;	/* physical unit (scsi lun) */
} st_softc[NST];

/* softc flags */
#define STF_ALIVE	0x0001
#define STF_OPEN	0x0002
#define STF_WMODE	0x0004
#define STF_WRTTN	0x0008
#define STF_CMD		0x0010
#define STF_LEOT	0x0020
#define STF_MOVED	0x0040

struct	scsi_fmt_sense stsense[NST];

#define	stunit(x)	(minor(x) & 3)
#define	stpunit(x)	((x) & 7)

#define STDEV_NOREWIND	0x04
#define STDEV_HIDENSITY	0x08
#define STDEV_EXSFMK	0x10
#define STDEV_FIXEDBLK	0x20

#define	b_lba		b_resid

#define	STRETRY		2	/* IO retry count */


/*
 * Initialize
 */

int
stinit(hd)
	register struct hp_device *hd;
{
	register struct st_softc *sc = &st_softc[hd->hp_unit];

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

	if (stat)
		return (-1);

	switch (inqbuf.type) {
	case 1:		/* tape */
		break;
	default:	/* not a disk */
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
stopen(dev)
	dev_t dev;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];

	if (unit >= NST || (sc->sc_flags & STF_ALIVE) == 0)
		return(-1);
	if (sc->sc_flags & STF_OPEN)
		return(-1);

	sc->sc_flags |= STF_OPEN;
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

	printf("st: sc->sc_flags = 0x%s\n", hexstr(sc->sc_flags, 8));

	if ((sc->sc_flags & (STF_WMODE|STF_WRTTN)) == (STF_WMODE|STF_WRTTN)) {
		st_write_EOF(dev);
	}

	if ((minor(dev) & STDEV_NOREWIND) == 0) {
		st_rewind(dev);
	}

	sc->sc_flags &= ~(STF_OPEN|STF_WMODE|STF_WRTTN);

	return(0);
}

/*
 * Strategy
 */

int
ststrategy()
{
}

int
ststart(unit)
	register int unit;
{
}

/*
 * Interrupt
 */

/*
 * Return:
 *	0	if not really an error
 *	<0	if we should do a retry
 *	>0	if a fatal error
 */
static int
sterror(unit, sc, hp, stat)
	int unit, stat;
	register struct st_softc *sc;
	register struct hp_device *hp;
{
	int cond = 1;

	stsense[unit].status = stat;
	if (stat & STS_CHECKCOND) {
		struct scsi_xsense *sp;

		scsi_request_sense(hp->hp_ctlr, hp->hp_slave,
				   sc->sc_punit, stsense[unit].sense,
				   sizeof(stsense[unit].sense));
		sp = (struct scsi_xsense *)stsense[unit].sense;
		printf("st%d: scsi sense class %d, code %d", unit,
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

int
stintr(unit, stat)
	register int unit;
	int stat;
{
}


/*
 * RAW Device Routines
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

static struct scsi_fmt_cdb st_cmd  = { 6, 0, 0, 0, 0, 0, 0 };

u_char sensebuf[8];

int
stread(dev, buf, size)
	dev_t dev;
	char *buf;
	int   size;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_fmt_cdb *cdb = &st_cmd;
	register int nblk = size >> DEV_BSHIFT;
	struct scsi_xsense *sp = (struct scsi_xsense *)sensebuf;
	int ctlr, slave, stat;

	ctlr  = sc->sc_hd->hp_ctlr;
	slave = sc->sc_hd->hp_slave;
	
	cdb->cdb[0] = CMD_READ;
	cdb->cdb[1] = 1;		/* unknown setup */

	cdb->cdb[2] = (nblk & 0xff0000) >> 16;
	cdb->cdb[3] = (nblk & 0x00ff00) >>  8;
	cdb->cdb[4] = (nblk & 0x0000ff);

	cdb->cdb[5] = 0;		/* unknown setup */

	stat = scsi_immed_command(ctlr, slave, 0, cdb, buf, size);

	if (stat == 0)
		return(size);
	else {
		scsi_request_sense(ctlr, slave, 0, sp, 8);

		if (stat == STS_CHECKCOND) {
			printf("stread: Sense Key = [%s]", sense_key(sp->key));
			if (sp->filemark)
				printf(" [EOF]");
			if (sp->eom)
				printf(" [EOM]");
			printf("\n");
		}

		return(-1);
	}
}

int
stwrite(dev, buf, size)
	dev_t dev;
	char *buf;
	int   size;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_fmt_cdb *cdb = &st_cmd;
	struct scsi_xsense *sp = (struct scsi_xsense *)sensebuf;
	register int nblk;
	int ctlr, slave, stat;

	nblk = size >> DEV_BSHIFT;
	if (size % DEV_BSIZE)
		nblk++;
	size = nblk << DEV_BSHIFT;

	ctlr  = sc->sc_hd->hp_ctlr;
	slave = sc->sc_hd->hp_slave;

	sc->sc_flags |= STF_WRTTN;
	
	cdb->cdb[0] = CMD_WRITE;
	cdb->cdb[1] = 1;		/* unknown setup */

	cdb->cdb[2] = (nblk & 0xff0000) >> 16;
	cdb->cdb[3] = (nblk & 0x00ff00) >>  8;
	cdb->cdb[4] = (nblk & 0x0000ff);

	cdb->cdb[5] = 0;		/* unknown setup */

	stat = scsi_immed_command(ctlr, slave, 0, cdb, buf, size);

	if (stat == 0)
		return(size);
	else {
		scsi_request_sense(ctlr, slave, 0, sp, 8);

		if (stat == STS_CHECKCOND) {
			printf("stwrite: Sense Key = [%s]", sense_key(sp->key));
			if (sp->eom)
				printf(" [EOM]");
			printf("\n");
		}

		return(-1);
	}
}

int
stioctl(dev, cmd, data, flag, p)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
	struct proc *p;
{
}

st_rewind(dev)
	dev_t dev;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_fmt_cdb *cdb = &st_cmd;
	struct scsi_xsense *sp = (struct scsi_xsense *)sensebuf;
	int ctlr, slave, stat;
	int retry = 5;

	ctlr  = sc->sc_hd->hp_ctlr;
	slave = sc->sc_hd->hp_slave;

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
		scsi_request_sense(ctlr, slave, 0, sp, 8);

		if (stat == STS_CHECKCOND) {
			printf("st_rewind: Sense Key = [%s]", sense_key(sp->key));
			printf("\n");
		}

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
	
	cdb->cdb[0] = CMD_WRITE_FILEMARK;
	cdb->cdb[1] = 1;	/* command finished soon */

	cdb->cdb[2] = (marks & 0xff0000) >> 16;
	cdb->cdb[3] = (marks & 0x00ff00) >>  8;
	cdb->cdb[4] = (marks & 0x0000ff);

	cdb->cdb[5] = 0;		/* unknown setup */

	stat = scsi_immed_command(ctlr, slave, 0, cdb, (char *) 0, 0);

	if (stat == 0)
		return(1);

	printf("st: write EOF error\n");

	return(0);
}

int
st_skip(dev)
	dev_t dev;
{
	register int unit = stunit(dev);
	register struct st_softc *sc = &st_softc[unit];
	register struct scsi_fmt_cdb *cdb = &st_cmd;
	register int nfmk = 1;
	struct scsi_xsense *sp = (struct scsi_xsense *)sensebuf;
	int ctlr, slave, stat;

	ctlr  = sc->sc_hd->hp_ctlr;
	slave = sc->sc_hd->hp_slave;
	
	cdb->cdb[0] = CMD_SPACE;
	cdb->cdb[1] = 1;		/* it mean skip until EOF */

	cdb->cdb[2] = (nfmk & 0xff0000) >> 16;
	cdb->cdb[3] = (nfmk & 0x00ff00) >>  8;
	cdb->cdb[4] = (nfmk & 0x0000ff);

	cdb->cdb[5] = 0;		/* unknown setup */

	stat = scsi_immed_command(ctlr, slave, 0, cdb, 0, 0);

	if (stat == 0)
		return(0);
	else {
		scsi_request_sense(ctlr, slave, 0, sp, 8);

		if (stat == STS_CHECKCOND) {
			printf("st_skip: Sense Key = [%s]", sense_key(sp->key));
			if (sp->filemark)
				printf(" [EOF]");
			if (sp->eom)
				printf(" [EOM]");
			printf("\n");
		}

		return(-1);
	}
}

/*
 * Dump
 */

int
stdump(dev)
	dev_t dev;
{
}
