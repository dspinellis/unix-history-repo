/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory and the Systems
 * Programming Group of the University of Utah Computer Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: sd.c 1.2 90/01/23$
 *
 *	@(#)sd.c	7.7 (Berkeley) %G%
 */

/*
 * SCSI CCS disk driver
 */

#include <sys/param.h>
#include <stand/saio.h>
#include <hp300/stand/samachdep.h>

#include <hp300/dev/scsireg.h>

struct	sd_softc {
	char	sc_retry;
	char	sc_alive;
	short	sc_blkshift;
} sd_softc[NSCSI][NSD];

int sdpartoff[] = {
	1024,	17408,	0,	17408,
	115712,	218112,	82944,	115712
};

#define	SDRETRY		2

sdinit(ctlr, unit)
	int ctlr, unit;
{
	register struct sd_softc *ss = &sd_softc[ctlr][unit];
	u_char stat;
	int capbuf[2];

	stat = scsi_test_unit_rdy(ctlr, unit);
	if (stat) {
		/* drive may be doing RTZ - wait a bit */
		if (stat == STS_CHECKCOND) {
			DELAY(1000000);
			stat = scsi_test_unit_rdy(ctlr, unit);
		}
		if (stat) {
			printf("sd(%d,%d,0,0): init failed (stat=%x)\n",
			       ctlr, unit, stat);
			return (0);
		}
	}
	/*
	 * try to get the drive block size.
	 */
	capbuf[0] = 0;
	capbuf[1] = 0;
	stat = scsi_read_capacity(ctlr, unit,
				  (u_char *)capbuf, sizeof(capbuf));
	if (stat == 0) {
		if (capbuf[1] > DEV_BSIZE)
			for (; capbuf[1] > DEV_BSIZE; capbuf[1] >>= 1)
				++ss->sc_blkshift;
	}
	ss->sc_alive = 1;
	return (1);
}

sdreset(ctlr, unit)
	int ctlr, unit;
{
}

sdopen(io)
	struct iob *io;
{
	register struct sd_softc *ss;
	int ctlr, unit, part;

	devconvert(io);

	ctlr = io->i_adapt;
	if (ctlr >= NSCSI || scsialive(ctlr) == 0)
		return (EADAPT);
	unit = io->i_ctlr;
	if (unit >= NSD)
		return (ECTLR);
	ss = &sd_softc[ctlr][unit];
	if (ss->sc_alive == 0)
		if (sdinit(ctlr, unit) == 0)
			return (ENXIO);
	part = io->i_part;
	if (part >= 8)
		return (EPART);
	io->i_boff = sdpartoff[part];
	return (0);
}

sdstrategy(io, func)
	register struct iob *io;
	int func;
{
	register int ctlr = io->i_adapt;
	register int unit = io->i_ctlr;
	register struct sd_softc *ss = &sd_softc[ctlr][unit];
	daddr_t blk = io->i_bn >> ss->sc_blkshift;
	u_int nblk = io->i_cc >> ss->sc_blkshift;
	char stat;

	if (io->i_cc == 0)
		return(0);

	ss->sc_retry = 0;
retry:
	if (func == F_READ)
		stat = scsi_tt_read(ctlr, unit, io->i_ma, io->i_cc, blk, nblk);
	else
		stat = scsi_tt_write(ctlr, unit, io->i_ma, io->i_cc, blk, nblk);
	if (stat) {
		printf("sd(%d,%d,%d,%d): block=%x, error=0x%x\n",
		       ctlr, unit, io->i_unit, io->i_part, blk, stat);
		if (++ss->sc_retry > SDRETRY)
			return(-1);
		goto retry;
	}
	return(io->i_cc);
}
