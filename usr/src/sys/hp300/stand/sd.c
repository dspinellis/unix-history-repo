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
 *	@(#)sd.c	7.3 (Berkeley) %G%
 */

/*
 * SCSI CCS disk driver
 */

#include "saio.h"
#include "samachdep.h"

#include "../dev/scsireg.h"

struct	sd_softc {
	char	sc_retry;
	char	sc_alive;
	short	sc_blkshift;
} sd_softc[NSD];

int sdpartoff[] = {
	1024,	17408,	0,	17408,
	115712,	218112,	82944,	0
};

#define	SDRETRY		2

sdinit(unit)
	register int unit;
{
	register struct sd_softc *ss;
	u_char stat;
	int capbuf[2];

	if (unit > NSD)
		return (0);
	ss = &sd_softc[unit];
	/* NB: HP6300 won't boot if next printf is removed (???) - vj */
	printf("sd%d: ", unit);
	if ((stat = scsi_test_unit_rdy(unit)) == 0) {
		/* drive may be doing RTZ - wait a bit */
		printf("not ready - retrying ... ");
		if (stat == STS_CHECKCOND) {
			DELAY(1000000);
			if (scsi_test_unit_rdy(unit) == 0) {
				printf("giving up.\n");
				return (0);
			}
		}
	}
	printf("unit ready.\n");
	/*
	 * try to get the drive block size.
	 */
	capbuf[0] = 0;
	capbuf[1] = 0;
	if (scsi_read_capacity(unit, (u_char *)capbuf, sizeof(capbuf)) != 0) {
		if (capbuf[1] > DEV_BSIZE)
			for (; capbuf[1] > DEV_BSIZE; capbuf[1] >>= 1)
				++ss->sc_blkshift;
	}
	ss->sc_alive = 1;
	return (1);
}

sdreset(unit)
{
}

sdopen(io)
	struct iob *io;
{
	register int unit = io->i_unit;
	register struct sd_softc *ss = &sd_softc[unit];
	struct sdinfo *ri;

	if (scsialive(unit) == 0)
		_stop("scsi controller not configured");
	if (ss->sc_alive == 0)
		if (sdinit(unit) == 0)
			_stop("sd init failed");
	if (io->i_boff < 0 || io->i_boff > 7)
		_stop("sd bad minor");
	io->i_boff = sdpartoff[io->i_boff];
}

sdstrategy(io, func)
	register struct iob *io;
	register int func;
{
	register int unit = io->i_unit;
	register struct sd_softc *ss = &sd_softc[unit];
	char stat;
	daddr_t blk = io->i_bn >> ss->sc_blkshift;
	u_int nblk = io->i_cc >> ss->sc_blkshift;

	ss->sc_retry = 0;
retry:
	if (func == READ)
		stat = scsi_tt_read(unit, io->i_ma, io->i_cc, blk, nblk);
	else
		stat = scsi_tt_write(unit, io->i_ma, io->i_cc, blk, nblk);
	if (stat) {
		printf("sd(%d,?) err: 0x%x\n", unit, stat);
		if (++ss->sc_retry > SDRETRY)
			return(-1);
		else
			goto retry;
	}
	return(io->i_cc);
}
