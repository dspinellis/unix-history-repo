/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1990, 1993
 *	The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Van Jacobson of Lawrence Berkeley Laboratory and the Systems
 * Programming Group of the University of Utah Computer Science Department.
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
 * from: Utah $Hdr: sd.c 1.9 92/12/21$
 *
 *	@(#)sd.c	8.1 (Berkeley) 6/10/93
 */

/*
 * SCSI CCS disk driver
 */

#include <sys/param.h>
#include <sys/disklabel.h>
#include <stand.att/saio.h>
#include <hp300/stand/samachdep.h>

#include <hp300/dev/scsireg.h>

struct	disklabel sdlabel;

struct	sdminilabel {
	u_short	npart;
	u_long	offset[MAXPARTITIONS];
};

struct	sd_softc {
	char	sc_retry;
	char	sc_alive;
	short	sc_blkshift;
	struct	sdminilabel sc_pinfo;
} sd_softc[NSCSI][NSD];

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

#ifdef COMPAT_NOLABEL
struct	sdminilabel defaultpinfo = {
	8,
	{ 1024, 17408, 0, 17408, 115712, 218112, 82944, 115712 }
};
#endif

sdgetinfo(io)
	register struct iob *io;
{
	struct sd_softc *ss = &sd_softc[io->i_adapt][io->i_ctlr];
	register struct sdminilabel *pi = &ss->sc_pinfo;
	register struct disklabel *lp = &sdlabel;
	char *msg, *readdisklabel();
	int sdstrategy(), i;

	bzero((caddr_t)lp, sizeof *lp);
	lp->d_secsize = (DEV_BSIZE << ss->sc_blkshift);
	msg = readdisklabel(io, sdstrategy, lp);
	if (msg) {
		printf("sd(%d,%d,%d,%d): WARNING: %s, ",
		       io->i_adapt, io->i_ctlr, io->i_unit, io->i_part, msg);
#ifdef COMPAT_NOLABEL
		printf("using old default partitioning\n");
		*pi = defaultpinfo;
#else
		printf("defining `c' partition as entire disk\n");
		pi->npart = 3;
		pi->offset[0] = pi->offset[1] = -1;
		pi->offset[2] = 0;
#endif
	} else {
		pi->npart = lp->d_npartitions;
		for (i = 0; i < pi->npart; i++)
			pi->offset[i] = lp->d_partitions[i].p_size == 0 ?
				-1 : lp->d_partitions[i].p_offset;
	}
	return(1);
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
	if (ss->sc_alive == 0) {
		if (sdinit(ctlr, unit) == 0)
			return (ENXIO);
		if (sdgetinfo(io) == 0)
			return (ERDLAB);
	}
	part = io->i_part;
	if (part >= ss->sc_pinfo.npart || ss->sc_pinfo.offset[part] == -1)
		return (EPART);
	io->i_boff = ss->sc_pinfo.offset[part];
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
