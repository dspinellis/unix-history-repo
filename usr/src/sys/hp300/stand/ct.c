/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)ct.c	7.5 (Berkeley) %G%
 */

/*
 * CS80 tape driver
 */
#include "sys/param.h"
#include "../dev/ctreg.h"

#include "saio.h"
#include "samachdep.h"

struct	ct_iocmd ct_ioc;
struct	ct_rscmd ct_rsc;
struct	ct_stat ct_stat;
struct	ct_ssmcmd ct_ssmc;

struct	ct_softc {
	char	sc_retry;
	char	sc_alive;
	short	sc_punit;
	int	sc_blkno;
} ct_softc[NHPIB][NCT];

#define	CTRETRY		5
#define	MTFSF		10
#define	MTREW		11

struct	ctinfo {
	short	hwid;
	short	punit;
} ctinfo[] = {
	CT7946ID,	1,
	CT7912PID,	1,
	CT7914PID,	1,
	CT9144ID,	0,
	CT9145ID,	0,
};
int	nctinfo = sizeof(ctinfo) / sizeof(ctinfo[0]);

ctinit(ctlr, unit)
	register int ctlr, unit;
{
	register struct ct_softc *rs = &ct_softc[ctlr][unit];
	u_char stat;
	register int type;

	if (hpibrecv(ctlr, unit, C_QSTAT, &stat, 1) != 1 || stat)
		return (0);
	if (ctident(ctlr, unit) < 0)
		return (0);
	ct_ssmc.unit = C_SUNIT(rs->sc_punit);
	ct_ssmc.cmd = C_SSM;
	ct_ssmc.fefm = FEF_MASK;
	ct_ssmc.refm = REF_MASK;
	ct_ssmc.aefm = AEF_MASK;
	ct_ssmc.iefm = IEF_MASK;
	hpibsend(ctlr, unit, C_CMD, &ct_ssmc, sizeof(ct_ssmc));
	hpibswait(ctlr, unit);
	hpibrecv(ctlr, unit, C_QSTAT, &stat, 1);
	rs->sc_alive = 1;
	return (1);
}

ctident(ctlr, unit)
	int ctlr, unit;
{
	struct ct_describe desc;
	u_char stat, cmd[3];
	char name[7];
	int id, i;

	id = hpibid(ctlr, unit);
	if ((id & 0x200) == 0)
		return(-1);
	for (i = 0; i < nctinfo; i++)
		if (id == ctinfo[i].hwid)
			break;
	if (i == nctinfo)
		return(-1);
	ct_softc[ctlr][unit].sc_punit = ctinfo[i].punit;
	id = i;

	/*
	 * Collect device description.
	 * Right now we only need this to differentiate 7945 from 7946.
	 * Note that we always issue the describe command to unit 0.
	 */
	cmd[0] = C_SUNIT(0);
	cmd[1] = C_SVOL(0);
	cmd[2] = C_DESC;
	hpibsend(ctlr, unit, C_CMD, cmd, sizeof(cmd));
	hpibrecv(ctlr, unit, C_EXEC, &desc, 37);
	hpibrecv(ctlr, unit, C_QSTAT, &stat, sizeof(stat));
	bzero(name, sizeof(name));
	if (!stat) {
		register int n = desc.d_name;
		for (i = 5; i >= 0; i--) {
			name[i] = (n & 0xf) + '0';
			n >>= 4;
		}
	}
	switch (ctinfo[id].hwid) {
	case CT7946ID:
		if (bcmp(name, "079450", 6) == 0)
			id = -1;		/* not really a 7946 */
		break;
	default:
		break;
	}
	return(id);
}

ctopen(io)
	struct iob *io;
{
	register struct ct_softc *rs;
	register int unit, ctlr;
	register int skip;

	devconvert(io);

	ctlr = io->i_adapt;
	if (ctlr >= NHPIB || hpibalive(ctlr) == 0)
		return(EADAPT);
	unit = io->i_ctlr;
	if (unit >= NCT)
		return(ECTLR);
	if (rs->sc_alive == 0)
		if (ctinit(ctlr, unit) == 0)
			return(ENXIO);
	ctstrategy(io, MTREW);
	skip = io->i_part;
	while (skip--)
		ctstrategy(io, MTFSF);
	return(0);
}

ctclose(io)
	struct iob *io;
{
	ctstrategy(io, MTREW);
}

ctstrategy(io, func)
	register struct iob *io;
	register int func;
{
	register int ctlr = io->i_adapt;
	register int unit = io->i_ctlr;
	register struct ct_softc *rs = &ct_softc[ctlr][unit];
	char stat;

	if (io->i_cc == 0 && (func == F_READ || func == F_WRITE))
		return(0);

	rs->sc_retry = 0;
	ct_ioc.unit = C_SUNIT(rs->sc_punit);
	ct_ioc.saddr = C_SADDR;
	ct_ioc.nop2 = C_NOP;
	ct_ioc.slen = C_SLEN;
	ct_ioc.nop3 = C_NOP;
top:
	if (func == F_READ) {
		ct_ioc.cmd = C_READ;
		ct_ioc.addr = rs->sc_blkno;
		ct_ioc.len = io->i_cc;
	}
	else if (func == F_WRITE) {
		ct_ioc.cmd = C_WRITE;
		ct_ioc.addr = rs->sc_blkno;
		ct_ioc.len = io->i_cc;
	}
	else if (func == MTFSF) {
		ct_ioc.cmd = C_READ;
		ct_ioc.addr = rs->sc_blkno;
		ct_ioc.len = io->i_cc = MAXBSIZE;
		io->i_ma = io->i_buf;
	}
	else {
		ct_ioc.cmd = C_READ;
		ct_ioc.addr = 0;
		ct_ioc.len = 0;
		rs->sc_blkno = 0;
		io->i_cc = 0;
	}
retry:
	hpibsend(ctlr, unit, C_CMD, &ct_ioc, sizeof(ct_ioc));
	if (func != MTREW) {
		hpibswait(ctlr, unit);
		hpibgo(ctlr, unit, C_EXEC, io->i_ma, io->i_cc,
			func != F_WRITE ? F_READ : F_WRITE);
		hpibswait(ctlr, unit);
	} else {
		while (hpibswait(ctlr, unit) < 0)
			;
	}
	hpibrecv(ctlr, unit, C_QSTAT, &stat, 1);
	if (stat) {
		stat = cterror(ctlr, unit);
		if (stat == 0)
			return (-1);
		if (stat == 2)
			return (0);
		if (++rs->sc_retry > CTRETRY)
			return (-1);
		goto retry;
	}
	rs->sc_blkno += CTBTOK(io->i_cc);
	if (func == MTFSF)
		goto top;
	return (io->i_cc);
}

cterror(ctlr, unit)
	register int ctlr, unit;
{
	register struct ct_softc *rs = &ct_softc[ctlr][unit];
	char stat;

	ct_rsc.unit = C_SUNIT(rs->sc_punit);
	ct_rsc.cmd = C_STATUS;
	hpibsend(ctlr, unit, C_CMD, &ct_rsc, sizeof(ct_rsc));
	hpibrecv(ctlr, unit, C_EXEC, &ct_stat, sizeof(ct_stat));
	hpibrecv(ctlr, unit, C_QSTAT, &stat, 1);
	if (stat) {
		printf("ct%d: request status fail %d\n", unit, stat);
		return(0);
	}
	if (ct_stat.c_aef & AEF_EOF) {
		/* 9145 drives don't increment block number at EOF */
		if ((ct_stat.c_blk - rs->sc_blkno) == 0)
			rs->sc_blkno++;
		else
			rs->sc_blkno = ct_stat.c_blk;
		return (2);
	}
	printf("ct%d err: vu 0x%x, pend 0x%x, bn%d", unit,
		ct_stat.c_vu, ct_stat.c_pend, ct_stat.c_blk);
	printf(", R 0x%x F 0x%x A 0x%x I 0x%x\n", ct_stat.c_ref,
		ct_stat.c_fef, ct_stat.c_aef, ct_stat.c_ief);
	return (1);
}
