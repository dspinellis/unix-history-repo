/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * Redistribution is only permitted until one year after the first shipment
 * of 4.4BSD by the Regents.  Otherwise, redistribution and use in source and
 * binary forms are permitted provided that: (1) source distributions retain
 * this entire copyright notice and comment, and (2) distributions including
 * binaries display the following acknowledgement:  This product includes
 * software developed by the University of California, Berkeley and its
 * contributors'' in the documentation or other materials provided with the
 * distribution and in all advertising materials mentioning features or use
 * of this software.  Neither the name of the University nor the names of
 * its contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 * from: Utah $Hdr: rd.c 1.14 89/02/27$
 *
 *	@(#)rd.c	7.1 (Berkeley) 5/8/90
 */

/*
 * CS80/SS80 disk driver
 */
#include "saio.h"
#include "samachdep.h"

#include "../hpdev/rdreg.h"

struct	rd_iocmd rd_ioc;
struct	rd_rscmd rd_rsc;
struct	rd_stat rd_stat;
struct	rd_ssmcmd rd_ssmc;

struct	rd_softc {
	char	sc_retry;
	char	sc_alive;
	short	sc_type;
} rd_softc[NRD];

#define	RDRETRY		5

int rdcyloff[][8] = {
	{ 1, 143, 0, 143, 0,   0,   323, 503, },	/* 7945A */
	{ 1, 167, 0, 0,   0,   0,   0,   0,   },	/* 9134D */
	{ 0, 0,   0, 0,   0,   0,   0,   0,   },	/* 9122S */
	{ 0, 71,  0, 221, 292, 542, 221, 0,   },	/* 7912P */
	{ 1, 72,  0, 72,  362, 802, 252, 362, },	/* 7914P */
	{ 1, 28,  0, 140, 167, 444, 140, 721, },	/* 7933H */
	{ 1, 200, 0, 200, 0,   0,   450, 600, },	/* 9134L */
	{ 1, 105, 0, 105, 380, 736, 265, 380, },	/* 7957A */
	{ 1, 65,  0, 65,  257, 657, 193, 257, },	/* 7958A */
	{ 1, 128, 0, 128, 518, 918, 388, 518, },	/* 7957B */
	{ 1, 44,  0, 44,  174, 496, 131, 174, },	/* 7958B */
	{ 1, 44,  0, 44,  218, 918, 174, 218, },	/* 7959B */
	{ 1, 20,  0, 98,  117, 256, 98,  397, },	/* 7936H */
	{ 1, 11,  0, 53,  63,  217, 53,  371, },	/* 7937H */
};

struct rdinfo {
	int	nbpc;
	int	hwid;
	int	*cyloff;
} rdinfo[] = {
	NRD7945ABPT*NRD7945ATRK, RD7946AID, rdcyloff[0],
	NRD9134DBPT*NRD9134DTRK, RD9134DID, rdcyloff[1],
	NRD9122SBPT*NRD9122STRK, RD9134LID, rdcyloff[2],
	NRD7912PBPT*NRD7912PTRK, RD7912PID, rdcyloff[3],
	NRD7914PBPT*NRD7914PTRK, RD7914PID, rdcyloff[4],
	NRD7958ABPT*NRD7958ATRK, RD7958AID, rdcyloff[8],
	NRD7957ABPT*NRD7957ATRK, RD7957AID, rdcyloff[7],
	NRD7933HBPT*NRD7933HTRK, RD7933HID, rdcyloff[5],
	NRD9134LBPT*NRD9134LTRK, RD9134LID, rdcyloff[6],
	NRD7936HBPT*NRD7936HTRK, RD7936HID, rdcyloff[12],
	NRD7937HBPT*NRD7937HTRK, RD7937HID, rdcyloff[13],
	NRD7914PBPT*NRD7914PTRK, RD7914CTID,rdcyloff[4],
	NRD7945ABPT*NRD7945ATRK, RD7946AID, rdcyloff[0],
	NRD9122SBPT*NRD9122STRK, RD9134LID, rdcyloff[2],
	NRD7957BBPT*NRD7957BTRK, RD7957BID, rdcyloff[9],
	NRD7958BBPT*NRD7958BTRK, RD7958BID, rdcyloff[10],
	NRD7959BBPT*NRD7959BTRK, RD7959BID, rdcyloff[11],
};
int	nrdinfo = sizeof(rdinfo) / sizeof(rdinfo[0]);
					
rdinit(unit)
	register int unit;
{
	register struct rd_softc *rs;
	u_char stat;

	if (unit > NRD)
		return (0);
	rs = &rd_softc[unit];
	rs->sc_type = rdident(unit);
	if (rs->sc_type < 0)
		return (0);
	rs->sc_alive = 1;
	return (1);
}

rdreset(unit)
{
	u_char stat;

	rd_ssmc.c_unit = C_SUNIT(0);
	rd_ssmc.c_cmd = C_SSM;
	rd_ssmc.c_refm = REF_MASK;
	rd_ssmc.c_fefm = FEF_MASK;
	rd_ssmc.c_aefm = AEF_MASK;
	rd_ssmc.c_iefm = IEF_MASK;
	hpibsend(unit, C_CMD, &rd_ssmc, sizeof(rd_ssmc));
	hpibswait(unit);
	hpibrecv(unit, C_QSTAT, &stat, 1);
}

rdident(unit)
{
	struct rd_describe desc;
	u_char stat, cmd[3];
	char name[7];
	register int id, i;

	id = hpibid(unit);
	if ((id & 0x200) == 0)
		return(-1);
	for (i = 0; i < nrdinfo; i++)
		if (id == rdinfo[i].hwid)
			break;
	if (i == nrdinfo)
		return(-1);
	id = i;
	rdreset(unit);
	cmd[0] = C_SUNIT(0);
	cmd[1] = C_SVOL(0);
	cmd[2] = C_DESC;
	hpibsend(unit, C_CMD, cmd, sizeof(cmd));
	hpibrecv(unit, C_EXEC, &desc, 37);
	hpibrecv(unit, C_QSTAT, &stat, sizeof(stat));
	bzero(name, sizeof(name));
	if (!stat) {
		register int n = desc.d_name;
		for (i = 5; i >= 0; i--) {
			name[i] = (n & 0xf) + '0';
			n >>= 4;
		}
	}
	/*
	 * Take care of a couple of anomolies:
	 * 1. 7945A and 7946A both return same HW id
	 * 2. 9122S and 9134D both return same HW id
	 * 3. 9122D and 9134L both return same HW id
	 */
	switch (rdinfo[id].hwid) {
	case RD7946AID:
		if (bcmp(name, "079450", 6) == 0)
			id = RD7945A;
		else
			id = RD7946A;
		break;

	case RD9134LID:
		if (bcmp(name, "091340", 6) == 0)
			id = RD9134L;
		else
			id = RD9122D;
		break;

	case RD9134DID:
		if (bcmp(name, "091220", 6) == 0)
			id = RD9122S;
		else
			id = RD9134D;
		break;
	}
	return(id);
}

rdopen(io)
	struct iob *io;
{
	register int unit = io->i_unit;
	register struct rd_softc *rs = &rd_softc[unit];
	struct rdinfo *ri;

	if (hpibalive(unit) == 0)
		_stop("rd controller not configured");
	if (rs->sc_alive == 0)
		if (rdinit(unit) == 0)
			_stop("rd init failed");
	if (io->i_boff < 0 || io->i_boff > 7)
		_stop("rd bad minor");
	ri = &rdinfo[rs->sc_type];
	io->i_boff = ri->cyloff[io->i_boff] * ri->nbpc;
}

rdstrategy(io, func)
	register struct iob *io;
	register int func;
{
	register int unit = io->i_unit;
	register struct rd_softc *rs = &rd_softc[unit];
	char stat;

	rs->sc_retry = 0;
	rd_ioc.c_unit = C_SUNIT(0);
	rd_ioc.c_volume = C_SVOL(0);
	rd_ioc.c_saddr = C_SADDR;
	rd_ioc.c_hiaddr = 0;
	rd_ioc.c_addr = RDBTOS(io->i_bn);
	rd_ioc.c_nop2 = C_NOP;
	rd_ioc.c_slen = C_SLEN;
	rd_ioc.c_len = io->i_cc;
	rd_ioc.c_cmd = func == READ ? C_READ : C_WRITE;
retry:
	hpibsend(unit, C_CMD, &rd_ioc.c_unit, sizeof(rd_ioc)-2);
	hpibswait(unit);
	hpibgo(unit, C_EXEC, io->i_ma, io->i_cc, func);
	hpibswait(unit);
	hpibrecv(unit, C_QSTAT, &stat, 1);
	if (stat) {
		if (rderror(unit) == 0)
			return(-1);
		if (++rs->sc_retry > RDRETRY)
			return(-1);
		else
			goto retry;
	}
	return(io->i_cc);
}

rderror(unit)
	register int unit;
{
	register struct rd_softc *rd = &rd_softc[unit];
	char stat;

	rd_rsc.c_unit = C_SUNIT(0);
	rd_rsc.c_sram = C_SRAM;
	rd_rsc.c_ram = C_RAM;
	rd_rsc.c_cmd = C_STATUS;
	hpibsend(unit, C_CMD, &rd_rsc, sizeof(rd_rsc));
	hpibrecv(unit, C_EXEC, &rd_stat, sizeof(rd_stat));
	hpibrecv(unit, C_QSTAT, &stat, 1);
	if (stat) {
		printf("rd(%d,?): request status fail %d\n", unit, stat);
		return(0);
	}
	printf("rd(%d,?) err: vu 0x%x", unit, rd_stat.c_vu);
	if ((rd_stat.c_aef & AEF_UD) || (rd_stat.c_ief & (IEF_MD|IEF_RD)))
		printf(", block %d", rd_stat.c_blk);
	printf(", R0x%x F0x%x A0x%x I0x%x\n",
	       rd_stat.c_ref, rd_stat.c_fef, rd_stat.c_aef, rd_stat.c_ief);
	return(1);
}
