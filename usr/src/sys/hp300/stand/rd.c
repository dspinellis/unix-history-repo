/*
 * Copyright (c) 1988 University of Utah.
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: rd.c 1.20 92/12/21$
 *
 *	@(#)rd.c	7.7 (Berkeley) %G%
 */

/*
 * CS80/SS80 disk driver
 */
#include <sys/param.h>
#include <sys/disklabel.h>
#include <stand/saio.h>
#include <hp300/stand/samachdep.h>

#include <hp300/dev/rdreg.h>

struct	rd_iocmd rd_ioc;
struct	rd_rscmd rd_rsc;
struct	rd_stat rd_stat;
struct	rd_ssmcmd rd_ssmc;

struct	disklabel rdlabel;

struct	rdminilabel {
	u_short	npart;
	u_long	offset[MAXPARTITIONS];
};

struct	rd_softc {
	char	sc_retry;
	char	sc_alive;
	short	sc_type;
	struct	rdminilabel sc_pinfo;
} rd_softc[NHPIB][NRD];

#define	RDRETRY		5

struct	rdidentinfo {
	short	ri_hwid;
	short	ri_maxunum;
	int	ri_nblocks;
} rdidentinfo[] = {
	{ RD7946AID,	0,	 108416 },
	{ RD9134DID,	1,	  29088 },
	{ RD9134LID,	1,	   1232 },
	{ RD7912PID,	0,	 128128 },
	{ RD7914PID,	0,	 258048 },
	{ RD7958AID,	0,	 255276 },
	{ RD7957AID,	0,	 159544 },
	{ RD7933HID,	0,	 789958 },
	{ RD9134LID,	1,	  77840 },
	{ RD7936HID,	0,	 600978 },
	{ RD7937HID,	0,	1116102 },
	{ RD7914CTID,	0,	 258048 },
	{ RD7946AID,	0,	 108416 },
	{ RD9134LID,	1,	   1232 },
	{ RD7957BID,	0,	 159894 },
	{ RD7958BID,	0,	 297108 },
	{ RD7959BID,	0,	 594216 },
	{ RD2200AID,	0,	 654948 },
	{ RD2203AID,	0,	1309896 }
};
int numrdidentinfo = sizeof(rdidentinfo) / sizeof(rdidentinfo[0]);

rdinit(ctlr, unit)
	int ctlr, unit;
{
	register struct rd_softc *rs = &rd_softc[ctlr][unit];
	u_char stat;

	rs->sc_type = rdident(ctlr, unit);
	if (rs->sc_type < 0)
		return (0);
	rs->sc_alive = 1;
	return (1);
}

rdreset(ctlr, unit)
	register int ctlr, unit;
{
	u_char stat;

	rd_ssmc.c_unit = C_SUNIT(0);
	rd_ssmc.c_cmd = C_SSM;
	rd_ssmc.c_refm = REF_MASK;
	rd_ssmc.c_fefm = FEF_MASK;
	rd_ssmc.c_aefm = AEF_MASK;
	rd_ssmc.c_iefm = IEF_MASK;
	hpibsend(ctlr, unit, C_CMD, &rd_ssmc, sizeof(rd_ssmc));
	hpibswait(ctlr, unit);
	hpibrecv(ctlr, unit, C_QSTAT, &stat, 1);
}

rdident(ctlr, unit)
	register int ctlr, unit;
{
	struct rd_describe desc;
	u_char stat, cmd[3];
	char name[7];
	register int id, i;

	id = hpibid(ctlr, unit);
	if ((id & 0x200) == 0)
		return(-1);
	for (i = 0; i < numrdidentinfo; i++)
		if (id == rdidentinfo[i].ri_hwid)
			break;
	if (i == numrdidentinfo || unit > rdidentinfo[i].ri_maxunum)
		return(-1);
	id = i;
	rdreset(ctlr, unit);
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
	/*
	 * Take care of a couple of anomolies:
	 * 1. 7945A and 7946A both return same HW id
	 * 2. 9122S and 9134D both return same HW id
	 * 3. 9122D and 9134L both return same HW id
	 */
	switch (rdidentinfo[id].ri_hwid) {
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

#ifdef COMPAT_NOLABEL
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
	{ 1, 44,  0, 44,  218, 1022,174, 218, },	/* 7959B */
	{ 1, 37,  0, 37,  183, 857, 147, 183, },	/* 2200A */
	{ 1, 19,  0, 94,  112, 450, 94,  788, },	/* 2203A */
	{ 1, 20,  0, 98,  117, 256, 98,  397, },	/* 7936H */
	{ 1, 11,  0, 53,  63,  217, 53,  371, },	/* 7937H */
};

struct rdcompatinfo {
	int	nbpc;
	int	*cyloff;
} rdcompatinfo[] = {
	NRD7945ABPT*NRD7945ATRK, rdcyloff[0],
	NRD9134DBPT*NRD9134DTRK, rdcyloff[1],
	NRD9122SBPT*NRD9122STRK, rdcyloff[2],
	NRD7912PBPT*NRD7912PTRK, rdcyloff[3],
	NRD7914PBPT*NRD7914PTRK, rdcyloff[4],
	NRD7958ABPT*NRD7958ATRK, rdcyloff[8],
	NRD7957ABPT*NRD7957ATRK, rdcyloff[7],
	NRD7933HBPT*NRD7933HTRK, rdcyloff[5],
	NRD9134LBPT*NRD9134LTRK, rdcyloff[6],
	NRD7936HBPT*NRD7936HTRK, rdcyloff[14],
	NRD7937HBPT*NRD7937HTRK, rdcyloff[15],
	NRD7914PBPT*NRD7914PTRK, rdcyloff[4],
	NRD7945ABPT*NRD7945ATRK, rdcyloff[0],
	NRD9122SBPT*NRD9122STRK, rdcyloff[2],
	NRD7957BBPT*NRD7957BTRK, rdcyloff[9],
	NRD7958BBPT*NRD7958BTRK, rdcyloff[10],
	NRD7959BBPT*NRD7959BTRK, rdcyloff[11],
	NRD2200ABPT*NRD2200ATRK, rdcyloff[12],
	NRD2203ABPT*NRD2203ATRK, rdcyloff[13],
};
int	nrdcompatinfo = sizeof(rdcompatinfo) / sizeof(rdcompatinfo[0]);
#endif					

rdgetinfo(io)
	register struct iob *io;
{
	struct rd_softc *rs = &rd_softc[io->i_adapt][io->i_ctlr];
	register struct rdminilabel *pi = &rs->sc_pinfo;
	register struct disklabel *lp = &rdlabel;
	char *msg, *readdisklabel();
	int rdstrategy(), i;

	bzero((caddr_t)lp, sizeof *lp);
	lp->d_secsize = DEV_BSIZE;
	msg = readdisklabel(io, rdstrategy, lp);
	if (msg) {
		printf("rd(%d,%d,%d,%d): WARNING: %s, ",
		       io->i_adapt, io->i_ctlr, io->i_unit, io->i_part, msg);
#ifdef COMPAT_NOLABEL
		{
			register struct rdcompatinfo *ci;

			printf("using old default partitioning\n");
			ci = &rdcompatinfo[rs->sc_type];
			pi->npart = 8;
			for (i = 0; i < pi->npart; i++)
				pi->offset[i] = ci->cyloff[i] * ci->nbpc;
		}
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

rdopen(io)
	struct iob *io;
{
	register struct rd_softc *rs;
	struct rdinfo *ri;
	int unit, ctlr, part;

	devconvert(io);

	ctlr = io->i_adapt;
	if (ctlr >= NHPIB || hpibalive(ctlr) == 0)
		return (EADAPT);
	unit = io->i_ctlr;
	if (unit >= NRD)
		return (ECTLR);
	rs = &rd_softc[ctlr][unit];
	if (rs->sc_alive == 0) {
		if (rdinit(ctlr, unit) == 0)
			return (ENXIO);
		if (rdgetinfo(io) == 0)
			return (ERDLAB);
	}
	part = io->i_part;
	if (part >= rs->sc_pinfo.npart || rs->sc_pinfo.offset[part] == -1)
		return (EPART);
	io->i_boff = rs->sc_pinfo.offset[part];
	return (0);
}

rdstrategy(io, func)
	register struct iob *io;
	int func;
{
	register int ctlr = io->i_adapt;
	register int unit = io->i_ctlr;
	register struct rd_softc *rs = &rd_softc[ctlr][unit];
	char stat;

	if (io->i_cc == 0)
		return(0);

	rs->sc_retry = 0;
	rd_ioc.c_unit = C_SUNIT(0);
	rd_ioc.c_volume = C_SVOL(0);
	rd_ioc.c_saddr = C_SADDR;
	rd_ioc.c_hiaddr = 0;
	rd_ioc.c_addr = RDBTOS(io->i_bn);
	rd_ioc.c_nop2 = C_NOP;
	rd_ioc.c_slen = C_SLEN;
	rd_ioc.c_len = io->i_cc;
	rd_ioc.c_cmd = func == F_READ ? C_READ : C_WRITE;
retry:
	hpibsend(ctlr, unit, C_CMD, &rd_ioc.c_unit, sizeof(rd_ioc)-2);
	hpibswait(ctlr, unit);
	hpibgo(ctlr, unit, C_EXEC, io->i_ma, io->i_cc, func);
	hpibswait(ctlr, unit);
	hpibrecv(ctlr, unit, C_QSTAT, &stat, 1);
	if (stat) {
		if (rderror(ctlr, unit, io->i_part) == 0)
			return(-1);
		if (++rs->sc_retry > RDRETRY)
			return(-1);
		goto retry;
	}
	return(io->i_cc);
}

rderror(ctlr, unit, part)
	register int ctlr, unit;
	int part;
{
	register struct rd_softc *rd = &rd_softc[ctlr][unit];
	char stat;

	rd_rsc.c_unit = C_SUNIT(0);
	rd_rsc.c_sram = C_SRAM;
	rd_rsc.c_ram = C_RAM;
	rd_rsc.c_cmd = C_STATUS;
	hpibsend(ctlr, unit, C_CMD, &rd_rsc, sizeof(rd_rsc));
	hpibrecv(ctlr, unit, C_EXEC, &rd_stat, sizeof(rd_stat));
	hpibrecv(ctlr, unit, C_QSTAT, &stat, 1);
	if (stat) {
		printf("rd(%d,%d,0,%d): request status fail %d\n",
		       ctlr, unit, part, stat);
		return(0);
	}
	printf("rd(%d,%d,0,%d) err: vu 0x%x",
	       ctlr, unit, part, rd_stat.c_vu);
	if ((rd_stat.c_aef & AEF_UD) || (rd_stat.c_ief & (IEF_MD|IEF_RD)))
		printf(", block %d", rd_stat.c_blk);
	printf(", R0x%x F0x%x A0x%x I0x%x\n",
	       rd_stat.c_ref, rd_stat.c_fef, rd_stat.c_aef, rd_stat.c_ief);
	return(1);
}
