/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)uda.c	7.7 (Berkeley) %G%
 */

/*
 * UDA50/RAxx disk device driver
 */
#include "../machine/pte.h"

#include "param.h"
#include "inode.h"
#include "fs.h"
#include "disklabel.h"

#include "saio.h"
#include "savax.h"

#define	NRA		8	/* max. unit number on controller */
#define	SECTSIZ 	512	/* sector size in bytes */

/*
 * Unused, but needed in udareg.h
 */
#define NRSP	1
#define NCMD	1

#include "../vaxuba/udareg.h"
#include "../vaxuba/ubareg.h"
#include "../vax/mscp.h"

#define	MAXCTLR		1		/* all addresses must be specified */
u_short udastd[MAXCTLR] = { 0772150 };

struct iob	cudbuf;

struct udadevice *udaddr[MAXNUBA][MAXCTLR];

struct	uda1 {
	struct	uda1ca uda1_ca;	/* communications area */
	struct	mscp uda1_rsp;	/* response packet */
	struct	mscp uda1_cmd;	/* command packet */
} uda1;

				/* Unibus address of uda structure */
struct	uda1 *ud_ubaddr[MAXNUBA][MAXCTLR];
struct	disklabel ralabel[MAXNUBA][MAXCTLR][NRA];
static	u_int ratype[MAXNUBA][MAXCTLR][NRA];
char	lbuf[SECTSIZ];

raopen(io)
	register struct iob *io;
{
	register struct mscp *mp;
	register struct disklabel *lp, *dlp;
	register struct udadevice *addr;
	register struct uda1 *ubaddr;
	register unit;
	static int udainit[MAXNUBA][MAXCTLR];
	int uba;

	if ((u_int)io->i_ctlr >= MAXCTLR)
		return (ECTLR);
	unit = io->i_unit;
	uba = io->i_adapt;
	addr = udaddr[uba][io->i_ctlr] =
	    (struct udadevice *)ubamem(uba, udastd[io->i_ctlr]);
	if (badaddr((char *)addr, sizeof(short))) {
		printf("ra: nonexistent device\n");
		return (ENXIO);
	}
	if (ud_ubaddr[uba][io->i_ctlr] == 0) {
		/*
		 * Initialize cudbuf.i_unit so that controllers
		 * on UNIBUSes other than 0 can be used.
		 */
		cudbuf.i_unit = unit;
		cudbuf.i_ma = (caddr_t)&uda1;
		cudbuf.i_cc = sizeof(uda1);
		ud_ubaddr[uba][io->i_ctlr] = (struct uda1 *)ubasetup(&cudbuf, 2);
	}
	ubaddr = ud_ubaddr[uba][io->i_ctlr];
	if (udainit[uba][io->i_ctlr] == 0) {
		addr->udaip = 0;
		while ((addr->udasa & UDA_STEP1) == 0)
			;
		addr->udasa = UDA_ERR;
		while ((addr->udasa & UDA_STEP2) == 0)
			;
		addr->udasa = (short)&ubaddr->uda1_ca.ca_rspdsc;
		while ((addr->udasa & UDA_STEP3) == 0)
			;
		addr->udasa =
		    (short)(((int)&ubaddr->uda1_ca.ca_rspdsc) >> 16);
		while ((addr->udasa & UDA_STEP4) == 0)
			;
		addr->udasa = UDA_GO;
		uda1.uda1_ca.ca_rspdsc = (long)&ubaddr->uda1_rsp.mscp_cmdref;
		uda1.uda1_ca.ca_cmddsc = (long)&ubaddr->uda1_cmd.mscp_cmdref;
		/* uda1.uda1_cmd.mscp_cntflgs = 0; */
		if (udcmd(M_OP_SETCTLRC, io)) {
			printf("ra: open error, SETCTLRC\n");
			return (ENXIO);
		}
		udainit[uba][io->i_ctlr] = 1;
	}
	lp = &ralabel[uba][io->i_ctlr][unit];
	if (ratype[uba][io->i_ctlr][unit] == 0) {
		struct iob tio;

		uda1.uda1_cmd.mscp_unit = unit;
		if (udcmd(M_OP_ONLINE, io)) {
			printf("ra: open error, ONLINE\n");
			return (ENXIO);
		}
		ratype[uba][io->i_ctlr][unit] =
		    uda1.uda1_rsp.mscp_onle.onle_drivetype;
		tio = *io;
		tio.i_bn = LABELSECTOR;
		tio.i_ma = lbuf;
		tio.i_cc = SECTSIZ;
		tio.i_flgs |= F_RDDATA;
		if (rastrategy(&tio, READ) != SECTSIZ) {
			printf("ra: can't read disk label\n");
			return (ENXIO);
		}
		*lp = *(struct disklabel *)(lbuf + LABELOFFSET);
		if (lp->d_magic != DISKMAGIC || lp->d_magic2 != DISKMAGIC) {
			printf("ra%d: unlabeled\n", unit);
#ifdef COMPAT_42
			ramaptype(io, lp);
#else
			return (ENXIO);
#endif
		}
	}
	if ((u_int)io->i_part >= lp->d_npartitions ||
	    (io->i_boff = lp->d_partitions[io->i_part].p_offset) == -1)
		return (EPART);
	return (0);
}

int
udcmd(op, io)
	int op;
	register struct iob *io;
{
	register struct uda1 *u = &uda1;
	register struct mscp *mp;
	int i;

	u->uda1_cmd.mscp_opcode = op;
	u->uda1_cmd.mscp_msglen = MSCP_MSGLEN;
	u->uda1_rsp.mscp_msglen = MSCP_MSGLEN;
	u->uda1_ca.ca_rspdsc |= MSCP_OWN|MSCP_INT;
	u->uda1_ca.ca_cmddsc |= MSCP_OWN|MSCP_INT;
	i = udaddr[io->i_adapt][io->i_ctlr]->udaip;
	mp = &u->uda1_rsp;
	for (;;) {
		if (u->uda1_ca.ca_cmdint)
			u->uda1_ca.ca_cmdint = 0;
		if (u->uda1_ca.ca_rspint == 0)
			continue;
		u->uda1_ca.ca_rspint = 0;
		if (mp->mscp_opcode == (op | M_OP_END))
			break;
		printf("unexpected rsp type %x op %x ignored\n",
			MSCP_MSGTYPE(mp->mscp_msgtc), mp->mscp_opcode);
		u->uda1_ca.ca_rspdsc |= MSCP_OWN | MSCP_INT;
	}
	if ((mp->mscp_status&M_ST_MASK) != M_ST_SUCCESS)
		return (-1);
	return (0);
}

rastrategy(io, func)
	register struct iob *io;
{
	register struct mscp *mp;
	int ubinfo;

	ubinfo = ubasetup(io, 1);
	mp = &uda1.uda1_cmd;
	mp->mscp_unit = io->i_unit;
	mp->mscp_seq.seq_lbn = io->i_bn;
	mp->mscp_seq.seq_bytecount = io->i_cc;
	mp->mscp_seq.seq_buffer = (ubinfo & 0x3ffff) | (((ubinfo>>28)&0xf)<<24);
	if (udcmd(func == READ ? M_OP_READ : M_OP_WRITE, io)) {
		printf("ra: I/O error\n");
		ubafree(io, ubinfo);
		return(-1);
	}
	ubafree(io, ubinfo);
	return(io->i_cc);
}

#ifdef COMPAT_42
u_long ra25_off[] = { 0, 15884, 0, -1, -1, -1, 25916, -1 };
u_long rx50_off[] = { 0, 0, 0, 0, 0, 0, 0, 0 };
u_long rd52_off[] = { 0, 15884, 0, 0, 0, 0, 25650, 0 };
u_long rd53_off[] = { 0, 15884, 0, 0, 0, 33440, 49324, 15884 };
u_long ra60_off[] = { 0, 15884, 0, 49324, 131404, 49324, 242606, 49324 };
u_long ra80_off[] = { 0, 15884, 0, -1, 49324, 49324, 49910, 131404 };
#ifndef	UCBRA
#ifdef RA_COMPAT
u_long ra81_off[] = { 0, 16422, 0, 49324, 131404, 412490, 375564, 83538 };
#else
u_long ra81_off[] = { 0, 16422, 0, 375564, 391986, 699720, 375564, 83538 };
#endif
#else
u_long ra81_off[] = { 0, 15884, 0, 242606, 258490, 565690, 242606, 49324 };
#endif

u_long	*ra_off[] = {
	0,		/* 0 = unused */
	ra80_off,	/* 1 = ra80 */
	ra25_off,	/* 2 = rc25 removable */
	ra25_off,	/* 3 = rc25 fixed */
	ra60_off,	/* 4 = ra60 */
	ra81_off,	/* 5 = ra81 */
	0,		/* 6 = ? */
	rx50_off,	/* 7 = rx50 */
	rd52_off,	/* 8 = rd52 */
	rd53_off,	/* 9 = rd53 */
};

#define NOFFS	(sizeof(ra_off)/sizeof(ra_off[0]))

ramaptype(io, lp)
	register struct iob *io;
	register struct disklabel *lp;
{
	register struct partition *pp;
	register u_int i;
	register u_long *off;

	if ((i = ratype[io->i_adapt][io->i_ctlr][io->i_unit]) >= NOFFS ||
	    (off = ra_off[i]) == 0) {
		printf("ra%d: ra type %d unsupported\n", io->i_unit, i);
		lp->d_npartitions = 0;
		return;
	}
	lp->d_npartitions = 8;
	for (pp = lp->d_partitions, i = 0; i < 8; pp++, i++)
		pp->p_offset = *off++;
}
#endif
