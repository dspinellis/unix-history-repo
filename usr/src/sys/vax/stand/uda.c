/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)uda.c	7.3 (Berkeley) %G%
 */

/*
 * UDA50/RAxx disk device driver
 */
#include "../machine/pte.h"

#include "../h/param.h"
#include "../h/inode.h"
#include "../h/fs.h"
#include "../h/disklabel.h"

#include "saio.h"
#include "savax.h"

#define	NRA		4	/* drives per controller */
#define	SECTSIZ 	512	/* sector size in bytes */
/*
 * Parameters for the communications area
 */
#define	NRSPL2	0
#define	NCMDL2	0
#define	NRSP	(1<<NRSPL2)
#define	NCMD	(1<<NCMDL2)

#include "../vaxuba/udareg.h"
#include "../vaxuba/ubareg.h"
#include "../vax/mscp.h"

u_short udastd[] = { 0772150 };

struct iob	cudbuf;

/*
 * Per-controller structures use dimension MAXNUBA,
 * as only one controller per UNIBUS is supported.
 */
struct udadevice *udaddr[MAXNUBA] = { 0 };

struct uda {
	struct udaca	uda_ca;
	struct mscp	uda_rsp;
	struct mscp	uda_cmd;
} uda;

struct	uda *ud_ubaddr[MAXNUBA];	/* Unibus address of uda structure */
struct	disklabel ralabel[MAXNUBA * NRA];
static	int ratype[MAXNUBA * NRA];
char	lbuf[SECTSIZ];
struct	mscp *udcmd();

raopen(io)
	register struct iob *io;
{
	register struct mscp *mp;
	register struct disklabel *lp;
	register struct udadevice *addr;
	register struct uda *ubaddr;
	register unit;
	static int udainit[MAXNUBA], udadriveinit[MAXNUBA * NRA];
	int uba;

	unit = io->i_unit;
	uba = UNITTOUBA(unit);
	if (udaddr[uba] == 0)
		udaddr[uba] = (struct udadevice *)ubamem(unit, udastd[0]);
	addr = udaddr[uba];
	if (badaddr((char *)addr, sizeof(short))) {
		printf("nonexistent device\n");
		return (ENXIO);
	}
	if (ud_ubaddr[uba] == 0) {
		/*
		 * Initialize cudbuf.i_unit so that controllers
		 * on UNIBUSes other than 0 can be used.
		 */
		cudbuf.i_unit = unit;
		cudbuf.i_ma = (caddr_t)&uda;
		cudbuf.i_cc = sizeof(uda);
		ud_ubaddr[uba] = (struct uda *)ubasetup(&cudbuf, 2);
	}
	ubaddr = ud_ubaddr[uba];
	if (udainit[uba] == 0) {
		addr->udaip = 0;
		while ((addr->udasa & UDA_STEP1) == 0)
			;
		addr->udasa = UDA_ERR;
		while ((addr->udasa & UDA_STEP2) == 0)
			;
		addr->udasa = (short)&ubaddr->uda_ca.ca_ringbase;
		while ((addr->udasa & UDA_STEP3) == 0)
			;
		addr->udasa =
		    (short)(((int)&ubaddr->uda_ca.ca_ringbase) >> 16);
		while ((addr->udasa & UDA_STEP4) == 0)
			;
		addr->udasa = UDA_GO;
		uda.uda_ca.ca_rspdsc[0] = (long)&ubaddr->uda_rsp.mscp_cmdref;
		uda.uda_ca.ca_cmddsc[0] = (long)&ubaddr->uda_cmd.mscp_cmdref;
		uda.uda_cmd.mscp_cntflgs = 0;
		if (udcmd(M_OP_STCON, io) == 0) {
			printf("ra: open error, STCON\n");
			return (EIO);
		}
	}
	lp = &ralabel[unit];
	if (udadriveinit[unit] == 0) {
		struct iob tio;

		uda.uda_cmd.mscp_unit = UNITTODRIVE(unit);
		if (udcmd(M_OP_ONLIN, io) == 0) {
			printf("ra: open error, ONLIN\n");
			return (EIO);
		}
		udainit[uba] = 1;
		tio = *io;
		tio.i_bn = LABELSECTOR;
		tio.i_ma = lbuf;
		tio.i_cc = SECTSIZ;
		tio.i_flgs |= F_RDDATA;
		if (rastrategy(&tio, READ) != SECTSIZ) {
			printf("can't read disk label\n");
			return (EIO);
		}
		if (lp->d_magic != DISKMAGIC || lp->d_magic2 != DISKMAGIC) {
			printf("ra%d: unlabeled\n", unit);
#ifdef COMPAT_42
			ramaptype(io, lp);
#else
			return (ENXIO);
#endif
		} else
			*lp = *(struct disklabel *)(lbuf + LABELOFFSET);
	}
	if ((unsigned)io->i_boff >= lp->d_npartitions ||
	    (io->i_boff = lp->d_partitions[io->i_boff].p_offset) == -1) {
		printf("ra: bad partition\n");
		return (EUNIT);
	}
	return (0);
}

struct mscp *
udcmd(op, io)
	int op;
	register struct iob *io;
{
	struct mscp *mp;
	int i;

	uda.uda_cmd.mscp_opcode = op;
	uda.uda_rsp.mscp_header.uda_msglen = sizeof (struct mscp);
	uda.uda_cmd.mscp_header.uda_msglen = sizeof (struct mscp);
	uda.uda_ca.ca_rspdsc[0] |= UDA_OWN|UDA_INT;
	uda.uda_ca.ca_cmddsc[0] |= UDA_OWN|UDA_INT;
	i = udaddr[UNITTOUBA(io->i_unit)]->udaip;
	for (;;) {
		if (uda.uda_ca.ca_cmdint)
			uda.uda_ca.ca_cmdint = 0;
		if (uda.uda_ca.ca_rspint)
			break;
	}
	uda.uda_ca.ca_rspint = 0;
	mp = &uda.uda_rsp;
	if (mp->mscp_opcode != (op|M_OP_END) ||
	    (mp->mscp_status&M_ST_MASK) != M_ST_SUCC)
		return(0);
	if (mp->mscp_opcode == (M_OP_ONLIN|M_OP_END))
		ratype[io->i_unit] = mp->mscp_mediaid & 0x7f;
	return(mp);
}

rastrategy(io, func)
	register struct iob *io;
{
	register struct mscp *mp;
	int ubinfo;

	ubinfo = ubasetup(io, 1);
	mp = &uda.uda_cmd;
	mp->mscp_lbn = io->i_bn;
	mp->mscp_unit = UNITTODRIVE(io->i_unit);
	mp->mscp_bytecnt = io->i_cc;
	mp->mscp_buffer = (ubinfo & 0x3ffff) | (((ubinfo>>28)&0xf)<<24);
	if ((mp = udcmd(func == READ ? M_OP_READ : M_OP_WRITE, io)) == 0) {
		printf("ra: I/O error\n");
		ubafree(io, ubinfo);
		return(-1);
	}
	ubafree(io, ubinfo);
	return(io->i_cc);
}

/*ARGSUSED*/
raioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{

	return (ECMD);
}

#ifdef COMPAT_42
u_long ra25_off[] = { 0, 15884, 0, -1, -1, -1, 25916, -1 };
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

ramaptype(io, lp)
	register struct iob *io;
	register struct disklabel *lp;
{
	register struct partition *pp;
	register i;
	register u_long *off;

	switch (ratype[io->i_unit]) {
	case    25:
		off = ra25_off;
		break;
	case    60:
		off = ra60_off;
		break;
	case    80:
		off = ra80_off;
		break;
	case    81:
		off = ra81_off;
		break;
	default:
		printf("uda%d: don't support ra%d's\n",
		    io->i_unit, ratype[io->i_unit]);
		lp->d_npartitions = 0;
		return;
	}
	lp->d_npartitions = 8;
	pp = lp->d_partitions;
	for (i = 0; i < 8; i++, pp++)
		pp->p_offset = *off++;
}
#endif
