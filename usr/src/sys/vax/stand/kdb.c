/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that this notice is preserved and that due credit is given
 * to the University of California at Berkeley. The name of the University
 * may not be used to endorse or promote products derived from this
 * software without specific prior written permission. This software
 * is provided ``as is'' without express or implied warranty.
 *
 *	@(#)kdb.c	7.1 (Berkeley) %G%
 */

/*
 * KDB50/RAxx disk device driver
 */
#include "../machine/pte.h"

#include "param.h"
#include "inode.h"
#include "fs.h"
#include "disklabel.h"

#include "saio.h"
#include "savax.h"

#define	NKRA		8	/* max unit number on ctlr */
#define	SECTSIZ		512	/* sector size in bytes */

/*
 * Parameters for the communications area
 */
#define	NRSP	1
#define	NCMD	1

#include "../vax/bireg.h"
#include "../vax/kdbreg.h"
#include "../vax/mscp.h"

struct kdb {
	struct kdbca	kdb_ca;
	struct mscp	kdb_rsp;
	struct mscp	kdb_cmd;
} kdb;

int	kdbinit[MAXNKDB];
char	kratype[MAXNKDB * NKRA];
struct	disklabel kralabel[MAXNUBA * NKRA];
char	lbuf[SECTSIZ];

kraopen(io)
	register struct iob *io;
{
	register struct mscp *mp;
	register struct kdb_regs *kr;
	register struct disklabel *lp;
	register int i, unit;
	daddr_t off;

	i = io->i_unit >> 3;
	if (i >= nkdb) {
		printf("nonexistent device");
		return (ENXIO);
	}
	kr = (struct kdb_regs *)kdbaddr[i];
	if (kdbinit[i] == 0) {
		kr->kdb_bi.bi_csr |= BICSR_NRST;
		DELAY(10000);	/* ??? */
		/* clear any bus errors */
		kr->kdb_bi.bi_ber = ~(BIBER_MBZ|BIBER_NMR|BIBER_UPEN);
		while ((kr->kdb_sa & KDB_STEP1) == 0)
			;
		kr->kdb_bi.bi_bcicsr |= BCI_STOPEN | BCI_IDENTEN;
		kr->kdb_sw = KDB_ERR;
		while ((kr->kdb_sa & KDB_STEP2) == 0)
			;
		kr->kdb_sw = (int)&kdb.kdb_ca.ca_rspdsc[0];
		while ((kr->kdb_sa & KDB_STEP3) == 0)
			;
		kr->kdb_sw = (int)&kdb.kdb_ca.ca_rspdsc[0] >> 16;
		while ((kr->kdb_sa & KDB_STEP4) == 0)
			;
		kr->kdb_sw = KDB_GO;
		kdb.kdb_ca.ca_rspdsc[0] = (long)&kdb.kdb_rsp.mscp_cmdref;
		kdb.kdb_ca.ca_cmddsc[0] = (long)&kdb.kdb_cmd.mscp_cmdref;
		if (kdbcmd(kr, M_OP_SETCTLRC)) {
			printf("open error, SETCTLRC");
			return (EIO);
		}
		kdbinit[i] = 1;
	}
	unit = io->i_unit;
	lp = &kralabel[unit];
	if (kratype[unit] == 0) {
		struct iob tio;

		kdb.kdb_cmd.mscp_unit = UNITTODRIVE(unit);
		if (kdbcmd(kr, M_OP_ONLINE)) {
			printf("open error, ONLINE");
			return (EIO);
		}
		kratype[unit] = kdb.kdb_rsp.mscp_onle.onle_drivetype;
		tio = *io;
		tio.i_bn = LABELSECTOR;
		tio.i_ma = lbuf;
		tio.i_cc = SECTSIZ;
		tio.i_flgs |= F_RDDATA;
		if (krastrategy(&tio, READ) != SECTSIZ) {
			printf("can't read disk label\n");
			return (EIO);
		}
		*lp = *(struct disklabel *)(lbuf + LABELOFFSET);
		if (lp->d_magic != DISKMAGIC || lp->d_magic2 != DISKMAGIC) {
			printf("kra%d: unlabeled\n", unit);
#ifdef COMPAT_42
			kramaptype(io, lp);
#else
			return (ENXIO);
#endif
		}
	}
	if ((unsigned)io->i_boff >= lp->d_npartitions ||
	    (io->i_boff = lp->d_partitions[io->i_boff].p_offset) == -1) {
		printf("kra: bad partition");
		return (EUNIT);
	}
	return (0);
}

kdbcmd(kr, op)
	struct kdb_regs *kr;
	int op;
{
	register struct kdb *k = &kdb;
	register struct mscp *mp;
	int i;

	k->kdb_cmd.mscp_opcode = op;
	k->kdb_rsp.mscp_msglen = MSCP_MSGLEN;
	k->kdb_cmd.mscp_msglen = MSCP_MSGLEN;
	k->kdb_ca.ca_rspdsc[0] |= MSCP_OWN | MSCP_INT;
	k->kdb_ca.ca_cmddsc[0] |= MSCP_OWN | MSCP_INT;
	i = kr->kdb_ip;
	mp = &k->kdb_rsp;
	for (;;) {
		if (k->kdb_ca.ca_cmdint)
			k->kdb_ca.ca_cmdint = 0;
		if (k->kdb_ca.ca_rspint == 0)
			continue;
		k->kdb_ca.ca_rspint = 0;
		if (mp->mscp_opcode == (op | M_OP_END))
			break;
		printf("unexpected rsp type %x op %x ignored\n",
			MSCP_MSGTYPE(mp->mscp_msgtc), mp->mscp_opcode);
	}
	if ((mp->mscp_status & M_ST_MASK) != M_ST_SUCCESS)
		return (-1);
	return (0);
}

krastrategy(io, func)
	register struct iob *io;
{
	register struct mscp *mp;

	mp = &kdb.kdb_cmd;
	mp->mscp_unit = io->i_unit & 7;
	mp->mscp_seq.seq_lbn = io->i_bn;
	mp->mscp_seq.seq_bytecount = io->i_cc;
	mp->mscp_seq.seq_buffer = (long)io->i_ma | KDB_PHYS;
	if (kdbcmd((struct kdb_regs *)kdbaddr[io->i_unit >> 3],
		    func == READ ? M_OP_READ : M_OP_WRITE)) {
		printf("kra: I/O error\n");
		return (-1);
	}
	return (io->i_cc);
}

/*ARGSUSED*/
kraioctl(io, cmd, arg)
	struct iob *io;
	int cmd;
	caddr_t arg;
{

	return (ECMD);
}

#ifdef COMPAT_42
u_long kra25_off[] = { 0, 15884, 0, -1, -1, -1, 25916, -1 };
u_long kra60_off[] = { 0, 15884, 0, 49324, 131404, 49324, 242606, 49324 };
u_long kra80_off[] = { 0, 15884, 0, -1, 49324, 49324, 49910, 131404 };
u_long kra81_off[] = { 0, 15884, 0, 131404, 49324, 498790, 563050, 131404 };
u_long *kra_off[] = {
	0,
	kra80_off,		/* 1 = ra80 */
	kra25_off,		/* 2 = rc25-r */
	kra25_off,		/* 3 = rc25-r */
	kra60_off,		/* 4 = ra60 */
	kra81_off,		/* 5 = ra81 */
};
#define	NOFFS (sizeof(kra_off) / sizeof(kra_off[0]))

kramaptype(io, lp)
	register struct iob *io;
	register struct disklabel *lp;
{
	register struct partition *pp;
	register u_int i;
	register u_long *off;

	if ((i = kratype[io->i_unit]) >= NOFFS || (off = kra_off[i]) == 0) {
		printf("kra%d: type %d unsupported\n", io->i_unit, i);
		lp->d_npartitions = 0;
		return;
	}
	lp->d_npartitions = 8;
	for (pp = lp->d_partitions, i = 0; i < 8; pp++, i++)
		pp->p_offset = *off++;
}
#endif
