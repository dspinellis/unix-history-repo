/*
 * Copyright (c) 1988 Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)kdb.c	7.8 (Berkeley) %G%
 */

/*
 * KDB50/RAxx disk device driver
 */
#include "../include/pte.h"

#include "sys/param.h"
#include "sys/disklabel.h"

#include "stand/saio.h"
#include "savax.h"

/*
 * N.B.: on KDB50, controller == adapter
 * here we just use the controller number
 */

#define	NKRA		8	/* max drive number */
#define	SECTSIZ		512	/* sector size in bytes */

/*
 * Parameters for the communications area:
 * command and response rings both one entry.
 */
#define	NRSP	1
#define	NCMD	1

#include "../bi/bireg.h"
#include "../bi/kdbreg.h"
#include "../vax/mscp.h"

struct kdb {
	struct kdbca	kdb_ca;
	struct mscp	kdb_rsp;
	struct mscp	kdb_cmd;
} kdb;

int	kdbinit[MAXNKDB];
struct	disklabel kralabel[MAXNKDB][NKRA];
u_long	kramedia[MAXNKDB][NKRA];
char	lbuf[SECTSIZ];

kraopen(io)
	register struct iob *io;
{
	register struct kdb_regs *kr;
	register struct disklabel *lp;
	register int ctlr, unit;
	struct iob tio;

	if ((u_int)(ctlr = io->i_ctlr) >= nkdb)
		return (EADAPT);
	if ((u_int)(unit = io->i_unit) >= NKRA)
		return (EUNIT);
	kr = (struct kdb_regs *)kdbaddr[ctlr];
	if (kdbinit[ctlr] == 0) {
		kr->kdb_bi.bi_csr |= BICSR_NRST;
		DELAY(10000);	/* ??? */
		/* clear any bus errors */
		kr->kdb_bi.bi_ber = ~(BIBER_MBZ|BIBER_NMR|BIBER_UPEN);
		while ((kr->kdb_sa & KDB_STEP1) == 0)
			/* void */;
		kr->kdb_bi.bi_bcicsr |= BCI_STOPEN | BCI_IDENTEN;
		kr->kdb_sw = KDB_ERR;
		while ((kr->kdb_sa & KDB_STEP2) == 0)
			/* void */;
		kr->kdb_sw = (int)&kdb.kdb_ca.ca_rspdsc[0];
		while ((kr->kdb_sa & KDB_STEP3) == 0)
			/* void */;
		kr->kdb_sw = (int)&kdb.kdb_ca.ca_rspdsc[0] >> 16;
		while ((kr->kdb_sa & KDB_STEP4) == 0)
			/* void */;
		kr->kdb_sw = KDB_GO;
		kdb.kdb_ca.ca_rspdsc[0] = (long)&kdb.kdb_rsp.mscp_cmdref;
		kdb.kdb_ca.ca_cmddsc[0] = (long)&kdb.kdb_cmd.mscp_cmdref;
		if (kdbcmd(M_OP_SETCTLRC, io)) {
			printf("kra: open error, SETCTLRC\n");
			return (ENXIO);
		}
		kdbinit[ctlr] = 1;
	}
	lp = &kralabel[ctlr][unit];
	if (kramedia[ctlr][unit] == 0) {
		kdb.kdb_cmd.mscp_unit = unit;
		if (kdbcmd(M_OP_ONLINE, io)) {
			printf("kra: open error, ONLINE\n");
			return (ENXIO);
		}
		kramedia[ctlr][unit] = kdb.kdb_rsp.mscp_onle.onle_mediaid;
		tio = *io;
		tio.i_bn = LABELSECTOR;
		tio.i_ma = lbuf;
		tio.i_cc = SECTSIZ;
		tio.i_flgs |= F_RDDATA;
		if (krastrategy(&tio, READ) != SECTSIZ)
			return (ERDLAB);
		*lp = *(struct disklabel *)(lbuf + LABELOFFSET);
		if (lp->d_magic != DISKMAGIC || lp->d_magic2 != DISKMAGIC) {
#ifdef COMPAT_42
			printf("kra%d: unlabeled\n", unit);
			kramaptype(io, lp);
#else
			return (EUNLAB);
#endif
		}
	}
	if ((u_int)io->i_part >= lp->d_npartitions ||
	    (io->i_boff = lp->d_partitions[io->i_part].p_offset) == -1)
		return (EPART);
	return (0);
}

kdbcmd(op, io)
	int op;
	struct iob *io;
{
	register struct kdb *k = &kdb;
	register struct mscp *mp;
	register int i;

	k->kdb_cmd.mscp_opcode = op;
	k->kdb_rsp.mscp_msglen = MSCP_MSGLEN;
	k->kdb_cmd.mscp_msglen = MSCP_MSGLEN;
	k->kdb_ca.ca_rspdsc[0] |= MSCP_OWN | MSCP_INT;
	k->kdb_ca.ca_cmddsc[0] |= MSCP_OWN | MSCP_INT;
	i = ((struct kdb_regs *)kdbaddr[io->i_ctlr])->kdb_ip;
#ifdef lint
	i = i;
#endif
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
		k->kdb_ca.ca_rspdsc[0] |= MSCP_OWN | MSCP_INT;
	}
	if ((mp->mscp_status & M_ST_MASK) != M_ST_SUCCESS)
		return (-1);
	return (0);
}

krastrategy(io, func)
	register struct iob *io;
	int func;
{
	register struct mscp *mp;

	mp = &kdb.kdb_cmd;
	mp->mscp_unit = io->i_unit;
	mp->mscp_seq.seq_lbn = io->i_bn;
	mp->mscp_seq.seq_bytecount = io->i_cc;
	mp->mscp_seq.seq_buffer = (long)io->i_ma | KDB_PHYS;
	if (kdbcmd(func == READ ? M_OP_READ : M_OP_WRITE, io)) {
		printf("kra: I/O error\n");
		return (-1);
	}
	return (io->i_cc);
}

#ifdef COMPAT_42
u_long kra60_off[] = { 0, 15884, 0, 49324, 131404, 49324, 242606, 49324 };
#define kra70_off kra60_off
u_long kra80_off[] = { 0, 15884, 0, -1, 49324, 49324, 49910, 131404 };
u_long kra81_off[] = { 0, 15884, 0, 131404, 49324, 498790, 563050, 131404 };
u_long kra82_off[] = { 0, 15884, 0, 375345, 391590, 699390, 375345, 83790 }; 

struct mediamap {
	u_long	id;		/* media ID */
	u_long	*off;		/* offsets */
} kra_map[] = {
	{ MSCP_MKDRIVE2('R', 'A', 60),		kra60_off },
	{ MSCP_MKDRIVE2('R', 'A', 70),		kra70_off },
	{ MSCP_MKDRIVE2('R', 'A', 80),		kra80_off },
	{ MSCP_MKDRIVE2('R', 'A', 81),		kra81_off },
	{ MSCP_MKDRIVE2('R', 'A', 82),		kra82_off },
	0
};

kramaptype(io, lp)
	register struct iob *io;
	register struct disklabel *lp;
{
	register struct partition *pp;
	register u_long i;
	register struct mediamap *map;

	i = MSCP_MEDIA_DRIVE(kramedia[io->i_ctlr][io->i_unit]);
	for (map = kra_map; map->id != 0; map++) {
		if (map->id == i) {
			lp->d_npartitions = 8;
			for (pp = lp->d_partitions, i = 0; i < 8; pp++, i++)
				pp->p_offset = map->off[i];
			return;
		}
	}
	printf("kra%d: media type 0x%x unsupported\n", io->i_unit, i);
	lp->d_npartitions = 0;
}
#endif
