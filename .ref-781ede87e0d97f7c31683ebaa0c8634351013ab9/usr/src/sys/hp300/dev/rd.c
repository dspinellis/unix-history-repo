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
 * from: Utah $Hdr: rd.c 1.30 89/09/17$
 *
 *	@(#)rd.c	7.5 (Berkeley) %G%
 */

/*
 * CS80/SS80 disk driver
 */
#include "rd.h"
#if NRD > 0

#include "sys/param.h"
#include "sys/systm.h"
#include "sys/errno.h"
#include "sys/dkstat.h"
#include "sys/disklabel.h"
#include "sys/buf.h"
#include "sys/uio.h"

#include "device.h"
#include "rdreg.h"

#include "vm/vm_param.h"
#include "vm/pmap.h"
#include "vm/vm_prot.h"

int	rdinit(), rdstart(), rdgo(), rdintr();
struct	driver rddriver = {
	rdinit, "rd", rdstart, rdgo, rdintr,
};

struct	rd_softc {
	struct	hp_device *sc_hd;
	int	sc_flags;
	short	sc_type;
	short	sc_punit;
	char	*sc_addr;
	int	sc_resid;
	u_int	sc_wpms;
	struct	rdinfo *sc_info;
	struct	devqueue sc_dq;
	struct	rd_iocmd sc_ioc;
	struct	rd_rscmd sc_rsc;
	struct	rd_stat sc_stat;
	struct	rd_ssmcmd sc_ssmc;
	struct	rd_srcmd sc_src;
	struct	rd_clearcmd sc_clear;
} rd_softc[NRD];

/* sc_flags values */
#define	RDF_ALIVE	0x1
#define	RDF_SEEK	0x2
#define RDF_SWAIT	0x4

struct	size {
	daddr_t	nblocks;
	int	cyloff;
};

#ifdef DEBUG
int rddebug = 0x80;
#define RDB_FOLLOW	0x01
#define RDB_STATUS	0x02
#define RDB_IDENT	0x04
#define RDB_IO		0x08
#define RDB_ASYNC	0x10
#define RDB_ERROR	0x80
#define RDB_DUMP	0x80000000

struct rdstats {
	long	rdretries;
	long	rdresets;
	long	rdtimeouts;
	long	rdpolltries;
	long	rdpollwaits;
} rdstats[NRD];

/* error message tables */
char *err_reject[] = {
	0, 0,
	"channel parity error",		/* 0x2000 */
	0, 0,
	"illegal opcode",		/* 0x0400 */
	"module addressing",		/* 0x0200 */
	"address bounds",		/* 0x0100 */
	"parameter bounds",		/* 0x0080 */
	"illegal parameter",		/* 0x0040 */
	"message sequence",		/* 0x0020 */
	0,
	"message length",		/* 0x0008 */
	0, 0, 0
};

char *err_fault[] = {
	0,
	"cross unit",			/* 0x4000 */
	0,
	"controller fault",		/* 0x1000 */
	0, 0,
	"unit fault",			/* 0x0200 */
	0,
	"diagnostic result",		/* 0x0080 */
	0,
	"operator release request",	/* 0x0020 */
	"diagnostic release request",	/* 0x0010 */
	"internal maintenance release request",	/* 0x0008 */
	0,
	"power fail",			/* 0x0002 */
	"retransmit"			/* 0x0001 */
};

char *err_access[] = {
	"illegal parallel operation",	/* 0x8000 */
	"uninitialized media",		/* 0x4000 */
	"no spares available",		/* 0x2000 */
	"not ready",			/* 0x1000 */
	"write protect",		/* 0x0800 */
	"no data found",		/* 0x0400 */
	0, 0,
	"unrecoverable data overflow",	/* 0x0080 */
	"unrecoverable data",		/* 0x0040 */
	0,
	"end of file",			/* 0x0010 */
	"end of volume",		/* 0x0008 */
	0, 0, 0
};

char *err_info[] = {
	"operator release request",	/* 0x8000 */
	"diagnostic release request",	/* 0x4000 */
	"internal maintenance release request",	/* 0x2000 */
	"media wear",			/* 0x1000 */
	"latency induced",		/* 0x0800 */
	0, 0,
	"auto sparing invoked",		/* 0x0100 */
	0,
	"recoverable data overflow",	/* 0x0040 */
	"marginal data",		/* 0x0020 */
	"recoverable data",		/* 0x0010 */
	0,
	"maintenance track overflow",	/* 0x0004 */
	0, 0
};
#endif

/*
 * CS/80 partitions.  We reserve the first cylinder for a LIF
 * style boot directory (the 8k allowed in the BSD filesystem
 * is just way too small).  This boot area is outside of all but
 * the C partition.  This implies that you cannot use the C 
 * partition on a bootable disk since the filesystem would overlay
 * the boot area.  You must use the A partition.
 *
 * These maps support four basic layouts:
 *
 *	A/B/G:   This is the "traditional" setup for a bootable disk.
 *	         A is the root partition, B the swap, and G a user partition.
 *	A/D/H:   This is a setup for bootable systems requiring more swap
 *		 (e.g. those who use HPCL).  It has A as the root, D as a
 *		 larger swap, and H as a smaller user partition.
 *	A/D/E/F: Similar to A/D/H with E and F breaking H into two partitions.
 *		 E could be used for /usr and F for users.
 *	C:       This gives a single, non-bootable, large user filesystem.
 *	         Good for second drives on a machine (e.g. /usr/src).
 */
struct size rd7945A_sizes[8] = {
	RDSZ(15904),	1,		/* A=cyl 1 thru 142 */
	RDSZ(20160),	143,		/* B=cyl 143 thru 322 */
	RDSZ(108416),	0,		/* C=cyl 0 thru 967 */
	RDSZ(40320),	143,		/* D=cyl 143 thru 502 */
	RDSZ(0),	0,		/* E=<undefined> */
	RDSZ(0),	0,		/* F=<undefined> */
	RDSZ(72240),	323,		/* G=cyl 323 thru 967 */
	RDSZ(52080),	503,		/* H=cyl 503 thru 967 */
}, rd9134D_sizes[8] = {
	RDSZ(15936),	1,		/* A=cyl 1 thru 166 */
	RDSZ(13056),	167,		/* B=cyl 167 thru 302 */
	RDSZ(29088),	0,		/* C=cyl 0 thru 302 */
	RDSZ(0),	0,		/* D=<undefined> */
	RDSZ(0),	0,		/* E=<undefined> */
	RDSZ(0),	0,		/* F=<undefined> */
	RDSZ(0),	0,		/* G=<undefined> */
	RDSZ(0),	0,		/* H=<undefined> */
}, rd9122S_sizes[8] = {
	RDSZ(0),	0,		/* A=<undefined> */
	RDSZ(0),	0,		/* B=<undefined> */
	RDSZ(1232),	0,		/* C=cyl 0 thru 76 */
	RDSZ(0),	0,		/* D=<undefined> */
	RDSZ(0),	0,		/* E=<undefined> */
	RDSZ(0),	0,		/* F=<undefined> */
	RDSZ(0),	0,		/* G=<undefined> */
	RDSZ(0),	0,		/* H=<undefined> */
}, rd7912P_sizes[8] = {
	RDSZ(15904),	0,		/* A=cyl 1 thru 71 */
	RDSZ(22400),	72,		/* B=cyl 72 thru 171 */
	RDSZ(128128),	0,		/* C=cyl 0 thru 571 */
	RDSZ(42560),	72,		/* D=cyl 72 thru 261 */
	RDSZ(0),	292,		/* E=<undefined> */
	RDSZ(0),	542,		/* F=<undefined> */
	RDSZ(89600),	172,		/* G=cyl 221 thru 571 */
	RDSZ(69440),	262,		/* H=cyl 262 thru 571 */
}, rd7914P_sizes[8] = {
	RDSZ(15904),	1,		/* A=cyl 1 thru 71 */
	RDSZ(40320),	72,		/* B=cyl 72 thru 251 */
	RDSZ(258048),	0,		/* C=cyl 0 thru 1151 */
	RDSZ(64960),	72,		/* D=cyl 72 thru 361 */
	RDSZ(98560),	362,		/* E=cyl 362 thru 801 */
	RDSZ(78400),	802,		/* F=cyl 802 thru 1151 */
	RDSZ(201600),	252,		/* G=cyl 221 thru 1151 */
	RDSZ(176960),	362,		/* H=cyl 362 thru 1151 */
}, rd7933H_sizes[8] = {
	RDSZ(16146),	1,		/* A=cyl 1 thru 27 */
	RDSZ(66976),	28,		/* B=cyl 28 thru 139 */
	RDSZ(789958),	0,		/* C=cyl 0 thru 1320 */
	RDSZ(16146),	140,		/* D=cyl 140 thru 166 */
	RDSZ(165646),	167,		/* E=cyl 167 thru 443 */
	RDSZ(165646),	444,		/* F=cyl 444 thru 720 */
	RDSZ(706238),	140,		/* G=cyl 140 thru 1320 */
	RDSZ(358800),	721,		/* H=cyl 721 thru 1320 */
}, rd9134L_sizes[8] = {
	RDSZ(15920),	1,		/* A=cyl 1 thru 199 */
	RDSZ(20000),	200,		/* B=cyl 200 thru 449 */
	RDSZ(77840),	0,		/* C=cyl 0 thru 972 */
	RDSZ(32000),	200,		/* D=cyl 200 thru 599 */
	RDSZ(0),	0,		/* E=<undefined> */
	RDSZ(0),	0,		/* F=<undefined> */
	RDSZ(41840),	450,		/* G=cyl 450 thru 972 */
	RDSZ(29840),	600,		/* H=cyl 600 thru 972 */
}, rd7957A_sizes[8] = {
	RDSZ(16016),	1,		/* A=cyl 1 thru 104 */
	RDSZ(24640),	105,		/* B=cyl 105 thru 264 */
	RDSZ(159544),	0,		/* C=cyl 0 thru 1035 */
	RDSZ(42350),	105,		/* D=cyl 105 thru 379 */
	RDSZ(54824),	380,		/* E=cyl 380 thru 735 */
	RDSZ(46200),	736,		/* F=cyl 736 thru 1035 */
	RDSZ(118734),	265,		/* G=cyl 265 thru 1035 */
	RDSZ(101024),	380,		/* H=cyl 380 thru 1035 */
}, rd7958A_sizes[8] = {
	RDSZ(16128),	1,		/* A=cyl 1 thru 64 */
	RDSZ(32256),	65,		/* B=cyl 65 thru 192 */
	RDSZ(255276),	0,		/* C=cyl 0 thru 1012 */
	RDSZ(48384),	65,		/* D=cyl 65 thru 256 */
	RDSZ(100800),	257,		/* E=cyl 257 thru 656 */
	RDSZ(89712),	657,		/* F=cyl 657 thru 1012 */
	RDSZ(206640),	193,		/* G=cyl 193 thru 1012 */
	RDSZ(190512),	257,		/* H=cyl 257 thru 1012 */
}, rd7957B_sizes[8] = {
	RDSZ(16002),	1,		/* A=cyl 1 thru 127 */
	RDSZ(32760),	128,		/* B=cyl 128 thru 387 */
	RDSZ(159894),	0,		/* C=cyl 0 thru 1268 */
	RDSZ(49140),	128,		/* D=cyl 128 thru 517 */
	RDSZ(50400),	518,		/* E=cyl 518 thru 917 */
	RDSZ(44226),	918,		/* F=cyl 918 thru 1268 */
	RDSZ(111006),	388,		/* G=cyl 388 thru 1268 */
	RDSZ(94626),	518,		/* H=cyl 518 thru 1268 */
}, rd7958B_sizes[8] = {
	RDSZ(16254),	1,		/* A=cyl 1 thru 43 */
	RDSZ(32886),	44,		/* B=cyl 44 thru 130 */
	RDSZ(297108),	0,		/* C=cyl 0 thru 785 */
	RDSZ(49140),	44,		/* D=cyl 44 thru 173 */
	RDSZ(121716),	174,		/* E=cyl 174 thru 495 */
	RDSZ(109620),	496,		/* F=cyl 496 thru 785 */
	RDSZ(247590),	131,		/* G=cyl 131 thru 785 */
	RDSZ(231336),	174,		/* H=cyl 174 thru 785 */
}, rd7959B_sizes[8] = {
	RDSZ(16254),	1,		/* A=cyl 1 thru 43 */
	RDSZ(49140),	44,		/* B=cyl 44 thru 173 */
	RDSZ(594216),	0,		/* C=cyl 0 thru 1571 */
	RDSZ(65772),	44,		/* D=cyl 44 thru 217 */
	RDSZ(303912),	218,		/* E=cyl 218 thru 1021 */
	RDSZ(207900),	1022,		/* F=cyl 1022 thru 1571 */
	RDSZ(528444),	174,		/* G=cyl 174 thru 1571 */
	RDSZ(511812),	218,		/* H=cyl 218 thru 1571 */

#if DEV_BSIZE == 512
/*
 * These values would not work for 1k,
 * since the number of cylinders would be different.
 */
}, rd7936H_sizes[8] = {
	RDSZ(16359),	1,		/* A=cyl 1 thru 19 */
	RDSZ(67158),	20,		/* B=cyl 20 thru 97 */
	RDSZ(600978),	0,		/* C=cyl 0 thru 697 */
	RDSZ(16359),	98,		/* D=cyl 98 thru 116 */
	RDSZ(120540),	117,		/* E=cyl 117 thru 256 */
	RDSZ(120540),	256,		/* F=cyl 256 thru 396 */
	RDSZ(516600),	98,		/* G=cyl 98 thru 697 */
	RDSZ(259161),	397,		/* H=cyl 397 thru 697 */
}, rd7937H_sizes[8] = {
#ifdef UTAH
	RDSZ(15990),	1,		/* A=cyl 1 thru 10 */
	RDSZ(67158),	11,		/* B=cyl 11 thru 52 */
	RDSZ(1116102),	0,		/* C=cyl 0 thru 697 */
	RDSZ(124722),	53,		/* D=cyl 53 thru 130 */
	RDSZ(163098),	131,		/* E=cyl 131 thru 232 */
	RDSZ(287820),	233,		/* F=cyl 233 thru 412 */
	RDSZ(1031355),	53,		/* G=cyl 53 thru 697 */
	RDSZ(455715),	413,		/* H=cyl 413 thru 697 */
#else
	RDSZ(15990),	1,		/* A=cyl 1 thru 10 */
	RDSZ(67158),	11,		/* B=cyl 11 thru 52 */
	RDSZ(1116102),	0,		/* C=cyl 0 thru 697 */
	RDSZ(15990),	53,		/* D=cyl 53 thru 62 */
	RDSZ(246246),	63,		/* E=cyl 63 thru 216 */
	RDSZ(246246),	217,		/* F=cyl 217 thru 370 */
	RDSZ(1031355),	53,		/* G=cyl 53 thru 697 */
	RDSZ(522873),	371,		/* H=cyl 371 thru 697 */
#endif
#endif
};

struct	rdinfo {
	int	nbpt;		/* DEV_BSIZE blocks per track */
	int	ntpc;		/* tracks per cylinder */
	int	nbpc;		/* blocks per cylinder */
	struct	size *sizes;	/* default partition info (if no disklabel) */
	short	hwid;		/* 2 byte HW id */
	short	maxunum;	/* maximum allowed unit number */
	char	*desc;		/* drive type description */
};

struct rdinfo rdinfo[] = {
	NRD7945ABPT,	NRD7945ATRK,	NRD7945ABPT * NRD7945ATRK,
	rd7945A_sizes,	RD7946AID,	0,	"7945A",
	NRD9134DBPT,	NRD9134DTRK,	NRD9134DBPT * NRD9134DTRK,
	rd9134D_sizes,	RD9134DID,	1,	"9134D",
	NRD9122SBPT,	NRD9122STRK,	NRD9122SBPT * NRD9122STRK,
	rd9122S_sizes,	RD9134LID,	1,	"9122S",
	NRD7912PBPT,	NRD7912PTRK,	NRD7912PBPT * NRD7912PTRK,
	rd7912P_sizes,	RD7912PID,	0,	"7912P",
	NRD7914PBPT,	NRD7914PTRK,	NRD7914PBPT * NRD7914PTRK,
	rd7914P_sizes,	RD7914PID,	0,	"7914P",
	NRD7958ABPT,	NRD7958ATRK,	NRD7958ABPT * NRD7958ATRK,
	rd7958A_sizes,	RD7958AID,	0,	"7958A",
	NRD7957ABPT,	NRD7957ATRK,	NRD7957ABPT * NRD7957ATRK,
	rd7957A_sizes,	RD7957AID,	0,	"7957A",
	NRD7933HBPT,	NRD7933HTRK,	NRD7933HBPT * NRD7933HTRK,
	rd7933H_sizes,	RD7933HID,	0,	"7933H",
	NRD9134LBPT,	NRD9134LTRK,	NRD9134LBPT * NRD9134LTRK,
	rd9134L_sizes,	RD9134LID,	1,	"9134L",
	NRD7936HBPT,	NRD7936HTRK,	NRD7936HBPT * NRD7936HTRK,
	rd7936H_sizes,	RD7936HID,	0,	"7936H",
	NRD7937HBPT,	NRD7937HTRK,	NRD7937HBPT * NRD7937HTRK,
	rd7937H_sizes,	RD7937HID,	0,	"7937H",
	NRD7914PBPT,	NRD7914PTRK,	NRD7914PBPT * NRD7914PTRK,
	rd7914P_sizes,	RD7914CTID,	0,	"7914CT",
	NRD7945ABPT,	NRD7945ATRK,	NRD7945ABPT * NRD7945ATRK,
	rd7945A_sizes,	RD7946AID,	0,	"7946A",
	NRD9122SBPT,	NRD9122STRK,	NRD9122SBPT * NRD9122STRK,
	rd9122S_sizes,	RD9134LID,	1,	"9122D",
	NRD7957BBPT,	NRD7957BTRK,	NRD7957BBPT * NRD7957BTRK,
	rd7957B_sizes,	RD7957BID,	0,	"7957B",
	NRD7958BBPT,	NRD7958BTRK,	NRD7958BBPT * NRD7958BTRK,
	rd7958B_sizes,	RD7958BID,	0,	"7958B",
	NRD7959BBPT,	NRD7959BTRK,	NRD7959BBPT * NRD7959BTRK,
	rd7959B_sizes,	RD7959BID,	0,	"7959B",
};
int nrdinfo = sizeof(rdinfo) / sizeof(rdinfo[0]);

struct	buf rdtab[NRD];
struct	buf rdbuf[NRD];

#define	rdunit(x)	((minor(x) >> 3) & 0xf)
#define rdpart(x)	(minor(x) & 0x7)
#define	rdpunit(x)	((x) & 7)
#define	b_cylin		b_resid
#define	RDRETRY		5
#define RDWAITC		1	/* min time for timeout in seconds */

int rderrthresh = RDRETRY-1;	/* when to start reporting errors */

rdinit(hd)
	register struct hp_device *hd;
{
	register struct rd_softc *rs = &rd_softc[hd->hp_unit];

	rs->sc_hd = hd;
	rs->sc_punit = rdpunit(hd->hp_flags);
	rs->sc_type = rdident(rs, hd);
	if (rs->sc_type < 0)
		return(0);
	rs->sc_dq.dq_ctlr = hd->hp_ctlr;
	rs->sc_dq.dq_unit = hd->hp_unit;
	rs->sc_dq.dq_slave = hd->hp_slave;
	rs->sc_dq.dq_driver = &rddriver;
	rs->sc_info = &rdinfo[rs->sc_type];
	rs->sc_flags = RDF_ALIVE;
#ifdef DEBUG
	/* always report errors */
	if (rddebug & RDB_ERROR)
		rderrthresh = 0;
#endif
	return(1);
}

rdident(rs, hd)
	struct rd_softc *rs;
	struct hp_device *hd;
{
	struct rd_describe desc;
	u_char stat, cmd[3];
	int unit, lunit;
	char name[7];
	register int ctlr, slave, id, i;

	ctlr = hd->hp_ctlr;
	slave = hd->hp_slave;
	unit = rs->sc_punit;
	lunit = hd->hp_unit;

	/*
	 * Grab device id and make sure:
	 * 1. It is a CS80 device.
	 * 2. It is one of the types we support.
	 * 3. If it is a 7946, we are accessing the disk unit (0)
	 */
	id = hpibid(ctlr, slave);
	if ((id & 0x200) == 0)
		return(-1);
	for (i = 0; i < nrdinfo; i++)
		if (id == rdinfo[i].hwid)
			break;
	if (i == nrdinfo || unit > rdinfo[i].maxunum)
		return(-1);
	id = i;

	/*
	 * Reset drive and collect device description.
	 * Don't really use the description info right now but
	 * might come in handy in the future (for disk labels).
	 */
	rdreset(rs, hd);
	cmd[0] = C_SUNIT(unit);
	cmd[1] = C_SVOL(0);
	cmd[2] = C_DESC;
	hpibsend(ctlr, slave, C_CMD, cmd, sizeof(cmd));
	hpibrecv(ctlr, slave, C_EXEC, &desc, 37);
	hpibrecv(ctlr, slave, C_QSTAT, &stat, sizeof(stat));
	bzero(name, sizeof(name));
	if (!stat) {
		register int n = desc.d_name;
		for (i = 5; i >= 0; i--) {
			name[i] = (n & 0xf) + '0';
			n >>= 4;
		}
		/* use drive characteristics to calculate xfer rate */
		rs->sc_wpms = 1000000 * (desc.d_sectsize/2) / desc.d_blocktime;
	}
#ifdef DEBUG
	if (rddebug & RDB_IDENT) {
		printf("rd%d: name: %x ('%s')\n",
		       lunit, desc.d_name, name);
		printf("  iuw %x, maxxfr %d, ctype %d\n",
		       desc.d_iuw, desc.d_cmaxxfr, desc.d_ctype);
		printf("  utype %d, bps %d, blkbuf %d, burst %d, blktime %d\n",
		       desc.d_utype, desc.d_sectsize,
		       desc.d_blkbuf, desc.d_burstsize, desc.d_blocktime);
		printf("  avxfr %d, ort %d, atp %d, maxint %d, fv %x, rv %x\n",
		       desc.d_uavexfr, desc.d_retry, desc.d_access,
		       desc.d_maxint, desc.d_fvbyte, desc.d_rvbyte);
		printf("  maxcyl/head/sect %d/%d/%d, maxvsect %d, inter %d\n",
		       desc.d_maxcyl, desc.d_maxhead, desc.d_maxsect,
		       desc.d_maxvsectl, desc.d_interleave);
	}
#endif
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
	printf("rd%d: %s\n", lunit, rdinfo[id].desc);
	return(id);
}

rdreset(rs, hd)
	register struct rd_softc *rs;
	register struct hp_device *hd;
{
	u_char stat;

	rs->sc_clear.c_unit = C_SUNIT(rs->sc_punit);
	rs->sc_clear.c_cmd = C_CLEAR;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_TCMD, &rs->sc_clear,
		sizeof(rs->sc_clear));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
	rs->sc_src.c_unit = C_SUNIT(RDCTLR);
	rs->sc_src.c_nop = C_NOP;
	rs->sc_src.c_cmd = C_SREL;
	rs->sc_src.c_param = C_REL;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_CMD, &rs->sc_src,
		sizeof(rs->sc_src));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
	rs->sc_ssmc.c_unit = C_SUNIT(rs->sc_punit);
	rs->sc_ssmc.c_cmd = C_SSM;
	rs->sc_ssmc.c_refm = REF_MASK;
	rs->sc_ssmc.c_fefm = FEF_MASK;
	rs->sc_ssmc.c_aefm = AEF_MASK;
	rs->sc_ssmc.c_iefm = IEF_MASK;
	hpibsend(hd->hp_ctlr, hd->hp_slave, C_CMD, &rs->sc_ssmc,
		sizeof(rs->sc_ssmc));
	hpibswait(hd->hp_ctlr, hd->hp_slave);
	hpibrecv(hd->hp_ctlr, hd->hp_slave, C_QSTAT, &stat, sizeof(stat));
#ifdef DEBUG
	rdstats[hd->hp_unit].rdresets++;
#endif
}

/*ARGSUSED*/
rdopen(dev, flags)
	dev_t dev;
{
	register int unit = rdunit(dev);
	register struct rd_softc *rs = &rd_softc[unit];

	if (unit >= NRD || (rs->sc_flags & RDF_ALIVE) == 0)
		return(ENXIO);
	if (rs->sc_hd->hp_dk >= 0) {
		/* guess at xfer rate based on 3600 rpm (60 rps) */
		if (rs->sc_wpms == 0)
			rs->sc_wpms = 60 * rs->sc_info->nbpt * DEV_BSIZE / 2;
		dk_wpms[rs->sc_hd->hp_dk] = rs->sc_wpms;
	}
	return(0);
}

rdstrategy(bp)
	register struct buf *bp;
{
	register int unit = rdunit(bp->b_dev);
	register struct rd_softc *rs = &rd_softc[unit];
	register struct size *pinfo = &rs->sc_info->sizes[rdpart(bp->b_dev)];
	register struct buf *dp = &rdtab[unit];
	register daddr_t bn;
	register int sz, s;

#ifdef DEBUG
	if (rddebug & RDB_FOLLOW)
		printf("rdstrategy(%x): dev %x, bn %x, bcount %x, %c\n",
		       bp, bp->b_dev, bp->b_blkno, bp->b_bcount,
		       (bp->b_flags & B_READ) ? 'R' : 'W');
#endif
	bn = bp->b_blkno;
	sz = howmany(bp->b_bcount, DEV_BSIZE);
	if (bn < 0 || bn + sz > pinfo->nblocks) {
		sz = pinfo->nblocks - bn;
		if (sz == 0) {
			bp->b_resid = bp->b_bcount;
			goto done;
		}
		if (sz < 0) {
			bp->b_error = EINVAL;
			bp->b_flags |= B_ERROR;
			goto done;
		}
		bp->b_bcount = dbtob(sz);
	}
	bp->b_cylin = bn / rs->sc_info->nbpc + pinfo->cyloff;
	s = splbio();
	disksort(dp, bp);
	if (dp->b_active == 0) {
		dp->b_active = 1;
		rdustart(unit);
	}
	splx(s);
	return;
done:
	biodone(bp);
}

/*
 * Called from timeout() when handling maintenance releases
 */
rdrestart(unit)
	int unit;
{
	int s = splbio();
	rdustart(unit);
	splx(s);
}

rdustart(unit)
	register int unit;
{
	register struct buf *bp;
	register struct rd_softc *rs = &rd_softc[unit];

	bp = rdtab[unit].b_actf;
	rs->sc_addr = bp->b_un.b_addr;
	rs->sc_resid = bp->b_bcount;
	if (hpibreq(&rs->sc_dq))
		rdstart(unit);
}

rdstart(unit)
	register int unit;
{
	register struct rd_softc *rs = &rd_softc[unit];
	register struct buf *bp = rdtab[unit].b_actf;
	register struct hp_device *hp = rs->sc_hd;
	register int part;

again:
#ifdef DEBUG
	if (rddebug & RDB_FOLLOW)
		printf("rdstart(%d): bp %x, %c\n", unit, bp,
		       (bp->b_flags & B_READ) ? 'R' : 'W');
#endif
	part = rdpart(bp->b_dev);
	rs->sc_flags |= RDF_SEEK;
	rs->sc_ioc.c_unit = C_SUNIT(rs->sc_punit);
	rs->sc_ioc.c_volume = C_SVOL(0);
	rs->sc_ioc.c_saddr = C_SADDR;
	rs->sc_ioc.c_hiaddr = 0;
	rs->sc_ioc.c_addr = RDBTOS(bp->b_blkno + rs->sc_info->nbpc *
		rs->sc_info->sizes[part].cyloff);
	rs->sc_ioc.c_nop2 = C_NOP;
	rs->sc_ioc.c_slen = C_SLEN;
	rs->sc_ioc.c_len = rs->sc_resid;
	rs->sc_ioc.c_cmd = bp->b_flags & B_READ ? C_READ : C_WRITE;
#ifdef DEBUG
	if (rddebug & RDB_IO)
		printf("rdstart: hpibsend(%x, %x, %x, %x, %x)\n",
		       hp->hp_ctlr, hp->hp_slave, C_CMD,
		       &rs->sc_ioc.c_unit, sizeof(rs->sc_ioc)-2);
#endif
	if (hpibsend(hp->hp_ctlr, hp->hp_slave, C_CMD, &rs->sc_ioc.c_unit,
		     sizeof(rs->sc_ioc)-2) == sizeof(rs->sc_ioc)-2) {
		if (hp->hp_dk >= 0) {
			dk_busy |= 1 << hp->hp_dk;
			dk_seek[hp->hp_dk]++;
		}
#ifdef DEBUG
		if (rddebug & RDB_IO)
			printf("rdstart: hpibawait(%x)\n", hp->hp_ctlr);
#endif
		hpibawait(hp->hp_ctlr);
		return;
	}
	/*
	 * Experience has shown that the hpibwait in this hpibsend will
	 * occasionally timeout.  It appears to occur mostly on old 7914
	 * drives with full maintenance tracks.  We should probably
	 * integrate this with the backoff code in rderror.
	 */
#ifdef DEBUG
	if (rddebug & RDB_ERROR)
		printf("rd%d: rdstart: cmd %x adr %d blk %d len %d ecnt %d\n",
		       unit, rs->sc_ioc.c_cmd, rs->sc_ioc.c_addr,
		       bp->b_blkno, rs->sc_resid, rdtab[unit].b_errcnt);
	rdstats[unit].rdretries++;
#endif
	rs->sc_flags &= ~RDF_SEEK;
	rdreset(rs, hp);
	if (rdtab[unit].b_errcnt++ < RDRETRY)
		goto again;
	printf("rd%d: rdstart err: cmd 0x%x sect %d blk %d len %d\n",
	       unit, rs->sc_ioc.c_cmd, rs->sc_ioc.c_addr,
	       bp->b_blkno, rs->sc_resid);
	rdtab[unit].b_errcnt = 0;
	rdtab[unit].b_actf = bp->b_actf;
	bp->b_flags |= B_ERROR;
	bp->b_error = EIO;
	bp->b_resid = 0;
	biodone(bp);
	hpibfree(&rs->sc_dq);
	bp = rdtab[unit].b_actf;
	if (bp == NULL) {
		rdtab[unit].b_active = 0;
		return;
	}
	rs->sc_addr = bp->b_un.b_addr;
	rs->sc_resid = bp->b_bcount;
	if (hpibreq(&rs->sc_dq))
		goto again;
}

rdgo(unit)
	register int unit;
{
	register struct rd_softc *rs = &rd_softc[unit];
	register struct hp_device *hp = rs->sc_hd;
	struct buf *bp = rdtab[unit].b_actf;

	if (hp->hp_dk >= 0) {
		dk_busy |= 1 << hp->hp_dk;
		dk_xfer[hp->hp_dk]++;
		dk_wds[hp->hp_dk] += rs->sc_resid >> 6;
	}
	hpibgo(hp->hp_ctlr, hp->hp_slave, C_EXEC,
	       rs->sc_addr, rs->sc_resid, bp->b_flags & B_READ);
}

rdintr(unit)
	register int unit;
{
	register struct rd_softc *rs = &rd_softc[unit];
	register struct buf *bp = rdtab[unit].b_actf;
	register struct hp_device *hp = rs->sc_hd;
	u_char stat = 13;	/* in case hpibrecv fails */
	int rv, restart;
	
#ifdef DEBUG
	if (rddebug & RDB_FOLLOW)
		printf("rdintr(%d): bp %x, %c, flags %x\n", unit, bp,
		       (bp->b_flags & B_READ) ? 'R' : 'W', rs->sc_flags);
	if (bp == NULL) {
		printf("rd%d: bp == NULL\n", unit);
		return;
	}
#endif
	if (hp->hp_dk >= 0)
		dk_busy &= ~(1 << hp->hp_dk);
	if (rs->sc_flags & RDF_SEEK) {
		rs->sc_flags &= ~RDF_SEEK;
		if (hpibustart(hp->hp_ctlr))
			rdgo(unit);
		return;
	}
	if ((rs->sc_flags & RDF_SWAIT) == 0) {
#ifdef DEBUG
		rdstats[unit].rdpolltries++;
#endif
		if (hpibpptest(hp->hp_ctlr, hp->hp_slave) == 0) {
#ifdef DEBUG
			rdstats[unit].rdpollwaits++;
#endif
			if (hp->hp_dk >= 0)
				dk_busy |= 1 << hp->hp_dk;
			rs->sc_flags |= RDF_SWAIT;
			hpibawait(hp->hp_ctlr);
			return;
		}
	} else
		rs->sc_flags &= ~RDF_SWAIT;
	rv = hpibrecv(hp->hp_ctlr, hp->hp_slave, C_QSTAT, &stat, 1);
	if (rv != 1 || stat) {
#ifdef DEBUG
		if (rddebug & RDB_ERROR)
			printf("rdintr: recv failed or bad stat %d\n", stat);
#endif
		restart = rderror(unit);
#ifdef DEBUG
		rdstats[unit].rdretries++;
#endif
		if (rdtab[unit].b_errcnt++ < RDRETRY) {
			if (restart)
				rdstart(unit);
			return;
		}
		bp->b_flags |= B_ERROR;
		bp->b_error = EIO;
	}
	rdtab[unit].b_errcnt = 0;
	rdtab[unit].b_actf = bp->b_actf;
	bp->b_resid = 0;
	biodone(bp);
	hpibfree(&rs->sc_dq);
	if (rdtab[unit].b_actf)
		rdustart(unit);
	else
		rdtab[unit].b_active = 0;
}

rdstatus(rs)
	register struct rd_softc *rs;
{
	register int c, s;
	u_char stat;
	int rv;

	c = rs->sc_hd->hp_ctlr;
	s = rs->sc_hd->hp_slave;
	rs->sc_rsc.c_unit = C_SUNIT(rs->sc_punit);
	rs->sc_rsc.c_sram = C_SRAM;
	rs->sc_rsc.c_ram = C_RAM;
	rs->sc_rsc.c_cmd = C_STATUS;
	bzero((caddr_t)&rs->sc_stat, sizeof(rs->sc_stat));
	rv = hpibsend(c, s, C_CMD, &rs->sc_rsc, sizeof(rs->sc_rsc));
	if (rv != sizeof(rs->sc_rsc)) {
#ifdef DEBUG
		if (rddebug & RDB_STATUS)
			printf("rdstatus: send C_CMD failed %d != %d\n",
			       rv, sizeof(rs->sc_rsc));
#endif
		return(1);
	}
	rv = hpibrecv(c, s, C_EXEC, &rs->sc_stat, sizeof(rs->sc_stat));
	if (rv != sizeof(rs->sc_stat)) {
#ifdef DEBUG
		if (rddebug & RDB_STATUS)
			printf("rdstatus: send C_EXEC failed %d != %d\n",
			       rv, sizeof(rs->sc_stat));
#endif
		return(1);
	}
	rv = hpibrecv(c, s, C_QSTAT, &stat, 1);
	if (rv != 1 || stat) {
#ifdef DEBUG
		if (rddebug & RDB_STATUS)
			printf("rdstatus: recv failed %d or bad stat %d\n",
			       rv, stat);
#endif
		return(1);
	}
	return(0);
}

/*
 * Deal with errors.
 * Returns 1 if request should be restarted,
 * 0 if we should just quietly give up.
 */
rderror(unit)
	int unit;
{
	struct rd_softc *rs = &rd_softc[unit];
	register struct rd_stat *sp;
	struct buf *bp;
	daddr_t hwbn, pbn;

	if (rdstatus(rs)) {
#ifdef DEBUG
		printf("rd%d: couldn't get status\n", unit);
#endif
		rdreset(rs, rs->sc_hd);
		return(1);
	}
	sp = &rs->sc_stat;
	if (sp->c_fef & FEF_REXMT)
		return(1);
	if (sp->c_fef & FEF_PF) {
		rdreset(rs, rs->sc_hd);
		return(1);
	}
	/*
	 * Unit requests release for internal maintenance.
	 * We just delay awhile and try again later.  Use expontially
	 * increasing backoff ala ethernet drivers since we don't really
	 * know how long the maintenance will take.  With RDWAITC and
	 * RDRETRY as defined, the range is 1 to 32 seconds.
	 */
	if (sp->c_fef & FEF_IMR) {
		extern int hz;
		int rdtimo = RDWAITC << rdtab[unit].b_errcnt;
#ifdef DEBUG
		printf("rd%d: internal maintenance, %d second timeout\n",
		       unit, rdtimo);
		rdstats[unit].rdtimeouts++;
#endif
		hpibfree(&rs->sc_dq);
		timeout(rdrestart, unit, rdtimo*hz);
		return(0);
	}
	/*
	 * Only report error if we have reached the error reporting
	 * threshhold.  By default, this will only report after the
	 * retry limit has been exceeded.
	 */
	if (rdtab[unit].b_errcnt < rderrthresh)
		return(1);

	/*
	 * First conjure up the block number at which the error occured.
	 * Note that not all errors report a block number, in that case
	 * we just use b_blkno.
 	 */
	bp = rdtab[unit].b_actf;
	pbn = rs->sc_info->nbpc *
		rs->sc_info->sizes[rdpart(bp->b_dev)].cyloff;
	if ((sp->c_fef & FEF_CU) || (sp->c_fef & FEF_DR) ||
	    (sp->c_ief & IEF_RRMASK)) {
		hwbn = RDBTOS(pbn + bp->b_blkno);
		pbn = bp->b_blkno;
	} else {
		hwbn = sp->c_blk;
		pbn = RDSTOB(hwbn) - pbn;
	}
	/*
	 * Now output a generic message suitable for badsect.
	 * Note that we don't use harderr cuz it just prints
	 * out b_blkno which is just the beginning block number
	 * of the transfer, not necessary where the error occured.
	 */
	printf("rd%d%c: hard error sn%d\n",
	       rdunit(bp->b_dev), 'a'+rdpart(bp->b_dev), pbn);
	/*
	 * Now report the status as returned by the hardware with
	 * attempt at interpretation (unless debugging).
	 */
	printf("rd%d %s error:",
	       unit, (bp->b_flags & B_READ) ? "read" : "write");
#ifdef DEBUG
	if (rddebug & RDB_ERROR) {
		/* status info */
		printf("\n    volume: %d, unit: %d\n",
		       (sp->c_vu>>4)&0xF, sp->c_vu&0xF);
		rdprinterr("reject", sp->c_ref, err_reject);
		rdprinterr("fault", sp->c_fef, err_fault);
		rdprinterr("access", sp->c_aef, err_access);
		rdprinterr("info", sp->c_ief, err_info);
		printf("    block: %d, P1-P10: ", hwbn);
		printf("%s", hexstr(*(u_int *)&sp->c_raw[0], 8));
		printf("%s", hexstr(*(u_int *)&sp->c_raw[4], 8));
		printf("%s\n", hexstr(*(u_short *)&sp->c_raw[8], 4));
		/* command */
		printf("    ioc: ");
		printf("%s", hexstr(*(u_int *)&rs->sc_ioc.c_pad, 8));
		printf("%s", hexstr(*(u_short *)&rs->sc_ioc.c_hiaddr, 4));
		printf("%s", hexstr(*(u_int *)&rs->sc_ioc.c_addr, 8));
		printf("%s", hexstr(*(u_short *)&rs->sc_ioc.c_nop2, 4));
		printf("%s", hexstr(*(u_int *)&rs->sc_ioc.c_len, 8));
		printf("%s\n", hexstr(*(u_short *)&rs->sc_ioc.c_cmd, 4));
		return(1);
	}
#endif
	printf(" v%d u%d, R0x%x F0x%x A0x%x I0x%x\n",
	       (sp->c_vu>>4)&0xF, sp->c_vu&0xF,
	       sp->c_ref, sp->c_fef, sp->c_aef, sp->c_ief);
	printf("P1-P10: ");
	printf("%s", hexstr(*(u_int *)&sp->c_raw[0], 8));
	printf("%s", hexstr(*(u_int *)&sp->c_raw[4], 8));
	printf("%s\n", hexstr(*(u_short *)&sp->c_raw[8], 4));
	return(1);
}

rdread(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = rdunit(dev);

	return(physio(rdstrategy, &rdbuf[unit], dev, B_READ, minphys, uio));
}

rdwrite(dev, uio)
	dev_t dev;
	struct uio *uio;
{
	register int unit = rdunit(dev);

	return(physio(rdstrategy, &rdbuf[unit], dev, B_WRITE, minphys, uio));
}

/*ARGSUSED*/
rdioctl(dev, cmd, data, flag)
	dev_t dev;
	int cmd;
	caddr_t data;
	int flag;
{
	return(EINVAL);
}

rdsize(dev)
	dev_t dev;
{
	register int unit = rdunit(dev);
	register struct rd_softc *rs = &rd_softc[unit];

	if (unit >= NRD || (rs->sc_flags & RDF_ALIVE) == 0)
		return(-1);
	return(rs->sc_info->sizes[rdpart(dev)].nblocks);
}

#ifdef DEBUG
rdprinterr(str, err, tab)
	char *str;
	short err;
	char *tab[];
{
	register int i;
	int printed;

	if (err == 0)
		return;
	printf("    %s error field:", str, err);
	printed = 0;
	for (i = 0; i < 16; i++)
		if (err & (0x8000 >> i))
			printf("%s%s", printed++ ? " + " : " ", tab[i]);
	printf("\n");
}
#endif

/*
 * Non-interrupt driven, non-dma dump routine.
 */
rddump(dev)
	dev_t dev;
{
	int part = rdpart(dev);
	int unit = rdunit(dev);
	register struct rd_softc *rs = &rd_softc[unit];
	register struct hp_device *hp = rs->sc_hd;
	register daddr_t baddr;
	register int maddr, pages, i;
	char stat;
	extern int lowram, dumpsize;
#ifdef DEBUG
	extern int pmapdebug;
	pmapdebug = 0;
#endif

	pages = dumpsize;
#ifdef DEBUG
	if (rddebug & RDB_DUMP)
		printf("rddump(%x): u %d p %d dumplo %d ram %x pmem %d\n",
		       dev, unit, part, dumplo, lowram, ctod(pages));
#endif
	/* is drive ok? */
	if (unit >= NRD || (rs->sc_flags & RDF_ALIVE) == 0)
		return (ENXIO);
	/* HPIB idle? */
	if (!hpibreq(&rs->sc_dq)) {
#ifdef DEBUG
		/* is this a safe thing to do?? */
		hpibreset(hp->hp_ctlr);
		rdreset(rs, rs->sc_hd);
		printf("[ drive %d reset ] ", unit);
#else
		return (EFAULT);
#endif
	}
	/* dump parameters in range? */
	if (dumplo < 0 || dumplo >= rs->sc_info->sizes[part].nblocks)
		return (EINVAL);
	if (dumplo + ctod(pages) > rs->sc_info->sizes[part].nblocks)
		pages = dtoc(rs->sc_info->sizes[part].nblocks - dumplo);
	maddr = lowram;
	baddr = dumplo + rs->sc_info->nbpc * rs->sc_info->sizes[part].cyloff;
#ifdef DEBUG
	if (rddebug & RDB_DUMP)
		printf("rddump: dumping %d pages from %x to disk block %d\n",
		       pages, maddr, baddr);
#endif
	for (i = 0; i < pages; i++) {
#ifdef DEBUG
#define NPGMB	(1024*1024/NBPG)
		/* print out how many Mbs we have dumped */
		if (i && (i % NPGMB) == 0)
			printf("%d ", i / NPGMB);
#undef NPBMG
#endif
		rs->sc_ioc.c_unit = C_SUNIT(rs->sc_punit);
		rs->sc_ioc.c_volume = C_SVOL(0);
		rs->sc_ioc.c_saddr = C_SADDR;
		rs->sc_ioc.c_hiaddr = 0;
		rs->sc_ioc.c_addr = RDBTOS(baddr);
		rs->sc_ioc.c_nop2 = C_NOP;
		rs->sc_ioc.c_slen = C_SLEN;
		rs->sc_ioc.c_len = NBPG;
		rs->sc_ioc.c_cmd = C_WRITE;
		hpibsend(hp->hp_ctlr, hp->hp_slave, C_CMD,
			 &rs->sc_ioc.c_unit, sizeof(rs->sc_ioc)-2);
		if (hpibswait(hp->hp_ctlr, hp->hp_slave)) {
#ifdef DEBUG
			if (rddebug & RDB_DUMP)
				printf("rddump: IOC wait timeout\n");
#endif
			return (EIO);
		}
		pmap_enter(pmap_kernel(), vmmap, maddr, VM_PROT_READ, TRUE);
		hpibsend(hp->hp_ctlr, hp->hp_slave, C_EXEC, vmmap, NBPG);
		if (hpibswait(hp->hp_ctlr, hp->hp_slave)) {
#ifdef DEBUG
			if (rddebug & RDB_DUMP)
				printf("rddump: write wait timeout\n");
#endif
		}
		hpibrecv(hp->hp_ctlr, hp->hp_slave, C_QSTAT, &stat, 1);
		if (stat) {
#ifdef DEBUG
			if (rddebug & RDB_DUMP)
				printf("rddump: write failed, status %x\n",
				       stat);
#endif
			return (EIO);
		}
		maddr += NBPG;
		baddr += ctod(1);
	}
	return (0);
}
#endif
