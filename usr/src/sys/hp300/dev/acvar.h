/*
 * Copyright (c) 1991 University of Utah.
 * Copyright (c) 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * the Systems Programming Group of the University of Utah Computer
 * Science Department.
 *
 * %sccs.include.redist.c%
 *
 * from: Utah $Hdr: acvar.h 1.1 91/06/19$
 *
 *	@(#)acvar.h	7.1 (Berkeley) %G%
 */

struct	ac_softc {
	struct	hp_device *sc_hd;
	int	sc_flags;
	struct	buf *sc_bp;
	struct	scsi_fmt_cdb *sc_cmd;
	struct	acinfo sc_einfo;
	short	sc_punit;
	short	sc_picker;
	struct	devqueue sc_dq;
};

#define	ACF_ALIVE	0x01
#define ACF_OPEN	0x02
#define ACF_ACTIVE	0x04

#define ACCMD_INITES	0x07
#define	ACCMD_MODESENSE	0x1A
#define ACCMD_READES	0xB8
#define ACCMD_MOVEM	0xA5

struct	ac_restathdr {
	short	ac_felt;	/* first element reported */
	short	ac_nelt;	/* number of elements reported */
	long	ac_bcount;	/* length of report (really only 24 bits) */
};

struct	ac_restatphdr {
	char	ac_type;	/* type code */
	char	ac_res;
	short	ac_dlen;	/* element descriptor length */
	long	ac_bcount;	/* byte count (really only 24 bits) */
};

struct	ac_restatdb {
	short	ac_eaddr;	/* element address */
	u_int	ac_res1:2,
		ac_ie:1,	/* import enabled (IEE only) */
		ac_ee:1,	/* export enabled (IEE only) */
		ac_acc:1,	/* accessible from MTE */
		ac_exc:1,	/* element in abnormal state */
		ac_imp:1,	/* 1 == user inserted medium (IEE only) */
		ac_full:1;	/* element contains media */
};
