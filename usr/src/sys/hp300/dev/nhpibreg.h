/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
 *
 * %sccs.include.redist.c%
 *
 *	@(#)nhpibreg.h	7.3 (Berkeley) %G%
 */

#include <hp/dev/iotypes.h>	/* XXX */

struct	nhpibdevice {
	u_char	hpib_pad0;
	vu_char	hpib_cid;
	u_char	hpib_pad1;
#define	hpib_ie		hpib_ids
	vu_char	hpib_ids;
	u_char	hpib_pad2;
	vu_char	hpib_csa;
	u_char	hpib_pad3[11];
#define	hpib_mim	hpib_mis
	vu_char	hpib_mis;
	u_char	hpib_pad4;
#define	hpib_lim	hpib_lis
	vu_char	hpib_lis;
	u_char	hpib_pad5;
	vu_char	hpib_is;
	u_char	hpib_pad6;
#define	hpib_acr	hpib_cls
	vu_char	hpib_cls;
	u_char	hpib_pad7;
	vu_char	hpib_ar;
	u_char	hpib_pad8;
	vu_char	hpib_sprb;
	u_char	hpib_pad9;
#define	hpib_ppr	hpib_cpt
	vu_char	hpib_cpt;
	u_char	hpib_pad10;
	vu_char	hpib_data;
};

#define	LIS_SRQ		0x02
#define	LIS_ERR		0x40

#define	MIS_END		0x08
#define	MIS_BO		0x10
#define	MIS_BI		0x20

#define	IS_TADS		0x02
#define	IS_LADS		0x04

#define	AUX_CSWRST	0
#define	AUX_RHDF	2
#define	AUX_CHDFA	3
#define	AUX_CHDFE	4
#define	AUX_EOI		8
#define	AUX_GTS		11
#define	AUX_TCA		12
#define	AUX_TCS		13
#define	AUX_CPP		14
#define	AUX_CSIC	15
#define	AUX_CSRE	16
#define	AUX_CDAI	19
#define	AUX_CSHDW	22
#define	AUX_SSWRST	128
#define	AUX_SHDFE	132
#define	AUX_SLON	137
#define	AUX_STON	138
#define	AUX_SPP		142
#define	AUX_SSIC	143
#define	AUX_SSRE	144
#define	AUX_SSTD1	149
#define	AUX_SVSTD1	151

