/*
 * Copyright (c) 1982, 1990 The Regents of the University of California.
 * All rights reserved.
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
 *	@(#)nhpibreg.h	7.1 (Berkeley) 5/8/90
 */

#define	vu_char	volatile u_char

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
