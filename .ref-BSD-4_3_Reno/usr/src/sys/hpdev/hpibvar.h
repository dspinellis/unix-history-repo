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
 *	@(#)hpibvar.h	7.1 (Berkeley) 5/8/90
 */

#define	HPIB_IPL(x)	((((x) >> 4) & 0x3) + 3)

#define	HPIBA		32
#define	HPIBB		1
#define	HPIBC		8
#define	HPIBA_BA	21
#define	HPIBC_BA	30
#define	HPIBA_IPL	3

#define	CSA_BA		0x1F

#define	IDS_WDMA	0x04
#define	IDS_WRITE	0x08
#define	IDS_IR		0x40
#define	IDS_IE		0x80
#define	IDS_DMA(x)	(1 << (x))

#define	C_DCL		0x14
#define	C_LAG		0x20
#define	C_UNL		0x3f
#define	C_TAG		0x40
#define	C_UNA		0x5e
#define	C_UNT		0x5f
#define	C_SCG		0x60

struct	hpib_softc {
	struct	hp_ctlr *sc_hc;
	int	sc_flags;
	struct	devqueue sc_dq;
	struct	devqueue sc_sq;
	int	sc_ba;
	int	sc_type;
	char	*sc_addr;
	int	sc_count;
	int	sc_curcnt;
};

/* sc_flags */
#define	HPIBF_IO	0x1
#define	HPIBF_DONE	0x2
#define	HPIBF_PPOLL	0x4
#define	HPIBF_READ	0x8
#define	HPIBF_DMA16	0x8000

#ifdef KERNEL
extern	struct hpib_softc hpib_softc[];
extern	int internalhpib;
#endif
